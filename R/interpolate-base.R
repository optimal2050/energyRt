# Derived scenarios: `interpolate_model(..., base = REF)`.
#
# A derived scenario is its base plus a few objects that leave the base's sets,
# maps and parameters untouched: user constraints, user cost terms, taxes and
# subsidies. Only what those objects contribute is computed -- in a throwaway
# copy of the base, through the same pipeline stages, in the same order -- and
# appended to a copy of the base. The result equals a full interpolation of the
# model with the base's additions and the new ones.
#
# Anything else (a new process or commodity, a replaced object, another
# calendar, horizon, geoscale or build setting) changes tables the base already
# holds, and is interpolated in full; `.base_refusal()` says why.

# Parameter names a stage loops over: all of them, or the given subset.
.param_subset <- function(scen, params = NULL) {
  nm <- names(scen@modInp@parameters)
  if (is.null(params)) nm else nm[nm %in% params]
}

# Classes a derived scenario can add without a full interpolation.
.base_delta_classes <- c("constraint", "costs", "tax", "subsidy")

# Arguments of interpolate_model() whose explicit use forces a full run.
.base_full_args <- c("code", "kvl", "boundary_prices", ".prefilter",
                     "parent_levels", "fx_equ", "on_missing_input")

# Identity of the model a scenario was interpolated from. A model loaded thinned
# from the store and not rehydrated is the stored version itself, so its
# recorded hash stands for it; anything in memory may have been changed since
# it was loaded and is hashed on its content.
.model_identity <- function(m) {
  h <- m@misc$hash %||% ""
  if (!isInMemory(m) && nzchar(h)) return(h)
  model_hash(.deep_mem(m))
}

# The objects passed through `...`, flattened out of their repositories.
.base_additions <- function(args) {
  objs <- list()
  bad <- character()
  for (a in args) {
    items <- if (inherits(a, "repository")) a@data else list(a)
    for (o in items) {
      if (inherits(o, .base_delta_classes)) {
        objs[[length(objs) + 1L]] <- o
      } else {
        bad <- c(bad, class(o)[1])
      }
    }
  }
  list(objects = objs, bad = unique(bad))
}

# NULL when `object` + `args` can be built on `base`, else the reason it cannot.
.base_refusal <- function(object, base, args, given, fold, sparse, prune,
                          check_model = TRUE) {
  st <- base@status
  if (!isTRUE(st$sparse)) {
    return("the base was built dense (sparse = FALSE)")
  }
  folded <- as.character(st$folded %||% character())
  obj_dims <- intersect(folded, c("comm", "tech", "stg", "trade"))
  if (length(obj_dims)) {
    return(paste0("the base is folded over ", paste(obj_dims, collapse = ", "),
                  ", decided over the whole column"))
  }
  if (isTRUE(given[["fold"]])) {
    req <- if (isTRUE(fold)) c("region", "timeslice", "year")
      else if (is.null(fold) || isFALSE(fold)) character(0)
      else intersect(as.character(fold), .foldable_dims)
    if (!setequal(req, folded)) return("`fold` differs from the base")
  }
  if (isTRUE(given[["sparse"]]) && !isTRUE(sparse)) {
    return("`sparse` differs from the base")
  }
  if (isTRUE(given[["prune"]]) && !identical(isTRUE(prune), isTRUE(st$pruned))) {
    return("`prune` differs from the base")
  }
  full <- .base_full_args[vapply(.base_full_args,
                                 function(a) isTRUE(given[[a]]), logical(1))]
  if (length(full)) {
    return(paste0("`", full[1], "` is given"))
  }
  add <- .base_additions(args)
  if (length(add$bad)) {
    return(paste0("an object of class ", add$bad[1], " is added"))
  }
  nm <- vapply(add$objects, function(o) o@name, character(1))
  dup <- unique(nm[duplicated(nm)])
  if (length(dup)) return(paste0("'", dup[1], "' is passed twice"))
  have <- collect_object_names(base@model, classes = NULL)$name
  clash <- intersect(nm, have)
  if (length(clash)) {
    return(paste0("'", clash[1], "' replaces an object of the base"))
  }
  if (!check_model) return(NULL)
  id <- base@misc$model_id
  if (is.null(id$hash)) {
    return("the base did not record the model it was built from")
  }
  if (!identical(object@name, id$name) || !identical(.model_identity(object), id$hash)) {
    return("the model differs from the one the base was built from")
  }
  NULL
}

# A parameter with its rows in memory and no reference: about to be written.
.param_to_memory <- function(p) {
  if (!is.null(p@misc$rule)) return(p)
  if (isInMemory(p) && is.null(p@misc$param_ref)) return(p)
  d <- get_data_slot(p)
  p@misc$param_ref <- NULL
  p@misc$onDisk <- NULL
  p <- mark_inMemory(setObjPath(p, NULL))
  p@data <- if (NROW(d)) {
    data.table::as.data.table(d)[, colnames(p@data), with = FALSE]
  } else {
    .empty_param_data(p)
  }
  p
}

# The base's tables in memory, except references that are not written to: a
# derived scenario never writes into the base's own store, and its referenced
# tables stay references.
.base_materialize <- function(mi, touched) {
  for (pn in names(mi@parameters)) {
    p <- mi@parameters[[pn]]
    keep_ref <- !is.null(p@misc$param_ref) && !(pn %in% touched)
    if (keep_ref) next
    mi@parameters[[pn]] <- .param_to_memory(p)
  }
  mark_inMemory(mi)
}

# The vTotalUserCosts equation from its terms (the same string the full
# pipeline writes), and its terms back from that string.
.wrap_user_costs <- function(terms) {
  if (length(terms) == 0) {
    return(paste0(
      "eqTotalUserCosts(region, year)$mvTotalUserCosts(region, year).. ",
      "vTotalUserCosts(region, year) =e= 0;"
    ))
  }
  paste0(
    "eqTotalUserCosts(region, year)$mvTotalUserCosts(region, year)..",
    "   vTotalUserCosts(region, year) =e= ",
    gsub("[+][ ]*[-]", "-", paste0(terms, collapse = " + ")), ";"
  )
}

.unwrap_user_costs <- function(eq) {
  if (length(eq) == 0 || identical(eq, .wrap_user_costs(character()))) {
    return(character())
  }
  if (length(eq) > 1) return(eq)           # not wrapped yet
  sub(";$", "", sub("^.*?=e= ", "", eq, perl = TRUE))
}

# Levy names in the order the full pipeline collects them: taxes, then
# subsidies, each in model order.
.levy_names <- function(model) {
  on <- collect_object_names(model, classes = c("tax", "subsidy"))
  c(on$name[on$class %in% "tax"], on$name[on$class %in% "subsidy"])
}

.levy_param_names <- function() {
  unique(unlist(c(.levy_comm_params, .levy_proc_params)))
}

#' Build a derived scenario from an interpolated base
#'
#' Called by [interpolate_model()] when `base` is given. Returns `NULL` (with a
#' message saying why) when the request needs a full interpolation.
#' @keywords internal
#' @noRd
.interpolate_from_base <- function(object, base, name, args, ondisk, given,
                                   fold, sparse, prune, verbose) {
  t0 <- Sys.time()
  if (!inherits(base, "scenario") || !isTRUE(base@status$interpolated)) {
    stop("`base` must be an interpolated scenario.", call. = FALSE)
  }
  if (inherits(object, "scenario")) {
    if (!identical(object@name, base@name)) {
      message("base = '", base@name, "': `object` is another scenario; ",
              "interpolating in full.")
      return(NULL)
    }
    object <- NULL
  }
  # the base passed as `object` is its own model
  why <- .base_refusal(object, base, args, given, fold, sparse, prune,
                       check_model = !is.null(object))
  if (!is.null(why)) {
    message("base = '", base@name, "': ", why, "; interpolating in full.")
    return(NULL)
  }
  if (is.null(name)) {
    name <- paste0("scen_", base@model@name)
    if (verbose) message("Scenario name not provided; using default: ", name)
  }
  .assert_object_name(name, what = "scenario")
  if (isTRUE(ondisk) && verbose) {
    message("Derived scenario '", name, "' is built in memory; ",
            "save it with save_scenario().")
  }

  # -- the copy -------------------------------------------------------------
  levy_par <- intersect(.levy_param_names(), names(base@modInp@parameters))
  levy_map <- c("mTaxCost", "mSubCost")
  agg_map <- c("mvTotalCost", "mvTotalUserCosts")
  scen <- base
  scen@name <- name
  scen@modInp <- .base_materialize(base@modInp,
                                   touched = c("levy", levy_par, levy_map, agg_map))
  scen@modOut <- new("modOut")
  scen@status$solved <- FALSE
  scen@status$script <- FALSE
  scen@misc$run <- NULL
  scen@misc$base <- list(name = base@name, path = base@path %||% "",
                         model = base@misc$model_id)
  scen <- mark_inMemory(scen)
  scen@path <- fp(get_scenarios_path(), .scenario_dir_name(
    name, scen@model@name, scen@settings@calendar@name,
    scen@settings@horizon@name)) |> .fix_path()

  # the additions join the model as interpolate_model() adds them:
  # repositories first, then the loose objects as one repository
  repos <- Filter(function(a) inherits(a, "repository"), args)
  bricks <- Filter(function(a) !inherits(a, "repository"), args)
  model <- scen@model
  added <- character()
  for (r in repos) {
    model <- add(model, r)
    added <- c(added, r@name)
  }
  if (length(bricks)) {
    br <- newRepository(name = paste0(name, "_repo")) |> add(bricks)
    model <- add(model, br)
    added <- c(added, br@name)
  }
  model@misc$hash <- NULL
  scen@model <- model
  scen@misc$added <- c(scen@misc$added, added)
  new_objs <- .base_additions(args)$objects
  levies <- Filter(function(o) inherits(o, c("tax", "subsidy")), new_objs)
  cns <- Filter(function(o) inherits(o, c("constraint", "costs")), new_objs)
  .interp_step(verbose, paste0("base '", base@name, "': adding ",
                               length(levies), " levies, ", length(cns),
                               " constraint(s) / cost term(s)"))

  # -- levy set --------------------------------------------------------------
  if (length(levies)) {
    scen@modInp@sets$levy <- .levy_names(scen@model)
    dup <- unique(scen@modInp@sets$levy[duplicated(scen@modInp@sets$levy)])
    if (length(dup)) {
      stop("A tax and a subsidy share the name ", paste0("'", dup, "'", collapse = ", "),
           ".", call. = FALSE)
    }
    p <- scen@modInp@parameters$levy
    p@data <- .empty_param_data(p)
    scen@modInp@parameters$levy <- d2p(p, scen@modInp@sets$levy, NULL)
  }

  # -- the delta, in a throwaway copy ----------------------------------------
  delta <- scen
  for (pn in c(levy_par, levy_map)) {
    p <- delta@modInp@parameters[[pn]]
    if (!is.null(p)) {
      p@data <- .empty_param_data(p)
      delta@modInp@parameters[[pn]] <- p
    }
  }
  for (o in levies) delta <- ob2mi(delta, o, list())
  delta <- .flush_pending_parameters(delta)
  if (length(levies)) {
    delta <- interpolate_parameters(delta, drop_default = isTRUE(scen@status$drop_default),
                                    params = levy_par)
    delta <- .filter_params_by_declared_timeslices(delta, params = levy_par)
    delta <- .filter_params_by_declared_regions(delta, params = levy_par)
    nofmp <- function(x) NULL
    delta <- map_mTaxCost(delta, nofmp)
    delta <- map_mSubCost(delta, nofmp)
  }
  old_pars <- names(delta@modInp@parameters)
  old_cns <- names(delta@modInp@user_constraints)
  delta@modInp@user_costs <- character()
  if (length(cns)) {
    approxim <- .constraint_approxim(delta)
    for (o in cns) delta <- ob2mi(delta, o, list(approxim = approxim))
  }
  new_pars <- setdiff(names(delta@modInp@parameters), old_pars)
  dpars <- c(levy_par, levy_map, new_pars)
  if (isTRUE(scen@status$pruned)) {
    delta <- drop_default_values(delta, params = dpars)
  }
  fold_dims <- as.character(scen@status$folded %||% character())
  if (length(fold_dims)) {
    delta <- fold_parameters(delta, dims = fold_dims, params = dpars)
    delta <- unfold_parameters(delta, dims = .foldable_dims, types = "map",
                               params = dpars)
  } else {
    delta <- unfold_parameters(delta, dims = .foldable_dims, params = dpars)
  }

  # -- the delta joins the copy ------------------------------------------------
  for (pn in c(levy_par, levy_map)) {
    p <- delta@modInp@parameters[[pn]]
    if (is.null(p)) next
    d <- get_data_slot(p)
    if (NROW(d)) {
      scen@modInp@parameters[[pn]] <- d2p(scen@modInp@parameters[[pn]],
                                          as.data.frame(d), NULL)
    }
  }
  for (pn in new_pars) scen@modInp@parameters[[pn]] <- delta@modInp@parameters[[pn]]
  new_cns <- setdiff(names(delta@modInp@user_constraints), old_cns)
  scen@modInp@user_constraints[new_cns] <- delta@modInp@user_constraints[new_cns]
  terms <- c(.unwrap_user_costs(scen@modInp@user_costs), delta@modInp@user_costs)
  rm(delta)

  # -- cost aggregation over the whole scenario --------------------------------
  base_cells <- .coarse_cost_cells(scen)
  for (pn in agg_map) {
    p <- scen@modInp@parameters[[pn]]
    if (!is.null(p)) {
      p@data <- .empty_param_data(p)
      scen@modInp@parameters[[pn]] <- p
    }
  }
  scen@modInp@user_costs <- terms
  scen <- build_mappings(scen, fmp = NULL, recipes = "cost_agg")
  scen <- .extend_discount_to_coarse(scen, skip = base_cells)
  scen@modInp@user_costs <- .wrap_user_costs(terms)

  .en_log("interpolate", scen@name,
          duration = difftime(Sys.time(), t0, units = "secs"),
          model = scen@model@name, base = base@name)
  if (verbose) {
    message("Derived scenario '", scen@name, "' from '", base@name, "' in ",
            format(round(difftime(Sys.time(), t0, units = "secs"), 1)))
  }
  scen
}
