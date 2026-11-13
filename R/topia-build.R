# ============================================================================ #
# build_topia() -- TOPIA instances cut from the shipped data files.
#
# `vignettes/topia-build.Rmd` generates the model files; this reads those files
# and selects from them. It authors nothing -- no newTechnology(), no cost, no
# corridor -- so the vignette stays the only place a TOPIA object is declared,
# and `build_topia()` with every argument at its default returns the shipped
# kit UNCHANGED. That identity is the drift gate, and it holds because each
# step below returns its input untouched when its argument is at the default,
# rather than because the default is a special case.
#
# Arguments that are not built yet are REFUSED, not ignored: a case that asks
# for one must fail rather than come back quietly wrong.
# ============================================================================ #

# The four levers, in the order the shipped kit carries them.
#' @noRd
.TB_LEVERS <- c("CO2_CAP", "CT_CO2", "EARLY_RET", "GREEN_H2")

# preset -> the arguments it stands for. A preset is sugar: it expands and
# says so, so it cannot hide a choice.
#' @noRd
.TB_PRESETS <- list(
  full   = list(),
  island = list(regions = "R01"),
  zone   = list(regions = "Z3")
)

#' @noRd
.tb_kit <- function() {
  k <- .topia_ds()$modules$electricity
  if (is.null(k) || !methods::is(k$repo, "repository")) {
    stop("the TOPIA dataset carries no electricity model; rebuild it with ",
         "`source(\"data-raw/topia_assemble.R\")`.", call. = FALSE)
  }
  k
}

# Refuse an argument whose step is not built yet, naming it.
#' @noRd
.tb_todo <- function(asked, arg, ...) {
  if (!isTRUE(asked)) return(invisible(NULL))
  stop("build_topia(", arg, " = ) is not built yet: ", paste0(...),
       ". It is refused rather than ignored -- a model that quietly kept the ",
       "default would be wrong in a way nothing downstream could see.",
       call. = FALSE)
}

# Which provinces a request stands for.
#
# Accepts codes at ANY geoframe -- a zone or the nation expands to the
# provinces beneath it, so a request never cuts a zone in half by accident --
# or an integer for the first n in canonical order. The result is always in
# the model's own order, whatever order it was asked in.
#' @noRd
.tb_resolve_regions <- function(x, all_r, gs) {
  if (is.null(x)) return(all_r)
  if (is.numeric(x)) {
    n <- suppressWarnings(as.integer(x))
    if (length(n) != 1L || is.na(n) || n < 1L || n > length(all_r)) {
      stop("build_topia(regions = ) as a count must be one whole number ",
           "between 1 and ", length(all_r), "; got: ",
           paste(x, collapse = ", "), call. = FALSE)
    }
    return(all_r[seq_len(n)])
  }
  check_package("geoscales")
  x <- as.character(x)
  frames <- geoscales::geoscale_geoframes(gs)
  fine <- frames[length(frames)]
  lt <- as.data.frame(geoscales::geoscale_leaftable(gs))
  atoms <- as.character(lt[[fine]])
  out <- character(0)
  for (code in x) {
    f <- frames[vapply(frames, function(n) code %in% as.character(lt[[n]]),
                       logical(1))]
    if (!length(f)) {
      stop("build_topia(regions = ): \"", code, "\" is not a code at any ",
           "geoframe of this geoscale. Known: ",
           paste(vapply(frames, function(n)
             paste0(n, " ", paste(unique(stats::na.omit(as.character(lt[[n]]))),
                                  collapse = "/")), character(1)),
             collapse = "; "), call. = FALSE)
    }
    out <- c(out, atoms[as.character(lt[[f[1]]]) == code])
  }
  all_r[all_r %in% unique(out)]
}

# The one repository a model's `@data` stands for.
#' @noRd
.tb_repo_of <- function(mod, name = "topia") {
  d <- mod@data
  if (length(d) == 1L) d[[1]] else
    do.call(newRepository, c(list(name = name), unname(d)))
}

# The levers, narrowed to the provinces that survived.
#
# They are NOT in the repository, so `subset_model_regions()` never sees them
# and two of the four would otherwise reach a smaller model still shaped for
# twelve provinces:
#
#   GREEN_H2  `for.each` names every (region, year, timeslice) -- 315,360 rows
#             -- and the ones naming a dropped province reference a region the
#             model no longer declares.
#   CO2_CAP   the cap is a per-province allowance times the province count, so
#             the shipped 60,000/30,000 is twelve allowances. Left alone on a
#             three-province model it is four times the emissions it is meant
#             to bind and simply never binds.
#
# The rescaling is by COUNT, because that is how the lever was built; it is
# not a measured share of emissions or demand, and a case that needs one
# should say so rather than inherit this.
#
#   CT_CO2    `region` is NA, the wildcard -- unchanged.
#   EARLY_RET `for.each` is year x tech, a per-province ceiling -- unchanged.
#' @noRd
.tb_levers <- function(kit, which, keep, n_all) {
  if (isFALSE(which)) return(list())
  nms <- .TB_LEVERS
  if (is.character(which)) {
    unknown <- setdiff(which, nms)
    if (length(unknown)) {
      stop("build_topia(levers = ): unknown lever(s) ",
           paste(unknown, collapse = ", "), ". TOPIA ships: ",
           paste(nms, collapse = ", "), call. = FALSE)
    }
    nms <- nms[nms %in% which]
  } else if (!isTRUE(which)) {
    stop("build_topia(levers = ) takes TRUE, FALSE, or lever names.",
         call. = FALSE)
  }
  lv <- kit[nms]
  if (length(keep) == n_all) return(lv)

  if (!is.null(lv$GREEN_H2)) {
    fe <- lv$GREEN_H2@for.each
    if (is.data.frame(fe) && "region" %in% names(fe)) {
      lv$GREEN_H2@for.each <- fe[as.character(fe$region) %in% keep, ,
                                 drop = FALSE]
    }
  }
  if (!is.null(lv$CO2_CAP)) {
    r <- lv$CO2_CAP@rhs
    if (is.data.frame(r) && "rhs" %in% names(r)) {
      r$rhs <- r$rhs * length(keep) / n_all
      lv$CO2_CAP@rhs <- r
    }
  }
  lv
}

# What was built, at the call site: the size trade-off is a choice, so it is
# reported where the choice is made rather than discovered at solve time.
#' @noRd
.tb_report <- function(kit, cal, note) {
  ob <- getObject(kit$repo)
  cls <- sort(table(vapply(ob, function(o) class(o)[1], character(1))),
              decreasing = TRUE)
  nts <- tryCatch(nrow(as.data.frame(cal@timeslice_share)),
                  error = function(e) NA_integer_)
  message("build_topia: ", length(kit$regions), " regions, ",
          cal@name, " (", nts, " timeslices), ", length(ob), " objects -- ",
          paste(sprintf("%s %d", names(cls), as.integer(cls)),
                collapse = ", "),
          if (nzchar(note)) paste0("\n  ", note) else "")
}

#' TOPIA instances, cut from the shipped model
#'
#' @description
#' The dataset ships ONE model: twelve provinces, hourly, 51 objects. This cuts
#' smaller cases from it -- one province, one zone, a sample across zones --
#' in a single call, resolving as it goes the things that are tedious and
#' error-prone to do by hand.
#'
#' @details
#' `build_topia()` authors nothing. Every object it returns was declared in
#' *TOPIA I: building the model*, which generates the shipped data files; this
#' selects from them. A technology TOPIA does not declare cannot be asked for,
#' and new processes are added to the article rather than here.
#'
#' With every argument at its default the result is `topia$modules$electricity`
#' **unchanged**, which is what `tests/testthat/test-topia-build.R` pins.
#'
#' `regions` takes codes at any geoframe. A zone code expands to the provinces
#' beneath it, so a request never cuts a zone in half by accident -- which
#' matters because `COA` and `GAS` balance at the zone, and a subset that
#' splits one is refused by [subset_model_regions()] rather than averaged.
#'
#' The two levers that carry a spatial shape are narrowed with the model:
#' `GREEN_H2`, whose `for.each` names every province, and `CO2_CAP`, whose cap
#' is a per-province allowance times the province count and so is rescaled by
#' the count kept. `CT_CO2` and `EARLY_RET` have no province dimension.
#'
#' `layout` chooses which of the four pictures the model is drawn on. The
#' corridor network is the SAME on all four -- fourteen corridors frozen in
#' `data-raw/topia_links.R`, not derived from any one layout -- so a layout
#' changes the map and nothing the model solves.
#'
#' @param preset one of `"full"`, `"island"` (`R01`, no wire and no domestic
#'   fuel) or `"zone"` (`Z3`, carrying the nuclear station and the reservoir).
#'   A preset expands to the arguments below and reports what it chose.
#' @param regions province codes, zone codes, the nation code, or a count for
#'   the first *n* in canonical order. `NULL` keeps all twelve.
#' @param layout `"squares"`, `"honeycomb"` (the default, and the one the
#'   shipped model carries), `"island"` or `"continent"`.
#' @param level the geoframe the model's own regions are. Only `"province"` is
#'   built so far.
#' @param calendar,horizon not built yet; only the shipped values are accepted.
#' @param techs,modules,clusters,demand,weather not built yet.
#' @param levers `TRUE` for all four, `FALSE` for none, or names.
#' @param as `"kit"` (the named list the dataset itself uses, so the two are
#'   interchangeable), `"model"` (a ready [newModel()] with the geoscale set),
#'   or `"repository"`.
#' @param quiet suppress the one-line report of what was built.
#' @param boundary_prices passed to [subset_model_regions()] to price the
#'   corridors a subset cuts.
#' @return A kit (named list), a `model`, or a `repository`.
#' @seealso [topia] for the dataset, [subset_model_regions()] and
#'   [aggregate_model_regions()] for the reduction verbs this sits beside.
#' @export
#' @examples
#' \dontrun{
#' # the shipped model, unchanged
#' identical(build_topia(), topia$modules$electricity)
#'
#' # one zone, ready to interpolate
#' mod <- build_topia("Z3", as = "model")
#'
#' # the island, by name
#' kit <- build_topia(preset = "island")
#' }
build_topia <- function(regions = NULL,
                        preset = NULL,
                        layout = "honeycomb",
                        level = "province",
                        calendar = "d365_h24",
                        horizon = "BY2025_50_by10",
                        techs = NULL,
                        modules = NULL,
                        clusters = TRUE,
                        demand = 1,
                        weather = NULL,
                        levers = TRUE,
                        as = c("kit", "model", "repository"),
                        boundary_prices = NULL,
                        quiet = FALSE) {
  as <- match.arg(as)
  ds <- .topia_ds()
  kit <- .tb_kit()
  note <- ""

  # -- presets: expand and say so -------------------------------------------
  if (!is.null(preset)) {
    preset <- as.character(preset)[1]
    if (!preset %in% names(.TB_PRESETS)) {
      stop("build_topia(preset = ): unknown preset \"", preset,
           "\". Built so far: ", paste(names(.TB_PRESETS), collapse = ", "),
           ".", call. = FALSE)
    }
    args <- .TB_PRESETS[[preset]]
    if (!is.null(args$regions)) {
      if (!is.null(regions)) {
        stop("build_topia(): give `preset` or `regions`, not both -- ",
             "preset \"", preset, "\" already sets regions = ",
             paste(args$regions, collapse = ", "), ".", call. = FALSE)
      }
      regions <- args$regions
    }
    note <- paste0("preset \"", preset, "\" = regions ",
                   paste(args$regions %||% "all", collapse = ", "))
  }

  # -- not built yet, refused rather than ignored ---------------------------
  .tb_todo(!identical(level, "province"), "level",
           "coarsening to zone or nation aggregates the source tables")
  .tb_todo(!identical(calendar, "d365_h24"), "calendar",
           "another calendar needs demand and weather re-timed through ",
           "topia_profiles()")
  .tb_todo(!identical(horizon, "BY2025_50_by10"), "horizon",
           "a different horizon re-keys the year-indexed slots")
  .tb_todo(!is.null(techs), "techs", "selecting processes needs the ",
           "dependency closure")
  .tb_todo(!is.null(modules), "modules", "selecting repository groups needs ",
           "the dependency closure")
  .tb_todo(!isTRUE(clusters), "clusters",
           "re-grading the VRE clusters merges weather series")
  .tb_todo(!identical(demand, 1) && !identical(demand, 1L), "demand",
           "rescaling demand re-normalises the year")
  .tb_todo(!is.null(weather), "weather", "swapping the series re-times them")

  # -- space ----------------------------------------------------------------
  if (!layout %in% names(ds$geoscales)) {
    stop("build_topia(layout = ): unknown layout \"", layout, "\". TOPIA ",
         "ships: ", paste(names(ds$geoscales), collapse = ", "), ".",
         call. = FALSE)
  }
  gs <- ds$geoscales[[layout]]
  all_r <- as.character(kit$regions)
  keep <- .tb_resolve_regions(regions, all_r, gs)
  if (!length(keep)) {
    stop("build_topia(regions = ) selected nothing.", call. = FALSE)
  }

  cal <- ds$modules$calendars[[calendar]]
  hor <- ds$modules$horizons[[horizon]]

  whole <- length(keep) == length(all_r)
  if (whole && identical(gs, kit$geoscale)) {
    # Nothing to cut: the shipped objects are returned as they are, which is
    # what makes the default an identity rather than a rebuild.
    repo <- kit$repo
    geo <- kit$geoscale
    mod <- NULL
  } else {
    mod <- newModel(name = "TOPIA", currency = "MUSD", region = all_r,
                    calendar = cal, horizon = hor, discount = 0.05,
                    data = kit$repo)
    mod <- setGeoscale(mod, gs)
    if (!whole) {
      # The reduction verb does the surgery: it drops the corridors that leave
      # the subset, prunes the weather objects the dropped provinces owned
      # (and the references to them), refuses a cut that splits a zone, and
      # filters the geoscale.
      # `verbose = FALSE` drops the per-object listing but keeps the one-line
      # summary, which is a `message()`; `quiet` means quiet.
      sub1 <- function() subset_model_regions(mod, keep,
                                              boundary_prices = boundary_prices,
                                              verbose = FALSE)
      mod <- if (quiet) suppressMessages(sub1()) else sub1()
    }
    repo <- .tb_repo_of(mod)
    geo <- mod@config@geoscale
  }

  lv <- .tb_levers(kit, levers, keep, length(all_r))

  out <- c(list(regions = keep, calendar = kit$calendar, repo = repo,
                geoscale = geo), lv)

  if (!quiet) .tb_report(out, cal, note)

  switch(as,
    kit = out,
    repository = repo,
    model = {
      if (is.null(mod)) {
        mod <- newModel(name = "TOPIA", currency = "MUSD", region = keep,
                        calendar = cal, horizon = hor, discount = 0.05,
                        data = repo)
        mod <- setGeoscale(mod, geo)
      }
      mod
    })
}
