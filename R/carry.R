# =============================================================================#
# carry.R — sequential-solving primitives: the solution ledger and its
# application to a model (Part 2: myopic/sequential solving).
#
# The seam has two halves:
#   solution_ledger(scen, years)   WHAT was decided — pure extraction of the
#                                  capacity/retirement/budget outcomes of the
#                                  decided milestone years of a solved
#                                  scenario, with the solving horizon's period
#                                  lengths and each process's lifetime frozen
#                                  in (windows may differ in interval shape).
#   model_apply_ledger(mod, ...)   HOW it enters the next problem — a model
#                                  COPY whose objects carry the decided past
#                                  as dense exogenous `stock` schedules (the
#                                  recursive stock balance and its Surv/New
#                                  split are re-derived by the normal
#                                  interpolation pipeline), with cumulative
#                                  budgets debited.
#
# Every application starts from the PRISTINE model and applies the FULL
# ledger, so double-counting is structurally impossible. Capacity carried
# forward mirrors eqTechCap exactly: a vintage built at yp with per-year rate
# r stands at plen(yp)*r while y - yp < olife (or forever when olife is
# infinite), minus its recorded retirements. Endogenous retirements of
# CARRIED stock in later steps are attributed to the carried pool
# oldest-vintage-first (FIFO); the user-stock share follows from the factual
# terminal stock. Carried capacity enters later steps as `stock`, which pays
# no EAC — sunk investment, by design (see solve_myopic()).
# =============================================================================#

# family spec: how each process class maps to variables, lifetime, and the
# stock column(s) of its @capacity slot
.carry_families <- function() {
  list(
    tech = list(
      class = "technology", proc = "tech",
      newcap = "vTechNewCap", retnew = "vTechRetiredNewCap",
      stockcap = "vTechStockCap", retstock = "vTechRetiredStock",
      olife = "pTechOlife", olife_inf = "mTechOlifeInf",
      stock_col = "stock", has_region = TRUE
    ),
    stg_out = list(
      class = "storage", proc = "stg",
      newcap = "vStorageOutNewCap", retnew = "vStorageOutRetiredNewCap",
      stockcap = "vStorageOutStockCap", retstock = "vStorageOutRetiredStock",
      olife = "pStorageOlife", olife_inf = "mStorageOlifeInf",
      stock_col = "out.stock", has_region = TRUE
    ),
    stg_inp = list(
      class = "storage", proc = "stg",
      newcap = "vStorageInpNewCap", retnew = "vStorageInpRetiredNewCap",
      stockcap = "vStorageInpStockCap", retstock = "vStorageInpRetiredStock",
      olife = "pStorageOlife", olife_inf = "mStorageOlifeInf",
      stock_col = "inp.stock", has_region = TRUE
    ),
    stg_stg = list(
      class = "storage", proc = "stg",
      newcap = "vStorageStgNewCap", retnew = "vStorageStgRetiredNewCap",
      stockcap = "vStorageStgStockCap", retstock = "vStorageStgRetiredStock",
      olife = "pStorageOlife", olife_inf = "mStorageOlifeInf",
      stock_col = "stg.stock", has_region = TRUE
    ),
    trade = list(
      class = "trade", proc = "trade",
      newcap = "vTradeNewCap", retnew = "vTradeRetiredNewCap",
      stockcap = "vTradeStockCap", retstock = "vTradeRetiredStock",
      olife = "pTradeOlife", olife_inf = "mTradeOlifeInf",
      stock_col = "stock", has_region = FALSE
    )
  )
}

.ledger_empty_tab <- function() {
  tibble(family = character(0), proc = character(0), region = character(0),
         vintage = integer(0), cap = numeric(0), olife = numeric(0))
}

.ledger_new <- function() {
  structure(list(
    newcap = .ledger_empty_tab(),
    retnew = tibble(family = character(0), proc = character(0),
                    region = character(0), vintage = integer(0),
                    ret_year = integer(0), amount = numeric(0)),
    retstock = tibble(family = character(0), proc = character(0),
                      region = character(0), amount = numeric(0)),
    terminal = tibble(family = character(0), proc = character(0),
                      region = character(0), y_last = integer(0),
                      value = numeric(0)),
    sup_use = tibble(sup = character(0), comm = character(0),
                     region = character(0), consumed = numeric(0)),
    provenance = NULL,
    steps = tibble(y_first = integer(0), y_last = integer(0))
  ), class = "carry_ledger")
}

# variable data as a data.frame, empty tibble when absent/empty
.ledger_var <- function(scen, name) {
  d <- tryCatch(suppressMessages(getData(scen, name, merge = TRUE)),
                error = function(e) NULL)
  if (is.null(d) || !is.data.frame(d) || nrow(d) == 0) return(NULL)
  d
}

# per-milestone period length of the SOLVING horizon
.ledger_plen <- function(scen) {
  iv <- scen@settings@horizon@intervals
  stats::setNames(as.numeric(iv$end - iv$start + 1L), as.character(iv$mid))
}

# olife per (proc, region): finite values from the parameter, Inf where the
# OlifeInf map names the process
.ledger_olife <- function(scen, fam) {
  ol <- .ledger_var(scen, fam$olife)
  inf <- .ledger_var(scen, fam$olife_inf)
  list(
    finite = ol,
    inf = inf
  )
}

.olife_of <- function(ol, fam, proc, region) {
  if (!is.null(ol$inf) && nrow(ol$inf)) {
    ii <- ol$inf[[fam$proc]] == proc
    if (fam$has_region && "region" %in% names(ol$inf)) {
      ii <- ii & (as.character(ol$inf$region) == region)
    }
    if (any(ii, na.rm = TRUE)) return(Inf)
  }
  if (is.null(ol$finite) || !nrow(ol$finite)) return(Inf)
  ii <- as.character(ol$finite[[fam$proc]]) == proc
  if (fam$has_region && "region" %in% names(ol$finite)) {
    ii <- ii & (as.character(ol$finite$region) == region)
  }
  v <- ol$finite$value[ii]
  if (length(v) == 0 || is.na(v[1])) Inf else as.numeric(v[1])
}

#' Extract the carry-forward ledger from a solved scenario
#'
#' @description
#' The extraction half of the sequential-solving seam: reads WHAT a solved
#' scenario decided in its (decided) milestone years — new capacity by
#' vintage (with the solving horizon's period length and the process
#' lifetime frozen in), retirements of that capacity, endogenous stock
#' retirements, the terminal exogenous-stock level, and cumulative supply
#' use — into a `carry_ledger`. Pure read: nothing is modified. Ledgers of
#' consecutive steps accumulate; [model_apply_ledger()] turns the
#' accumulated ledger into the next step's model.
#'
#' @param scen a solved scenario.
#' @param years integer, the DECIDED milestone years to extract (default:
#'   all milestones of the scenario's horizon). With look-ahead windows,
#'   restrict to the decided years so look-ahead decisions are discarded.
#' @param ledger an existing `carry_ledger` to append to (default: empty).
#'
#' @return a `carry_ledger` object.
#' @seealso [model_apply_ledger()], [solve_myopic()]
#' @export
solution_ledger <- function(scen, years = NULL, ledger = NULL) {
  stopifnot(is(scen, "scenario"))
  if (!isTRUE(scen@status$optimal)) {
    stop("solution_ledger() needs a solved scenario ('",
         scen@name, "' is not solved to optimal).")
  }
  L <- ledger %||% .ledger_new()
  stopifnot(inherits(L, "carry_ledger"))
  mids <- as.integer(scen@settings@horizon@intervals$mid)
  years <- as.integer(years %||% mids)
  years <- intersect(years, mids)
  if (length(years) == 0) stop("No decided years to extract")
  plen <- .ledger_plen(scen)
  y_last <- max(years)

  for (fk in names(.carry_families())) {
    fam <- .carry_families()[[fk]]
    ol <- .ledger_olife(scen, fam)
    reg_of <- function(d) {
      if (fam$has_region && "region" %in% names(d)) {
        as.character(d$region)
      } else {
        rep(NA_character_, nrow(d))
      }
    }

    nc <- .ledger_var(scen, fam$newcap)
    if (!is.null(nc)) {
      nc <- nc[nc$year %in% years & nc$value > 0, , drop = FALSE]
      if (nrow(nc)) {
        rr <- reg_of(nc)
        L$newcap <- bind_rows(L$newcap, tibble(
          family = fk,
          proc = as.character(nc[[fam$proc]]),
          region = rr,
          vintage = as.integer(nc$year),
          cap = nc$value * plen[as.character(nc$year)],
          olife = mapply(function(p, r) .olife_of(ol, fam, p, r),
                         as.character(nc[[fam$proc]]), rr)
        ))
      }
    }

    rn <- .ledger_var(scen, fam$retnew)
    if (!is.null(rn)) {
      # (proc[, region], year = vintage, yearp = retirement year)
      rn <- rn[rn$year %in% years & rn$value > 0, , drop = FALSE]
      if (nrow(rn)) {
        L$retnew <- bind_rows(L$retnew, tibble(
          family = fk,
          proc = as.character(rn[[fam$proc]]),
          region = reg_of(rn),
          vintage = as.integer(rn$year),
          ret_year = as.integer(rn$yearp),
          amount = rn$value * plen[as.character(rn$yearp)]
        ))
      }
    }

    rs <- .ledger_var(scen, fam$retstock)
    if (!is.null(rs)) {
      rs <- rs[rs$year %in% years & rs$value > 0, , drop = FALSE]
      if (nrow(rs)) {
        rs$.amount <- rs$value * plen[as.character(rs$year)]
        L$retstock <- bind_rows(L$retstock, tibble(
          family = fk,
          proc = as.character(rs[[fam$proc]]),
          region = reg_of(rs),
          amount = rs$.amount
        ))
      }
    }

    ts <- .ledger_var(scen, fam$stockcap)
    if (!is.null(ts)) {
      ts <- ts[ts$year == y_last, , drop = FALSE]
      if (nrow(ts)) {
        keep <- tibble(
          family = fk,
          proc = as.character(ts[[fam$proc]]),
          region = reg_of(ts),
          y_last = y_last,
          value = ts$value
        )
        # the LATEST terminal supersedes earlier ones for the same key
        L$terminal <- bind_rows(
          anti_join(L$terminal, keep, by = c("family", "proc", "region")),
          keep
        )
      }
    }
  }

  so <- .ledger_var(scen, "vSupOut")
  if (!is.null(so)) {
    so <- so[so$year %in% years, , drop = FALSE]
    if (nrow(so)) {
      so$.consumed <- so$value * plen[as.character(so$year)]
      grp <- intersect(c("sup", "comm", "region"), names(so))
      add <- so |>
        group_by(across(all_of(grp))) |>
        summarise(consumed = sum(.data$.consumed), .groups = "drop") |>
        mutate(across(all_of(grp), as.character))
      for (m in setdiff(c("sup", "comm", "region"), grp)) {
        add[[m]] <- NA_character_
      }
      L$sup_use <- bind_rows(L$sup_use, add)
    }
  }

  prov <- scen@modInp@sets$variant
  if (!is.null(prov) && is.data.frame(prov) && nrow(prov)) {
    L$provenance <- unique(bind_rows(L$provenance, as_tibble(prov)))
  }
  L$steps <- bind_rows(L$steps,
                       tibble(y_first = min(years), y_last = y_last))
  L
}

#' @method print carry_ledger
#' @export
print.carry_ledger <- function(x, ...) {
  cat("carry_ledger: ", nrow(x$steps), " step(s)",
      if (nrow(x$steps)) paste0(" (", min(x$steps$y_first), "-",
                                max(x$steps$y_last), ")"),
      "\n", sep = "")
  cat("  new-capacity vintages: ", nrow(x$newcap),
      " | retirements: ", nrow(x$retnew),
      " | stock retirements: ", nrow(x$retstock), "\n", sep = "")
  cat("  terminal stock keys: ", nrow(x$terminal),
      " | supply-use keys: ", nrow(x$sup_use), "\n", sep = "")
  invisible(x)
}

# standing carried capacity of one (family, proc, region) at year y:
# eqTechCap's vintage arithmetic, minus recorded new-cap retirements, minus
# the FIFO share of endogenous stock retirements
.carried_at <- function(L, fk, proc, region, y) {
  vv <- L$newcap[L$newcap$family == fk & L$newcap$proc == proc &
                   (L$newcap$region %in% region |
                      (is.na(L$newcap$region) & is.na(region))), ,
                 drop = FALSE]
  if (nrow(vv) == 0) return(0)
  vv <- vv[order(vv$vintage), , drop = FALSE]
  rn <- L$retnew[L$retnew$family == fk & L$retnew$proc == proc &
                   (L$retnew$region %in% region |
                      (is.na(L$retnew$region) & is.na(region))), ,
                 drop = FALSE]
  fifo <- sum(L$retstock$amount[
    L$retstock$family == fk & L$retstock$proc == proc &
      (L$retstock$region %in% region |
         (is.na(L$retstock$region) & is.na(region)))])
  total <- 0
  for (i in seq_len(nrow(vv))) {
    alive <- is.infinite(vv$olife[i]) || (y - vv$vintage[i] < vv$olife[i])
    if (!alive) next
    ret_i <- sum(rn$amount[rn$vintage == vv$vintage[i] & rn$ret_year <= y])
    net <- max(0, vv$cap[i] - ret_i)
    # FIFO: endogenous stock retirements consume the oldest carried vintages
    eat <- min(net, fifo)
    fifo <- fifo - eat
    total <- total + (net - eat)
  }
  total
}

# evaluate the user's declared stock schedule at `years`: linear between
# declared years; before the first declared year hold it; after the last one
# 0 for technologies (interpolation `.inter.`, falls to the 0 default) and
# held for storage/trade (`back.inter.forth`)
.user_stock_eval <- function(schedule, years, hold_end) {
  if (is.null(schedule) || nrow(schedule) == 0) {
    return(stats::setNames(rep(0, length(years)), years))
  }
  s <- schedule[order(schedule$year), , drop = FALSE]
  out <- stats::approx(s$year, s$value, xout = years,
                       method = "linear", rule = 2)$y
  if (!hold_end) out[years > max(s$year)] <- 0
  stats::setNames(out, years)
}

# roll the user-stock share forward from an anchor level, using the RATIOS of
# the declared schedule between consecutive grid years (the .stock_path_one
# split: commissioning stream + survival share)
.user_stock_roll <- function(anchor, y_anchor, schedule, mids, hold_end) {
  grid <- c(y_anchor, mids)
  sched <- .user_stock_eval(schedule, grid, hold_end)
  out <- numeric(length(mids))
  lev <- anchor
  prev <- sched[[1]]
  for (i in seq_along(mids)) {
    cur <- sched[[i + 1]]
    new_i <- max(0, cur - prev)
    surv_i <- if (prev <= 0) 1 else max(0, (cur - new_i) / prev)
    lev <- surv_i * lev + new_i
    out[i] <- lev
    prev <- cur
  }
  stats::setNames(out, mids)
}

#' Apply a carry ledger to a model
#'
#' @description
#' The application half of the sequential-solving seam: returns a COPY of
#' the (pristine) model whose technology / storage / trade objects carry the
#' ledger's decided past as dense exogenous `stock` schedules over the
#' target horizon's milestones, and whose supply objects have their
#' cumulative reserves debited by past use. Carried capacity stands while
#' `year - vintage < olife` (mirroring the model's own capacity equation)
#' and enters as `stock` — investment-cost-free, i.e. sunk.
#'
#' @param mod the pristine model (never a previously-applied copy: each
#'   application applies the FULL ledger from scratch).
#' @param ledger a `carry_ledger` from [solution_ledger()].
#' @param horizon the horizon of the NEXT step (its milestone `mid` years
#'   receive the dense stock rows).
#' @param carry character, what to carry: `"capacity"` (with its
#'   retirements) and/or `"budgets"` (supply reserves debited).
#' @param tolerance numeric, carried values below it are written as zero.
#' @param verbose logical.
#'
#' @return a model object.
#' @seealso [solution_ledger()], [solve_myopic()]
#' @export
model_apply_ledger <- function(mod, ledger, horizon,
                               carry = c("capacity", "budgets"),
                               tolerance = 1e-6, verbose = FALSE) {
  stopifnot(is(mod, "model"), inherits(ledger, "carry_ledger"),
            is(horizon, "horizon"))
  mids <- as.integer(horizon@intervals$mid)
  carry <- match.arg(carry, c("capacity", "retirements", "budgets"),
                     several.ok = TRUE)
  if ("retirements" %in% carry && !("capacity" %in% carry)) {
    warning("carry = \"retirements\" without \"capacity\" has no effect: ",
            "retirements are consumed inside the capacity carry.",
            call. = FALSE)
  }

  if ("capacity" %in% carry) {
    keys <- unique(bind_rows(
      ledger$newcap[, c("family", "proc", "region")],
      ledger$terminal[, c("family", "proc", "region")]
    ))
    for (i in seq_len(nrow(keys))) {
      fk <- keys$family[i]
      fam <- .carry_families()[[fk]]
      proc <- keys$proc[i]
      region <- keys$region[i]

      # solved (possibly variant-expanded) name -> base object + selectors
      base <- proc
      sel <- list()
      pv <- ledger$provenance
      if (!is.null(pv) && proc %in% pv$name) {
        pr <- pv[pv$name == proc, , drop = FALSE][1, ]
        base <- pr$base
        if (!is.na(pr$vintage %||% NA)) sel$vintage <- pr$vintage
        if (!is.na(pr$cluster %||% NA)) sel$cluster <- pr$cluster
      }
      hit <- .model_find_object(mod, base, fam$class)
      if (is.null(hit)) {
        warning("Ledger names ", fam$class, " '", base,
                "' which the model does not contain; skipped.",
                call. = FALSE)
        next
      }
      obj <- hit$obj

      # user-declared schedule for this key (NA selector rows apply too)
      cap <- obj@capacity
      sched <- NULL
      if (is.data.frame(cap) && nrow(cap) &&
          fam$stock_col %in% names(cap)) {
        rows <- rep(TRUE, nrow(cap))
        if (fam$has_region && "region" %in% names(cap)) {
          rows <- rows & (is.na(cap$region) | cap$region %in% region)
        }
        for (s in names(sel)) {
          if (s %in% names(cap)) {
            rows <- rows & (is.na(cap[[s]]) | cap[[s]] %in% sel[[s]])
          }
        }
        rows <- rows & !is.na(cap[[fam$stock_col]]) & !is.na(cap$year)
        if (any(rows)) {
          sched <- data.frame(year = as.integer(cap$year[rows]),
                              value = as.numeric(cap[[fam$stock_col]][rows]))
        }
      }
      hold_end <- fam$class %in% c("storage", "trade")

      term <- ledger$terminal[
        ledger$terminal$family == fk & ledger$terminal$proc == proc &
          (ledger$terminal$region %in% region |
             (is.na(ledger$terminal$region) & is.na(region))), ,
        drop = FALSE]
      y_anchor <- if (nrow(term)) term$y_last[1] else
        max(ledger$steps$y_last)
      carried_anchor <- .carried_at(ledger, fk, proc, region, y_anchor)
      user_anchor <- if (nrow(term)) {
        max(0, term$value[1] - carried_anchor)
      } else {
        0
      }

      user_path <- .user_stock_roll(user_anchor, y_anchor, sched, mids,
                                    hold_end)
      carried_path <- vapply(mids, function(y)
        .carried_at(ledger, fk, proc, region, y), numeric(1))
      total <- user_path + carried_path
      total[total < tolerance] <- 0

      # replace the schedule: clear this key's stock column on existing rows,
      # append one dense row per milestone
      if (is.data.frame(cap) && nrow(cap) &&
          fam$stock_col %in% names(cap)) {
        rows <- rep(TRUE, nrow(cap))
        if (fam$has_region && "region" %in% names(cap)) {
          rows <- rows & (is.na(cap$region) | cap$region %in% region)
        }
        for (s in names(sel)) {
          if (s %in% names(cap)) {
            rows <- rows & (is.na(cap[[s]]) | cap[[s]] %in% sel[[s]])
          }
        }
        cap[[fam$stock_col]][rows] <- NA
        # drop rows made empty (nothing but keys left)
        num_cols <- setdiff(names(cap)[vapply(cap, is.numeric, logical(1))],
                            "year")
        empty <- rows & !apply(!is.na(cap[, num_cols, drop = FALSE]), 1, any)
        cap <- cap[!empty, , drop = FALSE]
      }
      add <- data.frame(year = mids)
      if (fam$has_region) add$region <- region
      for (s in names(sel)) add[[s]] <- sel[[s]]
      add[[fam$stock_col]] <- as.numeric(total)
      if (!is.data.frame(cap) || nrow(cap) == 0) {
        cap <- add
      } else {
        cap <- bind_rows(cap, add)
      }
      obj@capacity <- as.data.frame(cap)
      mod <- .model_replace_object(mod, hit$repo, obj)
      if (verbose) {
        message("carry: ", fam$class, " '", base, "'",
                if (!is.na(region)) paste0(" [", region, "]"),
                " stock <- ", paste(round(total, 4), collapse = ", "))
      }
    }
  }

  if ("budgets" %in% carry && nrow(ledger$sup_use)) {
    use <- ledger$sup_use |>
      group_by(.data$sup, .data$region) |>
      summarise(consumed = sum(.data$consumed), .groups = "drop")
    for (i in seq_len(nrow(use))) {
      hit <- .model_find_object(mod, use$sup[i], "supply")
      if (is.null(hit)) next
      obj <- hit$obj
      res <- obj@reserve
      if (!is.data.frame(res) || nrow(res) == 0) next
      rows <- rep(TRUE, nrow(res))
      if ("region" %in% names(res) && !is.na(use$region[i])) {
        rows <- rows & (is.na(res$region) | res$region %in% use$region[i])
      }
      for (cc in intersect(c("res.up", "res.fx"), names(res))) {
        res[[cc]][rows] <- pmax(0, res[[cc]][rows] - use$consumed[i])
      }
      if ("res.lo" %in% names(res)) {
        res$res.lo[rows] <- pmax(0, res$res.lo[rows] - use$consumed[i])
      }
      obj@reserve <- res
      mod <- .model_replace_object(mod, hit$repo, obj)
      if (verbose) {
        message("carry: supply '", use$sup[i], "' reserve debited by ",
                round(use$consumed[i], 4))
      }
    }
  }

  mod
}

# locate an object by name (and class) across the model's repositories;
# returns list(repo = <repo name>, obj = <object>) or NULL
.model_find_object <- function(mod, name, class) {
  for (rp in names(mod@data)) {
    repo <- mod@data[[rp]]
    if (!isS4(repo) || !name %in% names(repo@data)) next
    o <- repo@data[[name]]
    if (is(o, class)) return(list(repo = rp, obj = o))
  }
  NULL
}

# replace an object IN PLACE inside its repository (never add(): it rejects
# duplicate names across repositories)
.model_replace_object <- function(mod, repo_name, obj) {
  mod@data[[repo_name]]@data[[obj@name]] <- obj
  mod
}
