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
  zone   = list(regions = "Z3"),
  # the one that has to be quick: a single province, a day per season, and
  # the two plants that make the trade-off visible
  tiny   = list(regions = "R01", calendar = "d365_h24_1dps",
                techs = c("EGAS", "ESOL"))
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

# --- the commodity graph ---------------------------------------------------- #
# Read off the objects, because the repository ships FLAT and nothing else
# records which commodity feeds which process.
#
# The column is not always `comm`: a technology names its auxiliary commodity
# in `acomm`, which is where `LANDS` and `LANDW` live. Reading only `comm`
# leaves solar and wind looking like they consume nothing, and the land pools
# then look unused and get dropped out from under them.
#' @noRd
.tb_vals <- function(o, sl, cols) {
  v <- tryCatch(methods::slot(o, sl), error = function(e) NULL)
  if (!is.data.frame(v) && !is.list(v)) return(character(0))
  cc <- intersect(cols, names(v))
  if (!length(cc)) return(character(0))
  out <- unique(unlist(lapply(v[cc], as.character), use.names = FALSE))
  out[!is.na(out) & nzchar(out)]
}

# What a process takes IN (an aux draw is a draw: land is consumed).
#' @noRd
.tb_in <- function(o) {
  if (methods::is(o, "technology"))
    return(unique(c(.tb_vals(o, "input", "comm"),
                    .tb_vals(o, "aux", c("acomm", "comm")),
                    .tb_vals(o, "aeff", "acomm"))))
  if (methods::is(o, "storage")) return(.tb_vals(o, "input", "comm"))
  if (methods::is(o, c("demand"))) return(.tb_own(o))
  if (methods::is(o, "export")) return(.tb_own(o))
  if (methods::is(o, "trade")) return(.tb_own(o))
  character(0)
}

# What a process puts OUT.
#' @noRd
.tb_out <- function(o) {
  if (methods::is(o, "technology")) return(.tb_vals(o, "output", "comm"))
  if (methods::is(o, "storage")) return(.tb_vals(o, "output", "comm"))
  if (methods::is(o, "supply") || methods::is(o, "import")) return(.tb_own(o))
  if (methods::is(o, "trade")) return(.tb_own(o))
  character(0)
}

# The object's own `@commodity`, where the class has one.
#' @noRd
.tb_own <- function(o) {
  v <- tryCatch(as.character(methods::slot(o, "commodity")),
                error = function(e) character(0))
  v[!is.na(v) & nzchar(v)]
}

#' @noRd
.tb_touches <- function(o) unique(c(.tb_in(o), .tb_out(o), .tb_own(o)))

# The weather series a process names.
#' @noRd
.tb_wrefs <- function(o) .tb_vals(o, "weather", "weather")

# Structural keywords: a VRE plant is one that declares a weather series, a
# thermal one is one that burns its input. Neither is a list kept here.
#' @noRd
.tb_keyword <- function(ob, kw) {
  tech <- Filter(function(o) methods::is(o, "technology"), ob)
  hit <- switch(kw,
    vre = vapply(tech, function(o) length(.tb_wrefs(o)) > 0L, logical(1)),
    thermal = vapply(tech, function(o) {
      v <- tryCatch(methods::slot(o, "input"), error = function(e) NULL)
      is.data.frame(v) && "combustion" %in% names(v) &&
        any(!is.na(v$combustion) & v$combustion > 0)
    }, logical(1)),
    stop("build_topia(techs = ): unknown keyword \"", kw, "\". Built: ",
         "\"vre\" (declares a weather series), \"thermal\" (burns its ",
         "input).", call. = FALSE))
  names(tech)[hit]
}

# Resolve `techs` / `modules` to the names they keep, or NULL for "all".
#
# A selection is either all positive or all negative; mixing them has no
# meaning that is not ambiguous, so it is refused rather than guessed.
#' @noRd
.tb_select <- function(x, universe, arg, expand = identity) {
  if (is.null(x)) return(NULL)
  x <- as.character(x)
  neg <- startsWith(x, "-")
  if (any(neg) && !all(neg)) {
    stop("build_topia(", arg, " = ): keep some or drop some, not both -- ",
         "got ", paste(x, collapse = ", "), ".", call. = FALSE)
  }
  bare <- sub("^-", "", x)
  named <- unique(unlist(lapply(bare, expand), use.names = FALSE))
  unknown <- setdiff(named, universe)
  if (length(unknown)) {
    stop("build_topia(", arg, " = ): unknown ", arg, " ",
         paste(unknown, collapse = ", "), ". TOPIA has: ",
         paste(universe, collapse = ", "), ".", call. = FALSE)
  }
  if (all(neg)) setdiff(universe, named) else named
}

# Everything that is still reachable, to a fixpoint.
#
# Dropping a process orphans whatever existed only to feed it, and the orphans
# orphan more: drop every coal burner and `SUP_COA`, `IMP_COA`, the coal
# corridors and the commodity `COA` itself all have nothing left to do. Doing
# that by hand is where a hand-cut kit ends up with a plant and no fuel, or a
# commodity nothing balances.
#' @noRd
.tb_closure <- function(ob, keep) {
  repeat {
    k <- ob[keep]
    # Who REALLY takes a commodity out of the system and who really puts one
    # in. A storage and a corridor do neither: one time-shifts, the other
    # moves. Counting them makes a commodity keep itself alive -- `STG_H2`
    # both consumes and produces `H2`, a coal corridor sits on both ends of
    # `COA` -- and that cycle props up a supply whose last real consumer is
    # gone, which is how a kit ends up with a fuel nothing burns.
    real <- Filter(function(o)
      methods::is(o, "technology") || methods::is(o, "demand") ||
      methods::is(o, "supply") || methods::is(o, "import") ||
      methods::is(o, "export"), k)
    consumed <- unique(unlist(lapply(real, .tb_in), use.names = FALSE))
    produced <- unique(unlist(lapply(real, .tb_out), use.names = FALSE))
    # A commodity is reachable through its EMISSIONS too, and that is the only
    # way `CO2` is reachable at all: nothing takes it in or puts it out, it is
    # declared on `COA` and `GAS` as `@emis`. Judged on flows alone it looks
    # orphaned, and dropping it takes the carbon cap and the tax with it.
    wanted <- unique(c(
      unlist(lapply(
        Filter(function(o) !methods::is(o, "commodity") &&
                           !methods::is(o, "weather"), k),
        .tb_touches), use.names = FALSE),
      unlist(lapply(k, function(o) .tb_vals(o, "emis", "comm")),
             use.names = FALSE)))
    wref <- unique(unlist(lapply(k, .tb_wrefs), use.names = FALSE))

    drop <- character(0)
    for (nm in keep) {
      o <- ob[[nm]]
      own <- .tb_own(o)
      gone <- if (methods::is(o, "supply") || methods::is(o, "import")) {
        !any(own %in% consumed)          # a source for something nothing takes
      } else if (methods::is(o, "export")) {
        !any(own %in% produced)          # a sink for something nothing makes
      } else if (methods::is(o, "trade")) {
        !any(own %in% consumed) || !any(own %in% produced)
      } else if (methods::is(o, "storage")) {
        c2 <- .tb_vals(o, "input", "comm")
        !any(c2 %in% consumed) || !any(c2 %in% produced)
      } else if (methods::is(o, "weather")) {
        !nm %in% wref
      } else if (methods::is(o, "commodity")) {
        !nm %in% wanted
      } else FALSE
      if (gone) drop <- c(drop, nm)
    }
    if (!length(drop)) break
    keep <- setdiff(keep, drop)
  }
  keep
}

# Every character value an object holds, however deep.
#
# A constraint keeps its terms nested -- `@lhs` is a list of terms, each a
# list whose `for.sum` is another list naming the processes summed over -- so
# a one-level scan never sees `H2ELC` and the lever survives a selection that
# removed it. `unique()` per column keeps the 315,360-row `for.each` cheap.
#' @noRd
.tb_chars <- function(x) {
  # A constraint's terms are S4 `summand` objects, and `is.list()` is FALSE
  # for S4 -- recursion that does not say so stops at the term and never sees
  # the processes it sums over.
  if (isS4(x)) {
    return(unique(unlist(lapply(methods::slotNames(x), function(sl)
      .tb_chars(tryCatch(methods::slot(x, sl), error = function(e) NULL))),
      use.names = FALSE)))
  }
  if (is.data.frame(x)) {
    cc <- Filter(function(z) is.character(z) || is.factor(z), as.list(x))
    return(unique(unlist(lapply(cc, function(z) unique(as.character(z))),
                         use.names = FALSE)))
  }
  if (is.list(x)) {
    return(unique(unlist(lapply(x, .tb_chars), use.names = FALSE)))
  }
  if (is.character(x) || is.factor(x)) return(unique(as.character(x)))
  character(0)
}

# A lever naming a process that is gone cannot be honoured.
#
# `GREEN_H2` sums over `H2ELC`, `ESOL`, `EWIN` and `EHYD` by name; with any of
# them dropped the constraint references a process the model does not declare
# and the model is refused before it is ever interpolated. The lever goes
# with them, and says so.
#' @noRd
.tb_drop_levers <- function(lv, gone, quiet) {
  if (!length(gone) || !length(lv)) return(lv)
  hits <- vapply(lv, function(o) {
    vals <- unique(unlist(lapply(methods::slotNames(o), function(sl)
      .tb_chars(tryCatch(methods::slot(o, sl), error = function(e) NULL))),
      use.names = FALSE))
    any(gone %in% vals)
  }, logical(1))
  if (any(hits) && !quiet) {
    message("  dropped lever(s) naming a process that is gone: ",
            paste(names(lv)[hits], collapse = ", "))
  }
  lv[!hits]
}

# --- time ------------------------------------------------------------------ #

# A calendar by name, from TOPIA's own two or from the shipped catalogue.
#' @noRd
.tb_resolve_calendar <- function(x, ds) {
  if (methods::is(x, "calendar")) return(x)
  if (is.list(x)) {
    stop("build_topia(calendar = ) does not take a per-milestone list yet. ",
         "Build the model and use `setCalendar(mod, cal, years = )`, which ",
         "is what that form would call.", call. = FALSE)
  }
  x <- as.character(x)[1]
  cal <- ds$modules$calendars[[x]] %||% energyRt::calendars[[x]]
  if (is.null(cal)) {
    stop("build_topia(calendar = ): unknown calendar \"", x, "\". TOPIA ",
         "ships ", paste(names(ds$modules$calendars), collapse = ", "),
         "; the catalogue adds ",
         paste(setdiff(names(energyRt::calendars),
                       names(ds$modules$calendars)), collapse = ", "), ".",
         call. = FALSE)
  }
  cal
}

# The timeslices a calendar carries at its finest frame -- where the measured
# series and the demand curve are keyed.
#' @noRd
.tb_leaves <- function(cal) {
  tf <- cal@timeframes
  as.character(tf[[length(tf)]])
}

# Re-key the series and the demand curve onto a cheaper calendar.
#
# Only a SAMPLE of the model calendar is accepted -- one whose timeslices are
# a subset of its own. That is not a restriction invented here: the measured
# wind and solar grades exist on `d365_h24` alone, and a calendar with its own
# timeslice names (`s4_h24`, whose series are a simulated stand-in with no
# grades at all) would leave every clustered technology with no weather row
# and running unbounded. Refusing is the point; the silent version of this is
# a renewable fleet with no ceiling.
#
# Nothing is authored: the shipped objects are kept and their data slot is
# replaced with the rows that survive, so a weather series keeps its name, its
# units and its timeframe.
#' @noRd
.tb_retime <- function(ob, cal, model_cal, quiet) {
  if (identical(cal@name, model_cal@name)) return(ob)
  leaf <- .tb_leaves(cal)
  full <- .tb_leaves(model_cal)
  if (!all(leaf %in% full)) {
    stop("build_topia(calendar = \"", cal@name, "\"): its timeslices are ",
         "not a sample of \"", model_cal@name, "\"'s, so the shipped series ",
         "do not match it. ", sum(!leaf %in% full), " of ", length(leaf),
         " are unknown (e.g. ",
         paste(utils::head(setdiff(leaf, full), 3), collapse = ", "),
         "). Samples of the hourly year -- ",
         paste(grep("^d365_h24", names(energyRt::calendars), value = TRUE),
               collapse = ", "),
         " -- keep the parent's timeslice names and do match.",
         call. = FALSE)
  }
  # what a full year of the parent calendar carries, to hold the totals fixed
  wfull <- as.data.frame(model_cal@timeslice_share)
  wsamp <- as.data.frame(cal@timeslice_share)
  wf <- stats::setNames(wfull$weight, as.character(wfull$timeslice))
  ws <- stats::setNames(wsamp$weight, as.character(wsamp$timeslice))

  cut <- function(df, col) {
    if (!is.data.frame(df) || !nrow(df) || !"timeslice" %in% names(df))
      return(df)
    df[as.character(df$timeslice) %in% leaf, , drop = FALSE]
  }
  for (nm in names(ob)) {
    o <- ob[[nm]]
    if (methods::is(o, "weather")) {
      d <- cut(o@weather)
      if (!nrow(d)) {
        stop("build_topia(calendar = \"", cal@name, "\"): the series \"", nm,
             "\" has no row left on it.", call. = FALSE)
      }
      o@weather <- d
      ob[[nm]] <- o
    } else if (methods::is(o, "demand")) {
      d <- o@demand
      if (is.data.frame(d) && nrow(d) && "timeslice" %in% names(d)) {
        # The annual energy is held FIXED: a sample is a cheaper view of the
        # same year, not a smaller year, and cases are only comparable if the
        # demand they must meet is the same. Scaling per (region, year) keeps
        # the shape the sample does carry.
        key <- interaction(d$region, d$year, drop = TRUE)
        tot <- tapply(d$demand * wf[as.character(d$timeslice)], key, sum)
        dc <- cut(d)
        if (!nrow(dc)) {
          stop("build_topia(calendar = \"", cal@name, "\"): \"", nm,
               "\" has no row left on it.", call. = FALSE)
        }
        keyc <- interaction(dc$region, dc$year, drop = FALSE,
                            lex.order = FALSE)
        keyc <- factor(as.character(keyc), levels = levels(key))
        cur <- tapply(dc$demand * ws[as.character(dc$timeslice)], keyc, sum)
        f <- as.numeric(tot[as.character(keyc)]) /
             as.numeric(cur[as.character(keyc)])
        f[!is.finite(f)] <- 1
        dc$demand <- dc$demand * f
        o@demand <- dc
        ob[[nm]] <- o
      }
    }
  }
  if (!quiet) {
    message("  re-timed to ", cal@name, ": ", length(leaf), " of ",
            length(full), " timeslices, annual energy held fixed")
  }
  ob
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
#' Selecting processes pulls a **dependency closure** behind it: drop every
#' coal burner and `COA`, `SUP_COA`, `IMP_COA` and both coal corridors go too,
#' because nothing is left to use them. A storage and a corridor are not
#' counted as consumers or producers while deciding this -- one time-shifts
#' and the other moves, so counting them lets a commodity keep itself alive
#' after its last real user is gone. `CO2` is reachable only as an emission
#' declared on `COA` and `GAS`, and is kept on that basis. A lever naming a
#' process that is gone goes with it, since it would otherwise reference what
#' the model does not declare.
#'
#' A cheaper `calendar` re-keys the series and the demand curve onto it, and
#' the **annual energy is held fixed**: a sample is a cheaper view of the same
#' year, not a smaller year, and two cases are only comparable if the demand
#' they must meet is the same. Only a sample of the model calendar is
#' accepted. The measured wind and solar grades exist on `d365_h24` alone, so
#' a calendar with timeslice names of its own -- `s4_h24`, whose series are a
#' simulated stand-in with no grades at all -- would leave every clustered
#' technology with no weather row and running unbounded.
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
#' @param techs which technologies to keep, or to drop with a leading `-`
#'   (`c("-EBIO", "-H2ELC")`). Two keywords are derived from the objects
#'   rather than listed: `"vre"` is a technology declaring a weather series,
#'   `"thermal"` one that burns its input. Keeps and drops cannot be mixed.
#' @param modules which of the article's nine sub-repositories to keep, or to
#'   drop with a leading `-`: `commodities`, `supply`, `weather`,
#'   `generators`, `storage`, `hydrogen`, `transmission`, `fuel_trade`,
#'   `demand`. `modules = "-hydrogen"` strips in one word the chain the
#'   article declares across three sections.
#' @param calendar a calendar name or object. It must be a **sample** of the
#'   model's own -- its timeslices a subset of `d365_h24`'s -- which
#'   `d365_h24_1dpm` (301) and `d365_h24_1dps` (101) are. Anything else is
#'   refused, naming the slices that do not match.
#' @param horizon `"BY2025_50_by10"`, `"BY2025"`, or a `horizon` object.
#' @param clusters,demand,weather not built yet.
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
    given <- list(regions = regions, techs = techs)
    for (a in names(args)) {
      if (!is.null(given[[a]])) {
        stop("build_topia(): give `preset` or `", a, "`, not both -- preset ",
             "\"", preset, "\" already sets ", a, " = ",
             paste(args[[a]], collapse = ", "), ".", call. = FALSE)
      }
    }
    if (!is.null(args$regions)) regions <- args$regions
    if (!is.null(args$techs)) techs <- args$techs
    if (!is.null(args$calendar)) calendar <- args$calendar
    note <- paste0("preset \"", preset, "\" = ",
                   paste(vapply(names(args), function(a)
                     paste0(a, " ", paste(args[[a]], collapse = "/")),
                     character(1)), collapse = ", "),
                   if (!length(args)) "the shipped model" else "")
  }

  # -- not built yet, refused rather than ignored ---------------------------
  .tb_todo(!identical(level, "province"), "level",
           "coarsening to zone or nation aggregates the source tables")
  .tb_todo(!isTRUE(clusters), "clusters",
           "re-grading the VRE clusters merges weather series")
  .tb_todo(!identical(demand, 1) && !identical(demand, 1L), "demand",
           "rescaling demand re-normalises the year")
  .tb_todo(!is.null(weather), "weather", "swapping the series re-times them")

  # -- processes --------------------------------------------------------------
  ob <- getObject(kit$repo)
  all_n <- names(ob)
  keep_n <- all_n
  gone <- character(0)

  if (!is.null(techs) || !is.null(modules)) {
    if (is.null(kit$groups)) {
      stop("this TOPIA dataset predates `modules`/`techs`: it carries no ",
           "`groups`. Rebuild it with ",
           "`source(\"data-raw/topia_assemble.R\")`.", call. = FALSE)
    }
    # modules first, coarsely: the nine sub-repositories the article builds
    # the model from, carried over by `data-raw/topia_model.R` rather than
    # re-derived here.
    if (!is.null(modules)) {
      g <- .tb_select(modules, names(kit$groups), "modules")
      keep_n <- intersect(keep_n, unlist(kit$groups[g], use.names = FALSE))
    }
    # then technologies, finely, within what survived
    if (!is.null(techs)) {
      tech_n <- names(Filter(function(o) methods::is(o, "technology"), ob))
      sel <- .tb_select(techs, tech_n, "techs",
                        expand = function(b)
                          if (b %in% tech_n) b else .tb_keyword(ob, b))
      keep_n <- setdiff(keep_n, setdiff(tech_n, sel))
    }
    keep_n <- .tb_closure(ob, keep_n)
    gone <- setdiff(all_n, keep_n)
    if (!length(keep_n)) {
      stop("build_topia(): that selection leaves nothing.", call. = FALSE)
    }
  }

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

  cal <- .tb_resolve_calendar(calendar, ds)
  model_cal <- ds$modules$calendars[[kit$calendar]]
  hor <- if (methods::is(horizon, "horizon")) horizon else {
    h <- ds$modules$horizons[[as.character(horizon)[1]]]
    if (is.null(h)) {
      stop("build_topia(horizon = ): unknown horizon \"", horizon,
           "\". TOPIA ships: ",
           paste(names(ds$modules$horizons), collapse = ", "), ".",
           call. = FALSE)
    }
    h
  }

  retimed <- !identical(cal@name, model_cal@name)
  if (retimed) {
    ob <- .tb_retime(ob, cal, model_cal, quiet)
    if (!length(gone)) gone <- character(0)
  }

  base_repo <- if (!length(gone) && !retimed) kit$repo else
    do.call(newRepository, c(list(name = "topia"), unname(ob[keep_n])))

  whole <- length(keep) == length(all_r)
  if (whole && identical(gs, kit$geoscale) && !length(gone) && !retimed) {
    # Nothing to cut: the shipped objects are returned as they are, which is
    # what makes the default an identity rather than a rebuild.
    repo <- kit$repo
    geo <- kit$geoscale
    mod <- NULL
  } else {
    mod <- newModel(name = "TOPIA", currency = "MUSD", region = all_r,
                    calendar = cal, horizon = hor, discount = 0.05,
                    data = base_repo)
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
  lv <- .tb_drop_levers(lv, gone, quiet)

  grp <- kit$groups
  if (!is.null(grp) && length(gone)) {
    grp <- lapply(grp, function(g) g[g %in% keep_n])
    grp <- grp[lengths(grp) > 0L]
  }
  out <- c(list(regions = keep),
           if (is.null(grp)) NULL else list(groups = grp),
           list(calendar = cal@name, repo = repo, geoscale = geo),
           lv)

  if (!quiet) {
    if (length(gone)) {
      note <- paste0(note, if (nzchar(note)) "
  " else "",
                     "dropped ", length(gone), " object(s) by selection: ",
                     paste(utils::head(gone, 8), collapse = ", "),
                     if (length(gone) > 8) ", ..." else "")
    }
    .tb_report(out, cal, note)
  }

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
