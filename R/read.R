#' Read solution
#'
#' The function and method read outputs of solved model/scenario and return the scenario object populated with variables data.
#'
#' @param obj scenario object
#' @param run character, optional run to read: `"<solve>"` for a base-problem
#'   run or `"<variant>/<solve>"` — see [scenario_runs()]. The scenario's
#'   active run switches to it. Default `NULL` reads the active run (or, for
#'   a freshly loaded scenario, the manifest's `default:` run).
#' @param ... optional `solver.dir` (an external solver directory, replacing
#'   the run resolution; `tmp.dir` is the deprecated alias)
#'
#' @return
#' The function returns the scenario object with populated modOut slot
#' from the solved model directory.
#' @export
#' @seealso [solve()] to run the script, solve the scenario. [write_sc()] to write model inputs.
#'
#' @rdname read
#' @examples
#' \dontrun{
#' scen <- read(scen)
#' }
read_solution <- function(obj, run = NULL, ...) {
  scen <- obj
  ## arguments
  # scen
  # readOutputFunction = read.csv (may use data.table::fread)
  # solver.dir - dir to read results from; default: the active run's solver/
  # echo = TRUE - print working data
  arg <- .solver_dir_aliases(list(...))
  read_result_time <- proc.time()[3]
  if (is.null(arg$echo)) arg$echo <- TRUE
  # if (is.null(arg$readOutputFunction)) arg$readOutputFunction <- read.csv
  if (is.null(arg$readOutputFunction)) {
    arg$readOutputFunction <- data.table::fread
  }
  if (!is.null(run)) {
    # switch the active run: read this run's solver/output and remember it
    id <- .parse_run_id(run, scen)
    run_solver <- .run_solver_dir(.run_dir(scen, id$variant, id$solve))
    if (!dir.exists(run_solver)) {
      stop("Run '", .run_id(id$variant, id$solve), "' has no solver ",
           "directory under '", scen@path, "'.\n  Available runs:\n",
           .run_list_hint(scen))
    }
    # crossing a problem boundary (base <-> variant, or between variants)
    # swaps the in-memory problem — settings + modInp — from the target's
    # saved swap data before the solution is read. A variant DIR without a
    # variant.yml is not an own problem — it is a legacy/interim wrapper
    # (e.g. the retired runs/default/) around the SAME problem: no swap.
    if (!identical(id$variant, .run_variant(scen))) {
      own_problem <- !nzchar(id$variant) ||
        file.exists(fp(scen@path, "runs", id$variant, "variant.yml"))
      if (own_problem) {
        scen <- .variant_swap(scen, id$variant)
      }
    }
    arg$solver.dir <- run_solver
    scen@misc$variant <- if (nzchar(id$variant)) id$variant else NULL
    scen@misc$run <- id$solve
    scen@misc$tmp.dir <- NULL
    scen@misc$solver.dir <- NULL
  }
  if (is.null(arg$solver.dir)) {
    if (nzchar(scen@misc$run %||% "")) {
      # recorded run: the directory is derived, never stored
      arg$solver.dir <- .run_solver_dir(
        .run_dir(scen, .run_variant(scen), scen@misc$run))
    } else if (!is.null(scen@misc$solver.dir %||% scen@misc$tmp.dir)) {
      # external solve, or an object saved by an older version
      arg$solver.dir <- scen@misc$solver.dir %||% scen@misc$tmp.dir
    } else {
      # fall back to the manifest's default run
      id <- .scenario_default_run(scen)
      if (!is.null(id)) {
        arg$solver.dir <- .run_solver_dir(
          .run_dir(scen, id$variant, id$solve))
        scen@misc$variant <- id$variant
        scen@misc$run <- id$solve
      }
    }
    if (is.null(arg$solver.dir)) {
      stop("No solver directory to read from: the scenario has no active or ",
           "default run. Pass run= (see scenario_runs()) or solver.dir=.")
    }
  }
  # Read basic variable list (vrb_list) and additional if user need (vrb_list2)
  var_file <- paste(arg$solver.dir, "/output/variable_list.csv", sep = "")
  vrb_list <- try({
    arg$readOutputFunction(
      var_file,
      stringsAsFactors = FALSE
      )$value
  })
  if (inherits(vrb_list, "try-error")) {
    msg <- paste0("Solution files not found\n", var_file)
    if (!is.null(arg$stop_on_error) && arg$stop_on_error) {
      stop(msg)
    } else {
      message(msg)
      return(invisible(obj))
    }
  }
  if (file.exists(paste(arg$solver.dir, "/output/variable_list2.csv", sep = ""))) {
    vrb_list2 <- arg$readOutputFunction(
      paste(arg$solver.dir, "/output/variable_list2.csv", sep = ""),
      stringsAsFactors = FALSE
    )$value
  } else {
    vrb_list2 <- character()
  }
  rr <- list(
    variables = list(),
    set = arg$readOutputFunction(
      paste(arg$solver.dir, "/output/raw_data_set.csv", sep = ""),
      stringsAsFactors = FALSE
    )
  )
  # Read set and alias
  ss <- list()
  for (k in unique(rr$set$set)) {
    ss[[k]] <- rr$set$value[rr$set$set == k]
  }
  # add alias
  ss$src <- ss$region
  ss$dst <- ss$region
  ss$regionp <- ss$region
  ss$yearp <- ss$year
  ss$acomm <- ss$comm
  ss$commp <- ss$comm
  ss$timeslicep <- ss$timeslice
  rr$set_vec <- ss
  # Every writer records the format it produced (the JuMP/Pyomo pair through
  # .resolve_exchange_formats(), GAMS in .write_model_GAMS). Only backends that
  # never set one -- GLPK -- land here, and those write CSV.
  if (is.null(scen@settings@solver$import_format) ||
      !nzchar(scen@settings@solver$import_format)) {
    scen@settings@solver$import_format <- "csv"
  }
  if (grepl("^gdx$", scen@settings@solver$import_format, ignore.case = TRUE)) {
    # .check_load_gdxlib()
    .check_load_gdxtools()
    # Read variables gdx
    gd <- gdxtools::gdx(paste(arg$solver.dir, "/output/output.gdx", sep = ""))
    for (i in c(vrb_list, vrb_list2)) {
      # cat(i, "\n")
      # if (i == "vOutTot") browser() # debug
      vr <- try(gd[i])
      if (inherits(vr, "try-error")) {
        message(paste0("Error reading ", i, " from output.gdx"))
        next
      }
      if (is.null(vr)) next
      # The two-`region` -> `src`/`dst` rename used to be special-cased here and
      # nowhere else, so the GDX and CSV branches disagreed on the column names
      # of every duplicate-dimension variable. `d2v()` now renames positionally
      # onto the variable's declared `@colNames`, for both branches alike.
      if (ncol(vr) == 1) {
        rr$variables[[i]] <- data.frame(value = vr[1, 1])
      } else {
        for (j in seq_len(ncol(vr))[colnames(vr) != "value"]) {
          # Remove [.][:digit:] if any
          if (all(colnames(vr)[j] != names(rr$set_vec))) {
            colnames(vr)[j] <- gsub("[.].*", "", colnames(vr)[j])
          }
          sname <- colnames(vr)[j] # set name
          # Save as.factor with existing levels
          if (sname != "year") {
            if (!is.null(scen@modInp@parameters[[sname]])) {
              # !!! move to the top before aliases
              set_levels <- scen@modInp@parameters[[sname]]@data[[sname]]
            } else {
              set_levels <- sort(rr$set_vec[[sname]])
            }
            # set_levels <- scen@modInp@parameters[[sname]]@data[[sname]]
            # vr[[j]] <- factor(vr[[j]], levels = sort(rr$set_vec[[sname]]))
            vr[[j]] <- factor(vr[[j]], levels = set_levels)
          } else {
            vr[[j]] <- as.integer(vr[[j]])
          }
        }
        rr$variables[[i]] <- vr
      }
    }
  } else {
    # Read variables from CSV or Arrow (feather/parquet) per import_format. The
    # solver writes one file per variable to `output/`; the per-variable post-
    # processing (column de-suffixing + factor levels) is shared across formats.
    .imf <- tolower(scen@settings@solver$import_format)
    .arrow_imp <- .imf %in% c("feather", "ipc", "arrow", "parquet")
    .ext <- if (.imf == "parquet") ".parquet" else if (.arrow_imp) ".arrow" else ".csv"
    for (i in c(vrb_list, vrb_list2)) {
      vfile <- paste(arg$solver.dir, "/output/", i, .ext, sep = "")
      if (.arrow_imp) {
        if (!file.exists(vfile)) next # variable with no non-zero values
        vr <- .read_exchange_table(vfile)
      } else {
        vr <- arg$readOutputFunction(vfile, stringsAsFactors = FALSE)
      }
      if (ncol(vr) == 1) {
        rr$variables[[i]] <- data.frame(value = vr[1, 1])
      } else {
        for (j in seq_len(ncol(vr))[colnames(vr) != "value"]) {
          # Remove [.][:digit:] if any
          if (all(colnames(vr)[j] != names(rr$set_vec))) {
            colnames(vr)[j] <- gsub("[.].*", "", colnames(vr)[j])
          }
          # Save all data with all levels
          if (colnames(vr)[j] != "year") {
            vr[[j]] <- factor(vr[[j]],
              levels = sort(rr$set_vec[[colnames(vr)[j]]])
            )
          }
        }
        rr$variables[[i]] <- vr
      }
    }
  }
  scen@modOut <- new("modOut")
  # Read solution status
  scen@modOut@solutionLogs <- read.csv(paste(arg$solver.dir, "/output/log.csv",
    sep = ""
  ))
  solver_data <- read.csv(paste(arg$solver.dir, "/solver", sep = ""),
    stringsAsFactors = FALSE
  )
  codes <- solver_data[grep("^code", solver_data$name), ]
  # Only the FILE NAMES are kept: the model text itself already lives in the
  # run's solver directory, and importing it here used to bloat
  # every solved scenario by the full model source (again, on top of
  # settings@sourceCode). Nothing in the package consumed `solver$code1..n`.
  scen@settings@solver$code_files <- codes$value
  if (all(scen@modOut@solutionLogs$parameter != "solution status")) {
    scen@modOut@stage <- "Scenario is not solved"
  } else if (all(scen@modOut@solutionLogs[
    scen@modOut@solutionLogs$parameter == "solution status", "value"
  ] != 1)) {
    scen@modOut@stage <- paste0(
      "The solution status is not optimal (",
      scen@modOut@solutionLogs[
        scen@modOut@solutionLogs$parameter == "solution status", "value"
      ], ")"
    )
  } else if (all(scen@modOut@solutionLogs$parameter != "done")) {
    scen@modOut@stage <- "Unexpected termination"
  } else {
    scen@modOut@stage <- "solved"
  }

  if (scen@modOut@stage != "solved") {
    warning(scen@modOut@stage,
            ". Any exported (incumbent) solution is still read; getData() ",
            "returns it with a warning.")
  }

  scen@modOut@sets <- rr$set_vec
  # `@variables` arrives pre-populated with one empty, typed `variable` per
  # declared variable (see `modOut`'s initialize); fill in what the solver
  # returned. Variables it skipped -- those with no non-zero values -- keep
  # their empty skeleton, so their column names are still known.
  for (i in names(rr$variables)) {
    v <- scen@modOut@variables[[i]]
    if (is.null(v)) {
      # `variable_list2.csv` can name variables the specification does not know.
      # Accept them rather than dropping the solve, but say so.
      warning("Solver returned an undeclared variable '", i,
              "'; add it to data-raw/variables.yml. Stored untyped.")
      v <- newVariable(
        i, dimSets = intersect(setdiff(colnames(rr$variables[[i]]), "value"),
                               .dimSets),
        origin = "solver", declared = FALSE)
    }
    scen@modOut@variables[[i]] <- d2v(v, rr$variables[[i]])
  }
  ## Salvage cost calculation
  salvage_cost0 <- function(scen, par) {
    invcost <- .add_dropped_zeros(scen@modInp, paste0("p", par, "Invcost"))
    olife <- .add_dropped_zeros(scen@modInp, paste0("p", par, "Olife"))
    # Salvage value of capital not yet recovered -- a financing calculation, so
    # it discounts at the cost of capital, not at the social rate.
    discount <- .add_dropped_zeros(scen@modInp, "pWacc")
    newcap <- get_variable(scen, paste0("v", par, "NewCap"))
    invcost$invcost <- invcost$value
    invcost$value <- NULL
    olife$olife <- olife$value
    olife$value <- NULL
    discount$discount <- discount$value
    discount$value <- NULL
    newcap$newcap <- newcap$value
    newcap$value <- NULL

    salvage <-
      merge0(
        merge0(
          newcap,
          merge0(olife, invcost)
        ),
        discount,
        all.x = TRUE
      )
    end_year <- max(.get_data_slot(scen@modInp@parameters$mEndMilestone)$yearp)
    salvage <- merge0(
      salvage,
      .get_data_slot(scen@modInp@parameters$mStartMilestone)
    )
    salvage$start <- salvage$yearp
    salvage$yearp <- NULL
    # if (F) {
    # !!! unfinished
    salvage <- salvage[salvage$start + salvage$olife > end_year, ]

    salvage$value <- salvage$newcap * salvage$invcost *
      ((1 + salvage$discount)^(salvage$olife) -
        (1 + salvage$discount)^(end_year - salvage$start + 1)) /
      ((1 + salvage$discount)^salvage$olife - 1)

    fl <- (salvage$discount == 0)
    salvage$value[fl] <- (salvage$newcap * salvage$invcost *
      (salvage$olife - (end_year - salvage$start + 1)) /
      salvage$olife)[fl]

    salvage[, c(3, 2, 1, 9)]
    # }
    # NULL # temporary
  }
  if (scen@modOut@stage == "solved") {
    # Postprocessing
    # scen@modOut@variables$vTechSalv <- salvage_cost0(scen, "Tech")
    # scen@modOut@variables$vStorageSalv <- salvage_cost0(scen, "Storage")
    # scen@modOut@variables$vTradeSalv <- salvage_cost0(scen, "Trade")
    # `vDummyImportCost` / `vDummyExportCost` are SOLVER variables: the model
    # declares them and `eqDummyImportCost` computes them weighted over timeslices
    # (glpk/energyRt.mod), and `read_solution` has already stored those values.
    # An R recomputation used to overwrite them here with an unweighted,
    # timeslice-resolved version whose shape did not even match the declaration --
    # removed. The solver's own value is correct and is what the objective used.
    tmp <- .get_data_slot(scen@modInp@parameters$pEmissionFactor)
    tmp$comm2 <- tmp$commp
    tmp$commp <- tmp$comm
    tmp$comm <- tmp$comm2
    tmp$comm2 <- NULL
    pTechEmisComm <- .get_data_slot(scen@modInp@parameters$pTechEmisComm)
    vTechEmsFuel <-
      merge0(
        merge0(pTechEmisComm, get_variable(scen, "vTechInp"),
          by = c("tech", "comm")
        ),
        tmp,
        by = "comm"
      )
    vTechEmsFuel$comm <- vTechEmsFuel$commp
    if (nrow(vTechEmsFuel) > 0) {
      vTechEmsFuel <- aggregate(
        vTechEmsFuel$value.x * vTechEmsFuel$value.y * vTechEmsFuel$value,
        vTechEmsFuel[, c("tech", "comm", "region", "year", "timeslice")], sum
      )
      vTechEmsFuel$value <- vTechEmsFuel$x
      vTechEmsFuel$x <- NULL
      scen@modOut@variables$vTechEmsFuel <-
        d2v(scen@modOut@variables$vTechEmsFuel, vTechEmsFuel)
    }
    # else: the empty branch used to build a 4-column frame (no `timeslice`) against
    # this variable's 5-column declaration. The pre-populated typed skeleton is
    # already empty and correctly shaped, so there is nothing to do.

    # Estimate Costs
    if (length(getNames(scen, "costs")) != 0) {
      cst <- getObjects(scen, "costs")
      costs_tot <- data.frame(
        costs = character(), region = character(), year = integer(),
        value = numeric(), stringsAsFactors = FALSE
      )
      for (tmp in cst) {
        in_dat <- get_variable(scen, tmp@variable)
        if (anyDuplicated(.variable_set[[tmp@variable]])) {
          # The `mCosts<name>` map this is merged against was built with the
          # "2"-suffix convention, so keep using it here rather than the
          # variable's own `@colNames` (src/dst), or the merge becomes a cross
          # join. `.dedup2()` is the shared spelling of that convention.
          colnames(in_dat) <- c(.dedup2(.variable_set[[tmp@variable]]), "value")
        }
        if (nrow(in_dat) != 0 &&
          !is.null(scen@modInp@parameters[[paste0("mCosts", tmp@name)]])) {
          in_dat <- merge0(
            in_dat,
            .get_data_slot(
              scen@modInp@parameters[[paste0("mCosts", tmp@name)]]
            )
          )
        }
        if (nrow(in_dat) != 0 &&
          !is.null(scen@modInp@parameters[[paste0("pCosts", tmp@name)]])) {
          prm <- .get_data_slot(
            scen@modInp@parameters[[paste0("pCosts", tmp@name)]]
          )
          if (ncol(prm) == 1) {
            in_dat$value <- in_dat$value * prm$value
          } else {
            colnames(prm)[ncol(prm)] <- "par"
            in_dat <- merge0(in_dat, prm)
            in_dat$value <- in_dat$value * in_dat$par
            in_dat$par <- NULL
          }
        } else if (nrow(in_dat) != 0 && length(tmp@defVal) == 1 &&
                   is.finite(tmp@defVal)) {
          # scalar multiplier: no pCosts* parameter is written, the constant
          # lives in the cost object's @defVal (same as the model equation)
          in_dat$value <- in_dat$value * tmp@defVal
        }
        if (nrow(in_dat) != 0) {
          in_dat <- aggregate(
            in_dat[, "value", drop = FALSE],
            in_dat[, c("region", "year"), drop = FALSE], sum
          )
          in_dat$costs <- tmp@name
          costs_tot <- rbind(
            costs_tot,
            in_dat[, c("costs", "region", "year", "value"),
              drop = FALSE
            ]
          )
        }
      }
      scen@modOut@variables$vUserCosts <-
        d2v(scen@modOut@variables$vUserCosts, costs_tot)
    }
  }
  if (arg$echo) {
    cat("Reading solution: ", round(proc.time()[3] -
      read_result_time, 2), "s\n", sep = "")
  }
  # `solved` had no writer at all (initialised FALSE at interpolation and never
  # updated), so it read FALSE even after an optimal solve. It now records that
  # the solve produced a readable optimal solution, alongside `optimal`.
  if (scen@modOut@stage == "solved") {
    scen@status$optimal <- TRUE
    scen@status$solved <- TRUE
  }
  invisible(scen)
}
#' @rdname read
#' @method read scenario
#' @export
setMethod("read", "scenario", read_solution)

# read_solution <- read.scenario
# read.scenario <- function(scen, ...) read_solution(scen, ...)
# .S3method("read", "scenario", read_solution)

# `.paste_base_result2new()` (myopic / rolling-horizon result stitching) moved to
# `drafts/paste_base_result2new.R`: it reads `scen@misc$data.before`, which is
# assigned nowhere in the package, so its call site never fired. See the file for
# what reviving it would take.

#
