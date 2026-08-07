#' Write scenario object as a Python, Julia, GAMS, or MathProg script with data files to a directory
#'
#' @param scen scenario object, must be interpolated
#' @param tmp.dir character, path
#' @param solver list of character with solver specification.
#' @param ... additional solver parameters
#' @family write scenario
#' @rdname write
#' @seealso [solve()] to run the script, solve the scenario. [read_solution] to read model solution.
#'
#' @export
write_script <- function(scen, tmp.dir = NULL, solver = NULL, ...) {
  # scen <- obj
  # if (is.null(tmp.dir)) {
  #   # get_tmp_dir(scen, list(tmp.dir = tmp.dir, solver = solver, ...))
  #   browser()
  #   if (!is.null(scen@misc$tmp.dir)) {
  #     tmp.dir <- scen@misc$tmp.dir
  #   } else if (!is.null(scen@path)) {
  #     tmp.dir <- file.path(scen@path, "script", solver$lang, solver$solver)
  #   }
  #   if (is.null(tmp.dir))
  #     stop('Either "tmp.dir" or "scenario@path" must be set')
  # }
  # scen@misc$tmp.dir <- tmp.dir
  if (!isTRUE(scen@status$interpolated)) {
    stop("Scenario must be interpolated before writing the script.")
  }
  # browser()
  # if (is.null(solver)) solver <- scen@settings@solver
  # if (is.null(solver)) solver <- get_default_solver()
  # if (is.null(solver)) stop("Solver must be specified.")
  # arg <- list()
  .executeScenario(scen,
                   tmp.dir = tmp.dir, solver = solver,
                   run = FALSE, write = TRUE, ...)
}

#' @export
#' @rdname write
write_sc <- function(x, tmp.dir = NULL, solver = NULL, ...) {
  write_script(x, tmp.dir, solver, ...)
}

#' @export
#' @rdname write
write.sc <- write_sc

# @method write scenario
# @family write scenario
# @rdname write
# @name write
#
# @export
# setMethod("write", signature(x = "scenario"), definition = write_script)

# @family write scenario
# @rdname write
# @export
# setMethod("write", signature(x = "missing"), definition = function(...) {
#   arg <- list(...)
#   if (is.null(arg$obj)) {
#     if (is.null(arg$x) | class(arg$x)[1] != "scenario") NextMethod(...)
#   } else if (!is.null(arg$x)) {
#     if (class(arg$x)[1] != "scenario") NextMethod(...)
#   } else if (class(arg$obj)[1] != "scenario") {
#     NextMethod(...)
#   }
#   return(do.call(write_script, arg))
# })

# @family write scenario
# @rdname write
# @export
# setMethod("write", signature(x = "ANY"), definition = function(...) {
#   browser()
#   arg <- list(...)
#   arg <- c(x = x, arg)
#   if (class(x)[1] == "scenario") return(do.call(write_script, arg))
#   return(do.call(base::write, arg))
# })

# write <- function(scen, ...) UseMethod("write")

# write.model <- function(scen, ...) {
#   message('Error:
#   No applicable method for "write" applied to an object of class "model".
#   Use "interpolate(model, ...)" instead to get "scenario" object,
#   and then "write(scenario, ...)"')
#   # stop()
# }
#
#
# write_model <- function(scen, tmp.dir = NULL, solver = NULL, ...) {
#   message("The function is depreciated, use `write` instead")
#   write_script(scen, tmp.dir, solver, ...)
# }


#### Internal functions ####
# .write_multi_threads <- function(arg, scen, func, type) {
#   # require(parallel)
#   tlp <- lapply(0:(arg$n.threads - 1), function(y) {
#     names(scen@modInp@parameters)[
#       seq_along(scen@modInp@parameters) %% arg$n.threads == y
#     ]
#   })
#   cl <- makeCluster(arg$n.threads)
#   wrt_fun <- function(x, tlp, par, drr, func, type) {
#     require(energyRt)
#     for (i in tlp[[x + 1]]) {
#       zz_data_tmp <- file(paste(drr, "/input/", i, ".", type, sep = ""), "w")
#       cat(func(par[[i]]), sep = "\n", file = zz_data_tmp)
#       close(zz_data_tmp)
#     }
#     NULL
#   }
#   parLapply(cl, 0:(arg$n.threads - 1), wrt_fun, tlp, scen@modInp@parameters, arg$tmp.dir, func, type)
#   stopCluster(cl)
#   NULL
# }

.write_inc_solver <- function(scen, arg, def_inc_solver, type, templ) {
  if (!is.null(scen@settings@solver$inc_solver) && !is.null(scen@settings@solver$solver)) {
    # browser()
    stop("have to define only one argument from scen@settings@solver$inc_solver & scen@settings@solver$solver")
  }
  if (!is.null(scen@settings@solver$solver)) {
    scen@settings@solver$inc_solver <- gsub(templ, scen@settings@solver$solver, def_inc_solver)
  }
  if (is.null(scen@settings@solver$inc_solver) && is.null(scen@settings@solver$solver)) {
    scen@settings@solver$inc_solver <- def_inc_solver
  }
  fn <- file(file.path(arg$tmp.dir, paste0("inc_solver", type)), "w")
  cat(scen@settings@solver$inc_solver, file = fn, sep = "\n")
  close(fn)
}

# writes "include" files to tmp.dir ("inc1" ... "inc5" - see GAMS code)
.write_inc_files <- function(arg, scen, type) { # renamed .add_five_includes
  if (is.null(type)) {
    fl <- c(names(scen@settings@solver$files))
    for (i in 1:5) {
      if (is.null(scen@settings@solver[[paste0("inc", i)]])) {
        fl <- c(fl, paste0("inc", i))
      }
    }
    if (is.null(fl)) {
      tmp <- paste0(
        "There is (are) ", length(fl),
        " files, that not suitable for lang ", scen@settings@solver@lang,
        '. File(s) list: "', paste0(fl, collapse = '", "'), '"'
      )
      stop(tmp)
    }
  }
  for (i in 1:5) {
    fn <- file(file.path(arg$tmp.dir, paste0("inc", i, type)), "w")
    cat(scen@settings@solver[[paste0("inc", i)]], sep = "\n", file = fn)
    close(fn)
  }
  for (i in names(scen@settings@solver$files)) {
    fn <- file(file.path(arg$tmp.dir, i), "w")
    cat(scen@settings@solver$files[[i]], sep = "\n", file = fn)
    close(fn)
  }
}

#' Title
#'
#' @param prec modInp object
#' @param interpolation_start_time numeric
#' @param interpolation_count numeric
#' @param len_name numeric
#'
#' @noRd
.make_mapping <- function(
    prec,
    interpolation_start_time,
    interpolation_count,
    len_name,
    scen_settings) {
  # prec <- scen@modInp
  reduce.duplicate <- function(x) x[!duplicated(x), , drop = FALSE]
  bacs <- paste0(rep("\b", len_name), collapse = "")
  rest <- interpolation_count - 45
  cat(paste0(rep(" ", len_name), collapse = ""))

  # Clean previous set data if any
  reduce.sect <- function(x, set) {
    # browser()
    # x <- x[, set, drop = FALSE]
    if (!missing(set)) {
      x <- select(x, all_of(set)) |> relocate(all_of(set))
    }
    x[!duplicated(x), , drop = FALSE]
  }
  reduce.sect.merge.unique <- function(tx, set) {
    gg <- NULL
    for (x in tx) {
      gg <- rbind(gg, reduce.sect(x, set))
    }
    unique(gg)
  }

  .interpolation_message("region", rest, interpolation_count,
                         interpolation_start_time, len_name)
  rest <- rest + 1
  region <- .get_data_slot(prec@parameters$region)

  # mMidMilestone ####
  .interpolation_message("mMidMilestone", rest, interpolation_count,
                         interpolation_start_time, len_name)

  # mSliceParentChildE ####
  rest <- rest + 1
  year <- .get_data_slot(prec@parameters$mMidMilestone)
  .interpolation_message("mSliceParentChildE", rest, interpolation_count,
                         interpolation_start_time, len_name)
  rest <- rest + 1
  # Total parameter reductions
  # mCommSlice ####
  mSliceParentChildE <- .get_data_slot(prec@parameters$mSliceParentChildE)
  .interpolation_message("mCommSlice", rest, interpolation_count,
                         interpolation_start_time, len_name)
  rest <- rest + 1
  mCommSlice <- .get_data_slot(prec@parameters$mCommSlice)
  uu <- mSliceParentChildE
  colnames(uu) <- c("slicep", "slice")
  map_for_comm <- merge0(mCommSlice, uu)[, c("comm", "slicep")]
  colnames(map_for_comm) <- c("comm", "slice")
  map_for_comm <- map_for_comm[!duplicated(map_for_comm), ]

  # # mCommReg ####
  # rest <- rest + 1
  # .interpolation_message("mCommReg", rest, interpolation_count,
  #                        interpolation_start_time, len_name)
  # browser()
  # # scan all "^p"-parameters for (comm, region)
  # allpar <- names(prec@parameters); allpar <- allpar[grepl("^p", allpar)]
  # comreg <- lapply(prec@parameters[allpar], function(x) {
  #   if (!all(c("comm", "region") %in% x@dimSets)) return(NULL)
  #   select(x@data, comm, region) |> unique()
  # }) |>
  #   rbindlist() |>
  #   unique()
  # prec@parameters[["mCommReg"]] <-
  #   .dat2par(prec@parameters[["mCommReg"]], comreg)

  .interpolation_message("mCommSliceOrParent", rest, interpolation_count,
                         interpolation_start_time, len_name)
  rest <- rest + 1
  # mCommSliceOrParent ####
  # browser()
  l1 <- merge0(.get_data_slot(prec@parameters$comm),
               .get_data_slot(prec@parameters$slice))
  # l2 <- merge0(mCommSlice, mSliceParentChildE)[, c("comm", "slice", "slicep")]
  l2 <- merge0(mCommSlice, mSliceParentChildE) |>
    select(all_of(c("comm", "slice", "slicep")))
  # l3 <- l2[!duplicated(l2[, c("comm", "slicep")]), c("comm", "slicep")]
  l3 <- l2 |> select(all_of(c("comm", "slicep"))) |> unique() |>
    rename(slice = slicep)
  # colnames(l3)[2] <- "slice"
  l3 <- rbind(l1, l3)
  l3 <- l3[!duplicated(l3) & !duplicated(l3, fromLast = TRUE), ]
  l3$slicep <- l3$slice
  mCommSliceOrParent <- rbind(l2, l3)
  prec@parameters[["mCommSliceOrParent"]] <-
    .dat2par(prec@parameters[["mCommSliceOrParent"]], mCommSliceOrParent)
  # browser()
  # !!! Attempt to separate commodities with multi and one time frame !!!
  # prec@parameters[["mCommSliceOrParent1"]] <-
  #   .dat2par(prec@parameters[["mCommSliceOrParent1"]],
  #            filter(mCommSliceOrParent, slice != slicep)
  #            )
  # cm <- unique(mCommSliceOrParent$comm)
  # cm1 <- unique(prec@parameters[["mCommSliceOrParent1"]]@data$comm)
  # ii <- cm %in% cm1
  # mCommSliceOrParent0 <- cm[!ii]
  # prec@parameters[["mCommSliceOrParent0"]] <-
  #   .dat2par(prec@parameters[["mCommSliceOrParent0"]],
  #            data.table(comm = mCommSliceOrParent0))
  #  Example of use in GAMS equations:
  # eqEmsFuelTot(comm, region, year, slice)$mEmsFuelTot(comm, region, year, slice)..
  # vEmsFuelTot(comm, region, year, slice)
  # =e=
  #   sum(commp$(pEmissionFactor(comm, commp) > 0),
  #       pEmissionFactor(comm, commp)
  #       * sum(tech$mTechInpComm(tech, commp),
  #             pTechEmisComm(tech, commp)
  #             * (sum(slicep$mCommSliceOrParent1(comm, slice, slicep),
  #                    vTechInp(tech, commp, region, year, slicep)$mTechEmsFuel(tech, comm, commp, region, year, slicep))
  #                +
  #                  vTechInp(tech, commp, region, year, slice)$(
  #                    mTechEmsFuel(tech, comm, commp, region, year, slice)
  #                    and
  #                    mCommSliceOrParent0(commp)
  #                  )
  #             )
  #       )
  #   ) * pSliceWeight(slice);

  # browser()
  # mvTechOutS ####
  # finish: mTechCommSliceSliceP
  # mvTechOutS <- prec@parameters[["mvTechOut"]]@data |>
  #   rename(slicep = slice) |>
  #   left_join(prec@parameters[["mCommSliceOrParent"]]@data,
  #             by = c("comm", "slicep")) |>
  #   select(all_of(prec@parameters[["mvTechOutS"]]@dimSets)) |>
  #   unique()
  # prec@parameters[["mvTechOutS"]] <-
  #   .dat2par(prec@parameters[["mvTechOutS"]], mvTechOutS)
  #
  # mvTechAOutS ####
  # mvTechAOutS <- prec@parameters[["mvTechAOut"]]@data |>
  #   rename(slicep = slice) |>
  #   left_join(prec@parameters[["mCommSliceOrParent"]]@data,
  #             by = c("comm", "slicep")) |>
  #   select(all_of(prec@parameters[["mvTechAOutS"]]@dimSets)) |>
  #   unique()
  # prec@parameters[["mvTechAOutS"]] <-
  #   .dat2par(prec@parameters[["mvTechAOutS"]], mvTechAOutS)
  #
  # # mTechCommSliceSliceP ####
  # ! mTechCommSliceSliceP, mTechCommOutSliceSliceP, mTechCommAOutSliceSliceP
  # ! have been dropped due to large size in models with many commodities
  # new map for eqTechOutTot
  # mTechCommSliceSliceP <- prec@parameters[["mTechSlice"]]@data |>
  #   rename(slicep = slice) |>
  #   left_join(mCommSliceOrParent, by = c("slicep")) |>
  #   select(all_of(prec@parameters[["mTechCommSliceSliceP"]]@dimSets)) |>
  #   unique()
  # prec@parameters[["mTechCommSliceSliceP"]] <-
  #   .dat2par(prec@parameters[["mTechCommSliceSliceP"]], mTechCommSliceSliceP)
  # # browser()
  #
  # mTechCommOutSliceSliceP <- prec@parameters[["mvTechOut"]]@data |>
  #   # bind_rows(prec@parameters[["mvTechAOut"]]@data) |>
  #   rename(slicep = slice) |>
  #   select(-region, -year) |> unique() |>
  #   left_join(mCommSliceOrParent, by = c("comm", "slicep")) |>
  #   select(all_of(prec@parameters[["mTechCommOutSliceSliceP"]]@dimSets)) |>
  #   unique()
  # prec@parameters[["mTechCommOutSliceSliceP"]] <-
  #   .dat2par(prec@parameters[["mTechCommOutSliceSliceP"]],
  #            mTechCommOutSliceSliceP)
  #
  # mTechCommAOutSliceSliceP <- prec@parameters[["mvTechAOut"]]@data |>
  #   # bind_rows(prec@parameters[["mvTechAOut"]]@data) |>
  #   rename(slicep = slice) |>
  #   select(-region, -year) |> unique() |>
  #   left_join(mCommSliceOrParent, by = c("comm", "slicep")) |>
  #   select(all_of(prec@parameters[["mTechCommAOutSliceSliceP"]]@dimSets)) |>
  #   unique()
  # prec@parameters[["mTechCommAOutSliceSliceP"]] <-
  #   .dat2par(prec@parameters[["mTechCommAOutSliceSliceP"]],
  #            mTechCommAOutSliceSliceP)

  # browser()
  # mTechInpCommAggSlice ####
  mTechInpCommAggSlice <- prec@parameters$mvTechInp@data |>
    select(-(any_of(c("region", "year")))) |> unique() |>
    left_join(prec@parameters$mCommSliceOrParent@data,
              by = c("comm", slice = "slicep"), suffix = c("", "p"))

  # mTechInpCommSameSlice ###
  mTechInpCommSameSlice <- mTechInpCommAggSlice |>
    filter(slicep == slice) |>
    select(-any_of(c("slicep", "slice"))) |>
    unique()
  prec@parameters[["mTechInpCommSameSlice"]] <-
    .dat2par(prec@parameters[["mTechInpCommSameSlice"]], mTechInpCommSameSlice)
  #
  # mvTechInpCommSameSlice <- prec@parameters[["mvTechInp"]]@data |>
  #   semi_join(mTechInpCommSameSlice,
  #             by = c("tech", "comm"))
  # prec@parameters[["mvTechInpCommSameSlice"]] <-
  #   .dat2par(prec@parameters[["mvTechInpCommSameSlice"]],
  #            mvTechInpCommSameSlice)
  # rm(mTechInpCommSameSlice, mvTechInpCommSameSlice)

  mTechInpCommAggSlice <- mTechInpCommAggSlice |>
    filter(slicep != slice) |>
    unique()
  prec@parameters[["mTechInpCommAggSlice"]] <-
    .dat2par(prec@parameters[["mTechInpCommAggSlice"]], mTechInpCommAggSlice)
  mTechInpCommAgg <- mTechInpCommAggSlice |>
    select(-any_of(c("slicep", "slice"))) |>
    unique()
  prec@parameters[["mTechInpCommAgg"]] <-
    .dat2par(prec@parameters[["mTechInpCommAgg"]], mTechInpCommAgg)
  rm(mTechInpCommAggSlice, mTechInpCommAgg)

  # mvTechInpCommAggSlice <- prec@parameters[["mvTechInp"]]@data |>
  #   semi_join(mTechInpCommAggSlice,
  #             by = c("tech", "comm")) |>
  #   unique()
  # prec@parameters[["mvTechInpCommAggSlice"]] <-
  #   .dat2par(prec@parameters[["mvTechInpCommAggSlice"]], mvTechInpCommAggSlice)
  # rm(mTechInpCommAggSlice, mvTechInpCommAggSlice)

  # mTechAInpCommAggSlice ####
  mTechAInpCommAggSlice <- prec@parameters$mvTechAInp@data |>
    select(-(any_of(c("region", "year")))) |> unique() |>
    left_join(prec@parameters$mCommSliceOrParent@data,
              by = c("comm", slice = "slicep"), suffix = c("", "p")) |>
    unique()

  # mTechAInpCommSameSlice ###
  mTechAInpCommSameSlice <- mTechAInpCommAggSlice |>
    filter(slicep == slice) |>
    select(-any_of(c("slicep", "slice"))) |>
    unique()
  prec@parameters[["mTechAInpCommSameSlice"]] <-
    .dat2par(prec@parameters[["mTechAInpCommSameSlice"]],
             mTechAInpCommSameSlice)
  rm(mTechAInpCommSameSlice)

  mTechAInpCommAggSlice <- mTechAInpCommAggSlice |>
    filter(slicep != slice) |>
    unique()
  prec@parameters[["mTechAInpCommAggSlice"]] <-
    .dat2par(prec@parameters[["mTechAInpCommAggSlice"]], mTechAInpCommAggSlice)
  mTechAInpCommAgg <- mTechAInpCommAggSlice |>
    select(-any_of(c("slicep", "slice"))) |>
    unique()
  prec@parameters[["mTechAInpCommAgg"]] <-
    .dat2par(prec@parameters[["mTechAInpCommAgg"]], mTechAInpCommAgg)
  rm(mTechAInpCommAggSlice, mTechAInpCommAgg)

  # mTechInpTot ####
  .interpolation_message("mTechInpTot", rest, interpolation_count,
                         interpolation_start_time, len_name)
  rest <- rest + 1
  reduce_total_map <- function(yy) {
    yy$slicep <- yy$slice
    yy$slice <- NULL
    reduce.duplicate(merge0(yy, mCommSliceOrParent,
                            by = c("comm", "slicep"))[, -2])
  }
  # browser()
  mTechInpTot <- rbind(
    select(.get_data_slot(prec@parameters$mvTechInp), -any_of("tech")),
    select(.get_data_slot(prec@parameters$mvTechAInp), -any_of("tech"))
  ) |>
    reduce.sect() |>
    reduce_total_map() |>
    inner_join(prec@parameters[["mCommReg"]]@data, by = c("comm", "region"))
  prec@parameters[["mTechInpTot"]] <-
    .dat2par(prec@parameters[["mTechInpTot"]], mTechInpTot)
  rm(mTechInpTot)

  # mTechOutTot ####
  .interpolation_message("mTechOutTot", rest, interpolation_count,
                         interpolation_start_time, len_name)
  rest <- rest + 1
  mTechOutTot <- rbind(
    select(.get_data_slot(prec@parameters$mvTechOut), -any_of("tech")),
    select(.get_data_slot(prec@parameters$mvTechAOut), -any_of("tech"))
  ) |>
    reduce.sect() |>
    reduce_total_map() |>
    inner_join(prec@parameters[["mCommReg"]]@data, by = c("comm", "region"))
  prec@parameters[["mTechOutTot"]] <-
    .dat2par(prec@parameters[["mTechOutTot"]], mTechOutTot)
  rm(mTechOutTot)
      # reduce_total_map(
      #   reduce.sect(
      #     rbind(.get_data_slot(prec@parameters$mvTechOut)[, -1],
      #           .get_data_slot(prec@parameters$mvTechAOut)[, -1])
      #     )
      #   )
      # )
  # browser()
  # mTechOutCommAggSlice ####
  mTechOutCommAggSlice <- prec@parameters$mvTechOut@data |>
    select(-(any_of(c("region", "year")))) |> unique() |>
    left_join(prec@parameters$mCommSliceOrParent@data,
              by = c("comm", slice = "slicep"), suffix = c("", "p"))

  # mTechOutCommSameSlice ###
  mTechOutCommSameSlice <- mTechOutCommAggSlice |>
    filter(slicep == slice) |>
    select(-any_of(c("slicep", "slice"))) |>
    unique()
  prec@parameters[["mTechOutCommSameSlice"]] <-
    .dat2par(prec@parameters[["mTechOutCommSameSlice"]], mTechOutCommSameSlice)
  rm(mTechOutCommSameSlice)

  mTechOutCommAggSlice <- mTechOutCommAggSlice |>
    filter(slicep != slice) |>
    unique()
  prec@parameters[["mTechOutCommAggSlice"]] <-
    .dat2par(prec@parameters[["mTechOutCommAggSlice"]], mTechOutCommAggSlice)

  mTechOutCommAgg <- mTechOutCommAggSlice |>
    select(-any_of(c("slicep", "slice"))) |>
    unique()
  prec@parameters[["mTechOutCommAgg"]] <-
    .dat2par(prec@parameters[["mTechOutCommAgg"]], mTechOutCommAgg)
  rm(mTechOutCommAggSlice, mTechOutCommAgg)

  # mTechAOutCommAggSlice ####
  mTechAOutCommAggSlice <- prec@parameters$mvTechAOut@data |>
    select(-(any_of(c("region", "year")))) |> unique() |>
    left_join(prec@parameters$mCommSliceOrParent@data,
              by = c("comm", slice = "slicep"), suffix = c("", "p"))

  # mTechAOutCommSameSlice ####
  mTechAOutCommSameSlice <- mTechAOutCommAggSlice |>
    filter(slicep == slice) |>
    select(-any_of(c("slicep", "slice"))) |>
    unique()
  prec@parameters[["mTechAOutCommSameSlice"]] <-
    .dat2par(prec@parameters[["mTechAOutCommSameSlice"]], mTechAOutCommSameSlice)
  rm(mTechAOutCommSameSlice)

  mTechAOutCommAggSlice <- mTechAOutCommAggSlice |>
    filter(slicep != slice) |>
    unique()
  prec@parameters[["mTechAOutCommAggSlice"]] <-
    .dat2par(prec@parameters[["mTechAOutCommAggSlice"]],
             mTechAOutCommAggSlice)

  mTechAOutCommAgg <- mTechAOutCommAggSlice |>
    select(-any_of(c("slicep", "slice"))) |>
    unique()
  # browser()
  prec@parameters[["mTechAOutCommAgg"]] <-
    .dat2par(prec@parameters[["mTechAOutCommAgg"]],
             mTechAOutCommAgg)
  rm(mTechAOutCommAgg)
  rm(mTechAOutCommAggSlice)

  # mSupOutTot ####
  .interpolation_message("mSupOutTot", rest, interpolation_count,
                         interpolation_start_time, len_name)
  rest <- rest + 1
  prec@parameters[["mSupOutTot"]] <-
    .dat2par(prec@parameters[["mSupOutTot"]],
             reduce.sect(.get_data_slot(prec@parameters$mSupAva)[, -1])
             )
  # pEmissionFactor ####
  # .interpolation_message("pEmissionFactor", rest, interpolation_count,
                         # interpolation_start_time, len_name)
  # rest <- rest + 1
  # mvTechAct ####
  # .interpolation_message("mvTechAct", rest, interpolation_count,
  #                        interpolation_start_time, len_name)
  # rest <- rest + 1
  # #### This section should be moved to add (after interpolate comm first)
  tmp0 <- .get_data_slot(prec@parameters$pTechEmisComm)
  tmp <- merge0(.get_data_slot(prec@parameters$mvTechInp),
                filter(tmp0, value != 0),
                by = c("tech", "comm"))
  colnames(tmp)[colnames(tmp) == "comm"] <- "commp"
  tmp1 <- .get_data_slot(prec@parameters$pEmissionFactor)
  tmp1 <- tmp1[tmp1$value != 0, ]
  tmp1 <- tmp1[!duplicated(tmp1), , drop = FALSE]
  tmp <- merge0(
    tmp1, tmp, by = "commp"
    )[, c("tech", "comm", "commp", "region", "year", "slice")]
  # tmp <- tmp[!duplicated(tmp), , drop = FALSE]
  tmp <- tmp |>
    inner_join(prec@parameters[["mCommReg"]]@data, by = c("comm", "region")) |>
    unique()
  # mTechEmsFuel ####
  # browser()
  .interpolation_message("mTechEmsFuel", rest, interpolation_count,
                         interpolation_start_time, len_name)
  rest <- rest + 1
  colnames(tmp)[3] <- "comm.1"
  prec@parameters[["mTechEmsFuel"]] <-
    .dat2par(prec@parameters[["mTechEmsFuel"]], tmp)
  # mEmsFuelTot ####
  .interpolation_message("mEmsFuelTot", rest, interpolation_count,
                         interpolation_start_time, len_name)
  rest <- rest + 1
  prec@parameters[["mEmsFuelTot"]] <-
    .dat2par(
      prec@parameters[["mEmsFuelTot"]],
      reduce_total_map(
        reduce.sect(
          .get_data_slot(prec@parameters[["mTechEmsFuel"]]),
          c("comm", "region", "year", "slice")
          )
        )
      )

  # browser()
  if (nrow(prec@parameters[["pTechCap"]]@data) > 0) {
    suppressMessages({
      mTechCap <- prec@parameters[["pTechCap"]]@data |>
        inner_join(prec@parameters[["mTechSpan"]]@data) |>
        # select(-value) |>
        unique()
    })
    mTechCapLo <- filter(mTechCap, type == "lo") |>
      select(-type, -value)
    if (!is.null(mTechCapLo) && nrow(mTechCapLo) > 0) {
      prec@parameters[["mTechCapLo"]] <-
        .dat2par(prec@parameters[["mTechCapLo"]], mTechCapLo)
    }
    rm(mTechCapLo)
    mTechCapUp <- filter(mTechCap, type == "up") |>
      select(-type, -value)
    if (!is.null(mTechCapUp) && nrow(mTechCapUp) > 0) {
      prec@parameters[["mTechCapUp"]] <-
        .dat2par(prec@parameters[["mTechCapUp"]], mTechCapUp)
    }
    rm(mTechCapUp)
  }

  if (nrow(prec@parameters[["pTechNewCap"]]@data) > 0) {
    # browser()
    suppressMessages({
      mTechNewCap <- prec@parameters[["pTechNewCap"]]@data |>
        inner_join(prec@parameters[["mTechNew"]]@data) |>
        # select(-value) |>
        unique()
    })
    mTechNewCapLo <- filter(mTechNewCap, type == "lo") |>
      select(-type, -value)
    if (!is.null(mTechNewCapLo) && nrow(mTechNewCapLo) > 0) {
      prec@parameters[["mTechNewCapLo"]] <-
        .dat2par(prec@parameters[["mTechNewCapLo"]], mTechNewCapLo)
    }
    rm(mTechNewCapLo)
    mTechNewCapUp <- filter(mTechNewCap, type == "up") |>
      select(-type, -value)
    if (!is.null(mTechNewCapUp) && nrow(mTechNewCapUp) > 0) {
      prec@parameters[["mTechNewCapUp"]] <-
        .dat2par(prec@parameters[["mTechNewCapUp"]], mTechNewCapUp)
    }
    rm(mTechNewCapUp)
  }

  # browser()
  if (nrow(prec@parameters[["pTechRet"]]@data) > 0 &&
      scen_settings@optimizeRetirement == TRUE) {
    suppressMessages({
      mTechRet <- prec@parameters[["pTechRet"]]@data |>
        inner_join(prec@parameters[["mTechSpan"]]@data) |>
        # select(-value) |>
        unique()
    })
    mTechRetLo <- filter(mTechRet, type == "lo") |>
      select(-type, -value)
    if (!is.null(mTechRetLo) && nrow(mTechRetLo) > 0) {
      prec@parameters[["mTechRetLo"]] <-
        .dat2par(prec@parameters[["mTechRetLo"]], mTechRetLo)
    }
    rm(mTechRetLo)
    mTechRetUp <- filter(mTechRet, type == "up") |>
      select(-type, -value)
    if (!is.null(mTechRetUp) && nrow(mTechRetUp) > 0) {
      prec@parameters[["mTechRetUp"]] <-
        .dat2par(prec@parameters[["mTechRetUp"]], mTechRetUp)
    }
    rm(mTechRetUp)
  }

  if (prec@parameters[["pStorageCap"]]@data |> nrow() > 0) {
    mStorageCap <- prec@parameters[["pStorageCap"]]@data |>
      inner_join(prec@parameters[["mStorageSpan"]]@data) |>
      # select(-value) |>
      unique()
    mStorageCapLo <- filter(mStorageCap, type == "lo") |>
      select(-type, -value)
    if (!is.null(mStorageCapLo) && nrow(mStorageCapLo) > 0) {
      prec@parameters[["mStorageCapLo"]] <-
        .dat2par(prec@parameters[["mStorageCapLo"]], mStorageCapLo)
    }
    rm(mStorageCapLo)
    mStorageCapUp <- filter(mStorageCap, type == "up") |>
      select(-type, -value)
    if (!is.null(mStorageCapUp) && nrow(mStorageCapUp) > 0) {
      prec@parameters[["mStorageCapUp"]] <-
        .dat2par(prec@parameters[["mStorageCapUp"]], mStorageCapUp)
    }
    rm(mStorageCapUp)
  }

  if (nrow(prec@parameters[["pStorageNewCap"]]@data) > 0) {
    suppressMessages({
      mStorageNewCap <- prec@parameters[["pStorageNewCap"]]@data |>
        inner_join(prec@parameters[["mStorageNew"]]@data) |>
        # select(-value) |>
        unique()
    })
    mStorageNewCapLo <- filter(mStorageNewCap, type == "lo") |>
      select(-type, -value)
    if (!is.null(mStorageNewCapLo) && nrow(mStorageNewCapLo) > 0) {
      prec@parameters[["mStorageNewCapLo"]] <-
        .dat2par(prec@parameters[["mStorageNewCapLo"]], mStorageNewCapLo)
    }
    rm(mStorageNewCapLo)
    mStorageNewCapUp <- filter(mStorageNewCap, type == "up") |>
      select(-type, -value)
    if (!is.null(mStorageNewCapUp) && nrow(mStorageNewCapUp) > 0) {
      prec@parameters[["mStorageNewCapUp"]] <-
        .dat2par(prec@parameters[["mStorageNewCapUp"]], mStorageNewCapUp)
    }
    rm(mStorageNewCapUp)
  }

  if(nrow(prec@parameters[["pStorageRet"]]@data) > 0) {
    suppressMessages({
      mStorageRet <- prec@parameters[["pStorageRet"]]@data |>
        inner_join(prec@parameters[["mStorageSpan"]]@data) |>
        # select(-value) |>
        unique()
    })
    mStorageRetLo <- filter(mStorageRet, type == "lo") |>
      select(-type, -value)
    if (!is.null(mStorageRetLo) && nrow(mStorageRetLo) > 0) {
      prec@parameters[["mStorageRetLo"]] <-
        .dat2par(prec@parameters[["mStorageRetLo"]], mStorageRetLo)
    }
    rm(mStorageRetLo)
    mStorageRetUp <- filter(mStorageRet, type == "up") |>
      select(-type, -value)
    if (!is.null(mStorageRetUp) && nrow(mStorageRetUp) > 0) {
      prec@parameters[["mStorageRetUp"]] <-
        .dat2par(prec@parameters[["mStorageRetUp"]], mStorageRetUp)
    }
    rm(mStorageRetUp)
  }
  # browser()
  # mTradeCap ####
  if (nrow(prec@parameters[["pTradeCap"]]@data) > 0) {
    suppressMessages({
      mTradeCap <- prec@parameters[["pTradeCap"]]@data |>
        inner_join(prec@parameters[["mTradeSpan"]]@data) |>
        # select(-value) |>
        unique()
    })
    mTradeCapLo <- filter(mTradeCap, type == "lo") |>
      select(-type, -value)
    if (!is.null(mTradeCapLo) && nrow(mTradeCapLo) > 0) {
      prec@parameters[["mTradeCapLo"]] <-
        .dat2par(prec@parameters[["mTradeCapLo"]], mTradeCapLo)
    }
    rm(mTradeCapLo)
    mTradeCapUp <- filter(mTradeCap, type == "up") |>
      select(-type, -value)
    if (!is.null(mTradeCapUp) && nrow(mTradeCapUp) > 0) {
      prec@parameters[["mTradeCapUp"]] <-
        .dat2par(prec@parameters[["mTradeCapUp"]], mTradeCapUp)
    }
    rm(mTradeCapUp)
  }
  # browser()
  #mTradeNewCap ####
  if (nrow(prec@parameters[["pTradeNewCap"]]@data) > 0) {
    suppressMessages({
      mTradeNewCap <- prec@parameters[["pTradeNewCap"]]@data |>
        inner_join(prec@parameters[["mTradeNew"]]@data) |>
        # select(-value) |>
        unique()
    })
    mTradeNewCapLo <- filter(mTradeNewCap, type == "lo") |>
      select(-type, -value)
    if (!is.null(mTradeNewCapLo) && nrow(mTradeNewCapLo) > 0) {
      prec@parameters[["mTradeNewCapLo"]] <-
        .dat2par(prec@parameters[["mTradeNewCapLo"]], mTradeNewCapLo)
    }
    rm(mTradeNewCapLo)
    mTradeNewCapUp <- filter(mTradeNewCap, type == "up") |>
      select(-type, -value)
    if (!is.null(mTradeNewCapUp) && nrow(mTradeNewCapUp) > 0) {
      prec@parameters[["mTradeNewCapUp"]] <-
        .dat2par(prec@parameters[["mTradeNewCapUp"]], mTradeNewCapUp)
    }
    rm(mTradeNewCapUp)
  }
  # browser()
  if (nrow(prec@parameters[["pTradeRet"]]@data) > 0) {
    suppressMessages({
      mTradeRet <- prec@parameters[["pTradeRet"]]@data |>
        inner_join(prec@parameters[["mTradeSpan"]]@data) |>
        # select(-value) |>
        unique()
    })
    mTradeRetLo <- filter(mTradeRet, type == "lo") |>
      select(-type, -value)
    if (!is.null(mTradeRetLo) && nrow(mTradeRetLo) > 0) {
      prec@parameters[["mTradeRetLo"]] <-
        .dat2par(prec@parameters[["mTradeRetLo"]], mTradeRetLo)
    }
    rm(mTradeRetLo)
    mTradeRetUp <- filter(mTradeRet, type == "up") |>
      select(-type, -value)
    if (!is.null(mTradeRetUp) && nrow(mTradeRetUp) > 0) {
      prec@parameters[["mTradeRetUp"]] <-
        .dat2par(prec@parameters[["mTradeRetUp"]], mTradeRetUp)
    }
    rm(mTradeRetUp)
  }

  # mDummyImport ####
  .interpolation_message("mDummyImport", rest, interpolation_count,
                         interpolation_start_time, len_name)
  rest <- rest + 1
  no_inf <- function(y) {
    x <- .get_data_slot(prec@parameters[[y]])
    if (!is.null(prec@parameters[[y]]@misc$not_need_interpolate)) {
      nn <- rev(prec@parameters[[y]]@misc$not_need_interpolate)
      nn[nn == "slice"] <- "mCommSlice"
      nn[nn == "year"] <- "mMidMilestone"
      for (i in nn) {
        x <- merge0(.get_data_slot(prec@parameters[[i]]), x)
      }
    }
    # x[x$value != Inf, -ncol(x)]
    x |> filter(value != Inf) |> select(-last_col())
  }
  # browser()
  prec@parameters[["mDummyImport"]] <-
    .dat2par(prec@parameters[["mDummyImport"]], no_inf("pDummyImportCost"))
  .interpolation_message("mDummyExport", rest, interpolation_count,
                         interpolation_start_time, len_name)
  rest <- rest + 1
  prec@parameters[["mDummyExport"]] <-
    .dat2par(prec@parameters[["mDummyExport"]], no_inf("pDummyExportCost"))


  .interpolation_message("mDummyCost", rest, interpolation_count,
                         interpolation_start_time, len_name)
  rest <- rest + 1

  prec@parameters[["mDummyImportCost"]] <- .dat2par(
    prec@parameters[["mDummyImportCost"]],
    reduce.sect(rbind(
      .get_data_slot(prec@parameters[["mDummyImport"]])
      # .get_data_slot(prec@parameters[["mDummyExport"]])
    ), c("comm", "region", "year"))
  )
  prec@parameters[["mDummyExportCost"]] <- .dat2par(
    prec@parameters[["mDummyExportCost"]],
    reduce.sect(rbind(
      .get_data_slot(prec@parameters[["mDummyExport"]])
    ), c("comm", "region", "year"))
  )


  .interpolation_message("mvTradeIrAInpTot", rest, interpolation_count,
                         interpolation_start_time, len_name)
  rest <- rest + 1

  # mvTradeIrAInpTot
  prec@parameters[["mvTradeIrAInpTot"]] <- .dat2par(
    prec@parameters[["mvTradeIrAInpTot"]],
    reduce_total_map(
      reduce.sect(
        .get_data_slot(prec@parameters$mvTradeIrAInp),
        c("comm", "region", "year", "slice"))
      )
    )
  .interpolation_message("mvTradeIrAOutTot", rest, interpolation_count,
                         interpolation_start_time, len_name)
  rest <- rest + 1
  prec@parameters[["mvTradeIrAOutTot"]] <- .dat2par(
    prec@parameters[["mvTradeIrAOutTot"]],
    reduce_total_map(
      reduce.sect(
        .get_data_slot(prec@parameters$mvTradeIrAOut),
        c("comm", "region", "year", "slice"))
      )
    )
  .interpolation_message("mTradeComm", rest, interpolation_count,
                         interpolation_start_time, len_name)
  rest <- rest + 1

  ### Export
  mCommSliceOrParent2 <- mCommSliceOrParent
  colnames(mCommSliceOrParent2)[3] <- "slice.1"
  mExportIrSubSliceTrd <- merge0(
    .get_data_slot(prec@parameters$mTradeComm),
    .get_data_slot(prec@parameters[["mTradeIr"]])
    )
  colnames(mExportIrSubSliceTrd)[6] <- "slice.1"
  mExportIrSubSliceTrd$dst <- NULL
  mExportIrSubSliceTrd <- merge0(mExportIrSubSliceTrd, mCommSliceOrParent2)
  colnames(mExportIrSubSliceTrd)[4] <- "region"
  mExportIrSubSliceTrd <- mExportIrSubSliceTrd[!duplicated(mExportIrSubSliceTrd), ]
  mExportIrSubSlice <- mExportIrSubSliceTrd[, -3]
  mExportIrSubSlice <- mExportIrSubSlice[!duplicated(mExportIrSubSlice), ]
  mExportIrSub <- mExportIrSubSlice[, -2]
  mExportIrSub <- mExportIrSub[!duplicated(mExportIrSub), ]
  mExportRowSubTmp <-
    reduce.sect(
      .get_data_slot(
        prec@parameters[["mExportRow"]]
        )[, c("comm", "region", "year", "slice")],
      c("comm", "region", "year", "slice")
      )
  colnames(mExportRowSubTmp)[4] <- "slice.1"
  mExportRowSubSlice <- merge0(mExportRowSubTmp, mCommSliceOrParent2)
  # mExportRowSub <- mExportRowSubSlice[, colnames(mExportRowSubSlice) != "slice.1"]
  mExportRowSub <- mExportRowSubSlice |> select(-any_of("slice.1"))
  mExportRowSub <- mExportRowSub[!duplicated(mExportRowSub), ]
  .interpolation_message("mExport", rest, interpolation_count,
                         interpolation_start_time, len_name)
  rest <- rest + 1
  mExport <- rbind(mExportRowSub, mExportIrSub)
  mExport <- mExport[!duplicated(mExport), ]
  prec@parameters[["mExport"]] <- .dat2par(prec@parameters[["mExport"]], mExport)

  ### Import
  mImportIrSubSliceTrd <- merge0(.get_data_slot(prec@parameters$mTradeComm),
                                 .get_data_slot(prec@parameters[["mTradeIr"]]))
  colnames(mImportIrSubSliceTrd)[6] <- "slice.1"
  mImportIrSubSliceTrd$src <- NULL
  mImportIrSubSliceTrd <- merge0(mImportIrSubSliceTrd, mCommSliceOrParent2)
  colnames(mImportIrSubSliceTrd)[4] <- "region"
  mImportIrSubSliceTrd <- mImportIrSubSliceTrd[!duplicated(mImportIrSubSliceTrd), ]
  mImportIrSubSlice <- mImportIrSubSliceTrd[, -3]
  mImportIrSubSlice <- mImportIrSubSlice[!duplicated(mImportIrSubSlice), ]
  mImportIrSub <- mImportIrSubSlice[, -2]
  mImportIrSub <- mImportIrSub[!duplicated(mImportIrSub), ]
  mImportRowSubTmp <- reduce.sect(
    .get_data_slot(
      prec@parameters[["mImportRow"]]
      )[, c("comm", "region", "year", "slice")],
    c("comm", "region", "year", "slice")
  )
  colnames(mImportRowSubTmp)[4] <- "slice.1"
  mImportRowSubSlice <- merge0(mImportRowSubTmp, mCommSliceOrParent2)
  # mImportRowSub <- mImportRowSubSlice[, colnames(mImportRowSubSlice) != "slice.1"]
  mImportRowSub <- mImportRowSubSlice |> select(-any_of("slice.1"))
  mImportRowSub <- mImportRowSub[!duplicated(mImportRowSub), ]
  .interpolation_message("mImport", rest, interpolation_count,
                         interpolation_start_time, len_name)
  rest <- rest + 1
  mImport <- rbind(mImportRowSub, mImportIrSub)
  mImport <- mImport[!duplicated(mImport), ]
  prec@parameters[["mImport"]] <-
    .dat2par(prec@parameters[["mImport"]], mImport)

  #
  .interpolation_message("mStorageInpTot", rest, interpolation_count,
                         interpolation_start_time, len_name)
  rest <- rest + 1

  prec@parameters[["mStorageInpTot"]] <- .dat2par(
    prec@parameters[["mStorageInpTot"]],
    reduce.sect(
      rbind(
        .get_data_slot(
          prec@parameters$mvStorageAInp)[, -1],
        .get_data_slot(prec@parameters$mvStorageStore)[, -1]
        )
      )
    )
  .interpolation_message("mStorageOutTot", rest, interpolation_count,
                         interpolation_start_time, len_name)
  rest <- rest + 1
  prec@parameters[["mStorageOutTot"]] <- .dat2par(
    prec@parameters[["mStorageOutTot"]],
    reduce.sect(
      rbind(
        .get_data_slot(
          prec@parameters$mvStorageAInp)[, -1],
        .get_data_slot(prec@parameters$mvStorageStore)[, -1]
        )
      )
    )
  .interpolation_message("mTaxCost", rest, interpolation_count,
                         interpolation_start_time, len_name)
  rest <- rest + 1

  # mTaxCost(comm, region, year)  sum(slice$pTaxCost(comm, region, year, slice), 1)
  prec@parameters[["mTaxCost"]] <- .dat2par(
    prec@parameters[["mTaxCost"]],
    reduce.sect.merge.unique(list(
      .get_data_slot(prec@parameters$pTaxCostInp),
      .get_data_slot(prec@parameters$pTaxCostOut),
      .get_data_slot(prec@parameters$pTaxCostBal)
    ), c("comm", "region", "year"))
  )
  # mSubCost(comm, region, year)  sum(slice$pSubCost(comm, region, year, slice), 1)
  .interpolation_message("mSubCost", rest, interpolation_count,
                         interpolation_start_time, len_name)
  rest <- rest + 1
  prec@parameters[["mSubCost"]] <- .dat2par(
    prec@parameters[["mSubCost"]],
    reduce.sect.merge.unique(list(
      .get_data_slot(prec@parameters$pSubCostInp),
      .get_data_slot(prec@parameters$pSubCostOut),
      .get_data_slot(prec@parameters$pSubCostBal)
    ), c("comm", "region", "year"))
  )
  #    (sum(commp$pAggregateFactor(comm, commp), 1))
  .interpolation_message("mAggOut", rest, interpolation_count,
                         interpolation_start_time, len_name)
  rest <- rest + 1
  if (nrow(.get_data_slot(prec@parameters$pAggregateFactor)) > 0) {
    prec@parameters[["mAggOut"]] <-
      .dat2par(
        prec@parameters[["mAggOut"]],
        reduce_total_map(
          reduce.duplicate(
            merge0(
              merge0(
                merge0(
                  reduce.sect(
                    .get_data_slot(prec@parameters$pAggregateFactor), "comm"
                    ),
                  .get_data_slot(prec@parameters$region)
                  ),
                year
                ),
              .get_data_slot(prec@parameters$slice)
              )
            )
          )
        )
  }
  .interpolation_message("mOut2Lo", rest, interpolation_count,
                         interpolation_start_time, len_name)
  rest <- rest + 1

  a1 <- mCommSlice
  colnames(a1)[2] <- "slicep"
  a2 <- .get_data_slot(prec@parameters$mSliceParentChild)
  for2Lo <- merge0(a1, a2, by = "slicep")
  for2Lo$slicep <- NULL
  for2Lo <- reduce.duplicate(for2Lo)
  # cll <- c("comm", "region", "year", "slice")
  # browser()
  mOut2Lo <-
    merge0(
      reduce.duplicate(
        rbind(
          merge0(
            .get_data_slot(prec@parameters$mSupOutTot),
            .get_data_slot(prec@parameters$year)
          )[, c("comm", "region", "year", "slice")],
          .get_data_slot(
            prec@parameters$mEmsFuelTot
          )[, c("comm", "region", "year", "slice")],
          .get_data_slot(
            prec@parameters$mAggOut
          )[, c("comm", "region", "year", "slice")],
          .get_data_slot(
            prec@parameters$mTechOutTot
          )[, c("comm", "region", "year", "slice")],
          .get_data_slot(
            prec@parameters$mStorageOutTot
          )[, c("comm", "region", "year", "slice")],
          .get_data_slot(
            prec@parameters$mImport
          )[, c("comm", "region", "year", "slice")],
          .get_data_slot(
            prec@parameters$mvTradeIrAOutTot
          )[, c("comm", "region", "year", "slice")]
        )
      ),
      for2Lo,
      by = c("comm", "slice")
    )[, c("comm", "region", "year", "slice")]
  mOut2Lo <- mOut2Lo[!(
    paste0(mOut2Lo$comm, "#", mOut2Lo$slice) %in%
      paste0(mCommSlice$comm, "#", mCommSlice$slice)
    ), ]
  prec@parameters[["mOut2Lo"]] <-
    .dat2par(prec@parameters[["mOut2Lo"]], mOut2Lo)

  .interpolation_message("mInp2Lo", rest, interpolation_count,
                         interpolation_start_time, len_name)
  rest <- rest + 1
  mInp2Lo <- merge0(
    reduce.duplicate(rbind(
      .get_data_slot(prec@parameters$mTechInpTot)[, c("comm", "region", "year", "slice")],
      .get_data_slot(prec@parameters$mStorageInpTot)[, c("comm", "region", "year", "slice")],
      .get_data_slot(prec@parameters$mExport)[, c("comm", "region", "year", "slice")],
      .get_data_slot(prec@parameters$mvTradeIrAInpTot)[, c("comm", "region", "year", "slice")]
    )),
    for2Lo,
    by = c("comm", "slice")
  )[, c("comm", "region", "year", "slice")]
  mInp2Lo <- mInp2Lo[!(paste0(mInp2Lo$comm, "#", mInp2Lo$slice) %in% paste0(mCommSlice$comm, "#", mCommSlice$slice)), ]
  prec@parameters[["mInp2Lo"]] <- .dat2par(prec@parameters[["mInp2Lo"]], mInp2Lo)
  # browser()
  ##
  dregionyear <- merge0(region, year)
  # .interpolation_message("mvTradeCost", rest, interpolation_count, interpolation_start_time, len_name)
  # rest <- rest + 1
  # prec@parameters[["mvTradeCost"]] <- .dat2par(prec@parameters[["mvTradeCost"]], dregionyear)
  # .interpolation_message("mvTradeRowCost", rest, interpolation_count, interpolation_start_time, len_name)
  # rest <- rest + 1
  # prec@parameters[["mvTradeRowCost"]] <- .dat2par(prec@parameters[["mvTradeRowCost"]], dregionyear)
  # .interpolation_message("mvTradeIrCost", rest, interpolation_count, interpolation_start_time, len_name)
  # rest <- rest + 1
  # prec@parameters[["mvTradeIrCost"]] <- .dat2par(prec@parameters[["mvTradeIrCost"]], dregionyear)
  prec@parameters[["mExportRowCost"]] <- .dat2par(
    prec@parameters[["mExportRowCost"]],
    merge(
      unique(
        select(
          .get_data_slot(prec@parameters[["mExportRow"]]),
          all_of(prec@parameters[["mExportRowCost"]]@dimSets)
        )
      ),
      dregionyear
    )
  )

  prec@parameters[["mImportRowCost"]] <- .dat2par(
    prec@parameters[["mImportRowCost"]],
    merge(
      unique(
        select(
          .get_data_slot(prec@parameters[["mImportRow"]]),
          all_of(prec@parameters[["mImportRowCost"]]@dimSets)
        )
      ),
      dregionyear
    )
  )
  # mImportIrCost(trade, region, year)
  x <- .get_data_slot(prec@parameters[["mTradeIr"]]) |>
    rename(region = dst) |>
    select(any_of(prec@parameters[["mImportIrCost"]]@dimSets)) |>
    unique()
  prec@parameters[["mImportIrCost"]] <- .dat2par(
    prec@parameters[["mImportIrCost"]],
    merge0(x, dregionyear)
    )

  # mExportIrCost(trade, region, year)
  x <- .get_data_slot(prec@parameters[["mTradeIr"]]) |>
    rename(region = src) |>
    select(any_of(prec@parameters[["mExportIrCost"]]@dimSets)) |>
    unique()

  prec@parameters[["mExportIrCost"]] <- .dat2par(
    prec@parameters[["mExportIrCost"]],
    merge(x, dregionyear)
  )
  rm(x)

  .interpolation_message("mvTotalCost", rest, interpolation_count, interpolation_start_time, len_name)
  rest <- rest + 1

  prec@parameters[["mvTotalCost"]] <- .dat2par(prec@parameters[["mvTotalCost"]], dregionyear)
  .interpolation_message("mvInp2Lo", rest, interpolation_count, interpolation_start_time, len_name)
  rest <- rest + 1
  mCommSlice2 <- .get_data_slot(prec@parameters[["mCommSlice"]])
  colnames(mCommSlice2)[2] <- "slicep"
  mvInp2Lo <- merge0(.get_data_slot(prec@parameters[["mInp2Lo"]]), .get_data_slot(prec@parameters[["mSliceParentChild"]]))[, c("comm", "region", "year", "slice", "slicep")]
  mvInp2Lo <- merge0(mvInp2Lo, mCommSlice2)
  colnames(mvInp2Lo)[colnames(mvInp2Lo) == "slicep"] <- "slice.1"
  mvInp2Lo <- mvInp2Lo[, c("comm", "region", "year", "slice", "slice.1")]
  prec@parameters[["mvInp2Lo"]] <- .dat2par(prec@parameters[["mvInp2Lo"]], mvInp2Lo)

  .interpolation_message("mInpSub", rest, interpolation_count, interpolation_start_time, len_name)
  rest <- rest + 1
  if (!is.null(mvInp2Lo)) {
    mInpSub <- mvInp2Lo[!duplicated(mvInp2Lo[, -4]), -4]
    colnames(mInpSub)[4] <- "slice"
    prec@parameters[["mInpSub"]] <- .dat2par(prec@parameters[["mInpSub"]], mInpSub)
  }

  .interpolation_message("mvOut2Lo", rest, interpolation_count,
                         interpolation_start_time, len_name)
  rest <- rest + 1
  mvOut2Lo <- merge0(
    .get_data_slot(prec@parameters[["mOut2Lo"]]),
    .get_data_slot(
      prec@parameters[["mSliceParentChild"]]))[
        , c("comm", "region", "year", "slice", "slicep")
        ]
  mvOut2Lo <- merge0(mvOut2Lo, mCommSlice2)
  colnames(mvOut2Lo)[colnames(mvOut2Lo) == "slicep"] <- "slice.1"
  mvOut2Lo <- mvOut2Lo[, c("comm", "region", "year", "slice", "slice.1")]

  prec@parameters[["mvOut2Lo"]] <-
    .dat2par(prec@parameters[["mvOut2Lo"]], mvOut2Lo)

  .interpolation_message("mOutSub", rest, interpolation_count,
                         interpolation_start_time, len_name)
  rest <- rest + 1
  if (!is.null(mvOut2Lo)) {
    mOutSub <- mvOut2Lo[!duplicated(mvOut2Lo[, -4]), -4]
    colnames(mOutSub)[4] <- "slice"
    prec@parameters[["mOutSub"]] <-
      .dat2par(prec@parameters[["mOutSub"]], mOutSub)
  }
  .interpolation_message("mvTotalUserCosts", rest, interpolation_count,
                         interpolation_start_time, len_name)
  rest <- rest + 1

  mCosts <- lapply(grep("^mCosts", names(prec@parameters), value = TRUE),
                   function(x) {
    xx <- .get_data_slot(prec@parameters[[x]])
    # xx <- unique(xx[, colnames(xx) %in% c("region", "year"), drop = FALSE])
    if (sum(colnames(xx) %in% c("region", "year")) > 2) browser() # rewrite for multiple columns
    xx <- select(xx, any_of(c("region", "year"))) |> unique()
    if (nrow(xx) == nrow(dregionyear) || ncol(xx) == 0) {
      return(dregionyear)
    }
    if (is.null(xx$region)) {
      return(dregionyear[dregionyear$year %in% unique(xx$year), , drop = FALSE])
    } else if (is.null(xx$year)) {
      return(dregionyear[
        dregionyear$region %in% unique(xx$region), , drop = FALSE
        ])
    } else {
      return(xx)
    }
  })
  if (any(sapply(mCosts, nrow) == nrow(dregionyear))) {
    prec@parameters[["mvTotalUserCosts"]] <- .dat2par(prec@parameters[["mvTotalUserCosts"]], dregionyear)
  } else {
    if (length(mCosts) != 0) {
      mCosts2 <- mCosts[[1]][0, ]
      mCosts2[prod(sapply(mCosts2, nrow)), ] <- NA
      nnn <- 0
      for (qqq in seq_along(mCosts2)) {
        mCosts2[seq_len(nrow(mCosts[[qqq]])) + nnn, ] <- mCosts[[qqq]]
        nnn <- nnn + nrow(mCosts[[qqq]])
      }
      mCosts <- mCosts2
      prec@parameters[["mvTotalUserCosts"]] <- .dat2par(
        prec@parameters[["mvTotalUserCosts"]],
        unique(mCosts)
      )
    }
  }
  # browser()
  if (length(prec@costs.equation) == 0) {
    prec@costs.equation <-
      "eqTotalUserCosts(region, year)$mvTotalUserCosts(region, year).. vTotalUserCosts(region, year) =e= 0;"
  } else {
    prec@costs.equation <- paste0(
      "eqTotalUserCosts(region, year)$mvTotalUserCosts(region, year)..",
      "   vTotalUserCosts(region, year) =e= ",
      gsub("[+][ ]*[-]", "-", paste0(prec@costs.equation, collapse = " + ")), ";"
    )
  }

  tmp_f0 <- function(x) {
    if (nrow(x) == 0) {
      x$value <- numeric()
      return(x)
    }
    x$value <- 1
    x
  }
  # browser()
  # mvInpTot ####
  .interpolation_message("mvInpTot", rest, interpolation_count,
                         interpolation_start_time, len_name)
  rest <- rest + 1
  mvInpTot <- rbind(
    .get_data_slot(prec@parameters$mvDemInp),
    .get_data_slot(prec@parameters$mDummyExport),
    .get_data_slot(prec@parameters$mTechInpTot),
    .get_data_slot(prec@parameters$mStorageInpTot),
    .get_data_slot(prec@parameters$mExport),
    .get_data_slot(prec@parameters$mvTradeIrAInpTot),
    .get_data_slot(prec@parameters$mInpSub)
  )
  mvInpTot <- mvInpTot[!duplicated(mvInpTot), ]
  mvInpTot <- merge0(mvInpTot, mCommSlice) |> unique()
  # if (T) { # check
  #   # mvInpTot <-
  #   dim_mvInpTot <- mvInpTot |>
  #     inner_join(prec@parameters$mCommReg@data, by = c("comm", "region")) |>
  #     unique() |> dim()
  #   if (!all(dim_mvInpTot == dim(mvInpTot))) {
  #    if (F) browser() # Debug
  #     x <- merge0(dregionyear, mCommSlice) |>
  #       inner_join(prec@parameters$mCommReg@data, by = c("comm", "region")) |>
  #       unique()
  #     browser()
  #     suppressMessages({
  #       y <- anti_join(x, mvBalance)
  #     })
  #     yc <- y$comm |> unique()
  #     warning("Dropped commodities: ", paste(yc, collapse = ", ", sep = ""))
  #     rm(x, y, yc)
  #   }
  # }
  prec@parameters[["mvInpTot"]] <-
    .dat2par(prec@parameters[["mvInpTot"]], mvInpTot)
  # rm(mvInpTot)

  # mInpTotRY
  mInpTotRY <- mvInpTot |> select(-slice) |> unique()
  prec@parameters[["mInpTotRY"]] <-
    .dat2par(prec@parameters[["mInpTotRY"]], mInpTotRY)

  # mvOutTot ####
  .interpolation_message("mvOutTot", rest, interpolation_count,
                         interpolation_start_time, len_name)
  rest <- rest + 1
  mvOutTot <- rbind(
    .get_data_slot(prec@parameters$mDummyImport),
    .get_data_slot(prec@parameters$mSupOutTot),
    .get_data_slot(prec@parameters$mEmsFuelTot),
    .get_data_slot(prec@parameters$mAggOut),
    .get_data_slot(prec@parameters$mTechOutTot),
    .get_data_slot(prec@parameters$mStorageOutTot),
    .get_data_slot(prec@parameters$mImport),
    .get_data_slot(prec@parameters$mvTradeIrAOutTot),
    .get_data_slot(prec@parameters$mOutSub)
  )
  mvOutTot <- mvOutTot[!duplicated(mvOutTot), ]
  mvOutTot <- merge0(mvOutTot, mCommSlice) |> unique()
  # if (T) { # check
  #   # moved below with an adjustment for 'mAggregateFactor'
  #   dim_mvOutTot <- mvOutTot |>
  #     inner_join(prec@parameters$mCommReg@data, by = c("comm", "region")) |>
  #     unique() |> dim()
  #   if (!all(dim_mvOutTot == dim(mvOutTot))) browser() # Debug
  # }
  prec@parameters[["mvOutTot"]] <-
    .dat2par(prec@parameters[["mvOutTot"]], mvOutTot)
  # rm(mvOutTot) # moved below
  # mvBalance ####
  .interpolation_message("mvBalance", rest, interpolation_count,
                         interpolation_start_time, len_name)
  rest <- rest + 1
  mvBalance <- rbind(
    prec@parameters[["mvInpTot"]]@data,
    prec@parameters[["mvOutTot"]]@data) |>
    unique()
  # mvBalance <- mvBalance[!duplicated(mvBalance), ]
  if (T) { # check
    dim_mvBalance <- merge0(dregionyear, mCommSlice) |>
      inner_join(prec@parameters$mCommReg@data, by = c("comm", "region")) |>
      unique() |> dim()
    if (!all(dim_mvBalance == dim(mvBalance))) {
      # browser() # !!! Debug
      x <- merge0(dregionyear, mCommSlice) |>
        inner_join(prec@parameters$mCommReg@data, by = c("comm", "region")) |>
        unique()
      y <- anti_join(x, mvBalance)
      y$comm |> unique() # check
    }
  }
  prec@parameters[["mvBalance"]] <-
    .dat2par(prec@parameters[["mvBalance"]], mvBalance)

  mBalanceRY <- mvBalance |> select(-slice) |> unique()
  prec@parameters[["mBalanceRY"]] <-
    .dat2par(prec@parameters[["mBalanceRY"]], mBalanceRY)

  # prec@parameters[["mvInpTot"]] <-
  #   .dat2par(prec@parameters[["mvInpTot"]], mvBalance)
  # prec@parameters[["mvOutTot"]] <-
  #   .dat2par(prec@parameters[["mvOutTot"]], mvBalance)

  if (T) { # check
    # mvInpTot <-
    dim_mvInpTot <- mvInpTot |>
      inner_join(prec@parameters$mCommReg@data, by = c("comm", "region")) |>
      unique() |> dim()
    if (!all(dim_mvInpTot == dim(mvInpTot))) {
      if (F) browser() # Debug
      x <- merge0(dregionyear, mCommSlice) |>
        inner_join(prec@parameters$mCommReg@data, by = c("comm", "region")) |>
        unique()
      suppressMessages({
        y <- anti_join(x, mvBalance)
      })
      yc <- y$comm |> unique()
      # !!! ToDo: add checks for agg-commodities
      if (length(yc) > 0) {
        warning("Dropped commodities: ", paste(yc, collapse = ", ", sep = ""))
      }
      rm(x, y, yc)
    }
    rm(mvInpTot)
  }

  .interpolation_message("meqBalLo", rest, interpolation_count, interpolation_start_time, len_name)
  rest <- rest + 1
  prec@parameters[["meqBalLo"]] <- .dat2par(
    prec@parameters[["meqBalLo"]],
    merge0(.get_data_slot(prec@parameters[["mvBalance"]]),
           .get_data_slot(prec@parameters[["mLoComm"]]))
  )
  .interpolation_message("meqBalUp", rest, interpolation_count, interpolation_start_time, len_name)
  rest <- rest + 1
  prec@parameters[["meqBalUp"]] <- .dat2par(
    prec@parameters[["meqBalUp"]],
    merge0(.get_data_slot(prec@parameters[["mvBalance"]]),
           .get_data_slot(prec@parameters[["mUpComm"]]))
  )
  .interpolation_message("meqBalFx", rest, interpolation_count, interpolation_start_time, len_name)
  rest <- rest + 1
  prec@parameters[["meqBalFx"]] <- .dat2par(
    prec@parameters[["meqBalFx"]],
    merge0(.get_data_slot(prec@parameters[["mvBalance"]]),
           .get_data_slot(prec@parameters[["mFxComm"]]))
  )

  .interpolation_message("mAggregateFactor", rest, interpolation_count, interpolation_start_time, len_name)
  rest <- rest + 1
  tmp <- .get_data_slot(prec@parameters[["pAggregateFactor"]])
  tmp <- tmp[tmp$value != 0, ]
  if (nrow(tmp)) {
    tmp$value <- NULL
    colnames(tmp)[2] <- "comm.1"
    prec@parameters[["mAggregateFactor"]] <-
      .dat2par(prec@parameters[["mAggregateFactor"]], tmp)
  }
  cat(bacs, paste0(rep(" ", len_name), collapse = ""), bacs)
  # mvOutTot |>
  #   filter(comm %in% prec@parameters[["mAggregateFactor"]]@data$comm.1) |>
  #   arrange(comm, year) |>
  #   left_join(prec@parameters[["mCommSlice"]]@data, by = c("comm", "slice")) |>
  # if (T) { # check
  #   # mvOutTot <-
  #   dim_mvOutTot <- mvOutTot |>
  #     inner_join(prec@parameters$mCommReg@data, by = c("comm", "region")) |>
  #     unique() |> dim()
  #   if (!all(dim_mvOutTot == dim(
  #     filter(mvOutTot, !(comm %in% prec@parameters$mAggregateFactor@data$comm))
  #     ))) browser() # Debug
  # }
  mOutTotRY <- mvOutTot |> select(-slice) |> unique()
  prec@parameters[["mOutTotRY"]] <-
    .dat2par(prec@parameters[["mOutTotRY"]], mOutTotRY)
  rm(mvOutTot)
  prec
}

# browser()


# Sets, parameters, + to use in write_* and interpolation functions ####
.set_al <- c("acomm", "stg", "trade", "expp", "imp", "tech", "dem", "sup", "weather", "region", "year", "slice", "group", "comm", "cns", "stgp", "tradep", "exppp", "impp", "techp", "demp", "supp", "weatherp", "regionp", "yearp", "slicep", "groupp", "commp", "cnsp", "stge", "tradee", "exppe", "impe", "teche", "deme", "supe", "weathere", "regione", "yeare", "slicee", "groupe", "comme", "cnse", "stgn", "traden", "exppn", "impn", "techn", "demn", "supn", "weathern", "regionn", "yearn", "slicen", "groupn", "commn", "cnsn", "src", "dst")
.alias_set <- c("ca", "st1", "t1", "e", "i", "t", "d", "s1", "wth1", "r", "y", "s", "g", "c", "cn1", "st1p", "t1p", "ep", "ip", "tp", "dp", "s1p", "wth1p", "rp", "yp", "sp", "gp", "cp", "cn1p", "st1e", "t1e", "ee", "ie", "te", "de", "s1e", "wth1e", "re", "ye", "se", "ge", "ce", "cn1e", "st1n", "t1n", "en", "in", "tn", "dn", "s1n", "wth1n", "rn", "yn", "sn", "gn", "cn", "cn1n", "src", "dst")
names(.alias_set) <- .set_al

.aliasName <- function(x) {
  if (!all(x %in% .set_al)) {
    cat("Unknown .set_al\n")
    browser()
    stop("Unknown set")
  }
  .alias_set[x]
}


.fremset <- c("comm", "stg", "trade", "expp", "imp", "tech", "dem", "sup", "weather", "region", "year", "slice", "group", "comm", "cns", "stg", "trade", "expp", "imp", "tech", "dem", "sup", "weather", "region", "year", "slice", "group", "comm", "cns", "stg", "trade", "expp", "imp", "tech", "dem", "sup", "weather", "region", "year", "slice", "group", "comm", "cns", "stg", "trade", "expp", "imp", "tech", "dem", "sup", "weather", "region", "year", "slice", "group", "comm", "cns", "region", "region")
names(.fremset) <- .set_al

.removeEndSet <- function(x) {
  .fremset[x]
}
