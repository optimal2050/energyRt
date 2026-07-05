#' Set GAMS and GDX library directory
#'
#' @description
#' This (optional) function sets path to GAMS directory to R-options. It might be useful if for the cases when several different version (and licenses) of GAMS installed, to easily switch between them. It is also possible to set different path for GAMS and GAMS Data Exchange (GDX) libraries.
#' If GDX path is not set, the GAMS path will be used. If GAMS path is not set, the default system GAMS-path (OS environment variables) instead.
#'
#' @param path character, path to installed GAMS distribution to use to solve models and/or with GDX library to use in reading and writing gdx-files.
#' @return
#' Sets path to GAMS library in R-options
#' @rdname solver
#' @family GAMS GDX solver
#' @export
#' @examples
#' # set_gams_path("C:/GAMS/win64/32.2/")
#'
set_gams_path <- function(path = NULL) {
  # browser()
  if (!is.null(path) && path != "") {
    if (!dir.exists(path)) {
      stop(paste0('The path "', path, '" does not exist.'), call. = FALSE)
    }
    if (!grepl("\\/$", path)) {
      path <- paste0(path, "/")
    }
  }
  options::opt_set("gams_path", path, env = "energyRt")
  # options(gams_path = path)
}

#' @rdname solver
#' @family GAMS GDX solver
#' @return
#' The current path to GAMS library, set in R-options
#' @export
#' @examples
#' # get_gams_path()
get_gams_path <- function() {
  options::opt("gams_path", env = "energyRt")
  # options::opt("gams_path")
}

#' @return
#'  Sets path to GDX library in R-options
#' @export
#'
#' @rdname solver
#' @family GAMS GDX solver
#'
#' @examples
#' # set_gdxlib("C:/GAMS/35")
set_gdxlib_path <- function(path = NULL) {
  if (!is.null(path) && path != "") {
    if (!dir.exists(path)) {
      stop(paste0('The path "', path, '" does not exist.'), call. = FALSE)
    }
    if (!grepl("\\/$", path)) {
      path <- paste0(path, "/")
    }
  }
  options::opt_set("gdxlib_path", path, env = "energyRt")
}

#' @rdname solver
#' @family GAMS GDX solver
#' @return
#' The current path to GDX library, set in R-options
#' @export
#' @examples
#' # get_gdxlib()
get_gdxlib_path <- function() {
  options::opt("gdxlib_path", env = "energyRt")
  # options::opt("gdxlib_path")
}

.check_load_gdxlib <- function() {
  # rw <- require("gdxrrw")
  # if (!rw) {
  #   stop('"gdxrrw" package has not been found. ',
  #        'It is required for writing and reading "*.gdx" files.\n',
  #        'Check: "https://github.com/GAMS-dev/gdxrrw"')
  # }
  rw <- requireNamespace("gdxtools", quietly = TRUE)
  # rw <- require("gdxtools", quietly = TRUE)
  if (!rw) {
    stop('"gdxtools" package has not been found. ',
         'It is required for reading "*.gdx" files.\n',
         'To install: pak::pkg_install("lolow/gdxtools")'
         )
  }
  en_gdxlib_loaded <- getOption("en_gdxlib_loaded")
  if (is.null(en_gdxlib_loaded) || as.logical(en_gdxlib_loaded) == FALSE) {
    lb <- options::opt("gdxlib_path")
    if (is.null(lb)) {
      lb <- options::opt("gams_path")
    }
    ix <- gdxtools::igdx(lb)
    if (!ix) {
      stop('Cannot load "gdx" library. Check "?set_gdxlib_path" to setup.')
    } else {
      options(en_gdxlib_loaded = TRUE)
    }
  }
}

.check_load_gdxtools <- function() {
  xt <- rlang::is_installed("gdxtools")
  if (!xt) {
    stop('"gdxtools" package has not been found. ',
         'It is required for reading "*.gdx" files.\n',
         'To install: "https://github.com/lolow/gdxtools".')
  }
  xt <- requireNamespace("gdxtools", quietly = TRUE)
  # xt <- require("gdxtools", quietly = TRUE)
  en_gdxlib_loaded <- getOption("en_gdxlib_loaded")
  if (is.null(en_gdxlib_loaded) || as.logical(en_gdxlib_loaded) == FALSE) {
    lb <- options::opt("gdxlib_path")
    if (is.null(lb)) {
      lb <- options::opt("gams_path")
    }
    ix <- gdxtools::igdx(lb)
    if (!ix) {
      stop('Cannot load "gdx" library. Check "?set_gdxlib_path" to setup.')
    } else {
      options(en_gdxlib_loaded = TRUE)
    }
  }
}


# Internal functions to write GAMS model files
.write_model_GAMS <- function(arg, scen, trim = FALSE) {
  # if (trim) scen <- fold(scen)
  # browser()
  .write_inc_solver(scen, arg, "option lp = cplex;", ".gms", "cplex")
  if (is.null(scen@status$sparse)) stop("scen@status$sparse not found")
  # GAMS needs a DENSE scenario: it has no native parameter default (an absent
  # tuple reads as 0, so omitting non-zero-default rows would be silently wrong)
  # and it does not substitute fold wildcards. Require an explicit dense build
  # rather than transforming here at write time -- a write-time unfold/densify
  # cannot faithfully reproduce the full `sparse = FALSE` interpolation (notably
  # map wildcards such as `mTradeRetUp`, which the value-parameter densify does
  # not touch).
  if (isTRUE(scen@status$sparse)) {
    stop("GAMS export requires a dense scenario. Re-interpolate with ",
         "interp_mod(..., sparse = FALSE) (which also disables folding), then ",
         "write to GAMS.")
  }
  # A dense scenario is always written with the dense (full-set) GAMS form.
  .toGams <- function(x) .toGams0(x, FALSE)
  run_code <- scen@settings@sourceCode[["GAMS"]]

  # For downsize
  fdownsize <- names(scen@modInp@parameters)[
    sapply(scen@modInp@parameters, function(x) length(x@misc$rem_col) != 0)
  ]
  for (nn in fdownsize) {
    rmm <- scen@modInp@parameters[[nn]]@misc$rem_col
    if (scen@modInp@parameters[[nn]]@type == "bounds") {
      uuu <- paste0(nn, c("Lo", "Up"))
    } else {
      uuu <- nn
    }
    for (yy in uuu) {
      templ <- paste0("(^|[^[:alnum:]])", yy, "[(]")
      if (any(grep("^pCns", nn))) {
        for (www in seq_along(scen@modInp@gams.equation)) {
          mmm <- grep(templ, scen@modInp@gams.equation[[www]]$equation)
          if (any(mmm)) {
            scen@modInp@gams.equation[[www]]$equation[mmm] <- sapply(
              strsplit(scen@modInp@gams.equation[[www]]$equation[mmm], yy),
              .rem_col, yy, rmm
            )
          }
        }
      } else if (any(grep("^pCosts", nn))) {
        # browser()
        mmm <- grep(templ, scen@modInp@costs.equation)
        if (any(mmm)) {
          scen@modInp@costs.equation[mmm] <- sapply(
            strsplit(scen@modInp@costs.equation[mmm], yy), .rem_col, yy, rmm
          )
        }
      } else {
        mmm <- grep(templ, run_code)
        if (any(mmm)) {
          run_code[mmm] <- sapply(
            strsplit(run_code[mmm], yy), .rem_col, yy, rmm
          )
        }
      }
    }
  }

  if (is.null(scen@settings@solver$export_format)) {
    scen@settings@solver$export_format <- "gms"
  }
  if (is.null(scen@settings@solver$import_format)) {
    scen@settings@solver$import_format <- "gms"
  } else {
    scen@settings@solver$import_format <- "gdx"
    scen@settings@sourceCode[["GAMS_output"]] <- c(
      scen@settings@sourceCode[["GAMS_output"]][grep(
      "^file variable_list_csv",
      scen@settings@sourceCode[["GAMS_output"]]
    ):length(scen@settings@sourceCode[["GAMS_output"]])],
    'execute_unload "output/output.gdx"')
  }
  dir.create(fp(arg$tmp.dir, "input"), showWarnings = FALSE)
  dir.create(fp(arg$tmp.dir, "output"), showWarnings = FALSE)
  # browser()
  zz_output <- file(fp(arg$tmp.dir, "output.gms"), "w")
  cat(scen@settings@sourceCode[["GAMS_output"]], sep = "\n", file = zz_output)
  close(zz_output)
  zz_data_gms <- file(fp(arg$tmp.dir, "data.gms"), "w")
  if (grepl("gdx", scen@settings@solver$export_format, ignore.case = TRUE)) {
    if (isTRUE(scen@status$sparse)) {
      # Should not happen: the sparse scenario is densified at the top of
      # .write_model_GAMS. Defensive guard against a future code path.
      stop('for export_format = "gdx", the scenario must be dense ',
           '(rebuild with interp_mod(..., sparse = FALSE))')
    }
    # Generate gdx
    # browser()
    .write_gdx_list(
      dat = .get_scen_data(scen),
      gdxName = fp(arg$tmp.dir, "input/data.gdx")
    )

    # Add gdx import
    cat("$gdxin input/data.gdx\n", file = zz_data_gms)
    for (j in c("set", "map", "numpar", "bounds")) {
      for (i in names(scen@modInp@parameters)) {
        if (scen@modInp@parameters[[i]]@type == j &&
          (is.null(scen@modInp@parameters[[i]]@misc$weather) ||
           !scen@modInp@parameters[[i]]@misc$weather)) {
          if (scen@modInp@parameters[[i]]@type != "bounds") {
            cat(paste0("$loadm ", i, "\n"), file = zz_data_gms)
          } else {
            cat(paste0("$loadm ", i, "Lo\n"), file = zz_data_gms)
            cat(paste0("$loadm ", i, "Up\n"), file = zz_data_gms)
          }
        }
      }
    }
    cat("$gdxin\n", file = zz_data_gms)
  } else if (arg$n.threads == 1) {
    for (j in c("set", "map", "numpar", "bounds")) {
      for (i in names(scen@modInp@parameters)) {
        if (scen@modInp@parameters[[i]]@type == j) {
          zz_data_tmp <- file(fp(arg$tmp.dir,
                                        paste0("input/", i, ".gms")), "w")
          cat(.toGams(scen@modInp@parameters[[i]]), sep = "\n",
              file = zz_data_tmp)
          close(zz_data_tmp)
          cat(paste0("$include input/", i, ".gms\n"), file = zz_data_gms)
        }
      }
    }
  } else {
    # for (j in c("set", "map", "numpar", "bounds")) {
    #   for (i in names(scen@modInp@parameters)) {
    #     if (scen@modInp@parameters[[i]]@type == j) {
    #       cat(paste0("$include input/", i, ".gms\n"), file = zz_data_gms)
    #     }
    #   }
    # }
    # .write_multi_threads(arg, scen, func = .toGams, type = "gms")
  }
  close(zz_data_gms)
  ### Model code to text
  .write_gams_project_file(arg$tmp.dir)
  fn <- file(fp(arg$tmp.dir, "energyRt.gms"), "w")
  zz_constrains <- file(fp(arg$tmp.dir, "inc_constraints.gms"), "w")
  cat(run_code[1:grep("[$]include[[:space:]]*data.gms", run_code)], sep = "\n",
      file = fn)
  # Add parameter constraint declaration
  if (length(scen@modInp@gams.equation) > 0) {
    mps_name <- grep("^[m]Cns", names(scen@modInp@parameters), value = TRUE)
    mps_name_def <- c("set ", paste0(mps_name, "(", sapply(
      scen@modInp@parameters[mps_name],
      function(x) paste0(x@dimSets, collapse = ", ")
    ), ")"), ";")
    pps_name <- grep("^[p]Cns", names(scen@modInp@parameters), value = TRUE)
    pps_name_def <- c("parameter ", paste0(pps_name, "(", sapply(
      scen@modInp@parameters[pps_name],
      function(x) paste0(x@dimSets, collapse = ", ")
    ), ")"), ";")
    pps_name_def <- gsub("[(][)]", "", pps_name_def)
    if (length(mps_name) != 0) {
      cat(mps_name_def, sep = "\n", file = zz_constrains)
      cat("\n", sep = "\n", file = zz_constrains)
    }
    if (length(pps_name) != 0) {
      cat(pps_name_def, sep = "\n", file = zz_constrains)
      cat("\n", sep = "\n", file = zz_constrains)
    }
  }

  # Add parameter costs declaration
  {
    mps_name <- grep("^[m]Costs", names(scen@modInp@parameters), value = TRUE)
    mps_name_def <- c("set ", paste0(mps_name, "(", sapply(
      scen@modInp@parameters[mps_name],
      function(x) paste0(x@dimSets, collapse = ", ")
    ), ")"), ";")
    pps_name <- grep("^[p]Costs", names(scen@modInp@parameters), value = TRUE)
    pps_name_def <- c("parameter ", paste0(pps_name, "(", sapply(
      scen@modInp@parameters[pps_name],
      function(x) paste0(x@dimSets, collapse = ", ")
    ), ")"), ";")
    pps_name_def <- gsub("[(][)]", "", pps_name_def)
    if (length(mps_name) != 0) {
      cat(mps_name_def, sep = "\n", file = zz_constrains)
      cat("\n", sep = "\n", file = zz_constrains)
    }
    if (length(pps_name) != 0) {
      cat(pps_name_def, sep = "\n", file = zz_constrains)
      cat("\n", sep = "\n", file = zz_constrains)
    }
  }

  # Add parameter costs declaration
  {
    zz_costs <- file(fp(arg$tmp.dir, "inc_costs.gms"), "w")
    mps_name <- grep("^[m]Costs", names(scen@modInp@parameters), value = TRUE)
    mps_name_def <- c("set ", paste0(mps_name, "(", sapply(
      scen@modInp@parameters[mps_name],
      function(x) paste0(x@dimSets, collapse = ", ")
    ), ")"), ";")
    pps_name <- grep("^[p]Costs", names(scen@modInp@parameters), value = TRUE)
    pps_name_def <- c("parameter ", paste0(pps_name, "(", sapply(
      scen@modInp@parameters[pps_name],
      function(x) paste0(x@dimSets, collapse = ", ")
    ), ")"), ";")
    pps_name_def <- gsub("[(][)]", "", pps_name_def)
    if (length(mps_name) != 0) {
      cat(mps_name_def, sep = "\n", file = zz_costs)
      cat("\n", sep = "\n", file = zz_costs)
    }
    if (length(pps_name) != 0) {
      cat(pps_name_def, sep = "\n", file = zz_costs)
      cat("\n", sep = "\n", file = zz_costs)
    }
    cat(c(
      "Equation\neqTotalUserCosts(region, year)\n;\n",
      scen@modInp@costs.equation
    ), file = zz_costs)
  }

  # Add constraint equation
  if (length(scen@modInp@gams.equation) > 0) {
    # Declaration
    cat("equation", sapply(
      scen@modInp@gams.equation,
      function(x) x$equationDeclaration
    ),
    ";", "",
    sep = "\n", file = zz_constrains
    )
    # Body equation
    cat(sapply(scen@modInp@gams.equation, function(x) x$equation), "",
      sep = "\n", file = zz_constrains
    )
  }
  if (!is.null(scen@model@misc$additionalEquationGAMS)) {
    cat(scen@model@misc$additionalEquationGAMS$code,
      sep = "\n",
      file = zz_constrains
    )
  }
  cat(run_code[
    (grep("[$]include[[:space:]]*data.gms", run_code) + 1):length(run_code)
  ], sep = "\n", file = fn)

  # Add constraint equation to model declaration
  if (!is.null(scen@model@misc$additionalEquationGAMS)) {
    cat(scen@model@misc$additionalEquationGAMS$code,
      sep = "\n",
      file = zz_constrains
    )
  }
  close(fn)
  close(zz_constrains)
  close(zz_costs)
  .write_inc_files(arg, scen, ".gms")
  if (is.null(scen@settings@solver$cmdline) || scen@settings@solver$cmdline == "") {
    fpath <- get_gams_path()
    if (is.null(fpath)) {
      scen@settings@solver$cmdline <- "gams energyRt.gms"
    } else {
      scen@settings@solver$cmdline <-
        fp(fpath, "gams energyRt.gms") |>
        str_replace_all("//", "/")
    }
  }
  scen@settings@solver$code <- c(
    "energyRt.gms", "output.gms", "inc_constraints.gms",
    "inc_solver.gms"
  )
  scen
}

.check_miss_rem_col <- function(x, nn) {
  kll <- rev(grep("[[:alnum:]]$", x))
  kll <- kll[kll != length(x)]
  if (length(kll) != 0) {
    for (i in kll) {
      x[i] <- paste0(x[i], nn, x[i + 1])
    }
    x <- x[-(kll + 1)]
  }
  x
}

.rem_col <- function(x, nn, rmm) {
  x <- .check_miss_rem_col(x, nn)
  for (i in 2:length(x)) {
    tt <- gsub("(^.|[)].*)", "", x[i])
    til <- substr(x[i], nchar(tt) + 3, nchar(x[i]))
    mm <- strsplit(tt, "[,]")[[1]][-rmm]
    if (length(mm) == 0) {
      x[i] <- paste0(nn, til)
    } else {
      x[i] <- paste0(nn, "(", paste0(mm, collapse = ", "), ")", til)
    }
  }
  return(paste0(x, collapse = ""))
}

.rem_col_sq <- function(x, nn, rmm) {
  x <- .check_miss_rem_col(x, nn)
  for (i in 2:length(x)) {
    tt <- gsub("(^.|[]].*)", "", x[i])
    til <- substr(x[i], nchar(tt) + 3, nchar(x[i]))
    mm <- strsplit(tt, "[,]")[[1]][-rmm]
    if (length(mm) == 0) {
      x[i] <- paste0(nn, til)
    } else {
      x[i] <- paste0(nn, "[", paste0(mm, collapse = ", "), "]", til)
    }
  }
  return(paste0(x, collapse = ""))
}

.rem_col_fg <- function(x, nn, rmm) {
  x <- .check_miss_rem_col(x, nn)
  for (i in 2:length(x)) {
    tt <- gsub("(^.|[}].*)", "", x[i])
    til <- substr(x[i], nchar(tt) + 3, nchar(x[i]))
    mm <- strsplit(tt, "[,]")[[1]][-rmm]
    if (length(mm) == 0) {
      x[i] <- paste0(nn, til)
    } else {
      x[i] <- paste0(nn, "{", paste0(mm, collapse = ", "), "}", til)
    }
  }
  return(paste0(x, collapse = ""))
}

# Generate GAMS code, return character string with the GAMS code
.toGams0 <- function(obj, include.def) {
  gen_gg <- function(name, dtt) {
    # browser()
    if (ncol(dtt) == 1) {
      ret <- paste0(name, " = ", dtt[[1]][1], ";")
    } else {
      ret <- paste0(name, '("', dtt[[1]])
      for (i in seq_len(ncol(dtt) - 2) + 1) {
        ret <- paste0(ret, '", "', dtt[[i]])
      }
      # browser()
      # paste0(ret, '") = ', dtt[, ncol(dtt)], ";")
      # paste0(ret, '") = ', select(dtt, last_col()), ";")
      paste0(ret, '") = ', dtt[[ncol(dtt)]], ";")
    }
  }
  as_numpar <- function(dtt, name, def, include.def) {
    if (include.def) {
      add_cnd <- function(y, x) {
        if (x == "") {
          return(x)
        } else {
          return(paste(x, "and", y))
        }
      }
      add_cond2 <- ""
      if (any(obj@dimSets == "tech") && any(obj@dimSets == "comm")) {
        add_cond2 <- "(mTechInpComm(tech, comm) or mTechOutComm(tech, comm) or mTechAInp(tech, comm) or mTechAOut(tech, comm))"
        if (any(obj@dimSets == "group")) add_cond2 <- paste("not(mTechOneComm(tech, comm)) and  ", add_cond2, sep = "")
      }
      if (any(obj@dimSets == "tech") && any(obj@dimSets == "slice")) {
        add_cond2 <- add_cnd("mTechSlice(tech, slice)", add_cond2)
      }
      if (any(obj@dimSets == "tech") && any(obj@dimSets == "acomm")) {
        add_cond2 <- add_cnd("(mTechAInp(tech, acomm) or mTechAOut(tech, acomm))", add_cond2)
      }
      if (any(obj@dimSets == "year")) {
        add_cond2 <- add_cnd("mMidMilestone(year)", add_cond2)
      }
      if (name == "pTradeIrEff") {
        add_cond2 <- "(sum(comm$(mTradeComm(trade, comm) and mvTradeIr(trade, comm, src, dst, year, slice)), 1))"
      }
      if (name == "pTechGinp2use") {
        add_cond2 <- "(sum(commp$meqTechGrp2Sng(tech, region, group, commp, year, slice), 1) + (sum(groupp$meqTechGrp2Grp(tech, region, group, groupp, year, slice), 1) <> 0))"
      }
      if (name == "pTechAfUp") {
        add_cond2 <- "meqTechAfUp(tech, region, year, slice)"
      }

      if (add_cond2 != "") add_cond2 <- paste("(", add_cond2, ")", sep = "")
      if (nrow(dtt) == 0 || all(dtt$value == def)) { #
        return(paste(name, "(", paste(obj@dimSets, collapse = ", "),
          ")", "$"[add_cond2 != ""], add_cond2, " = ", def,
          ";",
          sep = ""
        ))
      } else {
        if (def != 0 && def != Inf) {
          fn <- paste0(
            name, "(", paste0(obj@dimSets, collapse = ", "),
            ")", "$"[add_cond2 != ""], add_cond2, " = ", def, ";"
          )
        } else {
          fn <- ""
        }
        return(c(fn, gen_gg(name, dtt[dtt$value != def, , drop = FALSE]))) #
      }
    }
    # print(dtt)
    if (nrow(dtt) == 0 || all(dtt$value %in% c(0, Inf))) { #
      if (ncol(dtt) > 1) {
        # browser()
        return(paste0(
          name, "(",
          paste0(colnames(dtt)[-ncol(dtt)], collapse = ", "),
          ")$0 = 0;"
        ))
      }
      return(paste0(name, "$0 = 0;"))
    } else {
      return(c(gen_gg(name, dtt[dtt$value != 0 & dtt$value != Inf, , drop = FALSE]))) #
    }
  }
  if (obj@misc$nValues != -1) {
    obj@data <- obj@data[seq(length.out = obj@misc$nValues), , drop = FALSE]
  }
  if (obj@type == "set") {
    if (nrow(obj@data) == 0) {
      return(paste0("set\n", obj@name, " / 1 /;\n"))
    } else {
      return(c(
        "set", paste(obj@name, " /", sep = ""),
        sort(obj@data[[1]]), "/;", ""
      ))
    }
  } else if (obj@type == "map") {
    if (nrow(obj@data) == 0) {
      return(paste0(
        obj@name, "(",
        paste0(obj@dimSets, collapse = ", "), ")$0 = NO;"
      ))
    } else {
      ret <- c("set", paste(obj@name, "(",
        paste(obj@dimSets, collapse = ", "),
        ") /",
        sep = ""
      ))
      return(c(
        ret, apply(obj@data, 1, function(x) paste(x, collapse = ".")),
        "/;", ""
      ))
    }
  } else if (obj@type == "numpar") {
    return(as_numpar(obj@data, obj@name, obj@defVal, include.def))
  } else if (obj@type == "bounds") {
    # cat(obj@name, "\n")
    # browser()
    return(c(
      as_numpar(
        # obj@data[obj@data$type == "lo", 1 - ncol(obj@data), drop = FALSE],
        select(filter(obj@data, type == "lo"), -type),
        paste0(obj@name, "Lo"), obj@defVal[1], include.def
      ),
      as_numpar(
        # obj@data[obj@data$type == "up", 1 - ncol(obj@data), drop = FALSE],
        select(filter(obj@data, type == "up"), -type),
        paste0(obj@name, "Up"), obj@defVal[2], include.def
      )
    ))
  } else {
    stop(paste0("Error: .toGams: unknown parameter type: ", obj@type, " / ", obj@name))
  }
  ret
}


# GDX exchange ####
.get_scen_data <- function(scen) {
  # browser()
  all_factor <- function(x) {
    for (i in colnames(x)[colnames(x) != "value"]) {
      x[[i]] <- factor(x[[i]])
    }
    x
  }
  gx <- list()
  for (i in names(scen@modInp@parameters)) {
    if (scen@modInp@parameters[[i]]@type != "bounds") {
      gx[[i]] <- all_factor(.get_data_slot(scen@modInp@parameters[[i]]))
    } else {
      prm <- .get_data_slot(scen@modInp@parameters[[i]])
      gx[[paste0(i, "Lo")]] <- all_factor(
        # prm[prm$type == "lo", colnames(prm) != "type", drop = FALSE]
        select(filter(prm, type == "lo"), -type)
      )
      gx[[paste0(i, "Up")]] <- all_factor(
        # prm[prm$type == "up", colnames(prm) != "type", drop = FALSE]
        select(filter(prm, type == "up"), -type)
      )
    }
    gx
  }
  return(gx)
}

.df2uels <- function(df, name = "x", value = "value") {
  # The function takes data.frame or character vector and returns
  # named list for exporting to GDX-file using gdxrrw
  if (!is.data.frame(df)) {
    df <- data.frame(df)
    colnames(df) <- name
  }
  domains <- names(df)
  v <- domains != value
  nr <- nrow(df)
  nc <- sum(v)
  if (all(v)) {
    type <- "set"
  } else {
    type <- "parameter"
  }
  df2val <- function(dd) {
    if (nrow(dd) > 0) {
      for (j in domains[v]) {
        # dd[, j] <- as.numeric(dd[, j])
        dd[[j]] <- as.numeric(dd[[j]])
      }
      dd <- as.matrix(dd)
    } else {
      rr <- nr
      if (type == "set") rr <- 1
      dd <- matrix(1L, nrow = rr, ncol = length(v))
    }
    dd
  }
  if (nr > 0) {
    for (j in domains[v]) {
      # browser()
      # df[, j] <- factor(df[, j]) # add levels from sets!
      df[[j]] <- factor(df[[j]]) # add levels from sets!
    }
    uels <- list(
      name = name,
      type = type,
      dim = nc,
      domains = domains[v],
      # uels = lapply(domains[v], function(x) levels(df[, x])),
      uels = lapply(domains[v], function(x) levels(df[[x]])),
      val = df2val(df),
      form = "sparse"
    )
  } else {
    uels <- list(
      name = name,
      type = type,
      dim = nc,
      domains = domains[v],
      uels = lapply(domains[v], function(x) "1"),
      val = df2val(df), # matrix(nrow = 0, ncol = nc),
      form = "sparse"
    )
  }
  return(uels)
}

.write_gdx_list <- function(dat, gdxName = "data.gdx") {
  # the function exports named list of sets and parameters to GDX file
  # stopifnot("gdxrrw" %in% rownames(installed.packages()))
  .check_load_gdxtools()
  # .check_load_gdxlib()
  # rw <- require("gdxrrw")
  # if (!rw) {
  #   stop('"gdxrrw" package has not been found. ',
  #        'It is required for writing and reading "*.gdx" files.',
  #        '"https://github.com/GAMS-dev/gdxrrw"')
  # }
  # en_gdxlib_loaded <- getOption("en_gdxlib_loaded")
  # if (is.null(en_gdxlib_loaded) || as.logical(en_gdxlib_loaded) == FALSE) {
  #   lb <- options::opt("gdxlib_path")
  #   if (is.null(lb)) {
  #     lb <- options::opt("gams_path")
  #   }
  #   ix <- igdx(lb)
  #   if (!ix) {
  #     stop('Cannot load "gdx" library. Check "?set_gdxlib_path" to setup.')
  #   } else {
  #     options(en_gdxlib_loaded = TRUE)
  #   }
  # }
  # browser()
  cat(" data.gdx ")
  nms <- names(dat)
  max_length <- max(nchar(nms))
  x <- list()
  wipe <- ""
  for (i in nms) {
    cat(wipe, "(", i, ")", rep(" ", max_length - nchar(i) + 1), sep = "")
    wipe <- paste0(rep("\b", max_length + 3), collapse = "")
    x <- c(x, list(.df2uels(data.frame(dat[[i]]), i)))
  }
  # gdxrrw::wgdx(gdxName = gdxName, x, squeeze = FALSE)
  # browser()
  # !!!ToDo: add check for NAs
  gdxtools::wgdx(gdxName = gdxName, x, squeeze = FALSE)
  cat(wipe, sep = "")
  cat(rep(" ", max_length + 3), sep = "")
  cat(rep(" ", max_length + 3), sep = "")
  cat(wipe, wipe, "\b, ", format(object.size(file.size(gdxName)), "auto"),
      ", ", sep = "")
}

.write_sqlite_list <- function(dat, sqlFile = "data.db") {
  cat(basename(sqlFile), " ", sep = "")
  tStart <- Sys.time()
  if (file.exists(sqlFile)) file.remove(sqlFile)
  con <- DBI::dbConnect(RSQLite::SQLite(), sqlFile)
  # DBI::dbListTables(con)
  nms <- names(dat)
  max_length <- max(nchar(nms))
  wipe <- ""
  for (i in nms) {
    cat(wipe, "(", i, ")", rep(" ", max_length - nchar(i) + 1), sep = "")
    wipe <- paste0(rep("\b", max_length + 3), collapse = "")
    # cat(wipe, i, rep(" ", 10), sep = "")
    # wipe <- paste0(rep("\b", nchar(i) + 10), collapse = "")
    DBI::dbWriteTable(con, i, dat[[i]], overwrite = TRUE)
  }
  DBI::dbDisconnect(con)
  # finf <- file.desc(sqlFile)
  # format(finf["size"])
  cat(wipe, sep = "")
  cat(rep(" ", max_length + 3), sep = "")
  cat(rep(" ", max_length + 3), sep = "")
  cat(wipe, wipe, "\b, ", format(object.size(file.size(sqlFile)), "auto"),
      ", ", sep = "")
  # cat(format(round(Sys.time() - tStart), 1))
}

.write_gams_project_file <- function(tmp.dir) {
  # Generates GAMS-project file
  fn <- file(paste(tmp.dir, "/energyRt_project.gpr", sep = ""), "w")
  cat(c(
    "[RP:MDL]", "1=", "", "[OPENWINDOW_1]",
    "FILE0=energyRt.gms",
    "FILE1=energyRt.lst",
    "FILE2=input/data.gdx",
    "FILE2=output/output.gdx",
    # gsub('[/][/]*', '\\\\', paste('FILE0=', tmp.dir, '/energyRt.gms', sep = '')),
    # gsub('[/][/]*', '\\\\', paste('FILE1=', tmp.dir, '/energyRt.lst', sep = '')),
    "", "MAXIM=1",
    "TOP=50", "LEFT=50", "HEIGHT=400", "WIDTH=400", ""
  ), sep = "\n", file = fn)
  close(fn)
}


# end ####
