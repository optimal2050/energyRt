
#' @title Set the path to the GLPK library
#'
#' @param path character. Path to the GLPK library with `glpsol.*` executable.
#'
#' @return sets the path to the GLPK library in R options and returns NULL.
#'
#' @details By default energyRt auto-detects the `glpsol` executable on the
#'   session `PATH` (via [base::Sys.which()]). On Windows this picks up the copy
#'   **bundled with Rtools** (Rtools ships the GLPK dev kit), so an Rtools user
#'   needs no separate GLPK install. Use `set_glpk_path()` only to point at a
#'   standalone GLPK installation, which then takes precedence over auto-detection.
#'
#' @rdname solver
#' @family solver glpk
#'
#' @export
#'
#' @examples
#' \dontrun{
#' set_glpk_path("/usr/local/bin/glpk") # Linux & Mac
#' set_glpk_path("C:/Program Files/glpk/bin") # Windows
#' get_glpk_path()
#' }
set_glpk_path <- function(path = NULL) {
  # browser()
  if (!is.null(path) && path != "") {
    if (!dir.exists(path)) {
      stop(paste0('The path "', path, '" does not exist.'), call. = FALSE)
    }
    if (!grepl("\\/$", path)) {
      path <- paste0(path, "/")
    }
  }
  options::opt_set("glpk_path", path, env = "energyRt")
}

#' @export
#' @rdname solver
#' @family solver glpk
#' @return returns the path to the GLPK library.
get_glpk_path <- function() {
  options::opt("glpk_path")
}

# Resolve the `glpsol` executable for the built-in GLPK backend.
#
# Priority:
#   1. a directory configured via `set_glpk_path()` (standalone GLPK install);
#   2. `glpsol` on the session PATH via `Sys.which()` - on Windows this picks up
#      the copy BUNDLED WITH RTOOLS (rtools bundles the full GLPK dev kit), so an
#      Rtools user needs no separate GLPK install;
#   3. known Rtools toolchain locations (in case Rtools is installed but not on
#      the session PATH);
#   4. a bare `"glpsol"` resolved by the OS at run time (last resort).
#
# Returns the executable path (or bare command) as a length-1 character.
.find_glpsol <- function() {
  gp <- get_glpk_path()
  if (!is.null(gp) && nzchar(gp)) return(file.path(gp, "glpsol"))

  found <- Sys.which("glpsol")
  if (nzchar(found)) return(unname(found))

  if (.Platform$OS.type == "windows") {
    cand <- Sys.glob(c(
      file.path(Sys.getenv("RTOOLS45_HOME"), "*", "bin", "glpsol.exe"),
      file.path(Sys.getenv("RTOOLS44_HOME"), "*", "bin", "glpsol.exe"),
      "C:/rtools4*/x86_64-w64-mingw32.static.posix/bin/glpsol.exe"))
    cand <- cand[file.exists(cand)]
    if (length(cand)) return(normalizePath(cand[1], winslash = "/"))
  }
  "glpsol"
}

# Build a `glpsol` command line, quoting the executable if its path has spaces.
.glpsol_cmdline <- function(exe = .find_glpsol(),
                            args = "-m energyRt.mod -d energyRt.dat") {
  if (grepl(" ", exe, fixed = TRUE)) exe <- shQuote(exe)
  paste(exe, args)
}

# MathProg GLPK (& MathProg with CBC) ####
# `sm_fun` selects the parameter -> GMPL data converter (`.sm_to_glpk`, which
# writes each parameter's full `@data` slot).
.write_model_GLPK_CBC <- function(arg, scen, sm_fun = .sm_to_glpk) {
  run_code <- scen@settings@sourceCode[["GLPK"]]
  dir.create(paste(arg$tmp.dir, "/output", sep = ""), showWarnings = FALSE)
  file_w <- c()
  for (j in c("set", "map", "numpar", "bounds")) {
    for (i in names(scen@modInp@parameters)) {
      if (scen@modInp@parameters[[i]]@type == j) {
        file_w <- c(file_w, sm_fun(scen@modInp@parameters[[i]]))
      }
    }
  }
  # For downsize
  fdownsize <- names(scen@modInp@parameters)[
    sapply(scen@modInp@parameters, function(x) length(x@misc$rem_col) != 0)]
  for (nn in fdownsize) {
    rmm <- scen@modInp@parameters[[nn]]@misc$rem_col
    if (scen@modInp@parameters[[nn]]@type == "bounds") {
      uuu <- paste0(nn, c("Lo", "Up"))
    } else {
      uuu <- nn
    }
    for (yy in uuu) {
      templ <- paste0("(^|[^[:alnum:]])", yy, "[[]")
      templ2 <- paste0("(^|[^[:alnum:]])", yy, "[{]")
      if (any(grep("^pCns", nn))) {
        for (www in seq_along(scen@modInp@gams.equation)) {
          mmm <- grep(templ, scen@modInp@gams.equation[[www]]$equation)
          if (any(mmm)) {
            scen@modInp@gams.equation[[www]]$equation[mmm] <-
              sapply(strsplit(scen@modInp@gams.equation[[www]]$equation[mmm], yy),
                     .rem_col_sq, yy, rmm)
          }
        }
      } else {
        mmm <- grep(templ, run_code)
        if (any(mmm)) run_code[mmm] <- sapply(strsplit(run_code[mmm], yy),
                                              .rem_col_sq, yy, rmm)
        mmm <- grep(templ2, run_code)
        if (any(mmm)) run_code[mmm] <- sapply(strsplit(run_code[mmm], yy),
                                              .rem_col_fg, yy, rmm)
      }
    }
  }

  # Add constraint
  if (length(scen@modInp@gams.equation) > 0) {
    add_eq <- sapply(scen@modInp@gams.equation,
                     function(x) .equation.from.gams.to.glpk(x$equation))
    # Add additional maps
    mps_name <- grep("^[m]Cns", names(scen@modInp@parameters), value = TRUE)
    mps_name_def <- paste0("set ", mps_name, " dimen ",
                           sapply(scen@modInp@parameters[mps_name],
                                  function(x) length(x@dimSets)), ";")
    pps_name <- grep("^[p]Cns", names(scen@modInp@parameters), value = TRUE)
    pps_name_def <- paste0("param ", pps_name, " {",
                           sapply(scen@modInp@parameters[pps_name],
                                  function(x) paste0(x@dimSets, collapse = ", ")),
                           "};")
    if (length(pps_name) == 0) pps_name_def <- character()
  }


  ### Costs
  {
    # browser()
    add_eq_costs <- .equation.from.gams.to.glpk(scen@modInp@costs.equation)
    # Add additional maps
    mps_name_costs <- grep("^[m]Costs", names(scen@modInp@parameters),
                           value = TRUE)
    mps_name_def_costs <- paste0("set ", mps_name_costs, " dimen ",
                                 sapply(scen@modInp@parameters[mps_name_costs],
                                        function(x) length(x@dimSets)), ";")
    pps_name_costs <- grep("^[p]Costs", names(scen@modInp@parameters),
                           value = TRUE)
    pps_name_def_costs <- paste0("param ", pps_name_costs, " {",
                                 sapply(scen@modInp@parameters[pps_name_costs],
                                        function(x)
                                          paste0(x@dimSets, collapse = ", ")),
                                 "};")
    pps_name_def_costs <- gsub("[{][ ]*[}]", "", pps_name_def_costs)
    if (length(mps_name_def_costs) == 0) mps_name_def_costs <- character()
    if (length(pps_name_costs) == 0) pps_name_def_costs <- character()
  }

  ### FUNC GLPK
  fn <- file(paste(arg$tmp.dir, "/energyRt.mod", sep = ""), "w")
  if (length(grep("^minimize", run_code)) != 1) stop("Errors in GLPK model")

  cat(run_code[1:(grep("22b584bd-a17a-4fa0-9cd9-f603ab684e47", run_code) - 1)],
      sep = "\n", file = fn)
  if (length(scen@modInp@gams.equation) > 0) {
    cat(mps_name_def, sep = "\n", file = fn)
    cat(pps_name_def, sep = "\n", file = fn)
    cat(add_eq, sep = "\n", file = fn)
  }
  ### Costs
  cat(mps_name_def_costs, sep = "\n", file = fn)
  cat(pps_name_def_costs, sep = "\n", file = fn)
  cat(add_eq_costs, sep = "\n", file = fn)


  cat(run_code[grep(
    "22b584bd-a17a-4fa0-9cd9-f603ab684e47", run_code):(
      grep("^minimize", run_code) - 1)], sep = "\n", file = fn)
  cat(run_code[grep("^minimize", run_code):(grep("^end[;]", run_code) - 1)],
    sep = "\n", file = fn
  )
  cat(run_code[grep("^end[;]", run_code):length(run_code)], sep = "\n",
      file = fn)
  close(fn)
  fn <- file(paste(arg$tmp.dir, "/energyRt.dat", sep = ""), "w")
  cat("set FORIF := FORIFSET;\n", sep = "\n", file = fn)
  cat(file_w, sep = "\n", file = fn)
  cat("end;", "", sep = "\n", file = fn)
  close(fn)
  .write_inc_files(arg, scen, NULL)

  if (is.null(scen@settings@solver$cmdline) || scen@settings@solver$cmdline == "") {
    if (toupper(scen@settings@solver$lang) == "GLPK") {
      # Default to an auto-detected glpsol (bundled with Rtools on Windows);
      # `set_glpk_path()` overrides to a standalone GLPK install. See .find_glpsol().
      scen@settings@solver$cmdline <- .glpsol_cmdline()
    } else {
      scen@settings@solver$cmdline <- "cbc energyRt.mod%energyRt.dat -solve"
    }
  }
  scen@settings@solver$code <- c("energyRt.mod")
  scen
}


# Convert one scenario parameter to GMPL (GLPK/CBC) text. The interp_mod()
# pipeline keeps each parameter's `@data` slot authoritative, so the whole slot
# is written (no legacy `@misc$nValues` trimming).
.sm_to_glpk <- function(obj) {
  if (obj@type == "set") {
    if (nrow(obj@data) == 0) {
      ret <- c(paste("set ", obj@name, " := ;", sep = ""), "")
    } else {
      ret <- c(paste("set ", obj@name, " := ",
                     paste(obj@data[[1]], collapse = " "), ";", sep = ""), "")
    }
  } else if (obj@type == "map") {
    if (nrow(obj@data) == 0) {
      ret <- paste("set ", obj@name, " := ;", sep = "")
    } else {
      ret <- paste("set ", obj@name, " := ", sep = "")
      ret <- c(ret, apply(obj@data, 1, function(x) {
        paste(paste(x, collapse = " "), ", ", sep = "")
        }))
      ret <- c(ret, ";", "")
    }
  } else if (obj@type == "numpar") {
    if (nrow(obj@data) == 0) {
      dd <- obj@defVal
      if (dd == Inf) dd <- 0
      ret <- paste("param ", obj@name, " default ", dd, " := ;", sep = "")
    } else {
      dd <- obj@defVal
      if (dd == Inf) dd <- 0
      ret <- paste("param ", obj@name, " default ", dd, " := ", sep = "")
      fl <- obj@data[["value"]] != Inf
      if (any(fl)) {
        ret <- c(
          ret,
          paste("[", apply(
            # obj@data[fl, -ncol(obj@data), drop = FALSE],
            select(filter(obj@data, fl), -ncol(obj@data)),
            1, function(x) paste(x, collapse = ",")
            ), "] ", obj@data[["value"]][fl], sep = "")
          )
      }
      if (ncol(obj@data) == 1) ret <- gsub("[[][ ]*[]]", "", ret)
      ret <- c(ret, ";", "")
    }
  } else if (obj@type == "bounds") {
    gg <- obj@data
    gg <- gg[gg$type == "lo", , drop = FALSE]
    # gg <- gg[, colnames(gg) != "type"]
    gg <- gg |> select(-any_of("type"))
    if (nrow(gg) == 0) { #  || all(gg$value[1] == gg$value)
      if (nrow(gg) == 0) dd <- obj@defVal[1] else dd <- gg$value[1]
      if (dd == Inf) dd <- 0
      ret <- paste("param ", obj@name, "Lo default ", dd, " := ;", sep = "")
    } else {
      dd <- obj@defVal[1]
      if (dd == Inf) dd <- 0
      ret <- paste("param ", obj@name, "Lo default ", dd, " := ", sep = "")
      fl <- gg[["value"]] != Inf
      if (any(fl)) {
        ret <- c(
          ret, paste("[", apply(
            # gg[fl, -ncol(gg), drop = FALSE],
            select(filter(gg, fl), -last_col()),
            1, function(x) paste(x, collapse = ",")
            ), "] ", gg[["value"]][fl], sep = ""))
      }
      if (ncol(gg) == 1) ret <- gsub("[[][ ]*[]]", "", ret)
      ret <- c(ret, ";", "")
    }
    gg <- obj@data
    gg <- gg[gg$type == "up", , drop = FALSE]
    # gg <- gg[, colnames(gg) != "type"]
    gg <- gg |> select(-any_of("type"))
    if (nrow(gg) == 0) { #  || all(gg$value[1] == gg$value)
      if (nrow(gg) == 0) dd <- obj@defVal[2] else dd <- gg$value[1]
      if (dd == Inf) dd <- 0
      ret <- c(ret, paste("param ", obj@name, "Up default ", dd, " := ;", sep = ""))
    } else {
      dd <- obj@defVal[2]
      if (dd == Inf) dd <- 0
      ret <- c(ret, paste("param ", obj@name, "Up default ", dd, " := ", sep = ""))
      fl <- gg[["value"]] != Inf
      if (any(fl)) {
        ret <- c(
          ret, paste("[", apply(
            # gg[fl, -ncol(gg), drop = FALSE],
            select(filter(gg, fl), -ncol(gg)),
            1, function(x) paste(x, collapse = ",")
            ), "] ", gg[["value"]][fl], sep = "")
          )
      }
      if (ncol(gg) == 1) ret <- gsub("[[][ ]*[]]", "", ret)
      ret <- c(ret, ";", "")
    }
  } else {
    stop("Must realise")
  }
  ret
}

# Translate GAMS constraints to GLPK functions ####
# Make vector .alias_set (from gams to alias) and set_alias

# set_alias <- .set_al0
# names(set_alias) <- .alias_set

.get_glpk_loop_fast <- function(set_loop, set_cond, add_cond = NULL) {
  if (!is.null(set_cond) && substr(set_cond, 1, 1) == "(") {
    set_cond <- sub("^[(]", "", sub("[)]$", "", set_cond))
  }
  set_loop <- sub("^[(]", "", sub("[)]$", "", set_loop))
  xx <- .generate_loop_glpk(set_loop, set_cond)
  rs <- paste0("{", xx$first)
  if (!is.null(xx$end) || !is.null(add_cond)) {
    rs <- paste0(rs, " : ", paste0(xx$end, add_cond, collapse = " and "))
  }
  rs <- paste0(rs, "}")
  rs
}

# .set_al <- c("acomm", "stg", "trade", "expp", "imp", "tech", "dem", "sup", "weather", "region", "year", "slice", "group", "comm", "cns", "stgp", "tradep", "exppp", "impp", "techp", "demp", "supp", "weatherp", "regionp", "yearp", "slicep", "groupp", "commp", "cnsp", "stge", "tradee", "exppe", "impe", "teche", "deme", "supe", "weathere", "regione", "yeare", "slicee", "groupe", "comme", "cnse", "stgn", "traden", "exppn", "impn", "techn", "demn", "supn", "weathern", "regionn", "yearn", "slicen", "groupn", "commn", "cnsn", "src", "dst")
# .alias_set <- c("ca", "st1", "t1", "e", "i", "t", "d", "s1", "wth1", "r", "y", "s", "g", "c", "cn1", "st1p", "t1p", "ep", "ip", "tp", "dp", "s1p", "wth1p", "rp", "yp", "sp", "gp", "cp", "cn1p", "st1e", "t1e", "ee", "ie", "te", "de", "s1e", "wth1e", "re", "ye", "se", "ge", "ce", "cn1e", "st1n", "t1n", "en", "in", "tn", "dn", "s1n", "wth1n", "rn", "yn", "sn", "gn", "cn", "cn1n", "src", "dst")
# names(.alias_set) <- .set_al
# .aliasName <- function(x) {
#   if (!all(x %in% .set_al)) {
#     cat("Unknown .set_al\n")
#     browser()
#     stop("Unknown set")
#   }
#   .alias_set[x]
# }

.generate_loop_glpk <- function(set_num, set_loop) {
  # Consdition split and divet by subset
  while (!is.null(set_loop) && substr(set_loop, 1, 1) == "(" && substr(set_loop, nchar(set_loop), nchar(set_loop)) == ")") {
    set_loop <- substr(set_loop, 2, nchar(set_loop) - 1)
  }
  while (!is.null(set_num) && substr(set_num, 1, 1) == "(" && substr(set_num, nchar(set_num), nchar(set_num)) == ")") {
    set_num <- substr(set_num, 2, nchar(set_num) - 1)
  }
  cnd <- gsub(" ", "", strsplit(set_loop, "and ")[[1]])
  cnd_slice <- strsplit(gsub("(.*[(]|[)]| )", "",
                             strsplit(set_loop, "and ")[[1]]), ",")
  cnd_slice <- lapply(cnd_slice, .aliasName)
  cnd0 <- gsub("[(].*", "", cnd)

  to_merge_slice <- sapply(cnd_slice, paste0, collapse = "#")
  rs <- NULL
  for (i in unique(to_merge_slice)) {
    fl <- (to_merge_slice == i)
    fl1 <- seq_along(fl)[fl][1]
    rs <- c(rs, paste0(
      "("[length(cnd_slice[[fl1]]) > 1],
      paste0(cnd_slice[[fl1]], collapse = ", "), ")"[
        length(cnd_slice[[fl1]]) > 1],
      " in ", "("[sum(fl) > 1],
      paste0(cnd0[fl], collapse = " inter "), ")"[sum(fl) > 1]
    ))
  }
  # Check if subset on set (glpk is not allowed situation like "(t, g, c) in mTechGroupComm (t, g) in mTechInpGroup", mTechInpGroup - not allowed)

  not_use <- rep(FALSE, length(rs))
  sss <- unique(to_merge_slice)
  for (tt in seq_along(sss)) {
    jj <- strsplit(sss[tt], "#")[[1]]
    if (any(sapply(cnd_slice,
                   function(x) all(jj %in% x) && length(x) > length(jj)))) {
      not_use[tt] <- TRUE
    }
  }

  in_slc <- .aliasName(gsub(" ", "", strsplit(set_num, ",")[[1]]))
  not_use[sapply(strsplit(sss, "#"),
                 function(x) length(x) == 1 && all(x != in_slc))] <- TRUE

  if (any(not_use)) {
    tend <- paste0(rs[not_use], collapse = " and ")
  } else {
    tend <- NULL
  }
  rs <- paste0(rs[!not_use], collapse = ", ")
  # Rest slice without mapping
  slice <- gsub(" ", "", strsplit(set_num, ",")[[1]])
  # slice2 <- .aliasName(slice)
  rest_slice <- slice[!(.aliasName(slice) %in% c(cnd_slice, recursive = TRUE))]
  if (length(rest_slice) > 0) {
    if (rs == "") rs <- NULL
    rs <- paste0(c(rs, paste0(.aliasName(rest_slice), " in ", rest_slice)), collapse = ", ")
  }
  if (any(grep("^[,]", rs))) {
    browser()
  }
  list(first = rs, end = tend)
}
.get_glpk_loop_fast2 <- function(tx) {
  if (any(grep("[$]", tx))) {
    beg <- gsub("[$].*", "", tx)
    end <- substr(tx, nchar(beg) + 2, nchar(tx))
  } else {
    beg <- tx
    end <- NULL
  }
  .get_glpk_loop_fast(beg, end)
}

.get.bracket.glpk <- function(tmp) {
  brk0 <- gsub("[^)(]", "", tmp)
  brk <- cumsum(c("(" = 1, ")" = -1)[strsplit(brk0, "")[[1]]])
  k <- seq_along(brk)[brk == 0][1]
  end <- sub(paste0("^", paste0(paste0("[", names(brk)[1:(k - 1)], "]"),
                                rep("[^)(]*", k - 1), collapse = ""),
                    names(brk)[k]), "", tmp)
  list(beg = substr(tmp, 1, nchar(tmp) - nchar(end)), end = end)
}


# "s.t. eqCnsMINGASgrow2{y in (mMidMilestone inter mMilestoneHasNext)}: sum{y in mMidMilestone, (c, s) in mCommSlice, r in region : c in (mCnsMINGASgrow2_1 inter mCnsMINGASgrow2_1)}(-1 * vOutTot[c, r, y, s]+ sum{(y, yp) in mMilestoneNext, (c, s) in mCommSlice, r in region : c in (mCnsMINGASgrow2_1 inter mCnsMINGASgrow2_1) and yp in mMidMilestone}(pCnsMultMINGASgrow2_2[y]* vOutTot[c, r, yp, s]>=0;"

# tmp = '((comm, region, slice)$(mCnsMINGASgrow2_1(comm) and mMidMilestone(year) and mCommSlice(comm, slice) and mCnsMINGASgrow2_1(comm)), -1 * vOutTot(comm, region, year, slice))'
.handle.sum.glpk <- function(tmp) {
  hh <- .get.bracket.glpk(tmp)
  a1 <- sub("^[(]", "", sub("[)]$", "", hh$beg))
  a2 <- a1
  while (substr(a2, 1, 1) != ",") {
    a2 <- gsub("^([[:alnum:]]|[+]|[-]|[*]|[$])*", "", a2)
    if (substr(a2, 1, 1) == "(") {
      a2 <- .get.bracket.glpk(a2)$end
    }
  }
  paste0(.get_glpk_loop_fast2(substr(a1, 1, nchar(a1) - nchar(a2))), "(", .eqt.to.glpk(substr(a2, 2, nchar(a2))), ")", .eqt.to.glpk(hh$end))
}
.eqt.to.glpk <- function(tmp) {
  # ITER = get('ITER', globalenv()) + 1
  # cat(ITER, tmp, '\n')
  # assign('ITER', ITER, globalenv())
  # if (ITER > 20) stop('ITER', ITER)
  rs <- ""
  while (nchar(tmp) != 0) {
    tmp <- gsub("^[ ]*", "", tmp)
    if (substr(tmp, 1, 4) == "sum(") {
      rs <- paste0(rs, "sum", .handle.sum.glpk(substr(tmp, 4, nchar(tmp))))
      tmp <- ""
    # } else if (any(grep("^([.[:digit:]]|[+]|[-]|[ ]|[*])", tmp))) {
    #   a3 <- gsub("^([.[:digit:]_]|[+]|[-]|[ ]|[*])*", "", tmp)
    # changing pattern to include scientific numbers
    } else if (any(grep("^([.[:digit:]_]([eE][-+]?\\d+)?|[+]\\s*|[-]\\s*|[ ]|[*])",
                        tmp))) {
      a3 <- gsub("^([.[:digit:]_]([eE][-+]?\\d+)?|[+]\\s*|[-]\\s*|[ ]|[*])", "", tmp)
      rs <- paste0(rs, substr(tmp, 1, nchar(tmp) - nchar(a3)))
      tmp <- a3
    } else if (substr(tmp, 1, 1) %in% c("m", "v", "p")) {
      a1 <- sub("^[[:alnum:]_]*", "", tmp)
      vrb <- substr(tmp, 1, nchar(tmp) - nchar(a1))
      a2 <- .get.bracket.glpk(a1)
      arg <- paste0(.aliasName(strsplit(gsub("[() ]", "", a2$beg), ",")[[1]]),
                    collapse = ", ")
      if (nchar(a2$end) > 1 && substr(a2$end, 1, 1) == "$") {
        rs <- paste0(
          rs, "sum{FORIF: (", arg, ") in ", gsub("([$]|[(].*)", "", a2$end),
          "} (", vrb, "[", arg, "])",
          .eqt.to.glpk(gsub("^[^)]*[)]", "", a2$end))
        )
        tmp <- ""
      } else {
        rs <- paste0(rs, vrb, "[", arg, "]", .eqt.to.glpk(a2$end))
        tmp <- ""
      }
    } else if (substr(tmp, 1, 1) == "=") {
      rs <- paste0(rs, c("g" = ">=", "e" = "=", "l" = "<=")[substr(tmp, 2, 2)])
      tmp <- substr(tmp, 4, nchar(tmp))
    } else if (substr(tmp, 1, 1) == ";") {
      rs <- paste0(rs, ";")
      tmp <- substr(tmp, 2, nchar(tmp))
    } else {
      browser()
    }
  }
  rs
}

# Equation declaration
.equation.from.gams.to.glpk <- function(eqt) {
  declaration <- gsub("[.][.].*", "", eqt)
  rs <- paste0("s.t. ", gsub("[($].*", "", declaration))
  if (nchar(declaration) != nchar(gsub("[($].*", "", declaration))) {
    rs <- paste0(rs, .get_glpk_loop_fast2(gsub("^[[:alnum:]_]*", "", declaration)))
  }
  rs <- paste0(rs, ": ", gsub("[[][ ]*[]]", "",
                              .eqt.to.glpk(gsub(".*[.][.][ ]*", "", eqt))))
  rs
}



