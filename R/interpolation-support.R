#' Internal function to interpolate parameter (column) in the given data.frame
#'
#' @param dtf data.frame, normally a slot of an object with parameters and sets.
#' @param parameter character, name of parameter to interpolate.
#' @param defVal numeric, default value of a parameter.
#' @param arg list with interpolation settings.
#'
#' @noRd
.interpolation0 <- function(dtf, parameter, defVal, arg) {
  if (length(defVal) != 1 || is.na(defVal) || is.null(defVal)) {
    invisible()  # browser() disabled
    stop("defVal value is not defined")
  }
  if (arg$approxim$fullsets && defVal != 0 && is.finite(defVal)) arg$all <- TRUE

  # Get timeslice
  prior <- c(
    "stg", "trade", "tech", "sup", "group", "acomm", "comm", "commp", "region",
    "regionp", "src", "dst", "timeslice", "year"
  )
  true_prior <- c(
    "stg", "trade", "tech", "sup", "group", "acomm", "comm", "commp", "region",
    "regionp", "src", "dst", "year", "timeslice"
  )
  rule <- arg$rule
  approxim <- arg$approxim
  if (is.null(approxim)) {
    approxim <- list()
    for (i in names(arg)[!(names(arg) %in% c("rule", "approxim", "year_range"))]) {
      approxim[[i]] <- arg[[i]]
    }
  }
  approxim <- approxim[names(approxim) %in% prior]
  there.are.year <- any(colnames(dtf) == "year")
  if (there.are.year && any(names(arg) == "year_range") &&
    all(names(approxim) != "year")) {
    approxim$year <- arg$year_range
  }
  prior <- prior[prior %in% names(approxim)]
  prior <- prior[prior %in% colnames(dtf)[-ncol(dtf)]]
  true_prior <- true_prior[true_prior %in% prior]
  approxim <- approxim[names(approxim) %in% prior]
  # drop excess columns
  # dtf <- dtf[, colnames(dtf) %in% c(prior, parameter), drop = FALSE]
  if (anyDuplicated(c(prior, parameter))) invisible()  # browser() disabled # DEBUG-
  dtf <- select(dtf, all_of(c(prior, parameter)))
  # Sort column
  # dtf <- dtf[, c(
  #   prior[prior %in% colnames(dtf)],
  #   colnames(dtf)[ncol(dtf)]
  # ), drop = FALSE]
  col_ord <- c(prior[prior %in% colnames(dtf)], colnames(dtf)[ncol(dtf)])
  setcolorder(dtf, col_ord)
  # dtf <- dtf[!is.na(dtf[[parameter]]), , drop = FALSE]
  dtf <- dtf |> filter(!is.na(dtf[[parameter]]))
  ii <- select(dtf, -ncol(dtf)) |> duplicated(fromLast = TRUE)
  # if (anyDuplicated(dtf[, -ncol(dtf)])) {
  if (any(ii)) {
    sstat <- sys.status()
    kstat <- sapply(sstat$sys.calls, function(x) any(grep(".obj2modInp", x[1])))
    if (sum(kstat) == 0) {
      warning("Duplicated values found and dropped. Use findDuplicates()",
              " function for the identification.")
    } else {
      tst_env <- sstat$sys.frames[[max(seq_along(kstat)[kstat])]]
      tst_exm <- get("app", tst_env)
      warning(paste0(
        '"Duplicated values found (class "', class(tst_exm), '", name "',
        tst_exm@name, '", parameter: "', parameter, '") and dropped.'
      ))
    }
    # dtf <- dtf[!duplicated(dtf[, -ncol(dtf)], fromLast = TRUE), ]
    dtf <- dtf |> filter(!ii)
  }
  if (nrow(dtf) == 0 && (is.null(arg$all) || !arg$all)) {
    return(NULL)
  }
  if (ncol(dtf) == 1) {
    if (nrow(dtf) == 0) dtf[1, 1] <- defVal
    return(dtf)
  }
  # Check if interpolation is needed
  approxim2 <- approxim
  if (!is.null(dtf$year)) {
    approxim2$year <- arg$approxim$mileStoneYears
    if (is.null(approxim2$year)) {
      approxim2$year <- arg$approxim$year
    }
  }
  # tmp_nona <- (!is.na(dtf[, -ncol(dtf), drop = FALSE]))
  tmp_nona <- !is.na(select(dtf, -ncol(dtf)))
  if (all(tmp_nona)) { # There is not NA column
    possible_comb <- prod(sapply(approxim2, length))
    if (nrow(dtf) >= possible_comb) {
      obj3 <- dtf
      for (i in names(approxim2)) {
        obj3 <- obj3[obj3[[i]] %in% approxim2[[i]], , drop = FALSE]
      }
      if (nrow(obj3) == possible_comb) {
        return(obj3)
      }
    }
  } else { # There are only NA and not NA column
    f1 <- apply(tmp_nona, 2, any)
    f2 <- apply(tmp_nona, 2, all)
    if (all(f1 == f2)) { # Could be small appr
      # obj2 <- dtf[, c(f1, TRUE), drop = FALSE]
      if (anyDuplicated(colnames(dtf))) invisible()  # browser() disabled # mappings check
      obj2 <- dtf |> select(all_of(colnames(dtf)[c(f1, TRUE)]))
      for (i in colnames(obj2)[-ncol(obj2)]) {
        obj2 <- obj2[obj2[[i]] %in% approxim2[[i]], , drop = FALSE]
      }
      if (ncol(obj2) == 1 || nrow(obj2) == prod(
        sapply(approxim2[names(obj2)[-ncol(obj2)]], length)
      )) { # numpar approximation is applicable
        for (i in names(dtf)[c(!f1, FALSE)]) {
          obj2 <- merge0(obj2, approxim2[i])
        }
        # return(obj2[, colnames(dtf)])
        if (anyDuplicated(colnames(dtf))) invisible()  # browser() disabled # mappings check
        return(select(obj2, all_of(colnames(dtf))))
      }
    }
  }
  # Real interpolation
  if (there.are.year) {
    year_range <- arg$year_range
    yy <- range(c(
      year_range[1], year_range[2],
      dtf$year
    ), na.rm = TRUE)
    approxim$year <- yy[1]:yy[2]
    apr <- approxim[c("year", true_prior[true_prior != "year"])]
    if (any(sapply(apr, length) == 0)) {
      return(NULL)
    }
    dd <- as.data.frame.table(
      array(NA, dim = sapply(apr, length), dimnames = apr),
      stringsAsFactors = FALSE, responseName = parameter)
    # dd <- dd[, c(prior, parameter), drop = FALSE]
    if (anyDuplicated(c(prior, parameter))) invisible()  # browser() disabled # mappings check
    dd <- dd |> select(all_of(c(prior, parameter)))
  } else {
    dd <- as.data.frame.table(
      array(NA, dim = sapply(approxim, length), dimnames = approxim),
      stringsAsFactors = FALSE, responseName = parameter)
  }
  if (nrow(dtf) != 0) {
    ii <- 2^(seq(length.out = ncol(dtf) - 1) - 1)
    # KK <- colSums(ii * t(is.na(dtf[, true_prior[true_prior %in% prior],
    #                                drop = FALSE])))
    sel_col <- true_prior[true_prior %in% prior]
    if (anyDuplicated(sel_col)) invisible()  # browser() disabled # mappings check
    KK <- colSums(ii * t(is.na(select(dtf, all_of(sel_col)))))
    # dobj <- as.matrix(dtf[, -ncol(dtf), drop = FALSE])
    dobj <- as.matrix(select(dtf, -ncol(dtf)))
    # ddd <- t(as.matrix(dd[, -ncol(dd), drop = FALSE]))
    ddd <- t(as.matrix(select(dd, -ncol(dd))))
    # dff <- dd[, -ncol(dd), drop = FALSE]
    dff <- dd |> select(-ncol(dd))
    # dtf <- dtf[, c(colnames(dff), parameter), drop = FALSE]
    dtf <- dtf |> select(all_of(c(colnames(dff), parameter)))
    for (i in 1:ncol(dff)) dff[[i]] <- as.factor(as.character(dff[[i]]))
    for (i in 1:ncol(dff)) dtf[[i]] <- factor(as.character(dtf[[i]]),
                                              levels = levels(dff[[i]]))
    for (i in 1:ncol(dff)) dtf[[i]] <- as.numeric(dtf[[i]])
    for (i in 1:ncol(dff)) dff[[i]] <- as.numeric(dff[[i]])
    hh <- sapply(dff, max)
    hh <- c(1, cumprod(hh[-length(hh)]))
    dff <- as.matrix(dff)
    dtf <- as.matrix(dtf)
    for (i in 1:ncol(dff)) {
      dff[, i] <- hh[i] * (dff[, i] - 1)
      dtf[, i] <- hh[i] * (dtf[, i] - 1)
    }
    # check all(sort(rowSums(dff)) == 0:max(rowSums(dff)))
    for (i in rev(sort(unique(KK)))) {
      fl <- seq(along = KK)[KK == i]
      # dff <- dd[fl, -ncol(dd), drop = FALSE]
      mx <- !is.na(dtf[fl[1], -ncol(dtf)])
      # gg <- rowSums(dtf[fl, -ncol(dtf), drop = FALSE])
      r1 <- rowSums(dff[, mx, drop = FALSE])
      # r1 <- rowSums(select(dff, all_of(names(dff)[mx])))
      r2 <- rowSums(dtf[fl, c(mx, FALSE), drop = FALSE])
      # r2 <- rowSums(select(dtf[fl,], names(dtf)[c(mx, FALSE)]))
      ll <- dtf[fl, ncol(dtf)]
      # ll <- dtf[[ncol(dtf)]][fl]
      names(ll) <- r2
      nn <- (r1 %in% r2)
      # dd[nn, ncol(dd)] <- ll[as.character(r1[nn])]
      dd[[ncol(dd)]][nn] <- ll[as.character(r1[nn])]
    }
  }
  # Interpolation
  if (!there.are.year) {
    dd[[parameter]][is.na(dd[[parameter]])] <- defVal
  } else {
    if (all(is.na(dd[[parameter]]))) {
      dd[[parameter]][is.na(dd[[parameter]])] <- defVal
    } else if (any(is.na(dd[[parameter]]))) {
      mx <- matrix(dd[[parameter]], length(approxim$year))
      f1 <- apply(!is.na(mx), 2, all)
      if (any(!f1)) {
        gg <- seq(along = f1)[!f1][apply(is.na(mx[, !f1, drop = FALSE]), 2, all)]
        mx[, gg] <- defVal
        f1[gg] <- TRUE
      }
      if (any(!f1)) {
        nr <- nrow(mx)
        back <- any(grep("back", rule))
        forth <- any(grep("forth", rule))
        inter <- any(grep("inter", rule))
        ## Group by similiarity
        for (ee in seq(along = f1)[!f1]) {
          ll <- ee
          # Approximate
          hh <- mx[, ee[1]]
          # Back
          if (is.na(hh[1])) {
            hm <- (1:nr)[!is.na(hh)][1]
            if (back) hh[1:(hm - 1)] <- hh[hm] else hh[1:(hm - 1)] <- defVal
          }
          # Forth
          if (is.na(hh[nr])) {
            hm <- max((1:nr)[!is.na(hh)])
            if (forth) hh[(hm + 1):nr] <- hh[hm] else hh[(hm + 1):nr] <- defVal
          }
          # Inter
          if (any(is.na(hh))) {
            if (!inter) {
              hh[is.na(hh)] <- defVal
            } else {
              hm <- is.na(hh)
              bg <- (1:(nr - 1))[hm[-1] & !hm[-nr]]
              en <- (2:nr)[!hm[-1] & hm[-nr]]
              for (i in seq(along = bg)) {
                hh[bg[i]:en[i]] <- seq(hh[bg[i]], hh[en[i]],
                  length.out = en[i] - bg[i] + 1
                )
              }
            }
          }
          # Assign
          mx[, ll] <- hh
          f1[ll] <- TRUE
        }
      }
      dd[[parameter]] <- c(mx)
    }
    if (any(colnames(dtf)[-ncol(dtf)] == "timeslice")) {
      # dd <- dd[, c(true_prior, parameter), drop = FALSE]
      dd <- dd |> select(all_of(c(true_prior, parameter)))
    }
    if (length(approxim$year) != year_range[2] - year_range[1] + 1) {
      dd <- dd[rep(
        year_range[1] <= approxim$year & approxim$year <= year_range[2],
        nrow(dd) / length(approxim$year)
      ), , drop = FALSE]
    }
    # if (parameter == "rhs") browser()
  }
  return(dd)
}

#' Internal function to interpolate parameter in given data.frame
#'
#' @param dtf data.frame, normally a slot of an object with parameters and sets.
#' @param parameter character, name of parameter to interpolate.
#' @param defVal numeric, default value of a parameter.
#' @param ... interpolation parameters.
#'
#' @noRd
.interpolation <- function(dtf, parameter, defVal, ...) {
  # new pipeline for interpolation routine is in progress
  arg <- list(...)
  # if (parameter == "rhs") browser()
  dtf_int <- tryCatch(
    {
      .interpolation0(dtf, parameter, defVal, arg)
    },
    error = function(cond) {
      assign("interpolation_message", list(
        tracedata = sys.calls(),
        interpolation0_arg = list(
          dtf = dtf, parameter = parameter,
          defVal = defVal, arg = arg
        )
      ), globalenv())
      message(
        "\nInterpolation error, more information in",
        ' "interpolation_message" object\n'
      )
      stop(cond)
    }
  )
  dtf_int
}

# setMethod(".interpolation_bound", signature(dtf = 'data.frame',
#   parameter = 'character', defVal = 'numeric', rule = 'character'),

#' Internal function to interpolate bounds in a given data.frame
#'
#' @param dtf data.frame, normally a slot of an object with parameters and sets.
#' @param parameter character, name of parameter to interpolate.
#' @param defVal numeric, default value of a parameter.
#' @param rule character, interpolation rule.
#' @param ... list of additional interpolation settings.
#'
#' @noRd
.interpolation_bound <- function(dtf, parameter, defVal, rule, ...) {
  dtf <- as.data.table(dtf)
  gg <- paste(parameter, c(".lo", ".fx", ".up"), sep = "")
  # aa <- dtf[, !(colnames(dtf) %in% gg), drop = FALSE]
  aa <- dtf |> select(all_of(colnames(dtf)[!(colnames(dtf) %in% gg)]))
  aa[[parameter]] <- rep(NA, nrow(aa))
  a1 <- aa
  a1[[parameter]] <- dtf[[gg[1]]]
  a2 <- aa
  a2[[parameter]] <- dtf[[gg[2]]]
  a3 <- aa
  a3[[parameter]] <- dtf[[gg[3]]]
  d1 <- .interpolation(rbind(a1, a2), parameter,
    defVal = defVal[1], rule = rule[1], ...
  )
  if (!is.null(d1)) {
    # dd <- d1[, -ncol(d1), drop = FALSE]
    dd <- d1 |> select(-ncol(d1))
    dd[, "type"] <- "lo"
    dd[[parameter]] <- d1[[parameter]]
  }
  d2 <- .interpolation(rbind(a3, a2), parameter,
    defVal = defVal[2], rule = rule[2], ...
  )
  if (!is.null(d2)) {
    # mx <- d2[, -ncol(d2), drop = FALSE]
    mx <- d2 |> select(-ncol(d2))
    # mx[, "type"] <- "up"
    mx[["type"]] <- "up"
    mx[[parameter]] <- d2[[parameter]]
  }
  if (!is.null(d1) && !is.null(d2)) {
    return(as.data.table(rbind(dd, mx)))
  } else if (!is.null(d1)) {
    return(as.data.table(dd))
  } else if (!is.null(d2)) {
    return(as.data.table(mx))
  } else {
    return(NULL)
  }
}

#' Internal function to interpolate 'numpar' parameter
#'
#' @param dtf data.frame, a slot with the data for interpolation.
#' @param parameter character, name of the column in the `dtf` to interpolate.
#' @param mtp class `parameter` to add interpolated data (in `modInp`).
#' @param approxim list with interpolation rules
#' @param add_set_name character, name of set to add element
#' @param add_set_value character, the element to add to the set
#' @param remove_duplicate tbc
#' @param all.val logical, if `TRUE` all values are interpolated
#'
#' @noRd
.interp_numpar <- function(
    dtf, parameter, mtp, approxim,
    add_set_name = NULL, add_set_value = NULL, remove_duplicate = NULL,
    # removeDefault = TRUE, # not used
    # remValue = NULL, # not used
    all.val = FALSE) {
  # if (!is.null(dtf[["wval"]])) browser()
  # cat(parameter, "\n")
  # if (parameter == "rhs") browser() # DEBUG
  has_year_col <- any(colnames(dtf) == "year")
  if (approxim$fullsets && mtp@defVal != 0 && is.finite(mtp@defVal)) all.val <- TRUE
  if (!all.val && nrow(dtf) == 0) {
    return(NULL)
  }

  if (!is.null(mtp@misc$not_need_interpolate)) {
    # approxim <- approxim[!(names(approxim) %in% mtp@misc$not_need_interpolate)]
    # dtf <- dtf[, !(colnames(dtf) %in% mtp@misc$not_need_interpolate), drop = FALSE]
    dtf <- dtf |>
      select(all_of(
        colnames(dtf)[!(colnames(dtf) %in% mtp@misc$not_need_interpolate)]
        ))
    if (any(mtp@misc$not_need_interpolate == "year")) has_year_col <- FALSE
    fl <- add_set_name %in% mtp@misc$not_need_interpolate
    if (any(fl)) {
      add_set_name <- add_set_name[!fl]
      add_set_value <- add_set_value[!fl]
    }
    dtf <- dtf[!duplicated(dtf), , drop = FALSE]
  }
  dd <- .interpolation(dtf, parameter,
                       rule = mtp@interpolation,
                       defVal = mtp@defVal,
                       year_range = range(approxim$year),
                       # year_range = dtf_year_range,
                       approxim = approxim, all = all.val
  )
  dtf <- as.data.table(dtf)
  if (is.null(dd)) {
    return(NULL)
  }
  if (!all.val) {
    dd <- dd[dd[[ncol(dd)]] != 0, , drop = FALSE]
    if (nrow(dd) == 0) {
      return(NULL)
    }
  }
  # Must fix in the future
  colnames(dd)[[ncol(dd)]] <- "value"
  char_col <- colnames(dd)
  char_col <- char_col[!(char_col %in% c("year", "value"))]
  for (i in char_col) {
    dd[[i]] <- as.character(dd[[i]])
  }
  if (has_year_col) dd[["year"]] <- as.integer(dd[["year"]])
  if (is.null(add_set_name)) {
    # dd <- dd[, c(mtp@dimSets, "value"), drop = FALSE]
    dd <- dd |> select(all_of(c(mtp@dimSets, "value")))
  } else {
    # d3 <- data.frame(stringsAsFactors = FALSE)
    # for (i in 1:length(add_set_value)) {
    #   d3[1:nrow(dd), i] <- rep(add_set_value[i])
    # }
    # colnames(d3) <- add_set_name
    d3 <- matrix(add_set_value, nrow = nrow(dd), ncol = length(add_set_value),
                 byrow = TRUE, dimnames = list(NULL, add_set_name)) |>
      as.data.table()
    stnd <- mtp@dimSets[-(1:length(d3))]
    # It was added for trading routes
    if (sum(stnd %in% c("src", "dst")) == 2) {
      stnd <- c(stnd[stnd != "src" & stnd != "dst"], "region")
    }
    stnd <- stnd[!(stnd %in% mtp@misc$not_need_interpolate)]
    if (any(ls(globalenv()) == "kstat")) invisible()  # browser() disabled
    # dd <- cbind(d3, dd[, c(stnd, "value"), drop = FALSE])
    dd <- cbind(d3, select(dd, all_of(c(stnd, "value"))))
  }
  if (!is.null(remove_duplicate) && nrow(dd) != 0) {
    fl <- rep(TRUE, nrow(dd))
    for (i in seq_along(remove_duplicate)) {
      fl <- (fl & dd[[remove_duplicate[[i]][1]]] != dd[[remove_duplicate[[i]][2]]])
    }
    dd <- dd[fl, , drop = FALSE]
  }
  if (has_year_col && !is.null(approxim$mileStoneYears)) {
    dd <- dd[dd$year %in% approxim$mileStoneYears, , drop = FALSE]
  }
  if (nrow(dd) == 0) {
    return(NULL)
  }

  dd
}

#' Internal function to interpolate 'bounds' parameter
#'
#' @param dtf data.frame, a slot with the data for interpolation.
#' @param parameter character, name of the column in the `dtf` to interpolate.
#' @param mtp class `parameter` to add interpolated data (in `modInp`).
#' @param approxim list with interpolation rules
#' @param add_set_name character, name of set to add element
#' @param add_set_value character, the element to add to the set
#' @param remove_duplicate tbc
#' @param remValueUp tbc
#' @param remValueLo tbc
#'
#'
#' @noRd
.interp_bounds <- function(
    dtf, parameter, mtp, approxim,
    add_set_name = NULL, add_set_value = NULL, remove_duplicate = NULL,
    remValueUp = NULL, remValueLo = NULL) {
  # if (parameter == "cout") browser()
  has_year_col <- any(colnames(dtf) == "year")
  if (!is.null(mtp@misc$not_need_interpolate)) {
    # dtf <- dtf[, !(colnames(dtf) %in% mtp@misc$not_need_interpolate), drop = FALSE]
    dtf <- dtf |>
      select(colnames(dtf)[!(colnames(dtf) %in% mtp@misc$not_need_interpolate)])
    if (any(mtp@misc$not_need_interpolate == "year")) has_year_col <- FALSE
    fl <- add_set_name %in% mtp@misc$not_need_interpolate
    if (any(fl)) {
      add_set_name <- add_set_name[!fl]
      add_set_value <- add_set_value[!fl]
    }
    dtf <- dtf[!duplicated(dtf), , drop = FALSE]
  }

  dd <- .interpolation_bound(dtf, parameter,
                             defVal = mtp@defVal,
                             rule = mtp@interpolation,
                             year_range = range(approxim$year),
                             approxim = approxim
  )
  if (is.null(dd)) {
    return(NULL)
  }
  dd <- dd[dd[[ncol(dd)]] != 0 | dd$type == "up", , drop = FALSE]
  if (nrow(dd) == 0) {
    return(NULL)
  }

  colnames(dd)[[ncol(dd)]] <- "value"
  for (i in colnames(dd)[-ncol(dd)]) {
    dd[[i]] <- as.character(dd[[i]])
  }
  if (has_year_col) dd[["year"]] <- as.integer(dd[["year"]])
  if (is.null(add_set_name)) {
    # dd <- dd[, c(mtp@dimSets, "type", "value"), drop = FALSE]
    dd <- dd |> select(all_of(c(mtp@dimSets, "type", "value")))
  } else {
    d3 <- data.frame(stringsAsFactors = FALSE)
    for (i in 1:length(add_set_value)) { # !!! rewrite
      d3[1:nrow(dd), i] <- rep(add_set_value[i])
    }
    colnames(d3) <- add_set_name
    stnd <- mtp@dimSets[-(1:length(d3))]
    # It was added for trading routes
    if (sum(stnd %in% c("src", "dst")) == 2) {
      stnd <- c(stnd[stnd != "src" & stnd != "dst"], "region")
    }
    stnd <- stnd[!(stnd %in% mtp@misc$not_need_interpolate)]
    # dd <- cbind(d3, dd[, c(stnd, "type", "value"), drop = FALSE])
    dd <- try({
      cbind(d3, select(dd, all_of(c(stnd, "type", "value"))))
    }, silent = TRUE)
    if (inherits(dd, "try-error")) { #!!! Debug
      invisible()  # browser() disabled
    }


  }
  dd <- dd[(dd$type == "lo") | (dd$type == "up"), , drop = FALSE]
  if (!is.null(remove_duplicate) && nrow(dd) != 0) {
    fl <- rep(TRUE, nrow(dd))
    for (i in seq_along(remove_duplicate)) {
      invisible()  # browser() disabled # duplicated columns?
      fl <- (fl & dd[, remove_duplicate[[i]][1]] != dd[, remove_duplicate[[i]][2]])
    }
    dd <- dd[fl, , drop = FALSE]
  }
  if (has_year_col && !is.null(approxim$mileStoneYears)) {
    dd <- dd[dd$year %in% approxim$mileStoneYears, , drop = FALSE]
  }
  if (nrow(dd) == 0) {
    return(NULL)
  }
  return(as.data.table(dd))
}




# ---------------------------------------------------------------------------
# (was R/interp_shared.R, merged 2026-09-20)
# ---------------------------------------------------------------------------

# =============================================================================#
# interp_shared.R  -- shared utilities relocated out of the (retired) legacy
# interpolation files (interpolate.R / interpolate2.R / obj2modInp.R / add2set.R,
# now in depreciated/R/). These functions are used by the NEW interp_mod()
# pipeline and/or general package code, so they survive the legacy retirement.
# Extracted verbatim (srcref) by tmp/_extract.R; see PIPELINE.md.
# =============================================================================#

# ---- from add2set.R ----
.drop_config_param <- .drop_config_param <- function(modInp) {
  for (i in c("pWacc", "pSdr", "pDummyImportCost", "pDummyExportCost")) {
    modInp@parameters[[i]] <- .resetParameter(modInp@parameters[[i]])
  }
  modInp
}

# ---- from interpolate.R ----
.apply_to_code_ret_list <- .apply_to_code_ret_list <- function(scen, func, ..., clss = NULL,
                                    need.name = TRUE) {
  rs <- list()
  for (i in seq(along = scen@model@data)) {
    for (j in seq(along = scen@model@data[[i]]@data)) {
      if (is.null(clss) || any(class(scen@model@data[[i]]@data[[j]]) == clss)) {
        if (need.name) {
          rr <- func(scen@model@data[[i]]@data[[j]], ...)
          rs[[rr$name]] <- rr$val
        } else {
          rs[[length(rs) + 1]] <- func(scen@model@data[[i]]@data[[j]], ...)
        }
      }
    }
  }
  rs
}

.check_scen_par <- .check_scen_par <- function(scen) {
  # Check for non negative parameters, all except 'pAggregateFactor', 'pTechCvarom', 'pTechAvarom', 'pTechVarom', 'pTechInvcost'
  non_negative <- unique(c(
    "pTimesliceShare", "pTimesliceWeight", "pTechOlife", "pTechCinp2ginp",
    "pTechGinp2use", "pTechCinp2use", "pTechUse2cact", "pTechCact2cout",
    "pTechEmisComm", "pTechAct2AInp", "pTechCap2AInp", "pTechNCap2AInp",
    "pTechCinp2AInp", "pTechCout2AInp",
    "pTechAct2AOut", "pTechCap2AOut", "pTechNCap2AOut", "pTechCinp2AOut",
    "pTechCout2AOut", "pTechFixom", "pTechShare",
    "pTechShare", "pTechAf", "pTechAf", "pTechAfs", "pTechAfs", "pTechAfc",
    "pTechAfc", "pTechStock", "pTechCap2act", "pWacc", "pSdr",
    "pDiscountFactor", "pSupCost", "pSupAva", "pSupAva", "pSupReserve",
    "pSupReserve", "pDemand", "pEmissionFactor", "pDummyImportCost",
    "pDummyExportCost", "pTaxCostInp", "pSubCostInp", "pTaxCostOut",
    "pSubCostOut", "pTaxCostBal", "pSubCostBal",
    "pWeather", "pSupWeather", "pSupWeather", "pTechWeatherAf",
    "pTechWeatherAf", "pTechWeatherAfs", "pTechWeatherAfs",
    "pTechWeatherAfc", "pTechWeatherAfc", "pStorageWeatherAf",
    "pStorageWeatherAf", "pStorageWeatherInpAf", "pStorageWeatherInpAf",
    "pStorageWeatherOutAf", "pStorageWeatherOutAf", "pStorageInpEff",
    "pStorageOutEff", "pStorageStgEff", "pStorageOutStock", "pStorageOlife",
    "pStorageCostStore", "pStorageCostInp",
    "pStorageCostOut", "pStorageOutFixom", "pStorageOutInvcost", "pStorageDuration",
    "pStorageInp2stg",
    "pStorageAf", "pStorageAf", "pStorageInpAf", "pStorageInpAf", "pStorageOutAf",
    "pStorageOutAf", "pStorageStg2AInp", "pStorageStg2AOut", "pStorageCinp2AInp",
    "pStorageCinp2AOut", "pStorageCout2AInp", "pStorageCout2AOut",
    "pStorageInpCap2AInp", "pStorageInpCap2AOut",
    "pStorageInpNCap2AInp", "pStorageInpNCap2AOut",
    "pStorageStgCap2AInp", "pStorageStgCap2AOut",
    "pStorageStgNCap2AInp", "pStorageStgNCap2AOut",
    "pStorageOutCap2AInp", "pStorageOutCap2AOut",
    "pStorageOutNCap2AInp",
    "pStorageOutNCap2AOut", "pTradeIrEff", "pTradeIr", "pTradeIr",
    "pTradeIrCost", "pTradeIrMarkup", "pTradeIrCsrc2Ainp",
    "pTradeIrCsrc2Aout", "pTradeIrCdst2Ainp", "pTradeIrCdst2Aout",
    "pExportRowRes", "pExportRow",
    # "pExportRowPrice",
    "pImportRowRes", "pImportRow",
    "pImportRow",
    "pTechRet", "pTechCap", "pTechNewCap",
    "pStorageOutRet", "pStorageOutCap", "pStorageOutNewCap",
    "pTradeRet", "pTradeCap", "pTradeNewCap"
    # "pImportRowPrice"
  ))
  msg_small_err <- NULL
  for (i in non_negative) {
    if (any(scen@modInp@parameters[[i]]@data$value < 0)) {
      if (any(scen@modInp@parameters[[i]]@data$value < -1e-7)) {
        msg <- paste0('An attempt to assignin negative numbers
                      to non-negative parameter: "', i)
        tmp <- scen@modInp@parameters[[i]]@data[
          scen@modInp@parameters[[i]]@data$value < 0, ,
          drop = FALSE]
        msg <- c(msg, capture.output(print(tmp[1:min(c(10, nrow(tmp))), ,
                                               drop = FALSE])))

        if (nrow(tmp) > 10) {
          msg <- c(msg,
                   paste0("Showing only the first 10 errors in data, from ",
                          nrow(tmp), "\n")
                   )
        }
        stop(paste0(msg, collapse = "\n"))
      } else {
        msg_small_err <- c(msg_small_err, i)
        scen@modInp@parameters[[i]]@data[
          scen@modInp@parameters[[i]]@data$value > -1e-7 &
          scen@modInp@parameters[[i]]@data$value < 0, "value"] <- 0
      }
    }
  }
  if (length(msg_small_err) > 0) {
    warning(paste0(
      "There small negative value (abs(err) < 1e-7) in parameter",
      "s"[length(msg_small_err) > 1], ': "',
      paste0(msg_small_err, collapse = '", "'), '". Assigned as zerro.'
    ))
  }
  # Check share
  if (nrow(scen@modInp@parameters$pTechShare@data) > 0) {
  }
  scen
}

.get_map_commodity_timeslice_map <- .get_map_commodity_timeslice_map <- function(scen) {
  .apply_to_code_ret_list(
    scen = scen,
    clss = "commodity",
    func = function(x) {
      list(name = x@name, val = x@timeframe)
    }
  )
}

.get_map_commodity_timeslice_map_obj <- .get_map_commodity_timeslice_map_obj <- function(obj) {
  xx <- list()
  for (i in seq(along = obj@data)) {
    for (j in seq(along = obj@data[[i]]@data)) { #
      prec <- .add2set(
        prec,
        obj@data[[i]]@data[[j]],
        approxim = approxim)
      if (is(obj@data[[i]]@data[[j]], "commodity")) {
        if (length(obj@data[[i]]@data[[j]]@timeframe) == 0) {
          obj@data[[i]]@data[[j]]@timeframe <-
            approxim$calendar@default_timeframe
        }
        commodity_timeslice_map[[obj@data[[i]]@data[[j]]@name]] <-
          obj@data[[i]]@data[[j]]@timeframe
      }
    }
  }
}

.interpolation_message <- .interpolation_message <- function(name, num, interpolation_count,
                                   interpolation_start_time, len_name) {
  jj <- paste0(
    num, " (", interpolation_count, "),",
    paste0(rep(" ", max(c(1, 15 - (nchar(name) %% 15)))), collapse = ""),
    name, ", time: ", round(proc.time()[3] - interpolation_start_time, 2), "s"
  )
  # bug "invalid langth.out element - workaround
  length_out <- len_name - nchar(jj)
  if (length_out < 0) {
    len_name <- len_name + abs(length_out)
    length_out <- 0
  }
  jj <- paste0(jj, paste0(rep(" ", length_out), collapse = ""))
  # cat(rep_len("\b", len_name), jj, sep = "") # , rep(' ', 100), rep('\b', 100)
}

interpolate_slot <- interpolate_slot <- function(
    x,
    keys = c("region", "timeslice", "comm", "acomm", "tech", "process",
             "weather", "stg", "sub", "dst", "src"),
    year_seq = NULL,
    val = "value"
) {
  if (!is.null(x$year)) {
    if (is.null(year_seq)) year_seq <- full_seq(x$year, 1)
    # Every non-year key column is a group: interpolating the whole column at
    # once collapses tied years across groups (two regions' values average
    # into one), and a group with a single year has nothing to interpolate.
    by <- setdiff(names(x), c("year", val))
    x <- x |>
      group_by(across(all_of(by))) |>
      complete(year = year_seq)
    if (!is.null(val) && !is.na(val)) {
      x <- x |>
        mutate(
          {{val}} := if (sum(!is.na(.data[[val]])) >= 2) {
            zoo::na.approx(.data[[val]], x = year, na.rm = FALSE)
          } else {
            .data[[val]]
          }
        ) |>
        filter(!is.na(.data[[val]]))
    }
    x <- ungroup(x)
  }
  # if (is.null(year_seq)) year_seq = full_seq(x$year, 1)
    #
    # mutate(
    #   {{val}} := zoo::na.approx(.data[[val]], x = year)
    # ) |>
    # as.data.table() |>
    # ungroup()
  x
}

# ---- from obj2modInp.R ----
.add_ramp0 <- .add_ramp0 <- function(obj, name, tech, mact, approxim) {
  if (any(!is.na(tech@af[[name]]))) {
    pname <- paste0(
      "p", c("technology" = "Tech", "storage" = "Storage")[class(tech)],
      c("rampup" = "RampUp", "rampdown" = "RampDown", name)[name]
    )
    set_name <- c("technology" = "tech", "storage" = "stg")[class(tech)]
    mname <- sub("^p", "m", pname)
    rampup <- tech@af[!is.na(tech@af[[name]]), ]
    approxim2 <- approxim
    if (all(!is.na(rampup$timeslice))) {
      approxim2$timeslice <- approxim2$timeslice[approxim2$timeslice %in% unique(rampup$timeslice)]
    }
    pTechRampUp <- .interp_numpar(
      rampup, name,
      obj@parameters[[pname]], approxim2, set_name, tech@name
    )
    # mTechRampUp <- pTechRampUp[, colnames(pTechRampUp) != "value", drop = FALSE]
    mTechRampUp <- select(pTechRampUp, -value)
    if (ncol(mTechRampUp) != ncol(obj@parameters[[mname]]@data)) {
      mTechRampUp <- merge0(mTechRampUp, mact)
    }
    # adding timeslicep (next) to the mapping
    # ramp_data <- c(tech@af$rampdown, tech@af$rampup)
    # if (!is_empty(ramp_data) && any(!is.na(ramp_data))) {

    if (tech@fullYear) {
      TimesliceNext <- obj@parameters[["mTimesliceFYearNext"]]@data
    } else {
      TimesliceNext <- obj@parameters[["mTimesliceNext"]]@data
    }
    mTechRampUp <- left_join(mTechRampUp, TimesliceNext, by = "timeslice") |>
      select(all_of(obj@parameters[[mname]]@dimSets))

    obj@parameters[[pname]] <- .dat2par(obj@parameters[[pname]], pTechRampUp)
    obj@parameters[[mname]] <- .dat2par(obj@parameters[[mname]], mTechRampUp)
  }
  obj
}

.filter_data_in_slots <- .filter_data_in_slots <- function(obj, lst, coln) {
  # filter out
  # by INSTANCE, not by class definition: an object saved before a slot was
  # added still deserialises, and `slot()` on the missing name errors
  ss <- getSlots(class(obj))[.instance_slots(obj)]
  if (any(names(ss) == coln) && ss[coln] == "character") {
    # !!! adding (potentially) missing filter for character slots like region
    if (!all(is.na(slot(obj, coln))) && length(slot(obj, coln)) > 0) {
      slot(obj, coln) <- slot(obj, coln)[slot(obj, coln) %in% lst]
    }
  }
  ss <- names(ss)[ss %in% "data.frame"]
  ss <- ss[sapply(ss, function(x) {
    any(colnames(slot(obj, x)) == coln) && nrow(slot(obj, x)) != 0
    })]
  for (sl in ss) {
    slot(obj, sl) <- slot(obj, sl)[
      is.na(slot(obj, sl)[, coln]) |
        slot(obj, sl)[, coln] %in% lst, ,
      drop = FALSE]
  }
  obj
}

.fix_approximation_list <- .fix_approximation_list <- function(approxim, lev = NULL, comm = NULL) {
  # better name?
  if (length(lev) == 0) {
    if (length(comm) == 0) {
      stop("Internal error: 66a37cde-24e2-4ac5-ab24-b79e0f603bf7")
    }
    lev <- approxim$commodity_timeslice_map[[comm]]
  }
  # ??? better name for approxim$parent_child ???
  approxim$parent_child <- approxim$calendar@timeslice_ancestry
  approxim$timeslice <- approxim$calendar@timeframes[[lev]]
  # approxim$parent_child <-
  #   approxim$parent_child[approxim$parent_child$child %in% approxim$timeslice, ,
  #                         drop = FALSE]
  approxim$parent_child <- approxim$parent_child |>
    filter(child %in% approxim$timeslice)
  approxim
}

.force_value_class_df <- .force_value_class_df <- function(dtf) {
  if (!is.data.frame(dtf) & !is.list(dtf)) invisible()  # browser() disabled
  # return(dtf)
  # temporary solution to avoid merging conflicts
  # `inherits(x, "numeric")` is FALSE for an integer vector, so a solution whose
  # `value` column reads back as integer (a whole-numbered result, e.g. an
  # import of exactly 1500) used to trip the guard below -- note the coercion
  # that was stranded after `stop()`. Coerce integers, and keep stopping on a
  # genuinely non-numeric column.
  if (!is.null(dtf[["value"]]) && !is.numeric(dtf[["value"]])) {
    print(as_tibble(dtf))
    stop("Non-numeric 'value' column")
  }
  if (!is.null(dtf[["value"]]) && !is.double(dtf[["value"]])) {
    dtf[["value"]] <- as.numeric(dtf[["value"]])
  }
  as.data.table(dtf)
}

.force_year_class_df <- .force_year_class_df <- function(dtf) {
  # if (!is.data.frame(dtf) & !is.list(dtf)) browser()
  # return(dtf)
  # temporary solution to avoid merging conflicts
  year_vars <- c("year", "yearp", "start", "end", "olife")
  force_class <- "integer"
  # force_class <- "numeric"
  for (y in year_vars) {
    if (!is.null(dtf[[y]]) && !inherits(dtf, force_class)) {
      dtf[[y]] <- as(dtf[[y]], force_class)
    }
  }
  as.data.table(dtf)
}

.null_to_empty_param <- .null_to_empty_param <- function(pname, pp) {
  # pp - podInp@parameters
  # pp <- get(pp, envir = parent.frame())
  p <- get(pname, envir = parent.frame())
  if (is.null(p)) p <- pp[[pname]]@data[0, ]
  assign(pname, p, envir = parent.frame())
  # p
}

.toWeatherImply <- .toWeatherImply <- function(dtf, val, add_set, add_val, sets = NULL) {
  dtf <- as.data.table(dtf)
  # browser() ### !!! ToDo: dplyr
  # f1 <- dtf[!is.na(dtf[, paste0(val, ".up")]),
  #           c(paste0(val, ".up"), "weather", sets),
  #           drop = FALSE]
  # colnames(f1)[1] <- "value"
  c_nm <- paste0(val, ".up")
  ii <- select(dtf, all_of(c_nm))[[1]] |> is.na()
  f1 <- dtf |>
    filter(!ii) |>
    select(all_of(c(c_nm, "weather", sets))) |>
    rename(value = all_of(c_nm))
  # f2 <- dtf[!is.na(dtf[, paste0(val, ".fx")]),
  #           c(paste0(val, ".fx"), "weather", sets),
  #           drop = FALSE]
  # colnames(f2)[1] <- "value"
  c_nm <- paste0(val, ".fx")
  ii <- select(dtf, all_of(c_nm))[[1]] |> is.na()
  f2 <- dtf |>
    filter(!ii) |>
    select(all_of(c(c_nm, "weather", sets))) |>
    rename(value = all_of(c_nm))
  # f3 <- dtf[!is.na(dtf[, paste0(val, ".lo")]),
  #           c(paste0(val, ".lo"), "weather", sets),
  #           drop = FALSE]
  # colnames(f3)[1] <- "value"
  c_nm <- paste0(val, ".lo")
  ii <- select(dtf, all_of(c_nm))[[1]] |> is.na()
  f3 <- dtf |>
    filter(!ii) |>
    select(all_of(c(c_nm, "weather", sets))) |>
    rename(value = all_of(c_nm))
  rs <- list(par = NULL)
  if (nrow(f1) + nrow(f2) != 0) {
    tmp <- rbind(f1, f2)
    # tmp[, add_set] <- add_val
    tmp[[add_set]] <- add_val
    # rs$mapup <- tmp[, -1, drop = FALSE]
    rs$mapup <- select(tmp, -value)
    tmp$type <- "up"
    rs$par <- tmp
  }
  if (nrow(f3) + nrow(f2) != 0) {
    tmp <- rbind(f3, f2)
    # tmp[, add_set] <- add_val
    tmp[[add_set]] <- add_val
    # rs$maplo <- tmp[, -1, drop = FALSE]
    rs$maplo <- select(tmp, -value)
    tmp$type <- "lo"
    rs$par <- rbind(rs$par, tmp)
  }
  rs
}

# --------------------------------------------------------------------------- #
# Settings-level per-column overrides of parameter defaults / interpolation.
#
# `settings@defVal` / `settings@interpolation` (inherited from `config`, filled
# from config_default_values.yml / config_default_interpolation.yml, copied to
# the scenario by `.config_to_settings()`) were initialised for years but never
# consumed. They now override each parameter's catalog (`.modInp`) values,
# keyed by the parameter's `colName` provenance -- `pTechAf` (colName "af",
# bounds) reads columns `af.lo` / `af.up`; a numpar like `pDemand` (colName
# "dem") reads column `dem`.
#
# An override applies ONLY where the settings value DIFFERS from the baked
# baseline (`.defVal` / `.defInt`): the two YAML mirrors have drifted from
# modInp.yml in places, and an untouched settings table must remain a no-op --
# modInp.yml stays the source of defaults unless the user changes a column.
# --------------------------------------------------------------------------- #
.apply_settings_param_overrides <- function(scen) {
  ss <- scen@settings
  sv <- ss@defVal
  si <- ss@interpolation
  if ((is.null(sv) || nrow(sv) == 0) && (is.null(si) || nrow(si) == 0)) {
    return(scen)
  }
  base_v <- .defVal
  base_i <- .defInt

  changed <- function(tab, base, key) {
    !is.null(tab) && nrow(tab) >= 1 && key %in% colnames(tab) &&
      !is.null(base[[key]]) &&
      !isTRUE(all.equal(tab[[key]][1], base[[key]], tolerance = 0))
  }

  for (nm in names(scen@modInp@parameters)) {
    p <- scen@modInp@parameters[[nm]]
    if (!p@type %in% c("numpar", "bounds")) next
    # colName provenance lives in @inClass (class / slot / colName)
    cn <- unique(p@inClass$colName)
    cn <- cn[!is.na(cn) & nzchar(cn)]
    if (length(cn) != 1) next
    keys <- if (p@type == "bounds") paste0(cn, c(".lo", ".up")) else cn
    touched <- FALSE
    for (j in seq_along(keys)) {
      if (changed(sv, base_v, keys[j])) {
        p@defVal[j] <- as.numeric(sv[[keys[j]]][1])
        touched <- TRUE
      }
      if (changed(si, base_i, keys[j]) && j <= length(p@interpolation)) {
        p@interpolation[j] <- as.character(si[[keys[j]]][1])
        touched <- TRUE
      }
    }
    if (touched) scen@modInp@parameters[[nm]] <- p
  }
  scen
}

merge0 <- merge0 <- function(x, y,
                   by = intersect(
                     colnames(as.data.table(x)),
                     colnames(as.data.table(y))
                   ),
                   ...) {
  # assign('x', x, globalenv()) assign('y', y, globalenv())
  if (length(by) != 0) {
    y <- as.data.table(y) |> .force_year_class_df()
    x <- as.data.table(x) |> .force_year_class_df()
    xy <- merge(x, y, by = by, ..., allow.cartesian = TRUE)
    # return(as.data.table(xy)) # debug pDiscountFactorMileStone
    return(xy)
  }
  # y <- as.data.table(y) |> .force_year_class_df()
  # x <- as.data.table(x) |> .force_year_class_df()
  y <- .force_year_class_df(y)
  x <- .force_year_class_df(x)
  # xy <- merge(x, y)
  suppressMessages({
    xy <- dplyr::cross_join(x, y) # !!! rewrite
  })
  # colnames(xy) <- c(colnames(x), colnames(y)) # ???
  # return(as.data.table(xy))
  return(as.data.table(xy))
}



# ---------------------------------------------------------------------------
# (was R/interp_progress.R, merged 2026-09-20)
# ---------------------------------------------------------------------------

# =========================================================================== #
# interp_progress.R  —  progress reporting for interp_mod().
#
# Two layers, both opt-in:
#   * `verbose = TRUE` renders an always-visible cli banner (selected options) and
#     one cli step line per pipeline stage (including the fold/prune/densify
#     routines as they run), plus a closing `model_size()` summary.
#   * a `progressr` progressor wraps the per-object interpolation loop, so a user
#     who registers a handler (`progressr::handlers(...)`) gets a customizable
#     progress bar — matching the legacy pipeline's behaviour. Silent when no
#     handler is set, so it never disturbs the default / test path.
# =========================================================================== #

# Module clock so each stage reports its own elapsed time. The currently-running
# stage is the last `>` (info) line with no matching `v` (done) line yet, so a
# user always sees which stage is in progress -- important for the slow stages
# (ob2mi, densify, the model-size summary) on large models.
.interp_clock <- new.env(parent = emptyenv())

.interp_fmt_secs <- function(s) {
  if (is.na(s)) "" else if (s < 60) sprintf("%.1fs", s)
  else if (s < 3600) sprintf("%dm %02ds", s %/% 60, round(s %% 60))
  else sprintf("%dh %02dm", s %/% 3600, (s %% 3600) %/% 60)
}

# TRUE when we can rewrite the current terminal line in place (interactive TTY).
# On a dynamic TTY the stage prints `> msg` with no newline and is completed in
# place as `v msg (1.4s)`; otherwise (logs, tests, non-interactive) it falls back
# to two separate lines.
.interp_dyn_tty <- function() {
  tryCatch(isTRUE(cli::is_dynamic_tty()), error = function(e) interactive())
}

# Mark the currently-running stage (if any) as done, with elapsed time. On a
# dynamic TTY this overwrites the open `> msg` line in place with `v msg (time)`.
.interp_step_done <- function(verbose = TRUE) {
  if (is.null(.interp_clock$msg)) return(invisible())
  dt <- as.numeric(Sys.time() - .interp_clock$t, units = "secs")

  # Record before printing, so a non-verbose run still accounts for the stage.
  # `peak_mb` is the PER-STAGE heap high-water mark (the mark is reset at each
  # stage start in `.interp_step`); `rss_mb` / `peak_rss_mb` are OS process
  # memory (see `.en_rss` -- the peak is process-lifetime, a running maximum).
  m1 <- tryCatch(.en_mem(), error = function(e) NULL)
  .interp_clock$stages <- c(.interp_clock$stages %||% list(), list(list(
    stage = .interp_clock$msg,
    secs = round(dt, 2),
    mem_mb = if (is.null(m1)) NA_real_ else m1$mem_mb,
    peak_mb = if (is.null(m1)) NA_real_ else m1$peak_mb,
    d_mem_mb = if (is.null(m1) || is.null(.interp_clock$m0)) NA_real_
               else round(m1$mem_mb - .interp_clock$m0$mem_mb, 1),
    rss_mb = if (is.null(m1)) NA_real_ else m1$rss_mb,
    peak_rss_mb = if (is.null(m1)) NA_real_ else m1$peak_rss_mb)))

  if (!isTRUE(verbose)) {
    .interp_clock$open <- FALSE
    .interp_clock$msg <- NULL
    return(invisible())
  }
  done <- paste0(.interp_clock$msg, " ",
                 cli::col_grey("(", .interp_fmt_secs(dt), ")"))
  if (isTRUE(.interp_clock$open)) {
    # rewrite the open `> msg` line: `\r` to column 0, then `v msg (time)`. The
    # done text is always longer than `> msg`, so it fully overwrites it.
    cat("\r", cli::col_green(cli::symbol$tick), " ", done, "\n", sep = "")
  } else {
    cli::cli_alert_success(done, .envir = emptyenv())
  }
  .interp_clock$open <- FALSE
  .interp_clock$msg <- NULL
  invisible()
}

# Start-of-run banner: the resolved storage / sparse / prune / fold / validate
# choices. No-op unless verbose. Also resets the stage clock.
.interp_banner <- function(scen, sparse, prune, fold_dims, validate, ondisk,
                           verbose) {
  if (!isTRUE(verbose)) return(invisible())
  .interp_clock$msg  <- NULL
  .interp_clock$open <- FALSE
  .interp_clock$t    <- Sys.time()
  cli::cli_h1(paste0("interpolate_model: ", scen@name))
  cli::cli_dl(c(
    storage  = if (isTRUE(ondisk)) paste0("on-disk (", scen@path, ")") else "in-memory",
    sparse   = as.character(isTRUE(sparse)),
    prune    = as.character(isTRUE(prune)),
    fold     = if (length(fold_dims)) paste(fold_dims, collapse = ", ") else "none",
    validate = as.character(isTRUE(validate))
  ))
  invisible()
}

# One pipeline-stage line. Closes the previous stage (with its elapsed time),
# then announces this one. `msg` is treated literally (no glue interpolation).
#' @param oneline when TRUE (default) and on a dynamic TTY, the stage prints on a
#'   single line completed in place. Set FALSE for stages that emit their own
#'   output while running (e.g. a progressr bar), so that output gets a clean
#'   line and the stage falls back to the two-line info/success form.
#' @noRd
# Stage accounting is ALWAYS on; `verbose` only controls whether it is printed.
# The timings are what the operation log reports, and a log that only works in
# verbose runs is no log at all.
.interp_stages_reset <- function() {
  .interp_clock$stages <- list()
  .interp_clock$maps <- list()
  .interp_clock$params <- list()
  .interp_clock$memo <- NULL
  invisible()
}

# --------------------------------------------------------------------------- #
# Per-interpolation memoization for model-derived lookups (process classes,
# invest-year windows, process->timeslice tables). These are recomputed by
# dozens of map builders while the underlying model objects are fixed for the
# whole run (variants are expanded before any builder runs). Entries are
# guarded by a `fingerprint` compared with identical(), so a memo populated by
# one scenario can never serve another: any change in the inputs the caller
# fingerprints invalidates the entry. The cache lives in `.interp_clock` and
# is dropped by `.interp_stages_reset()` at the start of every interpolation.
# --------------------------------------------------------------------------- #
.interp_memo <- function(key, fingerprint, compute) {
  cache <- .interp_clock$memo
  if (is.null(cache)) {
    cache <- new.env(parent = emptyenv())
    .interp_clock$memo <- cache
  }
  hit <- cache[[key]]
  if (!is.null(hit) && identical(hit$fp, fingerprint)) return(hit$value)
  value <- compute()
  cache[[key]] <- list(fp = fingerprint, value = value)
  value
}

# Fingerprint of the model's object roster: object names per repository.
# Any object added, dropped, or renamed (variant expansion, transform
# materialization) changes it.
.interp_model_fp <- function(scen) {
  lapply(scen@model@data, function(r) names(r@data))
}

.interp_stages <- function() {
  st <- .interp_clock$stages
  if (is.null(st) || !length(st)) return(NULL)
  do.call(rbind, lapply(st, as.data.frame, stringsAsFactors = FALSE))
}

# Per-map / per-parameter tick accounting (same always-on contract as the
# stage table). Retrieve with .interp_map_table() / .interp_param_table()
# right after interpolate_model() in the same session.
.interp_map_tick <- function(name, recipe, secs) {
  .interp_clock$maps[[length(.interp_clock$maps) + 1L]] <-
    list(map = name, recipe = recipe, secs = secs)
  invisible()
}

.interp_param_tick <- function(name, secs) {
  .interp_clock$params[[length(.interp_clock$params) + 1L]] <-
    list(parameter = name, secs = secs)
  invisible()
}

.interp_map_table <- function() {
  x <- .interp_clock$maps
  if (is.null(x) || !length(x)) return(NULL)
  do.call(rbind, lapply(x, as.data.frame, stringsAsFactors = FALSE))
}

.interp_param_table <- function() {
  x <- .interp_clock$params
  if (is.null(x) || !length(x)) return(NULL)
  do.call(rbind, lapply(x, as.data.frame, stringsAsFactors = FALSE))
}

.interp_step <- function(verbose, msg, oneline = TRUE) {
  .interp_step_done(verbose)          # close prior stage with its timing
  .interp_clock$t   <- Sys.time()
  .interp_clock$msg <- msg
  # reset = TRUE re-arms the heap high-water mark, so this stage's closing
  # probe reports the stage's OWN peak, not the session's.
  .interp_clock$m0  <- tryCatch(.en_mem(reset = TRUE), error = function(e) NULL)
  if (!isTRUE(verbose)) {
    .interp_clock$open <- FALSE
    return(invisible())
  }
  if (isTRUE(oneline) && .interp_dyn_tty()) {
    # open the line (no newline); .interp_step_done() completes it in place
    cat(cli::col_cyan(cli::symbol$info), " ", msg, sep = "")
    utils::flush.console()
    .interp_clock$open <- TRUE
  } else {
    cli::cli_alert_info(msg, .envir = emptyenv())
    .interp_clock$open <- FALSE
  }
  invisible()
}

# --------------------------------------------------------------------------- #
# Interpolation profile log: the stage / per-map / per-parameter timing +
# memory tables, appended to CSV files after EVERY interpolation. The
# in-session tables (`.interp_stages()` & co) vanish with the session; these
# files are the record performance choices are made from. Sink resolution:
#   1. `get_profile_dir()` when set;
#   2. the operation log's directory (`get_log_file()`), when logging is on;
#   3. `<project>/logs/` when a project registry EXISTS on disk.
# No sink resolvable (plain library use, tests) = silent no-op, and a failed
# write must never fail the interpolation.
# --------------------------------------------------------------------------- #
.interp_profile_dir <- function() {
  d <- tryCatch(get_profile_dir(), error = function(e) "")
  if (!is.null(d) && nzchar(d)) return(d)
  lf <- tryCatch(get_log_file(), error = function(e) "")
  if (!is.null(lf) && nzchar(lf)) return(dirname(lf))
  rf <- tryCatch(get_registry_file(), error = function(e) "")
  if (!is.null(rf) && nzchar(rf) && file.exists(rf)) {
    return(file.path(dirname(rf), "logs"))
  }
  NULL
}

.interp_profile_append <- function(dir, file, df) {
  if (is.null(df) || !NROW(df)) return(invisible())
  f <- file.path(dir, file)
  suppressWarnings(
    utils::write.table(df, f, sep = ",", append = file.exists(f),
                       col.names = !file.exists(f), row.names = FALSE,
                       qmethod = "double"))
  invisible()
}

.interp_profile_log <- function(scen, mod, ondisk, wall) {
  tryCatch({
    if (startsWith(scen@name %||% "", ".")) return(invisible())
    dir <- .interp_profile_dir()
    if (is.null(dir)) return(invisible())
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE,
                                     showWarnings = FALSE)
    run_id <- paste0(format(Sys.time(), "%Y%m%d-%H%M%S"), "-", scen@name)
    m <- .en_mem()
    run <- data.frame(
      run_id = run_id,
      timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      scenario = scen@name,
      model = tryCatch(mod@name, error = function(e) ""),
      calendar = tryCatch(scen@settings@calendar@name, error = function(e) ""),
      ondisk = isTRUE(ondisk),
      dt_threads = tryCatch(data.table::getDTthreads(),
                            error = function(e) NA_integer_),
      total_secs = round(as.numeric(wall), 1),
      mem_mb = m$mem_mb,
      rss_mb = m$rss_mb,
      peak_rss_mb = m$peak_rss_mb,
      energyRt = as.character(utils::packageVersion("energyRt")),
      stringsAsFactors = FALSE)
    .interp_profile_append(dir, "interp_runs.csv", run)
    st <- .interp_stages()
    if (!is.null(st)) {
      st <- cbind(run_id = run_id, st, stringsAsFactors = FALSE)
      .interp_profile_append(dir, "interp_stages.csv", st)
    }
    mp <- .interp_map_table()
    if (!is.null(mp)) {
      mp <- cbind(run_id = run_id, mp, stringsAsFactors = FALSE)
      .interp_profile_append(dir, "interp_maps.csv", mp)
    }
    pp <- .interp_param_table()
    if (!is.null(pp)) {
      pp <- cbind(run_id = run_id, pp, stringsAsFactors = FALSE)
      .interp_profile_append(dir, "interp_params.csv", pp)
    }
    invisible()
  }, error = function(e) {
    warning("Interpolation profile log could not be written: ",
            conditionMessage(e), call. = FALSE)
    invisible()
  })
}

# Closing size + fold summary. The model_size() computation itself is timed as a
# stage (it scans every parameter's rows, which is slow on very large models).
.interp_footer <- function(scen, verbose) {
  if (!isTRUE(verbose)) return(invisible())
  .interp_step(verbose, "computing model-size summary")
  ms <- model_size(scen)              # the slow scan
  .interp_step_done(verbose)          # close it with its own elapsed time
  cli::cli_rule()
  print(ms)
  invisible()
}
