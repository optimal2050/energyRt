# =========================================================================== #
# compare_models.R -- declaration-level diff of two models (or repositories):
# which objects were added / removed / changed (by content hash), config
# differences, and an internal slot-by-slot drill-down for changed objects.
# =========================================================================== #

#' Compare two models' contents
#'
#' Diffs two models (or repositories) at the declaration level: every stored
#' object (technologies, commodities, supply, demand, ...) is matched by
#' name and compared by content hash ([object_hash()], which ignores
#' `@misc` bookkeeping), yielding an added / removed / changed / same
#' inventory. For models, the configuration (regions, horizon, calendar,
#' discount rates, `optimizeRetirement`) is compared as well. With
#' `details = TRUE` every changed object also gets a recursive slot-by-slot
#' drill-down, printable per object via `print(x, name = "...")`.
#'
#' @param a,b `model` or `repository` objects.
#' @param names character(2), display names of the two sides.
#' @param tol numeric tolerance for the drill-down's data comparisons.
#' @param details logical, compute the per-object slot drill-down for
#'   changed objects.
#'
#' @return A `models_cmp` object (a list): `inventory` (`name, class,
#'   hash_a, hash_b, status`), `config` (`item, A, B, differs`; `NULL` for
#'   repositories), `details` (named list of slot diffs for changed
#'   objects), `counts` (status table), `names`.
#'
#' @seealso [compare_scenarios()], [compare_inputs()], [object_hash()].
#' @family comparison
#' @export
compare_models <- function(a, b, names = c("A", "B"), tol = 1e-8,
                           details = TRUE) {
  objs <- function(x) {
    if (methods::is(x, "model") || methods::is(x, "repository")) {
      d <- x@data
      # a model's @data may hold repositories: flatten one level
      out <- list()
      for (el in d) {
        if (methods::is(el, "repository")) out <- c(out, el@data)
        else out <- c(out, list(el))
      }
      names(out) <- vapply(out, function(z)
        tryCatch(z@name, error = function(e) ""), "")
      out
    } else {
      stop("compare_models(): inputs must be 'model' or 'repository' ",
           "objects; got ", class(x)[1])
    }
  }
  oa <- objs(a)
  ob <- objs(b)

  all_nm <- union(base::names(oa), base::names(ob))
  inv <- lapply(all_nm, function(nm) {
    xa <- oa[[nm]]
    xb <- ob[[nm]]
    ha <- if (!is.null(xa)) tryCatch(object_hash(xa),
                                     error = function(e) NA_character_)
      else NA_character_
    hb <- if (!is.null(xb)) tryCatch(object_hash(xb),
                                     error = function(e) NA_character_)
      else NA_character_
    status <- if (is.null(xa)) "added"
      else if (is.null(xb)) "removed"
      else if (identical(ha, hb) && !is.na(ha)) "same"
      else "changed"
    data.frame(name = nm,
               class = class((xa %||% xb))[1],
               hash_a = substr(ha, 1, 8), hash_b = substr(hb, 1, 8),
               status = status, stringsAsFactors = FALSE)
  })
  inventory <- do.call(rbind, inv)
  rownames(inventory) <- NULL

  # -- config (models only) ------------------------------------------------
  config <- NULL
  if (methods::is(a, "model") && methods::is(b, "model")) {
    fmt <- function(x) paste(format(x), collapse = ", ")
    cfg_val <- function(m, what) {
      cfg <- m@config
      switch(what,
        regions = fmt(sort(cfg@region)),
        horizon = tryCatch(paste0(paste(range(cfg@horizon@period),
                                        collapse = " - "), " (",
                                  nrow(cfg@horizon@intervals),
                                  " intervals)"), error = function(e) ""),
        calendar = tryCatch(paste0(
          if (nzchar(cfg@calendar@name)) cfg@calendar@name else "(unnamed)",
          " (", nrow(cfg@calendar@timeslice_share), " timeslices)"),
          error = function(e) ""),
        discount = tryCatch({
          d <- cfg@discount
          if (is.data.frame(d) && nrow(d) > 0) {
            paste(vapply(intersect(c("wacc", "sdr"), base::names(d)),
                         function(cc) paste0(cc, "=", fmt(unique(
                           stats::na.omit(d[[cc]])))), ""),
                  collapse = "; ")
          } else ""
        }, error = function(e) ""),
        discountFirstYear = fmt(tryCatch(cfg@discountFirstYear,
                                         error = function(e) "")),
        optimizeRetirement = as.character(isTRUE(cfg@optimizeRetirement)))
    }
    items <- c("regions", "horizon", "calendar", "discount",
               "discountFirstYear", "optimizeRetirement")
    config <- data.frame(
      item = items,
      A = vapply(items, function(w) cfg_val(a, w), ""),
      B = vapply(items, function(w) cfg_val(b, w), ""),
      stringsAsFactors = FALSE)
    config$differs <- config$A != config$B
  }

  det <- list()
  if (isTRUE(details)) {
    for (nm in inventory$name[inventory$status == "changed"]) {
      det[[nm]] <- tryCatch(.compare_slots(oa[[nm]], ob[[nm]], tol = tol),
                            error = function(e)
                              list(error = conditionMessage(e)))
    }
  }

  structure(list(
    names = names, inventory = inventory, config = config,
    details = det, counts = table(inventory$status)
  ), class = "models_cmp")
}

# Recursive slot-by-slot diff of two objects; NULL when identical, else a
# nested list holding only the differing branches. data.frame leaves render
# through waldo (order-insensitive after .ci_norm) when it is installed,
# otherwise an all.equal verdict with both values.
#' @noRd
.compare_slots <- function(x, y, tol = 1e-8) {
  if (!identical(class(x), class(y))) {
    return(list(class_a = class(x)[1], class_b = class(y)[1]))
  }
  if (identical(x, y)) return(NULL)
  if (isS4(x)) {
    out <- list()
    for (s in methods::slotNames(x)) {
      d <- .compare_slots(methods::slot(x, s), methods::slot(y, s),
                          tol = tol)
      if (!is.null(d)) out[[s]] <- d
    }
    return(if (length(out) > 0) out else NULL)
  }
  if (is.data.frame(x)) {
    a <- .ci_norm(x)
    b <- .ci_norm(y)
    if (isTRUE(all.equal(a, b, tolerance = tol,
                         check.attributes = FALSE))) return(NULL)
    if (requireNamespace("waldo", quietly = TRUE)) {
      return(waldo::compare(a, b, tolerance = tol))
    }
    return(list(a = x, b = y))
  }
  if (is.list(x)) {
    nms <- union(base::names(x), base::names(y))
    if (is.null(nms)) {
      return(if (isTRUE(all.equal(x, y, tolerance = tol,
                                  check.attributes = FALSE))) NULL else
        list(a = x, b = y))
    }
    out <- list()
    for (nm in nms) {
      d <- .compare_slots(x[[nm]], y[[nm]], tol = tol)
      if (!is.null(d)) out[[nm]] <- d
    }
    return(if (length(out) > 0) out else NULL)
  }
  if (is.numeric(x) && length(x) == length(y) &&
      isTRUE(all.equal(x, y, tolerance = tol, check.attributes = FALSE))) {
    return(NULL)
  }
  list(a = x, b = y)
}

#' @method print models_cmp
#' @param x a `models_cmp` object.
#' @param name character, a changed object's name: print its slot-by-slot
#'   drill-down instead of the summary.
#' @param ... ignored.
#' @rdname compare_models
#' @export
print.models_cmp <- function(x, name = NULL, ...) {
  nm <- x$names
  if (!is.null(name)) {
    if (is.null(x$details[[name]])) {
      cat("No stored detail for '", name, "' (not changed, or ",
          "details = FALSE).\n", sep = "")
    } else {
      cat("slot differences for '", name, "' (", nm[1], " vs ", nm[2],
          "):\n", sep = "")
      print(x$details[[name]])
    }
    return(invisible(x))
  }
  cat("model comparison: ", nm[1], " vs ", nm[2], "\n", sep = "")
  cnt <- x$counts
  cat(paste(paste0(names(cnt), ": ", as.integer(cnt)), collapse = "  "),
      "\n")
  chg <- x$inventory[x$inventory$status != "same", , drop = FALSE]
  if (nrow(chg) > 0) {
    cat("\n--- Objects ---\n")
    print(chg, row.names = FALSE)
  } else {
    cat("\nAll objects identical.\n")
  }
  if (!is.null(x$config)) {
    cd <- x$config[x$config$differs, , drop = FALSE]
    if (nrow(cd) > 0) {
      cat("\n--- Configuration ---\n")
      print(cd[, c("item", "A", "B")], row.names = FALSE)
    } else {
      cat("Configuration: identical.\n")
    }
  }
  if (length(x$details) > 0) {
    cat("\nDrill down with print(x, name = \"",
        x$inventory$name[x$inventory$status == "changed"][1],
        "\")\n", sep = "")
  }
  invisible(x)
}
