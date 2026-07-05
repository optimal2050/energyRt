#' Collect the regions an object operates in
#'
#' Generic, reflective accessor that walks every slot of an S4 model object
#' (except `misc`) and gathers the regions it refers to. Regions are read from:
#'   * any atomic slot named `region`, `src`, or `dst`, and
#'   * the `region`, `src`, and `dst` columns of any `data.frame` slot.
#'
#' The result is the set of unique, non-missing, non-empty region labels. The
#' function is intentionally schema-agnostic so it keeps working as region
#' information is added to classes that do not yet carry an explicit `@region`
#' slot (e.g. `import` / `export`).
#'
#' @param obj a model object (S4) such as `technology`, `storage`, `trade`,
#'   `import`, or `export`.
#'
#' @returns a character vector of region labels (possibly empty).
#'
#' @family model
#' @export
get_region <- function(obj) {
  if (!isS4(obj)) {
    return(character(0))
  }
  keys <- c("region", "src", "dst")
  out <- character(0)
  for (sn in methods::slotNames(obj)) {
    if (identical(sn, "misc")) next
    v <- methods::slot(obj, sn)
    if (is.data.frame(v)) {
      for (cc in intersect(keys, colnames(v))) {
        out <- c(out, as.character(v[[cc]]))
      }
    } else if (is.atomic(v) && sn %in% keys) {
      out <- c(out, as.character(v))
    }
  }
  out <- out[!is.na(out) & nzchar(out)]
  unique(out)
}

# Guard: error if any region referenced in the model's objects is not declared.
# `model@data` is a list of repositories (or bare objects); regions are gathered
# reflectively via `get_region()` (covers region / src / dst, atomic or
# data.frame). NA / "" entries are wildcards ("all declared regions") and are
# ignored. Run early in `interp_mod()` so a stray region fails fast with a clear
# message rather than as an out-of-domain error in a solver writer.
.check_declared_regions <- function(model, declared) {
  declared <- as.character(declared)
  declared <- declared[!is.na(declared) & nzchar(declared)]
  used <- character(0)
  for (rp in model@data) {
    objs <- if (methods::is(rp, "repository")) rp@data else list(rp)
    for (o in objs) used <- c(used, get_region(o))
  }
  undeclared <- setdiff(unique(used), declared)
  if (length(undeclared) > 0) {
    stop(
      "The model references undeclared region(s): ",
      paste(sort(undeclared), collapse = ", "),
      ".\nDeclare them in the model's regions (config/settings) or remove them ",
      "from the affected objects' data.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}
