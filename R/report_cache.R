# =============================================================================#
# report_cache.R — output locations and skip-if-current for report().
#
# A rendered report is a pure function of (object content, template content,
# render/param arguments, image, levcost identity). The key over those parts
# is stored in a `<file_base>.report.yml` sidecar next to the rendered files;
# a format is re-rendered only when its file is missing or the key changed.
#
# Location: reports of SAVED objects live inside the owning folder
# (`<scenario>/reports/`, `models/<n>@<h8>/reports/`); reports of in-memory
# objects are temporary and land in the project-level `get_reports_path()`
# folder (default `reports/`). An explicit `file =` always wins verbatim and
# still gets a sidecar, so skip-if-current works there too.
# =============================================================================#

# Fallback order: file= > reports_path= arg > owning folder (saved objects
# only) > the reports_path option (temporary tier).
.report_output_base <- function(owner, file, reports_path, stub) {
  stub <- paste0("report_", gsub("[^A-Za-z0-9_]", "_", stub))
  base <- if (!is.null(file)) {
    tools::file_path_sans_ext(file)
  } else if (!is.null(reports_path)) {
    fp(reports_path, stub)
  } else {
    own <- if (!is.null(owner)) .object_store_dir(owner) else NULL
    if (!is.null(own)) fp(own, "reports", stub) else fp(get_reports_path(), stub)
  }
  dir_ <- dirname(base)
  if (!dir.exists(dir_)) dir.create(dir_, recursive = TRUE)
  normalizePath(base, mustWork = FALSE)
}

# The report content key. `args` are the user-supplied knobs (template params,
# render args, cost_unit, ...) normalized like the levcost key; the template
# enters by CONTENT so editing an Rmd re-renders; an image enters by content;
# `levcost_id` is the levcost identity (a cache key or a result hash), NULL
# when the report has no levcost section.
.report_key <- function(engine, object, tmpl, args = list(),
                        image_file = NULL, levcost_id = NULL) {
  a <- if (length(args)) args[order(names(args))] else list()
  norm_one <- function(v) {
    if (isS4(v)) return(object_hash(v))
    if (is.list(v) && !is.data.frame(v)) return(lapply(v, norm_one))
    v
  }
  tmpl_hash <- tryCatch(
    rlang::hash(readChar(tmpl, file.size(tmpl), useBytes = TRUE)),
    error = function(e) tmpl)
  img_hash <- if (!is.null(image_file) && file.exists(image_file)) {
    rlang::hash(readBin(image_file, "raw", file.size(image_file)))
  } else NULL
  rlang::hash(list(engine = engine, ver = 1L, object = object_hash(object),
                   template = tmpl_hash, args = lapply(a, norm_one),
                   image = img_hash, levcost = levcost_id))
}

.report_sidecar_path <- function(file_base) paste0(file_base, ".report.yml")

.report_sidecar_read <- function(file_base) {
  p <- .report_sidecar_path(file_base)
  if (!file.exists(p)) return(NULL)
  tryCatch(yaml::read_yaml(p), error = function(e) NULL)
}

# TRUE when this format's file exists and the sidecar records the same key.
.report_uptodate <- function(file_base, fmt, key) {
  side <- .report_sidecar_read(file_base)
  if (is.null(side) || !identical(side$key, key)) return(FALSE)
  f <- side$files[[fmt]]
  !is.null(f) && file.exists(fp(dirname(file_base), f))
}

.report_sidecar_write <- function(file_base, key, meta = list(),
                                  files = list()) {
  old <- .report_sidecar_read(file_base)
  # merge: files rendered earlier under the SAME key stay listed
  if (!is.null(old) && identical(old$key, key)) {
    files <- utils::modifyList(old$files %||% list(), files)
  }
  side <- c(list(kind = "report", version = 1L, key = key), meta,
            list(files = files,
                 created = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                 energyRt_version =
                   as.character(utils::packageVersion("energyRt"))))
  tryCatch(yaml::write_yaml(side, .report_sidecar_path(file_base)),
           error = function(e) {
             warning("report: could not write the sidecar: ",
                     conditionMessage(e), call. = FALSE)
           })
  invisible(side)
}

# The levcost identity term of a report key WITHOUT running levcost: the
# cache key an auto-run would use (so an up-to-date report short-circuits
# before levcost runs), or a content hash of a ready result.
.levcost_call_key <- function(object, lc_dots) {
  .levcost_cache_key(tolower(class(object)[1]), object, lc_dots)
}

.levcost_result_id <- function(levcost) {
  rlang::hash(.levcost_strip_scenario(levcost))
}

# -- removing rendered reports --------------------------------------------- #

# The report groups in a folder: one per sidecar, plus the files no sidecar
# claims. `reports/` is FLAT -- `<base>.html`, `<base>.pdf`, `<base>.report.yml`
# side by side -- so a report is identified by its sidecar, not by a directory.
.report_groups <- function(dir) {
  ff <- list.files(dir, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  ff <- ff[!dir.exists(ff)]
  side <- ff[endsWith(ff, ".report.yml")]
  rows <- list()
  claimed <- character(0)
  for (sp in side) {
    # suffix strip, no regex: the base name may hold regex metacharacters
    base <- substr(sp, 1L, nchar(sp) - nchar(".report.yml"))
    mf <- tryCatch(yaml::read_yaml(sp), error = function(e) NULL)
    files <- c(sp, ff[startsWith(ff, paste0(base, "."))])
    files <- unique(files)
    claimed <- c(claimed, files)
    rows[[length(rows) + 1L]] <- tibble(
      report = basename(base),
      formats = paste(sort(names(mf$files %||% list())), collapse = ", "),
      size_mb = round(sum(file.size(files), na.rm = TRUE) / 1024^2, 3),
      created = as.character(mf$created %||% ""),
      path = gsub("[\\/]+", "/", base))
  }
  loose <- setdiff(ff, claimed)
  if (length(loose)) {
    rows[[length(rows) + 1L]] <- tibble(
      report = NA_character_, formats = "",
      size_mb = round(sum(file.size(loose), na.rm = TRUE) / 1024^2, 3),
      created = "",
      path = gsub("[\\/]+", "/", dir))
  }
  attr(rows, "loose") <- loose
  attr(rows, "claimed") <- claimed
  rows
}

#' Delete rendered reports
#'
#' @description
#' Removes the reports [report()] rendered for an object — the files and their
#' `.report.yml` sidecars. **Dry-run by default.** Reports are derived
#' artifacts: the sidecar keys them on the object content, the template and the
#' render arguments, so anything removed here re-renders on demand.
#'
#' @details
#' Where it looks, matching where [report()] writes: an explicit directory
#' path; otherwise the owning folder's `reports/` for a saved object
#' (`<scenario>/reports/`, `models/<name>/reports/`); otherwise the
#' project-level [get_reports_path()] tier, which is where reports of
#' in-memory objects land.
#'
#' A sealed owner refuses — a sealed entry is an archive, and its reports are
#' part of what was archived.
#'
#' @param x an energyRt object whose reports should be removed, or a character
#'   path to a reports directory. `NULL` (default) targets the project-level
#'   tier.
#' @param dry_run logical. `TRUE` (default) lists what would go without
#'   removing it.
#' @param verbose logical.
#'
#' @return a tibble of report groups (`report`, `formats`, `size_mb`,
#'   `created`, `action`, `path`), visible on a dry run and invisible
#'   otherwise. Files no sidecar claims are listed with `report = NA` and are
#'   removed only when a whole directory is cleared.
#' @seealso [clear_levcost_cache()], [scenario_artifacts()]
#' @examples
#' \dontrun{
#' clear_report_cache(scen)                    # what would go
#' clear_report_cache(scen, dry_run = FALSE)   # remove them
#' }
#' @export
clear_report_cache <- function(x = NULL, dry_run = TRUE, verbose = TRUE) {
  empty <- tibble(report = character(0), formats = character(0),
                  size_mb = numeric(0), created = character(0),
                  action = character(0), path = character(0))
  dir <- if (is.character(x)) {
    gsub("[\\/]+", "/", x)
  } else {
    own <- if (!is.null(x)) .object_store_dir(x) else NULL
    if (!is.null(own)) fp(own, "reports") else get_reports_path()
  }
  if (isS4(x) && !is.null(.object_store_dir(x))) {
    tp <- if (is(x, "scenario")) "scenario" else
      if (is(x, "model")) "model" else
      if (is(x, "repository")) "repository" else NULL
    if (!is.null(tp)) {
      .seal_guard(.entry_manifest(tp, .object_store_dir(x)), tp, x@name,
                  .entry_kinds()[[tp]]$unseal)
    }
  }
  if (is.null(dir) || !dir.exists(dir)) {
    if (isTRUE(verbose)) message("No reports in '", dir %||% "?", "'.")
    return(if (dry_run) empty else invisible(empty))
  }

  grp <- .report_groups(dir)
  out <- if (length(grp)) bind_rows(grp) else empty
  if (!nrow(out)) {
    if (isTRUE(verbose)) message("No reports in '", dir, "'.")
    return(if (dry_run) empty else invisible(empty))
  }
  out$action <- if (dry_run) "would_delete" else "deleted"
  out <- out[, c("report", "formats", "size_mb", "created", "action", "path")]

  if (!dry_run) {
    files <- c(attr(grp, "claimed"), attr(grp, "loose"))
    unlink(files, force = TRUE)
    # a folder that held nothing else goes with them
    if (!length(list.files(dir, all.files = TRUE, no.. = TRUE))) {
      unlink(dir, recursive = TRUE, force = TRUE)
    }
  }
  if (isTRUE(verbose)) {
    mb <- round(sum(out$size_mb, na.rm = TRUE), 2)
    if (dry_run) {
      message("Dry run - nothing deleted. ", nrow(out), " report group",
              if (nrow(out) == 1L) "" else "s", " in '", dir, "' (", mb,
              " MB); clear_report_cache(dry_run = FALSE) to remove.")
    } else {
      message("Removed ", nrow(out), " report group",
              if (nrow(out) == 1L) "" else "s", " (", mb, " MB) from '",
              dir, "'.")
    }
  }
  if (dry_run) out else invisible(out)
}
