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
