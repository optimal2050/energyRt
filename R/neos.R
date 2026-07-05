# =============================================================================#
# neos.R  --  NEOS Server (https://neos-server.org) client  [Phase 1]
#
# NEOS is a *transport/execution backend*, not a modeling language: it runs
# commercial solvers (CPLEX/Gurobi/MOSEK/Xpress) for free for academic use.
# This file is a self-contained XML-RPC client (neos_ping / neos_list_* /
# neos_get_template) with NO dependency on the solve pipeline, so it is fully
# testable on its own. Job submission and the solve.R `backend == "neos"` branch
# come in later phases.
#
# Protocol: NEOS speaks XML-RPC over HTTPS at https://neos-server.org:3333.
# Auth is job-scoped (a (job, password) pair returned by submitJob); there is no
# API key. Built on httr2 + xml2 (both optional -> Suggests).
#
# LEGAL: commercial solvers via NEOS are academic/non-commercial ONLY, jobs are
# public/stored (no confidential data), ~3 GB / 8 h per job, cite NEOS in
# publications. The terms opt-in gate is a later phase; this file only talks to
# the API.
# =============================================================================#

.neos_endpoint <- function() {
  getOption("energyRt.neos_endpoint", "https://neos-server.org:3333")
}

#' NEOS submission email
#'
#' @description
#' Get / set the email address NEOS requires for job submission. `set_neos_email()`
#' stores it in the energyRt option **and** exports it to the `NEOS_EMAIL`
#' environment variable, so it is picked up by BOTH backends: the GAMS/NEOS
#' backend (R-side, [neos_submit_job()]) and the Pyomo/NEOS backend (which reads
#' `NEOS_EMAIL` from inside the python subprocess). `get_neos_email()` returns the
#' option, falling back to the `NEOS_EMAIL` environment variable, or `NULL`.
#'
#' @param email a valid email address (character), or `NULL` to clear.
#' @return `get_neos_email()` the email or `NULL`; `set_neos_email()` the email,
#'   invisibly.
#' @family solver
#' @examples
#' \dontrun{
#' set_neos_email("you@example.com")
#' get_neos_email()
#' }
#' @rdname neos_email
#' @export
get_neos_email <- function() {
  e <- options::opt("neos_email")
  if (is.null(e) || !nzchar(e)) NULL else e
}

#' @rdname neos_email
#' @export
set_neos_email <- function(email = NULL) {
  if (!is.null(email) && nzchar(email)) {
    Sys.setenv(NEOS_EMAIL = email)   # so the pyomo subprocess inherits it
  } else {
    Sys.unsetenv("NEOS_EMAIL")
  }
  options::opt_set("neos_email", email, env = "energyRt")
  invisible(email)
}

# Ensure the optional deps are present, with an actionable message.
.neos_check_deps <- function() {
  miss <- c("httr2", "xml2")[!vapply(c("httr2", "xml2"), requireNamespace,
                                     logical(1), quietly = TRUE)]
  if (length(miss)) {
    stop("The NEOS client needs package(s): ", paste(miss, collapse = ", "),
         '. Install with install.packages(c("', paste(miss, collapse = '", "'),
         '")).', call. = FALSE)
  }
}

# --- XML-RPC serialization ---------------------------------------------------
.neos_xml_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

# Serialize one R scalar to an XML-RPC <value>.
.neos_value_xml <- function(x) {
  if (is.character(x)) {
    paste0("<value><string>", .neos_xml_escape(x), "</string></value>")
  } else if (is.logical(x)) {
    paste0("<value><boolean>", as.integer(x), "</boolean></value>")
  } else if (is.integer(x) || (is.numeric(x) && x == round(x))) {
    paste0("<value><int>", format(as.integer(x), scientific = FALSE),
           "</int></value>")
  } else if (is.numeric(x)) {
    paste0("<value><double>", format(x, scientific = FALSE), "</double></value>")
  } else {
    stop("Unsupported XML-RPC param type: ", class(x)[1], call. = FALSE)
  }
}

.neos_build_call <- function(method, params = list()) {
  body <- vapply(params, function(p) {
    paste0("<param>", .neos_value_xml(p), "</param>")
  }, character(1))
  paste0('<?xml version="1.0"?><methodCall><methodName>', method,
         "</methodName><params>", paste(body, collapse = ""),
         "</params></methodCall>")
}

# --- XML-RPC response parsing ------------------------------------------------
# Parse a <value> node into an R object (recursive for array/struct).
.neos_parse_value <- function(value_node) {
  child <- xml2::xml_find_first(value_node, "./*")
  if (inherits(child, "xml_missing")) {
    return(xml2::xml_text(value_node))       # bare <value>text</value> == string
  }
  tag <- xml2::xml_name(child)
  switch(tag,
    string  = xml2::xml_text(child),
    int     = as.integer(xml2::xml_text(child)),
    i4      = as.integer(xml2::xml_text(child)),
    double  = as.numeric(xml2::xml_text(child)),
    boolean = xml2::xml_text(child) == "1",
    base64  = xml2::xml_text(child),         # raw base64; decode by caller
    array   = lapply(
      xml2::xml_find_all(child, "./data/value"), .neos_parse_value),
    struct  = {
      members <- xml2::xml_find_all(child, "./member")
      out <- lapply(members, function(m)
        .neos_parse_value(xml2::xml_find_first(m, "./value")))
      names(out) <- vapply(members, function(m)
        xml2::xml_text(xml2::xml_find_first(m, "./name")), character(1))
      out
    },
    xml2::xml_text(child)
  )
}

# Perform an XML-RPC call and return the parsed result (stops on <fault>).
.neos_call <- function(method, params = list(), timeout = 60) {
  .neos_check_deps()
  body <- .neos_build_call(method, params)
  resp <- tryCatch(
    httr2::request(.neos_endpoint()) |>
      httr2::req_body_raw(body, type = "text/xml") |>
      httr2::req_timeout(timeout) |>
      httr2::req_user_agent("energyRt (https://github.com/optimal2050/energyRt)") |>
      httr2::req_perform(),
    error = function(e)
      stop("NEOS request failed (", method, "): ", conditionMessage(e),
           call. = FALSE)
  )
  doc <- xml2::read_xml(httr2::resp_body_string(resp))
  fault <- xml2::xml_find_first(doc, "/methodResponse/fault/value")
  if (!inherits(fault, "xml_missing")) {
    f <- .neos_parse_value(fault)
    stop("NEOS fault: ", f[["faultString"]] %||% "unknown",
         " (code ", f[["faultCode"]] %||% NA, ")", call. = FALSE)
  }
  val <- xml2::xml_find_first(doc, "/methodResponse/params/param/value")
  .neos_parse_value(val)
}

# --- Public Phase-1 API ------------------------------------------------------

#' NEOS Server client (query the remote solver service)
#'
#' @description
#' Read-only queries against the NEOS Server XML-RPC API
#' (\url{https://neos-server.org}). These let you check connectivity and
#' discover which solver/input-method combinations are available before wiring
#' NEOS in as a solver backend. No account or API key is required.
#'
#' @details
#' NEOS provides commercial solvers free of charge **for academic /
#' non-commercial use only**, submitted jobs are **public and stored** (do not
#' send confidential data), and jobs are limited to roughly 3 GB RAM / 8 h.
#' Please cite NEOS in publications.
#'
#' @param category NEOS solver category abbreviation, e.g. `"milp"`, `"lp"`,
#'   `"nco"` (see [neos_list_categories()]).
#' @param solver NEOS solver name, e.g. `"CPLEX"`, `"Gurobi"` (see
#'   [neos_list_solvers()]).
#' @param inputMethod input format, e.g. `"GAMS"`, `"MPS"`, `"AMPL"`.
#' @param timeout request timeout in seconds.
#'
#' @return
#' - `neos_ping()`: a status string (invisibly), `TRUE` if the server is alive.
#' - `neos_list_categories()`: a named character vector (abbrev -> full name).
#' - `neos_list_solvers()`: a character vector of `solver:inputMethod` strings.
#' - `neos_get_template()`: the XML job template as a single string.
#' @name neos
#' @family solver
#' @examples
#' \dontrun{
#' neos_ping()
#' head(neos_list_categories())
#' neos_list_solvers("milp")
#' cat(neos_get_template("milp", "CPLEX", "GAMS"))
#' }
NULL

#' @rdname neos
#' @export
neos_ping <- function(timeout = 30) {
  msg <- .neos_call("ping", timeout = timeout)
  alive <- grepl("alive", msg, ignore.case = TRUE)
  if (alive) message("NEOS: ", trimws(msg)) else
    warning("NEOS ping unexpected response: ", msg)
  invisible(alive)
}

#' @rdname neos
#' @export
neos_list_categories <- function(timeout = 30) {
  res <- .neos_call("listCategories", timeout = timeout)
  unlist(res)   # struct: abbrev -> full name
}

#' @rdname neos
#' @export
neos_list_solvers <- function(category, timeout = 30) {
  res <- .neos_call("listSolversInCategory", list(category), timeout = timeout)
  unlist(res)
}

#' @rdname neos
#' @export
neos_get_template <- function(category, solver, inputMethod = "GAMS",
                              timeout = 30) {
  .neos_call("getSolverTemplate",
             list(category, solver, inputMethod), timeout = timeout)
}

# =============================================================================#
# Phase 2: job submission, polling, and result retrieval.
#
# A submitted job is identified by a (job number, password) pair returned by
# submitJob. NB: jobs submitted to NEOS are PUBLIC and STORED, and commercial
# solvers are for academic/non-commercial use only. Callers must supply a real
# email and accept the terms (opt-in gate lives in a later phase).
# =============================================================================#

.neos_need_base64 <- function() {
  if (!requireNamespace("base64enc", quietly = TRUE)) {
    stop("NEOS result retrieval needs 'base64enc'. ",
         'install.packages("base64enc").', call. = FALSE)
  }
}

# Encode a binary input file (e.g. a GDX) for the NEOS XML-RPC <gdx> field. The
# content goes inside a nested <base64> tag: <gdx><base64>...</base64></gdx>.
# NEOS base64-decodes that tag to produce `in.gdx`; it does NOT gunzip (unlike
# the AMPL <nlfile> path, which is gzipped) -- so base64 the raw bytes directly.
.neos_encode_binary <- function(path) {
  .neos_need_base64()
  base64enc::base64encode(path)
}

# Build a GAMS <document> job XML from the template fields confirmed live via
# getSolverTemplate("milp","CPLEX","GAMS"). `wantgdx`/`wantlst`/`wantlog` request
# the corresponding outputs back from NEOS; retrieve them with
# neos_get_output_file() / neos_final_results().
#' Assemble a NEOS GAMS job document
#'
#' @param model character. The GAMS model source (contents of a `.gms` file).
#' @param email character. A valid email (required by NEOS).
#' @param options,parameters character. GAMS options / double-dash parameters.
#' @param gdx character. Base64-of-gzip of an *input* GDX file; emitted as
#'   `<gdx><base64>...</base64></gdx>`. Empty for no input gdx.
#' @param wantgdx,wantlst,wantlog character. Non-empty to request GDX / listing /
#'   log output back. Default requests GDX + listing.
#' @param comments character. Free-text comment stored with the job.
#' @param category,solver NEOS category/solver (default `milp`/`CPLEX`).
#' @return A single XML string suitable for [neos_submit_job()].
#' @family solver
#' @export
neos_build_gams_xml <- function(model, email, options = "", parameters = "",
                                gdx = "", wantgdx = "yes", wantlst = "yes",
                                wantlog = "", comments = "energyRt",
                                category = "milp", solver = "CPLEX") {
  cdata <- function(x) paste0("<![CDATA[", x, "]]>")
  paste0(
    "<document>\n",
    "<category>", category, "</category>\n",
    "<solver>", solver, "</solver>\n",
    "<inputMethod>GAMS</inputMethod>\n",
    "<email>", email, "</email>\n",
    "<model>", cdata(model), "</model>\n",
    "<options>", cdata(options), "</options>\n",
    "<parameters>", cdata(parameters), "</parameters>\n",
    if (nzchar(gdx)) paste0("<gdx><base64>", gdx, "</base64></gdx>\n") else "<gdx></gdx>\n",
    "<wantgdx>", cdata(wantgdx), "</wantgdx>\n",
    "<wantlst>", cdata(wantlst), "</wantlst>\n",
    "<wantlog>", cdata(wantlog), "</wantlog>\n",
    "<comments>", cdata(comments), "</comments>\n",
    "</document>")
}

# --- Text-data (no-GDX) GAMS jobs --------------------------------------------
# NEOS runs a GAMS job from a single uploaded <model> plus an OPTIONAL input GDX.
# energyRt's GAMS writer, when NOT using a gdx (the default when set_gdxlib_path()
# is unset, i.e. export_format != "gdx"), emits the data as plain-text
# `$include`d .gms files. Inlining those includes yields ONE self-contained .gms
# that carries model AND data as text -- so a job can be submitted with an EMPTY
# <gdx> and WITHOUT a local GAMS / gdx library. Text is bulkier than a gdx, so
# watch the ~16 MB NEOS job-input cap for large models (sample / prune first).

#' Inline a GAMS model's `$include` files into one self-contained string
#'
#' @description
#' Recursively splices the contents of every `$include "file"` / `$batinclude`
#' directive in `main` (resolved against `base_dir`) into a single GAMS source
#' string. Use it to bundle a *text-mode* energyRt GAMS scenario (model plus the
#' `$include`d text data files) for submission to NEOS with **no input GDX** and
#' no local GAMS install.
#'
#' @param main path to the top GAMS file (e.g. `energyRt.gms`).
#' @param base_dir directory includes are resolved against (default: dir of `main`).
#' @param flatten logical; strip the `input/` and `output/` path prefixes so the
#'   job runs in NEOS's flat workspace (default `TRUE`).
#' @return a single character string: the fully inlined GAMS source.
#' @family solver
#' @export
neos_gams_inline <- function(main, base_dir = dirname(main), flatten = TRUE) {
  inc_re <- "^\\s*\\$(bat)?include\\s+\"?([^\"[:space:]]+)\"?"
  splice <- function(file) {
    lines <- readLines(file, warn = FALSE)
    out <- character(0)
    for (ln in lines) {
      m <- regmatches(ln, regexec(inc_re, ln, ignore.case = TRUE, perl = TRUE))[[1]]
      inc <- if (length(m) >= 3) file.path(base_dir, m[3]) else NA_character_
      if (!is.na(inc) && file.exists(inc)) {
        out <- c(out, splice(inc))       # recurse into the included file
      } else {
        out <- c(out, ln)
      }
    }
    out
  }
  src <- paste(splice(main), collapse = "\n")
  if (isTRUE(flatten)) {
    src <- gsub("output/", "", src, fixed = TRUE)
    src <- gsub("input/",  "", src, fixed = TRUE)
  }
  src
}

#' Build a NEOS GAMS job from a text-mode scenario directory (no input GDX)
#'
#' @description
#' Convenience wrapper: inline the model + text data of a *written* GAMS scenario
#' directory and assemble a job document with an **empty** `<gdx>`. The scenario
#' must have been written WITHOUT a gdx (i.e. `set_gdxlib_path()` unset), so its
#' data lives in text `$include`d `.gms` files. This is the "text data" path: no
#' gdx encoding and no local GAMS/gdx library needed to submit.
#'
#' @param dir the scenario's GAMS working directory (holds `energyRt.gms`).
#' @param main name of the top GAMS file in `dir`.
#' @inheritParams neos_build_gams_xml
#' @return an XML job document string for [neos_submit_job()].
#' @seealso [neos_build_gams_xml()] for the GDX-input path.
#' @family solver
#' @export
neos_build_gams_text_job <- function(dir, email, main = "energyRt.gms",
                                     solver = "CPLEX", category = "milp",
                                     options = "", parameters = "",
                                     wantgdx = "yes", wantlst = "yes",
                                     comments = "energyRt (text data)") {
  model <- neos_gams_inline(file.path(dir, main), dir)
  neos_build_gams_xml(model = model, email = email, options = options,
                      parameters = parameters, gdx = "",
                      wantgdx = wantgdx, wantlst = wantlst,
                      comments = comments, category = category, solver = solver)
}

#' Submit and manage a NEOS job
#'
#' @description
#' Low-level wrappers over the NEOS job API. `neos_submit_job()` submits a job
#' document and returns the `(job, password)` handle; the others use that handle
#' to poll status and retrieve results. **Submitting a job sends the model to a
#' public service** --- see the notes in [neos].
#'
#' @param xml character. A job document (see [neos_build_gams_xml()]).
#' @param user,password NEOS account credentials for `authenticatedSubmitJob`;
#'   if `NULL`, an anonymous `submitJob` is used.
#' @param job integer job number, and `pw` its password, from `neos_submit_job()`.
#' @param fileName name of an output file to fetch (e.g. `"soln.gdx"`).
#' @param poll,max_wait polling interval / overall wait cap in seconds.
#' @param verbose print status while waiting.
#' @param timeout per-request timeout in seconds.
#' @return
#' - `neos_submit_job()`: list(`job`, `password`).
#' - `neos_job_status()` / `neos_completion_code()`: a status string.
#' - `neos_final_results()`: combined solver output as text.
#' - `neos_get_output_file()`: raw bytes (e.g. GDX) --- write with `writeBin()`.
#' - `neos_wait()`: final status (invisibly) once `"Done"`.
#' @name neos_job
#' @family solver
NULL

#' @rdname neos_job
#' @export
neos_submit_job <- function(xml, user = NULL, password = NULL, timeout = 600) {
  # submitJob uploads the whole job (model + any base64 input gdx), which can be
  # tens of MB -> generous default timeout so large uploads don't time out.
  res <- if (!is.null(user) && !is.null(password)) {
    .neos_call("authenticatedSubmitJob", list(xml, user, password), timeout)
  } else {
    .neos_call("submitJob", list(xml), timeout)
  }
  job <- as.integer(res[[1]])
  pw  <- as.character(res[[2]])
  if (is.na(job) || job == 0L) {
    stop("NEOS rejected the job (job number 0). Message: ", pw, call. = FALSE)
  }
  list(job = job, password = pw)
}

#' @rdname neos_job
#' @export
neos_job_status <- function(job, pw, timeout = 30) {
  .neos_call("getJobStatus", list(as.integer(job), pw), timeout = timeout)
}

#' @rdname neos_job
#' @export
neos_completion_code <- function(job, pw, timeout = 30) {
  .neos_call("getCompletionCode", list(as.integer(job), pw), timeout = timeout)
}

#' @rdname neos_job
#' @export
neos_final_results <- function(job, pw, timeout = 120) {
  .neos_need_base64()
  b64 <- .neos_call("getFinalResults", list(as.integer(job), pw), timeout)
  rawToChar(base64enc::base64decode(b64))
}

#' @rdname neos_job
#' @export
neos_get_output_file <- function(job, pw, fileName, timeout = 120) {
  .neos_need_base64()
  b64 <- .neos_call("getOutputFile",
                    list(as.integer(job), pw, fileName), timeout)
  base64enc::base64decode(b64)   # raw bytes; caller writeBin()s to disk
}

#' @rdname neos_job
#' @export
neos_wait <- function(job, pw, poll = 5, max_wait = 600, verbose = TRUE) {
  waited <- 0
  repeat {
    st <- neos_job_status(job, pw)
    if (verbose) message(sprintf("NEOS job %d: %s (%ds)", as.integer(job), st, waited))
    if (identical(st, "Done")) return(invisible(st))
    if (waited >= max_wait) {
      stop("NEOS wait timed out after ", max_wait, "s (last status: ", st, ")",
           call. = FALSE)
    }
    Sys.sleep(poll)
    waited <- waited + poll
  }
}
