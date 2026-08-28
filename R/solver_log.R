# Solver progress to a file ----------------------------------------------------
#
# HiGHS writes its iteration log to stdout. A piped stdout is block-buffered, so
# in a non-interactive run the log appears only when the process exits and is not
# retained. Directing the solver at a file makes the log tailable during the
# solve and keeps it in the run directory alongside the generated model.

#' Send the solver's progress log to a file
#'
#' Adds a `log_file` attribute to a JuMP/HiGHS solver option set, so the solver
#' writes its iteration log where it can be watched and kept.
#'
#' @details
#' The log lands in the run's `solver/` directory, alongside the generated
#' model, so it can be followed while the solve runs:
#'
#' ```
#' tail -f <scenario>/runs/<solve>/solver/highs.log
#' ```
#'
#' Without it, HiGHS logs to stdout, which a piped R session buffers -- the
#' output appears only when the process ends, and nothing is retained.
#'
#' @param opts a solver option set, e.g. `solver_options$julia_highs_barrier`.
#' @param file log file name. A bare name is written into the run's `solver/`
#'   directory; an absolute path is used as given.
#'
#' @return `opts` with the attribute appended to its `inc3` block.
#'
#' @examples
#' \dontrun{
#' # barrier, with a followable log
#' opt <- with_solver_log(solver_options$julia_highs_barrier)
#' scen <- write_script(scen, solver = opt)
#' }
#' @seealso `solver_options` (the shipped solver option sets)
#' @family interpolation
#' @export
with_solver_log <- function(opts, file = "highs.log") {
  if (!is.list(opts) || is.null(opts$lang)) {
    stop("`opts` must be a solver option set, e.g. solver_options$julia_highs",
         call. = FALSE)
  }
  if (!identical(opts$lang, "JuMP")) {
    # Only the JuMP path writes an inc3 block of optimizer attributes. Saying so
    # is better than silently returning something that does nothing.
    warning("solver logging is only wired for JuMP/HiGHS; `", opts$name,
            "` uses ", opts$lang, " and is returned unchanged.", call. = FALSE)
    return(opts)
  }
  if (length(file) != 1L || !nzchar(file)) {
    stop("`file` must be a single non-empty name", call. = FALSE)
  }
  line <- sprintf(
    '\n# progress log (added by with_solver_log)\nset_optimizer_attribute(model, "log_file", "%s")\n',
    gsub("\\\\", "/", file))
  opts$inc3 <- paste0(paste(opts$inc3, collapse = "\n"), line)
  opts$name <- paste0(opts$name, "+log")
  opts
}
