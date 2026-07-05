# report.R
# Summary reports for energyRt S4 objects in PDF, HTML, or LaTeX format.
# Requires: rmarkdown, ggplot2; PDF/TeX also require tinytex or a local LaTeX
# installation.
#
# Ported and adapted from the IDEEA project (ideea_report.r).

# ── S4 generic ─────────────────────────────────────────────────────────────────

#' Generate a summary report for an energyRt object
#'
#' @description
#' Creates a PDF, HTML, or LaTeX document summarising key parameters of the
#' object.  The layout and content are controlled by the \code{template}
#' argument.
#'
#' @param object An energyRt S4 object.  Currently supports \code{technology}.
#' @param template Character.  Template name that defines which parameters to
#'   display.  Currently \code{"generic"} is built into the package.  Pass the
#'   absolute path to a custom \code{.Rmd} file to use your own template.
#'   Default \code{NULL} selects \code{"generic"} automatically.
#' @param image_file Character.  Optional path to a PNG/JPG image displayed in
#'   the upper-right corner of the page.  \code{NULL} skips the image.
#' @param file Character.  Destination file path.  Defaults to a temporary file
#'   with the extension appropriate for \code{format}.
#' @param format Character.  Output format: \code{"pdf"} (default),
#'   \code{"html"}, or \code{"tex"} (standalone LaTeX source).  Multiple
#'   values are accepted; one file is produced per format.
#' @param levcost A \code{levcost} (or \code{levcost_list}) object returned by
#'   \code{\link{levcost}}, or \code{NULL} (default).  When \code{NULL} and any
#'   \code{levcost} keyword arguments are passed via \code{...} (e.g.
#'   \code{group}, \code{repo}, \code{discount}), \code{levcost()} is called
#'   automatically on \code{object} with those arguments.
#' @param cost_unit Character or \code{NULL}.  Cost unit label used on LCOE
#'   axis (e.g. \code{"USD/GJ"}).  \code{NULL} derives the label from
#'   \code{object@@units}.
#' @param ... Arguments forwarded to \code{\link{levcost}} (when
#'   \code{levcost = NULL} and levcost parameters are provided) and/or to
#'   \code{rmarkdown::render()}.  Known \code{levcost} parameter names are
#'   intercepted automatically; everything else is passed to the renderer.
#'
#' @return The path(s) to the generated output file(s) (invisibly).  A single
#'   string when one format is requested; a character vector when multiple
#'   formats are requested.
#'
#' @seealso \code{\link{levcost}}, \code{\link{report_pdf}},
#'   \code{\link{report_html}}, \code{\link{report_tex}}
#'
#' @export
setGeneric(
  "report",
  function(object, template = NULL, image_file = NULL, file = NULL,
           format = c("pdf", "html", "tex"),
           levcost = NULL, cost_unit = NULL, ...) {
    standardGeneric("report")
  }
)

# ── technology method ──────────────────────────────────────────────────────────

#' @rdname report
#' @export
setMethod(
  "report",
  "technology",
  function(object, template = NULL, image_file = NULL, file = NULL,
           format = c("pdf", "html", "tex"),
           levcost = NULL, cost_unit = NULL, ...) {

    format <- match.arg(format, c("pdf", "html", "tex"), several.ok = TRUE)

    # -- resolve template --------------------------------------------------
    tmpl_name <- if (is.null(template)) "generic" else template

    # -- split ... into levcost args vs rmarkdown::render args -------------
    .levcost_params <- c("comm", "group", "repo", "fuel_costs",
                         "discount", "base_year",
                         "horizon", "calendar", "region", "weather",
                         "frontier", "solver", "full_output", "verbose")
    dots        <- list(...)
    lc_dots     <- dots[intersect(names(dots), .levcost_params)]
    render_dots <- dots[setdiff(names(dots), .levcost_params)]

    # Auto-run levcost() when the caller supplied its args but not the result
    if (is.null(levcost) && length(lc_dots) > 0) {
      levcost <- tryCatch(
        do.call(energyRt::levcost, c(list(object = object), lc_dots)),
        error = function(e) {
          warning("levcost() failed inside report(): ",
                  conditionMessage(e), "\nLevelized cost section will be omitted.")
          NULL
        }
      )
    }

    # -- locate Rmd template -----------------------------------------------
    # If tmpl_name is an existing file path, use it directly.
    if (file.exists(tmpl_name)) {
      tmpl <- normalizePath(tmpl_name, mustWork = TRUE)
    } else {
      tmpl <- .find_report_template(tmpl_name)
    }

    # -- output file base (extension stripped) ------------------------------
    if (is.null(file)) {
      file_base <- tempfile(
        pattern = paste0("report_", gsub("[^A-Za-z0-9_]", "_", object@name), "_"))
    } else {
      file_base <- tools::file_path_sans_ext(file)
    }
    file_base <- normalizePath(file_base, mustWork = FALSE)

    # -- absolute image path -----------------------------------------------
    image_file_abs <- NULL
    if (!is.null(image_file)) {
      if (!file.exists(image_file)) {
        warning("image_file not found and will be ignored: ", image_file)
      } else {
        image_file_abs <- normalizePath(image_file, mustWork = TRUE)
      }
    }

    # -- draw() schematic → temp PNG ---------------------------------------
    draw_file <- tryCatch({
      tmp_draw <- tempfile(pattern = "report_draw_", fileext = ".png")
      grDevices::png(tmp_draw, width = 900, height = 600, res = 150, bg = "white")
      draw_fn <- get("draw", envir = asNamespace("energyRt"))
      draw_fn(object)
      grDevices::dev.off()
      if (file.exists(tmp_draw) && file.info(tmp_draw)$size > 5000) tmp_draw else NULL
    }, error = function(e) {
      tryCatch(grDevices::dev.off(), error = function(e2) NULL)
      warning("draw() failed and will be omitted: ", conditionMessage(e))
      NULL
    })

    # -- share frontier chart (geometric, always when groups present) ------
    share_frontier_plot <- NULL
    if (requireNamespace("ggplot2", quietly = TRUE)) {
      share_frontier_plot <- tryCatch({
        df <- tech_share_frontier(object)
        if (!is.null(df) && nrow(df) > 0)
          plot_share_frontier(df, base_size = if (any(format %in% c("pdf", "tex"))) 8L else 11L)
        else NULL
      }, error = function(e) {
        warning("share frontier plot failed: ", conditionMessage(e))
        NULL
      })
    }

    # -- levcost plots (compact, for inline figure) ------------------------
    levcost_plot  <- NULL
    frontier_plot <- NULL
    if (!is.null(levcost)) {
      if (!requireNamespace("ggplot2", quietly = TRUE)) {
        warning("Package 'ggplot2' is required for levcost plots; they will be omitted.")
      } else {
        lc_obj <- if (inherits(levcost, "levcost_list")) levcost[[1]] else levcost
        compact_theme <- ggplot2::theme_bw(base_size = 10L) +
          ggplot2::theme(
            legend.position  = "bottom",
            legend.key.size  = ggplot2::unit(0.3, "cm"),
            legend.text      = ggplot2::element_text(size = 6),
            plot.title       = ggplot2::element_text(size = 8, face = "bold"),
            plot.subtitle    = ggplot2::element_text(size = 7),
            plot.caption     = ggplot2::element_text(size = 6),
            axis.text.x      = ggplot2::element_text(angle = 45, hjust = 1, size = 7),
            plot.margin      = ggplot2::margin(2, 4, 2, 2)
          )
        levcost_plot <- tryCatch({
          p <- autoplot(lc_obj, type = "npv", cost_unit = cost_unit)
          if (!is.null(p)) p + compact_theme else NULL
        }, error = function(e) {
          warning("levcost plot (type='npv') failed: ", conditionMessage(e))
          NULL
        })
        if (!is.null(lc_obj$frontier) && nrow(lc_obj$frontier) > 0) {
          frontier_plot <- tryCatch({
            p <- autoplot(lc_obj, type = "frontier", cost_unit = cost_unit)
            if (!is.null(p)) p + compact_theme else NULL
          }, error = function(e) {
            warning("frontier plot failed: ", conditionMessage(e))
            NULL
          })
        }
      }
    }

    # -- build params list passed to rmarkdown::render ---------------------
    params <- .tech_report_params_generic(object, image_file_abs, draw_file)
    params$levcost_plot        <- levcost_plot
    params$frontier_plot       <- frontier_plot
    params$share_frontier_plot <- share_frontier_plot
    params$units_costs         <- if (!is.null(cost_unit) && nzchar(cost_unit)) cost_unit else
      .slot_val_report(object@units, "costs", "")
    params$levcost_npv         <- NULL
    if (!is.null(levcost)) {
      lc_src <- if (inherits(levcost, "levcost_list")) levcost[[1]] else levcost
      npv    <- lc_src$levcost_npv
      if (!is.null(npv)) params$levcost_npv <- as.numeric(npv)[1]
    }

    # -- ensure TinyTeX is loaded when needed ------------------------------
    if (any(format %in% c("pdf", "tex"))) {
      if (!isNamespaceLoaded("tinytex") && requireNamespace("tinytex", quietly = TRUE)) {
        loadNamespace("tinytex")
      }
    }

    # -- render each format ------------------------------------------------
    out_files <- character(0)
    for (fmt in format) {
      ext  <- switch(fmt, pdf = ".pdf", html = ".html", tex = ".tex")
      fout <- paste0(file_base, ext)

      out_fmt <- switch(fmt,
        pdf  = rmarkdown::pdf_document(latex_engine = "pdflatex", keep_tex = FALSE),
        html = rmarkdown::html_document(self_contained = TRUE),
        tex  = rmarkdown::latex_document()
      )

      fmt_params <- params
      if (!is.null(image_file_abs) && fmt %in% c("pdf", "tex")) {
        img <- image_file_abs
        if (grepl(" ", img)) {
          ext_img <- tolower(tools::file_ext(img))
          tmp_img <- tempfile(pattern = "report_img_", fileext = paste0(".", ext_img))
          file.copy(img, tmp_img, overwrite = TRUE)
          img <- tmp_img
        }
        fmt_params$image_file <- gsub("\\\\", "/", img)
      }

      do.call(rmarkdown::render, c(
        list(
          input         = tmpl,
          output_format = out_fmt,
          output_file   = fout,
          params        = fmt_params,
          envir         = new.env(parent = globalenv()),
          quiet         = TRUE
        ),
        render_dots
      ))

      message("Report written to: ", fout)
      out_files <- c(out_files, fout)
    }

    invisible(if (length(out_files) == 1L) out_files[1L] else out_files)
  }
)

# ── format wrappers ────────────────────────────────────────────────────────────

#' Generate a PDF report for an energyRt object
#'
#' Thin wrapper around \code{\link{report}} that fixes \code{format = "pdf"}.
#'
#' @inheritParams report
#' @return Path to the generated \code{.pdf} file (invisibly).
#' @export
setGeneric("report_pdf", function(object, ...) standardGeneric("report_pdf"))

#' @rdname report_pdf
#' @export
setMethod("report_pdf", "technology", function(object, ...) {
  report(object, ..., format = "pdf")
})

#' Generate an HTML report for an energyRt object
#'
#' Thin wrapper around \code{\link{report}} that fixes \code{format = "html"}.
#' Produces a portable single-file \code{.html} document with embedded assets.
#'
#' @inheritParams report
#' @return Path to the generated \code{.html} file (invisibly).
#' @export
setGeneric("report_html", function(object, ...) standardGeneric("report_html"))

#' @rdname report_html
#' @export
setMethod("report_html", "technology", function(object, ...) {
  report(object, ..., format = "html")
})

#' Generate a LaTeX report for an energyRt object
#'
#' Thin wrapper around \code{\link{report}} that fixes \code{format = "tex"}.
#' Produces a standalone \code{.tex} file without compiling to PDF.
#'
#' @inheritParams report
#' @return Path to the generated \code{.tex} file (invisibly).
#' @export
setGeneric("report_tex", function(object, ...) standardGeneric("report_tex"))

#' @rdname report_tex
#' @export
setMethod("report_tex", "technology", function(object, ...) {
  report(object, ..., format = "tex")
})

# ── template finder ────────────────────────────────────────────────────────────

#' Locate a report Rmd template by name
#' @param name Character template name (e.g. \code{"generic"}).
#' @return Absolute path to the \code{.Rmd} file.
#' @noRd
.find_report_template <- function(name) {
  fname <- paste0("report_", name, ".Rmd")

  # 1. Installed package path
  pkg_path <- suppressWarnings(
    tryCatch(system.file("templates", fname, package = "energyRt"), error = function(e) "")
  )
  if (nzchar(pkg_path) && file.exists(pkg_path)) return(pkg_path)

  # 2. Development: inst/templates relative to this file's directory
  this_dir <- tryCatch(dirname(normalizePath(sys.frame(0)$ofile)), error = function(e) ".")
  candidates <- c(
    file.path(this_dir, "..", "inst", "templates", fname),
    file.path("inst", "templates", fname)
  )
  for (p in candidates) {
    p2 <- tryCatch(normalizePath(p, mustWork = TRUE), error = function(e) NULL)
    if (!is.null(p2) && file.exists(p2)) return(p2)
  }

  stop("Report template '", fname, "' not found.\n",
       "Expected location: inst/templates/", fname, "\n",
       "Available built-in templates: \"generic\"")
}

# ── params extractor ───────────────────────────────────────────────────────────

#' Build the params list passed to rmarkdown::render (generic technology)
#' @noRd
.tech_report_params_generic <- function(object, image_file = NULL,
                                        draw_file = NULL) {
  name    <- if (nzchar(object@name)) object@name else "(unnamed)"
  desc    <- if (nzchar(object@desc)) object@desc else ""
  cap2act <- object@cap2act

  units_cap   <- .slot_val_report(object@units, "capacity", "")
  units_act   <- .slot_val_report(object@units, "activity", "")
  units_costs <- .slot_val_report(object@units, "costs",    "")

  olife_val <- if (nrow(object@olife) > 0) object@olife$olife[1] else NA_integer_
  start_val <- if (nrow(object@start) > 0) object@start$start[1] else NA_integer_
  end_val   <- if (nrow(object@end)   > 0) object@end$end[1]     else NA_integer_

  cap_df <- object@capacity
  if (nrow(cap_df) > 0) {
    value_cols <- setdiff(names(cap_df), c("region", "year"))
    keep <- rowSums(!is.na(cap_df[, value_cols, drop = FALSE])) > 0
    cap_df <- cap_df[keep, , drop = FALSE]
  }
  stock_val <- if (nrow(cap_df) > 0 && "stock" %in% names(cap_df)) {
    v <- cap_df$stock[!is.na(cap_df$stock)]; if (length(v) > 0) v[1] else NA_real_
  } else NA_real_

  input_df  <- if (nrow(object@input)  > 0) object@input  else NULL
  output_df <- if (nrow(object@output) > 0) object@output else NULL

  ceff_df <- if (nrow(object@ceff) > 0) {
    preferred_cols <- c("comm", "cinp2use", "use2cact", "cact2cout",
                        "cinp2ginp", "share.lo", "share.up", "share.fx")
    cc  <- intersect(preferred_cols, names(object@ceff))
    df  <- object@ceff[, cc, drop = FALSE]
    df  <- df[, colSums(!is.na(df)) > 0, drop = FALSE]
    val_cols <- setdiff(names(df), "comm")
    if (length(val_cols) > 0)
      df <- df[rowSums(!is.na(df[, val_cols, drop = FALSE])) > 0, , drop = FALSE]
    if (nrow(df) > 0) df else NULL
  } else NULL

  invcost_df <- .clean_cost_df_report(object@invcost, "invcost")
  fixom_df   <- .clean_cost_df_report(object@fixom,   "fixom")
  varom_df   <- .clean_cost_df_report(object@varom,   "varom")

  list(
    name        = name,
    desc        = desc,
    cap2act     = cap2act,
    units_cap   = units_cap,
    units_act   = units_act,
    units_costs = units_costs,
    olife       = olife_val,
    stock       = stock_val,
    start       = start_val,
    end         = end_val,
    cap_df      = if (nrow(cap_df) > 0) cap_df else NULL,
    input_df    = input_df,
    output_df   = output_df,
    ceff_df     = ceff_df,
    invcost_df  = invcost_df,
    fixom_df    = fixom_df,
    varom_df    = varom_df,
    image_file  = image_file,
    draw_file   = draw_file
  )
}

# ── small helpers ──────────────────────────────────────────────────────────────

#' Extract a single value from a slot that may be a data.frame or list
#' @noRd
.slot_val_report <- function(x, col, default = NA) {
  if (is.data.frame(x) && col %in% names(x) && nrow(x) > 0) return(x[[col]][1])
  if (is.list(x)       && col %in% names(x) && length(x[[col]]) > 0) return(x[[col]][1])
  default
}

#' Keep only relevant columns from a cost data.frame, dropping all-NA rows
#' @noRd
.clean_cost_df_report <- function(df, cost_col) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)
  keep_cols <- intersect(c("region", "year", cost_col), names(df))
  df <- df[, keep_cols, drop = FALSE]
  val_cols <- setdiff(keep_cols, "region")
  if (length(val_cols) == 0) return(NULL)
  df <- df[rowSums(!is.na(df[, val_cols, drop = FALSE])) > 0, , drop = FALSE]
  if (nrow(df) == 0) NULL else df
}
