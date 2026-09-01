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
#' For a \code{technology}, the datasheet describes that technology. For a
#' \code{repository}, \code{model}, or solved \code{scenario}, give the
#' technology via \code{name = }: the same datasheet is produced, but the
#' embedded levelized cost is computed for that container / solution (the
#' \code{scenario} method uses the \emph{ex-post} cost from the solved model).
#'
#' \strong{Without} \code{name}, the container methods report the whole object:
#' \describe{
#'   \item{\code{model} / \code{repository}}{a full model report -- the
#'     configuration (regions, horizon, calendar, discount), an inventory of
#'     commodities / supplies / demands / trade / constraints, the process
#'     availability windows chart, and every technology and storage described
#'     one-by-one (diagram + key parameters).}
#'   \item{\code{scenario}}{a results overview of the solved scenario -- solve
#'     status and objective, generation / capacity / new-capacity mixes (via
#'     \code{\link{getMix}} and \code{autoplot}), a sub-annual dispatch profile
#'     when the calendar has one, emissions and cost tables.}
#' }
#'
#' @param object An energyRt S4 object: \code{technology}, \code{repository},
#'   \code{model}, or solved \code{scenario}.
#' @param name Character (container/scenario methods). Name of the technology /
#'   process to report.
#' @param template Character.  Template name that defines which parameters to
#'   display.  Currently \code{"generic"} is built into the package.  Pass the
#'   absolute path to a custom \code{.Rmd} file to use your own template.
#'   Default \code{NULL} selects \code{"generic"} automatically.
#' @param image_file Character.  Optional path to a PNG/JPG image displayed in
#'   the upper-right corner of the page.  Defaults to the object's own
#'   \code{misc$image} when that names an existing local file (see
#'   \code{\link{object_image}}); \code{NULL} with no \code{misc$image} skips
#'   the image.
#' @param file Character.  Destination file path (used verbatim).  Without it
#'   the report lands inside the owning folder's \code{reports/} when the
#'   object is saved (a scenario folder, a model/repository store entry), and
#'   otherwise in the temporary project-level [get_reports_path()] folder.
#' @param reports_path Character.  Redirects the default output directory for
#'   this call (\code{file =} still wins).
#' @param force Logical.  Re-render even when the existing files are up to
#'   date.  Reports are skip-if-current: a sidecar (\code{*.report.yml})
#'   records a content key over the object, template, arguments, image and
#'   levcost identity, and an unchanged report is not rebuilt.
#' @param format Character.  Output format: \code{"html"} (default),
#'   \code{"pdf"}, \code{"tex"} (standalone LaTeX source), or \code{"docx"}
#'   (Word; the templates' LaTeX styling degrades to Word's own styles).
#'   Multiple values are accepted; one file is produced per format.
#'   \code{"pdf"}/\code{"tex"} require a LaTeX installation (e.g.
#'   \code{tinytex::install_tinytex()}) and are skipped with a warning when
#'   none is found.
#' @param open Logical.  Open the rendered report in the system browser/viewer
#'   when done.  Defaults to \code{interactive()} (opens in an interactive
#'   session, stays quiet in scripts and knits).
#' @param levcost A \code{levcost} (or \code{levcost_list}) object returned by
#'   \code{\link{levcost}}, or \code{NULL} (default).  When \code{NULL} and any
#'   \code{levcost} keyword arguments are passed via \code{...} (e.g.
#'   \code{group}, \code{repo}, \code{discount}), \code{levcost()} is called
#'   automatically on \code{object} with those arguments.
#'
#'   For a process declaring vintages or clusters the report carries the
#'   per-variant levelised costs (a table plus a comparison chart) alongside
#'   the detail figures for one display instance. Pass \code{by_variant = FALSE}
#'   in \code{...} to report that single instance only. \code{by_variant} is
#'   consumed by \code{report()} and deliberately not forwarded to
#'   \code{levcost()}, whose \code{by_variant = TRUE} returns an extracted
#'   data frame rather than the object the report is built from.
#' @param cost_unit Character or \code{NULL}.  Cost unit label used on LCOE
#'   axis (e.g. \code{"USD/GJ"}).  \code{NULL} derives the label from
#'   \code{object@@units}.
#' @param ... Arguments forwarded to \code{\link{levcost}} (when
#'   \code{levcost = NULL} and levcost parameters are provided) and/or to
#'   \code{rmarkdown::render()}.  Known \code{levcost} parameter names are
#'   intercepted automatically; everything else is passed to the renderer.
#'
#'   The \code{scenario} method additionally accepts \code{run} (character or
#'   \code{NULL}): the run id to report, as listed by
#'   \code{\link{scenario_runs}} (\code{"<solve>"} or
#'   \code{"<variant>/<solve>"}).  The default \code{NULL} reports the active
#'   run; a different run is read into a local copy, so the caller's scenario
#'   keeps its active run.  It also accepts \code{verify} (logical, default
#'   \code{TRUE}): include the \code{\link{verify_solution}} checks section.
#'   For a whole-scenario report, \code{levcost = TRUE} adds an ex-post
#'   levelized-cost table via \code{levcost(scenario)}.
#'
#'   The model, repository and scenario methods accept branding arguments:
#'   \code{logos} (character vector of image paths rendered as a banner
#'   row; default from \code{misc$logos}, then the scalar \code{misc$logo}
#'   / \code{misc$image} -- a scenario shows its model's logos first, then
#'   its own), the model method \code{figure} (an optional half-page hero
#'   figure, default \code{misc$figure}), and the scenario method
#'   \code{badges} (small property-indicator images, default
#'   \code{misc$badges}). Branding content enters the render key, so a
#'   changed logo re-renders an otherwise up-to-date report.
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
           format = c("html", "pdf", "tex", "docx"),
           levcost = NULL, cost_unit = NULL, open = interactive(), ...) {
    standardGeneric("report")
  }
)

# TRUE when a LaTeX toolchain is available for pdf/tex output.
.report_has_latex <- function() {
  nzchar(Sys.which("pdflatex")) || nzchar(Sys.which("xelatex")) ||
    (requireNamespace("tinytex", quietly = TRUE) &&
       isTRUE(tryCatch(tinytex::is_tinytex(), error = function(e) FALSE)))
}

# Prefer a Unicode-capable LaTeX engine: report content carries UTF-8 typography
# (box-drawing banners, em-dashes for NA) that pdflatex cannot typeset. Only fall
# back to pdflatex when no Unicode-capable engine is available.
.report_latex_engine <- function() {
  if (nzchar(Sys.which("xelatex")))  return("xelatex")
  if (nzchar(Sys.which("lualatex"))) return("lualatex")
  # tinytex ships xelatex even when it is not on the system PATH
  if (requireNamespace("tinytex", quietly = TRUE) &&
      isTRUE(tryCatch(tinytex::is_tinytex(), error = function(e) FALSE)))
    return("xelatex")
  "pdflatex"
}

# Drop pdf/tex from `format` (with a helpful warning) when LaTeX is missing.
.report_check_formats <- function(format) {
  needs_latex <- format %in% c("pdf", "tex")
  if (any(needs_latex) && !.report_has_latex()) {
    warning("No LaTeX installation found -- skipping format(s): ",
            paste(format[needs_latex], collapse = ", "),
            ". Install one with tinytex::install_tinytex(), ",
            "or use format = \"html\".", call. = FALSE)
    format <- format[!needs_latex]
    if (length(format) == 0) format <- "html"
  }
  format
}

# Run expr while shielding knitr's global state from corruption.
# rmarkdown::render() calls knitr::knit() internally, which mutates
# knitr's global stores (opts_chunk, opts_knit, knit_hooks, knit_patterns).
# Those mutations persist in the calling R session after render() returns,
# corrupting subsequent renders and — when report() is called from inside
# a knitr chunk — breaking the outer knit session so that subsequent
# chunks emit raw source instead of rendered output.
# Always saving and restoring the four stores eliminates both failure modes.
#
# knit_code — the chunk-label registry behind knitr's duplicate-label
# check — needs the OPPOSITE treatment: when report() runs inside a knit,
# the OUTER document's labels are still registered, so the template's own
# chunks (e.g. "en-setup") would be rejected as duplicates of common user
# labels. Empty it before the nested render, put the outer registry back
# after.
.with_knitr_guard <- function(expr) {
  saved_chunk   <- knitr::opts_chunk$get()
  saved_knit    <- knitr::opts_knit$get()
  saved_hooks   <- knitr::knit_hooks$get()
  saved_pat     <- knitr::knit_patterns$get()
  saved_code    <- tryCatch(knitr::knit_code$get(), error = function(e) NULL)

  on.exit({
    knitr::opts_chunk$restore(saved_chunk)
    knitr::opts_knit$restore(saved_knit)
    knitr::knit_hooks$restore(saved_hooks)
    knitr::knit_patterns$restore(saved_pat)
    tryCatch({
      knitr::knit_code$restore()
      if (length(saved_code)) knitr::knit_code$set(saved_code)
    }, error = function(e) NULL)
  }, add = TRUE)

  tryCatch(knitr::knit_code$restore(), error = function(e) NULL)
  force(expr)
}

# Open the first rendered file in the system viewer/browser.
.report_open <- function(out_files, open) {
  if (isTRUE(open) && length(out_files) > 0) {
    pick <- out_files[grepl("\\.(html|pdf)$", out_files)]
    if (length(pick) > 0)
      tryCatch(utils::browseURL(pick[1]), error = function(e) invisible(NULL))
  }
  invisible(NULL)
}

# ── technology method ──────────────────────────────────────────────────────────

#' @rdname report
#' @export
setMethod(
  "report",
  "technology",
  function(object, template = NULL, image_file = NULL, file = NULL,
           format = c("html", "pdf", "tex", "docx"),
           levcost = NULL, cost_unit = NULL, open = interactive(),
           reports_path = NULL, force = FALSE, ...) {

    # default = html only (pdf/tex need LaTeX); explicit requests are honoured
    format <- if (missing(format)) "html" else
      match.arg(format, c("html", "pdf", "tex", "docx"), several.ok = TRUE)
    format <- .report_check_formats(format)

    # -- resolve template --------------------------------------------------
    tmpl_name <- template %||% .report_misc_template(object) %||% "generic"

    # -- split ... into levcost args vs rmarkdown::render args -------------
    .levcost_params <- c("comm", "group", "repo", "fuel_costs",
                         "discount", "base_year",
                         "horizon", "calendar", "region", "weather",
                         "frontier", "solver", "method", "full_output",
                         "verbose", "cache", "cache_dir",
                         # instance selectors, forwarded to levcost()
                         "vintage", "cluster")
    dots        <- list(...)
    # `by_variant` is consumed HERE, not forwarded: levcost(by_variant = TRUE)
    # returns an extracted data.frame, which would fail the levcost_variants
    # test below and silently drop the whole levelised-cost section. The flag
    # instead selects whether the report carries the per-variant tables.
    by_variant  <- dots$by_variant
    dots$by_variant <- NULL
    lc_dots     <- dots[intersect(names(dots), .levcost_params)]
    render_dots <- dots[setdiff(names(dots), .levcost_params)]

    # `levcost` may be a logical convenience flag: TRUE = have report() run
    # levcost() itself (using comm/group/... from ...), FALSE = disable it.
    # NA marks "not specified as a flag" (i.e. NULL or a ready levcost object).
    run_levcost <- NA
    if (is.logical(levcost)) {
      run_levcost <- isTRUE(levcost)
      levcost <- NULL
    }
    auto_levcost <- is.null(levcost) &&
      (isTRUE(run_levcost) || (is.na(run_levcost) && length(lc_dots) > 0))

    # -- locate Rmd template -----------------------------------------------
    # If tmpl_name is an existing file path (not a directory), use it.
    if (file.exists(tmpl_name) && !dir.exists(tmpl_name)) {
      tmpl <- normalizePath(tmpl_name, mustWork = TRUE)
    } else {
      tmpl <- .find_report_template(tmpl_name)
    }

    # -- sort remaining ... into template params vs render args ------------
    # Anything the template declares as a param is passed as one; anything
    # rmarkdown::render() accepts is forwarded; the rest is dropped with a
    # warning (old scripts may carry arguments from retired templates).
    sorted      <- .report_sort_dots(render_dots, tmpl)
    param_dots  <- sorted$params
    render_dots <- sorted$render

    # -- output file base (extension stripped) ------------------------------
    # saved objects render inside their owning folder's reports/; in-memory
    # objects into the temporary get_reports_path() tier; file= wins verbatim
    file_base <- .report_output_base(object, file, reports_path, object@name)

    # -- absolute image path -----------------------------------------------
    # Fall back to the object's own `misc$image` convention, so `report(tech)`
    # picks up an image set in the process designer or a techspec YAML without
    # the caller having to repeat it. URLs are skipped: templates take local
    # files only.
    if (is.null(image_file)) image_file <- .object_image_file(object)
    image_file_abs <- NULL
    if (!is.null(image_file)) {
      if (!file.exists(image_file)) {
        warning("image_file not found and will be ignored: ", image_file)
      } else {
        image_file_abs <- normalizePath(image_file, mustWork = TRUE)
      }
    }

    # -- skip-if-current -----------------------------------------------------
    # The key names the report's content: object hash, template content, user
    # args, image content, and the levcost identity (the cache key an auto-run
    # WOULD use — computed without running, so an up-to-date report returns
    # before levcost, draw() or any plot work happens).
    lc_id <- if (!is.null(levcost)) {
      .levcost_result_id(levcost)
    } else if (auto_levcost) {
      .levcost_call_key(object, lc_dots)
    } else NULL
    key <- .report_key("report/technology", object, tmpl,
                       args = c(param_dots, render_dots,
                                list(cost_unit = cost_unit)),
                       image_file = image_file_abs, levcost_id = lc_id)
    if (!isTRUE(force)) {
      current <- vapply(format, function(f)
        .report_uptodate(file_base, f, key), logical(1))
    } else {
      current <- rep(FALSE, length(format))
    }
    if (all(current)) {
      out_files <- paste0(file_base, ".", format)
      message("Report up to date: ", paste(out_files, collapse = ", "),
              " (force = TRUE to re-render)")
      .report_open(out_files, open)
      return(invisible(if (length(out_files) == 1L) out_files[1L] else
        out_files))
    }
    format <- format[!current]

    # -- auto-run levcost (cache-backed, see R/levcost_cache.R) --------------
    if (auto_levcost) {
      levcost <- tryCatch(
        do.call(energyRt::levcost, c(list(object = object), lc_dots)),
        error = function(e) {
          warning("levcost() failed inside report(): ",
                  conditionMessage(e), "\nLevelized cost section will be omitted.")
          NULL
        }
      )
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

    # -- levcost: resolve the display instance and vintage tables ----------
    # A `levcost_variants` result is NOT reduced to its first variant any
    # more: the per-vintage NPVs go to the report as a table + comparison
    # chart, while the detail figures (components, frontier) come from ONE
    # instance -- draw()'s default cell (newest vintage / first cluster) --
    # re-priced with the same arguments (analytic under method = "auto",
    # so essentially free) and labeled as that instance.
    levcost_by_vintage_df  <- NULL
    levcost_instance_label <- NULL
    lc_obj <- levcost
    if (inherits(levcost, "levcost_variants") && identical(by_variant, FALSE)) {
      # explicitly asked for a single-instance report: keep the display
      # instance and drop the per-variant table and comparison chart
      lc_obj <- .report_pick_instance(levcost)
    } else if (inherits(levcost, "levcost_variants")) {
      levcost_by_vintage_df <- tryCatch(
        .report_drop_empty_cols(levcost(levcost, by_variant = "npv")),
        error = function(e) NULL)
      inst <- tryCatch(.levcost_instance(object), error = function(e) NULL)
      lc_inst <- NULL
      if (!is.null(inst)) {
        ld <- lc_dots
        ld$frontier <- TRUE
        ld$verbose  <- FALSE
        lc_inst <- tryCatch(
          suppressMessages(suppressWarnings(
            do.call(energyRt::levcost, c(list(object = inst), ld)))),
          error = function(e) NULL)
        if (!inherits(lc_inst, "levcost")) lc_inst <- NULL
      }
      lc_obj <- if (!is.null(lc_inst)) lc_inst else
        .report_pick_instance(levcost)
      levcost_instance_label <- if (!is.null(inst)) inst@name else
        names(lc_obj$levcost_npv)[1]
    } else if (inherits(levcost, "levcost_list")) {
      lc_obj <- levcost[[1]]
    }

    # -- levcost plots (compact, for inline figure) ------------------------
    levcost_plot  <- NULL
    frontier_plot <- NULL
    levcost_variants_plot <- NULL
    if (!is.null(levcost)) {
      if (!requireNamespace("ggplot2", quietly = TRUE)) {
        warning("Package 'ggplot2' is required for levcost plots; they will be omitted.")
      } else {
        compact_theme <- theme_energyRt(base_size = 10L) +
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
          p <- ggplot2::autoplot(lc_obj, type = "npv", cost_unit = cost_unit)
          # the section header already names the instance -- an in-plot
          # title just repeats it and steals figure height
          if (!is.null(p)) p + compact_theme +
            ggplot2::labs(title = NULL, subtitle = NULL, caption = NULL)
          else NULL
        }, error = function(e) {
          warning("levcost plot (type='npv') failed: ", conditionMessage(e))
          NULL
        })
        if (!is.null(lc_obj$frontier) && nrow(lc_obj$frontier) > 0) {
          frontier_plot <- tryCatch({
            p <- ggplot2::autoplot(lc_obj, type = "frontier", cost_unit = cost_unit)
            if (!is.null(p)) p + compact_theme else NULL
          }, error = function(e) {
            warning("frontier plot failed: ", conditionMessage(e))
            NULL
          })
        }
        if (inherits(levcost, "levcost_variants")) {
          levcost_variants_plot <- tryCatch({
            p <- ggplot2::autoplot(levcost, type = "npv", cost_unit = cost_unit)
            # the compact raster clips autoplot's full "Levelized cost
            # [unit]" axis title; the section header already says
            # "Levelized Cost", so the axis carries just the unit
            y_short <- if (!is.null(cost_unit) && nzchar(cost_unit)) {
              cost_unit
            } else {
              u <- levcost[[1]]$units
              cu <- if (!is.null(u$costs) && nzchar(u$costs)) u$costs else ""
              au <- if (!is.null(u$activity) && nzchar(u$activity))
                u$activity else ""
              if (nzchar(cu) && nzchar(au)) paste0(cu, "/", au) else
                if (nzchar(cu)) cu else NULL
            }
            if (!is.null(p)) {
              p <- p + compact_theme +
                ggplot2::labs(title = NULL, subtitle = NULL, caption = NULL,
                              x = NULL)
              if (!is.null(y_short)) p <- p + ggplot2::labs(y = y_short)
              p
            } else NULL
          }, error = function(e) {
            warning("levcost by-vintage plot failed: ", conditionMessage(e))
            NULL
          })
        }
      }
    }

    # -- build params list passed to rmarkdown::render ---------------------
    params <- .tech_report_params_generic(object, image_file_abs, draw_file)
    params$levcost_plot           <- levcost_plot
    params$frontier_plot          <- frontier_plot
    params$share_frontier_plot    <- share_frontier_plot
    params$levcost_variants_plot  <- levcost_variants_plot
    params$levcost_by_vintage_df  <- levcost_by_vintage_df
    params$levcost_instance_label <- levcost_instance_label
    params$units_costs         <- if (!is.null(cost_unit) && nzchar(cost_unit)) cost_unit else
      .slot_val_report(object@units, "costs", "")
    params$levcost_npv         <- NULL
    if (!is.null(lc_obj)) {
      npv <- lc_obj$levcost_npv
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
      ext  <- switch(fmt, pdf = ".pdf", html = ".html", tex = ".tex",
                     docx = ".docx")
      fout <- paste0(file_base, ext)

      out_fmt <- switch(fmt,
        pdf  = rmarkdown::pdf_document(latex_engine = .report_latex_engine(),
                                       keep_tex = FALSE),
        html = rmarkdown::html_document(self_contained = TRUE,
               # pass hand-written <div>/<span> through as raw HTML: older pandoc
               # parses them as native divs and mis-balances them, swallowing the
               # rest of the document (chunks then leak as raw source).
               md_extensions = "-native_divs-native_spans"),
        tex  = rmarkdown::latex_document(),
        # LaTeX header-includes are ignored by pandoc's docx writer, so the
        # templates degrade gracefully to Word's own styles
        docx = rmarkdown::word_document()
      )

      fmt_params <- params
      if (length(param_dots) > 0) fmt_params[names(param_dots)] <- param_dots
      # only pass params the template declares -- rmarkdown hard-errors on
      # undeclared ones, which would break older / user-supplied templates
      # whenever report() grows a new param
      tmpl_params <- tryCatch(
        names(rmarkdown::yaml_front_matter(tmpl)$params),
        error = function(e) NULL)
      if (!is.null(tmpl_params))
        fmt_params <- fmt_params[names(fmt_params) %in% tmpl_params]
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

      .with_knitr_guard(
        do.call(rmarkdown::render, c(
          list(
            input             = tmpl,
            output_format     = out_fmt,
            output_file       = fout,
            params            = fmt_params,
            envir             = new.env(parent = globalenv()),
            intermediates_dir = tempfile("report_int_"),
            quiet             = TRUE
          ),
          render_dots
        ))
      )

      message("Report written to: ", fout)
      out_files <- c(out_files, fout)
    }

    .report_sidecar_write(
      file_base, key,
      meta = list(object = list(class = "technology", name = object@name),
                  template = tmpl_name),
      files = stats::setNames(as.list(basename(out_files)), format))

    .report_open(out_files, open)
    invisible(if (length(out_files) == 1L) out_files[1L] else out_files)
  }
)

# ── storage method ────────────────────────────────────────────────────────────
# The generic template renders storage through the same params contract as
# technology, plus the storage-only optional params (reservoir_df, seff_df,
# duration_df, storage_cost_df). The levelized-cost sections are not wired
# for storage yet and are omitted.

#' @rdname report
#' @export
setMethod(
  "report",
  "storage",
  function(object, template = NULL, image_file = NULL, file = NULL,
           format = c("html", "pdf", "tex", "docx"),
           levcost = NULL, cost_unit = NULL, open = interactive(),
           reports_path = NULL, force = FALSE, ...) {

    format <- if (missing(format)) "html" else
      match.arg(format, c("html", "pdf", "tex", "docx"), several.ok = TRUE)
    format <- .report_check_formats(format)

    # Levelized cost of storage (LCOS): energyRt >= 0.89 computes levcost()
    # for `storage`, so the section is built exactly as for technology --
    # `levcost = TRUE` runs it here using the levcost arguments passed in `...`.
    .levcost_params <- c("comm", "group", "repo", "fuel_costs", "discount",
                         "base_year", "horizon", "calendar", "region",
                         "weather", "solver", "method", "full_output",
                         "verbose", "vintage", "cluster")
    dots0   <- list(...)
    dots0$by_variant <- NULL   # consumed, see the technology method
    lc_dots <- dots0[intersect(names(dots0), .levcost_params)]
    run_levcost <- NA
    if (is.logical(levcost)) {
      run_levcost <- isTRUE(levcost)
      levcost <- NULL
    }
    if (is.null(levcost) &&
        (isTRUE(run_levcost) || (is.na(run_levcost) && length(lc_dots) > 0))) {
      levcost <- tryCatch(
        do.call(energyRt::levcost, c(list(object = object), lc_dots)),
        error = function(e) {
          warning("levcost() failed inside report(): ", conditionMessage(e),
                  "
Levelized cost section will be omitted.")
          NULL
        })
    }

    tmpl_name <- template %||% .report_misc_template(object) %||% "generic"
    tmpl <- if (file.exists(tmpl_name) && !dir.exists(tmpl_name)) {
      normalizePath(tmpl_name, mustWork = TRUE)
    } else {
      .find_report_template(tmpl_name)
    }

    sorted      <- .report_sort_dots(
      dots0[setdiff(names(dots0), .levcost_params)], tmpl)
    param_dots  <- sorted$params
    render_dots <- sorted$render

    file_base <- .report_output_base(object, file, reports_path, object@name)

    if (is.null(image_file)) image_file <- .object_image_file(object)
    image_file_abs <- NULL
    if (!is.null(image_file)) {
      if (!file.exists(image_file)) {
        warning("image_file not found and will be ignored: ", image_file)
      } else {
        image_file_abs <- normalizePath(image_file, mustWork = TRUE)
      }
    }

    key <- .report_key("report/storage", object, tmpl,
                       args = c(param_dots, render_dots,
                                list(cost_unit = cost_unit)),
                       image_file = image_file_abs)
    current <- if (isTRUE(force)) rep(FALSE, length(format)) else
      vapply(format, function(f) .report_uptodate(file_base, f, key),
             logical(1))
    if (all(current)) {
      out_files <- paste0(file_base, ".", format)
      message("Report up to date: ", paste(out_files, collapse = ", "),
              " (force = TRUE to re-render)")
      .report_open(out_files, open)
      return(invisible(if (length(out_files) == 1L) out_files[1L] else
        out_files))
    }
    format <- format[!current]

    draw_file <- tryCatch({
      tmp_draw <- tempfile(pattern = "report_draw_", fileext = ".png")
      grDevices::png(tmp_draw, width = 900, height = 600, res = 150,
                     bg = "white")
      draw_fn <- get("draw", envir = asNamespace("energyRt"))
      draw_fn(object)
      grDevices::dev.off()
      if (file.exists(tmp_draw) && file.info(tmp_draw)$size > 5000)
        tmp_draw else NULL
    }, error = function(e) {
      tryCatch(grDevices::dev.off(), error = function(e2) NULL)
      warning("draw() failed and will be omitted: ", conditionMessage(e))
      NULL
    })

    params <- .storage_report_params_generic(object, image_file_abs,
                                             draw_file)
    if (!is.null(cost_unit) && nzchar(cost_unit)) {
      params$units_costs <- cost_unit
    }

    # LCOS section. Storage has no `@units` slot, so `levcost()` returns no
    # cost unit: the label is composed here from the report's own units
    # (`units_costs` per `units_act`), which is also what the template uses
    # for the section header.
    if (!is.null(levcost)) {
      lc_unit <- if (nzchar(params$units_costs) && nzchar(params$units_act)) {
        paste0(params$units_costs, "/", params$units_act)
      } else if (nzchar(params$units_costs)) {
        params$units_costs
      } else NULL
      lc_theme <- theme_energyRt(base_size = 10L) +
        ggplot2::theme(
          legend.position = "bottom",
          legend.key.size = ggplot2::unit(0.3, "cm"),
          legend.text     = ggplot2::element_text(size = 6),
          axis.text.x     = ggplot2::element_text(angle = 45, hjust = 1,
                                                  size = 7))
      has_gg <- requireNamespace("ggplot2", quietly = TRUE)
      if (inherits(levcost, "levcost_variants")) {
        params$levcost_by_vintage_df <- tryCatch(
          .report_drop_empty_cols(energyRt::levcost(levcost,
                                                    by_variant = "npv")),
          error = function(e) NULL)
        if (has_gg) {
          params$levcost_variants_plot <- tryCatch({
            p <- ggplot2::autoplot(levcost, type = "npv",
                                   cost_unit = lc_unit)
            if (!is.null(p)) {
              p <- p + lc_theme + ggplot2::labs(title = NULL, subtitle = NULL,
                                                caption = NULL, x = NULL)
              if (!is.null(lc_unit)) p <- p + ggplot2::labs(y = lc_unit)
              p
            } else NULL
          }, error = function(e) {
            warning("levcost by-vintage plot failed: ", conditionMessage(e))
            NULL
          })
        }
      } else {
        npv <- levcost$levcost_npv
        if (!is.null(npv)) params$levcost_npv <- as.numeric(npv)[1]
        if (has_gg) {
          params$levcost_plot <- tryCatch({
            p <- ggplot2::autoplot(levcost, type = "npv",
                                   cost_unit = lc_unit)
            if (!is.null(p)) p + lc_theme +
              ggplot2::labs(title = NULL, subtitle = NULL, caption = NULL)
            else NULL
          }, error = function(e) NULL)
        }
      }
    }

    if (any(format %in% c("pdf", "tex"))) {
      if (!isNamespaceLoaded("tinytex") &&
          requireNamespace("tinytex", quietly = TRUE)) {
        loadNamespace("tinytex")
      }
    }

    out_files <- character(0)
    for (fmt in format) {
      ext  <- switch(fmt, pdf = ".pdf", html = ".html", tex = ".tex",
                     docx = ".docx")
      fout <- paste0(file_base, ext)
      out_fmt <- switch(fmt,
        pdf  = rmarkdown::pdf_document(latex_engine = .report_latex_engine(),
                                       keep_tex = FALSE),
        html = rmarkdown::html_document(self_contained = TRUE,
               md_extensions = "-native_divs-native_spans"),
        tex  = rmarkdown::latex_document(),
        docx = rmarkdown::word_document()
      )

      fmt_params <- params
      if (length(param_dots) > 0) fmt_params[names(param_dots)] <- param_dots
      tmpl_params <- tryCatch(
        names(rmarkdown::yaml_front_matter(tmpl)$params),
        error = function(e) NULL)
      if (!is.null(tmpl_params))
        fmt_params <- fmt_params[names(fmt_params) %in% tmpl_params]
      if (!is.null(image_file_abs) && fmt %in% c("pdf", "tex")) {
        img <- image_file_abs
        if (grepl(" ", img)) {
          ext_img <- tolower(tools::file_ext(img))
          tmp_img <- tempfile(pattern = "report_img_",
                              fileext = paste0(".", ext_img))
          file.copy(img, tmp_img, overwrite = TRUE)
          img <- tmp_img
        }
        fmt_params$image_file <- gsub("\\\\", "/", img)
      }

      .with_knitr_guard(
        do.call(rmarkdown::render, c(
          list(
            input             = tmpl,
            output_format     = out_fmt,
            output_file       = fout,
            params            = fmt_params,
            envir             = new.env(parent = globalenv()),
            intermediates_dir = tempfile("report_int_"),
            quiet             = TRUE
          ),
          render_dots
        ))
      )
      message("Report written to: ", fout)
      out_files <- c(out_files, fout)
    }

    .report_sidecar_write(
      file_base, key,
      meta = list(object = list(class = "storage", name = object@name),
                  template = tmpl_name),
      files = stats::setNames(as.list(basename(out_files)), format))

    .report_open(out_files, open)
    invisible(if (length(out_files) == 1L) out_files[1L] else out_files)
  }
)

# ── repository / model / scenario methods ────────────────────────────────────
# Report a technology from a container: reuse the technology datasheet, injecting
# the levelized cost computed for that container / solved scenario. Give the
# technology via `name = ` (in `...`).
.report_lc_params <- c("comm", "group", "repo", "autocomplete", "fuel_costs",
                       "discount", "base_year", "horizon", "calendar",
                       "region", "weather", "frontier", "solver", "method",
                       "full_output", "verbose", "cache", "cache_dir")

# The display instance of a ready-made `levcost_variants` result: newest
# vintage, first cluster (draw()'s default cell), via the variants key --
# never by parsing names.
#' @noRd
.report_pick_instance <- function(lcv) {
  key <- attr(lcv, "variants")
  if (is.null(key) || nrow(key) == 0) return(lcv[[1]])
  vin <- suppressWarnings(as.integer(key$vintage))
  pick <- if (any(is.finite(vin))) which(vin == max(vin, na.rm = TRUE))[1] else 1L
  lcv[[key$variant[pick]]]
}

.report_container <- function(container, tech_source, object_for_levcost,
                              template, image_file, file, format, levcost,
                              cost_unit, name, dots, open = interactive(),
                              reports_path = NULL, force = FALSE) {
  # a process report from a saved container lands in the CONTAINER's
  # reports/ folder (the process itself is not saved anywhere)
  if (is.null(file) && is.null(reports_path)) {
    own <- .object_store_dir(container)
    if (!is.null(own)) reports_path <- fp(own, "reports")
  }
  if (is.null(name) || !nzchar(name)) {
    message("report(): give the element `name = ` to report.")
    return(invisible(NULL))
  }
  # element lookup across ALL classes (was technology-only via
  # .levcost_find_tech); `class = ` in ... narrows/disambiguates
  cls_flt <- dots[["class"]]
  dots[["class"]] <- NULL
  found <- tryCatch(
    getObject(tech_source, name = name, class = cls_flt),
    error = function(e) list())
  if (length(found) == 0) {
    message("report(): element '", name, "' not found in the ",
            class(container)[1], ".")
    return(invisible(NULL))
  }
  if (length(found) > 1) {
    # the same name in several classes: deterministic pick, priced classes
    # first, then alphabetical; `class = ` chooses explicitly
    cl1 <- vapply(found, function(o) class(o)[1], character(1))
    pri <- c(.LEVCOST_CLASSES, setdiff(sort(unique(cl1)), .LEVCOST_CLASSES))
    pick <- order(match(cl1, pri))[1]
    message("report(): '", name, "' matches several classes (",
            paste(sort(unique(cl1)), collapse = ", "), "); reporting the ",
            cl1[pick], ". Pass `class = ` to choose another.")
    found <- found[pick]
  }
  obj <- found[[1]]
  cls <- class(obj)[1]

  lc_dots     <- dots[intersect(names(dots), .report_lc_params)]
  render_dots <- dots[setdiff(names(dots), .report_lc_params)]
  if (is.null(lc_dots$verbose)) lc_dots$verbose <- FALSE

  if (cls %in% c("technology", "storage")) {
    # process datasheet path, unchanged (storage becomes reachable by name)
    if (is.null(levcost)) {
      levcost <- tryCatch(
        do.call(energyRt::levcost,
                c(list(object_for_levcost, name = name), lc_dots)),
        error = function(e) {
          warning("levcost() failed in report(): ", conditionMessage(e))
          NULL
        })
      if (is.null(levcost)) {
        message("report(): could not compute levelized cost for '", name,
                "'.")
        return(invisible(NULL))
      }
    }
    return(do.call(report, c(list(obj, template = template,
      image_file = image_file, file = file, format = format,
      levcost = levcost, cost_unit = cost_unit, open = open,
      reports_path = reports_path, force = force), render_dots)))
  }

  if (cls == "trade" && is.null(levcost)) {
    # priceable, but a trade datasheet without the levcost section is
    # still useful -- non-fatal
    levcost <- tryCatch(suppressWarnings(
      do.call(energyRt::levcost,
              c(list(object_for_levcost, name = name), lc_dots))),
      error = function(e) NULL)
  }
  do.call(report, c(list(obj, template = template, image_file = image_file,
    file = file, format = format,
    levcost = if (cls == "trade") levcost else NULL,
    open = open, reports_path = reports_path, force = force,
    .container = container), render_dots))
}

# ── whole-object reports ──────────────────────────────────────────────────────
# report(model) / report(repository) without `name`: a full model report --
# configuration + inventory + every process one-by-one.
# report(scenario) without `name`: a results overview built on getMix()/autoplot.

# Shared render loop for the whole-object templates. `params` may be a ready
# list or a zero-argument function building one — the lazy form lets an
# up-to-date report return before any plot/draw work happens. When `owner` and
# `engine` are given the render is skip-if-current (see R/report_cache.R).
.report_render <- function(tmpl_name, params, file, format, render_dots, stub,
                           open = interactive(), class = NULL, owner = NULL,
                           reports_path = NULL, force = FALSE, engine = NULL,
                           key_args = list()) {
  format <- .report_check_formats(
    match.arg(format, c("html", "pdf", "tex", "docx"), several.ok = TRUE))
  # file.exists() is TRUE for directories too -- a template NAME that happens
  # to match a folder in the working directory (e.g. "scenarios") must still
  # resolve as a built-in template name
  tmpl <- if (!is.null(tmpl_name) && file.exists(tmpl_name) &&
              !dir.exists(tmpl_name)) {
    normalizePath(tmpl_name, mustWork = TRUE)
  } else {
    .find_report_template(tmpl_name, class = class)
  }
  sorted <- .report_sort_dots(render_dots, tmpl)
  render_dots <- sorted$render
  file_base <- .report_output_base(owner, file, reports_path, stub)
  key <- if (!is.null(owner) && !is.null(engine)) {
    # key_args lets callers key the render on content OUTSIDE the object
    # hash (e.g. branding image files), without touching .report_key
    .report_key(engine, owner, tmpl,
                args = c(sorted$params, render_dots, key_args))
  } else NULL
  if (!is.null(key)) {
    current <- if (isTRUE(force)) rep(FALSE, length(format)) else
      vapply(format, function(f) .report_uptodate(file_base, f, key),
             logical(1))
    if (all(current)) {
      out_files <- paste0(file_base, ".", format)
      message("Report up to date: ", paste(out_files, collapse = ", "),
              " (force = TRUE to re-render)")
      .report_open(out_files, open)
      return(invisible(if (length(out_files) == 1L) out_files[1L] else
        out_files))
    }
    format <- format[!current]
  }
  # Back-compat: rmarkdown hard-errors on params a template does not declare,
  # so filter to the declared set -- an older or minimal custom template keeps
  # rendering as the built-in param contracts grow. Computed BEFORE the
  # deferred builder runs so declaration-aware builders (closures taking a
  # `declared` argument) can skip components the template will not show.
  declared <- tryCatch(names(rmarkdown::yaml_front_matter(tmpl)$params),
                       error = function(e) NULL)
  params <- .report_call_params(params, declared)
  if (length(sorted$params) > 0) params[names(sorted$params)] <- sorted$params
  if (!is.null(declared)) params <- params[intersect(names(params), declared)]
  out_files <- character(0)
  for (fmt in format) {
    ext  <- switch(fmt, pdf = ".pdf", html = ".html", tex = ".tex",
                   docx = ".docx")
    fout <- paste0(file_base, ext)
    out_fmt <- switch(fmt,
      pdf  = rmarkdown::pdf_document(latex_engine = .report_latex_engine()),
      html = rmarkdown::html_document(self_contained = TRUE,
               # pass hand-written <div>/<span> through as raw HTML: older pandoc
               # parses them as native divs and mis-balances them, swallowing the
               # rest of the document (chunks then leak as raw source).
               md_extensions = "-native_divs-native_spans"),
      tex  = rmarkdown::latex_document(),
      docx = rmarkdown::word_document())
    .with_knitr_guard(
      do.call(rmarkdown::render, c(
        list(input = tmpl, output_format = out_fmt, output_file = fout,
             params = params, envir = new.env(parent = globalenv()),
             intermediates_dir = tempfile("report_int_"), quiet = TRUE),
        render_dots))
    )
    message("Report written to: ", fout)
    out_files <- c(out_files, fout)
  }
  if (!is.null(key)) {
    .report_sidecar_write(
      file_base, key,
      meta = list(object = list(class = sub("^report/", "", engine),
                                name = stub),
                  template = tmpl_name %||% ""),
      files = stats::setNames(as.list(basename(out_files)), format))
  }
  .report_open(out_files, open)
  invisible(if (length(out_files) == 1L) out_files[1L] else out_files)
}

# Compact parameter table for one process (technology or storage).
.proc_info_df <- function(p) {
  rows <- list()
  add <- function(k, v) {
    v <- v[!is.na(v) & nzchar(as.character(v))]
    if (length(v) > 0)
      rows[[length(rows) + 1L]] <<- data.frame(parameter = k,
        value = paste(unique(v), collapse = ", "), stringsAsFactors = FALSE)
  }
  gs <- function(sl) tryCatch(methods::slot(p, sl), error = function(e) NULL)
  inp <- gs("input"); out <- gs("output")
  if (is.data.frame(inp) && nrow(inp) > 0) add("input", as.character(inp$comm))
  if (is.data.frame(out) && nrow(out) > 0) add("output", as.character(out$comm))
  stg <- gs("storage")                       # storage reservoir role
  if (is.data.frame(stg) && nrow(stg) > 0) {
    add("storage", as.character(stg$comm))
  }
  ceff <- gs("ceff")
  if (is.data.frame(ceff) && nrow(ceff) > 0) {
    for (cc in intersect(c("cinp2use", "cact2cout", "cinp2ginp"), names(ceff))) {
      v <- ceff[[cc]]; v <- v[!is.na(v)]
      if (length(v) > 0) add(cc, paste(signif(v, 3)))
    }
  }
  geff <- gs("geff")
  if (is.data.frame(geff) && "ginp2use" %in% names(geff)) {
    v <- geff$ginp2use[!is.na(geff$ginp2use)]
    if (length(v) > 0) add("ginp2use", paste(signif(v, 3)))
  }
  num_rng <- function(d, col, unit = "") {
    if (is.data.frame(d) && col %in% names(d)) {
      v <- suppressWarnings(as.numeric(d[[col]])); v <- v[is.finite(v)]
      if (length(v) > 0) {
        r <- range(v)
        return(paste0(if (r[1] == r[2]) signif(r[1], 4) else
          paste(signif(r[1], 4), "-", signif(r[2], 4)), unit))
      }
    }
    NA_character_
  }
  add("invcost", num_rng(gs("invcost"), "invcost"))
  add("fixom",   num_rng(gs("fixom"),   "fixom"))
  add("varom",   num_rng(gs("varom"),   "varom"))
  # storage costs and efficiencies live in part-prefixed columns
  for (cc in c("stg.invcost", "inp.invcost", "out.invcost")) {
    add(cc, num_rng(gs("invcost"), cc))
  }
  for (cc in c("stg.fixom", "inp.fixom", "out.fixom")) {
    add(cc, num_rng(gs("fixom"), cc))
  }
  for (cc in c("stgcost", "inpcost", "outcost")) {
    add(cc, num_rng(gs("varom"), cc))
  }
  for (cc in c("inpeff", "outeff", "stgeff")) {
    add(cc, num_rng(gs("seff"), cc))
  }
  for (cc in c("duration.lo", "duration.up", "duration.fx", "duration")) {
    add(cc, num_rng(gs("duration"), cc))
  }
  # lifespan lives in `@vintage` for technology, in the separate slots elsewhere
  ol <- .lifespan_resolve(p, "olife")
  if (nrow(ol) > 0) add("olife", ol$olife[1])
  st <- .lifespan_resolve(p, "start")
  if (nrow(st) > 0) add("start", st$start[1])
  cap <- gs("capacity")
  if (is.data.frame(cap) && "stock" %in% names(cap)) {
    v <- cap$stock[!is.na(cap$stock)]
    if (length(v) > 0) add("stock (total)", signif(sum(v), 4))
  }
  w <- gs("weather")
  if (is.data.frame(w) && nrow(w) > 0) add("weather", as.character(w$weather))
  if (length(rows) == 0) return(NULL)
  do.call(rbind, rows)
}

# Materialize a params argument: plain lists pass through; a 0-arg closure
# is called as before; a closure with an argument receives the template's
# declared param names so it can build only what will be shown.
#' @noRd
.report_call_params <- function(params, declared = NULL) {
  if (!is.function(params)) return(params)
  if (length(formals(params)) > 0) params(declared) else params()
}

# User-supplied image paths: every file must exist (error names the
# missing ones), normalized and deduplicated.
#' @noRd
.validated_files <- function(paths, what = "image") {
  paths <- as.character(paths)
  bad <- paths[!file.exists(paths)]
  if (length(bad) > 0) {
    stop(what, " file(s) not found: ", paste(bad, collapse = ", "))
  }
  unique(normalizePath(paths, mustWork = TRUE))
}

# Validated existing files from misc keys, in order; character(0) if none.
#' @noRd
.misc_files <- function(x, keys) {
  out <- character(0)
  for (k in keys) {
    v <- tryCatch(x@misc[[k]], error = function(e) NULL)
    if (is.null(v) || !is.character(v)) next
    v <- v[nzchar(v) & file.exists(v)]
    if (length(v) > 0) out <- c(out, normalizePath(v, mustWork = TRUE))
  }
  unique(out)
}

# All branding images of a container, in display order. `override`
# (report(logos = )) REPLACES the misc-derived set entirely and every file
# must exist; otherwise `misc$logos` first, then the scalar `misc$logo` /
# `misc$image` conventions. A scenario shows its MODEL's logos first, then
# its own.
#' @noRd
.container_logo_files <- function(x, override = NULL) {
  if (!is.null(override)) return(.validated_files(override, "logo"))
  own <- .misc_files(x, c("logos", "logo", "image"))
  if (is(x, "scenario")) {
    mod_logos <- tryCatch(.misc_files(x@model, c("logos", "logo", "image")),
                          error = function(e) character(0))
    return(unique(c(mod_logos, own)))
  }
  own
}

# The optional half-page hero figure (misc$figure); override wins.
#' @noRd
.container_figure_file <- function(x, override = NULL) {
  if (!is.null(override)) return(.validated_files(override, "figure")[1])
  f <- .misc_files(x, "figure")
  if (length(f) > 0) f[1] else NULL
}

# Content hashes of branding files for the render key: a changed or
# reordered logo re-renders an otherwise up-to-date report.
#' @noRd
.branding_key <- function(...) {
  files <- unique(unlist(list(...)))
  files <- files[!is.na(files) & nzchar(files)]
  if (length(files) == 0) return(list())
  list(branding = vapply(files, function(f) {
    tryCatch(rlang::hash(readBin(f, "raw", file.size(f))),
             error = function(e) f)
  }, character(1)))
}

# Structural fingerprint of a process: class + sorted commodity sets. Two
# processes with the same key differ only in parameter VALUES -- values are
# deliberately not in the key, so region-replicated technologies group
# together.
#' @noRd
.proc_topology_key <- function(p) {
  gs <- function(sl, col) {
    d <- tryCatch(methods::slot(p, sl), error = function(e) NULL)
    if (!is.data.frame(d) || !col %in% names(d)) return(character(0))
    sort(unique(as.character(stats::na.omit(d[[col]]))))
  }
  paste0(class(p)[1],
         "|in=", paste(gs("input", "comm"), collapse = ","),
         "|out=", paste(gs("output", "comm"), collapse = ","),
         "|stg=", paste(gs("storage", "comm"), collapse = ","),
         "|aux=", paste(gs("aux", "acomm"), collapse = ","))
}

# Human-readable label from a topology key: "COA -> ELC (+CO2)".
#' @noRd
.proc_topology_label <- function(key) {
  part <- function(tag) {
    m <- regmatches(key, regexpr(paste0("\\|", tag, "=[^|]*"), key))
    if (length(m) == 0) return("")
    sub(paste0("^\\|", tag, "="), "", m)
  }
  inp <- part("in"); out <- part("out"); stg <- part("stg"); aux <- part("aux")
  lab <- paste0(if (nzchar(inp)) inp else "-", " -> ",
                if (nzchar(out)) out else if (nzchar(stg))
                  paste0("[", stg, "]") else "-")
  if (nzchar(aux)) lab <- paste0(lab, " (+", aux, ")")
  lab
}

# Merge the per-member .proc_info_df() tables of one topology group into a
# ranges view: numeric singletons collapse to "min - max", small string sets
# to their union, anything else to "varies (n distinct)". Reuses
# .proc_info_df() itself, so the parameter list cannot drift.
#' @noRd
.proc_ranges_df <- function(members) {
  infos <- lapply(members, function(p)
    tryCatch(.proc_info_df(p), error = function(e) NULL))
  infos <- Filter(function(d) is.data.frame(d) && nrow(d) > 0, infos)
  if (length(infos) == 0) return(NULL)
  all_par <- unique(unlist(lapply(infos, function(d) d$parameter)))
  rows <- lapply(all_par, function(pp) {
    vals <- unlist(lapply(infos, function(d)
      d$value[d$parameter == pp]))
    vals <- vals[!is.na(vals) & nzchar(vals)]
    u <- unique(vals)
    num <- suppressWarnings(as.numeric(u))
    rng <- if (length(u) == 1) {
      u
    } else if (all(is.finite(num))) {
      paste(signif(min(num), 4), "-", signif(max(num), 4))
    } else if (length(u) <= 4) {
      paste(u, collapse = "; ")
    } else {
      paste0("varies (", length(u), " distinct)")
    }
    data.frame(parameter = pp, range = rng, members = length(vals),
               stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

# Group processes by topology. One draw() schematic per GROUP, rendered from
# the representative = first member, sorted by name.
#' @noRd
.proc_groups <- function(procs, draw = TRUE) {
  if (length(procs) == 0) return(list())
  keys <- vapply(procs, .proc_topology_key, character(1))
  nms <- vapply(procs, function(p) p@name, character(1))
  groups <- split(seq_along(procs), keys)
  out <- lapply(names(groups), function(k) {
    idx <- groups[[k]][order(nms[groups[[k]]])]
    members <- procs[idx]
    rep_ <- members[[1]]
    draw_file <- if (isTRUE(draw)) tryCatch({
      tf <- tempfile(pattern = "report_draw_", fileext = ".png")
      grDevices::png(tf, width = 900, height = 600, res = 150, bg = "white")
      draw(rep_)
      grDevices::dev.off()
      if (file.exists(tf) && file.info(tf)$size > 5000) tf else NULL
    }, error = function(e) {
      tryCatch(grDevices::dev.off(), error = function(e2) NULL)
      NULL
    }) else NULL
    members_df <- data.frame(
      name = vapply(members, function(p) p@name, character(1)),
      desc = vapply(members, function(p)
        tryCatch(p@desc, error = function(e) ""), character(1)),
      stringsAsFactors = FALSE)
    detail_df <- tryCatch({
      dparts <- lapply(members, function(p) {
        d <- .proc_info_df(p)
        if (is.null(d)) return(NULL)
        keep <- d$parameter %in% c("input", "output", "invcost", "fixom",
                                   "varom", "olife", "start",
                                   "stock (total)")
        d <- d[keep, , drop = FALSE]
        if (nrow(d) == 0) return(NULL)
        w <- as.data.frame(as.list(stats::setNames(d$value, d$parameter)),
                          optional = TRUE, stringsAsFactors = FALSE)
        cbind(data.frame(name = p@name, stringsAsFactors = FALSE), w)
      })
      dparts <- Filter(Negate(is.null), dparts)
      if (length(dparts) == 0) NULL else {
        cols <- unique(unlist(lapply(dparts, names)))
        dparts <- lapply(dparts, function(d) {
          for (cc in setdiff(cols, names(d))) d[[cc]] <- NA
          d[, cols, drop = FALSE]
        })
        do.call(rbind, dparts)
      }
    }, error = function(e) NULL)
    list(key = k, class = class(rep_)[1],
         label = .proc_topology_label(k),
         members = members_df$name, n = length(members),
         representative = rep_@name,
         members_df = members_df,
         ranges_df = .proc_ranges_df(members),
         detail_df = detail_df,
         draw_file = draw_file)
  })
  out[order(-vapply(out, function(g) g$n, integer(1)),
            vapply(out, function(g) g$class, character(1)),
            vapply(out, function(g) g$label, character(1)))]
}

# A container's header image: `misc$logo` first (the report-specific hook),
# then the general `misc$image` convention. Local files only.
#' @noRd
.container_logo_file <- function(x) {
  cand <- c(tryCatch(x@misc$logo, error = function(e) NULL),
            tryCatch(x@misc$image, error = function(e) NULL))
  for (p in cand) {
    if (is.character(p) && length(p) == 1L && nzchar(p) && file.exists(p)) {
      return(normalizePath(p, mustWork = TRUE))
    }
  }
  NULL
}

.report_model <- function(object, template, file, format, dots,
                          open = interactive(), reports_path = NULL,
                          force = FALSE, logos = NULL, figure = NULL) {
  is_model <- inherits(object, "model")
  nm   <- if (nzchar(object@name)) object@name else class(object)[1]

  # branding resolved eagerly (cheap): it also enters the render key, so a
  # changed logo/figure re-renders an otherwise up-to-date report
  logos_r <- .container_logo_files(object, override = logos)
  figure_r <- .container_figure_file(object, override = figure)

  # Everything below is deferred: an up-to-date report never builds it.
  # `declared` = the chosen template's param names; components a template
  # does not declare are never built.
  params_fn <- function(declared = NULL) {
  want <- function(p) is.null(declared) || p %in% declared
  desc <- tryCatch(object@desc, error = function(e) "")

  # -- configuration (model only) ------------------------------------------
  config_df <- NULL
  horizon <- NULL
  discount_df <- calendar_df <- geoscale_df <- NULL
  horizon_plot <- calendar_plot <- NULL
  gg_ok <- requireNamespace("ggplot2", quietly = TRUE)
  geo_ok <- gg_ok && requireNamespace("geoscales", quietly = TRUE) &&
    requireNamespace("sf", quietly = TRUE)
  if (is_model) {
    cfg <- object@config
    horizon <- tryCatch(cfg@horizon, error = function(e) NULL)
    cal     <- tryCatch(cfg@calendar, error = function(e) NULL)
    config_df <- .config_report_rows(cfg)
    discount_df <- .config_discount_df(cfg)
    calendar_df <- if (!is.null(cal)) .calendar_frames_df(cal) else NULL

    if (gg_ok && !is.null(horizon) && want("horizon_plot")) {
      horizon_plot <- tryCatch(ggplot2::autoplot(horizon),
                               error = function(e) NULL)
    }
    if (gg_ok && !is.null(cal) && want("calendar_plot")) {
      calendar_plot <- tryCatch(ggplot2::autoplot(cal),
                                error = function(e) NULL)
    }
  }

  # geoscale summary + figures: geoscales is Suggests, so everything crosses
  # through its exported accessors under a requireNamespace() guard
  geo_map <- geo_stack <- NULL
  if (is_model &&
      (want("geoscale_df") || want("geo_map") || want("geo_stack"))) {
    gs <- tryCatch(getGeoscale(object), error = function(e) NULL)
    if (!is.null(gs) && requireNamespace("geoscales", quietly = TRUE)) {
      if (want("geoscale_df")) {
        geoscale_df <- tryCatch({
          lv <- as.character(geoscales::geoscale_geoframes(gs))
          data.frame(
            geoframe = lv,
            regions = vapply(lv, function(l)
              length(geoscales::geoscale_regions(gs, l)), integer(1)),
            stringsAsFactors = FALSE)
        }, error = function(e) NULL)
      }
      if (geo_ok && want("geo_map")) {
        geo_map <- tryCatch(plot_geoscale(gs, type = "map"),
                            error = function(e) NULL)
      }
      if (geo_ok && want("geo_stack")) {
        nfr <- tryCatch(length(geoscales::geoscale_geoframes(gs)),
                        error = function(e) 1L)
        if (nfr > 1) {
          geo_stack <- tryCatch(plot_geoscale(gs, type = "stack"),
                                error = function(e) NULL)
        }
      }
    }
  }

  gobj <- function(cls) tryCatch(getObjects(object, cls),
                                 error = function(e) list())
  comms <- gobj("commodity"); sups <- gobj("supply"); dems <- gobj("demand")
  stgs  <- gobj("storage");   trds <- gobj("trade")
  cns   <- gobj("constraint"); techs <- gobj("technology")
  exps  <- gobj("export");    imps  <- gobj("import")
  weas  <- gobj("weather");   taxes <- gobj("tax"); subs <- gobj("subsidy")
  csts  <- gobj("costs")

  # every class a repository can hold (`repository@permit`)
  counts_df <- data.frame(
    class = c("commodity", "supply", "demand", "technology", "storage",
              "trade", "export", "import", "weather", "tax", "subsidy",
              "costs", "constraint"),
    count = c(length(comms), length(sups), length(dems), length(techs),
              length(stgs), length(trds), length(exps), length(imps),
              length(weas), length(taxes), length(subs), length(csts),
              length(cns)),
    stringsAsFactors = FALSE)
  counts_df <- counts_df[counts_df$count > 0, , drop = FALSE]

  slot_chr <- function(x, sl) {
    v <- tryCatch(methods::slot(x, sl), error = function(e) NULL)
    v <- as.character(v); v <- v[!is.na(v) & nzchar(v)]
    if (length(v) == 0) "" else paste(unique(v), collapse = ", ")
  }

  comm_df <- if (length(comms) > 0) do.call(rbind, lapply(comms, function(x) {
    em <- if (nrow(x@emis) > 0)
      paste(x@emis$comm, x@emis$emis, x@emis$unit, collapse = "; ") else ""
    data.frame(name = x@name, unit = slot_chr(x, "unit"),
               timeframe = x@timeframe[1], emissions = em,
               desc = slot_chr(x, "desc"), stringsAsFactors = FALSE)
  })) else NULL

  sup_df <- if (length(sups) > 0) do.call(rbind, lapply(sups, function(x) {
    cost <- if (nrow(x@supply) > 0 && "cost" %in% names(x@supply)) {
      v <- x@supply$cost[!is.na(x@supply$cost)]
      if (length(v) > 0) paste(unique(signif(range(v), 4)), collapse = " - ") else ""
    } else ""
    data.frame(name = x@name, commodity = paste(x@commodity, collapse = ", "),
               region = slot_chr(x, "region"), cost = cost,
               stringsAsFactors = FALSE)
  })) else NULL

  dem_df <- if (length(dems) > 0) do.call(rbind, lapply(dems, function(x) {
    tot <- if (nrow(x@demand) > 0 && "demand" %in% names(x@demand)) {
      v <- x@demand$demand[!is.na(x@demand$demand)]
      if (length(v) > 0) signif(sum(v), 4) else NA
    } else NA
    data.frame(name = x@name, commodity = paste(x@commodity, collapse = ", "),
               region = slot_chr(x, "region"), total = tot,
               stringsAsFactors = FALSE)
  })) else NULL

  trd_df <- if (length(trds) > 0) do.call(rbind, lapply(trds, function(x) {
    rt <- tryCatch(x@routes, error = function(e) NULL)
    routes <- if (is.data.frame(rt) && nrow(rt) > 0 &&
                  all(c("src", "dst") %in% names(rt))) {
      paste(paste0(rt$src, " -> ", rt$dst), collapse = "; ")
    } else ""
    data.frame(name = x@name, commodity = paste(x@commodity, collapse = ", "),
               routes = routes, stringsAsFactors = FALSE)
  })) else NULL
  cns_df <- if (length(cns) > 0) data.frame(name = names(cns),
    desc = vapply(cns, function(x)
      tryCatch(x@desc, error = function(e) ""), character(1)),
    stringsAsFactors = FALSE) else NULL

  expimp <- c(exps, imps)
  expimp_df <- if (length(expimp) > 0) do.call(rbind,
    lapply(expimp, function(x) {
      data.frame(name = x@name, class = class(x)[1],
                 commodity = paste(x@commodity, collapse = ", "),
                 desc = slot_chr(x, "desc"), stringsAsFactors = FALSE)
    })) else NULL

  weather_df <- if (length(weas) > 0) do.call(rbind, lapply(weas, function(x) {
    data.frame(name = x@name, timeframe = slot_chr(x, "timeframe"),
               region = slot_chr(x, "region"), desc = slot_chr(x, "desc"),
               stringsAsFactors = FALSE)
  })) else NULL

  pols <- c(taxes, subs)
  policy_df <- if (length(pols) > 0) do.call(rbind, lapply(pols, function(x) {
    data.frame(name = x@name, class = class(x)[1],
               commodity = slot_chr(x, "comm"), desc = slot_chr(x, "desc"),
               stringsAsFactors = FALSE)
  })) else NULL

  windows_plot <- if (gg_ok && want("windows_plot")) tryCatch(
    plot_process_windows(object, horizon = horizon), error = function(e) NULL)
    else NULL

  # -- topology groups: one section per process STRUCTURE ------------------
  # (the grouped view is what the default template shows; per-process
  # `techs` below is built only for templates that still declare it)
  need_groups <- want("proc_groups") || want("windows_group_plot")
  proc_groups <- if (need_groups) tryCatch(
    .proc_groups(c(techs, stgs), draw = want("proc_groups")),
    error = function(e) list()) else list()
  windows_group_n <- length(proc_groups)
  windows_group_plot <- if (gg_ok && want("windows_group_plot") &&
                            windows_group_n > 0) tryCatch({
    .plot_windows_grouped(.proc_windows(object, horizon = horizon),
                          proc_groups)
  }, error = function(e) NULL) else NULL

  # -- per-process sections --------------------------------------------------
  tech_list <- if (!want("techs")) list() else
    lapply(c(techs, stgs), function(p) {
    draw_file <- tryCatch({
      tf <- tempfile(pattern = "report_draw_", fileext = ".png")
      grDevices::png(tf, width = 900, height = 600, res = 150, bg = "white")
      draw(p)
      grDevices::dev.off()
      if (file.exists(tf) && file.info(tf)$size > 5000) tf else NULL
    }, error = function(e) {
      tryCatch(grDevices::dev.off(), error = function(e2) NULL)
      NULL
    })
    list(name = p@name,
         desc = tryCatch(p@desc, error = function(e) ""),
         class = class(p)[1],
         draw_file = draw_file,
         info_df = .proc_info_df(p))
  })

  list(
    title      = paste0(if (is_model) "Model report: " else
      "Repository report: ", nm),
    model_name = nm, model_desc = desc,
    image_file = .container_logo_file(object),
    logos = logos_r, figure = figure_r,
    config_df  = config_df, discount_df = discount_df,
    horizon_plot = horizon_plot, calendar_plot = calendar_plot,
    calendar_df = calendar_df, geoscale_df = geoscale_df,
    counts_df = counts_df,
    comm_df = comm_df, sup_df = sup_df, dem_df = dem_df,
    trd_df = trd_df, expimp_df = expimp_df, weather_df = weather_df,
    policy_df = policy_df, cns_df = cns_df,
    windows_plot = windows_plot, techs = tech_list,
    proc_groups = proc_groups,
    windows_group_plot = windows_group_plot,
    windows_group_n = windows_group_n,
    geo_map = geo_map, geo_stack = geo_stack)
  }

  tmpl <- template %||% .report_misc_template(object) %||% "model"
  .report_render(tmpl, params_fn, file, format, dots, nm, open = open,
                 class = "model", owner = object, reports_path = reports_path,
                 force = force,
                 engine = if (is_model) "report/model" else "report/repository",
                 key_args = .branding_key(logos_r, figure_r))
}

.report_scenario <- function(scen, template, file, format, dots,
                             open = interactive(), reports_path = NULL,
                             force = FALSE, run = NULL, verify = TRUE,
                             levcost = NULL, logos = NULL, badges = NULL) {
  nm <- if (nzchar(scen@name)) scen@name else "scenario"

  # branding: model logos first, then the scenario's own; badges are the
  # scenario's small property indicators. Resolved eagerly -- they enter
  # the render key so a change re-renders.
  logos_r <- .container_logo_files(scen, override = logos)
  badges_r <- if (!is.null(badges)) .validated_files(badges, "badge") else
    .misc_files(scen, "badges")

  # -- run selection (layout 3) --------------------------------------------
  # `run =` reports a non-active run under runs/<variant>/<solve>/ by
  # switching a LOCAL copy of the scenario; the caller's object and its
  # active run are never touched.
  runs_df <- tryCatch(as.data.frame(scenario_runs(scen)),
                      error = function(e) NULL)
  if (!is.null(run)) {
    if (is.null(runs_df) || !run %in% runs_df$run) {
      stop("report(): unknown run '", run, "'. Available runs: ",
           if (is.null(runs_df) || nrow(runs_df) == 0) "(none)" else
             paste0('"', runs_df$run, '"', collapse = ", "), call. = FALSE)
    }
    active <- runs_df$run[which(runs_df$active)]
    if (!identical(run, active[1] %||% NA_character_)) {
      scen <- suppressMessages(read_solution(scen, run = run))
    }
  }
  run_id <- if (!is.null(run)) run else {
    a <- tryCatch(runs_df$run[which(runs_df$active)], error = function(e) NULL)
    if (length(a) > 0) a[1] else NA_character_
  }

  # Everything below is deferred: an up-to-date report never builds it.
  # `declared` = the chosen template's param names; a component the template
  # does not declare is never built (maps and levcost are expensive).
  params_fn <- function(declared = NULL) {
  want <- function(p) is.null(declared) || p %in% declared
  gg_ok <- requireNamespace("ggplot2", quietly = TRUE)
  geo_ok <- gg_ok && requireNamespace("geoscales", quietly = TRUE) &&
    requireNamespace("sf", quietly = TRUE)
  gd <- function(v) tryCatch(
    as.data.frame(getData(scen, name = v, merge = TRUE, drop.zeros = FALSE)),
    error = function(e) NULL)

  obj    <- gd("vObjective")
  active_row <- if (!is.null(runs_df) && any(runs_df$active))
    runs_df[which(runs_df$active)[1], , drop = FALSE] else NULL
  rr <- function(col) {
    if (!is.null(run) && !is.null(runs_df) && run %in% runs_df$run) {
      v <- runs_df[[col]][runs_df$run == run][1]
    } else if (!is.null(active_row) && col %in% names(active_row)) {
      v <- active_row[[col]][1]
    } else v <- NA
    if (is.null(v) || is.na(v)) "" else as.character(v)
  }
  status_df <- data.frame(
    item  = c("scenario", "model", "run", "variant", "solver",
              "solved (optimal)", "solve started", "duration (s)",
              "objective (total discounted cost)"),
    value = c(nm,
              tryCatch(scen@model@name, error = function(e) ""),
              if (is.na(run_id)) "" else run_id,
              tryCatch(scen@misc$variant %||% "", error = function(e) ""),
              paste0(rr("solver_name"),
                     if (nzchar(rr("lang"))) paste0(" (", rr("lang"), ")")),
              as.character(isTRUE(scen@status$optimal)),
              rr("started"), rr("duration_sec"),
              if (!is.null(obj) && nrow(obj) > 0)
                format(signif(obj$value[1], 6), big.mark = ",") else "n/a"),
    stringsAsFactors = FALSE)
  status_df <- status_df[nzchar(status_df$value), , drop = FALSE]

  # -- run provenance ------------------------------------------------------
  run_info_df <- if (!is.na(run_id)) tryCatch({
    ri <- scenario_run_info(scen, run_id)
    ri <- ri[!vapply(ri, is.list, logical(1))]
    data.frame(item = names(ri),
               value = vapply(ri, function(z)
                 paste(format(z), collapse = ", "), character(1)),
               stringsAsFactors = FALSE)
  }, error = function(e) NULL) else NULL

  # -- problem size (input side) -------------------------------------------
  size_df <- if (want("size_df")) tryCatch({
    ms <- model_size(scen)
    data.frame(
      item = c("value parameters", "parameter rows",
               "variables (estimate)", "constraints (estimate)"),
      value = format(c(ms$n_param, ms$param_rows, ms$n_var_est, ms$n_con_est),
                     big.mark = ","),
      stringsAsFactors = FALSE)
  }, error = function(e) NULL) else NULL

  # -- solution checks -----------------------------------------------------
  verify_df <- if (isTRUE(verify) && want("verify_df")) tryCatch({
    vr <- verify_solution(scen)
    do.call(rbind, lapply(names(vr$checks), function(k) {
      ck <- vr$checks[[k]]
      data.frame(check = k, status = as.character(ck$status),
                 rows = ck$n %||% NA_integer_,
                 violations = if (!is.null(ck$violations))
                   nrow(ck$violations) else 0L,
                 note = ck$reason %||% "", stringsAsFactors = FALSE)
    }))
  }, error = function(e) NULL) else NULL

  ap <- function(...) if (gg_ok)
    tryCatch(ggplot2::autoplot(scen, ...), error = function(e) NULL) else NULL
  # top_n pinned explicitly so report output never drifts with the
  # autoplot default; keeps legends readable on large models
  gen_plot    <- if (want("gen_plot")) ap("generation", top_n = 12) else NULL
  cap_plot    <- if (want("cap_plot")) ap("capacity", top_n = 12) else NULL
  newcap_plot <- if (want("newcap_plot")) ap("new_capacity", top_n = 12)
    else NULL
  # dispatch profile over a sub-annual sample when the calendar has one
  gen_day_plot <- NULL
  sl <- if (want("gen_day_plot"))
    tryCatch(unique(.mix_fetch(scen, "vTechOut", native = TRUE)$timeslice),
             error = function(e) NULL) else NULL
  sl <- setdiff(sl, "ANNUAL")
  if (length(sl) > 1) {
    pref <- sub("_.*$", "", sl[1])
    gen_day_plot <- ap("generation", timeslice = paste0("^", pref, "_"),
                       top_n = 12)
  }

  emis <- if (want("emis_df") || want("emis_total_df")) gd("vEmsFuelTot")
    else NULL
  emis_df <- if (!is.null(emis) && nrow(emis) > 0 && want("emis_df")) {
    ag <- stats::aggregate(value ~ comm + year, emis, sum)
    ag$value <- signif(ag$value, 5); ag
  } else NULL
  emis_total_df <- if (!is.null(emis) && nrow(emis) > 0 &&
                       want("emis_total_df")) tryCatch({
    ag <- stats::aggregate(value ~ comm, emis, sum)
    names(ag)[2] <- "total"
    rbind(ag, data.frame(comm = "TOTAL", total = sum(ag$total)))
  }, error = function(e) NULL) else NULL

  # -- costs, role-driven --------------------------------------------------
  # The variable catalogue (data-raw/variables.yml) declares what each
  # variable IS; every solved variable with role "cost" enters the breakdown
  # by name, so new cost variables appear without touching the report.
  cost_vars <- if (want("cost_df") || want("costs_plot") ||
                   want("cost_total_df")) tryCatch({
    vs <- scen@modOut@variables
    keep <- vapply(vs, function(v)
      identical(tryCatch(v@role, error = function(e) NA_character_), "cost"),
      logical(1))
    setdiff(names(vs)[keep], c("vObjective", "vTotalCost"))
  }, error = function(e) character()) else character()
  cost_df <- NULL
  if (length(cost_vars) > 0) {
    parts <- lapply(cost_vars, function(v) {
      d <- gd(v)
      if (is.null(d) || nrow(d) == 0 || !"value" %in% names(d)) return(NULL)
      ag <- if ("year" %in% names(d)) stats::aggregate(value ~ year, d, sum)
        else data.frame(year = NA_integer_, value = sum(d$value, na.rm = TRUE))
      if (all(ag$value == 0)) return(NULL)
      cbind(data.frame(variable = v, stringsAsFactors = FALSE), ag)
    })
    parts <- Filter(Negate(is.null), parts)
    if (length(parts) > 0) {
      cost_df <- do.call(rbind, parts)
      cost_df$value <- signif(cost_df$value, 5)
    }
  }
  # fall back to the yearly total when no per-component variable was solved
  if (is.null(cost_df) && (want("cost_df") || want("cost_total_df"))) {
    cost <- gd("vTotalCost")
    cost_df <- if (!is.null(cost) && nrow(cost) > 0) {
      ag <- stats::aggregate(value ~ year, cost, sum)
      cbind(data.frame(variable = "vTotalCost", stringsAsFactors = FALSE),
            data.frame(year = ag$year, value = signif(ag$value, 6)))
    } else NULL
  }
  costs_plot <- if (gg_ok && !is.null(cost_df) &&
                    length(unique(cost_df$variable)) > 1 &&
                    !all(is.na(cost_df$year))) {
    tryCatch(
      ggplot2::ggplot(cost_df,
                      ggplot2::aes(.data$year, .data$value,
                                   fill = .data$variable)) +
        ggplot2::geom_col() +
        ggplot2::labs(x = NULL, y = "cost", fill = NULL) +
        theme_energyRt(),
      error = function(e) NULL)
  } else NULL
  cost_total_df <- if (!is.null(cost_df) && want("cost_total_df")) tryCatch({
    ag <- stats::aggregate(value ~ variable, cost_df, sum)
    names(ag)[2] <- "total"
    rbind(ag, data.frame(variable = "TOTAL", total = sum(ag$total)))
  }, error = function(e) NULL) else NULL

  # -- variants ------------------------------------------------------------
  variants_df <- if (want("variants_df")) tryCatch({
    tv <- getVariants(scen)
    if (!is.null(tv) && nrow(tv) > 0) {
      agg_by <- intersect(c("class", "base"), names(tv))
      if (length(agg_by) > 0) {
        ag <- stats::aggregate(list(variants = seq_len(nrow(tv))),
                               by = tv[agg_by], FUN = length)
        utils::head(ag[order(-ag$variants), , drop = FALSE], 25L)
      } else NULL
    } else NULL
  }, error = function(e) NULL) else NULL

  # -- levelized costs (opt-in: levcost = TRUE) ----------------------------
  levcost_df <- if (isTRUE(levcost) && want("levcost_df")) tryCatch({
    lc <- suppressMessages(suppressWarnings(energyRt::levcost(scen)))
    if (inherits(lc, "levcost")) lc <- list(lc)
    rows <- lapply(lc, function(z) {
      npv <- z[["levcost_npv"]]
      if (is.null(npv) || length(npv) == 0) return(NULL)
      data.frame(process = names(npv)[1] %||% "",
                 levcost_npv = signif(as.numeric(npv)[1], 5),
                 stringsAsFactors = FALSE)
    })
    rows <- Filter(Negate(is.null), rows)
    if (length(rows) > 0) {
      lcdf <- do.call(rbind, rows)
      # most expensive first, so a template cap shows what matters
      lcdf[order(-lcdf$levcost_npv), , drop = FALSE]
    } else NULL
  }, error = function(e) NULL) else NULL

  # -- multiple runs: objective chart + comparison pointer -----------------
  runs_plot <- runs_note <- NULL
  if (!is.null(runs_df) && nrow(runs_df) > 1) {
    if (want("runs_plot") && gg_ok) {
      runs_plot <- tryCatch({
        rd <- runs_df[!is.na(runs_df$objective), , drop = FALSE]
        ggplot2::ggplot(rd, ggplot2::aes(.data$objective, .data$run,
                                         fill = .data$active)) +
          ggplot2::geom_col(show.legend = FALSE) +
          ggplot2::labs(x = "objective", y = NULL,
                        title = "Recorded runs") +
          theme_energyRt()
      }, error = function(e) NULL)
    }
    if (want("runs_note")) {
      runs_note <- paste0(
        "Compare runs side by side with compare_scenarios(scen, runs = c(",
        paste0('"', runs_df$run, '"', collapse = ", "), ")).")
    }
  }

  # -- time structure: calendars + horizon ---------------------------------
  calendar_plot <- calendar_model_plot <- NULL
  if (gg_ok && (want("calendar_plot") || want("calendar_model_plot"))) {
    scen_cal <- tryCatch(scen@settings@calendar, error = function(e) NULL)
    mod_cal <- tryCatch(scen@model@config@calendar, error = function(e) NULL)
    cal_differs <- !is.null(scen_cal) && !is.null(mod_cal) &&
      (!identical(scen_cal@name, mod_cal@name) ||
         !identical(tryCatch(nrow(scen_cal@timeslice_share),
                             error = function(e) NA),
                    tryCatch(nrow(mod_cal@timeslice_share),
                             error = function(e) NA)))
    if (want("calendar_plot") && !is.null(scen_cal)) {
      calendar_plot <- tryCatch({
        if (cal_differs) plot_calendar(scen_cal, reference = mod_cal) else
          ggplot2::autoplot(scen_cal)
      }, error = function(e) NULL)
    }
    if (want("calendar_model_plot") && cal_differs && !is.null(mod_cal)) {
      calendar_model_plot <- tryCatch(ggplot2::autoplot(mod_cal),
                                      error = function(e) NULL)
    }
  }
  horizon_plot <- if (want("horizon_plot") && gg_ok) tryCatch(
    ggplot2::autoplot(scen@settings@horizon), error = function(e) NULL)
    else NULL
  horizon_df <- if (want("horizon_df")) tryCatch({
    as.data.frame(scen@settings@horizon@intervals)
  }, error = function(e) NULL) else NULL

  # -- geography: geoscale map + layered stack -----------------------------
  geo_map <- geo_stack <- geo_note <- NULL
  if (geo_ok && (want("geo_map") || want("geo_stack") || want("geo_note"))) {
    gs <- tryCatch(getGeoscale(scen), error = function(e) NULL)
    if (!is.null(gs)) {
      if (want("geo_map")) {
        geo_map <- tryCatch(plot_geoscale(gs, type = "map"),
                            error = function(e) NULL)
      }
      if (want("geo_stack")) {
        nfr <- tryCatch(length(geoscales::geoscale_geoframes(gs)),
                        error = function(e) 1L)
        if (nfr > 1) {
          geo_stack <- tryCatch(plot_geoscale(gs, type = "stack"),
                                error = function(e) NULL)
        }
      }
      if (want("geo_note")) {
        geo_note <- tryCatch({
          mode <- .spatial_sample_mode(gs, scen@settings)
          paste0("geoframes: ",
                 paste(geoscales::geoscale_geoframes(gs), collapse = " > "),
                 "; spatial sample: ", mode)
        }, error = function(e) NULL)
      }
    }
  }

  # -- result maps ---------------------------------------------------------
  newcap_map <- if (want("newcap_map") && geo_ok) tryCatch(
    plot_map(scen, name = "vTechNewCap", facet = "year"),
    error = function(e) NULL) else NULL
  ret_map <- if (want("ret_map") && geo_ok) tryCatch({
    d <- gd("vTechRetiredStock")
    if (!is.null(d) && nrow(d) > 0 && any(d$value != 0)) {
      plot_map(scen, name = "vTechRetiredStock", facet = "year")
    } else NULL
  }, error = function(e) NULL) else NULL

  # -- retirement / inputs-outputs bars ------------------------------------
  ret_bars <- if (want("ret_bars") && gg_ok) tryCatch({
    parts <- Filter(Negate(is.null), list(
      tech = gd("vTechRetiredStock"),
      storage = gd("vStorageStgRetiredStock")))
    parts <- lapply(names(parts), function(k) {
      d <- parts[[k]]
      if (is.null(d) || nrow(d) == 0 || !"year" %in% names(d)) return(NULL)
      ag <- stats::aggregate(value ~ year, d, sum)
      ag$class <- k
      ag
    })
    parts <- Filter(function(d) !is.null(d) && any(d$value != 0), parts)
    if (length(parts) == 0) NULL else {
      rb <- do.call(rbind, parts)
      ggplot2::ggplot(rb, ggplot2::aes(factor(.data$year), .data$value,
                                       fill = .data$class)) +
        ggplot2::geom_col() +
        ggplot2::labs(x = NULL, y = "retired capacity", fill = NULL,
                      title = "Retirements by year") +
        theme_energyRt()
    }
  }, error = function(e) NULL) else NULL

  io_bars <- if (want("io_bars") && gg_ok) tryCatch({
    inp <- gd("vTechInp")
    out <- gd("vTechOut")
    parts <- list()
    if (!is.null(inp) && nrow(inp) > 0) {
      a <- stats::aggregate(value ~ comm + year, inp, sum)
      a$value <- -a$value
      parts$inp <- a
    }
    if (!is.null(out) && nrow(out) > 0) {
      parts$out <- stats::aggregate(value ~ comm + year, out, sum)
    }
    if (length(parts) == 0) NULL else {
      io <- do.call(rbind, parts)
      ggplot2::ggplot(io, ggplot2::aes(factor(.data$year), .data$value,
                                       fill = .data$comm)) +
        ggplot2::geom_col() +
        ggplot2::geom_hline(yintercept = 0, linewidth = 0.3) +
        ggplot2::labs(x = NULL, y = "inputs (-) / outputs (+)", fill = NULL,
                      title = "Process inputs and outputs") +
        theme_energyRt()
    }
  }, error = function(e) NULL) else NULL

  desc <- tryCatch(scen@desc, error = function(e) "")
  if (is.null(desc) || length(desc) == 0) desc <- ""
  list(
    title = paste0("Scenario report: ", nm),
    scen_name = nm, scen_desc = desc,
    image_file = .container_logo_file(scen),
    logos = logos_r, badges = badges_r,
    status_df = status_df, runs_df = runs_df, run_info_df = run_info_df,
    runs_plot = runs_plot, runs_note = runs_note,
    size_df = size_df, verify_df = verify_df,
    calendar_plot = calendar_plot,
    calendar_model_plot = calendar_model_plot,
    horizon_plot = horizon_plot, horizon_df = horizon_df,
    geo_map = geo_map, geo_stack = geo_stack, geo_note = geo_note,
    newcap_map = newcap_map, ret_map = ret_map,
    gen_plot = gen_plot, gen_day_plot = gen_day_plot,
    cap_plot = cap_plot, newcap_plot = newcap_plot,
    ret_bars = ret_bars, io_bars = io_bars,
    emis_df = emis_df, emis_total_df = emis_total_df,
    cost_df = cost_df, cost_total_df = cost_total_df,
    costs_plot = costs_plot,
    variants_df = variants_df, levcost_df = levcost_df)
  }

  tmpl <- template %||% .report_misc_template(scen) %||% "scenario"
  .report_render(tmpl, params_fn, file, format, dots, nm, open = open,
                 class = "scenario", owner = scen, reports_path = reports_path,
                 force = force, engine = "report/scenario",
                 key_args = .branding_key(logos_r, badges_r))
}

#' @rdname report
#' @export
setMethod("report", "repository",
  function(object, template = NULL, image_file = NULL, file = NULL,
           format = c("html", "pdf", "tex", "docx"), levcost = NULL, cost_unit = NULL,
           open = interactive(), reports_path = NULL, force = FALSE,
           logos = NULL, figure = NULL, ...) {
    if (missing(format)) format <- "html"
    dots <- list(...); name <- dots[["name"]]; dots[["name"]] <- NULL
    if (is.null(name)) {
      return(.report_model(object, template, file, format, dots, open = open,
                           reports_path = reports_path, force = force,
                           logos = logos, figure = figure))
    }
    .report_container(object, object, object, template, image_file, file, format,
                      levcost, cost_unit, name, dots, open = open,
                      reports_path = reports_path, force = force)
  })

#' @rdname report
#' @export
setMethod("report", "model",
  function(object, template = NULL, image_file = NULL, file = NULL,
           format = c("html", "pdf", "tex", "docx"), levcost = NULL, cost_unit = NULL,
           open = interactive(), reports_path = NULL, force = FALSE,
           logos = NULL, figure = NULL, ...) {
    if (missing(format)) format <- "html"
    dots <- list(...); name <- dots[["name"]]; dots[["name"]] <- NULL
    if (is.null(name)) {
      return(.report_model(object, template, file, format, dots, open = open,
                           reports_path = reports_path, force = force,
                           logos = logos, figure = figure))
    }
    .report_container(object, object, object, template, image_file, file, format,
                      levcost, cost_unit, name, dots, open = open,
                      reports_path = reports_path, force = force)
  })

#' @rdname report
#' @export
setMethod("report", "scenario",
  function(object, template = NULL, image_file = NULL, file = NULL,
           format = c("html", "pdf", "tex", "docx"), levcost = NULL, cost_unit = NULL,
           open = interactive(), reports_path = NULL, force = FALSE,
           run = NULL, verify = TRUE, logos = NULL, badges = NULL, ...) {
    if (missing(format)) format <- "html"
    dots <- list(...); name <- dots[["name"]]; dots[["name"]] <- NULL
    if (is.null(name)) {
      return(.report_scenario(object, template, file, format, dots, open = open,
                              reports_path = reports_path, force = force,
                              run = run, verify = verify, levcost = levcost,
                              logos = logos, badges = badges))
    }
    .report_container(object, object@model, object, template, image_file, file,
                      format, levcost, cost_unit, name, dots, open = open,
                      reports_path = reports_path, force = force)
  })

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
setMethod("report_pdf", "ANY", function(object, ...) {
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
setMethod("report_html", "ANY", function(object, ...) {
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
setMethod("report_tex", "ANY", function(object, ...) {
  report(object, ..., format = "tex")
})

#' Generate a Word report for an energyRt object
#'
#' Thin wrapper around \code{\link{report}} that fixes \code{format = "docx"}.
#'
#' @inheritParams report
#' @return Path to the generated \code{.docx} file (invisibly).
#' @export
setGeneric("report_docx", function(object, ...) standardGeneric("report_docx"))

#' @rdname report_docx
#' @export
setMethod("report_docx", "ANY", function(object, ...) {
  report(object, ..., format = "docx")
})

# Split leftover ... into template params vs rmarkdown::render() args;
# anything else is dropped with a warning rather than crashing deep inside
# render() ("unused arguments") — old scripts may carry arguments from
# retired templates (e.g. img_height / n_columns).
#' @noRd
.report_sort_dots <- function(dots, tmpl) {
  if (length(dots) == 0) return(list(params = list(), render = list()))
  tmpl_params <- tryCatch(
    names(rmarkdown::yaml_front_matter(tmpl)$params),
    error = function(e) character())
  is_param  <- names(dots) %in% tmpl_params
  is_render <- !is_param &
    names(dots) %in% names(formals(rmarkdown::render))
  bad <- names(dots)[!is_param & !is_render]
  if (length(bad) > 0) {
    warning("report(): ignoring unknown argument(s): ",
            paste(bad, collapse = ", "),
            " (not template params or rmarkdown::render() arguments)",
            call. = FALSE)
  }
  list(params = dots[is_param], render = dots[is_render])
}

# ── template finder ────────────────────────────────────────────────────────────

# The shipped templates directory: installed package path, with a dev-mode
# fallback to inst/templates.
#' @noRd
.report_templates_dir <- function() {
  tdir <- suppressWarnings(tryCatch(
    system.file("templates", package = "energyRt"), error = function(e) ""))
  if (nzchar(tdir) && dir.exists(tdir)) return(tdir)
  this_dir <- tryCatch(dirname(normalizePath(sys.frame(0)$ofile)),
                       error = function(e) ".")
  for (p in c(file.path(this_dir, "..", "inst", "templates"),
              file.path("inst", "templates"))) {
    p2 <- tryCatch(normalizePath(p, mustWork = TRUE), error = function(e) NULL)
    if (!is.null(p2) && dir.exists(p2)) return(p2)
  }
  NULL
}

#' Locate a report Rmd template by name
#' @param name Character template name (e.g. \code{"generic"}).
#' @param class Optional class scope: with \code{class = "model"},
#'   \code{name = "summary"} resolves \code{report_model_summary.Rmd} first,
#'   then falls back to the unscoped \code{report_summary.Rmd}.
#' @return Absolute path to the \code{.Rmd} file.
#' @noRd
.find_report_template <- function(name, class = NULL) {
  fnames <- paste0("report_", name, ".Rmd")
  if (!is.null(class) && !identical(class, name)) {
    fnames <- c(paste0("report_", class, "_", name, ".Rmd"), fnames)
  }
  tdir <- .report_templates_dir()
  if (!is.null(tdir)) {
    for (fname in fnames) {
      p <- file.path(tdir, fname)
      if (file.exists(p)) return(normalizePath(p, mustWork = TRUE))
    }
  }
  # list what actually ships rather than a hardcoded (stale) set
  avail <- if (is.null(tdir)) character() else
    sub("^report_(.*)[.]Rmd$", "\\1",
        list.files(tdir, pattern = "^report_.*[.]Rmd$"))
  stop("Report template '", fnames[length(fnames)], "' not found.\n",
       "Expected location: inst/templates/", fnames[length(fnames)], "\n",
       "Available built-in templates: ",
       paste0('"', avail, '"', collapse = ", "))
}

#' List the available report templates
#'
#' Scans the shipped templates directory (`inst/templates/` of the installed
#' package) and returns one row per template. The `class` column says which
#' object class a template is designed for; it is read from the optional
#' `report-class:` field of the template's YAML front matter, falling back to
#' inference from the file name. Use the `name` as the `template =` argument
#' of [report()]; a template designed for another class can still be forced by
#' passing its file path.
#'
#' @param class Optional filter: return only templates whose `class` matches
#'   (e.g. `"model"`, `"scenario"`, `"process"`).
#' @return A data.frame with columns `name`, `class`, `title`, `path`.
#' @examples
#' report_templates()
#' @export
report_templates <- function(class = NULL) {
  tdir <- .report_templates_dir()
  files <- if (is.null(tdir)) character() else
    list.files(tdir, pattern = "^report_.*[.]Rmd$", full.names = TRUE)
  known <- c("model", "scenario", "repository", "process",
             "technology", "storage", .REPORT_ELEMENT_CLASSES)
  rows <- lapply(files, function(f) {
    base <- sub("^report_(.*)[.]Rmd$", "\\1", basename(f))
    fm <- tryCatch(rmarkdown::yaml_front_matter(f), error = function(e) list())
    cls <- fm[["report-class"]]
    if (is.null(cls)) {
      # infer from the file name: report_<class>_<name>.Rmd or a bare name
      tok <- strsplit(base, "_", fixed = TRUE)[[1]]
      cls <- if (tok[1] %in% known) tok[1] else NA_character_
    }
    name <- base
    if (!is.na(cls) && startsWith(base, paste0(cls, "_"))) {
      name <- substr(base, nchar(cls) + 2L, nchar(base))
    }
    ttl <- fm[["title"]]
    if (!is.null(ttl) && grepl("`r ", ttl, fixed = TRUE)) {
      ttl <- fm[["params"]][["title"]]      # inline-R title: use its default
    }
    data.frame(name = name, class = as.character(cls),
               title = ttl %||% NA_character_,
               path = f, stringsAsFactors = FALSE)
  })
  out <- if (length(rows) == 0) {
    data.frame(name = character(), class = character(), title = character(),
               path = character(), stringsAsFactors = FALSE)
  } else {
    do.call(rbind, rows)
  }
  if (!is.null(class)) out <- out[!is.na(out$class) & out$class %in% class, ,
                                  drop = FALSE]
  rownames(out) <- NULL
  out
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

  # Key-parameter scalars are only honest when the value is unambiguous; a
  # vintaged technology carries per-vintage windows/lives, which belong in
  # the vintages TABLE, not in a scalar that silently shows the first row.
  .ls1 <- function(col) {
    d <- .lifespan_resolve(object, col)
    v <- unique(d[[col]])           # NA kept: an open window IS a value
    if (length(v) == 1 && !is.na(v)) v else NA_integer_
  }
  olife_val <- .ls1("olife")
  start_val <- .ls1("start")
  end_val   <- .ls1("end")

  vintage_df <- .report_drop_empty_cols(as.data.frame(object@vintage))

  # with several vintages the resolver only sees the non-NA rows, so a lone
  # closed window would masquerade as THE window -- the vintages table owns
  # this detail, the scalars stay blank
  multi_vin <- !is.null(vintage_df) && "vintage" %in% names(vintage_df) &&
    length(unique(vintage_df$vintage[!is.na(vintage_df$vintage)])) > 1
  if (multi_vin) {
    olife_val <- NA_integer_
    start_val <- NA_integer_
    end_val   <- NA_integer_
  }

  cap_df <- object@capacity
  if (nrow(cap_df) > 0) {
    value_cols <- setdiff(names(cap_df), c("region", "year"))
    keep <- rowSums(!is.na(cap_df[, value_cols, drop = FALSE])) > 0
    cap_df <- cap_df[keep, , drop = FALSE]
  }
  cap_df <- .report_drop_empty_cols(cap_df)
  stock_val <- if (!is.null(cap_df) && "stock" %in% names(cap_df)) {
    v <- cap_df$stock[!is.na(cap_df$stock)]; if (length(v) > 0) v[1] else NA_real_
  } else NA_real_

  input_df  <- .report_drop_empty_cols(object@input)
  output_df <- .report_drop_empty_cols(object@output)

  ceff_df <- if (nrow(object@ceff) > 0) {
    preferred_cols <- c("comm", "vintage", "cluster", "cinp2use", "use2cact",
                        "cact2cout", "cinp2ginp", "share.lo", "share.up",
                        "share.fx")
    cc  <- intersect(preferred_cols, names(object@ceff))
    .report_drop_empty_cols(object@ceff[, cc, drop = FALSE])
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
    vintage_df  = vintage_df,
    cap_df      = cap_df,
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

#' Build the params list passed to rmarkdown::render (generic storage)
#' Fills the same contract as `.tech_report_params_generic()` plus the
#' storage-only optional params of the generic template (reservoir_df,
#' seff_df, duration_df, storage_cost_df); the technology-only params
#' stay NULL/NA so their sections are skipped.
#' @noRd
.storage_report_params_generic <- function(object, image_file = NULL,
                                           draw_file = NULL) {
  name <- if (nzchar(object@name)) object@name else "(unnamed)"
  desc <- if (nzchar(object@desc)) object@desc else ""

  # role slots declare comm/unit/cap2act; there is no @units slot
  role1 <- function(df, col) {
    if (is.data.frame(df) && col %in% names(df) && nrow(df) > 0) {
      v <- df[[col]][!is.na(df[[col]])]
      if (length(v) > 0) v[1] else NA
    } else NA
  }
  cap2act <- suppressWarnings(as.numeric(role1(object@output, "cap2act")))
  units_act <- role1(object@output, "unit")
  if (is.na(units_act)) units_act <- ""

  .ls1 <- function(col) {
    d <- .lifespan_resolve(object, col)
    v <- unique(d[[col]])
    if (length(v) == 1 && !is.na(v)) v else NA_integer_
  }
  olife_val <- .ls1("olife")
  start_val <- .ls1("start")
  end_val   <- .ls1("end")
  vintage_df <- .report_drop_empty_cols(as.data.frame(object@vintage))
  multi_vin <- !is.null(vintage_df) && "vintage" %in% names(vintage_df) &&
    length(unique(vintage_df$vintage[!is.na(vintage_df$vintage)])) > 1
  if (multi_vin) {
    olife_val <- NA_integer_
    start_val <- NA_integer_
    end_val   <- NA_integer_
  }

  # costs: fold the part prefixes into tidy rows the template appends to
  # its cost table ("Investment cost (stg)" etc.)
  part_label <- c(stg = "stg", inp = "inp", out = "out")
  tidy_part_costs <- function(df, cols, label) {
    if (!is.data.frame(df) || nrow(df) == 0) return(NULL)
    rows <- list()
    for (cc in intersect(cols, names(df))) {
      v <- suppressWarnings(as.numeric(df[[cc]]))
      if (!any(is.finite(v))) next
      part <- sub("^(stg|inp|out)\\..*$", "\\1", cc)
      part <- if (part %in% names(part_label)) part_label[[part]] else cc
      rows[[length(rows) + 1L]] <- data.frame(
        cost = paste0(label, " (", part, ")"),
        vintage = if ("vintage" %in% names(df)) df$vintage else NA,
        cluster = if ("cluster" %in% names(df)) df$cluster else NA,
        region  = if ("region" %in% names(df)) df$region else NA,
        year    = if ("year" %in% names(df)) df$year else NA,
        value   = v,
        stringsAsFactors = FALSE
      )
    }
    if (length(rows) == 0) return(NULL)
    do.call(rbind, rows)
  }
  storage_cost_df <- do.call(rbind, Filter(Negate(is.null), list(
    tidy_part_costs(object@invcost,
                    c("stg.invcost", "inp.invcost", "out.invcost"),
                    "Investment cost"),
    tidy_part_costs(object@fixom,
                    c("stg.fixom", "inp.fixom", "out.fixom"),
                    "Fixed O&M"),
    tidy_part_costs(object@varom,
                    c("stgcost", "inpcost", "outcost"),
                    "Variable O&M")
  )))
  if (!is.null(storage_cost_df)) {
    storage_cost_df <- storage_cost_df[!is.na(storage_cost_df$value), ,
                                       drop = FALSE]
    if (nrow(storage_cost_df) == 0) storage_cost_df <- NULL
  }

  seff_df <- if (nrow(object@seff) > 0) {
    cc <- intersect(c("vintage", "cluster", "region", "year", "timeslice",
                      "stgeff", "inpeff", "outeff"), names(object@seff))
    .report_drop_empty_cols(object@seff[, cc, drop = FALSE])
  } else NULL

  duration_df <- .report_drop_empty_cols(as.data.frame(object@duration))

  cap_df <- .report_drop_empty_cols(as.data.frame(object@capacity))

  list(
    name        = name,
    desc        = desc,
    cap2act     = if (is.finite(cap2act)) cap2act else NA_real_,
    units_cap   = "",
    units_act   = as.character(units_act),
    units_costs = "",
    olife       = olife_val,
    stock       = NA_real_,
    start       = start_val,
    end         = end_val,
    vintage_df  = vintage_df,
    cap_df      = cap_df,
    input_df    = .report_drop_empty_cols(as.data.frame(object@input)),
    output_df   = .report_drop_empty_cols(as.data.frame(object@output)),
    reservoir_df = .report_drop_empty_cols(as.data.frame(object@storage)),
    ceff_df     = NULL,
    seff_df     = seff_df,
    duration_df = duration_df,
    invcost_df  = NULL,
    fixom_df    = NULL,
    varom_df    = NULL,
    storage_cost_df = storage_cost_df,
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

#' Drop all-NA / all-empty-string columns, then all-NA rows; NULL when nothing
#' remains. The one pruning rule every report table goes through, so no
#' template ever prints a column of NA.
#' @noRd
.report_drop_empty_cols <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)
  keep <- vapply(df, function(x) {
    if (is.character(x)) any(!is.na(x) & nzchar(x)) else any(!is.na(x))
  }, logical(1))
  df <- df[, keep, drop = FALSE]
  if (ncol(df) == 0) return(NULL)
  df <- df[rowSums(!is.na(df)) > 0, , drop = FALSE]
  if (nrow(df) == 0) NULL else df
}

#' Keep only relevant columns from a cost data.frame, dropping all-NA rows.
#' `vintage`/`cluster` are KEPT: collapsing them silently replaced a
#' per-vintage invcost table with its first row.
#' @noRd
.clean_cost_df_report <- function(df, cost_col) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)
  keep_cols <- intersect(c("vintage", "cluster", "region", "year", cost_col),
                         names(df))
  df <- df[, keep_cols, drop = FALSE]
  if (!cost_col %in% names(df)) return(NULL)
  df <- df[!is.na(df[[cost_col]]), , drop = FALSE]
  .report_drop_empty_cols(df)
}
