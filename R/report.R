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
#'   the upper-right corner of the page.  \code{NULL} skips the image.
#' @param file Character.  Destination file path.  Defaults to
#'   \code{report_<name>} in the current working directory, with the extension
#'   appropriate for \code{format}.
#' @param format Character.  Output format: \code{"html"} (default),
#'   \code{"pdf"}, or \code{"tex"} (standalone LaTeX source).  Multiple values
#'   are accepted; one file is produced per format.  \code{"pdf"}/\code{"tex"}
#'   require a LaTeX installation (e.g. \code{tinytex::install_tinytex()}) and
#'   are skipped with a warning when none is found.
#' @param open Logical.  Open the rendered report in the system browser/viewer
#'   when done.  Defaults to \code{interactive()} (opens in an interactive
#'   session, stays quiet in scripts and knits).
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
           format = c("html", "pdf", "tex"),
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
           format = c("html", "pdf", "tex"),
           levcost = NULL, cost_unit = NULL, open = interactive(), ...) {

    # default = html only (pdf/tex need LaTeX); explicit requests are honoured
    format <- if (missing(format)) "html" else
      match.arg(format, c("html", "pdf", "tex"), several.ok = TRUE)
    format <- .report_check_formats(format)

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
    # default: report_<NAME> in the working directory (findable, deterministic)
    if (is.null(file)) {
      file_base <- file.path(getwd(),
        paste0("report_", gsub("[^A-Za-z0-9_]", "_", object@name)))
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
          p <- ggplot2::autoplot(lc_obj, type = "npv", cost_unit = cost_unit)
          if (!is.null(p)) p + compact_theme else NULL
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

    .report_open(out_files, open)
    invisible(if (length(out_files) == 1L) out_files[1L] else out_files)
  }
)

# ── repository / model / scenario methods ────────────────────────────────────
# Report a technology from a container: reuse the technology datasheet, injecting
# the levelized cost computed for that container / solved scenario. Give the
# technology via `name = ` (in `...`).
.report_lc_params <- c("comm", "group", "autocomplete", "fuel_costs", "discount",
                       "base_year", "horizon", "calendar", "region", "weather",
                       "frontier", "solver", "verbose")

.report_container <- function(container, tech_source, object_for_levcost,
                              template, image_file, file, format, levcost,
                              cost_unit, name, dots, open = interactive()) {
  if (is.null(name) || !nzchar(name)) {
    message("report(): give the technology/process `name = ` to report.")
    return(invisible(NULL))
  }
  tech <- .levcost_find_tech(tech_source, name)
  if (is.null(tech)) {
    message("report(): technology '", name, "' not found in the ",
            class(container)[1], ".")
    return(invisible(NULL))
  }
  lc_dots     <- dots[intersect(names(dots), .report_lc_params)]
  render_dots <- dots[setdiff(names(dots), .report_lc_params)]
  if (is.null(lc_dots$verbose)) lc_dots$verbose <- FALSE
  if (is.null(levcost)) {
    levcost <- tryCatch(
      do.call(energyRt::levcost, c(list(object_for_levcost, name = name), lc_dots)),
      error = function(e) {
        warning("levcost() failed in report(): ", conditionMessage(e)); NULL
      })
    if (is.null(levcost)) {
      message("report(): could not compute levelized cost for '", name, "'.")
      return(invisible(NULL))
    }
  }
  do.call(report, c(list(tech, template = template, image_file = image_file,
    file = file, format = format, levcost = levcost, cost_unit = cost_unit,
    open = open), render_dots))
}

# ── whole-object reports ──────────────────────────────────────────────────────
# report(model) / report(repository) without `name`: a full model report --
# configuration + inventory + every process one-by-one.
# report(scenario) without `name`: a results overview built on getMix()/autoplot.

# Shared render loop for the whole-object templates.
.report_render <- function(tmpl_name, params, file, format, render_dots, stub,
                           open = interactive()) {
  format <- .report_check_formats(
    match.arg(format, c("html", "pdf", "tex"), several.ok = TRUE))
  tmpl <- if (!is.null(tmpl_name) && file.exists(tmpl_name)) {
    normalizePath(tmpl_name, mustWork = TRUE)
  } else {
    .find_report_template(tmpl_name)
  }
  # default: report_<name> in the working directory (findable, deterministic)
  file_base <- if (is.null(file)) {
    file.path(getwd(), paste0("report_", gsub("[^A-Za-z0-9_]", "_", stub)))
  } else {
    tools::file_path_sans_ext(file)
  }
  file_base <- normalizePath(file_base, mustWork = FALSE)
  out_files <- character(0)
  for (fmt in format) {
    ext  <- switch(fmt, pdf = ".pdf", html = ".html", tex = ".tex")
    fout <- paste0(file_base, ext)
    out_fmt <- switch(fmt,
      pdf  = rmarkdown::pdf_document(latex_engine = "pdflatex"),
      html = rmarkdown::html_document(self_contained = TRUE),
      tex  = rmarkdown::latex_document())
    do.call(rmarkdown::render, c(
      list(input = tmpl, output_format = out_fmt, output_file = fout,
           params = params, envir = new.env(parent = globalenv()),
           quiet = TRUE),
      render_dots))
    message("Report written to: ", fout)
    out_files <- c(out_files, fout)
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
  cm <- gs("commodity")                                   # storage
  if (is.character(cm) && length(cm) > 0) add("commodity", cm)
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
  ol <- gs("olife")
  if (is.data.frame(ol) && nrow(ol) > 0) add("olife", ol$olife[1])
  st <- gs("start")
  if (is.data.frame(st) && nrow(st) > 0) add("start", st$start[1])
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

.report_model <- function(object, template, file, format, dots,
                          open = interactive()) {
  is_model <- inherits(object, "model")
  nm   <- if (nzchar(object@name)) object@name else class(object)[1]
  desc <- tryCatch(object@desc, error = function(e) "")

  # -- configuration (model only) ------------------------------------------
  config_df <- NULL
  horizon <- NULL
  if (is_model) {
    cfg <- object@config
    horizon <- tryCatch(cfg@horizon, error = function(e) NULL)
    cal     <- tryCatch(cfg@calendar, error = function(e) NULL)
    pair <- function(k, v) data.frame(setting = k, value = v,
                                      stringsAsFactors = FALSE)
    cfg_rows <- list(pair("regions", paste(cfg@region, collapse = ", ")))
    if (!is.null(horizon) && nrow(horizon@intervals) > 0) {
      cfg_rows <- c(cfg_rows,
        list(pair("horizon", paste(range(horizon@period), collapse = " - ")),
             pair("milestone years",
                  paste(horizon@intervals$mid, collapse = ", "))))
    }
    if (!is.null(cal)) {
      ns <- tryCatch(nrow(cal@slice_share), error = function(e) NA)
      cfg_rows <- c(cfg_rows, list(pair("calendar",
        paste0(if (nzchar(cal@name)) cal@name else "(unnamed)",
               " (", ns, " slices)"))))
    }
    dsc <- tryCatch(cfg@discount, error = function(e) NULL)
    if (is.data.frame(dsc) && nrow(dsc) > 0) {
      for (col in c("sdr", "wacc", "discount")) {
        v <- suppressWarnings(as.numeric(dsc[[col]]))
        v <- v[is.finite(v)]
        if (length(v) > 0) {
          cfg_rows <- c(cfg_rows, list(pair("discount",
            paste(unique(signif(v, 4)), collapse = ", "))))
          break
        }
      }
    }
    cfg_rows <- c(cfg_rows, list(pair("optimizeRetirement",
      as.character(isTRUE(cfg@optimizeRetirement)))))
    config_df <- do.call(rbind, cfg_rows)
  }

  gobj <- function(cls) tryCatch(getObjects(object, cls),
                                 error = function(e) list())
  comms <- gobj("commodity"); sups <- gobj("supply"); dems <- gobj("demand")
  stgs  <- gobj("storage");   trds <- gobj("trade")
  cns   <- gobj("constraint"); techs <- gobj("technology")

  counts_df <- data.frame(
    class = c("commodity", "supply", "demand", "technology", "storage",
              "trade", "constraint"),
    count = c(length(comms), length(sups), length(dems), length(techs),
              length(stgs), length(trds), length(cns)),
    stringsAsFactors = FALSE)
  counts_df <- counts_df[counts_df$count > 0, , drop = FALSE]

  comm_df <- if (length(comms) > 0) do.call(rbind, lapply(comms, function(x) {
    em <- if (nrow(x@emis) > 0)
      paste(x@emis$comm, x@emis$emis, x@emis$unit, collapse = "; ") else ""
    data.frame(name = x@name, timeframe = x@timeframe[1], emissions = em,
               stringsAsFactors = FALSE)
  })) else NULL

  sup_df <- if (length(sups) > 0) do.call(rbind, lapply(sups, function(x) {
    cost <- if (nrow(x@availability) > 0 && "cost" %in% names(x@availability)) {
      v <- x@availability$cost[!is.na(x@availability$cost)]
      if (length(v) > 0) paste(unique(signif(range(v), 4)), collapse = " - ") else ""
    } else ""
    data.frame(name = x@name, commodity = paste(x@commodity, collapse = ", "),
               cost = cost, stringsAsFactors = FALSE)
  })) else NULL

  dem_df <- if (length(dems) > 0) do.call(rbind, lapply(dems, function(x) {
    tot <- if (nrow(x@dem) > 0 && "dem" %in% names(x@dem)) {
      v <- x@dem$dem[!is.na(x@dem$dem)]
      if (length(v) > 0) signif(sum(v), 4) else NA
    } else NA
    data.frame(name = x@name, commodity = paste(x@commodity, collapse = ", "),
               total = tot, stringsAsFactors = FALSE)
  })) else NULL

  trd_df <- if (length(trds) > 0) data.frame(name = names(trds),
    stringsAsFactors = FALSE) else NULL
  cns_df <- if (length(cns) > 0) data.frame(name = names(cns),
    desc = vapply(cns, function(x)
      tryCatch(x@desc, error = function(e) ""), character(1)),
    stringsAsFactors = FALSE) else NULL

  windows_plot <- if (requireNamespace("ggplot2", quietly = TRUE)) tryCatch(
    plot_process_windows(object, horizon = horizon), error = function(e) NULL)
    else NULL

  # -- per-process sections --------------------------------------------------
  tech_list <- lapply(c(techs, stgs), function(p) {
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
         draw_file = draw_file,
         info_df = .proc_info_df(p))
  })

  params <- list(
    title      = paste0("Model report: ", nm),
    model_name = nm, model_desc = desc,
    config_df  = config_df, counts_df = counts_df,
    comm_df = comm_df, sup_df = sup_df, dem_df = dem_df,
    trd_df = trd_df, cns_df = cns_df,
    windows_plot = windows_plot, techs = tech_list)

  tmpl <- if (is.null(template)) "model" else template
  .report_render(tmpl, params, file, format, dots, nm, open = open)
}

.report_scenario <- function(scen, template, file, format, dots,
                             open = interactive()) {
  nm <- if (nzchar(scen@name)) scen@name else "scenario"
  gd <- function(v) tryCatch(
    as.data.frame(getData(scen, name = v, merge = TRUE, drop.zeros = FALSE)),
    error = function(e) NULL)

  obj    <- gd("vObjective")
  status_df <- data.frame(
    item  = c("scenario", "solved (optimal)", "solver",
              "objective (total discounted cost)"),
    value = c(nm, as.character(isTRUE(scen@status$optimal)),
              tryCatch(paste(scen@settings@solver$lang), error = function(e) ""),
              if (!is.null(obj) && nrow(obj) > 0)
                format(signif(obj$value[1], 6), big.mark = ",") else "n/a"),
    stringsAsFactors = FALSE)

  ap <- function(...) if (requireNamespace("ggplot2", quietly = TRUE))
    tryCatch(ggplot2::autoplot(scen, ...), error = function(e) NULL) else NULL
  gen_plot    <- ap("generation")
  cap_plot    <- ap("capacity")
  newcap_plot <- ap("new_capacity")
  # dispatch profile over a sub-annual sample when the calendar has one
  gen_day_plot <- NULL
  sl <- tryCatch(unique(.mix_fetch(scen, "vTechOut", native = TRUE)$slice),
                 error = function(e) NULL)
  sl <- setdiff(sl, "ANNUAL")
  if (length(sl) > 1) {
    pref <- sub("_.*$", "", sl[1])
    gen_day_plot <- ap("generation", slice = paste0("^", pref, "_"))
  }

  emis <- gd("vEmsFuelTot")
  emis_df <- if (!is.null(emis) && nrow(emis) > 0) {
    ag <- stats::aggregate(value ~ comm + year, emis, sum)
    ag$value <- signif(ag$value, 5); ag
  } else NULL

  cost <- gd("vTotalCost")
  cost_df <- if (!is.null(cost) && nrow(cost) > 0) {
    ag <- stats::aggregate(value ~ year, cost, sum)
    names(ag)[2] <- "total cost"; ag[["total cost"]] <- signif(ag[["total cost"]], 6)
    ag
  } else NULL

  desc <- tryCatch(scen@desc, error = function(e) "")
  if (is.null(desc) || length(desc) == 0) desc <- ""
  params <- list(
    title = paste0("Scenario report: ", nm),
    scen_name = nm, scen_desc = desc,
    status_df = status_df,
    gen_plot = gen_plot, gen_day_plot = gen_day_plot,
    cap_plot = cap_plot, newcap_plot = newcap_plot,
    emis_df = emis_df, cost_df = cost_df)

  tmpl <- if (is.null(template)) "scenario" else template
  .report_render(tmpl, params, file, format, dots, nm, open = open)
}

#' @rdname report
#' @export
setMethod("report", "repository",
  function(object, template = NULL, image_file = NULL, file = NULL,
           format = c("html", "pdf", "tex"), levcost = NULL, cost_unit = NULL,
           open = interactive(), ...) {
    if (missing(format)) format <- "html"
    dots <- list(...); name <- dots[["name"]]; dots[["name"]] <- NULL
    if (is.null(name)) {
      return(.report_model(object, template, file, format, dots, open = open))
    }
    .report_container(object, object, object, template, image_file, file, format,
                      levcost, cost_unit, name, dots, open = open)
  })

#' @rdname report
#' @export
setMethod("report", "model",
  function(object, template = NULL, image_file = NULL, file = NULL,
           format = c("html", "pdf", "tex"), levcost = NULL, cost_unit = NULL,
           open = interactive(), ...) {
    if (missing(format)) format <- "html"
    dots <- list(...); name <- dots[["name"]]; dots[["name"]] <- NULL
    if (is.null(name)) {
      return(.report_model(object, template, file, format, dots, open = open))
    }
    .report_container(object, object, object, template, image_file, file, format,
                      levcost, cost_unit, name, dots, open = open)
  })

#' @rdname report
#' @export
setMethod("report", "scenario",
  function(object, template = NULL, image_file = NULL, file = NULL,
           format = c("html", "pdf", "tex"), levcost = NULL, cost_unit = NULL,
           open = interactive(), ...) {
    if (missing(format)) format <- "html"
    dots <- list(...); name <- dots[["name"]]; dots[["name"]] <- NULL
    if (is.null(name)) {
      return(.report_scenario(object, template, file, format, dots, open = open))
    }
    .report_container(object, object@model, object, template, image_file, file,
                      format, levcost, cost_unit, name, dots, open = open)
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
