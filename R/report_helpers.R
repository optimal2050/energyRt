# Shared helpers for report templates.
# Extracted from the inline helper block of inst/templates/report_generic.Rmd
# so that all shipped templates and user-authored templates use one exported
# implementation instead of copy-pasted chunks. Every emitter takes an
# explicit `output` argument (auto-detected from the knitr context by
# default), which also makes the helpers unit-testable outside a render.

#' Report template helpers
#'
#' Exported building blocks for `report()` templates. The shipped templates
#' in `inst/templates/` are built on these, and custom user templates should
#' call them (namespace-qualified, e.g. `energyRt::report_tbl(...)`) instead
#' of re-implementing formatting: they render the same content correctly for
#' the three supported outputs -- HTML, Word (`docx`), and LaTeX/PDF.
#'
#' Word must be told apart from LaTeX explicitly: `knitr::is_latex_output()`
#' is `FALSE` for docx, but so is `is_html_output()`, and raw TeX emitted in
#' an "else" branch is silently discarded by pandoc's docx writer. Emitters
#' therefore branch on the three-value `output` returned by [report_output()].
#'
#' @param output character, one of `"html"`, `"word"`, `"latex"`. Defaults to
#'   auto-detection via [report_output()]; pass explicitly in tests.
#' @param default character, the output assumed when no knitr/pandoc context
#'   is available (e.g. interactive use or tests).
#' @param x character or numeric value to escape / format.
#' @param digits integer, significant digits for numeric formatting.
#' @param path character, path to an image file.
#' @param paths character vector of image paths (missing files dropped).
#' @param frac numeric, width as a fraction of the text/line width.
#' @param frac_total numeric, total line-width fraction a row of images
#'   spans.
#' @param gap numeric, fraction of the line width between row images.
#' @param p a ggplot object.
#' @param w_in,h_in numeric, plot size in inches.
#' @param dpi integer, raster resolution.
#' @param title,note,caption character, section / table headings.
#' @param df data.frame to render as a table.
#' @param max_rows integer or `NULL`: cap the printed rows with a
#'   "... K more rows" footer. `NULL` prints all rows in HTML and caps
#'   PDF/Word at a 200-row safety limit.
#' @param scroll logical (HTML only): render tables over 15 rows inside a
#'   scrollable box with a sticky header.
#' @param name,desc character, object name and description for the page
#'   header.
#' @param image_file character or `NULL`, optional header image (logo).
#'
#' @return `report_output()` returns the output id invisibly-testable as a
#'   character scalar; `report_layout()` returns a named list of sizing
#'   constants; `report_plot_png()` returns the path to the rendered png;
#'   the `cat()`-based emitters (`report_img`, `report_sec`, `report_tbl`,
#'   `report_css`, `report_header`) return `invisible(NULL)` and are meant
#'   for `results='asis'` chunks.
#'
#' @examples
#' report_esc("a & b", output = "html")
#' report_fmt_val(1234.5678)
#' report_layout()$dpi
#'
#' @name report_helpers
NULL

#' @describeIn report_helpers Detect the current render target:
#'   `"html"`, `"word"`, or `"latex"`. Outside a knitr/pandoc context the
#'   `default` is returned.
#' @export
report_output <- function(default = c("html", "word", "latex")) {
  default <- match.arg(default)
  to <- knitr::pandoc_to()
  if (is.null(to)) return(default)
  if (identical(to, "docx")) return("word")
  if (knitr::is_html_output()) return("html")
  "latex"
}

#' @describeIn report_helpers Setup-chunk boilerplate: chunk options
#'   (`echo`/`warning`/`message` off) and the per-output `knitr.kable.NA`
#'   placeholder. Returns the detected output invisibly, so templates can
#'   open with `fmt <- energyRt::report_setup()`.
#' @export
report_setup <- function(default = c("html", "word", "latex")) {
  output <- report_output(default)
  knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
  options(knitr.kable.NA = if (output == "latex") "--" else "\u2014")
  invisible(output)
}

#' @describeIn report_helpers The one sizing system: named fractions of the
#'   text width shared by all outputs, plus the raster `dpi`.
#' @export
report_layout <- function() {
  list(
    hdr_img = 0.33,  # header image column
    side    = 0.45,  # schematic column in the two-column body
    main    = 0.52,  # table column in the two-column body
    lc_main = 0.55,  # levcost components plot
    lc_side = 0.43,  # frontier plot next to it
    panel   = 0.30,  # one share-mix panel
    dpi     = 150    # every rasterised ggplot
  )
}

# Safe HTML / TeX escaping (internal branches of report_esc)
.report_html_esc <- function(x) {
  if (is.null(x) || !nzchar(x)) return("")
  x <- gsub("&", "&amp;",  x, fixed = TRUE)
  x <- gsub("<", "&lt;",   x, fixed = TRUE)
  x <- gsub(">", "&gt;",   x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

.report_tex_esc <- function(x) {
  if (is.null(x) || !nzchar(x)) return("")
  x <- gsub("\\", "\a", x, fixed = TRUE)   # placeholder: braces come later
  x <- gsub("{",  "\\{",  x, fixed = TRUE)
  x <- gsub("}",  "\\}",  x, fixed = TRUE)
  x <- gsub("&",  "\\&",  x, fixed = TRUE)
  x <- gsub("%",  "\\%",  x, fixed = TRUE)
  x <- gsub("$",  "\\$",  x, fixed = TRUE)
  x <- gsub("#",  "\\#",  x, fixed = TRUE)
  x <- gsub("_",  "\\_",  x, fixed = TRUE)
  x <- gsub("~",  "\\textasciitilde{}", x, fixed = TRUE)
  x <- gsub("^",  "\\textasciicircum{}", x, fixed = TRUE)
  x <- gsub("\a", "\\textbackslash{}", x, fixed = TRUE)
  x
}

#' @describeIn report_helpers Escape a string for the current output (TeX
#'   escaping for `"latex"`, HTML escaping otherwise -- Word content goes
#'   through pandoc markdown, where HTML entities are safe).
#' @export
report_esc <- function(x, output = report_output()) {
  if (output == "latex") .report_tex_esc(x) else .report_html_esc(x)
}

#' @describeIn report_helpers Fixed-notation value formatting -- never
#'   scientific (no `"1.2e+03"`); `"--"` for missing.
#' @export
report_fmt_val <- function(x, digits = 4L) {
  if (is.null(x) || all(is.na(x))) return("--")
  formatC(as.numeric(x)[1], digits = digits, format = "fg")
}

#' @describeIn report_helpers Emit an image at a fraction of the line width;
#'   the ONLY way images enter a report page.
#' @export
report_img <- function(path, frac = 1, output = report_output()) {
  if (is.null(path) || !file.exists(path)) return(invisible(NULL))
  p <- gsub("\\\\", "/", path)
  if (output == "html") {
    cat("<img src='", p, "' style='width:", round(frac * 100),
        "%;height:auto' />\n", sep = "")
  } else if (output == "word") {
    cat("![](", p, "){width=", round(frac * 100), "%}\n\n", sep = "")
  } else {
    cat("\\includegraphics[width=", frac,
        "\\linewidth,keepaspectratio]{", p, "}\n", sep = "")
  }
  invisible(NULL)
}

#' @describeIn report_helpers Emit a ROW of images (logos, banners, badges)
#'   spanning `frac_total` of the line width, split evenly with `gap`
#'   between them. Missing files are dropped silently; a single image is
#'   capped at half of `frac_total` so a lone logo does not span the page.
#' @export
report_img_row <- function(paths, frac_total = 1, gap = 0.02,
                           output = report_output()) {
  paths <- as.character(paths %||% character(0))
  paths <- paths[nzchar(paths) & file.exists(paths)]
  n <- length(paths)
  if (n == 0) return(invisible(NULL))
  w <- (frac_total - gap * (n - 1)) / n
  if (n == 1) w <- min(w, frac_total / 2)
  w <- max(w, 0.01)
  pp <- gsub("\\\\", "/", paths)
  if (output == "html") {
    cat("<div style='display:flex;gap:", round(gap * 100),
        "%;align-items:center;width:", round(frac_total * 100),
        "%'>\n", sep = "")
    for (p in pp) {
      cat("<img src='", p, "' style='width:", round(100 / n),
          "%;height:auto;object-fit:contain' />\n", sep = "")
    }
    cat("</div>\n")
  } else if (output == "word") {
    # one markdown paragraph = one row
    cat(paste0("![](", pp, "){width=", round(w * 100), "%}",
               collapse = " "), "\n\n", sep = "")
  } else {
    for (i in seq_len(n)) {
      cat("\\includegraphics[width=", signif(w, 3),
          "\\linewidth,keepaspectratio]{", pp[i], "}", sep = "")
      if (i < n) cat("\\hfill\n")
    }
    cat("\\par\n")
  }
  invisible(NULL)
}

#' @describeIn report_helpers Emit a page break: an always-break `<div>` for
#'   HTML (honoured when printing), `\newpage` for LaTeX, and a raw
#'   `{=openxml}` block for Word. Call from a `results='asis'` chunk.
#' @export
report_pagebreak <- function(output = report_output()) {
  if (output == "html") {
    cat("\n<div style='page-break-after: always;'></div>\n")
  } else if (output == "word") {
    # pandoc raw-attribute fenced block; the fence must sit at column 0
    cat("\n```{=openxml}\n",
        "<w:p><w:r><w:br w:type=\"page\"/></w:r></w:p>\n",
        "```\n", sep = "")
  } else {
    cat("\n\\newpage\n")
  }
  invisible(NULL)
}

#' @describeIn report_helpers Rasterise a ggplot at `dpi` and return the png
#'   path (uniform pipeline for all three formats; avoids knitr fig options,
#'   which docx partly rejects).
#' @export
report_plot_png <- function(p, w_in, h_in, dpi = 150) {
  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp, width = round(w_in * dpi), height = round(h_in * dpi),
                 res = dpi, bg = "white")
  print(p)
  grDevices::dev.off()
  tmp
}

#' @describeIn report_helpers Section divider + bold title (+ gray note);
#'   Word gets a markdown heading.
#' @export
report_sec <- function(title, note = NULL, output = report_output()) {
  if (output == "html") {
    cat("\n<hr class='ert-rule-thin' />\n")
    cat("<p><strong>", .report_html_esc(title), "</strong>", sep = "")
    if (!is.null(note) && nzchar(note))
      cat(" <span class='ert-gray'><small>", .report_html_esc(note),
          "</small></span>", sep = "")
    cat("</p>\n")
  } else if (output == "word") {
    cat("\n## ", title, if (!is.null(note) && nzchar(note))
      paste0(" (", note, ")") else "", "\n\n", sep = "")
  } else {
    cat("\n\\vspace{8pt}\n")
    cat("\\textcolor{ert_blue}{\\rule{\\linewidth}{0.6pt}}\\par\\vspace{2pt}\n")
    cat("\\textbf{\\small ", .report_tex_esc(title), "}", sep = "")
    if (!is.null(note) && nzchar(note))
      cat(" \\textcolor{ert_gray}{\\small ", .report_tex_esc(note), "}",
          sep = "")
    cat("\\par\\vspace{2pt}\n")
  }
  invisible(NULL)
}

# PDF and Word cannot scroll: a table that forgets to pass max_rows is
# still hard-capped there, so a 1,000-row inventory can never take over a
# printed report. HTML shows all rows (scrolled) unless max_rows is given.
.report_tbl_hard_cap <- 200L

#' @describeIn report_helpers One table pipeline: prune empty columns, round
#'   numerics, `kable` per format. kable does the escaping (single-escaped,
#'   never double). Large tables stay readable: in HTML a table with more
#'   than 15 rows renders inside a scrollable box with a sticky header
#'   (`scroll = FALSE` disables); `max_rows` caps the printed rows with a
#'   "... K more rows (of N)" footer -- PDF/Word additionally enforce a
#'   200-row safety cap when `max_rows` is not given.
#' @export
report_tbl <- function(df, caption = NULL, digits = 4L,
                       output = report_output(),
                       max_rows = NULL, scroll = TRUE) {
  if (is.null(df) || nrow(df) == 0) return(invisible(NULL))
  keep <- vapply(df, function(x) {
    if (is.character(x)) any(!is.na(x) & nzchar(x)) else any(!is.na(x))
  }, logical(1))
  df <- df[, keep, drop = FALSE]
  if (ncol(df) == 0) return(invisible(NULL))
  for (i in seq_along(df)) {
    if (is.numeric(df[[i]])) df[[i]] <- signif(df[[i]], digits)
  }
  n_all <- nrow(df)
  cap_at <- if (output == "html") max_rows else
    max_rows %||% .report_tbl_hard_cap
  if (!is.null(cap_at) && is.finite(cap_at) && n_all > cap_at) {
    df <- utils::head(df, cap_at)
  }
  n_more <- n_all - nrow(df)
  if (!is.null(caption) && nzchar(caption)) {
    if (output == "html") {
      cat("<p><strong>", .report_html_esc(caption), "</strong></p>\n",
          sep = "")
    } else if (output == "word") {
      cat("**", caption, "**\n\n", sep = "")
    } else {
      cat("{\\small\\bfseries ", .report_tex_esc(caption),
          "}\\par\\vspace{1pt}\n", sep = "")
    }
  }
  wrap <- output == "html" && isTRUE(scroll) && nrow(df) > 15
  if (wrap) cat("<div class='ert-scroll'>\n")
  k <- if (output == "html") {
    knitr::kable(df, format = "html", row.names = FALSE,
                 table.attr = "class='kable-table'")
  } else if (output == "word") {
    knitr::kable(df, format = "pipe", row.names = FALSE)
  } else {
    knitr::kable(df, format = "latex", booktabs = TRUE, linesep = "",
                 row.names = FALSE)
  }
  cat(k, sep = "\n")
  cat("\n")
  if (wrap) cat("</div>\n")
  if (n_more > 0) {
    note <- paste0("... ", n_more, " more rows (of ", n_all, ")")
    if (output == "html") {
      cat("<p class='ert-gray'><small>", .report_html_esc(note),
          "</small></p>\n", sep = "")
    } else if (output == "word") {
      cat("*", note, "*\n\n", sep = "")
    } else {
      cat("{\\small\\color{ert_gray} ", .report_tex_esc(note), "}\\par\n",
          sep = "")
    }
  }
  invisible(NULL)
}

#' @describeIn report_helpers Emit the shared report stylesheet (HTML only;
#'   a no-op for Word and LaTeX).
#' @export
report_css <- function(output = report_output()) {
  if (output != "html") return(invisible(NULL))
  cat("<style>
body       { font-family: Arial, sans-serif; font-size: 13px; margin: 20px; }
h1, h2     { color: #1f497d; }
.ert-gray  { color: #808080; }
.ert-rule      { border: none; border-top: 2px solid #1f497d; margin: 8px 0 4px; }
.ert-rule-thin { border: none; border-top: 1px solid #1f497d; margin: 6px 0 4px; }
.ert-two-col   { display: flex; gap: 20px; align-items: flex-start; }
.ert-col-main  { flex: 0 0 52%; min-width: 0; }
.ert-col-side  { flex: 0 0 45%; min-width: 0; }
.ert-kp-table  { border-collapse: collapse; font-size: 12px; }
.ert-kp-table td { padding: 2px 12px 2px 0; vertical-align: top; }
.ert-kp-table td:first-child { font-family: monospace; }
table.kable-table { border-collapse: collapse; font-size: 11px; margin: 4px 0 8px; }
table.kable-table th { border-top: 1.5px solid #333; border-bottom: 1px solid #888;
                       padding: 3px 12px 3px 4px; text-align: left; }
table.kable-table td { border: none; padding: 2px 12px 2px 4px; }
table.kable-table tbody tr:last-child td { border-bottom: 1.5px solid #333; }
table.kable-table tbody tr:nth-child(odd) { background: #f7f7f7; }
.ert-scroll { max-height: 320px; overflow-y: auto;
              border-bottom: 1.5px solid #333; margin: 4px 0 8px; }
.ert-scroll table.kable-table { margin: 0; }
.ert-scroll table.kable-table thead th { position: sticky; top: 0;
              background: #fff; z-index: 1; }
.ert-scroll table.kable-table tbody tr:last-child td { border-bottom: none; }
</style>\n")
  invisible(NULL)
}

#' @describeIn report_helpers Page header: name + description on the left,
#'   optional image (logo) on the right.
#' @export
report_header <- function(name, desc = "", image_file = NULL,
                          frac = report_layout()$hdr_img,
                          output = report_output()) {
  if (is.null(desc) || is.na(desc)) desc <- ""
  has_img <- !is.null(image_file) && nzchar(image_file) &&
    file.exists(image_file)
  if (output == "html") {
    cat("<div class='ert-two-col'>\n<div style='flex:1;min-width:0'>\n")
    cat("<h1 style='margin:0 0 4px'>", .report_html_esc(name), "</h1>\n",
        sep = "")
    if (nzchar(desc))
      cat("<p class='ert-gray' style='margin:2px 0 8px 0'>",
          .report_html_esc(desc), "</p>\n", sep = "")
    cat("</div>\n")
    if (has_img) {
      cat("<div style='flex:0 0 ", round(frac * 100), "%;min-width:0'>\n",
          sep = "")
      report_img(image_file, 1, output = output)
      cat("</div>\n")
    }
    cat("</div>\n<hr class='ert-rule' />\n")
  } else if (output == "word") {
    cat("# ", name, "\n\n", sep = "")
    if (nzchar(desc)) cat("*", desc, "*\n\n", sep = "")
    if (has_img) report_img(image_file, frac + 0.1, output = output)
  } else {
    txt_w <- if (has_img) 1 - frac - 0.03 else 1
    cat("\\begin{minipage}[t]{", txt_w, "\\textwidth}\n", sep = "")
    cat("{\\Large\\bfseries\\color{ert_blue} ", .report_tex_esc(name),
        "}\\par\n", sep = "")
    if (nzchar(desc))
      cat("{\\small\\color{ert_gray} ", .report_tex_esc(desc), "}\\par\n",
          sep = "")
    cat("\\end{minipage}")
    if (has_img) {
      cat("\\hfill\n\\begin{minipage}[t]{", frac, "\\textwidth}\n", sep = "")
      cat("\\vspace{0pt}\\raggedleft\n")
      report_img(image_file, 1, output = output)
      cat("\\end{minipage}\n")
    }
    cat("\n\\vspace{4pt}\n")
    cat("\\textcolor{ert_blue}{\\rule{\\textwidth}{1.5pt}}\\par\\vspace{4pt}\n")
  }
  invisible(NULL)
}
