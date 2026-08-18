# =============================================================================
# energyRt process designer (M7)
# =============================================================================
# Vertical wizard (one tab per slot) over a fixed split: left = data
# editing, right = always-visible info panel (draw / autoplot / YAML /
# R code / levcost / report) + validation. Each data-slot tab shows the
# filled-data table (only populated columns by default, with a units
# header row from get_units()) and a row constructor with index dropdowns;
# "Edit row" loads a selected row into the constructor (the add button
# then reads "Save"). Undo/Redo at the top revert structural edits
# (ports, rows, loads). The report tab renders an Rmd template
# (inst/designer/reports/) to html/word/pdf via rmarkdown.
# Launched via energyRt::process_designer(); requires shiny + DT
# (Suggests).
# =============================================================================

library(shiny)

.ex_dir <- system.file("techspec", "examples", package = "energyRt")
if (.ex_dir == "" && file.exists("../techspec/examples")) {
  .ex_dir <- normalizePath("../techspec/examples")
}
.templates <- if (nzchar(.ex_dir)) {
  stats::setNames(file.path(.ex_dir, list.files(.ex_dir, pattern = "[.](yml|yaml|json)$")),
                  sub("[.](yml|yaml|json)$", "",
                      list.files(.ex_dir, pattern = "[.](yml|yaml|json)$")))
} else character()

# User spec directory (process_designer(dir = ...)): its .yml files join
# the gallery, labeled with the directory name
.user_dir <- getOption("energyRt.designer.dir")
if (!is.null(.user_dir) && dir.exists(.user_dir)) {
  fls <- list.files(.user_dir, pattern = "[.](yml|yaml|json)$", full.names = TRUE)
  if (length(fls) > 0) {
    .templates <- c(.templates, stats::setNames(
      fls, paste0(basename(.user_dir), "/", sub("[.](yml|yaml|json)$", "",
                                                basename(fls)))))
  }
}

# Report templates: energyRt::report()'s built-ins that apply to a single
# technology (inst/templates/report_<name>.Rmd), plus uploaded .Rmd files
# (which must follow the same params interface as report_generic.Rmd).
# Values are report()'s template names for built-ins, file paths for
# uploads.
.rep_templates <- c("generic (datasheet)" = "generic",
                    "summary (one-pager)" = "summary",
                    "vehicle"             = "vehicle")

.STD_COMMS <- c("COA", "GAS", "OIL", "BIO", "ELC", "ELCPJ", "HEAT", "H2",
                "CO2", "PM", "NOX")

# ── native file picker + remembered directories ──────────────────────────────
# The Browse... button must return the file's ORIGINAL path (a browser
# upload only yields a temp copy), so it needs a native dialog. On Windows
# the .NET OpenFileDialog is the SAME modern common dialog the browser's
# own Upload button shows (utils::choose.files() opens the legacy Win32
# template — the outdated, low-res one). Last-used directories persist in
# the user config dir; with no memory the dialog starts in getwd().

.designer_paths_file <- function() {
  file.path(tools::R_user_dir("energyRt", which = "config"),
            "designer_paths.rds")
}

.recall_dir <- function(key) {
  f <- .designer_paths_file()
  if (file.exists(f)) {
    p <- tryCatch(readRDS(f)[[key]], error = function(e) NULL)
    if (!is.null(p) && dir.exists(p)) return(p)
  }
  getwd()
}

.remember_dir <- function(key, path) {
  f <- .designer_paths_file()
  dir.create(dirname(f), showWarnings = FALSE, recursive = TRUE)
  d <- if (file.exists(f)) {
    tryCatch(readRDS(f), error = function(e) list())
  } else list()
  d[[key]] <- path
  tryCatch(saveRDS(d, f), error = function(e) invisible(NULL))
  invisible(NULL)
}

.pick_file_native <- function(title = "Choose a file",
                              filter = "All files (*.*)|*.*",
                              initial_dir = getwd()) {
  if (identical(.Platform$OS.type, "windows")) {
    ps <- c(
      "Add-Type -AssemblyName System.Windows.Forms | Out-Null",
      "$d = New-Object System.Windows.Forms.OpenFileDialog",
      sprintf("$d.Title = '%s'", gsub("'", "''", title)),
      sprintf("$d.Filter = '%s'", gsub("'", "''", filter)),
      sprintf("$d.InitialDirectory = '%s'",
              gsub("'", "''", gsub("/", "\\\\", initial_dir))),
      "$d.RestoreDirectory = $true",
      "if ($d.ShowDialog() -eq [System.Windows.Forms.DialogResult]::OK) {",
      "  Write-Output $d.FileName",
      "}")
    psf <- tempfile(fileext = ".ps1")
    writeLines(ps, psf)
    on.exit(unlink(psf), add = TRUE)
    out <- tryCatch(
      suppressWarnings(system2(
        "powershell",
        c("-NoProfile", "-Sta", "-ExecutionPolicy", "Bypass",
          "-File", shQuote(psf)),
        stdout = TRUE, stderr = FALSE)),
      error = function(e) character())
    out <- out[nzchar(out)]
    if (length(out) >= 1) {
      return(normalizePath(out[[1]], winslash = "/", mustWork = FALSE))
    }
    return("")
  }
  # non-Windows fallback: base picker
  tryCatch(normalizePath(file.choose(), winslash = "/"),
           error = function(e) "")
}

# Colour styles, switched from the top-right corner. "light" is plain
# Bootstrap; "dim" and "dark" are CSS overlays injected reactively.
.THEMES <- list(
  light = "",
  dim = "
    body { background: #dbe4ed; color: #1f2a35; }
    .well { background: #ccd9e5; border-color: #b3c4d4; }
    .form-control, .selectize-input { background: #edf3f8; }
    pre { background: #d2dde8; color: #1f2a35; border-color: #b3c4d4; }
    .nav-pills > li > a { color: #32475c; }
    table.dataTable, .dataTables_wrapper { background: transparent; }
    .btn-default { background: #c2d1de; border-color: #a4b8c9; }
    .tab-content > .tab-pane, .modal-content { background: transparent; }
  ",
  dark = "
    body { background: #1f2125; color: #d4d4d4; }
    h1, h2, h3, h4, h5, h6, label, .control-label { color: #d4d4d4; }
    .well { background: #292c31; border-color: #3a3d44; }
    .form-control, .selectize-input, .selectize-dropdown,
    .selectize-dropdown-content .option, textarea {
      background: #2e3138 !important; color: #ddd !important;
      border-color: #45484f; }
    .selectize-input > .item { color: #ddd; }
    pre { background: #26282d; color: #c9c9c9; border-color: #3a3d44; }
    .nav-pills > li > a { color: #a9adb3; }
    .nav-pills > li.active > a,
    .nav-pills > li.active > a:hover { background: #3d6fa8; color: #fff; }
    .nav-tabs > li > a { color: #a9adb3; }
    .nav-tabs > li.active > a { background: #292c31; color: #ddd;
      border-color: #3a3d44; }
    .nav-tabs { border-color: #3a3d44; }
    table.dataTable, table.dataTable th, table.dataTable td {
      background: #24262b !important; color: #d4d4d4 !important; }
    .dataTables_wrapper, .dataTables_info { color: #a9adb3 !important; }
    .btn-default { background: #3a3d44; color: #ddd;
      border-color: #55585f; }
    .btn-default:hover { background: #45484f; color: #fff; }
    a, details.param-help summary { color: #7fb2e5; }
    .text-muted, .navbar-brand, details.param-help { color: #8a8e94
      !important; }
    hr { border-color: #3a3d44; }
    code { background: #2e3138; color: #d0a0ff; }
    .modal-content { background: #292c31; color: #ddd; }
    .progress { background: #2e3138; }
    .shiny-notification { background: #2e3138; color: #ddd; }
    .irs-line, .irs-bar { background: #45484f; border-color: #45484f; }
  "
)

# Port tabs (tab id -> spec section)
.PORT_SECS <- c("input" = "inputs", "output" = "outputs", "aux" = "aux")

# Data-slot tabs, labeled by the technology SLOT name (tab -> spec section)
.SLOT_TABS <- c("ceff"     = "efficiency",
                "geff"     = "group_efficiency",
                "aeff"     = "aux_efficiency",
                "af"       = "availability",
                "afs"      = "afs",
                "weather"  = "weather",
                "invcost"  = "invcost",
                "fixom"    = "fixom",
                "varom"    = "varom",
                "capacity" = "capacity")

.WIZ <- c("Template", "Metadata", "input", "output", "aux", "group",
          "units", "cluster", "vintage", names(.SLOT_TABS))

# Display titles for the wizard tabs (tab VALUES stay the short slot ids)
.TAB_LABELS <- c(
  input    = "Input (@input)",
  output   = "Output (@output)",
  aux      = "Aux (@aux)",
  group    = "Group (@group)",
  units    = "Units (@units)",
  cluster  = "Cluster (@cluster)",
  vintage  = "Vintage (@vintage)",
  ceff     = "Comm. eff. (@ceff)",
  geff     = "Group eff. (@geff)",
  aeff     = "Aux. eff. (@aeff)",
  af       = "Availability (@af)",
  afs      = "Avail. sum (@afs)",
  weather  = "Weather (@weather)",
  invcost  = "Inv. cost (@invcost)",
  fixom    = "Fixed O&M (@fixom)",
  varom    = "Var. O&M (@varom)",
  capacity = "Capacity (@capacity)")
.tab_label <- function(id) {
  if (id %in% names(.TAB_LABELS)) .TAB_LABELS[[id]] else id
}

# First sentence of a description (cut at the first ". " boundary)
.first_sentence <- function(x) {
  m <- regexpr("[.](\\s|$)", x)
  if (m > 0) substr(x, 1L, m) else x
}

.slot_full_desc <- function(slot_nm) {
  d <- tryCatch(energyRt:::.get_param_desc("technology", slot_nm),
                error = function(e) NA_character_)
  if (length(d) != 1 || is.na(d) || !nzchar(d)) return(NULL)
  d
}

# Slot description from the class registry: tabs show ONE sentence; the
# full text lives inside the collapsible parameters fold (.param_help)
.slot_desc <- function(slot_nm) {
  d <- .slot_full_desc(slot_nm)
  if (is.null(d)) return(NULL)
  p(class = "text-muted", style = "margin-bottom: 4px;",
    .first_sentence(d))
}

# Everything the constructor offers is derived from the slot's own columns:
# dims get dropdowns, comm/acomm/group are parameter-dependent extra
# indices, the rest are the slot's value parameters.
.sec_meta <- function(section) {
  cols <- energyRt:::.TECHSPEC_SECTIONS[[section]]$cols
  dims <- intersect(c("vintage", "cluster", "region", "year", "timeslice",
                      "weather"),
                    cols)
  extras <- intersect(c("comm", "acomm", "group"), cols)
  list(cols = cols, dims = dims, extras = extras,
       params = setdiff(cols, c(dims, extras)))
}

.param_extra <- function(section, param) {
  want <- if (section == "efficiency") {
    "comm"
  } else if (section == "group_efficiency") {
    "group"
  } else if (section == "aux_efficiency") {
    if (grepl("^c(inp|out)2", param)) c("acomm", "comm") else "acomm"
  } else if (section == "varom") {
    switch(param, cvarom = "comm", avarom = "acomm", NULL)
  } else if (section == "weather") {
    if (grepl("^wafc", param)) "comm" else NULL
  } else NULL
  intersect(want, .sec_meta(section)$extras)
}

.blank_spec <- function() {
  list(techspec = 1L, class = "technology", name = "NEW_TECH", desc = "",
       units = list(capacity = "GW", activity = "PJ", costs = "MUSD"),
       inputs = list(), outputs = list(), efficiency = list(),
       vintages = list())
}

.rows_to_display <- function(rows, cols) {
  if (length(rows) == 0) {
    return(as.data.frame(stats::setNames(rep(list(character()), length(cols)),
                                         cols), optional = TRUE))
  }
  out <- lapply(cols, function(cc) {
    vapply(rows, function(r) {
      v <- r[[cc]]
      if (is.null(v)) "" else as.character(v)
    }, character(1))
  })
  names(out) <- cols
  as.data.frame(out, stringsAsFactors = FALSE, optional = TRUE)
}

# Columns to display: only columns that carry data, unless "show all
# columns" is checked; an empty table shows the full header set.
.shown_cols <- function(rows, cols, show_all = FALSE) {
  if (show_all || length(rows) == 0) return(cols)
  df <- .rows_to_display(rows, cols)
  filled <- vapply(df, function(x) any(nzchar(x)), logical(1))
  if (!any(filled)) cols else cols[filled]
}

# Collapsible per-slot column reference (from the class registry docs);
# x.lo/x.up/x.fx families share one line. Data slots list their value
# parameters; structural tabs (vintage/cluster/group) pass `cols` to
# describe every column.
.param_help <- function(section, cols = NULL) {
  slot_nm <- energyRt:::.TECHSPEC_SECTIONS[[section]]$arg
  pars <- cols %||% .sec_meta(section)$params
  if (length(pars) == 0) return(NULL)
  fam <- sub("[.](lo|up|fx)$", "", pars)
  items <- lapply(unique(fam), function(f) {
    grp <- pars[fam == f]
    dsc <- vapply(grp, function(p) {
      d <- energyRt:::.get_param_desc("technology", slot_nm, p)
      if (length(d) != 1 || is.na(d)) "" else trimws(d)
    }, character(1))
    dsc <- unique(dsc[nzchar(dsc)])
    tags$li(tags$b(paste(grp, collapse = ", ")), ": ",
            paste(dsc, collapse = " "))
  })
  # the tab shows only the first sentence of the slot description; the
  # full paragraph opens the fold when there is more to say
  full <- .slot_full_desc(slot_nm)
  intro <- if (!is.null(full) &&
               nchar(full) > nchar(.first_sentence(full)) + 1L) {
    tags$p(full)
  }
  tags$details(class = "param-help",
               tags$summary("parameters"),
               intro,
               tags$ul(items))
}

# Two-row DT header: column names + their units (from get_units() on the
# built technology, so ceff/varom units resolve through the declared port
# units). NULL when no shown column has a known unit.
.units_container <- function(shown, slot_nm, udf) {
  if (is.null(udf) || nrow(udf) == 0) return(NULL)
  u <- vapply(shown, function(cc) {
    vals <- unique(udf$unit[udf$slot == slot_nm & udf$parameter == cc])
    vals <- vals[!is.na(vals) & nzchar(vals)]
    paste(vals, collapse = " | ")
  }, character(1))
  if (!any(nzchar(u))) return(NULL)
  htmltools::tags$table(
    class = "display",
    htmltools::tags$thead(
      htmltools::tags$tr(lapply(shown, htmltools::tags$th)),
      htmltools::tags$tr(lapply(u, function(x) {
        htmltools::tags$th(
          x, style = paste0("font-weight: normal; color: #777;",
                            "font-size: 11px; padding: 2px 10px;"))
      }))
    )
  )
}

.display_to_rows <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(list())
  lapply(seq_len(nrow(df)), function(i) {
    r <- as.list(df[i, , drop = FALSE])
    r <- lapply(r, function(v) {
      v <- trimws(as.character(v))
      if (!nzchar(v)) return(NULL)
      if (identical(v, "ALL")) return("ALL")
      if (grepl("^-?[0-9.]+$", v)) return(as.numeric(v))
      v
    })
    r[!vapply(r, is.null, logical(1))]
  })
}

.struct_editor_ui <- function(b) {
  tagList(
    .slot_desc(energyRt:::.TECHSPEC_SECTIONS[[b]]$arg),
    .param_help(b, cols = energyRt:::.TECHSPEC_SECTIONS[[b]]$cols),
    DT::DTOutput(paste0("tbl_", b)),
    div(class = "btn-row",
        actionButton(paste0("add_", b), "+ row"),
        actionButton(paste0("del_", b), "delete selected"))
  )
}

.slot_editor_ui <- function(b) {
  tagList(
    .slot_desc(energyRt:::.TECHSPEC_SECTIONS[[b]]$arg),
    .param_help(b),
    checkboxInput(paste0("showall_", b), "show all columns", FALSE),
    DT::DTOutput(paste0("tbl_", b)),
    div(class = "btn-row",
        actionButton(paste0("edit_", b), "Edit row"),
        actionButton(paste0("del_", b), "delete selected")),
    tags$hr(),
    uiOutput(paste0("ctor_title_", b)),
    uiOutput(paste0("ctor_", b))
  )
}

.port_ui <- function(kind) {
  id <- tolower(kind)
  sec <- .PORT_SECS[[id]]
  tagList(
    .slot_desc(energyRt:::.TECHSPEC_SECTIONS[[sec]]$arg),
    fluidRow(
      column(5, selectizeInput(paste0("comm_", id), NULL,
                               choices = .STD_COMMS,
                               options = list(create = TRUE,
                                              placeholder = "commodity"))),
      column(3, textInput(paste0("unit_", id), NULL, "PJ",
                          placeholder = "unit")),
      column(4, actionButton(paste0("addport_", id), paste("+", kind)))
    ),
    checkboxInput(paste0("showall_", sec), "show all columns", FALSE),
    DT::DTOutput(paste0("tbl_", sec)),
    div(class = "btn-row",
        actionButton(paste0("del_", sec), "delete selected"))
  )
}

ui <- fluidPage(
  tags$style(HTML("
    .nav-pills > li > a { padding: 3px 10px; font-size: 13px; }
    .nav-pills { margin-bottom: 0; }
    .well { padding: 10px; margin-bottom: 10px; }
    .btn-row { margin-top: 6px; }
    .btn-row .btn { margin-right: 6px; }
    .form-group { margin-bottom: 8px; }
    .nav-pills > li.navbar-brand { float: none; height: auto;
      font-size: 11px; text-transform: uppercase; letter-spacing: 0.5px;
      color: #999; padding: 8px 10px 0; margin: 0; }
    details.param-help { margin: 2px 0 8px; font-size: 12px; color: #555; }
    details.param-help summary { cursor: pointer; color: #337ab7; }
    details.param-help ul { margin: 4px 0 0; padding-left: 18px; }
  ")),
  uiOutput("theme_css"),
  fluidRow(
    column(6, titlePanel("energyRt process designer")),
    column(6, div(style = paste0("margin-top: 18px; display: flex;",
                                 "justify-content: flex-end;"),
                  div(style = "width: 84px;",
                      selectInput("app_theme", NULL,
                                  choices = c("light", "dim", "dark"),
                                  selected = "light"))))
  ),
  fluidRow(
    # ── left pane: wizard ─────────────────────────────────────────────────
    column(
      6,
      div(style = paste0("display: flex; align-items: center; gap: 6px;",
                         "margin-bottom: 6px; flex-wrap: wrap;"),
          actionButton("back", "◀ Back"),
          actionButton("nxt", "Next ▶", class = "btn-primary"),
          span(style = "margin: 0 10px; color: #777; font-size: 13px;",
               textOutput("wiz_where", inline = TRUE)),
          div(style = "margin-left: auto;",
              actionButton("undo", "↶ Undo"),
              actionButton("redo", "↷ Redo"))),
      # the save actions live in the wizard sidebar, right below the last
      # tab (@capacity) -- appended into the navlist's well via tagQuery
      htmltools::tagQuery(do.call(navlistPanel, c(
        list(id = "wizard", well = TRUE, widths = c(3, 9)),
        list(
          tabPanel("Template",
            wellPanel(
              h5("Process class"),
              selectInput("proc_class", NULL,
                          choices = c("technology", "storage", "supply",
                                      "demand", "trade", "import",
                                      "export"),
                          selected = "technology")
            ),
            wellPanel(
              h5("Start from"),
              selectInput("template", NULL,
                          choices = c("(blank)" = "", .templates)),
              actionButton("load_template", "Load", class = "btn-primary")
            ),
            wellPanel(
              h5("Open a spec"),
              fileInput("upload", NULL, buttonLabel = "Upload spec",
                        accept = c(".yml", ".yaml", ".json"))
            ),
            wellPanel(
              h5("Open an R object"),
              fluidRow(
                column(8, uiOutput("envobj_ui")),
                column(4, actionButton("load_envobj", "Load",
                                       class = "btn-primary"))
              ),
              fileInput("rdata_upload", NULL,
                        buttonLabel = "Upload .RData / .rds",
                        accept = c(".RData", ".rda", ".rds"))
            ),
            wellPanel(
              h5("Image (for reports)"),
              fluidRow(
                column(3, actionButton("image_browse", "Browse...",
                                       style = "width: 100%;")),
                column(9, textInput("image_path", NULL, "",
                                    placeholder = "path or URL"))
              ),
              uiOutput("image_current")
            )),
          tabPanel("Metadata",
            wellPanel(
              h5("Identity"),
              textInput("name", "Name", "NEW_TECH"),
              textInput("desc", "Description", ""),
              uiOutput("timeframe_ui"),
              textInput("region", "Region(s) (comma-sep, @region)", ""),
              fluidRow(
                column(6, checkboxInput("fullYear",
                                        "full year (@fullYear)", TRUE)),
                column(6, checkboxInput("optretire",
                                        "optimize retirement (@optimizeRetirement)",
                                        FALSE))
              ),
              textAreaInput("notes", "Notes (@misc$notes)", "", rows = 2)
            ),
            wellPanel(
              h5("Design context"),
              selectInput("ctx_calendar", "Calendar",
                          choices = c("(none)" = "",
                                      names(energyRt::calendars))),
              fluidRow(
                column(6, numericInput("ctx_from", "Horizon from", 2020,
                                       step = 5)),
                column(6, numericInput("ctx_to", "Horizon to", 2050,
                                       step = 5))
              ),
              textInput("ctx_regions", "Regions (comma-sep)", "")
            )),
          tabPanel(.tab_label("input"), value = "input",
                   .port_ui("Input")),
          tabPanel(.tab_label("output"), value = "output",
                   .port_ui("Output")),
          tabPanel(.tab_label("aux"), value = "aux",
                   p(class = "text-muted",
                     "Auxiliary commodities (emissions, byproducts)."),
                   .port_ui("Aux")),
          tabPanel(.tab_label("group"), value = "group",
                   .struct_editor_ui("groups")),
          tabPanel(.tab_label("units"), value = "units",
            wellPanel(
              h5("Units"),
              textInput("unit_cap", "Capacity", "GW"),
              textInput("unit_act", "Activity", "PJ"),
              textInput("unit_use", "Use (empty = activity unit)", ""),
              textInput("unit_cost", "Costs", "MUSD")
            ),
            wellPanel(
              h5("Capacity to activity"),
              fluidRow(
                column(8, numericInput("cap2act",
                                       "cap2act (activity per unit capacity/yr)",
                                       NA)),
                column(4, div(style = "margin-top: 32px; color: #777;",
                              textOutput("cap2act_unit")))
              ))),
          "Variants",
          tabPanel(.tab_label("cluster"), value = "cluster",
                   .struct_editor_ui("clusters")),
          tabPanel(.tab_label("vintage"), value = "vintage",
                   .struct_editor_ui("vintages")),
          "Performance"
        ),
        lapply(c("ceff", "geff", "aeff", "af", "afs", "weather"),
               function(lbl) {
          tabPanel(.tab_label(lbl), value = lbl,
                   .slot_editor_ui(.SLOT_TABS[[lbl]]))
        }),
        list("Costs"),
        lapply(c("invcost", "fixom", "varom"), function(lbl) {
          tabPanel(.tab_label(lbl), value = lbl,
                   .slot_editor_ui(.SLOT_TABS[[lbl]]))
        }),
        list("Capacity"),
        list(tabPanel(.tab_label("capacity"), value = "capacity",
                      .slot_editor_ui(.SLOT_TABS[["capacity"]])))
      )))$find(".col-sm-3.well")$append(
        tags$hr(style = "margin: 10px 0;"),
        downloadButton("save", "Save YAML",
                       style = "width: 100%; margin-bottom: 4px;"),
        downloadButton("save_json", "Save JSON",
                       style = "width: 100%; margin-bottom: 4px;"),
        downloadButton("save_rdata", "Save .RData",
                       style = "width: 100%; margin-bottom: 4px;"),
        actionButton("to_env", "Save to R session",
                     style = "width: 100%;")
      )$allTags()
    ),
    # ── right pane: info panel, always visible ────────────────────────────
    column(
      6,
      tabsetPanel(
        id = "info",
        tabPanel("draw",
                 plotOutput("schematic", height = 480),
                 uiOutput("draw_dims_ui")),
        tabPanel("autoplot",
                 selectInput("ap_block", NULL,
                             choices = c("invcost", "fixom", "capacity",
                                         "availability", "afs"),
                             width = 220),
                 plotOutput("slotplot", height = 330)),
        tabPanel("YAML", verbatimTextOutput("yaml_out")),
        tabPanel("R code", verbatimTextOutput("code_out")),
        tabPanel("levcost",
                 fluidRow(
                   column(3, numericInput("lc_discount", "Discount", 0.05,
                                          step = 0.01)),
                   column(6, uiOutput("lc_fuel_ui")),
                   column(3, br(),
                          actionButton("lc_go", "Compute",
                                       class = "btn-primary"))
                 ),
                 fluidRow(
                   column(5, selectInput("lc_type", NULL,
                     choices = c("NPV by variant"       = "npv",
                                 "components (by year)" = "components",
                                 "totals"               = "totals",
                                 "output frontier"      = "frontier",
                                 "input frontier"       = "input_frontier"))),
                   column(7, uiOutput("lc_note"))
                 ),
                 DT::DTOutput("lc_tbl"),
                 plotOutput("lc_plot", height = 300)),
        tabPanel("report",
                 fluidRow(
                   column(4, selectInput("rep_template", "Template",
                                         choices = .rep_templates)),
                   column(3, selectInput("rep_format", "Format",
                                         choices = c("html", "pdf",
                                                     "docx"))),
                   column(3, br(),
                          actionButton("rep_go", "Render",
                                       class = "btn-primary")),
                   column(2, br(), uiOutput("rep_dl_ui"))
                 ),
                 fileInput("rep_upload", NULL,
                           buttonLabel = "Upload template (.Rmd)",
                           accept = c(".Rmd", ".rmd"), width = 320),
                 p(class = "text-muted", style = "margin-top: -8px;",
                   "custom templates use the report_generic.Rmd params",
                   "interface (see inst/templates/)"),
                 checkboxInput("rep_levcost",
                               "include levelized cost (needs GLPK)",
                               FALSE),
                 conditionalPanel("input.rep_levcost",
                                  uiOutput("rep_lc_ui")),
                 verbatimTextOutput("rep_status"),
                 uiOutput("rep_preview"))
      ),
      tags$hr(),
      h5("Validation"),
      DT::DTOutput("issues")
    )
  )
)

server <- function(input, output, session) {
  output$theme_css <- renderUI({
    css <- .THEMES[[input$app_theme %||% "light"]]
    if (!nzchar(css)) return(NULL)
    tags$style(HTML(css))
  })

  spec <- reactiveVal(.blank_spec())
  undo_stack <- reactiveVal(list())
  redo_stack <- reactiveVal(list())
  editing <- reactiveValues()   # per-slot: index of the row being edited

  # Structural updates go through set_spec() so Undo/Redo can revert them
  # (metadata text fields sync via a plain observer and are not snapshotted;
  # they live in the browser's own undo).
  set_spec <- function(s) {
    st <- undo_stack()
    st <- c(st, list(spec()))
    if (length(st) > 50) st <- st[-1]
    undo_stack(st)
    redo_stack(list())
    spec(s)
  }
  observeEvent(input$undo, {
    st <- undo_stack()
    req(length(st) > 0)
    redo_stack(c(redo_stack(), list(spec())))
    spec(st[[length(st)]])
    undo_stack(st[-length(st)])
    set_identity_inputs(spec())
  })
  observeEvent(input$redo, {
    st <- redo_stack()
    req(length(st) > 0)
    undo_stack(c(undo_stack(), list(spec())))
    spec(st[[length(st)]])
    redo_stack(st[-length(st)])
    set_identity_inputs(spec())
  })

  block_cols <- function(b) energyRt:::.TECHSPEC_SECTIONS[[b]]$cols

  # ── wizard navigation ─────────────────────────────────────────────────
  output$wiz_where <- renderText({
    i <- match(input$wizard, .WIZ)
    paste0("Step ", i, " of ", length(.WIZ), ": ",
           .tab_label(input$wizard %||% ""))
  })
  observeEvent(input$nxt, {
    i <- match(input$wizard, .WIZ)
    if (i < length(.WIZ)) updateNavlistPanel(session, "wizard",
                                             selected = .WIZ[i + 1])
  })
  observeEvent(input$back, {
    i <- match(input$wizard, .WIZ)
    if (i > 1) updateNavlistPanel(session, "wizard",
                                  selected = .WIZ[i - 1])
  })

  # ── template / upload / save ──────────────────────────────────────────
  set_identity_inputs <- function(s) {
    updateTextInput(session, "name", value = s$name %||% "")
    updateTextInput(session, "desc", value = s$desc %||% "")
    updateTextInput(session, "region",
                    value = paste(s$region %||% character(), collapse = ", "))
    updateCheckboxInput(session, "fullYear",
                        value = !isFALSE(s$fullYear))
    updateCheckboxInput(session, "optretire",
                        value = isTRUE(s$optimizeRetirement))
    updateTextAreaInput(session, "notes", value = s$notes %||% "")
    updateTextInput(session, "unit_cap", value = s$units$capacity %||% "")
    updateTextInput(session, "unit_act", value = s$units$activity %||% "")
    updateTextInput(session, "unit_use", value = s$units$use %||% "")
    updateTextInput(session, "unit_cost", value = s$units$costs %||% "")
    updateNumericInput(session, "cap2act", value = s$cap2act %||% NA_real_)
    updateNumericInput(session, "lc_discount",
                       value = s$levcost$discount %||% 0.05)
    updateTextInput(session, "image_path", value = s$image %||% "")
    # spec-stored report template: a report() built-in name, or an .Rmd
    # path (legacy designer names map onto their report() successors)
    rp <- s$report
    if (!is.null(rp) && nzchar(rp)) {
      rp <- switch(rp, tech_report = "generic", tech_summary = "summary",
                   rp)
      ch <- rep_choices()
      sel <- if (rp %in% ch) rp else if (file.exists(rp)) rp else NULL
      if (!is.null(sel)) {
        if (!sel %in% ch) {
          ch[[sub("[.][Rr]md$", "", basename(sel))]] <- sel
          rep_choices(ch)
        }
        updateSelectInput(session, "rep_template", choices = ch,
                          selected = sel)
      }
    }
  }
  observeEvent(input$load_template, {
    s <- if (nzchar(input$template)) {
      energyRt::read_techspec(input$template)
    } else .blank_spec()
    set_spec(s); set_identity_inputs(s)
    updateNavlistPanel(session, "wizard", selected = "Metadata")
  })
  observeEvent(input$upload, {
    s <- energyRt::read_techspec(input$upload$datapath)
    set_spec(s); set_identity_inputs(s)
    updateNavlistPanel(session, "wizard", selected = "Metadata")
  })
  output$save <- downloadHandler(
    filename = function() paste0(spec()$name %||% "techspec", ".yml"),
    content = function(file) yaml::write_yaml(spec(), file)
  )
  output$save_json <- downloadHandler(
    filename = function() paste0(spec()$name %||% "techspec", ".json"),
    content = function(file) {
      # same container rules as tech_to_spec(file = "*.json")
      jsonlite::write_json(spec(), file, auto_unbox = TRUE, pretty = TRUE,
                           digits = NA, na = "null")
    }
  )
  output$save_rdata <- downloadHandler(
    filename = function() paste0(spec()$name %||% "techspec", ".RData"),
    content = function(file) {
      b <- built()
      if (inherits(b, "error")) {
        stop("the spec does not build: ", conditionMessage(b))
      }
      nm <- spec()$name %||% "TECH"
      e <- new.env(parent = emptyenv())
      assign(nm, b, envir = e)
      save(list = nm, file = file, envir = e)
    }
  )
  observeEvent(input$to_env, {
    b <- built()
    if (inherits(b, "error")) {
      showNotification(paste("the spec does not build:",
                             conditionMessage(b)), type = "error")
      return()
    }
    nm <- spec()$name %||% "TECH"
    assign(nm, b, envir = globalenv())
    showNotification(paste0("saved '", nm, "' to the R session ",
                            "(global environment)"), type = "message")
  })

  # ── process class: technology only, the rest reserved ─────────────────
  observeEvent(input$proc_class, {
    req(!identical(input$proc_class, "technology"))
    showNotification(paste0("'", input$proc_class, "' processes are ",
                            "reserved for a future release; only ",
                            "'technology' can be edited for now"),
                     type = "warning")
    updateSelectInput(session, "proc_class", selected = "technology")
  })

  # ── open an R object (global environment or uploaded .RData/.rds) ─────
  rdata_env <- reactiveVal(NULL)
  observeEvent(input$rdata_upload, {
    f <- input$rdata_upload
    req(!is.null(f$datapath), file.exists(f$datapath))
    e <- new.env(parent = emptyenv())
    ok <- tryCatch({
      if (grepl("[.]rds$", f$name, ignore.case = TRUE)) {
        assign(sub("[.]rds$", "", f$name, ignore.case = TRUE),
               readRDS(f$datapath), envir = e)
      } else {
        load(f$datapath, envir = e)
      }
      TRUE
    }, error = function(err) {
      showNotification(paste("cannot read file:", conditionMessage(err)),
                       type = "error")
      FALSE
    })
    if (ok) rdata_env(e)
  })
  output$envobj_ui <- renderUI({
    input$envobj_refresh   # re-scan the global environment on demand
    cls <- input$proc_class %||% "technology"
    ch <- character()
    for (nm in ls(globalenv())) {
      obj <- tryCatch(get(nm, envir = globalenv()),
                      error = function(e) NULL)
      if (methods::is(obj, cls)) {
        ch[paste0(nm, "  (R session)")] <- paste0("env::", nm)
      }
    }
    e <- rdata_env()
    if (!is.null(e)) {
      for (nm in ls(e)) {
        if (methods::is(get(nm, envir = e), cls)) {
          ch[paste0(nm, "  (file)")] <- paste0("file::", nm)
        }
      }
    }
    tagList(
      if (length(ch) == 0) {
        p(class = "text-muted", "no ", cls, " objects found - upload an ",
          ".RData / .rds below, or create one and refresh")
      } else {
        selectInput("envobj", NULL, choices = ch)
      },
      actionLink("envobj_refresh", "refresh list",
                 style = "font-size: 11px;")
    )
  })
  observeEvent(input$load_envobj, {
    v <- input$envobj %||% ""
    req(nzchar(v), grepl("::", v, fixed = TRUE))
    parts <- strsplit(v, "::", fixed = TRUE)[[1]]
    obj <- if (identical(parts[1], "env")) {
      tryCatch(get(parts[2], envir = globalenv()),
               error = function(e) NULL)
    } else if (!is.null(rdata_env())) {
      tryCatch(get(parts[2], envir = rdata_env()),
               error = function(e) NULL)
    }
    if (!methods::is(obj, "technology")) {
      showNotification("object not found or not a technology",
                       type = "error")
      return()
    }
    s <- energyRt::tech_to_spec(obj)
    set_spec(s); set_identity_inputs(s)
    updateNavlistPanel(session, "wizard", selected = "Metadata")
  })

  # ── metadata / units -> spec (not snapshotted) ────────────────────────
  # Debounced: every spec() change re-renders the whole info panel (draw,
  # YAML, validation, every table's units row), so keystrokes are batched
  # instead of rebuilding the technology per character — this also keeps
  # the app responsive to a single stop instead of interrupting mid-render.
  meta_in <- debounce(reactive(list(
    name = input$name, desc = input$desc, image = input$image_path,
    unit_cap = input$unit_cap, unit_act = input$unit_act,
    unit_use = input$unit_use, unit_cost = input$unit_cost,
    cap2act = input$cap2act, timeframe = input$timeframe,
    region = input$region, fullYear = input$fullYear,
    optretire = input$optretire, notes = input$notes)), 400)
  observeEvent(meta_in(), {
    m <- meta_in()
    s <- isolate(spec())
    s$name <- m$name
    s$desc <- m$desc
    s$image <- if (nzchar(m$image %||% "")) m$image else NULL
    un <- list(capacity = m$unit_cap, activity = m$unit_act,
               costs = m$unit_cost)
    if (nzchar(m$unit_use %||% "")) un$use <- m$unit_use
    s$units <- un
    s$cap2act <- if (isTRUE(is.finite(m$cap2act))) m$cap2act else NULL
    if (!is.null(m$timeframe) && nzchar(m$timeframe)) {
      s$timeframe <- m$timeframe
    } else {
      s$timeframe <- NULL
    }
    rg <- trimws(strsplit(m$region %||% "", ",")[[1]])
    rg <- rg[nzchar(rg)]
    s$region <- if (length(rg) > 0) rg else NULL
    # class defaults (fullYear TRUE, optimizeRetirement FALSE) are not
    # written into the spec; only deviations are
    s$fullYear <- if (isFALSE(m$fullYear)) FALSE else NULL
    s$optimizeRetirement <- if (isTRUE(m$optretire)) TRUE else NULL
    s$notes <- if (nzchar(m$notes %||% "")) m$notes else NULL
    spec(s)
  })
  ctx <- reactive({
    cal <- if (nzchar(input$ctx_calendar %||% "")) {
      energyRt::calendars[[input$ctx_calendar]]
    } else NULL
    list(calendar = cal,
         timeframes = if (!is.null(cal)) names(cal@timeframes) else
           character(),
         timeslices = if (!is.null(cal)) {
           as.character(cal@timeslice_share$timeslice)
         } else character(),
         years = seq(input$ctx_from %||% 2020, input$ctx_to %||% 2050, 5),
         regions = trimws(strsplit(input$ctx_regions %||% "", ",")[[1]]))
  })
  output$timeframe_ui <- renderUI({
    selectInput("timeframe", "Timeframe (commodity default)",
                choices = c("(default)" = "", ctx()$timeframes))
  })
  # image browse: modern native dialog (same one the browser's Upload
  # buttons show), keeping the file's ORIGINAL path for the spec; starts
  # in the last-used image directory, or getwd() on first use
  observeEvent(input$image_browse, {
    path <- .pick_file_native(
      title = "Choose an image",
      filter = paste0("Images (*.png;*.jpg;*.jpeg;*.gif;*.svg)|",
                      "*.png;*.jpg;*.jpeg;*.gif;*.svg|",
                      "All files (*.*)|*.*"),
      initial_dir = .recall_dir("image_dir"))
    if (nzchar(path)) {
      .remember_dir("image_dir", dirname(path))
      updateTextInput(session, "image_path", value = path)
    }
  })
  output$cap2act_unit <- renderText({
    act <- if (nzchar(input$unit_act %||% "")) input$unit_act else
      "{activity}"
    cp <- if (nzchar(input$unit_cap %||% "")) input$unit_cap else
      "{capacity}"
    paste0(act, "/", cp)
  })
  # image path check: local files -> exists / missing; URLs are shown as
  # such without a network round-trip
  img_check <- function(img_p) {
    if (grepl("^https?://", img_p)) return("url")
    if (file.exists(img_p)) "ok" else "missing"
  }
  .img_status_tag <- function(st) {
    switch(st,
      ok      = span("✓", title = "file found", style = paste0(
        "color: #2e7d32; font-weight: bold; font-size: 18px;")),
      missing = span("✗", title = "file not found", style = paste0(
        "color: #b30000; font-weight: bold; font-size: 18px;")),
      url     = span("URL", title = "not verified", style = "color: #888;"))
  }
  # current image of the spec: check + preview (the path itself lives in
  # the field above)
  output$image_current <- renderUI({
    img_p <- spec()$image
    req(nzchar(img_p %||% ""))
    st <- img_check(img_p)
    src <- if (st == "url") {
      img_p
    } else if (st == "ok") {
      addResourcePath("specimg",
                      dirname(normalizePath(img_p, winslash = "/")))
      paste0("specimg/", basename(img_p), "?v=",
             as.integer(file.mtime(img_p)))
    } else NULL
    tagList(
      div(.img_status_tag(st)),
      if (!is.null(src)) {
        tags$img(src = src, alt = "",
                 style = paste0("max-width: 100%; max-height: 200px;",
                                "margin-top: 6px;"),
                 onerror = "this.style.display='none'")
      }
    )
  })

  declared <- reactive({
    s <- spec()
    grab <- function(rows, field) {
      v <- vapply(rows %||% list(),
                  function(r) as.character(r[[field]] %||% NA), character(1))
      unique(v[!is.na(v) & nzchar(v)])
    }
    list(vintages = grab(s$vintages, "vintage"),
         clusters = grab(s$clusters, "cluster"),
         comms    = c(grab(s$inputs, "comm"), grab(s$outputs, "comm")),
         acomms   = grab(s$aux, "acomm"),
         groups   = unique(c(grab(s$groups, "group"),
                             grab(s$inputs, "group"),
                             grab(s$outputs, "group"))))
  })

  # ── ports: add-row helper (tables render via the generic loop below) ──
  for (kind in names(.PORT_SECS)) local({
    kk <- kind
    sec <- .PORT_SECS[[kk]]
    observeEvent(input[[paste0("addport_", kk)]], {
      req(nzchar(input[[paste0("comm_", kk)]]))
      s <- spec()
      cfield <- if (kk == "aux") "acomm" else "comm"
      row <- stats::setNames(
        list(input[[paste0("comm_", kk)]], input[[paste0("unit_", kk)]]),
        c(cfield, "unit"))
      s[[sec]] <- c(s[[sec]] %||% list(), list(row))
      set_spec(s)
    })
  })

  # ── tables: ports + structural + data slots ───────────────────────────
  .ALL_TBL <- c(unname(.PORT_SECS), "groups", "vintages", "clusters",
                unname(.SLOT_TABS))
  for (b in .ALL_TBL) local({
    bb <- b
    is_slot <- bb %in% .SLOT_TABS
    is_port <- bb %in% .PORT_SECS
    shown_now <- function(rows, cols) {
      if (is_slot || is_port) {
        .shown_cols(rows, cols, isTRUE(input[[paste0("showall_", bb)]]))
      } else cols
    }
    output[[paste0("tbl_", bb)]] <- DT::renderDT({
      rows <- spec()[[bb]] %||% list()
      cols <- block_cols(bb)
      shown <- shown_now(rows, cols)
      args <- list(.rows_to_display(rows, cols)[, shown, drop = FALSE],
                   rownames = FALSE, selection = "single", editable = TRUE,
                   options = list(dom = "t", pageLength = 25,
                                  scrollX = TRUE))
      cont <- .units_container(
        shown, energyRt:::.TECHSPEC_SECTIONS[[bb]]$arg, units_tbl())
      if (!is.null(cont)) args$container <- cont
      do.call(DT::datatable, args)
    })
    observeEvent(input[[paste0("tbl_", bb, "_cell_edit")]], {
      e <- input[[paste0("tbl_", bb, "_cell_edit")]]
      s <- spec()
      rows <- s[[bb]] %||% list()
      cols <- block_cols(bb)
      shown <- shown_now(rows, cols)
      df <- .rows_to_display(rows, cols)
      df[e$row, shown[e$col + 1]] <- e$value
      s[[bb]] <- .display_to_rows(df)
      set_spec(s)
    })
    observeEvent(input[[paste0("add_", bb)]], {
      s <- spec()
      s[[bb]] <- c(s[[bb]] %||% list(), list(list()))
      set_spec(s)
    })
    observeEvent(input[[paste0("del_", bb)]], {
      i <- input[[paste0("tbl_", bb, "_rows_selected")]]
      req(length(i) == 1)
      s <- spec()
      s[[bb]] <- s[[bb]][-i]
      set_spec(s)
      editing[[bb]] <- NULL
    })
    if (is_slot) {
      # "Edit row": load the selected row into the constructor; the add
      # button then reads "Save" and updates that row in place.
      observeEvent(input[[paste0("edit_", bb)]], {
        i <- input[[paste0("tbl_", bb, "_rows_selected")]]
        if (length(i) != 1) {
          showNotification("select a row to edit", type = "warning")
        } else {
          editing[[bb]] <- i
        }
      })
      observeEvent(input[[paste0("ctorcancel_", bb)]], {
        editing[[bb]] <- NULL
      })

      output[[paste0("ctor_title_", bb)]] <- renderUI({
        if (!is.null(editing[[bb]])) {
          h5(sprintf("Edit row %d", editing[[bb]]))
        } else {
          h5("Add a value")
        }
      })

      output[[paste0("ctor_", bb)]] <- renderUI({
        d <- declared(); cx <- ctx()
        edit_i <- editing[[bb]]
        row <- if (!is.null(edit_i)) {
          rows <- isolate(spec())[[bb]] %||% list()
          if (edit_i <= length(rows)) rows[[edit_i]] else list()
        } else list()
        meta <- .sec_meta(bb)
        dims <- meta$dims
        sel <- function(dim, choices) {
          if (length(choices) == 0) return(NULL)
          selected <- as.character(row[[dim]] %||% "ALL")
          column(3, selectInput(paste0("ctor_", bb, "_", dim), dim,
                                choices = unique(c("ALL", choices,
                                                   selected)),
                                selected = selected))
        }
        dim_ui <- list(
          if ("vintage" %in% dims && length(d$vintages))
            sel("vintage", d$vintages),
          if ("cluster" %in% dims && length(d$clusters))
            sel("cluster", d$clusters),
          if ("region" %in% dims) sel("region", cx$regions),
          if ("year" %in% dims) sel("year", cx$years),
          if ("timeslice" %in% dims && length(cx$timeslices))
            sel("timeslice", cx$timeslices),
          if ("weather" %in% dims) {
            # weather names are free-form (they reference weather objects);
            # offer the ones already used and allow typing new ones
            wn <- unique(vapply(isolate(spec())$weather %||% list(),
                                function(r) {
                                  as.character(r$weather %||% NA)
                                }, character(1)))
            wn <- wn[!is.na(wn) & nzchar(wn)]
            column(3, selectizeInput(
              paste0("ctor_", bb, "_weather"), "weather",
              choices = unique(c(wn, as.character(row$weather %||% ""))),
              selected = as.character(row$weather %||% ""),
              options = list(create = TRUE, placeholder = "weather")))
          }
        )
        dim_ui <- dim_ui[!vapply(dim_ui, is.null, logical(1))]
        pars <- meta$params
        row_pars <- intersect(names(row), pars)
        par <- input[[paste0("ctorpar_", bb)]] %||% NULL
        if (!is.null(edit_i) && length(row_pars) > 0) par <- row_pars[1]
        if (is.null(par) || !par %in% pars) par <- pars[1]
        val <- if (par %in% names(row)) as.numeric(row[[par]]) else NA
        extra <- .param_extra(bb, par)
        extra_ui <- lapply(extra, function(ex) {
          ch <- if (ex == "acomm") d$acomms else
            if (ex == "group") d$groups else d$comms
          selected <- as.character(row[[ex]] %||% (if (length(ch)) ch[1]
                                                   else ""))
          column(3, selectInput(paste0("ctor_", bb, "_", ex), ex,
                                choices = unique(c(ch, selected)),
                                selected = selected))
        })
        # two rows: index columns on top (wider boxes), then the
        # parameter with its extra indices, the value and the buttons
        tagList(
          if (length(dim_ui) > 0) do.call(fluidRow, dim_ui),
          do.call(fluidRow, c(
            list(column(3, selectInput(paste0("ctorpar_", bb),
                                       "parameter",
                                       choices = pars, selected = par))),
            extra_ui,
            list(column(2, numericInput(paste0("ctorval_", bb), "value",
                                        val)),
                 column(3, br(),
                        actionButton(paste0("ctoradd_", bb),
                                     if (is.null(edit_i)) "+ Add row"
                                     else "Save",
                                     class = "btn-primary"),
                        if (!is.null(edit_i))
                          actionButton(paste0("ctorcancel_", bb),
                                       "Cancel")))
          ))
        )
      })

      observeEvent(input[[paste0("ctoradd_", bb)]], {
        par <- input[[paste0("ctorpar_", bb)]]
        val <- input[[paste0("ctorval_", bb)]]
        req(nzchar(par %||% ""), isTRUE(is.finite(val)),
            par %in% .sec_meta(bb)$params)
        dims <- .sec_meta(bb)$dims
        edit_i <- editing[[bb]]
        s <- spec()
        rows <- s[[bb]] %||% list()
        # editing: start from the existing row (other parameter values on
        # it are preserved); adding: start blank
        row <- if (!is.null(edit_i) && edit_i <= length(rows)) {
          rows[[edit_i]]
        } else list()
        for (dim in dims) {
          v <- input[[paste0("ctor_", bb, "_", dim)]]
          if (is.null(v) || !nzchar(v) || identical(v, "ALL")) {
            row[[dim]] <- NULL
          } else {
            row[[dim]] <- v
          }
        }
        for (ex in .param_extra(bb, par)) {
          v <- input[[paste0("ctor_", bb, "_", ex)]]
          if (!is.null(v) && nzchar(v)) row[[ex]] <- v
        }
        row[[par]] <- val
        if (!is.null(edit_i) && edit_i <= length(rows)) {
          rows[[edit_i]] <- row
          editing[[bb]] <- NULL
        } else {
          rows <- c(rows, list(row))
        }
        s[[bb]] <- rows
        set_spec(s)
      })
    }
  })

  # ── build + info panel ────────────────────────────────────────────────
  built <- reactive({
    tryCatch(suppressWarnings(energyRt::tech_from_spec(spec())),
             error = function(e) e)
  })
  # slot/parameter/unit table for the tables' units header rows; NULL while
  # the spec does not build (the tables then render without a units row).
  # complete = TRUE: parameters without data still show their unit.
  units_tbl <- reactive({
    b <- built()
    if (inherits(b, "error")) return(NULL)
    tryCatch(as.data.frame(energyRt::get_units(b, complete = TRUE)),
             error = function(e) NULL)
  })
  # Draw scrollers: the selection goes to draw(vintage=, cluster=), which
  # keeps the other vintages visible as faded ghosts behind the selected
  # one (and the cluster rail below). Index sliders -- one notch per
  # DECLARED level, nothing in between -- centred under the schematic with
  # prev/next arrows; defaults match draw()'s own (newest vintage, first
  # cluster). Position survives re-renders via isolate().
  .draw_scroller <- function(id, levels, default_i) {
    n <- length(levels)
    cur <- min(isolate(input[[id]]) %||% default_i, n)
    div(style = "max-width: 420px; margin: 0 auto;",
        div(style = "display: flex; align-items: center; gap: 8px;",
            actionButton(paste0(id, "_prev"), "<", class = "btn-sm"),
            div(style = "flex: 1;",
                sliderInput(id, NULL, min = 1, max = n, value = cur,
                            step = 1, ticks = FALSE, width = "100%")),
            actionButton(paste0(id, "_next"), ">", class = "btn-sm")),
        div(style = paste0("text-align: center; color: #777;",
                           "font-size: 12px; margin-top: -8px;"),
            textOutput(paste0(id, "_lab"), inline = TRUE)))
  }
  output$draw_dims_ui <- renderUI({
    d <- declared()
    parts <- list()
    if (length(d$vintages) > 1) {
      parts <- c(parts, list(.draw_scroller("draw_vintage_i", d$vintages,
                                            length(d$vintages))))
    }
    if (length(d$clusters) > 1) {
      parts <- c(parts, list(.draw_scroller("draw_cluster_i",
                                            d$clusters, 1)))
    }
    if (length(parts) == 0) return(NULL)
    do.call(tagList, parts)
  })
  sel_level <- function(levels, i, default_i) {
    if (length(levels) < 2) return(NULL)
    i <- i %||% default_i
    levels[min(max(i, 1), length(levels))]
  }
  draw_sel <- reactive({
    d <- declared()
    list(vintage = sel_level(d$vintages, input$draw_vintage_i,
                             length(d$vintages)),
         cluster = sel_level(d$clusters, input$draw_cluster_i, 1))
  })
  output$draw_vintage_i_lab <- renderText({
    d <- declared()
    v <- sel_level(d$vintages, input$draw_vintage_i, length(d$vintages))
    if (is.null(v)) "" else paste0("vintage: ", v)
  })
  output$draw_cluster_i_lab <- renderText({
    d <- declared()
    v <- sel_level(d$clusters, input$draw_cluster_i, 1)
    if (is.null(v)) "" else paste0("cluster: ", v)
  })
  for (ax in c("draw_vintage_i", "draw_cluster_i")) local({
    id <- ax
    observeEvent(input[[paste0(id, "_prev")]], {
      updateSliderInput(session, id,
                        value = max(1, (input[[id]] %||% 1) - 1))
    })
    observeEvent(input[[paste0(id, "_next")]], {
      # the slider clamps to its max client-side
      updateSliderInput(session, id, value = (input[[id]] %||% 0) + 1)
    })
  })
  output$schematic <- renderPlot({
    b <- built()
    if (inherits(b, "error")) {
      plot.new()
      text(0.5, 0.5, paste("cannot draw:\n", conditionMessage(b)),
           col = "grey40")
    } else {
      sel <- draw_sel()
      # draw() fans the non-selected vintages out as faded ghosts behind
      # the selected one; fall back to the plain figure if the variant
      # machinery rejects the selection mid-edit
      tryCatch(
        energyRt::draw(b, vintage = sel$vintage, cluster = sel$cluster),
        error = function(e) energyRt::draw(b))
    }
  })
  output$slotplot <- renderPlot({
    b <- built()
    validate(need(!inherits(b, "error"), "fix spec errors first"))
    blk <- input$ap_block %||% "invcost"
    sec <- energyRt:::.TECHSPEC_SECTIONS[[blk]]
    df <- methods::slot(b, sec$arg)
    df <- df[rowSums(!is.na(df)) > 0, , drop = FALSE]
    validate(need(nrow(df) > 0, paste("no", blk, "data")))
    vals <- setdiff(names(df), c("vintage", "cluster", "region", "year",
                                 "timeslice"))
    vals <- vals[vapply(df[vals], function(x) any(!is.na(x)), logical(1))]
    validate(need(length(vals) > 0, "no numeric values"))
    xdim <- if (any(!is.na(df$vintage))) "vintage" else if (
      "year" %in% names(df) && any(!is.na(df$year))) "year" else NULL
    validate(need(!is.null(xdim), "no vintage/year dimension to plot over"))
    x <- df[[xdim]]
    old <- par(mfrow = c(1, length(vals)), mar = c(4, 4, 2, 1))
    on.exit(par(old))
    for (v in vals) {
      barplot(df[[v]], names.arg = x, main = v, xlab = xdim,
              col = "#4477aa")
    }
  })
  output$yaml_out <- renderText(yaml::as.yaml(spec()))
  output$code_out <- renderText({
    tryCatch(energyRt::tech_spec_code(spec()),
             error = function(e) paste("##", conditionMessage(e)))
  })
  output$issues <- DT::renderDT({
    iss <- tryCatch(energyRt::tech_spec_issues(spec()),
                    error = function(e) data.frame(
                      severity = "error", section = "spec",
                      message = conditionMessage(e)))
    DT::datatable(iss, rownames = FALSE,
                  options = list(dom = "t", pageLength = 25)) |>
      DT::formatStyle("severity",
                      color = DT::styleEqual(c("error", "warning"),
                                             c("#b30000", "#8a6d00")))
  })

  # ── levcost (on demand) ───────────────────────────────────────────────
  # commodities levcost() can price: inputs and consumed aux commodities
  lc_comms <- function() {
    s <- spec()
    grab <- function(rows, f) {
      v <- vapply(rows %||% list(), function(r) r[[f]] %||% "",
                  character(1))
      v[nzchar(v)]
    }
    c(grab(s$inputs, "comm"), grab(s$aux, "acomm"))
  }
  # price unit for a priced commodity: <costs>/<commodity unit>, e.g. "MEUR/GWh"
  lc_comm_unit <- function(cm) {
    s <- spec()
    unit_of <- function(rows, key) {
      for (r in rows %||% list()) if ((r[[key]] %||% "") == cm) return(r$unit %||% "")
      ""
    }
    cu <- unit_of(s$inputs, "comm")
    if (!nzchar(cu)) cu <- unit_of(s$aux, "acomm")
    costs <- s$units$costs %||% ""
    if (nzchar(costs) && nzchar(cu)) paste0(costs, "/", cu) else ""
  }
  output$lc_fuel_ui <- renderUI({
    comms <- lc_comms()
    if (length(comms) == 0) return(p(class = "text-muted",
                                     "no priced commodities"))
    lcv <- spec()$levcost   # stored assumptions pre-fill the fields
    do.call(fluidRow, lapply(comms, function(cm) {
      u   <- lc_comm_unit(cm)
      lbl <- if (nzchar(u)) paste0(cm, " cost (", u, ")") else paste0(cm, " cost")
      column(max(2, floor(12 / max(1, length(comms)))),
             numericInput(paste0("lc_fuel_", cm), lbl,
                          as.numeric(lcv$costs[[cm]] %||% 0), step = 0.5))
    }))
  })
  # the assumptions used on Compute persist to the spec (-> Save YAML)
  observeEvent(input$lc_go, {
    comms <- lc_comms()
    fc <- lapply(stats::setNames(comms, comms), function(cm) {
      v <- input[[paste0("lc_fuel_", cm)]]
      if (isTRUE(is.finite(v))) v else 0
    })
    s <- spec()
    s$levcost <- list(discount = input$lc_discount %||% 0.05, costs = fc)
    spec(s)
  })
  # Two runs per Compute: the variant set for the NPV comparison, and ONE
  # instance (draw()'s default cell: newest vintage, first cluster) with
  # frontier = TRUE for the by-year / frontier figures. For a plain
  # technology they are the same single run.
  lc <- eventReactive(input$lc_go, {
    b <- built()
    if (inherits(b, "error")) return(b)
    comms <- lc_comms()
    fc <- vapply(comms, function(cm) {
      v <- input[[paste0("lc_fuel_", cm)]]
      if (isTRUE(is.finite(v))) v else 0
    }, numeric(1))
    dsc <- input$lc_discount %||% 0.05
    inst <- energyRt:::.levcost_instance(b)
    single <- tryCatch(suppressMessages(suppressWarnings(
      energyRt::levcost(inst, fuel_costs = fc, discount = dsc,
                        frontier = TRUE, method = "auto", verbose = FALSE))),
      error = function(e) e)
    is_var <- !is.null(energyRt:::.tech_variants(b))
    variants <- if (!is_var) single else tryCatch(
      suppressMessages(suppressWarnings(
        energyRt::levcost(b, fuel_costs = fc, discount = dsc,
                          frontier = FALSE, method = "auto",
                          verbose = FALSE))),
      error = function(e) e)
    list(single = single, variants = variants, is_var = is_var,
         inst_name = inst@name)
  })
  output$lc_note <- renderUI({
    x <- lc()
    req(!inherits(x, "error"), isTRUE(x$is_var))
    p(class = "text-muted", style = "margin-top: 8px;",
      sprintf("components / frontier figures show one instance: %s",
              x$inst_name))
  })
  output$lc_tbl <- DT::renderDT({
    x <- lc()
    validate(need(!inherits(x, "error"), "fix spec errors first"))
    v <- x$variants
    msg <- if (inherits(v, "error")) {
      paste("levcost failed:", conditionMessage(v))
    } else ""
    validate(need(!inherits(v, "error"), msg))
    df <- if (inherits(v, "levcost_list")) {
      do.call(rbind, lapply(names(v), function(nm) {
        npv <- v[[nm]]$levcost_npv
        data.frame(variant = nm,
                   output = if (is.null(names(npv))) "" else names(npv),
                   lcoe_npv = signif(as.numeric(npv), 5),
                   stringsAsFactors = FALSE)
      }))
    } else {
      cbd <- v$cost_breakdown_npv
      if (!is.null(cbd) && nrow(cbd) > 0) {
        rbind(data.frame(component = as.character(cbd$component),
                         value = signif(cbd$value, 5),
                         stringsAsFactors = FALSE),
              data.frame(component = "TOTAL",
                         value = signif(sum(cbd$value, na.rm = TRUE), 5)))
      } else {
        data.frame(lcoe_npv = signif(as.numeric(v$levcost_npv), 5))
      }
    }
    DT::datatable(df, rownames = FALSE,
                  options = list(dom = "t", pageLength = 25))
  })
  output$lc_plot <- renderPlot({
    x <- lc()
    validate(need(!inherits(x, "error"), ""))
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
      plot.new(); text(0.5, 0.5, "install ggplot2 for the levcost plots")
      return(invisible())
    }
    tp <- input$lc_type %||% "npv"
    target <- if (x$is_var && tp == "npv") x$variants else x$single
    validate(need(!inherits(target, "error"), "levcost failed"))
    pl <- if (x$is_var && tp == "npv") {
      ggplot2::autoplot(target)
    } else {
      suppressMessages(ggplot2::autoplot(target, type = tp))
    }
    validate(need(!is.null(pl), paste("no data for the", tp, "figure")))
    print(pl)
  })

  # ── report (on demand) ────────────────────────────────────────────────
  rep_file <- reactiveVal(NULL)
  rep_msg <- reactiveVal("choose a template and format, then press Render")
  # template gallery: built-ins + uploaded .Rmd files; the spec's `report`
  # key (a built-in name or a path) selects one on load
  rep_choices <- reactiveVal(.rep_templates)
  observeEvent(input$rep_upload, {
    f <- input$rep_upload
    req(!is.null(f$datapath), file.exists(f$datapath))
    tpl_dir <- file.path(tempdir(), "energyRt_designer_templates")
    dir.create(tpl_dir, showWarnings = FALSE, recursive = TRUE)
    dest <- normalizePath(file.path(tpl_dir, f$name), winslash = "/",
                          mustWork = FALSE)
    file.copy(f$datapath, dest, overwrite = TRUE)
    ch <- rep_choices()
    ch[[sub("[.][Rr]md$", "", f$name)]] <- dest
    rep_choices(ch)
    updateSelectInput(session, "rep_template", choices = ch,
                      selected = dest)
  })
  rep_ports <- function() {
    s <- spec()
    grab <- function(rows, f) {
      v <- vapply(rows %||% list(), function(r) r[[f]] %||% "",
                  character(1))
      unique(v[nzchar(v)])
    }
    list(inputs = grab(s$inputs, "comm"),
         outputs = grab(s$outputs, "comm"),
         aux = grab(s$aux, "acomm"))
  }
  output$rep_lc_ui <- renderUI({
    pp <- rep_ports()
    lcv <- spec()$levcost   # stored assumptions pre-fill the fields
    fld <- function(prefix, cm, tag) {
      column(3, numericInput(paste0(prefix, cm),
                             paste0(cm, " (", tag, ")"),
                             as.numeric(lcv$costs[[cm]] %||% 0),
                             step = 0.5))
    }
    tagList(
      fluidRow(column(3, numericInput("rep_discount", "discount",
                                      as.numeric(lcv$discount %||% 0.05),
                                      step = 0.01))),
      do.call(fluidRow, c(
        lapply(pp$inputs, function(cm) fld("repcost_in_", cm, "input")),
        lapply(pp$aux, function(cm) fld("repcost_aux_", cm, "aux"))))
    )
  })
  # Render via energyRt::report(): the app shares the package's report
  # system (inst/templates/report_*.Rmd) instead of shipping its own.
  observeEvent(input$rep_go, {
    rep_file(NULL)
    if (!requireNamespace("rmarkdown", quietly = TRUE)) {
      rep_msg("rendering requires the 'rmarkdown' package")
      return()
    }
    tpl <- input$rep_template %||% "generic"
    is_builtin <- tpl %in% .rep_templates
    if (!is_builtin && !file.exists(tpl)) {
      rep_msg("report template not found")
      return()
    }
    b <- built()
    if (inherits(b, "error")) {
      rep_msg(paste("the spec does not build:", conditionMessage(b)))
      return()
    }
    # remember the chosen template in the spec (built-ins by report()'s
    # name, uploads by path) -> Save YAML keeps it
    s_rep <- spec()
    s_rep$report <- if (is_builtin) tpl else
      normalizePath(tpl, winslash = "/", mustWork = FALSE)
    spec(s_rep)
    # levcost: report() runs levcost() itself from the assumption fields;
    # the assumptions used persist to the spec (-> Save YAML)
    lc_args <- list(levcost = FALSE)
    if (isTRUE(input$rep_levcost)) {
      pp <- rep_ports()
      gv <- function(prefix, cms) {
        vapply(cms, function(cm) {
          x <- input[[paste0(prefix, cm)]]
          if (isTRUE(is.finite(x))) x else 0
        }, numeric(1))
      }
      fc <- c(gv("repcost_in_", pp$inputs), gv("repcost_aux_", pp$aux))
      dsc <- input$rep_discount %||% 0.05
      lc_args <- list(levcost = TRUE, discount = dsc, verbose = FALSE)
      if (length(fc) > 0) lc_args$fuel_costs <- fc
      s2 <- spec()
      s2$levcost <- list(discount = dsc, costs = as.list(fc))
      spec(s2)
    }
    out_dir <- file.path(tempdir(), "energyRt_designer_report")
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
    file_base <- file.path(out_dir,
                           paste0(spec()$name %||% "tech", "_report"))
    img <- spec()$image
    img_ok <- !is.null(img) && nzchar(img) &&
      !grepl("^https?://", img) && file.exists(img)
    res <- tryCatch(
      withProgress(message = "rendering report...", value = 0.3, {
        suppressWarnings(do.call(energyRt::report, c(
          list(b, template = tpl,
               format = input$rep_format %||% "html",
               file = file_base, open = FALSE,
               image_file = if (img_ok) img else NULL),
          lc_args)))
      }),
      error = function(e) e)
    if (inherits(res, "error")) {
      rep_msg(paste0("render failed: ", conditionMessage(res),
                     if (identical(input$rep_format, "pdf")) {
                       "\n(pdf needs a LaTeX installation, e.g. tinytex)"
                     } else ""))
    } else {
      rep_file(res[1])
      rep_msg(paste("rendered:", basename(res[1])))
    }
  })
  output$rep_status <- renderText(rep_msg())
  # Inline preview of the rendered report: html always (self-contained),
  # pdf via the browser's built-in viewer (best-effort - the external
  # browser is more reliable than the RStudio window for pdf), docx has
  # no in-browser viewer and stays download-only.
  output$rep_preview <- renderUI({
    f <- rep_file()
    req(f, file.exists(f))
    ext <- tolower(tools::file_ext(f))
    if (!ext %in% c("html", "pdf")) {
      return(p(class = "text-muted",
               "no inline preview for .docx - use Download"))
    }
    addResourcePath("techreport", dirname(f))
    tags$iframe(
      src = paste0("techreport/", basename(f), "?v=",
                   as.integer(file.mtime(f))),
      style = paste0("width: 100%; height: 520px;",
                     "border: 1px solid #ddd; margin-top: 6px;"))
  })
  output$rep_dl_ui <- renderUI({
    req(rep_file())
    tagList(downloadButton("rep_dl", "Download"),
            actionButton("rep_open", "Open in browser",
                         style = "margin-top: 4px;"))
  })
  observeEvent(input$rep_open, {
    f <- rep_file()
    req(!is.null(f), file.exists(f))
    utils::browseURL(f)
  })
  output$rep_dl <- downloadHandler(
    filename = function() basename(rep_file()),
    content = function(file) file.copy(rep_file(), file)
  )

  # ── initial spec (process_designer(spec = ...)) ───────────────────────
  # last: set_identity_inputs() touches reactives defined above
  init <- getOption("energyRt.designer.spec")
  if (!is.null(init)) {
    spec(init)
    isolate(set_identity_inputs(init))
  }
}

shinyApp(ui, server)
