# =========================================================================== #
# build_matrix.R -- generate the parameter-level test-coverage matrix.
#
# Usage:
#   Rscript tools/coverage/build_matrix.R            # regenerate CSV + summary
#   Rscript tools/coverage/build_matrix.R --check    # validate @covers tags only
#
# Rows come from the package's own metadata (sysdata: .modInp, .mapping_spec,
# .equation_description, .variables) -- never hand-enumerated, so a new
# parameter shows up automatically as covered_depth = "none".
#
# Coverage comes from two scans of tests/testthat/*.R:
#   1. Structured tags (authoritative):
#        # @covers pTechRampUp pTechRampDown depth=S backends=glpk forks=fx,fold
#      depth in C < I < S < X; backends/forks comma-separated, optional.
#   2. Keyword inference (fallback, marked status="inferred"): a row is
#      inferred-covered when a test file mentions its exact name (or, for
#      class-sourced parameters, its colName in assignment position); the
#      depth is the file's own depth (C = constructed only, I = interpolates,
#      S = solves, X = names >= 2 solver backends).
#
# Outputs: tests/coverage/parameter-matrix.csv, tests/coverage/matrix-summary.md
# Tag convention documented in tests/README.md; recipes in dev/TESTING.md.
# =========================================================================== #

suppressMessages({
  # Prefer the source tree over a possibly stale installed copy: metadata
  # (sysdata) must match the code under test.
  if (file.exists("DESCRIPTION") &&
      isTRUE(read.dcf("DESCRIPTION")[1, "Package"] == "energyRt") &&
      requireNamespace("pkgload", quietly = TRUE)) {
    pkgload::load_all(".", quiet = TRUE)
  } else {
    library(energyRt)
  }
  library(data.table)
})

.cov_ns <- asNamespace("energyRt")
.cov_get <- function(nm) get(nm, envir = .cov_ns, inherits = FALSE)

DEPTHS <- c(none = 0L, C = 1L, I = 2L, S = 3L, X = 4L)
depth_max <- function(a, b) names(DEPTHS)[max(DEPTHS[[a]], DEPTHS[[b]]) + 1L]

# --------------------------------------------------------------------------- #
# Row assembly from package metadata
# --------------------------------------------------------------------------- #

# Feature-family classifier for numeric parameters (the ~35 families of the
# testing plan). Falls back to a coarse prefix family for maps/eqs/vars.
param_family <- function(name) {
  rules <- list(
    c("^pTech(Cinp2use|Ginp2use|Cinp2ginp|Cact2cout|Use2cact|Afc)", "tech-efficiency"),
    c("^pTech(Af|Afs|Ramp)", "tech-availability"),
    c("^pTechWeather", "tech-weather"),
    c("^pTech(Cinp|Cout|Cap|NCap|Pho|Ret|Act)2A(Inp|Out)", "tech-aux-flows"),
    c("^pTech(Invcost|Fixom|Varom|Cvarom|Avarom)$", "tech-costs"),
    c("^pTech(Eac|Wacc|Payback)", "tech-eac-wacc-payback"),
    c("^pTech(Cap$|CapLo|CapUp|NewCap|Cap2act)", "tech-capacity-bounds"),
    c("^pTech(Stock|Olife|Ret$|RetLo|RetUp|RetCost)", "tech-stock-retirement"),
    c("^pTechShare", "tech-share-bounds"),
    c("^pTechEmisComm", "tech-emis"),
    c("^pStorage(InpEff|OutEff|StgEff)", "storage-efficiency"),
    c("^pStorageInp2out", "storage-inp2out"),
    c("^pStorageDuration", "storage-duration"),
    c("^pStorageStartLevel", "storage-startlevel-fullyear"),
    c("^pStorageCost(Inp|Out|Store)", "storage-varom"),
    c("^pStorageWeather", "storage-weather"),
    c("^pStorage(Cinp|Cout|Cap|NCap|Pho|Ret|Stg)2A(Inp|Out)|^pStorageNCap2Stg|^pStorageStg2A", "storage-aux-flows"),
    c("^pStorage(Inp|Out|Stg)?(Invcost|Fixom|Eac|Wacc|Payback|RetCost|Stock|Olife)", "storage-costs-eac"),
    c("^pStorage(Af|Cinp$|Cout$|InpCap|OutCap|StgCap|InpNewCap|OutNewCap|StgNewCap|InpRet|OutRet|StgRet|InpStock|OutStock|StgStock|InpCap2act|OutCap2act)", "storage-roles-capacities"),
    c("^pTradeIr(Af)?$|^pTradeIrEff$", "trade-core"),
    c("^pTradeIr", "trade-costs"),
    c("^pTrade", "trade-capacity-vintage"),
    c("^pSup", "supply"),
    c("^pDemand$", "demand"),
    c("^p(Export|Import)Row", "import-export-row"),
    c("^p(Tax|Sub)", "tax-subsidy"),
    c("^pTimeslice|^pYearFraction$", "calendar-timeslices"),
    c("^pWacc$|^pSdr$|^pDiscountFactor", "discounting"),
    c("^pDummy", "dummy-debug"),
    c("^pEmissionFactor$|^pAggregateFactor$", "commodity"),
    c("^pWeather$", "weather-class"),
    c("^pPeriodLen$|^ordYear$|^cardYear$", "horizon-periodlen")
  )
  for (r in rules) if (grepl(r[1], name)) return(r[2])
  # coarse fallback by embedded object prefix
  m <- regmatches(name, regexpr("Tech|Storage|Trade|Sup|Dem|Comm|Export|Import|Weather|Aggregate|Emission|Balance|Objective|Cost|Timeslice", name))
  if (length(m) && nzchar(m)) return(paste0("other-", tolower(m)))
  "other"
}

# expand a bounds entry name to its Lo/Up pXxx pair
bounds_expand <- function(name) paste0(name, c("Lo", "Up"), collapse = ",")

assemble_rows <- function() {
  modInp <- .cov_get(".modInp")
  mspec  <- .cov_get(".mapping_spec")
  eqd    <- .cov_get(".equation_description")
  vars   <- .cov_get(".variables")

  chr1 <- function(x) if (is.null(x) || !length(x)) "" else paste(x, collapse = "+")

  prm <- rbindlist(lapply(modInp, function(e) {
    data.table(
      name = e$name,
      kind = e$type,   # set | map | numpar | bounds
      dimSets = chr1(e$dimSets),
      defVal = chr1(e$defVal),
      interpolation_rule = chr1(e$interpolation),
      class = chr1(e$class), slot = chr1(e$slot), colName = chr1(e$colName),
      pXxx_expanded = if (identical(e$type, "bounds")) bounds_expand(e$name) else "",
      gates = ""
    )
  }))

  map_gates <- rbindlist(lapply(mspec, function(m) {
    data.table(name = m$name,
               gates = paste(c(unlist(m$gates_var), unlist(m$gates_eq)), collapse = ","))
  }))
  prm[map_gates, gates := i.gates, on = "name"]

  eqs <- data.table(
    name = names(eqd), kind = "equation", dimSets = "", defVal = "",
    interpolation_rule = "", class = "", slot = "", colName = "",
    pXxx_expanded = "", gates = ""
  )

  vrs <- rbindlist(lapply(vars, function(v) {
    data.table(
      name = v$name, kind = "variable", dimSets = chr1(v$dimSets), defVal = "",
      interpolation_rule = "", class = "", slot = "",
      colName = chr1(v$role),          # role recorded in colName column for variables
      pXxx_expanded = "", gates = chr1(v$gate)
    )
  }))

  rows <- rbindlist(list(prm, eqs, vrs))
  rows[, family := vapply(name, param_family, "")]
  rows[kind %in% c("map", "set"), family := paste0(kind, ":", family)]
  rows[kind == "equation", family := paste0("eq:", family)]
  rows[kind == "variable", family := paste0("var:", family)]
  rows[]
}

# --------------------------------------------------------------------------- #
# Test-file scanning
# --------------------------------------------------------------------------- #

test_dir_default <- function() file.path("tests", "testthat")

# depth of a test file from what it does
file_depth <- function(txt) {
  solves  <- any(grepl("solve_scen|solve_mod|solve_model|solve_scenario|vt_solve|rt_solve|sp_solve|tr_solve|solve\\(", txt))
  interps <- any(grepl("interpolate_model|interp_mod|vt_interp|rt_interp|interpolate\\(", txt))
  backends <- c("GAMS|gams", "JuMP|julia", "PYOMO|pyomo", "GLPK|glpk|glpsol")
  nb <- sum(vapply(backends, function(b) any(grepl(b, txt)), TRUE))
  if (solves && nb >= 2) "X" else if (solves) "S" else if (interps) "I" else "C"
}

# parse "# @covers name1 name2 depth=S backends=glpk forks=fx,fold" lines
parse_tags <- function(lines, file) {
  hits <- grep("^\\s*#+\\s*@covers\\s+", lines, value = TRUE)
  if (!length(hits)) return(NULL)
  rbindlist(lapply(hits, function(h) {
    body <- sub("^\\s*#+\\s*@covers\\s+", "", h)
    toks <- strsplit(trimws(body), "\\s+")[[1]]
    kv <- grepl("=", toks, fixed = TRUE)
    opts <- toks[kv]; names_ <- toks[!kv]
    getopt <- function(key, default) {
      m <- grep(paste0("^", key, "="), opts, value = TRUE)
      if (length(m)) sub(paste0("^", key, "="), "", m[1]) else default
    }
    data.table(name = names_, depth = getopt("depth", "S"),
               backends = getopt("backends", "glpk"),
               forks = getopt("forks", ""), file = file)
  }))
}

scan_tests <- function(dir = test_dir_default()) {
  files <- list.files(dir, pattern = "^(test|helper)-.*\\.R$", full.names = TRUE)
  tags <- list(); infer <- list()
  for (f in files) {
    lines <- readLines(f, warn = FALSE, encoding = "UTF-8")
    tg <- parse_tags(lines, basename(f))
    if (!is.null(tg)) tags[[f]] <- tg
    infer[[basename(f)]] <- list(txt = lines, depth = file_depth(lines))
  }
  list(tags = if (length(tags)) rbindlist(tags) else NULL, files = infer)
}

# inferred coverage: exact-name mention anywhere, or colName in assignment
# position ($col, col =, "col") for class-sourced parameters
infer_coverage <- function(rows, files) {
  res <- rows[, .(name, kind, colName)]
  res[, `:=`(inf_depth = "none", inf_files = "")]
  # pre-join file text once
  for (fn in names(files)) {
    txt <- files[[fn]]$txt; fd <- files[[fn]]$depth
    blob <- paste(txt, collapse = "\n")
    for (i in seq_len(nrow(res))) {
      nm <- res$name[i]
      hit <- grepl(paste0("\\b", nm, "\\b"), blob)
      if (!hit && res$kind[i] %in% c("numpar", "bounds") && nzchar(res$colName[i])) {
        cn <- res$colName[i]
        # match $col, col =, 'col'/"col" -- skip ultra-generic short names
        if (nchar(cn) >= 3 && !cn %in% c("res", "ava", "dem", "exp", "imp", "cost")) {
          pat <- paste0("(\\$", cn, "\\b|\\b", cn, "\\s*=|[\"']", cn, "[\"'])")
          hit <- grepl(pat, blob)
        }
      }
      if (hit) {
        res$inf_depth[i] <- depth_max(res$inf_depth[i], fd)
        res$inf_files[i] <- paste(c(strsplit(res$inf_files[i], ";")[[1]], fn), collapse = ";")
        res$inf_files[i] <- sub("^;", "", res$inf_files[i])
      }
    }
  }
  res[, .(name, inf_depth, inf_files)]
}

# --------------------------------------------------------------------------- #
# Tag validation (--check)
# --------------------------------------------------------------------------- #

check_tags <- function(dir = test_dir_default(), rows = assemble_rows()) {
  sc <- scan_tests(dir)
  if (is.null(sc$tags)) {
    message("No @covers tags found -- nothing to validate.")
    return(invisible(TRUE))
  }
  known <- unique(c(rows$name, unlist(strsplit(rows$pXxx_expanded, ","))))
  known <- known[nzchar(known)]
  bad_name <- sc$tags[!name %in% known]
  bad_depth <- sc$tags[!depth %in% names(DEPTHS)]
  ok <- TRUE
  if (nrow(bad_name)) {
    ok <- FALSE
    for (i in seq_len(nrow(bad_name)))
      message("UNKNOWN @covers name '", bad_name$name[i], "' in ", bad_name$file[i])
  }
  if (nrow(bad_depth)) {
    ok <- FALSE
    for (i in seq_len(nrow(bad_depth)))
      message("BAD depth '", bad_depth$depth[i], "' in ", bad_depth$file[i])
  }
  if (ok) message("All ", nrow(sc$tags), " @covers tags resolve.")
  invisible(ok)
}

# --------------------------------------------------------------------------- #
# Matrix build + summary
# --------------------------------------------------------------------------- #

build_matrix <- function(dir = test_dir_default()) {
  rows <- assemble_rows()
  sc <- scan_tests(dir)

  inf <- infer_coverage(rows, sc$files)
  rows[inf, `:=`(covered_depth = i.inf_depth, test_files = i.inf_files), on = "name"]
  rows[, `:=`(backends_covered = "", forks_covered = "",
              status = fifelse(covered_depth == "none", "", "inferred"))]

  if (!is.null(sc$tags)) {
    # a tag may name either the .modInp entry or an expanded pXxx bound name
    expmap <- rows[nzchar(pXxx_expanded),
                   .(exp = strsplit(pXxx_expanded, ",")[[1]]), by = name]
    tg <- copy(sc$tags)
    tg[expmap, name := i.name, on = c(name = "exp")]
    agg <- tg[, .(
      depth = names(DEPTHS)[max(DEPTHS[depth]) + 1L],
      backends = paste(sort(unique(unlist(strsplit(backends, ",")))), collapse = ","),
      forks = paste(sort(unique(setdiff(unlist(strsplit(forks, ",")), ""))), collapse = ","),
      files = paste(sort(unique(file)), collapse = ";")
    ), by = name]
    rows[agg, `:=`(covered_depth = i.depth, backends_covered = i.backends,
                   forks_covered = i.forks, test_files = i.files,
                   status = "tagged"), on = "name"]
  }
  setcolorder(rows, c("name", "kind", "family", "dimSets", "defVal",
                      "interpolation_rule", "class", "slot", "colName",
                      "pXxx_expanded", "gates", "covered_depth",
                      "backends_covered", "forks_covered", "test_files", "status"))
  rows[]
}

write_summary <- function(rows, path) {
  lines <- c("# Coverage matrix summary", "",
             paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M"),
                    " | energyRt ", as.character(packageVersion("energyRt"))), "")
  tab <- dcast(rows[, .N, by = .(kind, covered_depth)],
               kind ~ factor(covered_depth, levels = names(DEPTHS)), value.var = "N", fill = 0L)
  lines <- c(lines, "## Rows by kind x depth", "", "```",
             capture.output(print(tab, row.names = FALSE)), "```", "")
  fam <- dcast(rows[kind %in% c("numpar", "bounds"), .N, by = .(family, covered_depth)],
               family ~ factor(covered_depth, levels = names(DEPTHS)), value.var = "N", fill = 0L)
  lines <- c(lines, "## Numeric parameters: family x depth", "", "```",
             capture.output(print(fam[order(family)], row.names = FALSE)), "```", "")
  gaps <- rows[kind %in% c("numpar", "bounds") & covered_depth == "none",
               .(name, family, class, slot, colName)][order(family, name)]
  lines <- c(lines, paste0("## Zero-coverage numeric parameters (", nrow(gaps), ")"), "", "```",
             capture.output(print(gaps, row.names = FALSE, nrows = Inf)), "```", "")
  tagged <- rows[status == "tagged", .N]
  lines <- c(lines,
             paste0("Tagged rows: ", tagged, " | inferred: ",
                    rows[status == "inferred", .N], " | uncovered: ",
                    rows[covered_depth == "none", .N], " of ", nrow(rows)))
  writeLines(lines, path)
}

main <- function(args = commandArgs(trailingOnly = TRUE)) {
  root <- normalizePath(".")
  if (!file.exists(file.path(root, "DESCRIPTION")))
    stop("Run from the package root (DESCRIPTION not found in ", root, ")")
  if ("--check" %in% args) {
    ok <- check_tags()
    quit(status = if (isTRUE(ok)) 0L else 1L)
  }
  out_dir <- file.path("tests", "coverage")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  rows <- build_matrix()
  csv <- file.path(out_dir, "parameter-matrix.csv")
  fwrite(rows, csv)
  write_summary(rows, file.path(out_dir, "matrix-summary.md"))
  message("Wrote ", csv, " (", nrow(rows), " rows) and matrix-summary.md")
}

if (sys.nframe() == 0L) main()
