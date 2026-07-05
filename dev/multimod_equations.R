# Generate the energyRt model-equations document via multimod
# =============================================================================
# Pipeline:  energyRt model  --write_gams-->  self-contained GAMS instance
#            --multimod::read_gams--> AST  --write_latex--> equations .tex (-> PDF)
#
# The two halves run in ISOLATED R sessions (callr) because energyRt and multimod
# both export symbols like `write_gams`/`set_gams_path` and would mask each other
# if load_all'd together.
#
# Requirements: local clones of energyRt (d:/RProjects/energyRt) and multimod
# (d:/RProjects/multimod); a LaTeX engine (tinytex) only for the optional PDF.
#
# Usage:  Rscript dev/multimod_equations.R
# Result: <out>/energyRt_equations.tex  (+ .pdf if a LaTeX engine is present)
# =============================================================================

energyrt_dir <- "d:/RProjects/energyRt"
multimod_dir <- "d:/RProjects/multimod"
model_rds    <- file.path(energyrt_dir, "tmp/new_interp/solved_scenario.rds") # any saved scenario/model
out_dir      <- file.path(energyrt_dir, "dev/multimod-out")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ---- Phase A: energyRt writes a self-contained GAMS instance -----------------
gms <- callr::r(function(energyrt_dir, model_rds, out_dir) {
  suppressMessages(devtools::load_all(energyrt_dir, quiet = TRUE))
  run <- file.path(out_dir, "gams_run")
  unlink(run, recursive = TRUE); dir.create(run, recursive = TRUE, showWarnings = FALSE)

  mod   <- readRDS(model_rds)@model            # swap in any energyRt model here
  scen0 <- interpolate(mod, sparse = FALSE)
  # A CSV-format GAMS target writes all files without needing gdxtools or a
  # working solver; the solve step may fail (no license) AFTER files are written.
  try(solve(scen0, solver = solver_options$gams_csv_cplex,
            tmp.dir = run, tmp.del = FALSE), silent = TRUE)
  list.files(run, pattern = "energyRt.gms$", recursive = TRUE, full.names = TRUE)[1]
}, args = list(energyrt_dir, model_rds, out_dir))

cat("GAMS instance:", gms, "\n")
stopifnot(!is.na(gms), file.exists(gms))

# ---- Phase B: multimod parses the GAMS and renders LaTeX ---------------------
res <- callr::r(function(multimod_dir, gms, out_dir) {
  suppressMessages(devtools::load_all(multimod_dir, quiet = TRUE))
  ms <- read_gams(gms, include = TRUE, verbose = FALSE)
  ms <- tryCatch(en_extract_domains_from_comments(ms, verbose = FALSE), error = function(e) ms)
  am <- as_multimod(ms)

  tex <- file.path(out_dir, "energyRt_equations.tex")
  write_latex(am, tex)

  ast_names <- unique(unlist(lapply(am$equations, function(e) e$name)))
  ln  <- readLines(gms)
  defnames <- unique(sub("^(eq[A-Za-z0-9_]+).*$", "\\1",
                grep("^eq[A-Za-z0-9_]+.*[.][.]", ln, value = TRUE)))
  list(tex = tex, n_ast = length(ast_names),
       n_gams = length(defnames), missing = setdiff(defnames, ast_names))
}, args = list(multimod_dir, gms, out_dir))

cat(sprintf("Equations: %d parsed / %d in GAMS\n", res$n_ast, res$n_gams))
if (length(res$missing)) cat("Not captured by multimod:", paste(res$missing, collapse = ", "), "\n")
cat("LaTeX written:", res$tex, "\n")

# ---- Optional: compile to PDF (needs a LaTeX engine, e.g. tinytex) -----------
if (requireNamespace("tinytex", quietly = TRUE) && tinytex::is_tinytex()) {
  pdf <- tryCatch(tinytex::latexmk(res$tex), error = function(e) NA)
  if (!is.na(pdf)) cat("PDF:", pdf, "\n")
} else {
  cat("No LaTeX engine found; install with tinytex::install_tinytex() to build the PDF.\n")
}
