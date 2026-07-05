## Python/Pyomo
Pyomo <- list(
  name = "pyomo",
  lang = "PYOMO",
  export_format = "SQLite",
  # solver = "cplex"
  # solver = "glpk"
  solver = "cbc"
)

pyomo_cbc <- Pyomo
pyomo_cbc$name <- "pyomo_cbc"


pyomo_cplex <- Pyomo
pyomo_cplex$name <- "pyomo_cplex"
pyomo_cplex$solver <- "cplex"

pyomo_cplex_barrier <- pyomo_cplex
pyomo_cplex_barrier$name <- "pyomo_cplex_barrier"

pyomo_cbc <- Pyomo; pyomo_cbc$name <- "pyomo_cbc"

pyomo_cplex <- Pyomo
pyomo_cplex$solver <- "cplex"; pyomo_cplex$name <- "pyomo_cplex"

pyomo_cplex_barrier <- pyomo_cplex;
pyomo_cplex_barrier$name <- "pyomo_cplex_barrier"
pyomo_cplex_barrier$inc4 <- {
"opt.options['lpmethod'] = 4
opt.options['solutiontype'] = 2"}

pyomo_glpk <- Pyomo
pyomo_glpk$name <- "pyomo_glpk"
pyomo_glpk$solver <- "glpk"

## Python/Pyomo via NEOS (remote solve; no local solver, needs env var NEOS_EMAIL)
# The scenario is still BUILT locally (Pyomo reads the data into the
# ConcreteModel); only the SOLVE is dispatched to NEOS, which serialises the
# model to NL and runs the chosen commercial solver. Implemented as an
# `inc_solver` override: a tiny shim makes `opt` a duck-typed object whose
# `.solve(model, tee=...)` calls SolverManagerFactory('neos'), so the baked
# `opt.solve(model, tee=True)` in the template works unchanged and the solution
# is loaded back for the usual output extraction. `solver` must be unset (the
# writer forbids both `solver` and `inc_solver`).
# `options` is a named list of CPLEX/solver option -> value passed to the remote
# solver. With no options we keep the simple string form (opt='<solver>') that
# needs no local solver plugin; with options we build a SolverFactory object and
# set its .options (Pyomo's documented way to pass solver settings via NEOS).
.pyomo_neos_inc_solver <- function(neos_solver = "cplex", options = list()) {
  head <- c(
    "import os as _os",
    "from pyomo.opt import SolverManagerFactory as _SMF",
    "if not _os.environ.get('NEOS_EMAIL'):",
    "    raise RuntimeError('Set the NEOS_EMAIL environment variable to use the pyomo NEOS backend.')")
  if (!length(options)) {
    body <- c(
      "class _NeosOpt:",
      "    def __init__(self, solver):",
      "        self._mgr = _SMF('neos'); self._solver = solver; self.options = {}",
      "    def solve(self, model, **kw):",
      "        return self._mgr.solve(model, opt=self._solver)",
      sprintf("opt = _NeosOpt('%s')", neos_solver))
  } else {
    opts_py <- paste0("{", paste(sprintf("'%s': %s", names(options),
      unlist(options)), collapse = ", "), "}")
    body <- c(
      "from pyomo.environ import SolverFactory as _SF",
      "class _NeosOpt:",
      "    def __init__(self, solver, options):",
      "        self._mgr = _SMF('neos'); self._opt = _SF(solver)",
      "        self.options = self._opt.options",
      "        for _k, _v in options.items(): self._opt.options[_k] = _v",
      "    def solve(self, model, **kw):",
      "        return self._mgr.solve(model, opt=self._opt)",
      sprintf("opt = _NeosOpt('%s', %s)", neos_solver, opts_py))
  }
  paste(c(head, body), collapse = "\n")
}

neos_pyomo_cplex <- Pyomo
neos_pyomo_cplex$name <- "neos_pyomo_cplex"
neos_pyomo_cplex$solver <- NULL
neos_pyomo_cplex$inc_solver <- .pyomo_neos_inc_solver("cplex")

neos_pyomo_cbc <- Pyomo
neos_pyomo_cbc$name <- "neos_pyomo_cbc"
neos_pyomo_cbc$solver <- NULL
neos_pyomo_cbc$inc_solver <- .pyomo_neos_inc_solver("cbc")

# CPLEX barrier (lpmethod 4) without crossover (solutiontype 2)
neos_pyomo_cplex_barrier <- Pyomo
neos_pyomo_cplex_barrier$name <- "neos_pyomo_cplex_barrier"
neos_pyomo_cplex_barrier$solver <- NULL
neos_pyomo_cplex_barrier$inc_solver <- .pyomo_neos_inc_solver(
  "cplex", list(lpmethod = 4, solutiontype = 2))


## Julia/JuMP ####
julia_cbc <- list(
  name = "julia_cbc",
  lang = "JuMP",
  solver = "Cbc"
)

julia_glpk <- list(
  name = "julia_glpk",
  lang = "JuMP",
  solver = "GLPK"
)

julia_cplex <- list(
  name = "julia_cplex",
  lang = "JuMP",
  solver = "CPLEX"
)

julia_cplex_barrier <- julia_cplex
julia_cplex_barrier$name <- "julia_cplex_barrier"
julia_cplex_barrier$inc3 <- {'
set_optimizer_attribute(model, "CPXPARAM_LPMethod", 4) # barrier CPX_ALG_BARRIER
set_optimizer_attribute(model, "CPXPARAM_SolutionType", 2) # CPX_NONBASIC_SOLN'}

julia_highs <- list(
  name = "julia_highs",
  lang = "JuMP",
  solver = "HiGHS"
)


julia_highs_barrier <- julia_highs
julia_highs_barrier$name <- "julia_highs_barrier"
julia_highs_barrier$inc3 <- c({
'# HiGHS options in JuMP/Julia
# Uncomment options to use
set_optimizer_attribute(model, "presolve", "on")
# set_attribute(model, "time_limit", 3600.0)

# "Barrier" method
set_optimizer_attribute(model, "solver", "ipm") # barrier "Interior Point Method"
# set_optimizer_attribute(model, "ipm_optimality_tolerance", 1e-5) #
set_optimizer_attribute(model, "run_crossover", "off") # polishing the solution
'
})

julia_highs_simplex <- julia_highs
julia_highs_simplex$name <- "julia_highs_simplex"
julia_highs_simplex$inc3 <- c({
'# HiGHS options in JuMP/Julia
# Uncomment options to use
set_optimizer_attribute(model, "presolve", "on")
# set_attribute(model, "time_limit", 3600.0)

# Simplex
set_attribute(model, "solver", "simplex")
#set_attribute(model, "simplex_strategy", "on")
'})

julia_highs_parallel <- julia_highs
julia_highs_parallel$name <- "julia_highs_parallel"
julia_highs_parallel$inc3 <- c({
'# HiGHS options in JuMP/Julia
# Uncomment options to use
set_optimizer_attribute(model, "presolve", "on")
# set_attribute(model, "time_limit", 3600.0)

# Parallel Dual simplex
set_attribute(model, "solver", "choose")
set_attribute(model, "parallel", "on")
set_attribute(model, "threads", 8)
set_attribute(model, "simplex_max_concurrency", 8)
'})


## GLPK
glpk <- list(name = "glpk", lang = "GLPK")

## GAMS
# gams_path <- options::opt("gams_path")
# gams_cmd_line <- file.path(gams_path, "gams.exe energyRt.gms")

gams_cplex <- list(
  name = "gams_cplex",
  lang = "GAMS",
  solver = "CPLEX"
)

gams_gdx_cplex <- list(
  name = "gams_gdx_cplex",
  lang = "GAMS",
  import_format = "GDX",
  export_format = "GDX",
  solver = "CPLEX"
)

gams_gdx_cplex_barrier <- gams_gdx_cplex
gams_gdx_cplex_barrier$name <- "gams_gdx_cplex_barrier"
gams_gdx_cplex_barrier$inc3 <- {"
*energyRt.holdfixed = 1;
*energyRt.dictfile = 0;
*option solvelink = 5;
*option InteractiveSolver = 1;
option iterlim = 1e9;
option reslim = 1e7;
option threads = 0;
*option LP = CPLEX;
energyRt.OptFile = 1;
*option savepoint = 1;
*option bRatio = 0;
*execute_loadpoint 'energyRt_p';

$onecho > cplex.opt
*interactive 1
*advind 0
* predual 1
* BarStartAlg 4
* tuningtilim 2400
*aggcutlim 3
*aggfill 10
*aggind 25

parallelmode -1
threads -1
lpmethod 4
*reinv 1e4

*preind: turn presolver on/off (1/0)
*preind 0
*scaind 1
*scaind -1
*predual -1
solutiontype 2

*printoptions 1
names no
freegamsmodel 1
*memoryemphasis 1

*barcolnz 5
*numericalemphasis 1
*barepcomp 1e-5
*barstartalg 2
*predual 1
*baralg 1

*epopt 1e-1
*eprhs 1e-1
*dpriind 2
*ppriind 3
*perind 1
*epmrk 0.1

*tuningdisplay 2
*simdisplay 2
*bardisplay 2

*CraInd 0

$offecho
"}


gams_gdx_cplex_parallel <- gams_gdx_cplex
gams_gdx_cplex_parallel$name <- "gams_gdx_cplex_parallel"
gams_gdx_cplex_parallel$inc3 <- {
"
*energyRt.holdfixed = 1;
*energyRt.dictfile = 0;
option solvelink = 0;
*option InteractiveSolver = 1;
option iterlim = 1e9;
option reslim = 1e7;
*option threads = 0;
*option solvelink = 5;
*option LP = CPLEX;
energyRt.OptFile = 1;
*option savepoint = 1;
*option bRatio = 0;
*execute_loadpoint 'energyRt_p';
$onecho > cplex.opt
*interactive 1
* advind 0
* predual 1
* BarStartAlg 4
* tuningtilim 2400
*aggcutlim 3
*aggfill 10
*aggind 25
*bardisplay 2
parallelmode -1
lpmethod 6
*printoptions 1
*names no
*freegamsmodel 1
*memoryemphasis 1
threads -1
*barepcomp 1e-5
*scaind 1
*predual -1
*solutiontype 2

*epopt 1e-1
*eprhs 1e-1
*barepcomp 1e-5
*epmrk 0.1

$offecho
*$exit
"} # GAMS options ####

gams_cbc <- list(
  name = "gams_cbc",
  lang = "GAMS",
  solver = "CBC"
)

## GAMS via NEOS (remote solve; text data, no local GAMS/gdx; needs NEOS_EMAIL)
# backend = "neos" makes .call_solver submit the written GAMS model to NEOS and
# fetch results instead of running gams locally. Data is sent as inlined TEXT
# (no gdx), so no local GAMS install is required. neos_solver/neos_category pick
# the remote solver and NEOS problem category.
neos_gams_cplex <- gams_cplex
neos_gams_cplex$name <- "neos_gams_cplex"
neos_gams_cplex$backend <- "neos"
neos_gams_cplex$neos_solver <- "CPLEX"
neos_gams_cplex$neos_category <- "milp"

neos_gams_cbc <- gams_cbc
neos_gams_cbc$name <- "neos_gams_cbc"
neos_gams_cbc$backend <- "neos"
neos_gams_cbc$neos_solver <- "CBC"
neos_gams_cbc$neos_category <- "milp"

# CPLEX barrier: inc3 becomes inc3.gms (included by energyRt.gms), writing a
# cplex.opt with lpmethod 4 (barrier) + solutiontype 2 (no crossover). The block
# travels with the inlined model to NEOS.
neos_gams_cplex_barrier <- neos_gams_cplex
neos_gams_cplex_barrier$name <- "neos_gams_cplex_barrier"
neos_gams_cplex_barrier$inc3 <- paste(
  "energyRt.OptFile = 1;",
  "$onecho > cplex.opt",
  "lpmethod 4",
  "solutiontype 2",
  "$offecho",
  sep = "\n")

gams_gdx_cbc <- list(
  name = "gams_gdx_cbc",
  lang = "GAMS",
  import_format = "GDX",
  export_format = "GDX",
  solver = "CBC"
)

# Arrow IPC/feather exchange variants: model data AND solution exchanged as
# Arrow IPC (feather, zstd) instead of SQLite/RData (input) and CSV (output).
pyomo_cbc_arrow <- pyomo_cbc
pyomo_cbc_arrow$name <- "pyomo_cbc_arrow"
pyomo_cbc_arrow$export_format <- "feather"
pyomo_cbc_arrow$import_format <- "feather"

julia_highs_arrow <- julia_highs
julia_highs_arrow$name <- "julia_highs_arrow"
julia_highs_arrow$export_format <- "feather"
julia_highs_arrow$import_format <- "feather"

solver_options <- list(
  # GLPK
  glpk = glpk,
  # Python/Pyomo
  pyomo_cbc = pyomo_cbc,
  pyomo_cbc_arrow = pyomo_cbc_arrow,
  pyomo_cplex = pyomo_cplex,
  pyomo_cplex_barrier = pyomo_cplex_barrier,
  pyomo_glpk = pyomo_glpk,
  # Python/Pyomo via NEOS (remote solve)
  neos_pyomo_cplex = neos_pyomo_cplex,
  neos_pyomo_cplex_barrier = neos_pyomo_cplex_barrier,
  neos_pyomo_cbc = neos_pyomo_cbc,
  # julia
  julia_cbc = julia_cbc,
  julia_cplex = julia_cplex,
  julia_cplex_barrier = julia_cplex_barrier,
  julia_highs = julia_highs,
  julia_highs_arrow = julia_highs_arrow,
  julia_highs_barrier = julia_highs_barrier,
  julia_glpk = julia_glpk,
  julia_highs_simplex = julia_highs_simplex,
  julia_highs_parallel = julia_highs_parallel,
  # GAMS
  gams_csv_cplex = gams_cplex,
  gams_gdx_cplex = gams_gdx_cplex,
  gams_gdx_cplex_barrier = gams_gdx_cplex_barrier,
  gams_gdx_cplex_parallel = gams_gdx_cplex_parallel,
  gams_csv_cbc = gams_cbc,
  gams_gdx_cbc = gams_gdx_cbc,
  # GAMS via NEOS (remote)
  neos_gams_cplex = neos_gams_cplex,
  neos_gams_cplex_barrier = neos_gams_cplex_barrier,
  neos_gams_cbc = neos_gams_cbc
)

usethis::use_data(solver_options, overwrite = TRUE)

## Solver options - DRAFT
# solver_options <- function(
#     lang = "Pyomo",
#     export_format = "SQLite",
#     solver = "cbc",
#     algorithm = NULL,
#     inc1 = NULL,
#     inc2 = NULL,
#     inc3 = NULL,
#     inc4 = NULL,
#     inc5 = NULL
#   ) {
#
#   options <- list(
#     lang = lang,
#     export_format = export_format,
#     solver = solver,
#     inc1 = inc1,
#     inc2 = inc2,
#     inc3 = inc3,
#     inc4 = inc4,
#     inc5 = inc5
#   )
#
#   if (lang == "Pyomo") {
#     # Pyomo
#   } else if (lang == "Julia") {
#     # Julia
#   } else if (lang == "GLPK") {
#     # GLPK
#   } else if (lang == "GAMS") {
#     # GAMS
#   } else {
#     stop("Unknown language")
#   }
# }
