# Golden-value regression for the tm_* tier models.
#
# The vintage/cluster work reshaped the `technology` class (@start/@end/@olife ->
# @vintage, new @cluster slot), added variant expansion to the interpolation
# pipeline, and touched `.data2slots`. None of that may change the answer for a
# model that uses no vintages and no clusters, so these objectives are pinned.
#
# They were captured from the tier builders BEFORE any of that work landed, and
# verified byte-identical after each step (objectives to 1e-9, plus the
# process_invest_window and pTechOlife tables).
#
# A change here is either a real regression or a deliberate model change -- in
# the latter case update the value in the same commit and say why.

.tm_golden <- c(
  tm_core    = 9921.9914995,
  tm_flows   = 34277.4251473,
  tm_policy  = 33447.4469695,
  tm_weather = 135773.813654
)

test_that("the tier models still produce their known objectives", {
  skip_if_no_fixtures()
  skip_if_no_solver()
  env <- .mapping_fixture_env()

  for (tier in names(.tm_golden)) {
    mod <- env[[tier]]()
    scen <- suppressMessages(suppressWarnings(
      interpolate_model(mod, name = tier, fold = TRUE)))
    sol <- suppressMessages(suppressWarnings(
      solve_scen(scen, solver = solver_options$glpk, wait = TRUE)))
    obj <- suppressMessages(getData(sol, "vObjective", merge = TRUE))
    expect_equal(sum(obj$value), unname(.tm_golden[[tier]]),
                 tolerance = 1e-9, info = tier)
  }
})

test_that("per-region start/end/olife survive the @vintage merge", {
  # The merge is a column-wise union, so a technology with per-region windows and
  # per-region lifetimes must yield exactly the same investment windows and
  # pTechOlife rows as the three separate slots did.
  regs <- c("R1", "R2")
  # NB distinct names: vt_model() already supplies an "ECOA" backstop.
  EPRA <- newTechnology(
    "EPRA", input = list(comm = "COA"), output = list(comm = "ELC"),
    invcost = data.frame(region = regs, invcost = c(1000, 1100)),
    start = data.frame(region = regs, start = c(2020L, 2030L)),
    end   = data.frame(region = regs, end   = c(2040L, 2050L)),
    olife = data.frame(region = regs, olife = c(30L, 20L)),
    cap2act = 1)
  # NB olife is given per region on BOTH technologies on purpose: mixing a
  # per-region and a region-agnostic olife makes the GLPK writer emit a literal
  # NA (pre-existing bug, unrelated to variants).
  EPRB <- newTechnology(
    "EPRB", input = list(comm = "COA"), output = list(comm = "ELC"),
    invcost = data.frame(region = regs, invcost = c(1500, 1500)),
    start = 2020L, end = 2050L,                 # global window, broadcast
    olife = data.frame(region = regs, olife = c(25L, 25L)),
    cap2act = 1)

  scen <- vt_interp(vt_model(EPRA, EPRB, name = "ls"), "ls")

  win <- as.data.frame(scen@modInp@sets$process_invest_window)
  win <- win[win$process %in% c("EPRA", "EPRB"), ]
  win <- win[order(win$process, win$region), ]
  # EPRA keeps its per-region window; EPRB's global one broadcasts to both
  expect_equal(win$start, c(2020L, 2030L, 2020L, 2020L))
  expect_equal(win$end,   c(2040L, 2050L, 2050L, 2050L))

  ol <- as.data.frame(scen@modInp@parameters$pTechOlife@data)
  ol <- ol[ol$tech %in% c("EPRA", "EPRB"), ]
  ol <- ol[order(ol$tech, ol$region), ]
  expect_equal(ol$value, c(30, 20, 25, 25))
})

test_that("every model parameter is declared in the GLPK and GAMS templates", {
  # The writers emit EVERY parameter in `modInp` unconditionally -- an empty one
  # still writes `param pX default 0 := ;`. A parameter registered in
  # modInp.yml but not declared in a template is therefore not a missing feature
  # but a hard solver error on the next solve, in a model that never touches it.
  # Cheap to check statically, and it is the failure mode adding a parameter
  # invites.
  mi <- getFromNamespace(".modInp", "energyRt")
  mc <- getFromNamespace(".modelCode", "energyRt")
  # `bounds` are declared under their .Lo/.Up names, not the base name.
  pars <- names(mi)[vapply(mi, function(p) !identical(p$type, "bounds"),
                           logical(1))]
  for (eng in c("GLPK", "GAMS")) {
    code <- paste(mc[[eng]], collapse = "\n")
    missed <- pars[!vapply(pars, function(n) {
      grepl(paste0("(^|[^[:alnum:]_])", n, "([^[:alnum:]_]|$)"), code)
    }, logical(1))]
    expect_equal(missed, character(), info = eng)
  }
})

test_that("every engine emits a parameter's full row count", {
  skip_if_no_solver()
  # The GAMS / Pyomo / Julia emitters used to truncate `@data` to a cached
  # `@misc$nValues` row count while GLPK never did, so a stale count made those
  # three engines write LESS data than GLPK for the same scenario -- silently,
  # and only for the engines the test suite cannot run. The cache is gone; this
  # pins the four emitters to the same row count.
  sc <- vt_interp(vt_model(name = "wr"), "wr")

  gams0 <- getFromNamespace(".toGams0", "energyRt")   # .toGams is a local closure
  emit <- list(
    GAMS   = function(p) gams0(p, FALSE),
    Pyomo  = getFromNamespace(".toPyomo", "energyRt"),
    Julia  = getFromNamespace(".toJuliaHead", "energyRt"),
    GLPK   = getFromNamespace(".sm_to_glpk", "energyRt")
  )
  # A GAMS map is "set", "<name>( dims ) /", <one line per row>, "/;", "".
  gams_records <- function(x) {
    sum(!grepl("^(set|/;|)$", x) & !grepl("[(].*[)] /$", x))
  }

  checked <- 0L
  for (nm in names(sc@modInp@parameters)) {
    p <- sc@modInp@parameters[[nm]]
    n <- nrow(getFromNamespace("get_data_slot", "energyRt")(p))
    if (n == 0) next
    checked <- checked + 1L
    for (eng in names(emit)) {
      expect_true(nzchar(paste(emit[[eng]](p), collapse = "")),
                  info = paste(eng, nm))
    }
    if (p@type == "map") {
      expect_equal(gams_records(emit$GAMS(p)), n, info = paste("GAMS", nm))
      expect_equal(sum(grepl(", *$", emit$GLPK(p))), n, info = paste("GLPK", nm))
    }
  }
  expect_gt(checked, 20L)
})
