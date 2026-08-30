# =========================================================================== #
# Kirchhoff's voltage law on the AC network.
#
# energyRt's `trade` is a TRANSPORT model: the optimiser CHOOSES the flow on a
# route. That is right for a DC link, a pipeline or a shipping lane, and wrong
# for an AC line, whose flow is set by impedance -- the network divides a
# transfer between parallel paths whether the optimiser likes the split or not.
#
# `interpolate_model(..., kvl = TRUE)` adds the missing physics as one equality
# per independent cycle of the network. The tests below are of three kinds:
#
#   * the split is the TEXTBOOK one -- inverse to path reactance;
#   * KVL is a RESTRICTION, so the objective can only rise;
#   * KVL is VACUOUS on a radial network, which is the sharpest available check
#     that the cycle basis is right (a wrong basis would invent a constraint).
# =========================================================================== #

# One-year, one-timeslice networks so the arithmetic is readable. Supply is
# cheap at R1 and prohibitive elsewhere; demand is 10 at R3. A small `varom` on
# the two-hop legs makes the TRANSPORT optimum strictly the direct line, so the
# two answers cannot coincide by accident.
kvl_line <- function(n, a, b, x, vom = 0) newACLine(
  n, "ELC", a, b, reactance = x, cap2act = 1,
  capacity = data.frame(cap.fx = 100), vintage = data.frame(olife = 50L),
  varom = if (vom) data.frame(varom = vom) else data.frame())

kvl_net <- function(name, lines) newModel(
  name = name, region = c("R1", "R2", "R3"), horizon = tr_hor(),
  discount = 0, calendar = tr_cal(),
  repo = do.call(newRepository, c(list("kvl_repo",
    newCommodity("ELC", timeframe = "ANNUAL"),
    newSupply("SUP", commodity = "ELC",
              supply = data.frame(region = c("R1", "R2", "R3"),
                                  cost = c(1, 1000, 1000))),
    newDemand("DEM", commodity = "ELC",
              demand = data.frame(region = "R3", demand = 10))),
    lines)))

# Triangle: two paths from R1 to R3 -- direct (x = 0.4) and via R2 (0.1 + 0.2).
KVL_TRIANGLE <- function() list(
  kvl_line("L12", "R1", "R2", 0.1, 0.01),
  kvl_line("L23", "R2", "R3", 0.2, 0.01),
  kvl_line("L13", "R1", "R3", 0.4))

# The same lines minus the closing one: a tree, so no cycles.
KVL_RADIAL <- function() list(
  kvl_line("L12", "R1", "R2", 0.1, 0.01),
  kvl_line("L23", "R2", "R3", 0.2, 0.01))

kvl_scen <- function(name, lines, kvl) suppressMessages(
  interpolate_model(kvl_net(name, lines), name = name, kvl = kvl))

kvl_obj <- function(s, ...) getData(
  suppressMessages(solve_scenario(s, ...)), "vObjective", merge = TRUE)$value[1]

kvl_flows <- function(s) {
  d <- as.data.frame(getData(suppressMessages(solve_scenario(s)), "vTradeIr",
                             merge = TRUE))
  d <- d[d$value > 1e-9, ]
  setNames(round(d$value, 4), paste0(d$src, "->", d$dst))
}

# --------------------------------------------------------------------------- #
# The cycle basis itself, with no solver involved
# --------------------------------------------------------------------------- #

test_that("the cycle basis has E - N + C members", {
  bas <- function(...) energyRt:::.kvl_cycle_basis(
    energyRt:::.kvl_lines(list(...)))
  ln <- function(n, a, b, x = 0.1) newACLine(n, "ELC", a, b, reactance = x)

  # A tree has none. This is the check that matters most: a basis that invented
  # a cycle here would constrain a network that has no loop to constrain.
  expect_equal(nrow(bas(ln("A", "R1", "R2"), ln("B", "R2", "R3"))), 0)

  # 3 lines over 3 nodes: 3 - 3 + 1 = 1 cycle, of three lines.
  tri <- bas(ln("A", "R1", "R2"), ln("B", "R2", "R3"), ln("C", "R1", "R3"))
  expect_equal(length(unique(tri$cycle)), 1)
  expect_equal(nrow(tri), 3)
  expect_setequal(tri$sign, c(-1, 1))   # a cycle traverses lines both ways

  # 5 lines over 4 nodes: 5 - 4 + 1 = 2.
  sq <- bas(ln("A", "R1", "R2"), ln("B", "R2", "R3"), ln("C", "R3", "R4"),
            ln("D", "R4", "R1"), ln("E", "R1", "R3"))
  expect_equal(length(unique(sq$cycle)), 2)

  # Two disconnected triangles: 6 - 6 + 2 = 2, one per component.
  two <- bas(ln("A", "R1", "R2"), ln("B", "R2", "R3"), ln("C", "R1", "R3"),
             ln("D", "R4", "R5"), ln("E", "R5", "R6"), ln("F", "R4", "R6"))
  expect_equal(length(unique(two$cycle)), 2)

  # A DC link carries no reactance and is never in the network.
  expect_equal(nrow(energyRt:::.kvl_lines(list(newDCLink("D", "ELC", "R1", "R2")))), 0)
})

test_that("an AC line that cannot carry the law is refused, not repaired", {
  # Each of these has an "obvious" fix -- take the mean, assume the reverse --
  # that would change the physics without saying so.
  expect_error(newACLine("A", "ELC", "R1", "R2", reactance = 0),
               "strictly positive")
  expect_error(newACLine("A", "ELC", "R1", "R2"), "`reactance` is required")
  expect_error(newACLine("A", "ELC", "R1", "R1", reactance = 0.1),
               "endpoints must differ")

  # KVL constrains the NET flow, so a one-way AC line is not expressible.
  oneway <- newTrade("A", commodity = "ELC",
                     routes = data.frame(src = "R1", dst = "R2"),
                     trade = data.frame(src = "R1", dst = "R2", reactance = 0.1))
  expect_error(energyRt:::.kvl_lines(list(oneway)), "one direction only")

  # A reactance is a property of a line, not of a direction.
  asym <- newTrade("A", commodity = "ELC",
                   routes = data.frame(src = c("R1", "R2"), dst = c("R2", "R1")),
                   trade = data.frame(src = c("R1", "R2"), dst = c("R2", "R1"),
                                      reactance = c(0.1, 0.2)))
  expect_error(energyRt:::.kvl_lines(list(asym)), "different reactances")

  # Parallel circuits must be merged with 1/x_eq = sum(1/x_i); two objects on
  # one corridor would each be constrained as if it were the whole line.
  expect_error(
    energyRt:::.kvl_lines(list(newACLine("A", "ELC", "R1", "R2", reactance = 0.1),
                               newACLine("B", "ELC", "R2", "R1", reactance = 0.2))),
    "more than one trade object")
})

# --------------------------------------------------------------------------- #
# The physics
# --------------------------------------------------------------------------- #

test_that("KVL splits the flow inversely to path reactance", {
  skip_if_no_solver()
  # Without it the optimiser ships everything the cheap way: the direct line has
  # no varom, the two-hop path has 0.01 on each leg.
  plain <- kvl_scen("kv1", KVL_TRIANGLE(), FALSE)
  expect_equal(kvl_obj(plain), 10)
  expect_equal(kvl_flows(plain), c("R1->R3" = 10))

  # With it the flow is not chosen at all. Path reactances are 0.4 (direct) and
  # 0.3 (via R2), and DC power flow splits INVERSELY to them:
  #   f_direct = 10 * 0.3/0.7,  f_via = 10 * 0.4/0.7
  on <- kvl_scen("kv2", KVL_TRIANGLE(), TRUE)
  f <- kvl_flows(on)
  expect_equal(unname(f["R1->R3"]), round(10 * 0.3 / 0.7, 4))
  expect_equal(unname(f["R1->R2"]), round(10 * 0.4 / 0.7, 4))
  # Whatever enters R2 leaves it: KCL is the commodity balance, already there.
  expect_equal(unname(f["R2->R3"]), unname(f["R1->R2"]))

  # The extra cost is exactly the wheeled flow times the two legs' varom.
  expect_equal(kvl_obj(on), 10 + (10 * 0.4 / 0.7) * 0.02)
})

test_that("a network with two cycles is constrained by both", {
  skip_if_no_solver()
  # Square R1-R2-R3-R4 plus the R1-R3 diagonal: 5 lines over 4 nodes, so
  # 5 - 4 + 1 = 2 independent cycles. All reactances equal, which makes the
  # answer a pure topology result: three paths from R1 to R3 with impedances
  # 0.1 (direct), 0.2 (via R2) and 0.2 (via R4), so conductances 10 : 5 : 5 and
  # an injection of 12 splits 6 : 3 : 3.
  #
  # This case also covers attaching MORE THAN ONE cycle, which one `add()` per
  # constraint could not do -- `add(model, <constraint>)` parks the object in an
  # unnamed repository and the second call fails on it.
  sq <- function(nm) newModel(
    name = nm, region = paste0("R", 1:4), horizon = tr_hor(), discount = 0,
    calendar = tr_cal(),
    repo = newRepository("sq",
      newCommodity("ELC", timeframe = "ANNUAL"),
      newSupply("SUP", commodity = "ELC",
                supply = data.frame(region = paste0("R", 1:4),
                                    cost = c(1, 1000, 1000, 1000))),
      kvl_line("A", "R1", "R2", 0.1), kvl_line("B", "R2", "R3", 0.1),
      kvl_line("C", "R3", "R4", 0.1), kvl_line("D", "R4", "R1", 0.1),
      kvl_line("E", "R1", "R3", 0.1),
      newDemand("DEM", commodity = "ELC",
                demand = data.frame(region = "R3", demand = 12))))

  scen <- suppressMessages(interpolate_model(sq("kvsq"), name = "kvsq", kvl = TRUE))
  expect_equal(length(energyRt:::.kvl_scenario_cycles(scen)), 2)

  d <- as.data.frame(getData(suppressMessages(solve_scenario(scen)), "vTradeIr",
                             merge = TRUE))
  d <- d[d$value > 1e-9, ]
  f <- setNames(round(d$value, 6), paste0(d$src, "->", d$dst))
  expect_equal(unname(f["R1->R3"]), 6)
  expect_equal(unname(f["R1->R2"]), 3)
  expect_equal(unname(f["R1->R4"]), 3)
  # ... and what enters each intermediate region leaves it.
  expect_equal(unname(f["R2->R3"]), unname(f["R1->R2"]))
  expect_equal(unname(f["R4->R3"]), unname(f["R1->R4"]))
})

test_that("KVL is a restriction, so the objective can only rise", {
  skip_if_no_solver()
  expect_gt(kvl_obj(kvl_scen("kv3", KVL_TRIANGLE(), TRUE)),
            kvl_obj(kvl_scen("kv4", KVL_TRIANGLE(), FALSE)))
})

test_that("KVL is vacuous on a radial network", {
  skip_if_no_solver()
  # No cycles means no constraints, so the two answers must be IDENTICAL -- not
  # merely close. A cycle basis that invented a member here would show up as a
  # difference, which makes this the sharpest check available.
  expect_equal(kvl_obj(kvl_scen("kv5", KVL_RADIAL(), TRUE)),
               kvl_obj(kvl_scen("kv6", KVL_RADIAL(), FALSE)))
  expect_equal(length(energyRt:::.kvl_scenario_cycles(
    kvl_scen("kv7", KVL_RADIAL(), TRUE))), 0)
})

test_that("a model with no reactance is untouched by the flag", {
  skip_if_no_solver()
  # `kvl = TRUE` on a model whose trade objects are plain routes is a no-op, so
  # the flag is safe to leave on in a script that also builds non-electrical
  # networks.
  expect_equal(tr_obj(tr_mesh_one(name = "kv8")), 10)
  expect_equal(getData(suppressMessages(solve_scenario(suppressMessages(
    interpolate_model(tr_mesh_one(name = "kv9"), name = "kv9", kvl = TRUE)))),
    "vObjective", merge = TRUE)$value[1], 10)
})

# --------------------------------------------------------------------------- #
# The solve-time guard
# --------------------------------------------------------------------------- #

test_that("a solve cannot silently differ from what was asked for", {
  skip_if_no_solver()
  # The constraints are baked in at interpolation time, so `kvl` on solve_scenario()
  # can only check. Both mismatches are errors rather than warnings: each would
  # otherwise return a perfectly plausible number for the other model.
  expect_error(kvl_obj(kvl_scen("kv10", KVL_TRIANGLE(), FALSE), kvl = TRUE),
               "carries no KVL constraints")
  expect_error(kvl_obj(kvl_scen("kv11", KVL_TRIANGLE(), TRUE), kvl = FALSE),
               "WAS built with KVL")
  # And the matching combination goes through, returning the KVL answer rather
  # than the transport one.
  expect_equal(kvl_obj(kvl_scen("kv12", KVL_TRIANGLE(), TRUE), kvl = TRUE),
               10 + (10 * 0.4 / 0.7) * 0.02)
})

test_that("the back-ends agree on a KVL solve", {
  skip_if_no_solver()
  # The cycle constraints are ordinary user constraints, so this also exercises
  # the two-endpoint `for.sum` and the scalar `mult` on every emitter. GAMS
  # needs a dense re-interpolation and a NEOS round trip; it is checked in the
  # manual cross-back-end sweep, where it agrees to eight decimals.
  scen <- kvl_scen("kv13", KVL_TRIANGLE(), TRUE)
  got <- vapply(c("glpk", "julia_highs", "pyomo_glpk"), function(e) {
    r <- try(suppressMessages(solve_scenario(scen, solver = solver_options[[e]])),
             silent = TRUE)
    if (inherits(r, "try-error")) NA_real_
    else getData(r, "vObjective", merge = TRUE)$value[1]
  }, numeric(1))
  expect_equal(unname(got), rep(10 + (10 * 0.4 / 0.7) * 0.02, 3))
})

# =========================================================================== #
# KVL and loss tranches
#
# The cycle basis is a property of the PHYSICAL graph: `E - N + C` counts LINES,
# not the parallel parts a line is modelled with. So splitting a line into loss
# tranches -- or into build vintages -- must leave the basis untouched, and each
# cycle must constrain the sum of the parts, all with the whole line's reactance.
# =========================================================================== #

# The triangle again, with every line split into `n` equal tranches. `teff` is
# held equal ACROSS tranches so the split cannot confound the line-level answer.
kvl_tranched <- function(name, n = 2) {
  tt <- lossTranches(rep(1 / n, n), loss = 1e-9, fix = 100)
  L <- function(nm, a, b, x, vom = 0) newACLine(
    nm, "ELC", a, b, reactance = x, cap2act = 1,
    vintage = data.frame(olife = 50L),
    trade = tt$trade, cluster = tt$cluster, capacity = tt$capacity,
    varom = if (vom) data.frame(varom = vom) else data.frame())
  kvl_net(name, list(L("L12", "R1", "R2", 0.1, 0.01),
                     L("L23", "R2", "R3", 0.2, 0.01),
                     L("L13", "R1", "R3", 0.4)))
}

test_that("tranching does not change the cycle basis", {
  scen <- suppressMessages(interpolate_model(kvl_tranched("kvt1", 3),
                                             name = "kvt1", kvl = TRUE))
  # 3 lines over 3 regions is one cycle, whether each line is one object or
  # three. A basis that counted tranches would find 9 - 3 + 1 = 7.
  expect_equal(length(energyRt:::.kvl_scenario_cycles(scen)), 1)
  # ... and every name the constraint sums over is a real trade set member.
  cn <- scen@model@data[["kvl_cycles"]]@data
  nms <- unique(unlist(lapply(cn, function(k)
    unlist(lapply(k@lhs, function(s) s@for.sum$trade)))))
  expect_length(nms, 9)                       # 3 lines x 3 tranches
  expect_true(all(nms %in% scen@modInp@sets$trade))
})

test_that("a tranched network solves to the same LINE-level flows", {
  skip_if_no_solver()
  plain <- kvl_scen("kvt2", KVL_TRIANGLE(), TRUE)
  tranched <- suppressMessages(interpolate_model(kvl_tranched("kvt3", 2),
                                                 name = "kvt3", kvl = TRUE))
  line_flows <- function(s) {
    d <- as.data.frame(getData(suppressMessages(solve_scenario(s)), "vTradeIr",
                               merge = TRUE))
    d <- d[d$value > 1e-9, ]
    # Sum the tranches back up to their line.
    k <- paste0(sub("_CL.*$", "", d$trade), ":", d$src, "->", d$dst)
    v <- tapply(d$value, k, sum)
    setNames(round(as.numeric(v), 4), names(v))
  }
  expect_equal(line_flows(tranched), line_flows(plain))
  expect_equal(round(kvl_obj(tranched), 6), round(kvl_obj(plain), 6))
})

test_that("a per-tranche reactance is refused", {
  # Reactance is a property of the LINE. Tranches differ in loss rate and in
  # capacity, never in impedance -- so a `cluster`-varying reactance is a
  # modelling error, not something to average.
  tt <- lossTranches(c(0.5, 0.5), loss = 1e-9, fix = 100)
  tr <- tt$trade
  tr$reactance <- c(0.1, 0.2)
  bad <- newACLine("L", "ELC", "R1", "R2", reactance = 0.1, cap2act = 1,
                   vintage = data.frame(olife = 50L),
                   trade = tr, cluster = tt$cluster, capacity = tt$capacity)
  prov <- data.frame(name = c("L_CLT1", "L_CLT2"), base = "L",
                     stringsAsFactors = FALSE)
  a <- bad; a@name <- "L_CLT1"; a@trade$reactance <- 0.1
  b <- bad; b@name <- "L_CLT2"; b@trade$reactance <- 0.2
  expect_error(energyRt:::.kvl_lines(list(a, b), prov),
               "Reactance is a property of the line")
})

test_that("independent parallel circuits are still refused", {
  # The guard is re-keyed on the BASE object, so tranches pass and genuinely
  # separate circuits on one corridor do not -- they must be merged by the user
  # with 1/x_eq = sum(1/x_i), because each would otherwise be constrained as if
  # it were the whole line.
  expect_error(
    energyRt:::.kvl_lines(list(newACLine("A", "ELC", "R1", "R2", reactance = 0.1),
                               newACLine("B", "ELC", "R2", "R1", reactance = 0.2))),
    "more than one trade object")
})

# =========================================================================== #
# Regressions: KVL must see the WHOLE, EXPANDED model
# =========================================================================== #

test_that("an AC line passed through `...` is in the cycle basis", {
  # `interpolate_model()` folds repositories from `...` into the model, and the
  # KVL hook has to run after that. Before the fix the basis was built on a
  # SUBGRAPH -- here it found no cycle at all and silently solved the transport
  # relaxation.
  radial <- kvl_net("kvt4", KVL_RADIAL())
  scen <- suppressMessages(interpolate_model(
    radial, name = "kvt4",
    newRepository("extra", kvl_line("L13", "R1", "R3", 0.4)), kvl = TRUE))
  expect_equal(length(energyRt:::.kvl_scenario_cycles(scen)), 1)
})

test_that("a VINTAGED AC line names set members that exist", {
  # `trade` has always supported vintages, so `L12` becomes `L12_VIN2020` etc.
  # The hook used to run before expansion and emit `for.sum = list(trade =
  # "L12")` -- a member that no longer existed by the time the model was written.
  vin <- function(nm, a, b, x) newACLine(
    nm, "ELC", a, b, reactance = x, cap2act = 1,
    vintage = data.frame(vintage = c("2020", "2030"), olife = 50L),
    capacity = data.frame(vintage = c("2020", "2030"), cap.fx = 50))
  m <- kvl_net("kvt5", list(vin("L12", "R1", "R2", 0.1),
                            kvl_line("L23", "R2", "R3", 0.2),
                            kvl_line("L13", "R1", "R3", 0.4)))
  scen <- suppressMessages(interpolate_model(m, name = "kvt5", kvl = TRUE))
  cn <- scen@model@data[["kvl_cycles"]]@data
  nms <- unique(unlist(lapply(cn, function(k)
    unlist(lapply(k@lhs, function(s) s@for.sum$trade)))))
  expect_true(all(c("L12_VIN2020", "L12_VIN2030") %in% nms))
  expect_true(all(nms %in% scen@modInp@sets$trade))
})
