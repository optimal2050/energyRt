# =========================================================================== #
# `commodity@cluster` -- resource tiers that must not pool.
#
# The commodity balance is keyed on (comm, region, year, timeslice) and carries
# no cluster, so every producer of one commodity NAME writes into the same cell:
# clusterizing a supply alone cannot stop tier-1 wind drawing tier-3 land. The
# commodity name is the only tier-carrying key the balance has, so a declared
# tier becomes its own commodity (`LAND` tier `T3` -> `LAND_CLT3`) with its own
# balance, and references from clustered processes are repointed by cluster NAME
# through the same `.resolve_cluster_ref()` the weather tiers use.
#
# This is what lets ONE commodity and ONE supply object cover a tier set, which
# is the whole point: `supply@commodity` is a scalar, so without tiered
# commodities a tier set means one supply object per tier.
#
# A table carrying its own `cluster` column resolves per ROW (`@ceff`, `@aeff`);
# one without resolves at the OBJECT's cluster (`@input`, `@output`, every
# `aux`, and the scalar `@commodity` of supply/demand/import/export).
#
# @covers mvBalance pSupAva depth=I backends=glpk
# =========================================================================== #

cc_tiers <- c("T1", "T2", "T3")

cc_cal <- function() {
  newCalendar(timetable = make_timetable(
    struct = list(ANNUAL = "ANNUAL", SEASON = c("WIN", "SUM"))), name = "cccal")
}

# ONE land commodity with tiers + ONE supply object with matching clusters
cc_land <- function(tiers = cc_tiers) {
  newCommodity("LAND", timeframe = "ANNUAL",
               cluster = data.frame(cluster = tiers,
                                    order = seq_along(tiers)))
}

cc_supply <- function(ava = c(4, 4, 4), tiers = cc_tiers) {
  newSupply("SUP_LAND", commodity = "LAND",
            cluster = data.frame(cluster = tiers, region = NA_character_,
                                 order = seq_along(tiers)),
            supply = data.frame(cluster = tiers, ava.up = ava,
                                cost = seq_along(tiers)))
}

cc_tech <- function(tiers = cc_tiers, comm = "LAND") {
  newTechnology("EWIN", input = list(comm = comm), output = list(comm = "ELC"),
                cluster = data.frame(cluster = tiers, region = NA_character_),
                ceff = data.frame(cluster = tiers, comm = "ELC",
                                  cact2cout = c(1, 0.8, 0.5)[seq_along(tiers)]),
                capacity = data.frame(cluster = tiers, cap.up = 100),
                invcost = data.frame(cluster = tiers, invcost = 0.1),
                vintage = data.frame(olife = 30L), cap2act = 1)
}

cc_model <- function(..., name = "cc") {
  newModel(name, calendar = cc_cal(), region = "R1",
           horizon = newHorizon(2025), discount = 0,
           repo = newRepository("r",
             newCommodity("ELC", timeframe = "SEASON"),
             newImport("IMP", commodity = "ELC",
                       import = data.frame(price = 100)),
             newDemand("DE", commodity = "ELC",
                       demand = data.frame(timeslice = c("WIN", "SUM"),
                                           demand = 3)),
             ...))
}

cc_interp <- function(mod, name = "cc_i") {
  suppressMessages(suppressWarnings(
    interpolate_model(mod, name = name, ondisk = FALSE)))
}

cc_solve <- function(mod, name) {
  s <- suppressMessages(suppressWarnings(solve_model(
    mod, name = name, ondisk = FALSE, overwrite = TRUE, echo = FALSE)))
  inp <- suppressWarnings(getData(s, "vTechInp", merge = TRUE))
  list(obj = getData(s, "vObjective", merge = TRUE)$value[1],
       draw = stats::aggregate(value ~ tech + comm, inp, sum))
}

# --------------------------------------------------------------------------- #

test_that("a tiered commodity round-trips and ELC stays plain", {
  land <- cc_land()
  expect_equal(land@cluster$cluster, cc_tiers)
  expect_false("region" %in% colnames(land@cluster))   # deliberately absent
  expect_equal(nrow(newCommodity("ELC")@cluster), 0L)
})

test_that("each tier becomes its own commodity and the bundle is gone", {
  scen <- cc_interp(cc_model(cc_land(), cc_supply(), cc_tech(), name = "cc_set"))
  expect_setequal(as.character(scen@modInp@sets$comm),
                  c("ELC", "LAND_CLT1", "LAND_CLT2", "LAND_CLT3"))
  expect_false("LAND" %in% as.character(scen@modInp@sets$comm))
  # one supply OBJECT became one variant per tier, each on its own commodity
  expect_setequal(as.character(scen@modInp@sets$sup),
                  c("SUP_LAND_CLT1", "SUP_LAND_CLT2", "SUP_LAND_CLT3"))
  sc <- as.data.frame(get_data_slot(scen@modInp@parameters$mSupComm))
  expect_equal(sc$comm[sc$sup == "SUP_LAND_CLT2"], "LAND_CLT2")
})

test_that("a technology input resolves at the OBJECT's cluster", {
  # `@input` has no cluster column, so the variant's own cluster picks the tier
  scen <- cc_interp(cc_model(cc_land(), cc_supply(), cc_tech(), name = "cc_inp"))
  inp <- as.data.frame(get_data_slot(scen@modInp@parameters$mvTechInp))
  pairs <- unique(inp[, c("tech", "comm")])
  expect_equal(pairs$comm[pairs$tech == "EWIN_CLT1"], "LAND_CLT1")
  expect_equal(pairs$comm[pairs$tech == "EWIN_CLT3"], "LAND_CLT3")
  # ELC is unclustered, so every variant shares it -- no flag needed
  out <- as.data.frame(get_data_slot(scen@modInp@parameters$mvTechOut))
  expect_true(all(out$comm == "ELC"))
})

test_that("tiers do not pool: more tier-1 land does not help another tier", {
  skip_if_no_solver()
  base <- cc_solve(cc_model(cc_land(), cc_supply(), cc_tech(), name = "cc_b"),
                   "cc_bind_base")
  # tier 1 exhausted (4), so the next grade runs
  expect_equal(base$draw$value[base$draw$tech == "EWIN_CLT1"], 4)
  expect_true(any(base$draw$tech == "EWIN_CLT2"))
  # every draw is from the variant's OWN tier
  expect_true(all(sub("^EWIN_CL", "", base$draw$tech) ==
                    sub("^LAND_CL", "", base$draw$comm)))

  more <- cc_solve(cc_model(cc_land(), cc_supply(ava = c(8, 4, 4)), cc_tech(),
                            name = "cc_m"), "cc_bind_more")
  # with more of the best grade the worse ones are not needed, and it is cheaper
  expect_false(any(more$draw$tech == "EWIN_CLT2"))
  expect_lt(more$obj, base$obj)
})

test_that("clustered commodities give the same LP as hand-named ones", {
  skip_if_no_solver()
  clustered <- cc_model(cc_land(), cc_supply(), cc_tech(), name = "cc_eq_c")
  # the encoding available before this feature: one commodity and one supply
  # object per tier, named by hand
  named <- cc_model(
    newCommodity("LAND_CLT1", timeframe = "ANNUAL"),
    newCommodity("LAND_CLT2", timeframe = "ANNUAL"),
    newCommodity("LAND_CLT3", timeframe = "ANNUAL"),
    newSupply("SUP_LAND_CLT1", commodity = "LAND_CLT1",
              supply = data.frame(ava.up = 4, cost = 1)),
    newSupply("SUP_LAND_CLT2", commodity = "LAND_CLT2",
              supply = data.frame(ava.up = 4, cost = 2)),
    newSupply("SUP_LAND_CLT3", commodity = "LAND_CLT3",
              supply = data.frame(ava.up = 4, cost = 3)),
    newTechnology("EWIN_CLT1", input = list(comm = "LAND_CLT1"),
                  output = list(comm = "ELC"),
                  ceff = data.frame(comm = "ELC", cact2cout = 1),
                  capacity = data.frame(cap.up = 100),
                  invcost = data.frame(invcost = 0.1),
                  vintage = data.frame(olife = 30L), cap2act = 1),
    newTechnology("EWIN_CLT2", input = list(comm = "LAND_CLT2"),
                  output = list(comm = "ELC"),
                  ceff = data.frame(comm = "ELC", cact2cout = 0.8),
                  capacity = data.frame(cap.up = 100),
                  invcost = data.frame(invcost = 0.1),
                  vintage = data.frame(olife = 30L), cap2act = 1),
    newTechnology("EWIN_CLT3", input = list(comm = "LAND_CLT3"),
                  output = list(comm = "ELC"),
                  ceff = data.frame(comm = "ELC", cact2cout = 0.5),
                  capacity = data.frame(cap.up = 100),
                  invcost = data.frame(invcost = 0.1),
                  vintage = data.frame(olife = 30L), cap2act = 1),
    name = "cc_eq_n")
  expect_equal(cc_solve(clustered, "cc_eqc")$obj,
               cc_solve(named, "cc_eqn")$obj, tolerance = 1e-9)
})

test_that("an unclustered process may not reference a tiered commodity", {
  plain <- newTechnology("EPLAIN", input = list(comm = "LAND"),
                         output = list(comm = "ELC"),
                         capacity = data.frame(cap.up = 10),
                         invcost = data.frame(invcost = 1),
                         vintage = data.frame(olife = 30L), cap2act = 1)
  expect_error(
    cc_interp(cc_model(cc_land(), cc_supply(), plain, name = "cc_plain")),
    "without a cluster")
})

test_that("a tier the commodity does not declare is refused", {
  expect_error(
    cc_interp(cc_model(cc_land(tiers = c("T1", "T2")),
                       cc_supply(ava = c(4, 4), tiers = c("T1", "T2")),
                       cc_tech(),                       # asks for T3 too
                       name = "cc_t3")),
    "does not declare")
})

test_that("a name collision with an existing commodity is refused", {
  expect_error(
    cc_interp(cc_model(cc_land(), cc_supply(), cc_tech(),
                       newCommodity("LAND_CLT2", timeframe = "ANNUAL"),
                       name = "cc_coll")),
    "already declares")
})

# --------------------------------------------------------------------------- #
# Per-tier DATA. Grades of one commodity differ in more than name: coal grades
# differ in carbon content and heat value, so `@emis`, `@agg` and `@property`
# carry a `cluster` column with the usual rule -- a row with no cluster is
# shared by every tier.
# --------------------------------------------------------------------------- #

cc_coal <- function(grades = c("ANTH", "LIG"), declare = TRUE) {
  newCommodity(
    "COA", timeframe = "ANNUAL",
    cluster = if (declare)
      data.frame(cluster = grades, desc = c("anthracite", "lignite")[seq_along(grades)],
                 order = seq_along(grades)) else data.frame(),
    emis = data.frame(cluster = c(grades, NA),
                      comm = c(rep("CO2", length(grades)), "SO2"),
                      emis = c(c(0.090, 0.110)[seq_along(grades)], 0.001)))
}

cc_coal_model <- function(coal = cc_coal(), grades = c("ANTH", "LIG"),
                          name = "cc_coal") {
  newModel(name, calendar = cc_cal(), region = "R1",
           horizon = newHorizon(c(2025, 2030)), discount = 0.05,
           repo = newRepository("r",
             newCommodity("ELC", timeframe = "SEASON"),
             newCommodity("CO2", timeframe = "ANNUAL", limtype = "UP"),
             newCommodity("SO2", timeframe = "ANNUAL", limtype = "UP"),
             coal,
             newSupply("SUP_COA", commodity = "COA",
                       cluster = data.frame(cluster = grades,
                                            region = NA_character_),
                       supply = data.frame(cluster = grades, ava.up = 100,
                                           cost = c(3, 1)[seq_along(grades)])),
             newTechnology("PP", input = list(comm = "COA"),
                           output = list(comm = "ELC"),
                           cluster = data.frame(cluster = grades,
                                                region = NA_character_),
                           ceff = data.frame(cluster = grades, comm = "ELC",
                                             cact2cout = 0.4),
                           capacity = data.frame(cluster = grades, cap.up = 100),
                           invcost = data.frame(cluster = grades, invcost = 1),
                           vintage = data.frame(olife = 30L), cap2act = 1),
             newImport("IMP", commodity = "ELC",
                       import = data.frame(price = 5000)),
             newDemand("DE", commodity = "ELC",
                       demand = data.frame(demand = 6))))
}

test_that("each tier keeps its own data rows plus the shared ones", {
  scen <- cc_interp(cc_coal_model(), "cc_coal_i")
  ef <- as.data.frame(getData(scen, "pEmissionFactor", merge = TRUE))
  co2 <- ef[ef$comm == "CO2", ]
  expect_equal(co2$value[co2$commp == "COA_CLANTH"], 0.090)
  expect_equal(co2$value[co2$commp == "COA_CLLIG"], 0.110)
  # the NA row is shared by both tiers
  so2 <- ef[ef$comm == "SO2", ]
  expect_setequal(so2$commp, c("COA_CLANTH", "COA_CLLIG"))
  expect_true(all(so2$value == 0.001))
})

test_that("the cluster column is dropped from a clone's data slots", {
  clone <- energyRt:::.commodity_cluster_clone(cc_coal(), "ANTH", "COA_CLANTH")
  expect_false("cluster" %in% colnames(clone@emis))
  expect_equal(nrow(clone@emis), 2L)          # its own CO2 + the shared SO2
  expect_equal(nrow(clone@cluster), 0L)       # the roster does not travel
})

test_that("a data-slot label absent from the declaration is a typo, not a member", {
  bad <- newCommodity(
    "COA", timeframe = "ANNUAL",
    cluster = data.frame(cluster = c("ANTH", "LIG"), order = 1:2),
    emis = data.frame(cluster = "LIGN",          # typo
                      comm = "CO2", emis = 0.11))
  expect_error(energyRt:::.commodity_clusters(bad), "does not declare")
})

test_that("tiers used in a data slot must be declared", {
  # harvesting the labels was the earlier behaviour: a misspelled grade then
  # became a commodity carrying one stray row
  undeclared <- newCommodity(
    "COA", timeframe = "ANNUAL",
    emis = data.frame(cluster = c("ANTH", "LIG"), comm = "CO2",
                      emis = c(0.09, 0.11)))
  expect_error(energyRt:::.commodity_clusters(undeclared),
               "declares no `cluster` roster")
  expect_error(energyRt:::.commodity_clusters(undeclared), "emis")
})

# --------------------------------------------------------------------------- #
# REGION-SCOPED rosters. A cluster declaration may carry one row per
# (cluster, region) -- which is how a tier set of a different size per region
# is expressed -- and expansion then keeps EVERY row of the variant's label.
# `.object_cluster()` must therefore key on the label, not on the row count:
# keying on the count resolved a region-scoped variant to NA, and the scalar
# `@commodity` reference was then refused for naming a tiered commodity
# "without a cluster" (reported against a 3-tier, 4-region land model).
# --------------------------------------------------------------------------- #

test_that("a region-scoped roster still identifies its variant's cluster", {
  regs <- c("EAST", "WEST")
  tiers <- c("01", "02")
  roster <- data.frame(
    cluster = rep(tiers, each = length(regs)),
    region = rep(regs, times = length(tiers)),
    stringsAsFactors = FALSE)

  sup <- newSupply(
    "RES_LAND", commodity = "LAND", cluster = roster,
    supply = data.frame(cluster = roster$cluster, region = roster$region,
                        ava.up = 4, cost = 1))
  expanded <- energyRt:::.expand_one_process(sup)$objects
  nm <- vapply(expanded, function(o) o@name, character(1))
  expect_setequal(nm, c("RES_LAND_CL01", "RES_LAND_CL02"))

  v <- expanded[[which(nm == "RES_LAND_CL01")]]
  expect_equal(nrow(v@cluster), length(regs))        # one row per region
  expect_equal(energyRt:::.object_cluster(v), "01")  # ... one label
})

test_that("a region-scoped tier set resolves its commodity end to end", {
  regs <- c("EAST", "WEST")
  tiers <- c("01", "02")
  roster <- data.frame(cluster = rep(tiers, each = length(regs)),
                       region = rep(regs, times = length(tiers)),
                       stringsAsFactors = FALSE)
  mod <- newModel(
    "cc_rs", calendar = cc_cal(), region = regs,
    horizon = newHorizon(c(2025, 2030)), discount = 0.05,
    repo = newRepository("r",
      newCommodity("ELC", timeframe = "SEASON"),
      newCommodity("LAND", timeframe = "ANNUAL",
                   cluster = data.frame(cluster = tiers, order = 1:2)),
      newSupply("RES_LAND", commodity = "LAND", cluster = roster,
                supply = data.frame(cluster = roster$cluster,
                                    region = roster$region,
                                    ava.up = 4, cost = c(1, 2, 1, 2))),
      newTechnology("EWIN", input = list(comm = "LAND"),
                    output = list(comm = "ELC"),
                    cluster = roster,
                    ceff = data.frame(cluster = tiers, comm = "ELC",
                                      cact2cout = c(1, 0.8)),
                    capacity = data.frame(cluster = tiers, cap.up = 100),
                    invcost = data.frame(cluster = tiers, invcost = 0.1),
                    vintage = data.frame(olife = 30L), cap2act = 1),
      newImport("IMP", commodity = "ELC", import = data.frame(price = 5000)),
      newDemand("DE", commodity = "ELC", demand = data.frame(demand = 3))))

  scen <- expect_no_error(suppressMessages(suppressWarnings(
    interpolate_model(mod, name = "cc_rs", ondisk = FALSE))))
  # each supply variant emits its OWN tier, in both regions
  ava <- as.data.frame(getData(scen, "pSupAva", merge = TRUE))
  pairs <- unique(ava[, c("sup", "comm", "region")])
  expect_equal(pairs$comm[pairs$sup == "RES_LAND_CL01"][1], "LAND_CL01")
  expect_setequal(pairs$region[pairs$sup == "RES_LAND_CL01"], regs)
})
