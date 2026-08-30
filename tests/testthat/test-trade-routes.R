# A-B corridors: what a route is, what shares capacity, where losses land, and
# what a per-route cost actually does.
#
# Every number here was measured before it was written down. Two of them
# contradict what `trade.qmd` said when it was drafted from reading the
# equations, which is the argument for the file existing.

test_that("a route is directed and nothing infers the reverse", {
  one <- newTrade("T1", commodity = "ELC", routes = TR_ONE_WAY)
  two <- newTrade("T2", commodity = "ELC", routes = TR_TWO_WAY)
  expect_equal(nrow(one@routes), 1L)
  expect_equal(nrow(two@routes), 2L)
  # Region scope is the UNION of endpoints, so it does not depend on direction.
  expect_setequal(get_region(one), c("R1", "R2"))
  expect_setequal(get_region(two), c("R1", "R2"))
})

test_that("capacity limits the flow -- once building it is not free", {
  skip_if_no_solver()
  # R2's own supply costs 100, R1's costs 1, demand is 10 at R2.
  # Unconstrained the corridor carries all 10 at a cost of 10.
  expect_equal(tr_obj(tr_model(TR_ONE_WAY, name = "trc1")), 10)
  # Pinned at 4, the corridor carries 4 and R2 buys the other 6 at 100:
  # 4 * 1 + 6 * 100 = 604.
  expect_equal(tr_obj(tr_model(TR_ONE_WAY, cap = 4, name = "trc2")), 604)
})

test_that("both directions share ONE capacity, so the sum is what binds", {
  skip_if_no_solver()
  # `ava.fx` forces a flow each way, so the only question is whether the
  # capacity constraint sums them. It does: eqTradeCapFlow sums over src and dst.
  fx <- function(a, b) data.frame(src = c("R1", "R2"), dst = c("R2", "R1"),
                                  ava.fx = c(a, b))
  both <- function(cap, a, b, nm) tr_model(
    TR_TWO_WAY, cap = cap, trade = fx(a, b), name = nm,
    demand = data.frame(region = c("R1", "R2"), demand = c(5, 5)),
    supply = data.frame(region = c("R1", "R2"), cost = c(1, 1)))

  expect_false(tr_infeasible(both(10, 5, 5, "trs1")))   # 5 + 5 = 10, fits
  expect_true( tr_infeasible(both(10, 6, 6, "trs2")))   # 6 + 6 = 12 > 10
  expect_false(tr_infeasible(both(12, 6, 6, "trs3")))   # 12 fits in 12

  # A corridor rated 100 therefore carries 100 IN TOTAL, not 100 each way. Two
  # independent parallel lines need two trade objects -- one object has exactly
  # one `vTradeCap`.
})

test_that("`ava.*` bounds the flow in absolute units, per direction", {
  skip_if_no_solver()
  # Same 604 as the capacity case above, by a different mechanism: `ava.up` is
  # an absolute commodity flow, NOT a fraction of capacity. There is no `af` for
  # trade, which is why a mesh in one object has no expressible line rating.
  expect_equal(
    tr_obj(tr_model(TR_ONE_WAY, name = "tra1",
                    trade = data.frame(src = "R1", dst = "R2", ava.up = 4))),
    604)
  # Asymmetric bounds: the reverse leg is bounded separately and is unused here.
  expect_equal(
    tr_obj(tr_model(TR_TWO_WAY, name = "tra2",
                    trade = data.frame(src = c("R1", "R2"), dst = c("R2", "R1"),
                                       ava.up = c(4, 1)))),
    604)
})

test_that("losses are counted on arrival, not on departure", {
  skip_if_no_solver()
  # `teff = 0.9`: `vTradeIr` is what LEAVES src. To land 10 at R2, 11.111 must
  # leave R1 -- and R1 pays for all of it.
  sol <- tr_solve(tr_model(TR_ONE_WAY, name = "trl1",
                           trade = data.frame(teff = 0.9)))
  expect_equal(unname(tr_flows(sol)["R1->R2"]), 10 / 0.9, tolerance = 1e-6)
  expect_equal(getData(sol, "vObjective", merge = TRUE)$value[1], 10 / 0.9,
               tolerance = 1e-6)
})

test_that("`@varom` is a real cost at the source; `markup` is a transfer", {
  skip_if_no_solver()
  # trade's `@varom` is per ROUTE -- the only trade cost that is -- and its two
  # columns mean different things:
  #
  #   varom  : variable O&M, a REAL resource cost, borne where the flow
  #            originates. Same economics as technology@varom.
  #   markup : a wheeling charge or border price. The importer pays it and the
  #            exporter receives it, so it moves cost between regions and
  #            changes no total.
  #
  # Until 0.85 both were lumped into one term and BOTH cancelled, so `varom`
  # could not express an operating cost at all. Baseline: 10 units flow, R1's
  # supply costs 1, so the objective is 10.
  plain  <- tr_model(TR_ONE_WAY, name = "trv1")
  cost   <- tr_model(TR_ONE_WAY, name = "trv2", varom  = data.frame(varom = 2))
  fee    <- tr_model(TR_ONE_WAY, name = "trv3", varom  = data.frame(markup = 3))
  both   <- tr_model(TR_ONE_WAY, name = "trv4",
                     varom = data.frame(varom = 2, markup = 3))

  expect_equal(tr_obj(plain), 10)
  # varom is real: 10 units x 2 added to system cost.
  expect_equal(tr_obj(cost), 30)
  # markup is not: the objective does not move at all.
  expect_equal(tr_obj(fee), 10)
  # and together, only the real half shows up in the total.
  expect_equal(tr_obj(both), 30)

  # Regionally: varom lands on the exporter, markup moves importer -> exporter.
  expect_equal(unname(tr_regional_cost(tr_solve(cost))["R1"]), 30)   # 10 + 10x2
  fee_rc <- tr_regional_cost(tr_solve(fee))
  expect_equal(unname(fee_rc["R2"]),  30)    # importer pays 10 x 3
  expect_equal(unname(fee_rc["R1"]), -20)    # exporter receives it
  expect_equal(sum(fee_rc), 10)              # ... and the transfer nets to zero
})

# =========================================================================== #
# Relative flow bounds: `af.*` per route
# =========================================================================== #

test_that("`af.up` rates one direction as a fraction of the object's capacity", {
  skip_if_no_solver()
  # `ava.*` is absolute and `af.*` is relative, and the difference is not
  # cosmetic: a multi-route object has ONE capacity, so an absolute per-line
  # rating would have to be a number correct for exactly one capacity -- and
  # capacity is the decision. `af` tracks it instead.
  #
  # Baseline: cap.fx = 100, cap2act = 1, one slice, so the corridor could carry
  # 100. Demand is 10 at R2, R1's supply costs 1 and R2's costs 100, so all 10
  # are imported and the objective is 10.
  expect_equal(tr_obj(tr_model(TR_ONE_WAY, name = "afa")), 10)

  # af.up = 0.05 rates the leg at 0.05 x cap2act x 100 x share = 5. Half the
  # demand must now be met from R2's own expensive supply.
  capped <- tr_model(TR_ONE_WAY, name = "afb", trade = data.frame(af.up = 0.05))
  expect_equal(tr_obj(capped), 505)              # 5 x 1 + 5 x 100
  expect_equal(unname(tr_flows(tr_solve(capped))["R1->R2"]), 5)

  # It scales with capacity, which an `ava.up` of 5 would not: halve the
  # corridor and the same af.up halves the flow.
  half <- tr_model(TR_ONE_WAY, name = "afc", cap = 50,
                   trade = data.frame(af.up = 0.05))
  expect_equal(unname(tr_flows(tr_solve(half))["R1->R2"]), 2.5)
})

test_that("`af` is per direction, and empty unless declared", {
  skip_if_no_solver()
  # Declared on one direction of a two-way corridor, it binds only that one.
  # R1 -> R2 is the direction demand pulls on, so rating R2 -> R1 changes
  # nothing at all.
  wrong_way <- tr_model(TR_TWO_WAY, name = "afd",
                        trade = data.frame(src = "R2", dst = "R1", af.up = 0.01))
  expect_equal(tr_obj(wrong_way), 10)

  right_way <- tr_model(TR_TWO_WAY, name = "afe",
                        trade = data.frame(src = "R1", dst = "R2", af.up = 0.01))
  expect_equal(tr_obj(right_way), 901)           # 1 x 1 + 9 x 100

  # And a model that declares no `af` at all is untouched: the gating maps are
  # empty, so no equations are generated.
  expect_equal(tr_obj(tr_model(TR_TWO_WAY, name = "aff")), 10)
})

# =========================================================================== #
# One object or several: the same network, packaged two ways
# =========================================================================== #

test_that("a mesh in ONE object shares a single throughput budget", {
  skip_if_no_solver()
  # `@routes` is a table, so a whole network fits in one object. But one object
  # has one `vTradeCap`, and `eqTradeCapFlow` sums EVERY route against it. The
  # mesh is therefore a shared budget, not a set of independently rated lines --
  # and a two-hop path spends the budget twice per unit delivered.
  #
  # Demand 10 at R3; supply costs 1 at R1, 5 at R2, 100 at R3. With a budget of
  # 10 the direct R1 -> R3 leg carries the lot.
  one <- tr_solve(tr_mesh_one(name = "mesha"))
  expect_equal(getData(one, "vObjective", merge = TRUE)$value[1], 10)
  expect_equal(tr_mesh_flows(one), c("R1->R3" = 10))

  # Rating the direct leg at half the budget is where the packaging shows.
  # Wheeling the other 5 through R2 would cost 5 on R1 -> R2 AND 5 on R2 -> R3,
  # which the shared budget cannot afford, so R2 sells its own supply instead.
  af <- data.frame(src = "R1", dst = "R3", af.up = 0.5)
  capped <- tr_solve(tr_mesh_one(trade = af, name = "meshb"))
  expect_equal(getData(capped, "vObjective", merge = TRUE)$value[1], 30)
  expect_equal(tr_mesh_flows(capped), c("R1->R3" = 5, "R2->R3" = 5))
})

test_that("the SAME network as three objects gives a different answer", {
  skip_if_no_solver()
  # Identical topology, identical rating -- but three objects mean three
  # independent capacities, so wheeling through R2 no longer competes with the
  # direct leg for the same budget. R1's cheap supply now serves all 10.
  af <- data.frame(src = "R1", dst = "R3", af.up = 0.5)
  many <- tr_solve(tr_mesh_many(trade = af, name = "meshc"))
  expect_equal(getData(many, "vObjective", merge = TRUE)$value[1], 10)
  expect_equal(tr_mesh_flows(many),
               c("R1->R2" = 5, "R1->R3" = 5, "R2->R3" = 5))

  # Unconstrained the two packagings agree, which is what makes the divergence
  # above a property of the capacity sharing rather than of the fixtures.
  expect_equal(getData(tr_solve(tr_mesh_many(name = "meshd")),
                       "vObjective", merge = TRUE)$value[1], 10)
})

test_that("`af.fx` pins a leg whether or not the flow is wanted", {
  skip_if_no_solver()
  # af.fx = 0.2 forces R1 -> R2 to carry exactly 2, spending 2 of the shared
  # budget of 10 on a leg the solver would not have used. What arrives at R2
  # goes on to R3 (2 more of the budget), leaving 6 for the direct leg. R3 is
  # then 2 short of its demand of 10 and must self-supply at 100.
  pinned <- tr_solve(tr_mesh_one(
    trade = data.frame(src = "R1", dst = "R2", af.fx = 0.2), name = "meshe"))
  expect_equal(tr_mesh_flows(pinned),
               c("R1->R2" = 2, "R1->R3" = 6, "R2->R3" = 2))
  expect_equal(getData(pinned, "vObjective", merge = TRUE)$value[1], 208)
})
