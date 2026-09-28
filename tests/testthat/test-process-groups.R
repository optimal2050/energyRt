# Cluster-preserving region aggregation: get_process_groups() decides WHAT may
# be merged, aggregate_model_regions() does the merging. The invariant that pins
# the whole thing is at the 1:1 end -- one cluster per fine region averages
# nothing, so the coarse model must reproduce the fine objective exactly.

skip_if_no_clustering <- function() {
  testthat::skip_if_not_installed("geoscales")
  testthat::skip_if_not_installed("clusterscales")
  testthat::skip_if_not_installed("multiscales")
  testthat::skip_if_not_installed("sf")
}

# Four regions over two zones: W1+W2 (WEST) and C1+C2 (CENTRAL), each pair
# adjacent on the honeycomb layout. Two archetypes per zone, so k = 2 collapses
# each zone to one and k = 4 keeps them all.
cr_regions <- c("W1", "W2", "C1", "C2")
cr_par <- data.frame(region = cr_regions,
                     eff = c(0.42, 0.33, 0.41, 0.34),
                     invcost = c(1000, 1500, 1020, 1480))

cr_geoscale <- function() {
  geoscales::filter_geoscale(topia$geoscales$honeycomb, "region", cr_regions)
}

cr_model <- function(with_trade = TRUE) {
  cal <- topia$modules$calendars$topia_seasons
  shape <- topia$demand[topia$demand$calendar == "topia_seasons", ]
  tt <- as.data.frame(cal@timetable)
  shape$share <- tt$share[match(shape$timeslice, tt$timeslice)]
  shape$w <- shape$load * shape$share
  shape$w <- shape$w / sum(shape$w)
  lvl <- stats::setNames(c(90, 100, 110, 120), cr_regions)
  peak <- vapply(cr_regions, function(r) max(lvl[[r]] * shape$w / shape$share),
                 numeric(1))

  objs <- list(
    newCommodity("COA", unit = "PJ"),
    newCommodity("ELC", unit = "PJ", timeframe = "HOUR"),
    newSupply("SUP_COA", commodity = "COA", unit = "PJ",
              supply = data.frame(region = cr_regions, cost = 2)),
    newDemand("DEM_ELC", commodity = "ELC",
              demand = do.call(rbind, lapply(cr_regions, function(r)
                data.frame(region = r, timeslice = shape$timeslice,
                           demand = lvl[[r]] * shape$w))))
  )
  techs <- lapply(cr_regions, function(r) {
    p <- cr_par[cr_par$region == r, ]
    newTechnology(paste0("ECOA_", r),
      input = list(comm = "COA", unit = "PJ"),
      output = list(comm = "ELC", unit = "PJ"),
      ceff = data.frame(comm = "COA", cinp2use = p$eff),
      region = r,
      invcost = data.frame(region = r, invcost = p$invcost),
      fixom = data.frame(region = r, fixom = 0.03 * p$invcost),
      olife = data.frame(olife = 30),
      af = data.frame(region = r, af.up = 0.85),
      # 1.3 x own peak: enough to cover itself at af.up 0.85, not enough for
      # the zone, so both plants in a zone stay in the merit order
      capacity = data.frame(region = r, cap.up = 1.3 * unname(peak[r])))
  })
  # ample, lossless links INSIDE each zone: that is what makes the fine model
  # already pooled, and the 1:1 invariant a test of the parameter carry-over
  trades <- if (with_trade) lapply(list(c("W1", "W2"), c("C1", "C2")), function(p)
    newTrade(paste0("TBD_", p[1], "_", p[2]), commodity = "ELC",
             routes = data.frame(src = p, dst = rev(p)),
             trade = data.frame(src = p, dst = rev(p), teff = 1),
             capacity = data.frame(stock = 1e4))) else list()

  mod <- newModel("CR", region = cr_regions, calendar = cal,
                  horizon = newHorizon(2025), discount = 0.05,
                  data = newRepository("cr", c(objs, techs, trades)))
  setGeoscale(mod, cr_geoscale())
}

cr_solve <- function(mod, nm) {
  s <- suppressMessages(interpolate_model(mod, name = nm, overwrite = TRUE))
  s <- suppressMessages(solve_scenario(s, solver = solver_options$glpk,
                                       force = TRUE, wait = TRUE, echo = FALSE))
  getData(s, "vObjective", merge = TRUE)$value
}

# A fleet often arrives clustered before any aggregation -- wind by site grade.
# Moving it up a region level adds a SECOND grouping, and the two must compose:
# dropping the incoming column averaged the grades away silently, so eight
# variants came back as four.
cr_graded <- function() {
  ewin <- newTechnology("EWIN",
    region = cr_regions,
    cluster = data.frame(cluster = c("GOOD", "POOR")),
    input = list(comm = "WIN", unit = "PJ"),
    output = list(comm = "ELC", unit = "PJ"),
    ceff = data.frame(comm = "WIN", cinp2use = 1),
    invcost = data.frame(
      region = rep(cr_regions, each = 2),
      cluster = rep(c("GOOD", "POOR"), length(cr_regions)),
      invcost = c(1200, 900, 1500, 1100, 1250, 950, 1240, 940)),
    af = data.frame(cluster = c("GOOD", "POOR"), af.up = c(0.45, 0.25)),
    olife = data.frame(olife = 25))
  mod <- newModel("CRG", region = cr_regions,
                  calendar = topia$modules$calendars$topia_seasons,
                  horizon = newHorizon(2025), discount = 0.05,
                  data = newRepository("crg", list(
                    newCommodity("WIN", unit = "PJ"),
                    newCommodity("ELC", unit = "PJ", timeframe = "HOUR"),
                    ewin)))
  setGeoscale(mod, cr_geoscale())
}

# Covers the R API: get_process_groups()
test_that("groups are formed by STRUCTURE, not by name", {
  skip_if_no_clustering()
  mod <- cr_model()
  g <- get_process_groups(mod)
  # the coal fleet, the coal supply AND the intra-zone corridors: supply and
  # trade are groupable too, each reported with its own class
  expect_equal(nrow(g), 3L)
  expect_setequal(g$group, c("ECOA", "SUP_COA", "TRD"))
  expect_equal(g$class[g$group == "TRD"], "trade")
  expect_equal(g$class[g$group == "ECOA"], "technology")
  expect_equal(g$class[g$group == "SUP_COA"], "supply")
  g <- g[g$group == "ECOA", ]
  expect_equal(g$n, 4L)
  expect_match(g$signature, "in:COA")
  expect_match(g$signature, "out:ELC")

  # a gas plant has a different input, so it is a different family however
  # similar its costs -- a cluster is a variant of the SAME process
  gas <- newTechnology("EGAS_W1", input = list(comm = "GAS", unit = "PJ"),
                       output = list(comm = "ELC", unit = "PJ"), region = "W1")
  # `add()` appends a NEW repository rather than growing the first, so this
  # also pins that discovery reads every repository, not just `@data[[1]]`
  mod2 <- add(add(mod, newCommodity("GAS", unit = "PJ")), gas)
  expect_gt(length(mod2@data), 1L)
  g2 <- get_process_groups(mod2)
  expect_equal(nrow(g2), 4L)
  expect_setequal(g2$group, c("ECOA", "EGAS_W1", "SUP_COA", "TRD"))

  # A technology that ALREADY spans several regions is a group on its own: its
  # own rows carry the spread, so there is nothing to merge it with, and
  # grouping it with single-region siblings would double-count the regions
  # they share.
  span <- newTechnology("ESPAN", input = list(comm = "COA", unit = "PJ"),
                        output = list(comm = "ELC", unit = "PJ"),
                        region = cr_regions)
  g3 <- get_process_groups(add(mod, span))
  expect_true("ESPAN" %in% g3$group)
  expect_equal(g3$n[g3$group == "ESPAN"], length(cr_regions))
  expect_equal(g3$members[g3$group == "ESPAN"], "ESPAN")
  # ... and it does not swallow the per-region family with the same structure
  expect_equal(g3$members[g3$group == "ECOA"],
               paste(paste0("ECOA_", cr_regions), collapse = ","))
})

# Covers the R API: get_process_groups(), aggregate_model_regions(clusters=)
test_that("one multi-region technology clusters like a family of objects", {
  skip_if_no_clustering()
  # The idiomatic energyRt shape for region-varying parameters is ONE object
  # whose slots carry a `region` column -- not one object per region. Both
  # must cluster, and to the same answer.
  arch <- cr_par
  one <- newTechnology(
    name = "ECOA1",
    region = arch$region,
    input = list(comm = "COA", unit = "PJ"),
    output = list(comm = "ELC", unit = "PJ"),
    ceff = data.frame(region = arch$region, comm = "COA",
                      cinp2use = arch$eff),
    invcost = data.frame(region = arch$region, invcost = arch$invcost),
    fixom = data.frame(region = arch$region, fixom = 0.03 * arch$invcost),
    olife = data.frame(olife = 30),
    af = data.frame(af.up = 0.85))
  mod <- cr_model()
  objs <- mod@data[[1]]@data
  objs[paste0("ECOA_", cr_regions)] <- NULL
  objs$ECOA1 <- one
  mod@data[[1]]@data <- objs

  g <- get_process_groups(mod)
  expect_true("ECOA1" %in% g$group)
  expect_equal(g$n[g$group == "ECOA1"], length(cr_regions))

  m <- aggregate_model_regions(mod, level = "zone",
                               clusters = list(ECOA1 = 4), verbose = FALSE)
  tech <- getObject(m, name = "ECOA1", drop = TRUE)
  expect_equal(nrow(tech@cluster), 4L)
  # every region's efficiency survives, one per cluster
  expect_setequal(round(tech@ceff$cinp2use, 4), round(arch$eff, 4))
  # a slot that named no region stays a wildcard rather than being pinned
  expect_true(any(is.na(tech@vintage$region)))
  expect_equal(unique(tech@vintage$olife), 30)
})


# Covers the R API: aggregate_model_regions(clusters=)
test_that("clusters never straddle the target geoframe, and k has a floor", {
  skip_if_no_clustering()
  mod <- cr_model()
  # the adjacency graph has one component per zone, so `clusterscales` cannot
  # merge below the number of zones -- and says so rather than quietly
  # violating the constraint
  expect_error(aggregate_model_regions(mod, level = "zone", clusters = list(ECOA = 1)),
               "out of range for group")
  expect_error(aggregate_model_regions(mod, level = "zone", clusters = list(ECOA = 1)),
               "`zone` region")

  for (k in 2:4) {
    m <- aggregate_model_regions(mod, level = "zone", clusters = list(ECOA = k), verbose = FALSE)
    xw <- model_clusters(m)$ECOA$crosswalk
    expect_equal(length(unique(xw$cluster)), k)
    # every cluster sits in exactly one zone
    per <- tapply(xw$target, xw$cluster, function(z) length(unique(z)))
    expect_true(all(per == 1L))
    expect_setequal(xw$region, cr_regions)
  }
})

# Covers the R API: aggregate_model_regions(clusters=)
test_that("one cluster per coarse region is plain aggregation", {
  skip_if_no_clustering()
  mod <- cr_model()
  m <- aggregate_model_regions(mod, level = "zone", clusters = list(ECOA = 2), verbose = FALSE)
  tech <- getObject(m, name = "ECOA", drop = TRUE)
  expect_equal(nrow(tech@cluster), 2L)
  expect_setequal(tech@cluster$region, c("WEST", "CENTRAL"))
  # the merged object replaces the four originals
  expect_false(any(paste0("ECOA_", cr_regions) %in% names(m@data[[1]]@data)))

  # the efficiency of each cluster is the weighted mean of its members, and
  # with one cluster per zone that is the pair's mean
  west <- tech@ceff$cinp2use[tech@ceff$region == "WEST"]
  w <- vapply(c("W1", "W2"), function(r)
    getObject(mod, name = paste0("ECOA_", r), drop = TRUE)@capacity$cap.up[1],
    numeric(1))
  e <- cr_par$eff[match(c("W1", "W2"), cr_par$region)]
  expect_equal(west, sum(e * w) / sum(w), tolerance = 1e-8)
  # ... and that is NOT the unweighted mean, which is the point of weighting
  expect_false(isTRUE(all.equal(west, mean(e), tolerance = 1e-8)))
})

# @covers vObjective depth=S backends=glpk
# Covers the R API: aggregate_model_regions(clusters=)
test_that("1:1 clusters reproduce the fine objective exactly", {
  skip_if_no_clustering()
  skip_if_no_solver()
  mod <- cr_model()
  fine <- cr_solve(mod, "CRFINE")

  # k = one cluster per fine region: nothing is averaged, and because the fine
  # model already pools inside each zone the coarse model is the same problem
  m11 <- aggregate_model_regions(mod, level = "zone", clusters = list(ECOA = 4), verbose = FALSE)
  expect_equal(cr_solve(m11, "CRK4"), fine, tolerance = 1e-9)

  # collapsing to one variant per zone costs something -- never less
  m2 <- aggregate_model_regions(mod, level = "zone", clusters = list(ECOA = 2), verbose = FALSE)
  expect_gt(cr_solve(m2, "CRK2"), fine)
})

# Covers the R API: aggregate_model_regions(clusters=)
test_that("a link column resolves to the cluster medoid, not a key", {
  skip_if_no_clustering()
  # A `weather` column names ANOTHER object. It cannot be summed or averaged,
  # and treating it as a key makes one cluster sprout a row per member -- the
  # bug this pins. Each cluster must end up with exactly one link, and it must
  # be one a real member had.
  mod <- cr_model()
  wx <- lapply(seq_along(cr_regions), function(i) {
    nm <- paste0("WX_", cr_regions[i])
    newWeather(nm, timeframe = "HOUR",
               weather = data.frame(region = cr_regions,
                                    timeslice = "WIN_DAY",
                                    wval = 0.5 + 0.1 * i))
  })
  techs <- lapply(seq_along(cr_regions), function(i) {
    r <- cr_regions[i]
    o <- getObject(mod, name = paste0("ECOA_", r), drop = TRUE)
    methods::slot(o, "af") <- methods::slot(o, "af")[0, , drop = FALSE]
    methods::slot(o, "weather") <- data.frame(weather = paste0("WX_", r),
                                              waf.up = 1)
    o
  })
  objs <- mod@data[[1]]@data
  objs[paste0("ECOA_", cr_regions)] <- techs
  mod@data[[1]]@data <- c(objs, stats::setNames(wx, vapply(wx, function(o) o@name, "")))

  for (k in 2:4) {
    m <- aggregate_model_regions(mod, level = "zone", clusters = list(ECOA = k), verbose = FALSE)
    w <- getObject(m, name = "ECOA", drop = TRUE)@weather
    expect_equal(nrow(w), k, label = paste0("weather rows at k = ", k))
    expect_equal(length(unique(w$cluster)), k)
    # every link is a real member's, never invented
    expect_true(all(w$weather %in% paste0("WX_", cr_regions)))
    # and it is the medoid's
    xw <- model_clusters(m)$ECOA$crosswalk
    med <- stats::setNames(xw$region[xw$medoid], xw$cluster[xw$medoid])
    expect_equal(unname(w$weather), unname(paste0("WX_", med[w$cluster])))
  }
})

# Covers the R API: aggregate_model_regions(clusters=)
test_that("k is settable per group, and auto returns its sweep", {
  skip_if_no_clustering()
  mod <- cr_model()
  m <- aggregate_model_regions(mod, level = "zone", clusters = list(ECOA = "auto"), verbose = FALSE)
  cl <- model_clusters(m)$ECOA
  expect_true(cl$k >= 2L && cl$k <= 4L)
  expect_true(all(c("k", "within", "silhouette") %in% names(cl$sweep)))

  m3 <- aggregate_model_regions(mod, level = "zone",
                              clusters = list(ECOA = list(k = 3)),
                              verbose = FALSE)
  expect_equal(model_clusters(m3)$ECOA$k, 3L)
})

# -- settings are PER FAMILY -------------------------------------------------

# Two families whose structure sits in DIFFERENT places: the coal fleet's
# spread is inside WEST (0.42 vs 0.33) and the gas fleet's inside CENTRAL
# (0.55 vs 0.40). One `k` for the model would be wrong for one of them, and a
# grouping computed from the wrong family's parameters wrong for both.
cr_gas_par <- data.frame(region = cr_regions,
                         eff = c(0.50, 0.49, 0.55, 0.40),
                         invcost = c(800, 810, 700, 1200))

cr_model2 <- function() {
  mod <- add(cr_model(), newCommodity("GAS", unit = "PJ"))
  mod <- add(mod, newSupply("SUP_GAS", commodity = "GAS", unit = "PJ",
                            supply = data.frame(region = cr_regions, cost = 4)))
  gas <- lapply(cr_regions, function(r) {
    p <- cr_gas_par[cr_gas_par$region == r, ]
    newTechnology(paste0("EGAS_", r),
      input = list(comm = "GAS", unit = "PJ"),
      output = list(comm = "ELC", unit = "PJ"),
      ceff = data.frame(comm = "GAS", cinp2use = p$eff),
      region = r,
      invcost = data.frame(region = r, invcost = p$invcost),
      olife = data.frame(olife = 30),
      af = data.frame(region = r, af.up = 0.9),
      capacity = data.frame(region = r, cap.up = 50))
  })
  Reduce(function(m, o) add(m, o), gas, mod)
}

# Covers the R API: aggregate_model_regions(clusters=)
test_that("clusters = NULL never enters the clustering path", {
  skip_if_not_installed("geoscales")
  mod <- cr_model()
  plain <- aggregate_model_regions(mod, level = "zone", verbose = FALSE)
  expect_length(model_clusters(plain), 0L)
  # the four per-region objects survive as four objects, retargeted -- nothing
  # is turned into a cluster
  expect_true(all(paste0("ECOA_", cr_regions) %in% names(plain@data[[1]]@data)))
  expect_setequal(unique(getObject(plain, name = "ECOA_W1",
                                   drop = TRUE)@invcost$region), "WEST")
})

# Covers the R API: aggregate_model_regions(clusters=), get_process_groups()
test_that("a bare k is refused when the model has more than one family", {
  skip_if_no_clustering()
  mod2 <- cr_model2()
  expect_setequal(get_process_groups(mod2)$group,
                  c("ECOA", "EGAS", "SUP_COA", "SUP_GAS", "TRD"))
  # the error must NAME the families -- that is the forcing function: it makes
  # you look at get_process_groups() before deciding
  expect_error(aggregate_model_regions(mod2, level = "zone", clusters = 3),
               "ECOA")
  expect_error(aggregate_model_regions(mod2, level = "zone", clusters = 3),
               "EGAS")
  expect_error(aggregate_model_regions(mod2, level = "zone", clusters = 3),
               "get_process_groups")
  # with one family there is nothing to be ambiguous about. cr_graded() is
  # that model: one technology and no supply, so nothing competes for the `k`.
  expect_no_error(aggregate_model_regions(cr_graded(), level = "zone",
                                          clusters = 3, verbose = FALSE))
  # a name that is not a family is an error too, not a silent no-op
  expect_error(aggregate_model_regions(mod2, level = "zone",
                                       clusters = list(ECOAL = 3)),
               "not a process group")
})

# Covers the R API: aggregate_model_regions(clusters=)
test_that("a family left out of `clusters` is averaged, not clustered", {
  skip_if_no_clustering()
  mod2 <- cr_model2()
  m <- aggregate_model_regions(mod2, level = "zone", clusters = list(ECOA = 3),
                               verbose = FALSE)
  expect_equal(names(model_clusters(m)), "ECOA")
  # ECOA became one clustered object ...
  expect_equal(nrow(getObject(m, name = "ECOA", drop = TRUE)@cluster), 3L)
  # ... and EGAS came through the plain path, one object per region, unclustered
  expect_true(all(paste0("EGAS_", cr_regions) %in% names(m@data[[1]]@data)))
  expect_equal(nrow(getObject(m, name = "EGAS_C1", drop = TRUE)@cluster), 0L)
  # identical to what the plain aggregation gives it
  plain <- aggregate_model_regions(mod2, level = "zone", verbose = FALSE)
  expect_equal(getObject(m, name = "EGAS_C1", drop = TRUE)@invcost,
               getObject(plain, name = "EGAS_C1", drop = TRUE)@invcost)
})

# Covers the R API: aggregate_model_regions(clusters=), process_cluster_sweep()
test_that("each family is clustered on its OWN parameters", {
  skip_if_no_clustering()
  mod2 <- cr_model2()
  m <- aggregate_model_regions(
    mod2, level = "zone", clusters = list(ECOA = 3, EGAS = 3), verbose = FALSE)
  cl <- model_clusters(m)
  split_zone <- function(xw) {
    n <- tapply(xw$cluster, xw$target, function(x) length(unique(x)))
    names(n)[which.max(n)]
  }
  # coal's spread is inside WEST, gas's inside CENTRAL: at k = 3 each family
  # keeps its own zone's two archetypes and merges the other zone
  expect_equal(split_zone(cl$ECOA$crosswalk), "WEST")
  expect_equal(split_zone(cl$EGAS$crosswalk), "CENTRAL")

  # and the sweeps disagree about where the structure is, which is why k is
  # named per family
  sw_coa <- process_cluster_sweep(mod2, "ECOA", level = "zone")
  sw_gas <- process_cluster_sweep(mod2, "EGAS", level = "zone")
  expect_equal(sw_coa$k, 2:4)
  expect_false(isTRUE(all.equal(sw_coa$within, sw_gas$within)))
  expect_true("sizes" %in% names(sw_coa))

  # choosing the features changes that family's table and nothing else
  sw_inv <- process_cluster_sweep(mod2, "ECOA", level = "zone",
                               features = "invcost")
  sw_eff <- process_cluster_sweep(mod2, "ECOA", level = "zone", features = "eff")
  expect_false(isTRUE(all.equal(sw_inv$within, sw_eff$within)))
})

# -- two cluster dimensions --------------------------------------------------

# Covers the R API: aggregate_model_regions(clusters=)
test_that("a pre-existing cluster dimension composes, it is not averaged away", {
  skip_if_no_clustering()
  t <- getObject(
    aggregate_model_regions(cr_graded(), level = "zone", clusters = 4,
                            verbose = FALSE),
    name = "EWIN", drop = TRUE)

  # eight variants in, eight out -- and the VALUES must be the originals, since
  # the count alone would pass on a wrong pairing
  expect_equal(nrow(t@cluster), 8L)
  got <- t@invcost
  expect_setequal(got$cluster,
                  paste(rep(c("GOOD", "POOR"), each = 4),
                        c("W1", "W2", "C1", "C2"), sep = "_"))
  expect_equal(got$invcost[match(paste0("GOOD_", c("W1", "W2", "C1", "C2")),
                                 got$cluster)],
               c(1200, 1500, 1250, 1240))
  expect_equal(got$invcost[match(paste0("POOR_", c("W1", "W2", "C1", "C2")),
                                 got$cluster)],
               c(900, 1100, 950, 940))

  # the grade leads, so it stays readable in solved output, and every label is
  # a legal name
  expect_true(all(vapply(t@cluster$cluster,
                         function(x) isTRUE(check_name(x)), logical(1))))
  # a region-less row keyed by a grade applies to every cluster of that grade:
  # carried through bare it would match none, dropped to NA the two grades'
  # af.up would collide
  expect_equal(nrow(t@af), 8L)
  expect_equal(unique(t@af$af.up[startsWith(t@af$cluster, "GOOD")]), 0.45)
  expect_equal(unique(t@af$af.up[startsWith(t@af$cluster, "POOR")]), 0.25)

  # the features the clustering saw are per (grade, region), not "the region's
  # invcost" -- whichever grade sorted first
  ft <- energyRt:::.cl_feature_table(
    list(getObject(cr_graded(), name = "EWIN", drop = TRUE)), cr_regions, NULL)
  expect_setequal(setdiff(names(ft), "region"),
                  c("invcost_GOOD", "invcost_POOR"))
})

# Covers the R API: aggregate_model_regions(clusters=)
test_that("a cluster of one region is labelled with that region", {
  skip_if_no_clustering()
  m <- aggregate_model_regions(cr_model(), level = "zone", clusters = list(ECOA = 4),
                               verbose = FALSE)
  expect_setequal(getObject(m, name = "ECOA", drop = TRUE)@cluster$cluster,
                  cr_regions)
  # several regions join, and `desc` carries the membership whatever the label
  m2 <- aggregate_model_regions(cr_model(), level = "zone", clusters = list(ECOA = 2),
                                verbose = FALSE)
  cl <- getObject(m2, name = "ECOA", drop = TRUE)@cluster
  expect_true(all(grepl("+", cl$desc, fixed = TRUE)))
  expect_true(all(vapply(cl$cluster, function(x) isTRUE(check_name(x)),
                         logical(1))))
})

# @covers vObjective depth=S backends=glpk
# Covers the R API: aggregate_model_regions(clusters=)
test_that("as = 'objects' is the same problem in a different shape", {
  skip_if_no_clustering()
  mod <- cr_model()
  cls <- aggregate_model_regions(mod, level = "zone", clusters = list(ECOA = 4),
                                 verbose = FALSE)
  obj <- aggregate_model_regions(mod, level = "zone", clusters = list(ECOA = 4),
                                 as = "objects", verbose = FALSE)
  # one object per (family x cluster), each keeping its own clusters -- here
  # none, so the cluster dimension is gone rather than trivial
  expect_true(all(paste0("ECOA_", cr_regions) %in% names(obj@data[[1]]@data)))
  w1 <- getObject(obj, name = "ECOA_W1", drop = TRUE)
  expect_equal(nrow(w1@cluster), 0L)
  expect_equal(unique(w1@invcost$region), "WEST")

  skip_if_no_solver()
  expect_equal(cr_solve(obj, "CRASOBJ"), cr_solve(cls, "CRASCLS"),
               tolerance = 1e-9)
})

# Covers the R API: aggregate_model_regions(clusters=)
test_that("as = 'objects' keeps a family's own clusters", {
  skip_if_no_clustering()
  m <- aggregate_model_regions(cr_graded(), level = "zone", clusters = 4,
                               as = "objects", verbose = FALSE)
  expect_true(all(paste0("EWIN_", cr_regions) %in% names(m@data[[1]]@data)))
  t <- getObject(m, name = "EWIN_W1", drop = TRUE)
  # the region grouping is in the object NAME, so only the grade is left as a
  # cluster -- the composition is undone, not carried twice
  expect_setequal(t@cluster$cluster, c("GOOD", "POOR"))
  expect_equal(t@invcost$invcost[match(c("GOOD", "POOR"), t@invcost$cluster)],
               c(1200, 900))
})

# -- seeing the method -------------------------------------------------------

# Covers the R API: model_clusters()
test_that("the clustering comes back as a geoscale with a `cluster` frame", {
  skip_if_no_clustering()
  m <- aggregate_model_regions(cr_model(), level = "zone", clusters = list(ECOA = 3),
                               verbose = FALSE)
  gs <- model_clusters(m)$ECOA$geoscale
  expect_true(is_geoscale(gs))
  # nation > zone > cluster > region, so plot_geoscale(geoframe = "cluster")
  # and the icicle both work with no new plotting code
  expect_equal(geoscales::geoscale_geoframes(gs),
               c("nation", "zone", "cluster", "region"))
  lt <- as.data.frame(geoscales::geoscale_leaftable(gs))
  expect_setequal(lt$region, cr_regions)
  expect_equal(anyDuplicated(lt$region), 0L)
  # a cluster sits in exactly one zone -- clusters never straddle `level`
  expect_true(all(tapply(lt$zone, lt$cluster,
                         function(z) length(unique(z))) == 1L))
  # and the model's OWN geoscale is untouched: its atoms must keep matching its
  # regions or the spatial-mode classifier mis-sorts it
  expect_equal(geoscales::geoscale_geoframes(m@config@geoscale, finest = TRUE),
               "zone")

  # both documented views must actually render. The icicle sizes its bands by a
  # weight column and refuses outright when the geoscale declares none, so the
  # source's weights have to come along.
  expect_equal(geoscales::geoscale_weights(gs),
               geoscales::geoscale_weights(cr_geoscale()))
  skip_if_not_installed("ggplot2")
  expect_s3_class(plot_geoscale(gs, type = "map", geoframe = "cluster"),
                  "ggplot")
  expect_s3_class(plot_geoscale(gs, type = "icicle"), "ggplot")
})

# -- three key dimensions at once --------------------------------------------

# A fleet can arrive with BOTH a cluster dimension (site grade) and vintages
# (build years) before its regions are coarsened. Region grouping then has to
# compose onto the first and leave the second alone: averaging across grades
# was the bug above, averaging across vintages would be the same bug one column
# over, and neither shows up in the row count unless the values are asserted.
cr_graded_vintage <- function() {
  grid <- expand.grid(region = cr_regions, cluster = c("GOOD", "POOR"),
                      vintage = c("V1", "V2"), stringsAsFactors = FALSE)
  # encoded so a wrong pairing is visible in the value itself
  grid$invcost <- 1000 +
    match(grid$region, cr_regions) * 10 -
    100 * (grid$cluster == "POOR") -
    50 * (grid$vintage == "V2")

  ewin <- newTechnology("EWIN",
    region = cr_regions,
    cluster = data.frame(cluster = c("GOOD", "POOR")),
    vintage = data.frame(vintage = c("V1", "V2"),
                         start = c(2025, 2030), end = c(2029, 2050),
                         olife = 25),
    input = list(comm = "WIN", unit = "PJ"),
    output = list(comm = "ELC", unit = "PJ"),
    ceff = data.frame(comm = "WIN", cinp2use = 1),
    invcost = grid[, c("region", "cluster", "vintage", "invcost")])

  mod <- newModel("CRGV", region = cr_regions,
                  calendar = topia$modules$calendars$topia_seasons,
                  horizon = newHorizon(2025, 2050), discount = 0.05,
                  data = newRepository("crgv", list(
                    newCommodity("WIN", unit = "PJ"),
                    newCommodity("ELC", unit = "PJ", timeframe = "HOUR"),
                    ewin)))
  setGeoscale(mod, cr_geoscale())
}

# @covers vObjective
# Covers the R API: aggregate_model_regions(clusters=)
test_that("cluster and vintage compose independently of the region grouping", {
  skip_if_no_clustering()
  t <- getObject(
    aggregate_model_regions(cr_graded_vintage(), level = "zone", clusters = 4,
                            verbose = FALSE),
    name = "EWIN", drop = TRUE)

  # 4 regions x 2 grades x 2 vintages, nothing averaged at 1:1
  expect_equal(nrow(t@invcost), 16L)
  expect_setequal(as.character(t@invcost$vintage), c("V1", "V2"))
  expect_equal(nrow(t@vintage), 2L)
  expect_setequal(t@cluster$cluster,
                  paste(rep(c("GOOD", "POOR"), each = 4), cr_regions,
                        sep = "_"))

  # the values, not just the shape: a wrong pairing keeps the count and moves
  # the numbers
  got <- t@invcost
  key <- paste(got$cluster, got$vintage)
  expect_equal(got$invcost[match("GOOD_W1 V1", key)], 1010)
  expect_equal(got$invcost[match("GOOD_W1 V2", key)], 960)
  expect_equal(got$invcost[match("POOR_W1 V1", key)], 910)
  expect_equal(got$invcost[match("POOR_C2 V2", key)], 890)

  # each cluster still sits in the zone its member came from
  expect_equal(unique(got$region[startsWith(as.character(got$cluster), "GOOD_W")]),
               "WEST")
})

# Covers the R API: aggregate_model_regions(clusters=)
test_that("merging regions averages WITHIN a (cluster, vintage), not across", {
  skip_if_no_clustering()
  t <- getObject(
    aggregate_model_regions(cr_graded_vintage(), level = "zone", clusters = 2,
                            verbose = FALSE),
    name = "EWIN", drop = TRUE)

  # one cluster per zone per grade, both vintages kept: 2 x 2 x 2
  expect_equal(nrow(t@invcost), 8L)
  expect_setequal(as.character(t@invcost$vintage), c("V1", "V2"))

  got <- t@invcost
  key <- paste(got$cluster, got$vintage)
  # W1 and W2 merge; GOOD V1 is the mean of 1010 and 1020, and it must NOT be
  # contaminated by the POOR rows (910, 920) or by V2 (960, 970)
  expect_equal(got$invcost[match("GOOD_W1_W2 V1", key)], 1015)
  expect_equal(got$invcost[match("GOOD_W1_W2 V2", key)], 965)
  expect_equal(got$invcost[match("POOR_W1_W2 V1", key)], 915)
  expect_equal(got$invcost[match("POOR_C1_C2 V2", key)], 885)

  # the grade leads the label, then the regions that joined
  expect_true(all(vapply(t@cluster$cluster,
                         function(x) isTRUE(check_name(x)), logical(1))))
})

# -- beyond technologies -----------------------------------------------------

# `supply`, `storage`, `import` and `export` carry a `@cluster` slot, so a
# region grouping can become sub-processes on them too. `demand` cannot (no
# slot: a demand is a requirement, not a process with archetypes) and `trade`
# cannot (no `@region` slot: its regions are src/dst pairs, so grouping them
# means clustering corridors, a different operation).
sup_cost <- c(W1 = 2.0, W2 = 5.0, C1 = 2.2, C2 = 4.8)
sup_ava  <- c(W1 = 100, W2 = 40,  C1 = 90,  C2 = 50)

# the SPANNING shape: one object whose slot carries a `region` column. This is
# the shape where plain aggregation actually loses the tranches.
cr_supply_span <- function() {
  s <- newSupply(name = "SUP1", commodity = "COA", unit = "PJ",
                 region = cr_regions,
                 supply = data.frame(region = cr_regions,
                                     cost = unname(sup_cost),
                                     ava.up = unname(sup_ava)))
  mod <- newModel("CRS", region = cr_regions,
                  calendar = topia$modules$calendars$topia_seasons,
                  horizon = newHorizon(2025), discount = 0.05,
                  data = newRepository("crs", list(
                    newCommodity("COA", unit = "PJ"),
                    newCommodity("ELC", unit = "PJ", timeframe = "HOUR"), s)))
  setGeoscale(mod, cr_geoscale())
}

# Covers the R API: get_process_groups()
test_that("supply, import and export are groupable; demand and trade are not", {
  skip_if_no_clustering()
  g <- get_process_groups(cr_supply_span())
  expect_equal(nrow(g), 1L)
  expect_equal(g$class, "supply")
  # the class LEADS the signature, so a supply never groups with a technology
  # that happens to touch the same commodity
  expect_match(g$signature, "comm:COA")

  expect_true(all(c("supply", "import", "export", "storage", "technology",
                    "trade") %in% energyRt:::.cl_groupable))
  # `demand` has no @cluster slot to hold variants and `weather` is grouped
  # with the processes that use it, not on its own
  expect_false(any(c("demand", "weather") %in% energyRt:::.cl_groupable))
  # trade is groupable but takes a DIFFERENT road: its parts become separate
  # objects, because a trade's @cluster is a loss tranche
  expect_equal(get_process_groups(cr_model())$class[
    get_process_groups(cr_model())$group == "TRD"], "trade")
})

# Covers the R API: aggregate_model_regions(clusters=)
test_that("clustering a supply keeps the tranches plain aggregation averages", {
  skip_if_no_clustering()
  mod <- cr_supply_span()

  # what plain aggregation does: one averaged row per zone, tranches gone
  plain <- getObject(aggregate_model_regions(mod, level = "zone",
                                             verbose = FALSE),
                     name = "SUP1", drop = TRUE)@supply
  expect_equal(nrow(plain), 2L)
  expect_equal(plain$cost[plain$region == "WEST"],
               (2.0 * 100 + 5.0 * 40) / 140)        # capacity-weighted
  expect_equal(plain$ava.up[plain$region == "WEST"], 140)   # extensive: summed

  # 1:1 clusters: every tranche survives with its own cost and quantity
  o <- getObject(aggregate_model_regions(mod, level = "zone",
                                         clusters = list(SUP1 = 4),
                                         verbose = FALSE),
                 name = "SUP1", drop = TRUE)
  expect_equal(nrow(o@cluster), 4L)
  expect_setequal(o@cluster$cluster, cr_regions)
  got <- o@supply
  expect_equal(got$cost[match("W1", got$cluster)], 2.0)
  expect_equal(got$cost[match("W2", got$cluster)], 5.0)
  expect_equal(got$ava.up[match("W2", got$cluster)], 40)

  # k = 2 is the plain answer again, now carried as one cluster per zone
  o2 <- getObject(aggregate_model_regions(mod, level = "zone",
                                          clusters = list(SUP1 = 2),
                                          verbose = FALSE),
                  name = "SUP1", drop = TRUE)
  g2 <- o2@supply
  expect_equal(g2$cost[match("W1_W2", g2$cluster)], (2.0 * 100 + 5.0 * 40) / 140)
  expect_equal(g2$ava.up[match("W1_W2", g2$cluster)], 140)
})

# Covers the R API: aggregate_model_regions(clusters=)
test_that("a storage clusters with its part-keyed columns intact", {
  skip_if_no_clustering()
  inv <- c(W1 = 300, W2 = 900, C1 = 320, C2 = 880)
  cap <- c(W1 = 10, W2 = 30, C1 = 12, C2 = 28)
  stg <- lapply(cr_regions, function(r)
    newStorage(name = paste0("STG_", r), commodity = "ELC", region = r,
               invcost = data.frame(region = r, stg.invcost = inv[[r]]),
               capacity = data.frame(region = r, stg.cap.up = cap[[r]]),
               seff = data.frame(region = r, stgeff = 0.9)))
  mod <- newModel("CRT", region = cr_regions,
                  calendar = topia$modules$calendars$topia_seasons,
                  horizon = newHorizon(2025), discount = 0.05,
                  data = newRepository("crt", c(
                    list(newCommodity("ELC", unit = "PJ",
                                      timeframe = "HOUR")), stg)))
  mod <- setGeoscale(mod, cr_geoscale())

  g <- get_process_groups(mod)
  expect_equal(g$class, "storage")
  expect_equal(g$n, 4L)

  o <- getObject(aggregate_model_regions(mod, level = "zone",
                                         clusters = list(STG = 4),
                                         verbose = FALSE),
                 name = "STG", drop = TRUE)
  expect_equal(nrow(o@cluster), 4L)
  expect_equal(o@invcost$stg.invcost[match("W2", o@invcost$cluster)], 900)

  # at k = 2 the part-prefixed columns obey their own kind: a capacity SUMS,
  # a cost takes the capacity-weighted mean
  o2 <- getObject(aggregate_model_regions(mod, level = "zone",
                                          clusters = list(STG = 2),
                                          verbose = FALSE),
                  name = "STG", drop = TRUE)
  expect_equal(o2@capacity$stg.cap.up[match("W1_W2", o2@capacity$cluster)], 40)
  expect_equal(o2@invcost$stg.invcost[match("W1_W2", o2@invcost$cluster)],
               (300 * 10 + 900 * 30) / 40)
})
