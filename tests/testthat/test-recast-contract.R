# =========================================================================== #
# The declared recast contract.
#
# `.defRecast` is generated from the `recast:` blocks of data-raw/classes.yml.
# It starts as a faithful snapshot of what `.agg_rules()` infers, so that the
# inference can be retired without changing any answer; every later departure
# from it is then a deliberate, tested decision rather than a side effect.
# =========================================================================== #

# Deliberate departures from the inference the contract was seeded from.
# Each has its own test below; the list exists so that a new departure cannot
# appear unnoticed, and so that a stale entry is caught too.
rc_departures <- c("technologyaframpup", "technologyaframpdown",
                   "storageseffstgeff", "tradetradereactance",
                   "tradetraderesistance")

test_that("the contract reproduces the inference apart from pinned departures", {
  d <- energyRt:::.defRecast
  mismatch <- NULL
  for (k in unique(paste(d$class, d$slot, sep = "\r"))) {
    p <- strsplit(k, "\r", fixed = TRUE)[[1]]
    rows <- d[d$class == p[1] & d$slot == p[2], ]
    inferred <- energyRt:::.agg_rules(rows$column, p[1])
    key <- paste(rows$class, rows$slot, rows$column, sep = "")
    bad <- which(unname(inferred) != rows$space & !(key %in% rc_departures))
    if (length(bad)) {
      mismatch <- rbind(mismatch, data.frame(
        class = p[1], slot = p[2], column = rows$column[bad],
        declared = rows$space[bad], inferred = unname(inferred)[bad]))
    }
  }
  expect_null(mismatch)
})

test_that("every pinned departure really does depart", {
  d <- energyRt:::.defRecast
  key <- paste(d$class, d$slot, d$column, sep = "")
  for (k in rc_departures) {
    i <- match(k, key)
    expect_false(is.na(i), label = paste("pinned but not declared:", k))
    p <- strsplit(k, "", fixed = TRUE)[[1]]
    # the inference must still disagree, or the pin is stale
    expect_false(identical(
      unname(energyRt:::.agg_rules(p[3], p[1])), d$space[i]),
      label = paste("pin is stale:", k))
  }
})

test_that("every column on an axis carries a declaration", {
  cand <- c(testthat::test_path("..", "..", "data-raw", "classes.yml"),
            file.path("data-raw", "classes.yml"))
  hit <- cand[file.exists(cand)]
  if (!length(hit)) testthat::skip("data-raw/classes.yml not available")
  y <- yaml::read_yaml(hit[1])
  ID <- c("region", "src", "dst", "year", "vintage", "cluster", "timeslice",
          "comm", "acomm", "group", "desc", "property", "unit", "comment",
          "weather", "transform", "process", "cell", "name")
  missing <- NULL
  for (cn in names(y$class)) for (sn in names(y$class[[cn]])) {
    s <- y$class[[cn]][[sn]]
    if (!is.list(s) || is.null(s$columns)) next
    cols <- names(s$columns)
    on_axis <- any(c("region", "src", "dst") %in% cols) || "timeslice" %in% cols
    if (!on_axis) next
    vals <- setdiff(cols, ID)
    if (!length(vals)) next
    gap <- energyRt:::.recast_undeclared(cn, sn, vals)
    if (length(gap)) missing <- c(missing, paste0(cn, "@", sn, "$", gap))
  }
  expect_equal(sort(as.character(missing)), character(0))
})

test_that("a rule is read back per column and per axis", {
  expect_equal(unname(energyRt:::.get_recast_rule(
    "storage", "startLevel", "startLevel", "space")), "sum")
  expect_equal(unname(energyRt:::.get_recast_rule(
    "technology", "invcost", "payback", "space")), "copy")
  expect_equal(unname(energyRt:::.get_recast_rule(
    "tax", "tax", "bal", "space")), "copy")
  expect_equal(unname(energyRt:::.get_recast_rule(
    "weather", "weather", "wval", "space")), "weighted_mean")
  # not indexed on time, so declared NA rather than undeclared
  expect_true(is.na(energyRt:::.get_recast_rule(
    "storage", "startLevel", "startLevel", "time")))
  expect_equal(energyRt:::.recast_undeclared(
    "storage", "startLevel", "startLevel"), character(0))
})

test_that("an undeclared column is named rather than guessed", {
  expect_true(is.na(energyRt:::.get_recast_rule(
    "technology", "af", "no_such_column", "space")))
  expect_equal(energyRt:::.recast_undeclared(
    "technology", "af", c("af.up", "no_such_column")), "no_such_column")
})
