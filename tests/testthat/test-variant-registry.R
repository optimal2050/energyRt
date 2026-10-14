# =========================================================================== #
# `.variant_classes` must stay in step with the classes it describes.
#
# The registry's `slots` entry lists the slots whose rows are SELECTED per
# variant. The two ways it can drift are both silent:
#
#   * a slot listed that no longer exists -- inert, but it documents a slot
#     that was renamed away (`storage`'s `charge`);
#   * a slot carrying `vintage`/`cluster` columns that is NOT listed -- the
#     rows are copied identically to every variant, so a user's per-variant
#     data looks right and does nothing. `storage@startLevel` and
#     `storage@inp2out` were in this state; the second is the C-rate coupling,
#     so a per-cluster ratio silently took whichever row came first.
#
# The registry's own comment states the rule for supply ("A slot left out is
# copied identically to every step, so an N-step curve would silently hold N
# times the resource"), so these tests just hold every class to it by
# re-deriving the list from the class definition.
#
# @covers depth=S backends=none
# =========================================================================== #

# data.frame slots carrying a variant selector. `@cluster` and `@vintage` are
# the DECLARATIONS -- `@vintage` is itself filtered (each variant keeps its own
# window) while `@cluster` is handled separately by the expansion.
vr_selector_slots <- function(cl) {
  o <- methods::new(cl)
  Filter(function(s) {
    v <- methods::slot(o, s)
    is.data.frame(v) && any(c("vintage", "cluster") %in% colnames(v)) &&
      !identical(s, "cluster")
  }, methods::slotNames(cl))
}

test_that("every registered variant slot exists on its class", {
  reg <- energyRt:::.variant_classes
  for (cl in names(reg)) {
    stale <- setdiff(reg[[cl]]$slots, methods::slotNames(cl))
    expect_equal(stale, character(0),
                 label = paste0(cl, ": registry lists slot(s) the class does not have"))
  }
})

test_that("every slot with a variant selector is registered for filtering", {
  reg <- energyRt:::.variant_classes
  for (cl in names(reg)) {
    missing <- setdiff(vr_selector_slots(cl), reg[[cl]]$slots)
    expect_equal(missing, character(0),
                 label = paste0(cl, ": slot(s) carry vintage/cluster columns but ",
                                "are not filtered per variant"))
  }
})

test_that("a class's selector columns match the axes it declares", {
  reg <- energyRt:::.variant_classes
  for (cl in names(reg)) {
    dims <- reg[[cl]]$dims
    o <- methods::new(cl)
    for (s in reg[[cl]]$slots) {
      cols <- colnames(methods::slot(o, s))
      # a filtered slot must carry a column for at least one declared axis,
      # or there is nothing to select on
      expect_true(any(dims %in% cols),
                  label = paste0(cl, "@", s, " has none of the axes (",
                                 paste(dims, collapse = "/"), ")"))
    }
  }
})

# --------------------------------------------------------------------------- #
# The behaviour the registry fix buys: the two slots that were unfiltered now
# select per variant.
# --------------------------------------------------------------------------- #

test_that("storage inp2out and startLevel are selected per cluster", {
  grades <- c("BIG", "SMALL")
  stg <- newStorage(
    "STG", commodity = "ELC",
    cluster = data.frame(cluster = grades, region = NA_character_,
                         order = 1:2),
    # the C-rate coupling, per grade -- the case that silently collapsed
    inp2out = data.frame(cluster = grades, inp2out = c(0.25, 0.5)),
    startLevel = data.frame(cluster = grades, startLevel = c(0.1, 0.9)),
    invcost = list(out.invcost = 1))

  expanded <- energyRt:::.expand_one_process(stg)$objects
  nm <- vapply(expanded, function(o) o@name, character(1))
  expect_setequal(nm, c("STG_CLBIG", "STG_CLSMALL"))

  big <- expanded[[which(nm == "STG_CLBIG")]]
  small <- expanded[[which(nm == "STG_CLSMALL")]]
  # a bare ratio normalises into the `.fx` column (`.norm_ratio()`)
  expect_equal(nrow(big@inp2out), 1L)
  expect_equal(big@inp2out$inp2out.fx, 0.25)
  expect_equal(small@inp2out$inp2out.fx, 0.5)
  expect_equal(big@startLevel$startLevel, 0.1)
  expect_equal(small@startLevel$startLevel, 0.9)
})

test_that("a row with no selector still reaches every variant", {
  grades <- c("BIG", "SMALL")
  stg <- newStorage(
    "STG", commodity = "ELC",
    cluster = data.frame(cluster = grades, region = NA_character_),
    inp2out = data.frame(inp2out = 0.3),          # no cluster column
    invcost = list(out.invcost = 1))
  expanded <- energyRt:::.expand_one_process(stg)$objects
  for (o in expanded) {
    expect_equal(nrow(o@inp2out), 1L)
    expect_equal(o@inp2out$inp2out.fx, 0.3)
  }
})

# --------------------------------------------------------------------------- #
# A roster must be DECLARED. Labels used to be harvested out of the data slots,
# guarded by a cross-slot consistency check; the guard could not see a single
# label or a single cluster-carrying slot, and a misspelling then became a
# variant with defaults for everything it did not carry -- no investment cost
# and no capacity bound, i.e. free and unlimited. The roster is also the only
# place `region` and `order` can be stated.
# --------------------------------------------------------------------------- #

test_that("the cluster axis requires a roster, naming slots and the fix", {
  t1 <- newTechnology(
    name = "T1", output = data.frame(comm = "ELC"),
    invcost = data.frame(cluster = c("01", "02"), invcost = c(600, 900)),
    capacity = data.frame(cluster = c("01", "02"), cap.up = 10),
    cap2act = 1)
  err <- tryCatch(energyRt:::.variant_validate(t1),
                  error = function(e) conditionMessage(e))
  expect_match(err, "declares no `cluster` roster")
  expect_match(err, '"01", "02"')
  expect_match(err, "invcost")
  expect_match(err, "capacity")
  expect_match(err, "cluster = data.frame")
})

test_that("the vintage axis requires a roster too", {
  t2 <- newTechnology(
    name = "T2", output = data.frame(comm = "ELC"),
    invcost = data.frame(vintage = c("2025", "2035"), invcost = c(10, 8)),
    capacity = data.frame(cap.up = 1), cap2act = 1)
  err <- tryCatch(energyRt:::.variant_validate(t2),
                  error = function(e) conditionMessage(e))
  expect_match(err, "declares no `vintage` roster")
  expect_match(err, "vintage = data.frame")
})

test_that("an undeclared vintage label is refused like a cluster one", {
  t3 <- newTechnology(
    name = "T3", output = data.frame(comm = "ELC"),
    vintage = data.frame(vintage = "2025", olife = 30L),
    invcost = data.frame(vintage = c("2025", "2035"), invcost = c(10, 8)),
    cap2act = 1)
  expect_error(energyRt:::.variant_validate(t3), "Undeclared vintage label")
})

test_that("a declared roster needs no window, and still expands", {
  t4 <- newTechnology(
    name = "T4", output = data.frame(comm = "ELC"),
    cluster = c("01", "02"),
    vintage = data.frame(vintage = c("2025", "2035")),   # no start/end/olife
    invcost = data.frame(cluster = c("01", "02"), invcost = c(600, 900)),
    capacity = data.frame(cap.up = 10), cap2act = 1)
  expect_silent(energyRt:::.variant_validate(t4))
  objs <- energyRt:::.expand_one_process(t4)$objects
  expect_setequal(vapply(objs, function(o) o@name, character(1)),
                  c("T4_VIN2025_CL01", "T4_VIN2035_CL01",
                    "T4_VIN2025_CL02", "T4_VIN2035_CL02"))
})

test_that("asSupplyCurve() builds a roster, so its output validates", {
  sup <- newSupply(
    name = "SUP", commodity = "COA",
    supply = data.frame(ava.up = 10, cost = 2))
  curve <- asSupplyCurve(sup, range = c(1, 3), nsteps = 3)
  expect_equal(nrow(curve@cluster), 3L)
  expect_silent(energyRt:::.variant_validate(curve))
})
