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
