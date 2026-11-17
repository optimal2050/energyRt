# Storage columns of the storing part carry the `stg.` prefix, and the per-part
# efficiencies and variable costs are dotted. The old spellings are renamed
# with a deprecation warning in newStorage()/update(), silently in a
# deserialized object, and in a process spec.

scn_new <- function() {
  newStorage("S", commodity = "ELC",
    seff = data.frame(stg.eff = 0.99, inp.eff = 0.9, out.eff = 0.95),
    af = data.frame(stg.af.lo = 0.1, stg.af.up = 0.9),
    varom = data.frame(stg.varom = 0.1, inp.varom = 0.2, out.varom = 0.3),
    target = data.frame(stg.af.target = 0.5, stg.af.cost = 10),
    weather = data.frame(weather = "W", stg.waf.up = 1))
}

test_that("old storage column names are renamed with a warning", {
  expect_warning(
    old <- newStorage("S", commodity = "ELC",
      seff = data.frame(stgeff = 0.99, inpeff = 0.9, outeff = 0.95),
      af = data.frame(af.lo = 0.1, af.up = 0.9),
      varom = data.frame(stgcost = 0.1, inpcost = 0.2, outcost = 0.3),
      target = data.frame(af.target = 0.5, cost = 10),
      weather = data.frame(weather = "W", waf.up = 1)),
    "`@seff\\$stgeff` is now `stg.eff`")
  new <- scn_new()
  for (sl in c("seff", "af", "varom", "target", "weather")) {
    expect_identical(methods::slot(old, sl), methods::slot(new, sl))
  }
})

test_that("both spellings of one column are an error", {
  expect_error(newStorage("S", seff = data.frame(stgeff = 1, stg.eff = 1)),
               "both the old and the new spelling")
})

test_that("a deserialized storage with old names is upgraded on use", {
  new <- scn_new()
  old <- new
  s <- old@seff; names(s)[names(s) == "stg.eff"] <- "stgeff"
  attr(old, "seff") <- s
  t <- old@target; names(t)[names(t) == "stg.af.cost"] <- "cost"
  attr(old, "target") <- t
  expect_true(energyRt:::.storage_needs_upgrade(old))
  up <- energyRt:::.upgrade_storage_inp2stg(old)
  expect_false(energyRt:::.storage_needs_upgrade(up))
  expect_identical(up@seff, new@seff)
  expect_identical(up@target, new@target)
  # update() upgrades the object before applying its arguments
  up2 <- expect_no_warning(update(old, af = data.frame(stg.af.fx = 0.5)))
  expect_identical(up2@seff, new@seff)
})

test_that("a storage spec with old names is renamed on read", {
  spec <- process_to_spec(scn_new())
  sec <- names(spec)[vapply(spec, function(x) {
    is.list(x) && length(x) && "stg.eff" %in% names(x[[1]])
  }, logical(1))]
  expect_length(sec, 1)
  names(spec[[sec]][[1]])[names(spec[[sec]][[1]]) == "stg.eff"] <- "stgeff"
  expect_warning(rs <- read_process_spec(spec), "`stgeff` is now `stg.eff`")
  expect_true("stg.eff" %in% names(rs[[sec]][[1]]))
})
