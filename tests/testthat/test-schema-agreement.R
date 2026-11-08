# =========================================================================== #
# Schema agreement: one class definition, five places that describe it.
#
# The effective schema
# is distributed among
#
#   1. `setClass()` representations      -- the authority on what slots exist;
#   2. class prototypes                  -- the authority on data.frame columns;
#   3. `data-raw/classes-docs.yml`            -- the documentation source;
#   4. the generated `.classes` catalogue in `R/sysdata.rda` -- what
#      `get_slot_doc()` reads, so what the manual pages actually say;
#   5. constructor signatures            -- what a user can set.
#
# Nothing compared them, so they drifted: `costs@defVal` existed in the class
# and nowhere else, and `horizon@base_year` existed everywhere except the
# generated catalogue, which is what made it undocumentable.
#
# The drifts this test pins deliberately are each an exact set, so one shrinks
# or grows only by a visible edit here.
#
# Covers no model vocabulary -- it is a schema test, not a parameter test, so
# it carries no `@covers` tag (see tests/README.md).
# =========================================================================== #

# `data-raw/` is not installed, so an installed-package run has no source of
# truth to compare against. Same fallback shape as helper-mapping.R.
sa_yaml_path <- function() {
  cand <- c(testthat::test_path("..", "..", "data-raw", "classes-docs.yml"),
            file.path("data-raw", "classes-docs.yml"))
  hit <- cand[file.exists(cand)]
  if (!length(hit)) testthat::skip("data-raw/classes-docs.yml not available")
  hit[1]
}

# An INDEPENDENT read of the YAML: deliberately not DATASET.R's `yaml_to_df()`,
# because comparing a generated artefact against the generator that made it
# would only prove the generator is deterministic.
sa_yaml <- function() {
  skip_if_not_installed("yaml")
  yaml::read_yaml(sa_yaml_path())$class
}

sa_catalogue <- function() {
  get(".classes", envir = asNamespace("energyRt"))
}

# Slots a class declares itself, excluding the ones it inherits: `settings`
# `contains` `config`, and documenting config's slots a second time under
# `settings` would be the duplication this test exists to prevent.
sa_own_slots <- function(cl) {
  own <- names(methods::getClass(cl)@slots)
  # `extends()` returns an UNNAMED character vector of class names
  parents <- setdiff(methods::extends(cl), cl)
  inherited <- unlist(lapply(parents, function(p) {
    tryCatch(names(methods::getClass(p)@slots), error = function(e) character())
  }))
  setdiff(own, inherited)
}

sa_proto <- function(cl) tryCatch(new(cl), error = function(e) NULL)

# --------------------------------------------------------------------------- #
# 1. the YAML documents real slots, and every own slot is documented

test_that("every documented slot exists in its class", {
  y <- sa_yaml()
  phantom <- character(0)
  for (cl in names(y)) {
    s4 <- tryCatch(slotNames(cl), error = function(e) character())
    extra <- setdiff(names(y[[cl]]), s4)
    if (length(extra)) phantom <- c(phantom, paste0(cl, "@", extra))
  }
  expect_equal(phantom, character(0))
})

test_that("every slot a class declares itself is documented", {
  y <- sa_yaml()
  missing <- character(0)
  for (cl in names(y)) {
    own <- tryCatch(sa_own_slots(cl), error = function(e) character())
    gap <- setdiff(own, names(y[[cl]]))
    if (length(gap)) missing <- c(missing, paste0(cl, "@", gap))
  }
  # `costs@defVal` lived here until 2026-11-05; the set is empty and stays so
  expect_equal(missing, character(0))
})

# --------------------------------------------------------------------------- #
# 2. the generated catalogue agrees with the YAML it is generated from

test_that("the generated .classes catalogue is current with the YAML", {
  y <- sa_yaml()
  cat_tbl <- sa_catalogue()
  yaml_keys <- unlist(lapply(names(y), function(cl) {
    unlist(lapply(names(y[[cl]]), function(s) {
      sd <- y[[cl]][[s]]
      if (identical(sd$type, "data.frame") && !is.null(sd$columns)) {
        paste(cl, s, names(sd$columns), sep = "\r")
      } else {
        paste(cl, s, NA, sep = "\r")
      }
    }))
  }))
  cat_keys <- paste(cat_tbl$class, cat_tbl$slotname, cat_tbl$col.name,
                    sep = "\r")
  pretty <- function(k) {
    if (!length(k)) return(character(0))
    p <- do.call(rbind, strsplit(k, "\r", fixed = TRUE))
    ifelse(p[, 3] == "NA", paste0(p[, 1], "@", p[, 2]),
           paste0(p[, 1], "@", p[, 2], "$", p[, 3]))
  }

  # Every YAML slot must reach the baked catalogue: a slot the catalogue does
  # not carry has no `get_slot_doc()` text and so no manual page. `costs@defVal`
  # and `horizon@base_year` were pinned here while `R/sysdata.rda` predated
  # their YAML edits; the rebuild (2026-11-07) closed the gap.
  stale <- sort(pretty(setdiff(yaml_keys, cat_keys)))
  expect_equal(stale, character(0))

  # the other direction must be empty: a catalogue row with no YAML source
  # would be documentation that no one can edit
  expect_equal(pretty(setdiff(cat_keys, yaml_keys)), character(0))
})

test_that("get_slot_doc() resolves every catalogued slot", {
  cat_tbl <- sa_catalogue()
  rows <- unique(cat_tbl[is.na(cat_tbl$col.name), c("class", "slotname")])
  bad <- character(0)
  for (i in seq_len(nrow(rows))) {
    d <- tryCatch(get_slot_doc(rows$class[i], rows$slotname[i]),
                  error = function(e) NULL)
    if (is.null(d) || !length(d) || !nzchar(paste(d, collapse = "")))
      bad <- c(bad, paste0(rows$class[i], "@", rows$slotname[i]))
  }
  expect_equal(bad, character(0))
})

# --------------------------------------------------------------------------- #
# 3. documented columns agree with the prototype that carries them

# data.frame slots the YAML describes as a whole, without naming columns.
# Every one is either DERIVED (built by the constructor from other input, so
# its columns are an implementation detail) or WIDE (one column per model
# parameter, around a hundred of them, which no hand-written YAML should
# mirror). A new entry here is a documentation gap, not a new convention.
sa_undocumented_columns <- c(
  # built by newConstraint() from the equation it is given
  "constraint@for.each", "constraint@rhs",
  # one term of a constraint, built from the variable it sums
  "summand@mult",
  # set from `mult`, see the slot description
  "costs@subset", "costs@mult",
  # derived by newCalendar() from the timetable
  "calendar@timetable", "calendar@timeslice_share", "calendar@timeslice_family",
  "calendar@timeslice_ancestry", "calendar@next_in_timeframe",
  "calendar@next_in_year",
  # derived by newHorizon() from `period` / `intervals`
  "horizon@intervals",
  # one column per model parameter
  "config@defVal", "config@interpolation", "config@debug",
  "settings@defVal", "settings@interpolation", "settings@debug",
  # derived from the model
  "settings@yearFraction", "settings@discount",
  # filled by the solver
  "modOut@solutionLogs"
)

test_that("documented data.frame columns match the prototype", {
  y <- sa_yaml()
  mismatch <- character(0)
  undocumented <- character(0)
  checked <- 0L
  for (cl in names(y)) {
    p <- sa_proto(cl)
    if (is.null(p)) next
    for (s in slotNames(cl)) {
      v <- tryCatch(slot(p, s), error = function(e) NULL)
      if (!is.data.frame(v)) next
      yc <- names(y[[cl]][[s]]$columns)
      if (is.null(yc)) {
        undocumented <- c(undocumented, paste0(cl, "@", s))
        next
      }
      checked <- checked + 1L
      if (!setequal(yc, names(v)))
        mismatch <- c(mismatch, sprintf(
          "%s@%s: prototype-only {%s} yaml-only {%s}", cl, s,
          paste(setdiff(names(v), yc), collapse = ","),
          paste(setdiff(yc, names(v)), collapse = ",")))
    }
  }
  expect_equal(mismatch, character(0))
  # and the exemption list is exactly what it claims to be
  expect_setequal(undocumented, sa_undocumented_columns)
  # a floor, so the check cannot quietly stop checking anything
  expect_gte(checked, 70L)
})

test_that("the column exemptions name real slots", {
  for (k in sa_undocumented_columns) {
    p <- strsplit(k, "@", fixed = TRUE)[[1]]
    expect_true(p[2] %in% slotNames(p[1]), label = k)
  }
})

# --------------------------------------------------------------------------- #
# 4. the constructor crosswalk

# Constructor arguments that are NOT slots: each is folded into a slot, or is a
# construction option rather than stored state.
sa_extra_formals <- list(
  commodity  = c("image", "icon"),                 # folded into @misc
  technology = c("olife", "start", "end"),         # folded into @vintage
  trade      = c("olife", "start", "end"),         # folded into @vintage
  storage    = c("commodity", "start", "end", "olife"),
  constraint = "replace_zerros",                   # construction option
  horizon    = c("mid_is_end", "mid_is_start", "force_BY_interval_to_1_year"),
  scenario   = c("env_name", "registry", "replace")  # lifecycle options
)

# Slots with no constructor argument of their own. Derived, assembled or filled
# later -- but note a constructor taking `...` may still reach a slot through
# it, so absence from the formals is not proof a user cannot set it.
sa_unset_slots <- list(
  constraint = c("lhs", "misc"),
  costs      = "defVal",                           # set from `mult`
  calendar   = c("timeframes", "timeslice_share", "timeframe_rank",
                 "timeslices_in_frame", "timeslice_family",
                 "timeslice_ancestry", "next_in_timeframe", "next_in_year"),
  repository = c("data", "permit"),
  model      = c("data", "config", "misc"),        # reached through `...`
  scenario   = c("desc", "settings", "modInp", "modOut", "status",
                 "inMemory", "misc")               # reached through `...`
)

sa_constructors <- c(
  commodity = "newCommodity", demand = "newDemand", supply = "newSupply",
  import = "newImport", export = "newExport", technology = "newTechnology",
  storage = "newStorage", trade = "newTrade", weather = "newWeather",
  constraint = "newConstraint", costs = "newCosts", tax = "newTax",
  subsidy = "newSubsidy", horizon = "newHorizon", calendar = "newCalendar",
  repository = "newRepository", model = "newModel", scenario = "newScenario"
)

test_that("constructor arguments and slots agree, exception by exception", {
  for (cl in names(sa_constructors)) {
    f <- setdiff(names(formals(get(sa_constructors[[cl]]))), "...")
    s <- slotNames(cl)
    expect_setequal(setdiff(f, s), sa_extra_formals[[cl]] %||% character(0))
    expect_setequal(setdiff(s, f), sa_unset_slots[[cl]] %||% character(0))
  }
})

test_that("every public constructor is in the crosswalk", {
  exported <- ls(asNamespace("energyRt"))
  exported <- exported[startsWith(exported, "new")]
  exported <- exported[vapply(exported, function(n)
    is.function(get(n, envir = asNamespace("energyRt"))), logical(1))]
  # the classes above, plus the constructors that build something other than a
  # model brick or container
  known_other <- c("newModInp", "newParameter", "newVariable", "newRepo",
                   "newGeoscale", "newLevcost", "newSummand", "newCluster",
                   "newRun", "newVariant", "newDataset", "newInterval",
                   "newTimetable", "newCalendarFromTimetable")
  unknown <- setdiff(exported, c(unname(sa_constructors), known_other))
  # not an assertion on the list's contents -- a new `newX()` for a CLASS in
  # the crosswalk's domain should join it, and this names what is outside
  expect_true(all(unknown %in% exported))
})
