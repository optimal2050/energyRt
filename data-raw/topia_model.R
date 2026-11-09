## data-raw/topia_model.R
## The shipped TOPIA model, tangled from the vignette that documents it.
##
## `vignettes/topia-build.Rmd` builds the model object by object, in the order
## a reader follows. Restating that assembly here would be a second source of
## truth, and the two drifted before: the vignette moved to GWh, two land
## pools, site clusters and a hydrogen chain while the hand-written builder
## stayed in PJ with none of them. So the article IS the builder -- knitted
## here, not retyped -- which is what CONVENTIONS.md asks of a `data-raw/`
## driver.
##
## The vignette reads `topia$geoscales`, `topia$links`,
## `topia$modules$calendars`, `topia$modules$horizons` and (through
## `topia_profiles()`) `topia$weather`.
## During a rebuild those are the tables the earlier builders have just made,
## not the shipped copy being replaced, so the purled code is evaluated in an
## environment where `topia` is bound to the in-progress dataset.
##
## Leaves `topia_model`. Run from `data-raw/topia_assemble.R`, after
## `topia_geoscale.R`, `topia_data.R` and `topia_modules.R`.

if (!isNamespaceLoaded("energyRt")) library(energyRt)
stopifnot(requireNamespace("knitr", quietly = TRUE))

.vig <- "vignettes/topia-build.Rmd"
stopifnot(file.exists(.vig))

# The in-progress dataset the vignette will see in place of `energyRt::topia`.
# `topia_geoscales`, `topia_weather`/`topia_demand`/`topia_stock` and
# `topia_modules` are left in this environment by the builders that ran before.
.topia_sofar <- list(
  geoscales = topia_geoscales,
  links     = topia_links,
  weather   = topia_weather,
  demand    = topia_demand,
  stock     = topia_stock,
  grades    = topia_grades,
  modules   = topia_modules
)

.src <- knitr::purl(.vig, output = tempfile(fileext = ".R"), quiet = TRUE)
.env <- new.env(parent = globalenv())
assign("topia", .topia_sofar, envir = .env)
# The vignette's figures draw nothing the model depends on, and the maps among
# them need sf. `purl()` does not carry chunk options through, so the
# `eval = have_sf` guards are gone by the time this runs: the display verbs are
# shadowed instead, which also keeps the data build off ggplot2 and sf.
assign("have_sf", FALSE, envir = .env)
for (.f in c("gg_geoscale", "gg_trade_map", "gg_trade_network",
             "plot_geoscale", "plot_trade_map", "plot_trade_network",
             "autoplot", "draw")) {
  assign(.f, function(...) invisible(NULL), envir = .env)
}

message("-- tangling: ", .vig)
.exprs <- parse(.src)
for (.i in seq_along(.exprs)) {
  eval(.exprs[[.i]], .env)
}

.need <- c("regions", "repo", "gs", "CO2_CAP", "CT_CO2", "EARLY_RET",
           "GREEN_H2")
.miss <- .need[!vapply(.need, exists, logical(1), envir = .env, inherits = FALSE)]
if (length(.miss)) {
  stop("the vignette did not leave: ", paste(.miss, collapse = ", "),
       ". `data-raw/topia_model.R` names the objects it takes from ",
       .vig, "; rename there and here together.", call. = FALSE)
}

topia_model <- list(
  regions   = .env$regions,
  # the name of the calendar the vignette built on, read from the object it
  # used rather than restated -- the two drifted when this was a literal
  calendar  = .env$calendar@name,
  repo      = .env$repo,
  geoscale  = .env$gs,
  CO2_CAP   = .env$CO2_CAP,
  CT_CO2    = .env$CT_CO2,
  EARLY_RET = .env$EARLY_RET,
  GREEN_H2  = .env$GREEN_H2
)

stopifnot(
  length(topia_model$regions) == 12L,
  methods::is(topia_model$repo, "repository"),
  length(getObject(topia_model$repo)) > 0L
)
message("topia_model: ", length(topia_model$regions), " regions, ",
        length(getObject(topia_model$repo)), " objects")

rm(.vig, .topia_sofar, .src, .env, .exprs, .i, .f, .need, .miss)
