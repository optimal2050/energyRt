# Precomputed storage_duration() result for the Storage article ##############
#
# The article's headline figure needs a FULL YEAR of hourly storage levels:
# duration bands only mean something when the model can actually hold energy
# for weeks. That is an 8760-timeslice model, which takes minutes to solve and
# wants julia/HiGHS rather than GLPK, so it is solved once here and the result
# ships. `vignettes/articles/storage.Rmd` shows the same model as a
# non-evaluated chunk and plots this table.
#
# Run from the package root, with Julia and JuMP available:
#   source("data-raw/vre_storage_duration.R")
##############################################################################

pkgload::load_all(".", quiet = TRUE)

cal <- calendars$d365_h24
slices <- unique(cal@timetable$timeslice)
stopifnot(setequal(slices, vre_cf$slice))

# Demand is CONSTANT. The whole shape of the answer then comes from the
# resource, not from the load: whatever the store does, it is doing because the
# wind and the sun arrive at the wrong time, never because demand asked for it.
load_shape <- 1

ELC <- newCommodity(name = "ELC", desc = "Electricity", unit = "MWh",
                    timeframe = "HOUR")
GAS <- newCommodity(name = "GAS", desc = "Natural gas", unit = "MWh",
                    timeframe = "ANNUAL")

# Gas is here to guarantee feasibility, not to compete.
SUP_GAS <- newSupply(
  name = "SUP_GAS", desc = "Natural gas, priced as a backstop",
  commodity = "GAS",
  supply = data.frame(cost = 300))

# The capacity factor goes on the TECHNOLOGY, as an hourly availability bound:
#   out[s] <= af.up[s] * cap2act * cap * share[s]  ==  cf[s] * cap
# Putting it on a supply's `ava.up` instead caps the RESOURCE in absolute terms
# -- the plant can then never be built larger than one unit however cheap it is,
# and the backstop silently covers the rest. That mistake is invisible in the
# objective and obvious in the generation mix.
ESOL <- newTechnology(
  name = "ESOL", desc = "Solar PV",
  output = list(comm = "ELC", unit = "MWh"),
  af = data.frame(timeslice = vre_cf$slice, af.up = vre_cf$solar),
  invcost = data.frame(invcost = 700),
  vintage = data.frame(olife = 25L), cap2act = 8760)

EWIN <- newTechnology(
  name = "EWIN", desc = "Wind turbine",
  output = list(comm = "ELC", unit = "MWh"),
  af = data.frame(timeslice = vre_cf$slice, af.up = vre_cf$wind),
  invcost = data.frame(invcost = 1300),
  vintage = data.frame(olife = 25L), cap2act = 8760)

EPEAK <- newTechnology(
  name = "EPEAK", desc = "Gas peaking plant, cheap to build and dear to run",
  input = list(comm = "GAS", unit = "MWh"),
  output = list(comm = "ELC", unit = "MWh"),
  ceff = data.frame(comm = "GAS", cinp2use = 0.45),
  invcost = data.frame(invcost = 400),
  vintage = data.frame(olife = 25L), cap2act = 8760)

STG_BTR <- newStorage(
  name = "STG_BTR", desc = "Cheap store, energy and power sized separately",
  commodity = "ELC",
  output = list(comm = "ELC", unit = "MWh"),
  storage = list(comm = "ELC", unit = "MWh"),
  # power sized per MW on the discharger, energy per MWh on the reservoir
  invcost = list(out.invcost = 20, stg.invcost = 1),
  seff = data.frame(inpeff = 0.95, outeff = 0.95),
  vintage = data.frame(olife = 15L),
  fullYear = TRUE)

DEM_ELC <- newDemand(
  name = "DEM_ELC", desc = "Constant electricity demand",
  commodity = "ELC",
  demand = data.frame(timeslice = vre_cf$slice, demand = load_shape))

repo <- newRepository(name = "repo_vre",
                      ELC, GAS, SUP_GAS,
                      ESOL, EWIN, EPEAK, STG_BTR, DEM_ELC)

mod <- newModel(name = "vre_storage", desc = "Wind, solar and a battery",
                region = "R1", horizon = newHorizon(2020), discount = 0.05,
                calendar = cal, repo = repo)

message("solving 8760 timeslices on julia/HiGHS ...")
t0 <- Sys.time()
scen <- solve_model(mod, name = "vre_storage",
                    solver = solver_options$julia_highs)
message("solved in ", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1),
        " min; objective ", getData(scen, "vObjective", merge = TRUE)$value[1])

# What the model actually did -- printed, not shipped. Worth reading: if
# curtailment is high or the store is small, the example is not demonstrating
# what it claims to.
peek <- function(v, cols = NULL) {
  d <- tryCatch(as.data.frame(getData(scen, v, merge = TRUE)),
                error = function(e) NULL)
  if (is.null(d) || !NROW(d)) { message(v, ": (none)"); return(invisible()) }
  key <- intersect(c("tech", "stg", "comm"), names(d))
  if (length(key)) {
    tot <- tapply(d$value, d[[key[1]]], sum)
    message(v, ": ", paste(names(tot), round(tot, 1), sep = "=", collapse = "  "))
  } else {
    message(v, ": ", round(sum(d$value), 1))
  }
  invisible(d)
}
peek("vTechCap")
peek("vStorageOutCap")
peek("vStorageStgCap")
gen <- peek("vTechOut")
sur <- peek("vBalance")
if (!is.null(gen) && !is.null(sur)) {
  message("curtailment share of generation: ",
          round(100 * sum(sur$value) / sum(gen$value), 2), "%")
}

vre_storage_duration <- storage_duration(scen)
vre_storage_duration <- vre_storage_duration[
  , c("stg", "timeslice", "datetime", "duration", "value")]
rownames(vre_storage_duration) <- NULL

stopifnot(nrow(vre_storage_duration) > 0,
          !anyNA(vre_storage_duration$value))
message("bands: ", paste(levels(vre_storage_duration$duration), collapse = " "))
print(round(tapply(vre_storage_duration$value,
                   vre_storage_duration$duration, sum)))

usethis::use_data(vre_storage_duration, overwrite = TRUE, compress = "xz")
