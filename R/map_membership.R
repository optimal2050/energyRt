# =========================================================================== #
# map_membership.R  —  process<->commodity membership maps (family "membership")
#
# One `map_<Name>(scen, fmp) -> scen` per core membership map. Each is the set of
# (object, commodity) pairs where the object consumes/produces the commodity,
# read from the corresponding `*_comm` set (populated in interp_mod from the model
# objects). Registered in `.membership_builders` and built via
# `build_mappings(recipes = "membership")` in interp_mod.
#
# These replace the inline membership block formerly in interp.R (archived to
# drafts/legacy-mapping/membership.R). The aux maps reuse the live
# `.build_aux_membership` helper (interp.R). The other ~18 membership-tagged maps
# (mTech*CommAgg / *SameSlice / *Group / mTechEmsFuel / mWeatherRegion / ...) are
# built later in the filter recipe, not here.
# =========================================================================== #

# named-list set (object -> commodities) -> 2-column map parameter.
.membership_map <- function(scen, name, named_list, key, fmp) {
  df <- named_list_to_df(named_list, col_names = c(key, "comm"))
  scen@modInp@parameters[[name]] <-
    d2p(scen@modInp@parameters[[name]], df, fmp(name))
  scen
}

map_mSupComm     <- function(scen, fmp) .membership_map(scen, "mSupComm",     scen@modInp@sets[["supply_comm"]],      "sup",   fmp)
map_mImpComm     <- function(scen, fmp) .membership_map(scen, "mImpComm",     scen@modInp@sets[["import_comm"]],      "imp",   fmp)
map_mDemComm     <- function(scen, fmp) .membership_map(scen, "mDemComm",     scen@modInp@sets[["demand_comm"]],      "dem",   fmp)
map_mExpComm     <- function(scen, fmp) .membership_map(scen, "mExpComm",     scen@modInp@sets[["export_comm"]],      "expp",  fmp)
map_mTradeComm   <- function(scen, fmp) .membership_map(scen, "mTradeComm",   scen@modInp@sets[["trade_comm"]],       "trade", fmp)
map_mStorageComm <- function(scen, fmp) .membership_map(scen, "mStorageComm", scen@modInp@sets[["storage_comm"]],     "stg",   fmp)
map_mTechInpComm <- function(scen, fmp) .membership_map(scen, "mTechInpComm", scen@modInp@sets[["tech_input_comm"]],  "tech",  fmp)
map_mTechOutComm <- function(scen, fmp) .membership_map(scen, "mTechOutComm", scen@modInp@sets[["tech_output_comm"]], "tech",  fmp)

# Auxiliary-commodity membership maps, split by input / output direction. The
# helper builds BOTH directions per family in one call (idempotent); registering
# each name to it keeps the registry interface per-mapping.
map_mTechAInp    <- function(scen, fmp) .build_aux_membership(scen, "technology", "tech", fmp, "mTechAInp",    "mTechAOut")
map_mTechAOut    <- function(scen, fmp) .build_aux_membership(scen, "technology", "tech", fmp, "mTechAInp",    "mTechAOut")
map_mStorageAInp <- function(scen, fmp) .build_aux_membership(scen, "storage",    "stg",  fmp, "mStorageAInp", "mStorageAOut")
map_mStorageAOut <- function(scen, fmp) .build_aux_membership(scen, "storage",    "stg",  fmp, "mStorageAInp", "mStorageAOut")

# -- registry for the membership family ------------------------------------ #
.membership_builders <- list(
  mSupComm     = map_mSupComm,
  mImpComm     = map_mImpComm,
  mDemComm     = map_mDemComm,
  mExpComm     = map_mExpComm,
  mTradeComm   = map_mTradeComm,
  mStorageComm = map_mStorageComm,
  mTechInpComm = map_mTechInpComm,
  mTechOutComm = map_mTechOutComm,
  mTechAInp    = map_mTechAInp,
  mTechAOut    = map_mTechAOut,
  mStorageAInp = map_mStorageAInp,
  mStorageAOut = map_mStorageAOut
)
