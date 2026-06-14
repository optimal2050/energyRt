# =========================================================================== #
# map_constraint.R  —  equation index-domain maps (family "constraint")
#
# One `map_<Name>(scen, fmp) -> scen` per regular constraint map. A regular
# constraint map = a base activity/flow domain intersected with a bound parameter
# (filtered by type lo/up/fx and a value predicate) projected onto its dims — the
# "domain x filtered-source" shape implemented by the shared
# `.build_constraint_join_map` (mapping_engine.R), driven by `.constraint_map_def`.
#
# This migrates the ~39 def-table maps. The bespoke maps (meqStorageStore,
# meqTradeCapFlow, mTradeCapacityVariable), the tech-group/share and ramp maps,
# and the cross-stage maps (built in filter / elsewhere / empty-legacy /
# deprecated) remain on the recipe_constraint fallback. Reuses the engine's
# `.constraint_map_def` + `.build_constraint_join_map` in place (archiving deferred
# to the Phase 4 sweep).
#
# These maps depend only on earlier-recipe domains (mv*, m*Span/New) and
# interpolated bound parameters, so they are mutually independent (no intra-family
# build order needed).
# =========================================================================== #

.cjoin <- function(scen, name, fmp)
  .build_constraint_join_map(scen, name, .constraint_map_def[[name]], fmp)

# C1 commodity balance
map_meqBalLo <- function(scen, fmp) .cjoin(scen, "meqBalLo", fmp)
map_meqBalUp <- function(scen, fmp) .cjoin(scen, "meqBalUp", fmp)
map_meqBalFx <- function(scen, fmp) .cjoin(scen, "meqBalFx", fmp)
# C2 technology availability factors
map_meqTechAfLo     <- function(scen, fmp) .cjoin(scen, "meqTechAfLo", fmp)
map_meqTechAfUp     <- function(scen, fmp) .cjoin(scen, "meqTechAfUp", fmp)
map_meqTechAfsLo    <- function(scen, fmp) .cjoin(scen, "meqTechAfsLo", fmp)
map_meqTechAfsUp    <- function(scen, fmp) .cjoin(scen, "meqTechAfsUp", fmp)
map_meqTechAfcInpLo <- function(scen, fmp) .cjoin(scen, "meqTechAfcInpLo", fmp)
map_meqTechAfcInpUp <- function(scen, fmp) .cjoin(scen, "meqTechAfcInpUp", fmp)
map_meqTechAfcOutLo <- function(scen, fmp) .cjoin(scen, "meqTechAfcOutLo", fmp)
map_meqTechAfcOutUp <- function(scen, fmp) .cjoin(scen, "meqTechAfcOutUp", fmp)
# C4 technology capacity / retirement
map_mTechCapLo    <- function(scen, fmp) .cjoin(scen, "mTechCapLo", fmp)
map_mTechCapUp    <- function(scen, fmp) .cjoin(scen, "mTechCapUp", fmp)
map_mTechNewCapLo <- function(scen, fmp) .cjoin(scen, "mTechNewCapLo", fmp)
map_mTechNewCapUp <- function(scen, fmp) .cjoin(scen, "mTechNewCapUp", fmp)
map_mTechRetLo    <- function(scen, fmp) .cjoin(scen, "mTechRetLo", fmp)
map_mTechRetUp    <- function(scen, fmp) .cjoin(scen, "mTechRetUp", fmp)
# C5 storage activity bounds
map_meqStorageAfLo  <- function(scen, fmp) .cjoin(scen, "meqStorageAfLo", fmp)
map_meqStorageAfUp  <- function(scen, fmp) .cjoin(scen, "meqStorageAfUp", fmp)
map_meqStorageInpLo <- function(scen, fmp) .cjoin(scen, "meqStorageInpLo", fmp)
map_meqStorageInpUp <- function(scen, fmp) .cjoin(scen, "meqStorageInpUp", fmp)
map_meqStorageOutLo <- function(scen, fmp) .cjoin(scen, "meqStorageOutLo", fmp)
map_meqStorageOutUp <- function(scen, fmp) .cjoin(scen, "meqStorageOutUp", fmp)
# C6 storage capacity / retirement
map_mStorageCapLo    <- function(scen, fmp) .cjoin(scen, "mStorageCapLo", fmp)
map_mStorageCapUp    <- function(scen, fmp) .cjoin(scen, "mStorageCapUp", fmp)
map_mStorageNewCapLo <- function(scen, fmp) .cjoin(scen, "mStorageNewCapLo", fmp)
map_mStorageNewCapUp <- function(scen, fmp) .cjoin(scen, "mStorageNewCapUp", fmp)
map_mStorageRetLo    <- function(scen, fmp) .cjoin(scen, "mStorageRetLo", fmp)
map_mStorageRetUp    <- function(scen, fmp) .cjoin(scen, "mStorageRetUp", fmp)
# C7 trade capacity / retirement
map_mTradeCapLo    <- function(scen, fmp) .cjoin(scen, "mTradeCapLo", fmp)
map_mTradeCapUp    <- function(scen, fmp) .cjoin(scen, "mTradeCapUp", fmp)
map_mTradeNewCapLo <- function(scen, fmp) .cjoin(scen, "mTradeNewCapLo", fmp)
map_mTradeNewCapUp <- function(scen, fmp) .cjoin(scen, "mTradeNewCapUp", fmp)
map_mTradeRetLo    <- function(scen, fmp) .cjoin(scen, "mTradeRetLo", fmp)
map_mTradeRetUp    <- function(scen, fmp) .cjoin(scen, "mTradeRetUp", fmp)
# C7 trade inter-regional flow bounds
map_meqTradeFlowLo <- function(scen, fmp) .cjoin(scen, "meqTradeFlowLo", fmp)
map_meqTradeFlowUp <- function(scen, fmp) .cjoin(scen, "meqTradeFlowUp", fmp)
# C8 supply reserve margins
map_meqSupReserveLo <- function(scen, fmp) .cjoin(scen, "meqSupReserveLo", fmp)
map_mSupReserveUp   <- function(scen, fmp) .cjoin(scen, "mSupReserveUp", fmp)

# -- bespoke builders (each already a per-mapping function) ---------------- #
map_meqStorageStore       <- function(scen, fmp) .build_meqStorageStore(scen, fmp)
map_meqTradeCapFlow       <- function(scen, fmp) .build_meqTradeCapFlow(scen, fmp)
map_mTradeCapacityVariable <- function(scen, fmp) .build_mTradeCapacityVariable(scen, fmp)

# -- technology group / share maps ----------------------------------------- #
# Built together from shared intermediates by .build_tech_group_maps; the wrapper
# passes the single requested name (the builder gates output by `names`).
.tgroup <- function(scen, name, fmp) .build_tech_group_maps(scen, name, fmp)
map_meqTechActSng     <- function(scen, fmp) .tgroup(scen, "meqTechActSng", fmp)
map_meqTechActGrp     <- function(scen, fmp) .tgroup(scen, "meqTechActGrp", fmp)
map_meqTechGrp2Sng    <- function(scen, fmp) .tgroup(scen, "meqTechGrp2Sng", fmp)
map_meqTechSng2Grp    <- function(scen, fmp) .tgroup(scen, "meqTechSng2Grp", fmp)
map_meqTechSng2Sng    <- function(scen, fmp) .tgroup(scen, "meqTechSng2Sng", fmp)
map_meqTechGrp2Grp    <- function(scen, fmp) .tgroup(scen, "meqTechGrp2Grp", fmp)
map_meqTechShareInpLo <- function(scen, fmp) .tgroup(scen, "meqTechShareInpLo", fmp)
map_meqTechShareInpUp <- function(scen, fmp) .tgroup(scen, "meqTechShareInpUp", fmp)
map_meqTechShareOutLo <- function(scen, fmp) .tgroup(scen, "meqTechShareOutLo", fmp)
map_meqTechShareOutUp <- function(scen, fmp) .tgroup(scen, "meqTechShareOutUp", fmp)

# -- ramping maps ---------------------------------------------------------- #
map_mTechRampUp   <- function(scen, fmp) .build_ramp_maps(scen, "mTechRampUp", fmp)
map_mTechRampDown <- function(scen, fmp) .build_ramp_maps(scen, "mTechRampDown", fmp)

# -- intentionally-empty maps ---------------------------------------------- #
# empty-legacy: declared as solver index sets but never populated by the legacy
# pipeline; deprecated: the LEC feature being removed. Both emit empty (faithful).
map_mTechAfUp      <- function(scen, fmp) scen
map_mTechAfcUp     <- function(scen, fmp) scen
map_meqLECActivity <- function(scen, fmp) scen
map_mLECRegion     <- function(scen, fmp) scen

# -- registry for the constraint family (def-table maps) ------------------- #
.constraint_builders <- list(
  meqBalLo = map_meqBalLo, meqBalUp = map_meqBalUp, meqBalFx = map_meqBalFx,
  meqTechAfLo = map_meqTechAfLo, meqTechAfUp = map_meqTechAfUp,
  meqTechAfsLo = map_meqTechAfsLo, meqTechAfsUp = map_meqTechAfsUp,
  meqTechAfcInpLo = map_meqTechAfcInpLo, meqTechAfcInpUp = map_meqTechAfcInpUp,
  meqTechAfcOutLo = map_meqTechAfcOutLo, meqTechAfcOutUp = map_meqTechAfcOutUp,
  mTechCapLo = map_mTechCapLo, mTechCapUp = map_mTechCapUp,
  mTechNewCapLo = map_mTechNewCapLo, mTechNewCapUp = map_mTechNewCapUp,
  mTechRetLo = map_mTechRetLo, mTechRetUp = map_mTechRetUp,
  meqStorageAfLo = map_meqStorageAfLo, meqStorageAfUp = map_meqStorageAfUp,
  meqStorageInpLo = map_meqStorageInpLo, meqStorageInpUp = map_meqStorageInpUp,
  meqStorageOutLo = map_meqStorageOutLo, meqStorageOutUp = map_meqStorageOutUp,
  mStorageCapLo = map_mStorageCapLo, mStorageCapUp = map_mStorageCapUp,
  mStorageNewCapLo = map_mStorageNewCapLo, mStorageNewCapUp = map_mStorageNewCapUp,
  mStorageRetLo = map_mStorageRetLo, mStorageRetUp = map_mStorageRetUp,
  mTradeCapLo = map_mTradeCapLo, mTradeCapUp = map_mTradeCapUp,
  mTradeNewCapLo = map_mTradeNewCapLo, mTradeNewCapUp = map_mTradeNewCapUp,
  mTradeRetLo = map_mTradeRetLo, mTradeRetUp = map_mTradeRetUp,
  meqTradeFlowLo = map_meqTradeFlowLo, meqTradeFlowUp = map_meqTradeFlowUp,
  meqSupReserveLo = map_meqSupReserveLo, mSupReserveUp = map_mSupReserveUp,
  # bespoke
  meqStorageStore = map_meqStorageStore, meqTradeCapFlow = map_meqTradeCapFlow,
  mTradeCapacityVariable = map_mTradeCapacityVariable,
  # tech-group / share
  meqTechActSng = map_meqTechActSng, meqTechActGrp = map_meqTechActGrp,
  meqTechGrp2Sng = map_meqTechGrp2Sng, meqTechSng2Grp = map_meqTechSng2Grp,
  meqTechSng2Sng = map_meqTechSng2Sng, meqTechGrp2Grp = map_meqTechGrp2Grp,
  meqTechShareInpLo = map_meqTechShareInpLo, meqTechShareInpUp = map_meqTechShareInpUp,
  meqTechShareOutLo = map_meqTechShareOutLo, meqTechShareOutUp = map_meqTechShareOutUp,
  # ramp
  mTechRampUp = map_mTechRampUp, mTechRampDown = map_mTechRampDown,
  # intentionally empty (empty-legacy + deprecated LEC)
  mTechAfUp = map_mTechAfUp, mTechAfcUp = map_mTechAfcUp,
  meqLECActivity = map_meqLECActivity, mLECRegion = map_mLECRegion
)
