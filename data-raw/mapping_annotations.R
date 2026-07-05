# =========================================================================== #
# mapping_annotations.R
#
# MANUAL annotation of the mapping specification, expressed as data so it is
# reproducible and survives regeneration of the skeleton. Consumed by
# `data-raw/make_mapping_spec.R` (via `annotate_specs()`), which writes the
# combined result to `data-raw/mapping_spec.yml`.
#
# `recipe` encodes the build tier / dependency order. The mapping engine
# evaluates recipes in this order, so each recipe's inputs are guaranteed to
# exist beforehand:
#   1 membership      - object-slot -> 2-column membership map
#   2 calendar        - calendar / horizon structure (model-independent)
#   closure           - mCommReg commodity<->region reachability fixpoint
#   3 lifespan        - new / span / eac windows (wraps .process_lifespan)
#   4 value           - value-derived maps (exist where a parameter has a value)
#   5 filter          - activity / flow / aggregation / aux-conversion domains
#   6 constraint      - meq* and bound m* derived from user constraint objects
#   7 cost_agg        - top-level cost aggregations (after everything else)
# =========================================================================== #

.mapping_recipe <- list(
  membership = c(
    "mDemComm", "mSupComm", "mExpComm", "mImpComm", "mTradeComm", "mStorageComm",
    "mTechInpComm", "mTechOutComm", "mTechAInp", "mTechAOut", "mTechOneComm",
    "mTechGroupComm", "mTechInpGroup", "mTechOutGroup",
    "mTechInpCommSameSlice", "mTechOutCommSameSlice",
    "mTechAInpCommSameSlice", "mTechAOutCommSameSlice",
    "mTechInpCommAgg", "mTechOutCommAgg", "mTechAInpCommAgg", "mTechAOutCommAgg",
    "mTechInpCommAggSlice", "mTechOutCommAggSlice",
    "mTechAInpCommAggSlice", "mTechAOutCommAggSlice",
    "mTechEmsFuel",
    "mStorageAInp", "mStorageAOut", "mWeatherRegion"
  ),
  calendar = c(
    "mSliceNext", "mSliceFYearNext", "mSameRegion", "mSameSlice",
    "mMilestoneFirst", "mMilestoneLast", "mMilestoneNext", "mMilestoneHasNext",
    "mMidMilestone", "mCommSlice", "mCommSliceOrParent",
    "mSliceParentChild", "mSliceParentChildE", "mSliceFamily",
    "mStorageFullYear", "mTechFullYear",
    "mTechSlice", "mSupSlice", "mExpSlice", "mImpSlice", "mTradeSlice",
    "mWeatherSlice"
  ),
  closure = c(
    "mCommReg"
  ),
  lifespan = c(
    "mTechNew", "mTechSpan", "mTechEac", "mvTechRetiredStock",
    "mvTechRetiredNewCap", "meqTechRetiredNewCap", "mTechOlifeInf",
    "mStorageNew", "mStorageSpan", "mStorageEac", "mStorageOlifeInf",
    "mTradeSpan", "mTradeNew", "mTradeOlifeInf"
  ),
  value = c(
    "mTechInv", "mTechFixom", "mTechVarom", "mTechRetCost", "mTechRetirement",
    "mTechUpgrade", "mvSupCost", "mvSupReserve", "mSupSpan",
    "mTradeInv", "mTradeEac", "mTradeFixom",
    "mImportIrCost", "mExportIrCost", "mImportRowCost", "mExportRowCost",
    "mDummyImportCost", "mDummyExportCost", "mTaxCost", "mSubCost",
    "mStorageFixom", "mStorageVarom",
    "mSupWeatherLo", "mSupWeatherUp",
    "mTechWeatherAfLo", "mTechWeatherAfUp", "mTechWeatherAfsLo",
    "mTechWeatherAfsUp", "mTechWeatherAfcLo", "mTechWeatherAfcUp",
    "mStorageWeatherAfLo", "mStorageWeatherAfUp",
    "mStorageWeatherCinpLo", "mStorageWeatherCinpUp",
    "mStorageWeatherCoutLo", "mStorageWeatherCoutUp"
  ),
  filter = c(
    # activity / flow
    "mvTechAct", "mvTechInp", "mvTechOut", "mvTechAInp", "mvTechAOut",
    "mSupAva", "mSupAvaUp", "mvDemInp",
    "mvStorageStore", "mvStorageAInp", "mvStorageAOut",
    "mvTradeIr", "mTradeIr", "mImport", "mExport",
    "mImportRow", "mExportRow", "mImportRowUp", "mExportRowUp",
    "mDummyImport", "mDummyExport", "mEmsFuelTot", "mAggOut",
    "mAggregateFactor",
    # aggregations
    "mvBalance", "mvOutTot", "mvInpTot",
    "mSupOutTot", "mTechInpTot", "mTechOutTot", "mStorageInpTot",
    "mStorageOutTot",
    "mvTradeIrAInp", "mvTradeIrAOut", "mvTradeIrAInpTot", "mvTradeIrAOutTot",
    "mTradeIrAInp", "mTradeIrAOut",
    # aux-conversion coefficient maps
    "mTechAct2AInp", "mTechCap2AInp", "mTechNCap2AInp", "mTechCinp2AInp",
    "mTechCout2AInp", "mTechAct2AOut", "mTechCap2AOut", "mTechNCap2AOut",
    "mTechCinp2AOut", "mTechCout2AOut",
    "mStorageStg2AInp", "mStorageCinp2AInp", "mStorageCout2AInp",
    "mStorageCap2AInp", "mStorageNCap2AInp",
    "mStorageStg2AOut", "mStorageCinp2AOut", "mStorageCout2AOut",
    "mStorageCap2AOut", "mStorageNCap2AOut",
    "mTradeIrCsrc2Ainp", "mTradeIrCdst2Ainp",
    "mTradeIrCsrc2Aout", "mTradeIrCdst2Aout"
  ),
  constraint = c(
    # meq* (all except the lifespan-generated meqTechRetiredNewCap)
    "meqBalFx", "meqBalLo", "meqBalUp", "meqExportRowLo", "meqImportRowLo",
    "meqLECActivity", "meqStorageAfLo", "meqStorageAfUp", "meqStorageInpLo",
    "meqStorageInpUp", "meqStorageOutLo", "meqStorageOutUp", "meqStorageStore",
    "meqSupAvaLo", "meqSupReserveLo", "meqTechActGrp", "meqTechActSng",
    "meqTechAfcInpLo", "meqTechAfcInpUp", "meqTechAfcOutLo", "meqTechAfcOutUp",
    "meqTechAfLo", "meqTechAfsLo", "meqTechAfsUp", "meqTechAfUp",
    "meqTechGrp2Grp", "meqTechGrp2Sng", "meqTechShareInpLo", "meqTechShareInpUp",
    "meqTechShareOutLo", "meqTechShareOutUp", "meqTechSng2Grp", "meqTechSng2Sng",
    "meqTradeCapFlow", "meqTradeFlowLo", "meqTradeFlowUp",
    # bound m* sourced from constraint slots
    "mTechCapLo", "mTechCapUp", "mTechNewCapLo", "mTechNewCapUp",
    "mTechRetLo", "mTechRetUp", "mTechAfUp", "mTechAfcUp",
    "mTechRampUp", "mTechRampDown", "mSupReserveUp",
    "mStorageCapLo", "mStorageCapUp", "mStorageNewCapLo", "mStorageNewCapUp",
    "mStorageRetLo", "mStorageRetUp",
    "mTradeCapLo", "mTradeCapUp", "mTradeNewCapLo", "mTradeNewCapUp",
    "mTradeRetLo", "mTradeRetUp",
    "mExportRowCumUp", "mImportRowCumUp",
    "mUpComm", "mLoComm", "mFxComm", "mLECRegion", "mTradeRoutes",
    "mTradeCapacityVariable"
  ),
  cost_agg = c(
    "mvTotalCost", "mvTotalUserCosts", "mvTradeCost", "mvTradeRowCost"
  )
)

# Default `source` per recipe.
.recipe_source <- c(
  membership = "object_slot",
  calendar   = "calendar",
  closure    = "derived",
  lifespan   = "derived",
  value      = "param",
  filter     = "derived",
  constraint = "constraint",
  cost_agg   = "derived"
)

# Recipes whose maps are gated by commodity availability (mCommReg).
.recipe_filtered_by_commreg <- c("filter")

# mCommReg depends on the membership maps that seed/propagate availability.
.commreg_depends_on <- c(
  "mSupComm", "mImpComm", "mDemComm", "mTradeComm",
  "mTechInpComm", "mTechOutComm", "mStorageComm",
  "mExpComm", "mTechAInp", "mTechAOut"
)

# Apply the annotations above onto a generated skeleton (named list of specs).
annotate_specs <- function(specs) {
  # Build name -> recipe lookup.
  recipe_of <- character(0)
  for (rc in names(.mapping_recipe)) {
    for (nm in .mapping_recipe[[rc]]) recipe_of[nm] <- rc
  }

  for (nm in names(specs)) {
    rc <- recipe_of[[nm]]
    if (is.null(rc) || is.na(rc)) {
      specs[[nm]]$recipe <- "UNCLASSIFIED"
      next
    }
    specs[[nm]]$recipe <- rc
    specs[[nm]]$source <- unname(.recipe_source[rc])
    if (rc == "value") {
      specs[[nm]]$predicate <- "!is.na(value)"
    }
    if (rc %in% .recipe_filtered_by_commreg) {
      specs[[nm]]$filter_by <- as.list(union(
        unlist(specs[[nm]]$filter_by), "mCommReg"
      ))
    }
    if (nm == "mCommReg") {
      specs[[nm]]$depends_on <- as.list(.commreg_depends_on)
    }
  }
  specs
}
