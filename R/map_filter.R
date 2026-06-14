# =========================================================================== #
# map_filter.R  —  activity / flow index-domain maps (family "filter")
#
# One `map_<Name>(scen, fmp) -> scen` per filter map. A filter map is the index
# domain of a flow/activity variable: the product of an operation window x a
# membership map x a slice map (x years), restricted to feasible (comm, region)
# pairs via the commodity-region closure mCommReg (.filt_cr). Each map reads its
# dependency maps from the scenario with .gds; the registry builds them in
# dependency order (the `.filter_builders` list order — build_mappings iterates in
# registry order).
#
# CORE flow domains are migrated here; the ~55 DERIVED totals / aggregations are
# still built by the recipe_filter fallback (which reads these core maps from the
# scenario). Migration proceeds core-first, then derived in batches.
# =========================================================================== #

# per-object slice map (leaf slices of each object's finest timeframe).
.filter_slice <- function(scen, cls, key) {
  .proc_slice_for(.process_slice_df(scen), get_process_class(scen), cls, key)
}

# -- technology core ------------------------------------------------------- #
# mvTechAct: operation window x slice (no commodity, so no mCommReg filter).
map_mvTechAct <- function(scen, fmp) {
  tech_span  <- .gds(scen, "mTechSpan")
  tech_slice <- .filter_slice(scen, "technology", "tech")
  if (is.null(tech_span) || is.null(tech_slice)) return(scen)
  .set_map(scen, "mvTechAct", as.data.frame(merge0(tech_span, tech_slice)), fmp)
}

map_mvTechInp <- function(scen, fmp) {
  act <- .gds(scen, "mvTechAct"); inp <- .gds(scen, "mTechInpComm")
  if (is.null(act) || is.null(inp)) return(scen)
  .set_map(scen, "mvTechInp", .filt_cr(scen, as.data.frame(merge0(act, inp))), fmp)
}

map_mvTechOut <- function(scen, fmp) {
  act <- .gds(scen, "mvTechAct"); out <- .gds(scen, "mTechOutComm")
  if (is.null(act) || is.null(out)) return(scen)
  .set_map(scen, "mvTechOut", .filt_cr(scen, as.data.frame(merge0(act, out))), fmp)
}

# mTechOutRY: year-resolution projection of mvTechOut (drop slice).
map_mTechOutRY <- function(scen, fmp) {
  mvTechOut <- .gds(scen, "mvTechOut")
  if (is.null(mvTechOut)) return(scen)
  ry <- dplyr::distinct(dplyr::select(mvTechOut,
    dplyr::any_of(c("tech", "comm", "region", "year"))))
  .set_map(scen, "mTechOutRY", ry, fmp)
}

map_mvTechAInp <- function(scen, fmp) {
  act <- .gds(scen, "mvTechAct"); ainp <- .gds(scen, "mTechAInp")
  if (is.null(act) || is.null(ainp)) return(scen)
  .set_map(scen, "mvTechAInp", .filt_cr(scen, as.data.frame(merge0(act, ainp))), fmp)
}

map_mvTechAOut <- function(scen, fmp) {
  act <- .gds(scen, "mvTechAct"); aout <- .gds(scen, "mTechAOut")
  if (is.null(act) || is.null(aout)) return(scen)
  .set_map(scen, "mvTechAOut", .filt_cr(scen, as.data.frame(merge0(act, aout))), fmp)
}

# -- supply core ----------------------------------------------------------- #
# mSupAva: supply span x comm x slice x milestone years, mCommReg-restricted.
map_mSupAva <- function(scen, fmp) {
  sup_span  <- .gds(scen, "mSupSpan")
  sup_comm  <- .gds(scen, "mSupComm")
  sup_slice <- .filter_slice(scen, "supply", "sup")
  milestones <- as.integer(scen@settings@horizon@intervals$mid)
  if (is.null(sup_span) || is.null(sup_comm) || is.null(sup_slice) ||
      length(milestones) == 0) return(scen)
  ava <- merge0(sup_span, sup_comm)
  ava <- merge0(ava, sup_slice)
  ava <- merge0(as.data.frame(ava), data.frame(year = milestones))
  .set_map(scen, "mSupAva", .filt_cr(scen, as.data.frame(ava)), fmp)
}

# -- storage core ---------------------------------------------------------- #
# Unfiltered storage store intermediate (stg span x comm x slice). The aux maps
# below derive their base from THIS (unfiltered) frame, not the comm-filtered
# persisted mvStorageStore, so it is recomputed here.
.filter_storage_store <- function(scen) {
  stg_span  <- .gds(scen, "mStorageSpan")
  stg_comm  <- .gds(scen, "mStorageComm")
  stg_slice <- .filter_slice(scen, "storage", "stg")
  if (is.null(stg_span) || is.null(stg_comm) || is.null(stg_slice)) return(NULL)
  as.data.frame(merge0(as.data.frame(merge0(stg_span, stg_comm)), stg_slice))
}

map_mvStorageStore <- function(scen, fmp) {
  store <- .filter_storage_store(scen)
  if (is.null(store)) return(scen)
  .set_map(scen, "mvStorageStore", .filt_cr(scen, store), fmp)
}

map_mvStorageAInp <- function(scen, fmp) {
  store <- .filter_storage_store(scen)
  sa_inp <- .gds(scen, "mStorageAInp")
  if (is.null(store) || is.null(sa_inp)) return(scen)
  base <- dplyr::select(store, -dplyr::any_of("comm"))
  .set_map(scen, "mvStorageAInp",
           .filt_cr(scen, as.data.frame(merge0(base, sa_inp))), fmp)
}

map_mvStorageAOut <- function(scen, fmp) {
  store <- .filter_storage_store(scen)
  sa_out <- .gds(scen, "mStorageAOut")
  if (is.null(store) || is.null(sa_out)) return(scen)
  base <- dplyr::select(store, -dplyr::any_of("comm"))
  .set_map(scen, "mvStorageAOut",
           .filt_cr(scen, as.data.frame(merge0(base, sa_out))), fmp)
}

# =========================================================================== #
# DERIVED maps (totals / aggregations) — read core/membership/calendar maps from
# the scenario and collapse to each commodity's native slice via
# mCommSliceOrParent (.reduce_total_map). Migrated in batches; the rest remain on
# the recipe_filter fallback.
# =========================================================================== #

# mvDemInp: each demand commodity over its own slices x region x milestones.
map_mvDemInp <- function(scen, fmp) {
  dc <- .gds(scen, "mDemComm"); comm_slice <- .gds(scen, "mCommSlice")
  regions <- as.character(scen@settings@region)
  milestones <- as.integer(scen@settings@horizon@intervals$mid)
  if (is.null(dc) || is.null(comm_slice) ||
      length(regions) == 0 || length(milestones) == 0) return(scen)
  di <- merge0(dc, comm_slice)
  di <- merge0(as.data.frame(di),
               data.frame(region = regions, stringsAsFactors = FALSE))
  di <- merge0(as.data.frame(di), data.frame(year = milestones))
  .set_map(scen, "mvDemInp", di, fmp)
}

# Per-process input/output total = union of the (a)main + aux flow domains with
# the process key dropped, reduced to the commodity's native slice, mCommReg-cut.
.filter_proc_tot <- function(scen, name, main, aux, key, fmp) {
  m <- .gds(scen, main); a <- .gds(scen, aux)
  if (is.null(m) && is.null(a)) return(scen)
  pieces <- lapply(Filter(Negate(is.null), list(m, a)),
                   function(x) dplyr::select(x, -dplyr::any_of(key)))
  tot <- .reduce_total_map(.reduce_sect(dplyr::bind_rows(pieces)),
                           .gds(scen, "mCommSliceOrParent"))
  .set_map(scen, name, .filt_cr(scen, tot), fmp)
}
map_mTechInpTot <- function(scen, fmp) .filter_proc_tot(scen, "mTechInpTot", "mvTechInp", "mvTechAInp", "tech", fmp)
map_mTechOutTot <- function(scen, fmp) .filter_proc_tot(scen, "mTechOutTot", "mvTechOut", "mvTechAOut", "tech", fmp)

# mSupOutTot: supply availability domain with the leading `sup` column dropped.
map_mSupOutTot <- function(scen, fmp) {
  mSupAva <- .gds(scen, "mSupAva")
  if (is.null(mSupAva)) return(scen)
  .set_map(scen, "mSupOutTot", .reduce_sect(mSupAva[, -1, drop = FALSE]), fmp)
}

# mStorageInpTot / mStorageOutTot: legacy uses the SAME frame for both =
# reduce_sect(rbind(mvStorageAInp[,-1], mvStorageStore[,-1])).
.filter_storage_tot <- function(scen, name, fmp) {
  ai <- .gds(scen, "mvStorageAInp"); st <- .gds(scen, "mvStorageStore")
  if (is.null(ai) && is.null(st)) return(scen)
  pieces <- lapply(Filter(Negate(is.null), list(ai, st)),
                   function(x) x[, -1, drop = FALSE])
  .set_map(scen, name, .reduce_sect(dplyr::bind_rows(pieces)), fmp)
}
map_mStorageInpTot <- function(scen, fmp) .filter_storage_tot(scen, "mStorageInpTot", fmp)
map_mStorageOutTot <- function(scen, fmp) .filter_storage_tot(scen, "mStorageOutTot", fmp)

# -- auxiliary-conversion domains (mTech*2A* / mStorage*2A*) ---------------- #
# Each: relabel a conversion-factor parameter's aux commodity as `comm` and
# materialise dims through the relevant (already-built) flow domain, mCommReg-cut.
# spec: map name -> (conversion param, flow-domain map, second_comm flag).
.aux_conv_spec <- list(
  mTechAct2AInp     = list("pTechAct2AInp",     "mvTechAct",      FALSE),
  mTechAct2AOut     = list("pTechAct2AOut",     "mvTechAct",      FALSE),
  mTechCap2AInp     = list("pTechCap2AInp",     "mvTechAct",      FALSE),
  mTechCap2AOut     = list("pTechCap2AOut",     "mvTechAct",      FALSE),
  mTechNCap2AInp    = list("pTechNCap2AInp",    "mvTechAct",      FALSE),
  mTechNCap2AOut    = list("pTechNCap2AOut",    "mvTechAct",      FALSE),
  mTechCinp2AInp    = list("pTechCinp2AInp",    "mvTechInp",      TRUE),
  mTechCinp2AOut    = list("pTechCinp2AOut",    "mvTechOut",      TRUE),
  mTechCout2AInp    = list("pTechCout2AInp",    "mvTechInp",      TRUE),
  mTechCout2AOut    = list("pTechCout2AOut",    "mvTechOut",      TRUE),
  mStorageStg2AInp  = list("pStorageStg2AInp",  "mvStorageAInp",  FALSE),
  mStorageStg2AOut  = list("pStorageStg2AOut",  "mvStorageAOut",  FALSE),
  mStorageCinp2AInp = list("pStorageCinp2AInp", "mvStorageAInp",  FALSE),
  mStorageCinp2AOut = list("pStorageCinp2AOut", "mvStorageAOut",  FALSE),
  mStorageCout2AInp = list("pStorageCout2AInp", "mvStorageAInp",  FALSE),
  mStorageCout2AOut = list("pStorageCout2AOut", "mvStorageAOut",  FALSE),
  mStorageCap2AInp  = list("pStorageCap2AInp",  "mvStorageAInp",  FALSE),
  mStorageCap2AOut  = list("pStorageCap2AOut",  "mvStorageAOut",  FALSE),
  mStorageNCap2AInp = list("pStorageNCap2AInp", "mvStorageAInp",  FALSE),
  mStorageNCap2AOut = list("pStorageNCap2AOut", "mvStorageAOut",  FALSE)
)
.filter_aux_conv <- function(scen, name, fmp) {
  sp <- .aux_conv_spec[[name]]
  m <- .aux_conv_map(.gds(scen, sp[[1]]), .gds(scen, sp[[2]]), second_comm = sp[[3]])
  .set_map(scen, name, .filt_cr(scen, m), fmp)
}
map_mTechAct2AInp     <- function(scen, fmp) .filter_aux_conv(scen, "mTechAct2AInp", fmp)
map_mTechAct2AOut     <- function(scen, fmp) .filter_aux_conv(scen, "mTechAct2AOut", fmp)
map_mTechCap2AInp     <- function(scen, fmp) .filter_aux_conv(scen, "mTechCap2AInp", fmp)
map_mTechCap2AOut     <- function(scen, fmp) .filter_aux_conv(scen, "mTechCap2AOut", fmp)
map_mTechNCap2AInp    <- function(scen, fmp) .filter_aux_conv(scen, "mTechNCap2AInp", fmp)
map_mTechNCap2AOut    <- function(scen, fmp) .filter_aux_conv(scen, "mTechNCap2AOut", fmp)
map_mTechCinp2AInp    <- function(scen, fmp) .filter_aux_conv(scen, "mTechCinp2AInp", fmp)
map_mTechCinp2AOut    <- function(scen, fmp) .filter_aux_conv(scen, "mTechCinp2AOut", fmp)
map_mTechCout2AInp    <- function(scen, fmp) .filter_aux_conv(scen, "mTechCout2AInp", fmp)
map_mTechCout2AOut    <- function(scen, fmp) .filter_aux_conv(scen, "mTechCout2AOut", fmp)
map_mStorageStg2AInp  <- function(scen, fmp) .filter_aux_conv(scen, "mStorageStg2AInp", fmp)
map_mStorageStg2AOut  <- function(scen, fmp) .filter_aux_conv(scen, "mStorageStg2AOut", fmp)
map_mStorageCinp2AInp <- function(scen, fmp) .filter_aux_conv(scen, "mStorageCinp2AInp", fmp)
map_mStorageCinp2AOut <- function(scen, fmp) .filter_aux_conv(scen, "mStorageCinp2AOut", fmp)
map_mStorageCout2AInp <- function(scen, fmp) .filter_aux_conv(scen, "mStorageCout2AInp", fmp)
map_mStorageCout2AOut <- function(scen, fmp) .filter_aux_conv(scen, "mStorageCout2AOut", fmp)
map_mStorageCap2AInp  <- function(scen, fmp) .filter_aux_conv(scen, "mStorageCap2AInp", fmp)
map_mStorageCap2AOut  <- function(scen, fmp) .filter_aux_conv(scen, "mStorageCap2AOut", fmp)
map_mStorageNCap2AInp <- function(scen, fmp) .filter_aux_conv(scen, "mStorageNCap2AInp", fmp)
map_mStorageNCap2AOut <- function(scen, fmp) .filter_aux_conv(scen, "mStorageNCap2AOut", fmp)

# -- dummy import / export slack domains ----------------------------------- #
# (comm, region, year, slice) tuples with a finite dummy-slack cost (default Inf
# -> empty), mCommReg-restricted.
.filter_dummy <- function(scen, param, name, fmp) {
  d <- .gds(scen, param)
  if (is.null(d)) return(scen)
  d <- d[!is.na(d$value) & is.finite(d$value), , drop = FALSE]
  if (nrow(d) == 0) return(scen)
  d <- d[, intersect(c("comm", "region", "year", "slice"), colnames(d)),
         drop = FALSE]
  .set_map(scen, name, .filt_cr(scen, .reduce_dup(d)), fmp)
}
map_mDummyImport <- function(scen, fmp) .filter_dummy(scen, "pDummyImportCost", "mDummyImport", fmp)
map_mDummyExport <- function(scen, fmp) .filter_dummy(scen, "pDummyExportCost", "mDummyExport", fmp)

# -- emission-fuel domains -------------------------------------------------- #
# Links each technology's input commodity (commp) to the emission commodity it
# generates (comm) via pEmissionFactor; a zero combustion coeff (pTechEmisComm)
# excludes a tech. Builds mTechEmsFuel (membership-tagged, produced here) and its
# aggregate mEmsFuelTot. Registered under mEmsFuelTot; mTechEmsFuel is its
# side-effect (matches the legacy `if mEmsFuelTot %in% want` block).
map_mEmsFuelTot <- function(scen, fmp) {
  effac     <- .gds(scen, "pEmissionFactor")
  mvTechInp <- .gds(scen, "mvTechInp")
  if (is.null(mvTechInp) || is.null(effac)) return(scen)
  effac <- .reduce_dup(effac[!is.na(effac$value) & effac$value != 0, , drop = FALSE])
  if (nrow(effac) == 0) return(scen)
  ti <- dplyr::rename(mvTechInp, commp = "comm")   # input fuel -> commp for join
  emis <- .gds(scen, "pTechEmisComm")
  if (!is.null(emis)) {
    zero <- emis[!is.na(emis$value) & emis$value == 0, c("tech", "comm"), drop = FALSE]
    if (nrow(zero) > 0) {
      zero <- dplyr::rename(zero, commp = "comm")
      ti <- dplyr::anti_join(ti, zero, by = c("tech", "commp"))
    }
  }
  ems  <- merge0(effac, ti, by = "commp")
  keep <- intersect(c("tech", "comm", "commp", "region", "year", "slice"),
                    colnames(ems))
  ems <- .filt_cr(scen, as.data.frame(ems)[, keep, drop = FALSE])
  if (is.null(ems) || nrow(ems) == 0) return(scen)
  scen <- .set_map(scen, "mTechEmsFuel",
                   dplyr::rename(dplyr::distinct(ems), comm.1 = "commp"), fmp)
  tot <- .reduce_total_map(.reduce_sect(ems, c("comm", "region", "year", "slice")),
                           .gds(scen, "mCommSliceOrParent"))
  .set_map(scen, "mEmsFuelTot", tot, fmp)
}

# -- supply availability upper-bound domain -------------------------------- #
# mSupAvaUp: restrict mSupAva to (sup,comm,region,year) keys with a finite
# non-default upper bound. (The lower-bound sibling meqSupAvaLo is a constraint
# map produced by the same derivation via setm_any in recipe_filter.)
map_mSupAvaUp <- function(scen, fmp) {
  mSupAva <- .gds(scen, "mSupAva"); pSupAva <- .gds(scen, "pSupAva")
  if (is.null(mSupAva) || is.null(pSupAva)) return(scen)
  sk <- intersect(c("sup", "comm", "region", "year"), colnames(pSupAva))
  up <- unique(pSupAva[pSupAva$type == "up" & !is.na(pSupAva$value) &
                         is.finite(pSupAva$value) & pSupAva$value != 0,
                       sk, drop = FALSE])
  if (nrow(up) == 0) return(scen)
  .set_map(scen, "mSupAvaUp",
           .filt_cr(scen, .reduce_dup(dplyr::inner_join(mSupAva, up, by = sk))), fmp)
}

# -- import / export row flow domains -------------------------------------- #
# .io_row_maps returns {row, up, lo, cumup}; the filter maps take row/up (the
# lo/cumup constraint siblings are built by recipe_filter's setm_any).
.filter_row_bundle <- function(scen, key, slice_m, comm_m, prow, pres) {
  .io_row_maps(key, .gds(scen, slice_m), .gds(scen, comm_m),
               .gds(scen, prow), .gds(scen, pres),
               as.character(scen@settings@region),
               as.integer(scen@settings@horizon@intervals$mid))
}
map_mImportRow   <- function(scen, fmp)
  .set_map(scen, "mImportRow",
           .filt_cr(scen, .filter_row_bundle(scen, "imp", "mImpSlice", "mImpComm",
                                             "pImportRow", "pImportRowRes")$row), fmp)
map_mImportRowUp <- function(scen, fmp)
  .set_map(scen, "mImportRowUp",
           .filt_cr(scen, .filter_row_bundle(scen, "imp", "mImpSlice", "mImpComm",
                                             "pImportRow", "pImportRowRes")$up), fmp)
map_mExportRow   <- function(scen, fmp)
  .set_map(scen, "mExportRow",
           .filt_cr(scen, .filter_row_bundle(scen, "expp", "mExpSlice", "mExpComm",
                                             "pExportRow", "pExportRowRes")$row), fmp)
map_mExportRowUp <- function(scen, fmp)
  .set_map(scen, "mExportRowUp",
           .filt_cr(scen, .filter_row_bundle(scen, "expp", "mExpSlice", "mExpComm",
                                             "pExportRow", "pExportRowRes")$up), fmp)

# -- registry for the filter family (dependency order) --------------------- #
# Core flow domains first, then derived totals; remaining derived maps are still
# served by the recipe_filter fallback until migrated.
.filter_builders <- list(
  # core
  mvTechAct      = map_mvTechAct,
  mvTechInp      = map_mvTechInp,
  mvTechOut      = map_mvTechOut,
  mTechOutRY     = map_mTechOutRY,
  mvTechAInp     = map_mvTechAInp,
  mvTechAOut     = map_mvTechAOut,
  mSupAva        = map_mSupAva,
  mvStorageStore = map_mvStorageStore,
  mvStorageAInp  = map_mvStorageAInp,
  mvStorageAOut  = map_mvStorageAOut,
  # derived totals
  mvDemInp       = map_mvDemInp,
  mTechInpTot    = map_mTechInpTot,
  mTechOutTot    = map_mTechOutTot,
  mSupOutTot     = map_mSupOutTot,
  mStorageInpTot = map_mStorageInpTot,
  mStorageOutTot = map_mStorageOutTot,
  # aux-conversion
  mTechAct2AInp     = map_mTechAct2AInp,
  mTechAct2AOut     = map_mTechAct2AOut,
  mTechCap2AInp     = map_mTechCap2AInp,
  mTechCap2AOut     = map_mTechCap2AOut,
  mTechNCap2AInp    = map_mTechNCap2AInp,
  mTechNCap2AOut    = map_mTechNCap2AOut,
  mTechCinp2AInp    = map_mTechCinp2AInp,
  mTechCinp2AOut    = map_mTechCinp2AOut,
  mTechCout2AInp    = map_mTechCout2AInp,
  mTechCout2AOut    = map_mTechCout2AOut,
  mStorageStg2AInp  = map_mStorageStg2AInp,
  mStorageStg2AOut  = map_mStorageStg2AOut,
  mStorageCinp2AInp = map_mStorageCinp2AInp,
  mStorageCinp2AOut = map_mStorageCinp2AOut,
  mStorageCout2AInp = map_mStorageCout2AInp,
  mStorageCout2AOut = map_mStorageCout2AOut,
  mStorageCap2AInp  = map_mStorageCap2AInp,
  mStorageCap2AOut  = map_mStorageCap2AOut,
  mStorageNCap2AInp = map_mStorageNCap2AInp,
  mStorageNCap2AOut = map_mStorageNCap2AOut,
  # dummy + emission (mEmsFuelTot also builds mTechEmsFuel as a side-effect)
  mDummyImport   = map_mDummyImport,
  mDummyExport   = map_mDummyExport,
  mEmsFuelTot    = map_mEmsFuelTot,
  # supply-ava + import/export row (lo/cumup constraint siblings stay on fallback)
  mSupAvaUp      = map_mSupAvaUp,
  mImportRow     = map_mImportRow,
  mImportRowUp   = map_mImportRowUp,
  mExportRow     = map_mExportRow,
  mExportRowUp   = map_mExportRowUp
)
