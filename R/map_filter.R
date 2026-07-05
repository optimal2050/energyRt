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
# The family is FULLY migrated: core flow domains, derived totals / aggregations,
# aux-conversion, import/export-row, inter-regional trade (mTradeIr), the coarse-
# slice substitution chain (*2Lo) and the commodity balance domains all build
# here in `.filter_builders` order. A handful of constraint-recipe maps
# (meqSupAvaLo, meq{Import,Export}RowLo, m{Import,Export}RowCumUp) and value-recipe
# maps (mInpSub/mOutSub, the *Cost maps) plus mTradeRoutes share a derivation with
# a filter map and are emitted as SIDE-EFFECTS of that sibling here (recipe_value /
# recipe_constraint skip them). The legacy recipe_filter fallback has been retired
# (archived to drafts/legacy-mapping/filter.R).
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

# [agg-rewrite] map_mTechOutRY removed (*RY retired — dead reporting)

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
# mCommSliceOrParent (.reduce_total_map), in dependency order.
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
# Commodity-slice classification of a flow domain: join the flow's (key, comm,
# slice) to mCommSliceOrParent (slicep = the flow's child slice, slice = the
# commodity's native/parent slice) and split by whether the flow runs at the
# commodity's native slice (`SameSlice`) or a finer one needing aggregation
# (`AggSlice` keeps slice/slicep, `Agg` drops them). These membership-tagged maps
# depend on the filter flow domains, so they are emitted here (in the filter
# recipe) as side-effects of the totals builders; recipe_membership skips them.
# Faithful port of the legacy write.R block (L361-494).
.comm_slice_class_maps <- function(scen, flow, base, fmp) {
  mv <- .gds(scen, flow); csop <- .gds(scen, "mCommSliceOrParent")
  if (is.null(mv) || is.null(csop)) return(scen)
  ags <- dplyr::distinct(dplyr::select(mv, -dplyr::any_of(c("region", "year"))))
  ags <- as.data.frame(dplyr::left_join(ags, csop,
            by = c("comm", "slice" = "slicep"), suffix = c("", "p")))
  ags <- ags[!is.na(ags$slicep), , drop = FALSE]
  same <- ags[ags$slicep == ags$slice, , drop = FALSE]   # flow at commodity's native slice
  aggs <- ags[ags$slicep != ags$slice, , drop = FALSE]   # flow finer than native -> aggregate
  scen <- .set_map(scen, paste0(base, "SameSlice"), same, fmp)  # dims: key, comm
  scen <- .set_map(scen, paste0(base, "AggSlice"),  aggs, fmp)  # dims: key, comm, slice, slicep
  .set_map(scen, paste0(base, "Agg"), aggs, fmp)               # dims: key, comm
}

map_mTechInpTot <- function(scen, fmp) {
  scen <- .filter_proc_tot(scen, "mTechInpTot", "mvTechInp", "mvTechAInp", "tech", fmp)
  scen <- .comm_slice_class_maps(scen, "mvTechInp",  "mTechInpComm",  fmp)
  .comm_slice_class_maps(scen, "mvTechAInp", "mTechAInpComm", fmp)
}
map_mTechOutTot <- function(scen, fmp) {
  scen <- .filter_proc_tot(scen, "mTechOutTot", "mvTechOut", "mvTechAOut", "tech", fmp)
  scen <- .comm_slice_class_maps(scen, "mvTechOut",  "mTechOutComm",  fmp)
  .comm_slice_class_maps(scen, "mvTechAOut", "mTechAOutComm", fmp)
}

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
# mDummy*Cost (value-recipe sibling): project the dummy flow domain onto the
# cost param's dims. Built here as a side-effect (recipe_value skips it).
.cost_sect <- function(scen, src_nm, nm, fmp) {
  d <- .gds(scen, src_nm); p <- scen@modInp@parameters[[nm]]
  if (is.null(d) || is.null(p)) return(scen)
  .set_map(scen, nm, .reduce_sect(d, p@dimSets), fmp)
}
map_mDummyImport <- function(scen, fmp) {
  scen <- .filter_dummy(scen, "pDummyImportCost", "mDummyImport", fmp)
  .cost_sect(scen, "mDummyImport", "mDummyImportCost", fmp)
}
map_mDummyExport <- function(scen, fmp) {
  scen <- .filter_dummy(scen, "pDummyExportCost", "mDummyExport", fmp)
  .cost_sect(scen, "mDummyExport", "mDummyExportCost", fmp)
}

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
# non-default upper bound. The lower-bound sibling meqSupAvaLo is a constraint
# map sharing this derivation; it is emitted here as a side-effect (recipe_constraint
# skips it).
map_mSupAvaUp <- function(scen, fmp) {
  mSupAva <- .gds(scen, "mSupAva"); pSupAva <- .gds(scen, "pSupAva")
  if (is.null(mSupAva) || is.null(pSupAva)) return(scen)
  sk <- intersect(c("sup", "comm", "region", "year"), colnames(pSupAva))
  up <- unique(pSupAva[pSupAva$type == "up" & !is.na(pSupAva$value) &
                         is.finite(pSupAva$value) & pSupAva$value != 0,
                       sk, drop = FALSE])
  if (nrow(up) > 0) {
    scen <- .set_map(scen, "mSupAvaUp",
             .filt_cr(scen, .reduce_dup(.fold_join(mSupAva, up))), fmp)
  }
  # meqSupAvaLo (constraint-recipe sibling): same derivation, lower-bound keys.
  lo <- unique(pSupAva[pSupAva$type == "lo" & !is.na(pSupAva$value) &
                         pSupAva$value != 0, sk, drop = FALSE])
  if (nrow(lo) > 0) {
    scen <- .set_map(scen, "meqSupAvaLo",
             .filt_cr(scen, .reduce_dup(.fold_join(mSupAva, lo))), fmp)
  }
  scen
}

# -- import / export row flow domains -------------------------------------- #
# .io_row_maps returns {row, up, lo, cumup}; map_m*Row persists row + the lo/cumup
# constraint siblings (meq*RowLo, m*RowCumUp) as side-effects, map_m*RowUp the up.
.filter_row_bundle <- function(scen, key, slice_m, comm_m, prow, pres) {
  .io_row_maps(key, .gds(scen, slice_m), .gds(scen, comm_m),
               .gds(scen, prow), .gds(scen, pres),
               as.character(scen@settings@region),
               as.integer(scen@settings@horizon@intervals$mid))
}
# m*RowCost (value-recipe sibling): unique (key, region, year) of the row flow
# domain, intersected with the model's region x year grid (recipe_value skips it).
.cost_row <- function(scen, src_nm, nm, fmp) {
  d <- .gds(scen, src_nm); p <- scen@modInp@parameters[[nm]]
  regions    <- as.character(scen@settings@region)
  milestones <- as.integer(scen@settings@horizon@intervals$mid)
  if (is.null(d) || is.null(p) ||
      length(regions) == 0 || length(milestones) == 0) return(scen)
  dregionyear <- expand.grid(region = regions, year = as.integer(milestones),
                             stringsAsFactors = FALSE)
  x <- .reduce_sect(d, p@dimSets)
  .set_map(scen, nm, as.data.frame(merge0(x, dregionyear)), fmp)
}
# map_mImportRow / map_mExportRow build the row domain AND its constraint-recipe
# siblings (meq*RowLo, m*RowCumUp) and value-recipe cost (m*RowCost). The lo/cumup
# maps share .io_row_maps; cumup is NOT mCommReg-filtered (legacy parity).
map_mImportRow <- function(scen, fmp) {
  b <- .filter_row_bundle(scen, "imp", "mImpSlice", "mImpComm",
                          "pImportRow", "pImportRowRes")
  scen <- .set_map(scen, "mImportRow", .filt_cr(scen, b$row), fmp)
  scen <- .set_map(scen, "meqImportRowLo", .filt_cr(scen, b$lo), fmp)
  scen <- .set_map(scen, "mImportRowCumUp", b$cumup, fmp)
  .cost_row(scen, "mImportRow", "mImportRowCost", fmp)
}
map_mImportRowUp <- function(scen, fmp)
  .set_map(scen, "mImportRowUp",
           .filt_cr(scen, .filter_row_bundle(scen, "imp", "mImpSlice", "mImpComm",
                                             "pImportRow", "pImportRowRes")$up), fmp)
map_mExportRow <- function(scen, fmp) {
  b <- .filter_row_bundle(scen, "expp", "mExpSlice", "mExpComm",
                          "pExportRow", "pExportRowRes")
  scen <- .set_map(scen, "mExportRow", .filt_cr(scen, b$row), fmp)
  scen <- .set_map(scen, "meqExportRowLo", .filt_cr(scen, b$lo), fmp)
  scen <- .set_map(scen, "mExportRowCumUp", b$cumup, fmp)
  .cost_row(scen, "mExportRow", "mExportRowCost", fmp)
}
map_mExportRowUp <- function(scen, fmp)
  .set_map(scen, "mExportRowUp",
           .filt_cr(scen, .filter_row_bundle(scen, "expp", "mExpSlice", "mExpComm",
                                             "pExportRow", "pExportRowRes")$up), fmp)

# -- inter-regional trade flow domains (mTradeIr family) ------------------- #
# Faithful port of the recipe_filter trade block (mapping_engine.R L1295-1438).
# mTradeIr = per-trade routes x slices x years; the year domain is the trade's
# operation span (mTradeSpan) for capacity-variable trades, else all milestones.
# Each trade's routes + capacityVariable flag come straight from the object
# (apply_to_scenario_data); every downstream map reads mTradeIr back from scen.

.trade_route_info <- function(scen) {
  apply_to_scenario_data(
    scen = scen, classes = "trade", as_list = TRUE,
    func = function(x) {
      rt <- x@routes
      if (is.null(rt) || nrow(rt) == 0) return(list())
      o <- list()
      o[[x@name]] <- list(
        routes = data.frame(
          trade = x@name, src = as.character(rt$src),
          dst = as.character(rt$dst), stringsAsFactors = FALSE),
        capvar = isTRUE(x@capacityVariable))
      o
    })
}

# mImportIrCost / mExportIrCost: trade flow domain with the destination / source
# region renamed to `region`, projected onto the param's dims and intersected
# with the model's region x year grid. Value-recipe maps built here (recipe_value
# runs before filter, so the source map is absent there; recipe_value skips them).
.trade_ir_cost <- function(scen, ren_from, nm, fmp) {
  mti <- .gds(scen, "mTradeIr")
  p <- scen@modInp@parameters[[nm]]
  regions    <- as.character(scen@settings@region)
  milestones <- as.integer(scen@settings@horizon@intervals$mid)
  if (is.null(mti) || is.null(p) ||
      length(regions) == 0 || length(milestones) == 0) return(scen)
  dregionyear <- expand.grid(region = regions, year = as.integer(milestones),
                             stringsAsFactors = FALSE)
  names(mti)[names(mti) == ren_from] <- "region"
  keep <- intersect(p@dimSets, colnames(mti))
  x <- .reduce_sect(mti[, keep, drop = FALSE])
  .set_map(scen, nm, as.data.frame(merge0(x, dregionyear)), fmp)
}

map_mTradeIr <- function(scen, fmp) {
  trade_info <- .trade_route_info(scen)
  if (length(trade_info) == 0) return(scen)
  # mTradeRoutes (constraint-recipe intermediate): the per-trade (src, dst)
  # pairs. Built here because the inter-regional flow maps below depend on it AND
  # fold_scenario_parameters() reads it to fold the trade aux-coefficient params.
  # recipe_constraint skips it (.constraint_maps_built_elsewhere).
  routes_all <- do.call(rbind, lapply(trade_info, `[[`, "routes"))
  rownames(routes_all) <- NULL
  scen <- .set_map(scen, "mTradeRoutes", .reduce_dup(routes_all), fmp)
  trade_slice <- .gds(scen, "mTradeSlice")
  trade_span  <- .gds(scen, "mTradeSpan")
  milestones  <- as.integer(scen@settings@horizon@intervals$mid)
  if (is.null(trade_slice)) return(scen)
  ir_pieces <- list()
  for (nm in names(trade_info)) {
    rt <- trade_info[[nm]]$routes
    sl <- trade_slice[trade_slice$trade == nm, , drop = FALSE]
    if (nrow(sl) == 0) next
    base <- as.data.frame(merge0(rt, sl))      # trade, src, dst, slice
    if (trade_info[[nm]]$capvar && !is.null(trade_span)) {
      yrs <- trade_span$year[trade_span$trade == nm]
    } else {
      yrs <- milestones
    }
    if (length(yrs) == 0) next
    ir_pieces[[nm]] <- as.data.frame(
      merge0(base, data.frame(year = as.integer(yrs))))
  }
  if (length(ir_pieces) == 0) return(scen)
  mTradeIr <- .reduce_dup(do.call(rbind, ir_pieces))
  mTradeIr <- mTradeIr[, c("trade", "src", "dst", "year", "slice"),
                       drop = FALSE]
  scen <- .set_map(scen, "mTradeIr", mTradeIr, fmp)
  # value-recipe cost siblings (read mTradeIr back from scen)
  scen <- .trade_ir_cost(scen, "dst", "mImportIrCost", fmp)
  scen <- .trade_ir_cost(scen, "src", "mExportIrCost", fmp)
  scen
}

# mvTradeIr: mTradeIr tagged with the traded commodity (via mTradeComm).
map_mvTradeIr <- function(scen, fmp) {
  mTradeIr <- .gds(scen, "mTradeIr"); trade_comm <- .gds(scen, "mTradeComm")
  if (is.null(mTradeIr) || is.null(trade_comm)) return(scen)
  mv <- as.data.frame(merge0(mTradeIr, trade_comm))
  mv <- mv[, c("trade", "comm", "src", "dst", "year", "slice"), drop = FALSE]
  .set_map(scen, "mvTradeIr", mv, fmp)
}

# membership: mTradeIrAInp / mTradeIrAOut (trade, comm) = the aux commodities
# with a non-zero src/dst conversion coefficient for the direction.
.trade_nz_trade_comm <- function(p) {
  if (is.null(p)) return(NULL)
  p <- as.data.frame(p)
  p <- p[!is.na(p$value) & p$value != 0, , drop = FALSE]
  if (nrow(p) == 0) return(NULL)
  .reduce_dup(p[, c("trade", "acomm"), drop = FALSE])
}
.trade_aux_membership <- function(scen, name, ps, fmp) {
  m <- .reduce_dup(do.call(rbind, lapply(ps,
                   function(pn) .trade_nz_trade_comm(.gds(scen, pn)))))
  if (is.null(m) || nrow(m) == 0) return(scen)
  names(m)[names(m) == "acomm"] <- "comm"
  .set_map(scen, name, m, fmp)
}
map_mTradeIrAInp <- function(scen, fmp)
  .trade_aux_membership(scen, "mTradeIrAInp",
                        c("pTradeIrCsrc2Ainp", "pTradeIrCdst2Ainp"), fmp)
map_mTradeIrAOut <- function(scen, fmp)
  .trade_aux_membership(scen, "mTradeIrAOut",
                        c("pTradeIrCsrc2Aout", "pTradeIrCdst2Aout"), fmp)

# per-route aux-coefficient domains: project the interpolated coefficient param
# onto mTradeIr (.trade_aux_derived). Empty -> NULL -> no-op.
map_mTradeIrCsrc2Ainp <- function(scen, fmp)
  .set_map(scen, "mTradeIrCsrc2Ainp",
           .trade_aux_derived(.gds(scen, "pTradeIrCsrc2Ainp"),
                              .gds(scen, "mTradeIr")), fmp)
map_mTradeIrCdst2Ainp <- function(scen, fmp)
  .set_map(scen, "mTradeIrCdst2Ainp",
           .trade_aux_derived(.gds(scen, "pTradeIrCdst2Ainp"),
                              .gds(scen, "mTradeIr")), fmp)
map_mTradeIrCsrc2Aout <- function(scen, fmp)
  .set_map(scen, "mTradeIrCsrc2Aout",
           .trade_aux_derived(.gds(scen, "pTradeIrCsrc2Aout"),
                              .gds(scen, "mTradeIr")), fmp)
map_mTradeIrCdst2Aout <- function(scen, fmp)
  .set_map(scen, "mTradeIrCdst2Aout",
           .trade_aux_derived(.gds(scen, "pTradeIrCdst2Aout"),
                              .gds(scen, "mTradeIr")), fmp)

# region projection of a per-route coefficient map (src/dst -> region).
.trade_proj_region <- function(m, dim_col) {
  if (is.null(m) || nrow(m) == 0) return(NULL)
  a <- .reduce_dup(m[, c("trade", "comm", dim_col, "year", "slice"),
                     drop = FALSE])
  names(a)[names(a) == dim_col] <- "region"
  a
}
map_mvTradeIrAInp <- function(scen, fmp) {
  mv <- .reduce_dup(do.call(rbind, list(
    .trade_proj_region(.gds(scen, "mTradeIrCsrc2Ainp"), "src"),
    .trade_proj_region(.gds(scen, "mTradeIrCdst2Ainp"), "dst"))))
  .set_map(scen, "mvTradeIrAInp", mv, fmp)
}
map_mvTradeIrAOut <- function(scen, fmp) {
  mv <- .reduce_dup(do.call(rbind, list(
    .trade_proj_region(.gds(scen, "mTradeIrCsrc2Aout"), "src"),
    .trade_proj_region(.gds(scen, "mTradeIrCdst2Aout"), "dst"))))
  .set_map(scen, "mvTradeIrAOut", mv, fmp)
}

# inter-regional trade aux totals: reduce to the commodity's native slice.
.trade_aux_tot <- function(scen, src, name, fmp) {
  mv <- .gds(scen, src)
  if (is.null(mv)) return(scen)
  .set_map(scen, name,
           .reduce_total_map(.reduce_sect(mv, c("comm", "region", "year",
                                                "slice")),
                             .gds(scen, "mCommSliceOrParent")), fmp)
}
map_mvTradeIrAInpTot <- function(scen, fmp)
  .trade_aux_tot(scen, "mvTradeIrAInp", "mvTradeIrAInpTot", fmp)
map_mvTradeIrAOutTot <- function(scen, fmp)
  .trade_aux_tot(scen, "mvTradeIrAOut", "mvTradeIrAOutTot", fmp)

# -- aggregate commodity domains ------------------------------------------- #
# mAggregateFactor: (comm, comm.1) linking each aggregate commodity to its
# component commodity (pAggregateFactor value != 0).
map_mAggregateFactor <- function(scen, fmp) {
  agg <- .gds(scen, "pAggregateFactor")
  if (is.null(agg)) return(scen)
  agg <- agg[!is.na(agg$value) & agg$value != 0, , drop = FALSE]
  if (nrow(agg) == 0) return(scen)
  af <- dplyr::distinct(dplyr::rename(
    agg[, c("comm", "commp"), drop = FALSE], comm.1 = "commp"))
  .set_map(scen, "mAggregateFactor", af, fmp)
}

# mAggOut: aggregate-output total = aggregate commodities x region x year x all
# slices, reduced to each commodity's native slice.
map_mAggOut <- function(scen, fmp) {
  agg <- .gds(scen, "pAggregateFactor"); comm_slice <- .gds(scen, "mCommSlice")
  regions    <- as.character(scen@settings@region)
  milestones <- as.integer(scen@settings@horizon@intervals$mid)
  if (is.null(agg) || is.null(comm_slice) ||
      length(regions) == 0 || length(milestones) == 0) return(scen)
  slices <- unique(comm_slice$slice)
  a <- .reduce_sect(agg, "comm")
  a <- merge0(a, data.frame(region = regions, stringsAsFactors = FALSE))
  a <- merge0(as.data.frame(a), data.frame(year = milestones))
  a <- merge0(as.data.frame(a),
              data.frame(slice = as.character(slices), stringsAsFactors = FALSE))
  .set_map(scen, "mAggOut",
           .reduce_total_map(.reduce_dup(a), .gds(scen, "mCommSliceOrParent")),
           fmp)
}

# -- import / export to Rest-of-World flow domains ------------------------- #
# mExport / mImport: the (comm, region, year, slice) domains for the row exchange
# (m*Row) unioned with the inter-regional trade flows (mTradeIr x mTradeComm),
# each remapped to the commodity's own slice via mCommSliceOrParent. Export keeps
# the source region (src), import keeps the destination (dst).
.io_rowir_union <- function(scen, row_nm, region_col, drop_col) {
  csop <- .gds(scen, "mCommSliceOrParent")
  ry_cols <- c("comm", "region", "year", "slice")
  csop2 <- csop
  if (!is.null(csop2)) names(csop2)[names(csop2) == "slicep"] <- "slice.1"
  io_row <- function() {
    d <- .gds(scen, row_nm)
    if (is.null(d) || is.null(csop2)) return(NULL)
    x <- .reduce_sect(as.data.frame(d)[, ry_cols, drop = FALSE], ry_cols)
    names(x)[names(x) == "slice"] <- "slice.1"
    out <- as.data.frame(merge0(x, csop2))    # by comm, slice.1 -> native slice
    if (nrow(out) == 0) return(NULL)
    .reduce_dup(out[, ry_cols, drop = FALSE])
  }
  io_ir <- function() {
    mTradeComm_d <- .gds(scen, "mTradeComm"); mTradeIr_d <- .gds(scen, "mTradeIr")
    if (is.null(mTradeComm_d) || is.null(mTradeIr_d) || is.null(csop2)) {
      return(NULL)
    }
    trd <- as.data.frame(merge0(mTradeComm_d, mTradeIr_d))
    names(trd)[names(trd) == "slice"] <- "slice.1"
    trd[[drop_col]] <- NULL
    names(trd)[names(trd) == region_col] <- "region"
    out <- as.data.frame(merge0(trd, csop2))  # by comm, slice.1 -> native slice
    if (nrow(out) == 0) return(NULL)
    .reduce_dup(out[, ry_cols, drop = FALSE])
  }
  .reduce_dup(dplyr::bind_rows(io_row(), io_ir()))
}
map_mExport <- function(scen, fmp)
  .set_map(scen, "mExport",
           .io_rowir_union(scen, "mExportRow", "src", "dst"), fmp)
map_mImport <- function(scen, fmp)
  .set_map(scen, "mImport",
           .io_rowir_union(scen, "mImportRow", "dst", "src"), fmp)

# -- coarser-than-native flow substitution chain (*2Lo) -------------------- #
# A flow total tagged at a slice coarser than its commodity's native resolution
# (e.g. an ANNUAL output of a seasonal commodity) must be redistributed to the
# native slices via a substitution variable. mInp2Lo/mOut2Lo = the coarse rows;
# mvInp2Lo/mvOut2Lo = those rows expanded to (coarse slice, native slice.1);
# mInpSub/mOutSub (value-recipe siblings) = the native-slice substitution domain.
.ry_cols <- c("comm", "region", "year", "slice")

# [agg-rewrite] *2Lo substitution chain removed (mInp2Lo/mOut2Lo/mvInp2Lo/
# mvOut2Lo + mInpSub/mOutSub builders & helpers). These maps were always 0 rows
# in well-formed models (processes are at-or-finer than their commodities), so
# the down-disaggregation they fed is dead code; up-aggregation in eqOutTot/
# eqInpTot (mSliceFamily/pSliceAgg) supersedes it across all engines.

# -- commodity balance domains -------------------------------------------- #
# mvInpTot / mvOutTot: union of all input- / output-side flow totals, restricted
# to each commodity's own slice level via mCommSlice. mvBalance = their union;
# the *RY maps are the year-resolution projections (slice dropped).
.bind_ry <- function(...) {
  fr <- Filter(function(x) !is.null(x) && nrow(as.data.frame(x)) > 0, list(...))
  if (length(fr) == 0) return(NULL)
  fr <- lapply(fr, function(x) {
    x <- as.data.frame(x); x[, intersect(.ry_cols, colnames(x)), drop = FALSE]
  })
  .reduce_dup(dplyr::bind_rows(fr))
}
.restrict_comm_slice <- function(df, comm_slice) {
  if (is.null(df) || nrow(df) == 0 || is.null(comm_slice)) return(df)
  dplyr::distinct(as.data.frame(merge0(df, comm_slice)))
}
map_mvInpTot <- function(scen, fmp) {
  inptot <- .restrict_comm_slice(.bind_ry(
    .gds(scen, "mvDemInp"), .gds(scen, "mDummyExport"), .gds(scen, "mTechInpTot"),
    .gds(scen, "mStorageInpTot"), .gds(scen, "mExport"),
    .gds(scen, "mvTradeIrAInpTot")),                    # [agg-rewrite] mInpSub dropped
    .gds(scen, "mCommSlice"))
  .set_map(scen, "mvInpTot", inptot, fmp)
}
map_mvOutTot <- function(scen, fmp) {
  outtot <- .restrict_comm_slice(.bind_ry(
    .gds(scen, "mDummyImport"), .gds(scen, "mSupOutTot"), .gds(scen, "mEmsFuelTot"),
    .gds(scen, "mAggOut"), .gds(scen, "mTechOutTot"), .gds(scen, "mStorageOutTot"),
    .gds(scen, "mImport"), .gds(scen, "mvTradeIrAOutTot")),  # [agg-rewrite] mOutSub dropped
    .gds(scen, "mCommSlice"))
  .set_map(scen, "mvOutTot", outtot, fmp)
}
.drop_slice_distinct <- function(scen, src, name, fmp) {
  d <- .gds(scen, src)
  if (is.null(d)) return(scen)
  .set_map(scen, name,
           dplyr::distinct(dplyr::select(d, -dplyr::any_of("slice"))), fmp)
}
map_mvBalance <- function(scen, fmp)
  .set_map(scen, "mvBalance",
           .bind_ry(.gds(scen, "mvInpTot"), .gds(scen, "mvOutTot")), fmp)
# [agg-rewrite] map_mInpTotRY/mOutTotRY/mBalanceRY removed (*RY retired)

# -- registry for the filter family (dependency order) --------------------- #
# Built in THIS order (build_mappings iterates the list): core flow domains, then
# derived totals / aux-conversion / dummy / emission / row / supply, then trade
# (mTradeIr family), aggregate + import/export union, and finally the *2Lo
# substitution chain and commodity balance domains. The whole filter family is
# registry-backed — there is no recipe_filter fallback.
.filter_builders <- list(
  # core
  mvTechAct      = map_mvTechAct,
  mvTechInp      = map_mvTechInp,
  mvTechOut      = map_mvTechOut,
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
  mExportRowUp   = map_mExportRowUp,
  # inter-regional trade (mTradeIr family; mImport/ExportIrCost side-effects)
  mTradeIr          = map_mTradeIr,
  mvTradeIr         = map_mvTradeIr,
  mTradeIrAInp      = map_mTradeIrAInp,
  mTradeIrAOut      = map_mTradeIrAOut,
  mTradeIrCsrc2Ainp = map_mTradeIrCsrc2Ainp,
  mTradeIrCdst2Ainp = map_mTradeIrCdst2Ainp,
  mTradeIrCsrc2Aout = map_mTradeIrCsrc2Aout,
  mTradeIrCdst2Aout = map_mTradeIrCdst2Aout,
  mvTradeIrAInp     = map_mvTradeIrAInp,
  mvTradeIrAOut     = map_mvTradeIrAOut,
  mvTradeIrAInpTot  = map_mvTradeIrAInpTot,
  mvTradeIrAOutTot  = map_mvTradeIrAOutTot,
  # aggregate + import/export ROW union (depend on mTradeIr + m*Row above)
  mAggregateFactor  = map_mAggregateFactor,
  mAggOut           = map_mAggOut,
  mExport           = map_mExport,
  mImport           = map_mImport,
  # [agg-rewrite] mInp2Lo/mOut2Lo/mvInp2Lo/mvOut2Lo builders removed (dead *2Lo)
  # commodity balance domains (depend on all totals)
  mvInpTot          = map_mvInpTot,
  mvOutTot          = map_mvOutTot,
  mvBalance         = map_mvBalance
)
