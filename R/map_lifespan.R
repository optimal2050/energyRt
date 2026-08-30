# =========================================================================== #
# map_lifespan.R  —  lifespan-window mapping builders (family "lifespan")
#
# One `map_<Name>(scen, fmp) -> scen` per lifespan map. New = investment-year
# window; Span = invest UNION stock years; Eac = Span; OlifeInf = (obj[,region])
# whose operational life is infinite. Plus the technology capacity-retirement maps
# (gated on settings@optimizeRetirement). Registered in `.lifespan_builders`.
#
# Reuses the shared helpers / family table still defined in mapping_engine.R
# (`.lifespan_family_def`, `.set_lifespan_map`, `.lifespan_olife_inf`,
# `.lifespan_retirement_tech`) and the window accessors get_process_invest_years()
# / get_process_years() (interp.R). Those are archived/relocated in the Phase 4
# sweep; here they are simply called.
# =========================================================================== #

# Family table: object family -> key column, whether region-indexed, and the
# new / span / eac / inf map names. (Relocated from mapping_engine.R.)
.lifespan_family_def <- list(
  technology = list(key = "tech",  region = TRUE,
                    new = "mTechNew",    span = "mTechSpan",
                    eac = "mTechEac",    inf  = "mTechOlifeInf",
                    ret_eqnew = "meqTechRetiredNewCap",
                    ret_stock = "mvTechRetiredStock",
                    ret_pairs = "mvTechRetiredNewCap",
                    phaseout = "mvTechPhaseOut"),
  storage    = list(key = "stg",   region = TRUE,
                    new = "mStorageNew", span = "mStorageSpan",
                    eac = "mStorageEac", inf  = "mStorageOlifeInf",
                    # One set for all three parts: olife is storage-wide.
                    ret_eqnew = "meqStorageRetiredNewCap",
                    ret_stock = "mvStorageRetiredStock",
                    ret_pairs = "mvStorageRetiredNewCap",
                    phaseout = "mvStoragePhaseOut"),
  trade      = list(key = "trade", region = FALSE,
                    new = "mTradeNew",   span = "mTradeSpan",
                    eac = NA_character_, inf  = "mTradeOlifeInf",
                    ret_eqnew = "meqTradeRetiredNewCap",
                    ret_stock = "mvTradeRetiredStock",
                    ret_pairs = "mvTradeRetiredNewCap",
                    phaseout  = "mvTradePhaseOut")
)

# process -> class tibble (column needed to route windows to a family).
.lifespan_cls_map <- function(scen) {
  get_process_class(scen) |>
    named_list_to_df(col_names = c("process", "class")) |>
    dplyr::as_tibble()
}

# New / Span / Eac window map for one object family (kind = "new" | "span").
.lifespan_window_map <- function(scen, name, cls, kind, fmp) {
  f   <- .lifespan_family_def[[cls]]
  win <- if (kind == "new") get_process_invest_years(scen) else get_process_years(scen)
  win <- win |>
    dplyr::as_tibble() |>
    dplyr::left_join(.lifespan_cls_map(scen), by = "process") |>
    dplyr::filter(.data$class == cls) |>
    dplyr::select(-"class")
  .set_lifespan_map(scen, name, win, f$key, fmp)
}

# Infinite-operational-life membership map for one object family.
.lifespan_inf_map <- function(scen, name, cls, fmp) {
  f  <- .lifespan_family_def[[cls]]
  df <- .lifespan_olife_inf(scen, cls, f$key, f$region,
                            as.character(scen@settings@region))
  .set_lifespan_map(scen, name, df, f$key, fmp)
}

# mStorageOlifeInf: (stg, region) for storages with a FINITE olife, over their
# operating regions (mStorageSpan). NOTE: storage uses the OPPOSITE convention to
# technology (which lists INFINITE-olife). It is redundant with the
# `ordYear < pStorageOlife + ordYear[yp]` clause in eqStorageOutCap, so it is
# behaviour-neutral; ported for v0.51 parity. Faithful port of obj2modInp.R:1055.
map_mStorageOlifeInf <- function(scen, fmp) {
  span <- .gds(scen, "mStorageSpan")
  if (is.null(span)) return(scen)
  fin <- apply_to_scenario_data(
    scen = scen, classes = "storage", as_list = TRUE,
    func = function(x) {
      ol <- .lifespan_resolve(x, "olife")
      if (nrow(ol) == 0 || all(is.infinite(ol$olife))) return(NULL)
      o <- list(); o[[x@name]] <- data.frame(stg = x@name, stringsAsFactors = FALSE)
      o
    })
  fdf <- dplyr::bind_rows(fin)
  if (is.null(fdf) || nrow(fdf) == 0) return(scen)
  sr <- dplyr::distinct(dplyr::select(span, dplyr::any_of(c("stg", "region"))))
  df <- dplyr::distinct(dplyr::inner_join(sr, fdf, by = "stg"))
  .set_map(scen, "mStorageOlifeInf", df, fmp)
}

# One technology capacity-retirement map (delegates to the shared builder, which
# self-gates on settings@optimizeRetirement and builds only the requested name).
.lifespan_retire_one <- function(scen, name, fmp, cls = "technology") {
  clsm <- .lifespan_cls_map(scen)
  win <- function(accessor) {
    accessor(scen) |>
      dplyr::as_tibble() |>
      dplyr::left_join(clsm, by = "process") |>
      dplyr::filter(.data$class == cls) |>
      dplyr::select(-"class")
  }
  .lifespan_retirement(scen, name, fmp,
                       win(get_process_invest_years),
                       win(get_process_years),
                       as.character(scen@settings@region),
                       cls)
}

# -- per-mapping entry points ---------------------------------------------- #
map_mTechNew         <- function(scen, fmp) .lifespan_window_map(scen, "mTechNew",      "technology", "new",  fmp)
map_mTechSpan        <- function(scen, fmp) .lifespan_window_map(scen, "mTechSpan",     "technology", "span", fmp)
map_mTechEac         <- function(scen, fmp) .lifespan_window_map(scen, "mTechEac",      "technology", "span", fmp)
map_mTechOlifeInf    <- function(scen, fmp) .lifespan_inf_map(scen,    "mTechOlifeInf", "technology",         fmp)
map_mStorageNew      <- function(scen, fmp) .lifespan_window_map(scen, "mStorageNew",      "storage", "new",  fmp)
map_mStorageSpan     <- function(scen, fmp) .lifespan_window_map(scen, "mStorageSpan",     "storage", "span", fmp)
map_mStorageEac      <- function(scen, fmp) .lifespan_window_map(scen, "mStorageEac",      "storage", "span", fmp)
# map_mStorageOlifeInf defined above (storage uses the finite-olife convention).
map_mTradeNew        <- function(scen, fmp) .lifespan_window_map(scen, "mTradeNew",      "trade", "new",  fmp)
map_mTradeSpan       <- function(scen, fmp) .lifespan_window_map(scen, "mTradeSpan",     "trade", "span", fmp)
map_mTradeOlifeInf   <- function(scen, fmp) .lifespan_inf_map(scen,    "mTradeOlifeInf", "trade",        fmp)
map_meqTechRetiredNewCap <- function(scen, fmp) .lifespan_retire_one(scen, "meqTechRetiredNewCap", fmp)
map_mvTechRetiredStock   <- function(scen, fmp) .lifespan_retire_one(scen, "mvTechRetiredStock",   fmp)
map_mvTechRetiredNewCap  <- function(scen, fmp) .lifespan_retire_one(scen, "mvTechRetiredNewCap",  fmp)
map_meqStorageRetiredNewCap <- function(scen, fmp) .lifespan_retire_one(scen, "meqStorageRetiredNewCap", fmp, "storage")
map_mvStorageRetiredStock   <- function(scen, fmp) .lifespan_retire_one(scen, "mvStorageRetiredStock",   fmp, "storage")
map_mvStorageRetiredNewCap  <- function(scen, fmp) .lifespan_retire_one(scen, "mvStorageRetiredNewCap",  fmp, "storage")
map_meqTradeRetiredNewCap   <- function(scen, fmp) .lifespan_retire_one(scen, "meqTradeRetiredNewCap",   fmp, "trade")
map_mvTradeRetiredStock     <- function(scen, fmp) .lifespan_retire_one(scen, "mvTradeRetiredStock",     fmp, "trade")
map_mvTradeRetiredNewCap    <- function(scen, fmp) .lifespan_retire_one(scen, "mvTradeRetiredNewCap",    fmp, "trade")

# Phase-out domain: a process's span years MINUS the earliest one. Phase-out is
# a transition between milestones, so the first year of a span has nothing
# behind it to have aged out. `eqXPhaseOut` reads `vXCap` at the previous
# milestone, which does not exist there either.
.lifespan_phaseout_one <- function(scen, name, fmp, cls) {
  f <- .lifespan_family_def[[cls]]
  if (is.null(f$phaseout) || is.na(f$phaseout) || !identical(name, f$phaseout))
    return(scen)
  clsm <- .lifespan_cls_map(scen)
  win <- get_process_years(scen) |>
    dplyr::as_tibble() |>
    dplyr::left_join(clsm, by = "process") |>
    dplyr::filter(.data$class == cls) |>
    dplyr::select(-"class")
  if (!NROW(win)) return(scen)
  win <- win |>
    dplyr::group_by(dplyr::across(dplyr::any_of(c("process", "region")))) |>
    dplyr::filter(.data$year > min(.data$year)) |>
    dplyr::ungroup()
  .set_lifespan_map(scen, name, win, f$key, fmp)
}

map_mvTechPhaseOut    <- function(scen, fmp) .lifespan_phaseout_one(scen, "mvTechPhaseOut",    fmp, "technology")
map_mvStoragePhaseOut <- function(scen, fmp) .lifespan_phaseout_one(scen, "mvStoragePhaseOut", fmp, "storage")
map_mvTradePhaseOut   <- function(scen, fmp) .lifespan_phaseout_one(scen, "mvTradePhaseOut",   fmp, "trade")

# -- registry for the lifespan family -------------------------------------- #
.lifespan_builders <- list(
  mTechNew         = map_mTechNew,
  mTechSpan        = map_mTechSpan,
  mTechEac         = map_mTechEac,
  mTechOlifeInf    = map_mTechOlifeInf,
  mStorageNew      = map_mStorageNew,
  mStorageSpan     = map_mStorageSpan,
  mStorageEac      = map_mStorageEac,
  mStorageOlifeInf = map_mStorageOlifeInf,
  mTradeNew        = map_mTradeNew,
  mTradeSpan       = map_mTradeSpan,
  mTradeOlifeInf   = map_mTradeOlifeInf,
  meqTechRetiredNewCap = map_meqTechRetiredNewCap,
  mvTechRetiredStock   = map_mvTechRetiredStock,
  mvTechRetiredNewCap  = map_mvTechRetiredNewCap,
  meqStorageRetiredNewCap = map_meqStorageRetiredNewCap,
  mvStorageRetiredStock   = map_mvStorageRetiredStock,
  mvStorageRetiredNewCap  = map_mvStorageRetiredNewCap,
  meqTradeRetiredNewCap   = map_meqTradeRetiredNewCap,
  mvTradeRetiredStock     = map_mvTradeRetiredStock,
  mvTradeRetiredNewCap    = map_mvTradeRetiredNewCap,
  mvTechPhaseOut          = map_mvTechPhaseOut,
  mvStoragePhaseOut       = map_mvStoragePhaseOut,
  mvTradePhaseOut         = map_mvTradePhaseOut
)
