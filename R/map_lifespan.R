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
                    eac = "mTechEac",    inf  = "mTechOlifeInf"),
  storage    = list(key = "stg",   region = TRUE,
                    new = "mStorageNew", span = "mStorageSpan",
                    eac = "mStorageEac", inf  = "mStorageOlifeInf"),
  trade      = list(key = "trade", region = FALSE,
                    new = "mTradeNew",   span = "mTradeSpan",
                    eac = NA_character_, inf  = "mTradeOlifeInf")
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

# One technology capacity-retirement map (delegates to the shared builder, which
# self-gates on settings@optimizeRetirement and builds only the requested name).
.lifespan_retire_one <- function(scen, name, fmp) {
  clsm <- .lifespan_cls_map(scen)
  tech_win <- function(accessor) {
    accessor(scen) |>
      dplyr::as_tibble() |>
      dplyr::left_join(clsm, by = "process") |>
      dplyr::filter(.data$class == "technology") |>
      dplyr::select(-"class")
  }
  .lifespan_retirement_tech(scen, name, fmp,
                            tech_win(get_process_invest_years),
                            tech_win(get_process_years),
                            as.character(scen@settings@region))
}

# -- per-mapping entry points ---------------------------------------------- #
map_mTechNew         <- function(scen, fmp) .lifespan_window_map(scen, "mTechNew",      "technology", "new",  fmp)
map_mTechSpan        <- function(scen, fmp) .lifespan_window_map(scen, "mTechSpan",     "technology", "span", fmp)
map_mTechEac         <- function(scen, fmp) .lifespan_window_map(scen, "mTechEac",      "technology", "span", fmp)
map_mTechOlifeInf    <- function(scen, fmp) .lifespan_inf_map(scen,    "mTechOlifeInf", "technology",         fmp)
map_mStorageNew      <- function(scen, fmp) .lifespan_window_map(scen, "mStorageNew",      "storage", "new",  fmp)
map_mStorageSpan     <- function(scen, fmp) .lifespan_window_map(scen, "mStorageSpan",     "storage", "span", fmp)
map_mStorageEac      <- function(scen, fmp) .lifespan_window_map(scen, "mStorageEac",      "storage", "span", fmp)
map_mStorageOlifeInf <- function(scen, fmp) .lifespan_inf_map(scen,    "mStorageOlifeInf", "storage",        fmp)
map_mTradeNew        <- function(scen, fmp) .lifespan_window_map(scen, "mTradeNew",      "trade", "new",  fmp)
map_mTradeSpan       <- function(scen, fmp) .lifespan_window_map(scen, "mTradeSpan",     "trade", "span", fmp)
map_mTradeOlifeInf   <- function(scen, fmp) .lifespan_inf_map(scen,    "mTradeOlifeInf", "trade",        fmp)
map_meqTechRetiredNewCap <- function(scen, fmp) .lifespan_retire_one(scen, "meqTechRetiredNewCap", fmp)
map_mvTechRetiredStock   <- function(scen, fmp) .lifespan_retire_one(scen, "mvTechRetiredStock",   fmp)
map_mvTechRetiredNewCap  <- function(scen, fmp) .lifespan_retire_one(scen, "mvTechRetiredNewCap",  fmp)

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
  mvTechRetiredNewCap  = map_mvTechRetiredNewCap
)
