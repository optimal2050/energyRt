# =========================================================================== #
# mapping_registry.R
#
# Registry of per-mapping builder functions. The refactor replaces the generic
# spec-driven recipe engine (`recipe_*` in mapping_engine.R) with one named
# function per mapping, `map_<Name>(scen, fmp) -> scen`, calling the shared
# helpers in `mapping_helpers.R`.
#
# `.mapping_builders` maps a mapping NAME to its builder function. `build_mappings()`
# consults this registry first; any mapping NOT registered falls back to the legacy
# `recipe_*` for its family. Families migrate one at a time: while the registry is
# empty every mapping falls back, so behaviour is identical to before.
#
# To add a mapping (user-extensibility goal): declare it in `modInp.yml`
# (type: map, dimSets), declare its variable/equation linkage in `maps.R`, write a
# `map_<Name>(scen, fmp)` function, and register it here.
# =========================================================================== #

# Per-family builder lists, each a named list (mapping name -> function(scen, fmp)),
# defined in the R/map_<family>.R files. Listed here by NAME and assembled at
# call-time (not source-time) so package source/collate order is irrelevant.
.mapping_builder_lists <- c(
  ".membership_builders", ".closure_builders", ".calendar_builders",
  ".lifespan_builders", ".value_builders", ".filter_builders",
  ".constraint_builders", ".cost_agg_builders"
)

# Assemble the name -> builder registry from whichever family lists currently exist.
.get_mapping_builders <- function() {
  reg <- list()
  for (v in .mapping_builder_lists) {
    b <- get0(v, envir = asNamespace("energyRt"), ifnotfound = NULL)
    if (!is.null(b)) reg <- utils::modifyList(reg, b)
  }
  reg
}
