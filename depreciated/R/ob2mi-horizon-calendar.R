# =============================================================================#
# Archived: unwired ob2mi("horizon") / ob2mi("calendar") methods.
#
# These two `ob2mi` S4 methods were an alternative, data-driven design for
# building the horizon- and calendar-derived modInp parameters. They were never
# dispatched: the live `interp_mod` pipeline only calls `ob2mi` on the model's
# data objects (R/interp.R) and on the `settings` object (via
# `.interp_settings_params` -> `ob2mi(scen, settings, ...)`). The horizon-derived
# numerics (ordYear / cardYear / pPeriodLen / ...) and the calendar maps are
# instead produced by the ported `ob2mi("settings")` method and the `calendar`
# mapping recipe respectively.
#
# `ob2mi("horizon")` was the sole consumer of the `fn` function list (R/fn.R,
# also archived here). Moved out of R/obj2modInp2.R during the legacy retirement.
# Kept for reference in case the `fn`-driven horizon path is revived to replace
# the corresponding lines in `ob2mi("settings")`.
# =============================================================================#

# =============================================================================#
## horizon ####
# =============================================================================#
setMethod(
  "ob2mi",
  signature(scen = "scenario", obj = "horizon", extra_params = "list"),
  function(scen, obj, extra_params = list()) {
    # alternatively use Reduce()
    for (p in scen@modInp@parameters) {
      if (any(p@inClass %in% "horizon")) {
        dat <- fn[[p@name]](obj)
        scen <- update_parameter(scen, p@name, dat)
      }
    }
    return(scen)
  }
)

# =============================================================================#
## calendar ####
# =============================================================================#
setMethod(
  "ob2mi",
  signature(scen = "scenario", obj = "calendar", extra_params = "list"),
  function(scen, obj, extra_params = list()) {
    for (s in slotNames(obj)) {
      message("slot: ", s)
      if (s %in% c("name", "timeframe", "commodity", "region")) {next}
      slot_info <- get_slot_meta(class(obj), s)
      if (is_empty(slot_info)) {next}
      slot_data <- get_lazy_data(obj, s)
      for (p in slot_info) {
        cat(paste0("  param: ", p$name, "\n"))
        dat <- make_data_param(
          scen = scen,
          obj_name = obj@name,
          slot_data = slot_data,
          par_meta = p,
          class_col = "calendar"
        )
        scen <- update_parameter(scen, p$name, dat)
      }
    }
  }
)
