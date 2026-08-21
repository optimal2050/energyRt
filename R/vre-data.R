#' Hourly wind and solar capacity factors, one site, one year
#'
#' A full year of hourly capacity factors for one solar and one wind resource
#' at a single site, for examples that need a realistic sub-annual profile
#' rather than a synthetic one -- storage sizing, curtailment and
#' [storage_duration()] in particular.
#'
#' The site is deliberately unlabelled: the data carries no coordinates, region
#' code or name. It is one plausible pair of contrasting profiles, not a claim
#' about a particular place. Solar is a fixed-tilt profile, wind a 100 m hub
#' height; each comes from a single resource cluster.
#'
#' `slice` is already in the `d365_h24` vocabulary, so the table joins directly
#' onto `calendars$d365_h24` with no conversion.
#'
#' @format A data.frame of 8760 rows and three columns:
#' \describe{
#'   \item{slice}{timeslice, `"d001_h00"` to `"d365_h23"`.}
#'   \item{solar}{solar capacity factor, 0-1.}
#'   \item{wind}{wind capacity factor, 0-1.}
#' }
#' @source Derived from the NASA MERRA-2 reanalysis with the `merra2ools`
#'   package. See `data-raw/vre_cf.R` for the generating script.
#' @seealso [calendars], [storage_duration()]
#' @examples
#' str(vre_cf)
#' # annual mean capacity factor of each resource
#' colMeans(vre_cf[, c("solar", "wind")])
"vre_cf"

#' Duration bands of a full-year battery, precomputed
#'
#' The [storage_duration()] decomposition of a battery in an hourly, full-year
#' wind-solar model built on [vre_cf] and `calendars$d365_h24`. Duration bands
#' only say something when the model can hold energy for weeks, which needs all
#' 8760 timeslices -- a solve of minutes, so it is done once and the result
#' ships. `data-raw/vre_storage_duration.R` is the model, and the Storage
#' article shows it as a non-evaluated chunk beside this table.
#'
#' @format A data.frame with one row per timeslice and band:
#' \describe{
#'   \item{stg}{storage name.}
#'   \item{timeslice}{timeslice, `"d001_h00"` to `"d365_h23"`.}
#'   \item{datetime}{the timeslice as a `POSIXct`, for plotting.}
#'   \item{duration}{ordered factor of duration bands, shortest first.}
#'   \item{value}{energy in that band; the bands sum to `vStorageLevel`.}
#' }
#' @source Solved with energyRt on julia/HiGHS. See
#'   `data-raw/vre_storage_duration.R`.
#' @seealso [storage_duration()], [vre_cf]
#' @examples
#' # the bands partition the level, so they sum back to it
#' tapply(vre_storage_duration$value, vre_storage_duration$duration, sum)
"vre_storage_duration"
