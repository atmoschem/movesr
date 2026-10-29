.movesr_regions <- list(
  dmv = data.frame(
    countyID = c(11001L, 24009L, 24017L, 24021L, 24031L, 24033L, 51013L,
                 51059L, 51107L, 51153L, 51510L, 51600L, 51610L, 51683L,
                 51685L),
    name = c("District of Columbia", "Calvert County, MD", "Charles County, MD",
             "Frederick County, MD", "Montgomery County, MD",
             "Prince George's County, MD", "Arlington County, VA",
             "Fairfax County, VA", "Loudoun County, VA",
             "Prince William County, VA", "Alexandria city, VA",
             "Fairfax city, VA", "Falls Church city, VA", "Manassas city, VA",
             "Manassas Park city, VA"),
    stringsAsFactors = FALSE),
  dmv_core = data.frame(
    countyID = c(11001L, 24031L, 24033L, 51013L, 51059L, 51510L, 51600L,
                 51610L),
    name = c("District of Columbia", "Montgomery County, MD",
             "Prince George's County, MD", "Arlington County, VA",
             "Fairfax County, VA", "Alexandria city, VA", "Fairfax city, VA",
             "Falls Church city, VA"),
    stringsAsFactors = FALSE)
)

#' Counties of a predefined region
#'
#' @description Returns the MOVES county IDs of a named region.
#' \describe{
#' \item{`"dmv"`}{Washington DC-MD-VA ozone nonattainment area (15
#' jurisdictions): DC; Calvert, Charles, Frederick, Montgomery and Prince
#' George's counties, MD; Arlington, Fairfax, Loudoun and Prince William
#' counties and the cities of Alexandria, Fairfax, Falls Church, Manassas and
#' Manassas Park, VA.}
#' \item{`"dmv_core"`}{DC, Montgomery, Prince George's, Arlington, Fairfax,
#' Alexandria, Fairfax city and Falls Church.}
#' }
#' @param region Region name, or a numeric vector of county IDs (returned
#' as is, with names from the database when supplied).
#' @param database Optional `movesr_database` to add county names, state and
#' barometric pressure.
#' @return data.table with countyID and name.
#' @export
movesr_region <- function(region = "dmv", database = NULL) {
  if (is.numeric(region)) {
    out <- data.table::data.table(countyID = as.integer(region),
                                  name = NA_character_)
  } else {
    if (!region %in% names(.movesr_regions)) {
      stop("Unknown region. Available: ", paste(names(.movesr_regions), collapse = ", "))
    }
    out <- data.table::as.data.table(.movesr_regions[[region]])
  }
  if (!is.null(database)) {
    cty <- movesr_table(database, "county")[, .(countyID, countyName, stateID,
                                                barometricPressure, GPAFract)]
    missing <- setdiff(out$countyID, cty$countyID)
    if (length(missing)) stop("Unknown county ID: ", paste(missing, collapse = ", "))
    out <- merge(out, cty, by = "countyID", sort = FALSE)
  }
  out[]
}

#' MOVES default hourly meteorology for a county
#'
#' @description Reads `ZoneMonthHour` (default zone = countyID * 10) for one
#' month.
#' @param database A `movesr_database`.
#' @param county_id County ID.
#' @param month_id Month 1--12.
#' @param hour_id Hours 1--24.
#' @return data.frame with hourID, temperature (F) and relHumidity (%).
#' @export
movesr_default_meteorology <- function(database, county_id, month_id = 7,
                                       hour_id = 1:24) {
  zone <- movesr_table(database, "zone")[countyID == county_id]$zoneID
  if (!length(zone)) stop("No zone for county ", county_id)
  z <- movesr_table(database, "zonemonthhour")[zoneID == zone[1] &
                                                 monthID == month_id &
                                                 hourID %in% hour_id]
  if (!nrow(z)) stop("No default meteorology for county ", county_id)
  as.data.frame(z[order(hourID), .(hourID, temperature, relHumidity)])
}

#' Generate rates for a region
#'
#' @description Runs the movesr engines county by county with each county's
#' own MOVES inputs (fuel supply, fuel usage, I/M program, barometric pressure
#' and, unless supplied, default hourly weather) and stacks the results with a
#' `countyID` column.
#' @param database Path or `movesr_database`.
#' @param region Region name (see \code{\link{movesr_region}}) or county IDs.
#' @param process Any of `"running"`, `"start"`, `"brake"`, `"tire"`,
#' `"evap"`. For running and start, pollutants follow `pollutant_id`; brake
#' and tire use PM2.5 (116/117) and PM10 (106/107); evap (processes 11-13,
#' \code{\link{movesr_evap}}) uses the HC pollutants among `pollutant_id`
#' (THC when none) and always the county's 24-hour default weather unless
#' `meteorology` has 24 hours.
#' @param month_id Month 1--12.
#' @param hour_id Hours used when `meteorology` is `NULL`.
#' @param meteorology Optional weather table used for every county, or a
#' named list of tables keyed by county ID.
#' @param ... Passed to \code{\link{movesr_running_rates}},
#' \code{\link{movesr_start_rates}} and \code{\link{movesr_evap}} (year,
#' source_type_id, pollutant_id, day_id, method, ...).
#' @return A list with one data.table per process.
#' @examples
#' \dontrun{
#' dmv <- movesr_region_rates(db, "dmv", month_id = 7, hour_id = 16:17,
#'                            source_type_id = 21, road_type_id = 5)
#' }
#' @export
movesr_region_rates <- function(database, region = "dmv",
                                process = c("running", "start"),
                                month_id = 7, hour_id = 1:24,
                                meteorology = NULL, ...) {
  if (is.character(database)) database <- movesr_database(database)
  process <- match.arg(process, c("running", "start", "brake", "tire", "evap"),
                       several.ok = TRUE)
  counties <- movesr_region(region, database)$countyID
  dots <- list(...)
  call <- function(fun, args) {
    args <- c(args, dots[names(dots) %in% names(formals(fun))])
    do.call(fun, args)
  }
  out <- list(running = list(), start = list(), brake = list(), tire = list(),
              evap = list())
  for (cty in counties) {
    met <- if (is.null(meteorology)) {
      movesr_default_meteorology(database, cty, month_id, hour_id)
    } else if (is.data.frame(meteorology)) {
      meteorology
    } else {
      meteorology[[as.character(cty)]]
    }
    if (is.null(met)) stop("No meteorology for county ", cty)
    if ("running" %in% process) {
      r <- call(movesr_running_rates, list(database = database, county_id = cty,
                                           month_id = month_id, meteorology = met))
      out$running[[length(out$running) + 1L]] <- r[, countyID := cty]
    }
    for (bt in intersect(process, c("brake", "tire"))) {
      pid <- if (bt == "brake") 9L else 10L
      pol <- if (bt == "brake") c(116L, 106L) else c(117L, 107L)
      args <- list(database = database, county_id = cty, month_id = month_id,
                   meteorology = met, process_id = pid)
      d <- dots[names(dots) %in% names(formals(movesr_running_rates))]
      sel <- intersect(d$pollutant_id, pol)
      d$pollutant_id <- if (length(sel)) sel else pol
      x <- do.call(movesr_running_rates, c(args, d))
      out[[bt]][[length(out[[bt]]) + 1L]] <- x[, countyID := cty]
    }
    if ("evap" %in% process) {
      d <- dots[names(dots) %in% c(names(formals(movesr_evap)),
                                   names(formals(movesr_evap_rates)))]
      sel <- intersect(d$pollutant_id, c(1L, 5L, 79L, 80L, 86L, 87L))
      d$pollutant_id <- if (length(sel)) sel else 1L
      d$database <- NULL; d$county_id <- NULL; d$month_id <- NULL
      evmet <- if (!is.null(met) && nrow(met) == 24L) met else NULL
      e <- do.call(movesr_evap, c(list(database = database, county_id = cty,
                                       month_id = month_id, hour_id = hour_id,
                                       meteorology = evmet), d))
      out$evap[[length(out$evap) + 1L]] <- e[, countyID := cty]
    }
    if ("start" %in% process) {
      s <- call(movesr_start_rates, list(database = database, county_id = cty,
                                         month_id = month_id, meteorology = met))
      out$start[[length(out$start) + 1L]] <- s[, countyID := cty]
    }
  }
  res <- list()
  for (p in process) {
    x <- data.table::rbindlist(out[[p]])
    if (nrow(x)) data.table::setcolorder(x, c("yearID", "countyID"))
    res[[p]] <- x
  }
  res
}
