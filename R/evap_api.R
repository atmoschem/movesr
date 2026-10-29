#' Evaporative rates for a county (MOVES processes 11, 12, 13)
#'
#' @description One-call evaporative rates per vehicle (MOVES `RatePerVehicle`
#' / `RatePerProfile`, off-network):
#' \enumerate{
#' \item 24-hour weather (default MOVES `ZoneMonthHour` of the county);
#' \item tank temperatures, soak fractions and cold-soak initial-hour
#' fractions: `method = "original"` runs the exact MOVES sample-trip
#' simulation (\code{\link{movesr_tank_temperature}}, about 13 s);
#' `method = "simplified"` uses \code{\link{movesr_tank_temperature_simple}}
#' (instant, calibrated surrogate);
#' \item activity (\code{\link{movesr_activity}}): parked hours per vehicle;
#' \item permeation, vapor venting and leak calculators with HC speciation
#' (\code{\link{movesr_evap_rates}}).
#' }
#' @param database Path or `movesr_database`.
#' @param county_id County.
#' @param year Calendar year.
#' @param month_id Month.
#' @param day_id 5 (weekday) or 2 (weekend).
#' @param hour_id Output hours.
#' @param meteorology Optional 24-hour table (hourID, temperature F,
#' relHumidity \%); default MOVES weather of the county.
#' @param method `"original"` (exact MOVES port) or `"simplified"`.
#' @param process_id Any of 11, 12, 13.
#' @param pollutant_id THC (1) and HC species 5, 79, 80, 86, 87.
#' @param tank Optional precomputed tank temperature list (skips step 2).
#' @param ... Passed to \code{\link{movesr_evap_rates}} (source_type_id,
#' fuel_type_id, fuel_supply, im_coverage, tvv_hours, by_reg_class, ...).
#' @return data.table yearID, monthID, dayID, hourID, sourceTypeID, regClassID,
#' fuelTypeID, modelYearID, pollutantID, processID, ratePerVehicle (g per
#' vehicle per hour).
#' @examples
#' \dontrun{
#' ev <- movesr_evap(db, county_id = 11001, month_id = 7, method = "simplified")
#' }
#' @export
movesr_evap <- function(database, county_id, year = 2025, month_id = 7,
                        day_id = 5L, hour_id = 1:24, meteorology = NULL,
                        method = c("original", "simplified"),
                        process_id = c(11L, 12L, 13L), pollutant_id = 1L,
                        tank = NULL, ...) {
  if (is.character(database)) database <- movesr_database(database)
  method <- match.arg(method)
  met <- if (is.null(meteorology)) {
    movesr_default_meteorology(database, county_id, month_id, 1:24)
  } else meteorology
  met <- data.table::as.data.table(met)[order(hourID)]
  if (!identical(as.integer(met$hourID), 1:24)) {
    stop("meteorology must have the 24 hours 1..24 (evaporative tank model)")
  }
  if (is.null(tank)) {
    tank <- if (method == "original") {
      movesr_tank_temperature(database, met$temperature, day_id = day_id)
    } else {
      movesr_tank_temperature_simple(database, met$temperature, day_id = day_id)
    }
  }
  act <- movesr_activity(database, year, county_id, month_id, day_id)$parked
  movesr_evap_rates(database, tank, act, year = year, county_id = county_id,
                    month_id = month_id, day_id = day_id,
                    meteorology = met[hourID %in% hour_id],
                    process_id = process_id, pollutant_id = pollutant_id, ...)
}

#' Evaporative rates per distance on running roads
#'
#' @description MOVES evaporative emissions while driving (road types 2-5,
#' `RatePerDistance`): only the operating mode (300) applies, at the operating
#' tank temperature (permeation), ambient temperature and tank RVP (vapor
#' venting) or the operating leak rate:
#' \preformatted{ratePerDistance = rate300 / avgBinSpeed}
#' @inheritParams movesr_evap
#' @param tank Output of \code{\link{movesr_tank_temperature}} or
#' \code{\link{movesr_tank_temperature_simple}} (computed when NULL).
#' @param avg_speed_bin Average-speed bins (1-16).
#' @param road_type_id Road types (rates do not depend on the road type).
#' @return data.table like \code{\link{movesr_evap}} plus roadTypeID,
#' avgSpeedBinID and ratePerDistance (g/mile).
#' @export
movesr_evap_running_rates <- function(database, county_id, year = 2025,
                                      month_id = 7, day_id = 5L, hour_id = 1:24,
                                      meteorology = NULL,
                                      method = c("original", "simplified"),
                                      process_id = c(11L, 12L, 13L),
                                      pollutant_id = 1L, tank = NULL,
                                      avg_speed_bin = 1:16, road_type_id = 5L, ...) {
  if (is.character(database)) database <- movesr_database(database)
  method <- match.arg(method)
  met <- if (is.null(meteorology)) {
    movesr_default_meteorology(database, county_id, month_id, 1:24)
  } else meteorology
  met <- data.table::as.data.table(met)[order(hourID)]
  if (is.null(tank)) {
    if (!identical(as.integer(met$hourID), 1:24)) {
      stop("meteorology must have the 24 hours 1..24 (evaporative tank model)")
    }
    tank <- if (method == "original") {
      movesr_tank_temperature(database, met$temperature, day_id = day_id)
    } else {
      movesr_tank_temperature_simple(database, met$temperature, day_id = day_id)
    }
  }
  # Ages without population have no activity and no MOVES output.
  pop <- movesr_activity(database, year, county_id, month_id, day_id)$population[
    population > 0, .(sourceTypeID, ageID)]
  act <- pop[rep(seq_len(.N), each = length(hour_id))]
  act[, `:=`(hourDayID = rep(hour_id * 10L + day_id, times = nrow(pop)),
             shpPerVehicle = 1)]
  x <- movesr_evap_rates(database, tank, act, year = year, county_id = county_id,
                         month_id = month_id, day_id = day_id,
                         meteorology = met[hourID %in% hour_id],
                         process_id = process_id, pollutant_id = pollutant_id,
                         fraction_of_operating = 1, tvv_hours = integer(), ...)
  bins <- movesr_table(database, "avgspeedbin")[avgSpeedBinID %in% avg_speed_bin,
                                                .(avgSpeedBinID, avgBinSpeed)]
  n <- nrow(x)
  x <- x[rep(seq_len(n), each = nrow(bins))]
  b <- bins[rep(seq_len(nrow(bins)), times = n)]
  x[, `:=`(avgSpeedBinID = b$avgSpeedBinID, avgBinSpeed = b$avgBinSpeed)]
  x[, ratePerDistance := ratePerVehicle / avgBinSpeed]
  n <- nrow(x)
  x <- x[rep(seq_len(n), each = length(road_type_id))]
  x[, roadTypeID := rep(as.integer(road_type_id), times = n)]
  x[, c("ratePerVehicle", "avgBinSpeed") := NULL]
  x[]
}
