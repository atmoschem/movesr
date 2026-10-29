#' Define a MOVES-compatible factor scenario
#'
#' @description Creates a scenario definition for \code{\link{movesr_generate}}.
#' Each argument may be a vector; the result describes one or more requested
#' factor combinations. Multiple values are not silently averaged.
#' @param year Calendar year of the fleet/activity.
#' @param source_type_id MOVES source type ID(s), e.g. 21.
#' @param fuel_type_id MOVES fuel type ID(s), e.g. 1.
#' @param model_year Vehicle model year(s).
#' @param pollutant_id MOVES pollutant ID(s).
#' @param process_id MOVES process ID(s): 1 running, 2 start, 11 permeation,
#' 12 vapor venting, 13 fuel leaks.
#' @param road_type_id MOVES road type ID(s); use `NA` or `NULL` for event
#' processes that have no road type.
#' @param speed_bin_id Optional MOVES average-speed bin(s).
#' @param temperature_c Optional temperature(s) in degrees Celsius.
#' @param rel_humidity Optional relative humidity value(s) in percent.
#' @param hour_id Optional local hour-of-day value(s), 1--24.
#' @return A named list with class \code{movesr_scenario}.
#' @export
movesr_scenario <- function(year = 2025, source_type_id = 21,
                            fuel_type_id = 1, model_year = 2020,
                            pollutant_id = 3, process_id = 1,
                            road_type_id = 5, speed_bin_id = NULL,
                            temperature_c = 20, rel_humidity = 50,
                            hour_id = 1) {
  required <- list(year = year, source_type_id = source_type_id,
                   fuel_type_id = fuel_type_id, model_year = model_year,
                   pollutant_id = pollutant_id, process_id = process_id)
  for (name in names(required)) {
    value <- required[[name]]
    if (!is.numeric(value) || !length(value) || anyNA(value) ||
        any(!is.finite(value))) {
      stop(name, " must be a non-empty finite numeric vector")
    }
  }
  optional <- list(road_type_id = road_type_id,
                   speed_bin_id = speed_bin_id,
                   temperature_c = temperature_c,
                   rel_humidity = rel_humidity, hour_id = hour_id)
  for (name in names(optional)) {
    value <- optional[[name]]
    if (is.null(value)) next
    if (!is.numeric(value) || any(!is.finite(value[!is.na(value)]))) {
      stop(name, " must be numeric when supplied")
    }
  }
  structure(c(required, optional), class = "movesr_scenario")
}

#' @export
print.movesr_scenario <- function(x, ...) {
  cat("movesr scenario\n")
  for (name in names(x)) {
    cat("  ", name, ": ", paste(x[[name]], collapse = ", "), "\n", sep = "")
  }
  invisible(x)
}
