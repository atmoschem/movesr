.movesr_meteorology_from_scenario <- function(scenario) {
  hours <- scenario$hour_id
  temp <- scenario$temperature_c
  rh <- scenario$rel_humidity
  if (is.null(hours)) hours <- 1L
  if (is.null(temp)) temp <- 20
  if (is.null(rh)) rh <- 50
  n <- max(length(hours), length(temp), length(rh))
  met <- data.table::data.table(hourID = as.integer(rep_len(hours, n)),
    temperature = rep_len(temp, n) * 9 / 5 + 32,
    relHumidity = rep_len(rh, n))
  if (anyDuplicated(met$hourID)) {
    stop("hour_id values must be unique; pair one temperature/humidity per hour")
  }
  met
}

#' Run a movesr scenario
#'
#' @description High-level entry point. Converts a \code{\link{movesr_scenario}}
#' into MOVES inputs and calls the validated engines:
#' \code{\link{movesr_running_rates}} for process 1 and
#' \code{\link{movesr_start_rates}} for process 2.
#' @param database Path to the Parquet catalog or a `movesr_database` object.
#' @param scenario A `movesr_scenario`. `hour_id`, `temperature_c` and
#' `rel_humidity` are recycled to a common length and paired (one weather row
#' per hour). Other fields may be vectors and are used as filters.
#' @param county_id County for fuel supply, fuel usage, I/M and pressure.
#' @param month_id Month 1--12.
#' @param day_id Day type: 5 weekday, 2 weekend (start exhaust only).
#' @param duckdb Optional path to the DuckDB CLI for this call.
#' @param ... Passed to the engines (e.g. `fuel_supply`,
#' `barometric_pressure`, `im_coverage`, `keep_regclass`).
#' @return A list with `rates` (data.table) and `scenario`. `rates` has one
#' table per process, combined with `fill = TRUE`.
#' @export
movesr_run <- function(database, scenario = movesr_scenario(),
                       county_id = 26161, month_id = 1, day_id = 5L,
                       duckdb = NULL, ...) {
  if (!is.null(duckdb)) {
    old <- getOption("movesr.duckdb")
    options(movesr.duckdb = duckdb)
    on.exit(options(movesr.duckdb = old), add = TRUE)
  }
  if (!inherits(scenario, "movesr_scenario")) {
    stop("scenario must be created by movesr_scenario()")
  }
  if (is.character(database)) database <- movesr_database(database)
  if (length(scenario$year) != 1L) stop("year must be one calendar year")
  if (any(!scenario$process_id %in% c(1, 2))) {
    stop("movesr_run supports process_id 1 (running) and 2 (start); ",
      "evaporative processes are not ported yet")
  }
  met <- .movesr_meteorology_from_scenario(scenario)
  out <- list()
  if (1 %in% scenario$process_id) {
    road <- scenario$road_type_id
    road <- road[!is.na(road)]
    if (!length(road)) road <- 2:5
    r <- movesr_running_rates(database, year = scenario$year,
      county_id = county_id, month_id = month_id,
      meteorology = met,
      source_type_id = scenario$source_type_id,
      fuel_type_id = scenario$fuel_type_id,
      pollutant_id = scenario$pollutant_id,
      road_type_id = road, ...)
    if (!is.null(scenario$speed_bin_id)) {
      r <- r[avgSpeedBinID %in% scenario$speed_bin_id]
    }
    out$running <- r
  }
  if (2 %in% scenario$process_id) {
    out$start <- movesr_start_rates(database, year = scenario$year,
      county_id = county_id, month_id = month_id,
      day_id = day_id, meteorology = met,
      source_type_id = scenario$source_type_id,
      fuel_type_id = scenario$fuel_type_id,
      pollutant_id = scenario$pollutant_id, ...)
  }
  for (n in names(out)) {
    out[[n]] <- out[[n]][modelYearID %in% scenario$model_year]
  }
  rates <- data.table::rbindlist(out, fill = TRUE)
  list(rates = rates, running = out$running, start = out$start,
    scenario = scenario)
}
