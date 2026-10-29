#' Generate emission factors from portable MOVES data
#'
#' @description Selects factors for a scenario. Distance factors require a MOVES
#' speed bin or a speed in km/h. Event factors (start and evaporative) do not
#' use speed.
#' @param scenario A \code{movesr_scenario} from \code{\link{movesr_scenario}}.
#' @param factors A \code{movesr_factors} object from
#' \code{\link{movesr_factors}}.
#' @param speed_kmh Optional speed in km/h for distance factors. It is mapped
#' to a MOVES average-speed bin using the lower-inclusive bin boundaries.
#' @param method Reserved for future interpolation methods; currently `"bin"`.
#' @return A data.table of matching factors with units in the `units` column.
#' @details The function performs exact categorical selection. It does not
#' average different fuels, vehicles, model years, temperatures or runs.
#' Distance factors are returned in g/km; start factors in g/start; evaporative
#' factors in g/vehicle.
#' @export
movesr_generate <- function(scenario, factors,
                            speed_kmh = NULL, method = "bin") {
  if (!inherits(scenario, "movesr_scenario")) {
    stop("scenario must be created by movesr_scenario()")
  }
  if (!inherits(factors, "movesr_factors")) {
    stop("factors must be created by movesr_factors()")
  }
  if (!identical(method, "bin")) stop("Only method = 'bin' is implemented")
  data <- data.table::copy(factors$data)

  .filter <- function(data, column, values) {
    if (is.null(values) || !length(values) || all(is.na(values))) return(data)
    if (!column %in% names(data)) {
      stop("Factor data has no column ", column)
    }
    data[data[[column]] %in% values]
  }

  data <- .filter(data, "yearID", scenario$year)
  data <- .filter(data, "sourceTypeID", scenario$source_type_id)
  data <- .filter(data, "fuelTypeID", scenario$fuel_type_id)
  data <- .filter(data, "modelYearID", scenario$model_year)
  data <- .filter(data, "pollutantID", scenario$pollutant_id)
  data <- .filter(data, "processID", scenario$process_id)
  data <- .filter(data, "roadTypeID", scenario$road_type_id)
  data <- .filter(data, "hourID", scenario$hour_id)
  data <- .filter(data, "temperature_C", scenario$temperature_c)
  data <- .filter(data, "relHumidity", scenario$rel_humidity)

  distance <- unique(data$basis) == "distance"
  if (length(unique(data$basis)) > 1L) {
    stop("Select one basis: distance, start or vehicle")
  }
  if (length(unique(data$basis)) == 0L) {
    stop("No factors match the scenario")
  }

  if (!is.null(scenario$speed_bin_id)) {
    data <- .filter(data, "avgSpeedBinID", scenario$speed_bin_id)
  }
  if (!is.null(speed_kmh)) {
    if (!distance) stop("speed_kmh is only valid for distance factors")
    if (!is.numeric(speed_kmh) || length(speed_kmh) != 1L ||
        !is.finite(speed_kmh) || speed_kmh <= 0) {
      stop("speed_kmh must be one positive finite value")
    }
    mph <- speed_kmh / 1.609344
    if (mph > 75) stop("speed_kmh exceeds the MOVES range (75 mph)")
    bin <- findInterval(mph, seq(2.5, 72.5, 5)) + 1L
    data <- data[avgSpeedBinID %in% bin]
    data[, requested_speed_kmh := speed_kmh]
  }

  if (distance && is.null(scenario$speed_bin_id) && is.null(speed_kmh)) {
    stop("Distance factors require speed_bin_id or speed_kmh")
  }
  if (!nrow(data)) stop("No factors match the scenario")
  data.table::setattr(data, "scenario", scenario)
  data
}
