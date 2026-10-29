#' Apply MOVES temperature and NOx humidity adjustments
#'
#' @description Applies the MOVES temperature-adjustment equations to a rate
#' table. NOx uses the diesel/non-diesel branch and, when humidity tables and
#' humidity inputs are supplied, the MOVES humidity correction.
#' @param rates A data.table with polProcessID, fuelTypeID, regClassID,
#' modelYearID and a rate column.
#' @param temperature_adjustment `TemperatureAdjustment` table.
#' @param temperature Temperature in degrees Fahrenheit.
#' @param fuel_type_id Fuel filter.
#' @param pollutant_id Pollutant filter; NOx is 3.
#' @param process_id Process filter; default 1 (running).
#' @param rate_column Name of the rate column. Default `"emission_rate"`.
#' @param humidity_adjustment Optional `NOxHumidityAdjust` table.
#' @param specific_humidity Optional specific humidity in g H2O/kg dry air.
#' @param mol_water_fraction Optional mole water fraction.
#' @return A copy of `rates` with `temperature_factor` and the adjusted rate
#' in the original rate column. The factor is multiplicative.
#' @details The general equation is:
#' \preformatted{
#' 1 + (T - 75) * (A + (T - 75) * B)
#' }
#' For NOx from diesel, MOVES uses:
#' \preformatted{
#' 1 + (77 - T) * A     # zero above 77 F
#' }
#' For NOx, the humidity factor is applied multiplicatively:
#' \preformatted{
#' CFR 86:   1 - A * (specific_humidity - 10.71)
#' CFR 1065: 1 / (A * mol_water_fraction + B)
#' }
#' with the table bounds enforced.
#' @export
movesr_temperature_adjustment <- function(rates, temperature_adjustment,
                                           temperature, fuel_type_id,
                                           pollutant_id = 3, process_id = 1,
                                           rate_column = "emission_rate",
                                           humidity_adjustment = NULL,
                                           specific_humidity = NULL,
                                           mol_water_fraction = NULL) {
  x <- data.table::as.data.table(data.table::copy(rates))
  ta <- data.table::as.data.table(data.table::copy(temperature_adjustment))
  need <- c("polProcessID", "fuelTypeID", "regClassID", "minModelYearID",
            "maxModelYearID", "tempAdjustTermA", "tempAdjustTermB")
  missing <- setdiff(need, names(ta))
  if (length(missing)) stop("temperature_adjustment is missing: ",
                            paste(missing, collapse = ", "))
  for (name in c("pollutantID", "processID", "fuelTypeID", "regClassID",
                 "modelYearID", "polProcessID")) {
    if (!name %in% names(x)) stop("rates has no ", name)
  }
  if (!rate_column %in% names(x)) stop("rates has no ", rate_column)
  if (!is.numeric(temperature) || length(temperature) != 1L ||
      !is.finite(temperature)) stop("temperature must be one finite value")
  if (!is.numeric(fuel_type_id) || length(fuel_type_id) != 1L) {
    stop("fuel_type_id must be one numeric value")
  }
  if (!is.numeric(pollutant_id) || length(pollutant_id) != 1L ||
      !is.numeric(process_id) || length(process_id) != 1L) {
    stop("pollutant_id and process_id must be scalars")
  }

  x <- x[pollutantID == pollutant_id & processID == process_id]
  x <- x[fuelTypeID == fuel_type_id]
  if (!nrow(x)) stop("No rates match pollutant/process/fuel")
  unchanged <- data.table::copy(x)
  x <- merge(x, ta[, .(polProcessID, fuelTypeID, regClassID, minModelYearID,
                       maxModelYearID, tempAdjustTermA, tempAdjustTermB)],
             by = c("polProcessID", "fuelTypeID", "regClassID"),
             allow.cartesian = TRUE)
  x <- x[modelYearID >= minModelYearID & modelYearID <= maxModelYearID]
  if (!nrow(x)) {
    unchanged[, `:=`(temp_factor = 1, humidity_factor = 1,
                     temperature_factor = 1)]
    return(unchanged)
  }

  if (pollutant_id == 3L) {
    x[, temp_factor := data.table::fifelse(
      fuelTypeID == 2L,
      data.table::fifelse(temperature > 77, 1,
                          1 + (77 - temperature) * tempAdjustTermA),
      1 + (temperature - 75) * (tempAdjustTermA +
                                  (temperature - 75) * tempAdjustTermB))]
  } else {
    x[, temp_factor := 1 + (temperature - 75) *
        (tempAdjustTermA + (temperature - 75) * tempAdjustTermB)]
  }

  x[, humidity_factor := 1]
  if (!is.null(humidity_adjustment) && pollutant_id == 3L) {
    nha <- data.table::as.data.table(data.table::copy(humidity_adjustment))
    nneed <- c("fuelTypeID", "humidityNOxEq", "humidityTermA",
               "humidityTermB", "humidityLowBound", "humidityUpBound")
    nmiss <- setdiff(nneed, names(nha))
    if (length(nmiss)) stop("humidity_adjustment is missing: ",
                            paste(nmiss, collapse = ", "))
    if (is.null(specific_humidity) && is.null(mol_water_fraction)) {
      stop("Supply specific_humidity or mol_water_fraction for NOx humidity")
    }
    nha <- nha[fuelTypeID == fuel_type_id]
    if (nrow(nha)) {
      h <- nha[1]
      if (identical(h$humidityNOxEq, "CFR 86")) {
        if (is.null(specific_humidity)) stop("CFR 86 needs specific_humidity")
        v <- min(max(specific_humidity, h$humidityLowBound), h$humidityUpBound)
        x[, humidity_factor := 1 - h$humidityTermA * (v - 10.71)]
      } else if (identical(h$humidityNOxEq, "CFR 1065")) {
        if (is.null(mol_water_fraction)) stop("CFR 1065 needs mol_water_fraction")
        v <- min(max(mol_water_fraction, h$humidityLowBound), h$humidityUpBound)
        x[, humidity_factor := 1 / (h$humidityTermA * v + h$humidityTermB)]
      }
    }
  }
  x[, temperature_factor := temp_factor * humidity_factor]
  x[, (rate_column) := get(rate_column) * temperature_factor]
  x[, c("minModelYearID", "maxModelYearID") := NULL]
  x
}
