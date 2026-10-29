#' MOVES meteorology: heat index and humidity
#'
#' @description Port of the MOVES `MeteorologyGenerator`. Computes heat index,
#' specific humidity (g H2O / kg dry air) and mole water fraction.
#' @param temperature Temperature in degrees Fahrenheit.
#' @param rel_humidity Relative humidity in percent.
#' @param barometric_pressure Barometric pressure in inHg. MOVES defaults are
#' 28.94 (low altitude) and 24.59 (high altitude). Quito is about 21.4.
#' @return A data.table with temperature, relHumidity, heatIndex,
#' specificHumidity and molWaterFraction.
#' @details
#' \preformatted{
#' heatIndex = temperature                      if temperature < 78 F
#'           = min(NWS regression, 120)         otherwise
#' TK   = (5/9)(T - 32) + 273.15
#' PH2O = 10^(10.79574(1 - 273.15/TK) - 5.028 log10(TK/273.15)
#'        + 1.50475e-4 (1 - 10^(-8.2969(TK/273.15 - 1)))
#'        + 0.42873e-3 (10^(4.76955(1 - 273.15/TK)) - 1) - 0.2138602)
#' PV   = RH/100 * PH2O
#' XH2O = PV / (PB * 3.38639)
#' specificHumidity = 621.1 PV / (PB * 3.38639 - PV)
#' }
#' @export
movesr_meteorology <- function(temperature, rel_humidity,
                               barometric_pressure = 28.94) {
  if (!is.numeric(temperature) || !is.numeric(rel_humidity)) {
    stop("temperature and rel_humidity must be numeric")
  }
  n <- max(length(temperature), length(rel_humidity))
  t <- rep_len(temperature, n)
  rh <- rep_len(rel_humidity, n)
  pb <- rep_len(barometric_pressure, n)
  pb[is.na(pb) | pb <= 0] <- 28.94
  hi <- ifelse(
    t < 78, t,
    pmin(-42.379 + 2.04901523 * t + 10.14333127 * rh -
      0.22475541 * t * rh - 0.00683783 * t * t -
      0.05481717 * rh * rh + 0.00122874 * t * t * rh +
      0.00085282 * t * rh * rh - 0.00000199 * t * t * rh * rh, 120))
  tk <- (5 / 9) * (t - 32) + 273.15
  ph2o <- 10^(10.79574 * (1 - 273.15 / tk) - 5.028 * log10(tk / 273.15) +
    1.50475e-4 * (1 - 10^(-8.2969 * (tk / 273.15 - 1))) +
    0.42873e-3 * (10^(4.76955 * (1 - 273.15 / tk)) - 1) -
    0.2138602)
  pv <- rh / 100 * ph2o
  data.table::data.table(
    temperature = t, relHumidity = rh, heatIndex = hi,
    specificHumidity = (621.1 * pv) / (pb * 3.38639 - pv),
    molWaterFraction = pv / (pb * 3.38639))
}

#' MOVES NOx humidity correction factor K
#'
#' @param fuel_type_id Fuel type.
#' @param specific_humidity g H2O / kg dry air.
#' @param mol_water_fraction mole H2O / mole air.
#' @param nox_humidity `NOxHumidityAdjust` table.
#' @return Numeric K (1 where the fuel has no equation).
#' @export
movesr_nox_humidity <- function(fuel_type_id, specific_humidity,
                                mol_water_fraction, nox_humidity) {
  h <- data.table::as.data.table(nox_humidity)
  n <- max(length(fuel_type_id), length(specific_humidity))
  f <- rep_len(fuel_type_id, n)
  sh <- rep_len(specific_humidity, n)
  mw <- rep_len(mol_water_fraction, n)
  i <- match(f, h$fuelTypeID)
  k <- rep(1, n)
  eq <- h$humidityNOxEq[i]
  a <- h$humidityTermA[i]
  b <- h$humidityTermB[i]
  lo <- h$humidityLowBound[i]
  up <- h$humidityUpBound[i]
  cfr86 <- !is.na(eq) & eq == "CFR 86"
  cfr1065 <- !is.na(eq) & eq == "CFR 1065"
  k[cfr86] <- 1 - a[cfr86] * (pmin(pmax(sh[cfr86], lo[cfr86]), up[cfr86]) - 10.71)
  k[cfr1065] <- 1 / (a[cfr1065] *
    pmin(pmax(mw[cfr1065], lo[cfr1065]), up[cfr1065]) +
    b[cfr1065])
  k
}

#' MOVES air-conditioning activity factor
#'
#' @description
#' \preformatted{
#' ACFactor = clamp(A + HI (B + C HI), 0, 1) *
#'            ACPenetrationFraction * functioningACFraction
#' }
#' with A, B, C from `MonthGroupHour`, penetration from `SourceTypeModelYear`
#' and functioning fraction from `SourceTypeAge` at age = year - model year.
#' @param heat_index Heat index (F) by hour: data.table with hourID, heatIndex.
#' @param month_group_hour `MonthGroupHour`.
#' @param source_type_model_year `SourceTypeModelYear`.
#' @param source_type_age `SourceTypeAge`.
#' @param year Calendar year.
#' @param month_group_id Month group.
#' @return data.table hourID, sourceTypeID, modelYearID, ACFactor.
#' @export
movesr_ac_factor <- function(heat_index, month_group_hour,
                             source_type_model_year, source_type_age,
                             year, month_group_id) {
  hi <- data.table::as.data.table(heat_index)
  mgh <- data.table::as.data.table(month_group_hour)[monthGroupID == month_group_id]
  on <- merge(hi[, .(hourID, heatIndex)],
    mgh[, .(hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC)],
    by = "hourID")
  on[, ACOnFraction := pmin(pmax(ACActivityTermA + heatIndex *
    (ACActivityTermB + ACActivityTermC * heatIndex),
  0), 1)]
  stmy <- data.table::as.data.table(source_type_model_year)
  sta <- data.table::as.data.table(source_type_age)
  stmy <- stmy[modelYearID <= year & modelYearID >= year - 40]
  stmy[, ageID := year - modelYearID]
  pen <- merge(stmy[, .(sourceTypeID, modelYearID, ageID, ACPenetrationFraction)],
    sta[, .(sourceTypeID, ageID, functioningACFraction)],
    by = c("sourceTypeID", "ageID"))
  pen[, k := 1L]
  on[, k := 1L]
  out <- merge(on[, .(k, hourID, ACOnFraction)], pen, by = "k",
    allow.cartesian = TRUE)
  out[, ACFactor := ACOnFraction * ACPenetrationFraction * functioningACFraction]
  out[, .(hourID, sourceTypeID, modelYearID, ACFactor)]
}
