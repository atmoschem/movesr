#' Build a running-exhaust base-rate table
#'
#' @description Combines MOVES source-bin distributions, operating-mode
#' fractions and age-based emission rates into distance-based running-exhaust
#' rates. The result is a base-rate calculation; fuel, meteorology and I/M
#' adjustments are separate layers and are not applied here.
#' @param emission_rate_by_age `EmissionRateByAge` table.
#' @param source_bin `SourceBin` table, normally from
#' \code{\link{movesr_sourcebins}}.
#' @param source_bin_distribution `SourceBinDistribution` table, normally from
#' \code{\link{movesr_sourcebins}}.
#' @param source_type_model_year `SourceTypeModelYear` table.
#' @param operating_mode_fraction Output of \code{\link{movesr_opmodes}}.
#' @param average_speed_bin `AvgSpeedBin` table.
#' @param source_type_id Optional source type filter.
#' @param process_id Process filter, default 1 (running exhaust).
#' @param road_type_id Optional road type filter.
#' @return A data.table with rates in g/mile by source type, fuel, model year,
#' age group, road type, speed bin, pollutant and process.
#' @details The calculation is:
#' \preformatted{
#' source-bin weighted mean rate =
#'   sum(sourceBinActivityFraction * meanBaseRate) /
#'   sum(sourceBinActivityFraction)
#' rate = sum(weighted mean rate * opModeFraction) / avgBinSpeed
#' }
#' Units are g/mile when the base rates are grams per source-hour and the
#' average speed is miles per hour. Convert to g/km by dividing by 1.609344.
#' This function is not yet parity-validated against MOVES.
#' @export
movesr_running <- function(emission_rate_by_age, source_bin,
                           source_bin_distribution, source_type_model_year,
                           operating_mode_fraction, average_speed_bin,
                           source_type_id = NULL, process_id = 1,
                           road_type_id = NULL) {
  er <- data.table::as.data.table(data.table::copy(emission_rate_by_age))
  sb <- data.table::as.data.table(data.table::copy(source_bin))
  sbd <- data.table::as.data.table(data.table::copy(source_bin_distribution))
  stmy <- data.table::as.data.table(data.table::copy(source_type_model_year))
  om <- data.table::as.data.table(data.table::copy(operating_mode_fraction))
  bins <- data.table::as.data.table(data.table::copy(average_speed_bin))

  for (pair in list(
      list(er, c("sourceBinID", "polProcessID", "opModeID", "ageGroupID",
                 "meanBaseRate"), "emission_rate_by_age"),
      list(sb, c("sourceBinID", "fuelTypeID", "regClassID"), "source_bin"),
      list(sbd, c("sourceTypeModelYearID", "polProcessID", "sourceBinID",
                  "sourceBinActivityFraction"), "source_bin_distribution"),
      list(stmy, c("sourceTypeModelYearID", "sourceTypeID", "modelYearID"),
           "source_type_model_year"),
      list(om, c("sourceTypeID", "roadTypeID", "avgSpeedBinID",
                 "polProcessID", "opModeID", "opModeFraction"),
           "operating_mode_fraction"),
      list(bins, c("avgSpeedBinID", "avgBinSpeed"), "average_speed_bin"))) {
    missing <- setdiff(pair[[2]], names(pair[[1]]))
    if (length(missing)) {
      stop(pair[[3]], " is missing: ", paste(missing, collapse = ", "))
    }
  }

  if (!is.null(source_type_id)) stmy <- stmy[sourceTypeID %in% source_type_id]
  if (!is.null(road_type_id)) om <- om[roadTypeID %in% road_type_id]
  er[, sourceBinID := as.character(sourceBinID)]
  sb[, sourceBinID := as.character(sourceBinID)]
  sbd[, sourceBinID := as.character(sourceBinID)]
  er <- er[polProcessID %% 100L == process_id]

  x <- merge(er, sb, by = "sourceBinID")
  x <- merge(x, sbd, by = c("sourceBinID", "polProcessID"))
  x <- merge(x, stmy, by = "sourceTypeModelYearID")
  x[, weighted := sourceBinActivityFraction * meanBaseRate]
  x <- x[, .(weighted = sum(weighted, na.rm = TRUE),
             fraction = sum(sourceBinActivityFraction, na.rm = TRUE)),
         by = .(sourceTypeID, fuelTypeID, regClassID, modelYearID,
                polProcessID, opModeID, ageGroupID)]
  x <- x[fraction > 0]
  x[, mean_base_rate := weighted / fraction]

  y <- merge(x, om, by = c("sourceTypeID", "polProcessID", "opModeID"),
             allow.cartesian = TRUE)
  y <- merge(y, bins, by = "avgSpeedBinID")
  y[, adjusted := mean_base_rate * opModeFraction]
  out <- y[, .(emission_rate = sum(adjusted, na.rm = TRUE) / avgBinSpeed[1]),
           by = .(sourceTypeID, fuelTypeID, regClassID, modelYearID,
                  ageGroupID, roadTypeID, avgSpeedBinID, polProcessID,
                  pollutantID = polProcessID %/% 100L)]
  out[, processID := process_id]
  out[, units := "g/mile"]
  data.table::setkey(out, sourceTypeID, fuelTypeID, modelYearID, ageGroupID,
                     roadTypeID, avgSpeedBinID, polProcessID)
  out
}
