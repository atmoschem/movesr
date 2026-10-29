.sourcebin_id <- function(fuel, eng_tech, reg_class, short_group) {
  pad <- function(x) sprintf("%02d", as.integer(x))
  paste0("1", pad(fuel), pad(eng_tech), pad(reg_class), pad(short_group),
    "0000000000")
}

#' Generate MOVES source bins and source-bin distributions
#'
#' @description Builds the `SourceBin` and `SourceBinDistribution` layers from
#' MOVES default-database tables. Source-bin IDs follow the MOVES formula:
#' 1e18 + fuelTypeID*1e16 + engTechID*1e14 + regClassID*1e12 +
#' shortModYrGroupID*1e10. IDs are returned as character strings because they
#' exceed the exact integer range of R doubles.
#' @param sample_vehicle `sampleVehiclePopulation` table.
#' @param pollutant_process_model_year `pollutantProcessModelYear` table.
#' @param model_year_group `modelYearGroup` table.
#' @param year Calendar year of the scenario. Model years older than 40 years
#' and newer than the calendar year are excluded, as in MOVES.
#' @param source_type_id Optional source type filter.
#' @param process_id Optional process filter; matches the last two digits of
#' `polProcessID`.
#' @param source_type_pol_process Optional `SourceTypePolProcess`; when given,
#' regulatory class and model-year group are dropped from the bin where MOVES
#' does not require them (e.g. evaporative permeation, tirewear).
#' @return A list with `source_bin` and `distribution` data.tables.
#' @details Source-bin distributions are calculated as the sum of
#' `stmyFraction` for each source type/model year and source bin. This is the
#' first generated table needed by the running-exhaust calculator; operating
#' modes are a separate layer.
#' @export
movesr_sourcebins <- function(sample_vehicle, pollutant_process_model_year,
                              model_year_group, year,
                              source_type_id = NULL, process_id = NULL,
                              source_type_pol_process = NULL) {
  sample <- data.table::as.data.table(data.table::copy(sample_vehicle))
  ppmy <- data.table::as.data.table(data.table::copy(pollutant_process_model_year))
  groups <- data.table::as.data.table(data.table::copy(model_year_group))

  need_sample <- c("sourceTypeModelYearID", "sourceTypeID", "modelYearID",
    "fuelTypeID", "engTechID", "regClassID", "stmyFraction")
  need_ppmy <- c("polProcessID", "modelYearID", "modelYearGroupID")
  need_groups <- c("modelYearGroupID", "shortModYrGroupID")
  for (pair in list(list(sample, need_sample, "sample_vehicle"),
    list(ppmy, need_ppmy, "pollutant_process_model_year"),
    list(groups, need_groups, "model_year_group"))) {
    missing <- setdiff(pair[[2]], names(pair[[1]]))
    if (length(missing)) {
      stop(pair[[3]], " is missing: ", paste(missing, collapse = ", "))
    }
  }
  if (missing(year) || !is.numeric(year) || length(year) != 1L ||
    !is.finite(year)) stop("year must be one calendar year")
  sample <- sample[modelYearID <= year & modelYearID >= year - 40]
  ppmy <- ppmy[modelYearID <= year & modelYearID >= year - 40]
  if (!is.null(source_type_id)) {
    sample <- sample[sourceTypeID %in% source_type_id]
  }
  if (!is.null(process_id)) {
    ppmy <- ppmy[polProcessID %% 100L %in% process_id]
  }
  if (!nrow(sample) || !nrow(ppmy)) stop("No source-bin inputs match the filter")

  sample <- sample[stmyFraction > 0]
  x <- merge(sample, ppmy, by = "modelYearID", allow.cartesian = TRUE)
  x <- merge(x, groups, by = "modelYearGroupID", all.x = TRUE)
  if (anyNA(x$shortModYrGroupID)) stop("Missing shortModYrGroupID")
  if (!is.null(source_type_pol_process)) {
    # SourceBinDistributionGenerator: regulatory class and model-year group are
    # part of the bin only when SourceTypePolProcess requires them.
    stpp <- data.table::as.data.table(source_type_pol_process)[
      , .(sourceTypeID, polProcessID, isRegClassReqd, isMYGroupReqd)]
    x <- merge(x, stpp, by = c("sourceTypeID", "polProcessID"), all.x = TRUE)
    x[isRegClassReqd %in% "N", regClassID := 0L]
    x[isMYGroupReqd %in% "N", `:=`(modelYearGroupID = 0L, shortModYrGroupID = 0L)]
    x[, c("isRegClassReqd", "isMYGroupReqd") := NULL]
  }
  x[, sourceBinID := .sourcebin_id(fuelTypeID, engTechID, regClassID,
    shortModYrGroupID)]
  x[, sourceBinActivityFraction :=
    sum(stmyFraction, na.rm = TRUE), by = .(sourceTypeModelYearID,
    polProcessID, sourceBinID)]
  distribution <- unique(x[, .(sourceTypeModelYearID, polProcessID,
    sourceBinID, sourceBinActivityFraction)])
  source_bin <- unique(x[, .(sourceBinID, fuelTypeID, engTechID, regClassID,
    modelYearGroupID)])
  source_bin[, `:=`(engSizeID = 0L, weightClassID = 0L)]
  data.table::setcolorder(source_bin, c("sourceBinID", "engSizeID", "fuelTypeID",
    "engTechID", "regClassID",
    "modelYearGroupID", "weightClassID"))
  data.table::setkey(source_bin, sourceBinID)
  data.table::setkey(distribution, sourceTypeModelYearID, polProcessID, sourceBinID)
  list(source_bin = source_bin, distribution = distribution)
}
