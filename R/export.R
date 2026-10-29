#' Export movesr rates in the format read by vein
#'
#' @description Writes MOVES-style output tables that `vein::moves_factors()`,
#' `vein::moves_start_factors()` and the `ef_moves*()` functions read:
#' `rateperdistance.csv`, `rateperstart.csv`, `ratepervehicle.csv`
#' (permeation, leaks) and `rateperprofile.csv` (vapor venting), a run table and
#' `vehicles.csv`.
#' @param path Output directory (created if needed).
#' @param running Output of \code{\link{movesr_running_rates}} or `NULL`.
#' @param start Output of \code{\link{movesr_start_rates}} or `NULL`.
#' @param evap Output of \code{\link{movesr_evap}} or `NULL` (read by
#' `vein::moves_evap_factors()`; evap on running roads from
#' \code{\link{movesr_evap_running_rates}} belongs in `running`).
#' @param database A `movesr_database` (for vehicle and fuel names).
#' @param scenario_id Text stored in `MOVESScenarioID`.
#' @param run_id Integer `MOVESRunID`.
#' @param description Free text stored in the run table.
#' @return Invisibly, the written file paths.
#' @details Rates are written in grams per mile, grams per start and grams per
#' vehicle per hour (evap), the MOVES defaults, with `massUnits = "g"` and
#' `distanceUnits = "mi"`; vein converts to g/km. The run table records
#' `masterVersion` as the movesr version and the MOVES algorithm it reproduces.
#' @examples
#' \dontrun{
#' db <- movesr_database("movesdb-parquet")
#' r <- movesr_running_rates(db, source_type_id = 21)
#' movesr_export_vein("factors", running = r, database = db)
#' f <- vein::moves_factors("factors/rateperdistance.csv",
#'                          "factors/movesrun.csv", "factors/vehicles.csv")
#' }
#' @export
movesr_export_vein <- function(path, running = NULL, start = NULL, evap = NULL,
                               database,
                               scenario_id = "movesr", run_id = 1L,
                               description = "movesr generated factors") {
  if (is.null(running) && is.null(start) && is.null(evap)) {
    stop("Supply running, start and/or evap rates")
  }
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  version <- paste0("movesr ", utils::packageVersion("movesr"),
                    " (MOVES6 algorithm port)")
  files <- character()
  prep <- function(x, rate) {
    x <- data.table::as.data.table(data.table::copy(x))
    x[, ratePerDistanceKm := NULL]
    x[, `:=`(MOVESScenarioID = scenario_id, MOVESRunID = as.integer(run_id),
             SCC = NA_character_)]
    first <- c("MOVESScenarioID", "MOVESRunID")
    data.table::setcolorder(x, c(first, setdiff(names(x), c(first, rate)), rate))
    x
  }
  if (!is.null(running)) {
    f <- file.path(path, "rateperdistance.csv")
    data.table::fwrite(suppressWarnings(prep(running, "ratePerDistance")), f)
    files <- c(files, f)
  }
  if (!is.null(start)) {
    f <- file.path(path, "rateperstart.csv")
    x <- data.table::as.data.table(data.table::copy(start))
    x[, `:=`(MOVESScenarioID = scenario_id, MOVESRunID = as.integer(run_id),
             SCC = NA_character_)]
    first <- c("MOVESScenarioID", "MOVESRunID")
    data.table::setcolorder(x, c(first, setdiff(names(x), c(first, "ratePerStart")),
                                 "ratePerStart"))
    data.table::fwrite(x, f)
    files <- c(files, f)
  }
  if (!is.null(evap)) {
    x <- data.table::as.data.table(data.table::copy(evap))
    x[, `:=`(MOVESScenarioID = scenario_id, MOVESRunID = as.integer(run_id),
             SCC = NA_character_)]
    if ("countyID" %in% names(x)) x[, zoneID := countyID * 10L]
    first <- c("MOVESScenarioID", "MOVESRunID")
    v <- x[processID %in% c(11L, 13L)]
    data.table::setcolorder(v, c(first, setdiff(names(v), c(first, "ratePerVehicle")),
                                 "ratePerVehicle"))
    f <- file.path(path, "ratepervehicle.csv")
    data.table::fwrite(v, f)
    files <- c(files, f)
    pr <- x[processID == 12L]
    if (nrow(pr)) {
      pr[, temperatureProfileID := if ("zoneID" %in% names(pr))
        zoneID * 10000L + monthID * 100L else NA_integer_]
      data.table::setcolorder(pr, c(first, "temperatureProfileID",
                                    setdiff(names(pr), c(first, "temperatureProfileID",
                                                         "ratePerVehicle")),
                                    "ratePerVehicle"))
      f <- file.path(path, "rateperprofile.csv")
      data.table::fwrite(pr, f)
      files <- c(files, f)
    }
  }
  run <- data.table::data.table(
    MOVESRunID = as.integer(run_id), outputTimePeriod = "Hour",
    timeUnits = "hour", distanceUnits = "mi", massUnits = "g",
    energyUnits = "KJ", runSpecDescription = description, scale = "Rates",
    defaultDatabaseUsed = "movesdb20260729", masterVersion = version,
    domain = "DEFAULT", models = "onroad")
  f <- file.path(path, "movesrun.csv")
  data.table::fwrite(run, f)
  files <- c(files, f)
  if (!is.null(start) || !is.null(evap)) {
    f2 <- file.path(path, "movesrun_events.csv")
    data.table::fwrite(run, f2)
    files <- c(files, f2)
  }
  all <- data.table::rbindlist(list(running, start, evap), fill = TRUE)
  veh <- unique(all[, .(sourceTypeID, fuelTypeID)])
  if (!missing(database)) {
    ft <- movesr_table(database, "fueltype")[, .(fuelTypeID, fuelTypeDesc)]
    st <- movesr_table(database, "sourceusetype")[, .(sourceTypeID, sourceTypeName)]
    veh <- merge(merge(veh, ft, by = "fuelTypeID", all.x = TRUE), st,
                 by = "sourceTypeID", all.x = TRUE)
  } else {
    veh[, `:=`(fuelTypeDesc = NA_character_, sourceTypeName = NA_character_)]
  }
  data.table::setcolorder(veh, c("fuelTypeID", "fuelTypeDesc", "sourceTypeID",
                                 "sourceTypeName"))
  f <- file.path(path, "vehicles.csv")
  data.table::fwrite(veh[order(sourceTypeID, fuelTypeID)], f)
  invisible(c(files, f))
}
