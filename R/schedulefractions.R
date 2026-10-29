#' Build MOVES drive-schedule fractions for average-speed bins
#'
#' @description Brackets each average-speed bin between the closest lower and
#' higher drive-schedule average speeds and interpolates their fractions,
#' following the MOVES `BracketSchedule`/`DriveScheduleFraction` logic.
#' @param drive_schedule `DriveSchedule` with driveScheduleID and averageSpeed.
#' @param schedule_assoc `DriveScheduleAssoc` with sourceTypeID, roadTypeID and
#' driveScheduleID.
#' @param average_speed_bin `AvgSpeedBin` with avgSpeedBinID and avgBinSpeed.
#' @param source_type_id Optional source type filter.
#' @param road_type_id Optional road type filter.
#' @return A data.table with sourceTypeID, roadTypeID, avgSpeedBinID,
#' driveScheduleID and driveScheduleFraction.
#' @details When a speed bin lies outside the available schedules, the nearest
#' schedule is used with fraction 1. MOVES may treat this as extrapolation; the
#' function does not hide that condition.
#' @export
movesr_schedule_fractions <- function(drive_schedule, schedule_assoc,
                                      average_speed_bin,
                                      source_type_id = NULL,
                                      road_type_id = NULL) {
  ds <- data.table::as.data.table(data.table::copy(drive_schedule))
  assoc <- data.table::as.data.table(data.table::copy(schedule_assoc))
  bins <- data.table::as.data.table(data.table::copy(average_speed_bin))
  for (pair in list(
      list(ds, c("driveScheduleID", "averageSpeed"), "drive_schedule"),
      list(assoc, c("sourceTypeID", "roadTypeID", "driveScheduleID"),
           "schedule_assoc"),
      list(bins, c("avgSpeedBinID", "avgBinSpeed"), "average_speed_bin"))) {
    missing <- setdiff(pair[[2]], names(pair[[1]]))
    if (length(missing)) {
      stop(pair[[3]], " is missing: ", paste(missing, collapse = ", "))
    }
  }
  if (!is.null(source_type_id)) assoc <- assoc[sourceTypeID %in% source_type_id]
  if (!is.null(road_type_id)) assoc <- assoc[roadTypeID %in% road_type_id]
  if (!nrow(assoc)) stop("No schedules match the filter")

  x <- merge(assoc, ds, by = "driveScheduleID", allow.cartesian = TRUE)
  x[, dummy := 1L]
  bins[, dummy := 1L]
  x <- merge(x, bins, by = "dummy", allow.cartesian = TRUE)
  x[, dummy := NULL]
  x <- x[!is.na(averageSpeed) & !is.na(avgBinSpeed)]
  if (!nrow(x)) stop("No schedule/speed-bin pairs")

  # Closest schedules below and above each bin.
  x[, loSpeed := suppressWarnings(max(averageSpeed[averageSpeed <= avgBinSpeed])),
    by = .(sourceTypeID, roadTypeID, avgSpeedBinID)]
  x[, hiSpeed := suppressWarnings(min(averageSpeed[averageSpeed > avgBinSpeed])),
    by = .(sourceTypeID, roadTypeID, avgSpeedBinID)]
  x[!is.finite(loSpeed), loSpeed := min(averageSpeed),
    by = .(sourceTypeID, roadTypeID, avgSpeedBinID)]
  x[!is.finite(hiSpeed), hiSpeed := max(averageSpeed),
    by = .(sourceTypeID, roadTypeID, avgSpeedBinID)]

  x[, loFraction := data.table::fifelse(
    hiSpeed == loSpeed, 1,
    (hiSpeed - avgBinSpeed) / (hiSpeed - loSpeed))]
  x[, loFraction := pmin(pmax(loFraction, 0), 1)]

  lo <- x[averageSpeed == loSpeed,
          .(driveScheduleID, weight = loFraction),
          by = .(sourceTypeID, roadTypeID, avgSpeedBinID)]
  hi <- x[averageSpeed == hiSpeed,
          .(driveScheduleID, weight = 1 - loFraction),
          by = .(sourceTypeID, roadTypeID, avgSpeedBinID)]
  out <- data.table::rbindlist(list(lo, hi))
  out <- out[, .(driveScheduleFraction = sum(weight)), by =
               .(sourceTypeID, roadTypeID, avgSpeedBinID, driveScheduleID)]
  out <- out[driveScheduleFraction > 0]
  data.table::setkey(out, sourceTypeID, roadTypeID, avgSpeedBinID, driveScheduleID)
  out
}
