.physics_rows <- function(physics) {
  p <- data.table::as.data.table(data.table::copy(physics))
  if (!"regClassID" %in% names(p)) p[, regClassID := 0L]
  if (!"beginModelYearID" %in% names(p)) p[, beginModelYearID := 1950L]
  if (!"endModelYearID" %in% names(p)) p[, endModelYearID := 2060L]
  p <- p[sourceMass != 0]
  data.table::setorder(p, sourceTypeID, regClassID, beginModelYearID)
  p[, physicsID := seq_len(.N)]
  p
}

#' Calculate MOVES operating-mode fractions from drive schedules
#'
#' @description Port of the MOVES external Go generator
#' (`generators/baserategenerator`: `findDriveCycles`,
#' `calculateDriveCycleOpModeDistribution`, `processDriveCycles`) that MOVES
#' uses for running-exhaust rates. Each row of `source_physics` (source type,
#' regulatory class and model-year range) gets its own distribution.
#' @param drive_schedule_second `DriveScheduleSecond`: driveScheduleID, second,
#' speed in mph.
#' @param drive_schedule `DriveSchedule`: driveScheduleID, averageSpeed.
#' @param source_physics `SourceUseTypePhysics`. Required: sourceTypeID,
#' rollingTermA, rotatingTermB, dragTermC, sourceMass, fixedMassFactor.
#' Optional: regClassID, beginModelYearID, endModelYearID.
#' @param operating_mode `OperatingMode` table.
#' @param opmode_polproc Optional `OpModePolProcAssoc`. When supplied the
#' result is expanded to the pollutant/processes and their associated modes;
#' MOVES itself does not filter by this table.
#' @param schedule_assoc `DriveScheduleAssoc`.
#' @param schedule_fraction Output of \code{\link{movesr_schedule_fractions}}.
#' If omitted, each associated schedule gets weight 1 with `avgSpeedBinID = 0`.
#' @param source_type_id Optional source type filter.
#' @param road_type_id Optional road type filter.
#' @return A data.table with sourceTypeID, regClassID, beginModelYearID,
#' endModelYearID, roadTypeID, avgSpeedBinID, (polProcessID), opModeID and
#' opModeFraction.
#' @details Rules (MOVES Go generator):
#' \itemize{
#' \item speed < 1 mph is idle (mode 1);
#' \item acceleration is `speed[t] - speed[t-1]`, 0 when the previous second
#' is missing, and the first second copies the second second;
#' \item braking (mode 0): acceleration <= -2 mph/s, or three consecutive
#' seconds (t-2, t-1, t present in the schedule) below -1 mph/s;
#' \item `VSP = (A v + B v^2 + C v^3 + M v a) / fixedMassFactor` with v, a in
#' m/s, double precision;
#' \item VSP/speed bins from `OperatingMode` 2--99, excluding 26 and 36;
#' \item only seconds > 0 are counted; fractions are per schedule and then
#' weighted by the bracketing `driveScheduleFraction`.
#' }
#' @export
movesr_opmodes <- function(drive_schedule_second, drive_schedule,
                           source_physics, operating_mode,
                           opmode_polproc = NULL,
                           schedule_assoc, schedule_fraction = NULL,
                           source_type_id = NULL, road_type_id = NULL) {
  dss <- data.table::as.data.table(data.table::copy(drive_schedule_second))
  modes <- data.table::as.data.table(data.table::copy(operating_mode))
  assoc <- data.table::as.data.table(data.table::copy(schedule_assoc))
  for (pair in list(
      list(dss, c("driveScheduleID", "second", "speed"), "drive_schedule_second"),
      list(source_physics, c("sourceTypeID", "rollingTermA", "rotatingTermB",
                             "dragTermC", "sourceMass", "fixedMassFactor"),
           "source_physics"),
      list(modes, c("opModeID", "VSPLower", "VSPUpper", "speedLower",
                    "speedUpper"), "operating_mode"),
      list(assoc, c("sourceTypeID", "roadTypeID", "driveScheduleID"),
           "schedule_assoc"))) {
    missing <- setdiff(pair[[2]], names(pair[[1]]))
    if (length(missing)) {
      stop(pair[[3]], " is missing: ", paste(missing, collapse = ", "))
    }
  }
  physics <- .physics_rows(source_physics)
  physics <- physics[fixedMassFactor > 0]
  if (!is.null(source_type_id)) {
    assoc <- assoc[sourceTypeID %in% source_type_id]
    physics <- physics[sourceTypeID %in% source_type_id]
  }
  if (!is.null(road_type_id)) assoc <- assoc[roadTypeID %in% road_type_id]
  if (!nrow(assoc) || !nrow(physics)) stop("No schedules match the filter")

  if (is.null(schedule_fraction)) {
    fraction <- unique(assoc[, .(sourceTypeID, roadTypeID, driveScheduleID)])
    fraction[, `:=`(avgSpeedBinID = 0L, driveScheduleFraction = 1)]
  } else {
    fraction <- data.table::as.data.table(data.table::copy(schedule_fraction))
    need <- c("sourceTypeID", "roadTypeID", "avgSpeedBinID",
              "driveScheduleID", "driveScheduleFraction")
    missing <- setdiff(need, names(fraction))
    if (length(missing)) {
      stop("schedule_fraction is missing: ", paste(missing, collapse = ", "))
    }
    fraction <- fraction[sourceTypeID %in% physics$sourceTypeID &
                           driveScheduleFraction > 0]
    if (!is.null(road_type_id)) fraction <- fraction[roadTypeID %in% road_type_id]
  }

  dss <- dss[driveScheduleID %in% fraction$driveScheduleID]
  data.table::setorder(dss, driveScheduleID, second)
  dss[, accel := {
    prv <- data.table::shift(speed, 1L)
    psec <- data.table::shift(second, 1L)
    a <- ifelse(!is.na(psec) & psec == second - 1L, speed - prv, 0)
    if (.N > 1L && second[2L] == second[1L] + 1L) a[1L] <- a[2L]
    a
  }, by = driveScheduleID]
  dss[, brake := {
    a1 <- data.table::shift(accel, 1L)
    a2 <- data.table::shift(accel, 2L)
    s1 <- data.table::shift(second, 1L)
    s2 <- data.table::shift(second, 2L)
    three <- !is.na(s2) & s1 == second - 1L & s2 == second - 2L &
      accel < -1 & a1 < -1 & a2 < -1
    accel <= -2 | three
  }, by = driveScheduleID]
  dss[, idle := speed < 1]
  dss <- dss[second > 0]
  dss[, `:=`(v = speed * 0.44704, a = accel * 0.44704)]

  sched <- unique(fraction[, .(sourceTypeID, driveScheduleID)])
  x <- merge(sched, physics[, .(sourceTypeID, physicsID, rollingTermA,
                                 rotatingTermB, dragTermC, sourceMass,
                                 fixedMassFactor)],
             by = "sourceTypeID", allow.cartesian = TRUE)
  x <- merge(x, dss[, .(driveScheduleID, second, speed, v, a, brake, idle)],
             by = "driveScheduleID", allow.cartesian = TRUE)
  x[, vsp := (rollingTermA * v + rotatingTermB * v^2 + dragTermC * v^3 +
                sourceMass * v * a) / fixedMassFactor]
  m <- rep(NA_integer_, nrow(x))
  m[x$idle] <- 1L
  m[is.na(m) & x$brake] <- 0L
  bins <- modes[opModeID > 1L & opModeID < 100L & !opModeID %in% c(26L, 36L)]
  data.table::setorder(bins, opModeID)
  for (i in seq_len(nrow(bins))) {
    b <- bins[i]
    idx <- is.na(m)
    if (!is.na(b$VSPLower)) idx <- idx & x$vsp >= b$VSPLower
    if (!is.na(b$VSPUpper)) idx <- idx & x$vsp < b$VSPUpper
    if (!is.na(b$speedLower)) idx <- idx & x$speed >= b$speedLower
    if (!is.na(b$speedUpper)) idx <- idx & x$speed < b$speedUpper
    m[idx] <- b$opModeID
  }
  y <- data.table::data.table(physicsID = x$physicsID,
                              sourceTypeID = x$sourceTypeID,
                              driveScheduleID = x$driveScheduleID,
                              opModeID = m)[!is.na(opModeID)]
  y <- y[, .N, by = .(physicsID, sourceTypeID, driveScheduleID, opModeID)]
  y[, modeFraction := N / sum(N), by = .(physicsID, driveScheduleID)]
  w <- merge(y, fraction, by = c("sourceTypeID", "driveScheduleID"),
             allow.cartesian = TRUE)
  res <- w[, .(opModeFraction = sum(modeFraction * driveScheduleFraction)),
           by = .(physicsID, sourceTypeID, roadTypeID, avgSpeedBinID, opModeID)]
  res <- res[opModeFraction > 0]
  res <- merge(res, physics[, .(physicsID, regClassID, beginModelYearID,
                                 endModelYearID)], by = "physicsID")
  res[, physicsID := NULL]
  cols <- c("sourceTypeID", "regClassID", "beginModelYearID", "endModelYearID",
            "roadTypeID", "avgSpeedBinID")
  if (!is.null(opmode_polproc)) {
    pp <- unique(data.table::as.data.table(opmode_polproc)[, .(polProcessID,
                                                                opModeID)])
    res <- merge(res, pp, by = "opModeID", allow.cartesian = TRUE)
    cols <- c(cols, "polProcessID")
  }
  data.table::setcolorder(res, c(cols, "opModeID", "opModeFraction"))
  data.table::setkeyv(res, c(cols, "opModeID"))
  res
}
