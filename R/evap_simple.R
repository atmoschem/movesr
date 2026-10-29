# Simplified evaporative tank model: interpolation in a library of exact
# TankTemperatureGenerator results for reference temperature profiles.

#' Build a tank-temperature library for the simplified evap method
#'
#' @description Runs the exact \code{\link{movesr_tank_temperature}} for each
#' 24-hour reference profile and stores what the simplified method needs:
#' hot-soak (150) and operating (300) tank temperature rises over the cold-soak
#' temperature, soak activity fractions and cold-soak initial-hour fractions,
#' with the profile's daily mean and range.
#' @param database A `movesr_database`.
#' @param profiles Matrix or list of 24-hour temperature profiles (F).
#' @param day_id Day types to build (5 weekday, 2 weekend).
#' @return A `movesr_tank_library` list (profile, rise, soak, initial).
#' @export
movesr_tank_library <- function(database, profiles, day_id = c(5L, 2L)) {
  if (is.character(database)) database <- movesr_database(database)
  if (is.matrix(profiles)) profiles <- lapply(seq_len(nrow(profiles)), function(i) profiles[i, ])
  out <- list()
  for (d in day_id) for (i in seq_along(profiles)) {
    x <- movesr_tank_temperature(database, profiles[[i]], day_id = d)
    out[[length(out) + 1L]] <- .tank_library_entry(x, d, length(out) + 1L)
  }
  .tank_library_bind(out)
}

.tank_library_entry <- function(x, day_id, id) {
  t <- x$temperature
  cs <- x$cold_soak$coldSoakTankTemperature
  a <- x$average_tank_temperature[opModeID %in% c(150L, 300L)]
  a <- a[, .(profileID = id, opModeID, tankTemperatureGroupID, hourID,
    rise = averageTankTemperature - cs[hourID])]
  s <- x$soak_activity_fraction[opModeID == 150L,
    .(profileID = id, sourceTypeID,
      hourID = hourDayID %/% 10L, hotSoak = soakActivityFraction)]
  ci <- x$cold_soak_initial_hour_fraction[
    , .(profileID = id, sourceTypeID, hourID = hourDayID %/% 10L,
      initialHourID = initialHourDayID %/% 10L, fraction = coldSoakInitialHourFraction)]
  list(profile = data.table::data.table(profileID = id, dayID = day_id,
    tmean = mean(t), trange = max(t) - min(t)),
  rise = a, soak = s, initial = ci)
}

.tank_library_bind <- function(entries) {
  parts <- c("profile", "rise", "soak", "initial")
  lib <- lapply(parts, function(n) data.table::rbindlist(lapply(entries, `[[`, n)))
  names(lib) <- parts
  class(lib) <- "movesr_tank_library"
  lib
}

#' MOVES tank temperatures and soak activity (simplified method)
#'
#' @description Fast alternative to \code{\link{movesr_tank_temperature}}
#' (no trip simulation):
#' \itemize{
#' \item cold-soak tank temperature: exact MOVES recurrence;
#' \item hot-soak (150) and operating (300) tank temperatures:
#' `Tcold(h) + rise(h)`, with the rise interpolated from the library;
#' \item soak activity fractions and cold-soak initial-hour fractions:
#' interpolated from the library.
#' }
#' Interpolation uses inverse squared distance among the `k` library profiles
#' of the same day type nearest in (daily mean, diurnal range) of the ambient
#' temperature, distances in F. (Hour-by-hour matching on ambient and
#' cold-soak temperatures was tested and was not more accurate.) The default library holds exact results for
#' MOVES default `ZoneMonthHour` profiles spanning US climates (see
#' `data-raw/evap_library.R`). Error versus the exact method is documented in
#' PORTING.md.
#' @param database A `movesr_database`.
#' @param temperature 24 hourly ambient temperatures (F).
#' @param day_id 5 (weekday) or 2 (weekend).
#' @param library A \code{\link{movesr_tank_library}}; default the built-in one.
#' @param k Number of library profiles used.
#' @return Same structure as \code{\link{movesr_tank_temperature}} (without the
#' trip-level intermediate tables).
#' @export
movesr_tank_temperature_simple <- function(database, temperature, day_id = 5L,
                                           library = NULL, k = 3L) {
  if (length(temperature) != 24L) stop("temperature must have 24 hourly values")
  if (is.null(library)) library <- get0(".movesr_tank_library", envir = asNamespace("movesr"),
    inherits = FALSE)
  if (is.null(library)) stop("No tank library available; build one with movesr_tank_library()")
  cs <- movesr_cold_soak_tank_temperature(temperature)[timeStepID == 1L]
  cold <- cs$quarterHourTankTemperature
  p <- library$profile[dayID == day_id]
  if (!nrow(p)) stop("The tank library has no profiles for day type ", day_id)
  p[, dist := sqrt((tmean - mean(temperature))^2 +
    (trange - diff(range(temperature)))^2)]
  wt <- data.table::CJ(profileID = p$profileID, hourID = 1:24)
  wt <- merge(wt, p[, .(profileID, dist)], by = "profileID")
  if (min(p$dist) > 5) {
    warning(sprintf(paste0("Temperature profile (mean %.1f F, range %.1f F) is %.1f F from the ",
      "nearest library profile; consider method = \"original\""),
    mean(temperature), diff(range(temperature)), min(p$dist)))
  }
  data.table::setorder(wt, hourID, dist)
  wt <- wt[, .SD[seq_len(min(k, .N))], by = hourID]
  wt[, w := if (any(dist < 1e-9)) as.numeric(dist < 1e-9) else 1 / dist^2, by = hourID]
  wt[, w := w / sum(w), by = hourID]
  wt <- wt[, .(profileID, hourID, w)]
  hd <- function(h) h * 10L + day_id
  # Tank temperatures: weighted rise over cold soak (renormalised over the
  # profiles that have the mode in that hour).
  r <- merge(library$rise, wt, by = c("profileID", "hourID"))
  r <- r[, .(rise = sum(w * rise) / sum(w)), by = .(opModeID, tankTemperatureGroupID, hourID)]
  r[, averageTankTemperature := .f32(cold[hourID] + rise)]
  groups <- unique(library$rise$tankTemperatureGroupID)
  a151 <- data.table::CJ(tankTemperatureGroupID = groups, hourID = 1:24)
  a151[, `:=`(opModeID = 151L, averageTankTemperature = cold[hourID])]
  att <- data.table::rbindlist(list(a151, r[, .(tankTemperatureGroupID, hourID, opModeID,
    averageTankTemperature)]),
  use.names = TRUE)
  att[, hourDayID := hd(hourID)]
  # Soak fractions: hot soak interpolated (absent = 0), cold soak the rest.
  s <- merge(library$soak, wt, by = c("profileID", "hourID"))
  st <- unique(library$soak$sourceTypeID)
  g <- data.table::CJ(sourceTypeID = st, hourID = 1:24)
  s <- s[, .(hotSoak = sum(w * hotSoak)), by = .(sourceTypeID, hourID)]
  g <- merge(g, s, by = c("sourceTypeID", "hourID"), all.x = TRUE)
  g[is.na(hotSoak), hotSoak := 0]
  saf <- data.table::rbindlist(list(
    g[hotSoak > 0, .(sourceTypeID, hourDayID = hd(hourID), opModeID = 150L,
      soakActivityFraction = hotSoak)],
    g[, .(sourceTypeID, hourDayID = hd(hourID), opModeID = 151L,
      soakActivityFraction = 1 - hotSoak)]))
  ci <- merge(library$initial, wt, by = c("profileID", "hourID"))
  ci <- ci[, .(coldSoakInitialHourFraction = sum(w * fraction)),
    by = .(sourceTypeID, hourID, initialHourID)]
  ci[, coldSoakInitialHourFraction := coldSoakInitialHourFraction /
    sum(coldSoakInitialHourFraction), by = .(sourceTypeID, hourID)]
  ci <- ci[coldSoakInitialHourFraction > 0,
    .(sourceTypeID, hourDayID = hd(hourID), initialHourDayID = hd(initialHourID),
      coldSoakInitialHourFraction)]
  list(temperature = temperature,
    cold_soak = data.table::data.table(hourID = 1:24, coldSoakTankTemperature = cold),
    average_tank_temperature = att[, .(hourDayID, hourID, tankTemperatureGroupID,
      opModeID, averageTankTemperature)],
    soak_activity_fraction = saf,
    cold_soak_initial_hour_fraction = ci,
    library_weights = wt)
}
