# Port of TankTemperatureGenerator.java (MOVES6), steps 100-282.
# Times are minutes from the start of the day; temperatures in F.

.ttg_trips <- function(database, day_id) {
  svd <- data.table::copy(movesr_table(database, "samplevehicleday"))[dayID == day_id]
  svt <- data.table::copy(movesr_table(database, "samplevehicletrip"))[dayID == day_id]
  if ("keyontime" %in% names(svt)) data.table::setnames(svt, "keyontime", "keyOnTime")
  # Step 210: marker trips (M), first trips of the day (F), normal trips (N).
  svt[, tripType := "N"]
  svt[is.na(keyOnTime) & is.na(priorTripID), tripType := "M"]
  mk <- svt[tripType == "M", .(vehID, dayID, priorTripID = tripID, marker = TRUE)]
  svt <- merge(svt, mk, by = c("vehID", "dayID", "priorTripID"), all.x = TRUE)
  svt[marker == TRUE, `:=`(priorTripID = NA_integer_, tripType = "F")]
  svt[, marker := NULL]
  svt[is.na(priorTripID) & !is.na(keyOnTime) & tripType == "N", tripType := "F"]
  list(day = svd, trip = svt)
}

# Step 230: SampleVehicleTripByHour.
.ttg_trip_by_hour <- function(trips, day) {
  x <- merge(trips[tripType != "M"], day[, .(vehID, dayID)], by = c("vehID", "dayID"))
  data.table::setorder(x, vehID, dayID, tripID, keyOnTime)
  x[, endOfHour0 := (keyOnTime %/% 60L + 1L) * 60L]
  x[, n := data.table::fifelse(keyOffTime <= endOfHour0, 1L,
    1L + as.integer(ceiling((keyOffTime - endOfHour0) / 60)))]
  y <- x[rep(seq_len(.N), n)]
  y[, i := seq_len(.N) - 1L, by = .(vehID, dayID, tripID)]
  y[, endOfHour := endOfHour0 + 60L * i]
  y[, hourID := ((hourID - 1L + i) %% 24L) + 1L]
  y[i > 0L, keyOnTime := endOfHour - 60L + 1L]
  y[, keyOffTime := pmin(keyOffTime, endOfHour)]
  y[, `:=`(startOfTrip = as.integer(i == 0L), endOfTrip = as.integer(i == n - 1L))]
  y[is.na(priorTripID), priorTripID := 0L]
  y[, hourDayID := hourID * 10L + dayID]
  y[, .(vehID, tripID, dayID, hourDayID, hourID, priorTripID, keyOnTime,
    keyOffTime, endOfHour, startOfTrip, endOfTrip)]
}

# Step 240: HotSoakEventByHour.
.ttg_hot_soak_events <- function(svth) {
  ends <- svth[endOfTrip == 1L, .(vehID, tripID, dayID, hourID, keyOffTime, endOfHour)]
  nxt <- svth[startOfTrip == 1L & priorTripID > 0L,
    .(nextKeyOnTime = keyOnTime[1]), by = .(vehID, dayID, priorTripID)]
  ends <- merge(ends, nxt, by.x = c("vehID", "dayID", "tripID"),
    by.y = c("vehID", "dayID", "priorTripID"), all.x = TRUE)
  out <- list()
  # First hour.
  e <- data.table::copy(ends)
  e[, `:=`(runToEnd = is.na(nextKeyOnTime), hotSoakBegin = keyOffTime,
    startOfHotSoak = 1L)]
  e[is.na(nextKeyOnTime), nextKeyOnTime := endOfHour]
  e[, `:=`(hotSoakEnd = data.table::fifelse(!runToEnd & nextKeyOnTime < endOfHour,
    nextKeyOnTime, endOfHour),
  endOfHotSoak = as.integer(!runToEnd & nextKeyOnTime < endOfHour))]
  out[[1]] <- e
  active <- e[endOfHotSoak == 0L]
  k <- 1L
  while (nrow(active)) {
    active <- active[!(runToEnd & hourID == 24L)]
    if (!nrow(active)) break
    active <- data.table::copy(active)
    active[runToEnd == TRUE, nextKeyOnTime := endOfHour + 60L + 1L]
    active[, `:=`(hourID = data.table::fifelse(hourID == 24L, 1L, hourID + 1L),
      hotSoakBegin = endOfHour + 1L, endOfHour = endOfHour + 60L,
      startOfHotSoak = 0L)]
    active[, `:=`(hotSoakEnd = data.table::fifelse(nextKeyOnTime > endOfHour,
      endOfHour, nextKeyOnTime),
    endOfHotSoak = as.integer(nextKeyOnTime <= endOfHour))]
    k <- k + 1L
    out[[k]] <- active
    active <- active[endOfHotSoak == 0L]
  }
  ev <- data.table::rbindlist(out, use.names = TRUE)
  ev[, hourDayID := hourID * 10L + dayID]
  data.table::setorder(ev, vehID, tripID, hotSoakBegin)
  ev[, .(vehID, tripID, dayID, hourID, hourDayID, endOfHour, hotSoakBegin,
    hotSoakEnd, startOfHotSoak, endOfHotSoak)]
}

# Step 251: minute-by-minute hot soak for a set of trip ends (vectorised).
# ends: id, vehID, dayID, tripID, keyOffTemp. Returns list(end = id/endTemp,
# minutes = id/hourID/minute/soakTankTemp).
.ttg_hot_soak <- function(ends, events, amb, cold) {
  ev <- merge(ends[, .(id, vehID, dayID, tripID)], events,
    by = c("vehID", "dayID", "tripID"), allow.cartesian = TRUE)
  ev <- ev[hotSoakEnd > hotSoakBegin]
  data.table::setorder(ev, id, hotSoakBegin)
  ev[, n := hotSoakEnd - hotSoakBegin]
  m <- ev[rep(seq_len(.N), n), .(id, hourID, hotSoakBegin)]
  m[, minute := hotSoakBegin + seq_len(.N) - 1L, by = .(id, hotSoakBegin)]
  m[, hotSoakBegin := NULL]
  m[, `:=`(temperature = amb[hourID], coldLimit = .f32(cold[hourID] + 3))]
  m[, k := seq_len(.N), by = id]
  data.table::setkey(m, id, k)
  # State per trip end (ids are 1..n); plain vectors, one step per minute.
  n <- nrow(ends)
  init <- numeric(n)
  init[ends$id] <- ends$keyOffTemp
  tsum <- numeric(n)
  done <- logical(n)
  last <- init
  idv <- m$id
  tv <- m$temperature
  cl <- m$coldLimit
  soakv <- rep(NA_real_, nrow(m))
  c14 <- .f32(1.4)
  idx_by_k <- split(seq_len(nrow(m)), m$k)
  for (rows in idx_by_k) {
    ids <- idv[rows]
    keep <- !done[ids]
    if (!any(keep)) next
    rows <- rows[keep]
    ids <- ids[keep]
    soak <- .f32(.f32(.f32(c14 * tsum[ids]) / 60) + init[ids])
    tsum[ids] <- .f32(tsum[ids] + .f32(tv[rows] - soak))
    hot <- soak > cl[rows]
    soakv[rows[hot]] <- soak[hot]
    last[ids] <- ifelse(hot, soak, -1000)
    done[ids] <- !hot
  }
  m[, soakTankTemp := soakv]
  list(end = data.table::data.table(id = seq_len(n), endTemp = last),
    minutes = m[!is.na(soakTankTemp), .(id, hourID, minute, soakTankTemp)])
}

#' MOVES tank temperatures and soak activity (original algorithm)
#'
#' @description Port of `TankTemperatureGenerator.java`: simulates the MOVES
#' sample-vehicle trips of one day type, minute by minute during hot soaks,
#' for every tank temperature group.
#' \enumerate{
#' \item cold-soak tank temperature from the 24-hour ambient profile;
#' \item trips split by hour; hot-soak events after each trip;
#' \item operating tank temperature:
#' `Toff = Ton + ((A + B (95 - Ton)) / 1.2) * minutes / 60`, max 140 F;
#' \item hot soak: `T = 1.4 sum(delta) / 60 + Toff`, ends when
#' `T <= Tcold + 3`; the next trip starts at the soak temperature;
#' \item average tank temperature by hour for modes 150 (hot soak), 151 (cold
#' soak) and 300 (operating);
#' \item soak activity fractions (hot/cold soak share of non-operating minutes);
#' \item cold-soak initial-hour fractions for vapor venting.
#' }
#' Single-precision arithmetic is emulated where MOVES uses Java `float`.
#' @param database A `movesr_database`.
#' @param temperature 24 hourly ambient temperatures (F).
#' @param day_id Day type: 5 weekday, 2 weekend.
#' @return list with temperature (the input), cold_soak, average_tank_temperature,
#' soak_activity_fraction, cold_soak_initial_hour_fraction and intermediate
#' tables (trip_by_hour, hot_soak_events, operating_temperature).
#' @export
movesr_tank_temperature <- function(database, temperature, day_id = 5L) {
  if (is.character(database)) database <- movesr_database(database)
  cs <- movesr_cold_soak_tank_temperature(temperature)[timeStepID == 1L]
  cold <- cs$quarterHourTankTemperature
  amb <- .f32(temperature)
  tr <- .ttg_trips(database, day_id)
  svth <- .ttg_trip_by_hour(tr$trip, tr$day)
  events <- .ttg_hot_soak_events(svth)
  stmyg <- movesr_table(database, "sourcetypemodelyeargroup")
  vt <- unique(merge(tr$day[, .(vehID, sourceTypeID)],
    stmyg[, .(sourceTypeID, tankTemperatureGroupID)],
    by = "sourceTypeID", allow.cartesian = TRUE)[
    , .(vehID, tankTemperatureGroupID)])
  rise <- movesr_table(database, "tanktemperaturerise")[
    , .(tankTemperatureGroupID, A = .f32(tankTemperatureRiseTermA),
      B = .f32(tankTemperatureRiseTermB))]
  vt <- merge(vt, rise, by = "tankTemperatureGroupID")

  # Step 250: operating temperatures, chained trip by trip.
  rows <- merge(svth, vt, by = "vehID", allow.cartesian = TRUE)
  data.table::setorder(rows, vehID, tankTemperatureGroupID, tripID, keyOnTime)
  level <- rows[priorTripID == 0L, .(vehID, dayID, tankTemperatureGroupID,
    tripID, initTemp = -1000)]
  level <- unique(level)
  ops <- list()
  soaks <- list()
  lv <- 0L
  while (nrow(level)) {
    lv <- lv + 1L
    r <- merge(rows, level, by = c("vehID", "dayID", "tankTemperatureGroupID",
      "tripID"))
    data.table::setorder(r, vehID, tankTemperatureGroupID, tripID, keyOnTime)
    r[, j := seq_len(.N), by = .(vehID, tankTemperatureGroupID, tripID)]
    r[, `:=`(keyOnTemp = NA_real_, keyOffTemp = NA_real_)]
    for (jj in seq_len(max(r$j))) {
      w <- which(r$j == jj)
      if (jj == 1L) {
        on <- data.table::fifelse(r$initTemp[w] <= -1000, cold[r$hourID[w]],
          r$initTemp[w])
      } else {
        on <- r$keyOffTemp[w - 1L]
      }
      off <- .f32(on + ((r$A[w] + r$B[w] * (95.0 - on)) / 1.2) *
        ((r$keyOffTime[w] - r$keyOnTime[w]) / 60.0))
      off[off > 140] <- 140
      r$keyOnTemp[w] <- on
      r$keyOffTemp[w] <- off
    }
    ops[[lv]] <- r
    ends <- r[endOfTrip == 1L, .(vehID, dayID, tankTemperatureGroupID, tripID,
      keyOffTemp)]
    ends[, id := seq_len(.N)]
    hs <- .ttg_hot_soak(ends, events, amb, cold)
    mins <- merge(hs$minutes, ends[, .(id, vehID, tankTemperatureGroupID, tripID)],
      by = "id")
    soaks[[lv]] <- mins
    nxt <- merge(ends[, .(id, vehID, dayID, tankTemperatureGroupID, tripID)],
      hs$end, by = "id")
    level <- unique(merge(
      unique(svth[startOfTrip == 1L & priorTripID > 0L,
        .(vehID, dayID, priorTripID, nextTrip = tripID)]),
      nxt[, .(vehID, dayID, tankTemperatureGroupID, priorTripID = tripID,
        initTemp = endTemp)],
      by = c("vehID", "dayID", "priorTripID"))[
      , .(vehID, dayID, tankTemperatureGroupID, tripID = nextTrip, initTemp)])
  }
  op <- data.table::rbindlist(ops, use.names = TRUE, fill = TRUE)
  hot <- data.table::rbindlist(soaks, use.names = TRUE)
  hot[, hourDayID := hourID * 10L + day_id]

  # Step 260: average tank temperature.
  a300 <- op[, .(averageTankTemperature =
    sum((keyOffTime - keyOnTime) * (keyOnTemp + keyOffTemp) / 2) /
      sum(keyOffTime - keyOnTime)),
  by = .(hourDayID, hourID, tankTemperatureGroupID)]
  a300[, opModeID := 300L]
  a151 <- unique(op[, .(hourDayID, hourID, tankTemperatureGroupID)])
  a151[, `:=`(opModeID = 151L, averageTankTemperature = cold[hourID])]
  a150 <- hot[, .(averageTankTemperature = mean(soakTankTemp)),
    by = .(hourDayID, hourID, tankTemperatureGroupID)]
  a150[, opModeID := 150L]
  att <- data.table::rbindlist(list(a151, a300, a150), use.names = TRUE)
  att[, averageTankTemperature := .f32(averageTankTemperature)]

  # Steps 220/270: soak activity fractions.
  vehsrc <- tr$day[, .(vehID, sourceTypeID)]
  em <- tr$day[, .(eMinutes = 60L * .N), by = sourceTypeID]
  om <- merge(svth, vehsrc, by = "vehID")[, .(oMinutes = sum(keyOffTime - keyOnTime)),
    by = .(sourceTypeID, hourDayID)]
  hc <- unique(merge(hot[, .(vehID, hourDayID, minute)], vehsrc, by = "vehID"))[
    , .(hotSoakOpModeCount = .N), by = .(sourceTypeID, hourDayID)]
  saf <- data.table::CJ(sourceTypeID = em$sourceTypeID,
    hourDayID = (1:24) * 10L + day_id)
  saf <- merge(merge(merge(saf, em, by = "sourceTypeID"), om, all.x = TRUE,
    by = c("sourceTypeID", "hourDayID")),
  hc, all.x = TRUE, by = c("sourceTypeID", "hourDayID"))
  saf[is.na(oMinutes), oMinutes := 0L]
  den <- saf$eMinutes - saf$oMinutes
  s150 <- saf[!is.na(hotSoakOpModeCount),
    .(sourceTypeID, hourDayID, opModeID = 150L,
      soakActivityFraction = hotSoakOpModeCount / (eMinutes - oMinutes))]
  s151 <- saf[, .(sourceTypeID, hourDayID, opModeID = 151L,
    soakActivityFraction = (eMinutes - oMinutes -
      data.table::fifelse(is.na(hotSoakOpModeCount), 0L,
        hotSoakOpModeCount)) / den)]
  safr <- data.table::rbindlist(list(s150, s151))

  csihf <- .ttg_cold_soak_initial_hour(tr$day, svth, hot, vt, day_id)
  list(temperature = temperature,
    cold_soak = cs[, .(hourID, coldSoakTankTemperature = quarterHourTankTemperature)],
    average_tank_temperature = att[, .(hourDayID, hourID, tankTemperatureGroupID,
      opModeID, averageTankTemperature)],
    soak_activity_fraction = safr,
    cold_soak_initial_hour_fraction = csihf,
    trip_by_hour = svth, hot_soak_events = events, operating_temperature = op,
    hot_soak_minutes = hot)
}

# Step 282: ColdSoakInitialHourFraction.
.ttg_cold_soak_initial_hour <- function(day, svth, hot, vt, day_id) {
  hours <- (1:24) * 10L + day_id
  vs <- day[, .(vehID, sourceTypeID)]
  sv <- svth[order(vehID, hourDayID, keyOnTime),
    .(minOn = keyOnTime[1], sumOp = sum(keyOffTime - keyOnTime)),
    by = .(vehID, hourDayID)]
  hs <- hot[, .(hsMin = min(minute), hsCount = .N),
    by = .(vehID, hourDayID, tankTemperatureGroupID)]
  g <- unique(vt[, .(vehID, tankTemperatureGroupID)])
  g <- merge(g, vs, by = "vehID")
  g[, initial := NA_integer_]
  acc <- list()
  for (h in seq_along(hours)) {
    hd <- hours[h]
    if (h == 1L) g[, initial := hd]
    x <- merge(g, sv[hourDayID == hd], by = "vehID", all.x = TRUE)
    x <- merge(x, hs[hourDayID == hd, .(vehID, tankTemperatureGroupID, hsMin, hsCount)],
      by = c("vehID", "tankTemperatureGroupID"), all.x = TRUE)
    hasT <- !is.na(x$minOn)
    hasH <- !is.na(x$hsCount) & x$hsCount > 0
    fromI <- rep(60L, nrow(x))
    cur <- rep(0L, nrow(x))
    reset <- rep(FALSE, nrow(x))
    b2 <- hasT & (!hasH | x$minOn < x$hsMin)
    b3 <- !b2 & (hasT | hasH)
    fromI[b2] <- x$minOn[b2] %% 60L
    cur[b2] <- 60L - fromI[b2] - x$sumOp[b2] - ifelse(hasH[b2], x$hsCount[b2], 0L)
    fromI[b3] <- 0L
    cur[b3] <- 60L - ifelse(hasH[b3], x$hsCount[b3], 0L) -
      ifelse(hasT[b3], x$sumOp[b3], 0L)
    reset <- b2 | b3
    acc[[h]] <- data.table::rbindlist(list(
      data.table::data.table(sourceTypeID = x$sourceTypeID, hourDayID = hd,
        initialHourDayID = x$initial, minutes = fromI),
      data.table::data.table(sourceTypeID = x$sourceTypeID, hourDayID = hd,
        initialHourDayID = hd, minutes = cur)))
    x[reset, initial := hd]
    g <- x[, .(vehID, tankTemperatureGroupID, sourceTypeID, initial)]
  }
  a <- data.table::rbindlist(acc)
  s <- a[, .(total = sum(minutes)), by = .(sourceTypeID, hourDayID)]
  a <- a[, .(minutes = sum(minutes)), by = .(sourceTypeID, hourDayID, initialHourDayID)]
  a <- merge(a, s, by = c("sourceTypeID", "hourDayID"))
  a <- a[total > 0 & minutes > 0]
  a[, coldSoakInitialHourFraction := minutes / total]
  a[, .(sourceTypeID, hourDayID, initialHourDayID, coldSoakInitialHourFraction)]
}
