# Evaporative emissions: TankTemperatureGenerator port (work in progress).

.f32 <- function(x) {
  out <- rep(NA_real_, length(x))
  ok <- is.finite(x)
  if (any(ok)) {
    out[ok] <- readBin(writeBin(as.numeric(x[ok]), raw(), size = 4L),
      "double", size = 4L, n = sum(ok))
  }
  out
}

#' MOVES cold-soak tank temperature
#'
#' @description Port of `TankTemperatureGenerator.calculateColdSoakTankTemperature`
#' (step 200). The 24 hourly ambient temperatures are interpolated to quarter
#' hours, and the tank follows the ambient temperature with a lag:
#' \preformatted{
#' Tq[h, s]  = T[h] + (s - 1) * 0.25 * (T[h+1] - T[h])      (hour 24 -> hour 1)
#' Ttank     = 1.4 * sum(delta) + Tq[1, 1]
#' delta     = Tq - Ttank ;  sum(delta) accumulates over the day
#' coldSoakTankTemperature[h] = Ttank[h, s = 1]
#' }
#' Arithmetic is single precision, as in MOVES (Java `float`).
#' @param temperature 24 hourly temperatures (F), hours 1--24.
#' @return data.table hourID, timeStepID, quarterHourTemperature,
#' quarterHourTankTemperature; the cold-soak temperature is timeStepID 1.
#' @export
movesr_cold_soak_tank_temperature <- function(temperature) {
  if (length(temperature) != 24L || anyNA(temperature)) {
    stop("Supply 24 hourly temperatures (hours 1-24)")
  }
  t <- temperature
  nxt <- c(t[-1], t[1])
  q <- data.table::CJ(hourID = 1:24, timeStepID = 1:4)
  q[, quarterHourTemperature := t[hourID] + (timeStepID - 1) * 0.25 *
    (nxt[hourID] - t[hourID])]
  qt <- .f32(q$quarterHourTemperature)
  first <- qt[1]
  tank <- numeric(nrow(q))
  sum_delta <- 0
  for (i in seq_len(nrow(q))) {
    tank[i] <- .f32(.f32(1.4) * sum_delta + first)
    delta <- .f32(qt[i] - tank[i])
    sum_delta <- .f32(sum_delta + delta)
  }
  q[, quarterHourTankTemperature := tank]
  q[]
}
