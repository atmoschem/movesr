# Port of MultidayTankVaporVentingCalculator.sql (MOVES6, USE_MULTIDAY_DIURNALS)
# and TankFuelGenerator.java. MOVES float columns are emulated with .f32.

#' MOVES average tank gasoline (TankFuelGenerator)
#'
#' @description Tank RVP and ethanol content after weathering and
#' commingling, by evaporative fuel type:
#' \preformatted{
#' kGas = -7e-7 E^3 + 2e-4 E^2 + 0.0024 E + 1
#' kEth = 46.321 E^-0.8422 (1000 when E = 0)
#' gasPortionRVP = (RVP - kEth E/100 2.3) / (kGas (100 - E)/100)
#' zoneEvapTemp = -1.7474 + 1.029 Tmin + 0.99202 dT - 0.0025173 Tmin dT
#' loss = max(0, (-2.4908 + 0.026196 Te + 0.00076898 Te gp) / (-0.086 + 0.070592 gp))
#' weathered gp = gp (1 - 0.049 loss)
#' RVP_tank = ReddyRVP(weathered) * commingledRVP / ReddyRVP(unweathered)
#' }
#' @param database A `movesr_database`.
#' @param county_id,year,month_id Location and time.
#' @param temperature The 24 hourly temperatures of the month (F).
#' @param fuel_formulation Optional fuel supply with fuelTypeID,
#' fuelFormulationID, marketShare, RVP and ETOHVolume (defaults to the MOVES
#' fuel supply of the county).
#' @return data.table fuelTypeID, ETOHVolume, RVP.
#' @export
movesr_average_tank_gasoline <- function(database, county_id, year, month_id,
                                         temperature, fuel_formulation = NULL) {
  if (is.character(database)) database <- movesr_database(database)
  tb <- function(n) movesr_table(database, n)
  if (is.null(fuel_formulation)) {
    fuel_year <- tb("year")[yearID == year]$fuelYearID[1]
    region <- tb("regioncounty")[regionCodeID == 1L & countyID == county_id &
      fuelYearID == fuel_year]$regionID[1]
    mg <- tb("monthofanyyear")[monthID == month_id]$monthGroupID[1]
    fs <- tb("fuelsupply")[fuelRegionID == region & fuelYearID == fuel_year &
      monthGroupID == mg & marketShare > 0]
    ff <- merge(fs[, .(fuelFormulationID, marketShare)],
      tb("fuelformulation")[, .(fuelFormulationID, fuelSubtypeID, RVP, ETOHVolume)],
      by = "fuelFormulationID")
    ff <- merge(ff, tb("fuelsubtype")[, .(fuelSubtypeID, fuelTypeID)], by = "fuelSubtypeID")
  } else {
    ff <- data.table::as.data.table(fuel_formulation)[marketShare > 0]
  }
  evap <- tb("fueltype")[subjectToEvapCalculations == "Y"]$fuelTypeID
  ff <- ff[fuelTypeID %in% evap]
  kg <- function(e) .f32(-7e-7 * e^3 + 0.0002 * e^2 + 0.0024 * e + 1.0)
  ke <- function(e) .f32(ifelse(e > 0, 46.321 * e^-0.8422, 1000.0))
  ff[, `:=`(kG = kg(ETOHVolume), kE = ke(ETOHVolume))]
  ff[, gp := .f32((RVP - kE * ETOHVolume / 100.0 * 2.3) / (kG * (100.0 - ETOHVolume) / 100.0))]
  a <- ff[, .(lin = .f32(sum(RVP * marketShare) / sum(marketShare)),
    E = .f32(sum(ETOHVolume * marketShare) / sum(marketShare)),
    gp = .f32(sum(gp * marketShare) / sum(marketShare)),
    gasohol = if (any(ETOHVolume >= 4 & ETOHVolume <= 20))
      .f32(sum(marketShare[ETOHVolume >= 4 & ETOHVolume <= 20])) else NA_real_),
  by = fuelTypeID]
  a[, `:=`(aKG = kg(E), aKE = ke(E))]
  a[, noW := .f32(aKG * (100.0 - E) / 100.0 * gp + aKE * E / 100.0 * 2.3)]
  tmin <- .f32(min(temperature))
  tmax <- .f32(max(temperature))
  te <- .f32(if (tmax < 40 || tmax - tmin <= 0) (tmin + tmax) / 2.0 else
    -1.7474 + 1.029 * tmin + 0.99202 * (tmax - tmin) - 0.0025173 * tmin * (tmax - tmin))
  a[, loss := .f32(pmax(0, (-2.4908 + 0.026196 * te + 0.00076898 * te * gp) /
    (-0.0860 + 0.070592 * gp)))]
  a[, wgp := .f32(gp * (1.0 - loss * 0.049))]
  a[, wR := .f32(aKG * (100.0 - E) / 100.0 * wgp + aKE * E / 100.0 * 2.3)]
  brk <- c(1.0, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1)
  fac <- c(1.000, 1.018, 1.027, 1.034, 1.038, 1.040, 1.039, 1.035, 1.028, 1.016)
  a[, comm := .f32(lin * vapply(gasohol, function(g) {
    if (is.na(g)) return(1.0)
    i <- which(g >= brk)
    if (length(i)) fac[i[1]] else 1.0
  }, numeric(1)))]
  a[, .(fuelTypeID, ETOHVolume = E, RVP = .f32((wR * comm) / noW))]
}

# FillSampleVehicleSoaking: fraction of vehicles in their n-th day of cold soak
# (soakDayID 0 = not yet started today; 1..5 = multiday diurnal days).
.tvv_sample_vehicle_soaking <- function(database, day_id) {
  tb <- function(n) movesr_table(database, n)
  svd <- tb("samplevehicleday")[dayID == day_id, .(vehID, sourceTypeID)]
  svt <- data.table::copy(tb("samplevehicletrip"))[dayID == day_id]
  if ("keyontime" %in% names(svt)) data.table::setnames(svt, "keyontime", "keyOnTime")
  tot <- svd[, .(total = .N), by = sourceTypeID]
  first <- merge(svd, svt[!is.na(keyOnTime), .(vehID, hourID)], by = "vehID")[
    , .(hourID = min(hourID)), by = .(vehID, sourceTypeID)]
  cnt <- first[, .(n = .N), by = .(sourceTypeID, hourID)]
  g <- data.table::CJ(sourceTypeID = unique(cnt$sourceTypeID), hourID = 1:24)
  g <- merge(g, cnt, by = c("sourceTypeID", "hourID"), all.x = TRUE)
  g[is.na(n), n := 0L]
  g <- merge(g, tot, by = "sourceTypeID")
  data.table::setorder(g, sourceTypeID, hourID)
  g[, d0 := data.table::fifelse(total <= 0, 0, (total - cumsum(n)) / total), by = sourceTypeID]
  basis <- data.table::rbindlist(list(tb("samplevehiclesoakingdaybasis"),
    tb("samplevehiclesoakingdaybasisused")),
  use.names = TRUE, fill = TRUE)
  basis <- unique(basis[dayID == day_id, .(soakDayID, F)], by = "soakDayID")
  preset <- data.table::rbindlist(list(tb("samplevehiclesoakingday"),
    tb("samplevehiclesoakingdayused")),
  use.names = TRUE, fill = TRUE)
  preset <- if (nrow(preset)) unique(preset[dayID == day_id], by = c("soakDayID", "sourceTypeID")) else NULL
  out <- list(g[, .(soakDayID = 0L, sourceTypeID, hourID, soakFraction = d0)])
  for (s in unique(g$sourceTypeID)) {
    d0 <- g[sourceTypeID == s]$d0
    Fv <- numeric(5)
    pre <- function(n) if (!is.null(preset)) preset[soakDayID == n & sourceTypeID == s]$F else numeric()
    for (n in 1:5) {
      p <- pre(n)
      Fv[n] <- if (length(p)) p[1] else if (n == 1) d0[24] else {
        b <- basis[soakDayID == n]$F
        if (!length(b)) NA_real_ else if (b < Fv[n - 1]) Fv[n - 1] else b
      }
    }
    G <- cumprod(Fv)
    G1 <- c(Fv[1], G[1:4])
    D1 <- numeric(5)
    D24 <- numeric(5)
    D24[1:4] <- G[1:4] * (1 - Fv[2:5])
    D24[5] <- G[5]
    D1[1] <- d0[1] - d0[24]
    D1[2:4] <- D24[1:3]
    D1[5] <- G1[5]
    P24 <- ifelse(D1 <= 0, 0, D24 / D1)
    den <- d0[1] - d0[24]
    DF <- if (den <= 0) rep(0, 22) else (d0[2:23] - d0[24]) / den
    for (n in 1:5) {
      Pn <- DF * (1 - P24[n]) + P24[n]
      out[[length(out) + 1L]] <- data.table::data.table(
        soakDayID = n, sourceTypeID = s, hourID = 1:24,
        soakFraction = c(D1[n], D1[n] * Pn, D24[n]))
    }
  }
  data.table::rbindlist(out)
}

# stmyTVVEquations / stmyTVVCoeffs (Section NewTVVYear).
.tvv_coefficients <- function(database, year, pp, source_type_id) {
  tb <- function(n) movesr_table(database, n)
  svp <- tb("samplevehiclepopulation")
  rf <- svp[sourceTypeID %in% source_type_id & modelYearID <= year &
    modelYearID >= year - 40,
  .(rf = sum(stmyFraction)),
  by = .(sourceTypeID, modelYearID, fuelTypeID, regClassID)][rf > 0]
  c0 <- tb("cumtvvcoeffs")[polProcessID %in% pp]
  ppmy <- tb("pollutantprocessmodelyear")[polProcessID %in% pp, .(polProcessID, modelYearID, modelYearGroupID)]
  ac <- tb("agecategory")[, .(ageID, ageGroupID)]
  x <- merge(c0, ppmy, by = c("polProcessID", "modelYearGroupID"), allow.cartesian = TRUE)
  x <- merge(x, ac, by = "ageGroupID", allow.cartesian = TRUE)
  x <- x[modelYearID == year - ageID]
  x <- merge(x, rf, by = c("modelYearID", "regClassID"), allow.cartesian = TRUE)
  x[is.na(tvvEquation) | !nzchar(tvvEquation), tvvEquation := "0"]
  x[is.na(leakEquation) | !nzchar(leakEquation), leakEquation := "0"]
  eq <- x[, .(backPurgeFactor = sum(backPurgeFactor * rf),
    averageCanisterCapacity = sum(averageCanisterCapacity * rf),
    rf = sum(rf), leakFraction = sum(leakFraction * rf),
    leakFractionIM = sum(leakFractionIM * rf),
    tankSize = sum(tankSize * rf), tankFillFraction = sum(tankFillFraction * rf)),
  by = .(sourceTypeID, modelYearID, fuelTypeID, polProcessID, regClassID,
    tvvEquation, leakEquation)]
  co <- eq[, .(backPurgeFactor = sum(backPurgeFactor),
    averageCanisterCapacity = sum(averageCanisterCapacity),
    leakFraction = sum(leakFraction), leakFractionIM = sum(leakFractionIM),
    tankSize = sum(tankSize), tankFillFraction = sum(tankFillFraction)),
  by = .(sourceTypeID, modelYearID, fuelTypeID, polProcessID)]
  list(equations = eq, coeffs = co)
}

# Evaluate a MOVES SQL equation (cumTVVCoeffs.tvvEquation / leakEquation).
# SQL NULL (sqrt of a negative number) is returned as NA.
.tvv_eval <- function(expr, Xn, TVGdaily) {
  e <- gsub("\\bgreatest\\s*\\(", "pmax(", expr, perl = TRUE)
  e <- gsub("\\bleast\\s*\\(", "pmin(", e, perl = TRUE)
  v <- suppressWarnings(eval(parse(text = e),
    list(Xn = Xn, TVGdaily = TVGdaily, pmax = pmax, pmin = pmin,
      sqrt = sqrt), baseenv()))
  v <- rep_len(as.numeric(v), length(Xn))
  v[is.nan(v)] <- NA_real_
  v
}

#' MOVES multiday cold-soak tank vapor venting (hourly TVV)
#'
#' @description Port of the cold-soak part (operating mode 151) of
#' `MultidayTankVaporVentingCalculator.sql`:
#' \enumerate{
#' \item peak hour of the cold-soak tank temperature;
#' \item tank vapor generated between the initial hour I and hour h
#' (`tankSize (1 - fill) A exp(B RVP) (exp(C T_h) - exp(C T_I))`),
#' interpolated between low/high altitude by barometric pressure and weighted
#' between E0 and E10 by tank ethanol;
#' \item canister loading over five soak days
#' (`X_n = (1 - backPurge) min(X_{n-1} + TVG_after_h, capacity) + TVG_1..h`);
#' \item cumulative vented vapor from the cumTVVCoeffs equations and leak
#' equations, hourly increments only while the tank temperature rises;
#' \item weighting by the cold-soak initial-hour fractions (day 1, from the
#' tank temperature model) and multiday soaking fractions (days 2-5);
#' \item 2\%, 1\%, 0.4\%, 0.05\% of the peak-hour value for the 4 hours after the
#' peak.
#' }
#' @param database A `movesr_database`.
#' @param tank Output of \code{\link{movesr_tank_temperature}}.
#' @param tank_gasoline Output of \code{\link{movesr_average_tank_gasoline}}.
#' @param year,day_id Calendar year and day type.
#' @param barometric_pressure County barometric pressure (inHg).
#' @param source_type_id Source types.
#' @param hours Hours present in the MOVES run (the worker's `HourDay`). MOVES
#' only builds venting for initial and current hours inside this set; the
#' multiday terms need hour 1. Use 1:24 for full-day results.
#' @param pol_process_id Vapor-venting polProcessID (112, THC).
#' @return data.table regClassID, ageID, polProcessID, hourDayID, sourceTypeID,
#' fuelTypeID, hourlyTVV, hourlyTVVIM (g per vehicle-hour of soak).
#' @export
movesr_tvv_hourly <- function(database, tank, tank_gasoline, year, day_id = 5L,
                              barometric_pressure, source_type_id, hours = 1:24,
                              pol_process_id = 112L) {
  if (is.character(database)) database <- movesr_database(database)
  tb <- function(n) movesr_table(database, n)
  cst <- tank$cold_soak[order(hourID)]$coldSoakTankTemperature
  peak <- which.max(round(cst, 2) * 100000 + (999 - (1:24)))
  hd <- function(h) h * 10L + day_id
  co <- .tvv_coefficients(database, year, pol_process_id, source_type_id)
  # TVV-3: tank vapor generated.
  ihf <- data.table::copy(tank$cold_soak_initial_hour_fraction)[
    , `:=`(h = hourDayID %/% 10L, ini = initialHourDayID %/% 10L)]
  ihf <- ihf[h %in% hours & ini %in% hours & coldSoakInitialHourFraction > 0 &
    (h != ini | (h == 1L & ini == 1L)) & h <= peak]
  atg <- tank_gasoline
  x <- merge(ihf[, .(sourceTypeID, h, ini)], co$coeffs, by = "sourceTypeID",
    allow.cartesian = TRUE)
  x <- merge(x, atg[, .(fuelTypeID, ETOHVolume, RVP)], by = "fuelTypeID")
  if (!nrow(x)) return(NULL)
  x[, `:=`(t1 = cst[ini], t2 = cst[h])]
  gc <- tb("tankvaporgencoeffs")
  tvg <- function(eth, alt) {
    k <- gc[ethanolLevelID == eth & altitude == alt]
    .f32(data.table::fifelse(x$t1 >= x$t2, 0,
      (x$tankSize * (1 - x$tankFillFraction)) *
        (k$tvgTermA * exp(k$tvgTermB * x$RVP) *
          (exp(k$tvgTermC * x$t2) - exp(k$tvgTermC * x$t1)))))
  }
  bp <- barometric_pressure
  interp <- function(eth) {
    lo <- tvg(eth, "L")
    hi <- tvg(eth, "H")
    .f32(pmax(((bp - 29.069) / (24.087 - 29.069)) * (hi - lo) + lo, 0))
  }
  t0 <- interp(0L)
  t10 <- interp(10L)
  w <- pmin(10.0, x$ETOHVolume) / 10.0
  x[, ew := .f32(t10 * w + t0 * (1.0 - w))]
  # Hourly increment of the cumulative TVG (previous hour row if present).
  key <- c("sourceTypeID", "fuelTypeID", "modelYearID", "polProcessID", "ini")
  prev <- x[, c(key, "h", "ew"), with = FALSE]
  prev[, h := h + 1L]
  data.table::setnames(prev, "ew", "ewPrev")
  x <- merge(x, prev, by = c(key, "h"), all.x = TRUE)
  x[, ew2 := .f32(pmax(0, ew - data.table::fifelse(is.na(ewPrev), 0, ewPrev)))]
  data.table::setorderv(x, c(key, "h"))
  x[, `:=`(sIH = cumsum(ew2), sI24 = sum(ew2)), by = key]
  # tvgSumH24: sum strictly after h; exact sums to avoid cancellation.
  x[, sH24 := rev(cumsum(rev(ew2))) - ew2, by = key]
  one <- x[ini == 1L, c(setdiff(key, "ini"), "h", "ew2"), with = FALSE]
  data.table::setorderv(one, c(setdiff(key, "ini"), "h"))
  one[, s1H := cumsum(ew2), by = c(setdiff(key, "ini"))]
  one[, ew2 := NULL]
  data.table::setnames(one, "h", "h1")
  # tvgSum1H at hour h = cumulative initial-hour-1 TVG up to the last hour <= h.
  x[, hq := h]
  x <- one[x, on = c(setdiff(key, "ini"), "h1<=hq"), mult = "last",
    .(sourceTypeID, fuelTypeID, modelYearID, polProcessID, ini, h,
      backPurgeFactor = i.backPurgeFactor,
      averageCanisterCapacity = i.averageCanisterCapacity,
      sIH = i.sIH, sI24 = i.sI24, sH24 = i.sH24, s1H = x.s1H)]
  x <- x[!is.na(s1H)]
  if (!nrow(x)) return(NULL)
  # TVG by soak day.
  soak <- .tvv_sample_vehicle_soaking(database, day_id)
  days <- sort(unique(soak$soakDayID[soak$soakDayID > 2L]))
  tv <- list()
  x[, `:=`(TVGdaily = .f32(sIH), Xn = sIH)]
  tv[[1]] <- x[, .(soakDayID = 1L, sourceTypeID, fuelTypeID, modelYearID, polProcessID,
    ini, h, TVGdaily, Xn)]
  x[, Xn := (1 - backPurgeFactor) * pmin(sI24, averageCanisterCapacity) + s1H]
  tv[[2]] <- x[, .(soakDayID = 2L, sourceTypeID, fuelTypeID, modelYearID, polProcessID,
    ini, h, TVGdaily, Xn)]
  for (n in days) {
    x[, Xn := (1 - backPurgeFactor) * pmin(Xn + sH24, averageCanisterCapacity) + s1H]
    tv[[length(tv) + 1L]] <- x[, .(soakDayID = n, sourceTypeID, fuelTypeID, modelYearID,
      polProcessID, ini, h, TVGdaily, Xn)]
  }
  tv <- data.table::rbindlist(tv)
  # TVV-5: cumulative tank vapor vented by regulatory class.
  cv <- merge(co$equations, tv, by = c("sourceTypeID", "modelYearID", "fuelTypeID",
    "polProcessID"), allow.cartesian = TRUE)
  cv[, `:=`(T = NA_real_, L = NA_real_)]
  for (e in unique(cv$tvvEquation)) {
    r <- which(cv$tvvEquation == e)
    data.table::set(cv, r, "T", .tvv_eval(e, cv$Xn[r], cv$TVGdaily[r]))
  }
  for (e in unique(cv$leakEquation)) {
    r <- which(cv$leakEquation == e)
    data.table::set(cv, r, "L", .tvv_eval(e, cv$Xn[r], cv$TVGdaily[r]))
  }
  lfim <- data.table::fifelse(is.na(cv$leakFractionIM), cv$leakFraction, cv$leakFractionIM)
  cv[, `:=`(tvv = .f32(rf * pmax(0, (1 - leakFraction) * T + leakFraction * L)),
    tvvIM = .f32(rf * pmax(0, (1 - lfim) * T + lfim * L)))]
  cv[, ageID := year - modelYearID]
  cv <- cv[, .(soakDayID, regClassID, ageID, modelYearID, polProcessID, h, ini,
    sourceTypeID, fuelTypeID, tvv, tvvIM)]
  # TVV-6: hourly increments while the tank temperature rises.
  pv <- cv[, .(soakDayID, regClassID, ageID, polProcessID, ini, sourceTypeID, fuelTypeID,
    h = data.table::fifelse(h == 24L, 1L, h + 1L), tvvP = tvv, tvvIMP = tvvIM)]
  cv <- merge(cv, pv, by = c("soakDayID", "regClassID", "ageID", "polProcessID", "ini",
    "sourceTypeID", "fuelTypeID", "h"), all.x = TRUE)
  ph <- ((cv$h - 2L + 24L) %% 24L) + 1L
  rise <- cst[cv$h] > cst[ph]
  cv[, `:=`(u = .f32(data.table::fifelse(rise, pmax(0, tvv - data.table::fifelse(is.na(tvvP), 0, tvvP)), 0)),
    uIM = .f32(data.table::fifelse(rise, pmax(0, tvvIM - data.table::fifelse(is.na(tvvIMP), 0, tvvIMP)), 0)))]
  # TVV-7: weight by initial-hour (day 1) and multiday soaking fractions.
  f1 <- tank$cold_soak_initial_hour_fraction[, .(soakDayID = 1L, sourceTypeID,
    h = hourDayID %/% 10L,
    ini = initialHourDayID %/% 10L,
    frac = coldSoakInitialHourFraction)]
  fn <- soak[soakDayID > 1L & hourID %in% hours & 1L %in% hours,
    .(soakDayID, sourceTypeID, h = hourID, ini = 1L, frac = soakFraction)]
  fr <- data.table::rbindlist(list(f1[h %in% hours & ini %in% hours], fn))
  cv <- merge(cv, fr, by = c("soakDayID", "sourceTypeID", "h", "ini"))
  ht <- cv[, .(hourlyTVV = .f32(sum(.f32(u * frac), na.rm = TRUE)),
    hourlyTVVIM = .f32(sum(.f32(uIM * frac), na.rm = TRUE))),
  by = .(regClassID, ageID, polProcessID, h, sourceTypeID, fuelTypeID)]
  # Hours after the peak.
  pk <- ht[h == peak]
  post <- list()
  for (k in 1:4) {
    if ((peak + k) %in% hours) {
      f <- c(0.0200, 0.0100, 0.0040, 0.0005)[k]
      post[[k]] <- pk[, .(regClassID, ageID, polProcessID, h = peak + k, sourceTypeID,
        fuelTypeID, hourlyTVV = .f32(hourlyTVV * f),
        hourlyTVVIM = .f32(hourlyTVVIM * f))]
    }
  }
  ht <- data.table::rbindlist(c(list(ht), post), use.names = TRUE)
  ht[, hourDayID := hd(h)]
  ht[, .(regClassID, ageID, polProcessID, hourDayID, sourceTypeID, fuelTypeID,
    hourlyTVV, hourlyTVVIM)]
}
