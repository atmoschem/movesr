# Evaporative calculators (MOVES6): permeation (11), leaks (13) and, in
# evap_tvv.R, vapor venting (12). Inputs that come from MOVES activity and the
# tank temperature model are explicit arguments so that the original and the
# simplified methods share the same calculators.

.evap_bins <- function(ctx, s) {
  year <- ctx$year
  st <- ctx$stmy[sourceTypeID == s & modelYearID <= year & modelYearID >= year - 40]
  d <- ctx$sbd[sourceTypeModelYearID %in% st$sourceTypeModelYearID]
  if (!nrow(d)) return(NULL)
  d <- merge(d, st[, .(sourceTypeModelYearID, sourceTypeID, modelYearID)],
             by = "sourceTypeModelYearID")
  d <- merge(d, ctx$bins[, .(sourceBinID, fuelTypeID, regClassID, modelYearGroupID)],
             by = "sourceBinID")
  d <- merge(d, ctx$ppmy[, .(polProcessID, modelYearID, mg = modelYearGroupID)],
             by = c("polProcessID", "modelYearID"))
  d <- d[mg == modelYearGroupID]
  d[, ageID := year - modelYearID]
  d <- merge(d, ctx$ac[, .(ageID, ageGroupID)], by = "ageID")
  evap_fuels <- ctx$tb("fueltype")[subjectToEvapCalculations == "Y"]$fuelTypeID
  d <- d[fuelTypeID %in% evap_fuels]
  if (!is.null(ctx$fuel_type_id)) d <- d[fuelTypeID %in% ctx$fuel_type_id]
  d
}

# Source-bin weighted evap rate by operating mode, normalised by the fuel's
# activity share (equivalent to MOVES' division by population per fuel).
.evap_rates <- function(ctx, s) {
  d <- .evap_bins(ctx, s)
  if (is.null(d) || !nrow(d)) return(NULL)
  share <- d[, .(S = sum(sourceBinActivityFraction)),
             by = .(sourceTypeID, polProcessID, modelYearID, fuelTypeID)]
  x <- merge(d, ctx$er_age, by = c("sourceBinID", "polProcessID", "ageGroupID"),
             allow.cartesian = TRUE)
  if (!nrow(x)) return(NULL)
  x[is.na(meanBaseRateIM), meanBaseRateIM := meanBaseRate]
  w <- x[, .(MBR = sum(sourceBinActivityFraction * meanBaseRate),
             MBRIM = sum(sourceBinActivityFraction * meanBaseRateIM)),
         by = .(sourceTypeID, polProcessID, modelYearID, fuelTypeID, regClassID,
                ageID, ageGroupID, opModeID)]
  w <- merge(w, share, by = c("sourceTypeID", "polProcessID", "modelYearID",
                              "fuelTypeID"))
  w[, `:=`(MBR = MBR / S, MBRIM = MBRIM / S)]
  w[, pollutantID := polProcessID %/% 100L]
  w[, processID := polProcessID %% 100L]
  w
}

# PC-3: permeation fuel adjustment by ethanol bin and fuel model-year group.
.evap_permeation_fuel <- function(ctx) {
  tb <- ctx$tb
  bins <- tb("etohbin")
  sup <- ctx$supply
  sup <- sup[, .(fuelTypeID, fuelFormulationID, marketShare, ETOHVolume)]
  sup <- sup[bins, on = .(ETOHVolume >= etohThreshLow, ETOHVolume < etohThreshHigh),
             nomatch = NULL, .(fuelTypeID, fuelFormulationID, marketShare,
                               etohThreshID = i.etohThreshID)]
  hc <- tb("hcpermeationcoeff")[polProcessID %in% ctx$pp]
  pmy <- tb("pollutantprocessmodelyear")[polProcessID %in% ctx$pp &
                                           modelYearID <= ctx$year &
                                           modelYearID >= ctx$year - 40]
  x <- merge(sup, hc, by = "etohThreshID", allow.cartesian = TRUE)
  x <- merge(x, pmy[, .(polProcessID, modelYearID, fuelMYGroupID)],
             by = c("polProcessID", "fuelMYGroupID"), allow.cartesian = TRUE)
  x[, .(fuelAdjust = sum(marketShare * (fuelAdjustment + ctx$gpa_fract *
                                           (fuelAdjustmentGPA - fuelAdjustment)))),
    by = .(polProcessID, modelYearID, fuelTypeID)]
}

# Operating-mode fractions for an off-network link:
# 150/151 = soakActivityFraction * (1 - fractionOfOperating), 300 = remainder.
.evap_opmode_fractions <- function(saf, fraction_of_operating = 0) {
  x <- data.table::copy(saf)
  fo <- data.table::as.data.table(fraction_of_operating)
  if (!"fractionOfOperating" %in% names(fo)) {
    x[, fractionOfOperating := as.numeric(fraction_of_operating)]
  } else {
    x <- merge(x, fo, by = intersect(c("sourceTypeID", "hourDayID"), names(fo)),
               all.x = TRUE)
    x[is.na(fractionOfOperating), fractionOfOperating := 0]
  }
  x[, opModeFraction := soakActivityFraction * (1 - fractionOfOperating)]
  op <- x[, .(opModeFraction = max(0, 1 - sum(opModeFraction))),
          by = .(sourceTypeID, hourDayID)]
  op[, opModeID := 300L]
  data.table::rbindlist(list(x[, .(sourceTypeID, hourDayID, opModeID, opModeFraction)],
                             op), use.names = TRUE)
}

#' MOVES evaporative permeation and fuel-leak rates
#'
#' @description Rates per vehicle (MOVES `RatePerVehicle`, off-network road)
#' for processes 11 (permeation) and 13 (liquid leaks), THC.
#' \preformatted{
#' permeation = rate * fuelAdj * sum_mode(A exp(B Ttank[mode]) f[mode]) * SHP/pop
#' leaks      = sum_mode(rate[mode] f[mode]) * SHP/pop          (then I/M)
#' venting    = (sum_bins(SBD * hourlyTVV) f[151] + rate[150] f[150]
#'               + rate[300] tempAdj(T) rvpAdj(T) f[300]) * SHP/pop (then I/M)
#' }
#' `rate` is the source-bin weighted base rate, `f` the evap operating-mode
#' fractions (hot soak 150, cold soak 151, operating 300) and `SHP/pop` the
#' parked source hours per vehicle: MOVES reports evap rates per vehicle over
#' all real days of the day type (5 for weekdays, 2 for weekends).
#' @param database Path or `movesr_database`.
#' @param tank Output of \code{\link{movesr_tank_temperature}} (or the
#' simplified equivalent).
#' @param activity data.table sourceTypeID, hourDayID, ageID, shpPerVehicle:
#' parked source hours per vehicle and real day (SHP / (population *
#' noOfRealDays)), i.e. the parked fraction of the hour.
#' @param day_id Day type: 5 (weekday) or 2 (weekend).
#' @param process_id Any of 11 (permeation), 12 (vapor venting), 13 (leaks).
#' @param fraction_of_operating 0 (off-network only) or a table by
#' sourceTypeID/hourDayID.
#' @param tank_gasoline Optional \code{\link{movesr_average_tank_gasoline}}
#' output (computed from the fuel supply and `tank$temperature` when NULL).
#' @param tvv_hours Hours in the MOVES run for vapor venting (see
#' \code{\link{movesr_tvv_hourly}}); 1:24 gives full-day MOVES results.
#' @param e85_tank_properties E85 properties used for the average tank fuel:
#' `"raw"` (the FuelFormulation as supplied) or `"e10"` (E85 with E10 fuel
#' properties, as left by MOVES' FuelEffectsGenerator; MOVES uses either
#' depending on the run, see PORTING.md). Affects E85 vapor venting only
#' (about 0.4 \%).
#' @param pollutant_id THC (1) and HC species 5 (CH4), 79 (NMHC), 80 (NMOG),
#' 86 (TOG), 87 (VOC).
#' @param by_reg_class Keep regulatory classes (MOVES computes evap by
#' regulatory class; the default aggregates them as in MOVES rate output).
#' @inheritParams movesr_running_rates
#' @return data.table with `ratePerVehicle` (g per vehicle per hour, summed
#' over the day type's real days).
#' @export
movesr_evap_rates <- function(database, tank, activity, year = 2025,
                              county_id = 26161, month_id = 7, day_id = 5L,
                              meteorology, process_id = c(11L, 13L),
                              source_type_id = NULL, fuel_type_id = NULL,
                              fuel_supply = NULL, barometric_pressure = NULL,
                              gpa_fract = NULL, im_coverage = NULL,
                              fraction_of_operating = 0, tank_gasoline = NULL,
                              tvv_hours = 1:24, pollutant_id = 1L,
                              by_reg_class = FALSE,
                              e85_tank_properties = c("raw", "e10")) {
  if (is.character(database)) database <- movesr_database(database)
  out <- list()
  met <- data.table::as.data.table(meteorology)
  hours <- met$hourID
  for (pid in intersect(as.integer(process_id), c(11L, 12L, 13L))) {
    ctx <- .movesr_context(database, pid, year, county_id, month_id, day_id,
                           meteorology, source_type_id, fuel_type_id, 1L,
                           1L, fuel_supply, barometric_pressure, gpa_fract,
                           im_coverage)
    if (pid == 12L) {
      if (is.null(tank$temperature)) stop("tank$temperature (24 hourly values) is required")
      e85 <- match.arg(e85_tank_properties, c("raw", "e10"))
      atg <- if (is.null(tank_gasoline)) {
        movesr_average_tank_gasoline(database, county_id, year, month_id,
                                     tank$temperature,
                                     if (e85 == "e10") ctx$supply else fuel_supply)
      } else data.table::as.data.table(tank_gasoline)
      bp <- barometric_pressure
      if (is.null(bp)) bp <- ctx$tb("county")[countyID == county_id]$barometricPressure[1]
      ht <- movesr_tvv_hourly(database, tank, atg, year, day_id, bp,
                              ctx$source_type_id, tvv_hours, ctx$pp)
      if (is.null(ht)) ht <- data.table::data.table(
        regClassID = integer(), ageID = integer(), polProcessID = integer(),
        hourDayID = integer(), sourceTypeID = integer(), fuelTypeID = integer(),
        hourlyTVV = numeric(), hourlyTVVIM = numeric())
      ht <- ht[hourDayID %in% (hours * 10L + day_id)]
      rvp_adj <- .evap_rvp_adjustment(ctx$tb("evaprvptemperatureadjustment"), atg)
    }
    omf <- .evap_opmode_fractions(tank$soak_activity_fraction, fraction_of_operating)
    omf <- omf[hourDayID %in% (hours * 10L + day_id)]
    fa <- if (pid == 11L) .evap_permeation_fuel(ctx) else NULL
    rcf <- if (pid == 11L) .evap_regclass_fraction(ctx) else NULL
    stmyg <- ctx$tb("sourcetypemodelyeargroup")
    ta <- ctx$ta
    res <- list()
    for (s in ctx$source_type_id) {
      w <- .evap_rates(ctx, s)
      if (is.null(w)) next
      if (pid == 11L) {
        w <- merge(w, fa, by = c("polProcessID", "modelYearID", "fuelTypeID"))
        w <- merge(w, ctx$ppmy[, .(polProcessID, modelYearID, modelYearGroupID)],
                   by = c("polProcessID", "modelYearID"))
        w <- merge(w, stmyg[sourceTypeID == s, .(sourceTypeID, modelYearGroupID,
                                                 tankTemperatureGroupID)],
                   by = c("sourceTypeID", "modelYearGroupID"))
        tt <- ta[, .(polProcessID, fuelTypeID, minModelYearID, maxModelYearID,
                     tA = tempAdjustTermA, tB = tempAdjustTermB)]
        w <- merge(w[, !"opModeID"], tt, by = c("polProcessID", "fuelTypeID"),
                   allow.cartesian = TRUE)
        w <- w[modelYearID >= minModelYearID & modelYearID <= maxModelYearID]
        att <- tank$average_tank_temperature
        x <- merge(w, att, by = "tankTemperatureGroupID", allow.cartesian = TRUE)
        x <- merge(x, omf[sourceTypeID == s], by = c("sourceTypeID", "hourDayID",
                                                     "opModeID"))
        x[, wta := tA * exp(tB * averageTankTemperature) * opModeFraction]
        x <- x[, .(rate = MBR[1] * fuelAdjust[1] * sum(wta),
                   rateIM = MBRIM[1] * fuelAdjust[1] * sum(wta)),
               by = .(sourceTypeID, polProcessID, pollutantID, processID,
                      modelYearID, fuelTypeID, ageID, hourDayID)]
        # Bins carry no regulatory class: split by RegClassSourceTypeFraction.
        x <- merge(x, rcf, by = c("sourceTypeID", "modelYearID", "fuelTypeID"),
                   allow.cartesian = TRUE)
        x[, `:=`(rate = rate * regClassFraction, rateIM = rateIM * regClassFraction,
                 regClassFraction = NULL)]
      } else if (pid == 12L) {
        x <- .evap_tvv_rates(ctx, s, w, ht, rvp_adj, omf[sourceTypeID == s], met)
      } else {
        x <- merge(w, omf[sourceTypeID == s], by = c("sourceTypeID", "opModeID"),
                   allow.cartesian = TRUE)
        x <- x[, .(rate = sum(MBR * opModeFraction), rateIM = sum(MBRIM * opModeFraction)),
               by = .(sourceTypeID, polProcessID, pollutantID, processID,
                      modelYearID, fuelTypeID, regClassID, ageID, hourDayID)]
      }
      res[[length(res) + 1L]] <- x
    }
    x <- data.table::rbindlist(res)
    x <- merge(x, activity, by = c("sourceTypeID", "hourDayID", "ageID"))
    im <- ctx$im
    if (!is.null(im) && nrow(im)) {
      x <- merge(x, im, by = c("polProcessID", "modelYearID", "sourceTypeID",
                               "fuelTypeID"), all.x = TRUE)
    } else x[, IMAdjustFract := NA_real_]
    x[is.na(IMAdjustFract), IMAdjustFract := 0]
    x[, ratePerVehicle := pmax((1 - IMAdjustFract) * rate + IMAdjustFract * rateIM, 0) *
        shpPerVehicle]
    x <- x[, .(polProcessID, pollutantID, processID, sourceTypeID, regClassID, fuelTypeID,
               modelYearID, ageID, hourDayID, ratePerVehicle)]
    x <- .evap_speciate(ctx, x, pollutant_id)
    if (!by_reg_class) {
      x <- x[, .(ratePerVehicle = sum(ratePerVehicle)),
             by = .(pollutantID, processID, sourceTypeID, fuelTypeID, modelYearID,
                    hourDayID)]
      x[, regClassID := 0L]
    }
    x[, `:=`(hourID = hourDayID %/% 10L, dayID = day_id, yearID = year,
             monthID = month_id)]
    x <- merge(x, met[, .(hourID, temperature, relHumidity)], by = "hourID")
    out[[length(out) + 1L]] <- x[, .(yearID, monthID, dayID, hourID, sourceTypeID,
                                     regClassID, fuelTypeID, modelYearID,
                                     pollutantID, processID, temperature,
                                     relHumidity, ratePerVehicle)]
  }
  data.table::rbindlist(out)
}

# Interpolated RVP temperature-adjustment terms for each tank fuel (TVV-8).
.evap_rvp_adjustment <- function(adj, atg) {
  adj <- data.table::as.data.table(adj)[processID == 12L]
  out <- list()
  for (k in seq_len(nrow(atg))) {
    a <- adj[fuelTypeID == atg$fuelTypeID[k]][order(RVP)]
    if (!nrow(a)) next
    a <- data.table::rbindlist(list(a[1][, RVP := -1], a, a[.N][, RVP := 1000]))
    r <- atg$RVP[k]
    lo <- a[RVP <= r][.N]; hi <- a[RVP > r][1]
    f <- (r - lo$RVP) / (hi$RVP - lo$RVP)
    term <- function(n) lo[[n]] + (hi[[n]] - lo[[n]]) * f
    out[[length(out) + 1L]] <- data.table::data.table(
      fuelTypeID = atg$fuelTypeID[k], a3 = term("adjustTerm3"), a2 = term("adjustTerm2"),
      a1 = term("adjustTerm1"), a0 = term("adjustConstant"))
  }
  data.table::rbindlist(out)
}

# Vapor venting for one source type: cold soak (151) from hourly TVV, hot soak
# (150) and operating (300) from base rates, operating adjusted for ambient
# temperature and tank RVP (gasoline and E85 only).
.evap_tvv_rates <- function(ctx, s, w, ht, rvp_adj, omf, met) {
  day_id <- ctx$day_id
  tmp <- met[, .(hourDayID = hourID * 10L + day_id, temperature)]
  eta <- ctx$tb("evaptemperatureadjustment")[processID == 12L]
  hs <- w[opModeID %in% c(150L, 300L)]
  n <- nrow(hs)
  hs <- hs[rep(seq_len(n), each = nrow(tmp))]
  hs[, c("hourDayID", "temperature") := tmp[rep(seq_len(nrow(tmp)), times = n)]]
  hs <- merge(hs, rvp_adj, by = "fuelTypeID", all.x = TRUE)
  hs[, adj := 1]
  t40 <- pmax(hs$temperature, 40)
  op <- hs$opModeID == 300L & hs$fuelTypeID %in% c(1L, 5L)
  ta <- eta$tempAdjustTerm3 * t40^3 + eta$tempAdjustTerm2 * t40^2 +
    eta$tempAdjustTerm1 * t40 + eta$tempAdjustConstant
  T <- hs$temperature
  ra <- data.table::fifelse(T >= 40, hs$a3 * T^3 + hs$a2 * T^2 + hs$a1 * T + hs$a0, 1)
  hs[op, adj := ta[op] * ra[op]]
  hs <- hs[, .(sourceTypeID, polProcessID, pollutantID, processID, modelYearID,
               fuelTypeID, regClassID, ageID, opModeID, hourDayID, MBR = MBR * adj,
               MBRIM = MBRIM * adj)]
  d <- .evap_bins(ctx, s)
  cs <- NULL
  if (!is.null(d) && nrow(d) && nrow(ht)) {
    S <- d[, .(S = sum(sourceBinActivityFraction)),
           by = .(sourceTypeID, polProcessID, modelYearID, fuelTypeID)]
    cs <- merge(d[, .(sourceTypeID, polProcessID, modelYearID, ageID, fuelTypeID,
                      regClassID, sourceBinActivityFraction)],
                ht[sourceTypeID == s], by = c("sourceTypeID", "polProcessID", "ageID",
                                              "fuelTypeID", "regClassID"),
                allow.cartesian = TRUE)
    cs <- cs[, .(MBR = sum(sourceBinActivityFraction * hourlyTVV),
                 MBRIM = sum(sourceBinActivityFraction * hourlyTVVIM)),
             by = .(sourceTypeID, polProcessID, modelYearID, fuelTypeID, regClassID, ageID,
                    hourDayID)]
    cs <- merge(cs, S, by = c("sourceTypeID", "polProcessID", "modelYearID", "fuelTypeID"))
    cs[, `:=`(MBR = MBR / S, MBRIM = MBRIM / S, S = NULL, opModeID = 151L,
              pollutantID = polProcessID %/% 100L, processID = polProcessID %% 100L)]
  }
  x <- data.table::rbindlist(list(hs, cs), use.names = TRUE)
  x <- merge(x, omf, by = c("sourceTypeID", "hourDayID", "opModeID"))
  x[, .(rate = sum(MBR * opModeFraction), rateIM = sum(MBRIM * opModeFraction)),
    by = .(sourceTypeID, polProcessID, pollutantID, processID, modelYearID,
           fuelTypeID, regClassID, ageID, hourDayID)]
}

# RegClassSourceTypeFraction (UpdateExecution.sql): regulatory-class share of a
# source type, used fuel type and model year, weighted by fuel usage.
.evap_regclass_fraction <- function(ctx) {
  tb <- ctx$tb
  svp <- tb("samplevehiclepopulation")[sourceTypeID %in% ctx$source_type_id &
                                         modelYearID <= ctx$year &
                                         modelYearID >= ctx$year - 40]
  fy <- tb("year")[yearID == ctx$year]$fuelYearID[1]
  fuf <- tb("fuelusagefraction")[countyID == ctx$county_id & fuelYearID == fy]
  if (!nrow(fuf)) {
    f <- unique(svp$fuelTypeID)
    fuf <- data.table::data.table(sourceBinFuelTypeID = f, fuelSupplyFuelTypeID = f,
                                  usageFraction = 1)
  }
  x <- merge(svp[, .(sourceTypeID, modelYearID, fuelTypeID, regClassID, stmyFraction)],
             fuf[, .(fuelTypeID = sourceBinFuelTypeID, used = fuelSupplyFuelTypeID,
                     usageFraction)], by = "fuelTypeID", allow.cartesian = TRUE)
  x <- x[, .(v = sum(usageFraction * stmyFraction)),
         by = .(sourceTypeID, modelYearID, fuelTypeID = used, regClassID)]
  x[, tot := sum(v), by = .(sourceTypeID, modelYearID, fuelTypeID)]
  x <- x[tot != 0, .(sourceTypeID, modelYearID, fuelTypeID, regClassID,
                     regClassFraction = v / tot)]
  x[regClassFraction != 0]
}

# HCSpeciationCalculator for evaporative THC: the THC of each fuel type is split
# over fuel formulations by market share (mwo reader), then CH4/NMHC/NMOG/TOG/VOC
# by regulatory class, fuel subtype and model year.
.evap_speciate <- function(ctx, x, pollutant_id) {
  want <- intersect(as.integer(pollutant_id), c(5L, 79L, 80L, 86L, 87L))
  out <- if (1L %in% pollutant_id) list(x) else list()
  if (!length(want) || !nrow(x)) return(data.table::rbindlist(out))
  c2 <- ctx
  c2$ratios <- data.table::data.table()
  c2$alt_ratios <- NULL
  hf <- .movesr_hc_factors(c2, unique(x[, .(fuelTypeID, polProcessID, sourceTypeID,
                                             modelYearID, ageID, regClassID, processID)]))
  y <- merge(x, hf, by = c("fuelTypeID", "polProcessID", "sourceTypeID", "modelYearID",
                           "ageID", "regClassID", "processID"))
  for (pol in want) {
    col <- paste0("S", pol)
    z <- y[!is.na(get(col))]
    z[, `:=`(ratePerVehicle = ratePerVehicle * get(col), pollutantID = pol,
             polProcessID = pol * 100L + processID)]
    out[[length(out) + 1L]] <- z[, names(x), with = FALSE]
  }
  data.table::rbindlist(out, use.names = TRUE)
}
