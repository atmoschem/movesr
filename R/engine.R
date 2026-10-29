#' EV fleet-average adjustment factors
#'
#' @description Port of `BaseRateGenerator` step 010 (EV sales). National EV
#' sales fractions by model year and fleet-average group are computed from
#' `SourceTypeYear`, `SourceTypeAgeDistribution` (age 0) and
#' `SampleVehiclePopulation`:
#' \preformatted{
#' evFraction = sum(pop * ageFraction * stmyFraction | fuel 9) /
#'              sum(pop * ageFraction * stmyFraction)
#' factor     = min(1 / (1 - ev*m / ((1 - ev) + ev*m)), adjustmentCap)
#' }
#' The factor multiplies non-electric rates of affected pollutant/processes.
#' @param database A `movesr_database`.
#' @param year Calendar year.
#' @return data.table polProcessID, modelYearID, regClassID, evFactor.
#' @export
movesr_ev_adjustment <- function(database, year) {
  sty <- movesr_table(database, "sourcetypeyear")
  stad <- movesr_table(database, "sourcetypeagedistribution")[ageID == 0L]
  svp <- movesr_table(database, "samplevehiclepopulation")
  rc <- movesr_table(database, "regulatoryclass")
  faa <- movesr_table(database, "fleetavgadjustment")
  x <- merge(sty[, .(sourceTypeID, yearID, sourceTypePopulation)],
             stad[, .(sourceTypeID, yearID, ageFraction)],
             by = c("sourceTypeID", "yearID"),
        allow.cartesian = TRUE)
  x <- merge(x, svp[, .(sourceTypeID, modelYearID, fuelTypeID, regClassID,
                        stmyFraction)],
             by.x = c("sourceTypeID", "yearID"),
             by.y = c("sourceTypeID", "modelYearID"),
        allow.cartesian = TRUE)
  data.table::setnames(x, "yearID", "modelYearID")
  x <- x[modelYearID <= year & modelYearID >= year - 40]
  x <- merge(x, rc[, .(regClassID, fleetAvgGroupID)], by = "regClassID",
        allow.cartesian = TRUE)
  x[, w := sourceTypePopulation * ageFraction * stmyFraction]
  ev <- x[, .(evFraction = sum(w[fuelTypeID == 9L]) / sum(w)),
          by = .(modelYearID, fleetAvgGroupID)]
  ev <- ev[is.finite(evFraction)]
  y <- merge(faa, ev, by = "fleetAvgGroupID", allow.cartesian = TRUE)
  y <- y[modelYearID >= beginModelYearID & modelYearID <= endModelYearID &
           evFraction != 1]
  y[, cap := data.table::fifelse(is.na(adjustmentCap), 1e99, adjustmentCap)]
  y[, evFactor := pmin(1 / (1 - (evFraction * evMultiplier) /
                              ((1 - evFraction) + evFraction * evMultiplier)),
                       cap)]
  y <- merge(y, rc[, .(regClassID, fleetAvgGroupID)], by = "fleetAvgGroupID",
             allow.cartesian = TRUE)
  unique(y[, .(polProcessID, modelYearID, regClassID, evFactor)],
         by = c("polProcessID", "modelYearID", "regClassID"))
}

#' I/M adjustment fractions
#'
#' @description Port of the `IMCoverageMergedUngrouped` step of
#' `BaseRateCalculator.sql`:
#' \preformatted{
#' IMAdjustFract = sum(IMFactor * complianceFactor * 0.01)
#' }
#' by pollutant/process, model year, source type and fuel. Rates are then
#' blended as `(1 - f) * rate + f * rateIM`.
#' @param database A `movesr_database`.
#' @param county_id County ID.
#' @param year Calendar year.
#' @param pol_process_id polProcessIDs.
#' @param im_coverage Optional user `IMCoverage` table (same columns as
#' MOVES). If `NULL`, the default database table is used.
#' @return data.table polProcessID, modelYearID, sourceTypeID, fuelTypeID,
#' IMAdjustFract. Empty when there is no I/M program.
#' @export
movesr_im_fraction <- function(database, county_id, year, pol_process_id,
                               im_coverage = NULL) {
  imc <- if (is.null(im_coverage)) movesr_table(database, "imcoverage") else
    data.table::as.data.table(im_coverage)
  imc <- imc[countyID == county_id & yearID == year & useIMyn == "Y" &
               polProcessID %in% pol_process_id]
  empty <- data.table::data.table(polProcessID = integer(),
                                  modelYearID = integer(),
                                  sourceTypeID = integer(),
                                  fuelTypeID = integer(),
                                  IMAdjustFract = numeric())
  if (!nrow(imc)) return(empty)
  imf <- movesr_table(database, "imfactor")[polProcessID %in% pol_process_id]
  ppmy <- movesr_table(database, "pollutantprocessmodelyear")
  ac <- movesr_table(database, "agecategory")
  ppmy <- ppmy[polProcessID %in% pol_process_id &
                 modelYearID <= year & modelYearID >= year - 40]
  ppmy[, ageID := year - modelYearID]
  ppmy <- merge(ppmy, ac[, .(ageID, ageGroupID)], by = "ageID",
        allow.cartesian = TRUE)
  x <- merge(ppmy[, .(polProcessID, modelYearID, IMModelYearGroupID, ageGroupID)],
             imf, by = c("polProcessID", "IMModelYearGroupID", "ageGroupID"),
             allow.cartesian = TRUE)
  x <- merge(x, imc[, .(polProcessID, inspectFreq, testStandardsID,
                        sourceTypeID, fuelTypeID, begModelYearID,
                        endModelYearID, complianceFactor)],
             by = c("polProcessID", "inspectFreq", "testStandardsID",
                    "sourceTypeID", "fuelTypeID"), allow.cartesian = TRUE)
  x <- x[begModelYearID <= modelYearID & endModelYearID >= modelYearID]
  if (!nrow(x)) return(empty)
  x[, .(IMAdjustFract = sum(IMFactor * complianceFactor * 0.01)),
    by = .(polProcessID, modelYearID, sourceTypeID, fuelTypeID)]
}

.movesr_temp_factor <- function(process_id, pollutant_id, fuel_type_id,
                                source_type_id, k, temperature, heat_index,
                                a, b) {
  a[is.na(a)] <- 0
  b[is.na(b)] <- 0
  out <- 1 + (temperature - 75) * (a + (temperature - 75) * b)
  pm <- process_id %in% c(1L, 2L) & pollutant_id %in% c(112L, 118L)
  out[pm] <- ifelse(temperature <= 72, exp(a[pm] * (72 - temperature)), 1)
  ev <- process_id == 1L & fuel_type_id == 9L & pollutant_id == 91L
  if (any(ev)) {
    adj <- (temperature - 72) * (a[ev] + b[ev] * (temperature - 72))
    adj[adj < 0] <- 0
    adj[source_type_id[ev] < 40 & heat_index > 67] <- 0
    out[ev] <- 1 + adj
  }
  nox <- process_id %in% c(1L, 90L, 91L) & pollutant_id == 3L
  if (any(nox)) {
    diesel <- fuel_type_id[nox] == 2L
    t_adj <- ifelse(diesel,
                    ifelse(temperature > 77, 0, (77 - temperature) * a[nox]),
                    (temperature - 75) * (a[nox] + (temperature - 75) * b[nox]))
    out[nox] <- (1 + t_adj) * k[nox]
  }
  out
}

.movesr_need <- function(x, cols, name) {
  missing <- setdiff(cols, names(x))
  if (length(missing)) stop(name, " is missing: ", paste(missing, collapse = ", "))
}

.movesr_start_temp <- function(sta_a, sta_b, sta_c, is_log, pol_process_id,
                               temperature, weight, rate) {
  ok <- !is.na(sta_a)
  a <- ifelse(ok, sta_a, 0)
  b <- ifelse(ok, sta_b, 0)
  cc <- ifelse(ok, sta_c, 0)
  tl <- min(temperature, 75) - 75
  add <- ifelse(is_log & ok,
                weight * (b * exp(a * tl) + cc),
                weight * tl * (a + tl * (b + tl * cc)))
  pm <- pol_process_id %in% c(11202L, 11802L) & ok
  out <- rate + ifelse(ok, add, 0)
  out[pm] <- rate[pm] * b[pm] * exp(a[pm] * (72 - min(temperature, 72))) + cc[pm]
  out
}

.movesr_context <- function(database, process_id, year, county_id, month_id,
                            day_id, meteorology, source_type_id, fuel_type_id,
                            pollutant_id, road_type_id, fuel_supply,
                            barometric_pressure, gpa_fract, im_coverage) {
  if (is.character(database)) database <- movesr_database(database)
  tb <- function(name) movesr_table(database, name)
  met <- data.table::as.data.table(meteorology)
  .movesr_need(met, c("hourID", "temperature", "relHumidity"), "meteorology")
  county <- tb("county")[countyID == county_id]
  if (!nrow(county)) stop("Unknown county ", county_id)
  if (is.null(barometric_pressure)) {
    barometric_pressure <- county$barometricPressure[1]
    if (is.na(barometric_pressure) || barometric_pressure <= 0) {
      barometric_pressure <- if (identical(county$altitude[1], "H")) 24.59 else 28.94
    }
  }
  if (is.null(gpa_fract)) gpa_fract <- county$GPAFract[1]
  if (is.na(gpa_fract)) gpa_fract <- 0
  met <- cbind(met[, .(hourID)],
               movesr_meteorology(met$temperature, met$relHumidity,
                                  barometric_pressure))
  plan <- .movesr_pollutant_plan(pollutant_id)
  ppa <- tb("pollutantprocessassoc")
  pp <- ppa[processID == process_id & pollutantID %in% plan$base]$polProcessID
  if (!length(pp)) stop("No pollutant/process ", process_id, " for the pollutants")
  if (is.null(source_type_id)) {
    source_type_id <- sort(unique(tb("sourceusetype")$sourceTypeID))
  }
  month_group <- tb("monthofanyyear")[monthID == month_id]$monthGroupID[1]

  # Source bins converted from equipped to used fuels (FuelUsageFraction).
  svp <- tb("samplevehiclepopulation")
  ppmy <- tb("pollutantprocessmodelyear")[polProcessID %in% pp]
  myg <- tb("modelyeargroup")
  sb <- movesr_sourcebins(svp, ppmy, myg, year = year,
                          source_type_id = source_type_id,
                          process_id = process_id,
                          source_type_pol_process = tb("sourcetypepolprocess"))
  bins <- merge(data.table::copy(sb$source_bin),
                myg[, .(modelYearGroupID, shortModYrGroupID)],
                by = "modelYearGroupID")
  fuel_year <- tb("year")[yearID == year]$fuelYearID[1]
  fuf <- tb("fuelusagefraction")[countyID == county_id & fuelYearID == fuel_year]
  if (!nrow(fuf)) {
    fuf <- data.table::data.table(modelYearGroupID = 0L,
                                  sourceBinFuelTypeID = unique(bins$fuelTypeID),
                                  fuelSupplyFuelTypeID = unique(bins$fuelTypeID),
                                  usageFraction = 1)
  }
  use <- merge(bins, fuf[, .(sourceBinFuelTypeID, fuelSupplyFuelTypeID,
                             fmyg = modelYearGroupID, usageFraction)],
               by.x = "fuelTypeID", by.y = "sourceBinFuelTypeID",
               allow.cartesian = TRUE)
  use <- use[fmyg == 0L | fmyg == modelYearGroupID]
  use[, usedSourceBinID := .sourcebin_id(fuelSupplyFuelTypeID, engTechID,
                                         regClassID, shortModYrGroupID)]
  use <- use[usedSourceBinID %in% bins$sourceBinID,
             .(sourceBinID, usedSourceBinID, usageFraction)]
  sbd <- merge(sb$distribution, use, by = "sourceBinID", allow.cartesian = TRUE)
  sbd <- sbd[, .(sourceBinActivityFraction =
                   sum(usageFraction * sourceBinActivityFraction)),
             by = .(sourceTypeModelYearID, polProcessID,
                    sourceBinID = usedSourceBinID)]

  er_age <- tb("emissionratebyage")[polProcessID %in% pp]
  er_age[, sourceBinID := as.character(sourceBinID)]
  er_flat <- tb("emissionrate")[polProcessID %in% pp]
  er_flat[, sourceBinID := as.character(sourceBinID)]
  supply <- if (is.null(fuel_supply)) {
    movesr_fuel_supply(database, county_id, year, month_id)
  } else {
    .movesr_fuel_nulls(fuel_supply)
  }
  gfre <- tb("generalfuelratioexpression")
  extra <- if (any(plan$requested %in% c(100L, 110L, 111L, 115L, 118L, 119L))) {
    12000L + process_id
  } else integer()
  ratios <- movesr_fuel_ratios(gfre, supply, pol_process_id = c(pp, extra))
  # E85 alternate THC (MOVES pollutant 10001): THC expressions with altRVP,
  # E70/E85 subtypes, model years >= 2001.
  alt_supply <- supply[fuelSubtypeID %in% c(51L, 52L)]
  alt_ratios <- NULL
  if (nrow(alt_supply) && any(pp %/% 100L == 1L)) {
    if ("altRVP" %in% names(alt_supply)) alt_supply[, RVP := altRVP]
    ax <- gfre[fuelTypeID == 5L & polProcessID %in% pp & polProcessID %/% 100L == 1L &
                 polProcessID %% 100L %in% c(1L, 2L) & maxModelYearID >= 2001L]
    ax[, minModelYearID := pmax(2001L, minModelYearID)]
    alt_ratios <- movesr_fuel_ratios(ax, alt_supply)
  }
  # CriteriaRatio: general expressions first, then the Complex/Predictive
  # Model for model years without an expression (MOVES "insert ignore").
  ratios <- data.table::rbindlist(list(
    ratios, movesr_complex_ratios(database, supply, year, pp, source_type_id)),
    use.names = TRUE)
  stmy <- tb("sourcetypemodelyear")
  list(database = database, tb = tb, process_id = process_id, year = year,
       county_id = county_id, month_id = month_id, day_id = day_id,
       month_group = month_group, met = met, pp = pp,
       source_type_id = source_type_id, fuel_type_id = fuel_type_id,
       road_type_id = road_type_id, gpa_fract = gpa_fract,
       ppmy = ppmy, bins = bins, sbd = sbd, er_age = er_age,
       er_flat = er_flat, stmy = stmy, ac = tb("agecategory"),
       fac = if (process_id == 2L) tb("fullacadjustment")[0] else
         tb("fullacadjustment")[polProcessID %in% pp],
       ev = movesr_ev_adjustment(database, year)[polProcessID %in% pp],
       era = tb("emissionrateadjustment")[polProcessID %in% pp],
       eff = tb("evefficiency")[polProcessID %in% pp],
       ta = tb("temperatureadjustment")[polProcessID %in% pp],
       nha = tb("noxhumidityadjust"),
       im = if (isFALSE(im_coverage)) NULL else
         movesr_im_fraction(database, county_id, year, pp, im_coverage),
       acf = movesr_ac_factor(met[, .(hourID, heatIndex)], tb("monthgrouphour"),
                              stmy, tb("sourcetypeage"), year, month_group),
       supply = supply, ratios = ratios, alt_supply = alt_supply,
       alt_ratios = alt_ratios, plan = plan)
}

# BaseRateGenerator SBWeightedEmissionRate(ByAge) for one source type.
.movesr_sbweighted <- function(ctx, s) {
  year <- ctx$year
  st <- ctx$stmy[sourceTypeID == s & modelYearID <= year & modelYearID >= year - 40]
  d <- ctx$sbd[sourceTypeModelYearID %in% st$sourceTypeModelYearID]
  if (!nrow(d)) return(NULL)
  d <- merge(d, st[, .(sourceTypeModelYearID, sourceTypeID, modelYearID)],
             by = "sourceTypeModelYearID")
  d <- merge(d, ctx$bins[, .(sourceBinID, fuelTypeID, regClassID,
                             modelYearGroupID)], by = "sourceBinID")
  d <- merge(d, ctx$ppmy[, .(polProcessID, modelYearID, mg = modelYearGroupID)],
             by = c("polProcessID", "modelYearID"))
  d <- d[mg == modelYearGroupID]
  d[, ageID := year - modelYearID]
  d <- merge(d, ctx$ac[, .(ageID, ageGroupID)], by = "ageID")
  if (!is.null(ctx$fuel_type_id)) d <- d[fuelTypeID %in% ctx$fuel_type_id]
  if (!nrow(d)) return(NULL)
  smfr <- d[, .(sbdTotal = sum(sourceBinActivityFraction)),
            by = .(polProcessID, modelYearID, fuelTypeID, regClassID)]
  keys <- c("sourceTypeID", "polProcessID", "modelYearID", "fuelTypeID",
            "opModeID", "ageGroupID", "regClassID")
  a <- merge(d, ctx$er_age, by = c("sourceBinID", "polProcessID", "ageGroupID"),
             allow.cartesian = TRUE)
  f <- merge(d, ctx$er_flat, by = c("sourceBinID", "polProcessID"),
             allow.cartesian = TRUE)
  x <- data.table::rbindlist(list(a, f), use.names = TRUE, fill = TRUE)
  if (!nrow(x)) return(NULL)
  x <- merge(x, ctx$fac[sourceTypeID == s, .(polProcessID, opModeID,
                                             fullACAdjustment)],
             by = c("polProcessID", "opModeID"), all.x = TRUE,
        allow.cartesian = TRUE)
  x[is.na(fullACAdjustment), fullACAdjustment := 1]
  x[is.na(meanBaseRateIM), meanBaseRateIM := meanBaseRate]
  w <- x[, .(S = sum(sourceBinActivityFraction),
             MBR = sum(sourceBinActivityFraction * meanBaseRate),
             MBRIM = sum(sourceBinActivityFraction * meanBaseRateIM),
             AC = sum(sourceBinActivityFraction * meanBaseRate *
                        (fullACAdjustment - 1)),
             IMAC = sum(sourceBinActivityFraction * meanBaseRateIM *
                          (fullACAdjustment - 1))),
         by = keys][S > 0]
  w[, `:=`(MBR = MBR / S, MBRIM = MBRIM / S, AC = AC / S, IMAC = IMAC / S)]
  w <- merge(w, ctx$ev, by = c("polProcessID", "modelYearID", "regClassID"),
             all.x = TRUE,
        allow.cartesian = TRUE)
  w[is.na(evFactor) | fuelTypeID == 9L, evFactor := 1]
  w[, `:=`(MBR = MBR * evFactor, MBRIM = MBRIM * evFactor,
           AC = AC * evFactor, IMAC = IMAC * evFactor)]
  list(w = w, smfr = smfr)
}

# BaseRateCalculator factors that do not depend on the hour.
.movesr_static_factors <- function(ctx, y, s) {
  year <- ctx$year
  y[, `:=`(pollutantID = polProcessID %/% 100L,
           processID = polProcessID %% 100L,
           ageID = year - modelYearID)]
  fk <- unique(y[, .(fuelTypeID, polProcessID, sourceTypeID, modelYearID, ageID)])
  fa <- movesr_fuel_adjustment(ctx$supply, ctx$ratios, fk, ctx$gpa_fract)
  y <- merge(y, fa, by = c("fuelTypeID", "polProcessID", "sourceTypeID",
                           "modelYearID", "ageID"), all.x = TRUE,
        allow.cartesian = TRUE)
  # MOVES produces no rate for a fuel type absent from the fuel supply.
  y <- y[!is.na(fuelFactor) & marketShare > 0]
  if (!nrow(y)) return(y)
  y[, era := 1]
  era <- ctx$era[sourceTypeID == s]
  if (nrow(era)) {
    m <- merge(y[, .(i = .I, polProcessID, regClassID, fuelTypeID, modelYearID)],
               era, by = c("polProcessID", "regClassID", "fuelTypeID"),
               allow.cartesian = TRUE)
    m <- m[modelYearID >= beginModelYearID & modelYearID <= endModelYearID]
    m <- m[!duplicated(i)]
    y[m$i, era := m$emissionRateAdjustment]
  }
  y[, eff := 1]
  eff <- ctx$eff[sourceTypeID == s]
  if (nrow(eff) && any(y$fuelTypeID == 9L)) {
    m <- merge(y[, .(i = .I, polProcessID, regClassID, modelYearID,
                     ageGroupID, fuelTypeID)][fuelTypeID == 9L],
               eff, by = c("polProcessID", "regClassID", "ageGroupID"),
               allow.cartesian = TRUE)
    m <- m[modelYearID >= beginModelYearID & modelYearID <= endModelYearID &
             batteryEfficiency * chargingEfficiency != 0]
    y[m$i, eff := m$batteryEfficiency * m$chargingEfficiency]
  }
  y[, `:=`(taA = NA_real_, taB = NA_real_)]
  ta <- ctx$ta
  for (zero in c(FALSE, TRUE)) {
    tt <- if (zero) ta[regClassID == 0L] else ta
    if (!nrow(tt)) next
    lhs <- y[, .(i = .I, polProcessID, fuelTypeID, regClassID, modelYearID,
                 todo = is.na(taA))][todo == TRUE]
    if (zero) lhs[, regClassID := 0L]
    m <- merge(lhs, tt, by = c("polProcessID", "fuelTypeID", "regClassID"),
        allow.cartesian = TRUE)
    m <- m[modelYearID >= minModelYearID & modelYearID <= maxModelYearID]
    m <- m[!duplicated(i)]
    y[m$i, `:=`(taA = m$tempAdjustTermA, taB = m$tempAdjustTermB)]
  }
  im <- ctx$im
  if (!is.null(im) && nrow(im)) {
    y <- merge(y, im, by = c("polProcessID", "modelYearID", "sourceTypeID",
                             "fuelTypeID"), all.x = TRUE,
        allow.cartesian = TRUE)
  } else {
    y[, IMAdjustFract := NA_real_]
  }
  y[is.na(IMAdjustFract), IMAdjustFract := 0]
  y
}

.movesr_finish <- function(ctx, z, rate_name, keep_regclass, keep_components,
                           idcols, energy_units = "J") {
  z[, `:=`(yearID = ctx$year, monthID = ctx$month_id, dayID = ctx$day_id)]
  # MOVES weights rates by activity; ages with no population give 0.
  stad <- ctx$tb("sourcetypeagedistribution")[yearID == ctx$year & ageFraction == 0]
  if (nrow(stad)) {
    zero <- stad[, .(sourceTypeID, modelYearID = ctx$year - ageID)]
    z[zero, on = .(sourceTypeID, modelYearID), rate := 0]
  }
  # Energy base rates are KJ; MOVES reports the RunSpec energy unit.
  scale <- c(J = 1000, KJ = 1, MJ = 0.001)[energy_units]
  if (is.na(scale)) stop("energy_units must be J, KJ or MJ")
  z[pollutantID == 91L, rate := rate * scale]
  if (keep_regclass) {
    res <- z[, intersect(c(idcols, "regClassID", "rate", "sbdTotal",
                           if (keep_components) c("fuelFactor", "base")),
                         names(z)), with = FALSE]
  } else {
    res <- z[, .(rate = sum(rate * sbdTotal) / sum(sbdTotal)), by = idcols]
    res[, regClassID := 0L]
  }
  data.table::setnames(res, "rate", rate_name)
  data.table::setcolorder(res, c(idcols[1:5], "regClassID", idcols[-(1:5)]))
  res[]
}

#' Generate MOVES running-exhaust emission rates
#'
#' @description Full running-exhaust (process 1) rate engine. Reproduces the
#' MOVES Rates-mode chain:
#' \enumerate{
#' \item source bins and source-bin distributions (`SourceBinDistributionGenerator`),
#' converted from equipped to used fuels with `FuelUsageFraction`;
#' \item operating-mode distributions by source type, regulatory class and
#' model-year physics (MOVES Go drive-cycle generator);
#' \item source-bin weighted rates with full-AC and I/M variants and EV
#' fleet-average adjustment (`BaseRateGenerator`);
#' \item fuel effects (general expressions, Complex/Predictive Model and sulfur
#' model), emission-rate adjustments, temperature and NOx humidity, air
#' conditioning and I/M (`BaseRateCalculator`);
#' \item fuel-supply market-share weighting and regulatory-class aggregation
#' weighted by source-bin activity.
#' }
#' @param database Path or `movesr_database`.
#' @param year Calendar year.
#' @param county_id County used for fuel supply, fuel usage, I/M and
#' barometric pressure. Default 26161 (Washtenaw County, MI).
#' @param month_id Month 1--12.
#' @param meteorology data.frame with hourID, temperature (F) and relHumidity
#' (percent). One rate set is produced per row.
#' @param source_type_id Source types; `NULL` for all.
#' @param fuel_type_id Fuel types (used fuel); `NULL` for all.
#' @param pollutant_id Pollutant IDs, e.g. 2 (CO), 3 (NOx).
#' @param road_type_id Road types 2--5.
#' @param fuel_supply Optional replacement for \code{\link{movesr_fuel_supply}}
#' (columns fuelTypeID, fuelFormulationID, marketShare and fuel properties).
#' Use it to describe local fuels.
#' @param barometric_pressure Optional pressure (inHg); default from `County`.
#' @param gpa_fract Optional GPA fraction; default from `County`.
#' @param im_coverage Optional user `IMCoverage` table; `FALSE` disables I/M.
#' @param keep_regclass Keep regulatory class detail instead of aggregating.
#' @param keep_components Keep intermediate factor columns.
#' @param process_id 1 running exhaust, 9 brakewear, 10 tirewear.
#' @param energy_units Unit for total energy (pollutant 91): "J" (default,
#' as the MOVES output), "KJ" or "MJ".
#' @return data.table of rates with `ratePerDistance` in g/mile (energy in
#' KJ/mile) and `ratePerDistanceKm`.
#' @details Validated against MOVES6 (86,304 reference factors, all 13
#' source types, 4 fuels, 3 temperatures): maximum relative difference
#' 6.5e-6, the precision of the MOVES output.
#' @export
movesr_running_rates <- function(database, year = 2025, county_id = 26161,
                                 month_id = 1,
                                 meteorology = data.frame(hourID = 1L,
                                                          temperature = 68,
                                                          relHumidity = 50),
                                 source_type_id = NULL, fuel_type_id = NULL,
                                 pollutant_id = c(2, 3), road_type_id = 2:5,
                                 fuel_supply = NULL,
                                 barometric_pressure = NULL, gpa_fract = NULL,
                                 im_coverage = NULL, keep_regclass = FALSE,
                                 keep_components = FALSE, process_id = 1L,
                                 energy_units = "J") {
  process_id <- as.integer(process_id)
  if (length(process_id) != 1L || !process_id %in% c(1L, 9L, 10L)) {
    stop("process_id must be 1 (running), 9 (brakewear) or 10 (tirewear)")
  }
  ctx <- .movesr_context(database, process_id, year, county_id, month_id, 5L,
                         meteorology, source_type_id, fuel_type_id,
                         pollutant_id, road_type_id, fuel_supply,
                         barometric_pressure, gpa_fract, im_coverage)
  tb <- ctx$tb
  source_type_id <- ctx$source_type_id
  phys <- tb("sourceusetypephysics")[sourceTypeID %in% source_type_id &
                                       endModelYearID >= year - 40 &
                                       beginModelYearID <= year]
  assoc <- tb("drivescheduleassoc")[sourceTypeID %in% source_type_id &
                                      roadTypeID %in% road_type_id]
  ds <- tb("driveschedule")
  speed_bins <- tb("avgspeedbin")
  fractions <- movesr_schedule_fractions(ds, assoc, speed_bins,
                                         source_type_id = source_type_id,
                                         road_type_id = road_type_id)
  dss <- tb("driveschedulesecond")[driveScheduleID %in% fractions$driveScheduleID]
  omd <- movesr_opmodes(dss, ds, phys, tb("operatingmode"), NULL, assoc,
                        schedule_fraction = fractions)
  pkeys <- unique(omd[, .(sourceTypeID, regClassID, beginModelYearID,
                          endModelYearID)])
  pkeys[, physicsID := seq_len(.N)]
  omd <- merge(omd, pkeys, by = c("sourceTypeID", "regClassID",
                                  "beginModelYearID", "endModelYearID"))
  omd <- omd[, .(physicsID, roadTypeID, avgSpeedBinID, opModeID,
                 opModeFraction)]
  rb <- unique(omd[, .(physicsID, roadTypeID, avgSpeedBinID)])
  if (process_id == 10L) {
    # AverageSpeedOperatingModeDistributionGenerator: one tirewear mode per
    # speed bin (avgSpeedBin.opModeIDTirewear), fraction 1.
    omd <- merge(rb, speed_bins[, .(avgSpeedBinID, opModeID = opModeIDTirewear)],
                 by = "avgSpeedBinID")[, opModeFraction := 1]
  } else {
    omd300 <- data.table::copy(rb)[, `:=`(opModeID = 300L, opModeFraction = 1)]
    omd <- data.table::rbindlist(list(omd, omd300), use.names = TRUE)
  }
  speed_bins <- speed_bins[, .(avgSpeedBinID, avgBinSpeed)]
  met <- ctx$met

  out <- vector("list", length(source_type_id))
  for (si in seq_along(source_type_id)) {
    s <- source_type_id[si]
    sw <- .movesr_sbweighted(ctx, s)
    if (is.null(sw)) next
    w <- sw$w
    pk <- data.table::copy(pkeys[sourceTypeID == s])
    wk <- unique(w[, .(regClassID, modelYearID)])
    wk <- merge(wk[, k := 1L], pk[, k := 1L], by = "k", allow.cartesian = TRUE,
                suffixes = c("", ".p"))
    wk <- wk[(regClassID.p == regClassID | regClassID.p == 0L |
                regClassID == 0L) &
               modelYearID >= beginModelYearID & modelYearID <= endModelYearID]
    wk <- wk[, .(physicsID = physicsID[1]), by = .(regClassID, modelYearID)]
    w <- merge(w, wk, by = c("regClassID", "modelYearID"))
    y <- merge(w, omd, by = c("physicsID", "opModeID"), allow.cartesian = TRUE)
    y <- y[, .(MBR = sum(MBR * opModeFraction),
               MBRIM = sum(MBRIM * opModeFraction),
               AC = sum(AC * opModeFraction),
               IMAC = sum(IMAC * opModeFraction)),
           by = .(sourceTypeID, roadTypeID, avgSpeedBinID, polProcessID,
                  modelYearID, fuelTypeID, ageGroupID, regClassID)]
    y <- merge(y, speed_bins, by = "avgSpeedBinID")
    y <- y[avgBinSpeed > 0]
    y[, `:=`(ER = MBR / avgBinSpeed, ERIM = MBRIM / avgBinSpeed,
             ERAC = AC / avgBinSpeed, ERIMAC = IMAC / avgBinSpeed)]
    y <- .movesr_static_factors(ctx, y, s)
    if (!nrow(y)) next
    hours <- vector("list", nrow(met))
    for (h in seq_len(nrow(met))) {
      mh <- met[h]
      z <- data.table::copy(y)
      z[, `:=`(hourID = mh$hourID, temperature = mh$temperature,
               relHumidity = mh$relHumidity)]
      k <- movesr_nox_humidity(z$fuelTypeID, mh$specificHumidity,
                               mh$molWaterFraction, ctx$nha)
      z[, tempFactor := .movesr_temp_factor(processID, pollutantID, fuelTypeID,
                                           sourceTypeID, k, mh$temperature,
                                           mh$heatIndex, taA, taB)]
      z <- merge(z, ctx$acf[hourID == mh$hourID & sourceTypeID == s,
                            .(modelYearID, ACFactor)],
                 by = "modelYearID", all.x = TRUE)
      z[is.na(ACFactor) | ACFactor <= 0, ACFactor := 0]
      z[, base := tempFactor *
          ((1 - IMAdjustFract) * (ER + ACFactor * ERAC) +
             IMAdjustFract * (ERIM + ACFactor * ERIMAC)) * era / eff]
      z[, rate := fuelFactor * base]
      hours[[h]] <- z
    }
    z <- data.table::rbindlist(hours)
    out[[si]] <- merge(z, sw$smfr, by = c("polProcessID", "modelYearID",
                                          "fuelTypeID", "regClassID"),
        allow.cartesian = TRUE)
  }
  z <- data.table::rbindlist(out)
  if (!nrow(z)) stop("No rates were generated for the selection")
  z <- .movesr_chain(ctx, z, ctx$plan$requested)
  res <- .movesr_finish(ctx, z, "ratePerDistance", keep_regclass,
                        keep_components, energy_units = energy_units, idcols =
                          c("yearID", "monthID", "dayID", "hourID",
                          "sourceTypeID", "fuelTypeID", "modelYearID",
                          "roadTypeID", "avgSpeedBinID", "pollutantID",
                          "processID", "temperature", "relHumidity"))
  res[, ratePerDistanceKm := ratePerDistance / 1.609344]
  data.table::setkeyv(res, c("hourID", "sourceTypeID", "fuelTypeID",
                             "modelYearID", "pollutantID", "roadTypeID",
                             "avgSpeedBinID"))
  res[]
}

#' Generate MOVES start-exhaust emission rates
#'
#' @description Start-exhaust (process 2) rate engine, grams per start. Uses
#' the same source-bin, fuel, I/M and regulatory-class stages as
#' \code{\link{movesr_running_rates}}, with:
#' \itemize{
#' \item operating modes 101--108 (soak time) from `StartsOpModeDistribution`
#' for the hour and day type;
#' \item MOVES start temperature adjustment by operating mode
#' (`StartTempAdjustment`), additive and weighted by the operating-mode
#' fraction:
#' \preformatted{
#' POLY: rate + f * (T' (A + T' (B + T' C))),  T' = min(T, 75) - 75
#' LOG:  rate + f * (B exp(A T') + C)
#' PM (11202, 11802): rate * B * exp(A (72 - min(T, 72))) + C
#' }
#' \item no air-conditioning adjustment.
#' }
#' @inheritParams movesr_running_rates
#' @param day_id MOVES day type: 5 weekday, 2 weekend.
#' @param energy_units Unit for total energy (pollutant 91).
#' @return data.table with `ratePerStart` (g/start).
#' @export
movesr_start_rates <- function(database, year = 2025, county_id = 26161,
                               month_id = 1, day_id = 5L,
                               meteorology = data.frame(hourID = 1L,
                                                        temperature = 68,
                                                        relHumidity = 50),
                               source_type_id = NULL, fuel_type_id = NULL,
                               pollutant_id = c(2, 3), fuel_supply = NULL,
                               barometric_pressure = NULL, gpa_fract = NULL,
                               im_coverage = NULL, keep_regclass = FALSE,
                               keep_components = FALSE, energy_units = "J") {
  ctx <- .movesr_context(database, 2L, year, county_id, month_id, day_id,
                         meteorology, source_type_id, fuel_type_id,
                         pollutant_id, 1L, fuel_supply, barometric_pressure,
                         gpa_fract, im_coverage)
  tb <- ctx$tb
  somd <- tb("startsopmodedistribution")[dayID == day_id &
                                           sourceTypeID %in% ctx$source_type_id]
  data.table::setorder(somd, sourceTypeID, hourID, ageID, opModeID)
  somd <- somd[, .SD[ageID == ageID[1]], by = .(sourceTypeID, hourID)]
  somd <- unique(somd[, .(sourceTypeID, hourID, opModeID, opModeFraction)],
                 by = c("sourceTypeID", "hourID", "opModeID"))
  # StartOperatingModeDistributionGenerator step 400: "All Starts" mode 100
  # with fraction 1 (used by rates given only for mode 100, e.g. N2O 602).
  somd <- data.table::rbindlist(list(
    somd, unique(somd[, .(sourceTypeID, hourID)])[, `:=`(opModeID = 100L,
                                                          opModeFraction = 1)]))
  sta <- tb("starttempadjustment")[polProcessID %in% ctx$pp]
  # Rates are activity-weighted in MOVES; with no starts the rate is 0.
  spd <- tb("startsperdaypervehicle")[dayID == day_id]
  shf <- tb("startshourfraction")[dayID == day_id]
  met <- ctx$met
  out <- vector("list", length(ctx$source_type_id))
  for (si in seq_along(ctx$source_type_id)) {
    s <- ctx$source_type_id[si]
    sw <- .movesr_sbweighted(ctx, s)
    if (is.null(sw)) next
    w <- merge(sw$w, ctx$ppmy[, .(polProcessID, modelYearID,
                                  modelYearGroupID)],
               by = c("polProcessID", "modelYearID"),
        allow.cartesian = TRUE)
    w <- merge(w, sta[, .(fuelTypeID, polProcessID, modelYearGroupID, opModeID,
                          staA = tempAdjustTermA, staB = tempAdjustTermB,
                          staC = tempAdjustTermC,
                          isLog = startTempEquationType == "LOG")],
               by = c("fuelTypeID", "polProcessID", "modelYearGroupID",
                      "opModeID"), all.x = TRUE,
        allow.cartesian = TRUE)
    hours <- vector("list", nrow(met))
    for (h in seq_len(nrow(met))) {
      mh <- met[h]
      om <- somd[sourceTypeID == s & hourID == mh$hourID,
                 .(opModeID, opModeFraction)]
      y <- merge(w, om, by = "opModeID",
        allow.cartesian = TRUE)
      if (!nrow(y)) next
      y[is.na(isLog), isLog := FALSE]
      y[, `:=`(
        ER = .movesr_start_temp(staA, staB, staC, isLog, polProcessID,
                                mh$temperature, opModeFraction,
                                MBR * opModeFraction),
        ERIM = .movesr_start_temp(staA, staB, staC, isLog, polProcessID,
                                  mh$temperature, opModeFraction,
                                  MBRIM * opModeFraction))]
      y <- y[, .(ER = sum(ER), ERIM = sum(ERIM)),
             by = .(sourceTypeID, polProcessID, modelYearID, fuelTypeID,
                    ageGroupID, regClassID)]
      y <- .movesr_static_factors(ctx, y, s)
      if (!nrow(y)) next
      y[, `:=`(hourID = mh$hourID, temperature = mh$temperature,
               relHumidity = mh$relHumidity)]
      y[, tempFactor := .movesr_temp_factor(processID, pollutantID, fuelTypeID,
                                           sourceTypeID, rep(1, .N),
                                           mh$temperature, mh$heatIndex,
                                           taA, taB)]
      y[, ACFactor := 0]
      y[, base := tempFactor *
          ((1 - IMAdjustFract) * ER + IMAdjustFract * ERIM) * era / eff]
      y[, rate := fuelFactor * base]
      act <- spd[sourceTypeID == s]$startsPerDayPerVehicle *
        shf[sourceTypeID == s & hourID == mh$hourID]$allocationFraction
      if (length(act) && !is.na(act[1]) && act[1] == 0) y[, `:=`(rate = 0, base = 0)]
      hours[[h]] <- y
    }
    z <- data.table::rbindlist(hours)
    if (!nrow(z)) next
    out[[si]] <- merge(z, sw$smfr, by = c("polProcessID", "modelYearID",
                                          "fuelTypeID", "regClassID"),
        allow.cartesian = TRUE)
  }
  z <- data.table::rbindlist(out)
  if (!nrow(z)) stop("No start rates were generated for the selection")
  z <- .movesr_chain(ctx, z, ctx$plan$requested)
  res <- .movesr_finish(ctx, z, "ratePerStart", keep_regclass, keep_components,
                        energy_units = energy_units, idcols =
                        c("yearID", "monthID", "dayID", "hourID",
                          "sourceTypeID", "fuelTypeID", "modelYearID",
                          "pollutantID", "processID", "temperature",
                          "relHumidity"))
  data.table::setkeyv(res, c("hourID", "sourceTypeID", "fuelTypeID",
                             "modelYearID", "pollutantID"))
  res[]
}
