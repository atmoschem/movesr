# Port of the TotalActivityGenerator activity needed for evaporative rates:
# population, travel fractions, VMT, SHO, SHP and the zone population.

#' MOVES activity: SHO, parked hours and zone population
#'
#' @description Port of `TotalActivityGenerator` (TAG steps for base years)
#' and the population part of `ActivityCalculator.sql`:
#' \preformatted{
#' population[st, age]   = sourceTypePopulation * ageFraction
#' travelFraction        = popShareWithinHPMS * relativeMAR / sum(...)
#' annualVMT[st,age,rd]  = HPMS VMT * roadTypeVMTFraction * travelFraction
#' VMT[hour]             = annualVMT * month * day * hour fractions / (days/7)
#' SHO                   = VMT / averageSpeed
#' SHP                   = population * noOfRealDays - sum_roads SHO
#' zone population       = population * sum(rtVMTFrac*SHOAlloc)/sum(rtVMTFrac)
#' parked fraction       = SHP * SHPAllocFactor / (zone population * noOfRealDays)
#' }
#' @param database A `movesr_database`.
#' @param year Calendar year (MOVES base year; for other years the default
#' `SourceTypeYear` population of that year is used, which approximates the
#' MOVES growth step).
#' @param county_id County (zone = county * 10).
#' @param month_id Month.
#' @param day_id Day type.
#' @return list with population, travel_fraction, annual_vmt, sho, shp_by_age_hour,
#' zone_population and parked (sourceTypeID, ageID, hourDayID, shpPerVehicle).
#' @export
movesr_activity <- function(database, year = 2025, county_id = 26161,
                            month_id = 7, day_id = 5L) {
  if (is.character(database)) database <- movesr_database(database)
  tb <- function(n) movesr_table(database, n)
  sty <- tb("sourcetypeyear")[yearID == year, .(sourceTypeID, sourceTypePopulation)]
  stad <- tb("sourcetypeagedistribution")[yearID == year, .(sourceTypeID, ageID, ageFraction)]
  pop <- merge(sty, stad, by = "sourceTypeID")
  pop[, population := sourceTypePopulation * ageFraction]
  sut <- tb("sourceusetype")[, .(sourceTypeID, HPMSVTypeID = HPMSVtypeID)]
  sta <- tb("sourcetypeage")[, .(sourceTypeID, ageID, relativeMAR)]
  x <- merge(pop, sut, by = "sourceTypeID")
  x[, hpmsPop := sum(population), by = HPMSVTypeID]
  x[, fwh := data.table::fifelse(hpmsPop > 0, population / hpmsPop, 0)]
  x <- merge(x, sta, by = c("sourceTypeID", "ageID"))
  x[, hpmsTF := sum(fwh * relativeMAR), by = HPMSVTypeID]
  x[, travelFraction := data.table::fifelse(hpmsTF > 0, fwh * relativeMAR / hpmsTF, 0)]
  vmt <- tb("hpmsvtypeyear")[yearID == year, .(HPMSVTypeID = HPMSVtypeID, VMT = HPMSBaseYearVMT)]
  rtd <- tb("roadtypedistribution")[, .(sourceTypeID, roadTypeID, roadTypeVMTFraction)]
  av <- merge(x[, .(sourceTypeID, ageID, HPMSVTypeID, travelFraction)], vmt,
    by = "HPMSVTypeID")
  av <- merge(av, rtd, by = "sourceTypeID", allow.cartesian = TRUE)
  av[, annualVMT := VMT * roadTypeVMTFraction * travelFraction]
  mvf <- tb("monthvmtfraction")[monthID == month_id, .(sourceTypeID, monthVMTFraction)]
  dvf <- tb("dayvmtfraction")[monthID == month_id & dayID == day_id,
    .(sourceTypeID, roadTypeID, dayVMTFraction)]
  hvf <- tb("hourvmtfraction")[dayID == day_id, .(sourceTypeID, roadTypeID, hourID,
    hourVMTFraction)]
  days <- tb("monthofanyyear")[monthID == month_id]$noOfDays
  h <- merge(merge(av, mvf, by = "sourceTypeID"), dvf, by = c("sourceTypeID", "roadTypeID"))
  h <- merge(h, hvf, by = c("sourceTypeID", "roadTypeID"), allow.cartesian = TRUE)
  h[, hourVMT := annualVMT * (monthVMTFraction * dayVMTFraction) * hourVMTFraction /
    (days / 7.0)]
  bins <- tb("avgspeedbin")[, .(avgSpeedBinID, avgBinSpeed)]
  asd <- tb("avgspeeddistribution")[hourDayID %% 10L == day_id]
  asp <- merge(asd, bins, by = "avgSpeedBinID")[
    , .(averageSpeed = sum(avgBinSpeed * avgSpeedFraction)),
    by = .(sourceTypeID, roadTypeID, hourID = hourDayID %/% 10L)]
  h <- merge(h, asp, by = c("sourceTypeID", "roadTypeID", "hourID"), all.x = TRUE)
  h[, SHO := data.table::fifelse(!is.na(averageSpeed) & averageSpeed != 0,
    hourVMT / averageSpeed, 0)]
  nrd <- tb("dayofanyweek")[dayID == day_id]$noOfRealDays
  shp <- merge(h[hourVMT > 0, .(SHO = sum(SHO)), by = .(sourceTypeID, ageID, hourID)],
    pop[, .(sourceTypeID, ageID, population)], by = c("sourceTypeID", "ageID"))
  shp[, SHP := population * nrd - SHO]
  zone <- tb("zone")[countyID == county_id]
  zr <- tb("zoneroadtype")[zoneID == zone$zoneID[1], .(SHOAllocFactor = sum(SHOAllocFactor)),
    by = roadTypeID]
  sf <- merge(rtd, zr, by = "roadTypeID")[
    , .(sutFraction = sum(roadTypeVMTFraction * SHOAllocFactor) / sum(roadTypeVMTFraction)),
    by = sourceTypeID]
  zp <- merge(pop[, .(sourceTypeID, ageID, population)], sf, by = "sourceTypeID")
  zp[, zonePopulation := population * sutFraction]
  pk <- merge(shp, zp[, .(sourceTypeID, ageID, zonePopulation)], by = c("sourceTypeID", "ageID"))
  pk[, shpPerVehicle := SHP * zone$SHPAllocFactor[1] / (zonePopulation * nrd)]
  pk[, hourDayID := hourID * 10L + day_id]
  list(population = pop, travel_fraction = x[, .(sourceTypeID, ageID, travelFraction)],
    annual_vmt = av, sho = h, shp_by_age_hour = shp, zone_population = zp,
    parked = pk[is.finite(shpPerVehicle), .(sourceTypeID, ageID, hourDayID, shpPerVehicle)])
}
