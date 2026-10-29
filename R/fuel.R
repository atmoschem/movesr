#' Fuel supply for a county, year and month
#'
#' @description Resolves the MOVES fuel region (`RegionCounty`, region code 1),
#' fuel year (`Year`) and month group (`MonthOfAnyYear`) and returns the fuel
#' formulations with their market shares and properties.
#' @param database A `movesr_database`.
#' @param county_id County ID.
#' @param year Calendar year.
#' @param month_id Month 1--12.
#' @return data.table with fuelTypeID, fuelFormulationID, fuelSubtypeID,
#' marketShare and all `FuelFormulation` properties. NULL properties are set
#' to 0, as MOVES does before evaluating fuel effects.
#' @export
movesr_fuel_supply <- function(database, county_id, year, month_id = 1) {
  yr <- movesr_table(database, "year")
  fuel_year <- yr[yearID == year]$fuelYearID
  if (!length(fuel_year)) stop("No fuel year for calendar year ", year)
  rc <- movesr_table(database, "regioncounty")
  region <- rc[regionCodeID == 1L & countyID == county_id &
                 fuelYearID == fuel_year[1]]$regionID
  if (!length(region)) stop("No fuel region for county ", county_id)
  moy <- movesr_table(database, "monthofanyyear")
  month_group <- moy[monthID == month_id]$monthGroupID
  fs <- movesr_table(database, "fuelsupply")
  fs <- fs[fuelRegionID == region[1] & fuelYearID == fuel_year[1] &
             monthGroupID == month_group[1]]
  ff <- movesr_table(database, "fuelformulation")
  st <- movesr_table(database, "fuelsubtype")
  out <- merge(fs[, .(fuelFormulationID, marketShare)], ff,
               by = "fuelFormulationID")
  out <- merge(out, st[, .(fuelSubtypeID, fuelTypeID)], by = "fuelSubtypeID")
  out <- .movesr_high_ethanol(out, movesr_table(database, "e10fuelproperties"),
                              region[1], fuel_year[1], month_group[1])
  .movesr_fuel_nulls(out)
}

# FuelEffectsGenerator.alterHighEthanolFuelProperties(): E70/E85 formulations
# take their properties from E10FuelProperties (region first, then nation),
# keep RVP and get altRVP from the E10 RVP.
.movesr_high_ethanol <- function(x, e10, region, fuel_year, month_group) {
  x <- data.table::as.data.table(data.table::copy(x))
  x[, altRVP := RVP]
  hi <- which(x$fuelSubtypeID %in% c(51L, 52L))
  if (!length(hi)) return(x)
  e10 <- data.table::as.data.table(e10)[fuelYearID == fuel_year &
                                          monthGroupID == month_group]
  e0 <- e10[fuelRegionID == 0L]
  if (!nrow(e0)) return(x)
  e1 <- e10[fuelRegionID == region]
  pick <- function(col) {
    v1 <- if (nrow(e1) && col %in% names(e1)) e1[[col]][1] else NA
    v0 <- if (col %in% names(e0)) e0[[col]][1] else NA
    if (!is.na(v1)) v1 else v0
  }
  v <- pick("RVP")
  if (!is.na(v)) data.table::set(x, hi, "altRVP", v)
  for (col in c("sulfurLevel", "ETOHVolume", "MTBEVolume", "ETBEVolume",
                "TAMEVolume", "aromaticContent", "olefinContent",
                "benzeneContent", "e200", "e300", "BioDieselEsterVolume",
                "CetaneIndex", "PAHContent", "T50", "T90")) {
    v <- pick(col)
    if (!is.na(v) && col %in% names(x)) {
      data.table::set(x, hi, col, as.numeric(v))
    }
  }
  if (all(c("ETOHVolume", "MTBEVolume", "ETBEVolume", "TAMEVolume") %in% names(x))) {
    s <- rowSums(cbind(x$ETOHVolume, x$MTBEVolume, x$ETBEVolume, x$TAMEVolume),
                 na.rm = TRUE)
    num <- rowSums(cbind(x$ETOHVolume * 0.3653, x$MTBEVolume * 0.1792,
                         x$ETBEVolume * 0.1537, x$TAMEVolume * 0.1651),
                   na.rm = TRUE)
    x[hi, volToWtPercentOxy := ifelse(s[hi] > 0, num[hi] / s[hi], 0)]
  }
  x
}

.movesr_criteria_polproc <- c(101L, 102L, 201L, 202L, 301L, 302L)

.movesr_fuel_nulls <- function(x) {
  x <- data.table::as.data.table(data.table::copy(x))
  props <- c("RVP", "sulfurLevel", "ETOHVolume", "MTBEVolume", "ETBEVolume",
             "TAMEVolume", "aromaticContent", "olefinContent",
             "benzeneContent", "e200", "e300", "volToWtPercentOxy",
             "BioDieselEsterVolume", "CetaneIndex", "PAHContent", "T50", "T90")
  for (p in intersect(props, names(x))) {
    v <- as.numeric(x[[p]])
    v[is.na(v)] <- 0
    data.table::set(x, j = p, value = v)
  }
  x
}

.movesr_translate_expression <- function(expr) {
  e <- tolower(expr)
  e <- gsub("\\bif\\s*\\(", "ifelse(", e, perl = TRUE)
  e <- gsub("\\bleast\\s*\\(", "pmin(", e, perl = TRUE)
  e <- gsub("\\bgreatest\\s*\\(", "pmax(", e, perl = TRUE)
  e <- gsub("\\bln\\s*\\(", "log(", e, perl = TRUE)
  e <- gsub("\\bpow\\s*\\(", "`^`(", e, perl = TRUE)
  e <- gsub("\\band\\b", "&", e, perl = TRUE)
  e <- gsub("\\bor\\b", "|", e, perl = TRUE)
  e <- gsub("([^<>!=])=([^=])", "\\1==\\2", e, perl = TRUE)
  e
}

.movesr_eval_expression <- function(expr, data) {
  if (is.na(expr) || !nzchar(trimws(expr))) return(rep(1, nrow(data)))
  env <- as.list(data)
  names(env) <- tolower(names(env))
  value <- suppressWarnings(eval(parse(text = .movesr_translate_expression(expr)),
                                 envir = env, enclos = baseenv()))
  value <- rep_len(as.numeric(value), nrow(data))
  value[!is.finite(value)] <- 1
  value
}

#' Evaluate MOVES general fuel-effect ratios
#'
#' @description Port of `FuelEffectsGenerator.doGeneralFuelRatio()`. Each
#' `GeneralFuelRatioExpression` is evaluated against the supplied fuel
#' formulations of the same fuel type. Results feed both `GeneralFuelRatio`
#' and, for THC/CO/NOx (101, 102, 201, 202, 301, 302), `CriteriaRatio`.
#' @param expressions `GeneralFuelRatioExpression` table.
#' @param formulations Fuel formulations with fuelTypeID and properties,
#' normally from \code{\link{movesr_fuel_supply}}.
#' @param pol_process_id Optional polProcessID filter.
#' @return data.table fuelTypeID, fuelFormulationID, polProcessID,
#' sourceTypeID, minModelYearID, maxModelYearID, minAgeID, maxAgeID,
#' fuelEffectRatio, fuelEffectRatioGPA.
#' @details Non-finite results (MySQL NULL, e.g. `ln(0)`) are treated as 1, as
#' MOVES applies `ifnull(ratio, 1)`.
#' @export
movesr_fuel_ratios <- function(expressions, formulations, pol_process_id = NULL) {
  ex <- data.table::as.data.table(data.table::copy(expressions))
  ff <- .movesr_fuel_nulls(formulations)
  if (!is.null(pol_process_id)) ex <- ex[polProcessID %in% pol_process_id]
  ex <- ex[minModelYearID <= maxModelYearID & fuelTypeID %in% ff$fuelTypeID]
  if (!nrow(ex)) {
    return(data.table::data.table(
      fuelTypeID = integer(), fuelFormulationID = integer(),
      polProcessID = integer(), sourceTypeID = integer(),
      minModelYearID = integer(), maxModelYearID = integer(),
      minAgeID = integer(), maxAgeID = integer(),
      fuelEffectRatio = numeric(), fuelEffectRatioGPA = numeric()))
  }
  out <- vector("list", nrow(ex))
  for (i in seq_len(nrow(ex))) {
    e <- ex[i]
    d <- ff[fuelTypeID == e$fuelTypeID]
    out[[i]] <- data.table::data.table(
      fuelTypeID = e$fuelTypeID, fuelFormulationID = d$fuelFormulationID,
      polProcessID = e$polProcessID, sourceTypeID = e$sourceTypeID,
      minModelYearID = e$minModelYearID, maxModelYearID = e$maxModelYearID,
      minAgeID = e$minAgeID, maxAgeID = e$maxAgeID,
      fuelEffectRatio = .movesr_eval_expression(e$fuelEffectRatioExpression, d),
      fuelEffectRatioGPA = .movesr_eval_expression(e$fuelEffectRatioGPAExpression, d))
  }
  data.table::rbindlist(out)
}

#' Market-share-weighted fuel adjustment
#'
#' @description Combines fuel supply shares and fuel-effect ratios into one
#' multiplicative factor per fuel type, pollutant/process, source type, model
#' year and age:
#' \preformatted{
#' r      = ratio + GPAFract * (ratioGPA - ratio)
#' factor = sum(marketShare * r) over formulations of the fuel type
#' }
#' Formulations without a ratio contribute `marketShare * 1`.
#' @param supply Output of \code{\link{movesr_fuel_supply}}.
#' @param ratios Output of \code{\link{movesr_fuel_ratios}}.
#' @param keys data.table with fuelTypeID, polProcessID, sourceTypeID,
#' modelYearID and ageID to evaluate.
#' @param gpa_fract County GPA fraction (0 outside the GPA).
#' @param by_formulation Return one row per fuel formulation with weight
#' `w = marketShare * ratio` instead of the sum.
#' @return `keys` with column `fuelFactor` (or per-formulation `w`).
#' @export
movesr_fuel_adjustment <- function(supply, ratios, keys, gpa_fract = 0,
                                   by_formulation = FALSE) {
  k <- unique(data.table::as.data.table(keys)[, .(fuelTypeID, polProcessID,
                                                  sourceTypeID, modelYearID,
                                                  ageID)])
  s <- data.table::as.data.table(supply)[, .(fuelTypeID, fuelFormulationID,
                                             marketShare)]
  x <- merge(k, s, by = "fuelTypeID", allow.cartesian = TRUE)
  x[, `:=`(ratio = 1, ratioFound = FALSE)]
  r <- data.table::as.data.table(ratios)
  if (nrow(r) && nrow(x)) {
    r <- r[polProcessID %in% x$polProcessID]
    r[, r := fuelEffectRatio + gpa_fract * (fuelEffectRatioGPA - fuelEffectRatio)]
    r[, order := seq_len(.N)]
    x[, rowid := .I]
    m <- merge(x[, .(rowid, fuelFormulationID, polProcessID, sourceTypeID,
                     modelYearID, ageID)],
               r[, .(fuelFormulationID, polProcessID, sourceTypeID,
                     minModelYearID, maxModelYearID, minAgeID, maxAgeID, r,
                     order)],
               by = c("fuelFormulationID", "polProcessID", "sourceTypeID"),
               allow.cartesian = TRUE)
    m <- m[modelYearID >= minModelYearID & modelYearID <= maxModelYearID &
             ageID >= minAgeID & ageID <= maxAgeID]
    if (nrow(m)) {
      data.table::setorder(m, rowid, order)
      # CriteriaRatio keeps the first ratio (insert ignore); GeneralFuelRatio
      # multiplies every matching detail.
      m <- m[, .(r = if (polProcessID[1] %in% .movesr_criteria_polproc) r[1]
                 else prod(r)), by = rowid]
      x[m$rowid, `:=`(ratio = m$r, ratioFound = TRUE)]
    }
    x[, rowid := NULL]
  }
  if (by_formulation) {
    x[, w := marketShare * ratio]
    return(x[])
  }
  x[, .(fuelFactor = sum(marketShare * ratio), marketShare = sum(marketShare)),
    by = .(fuelTypeID, polProcessID, sourceTypeID, modelYearID, ageID)]
}
