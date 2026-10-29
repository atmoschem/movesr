.cm_name <- function(x) tolower(gsub("[^A-Za-z0-9]", "", x))

# Rewrites ComplexModelParameterName.cmpExpression into R:
#   cmp.coeff -> coeff, fp.X -> f_x, fp_X.center -> c_x, fp_X.stddev -> s_x
.cm_translate <- function(expr, stddev = FALSE) {
  e <- expr
  if (stddev) {
    # FuelEffectsGenerator.rewriteCmpExpressionToIncludeStdDev()
    e <- gsub("\\(([^()]*?)(fp_([A-Za-z0-9]+))\\.center\\)",
      "((\\1\\2.center)/\\2.stddev)", e, perl = TRUE)
  }
  e <- gsub("cmp\\.coeff", "coeff", e)
  e <- gsub("fp_([A-Za-z0-9]+)\\.center", "c_\\L\\1", e, perl = TRUE)
  e <- gsub("fp_([A-Za-z0-9]+)\\.stddev", "s_\\L\\1", e, perl = TRUE)
  e <- gsub("fp\\.([A-Za-z0-9]+)", "f_\\L\\1", e, perl = TRUE)
  e
}

.cm_fuel_parameters <- function(parameters, formulations) {
  ff <- .movesr_fuel_nulls(formulations)
  env <- as.list(ff)
  names(env) <- tolower(names(env))
  out <- list()
  for (i in seq_len(nrow(parameters))) {
    p <- parameters[i]
    e <- tolower(gsub("ff\\.", "", p$fuelParameterExpression))
    v <- eval(parse(text = e), envir = env, enclos = baseenv())
    out[[paste0("f_", .cm_name(p$fuelParameterName))]] <- rep_len(as.numeric(v), nrow(ff))
  }
  out
}

#' MOVES Complex/Predictive Model fuel ratios (pre-2001 gasoline)
#'
#' @description Port of `FuelEffectsGenerator.doCOCalculations()`,
#' `doPredictiveCalculations()` (THC `predictHC`, NOx `predictNOx`) and the
#' sulfur model (`database/SulfurModel.sql`). Produces `CriteriaRatio` rows
#' for model years without a general fuel-ratio expression.
#' @param database A `movesr_database`.
#' @param supply Fuel supply (\code{\link{movesr_fuel_supply}}).
#' @param year Calendar year.
#' @param pol_process_id Requested polProcessIDs (101, 102, 201, 202, 301,
#' 302 are used).
#' @param source_type_id Source types.
#' @return data.table in the format of \code{\link{movesr_fuel_ratios}}
#' (model-year and age ranges collapsed to single values).
#' @details
#' \preformatted{
#' predictive:  ratioNoSulfur = sum_m w_m exp(T_m) / sum_m w_m exp(B_m)
#' CO:          ratioNoSulfur = 1 + sum_m w_m (exp(T_m) / exp(B_m) - 1)
#' T_m, B_m     = sum_cmp coeff * term(fuel parameters - centre [/ stddev])
#' ratio        = sulfur model(ratioNoSulfur, target and base sulfur)
#' }
#' Weights `w_m` come from `FuelModelWtFactor` by model-year group and age,
#' centres and standard deviations from `MeanFuelParameters`, base fuels from
#' `BaseFuel`. The CO model uses the base fuel sulfur for the target fuel.
#' @export
movesr_complex_ratios <- function(database, supply, year, pol_process_id,
                                  source_type_id) {
  tb <- function(name) movesr_table(database, name)
  empty <- data.table::data.table(
    fuelTypeID = integer(), fuelFormulationID = integer(),
    polProcessID = integer(), sourceTypeID = integer(),
    minModelYearID = integer(), maxModelYearID = integer(),
    minAgeID = integer(), maxAgeID = integer(),
    fuelEffectRatio = numeric(), fuelEffectRatioGPA = numeric())
  engines <- list(co = c(201L, 202L), predictHC = c(101L, 102L),
    predictNOx = c(301L, 302L))
  cutoffs <- tb("modelyearcutpoints")
  cut <- function(name, default) {
    v <- cutoffs[tolower(cutPointName) == tolower(name)]$modelYearID
    if (length(v)) v[1] else default
  }
  highest <- cut("HighestFuelPredictiveModelYear", 2003L)
  fmn <- tb("fuelmodelname")
  fpn <- tb("fuelparametername")
  cmpn <- tb("complexmodelparametername")
  cmp <- tb("complexmodelparameters")
  mfp <- tb("meanfuelparameters")
  wt <- tb("fuelmodelwtfactor")
  base <- tb("basefuel")
  ffall <- tb("fuelformulation")
  ffall <- merge(ffall, tb("fuelsubtype")[, .(fuelSubtypeID, fuelTypeID)],
    by = "fuelSubtypeID")
  ages <- sort(unique(wt$ageID))
  ages <- ages[year - ages >= year - 40 & year - ages <= year]
  sup <- data.table::as.data.table(supply)[marketShare > 0]
  rows <- list()
  for (engine in names(engines)) {
    pps <- intersect(engines[[engine]], pol_process_id)
    if (!length(pps)) next
    models <- fmn[grepl(paste0("|", engine, "|"), calculationEngines,
      fixed = TRUE)]$fuelModelID
    if (!length(models)) next
    coeff_n <- if (engine == "co") 3L else 1L
    terms <- data.table::copy(cmpn)
    terms[, rexpr := vapply(cmpExpression, .cm_translate, "",
      stddev = engine != "co")]
    for (pp in pps) {
      cf <- cmp[polProcessID == pp & fuelModelID %in% models]
      if (!nrow(cf)) next
      cf[, coeff := if (coeff_n == 3L) coeff1 + coeff2 + coeff3 else coeff1]
      for (fuel in intersect(unique(mfp[polProcessID == pp]$fuelTypeID),
        unique(sup$fuelTypeID))) {
        targets <- sup[fuelTypeID == fuel]
        groups <- sort(unique(mfp[polProcessID == pp & fuelTypeID == fuel]$modelYearGroupID))
        for (g in groups) {
          lo <- max(g %/% 10000L, 1950L)
          hi <- g %% 10000L
          if (hi < 1950L || hi > 2060L) hi <- 2060L
          lo <- max(lo, year - 40L)
          hi <- min(hi, year)
          if (lo > hi) next
          bf <- base[calculationEngine == engine & fuelTypeID == fuel &
            modelYearGroupID == g]$fuelFormulationID
          if (!length(bf)) bf <- base[calculationEngine == engine &
            fuelTypeID == fuel &
            modelYearGroupID == 0L]$fuelFormulationID
          if (!length(bf)) next
          bff <- ffall[fuelFormulationID == bf[1]]
          tff <- data.table::rbindlist(list(targets, bff), fill = TRUE)
          tff <- tff[!duplicated(fuelFormulationID)]
          bff <- .movesr_fuel_nulls(bff)
          tgt_fuel <- .movesr_fuel_nulls(tff)
          if (engine == "co") tgt_fuel[, sulfurLevel := bff$sulfurLevel]
          ft <- .cm_fuel_parameters(fpn, tgt_fuel)
          fb <- .cm_fuel_parameters(fpn, bff)
          m <- mfp[polProcessID == pp & fuelTypeID == fuel & modelYearGroupID == g]
          if (!nrow(m)) m <- mfp[polProcessID == pp & fuelTypeID == fuel &
            modelYearGroupID == 0L]
          cs <- list()
          for (nm in .cm_name(fpn$fuelParameterName)) {
            cs[[paste0("c_", nm)]] <- 0
            cs[[paste0("s_", nm)]] <- 1
          }
          if (nrow(m)) {
            m <- merge(m, fpn[, .(fuelParameterID, fuelParameterName)],
              by = "fuelParameterID")
            for (i in seq_len(nrow(m))) {
              nm <- .cm_name(m$fuelParameterName[i])
              cv <- m$centeringValue[i]
              sv <- m$stdDevValue[i]
              cs[[paste0("c_", nm)]] <- if (is.na(cv)) 0 else cv
              cs[[paste0("s_", nm)]] <- if (is.na(sv) || sv <= 0) 1 else sv
            }
          }
          sums_t <- sums_b <- list()
          for (fm in models) {
            st <- rep(0, nrow(tgt_fuel))
            sb <- 0
            for (j in seq_len(nrow(terms))) {
              co <- cf[fuelModelID == fm & cmpID == terms$cmpID[j]]$coeff
              if (!length(co) || co[1] == 0) next
              env_t <- c(ft, cs, list(coeff = co[1]))
              env_b <- c(fb, cs, list(coeff = co[1]))
              ex <- parse(text = terms$rexpr[j])
              st <- st + eval(ex, env_t, baseenv())
              sb <- sb + eval(ex, env_b, baseenv())
            }
            sums_t[[as.character(fm)]] <- st
            sums_b[[as.character(fm)]] <- sb
          }
          for (age in ages) {
            my <- year - age
            if (my < lo || my > hi) next
            w <- wt[modelYearGroupID == g & ageID == age & fuelModelID %in% models]
            if (!nrow(w)) w <- wt[modelYearGroupID == 0L & ageID == age &
              fuelModelID %in% models]
            ww <- stats::setNames(rep(0, length(models)), models)
            ww[as.character(w$fuelModelID)] <- w$fuelModelWtFactor
            if (engine == "co") {
              r <- rep(0, nrow(tgt_fuel))
              for (fm in as.character(models)) {
                te <- ifelse(sums_t[[fm]] == 0, 0, exp(sums_t[[fm]]))
                be <- if (sums_b[[fm]] == 0) 0 else exp(sums_b[[fm]])
                r <- r + ww[[fm]] * (te / be - 1)
              }
              r <- 1 + r
            } else {
              num <- rep(0, nrow(tgt_fuel))
              den <- 0
              for (fm in as.character(models)) {
                num <- num + ww[[fm]] * exp(sums_t[[fm]])
                den <- den + ww[[fm]] * exp(sums_b[[fm]])
              }
              r <- num / den
            }
            r[!is.finite(r)] <- 1
            # Predictive model years after HighestFuelPredictiveModelYear get
            # ratioNoSulfur = 1 but still pass through the sulfur model.
            if (engine != "co" && my > highest) r[] <- 1
            rows[[length(rows) + 1L]] <- data.table::data.table(
              fuelTypeID = fuel, fuelFormulationID = tff$fuelFormulationID,
              baseFuelFormulationID = bf[1], polProcessID = pp,
              pollutantID = pp %/% 100L, processID = pp %% 100L,
              modelYearID = my, ageID = age, ratioNoSulfur = r)
          }
        }
      }
    }
  }
  if (!length(rows)) return(empty)
  input <- data.table::rbindlist(rows)
  tgt <- data.table::rbindlist(list(sup, ffall), fill = TRUE)
  tgt <- tgt[!duplicated(fuelFormulationID)]
  out <- .movesr_sulfur_model(database, input, tgt = tgt, year = year,
    source_type_id = source_type_id, cut = cut)
  if (!nrow(out)) return(empty)
  out[, .(fuelTypeID, fuelFormulationID, polProcessID, sourceTypeID,
    minModelYearID = modelYearID, maxModelYearID = modelYearID,
    minAgeID = ageID, maxAgeID = ageID,
    fuelEffectRatio = ratio, fuelEffectRatioGPA = ratioGPA)]
}

# Port of database/SulfurModel.sql (non rate-of-progress version).
.movesr_sulfur_model <- function(database, input, tgt, year, source_type_id, cut) {
  tb <- function(name) movesr_table(database, name)
  smc <- tb("sulfurmodelcoeff")[sourceTypeID %in% source_type_id]
  smn <- tb("sulfurmodelname")
  sbase <- tb("sulfurbase")
  m6 <- tb("m6sulfurcoeff")
  cap <- tb("sulfurcapamount")
  thc_nox_start <- cut("sulfurModelTHCNOxStart", 1975L)
  thc_nox_end <- cut("sulfurModelTHCNOxEnd", 2003L)
  co_start <- cut("sulfurModelCOStart", 1975L)
  gpa_start <- cut("sulfurModelGPAPhaseInStart", 2004L)
  gpa_end <- cut("sulfurModelGPAPhaseInEnd", 2006L)

  range_years <- function(id) {
    lo <- ifelse(round(id / 10000) == 0, 1950, round(id / 10000))
    list(lo = lo, hi = id %% 10000)
  }
  r <- range_years(sbase$modelYearGroupID)
  sb <- data.table::rbindlist(lapply(seq_len(nrow(sbase)), function(i) {
    data.table::data.table(modelYearID = r$lo[i]:r$hi[i],
      sulfurBasis = sbase$sulfurBasis[i],
      sulfurGPAMax = sbase$sulfurGPAMax[i],
      sulfurBase = sbase$sulfurBase[i])
  }))
  smc <- merge(smc, smn, by.x = c("M6emitterID", "sulfurFunctionID"),
    by.y = c("M6EmitterID", "sulfurFunctionID"))
  r <- range_years(smc$fuelMYGroupID)
  sc <- data.table::rbindlist(lapply(seq_len(nrow(smc)), function(i) {
    data.table::data.table(modelYearID = r$lo[i]:r$hi[i],
      processID = smc$processID[i],
      pollutantID = smc$pollutantID[i],
      m6emitterID = smc$M6emitterID[i],
      sourceTypeID = smc$sourceTypeID[i],
      sulfurCoeff = smc$sulfurCoeff[i],
      sulfurFunctionName = smc$sulfurFunctionName[i],
      lowSulfurCoeff = smc$lowSulfurCoeff[i])
  }))
  x <- merge(input, sc, by = c("processID", "pollutantID", "modelYearID"),
    allow.cartesian = TRUE)
  x <- merge(x, sb, by = "modelYearID")
  x <- merge(x, .movesr_fuel_nulls(tgt)[, .(fuelFormulationID, sulfurLevel)],
    by = "fuelFormulationID")
  x <- x[year - modelYearID == ageID]
  x <- x[fuelTypeID %in% cap$fuelTypeID]
  if (!nrow(x)) return(data.table::data.table())
  loglog <- x$sulfurFunctionName == "log-log"
  x[, sulfShortTarget := ifelse(loglog,
    ifelse(sulfurLevel > 0, exp(sulfurCoeff * log(pmax(sulfurLevel, 1e-300))), 0),
    exp(sulfurCoeff * sulfurLevel))]
  x[, sulfShort30 := ifelse(loglog, exp(sulfurCoeff * log(sulfurBasis)),
    exp(sulfurCoeff * sulfurBasis))]
  m6r <- m6[x, on = .(pollutantID, minModelYearID <= modelYearID,
    maxModelYearID >= modelYearID),
  .(sulfurLongCoeff = x.sulfurLongCoeff,
    sulfurIRFactor = x.sulfurIRFactor,
    maxIRFactorSulfur = x.maxIRFactorSulfur), mult = "first"]
  x[, `:=`(longRaw = m6r$sulfurLongCoeff, irRaw = m6r$sulfurIRFactor,
    maxIR = m6r$maxIRFactorSulfur)]
  x[, sulfurIRFactor := ifelse(is.na(irRaw), 0,
    ifelse(!is.na(maxIR) & sulfurLevel <= maxIR, irRaw, 0))]
  x[, sulfAdj2 := ((sulfShortTarget - sulfShort30) / sulfShort30) *
    ifelse(is.na(longRaw), 1, longRaw)]
  x[, SulfIRR := ifelse(is.na(maxIR) | maxIR <= 0, 0,
    ifelse(sulfurLevel <= maxIR, exp(sulfurCoeff * log(pmax(maxIR, 1e-300))),
      ifelse(sulfurLevel > 0,
        exp(sulfurCoeff * log(pmax(sulfurLevel, 1e-300))), 0)))]
  x[, sulfurLongCoeff := ifelse(is.na(longRaw), 1, longRaw)]
  x[, minSulfAdjust := ifelse(sulfurBase <= 30, 0.85, 0.50)]
  x[, v := 1 + (sulfurIRFactor * ((SulfIRR - sulfShort30) / sulfShort30) +
    (1 - sulfurIRFactor) * sulfAdj2)]
  x[, sulfAdj3 := ifelse(v <= minSulfAdjust, minSulfAdjust, v)]
  gpa <- x$modelYearID >= gpa_start & x$modelYearID <= gpa_end & x$sulfurLevel >= 0
  x[, GPASulfadj := ifelse(gpa,
    1 + (sulfurIRFactor * (((exp(sulfurCoeff * log(sulfurGPAMax)) -
      sulfShort30) / sulfShort30) *
      sulfurLongCoeff) +
      (1 - sulfurIRFactor) * sulfAdj2),
    sulfAdj3)]
  k <- c("fuelFormulationID", "polProcessID", "modelYearID", "ageID", "sourceTypeID")
  hi <- x[m6emitterID == 2L]
  no <- x[m6emitterID == 1L]
  y <- merge(hi[, c(k, "fuelTypeID", "pollutantID", "processID",
    "baseFuelFormulationID", "ratioNoSulfur", "sulfAdj3",
    "GPASulfadj"), with = FALSE],
  no[, c(k, "sulfAdj3", "GPASulfadj", "minSulfAdjust",
    "lowSulfurCoeff", "sulfurLevel", "sulfurBasis"), with = FALSE],
  by = k, suffixes = c(".h", ".n"))
  y[, sulfAdj3 := 0.5 * sulfAdj3.n + 0.5 * sulfAdj3.h]
  y[, GPAsulfAdj3 := 0.5 * GPASulfadj.n + 0.5 * GPASulfadj.h]
  b <- y[, .(baseFuelFormulationID = fuelFormulationID, polProcessID,
    modelYearID, ageID, sourceTypeID, baseAdj3 = sulfAdj3)]
  z <- merge(y, b, by = c("baseFuelFormulationID", "polProcessID",
    "modelYearID", "ageID", "sourceTypeID"))
  rel <- pmax(z$sulfAdj3 / z$baseAdj3, z$minSulfAdjust)
  low <- !is.na(z$lowSulfurCoeff) & z$sulfurLevel <= 30 & z$sulfurBasis == 30
  inner <- (z$modelYearID >= thc_nox_start & z$modelYearID <= thc_nox_end &
    z$pollutantID %in% c(1L, 3L)) |
    (z$modelYearID >= co_start & z$pollutantID == 2L)
  later <- z$modelYearID > thc_nox_end & z$pollutantID %in% c(1L, 3L)
  ratio <- ifelse(low, pmax(1 - z$lowSulfurCoeff * (30 - z$sulfurLevel), 0) * z$ratioNoSulfur,
    ifelse(inner, rel * z$ratioNoSulfur, ifelse(later, rel, 1)))
  g <- z$GPAsulfAdj3 / z$baseAdj3
  gfac <- ifelse(z$modelYearID >= gpa_start & z$modelYearID <= gpa_end & g > 1,
    pmax(g, z$minSulfAdjust), 1)
  z[, `:=`(ratio = ratio, ratioGPA = ratio * gfac)]
  z[, .(fuelTypeID, fuelFormulationID, polProcessID, pollutantID, processID,
    sourceTypeID, modelYearID, ageID, ratio, ratioGPA, ratioNoSulfur)]
}
