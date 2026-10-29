# Chained pollutants: MOVES calculators that run after BaseRateCalculator.
# All operate per regulatory class, before regulatory-class aggregation.

.movesr_chain_inputs <- list(
  `5` = 1L, `79` = 1L, `80` = 1L, `86` = 1L, `87` = 1L,
  `90` = 91L, `98` = c(91L, 1L, 6L), `31` = 91L,
  `110` = c(112L, 118L), `111` = c(112L, 118L), `112` = c(112L, 118L),
  `115` = c(112L, 118L), `118` = c(112L, 118L), `119` = c(112L, 118L),
  `100` = c(112L, 118L), `106` = 116L, `107` = 117L
)

# Pollutants that the BaseRateCalculator produces directly.
.movesr_base_pollutants <- c(1L, 2L, 3L, 6L, 30L, 91L, 112L, 118L, 116L, 117L)

.movesr_pollutant_plan <- function(pollutant_id) {
  pollutant_id <- as.integer(pollutant_id)
  base <- intersect(pollutant_id, .movesr_base_pollutants)
  for (p in pollutant_id) {
    inp <- .movesr_chain_inputs[[as.character(p)]]
    if (!is.null(inp)) base <- union(base, inp)
  }
  bad <- setdiff(pollutant_id, c(.movesr_base_pollutants,
                                 as.integer(names(.movesr_chain_inputs))))
  if (length(bad)) {
    stop("Pollutant(s) not supported yet: ", paste(bad, collapse = ", "))
  }
  list(base = sort(base), requested = pollutant_id,
       chained = any(!pollutant_id %in% .movesr_base_pollutants) ||
         any(pollutant_id %in% c(112L, 118L)))
}

.movesr_expand_years <- function(x, begin, end) {
  x <- data.table::as.data.table(x)
  n <- x[[end]] - x[[begin]] + 1L
  y <- x[rep(seq_len(.N), n)]
  y[, modelYearID := unlist(mapply(seq, x[[begin]], x[[end]], SIMPLIFY = FALSE))]
  y
}

.movesr_supply_attributes <- function(ctx) {
  sup <- data.table::copy(ctx$supply)
  st <- ctx$tb("fuelsubtype")[, .(fuelSubtypeID, carbonContent, oxidationFraction,
                                  energyContent)]
  if (!"fuelSubtypeID" %in% names(sup)) {
    stop("fuel_supply needs fuelSubtypeID for chained pollutants")
  }
  sup <- merge(sup, st, by = "fuelSubtypeID", all.x = TRUE)
  if (!"volToWtPercentOxy" %in% names(sup)) sup[, volToWtPercentOxy := 0]
  sup[, totalOxygenate := MTBEVolume + ETBEVolume + TAMEVolume + ETOHVolume]
  sup
}

# HCSpeciationCalculator (calc/hcspeciation): CH4, NMHC, NMOG, VOC, TOG from THC.
.movesr_hc_factors <- function(ctx, thc) {
  tb <- ctx$tb
  sup <- .movesr_supply_attributes(ctx)
  keys <- unique(thc[, .(fuelTypeID, polProcessID, sourceTypeID, modelYearID,
                         ageID, regClassID, processID)])
  w <- movesr_fuel_adjustment(ctx$supply, ctx$ratios,
                              unique(keys[, .(fuelTypeID, polProcessID, sourceTypeID,
                                              modelYearID, ageID)]),
                              ctx$gpa_fract, by_formulation = TRUE)
  w <- w[, .(fuelTypeID, polProcessID, sourceTypeID, modelYearID, ageID,
             fuelFormulationID, w)]
  # E85 alternate THC (pollutant 10001): expressions evaluated with altRVP.
  alt <- ctx$alt_ratios
  if (!is.null(alt) && nrow(alt)) {
    wa <- movesr_fuel_adjustment(ctx$alt_supply, alt,
                                 unique(keys[fuelTypeID == 5L & modelYearID >= 2001L,
                                             .(fuelTypeID, polProcessID, sourceTypeID,
                                               modelYearID, ageID)]),
                                 ctx$gpa_fract, by_formulation = TRUE)
    wa <- wa[ratioFound == TRUE, .(fuelTypeID, polProcessID, sourceTypeID,
                                   modelYearID, ageID, fuelFormulationID,
                                   w_alt = w)]
    w <- merge(w, wa, by = c("fuelTypeID", "polProcessID", "sourceTypeID",
                             "modelYearID", "ageID", "fuelFormulationID"),
               all.x = TRUE)
  } else {
    w[, w_alt := NA_real_]
  }
  x <- merge(keys, w, by = c("fuelTypeID", "polProcessID", "sourceTypeID",
                             "modelYearID", "ageID"), allow.cartesian = TRUE)
  x <- merge(x, sup[, .(fuelFormulationID, fuelSubtypeID, volToWtPercentOxy,
                        totalOxygenate)], by = "fuelFormulationID")
  mr <- .movesr_expand_years(tb("methanethcratio")[processID %in% unique(x$processID)],
                             "beginModelYearID", "endModelYearID")
  mr <- unique(mr[, .(processID, fuelSubtypeID, regClassID, modelYearID, CH4THCRatio)],
               by = c("processID", "fuelSubtypeID", "regClassID", "modelYearID"),
               fromLast = TRUE)
  hs <- tb("hcspeciation")[(polProcessID %% 100L) %in% unique(x$processID) &
                             polProcessID %/% 100L %in% c(80L, 87L)]
  hs <- .movesr_expand_years(hs, "beginModelYearID", "endModelYearID")
  hs[, `:=`(pollutantID = polProcessID %/% 100L, processID = polProcessID %% 100L)]
  hs <- unique(hs[, .(pollutantID, processID, fuelSubtypeID, regClassID, modelYearID,
                      speciationConstant, oxySpeciation)],
               by = c("pollutantID", "processID", "fuelSubtypeID", "regClassID",
                      "modelYearID"), fromLast = TRUE)
  look_r <- function(sub) {
    mr[data.table::data.table(processID = x$processID, fuelSubtypeID = sub,
                              regClassID = x$regClassID, modelYearID = x$modelYearID),
       on = .(processID, fuelSubtypeID, regClassID, modelYearID), CH4THCRatio]
  }
  look_f <- function(pol, sub) {
    h <- hs[data.table::data.table(pollutantID = pol, processID = x$processID,
                                   fuelSubtypeID = sub, regClassID = x$regClassID,
                                   modelYearID = x$modelYearID),
            on = .(pollutantID, processID, fuelSubtypeID, regClassID, modelYearID)]
    ifelse(is.na(h$speciationConstant), 0,
           h$speciationConstant + h$oxySpeciation * x$volToWtPercentOxy *
             x$totalOxygenate)
  }
  x[, r := look_r(fuelSubtypeID)]
  x[, r10 := look_r(rep(12L, .N))]
  altcase <- x$fuelSubtypeID %in% 50:52 & x$processID %in% 1:2 &
    x$fuelTypeID == 5L & x$modelYearID >= 2001L
  has_r <- !is.na(x$r)
  x[, `:=`(c5 = ifelse(has_r, w * r, NA_real_),
           c79 = ifelse(has_r, w * (1 - r), NA_real_))]
  f80 <- look_f(80L, x$fuelSubtypeID)
  f87 <- look_f(87L, x$fuelSubtypeID)
  f80a <- look_f(80L, rep(12L, nrow(x)))
  f87a <- look_f(87L, rep(12L, nrow(x)))
  normal <- has_r & !altcase
  alt_ok <- altcase & !is.na(x$w_alt) & !is.na(x$r10)
  x[, c80 := ifelse(normal, w * (1 - r) * f80,
                    ifelse(alt_ok, w_alt * (1 - r10) * f80a, NA_real_))]
  x[, c87 := ifelse(normal, w * (1 - r) * f87,
                    ifelse(alt_ok, w_alt * (1 - r10) * f87a, NA_real_))]
  x[, c86 := ifelse(is.na(c80) & is.na(c5), NA_real_,
                    ifelse(is.na(c80), 0, c80) + ifelse(is.na(c5), 0, c5))]
  s <- function(v) if (all(is.na(v))) NA_real_ else sum(v, na.rm = TRUE)
  f <- x[, .(S5 = s(c5), S79 = s(c79), S80 = s(c80), S86 = s(c86), S87 = s(c87)),
         by = .(fuelTypeID, polProcessID, sourceTypeID, modelYearID, ageID,
                regClassID, processID)]
  f
}

.movesr_chain <- function(ctx, z, requested) {
  tb <- ctx$tb
  idk <- intersect(c("hourID", "temperature", "relHumidity", "sourceTypeID",
                     "regClassID", "fuelTypeID", "modelYearID", "ageID",
                     "ageGroupID", "roadTypeID", "avgSpeedBinID", "processID"),
                   names(z))
  keep <- c(idk, "pollutantID", "polProcessID", "rate", "base", "fuelFactor",
            "sbdTotal")
  z <- z[, intersect(keep, names(z)), with = FALSE]
  out <- list(z)
  add <- function(x, pol) {
    if (is.null(x) || !nrow(x)) return(invisible())
    x <- data.table::copy(x)
    x[, `:=`(pollutantID = pol, polProcessID = pol * 100L + processID)]
    out[[length(out) + 1L]] <<- x[, names(z), with = FALSE]
  }
  # HC speciation.
  if (any(requested %in% c(5L, 79L, 80L, 86L, 87L, 98L))) {
    thc <- z[pollutantID == 1L]
    if (nrow(thc)) {
      hf <- .movesr_hc_factors(ctx, thc)
      thc <- merge(thc, hf, by = c("fuelTypeID", "polProcessID", "sourceTypeID",
                                   "modelYearID", "ageID", "regClassID",
                                   "processID"))
      for (pol in c(5L, 79L, 80L, 87L, 86L)) {
        col <- paste0("S", pol)
        y <- thc[!is.na(get(col))]
        y[, rate := base * get(col)]
        add(y, pol)
      }
    }
  }
  sup <- NULL
  if (any(requested %in% c(90L, 98L, 31L))) {
    sup <- .movesr_supply_attributes(ctx)
    energy <- z[pollutantID == 91L]
  }
  # CO2AERunningStartExtendedIdleCalculator.
  if (any(requested %in% c(90L, 98L)) && nrow(energy)) {
    co <- sup[, .(C = sum(marketShare * carbonContent),
                  O = sum(marketShare * oxidationFraction)), by = fuelTypeID]
    y <- merge(energy, co, by = "fuelTypeID")
    y[, rate := rate * C * O * (44 / 12)]
    add(y[, !c("C", "O")], 90L)
  }
  if (98L %in% requested) {
    gwp <- tb("pollutant")[!is.na(globalWarmingPotential) &
                             globalWarmingPotential > 0, .(pollutantID,
                                                           globalWarmingPotential)]
    parts <- data.table::rbindlist(out, use.names = TRUE)[pollutantID %in% gwp$pollutantID]
    parts <- merge(parts, gwp, by = "pollutantID")
    sb <- parts[, .(sbdTotal = sbdTotal[1]), by = idk]
    y <- parts[, .(rate = sum(rate * globalWarmingPotential)), by = idk]
    y <- merge(y, sb, by = idk)
    y[, `:=`(base = NA_real_, fuelFactor = NA_real_)]
    add(y, 98L)
  }
  # SO2Calculator.
  if (31L %in% requested && nrow(energy)) {
    ws <- sup[, .(W = sum(marketShare * sulfurLevel, na.rm = TRUE),
                  EC = sum(marketShare * energyContent, na.rm = TRUE)), by = fuelTypeID]
    ser <- tb("sulfateemissionrate")[polProcessID %/% 100L == 31L]
    ser[, `:=`(processID = polProcessID %% 100L,
               minModelYearID = modelYearGroupID %/% 10000L,
               maxModelYearID = modelYearGroupID %% 10000L)]
    y <- merge(energy, ws, by = "fuelTypeID")
    y <- merge(y, ser[, .(processID, fuelTypeID, minModelYearID, maxModelYearID,
                          meanBaseRate)], by = c("processID", "fuelTypeID"),
               allow.cartesian = TRUE)
    y <- y[modelYearID >= minModelYearID & modelYearID <= maxModelYearID]
    y[, rate := meanBaseRate * W * rate / EC]
    add(y[, names(z), with = FALSE], 31L)
  }
  # SulfatePMCalculator and PM10EmissionCalculator.
  if (any(requested %in% c(100L, 110L, 111L, 112L, 115L, 118L, 119L))) {
    pm <- .movesr_sulfate_pm(ctx, z, idk)
    z <- z[!pollutantID %in% c(112L, 118L)]
    out[[1]] <- z
    for (pol in names(pm)) add(pm[[pol]], as.integer(pol))
  }
  # PM10BrakeTireCalculator: PM10 = PM2.5 * PM10PM25Ratio.
  for (pair in list(c(106L, 116L), c(107L, 117L))) {
    if (pair[1] %in% requested) {
      x <- z[pollutantID == pair[2]]
      r10 <- tb("pm10emissionratio")[polProcessID %/% 100L == pair[1]]
      r10[, processID := polProcessID %% 100L]
      y <- merge(x, r10[, .(processID, sourceTypeID, fuelTypeID, minModelYearID,
                            maxModelYearID, PM10PM25Ratio)],
                 by = c("processID", "sourceTypeID", "fuelTypeID"),
                 allow.cartesian = TRUE)
      y <- y[modelYearID >= minModelYearID & modelYearID <= maxModelYearID]
      y[, rate := rate * PM10PM25Ratio]
      add(y[, names(z), with = FALSE], pair[1])
    }
  }
  res <- data.table::rbindlist(out, use.names = TRUE)
  res[pollutantID %in% requested]
}

.movesr_sulfate_pm <- function(ctx, z, idk) {
  tb <- ctx$tb
  sup <- ctx$supply
  ec <- z[pollutantID == 112L]
  nec <- z[pollutantID == 118L]
  procs <- unique(c(ec$processID, nec$processID))
  sf <- tb("sulfatefractions")[processID %in% procs]
  # Fractions adjusted for the sulfur of each supplied formulation.
  x <- merge(sf, sup[, .(fuelTypeID, marketShare, sulfurLevel)], by = "fuelTypeID",
             allow.cartesian = TRUE)
  x[, adj := 1 + BaseFuelSulfateFraction * ((sulfurLevel / BaseFuelSulfurLevel) - 1)]
  x <- x[, .(fS = sum(marketShare * SulfatenonECPMFraction * adj),
             fH = sum(marketShare * H2OnonECPMFraction * adj),
             uS = SulfatenonECPMFraction[1], uH = H2OnonECPMFraction[1]),
         by = .(processID, fuelTypeID, sourceTypeID, minModelYearID, maxModelYearID)]
  cc <- tb("crankcaseemissionratio")[polProcessID %% 100L %in% procs &
                                       polProcessID %/% 100L %in% c(112L, 115L, 119L, 120L)]
  cc[, `:=`(pollutantID = polProcessID %/% 100L, processID = polProcessID %% 100L)]
  crank <- function(x, pol) {
    m <- merge(x, cc[pollutantID == pol, .(processID, sourceTypeID, regClassID,
                                            fuelTypeID, minModelYearID,
                                            maxModelYearID, crankcaseRatio)],
               by = c("processID", "sourceTypeID", "regClassID", "fuelTypeID"),
               allow.cartesian = TRUE)
    m <- m[modelYearID >= minModelYearID & modelYearID <= maxModelYearID]
    m[, rate := rate * crankcaseRatio]
    m[, !c("minModelYearID", "maxModelYearID", "crankcaseRatio")]
  }
  res <- list()
  ec_out <- crank(ec, 112L)
  res[["112"]] <- ec_out
  if (nrow(nec)) {
    y <- merge(nec, x, by = c("processID", "fuelTypeID", "sourceTypeID"),
               allow.cartesian = TRUE)
    y <- y[modelYearID >= minModelYearID & modelYearID <= maxModelYearID]
    y[, c("minModelYearID", "maxModelYearID") := NULL]
    # General fuel effects for NonECNonSO4PM (120).
    pp120 <- 12000L + y$processID
    keys <- unique(data.table::data.table(fuelTypeID = y$fuelTypeID, polProcessID = pp120,
                                          sourceTypeID = y$sourceTypeID,
                                          modelYearID = y$modelYearID, ageID = y$ageID))
    fa <- movesr_fuel_adjustment(sup, ctx$ratios, keys, ctx$gpa_fract)
    y[, polProcessID120 := 12000L + processID]
    y <- merge(y, fa[, .(fuelTypeID, polProcessID120 = polProcessID, sourceTypeID,
                         modelYearID, ageID, g120 = fuelFactor)],
               by = c("fuelTypeID", "polProcessID120", "sourceTypeID", "modelYearID",
                      "ageID"), all.x = TRUE)
    y[is.na(g120), g120 := 1]
    so4 <- data.table::copy(y)[, rate := rate * fS]
    h2o <- data.table::copy(y)[, rate := rate * fH]
    rest <- data.table::copy(y)[, rate := rate * pmax(1 - uH - uS, 0) * g120]
    drop <- c("fS", "fH", "uS", "uH", "g120", "polProcessID120")
    so4 <- crank(so4[, !drop, with = FALSE], 115L)
    h2o <- crank(h2o[, !drop, with = FALSE], 119L)
    rest <- crank(rest[, !drop, with = FALSE], 120L)
    res[["115"]] <- so4
    res[["119"]] <- h2o
    sumrows <- function(parts) {
      p <- data.table::rbindlist(parts, use.names = TRUE)
      if (!nrow(p)) return(p)
      sb <- p[, .(sbdTotal = sbdTotal[1]), by = idk]
      s <- p[, .(rate = sum(rate)), by = idk]
      s <- merge(s, sb, by = idk)
      s[, `:=`(base = NA_real_, fuelFactor = NA_real_, pollutantID = 0L,
               polProcessID = 0L)]
      s
    }
    res[["118"]] <- sumrows(list(rest, so4, h2o))
    res[["110"]] <- sumrows(list(ec_out, rest, so4, h2o))
    ps <- tb("pmspeciation")[processID %in% procs & inputPollutantID == 120L &
                               outputPollutantID == 111L]
    oc <- merge(rest, ps[, .(processID, sourceTypeID, fuelTypeID, minModelYearID,
                             maxModelYearID, pmSpeciationFraction)],
                by = c("processID", "sourceTypeID", "fuelTypeID"),
                allow.cartesian = TRUE)
    oc <- oc[modelYearID >= minModelYearID & modelYearID <= maxModelYearID]
    oc[, rate := rate * pmSpeciationFraction]
    res[["111"]] <- oc[, !c("minModelYearID", "maxModelYearID", "pmSpeciationFraction")]
    # PM10 total = PM2.5 total * PM10PM25Ratio.
    r10 <- tb("pm10emissionratio")[polProcessID %/% 100L == 100L]
    r10[, processID := polProcessID %% 100L]
    p10 <- merge(res[["110"]], r10[, .(processID, sourceTypeID, fuelTypeID,
                                       minModelYearID, maxModelYearID,
                                       PM10PM25Ratio)],
                 by = c("processID", "sourceTypeID", "fuelTypeID"),
                 allow.cartesian = TRUE)
    p10 <- p10[modelYearID >= minModelYearID & modelYearID <= maxModelYearID]
    p10[, rate := rate * PM10PM25Ratio]
    res[["100"]] <- p10[, !c("minModelYearID", "maxModelYearID", "PM10PM25Ratio")]
  }
  res
}
