test_that("MOVES meteorology reproduces heat index and humidity", {
  m <- movesr_meteorology(68, 50, 29.095)
  expect_equal(m$heatIndex, 68)
  expect_equal(m$specificHumidity, 7.457, tolerance = 1e-3)
  hot <- movesr_meteorology(95, 60, 28.94)
  expect_true(hot$heatIndex > 95)
  expect_lte(hot$heatIndex, 120)
})

test_that("NOx humidity K follows the CFR 86 and CFR 1065 equations", {
  nha <- data.frame(fuelTypeID = c(1L, 2L),
    humidityNOxEq = c("CFR 86", "CFR 1065"),
    humidityTermA = c(0.0329, 9.953),
    humidityTermB = c(NA, 0.832),
    humidityLowBound = c(3, 0.002),
    humidityUpBound = c(17.71, 0.035))
  k <- movesr_nox_humidity(c(1L, 2L, 9L), 10.71, 0.0169, nha)
  expect_equal(k[1], 1)
  expect_equal(k[2], 1 / (9.953 * 0.0169 + 0.832))
  expect_equal(k[3], 1)
  expect_equal(movesr_nox_humidity(1L, 50, 0, nha),
    1 - 0.0329 * (17.71 - 10.71))
})

test_that("MySQL fuel-effect expressions are evaluated like MOVES", {
  ff <- data.frame(fuelTypeID = 1L, fuelFormulationID = 1:2,
    sulfurLevel = c(10, 40), ETOHVolume = 10,
    aromaticContent = 20, RVP = 9, T50 = 200, T90 = 320)
  expect_equal(movesr:::.movesr_eval_expression(
    "if(sulfurLevel>30, 2, least(3, ETOHVolume))", ff), c(3, 2))
  expect_equal(movesr:::.movesr_eval_expression(
    "IF(ETOHVolume >=6.0 AND ETOHVolume < 12, ln(exp(1)), 0)", ff), c(1, 1))
  expect_equal(movesr:::.movesr_eval_expression("ln(0)", ff), c(1, 1))
  ex <- data.frame(polProcessID = 301L, fuelTypeID = 1L, minModelYearID = 2001L,
    maxModelYearID = 2060L, minAgeID = 0L, maxAgeID = 40L,
    sourceTypeID = 21L,
    fuelEffectRatioExpression = "1+0.01*ETOHVolume",
    fuelEffectRatioGPAExpression = "")
  r <- movesr_fuel_ratios(ex, ff)
  expect_equal(r$fuelEffectRatio, c(1.1, 1.1))
  expect_equal(r$fuelEffectRatioGPA, c(1, 1))
})

test_that("fuel adjustment weights formulations by market share", {
  supply <- data.frame(fuelTypeID = 1L, fuelFormulationID = 1:2,
    marketShare = c(0.25, 0.75))
  ratios <- data.frame(fuelTypeID = 1L, fuelFormulationID = 1L,
    polProcessID = 301L, sourceTypeID = 21L,
    minModelYearID = 2001L, maxModelYearID = 2060L,
    minAgeID = 0L, maxAgeID = 40L,
    fuelEffectRatio = 2, fuelEffectRatioGPA = 3)
  keys <- data.frame(fuelTypeID = 1L, polProcessID = 301L, sourceTypeID = 21L,
    modelYearID = 2020L, ageID = 5L)
  a <- movesr_fuel_adjustment(supply, ratios, keys)
  expect_equal(a$fuelFactor, 0.25 * 2 + 0.75)
  b <- movesr_fuel_adjustment(supply, ratios, keys, gpa_fract = 1)
  expect_equal(b$fuelFactor, 0.25 * 3 + 0.75)
})

test_that("start temperature adjustment is additive and opmode weighted", {
  r <- movesr:::.movesr_start_temp(-0.15, 0, 0, FALSE, 202L, 50, 0.5, 10)
  expect_equal(r, 10 + 0.5 * (50 - 75) * -0.15)
  r <- movesr:::.movesr_start_temp(0.1, 2, 1, TRUE, 302L, 80, 0.5, 10)
  expect_equal(r, 10 + 0.5 * (2 * exp(0) + 1))
  r <- movesr:::.movesr_start_temp(NA, NA, NA, FALSE, 202L, 50, 0.5, 10)
  expect_equal(r, 10)
})

test_that("Complex Model expressions gain standard deviations", {
  e <- movesr:::.cm_translate("(cmp.coeff*(fp.Oxygen-fp_Oxygen.center)^2)",
    stddev = TRUE)
  expect_equal(e, "(coeff*((f_oxygen-c_oxygen)/s_oxygen)^2)")
  e <- movesr:::.cm_translate("(cmp.coeff*(0.3488*fp.Ethanol-fp_Ethanol.center))")
  expect_equal(e, "(coeff*(0.3488*f_ethanol-c_ethanol))")
})

test_that("high-ethanol fuels take E10 properties", {
  x <- data.frame(fuelFormulationID = 1L, fuelSubtypeID = 51L, RVP = 10.5,
    sulfurLevel = 8, ETOHVolume = 74, MTBEVolume = 0,
    ETBEVolume = 0, TAMEVolume = 0, aromaticContent = 0,
    T50 = 999, T90 = 999)
  e10 <- data.frame(fuelRegionID = c(0L, 7L), fuelYearID = 2025L,
    monthGroupID = 1L, RVP = c(13, 12), sulfurLevel = c(NA, 7),
    ETOHVolume = c(NA, 10), aromaticContent = c(18, 16),
    T50 = c(158, 150), T90 = c(308, 316))
  y <- movesr:::.movesr_high_ethanol(x, e10, 7L, 2025L, 1L)
  expect_equal(y$RVP, 10.5)
  expect_equal(y$altRVP, 12)
  expect_equal(y$ETOHVolume, 10)
  expect_equal(y$T50, 150)
})

test_that("running rates reproduce MOVES reference factors", {
  db <- .movesr_test_db()
  ref <- Sys.getenv("MOVESR_REFERENCE")
  skip_if(is.null(db) || !file.exists(file.path(ref, "rateperdistance.csv")),
    "Set MOVESR_DB, DUCKDB and MOVESR_REFERENCE for parity tests")
  r <- movesr_running_rates(db, source_type_id = 21, road_type_id = 5,
    meteorology = data.frame(hourID = 1:3,
      temperature = c(68, 50, 86),
      relHumidity = 50))
  x <- data.table::fread(file.path(ref, "rateperdistance.csv"))[sourceTypeID == 21]
  k <- c("hourID", "sourceTypeID", "fuelTypeID", "modelYearID", "pollutantID",
    "roadTypeID", "avgSpeedBinID")
  m <- merge(x[, c(k, "ratePerDistance"), with = FALSE],
    r[, c(k, "ratePerDistance"), with = FALSE], by = k)
  expect_equal(nrow(m), nrow(x))
  d <- ifelse(m$ratePerDistance.x == 0, m$ratePerDistance.y,
    m$ratePerDistance.y / m$ratePerDistance.x - 1)
  expect_lt(max(abs(d)), 1e-5)
})

test_that("start rates reproduce MOVES reference factors", {
  db <- .movesr_test_db()
  ref <- Sys.getenv("MOVESR_REFERENCE")
  skip_if(is.null(db) || !file.exists(file.path(ref, "rateperstart.csv")),
    "Set MOVESR_DB, DUCKDB and MOVESR_REFERENCE for parity tests")
  r <- movesr_start_rates(db, day_id = 2L, source_type_id = c(21, 43),
    meteorology = data.frame(hourID = 1:3,
      temperature = c(68, 50, 86),
      relHumidity = 50))
  x <- data.table::fread(file.path(ref, "rateperstart.csv"))[sourceTypeID %in% c(21, 43)]
  k <- c("hourID", "sourceTypeID", "fuelTypeID", "modelYearID", "pollutantID")
  m <- merge(x[, c(k, "ratePerStart"), with = FALSE],
    r[, c(k, "ratePerStart"), with = FALSE], by = k)
  expect_equal(nrow(m), nrow(x))
  d <- ifelse(m$ratePerStart.x == 0, m$ratePerStart.y,
    m$ratePerStart.y / m$ratePerStart.x - 1)
  expect_lt(max(abs(d)), 1e-5)
})

test_that("region helpers return the DMV counties", {
  r <- movesr_region("dmv")
  expect_equal(nrow(r), 15)
  expect_true(all(c(11001L, 24031L, 51059L) %in% r$countyID))
  expect_equal(movesr_region(c(11001, 24031))$countyID, c(11001L, 24031L))
  expect_error(movesr_region("nowhere"), "Unknown region")
})

test_that("pollutant plan maps chained pollutants to base pollutants", {
  p <- movesr:::.movesr_pollutant_plan(c(87, 98, 110))
  expect_true(all(c(1L, 6L, 91L, 112L, 118L) %in% p$base))
  expect_error(movesr:::.movesr_pollutant_plan(20), "not supported")
})

test_that("cold-soak tank temperature follows the MOVES recurrence", {
  x <- movesr_cold_soak_tank_temperature(rep(70, 24))
  expect_equal(nrow(x), 96)
  expect_true(all(abs(x$quarterHourTankTemperature - 70) < 1e-4))
})

test_that("DMV rates reproduce MOVES for all pollutants", {
  db <- .movesr_test_db()
  ref <- Sys.getenv("MOVESR_DMV_REFERENCE")
  skip_if(is.null(db) || !file.exists(file.path(ref, "rateperdistance.csv.gz")),
    "Set MOVESR_DB, DUCKDB and MOVESR_DMV_REFERENCE for DMV parity tests")
  pols <- c(1, 2, 3, 5, 6, 30, 31, 79, 80, 86, 87, 90, 91, 98, 100, 110, 111,
    112, 115, 118, 119)
  r <- movesr_region_rates(db, 11001, process = c("running", "brake", "tire"),
    month_id = 7, hour_id = 16, source_type_id = c(21, 42),
    road_type_id = 5, pollutant_id = pols)
  mine <- data.table::rbindlist(r, fill = TRUE)
  x <- data.table::fread(file.path(ref, "rateperdistance.csv.gz"))
  x <- x[linkID %/% 10000 == 11001 & hourID == 16 & sourceTypeID %in% c(21, 42) &
    processID %in% c(1, 9, 10)]
  k <- c("sourceTypeID", "fuelTypeID", "modelYearID", "pollutantID",
    "processID", "avgSpeedBinID")
  m <- merge(x[, c(k, "ratePerDistance"), with = FALSE],
    mine[, c(k, "ratePerDistance"), with = FALSE], by = k)
  expect_equal(nrow(m), nrow(x))
  d <- ifelse(m$ratePerDistance.x == 0, m$ratePerDistance.y,
    m$ratePerDistance.y / m$ratePerDistance.x - 1)
  expect_lt(max(abs(d)), 2e-5)
})
