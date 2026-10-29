test_that("TVV equations keep SQL NULL from sqrt of a negative number", {
  v <- movesr:::.tvv_eval("greatest(0,(-(-1*Xn+85)+sqrt(((-1*Xn+85)*(-1*Xn+85))-(4*1.25)*(-0.25*Xn*Xn+.2*Xn+70)))/(2*1.25))",
    c(0, 50), c(1, 1))
  expect_equal(length(v), 2L)
  expect_true(all(is.na(v) | v >= 0))
  expect_equal(movesr:::.tvv_eval("0.796*TVGdaily", 1, 2), 1.592)
  expect_equal(movesr:::.tvv_eval("0", c(1, 2), c(1, 1)), c(0, 0))
})

test_that("evap operating-mode fractions follow soak fractions", {
  saf <- data.table::data.table(sourceTypeID = 21L, hourDayID = 165L,
    opModeID = c(150L, 151L),
    soakActivityFraction = c(0.1, 0.9))
  x <- movesr:::.evap_opmode_fractions(saf, 0)
  expect_equal(x[opModeID == 300L]$opModeFraction, 0)
  y <- movesr:::.evap_opmode_fractions(saf, 0.25)
  expect_equal(sort(y$opModeFraction), sort(c(0.075, 0.675, 0.25)))
  z <- movesr:::.evap_opmode_fractions(saf, 1)
  expect_equal(z[opModeID == 300L]$opModeFraction, 1)
})

.evap_tank <- local({
  cache <- NULL
  function(db) {
    if (is.null(cache)) {
      met <- movesr_default_meteorology(db, 11001, 7, 1:24)
      cache <<- movesr_tank_temperature(db, met$temperature, day_id = 5L)
    }
    cache
  }
})

test_that("evap rates reproduce MOVES (DC, 24 hours, 11/12/13)", {
  db <- .movesr_test_db()
  ref <- Sys.getenv("MOVESR_DMV_EVAP_REFERENCE")
  skip_if(is.null(db) || !file.exists(file.path(ref, "rateperprofile.csv.gz")),
    "Set MOVESR_DB, DUCKDB and MOVESR_DMV_EVAP_REFERENCE (exports-dmv-evap24)")
  x <- movesr_evap(db, 11001, 2025, 7, 5L, tank = .evap_tank(db),
    source_type_id = c(21, 32), pollutant_id = c(1, 79, 87))
  r <- rbind(data.table::fread(file.path(ref, "ratepervehicle.csv.gz")),
    data.table::fread(file.path(ref, "rateperprofile.csv.gz")), fill = TRUE)
  r <- r[sourceTypeID %in% c(21, 32) & pollutantID %in% c(1, 79, 87)]
  k <- c("hourID", "sourceTypeID", "fuelTypeID", "modelYearID", "pollutantID",
    "processID")
  m <- merge(r[, c(k, "ratePerVehicle"), with = FALSE],
    x[, c(k, "ratePerVehicle"), with = FALSE], by = k, all = TRUE)
  expect_false(anyNA(m$ratePerVehicle.x))
  expect_false(anyNA(m$ratePerVehicle.y))
  d <- abs(m$ratePerVehicle.y / m$ratePerVehicle.x - 1)
  expect_lt(max(d[m$processID != 12L | m$fuelTypeID == 1L]), 1e-4)
  expect_lt(max(d), 2e-3)
})

test_that("evap rates per distance reproduce MOVES (DC, hour 16)", {
  db <- .movesr_test_db()
  ref <- Sys.getenv("MOVESR_DMV_REFERENCE")
  skip_if(is.null(db) || !file.exists(file.path(ref, "rateperdistance.csv.gz")),
    "Set MOVESR_DB, DUCKDB and MOVESR_DMV_REFERENCE for DMV parity tests")
  x <- movesr_evap_running_rates(db, 11001, 2025, 7, 5L, hour_id = 16,
    tank = .evap_tank(db), source_type_id = 21,
    pollutant_id = c(1, 80), e85_tank_properties = "e10")
  r <- data.table::fread(file.path(ref, "rateperdistance.csv.gz"))
  r <- r[linkID %/% 10000 == 11001 & hourID == 16 & sourceTypeID == 21 &
    processID %in% 11:13 & pollutantID %in% c(1, 80)]
  k <- c("sourceTypeID", "fuelTypeID", "modelYearID", "pollutantID",
    "processID", "avgSpeedBinID")
  m <- merge(r[, c(k, "ratePerDistance"), with = FALSE],
    x[, c(k, "ratePerDistance"), with = FALSE], by = k)
  expect_equal(nrow(m), nrow(r))
  expect_lt(max(abs(m$ratePerDistance.y / m$ratePerDistance.x - 1)), 5e-5)
})

test_that("simplified evap method is close to MOVES (DC, 24 hours)", {
  db <- .movesr_test_db()
  ref <- Sys.getenv("MOVESR_DMV_EVAP_REFERENCE")
  skip_if(is.null(db) || !file.exists(file.path(ref, "rateperprofile.csv.gz")),
    "Set MOVESR_DB, DUCKDB and MOVESR_DMV_EVAP_REFERENCE (exports-dmv-evap24)")
  x <- movesr_evap(db, 11001, 2025, 7, 5L, method = "simplified", source_type_id = 21)
  r <- rbind(data.table::fread(file.path(ref, "ratepervehicle.csv.gz")),
    data.table::fread(file.path(ref, "rateperprofile.csv.gz")), fill = TRUE)
  r <- r[sourceTypeID == 21 & pollutantID == 1]
  k <- c("hourID", "fuelTypeID", "modelYearID", "processID")
  m <- merge(r[, c(k, "ratePerVehicle"), with = FALSE],
    x[, c(k, "ratePerVehicle"), with = FALSE], by = k)
  expect_equal(nrow(m), nrow(r))
  tot <- m[, .(d = sum(ratePerVehicle.y) / sum(ratePerVehicle.x) - 1), by = processID]
  expect_lt(max(abs(tot[processID != 12L]$d)), 0.005)
  expect_lt(abs(tot[processID == 12L]$d), 0.05)
})

test_that("simplified tank model keeps fractions consistent", {
  db <- .movesr_test_db()
  skip_if(is.null(db), "Set MOVESR_DB and DUCKDB")
  t <- 70 + 10 * sin((1:24 - 9) / 24 * 2 * pi)
  x <- movesr_tank_temperature_simple(db, t, day_id = 2L)
  s <- x$soak_activity_fraction[, .(s = sum(soakActivityFraction)), by = .(sourceTypeID, hourDayID)]
  expect_true(all(abs(s$s - 1) < 1e-9))
  ci <- x$cold_soak_initial_hour_fraction[, .(s = sum(coldSoakInitialHourFraction)),
    by = .(sourceTypeID, hourDayID)]
  expect_true(all(abs(ci$s - 1) < 1e-9))
  expect_equal(x$cold_soak$coldSoakTankTemperature,
    movesr_cold_soak_tank_temperature(t)[timeStepID == 1L]$quarterHourTankTemperature)
})
