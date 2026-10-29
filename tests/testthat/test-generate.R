fixture <- function() {
  dir <- tempfile()
  dir.create(dir)
  distance <- data.frame(
    MOVESScenarioID = "test", MOVESRunID = 1L, yearID = 2025L,
    monthID = 1L, dayID = 2L, hourID = 1L, linkID = 1L,
    pollutantID = 3L, processID = 1L, sourceTypeID = 21L,
    regClassID = 0L, SCC = "", fuelTypeID = 1L, modelYearID = 2020L,
    roadTypeID = 5L, avgSpeedBinID = c(10L, 11L),
    temperature = c(50, 68), relHumidity = 50,
    ratePerDistance = c(1.609344, 3.218688)
  )
  start <- data.frame(
    MOVESScenarioID = "test", MOVESRunID = 1L, yearID = 2025L,
    monthID = 1L, dayID = 2L, hourID = 1L, zoneID = 1L,
    pollutantID = 3L, processID = 2L, sourceTypeID = 21L,
    regClassID = 0L, SCC = "", fuelTypeID = 1L, modelYearID = 2020L,
    temperature = 68, relHumidity = 50, ratePerStart = 0.5
  )
  data.table::fwrite(distance, file.path(dir, "rateperdistance.csv"))
  data.table::fwrite(start, file.path(dir, "rateperstart.csv"))
  dir
}

test_that("factor tables are read and distance units are converted", {
  f <- movesr_factors(fixture(), distance_units = "mile")
  expect_s3_class(f, "movesr_factors")
  d <- f$data[f$data$basis == "distance"]
  expect_equal(d$rate, c(1, 2), tolerance = 1e-12)
  expect_equal(d$units, c("g/km", "g/km"))
  expect_equal(d$temperature_C, c(10, 20))
})

test_that("distance factors select bins and temperature", {
  f <- movesr_factors(fixture(), distance_units = "mile")
  s <- movesr_scenario(2025, 21, 1, 2020, 3, 1, 5,
    speed_bin_id = 10, temperature_c = 10, hour_id = 1)
  out <- movesr_generate(s, f)
  expect_equal(out$rate, 1, tolerance = 1e-12)
  expect_equal(out$requested_speed_kmh, NULL)
  out <- movesr_generate(
    movesr_scenario(2025, 21, 1, 2020, 3, 1, 5,
      temperature_c = 10, hour_id = 1),
    f, speed_kmh = 72.42048
  )
  expect_equal(out$rate, 1, tolerance = 1e-12)
  expect_equal(out$requested_speed_kmh, 72.42048)
})

test_that("start factors are event-based and do not require speed", {
  f <- movesr_factors(fixture())
  s <- movesr_scenario(2025, 21, 1, 2020, 3, 2, road_type_id = NULL,
    temperature_c = 20, hour_id = 1)
  out <- movesr_generate(s, f)
  expect_equal(out$rate, 0.5)
  expect_equal(out$units, "g/start")
  expect_error(movesr_generate(s, f, speed_kmh = 50), "only valid")
})

test_that("missing or ambiguous scenarios fail explicitly", {
  f <- movesr_factors(fixture())
  s <- movesr_scenario(2025, 21, 1, 2020, 3, 1, 5,
    temperature_c = 25, hour_id = 1)
  expect_error(movesr_generate(s, f, speed_kmh = 50), "No factors")
  s <- movesr_scenario(2025, 21, 1, 2020, 3, 1, 5,
    temperature_c = 10, hour_id = 1)
  expect_error(movesr_generate(s, f), "require speed")
  expect_error(movesr_generate(s, f, speed_kmh = 200), "75 mph")
})
