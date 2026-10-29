test_that("temperature adjustment uses the MOVES quadratic", {
  rates <- data.frame(pollutantID = 3L, processID = 1L, fuelTypeID = 1L,
    regClassID = 1L, modelYearID = 2020L,
    polProcessID = 301L, emission_rate = 10)
  ta <- data.frame(polProcessID = 301L, fuelTypeID = 1L, regClassID = 1L,
    minModelYearID = 2000L, maxModelYearID = 2025L,
    tempAdjustTermA = 0.01, tempAdjustTermB = 0)
  out <- movesr_temperature_adjustment(rates, ta, temperature = 95,
    fuel_type_id = 1)
  expect_equal(out$temperature_factor, 1.2)
  expect_equal(out$emission_rate, 12)
})

test_that("diesel NOx has the MOVES cold-temperature branch", {
  rates <- data.frame(pollutantID = 3L, processID = 1L, fuelTypeID = 2L,
    regClassID = 1L, modelYearID = 2020L,
    polProcessID = 302L, emission_rate = 10)
  ta <- data.frame(polProcessID = 302L, fuelTypeID = 2L, regClassID = 1L,
    minModelYearID = 2000L, maxModelYearID = 2025L,
    tempAdjustTermA = 0.01, tempAdjustTermB = 0)
  out <- movesr_temperature_adjustment(rates, ta, temperature = 67,
    fuel_type_id = 2)
  expect_equal(out$temperature_factor, 1.1)
  out <- movesr_temperature_adjustment(rates, ta, temperature = 90,
    fuel_type_id = 2)
  expect_equal(out$temperature_factor, 1)
})

test_that("NOx humidity correction is bounded", {
  rates <- data.frame(pollutantID = 3L, processID = 1L, fuelTypeID = 1L,
    regClassID = 1L, modelYearID = 2020L,
    polProcessID = 301L, emission_rate = 10)
  ta <- data.frame(polProcessID = 301L, fuelTypeID = 1L, regClassID = 1L,
    minModelYearID = 2000L, maxModelYearID = 2025L,
    tempAdjustTermA = 0, tempAdjustTermB = 0)
  nha <- data.frame(fuelTypeID = 1L, humidityNOxEq = "CFR 86",
    humidityTermA = 0.01, humidityTermB = 0,
    humidityLowBound = 5, humidityUpBound = 15)
  out <- movesr_temperature_adjustment(rates, ta, temperature = 75,
    fuel_type_id = 1,
    humidity_adjustment = nha,
    specific_humidity = 20)
  expect_equal(out$humidity_factor, 1 - 0.01 * (15 - 10.71))
})
