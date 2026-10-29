test_that("running rates combine source bins and operating modes", {
  er <- data.frame(
    sourceBinID = c("1", "1", "2", "2"),
    polProcessID = 301L,
    opModeID = c(1L, 11L, 1L, 11L),
    ageGroupID = 1L,
    meanBaseRate = c(10, 20, 30, 40)
  )
  sb <- data.frame(sourceBinID = c("1", "2"), fuelTypeID = 1L,
    regClassID = 1L)
  sbd <- data.frame(sourceTypeModelYearID = 1L,
    polProcessID = 301L,
    sourceBinID = c("1", "2"),
    sourceBinActivityFraction = c(0.5, 0.5))
  stmy <- data.frame(sourceTypeModelYearID = 1L, sourceTypeID = 21L,
    modelYearID = 2020L)
  om <- data.frame(sourceTypeID = 21L, roadTypeID = 5L, avgSpeedBinID = 1L,
    polProcessID = 301L,
    opModeID = c(1L, 11L),
    opModeFraction = c(0.25, 0.75))
  bins <- data.frame(avgSpeedBinID = 1L, avgBinSpeed = 10)
  out <- movesr_running(er, sb, sbd, stmy, om, bins)
  # Weighted mean rates: 20 and 30 for opModes 1 and 11.
  # (0.25 * 20 + 0.75 * 30) / 10 = 2.75
  expect_equal(out$emission_rate, 2.75)
  expect_equal(out$pollutantID, 3L)
  expect_equal(out$units, "g/mile")
})
