test_that("speed bins are bracketed by lower and higher schedules", {
  ds <- data.frame(driveScheduleID = 1:3, averageSpeed = c(10, 20, 30))
  assoc <- data.frame(sourceTypeID = 21L, roadTypeID = 5L,
    driveScheduleID = 1:3)
  bins <- data.frame(avgSpeedBinID = 1:2, avgBinSpeed = c(15, 20))
  out <- movesr_schedule_fractions(ds, assoc, bins)
  expect_equal(out[avgSpeedBinID == 1 & driveScheduleID == 1]$driveScheduleFraction, 0.5)
  expect_equal(out[avgSpeedBinID == 1 & driveScheduleID == 2]$driveScheduleFraction, 0.5)
  expect_equal(out[avgSpeedBinID == 2]$driveScheduleID, 2L)
  expect_equal(out[avgSpeedBinID == 2]$driveScheduleFraction, 1)
  expect_equal(out[, sum(driveScheduleFraction), by = avgSpeedBinID]$V1, c(1, 1))
})

test_that("outside bins use the nearest schedule", {
  ds <- data.frame(driveScheduleID = 1:2, averageSpeed = c(10, 20))
  assoc <- data.frame(sourceTypeID = 21L, roadTypeID = 5L,
    driveScheduleID = 1:2)
  bins <- data.frame(avgSpeedBinID = 1:2, avgBinSpeed = c(5, 25))
  out <- movesr_schedule_fractions(ds, assoc, bins)
  expect_equal(out[avgSpeedBinID == 1]$driveScheduleID, 1L)
  expect_equal(out[avgSpeedBinID == 2]$driveScheduleID, 2L)
})
