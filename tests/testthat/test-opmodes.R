test_that("operating modes are assigned from VSP and speed", {
  dss <- data.frame(driveScheduleID = 1L, second = 1:3, speed = c(3, 3, 3))
  ds <- data.frame(driveScheduleID = 1L, averageSpeed = 3)
  physics <- data.frame(sourceTypeID = 21L, rollingTermA = 0,
    rotatingTermB = 0, dragTermC = 0, sourceMass = 1,
    fixedMassFactor = 1)
  modes <- data.frame(opModeID = c(1L, 11L, 12L),
    VSPLower = c(NA, 0, 1), VSPUpper = c(NA, 1, 2),
    speedLower = c(NA, 1, 1), speedUpper = c(NA, 100, 100))
  pp <- data.frame(polProcessID = 301L, opModeID = c(1L, 11L, 12L))
  assoc <- data.frame(sourceTypeID = 21L, roadTypeID = 5L, driveScheduleID = 1L)
  fraction <- data.frame(sourceTypeID = 21L, roadTypeID = 5L,
    avgSpeedBinID = 1L, driveScheduleID = 1L,
    driveScheduleFraction = 1)
  out <- movesr_opmodes(dss, ds, physics, modes, pp, assoc, fraction)
  expect_equal(sum(out$opModeFraction), 1)
  expect_equal(out[opModeID == 11L]$opModeFraction, 1)
})

test_that("braking seconds receive operating mode zero", {
  dss <- data.frame(driveScheduleID = 1L, second = 1:4,
    speed = c(30, 20, 10, 0))
  ds <- data.frame(driveScheduleID = 1L, averageSpeed = 15)
  physics <- data.frame(sourceTypeID = 21L, rollingTermA = 0,
    rotatingTermB = 0, dragTermC = 0, sourceMass = 1,
    fixedMassFactor = 1)
  modes <- data.frame(opModeID = c(1L, 11L),
    VSPLower = c(NA, 0), VSPUpper = c(NA, 100),
    speedLower = c(NA, 1), speedUpper = c(NA, 100))
  pp <- data.frame(polProcessID = 301L, opModeID = c(0L, 1L, 11L))
  assoc <- data.frame(sourceTypeID = 21L, roadTypeID = 5L, driveScheduleID = 1L)
  fraction <- data.frame(sourceTypeID = 21L, roadTypeID = 5L,
    avgSpeedBinID = 1L, driveScheduleID = 1L,
    driveScheduleFraction = 1)
  out <- movesr_opmodes(dss, ds, physics, modes, pp, assoc, fraction)
  expect_true(any(out$opModeID == 0L))
})
