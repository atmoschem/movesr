test_that("source bins follow the MOVES ID formula and distributions sum", {
  sample <- data.frame(
    sourceTypeModelYearID = c(1L, 1L, 2L),
    sourceTypeID = c(21L, 21L, 21L),
    modelYearID = c(2020L, 2020L, 2019L),
    fuelTypeID = c(1L, 2L, 1L),
    engTechID = c(1L, 2L, 1L),
    regClassID = c(1L, 2L, 1L),
    stmyFraction = c(0.6, 0.4, 1)
  )
  ppmy <- data.frame(
    polProcessID = c(301L, 301L),
    modelYearID = c(2020L, 2019L),
    modelYearGroupID = c(1L, 1L)
  )
  groups <- data.frame(modelYearGroupID = 1L, shortModYrGroupID = 3L)
  out <- movesr_sourcebins(sample, ppmy, groups, year = 2025, process_id = 1)
  expect_equal(nrow(out$source_bin), 2)
  expect_equal(out$source_bin$sourceBinID,
    c("1010101030000000000", "1020202030000000000"))
  expect_equal(sum(out$distribution$sourceBinActivityFraction), 2)
  expect_equal(out$distribution$sourceBinActivityFraction, c(0.6, 0.4, 1))
})
