test_that("scenario vectors expand to one row per combination", {
  s <- movesr_scenario(
    year = 2025,
    source_type_id = c(21, 31),
    fuel_type_id = c(1, 2),
    model_year = c(2020, 2021),
    pollutant_id = 3
  )
  grid <- movesr_expand_scenario(s)
  expect_equal(nrow(grid), 8)
  expect_equal(sort(unique(grid$source_type_id)), c(21, 31))
  expect_equal(sort(unique(grid$fuel_type_id)), c(1, 2))
  expect_equal(sort(unique(grid$model_year)), c(2020, 2021))
})
