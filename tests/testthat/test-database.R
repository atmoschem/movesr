test_that("portable database catalogs are validated", {
  dir <- tempfile()
  dir.create(dir)
  catalog <- data.frame(table = c("avgspeedbin", "sourcebin"),
                         rows = c(16L, 100L),
                         parquet_rows = c(16L, 100L))
  data.table::fwrite(catalog, file.path(dir, "catalog.csv"))
  file.create(file.path(dir, c("avgspeedbin.parquet", "sourcebin.parquet")))
  db <- movesr_database(dir)
  expect_s3_class(db, "movesr_database")
  expect_equal(nrow(movesr_tables(db)), 2)
  expect_error(movesr_table(db, "missing"), "Unknown")
  catalog$parquet_rows[1] <- 15L
  data.table::fwrite(catalog, file.path(dir, "catalog.csv"))
  expect_error(movesr_database(dir), "row counts")
})
