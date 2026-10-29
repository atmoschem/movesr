.movesr_test_db <- function() {
  path <- Sys.getenv("MOVESR_DB")
  if (!nzchar(path) || !dir.exists(path)) return(NULL)
  if (!nzchar(Sys.which(getOption("movesr.duckdb", Sys.getenv("DUCKDB", "duckdb")))) &&
    !file.exists(getOption("movesr.duckdb", Sys.getenv("DUCKDB", "")))) {
    return(NULL)
  }
  movesr_database(path)
}
