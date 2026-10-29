#' Open a portable MOVES database catalog
#'
#' @description Opens a directory produced by the MOVES SQL-to-Parquet
#' conversion. The directory contains one Parquet file per MOVES table and a
#' `catalog.csv` with table names, row counts and checksums. No database server
#' is started.
#' @param path Directory containing `catalog.csv` and the Parquet tables.
#' @return A `movesr_database` object.
#' @export
movesr_database <- function(path) {
  catalog_path <- file.path(path, "catalog.csv")
  if (!file.exists(catalog_path)) {
    stop("catalog.csv not found in ", path)
  }
  catalog <- data.table::fread(catalog_path, showProgress = FALSE)
  required <- c("table", "rows", "parquet_rows")
  if (!all(required %in% names(catalog))) {
    stop("catalog.csv must contain: ", paste(required, collapse = ", "))
  }
  if (any(catalog$rows != catalog$parquet_rows)) {
    stop("Catalog row counts do not match the exported Parquet row counts")
  }
  missing <- catalog$table[!file.exists(file.path(path, paste0(catalog$table, ".parquet")))]
  if (length(missing)) stop("Missing Parquet tables: ", paste(missing, collapse = ", "))
  structure(list(path = path, catalog = catalog),
            class = "movesr_database")
}

#' @export
print.movesr_database <- function(x, ...) {
  cat("movesr database:", nrow(x$catalog), "tables\n")
  cat("path:", x$path, "\n")
  invisible(x)
}

#' List tables in a portable MOVES database
#'
#' @param database A `movesr_database` object.
#' @return A data.table with table names and row counts.
#' @export
movesr_tables <- function(database) {
  if (!inherits(database, "movesr_database")) {
    stop("database must be created by movesr_database()")
  }
  data.table::copy(database$catalog)
}

.movesr_table_cache <- new.env(parent = emptyenv())

#' Read one MOVES table from a portable database
#'
#' @param database A `movesr_database` object.
#' @param table Table name from \code{\link{movesr_tables}}.
#' @param cache Logical; keep the table in memory for repeated scenario runs.
#' @return A data.table.
#' @export
movesr_table <- function(database, table, cache = TRUE) {
  if (!inherits(database, "movesr_database")) {
    stop("database must be created by movesr_database()")
  }
  if (!table %in% database$catalog$table) {
    stop("Unknown MOVES table: ", table)
  }
  key <- paste(normalizePath(database$path, mustWork = TRUE), table, sep = "::")
  if (cache && exists(key, .movesr_table_cache, inherits = FALSE)) {
    return(get(key, .movesr_table_cache, inherits = FALSE))
  }
  out <- .movesr_read_parquet(file.path(database$path, paste0(table, ".parquet")))
  if (cache) assign(key, out, .movesr_table_cache)
  out
}
