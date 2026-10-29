.movesr_duckdb <- function() {
  path <- getOption("movesr.duckdb", Sys.getenv("DUCKDB", "duckdb"))
  found <- Sys.which(path)
  if (!nzchar(found) && !file.exists(path)) {
    stop("DuckDB CLI not found. Set options(movesr.duckdb = 'path/to/duckdb')")
  }
  if (!nzchar(found)) path else found
}

.movesr_read_parquet <- function(path) {
  duckdb <- .movesr_duckdb()
  output <- tempfile(fileext = ".csv")
  on.exit(unlink(output), add = TRUE)
  src <- gsub("'", "''", normalizePath(path, mustWork = TRUE))
  dst <- gsub("'", "''", output)
  sql <- paste0("COPY (SELECT * FROM read_parquet('", src,
                "')) TO '", dst, "' (HEADER, DELIMITER ',')")
  status <- system2(duckdb, c("-no-agent", "-csv", "-c", shQuote(sql)),
                    stdout = TRUE, stderr = TRUE)
  if (!file.exists(output)) {
    stop("DuckDB could not export Parquet: ", paste(status, collapse = "\n"))
  }
  data.table::as.data.table(data.table::fread(output, showProgress = FALSE))
}

.movesr_read <- function(path) {
  if (!file.exists(path)) stop("File does not exist: ", path)
  if (grepl("[.]parquet$", path, ignore.case = TRUE)) {
    return(.movesr_read_parquet(path))
  }
  data.table::as.data.table(data.table::fread(path, showProgress = FALSE))
}

.movesr_basis <- function(path, data) {
  if ("ratePerDistance" %in% names(data)) return("distance")
  if ("ratePerStart" %in% names(data)) return("start")
  if ("ratePerVehicle" %in% names(data)) {
    if (grepl("profile", basename(path), ignore.case = TRUE) ||
        "temperatureProfileID" %in% names(data)) return("profile")
    return("vehicle")
  }
  stop("Cannot infer MOVES rate basis for ", path)
}

.movesr_one <- function(path, basis = NULL, distance_units = c("mile", "km")) {
  data <- .movesr_read(path)
  rate_columns <- c("ratePerDistance", "ratePerStart", "ratePerVehicle")
  if (is.null(basis) && !any(rate_columns %in% names(data))) return(NULL)
  if (is.null(basis)) basis <- .movesr_basis(path, data)
  rate_name <- switch(basis,
    distance = "ratePerDistance",
    start = "ratePerStart",
    vehicle = "ratePerVehicle",
    profile = "ratePerVehicle",
    stop("Unknown basis: ", basis))
  if (!rate_name %in% names(data)) stop(path, " has no ", rate_name)
  required <- intersect(c("MOVESRunID", "yearID", "pollutantID", "processID",
                          "sourceTypeID", "fuelTypeID", "modelYearID",
                          "temperature", "relHumidity"), names(data))
  missing <- setdiff(c("pollutantID", "processID", "sourceTypeID",
                       "fuelTypeID", "modelYearID", "temperature",
                       "relHumidity"), required)
  if (length(missing)) {
    stop(path, " is missing: ", paste(missing, collapse = ", "))
  }
  data[, rate := as.numeric(get(rate_name))]
  if (basis == "distance") {
    distance_units <- match.arg(distance_units)
    data[, rate := rate / if (distance_units == "mile") 1.609344 else 1]
    units <- "g/km"
  } else if (basis == "start") {
    units <- "g/start"
  } else {
    units <- "g/vehicle"
  }
  if (any(data$rate < 0, na.rm = TRUE)) stop(path, " has negative rates")
  data[, temperature_C := (temperature - 32) * 5 / 9]
  data[, basis := basis]
  data[, units := units]
  data[, rate_raw := get(rate_name)]
  data[, (rate_name) := NULL]
  data
}

#' Load portable MOVES factor data
#'
#' @description Reads one or more MOVES rate tables from CSV, TSV or Parquet.
#' The result is independent of MariaDB and Java.
#' @param path A file or directory. A directory may contain
#' `rateperdistance`, `rateperstart`, `ratepervehicle` and `rateperprofile`
#' files with `.csv`, `.tsv` or `.parquet` extensions.
#' @param distance_units Units of the distance rate input: `"mile"` or `"km"`.
#' The returned distance rate is always g/km.
#' @param basis Optional basis override for a single file: `"distance"`,
#' `"start"`, `"vehicle"` or `"profile"`.
#' @return A `movesr_factors` object containing a data.table and metadata.
#' @export
movesr_factors <- function(path, distance_units = c("mile", "km"), basis = NULL) {
  distance_units <- match.arg(distance_units)
  if (dir.exists(path)) {
    files <- list.files(path, pattern = "[.](csv|tsv|parquet)$",
                        full.names = TRUE, ignore.case = TRUE)
    files <- files[file.info(files)$size > 0]
    if (!length(files)) stop("No factor files found in ", path)
    tables <- lapply(files, function(file) .movesr_one(file, distance_units = distance_units))
    tables <- Filter(Negate(is.null), tables)
    if (!length(tables)) stop("No MOVES rate tables found in ", path)
    data <- data.table::rbindlist(tables, fill = TRUE)
  } else {
    data <- .movesr_one(path, basis = if (is.null(basis)) NULL else basis,
                        distance_units = distance_units)
  }
  key_columns <- c("MOVESRunID", "basis", "pollutantID", "processID",
                   "sourceTypeID", "fuelTypeID", "modelYearID", "yearID",
                   "roadTypeID", "avgSpeedBinID", "hourID", "temperature",
                   "relHumidity", "temperatureProfileID", "linkID")
  key_columns <- intersect(key_columns, names(data))
  if (anyDuplicated(data[, key_columns, with = FALSE])) {
    stop("Duplicate factor keys in the input")
  }
  data.table::setkeyv(data, c("basis", "pollutantID", "processID",
                              "sourceTypeID", "fuelTypeID", "modelYearID"))
  structure(list(data = data, distance_units = distance_units),
            class = "movesr_factors")
}

#' @export
print.movesr_factors <- function(x, ...) {
  cat("movesr factors:", nrow(x$data), "rows\n")
  print(x$data[, .N, by = basis])
  invisible(x)
}

#' Write portable MOVES factor data
#'
#' @param x A `movesr_factors` object.
#' @param path Output CSV file.
#' @return The output path invisibly.
#' @export
movesr_write <- function(x, path) {
  if (!inherits(x, "movesr_factors")) stop("x must be a movesr_factors object")
  data.table::fwrite(x$data, path)
  invisible(path)
}
