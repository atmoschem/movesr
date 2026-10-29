# Vapor venting by hour in the District of Columbia (July weekday): full-day
# computation (as a MOVES run with hours 1-24) against single-hour runs (as
# MOVES computes venting when the RunSpec selects only that hour).
# Writes paper/data/dc_venting_hours.csv. Same environment as 01_parity.R.

suppressMessages(library(movesr))
library(data.table)
options(movesr.duckdb = Sys.getenv("DUCKDB", "duckdb"))
db <- movesr_database(Sys.getenv("MOVESR_DB"))
work <- Sys.getenv("MOVES_WORK")
met <- movesr_default_meteorology(db, 11001, 7, 1:24)
tank <- movesr_tank_temperature(db, met$temperature, day_id = 5L)
full <- movesr_evap(db, 11001, 2025, 7, 5L, meteorology = met, tank = tank,
                    process_id = 12, source_type_id = 21)
single <- rbindlist(lapply(1:24, function(h) {
  movesr_evap(db, 11001, 2025, 7, 5L, hour_id = h, meteorology = met, tank = tank,
              process_id = 12, source_type_id = 21, tvv_hours = h)
}))
k <- c("hourID", "sourceTypeID", "fuelTypeID", "modelYearID")
x <- merge(full[, c(k, "ratePerVehicle"), with = FALSE],
           single[, c(k, "ratePerVehicle"), with = FALSE], by = k,
           suffixes = c(".full", ".single"))
# MOVES itself: the 24-hour run and the hours 16-17 run of the DMV events set.
m24 <- fread(file.path(work, "exports-dmv-evap24", "rateperprofile.csv.gz"))[
  pollutantID == 1 & processID == 12 & sourceTypeID == 21, c(k, "ratePerVehicle"), with = FALSE]
m2 <- fread(file.path(work, "exports-dmv-events", "rateperprofile.csv.gz"))[
  temperatureProfileID %/% 10000 == 110010 & pollutantID == 1 & processID == 12 &
    sourceTypeID == 21, c(k, "ratePerVehicle"), with = FALSE]
x <- merge(x, setnames(m24, "ratePerVehicle", "moves_24h"), by = k, all.x = TRUE)
x <- merge(x, setnames(m2, "ratePerVehicle", "moves_16_17h"), by = k, all.x = TRUE)
fwrite(x, "paper/data/dc_venting_hours.csv")
print(x[fuelTypeID == 1 & modelYearID == 2015])
