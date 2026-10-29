# Weekend evaporative rates: movesr against the MOVES6 run distributed with
# VEIN (Washtenaw County MI, January, weekend day, hours 1-3). That run set the
# whole ZoneMonthHour table of the county to 68 F, except hours 1-3 (68, 50 and
# 86 F), with 50% relative humidity (vein/data-raw/moves/generate_reference.py).
# Writes paper/data/weekend_evap.csv. Same environment as 01_parity.R.
# Rates for model years before 2000 are produced by movesr but are not part of
# the reference run.

suppressMessages(library(movesr))
library(data.table)
options(movesr.duckdb = Sys.getenv("DUCKDB", "duckdb"))
db <- movesr_database(Sys.getenv("MOVESR_DB"))
wash <- Sys.getenv("MOVESR_REFERENCE")
met <- data.table(hourID = 1:24, temperature = c(68, 50, 86, rep(68, 21)), relHumidity = 50)
# E85 tank fuel: this MOVES run used the E85 formulation with E10 properties
# (see the manuscript); the raw setting is also recorded for the venting check.
x <- movesr_evap(db, 26161, 2025, 1, 2L, hour_id = 1:3, meteorology = met,
                 tvv_hours = 1:3, e85_tank_properties = "e10")
xr <- movesr_evap(db, 26161, 2025, 1, 2L, hour_id = 1:3, meteorology = met,
                  tvv_hours = 1:3, process_id = 12, e85_tank_properties = "raw")
ref <- rbind(fread(file.path(wash, "ratepervehicle.csv")),
             fread(file.path(wash, "rateperprofile.csv")), fill = TRUE)
k <- c("hourID", "sourceTypeID", "fuelTypeID", "modelYearID", "pollutantID", "processID")
m <- merge(ref[, c(k, "ratePerVehicle"), with = FALSE], x[, c(k, "ratePerVehicle"), with = FALSE],
           by = k, all = TRUE, suffixes = c(".ref", ".mr"))
m[, rel := ratePerVehicle.mr / ratePerVehicle.ref - 1]
s <- m[, .(rows_ref = sum(!is.na(ratePerVehicle.ref)), rows_movesr = sum(!is.na(ratePerVehicle.mr)),
           matched = sum(!is.na(ratePerVehicle.ref) & !is.na(ratePerVehicle.mr)),
           median_abs = median(abs(rel), na.rm = TRUE),
           max_abs = max(abs(rel), na.rm = TRUE),
           gt_1e4 = sum(abs(rel) > 1e-4, na.rm = TRUE)), by = processID]
# The MOVES reference covers model years 2000-2025 only.
s[, extra_movesr_before_2000 := m[is.na(ratePerVehicle.ref) & modelYearID < 2000, .N]]
r <- merge(ref[processID == 12, c(k, "ratePerVehicle"), with = FALSE],
           xr[, c(k, "ratePerVehicle"), with = FALSE], by = k)
r[, rel := ratePerVehicle.y / ratePerVehicle.x - 1]
s[, `:=`(venting_raw_max_abs = r[, max(abs(rel))],
         venting_raw_gt_1e4 = r[, sum(abs(rel) > 1e-4)],
         venting_raw_gt_1e4_e85_hour3 = r[abs(rel) > 1e-4, all(fuelTypeID == 5 & hourID == 3)])]
fwrite(s, "paper/data/weekend_evap.csv")
fwrite(m, "paper/data/weekend_evap_pairs.csv.gz")
print(s)
