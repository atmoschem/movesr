# One movesr workload, run in a fresh R process by 06_timing.sh so that the
# measured time includes starting R, loading movesr and reading the database.
# Each workload computes what the corresponding MOVES reference run computes.
#   dmv_running  running (21 pollutants), brake and tire, and on-road evaporative
#                rates (5 HC pollutants), 3 counties, July weekday, hours 16-17
#   dmv_events   start (21 pollutants) and parked evaporative rates (5 HC
#                pollutants, venting as a 16-17 h run), 3 counties
#   dc_evap24    parked evaporative rates, DC, 24 hours, exact tank model
#   dc_evap24_simple  the same with the simplified tank model

args <- commandArgs(TRUE)
w <- args[1]
out <- if (length(args) > 1) args[2] else tempfile(fileext = ".csv")
suppressMessages(library(movesr))
library(data.table)
options(movesr.duckdb = Sys.getenv("DUCKDB", "duckdb"))
db <- movesr_database(Sys.getenv("MOVESR_DB"))
counties <- c(11001L, 24031L, 51059L)
pols <- c(1, 2, 3, 5, 6, 30, 31, 79, 80, 86, 87, 90, 91, 98, 100, 110, 111, 112, 115,
          118, 119)
hc <- c(1, 79, 80, 86, 87)
res <- list()
if (w == "dmv_running") {
  r <- movesr_region_rates(db, counties, process = c("running", "brake", "tire"),
                           month_id = 7, hour_id = 16:17, day_id = 5L, road_type_id = 5,
                           pollutant_id = pols)
  res <- c(res, r)
  for (cty in counties) {
    met <- movesr_default_meteorology(db, cty, 7, 1:24)
    tank <- movesr_tank_temperature(db, met$temperature, day_id = 5L)
    res[[length(res) + 1L]] <- movesr_evap_running_rates(
      db, cty, 2025, 7, 5L, hour_id = 16:17, meteorology = met, tank = tank,
      pollutant_id = hc, e85_tank_properties = "e10")[, countyID := cty]
  }
} else if (w == "dmv_events") {
  res$start <- movesr_region_rates(db, counties, process = "start", month_id = 7,
                                   hour_id = 16:17, day_id = 5L, pollutant_id = pols)$start
  for (cty in counties) {
    met <- movesr_default_meteorology(db, cty, 7, 1:24)
    res[[length(res) + 1L]] <- movesr_evap(db, cty, 2025, 7, 5L, hour_id = 16:17,
                                           meteorology = met, tvv_hours = 16:17,
                                           pollutant_id = hc)[, countyID := cty]
  }
} else if (w %in% c("dc_evap24", "dc_evap24_simple")) {
  met <- movesr_default_meteorology(db, 11001, 7, 1:24)
  res$evap <- movesr_evap(db, 11001, 2025, 7, 5L, meteorology = met, pollutant_id = hc,
                          method = if (w == "dc_evap24") "original" else "simplified")
} else stop("Unknown workload ", w)
x <- rbindlist(res, fill = TRUE)
fwrite(x, out)
cat(w, nrow(x), "rates\n")
