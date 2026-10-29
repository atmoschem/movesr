# Simplified evaporative method versus the exact port on MOVES default weather
# profiles that are not in the library. Writes paper/data/simplified_eval.csv.
# Same environment variables as 01_parity.R.

suppressMessages(library(movesr))
library(data.table)
options(movesr.duckdb = Sys.getenv("DUCKDB", "duckdb"))
db <- movesr_database(Sys.getenv("MOVESR_DB"))
lib <- movesr:::.movesr_tank_library
src <- attr(lib, "source")
zmh <- movesr_table(db, "zonemonthhour")
s <- zmh[, .(tmean = mean(temperature), trange = max(temperature) - min(temperature), n = .N),
         by = .(zoneID, monthID)][n == 24]
s <- s[!src, on = .(zoneID, monthID)]
set.seed(2026)
te <- s[sample(.N, 20)]
res <- list()
hourly <- list()
for (d in c(5L, 2L)) {
  act <- movesr_activity(db, 2025, 11001, 7, d)$parked
  for (i in seq_len(nrow(te))) {
    t <- zmh[zoneID == te$zoneID[i] & monthID == te$monthID[i]][order(hourID)]$temperature
    t0 <- proc.time()[["elapsed"]]
    ex <- movesr_tank_temperature(db, t, day_id = d)
    t_exact <- proc.time()[["elapsed"]] - t0
    t0 <- proc.time()[["elapsed"]]
    si <- suppressWarnings(movesr_tank_temperature_simple(db, t, day_id = d))
    t_simple <- proc.time()[["elapsed"]] - t0
    met <- data.table(hourID = 1:24, temperature = t, relHumidity = 50)
    rate <- function(tk) movesr_evap_rates(db, tk, act, county_id = 11001, month_id = 7,
                                           day_id = d, meteorology = met,
                                           process_id = c(11, 12, 13))
    x <- merge(rate(ex), rate(si), by = c("hourID", "sourceTypeID", "fuelTypeID",
                                          "modelYearID", "processID", "pollutantID"))
    h <- x[, .(exact = sum(ratePerVehicle.x), simple = sum(ratePerVehicle.y)),
           by = .(processID, hourID)]
    h[, `:=`(dayID = d, zoneID = te$zoneID[i], monthID = te$monthID[i])]
    hourly[[length(hourly) + 1L]] <- h
    r <- h[, .(exact = sum(exact), simple = sum(simple)), by = processID]
    r[, `:=`(dayID = d, zoneID = te$zoneID[i], monthID = te$monthID[i],
             tmean = te$tmean[i], trange = te$trange[i],
             seconds_exact = t_exact, seconds_simple = t_simple)]
    res[[length(res) + 1L]] <- r
    cat(d, i, "\n")
  }
}
res <- rbindlist(res)
res[, rel := simple / exact - 1]
dir.create("paper/data", showWarnings = FALSE)
fwrite(res, "paper/data/simplified_eval.csv")
fwrite(rbindlist(hourly), "paper/data/simplified_eval_hourly.csv")
# Nearest library profile for each test profile (in daily mean and range, F).
p <- lib$profile
te[, nearest := sapply(seq_len(.N), function(i)
  min(sqrt((p$tmean - tmean[i])^2 + (p$trange - trange[i])^2)))]
fwrite(te, "paper/data/simplified_test_profiles.csv")
fwrite(p, "paper/data/simplified_library_profiles.csv")
print(res[, .(mean_abs = mean(abs(rel)), p90 = quantile(abs(rel), 0.9), max = max(abs(rel))),
          by = .(dayID, processID)])
