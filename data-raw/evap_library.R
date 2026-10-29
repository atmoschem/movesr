# Build the default tank-temperature library of the simplified evap method
# (R/sysdata.rda: .movesr_tank_library).
#
# Reference profiles: MOVES default ZoneMonthHour 24-hour profiles (all US
# zones and months), stratified by daily mean temperature (quantile bins) and
# diurnal range (quantile bins within each mean bin); one profile per stratum, the one
# closest to the stratum median, plus profiles at the extreme daily-mean
# quantiles. Each profile runs the exact
# TankTemperatureGenerator port for weekdays and weekends (15 s weekday, 6 s weekend).
#
# Usage: MOVESR_DB=.../movesdb-parquet DUCKDB=.../duckdb Rscript data-raw/evap_library.R [nmean] [nrange]
# (about 20 s per profile and both day types)

library(data.table)
library(movesr)
args <- commandArgs(TRUE)
nmean <- if (length(args) > 0) as.integer(args[1]) else 20L
nrange <- if (length(args) > 1) as.integer(args[2]) else 5L
db <- movesr_database(Sys.getenv("MOVESR_DB"))
zmh <- movesr_table(db, "zonemonthhour")
s <- zmh[, .(tmean = mean(temperature), trange = max(temperature) - min(temperature), n = .N),
         by = .(zoneID, monthID)][n == 24L]
s[, mbin := cut(tmean, quantile(tmean, seq(0, 1, length.out = nmean + 1L)),
                include.lowest = TRUE, labels = FALSE)]
s[, rbin := cut(trange, unique(quantile(trange, seq(0, 1, length.out = nrange + 1L))),
                include.lowest = TRUE, labels = FALSE), by = mbin]
pick <- s[, .SD[which.min(abs(tmean - median(tmean)) + abs(trange - median(trange)))],
          by = .(mbin, rbin)]
# Climate tails (stratum medians leave them uncovered): profiles at extreme
# daily-mean quantiles.
q <- quantile(s$tmean, c(0, 0.001, 0.003, 0.006, 0.01, 0.015, 0.02, 0.03, 0.97,
                         0.98, 0.99, 0.997, 1))
tails <- rbindlist(lapply(q, function(v) s[which.min(abs(tmean - v))]))
pick <- unique(rbind(pick[, .(zoneID, monthID, tmean, trange)],
                     tails[, .(zoneID, monthID, tmean, trange)]))
setorder(pick, tmean)
cache <- Sys.getenv("MOVESR_LIBRARY_CACHE", "data-raw/evap_library_cache")
dir.create(cache, showWarnings = FALSE)
entries <- list()
for (d in c(5L, 2L)) for (i in seq_len(nrow(pick))) {
  f <- file.path(cache, sprintf("ttg_%d_%d_%d.rds", pick$zoneID[i], pick$monthID[i], d))
  if (!file.exists(f)) {
    t <- zmh[zoneID == pick$zoneID[i] & monthID == pick$monthID[i]][order(hourID)]$temperature
    x <- movesr_tank_temperature(db, t, day_id = d)
    saveRDS(x[c("temperature", "cold_soak", "average_tank_temperature",
                "soak_activity_fraction", "cold_soak_initial_hour_fraction")], f)
  }
  entries[[length(entries) + 1L]] <- movesr:::.tank_library_entry(readRDS(f), d, length(entries) + 1L)
}
.movesr_tank_library <- movesr:::.tank_library_bind(entries)
# Round far below the method error so the library compresses (about 2 MB).
.movesr_tank_library$rise[, rise := round(rise, 4)]
.movesr_tank_library$soak[, hotSoak := signif(hotSoak, 6)]
.movesr_tank_library$initial[, fraction := signif(fraction, 6)]
attr(.movesr_tank_library, "source") <- pick[, .(zoneID, monthID, tmean, trange)]
save(.movesr_tank_library, file = "R/sysdata.rda", compress = "xz")
