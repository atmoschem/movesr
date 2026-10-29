# VEIN round trip for the District of Columbia: movesr factors exported with
# movesr_export_vein(), read with vein::moves_factors() and evaluated with
# vein::ef_moves(), compared with the MOVES golden set at the same speed bins.
# Writes paper/data/vein_roundtrip.csv. Same environment as 01_parity.R.

suppressMessages(library(movesr))
library(data.table)
options(movesr.duckdb = Sys.getenv("DUCKDB", "duckdb"))
db <- movesr_database(Sys.getenv("MOVESR_DB"))
work <- Sys.getenv("MOVES_WORK")
met <- movesr_default_meteorology(db, 11001, 7, 16)
pol <- c(2, 3, 87, 110)
r <- movesr_running_rates(db, county_id = 11001, month_id = 7, meteorology = met,
                          source_type_id = c(21, 32, 62), road_type_id = 5,
                          pollutant_id = pol)
b <- movesr_running_rates(db, county_id = 11001, month_id = 7, meteorology = met,
                          source_type_id = c(21, 32, 62), road_type_id = 5,
                          process_id = 9, pollutant_id = 116)
dir <- file.path(tempdir(), "vein_dc")
movesr_export_vein(dir, running = rbindlist(list(r, b), fill = TRUE), database = db)
f <- vein::moves_factors(file.path(dir, "rateperdistance.csv"), file.path(dir, "movesrun.csv"),
                         file.path(dir, "vehicles.csv"))
bins <- movesr_table(db, "avgspeedbin")[, .(avgSpeedBinID, avgBinSpeed)]
# vein bins are lower inclusive at 2.5, 7.5, ... mph, so bin 1 (nominal 2.5
# mph) is evaluated at 1.25 mph and the other bins at their nominal speed.
kmh <- ifelse(bins$avgSpeedBinID == 1L, 1.25, bins$avgBinSpeed) * 1.609344
grid <- CJ(sourceTypeID = c(21L, 32L, 62L), fuelTypeID = c(1L, 2L), modelYearID = c(2010L, 2020L),
           pollutantID = c(2L, 3L, 87L, 110L, 116L))
ref <- fread(file.path(work, "exports-dmv", "rateperdistance.csv.gz"))[
  linkID %/% 10000 == 11001 & hourID == 16 & roadTypeID == 5]
out <- list()
for (i in seq_len(nrow(grid))) {
  g <- grid[i]
  proc <- if (g$pollutantID == 116L) "brake" else "running"
  v <- tryCatch(vein::ef_moves(g$pollutantID, g$modelYearID, speed = kmh, factors = f,
                               source_type_id = g$sourceTypeID, fuel_type_id = g$fuelTypeID,
                               process = proc, select = list(hourID = 16)),
                error = function(e) NULL)
  if (is.null(v)) next
  m <- ref[sourceTypeID == g$sourceTypeID & fuelTypeID == g$fuelTypeID &
             modelYearID == g$modelYearID & pollutantID == g$pollutantID &
             processID == if (proc == "brake") 9L else 1L][order(avgSpeedBinID)]
  if (!nrow(m)) next
  out[[length(out) + 1L]] <- data.table(g, avgSpeedBinID = bins$avgSpeedBinID, speed_kmh = kmh,
                                        vein_gkm = as.numeric(v),
                                        moves_gkm = m$ratePerDistance / 1.609344)
}
out <- rbindlist(out)
out[, rel := vein_gkm / moves_gkm - 1]
fwrite(out, "paper/data/vein_roundtrip.csv")
print(out[, .(n = .N, max_abs_rel = max(abs(rel[moves_gkm > 0]))), by = pollutantID])
