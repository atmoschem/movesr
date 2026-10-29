#!/bin/bash
# Times the MOVES reference runs and the equivalent movesr workloads on the same
# machine, one at a time, with GNU time (wall clock and peak resident memory).
# MOVES runs are written to separate folders (exports-*-timing) so that the
# reference exports are not touched; 07_moves_repeat.R then checks that the
# repeated MOVES runs give the same rates.
# Environment: MOVESR_DB, DUCKDB, MOVES_WORK, R_LIBS (as for 01_parity.R).
# Run from the movesr root: bash paper/scripts/06_timing.sh
set -e
ROOT=$(pwd)
LOG=$ROOT/paper/data/timing_logs
mkdir -p "$LOG"
REPS=${REPS:-3}

# MOVES6: one run each (single worker, private MariaDB started and stopped by
# the script; the Java time is also printed by generate_dmv.py).
# SKIP_MOVES=1 keeps MOVES timings already recorded in timing_logs.
for mode in evap24 events running; do
  if [ "${SKIP_MOVES:-0}" = 1 ] && [ -s "$LOG/moves_$mode.time" ]; then continue; fi
  case $mode in
    evap24) dir=exports-dmv-evap24-timing ;;
    events) dir=exports-dmv-events-timing ;;
    running) dir=exports-dmv-timing ;;
  esac
  (cd data-raw/dmv && MOVES_OUT_DIR=$dir /usr/bin/time -v -o "$LOG/moves_$mode.time" \
     python3 generate_dmv.py $mode > "$LOG/moves_$mode.log" 2>&1)
done

# movesr: each workload in a fresh R process, REPS times.
for w in dc_evap24 dc_evap24_simple dmv_events dmv_running; do
  for r in $(seq "$REPS"); do
    /usr/bin/time -v -o "$LOG/movesr_${w}_$r.time" \
      Rscript --vanilla paper/scripts/06_timing_workload.R $w "${TMPDIR:-/tmp}/movesr_${w}.csv" \
      > "$LOG/movesr_${w}_$r.log" 2>&1
  done
done

Rscript --vanilla - <<'RS'
library(data.table)
log <- "paper/data/timing_logs"
f <- list.files(log, "\\.time$", full.names = TRUE)
get <- function(x, key) sub(".*: ", "", grep(key, x, value = TRUE, fixed = TRUE)[1])
wall <- function(s) {
  p <- as.numeric(strsplit(s, ":")[[1]])
  sum(p * 60^(rev(seq_along(p)) - 1))
}
tm <- rbindlist(lapply(f, function(p) {
  x <- readLines(p)
  b <- sub("\\.time$", "", basename(p))
  tool <- sub("_.*", "", b)
  rest <- sub("^[^_]+_", "", b)
  rep <- if (tool == "movesr") as.integer(sub(".*_", "", rest)) else 1L
  wl <- if (tool == "movesr") sub("_[0-9]+$", "", rest) else rest
  java <- NA_real_
  if (tool == "moves") {
    l <- readLines(file.path(log, paste0(b, ".log")))
    j <- grep("MOVES finished in", l, value = TRUE)
    if (length(j)) java <- 60 * as.numeric(sub(".*in ([0-9.]+) min.*", "\\1", j[1]))
  }
  data.table(tool = tool, workload = wl, rep = rep,
             wall_s = wall(get(x, "Elapsed (wall clock) time")),
             java_s = java,
             max_rss_mb = as.numeric(get(x, "Maximum resident set size")) / 1024)
}))
fwrite(tm[order(tool, workload, rep)], "paper/data/timing.csv")
print(tm[order(tool, workload, rep)])
RS
