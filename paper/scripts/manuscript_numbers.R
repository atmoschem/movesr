# Numbers used in the manuscript, read from the files written by scripts 01-07.
# Sourced by manuscript.Rmd (working directory: paper/).
library(data.table)
dd <- function(f) fread(file.path("data", f))
ov <- dd("parity_overall.csv")
bp <- dd("parity_by_pollutant.csv")
inter <- dd("intermediate_tables.csv")
simp <- dd("simplified_eval.csv")
simp_prof <- dd("simplified_test_profiles.csv")
vr <- dd("vein_roundtrip.csv")
fmt_n <- function(x) formatC(x, format = "d", big.mark = ",")
fmt_e <- function(x, d = 2) {
  if (!is.finite(x) || x == 0) return("0")
  e <- floor(log10(abs(x))); m <- signif(x / 10^e, d)
  if (m >= 10) { m <- m / 10; e <- e + 1 }
  sprintf("$%s \\times 10^{%d}$", format(m), e)
}
fmt_pct <- function(x, d = 2) paste0(format(signif(100 * x, d)), "\\%")
exh <- ov[!grepl("evap", part)]
evp <- ov[grepl("evap", part)]
we <- dd("weekend_evap.csv")
tot_matched <- sum(ov$matched) + sum(we$matched)
max_exhaust <- max(exh$max_abs)
max_evap <- max(c(evp$max_abs, we$max_abs))
p99_evap <- max(evp$p99_abs)
simp[, rel := simple / exact - 1]
sp <- simp[, .(mean_abs = mean(abs(rel)), p90 = quantile(abs(rel), 0.9), max = max(abs(rel))),
           by = .(processID)]
simp_max_pl <- max(simp[processID != 12, abs(rel)])
simp_mean_v <- mean(simp[processID == 12, abs(rel)])
far <- simp_prof[nearest > 5]
v_far <- simp[zoneID %in% far$zoneID & monthID %in% far$monthID & processID == 12]
simp_v_ok <- simp[processID == 12 & !(paste(zoneID, monthID) %in% paste(far$zoneID, far$monthID))]

tmg <- if (file.exists(file.path("data", "timing.csv"))) dd("timing.csv") else
  data.table(tool = character(), workload = character(), rep = integer(),
             wall_s = numeric(), java_s = numeric(), max_rss_mb = numeric())
mvt <- tmg[tool == "moves", .(workload, moves_wall = wall_s)]
mrt <- tmg[tool == "movesr", .(r_med = median(wall_s)), by = workload]
pairs_t <- data.table(moves = c("running", "events", "evap24"),
                      movesr = c("dmv_running", "dmv_events", "dc_evap24"))
pairs_t[, ratio := mvt$moves_wall[match(moves, mvt$workload)] / mrt$r_med[match(movesr, mrt$workload)]]
speed_min <- min(pairs_t$ratio)
speed_max <- max(pairs_t$ratio)
""
