# Figures for the manuscript. Reads paper/data (written by scripts 01-03) and
# writes paper/figures/*.pdf and *.png. Run from the movesr root.

library(data.table)
library(ggplot2)
dir.create("paper/figures", showWarnings = FALSE)
d <- function(f) fread(file.path("paper/data", f))
save_fig <- function(p, name, w, h) {
  ggsave(file.path("paper/figures", paste0(name, ".pdf")), p, width = w, height = h)
  ggsave(file.path("paper/figures", paste0(name, ".png")), p, width = w, height = h, dpi = 300)
}
theme_set(theme_bw(base_size = 9) +
            theme(panel.grid.minor = element_blank(),
                  strip.background = element_rect(fill = "grey92", colour = NA)))

# Figure 1. Structure of the port --------------------------------------------
box <- data.table(
  x = c(4, 2, 2, 2, 2, 2, 2, 6, 6, 6, 6, 4, 4, 7.9),
  y = c(10.2, 8.8, 7.6, 6.4, 5.2, 4.0, 2.8, 7.6, 6.4, 5.2, 4.0, 1.4, 0.2, 1.4),
  label = c(
    "MOVES6 default database\n240 tables, Parquet, read with DuckDB",
    "Source bins and fuel usage\nmovesr_sourcebins()",
    "Operating modes (drive cycles, VSP)\nmovesr_opmodes()",
    "Base rates: source-bin weighting,\nair conditioning, EV adjustment",
    "Fuel effects: general ratios, Complex and\nPredictive Model, sulfur model",
    "Meteorology, temperature, humidity, I/M",
    "Running, start, brake, tire rates\nand chained pollutants",
    "Activity: population, VMT, SHO, SHP\nmovesr_activity()",
    "Tank temperatures and soak fractions\nmovesr_tank_temperature() (exact)\nmovesr_tank_temperature_simple()",
    "Tank fuel RVP and ethanol\nmovesr_average_tank_gasoline()",
    "Permeation, vapor venting, leaks\nmovesr_evap_rates(), movesr_tvv_hourly()",
    "MOVES-format rate tables\nmovesr_export_vein()",
    "vein: moves_factors(), ef_moves(),\nef_moves_start(), ef_moves_evap()",
    "MOVES6 runs (Java, MariaDB, Go)\ngolden sets, row-by-row comparison"),
  fill = c("db", rep("exhaust", 6), rep("evap", 4), "out", "out", "check"))
arrows <- data.table(
  x = c(3.3, 2, 2, 2, 2, 2, 4.7, 6, 6, 6, 2, 6, 4, 6.5),
  y = c(9.8, 8.35, 7.15, 5.95, 4.75, 3.55, 9.8, 7.15, 5.95, 4.75, 2.35, 3.55, 0.95, 1.4),
  xend = c(2.3, 2, 2, 2, 2, 2, 5.7, 6, 6, 6, 3.6, 4.4, 4, 5.6),
  yend = c(9.25, 8.05, 6.85, 5.65, 4.45, 3.25, 8.05, 6.85, 5.65, 4.45, 1.85, 1.85, 0.65, 1.4))
p1 <- ggplot() +
  geom_segment(data = arrows, aes(x, y, xend = xend, yend = yend),
               arrow = arrow(length = unit(1.6, "mm")), colour = "grey35") +
  geom_label(data = box, aes(x, y, label = label, fill = fill), size = 2.3,
             label.size = 0.2, lineheight = 0.9, label.padding = unit(1.2, "mm")) +
  scale_fill_manual(values = c(db = "grey90", exhaust = "#dbe9f6", evap = "#fde5cf",
                               out = "#e3f1dc", check = "white"), guide = "none") +
  annotate("text", x = 0.1, y = 9.45, label = "Exhaust, brake and tire", size = 2.8,
           fontface = "bold", hjust = 0) +
  annotate("text", x = 7.6, y = 8.35, label = "Evaporative", size = 2.8, fontface = "bold",
           hjust = 0) +
  coord_cartesian(xlim = c(0, 9.4), ylim = c(-0.3, 10.9)) +
  theme_void()
save_fig(p1, "fig1_structure", 7, 5.6)

# Figure 2. movesr against MOVES, all golden sets ---------------------------
s <- d("parity_sample.csv.gz")
panels <- c("Washtenaw: running", "Washtenaw: start", "DMV: running", "DMV: brake/tire",
            "DMV: start", "DMV: evap parked", "DMV: evap driving", "DC 24 h: evap parked")
s[, panel := factor(paste(set, part, sep = ": "), panels)]
logb <- scales::trans_breaks("log10", function(x) 10^x, n = 3)
logl <- scales::trans_format("log10", scales::math_format(10^.x))
p2 <- ggplot(s, aes(ref, mr)) +
  geom_abline(slope = 1, intercept = 0, colour = "grey60", size = 0.3) +
  geom_point(size = 0.25, alpha = 0.4, colour = "#1f4e79") +
  scale_x_log10(breaks = logb, labels = logl) +
  scale_y_log10(breaks = logb, labels = logl) +
  facet_wrap(~panel, ncol = 4, scales = "free") +
  labs(x = "MOVES6 rate (g/mile, g/start or g/vehicle/h)",
       y = "movesr rate (same unit)")
save_fig(p2, "fig2_parity", 7, 4.2)

# Figure 3. Distribution of relative differences ----------------------------
dec <- d("parity_decades.csv")
dec[, panel := factor(paste(set, part, sep = ": "), panels)]
dec[, bin := "exact"]
dec[is.finite(decade), bin := sprintf("1e%d", as.integer(decade))]
lev <- c("exact", sprintf("1e%d", -12:-3))
dec[, bin := factor(bin, levels = lev)]
dec[, share := N / sum(N), by = panel]
p3 <- ggplot(dec[!is.na(bin)], aes(bin, share)) +
  geom_col(fill = "#1f4e79", width = 0.75) +
  geom_vline(xintercept = which(lev == "1e-6") + 0.5, linetype = 2, colour = "firebrick") +
  facet_wrap(~panel, ncol = 4) +
  labs(x = expression("Decade of |relative difference| (bin 1e" * italic(k) *
                        " holds values in [10"^italic(k) * ", 10"^{italic(k) + 1} * "))"),
       y = "Share of rates") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 6))
save_fig(p3, "fig3_reldiff", 7, 4.2)

# Figure 4. Tank temperatures and hot-soak share, DC July weekday ------------
tt <- d("dc_tank_temperature.csv")
tt[, hourID := hourDayID %/% 10L]
tt <- tt[tankTemperatureGroupID == 3L]
tt[, mode := factor(opModeID, c(151, 150, 300), c("Cold soak (151)", "Hot soak (150)",
                                                    "Operating (300)"))]
amb <- data.table(hourID = 1:24, temperature = c(73.2, 72.1, 71.2, 70.4, 69.6, 69.0, 70.0,
  73.2, 76.5, 79.7, 82.3, 84.6, 86.3, 87.4, 88.0, 88.0, 87.5, 86.5, 84.8, 82.2, 79.0,
  77.0, 75.6, 74.3))
p4 <- ggplot(tt, aes(hourID)) +
  geom_line(data = amb, aes(y = temperature, linetype = "Ambient"), colour = "grey40") +
  geom_line(aes(y = mr, colour = mode)) +
  geom_point(aes(y = ref, colour = mode), shape = 1, size = 1.3) +
  scale_colour_manual(values = c("#1f4e79", "#c0504d", "#e69f00")) +
  scale_linetype_manual(values = 2) +
  labs(x = "Hour of day", y = "Temperature (F)", colour = NULL, linetype = NULL) +
  theme(legend.position = "bottom")
save_fig(p4, "fig4_tank_temperature", 5, 3.4)

# Figure 5. Vapor venting by hour: full-day run and single-hour runs --------
v <- d("dc_venting_hours.csv")[fuelTypeID == 1 & modelYearID %in% c(2005, 2015, 2025)]
v[, MY := factor(modelYearID)]
lv <- melt(v, id.vars = c("hourID", "MY"), measure.vars = c("ratePerVehicle.full",
                                                            "ratePerVehicle.single"))
lv[, run := factor(variable, c("ratePerVehicle.full", "ratePerVehicle.single"),
                   c("movesr, 24 hours", "movesr, single-hour run"))]
pts <- melt(v, id.vars = c("hourID", "MY"), measure.vars = c("moves_24h", "moves_16_17h"),
            na.rm = TRUE)
pts[, run := factor(variable, c("moves_24h", "moves_16_17h"),
                    c("MOVES6, 24-hour run", "MOVES6, hours 16-17 run"))]
p5 <- ggplot(lv, aes(hourID, value, colour = MY)) +
  geom_line(aes(linetype = run)) +
  geom_point(data = pts, aes(shape = run), size = 1.6) +
  scale_shape_manual(values = c(1, 17)) +
  scale_linetype_manual(values = c(1, 2)) +
  scale_y_log10() +
  labs(x = "Hour of day", y = "THC vapor venting (g/vehicle/h)", colour = "Model year",
       linetype = NULL, shape = NULL) +
  guides(colour = guide_legend(order = 1), linetype = guide_legend(order = 2, nrow = 2),
         shape = guide_legend(order = 3, nrow = 2)) +
  theme(legend.position = "bottom", legend.box = "horizontal")
save_fig(p5, "fig5_venting_hours", 6, 3.8)

# Figure 6. Simplified method against the exact port --------------------------
e <- d("simplified_eval.csv")
e[, process := factor(processID, c(11, 12, 13), c("Permeation", "Vapor venting", "Leaks"))]
e[, day := factor(dayID, c(5, 2), c("Weekday", "Weekend"))]
p6 <- ggplot(e, aes((tmean - 32) * 5 / 9, 100 * rel, shape = day)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_point(size = 1.4) +
  facet_wrap(~process, scales = "free_y") +
  scale_shape_manual(values = c(16, 1)) +
  labs(x = "Daily mean temperature of the test profile (C)",
       y = "Daily total, simplified minus exact (%)", shape = NULL) +
  theme(legend.position = "bottom")
save_fig(p6, "fig6_simplified", 7, 2.9)

# Figure 7. VEIN round trip, District of Columbia ------------------------------
vr <- d("vein_roundtrip.csv")[pollutantID == 3 & sourceTypeID %in% c(21, 62) &
                                ((sourceTypeID == 21 & fuelTypeID == 1) |
                                   (sourceTypeID == 62 & fuelTypeID == 2))]
vr[, vehicle := fifelse(sourceTypeID == 21, "Passenger car, gasoline",
                        "Combination long-haul truck, diesel")]
vr[, MY := factor(modelYearID)]
p7 <- ggplot(vr, aes(speed_kmh, colour = MY)) +
  geom_step(aes(y = vein_gkm), direction = "mid") +
  geom_point(aes(y = moves_gkm), shape = 1, size = 1.3) +
  facet_wrap(~vehicle, scales = "free_y") +
  scale_y_log10() +
  labs(x = "Speed (km/h)", y = "NOx (g/km)", colour = "Model year") +
  theme(legend.position = "bottom")
save_fig(p7, "fig7_vein_dc", 7, 3)
