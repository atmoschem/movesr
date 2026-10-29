# Checks that repeating a MOVES run (06_timing.sh) reproduces the reference
# rates. Run identifiers are ignored. Writes paper/data/moves_repeat.csv.

library(data.table)
work <- Sys.getenv("MOVES_WORK")
sets <- c("exports-dmv", "exports-dmv-events", "exports-dmv-evap24")
tabs <- c(rateperdistance = "ratePerDistance", rateperstart = "ratePerStart",
          ratepervehicle = "ratePerVehicle", rateperprofile = "ratePerVehicle")
out <- list()
for (s in sets) for (t in names(tabs)) {
  a <- file.path(work, s, paste0(t, ".csv.gz"))
  b <- file.path(work, paste0(s, "-timing"), paste0(t, ".csv.gz"))
  if (!file.exists(a) || !file.exists(b)) next
  x <- fread(a); y <- fread(b)
  if (!nrow(x) && !nrow(y)) next
  drop <- intersect(c("MOVESRunID", "MOVESScenarioID"), names(x))
  k <- setdiff(names(x), c(drop, tabs[[t]]))
  m <- merge(x[, c(k, tabs[[t]]), with = FALSE], y[, c(k, tabs[[t]]), with = FALSE],
             by = k, all = TRUE)
  v1 <- m[[paste0(tabs[[t]], ".x")]]; v2 <- m[[paste0(tabs[[t]], ".y")]]
  out[[length(out) + 1L]] <- data.table(
    set = s, table = t, rows_first = nrow(x), rows_repeat = nrow(y),
    unmatched = sum(xor(is.na(v1), is.na(v2))),
    identical_values = sum(v1 == v2, na.rm = TRUE),
    both_null = sum(is.na(v1) & is.na(v2)),
    max_abs_rel = max(abs(v2 / v1 - 1)[v1 != 0], na.rm = TRUE))
}
out <- rbindlist(out)
fwrite(out, "paper/data/moves_repeat.csv")
print(out)
