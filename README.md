# movesr

`movesr` is a standalone R (`data.table`) port of the USEPA MOVES on-road
emission-rate engine. It needs no Java, no MariaDB and no MOVES installation:
the MOVES default database is read from a 10 MiB Parquet catalog through the
DuckDB command-line tool.

`movesr` generates factors; [`vein`](https://github.com/atmoschem/vein)
consumes them for inventories.

```text
MOVES default DB (Parquet) + your inputs
        ↓  movesr
rateperdistance.csv / rateperstart.csv
        ↓  vein::moves_factors(), ef_moves(), ef_moves_start()
emission inventory
```

## Status

| Process | Function | Validation against MOVES6 |
|---|---|---|
| 1 Running exhaust | `movesr_running_rates()` | Washtenaw MI: 86,304 factors; DMV: 2,522,784 |
| 2 Start exhaust | `movesr_start_rates()` | Washtenaw MI: 5,394; DMV: 155,112 |
| 9 Brakewear, 10 Tirewear | `movesr_running_rates(process_id = 9/10)` | DMV: 529,536 |
| 11/12/13 Evaporative (parked) | `movesr_evap()` | DC 24 h: 190,440; DMV 16-17 h: 9,522 |
| 11/12/13 Evaporative (driving) | `movesr_evap_running_rates()` | DMV: 761,760 |
| 90/91 idle/APU, refueling, air toxics | not yet ported | — |

Every MOVES factor in the golden sets is reproduced. Exhaust, brake and tire
agree within 8e-6, the 6-significant-digit precision of MOVES output (energy,
written in joules, within 1.6e-5); evap
within 7e-5, except 70 E85 vapor-venting rates (up to 9e-4) that are sensitive
to single-precision tank temperatures.

Pollutants (running and start): THC 1, CO 2, NOx 3, CH4 5, N2O 6, NH3 30,
SO2 31, NMHC 79, NMOG 80, TOG 86, VOC 87, CO2 90, energy 91, CO2e 98,
PM10 100, PM2.5 110, OC 111, EC 112, SO4 115, NonECPM 118, H2O 119; brake
and tire PM2.5/PM10 (116/106, 117/107).

The DMV golden set covers the District of Columbia, Montgomery County MD and
Fairfax County VA (July weekday, hours 16-17, MOVES default weather, fuels and
I/M programs), all 13 source types and model years 1985-2025.
See [`PORTING.md`](PORTING.md) for algorithms, sources and findings.

## Setup

```r
options(movesr.duckdb = "/path/to/duckdb")      # or env var DUCKDB
db <- movesr_database("/path/to/movesdb-parquet")
```

## Generate factors

```r
met <- data.frame(hourID = 1:3,
                  temperature = c(68, 50, 86),   # F, as in MOVES
                  relHumidity = 50)

running <- movesr_running_rates(db, year = 2025, meteorology = met,
                                source_type_id = c(21, 31, 32),
                                pollutant_id = c(2, 3))   # g/mile

start <- movesr_start_rates(db, year = 2025, meteorology = met,
                            day_id = 5L)                 # g/start
```

Or from a scenario object (Celsius):

```r
s <- movesr_scenario(year = 2025, source_type_id = 21, fuel_type_id = 1,
                     model_year = 2010:2025, pollutant_id = c(2, 3),
                     process_id = c(1, 2), road_type_id = 5,
                     hour_id = 1:2, temperature_c = c(20, 10),
                     rel_humidity = 50)
res <- movesr_run(db, s)
```

## Evaporative emissions

```r
# Exact MOVES port: sample-trip tank temperature simulation (~13 s per
# county/month/day type), activity, permeation, vapor venting and leaks.
ev <- movesr_evap(db, county_id = 11001, year = 2025, month_id = 7,
                  day_id = 5L, pollutant_id = c(1, 79, 87))   # g/vehicle/hour

# Simplified alternative: same calculators, tank temperatures and soak
# fractions interpolated from a library of exact results (0.2 s; daily
# totals within 0.5 % for permeation/leaks, ~4 % for vapor venting).
ev2 <- movesr_evap(db, county_id = 11001, month_id = 7, method = "simplified")

# Evap while driving, g/mile by average-speed bin.
evr <- movesr_evap_running_rates(db, county_id = 11001, month_id = 7)
```

Weather must cover the 24 hours of the day (tank temperatures depend on the
whole diurnal cycle); the default is the county's MOVES weather. The
building blocks are exported: `movesr_tank_temperature()`,
`movesr_tank_temperature_simple()`, `movesr_activity()`,
`movesr_average_tank_gasoline()`, `movesr_tvv_hourly()`,
`movesr_evap_rates()`.

## Regions

```r
movesr_region("dmv")          # 15 jurisdictions of the DC-MD-VA area
dmv <- movesr_region_rates(db, "dmv",
                           process = c("running", "start", "brake", "tire"),
                           month_id = 7, hour_id = 1:24,
                           pollutant_id = c(2, 3, 87, 90, 110))
```

Each county uses its own fuel supply, I/M program, barometric pressure and
(unless `meteorology` is given) MOVES default hourly weather
(`movesr_default_meteorology()`).

## Local inputs

All MOVES location inputs can be replaced:

```r
movesr_running_rates(
  db,
  county_id = 26161,               # default county (fuel region, I/M)
  barometric_pressure = 21.4,      # inHg, e.g. Quito (~2,850 m)
  fuel_supply = my_fuels,          # formulations + market shares + properties
  im_coverage = FALSE,             # or a user IMCoverage table
  meteorology = my_hourly_weather
)
```

`my_fuels` uses the MOVES `FuelFormulation` columns (RVP, sulfurLevel,
ETOHVolume, aromaticContent, olefinContent, benzeneContent, e200, e300, T50,
T90, BioDieselEsterVolume, ...) plus `fuelTypeID` and `marketShare`. Fuel
effects are recomputed from the MOVES expressions for those fuels.

## Export to vein

```r
movesr_export_vein("factors", running = running, start = start, evap = ev,
                   database = db)

f  <- vein::moves_factors("factors/rateperdistance.csv",
                          "factors/movesrun.csv", "factors/vehicles.csv")
vein::ef_moves("NOx", model_year = 2020, speed = 50, factors = f,
               temperature = 20, rel_humidity = 50)
```

## Inspectable layers

Each MOVES stage is an exported function:

| Stage | Function |
|---|---|
| Parquet database | `movesr_database()`, `movesr_tables()`, `movesr_table()` |
| Source bins | `movesr_sourcebins()` |
| Drive-schedule brackets | `movesr_schedule_fractions()` |
| Operating modes (Go drive-cycle generator) | `movesr_opmodes()` |
| Meteorology, heat index, humidity | `movesr_meteorology()` |
| NOx humidity correction | `movesr_nox_humidity()` |
| Air conditioning | `movesr_ac_factor()` |
| Fuel supply / E85 properties | `movesr_fuel_supply()` |
| General fuel effects | `movesr_fuel_ratios()`, `movesr_fuel_adjustment()` |
| Complex/Predictive Model + sulfur model | `movesr_complex_ratios()` |
| EV fleet-average adjustment | `movesr_ev_adjustment()` |
| I/M | `movesr_im_fraction()` |

## License

MOVES is GPL. This package uses the GPL-compatible license `GPL (>= 2)`.
