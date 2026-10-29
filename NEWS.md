# movesr 0.0.1

- Evaporative processes: permeation (11), vapor venting (12, multiday
  diurnals) and fuel leaks (13), parked (`movesr_evap()`, per vehicle) and
  while driving (`movesr_evap_running_rates()`, per mile), with HC speciation.
  Ports of TotalActivityGenerator (`movesr_activity()`),
  TankTemperatureGenerator (`movesr_tank_temperature()`), TankFuelGenerator
  (`movesr_average_tank_gasoline()`) and the evap calculators
  (`movesr_evap_rates()`, `movesr_tvv_hourly()`). 961,722 MOVES evap factors
  reproduced.
- Simplified evap method (`method = "simplified"`,
  `movesr_tank_temperature_simple()`, `movesr_tank_library()`): tank
  temperatures and soak fractions interpolated from exact results.
- `movesr_region_rates(process = "evap")`; `movesr_export_vein(evap = )`
  writes `ratepervehicle.csv`/`rateperprofile.csv` for
  `vein::moves_evap_factors()`.
- Source bins honour `SourceTypePolProcess` (regulatory class and model-year
  group only where MOVES requires them).

- Chained pollutants: CH4, NMHC, NMOG, TOG, VOC, CO2, CO2e, SO2, PM2.5 total
  and species (EC, OC, SO4, H2O, NonECPM), PM10; brakewear and tirewear
  PM2.5/PM10.
- `movesr_region()`, `movesr_region_rates()`, `movesr_default_meteorology()`;
  DMV (DC-MD-VA) region.
- DMV golden set generated with MOVES6 (`data-raw/dmv`): 3,207,432 factors
  reproduced (running, start, brake, tire) within 8e-6.
- Activity-weighting rule (zero-population ages give 0) and energy units.

- MOVES6 running-exhaust engine `movesr_running_rates()`: reproduces all
  86,304 MOVES reference factors (13 source types, 4 fuels, 3 temperatures)
  within 6.5e-6.
- Start-exhaust engine `movesr_start_rates()`: reproduces all 5,394 reference
  factors within 7.2e-6.
- Ports of the MOVES Go drive-cycle operating-mode generator, model-year
  physics, fuel usage, EV fleet-average adjustment, general fuel effects,
  E85 property substitution, Complex/Predictive Model and sulfur model,
  meteorology, NOx humidity, air conditioning, I/M and regulatory-class
  aggregation.
- `movesr_run()` dispatches scenarios to the engines; `movesr_export_vein()`
  writes tables read by `vein::moves_factors()` and `vein::ef_moves*()`.
- Parquet catalog read through the DuckDB CLI; no Java, MariaDB or arrow.
- `movesr_factors()`, `movesr_generate()` and `movesr_write()` read and select
  existing MOVES rate tables.
