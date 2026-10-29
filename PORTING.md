# movesr porting notes

What has been ported from MOVES6, from which source files, how it was
validated, and what remains. Goal: a Java-free, MariaDB-free on-road MOVES rate
engine that reproduces MOVES output for any scenario inputs.

## Summary

| Stage | MOVES source | movesr | Status |
|---|---|---|---|
| Default database | `movesdb20260729.zip` | Parquet catalog | complete |
| Source bins, fuel usage | `SourceBinDistributionGenerator.java` | `movesr_sourcebins()`, `.movesr_context()` | validated |
| Drive-schedule brackets | Go `findDriveCycles` | `movesr_schedule_fractions()` | validated |
| Running operating modes | Go `calculateDriveCycleOpModeDistribution` | `movesr_opmodes()` | validated |
| Model-year physics | `SourceTypePhysics.java` | physics rows in `movesr_opmodes()` | validated |
| SB-weighted rates, full-AC, EV adjustment | `BaseRateGenerator.java`, Go `baserategenerator` | `.movesr_sbweighted()`, `movesr_ev_adjustment()` | validated |
| General fuel effects | `FuelEffectsGenerator.doGeneralFuelRatio` | `movesr_fuel_ratios()` | validated |
| E70/E85 properties | `alterHighEthanolFuelProperties` | `.movesr_high_ethanol()` | validated |
| Complex/Predictive Model | `doCOCalculations`, `doPredictiveCalculations` | `movesr_complex_ratios()` | validated |
| Sulfur model | `database/SulfurModel.sql` | `.movesr_sulfur_model()` | validated |
| Meteorology | `MeteorologyGenerator.java` | `movesr_meteorology()` | validated |
| Temperature, NOx humidity | Go `baseratecalculator` | `.movesr_temp_factor()`, `movesr_nox_humidity()` | validated |
| Air conditioning | `BaseRateCalculator.sql`, Go | `movesr_ac_factor()` | validated |
| I/M | `BaseRateCalculator.sql` | `movesr_im_fraction()` | implemented (no I/M in the reference county) |
| EmissionRateAdjustment, EV efficiency | Go `baseratecalculator` | `.movesr_static_factors()` | implemented |
| Start operating modes | `StartOperatingModeDistributionGenerator.java` | `movesr_start_rates()` | validated |
| Start temperature | Go `startTempAdjust` | `.movesr_start_temp()` | validated |
| RegClass aggregation | Go `calculateActivityWeight` | `.movesr_finish()` | validated |
| HC speciation (CH4, NMHC, NMOG, TOG, VOC) | Go `calc/hcspeciation` | `.movesr_hc_factors()` | validated |
| CO2, CO2e | `CO2AERunningStartExtendedIdleCalculator.sql` | `.movesr_chain()` | validated |
| SO2 | `SO2Calculator.sql` | `.movesr_chain()` | validated |
| PM2.5 species (EC, OC, SO4, H2O, NonECPM, total) | `SulfatePMCalculator.sql` | `.movesr_sulfate_pm()` | validated |
| PM10 | `PM10EmissionCalculator.sql`, `PM10BrakeTireCalculator.sql` | `.movesr_chain()` | validated |
| Brakewear (9), tirewear (10) | Go generator, `AverageSpeedOperatingModeDistributionGenerator` | `movesr_running_rates(process_id=)` | validated |
| Region runs, default weather | — | `movesr_region_rates()` | validated |
| Activity (population, VMT, SHO, SHP) | `TotalActivityGenerator.java`, `ActivityCalculator.sql` | `movesr_activity()` | validated |
| Tank temperatures, soak fractions | `TankTemperatureGenerator.java` | `movesr_tank_temperature()` | validated |
| Tank fuel RVP/ethanol | `TankFuelGenerator.java` | `movesr_average_tank_gasoline()` | validated |
| Evap permeation (11), leaks (13) | `EvaporativePermeationCalculator.sql`, `LiquidLeakingCalculator.sql` | `movesr_evap_rates()` | validated |
| Evap vapor venting (12) | `MultidayTankVaporVentingCalculator.sql` | `movesr_tvv_hourly()`, `movesr_evap_rates()` | validated |
| Evap on running roads | same, operating mode 300 | `movesr_evap_running_rates()` | validated |
| Evap simplified method | — (movesr surrogate) | `movesr_tank_temperature_simple()` | error documented below |
| Idle/APU, refueling, air toxics | various | — | **not ported** |

## Validation

Golden set: MOVES6.0.0 runs generated with the real model for VEIN
(`vein/inst/extdata/moves`), Washtenaw County MI defaults, calendar year 2025,
January, hours 1–3 at 68/50/86 F and 50 % RH, all 13 source types and every
valid fuel.

| Table | Rows | Matched | Max relative difference |
|---|---:|---:|---:|
| `rateperdistance.csv` (running CO, NOx) | 86,304 | 86,304 | 6.5e-6 |
| `rateperstart.csv` (start CO, NOx) | 5,394 | 5,394 | 7.2e-6 |

MOVES writes 6 significant digits from single-precision columns; the residual
is that rounding. Reproduce with:

```sh
MOVESR_DB=.../movesdb-parquet DUCKDB=.../duckdb \
MOVESR_REFERENCE=.../vein/inst/extdata/moves \
Rscript -e 'testthat::test_dir("tests/testthat")'
```

DMV golden set (`data-raw/dmv/`, generated with MOVES6 by
`generate_dmv.py`; DC 11001, Montgomery 24031, Fairfax 51059; July weekday
hours 16-17; default inputs):

| Table | Rows | Matched | Max relative difference |
|---|---:|---:|---:|
| `rateperdistance` running, 21 pollutants | 2,522,784 | all | 8.1e-6 (energy, J: 1.6e-5) |
| `rateperdistance` brake/tire PM2.5, PM10 | 529,536 | all | 7.8e-6 |
| `rateperstart`, 21 pollutants | 155,112 | all | 6.6e-6 |

Evaporative golden sets (off-network `RatePerVehicle`/`RatePerProfile`, and
`RatePerDistance` on urban unrestricted roads), THC, NMHC, NMOG, TOG, VOC:

| Run | Rows | Matched | Max relative difference |
|---|---:|---:|---:|
| DC, 24 hours, processes 11/12/13 (`dmv_evap24.mrs`) | 190,440 | all | 8.7e-4 (70 E85 venting rows > 1e-4; rest < 7e-5) |
| 3 zones, hours 16-17, processes 11/13 per vehicle | 6,348 | all | 2.0e-5 |
| 3 zones, hours 16-17, process 12 per profile (`tvv_hours = 16:17`) | 3,174 | all | 1.3e-5 |
| 3 zones, hours 16-17, 11/12/13 per distance, 16 speed bins | 761,760 | all (2,880 NULL in MOVES) | 3.6e-5 |

Weekend evaporative rates (Washtenaw events run in `vein/inst/extdata/moves`,
January, hours 1-3, model years 2000-2025): all 3,186 THC rates within 2.9e-5
with `e85_tank_properties = "e10"`.

A second MOVES run of the same RunSpecs is not bit-identical: 15,022 of
4,361,724 rates changed, by up to 1.0e-5.

Venting compounds hourly differences of cumulative single-precision values;
tank temperatures known only to MOVES' printed precision (feeding MOVES' own
printed `ColdSoakTankTemperature` gives 2.7e-3) explain the E85 residual.
Intermediate tables matched individually: sample-vehicle soaking (3e-9),
`stmyTVVEquations` (7e-15), `RegClassSourceTypeFraction` (2e-16), population,
travel fraction, annual VMT, SHO and SHP (5e-6), average tank temperatures
(5e-4 F, print precision), soak activity fractions (5e-7), cold-soak initial
hour fractions (5e-6).

Findings from the DMV set: MOVES reports 0 for model years whose age has
`ageFraction = 0` (activity weighting; e.g. transit buses aged 36, 38, 39 in
2025); energy is reported in the RunSpec unit (J); `EmissionRateAdjustment`
and temperature adjustments may have several model-year ranges per key.

VEIN round trip: `movesr_export_vein()` output read by `vein::moves_factors()`
gives 0.009754123 g/km NOx (2020 gasoline car, 50 km/h, 20 C); the MOVES
reference bundled in VEIN gives 0.009754099 g/km.

## Database

240 tables, 10.0 MiB of Parquet, converted once with a temporary MariaDB and
DuckDB's MySQL extension; 0 row-count mismatches; SHA-256 per table in
`catalog.csv`. Read through the DuckDB CLI (`options(movesr.duckdb=)` or
`DUCKDB`); no `arrow`. Tables are cached in memory per session.

Tables that MOVES fills at run time and are empty in the default database:
`sourcebin`, `sourcebindistribution`, `opmodedistribution`, `fueladjustment`,
`generalfuelratio`, `criteriaratio`, `starts`. movesr generates the
equivalent intermediate tables.

## Running exhaust (process 1)

### Source bins

`sourceBinID = 1e18 + fuel*1e16 + engTech*1e14 + regClass*1e12 +
shortModYrGroup*1e10` (character strings). Distribution = sum of
`stmyFraction` by source type/model year, model years `year-40 .. year`.
`FuelUsageFraction` converts equipped fuels to used fuels:
`SBD[used bin] = sum(usageFraction * SBD[equipped bin])`; e.g. 96.9 % of E85
flex-fuel activity is reported as gasoline in Washtenaw 2025.

### Operating modes — the Go generator, not the Java SQL

MOVES6 computes drive-cycle operating modes in
`generators/baserategenerator` (`USE_EXTERNAL_GENERATOR_FOR_DRIVE_CYCLES =
true`). The Java SQL in `RatesOperatingModeDistributionGenerator` is not used
and gives different results for heavy-duty cycles. Go rules:

- speed < 1 mph → idle (mode 1);
- acceleration `speed[t]-speed[t-1]` (mph/s); **0 when the previous second is
  missing** (heavy-duty cycles have gaps); first second copies the second;
- braking (0): `a <= -2`, or `a[t], a[t-1], a[t-2] < -1` with those seconds
  present;
- `VSP = (A v + B v^2 + C v^3 + M v a) / fixedMassFactor`, v and a in m/s,
  double precision;
- VSP/speed bins from `OperatingMode` 2–99 except 26 and 36; speed bounds in
  mph;
- only seconds > 0 counted; fractions per schedule, then
  `sum(modeFraction * driveScheduleFraction)`;
- bracketing: lowest schedule ≤ bin speed and highest ≥ bin speed,
  `lo = (hi - bin)/(hi - lo)`; outside the range the nearest schedule.

One distribution per `SourceUseTypePhysics` row (source type, regulatory class,
model-year range). Rates join on base operating mode, regClass and model year
within the physics range.

Two earlier bugs were found by the parity test: speed bounds compared in m/s
instead of mph, and forward acceleration after gaps.

### Base rates

Rates mode normalises by source-bin activity:

```text
MBR    = sum(sbd * meanBaseRate) / sum(sbd)            (also MBR_IM)
ACAdj  = sum(sbd * meanBaseRate * (fullACAdjustment - 1)) / sum(sbd)
EV     = min(1 / (1 - ev*m / ((1-ev) + ev*m)), adjustmentCap)   non-EV rates
ER     = sum(MBR * opModeFraction) / avgBinSpeed       (g/mile)
```

`ev` is the national EV sales fraction by model year and fleet-average group
(`SourceTypeYear`, `SourceTypeAgeDistribution` age 0, `SampleVehiclePopulation`).

### Calculator (per fuel formulation, then market share)

```text
rate = fuelRatio * T * [(1-f)(ER + AC*ERAC) + f(ERIM + AC*ERIMAC)] * ERA / EVeff
T    = 1 + (t-75)(A + (t-75)B)                      general
     = (1 + (t-75)(A + (t-75)B)) * K                NOx, non-diesel
     = (1 + (77-t)A) * K  (0 above 77 F)            NOx, diesel
K    = 1 - 0.0329 (SH - 10.71)   CFR 86, SH in g/kg bounded [3, 17.71]
AC   = clamp(A + HI(B + C HI), 0, 1) * ACPenetration * functioningAC
f    = sum(IMFactor * complianceFactor * 0.01)
```

`TemperatureAdjustment` is looked up by regClass, then regClass 0, else zero
terms (NOx keeps K). Meteorology: heat index = T below 78 F, NWS regression
above (max 120); specific humidity from the Goff–Gratch style vapour pressure
and county barometric pressure.

### Fuel effects

1. `GeneralFuelRatioExpression` strings are MySQL expressions; movesr
   translates `if`, `least`, `greatest`, `ln`, `and/or` and evaluates them in R
   against each supplied formulation. NULL results (e.g. `ln(0)`) become 1.
2. E70/E85 formulations take E10 properties (`E10FuelProperties`, region then
   nation), keep RVP and get `altRVP`.
3. For THC/CO/NOx (101,102,201,202,301,302) the expression results are
   `CriteriaRatio`; the first matching row wins (MOVES `insert ignore`).
4. Model years without an expression (pre-2001 gasoline) use the Complex Model
   (CO) or the Predictive Model (THC, NOx) followed by the sulfur model:
   ```text
   predictive: sum_m w_m exp(T_m) / sum_m w_m exp(B_m)
   CO:         1 + sum_m w_m (exp(T_m)/exp(B_m) - 1)
   T_m, B_m:   sum_cmp coeff * term((fp - centre)[/stddev])
   ```
   weights `FuelModelWtFactor`, centres `MeanFuelParameters`, base fuels
   `BaseFuel`, coefficients `ComplexModelParameters` (CO sums coeff1..3), then
   `SulfurModel.sql` (normal/high emitters, IR factor, minimum adjustment).
5. `factor = sum(marketShare * (ratio + GPAFract (ratioGPA - ratio)))`.

### Regulatory-class aggregation

`rate(regClass 0) = sum(rate_rc * sbdTotal_rc) / sum(sbdTotal_rc)`, from Go
`calculateActivityWeight`; activity cancels.

## Start exhaust (process 2)

- Operating modes 101–108 from `StartsOpModeDistribution` by source type, hour
  and day type (age-invariant; MOVES keeps the first age).
- Per mode: `ER = MBR*f + f * TA(t)` with `StartTempAdjustment` by fuel,
  pollutant/process, model-year group and mode:
  POLY `T'(A + T'(B + T'C))`, LOG `B exp(A T') + C`, `T' = min(t,75) - 75`;
  PM 11202/11802 multiplicative.
- Fuel ratios, general temperature factor, I/M and EV/regClass stages as for
  running; no AC.
- Zero activity gives a zero rate (MOVES activity weighting): e.g. school buses
  on weekends (`StartsPerDayPerVehicle = 0`).

## Chained pollutants

Computed per regulatory class, then aggregated like the base pollutants.

```text
CH4  = THC * CH4THCRatio(process, fuel subtype, regClass, MY)
NMHC = THC * (1 - CH4THCRatio)
NMOG, VOC = NMHC * (c + k * volToWtPercentOxy * oxygenate volume)
TOG  = NMOG + CH4
E85, MY >= 2001: NMOG/VOC from "alt THC" (THC fuel effects with E10 RVP) and
     E10 (subtype 12) speciation
CO2  = energy * sum(share*carbonContent) * sum(share*oxidationFraction) * 44/12
CO2e = sum(CO2, CH4, N2O * globalWarmingPotential)
SO2  = SulfateEmissionRate * sum(share*sulfur) * energy / sum(share*energyContent)
SO4  = NonECPM * f_SO4 * (1 + b (sulfur/sulfur_base - 1))      share-weighted
H2O  = NonECPM * f_H2O * (same sulfur adjustment)
rest = NonECPM * max(1 - f_SO4 - f_H2O, 0) * fuel ratio(120)
EC, SO4, H2O, rest *= crankcase split (primary process share)
PM2.5 = EC + SO4 + H2O + rest; NonECPM = SO4 + H2O + rest
OC = rest * PMSpeciation(120 -> 111);  PM10 = PM2.5 * PM10PM25Ratio
```

HC speciation is done per fuel formulation, so `fuel_supply` needs
`fuelSubtypeID` when chained pollutants are requested.

## Evaporative processes (11, 12, 13)

### Activity (`TotalActivityGenerator`, base years)

```text
population[st,age] = sourceTypePopulation * ageFraction
travelFraction     = (pop/pop_HPMS) * relativeMAR / sum(...)       by HPMS type
annualVMT          = HPMSBaseYearVMT * roadTypeVMTFraction * travelFraction
hourVMT            = annualVMT * month * day * hour fractions / (noOfDays/7)
SHO                = hourVMT / averageSpeed      (avgSpeedDistribution)
SHP                = population * noOfRealDays - sum_roads SHO   (VMT > 0)
zonePopulation     = population * sum(rtVMT * SHOAlloc) / sum(rtVMT)
parked fraction    = SHP * SHPAllocFactor / (zonePopulation * noOfRealDays)
```

MOVES' evap `RatePerVehicle` is emissions / population for the hour of an
average day of the day type, so the per-vehicle rate is
`rate per parked hour * parked fraction`. Non-base years take the default
`SourceTypeYear` population of that year (an approximation of MOVES' growth
step).

### Tank temperatures (`TankTemperatureGenerator`)

- Cold soak: quarter-hour recursion on the 24-hour ambient profile (Java
  `float` emulated).
- Sample-vehicle trips of the day type split by hour; hot-soak events after
  each trip; operating
  `Toff = Ton + ((A + B (95 - Ton))/1.2) * minutes/60`, max 140 F; hot soak
  minute by minute `T = Toff + 1.4 sum(Tamb - T)/60` until `T <= Tcold + 3`;
  the next trip starts from the soak temperature.
- Average tank temperature for modes 150/151/300 by hour and tank-temperature
  group; soak activity fractions; cold-soak initial-hour fractions (which hour
  each cold-soaking vehicle started soaking).
- About 13 s per zone/month/day type for weekdays, 6 s for weekends (the
  minute-by-minute hot soak runs vectorised over all trip ends).

### Calculators

```text
mode fractions  150/151 = soakActivityFraction * (1 - fractionOfOperating), 300 = rest
permeation      = MBR * fuelAdj(ethanol bin, fuel MY group) * sum_m A exp(B Ttank_m) f_m
                  split by RegClassSourceTypeFraction
leaks           = sum_m MBR_m f_m
venting  151    = sum_bins SBD * hourlyTVV(regClass)
         150    = MBR150
         300    = MBR300 * tempAdj(max(T,40)) * rvpAdj(T, tank RVP)  (gasoline, E85)
I/M             = (1-f) rate + f rateIM
```

All rates are by regulatory class (`WithRegClassID` is always enabled), then
HC-speciated per fuel formulation (evap THC is split by market share, no fuel
ratio) and summed over regulatory classes.

Multiday vapor venting (`USE_MULTIDAY_DIURNALS = true`; the older
`TankVaporVentingCalculator.sql` is not used):

```text
peak hour   = earliest max of round(Tcold, 2)
TVG(I,h)    = tankSize (1-fill) A exp(B RVP) (exp(C Th) - exp(C TI))  if TI < Th, h <= peak
              interpolated L/H altitude by barometric pressure, E0/E10 by tank ethanol
increments  = TVG(I,h) - TVG(I,h-1); sums I..h, I..24, 1..h, h+1..24
day 1       Xn = TVG(I..h)
day 2       Xn = (1-backPurge) min(TVG(I..24), capacity) + TVG(1..h)
day 3-5     Xn = (1-backPurge) min(Xn-1 + TVG(h+1..24), capacity) + TVG(1..h)
TVV         = rcFrac * max(0, (1-lf) tvvEquation(Xn) + lf leakEquation(TVGdaily))
hourly      = TVV(h) - TVV(h-1) while Tcold rises, weighted by initial-hour
              fractions (day 1) and multiday soaking fractions (days 2-5)
after peak  = 2 %, 1 %, 0.4 %, 0.05 % of the peak hour for hours +1..+4
```

`tvvEquation`/`leakEquation` are SQL strings from `cumTVVCoeffs` evaluated in
R (SQL NULL from `sqrt` of a negative is kept as missing). The multiday soaking
fractions come from first-start hours of the sample vehicles and the
`sampleVehicleSoakingDayBasis` factors (0.86, 0.87, 0.91, 0.93).

Tank fuel (`TankFuelGenerator`): Reddy RVP of the market-share average fuel,
weathered with the zone's evaporation temperature from the monthly min/max,
times the commingling factor.

On running roads only mode 300 applies and
`ratePerDistance = rate300 / avgBinSpeed`
(`movesr_evap_running_rates()`).

### Simplified method (`method = "simplified"`)

Same activity, tank fuel and calculators; only the tank model is replaced.
`movesr_tank_temperature_simple()` computes the cold-soak tank temperature
exactly and interpolates everything that needs the sample-trip simulation
from a library of exact results (`R/sysdata.rda`, built by
`data-raw/evap_library.R`): 113 MOVES default 24-hour profiles per day type
(20 daily-mean bins x 5 diurnal-range bins, plus the climate tails), storing
the hot-soak and operating tank-temperature rises over the cold-soak
temperature, the soak activity fractions and the cold-soak initial-hour
fractions. A new profile uses the 3 library profiles nearest in
(daily mean, diurnal range) with inverse squared distance weights; a warning
is given when the nearest is more than 5 F away. Hour-by-hour matching (on
ambient and cold-soak temperature, or on their gap) was tested and was not
more accurate. Venting error comes almost entirely from the hot-soak share of
parked time; the initial-hour fractions and the tank temperatures matter
little.

Error of daily THC totals versus the exact port, 20 random MOVES zone-months
not in the library (6-76 F daily mean), weekday and weekend, DC fleet and
fuel:

| Process | Mean abs. (weekday / weekend) | 90th pct | Max |
|---|---:|---:|---:|
| Permeation (11) | 0.15 % / 0.08 % | 0.38 % | 0.44 % |
| Vapor venting (12) | 4.2 % / 3.4 % | 6.1 % | 10 % (54 % / 22 % for a 6 F Alaska profile 10.7 F from the library, warned) |
| Leaks (13) | 0.11 % / 0.08 % | 0.25 % | 0.38 % |

Against MOVES itself (DC, July weekday, 24 hours, 12,696 rates per process):
daily totals +0.0004 % (11), +0.87 % (12), +0.09 % (13); median row error
0.07 %, 1.3 %, 0.05 %. Time per profile: 0.17 s versus 13 s (weekday) and
6 s (weekend) for the exact simulation.

### MOVES behaviours found while porting

- Vapor venting depends on the hours selected in the RunSpec: the worker's
  `HourDay` holds only those hours, so initial hours outside the selection
  are dropped and the multiday terms need hour 1. A 16-17 h run has no
  cold-soak venting at all. movesr uses all 24 hours by default
  (`tvv_hours = 1:24`, identical to a full-day MOVES run); `tvv_hours = 16:17`
  reproduces the restricted run.
- MOVES writes vapor venting to `RatePerProfile`, not `RatePerVehicle`.
- `stmyTVVEquations.leakFraction` is `leakFraction * regClassFraction`, and the
  vented vapor is again multiplied by the regulatory-class fraction; ported
  as is.
- The E85 tank fuel is not the same in every run. `FuelEffectsGenerator`
  replaces E85 properties with E10 values (cloning formulations per
  fuel-supply usage) and `TankFuelGenerator` reads the same table. The DMV
  events and DC 24-hour runs match the E85 formulation as supplied; the DMV
  running and Washtenaw events runs match the E10-substituted one. What
  decides this is not known (the two events runs have the same processes).
  Option `e85_tank_properties = "raw"` (default) or `"e10"`; 0.4 % (DMV
  running) to 1.7 % (Washtenaw, 86 F) on E85 venting.
- Rates for ages without population are NULL in `RatePerDistance` and absent
  from `RatePerVehicle`; movesr omits them.

## Not yet ported

- Off-network evap with `fractionOfOperating > 0` (project-scale links;
  movesr accepts it as an input).
- Extended idle and APU (90, 91), crankcase processes, refueling, air toxics.
- Project-scale links and off-network idle (road type 1).

For these, `vein` keeps reading MOVES-produced tables.

## Validation rules

A calculator is accepted only after: synthetic unit tests; a row-by-row
comparison with MOVES output; all rows, not a sample; explicit failure on
missing combinations.

## Licensing

MOVES is GPL. `movesr` is GPL (>= 2).
