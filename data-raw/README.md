# movesr data preparation

The base-rate engine will use the EPA MOVES default database as input data,
but not the Java or MariaDB runtime.

## Source

```text
../moves/database/Setup/movesdb20260729.zip
```

This is the MOVES6.0.0 default database. It is not a precomputed factor table.

## Target store

Convert the tables needed by each process family to Parquet or DuckDB. Suggested
layout:

```text
movesdb/
├── emissionrate.parquet
├── emissionratebyage.parquet
├── sourcebin.parquet
├── sourcebindistribution.parquet
├── opmodedistribution.parquet
├── avgspeeddistribution.parquet
├── avgspeedbin.parquet
├── fuelt*.parquet
├── zonemonthhour.parquet
└── pollutantprocessassoc.parquet
```

The conversion is a one-time data-preparation step. The engine itself reads only
local Parquet files.

## Phase 1 result

The MOVES6 default database has been converted:

```text
moves_work/movesdb-parquet/
├── catalog.csv
├── activitytype.parquet
├── ...
└── year.parquet
```

- 240 tables
- 10.0 MiB total
- 0 row-count mismatches
- no Java
- MariaDB used only during conversion, then shut down

The conversion used DuckDB 2.0 with its MySQL extension:

```sql
INSTALL mysql;
LOAD mysql;
ATTACH 'host=127.0.0.1 port=13316 user=root database=movesdb20260729'
  AS moves (TYPE mysql, READ_ONLY);
COPY (SELECT * FROM moves."emissionratebyage")
  TO 'movesdb-parquet/emissionratebyage.parquet'
  (FORMAT PARQUET, COMPRESSION ZSTD);
```

`movesr_database()` reads `catalog.csv` and validates that all 240 Parquet files
exist and that catalog row counts match.

### Empty tables are expected

The default database contains empty `sourcebin`, `sourcebindistribution` and
`opmodedistribution` tables. MOVES creates them during a run from
`sampleVehiclePopulation`, fuel/regulatory-class data, drive schedules and
source-type physics. The next engine layer must generate them.

## Engine order

1. running exhaust (`RatePerDistance`),
2. start exhaust (`RatePerStart`),
3. evaporative (`RatePerVehicle`, `RatePerProfile`),
4. idle/APU (`RatePerHour`),
5. brake/tire, refueling, remaining processes.

Validation must compare against actual MOVES output, not against assumptions.
The `vein` reference dataset is the first golden set.

## Current limitation

`movesr` 0.0.1 contains the scenario and factor-table layer. It does not yet
contain the base-rate calculator. The base-rate calculator will be added behind
`movesr_generate()` so the public API remains stable.
