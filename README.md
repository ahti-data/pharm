# pharm

## Overview

This repository contains R scripts for building summary datasets and outcome tables related to:

- **GGZ diagnoses and utilization**
- **Medication use / ATC-based medication patterns**
- **Iteration 0 results exports** for downstream analysis

The code works by reading raw CBS-linked source data, harmonizing codes across years and formats, reshaping person-year records, and exporting parquet / Excel outputs for analysis.

## Linked Datasets

The scripts reference several CBS-linked source sources and local parquet/SAV files:

- **GGZ Vektis declarations (2014–2021)**
  - Used to derive person-year GGZ diagnosis indicators from declaration records.
  - Adds historical coverage before the ZPM era.
- **GGZ ZPM / prestatie data (2022–2024)**
  - Used for newer GGZ diagnosis and performance-based records.
  - Supports prevalence and cost/utilization summaries in the post-2021 period.
- **Medication tab / ATC data**
  - Used to identify medication use by ATC4 group.
  - Focuses on psychotropic medication classes:
    - `N05A`, `N05B`, `N05C`, `N06A`, `N06B`, `N06D`
- **Demographic / socioeconomic parquet files**
  - Used to merge in:
    - age
    - sex
    - education (`hbopl`)
    - income class (`inkomen_klasse`)
    - SES/WOA category (`seswoa_cat`)

### What the datasets add

- **GGZ datasets** provide diagnosis-based mental health indicators and GGZ service use.
- **Medication datasets** provide medication exposure and polypharmacy-style grouping.
- **Demographics** enable stratified prevalence and outcome reporting by population subgroup.

## Repository Structure

```text
src/
  input.R
  00_ggz_diagnoses.R
  00_medication_use.R
  01_compute_ggz_prevalences.R
  02_atc_results_iteration0.R
  02_ggz_results_iteration0.R
data/
  raw/
  results/
```

### Key folders

- `src/` — R scripts for data preparation and result generation
- `data/raw/` — intermediate parquet outputs
- `data/results/iteration0/` — exported iteration 0 result tables

## Source Code Summary

### `src/input.R`
Shared configuration and lookup tables:

- loads helper functions from external utility scripts
- defines ATC codes and GGZ product/diagnosis code groups
- sets year ranges for Vektis and ZPM processing
- defines the diagnosis renaming map used to harmonize GGZ codes across data sources

### `src/00_ggz_diagnoses.R`
Builds a person-year GGZ diagnosis dataset from Vektis declaration files:

- reads yearly parquet files for 2014–2021
- cleans and standardizes diagnosis codes
- filters invalid or inconsistent records
- expands treatment spans into person-year observations
- writes:
  - `data/raw/00_ggz_diagnoses_tot_2021.parquet`
  - `data/diag_productcode_combi.xlsx`

### `src/00_medication_use.R`
Builds a medication-use dataset from medication tab files:

- reads yearly medication SAV files
- filters to selected ATC4 psychotropic classes
- creates person-level ATC dummies
- counts number of medication classes per person
- assigns medication class groups:
  - monotherapy
  - 2-class polypharmacy
  - 3-class polypharmacy
  - 4+ classes
- merges demographic variables
- writes:
  - `data/raw/00_atc_2024.parquet`

### `src/01_compute_ggz_prevalences.R`
Computes GGZ prevalence tables:

- loads historical and newer GGZ diagnosis parquet files
- harmonizes diagnosis naming across Vektis and ZPM formats
- computes prevalence per 1,000 by subgroup
- supports stratification by:
  - SES/WOA
  - income class
  - age group
  - education
- intended to produce comparable prevalence outputs across years

### `src/02_atc_results_iteration0.R`
Creates iteration 0 medication outcome tables:

- merges ATC data with demographics
- fills missing ATC indicators with zero
- computes prevalence of each medication class per subgroup
- summarizes common ATC combinations
- writes:
  - `data/results/iteration0/iteration0_atc.xlsx`

### `src/02_ggz_results_iteration0.R`
Creates iteration 0 GGZ outcome tables:

- reads 2024 GGZ ZPM data
- defines GGZ type indicators:
  - basis GGZ
  - specialist GGZ
  - verblijf/day combinations
- merges demographics
- removes records not suitable for comparison
- computes:
  - total costs
  - number of users
  - mean cost per user
  - subgroup-specific summaries
- intended to export GGZ result tables for iteration 0 analysis

## Output Artifacts

Generated outputs referenced in the scripts include:

- `data/raw/00_ggz_diagnoses_tot_2021.parquet`
- `data/raw/00_atc_2024.parquet`
- `data/diag_productcode_combi.xlsx`
- `data/results/iteration0/iteration0_atc.xlsx`

No output files were included in the uploaded bundle itself.

## Next Steps

- Add a project-level `README` with exact input file locations and access requirements.
- Document the external helper scripts referenced in `src/input.R`:
  - `m_functions.R`
  - `demog_functions.R`
- Add a reproducible pipeline entrypoint, for example:
  - `targets`
  - `Makefile`
  - or a single orchestration script
- Replace hard-coded drive paths with configurable project paths.
- Add a data dictionary for:
  - GGZ diagnosis groups
  - ATC classes
  - demographic variables
- Consider adding unit checks for:
  - code harmonization
  - year coverage
  - merge completeness
