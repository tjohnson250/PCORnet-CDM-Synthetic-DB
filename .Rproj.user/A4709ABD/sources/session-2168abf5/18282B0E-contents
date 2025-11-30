---
editor_options: 
  markdown: 
    wrap: 72
---

# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## Project Overview

This repository contains a synthetic data generator for PCORnet Common
Data Model (CDM) and Master Patient Index (MPI) databases. It creates
in-memory DuckDB databases populated with realistic test data for
healthcare research and development.

## Running the Code

``` r
# Execute the main script in R or RStudio
source("create_synthetic_database.R")
```

The script returns a list with two database connections: - `con_cdw` -
Clinical Data Warehouse (PCORnet CDM tables) - `con_mpi` - Master
Patient Index (patient identification and linkage)

## Saving and Loading Databases

The generation script automatically saves databases to disk files: -
`pcornet_cdw.duckdb` - Clinical Data Warehouse - `mpi.duckdb` - Master
Patient Index

To reload previously generated databases without regenerating data:

``` r
source("load_databases.R")
```

This is much faster than regenerating and useful for working with the
same test dataset across sessions.

## Architecture

### Two-Database Structure

**MPI Database** - Master Patient Index for patient identity
management: - `EnterpriseRecords`: Core patient demographics (Uid, name,
DoB, SSN, address, phone) - `EnterpriseRecords_Ext`: Extended
demographics and system identifiers (EPIC_PAT_ID, ALLSCRIPTS_PERSON_ID,
MHH_MRN, UTP_MRN, race/ethnicity, deceased status) - `Mpi`:
Cross-reference mapping between source systems (EPIC, ALLSCRIPTS,
MHH_COVID, UTP) and unified patient identifiers (Uid) - `MPI_Src`:
Source system definitions

**CDW Database** - PCORnet Common Data Model clinical tables: -
`DEMOGRAPHIC`: Patient demographics linked to MPI via UID - `DEATH`:
Death records for deceased patients - `ENCOUNTER`: Patient encounters
(IP, ED, AV, OA, IS types) - `DIAGNOSIS`: ICD-10 diagnoses linked to
encounters - `PROCEDURES`: CPT procedures linked to encounters -
`LAB_RESULT_CM`: Laboratory results with LOINC codes - `PRESCRIBING`:
Medication prescriptions with RxNorm codes - `VITAL`: Vital signs
(height, weight, BP, BMI) - `PROVIDER`: Provider directory

### Key Identifiers

-   **Uid**: Unified patient identifier linking MPI and CDW databases (1
    to N_PATIENTS)
-   **PATID**: PCORnet patient ID format (`PAT0000001` to
    `PAT{N_PATIENTS}`)
-   **ENCOUNTERID**: Encounter identifier (`ENC0000000001` format)
-   System-specific IDs: EPIC_PAT_ID, ALLSCRIPTS_PERSON_ID, MHH_MRN,
    UTP_MRN (with varying null rates)

### Data Generation Patterns

1.  **Patient-centric**: All data flows from N_PATIENTS (default: 3000)
2.  **Realistic missingness**: `sample_with_na()` function introduces
    configurable NULL rates
3.  **Data quality issues**: 5% of encounters intentionally fall before
    birth or after death
4.  **Temporal consistency**: Encounter dates drive diagnosis,
    procedure, lab, prescription, and vital dates
5.  **Variable system presence**: Patients appear in 60-95% of source
    systems (EPIC, ALLSCRIPTS, etc.)

### Configuration

Key parameters at top of script: - `N_PATIENTS`: Number of synthetic
patients (default: 3000) - `CURRENT_DATE`: Reference date for data
generation (default: 2024-11-27) - `set.seed(42)`: Ensures
reproducibility

### Common Sampling Gotchas

When using `sample()` with populations smaller than the sample size,
always add `replace = TRUE`:

``` r
# Example: Sampling middle initials for all patients
M = sample_with_na(LETTERS, N_PATIENTS, prob_na = 0.3, replace = TRUE)
```

## Exploring the Data

### Utility Functions

Use these helper functions to quickly explore the databases:

``` r
# Load utilities (automatically loaded by load_databases.R)
source("utility_functions.R")

# View first 10 rows of every table in both databases
print_all_tables(con_cdw, con_mpi)

# View summary of all tables (row counts, column counts)
print_table_summary(con_cdw, con_mpi)
```

## Querying the Databases

``` r
# List all tables
dbListTables(con_cdw)
dbListTables(con_mpi)

# Basic queries
dbGetQuery(con_cdw, "SELECT COUNT(*) FROM DEMOGRAPHIC")
dbGetQuery(con_mpi, "SELECT COUNT(*) FROM Mpi")

# Join data across databases - Option 1: Using ATTACH
dbExecute(con_cdw, "ATTACH 'mpi.duckdb' AS mpi")
dbGetQuery(con_cdw, "
  SELECT d.PATID, e.First, e.Last, d.BIRTH_DATE
  FROM DEMOGRAPHIC d
  JOIN mpi.EnterpriseRecords e ON d.UID = e.Uid
  LIMIT 10
")

# Join data across databases - Option 2: Using dplyr in R
library(dplyr)
demographic <- dbReadTable(con_cdw, "DEMOGRAPHIC")
enterprise <- dbReadTable(con_mpi, "EnterpriseRecords")
inner_join(demographic, enterprise, by = c("UID" = "Uid")) %>%
  select(PATID, First, Last, BIRTH_DATE) %>%
  head(10)
```

## Dependencies

Required R packages: - DBI - duckdb - dplyr - lubridate
