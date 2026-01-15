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
# Load the API
source("R/pcornet.R")

# Generate databases (default: 100 patients with clinical profiles)
dbs <- create_pcornet_database()

# Access the connections
dbs$cdw  # Clinical Data Warehouse
dbs$mpi  # Master Patient Index
```

## Function API

### Main Functions

- `create_pcornet_database(n_patients, mode, current_date, seed, save_to_disk, output_dir, synthea_dir, profile_weights)` - Generate synthetic databases
- `load_pcornet_database(cdw_path, mpi_path)` - Load existing databases from disk
- `get_database_summary(con_cdw, con_mpi)` - Get table row counts

### Generation Modes

``` r
# Enhanced mode (default) - Clinically coherent profiles
dbs <- create_pcornet_database(n_patients = 500)

# Random mode - No clinical logic
dbs <- create_pcornet_database(n_patients = 500, mode = "random")

# Synthea mode - Import from Synthea CSV
dbs <- create_pcornet_database(mode = "synthea", synthea_dir = "path/to/csv")
```

## Architecture

### Two-Database Structure

**MPI Database** - Master Patient Index for patient identity
management:
- `EnterpriseRecords`: Core patient demographics (Uid, name, DoB, SSN, address, phone)
- `EnterpriseRecords_Ext`: Extended demographics and system identifiers
- `Mpi`: Cross-reference mapping between source systems and unified patient identifiers
- `MPI_Src`: Source system definitions

**CDW Database** - PCORnet Common Data Model clinical tables:
- `DEMOGRAPHIC`: Patient demographics linked to MPI via UID
- `DEATH`: Death records for deceased patients
- `ENCOUNTER`: Patient encounters (IP, ED, AV, OA, IS types)
- `DIAGNOSIS`: ICD-10 diagnoses linked to encounters
- `PROCEDURES`: CPT procedures linked to encounters
- `LAB_RESULT_CM`: Laboratory results with LOINC codes
- `PRESCRIBING`: Medication prescriptions with RxNorm codes
- `VITAL`: Vital signs (height, weight, BP, BMI)
- `PROVIDER`: Provider directory

### Key Identifiers

- **Uid**: Unified patient identifier linking MPI and CDW databases (1 to n_patients)
- **PATID**: PCORnet patient ID format (`PAT0000001` to `PAT{n_patients}`)
- **ENCOUNTERID**: Encounter identifier (`ENC0000000001` format)
- System-specific IDs: EPIC_PAT_ID, ALLSCRIPTS_PERSON_ID, MHH_MRN, UTP_MRN (with varying null rates)

### Data Generation Patterns

1. **Patient-centric**: All data flows from n_patients parameter
2. **Realistic missingness**: Configurable NULL rates for various fields
3. **Data quality issues**: ~5% of encounters intentionally fall before birth or after death
4. **Temporal consistency**: Encounter dates drive diagnosis, procedure, lab, prescription, and vital dates
5. **Variable system presence**: Patients appear in 60-95% of source systems

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
source("utility_functions.R")

# View first 10 rows of every table in both databases
print_all_tables(dbs$cdw, dbs$mpi)

# View summary of all tables (row counts, column counts)
print_table_summary(dbs$cdw, dbs$mpi)
```

## Querying the Databases

``` r
# List all tables
dbListTables(dbs$cdw)
dbListTables(dbs$mpi)

# Basic queries
dbGetQuery(dbs$cdw, "SELECT COUNT(*) FROM DEMOGRAPHIC")
dbGetQuery(dbs$mpi, "SELECT COUNT(*) FROM Mpi")

# Join data across databases - Option 1: Using ATTACH
dbExecute(dbs$cdw, "ATTACH 'mpi.duckdb' AS mpi")
dbGetQuery(dbs$cdw, "
  SELECT d.PATID, e.First, e.Last, d.BIRTH_DATE
  FROM DEMOGRAPHIC d
  JOIN mpi.EnterpriseRecords e ON d.UID = e.Uid
  LIMIT 10
")

# Join data across databases - Option 2: Using dplyr in R
library(dplyr)
demographic <- dbReadTable(dbs$cdw, "DEMOGRAPHIC")
enterprise <- dbReadTable(dbs$mpi, "EnterpriseRecords")
inner_join(demographic, enterprise, by = c("UID" = "Uid")) %>%
  select(PATID, First, Last, BIRTH_DATE) %>%
  head(10)
```

## Dependencies

Required R packages: - DBI - duckdb - dplyr - lubridate
