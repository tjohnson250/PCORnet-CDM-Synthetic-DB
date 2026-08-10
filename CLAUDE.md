# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## Project Overview

This repository contains a synthetic data generator for PCORnet Common
Data Model (CDM) and Master Patient Index (MPI) databases. It generates
data in R and writes directly to Microsoft SQL Server databases.

## Running the Code

``` r
# Load the API
source("R/pcornet.R")

# Generate databases (default: 100 patients with clinical profiles)
dbs <- create_pcornet_database(
  server = "localhost",
  uid = "sa",
  pwd = "YourPassword123"
)

# Or with Windows authentication
dbs <- create_pcornet_database(
  server = "localhost",
  trusted_connection = TRUE
)

# Access the connections
dbs$cdw  # Clinical Data Warehouse (SQL Server connection)
dbs$mpi  # Master Patient Index (SQL Server connection)
```

## Function API

### Main Functions

- `create_pcornet_database(n_patients, mode, server, cdw_database, mpi_database, uid, pwd, ...)` - Generate synthetic data and write to SQL Server
- `load_pcornet_database(server, cdw_database, mpi_database, uid, pwd, ...)` - Connect to existing SQL Server databases
- `get_database_summary(con_cdw, con_mpi)` - Get table row counts

### Generation Modes

``` r
# Enhanced mode (default) - Clinically coherent profiles
dbs <- create_pcornet_database(n_patients = 500, server = "localhost", uid = "sa", pwd = "pw")

# Random mode - No clinical logic
dbs <- create_pcornet_database(n_patients = 500, mode = "random", server = "localhost", uid = "sa", pwd = "pw")

# Synthea mode - Import from Synthea CSV
dbs <- create_pcornet_database(mode = "synthea", synthea_dir = "path/to/csv", server = "localhost", uid = "sa", pwd = "pw")

# Synthea mode - reload only certain tables (leaves the rest untouched, and
# skips reading source CSVs those tables don't need)
dbs <- create_pcornet_database(mode = "synthea", synthea_dir = "path/to/csv",
                               tables = "IMMUNIZATION",
                               server = "localhost", uid = "sa", pwd = "pw")
```

### SQL Server Connection Options

- `server`, `uid`, `pwd` - Basic SQL Server authentication
- `trusted_connection = TRUE` - Windows integrated authentication
- `connection_string` / `mpi_connection_string` - Full ODBC connection strings
- `driver` - ODBC driver name (default: "ODBC Driver 18 for SQL Server")
- `cdw_database` / `mpi_database` - Database names (default: "PCORnet_CDW" / "MPI")
- `overwrite` - Whether to overwrite existing tables (default: TRUE)
- `batch_size` - Rows per batch for large table writes (default: 10000)

## Architecture

### Two-Database Structure

**MPI Database** (`mpi_database`, default "MPI") - Master Patient Index for patient identity management:
- `EnterpriseRecords`: Core patient demographics (Uid, name, DoB, SSN, address, phone)
- `EnterpriseRecords_Ext`: Extended demographics and system identifiers
- `Mpi`: Cross-reference mapping between source systems and unified patient identifiers
- `MPI_Src`: Source system definitions

**CDW Database** (`cdw_database`, default "PCORnet_CDW") - PCORnet Common Data Model clinical tables:
- `DEMOGRAPHIC`: Patient demographics linked to MPI via UID
- `DEATH`: Death records for deceased patients
- `ENCOUNTER`: Patient encounters (IP, ED, AV, OA, IS types)
- `DIAGNOSIS`: ICD-10 diagnoses linked to encounters
- `PROCEDURES`: CPT procedures linked to encounters
- `LAB_RESULT_CM`: Laboratory results with LOINC codes
- `PRESCRIBING`: Medication prescriptions with RxNorm codes
- `VITAL`: Vital signs (height, weight, BP, BMI, smoking status)
- `OBS_CLIN`: Clinical observations with LOINC codes (surveys, exams, social
  history, and the vital signs VITAL has no column for)
- `IMMUNIZATION`: Administered vaccine doses with CVX codes
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

# Join data across databases using fully qualified names
dbGetQuery(dbs$cdw, "
  SELECT d.PATID, d.BIRTH_DATE, d.SEX
  FROM DEMOGRAPHIC d
  WHERE d.isDeceased = 'N'
")

# Join data across databases using dplyr in R
library(dplyr)
demographic <- dbReadTable(dbs$cdw, "DEMOGRAPHIC")
enterprise <- dbReadTable(dbs$mpi, "EnterpriseRecords")
inner_join(demographic, enterprise, by = c("UID" = "Uid")) %>%
  select(PATID, First, Last, BIRTH_DATE) %>%
  head(10)
```

## Dependencies

Required R packages:
- DBI
- odbc
- dplyr
- lubridate

### SQL Server ODBC Driver

Install the Microsoft ODBC Driver for SQL Server:
- **Windows**: Download from Microsoft
- **Mac**: `brew install microsoft/mssql-release/msodbcsql17`
- **Linux**: See Microsoft documentation for your distribution
