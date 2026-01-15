# PCORnet CDM Synthetic Database Generator

A synthetic data generator for [PCORnet Common Data Model (CDM)](https://pcornet.org/data/) and Master Patient Index (MPI) databases. Creates DuckDB databases populated with synthetic test data for development and testing purposes.

## Features

-   Generates synthetic patient data following PCORnet CDM v6.0 schema structure
-   Creates two linked databases: Clinical Data Warehouse (CDW) and Master Patient Index (MPI)
-   Configurable patient population size
-   Reproducible data generation with seeded randomization
-   Includes simulated data quality issues (missing values, temporal inconsistencies)
-   Three generation modes: clinically coherent profiles, random, or Synthea import

## Installation

``` r
# Install devtools if needed
install.packages("devtools")

# Install the package from GitHub
devtools::install_github("tjohnson250/PCORnet-CDM-Synthetic-DB")

# Or install from local source
devtools::install()
```

## Quick Start

``` r
# Load the packages
library(DBI)
library(pcornet.synthetic)

# Generate 100 patients with clinical profiles (default)
dbs <- create_pcornet_database()

# Access the databases
dbListTables(dbs$cdw)
dbListTables(dbs$mpi)
```

### Generation Modes

**Enhanced Mode (Default)** - Clinically coherent data with patient profiles:

``` r
dbs <- create_pcornet_database(n_patients = 500)
```

**Random Mode** - Randomly assigned clinical elements:

``` r
dbs <- create_pcornet_database(n_patients = 500, mode = "random")
```

**Synthea Mode** - Import from Synthea CSV output:

``` r
dbs <- create_pcornet_database(
  mode = "synthea",
  synthea_dir = "path/to/synthea/output/csv"
)
```

See `synthea/README.md` for Synthea setup instructions.

### Load Existing Databases

``` r
library(pcornet.synthetic)
dbs <- load_pcornet_database()
```

### View Database Summary

``` r
summary <- get_database_summary(dbs$cdw, dbs$mpi)
print(summary)
```

## Function Reference

### `create_pcornet_database()`

Main function to generate synthetic PCORnet databases.

| Parameter | Default | Description |
|-----------|---------|-------------|
| `n_patients` | 100 | Number of synthetic patients to generate |
| `mode` | "enhanced" | Generation mode: "enhanced", "random", or "synthea" |
| `current_date` | `Sys.Date()` | Reference date for data generation |
| `seed` | 42 | Random seed for reproducibility |
| `save_to_disk` | TRUE | Save databases to disk files |
| `output_dir` | "." | Directory for output files |
| `synthea_dir` | NULL | Path to Synthea CSV output (required for mode="synthea") |
| `profile_weights` | NULL | Named list to override default profile distribution |
| `sources` | NULL | Named list of MPI source systems (see Configuring Source Systems) |

**Returns:** List with `$cdw` (CDW connection), `$mpi` (MPI connection), and `$summary` (statistics)

### `load_pcornet_database()`

Load previously generated databases from disk.

| Parameter | Default | Description |
|-----------|---------|-------------|
| `cdw_path` | "pcornet_cdw.duckdb" | Path to CDW database file |
| `mpi_path` | "mpi.duckdb" | Path to MPI database file |

### `get_database_summary()`

Get row counts and statistics for all tables.

| Parameter | Description |
|-----------|-------------|
| `con_cdw` | Connection to CDW database |
| `con_mpi` | Connection to MPI database (optional) |

## Configuration Examples

``` r
library(pcornet.synthetic)

# Custom patient count
dbs <- create_pcornet_database(n_patients = 5000)

# Different random seed for unique data
dbs <- create_pcornet_database(seed = 123)

# In-memory only (don't save to disk)
dbs <- create_pcornet_database(save_to_disk = FALSE)

# Save to custom directory
dbs <- create_pcornet_database(output_dir = "data/output")

# Custom profile distribution (more diabetics)
dbs <- create_pcornet_database(
  n_patients = 1000,
  profile_weights = list(
    healthy = 0.30,
    diabetic = 0.30,
    cardiac = 0.15,
    respiratory = 0.10,
    mental_health = 0.10,
    multimorbid = 0.05
  )
)
```

### Configuring Source Systems

The MPI tracks which source systems each patient appears in. By default, four source systems are configured (EPIC, ALLSCRIPTS, MHH_COVID, UTP). You can customize the source systems using the `sources` parameter:

``` r
# Custom source systems
dbs <- create_pcornet_database(
  n_patients = 500,
  sources = list(
    EPIC = list(
      id_field = "EPIC_PAT_ID",
      description = "Epic EHR System",
      null_rate = 0.1  # 10% of patients missing from this system
    ),
    CERNER = list(
      id_field = "CERNER_MRN",
      description = "Cerner EHR System",
      null_rate = 0.3
    ),
    LAB_SYSTEM = list(
      id_field = "LAB_ID",
      description = "Laboratory Information System",
      null_rate = 0.2
    )
  )
)
```

Each source system requires:
- `id_field`: Column name for this system's patient identifier
- `description`: Human-readable description of the source system
- `null_rate`: Proportion of patients missing from this system (0.0 to 1.0)

## Database Schema

### MPI Database (Master Patient Index)

The MPI (Master Patient Index) is a separate database that manages patient identity across multiple source systems. In healthcare organizations, patients often have different identifiers in different systems (Epic, Cerner, lab systems, etc.). The MPI solves this by:

1. **Assigning a unified identifier (Uid)** to each unique patient
2. **Tracking source system identifiers** for each patient (which systems they appear in)
3. **Enabling patient matching** across systems using demographics (name, DOB, SSN)

This two-database structure (MPI + CDW) mirrors real-world healthcare data architectures where identity management is separated from clinical data storage.

| Table | Description |
|--------------------------|----------------------------------------------|
| `EnterpriseRecords` | Core patient demographics (Uid, name, DoB, SSN, address, phone) |
| `EnterpriseRecords_Ext` | Extended demographics and system identifiers (EPIC_PAT_ID, ALLSCRIPTS_PERSON_ID, MHH_MRN, UTP_MRN, race/ethnicity, deceased status) |
| `Mpi` | Cross-reference mapping between source systems and unified patient identifiers |
| `MPI_Src` | Source system definitions |

### CDW Database (PCORnet CDM)

| Table           | Description                                   |
|-----------------|-----------------------------------------------|
| `DEMOGRAPHIC`   | Patient demographics linked to MPI via UID    |
| `DEATH`         | Death records for deceased patients           |
| `ENCOUNTER`     | Patient encounters (IP, ED, AV, OA, IS types) |
| `DIAGNOSIS`     | ICD-10 diagnoses linked to encounters         |
| `PROCEDURES`    | CPT procedures linked to encounters           |
| `LAB_RESULT_CM` | Laboratory results with LOINC codes           |
| `PRESCRIBING`   | Medication prescriptions with RxNorm codes    |
| `VITAL`         | Vital signs (height, weight, BP, BMI)         |
| `PROVIDER`      | Provider directory                            |

### Key Identifiers

| Identifier | Format | Description |
|----|----|----|
| `Uid` | 1, 2, 3... | Unified patient identifier linking MPI and CDW |
| `PATID` | PAT0000001 | PCORnet patient ID |
| `ENCOUNTERID` | ENC0000000001 | Encounter identifier |
| `EPIC_PAT_ID` | EPIC0000000001 | Epic system patient ID (20% null) |
| `MHH_MRN` | MHH0000001 | Memorial Hermann MRN (60% null) |
| `UTP_MRN` | UTP0000001 | UT Physicians MRN (40% null) |

## Usage Examples

### Basic Queries

``` r
library(DBI)

# List all tables
dbListTables(dbs$cdw)
dbListTables(dbs$mpi)

# Count patients
dbGetQuery(dbs$cdw, "SELECT COUNT(*) FROM DEMOGRAPHIC")

# View encounters by type
dbGetQuery(dbs$cdw, "
  SELECT ENC_TYPE, COUNT(*) as count
  FROM ENCOUNTER
  GROUP BY ENC_TYPE
")
```

### Join Across Databases

``` r
library(DBI)

# Option 1: Using DuckDB ATTACH
dbExecute(dbs$cdw, "ATTACH 'mpi.duckdb' AS mpi")
dbGetQuery(dbs$cdw, "
  SELECT d.PATID, e.First, e.Last, d.BIRTH_DATE
  FROM DEMOGRAPHIC d
  JOIN mpi.EnterpriseRecords e ON d.UID = e.Uid
  LIMIT 10
")

# Option 2: Using dplyr
library(dplyr)
demographic <- dbReadTable(dbs$cdw, "DEMOGRAPHIC")
enterprise <- dbReadTable(dbs$mpi, "EnterpriseRecords")
inner_join(demographic, enterprise, by = c("UID" = "Uid")) %>%
  select(PATID, First, Last, BIRTH_DATE) %>%
  head(10)
```

### Utility Functions

The package includes several utility functions for exploring the databases:

``` r
# View first 10 rows of every table
print_all_tables(dbs$cdw, dbs$mpi)

# View summary (row counts, column counts)
print_table_summary(dbs$cdw, dbs$mpi)

# Get summary as a data structure
summary <- get_database_summary(dbs$cdw, dbs$mpi)
print(summary)
```

### Export to SQL Server

Transfer synthetic data to a Microsoft SQL Server database:

``` r
# Export with username/password authentication
export_to_sql_server(
  dbs,
  server = "localhost",
  database = "PCORnet_Dev",
  uid = "sa",
  pwd = "YourPassword123"
)

# Export with Windows integrated authentication
export_to_sql_server(
  dbs,
  server = "localhost",
  database = "PCORnet_Dev",
  trusted_connection = TRUE
)

# Export only specific tables
export_to_sql_server(
  dbs,
  server = "localhost",
  database = "PCORnet_Dev",
  uid = "sa",
  pwd = "YourPassword123",
  tables = c("DEMOGRAPHIC", "ENCOUNTER", "DIAGNOSIS"),
  overwrite = TRUE
)
```

**Prerequisites:** Install the `odbc` package and Microsoft ODBC Driver 17 for SQL Server:
- **R package:** `install.packages("odbc")`
- **Windows:** Download from [Microsoft](https://docs.microsoft.com/en-us/sql/connect/odbc/download-odbc-driver-for-sql-server)
- **Mac:** `brew install microsoft/mssql-release/msodbcsql17`

### Export to CSV

Export tables to CSV files for import into other systems:

``` r
# Export all tables to CSV
export_to_csv(dbs, output_dir = "pcornet_export")

# Export only CDW tables
export_to_csv(dbs, output_dir = "pcornet_export", include_mpi = FALSE)
```

## Data Characteristics

### Clinical Profiles (Enhanced Mode)

The enhanced mode assigns patients to clinical profiles:

| Profile       | Prevalence | Key Features                                |
|---------------|------------|---------------------------------------------|
| Healthy       | 40%        | Routine preventive care only                |
| Diabetic      | 15%        | E11.x diagnoses, HbA1c labs, Metformin      |
| Cardiac       | 12%        | I25.x diagnoses, lipid panels, statins      |
| Respiratory   | 8%         | J44.x/J45.x diagnoses, spirometry, inhalers |
| Mental Health | 10%        | F32.x/F41.x diagnoses, SSRIs                |
| Multi-morbid  | 15%        | Multiple chronic conditions                 |

This creates clinically coherent data suitable for demos and visualization.

### Random Mode

Randomly assigns diagnoses, procedures, labs, and medications without clinical logic. Suitable for:

-   Testing database schemas and queries
-   Developing ETL pipelines
-   Learning the PCORnet CDM structure

### Synthea Mode

Imports data from [Synthea](https://github.com/synthetichealth/synthea), a realistic synthetic patient generator. Characteristics:

-   **Disease progression models**: Patients develop conditions over time with realistic onset, treatment, and outcomes
-   **SNOMED-CT codes**: Conditions use SNOMED codes mapped to ICD-10 where possible
-   **Complete medical histories**: Longitudinal records from birth through death (if applicable)
-   **Realistic demographics**: Based on US Census data for age, gender, race, and geographic distribution

Synthea mode produces the most clinically realistic data but requires:
1. Java runtime to run Synthea
2. Pre-generated Synthea CSV output

See `synthea/README.md` for setup instructions.

### Simulated Data Quality Issues

The enhanced and random modes include:

-   **Missing values**: Configurable NULL rates for various fields
-   **Temporal inconsistencies**: ~5% of encounters fall before birth or after death
-   **Variable system presence**: Patients appear in 60-95% of source systems

## Development

### Running Tests

This project uses testthat for testing. To run the tests:

``` r
# Install devtools if needed
install.packages("devtools")

# Install the package locally
devtools::install()

# Run tests
devtools::test()

# Run R CMD check (comprehensive)
devtools::check()
```

### Test Coverage

Tests cover:
- `create_pcornet_database()` - Structure, patient count, validation, modes, reproducibility
- `load_pcornet_database()` - Error handling, file loading
- `get_database_summary()` - Structure, optional parameters
- Internal helper functions - Date generation, NA sampling

## License

MIT License
