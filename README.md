# PCORnet CDM Synthetic Database Generator

A synthetic data generator for [PCORnet Common Data Model (CDM)](https://pcornet.org/data/) and Master Patient Index (MPI) databases. Generates data in R and writes directly to Microsoft SQL Server for development and testing purposes.

## Features

-   Generates synthetic patient data following PCORnet CDM v6.0 schema structure
-   Creates two linked SQL Server databases: Clinical Data Warehouse (CDW) and Master Patient Index (MPI)
-   Configurable patient population size
-   Reproducible data generation with seeded randomization
-   Includes simulated data quality issues (missing values, temporal inconsistencies)
-   Three generation modes: clinically coherent profiles, random, or Synthea import
-   Batched writes for large datasets

## Prerequisites

### R Packages

``` r
install.packages(c("DBI", "odbc", "dplyr", "lubridate", "tidyr"))
```

### SQL Server ODBC Driver

Install the Microsoft ODBC Driver for SQL Server:
- **Windows:** Download from [Microsoft](https://docs.microsoft.com/en-us/sql/connect/odbc/download-odbc-driver-for-sql-server)
- **Mac:** `brew install microsoft/mssql-release/msodbcsql17`
- **Linux:** See [Microsoft documentation](https://docs.microsoft.com/en-us/sql/connect/odbc/linux-mac/installing-the-microsoft-odbc-driver-for-sql-server) for your distribution

### SQL Server Databases

Create two databases on your SQL Server instance before running:

``` sql
CREATE DATABASE PCORnet_CDW;
CREATE DATABASE MPI;
```

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

# Access the databases
dbListTables(dbs$cdw)
dbListTables(dbs$mpi)
```

### Generation Modes

**Enhanced Mode (Default)** - Clinically coherent data with patient profiles:

``` r
dbs <- create_pcornet_database(
  n_patients = 500,
  server = "localhost",
  uid = "sa",
  pwd = "YourPassword123"
)
```

**Random Mode** - Randomly assigned clinical elements:

``` r
dbs <- create_pcornet_database(
  n_patients = 500,
  mode = "random",
  server = "localhost",
  uid = "sa",
  pwd = "YourPassword123"
)
```

**Synthea Mode** - Import from Synthea CSV output:

``` r
dbs <- create_pcornet_database(
  mode = "synthea",
  synthea_dir = "path/to/synthea/output/csv",
  server = "localhost",
  uid = "sa",
  pwd = "YourPassword123"
)
```

See `synthea/README.md` for Synthea setup instructions.

### Load Existing Databases

``` r
library(pcornet.synthetic)
dbs <- load_pcornet_database(
  server = "localhost",
  uid = "sa",
  pwd = "YourPassword123"
)
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
| `server` | "localhost" | SQL Server hostname |
| `cdw_database` | "PCORnet_CDW" | CDW database name |
| `mpi_database` | "MPI" | MPI database name |
| `uid` | NULL | SQL Server username |
| `pwd` | NULL | SQL Server password |
| `driver` | "ODBC Driver 18 for SQL Server" | ODBC driver name |
| `port` | 1433 | SQL Server port |
| `schema` | "dbo" | Target schema |
| `trusted_connection` | FALSE | Use Windows integrated auth |
| `connection_string` | NULL | Full ODBC connection string for CDW |
| `mpi_connection_string` | NULL | Full ODBC connection string for MPI |
| `overwrite` | TRUE | Overwrite existing tables |
| `batch_size` | 10000 | Rows per batch for large tables |
| `synthea_dir` | NULL | Path to Synthea CSV output (required for mode="synthea") |
| `profile_weights` | NULL | Named list to override default profile distribution |
| `sources` | NULL | Named list of MPI source systems (see Configuring Source Systems) |

**Returns:** List with `$cdw` (CDW connection), `$mpi` (MPI connection), and `$summary` (statistics)

### `load_pcornet_database()`

Connect to existing PCORnet databases on SQL Server.

| Parameter | Default | Description |
|-----------|---------|-------------|
| `server` | "localhost" | SQL Server hostname |
| `cdw_database` | "PCORnet_CDW" | CDW database name |
| `mpi_database` | "MPI" | MPI database name |
| `uid` | NULL | SQL Server username |
| `pwd` | NULL | SQL Server password |
| `driver` | "ODBC Driver 18 for SQL Server" | ODBC driver name |
| `port` | 1433 | SQL Server port |
| `trusted_connection` | FALSE | Use Windows integrated auth |
| `connection_string` | NULL | Full ODBC connection string for CDW |
| `mpi_connection_string` | NULL | Full ODBC connection string for MPI |

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
dbs <- create_pcornet_database(
  n_patients = 5000,
  server = "localhost",
  uid = "sa",
  pwd = "YourPassword123"
)

# Different random seed for unique data
dbs <- create_pcornet_database(
  seed = 123,
  server = "localhost",
  uid = "sa",
  pwd = "YourPassword123"
)

# Custom database names
dbs <- create_pcornet_database(
  cdw_database = "MyProject_CDW",
  mpi_database = "MyProject_MPI",
  server = "localhost",
  uid = "sa",
  pwd = "YourPassword123"
)

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
  ),
  server = "localhost",
  uid = "sa",
  pwd = "YourPassword123"
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
  ),
  server = "localhost",
  uid = "sa",
  pwd = "YourPassword123"
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
library(dplyr)

# Using dplyr to join across CDW and MPI connections
demographic <- dbReadTable(dbs$cdw, "DEMOGRAPHIC")
enterprise <- dbReadTable(dbs$mpi, "EnterpriseRecords")
inner_join(demographic, enterprise, by = c("UID" = "Uid")) %>%
  select(PATID, First, Last, BIRTH_DATE) %>%
  head(10)

# Or use cross-database queries in SQL Server
# (requires both databases on the same server)
dbGetQuery(dbs$cdw, "
  SELECT d.PATID, e.First, e.Last, d.BIRTH_DATE
  FROM DEMOGRAPHIC d
  JOIN MPI.dbo.EnterpriseRecords e ON d.UID = e.Uid
")
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

The enhanced mode assigns patients to clinical profiles that ensure clinically coherent data. Instead of randomly assigning diagnoses, labs, and medications, each patient receives a profile that determines their clinical data. This means:

- Diabetic patients get diabetes diagnoses, HbA1c labs, and Metformin prescriptions
- Cardiac patients get heart disease diagnoses, lipid panels, and statins
- Lab values are appropriately abnormal for the condition

| Profile       | Prevalence | Key Features                                |
|---------------|------------|---------------------------------------------|
| Healthy       | 40%        | Routine preventive care only                |
| Diabetic      | 15%        | E11.x diagnoses, HbA1c labs, Metformin      |
| Cardiac       | 12%        | I25.x diagnoses, lipid panels, statins      |
| Respiratory   | 8%         | J44.x/J45.x diagnoses, spirometry, inhalers |
| Mental Health | 10%        | F32.x/F41.x diagnoses, SSRIs                |
| Multi-morbid  | 15%        | Multiple chronic conditions                 |

Prevalence rates are adjusted by age (e.g., cardiac profiles are more common in older patients).

#### Profile Structure

Each profile defines:

- **Diagnoses**: Primary (always assigned) and secondary (probability-based) ICD-10 codes
- **Labs**: LOINC codes with normal/abnormal reference ranges appropriate to the condition
- **Medications**: RxNorm codes with prescription probabilities
- **Procedures**: CPT codes for condition-appropriate tests and treatments
- **Vitals**: BMI and blood pressure parameters typical for the condition
- **Encounter range**: Min/max encounters per patient

#### Clinical Coherence Examples

**Diabetic Patient:**
- Diagnoses: E11.9 (Type 2 DM), I10 (Hypertension), E78.5 (Hyperlipidemia)
- Labs: HbA1c (elevated), Glucose (elevated), Creatinine, GFR
- Medications: Metformin, Lisinopril, Atorvastatin
- Procedures: HbA1c test, Comprehensive metabolic panel

**Cardiac Patient:**
- Diagnoses: I25.10 (CAD), I10 (Hypertension), E78.5 (Hyperlipidemia)
- Labs: Lipid panel, BNP, Troponin
- Medications: Atorvastatin, Aspirin, Metoprolol, Lisinopril
- Procedures: ECG, Echocardiogram, Lipid panel

#### Customizing Profile Distribution

Override the default prevalence using the `profile_weights` parameter:

``` r
dbs <- create_pcornet_database(
  n_patients = 1000,
  profile_weights = list(
    healthy = 0.20,
    diabetic = 0.40,  # More diabetic patients
    cardiac = 0.15,
    respiratory = 0.10,
    mental_health = 0.10,
    multimorbid = 0.05
  ),
  server = "localhost",
  uid = "sa",
  pwd = "YourPassword123"
)
```

For full technical details on profile definitions, see `clinical_profiles/README.md`.

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
1. Java JDK 11 or newer to run Synthea
2. Pre-generated Synthea CSV output (see `synthea/README.md` for setup instructions)

### Simulated Data Quality Issues

The enhanced and random modes include:

-   **Missing values**: Configurable NULL rates for various fields
-   **Temporal inconsistencies**: ~5% of encounters fall before birth or after death
-   **Variable system presence**: Patients appear in 60-95% of source systems

## Development

### Running Tests

This project uses testthat for testing. Tests require a SQL Server connection. Set these environment variables:

``` bash
export PCORNET_TEST_SERVER="localhost"
export PCORNET_TEST_UID="sa"
export PCORNET_TEST_PWD="YourPassword123"
# Or for Windows auth:
export PCORNET_TEST_TRUSTED="TRUE"
# Optional: custom test database names
export PCORNET_TEST_CDW_DB="PCORnet_CDW_Test"
export PCORNET_TEST_MPI_DB="MPI_Test"
```

Then run:

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
- `load_pcornet_database()` - Error handling, connection loading
- `get_database_summary()` - Structure, optional parameters
- Internal helper functions - Date generation, NA sampling
- Synthea import - Patient/encounter/diagnosis/medication mapping

## License

MIT License
