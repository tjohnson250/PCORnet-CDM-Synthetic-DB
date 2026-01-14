# PCORnet CDM Synthetic Database Generator

A synthetic data generator for [PCORnet Common Data Model (CDM)](https://pcornet.org/data/) and Master Patient Index (MPI) databases. Creates DuckDB databases populated with synthetic test data for development and testing purposes.

## Features

-   Generates synthetic patient data following PCORnet CDM v6.0 schema structure
-   Creates two linked databases: Clinical Data Warehouse (CDW) and Master Patient Index (MPI)
-   Configurable patient population size
-   Reproducible data generation with seeded randomization
-   Includes simulated data quality issues (missing values, temporal inconsistencies)

## Requirements

R packages: - DBI - duckdb - dplyr - lubridate

Install dependencies:

``` r
install.packages(c("DBI", "duckdb", "dplyr", "lubridate"))
```

## Quick Start

### Option 1: Clinically Coherent Data (Recommended)

``` r
source("create_synthetic_database_enhanced.R")
```

This generates data using **clinical profiles** where diagnoses, labs, and medications are logically related: - Diabetic patients get diabetes diagnoses, HbA1c labs, and Metformin - Cardiac patients get heart disease diagnoses, lipid panels, and statins - etc.

### Option 2: Random Data (Original)

``` r
source("create_synthetic_database.R")
```

Generates data with randomly assigned clinical elements (no clinical logic).

### Option 3: Synthea Integration (Most Realistic)

For highly realistic clinical data, you can import from [Synthea](https://github.com/synthetichealth/synthea):

``` r
source("synthea/synthea_to_pcornet.R")
dbs <- load_synthea_data("path/to/synthea/output/csv")
```

See `synthea/README.md` for setup instructions.

### Load Existing Databases

``` r
source("load_databases.R")
```

Faster than regenerating - useful for working with the same dataset across sessions.

All generation options create:

- `con_cdw` - Clinical Data Warehouse (`pcornet_cdw.duckdb`)

- `con_mpi` - Master Patient Index (`mpi.duckdb`)

## Database Schema

### MPI Database (Master Patient Index)

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
| `DEATH_CAUSE`   | Cause of death information                    |
| `ENCOUNTER`     | Patient encounters (IP, ED, AV, OA, IS types) |
| `DIAGNOSIS`     | ICD-10 diagnoses linked to encounters         |
| `PROCEDURES`    | CPT procedures linked to encounters           |
| `LAB_RESULT_CM` | Laboratory results with LOINC codes           |
| `PRESCRIBING`   | Medication prescriptions with RxNorm codes    |
| `DISPENSING`    | Medication dispensing records                 |
| `VITAL`         | Vital signs (height, weight, BP, BMI)         |
| `CONDITION`     | Patient conditions                            |
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

## Configuration

Edit parameters at the top of `create_synthetic_database.R`:

``` r
N_PATIENTS <- 3000           # Number of synthetic patients
CURRENT_DATE <- as.Date("2024-11-27")  # Reference date for data generation
set.seed(42)                 # Change for different random data
```

## Usage Examples

### Basic Queries

``` r
# List all tables
dbListTables(con_cdw)
dbListTables(con_mpi)

# Count patients
dbGetQuery(con_cdw, "SELECT COUNT(*) FROM DEMOGRAPHIC")

# View encounters by type
dbGetQuery(con_cdw, "
  SELECT ENC_TYPE, COUNT(*) as count
  FROM ENCOUNTER
  GROUP BY ENC_TYPE
")
```

### Join Across Databases

``` r
# Option 1: Using DuckDB ATTACH
dbExecute(con_cdw, "ATTACH 'mpi.duckdb' AS mpi")
dbGetQuery(con_cdw, "
  SELECT d.PATID, e.First, e.Last, d.BIRTH_DATE
  FROM DEMOGRAPHIC d
  JOIN mpi.EnterpriseRecords e ON d.UID = e.Uid
  LIMIT 10
")

# Option 2: Using dplyr
library(dplyr)
demographic <- dbReadTable(con_cdw, "DEMOGRAPHIC")
enterprise <- dbReadTable(con_mpi, "EnterpriseRecords")
inner_join(demographic, enterprise, by = c("UID" = "Uid")) %>%
  select(PATID, First, Last, BIRTH_DATE) %>%
  head(10)
```

### Utility Functions

``` r
source("utility_functions.R")

# View first 10 rows of every table
print_all_tables(con_cdw, con_mpi)

# View summary (row counts, column counts)
print_table_summary(con_cdw, con_mpi)
```

## Data Characteristics

### Clinical Profiles (Enhanced Generator)

The enhanced generator (`create_synthetic_database_enhanced.R`) assigns patients to clinical profiles:

| Profile       | Prevalence | Key Features                                |
|---------------|------------|---------------------------------------------|
| Healthy       | 40%        | Routine preventive care only                |
| Diabetic      | 15%        | E11.x diagnoses, HbA1c labs, Metformin      |
| Cardiac       | 12%        | I25.x diagnoses, lipid panels, statins      |
| Respiratory   | 8%         | J44.x/J45.x diagnoses, spirometry, inhalers |
| Mental Health | 10%        | F32.x/F41.x diagnoses, SSRIs                |
| Multi-morbid  | 15%        | Multiple chronic conditions                 |

This creates clinically coherent data suitable for demos and visualization.

### Random Data (Original Generator)

The original generator (`create_synthetic_database.R`) randomly assigns diagnoses, procedures, labs, and medications without clinical logic. Suitable for:

-   Testing database schemas and queries
-   Developing ETL pipelines
-   Learning the PCORnet CDM structure

### Simulated Data Quality Issues

Both generators include:

-   **Missing values**: Configurable NULL rates for various fields
-   **Temporal inconsistencies**: \~5% of encounters fall before birth or after death
-   **Variable system presence**: Patients appear in 60-95% of source systems

## License

MIT License
