# PCORnet CDM Synthetic Database Generator

A synthetic data generator for [PCORnet Common Data Model (CDM)](https://pcornet.org/data/) and Master Patient Index (MPI) databases. Creates DuckDB databases populated with synthetic test data for development and testing purposes.

## Features

- Generates synthetic patient data following PCORnet CDM v6.0 schema structure
- Creates two linked databases: Clinical Data Warehouse (CDW) and Master Patient Index (MPI)
- Configurable patient population size
- Reproducible data generation with seeded randomization
- Includes simulated data quality issues (missing values, temporal inconsistencies)

## Requirements

R packages:
- DBI
- duckdb
- dplyr
- lubridate

Install dependencies:
```r
install.packages(c("DBI", "duckdb", "dplyr", "lubridate"))
```

## Quick Start

### Generate New Databases

```r
source("create_synthetic_database.R")
```

This creates two database connections and saves them to disk:
- `con_cdw` - Clinical Data Warehouse (`pcornet_cdw.duckdb`)
- `con_mpi` - Master Patient Index (`mpi.duckdb`)

### Load Existing Databases

```r
source("load_databases.R")
```

Faster than regenerating - useful for working with the same dataset across sessions.

## Database Schema

### MPI Database (Master Patient Index)

| Table | Description |
|-------|-------------|
| `EnterpriseRecords` | Core patient demographics (Uid, name, DoB, SSN, address, phone) |
| `EnterpriseRecords_Ext` | Extended demographics and system identifiers (EPIC_PAT_ID, ALLSCRIPTS_PERSON_ID, MHH_MRN, UTP_MRN, race/ethnicity, deceased status) |
| `Mpi` | Cross-reference mapping between source systems and unified patient identifiers |
| `MPI_Src` | Source system definitions |

### CDW Database (PCORnet CDM)

| Table | Description |
|-------|-------------|
| `DEMOGRAPHIC` | Patient demographics linked to MPI via UID |
| `DEATH` | Death records for deceased patients |
| `DEATH_CAUSE` | Cause of death information |
| `ENCOUNTER` | Patient encounters (IP, ED, AV, OA, IS types) |
| `DIAGNOSIS` | ICD-10 diagnoses linked to encounters |
| `PROCEDURES` | CPT procedures linked to encounters |
| `LAB_RESULT_CM` | Laboratory results with LOINC codes |
| `PRESCRIBING` | Medication prescriptions with RxNorm codes |
| `DISPENSING` | Medication dispensing records |
| `VITAL` | Vital signs (height, weight, BP, BMI) |
| `CONDITION` | Patient conditions |
| `PROVIDER` | Provider directory |

### Key Identifiers

| Identifier | Format | Description |
|------------|--------|-------------|
| `Uid` | 1, 2, 3... | Unified patient identifier linking MPI and CDW |
| `PATID` | PAT0000001 | PCORnet patient ID |
| `ENCOUNTERID` | ENC0000000001 | Encounter identifier |
| `EPIC_PAT_ID` | EPIC0000000001 | Epic system patient ID (20% null) |
| `MHH_MRN` | MHH0000001 | Memorial Hermann MRN (60% null) |
| `UTP_MRN` | UTP0000001 | UT Physicians MRN (40% null) |

## Configuration

Edit parameters at the top of `create_synthetic_database.R`:

```r
N_PATIENTS <- 3000           # Number of synthetic patients
CURRENT_DATE <- as.Date("2024-11-27")  # Reference date for data generation
set.seed(42)                 # Change for different random data
```

## Usage Examples

### Basic Queries

```r
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

```r
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

```r
source("utility_functions.R")

# View first 10 rows of every table
print_all_tables(con_cdw, con_mpi)

# View summary (row counts, column counts)
print_table_summary(con_cdw, con_mpi)
```

## Data Characteristics

The generated data is **not clinically realistic** - diagnoses, procedures, labs, and medications are randomly assigned without clinical logic. This data is suitable for:

- Testing database schemas and queries
- Developing ETL pipelines
- UI/application development
- Learning the PCORnet CDM structure

The generator includes simulated data quality issues:

- **Missing values**: Configurable NULL rates for various fields
- **Temporal inconsistencies**: ~5% of encounters fall before birth or after death
- **Variable system presence**: Patients appear in 60-95% of source systems

## License

MIT License
