# Synthea Integration

This directory contains tools to import [Synthea](https://github.com/synthetichealth/synthea) synthetic patient data and convert it to PCORnet CDM format.

Synthea generates highly realistic synthetic patient data using disease state models informed by CDC, NIH, and clinical research. This produces the most clinically accurate synthetic data available.

## Prerequisites

- **Java JDK 11 or 17** (LTS versions recommended)
- At least 4GB RAM for moderate population sizes
- R packages: DBI, duckdb, dplyr, lubridate

### Check Java Installation

```bash
java -version
```

If not installed, download from [Adoptium](https://adoptium.net/) or install via Homebrew:

```bash
brew install openjdk@17
```

## Synthea Setup

### 1. Download Synthea

Download the pre-built JAR file from the official releases:

```bash
# Create a directory for Synthea
mkdir -p ~/synthea && cd ~/synthea

# Download the latest release
curl -L -O https://github.com/synthetichealth/synthea/releases/download/master-branch-latest/synthea-with-dependencies.jar
```

Or download directly from your browser: [synthea-with-dependencies.jar](https://github.com/synthetichealth/synthea/releases/download/master-branch-latest/synthea-with-dependencies.jar)

### 2. Generate Patients

```bash
cd ~/synthea

# Generate 100 patients from Texas with CSV output
java -jar synthea-with-dependencies.jar -p 100 --exporter.csv.export=true Texas

# Generate 1000 patients from any US state
java -jar synthea-with-dependencies.jar -p 1000 --exporter.csv.export=true

# Generate specific conditions (e.g., diabetes)
java -jar synthea-with-dependencies.jar -p 100 --exporter.csv.export=true -m diabetes

# View all available options
java -jar synthea-with-dependencies.jar -h
```

Output will be in `./output/csv/`.

### Common Options

| Option | Description |
|--------|-------------|
| `-p 100` | Generate 100 patients |
| `-s 12345` | Set random seed for reproducibility |
| `-a 30-50` | Limit to patients aged 30-50 |
| `-g F` | Generate only female patients |
| `--exporter.csv.export=true` | Enable CSV output (required for PCORnet import) |
| `--exporter.fhir.export=false` | Disable FHIR output (speeds up generation) |
| `Texas` | Generate patients from Texas |
| `Texas Houston` | Generate patients from Houston, Texas |

## Convert to PCORnet CDM

After generating Synthea data:

```r
library(pcornet.synthetic)

# Convert Synthea CSV to PCORnet format
dbs <- load_synthea_data("~/synthea/output/csv")

# Access the databases
dbs$cdw  # Clinical Data Warehouse
dbs$mpi  # Master Patient Index

# List tables
DBI::dbListTables(dbs$cdw)
```

## Synthea Output Files

The converter uses these Synthea CSV files:

| File | PCORnet Tables |
|------|----------------|
| `patients.csv` | DEMOGRAPHIC, EnterpriseRecords |
| `encounters.csv` | ENCOUNTER |
| `conditions.csv` | DIAGNOSIS, CONDITION |
| `medications.csv` | PRESCRIBING |
| `procedures.csv` | PROCEDURES |
| `observations.csv` | LAB_RESULT_CM, VITAL |

## Terminology Mappings

Synthea uses different code systems than PCORnet CDM:

| Synthea | PCORnet | Mapping |
|---------|---------|---------|
| SNOMED-CT | ICD-10-CM | `mappings/snomed_icd10_common.csv` |
| Encounter class | ENC_TYPE | `mappings/encounter_type_map.csv` |

The converter includes mappings for common conditions. Unmapped SNOMED codes are preserved in RAW_ fields.

## Customization

### Generate Specific Conditions

Synthea can generate patients with specific conditions:

```bash
java -jar synthea-with-dependencies.jar -p 100 --exporter.csv.export=true -m diabetes
java -jar synthea-with-dependencies.jar -p 100 --exporter.csv.export=true -m lung_cancer
java -jar synthea-with-dependencies.jar -p 100 --exporter.csv.export=true -m covid19
```

See [Synthea Wiki](https://github.com/synthetichealth/synthea/wiki) for available modules.

### Adjust Demographics

Pass configuration options on the command line:

```bash
# Patients aged 18-85 with 10 years of medical history
java -jar synthea-with-dependencies.jar -p 100 \
  --exporter.csv.export=true \
  -a 18-85 \
  --exporter.years_of_history=10
```

Alternatively, create a `synthea.properties` file in your working directory:

```properties
# Age range
generate.demographics.minimum_age = 18
generate.demographics.maximum_age = 85

# Years of medical history
exporter.years_of_history = 10

# Enable CSV export
exporter.csv.export = true
```

Then run with the config file:

```bash
java -jar synthea-with-dependencies.jar -p 100 -c synthea.properties
```

## Troubleshooting

### Java Not Found

Ensure Java is in your PATH:

```bash
export JAVA_HOME=$(/usr/libexec/java_home)
export PATH=$JAVA_HOME/bin:$PATH
```

### Out of Memory

For large populations, increase Java heap:

```bash
java -Xms2g -Xmx4g -jar synthea-with-dependencies.jar -p 10000 --exporter.csv.export=true
```

### Missing CSV Files

Ensure you included `--exporter.csv.export=true` when running Synthea and re-run generation.

## Resources

- [Synthea GitHub](https://github.com/synthetichealth/synthea)
- [Synthea Wiki](https://github.com/synthetichealth/synthea/wiki)
- [Synthea Module Builder](https://synthetichealth.github.io/module-builder/)
- [Sample Synthea Data](https://synthea.mitre.org/downloads)
