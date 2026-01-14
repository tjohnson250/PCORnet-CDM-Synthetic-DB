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

### 1. Clone Synthea

```bash
git clone https://github.com/synthetichealth/synthea.git
cd synthea
```

### 2. Build Synthea

```bash
./gradlew build check test
```

On Windows, use `gradlew.bat` instead.

### 3. Configure for CSV Output

Edit `src/main/resources/synthea.properties`:

```properties
exporter.csv.export = true
exporter.fhir.export = false
```

Setting `exporter.fhir.export = false` speeds up generation if you only need CSV.

### 4. Generate Patients

```bash
# Generate 100 patients from Texas
./run_synthea -p 100 Texas

# Generate 1000 patients from any US state
./run_synthea -p 1000

# Generate specific conditions
./run_synthea -p 100 -m diabetes
```

Output will be in `synthea/output/csv/`.

## Convert to PCORnet CDM

After generating Synthea data:

```r
# In R, from the PCORnet-CDM-Synthetic-DB directory
source("synthea/synthea_to_pcornet.R")

# Convert Synthea CSV to PCORnet format
dbs <- load_synthea_data("/path/to/synthea/output/csv")

# Access the databases
con_cdw <- dbs$cdw
con_mpi <- dbs$mpi
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
./run_synthea -p 100 -m diabetes
./run_synthea -p 100 -m lung_cancer
./run_synthea -p 100 -m covid19
```

See [Synthea Wiki](https://github.com/synthetichealth/synthea/wiki) for available modules.

### Adjust Demographics

Edit `src/main/resources/synthea.properties`:

```properties
# Age range
generate.demographics.minimum_age = 18
generate.demographics.maximum_age = 85

# Years of medical history
exporter.years_of_history = 10
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
./run_synthea -p 10000 -- -Xms2g -Xmx4g
```

### Missing CSV Files

Ensure `exporter.csv.export = true` in synthea.properties and re-run generation.

## Resources

- [Synthea GitHub](https://github.com/synthetichealth/synthea)
- [Synthea Wiki](https://github.com/synthetichealth/synthea/wiki)
- [Synthea Module Builder](https://synthetichealth.github.io/module-builder/)
- [Sample Synthea Data](https://synthea.mitre.org/downloads)
