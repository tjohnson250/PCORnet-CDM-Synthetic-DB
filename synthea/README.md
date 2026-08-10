# Synthea Integration

This directory contains tools to import [Synthea](https://github.com/synthetichealth/synthea) synthetic patient data and convert it to PCORnet CDM format.

Synthea generates highly realistic synthetic patient data using disease state models informed by CDC, NIH, and clinical research. This produces the most clinically accurate synthetic data available.

## Prerequisites

- **Java JDK 11 or newer** (JDK, not JRE)
- At least 4GB RAM for moderate population sizes
- The `pcornet.synthetic` R package (includes all required dependencies)

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
| `conditions.csv` | CONDITION |
| `claims.csv` | DIAGNOSIS |
| `medications.csv` | PRESCRIBING |
| `procedures.csv` | PROCEDURES |
| `observations.csv` | LAB_RESULT_CM, VITAL, OBS_CLIN |
| `immunizations.csv` | IMMUNIZATION |
| `payer_transitions.csv` + `payers.csv` | ENROLLMENT |

### How observations are routed

Synthea files every observation under a `CATEGORY`, and the three destination
tables split along it:

| Rows | Destination | Why |
|------|-------------|-----|
| `CATEGORY = laboratory` | `LAB_RESULT_CM` | actual laboratory results |
| everything else — `vital-signs`, `survey`, `social-history`, `exam`, `procedure`, `imaging`, `therapy` | `OBS_CLIN` | every non-laboratory observation, including body temperature, heart rate, respiratory rate, oxygen saturation and pain scores, none of which have a `VITAL` column |
| the five LOINC codes VITAL has columns for — height `8302-2`, weight `29463-7`, BMI `39156-5`, systolic `8480-6`, diastolic `8462-4` | `VITAL` **as well as** `OBS_CLIN` | selected by code, not category, so a height filed under any category still lands here |
| blank `CATEGORY` (QALY, DALY, QOLS) | *not loaded* | simulation scoring metrics, not patient observations, and not LOINC |

Height, weight, BMI and blood pressure are written to **both** `OBS_CLIN` and
`VITAL`. The duplication is deliberate: PCORnet is deprecating `VITAL`, and
`OBS_CLIN` is where these measures are headed, so both are populated while that
transition is underway. Body temperature is the exception — `VITAL` has no
column for it, so it exists only in `OBS_CLIN`.

Numeric-typed values go to `OBSCLIN_RESULT_NUM` with their units; text-typed
values go to `OBSCLIN_RESULT_TEXT`. Survey responses get
`OBSCLIN_SOURCE = 'PR'` (patient-reported), everything else `'HC'`.

### How enrollment is derived

`payer_transitions.csv` records one span per payer per year, so a patient on the
same plan for a decade produces ten abutting rows. Each unbroken run with the
same payer is collapsed into the single coverage period it represents.

Spans whose payer is `NO_INSURANCE` are excluded. `ENR_BASIS = 'I'` asserts the
patient was enrolled in insurance, and the CDM has no column to carry the payer,
so loading uninsured spans would make them read as insured. The consequence is
that encounters occurring while a patient was uninsured fall outside any
enrollment period — that is the source data being represented faithfully, not a
gap in the mapping.

### Terminology and value mapping

Smoking status (LOINC 72166-2) is written to `OBS_CLIN` as an observation and
also crosswalked into `VITAL.SMOKING` via `SYNTHEA_SMOKING_MAP`. `TOBACCO` and
`TOBACCO_TYPE` stay NULL — Synthea reports smoking only, which says nothing
about smokeless tobacco.

Synthea does not model medication administration events, so `MED_ADMIN` has no
source here. `DISPENSING` requires an NDC, which Synthea does not emit — it
codes medications in RxNorm only.

## Loading Selected Tables

Pass `tables` to refresh part of the CDM without rewriting the rest. Source CSVs
are read only when a requested table needs them, so this also skips the
multi-gigabyte files:

```r
load_synthea_data("~/synthea/output/csv", con_cdw, con_mpi,
                  tables = "IMMUNIZATION")
```

See `SYNTHEA_LOADABLE_TABLES` for the accepted names.

## Terminology Mappings

Synthea uses different code systems than PCORnet CDM:

| Synthea | PCORnet | Mapping |
|---------|---------|---------|
| CVX | `VX_CODE_TYPE = 'CX'` | direct, no crosswalk needed |
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
