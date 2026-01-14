# Clinical Profiles for Synthetic Data Generation

This directory contains clinical profile definitions that enable generation of clinically coherent synthetic patient data.

## Overview

Instead of randomly assigning diagnoses, labs, and medications, the enhanced generator assigns each patient a clinical profile that determines what clinical data they receive. This ensures that:

- Diabetic patients get diabetes diagnoses, HbA1c labs, and Metformin prescriptions
- Cardiac patients get heart disease diagnoses, lipid panels, and statins
- etc.

## Available Profiles

| Profile | Prevalence | Description |
|---------|------------|-------------|
| `healthy` | 40% | Routine preventive care, minimal diagnoses |
| `diabetic` | 15% | Type 2 diabetes with common comorbidities |
| `cardiac` | 12% | Coronary artery disease, hypertension |
| `respiratory` | 8% | COPD and/or asthma |
| `mental_health` | 10% | Depression and anxiety |
| `multimorbid` | 15% | Multiple chronic conditions (elderly pattern) |

Prevalence rates are adjusted by age - e.g., cardiac profiles are more common in older patients.

## Profile Structure

Each profile defines:

```r
profile_name = list(
  name = "Display Name",
  prevalence_base = 0.15,           # Base probability
  age_modifier = function(age) ...,  # Adjusts prevalence by age
  encounter_range = c(6, 15),        # Min/max encounters per patient

  diagnoses = list(
    primary = list(...),             # Primary diagnoses (always assigned)
    secondary = list(...)            # Secondary diagnoses (probability-based)
  ),

  labs = list(...),                  # Lab tests with normal/abnormal ranges
  medications = list(...),           # Medications with probabilities
  procedures = list(...),            # CPT procedures
  vitals = list(...)                 # BMI and BP parameters
)
```

## Files

- `clinical_profiles.R` - Profile definitions with ICD-10, LOINC, RxNorm, and CPT codes
- `profile_generator.R` - Functions to assign profiles and generate data

## Usage

The enhanced generator uses these profiles automatically:

```r
source("create_synthetic_database_enhanced.R")
```

To use profiles in custom code:

```r
source("clinical_profiles/clinical_profiles.R")
source("clinical_profiles/profile_generator.R")

# Assign profiles to patients
patient_data <- assign_clinical_profiles(patient_df, current_date)

# Generate profile-based data
diagnoses <- generate_profile_diagnoses(CLINICAL_PROFILES[["diabetic"]])
labs <- generate_profile_labs(CLINICAL_PROFILES[["diabetic"]], "AV")
medications <- generate_profile_medications(CLINICAL_PROFILES[["diabetic"]])
```

## Customization

To add or modify profiles, edit `clinical_profiles.R`. Each clinical element uses standard codes:

- **Diagnoses**: ICD-10-CM codes
- **Labs**: LOINC codes with reference ranges
- **Medications**: RxNorm CUI codes
- **Procedures**: CPT codes

## Clinical Coherence Examples

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
