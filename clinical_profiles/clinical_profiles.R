# Clinical Profile Definitions for PCORnet CDM Synthetic Data
# Each profile defines clinically coherent diagnoses, labs, medications, and procedures

CLINICAL_PROFILES <- list(

  # =============================================================================
  # HEALTHY - Routine preventive care only
  # =============================================================================
  healthy = list(
    name = "Healthy/Routine Care",
    prevalence_base = 0.40,
    age_modifier = function(age) ifelse(age < 45, 1.5, ifelse(age < 65, 1.0, 0.5)),
    encounter_range = c(2, 6),

    diagnoses = list(
      primary = list(
        list(code = "Z00.00", description = "Encounter for general adult medical examination", probability = 1.0)
      ),
      secondary = list(
        list(code = "Z23", description = "Encounter for immunization", probability = 0.4),
        list(code = "Z12.31", description = "Encounter for screening mammogram", probability = 0.2),
        list(code = "Z01.411", description = "Encounter for gynecological examination", probability = 0.15)
      )
    ),

    labs = list(
      list(loinc = "2951-2", name = "Sodium", unit = "mmol/L",
           normal_range = c(135, 145), abnormal_range = c(130, 134),
           abnormal_probability = 0.05, frequency_per_year = 1),
      list(loinc = "6690-2", name = "WBC", unit = "10*3/uL",
           normal_range = c(4.0, 11.0), abnormal_range = c(3.0, 3.9),
           abnormal_probability = 0.05, frequency_per_year = 1),
      list(loinc = "2093-3", name = "Total Cholesterol", unit = "mg/dL",
           normal_range = c(125, 200), abnormal_range = c(201, 240),
           abnormal_probability = 0.15, frequency_per_year = 0.5)
    ),

    medications = list(
      list(rxnorm = "161", name = "Acetaminophen", dose = 500, unit = "mg",
           probability = 0.15, frequency = "PRN"),
      list(rxnorm = "5640", name = "Ibuprofen", dose = 200, unit = "mg",
           probability = 0.10, frequency = "PRN")
    ),

    procedures = list(
      list(cpt = "99395", name = "Preventive visit, 18-39 years", probability = 0.8),
      list(cpt = "99396", name = "Preventive visit, 40-64 years", probability = 0.8),
      list(cpt = "36415", name = "Venipuncture", probability = 0.5),
      list(cpt = "85025", name = "CBC with differential", probability = 0.4)
    ),

    vitals = list(
      bmi_mean = 24, bmi_sd = 3,
      systolic_mean = 118, systolic_sd = 8,
      diastolic_mean = 75, diastolic_sd = 6
    )
  ),

  # =============================================================================
  # DIABETIC - Type 2 Diabetes with common comorbidities
  # =============================================================================
  diabetic = list(
    name = "Type 2 Diabetes",
    prevalence_base = 0.15,
    age_modifier = function(age) ifelse(age < 45, 0.3, ifelse(age < 65, 1.0, 1.5)),
    encounter_range = c(6, 15),

    diagnoses = list(
      primary = list(
        list(code = "E11.9", description = "Type 2 diabetes mellitus without complications", probability = 1.0)
      ),
      secondary = list(
        list(code = "E11.65", description = "Type 2 DM with hyperglycemia", probability = 0.30),
        list(code = "E11.40", description = "Type 2 DM with diabetic neuropathy", probability = 0.20),
        list(code = "E11.21", description = "Type 2 DM with diabetic nephropathy", probability = 0.15),
        list(code = "E11.319", description = "Type 2 DM with unspecified diabetic retinopathy", probability = 0.12),
        list(code = "I10", description = "Essential hypertension", probability = 0.60),
        list(code = "E78.5", description = "Hyperlipidemia, unspecified", probability = 0.50),
        list(code = "E66.9", description = "Obesity, unspecified", probability = 0.40),
        list(code = "N18.3", description = "Chronic kidney disease, stage 3", probability = 0.15)
      )
    ),

    labs = list(
      list(loinc = "4548-4", name = "Hemoglobin A1c", unit = "%",
           normal_range = c(4.0, 5.6), abnormal_range = c(6.5, 12.0),
           abnormal_probability = 0.70, frequency_per_year = 4),
      list(loinc = "2345-7", name = "Glucose", unit = "mg/dL",
           normal_range = c(70, 100), abnormal_range = c(126, 350),
           abnormal_probability = 0.50, frequency_per_year = 6),
      list(loinc = "2160-0", name = "Creatinine", unit = "mg/dL",
           normal_range = c(0.6, 1.2), abnormal_range = c(1.3, 3.0),
           abnormal_probability = 0.20, frequency_per_year = 2),
      list(loinc = "33914-3", name = "GFR", unit = "mL/min/1.73m2",
           normal_range = c(60, 120), abnormal_range = c(15, 59),
           abnormal_probability = 0.15, frequency_per_year = 2),
      list(loinc = "2093-3", name = "Total Cholesterol", unit = "mg/dL",
           normal_range = c(125, 200), abnormal_range = c(201, 280),
           abnormal_probability = 0.40, frequency_per_year = 1),
      list(loinc = "13457-7", name = "LDL Cholesterol", unit = "mg/dL",
           normal_range = c(50, 100), abnormal_range = c(101, 180),
           abnormal_probability = 0.45, frequency_per_year = 1)
    ),

    medications = list(
      list(rxnorm = "6809", name = "Metformin", dose = 500, unit = "mg",
           probability = 0.85, frequency = "BID"),
      list(rxnorm = "274783", name = "Metformin 1000mg", dose = 1000, unit = "mg",
           probability = 0.40, frequency = "BID"),
      list(rxnorm = "4815", name = "Glipizide", dose = 5, unit = "mg",
           probability = 0.25, frequency = "QD"),
      list(rxnorm = "1373458", name = "Insulin glargine", dose = 20, unit = "units",
           probability = 0.20, frequency = "QD"),
      list(rxnorm = "29046", name = "Lisinopril", dose = 10, unit = "mg",
           probability = 0.55, frequency = "QD"),
      list(rxnorm = "83367", name = "Atorvastatin", dose = 20, unit = "mg",
           probability = 0.60, frequency = "QD"),
      list(rxnorm = "1191", name = "Aspirin", dose = 81, unit = "mg",
           probability = 0.45, frequency = "QD")
    ),

    procedures = list(
      list(cpt = "83036", name = "Hemoglobin A1c", probability = 0.90),
      list(cpt = "80053", name = "Comprehensive metabolic panel", probability = 0.85),
      list(cpt = "82947", name = "Glucose, quantitative", probability = 0.70),
      list(cpt = "80061", name = "Lipid panel", probability = 0.60),
      list(cpt = "99214", name = "Office visit, established patient", probability = 0.95),
      list(cpt = "92250", name = "Fundus photography", probability = 0.25),
      list(cpt = "95860", name = "EMG, 1 extremity", probability = 0.10)
    ),

    vitals = list(
      bmi_mean = 32, bmi_sd = 5,
      systolic_mean = 138, systolic_sd = 12,
      diastolic_mean = 85, diastolic_sd = 8
    )
  ),

  # =============================================================================
  # CARDIAC - Coronary artery disease and related conditions
  # =============================================================================
  cardiac = list(
    name = "Cardiac Disease",
    prevalence_base = 0.12,
    age_modifier = function(age) ifelse(age < 50, 0.2, ifelse(age < 65, 1.0, 2.0)),
    encounter_range = c(8, 20),

    diagnoses = list(
      primary = list(
        list(code = "I25.10", description = "Atherosclerotic heart disease of native coronary artery", probability = 0.70),
        list(code = "I10", description = "Essential hypertension", probability = 0.30)
      ),
      secondary = list(
        list(code = "I10", description = "Essential hypertension", probability = 0.80),
        list(code = "I50.9", description = "Heart failure, unspecified", probability = 0.30),
        list(code = "I48.91", description = "Unspecified atrial fibrillation", probability = 0.25),
        list(code = "I25.5", description = "Ischemic cardiomyopathy", probability = 0.20),
        list(code = "E78.5", description = "Hyperlipidemia, unspecified", probability = 0.70),
        list(code = "E11.9", description = "Type 2 diabetes mellitus", probability = 0.35),
        list(code = "I73.9", description = "Peripheral vascular disease", probability = 0.15),
        list(code = "Z95.1", description = "Presence of coronary stent", probability = 0.20)
      )
    ),

    labs = list(
      list(loinc = "2093-3", name = "Total Cholesterol", unit = "mg/dL",
           normal_range = c(125, 200), abnormal_range = c(201, 300),
           abnormal_probability = 0.50, frequency_per_year = 2),
      list(loinc = "2571-8", name = "Triglycerides", unit = "mg/dL",
           normal_range = c(50, 150), abnormal_range = c(151, 400),
           abnormal_probability = 0.40, frequency_per_year = 2),
      list(loinc = "13457-7", name = "LDL Cholesterol", unit = "mg/dL",
           normal_range = c(50, 100), abnormal_range = c(101, 190),
           abnormal_probability = 0.50, frequency_per_year = 2),
      list(loinc = "2085-9", name = "HDL Cholesterol", unit = "mg/dL",
           normal_range = c(40, 60), abnormal_range = c(20, 39),
           abnormal_probability = 0.30, frequency_per_year = 2),
      list(loinc = "30522-7", name = "BNP", unit = "pg/mL",
           normal_range = c(0, 100), abnormal_range = c(101, 900),
           abnormal_probability = 0.25, frequency_per_year = 1),
      list(loinc = "10839-9", name = "Troponin I", unit = "ng/mL",
           normal_range = c(0, 0.04), abnormal_range = c(0.05, 2.0),
           abnormal_probability = 0.08, frequency_per_year = 0.5)
    ),

    medications = list(
      list(rxnorm = "83367", name = "Atorvastatin", dose = 40, unit = "mg",
           probability = 0.85, frequency = "QD"),
      list(rxnorm = "29046", name = "Lisinopril", dose = 20, unit = "mg",
           probability = 0.70, frequency = "QD"),
      list(rxnorm = "1191", name = "Aspirin", dose = 81, unit = "mg",
           probability = 0.85, frequency = "QD"),
      list(rxnorm = "6918", name = "Metoprolol succinate", dose = 50, unit = "mg",
           probability = 0.65, frequency = "QD"),
      list(rxnorm = "32968", name = "Clopidogrel", dose = 75, unit = "mg",
           probability = 0.35, frequency = "QD"),
      list(rxnorm = "17767", name = "Amlodipine", dose = 5, unit = "mg",
           probability = 0.40, frequency = "QD"),
      list(rxnorm = "8787", name = "Furosemide", dose = 20, unit = "mg",
           probability = 0.25, frequency = "QD")
    ),

    procedures = list(
      list(cpt = "93000", name = "Electrocardiogram, complete", probability = 0.90),
      list(cpt = "93306", name = "Echocardiography, complete", probability = 0.55),
      list(cpt = "80061", name = "Lipid panel", probability = 0.85),
      list(cpt = "93015", name = "Cardiovascular stress test", probability = 0.25),
      list(cpt = "93458", name = "Cardiac catheterization", probability = 0.10),
      list(cpt = "99214", name = "Office visit, established patient", probability = 0.95),
      list(cpt = "80053", name = "Comprehensive metabolic panel", probability = 0.70)
    ),

    vitals = list(
      bmi_mean = 29, bmi_sd = 4,
      systolic_mean = 145, systolic_sd = 15,
      diastolic_mean = 88, diastolic_sd = 10
    )
  ),

  # =============================================================================
  # RESPIRATORY - COPD and Asthma
  # =============================================================================
  respiratory = list(
    name = "Chronic Respiratory Disease",
    prevalence_base = 0.08,
    age_modifier = function(age) ifelse(age < 40, 0.6, ifelse(age < 65, 1.0, 1.4)),
    encounter_range = c(5, 12),

    diagnoses = list(
      primary = list(
        list(code = "J44.9", description = "Chronic obstructive pulmonary disease, unspecified", probability = 0.55),
        list(code = "J45.909", description = "Unspecified asthma, uncomplicated", probability = 0.45)
      ),
      secondary = list(
        list(code = "J44.1", description = "COPD with acute exacerbation", probability = 0.25),
        list(code = "J45.901", description = "Unspecified asthma with acute exacerbation", probability = 0.20),
        list(code = "R05.9", description = "Cough, unspecified", probability = 0.50),
        list(code = "R06.02", description = "Shortness of breath", probability = 0.55),
        list(code = "J18.9", description = "Pneumonia, unspecified organism", probability = 0.15),
        list(code = "F17.210", description = "Nicotine dependence, cigarettes, uncomplicated", probability = 0.35)
      )
    ),

    labs = list(
      list(loinc = "2019-8", name = "Carbon dioxide", unit = "mmol/L",
           normal_range = c(22, 29), abnormal_range = c(30, 40),
           abnormal_probability = 0.25, frequency_per_year = 2),
      list(loinc = "6690-2", name = "WBC", unit = "10*3/uL",
           normal_range = c(4.0, 11.0), abnormal_range = c(11.1, 18.0),
           abnormal_probability = 0.20, frequency_per_year = 2),
      list(loinc = "2708-6", name = "Oxygen saturation", unit = "%",
           normal_range = c(95, 100), abnormal_range = c(85, 94),
           abnormal_probability = 0.30, frequency_per_year = 3)
    ),

    medications = list(
      list(rxnorm = "435", name = "Albuterol", dose = 90, unit = "mcg",
           probability = 0.90, frequency = "PRN"),
      list(rxnorm = "896188", name = "Fluticasone-salmeterol", dose = 250, unit = "mcg",
           probability = 0.55, frequency = "BID"),
      list(rxnorm = "1546062", name = "Tiotropium", dose = 18, unit = "mcg",
           probability = 0.35, frequency = "QD"),
      list(rxnorm = "7994", name = "Prednisone", dose = 20, unit = "mg",
           probability = 0.25, frequency = "QD"),
      list(rxnorm = "1649574", name = "Montelukast", dose = 10, unit = "mg",
           probability = 0.30, frequency = "QD"),
      list(rxnorm = "197319", name = "Azithromycin", dose = 250, unit = "mg",
           probability = 0.15, frequency = "QD")
    ),

    procedures = list(
      list(cpt = "94010", name = "Spirometry", probability = 0.70),
      list(cpt = "94060", name = "Bronchodilation responsiveness", probability = 0.45),
      list(cpt = "71046", name = "Chest X-ray, 2 views", probability = 0.55),
      list(cpt = "94760", name = "Pulse oximetry", probability = 0.80),
      list(cpt = "99214", name = "Office visit, established patient", probability = 0.95)
    ),

    vitals = list(
      bmi_mean = 26, bmi_sd = 5,
      systolic_mean = 125, systolic_sd = 12,
      diastolic_mean = 78, diastolic_sd = 8
    )
  ),

  # =============================================================================
  # MENTAL HEALTH - Depression and Anxiety
  # =============================================================================
  mental_health = list(
    name = "Mental Health Conditions",
    prevalence_base = 0.10,
    age_modifier = function(age) ifelse(age < 18, 0.5, ifelse(age < 65, 1.0, 0.7)),
    encounter_range = c(4, 12),

    diagnoses = list(
      primary = list(
        list(code = "F32.9", description = "Major depressive disorder, single episode, unspecified", probability = 0.50),
        list(code = "F41.9", description = "Anxiety disorder, unspecified", probability = 0.50)
      ),
      secondary = list(
        list(code = "F41.1", description = "Generalized anxiety disorder", probability = 0.35),
        list(code = "F32.1", description = "Major depressive disorder, single episode, moderate", probability = 0.30),
        list(code = "F33.0", description = "Major depressive disorder, recurrent, mild", probability = 0.20),
        list(code = "G47.00", description = "Insomnia, unspecified", probability = 0.40),
        list(code = "F43.10", description = "Post-traumatic stress disorder, unspecified", probability = 0.15)
      )
    ),

    labs = list(
      list(loinc = "3016-3", name = "TSH", unit = "mIU/L",
           normal_range = c(0.4, 4.0), abnormal_range = c(4.1, 10.0),
           abnormal_probability = 0.10, frequency_per_year = 1),
      list(loinc = "2951-2", name = "Sodium", unit = "mmol/L",
           normal_range = c(135, 145), abnormal_range = c(130, 134),
           abnormal_probability = 0.05, frequency_per_year = 1)
    ),

    medications = list(
      list(rxnorm = "36437", name = "Sertraline", dose = 50, unit = "mg",
           probability = 0.55, frequency = "QD"),
      list(rxnorm = "321988", name = "Escitalopram", dose = 10, unit = "mg",
           probability = 0.35, frequency = "QD"),
      list(rxnorm = "72625", name = "Bupropion", dose = 150, unit = "mg",
           probability = 0.25, frequency = "QD"),
      list(rxnorm = "42347", name = "Trazodone", dose = 50, unit = "mg",
           probability = 0.30, frequency = "QHS"),
      list(rxnorm = "596", name = "Alprazolam", dose = 0.5, unit = "mg",
           probability = 0.15, frequency = "PRN"),
      list(rxnorm = "6470", name = "Lorazepam", dose = 0.5, unit = "mg",
           probability = 0.12, frequency = "PRN")
    ),

    procedures = list(
      list(cpt = "90834", name = "Psychotherapy, 45 minutes", probability = 0.45),
      list(cpt = "90837", name = "Psychotherapy, 60 minutes", probability = 0.30),
      list(cpt = "96127", name = "Brief emotional assessment", probability = 0.60),
      list(cpt = "99214", name = "Office visit, established patient", probability = 0.90),
      list(cpt = "99213", name = "Office visit, established patient, low", probability = 0.50)
    ),

    vitals = list(
      bmi_mean = 27, bmi_sd = 6,
      systolic_mean = 122, systolic_sd = 12,
      diastolic_mean = 78, diastolic_sd = 8
    )
  ),

  # =============================================================================
  # MULTIMORBID - Multiple chronic conditions (elderly pattern)
  # =============================================================================
  multimorbid = list(
    name = "Multiple Chronic Conditions",
    prevalence_base = 0.15,
    age_modifier = function(age) ifelse(age < 55, 0.1, ifelse(age < 70, 0.8, 2.0)),
    encounter_range = c(10, 25),

    diagnoses = list(
      primary = list(
        list(code = "E11.9", description = "Type 2 diabetes mellitus without complications", probability = 0.50),
        list(code = "I25.10", description = "Atherosclerotic heart disease", probability = 0.50)
      ),
      secondary = list(
        list(code = "I10", description = "Essential hypertension", probability = 0.85),
        list(code = "E78.5", description = "Hyperlipidemia, unspecified", probability = 0.75),
        list(code = "I50.9", description = "Heart failure, unspecified", probability = 0.35),
        list(code = "N18.3", description = "Chronic kidney disease, stage 3", probability = 0.30),
        list(code = "J44.9", description = "COPD, unspecified", probability = 0.25),
        list(code = "M17.11", description = "Primary osteoarthritis, right knee", probability = 0.40),
        list(code = "G47.33", description = "Obstructive sleep apnea", probability = 0.30),
        list(code = "E66.9", description = "Obesity, unspecified", probability = 0.45),
        list(code = "F32.9", description = "Major depressive disorder", probability = 0.25)
      )
    ),

    labs = list(
      list(loinc = "4548-4", name = "Hemoglobin A1c", unit = "%",
           normal_range = c(4.0, 5.6), abnormal_range = c(6.5, 10.0),
           abnormal_probability = 0.60, frequency_per_year = 3),
      list(loinc = "2345-7", name = "Glucose", unit = "mg/dL",
           normal_range = c(70, 100), abnormal_range = c(126, 280),
           abnormal_probability = 0.45, frequency_per_year = 4),
      list(loinc = "2160-0", name = "Creatinine", unit = "mg/dL",
           normal_range = c(0.6, 1.2), abnormal_range = c(1.3, 2.5),
           abnormal_probability = 0.30, frequency_per_year = 3),
      list(loinc = "33914-3", name = "GFR", unit = "mL/min/1.73m2",
           normal_range = c(60, 120), abnormal_range = c(30, 59),
           abnormal_probability = 0.30, frequency_per_year = 3),
      list(loinc = "2093-3", name = "Total Cholesterol", unit = "mg/dL",
           normal_range = c(125, 200), abnormal_range = c(201, 260),
           abnormal_probability = 0.40, frequency_per_year = 2),
      list(loinc = "30522-7", name = "BNP", unit = "pg/mL",
           normal_range = c(0, 100), abnormal_range = c(101, 600),
           abnormal_probability = 0.30, frequency_per_year = 2)
    ),

    medications = list(
      list(rxnorm = "6809", name = "Metformin", dose = 500, unit = "mg",
           probability = 0.50, frequency = "BID"),
      list(rxnorm = "83367", name = "Atorvastatin", dose = 40, unit = "mg",
           probability = 0.75, frequency = "QD"),
      list(rxnorm = "29046", name = "Lisinopril", dose = 20, unit = "mg",
           probability = 0.70, frequency = "QD"),
      list(rxnorm = "6918", name = "Metoprolol", dose = 50, unit = "mg",
           probability = 0.55, frequency = "BID"),
      list(rxnorm = "1191", name = "Aspirin", dose = 81, unit = "mg",
           probability = 0.70, frequency = "QD"),
      list(rxnorm = "17767", name = "Amlodipine", dose = 5, unit = "mg",
           probability = 0.45, frequency = "QD"),
      list(rxnorm = "8787", name = "Furosemide", dose = 40, unit = "mg",
           probability = 0.30, frequency = "QD"),
      list(rxnorm = "7646", name = "Omeprazole", dose = 20, unit = "mg",
           probability = 0.50, frequency = "QD"),
      list(rxnorm = "25480", name = "Gabapentin", dose = 300, unit = "mg",
           probability = 0.25, frequency = "TID")
    ),

    procedures = list(
      list(cpt = "99214", name = "Office visit, established patient", probability = 0.95),
      list(cpt = "99215", name = "Office visit, established, high complexity", probability = 0.40),
      list(cpt = "80053", name = "Comprehensive metabolic panel", probability = 0.85),
      list(cpt = "83036", name = "Hemoglobin A1c", probability = 0.50),
      list(cpt = "80061", name = "Lipid panel", probability = 0.60),
      list(cpt = "93000", name = "ECG, complete", probability = 0.50),
      list(cpt = "93306", name = "Echocardiography", probability = 0.25)
    ),

    vitals = list(
      bmi_mean = 31, bmi_sd = 5,
      systolic_mean = 142, systolic_sd = 15,
      diastolic_mean = 82, diastolic_sd = 10
    )
  )
)

# Frequency code mappings
FREQUENCY_CODES <- list(
  "QD" = list(code = "01", raw = "Once daily"),
  "BID" = list(code = "02", raw = "Twice daily"),
  "TID" = list(code = "03", raw = "Three times daily"),
  "QID" = list(code = "04", raw = "Four times daily"),
  "PRN" = list(code = "05", raw = "As needed"),
  "QHS" = list(code = "06", raw = "At bedtime")
)
