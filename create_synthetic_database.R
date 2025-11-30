# Synthetic Database Generator for PCORnet CDM and MPI
# Creates in-memory DuckDB databases with realistic test data

library(DBI)
library(duckdb)
library(dplyr)
library(lubridate)

set.seed(42)  # For reproducibility

# Configuration
N_PATIENTS <- 100
CURRENT_DATE <- as.Date("2024-11-27")
CURRENT_DATETIME <- as.POSIXct(paste(CURRENT_DATE, "12:00:00"), tz = "UTC")

# Helper Functions
random_date <- function(n, start_date, end_date) {
  start <- as.numeric(start_date)
  end <- as.numeric(end_date)
  dates <- as.Date(runif(n, start, end), origin = "1970-01-01")
  return(dates)
}

sample_with_na <- function(x, n, prob_na = 0.1, ...) {
  result <- sample(x, n, ...)
  if (prob_na > 0) {
    na_indices <- sample(1:n, size = floor(n * prob_na))
    result[na_indices] <- NA
  }
  return(result)
}

random_datetime <- function(n, start_date, end_date) {
  dates <- random_date(n, start_date, end_date)
  # Add random time component (hours, minutes, seconds)
  times <- runif(n, 0, 86400)  # 0 to 86400 seconds in a day
  datetimes <- as.POSIXct(as.numeric(dates) * 86400 + times, origin = "1970-01-01", tz = "UTC")
  return(datetimes)
}

# Create databases
con_cdw <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")
con_mpi <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")

cat("Generating synthetic patient data...\n")

# ============================================================================
# MPI DATABASE - Master Patient Index
# ============================================================================

cat("Creating MPI database...\n")

# Generate UIDs (unified patient identifiers)
uids <- 1:N_PATIENTS

# EnterpriseRecords - Master patient records
enterprise_records <- data.frame(
  Uid = uids,
  First = replicate(N_PATIENTS, paste0(sample(LETTERS, 1), 
                                        paste(sample(letters, sample(3:8, 1)), collapse = ""))),
  M = sample_with_na(LETTERS, N_PATIENTS, prob_na = 0.3, replace = TRUE),
  Last = replicate(N_PATIENTS, paste0(sample(LETTERS, 1), 
                                       paste(sample(letters, sample(4:10, 1)), collapse = ""))),
  DoB = random_date(N_PATIENTS, as.Date("1930-01-01"), as.Date("2020-01-01")),
  SSN = sprintf("%09d", sample(100000000:999999999, N_PATIENTS, replace = FALSE)),
  G = sample(c("M", "F"), N_PATIENTS, replace = TRUE),
  Addy = replicate(N_PATIENTS, paste(sample(100:9999, 1), 
                                      sample(c("Main St", "Oak Ave", "Elm Dr", "Maple Ln"), 1))),
  Phone = sprintf("(%03d)%03d-%04d", sample(200:999, N_PATIENTS, replace = TRUE),
                  sample(200:999, N_PATIENTS, replace = TRUE),
                  sample(1000:9999, N_PATIENTS, replace = TRUE)),
  City = sample(c("Houston", "Dallas", "Austin", "San Antonio", "Fort Worth"), 
                N_PATIENTS, replace = TRUE),
  Zip = sprintf("%05d", sample(70000:79999, N_PATIENTS, replace = TRUE)),
  LastEditDTTM = random_datetime(N_PATIENTS, as.Date("2020-01-01"), CURRENT_DATE),
  isActive = 1,
  stringsAsFactors = FALSE
)

# Add soundex columns
enterprise_records$FirstSdx <- substr(enterprise_records$First, 1, 4)
enterprise_records$LastSdx <- substr(enterprise_records$Last, 1, 4)

# DuckDB handles native date types - no conversion needed
dbWriteTable(con_mpi, "EnterpriseRecords", enterprise_records, overwrite = TRUE)

# EnterpriseRecords_Ext - Extended patient information
enterprise_ext <- data.frame(
  Uid = uids,
  MPIUpdateDTTM = random_datetime(N_PATIENTS, as.Date("2020-01-01"), CURRENT_DATE),
  CDM_PATID = paste0("PAT", sprintf("%07d", uids)),
  EPIC_PAT_ID = sample_with_na(paste0("EPIC", sprintf("%010d", sample(1:99999999, N_PATIENTS))),
                                N_PATIENTS, prob_na = 0.2),
  ALLSCRIPTS_PERSON_ID = sample_with_na(as.integer(sample(1:999999, N_PATIENTS)),
                                        N_PATIENTS, prob_na = 0.7),
  MHH_COVID_PERSON = sample_with_na(as.integer(sample(1:999999, N_PATIENTS)),
                                     N_PATIENTS, prob_na = 0.8),
  MHH_MRN = sample_with_na(paste0("MHH", sprintf("%07d", sample(1:9999999, N_PATIENTS))),
                           N_PATIENTS, prob_na = 0.6),
  UTP_MRN = sample_with_na(paste0("UTP", sprintf("%07d", sample(1:9999999, N_PATIENTS))),
                           N_PATIENTS, prob_na = 0.7),
  EpicOnly = sample(c("Y", "N"), N_PATIENTS, replace = TRUE, prob = c(0.3, 0.7)),
  CDM_SEX = enterprise_records$G,
  CDM_RACE = sample(c("01", "02", "03", "04", "05", "06", "07"), N_PATIENTS, replace = TRUE),
  CDM_HISPANIC = sample(c("Y", "N", "NI"), N_PATIENTS, replace = TRUE, prob = c(0.2, 0.7, 0.1)),
  Race = sample(c("White", "Black", "Asian", "Other", "Unknown"), N_PATIENTS, replace = TRUE),
  Ethnicity = sample(c("Hispanic", "Not Hispanic", "Unknown"), N_PATIENTS, replace = TRUE),
  Language = sample(c("English", "Spanish", "Other"), N_PATIENTS, replace = TRUE, prob = c(0.7, 0.2, 0.1)),
  IsDeceased = sample(c("Y", "N"), N_PATIENTS, replace = TRUE, prob = c(0.05, 0.95)),
  DeceasedDTTM = as.POSIXct(NA),
  State = "TX",
  stringsAsFactors = FALSE
)

# Set death dates for deceased patients
deceased_idx <- which(enterprise_ext$IsDeceased == "Y")
enterprise_ext$DeceasedDTTM[deceased_idx] <- as.POSIXct(
  random_date(length(deceased_idx),
              pmax(enterprise_records$DoB[deceased_idx] + 365, as.Date("2015-01-01")),
              CURRENT_DATE)
)

# DuckDB handles native date types - no conversion needed
dbWriteTable(con_mpi, "EnterpriseRecords_Ext", enterprise_ext, overwrite = TRUE)

# MPI - Master Patient Index mapping
# Create source systems
sources <- c("EPIC", "ALLSCRIPTS", "MHH_COVID", "UTP")
mpi_records <- list()

for (src in sources) {
  # Not all patients in all systems
  n_in_system <- sample(floor(N_PATIENTS * 0.6):floor(N_PATIENTS * 0.95), 1)
  uids_in_system <- sample(uids, n_in_system)
  
  mpi_records[[src]] <- data.frame(
    Src = src,
    Lid = paste0(src, "_", sprintf("%010d", sample(1:99999999, n_in_system))),
    Uid = uids_in_system,
    Organization = sample(c("MHH", "UTP", "GECC"), n_in_system, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

mpi <- do.call(rbind, mpi_records)
rownames(mpi) <- NULL

dbWriteTable(con_mpi, "Mpi", mpi, overwrite = TRUE)

# MPI_Src - Source system definitions
mpi_src <- data.frame(
  SRC = sources,
  LID_SRC = c("EPIC_PAT_ID", "ALLSCRIPTS_PERSON_ID", "MHH_COVID_PERSON", "UTP_MRN"),
  Description = c("Epic EHR System", "Allscripts EHR", "MHH COVID Registry", "UTP Medical Records"),
  stringsAsFactors = FALSE
)

dbWriteTable(con_mpi, "MPI_Src", mpi_src, overwrite = TRUE)

cat("MPI database created with", N_PATIENTS, "patients\n")

# ============================================================================
# CDW DATABASE - Clinical Data Warehouse (PCORnet CDM)
# ============================================================================

cat("Creating CDW database...\n")

# DEMOGRAPHIC table
demographic <- data.frame(
  PATID = paste0("PAT", sprintf("%07d", uids)),
  BIRTH_DATE = enterprise_records$DoB,
  BIRTH_TIME = sample_with_na(sprintf("%02d:%02d", sample(0:23, N_PATIENTS, replace = TRUE),
                                       sample(0:59, N_PATIENTS, replace = TRUE)),
                               N_PATIENTS, prob_na = 0.5),
  SEX = enterprise_records$G,
  SEXUAL_ORIENTATION = sample_with_na(c("AS", "BI", "GA", "LE", "QS", "SE", "OT", "NI", "UN"),
                                      N_PATIENTS, prob_na = 0.3, replace = TRUE),
  GENDER_IDENTITY = sample_with_na(c("F", "M", "TM", "TF", "GQ", "OT", "NI", "UN"),
                                   N_PATIENTS, prob_na = 0.2, replace = TRUE),
  HISPANIC = enterprise_ext$CDM_HISPANIC,
  RACE = enterprise_ext$CDM_RACE,
  PAT_PREF_LANGUAGE_SPOKEN = sample_with_na(c("eng", "spa", "zho", "vie", "ara", "oth"),
                                            N_PATIENTS, prob_na = 0.15, replace = TRUE,
                                            prob = c(0.70, 0.20, 0.03, 0.02, 0.02, 0.03)),
  firstName = enterprise_records$First,
  lastName = enterprise_records$Last,
  addressLine1 = enterprise_records$Addy,
  city = enterprise_records$City,
  state = "TX",
  zipCode = enterprise_records$Zip,
  isDeceased = enterprise_ext$IsDeceased,
  CDW_Source = sample(c("EPIC", "ALLSCRIPTS", "MHH"), N_PATIENTS, replace = TRUE),
  CDW_UpdatedDTTM = random_datetime(N_PATIENTS, as.Date("2020-01-01"), CURRENT_DATE),
  GPC_FLAG = "Y",
  UID = uids,
  stringsAsFactors = FALSE
)

# Add MRNs
demographic$EPIC_PAT_ID <- enterprise_ext$EPIC_PAT_ID
demographic$MHH_MRN <- enterprise_ext$MHH_MRN
demographic$UTP_MRN <- enterprise_ext$UTP_MRN

# Add RAW_ fields (source system original values before mapping to PCORnet codes)
demographic$RAW_SEX <- demographic$SEX
demographic$RAW_RACE <- enterprise_ext$Race
demographic$RAW_HISPANIC <- enterprise_ext$Ethnicity

# DuckDB handles native date types - no conversion needed
dbWriteTable(con_cdw, "DEMOGRAPHIC", demographic, overwrite = TRUE)

# DEATH table
death_records <- enterprise_ext %>%
  filter(IsDeceased == "Y") %>%
  transmute(
    PATID = paste0("PAT", sprintf("%07d", Uid)),
    DEATH_DATE = as.Date(DeceasedDTTM),
    DEATH_DATE_IMPUTE = "N",
    DEATH_SOURCE = sample(c("L", "N", "S"), n(), replace = TRUE),
    DEATH_MATCH_CONFIDENCE = NA,
    GPC_FLAG = "Y",
    CDW_UpdatedDTTM = CURRENT_DATETIME,
    UID = Uid,
    MPI_LID = NA,
    MPI_SRC = NA
  )

dbWriteTable(con_cdw, "DEATH", death_records, overwrite = TRUE)

# DEATH_CAUSE table
if (nrow(death_records) > 0) {
  death_cause_codes <- c(
    "I21.9", "I25.10", "J44.1", "C34.90", "N18.9",
    "E11.9", "I64", "J18.9", "I50.9", "C25.9"
  )
  death_cause_names <- c(
    "Acute myocardial infarction", "Coronary artery disease", "COPD with exacerbation",
    "Lung cancer", "Chronic kidney disease", "Type 2 diabetes complications",
    "Cerebrovascular accident", "Pneumonia", "Heart failure", "Pancreatic cancer"
  )

  death_causes <- list()

  for (i in 1:nrow(death_records)) {
    # Each death gets 1-3 cause codes
    n_causes <- sample(1:3, 1, prob = c(0.5, 0.35, 0.15))

    for (j in 1:n_causes) {
      code_idx <- sample(1:length(death_cause_codes), 1)

      death_causes[[length(death_causes) + 1]] <- data.frame(
        PATID = death_records$PATID[i],
        DEATH_CAUSE = death_cause_names[code_idx],
        DEATH_CAUSE_CODE = death_cause_codes[code_idx],
        DEATH_CAUSE_TYPE = "09",
        DEATH_CAUSE_SOURCE = sample(c("L", "N", "D"), 1),
        DEATH_CAUSE_CONFIDENCE = sample_with_na(c("E", "P"), 1, prob_na = 0.3),
        GPC_FLAG = "Y",
        CDW_UpdatedDTTM = CURRENT_DATETIME,
        UID = death_records$UID[i],
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(death_causes) > 0) {
    death_cause <- do.call(rbind, death_causes)
    dbWriteTable(con_cdw, "DEATH_CAUSE", death_cause, overwrite = TRUE)
    cat("Generated", nrow(death_cause), "death causes for", nrow(death_records), "deceased patients\n")
  }
}

# ENCOUNTER table
cat("Generating encounters...\n")
encounters <- list()
enc_id <- 1

for (i in 1:N_PATIENTS) {
  patid <- paste0("PAT", sprintf("%07d", i))
  birth_date <- demographic$BIRTH_DATE[i]
  death_date <- if (i %in% deceased_idx) as.Date(enterprise_ext$DeceasedDTTM[i]) else CURRENT_DATE

  # Generate 5-20 encounters per patient
  n_enc <- sample(5:20, 1)

  # Most encounters after birth, some data quality issues (5% before birth or after death)
  valid_encounters <- floor(n_enc * 0.95)
  invalid_encounters <- n_enc - valid_encounters

  admit_dates <- c(
    random_date(valid_encounters, birth_date + 365, death_date),
    random_date(invalid_encounters, birth_date - 365, birth_date + 365)
  )
  
  for (j in 1:n_enc) {
    enc_type <- sample(c("IP", "ED", "AV", "OA", "IS"), 1, 
                       prob = c(0.15, 0.10, 0.50, 0.15, 0.10))
    
    # Length of stay varies by type
    los_days <- switch(enc_type,
                       "IP" = sample(1:14, 1),
                       "ED" = sample(0:1, 1),
                       "AV" = 0,
                       "OA" = 0,
                       "IS" = sample(1:7, 1),
                       0)
    
    discharge_disp <- if (enc_type %in% c("IP", "ED"))
      sample(c("A", "E", "H", "NI"), 1) else NA

    encounters[[enc_id]] <- data.frame(
      ENCOUNTERID = paste0("ENC", sprintf("%010d", enc_id)),
      PATID = patid,
      ADMIT_DATE = admit_dates[j],
      ADMIT_TIME = sprintf("%02d:%02d", sample(0:23, 1), sample(0:59, 1)),
      DISCHARGE_DATE = if (los_days > 0) admit_dates[j] + los_days else NA,
      DISCHARGE_TIME = if (los_days > 0) sprintf("%02d:%02d", sample(0:23, 1), sample(0:59, 1)) else NA,
      PROVIDERID = paste0("PROV", sprintf("%06d", sample(1:500, 1))),
      ENC_TYPE = enc_type,
      FACILITY_LOCATION = sample_with_na(c("01", "02", "03", "04"), 1, prob_na = 0.4),
      FACILITYID = sample_with_na(paste0("FAC", sprintf("%03d", sample(1:50, 1))), 1, prob_na = 0.5),
      DISCHARGE_DISPOSITION = discharge_disp,
      DISCHARGE_STATUS = if (!is.na(discharge_disp)) sample(c("01", "02", "NI"), 1) else NA,
      DRG = if (enc_type == "IP") sprintf("%03d", sample(1:999, 1)) else NA,
      DRG_TYPE = if (enc_type == "IP") "02" else NA,
      ADMITTING_SOURCE = sample_with_na(c("01", "02", "03", "04", "NI"), 1, prob_na = 0.3),
      RAW_ENC_TYPE = paste(sample(c("Inpatient", "Emergency", "Ambulatory", "Outpatient", "Institutional"), 1)),
      RAW_DISCHARGE_DISPOSITION = if (!is.na(discharge_disp))
        sample(c("Home", "Expired", "Hospice", "Unknown"), 1) else NA,
      RAW_DISCHARGE_STATUS = if (!is.na(discharge_disp))
        sample(c("Alive", "Deceased", "Unknown"), 1) else NA,
      PAYER_TYPE_PRIMARY = sample_with_na(c("01", "02", "03", "04", "05"), 1, prob_na = 0.2),
      CDW_Source = sample(c("EPIC", "ALLSCRIPTS"), 1),
      CDW_UpdatedDTTM = CURRENT_DATETIME,
      GPC_FLAG = "Y",
      UID = i,
      MPI_SRC = NA,
      MPI_LID = NA,
      stringsAsFactors = FALSE
    )

    enc_id <- enc_id + 1
  }
}

encounter <- do.call(rbind, encounters)

# DuckDB handles native date types - no conversion needed
dbWriteTable(con_cdw, "ENCOUNTER", encounter, overwrite = TRUE)

cat("Generated", nrow(encounter), "encounters\n")

# DIAGNOSIS table
cat("Generating diagnoses...\n")
icd10_codes <- c("E11.9", "I10", "J44.9", "F41.9", "M79.3", "Z79.4", "E78.5", 
                 "N18.3", "K21.9", "G47.33", "E66.9", "Z87.891", "F17.210")
dx_descriptions <- c("Type 2 diabetes", "Hypertension", "COPD", "Anxiety", "Fibromyalgia",
                    "Long term insulin use", "Hyperlipidemia", "CKD Stage 3", "GERD", 
                    "Sleep apnea", "Obesity", "Nicotine dependence", "Tobacco use")

diagnoses <- list()
dx_id <- 1

for (i in 1:nrow(encounter)) {
  n_dx <- sample(1:5, 1)
  
  for (j in 1:n_dx) {
    code_idx <- sample(1:length(icd10_codes), 1)
    
    dx_type_val <- sample(c("09", "10"), 1)
    dx_origin_val <- sample_with_na(c("OD", "BI", "CL", "NI"), 1, prob_na = 0.3)
    dx_poa_val <- if (encounter$ENC_TYPE[i] == "IP") sample(c("Y", "N", "U", "W"), 1) else NA
    pdx_val <- if (j == 1) "P" else "S"

    diagnoses[[dx_id]] <- data.frame(
      DIAGNOSISID = paste0("DX", sprintf("%010d", dx_id)),
      PATID = encounter$PATID[i],
      ENCOUNTERID = encounter$ENCOUNTERID[i],
      ADMIT_DATE = encounter$ADMIT_DATE[i],
      DX = icd10_codes[code_idx],
      DX_DATE = encounter$ADMIT_DATE[i],
      DX_TYPE = dx_type_val,
      DX_SOURCE = "AD",
      DX_ORIGIN = dx_origin_val,
      DX_POA = dx_poa_val,
      ENC_TYPE = encounter$ENC_TYPE[i],
      PDX = pdx_val,
      RAW_DX = dx_descriptions[code_idx],
      RAW_DX_TYPE = if (dx_type_val == "09") "ICD-9" else "ICD-10",
      RAW_DX_SOURCE = "Admitting Diagnosis",
      RAW_PDX = if (pdx_val == "P") "Primary" else "Secondary",
      PROVIDERID = paste0("PROV", sprintf("%06d", sample(1:500, 1))),
      CDW_Source = encounter$CDW_Source[i],
      CDW_UpdatedDTTM = CURRENT_DATETIME,
      GPC_FLAG = "Y",
      UID = encounter$UID[i],
      stringsAsFactors = FALSE
    )
    
    dx_id <- dx_id + 1
  }
}

diagnosis <- do.call(rbind, diagnoses)

# DuckDB handles native date types - no conversion needed
dbWriteTable(con_cdw, "DIAGNOSIS", diagnosis, overwrite = TRUE)

cat("Generated", nrow(diagnosis), "diagnoses\n")

# PROCEDURES table
cat("Generating procedures...\n")
cpt_codes <- c("99213", "99214", "80053", "93000", "85025", "36415", "99395", "71020")
px_descriptions <- c("Office visit", "Office visit extended", "Comprehensive metabolic panel",
                    "ECG", "CBC", "Venipuncture", "Annual exam", "Chest X-ray")

procedures <- list()
px_id <- 1

for (i in 1:nrow(encounter)) {
  n_px <- sample(0:3, 1, prob = c(0.2, 0.4, 0.3, 0.1))
  
  if (n_px > 0) {
    for (j in 1:n_px) {
      code_idx <- sample(1:length(cpt_codes), 1)
      
      px_type_val <- "CH"
      ppx_val <- if (j == 1) "P" else sample_with_na(c("P", "S"), 1, prob_na = 0.5)

      procedures[[px_id]] <- data.frame(
        PROCEDURESID = paste0("PX", sprintf("%010d", px_id)),
        PATID = encounter$PATID[i],
        ENCOUNTERID = encounter$ENCOUNTERID[i],
        ADMIT_DATE = encounter$ADMIT_DATE[i],
        PX = cpt_codes[code_idx],
        PX_DATE = encounter$ADMIT_DATE[i],
        PX_TYPE = px_type_val,
        PX_SOURCE = "OD",
        PPX = ppx_val,
        ENC_TYPE = encounter$ENC_TYPE[i],
        RAW_PX = px_descriptions[code_idx],
        RAW_PX_TYPE = "CPT",
        RAW_PX_NAME = px_descriptions[code_idx],
        PROVIDERID = paste0("PROV", sprintf("%06d", sample(1:500, 1))),
        CDW_Source = encounter$CDW_Source[i],
        CDW_UpdatedDTTM = CURRENT_DATETIME,
        GPC_FLAG = "Y",
        UID = encounter$UID[i],
        stringsAsFactors = FALSE
      )
      
      px_id <- px_id + 1
    }
  }
}

if (length(procedures) > 0) {
  procedure <- do.call(rbind, procedures)

  # DuckDB handles native date types - no conversion needed
  dbWriteTable(con_cdw, "PROCEDURES", procedure, overwrite = TRUE)
  cat("Generated", nrow(procedure), "procedures\n")
}

# LAB_RESULT_CM table
cat("Generating lab results...\n")
lab_tests <- data.frame(
  LOINC = c("2345-7", "4548-4", "2160-0", "33914-3", "2951-2", "6690-2"),
  NAME = c("Glucose", "HbA1c", "Creatinine", "GFR", "Sodium", "WBC"),
  UNIT = c("mg/dL", "%", "mg/dL", "mL/min/1.73m2", "mmol/L", "10*3/uL"),
  NORMAL_LOW = c(70, 4.0, 0.6, 60, 135, 4.0),
  NORMAL_HIGH = c(100, 5.6, 1.2, 120, 145, 11.0),
  stringsAsFactors = FALSE
)

lab_results <- list()
lab_id <- 1

for (i in 1:nrow(encounter)) {
  # Labs more common for certain encounter types
  prob_lab <- ifelse(encounter$ENC_TYPE[i] %in% c("IP", "ED", "IS"), 0.8, 0.3)
  
  if (runif(1) < prob_lab) {
    n_labs <- sample(2:8, 1)
    
    for (j in 1:n_labs) {
      test_idx <- sample(1:nrow(lab_tests), 1)
      
      # Generate result value (80% normal, 20% abnormal)
      if (runif(1) < 0.8) {
        result_num <- runif(1, lab_tests$NORMAL_LOW[test_idx], lab_tests$NORMAL_HIGH[test_idx])
        abn_ind <- "N"
      } else {
        if (runif(1) < 0.5) {
          result_num <- runif(1, lab_tests$NORMAL_LOW[test_idx] * 0.5, lab_tests$NORMAL_LOW[test_idx])
          abn_ind <- "AH"  # Abnormal Low
        } else {
          result_num <- runif(1, lab_tests$NORMAL_HIGH[test_idx], lab_tests$NORMAL_HIGH[test_idx] * 1.5)
          abn_ind <- "AH"  # Abnormal High
        }
      }
      
      result_date <- encounter$ADMIT_DATE[i]
      if (!is.na(encounter$DISCHARGE_DATE[i])) {
        result_date <- random_date(1, encounter$ADMIT_DATE[i], encounter$DISCHARGE_DATE[i])
      }
      
      lab_results[[lab_id]] <- data.frame(
        LAB_RESULT_CM_ID = paste0("LAB", sprintf("%010d", lab_id)),
        PATID = encounter$PATID[i],
        ENCOUNTERID = encounter$ENCOUNTERID[i],
        LAB_LOINC = lab_tests$LOINC[test_idx],
        LAB_PX = sample_with_na(paste0("LAB", sample(1000:9999, 1)), 1, prob_na = 0.7),
        LAB_PX_TYPE = sample_with_na(c("CH", "LC"), 1, prob_na = 0.7),
        LAB_ORDER_DATE = result_date - sample(0:3, 1),
        RESULT_DATE = result_date,
        RESULT_TIME = sprintf("%02d:%02d", sample(0:23, 1), sample(0:59, 1)),
        RESULT_NUM = round(result_num, 2),
        RESULT_QUAL = sample_with_na(c("POSITIVE", "NEGATIVE", "NORMAL"), 1, prob_na = 0.8),
        RESULT_MODIFIER = sample_with_na(c("EQ", "GE", "GT", "LE", "LT"), 1, prob_na = 0.9),
        RESULT_UNIT = lab_tests$UNIT[test_idx],
        NORM_RANGE_LOW = as.character(lab_tests$NORMAL_LOW[test_idx]),
        NORM_RANGE_HIGH = as.character(lab_tests$NORMAL_HIGH[test_idx]),
        NORM_MODIFIER_LOW = sample_with_na(c("EQ", "GE", "GT"), 1, prob_na = 0.8),
        NORM_MODIFIER_HIGH = sample_with_na(c("EQ", "LE", "LT"), 1, prob_na = 0.8),
        ABN_IND = abn_ind,
        SPECIMEN_SOURCE = sample_with_na(c("BLOOD", "URINE", "SERUM"), 1, prob_na = 0.5),
        SPECIMEN_DATE = result_date,
        PRIORITY = sample_with_na(c("01", "02", "03"), 1, prob_na = 0.6),
        RESULT_LOC = sample_with_na(c("L", "P"), 1, prob_na = 0.4),
        LAB_LOINC_SOURCE = "OD",
        LAB_RESULT_SOURCE = "OD",
        RAW_LAB_NAME = lab_tests$NAME[test_idx],
        RAW_LAB_CODE = lab_tests$LOINC[test_idx],
        RAW_RESULT = as.character(round(result_num, 2)),
        RAW_UNIT = lab_tests$UNIT[test_idx],
        CDW_Source = encounter$CDW_Source[i],
        CDW_UpdatedDTTM = CURRENT_DATETIME,
        GPC_FLAG = "Y",
        UID = encounter$UID[i],
        stringsAsFactors = FALSE
      )
      
      lab_id <- lab_id + 1
    }
  }
}

if (length(lab_results) > 0) {
  lab_result_cm <- do.call(rbind, lab_results)

  # DuckDB handles native date types - no conversion needed
  dbWriteTable(con_cdw, "LAB_RESULT_CM", lab_result_cm, overwrite = TRUE)
  cat("Generated", nrow(lab_result_cm), "lab results\n")
}

# PRESCRIBING table
cat("Generating prescriptions...\n")
medications <- data.frame(
  NAME = c("Metformin", "Lisinopril", "Atorvastatin", "Levothyroxine", "Amlodipine",
           "Albuterol", "Omeprazole", "Gabapentin", "Sertraline", "Aspirin"),
  RXNORM = c("6809", "29046", "83367", "10582", "17767", "435", "7646", "25480", "36437", "1191"),
  DOSE = c(500, 10, 20, 50, 5, 90, 20, 300, 50, 81),
  UNIT = c("mg", "mg", "mg", "mcg", "mg", "mcg", "mg", "mg", "mg", "mg"),
  stringsAsFactors = FALSE
)

prescriptions <- list()
rx_id <- 1

for (i in 1:nrow(encounter)) {
  n_rx <- sample(0:4, 1, prob = c(0.3, 0.3, 0.2, 0.15, 0.05))
  
  if (n_rx > 0) {
    for (j in 1:n_rx) {
      med_idx <- sample(1:nrow(medications), 1)
      
      rx_start <- encounter$ADMIT_DATE[i]
      days_supply <- sample(c(30, 60, 90), 1)
      rx_end <- rx_start + days_supply
      
      rx_freq <- sample(c("01", "02", "03", "04", "05"), 1)
      rx_order_date <- rx_start - sample(0:2, 1)

      prescriptions[[rx_id]] <- data.frame(
        PRESCRIBINGID = paste0("RX", sprintf("%010d", rx_id)),
        PATID = encounter$PATID[i],
        ENCOUNTERID = encounter$ENCOUNTERID[i],
        RX_PROVIDERID = paste0("PROV", sprintf("%06d", sample(1:500, 1))),
        RX_ORDER_DATE = rx_order_date,
        RX_ORDER_TIME = sprintf("%02d:%02d", sample(0:23, 1), sample(0:59, 1)),
        RX_START_DATE = rx_start,
        RX_END_DATE = rx_end,
        RX_DAYS_SUPPLY = days_supply,
        RX_REFILLS = sample(0:3, 1),
        RX_QUANTITY = days_supply,
        RX_DOSE_ORDERED = as.character(medications$DOSE[med_idx]),
        RX_DOSE_ORDERED_UNIT = medications$UNIT[med_idx],
        RX_DOSE_FORM = sample_with_na(c("01", "02", "03", "04"), 1, prob_na = 0.4),
        RX_FREQUENCY = rx_freq,
        RX_ROUTE = sample_with_na(c("01", "02", "03", "06"), 1, prob_na = 0.3),
        RX_BASIS = "01",
        RX_PRN_FLAG = sample_with_na(c("Y", "N"), 1, prob_na = 0.7),
        RX_DISPENSE_AS_WRITTEN = sample_with_na(c("Y", "N"), 1, prob_na = 0.6),
        RX_SOURCE = "OD",
        RXNORM_CUI = medications$RXNORM[med_idx],
        RAW_RX_MED_NAME = medications$NAME[med_idx],
        RAW_RX_FREQUENCY = paste(sample(c("Once daily", "Twice daily", "Three times daily"), 1)),
        RAW_RX_DOSE_ORDERED = as.character(medications$DOSE[med_idx]),
        RAW_RX_DOSE_ORDERED_UNIT = medications$UNIT[med_idx],
        RAW_RX_ROUTE = sample_with_na(c("Oral", "Topical", "Injection"), 1, prob_na = 0.5),
        RAW_RX_REFILLS = as.character(sample(0:3, 1)),
        RAW_RXNORM_CUI = medications$RXNORM[med_idx],
        RAW_RX_NDC = sample_with_na(paste0(
          sprintf("%05d", sample(10000:99999, 1)),
          sprintf("%04d", sample(1000:9999, 1)),
          sprintf("%02d", sample(10:99, 1))
        ), 1, prob_na = 0.5),
        CDW_Source = encounter$CDW_Source[i],
        CDW_UpdatedDTTM = CURRENT_DATETIME,
        GPC_FLAG = "Y",
        UID = encounter$UID[i],
        stringsAsFactors = FALSE
      )
      
      rx_id <- rx_id + 1
    }
  }
}

if (length(prescriptions) > 0) {
  prescribing <- do.call(rbind, prescriptions)

  # DuckDB handles native date types - no conversion needed
  dbWriteTable(con_cdw, "PRESCRIBING", prescribing, overwrite = TRUE)
  cat("Generated", nrow(prescribing), "prescriptions\n")
}

# DISPENSING table
cat("Generating dispensing records...\n")
dispensing_records <- list()
disp_id <- 1

if (length(prescriptions) > 0 && nrow(prescribing) > 0) {
  # Generate dispensing records for ~60% of prescriptions
  for (i in 1:nrow(prescribing)) {
    # Some prescriptions get multiple fills (refills)
    n_dispenses <- sample(0:3, 1, prob = c(0.4, 0.4, 0.15, 0.05))

    if (n_dispenses > 0) {
      for (j in 1:n_dispenses) {
        # Dispense date is after RX start date
        days_after_rx <- if (j == 1) sample(0:7, 1) else sample(25:35, 1) * j
        dispense_date <- prescribing$RX_START_DATE[i] + days_after_rx

        # Make sure dispense date doesn't exceed RX end date + reasonable window
        if (dispense_date <= prescribing$RX_END_DATE[i] + 30) {
          dispensing_records[[disp_id]] <- data.frame(
            DISPENSINGID = paste0("DISP", sprintf("%010d", disp_id)),
            PATID = prescribing$PATID[i],
            PRESCRIBINGID = prescribing$PRESCRIBINGID[i],
            DISPENSE_DATE = dispense_date,
            NDC = paste0(
              sprintf("%05d", sample(10000:99999, 1)),
              sprintf("%04d", sample(1000:9999, 1)),
              sprintf("%02d", sample(10:99, 1))
            ),
            DISPENSE_SUP = sample(c(30, 60, 90), 1),
            DISPENSE_AMT = sample(30:90, 1),
            DISPENSE_DOSE_DISP = prescribing$RX_DOSE_ORDERED[i],
            DISPENSE_DOSE_DISP_UNIT = prescribing$RX_DOSE_ORDERED_UNIT[i],
            DISPENSE_ROUTE = prescribing$RX_ROUTE[i],
            DISPENSE_SOURCE = "OD",
            RAW_NDC = paste0(
              sprintf("%05d", sample(10000:99999, 1)),
              sprintf("%04d", sample(1000:9999, 1)),
              sprintf("%02d", sample(10:99, 1))
            ),
            RAW_DISPENSE_DOSE_DISP = prescribing$RAW_RX_DOSE_ORDERED[i],
            RAW_DISPENSE_DOSE_DISP_UNIT = prescribing$RAW_RX_DOSE_ORDERED_UNIT[i],
            RAW_DISPENSE_ROUTE = prescribing$RAW_RX_ROUTE[i],
            RAW_DISP_MED_NAME = prescribing$RAW_RX_MED_NAME[i],
            CDW_Source = prescribing$CDW_Source[i],
            CDW_UpdatedDTTM = CURRENT_DATETIME,
            GPC_FLAG = "Y",
            UID = prescribing$UID[i],
            stringsAsFactors = FALSE
          )

          disp_id <- disp_id + 1
        }
      }
    }
  }
}

if (length(dispensing_records) > 0) {
  dispensing <- do.call(rbind, dispensing_records)

  # DuckDB handles native date types - no conversion needed
  dbWriteTable(con_cdw, "DISPENSING", dispensing, overwrite = TRUE)
  cat("Generated", nrow(dispensing), "dispensing records\n")
}

# VITAL table
cat("Generating vitals...\n")
vitals <- list()
vital_id <- 1

for (i in 1:nrow(encounter)) {
  # Vitals very common for most encounters
  if (runif(1) < 0.9) {
    measure_date <- encounter$ADMIT_DATE[i]
    
    ht_val <- round(rnorm(1, 170, 15), 1)
    wt_val <- round(rnorm(1, 80, 20), 1)
    sys_val <- round(rnorm(1, 125, 15), 0)
    dia_val <- round(rnorm(1, 80, 10), 0)
    bmi_val <- round(wt_val / (ht_val/100)^2, 1)
    bp_pos <- sample_with_na(c("01", "02", "03"), 1, prob_na = 0.6)
    smoking_val <- sample_with_na(c("01", "02", "03", "04", "05", "06", "07", "NI"), 1, prob_na = 0.5)
    tobacco_val <- sample_with_na(c("01", "02", "03", "04", "06", "NI"), 1, prob_na = 0.5)

    vitals[[vital_id]] <- data.frame(
      VITALID = paste0("VIT", sprintf("%010d", vital_id)),
      PATID = encounter$PATID[i],
      ENCOUNTERID = encounter$ENCOUNTERID[i],
      MEASURE_DATE = measure_date,
      MEASURE_TIME = sprintf("%02d:%02d", sample(0:23, 1), sample(0:59, 1)),
      VITAL_SOURCE = "HC",
      HT = ht_val,
      WT = wt_val,
      ORIGINAL_BMI = bmi_val,
      SYSTOLIC = sys_val,
      DIASTOLIC = dia_val,
      BP_POSITION = bp_pos,
      SMOKING = smoking_val,
      TOBACCO = tobacco_val,
      TOBACCO_TYPE = sample_with_na(c("01", "02", "03", "04", "05", "NI"), 1, prob_na = 0.7),
      RAW_SYSTOLIC = as.character(sys_val),
      RAW_DIASTOLIC = as.character(dia_val),
      RAW_BP_POSITION = if (!is.na(bp_pos)) sample(c("Sitting", "Standing", "Lying"), 1) else NA,
      RAW_SMOKING = if (!is.na(smoking_val)) sample(c("Current smoker", "Former smoker", "Never smoker"), 1) else NA,
      RAW_TOBACCO = if (!is.na(tobacco_val)) sample(c("Yes", "No", "Unknown"), 1) else NA,
      RAW_TOBACCO_TYPE = sample_with_na(c("Cigarettes", "Cigars", "Chewing tobacco", "E-cigarettes"), 1, prob_na = 0.8),
      CDW_Source = encounter$CDW_Source[i],
      CDW_UpdatedDTTM = CURRENT_DATETIME,
      GPC_FLAG = "Y",
      UID = encounter$UID[i],
      stringsAsFactors = FALSE
    )
    
    vital_id <- vital_id + 1
  }
}

if (length(vitals) > 0) {
  vital <- do.call(rbind, vitals)

  # DuckDB handles native date types - no conversion needed
  dbWriteTable(con_cdw, "VITAL", vital, overwrite = TRUE)
  cat("Generated", nrow(vital), "vital measurements\n")
}

# CONDITION table
cat("Generating conditions...\n")
condition_codes <- c(
  "E11.9", "I10", "J44.9", "F41.9", "M79.3", "E78.5", "N18.3",
  "K21.9", "G47.33", "E66.9", "Z87.891", "I25.10", "M17.11"
)
condition_names <- c(
  "Type 2 diabetes mellitus", "Essential hypertension", "COPD", "Generalized anxiety disorder",
  "Fibromyalgia", "Hyperlipidemia", "CKD Stage 3", "GERD", "Sleep apnea", "Obesity",
  "Nicotine dependence", "Coronary artery disease", "Osteoarthritis of knee"
)

conditions <- list()
cond_id <- 1

# Generate chronic conditions for ~50% of patients
for (i in uids) {
  if (runif(1) < 0.5) {
    # Each patient with conditions gets 1-4 chronic conditions
    n_cond <- sample(1:4, 1, prob = c(0.4, 0.3, 0.2, 0.1))

    # Get patient's encounters for linking
    pat_encounters <- encounter %>% filter(UID == i)

    for (j in 1:n_cond) {
      code_idx <- sample(1:length(condition_codes), 1)

      # Onset date is before first encounter or random date in past
      onset_date <- if (nrow(pat_encounters) > 0) {
        min(pat_encounters$ADMIT_DATE) - sample(365:1825, 1)  # 1-5 years before first encounter
      } else {
        CURRENT_DATE - sample(365:3650, 1)  # 1-10 years ago
      }

      # Report date is onset date or later
      report_date <- onset_date + sample(0:180, 1)

      # Link to encounter if available
      encounterid <- if (nrow(pat_encounters) > 0 && runif(1) < 0.7) {
        sample(pat_encounters$ENCOUNTERID, 1)
      } else {
        NA
      }

      cond_status <- sample(c("AC", "RS", "IN"), 1, prob = c(0.7, 0.2, 0.1))
      resolve_date <- if (cond_status == "RS") report_date + sample(180:730, 1) else NA

      conditions[[cond_id]] <- data.frame(
        CONDITIONID = paste0("COND", sprintf("%010d", cond_id)),
        PATID = paste0("PAT", sprintf("%07d", i)),
        ENCOUNTERID = encounterid,
        CONDITION = condition_codes[code_idx],
        CONDITION_TYPE = "09",
        CONDITION_STATUS = cond_status,
        CONDITION_SOURCE = "HC",
        ONSET_DATE = onset_date,
        REPORT_DATE = report_date,
        RESOLVE_DATE = resolve_date,
        RAW_CONDITION = condition_names[code_idx],
        RAW_CONDITION_TYPE = "ICD-10",
        RAW_CONDITION_STATUS = switch(cond_status,
                                      "AC" = "Active",
                                      "RS" = "Resolved",
                                      "IN" = "Inactive"),
        RAW_CONDITION_SOURCE = "Healthcare",
        CDW_Source = sample(c("EPIC", "ALLSCRIPTS", "MHH"), 1),
        CDW_UpdatedDTTM = CURRENT_DATETIME,
        GPC_FLAG = "Y",
        UID = i,
        stringsAsFactors = FALSE
      )

      cond_id <- cond_id + 1
    }
  }
}

if (length(conditions) > 0) {
  condition <- do.call(rbind, conditions)

  # DuckDB handles native date types - no conversion needed
  dbWriteTable(con_cdw, "CONDITION", condition, overwrite = TRUE)
  cat("Generated", nrow(condition), "conditions\n")
}

# PROVIDER table
providers <- data.frame(
  PROVIDERID = paste0("PROV", sprintf("%06d", 1:500)),
  ProviderName = replicate(500, paste(sample(c("Dr.", "Dr."), 1),
                                      paste(sample(LETTERS, 1),
                                            paste(sample(letters, sample(4:8, 1)), collapse = "")))),
  PROVIDER_SPECIALTY_PRIMARY = sample(c("Internal Medicine", "Family Medicine", "Cardiology",
                                       "Endocrinology", "Emergency Medicine"), 500, replace = TRUE),
  PROVIDER_NPI = as.integer(sprintf("1%09d", sample(100000000:999999999, 500))),
  GPC_FLAG = "Y",
  stringsAsFactors = FALSE
)

dbWriteTable(con_cdw, "PROVIDER", providers, overwrite = TRUE)

# Summary statistics
cat("\n=== Database Creation Complete ===\n")
cat("MPI Database:\n")
cat("  Patients:", N_PATIENTS, "\n")
cat("  MPI mappings:", nrow(mpi), "\n")
cat("\nCDW Database:\n")
cat("  Patients:", N_PATIENTS, "\n")
cat("  Deceased patients:", nrow(death_records), "\n")
cat("  Encounters:", nrow(encounter), "\n")
cat("  Diagnoses:", nrow(diagnosis), "\n")
if (exists("procedure")) cat("  Procedures:", nrow(procedure), "\n")
if (exists("lab_result_cm")) cat("  Lab results:", nrow(lab_result_cm), "\n")
if (exists("prescribing")) cat("  Prescriptions:", nrow(prescribing), "\n")
if (exists("vital")) cat("  Vitals:", nrow(vital), "\n")
cat("  Providers:", nrow(providers), "\n")

# Save connection objects for use
cat("\nDatabases are in memory and ready to use:\n")
cat("  con_cdw - Clinical Data Warehouse\n")
cat("  con_mpi - Master Patient Index\n")

# Save databases to disk
cat("\nSaving databases to disk...\n")
con_cdw_disk <- dbConnect(duckdb::duckdb(), dbdir = "pcornet_cdw.duckdb")
con_mpi_disk <- dbConnect(duckdb::duckdb(), dbdir = "mpi.duckdb")

# Copy all tables from in-memory to disk databases
for (table in dbListTables(con_cdw)) {
  dbWriteTable(con_cdw_disk, table, dbReadTable(con_cdw, table), overwrite = TRUE)
}

for (table in dbListTables(con_mpi)) {
  dbWriteTable(con_mpi_disk, table, dbReadTable(con_mpi, table), overwrite = TRUE)
}

dbDisconnect(con_cdw_disk, shutdown = TRUE)
dbDisconnect(con_mpi_disk, shutdown = TRUE)

cat("Databases saved:\n")
cat("  pcornet_cdw.duckdb - Clinical Data Warehouse\n")
cat("  mpi.duckdb - Master Patient Index\n")

# Example queries
cat("\nExample queries:\n")
cat("  dbListTables(con_cdw)\n")
cat("  dbGetQuery(con_cdw, 'SELECT COUNT(*) FROM DEMOGRAPHIC')\n")
cat("  dbGetQuery(con_mpi, 'SELECT COUNT(*) FROM Mpi')\n")

cat("\nTo reload databases later:\n")
cat("  source('load_databases.R')\n")

# Return the connections
list(cdw = con_cdw, mpi = con_mpi)
