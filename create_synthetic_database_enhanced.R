# Enhanced Synthetic Database Generator for PCORnet CDM and MPI
# Creates DuckDB databases with clinically coherent test data using clinical profiles
#
# This version uses clinical profiles to generate realistic patterns where:
# - Diabetic patients have diabetes diagnoses, HbA1c labs, and Metformin prescriptions
# - Cardiac patients have heart disease diagnoses, lipid panels, and statins
# - etc.

library(DBI)
library(duckdb)
library(dplyr)
library(lubridate)

# Load clinical profile definitions and generator functions
source("clinical_profiles/clinical_profiles.R")
source("clinical_profiles/profile_generator.R")

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
  times <- runif(n, 0, 86400)
  datetimes <- as.POSIXct(as.numeric(dates) * 86400 + times, origin = "1970-01-01", tz = "UTC")
  return(datetimes)
}

# Create databases
con_cdw <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")
con_mpi <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")

cat("Generating synthetic patient data with clinical profiles...\n")

# ============================================================================
# MPI DATABASE - Master Patient Index (unchanged from original)
# ============================================================================

cat("Creating MPI database...\n")

uids <- 1:N_PATIENTS

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

enterprise_records$FirstSdx <- substr(enterprise_records$First, 1, 4)
enterprise_records$LastSdx <- substr(enterprise_records$Last, 1, 4)

dbWriteTable(con_mpi, "EnterpriseRecords", enterprise_records, overwrite = TRUE)

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

deceased_idx <- which(enterprise_ext$IsDeceased == "Y")
enterprise_ext$DeceasedDTTM[deceased_idx] <- as.POSIXct(
  random_date(length(deceased_idx),
              pmax(enterprise_records$DoB[deceased_idx] + 365, as.Date("2015-01-01")),
              CURRENT_DATE)
)

dbWriteTable(con_mpi, "EnterpriseRecords_Ext", enterprise_ext, overwrite = TRUE)

sources <- c("EPIC", "ALLSCRIPTS", "MHH_COVID", "UTP")
mpi_records <- list()

for (src in sources) {
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

mpi_src <- data.frame(
  SRC = sources,
  LID_SRC = c("EPIC_PAT_ID", "ALLSCRIPTS_PERSON_ID", "MHH_COVID_PERSON", "UTP_MRN"),
  Description = c("Epic EHR System", "Allscripts EHR", "MHH COVID Registry", "UTP Medical Records"),
  stringsAsFactors = FALSE
)
dbWriteTable(con_mpi, "MPI_Src", mpi_src, overwrite = TRUE)

cat("MPI database created with", N_PATIENTS, "patients\n")

# ============================================================================
# CDW DATABASE - Clinical Data Warehouse with Clinical Profiles
# ============================================================================

cat("Creating CDW database with clinical profiles...\n")

# Create base patient data frame for profile assignment
patient_data <- data.frame(
  uid = uids,
  birth_date = enterprise_records$DoB,
  sex = enterprise_records$G,
  stringsAsFactors = FALSE
)

# Assign clinical profiles to patients
cat("Assigning clinical profiles to patients...\n")
patient_data <- assign_clinical_profiles(patient_data, CURRENT_DATE, CLINICAL_PROFILES)

# Display profile distribution
profile_counts <- table(patient_data$clinical_profile)
cat("\nClinical profile distribution:\n")
for (profile_name in names(profile_counts)) {
  cat(sprintf("  %s: %d patients (%.1f%%)\n",
              profile_name,
              profile_counts[profile_name],
              100 * profile_counts[profile_name] / N_PATIENTS))
}

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
  CLINICAL_PROFILE = patient_data$clinical_profile,
  stringsAsFactors = FALSE
)

demographic$EPIC_PAT_ID <- enterprise_ext$EPIC_PAT_ID
demographic$MHH_MRN <- enterprise_ext$MHH_MRN
demographic$UTP_MRN <- enterprise_ext$UTP_MRN
demographic$RAW_SEX <- demographic$SEX
demographic$RAW_RACE <- enterprise_ext$Race
demographic$RAW_HISPANIC <- enterprise_ext$Ethnicity

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

# Generate clinical data using profiles
cat("\nGenerating clinically coherent encounters and clinical data...\n")

all_encounters <- list()
all_diagnoses <- list()
all_labs <- list()
all_medications <- list()
all_procedures <- list()
all_vitals <- list()

enc_id <- 1
dx_id <- 1
lab_id <- 1
rx_id <- 1
px_id <- 1
vital_id <- 1

for (i in 1:N_PATIENTS) {
  patid <- paste0("PAT", sprintf("%07d", i))
  birth_date <- demographic$BIRTH_DATE[i]
  death_date <- if (i %in% deceased_idx) as.Date(enterprise_ext$DeceasedDTTM[i]) else CURRENT_DATE
  profile_name <- patient_data$clinical_profile[i]
  profile <- CLINICAL_PROFILES[[profile_name]]

  # Generate encounters based on profile
  n_enc <- generate_encounter_count(profile_name, CLINICAL_PROFILES)

  # Generate encounter dates (95% valid, 5% data quality issues)
  valid_encounters <- floor(n_enc * 0.95)
  invalid_encounters <- n_enc - valid_encounters

  admit_dates <- c(
    random_date(valid_encounters, birth_date + 365, death_date),
    random_date(invalid_encounters, birth_date - 365, birth_date + 365)
  )

  # Patient's medications (assigned once, used across encounters)
  patient_medications <- generate_profile_medications(profile)

  for (j in 1:n_enc) {
    enc_type <- select_encounter_type(profile_name)
    los_days <- calculate_length_of_stay(enc_type)

    discharge_disp <- if (enc_type %in% c("IP", "ED"))
      sample(c("A", "E", "H", "NI"), 1) else NA

    # Create encounter
    all_encounters[[enc_id]] <- data.frame(
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

    # Generate profile-based diagnoses
    dx_data <- generate_profile_diagnoses(profile, is_first_encounter = (j == 1))
    if (!is.null(dx_data) && nrow(dx_data) > 0) {
      for (k in 1:nrow(dx_data)) {
        dx_type_val <- "10"  # Always ICD-10 for profile-based
        dx_poa_val <- if (enc_type == "IP") sample(c("Y", "N", "U", "W"), 1) else NA
        pdx_val <- if (dx_data$is_primary[k]) "P" else "S"

        all_diagnoses[[dx_id]] <- data.frame(
          DIAGNOSISID = paste0("DX", sprintf("%010d", dx_id)),
          PATID = patid,
          ENCOUNTERID = paste0("ENC", sprintf("%010d", enc_id)),
          ADMIT_DATE = admit_dates[j],
          DX = dx_data$code[k],
          DX_DATE = admit_dates[j],
          DX_TYPE = dx_type_val,
          DX_SOURCE = "AD",
          DX_ORIGIN = sample_with_na(c("OD", "BI", "CL", "NI"), 1, prob_na = 0.3),
          DX_POA = dx_poa_val,
          ENC_TYPE = enc_type,
          PDX = pdx_val,
          RAW_DX = dx_data$description[k],
          RAW_DX_TYPE = "ICD-10",
          RAW_DX_SOURCE = "Admitting Diagnosis",
          RAW_PDX = if (pdx_val == "P") "Primary" else "Secondary",
          PROVIDERID = paste0("PROV", sprintf("%06d", sample(1:500, 1))),
          CDW_Source = all_encounters[[enc_id]]$CDW_Source,
          CDW_UpdatedDTTM = CURRENT_DATETIME,
          GPC_FLAG = "Y",
          UID = i,
          stringsAsFactors = FALSE
        )
        dx_id <- dx_id + 1
      }
    }

    # Generate profile-based labs
    lab_data <- generate_profile_labs(profile, enc_type)
    if (!is.null(lab_data) && nrow(lab_data) > 0) {
      for (k in 1:nrow(lab_data)) {
        result_date <- admit_dates[j]

        all_labs[[lab_id]] <- data.frame(
          LAB_RESULT_CM_ID = paste0("LAB", sprintf("%010d", lab_id)),
          PATID = patid,
          ENCOUNTERID = paste0("ENC", sprintf("%010d", enc_id)),
          LAB_LOINC = lab_data$loinc[k],
          LAB_PX = sample_with_na(paste0("LAB", sample(1000:9999, 1)), 1, prob_na = 0.7),
          LAB_PX_TYPE = sample_with_na(c("CH", "LC"), 1, prob_na = 0.7),
          LAB_ORDER_DATE = result_date - sample(0:3, 1),
          RESULT_DATE = result_date,
          RESULT_TIME = sprintf("%02d:%02d", sample(0:23, 1), sample(0:59, 1)),
          RESULT_NUM = lab_data$value[k],
          RESULT_QUAL = sample_with_na(c("POSITIVE", "NEGATIVE", "NORMAL"), 1, prob_na = 0.8),
          RESULT_MODIFIER = sample_with_na(c("EQ", "GE", "GT", "LE", "LT"), 1, prob_na = 0.9),
          RESULT_UNIT = lab_data$unit[k],
          NORM_RANGE_LOW = NA,
          NORM_RANGE_HIGH = NA,
          NORM_MODIFIER_LOW = sample_with_na(c("EQ", "GE", "GT"), 1, prob_na = 0.8),
          NORM_MODIFIER_HIGH = sample_with_na(c("EQ", "LE", "LT"), 1, prob_na = 0.8),
          ABN_IND = lab_data$abn_ind[k],
          SPECIMEN_SOURCE = sample_with_na(c("BLOOD", "URINE", "SERUM"), 1, prob_na = 0.5),
          SPECIMEN_DATE = result_date,
          PRIORITY = sample_with_na(c("01", "02", "03"), 1, prob_na = 0.6),
          RESULT_LOC = sample_with_na(c("L", "P"), 1, prob_na = 0.4),
          LAB_LOINC_SOURCE = "OD",
          LAB_RESULT_SOURCE = "OD",
          RAW_LAB_NAME = lab_data$name[k],
          RAW_LAB_CODE = lab_data$loinc[k],
          RAW_RESULT = as.character(lab_data$value[k]),
          RAW_UNIT = lab_data$unit[k],
          CDW_Source = all_encounters[[enc_id]]$CDW_Source,
          CDW_UpdatedDTTM = CURRENT_DATETIME,
          GPC_FLAG = "Y",
          UID = i,
          stringsAsFactors = FALSE
        )
        lab_id <- lab_id + 1
      }
    }

    # Generate profile-based procedures
    px_data <- generate_profile_procedures(profile, enc_type)
    if (!is.null(px_data) && nrow(px_data) > 0) {
      for (k in 1:nrow(px_data)) {
        ppx_val <- if (k == 1) "P" else sample_with_na(c("P", "S"), 1, prob_na = 0.5)

        all_procedures[[px_id]] <- data.frame(
          PROCEDURESID = paste0("PX", sprintf("%010d", px_id)),
          PATID = patid,
          ENCOUNTERID = paste0("ENC", sprintf("%010d", enc_id)),
          ADMIT_DATE = admit_dates[j],
          PX = px_data$cpt[k],
          PX_DATE = admit_dates[j],
          PX_TYPE = "CH",
          PX_SOURCE = "OD",
          PPX = ppx_val,
          ENC_TYPE = enc_type,
          RAW_PX = px_data$name[k],
          RAW_PX_TYPE = "CPT",
          RAW_PX_NAME = px_data$name[k],
          PROVIDERID = paste0("PROV", sprintf("%06d", sample(1:500, 1))),
          CDW_Source = all_encounters[[enc_id]]$CDW_Source,
          CDW_UpdatedDTTM = CURRENT_DATETIME,
          GPC_FLAG = "Y",
          UID = i,
          stringsAsFactors = FALSE
        )
        px_id <- px_id + 1
      }
    }

    # Generate profile-based vitals
    if (runif(1) < 0.9) {
      vital_data <- generate_profile_vitals(profile)
      bp_pos <- sample_with_na(c("01", "02", "03"), 1, prob_na = 0.6)
      smoking_val <- sample_with_na(c("01", "02", "03", "04", "05", "06", "07", "NI"), 1, prob_na = 0.5)
      tobacco_val <- sample_with_na(c("01", "02", "03", "04", "06", "NI"), 1, prob_na = 0.5)

      all_vitals[[vital_id]] <- data.frame(
        VITALID = paste0("VIT", sprintf("%010d", vital_id)),
        PATID = patid,
        ENCOUNTERID = paste0("ENC", sprintf("%010d", enc_id)),
        MEASURE_DATE = admit_dates[j],
        MEASURE_TIME = sprintf("%02d:%02d", sample(0:23, 1), sample(0:59, 1)),
        VITAL_SOURCE = "HC",
        HT = vital_data$ht,
        WT = vital_data$wt,
        ORIGINAL_BMI = vital_data$bmi,
        SYSTOLIC = vital_data$systolic,
        DIASTOLIC = vital_data$diastolic,
        BP_POSITION = bp_pos,
        SMOKING = smoking_val,
        TOBACCO = tobacco_val,
        TOBACCO_TYPE = sample_with_na(c("01", "02", "03", "04", "05", "NI"), 1, prob_na = 0.7),
        RAW_SYSTOLIC = as.character(vital_data$systolic),
        RAW_DIASTOLIC = as.character(vital_data$diastolic),
        RAW_BP_POSITION = if (!is.na(bp_pos)) sample(c("Sitting", "Standing", "Lying"), 1) else NA,
        RAW_SMOKING = if (!is.na(smoking_val)) sample(c("Current smoker", "Former smoker", "Never smoker"), 1) else NA,
        RAW_TOBACCO = if (!is.na(tobacco_val)) sample(c("Yes", "No", "Unknown"), 1) else NA,
        RAW_TOBACCO_TYPE = sample_with_na(c("Cigarettes", "Cigars", "Chewing tobacco", "E-cigarettes"), 1, prob_na = 0.8),
        CDW_Source = all_encounters[[enc_id]]$CDW_Source,
        CDW_UpdatedDTTM = CURRENT_DATETIME,
        GPC_FLAG = "Y",
        UID = i,
        stringsAsFactors = FALSE
      )
      vital_id <- vital_id + 1
    }

    # Generate prescriptions for some encounters (using patient's medication list)
    if (!is.null(patient_medications) && nrow(patient_medications) > 0 && runif(1) < 0.6) {
      # Select a subset of the patient's medications for this encounter
      n_meds_this_enc <- min(nrow(patient_medications), sample(1:3, 1))
      med_indices <- sample(1:nrow(patient_medications), n_meds_this_enc)

      for (k in med_indices) {
        med <- patient_medications[k, ]
        rx_start <- admit_dates[j]
        days_supply <- sample(c(30, 60, 90), 1)
        rx_end <- rx_start + days_supply
        rx_order_date <- rx_start - sample(0:2, 1)

        all_medications[[rx_id]] <- data.frame(
          PRESCRIBINGID = paste0("RX", sprintf("%010d", rx_id)),
          PATID = patid,
          ENCOUNTERID = paste0("ENC", sprintf("%010d", enc_id)),
          RX_PROVIDERID = paste0("PROV", sprintf("%06d", sample(1:500, 1))),
          RX_ORDER_DATE = rx_order_date,
          RX_ORDER_TIME = sprintf("%02d:%02d", sample(0:23, 1), sample(0:59, 1)),
          RX_START_DATE = rx_start,
          RX_END_DATE = rx_end,
          RX_DAYS_SUPPLY = days_supply,
          RX_REFILLS = sample(0:3, 1),
          RX_QUANTITY = days_supply,
          RX_DOSE_ORDERED = as.character(med$dose),
          RX_DOSE_ORDERED_UNIT = med$unit,
          RX_DOSE_FORM = sample_with_na(c("01", "02", "03", "04"), 1, prob_na = 0.4),
          RX_FREQUENCY = med$frequency_code,
          RX_ROUTE = sample_with_na(c("01", "02", "03", "06"), 1, prob_na = 0.3),
          RX_BASIS = "01",
          RX_PRN_FLAG = sample_with_na(c("Y", "N"), 1, prob_na = 0.7),
          RX_DISPENSE_AS_WRITTEN = sample_with_na(c("Y", "N"), 1, prob_na = 0.6),
          RX_SOURCE = "OD",
          RXNORM_CUI = med$rxnorm,
          RAW_RX_MED_NAME = med$name,
          RAW_RX_FREQUENCY = med$frequency_raw,
          RAW_RX_DOSE_ORDERED = as.character(med$dose),
          RAW_RX_DOSE_ORDERED_UNIT = med$unit,
          RAW_RX_ROUTE = sample_with_na(c("Oral", "Topical", "Injection"), 1, prob_na = 0.5),
          RAW_RX_REFILLS = as.character(sample(0:3, 1)),
          RAW_RXNORM_CUI = med$rxnorm,
          RAW_RX_NDC = sample_with_na(paste0(
            sprintf("%05d", sample(10000:99999, 1)),
            sprintf("%04d", sample(1000:9999, 1)),
            sprintf("%02d", sample(10:99, 1))
          ), 1, prob_na = 0.5),
          CDW_Source = all_encounters[[enc_id]]$CDW_Source,
          CDW_UpdatedDTTM = CURRENT_DATETIME,
          GPC_FLAG = "Y",
          UID = i,
          stringsAsFactors = FALSE
        )
        rx_id <- rx_id + 1
      }
    }

    enc_id <- enc_id + 1
  }

  if (i %% 20 == 0) {
    cat(sprintf("  Processed %d/%d patients...\n", i, N_PATIENTS))
  }
}

# Combine and write tables
encounter <- do.call(rbind, all_encounters)
dbWriteTable(con_cdw, "ENCOUNTER", encounter, overwrite = TRUE)
cat("Generated", nrow(encounter), "encounters\n")

diagnosis <- do.call(rbind, all_diagnoses)
dbWriteTable(con_cdw, "DIAGNOSIS", diagnosis, overwrite = TRUE)
cat("Generated", nrow(diagnosis), "diagnoses\n")

if (length(all_procedures) > 0) {
  procedure <- do.call(rbind, all_procedures)
  dbWriteTable(con_cdw, "PROCEDURES", procedure, overwrite = TRUE)
  cat("Generated", nrow(procedure), "procedures\n")
}

if (length(all_labs) > 0) {
  lab_result_cm <- do.call(rbind, all_labs)
  dbWriteTable(con_cdw, "LAB_RESULT_CM", lab_result_cm, overwrite = TRUE)
  cat("Generated", nrow(lab_result_cm), "lab results\n")
}

if (length(all_medications) > 0) {
  prescribing <- do.call(rbind, all_medications)
  dbWriteTable(con_cdw, "PRESCRIBING", prescribing, overwrite = TRUE)
  cat("Generated", nrow(prescribing), "prescriptions\n")
}

if (length(all_vitals) > 0) {
  vital <- do.call(rbind, all_vitals)
  dbWriteTable(con_cdw, "VITAL", vital, overwrite = TRUE)
  cat("Generated", nrow(vital), "vital measurements\n")
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
cat("\n=== Database Creation Complete (Enhanced with Clinical Profiles) ===\n")
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

cat("\nClinical Profile Summary:\n")
for (profile_name in names(profile_counts)) {
  cat(sprintf("  %s: %d patients\n", profile_name, profile_counts[profile_name]))
}

cat("\nDatabases are in memory and ready to use:\n")
cat("  con_cdw - Clinical Data Warehouse\n")
cat("  con_mpi - Master Patient Index\n")

# Save databases to disk
cat("\nSaving databases to disk...\n")
con_cdw_disk <- dbConnect(duckdb::duckdb(), dbdir = "pcornet_cdw.duckdb")
con_mpi_disk <- dbConnect(duckdb::duckdb(), dbdir = "mpi.duckdb")

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

cat("\nTo verify clinical coherence, try:\n")
cat("  # Check diabetic patients have diabetes diagnoses and Metformin\n")
cat("  dbGetQuery(con_cdw, \"\n")
cat("    SELECT d.PATID, d.CLINICAL_PROFILE, dx.DX, p.RXNORM_CUI, p.RAW_RX_MED_NAME\n")
cat("    FROM DEMOGRAPHIC d\n")
cat("    LEFT JOIN DIAGNOSIS dx ON d.PATID = dx.PATID\n")
cat("    LEFT JOIN PRESCRIBING p ON d.PATID = p.PATID\n")
cat("    WHERE d.CLINICAL_PROFILE = 'diabetic'\n")
cat("    AND dx.DX LIKE 'E11%'\n")
cat("    LIMIT 20\n")
cat("  \")\n")

list(cdw = con_cdw, mpi = con_mpi)
