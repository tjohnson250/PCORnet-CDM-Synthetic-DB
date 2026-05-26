# Synthea to PCORnet CDM Transformation Functions
# Converts Synthea CSV output to PCORnet CDM format in SQL Server

#' Load encounter type mappings
#' @keywords internal
.load_encounter_type_map <- function() {
  # Try to find mappings in package inst directory
  map_file <- system.file("synthea", "mappings", "encounter_type_map.csv",
                          package = "pcornet.synthetic")

  # Fallback to local synthea directory

if (map_file == "") {
    map_file <- file.path("synthea", "mappings", "encounter_type_map.csv")
  }

  if (file.exists(map_file)) {
    read.csv(map_file, stringsAsFactors = FALSE)
  } else {
    # Default mappings if file doesn't exist
    data.frame(
      synthea_class = c("ambulatory", "emergency", "inpatient", "wellness",
                        "urgentcare", "outpatient", "hospice", "home", "snf"),
      pcornet_enc_type = c("AV", "ED", "IP", "AV", "ED", "OA", "IS", "OA", "IS"),
      stringsAsFactors = FALSE
    )
  }
}

#' Load SNOMED to ICD-10 mappings for common conditions
#' @keywords internal
.load_snomed_icd10_map <- function() {
  # Try to find mappings in package inst directory
  map_file <- system.file("synthea", "mappings", "snomed_icd10_common.csv",
                          package = "pcornet.synthetic")

  # Fallback to local synthea directory
  if (map_file == "") {
    map_file <- file.path("synthea", "mappings", "snomed_icd10_common.csv")
  }

  if (file.exists(map_file)) {
    read.csv(map_file, stringsAsFactors = FALSE)
  } else {
    # Default mappings for common conditions
    data.frame(
      snomed_code = c(
        "44054006", "38341003", "73211009", "13645005", "40055000",
        "59621000", "195967001", "53741008", "84114007", "49436004",
        "230690007", "22298006", "399211009", "427089005", "706970001"
      ),
      icd10_code = c(
        "E11.9", "I10", "E11.9", "J44.9", "J45.909",
        "I10", "J06.9", "E66.9", "I50.9", "I48.91",
        "I63.9", "F32.9", "F41.9", "I25.10", "U07.1"
      ),
      description = c(
        "Type 2 diabetes mellitus", "Essential hypertension", "Diabetes mellitus",
        "COPD", "Asthma", "Hypertensive disorder", "Upper respiratory infection",
        "Obesity", "Heart failure", "Atrial fibrillation", "Stroke",
        "Major depression", "Anxiety disorder", "Coronary artery disease", "COVID-19"
      ),
      stringsAsFactors = FALSE
    )
  }
}

#' Map Synthea encounter class to PCORnet ENC_TYPE
#' @keywords internal
.map_encounter_type <- function(synthea_class, enc_map) {
  result <- enc_map$pcornet_enc_type[match(tolower(synthea_class), tolower(enc_map$synthea_class))]
  ifelse(is.na(result), "OA", result)  # Default to Other Ambulatory
}

#' Map SNOMED code to ICD-10
#' @keywords internal
.map_snomed_to_icd10 <- function(snomed_code, snomed_map) {
  result <- snomed_map$icd10_code[match(as.character(snomed_code), snomed_map$snomed_code)]
  ifelse(is.na(result), NA, result)
}

#' Load and transform Synthea data to PCORnet CDM format
#'
#' Imports Synthea CSV output and converts it to PCORnet CDM format, writing
#' directly to SQL Server databases.
#'
#' @param synthea_dir Path to Synthea output/csv directory containing patients.csv
#'   and other Synthea output files (encounters.csv, conditions.csv, etc.)
#' @param con_cdw DBI connection to CDW SQL Server database
#' @param con_mpi DBI connection to MPI SQL Server database
#' @param overwrite If TRUE, overwrite existing tables (default: TRUE)
#' @param batch_size Number of rows to write at a time (default: 10000)
#' @return List with `$cdw` (CDW connection) and `$mpi` (MPI connection)
#'
#' @examples
#' \dontrun{
#' # Generate data with Synthea first:
#' # java -jar synthea-with-dependencies.jar -p 100 --exporter.csv.export=true
#'
#' # Then import into PCORnet format via create_pcornet_database
#' dbs <- create_pcornet_database(
#'   mode = "synthea",
#'   synthea_dir = "~/synthea/output/csv",
#'   server = "localhost",
#'   uid = "sa",
#'   pwd = "YourPassword123"
#' )
#' DBI::dbListTables(dbs$cdw)
#' }
#'
#' @export
load_synthea_data <- function(synthea_dir, con_cdw, con_mpi,
                              overwrite = TRUE, batch_size = 10000) {

  cat("Loading Synthea data from:", synthea_dir, "\n")

  # Verify directory exists
  if (!dir.exists(synthea_dir)) {
    stop("Synthea directory not found: ", synthea_dir)
  }

  # Load mapping tables
  enc_map <- .load_encounter_type_map()
  snomed_map <- .load_snomed_icd10_map()

  CURRENT_DATETIME <- Sys.time()

  # ============================================================================
  # Load Synthea CSV files
  # ============================================================================

  cat("Reading Synthea CSV files...\n")

  patients_file <- file.path(synthea_dir, "patients.csv")
  if (!file.exists(patients_file)) {
    stop("patients.csv not found in ", synthea_dir)
  }
  patients <- read.csv(patients_file, stringsAsFactors = FALSE)
  cat("  Loaded", nrow(patients), "patients\n")

  encounters <- tryCatch(
    read.csv(file.path(synthea_dir, "encounters.csv"), stringsAsFactors = FALSE),
    error = function(e) { cat("  Warning: encounters.csv not found\n"); data.frame() }
  )

  conditions <- tryCatch(
    read.csv(file.path(synthea_dir, "conditions.csv"), stringsAsFactors = FALSE),
    error = function(e) { cat("  Warning: conditions.csv not found\n"); data.frame() }
  )

  medications <- tryCatch(
    read.csv(file.path(synthea_dir, "medications.csv"), stringsAsFactors = FALSE),
    error = function(e) { cat("  Warning: medications.csv not found\n"); data.frame() }
  )

  procedures <- tryCatch(
    read.csv(file.path(synthea_dir, "procedures.csv"), stringsAsFactors = FALSE),
    error = function(e) { cat("  Warning: procedures.csv not found\n"); data.frame() }
  )

  observations <- tryCatch(
    read.csv(file.path(synthea_dir, "observations.csv"), stringsAsFactors = FALSE),
    error = function(e) { cat("  Warning: observations.csv not found\n"); data.frame() }
  )

  # ============================================================================
  # Create ID mappings (Synthea UUID -> PCORnet format)
  # ============================================================================

  cat("Creating ID mappings...\n")

  # Use same PATID format for both uid and patid (they are equivalent in this schema)
  patid_values <- paste0("PAT", sprintf("%07d", 1:nrow(patients)))
  patient_map <- data.frame(
    synthea_id = patients$Id,
    uid = patid_values,
    patid = patid_values,
    stringsAsFactors = FALSE
  )

  if (nrow(encounters) > 0) {
    encounter_map <- data.frame(
      synthea_id = encounters$Id,
      encounterid = paste0("ENC", sprintf("%010d", 1:nrow(encounters))),
      stringsAsFactors = FALSE
    )
  } else {
    encounter_map <- data.frame(synthea_id = character(), encounterid = character())
  }

  # ============================================================================
  # MPI Database
  # ============================================================================

  cat("Creating MPI database...\n")

  # EnterpriseRecords
  enterprise_records <- data.frame(
    Uid = patient_map$uid,
    First = patients$FIRST,
    M = substr(patients$FIRST, 2, 2),  # Fake middle initial
    Last = patients$LAST,
    DoB = as.Date(patients$BIRTHDATE),
    SSN = sprintf("%09d", sample(100000000:999999999, nrow(patients), replace = FALSE)),
    G = ifelse(patients$GENDER == "M", "M", "F"),
    Addy = patients$ADDRESS,
    Phone = sprintf("(%03d)%03d-%04d",
                    sample(200:999, nrow(patients), replace = TRUE),
                    sample(200:999, nrow(patients), replace = TRUE),
                    sample(1000:9999, nrow(patients), replace = TRUE)),
    City = patients$CITY,
    Zip = patients$ZIP,
    LastEditDTTM = CURRENT_DATETIME,
    isActive = 1,
    stringsAsFactors = FALSE
  )
  enterprise_records$FirstSdx <- substr(enterprise_records$First, 1, 4)
  enterprise_records$LastSdx <- substr(enterprise_records$Last, 1, 4)

  .write_table_batched(con_mpi, "EnterpriseRecords", enterprise_records, overwrite = overwrite, batch_size = batch_size)

  # EnterpriseRecords_Ext
  is_deceased <- !is.na(patients$DEATHDATE) & patients$DEATHDATE != ""

  enterprise_ext <- data.frame(
    Uid = patient_map$uid,
    MPIUpdateDTTM = CURRENT_DATETIME,
    CDM_PATID = patient_map$patid,
    EPIC_PAT_ID = paste0("EPIC", sprintf("%010d", sample(1:99999999, nrow(patients)))),
    ALLSCRIPTS_PERSON_ID = NA,
    MHH_COVID_PERSON = NA,
    MHH_MRN = NA,
    UTP_MRN = NA,
    EpicOnly = "N",
    CDM_SEX = enterprise_records$G,
    CDM_RACE = ifelse(patients$RACE == "white", "05",
              ifelse(patients$RACE == "black", "03",
              ifelse(patients$RACE == "asian", "02", "07"))),
    CDM_HISPANIC = ifelse(patients$ETHNICITY == "hispanic", "Y", "N"),
    Race = patients$RACE,
    Ethnicity = patients$ETHNICITY,
    Language = "English",
    IsDeceased = ifelse(is_deceased, "Y", "N"),
    DeceasedDTTM = as.POSIXct(ifelse(is_deceased, patients$DEATHDATE, NA)),
    State = patients$STATE,
    stringsAsFactors = FALSE
  )

  .write_table_batched(con_mpi, "EnterpriseRecords_Ext", enterprise_ext, overwrite = overwrite, batch_size = batch_size)

  # Mpi mapping table
  mpi <- data.frame(
    Src = "SYNTHEA",
    Lid = patients$Id,
    Uid = patient_map$uid,
    Organization = "SYNTHEA",
    stringsAsFactors = FALSE
  )
  .write_table_batched(con_mpi, "Mpi", mpi, overwrite = overwrite, batch_size = batch_size)

  # MPI_Src
  mpi_src <- data.frame(
    SRC = "SYNTHEA",
    LID_SRC = "SYNTHEA_ID",
    Description = "Synthea Synthetic Patient Generator",
    stringsAsFactors = FALSE
  )
  .write_table_batched(con_mpi, "MPI_Src", mpi_src, overwrite = overwrite, batch_size = batch_size)

  cat("  MPI database created\n")

  # ============================================================================
  # CDW Database - DEMOGRAPHIC
  # ============================================================================

  cat("Creating CDW database...\n")

  demographic <- data.frame(
    PATID = patient_map$patid,
    BIRTH_DATE = as.Date(patients$BIRTHDATE),
    BIRTH_TIME = NA,
    SEX = enterprise_records$G,
    SEXUAL_ORIENTATION = NA,
    GENDER_IDENTITY = NA,
    HISPANIC = enterprise_ext$CDM_HISPANIC,
    RACE = enterprise_ext$CDM_RACE,
    PAT_PREF_LANGUAGE_SPOKEN = "eng",
    firstName = patients$FIRST,
    lastName = patients$LAST,
    addressLine1 = patients$ADDRESS,
    city = patients$CITY,
    state = patients$STATE,
    zipCode = patients$ZIP,
    isDeceased = enterprise_ext$IsDeceased,
    CDW_Source = "SYNTHEA",
    CDW_UpdatedDTTM = CURRENT_DATETIME,
    GPC_FLAG = "Y",
    UID = patient_map$uid,
    RAW_SEX = patients$GENDER,
    RAW_RACE = patients$RACE,
    RAW_HISPANIC = patients$ETHNICITY,
    stringsAsFactors = FALSE
  )

  .write_table_batched(con_cdw, "DEMOGRAPHIC", demographic, overwrite = overwrite, batch_size = batch_size)
  cat("  DEMOGRAPHIC:", nrow(demographic), "records\n")

  # ============================================================================
  # CDW Database - DEATH
  # ============================================================================

  deceased_patients <- patients[is_deceased, ]
  if (nrow(deceased_patients) > 0) {
    death_records <- data.frame(
      PATID = patient_map$patid[is_deceased],
      DEATH_DATE = as.Date(deceased_patients$DEATHDATE),
      DEATH_DATE_IMPUTE = "N",
      DEATH_SOURCE = "L",
      DEATH_MATCH_CONFIDENCE = NA,
      GPC_FLAG = "Y",
      CDW_UpdatedDTTM = CURRENT_DATETIME,
      UID = patient_map$uid[is_deceased],
      MPI_LID = NA,
      MPI_SRC = NA,
      stringsAsFactors = FALSE
    )
    .write_table_batched(con_cdw, "DEATH", death_records, overwrite = overwrite, batch_size = batch_size)
    cat("  DEATH:", nrow(death_records), "records\n")
  }

  # ============================================================================
  # CDW Database - ENCOUNTER
  # ============================================================================

  if (nrow(encounters) > 0) {
    # Map patient IDs
    encounters$PATID <- patient_map$patid[match(encounters$PATIENT, patient_map$synthea_id)]
    encounters$UID <- patient_map$uid[match(encounters$PATIENT, patient_map$synthea_id)]
    encounters$ENCOUNTERID <- encounter_map$encounterid

    encounter_df <- data.frame(
      ENCOUNTERID = encounters$ENCOUNTERID,
      PATID = encounters$PATID,
      ADMIT_DATE = as.Date(substr(encounters$START, 1, 10)),
      ADMIT_TIME = substr(encounters$START, 12, 19),
      DISCHARGE_DATE = as.Date(substr(encounters$STOP, 1, 10)),
      DISCHARGE_TIME = substr(encounters$STOP, 12, 19),
      PROVIDERID = paste0("PROV", sprintf("%06d", sample(1:500, nrow(encounters), replace = TRUE))),
      ENC_TYPE = .map_encounter_type(encounters$ENCOUNTERCLASS, enc_map),
      FACILITY_LOCATION = NA,
      FACILITYID = NA,
      DISCHARGE_DISPOSITION = NA,
      DISCHARGE_STATUS = NA,
      DRG = NA,
      DRG_TYPE = NA,
      ADMITTING_SOURCE = NA,
      RAW_ENC_TYPE = encounters$ENCOUNTERCLASS,
      RAW_DISCHARGE_DISPOSITION = NA,
      RAW_DISCHARGE_STATUS = NA,
      PAYER_TYPE_PRIMARY = NA,
      CDW_Source = "SYNTHEA",
      CDW_UpdatedDTTM = CURRENT_DATETIME,
      GPC_FLAG = "Y",
      UID = encounters$UID,
      MPI_SRC = NA,
      MPI_LID = NA,
      stringsAsFactors = FALSE
    )

    .write_table_batched(con_cdw, "ENCOUNTER", encounter_df, overwrite = overwrite, batch_size = batch_size)
    cat("  ENCOUNTER:", nrow(encounter_df), "records\n")
  }

  # ============================================================================
  # CDW Database - DIAGNOSIS
  # ============================================================================

  if (nrow(conditions) > 0) {
    conditions$PATID <- patient_map$patid[match(conditions$PATIENT, patient_map$synthea_id)]
    conditions$UID <- patient_map$uid[match(conditions$PATIENT, patient_map$synthea_id)]
    conditions$ENCOUNTERID <- encounter_map$encounterid[match(conditions$ENCOUNTER, encounter_map$synthea_id)]

    # Map SNOMED to ICD-10
    conditions$ICD10 <- .map_snomed_to_icd10(conditions$CODE, snomed_map)

    diagnosis_df <- data.frame(
      DIAGNOSISID = paste0("DX", sprintf("%010d", 1:nrow(conditions))),
      PATID = conditions$PATID,
      ENCOUNTERID = conditions$ENCOUNTERID,
      ADMIT_DATE = as.Date(substr(conditions$START, 1, 10)),
      DX = ifelse(!is.na(conditions$ICD10), conditions$ICD10, paste0("SNOMED:", conditions$CODE)),
      DX_DATE = as.Date(substr(conditions$START, 1, 10)),
      DX_TYPE = ifelse(!is.na(conditions$ICD10), "10", "SM"),
      DX_SOURCE = "AD",
      DX_ORIGIN = "OD",
      DX_POA = NA,
      ENC_TYPE = NA,
      PDX = "P",
      RAW_DX = conditions$DESCRIPTION,
      RAW_DX_TYPE = "SNOMED-CT",
      RAW_DX_SOURCE = "Synthea",
      RAW_PDX = NA,
      PROVIDERID = NA,
      CDW_Source = "SYNTHEA",
      CDW_UpdatedDTTM = CURRENT_DATETIME,
      GPC_FLAG = "Y",
      UID = conditions$UID,
      SNOMED_CODE = conditions$CODE,
      stringsAsFactors = FALSE
    )

    .write_table_batched(con_cdw, "DIAGNOSIS", diagnosis_df, overwrite = overwrite, batch_size = batch_size)
    cat("  DIAGNOSIS:", nrow(diagnosis_df), "records\n")
  }

  # ============================================================================
  # CDW Database - PRESCRIBING
  # ============================================================================

  if (nrow(medications) > 0) {
    medications$PATID <- patient_map$patid[match(medications$PATIENT, patient_map$synthea_id)]
    medications$UID <- patient_map$uid[match(medications$PATIENT, patient_map$synthea_id)]
    medications$ENCOUNTERID <- encounter_map$encounterid[match(medications$ENCOUNTER, encounter_map$synthea_id)]

    prescribing_df <- data.frame(
      PRESCRIBINGID = paste0("RX", sprintf("%010d", 1:nrow(medications))),
      PATID = medications$PATID,
      ENCOUNTERID = medications$ENCOUNTERID,
      RX_PROVIDERID = paste0("PROV", sprintf("%06d", sample(1:500, nrow(medications), replace = TRUE))),
      RX_ORDER_DATE = as.Date(substr(medications$START, 1, 10)),
      RX_ORDER_TIME = NA,
      RX_START_DATE = as.Date(substr(medications$START, 1, 10)),
      RX_END_DATE = as.Date(substr(medications$STOP, 1, 10)),
      RX_DAYS_SUPPLY = NA,
      RX_REFILLS = NA,
      RX_QUANTITY = NA,
      RX_DOSE_ORDERED = NA,
      RX_DOSE_ORDERED_UNIT = NA,
      RX_DOSE_FORM = NA,
      RX_FREQUENCY = NA,
      RX_ROUTE = NA,
      RX_BASIS = NA,
      RX_PRN_FLAG = NA,
      RX_DISPENSE_AS_WRITTEN = NA,
      RX_SOURCE = "OD",
      RXNORM_CUI = as.character(medications$CODE),
      RAW_RX_MED_NAME = medications$DESCRIPTION,
      RAW_RX_FREQUENCY = NA,
      RAW_RX_DOSE_ORDERED = NA,
      RAW_RX_DOSE_ORDERED_UNIT = NA,
      RAW_RX_ROUTE = NA,
      RAW_RX_REFILLS = NA,
      RAW_RXNORM_CUI = as.character(medications$CODE),
      RAW_RX_NDC = NA,
      CDW_Source = "SYNTHEA",
      CDW_UpdatedDTTM = CURRENT_DATETIME,
      GPC_FLAG = "Y",
      UID = medications$UID,
      stringsAsFactors = FALSE
    )

    .write_table_batched(con_cdw, "PRESCRIBING", prescribing_df, overwrite = overwrite, batch_size = batch_size)
    cat("  PRESCRIBING:", nrow(prescribing_df), "records\n")
  }

  # ============================================================================
  # CDW Database - PROCEDURES
  # ============================================================================

  if (nrow(procedures) > 0) {
    procedures$PATID <- patient_map$patid[match(procedures$PATIENT, patient_map$synthea_id)]
    procedures$UID <- patient_map$uid[match(procedures$PATIENT, patient_map$synthea_id)]
    procedures$ENCOUNTERID <- encounter_map$encounterid[match(procedures$ENCOUNTER, encounter_map$synthea_id)]

    # Handle both DATE (old format) and START (current Synthea format)
    proc_date_col <- if ("START" %in% names(procedures)) procedures$START else procedures$DATE

    procedures_df <- data.frame(
      PROCEDURESID = paste0("PX", sprintf("%010d", 1:nrow(procedures))),
      PATID = procedures$PATID,
      ENCOUNTERID = procedures$ENCOUNTERID,
      ADMIT_DATE = as.Date(substr(proc_date_col, 1, 10)),
      PX = paste0("SNOMED:", procedures$CODE),
      PX_DATE = as.Date(substr(proc_date_col, 1, 10)),
      PX_TYPE = "SM",
      PX_SOURCE = "OD",
      PPX = "P",
      ENC_TYPE = NA,
      RAW_PX = procedures$DESCRIPTION,
      RAW_PX_TYPE = "SNOMED-CT",
      RAW_PX_NAME = procedures$DESCRIPTION,
      PROVIDERID = NA,
      CDW_Source = "SYNTHEA",
      CDW_UpdatedDTTM = CURRENT_DATETIME,
      GPC_FLAG = "Y",
      UID = procedures$UID,
      SNOMED_CODE = procedures$CODE,
      stringsAsFactors = FALSE
    )

    .write_table_batched(con_cdw, "PROCEDURES", procedures_df, overwrite = overwrite, batch_size = batch_size)
    cat("  PROCEDURES:", nrow(procedures_df), "records\n")
  }

  # ============================================================================
  # CDW Database - LAB_RESULT_CM and VITAL from observations
  # ============================================================================

  if (nrow(observations) > 0) {
    observations$PATID <- patient_map$patid[match(observations$PATIENT, patient_map$synthea_id)]
    observations$UID <- patient_map$uid[match(observations$PATIENT, patient_map$synthea_id)]
    observations$ENCOUNTERID <- encounter_map$encounterid[match(observations$ENCOUNTER, encounter_map$synthea_id)]

    # Split into labs and vitals based on category or code
    vital_codes <- c("8302-2", "29463-7", "39156-5", "8480-6", "8462-4", "8310-5")
    is_vital <- observations$CODE %in% vital_codes

    # Labs
    labs <- observations[!is_vital, ]
    if (nrow(labs) > 0) {
      lab_df <- data.frame(
        LAB_RESULT_CM_ID = paste0("LAB", sprintf("%010d", 1:nrow(labs))),
        PATID = labs$PATID,
        ENCOUNTERID = labs$ENCOUNTERID,
        LAB_LOINC = labs$CODE,
        LAB_PX = NA,
        LAB_PX_TYPE = NA,
        LAB_ORDER_DATE = as.Date(substr(labs$DATE, 1, 10)),
        RESULT_DATE = as.Date(substr(labs$DATE, 1, 10)),
        RESULT_TIME = NA,
        RESULT_NUM = as.numeric(labs$VALUE),
        RESULT_QUAL = NA,
        RESULT_MODIFIER = NA,
        RESULT_UNIT = labs$UNITS,
        NORM_RANGE_LOW = NA,
        NORM_RANGE_HIGH = NA,
        NORM_MODIFIER_LOW = NA,
        NORM_MODIFIER_HIGH = NA,
        ABN_IND = NA,
        SPECIMEN_SOURCE = NA,
        SPECIMEN_DATE = NA,
        PRIORITY = NA,
        RESULT_LOC = NA,
        LAB_LOINC_SOURCE = "OD",
        LAB_RESULT_SOURCE = "OD",
        RAW_LAB_NAME = labs$DESCRIPTION,
        RAW_LAB_CODE = labs$CODE,
        RAW_RESULT = labs$VALUE,
        RAW_UNIT = labs$UNITS,
        CDW_Source = "SYNTHEA",
        CDW_UpdatedDTTM = CURRENT_DATETIME,
        GPC_FLAG = "Y",
        UID = labs$UID,
        stringsAsFactors = FALSE
      )

      .write_table_batched(con_cdw, "LAB_RESULT_CM", lab_df, overwrite = overwrite, batch_size = batch_size)
      cat("  LAB_RESULT_CM:", nrow(lab_df), "records\n")
    }

    # Vitals - aggregate by encounter
    vitals <- observations[is_vital, ]
    if (nrow(vitals) > 0) {
      # Pivot vitals by encounter
      vital_wide <- vitals %>%
        dplyr::select(PATID, UID, ENCOUNTERID, DATE, CODE, VALUE) %>%
        tidyr::pivot_wider(
          id_cols = c(PATID, UID, ENCOUNTERID, DATE),
          names_from = CODE,
          values_from = VALUE,
          values_fn = function(x) as.numeric(x[1])
        )

      vital_df <- data.frame(
        VITALID = paste0("VIT", sprintf("%010d", 1:nrow(vital_wide))),
        PATID = vital_wide$PATID,
        ENCOUNTERID = vital_wide$ENCOUNTERID,
        MEASURE_DATE = as.Date(substr(vital_wide$DATE, 1, 10)),
        MEASURE_TIME = NA,
        VITAL_SOURCE = "HC",
        HT = if ("8302-2" %in% names(vital_wide)) vital_wide$`8302-2` else NA,
        WT = if ("29463-7" %in% names(vital_wide)) vital_wide$`29463-7` else NA,
        ORIGINAL_BMI = if ("39156-5" %in% names(vital_wide)) vital_wide$`39156-5` else NA,
        SYSTOLIC = if ("8480-6" %in% names(vital_wide)) vital_wide$`8480-6` else NA,
        DIASTOLIC = if ("8462-4" %in% names(vital_wide)) vital_wide$`8462-4` else NA,
        BP_POSITION = NA,
        SMOKING = NA,
        TOBACCO = NA,
        TOBACCO_TYPE = NA,
        RAW_SYSTOLIC = NA,
        RAW_DIASTOLIC = NA,
        RAW_BP_POSITION = NA,
        RAW_SMOKING = NA,
        RAW_TOBACCO = NA,
        RAW_TOBACCO_TYPE = NA,
        CDW_Source = "SYNTHEA",
        CDW_UpdatedDTTM = CURRENT_DATETIME,
        GPC_FLAG = "Y",
        UID = vital_wide$UID,
        stringsAsFactors = FALSE
      )

      .write_table_batched(con_cdw, "VITAL", vital_df, overwrite = overwrite, batch_size = batch_size)
      cat("  VITAL:", nrow(vital_df), "records\n")
    }
  }

  # ============================================================================
  # CDW Database - PROVIDER
  # ============================================================================

  providers <- data.frame(
    PROVIDERID = paste0("PROV", sprintf("%06d", 1:500)),
    ProviderName = paste("Dr.", paste0(sample(LETTERS, 500, replace = TRUE),
                                       sapply(1:500, function(x) paste(sample(letters, 5), collapse = "")))),
    PROVIDER_SPECIALTY_PRIMARY = sample(c("Internal Medicine", "Family Medicine", "Cardiology",
                                         "Endocrinology", "Emergency Medicine"), 500, replace = TRUE),
    PROVIDER_NPI = as.integer(sprintf("1%09d", sample(100000000:999999999, 500))),
    GPC_FLAG = "Y",
    stringsAsFactors = FALSE
  )
  .write_table_batched(con_cdw, "PROVIDER", providers, overwrite = overwrite, batch_size = batch_size)

  # ============================================================================
  # Summary
  # ============================================================================

  cat("\n=== Synthea to PCORnet Conversion Complete ===\n")
  cat("CDW Tables:\n")
  for (tbl in DBI::dbListTables(con_cdw)) {
    count <- DBI::dbGetQuery(con_cdw, paste("SELECT COUNT(*) FROM", tbl))[[1]]
    cat(sprintf("  %s: %d records\n", tbl, count))
  }

  cat("\nData written to SQL Server.\n")

  list(cdw = con_cdw, mpi = con_mpi)
}
