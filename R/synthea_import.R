# Synthea to PCORnet CDM Transformation Functions
# Converts Synthea CSV output to PCORnet CDM format in SQL Server

#' Tables that load_synthea_data() can populate, for use with its `tables`
#' argument. PROVIDERID values are stable across runs, so tables carrying a
#' provider FK stay valid whether or not PROVIDER is reloaded with them.
#' @export
SYNTHEA_LOADABLE_TABLES <- c(
  "EnterpriseRecords", "EnterpriseRecords_Ext", "Mpi", "MPI_Src",
  "DEMOGRAPHIC", "DEATH", "ENCOUNTER", "CONDITION", "DIAGNOSIS",
  "PRESCRIBING", "PROCEDURES", "LAB_RESULT_CM", "VITAL", "IMMUNIZATION",
  "PROVIDER"
)

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
#' @param tables Character vector of tables to load; NULL (default) loads all.
#'   See `SYNTHEA_LOADABLE_TABLES`. Source CSVs are read only when a requested
#'   table needs them, so a single-table load skips the multi-gigabyte files.
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
                              overwrite = TRUE, batch_size = 10000,
                              tables = NULL) {

  cat("Loading Synthea data from:", synthea_dir, "\n")

  # Verify directory exists
  if (!dir.exists(synthea_dir)) {
    stop("Synthea directory not found: ", synthea_dir)
  }

  # `tables = NULL` loads everything; naming tables loads just those, which
  # keeps a single-table refresh from rewriting the multi-million-row tables.
  want <- function(...) is.null(tables) || any(c(...) %in% tables)
  if (!is.null(tables)) {
    unknown <- setdiff(tables, SYNTHEA_LOADABLE_TABLES)
    if (length(unknown) > 0) {
      stop("Unknown table(s): ", paste(unknown, collapse = ", "),
           "\nLoadable tables: ", paste(SYNTHEA_LOADABLE_TABLES, collapse = ", "))
    }
    cat("Loading only:", paste(tables, collapse = ", "), "\n")
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

  # Each source file is read only when a requested table needs it; several of
  # them run to gigabytes.
  read_synthea_csv <- function(filename, needed, ...) {
    if (!needed) return(data.frame())
    tryCatch(
      read.csv(file.path(synthea_dir, filename), stringsAsFactors = FALSE, ...),
      error = function(e) { cat("  Warning:", filename, "not found\n"); data.frame() }
    )
  }

  conditions <- read_synthea_csv("conditions.csv", want("CONDITION"))
  medications <- read_synthea_csv("medications.csv", want("PRESCRIBING"))
  procedures <- read_synthea_csv("procedures.csv", want("PROCEDURES"))
  observations <- read_synthea_csv("observations.csv", want("LAB_RESULT_CM", "VITAL"))

  # CODE holds CVX codes, which are zero-padded ("08", "03"). Reading them as
  # character keeps the padding that numeric coercion would strip.
  immunizations <- read_synthea_csv("immunizations.csv", want("IMMUNIZATION"),
                                    colClasses = c(CODE = "character"))

  # claims.csv is the billing-context source for DIAGNOSIS. Problem-list
  # entries come from conditions.csv and land in CONDITION instead.
  claims <- read_synthea_csv("claims.csv", want("DIAGNOSIS"))

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

  # Synthetic provider pool that the PROVIDER table is built from. Tables that
  # carry a provider FK draw their IDs from here so the references resolve.
  provider_ids <- paste0("PROV", sprintf("%06d", 1:500))

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

  if (want("EnterpriseRecords")) .write_table_batched(con_mpi, "EnterpriseRecords", enterprise_records, overwrite = overwrite, batch_size = batch_size)

  # EnterpriseRecords_Ext
  is_deceased <- !is.na(patients$DEATHDATE) & patients$DEATHDATE != ""

  enterprise_ext <- data.frame(
    Uid = patient_map$uid,
    MPIUpdateDTTM = CURRENT_DATETIME,
    CDM_PATID = patient_map$patid,
    EPIC_PAT_ID = paste0("EPIC", sprintf("%010d", sample(1:99999999, nrow(patients)))),
    ALLSCRIPTS_PERSON_ID = NA_character_,
    MHH_COVID_PERSON = NA_character_,
    MHH_MRN = NA_character_,
    UTP_MRN = NA_character_,
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

  if (want("EnterpriseRecords_Ext")) .write_table_batched(con_mpi, "EnterpriseRecords_Ext", enterprise_ext, overwrite = overwrite, batch_size = batch_size)

  # Mpi mapping table
  mpi <- data.frame(
    Src = "SYNTHEA",
    Lid = patients$Id,
    Uid = patient_map$uid,
    Organization = "SYNTHEA",
    stringsAsFactors = FALSE
  )
  if (want("Mpi")) .write_table_batched(con_mpi, "Mpi", mpi, overwrite = overwrite, batch_size = batch_size)

  # MPI_Src
  mpi_src <- data.frame(
    SRC = "SYNTHEA",
    LID_SRC = "SYNTHEA_ID",
    Description = "Synthea Synthetic Patient Generator",
    stringsAsFactors = FALSE
  )
  if (want("MPI_Src")) .write_table_batched(con_mpi, "MPI_Src", mpi_src, overwrite = overwrite, batch_size = batch_size)

  if (want("EnterpriseRecords", "EnterpriseRecords_Ext", "Mpi", "MPI_Src")) {
    cat("  MPI database created\n")
  }

  # ============================================================================
  # CDW Database - DEMOGRAPHIC
  # ============================================================================

  cat("Creating CDW database...\n")

  demographic <- data.frame(
    PATID = patient_map$patid,
    BIRTH_DATE = as.Date(patients$BIRTHDATE),
    BIRTH_TIME = NA_character_,
    SEX = enterprise_records$G,
    SEXUAL_ORIENTATION = NA_character_,
    GENDER_IDENTITY = NA_character_,
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

  if (want("DEMOGRAPHIC")) {
    .write_table_batched(con_cdw, "DEMOGRAPHIC", demographic, overwrite = overwrite, batch_size = batch_size)
    cat("  DEMOGRAPHIC:", nrow(demographic), "records\n")
  }

  # ============================================================================
  # CDW Database - DEATH
  # ============================================================================

  deceased_patients <- patients[is_deceased, ]
  if (want("DEATH") && nrow(deceased_patients) > 0) {
    death_records <- data.frame(
      PATID = patient_map$patid[is_deceased],
      DEATH_DATE = as.Date(deceased_patients$DEATHDATE),
      DEATH_DATE_IMPUTE = "N",
      DEATH_SOURCE = "L",
      DEATH_MATCH_CONFIDENCE = NA_character_,
      GPC_FLAG = "Y",
      CDW_UpdatedDTTM = CURRENT_DATETIME,
      UID = patient_map$uid[is_deceased],
      MPI_LID = NA_character_,
      MPI_SRC = NA_character_,
      stringsAsFactors = FALSE
    )
    .write_table_batched(con_cdw, "DEATH", death_records, overwrite = overwrite, batch_size = batch_size)
    cat("  DEATH:", nrow(death_records), "records\n")
  }

  # ============================================================================
  # CDW Database - ENCOUNTER
  # ============================================================================

  if (want("ENCOUNTER") && nrow(encounters) > 0) {
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
      FACILITY_LOCATION = NA_character_,
      FACILITYID = NA_character_,
      DISCHARGE_DISPOSITION = NA_character_,
      DISCHARGE_STATUS = NA_character_,
      DRG = NA_character_,
      DRG_TYPE = NA_character_,
      ADMITTING_SOURCE = NA_character_,
      RAW_ENC_TYPE = encounters$ENCOUNTERCLASS,
      RAW_DISCHARGE_DISPOSITION = NA_character_,
      RAW_DISCHARGE_STATUS = NA_character_,
      PAYER_TYPE_PRIMARY = NA_character_,
      CDW_Source = "SYNTHEA",
      CDW_UpdatedDTTM = CURRENT_DATETIME,
      GPC_FLAG = "Y",
      UID = encounters$UID,
      MPI_SRC = NA_character_,
      MPI_LID = NA_character_,
      stringsAsFactors = FALSE
    )

    .write_table_batched(con_cdw, "ENCOUNTER", encounter_df, overwrite = overwrite, batch_size = batch_size)
    cat("  ENCOUNTER:", nrow(encounter_df), "records\n")
  }

  # ============================================================================
  # CDW Database - CONDITION  (problem list, from conditions.csv)
  # ============================================================================
  # PCORnet CDM v7.0, CONDITION guidance: "These records should NOT be
  # duplicated in the DIAGNOSIS table." Synthea's conditions.csv is problem-list
  # shaped (SNOMED-coded, with onset/abatement via START/STOP), so it maps here
  # at full fidelity - CONDITION_TYPE = 'SM' is a first-class value, so no
  # terminology crosswalk is required.

  if (nrow(conditions) > 0) {
    cond_patid <- patient_map$patid[match(conditions$PATIENT, patient_map$synthea_id)]
    cond_uid   <- patient_map$uid[match(conditions$PATIENT, patient_map$synthea_id)]
    cond_encid <- encounter_map$encounterid[match(conditions$ENCOUNTER, encounter_map$synthea_id)]

    cond_stop <- if ("STOP" %in% names(conditions)) trimws(as.character(conditions$STOP)) else ""
    has_stop  <- !is.na(cond_stop) & nzchar(cond_stop)

    cond_system <- if ("SYSTEM" %in% names(conditions)) {
      as.character(conditions$SYSTEM)
    } else {
      "http://snomed.info/sct"
    }

    condition_df <- data.frame(
      CONDITIONID  = paste0("COND", sprintf("%010d", seq_len(nrow(conditions)))),
      PATID        = cond_patid,
      ENCOUNTERID  = cond_encid,
      REPORT_DATE  = as.Date(substr(conditions$START, 1, 10)),
      RESOLVE_DATE = as.Date(ifelse(has_stop, substr(cond_stop, 1, 10), NA_character_)),
      # Spec: ONSET_DATE "should only be provided where it exists in the source
      # data. It is not calculated." Synthea's START is a recording date - 78.7%
      # of rows share the date of their linked encounter - so it maps to
      # REPORT_DATE. Deriving ONSET_DATE from it would fabricate the concept.
      ONSET_DATE   = as.Date(NA),
      CONDITION_STATUS = ifelse(has_stop, "RS", "AC"),
      # Bare SNOMED code. The field is Text(18) and the longest observed code is
      # 17 chars, so a "SNOMED:" prefix would overflow it.
      CONDITION        = as.character(conditions$CODE),
      CONDITION_TYPE   = "SM",
      CONDITION_SOURCE = "HC",
      RAW_CONDITION_STATUS = NA_character_,
      RAW_CONDITION        = conditions$DESCRIPTION,
      RAW_CONDITION_TYPE   = cond_system,
      RAW_CONDITION_SOURCE = "Synthea conditions.csv",
      CDW_Source      = "SYNTHEA",
      CDW_UpdatedDTTM = CURRENT_DATETIME,
      GPC_FLAG        = "Y",
      UID             = cond_uid,
      stringsAsFactors = FALSE
    )

    .write_table_batched(con_cdw, "CONDITION", condition_df, overwrite = overwrite, batch_size = batch_size)
    cat("  CONDITION:", nrow(condition_df), "records\n")
  }

  # ============================================================================
  # CDW Database - DIAGNOSIS  (billing context, from claims.csv)
  # ============================================================================
  # PCORnet CDM v7.0, DIAGNOSIS guidance: this table captures diagnoses "with
  # the exception of problem list entries... Diagnoses from problem lists will
  # be captured in the CONDITION table." Synthea's claims.csv is the billing
  # source: each claim carries up to 8 diagnosis references, where DIAGNOSIS1 is
  # principal and DIAGNOSIS2-8 are secondary, which yields a real PDX value.

  if (nrow(claims) > 0) {
    dx_cols <- intersect(paste0("DIAGNOSIS", 1:8), names(claims))

    # Pivot the DIAGNOSIS1..8 columns to one row per diagnosis, keeping the
    # column position so principal vs secondary can be derived.
    dx_long <- do.call(rbind, lapply(seq_along(dx_cols), function(i) {
      v <- trimws(as.character(claims[[dx_cols[i]]]))
      keep <- !is.na(v) & nzchar(v)
      if (!any(keep)) return(NULL)
      data.frame(
        PATIENT   = claims$PATIENTID[keep],
        ENCOUNTER = claims$APPOINTMENTID[keep],
        SVCDATE   = claims$SERVICEDATE[keep],
        CODE      = v[keep],
        SEQ       = i,
        stringsAsFactors = FALSE
      )
    }))

    if (!is.null(dx_long) && nrow(dx_long) > 0) {
      dx_icd10 <- .map_snomed_to_icd10(dx_long$CODE, snomed_map)
      dx_mapped <- !is.na(dx_icd10)
      dx_date <- as.Date(substr(dx_long$SVCDATE, 1, 10))

      diagnosis_df <- data.frame(
        DIAGNOSISID = paste0("DX", sprintf("%010d", seq_len(nrow(dx_long)))),
        PATID       = patient_map$patid[match(dx_long$PATIENT, patient_map$synthea_id)],
        ENCOUNTERID = encounter_map$encounterid[match(dx_long$ENCOUNTER, encounter_map$synthea_id)],
        ADMIT_DATE  = dx_date,
        # Bare code, never prefixed - DX is a fixed-width coded field.
        DX          = ifelse(dx_mapped, dx_icd10, dx_long$CODE),
        DX_DATE     = dx_date,
        DX_TYPE     = ifelse(dx_mapped, "10", "SM"),
        DX_SOURCE   = "FI",   # spec: ambulatory encounters expected to be Final
        DX_ORIGIN   = "BI",   # provider-side billing, not payer claim fulfillment
        DX_POA      = "NI",   # Synthea models no present-on-admission data
        ENC_TYPE    = NA_character_,
        PDX         = ifelse(dx_long$SEQ == 1, "P", "S"),
        RAW_DX        = dx_long$CODE,
        RAW_DX_TYPE   = "SNOMED-CT",
        RAW_DX_SOURCE = "Synthea claims.csv",
        RAW_PDX       = as.character(dx_long$SEQ),
        PROVIDERID    = NA_character_,
        CDW_Source      = "SYNTHEA",
        CDW_UpdatedDTTM = CURRENT_DATETIME,
        GPC_FLAG        = "Y",
        UID         = patient_map$uid[match(dx_long$PATIENT, patient_map$synthea_id)],
        SNOMED_CODE = dx_long$CODE,
        stringsAsFactors = FALSE
      )

      .write_table_batched(con_cdw, "DIAGNOSIS", diagnosis_df, overwrite = overwrite, batch_size = batch_size)
      cat("  DIAGNOSIS:", nrow(diagnosis_df), "records\n")
    }
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
      RX_ORDER_TIME = NA_character_,
      RX_START_DATE = as.Date(substr(medications$START, 1, 10)),
      RX_END_DATE = as.Date(substr(medications$STOP, 1, 10)),
      RX_DAYS_SUPPLY = NA_real_,
      RX_REFILLS = NA_real_,
      RX_QUANTITY = NA_real_,
      RX_DOSE_ORDERED = NA_real_,
      RX_DOSE_ORDERED_UNIT = NA_character_,
      RX_DOSE_FORM = NA_character_,
      RX_FREQUENCY = NA_character_,
      RX_ROUTE = NA_character_,
      RX_BASIS = NA_character_,
      RX_PRN_FLAG = NA_character_,
      RX_DISPENSE_AS_WRITTEN = NA_character_,
      RX_SOURCE = "OD",
      RXNORM_CUI = as.character(medications$CODE),
      RAW_RX_MED_NAME = medications$DESCRIPTION,
      RAW_RX_FREQUENCY = NA_character_,
      RAW_RX_DOSE_ORDERED = NA_character_,
      RAW_RX_DOSE_ORDERED_UNIT = NA_character_,
      RAW_RX_ROUTE = NA_character_,
      RAW_RX_REFILLS = NA_character_,
      RAW_RXNORM_CUI = as.character(medications$CODE),
      RAW_RX_NDC = NA_character_,
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
      ENC_TYPE = NA_character_,
      RAW_PX = procedures$DESCRIPTION,
      RAW_PX_TYPE = "SNOMED-CT",
      RAW_PX_NAME = procedures$DESCRIPTION,
      PROVIDERID = NA_character_,
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
    labs <- if (want("LAB_RESULT_CM")) observations[!is_vital, ] else observations[0, ]
    if (nrow(labs) > 0) {
      lab_df <- data.frame(
        LAB_RESULT_CM_ID = paste0("LAB", sprintf("%010d", 1:nrow(labs))),
        PATID = labs$PATID,
        ENCOUNTERID = labs$ENCOUNTERID,
        LAB_LOINC = labs$CODE,
        LAB_PX = NA_character_,
        LAB_PX_TYPE = NA_character_,
        LAB_ORDER_DATE = as.Date(substr(labs$DATE, 1, 10)),
        RESULT_DATE = as.Date(substr(labs$DATE, 1, 10)),
        RESULT_TIME = NA_character_,
        RESULT_NUM = as.numeric(labs$VALUE),
        RESULT_QUAL = NA_character_,
        RESULT_MODIFIER = NA_character_,
        RESULT_UNIT = labs$UNITS,
        NORM_RANGE_LOW = NA_character_,
        NORM_RANGE_HIGH = NA_character_,
        NORM_MODIFIER_LOW = NA_character_,
        NORM_MODIFIER_HIGH = NA_character_,
        ABN_IND = NA_character_,
        SPECIMEN_SOURCE = NA_character_,
        SPECIMEN_DATE = as.Date(NA),
        PRIORITY = NA_character_,
        RESULT_LOC = NA_character_,
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
    vitals <- if (want("VITAL")) observations[is_vital, ] else observations[0, ]
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
        MEASURE_TIME = NA_character_,
        VITAL_SOURCE = "HC",
        HT = if ("8302-2" %in% names(vital_wide)) vital_wide$`8302-2` else NA,
        WT = if ("29463-7" %in% names(vital_wide)) vital_wide$`29463-7` else NA,
        ORIGINAL_BMI = if ("39156-5" %in% names(vital_wide)) vital_wide$`39156-5` else NA,
        SYSTOLIC = if ("8480-6" %in% names(vital_wide)) vital_wide$`8480-6` else NA,
        DIASTOLIC = if ("8462-4" %in% names(vital_wide)) vital_wide$`8462-4` else NA,
        BP_POSITION = NA_character_,
        SMOKING = NA_character_,
        TOBACCO = NA_character_,
        TOBACCO_TYPE = NA_character_,
        RAW_SYSTOLIC = NA_character_,
        RAW_DIASTOLIC = NA_character_,
        RAW_BP_POSITION = NA_character_,
        RAW_SMOKING = NA_character_,
        RAW_TOBACCO = NA_character_,
        RAW_TOBACCO_TYPE = NA_character_,
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
  # CDW Database - IMMUNIZATION
  # ============================================================================

  if (nrow(immunizations) > 0) {
    immunizations$PATID <- patient_map$patid[match(immunizations$PATIENT, patient_map$synthea_id)]
    immunizations$UID <- patient_map$uid[match(immunizations$PATIENT, patient_map$synthea_id)]
    immunizations$ENCOUNTERID <- encounter_map$encounterid[match(immunizations$ENCOUNTER, encounter_map$synthea_id)]

    # PATID and ENCOUNTERID are NOT NULL in the CDM, so drop any dose whose
    # patient or encounter did not resolve rather than writing a broken FK.
    unresolved <- is.na(immunizations$PATID) | is.na(immunizations$ENCOUNTERID)
    if (any(unresolved)) {
      cat("  IMMUNIZATION: dropping", sum(unresolved), "doses with unresolved PATID/ENCOUNTERID\n")
      immunizations <- immunizations[!unresolved, ]
    }
  }

  if (nrow(immunizations) > 0) {
    # Synthea records the administering clinician on the encounter, not on the
    # dose. Map each Synthea provider onto the synthetic pool so the same
    # clinician always resolves to the same PROVIDERID.
    if (nrow(encounters) > 0) {
      synthea_provider <- encounters$PROVIDER[match(immunizations$ENCOUNTER, encounters$Id)]
      provider_slot <- match(synthea_provider, unique(encounters$PROVIDER))
      vx_providerid <- provider_ids[((provider_slot - 1) %% length(provider_ids)) + 1]
    } else {
      vx_providerid <- NA_character_
    }

    vx_admin_date <- as.Date(substr(immunizations$DATE, 1, 10))

    immunization_df <- data.frame(
      IMMUNIZATIONID = paste0("IMM", sprintf("%010d", 1:nrow(immunizations))),
      PATID = immunizations$PATID,
      ENCOUNTERID = immunizations$ENCOUNTERID,
      PROCEDURESID = NA_character_,
      VX_PROVIDERID = vx_providerid,
      VX_RECORD_DATE = vx_admin_date,
      VX_ADMIN_DATE = vx_admin_date,
      # Synthea emits CVX codes and only records doses that were given.
      VX_CODE = immunizations$CODE,
      VX_CODE_TYPE = "CX",
      VX_STATUS = "CP",
      VX_STATUS_REASON = NA_character_,
      VX_SOURCE = "OD",
      # Not modelled by Synthea.
      VX_DOSE = NA_real_,
      VX_DOSE_UNIT = NA_character_,
      VX_ROUTE = NA_character_,
      VX_BODY_SITE = NA_character_,
      VX_LOT_NUM = NA_character_,
      VX_MANUFACTURER = NA_character_,
      VX_EXP_DATE = as.Date(NA),
      RAW_VX_CODE = immunizations$CODE,
      RAW_VX_CODE_TYPE = "CVX",
      RAW_VX_NAME = immunizations$DESCRIPTION,
      RAW_VX_STATUS = "completed",
      RAW_VX_STATUS_REASON = NA_character_,
      RAW_VX_DOSE = NA_character_,
      RAW_VX_DOSE_UNIT = NA_character_,
      RAW_VX_ROUTE = NA_character_,
      RAW_VX_BODY_SITE = NA_character_,
      RAW_VX_MANUFACTURER = NA_character_,
      RAW_ENCOUNTERID = immunizations$ENCOUNTER,
      CDW_Source = "SYNTHEA",
      CDW_UpdatedDTTM = CURRENT_DATETIME,
      GPC_FLAG = "Y",
      UID = immunizations$UID,
      stringsAsFactors = FALSE
    )

    .write_table_batched(con_cdw, "IMMUNIZATION", immunization_df, overwrite = overwrite, batch_size = batch_size)
    cat("  IMMUNIZATION:", nrow(immunization_df), "records\n")
  }

  # ============================================================================
  # CDW Database - PROVIDER
  # ============================================================================

  providers <- data.frame(
    PROVIDERID = provider_ids,
    ProviderName = paste("Dr.", paste0(sample(LETTERS, 500, replace = TRUE),
                                       sapply(1:500, function(x) paste(sample(letters, 5), collapse = "")))),
    PROVIDER_SPECIALTY_PRIMARY = sample(c("Internal Medicine", "Family Medicine", "Cardiology",
                                         "Endocrinology", "Emergency Medicine"), 500, replace = TRUE),
    PROVIDER_NPI = as.integer(sprintf("1%09d", sample(100000000:999999999, 500))),
    GPC_FLAG = "Y",
    stringsAsFactors = FALSE
  )
  if (want("PROVIDER")) .write_table_batched(con_cdw, "PROVIDER", providers, overwrite = overwrite, batch_size = batch_size)

  # ============================================================================
  # Summary
  # ============================================================================

  cat("\n=== Synthea to PCORnet Conversion Complete ===\n")
  cat("CDW Tables:\n")
  # dbListTables() also returns system objects outside dbo, which are not
  # queryable unqualified. Restrict to user tables in the dbo schema.
  user_tables <- DBI::dbGetQuery(con_cdw, paste(
    "SELECT t.name FROM sys.tables t",
    "JOIN sys.schemas s ON s.schema_id = t.schema_id",
    "WHERE s.name = 'dbo' ORDER BY t.name"))$name
  for (tbl in user_tables) {
    count <- DBI::dbGetQuery(con_cdw, sprintf("SELECT COUNT(*) FROM dbo.[%s]", tbl))[[1]]
    cat(sprintf("  %s: %d records\n", tbl, count))
  }

  cat("\nData written to SQL Server.\n")

  list(cdw = con_cdw, mpi = con_mpi)
}
