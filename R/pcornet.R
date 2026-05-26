#' @title PCORnet CDM Synthetic Database Generator
#' @description Function-based API for generating synthetic PCORnet CDM and MPI databases
#'
#' @import DBI
#' @import odbc
#' @import dplyr
#' @import lubridate
#' @keywords internal
"_PACKAGE"

# =============================================================================
# MAIN API FUNCTIONS
# =============================================================================

#' Default MPI source systems
#' @export
DEFAULT_SOURCES <- list(
  EPIC = list(id_field = "EPIC_PAT_ID", description = "Epic EHR System", null_rate = 0.2),
  ALLSCRIPTS = list(id_field = "ALLSCRIPTS_PERSON_ID", description = "Allscripts EHR", null_rate = 0.7),
  MHH_COVID = list(id_field = "MHH_COVID_PERSON", description = "MHH COVID Registry", null_rate = 0.8),
  UTP = list(id_field = "UTP_MRN", description = "UTP Medical Records", null_rate = 0.7)
)

#' Create PCORnet CDM Synthetic Database
#'
#' Generates synthetic patient data following PCORnet CDM schema with optional
#' clinical coherence using patient profiles. Writes directly to SQL Server.
#'
#' @param n_patients Number of synthetic patients to generate (default: 100)
#' @param mode Generation mode: "enhanced" (clinical profiles), "random", or "synthea"
#' @param current_date Reference date for data generation (default: Sys.Date())
#' @param seed Random seed for reproducibility (default: 42)
#' @param server SQL Server hostname (e.g., "localhost" or "server.database.windows.net")
#' @param cdw_database Database name for the Clinical Data Warehouse (default: "PCORnet_CDW")
#' @param mpi_database Database name for the Master Patient Index (default: "MPI")
#' @param uid Username for SQL Server authentication
#' @param pwd Password for SQL Server authentication
#' @param driver ODBC driver name (default: "ODBC Driver 18 for SQL Server")
#' @param port SQL Server port (default: 1433)
#' @param schema Target schema for tables (default: "dbo")
#' @param trusted_connection Use Windows integrated authentication instead of uid/pwd
#'   (default: FALSE)
#' @param connection_string Optional full ODBC connection string for CDW database.
#'   If provided, overrides server/cdw_database/uid/pwd/driver/port parameters.
#' @param mpi_connection_string Optional full ODBC connection string for MPI database.
#'   If provided, overrides server/mpi_database/uid/pwd/driver/port parameters.
#' @param overwrite If TRUE, overwrite existing tables (default: TRUE)
#' @param batch_size Number of rows to write at a time for large tables (default: 10000)
#' @param synthea_dir Path to Synthea CSV output (required when mode = "synthea")
#' @param profile_weights Named list to override default profile distribution
#' @param sources Named list of source systems for MPI. Each source should have
#'   id_field, description, and null_rate. Use DEFAULT_SOURCES as a template.
#'
#' @return List with:
#'   \item{cdw}{DBI connection to Clinical Data Warehouse on SQL Server}
#'   \item{mpi}{DBI connection to Master Patient Index on SQL Server}
#'   \item{summary}{Generation statistics}
#'
#' @examples
#' \dontrun{
#' # Basic usage with SQL Server authentication
#' dbs <- create_pcornet_database(
#'   server = "localhost",
#'   uid = "sa",
#'   pwd = "YourPassword123"
#' )
#'
#' # Custom patient count with Windows authentication
#' dbs <- create_pcornet_database(
#'   n_patients = 5000,
#'   server = "localhost",
#'   trusted_connection = TRUE
#' )
#'
#' # Random mode (no clinical coherence)
#' dbs <- create_pcornet_database(
#'   mode = "random",
#'   server = "localhost",
#'   uid = "sa",
#'   pwd = "YourPassword123"
#' )
#'
#' # Custom profile distribution
#' dbs <- create_pcornet_database(
#'   profile_weights = list(diabetic = 0.3, healthy = 0.7),
#'   server = "localhost",
#'   uid = "sa",
#'   pwd = "YourPassword123"
#' )
#'
#' # Custom source systems
#' dbs <- create_pcornet_database(
#'   sources = list(
#'     EPIC = list(id_field = "EPIC_PAT_ID", description = "Epic EHR", null_rate = 0.1),
#'     CERNER = list(id_field = "CERNER_ID", description = "Cerner EHR", null_rate = 0.3)
#'   ),
#'   server = "localhost",
#'   uid = "sa",
#'   pwd = "YourPassword123"
#' )
#' }
#'
#' @export
create_pcornet_database <- function(
    n_patients = 100,
    mode = c("enhanced", "random", "synthea"),
    current_date = Sys.Date(),
    seed = 42,
    server = "localhost",
    cdw_database = "PCORnet_CDW",
    mpi_database = "MPI",
    uid = NULL,
    pwd = NULL,
    driver = "ODBC Driver 18 for SQL Server",
    port = 1433,
    schema = "dbo",
    trusted_connection = FALSE,
    connection_string = NULL,
    mpi_connection_string = NULL,
    overwrite = TRUE,
    batch_size = 10000,
    synthea_dir = NULL,
    profile_weights = NULL,
    sources = NULL
) {
  # Validate arguments
  mode <- match.arg(mode)

  if (n_patients < 1) {
    stop("n_patients must be at least 1")
  }

  if (mode == "synthea") {
    if (is.null(synthea_dir)) {
      stop("synthea_dir is required when mode = 'synthea'")
    }
    if (!dir.exists(synthea_dir)) {
      stop("synthea_dir does not exist: ", synthea_dir)
    }
  }

  # Set seed for reproducibility
  set.seed(seed)

  # Use default sources if not provided
  if (is.null(sources)) {
    sources <- DEFAULT_SOURCES
  }

  # Create SQL Server connections
  con_cdw <- .connect_sql_server(
    server = server, database = cdw_database, uid = uid, pwd = pwd,
    driver = driver, port = port, trusted_connection = trusted_connection,
    connection_string = connection_string
  )
  cat("Connected to CDW database:", cdw_database, "on", server, "\n")

  con_mpi <- .connect_sql_server(
    server = server, database = mpi_database, uid = uid, pwd = pwd,
    driver = driver, port = port, trusted_connection = trusted_connection,
    connection_string = mpi_connection_string
  )
  cat("Connected to MPI database:", mpi_database, "on", server, "\n\n")

  # Dispatch to appropriate generator
  if (mode == "synthea") {
    result <- .generate_from_synthea(synthea_dir, con_cdw, con_mpi, overwrite, batch_size)
  } else if (mode == "enhanced") {
    result <- .generate_enhanced(n_patients, current_date, profile_weights, con_cdw, con_mpi, overwrite, batch_size, sources)
  } else {
    result <- .generate_random(n_patients, current_date, con_cdw, con_mpi, overwrite, batch_size, sources)
  }

  # Add metadata
  result$summary$mode <- mode
  result$summary$seed <- seed
  result$summary$generated_at <- Sys.time()

  return(result)
}

#' Load Existing PCORnet Databases
#'
#' Connects to existing PCORnet CDM and MPI databases on SQL Server.
#'
#' @param server SQL Server hostname
#' @param cdw_database Database name for the Clinical Data Warehouse (default: "PCORnet_CDW")
#' @param mpi_database Database name for the Master Patient Index (default: "MPI")
#' @param uid Username for SQL Server authentication
#' @param pwd Password for SQL Server authentication
#' @param driver ODBC driver name (default: "ODBC Driver 18 for SQL Server")
#' @param port SQL Server port (default: 1433)
#' @param trusted_connection Use Windows integrated authentication (default: FALSE)
#' @param connection_string Optional full ODBC connection string for CDW database
#' @param mpi_connection_string Optional full ODBC connection string for MPI database
#'
#' @return List with cdw and mpi connections
#'
#' @examples
#' \dontrun{
#' dbs <- load_pcornet_database(
#'   server = "localhost",
#'   uid = "sa",
#'   pwd = "YourPassword123"
#' )
#' dbListTables(dbs$cdw)
#' }
#'
#' @export
load_pcornet_database <- function(
    server = "localhost",
    cdw_database = "PCORnet_CDW",
    mpi_database = "MPI",
    uid = NULL,
    pwd = NULL,
    driver = "ODBC Driver 18 for SQL Server",
    port = 1433,
    trusted_connection = FALSE,
    connection_string = NULL,
    mpi_connection_string = NULL
) {
  con_cdw <- .connect_sql_server(
    server = server, database = cdw_database, uid = uid, pwd = pwd,
    driver = driver, port = port, trusted_connection = trusted_connection,
    connection_string = connection_string
  )

  con_mpi <- .connect_sql_server(
    server = server, database = mpi_database, uid = uid, pwd = pwd,
    driver = driver, port = port, trusted_connection = trusted_connection,
    connection_string = mpi_connection_string
  )

  cat("Connected to databases:\n")
  cat("  CDW:", cdw_database, "on", server, "\n")
  cat("  MPI:", mpi_database, "on", server, "\n")

  list(cdw = con_cdw, mpi = con_mpi)
}

#' Get Database Summary
#'
#' Returns summary statistics for PCORnet CDM and MPI databases.
#'
#' @param con_cdw Connection to CDW database
#' @param con_mpi Connection to MPI database (optional)
#'
#' @return List with table counts and row counts
#'
#' @export
get_database_summary <- function(con_cdw, con_mpi = NULL) {
  cdw_tables <- dbListTables(con_cdw)
  cdw_counts <- sapply(cdw_tables, function(tbl) {
    dbGetQuery(con_cdw, paste("SELECT COUNT(*) FROM", tbl))[[1]]
  })

  summary <- list(
    cdw = data.frame(
      table = cdw_tables,
      rows = cdw_counts,
      stringsAsFactors = FALSE
    )
  )

  if (!is.null(con_mpi)) {
    mpi_tables <- dbListTables(con_mpi)
    mpi_counts <- sapply(mpi_tables, function(tbl) {
      dbGetQuery(con_mpi, paste("SELECT COUNT(*) FROM", tbl))[[1]]
    })
    summary$mpi <- data.frame(
      table = mpi_tables,
      rows = mpi_counts,
      stringsAsFactors = FALSE
    )
  }

  class(summary) <- c("pcornet_summary", "list")
  summary
}

#' @export
print.pcornet_summary <- function(x, ...) {
  cat("=== PCORnet Database Summary ===\n\n")
  cat("CDW Tables:\n")
  for (i in 1:nrow(x$cdw)) {
    cat(sprintf("  %-20s %s rows\n", x$cdw$table[i], format(x$cdw$rows[i], big.mark = ",")))
  }
  if (!is.null(x$mpi)) {
    cat("\nMPI Tables:\n")
    for (i in 1:nrow(x$mpi)) {
      cat(sprintf("  %-20s %s rows\n", x$mpi$table[i], format(x$mpi$rows[i], big.mark = ",")))
    }
  }
  invisible(x)
}

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

.connect_sql_server <- function(server, database, uid = NULL, pwd = NULL,
                                driver = "ODBC Driver 18 for SQL Server",
                                port = 1433, trusted_connection = FALSE,
                                connection_string = NULL) {
  if (is.null(connection_string)) {
    if (!trusted_connection && (is.null(uid) || is.null(pwd))) {
      stop("Either provide uid/pwd, set trusted_connection = TRUE, or provide connection_string")
    }
    if (trusted_connection) {
      connection_string <- sprintf(
        "Driver={%s};Server=%s,%d;Database=%s;Trusted_Connection=yes;TrustServerCertificate=yes;",
        driver, server, port, database
      )
    } else {
      connection_string <- sprintf(
        "Driver={%s};Server=%s,%d;Database=%s;Uid=%s;Pwd=%s;TrustServerCertificate=yes;",
        driver, server, port, database, uid, pwd
      )
    }
  }

  tryCatch({
    DBI::dbConnect(odbc::odbc(), .connection_string = connection_string)
  }, error = function(e) {
    stop("Failed to connect to SQL Server (", database, "): ", e$message, "\n",
         "Make sure the ODBC driver is installed and connection parameters are correct.")
  })
}

.write_table_batched <- function(con, table_name, data, overwrite = TRUE, batch_size = 10000) {
  n_rows <- nrow(data)
  if (n_rows == 0) return(invisible(NULL))

  table_id <- DBI::Id(schema = "dbo", table = table_name)

  if (overwrite) {
    tryCatch(DBI::dbRemoveTable(con, table_id), error = function(e) NULL)
  }

  if (n_rows <= batch_size) {
    DBI::dbWriteTable(con, table_id, data, overwrite = FALSE, append = FALSE)
  } else {
    DBI::dbWriteTable(con, table_id, data[1:batch_size, ], overwrite = FALSE, append = FALSE)
    for (i in seq(batch_size + 1, n_rows, by = batch_size)) {
      end_row <- min(i + batch_size - 1, n_rows)
      DBI::dbAppendTable(con, table_id, data[i:end_row, ])
    }
  }
}

.random_date <- function(n, start_date, end_date) {
  start <- as.numeric(start_date)
  end <- as.numeric(end_date)
  as.Date(runif(n, start, end), origin = "1970-01-01")
}

.sample_with_na <- function(x, n, prob_na = 0.1, ...) {
  result <- sample(x, n, ...)
  if (prob_na > 0 && n > 0) {
    na_count <- max(1, floor(n * prob_na))
    if (na_count < n) {
      na_indices <- sample(1:n, size = na_count)
      result[na_indices] <- NA
    }
  }
  result
}

.random_datetime <- function(n, start_date, end_date) {
  dates <- .random_date(n, start_date, end_date)
  times <- runif(n, 0, 86400)
  as.POSIXct(as.numeric(dates) * 86400 + times, origin = "1970-01-01", tz = "UTC")
}

# =============================================================================
# RANDOM GENERATION (mode = "random")
# =============================================================================

.generate_random <- function(n_patients, current_date, con_cdw, con_mpi, overwrite, batch_size, sources) {
  cat("Generating random synthetic data for", n_patients, "patients...\n")

  CURRENT_DATETIME <- as.POSIXct(paste(current_date, "12:00:00"), tz = "UTC")

  # Use PATID format for UIDs to match production schema where UID = CDM_PATID
  uids <- paste0("PAT", sprintf("%07d", 1:n_patients))

  # =========================================================================
  # MPI Database
  # =========================================================================

  enterprise_records <- data.frame(
    Uid = uids,
    First = replicate(n_patients, paste0(sample(LETTERS, 1),
                                          paste(sample(letters, sample(3:8, 1)), collapse = ""))),
    M = .sample_with_na(LETTERS, n_patients, prob_na = 0.3, replace = TRUE),
    Last = replicate(n_patients, paste0(sample(LETTERS, 1),
                                         paste(sample(letters, sample(4:10, 1)), collapse = ""))),
    DoB = .random_date(n_patients, as.Date("1930-01-01"), as.Date("2020-01-01")),
    SSN = sprintf("%09d", sample(100000000:999999999, n_patients, replace = FALSE)),
    G = sample(c("M", "F"), n_patients, replace = TRUE),
    Addy = replicate(n_patients, paste(sample(100:9999, 1),
                                        sample(c("Main St", "Oak Ave", "Elm Dr", "Maple Ln"), 1))),
    Phone = sprintf("(%03d)%03d-%04d", sample(200:999, n_patients, replace = TRUE),
                    sample(200:999, n_patients, replace = TRUE),
                    sample(1000:9999, n_patients, replace = TRUE)),
    City = sample(c("Houston", "Dallas", "Austin", "San Antonio", "Fort Worth"),
                  n_patients, replace = TRUE),
    Zip = sprintf("%05d", sample(70000:79999, n_patients, replace = TRUE)),
    LastEditDTTM = .random_datetime(n_patients, as.Date("2020-01-01"), current_date),
    isActive = 1,
    stringsAsFactors = FALSE
  )
  enterprise_records$FirstSdx <- substr(enterprise_records$First, 1, 4)
  enterprise_records$LastSdx <- substr(enterprise_records$Last, 1, 4)

  .write_table_batched(con_mpi, "EnterpriseRecords", enterprise_records, overwrite = overwrite, batch_size = batch_size)

  enterprise_ext <- data.frame(
    Uid = uids,
    MPIUpdateDTTM = .random_datetime(n_patients, as.Date("2020-01-01"), current_date),
    CDM_PATID = uids,  # Now same as Uid
    EPIC_PAT_ID = .sample_with_na(paste0("EPIC", sprintf("%010d", sample(1:99999999, n_patients))),
                                  n_patients, prob_na = 0.2),
    ALLSCRIPTS_PERSON_ID = .sample_with_na(as.integer(sample(1:999999, n_patients)),
                                          n_patients, prob_na = 0.7),
    MHH_COVID_PERSON = .sample_with_na(as.integer(sample(1:999999, n_patients)),
                                       n_patients, prob_na = 0.8),
    MHH_MRN = .sample_with_na(paste0("MHH", sprintf("%07d", sample(1:9999999, n_patients))),
                             n_patients, prob_na = 0.6),
    UTP_MRN = .sample_with_na(paste0("UTP", sprintf("%07d", sample(1:9999999, n_patients))),
                             n_patients, prob_na = 0.7),
    EpicOnly = sample(c("Y", "N"), n_patients, replace = TRUE, prob = c(0.3, 0.7)),
    CDM_SEX = enterprise_records$G,
    CDM_RACE = sample(c("01", "02", "03", "04", "05", "06", "07"), n_patients, replace = TRUE),
    CDM_HISPANIC = sample(c("Y", "N", "NI"), n_patients, replace = TRUE, prob = c(0.2, 0.7, 0.1)),
    Race = sample(c("White", "Black", "Asian", "Other", "Unknown"), n_patients, replace = TRUE),
    Ethnicity = sample(c("Hispanic", "Not Hispanic", "Unknown"), n_patients, replace = TRUE),
    Language = sample(c("English", "Spanish", "Other"), n_patients, replace = TRUE, prob = c(0.7, 0.2, 0.1)),
    IsDeceased = sample(c("Y", "N"), n_patients, replace = TRUE, prob = c(0.05, 0.95)),
    DeceasedDTTM = as.POSIXct(NA),
    State = "TX",
    stringsAsFactors = FALSE
  )

  deceased_idx <- which(enterprise_ext$IsDeceased == "Y")
  if (length(deceased_idx) > 0) {
    enterprise_ext$DeceasedDTTM[deceased_idx] <- as.POSIXct(
      .random_date(length(deceased_idx),
                pmax(enterprise_records$DoB[deceased_idx] + 365, as.Date("2015-01-01")),
                current_date)
    )
  }

  .write_table_batched(con_mpi, "EnterpriseRecords_Ext", enterprise_ext, overwrite = overwrite, batch_size = batch_size)

  # MPI mappings - uids_in_system is now VARCHAR (PATID format)
  source_names <- names(sources)
  mpi_records <- list()
  for (src in source_names) {
    n_in_system <- sample(floor(n_patients * 0.6):floor(n_patients * 0.95), 1)
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
  .write_table_batched(con_mpi, "Mpi", mpi, overwrite = overwrite, batch_size = batch_size)

  mpi_src <- data.frame(
    SRC = source_names,
    LID_SRC = sapply(sources, function(x) x$id_field),
    Description = sapply(sources, function(x) x$description),
    stringsAsFactors = FALSE
  )
  .write_table_batched(con_mpi, "MPI_Src", mpi_src, overwrite = overwrite, batch_size = batch_size)

  # =========================================================================
  # CDW Database
  # =========================================================================

  demographic <- data.frame(
    PATID = uids,  # Now same as Uid
    BIRTH_DATE = enterprise_records$DoB,
    BIRTH_TIME = .sample_with_na(sprintf("%02d:%02d", sample(0:23, n_patients, replace = TRUE),
                                         sample(0:59, n_patients, replace = TRUE)),
                                 n_patients, prob_na = 0.5),
    SEX = enterprise_records$G,
    SEXUAL_ORIENTATION = .sample_with_na(c("AS", "BI", "GA", "LE", "QS", "SE", "OT", "NI", "UN"),
                                        n_patients, prob_na = 0.3, replace = TRUE),
    GENDER_IDENTITY = .sample_with_na(c("F", "M", "TM", "TF", "GQ", "OT", "NI", "UN"),
                                     n_patients, prob_na = 0.2, replace = TRUE),
    HISPANIC = enterprise_ext$CDM_HISPANIC,
    RACE = enterprise_ext$CDM_RACE,
    PAT_PREF_LANGUAGE_SPOKEN = .sample_with_na(c("eng", "spa", "zho", "vie", "ara", "oth"),
                                              n_patients, prob_na = 0.15, replace = TRUE,
                                              prob = c(0.70, 0.20, 0.03, 0.02, 0.02, 0.03)),
    firstName = enterprise_records$First,
    lastName = enterprise_records$Last,
    addressLine1 = enterprise_records$Addy,
    city = enterprise_records$City,
    state = "TX",
    zipCode = enterprise_records$Zip,
    isDeceased = enterprise_ext$IsDeceased,
    CDW_Source = sample(c("EPIC", "ALLSCRIPTS", "MHH"), n_patients, replace = TRUE),
    CDW_UpdatedDTTM = .random_datetime(n_patients, as.Date("2020-01-01"), current_date),
    GPC_FLAG = "Y",
    UID = uids,
    stringsAsFactors = FALSE
  )

  demographic$EPIC_PAT_ID <- enterprise_ext$EPIC_PAT_ID
  demographic$MHH_MRN <- enterprise_ext$MHH_MRN
  demographic$UTP_MRN <- enterprise_ext$UTP_MRN
  demographic$RAW_SEX <- demographic$SEX
  demographic$RAW_RACE <- enterprise_ext$Race
  demographic$RAW_HISPANIC <- enterprise_ext$Ethnicity

  .write_table_batched(con_cdw, "DEMOGRAPHIC", demographic, overwrite = overwrite, batch_size = batch_size)

  # DEATH table
  death_records <- enterprise_ext %>%
    filter(IsDeceased == "Y") %>%
    transmute(
      PATID = Uid,  # Uid is now PATID format
      DEATH_DATE = as.Date(DeceasedDTTM),
      DEATH_DATE_IMPUTE = "N",
      DEATH_SOURCE = sample(c("L", "N", "S"), dplyr::n(), replace = TRUE),
      DEATH_MATCH_CONFIDENCE = NA,
      GPC_FLAG = "Y",
      CDW_UpdatedDTTM = CURRENT_DATETIME,
      UID = Uid,
      MPI_LID = NA,
      MPI_SRC = NA
    )
  .write_table_batched(con_cdw, "DEATH", death_records, overwrite = overwrite, batch_size = batch_size)

  # Generate encounters, diagnoses, procedures, labs, prescriptions, vitals
  # (Simplified version - generates random clinical data)
  icd10_codes <- c("E11.9", "I10", "J44.9", "F41.9", "M79.3", "Z79.4", "E78.5",
                   "N18.3", "K21.9", "G47.33", "E66.9", "Z87.891", "F17.210")
  cpt_codes <- c("99213", "99214", "80053", "93000", "85025", "36415", "99395", "71020")
  medications <- data.frame(
    NAME = c("Metformin", "Lisinopril", "Atorvastatin", "Levothyroxine", "Amlodipine",
             "Albuterol", "Omeprazole", "Gabapentin", "Sertraline", "Aspirin"),
    RXNORM = c("6809", "29046", "83367", "10582", "17767", "435", "7646", "25480", "36437", "1191"),
    DOSE = c(500, 10, 20, 50, 5, 90, 20, 300, 50, 81),
    UNIT = c("mg", "mg", "mg", "mcg", "mg", "mcg", "mg", "mg", "mg", "mg"),
    stringsAsFactors = FALSE
  )
  lab_tests <- data.frame(
    LOINC = c("2345-7", "4548-4", "2160-0", "33914-3", "2951-2", "6690-2"),
    NAME = c("Glucose", "HbA1c", "Creatinine", "GFR", "Sodium", "WBC"),
    UNIT = c("mg/dL", "%", "mg/dL", "mL/min/1.73m2", "mmol/L", "10*3/uL"),
    NORMAL_LOW = c(70, 4.0, 0.6, 60, 135, 4.0),
    NORMAL_HIGH = c(100, 5.6, 1.2, 120, 145, 11.0),
    stringsAsFactors = FALSE
  )

  encounters <- list()
  diagnoses <- list()
  procedures <- list()
  lab_results <- list()
  prescriptions <- list()
  vitals <- list()

  enc_id <- 1
  dx_id <- 1
  px_id <- 1
  lab_id <- 1
  rx_id <- 1
  vital_id <- 1

  for (i in 1:n_patients) {
    patid <- uids[i]  # Use PATID format from uids vector
    birth_date <- demographic$BIRTH_DATE[i]
    death_date <- if (i %in% deceased_idx) as.Date(enterprise_ext$DeceasedDTTM[i]) else current_date

    n_enc <- sample(5:20, 1)
    admit_dates <- .random_date(n_enc, birth_date + 365, death_date)

    for (j in 1:n_enc) {
      enc_type <- sample(c("IP", "ED", "AV", "OA", "IS"), 1, prob = c(0.15, 0.10, 0.50, 0.15, 0.10))
      los_days <- switch(enc_type, "IP" = sample(1:14, 1), "ED" = sample(0:1, 1),
                         "IS" = sample(1:7, 1), 0)

      discharge_disp <- sample(c("A", "E", "NI", "UN", "OT"), 1)
      discharge_status <- sample(c("AF", "AL", "AM", "AW", "EX", "HH", "HO", "HS", "IP", "NH", "RH", "RS", "SH", "SN", "NI", "UN", "OT"), 1)
      payer_type <- sample(c("1", "2", "3", "4", "5", "6", "NI", "UN", "OT"), 1)
      encounters[[enc_id]] <- data.frame(
        ENCOUNTERID = paste0("ENC", sprintf("%010d", enc_id)),
        PATID = patid,
        ADMIT_DATE = admit_dates[j],
        ADMIT_TIME = sprintf("%02d:%02d", sample(0:23, 1), sample(0:59, 1)),
        DISCHARGE_DATE = if (los_days > 0) admit_dates[j] + los_days else NA,
        DISCHARGE_TIME = if (los_days > 0) sprintf("%02d:%02d", sample(0:23, 1), sample(0:59, 1)) else NA,
        ENC_TYPE = enc_type,
        PROVIDERID = paste0("PROV", sprintf("%06d", sample(1:500, 1))),
        FACILITY_LOCATION = sample(c("77001", "77002", "77003", "77004", "77005"), 1),
        FACILITYID = paste0("FAC", sprintf("%05d", sample(1:100, 1))),
        DISCHARGE_DISPOSITION = discharge_disp,
        DISCHARGE_STATUS = discharge_status,
        DRG = if (enc_type == "IP") sprintf("%03d", sample(1:999, 1)) else NA,
        DRG_TYPE = if (enc_type == "IP") sample(c("01", "02"), 1) else NA,
        ADMITTING_SOURCE = sample(c("AF", "AL", "AV", "ED", "HH", "HO", "HS", "IP", "NH", "RH", "RS", "SN", "NI", "UN", "OT"), 1),
        RAW_ENC_TYPE = enc_type,
        RAW_DISCHARGE_DISPOSITION = discharge_disp,
        RAW_DISCHARGE_STATUS = discharge_status,
        PAYER_TYPE_PRIMARY = payer_type,
        CDW_Source = sample(c("EPIC", "ALLSCRIPTS"), 1),
        GPC_FLAG = "Y",
        UID = patid,  # Now PATID format
        stringsAsFactors = FALSE
      )

      # Diagnoses
      n_dx <- sample(1:5, 1)
      for (k in 1:n_dx) {
        code_idx <- sample(1:length(icd10_codes), 1)
        dx_code <- icd10_codes[code_idx]
        pdx_val <- if (k == 1) "P" else "S"
        dx_source <- sample(c("AD", "DI", "FI", "IN", "NI", "UN", "OT"), 1)
        diagnoses[[dx_id]] <- data.frame(
          DIAGNOSISID = paste0("DX", sprintf("%010d", dx_id)),
          PATID = patid,
          ENCOUNTERID = paste0("ENC", sprintf("%010d", enc_id)),
          DX = dx_code,
          DX_TYPE = "10",
          DX_DATE = admit_dates[j],
          DX_SOURCE = dx_source,
          DX_ORIGIN = sample(c("OD", "BI", "CL", "NI", "UN", "OT"), 1),
          DX_POA = sample(c("Y", "N", "U", "W", "1", "NI", "UN", "OT"), 1),
          PDX = pdx_val,
          ADMIT_DATE = admit_dates[j],
          ENC_TYPE = enc_type,
          PROVIDERID = paste0("PROV", sprintf("%06d", sample(1:500, 1))),
          RAW_DX = dx_code,
          RAW_DX_TYPE = "ICD-10-CM",
          RAW_DX_SOURCE = dx_source,
          RAW_PDX = pdx_val,
          CDW_Source = sample(c("EPIC", "ALLSCRIPTS"), 1),
          UID = patid,  # Now PATID format
          stringsAsFactors = FALSE
        )
        dx_id <- dx_id + 1
      }

      # Procedures
      if (runif(1) < 0.7) {
        n_px <- sample(1:3, 1)
        for (k in 1:n_px) {
          px_code <- sample(cpt_codes, 1)
          px_source <- sample(c("OD", "BI", "CL", "NI", "UN", "OT"), 1)
          procedures[[px_id]] <- data.frame(
            PROCEDURESID = paste0("PX", sprintf("%010d", px_id)),
            PATID = patid,
            ENCOUNTERID = paste0("ENC", sprintf("%010d", enc_id)),
            PX = px_code,
            PX_TYPE = "CH",
            PX_DATE = admit_dates[j],
            PX_SOURCE = px_source,
            PPX = if (k == 1) "P" else "S",
            ADMIT_DATE = admit_dates[j],
            ENC_TYPE = enc_type,
            PROVIDERID = paste0("PROV", sprintf("%06d", sample(1:500, 1))),
            RAW_PX = px_code,
            RAW_PX_TYPE = "CPT",
            RAW_PX_NAME = paste0("Procedure ", px_code),
            CDW_Source = sample(c("EPIC", "ALLSCRIPTS"), 1),
            UID = patid,  # Now PATID format
            stringsAsFactors = FALSE
          )
          px_id <- px_id + 1
        }
      }

      # Labs
      if (runif(1) < 0.5) {
        n_labs <- sample(2:6, 1)
        for (k in 1:n_labs) {
          test_idx <- sample(1:nrow(lab_tests), 1)
          result_num <- runif(1, lab_tests$NORMAL_LOW[test_idx] * 0.8,
                              lab_tests$NORMAL_HIGH[test_idx] * 1.2)
          result_num <- round(result_num, 2)
          norm_low <- lab_tests$NORMAL_LOW[test_idx]
          norm_high <- lab_tests$NORMAL_HIGH[test_idx]
          abn_ind <- if (result_num < norm_low) "AB" else if (result_num > norm_high) "AH" else "NI"
          lab_results[[lab_id]] <- data.frame(
            LAB_RESULT_CM_ID = paste0("LAB", sprintf("%010d", lab_id)),
            PATID = patid,
            ENCOUNTERID = paste0("ENC", sprintf("%010d", enc_id)),
            LAB_LOINC = lab_tests$LOINC[test_idx],
            LAB_PX = lab_tests$LOINC[test_idx],
            LAB_PX_TYPE = "LC",
            LAB_ORDER_DATE = admit_dates[j],
            RESULT_DATE = admit_dates[j],
            RESULT_TIME = sprintf("%02d:%02d", sample(0:23, 1), sample(0:59, 1)),
            RESULT_NUM = result_num,
            RESULT_QUAL = sample(c("POSITIVE", "NEGATIVE", "NI", "UN", "OT"), 1, prob = c(0.1, 0.1, 0.6, 0.1, 0.1)),
            RESULT_MODIFIER = sample(c("EQ", "GE", "GT", "LE", "LT", "NI"), 1, prob = c(0.8, 0.05, 0.05, 0.05, 0.03, 0.02)),
            RESULT_UNIT = lab_tests$UNIT[test_idx],
            NORM_RANGE_LOW = as.character(norm_low),
            NORM_RANGE_HIGH = as.character(norm_high),
            NORM_MODIFIER_LOW = "EQ",
            NORM_MODIFIER_HIGH = "EQ",
            ABN_IND = abn_ind,
            SPECIMEN_SOURCE = sample(c("BLOOD", "URINE", "CSF", "OTHER", "NI", "UN", "OT"), 1, prob = c(0.5, 0.2, 0.05, 0.05, 0.1, 0.05, 0.05)),
            SPECIMEN_DATE = admit_dates[j],
            PRIORITY = sample(c("R", "S", "NI", "UN", "OT"), 1),
            RESULT_LOC = sample(c("L", "P", "NI", "UN", "OT"), 1),
            LAB_LOINC_SOURCE = sample(c("OD", "BI", "CL", "NI", "UN", "OT"), 1),
            LAB_RESULT_SOURCE = sample(c("OD", "BI", "CL", "NI", "UN", "OT"), 1),
            RAW_LAB_NAME = lab_tests$NAME[test_idx],
            RAW_LAB_CODE = lab_tests$LOINC[test_idx],
            RAW_RESULT = as.character(result_num),
            RAW_UNIT = lab_tests$UNIT[test_idx],
            CDW_Source = sample(c("EPIC", "ALLSCRIPTS"), 1),
            UID = patid,  # Now PATID format
            stringsAsFactors = FALSE
          )
          lab_id <- lab_id + 1
        }
      }

      # Prescriptions
      if (runif(1) < 0.6) {
        n_rx <- sample(1:3, 1)
        for (k in 1:n_rx) {
          med_idx <- sample(1:nrow(medications), 1)
          days_supply <- sample(c(7, 14, 30, 60, 90), 1)
          refills <- sample(0:5, 1)
          quantity <- sample(c(30, 60, 90, 120), 1)
          dose <- sample(c("5", "10", "20", "25", "50", "100", "250", "500"), 1)
          dose_unit <- sample(c("mg", "mcg", "g", "mL"), 1)
          frequency <- sample(c("QD", "BID", "TID", "QID", "PRN", "QHS"), 1)
          route <- sample(c("PO", "IV", "IM", "SC", "TOP", "INH", "NI", "UN", "OT"), 1)
          prescriptions[[rx_id]] <- data.frame(
            PRESCRIBINGID = paste0("RX", sprintf("%010d", rx_id)),
            PATID = patid,
            ENCOUNTERID = paste0("ENC", sprintf("%010d", enc_id)),
            RX_PROVIDERID = paste0("PROV", sprintf("%06d", sample(1:500, 1))),
            RX_ORDER_DATE = admit_dates[j],
            RX_ORDER_TIME = sprintf("%02d:%02d", sample(0:23, 1), sample(0:59, 1)),
            RX_START_DATE = admit_dates[j],
            RX_END_DATE = admit_dates[j] + days_supply,
            RX_DAYS_SUPPLY = days_supply,
            RX_REFILLS = refills,
            RX_QUANTITY = quantity,
            RX_DOSE_ORDERED = dose,
            RX_DOSE_ORDERED_UNIT = dose_unit,
            RX_DOSE_FORM = sample(c("TAB", "CAP", "SOL", "INJ", "CRE", "OIN", "NI", "UN", "OT"), 1),
            RX_FREQUENCY = frequency,
            RX_ROUTE = route,
            RX_BASIS = sample(c("01", "02", "NI", "UN", "OT"), 1),
            RX_PRN_FLAG = sample(c("Y", "N"), 1, prob = c(0.2, 0.8)),
            RX_DISPENSE_AS_WRITTEN = sample(c("Y", "N", "NI", "UN", "OT"), 1),
            RX_SOURCE = sample(c("OD", "BI", "CL", "NI", "UN", "OT"), 1),
            RXNORM_CUI = medications$RXNORM[med_idx],
            RAW_RX_MED_NAME = medications$NAME[med_idx],
            RAW_RX_FREQUENCY = frequency,
            RAW_RX_DOSE_ORDERED = dose,
            RAW_RX_DOSE_ORDERED_UNIT = dose_unit,
            RAW_RX_ROUTE = route,
            RAW_RX_REFILLS = as.character(refills),
            RAW_RXNORM_CUI = as.character(medications$RXNORM[med_idx]),
            RAW_RX_NDC = paste0(sample(10000:99999, 1), "-", sample(1000:9999, 1), "-", sample(10:99, 1)),
            CDW_Source = sample(c("EPIC", "ALLSCRIPTS"), 1),
            UID = patid,  # Now PATID format
            stringsAsFactors = FALSE
          )
          rx_id <- rx_id + 1
        }
      }

      # Vitals
      if (runif(1) < 0.9) {
        ht <- round(rnorm(1, 170, 15), 1)
        wt <- round(rnorm(1, 80, 20), 1)
        systolic <- round(rnorm(1, 125, 15), 0)
        diastolic <- round(rnorm(1, 80, 10), 0)
        bp_position <- sample(c("01", "02", "03", "NI", "UN", "OT"), 1)
        smoking <- sample(c("01", "02", "03", "04", "05", "06", "07", "08", "NI", "UN"), 1)
        tobacco <- sample(c("01", "02", "03", "04", "06", "NI", "UN"), 1)
        tobacco_type <- sample(c("01", "02", "03", "04", "05", "NI", "UN"), 1)
        vitals[[vital_id]] <- data.frame(
          VITALID = paste0("VIT", sprintf("%010d", vital_id)),
          PATID = patid,
          ENCOUNTERID = paste0("ENC", sprintf("%010d", enc_id)),
          MEASURE_DATE = admit_dates[j],
          MEASURE_TIME = sprintf("%02d:%02d", sample(0:23, 1), sample(0:59, 1)),
          VITAL_SOURCE = sample(c("PR", "PD", "HC", "HD", "NI", "UN", "OT"), 1),
          HT = ht,
          WT = wt,
          ORIGINAL_BMI = round(wt / ((ht / 100)^2), 2),
          SYSTOLIC = systolic,
          DIASTOLIC = diastolic,
          BP_POSITION = bp_position,
          SMOKING = smoking,
          TOBACCO = tobacco,
          TOBACCO_TYPE = tobacco_type,
          RAW_SYSTOLIC = as.character(systolic),
          RAW_DIASTOLIC = as.character(diastolic),
          RAW_BP_POSITION = bp_position,
          RAW_SMOKING = smoking,
          RAW_TOBACCO = tobacco,
          RAW_TOBACCO_TYPE = tobacco_type,
          CDW_Source = sample(c("EPIC", "ALLSCRIPTS"), 1),
          UID = patid,  # Now PATID format
          stringsAsFactors = FALSE
        )
        vital_id <- vital_id + 1
      }

      enc_id <- enc_id + 1
    }
  }

  encounter <- do.call(rbind, encounters)
  diagnosis <- do.call(rbind, diagnoses)
  procedure <- if (length(procedures) > 0) do.call(rbind, procedures) else data.frame()
  lab_result_cm <- if (length(lab_results) > 0) do.call(rbind, lab_results) else data.frame()
  prescribing <- if (length(prescriptions) > 0) do.call(rbind, prescriptions) else data.frame()
  vital <- if (length(vitals) > 0) do.call(rbind, vitals) else data.frame()

  .write_table_batched(con_cdw, "ENCOUNTER", encounter, overwrite = overwrite, batch_size = batch_size)
  .write_table_batched(con_cdw, "DIAGNOSIS", diagnosis, overwrite = overwrite, batch_size = batch_size)
  if (nrow(procedure) > 0) .write_table_batched(con_cdw, "PROCEDURES", procedure, overwrite = overwrite, batch_size = batch_size)
  if (nrow(lab_result_cm) > 0) .write_table_batched(con_cdw, "LAB_RESULT_CM", lab_result_cm, overwrite = overwrite, batch_size = batch_size)
  if (nrow(prescribing) > 0) .write_table_batched(con_cdw, "PRESCRIBING", prescribing, overwrite = overwrite, batch_size = batch_size)
  if (nrow(vital) > 0) .write_table_batched(con_cdw, "VITAL", vital, overwrite = overwrite, batch_size = batch_size)

  # Provider table
  providers <- data.frame(
    PROVIDERID = paste0("PROV", sprintf("%06d", 1:500)),
    ProviderName = replicate(500, paste("Dr.", paste0(sample(LETTERS, 1),
                                                       paste(sample(letters, sample(4:8, 1)), collapse = "")))),
    PROVIDER_SPECIALTY_PRIMARY = sample(c("Internal Medicine", "Family Medicine", "Cardiology",
                                         "Endocrinology", "Emergency Medicine"), 500, replace = TRUE),
    GPC_FLAG = "Y",
    stringsAsFactors = FALSE
  )
  .write_table_batched(con_cdw, "PROVIDER", providers, overwrite = overwrite, batch_size = batch_size)

  cat("Data written to SQL Server.\n")

  list(
    cdw = con_cdw,
    mpi = con_mpi,
    summary = list(
      n_patients = n_patients,
      n_encounters = nrow(encounter),
      n_diagnoses = nrow(diagnosis),
      n_procedures = nrow(procedure),
      n_labs = nrow(lab_result_cm),
      n_prescriptions = nrow(prescribing),
      n_vitals = nrow(vital)
    )
  )
}

# =============================================================================
# ENHANCED GENERATION (mode = "enhanced")
# =============================================================================

.generate_enhanced <- function(n_patients, current_date, profile_weights, con_cdw, con_mpi, overwrite, batch_size, sources) {
  cat("Generating clinically coherent data for", n_patients, "patients...\n")


  # Clinical profiles are now loaded as part of the package (R/clinical_profiles.R)

  # Apply custom profile weights if provided
  if (!is.null(profile_weights)) {
    for (name in names(profile_weights)) {
      if (name %in% names(CLINICAL_PROFILES)) {
        CLINICAL_PROFILES[[name]]$prevalence_base <- profile_weights[[name]]
      }
    }
  }

  CURRENT_DATETIME <- as.POSIXct(paste(current_date, "12:00:00"), tz = "UTC")

  # Use PATID format for UIDs to match production schema where UID = CDM_PATID
  uids <- paste0("PAT", sprintf("%07d", 1:n_patients))

  # =========================================================================
  # MPI Database (same as random)
  # =========================================================================

  enterprise_records <- data.frame(
    Uid = uids,
    First = replicate(n_patients, paste0(sample(LETTERS, 1),
                                          paste(sample(letters, sample(3:8, 1)), collapse = ""))),
    M = .sample_with_na(LETTERS, n_patients, prob_na = 0.3, replace = TRUE),
    Last = replicate(n_patients, paste0(sample(LETTERS, 1),
                                         paste(sample(letters, sample(4:10, 1)), collapse = ""))),
    DoB = .random_date(n_patients, as.Date("1930-01-01"), as.Date("2020-01-01")),
    SSN = sprintf("%09d", sample(100000000:999999999, n_patients, replace = FALSE)),
    G = sample(c("M", "F"), n_patients, replace = TRUE),
    Addy = replicate(n_patients, paste(sample(100:9999, 1),
                                        sample(c("Main St", "Oak Ave", "Elm Dr", "Maple Ln"), 1))),
    Phone = sprintf("(%03d)%03d-%04d", sample(200:999, n_patients, replace = TRUE),
                    sample(200:999, n_patients, replace = TRUE),
                    sample(1000:9999, n_patients, replace = TRUE)),
    City = sample(c("Houston", "Dallas", "Austin", "San Antonio", "Fort Worth"),
                  n_patients, replace = TRUE),
    Zip = sprintf("%05d", sample(70000:79999, n_patients, replace = TRUE)),
    LastEditDTTM = .random_datetime(n_patients, as.Date("2020-01-01"), current_date),
    isActive = 1,
    stringsAsFactors = FALSE
  )
  enterprise_records$FirstSdx <- substr(enterprise_records$First, 1, 4)
  enterprise_records$LastSdx <- substr(enterprise_records$Last, 1, 4)

  .write_table_batched(con_mpi, "EnterpriseRecords", enterprise_records, overwrite = overwrite, batch_size = batch_size)

  enterprise_ext <- data.frame(
    Uid = uids,
    MPIUpdateDTTM = .random_datetime(n_patients, as.Date("2020-01-01"), current_date),
    CDM_PATID = uids,  # Now same as Uid
    EPIC_PAT_ID = .sample_with_na(paste0("EPIC", sprintf("%010d", sample(1:99999999, n_patients))),
                                  n_patients, prob_na = 0.2),
    ALLSCRIPTS_PERSON_ID = .sample_with_na(as.integer(sample(1:999999, n_patients)),
                                          n_patients, prob_na = 0.7),
    MHH_COVID_PERSON = .sample_with_na(as.integer(sample(1:999999, n_patients)),
                                       n_patients, prob_na = 0.8),
    MHH_MRN = .sample_with_na(paste0("MHH", sprintf("%07d", sample(1:9999999, n_patients))),
                             n_patients, prob_na = 0.6),
    UTP_MRN = .sample_with_na(paste0("UTP", sprintf("%07d", sample(1:9999999, n_patients))),
                             n_patients, prob_na = 0.7),
    EpicOnly = sample(c("Y", "N"), n_patients, replace = TRUE, prob = c(0.3, 0.7)),
    CDM_SEX = enterprise_records$G,
    CDM_RACE = sample(c("01", "02", "03", "04", "05", "06", "07"), n_patients, replace = TRUE),
    CDM_HISPANIC = sample(c("Y", "N", "NI"), n_patients, replace = TRUE, prob = c(0.2, 0.7, 0.1)),
    Race = sample(c("White", "Black", "Asian", "Other", "Unknown"), n_patients, replace = TRUE),
    Ethnicity = sample(c("Hispanic", "Not Hispanic", "Unknown"), n_patients, replace = TRUE),
    Language = sample(c("English", "Spanish", "Other"), n_patients, replace = TRUE, prob = c(0.7, 0.2, 0.1)),
    IsDeceased = sample(c("Y", "N"), n_patients, replace = TRUE, prob = c(0.05, 0.95)),
    DeceasedDTTM = as.POSIXct(NA),
    State = "TX",
    stringsAsFactors = FALSE
  )

  deceased_idx <- which(enterprise_ext$IsDeceased == "Y")
  if (length(deceased_idx) > 0) {
    enterprise_ext$DeceasedDTTM[deceased_idx] <- as.POSIXct(
      .random_date(length(deceased_idx),
                pmax(enterprise_records$DoB[deceased_idx] + 365, as.Date("2015-01-01")),
                current_date)
    )
  }

  .write_table_batched(con_mpi, "EnterpriseRecords_Ext", enterprise_ext, overwrite = overwrite, batch_size = batch_size)

  # MPI mappings - uids_in_system is now VARCHAR (PATID format)
  source_names <- names(sources)
  mpi_records <- list()
  for (src in source_names) {
    n_in_system <- sample(floor(n_patients * 0.6):floor(n_patients * 0.95), 1)
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
  .write_table_batched(con_mpi, "Mpi", mpi, overwrite = overwrite, batch_size = batch_size)

  mpi_src <- data.frame(
    SRC = source_names,
    LID_SRC = sapply(sources, function(x) x$id_field),
    Description = sapply(sources, function(x) x$description),
    stringsAsFactors = FALSE
  )
  .write_table_batched(con_mpi, "MPI_Src", mpi_src, overwrite = overwrite, batch_size = batch_size)

  # =========================================================================
  # Assign clinical profiles
  # =========================================================================

  patient_data <- data.frame(
    uid = uids,
    birth_date = enterprise_records$DoB,
    sex = enterprise_records$G,
    stringsAsFactors = FALSE
  )

  patient_data <- assign_clinical_profiles(patient_data, current_date, CLINICAL_PROFILES)

  profile_counts <- table(patient_data$clinical_profile)
  cat("\nClinical profile distribution:\n")
  for (profile_name in names(profile_counts)) {
    cat(sprintf("  %s: %d patients (%.1f%%)\n",
                profile_name,
                profile_counts[profile_name],
                100 * profile_counts[profile_name] / n_patients))
  }

  # =========================================================================
  # CDW Database with clinical profiles
  # =========================================================================

  demographic <- data.frame(
    PATID = uids,  # Now same as Uid
    BIRTH_DATE = enterprise_records$DoB,
    BIRTH_TIME = .sample_with_na(sprintf("%02d:%02d", sample(0:23, n_patients, replace = TRUE),
                                         sample(0:59, n_patients, replace = TRUE)),
                                 n_patients, prob_na = 0.5),
    SEX = enterprise_records$G,
    HISPANIC = enterprise_ext$CDM_HISPANIC,
    RACE = enterprise_ext$CDM_RACE,
    isDeceased = enterprise_ext$IsDeceased,
    CDW_Source = sample(c("EPIC", "ALLSCRIPTS", "MHH"), n_patients, replace = TRUE),
    GPC_FLAG = "Y",
    UID = uids,
    CLINICAL_PROFILE = patient_data$clinical_profile,
    stringsAsFactors = FALSE
  )

  .write_table_batched(con_cdw, "DEMOGRAPHIC", demographic, overwrite = overwrite, batch_size = batch_size)

  # DEATH table
  death_records <- enterprise_ext %>%
    filter(IsDeceased == "Y") %>%
    transmute(
      PATID = Uid,  # Uid is now PATID format
      DEATH_DATE = as.Date(DeceasedDTTM),
      DEATH_DATE_IMPUTE = "N",
      DEATH_SOURCE = sample(c("L", "N", "S"), dplyr::n(), replace = TRUE),
      GPC_FLAG = "Y",
      UID = Uid
    )
  .write_table_batched(con_cdw, "DEATH", death_records, overwrite = overwrite, batch_size = batch_size)

  # Generate profile-based clinical data
  cat("\nGenerating clinical data based on profiles...\n")

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

  for (i in 1:n_patients) {
    patid <- uids[i]  # Use PATID format from uids vector
    birth_date <- demographic$BIRTH_DATE[i]
    death_date <- if (i %in% deceased_idx) as.Date(enterprise_ext$DeceasedDTTM[i]) else current_date
    profile_name <- patient_data$clinical_profile[i]
    profile <- CLINICAL_PROFILES[[profile_name]]

    n_enc <- generate_encounter_count(profile_name, CLINICAL_PROFILES)
    admit_dates <- .random_date(n_enc, birth_date + 365, death_date)

    patient_medications <- generate_profile_medications(profile)

    for (j in 1:n_enc) {
      enc_type <- select_encounter_type(profile_name)
      los_days <- calculate_length_of_stay(enc_type)

      discharge_disp <- sample(c("A", "E", "NI", "UN", "OT"), 1)
      discharge_status <- sample(c("AF", "AL", "AM", "AW", "EX", "HH", "HO", "HS", "IP", "NH", "RH", "RS", "SH", "SN", "NI", "UN", "OT"), 1)
      payer_type <- sample(c("1", "2", "3", "4", "5", "6", "NI", "UN", "OT"), 1)
      all_encounters[[enc_id]] <- data.frame(
        ENCOUNTERID = paste0("ENC", sprintf("%010d", enc_id)),
        PATID = patid,
        ADMIT_DATE = admit_dates[j],
        ADMIT_TIME = sprintf("%02d:%02d", sample(0:23, 1), sample(0:59, 1)),
        DISCHARGE_DATE = if (los_days > 0) admit_dates[j] + los_days else NA,
        DISCHARGE_TIME = if (los_days > 0) sprintf("%02d:%02d", sample(0:23, 1), sample(0:59, 1)) else NA,
        ENC_TYPE = enc_type,
        PROVIDERID = paste0("PROV", sprintf("%06d", sample(1:500, 1))),
        FACILITY_LOCATION = sample(c("77001", "77002", "77003", "77004", "77005"), 1),
        FACILITYID = paste0("FAC", sprintf("%05d", sample(1:100, 1))),
        DISCHARGE_DISPOSITION = discharge_disp,
        DISCHARGE_STATUS = discharge_status,
        DRG = if (enc_type == "IP") sprintf("%03d", sample(1:999, 1)) else NA,
        DRG_TYPE = if (enc_type == "IP") sample(c("01", "02"), 1) else NA,
        ADMITTING_SOURCE = sample(c("AF", "AL", "AV", "ED", "HH", "HO", "HS", "IP", "NH", "RH", "RS", "SN", "NI", "UN", "OT"), 1),
        RAW_ENC_TYPE = enc_type,
        RAW_DISCHARGE_DISPOSITION = discharge_disp,
        RAW_DISCHARGE_STATUS = discharge_status,
        PAYER_TYPE_PRIMARY = payer_type,
        CDW_Source = sample(c("EPIC", "ALLSCRIPTS"), 1),
        GPC_FLAG = "Y",
        UID = patid,  # Now PATID format
        stringsAsFactors = FALSE
      )

      # Profile-based diagnoses
      dx_data <- generate_profile_diagnoses(profile, is_first_encounter = (j == 1))
      if (!is.null(dx_data) && nrow(dx_data) > 0) {
        for (k in 1:nrow(dx_data)) {
          pdx_val <- if (dx_data$is_primary[k]) "P" else "S"
          dx_source <- sample(c("AD", "DI", "FI", "IN", "NI", "UN", "OT"), 1)
          all_diagnoses[[dx_id]] <- data.frame(
            DIAGNOSISID = paste0("DX", sprintf("%010d", dx_id)),
            PATID = patid,
            ENCOUNTERID = paste0("ENC", sprintf("%010d", enc_id)),
            DX = dx_data$code[k],
            DX_TYPE = "10",
            DX_DATE = admit_dates[j],
            DX_SOURCE = dx_source,
            DX_ORIGIN = sample(c("OD", "BI", "CL", "NI", "UN", "OT"), 1),
            DX_POA = sample(c("Y", "N", "U", "W", "1", "NI", "UN", "OT"), 1),
            PDX = pdx_val,
            ADMIT_DATE = admit_dates[j],
            ENC_TYPE = enc_type,
            PROVIDERID = paste0("PROV", sprintf("%06d", sample(1:500, 1))),
            RAW_DX = dx_data$description[k],
            RAW_DX_TYPE = "ICD-10-CM",
            RAW_DX_SOURCE = dx_source,
            RAW_PDX = pdx_val,
            CDW_Source = sample(c("EPIC", "ALLSCRIPTS"), 1),
            UID = patid,
            stringsAsFactors = FALSE
          )
          dx_id <- dx_id + 1
        }
      }

      # Profile-based labs
      lab_data <- generate_profile_labs(profile, enc_type)
      if (!is.null(lab_data) && nrow(lab_data) > 0) {
        for (k in 1:nrow(lab_data)) {
          all_labs[[lab_id]] <- data.frame(
            LAB_RESULT_CM_ID = paste0("LAB", sprintf("%010d", lab_id)),
            PATID = patid,
            ENCOUNTERID = paste0("ENC", sprintf("%010d", enc_id)),
            LAB_LOINC = lab_data$loinc[k],
            LAB_PX = lab_data$loinc[k],
            LAB_PX_TYPE = "LC",
            LAB_ORDER_DATE = admit_dates[j],
            RESULT_DATE = admit_dates[j],
            RESULT_TIME = sprintf("%02d:%02d", sample(0:23, 1), sample(0:59, 1)),
            RESULT_NUM = lab_data$value[k],
            RESULT_QUAL = sample(c("POSITIVE", "NEGATIVE", "NI", "UN", "OT"), 1, prob = c(0.1, 0.1, 0.6, 0.1, 0.1)),
            RESULT_MODIFIER = sample(c("EQ", "GE", "GT", "LE", "LT", "NI"), 1, prob = c(0.8, 0.05, 0.05, 0.05, 0.03, 0.02)),
            RESULT_UNIT = lab_data$unit[k],
            NORM_RANGE_LOW = NA,
            NORM_RANGE_HIGH = NA,
            NORM_MODIFIER_LOW = "EQ",
            NORM_MODIFIER_HIGH = "EQ",
            ABN_IND = lab_data$abn_ind[k],
            SPECIMEN_SOURCE = sample(c("BLOOD", "URINE", "CSF", "OTHER", "NI", "UN", "OT"), 1, prob = c(0.5, 0.2, 0.05, 0.05, 0.1, 0.05, 0.05)),
            SPECIMEN_DATE = admit_dates[j],
            PRIORITY = sample(c("R", "S", "NI", "UN", "OT"), 1),
            RESULT_LOC = sample(c("L", "P", "NI", "UN", "OT"), 1),
            LAB_LOINC_SOURCE = sample(c("OD", "BI", "CL", "NI", "UN", "OT"), 1),
            LAB_RESULT_SOURCE = sample(c("OD", "BI", "CL", "NI", "UN", "OT"), 1),
            RAW_LAB_NAME = lab_data$name[k],
            RAW_LAB_CODE = lab_data$loinc[k],
            RAW_RESULT = as.character(lab_data$value[k]),
            RAW_UNIT = lab_data$unit[k],
            CDW_Source = sample(c("EPIC", "ALLSCRIPTS"), 1),
            UID = patid,
            stringsAsFactors = FALSE
          )
          lab_id <- lab_id + 1
        }
      }

      # Profile-based procedures
      px_data <- generate_profile_procedures(profile, enc_type)
      if (!is.null(px_data) && nrow(px_data) > 0) {
        for (k in 1:nrow(px_data)) {
          px_source <- sample(c("OD", "BI", "CL", "NI", "UN", "OT"), 1)
          all_procedures[[px_id]] <- data.frame(
            PROCEDURESID = paste0("PX", sprintf("%010d", px_id)),
            PATID = patid,
            ENCOUNTERID = paste0("ENC", sprintf("%010d", enc_id)),
            PX = px_data$cpt[k],
            PX_TYPE = "CH",
            PX_DATE = admit_dates[j],
            PX_SOURCE = px_source,
            PPX = if (k == 1) "P" else "S",
            ADMIT_DATE = admit_dates[j],
            ENC_TYPE = enc_type,
            PROVIDERID = paste0("PROV", sprintf("%06d", sample(1:500, 1))),
            RAW_PX = px_data$cpt[k],
            RAW_PX_TYPE = "CPT",
            RAW_PX_NAME = px_data$name[k],
            CDW_Source = sample(c("EPIC", "ALLSCRIPTS"), 1),
            UID = patid,
            stringsAsFactors = FALSE
          )
          px_id <- px_id + 1
        }
      }

      # Profile-based vitals
      if (runif(1) < 0.9) {
        vital_data <- generate_profile_vitals(profile)
        bp_position <- sample(c("01", "02", "03", "NI", "UN", "OT"), 1)
        smoking <- sample(c("01", "02", "03", "04", "05", "06", "07", "08", "NI", "UN"), 1)
        tobacco <- sample(c("01", "02", "03", "04", "06", "NI", "UN"), 1)
        tobacco_type <- sample(c("01", "02", "03", "04", "05", "NI", "UN"), 1)
        all_vitals[[vital_id]] <- data.frame(
          VITALID = paste0("VIT", sprintf("%010d", vital_id)),
          PATID = patid,
          ENCOUNTERID = paste0("ENC", sprintf("%010d", enc_id)),
          MEASURE_DATE = admit_dates[j],
          MEASURE_TIME = sprintf("%02d:%02d", sample(0:23, 1), sample(0:59, 1)),
          VITAL_SOURCE = sample(c("PR", "PD", "HC", "HD", "NI", "UN", "OT"), 1),
          HT = vital_data$ht,
          WT = vital_data$wt,
          ORIGINAL_BMI = vital_data$bmi,
          SYSTOLIC = vital_data$systolic,
          DIASTOLIC = vital_data$diastolic,
          BP_POSITION = bp_position,
          SMOKING = smoking,
          TOBACCO = tobacco,
          TOBACCO_TYPE = tobacco_type,
          RAW_SYSTOLIC = as.character(vital_data$systolic),
          RAW_DIASTOLIC = as.character(vital_data$diastolic),
          RAW_BP_POSITION = bp_position,
          RAW_SMOKING = smoking,
          RAW_TOBACCO = tobacco,
          RAW_TOBACCO_TYPE = tobacco_type,
          CDW_Source = sample(c("EPIC", "ALLSCRIPTS"), 1),
          UID = patid,
          stringsAsFactors = FALSE
        )
        vital_id <- vital_id + 1
      }

      # Profile-based medications
      if (!is.null(patient_medications) && nrow(patient_medications) > 0 && runif(1) < 0.6) {
        n_meds <- min(nrow(patient_medications), sample(1:3, 1))
        med_indices <- sample(1:nrow(patient_medications), n_meds)
        for (k in med_indices) {
          med <- patient_medications[k, ]
          days_supply <- sample(c(7, 14, 30, 60, 90), 1)
          refills <- sample(0:5, 1)
          quantity <- sample(c(30, 60, 90, 120), 1)
          frequency <- sample(c("QD", "BID", "TID", "QID", "PRN", "QHS"), 1)
          route <- sample(c("PO", "IV", "IM", "SC", "TOP", "INH", "NI", "UN", "OT"), 1)
          all_medications[[rx_id]] <- data.frame(
            PRESCRIBINGID = paste0("RX", sprintf("%010d", rx_id)),
            PATID = patid,
            ENCOUNTERID = paste0("ENC", sprintf("%010d", enc_id)),
            RX_PROVIDERID = paste0("PROV", sprintf("%06d", sample(1:500, 1))),
            RX_ORDER_DATE = admit_dates[j],
            RX_ORDER_TIME = sprintf("%02d:%02d", sample(0:23, 1), sample(0:59, 1)),
            RX_START_DATE = admit_dates[j],
            RX_END_DATE = admit_dates[j] + days_supply,
            RX_DAYS_SUPPLY = days_supply,
            RX_REFILLS = refills,
            RX_QUANTITY = quantity,
            RX_DOSE_ORDERED = as.character(med$dose),
            RX_DOSE_ORDERED_UNIT = med$unit,
            RX_DOSE_FORM = sample(c("TAB", "CAP", "SOL", "INJ", "CRE", "OIN", "NI", "UN", "OT"), 1),
            RX_FREQUENCY = frequency,
            RX_ROUTE = route,
            RX_BASIS = sample(c("01", "02", "NI", "UN", "OT"), 1),
            RX_PRN_FLAG = sample(c("Y", "N"), 1, prob = c(0.2, 0.8)),
            RX_DISPENSE_AS_WRITTEN = sample(c("Y", "N", "NI", "UN", "OT"), 1),
            RX_SOURCE = sample(c("OD", "BI", "CL", "NI", "UN", "OT"), 1),
            RXNORM_CUI = med$rxnorm,
            RAW_RX_MED_NAME = med$name,
            RAW_RX_FREQUENCY = frequency,
            RAW_RX_DOSE_ORDERED = as.character(med$dose),
            RAW_RX_DOSE_ORDERED_UNIT = med$unit,
            RAW_RX_ROUTE = route,
            RAW_RX_REFILLS = as.character(refills),
            RAW_RXNORM_CUI = as.character(med$rxnorm),
            RAW_RX_NDC = paste0(sample(10000:99999, 1), "-", sample(1000:9999, 1), "-", sample(10:99, 1)),
            CDW_Source = sample(c("EPIC", "ALLSCRIPTS"), 1),
            UID = patid,
            stringsAsFactors = FALSE
          )
          rx_id <- rx_id + 1
        }
      }

      enc_id <- enc_id + 1
    }

    if (i %% 50 == 0) cat("  Processed", i, "/", n_patients, "patients\n")
  }

  # Combine and write tables
  encounter <- do.call(rbind, all_encounters)
  diagnosis <- do.call(rbind, all_diagnoses)
  procedure <- if (length(all_procedures) > 0) do.call(rbind, all_procedures) else data.frame()
  lab_result_cm <- if (length(all_labs) > 0) do.call(rbind, all_labs) else data.frame()
  prescribing <- if (length(all_medications) > 0) do.call(rbind, all_medications) else data.frame()
  vital <- if (length(all_vitals) > 0) do.call(rbind, all_vitals) else data.frame()

  .write_table_batched(con_cdw, "ENCOUNTER", encounter, overwrite = overwrite, batch_size = batch_size)
  .write_table_batched(con_cdw, "DIAGNOSIS", diagnosis, overwrite = overwrite, batch_size = batch_size)
  if (nrow(procedure) > 0) .write_table_batched(con_cdw, "PROCEDURES", procedure, overwrite = overwrite, batch_size = batch_size)
  if (nrow(lab_result_cm) > 0) .write_table_batched(con_cdw, "LAB_RESULT_CM", lab_result_cm, overwrite = overwrite, batch_size = batch_size)
  if (nrow(prescribing) > 0) .write_table_batched(con_cdw, "PRESCRIBING", prescribing, overwrite = overwrite, batch_size = batch_size)
  if (nrow(vital) > 0) .write_table_batched(con_cdw, "VITAL", vital, overwrite = overwrite, batch_size = batch_size)

  # Provider table
  providers <- data.frame(
    PROVIDERID = paste0("PROV", sprintf("%06d", 1:500)),
    ProviderName = replicate(500, paste("Dr.", paste0(sample(LETTERS, 1),
                                                       paste(sample(letters, sample(4:8, 1)), collapse = "")))),
    PROVIDER_SPECIALTY_PRIMARY = sample(c("Internal Medicine", "Family Medicine", "Cardiology",
                                         "Endocrinology", "Emergency Medicine"), 500, replace = TRUE),
    GPC_FLAG = "Y",
    stringsAsFactors = FALSE
  )
  .write_table_batched(con_cdw, "PROVIDER", providers, overwrite = overwrite, batch_size = batch_size)

  cat("\nData written to SQL Server.\n")

  list(
    cdw = con_cdw,
    mpi = con_mpi,
    summary = list(
      n_patients = n_patients,
      n_encounters = nrow(encounter),
      n_diagnoses = nrow(diagnosis),
      n_procedures = nrow(procedure),
      n_labs = nrow(lab_result_cm),
      n_prescriptions = nrow(prescribing),
      n_vitals = nrow(vital),
      profiles = as.list(profile_counts)
    )
  )
}

# =============================================================================
# SYNTHEA GENERATION (mode = "synthea")
# =============================================================================

.generate_from_synthea <- function(synthea_dir, con_cdw, con_mpi, overwrite, batch_size) {
  result <- load_synthea_data(synthea_dir, con_cdw = con_cdw, con_mpi = con_mpi,
                              overwrite = overwrite, batch_size = batch_size)

  # Get counts for summary
  n_patients <- dbGetQuery(result$cdw, "SELECT COUNT(*) FROM DEMOGRAPHIC")[[1]]
  n_encounters <- tryCatch(
    dbGetQuery(result$cdw, "SELECT COUNT(*) FROM ENCOUNTER")[[1]],
    error = function(e) 0
  )

  result$summary <- list(
    n_patients = n_patients,
    n_encounters = n_encounters,
    source = "synthea"
  )

  result
}
