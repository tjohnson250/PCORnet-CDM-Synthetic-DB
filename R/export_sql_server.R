# SQL Server Export Functions

#' Export PCORnet databases to SQL Server
#'
#' Transfers CDW and MPI tables from DuckDB to a Microsoft SQL Server database.
#'
#' @param dbs List containing `$cdw` and `$mpi` database connections (from
#'   `create_pcornet_database()` or `load_pcornet_database()`)
#' @param server SQL Server hostname (e.g., "localhost" or "server.database.windows.net")
#' @param database Target database name
#' @param uid Username for SQL Server authentication
#' @param pwd Password for SQL Server authentication
#' @param driver ODBC driver name (default: "ODBC Driver 17 for SQL Server")
#' @param port SQL Server port (default: 1433)
#' @param schema Target schema for tables (default: "dbo")
#' @param overwrite If TRUE, overwrite existing tables. If FALSE, fail if tables exist.
#'   (default: FALSE)
#' @param tables Character vector of specific tables to export, or NULL for all tables
#'   (default: NULL)
#' @param include_cdw Export CDW tables (default: TRUE)
#' @param include_mpi Export MPI tables (default: TRUE)
#' @param trusted_connection Use Windows integrated authentication instead of uid/pwd
#'   (default: FALSE)
#' @param connection_string Optional full ODBC connection string. If provided, overrides
#'   individual connection parameters.
#' @param batch_size Number of rows to write at a time for large tables (default: 10000)
#' @param verbose Print progress messages (default: TRUE)
#'
#' @return Invisible list with export statistics (tables exported, row counts, any errors)
#'
#' @details
#' Requires the `odbc` package to be installed. Install the Microsoft ODBC Driver:
#' \itemize{
#'   \item Windows: Download from Microsoft
#'   \item Mac: `brew install microsoft/mssql-release/msodbcsql17`
#'   \item Linux: See Microsoft documentation for your distribution
#' }
#'
#' @examples
#' \dontrun{
#' # Generate synthetic data
#' dbs <- create_pcornet_database(n_patients = 100)
#'
#' # Export to SQL Server with username/password
#' export_to_sql_server(
#'   dbs,
#'   server = "localhost",
#'   database = "PCORnet_Dev",
#'   uid = "sa",
#'   pwd = "YourPassword123"
#' )
#'
#' # Export with Windows authentication
#' export_to_sql_server(
#'   dbs,
#'   server = "localhost",
#'   database = "PCORnet_Dev",
#'   trusted_connection = TRUE
#' )
#'
#' # Export only specific tables
#' export_to_sql_server(
#'   dbs,
#'   server = "localhost",
#'   database = "PCORnet_Dev",
#'   uid = "sa",
#'   pwd = "YourPassword123",
#'   tables = c("DEMOGRAPHIC", "ENCOUNTER", "DIAGNOSIS")
#' )
#'
#' # Export to Azure SQL Database
#' export_to_sql_server(
#'   dbs,
#'   server = "myserver.database.windows.net",
#'   database = "PCORnet_Dev",
#'   uid = "admin@myserver",
#'   pwd = "YourPassword123"
#' )
#' }
#'
#' @export
export_to_sql_server <- function(
    dbs,
    server,
    database,
    uid = NULL,
    pwd = NULL,
    driver = "ODBC Driver 17 for SQL Server",
    port = 1433,
    schema = "dbo",
    overwrite = FALSE,
    tables = NULL,
    include_cdw = TRUE,
    include_mpi = TRUE,
    trusted_connection = FALSE,
    connection_string = NULL,
    batch_size = 10000,
    verbose = TRUE
) {

  # Check for odbc package

if (!requireNamespace("odbc", quietly = TRUE)) {
    stop("The 'odbc' package is required for SQL Server export. ",
         "Install it with: install.packages('odbc')")
  }

  # Validate inputs
  if (is.null(dbs) || !is.list(dbs)) {
    stop("dbs must be a list with $cdw and/or $mpi database connections")
  }

  if (include_cdw && is.null(dbs$cdw)) {
    stop("dbs$cdw is NULL but include_cdw is TRUE")
  }

  if (include_mpi && is.null(dbs$mpi)) {
    stop("dbs$mpi is NULL but include_mpi is TRUE")
  }

  if (!trusted_connection && (is.null(uid) || is.null(pwd)) && is.null(connection_string)) {
    stop("Either provide uid/pwd, set trusted_connection = TRUE, or provide connection_string")
  }

  # Build connection string if not provided
  if (is.null(connection_string)) {
    if (trusted_connection) {
      connection_string <- sprintf(
        "Driver={%s};Server=%s,%d;Database=%s;Trusted_Connection=yes;",
        driver, server, port, database
      )
    } else {
      connection_string <- sprintf(
        "Driver={%s};Server=%s,%d;Database=%s;Uid=%s;Pwd=%s;",
        driver, server, port, database, uid, pwd
      )
    }
  }

  # Connect to SQL Server
  if (verbose) cat("Connecting to SQL Server...\n")

  sql_con <- tryCatch({
    DBI::dbConnect(odbc::odbc(), .connection_string = connection_string)
  }, error = function(e) {
    stop("Failed to connect to SQL Server: ", e$message, "\n",
         "Make sure the ODBC driver is installed and connection parameters are correct.")
  })

  on.exit(DBI::dbDisconnect(sql_con), add = TRUE)

  if (verbose) cat("Connected to", database, "on", server, "\n\n")

  # Track results
  results <- list(
    cdw_tables = list(),
    mpi_tables = list(),
    errors = list()
  )

  # Helper function to export a single table
  export_table <- function(source_con, table_name, source_name) {
    # Check if table should be exported
    if (!is.null(tables) && !(table_name %in% tables)) {
      return(NULL)
    }

    full_table_name <- if (schema != "dbo") {
      paste0(schema, ".", table_name)
    } else {
      table_name
    }

    if (verbose) cat(sprintf("  Exporting %s.%s...", source_name, table_name))

    tryCatch({
      # Read data from source
      data <- DBI::dbReadTable(source_con, table_name)
      n_rows <- nrow(data)

      # Check if table exists
      table_exists <- DBI::dbExistsTable(sql_con, table_name)

      if (table_exists && !overwrite) {
        stop("Table already exists. Set overwrite = TRUE to replace it.")
      }

      # Write to SQL Server
      if (n_rows <= batch_size) {
        # Small table - write in one go
        DBI::dbWriteTable(sql_con, table_name, data, overwrite = overwrite)
      } else {
        # Large table - write in batches
        if (overwrite && table_exists) {
          DBI::dbRemoveTable(sql_con, table_name)
        }

        # Write first batch to create table
        DBI::dbWriteTable(sql_con, table_name, data[1:batch_size, ], overwrite = FALSE)

        # Append remaining batches
        for (i in seq(batch_size + 1, n_rows, by = batch_size)) {
          end_row <- min(i + batch_size - 1, n_rows)
          DBI::dbAppendTable(sql_con, table_name, data[i:end_row, ])
        }
      }

      if (verbose) cat(sprintf(" %d rows\n", n_rows))

      return(list(table = table_name, rows = n_rows, status = "success"))

    }, error = function(e) {
      if (verbose) cat(sprintf(" ERROR: %s\n", e$message))
      return(list(table = table_name, rows = 0, status = "error", message = e$message))
    })
  }

  # Export CDW tables
  if (include_cdw && !is.null(dbs$cdw)) {
    if (verbose) cat("Exporting CDW tables:\n")
    cdw_tables <- DBI::dbListTables(dbs$cdw)

    for (table in cdw_tables) {
      result <- export_table(dbs$cdw, table, "CDW")
      if (!is.null(result)) {
        results$cdw_tables[[table]] <- result
        if (result$status == "error") {
          results$errors[[paste0("CDW.", table)]] <- result$message
        }
      }
    }
    if (verbose) cat("\n")
  }

  # Export MPI tables
  if (include_mpi && !is.null(dbs$mpi)) {
    if (verbose) cat("Exporting MPI tables:\n")
    mpi_tables <- DBI::dbListTables(dbs$mpi)

    for (table in mpi_tables) {
      result <- export_table(dbs$mpi, table, "MPI")
      if (!is.null(result)) {
        results$mpi_tables[[table]] <- result
        if (result$status == "error") {
          results$errors[[paste0("MPI.", table)]] <- result$message
        }
      }
    }
    if (verbose) cat("\n")
  }

  # Summary
  cdw_success <- sum(sapply(results$cdw_tables, function(x) x$status == "success"))
  mpi_success <- sum(sapply(results$mpi_tables, function(x) x$status == "success"))
  total_rows <- sum(sapply(c(results$cdw_tables, results$mpi_tables),
                           function(x) if (x$status == "success") x$rows else 0))

  if (verbose) {
    cat("=== Export Complete ===\n")
    cat(sprintf("CDW tables exported: %d\n", cdw_success))
    cat(sprintf("MPI tables exported: %d\n", mpi_success))
    cat(sprintf("Total rows: %d\n", total_rows))

    if (length(results$errors) > 0) {
      cat(sprintf("\nErrors: %d\n", length(results$errors)))
      for (name in names(results$errors)) {
        cat(sprintf("  %s: %s\n", name, results$errors[[name]]))
      }
    }
  }

  invisible(results)
}


#' Export PCORnet databases to CSV files
#'
#' Exports all CDW and MPI tables to CSV files for import into other systems.
#'
#' @param dbs List containing `$cdw` and `$mpi` database connections
#' @param output_dir Directory to write CSV files (will be created if it doesn't exist)
#' @param include_cdw Export CDW tables (default: TRUE)
#' @param include_mpi Export MPI tables (default: TRUE)
#' @param tables Character vector of specific tables to export, or NULL for all
#' @param verbose Print progress messages (default: TRUE)
#'
#' @return Invisible character vector of exported file paths
#'
#' @examples
#' \dontrun{
#' dbs <- create_pcornet_database(n_patients = 100)
#' export_to_csv(dbs, output_dir = "pcornet_export")
#' }
#'
#' @export
export_to_csv <- function(
    dbs,
    output_dir,
    include_cdw = TRUE,
    include_mpi = TRUE,
    tables = NULL,
    verbose = TRUE
) {

  # Create output directory
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  exported_files <- character()

  # Export CDW tables
  if (include_cdw && !is.null(dbs$cdw)) {
    cdw_dir <- file.path(output_dir, "cdw")
    dir.create(cdw_dir, showWarnings = FALSE)

    if (verbose) cat("Exporting CDW tables to", cdw_dir, "\n")

    for (table in DBI::dbListTables(dbs$cdw)) {
      if (!is.null(tables) && !(table %in% tables)) next

      file_path <- file.path(cdw_dir, paste0(table, ".csv"))
      data <- DBI::dbReadTable(dbs$cdw, table)
      write.csv(data, file_path, row.names = FALSE)
      exported_files <- c(exported_files, file_path)

      if (verbose) cat(sprintf("  %s: %d rows\n", table, nrow(data)))
    }
  }

  # Export MPI tables
  if (include_mpi && !is.null(dbs$mpi)) {
    mpi_dir <- file.path(output_dir, "mpi")
    dir.create(mpi_dir, showWarnings = FALSE)

    if (verbose) cat("\nExporting MPI tables to", mpi_dir, "\n")

    for (table in DBI::dbListTables(dbs$mpi)) {
      if (!is.null(tables) && !(table %in% tables)) next

      file_path <- file.path(mpi_dir, paste0(table, ".csv"))
      data <- DBI::dbReadTable(dbs$mpi, table)
      write.csv(data, file_path, row.names = FALSE)
      exported_files <- c(exported_files, file_path)

      if (verbose) cat(sprintf("  %s: %d rows\n", table, nrow(data)))
    }
  }

  if (verbose) {
    cat("\n=== Export Complete ===\n")
    cat(sprintf("Files written to: %s\n", normalizePath(output_dir)))
    cat(sprintf("Total files: %d\n", length(exported_files)))
  }

  invisible(exported_files)
}
