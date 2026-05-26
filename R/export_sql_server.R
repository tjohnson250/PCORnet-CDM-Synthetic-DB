# Export Functions

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
#' dbs <- create_pcornet_database(
#'   n_patients = 100,
#'   server = "localhost",
#'   uid = "sa",
#'   pwd = "YourPassword123"
#' )
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
