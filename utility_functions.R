# Utility Functions for Working with PCORnet CDM and MPI Databases

library(DBI)

#' Print First 10 Rows of All Tables in Both Databases
#'
#' @param con_cdw Connection to the CDW database
#' @param con_mpi Connection to the MPI database
#' @param n Number of rows to display (default: 10)
print_all_tables <- function(con_cdw, con_mpi, n = 10) {

  cat(strrep("=", 80), "\n")
  cat("CDW DATABASE TABLES\n")
  cat(strrep("=", 80), "\n\n")

  cdw_tables <- dbListTables(con_cdw)
  for (table in cdw_tables) {
    cat("\n")
    cat(strrep("-", 80), "\n")
    cat("Table:", table, "\n")
    cat(strrep("-", 80), "\n")

    query <- sprintf("SELECT * FROM %s LIMIT %d", table, n)
    data <- dbGetQuery(con_cdw, query)

    cat("Rows:", nrow(data), "of", dbGetQuery(con_cdw, sprintf("SELECT COUNT(*) FROM %s", table))[[1]], "\n")
    cat("Columns:", ncol(data), "\n\n")

    print(data)
    cat("\n")
  }

  cat("\n")
  cat(strrep("=", 80), "\n")
  cat("MPI DATABASE TABLES\n")
  cat(strrep("=", 80), "\n\n")

  mpi_tables <- dbListTables(con_mpi)
  for (table in mpi_tables) {
    cat("\n")
    cat(strrep("-", 80), "\n")
    cat("Table:", table, "\n")
    cat(strrep("-", 80), "\n")

    query <- sprintf("SELECT * FROM %s LIMIT %d", table, n)
    data <- dbGetQuery(con_mpi, query)

    cat("Rows:", nrow(data), "of", dbGetQuery(con_mpi, sprintf("SELECT COUNT(*) FROM %s", table))[[1]], "\n")
    cat("Columns:", ncol(data), "\n\n")

    print(data)
    cat("\n")
  }

  invisible(NULL)
}


#' Print Summary of All Tables in Both Databases
#'
#' @param con_cdw Connection to the CDW database
#' @param con_mpi Connection to the MPI database
print_table_summary <- function(con_cdw, con_mpi) {

  cat(strrep("=", 80), "\n")
  cat("DATABASE SUMMARY\n")
  cat(strrep("=", 80), "\n\n")

  cat("CDW DATABASE:\n")
  cdw_tables <- dbListTables(con_cdw)
  for (table in cdw_tables) {
    count <- dbGetQuery(con_cdw, sprintf("SELECT COUNT(*) FROM %s", table))[[1]]
    cols <- ncol(dbGetQuery(con_cdw, sprintf("SELECT * FROM %s LIMIT 1", table)))
    cat(sprintf("  %-20s %8d rows, %3d columns\n", table, count, cols))
  }

  cat("\nMPI DATABASE:\n")
  mpi_tables <- dbListTables(con_mpi)
  for (table in mpi_tables) {
    count <- dbGetQuery(con_mpi, sprintf("SELECT COUNT(*) FROM %s", table))[[1]]
    cols <- ncol(dbGetQuery(con_mpi, sprintf("SELECT * FROM %s LIMIT 1", table)))
    cat(sprintf("  %-20s %8d rows, %3d columns\n", table, count, cols))
  }

  invisible(NULL)
}
