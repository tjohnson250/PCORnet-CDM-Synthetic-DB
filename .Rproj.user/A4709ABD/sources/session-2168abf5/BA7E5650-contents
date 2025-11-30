# Load Saved PCORnet CDM and MPI Databases
# Connects to previously generated DuckDB database files

library(DBI)
library(duckdb)

# Load utility functions
source("utility_functions.R")

cat("Loading saved databases...\n")

# Connect to saved database files
con_cdw <- dbConnect(duckdb::duckdb(), dbdir = "pcornet_cdw.duckdb")
con_mpi <- dbConnect(duckdb::duckdb(), dbdir = "mpi.duckdb")

# Verify tables exist
cat("\nCDW Tables:\n")
print(dbListTables(con_cdw))

cat("\nMPI Tables:\n")
print(dbListTables(con_mpi))

# Summary statistics
cat("\n=== Database Summary ===\n")
cat("CDW Database:\n")
cat("  Patients:", dbGetQuery(con_cdw, "SELECT COUNT(*) FROM DEMOGRAPHIC")[[1]], "\n")
cat("  Encounters:", dbGetQuery(con_cdw, "SELECT COUNT(*) FROM ENCOUNTER")[[1]], "\n")
cat("  Diagnoses:", dbGetQuery(con_cdw, "SELECT COUNT(*) FROM DIAGNOSIS")[[1]], "\n")

cat("\nMPI Database:\n")
cat("  Patients:", dbGetQuery(con_mpi, "SELECT COUNT(*) FROM EnterpriseRecords")[[1]], "\n")
cat("  MPI mappings:", dbGetQuery(con_mpi, "SELECT COUNT(*) FROM Mpi")[[1]], "\n")

cat("\nDatabases are ready to use:\n")
cat("  con_cdw - Clinical Data Warehouse\n")
cat("  con_mpi - Master Patient Index\n")

cat("\nUtility functions available:\n")
cat("  print_all_tables(con_cdw, con_mpi)    - View first 10 rows of all tables\n")
cat("  print_table_summary(con_cdw, con_mpi) - View row/column counts for all tables\n")

# Return the connections
list(cdw = con_cdw, mpi = con_mpi)
