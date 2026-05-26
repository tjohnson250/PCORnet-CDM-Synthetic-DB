# Test helper: SQL Server connection for tests
# Set these environment variables to enable integration tests:
#   PCORNET_TEST_SERVER, PCORNET_TEST_UID, PCORNET_TEST_PWD
# Or set PCORNET_TEST_TRUSTED=TRUE for Windows auth

skip_if_no_sql_server <- function() {
  server <- Sys.getenv("PCORNET_TEST_SERVER", "")
  trusted <- Sys.getenv("PCORNET_TEST_TRUSTED", "") == "TRUE"
  uid <- Sys.getenv("PCORNET_TEST_UID", "")
  pwd <- Sys.getenv("PCORNET_TEST_PWD", "")

  if (server == "") {
    skip("SQL Server not configured (set PCORNET_TEST_SERVER)")
  }
  if (!trusted && (uid == "" || pwd == "")) {
    skip("SQL Server credentials not configured (set PCORNET_TEST_UID/PWD or PCORNET_TEST_TRUSTED=TRUE)")
  }
}

test_sql_params <- function() {
  list(
    server = Sys.getenv("PCORNET_TEST_SERVER"),
    uid = Sys.getenv("PCORNET_TEST_UID", unset = NULL),
    pwd = Sys.getenv("PCORNET_TEST_PWD", unset = NULL),
    trusted_connection = Sys.getenv("PCORNET_TEST_TRUSTED", "") == "TRUE",
    cdw_database = Sys.getenv("PCORNET_TEST_CDW_DB", "PCORnet_CDW_Test"),
    mpi_database = Sys.getenv("PCORNET_TEST_MPI_DB", "MPI_Test")
  )
}

create_test_db <- function(...) {
  params <- test_sql_params()
  create_pcornet_database(
    server = params$server,
    cdw_database = params$cdw_database,
    mpi_database = params$mpi_database,
    uid = params$uid,
    pwd = params$pwd,
    trusted_connection = params$trusted_connection,
    ...
  )
}

cleanup_test_db <- function(dbs) {
  tryCatch(DBI::dbDisconnect(dbs$cdw), error = function(e) NULL)
  tryCatch(DBI::dbDisconnect(dbs$mpi), error = function(e) NULL)
}
