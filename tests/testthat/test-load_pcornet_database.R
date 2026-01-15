test_that("load_pcornet_database errors on missing files", {
  expect_error(load_pcornet_database(cdw_path = "nonexistent.duckdb"))
  expect_error(load_pcornet_database(mpi_path = "nonexistent.duckdb"))
})

test_that("load_pcornet_database loads existing databases", {
  # First create databases
  dbs <- create_pcornet_database(n_patients = 5, save_to_disk = TRUE, output_dir = tempdir())

  DBI::dbDisconnect(dbs$cdw, shutdown = TRUE)
  DBI::dbDisconnect(dbs$mpi, shutdown = TRUE)

  # Now load them
  cdw_path <- file.path(tempdir(), "pcornet_cdw.duckdb")
  mpi_path <- file.path(tempdir(), "mpi.duckdb")

  loaded <- load_pcornet_database(cdw_path = cdw_path, mpi_path = mpi_path)

  expect_type(loaded, "list")
  expect_named(loaded, c("cdw", "mpi"))
  expect_s4_class(loaded$cdw, "duckdb_connection")

  DBI::dbDisconnect(loaded$cdw, shutdown = TRUE)
  DBI::dbDisconnect(loaded$mpi, shutdown = TRUE)

  # Cleanup
  unlink(cdw_path)
  unlink(mpi_path)
})
