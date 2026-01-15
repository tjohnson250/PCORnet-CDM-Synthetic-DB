test_that("create_pcornet_database returns expected structure", {
  dbs <- create_pcornet_database(n_patients = 10, save_to_disk = FALSE)

  expect_type(dbs, "list")
  expect_named(dbs, c("cdw", "mpi", "summary"))
  expect_s4_class(dbs$cdw, "duckdb_connection")
  expect_s4_class(dbs$mpi, "duckdb_connection")

  # Cleanup

  DBI::dbDisconnect(dbs$cdw, shutdown = TRUE)
  DBI::dbDisconnect(dbs$mpi, shutdown = TRUE)
})

test_that("create_pcornet_database generates correct patient count", {
  dbs <- create_pcornet_database(n_patients = 25, save_to_disk = FALSE)
  count <- DBI::dbGetQuery(dbs$cdw, "SELECT COUNT(*) FROM DEMOGRAPHIC")[[1]]
  expect_equal(count, 25)

  DBI::dbDisconnect(dbs$cdw, shutdown = TRUE)
  DBI::dbDisconnect(dbs$mpi, shutdown = TRUE)
})

test_that("create_pcornet_database validates n_patients", {
  expect_error(create_pcornet_database(n_patients = 0))
  expect_error(create_pcornet_database(n_patients = -1))
})

test_that("create_pcornet_database validates mode", {
  expect_error(create_pcornet_database(mode = "invalid"))
})

test_that("create_pcornet_database validates synthea_dir", {
  expect_error(create_pcornet_database(mode = "synthea"))
  expect_error(create_pcornet_database(mode = "synthea", synthea_dir = "/nonexistent"))
})

test_that("random mode generates all expected tables", {
  dbs <- create_pcornet_database(n_patients = 10, mode = "random", save_to_disk = FALSE)

  cdw_tables <- DBI::dbListTables(dbs$cdw)
  expect_true("DEMOGRAPHIC" %in% cdw_tables)
  expect_true("ENCOUNTER" %in% cdw_tables)
  expect_true("DIAGNOSIS" %in% cdw_tables)

  mpi_tables <- DBI::dbListTables(dbs$mpi)
  expect_true("EnterpriseRecords" %in% mpi_tables)
  expect_true("Mpi" %in% mpi_tables)

  DBI::dbDisconnect(dbs$cdw, shutdown = TRUE)
  DBI::dbDisconnect(dbs$mpi, shutdown = TRUE)
})

test_that("enhanced mode generates clinical profiles", {
  dbs <- create_pcornet_database(n_patients = 50, mode = "enhanced", save_to_disk = FALSE)

  profiles <- DBI::dbGetQuery(dbs$cdw, "SELECT DISTINCT CLINICAL_PROFILE FROM DEMOGRAPHIC")[[1]]
  expect_true(length(profiles) >= 1)

  DBI::dbDisconnect(dbs$cdw, shutdown = TRUE)
  DBI::dbDisconnect(dbs$mpi, shutdown = TRUE)
})

test_that("seed produces reproducible results", {
  dbs1 <- create_pcornet_database(n_patients = 10, seed = 123, save_to_disk = FALSE)
  dbs2 <- create_pcornet_database(n_patients = 10, seed = 123, save_to_disk = FALSE)

  demo1 <- DBI::dbGetQuery(dbs1$cdw, "SELECT PATID, BIRTH_DATE FROM DEMOGRAPHIC ORDER BY PATID")
  demo2 <- DBI::dbGetQuery(dbs2$cdw, "SELECT PATID, BIRTH_DATE FROM DEMOGRAPHIC ORDER BY PATID")

  expect_equal(demo1, demo2)

  DBI::dbDisconnect(dbs1$cdw, shutdown = TRUE)
  DBI::dbDisconnect(dbs1$mpi, shutdown = TRUE)
  DBI::dbDisconnect(dbs2$cdw, shutdown = TRUE)
  DBI::dbDisconnect(dbs2$mpi, shutdown = TRUE)
})

test_that("custom sources are applied", {
  custom_sources <- list(
    CUSTOM1 = list(id_field = "CUSTOM1_ID", description = "Custom System 1", null_rate = 0.1)
  )
  dbs <- create_pcornet_database(n_patients = 10, sources = custom_sources, save_to_disk = FALSE)

  mpi_src <- DBI::dbGetQuery(dbs$mpi, "SELECT * FROM MPI_Src")
  expect_equal(nrow(mpi_src), 1)
  expect_equal(mpi_src$SRC[1], "CUSTOM1")

  DBI::dbDisconnect(dbs$cdw, shutdown = TRUE)
  DBI::dbDisconnect(dbs$mpi, shutdown = TRUE)
})
