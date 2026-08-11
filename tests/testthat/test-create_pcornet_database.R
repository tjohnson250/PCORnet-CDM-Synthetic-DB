test_that("create_pcornet_database returns expected structure", {
  skip_if_no_sql_server()
  dbs <- create_test_db(n_patients = 10)
  on.exit(cleanup_test_db(dbs))

  expect_type(dbs, "list")
  expect_named(dbs, c("cdw", "mpi", "summary"))
  expect_true(DBI::dbIsValid(dbs$cdw))
  expect_true(DBI::dbIsValid(dbs$mpi))
})

test_that("create_pcornet_database generates correct patient count", {
  skip_if_no_sql_server()
  dbs <- create_test_db(n_patients = 25)
  on.exit(cleanup_test_db(dbs))

  count <- DBI::dbGetQuery(dbs$cdw, "SELECT COUNT(*) FROM DEMOGRAPHIC")[[1]]
  expect_equal(count, 25)
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
  skip_if_no_sql_server()
  dbs <- create_test_db(n_patients = 10, mode = "random")
  on.exit(cleanup_test_db(dbs))

  cdw_tables <- DBI::dbListTables(dbs$cdw)
  expect_true("DEMOGRAPHIC" %in% cdw_tables)
  expect_true("ENCOUNTER" %in% cdw_tables)
  expect_true("DIAGNOSIS" %in% cdw_tables)

  mpi_tables <- DBI::dbListTables(dbs$mpi)
  expect_true("EnterpriseRecords" %in% mpi_tables)
  expect_true("Mpi" %in% mpi_tables)
})

test_that("enhanced mode generates clinical profiles", {
  skip_if_no_sql_server()
  dbs <- create_test_db(n_patients = 50, mode = "enhanced")
  on.exit(cleanup_test_db(dbs))

  profiles <- DBI::dbGetQuery(dbs$cdw, "SELECT DISTINCT CLINICAL_PROFILE FROM DEMOGRAPHIC")[[1]]
  expect_true(length(profiles) >= 1)
})

test_that("seed produces reproducible results", {
  skip_if_no_sql_server()
  dbs1 <- create_test_db(n_patients = 10, seed = 123)
  dbs2 <- create_test_db(n_patients = 10, seed = 123)
  on.exit({
    cleanup_test_db(dbs1)
    cleanup_test_db(dbs2)
  })

  demo1 <- DBI::dbGetQuery(dbs1$cdw, "SELECT PATID, BIRTH_DATE FROM DEMOGRAPHIC ORDER BY PATID")
  demo2 <- DBI::dbGetQuery(dbs2$cdw, "SELECT PATID, BIRTH_DATE FROM DEMOGRAPHIC ORDER BY PATID")

  expect_equal(demo1, demo2)
})

test_that("custom sources are applied", {
  skip_if_no_sql_server()
  custom_sources <- list(
    CUSTOM1 = list(id_field = "CUSTOM1_ID", description = "Custom System 1", null_rate = 0.1)
  )
  dbs <- create_test_db(n_patients = 10, sources = custom_sources)
  on.exit(cleanup_test_db(dbs))

  mpi_src <- DBI::dbGetQuery(dbs$mpi, "SELECT * FROM MPI_Src")
  expect_equal(nrow(mpi_src), 1)
  expect_equal(mpi_src$SRC[1], "CUSTOM1")
})
