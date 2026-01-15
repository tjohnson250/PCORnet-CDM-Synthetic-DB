test_that("get_database_summary returns correct structure", {
  dbs <- create_pcornet_database(n_patients = 10, save_to_disk = FALSE)
  summary <- get_database_summary(dbs$cdw, dbs$mpi)

  expect_s3_class(summary, "pcornet_summary")
  expect_true("cdw" %in% names(summary))
  expect_true("mpi" %in% names(summary))
  expect_true("table" %in% names(summary$cdw))
  expect_true("rows" %in% names(summary$cdw))

  DBI::dbDisconnect(dbs$cdw, shutdown = TRUE)
  DBI::dbDisconnect(dbs$mpi, shutdown = TRUE)
})

test_that("get_database_summary works without MPI", {
  dbs <- create_pcornet_database(n_patients = 10, save_to_disk = FALSE)
  summary <- get_database_summary(dbs$cdw)

  expect_s3_class(summary, "pcornet_summary")
  expect_null(summary$mpi)

  DBI::dbDisconnect(dbs$cdw, shutdown = TRUE)
  DBI::dbDisconnect(dbs$mpi, shutdown = TRUE)
})

test_that("print.pcornet_summary produces output", {
  dbs <- create_pcornet_database(n_patients = 10, save_to_disk = FALSE)
  summary <- get_database_summary(dbs$cdw, dbs$mpi)

  expect_output(print(summary), "PCORnet Database Summary")
  expect_output(print(summary), "CDW Tables")

  DBI::dbDisconnect(dbs$cdw, shutdown = TRUE)
  DBI::dbDisconnect(dbs$mpi, shutdown = TRUE)
})
