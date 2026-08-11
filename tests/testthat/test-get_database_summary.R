test_that("get_database_summary returns correct structure", {
  skip_if_no_sql_server()
  dbs <- create_test_db(n_patients = 10)
  on.exit(cleanup_test_db(dbs))

  summary <- get_database_summary(dbs$cdw, dbs$mpi)

  expect_s3_class(summary, "pcornet_summary")
  expect_true("cdw" %in% names(summary))
  expect_true("mpi" %in% names(summary))
  expect_true("table" %in% names(summary$cdw))
  expect_true("rows" %in% names(summary$cdw))
})

test_that("get_database_summary works without MPI", {
  skip_if_no_sql_server()
  dbs <- create_test_db(n_patients = 10)
  on.exit(cleanup_test_db(dbs))

  summary <- get_database_summary(dbs$cdw)

  expect_s3_class(summary, "pcornet_summary")
  expect_null(summary$mpi)
})

test_that("print.pcornet_summary produces output", {
  skip_if_no_sql_server()
  dbs <- create_test_db(n_patients = 10)
  on.exit(cleanup_test_db(dbs))

  summary <- get_database_summary(dbs$cdw, dbs$mpi)

  expect_output(print(summary), "PCORnet Database Summary")
  expect_output(print(summary), "CDW Tables")
})
