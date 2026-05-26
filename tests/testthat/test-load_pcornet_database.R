test_that("load_pcornet_database errors on bad credentials", {
  skip_if_no_sql_server()
  expect_error(load_pcornet_database(
    server = "nonexistent.server.example.com",
    uid = "bad_user",
    pwd = "bad_pass"
  ))
})

test_that("load_pcornet_database connects to existing databases", {
  skip_if_no_sql_server()
  # First create databases
  dbs <- create_test_db(n_patients = 5)
  cleanup_test_db(dbs)

  # Now load them
  params <- test_sql_params()
  loaded <- load_pcornet_database(
    server = params$server,
    cdw_database = params$cdw_database,
    mpi_database = params$mpi_database,
    uid = params$uid,
    pwd = params$pwd,
    trusted_connection = params$trusted_connection
  )
  on.exit(cleanup_test_db(loaded))

  expect_type(loaded, "list")
  expect_named(loaded, c("cdw", "mpi"))
  expect_true(DBI::dbIsValid(loaded$cdw))
  expect_true(DBI::dbIsValid(loaded$mpi))
})
