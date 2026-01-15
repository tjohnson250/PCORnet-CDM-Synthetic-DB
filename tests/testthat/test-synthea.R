# Tests for Synthea import functionality

test_that("synthea mode imports patients correctly", {
  synthea_dir <- testthat::test_path("fixtures", "synthea")
  skip_if_not(dir.exists(synthea_dir), "Synthea fixtures not found")

  dbs <- create_pcornet_database(
    mode = "synthea",
    synthea_dir = synthea_dir,
    save_to_disk = FALSE
  )

  expect_type(dbs, "list")
  expect_named(dbs, c("cdw", "mpi", "summary"))

  # Verify patient count
  patient_count <- DBI::dbGetQuery(dbs$cdw, "SELECT COUNT(*) FROM DEMOGRAPHIC")[[1]]
  expect_equal(patient_count, 3)

  # Verify MPI records
  mpi_count <- DBI::dbGetQuery(dbs$mpi, "SELECT COUNT(*) FROM EnterpriseRecords")[[1]]
  expect_equal(mpi_count, 3)
})

test_that("synthea mode imports patient demographics correctly", {
  synthea_dir <- testthat::test_path("fixtures", "synthea")
  skip_if_not(dir.exists(synthea_dir), "Synthea fixtures not found")

  dbs <- create_pcornet_database(
    mode = "synthea",
    synthea_dir = synthea_dir,
    save_to_disk = FALSE
  )

  # Check specific patient data
  john <- DBI::dbGetQuery(dbs$cdw, "SELECT * FROM DEMOGRAPHIC WHERE firstName = 'John'")
  expect_equal(nrow(john), 1)
  expect_equal(john$lastName, "Smith")
  expect_equal(john$SEX, "M")
  expect_equal(as.character(john$BIRTH_DATE), "1980-05-15")

  # Check deceased patient
  jane <- DBI::dbGetQuery(dbs$cdw, "SELECT * FROM DEMOGRAPHIC WHERE firstName = 'Jane'")
  expect_equal(jane$isDeceased, "Y")
})

test_that("synthea mode creates death records for deceased patients", {
  synthea_dir <- testthat::test_path("fixtures", "synthea")
  skip_if_not(dir.exists(synthea_dir), "Synthea fixtures not found")

  dbs <- create_pcornet_database(
    mode = "synthea",
    synthea_dir = synthea_dir,
    save_to_disk = FALSE
  )

  # Only patient-002 (Jane) is deceased
  death_count <- DBI::dbGetQuery(dbs$cdw, "SELECT COUNT(*) FROM DEATH")[[1]]
  expect_equal(death_count, 1)

  death_record <- DBI::dbGetQuery(dbs$cdw, "SELECT * FROM DEATH")
  expect_equal(as.character(death_record$DEATH_DATE), "2023-06-10")
})

test_that("synthea mode imports encounters correctly", {
  synthea_dir <- testthat::test_path("fixtures", "synthea")
  skip_if_not(dir.exists(synthea_dir), "Synthea fixtures not found")

  dbs <- create_pcornet_database(
    mode = "synthea",
    synthea_dir = synthea_dir,
    save_to_disk = FALSE
  )

  # Should have 5 encounters
  enc_count <- DBI::dbGetQuery(dbs$cdw, "SELECT COUNT(*) FROM ENCOUNTER")[[1]]
  expect_equal(enc_count, 5)

  # Check encounter type mapping
  enc_types <- DBI::dbGetQuery(dbs$cdw, "SELECT ENC_TYPE, COUNT(*) as n FROM ENCOUNTER GROUP BY ENC_TYPE")
  expect_true("AV" %in% enc_types$ENC_TYPE)  # ambulatory/wellness -> AV
  expect_true("IP" %in% enc_types$ENC_TYPE)  # inpatient -> IP
  expect_true("ED" %in% enc_types$ENC_TYPE)  # emergency -> ED
})

test_that("synthea mode imports diagnoses correctly", {
  synthea_dir <- testthat::test_path("fixtures", "synthea")
  skip_if_not(dir.exists(synthea_dir), "Synthea fixtures not found")

  dbs <- create_pcornet_database(
    mode = "synthea",
    synthea_dir = synthea_dir,
    save_to_disk = FALSE
  )

  # Should have 5 diagnoses
  dx_count <- DBI::dbGetQuery(dbs$cdw, "SELECT COUNT(*) FROM DIAGNOSIS")[[1]]
  expect_equal(dx_count, 5)

  # Check SNOMED to ICD-10 mapping (diabetes: 44054006 -> E11.9)
  diabetes_dx <- DBI::dbGetQuery(dbs$cdw, "SELECT * FROM DIAGNOSIS WHERE SNOMED_CODE = '44054006'")
  expect_equal(nrow(diabetes_dx), 1)
  expect_equal(diabetes_dx$DX, "E11.9")
  expect_equal(diabetes_dx$DX_TYPE, "10")  # ICD-10
})

test_that("synthea mode imports medications correctly", {
  synthea_dir <- testthat::test_path("fixtures", "synthea")
  skip_if_not(dir.exists(synthea_dir), "Synthea fixtures not found")

  dbs <- create_pcornet_database(
    mode = "synthea",
    synthea_dir = synthea_dir,
    save_to_disk = FALSE
  )

  # Should have 3 medications
  rx_count <- DBI::dbGetQuery(dbs$cdw, "SELECT COUNT(*) FROM PRESCRIBING")[[1]]
  expect_equal(rx_count, 3)

  # Check metformin prescription
  metformin <- DBI::dbGetQuery(dbs$cdw, "SELECT * FROM PRESCRIBING WHERE RXNORM_CUI = '860975'")
  expect_equal(nrow(metformin), 1)
  expect_true(grepl("Metformin", metformin$RAW_RX_MED_NAME))
})

test_that("synthea mode imports procedures correctly", {
  synthea_dir <- testthat::test_path("fixtures", "synthea")
  skip_if_not(dir.exists(synthea_dir), "Synthea fixtures not found")

  dbs <- create_pcornet_database(
    mode = "synthea",
    synthea_dir = synthea_dir,
    save_to_disk = FALSE
  )

  # Should have 3 procedures
  px_count <- DBI::dbGetQuery(dbs$cdw, "SELECT COUNT(*) FROM PROCEDURES")[[1]]
  expect_equal(px_count, 3)
})

test_that("synthea mode imports labs and vitals correctly", {
  synthea_dir <- testthat::test_path("fixtures", "synthea")
  skip_if_not(dir.exists(synthea_dir), "Synthea fixtures not found")

  dbs <- create_pcornet_database(
    mode = "synthea",
    synthea_dir = synthea_dir,
    save_to_disk = FALSE
  )

  # Should have 1 lab (HbA1c - non-vital observation)
  lab_count <- DBI::dbGetQuery(dbs$cdw, "SELECT COUNT(*) FROM LAB_RESULT_CM")[[1]]
  expect_equal(lab_count, 1)

  # Check HbA1c lab
  hba1c <- DBI::dbGetQuery(dbs$cdw, "SELECT * FROM LAB_RESULT_CM WHERE LAB_LOINC = '4548-4'")
  expect_equal(nrow(hba1c), 1)
  expect_equal(hba1c$RESULT_NUM, 7.2)

  # Should have vitals (pivoted by encounter)
  vital_count <- DBI::dbGetQuery(dbs$cdw, "SELECT COUNT(*) FROM VITAL")[[1]]
  expect_true(vital_count > 0)
})

test_that("synthea mode creates MPI cross-reference correctly", {
  synthea_dir <- testthat::test_path("fixtures", "synthea")
  skip_if_not(dir.exists(synthea_dir), "Synthea fixtures not found")

  dbs <- create_pcornet_database(
    mode = "synthea",
    synthea_dir = synthea_dir,
    save_to_disk = FALSE
  )

  # Check MPI table
  mpi <- DBI::dbGetQuery(dbs$mpi, "SELECT * FROM Mpi")
  expect_equal(nrow(mpi), 3)
  expect_true(all(mpi$Src == "SYNTHEA"))

  # Check MPI_Src
  mpi_src <- DBI::dbGetQuery(dbs$mpi, "SELECT * FROM MPI_Src")
  expect_equal(nrow(mpi_src), 1)
  expect_equal(mpi_src$SRC, "SYNTHEA")
})

test_that("synthea mode links CDW and MPI via UID", {
  synthea_dir <- testthat::test_path("fixtures", "synthea")
  skip_if_not(dir.exists(synthea_dir), "Synthea fixtures not found")

  dbs <- create_pcornet_database(
    mode = "synthea",
    synthea_dir = synthea_dir,
    save_to_disk = FALSE
  )

  # Get UIDs from both databases
  cdw_uids <- DBI::dbGetQuery(dbs$cdw, "SELECT DISTINCT UID FROM DEMOGRAPHIC ORDER BY UID")[[1]]
  mpi_uids <- DBI::dbGetQuery(dbs$mpi, "SELECT DISTINCT Uid FROM EnterpriseRecords ORDER BY Uid")[[1]]

  expect_equal(cdw_uids, mpi_uids)
})
