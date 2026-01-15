# Test internal helper functions

test_that(".sample_with_na introduces expected NA rate", {
  # Access internal function using ::: operator
  sample_with_na <- pcornet.synthetic:::.sample_with_na

  set.seed(42)
  result <- sample_with_na(letters, 1000, prob_na = 0.2, replace = TRUE)
  na_rate <- sum(is.na(result)) / length(result)

  # Allow some variance in the NA rate
  expect_true(na_rate > 0.10 && na_rate < 0.30)
})

test_that(".sample_with_na with prob_na = 0 produces no NAs", {
  sample_with_na <- pcornet.synthetic:::.sample_with_na

  result <- sample_with_na(letters, 100, prob_na = 0, replace = TRUE)
  expect_equal(sum(is.na(result)), 0)
})

test_that(".random_date generates dates in range", {
  random_date <- pcornet.synthetic:::.random_date

  start <- as.Date("2020-01-01")
  end <- as.Date("2020-12-31")
  dates <- random_date(100, start, end)

  expect_true(all(dates >= start))
  expect_true(all(dates <= end))
  expect_length(dates, 100)
})

test_that(".random_datetime generates datetimes in range", {
  random_datetime <- pcornet.synthetic:::.random_datetime

  start <- as.Date("2020-01-01")
  end <- as.Date("2020-12-31")
  datetimes <- random_datetime(100, start, end)

  expect_true(all(as.Date(datetimes) >= start))
  expect_true(all(as.Date(datetimes) <= end))
  expect_length(datetimes, 100)
  expect_s3_class(datetimes, "POSIXct")
})
