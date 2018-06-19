
context("Dates counters")

test_that("Arguments are checked", {
  expect_error(CalculateDatesInMonths("A"))
  expect_error(CalculateDatesInMonths(numeric(0)))

  expect_error(CalculateDatesInYears("A"))
  expect_error(CalculateDatesInYears(numeric(0)))
})



dates_vector <- seq.Date(as.Date("2018-04-01"), as.Date("2018-06-19"), by = "1 day")
monthly_counts <- data.frame(year = rep(2018, 3), month = 4:6, date_count = c(30, 31, 19))
yearly_counts <- data.frame(year = 2018, date_count = 80)

dates_vector_reps <- rep(as.Date("2018-06-19"), 10)
monthly_counts_reps <- data.frame(year = 2018, month = 6, date_count = 10)
yearly_counts_reps <- data.frame(year = 2018, date_count = 10)

test_that("Counts are correct", {
  expect_equal(CalculateDatesInMonths(dates_vector), monthly_counts)
  expect_equal(CalculateDatesInMonths(dates_vector_reps), monthly_counts_reps)

  expect_equal(CalculateDatesInYears(dates_vector), yearly_counts)
  expect_equal(CalculateDatesInYears(dates_vector_reps), yearly_counts_reps)
})
