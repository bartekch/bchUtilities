
context("Hours counters")

test_that("Arguments are checked", {
  expect_error(CalculateHoursInMonths("A", "2018-06-30"))
  expect_error(CalculateHoursInMonths(c("2018-06-01", "2018-06-02"), "2018-06-31"))

  expect_error(CalculateHoursInYears("A", "2018-06-30"))
  expect_error(CalculateHoursInYears(c("2018-06-01", "2018-06-02"), "2018-06-31"))
})



hours <- data.frame(year = rep(2018, 12),
                    month = 1:12,
                    hour_count = c(744, 672, 743, 720, 744, 720, 744, 744, 720, 745, 720, 744))

test_that("Counts are correct", {
  expect_equal(CalculateHoursInMonths("2018-01-01", "2018-12-31"), hours)
  expect_equal(CalculateHoursInMonths("2018-05-30", "2018-06-03"),
               data.frame(year = rep(2018, 2), month = 5:6, hour_count = c(48, 72)))

  expect_equal(CalculateHoursInYears("2016-01-01", "2018-05-31"),
               data.frame(year = 2016:2018, hour_count = c(8784, 8760, 3623)))
})
