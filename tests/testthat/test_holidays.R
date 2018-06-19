
context("Holidays")

test_that("Arguments are checked", {
  expect_error(Holidays("a", "2018-06-01"))
  expect_error(Holidays("2018-05-01", "b"))
  expect_error(Holidays(c("2018-05-01", "2018-05-10"), "2018-06-01"))
  expect_error(Holidays("2018-05-01", "2018-06-01", include_weekends = "a"))
  expect_error(Holidays("2018-05-33", "2018-06-05"))
  expect_error(Holidays("2018-06-03", "2018-05-05"))
  expect_error(IsHoliday(c("2018-05-01", "2018-05-33")))
  expect_error(IsHoliday(c("2018-05-01", "a")))
  expect_error(IsHoliday(c("2018-05-01", "2018-05-02"), include_weekends = c(TRUE, FALSE)))
})



test_that("Holidays are properly identified", {
  expect_equal(IsHoliday("2018-01-01"), TRUE)
  expect_equal(IsHoliday("2018-04-01"), TRUE)
  expect_equal(IsHoliday("2018-05-31"), TRUE)
  expect_equal(IsHoliday("2018-06-17"), TRUE)
  expect_equal(IsHoliday("2018-08-15"), TRUE)
  expect_equal(IsHoliday("2018-11-11"), TRUE)
  expect_equal(IsHoliday("2018-12-26"), TRUE)
  expect_equal(IsHoliday("2020-04-12"), TRUE)
  expect_equal(IsHoliday("2015-12-31"), FALSE)
  expect_equal(IsHoliday("2018-06-18"), FALSE)
  expect_equal(IsHoliday("2020-07-10"), FALSE)

  expect_equal(Holidays("2018-03-28", "2018-04-03"),
               as.Date(c("2018-03-31", "2018-04-01", "2018-04-02")))
  expect_equal(Holidays("2018-01-01", "2018-12-31", include_weekends = FALSE),
               as.Date(c("2018-01-01", "2018-01-06", "2018-04-01", "2018-04-02",
                         "2018-05-01", "2018-05-03", "2018-05-20", "2018-05-31",
                         "2018-08-15", "2018-11-01", "2018-11-11", "2018-12-25",
                         "2018-12-26")))
})
