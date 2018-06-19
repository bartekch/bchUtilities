
#' Calculate number of dates in months
#'
#' Calculates number of dates in every month in the given dates vector.
#' Missing values are omitted. Duplicated dates are counted multiple times!
#'
#' @param dates vector of dates
#'
#' @return data frame with columns: year (integer), month(integer), date_count (integer)
#'
#' @examples
#' CalculateDatesInMonths(seq.Date(as.Date("2018-04-01"), as.Date("2018-06-19"), by = "1 day"))
#'
#' @export

CalculateDatesInMonths <- function(dates) {
  # check arguments
  stopifnot(length(dates) >= 1)
  tryCatch(dates <- as.Date(dates),
           error = function(e) stop("Cannot coerce dates to Date object."))

  # calculate counts
  date_counts <- MonthCounter(dates)
  colnames(date_counts) <- c("year", "month", "date_count")
  date_counts
}




#' Calculate number of dates in years
#'
#' Calculates number of dates in every year in the given dates vector.
#' Missing values are omitted. Duplicated dates are counted multiple times!
#'
#' @param dates vector of dates
#'
#' @return data frame with columns: year (integer), date_count (integer)
#'
#' @examples
#' CalculateDatesInYears(seq.Date(as.Date("2018-04-01"), as.Date("2018-06-19"), by = "1 day"))
#'
#' @export

CalculateDatesInYears <- function(dates) {
  # check arguments
  stopifnot(length(dates) >= 1)
  tryCatch(dates <- as.Date(dates),
           error = function(e) stop("Cannot coerce dates to Date object."))

  # calculate counts
  date_counts <- YearCounter(dates)
  colnames(date_counts) <- c("year", "date_count")
  date_counts
}
