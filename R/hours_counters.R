
#' Calculate number of hours in months
#'
#' Calculates number of dates in every month between (and including) the given dates.
#' Missing values are omitted.
#'
#' @param date_start beginning of the period, Date or object coercible to date
#' @param date_end end of the period, Date or object coercible to date
#'
#' @return data frame with columns: year (integer), month (integer), hour_count (integer)
#'
#' @examples
#' CalculateHoursInMonths("2018-01-01", "2018-12-31")
#'
#' @export

CalculateHoursInMonths <- function(date_start, date_end) {
  # check arguments
  stopifnot(length(date_start) == 1)
  stopifnot(length(date_end) == 1)

  tryCatch(date_start <- as.Date(date_start),
           error = function(e) stop("Cannot coerce date_start to Date object."))
  tryCatch(date_end <- as.Date(date_end),
           error = function(e) stop("Cannot coerce date_end to Date object."))
  stopifnot(date_start <= date_end)


  # calculate number of hours
  hours_seq <- seq(from = as.POSIXlt(paste0(date_start, " 00:00:00")),
                   to = as.POSIXlt(paste0(date_end, " 23:00:00")),
                   by = "1 hour")

  hour_counts <- MonthCounter(hours_seq)
  colnames(hour_counts) <- c("year", "month", "hour_count")
  hour_counts
}




#' Calculate number of hours in years
#'
#' Calculates number of dates in every year between (and including) the given dates.
#' Missing values are omitted.
#'
#' @param date_start beginning of the period, Date or object coercible to date
#' @param date_end end of the period, Date or object coercible to date
#'
#' @return data frame with columns: year (integer), hour_count (integer)
#'
#' @examples
#' CalculateHoursInYears("2016-01-01", "2018-05-31")
#'
#' @export

CalculateHoursInYears <- function(date_start, date_end) {
  # check arguments
  stopifnot(length(date_start) == 1)
  stopifnot(length(date_end) == 1)

  tryCatch(date_start <- as.Date(date_start),
           error = function(e) stop("Cannot coerce date_start to Date object."))
  tryCatch(date_end <- as.Date(date_end),
           error = function(e) stop("Cannot coerce date_end to Date object."))
  stopifnot(date_start <= date_end)


  # calculate number of hours
  hours_seq <- seq(from = as.POSIXlt(paste0(date_start, " 00:00:00")),
                   to = as.POSIXlt(paste0(date_end, " 23:00:00")),
                   by = "1 hour")

  hour_counts <- YearCounter(hours_seq)
  colnames(hour_counts) <- c("year", "hour_count")
  hour_counts
}
