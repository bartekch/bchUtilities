
#' Find Polish holidays
#'
#' Returns vector of all Polish holidays and weekends between (and including) th given dates
#'
#' @param date_start beginning of the period, Date or object coercible to date
#' @param date_end end of the period, Date or object coercible to date
#' @param include_weekends logical, whether or not weekends should be included
#'
#' @return vector of dates with holidays and weekends
#'
#' @examples
#' Holidays("2018-03-28", "2018-04-03")
#' Holidays("2018-01-01", "2018-12-31", include_weekends = FALSE)
#'
#' @export

Holidays <- function(date_start, date_end = date_start, include_weekends = TRUE) {

  stopifnot(length(date_start) == 1)
  stopifnot(length(date_end) == 1)
  stopifnot(length(include_weekends) == 1)
  stopifnot(is.logical(include_weekends))

  tryCatch(date_start <- as.Date(date_start),
           error = function(e) stop("Cannot coerce date_start to Date object."))
  tryCatch(date_end <- as.Date(date_end),
           error = function(e) stop("Cannot coerce date_end to Date object."))
  stopifnot(date_start <= date_end)

  dates_seq <- seq(date_start, date_end, by = "1 day")

  holidays <- IsHoliday(dates_seq, include_weekends)

  return(dates_seq[holidays])
}


#' Check for Polish holidays
#'
#' Checks whether given dates are weekends or holidays
#'
#' @param dates vector of Dates of object coercible by as.Date
#' @param include_weekends logical, whether or not weekends should be included
#'
#' @return logical vector of the same length as dates, TRUE when given date is
#'   holiday (or weekend), FALSE otherwise. Some holidays could fall on weekends.
#'
#' @examples
#' IsHoliday("2020-04-12")
#' IsHoliday("2015-12-31")
#'
#' @export

IsHoliday <- function(dates, include_weekends = TRUE) {
  stopifnot(length(include_weekends) == 1)
  stopifnot(is.logical(include_weekends))

  tryCatch(dates <- as.Date(dates),
           error = function(e) stop("Cannot coerce dates to Date object."))

  # weekends
  if (include_weekends) {
    weekends <- lubridate::wday(dates) %in% c(1, 7)
  } else {
    weekends <- logical(length(dates))
  }

  # fixed holidays
  days <- lubridate::day(dates)
  months <- lubridate::month(dates)
  holidays_fixed <- (months == 1 & days == 1) |  # new year
    (months == 1 & days == 6) |                  # epiphany
    (months == 5 & days == 1) |                  # labour day
    (months == 5 & days == 3) |                  # constitution day
    (months == 8 & days == 15) |                 # assumption of mary/ armed forces  day
    (months == 11 & days == 1) |                 # all saints' day
    (months == 11 & days == 11) |                # independence day
    (months == 12 & days == 25) |                # christmas first day
    (months == 12 & days == 26)                  # christmas second day

  # float holidays
  years <- seq(lubridate::year(min(dates)), lubridate::year(max(dates)))
  easter_sundays <- as.Date(timeDate::EasterSunday(years))
  easter_mondays <- easter_sundays + 1
  green_week <- easter_sundays + 49
  corpus_christi <- as.Date(timeDate::CorpusChristi(years))
  holidays_float <- (dates %in% easter_mondays) | (dates %in% corpus_christi) |
    (dates %in% easter_sundays) | (dates %in% green_week)

  # gather results
  res <- weekends | holidays_fixed | holidays_float
  return(res)
}

