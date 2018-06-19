
#' Calculates counts of every month for the given date-time vector
#'
#' @param date_times date-time object
#'
#' @return data frame with columns: year (integer), month (integer), count (integer)

MonthCounter <- function(date_times) {

  years <- as.integer(format(date_times, "%Y"))
  months <- as.integer(format(date_times, "%m"))
  counts <- as.data.frame(table(months, years),
                          responseName = "count", stringsAsFactors = FALSE)

  # reformat output
  counts$years <- as.integer(counts$years)
  counts$months <- as.integer(counts$months)
  counts <- counts[counts$count > 0, ]
  counts[, c("years", "months", "count")]
}




#' Calculates counts of every year for the given date-time vector
#'
#' @param date_times date-time object
#'
#' @return data frame with columns: year (integer), count (integer)

YearCounter <- function(date_times) {

  years <- as.integer(format(date_times, "%Y"))
  counts <- as.data.frame(table(years),
                          responseName = "count", stringsAsFactors = FALSE)

  # reformat output
  counts$years <- as.integer(counts$years)
  counts
}
