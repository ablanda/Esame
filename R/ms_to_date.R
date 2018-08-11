#' MS_to_date
#'
#' @param sec
#' @param t0
#' @param timezone
#'
#' @return
#' @export
#'
#' @examples
ms_to_date = function(sec, t0="1970-01-01", timezone="CET") {
  ## @ms: a numeric vector of milliseconds (big integers of 13 digits)
  ## @t0: a string of the format "yyyy-mm-dd", specifying the date that
  ##      corresponds to 0 millisecond
  ## @timezone: a string specifying a timezone that can be recognized by R
  ## return: a POSIXct vector representing calendar dates and times
  # sec = ms / 1000
  as.POSIXct(sec, origin=t0, tz=timezone)
}
