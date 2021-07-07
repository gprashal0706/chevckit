#' Half-hour to period of day
#'
#' @param x vector of datetimes.
#'
#' @return vector giving period of day.
#' @export
#' 
#' @importFrom lubridate hour minute
hh_to_period <- function(x) {
  hour(x)*60 + minute(x)/1 + 1
}
