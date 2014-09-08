#' create a regular time series 
#' 
#' @description
#' This function will create a (empty) character time series.
#' This can be used for creating regular time series of real-world observations
#' that have missing data via merging.
#' 
#' @param start character string of starting date and time. Needs to be of 
#' format "YYYY-mm-dd HH:MM:SS"
#' @param end character string of ending date and time. Needs to be of 
#' format "YYYY-mm-dd HH:MM:SS"
#' @param step the time step of the time series (in seconds)
#' 
#' @return
#' a character vector of datetimes spaced regularly from start to end
#' 
#' @examples
#' ## 24 hrs
#' createTimeSeries("1990-01-01 00:00:00", "1990-01-01 23:00:00", step = 60 * 60)
#' 
#' ## 365 days
#' createTimeSeries("1990-01-01 00:00:00", "1990-12-31 23:00:00", step = 60 * 60 * 24)
#' 
#' @export createTimeSeries

createTimeSeries <- function(start, end, step) {
  
  ## set system locale time zone to "UTC" for time handling w/out
  ## daylight saving
  Sys.setenv(TZ = "UTC")
  
  ## create series 
  date.from <- as.POSIXct(start)
  date.to <- as.POSIXct(end)
  
  ## create regular time series
  tseries <- seq(from = date.from, to = date.to, by = step)
  
  ## convert to character and write
  tseries <- format(tseries, usetz = F)
  
  return(tseries)
  
}
