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
#' @param type character. Determines whether to return an object of class 
#' 'POSIXct' (default) or 'character'.
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

createTimeSeries <- function(start, end, step, type = c("POSIX", "char")) {
  
  type <- type[1]
  
  ## set system locale time zone to "UTC" for time handling w/out
  ## daylight saving
  Sys.setenv(TZ = "UTC")
  op <- options(digits.secs = 4)
  
  ## create series 
  date.from <- as.POSIXct(start, format = '%Y-%m-%d %H:%M:%OS')
  date.to <- as.POSIXct(end, format = '%Y-%m-%d %H:%M:%OS')
  
  ## create regular time series
  tseries <- seq(from = date.from, to = date.to, by = step)
  
  ## convert to character and write
  if (type == "char")  tseries <- format(tseries, usetz = FALSE)
  
  options(op)
  return(tseries)
  
}
