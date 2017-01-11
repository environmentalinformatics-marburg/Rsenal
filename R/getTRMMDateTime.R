#' TRMM 3B42 Date/Time Functions
#' 
#' @description 
#' Extract date/time information from TRMM 3B42 files.
#' 
#' @param x \code{character}. TRMM 3B42 filename(s).
#' @param as_posix,as_date \code{logical}, default to \code{FALSE}. See 
#' respective function descriptions below.
#' 
#' @return See 'Functions' section below.
#'  
#' @examples 
#' getTRMMDateTime("3B42.20150106.03.7.HDF")
#' getTRMMDateTime("3B42.20150106.03.7.HDF", as_posix = TRUE)
#'     
#'             
#' @name getTRMMDateTime
#' 
NULL

#' @describeIn getTRMMDateTime Get datetime of 3-hourly TRMM 3B42 file(s) 
#' (\code{character} or \code{POSIXt} depending on 'as_posix'). 
#' @export getTRMMDateTime
getTRMMDateTime <- function(x, as_posix = FALSE) {
  
  dtm <- paste(getTRMMDate(x), getTRMMTime(x), sep = ".")
  
  if (as_posix)
    dtm <- strptime(dtm, format = "%Y%m%d.%H")

  return(dtm)
}

#' @describeIn getTRMMDateTime Get date of TRMM 3B42 file(s) (\code{character} 
#' or \code{Date} depending on 'as_date').
#' @export getTRMMDate
getTRMMDate <- function(x, as_date = FALSE) {
  dt <- sapply(strsplit(basename(x), "\\."), "[[", 2)
  
  if (as_date)
    dt <- as.Date(dt, format = "%Y%m%d")
  
  return(dt)
}

#' @describeIn getTRMMDateTime Get time of 3-hourly TRMM 3B42 file(s) 
#' (\code{character}).
#' @export getTRMMTime
getTRMMTime <- function(x) {
  sapply(strsplit(basename(x), "\\."), "[[", 3)
}