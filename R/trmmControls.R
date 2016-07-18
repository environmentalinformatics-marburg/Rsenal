trmmServer <- function(type = c("daily", "3-hourly")) {
  ## daily
  if (type[1] == "daily") {
    "ftp://disc3.nascom.nasa.gov/data/s4pa/TRMM_L3/TRMM_3B42_daily/"
  ## 3-hourly  
  } else if (type[1] == "3-hourly") {
    "ftp://disc3.nascom.nasa.gov/data/s4pa/TRMM_L3/TRMM_3B42/"
  ## invalid  
  } else {
    stop("'type = ", type[1], "' not supported. See ?downloadTRMM for available options.\n")
  }
}