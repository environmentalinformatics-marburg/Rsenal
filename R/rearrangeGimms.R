#' Rearrange GIMMS 3G binary files
#' 
#' @description
#' Rearrange (previously downloaded) GIMMS 3G binary data in ascending order of 
#' time.
#' 
#' @param fls Character. Vector of filepaths. If \code{NULL}, folder \code{dsn}
#' will be searched for available GIMMS data to rearrange.
#' @param dsn Character. Path to look for GIMMS data. If not supplied and 
#' \code{is.null(fls)}, this argument defaults to the current working directory.
#' @param pattern Character. A regular expression passed on to 
#' \code{\link{list.files}}.
#' @param ... Further arguments passed on to \code{\link{list.files}}. 
#' 
#' @return
#' A vector of GIMMS filepaths in ascending order of time.
#' 
#' @author
#' Florian Detsch
#' 
#' @seealso
#' \code{\link{list.files}}
#' 
#' @examples  
#' # Download GIMMS 3G data from 1990 to 2009 (this might take some time...)
#' gimms_fls <- downloadGimms(decades = c(1990, 2000), 
#'                            dsn = paste(getwd(), "data", sep = "/"))
#'                            
#' # Rearrange downloaded GIMMS data in ascending order of time
#' rearrangeGimms(fls = gimms_fls)
#'               
#' @export rearrangeGimms
#' @aliases rearrangeGimms
rearrangeGimms <- function(fls = NULL, 
                           dsn = ".", 
                           pattern = "^geo", 
                           ...) {
  
  # Required package
  library(dplyr)
  library(zoo)
  
  # List GIMMS files avl in `dsn` if `fls == NULL`
  if (is.null(fls))
    gimms_fls <- list.files(dsn, pattern = pattern, ...)
  
  gimms_df <- data.frame(file = gimms_fls, stringsAsFactors = FALSE)
  
  # Switch current locale time to US standard
  systime_locale <- Sys.getlocale(category = "LC_TIME")
  Sys.setlocale(category = "LC_TIME", locale = "en_US.UTF-8")
  
  # Create columns 'month' and 'year' from avl files, and rearrange files based
  # on concatenated column 'yearmon'
  gimms_df <- 
    gimms_df %>% 
    mutate(month = substr(basename(file), 6, 8)) %>%
    mutate(year = substr(basename(file), 4, 5)) %>%
    mutate(yearmon = strftime(as.yearmon(paste0(month, year), format = "%b%y"), "%Y%m")) %>%
    arrange(yearmon)

  # Revoke locale time adjustment
  Sys.setlocale(category = "LC_TIME", locale = systime_locale)
  
  # Return rearranged files
  gimms_fls <- gimms_df$file
  return(gimms_fls)
}