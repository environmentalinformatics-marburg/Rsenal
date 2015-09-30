#' Download GIMMS 3G data
#' 
#' @description
#' Download GIMMS 3G binary data for a given time span from NASA FTP server 
#' (\url{http://ecocast.arc.nasa.gov/data/pub/gimms/3g.v0/}).
#' 
#' @param begin Numeric. Start year, e.g. '2000'. If not supplied, download will 
#' start from the first year available.
#' @param end Numeric. End year. If not supplied, download will stop at the last 
#' year available.
#' @param dsn Character. Destination folder for file download. If not supplied, 
#' all downloaded files will be stored in the current working directory. 
#' @param ... Further arguments. Currently not in use. 
#' 
#' @return
#' A vector of filepaths.
#' 
#' @author
#' Florian Detsch
#' 
#' @seealso
#' \code{\link{download.file}}
#' 
#' @examples  
#' # Download GIMMS 3G data from 1990 to 2009 (this might take some time...)
#' downloadGimms(begin = 2000, dsn = paste(getwd(), "data", sep = "/"))
#'               
#' @export downloadGimms
#' @aliases downloadGimms
downloadGimms <- function(begin = NULL, end = NULL,
                          dsn = ".",
                          ...) {
  
  #   # URL of GIMMS 3G FTP server
  #   gimms_ftp <- "http://ecocast.arc.nasa.gov/data/pub/gimms/3g.v0/"
  
  ## available files
  gimms_fls <- readLines("http://ecocast.arc.nasa.gov/data/pub/gimms/3g.v0/00FILE-LIST.txt")
  
  ## if specified, subset available files by time frame
  gimms_bsn <- basename(gimms_fls)
  gimms_yrs_chr <- substr(gimms_bsn, 4, 5)
  gimms_yrs_num <- as.numeric(gimms_yrs_chr)
  gimms_yrs_chr[gimms_yrs_num > 80] <- paste0("19", gimms_yrs_chr[gimms_yrs_num > 80])
  gimms_yrs_chr[gimms_yrs_num <= 80] <- paste0("20", gimms_yrs_chr[gimms_yrs_num <= 80])
  gimms_yrs_num <- as.numeric(gimms_yrs_chr)
  
  if (is.null(begin)) begin <- gimms_yrs_num[1]
  if (is.null(end)) end <- gimms_yrs_num[length(gimms_yrs_num)]
  gimms_fls <- gimms_fls[gimms_yrs_chr %in% seq(begin, end)]
  
  ## download
  for (i in gimms_fls) {
    destfile <- paste0(dsn, "/", basename(i))
    if (file.exists(destfile)) {
      cat("File", destfile, "already exists in destination folder. Proceeding to next file ...\n")
    } else {
      try(download.file(i, destfile = destfile), silent = TRUE)
    }
  }
  
  ## return vector with output files
  gimms_out <- paste0(dsn, "/", gimms_fls)
  return(gimms_out)
  
}