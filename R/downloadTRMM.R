#' Download TRMM 3B42 daily data
#' 
#' @description
#' Download TRMM 3B42 daily binary data for a given time span from the NASA FTP servers 
#' (\url{ftp://disc3.nascom.nasa.gov/data/s4pa/TRMM_L3/TRMM_3B42_daily/}).
#' 
#' @param begin Date or character. Desired start date.
#' @param end Date or character. Desired end date.
#' @param dsn Character. Target directory for file download. If not supplied, 
#' this defaults to the current working directory. 
#' @param format Character. See \code{\link{as.Date}}.
#' @param ... Further arguments. Currently not used. 
#' 
#' @return
#' A vector of filepaths.
#' 
#' @author
#' Florian Detsch
#' 
#' @seealso
#' \code{\link{download.file}}, \code{\link{getURL}}
#' 
#' @examples  
#' # Download TRMM 3B42 data from Jan 1 to Jan 31, 2015 (might take some time...)
#' downloadTRMM(begin = "2015-01-01", end = "2015-01-31")
#'               
#' @export downloadTRMM
#' @aliases downloadTRMM
downloadTRMM <- function(begin, end, dsn = ".", format = "%Y-%m-%d") {
  
  ## required packages
  stopifnot(require(lubridate))
  stopifnot(require(RCurl))
  
  ## transform 'begin' and 'end' to 'Date' object if necessary
  if (!is.Date(begin))
    begin <- as.Date(begin, format = format)
  
  if (!is.Date(end))
    end <- as.Date(end, format = format)
  
  ## trmm ftp server
  ch_url <-"ftp://disc3.nascom.nasa.gov/data/s4pa/TRMM_L3/TRMM_3B42_daily/"
  
  ## loop over daily sequence
  ls_fls_out <- lapply(seq(begin, end, "day"), function(i) {
    
    # year and julian day
    tmp_ch_yr <- year(i)
    tmp_ch_dy <- strftime(i, format = "%j")
    
    # list files available on server
    tmp_ch_url <- paste(ch_url, tmp_ch_yr, tmp_ch_dy, "", sep = "/")
    tmp_ch_fls <- getURL(tmp_ch_url, verbose = FALSE, dirlistonly = TRUE)
    tmp_ls_fls <- strsplit(tmp_ch_fls, "\n")
    tmp_ch_fls <- unlist(tmp_ls_fls)
    
    # download all files found (including *.xml)
    tmp_ch_fls <- paste(tmp_ch_url, tmp_ch_fls, sep = "/")
    tmp_ch_fls_out <- sapply(tmp_ch_fls, function(j) {
      tmp_ch_fls_out <- paste(dsn, basename(j), sep = "/")
      download.file(j, tmp_ch_fls_out)
      return(tmp_ch_fls_out)
    })
    
    # return data frame with *.bin and *.xml filenames
    tmp_ch_fls_out <- as.character(tmp_ch_fls_out)
    tmp_id_xml <- grep("xml", tmp_ch_fls_out)
    data.frame(bin = tmp_ch_fls_out[-tmp_id_xml], 
               xml = tmp_ch_fls_out[tmp_id_xml])
    
  })
  
  ## join and return names of processed files
  ch_fls_out <- do.call("rbind",ls_fls_out)
  return(ch_fls_out)
}