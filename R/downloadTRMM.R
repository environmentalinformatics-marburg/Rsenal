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
#' ## download TRMM 3B42 data from Jan 1 to Jan 5, 2015
#' downloadTRMM(begin = "2015-01-01", end = "2015-01-05")
#'               
#' @export downloadTRMM
#' @aliases downloadTRMM
downloadTRMM <- function(begin, end, dsn = ".", format = "%Y-%m-%d") {
  
  ## transform 'begin' and 'end' to 'Date' object if necessary
  if (!class(begin) == "Date")
    begin <- as.Date(begin, format = format)
  
  if (!class(end) == "Date")
    end <- as.Date(end, format = format)
  
  ## trmm ftp server
  ch_url <-"ftp://disc3.nascom.nasa.gov/data/s4pa/TRMM_L3/TRMM_3B42_daily/"
  
  ## loop over daily sequence
  ls_fls_out <- lapply(seq(begin, end, 1), function(i) {
    
    # year and julian day
    tmp_ch_yr <- strftime(i, format = "%Y")
    tmp_ch_dy <- strftime(i, format = "%j")
    
    # trmm date format
    tmp_dt <- strftime(i+1, format = "%Y.%m.%d")
    
    # list files available on server
    tmp_ch_url <- paste(ch_url, tmp_ch_yr, tmp_ch_dy, "", sep = "/")
    
    tmp_ch_fls <- tmp_ch_fls_out <- character(2L)
    for (j in 1:2) {
      tmp_ch_fls[j] <- paste0("3B42_daily.", tmp_dt, ".7", 
                              ifelse(j == 1, ".bin", ".bin.xml"))
      
      tmp_ch_fls[j] <- paste(tmp_ch_url, tmp_ch_fls[j], sep = "/")
      tmp_ch_fls_out[j] <- paste(dsn, basename(tmp_ch_fls[j]), sep = "/")
      
      download.file(tmp_ch_fls[j], tmp_ch_fls_out[j], mode = "wb")
    }

    # return data frame with *.bin and *.xml filenames
    tmp_id_xml <- grep("xml", tmp_ch_fls_out)
    data.frame(bin = tmp_ch_fls_out[-tmp_id_xml], 
               xml = tmp_ch_fls_out[tmp_id_xml], 
               stringsAsFactors = FALSE)
    
  })
  
  ## join and return names of processed files
  ch_fls_out <- do.call("rbind",ls_fls_out)
  return(ch_fls_out)
}