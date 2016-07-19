if ( !isGeneric("downloadTRMM") ) {
  setGeneric("downloadTRMM", function(begin, ...)
    standardGeneric("downloadTRMM"))
}

#' Download TRMM 3B42 data
#' 
#' @description
#' Download TRMM 3B42 daily or 3-hourly data for a given time span from the NASA 
#' FTP servers (\url{ftp://disc3.nascom.nasa.gov/data/s4pa/TRMM_L3}).
#' 
#' @param begin \code{POSIXlt} or \code{character}, starting time.
#' @param end Same as \code{begin}, end time.
#' @param type \code{character}, data repetition cycle. Currently available 
#' options are "daily" (default) and "3-hourly".
#' @param dsn \code{character}, target folder for file download. Defaults to the 
#' current working directory if not specified otherwise. 
#' @param format \code{character}, see \code{\link{strptime}}.
#' 
#' @return
#' A \code{data.frame} with downloaded .bin (or .Z for 3-hourly data) and .xml 
#' filepaths.
#' 
#' @author
#' Florian Detsch
#' 
#' @seealso
#' \code{\link{strptime}}, \code{\link{download.file}}.
#' 
#' @examples  
#' \dontrun{
#' ## download TRMM 3B42 daily data from Jan 1 to Jan 3, 2015
#' downloadTRMM(begin = "2015-01-01", end = "2015-01-03")
#' 
#' ## same for 3-hourly data, from noon to noon
#' downloadTRMM(begin = "2015-01-01 12:00", end = "2015-01-03 12:00", 
#'              type = "3-hourly", format = "%Y-%m-%d %H:%M")

#' }
#'               
#' @export downloadTRMM
#' @name downloadTRMM
NULL

################################################################################
### function using 'character' input -----
#' @aliases downloadTRMM,character-method
#' @rdname downloadTRMM
setMethod("downloadTRMM",
          signature(begin = "character"),
          function(begin, end, type = c("daily", "3-hourly"), 
                   dsn = getwd(), format = "%Y-%m-%d") {
  
  ## transform 'begin' and 'end' to 'Date' object if necessary
  begin <- strptime(begin, format = format)
  end <- strptime(end, format = format)

  downloadTRMM(begin = begin, end = end, type = type[1], dsn = dsn)
})
  
  
################################################################################
### function using 'POSIXlt' input -----
#' @aliases downloadTRMM,POSIXlt-method
#' @rdname downloadTRMM
setMethod("downloadTRMM",
          signature(begin = "POSIXlt"),
          function(begin, end, type = c("daily", "3-hourly"), dsn = getwd()) {

  ## tile server
  ftp <- trmmServer(type = type[1])
  
  ## download daily data
  if (type[1] == "daily") {
    do.call("rbind", lapply(seq(as.Date(begin), as.Date(end), 1), function(i) {
      
      # trmm date naming convention
      drs <- paste0(ftp, strftime(i, format = "%Y/%j/"))
      nms <- strftime(i + 1, format = "%Y.%m.%d")
      
      # list files available on server
      fls <- fls_out <- character(2L)
      
      for (j in 1:2) {
        fls[j] <- paste0(drs, "3B42_daily.", nms, ".7", 
                         ifelse(j == 1, ".bin", ".bin.xml"))
        fls_out[j] <- paste0(dsn, "/", basename(fls[j]))
        
        # if required, download current file  
        if (!file.exists(fls_out[j]))
          download.file(fls[j], fls_out[j], mode = "wb")
      }
      
      # return data frame with *.bin and *.xml filenames
      xml <- grep("xml", fls_out)
      data.frame(bin = fls_out[-xml], xml = fls_out[xml], 
                 stringsAsFactors = FALSE)
    }))
    
  ## download 3-hourly data  
  } else if (type[1] == "3-hourly") {

    # 3-hourly sequence
    hrs <- c(seq(3, 21, 3), 0)
    
    # loop over single days
    dys <- if (strftime(end, "%H") != "00") { 
      seq(as.Date(begin), as.Date(end), 1)
    } else {
      seq(as.Date(begin), as.Date(end) - 1, 1)
    }
    
    do.call("rbind", lapply(dys, function(i) {
      drs <- paste0(ftp, strftime(i, format = "%Y/%j/"))

      ## start day
      if (i == as.Date(begin)) {
        
        nxt <- hrs - as.integer(strftime(begin, "%H"))
        
        # before midnight
        if (any(nxt < 0)) {
          nxt <- which(nxt >= 0)[1]
          hr3 <- seq(strptime(paste(i, hrs[nxt]), format = "%Y-%m-%d %H"), 
                     strptime(paste(i + 1, hrs[length(hrs)]), format = "%Y-%m-%d %H"), 
                     "3 hours")
          
        # midnight    
        } else {
          nxt <- length(hrs)
          hr3 <- strptime(paste(i + 1, hrs[length(hrs)]), format = "%Y-%m-%d %H")
        }
      
      ## end day    
      } else if (i == as.Date(end) | 
                 (i == as.Date(end) - 1 & strftime(end, "%H") == "00")) {
        
        lst <- hrs - as.integer(strftime(end, "%H"))
        
        # before midnight
        if (any(lst < 0)) {
          lst <- which(lst > 0)[1] - 1
          hr3 <- seq(strptime(paste(i, hrs[1]), format = "%Y-%m-%d %H"), 
                     strptime(paste(i, hrs[lst]), format = "%Y-%m-%d %H"), 
                     "3 hours")
          
        # midnight  
        } else {
          hr3 <- seq(strptime(paste(i, hrs[1]), format = "%Y-%m-%d %H"), 
                     strptime(paste(i + 1, hrs[length(lst)]), format = "%Y-%m-%d %H"), 
                     "3 hours")
        }
        
      ## intermediary day  
      } else {
        hr3 <- seq(strptime(paste(i, "03"), format = "%Y-%m-%d %H"), 
                   strptime(paste(i + 1, hrs[length(hrs)]), format = "%Y-%m-%d %H"), 
                   "3 hours")
      }
        
      fls <- paste0("3B42.", strftime(hr3, format = "%Y%m%d.%H.7.HDF"))
      fls_Z <- paste0(drs, fls, ".Z")
      fls_xml <- paste0(drs, fls, ".xml")
      
      fls_out_Z <- paste0(dsn, "/", basename(fls_Z))
      fls_out_xml <- paste0(dsn, "/", basename(fls_xml))
      
      for (j in seq(fls)) {
        if (!file.exists(fls_out_Z[j]))
          download.file(fls_Z[j], fls_out_Z[j], mode = "wb")
        
        if (!file.exists(fls_out_xml[j]))
        download.file(fls_xml[j], fls_out_xml[j], mode = "wb")
      }
      
      # return data frame with *.Z and *.xml filenames
      data.frame(Z = fls_out_Z, xml = fls_out_xml, 
                 stringsAsFactors = FALSE)
      

    }))
  }
})
