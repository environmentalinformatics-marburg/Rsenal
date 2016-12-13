#' Download TRMM 3B42 Data
#' 
#' @description
#' Download TRMM 3B42 daily (NetCDF) or 3-hourly (HDF) files for a given time 
#' span from the NASA FTP servers 
#' (\url{ftp://disc3.nascom.nasa.gov/data/s4pa/TRMM_L3/}).
#' 
#' @param begin,end Start and end date as \code{Date} or \code{character}. 
#' @param type \code{character}. Temporal resolution of downloaded TRMM data. 
#' Currently available options are \code{"daily"} (default) and 
#' \code{"3-hourly"}.
#' @param dsn \code{character}. Download folder, defaults to the current working 
#' directory. 
#' @param xml \code{logical}, defaults to \code{FALSE}. If \code{TRUE}, .xml 
#' files associated with each .nc4 file are also downloaded to 'dsn'.
#' @param overwrite \code{logical}, defaults to \code{FALSE}. Determines whether 
#' existing files are overwritten. 
#' @param cores \code{integer}. Number of cores for parallel processing. Note 
#' that this takes only effect if a sufficiently fast internet connection is 
#' available.
#' @param ... In case 'begin' and/or 'end' are \code{character} objects, 
#' additional arguments passed to \code{\link{as.Date}}.
#' 
#' @return
#' A \code{character} vector of local filepaths.
#' 
#' @author
#' Florian Detsch
#' 
#' @seealso
#' \code{\link{download.file}}.
#' 
#' @examples  
#' \dontrun{
#' ## download TRMM 3B42 daily data from Jan 1 to Jan 5, 2015
#' downloadTRMM(begin = "2015-01-01", end = "2015-01-05")
#' 
#' ## same for 3-hourly data
#' downloadTRMM(begin = "2015-01-01", end = "2015-01-05", type = "3-hourly")
#' }
#'               
#' @export downloadTRMM
#' @aliases downloadTRMM
downloadTRMM <- function(begin, end, type = c("daily", "3-hourly"), 
                         dsn = ".", xml = FALSE, overwrite = FALSE, 
                         cores = 1L, ...) {
  
  ## transform 'begin' and 'end' to 'Date' object if necessary
  if (!inherits(begin, "Date"))
    begin <- as.Date(begin, ...)
  
  if (!inherits(end, "Date"))
    end <- as.Date(end, ...)
  
  ## create online and offline target files
  onl <- getTRMMFiles(begin, end, type, xml)
  ofl <- paste(dsn, basename(onl), sep = "/")
  
  ## parallelization
  cl <- parallel::makePSOCKcluster(cores)
  parallel::clusterExport(cl, c("onl", "ofl", "overwrite"), 
                          envir = environment())
  on.exit(parallel::stopCluster(cl))
  
  ## download
  parallel::parSapply(cl, 1:length(onl), function(i) {
    if (!file.exists(ofl[i]) | overwrite)
      jnk <- utils::download.file(onl[i], ofl[i], mode = "wb")
    
    return(ofl[i])
  })
}


### helper function to create TRMM daily or 3-hourly filenames -----

getTRMMFiles <- function(begin, end, type = c("daily", "3-hourly"), 
                         xml = FALSE) {

  ## trmm ftp server
  ftp <- "ftp://disc3.nascom.nasa.gov/data/s4pa/TRMM_L3/"
  
  ## daily product
  if (type[1] == "daily") {  
    
    # online filepath to daily files
    ftp <- paste0(ftp, "TRMM_3B42_Daily.7")
    
    # create target filenames
    sqc <- seq(begin, end, "day")
    
    onl <- strftime(sqc, "%Y/%m/3B42_Daily.%Y%m%d.7.nc4")
    onl <- paste(ftp, onl, sep = "/")
    if (xml) onl <- sort(c(onl, paste0(onl, ".xml")))
   
  ## 3-hourly product   
  } else if (type[1] == "3-hourly") {
    
    # online filepath to 3-hourly files
    ftp <- paste0(ftp, "TRMM_3B42.7/")

    # create target folder structure
    begin <- as.POSIXct(paste(begin, "00:00:00"))
    end <- as.POSIXct(paste(end, "21:00:00"))
    sqc <- seq(begin, end, "3 hours")
    
    ftp <- paste0(ftp, strftime(sqc, "%Y/%j"))
    
    ## create target filenames
    sqc <- sqc + 3 * 60 * 60
    onl <- paste0(ftp, strftime(sqc, "/3B42.%Y%m%d.%H.7.HDF"))

    if (xml) onl <- sort(c(onl, paste0(onl, ".xml")))
    
  ## product not available  
  } else {
    stop("Specified product not available, see ?downloadTRMM for available options.\n")
  }
 
  return(onl)   
}