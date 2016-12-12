#' Download TRMM 3B42 Daily Data
#' 
#' @description
#' Download TRMM 3B42 daily NetCDF files for a given time span from the NASA FTP 
#' servers (\url{ftp://disc3.nascom.nasa.gov/data/s4pa/TRMM_L3/TRMM_3B42_Daily.7/}).
#' 
#' @param begin,end Start and end date as \code{Date} or \code{character}. 
#' @param dsn \code{character}. Download folder, defaults to the current working 
#' directory. 
#' @param xml \code{logical}, defaults to \code{FALSE}. If \code{TRUE}, .xml 
#' files associated with each .nc4 file are also downloaded to 'dsn'.
#' @param overwrite \code{logical}, defaults to \code{FALSE}. Determines whether 
#' existing files are overwritten. 
#' @param cores \code{integer}. Number of cores for parallel processing. Note 
#' that this takes only effect if a sufficiently fast internet connection is 
#' available.
#' @param ... Additional arguments passed to \code{\link{as.Date}}.
#' 
#' @return
#' A \code{character} vector of filepaths.
#' 
#' @author
#' Florian Detsch
#' 
#' @seealso
#' \code{\link{download.file}}.
#' 
#' @examples  
#' \dontrun{
#' ## download TRMM 3B42 data from Jan 1 to Jan 5, 2015
#' downloadTRMM(begin = "2015-01-01", end = "2015-01-05")
#' }
#'               
#' @export downloadTRMM
#' @aliases downloadTRMM
downloadTRMM <- function(begin, end, dsn = ".", xml = FALSE, overwrite = FALSE, 
                         cores = 1L, ...) {
  
  ## transform 'begin' and 'end' to 'Date' object if necessary
  if (!inherits(begin, "Date"))
    begin <- as.Date(begin, ...)
  
  if (!inherits(end, "Date"))
    end <- as.Date(end, ...)
  
  ## trmm ftp server
  ftp <-"ftp://disc3.nascom.nasa.gov/data/s4pa/TRMM_L3/TRMM_3B42_Daily.7"
  
  ## online and offline target files
  sqc <- seq(begin, end, "day")

  onl <- strftime(sqc, "%Y/%m/3B42_Daily.%Y%m%d.7.nc4")
  onl <- paste(ftp, onl, sep = "/")
  if (xml) onl <- sort(c(onl, paste0(onl, ".xml")))

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