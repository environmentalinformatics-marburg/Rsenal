#' Download GIMMS 3G data
#' 
#' @description
#' Download GIMMS 3G binary data for a given time span from NASA FTP server 
#' (\url{http://ecocast.arc.nasa.gov/data/pub/gimms/3g/}).
#' 
#' @param decades Numeric. Decadal data to download. If not supplied, the entire
#' GIMMS 3G dataset will be acquired, i.e. \code{decades = seq(1980, 2010, 10)}.
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
#' downloadGimms(decades = c(1990, 2000), 
#'               dsn = paste(getwd(), "data", sep = "/"))
#'               
#' @export downloadGimms
#' @aliases downloadGimms
downloadGimms <- function(decades, 
                          dsn = ".",
                          ...) {
  
  # URL of GIMMS 3G FTP server
  gimms_ftp <- "http://ecocast.arc.nasa.gov/data/pub/gimms/3g/"
  
  # Loop through decades
  gimms_fls <- lapply(decades, function(yr) {
    # Download list of avl files from current download directory
    tmp_dir <- paste0(gimms_ftp, yr, "s_new")
    download.file(tmp_dir, 
                  destfile = paste0(dsn, "/", basename(tmp_dir), ".txt"))
    
    # Extract and import avl files from downloaded list
    lns <- readLines(paste0(dsn, "/", basename(tmp_dir), ".txt"))
    id <- which(substr(lns, 1, 4) == "<img")
    lns <- lns[id]
    
    # Download avl files if not present in `dsn`
    tmp_fls <- substr(lns, 53, 72)
    tmp_url <- paste(tmp_dir, tmp_fls, sep = "/")
    
    for (i in tmp_url) {
      destfile <- paste0(dsn, "/", basename(i))
      if (file.exists(destfile)) {
        cat("File", destfile, "already exists in destination folder. Proceeding to next file ...\n")
      } else {
        try(download.file(i, destfile = destfile), silent = TRUE)
      }
    }
    
    # Remove list of avl files
    file.remove(paste0(dsn, "/", basename(tmp_dir), ".txt"))
    
    tmp_fls <- paste0(dsn, "/", tmp_fls)
    return(tmp_fls)
  })
  
  gimms_fls <- unlist(gimms_fls)
  return(gimms_fls)
  
}