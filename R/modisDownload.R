#' Download and process MODIS *.hdf data.
#' 
#' @description
#' This is a wrapper function to download and optionally process MODIS *.hdf 
#' data by invoking \code{\link{getHdf}} and \code{\link{runGdal}} from the
#' 'MODIS' package.
#' 
#' @param modis.products Character. A vector containing the names of the desired 
#' MODIS products. See \code{\link{getProduct}} for further information.
#' @param modis.download.only Logical, default is TRUE. Download only or 
#' download and process desired MODIS data.
#' @param modis.outproj Character, default is "asIn" (i.e. MODIS Sinusoidal, 
#' EPSG:7030). Used to define the output projection of processed raster data.
#' @param modis.job Character, default is NULL. Job name passed on to 
#' \code{\link{runGdal}}. If not specified, a subfolder holding the current
#' system timestamp will be created in getOption("MODIS_outDirPath"). 
#' @param ... Additional arguments passed to \code{\link{getHdf}} and 
#' \code{\link{runGdal}}, respectively.
#' 
#' @return
#' A vector of filenames.
#' 
#' @author
#' Florian Detsch
#' 
#' @seealso
#' \code{\link{getProduct}}, \code{\link{getHdf}}, \code{\link{runGdal}}
#' 
#' @examples
#' # Set MODIS paths
#' MODISoptions(localArcPath = paste(getwd(), "MODIS_ARC", sep = "/"), 
#'              outDirPath = paste(getwd(), "MODIS_ARC/PROCESSED", sep = "/"))
#' 
#' # Download MOD14A1 and MYD14A1 (Thermal Anomalies & Fire Daily L3 Global 1km)
#' # for the Kilimanjaro region from 2013-05-20 to 2013-06-09                          
#' modisDownload(modis.products = c("MOD14A1", "MYD14A1"), 
#'               modis.download.only = TRUE,
#'               tileH = "21", tileV = "09",
#'               begin = "2013140", end = "2013160")
#'              
#' # Download MOD14A1 and MYD14A1, extract and reproject SDS layers 1 and 2 to 
#' # EPSG:21037 (Arc 1960 / UTM zone 37S)                            
#' modisDownload(modis.products = c("MOD14A1", "MYD14A1"), 
#'               modis.download.only = FALSE,
#'               SDSstring = "1100",
#'               modis.outproj = "21037",
#'               tileH = "21", tileV = "09",
#'               begin = "2013140", end = "2013160", 
#'               modis.job = "md14_tmp")
#' 
#' @export modisDownload
#' @aliases modisDownload
modisDownload <- function(modis.products, 
                          modis.download.only = TRUE,
                          modis.outproj = "asIn",
                          modis.job = NULL,
                          ...) {
  
  pkgs <- c("MODIS")
  tmp <- sapply(pkgs, function(x) library(x, character.only = TRUE))

  # Download MODIS data with given extent
  if (modis.download.only) {
    fls <- lapply(modis.products, function(i) {
      jnk <- getHdf(i, ...)
      jnk <- list.files(getOption("MODIS_localArcPath"), 
                        pattern = paste(i, ".hdf", sep = ".*"), 
                        recursive = TRUE, full.names = TRUE)
      return(jnk)
    })
    
  # Download and process MODIS data
  } else {
    # Extract specified SDS from .hdf files
    fls <- lapply(modis.products, function(i) {
      # Generate 'job' name (if not user-specified)
      if (is.null(modis.job)) {
        timestamp <- strftime(Sys.time(), format = "%Y%m%d%H%M")
        modis.job <- paste(i, getCollection(i), timestamp, sep = "_")
      }
        
      jnk <- runGdal(i, outProj = modis.outproj, job = modis.job, ...)
      jnk <- list.files(paste0(getOption("MODIS_outDirPath"), modis.job), 
                        pattern = i, recursive = TRUE, full.names = TRUE)
      return(jnk)
    }) 
  }
  
  # Name returned list of filenames according to supplied MODIS products
  names(fls) <- modis.products

  return(fls)
}
