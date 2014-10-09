#' Rasterize GIMMS 3G binary data
#' 
#' @description
#' Convert GIMMS 3G binary data to an object of class \code{raster} by the use 
#' of an ENVI header file.
#' 
#' @param file Character. GIMMS binary file to rasterize. 
#' @param headerfile Character. Companion header file passed on to 
#' \code{\link{read.ENVI}}. Usually created from \code{\link{createHdr}}. 
#' @param water2na Logical. If TRUE, pixels holding 'water-mask' value (-10000) 
#' are set to NA. 
#' @param nodata2na Logical. If TRUE, pixels holding 'nodata-mask' value (-5000)
#' are set to NA.
#' @param scaling Logical. If TRUE, initial values are multiplied by scaling
#' factor (1/10000). 
#' @param ... Further arguments. Currently not in use. 
#' 
#' @return
#' A \code{RasterLayer} object.
#' 
#' @author
#' Florian Detsch
#' 
#' @seealso
#' \code{\link{createHdr}}, \code{\link{read.ENVI}}
#' 
#' @examples  
#' # Download GIMMS 3G data from 1990 to 2009 (this might take some time...)
#' fls_gimms <- downloadGimms(decades = c(1990, 2000), 
#'                            dsn = paste(getwd(), "data", sep = "/"))
#'                       
#' # Create companion header file                                 
#' fls_hdr <- createHdr()        
#' 
#' # Rasterize selected GIMMS data
#' rst_gimms <- rasterizeGimms(file = fls_gimms[1], 
#'                             headerfile = fls_hdr)
#'                             
#' plot(rst_gimms)                                     
#'               
#' @export rasterizeGimms
#' @aliases rasterizeGimms
rasterizeGimms <- function(file, 
                           headerfile,
                           water2na = TRUE, 
                           nodata2na = TRUE,
                           scaling = TRUE,
                           ...) {
  
  # Required packages
  library(caTools)
  library(raster)
  
  # Import ENVI binary data
  mat <- read.ENVI(file, headerfile)
  
  # Flip matrix
  mat <- t(mat)
  
  # Setup raster template 
  ext_tmp <- extent(c(-180 + 1/24, 180 - 1/24, -90 + 1/24, 90 - 1/24))
  rst_tmp <- raster(nrows = dim(mat)[1], ncols = dim(mat)[2], 
                    ext = ext_tmp)
  
  # Insert matrix values into raster template
  rst <- raster(mat, template = rst_tmp)
  
  # Water -> NA (optional)
  if (water2na)
    rst[rst[] == -10000] <- NA
  
  # Nodata -> NA (optional)
  if (nodata2na)
    rst[rst[] == -5000] <- NA
  
  # Scaling (optional)
  if (scaling)
    rst <- rst / 10000

  # Return raster
  return(rst)
}