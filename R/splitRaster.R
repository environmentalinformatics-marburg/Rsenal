#' Split a Raster object
#' 
#' @description
#' Split a Raster* object into a given number of horizontal and vertical slices.
#' Inspired by \url{http://java.dzone.com/articles/use-gdal-r-console-split}. 
#' 
#' @param file Character. Filename. 
#' @param s Integer. Number of slices \code{file} should be divided into.
#' @param ... Further arguments passed on to \code{\link{gdal_translate}}. 
#' 
#' @return
#' A list of RasterBrick objects.
#' 
#' @author
#' Florian Detsch
#' 
#' @seealso
#' \code{\link{gdal_translate}}, \code{\link{GDALinfo}}
#'  
#' @export splitRaster
#' @aliases splitRaster
splitRaster <- function(file, s = 2, ...) {
  
  # gdalinfo
  nfo <- rgdal::GDALinfo(file)
  
  # pick size of each side
  x <- nfo[2]
  y <- nfo[1]
  
  # t is nr. of iterations per side
  t <- s - 1   # no. of iterations per side
  
  ls_split <- lapply(0:t, function(i) {
    lapply(0:t, function(j) {
      # output file
      dst_dataset_prf <- substr(file, 1, nchar(file)-4)
      dst_dataset_prf <- paste(dst_dataset_prf, i, j, sep = "_")
      dst_dataset_sff <- substr(file, nchar(file)-3, nchar(file))
      dst_dataset <- paste0(dst_dataset_prf, dst_dataset_sff)
      # location of subwindow
      srcwin <- c(i * x/s, j * y/s, x/s, y/s)
      
      # execute split
      gdalUtils::gdal_translate(file, dst_dataset, srcwin = srcwin, 
                                output_Raster = TRUE, ...)
    })
  })
  
  ls_split <- unlist(ls_split)
  return(ls_split)
}
