#' Split a Raster object
#' 
#' @description
#' Split one or multiple \code{Raster*} object(s) into a given number of 
#' horizontal and vertical slices. Inspired by 
#' \url{http://java.dzone.com/articles/use-gdal-r-console-split}. 
#' 
#' @param x Character. File(s) to process. 
#' @param s Integer. Number of slices 'x' should be divided into. Note that the 
#' default setting (\code{s = 2}) splits 'x' into four equally sized tiles, i.e. 
#' two in x-direction and two in y-direction.
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
splitRaster <- function(x, s = 2, ...) {
  
  # t is nr. of iterations per side
  t <- s - 1   # no. of iterations per side
  
  lst_out <- lapply(1:length(x), function(h) {
    
    # gdalinfo
    nfo <- rgdal::GDALinfo(x[h])
    
    # pick size of each side
    cols <- nfo[2]
    rows <- nfo[1]
    
    ls_split <- lapply(0:t, function(i) {
      lapply(0:t, function(j) {
        # output file
        dst_dataset_prf <- substr(x[h], 1, nchar(x[h])-4)
        dst_dataset_prf <- paste(dst_dataset_prf, i, j, sep = "_")
        dst_dataset_sff <- substr(x[h], nchar(x[h])-3, nchar(x[h]))
        dst_dataset <- paste0(dst_dataset_prf, dst_dataset_sff)
        # location of subwindow
        srcwin <- c(i * cols/s, j * rows/s, cols/s, rows/s)
        
        # execute split
        gdalUtils::gdal_translate(x[h], dst_dataset, srcwin = srcwin, 
                                  output_Raster = TRUE, ...)
      })
    })
    
    unlist(ls_split)
  })
  
  return(lst_out)
}
