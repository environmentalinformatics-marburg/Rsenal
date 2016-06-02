#' column/row coordinates to geos projection
#'
#' @description
#' Converts column/row coordinates of MSG images to geos projection.
#' For HRV use COFF=5566 and ColumnDirGridStep=1000.13434886932 instead of the 
#' default values.
#' @param x RasterLayer or RasterStack of the msg data where extent is defined in
#'  column/row coordinates
#' @param COFF see description
#' @param ColumnDirGridStep see description
#' @return
#' RasterLayer or RasterStack in Geos projection
#'
#' @author
#' Hanna Meyer and Johannes Droenner
#' @examples
#' cr2Geos(raster(extent(2200, 3000, 2450, 3050)))
#' 
#' @export cr2Geos
#' @aliases cr2Geos


cr2Geos <- function (x, COFF=1856, ColumnDirGridStep=3000.40316581726){
  x_coord_min <- extent(x)@xmin
  x_coord_max <- extent(x)@xmax
  y_coord_max <- extent(x)@ymin
  y_coord_min <- extent(x)@ymax
  extent(x) <- c((x_coord_min - COFF - 0.5) *ColumnDirGridStep,
                 (x_coord_max - COFF - 0.5) *ColumnDirGridStep,
                 (COFF - y_coord_min + 0.5) *ColumnDirGridStep,
                 (COFF - y_coord_max + 0.5) *ColumnDirGridStep)
  proj4string(x)<-CRS("+proj=geos +lon_0=0 +h=35785831 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
  return(x)
}


