#' column/row coordinates to geos projection
#'
#' @description
#' Converts column/row coordinates of MSG images to geos projection.
#' For HRV use COFF=5566 and ColumnDirGridStep=1000.13434886932 instead of the 
#' default values.
#' @param cr Numeric value representing the column/row coordinate of a MSG image
#' @return
#' Numeric value representing the column/row coordinate in Geos projection
#'
#' @author
#' Hanna Meyer and Johannes Drönner
#' @examples
#' cr2Geos(2325)
#' 
#' @export cr2Geos
#' @aliases cr2Geos

cr2Geos <- function (cr, COFF=1856, ColumnDirGridStep=3000.40316581726){
  geoscoord <- (cr-COFF)*ColumnDirGridStep
  return(geoscoord)
}