#' Determine a polygon's centroid
#' 
#' @description
#' Determine a polygon's centroid based on information stored in slot 'labpt'.
#' Taken from \url{http://linkedscience.org/tools/sparql-package-for-r/tutorial-part-ii-calculating-deforestation-per-state/}. 
#' 
#' @param spP SpatialPolygons or SpatialPolygonsDataFrame.
#' 
#' @return
#' A numeric matrix of x and y coordinates.
#' 
#' @author
#' Florian Detsch
#' 
#' @seealso
#' \code{\link{SpatialPolygons}}, \code{\link{SpatialPolygonsDataFrame}}
#' 
#' @export getCentroids
#' @aliases getCentroids
getCentroids <- function(spP) {
  
  t(sapply(spP@polygons, function(x) x@labpt))
  
}