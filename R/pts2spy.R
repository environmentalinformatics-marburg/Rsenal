#' Convert points to SpatialPolygons*
#' 
#' @description 
#' Create a \code{SpatialPolygons*} object from a set of point coordinates in 
#' one go, i.e. without being required to run through the single steps outlined 
#' in \code{\link{SpatialPolygons}}.
#' 
#' @param coords 2-column \code{numeric} matrix with x and y coordinates.
#' @param hole \code{logical}, see \code{\link{Polygon}}.
#' @param ID \code{character}, see \code{\link{Polygons}}.
#' @param data \code{data.frame} with data to add to the output 
#' \code{SpatialPolygons*} object (optional). 
#' @param match.ID \code{logical}, see \code{\link{SpatialPolygonsDataFrame}}.
#' 
#' @return If \code{data} is missing, a \code{SpatialPolygons} object; else a 
#' \code{SpatialPolygonsDataFrame} object.
#' 
#' @author 
#' Florian Detsch
#' 
#' @seealso 
#' \code{\link{SpatialPolygons-class}}, \code{\link{SpatialPolygonsDataFrame}}.
#' 
#' @examples 
#' coords1 <- cbind(c(2, 4, 4, 1, 2), c(2, 3, 5, 4, 2))
#' spy1 <- pts2spy(coords1, ID = "A")
#'
#' coords2 <- cbind(c(5, 4, 2, 5), c(2, 3, 2, 2))
#' spy2 <- pts2spy(coords2, ID = "B")
#' 
#' plot(spy1, col = "grey75")
#' plot(spy2, col = "grey25", add = TRUE)
#' 
#' @export pts2spy
#' @name pts2spy
pts2spy <- function(coords, hole = NA, ID, data, match.ID = TRUE, ...) {
  
  # 'data.frame' to 'Polygons'
  py <- sp::Polygon(coords, hole)
  pys <- sp::Polygons(list(py), ID)
  
  # 'Polygons' to 'SpatialPolygons'
  spy <- sp::SpatialPolygons(list(pys), ...)
  
  if (!missing("data")) {
    return(sp::SpatialPolygonsDataFrame(spy, data, match.ID))
  } else {
    return(spy)
  }
}