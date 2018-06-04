#' combine multiple lattice plots in a facetted grid (panels)
#' 
#' @description
#' this function combines multiple lattice plot objects in a facetted
#' grid. Note that the global plot settings (e.g. xlim, ylim, ...) 
#' are taken from the first object though the user can specify whether
#' \code{scales} should be identical or not. 
#' This is particularly useful when looping over large amounts of data
#' using \code{\link{lapply}} (see examples).
#' 
#' @param trellis.list a list containing lattice plot objects
#' @param between space between panels
#' @param as.table if TRUE (default) drawing is top left to bottom right
#' @param ... additional arguments passed to \code{\link{c.trellis}}
#' 
#' @return
#' a single lattice plot object
#' 
#' @author
#' Tim Appelhans
#' 
#' @name latticeCombineGrid-deprecated
#' @usage latticeCombineGrid(trellis.list, between = list(y = 0.3, x = 0.3)
#' , as.table = TRUE, ...)     
#' @seealso \code{\link{Rsenal-deprecated}}  
#' @keywords internal
NULL

#' @rdname Rsenal-deprecated
#' @section \code{latticeCombineGrid}:
#' For \code{latticeCombineGrid}, use \code{\link[Orcs]{latticeCombineGrid}} 
#' instead.
#' 
#' @export 
latticeCombineGrid <- function(trellis.list,
                               between = list(y = 0.3, x = 0.3),
                               as.table = TRUE,
                               ...) {
  
  .Deprecated("Orcs::latticeCombineGrid", "Rsenal")
  Orcs::latticeCombineGrid(trellis.list, between, as.table, ...)
}