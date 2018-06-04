#' combine multiple lattice plots layerwise
#' 
#' @description
#' this function combines multiple lattice plot objects drawing 
#' each as a layer on top of the previous plots. Note that the 
#' global plot settings (e.g. xlim, ylim, ...) is taken from the 
#' first object. This is particularly useful when looping over large amounts of data
#' using \code{\link{lapply}} (see examples).
#' 
#' @param trellis.list a list containing lattice plot objects
#' @param ... additional arguments passed to \code{\link{as.layer}}
#' 
#' @return
#' a single lattice plot object
#' 
#' @author
#' Tim Appelhans
#' 
#' @name latticeCombineLayer-deprecated
#' @usage latticeCombineLayer(trellis.list, ...)     
#' @seealso \code{\link{Rsenal-deprecated}}  
#' @keywords internal
NULL

#' @rdname Rsenal-deprecated
#' @section \code{latticeCombineLayer}:
#' For \code{latticeCombineLayer}, use \code{\link[Orcs]{latticeCombineLayer}} 
#' instead.
#' 
#' @export 
latticeCombineLayer <- function(trellis.list, ...) {
  
  .Deprecated("Orcs::latticeCombineLayer", "Rsenal")
  Orcs::latticeCombineLayer(trellis.list, ...)
}