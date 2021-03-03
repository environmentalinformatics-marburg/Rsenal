if (!isGeneric("vvi")) {
  setGeneric(
    "vvi"
    , function(
      x
      , ...
    ) {
      standardGeneric("vvi")
    }
  )
}

#' Visible vegetation index
#' 
#' @description
#' This function calculates a visible vegetation index (VVI) from a RGB 
#' \code{Raster*} object.  
#' 
#' @param x A \code{Raster} or \code{stars} object.
#' @param r,g,b Band numbers as \code{integer}.
#' 
#' @return 
#' A VVI \code{RasterLayer} or \code{stars} object.
#' 
#' @author
#' Florian Detsch, Tim Appelhans
#' 
#' @references
#' Planetary Habitability Laboratory (2015): Visible Vegetation Index (VVI). 
#' Available online: \url{http://phl.upr.edu/projects/visible-vegetation-index-vvi}.
#' 
#' @examples
#' library(RColorBrewer)
#' 
#' data(gmap_hel)
#' plotRGB(gmap_hel)
#'
#' gmap_hel_veg = vvi(gmap_hel)
#' plot(gmap_hel_veg, col = brewer.pal(5, "BrBG"), alpha = .5, add = TRUE)
#' 
#' @export vvi
#' @name vvi


### 0 MISSING ====

#' @aliases vvi,missing-method
#' @rdname vvi
methods::setMethod(
  "vvi"
  , methods::signature(
    x = "missing"
  )
  , function(
    r = 1
    , g = 2
    , b = 3
  ) {
    return(
      (1 - abs((r - 30) / (r + 30))) * 
        (1 - abs((g - 50) / (g + 50))) * 
        (1 - abs((b - 1) / (b + 1)))
    )
  }
)


### 1 RASTER ====

#' @aliases vvi,Raster-method
#' @rdname vvi
methods::setMethod(
  "vvi"
  , methods::signature(
    x = "Raster"
  )
  , function(
    x
    , r = 1
    , g = 2
    , b = 3
  ) {
    return(
      vvi(
        r = x[[r]]
        , g = x[[g]]
        , b = x[[b]]
      )
    )
  }
)


### 2 STARS ====

methods::setOldClass("stars")

#' @aliases vvi,stars-method
#' @rdname vvi
methods::setMethod(
  "vvi"
  , methods::signature(
    x = "stars"
  )
  , function(
    x
    , r = 1
    , g = 2
    , b = 3
  ) {
    out = vvi(
      r = x[, , , r]
      , g = x[, , , g]
      , b = x[, , , b]
    )
    names(out) = "vvi"
    
    ## keep x, y dimensions only
    stars::st_redimension(
      out
      , new_dims = stars::st_dimensions(
        out
      )[1:2, ]
    )
  }
)
