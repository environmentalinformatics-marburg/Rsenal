#' Shadow mask from RGB (satellite) imagery
#' 
#' @description
#' This function builds a shadow mask from a RGB \code{Raster*} object.  
#' 
#' @param x A \code{RasterStack} or \code{RasterBrick} object. 3
#' bands are mandatory (usually red, green and blue).
#' @param n Integer, defaults to '1'. Number of iterations to distinguish 
#' between shadow and non-shadow pixels. The more iterations, the fewer shadow 
#' pixels will remain.
#' @param ... Further arguments passed on to \code{\link{rgb2YCbCr}}.
#' 
#' @return 
#' A shadow mask \code{RasterLayer}.
#' 
#' @author
#' Florian Detsch, Tim Appelhans
#' 
#' @references
#' Deb, K. and Suny, A.H. (2014): Shadow Detection and Removal Based on YCbCr Color Space. Smart Computing Review 4, 23-33, doi:10.6029/smartcr.2014.01.003.
#' 
#' @seealso \code{\link{rgb2YCbCr}}
#' 
#' @examples
#' library(RColorBrewer)
#' 
#' data(gmap_hel)
#' plotRGB(gmap_hel)
#' 
#' gmap_hel_wsh <- rgbShadowMask(gmap_hel)
#' plot(gmap_hel_wsh, axes = FALSE, legend = FALSE, 
#'      col = c(rev(brewer.pal(9, "Greys"))), alpha = .25, add = TRUE)
#' 
#' @export rgbShadowMask
#' @aliases rgbShadowMask
rgbShadowMask <- function(x, n = 1L, ...) {

  ### prerequisites
  
  ## compatibility check
  if (raster::nlayers(x) < 3)
    stop("Argument 'x' needs to be a Raster* object with at least 3 layers (usually red, green and blue).")
  
  ## 3-by-3 focal matrix
  mat_w3by3 <- matrix(rep(1, 9), ncol = 3)
  
  
  ### processing
  
  ## rgb to ycbcr color space
  rst_ycbcr <- rgb2YCbCr(x = x, ...)
  
  ## initial threshold
  y <- rst_ycbcr[[1]]
  num_mu_y <- mean(y[], na.rm = TRUE)
  num_sd_y <- sd(y[], na.rm = TRUE)
  num_trsh <- num_mu_y - num_sd_y
  
  ## rejection of focal means based on calculated threshold
  rst_fcl_mu <- raster::focal(y, w = mat_w3by3, fun = mean, na.rm = TRUE, pad = TRUE)
  rst_fcl_mu[rst_fcl_mu[] < num_trsh] <- 0
  rst_fcl_mu[rst_fcl_mu[] > 0] <- 1

  ## iteratively identify shadow
  if (n > 0) {
    for (i in 1:n) {
      
      ## modified threshold
      y_msk <- y[rst_fcl_mu[] == 0]
      num_mu_y_msk <- mean(y_msk, na.rm = TRUE)
      num_sd_y_msk <- sd(y_msk, na.rm = TRUE)
      num_trsh_msk <- num_mu_y_msk - num_sd_y_msk
      
      ## rejection of focal means based on modified threshold
      rst_fcl_mu_msk <- raster::focal(y, w = mat_w3by3, fun = mean, na.rm = TRUE, pad = TRUE)
      rst_fcl_mu_msk[rst_fcl_mu_msk[] < num_trsh_msk] <- 0
      rst_fcl_mu_msk[rst_fcl_mu_msk[] > 0] <- 1
      
      if (any(rst_fcl_mu_msk[] == 0)) {
        rst_fcl_mu <- rst_fcl_mu_msk
      } else {
        cat("Break at iteration # ", i, ": there will be no shadow pixels left after next iteration.", sep = "")
        break
      }
    }
  }
  
  ## return shadow mask
  return(rst_fcl_mu)
}