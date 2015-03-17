#' compare two density distributions side by side
#' 
#' @description 
#' this function will produce a plot of two density functions displayed 
#' side by side
#' 
#' @param left numeric vector
#' @param right numeric vector
#' @param add.spred logical, whether to plot the spread (q25 to q75 and the median)
#' @param clrs a character vector of length 2 specifying the colors 
#' for the filled density regions
#' @param ... currently not used
#' 
#' @return
#' A trellis object
#' 
#' @author
#' Tim Appelhans
#' 
#' @examples
#' compareDistributions(rnorm(1000, 2, 3), rnorm(1000, 8, 1))
#' compareDistributions(rnorm(1000, 2, 3), rnorm(1000, 8, 1), 
#'                      add.spread = FALSE)
#' compareDistributions(rnorm(1000, 2, 3), rnorm(1000, 8, 1), 
#'                      add.spread = TRUE, clrs = c("red", "brown"))
#' 
#' @export compareDistributions
#' @aliases compareDistributions

compareDistributions <- function(left, 
                                 right,
                                 add.spread = TRUE,
                                 clrs = c("purple", "lightblue"),
                                 xlab = "density",
                                 ylab = "value",
                                 ...) {
  
  library(latticeExtra)
  
  left_x <- density(left)$x
  left_y <- -density(left)$y
  right_x <- density(right)$x
  right_y <- density(right)$y
  
  x_lim <- c(-sort(abs(c(range(left_y), range(right_y))), 
                   decreasing = TRUE)[1] * 1.1,
             sort(abs(c(range(left_y), range(right_y))), 
                  decreasing = TRUE)[1] * 1.1)
  y_lim <- c(sort(c(range(left_x), range(right_x)))[1] * 1.1,
             sort(abs(c(range(left_x), range(right_x))), 
                  decreasing = TRUE)[1] * 1.1)
  
  
  p <- xyplot(left_x ~ left_y, xlim = x_lim, ylim = y_lim, type = "l",
              panel = panel.polygon, col = clrs[1], xlab = xlab,
              ylab = ylab) +
    as.layer(xyplot(right_x ~ right_y, xlim = x_lim, type = "l", 
                    panel = panel.polygon, col = clrs[2])) 
  
  if (add.spread) {
    
    mean_left <- mean(left_x, na.rm = TRUE)
    mean_right <- mean(right_x, na.rm = TRUE)
    med_left <- median(left_x, na.rm = TRUE)
    med_right <- median(right_x, na.rm = TRUE)
    q25_left <- quantile(left_x, 0.25)
    q25_right <- quantile(right_x, 0.25)
    q75_left <- quantile(left_x, 0.75)
    q75_right <- quantile(right_x, 0.75)
    
    pol_left <- list(x = c(0.04 * x_lim[1], 0, 0, 0.04 * x_lim[1]),
                     y = c(q25_left, q25_left, q75_left, q75_left))
    
    pol_right <- list(x = c(0, 0.04 * x_lim[2], 0.04 * x_lim[2], 0),
                      y = c(q25_right, q25_right, q75_right, q75_right))
    
  
    p <- p + as.layer(xyplot(pol_left$y ~ pol_left$x, type = "l",
                             panel = panel.polygon, col = "grey60")) +
      as.layer(xyplot(pol_right$y ~ pol_right$x, type = "l",
                      panel = panel.polygon, col = "grey60")) +
      as.layer(xyplot(med_left ~ 0.02 * x_lim[1], pch = 22, 
                     fill = "white", col = "black")) +
      as.layer(xyplot(med_right ~ 0.02 * x_lim[2], pch = 22, 
                      fill = "white", col = "black"))
  }
  
  return(p)
  
}
  