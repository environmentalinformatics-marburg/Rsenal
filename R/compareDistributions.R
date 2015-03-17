

compareDistributions <- function(left, 
                                 right, 
                                 add.boxplot = FALSE,
                                 add.spread = FALSE,
                                 add.mean = FALSE,
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
              ylab = ylab, ...) +
    as.layer(xyplot(right_x ~ right_y, xlim = x_lim, type = "l", 
                    panel = panel.polygon, col = clrs[2])) 
  
  return(p)
  
}
  