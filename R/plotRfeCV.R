#' plot cross-validated variable importance
#' 
#' @description 
#' this function plots variable imporatnce for cross-validated 
#' \code{\link{rfe}} variable selection classes. It uses the k cross-validations
#' to compute the mean +/- sd error metric.
#' 
#' @param rfe.model a rfe model. See \code{\link{rfe}}
#' @param metric the metric to be used. Note this needs to be the metric used 
#' to calculate the \code{\link{rfe}} model
#' @param xlim the xlim for the plot
#' @param ylim the ylim for the plot  
#' 
#' @return
#' a trellis object
#' 
#' @author
#' Hanna Meyer, Tim Appelhans
#' 
#' @export plotRfeCV
#' @aliases plotRfeCV

plotRfeCV <- function(rfe.model,
                      metric = rfe.model$metric,
                      xlim = "minmax",
                      ylim = "minmax") {

  data <- as.data.frame(rfe.model$resample)
  sdv <- c()
  means <- c()

  for (i in unique(data$Variables)) {
    sdv <- c(sdv,
             sd(eval(parse(text = paste("data$",
                                        metric)))[data$Variables == i]))
    means <- c(means,
               mean(eval(parse(text = paste("data$",
                                            metric)))[data$Variables==i]))
  }

  # input_list <- list(...)
  if (xlim == "minmax"){
    xlim <- c(min(data$Variables) - 0.4, max(data$Variables) + 0.4)
  }
  if (ylim=="minmax"){
    ylim <- c(min(means - sdv) - 0.1 * min(means - sdv),
              max(means + sdv) + 0.1 * min(means - sdv))
  }

  xyplot(means ~ unique(data$Variables),
         ylim = ylim,
         xlim = xlim,
         xlab = "Number of Variables",
         ylab = paste0(metric, " (Cross-Validation)"),
         panel = function(x, y, ...) {
           panel.polygon(c(unique(data$Variables),
                           rev(unique(data$Variables))),
                         c(means + sdv, rev(means - sdv)),
                         col = "grey80",
                         border = FALSE)
           panel.xyplot(x, y, type = c("b", "g"), col = "black", pch = 16)
         }
  )
}
