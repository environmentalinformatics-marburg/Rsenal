#' calculate prediction performance statistics for classification models 
#' 
#' @description
#' this function calculates prediction performance statistics
#' between vectors of predicted and observed values. Users may also create 
#' a dotplot visualising the results.
#' 
#' @param prd factor vector of predicted values with two levels
#' @param obs factor vector of observed values with two levels
#' @param prob optional. Predicted probabilities for the first class
#' @param plot logical, whether to produce a visualisation of the results.
#' Defaults to FALSE
#' 
#' @return
#' If \code{plot = FALSE} (the default), a data frame. 
#' If \code{plot = TRUE}, a list with components \code{stats} - data frame
#' and \code{plot} - a trellis plot object.
#' 
#' @author
#' Hanna Meyer and Tim Appelhans
#' 
#' @examples
#' #create two random vectors with classes "yes" and "no" to simulate a model
#' #with random performance. Expected POD and PFD  
#' pred_vals <- factor(sample(c("Yes","No"), 50, replace = TRUE),levels=c("Yes","No"))
#' obs_vals <- factor(sample(c("Yes","No"), 50, replace = TRUE),levels=c("Yes","No"))
#' 
#' result <- classificationStats(pred_vals, obs_vals, plot=TRUE)
#' result$plot
#' result$stats
#' 
#' @export classificationStats
#' @aliases classificationStats
#' @seealso \code{\link{regressionStats}}

classificationStats <- function(prd, obs, prob=NULL, plot=FALSE) {
  tab <- (table(prd,obs))/100
  TP <- tab[1,1]
  FP <- tab[1,2]
  TN <- tab[2,2]
  FN <- tab[2,1]
  
  bias <- (TP+FP)/(TP+FN)
  POD <- TP/(TP+FN)
  PFD <- FP/(FP+TN)
  FAR <- FP/(TP+FP)
  CSI <- TP/(TP+FP+FN)
  ph <- ((TP+FN)*(TP+FP))/(sum(tab))
  ETS <- (TP-ph)/((TP+FP+FN)-ph)
  HSS <- (TP*TN-FP*FN)/(((TP+FN)*(FN+TN)+(TP+FP)*(FP+TN))/2)
  HKD <- (TP/(TP+FN))-(FP/(FP+TN))
  if (!is.null(prob)){
    AUC <- as.numeric(pROC::roc(obs,prob)$auc)
    df_all <- data.frame(bias,PFD,FAR,POD,CSI,ETS,HSS,HKD,AUC)
    names(df_all) <- c("Bias","PFD","FAR","POD","CSI","ETS","HSS","HKD","AUC")
  }else{
  df_all <- data.frame(bias,PFD,FAR,POD,CSI,ETS,HSS,HKD)
  names(df_all) <- c("Bias","PFD","FAR","POD","CSI","ETS","HSS","HKD")
  }
  
  if (plot) {
    df_melt <- reshape2::melt(df_all)
    ## panel.fun modified from 
    ## http://thebiobucket.blogspot.de/2011/04/r-graphs-lattice-use-of-panel-functions.html
    panel.fun <- function(...) {
      if (panel.number() == 1) { 
        at<-pretty(rng)
        panel.axis("top", at = at, outside = FALSE,
                   labels = TRUE, half = FALSE)
        panel.abline(v = 0, lty = 3, lwd = 1)
        panel.dotplot(..., lwd = 0.5)
      }
      if (panel.number() == 2) {
        at <- pretty(c(0, 1))
        panel.axis("bottom", at = at, outside = FALSE,
                   labels = TRUE, half = FALSE)
        panel.lines(x = c(0, 0), y = c(0.75, 2.25), lty = 3, lwd = 1)
        panel.lines(x = c(1, 1), y = c(2.75, 7.25), lty = 3, lwd = 1)
        #panel.abline(v = 0, lty = 3, lwd = 1)
        panel.dotplot(..., lwd = 0.5)
      }
    }
    
    nms <- names(df_all)[c(-1)]
  
    rng <- c(df_all$Bias - 1, df_all$Bias + 1)
    rsq_plt <- dotplot("Bias" ~ df_all$Bias, #asp = 0.5, 
                       xlab = "Value", ylab = "",
                       col = "grey20",
                       scales = list(x = list(draw = FALSE)),
                       par.settings = envinmr.theme(),
                       cex = 1.2, as.table = TRUE)
  
    
    err_plt <- dotplot(variable ~ value, data=df_melt[-1,],
                       xlab = "Value", ylab = "",
                       xlim=c(-0.05,1.05),
                       col = "grey20",
                       pch = 20,
                       par.settings = envinmr.theme(),
                       cex = 1.2, as.table = TRUE)
    
    out_plt <- resizePanels(Orcs::latticeCombineGrid(list(rsq_plt, err_plt),
                                                     layout = c(1, 2)), 
                            h = c(1/4, 3/4))
    
    out_plt <- update(out_plt, panel = panel.fun)
  
  }
  if (!plot) return(df_all) else 
    return(list(stats = df_all,
                plot = out_plt))
}
  
