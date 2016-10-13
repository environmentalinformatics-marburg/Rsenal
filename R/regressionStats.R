#' calculate prediction performance statistics
#' 
#' @description
#' this function calculates prediction performance statistics
#' between vectors of predicted and observed values, namely
#' coefficient of determination (Rsq), root mean squared error (RMSE), 
#' mean error (ME), mean absolute error (MAE). Users may also create 
#' a dotplot visualising the results.
#' 
#' @param prd numeric vector of predicted values
#' @param obs numeric vector of observed values
#' @param adj.rsq logical, whether to return adjusted r-squared. 
#' Defaults to TRUE
#' @param plot logical, whether to produce a visualisation of the results.
#' Defaults to FALSE
#' @param method character. Method to use for correlation. See \code{?cor.test}
#' for details.
#' 
#' @return
#' If \code{plot = FALSE} (the default), a data frame. 
#' If \code{plot = TRUE}, a list with components \code{stats} - data frame
#' and \code{plot} - a trellis plot object.
#' 
#' @author
#' Tim Appelhans, Hanna Meyer
#' 
#' @examples
#' ## create predictions with high accuracy (identical mean), 
#' ## but low precision (sd double that of observations). Hence,
#' ## ME should be close to zero and RMSE close to ten.
#' pred_vals <- sort(rnorm(1000, 200, 20)) # sorting ensures high Rsq
#' obs_vals <- sort(rnorm(1000, 200, 10))
#' 
#' ## with plot = TRUE
#' result <- regressionStats(pred_vals, obs_vals, plot = TRUE)
#' result$stats
#' result$plot
#' 
#' ## with plot = FALSE
#' result <- regressionStats(pred_vals, obs_vals, 
#'                           plot = FALSE, adj.rsq = FALSE)
#' result
#' 
#' @export regressionStats
#' @aliases regressionStats

regressionStats <- function(prd, obs, adj.rsq = TRUE, plot = FALSE,
                            method="pearson") {
  
  mod <- lm(prd ~ obs)
  if(adj.rsq) rsq <- summary(mod)$adj.r.squared else
    rsq <- summary(mod)$r.squared
  cor <- cor.test(obs,prd,method=method,exact=FALSE)$estimate
  
  df_all <- data.frame(ME = mean(prd - obs, na.rm = TRUE),
                       ME.se = se(prd - obs),
                       MAE = mean(abs(prd - obs), na.rm = TRUE),
                       MAE.se = se(abs(prd - obs)),
                       RMSE = sqrt(mean((prd - obs)^2, na.rm = TRUE)),
                       RMSE.se = se((prd - obs)^2),
                       Rsq = rsq,
                       cor=cor[[1]])
  names(df_all)[names(df_all)=="cor"] <- names(cor) #adapt rho/tau depending on method
  
  if (plot) {
    ## panel.fun modified from 
    ## http://thebiobucket.blogspot.de/2011/04/r-graphs-lattice-use-of-panel-functions.html
    panel.fun <- function(...) {
      if (panel.number() == 1) { 
        at<-pretty(c(0, 1))
        panel.axis("top", at = at, outside = FALSE,
                   labels = TRUE, half = FALSE)
        panel.abline(v = 1, lty = 3, lwd = 1)
        panel.dotplot(..., lwd = 0.5)
      }
      if (panel.number() == 2) {
        at <- pretty(rng)
        panel.axis("bottom", at = at, outside = FALSE,
                   labels = TRUE, half = FALSE)
        panel.abline(v = 0, lty = 3, lwd = 1)
        panel.lines(x = x_se[c(1, 4)], col = "grey60",
                    y = y_se[c(1, 4)], lwd = 4)
        panel.lines(x = x_se[c(2, 5)], col = "grey60",
                    y = y_se[c(2, 5)], lwd = 4)
        panel.lines(x = x_se[c(3, 6)], col = "grey60",
                    y = y_se[c(3, 6)], lwd = 4)
        panel.dotplot(..., lwd = 0.5)
        panel.dotplot(x = df_plt$fit, y = df_plt$nms,
                      cex = 1.3, col = "grey20", lwd = 0.5)
      }
    }
    
    nms <- names(df_all)[c(1, 3, 5)]
    
    rsq_plt <- dotplot("Rsq" ~ rsq, #asp = 0.5, 
                       xlab = "Value", ylab = "",
                       col = "grey20", xlim = c(-0.05, 1.05),
                       scales = list(x = list(draw = FALSE)),
                       par.settings = envinmr.theme(),
                       cex = 1.2, as.table = TRUE)
    
    fit <- c(df_all$ME,
             df_all$MAE,
             df_all$RMSE)
    
    fit_se <- c(df_all$ME.se,
                df_all$MAE.se,
                df_all$RMSE.se)
    
    df_plt <- data.frame(nms = nms,
                         fit = fit,
                         fit_se = fit_se,
                         upr = fit + fit_se,
                         lwr = fit - fit_se)
    
    x_se <- c(df_plt$lwr, df_plt$upr)
    y_se <- rep(c(2, 1, 3), 2)
    
    rng <- range(x_se)
    rng <- round(c(rng[1] - 0.2 * rng[1],
                   rng[2] + 0.2 * rng[2]))
    
    err_plt <- dotplot(nms ~ upr + lwr,
                       data = df_plt, , 
                       xlab = "Value", ylab = "",
                       col = "grey20",
                       pch = "|",
                       par.settings = envinmr.theme(),
                       cex = 1.2, as.table = TRUE)
    
    out_plt <- resizePanels(latticeCombineGrid(list(rsq_plt, err_plt),
                                               layout = c(1, 2)), 
                            h = c(1/4, 3/4))
    out_plt$y.limits[[2]] <- nms[c(2, 1, 3)]
    
    out_plt <- update(out_plt, panel = panel.fun)
  }
  
  if (!plot) return(df_all) else 
    return(list(stats = df_all,
                plot = out_plt))
}
