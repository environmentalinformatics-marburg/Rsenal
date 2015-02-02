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
#' 
#' @return
#' If \code{plot = FALSE} (the default), a data frame. 
#' If \code{plot = TRUE}, a list with components \code{stats} - data frame
#' and \code{plot} - a trellis plot object.
#' 
#' @author
#' Tim Appelhans
#' 
#' @examples
#' ## create predictions with high accuracy (identical mean), 
#' ## but low precision (sd double that of observations). Hence,
#' ## ME should be close to zero and RMSE close to ten.
#' pred_vals <- sort(rnorm(1000, 200, 20)) # sorting ensures high Rsq
#' obs_vals <- sort(rnorm(1000, 200, 10))
#' 
#' ## with plot = TRUE
#' result <- predictionStats(pred_vals, obs_vals, plot = TRUE)
#' result$stats
#' result$plot
#' 
#' ## with plot = FALSE
#' result <- predictionStats(pred_vals, obs_vals, 
#'                           plot = FALSE, adj.rsq = FALSE)
#' result
#' 
#' @export predictionStats
#' @aliases predictionStats

predictionStats <- function(prd, obs, adj.rsq = TRUE, plot = FALSE) {
  
  mod <- lm(prd ~ obs)
  if(adj.rsq) rsq <- summary(mod)$adj.r.squared else
    rsq <- summary(mod)$r.squared
  
  df_all <- data.frame(ME = mean(prd - obs, na.rm = TRUE),
                       ME.se = se(prd - obs),
                       MAE = mean(abs(prd - obs), na.rm = TRUE),
                       MAE.se = se(abs(prd - obs)),
                       RMSE = sqrt(mean((prd - obs)^2, na.rm = TRUE)),
                       RMSE.se = se((prd - obs)^2),
                       Rsq = rsq)

  if (plot) {
    library(latticeExtra)
    library(Hmisc)
    
    nms <- names(df_all)[c(1, 3, 5)]
    
    rsq_plt <- dotplot("Rsq" ~ rsq, asp = 1, 
                       xlab = "Value", ylab = "",
                       col = "grey20", xlim = c(-0.05, 1.05),
                       scales = list(x = list(alternating = 2,
                                              tck = c(1, 1))),
                       par.settings = envinmr.theme(),
                       cex = 1.2, as.table = TRUE) +
      latticeExtra::layer_(panel.abline(v = 1, lty = 3, lwd = 1))
    
    fit <- c(df_all$ME,
             df_all$MAE,
             df_all$RMSE)
    
    fit_se <- c(df_all$ME.se,
                df_all$MAE.se,
                df_all$RMSE.se)
    
    df_plt <- data.frame(nms = nms,
                         fit = fit,
                         fit_se = fit_se)
    
    err_plt <- Hmisc::Dotplot(nms ~ Cbind(fit, 
                                          fit + fit_se, 
                                          fit - fit_se),
                              data = df_plt, , asp = 1, 
                              xlab = "Value", ylab = "",
                              col = "grey20",  
                              par.settings = envinmr.theme(),
                              cex = 1.2, as.table = TRUE) + 
      latticeExtra::layer_(panel.lines(x = c(0, 0), 
                                       y = c(0.75, 3.25), 
                                       lty = 3, lwd = 1))
    
    err_plt <- update(err_plt, scales = list(tck = c(1, 1)))
    
    out_plt <- resizePanels(latticeCombineGrid(list(rsq_plt, err_plt),
                                               layout = c(1, 2)), 
                            h = c(1/4, 3/4))
    out_plt$y.limits[[2]] <- nms[c(2, 1, 3)]
    
  }
  
  if (!plot) return(df_all) else 
    return(list(stats = df_all,
                plot = out_plt))
}
