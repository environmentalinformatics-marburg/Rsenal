library(rgdal)
library(caret)
library(Rsenal)
library(latticeExtra)
library(dplyr)
library(gridExtra)

path <- "/media/ede/tims_ex/kilimanjaro_precip_sp1_vs_hemp"
setwd(path)

shp <- "/media/ede/tims_ex/kilimanjaro_plots_dem/Plots_DEM_final/PlotPoles_ARC1960_mod_20140807_final.shp"
lyr <- ogrListLayers(shp)
plts <- readOGR(shp, lyr)
proj4string(plts) <- "+init=epsg:21037"
plts_amp <- subset(plts, PoleType == "AMP")

the60 <- data.frame(PlotID =
                      c(paste(rep("cof", 5), 1:5, sep = ""),
                        paste(rep("fer", 5), 0:4, sep = ""),
                        paste(rep("flm", 5), c(1:4, 6), sep = ""),
                        paste(rep("foc", 5), 1:5, sep = ""),
                        paste(rep("fod", 5), 1:5, sep = ""),
                        paste(rep("fpd", 5), 1:5, sep = ""),
                        paste(rep("fpo", 5), 1:5, sep = ""),
                        paste(rep("gra", 5), 1:5, sep = ""),
                        paste(rep("hel", 5), 1:5, sep = ""),
                        paste(rep("hom", 5), 1:5, sep = ""),
                        paste(rep("mai", 5), 1:5, sep = ""),
                        paste(rep("sav", 5), 1:5, sep = "")))

plts_amp_df <- plts_amp@data

the60_amp <- merge(the60, plts_amp_df,
                        by.x = "PlotID", by.y = "PlotID",
                        all.x = TRUE)

kili60 <- data.frame(PlotID = the60_amp$PlotID,
                     Elevation = the60_amp$Z_DEM_HMP)


the12 <- data.frame(Habitat =
                      c("cof", "fer", "flm", "foc", "fod", "fpd",
                        "fpo", "gra", "hel", "hom", "mai", "sav"))



ele_habs <- plts_amp@data %>%
  group_by(substr(plts_amp$PlotID, 1, 3)) %>%
  summarise(Elevation = mean(Z_DEM_HMP, na.rm = TRUE))
names(ele_habs) <- c("Habitat", "Elevation")
ele_habs <- ele_habs[order(ele_habs$Elevation), ]
ele_habs <- ele_habs[ele_habs$Habitat %in% the12, ]

official_plots <- prcp_mnthly_fit[prcp_mnthly_fit$PlotID %in% the60, ]
official_plots$Habitat <- substr(official_plots$PlotID, 1, 3)
official_plots$Habitat <- factor(official_plots$Habitat,
                                 levels = rev(ele_habs$Habitat))
official_plots <- official_plots[order(official_plots$Habitat), ]

ta200 <- read.csv("../kilimanjaro_release_data/points/envin-umr_ki_ta_all-plots_aggregated-monthly_v010.csv")
ta200 <- ta200[ta200$PlotID %in% the60, ]
ta200 <- ta200[, -c(2, 4)]
ta200$Habitat <- substr(ta200$PlotID, 1, 3)
ta200$Habitat <- factor(ta200$Habitat,
                        levels = rev(ele_habs$Habitat))
ta200 <- ta200[order(ta200$Habitat), ]

official_plots$MMT <- ta200$MMT

p_all <- lapply(unique(official_plots$PlotID), function(i) {
  pre <- official_plots$MMP[official_plots$PlotID == i]
  sum_pre <- round(sum(official_plots$MMP[official_plots$PlotID == i]), 0)
  elev <- round(plts_amp$Z_DEM_HMP[plts_amp$PlotID == i], 0)
  pred_p <- xyplot(pre ~ seq_along(pre), asp = 0.65,
                   type = "l", col = "#3182bd", lwd = 3,
                   ylim = c(-10, 690), as.table = TRUE,
                   xlab = "Month", ylab = "Precipitation [mm]",
                   yscale.components = yscale.components.subticks,
                   panel = function(x, y, ...) {
                     panel.xyplot(x, y, ...)
                     panel.text(x = 7, y = 640,
                                labels = paste(i, ": ", sum_pre,
                                               " mm", sep = ""))
                     panel.text(x = 7, y = 550,
                                labels = paste("elevation", ": ", elev,
                                               " m", sep = ""))
                   })
})
#   obs <- prcp_mnthly_lst_clean[[i]]$P_RT_NRT
#   obs_p <- xyplot(obs ~ seq(12), asp = 1,
#                   type = "l", col = "black", lwd = 2, lty = 3)
#
#   out_p <- pred_p + as.layer(obs_p)
# })

t_all <- lapply(unique(official_plots$PlotID), function(i) {
  ta <- official_plots$MMT[official_plots$PlotID == i]
  pred_t <- xyplot(ta ~ seq_along(ta), asp = 0.65,
                   type = "l", col = "#3182bd", lwd = 3,
                   ylim = c(-10/2, 100/2), as.table = TRUE,
                   xlab = "Month", ylab = "Temperature [°C]",
                   yscale.components = yscale.components.subticks)
})


p_final_all <- latticeCombineGrid(p_all, layout = c(5, 12))

png("monthly_precip_60plots_prcp_interp_krig.png",
    width = 40, height = 60, units = "cm", res = 300)
grid.newpage()
print(p_final_all)
dev.off()

# ### test other plots
# plt <- "fod4"
# pre <- prcp_mnthly_fit$MMP[prcp_mnthly_fit$PlotID == plt]
# sum_pre <- round(sum(prcp_mnthly_fit$MMP[prcp_mnthly_fit$PlotID == plt]), 0)
# pred_p <- xyplot(pre ~ seq_along(pre),
#                  type = "l", col = "red2", lwd = 2,
#                  ylim = c(-10, 690),
#                  panel = function(x, y, ...) {
#                    panel.xyplot(x, y, ...)
#                    panel.text(x = 1, y = 550,
#                               labels = paste(plt, ":", sum_pre, sep = " "))
#                  })
#
# pred_p

# plot(prcp_mnthly_fit$MMP[prcp_mnthly_fit$PlotID == "nkw1"], type = "l")
# lines(prcp_mnthly_lst$nkw1$P_RT_NRT, col = "red")


# prcp_mnthly <- as.data.frame(do.call("rbind",
#                                      lapply(seq(mnthly_wgts), function(i) {
#   mnthly_wgts[[i]] * prcp_ann$MAP[i]
# })))
#
# prcp_mnthly$PlotID <- prcp_ann$PlotID
# names(prcp_mnthly) <- c(sprintf("%02.f", 1:12), "PlotID")
#
# prcp_mnthly_obs <- read.csv("data/P_RT_NRT_0200_monthly_lt_average_sums.dat",
#                             stringsAsFactors = FALSE)
#
#
