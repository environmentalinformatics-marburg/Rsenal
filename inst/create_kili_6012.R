library(Rsenal)
library(dplyr)

path <- "/media/ede/tims_ex/kilimanjaro_precip_sp1_vs_hemp"
setwd(path)

shp <- "/media/ede/tims_ex/kilimanjaro_plots_dem/Plots_DEM_final/PlotPoles_ARC1960_mod_20140807_final.shp"
lyr <- ogrListLayers(shp)
plts <- readOGR(shp, lyr)
proj4string(plts) <- "+init=epsg:21037"
plts_amp <- subset(plts, PoleType == "AMP")

plts_amp_df <- plts_amp@data

#### plots ----------------------------------------------------------------
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

the60_amp <- merge(the60, plts_amp_df,
                        by.x = "PlotID", by.y = "PlotID",
                        all.x = TRUE)

kili60 <- data.frame(PlotID = the60_amp$PlotID,
                     Elevation = the60_amp$Z_DEM_HMP)

kili60 <- kili60[order(kili60$Elevation), ]
kili60$PlotID <- factor(kili60$PlotID,
                        levels = as.character(kili60$PlotID))
kili60 <- kili60[order(as.character(kili60$PlotID)), ]

save(kili60, file = "kili60.rda")


##### habitats ------------------------------------------------------------
the12 <- data.frame(Habitat =
                      c("cof", "fer", "flm", "foc", "fod", "fpd",
                        "fpo", "gra", "hel", "hom", "mai", "sav"))

ele_habs <- plts_amp_df %>%
  group_by(substr(plts_amp_df$PlotID, 1, 3)) %>%
  summarise(Elevation = mean(Z_DEM_HMP, na.rm = TRUE))
names(ele_habs) <- c("Habitat", "Elevation")
ele_habs <- ele_habs[order(ele_habs$Elevation), ]
kili12 <- ele_habs[ele_habs$Habitat %in% the12$Habitat, ]

save(kili12, file = "kili12.rda")
