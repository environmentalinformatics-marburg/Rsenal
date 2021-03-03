#' an Rsenal of functions by Environmentalinformatics Marburg
#' 
#' A collection of functions which we find helpful/useful for our 
#' day-to-day ventures into data analysis at Environmentalinformatics
#' group, Department of Geography, Philipps University Marburg.
#' There is no specific focus of interest in this collection, some 
#' functions are very task specific (e.g. \code{\link{roc}} which let's
#' you create roc curves for land-use classifications), yet others are
#' very general and widely applicable.
#' 
#' @name Rsenal-package
#' @aliases Rsenal
#' @docType package
#' @title Rsenal: magic functions for things various
#' @author Tim Appelhans, Florian Detsch, Insa Otte, Meike Kuehnlein, 
#' Hanna Meyer, Spaska Forteva, Thomas Nauss, Stefan Woellauer\cr
#' \cr
#' \emph{Maintainer:} Tim Appelhans \email{tim.appelhans@@gmail.com}
#' 
#' @keywords package
#' 
#' @import methods raster sp rgdal stars
#' @import RColorBrewer lattice latticeExtra grid gridBase gridExtra
#' @import caret hflights party satellite
#' @import parallel doParallel foreach
#' @importFrom devtools build
#' @importFrom dismo gmap
#' @importFrom dplyr count_
#' @importFrom gdalUtils gdal_translate get_subdatasets
#' @importFrom ggplot2 ggplot_gtable ggplot_build
#' @importFrom graphics .filled.contour legend par plot.window
#' @importFrom grDevices cm.colors colorRampPalette hcl
#' @importFrom landsat histmatch
#' @importFrom maps map
#' @importFrom Orcs bumpVersion compareDistributions latticeCombineGrid latticeCombineLayer offsetGridText rgb2spLayout stextGrob
#' @importFrom plotrix thigmophobe
#' @importFrom plyr round_any
#' @importFrom pROC roc
#' @importFrom Rcpp sourceCpp
#' @importFrom reshape2 melt
#' @importFrom rgeos gCentroid
#' @importFrom roxygen2 roxygenize
#' @importFrom SDMTools PatchStat
#' @importFrom stats as.formula cor.test fitted integrate lm median na.exclude sd splinefun ts
#' @importFrom tagcloud tagcloud
#' @importFrom TSA harmonic
#' @importFrom utils combn download.file glob2rx install.packages installed.packages modifyList read.csv update.packages
#'
#' @rawNamespace if (.Platform$OS.type=="windows") importFrom(grDevices,windowsFonts)
#' @rawNamespace useDynLib(Rsenal, .registration = TRUE)
NULL
#' 
#' @docType data 
#' @name kili12
#' @title KiLi FOR1246 Habitats
#' @description data frame of KiLi FOR1246 Habitats and respective elevations
#' @details Note, the Habitats are ordered according to their mean elevation
#' @format a data frame with the following columns\cr
#' \cr
#' Habitat    : factor with levels ordered according to their mean elevation\cr
#' Elevation  : numeric\cr
#' 
NULL
#' 
#' @docType data 
#' @name kili60
#' @title KiLi FOR1246 PlotIDs
#' @description data frame of KiLi FOR1246 PlotIDs and respective elevations
#' @details Note, the PlotIDs are ordered according to their mean elevation
#' @format a data frame with the following columns\cr
#' \cr
#' PlotID    : factor with levels ordered according to their mean elevation\cr
#' Elevation  : numeric\cr
#' 
NULL
#' @import leaflet scales
NULL
#'
#' @docType data
#' @name gmap_hel
#' @title Google Maps aerial image of HEL1
#' @description This aerial image was downloaded from Google Maps and covers the 
#' research plot HEL1 declared within the framework of the research unit 
#' "Kilimanjaro Ecosystems Under Global Change: Linking biodiversity, biotic 
#' interactions and biogeochemical ecosystem processes". It was used to conduct 
#' unsupervised image classification based on \code{\link{kmeans}} clustering. 
#' @details For a more detailed description of the applied analysis, please refer to 
#' \url{https://www.researchgate.net/publication/281716190_Unsupervised_classification_of_Google_maps_imagery_in_R} 
#' (accessed on January 27, 2016).
#' @format \code{raster::RasterBrick}
#' 
NULL
#'
#' @docType data
#' @name atlStorms2005
#' @title Atlantic Ocean storms 2005
#' @description Atlantic Ocean storms 2005
#' @details This dataset contains storm tracks for selected storms
#' in the Atlantic Ocean basin for the year 2005
#' @format \code{sp::SpatialLinesDataFrame}
#' 
NULL
