#' download and install packages for envin-umr
#' 
#' @description 
#' download and install a bunch of packages that we find useful for our 
#' day-to-day workflow. If \code{cran.pkgs = NULL} a standard list of 
#' packages will be downloaded and installed. If you supply a character
#' vector of packages, these wil be installed instead. 
#' 
#' @param cran.pkgs character vector of packages to be installed from CRAN
#' @param git.pkgs character vector of packages to be installed from GitHub (not yet implemented)
#' @param check logical, whether to check if any packages specified in \code{cran.pkgs}
#' are already installed and skip their installation
#' @param update logical, whether to run \code{update.packages()} after installation
#' @param MODIS logical, if TRUE MODIS package will be installed from r-forge
#' @param dep logical, whether dependencies should be onstalled for specified packages
#' 
#' @author
#' Tim Appelhans
#' 
#' @export installEIPkgs
#' 

installEIPkgs <- function(cran.pkgs = NULL, 
                          git.pkgs = NULL, 
                          check = TRUE,
                          update = FALSE,
                          MODIS = FALSE,
                          dep = TRUE,
                          ...) {
  
  ## environmental informatics package list if no pkgs supplied
  if (is.null(cran.pkgs)) {
    cran.pkgs <- c("BradleyTerry2",
                   "caret", "kernlab", "ROCR", "raster", "latticeExtra", 
                   "fields", "reshape2", "gridExtra", "maps", "maptools", 
                   "mapdata", "sp", "rgdal", "RColorBrewer", "lattice", 
                   "doParallel", "hydroGOF", "corrplot", "sp", "plotKML", 
                   "rgeos","dismo", "automap", "colorspace", "devtools", 
                   "dplyr", "vegan", "gdalUtils", "geoR", "ggplot2", "gstat",
                   "spatstat", "Hmisc", "Kendall", "knitr", "landsat", 
                   "leaflet", "mapview", "RNiftyReg", "png", "jpeg", 
                   "jsonlite", "foreach", "gimms", "satellite", 
                   "lubridate", "modiscloud", "MODISTools", "grid", "ncdf", 
                   "ncdf4", "OpenStreetMap", "party", "randomForest",
                   "rasterVis", "RColorBrewer", "Rcpp", "remote", "rgl", 
                   "spacetime", "spatial.tools", "stargazer", "stringr", 
                   "TSA", "vcd", "xts", "zoo", "bfast", "httpuv", "shiny") 
  }
  
  if (check) {
    chk <- as.character(installed.packages()[, 1])
    '%!in%' <- function(x, y) !('%in%'(x, y))
    ind <- which(cran.pkgs %!in% chk)
  } else {
    ind <- 1:length(cran.pkgs)
  }
  
  
  lapply(cran.pkgs[ind], function(i) install.packages(i, dependencies = dep))
  
  if (update) update.packages()
  
  if (MODIS) {
    install.packages("MODIS", repos="http://R-Forge.R-project.org")
  }
}
  