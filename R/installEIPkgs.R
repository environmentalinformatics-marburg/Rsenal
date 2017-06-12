#' Download and install packages for envin-umr
#' 
#' @description 
#' Download and install a bunch of packages that we find useful for our 
#' day-to-day workflow. If \code{cran.pkgs = NULL} (default), a standard list of 
#' packages will be downloaded and installed. If you supply a \code{character}
#' vector of packages, these wil be installed instead. 
#' 
#' @param cran.pkgs \code{character} vector of packages to be installed from 
#' CRAN.
#' @param git.pkgs \code{character} vector of packages to be installed from 
#' GitHub (not yet implemented).
#' @param check \code{logical}, determines whether to check if any packages 
#' specified in 'cran.pkgs' are already installed and skip their installation.
#' @param update \code{logical}, determines whether to run 
#' \code{\link{update.packages}} after installation.
#' @param ... Additional arguments passed to \code{\link{install.packages}}.
#' 
#' @author
#' Tim Appelhans, Florian Detsch
#' 
#' @seealso 
#' \code{\link{install.packages}}, \code{\link{install_github}}.
#' 
#' @examples 
#' \dontrun{
#' installEIPkgs()
#' }
#' 
#' @export installEIPkgs
#' @name installEIPkgs
installEIPkgs <- function(cran.pkgs = NULL, 
                          git.pkgs = NULL, 
                          check = TRUE,
                          update = FALSE,
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
                   "jsonlite", "foreach", "gimms", "satellite", "htmltools", 
                   "lubridate", "modiscloud", "MODISTools", "grid", "ncdf", 
                   "ncdf4", "OpenStreetMap", "party", "randomForest", "MODIS",
                   "rasterVis", "RColorBrewer", "Rcpp", "remote", "rgl", 
                   "spacetime", "spatial.tools", "stargazer", "stringr", 
                   "TSA", "vcd", "xts", "zoo", "bfast", "httpuv", "shiny", 
                   "plotrix", "RStoolbox") 
  }
  
  ## install cran packages (if required)
  jnk <- lapply(cran.pkgs, function(i) {
    install <- if (check) {
      !(i %in% installed.packages()[, 1])
    } else TRUE
    
    if (install) install.packages(i, ...)
  })
  
  ## update cran packages
  if (update) update.packages()
  return(invisible())
}