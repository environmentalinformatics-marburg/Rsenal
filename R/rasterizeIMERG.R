#' Rasterize GPM IMERG data
#'
#' @description
#' Data can be downloaded in HDF5 from 
#' http://pmm.nasa.gov/data-access/downloads/gpm
#' This function mainly follows the description for ArcGIS provided by the NASA: 
#' http://disc.sci.gsfc.nasa.gov/recipes/?q=recipes/How-to-Import-HDF5-formatted-IMERG-GPM-Precipitation-Data-into-ArcGIS
#'
#' @param x Filename of the IMERG HDF5 file
#' @param layer IMERG layer name
#'
#' @return
#' A \code{RasterLayer}
#'
#' @author
#' Hanna Meyer

#' @export rasterizeIMERG
#' @aliases rasterizeIMERG

rasterizeIMERG <- function (x, layer="precipitationCal"){
  require(rgdal)
  require(raster)
  result <- raster(readGDAL(paste0("HDF5:",x,"://Grid/",layer)))
  proj4string(result)<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  result <- t(result)
  extent(result) <- c(-180,180,-90,90)
  result[result<0] <- NA
  result <- flip(result,2)
  return(result)
}
