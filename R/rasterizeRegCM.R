#' Rasterize RegCM outout
#' 
#' @description
#' The output of RegCM data are (amongst others) a rasters containing the 
#' latitude and a longitude coordinates as well as raster of the data values. 
#' All raster, however, are not projected but only in column/rows. A simple 
#' assignment of the lat/lon coordinates to the data is not possible due to
#' the irregular spacing between the lat/lon coordinates. Therefore, the data
#' must first be converted to spatial points and projected to a metric reference
#' system. The values can then be rasterized by creating a template raster of 
#' the extent and proj4string of the projected points as well as either 
#' (if known) the resolution of the target raster in meter or (if resolution 
#' unknown) the number of columns and rows from the original rasters.
#' Note that often Model data need to be rotated and transposed.
#' 
#' The function also works for other data which are not projected but which have
#' a latitude and a longitude raster as additional information.
#' 
#' @param x A Raster layer or raster stack containing the values
#' @param lat A raster layer containing the latitude coordinates as pixel values
#' @param lon A raster layer containing the longitude coordinates as pixel values
#' @param res A Vector containing the target x and y resolution
#' @param transpose (requires documentation)
#' @param rotate (requires documentation)
#' @param targetcrs EPSG code or proj4String of the metric target reference system
#' 
#' @return
#' A \code{RasterLayer} or \code{RasterStack} object.
#' 
#' @author
#' Hanna Meyer and Tim Appelhans

#' @export rasterizeRegCM
#' @aliases rasterizeRegCM
rasterizeRegCM <- function(x, lat, lon, res=NULL, transpose=TRUE, rotate =c("x","y"), targetcrs="+init=epsg:3031"){
  require(raster)
  datstack <- stack(x,lat,lon)
  if (transpose){
    datstack <- t(datstack)
  }
  if(any(rotate=="y")){
    datstack <- flip(datstack,"y")
  }
  if(any(rotate=="x")){
    datstack <- flip(datstack,"x")
  }
  coord <- SpatialPointsDataFrame(data.frame(values(datstack[[nlayers(datstack)]]),
                                             values(datstack[[nlayers(datstack)-1]])),
                                  data<-data.frame(values(datstack[[1:(nlayers(datstack)-2)]])),
                                  proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
  coord_proj <- spTransform(coord,CRS(targetcrs))
  if(!is.null(res)){
    template <- raster(crs=targetcrs,
                       ext=extent(coord_proj),
                       res=res)
  }else{
    template <- raster(crs=targetcrs,
                       ext=extent(coord_proj),
                       ncol = ncol(datstack),
                       nrow = nrow(datstack))
  }
  dat_projected<-stack()
  for (i in 1:(nlayers(datstack)-2)){
    dat_projected <- stack(dat_projected,
                           rasterize(coord_proj,template,field=coord_proj@data[,i]))
    if (i>1){
      print (paste0("layer ", i, " in progress"))
    }
  }
  dat_projected <- resample(dat_projected, dat_projected)
}
