if ( !isGeneric('mapView') ) {
  setGeneric('mapView', function(x, ...)
    standardGeneric('mapView'))
}

#' view raster layers interactively
#' 
#' @description 
#' this function produces an interactive GIS-like view of the specified 
#' raster layers on top of the specified base maps.
#' 
#' @param x a \code{\link{raster}}* object
#' @param cols color palette for the layers
#' @param na.color color for missing values
#' @param values a vector of values for the visualisation of the layers.
#' Per default these are calculated based on the supplied raster* object.
#' @param map.types character spcifications for the base maps.
#' see \url{http://leaflet-extras.github.io/leaflet-providers/preview/}
#' for available options.
#' @param layer.opacity opacity of the layers
#' @param legend should a legend be plotted
#' @param legend.opacity opacity of the legend
#' @param trim should the raster be trimmed in case there are NAs on the egdes
#' @param ... additional arguments passed on to \code{\link{addLegend}} 
#' 
#' @author
#' Tim Appelhans
#' 
#' @examples
#' ### raster data ###
#' data(meuse.grid)
#' coordinates(meuse.grid) = ~x+y
#' proj4string(meuse.grid) <- CRS("+init=epsg:28992")
#' gridded(meuse.grid) = TRUE
#' meuse_rst <- stack(meuse.grid)
#' 
#' m1 <- mapView(meuse_rst)
#' m1
#' 
#' m2 <- mapView(meuse_rst[[1]])
#' m2
#' 
#' ### point vector data ###
#' data(meuse)
#' coordinates(meuse) <- ~x+y
#' proj4string(meuse) <- CRS("+init=epsg:28992")
#' 
#' # all layers of meuse
#' mapView(meuse)
#' 
#' # only one layer, all info in popups
#' mapView(meuse, burst = FALSE)
#' 
#' ### overlay vector on top of raster ###
#' m3 <- mapView(meuse, map = m2, burst = FALSE)
#' m3
#' 
#' m4 <- mapView(meuse, map = m2)
#' m4
#' 
#' ### polygon vector data ###
#' data("DEU_adm2")
#' mapView(gadm, burst = FALSE)
#' 
#' @export mapView
#' @name mapView
#' @rdname mapView
#' @aliases mapView


## RasterLayer ============================================================
setMethod('mapView', signature(x = 'RasterLayer'), 
          function(x,
                   map = NULL,
                   cols = envinmrPalette(7), 
                   na.color = "transparent",
                   values = NULL,
                   map.types = c("OpenStreetMap",
                                 "Esri.WorldImagery"),
                   layer.opacity = 0.8,
                   legend = TRUE,
                   legend.opacity = 1,
                   trim = TRUE,
                   ...) {
  
            pkgs <- c("leaflet", "raster", "magrittr")
            tst <- sapply(pkgs, "requireNamespace", 
                          quietly = TRUE, USE.NAMES = FALSE)
            
            is.fact <- raster::is.factor(x)
            
            #llcrs <- CRS("+init=epsg:3857")@projargs
            llcrs <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs"
            
            if (!identical(projection(x), llcrs)) {
              cat("\n", "reprojecting to web mercator", "\n\n")
#               if (is.fact) {
#                 x <- raster::projectRaster(x, crs = llcrs, method = "ngb")
#               } else {
#                 x <- raster::projectRaster(x, crs = llcrs)
#               }
            }
            
            if (trim) x <- trim(x)
            
            if (is.fact) x <- raster::as.factor(x)
            
            if (is.null(values)) {
              if (is.fact) {
                values <- x@data@attributes[[1]]$ID
              } else {
                values <- seq(floor(min(x[], na.rm = TRUE)),
                              ceiling(max(x[], na.rm = TRUE)),
                              length.out = 10)
                values <- round(values, 5)
              }
            } else {
              values <- round(values, 5)
            }
            
            if (is.fact) {
              pal <- leaflet::colorFactor(cols, as.factor(values), 
                                          levels = values, na.color = na.color)
            } else {
              pal <- leaflet::colorNumeric(cols, values, na.color = na.color)
            }
            
            ## create base map using specified map types
            if (is.null(map)) {
              m <- leaflet::leaflet()
              m <- leaflet::addTiles(m, group = map.types[1])
              m <- leaflet::addProviderTiles(m, provider = map.types[2],
                                             group = map.types[2])
            } else {
              m <- map
            }
            
            ## add layers to base map
            m <- leaflet::addRasterImage(map = m,
                                         x = x,
                                         colors = pal,
                                         project = TRUE,
                                         opacity = layer.opacity,
                                         group = names(x))

            if (legend) {
              ## add legend
              m <- leaflet::addLegend(map = m,
                                      pal = pal,
                                      opacity = legend.opacity, 
                                      values = values, ...)
            }
            
            ## add layer control buttons
            if (is.null(map)) {
              m <- leaflet::addLayersControl(map = m,
                                             position = "topleft",
                                             baseGroups = map.types,
                                             overlayGroups = names(x))
            } else {
              list.len <- length(map$x$calls)
              m <- leaflet::addLayersControl(map = m,
                                             position = "topleft",
                                             baseGroups = map.types,
                                             overlayGroups = 
                                               c(m$x$calls[[list.len]]$args[[2]], 
                                                 names(x)))
            }
            
            return(m)
            
          }
          
)

## Raster Stack ===========================================================
#' @describeIn mapView

setMethod('mapView', signature(x = 'RasterStack'), 
          function(x, 
                   ...) {
            
            pkgs <- c("leaflet", "raster", "magrittr")
            tst <- sapply(pkgs, "requireNamespace", 
                          quietly = TRUE, USE.NAMES = FALSE)
            
            m <- mapView(x[[1]], ...)
            
            for (i in 2:nlayers(x)) {
              m <- mapView(x[[i]], m, ...)
            }
            
            return(m)
            
          }
          
)


## Raster Brick ===========================================================
#' @describeIn mapView

setMethod('mapView', signature(x = 'RasterBrick'), 
          function(x, 
                   ...) {
            
            pkgs <- c("leaflet", "raster", "magrittr")
            tst <- sapply(pkgs, "requireNamespace", 
                          quietly = TRUE, USE.NAMES = FALSE)
            
            m <- mapView(x[[1]], ...)
            
            for (i in 2:nlayers(x)) {
              m <- mapView(x[[i]], m, ...)
            }
            
            return(m)
            
          }
          
)


## SpatialPointsDataFrame =================================================
#' @describeIn mapView
#' @param burst whether to show all (TRUE) or only one (FALSE) layers 

setMethod('mapView', signature(x = 'SpatialPointsDataFrame'), 
          function(x,
                   map = NULL,
                   burst = TRUE,
                   cols = envinmrPalette(7), 
                   na.color = "transparent",
                   values = NULL,
                   map.types = c("OpenStreetMap",
                                 "Esri.WorldImagery"),
                   layer.opacity = 0.8,
                   legend = TRUE,
                   legend.opacity = 1,
                   ...) {
            
            pkgs <- c("leaflet", "sp", "magrittr")
            tst <- sapply(pkgs, "requireNamespace", 
                          quietly = TRUE, USE.NAMES = FALSE)
            
            llcrs <- CRS("+init=epsg:4326")@projargs
            
            if (!identical(projection(x), llcrs)) {
              cat("\n", "reprojecting to web mercator", "\n\n")
              x <- spTransform(x, CRSobj = llcrs)
            }
            
            if (is.null(map)) {
              m <- leaflet::leaflet()
              m <- leaflet::addTiles(m, group = map.types[1])
              m <- leaflet::addProviderTiles(m, provider = map.types[2],
                                             group = map.types[2])
            } else {
              m <- map
            }
            
            if (burst) {
              lst <- lapply(names(x), function(j) x[j])
              
              vals <- lapply(seq(lst), function(i) lst[[i]]@data[, 1])
              
              pal_n <- lapply(seq(lst), function(i) {
                if (is.factor(lst[[i]]@data[, 1])) {
                  leaflet::colorFactor(cols, lst[[i]]@data[, 1], 
                                       levels = levels(lst[[i]]@data[, 1]))
                } else {
                  leaflet::colorNumeric(cols, vals[[i]], 
                                        na.color = "transparent")
                }
              })
            
              for (i in seq(lst)) {
                len <- length(m$x$calls)
                m <- leaflet::addCircleMarkers(m, lng = coordinates(lst[[i]])[, 1],
                                               lat = coordinates(lst[[i]])[, 2],
                                               group = names(lst[[i]]),
                                               color = pal_n[[i]](vals[[i]]))
                
                
                m <- leaflet::addMarkers(m, lng = coordinates(lst[[i]])[, 1],
                                         lat = coordinates(lst[[i]])[, 2],
                                         group = names(lst[[i]]),
                                         options = leaflet::markerOptions(opacity = 0),
                                         popup = as.character(vals[[i]]))
                
                m <- leaflet::addLegend(map = m, position = "topright", pal = pal_n[[i]],
                                        opacity = 1, values = vals[[i]], title = "Legend")
                
                if (i == 1) {
                  m <- leaflet::addLayersControl(map = m,
                                                 position = "topleft",
                                                 baseGroups = c("OpenStreetMap",
                                                                "Esri.WorldImagery"),
                                                 overlayGroups = c(
                                                   m$x$calls[[len]]$args[[2]],
                                                   names(lst[[i]])))
                } else {
                  m <- leaflet::addLayersControl(map = m,
                                                 position = "topleft",
                                                 baseGroups = c("OpenStreetMap",
                                                                "Esri.WorldImagery"),
                                                 overlayGroups = c(
                                                   m$x$calls[[len]]$args[[2]],
                                                   names(lst[[i]])))
                }
              }
              
            } else {
              
              df <- as.data.frame(sapply(x@data, as.character),
                                  stringsAsFactors = FALSE)
              
              nms <- names(df)
              grp <- strsplit(strsplit(as.character(sys.calls()[1]), 
                                       "\\(")[[1]][2], ",")[[1]][1]
              
              len <- length(m$x$calls)
              
              m <- leaflet::addCircleMarkers(m, lng = coordinates(x)[, 1],
                                             lat = coordinates(x)[, 2],
                                             group = grp,
                                             color = cols[length(cols)])
              
              txt <- sapply(seq(nrow(x@data)), function(i) {
                paste(nms, df[i, ], sep = ": ")
              })
              
              txt <- sapply(seq(ncol(txt)), function(j) {
                paste(txt[, j], collapse = " <br> ")
              })
              
              m <- leaflet::addMarkers(m, lng = coordinates(x)[, 1],
                                       lat = coordinates(x)[, 2],
                                       group = grp,
                                       options = leaflet::markerOptions(opacity = 0),
                                       popup = txt)
              
              m <- leaflet::addLayersControl(map = m,
                                             position = "topleft",
                                             baseGroups = c("OpenStreetMap",
                                                            "Esri.WorldImagery"),
                                             overlayGroups = c(
                                               m$x$calls[[len]]$args[[2]],
                                               grp))
            }
            
            return(m)
            
          }
          
)


## SpatialPolygonsDataFrame =================================================
#' @describeIn mapView
#' @param burst whether to show all (TRUE) or only one (FALSE) layers 

setMethod('mapView', signature(x = 'SpatialPolygonsDataFrame'), 
          function(x,
                   map = NULL,
                   burst = TRUE,
                   cols = envinmrPalette(7), 
                   na.color = "transparent",
                   values = NULL,
                   map.types = c("OpenStreetMap",
                                 "Esri.WorldImagery"),
                   layer.opacity = 0.8,
                   legend = TRUE,
                   legend.opacity = 1,
                   weight = 2,
                   ...) {
            
            pkgs <- c("leaflet", "sp", "magrittr")
            tst <- sapply(pkgs, "requireNamespace", 
                          quietly = TRUE, USE.NAMES = FALSE)
            
            llcrs <- CRS("+init=epsg:4326")@projargs
            
            if (!identical(projection(x), llcrs)) {
              cat("\n", "reprojecting to web mercator", "\n\n")
              x <- spTransform(x, CRSobj = llcrs)
            }
            
            if (is.null(map)) {
              m <- leaflet::leaflet(x)
              m <- leaflet::addTiles(m, group = map.types[1])
              m <- leaflet::addProviderTiles(m, provider = map.types[2],
                                             group = map.types[2])
            } else {
              m <- map
            }
            
            if (burst) {
              lst <- lapply(names(x), function(j) x[j])
              
              df <- lapply(seq(lst), function(i) {
                dat <- data.frame(lst[[i]], stringsAsFactors = TRUE)
                if (is.character(dat[, 1])) {
                  dat[, 1] <- factor(dat[, 1], levels = unique(dat[, 1]))
                }
                return(dat)
              })
              
              vals <- lapply(seq(lst), function(i) df[[i]][, 1])
              
              pal_n <- lapply(seq(lst), function(i) {
                if (is.factor(df[[i]][, 1])) {
                  leaflet::colorFactor(cols, df[[i]][, 1], levels = levels(df[[i]][, 1]))
                } else {
                  leaflet::colorNumeric(cols, vals[[i]], na.color = "transparent")
                }
              })
              
              for (i in seq(lst)) {
                len <- length(m$x$calls)
                m <- leaflet::addPolygons(m, 
                                          weight = weight,
                                          group = names(lst[[i]]),
                                          color = pal_n[[i]](vals[[i]]),
                                          popup = as.character(vals[[i]]))

                m <- leaflet::addLegend(map = m, position = "topright", pal = pal_n[[i]],
                                        opacity = 1, values = vals[[i]], title = "Legend")
                
                if (i == 1) {
                  m <- leaflet::addLayersControl(map = m,
                                                 position = "topleft",
                                                 baseGroups = c("OpenStreetMap",
                                                                "Esri.WorldImagery"),
                                                 overlayGroups = c(
                                                   m$x$calls[[len]]$args[[2]],
                                                   names(lst[[i]])))
                } else {
                  m <- leaflet::addLayersControl(map = m,
                                                 position = "topleft",
                                                 baseGroups = c("OpenStreetMap",
                                                                "Esri.WorldImagery"),
                                                 overlayGroups = c(
                                                   m$x$calls[[len]]$args[[2]],
                                                   names(lst[[i]])))
                }
              }
              
            } else {
              
              df <- as.data.frame(sapply(x@data, as.character),
                                  stringsAsFactors = FALSE)
              
              nms <- names(df)
              grp <- strsplit(strsplit(as.character(sys.calls()[1]), 
                                       "\\(")[[1]][2], ",")[[1]][1]
              
              txt <- sapply(seq(nrow(x@data)), function(i) {
                paste(nms, df[i, ], sep = ": ")
              })
              
              txt <- sapply(seq(ncol(txt)), function(j) {
                paste(txt[, j], collapse = " <br> ")
              })
              
              m <- leaflet::addPolygons(m,
                                        weight = weight,
                                        group = grp,
                                        color = cols[length(cols)],
                                        popup = txt)

              len <- length(m$x$calls)
              
              m <- leaflet::addLayersControl(map = m,
                                             position = "topleft",
                                             baseGroups = c("OpenStreetMap",
                                                            "Esri.WorldImagery"),
                                             overlayGroups = c(
                                               m$x$calls[[len]]$args[[2]],
                                               grp))
            }
            
            return(m)
            
          }
          
)



## Satellite object =======================================================

#' @describeIn mapView

setMethod('mapView', signature(x = 'Satellite'), 
          function(x,
                   ...) {
            
            pkgs <- c("leaflet", "satellite", "magrittr")
            tst <- sapply(pkgs, "requireNamespace", 
                          quietly = TRUE, USE.NAMES = FALSE)
            
            lyrs <- x@layers
            
            m <- mapView(lyrs[[1]], ...)
            
            if (length(lyrs) > 1) {
              for (i in 2:length(lyrs)) {
                m <- mapView(lyrs[[i]], m, ...)
              }
            }
            
            return(m)
            
          }
          
)
