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
#' ### vector data ###
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
#' @export mapView
#' @name mapView
#' @rdname mapView
#' @aliases mapView,RasterStack-method

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
  
            stopifnot(require(leaflet))
            
            is.fact <- raster::is.factor(x)
            
            llcrs <- CRS("+init=epsg:3857")@projargs
            
            if (!identical(projection(x), llcrs)) {
              cat("\n", "reprojecting to web mercator", "\n\n")
              if (is.fact) {
                x <- raster::projectRaster(x, crs = llcrs, method = "ngb")
              } else {
                x <- projectRaster(x, crs = llcrs)
              }
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
              pal <- colorFactor(cols, as.factor(values), 
                                 levels = values, na.color = na.color)
            } else {
              pal <- colorNumeric(cols, values, na.color = na.color)
            }
            
            ## create base map using specified map types
            if (is.null(map)) {
              m <- leaflet() %>%
                addTiles(group = map.types[1]) %>%
                addProviderTiles(provider = map.types[2],
                                 group = map.types[2])
            } else {
              m <- map
            }
            
            ## add layers to base map
            m <- addRasterImage(map = m,
                                x = x,
                                colors = pal,
                                project = FALSE,
                                opacity = layer.opacity,
                                group = names(x))

            if (legend) {
              ## add legend
              m <- addLegend(map = m,
                             pal = pal,
                             opacity = legend.opacity, 
                             values = values, ...)
            }
            
            ## add layer control buttons
            if (is.null(map)) {
              m <- addLayersControl(map = m,
                                  position = "topleft",
                                  baseGroups = map.types,
                                  overlayGroups = names(x))
            } else {
              list.len <- length(map$x$calls)
              m <- addLayersControl(map = m,
                                    position = "topleft",
                                    baseGroups = map.types,
                                    overlayGroups = 
                                      c(m$x$calls[[list.len]]$args[[2]], 
                                        names(x)))
            }
            
            return(m)
            
          }
          
)

#' @describeIn mapView

setMethod('mapView', signature(x = 'RasterStack'), 
          function(x, 
                   ...) {
            
            m <- mapView(x[[1]], ...)
            
            for (i in 2:nlayers(x)) {
              m <- mapView(x[[i]], m, ...)
            }
            
            return(m)
            
          }
          
)

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
            
            stopifnot(require(leaflet))
            
            llcrs <- CRS("+init=epsg:4326")@projargs
            
            if (!identical(projection(x), llcrs)) {
              cat("\n", "reprojecting to web mercator", "\n\n")
              x <- spTransform(x, CRSobj = llcrs)
            }
            
            if (is.null(map)) {
              m <- leaflet() %>%
                addTiles(group = map.types[1]) %>%
                addProviderTiles(provider = map.types[2],
                                 group = map.types[2])
            } else {
              m <- map
            }
            
            if (burst) {
              lst <- lapply(names(x), function(j) x[j])
              
              vals <- lapply(seq(lst), function(i) lst[[i]]@data[, 1])
              
              pal_n <- lapply(seq(lst), function(i) {
                if (is.factor(lst[[i]]@data[, 1])) {
                  colorFactor(cols, lst[[i]]@data[, 1], 
                              levels = levels(lst[[i]]@data[, 1]))
                } else {
                  colorNumeric(cols, vals[[i]], na.color = "transparent")
                }
              })
            
              for (i in seq(lst)) {
                len <- length(m$x$calls)
                m <- addCircleMarkers(m, lng = coordinates(lst[[i]])[, 1],
                                      lat = coordinates(lst[[i]])[, 2],
                                      group = names(lst[[i]]),
                                      color = pal_n[[i]](vals[[i]]))
                
                
                m <- addMarkers(m, lng = coordinates(lst[[i]])[, 1],
                                lat = coordinates(lst[[i]])[, 2],
                                group = names(lst[[i]]),
                                options = markerOptions(opacity = 0),
                                popup = as.character(vals[[i]]))
                
                m <- addLegend(map = m, position = "topright", pal = pal_n[[i]],
                               opacity = 1, values = vals[[i]], title = "Legend")
                
                if (i == 1) {
                  m <- addLayersControl(map = m,
                                        position = "topleft",
                                        baseGroups = c("OpenStreetMap",
                                                       "Esri.WorldImagery"),
                                        overlayGroups = c(
                                          m$x$calls[[len]]$args[[2]],
                                          names(lst[[i]])))
                } else {
                  m <- addLayersControl(map = m,
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
              
              m <- addCircleMarkers(m, lng = coordinates(x)[, 1],
                                    lat = coordinates(x)[, 2],
                                    group = grp,
                                    color = cols[length(cols)])
              
              txt <- sapply(seq(nrow(x@data)), function(i) {
                paste(nms, df[i, ], sep = ": ")
              })
              
              txt <- sapply(seq(ncol(txt)), function(j) {
                paste(txt[, j], collapse = " <br> ")
              })
              
              m <- addMarkers(m, lng = coordinates(x)[, 1],
                              lat = coordinates(x)[, 2],
                              group = grp,
                              options = markerOptions(opacity = 0),
                              popup = txt)
              
              m <- addLayersControl(map = m,
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
