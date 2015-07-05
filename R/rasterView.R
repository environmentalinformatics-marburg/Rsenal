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
#' @param ... additional arguments passed on to \code{\link{addLegend}} 
#' 
#' @author
#' Tim Appelhans
#' 
#' @examples
#' LUC1990 <- raster(system.file("LUC1990.rst", package = "Rsenal"))
#' LUC2006 <- raster(system.file("LUC2006.rst", package = "Rsenal"))
#' 
#' rasterView(as.factor(LUC1990))
#' rasterView(LUC2006)
#' 
#' stck <- stack(LUC1990, LUC2006)
#' rasterView(stck)
#' 
#' @export rasterView
#' @aliases rasterView

rasterView <- function(x, 
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
  
  llcrs <- CRS("+init=epsg:3857")@projargs
  
  if (!identical(projection(x), llcrs)) {
    cat("\n", "reprojecting to web mercator", "\n\n")
    if (any(is.factor(x))) {
      x <- projectRaster(x, crs = llcrs, method = "ngb")
    } else {
      x <- projectRaster(x, crs = llcrs)
    }
  }
  
  
  if (is.null(values)) {
    values <- seq(round(min(x[], na.rm = TRUE), 5),
                  round(max(x[], na.rm = TRUE), 5),
                  length.out = 10)
  } else {
    values <- round(values, 5)
  }
  
  if (is.factor(x)) {
    pal <- colorFactor(cols, as.factor(values), levels = values)
  } else {
    pal <- colorNumeric(cols, values, na.color = na.color)
  }
  
  ## create base map using specified map types
  m <- leaflet() %>%
    addTiles(group = map.types[1]) %>%
    addProviderTiles(provider = map.types[2],
                     group = map.types[2])
  
  ## add layers to base map
  for (i in 1:nlayers(x)) {
    m <- addRasterImage(map = m,
                        x = x[[i]],
                        colors = pal,
                        project = FALSE,
                        opacity = layer.opacity,
                        group = names(x)[i])
  }
  
  if (legend) {
    ## add legend
    m <- addLegend(map = m,
                   pal = pal,
                   opacity = legend.opacity, 
                   values = values, ...)
  }
  
  ## add layer control buttons
  m <- addLayersControl(map = m,
                        position = "topleft",
                        baseGroups = map.types,
                        overlayGroups = names(x))
 
  return(m)
  
}
