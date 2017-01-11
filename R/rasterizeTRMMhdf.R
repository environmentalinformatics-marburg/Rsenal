#' Rasterize TRMM 3B42 HDF Data
#' 
#' @description
#' Rasterize TRMM 3B42 HDF files.
#' 
#' @param x \code{character}. TRMM 3B42 .HDF file(s). 
#' @param SDSstring \code{character}. Determines which SDS layers to process 
#' (1 = process, 0 = do not process). The default is to process all SDS layers 
#' (i.e., "111111").
#' @param ext \code{Extent}, or any object from which an \code{Extent} can be 
#' extracted, see \code{\link[raster]{crop}}.
#' @param cores \code{integer}. Number of cores for parallel processing.
#' @param filename \code{character}. Optional output filename(s). If specified, 
#' this must be of the same length as 'x'.
#' @param ... Additional arguments passed to \code{\link{writeRaster}}.
#' 
#' @return 
#' A \code{list} of \code{Raster*} objects, with each entry corresponding to 
#' one element in 'x' and the number of layers per \code{Raster*} object 
#' corresponding to the processed SDS layers. 
#' 
#' @references 
#' GES DISC (2016) Readme for TRMM Product 3B42 (V7). Available online: 
#' \url{http://disc.sci.gsfc.nasa.gov/precipitation/documentation/TRMM_README/TRMM_3B42_readme.shtml}.
#' 
#' @examples 
#' ## see ?rasterizeTRMM
#'
#' @export rasterizeTRMMhdf
#' @name rasterizeTRMMhdf
rasterizeTRMMhdf <- function(x, SDSstring = "111111", ext = NULL, cores = 1L, 
                             filename = '', ...) {
  
  ## reformat 'SDSstring'
  SDSstring <- strsplit(SDSstring, "")[[1]]
  SDSstring <- as.integer(SDSstring)
  
  ## parallelization  
  cl <- parallel::makeCluster(cores)
  on.exit(parallel::stopCluster(cl))

  vld <- length(filename) == length(x)
  filename <- if (vld) filename else rep("", length(x))
  
  dots <- list(...)
  parallel::clusterExport(cl, c("x", "SDSstring", "ext", "filename", "dots"), 
                          envir = environment())
  
  ## loop over files
  parallel::parLapply(cl, 1:length(x), function(i) {
    
    # get required sds
    sds <- gdalUtils::get_subdatasets(x[i])
    
    lst <- vector("list", length(SDSstring))
    for (j in 1:length(SDSstring)) {
      lst[[j]] <- if (SDSstring[j] == 1) {
        
        # windows operating system
        if (Sys.info()[["sysname"]] == "Windows") {
          trl <- gsub(".HDF", ".tif", x[i])
          gdalUtils::gdal_translate(sds[j], dst_dataset = trl, 
                                    output_Raster = TRUE) * 1

        # all other operating systems (currently tested on linux only)            
        } else {
          raster::raster(sds[j])
        }
      } else {
        NULL
      }
    }
    
    # delete temporary raster file (windows only)
    if (Sys.info()[["sysname"]] == "Windows" & exists("trl"))
      jnk <- file.remove(trl)
      
    rst <- raster::stack(lst)
    
    # assign spatial information
    rst <- raster::t(rst)
    raster::projection(rst) <- "+init=epsg:4326"
    raster::extent(rst) <- raster::extent(c(-180, 180, -50, 50))
    
    # discard missing values
    val <- raster::getValues(rst)
    val[val < -9999] <- NA
    rst <- raster::setValues(rst, val)
    
    # clip layers (optional)
    if (!is.null(ext)) 
      rst <- raster::crop(rst, ext, snap = "out")
    
    # write layers to file (optional)
    if (filename[i] != "") {
      dots_sub <- list(x = rst, filename = filename[i])
      dots_sub <- append(dots_sub, dots)
      rst <- do.call(raster::writeRaster, dots_sub)
    }
    
    return(rst)
  })
}

