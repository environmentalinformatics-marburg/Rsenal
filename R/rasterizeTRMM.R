#' Rasterize TRMM 3B42 Data
#' 
#' @description
#' Rasterize TRMM 3B42 .bin or .HDF files.
#' 
#' @param x \code{character}. TRMM 3B42 file(s). Currently supported filetypes 
#' are .bin and .HDF. Note that \code{\link{rasterizeTRMMbin}} takes one file 
#' only.
#' @param ... Additional arguments passed to \code{\link{rasterizeTRMMbin}} or 
#' \code{\link{rasterizeTRMMhdf}}.
#' 
#' @return
#' \itemize{
#' \item{\strong{.bin files}: A single \code{RasterLayer} object.}
#' \item{\strong{.HDF files}: A \code{list} of \code{Raster*} objects.}
#' }
#' 
#' @author
#' Florian Detsch
#' 
#' @seealso
#' \code{\link{downloadTRMM}}, \code{\link{readBin}}, 
#' \code{\link{rasterizeTRMMbin}}, \code{\link{rasterizeTRMMhdf}}.
#' 
#' @examples  
#' \dontrun{
#' ## download 3-hourly TRMM 3B42 data from Jan 1, 2015
#' fls <- downloadTRMM(begin = "2015-01-01", end = "2015-01-01", 
#'                     type = "3-hourly")
#' fls[1]                     
#' 
#' ## rasterize
#' library(rworldmap)
#' eth <- subset(countriesCoarse, ADMIN == "Ethiopia")
#' rst <- rasterizeTRMM(fls, ext = eth)
#' 
#' ## visualize
#' plot(rst[[1]])
#' }
#' 
#' @export rasterizeTRMM
#' @name rasterizeTRMM
rasterizeTRMM <- function(x, ...) {
  
  filetype <- strsplit(x, "\\.")  
  filetype <- unique(sapply(filetype, "[[", length(filetype[[1]])))
  
  if (length(filetype) > 1)
    stop("Please make sure all files in 'x' are of the same filetype.\n")
  
  if (filetype %in% c("HDF", "hdf")) {
    rasterizeTRMMhdf(x, ...)
  } else if (filetype %in% c("BIN", "bin")) {
    rasterizeTRMMbin(x, ...)
  } else {
    stop("Provided filetype currently not supported, see ?rasterizeTRMM. \n")
  }
}

