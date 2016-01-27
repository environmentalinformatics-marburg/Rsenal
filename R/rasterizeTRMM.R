#' Rasterize TRMM 3B42 binary data
#' 
#' @description
#' Rasterize TRMM 3B42 daily binary data, e.g. previously downloaded via 
#' \code{\link{downloadTRMM}}.
#' 
#' @param binary Character. TRMM binary file (*.bin). 
#' @param meta Character. Corresponding metadata file (*.xml). 
#' @param write_out Logical. Enables output storage.
#' 
#' @return
#' A \code{RasterLayer} object.
#' 
#' @author
#' Florian Detsch
#' 
#' @seealso
#' \code{\link{readBin}}, \code{\link{downloadTRMM}}
#' 
#' @references
#' \url{https://stat.ethz.ch/pipermail/r-sig-geo/attachments/20110913/5ece92b0/attachment.pl}
#' 
#' @examples  
#' \dontrun{
#' ## download TRMM 3B42 data from Jan 1 to Jan 5, 2015
#' fls_trmm <- downloadTRMM(begin = "2015-01-01", end = "2015-01-05")
#' 
#' ## rasterize
#' rst_trmm <- rasterizeTRMM(binary = fls_trmm[1, 1], meta = fls_trmm[1, 2], 
#'                           write_out = FALSE)
#'                             
#' plot(rst_trmm)                                     
#' }
#'               
#' @export rasterizeTRMM
#' @aliases rasterizeTRMM
rasterizeTRMM <- function(binary, meta, write_out = TRUE) {
  
  ## import metadata
  ch_meta <- readLines(meta)
  
  ## extract bounding box
  num_bbox <- sapply(c("WestBoundingCoordinate", "NorthBoundingCoordinate", 
           "EastBoundingCoordinate", "SouthBoundingCoordinate"), 
         function(i) {
           
           tmp_int_id <- grep(i, ch_meta)
           tmp_ch_crd <- ch_meta[tmp_int_id]
           
           tmp_ch_crd <- gsub(" ", "", tmp_ch_crd)
           tmp_ch_crd <- gsub("/", "", tmp_ch_crd)
           tmp_ls_crd <- strsplit(tmp_ch_crd, paste0("<", i, ">"))
           tmp_ch_crd <- unlist(tmp_ls_crd)[[2]]
           tmp_ls_crd <- strsplit(tmp_ch_crd, paste0("</", i, ">"))
           tmp_ch_crd <- unlist(tmp_ls_crd)
           tmp_num_crd <- as.numeric(tmp_ch_crd)
           
           return(tmp_num_crd)                          
         })

  ## number of rows and columns
  int_ncols <- (num_bbox[3] - num_bbox[1]) / 0.25
  int_nrows <- (num_bbox[2] - num_bbox[4]) / 0.25
  
  ## build world grid
  rst_trmm <- raster(nrows = int_nrows, ncols = int_ncols, 
                     xmn = num_bbox[1]-180, xmx = num_bbox[3]-180, 
                     ymn = num_bbox[4], ymx = num_bbox[2])
  rst_trmm <- setValues(rst_trmm, 0)
  
  ## import binary rainfall data
  num_val <- readBin(binary, "double", n = int_ncols * int_nrows, size = 4, 
                     endian = "big")
  
  ## insert values into raster template and save
  ## (taken from https://stat.ethz.ch/pipermail/r-sig-geo/attachments/20110913/5ece92b0/attachment.pl)
  rst_trmm[] <- num_val
  
  ch_fls_out <- gsub("\\.", "_", basename(binary))
  ch_fls_out <- paste(dirname(binary), substr(ch_fls_out, 1, nchar(ch_fls_out)-4), 
                      sep = "/")
  
  rst_trmm <- flip(rst_trmm, direction = "y", 
                   filename = ifelse(write_out, ch_fls_out, ""), 
                   format = "GTiff", overwrite = TRUE)
  
  return(rst_trmm)
}