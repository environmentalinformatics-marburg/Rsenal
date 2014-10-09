#' Create ENVI header file
#' 
#' @description
#' Create an ENVI header file (see \url{http://www.exelisvis.com/docs/ENVIHeaderFiles.html}) 
#' following the documentation for \code{\link{read.ENVI}}.
#' 
#' @param hdr Character. If not supplied, defaults to the companion header file 
#' for GIMMS 3G binary data.
#' @param file Character. Filepath passed on to \code{\link{writeLines}}. 
#' @param ... Further arguments. Currently not in use. 
#' 
#' @return
#' A filename corresponding to \code{file} argument.
#' 
#' @author
#' Florian Detsch
#' 
#' @seealso
#' \code{\link{read.ENVI}}
#' 
#' @examples  
#' # Create standard GIMMS 3G header file in current working directory
#' createHdr()
#'               
#' @export createHdr
#' @aliases createHdr
createHdr <- function(hdr = NULL, 
                      file = "./tmp.hdr",
                      ...) {
  
  # ENVI .hdr string
  if (is.null(hdr))
    hdr <- paste("ENVI", 
                 "description = { R-language data }", 
                 "samples = 2160", 
                 "lines = 4320", 
                 "bands = 1", 
                 "data type = 2",
                 "header offset = 0", 
                 "interleave = bsq", 
                 "byte order = 1", sep = "\n")
  
  # ENVI .hdr file
  writeLines(hdr, file)
  
  return(file)
}