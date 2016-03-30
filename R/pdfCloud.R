#' create a word cloud from a bunch of pdfs
#' 
#' @description
#' this function creates a wordcloud by converting a bunch of pdf files to
#' txt files. Optionally, a file with common words to be excluded can be 
#' supplied. See \code{\link{tagcloud}} for details on the cloud building. 
#' 
#' @param pdf.path path to the pdf folder
#' @param exclude optional path to a .txt file with words to be excluded 
#' from the cloud
#' @param nwords the number of words to be plotted
#' @param min.char.length minimum length of words to be included in the cloud
#' @param max.char.length maximum length of words to be included in the cloud
#' @param clrs vector of colors to be used for the word cloud
#' @param seed seed to be used for the random word placement
#' @param wmin see \code{\link{tagcloud}} for details
#' @param stat return statistics (words and frequencies)
#' @param sortby if stat is TRUE, sort statistics by names or frequency
#' @param ... additional arguments passed to \code{\link{tagcloud}}
#' 
#' @author
#' Tim Appelhans
#' 
#' @seealso
#' \code{\link{tagcloud}}
#' 
#' @export pdfCloud
#' @aliases pdfCloud

pdfCloud <- function(pdf.path, exclude = NULL, nwords = 180,
                     min.char.length = 5, max.char.length = 30,
                     clrs = c("#8c510a", "#bf812d", "#35978f",
                              "#01665e", "#7fbc41", "#4d9221"),
                     seed = 123, wmin = NULL, stat = FALSE, 
                     sortby = c("freq, names"), ...) {

  if(!system('pdftotext -v') == 0) {
    stop("please install 'pdftotext'")
  }

  pdflist <- list.files(pdf.path,
                        pattern = glob2rx("*.pdf"),
                        full.names = TRUE,
                        recursive = TRUE)


  lapply(seq(pdflist), function(i) {
    system(paste('pdftotext', pdflist[i],
                 paste(pdflist[i], ".txt", sep = "")))
  })

  txtlist <- list.files(pdf.path,
                        pattern = glob2rx("*.txt"),
                        full.names = TRUE,
                        recursive = TRUE)

  txt <- lapply(seq(txtlist), function(i) {
    txt <- readLines(txtlist[i])
  })

  txt <- unlist(txt)
  return(charCloud(txt, ...))
}

