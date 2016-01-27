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
                     seed = 123, wmin = NULL, ...) {

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
  if (is.null(exclude)) excl <- NA else excl <- readLines(exclude)

  wrds <- strsplit(txt, " ")
  wrds <- unlist(wrds)
  wrdsvec <- gsub("\\,", "", wrds)
  wrdsvec <- gsub("\\:", "", wrds)
  wrdsvec <- gsub("\\.", "", wrdsvec)
  wrdsvec <- gsub("\\)", "", wrdsvec)
  wrdsvec <- gsub("\\(", "", wrdsvec)
  wrdsvec <- wrdsvec[nchar(wrdsvec) > min.char.length]
  wrdsvec <- wrdsvec[nchar(wrdsvec) < max.char.length]

  "%w/o%" <- function(x, y) x[!x %in% y]

  wrdsvec <- tolower(wrdsvec) %w/o% c(excl, letters, LETTERS, 1:1e6, "")
  freqs <- table(wrdsvec)

  tags <- names(freqs[order(freqs, decreasing = TRUE)][1:nwords])
  wgts <- freqs[order(freqs, decreasing = TRUE)][1:nwords]

  if (is.null(wmin)) wmin <- quantile(wgts, probs = 0.6)

  set.seed(seed)
  return(tagcloud::tagcloud(tags, wgts, wmin = wmin,
                            col = clrs, ...))

}

