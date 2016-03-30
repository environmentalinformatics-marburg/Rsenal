#' create a word cloud from a character string
#' 
#' @description
#' this function creates a wordcloud based on a character string. 
#' Optionally, a file with common words to be excluded can be 
#' supplied. See \code{\link{tagcloud}} for details on the cloud building. 
#' 
#' @param txt character string from which the cloud will be computed
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
#' @export charCloud
#' @aliases charCloud

charCloud <- function(txt, exclude = NULL, nwords = 180,
                     min.char.length = 5, max.char.length = 30,
                     clrs = c("#8c510a", "#bf812d", "#35978f",
                              "#01665e", "#7fbc41", "#4d9221"),
                     seed = 123, wmin = NULL, stat = FALSE, 
                     sortby = c("freq, names"), ...) {

  sortby <- sortby[1]
  
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
  
  
  freqs <- data.frame(NAMES = names(freqs),
                      FREQ = as.integer(freqs))
  
  if(sortby == "freq"){
    freqs <- freqs[order(freqs$FREQ, decreasing = TRUE),]  
  }
  
  tcloud <- tagcloud::tagcloud(tags, wgts, wmin = wmin,
                               col = clrs, ...)
  
  
  if(stat == TRUE){
    return(list(freqs = freqs, tcloud = tcloud))
  } else {
    return(tcloud)
  }
  
}

