#' an Rsenal of functions by Environmentalinformatics Marburg
#' 
#' A collection of functions which we find helpful/useful for our 
#' day-to-day ventures into data analysis at Environmentalinformatics
#' group, Department of Geography, Philipps University Marburg.
#' There is no specific focus of interest in this collection, some 
#' functions are very task specific (e.g. \code{\link{roc}} which let's
#' you create roc curves for land-use classifications), yet others are
#' very general and widely applicable (e.g. \code{\link{latticeCombineGrid}}
#' which produces a panel plot based on a list of lattice plot objects).
#' 
#' @name Rsenal-package
#' @aliases Rsenal
#' @docType package
#' @title Rsenal: magic functions for things various
#' @author Tim Appelhans, Florian Detsch, Insa Otte, Meike Kuehnlein, 
#' Hanna Meyer, Spaska Forteva, Thomas Nauss, Stefan Woellauer\cr
#' \cr
#' \emph{Maintainer:} Tim Appelhans \email{tim.appelhans@@gmail.com}
#' 
#' @keywords package
#' 
#' @import Rcpp
#' @useDynLib Rsenal
NULL
#' 
#' @docType data 
#' @name kili12
#' @title KiLi FOR1246 Habitats
#' @description data frame of KiLi FOR1246 Habitats and respective elevations
#' @details Note, the Habitats are ordered according to their mean elevation
#' @format a data frame with the following columns\cr
#' \cr
#' Habitat    : factor with levels ordered according to their mean elevation\cr
#' Elevation  : numeric\cr
#' 
NULL
#' 
#' @docType data 
#' @name kili60
#' @title KiLi FOR1246 PlotIDs
#' @description data frame of KiLi FOR1246 PlotIDs and respective elevations
#' @details Note, the PlotIDs are ordered according to their mean elevation
#' @format a data frame with the following columns\cr
#' \cr
#' PlotID    : factor with levels ordered according to their mean elevation\cr
#' Elevation  : numeric\cr
#' 
NULL
#' @import leaflet scales
NULL
