#' bump package 'Version:' and 'Date:' in DESCRIPTION file
#'
#' @description
#' this function let's you bump the version number and creation date of
#' your package's DESCRIPTION file. Additionally, it bumps the version 
#' numbers of a NEWS.md file and automatically generates a corresponding 
#' plain NEWS file (for R-help pages). 
#' Supported versioning system is major.minor.patch
#'
#' @param element character - one of "major", "minor", "patch" to be bumped.
#' Defaults to "patch"
#' @param pkg.repo path to package repository folder. Default is current
#' working directory (".")
#' @param news the NEWS.md file of the repo (assumed to be in top level path). 
#' If this exists, the first line of that file will be rewritten 
#' to be "<packagename> <major.minor.patch>". Note that the current implementation 
#' assumes that the NEWS file is in .md format, thus NEWS.md. A plain NEWS
#' file (for R-help pages) will be generated automatically.
#' @param plain_news whether to generate a plain NEWS file in the package
#' root directory from the NEWS.md file supplied to argument \code{news}.
#'
#' @author
#' Tim Appelhans
#'
#' @name bumpVersion-deprecated
#' @usage bumpVersion(element = "patch", pkg.repo = "."
#' , news = file.path(pkg.repo, "NEWS.md"), plain_news = TRUE)     
#' @seealso \code{\link{Rsenal-deprecated}}  
#' @keywords internal
NULL

#' @rdname Rsenal-deprecated
#' @section \code{bumpVersion}:
#' For \code{bumpVersion}, use \code{\link[Orcs]{bumpVersion}} instead.
#' 
#' @export 
bumpVersion <- function(element = "patch", pkg.repo= ".", 
                        news = file.path(pkg.repo, "NEWS.md"),
                        plain_news = TRUE) {

  .Deprecated("Orcs::bumpVersion", "Rsenal")
  Orcs::bumpVersion(element, pkg.repo, news, plain_news)
}
