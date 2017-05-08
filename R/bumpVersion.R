#' bump package 'Version:' and 'Date:' in DESCRIPTION file
#'
#' @description
#' this function let's you bump the version number and creation date of
#' your package's DESCRIPTION file.
#' Supported versioning system is major.minor.patch
#'
#' @param element character - one of "major", "minor", "patch" to be bumped.
#' Defaults to "patch"
#' @param pkg.repo path to package repository folder. Default is current
#' working directory
#' @param news the NEWS file of the repo (assumed to be in top level path). 
#' If this exists, the first line of that file will be rewritten 
#' to be "<packagename> <major.minor.patch>"
#'
#' @author
#' Tim Appelhans
#'
#' @export bumpVersion
#' @name bumpVersion
bumpVersion <- function(element = "patch", pkg.repo= ".", 
                        news = file.path(pkg.repo, "NEWS.md"),
                        plain_news = TRUE) {

  ### DESCRIPTION file
  desc <- readLines(paste(pkg.repo, "DESCRIPTION", sep = "/"))
  
  old.ver <- substr(desc[grep("Version*", desc)], 10,
                    nchar(desc[grep("Version*", desc)]))

  old <- as.numeric(unlist(strsplit(old.ver, "\\.")))

  new.v <- switch(element,
                  major = c(old[1] + 1, 0, 0),
                  minor = c(old[1], old[2] + 1, 0),
                  patch = c(old[1], old[2], old[3] + 1))

  new.ver <- paste(new.v[1], new.v[2], new.v[3], sep = ".")

  new.v <- new.v[1] * 100 + new.v[2] * 10 + new.v[3]
  old <- old[1] * 100 + old[2] * 10 + old[3]

  desc[grep(glob2rx("Version*"), desc)] <- paste("Version: ", new.ver, sep = "")
  desc[grep(glob2rx("Date*"), desc)] <- paste("Date:", Sys.Date(), sep = " ")

  writeLines(desc, paste(pkg.repo, "DESCRIPTION", sep = "/"))

  ### pkg.name-package.Rd file
  pkg.name <- substr(desc[grep(glob2rx("Package:*"), desc)], 10,
                     nchar(desc[grep(glob2rx("Package:*"), desc)]))

  pkg.doc <- readLines(paste(pkg.repo, "man",
                             paste(pkg.name, "-package.Rd", sep = ""),
                             sep = "/"))

  pkg.doc[grep(glob2rx("Version*"), pkg.doc)] <- paste("Version: \\tab ",
                                                       new.ver, "\\cr", sep = "")
  pkg.doc[grep(glob2rx("Date*"), pkg.doc)] <- paste("Date: \\tab ",
                                                    Sys.Date(), "\\cr", sep = "")

  writeLines(pkg.doc, paste(pkg.repo, "man",
                            paste(pkg.name, "-package.Rd", sep = ""),
                            sep = "/"))

  ## NEWS
  if (file.exists(news)) {
    newsfile <- readLines(news)
    newsfile[1] <- paste("##", pkg.name, new.ver)
    writeLines(newsfile, con = news)
    if (basename(news) == "NEWS.md") {
      nfl = gsub("## ", "", newsfile)
      writeLines(nfl, con = gsub(".md", "", news))
    }
  }
}
