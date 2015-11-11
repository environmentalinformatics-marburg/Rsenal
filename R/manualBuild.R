manualBuild <- function(dsn = getwd(), document = TRUE, ...) {

  ## reset 'dsn' to 'H:/...'  
  if (grep("students_smb", dsn) == 1) {
    lst_dsn <- strsplit(dsn, "/")
    chr_dsn <- unlist(lst_dsn)[3:5]
    dsn <- paste0("H:/", paste(chr_dsn, collapse = "/"))
  }

  ## if 'document = TRUE', create documentation 
  if (document) {
    cat("\nCreating package documentation...\n")
    roxygen2::roxygenize(package.dir = dsn, 
                         roclets = c('rd', 'collate', 'namespace'))
  }
  
  ## build package
  cat("\nBuilding package...\n")
  devtools::build(pkg = dsn, path = dirname(dsn), ...)
  
  ## install package
  cat("Installing package...\n")
  pkg <- list.files(dirname(dsn), full.names = TRUE,
                    pattern = paste0(basename(dsn), ".*.tar.gz$"))
  pkg <- pkg[length(pkg)]
  
  install.packages(pkg, repos = NULL)
  
  return(invisible(NULL))
}