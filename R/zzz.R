### This needs to be commented once 'Orcs' is available on CRAN ----

.onLoad <- function(lib, pkg) {

  ## Orcs
  avl <- length(find.package("Orcs", quiet = TRUE)) > 0

  if (!avl) {
    devtools::install_github("fdetsch/Orcs")
  }
}
