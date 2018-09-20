### To include non-CRAN packages from GitHub, do the following ----

# .onLoad <- function(lib, pkg) {
# 
#   ## Orcs
#   avl <- length(find.package("Orcs", quiet = TRUE)) > 0
# 
#   if (!avl) {
#     devtools::install_github("fdetsch/Orcs")
#   }
# }
