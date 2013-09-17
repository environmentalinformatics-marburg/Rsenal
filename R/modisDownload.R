modisDownload <- function(modis.products, 
                          modis.download.only = T,
                          modis.outproj = "asIn",
                          ...) {

  #########################################################################################
  # Parameters are as follows:
  #
  # modis.products (character):     Names of the desired MODIS products. See
  #                                 ?getProduct for detailed information.
  # modis.download.only (logical):  Download only (default) or download and
  #                                 reproject the specified products.
  # modis.outproj (character):      Desired output CRS. Default is "asIn"
  #                                 (i.e. MODIS Sinusoidal).
  # ...:                            Further arguments passed on to getHdf and, 
  #                                 runGdal, respectively.
  #
  #########################################################################################
  
  # Required libraries
  stopifnot(require(MODIS))
  
  # Download MODIS data with given extent
  if (modis.download.only) {
    lapply(modis.products, function(i) {
      getHdf(i, ...)
    })
    
  # Download and extract MODIS data with given extent
  } else {
    # Extract specified SDS from .hdf files
    lapply(modis.products, function(i) {
      runGdal(i, outProj = modis.outproj, ...)
    })
  }
  
  # Return message when download is finished
  return("Processing MODIS data finished!")
}

# ### Call
# 
# MODISoptions(localArcPath = "G:/ki_modis_ndvi/data/MODIS_ARC", 
#              outDirPath = "G:/ki_modis_ndvi/data/MODIS_ARC/PROCESSED")
# modisDownload(modis.products = c("MOD14A1", "MYD14A1"), 
#               modis.download.only = F, 
#               extent = extent(37, 37.72, -3.4, -2.84),
#               SDSstring = "1100",
#               begin = "2013140", 
#               modis.outproj = "32737", 
#               job = "md14_tmp")
