Rsenal
======

magic R functions for things various

<hr>

In order to install the package, **SDMTools** needs to be available. It can be installed from the CRAN archive like this:

```r
remotes::install_version(
  "SDMTools"
  , version = "1.1-221.2"
)
```

In addition, issues with **gdalUtils** might occur ("ERROR: dependency ‘gdalUtils’ is not available for package ‘Rsenal’"). These can be resolved with:

```r
remotes::install_version(
    "gdalUtils"
    , version = "2.0.1.7"
)
```

Then, install **Rsenal** as follows:

```r
remotes::install_github(
  "environmentalinformatics-marburg/Rsenal.git"
  # , Ncpus = parallel::detectCores() - 1L
)
```

