\name{readCDO}
\alias{readCDO}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Read climate station data from "Climate Data Online"
}
\description{
  Read climate station data from CDO (http://gis.ncdc.noaa.gov/map/viewer/). Function is mainly designed for reading hourly global data which come along with a format not directly readable by R.
}
\usage{
  readCDO(file="", data="HourlyGlobal", advanced=TRUE, sep=",")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
    The name of the file the data are to be read from.
  }
  \item{data}{
    The name of the data source downloaded at http://gis.ncdc.noaa.gov/map/viewer/. Currently Hourly Global ("HourlyGlobal"), Global summary of the day ("GSOD") and Global Historical Climatology Network-Daily "GHCN" are supported
  }
  \item{advanced}{
    logical. Indicated whether advanced download option for "HourlyGlobal" was used or not.
  }
  \item{sep}{
    The field separator character. Note that the separation character of the the header might differ in some cases which however can   be ignored since the function will account on this.
  }
}

\value{
  A data frame containing a representation of the data in the file.
}
\note{
Space separated data are only supported with the options data="HourlyGlobal" and advanced==FALSE.
}

\author{
  Hanna Meyer
}
\seealso{
 \code{\link{read.table}}
}
\examples{
#### Example 1: Read a table of hourly global data downloaded using advanced download option

file=system.file("ClimateStationData.txt", package="Rsenal")
readCDO(file)
}