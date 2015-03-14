% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/hist_boundaries.R
\name{hist_boundaries}
\alias{hist_boundaries}
\title{Swe Boundaries}
\usage{
hist_boundaries(date, type = c("parish", "county"), format = c("sp", "df",
  "meta"))
}
\arguments{
\item{date}{a date or a year}

\item{type}{type of unit, "county" or "municipal"}

\item{format}{format of return object, "df" for data.frame and "sp" for
SpatialPolygonsDataFrame}
}
\description{
Get swedish administrative boundaries for a specified year.
}
\examples{
map <- hist_boundaries(1900, "county")
library(sp)
plot(map)
}
