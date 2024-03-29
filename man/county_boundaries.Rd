% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hist_boundaries.R
\name{county_boundaries}
\alias{county_boundaries}
\title{County boundaries}
\usage{
county_boundaries(date, format = c("sp", "df", "meta"))
}
\arguments{
\item{date}{a date, a year or a vector with date/year range}

\item{format}{format of return object, "df" for data.frame, "sp" for 
SpatialPolygonsDataFrame and "meta" for only meta-data.}
}
\description{
Get Swedish county boundaries for a specified year 1634-1990.
}
