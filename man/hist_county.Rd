% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datasets.R
\docType{data}
\name{hist_county}
\alias{hist_county}
\title{Historical county boundaries}
\format{A \code{SpatialPolygonsDataFrame} with 101 elements
\describe{
  \item{lan}{County code}
  \item{from}{Starting year}
  \item{tom}{End year}
  \item{name}{County name}
  \item{letter}{County letter code}
  \item{center}{County center}
  \item{geomid}{Unique geomid}
  \item{area}{Area in square meters}
}}
\source{
NAD Topography data from Riksarkivet \url{http://riksarkivet.se/psidata}.
}
\usage{
data(hist_county)
}
\description{
Swedish county borders 1600-1990.
}
