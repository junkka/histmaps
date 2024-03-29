% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datasets.R
\docType{data}
\name{geom_meta}
\alias{geom_meta}
\title{Meta data for Swedish administrative boundaries}
\format{
A \code{tibble} with 3676 observations of 16 variables
\describe{
  \item{geom_id}{ID of geom in geom_sp}
  \item{topo_id}{Toporaphic ID}
  \item{ref_code}{Riksarkivet code}
  \item{county}{County ID}
  \item{letter}{County ID letter}
  \item{center}{Administrative center of county}
  \item{name.x}{Name version 1}
  \item{name.y}{Name version 2}
  \item{type_id}{Type of unit, parish or county}
  \item{nadkod}{NAD code}
  \item{grkod}{Unknown code}
  \item{dedik}{Old DEDIK code}
  \item{dedikscb}{Old DEDIK code used by SCB}
  \item{forkod}{Old parish code used by Riksarkivet}
  \item{from}{Start year}
  \item{tom}{End year}
}
}
\usage{
data(geom_meta)
}
\description{
Providing information on meta data for the administrative division of Swedish counties and parishes.
}
