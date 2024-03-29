% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_boundaries.R
\name{get_boundaries}
\alias{get_boundaries}
\title{Retrieve Administrative Boundaries}
\usage{
get_boundaries(
  date,
  type = c("magistrate", "bailiwick", "hundred", "court", "municipal", "contract",
    "parish", "county", "pastorship", "diocese", "district_couryt"),
  format = c("sf", "meta"),
  boundary_type = c("polygons", "borders")
)
}
\arguments{
\item{date}{A date for which the boundaries should be retrieved.}

\item{type}{A character vector indicating the type of administrative unit (default options are available). Possible values include: "magistrate", "bailiwick", "hundred", "court", "municipal", "contract", "parish", "county", "pastorship", "diocese", and "district_couryt".}

\item{format}{A character indicating the format of the returned object (default is "sf").}

\item{boundary_type}{A character string specifying the type of boundaries to return. Choose between "polygons" (default) for polygon shapes of the administrative units, and "borders" for the multiline border representation of the units.}
}
\description{
This function obtains administrative boundaries for a specific date and type of unit, allowing users to specify the format of the returned object.
}
\examples{

county_map <- get_boundaries(1800, "county")

plot(st_geometry(county_map))

}
