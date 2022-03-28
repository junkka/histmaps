#' @name geom_meta
#' @title Meta data for Swedish administrative boundaries
#' @description 
#' Providing information on meta data for the administrative division of Swedish counties and parishes.
#' 
#' @docType data
#' @usage data(geom_meta)
#' @format A \code{tibble} with 3676 observations of 16 variables
#' \describe{
#'   \item{geom_id}{ID of geom in geom_sp}
#'   \item{topo_id}{Toporaphic ID}
#'   \item{ref_code}{Riksarkivet code}
#'   \item{county}{County ID}
#'   \item{letter}{County ID letter}
#'   \item{center}{Administrative center of county}
#'   \item{name.x}{Name version 1}
#'   \item{name.y}{Name version 2}
#'   \item{type_id}{Type of unit, parish or county}
#'   \item{nadkod}{NAD code}
#'   \item{grkod}{Unknown code}
#'   \item{dedik}{Old DEDIK code}
#'   \item{dedikscb}{Old DEDIK code used by SCB}
#'   \item{forkod}{Old parish code used by Riksarkivet}
#'   \item{from}{Start year}
#'   \item{tom}{End year}
#' }
NULL

#' @name eu_geom
#' @title Administrative division of the European states, 1900-2003
#' @description 
#' Providing information on the administrative division of the European states 
#' at varying geographic detail in 30-years intervals (1900, 1930, 1960, 1990, 2003). 
#' Until 1960, geocodes are based on the IDs used in the Princeton European Fertility 
#' Project. The shapefile-series is partly based on a shapefile by 
#' EuroGeographics (see citation information)
#' 
#' Projection ETRS89-extended / LAEA Europe EPSG:3035 
#' 
#' @docType data
#' @usage data(eu_geom)
#' @format A \code{sf} Simple feature collection with 198 features
#' \describe{
#'   \item{country}{Country code}
#'   \item{name}{Country name}
#'   \item{year}{Year}
#'   \item{geom}{Geometry}
#' }
#' @source  MPIDR [Max Planck Institute for Demographic Research] and CGG [Chair for
#' Geodesy and Geoinformatics, University of Rostock] 2013: MPIDR Population
#' History GIS Collection – Europe (partly based on © EuroGeographics for the
#' administrative boundaries). Rostock. 
#' The maps used in this publication are partly based on the following source: ©
#' EuroGeographics for the administrative boundaries.
NULL

#' @name eu_border
#' @title Borders of administrative division of the European states, 1900-2003
#' @description 
#' Providing information on the borders of administrative division of the European states 
#' at varying geographic detail in 30-years intervals (1900, 1930, 1960, 1990, 2003). Based on
#' \code{eu_geom} dataset.
#' 
#' Projection ETRS89-extended / LAEA Europe EPSG:3035 
#' 
#' @docType data
#' @usage data(eu_geom)
#' @format A \code{sf} Simple feature collection with 173 features
#' \describe{
#'   \item{country}{Country code}
#'   \item{name}{Country name}
#'   \item{year}{Year}
#'   \item{geom}{Geometry}
#' }
#' @source  MPIDR [Max Planck Institute for Demographic Research] and CGG [Chair for
#' Geodesy and Geoinformatics, University of Rostock] 2013: MPIDR Population
#' History GIS Collection – Europe (partly based on © EuroGeographics for the
#' administrative boundaries). Rostock. 
#' The maps used in this publication are partly based on the following source: ©
#' EuroGeographics for the administrative boundaries.
NULL


#' @name geom_sp
#' @title Administrative boundaries of Sweden, 1600-1990
#' @description 
#' Providing information on administrative geographical division of Swedish administrative units of:
#' Municipal, Pastorship, Parish, Bailiwick, Contract, Magistrates court, Hundred, District court, 
#' County, Diocese and Court.
#' 
#' Based upon historical GIS data from the Swedish National Archive. Data on parishes and counties has 
#' been verified, while the other types of units have not. Thus, be aware of possible inconsistencies and
#' faults. 
#' 
#' Projection SWEREF99 EPSG:3006
#' 
#' @docType data
#' @usage data(geom_sp)
#' @format A sf Simple feature collection with 12433 features
#' \describe{
#'   \item{geom_id}{Unique id}
#'   \item{topo_id}{Unique topographic ID}
#'   \item{ref_code}{Geocodes from the Swedish National Archive}
#'   \item{name}{Unity name}
#'   \item{type}{Name of unit type}   
#'   \item{type_id}{Unit type id}
#'   \item{start}{Start year}
#'   \item{end}{End year}
#'   \item{geometry}{Geometry}
#' }
#' @source "Historiska GIS-kartor (information om territoriella indelningar i Sverige från 
#' 1600-talets slut till 1900-talets slut)" historical GIS data from the Swedish 
#' National Archive released under Creative Commons CCZero.
NULL


#' @name geom_borders
#' @title Administrative borders of Sweden, 1600-1990
#' @description 
#' Providing supplementary information on the boundaries (without the area) administrative geographical division of Swedish administrative units of:
#' Municipal, Pastorship, Parish, Bailiwick, Contract, Magistrates court, Hundred, District court, 
#' County, Diocese and Court.
#' 
#' Based upon historical GIS data from the Swedish National Archive. Data on parishes and counties has 
#' been verified, while the other types of units have not. Thus, be aware of possible inconsistencies and
#' faults. 
#' 
#' Projection SWEREF99 EPSG:3006
#' 
#' @docType data
#' @usage data(geom_borders)
#' @format A \code{sf} Simple feature collection with 9716 features
#' \describe{
#'   \item{geom_id}{Unique id}
#'   \item{borders}{Number of borders in collection}
#'   \item{ref_code}{Geocodes from the Swedish National Archive}
#'   \item{start}{Start year}
#'   \item{end}{End year}
#'   \item{type_id}{Unit type id}
#'   \item{geometry}{Geometry}
#' }
#' @source Based upon "Historiska GIS-kartor (information om territoriella indelningar i Sverige från 
#' 1600-talets slut till 1900-talets slut)" historical GIS data from the Swedish 
#' National Archive released under Creative Commons CCZero.
NULL



#' @name geom_relations
#' @title Geographical relatives of units
#' @description Relations between units, succeeding and preceding units.
#' @docType data
#' @usage data(geom_relations)
#' @format A \code{tibble} with 19958 rows and 5 columns
#' \describe{
#'   \item{g1}{geom_id of unit 1}
#'   \item{g2}{geom_id of unit 2}
#'   \item{relation}{Type of relation "pre" or "succ"}
#'   \item{year}{Year of relationship}
#'   \item{type_id}{Unit type id}
#' }
NULL

#' @name hist_town
#' @title County towns
#' @description County towns, residensstad. Projection: SWEREF99 EPSG:3006
#' @docType data
#' @usage data(hist_town)
#' @format A \code{Simple feature} collection with 31 elements
#' \describe{
#'   \item{code}{County code}
#'   \item{town}{County town name}
#'   \item{from}{From year}
#'   \item{tom}{To year}
#' }
NULL



#' @name map_desc
#' @title Description of units
#' @description Description of units
#' @docType data
#' @usage data(map_desc)
#' @format A \code{SpatialPolygonsDataFrame} with 28 elements
#' \describe{
#'   \item{type_id}{Unit type ID}
#'   \item{units}{Number of units of that type}
#'   \item{bounds}{Number of unique years with boundary changes}
#'   \item{start}{Start year}
#'   \item{end}{End year}
#' }
NULL


