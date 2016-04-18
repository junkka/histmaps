#' @name hist_county
#' @title Historical county boundaries
#' @description Swedish county borders 1600-1990.
#' @docType data
#' @usage data(hist_county)
#' @format A \code{SpatialPolygonsDataFrame} with 101 elements
#' \describe{
#'   \item{lan}{County code}
#'   \item{from}{Starting year}
#'   \item{tom}{End year}
#'   \item{name}{County name}
#'   \item{letter}{County letter code}
#'   \item{center}{County center}
#'   \item{geomid}{Unique geomid}
#'   \item{area}{Area in square meters}
#' }
#' @source NAD Topography data from Riksarkivet \url{http://riksarkivet.se/psidata}.
NULL

#' @name hist_parish
#' @title Historical parish boundaries
#' @description Swedish parish borders 1600-1990.
#' @docType data
#' @usage data(hist_parish)
#' @format A \code{SpatialPolygonsDataFrame} with 101 elements
#' \describe{
#'   \item{geomid}{Geomid}
#'   \item{nadkod}{Unique parish code}
#'   \item{grkod}{A code}
#'   \item{socken}{Parish name}
#'   \item{from}{Starting year}
#'   \item{tom}{End year}
#'   \item{area}{Area in km2}
#'   \item{dedikscb}{Dediksvb code}
#'   \item{dedik}{Dedik code}
#'   \item{forkod}{Parish code}
#' }
#' @source NAD Topography data from Riksarkivet \url{http://riksarkivet.se/psidata}.
NULL


#' @name par_to_county
#' @title Parish to county 
#' @description Parish to county lookuptable.
#' @docType data
#' @usage data(par_to_county)
#' @format A \code{data.frame} with 2563 rows and 4 columns
#' \describe{
#'   \item{nadkod}{Parish code}
#'   \item{from}{Start year}
#'   \item{tom}{End year}
#'   \item{county}{County code}
#' }
#' @source NAD Topography data from Riksarkivet \url{http://riksarkivet.se/psidata}.
NULL

#' @name parish_relations
#' @title Parish geographical relatives
#' @description Parish relations, succeeding and preceding parish.
#' @docType data
#' @usage data(parish_relations)
#' @format A \code{data.frame} with 2563 rows and 4 columns
#' \describe{
#'   \item{nadkod}{Parish code}
#'   \item{nadkod2}{Parish relative}
#'   \item{year}{Year of change}
#'   \item{relation}{Type of relation "pre" or "succ"}
#' }
NULL

#' @name hist_town
#' @title County towns
#' @description County towns, residensstad
#' @docType data
#' @usage data(hist_town)
#' @format A \code{SpatialPolygonsDataFrame} with 28 elements
#' \describe{
#'   \item{code}{County code}
#'   \item{town}{County town name}
#'   \item{from}{From year}
#'   \item{tom}{To year}
#' }
NULL



#' @name sweden
#' @title Sweden
#' @description Sweden national boundaries
#' @docType data
#' @usage data(sweden)
#' @format A \code{SpatialPolygonsDataFrame} with 28 elements
#' \describe{
#'   \item{SVE_ID}{id}
#' }
NULL


