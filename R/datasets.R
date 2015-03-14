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
#' @source Based on parish borders from Riksarkivet.
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
#' @source Based on parish borders from Riksarkivet.
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
#' @source Based on parish borders from Riksarkivet.
NULL

