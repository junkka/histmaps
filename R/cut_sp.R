#' Cut SpatialPolygonsDataFrame
#'
#' Cut a spdf to a bondry box
#'
#' @param shp SpatialPolygonsDataFrame
#' @param bb Boundry box
#' @param byid boolean
#' @export

clip_spdf <- function(shp, bb, byid = T){
  if(class(bb) == "matrix") 
    b_poly <- as(raster::extent(as.vector(t(bb))), "SpatialPolygons")
  else 
    b_poly <- as(raster::extent(bb), "SpatialPolygons")
  sp::proj4string(b_poly) <- sp::proj4string(shp)
  rgeos::gIntersection(shp, b_poly, byid = byid)
}