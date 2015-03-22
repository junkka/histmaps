#' Clip SpatialPolygonsDataFrame
#'
#' Cut a \code{SpatialPolygonsDataFrame} to a boundry box
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
  res <- rgeos::gIntersection(shp, b_poly, id = rownames(shp@data), byid = byid)
  ids <- sapply(slot(res, "polygons"), slot, "ID")
  d <- slot(shp, "data") %>% add_rownames() %>% 
    filter(rowname %in% ids)
  rownames(d) <- ids
  SpatialPolygonsDataFrame(res, data = d)
}