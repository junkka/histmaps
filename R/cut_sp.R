#' Clip SpatialPolygonsDataFrame
#'
#' Cut a \code{SpatialPolygonsDataFrame} to a boundry box
#'
#' @param shp SpatialPolygonsDataFrame
#' @param bb Boundry box
#' @param byid boolean
#' @export

clip_spdf <- function(shp, bb, byid = T){
  if (all(c("raster", "rgeos") %in% rownames(installed.packages())) == FALSE)
    stop("Requires raster and rgeos packages")
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
  SpatialPolygonsDataFrame(res, data = as.data.frame(d))
}

#' Cut SpatialPolygonsDataFrame
#'
#' Cut a sp object to the boundary box of another sp object
#'
#' @param big SpatialPolygonsDataFrame to be cut
#' @param small SpatialPolygonsDataFrame which to get bbox of
#' @param dist Added distanse of bbox
#' @param ... optional arguments passed tp \code{clip_spdf}
#' @export

cut_spbox <- function(big, small, dist, ...) {
  bb <- sp::bbox(small)
  bb[ ,1] <- bb[ ,1] - dist
  bb[ ,2] <- bb[ ,2] + dist
  clip_spdf(big, bb, ...)
}