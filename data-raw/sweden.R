library(sp)
library(rgdal)
library(maptools)
library(rgeos)

# Import sources
tmpdir <- tempdir()
untar('inst/sources/sverige.tar.gz', exdir=tmpdir)

x <- readOGR(dsn = file.path(tmpdir,'Sverige'), layer = "sve")
proj4string(x) <- CRS("+init=epsg:2400")

x2 <- spTransform(x, CRS("+proj=tmerc +lat_0=0 +lon_0=15.80827777777778 +k=1 +x_0=1500000 +y_0=0 +ellps=bessel +units=m +no_defs"))


remove_small_holes <- function(x) {
  message('Remove holes')
  # For each polygons object in x find holes and remove
  for (i in rownames(x@data)) {
    b     <- x[rownames(x@data) == i, ]
    b_p   <- slot(b, 'polygons')[[1]]
    b_p_P <- Polygons(slot(b_p, 'Polygons'), i)
    holes <- sapply(slot(b_p_P, "Polygons"), slot, "hole")
    # check hole size
    area   <- sapply(slot(b_p_P, "Polygons"), slot, "area") < 1000000000
    holes2 <- apply(matrix(c(holes, area), nrow= 2, byrow = T), 2, all)
    res   <- slot(b_p_P, "Polygons")[!holes2]
    # make new SPs
    new_sps <- SpatialPolygons(
      list(Polygons(res, ID = i)), 
      proj4string = CRS(proj4string(x))
    ) 

    x <- SpatialPolygonsDataFrame(
      new_sps, 
      x@data
    )
  }
  return(x)
} 

sweden <- remove_small_holes(x2)

save(sweden, file = "data/sweden.rda")
