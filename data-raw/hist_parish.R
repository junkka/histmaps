library(sp)
library(rgdal)
library(maptools)
library(rgeos)
library(plyr)

# Import sources
tmpdir <- tempdir()
untar('inst/sources/nad.tar.gz', exdir=tmpdir)

x <- readOGR(dsn = file.path(tmpdir,'NAD'), layer = "NAD")
proj4string(x) <- CRS("+init=epsg:2400")

x <- spTransform(x, CRS("+proj=tmerc +lat_0=0 +lon_0=15.80827777777778 +k=1 +x_0=1500000 +y_0=0 +ellps=bessel +units=m +no_defs"))

df           <- as(x, "data.frame")
colnames(df) <- tolower(colnames(df))
df$forkod    <- floor(df$nadkod/1000)
x@data  <- df

# 'Cleaning'
# For each polygons object that have problems

#' Find holes within a object
#'
#' Find all holes within a SP object
#'
#' @param o a spatialPolygons object
#' @return vector with the ids of Polygons 
#'   with holes and ids of Polygon within
#' @export


find_threes <- function(o) {
  # extract list of polygons
  e <- c()
  a <- llply(o@polygons, function(p1){
    # For each Polygons1 object extract Polygons2 slot
    p2list  <- p1@Polygons
    # get vector of length of polygon2 coords
    for (i in 1:length(p2list)) {
      # id coords are less than 4
      if (nrow(p2list[[i]]@coords) < 4) {
        # return vector with id and i
        b <- i 
        names(b) <- p1@ID
        e <<- c(e, b)
      }
    }
  })
  return(e)
}



to_fix = find_threes(x)

for (i in 1:length(to_fix)) {
  a     <- to_fix[i]
  b     <- x[rownames(x@data) == names(a), ]
  b_p   <- slot(b, 'polygons')[[1]]
  b_p_P <- Polygons(slot(b_p, 'Polygons')[-a], 'a')
  holes <- sapply(slot(b_p_P, "Polygons"), slot, "hole")
  res   <- slot(b_p_P, "Polygons")[!holes]

  # make new SPs
  new_sps <- SpatialPolygons(
    list(Polygons(res, ID = names(a))), 
    proj4string = CRS(proj4string(x))
  ) 
  x <<- SpatialPolygonsDataFrame(
    spRbind(x[rownames(x@data) != names(a), ], new_sps), 
    x@data
  )
}

# Check sp
x <- gBuffer(x, width=0, byid=TRUE)
slot(x, "polygons") <- lapply(slot(x, "polygons"), maptools::checkPolygonsHoles)

num_to_int <- function(x) {
  # numeric to integer
  for (i in colnames(x)){
    if (inherits(x[ ,i], 'numeric'))
      x[ ,i] <- as.integer(x[ ,i])
  }
  return(x)
}  


x@data <- num_to_int(x@data)

update_encoding <- function(x) {
  Encoding(levels(x)) <- "UTF-8"
  levels(x) <- iconv(
    levels(x), 
    "UTF-8",
    "latin1" 
  )

  Encoding(levels(x)) <- "latin1"
  levels(x) <- iconv(
    levels(x), 
    "latin1", 
    "UTF-8"
  )
  return(x)
}

x@data$socken <- update_encoding(x@data$socken)
colnames(x@data)[1] <- "geomid"
hist_parish <- x
# Saving to data
save(hist_parish, file='data/hist_parish.rda',compress='xz')
