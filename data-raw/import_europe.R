#' import_europe.R

library(histmaps)
library(sp)
library(rgdal)
library(maptools)
library(rgeos)
library(dplyr)
# library(cleangeo)

# tmpdir <- tempdir()
# unzip('data-raw/data/europe_1900-2003.zip', exdir=tmpdir)
# 
# shps <- list.files(file.path(tmpdir, "01\ Europe\ Main"), pattern = ".shp$")
# basename <- stringr::str_replace(shps, ".shp", "")
# baseyear <- stringr::str_extract(basename, "[1-2][0-9]{1,3}")
# test <- "data-raw/data/e1900fixed/e1900temp.shp"
# for (i in 1:length(shps)){
#   tempfile <- file.path(tmpdir,'01\ Europe\ Main', shps[i])
#   x <- readOGR(dsn = tempfile, layer = basename[i])
#   writeOGR(x, sprintf('tempshps/e%stemp', baseyear[i]), sprintf('e%stemp', baseyear[i]), driver="ESRI Shapefile", overwrite_layer = TRUE)
# }


#' load in all data

# sp.report <- clgeo_CollectionReport(x)
# sp.summary <- clgeo_SummaryReport(sp.report)
# 
# sp.fixed <- clgeo_Clean(x, verbose = TRUE)
# sp.report <- clgeo_CollectionReport(sp.fixed)
# sp.summary <- clgeo_SummaryReport(sp.report)

#' # Design
#' 1. read in data
#' 2. combine countries
#' 3. make one file for all periods
#' 4. make boundaries 
#' 

tmp <- data.frame(nr = 1:5)
tmp$outshps <- list.dirs("data-raw/data/outshps")[-1]
tmp$nms <- sapply(tmp$outshps, function(a) list.files(a, pattern = "*.shp")) %>% as.vector()
tmp$basenms <- stringr::str_extract(tmp$nms, "^[a-z0-9]*")

shps <- plyr::llply(2, function(i){
  x <- readOGR(dsn = file.path(tmp$outshps[i], tmp$nms[i]), layer = tmp$basenms[i])
  #epsg:3035
  # crs <- CRS("+proj=tmerc +lat_0=0 +lon_0=15.80827777777778 +k=1 +x_0=1500000 +y_0=0 +ellps=bessel +units=m +no_defs")
  # orig_crs <- proj4string(x)
  
  # x <- spTransform(x, crs)
  
  #' Check sp
  x <- gBuffer(x, width=0, byid=TRUE)
  
  slot(x, "polygons") <- lapply(slot(x, "polygons"), maptools::checkPolygonsHoles)
  
  
  sum(gIsValid(x, byid=TRUE)==FALSE)
  
  
  x2 <- x
  
  #' Combine countries
  
  x3 <- unionSpatialPolygons(x2, x2$COUNTRY)
  
  d <- x2@data %>% count(COUNTRY) %>% as.data.frame() %>% mutate(layer=  tmp$basenms[i])
  order <- sapply(x3@polygons, function(b) b@ID) %>% as.integer()
  d2 <- d[match(d$COUNTRY, order), ]
  row.names(d2) <- order
  x4 <- SpatialPolygonsDataFrame(x3, data = d2)
  
  #' Remove small holes
  
  
  remove_small_holes <- function(x) {
    message('Remove holes')
    #' For each polygons object in x find holes and remove
    for (i in rownames(x@data)) {
      b     <- x[rownames(x@data) == i, ]
      b_p   <- slot(b, 'polygons')[[1]]
      b_p_P <- Polygons(slot(b_p, 'Polygons'), i)
      holes <- sapply(slot(b_p_P, "Polygons"), slot, "hole")
      #' check hole size
      area   <- sapply(slot(b_p_P, "Polygons"), slot, "area") < 7000000
      holes2 <- apply(matrix(c(holes, area), nrow= 2, byrow = T), 2, all)
      res   <- slot(b_p_P, "Polygons")[!holes2]
      #' make new SPs
      new_sps <- SpatialPolygons(
        list(Polygons(res, ID = i)), 
        proj4string = CRS(proj4string(x))
      ) 
      x <- SpatialPolygonsDataFrame(
        spRbind(x[rownames(x@data) != i, ], new_sps), 
        x@data
      )
    }
    return(x)
  } 
  
  x5 <- remove_small_holes(x4)
  x5
})

#' Create borders by checking intersecting/overlapping lines
#' one border line/s per country 
#' or one border line for all countries

#' iterate over all countries and get intersections

templ = SpatialLines(list(Lines(Line(matrix(1, 2, 2)), "1")), CRS(orig_crs))

x5 <- shps[[1]]

plyr::l_ply(x5$COUNTRY, function(b){
  ego   <- subset(x5, COUNTRY == b)
  alter <- subset(x5, COUNTRY != b)
  res <- gIntersection(ego, alter)
  
  if (is.null(res)) return(NULL)
  
  res2 <- spChFIDs(res, as.character(b))
  templ <<- spRbind(templ, res2)
}, .progress = "text")

templ2 <- templ[2:length(templ)]
d <- data.frame(COUNTRY = row.names(templ2))
rownames(d) <- row.names(templ2)
templ3 <- SpatialLinesDataFrame(templ2, d)

e1930bounds <- templ3
e1930 <- x5
save(e1930, file = "data/e1930.rda")
save(res, "data/res.rda")
# e1900bounds <- templ3
save(e1930bounds, file = "data/e1930bounds.rda")

bounds <- sp_to_ggplot(templ3)

data(sweden)
bkgr <- cut_spbox(x5, sweden, 100000)
d5 <- sp_to_ggplot(bkgr)

d6 <- d5 %>% 
  filter(COUNTRY != 20)

counti <- hist_boundaries(1900, "county", "df")

bounds2 <- bounds %>% 
  filter(COUNTRY %in% unique(d6$COUNTRY))

ggplot() + 
  geom_polygon(data = d6, aes(long, lat, group = group), fill = "#E1E1E1") + 
  geom_polygon(data = counti, aes(long, lat, group = group, fill = letter)) +
  geom_path(data = bounds2, aes(long, lat, group = group), color = "#FFFFFF", size = 1) +
  coord_equal() +
  ylim(min(d6$lat), max(d6$lat)) + 
  xlim(min(d6$long), max(d6$long)) + 
  cust_mtheme()

ggsave(file="swe_map.png", height = 14, width = 7)
