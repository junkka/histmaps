#### get_fullmapdata.R

library(sp)
library(rgdal)
library(maptools)
library(rgeos)
library(sf)
library(dplyr)
library(purrr)
library(readr)
library(tibble)
library(assertthat)

tmpdir <- tempdir()
untar('data-raw/data/histmaps_raw.tar.gz', exdir=tmpdir)

sf_x <- st_read(dsn = file.path(tmpdir,'histmaps_raw'), layer = "histmaps_raw")

x3 <- lwgeom::st_make_valid(sf_x)
sf_x2 <- st_is_valid(x3, reason = F)
all(sf_x2)

# group by unique geom

similar <- st_equals(x3, x3)

sim_long <- as_tibble(similar) %>% 
  mutate(id = create_block(row.id, col.id))

temp1 <- x3 %>% rownames_to_column() %>% 
  st_as_data_frame() %>% 
  transmute(row.id = as.integer(rowname), id1 = id, t1 = vtidstart, t2 = vtidslut, typ, namn) 

a <- temp1 %>% 
  transmute(col.id = row.id, y1 = t1, y2 = t2, typ2 = typ, name2 = namn, id2 = id1) %>% 
  left_join(sim_long, .) %>% 
  left_join(temp1)


a2 <- a %>% 
  filter(typ2 == typ, t1 <= y1, t2 >= y1, id1 != id2) %>% 
  group_by(id) %>% 
  mutate(s1 = min(y1, t1), e1 = max(y2, t2)) %>% 
  ungroup(id) %>% 
  distinct(groupid= id, id = id1, s1,e1) #%>% 
  # left_join(x3, ., by = "id")

a22 <- a %>% filter(typ2 == typ, id1 == id2) %>% 
  distinct(id = id1) %>% 
  filter(!id %in% a2$id) %>% 
  mutate(groupid = row_number() + max(a2$groupid))

a3 <- bind_rows(a2, a22) %>% 
  left_join(x3, .)

a4 <- a3 %>% group_by(groupid) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(
    vtidstart = ifelse(!is.na(s1), s1, vtidstart),
    vtidslut = ifelse(!is.na(e1), e1, vtidslut)
  ) %>% 
  select(id:vtidslut, geometry)


# x2 <- readOGR(dsn = file.path(tmpdir,'histmaps_raw'), layer = "histmaps_raw")

# t <- st_as_sf(x)

# How many unique years by type
# save(geom_sp, file = "data-raw/data/geom_sp.rda")

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
# 
# 
# find_threes <- function(o) {
#   # extract list of polygons
#   e <- c()
#   a <- plyr::llply(o@polygons, function(p1){
#     # For each Polygons1 object extract Polygons2 slot
#     p2list  <- p1@Polygons
#     # get vector of length of polygon2 coords
#     for (i in 1:length(p2list)) {
#       # id coords are less than 4
#       if (nrow(p2list[[i]]@coords) < 4) {
#         # return vector with id and i
#         b <- i 
#         names(b) <- p1@ID
#         e <<- c(e, b)
#       }
#     }
#   })
#   return(e)
# }
# 
# 
# 
# to_fix = find_threes(x)
# 
# if (!is.null(to_fix)){
#   for (i in 1:length(to_fix)) {
#     a     <- to_fix[i]
#     b     <- x[rownames(x@data) == names(a), ]
#     b_p   <- slot(b, 'polygons')[[1]]
#     b_p_P <- Polygons(slot(b_p, 'Polygons')[-a], 'a')
#     holes <- sapply(slot(b_p_P, "Polygons"), slot, "hole")
#     res   <- slot(b_p_P, "Polygons")[!holes]
# 
#     # make new SPs
#     new_sps <- SpatialPolygons(
#       list(Polygons(res, ID = names(a))), 
#       proj4string = CRS(proj4string(x))
#     ) 
#     x <<- SpatialPolygonsDataFrame(
#       spRbind(x[rownames(x@data) != names(a), ], new_sps), 
#       x@data
#     )
#   }
# }
# 
# 
# # Check sp
# library(furrr)
# future::plan(future::multicore, workers = (future::availableCores() - 1))
# options(future.globals.maxSize = 9e9)
# 
# temp <-  future_map(slot(x, "polygons"), maptools::checkPolygonsHoles)
# slot(x, "polygons") <- map(slot(x, "polygons"), maptools::checkPolygonsHoles)
# x <- gBuffer(x, width=0, byid=TRUE)

# x <- as(x3, "Spatial")
# Fix intersections
# assert_that(all(gIsValid(x, byid = T)))

# 
# geom_sp = x3
# d <- as_tibble(geom_sp)
# d$geometry <- NULL

make_id <- function(x) {
  y <- as.factor(x) 
  levels(y) <- c(1:length(levels(y))) 
  return(as.numeric(y)) 
} 
convert_enc <- function(x){
  x = as.character(x)
  y = iconv(x, "utf8", "utf8")
}

hist_maps_meta <- a4 %>% 
  mutate(
    # admin_id = make_id(topo_id),
    name = convert_enc(namn),
    type = factor(as.character(typ), 
      labels = c("Magistrates court", "Bailiwick", "Hundred", "Court", "Municipal", "Contract", "Parish", "County", "Pastorship", "Diocese", "District court")),
    type_id = factor(as.character(typ),
      labels = c("magistrate", "bailiwick", "hundred", "court", "municipal", "contract", "parish", "county", "pastorship", "diocese", "distict_court"))
  ) %>% 
  select(
    topo_id,
    geom_id = id, name, 
    type, type_id, start = vtidstart, end = vtidslut) 

raw <- read_csv("data-raw/data/Tbl_topografi.csv")
colnames(raw) <- c("id", "topo_id", "ref_code", "name", 
  "type", "start", "end")
raw <- raw %>% select(topo_id, ref_code)
hist_maps_meta <- hist_maps_meta %>% left_join(raw, by = "topo_id")

temp <- hist_maps_meta
temp$geometry <- NULL
# lookup <- select(temp, topo_id, admin_id) %>% distinct() %>% as_tibble()
# relations
rel <- read_csv("data-raw/data/Tbl_topografi_rel.csv")

relations <- rel %>% 
  rename(source = kalla_id) %>% 
  rename(dest = dest_id) %>% 
  filter(!is.na(source), !is.na(dest)) %>% 
  mutate(Ordning = ifelse(Ordning == "Efterföljare", "after", "before")) %>% 
  select(source, dest, Ordning, Start, Slut)

colnames(relations) <- c("source", "dest", "order", "start", "end")

relations <- as_tibble(relations)
save(relations, file = "data/relations.rda")

# rownames(hist_maps_meta) = hist_maps_meta$geom_id 

hist_maps_meta <- select(hist_maps_meta, geom_id, topo_id, ref_code, name:end)

# geom_sp@data = hist_maps_meta

#writeOGR(geom_sp, "PG:dbname=maps", "histmaps")
#writeOGR(geom_sp, "PG:dbname=maps", layer_options = "geometry_name=geom", "histmaps", "PostgreSQL")
# temp_dir <- tempdir()
# unlink(file.path(temp_dir, 'nad_temp'), recursive = T)
# writeOGR(geom_sp, file.path(temp_dir, 'histmaps'), "histmaps", driver="ESRI Shapefile")

# system(sprintf('shp2pgsql -I -s 2400 -W "latin1" %s/histmaps/histmaps.shp histmaps | psql -d maps', temp_dir))


# geom_sp <- st_as_sf(geom_sp)
geom_sp <- hist_maps_meta


geom_sp2 <- smoothr::fill_holes(st_collection_extract(geom_sp, "POLYGON"), 1e7)

save(geom_sp, file = "data/geom_sp.rda", compress = "xz")
