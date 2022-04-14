# europe-sf.R

library(sf)
library(tidyverse)

# library(furrr)

# plan(multicore, workers = (future::availableCores() - 1))
# options(future.globals.maxSize = 9e9)

library(parallel)

# load all data

folder <- "data-raw/data/outshps/"
shps_names <- list.files(folder)
# 
# base_nm <- str_replace(shps_names[1], "fixed", "")
# yearn   <- str_extract(base_nm, "[0-9]{4}")
# indata1 <- st_read(paste0(folder, "/", base_nm, "fixed/", base_nm, "temp.shp"), paste0(base_nm, "temp")) %>% 
#   mutate(year = as.integer(yearn))


f <- function(x){
  base_nm <- str_replace(x, "fixed", "")
  yearn   <- str_extract(base_nm, "[0-9]{4}")
  indata2 <- st_read(paste0(folder, "/", base_nm, "fixed/", base_nm, "temp.shp"), paste0(base_nm, "temp")) %>% 
    mutate(year = as.integer(yearn))
  st_crs(indata2) <- 3035
  indata2
}

res <- map(shps_names, f)
# 
# f2 <- function(d){
#   temp_f <- function(x){
#     dd <- d %>% 
#       filter(COUNTRY == x)
#     union_res <- st_union(dd)
#     tibble(country = as.character(x), year = dd$year[1], geom = union_res)
#   }
#   cntr <- unique(d$COUNTRY)
#   st_sf(future_map_dfr(cntr, temp_f),crs = st_crs(d))
# }
# 
# res2 <- map(res, f2)
# 
# res3 <- rbind(res2[[1]], res2[[2]])
# for(i in 3:length(res3)){
#   res3 <<- rbind(res3, res2[[i]])
# }
# 
# meta <- read_csv("data-raw/data/eu-meta.csv") %>% mutate(country = as.character(country))
# 
# eu_country <- left_join(res3, meta)
# 
# save(eu_country, file = "temp.rda")
# 
# library(tmap)
# tmm <- tm_shape(eu_country %>% filter(year == 1900) %>%  select(name, country, year)) +
#   tm_polygons()
# 
# 
# tmap_leaflet(tmm)
# 
# eu1900 <- eu_country %>% filter(year == 1900) %>% as("Spatial")
# library("rgeos")
# eu_simp <-gSimplify(eu1900, tol = 1000, topologyPreserve = FALSE)
# 
# eu_snapped <- st_snap(st_as_sf(eu_simp), st_as_sf(eu_simp), 2000)
# 
# tm_shape(eu_snapped) + tm_polygons(border.col = NULL)
# tmap_leaflet(tmap_last())
# meta <- read_csv("data-raw/data/eu-meta.csv") %>% filter(YEAR == 1900)
# eu_country <- eu_country[1:n_count, ]
# # plot(st_geometry(eu_country)) 
# 
# eu_c2 <- left_join(eu_country, meta)
# 
# scan <- eu_c2 %>% filter(COUNTRY %in% c(190,60, 130))
# plot(st_geometry(scan))
# 
# inters <- st_intersection(eu_c2, eu_c2 %>% select(c2 = COUNTRY))
# 
# inters <- inters %>% mutate(
#   is_string = st_is(geometry, "MULTILINESTRING")
# ) %>% 
#   filter(is_string)
# plot(st_geometry(inters %>% filter(is_string)))
# 
# 
# 
# library(tmap)
# 
# tmm <- tm_shape(eu_c2) +
#   tm_polygons("NAME")
# 
# tmap_leaflet(tmm)
# 
# 
# 
# 
# library(histmaps)
# map_d <- hist_boundaries(1880, "parish", "sf")
# 
# parish_coded <- read_csv("../disability-migration/data/parish_meta.csv") %>% select(dedik, type)
# 
# sund_map <- map_d %>% filter(dedik %in% unique(parish_coded$dedik))
# 
# par_map <- hist_boundaries(1880, "county", "sf")
# 
# su_coded <- left_join(sund_map, parish_coded)
# 
# crs <- st_crs(indata)
# 
# bb <- st_bbox(sund_map)
# # bb <- bb + c(0,0,20000, 0)
# 
# sund_over <- st_transform(sund_map, crs)
# bb <- st_bbox(sund_over)
# bb <- bb + c(-10000, -10000, 10000, 10000)
# load("data/e1900bounds.rda")
# 
# borders <- st_as_sf(e1900bounds)
# 
# 
# ggplot(eu_c2) +
#   geom_sf(color = NA) +
#   geom_sf(data = inters, color = "gray80") +
#   # geom_sf(data = sund_over, fill = "gray70") + 
#   # geom_rect(aes(xmin = bb[1], ymin = bb[2], xmax = bb[3], ymax = bb[4]), fill = NA, color = "red") +
#   theme(panel.background = element_rect(fill = "#87ceeb"))
# 
# library(tmap)
# # tmap_leaflet(p)
# 
# 
# tmm <- tm_shape(eu_c2) +
#   tm_polygons() +
#   tm_shape(inters) + 
#   tm_lines()
# 
# tmap_leaflet(tmm)
# 

library(leaflet)
library(leafletwrapper)


xs <- do.call(rbind, res)

xs2 <- xs %>% 
  mutate(
    COUNTRY = case_when(
      ID == 50020 ~ "50",
      ID == 370010 ~ "50",
      COMMENTS == "Osman Empire" ~ "OSM",
      ID == 370020 ~"GUERNSEY",
      NAME == "MALTA" ~"MALTA",
      NAME == "GIBRALTAR" ~"GIBRALTAR",
      ID == 310000 ~ "LICHTEN",
      ID == 350000 ~ "ANDORRA",
      ID == 320000 ~ "SAN MARINO",
      NAME == "EUROPEAN TURKEY" ~"OSM",
      NAME == "KOMI-PERMJAKEN"~"160", #SOVJET
      TRUE ~ COUNTRY
    )
  )


xs3 <- xs2 %>% group_by(COUNTRY, year) %>% summarise(n = n(), .groups = "drop")

leaf_init(crs = 3035) %>% leaf_polygon(xs2 %>% filter(year == 1960) , interactive = T)


xs4 <- st_sf(st_as_data_frame(xs3) %>% mutate(geometry = xs3$geometry), crs = st_crs(xs3))

xs5 <- st_simplify(xs4, preserveTopology = T, dTolerance = 1000)

# leaf_init(crs = 3035) %>% leaf_polygon(xs4 %>% filter(year == 1960) , interactive = T)


xs6 <- xs5 %>% group_by(year) %>% nest() %>% 
  mutate(
    data = mclapply(data, ~st_snap(., ., 1500), mc.cores = 11)
  )

# xs6 <- st_snap(xs5 %>% filter(year == 1960), xs5 %>% filter(year == 1960), 1000)

xs61 <- xs6 %>% unnest(data) %>% ungroup()

xs62 <- st_sf(st_as_data_frame(xs61) %>% mutate(geometry = xs61$geometry), crs = st_crs(xs3))

# leaf_init(crs = 3035) %>% leaf_polygon(xs6, interactive = T)

meta <- read_csv("data-raw/data/eu-meta.csv") %>% mutate(country = as.character(country))

xs7 <- xs62 %>% left_join(meta, by = c("year" = "year", "COUNTRY"="country")) %>% 
  mutate(ind = is.na(name))



leaf_init(crs = 3035) %>% leaf_polygon(xs7 %>% filter(year == 1900), interactive = T, colorby = "ind", lbl = "name")


xs8 <- st_as_data_frame(xs7)

# install.packages("countrycode")

codes <- xs8 %>% filter(year > 1960, is.na(name)) %>% pluck("COUNTRY")

library(countrycode)

xs8$name[xs8$year > 1960 & is.na(xs8$name)] <- countrycode(codes, "iso2c", "country.name")



xs81 <- st_sf(xs8 %>% mutate(geometry = xs7$geometry, ind = is.na(name)), crs = st_crs(xs3))

leaf_init(crs = 3035) %>% leaf_polygon(xs81 %>% filter(year == 2003), interactive = T, lbl = "COUNTRY")
 
xs9 <- nngeo::st_remove_holes(xs81, 100000)


# leaf_init(crs = 3035) %>% leaf_polygon(xs9 %>% filter(year == 1930), interactive = T, lbl = "name")

# make eu_borders
library(lwgeom)
t_0 <- xs9 %>% filter(year == 1930) %>% st_make_valid() %>% st_cast("MULTIPOLYGON")

f2 <- function(x) st_intersection(x,x) %>% filter(COUNTRY != COUNTRY.1)

f4 <- function(x,y) cbind(x,y)

xs10 <- xs9 %>% 
  group_by(year) %>% 
  nest() %>% 
  mutate(
    data = mclapply(data, function(x){st_make_valid(x) %>% st_cast("MULTIPOLYGON")}, mc.cores = 11),
    data2 = mclapply(data, f2, mc.cores = 11),
    data2 = map2(data2, year, f4),
    data = map2(data, year, f4)
  )


xp11 <- do.call(rbind, xs10$data)

xs11 <- do.call(rbind, xs10$data2)

# Drop points from geom collections

xs11a <- st_collection_extract(xs11, "LINESTRING")
xs11b <- st_collection_extract(xs11, "POLYGON")

xs11c <- rbind(xs11a, xs11b)

xs12 <- xs11c %>% group_by(COUNTRY, name, y) %>% 
  summarise(ns = n(), .groups = "drop")




ggplot() + 
  geom_sf(data = xp11 %>% filter(y == 1960), color = NA) +
  geom_sf(data = xs12 %>% filter(y == 1960), color = "gray70") + 
  theme_void()

# inters <- st_intersection(t_0,t_0) %>% filter(COUNTRY != COUNTRY.1)


eu_geom <- st_sf(st_as_data_frame(xp11 )%>% select(country = COUNTRY, name, year = y) %>% 
                   mutate(geometry = xp11$geom), crs = st_crs(xs3))

eu_border <- st_sf(st_as_data_frame(xs12 )%>% select(country = COUNTRY, name, year = y) %>% 
                   mutate(geometry = xs12$geom), crs = st_crs(xs3))


save(eu_geom, file = "data/eu_geom.rda", compress = "xz")
save(eu_border, file = "data/eu_border.rda", compress = "xz")

# leaf_init(crs = 3035) %>% leaf_polygon(xs12 %>% filter(y == 1930), interactive = T, lbl = "name")
# 
# inters <- st_intersection(dat_x, dat_y) %>% 
#   filter(geom_id.1 != geom_id)
# 
# inters %>% 
#   group_by(geom_id) %>% 
#   summarise(borders = n(), start = first(start), end = first(end), type_id = first(type_id),  .groups = 'drop') 