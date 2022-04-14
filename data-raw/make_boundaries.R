
#' Create borders by checking intersecting/overlapping lines
#' one border line/s per country 
#' or one border line for all countries

#' iterate over all countries and get intersections


library(sf)
library(histmaps)
library(tidyverse)

load("data/geom_sp.rda")

ref_codes_country <- c(60, 130) 

no_fin <- st_as_sf(eu_geom) %>%
  filter(country %in% ref_codes_country, year == 1900)

no_fin <- st_transform(no_fin, crs = st_crs(geom_sp)) %>%
  rename(
    geom_id = country
  )





folder <- "data-raw/data/outshps/"
shps_names <- list.files(folder)


f <- function(x){
  base_nm <- str_replace(x, "fixed", "")
  yearn   <- str_extract(base_nm, "[0-9]{4}")
  indata2 <- st_read(paste0(folder, "/", base_nm, "fixed/", base_nm, "temp.shp"), paste0(base_nm, "temp")) %>% 
    mutate(year = as.integer(yearn))
  st_crs(indata2) <- 3035
  indata2
}

res <- map(shps_names, f)


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
      NAME == "KOMI-PERMJAKEN"~"160", 
      TRUE ~ COUNTRY
    )
  )


xs3 <- xs2 %>% group_by(COUNTRY, year) %>% summarise(n = n(), .groups = "drop")

e1900 <- xs3 %>% filter(year == 1900)
rm(list = c("xs3", "xs2", "xs", "res"))
bordering_c <- e1900 %>% filter(COUNTRY %in% c(60, 130)) %>% st_transform(st_crs(geom_sp))

swe2 <- geom_sp %>% filter(type_id == "parish", start <= 1900, end >= 1900) %>% 
  summarise(geom_id = 1) 


library(parallel)

f <- function(dat_x, dat_o, swe){

  
  dat_y <- dat_o %>% filter(start <= dat_x$start, end >= dat_x$start, type_id == dat_x$type_id) %>% 
    mutate(geom_id = as.character(geom_id)) %>% 
    bind_rows(swe %>% mutate(geom_id = "0"))
  
  inters <- st_intersection(dat_x, dat_y) %>% 
    filter(geom_id.1 != geom_id)
  
  
  res02 <- tryCatch({st_collection_extract(inters, "LINESTRING")}, error = function(e){})
  res03 <- tryCatch({st_collection_extract(inters, "POLYGON")}, error = function(e){})
  
  res04 <- rbind(res03, res02)
  
  res05 <- res04 %>% 
    group_by(geom_id) %>% 
    summarise(borders = n(), start = first(start), end = first(end), type_id = first(type_id),  .groups = 'drop') 
  
  res05
}

no_fin2 <- bordering_c %>% st_snap(swe2, tolerance = 500)%>% st_buffer(100)


dat00 <-geom_sp %>%
  filter(type_id %in% c("parish", "municipal", "county", "pastorship")) %>% 
  st_boundary() %>% 
  group_by(geom_id) %>% 
  split(group_indices(.))


res <- mclapply(dat00, f, dat_o = geom_sp, swe = no_fin2, mc.cores = 11)

res01 <- do.call(rbind, res)


res02 <- st_collection_extract(res01, "LINESTRING")
res03 <- st_collection_extract(res01, "POLYGON")

res04 <- rbind(res03, res02)


res05 <- res04 %>% group_by(geom_id) %>% 
  summarise(
    start = first(start), end = first(end), type_id = first(type_id), .groups = "drop"
  )


res01 %>% 
  filter(start <= 1900, end >= 1900) %>% 
  st_geometry() %>% 
  plot()



geom_borders <- res01

save(geom_borders, file = "data/geom_borders.rda", compress = "xz")
