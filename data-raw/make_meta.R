library(sf)
library(tidyverse)


load("../histmaps/data/par_to_county.rda")

par_to_county <- par_to_county %>% as_tibble() 

load("../histmaps/data/hist_parish.rda")
load("../histmaps/data/hist_county.rda")

meta <- hist_parish@data %>% as_tibble()
meta2 <- hist_county@data %>% as_tibble()

meta_parish <- meta %>% 
  mutate(ref_code = sprintf("SE/%09d", nadkod)) %>% 
  select(geomid, ref_code, nadkod, socken, grkod, dedik, dedikscb, forkod, start = from, end = tom)


overlapps <- function(x1,y1,x2,y2){
  (pmin(x1, y1) <= pmax(x2, y2)) &
    (pmax(x1, y1) >= pmin(x2, y2))
}

meta_parish <- meta_parish %>% left_join(par_to_county) %>% 
  filter(overlapps(start, end, from, tom)) %>% 
  mutate(id = row_number())


meta2 %>% 
  mutate(
    ref_code = sprintf("SE/%09d", lan*1e7)
  )

res <- hist_county %>% st_as_sf()

load("../histmaps/data/geom_sp.rda")

res <- st_set_crs(res, st_crs(geom_sp)) %>% mutate(
  ref_code = sprintf("SE/%09d", lan*1e7),
  ref_code = ifelse(lan == 27, "SE/180000004", ref_code)
)


res3 <- geom_sp %>% filter(type_id == "county") %>% histmaps::st_as_data_frame()



res3 <- res3 %>% distinct(topo_id, ref_code, name, type, type_id) %>% 
  mutate(ref_code = ifelse(str_detect(name, "Dalar"), "SE/200000000", ref_code))


res4 <- left_join(res, res3, by = c("ref_code"))

res4 <- res4 %>% 
  mutate(
    geom_id =  (max(geom_sp$geom_id):(max(geom_sp$geom_id)+nrow(res4)-1)+1)
  ) 
res5 <- res4 %>% 
  select(
    geom_id, topo_id, ref_code, name = name.x, type, type_id, start =from, end = tom
  ) %>% 
  mutate(
    type_id = "county",
    type    = "County"
  )

meta2 <- res4 %>% select(geom_id, topo_id, ref_code, lan, letter, center, name.x, name.y) %>% 
  histmaps::st_as_data_frame() %>% mutate(type_id = "county")

geom_sp <- geom_sp %>% filter(type_id != "county") %>% 
  rbind(res5)

# geom_sp <- st_transform(geom_sp, 3006)

save(geom_sp, file= "data/geom_sp.rda", compress = "xz")

# post match through intersection



meta_parish <- st_as_sf(hist_parish) %>% 
  mutate(ref_code = sprintf("SE/%09d", nadkod)) %>% 
  select(geomid, ref_code, nadkod, socken, grkod, dedik, dedikscb, forkod, start = from, end = tom)


overlapps <- function(x1,y1,x2,y2){
  (pmin(x1, y1) <= pmax(x2, y2)) &
    (pmax(x1, y1) >= pmin(x2, y2))
}

meta_parish <- meta_parish %>% left_join(par_to_county) %>% 
  filter(overlapps(start, end, from, tom)) %>% 
  mutate(id = row_number())


par_d <- geom_sp %>% filter(type_id == "parish")

meta_parish <- st_set_crs(meta_parish, st_crs(par_d))

d0 <- st_intersection(meta_parish, par_d)

d1 <- d0 %>% mutate(area = as.numeric(st_area(geometry)))

d2 <- d1 %>% 
  filter(overlapps(start.1, end.1, from, tom)) %>% 
  group_by(id) %>% 
  filter(area == max(area)) %>% 
  ungroup()

# d2

no_meta <- par_d %>% filter(!geom_id %in% unique(d2$geom_id)) %>% histmaps::st_as_data_frame()

manual_meta <- read_csv("data-raw/meta-manual.csv") %>% select(-name)

d3 <- d2 %>% 
  select(
    geom_id, id, start = from, end = tom 
  ) %>% histmaps::st_as_data_frame() %>% 
  bind_rows(manual_meta)

d4 <- d3 %>% 
  left_join(par_d %>% select(-start, -end), .)

d5 <- d4 %>% 
  mutate(
    geom_id =  (max(geom_sp$geom_id):(max(geom_sp$geom_id)+nrow(d4)-1)+1)
  ) 

meta_p <- meta_parish %>% histmaps::st_as_data_frame() %>% 
  left_join(d5 %>% histmaps::st_as_data_frame() %>% select(geom_id, topo_id, name, id, type_id)) %>% 
  select(geom_id, type_id ,topo_id,  ref_code, name.x = name, name.y = socken, nadkod, grkod:forkod, county, from, tom)

geom_p <- d5 %>% select(-id)

geom_sp2 <- rbind(geom_sp %>% filter(type_id != "parish"), geom_p) %>% 
  select(-topo_id)

geom_sp <- geom_sp2 

all(geom_sp$geom_id[geom_sp$type_id == "parish"] %in% meta_p$geom_id[meta_p$type_id == "parish"])

all(meta_p$geom_id[meta_p$type_id == "parish"] %in% geom_sp$geom_id[geom_sp$type_id == "parish"])


geom_sp <- st_transform(geom_sp, crs = 3006)

save(geom_sp, file = "data/geom_sp.rda", compress = "xz")

geom_meta <- meta2 %>% rename(county = lan) %>% 
  bind_rows(meta_p)

save(geom_meta, file = "data/geom_meta.rda", compress = "xz")
