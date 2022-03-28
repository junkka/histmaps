
#' Create borders by checking intersecting/overlapping lines
#' one border line/s per country 
#' or one border line for all countries

#' iterate over all countries and get intersections


library(sf)
library(histmaps)
library(tidyverse)


data(geom_sp, package = "histmaps")
data(sweden, package = "histmaps")


load("data/e1900.rda")

ref_codes_country <- c(200, 130) # swe 20

no_fin <- st_as_sf(e1900) %>%
  filter(COUNTRY %in% ref_codes_country)

no_fin <- st_transform(no_fin, crs = st_crs(geom_sp)) %>%
  rename(
    geom_id = COUNTRY
  )


dat0 <- geom_sp %>% filter(type_id == "parish")

# dat01 <- bind_rows(dat0, no_fin)

swe <- st_as_sf(sweden) %>% 
  st_transform(crs= st_crs(geom_sp))

swe2 <- swe <- dat0 %>% summarise(
  n = n(),
  geom_id = 1
)

load("data/e1900bounds.rda")  

swe <- st_as_sf(e1900bounds) %>% filter(COUNTRY == 20) %>% 
  mutate(
    geom_id = 1
  ) %>% 
  st_transform(crs = st_crs(geom_sp)) %>% 
  st_snap(swe2, tolerance = 800)


ggplot() +
  geom_sf(data = swe2, color = NA) + 
  geom_sf(data = swe) + 
  ggthemes::theme_map()



dat1 <- get_boundaries(1900, "parish")


dat2 <- dat1 %>%
  filter(str_detect(ref_code, "SE/24"))

ggplot(dat2) +
  geom_sf(alpha = .5)

# for unit, get all other boundaries and extract

library(parallel)

f <- function(dat_x, dat_o, swe){
  library(sf)
  library(tidyverse)
  dat_y <- dat_o %>% filter(start <= dat_x$start, end >= dat_x$start, type_id == dat_x$type_id) %>% 
    bind_rows(swe)
  
  # if (length(dat_y))
  
  inters <- st_intersection(dat_x, dat_y) %>% 
    filter(geom_id.1 != geom_id)
  
  inters %>% 
    group_by(geom_id) %>% 
    summarise(borders = n(), start = first(start), end = first(end), type_id = first(type_id),  .groups = 'drop') 
  
}


x <- st_intersection(st_boundary(swe2), st_buffer(no_fin, 1500)) %>% 
  filter(geom_id != geom_id.1) %>% 
  group_by(geom_id) %>% summarise(n = n()) %>% 
  mutate(geom_id = 0)


dat00 <-geom_sp %>% 
  filter(type_id %in% c("parish", "municipal", "county", "pastorship")) %>% 
  group_by(geom_id) %>% 
  split(group_indices(.))

# dat_samp <- dat00 %>% sample(100)

res <- mclapply(dat00, f, mc.cores = 11, dat_o = geom_sp, swe = x)
# res <- lapply(dat00, f, dat_o = dat_o, swe = swe)

geom_borders <- bind_rows(res)
# # geom_borders
# 
# # geom_borders %>% filter(start <= 1800, end >= 1800) %>%
#   ggplot() + 
#   geom_sf(data =geom_sp %>% filter(start <= 1900, end >= 1900, type_id == "municipal") , color=NA, alpha = .5, fill = "blue") + 
#   # geom_sf(alpha = .2) +
#   ggthemes::theme_map()

save(geom_borders, file = "data/geom_borders.rda")

# dat_x <- dat0 %>% sample_n(1)



# inters <- st_intersection(dat01, dat01) %>% 
#   filter(geom_id.1 != geom_id)

# all unique combinations of x and y 



coord_cut <- function(x){
  bb <- st_bbox(x)
  coord_sf(ylim = bb[c(2,4)],xlim = bb[c(1,3)])
}

cnty <- get_boundaries(1900, "county")

# intersection of sweden boundaries with buffered norr_fin area

load("data/geom_borders.rda")

dat2 <- geom_sp %>%
  filter(str_detect(ref_code, "SE/24"), type_id == "parish", start <= 1980, end >= 1980)


ggplot() +
  geom_sf(data= no_fin, color = NA, fill = "gray80") +
  geom_sf(data = swe2, color = NA, fill = "gray80") + 
  # geom_sf(data = cnty, fill =NA)+
  geom_sf(data = dat2, aes(fill = geom_id),color = NA)  +
  geom_sf(data = geom_borders %>% filter(geom_id %in% dat2$geom_id), size = .2) +
  geom_sf(data = geom_borders %>% filter(type_id == "county", start <= 1980, end >= 1980)) +
  # ggrepel::geom_label_repel(data = dat2, aes(label = str_replace(name, " kommun", ""), geometry = geometry ),  stat = "sf_coordinates")  + 
  # geom_sf(data = no_fin, fill = NA) 
  ggthemes::scale_fill_gradient_tableau(palette = "Orange") + 
  coord_cut(dat2)+
  ggthemes::theme_map() +
  theme(panel.background = element_rect(fill = "#7fcdff", color = NA))

# templ = SpatialLines(list(Lines(Line(matrix(1, 2, 2)), "1")), crs)


ggplot() + 
  geom_sf(data = swe2, color = NA) + 
  geom_sf(data = res2 %>% filter(start <= 1800, end >= 1800), color = "gray70", size = .05) +
  ggthemes::theme_map() +
  theme(panel.background = element_rect(fill = "#7fcdff", color = NA))


no_b <- dat0 %>% filter(!geom_id %in% res2$geom_id)

ggplot() +
geom_sf(data = swe2, color = NA, fill = "gray80")+ 
  geom_sf(data= no_b, color = "red")


load("data/geom_borders_county.rda")
load("data/geom_borders_municipal.rda")
load("data/geom_borders_par.rda")
load("data/geom_borders_pastorship.rda")

geom_borders <- bind_rows(list(
    geom_borders_county %>% mutate(type_id = "county"),
    geom_borders_municipal %>% mutate(type_id = "municipal"),
    geom_borders_par %>% mutate(type_id = "parish"),
    geom_borders_pastorship %>% mutate(type_id = "pastorship")
  ))


save(geom_borders, file = "data/geom_borders.rda", compress = "xz")


