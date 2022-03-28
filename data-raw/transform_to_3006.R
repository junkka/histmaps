# transform all to sweref 99 epsg 3006

library(sf)
library(tidyverse)


load("data/geom_sp.rda")

geom_sp <- st_transform(geom_sp, crs = 3006)

save(geom_sp, file = "data/geom_sp.rda", compress = "xz")


load("data/geom_borders.rda")

geom_borders <- st_transform(geom_borders, crs = 3006)

save(geom_borders, file = "data/geom_borders.rda", compress = "xz")


load("data/hist_town.rda")

hist_town <- st_transform(hist_town, crs = 3006)

save(hist_town, file = "data/hist_town.rda", compress = "xz")


