#' Check overlapping geometries
#'
#' For each unit type and unique end year check overlapps

library(sf)
library(tidyverse)

load("data/geom_sp.rda")


types <- geom_sp$type_id %>% unique()


get_un_dates <- function(d, typeid){
  d %>% filter(type_id == typeid, end < 9999) %>% 
    pluck("end") %>% 
    unique() %>% sort()
}

# for each year check intersections

h_years <- get_un_dates(geom_sp, "hundred")

test <- geom_sp %>% filter(start <= 1948, end >= 1948, type_id == "hundred")

over_test <- st_overlaps(test, test)
