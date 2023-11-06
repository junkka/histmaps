
library(localpostgres)
library(tidyverse)
library(sf)

db <- pgrs_connect("rstudiojunkka")


preceding <- pgrs_query(db, 
"
    SELECT n1.geom_id, n2.geom_id as geom_id_2, n2.stop as year,
      n1.type_id
    FROM geom_sp n1 
    LEFT JOIN geom_sp n2 ON 
      ST_Intersects(n1.geometry, n2.geometry)
      AND n1.start = n2.stop +1
    WHERE n1.start > 0 
      AND ST_area(ST_Intersection(n1.geometry, n2.geometry)) > 10000
      AND n1.type_id = n2.type_id
    ")


succeeding <- pgrs_query(db, 
                        "
    SELECT n1.geom_id, n2.geom_id as geom_id_2, n2.stop as year, 
    n1.type_id
    FROM geom_sp n1 
    LEFT JOIN geom_sp n2 ON 
      ST_Intersects(n1.geometry, n2.geometry)
      AND n1.stop = n2.start -1
    WHERE n1.stop < 9999 
      AND ST_area(ST_Intersection(n1.geometry, n2.geometry)) > 10000
      AND n1.type_id = n2.type_id
    ")

preceding2 <- preceding %>% 
  transmute(
    g1 = as.integer(geom_id),
    g2 = as.integer(geom_id_2),
    relation = "pre",
    year,
    type_id
  )
succeeding2 <- succeeding %>% 
  transmute(
    g1 = as.integer(geom_id),
    g2 = as.integer(geom_id_2),
    relation = "succ",
    year,
    type_id
  )

geom_relations <- rbind(preceding2, succeeding2) %>% mutate(relation = as.factor(relation)) 

save(geom_relations, file="data/geom_relations.rda")

# source("../geocodeortnamn/R/st_tbl.R")
# sweb <- st_tbl(db, "sweboundaries")
