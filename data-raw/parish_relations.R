# parish_relations.R
library(dplyr)
source("data-raw/db.R")

db <- pg_db()

preceding <- db$get("
    SELECT n1.g_unit, n1.g_type, n2.g_unit as g_unit2, n2.g_end_y as year
    FROM nad2 n1 
    LEFT JOIN nad2 n2 ON 
      ST_Intersects(n1.geom, n2.geom)
      AND n1.g_start_y = (n2.g_end_y +1)
    where n1.g_start_y > 0 AND ST_area(ST_Intersection(n1.geom, n2.geom)) > 10000 AND
    n1.g_type = n2.g_type
    ")
succeeding <- db$get("
    SELECT n1.g_unit, n1.g_type, n2.g_unit as g_unit2, n2.g_start_y as year
    FROM nad2 n1 
    LEFT JOIN nad2 n2 ON 
      ST_Intersects(n1.geom, n2.geom)
    AND n1.g_end_y = (n2.g_start_y - 1)
    where n1.g_end_y < 9999 AND ST_area(ST_Intersection(n1.geom, n2.geom)) > 10000 AND
    n1.g_type = n2.g_type
  ")

preceding <- preceding %>% 
  mutate(
    g_unit = as.integer(g_unit),
    g_unit2 = as.integer(g_unit2),
    relation = "pre"
  )
succeeding <- succeeding %>% 
  mutate(
    g_unit = as.integer(g_unit),
    g_unit2 = as.integer(g_unit2),
    relation = "succ"
  )

parish_relations <- rbind(preceding, succeeding) %>% mutate(relation = as.factor(relation))
db$close()

save(parish_relations, file = "data/parish_relations.rda")
