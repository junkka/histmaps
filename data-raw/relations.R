# relations.R

library(sp)

# load("data/relations.rda")
# load("data/geom_sp.rda")

# d <- tbl_df(slot(geom_sp, "data"))

# rel <- tbl_df(relations)

# rel1 <- d %>% select(source = admin_id, s_name = name, s_type = type_id) %>% 
#   distinct() %>% 
#   left_join(rel) %>% select(s_name,s_type, dest, order)

# rel2 <- d %>% select(dest = admin_id, d_name = name, d_type = type_id) %>% 
#   distinct() %>% 
#   left_join(rel1) %>% select(-dest)

# # Connections between types
# rel2 %>% filter(!is.na(s_type)) %>% count(s_type, d_type, order)

# parish_relations.R
library(dplyr)
library(RPostgreSQL)

#load("data/geom_sp.rda")
#writeOGR(geom_sp, 'hist_county', "hist_county", driver="ESRI Shapefile")

source("db_config.R")

pg_db <- function(db_cnfg = db_config) {
  drv <- DBI::dbDriver("PostgreSQL")
  con <- DBI::dbConnect(drv, 
      dbname = db_cnfg$dbname,
      host = db_cnfg$host,
      port = db_cnfg$port,
      user = db_cnfg$user,
      password = db_cnfg$password
    )
  return(list(
    get_con = function(){
      return(con)
    },
    close = function(){
      DBI::dbDisconnect(con)  
    },
    get = function(query){
      res <- DBI::dbGetQuery(con, query)
      return(res)
    },
    send = function(statement){
      DBI::dbSendQuery(con, statement)
    }
  ))
}



# load("data/hist_parish.rda")
# parish_meta <- hist_parish@data
# parss <- parish_meta %>% filter(from > 0 | tom < 9999)

db <- pg_db()

preceding <- db$get('
  SELECT h1.geom_id AS g1, h2.geom_id as g2, h1."end" as year,
    h1.type_id
  FROM histmaps as h1 
  LEFT JOIN histmaps h2 on 
    ST_Intersects(h1.geom, h2.geom)
      AND h1.start = (h2."end" +1) 
      AND h1.type_id = h2.type_id
  WHERE ST_area(ST_Intersection(h1.geom, h2.geom)) > 10000
  ')

succeeding <- db$get('
    SELECT h1.geom_id AS g1, h2.geom_id as g2, h2."end" as year,
    h1.type_id
    FROM histmaps h1 
    LEFT JOIN histmaps h2 ON 
      ST_Intersects(h1.geom, h2.geom)
      AND h1."end" = (h2.start - 1)
      AND h1.type_id = h2.type_id
    where h1."end" < 9999 AND ST_area(ST_Intersection(h1.geom, h2.geom)) > 10000
  ')

db$close()

preceding <- preceding %>% 
  mutate(
    g1 = as.integer(g1),
    g2 = as.integer(g2),
    relation = "pre"
  )
succeeding <- succeeding %>% 
  mutate(
    g1 = as.integer(g1),
    g2 = as.integer(g2),
    relation = "succ"
  )

relations <- rbind(preceding, succeeding) %>% mutate(relation = as.factor(relation))

save(relations, file = "data/relations.rda")
