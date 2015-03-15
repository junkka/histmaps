# parish_relations.R
library(dplyr)
library(RPostgreSQL)

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

db <- pg_db()

load("data/hist_parish.rda")
parish_meta <- hist_parish@data
parss <- parish_meta %>% filter(from > 0 | tom < 9999)

preceding <- db$get("
    SELECT n1.nadkod, n2.nadkod as nadkod2, n2.tom as year
    FROM nad n1 
    LEFT JOIN nad n2 ON 
      ST_Intersects(n1.geom, n2.geom)
      AND n1.from = (n2.tom +1)
    where n1.from > 0 AND ST_area(ST_Intersection(n1.geom, n2.geom)) > 10000
    ")
succeeding <- db$get("
    SELECT n1.nadkod, n2.nadkod as nadkod2, n2.from as year
    FROM nad n1 
    LEFT JOIN nad n2 ON 
      ST_Intersects(n1.geom, n2.geom)
      AND n1.tom = (n2.from - 1)
    where n1.tom < 9999 AND ST_area(ST_Intersection(n1.geom, n2.geom)) > 10000
  ")
preceding <- preceding %>% 
  mutate(
    nadkod = as.integer(nadkod),
    nadkod2 = as.integer(nadkod2),
    relation = "pre"
  )
succeeding <- succeeding %>% 
  mutate(
    nadkod = as.integer(nadkod),
    nadkod2 = as.integer(nadkod2),
    relation = "succ"
  )

parish_relations <- rbind(preceding, succeeding) %>% mutate(relation = as.factor(relation))
db$close()

save(parish_relations, file = "data/parish_relations.rda")
