
library(RPostgreSQL)

pg_db <- function() {
  drv <- DBI::dbDriver("PostgreSQL")
  con <- DBI::dbConnect(drv, 
                        dbname = "maps"
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