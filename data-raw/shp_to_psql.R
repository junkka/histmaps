# shp_to_psql.R

library(rgdal)

source("data-raw/db.R")

data(nad)
temp_dir <- tempdir()
unlink(file.path(temp_dir, 'nad2'), recursive = T)
writeOGR(nad, file.path(temp_dir, 'nad2'), "nad2", driver="ESRI Shapefile")

db <- pg_db()
db$send("DROP TABLE IF EXISTS nad2;")
db$close()

system(sprintf('shp2pgsql -I -s 2400 -W "latin1" %s/nad2/nad2.shp nad2 | psql -d maps', temp_dir))
