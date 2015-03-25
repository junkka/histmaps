# residential
# library(rvest)
# library(stringr)
# library(dplyr)
library(sp)

# wiki <- "http://sv.wikipedia.org"
# url <- "wiki/Sveriges_l%C3%A4n"

# page <- html(file.path(wiki, url))

# tbl_node <- page %>% html_node(".wikitable") 

# counties <-  tbl_node %>% html_table()

# rows <- tbl_node %>% html_nodes("tr")

# res <- plyr::ldply(rows[-1], function(a){

#   link <- a %>% html_nodes("a") %>% .[[3]] %>% html_attr("href")
#   link_page <- html(paste0(wiki, link))
#   coord_link <- link_page %>% html_node("#coordinates") %>% html_nodes("a") %>% last() %>% 
#     html_attr("href")
#   coord_page <- html(paste0("http:", coord_link))
#   coords <- coord_page %>% html_node(".geo") %>% html_text() %>% 
#     str_extract_all("[0-9.]{3,12}") %>% unlist()
#   data.frame(lat = coords[1], lon = coords[2])
# })

# d <- cbind(counties, res) %>% 
#   select(Kod, Residensstad, lon, lat) %>% 
#   filter(!is.na(Kod)) %>% 
#   mutate(
#     lat = as.numeric(as.character(lat)),
#     lon = as.numeric(as.character(lon))
#   )
# colnames(d) <- tolower(colnames(d))
# write.csv(arrange(d, kod), file = "data-raw/residential.csv", row.names = F)

d <- read.csv("data-raw/residential.csv")
Encoding(levels(d$town)) <- "UTF-8"
x <- SpatialPoints(d[ ,c("lon", "lat")], CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "))
x <- spTransform(x, CRS("+proj=tmerc +lat_0=0 +lon_0=15.80827777777778 +k=1 +x_0=1500000 +y_0=0 +ellps=bessel +units=m +no_defs"))
hist_town <- SpatialPointsDataFrame(x, d[ ,c("code", "town", "from", "tom")])


save(hist_town, file = "data/hist_town.rda")
