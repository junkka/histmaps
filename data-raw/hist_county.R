# hist_county.R

library(dplyr)
library(stringr)

# load parish meta data see https://github.com/junkka/swe-parish
load('data-raw/data/for_hist.rda')
load('data/hist_parish.rda')
counties_meta <- read.csv("data-raw/data/county_meta.csv")
# create county code table
# nadkod
# from 
# to
# county
parish_meta <- hist_parish@data %>% 
  mutate(lanskod = floor(as.integer(forkod/10000)))

lan_meta <- tbl_df(for_hist) %>% 
  filter(!is.na(lan)) %>% 
  select(pid, forkod, name, lan, county) %>% 
  plyr::ddply("pid", function(a){
    counties <- str_trim(unlist(str_split(a$lan, ",")))
    from <- as.integer(str_replace(str_extract(counties, "^[0-9]{4}\\-"), "-", ""))
    tom <- as.integer(str_replace(str_extract(counties, "\\-[0-9]{4}"), "-", ""))
    c_name <- str_trim(str_replace(str_extract(counties, "([A-Za-zåäöÅÄÖ]* län)|(Närkes)"), "län", ""))
    data.frame(
      forkod = a$forkod,
      name = a$name,
      lan = counties,
      c_name = c_name,
      from = from,
      tom = tom,
      county = a$county
    )
  }) %>% 
  select(pid, forkod, name, c_name, from, tom, lan, county) 

pa <- parish_meta %>% 
  group_by(forkod) %>% mutate(n = n()) %>% 
  filter(n == 1) %>% 
  select(nadkod, f = from, t = tom, forkod)

ca <- counties_meta %>% 
  mutate(c_name = as.factor(name)) %>% 
  select(c_name, code)

lan_meta2 <- left_join(lan_meta, pa, by = "forkod") %>% 
  mutate(
    from = ifelse(is.na(from), f, from),
    tom  = ifelse(is.na(tom), t, tom)
  )  %>% 
  left_join(ca, by = "c_name") %>% 
  select(forkod, nadkod, name, code, c_name, from, tom, lan)
# write.csv(lan_meta2, file = "data-raw/code_counties.csv")

# Sel for stockholm stads län
# "select n2.nadkod
# from nad n1
# join nad n2 on ST_covers(n2.geom, n1.geom)
# where n2.tom >= 1715 AND n1.tom = 9999 AND n1.forkod in(18001, 18004, 18005, 18006, 18007, 18009, 18010, 18011, 18013, 18014, 18015, 18017,18018, 18019, 18021, 18023, 18025, 18027, 18028, 18029, 18031, 18034, 18036, 18038, 18039, 18040, 18041) 
# group by n2.nadkod"

manual_raw <- read.csv("data-raw/data/code_counties.csv") 
manual <- manual_raw %>% 
  filter(!is.na(forkod)) %>% 
  select(nadkod, code, f = from, t = tom)

counties <- parish_meta %>% select(nadkod, lanskod, from, tom) %>% 
  left_join(manual, by = "nadkod") 

library(assertthat)

linked <- counties[!is.na(counties$f), ]
with(linked, assert_that(all(from <= f)))
with(linked, assert_that(all(tom >= t)))
with(linked, assert_that(all(!is.na(code))))

# check by nadkod min(from) == min(f)
linked <- counties %>% 
  filter(!is.na(code)) %>% 
  group_by(nadkod) %>% 
  mutate(
    equal = min(from) == min(f),
    equal = max(tom) == max(t)
  ) %>% as.data.frame()

with(linked, assert_that(all(equal)))

counties <- counties %>% 
  mutate(
    ccode = ifelse(is.na(code), lanskod, code),
    from    = ifelse(is.na(f), from, f),
    tom     = ifelse(is.na(t), tom, t)
  ) %>% 
  select(-code, -f, -t, -lanskod)

par_to_county <- counties %>% rename(county = ccode)
save(par_to_county, file = "data/par_to_county.rda")

# check for overlap 
library(survival)
linked <- counties %>% 
  mutate(tom = tom + 1, event = 1) %>% 
  survSplit(data = ., cut = c(min(counties$from[counties$from > 0]):1991), end = "tom", event = "event", start = "from") %>% 
  group_by(nadkod, tom) %>% 
  summarise(n = n()) %>% 
  filter(n > 1) %>% 
  ungroup %>% count(nadkod)

assert_that(nrow(linked) == 0)

# for each year fortify and compare to last
library(ggplot2)
library(maptools)

get_county <- function(code, year, fort = T){
  nad1850 <- hist_parish[hist_parish@data$from <= year & hist_parish@data$tom >= year, ]
  co2 <- counties %>% 
    filter(from <= year, tom >= year) %>% 
    select(nadkod, ccode)
  dat <- slot(nad1850, "data")
  dat2 <- left_join(dat, co2, by = "nadkod")
  slot(nad1850, "data") <- dat2
  nad1850 <- nad1850[nad1850@data$ccode == code, ]
  a <- unionSpatialPolygons(nad1850, as.character(nad1850@data$ccode))  
  if (fort)
    return(fortify(a))
  else 
    return(a)
}

res2 <- plyr::ldply(c(1:28), function(pa){
  message(sprintf("County: %d", pa))
  startyear <- max(c((min(c((counties$from[counties$from > 0 & counties$ccode == pa] - 1), counties$tom[counties$ccode == pa]))), min(counties$from[counties$ccode == pa])))
  endyear   <- min(
    c(
      (max(c(
        counties$tom[counties$tom < 9999  & counties$ccode == pa],
        max(counties$from[counties$ccode == pa]))
      ) + 1),
      max(counties$tom[counties$ccode == pa])
    )
  )
  message(sprintf("start: %d end: %d", startyear, endyear))
  years <- c(startyear:endyear)
  n <-years[1]
  compare <- get_county(pa, n) %>% 
    select(long, lat) %>% 
    distinct() %>% 
    arrange(long, lat)

  res <- plyr::ldply(years[-1], function(i){
    nexty <- get_county(pa, i) %>% 
      select(long, lat) %>% 
      distinct() %>% 
      arrange(long, lat)
    
    # if nexty and compare is equal return inv
    # else return year
    if (!all(dim(compare) == dim(nexty))) {
      compare <<- nexty
      return(data.frame(data = i))
    } else if (all(compare == nexty)){
      return(invisible())
    } else {
      compare <<- nexty
      return(data.frame(data = i))
    }
  }, .progress = "tk")
  res <- rbind(data.frame(data = years[1]), res)
  res$id = pa
  return(res)
})

# replace min res2 


test_plot <- function(pa){

  get_dat <- function(k){
    nad1850 <- hist_parish[hist_parish@data$from <= k & hist_parish@data$tom >= k, ]
    co2 <- counties %>% filter(from <= k, tom >= k) %>% select(nadkod, ccode)
    dat <- slot(nad1850, "data")
    dat2 <- left_join(dat, co2, by = "nadkod")
    slot(nad1850, "data") <- dat2
    nad1850 <- nad1850[nad1850@data$ccode == pa, ]
    a <- unionSpatialPolygons(nad1850, as.character(nad1850@data$ccode))
  }
  x <- res2$data[res2$id == pa] %>% unique()
  x <- x[ordered(x)]
  # make sp for each year in res2 add metadata and combine
  par(mfrow=c(2,length(x)))
  i <- x[1]
  a <- get_dat(i)
  # first <- fortify(a)
  plot(a, col = "red")
  plyr::l_ply(x[-1], function(i){
    a <- get_dat(i)
    plot(a, add = T, col = "white")
    plot(a, col = "red")
  })
  # plot 2 add 1, 3 add 2, 4 add 3, 5 add 4, 6 add 5

  a <- get_dat(x[2])
  plot(a, col = "red")
  if (length(x)  < 3) {
    a <- get_dat(x[1]) 
    plot(a1, add = T, col = "white")
  }
  plyr::l_ply(c(3:length(x)), function(b){
    i <- x[b]
    j <- x[(b - 2)]
    a1 <- get_dat(j)
    a2 <- get_dat(i)

    plot(a1, add = T, col = "white")
    plot(a2, col = "red")
  })
  a <- get_dat(x[(length(x) - 1)])
  plot(a, add = T, , col = "white")
}
# save(res2, file = "data-raw/res2.rda")

count_range <- res2 %>% 
  group_by(id) %>% 
  mutate(from = data, tom = as.integer(lead(data, default = NA) -1)) %>% 
  select(-data)
# update min date
count_range <- counties %>% group_by(ccode) %>% summarise(m = min(from)) %>% 
  left_join(count_range, ., by = c("id" = "ccode")) %>% 
  group_by(id) %>% 
  mutate(from = ifelse(row_number() == 1, m, from)) %>% 
  select(-m) %>% ungroup()
counties2 <- counties %>% group_by(ccode) %>% 
  summarise(last = max(tom)) %>% 
  left_join(count_range, ., by = c("id" = "ccode")) %>% 
  mutate(tom = ifelse(is.na(tom), last, tom)) %>% 
  select(-last)

# Fix manually 
# write.csv(counties2, file = "data-raw/counties-meta2.csv")
counties_meta <- read.csv("data-raw/data/county_meta.csv")

counties2 <- read.csv("data-raw/data/counties-meta.csv")
counties2 <- counties_meta %>% select(code, name, letter, center) %>% left_join(counties2, ., by = c("id" = "code")) 

# -------------------------------------------------
# By counties2 extract sp and combine
# -------------------------------------------------

get_county_shp <- function(code, year){
  shp <- hist_parish[hist_parish@data$from <= year & hist_parish@data$tom >= year, ]
  co2 <- counties %>% 
    filter(from <= year, tom >= year) %>% 
    select(nadkod, ccode)
  dat <- slot(shp, "data")
  dat2 <- left_join(dat, co2, by = "nadkod")
  slot(shp, "data") <- dat2
  shp <- shp[shp@data$ccode == code, ]
  meta <- counties2 %>% filter(from == year, id == code) %>% 
    select(lan = id, from, tom, name, letter, center) %>% as.data.frame()
  rownames(meta) <- as.character(code)
  meta$geomid <- paste0(meta$lan, meta$tom)
  a <- unionSpatialPolygons(shp, as.character(shp@data$ccode))  
  a <- SpatialPolygonsDataFrame(a, meta)
  a <- spChFIDs(a, meta$geomid)
  return(a)
}

counties2$rid <- rownames(counties2)
counties2 <- as.data.frame(counties2)
shp <- get_county_shp(counties2$id[1], counties2$from[1])


plyr::d_ply(counties2[-1, ], "rid", function(obs){
  shp2 <- get_county_shp(obs$id[1], obs$from[1])
  shp <<- spRbind(shp, shp2)
})

# Find small holes 

remove_small_holes <- function(x) {
  message('Remove holes')
  # For each polygons object in x find holes and remove
  for (i in rownames(x@data)) {
    b     <- x[rownames(x@data) == i, ]
    b_p   <- slot(b, 'polygons')[[1]]
    b_p_P <- Polygons(slot(b_p, 'Polygons'), i)
    holes <- sapply(slot(b_p_P, "Polygons"), slot, "hole")
    # check hole size
    area   <- sapply(slot(b_p_P, "Polygons"), slot, "area") < 7000000
    holes2 <- apply(matrix(c(holes, area), nrow= 2, byrow = T), 2, all)
    res   <- slot(b_p_P, "Polygons")[!holes2]
    # make new SPs
    new_sps <- SpatialPolygons(
      list(Polygons(res, ID = i)), 
      proj4string = CRS(proj4string(x))
    ) 
    x <- SpatialPolygonsDataFrame(
      spRbind(x[rownames(x@data) != i, ], new_sps), 
      x@data
    )
  }
  return(x)
} 

hist_county <- remove_small_holes(shp)
hist_county@data$lan <- as.integer(hist_county@data$lan)

area <- data.frame(
    geomid = sapply(hist_county@polygons, function(x) x@ID),
    area = sapply(hist_county@polygons, function(x) x@area)
  )
slot(hist_county, "data") <- slot(hist_county, "data") %>% 
  left_join(area, by = "geomid")


update_encoding <- function(x) {
  Encoding(levels(x)) <- "UTF-8"
  levels(x) <- iconv(
    levels(x), 
    "UTF-8",
    "latin1" 
  )

  Encoding(levels(x)) <- "latin1"
  levels(x) <- iconv(
    levels(x), 
    "latin1", 
    "UTF-8"
  )
  return(x)
}

hist_county@data$name <- update_encoding(hist_county@data$name)
hist_county@data$center <- update_encoding(hist_county@data$center)

# library(rgdal)
# writeOGR(hist_county, 'hist_county', "hist_county", driver="ESRI Shapefile")
save(hist_county, file = "data/hist_county.rda", compress = "xz")
