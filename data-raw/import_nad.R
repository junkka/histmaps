# import_nad.R

library(rgdal)
library(rgeos)
library(readxl)
library(assertthat)
library(dplyr)

# Download data from http://riksarkivet.se/psidata

tempd <- tempdir()

unzip("data-raw/ra/NAD_Topografidata.zip", exdir = tempd)

version <-  "7520" # "2504"
fname  <- paste0("__pgsql2shp",version ,"_tmp_table")

x <- readOGR(dsn = file.path(tempd, version, paste0(fname, ".shp")), layer = fname)

d <- x@data

colnames(d) <- c("g_end_y", "g_start_y", "g_type", "g_unit", "g_seq")

d2 <- d %>% 
  mutate(
    g_start_y = ifelse(is.na(g_start_y), 0, g_start_y),
    g_type = tolower(as.character(g_type))
  ) %>% 
  as.data.frame()

meta_names <- read_excel("data-raw/ra/g_units_names.xls")

colnames(meta_names) <- tolower(colnames(meta_names))

d3 <- left_join(d2, meta_names, by = "g_unit") 

assert_that(nrow(d3) == nrow(d2))
assert_that(nrow(d3[is.na(d3$ref), ]) == 0)

d4 <- d3 %>% 
  mutate(
    nadkod = stringr::str_replace(ref, "SE/", ""),
    nadkod = as.integer(nadkod)
  )

# old codes
data("hist_parish")
h <- hist_parish@data %>% 
  select(nadkod, dedikscb, dedik, forkod)

d5 <- left_join(d4, h)

assert_that(nrow(d5[is.na(d5$dedik) & d5$g_type == "swe_kyrk", ]) == 0)

x@data <- d5

# Fix self intersections
x2 <- gBuffer(x, width=0, byid = T)
# Check polygons
gIsValid(x2, reason = T)

nad <- x2

save(nad, file = "data/nad.rda", compress = "xz")

# 
# 
# year <- 1990 
# typ <- levels(d$type)[7]
# m <- subset(x, type == typ & end >= year & start <= year & unit == 10778736) 
# plot(m, col = "grey")
# title(paste(typ, year))
# 
# library(readxl)
# 
# meta_names <- read_excel("data-raw/ra/g_units_names.xls")
# meta_rel <- read_excel("data-raw/ra/g_units_relations.xls")
# 
# library(histmaps)
# 
# prs <- subset(x, type == "SWE_KYRK" & end >= year & start <= year)
# 
# a <- cut_spbox(prs, m, 2000)
# 
# plot(m, col = "red")
# plot(prs, add = T)
# 
