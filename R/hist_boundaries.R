# swe_boundries.R

#' Swe Boundaries
#'
#' Get swedish administrative boundaries for a specified year.
#'
#' @param date a date, a year or a date/year range
#' @param type type of unit, "county" or "municipal"
#' @param format format of return object, "df" for data.frame, "sp" for 
#'   SpatialPolygonsDataFrame and "meta" for only meta-data.
#' @export
#' @import dplyr
#' @import sp
#' @import sweboundaries
#' @import assertthat
#' @import maptools
#' @examples
#' map <- hist_boundaries(1900, "county")
#' library(sp)
#' plot(map)
#' 

hist_boundaries <- function(date, 
    type = c("parish", "county"), 
    format = c("sp", "df", "meta")) {

  if (length(date) == 1){
    x <- get_year(date)
    y <- x
    period <- FALSE
  } else if (length(date) == 2){
    # get year of both
    y <- get_year(date[1])
    x <- get_year(date[2])
    assert_that(y <= x, msg = "Range start must be before end")
    # set period to TRUE
    period <- TRUE
  }

  if (x > 1990 || x < 1600 || y > 1990 || y < 1600 )
    stop("Date must be between 1600-01-01 and 1990-12-31")
  
  # env <- environment()
  type <- match.arg(type)
  format <- match.arg(format)

  if (type == "county"){
    res <- subset(hist_county, from <= x & tom >= y)
  }
  if (type == "parish") {
    res <- subset(hist_parish, from <= x & tom >= y)
    # add county
    slot(res, "data") <- par_to_county %>% 
      filter(from <= x, tom >= y) %>% 
      select(nadkod, county) %>% 
      left_join(slot(res, "data"), ., by = "nadkod")
  }

  if (period) {
    # get new  commbined id 
    ids <- get_period(x, y)
    res <- get_period_map(res, ids)
    return(list(map = switch(format,
        sp = res,
        df = sweboundaries::sp_to_ggplot(res),
        meta = res@data
      ), lookup = ids))
  }

  return(switch(format,
    sp = res,
    df = sweboundaries::sp_to_ggplot(res),
    meta = res@data
  ))
}

#' Get year
#'
#' Transforms a atomic vector to a integer of year
#'
#' @param x An oabject to be converted
#' @param ... other paramaters for methods

get_year <- function(x, ...) UseMethod("get_year", x)

get_year.integer <- function(x, ...) {
  if(x < 2020) return(x)

  as.integer(substr(as.character(x), 1, 4))
}

get_year.numeric <- function(x, ...) {
  if(x < 2020) 
    return(as.integer(x))

  as.integer(substr(as.character(x), 1, 4))
}

get_year.character <- function(x, ...) {
  if (nchar(x) <= 4)
    return(as.integer(x))

  y <- as.integer(lubridate::year(lubridate::ymd(x, quiet = TRUE, ...)))
  if (!is.na(y))
    return(y)
  else
    as.integer(stringr::str_extract(x, "^[0-9]{1,4}"))
}

get_year.default <- function(x, ...) {
  as.integer(lubridate::year(x))
}

#' Get common boundaries over a period
#'
#' Description
#'
#' @param x end
#' @param y start
#' 


get_period <- function(x, y){
  # for a range get all relationscodes that are stretching over period
  rels <- filter(parish_relations, year <= x, year >= y) %>% 
    select(nadkod, nadkod2)
  pars <- slot(subset(hist_parish, from <= x & tom >= y), "data") %>% 
    mutate(nadkod2 = nadkod) %>% 
    select(nadkod, nadkod2)
  parsc <- rbind(rels, pars) %>% 
    distinct() %>% 
    mutate(geomid = create_block(nadkod, nadkod2)) %>% 
    select(nadkod, geomid) %>% distinct()
  return(parsc)
}


get_period_map <- function(m, ids){
  if (!require(maptools))
    stop("Install maptools for SpatialPolygonsDataFrame")
  
  d <- slot(m, "data") %>% 
    select(nadkod) %>% 
    left_join(ids, by = "nadkod")
  
  assert_that(all(!is.na(d$geomid)))
  assert_that(nrow(d) == nrow(m))

  slot(m, "data") <- d
  res <- unionSpatialPolygons(m, m@data$geomid)
  
  assert_that(length(res) == length(unique(ids$geomid)))
  
  dat <- data.frame(geomid = unique(ids$geomid))
  rownames(dat) <- unique(ids$geomid)
  res <- SpatialPolygonsDataFrame(res, dat)
  
  return(res)
}