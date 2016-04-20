# hist_boundries.R

#' Historical boundaries
#'
#' Get Swedish administrative boundaries of parishes or counties 
#'   for a specified year 1634-1990.
#'
#' @param date a date, a year or a vector with date/year range
#' @param type type of unit, "parish", "county" or "town"
#' @param format format of return object, "df" for data.frame, "sp" for 
#'   SpatialPolygonsDataFrame and "meta" for only meta-data.
#' @export
#' @import sp
#' @import maptools
#' @import dplyr
#' @examples
#' map <- hist_boundaries(1900, "county")
#' library(sp)
#' plot(map)
#' 
#' period_map <- hist_boundaries(c(1800, 1900))
#' plot(period_map$map)
#' 

hist_boundaries <- function(date, 
    type = c("parish", "county", "harad", "fogderi", "hovratt", "kommun", "kontrakt", "domsaga", "stift", "tingsratt"), 
    format = c("sp", "df", "meta")) {

  type <- match.arg(type)
  type <- type_match(type)
  format <- match.arg(format)

  if (length(date) == 1){
    x <- get_year(date)
    y <- x
    period <- FALSE
  } else if (length(date) == 2){
    # get year of both
    y <- get_year(date[1])
    x <- get_year(date[2])
    if (y > x)
      stop("Range start must be before end")
    # set period to TRUE
    period <- TRUE
  }

  if (x > 1990 || x < 1634 || y > 1990 || y < 1634 )
    stop("Date must be between 1634-01-01 and 1990-12-31")
  
  env <- environment()

  
  data(nad, package = "histmaps", envir = env)
  res <- subset(nad, g_type == type & g_end_y >= y & g_start_y <=x)
  
  if (period) {
    # get new  commbined id 
    ids <- get_period(x, y, type)
    res <- get_period_map(res, ids)
    return(list(map = switch(format,
        sp = res,
        df = sp_to_ggplot(res),
        meta = res@data
      ), lookup = ids))
  }

  return(switch(format,
    sp = res,
    df = sp_to_ggplot(res),
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


get_period <- function(x, y, type){
  # for a range get all relationscodes that are stretching over period
  e <- environment()
  data(parish_relations, package = "histmaps", envir = e)
  data(nad, package = "histmaps", envir = e)

  rels <- filter(parish_relations, year <= x, year >= y & g_type == type) %>% 
    select(g_unit, g_unit2)
  pars <- slot(subset(nad, g_start_y <= x & g_end_y >= y & g_type == type), "data") %>% 
    mutate(g_unit2 = g_unit) %>% 
    select(g_unit, g_unit2)
  parsc <- rbind(rels, pars) %>% 
    distinct() %>% 
    mutate(geomid = create_block(g_unit, g_unit2)) %>% 
    select(g_unit, geomid) %>% distinct()
  return(parsc)
}


get_period_map <- function(m, ids){
  if (!require(maptools))
    stop("Period map requires maptools package")
  
  d <- slot(m, "data") %>% 
    select(g_unit) %>% 
    left_join(ids, by = "g_unit")
    
  stopifnot(all(!is.na(d$geomid)))
  stopifnot(nrow(d) == nrow(m))

  slot(m, "data") <- d
  res <- maptools::unionSpatialPolygons(m, m@data$geomid)
   
  dat <- data.frame(geomid = unique(ids$geomid))
  rownames(dat) <- unique(ids$geomid)
  res <- SpatialPolygonsDataFrame(res, dat)
  
  return(res)
}

type_match <- function(x){
  ind <- c("parish", "county", "harad", "fogderi", "hovratt", "kommun", "kontrakt", "domsaga", "stift", "tingsratt")
  out <- c("swe_kyrk", "swe_lan", "swe_harad", "swe_fog", "swe_hov", "swe_kom", "swe_kon", "swe_rad", "swe_stift", "swe_tingt")
  out[ind == x]
}