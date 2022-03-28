# hist_boundries.R

#' Historical boundaries
#'
#' Deprecated. Use `get_boundaries`. 
#' Get Swedish administrative boundaries of parishes or counties 
#'   for a specified year 1634-1990.
#'
#' @param date a date, a year or a vector with date/year range
#' @param type type of unit, "parish", "county" or "town"
#' @param format format of return object, "df" for data.frame, "sp" for 
#'   SpatialPolygonsDataFrame and "meta" for only meta-data.
#' @export
#' @examples
#' library(sf)
#' map <- hist_boundaries(1900, "county")
#' plot(st_geometry(map))
#' 
#' 

hist_boundaries <- function(date, 
    type = c("parish", "county", "town"), 
    format = c("sp", "df", "meta")) {
  message("Deprecated. Use `get_boundaries`")
  type <- match.arg(type)
  format <- match.arg(format)
  get_boundaries(date, type)
}


get_date <- function(date){
  
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
    period <- TRUE
  }
  return(list(x=x,y=y,period=period))
}

#' Parish boundaries
#' 
#' Get Swedish parish boundaries for a specified year 1634-1990.
#' 
#' @param date a date, a year or a vector with date/year range
#' @param format format of return object, "df" for data.frame, "sp" for 
#'   SpatialPolygonsDataFrame and "meta" for only meta-data.
#' @export

parish_boundaries <- function(date, format = c("sp", "df", "meta")) {
  get_boundaries(date, "parish")
}


#' County boundaries
#' 
#' Get Swedish county boundaries for a specified year 1634-1990.
#' 
#' @param date a date, a year or a vector with date/year range
#' @param format format of return object, "df" for data.frame, "sp" for 
#'   SpatialPolygonsDataFrame and "meta" for only meta-data.
#' @export


county_boundaries <- function(date, format = c("sp", "df", "meta")){
  get_boundaries(date, "county")
}

#' Towns in Sweden
#' 
#' Get Swedish towns for a specified year 1634-1990.
#' 
#' @param date a date, a year or a vector with date/year range
#' @param format format of return object, "df" for data.frame, "sp" for 
#'   SpatialPointsDataFrame and "meta" for only meta-data.
#' @export

county_towns <- function(date, format = c("sp", "df", "meta")){
  
  format <- match.arg(format)
  
  date_l <- get_date(date)
  x <- date_l$x
  y <- date_l$y
  
  data(hist_town, package = "histmaps", envir = env)
  res <- subset(hist_town, from <= x & tom >= y)
  return(switch(format,
                sp = res,
                df = as.data.frame(res),
                meta = res@data))
}


#' Get year
#'
#' Transforms a atomic vector to a integer of year
#'
#' @param x An oabject to be converted
#' @param ... other paramaters for methods
#' @importFrom lubridate ymd year

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
    as.integer(grep( "^[0-9]{1,4}", x, value = T))
}

get_year.default <- function(x, ...) {
  as.integer(lubridate::year(x))
}
