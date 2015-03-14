# swe_boundries.R

#' Swe Boundaries
#'
#' Get swedish administrative boundaries for a specified year.
#'
#' @param date a date or a year
#' @param type type of unit, "county" or "municipal"
#' @param format format of return object, "df" for data.frame and "sp" for 
#'   SpatialPolygonsDataFrame
#' @export
#' @import dplyr
#' @import sp
#' @import sweboundaries
#' @examples
#' map <- hist_boundaries(1900, "county")
#' library(sp)
#' plot(map)
#' 

hist_boundaries <- function(date, 
    type = c("parish", "county"), 
    format = c("sp", "df", "meta")) {
  x <- get_year(date)
  if (x > 1990 || x < 1500)
    stop("For county, date must be a date between 1998-01-01 and 2010-12-31")
  
  # env <- environment()
  type <- match.arg(type)
  format <- match.arg(format)

  if (type == "county"){
    res <- subset(hist_county, from <= x & tom >= x)
  }
  if (type == "parish") {
    res <- subset(hist_parish, from <= x & tom >= x)
    # add county
    slot(res, "data") <- par_to_county %>% 
      filter(from <= x, tom >= x) %>% 
      select(nadkod, county) %>% 
      left_join(slot(res, "data"), ., by = "nadkod")
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