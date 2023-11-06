#' Retrieve Administrative Boundaries
#'
#' This function obtains administrative boundaries for a specific date and type of unit, allowing users to specify the format of the returned object.
#'
#' @param date A date for which the boundaries should be retrieved.
#' @param type A character vector indicating the type of administrative unit (default options are available). Possible values include: "magistrate", "bailiwick", "hundred", "court", "municipal", "contract", "parish", "county", "pastorship", "diocese", and "district_couryt".
#' @param format A character indicating the format of the returned object (default is "sf").
#' @param boundary_type A character string specifying the type of boundaries to return. Choose between "polygons" (default) for polygon shapes of the administrative units, and "borders" for the multiline border representation of the units.
#' 
#' @import sf
#' @importFrom stats aggregate
#' @importFrom utils data
#' @importFrom methods slot "slot<-"
#' @export
#' @examples
#' 
#' county_map <- get_boundaries(1800, "county")
#' 
#' plot(st_geometry(county_map))
#' 

get_boundaries <- function(
  date, 
  type = c("magistrate", "bailiwick", "hundred", "court", "municipal", 
           "contract", "parish", "county", "pastorship", "diocese", "district_couryt"), 
  format = c("sf", "meta"),
  boundary_type = c("polygons", "borders")){
  
  
  
  typed <- match.arg(type)
  format <- match.arg(format)
  boundary_type <- match.arg(boundary_type)
  

  date_l <- get_date(date)
  y <- date_l$x
  x <- date_l$y
  period <- date_l$period
  
  if (x > 1990 || x < 1634 || y > 1990 || y < 1634 )
    stop("Date must be between 1634-01-01 and 1990-12-31")
  
  env <- environment()
  
  if (boundary_type == "polygons") {
    data(geom_sp, package = "histmaps", envir = env)
    # res <- filter(geom_sp, start <= y, end >= x, type_id == typed)
    res <- filter_period_type(geom_sp, x, y, typed)
    # add county
    
    if (period) {
      # get new  commbined id 
      ids <- get_geom_period(x, y, typed)
      res <- get_geom_period_map(res, ids, typed)
      return(list(map = switch(format,
                               sf = res,
                               meta = st_as_data_frame(sf)
      ), lookup = ids))
    }
  } else {
    if (x != y) {
      stop("For 'borders', date must be a single year")
    }
    data(geom_borders, package = "histmaps", envir = env)
    res <- filter_period_type(geom_borders, x, y, typed)
  }
  
  return(switch(format,
                sf = res,
                meta = st_as_data_frame(res)
  ))
}


get_geom_period <- function(x, y, typed){
  e <- environment()
  data(geom_relations, package = "histmaps", envir = e)
  data(geom_sp, package = "histmaps", envir = e)
  
  # rels <- filter(geom_relations, year >= x, year <= y, type_id == typed) %>% 
  #   select(geom_id = g1, geom_id2 = g2)
  
  rels <- geom_relations[geom_relations$year <= y & geom_relations$year >= x & geom_relations$type_id == typed, ]
  
  rels$geom_id <- rels$g1
  rels$geom_id2 <- rels$g2
  rels <- rels[ ,c("geom_id", "geom_id2")]
  
  # pars <- st_as_data_frame(geom_sp) %>% 
  #   filter(start <= y & end >= x, type_id == typed) %>% 
  #   mutate(geom_id2 = geom_id) %>% 
  #   select(geom_id, geom_id2)
  
  pars <- st_as_data_frame(geom_sp)
  pars <- filter_period_type(pars, x, y, typed)
  
  pars$geom_id2 <- pars$geom_id
  pars <- pars[ ,c("geom_id", "geom_id2")]
  
  
  # parsc <- rbind(rels, pars) %>% 
  #   distinct() %>% 
  #   mutate(geomid = histmaps::create_block(geom_id, geom_id2)) %>% 
  #   select(geom_id, geomid) %>% distinct()
  
  parsc <- rbind(rels, pars)
  parsc <- parsc[!duplicated(parsc), ]
  parsc$geomid <- histmaps::create_block(parsc$geom_id, parsc$geom_id2)
  parsc <- parsc[ ,c("geom_id", "geomid")]
  parsc <- parsc[!duplicated(parsc), ]
  
  return(parsc)
}

get_geom_period_map <- function(m, ids, typed){
  
  # m %>% left_join(ids, by = "geom_id") %>% 
  #   group_by(geomid) %>% 
  #   summarise(type = first(type))
  t1 <- merge(m, ids, by = "geom_id", all.x = T)
  aggregate(t1, by = list(t1$geom_id), function(x) x[1])
}

filter_period_type <- function(d, x, y, typed){
  d[d$start <= y & d$end >= x & d$type_id == typed, ]
}