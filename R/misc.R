#' Create lowest common denominator id
#'
#' Foreach \code{x} and \code{y}, checks for matches in all \code{x} 
#'   and all \code{y}, gets all matches + all matches newid and updates
#'   \code{newid}. Essentially find the lowest common demoninator.
#'
#' @param x id 1
#' @param y id 2
#' @import igraph
#' @export

create_block <- function(x, y){
  g <- graph_from_data_frame(data.frame(x, y))
  m <- membership(components(g))
  as.vector(m[match(x, as.integer(names(m)))])
}


#' Create ggplot map data.frame from NAD sp
#'
#' @param sp a SparialPolygonsDataFrame object
#' @importFrom ggplot2 fortify
#' @export

sp_to_ggplot <- function(sp){

  if (inherits(sp, "SpatialPointsDataFrame"))  
    return(as.data.frame(sp))

  sp@data$id = rownames(sp@data)
  ret_d = ggplot2::fortify(sp, region="id")
  ret_d = left_join(ret_d, sp@data, by="id")
  return(ret_d)
}