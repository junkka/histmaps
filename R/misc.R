#' Create lowest common denominator id
#'
#' Foreach \code{x} and \code{y}, checks for matches in all \code{x} 
#'   and all \code{y}, gets all matches + all matches newid and updates
#'   \code{newid}. Essentially find the lowest common demoninator.
#'
#' @param x id 1
#' @param y id 2
#' @export

create_block <- function(x, y) {
    
  new <- rep(NA, length(x))
  names(x) <- c(1:length(x))
  names(y) <- c(1:length(y))

  for (i in 1:length(x)) {
    x_matches <- na.omit(x[x %in% c(x[i], y[i])])
    y_matches <- na.omit(y[y %in% c(x[i], y[i])])
    matches <- as.integer(unique(c(names(x_matches), names(y_matches))))
    newids <- unique(na.omit(new[matches]))
    # get id that have match
    new_match <- which(new %in% newids)
    matches <- na.omit(unique(c(matches, new_match)))
    new[matches] <- i
  }
  return(new)
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