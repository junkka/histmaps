#' Create lowest common denominator id
#'
#' Foreach \code{x} and \code{y}, checks for matches in all \code{x} 
#'   and all \code{y}, gets all matches + all matches newid and updates
#'   \code{newid}. Essentially find the lowest common demoninator.
#'
#' @param x id 1
#' @param y id 2
#' @importFrom igraph graph_from_data_frame membership components
#' @export

create_block <- function(x, y){
  g <- igraph::graph_from_data_frame(data.frame(x, y))
  m <- igraph::membership(components(g))
  as.vector(m[match(x, as.integer(names(m)))])
}

