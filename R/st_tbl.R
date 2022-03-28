#  Bla sf object to tibble
#' 
#' Transform sf object to tibble
#' 
#' @param x sf object
#' @importFrom tibble as_tibble
#' @export


st_as_data_frame <- function(x){
  x$geometry <- NULL
  tibble::as_tibble(x)
}
