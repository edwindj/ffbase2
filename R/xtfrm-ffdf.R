#' @export
xtfrm.ff_vector <- function(x){
  # just make sure it is not a factor
  levels(x) <- NULL
  x
}
