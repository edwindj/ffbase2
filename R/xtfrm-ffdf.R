#' @export
xtfrm.ff_vector <- function(x){
  # just make sure it is not a factor
  if (is.factor(x)){
    levels(x) <- NULL
  }
  x
}
