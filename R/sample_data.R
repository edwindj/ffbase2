#' Uniformly sample data tabular object
#' 
#' @param x tabular object to sample from
#' @param n size of sample
#' @param seed random seed 
#' @param ... not used
#' @export
sample_data <- function(x, n=1e4, seed = NULL, ...){
  UseMethod("sample_data")
}

#' @export
sample_data.default <- function(x, n=1e4, seed = NULL, ...){
  N <- nrow(x)
  if (!is.null(seed)){ 
    set.seed(seed)
  }
  i <- sort.int(as.integer(1+N*runif(n)))
  x[i,]
}

#' @export
sample_data.ffdf <- function(x, n=1e4, seed = NULL, ...){
  N <- nrow(x)
  if (!is.null(seed)){ 
    set.seed(seed)
  }
  i <- ff(sort.int(as.integer(1+N*runif(n))))
  x[i,]
}