#' @export
sample_n.ffdf <- function(tbl, size, replace=FALSE, weight=NULL
                          , .env=parent.frame()){
  open(tbl)
  on.exit(close(tbl))

  if (!missing(weight)) {
    weight <- eval(substitute(weight), tbl, .env)
  }
  
  #TODO Hm, should this be a data.frame or a ffdf frame...
  #TODO move dplyr sample_n_basic into ffbase2
  #TODO make this one more efficient by using a ff(integer) vector for selecting.
  tbl_ffdf(sample_n_basic(tbl, size, replace=replace, weight=weight))
}

#' @export
sample_frac.ffdf <- function(tbl, size=1, replace=FALSE, weight=NULL
                             , .env=parent.frame()){
  if (!missing(weight)) {
    weight <- eval(substitute(weight), tbl, .env)
  }
  sample_n.ffdf(tbl, round(size*nrow(tbl)), replace=replace, weight=weight)
}

sample_n_basic <-
  function (tbl, size, replace = FALSE, weight = NULL) 
  {
    n <- nrow(tbl)
    weight <- check_weight(weight, n)
    assert_that(is.numeric(size), length(size) == 1, size >= 
                  0)
    check_size(size, n, replace)
    idx <- sample.int(n, size, replace = replace, prob = weight)
    tbl[as.ff(idx), , drop = FALSE]
  }
