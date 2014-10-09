#' assumes that tbl is an ffdf and that dat has same structure as tbl
#' @importFrom ff hi
#' @keywords internal
append_to <- function(tbl, dat, check_structure=TRUE, ...){
  if (check_structure){
    assert_that(has_same_structure(tbl, dat))
  }
  
  is_char <- sapply(dat, is.character)
  if (any(is_char)){
    dat[is_char] <- lapply(dat[is_char], factor)
  }
  
  if (is.null(tbl)){
    return(tbl_ffdf(dat))
  }
  
  N <- nrow(tbl)
  n <- nrow(dat)
  
  if (n > 0){
    open(tbl)
    nrow(tbl) <- N+n
    tbl[ff::hi(N+1, N+n),] <- dat
  }
  tbl
}

has_same_structure <- function(tbl, dat){
  TRUE
}