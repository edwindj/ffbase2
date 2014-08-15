#' A grouped ffdf.
#'
#' The easiest way to create a grouped ffdf is to call the \code{group_by}
#' method on a ffdf or data source: this will take care of capturing
#' the unevalated expressions for you.
#'
#' @param data a data source or data frame.
#' @param vars a list of quoted variables.
#' @param drop 
grouped_ffdf <- function(data, vars, drop=TRUE) {
  assert_that(is.ffdf(data))
  is_name <- vapply(vars, is.name, logical(1))
  if (!all(is_name)) {
    stop("Data tables can only be grouped by variables, not expressions",
      call. = FALSE)
  }
  
  o <- ff::ffdforder(data[as.character(vars)])
  rles <- lapply(as.character(vars), function(v){
    rle_ff(data[[v]][o])
    #TODO fix/checks the rles, so that rle[[i+1]]] is a subdivision of rle[i] 
  })
  
#   attr(data, "vars") <- vars
#   attr(data, "indices") <- list(order=o, rles=rles, n_groups=rles[[length(rles)]])

  structure( data
           , class = c("grouped_ffdf", "tbl_ffdf", "tbl", "ffdf")
           , vars = vars
           , indices = list(order=o, rles=rles, n_groups=rles[[length(rles)]])
           )
}

#' @rdname grouped_ffdf
#' @param x an object to check
#' @export
is.grouped_ffdf <- function(x) inherits(x, "grouped_ffdf")

#' @export
print.grouped_ffdf <- function(x, ..., n=NULL) {
  cat("Source: local ffdf ", dim_desc(x), "\n", sep = "")
  cat("Groups: ", commas(deparse_all(groups(x))), "\n", sep = "")
  cat("\n")
  trunc_mat(x, n=n)
}

#' @export
group_size.grouped_ffdf <- function(x) {
  n <- attr(x, "indices")$n_groups
  n$lengths
}

#' @export
groups.tbl_ffdf <- function(x) {
  attr(x, "vars")
}

#' @export
as.data.frame.grouped_ffdf <- function(x, row.names = NULL,
                                     optional = FALSE, ...) {
  x <- ungroup(x)
  x[,,drop=FALSE]
}

#' @export
ungroup.grouped_ffdf <- function(x) {
  class(x) <- "ffdf"
  attr(x,"indices") <- NULL
  attr(x,"vars") <- NULL
  tbl_ffdf(x)
}

#' @export
regroup.grouped_ffdf <- function(x, value) {
  grouped_ffdf(x, unname(value))
}

#' @export
regroup.ffdf <- function(x, value) {
  grouped_ffdf(x, unname(value))
}

### testing...
# ds <- tbl_ffdf(mtcars)
# g <- group_by(ds, cyl)
# group_size(g)
