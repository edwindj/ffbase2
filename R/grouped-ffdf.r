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
  attr(data, "vars") <- vars
  
  #TODO create a group ff vector
  indices <- ff::ffdforder(data[as.character(vars)])
  attr(data, "indices") <- indices
  structure(data, class = c("grouped_ffdf", "tbl_ffdf", "tbl", "ffdf"))
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
  stop("Not implemented")
}

#' @export
n_groups.grouped_ffdf <- function(x) {
  length(attr(x, "indices"))
}

#' @export
groups.tbl_ffdf <- function(x) {
  attr(x, "vars")
}

#' @export
as.data.frame.grouped_ffdf <- function(x, row.names = NULL,
                                     optional = FALSE, ...) {
  x <- ungroup(x)
  x[,]
}

#' @export
ungroup.grouped_ffdf <- function(x) {
  stop("ungroup not implemented")
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
# group_by(ds, cyl)
