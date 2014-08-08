#' A grouped ffdf.
#'
#' The easiest way to create a grouped ffdf is to call the \code{group_by}
#' method on a ffdf or data source: this will take care of capturing
#' the unevalated expressions for you.
#'
#' @param data a data source or data frame.
#' @param vars a list of quoted variables.
grouped_ffdf <- function(data, vars) {
  stopifnot(is.ffdf(data))

  is_name <- vapply(vars, is.name, logical(1))
  if (!all(is_name)) {
    stop("Data tables can only be grouped by variables, not expressions",
      call. = FALSE)
  }
  data <- list(obj = data, vars = vars)
  structure(data, class = c("grouped_ffdf", "tbl_ffdf", "source"))
}

#' @rdname grouped_ffdf
#' @param x an object to check
#' @export
is.grouped_ffdf <- function(x) inherits(x, "grouped_ffdf")

#' @S3method print grouped_ffdf
print.grouped_ffdf <- function(x, ...) {
  cat("Source: local ffdf ", dim_desc(x), "\n", sep = "")
  cat("Groups: ", dplyr:::commas(deparse_all(x$vars)), "\n", sep = "")
  cat("\n")
  trunc_mat(x)
}

#' @method group_by data.table
#' @export
#' @rdname grouped_ffdf
#' @param ... variables to group by
group_by.data.table <- function(x, ...) {
  vars <- dots(...)
  grouped_ffdf(x, c(x$group_by, vars))
}

#' @method group_by tbl_ffdf
#' @export
#' @rdname grouped_ffdf
group_by.tbl_ffdf <- function(x, ...) {
  vars <- dots(...)
  grouped_ffdf(x$obj, vars)
}

#' @S3method ungroup grouped_ffdf
ungroup.grouped_ffdf <- function(x) {
  tbl_ffdf(x$obj)
}

