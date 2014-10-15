#' A grouped ffdf.
#'
#' The easiest way to create a grouped ffdf is to call the \code{group_by}
#' method on a ffdf or data source: this will take care of capturing
#' the unevalated expressions for you.
#'
#' @param data a data source or data frame.
#' @param vars a list of quoted variables.
#' @param is_sorted if \code{data} is sorted on \code{vars} is_sorted should be
#'  set to \code{TRUE} to avoid unnecesary sorting
#' @export
grouped_ffdf <- function(data, vars, is_sorted=FALSE) {
  assert_that(is.ffdf(data))
  is_name <- vapply(vars, is.name, logical(1))
  if (!all(is_name)) {
    stop("Data tables can only be grouped by variables, not expressions",
      call. = FALSE)
  }
  
  vars_s <- deparse_all(vars)
  open(data)
  if (is_sorted){
    .data_sorted <- data
  } else {
    o <- ff::ffdforder(data[vars_s])
    .data_sorted <- data[o,,drop=FALSE]
  }
  
  rles <- lapply(vars_s, function(v){
    v <- .data_sorted[[v]]
    lev <- levels(v)
    if (!is.null(lev)){
      levels(v) <- NULL
    }
    r <- ffbase::rle_ff(v)
    if (!is.null(lev)){
      levels(v) <- lev
    }
    r
  })
  
#   attr(data, "vars") <- vars
#   attr(data, "indices") <- list(order=o, rles=rles, n_groups=rles[[length(rles)]])
  structure( data
           , class = c("grouped_ffdf", "tbl_ffdf", "tbl", "ffdf")
           , vars = vars
           , indices = list( rles=rles
                           , n_groups=rles[[length(rles)]]
                           , data_sorted=.data_sorted
                           )
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

#' @rdname manip_ffdf
#' @export
rename_.grouped_ffdf <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  vars <- rename_vars_(names(.data), dots)
  
  out <- 
    physical(.data)[vars] %>%
    setNames(out, names(vars)) %>%
    do.call(ffdf, .)
  
  
  attr(out, "indices")$data_sorted <-
    data_sorted(.data) %>%
    physical(.)[vars] %>%
    do.call(ffdf, .)  
  
  grouped_ffdf(out, vars=groups(.data))
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
group_by_.ffdf <- function (.data, ..., .dots, add = FALSE) 
{
  groups <- group_by_prepare(.data, ..., .dots = .dots, add = add)
  grouped_ffdf(groups$data, groups$groups)
}

data_sorted <- function(x){
  attr(x, "indices")$data_sorted
}
### testing...
# ds <- tbl_ffdf(mtcars)
# g <- group_by(ds, cyl)
# group_size(g)
