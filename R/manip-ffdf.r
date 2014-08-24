#' Data manipulation for ffdf.
#'
#' @param .data a ffdf
#' @param ... variables interpreted in the context of \code{.data}
#' @param inplace if \code{FALSE} (the default) the data frame will be copied
#'   prior to modification to avoid changes propagating via reference.
#' @examples
#' if (require("hflights")) {
#' 
#' filter(hflights2, Month == 1, DayofMonth == 1, Dest == "DFW")
#' head(select(hflights2, Year:DayOfWeek))
#' summarise(hflights2, delay = mean(ArrDelay, na.rm = TRUE), n = length(ArrDelay))
#' head(mutate(hflights2, gained = ArrDelay - DepDelay))
#' head(arrange(hflights2, Dest, desc(ArrDelay)))
#' }
#' @name manip_ffdf
NULL

and_expr <- function(exprs) {
  assert_that(is.list(exprs))
  if (length(exprs) == 0) return(TRUE)
  if (length(exprs) == 1) return(exprs[[1]])

  left <- exprs[[1]]
  for (i in 2:length(exprs)) {
    left <- substitute(left & right, list(left = left, right = exprs[[i]]))
  }
  left
}

#' @rdname manip_ffdf
#' @export
#' @importFrom ffbase ffwhich
filter.ffdf <- function(.data, ..., env=parent.frame()) {
  expr <- and_expr(dots(...))
  idx <- ffwhich(.data, as.expression(expr), envir=env)
  .data[idx, ]
}

#' @rdname manip_ffdf
#' @export
filter.tbl_ffdf <- function(.data, ..., env=parent.frame()) {
  tbl_ffdf(
    filter.ffdf(.data, ..., env=env)
  )
}

 #' @rdname manip_ffdf
#' @export
summarise.ffdf <- function(.data, ...) {
  #cols <- named_dots(...)
  input <- partial_eval(dots(...), .data, parent.frame())
  input <- auto_name(input)
  
  data_env <- list2env(physical(.data), parent = parent.frame())
  data_env$n <- function() nrow(.data)
  
  result <- list()
  
  for (col in names(input)) {
    result[[col]] <- as.ff(eval(input[[col]], data_env))
  }
  
  do.call("ffdf", result)
#   quote
#   l <- list()
#   for (col in names(cols)){
#     a <- substitute(.data$col, list(col=col))
#   }
#   a
}

#' @rdname manip_ffdf
#' @export
summarise.tbl_ffdf <- function(.data, ...) {
  tbl_ffdf(
    summarise.ffdf(.data, ...)
  )
}

#' @rdname manip_ffdf
#' @export
#' @importFrom ffbase transform.ffdf
mutate.ffdf <- function(.data, ..., inplace = FALSE) {
  if (!inplace) .data <- clone(.data)
  eval(substitute(ffbase::transform.ffdf(.data, ...)))
}

#' @rdname manip_ffdf
#' @export
mutate.tbl_ffdf <- function(.data, ...) {
  tbl_ffdf(
    mutate.ffdf(.data, ...)
  )
}

#' @rdname manip_ffdf
#' @export
arrange.ffdf <- function(.data, ...) {
  vars <- select_vars(names(.data), ..., env = parent.frame())
  idx <- ffdforder(.data[vars])
  .data[idx,,drop=FALSE]
}

#' @rdname manip_ffdf
#' @export
arrange.tbl_ffdf <- function(.data, ...) {
  tbl_ffdf(
    arrange.ffdf(.data, ...)
  )
}

#' @rdname manip_ffdf
#' @export
select.ffdf <- function(.data, ...) {
  vars <- select_vars(names(.data), ..., env = parent.frame(),
                      include = as.character(groups(.data)))
  .data[vars]
}

#' @rdname manip_ffdf
#' @export
select.tbl_ffdf <- function(.data, ...) {
  tbl_ffdf(
    select.ffdf(.data, ...)
  )
}

#' @rdname manip_ffdf
#' @export
rename.ffdf <- function(.data, ...) {
  vars <- rename_vars(names(.data), ...)
  # bug in ff: setting names does not changed the physical names so work around it
  l <- physical(.data)
  names(l) <- names(vars)
  do.call(ffdf, l)
}


#' @rdname manip_ffdf
#' @export
do.ffdf <- function(.data, .f, ...) {
  list(.f(.data[,,drop=F], ...))
}

#' @rdname manip_ffdf
#' @export
do.tbl_ffdf <- function(.data, .f, ...) {
  tbl_ffdf(.f(as.data.frame(.data), ...))
}
