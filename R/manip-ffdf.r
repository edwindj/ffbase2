#' Data manipulation for ffdf.
#'
#' @param .data a ffdf
#' @param ... variables interpreted in the context of \code{.data}
#' @param inplace if \code{FALSE} (the default) the data frame will be copied
#'   prior to modification to avoid changes propagating via reference.
#' @examples
#' if (require("hflights")) {
#' hflights2 <- tbl_ffdf(hflights)
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
#' @param .env The environment in which to evaluate arguments not included in 
#' the data. The default should suffice for ordinary usage.
filter_.ffdf <- function(.data, ..., .dots){
  dots <- lazyeval::all_dots(.dots, ...)
  expr <- lapply(dots, `[[`, "expr")
  expr <- and_expr(expr)
  # TODO reimplement ffwhich in ffbase2!
  open(.data)
  idx <- ffbase::ffwhich(.data, as.expression(expr), envir=.dots[[1]]$env)
  .data[idx,,drop=FALSE]
}

#' @rdname manip_ffdf
#' @export
filter_.tbl_ffdf <- function(.data, ..., .dots) {
  tbl_ffdf(NextMethod())
}

 #' @rdname manip_ffdf
#' @export
summarise_.ffdf <- function(.data, ..., .dots) {
  #cols <- named_dots(...)
  open(.data)
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  input <- partial_eval(dots, .data)
  
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
summarise_.tbl_ffdf <- function(.data, ...) {
  tbl_ffdf(NextMethod())
}

#' @rdname manip_ffdf
#' @export
mutate_.ffdf <- function(.data, ..., .dots, inplace = FALSE) {
  open(.data)
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  if (!inplace){
    # only clone those vectors that are overwritten
    nms <- names(dots)
    nms <- nms[nms %in% names(.data)]
    if (length(nms)){
      .data[nms] <- clone(.data[nms])
    }
  }
  #TODO improve: transform does not support just defined variables
  eval(substitute(transform(.data, ...)))
}

#' @rdname manip_ffdf
#' @export
mutate_.tbl_ffdf <- function(.data, ...) {
  tbl_ffdf(NextMethod())
}

desc <- function(x){
  if (is.factor(x)){
    x <- as.integer(x)
  }
  0-x
}

#' @rdname manip_ffdf
#' @export
arrange_.ffdf <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  if (length(dots) == 0){
    return(.data)
  }
  fforder_call <- lazyeval::make_call(quote(fforder), dots)
  open(.data)
  idx <- lazyeval::lazy_eval(fforder_call, physical(.data))
  .data[idx,,drop=FALSE]
}

#' @rdname manip_ffdf
#' @export
arrange_.tbl_ffdf <- function(.data, ...) {
  tbl_ffdf(NextMethod())
}

#' @rdname manip_ffdf
#' @export
select_.ffdf <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  vars <- select_vars_(names(.data), dots)
  setNames(.data[vars], names(vars))
}

#' @rdname manip_ffdf
#' @export
select_.tbl_ffdf <- function(.data, ..., .dots) {
  tbl_ffdf(NextMethod())
}

#' @rdname manip_ffdf
#' @export
rename_.ffdf <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  vars <- rename_vars_(names(.data), dots)
  out <- physical(.data)[vars]
  out <- setNames(out, names(vars))
  do.call(ffdf, out)
}


#' @rdname manip_ffdf
#' @export
rename_.tbl_ffdf <- function(.data, ..., .dots){
  tbl_ffdf(NextMethod())
}

#' @rdname manip_ffdf
#' @export
slice_.ffdf <- function(.data, ..., .dots){
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  n <- function() nrow(.data)
  
  dots[] <- lapply(dots, function(dot){
    dot$env <- new.env(parent=dot$env)
    dot$env$n <- n
    dot
  })
  idx <- as.ff(lazyeval::lazy_eval(dots, .data)[[1]])
  .data[idx,, drop=FALSE]
}

#' @rdname manip_ffdf
#' @export
slice_.tbl_ffdf <- function(.data, ..., .dots){
  tbl_ffdf(NextMethod())
}

#' @rdname manip_ffdf
#' @export
do.ffdf <- function(.data, ...) {
  tbl_ffdf(dplyr::do(as.data.frame(.data, ...)))
}

#' @rdname manip_ffdf
#' @export
do.tbl_ffdf <- function(.data, ...) {
  NextMethod()
}
