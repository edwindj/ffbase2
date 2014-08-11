#' Data manipulation for grouped data tables.
#'
#' @param .data a data table
#' @param ... variables interpreted in the context of \code{.data}
#' @param inplace if \code{FALSE} (the default) the data frame will be copied
#'   prior to modification to avoid changes propagating via reference.
#' @examples
#' if (require("ffbase")) {
#' data("baseball", package = "plyr")
#' baseball_ffdf <- as.ffdf(baseball)
#' players <- group_by(baseball_ffdf, id)
#
#' filter(players, g == max(g))
#' summarise(players, g = mean(g))
#' mutate(players, cyear = year - min(year) + 1)
#' arrange(players, id, desc(year))
#' select(players, id:team)
#'
#' # All manip functions preserve grouping structure, except for summarise
#' # (for hopefully obvious reasons)
#' by_year <- mutate(players, cyear = year - min(year) + 1)
#' summarise(by_year, years = max(cyear))
#'
#' # You can also manually ungroup:
#' arrange(ungroup(by_year), id, year)
#' }
#' @name manip_grouped_ffdf
NULL

#' @rdname manip_grouped_ffdf
#' @export
#' @method filter grouped_ffdf
filter.grouped_ffdf <- function(.data, ...) {
  expr <- and_expr(dots(...))
  stop("Not implemented")
  grouped_ffdf(
    data = out,
    vars = .data$vars
  )
}

#' @rdname manip_grouped_ffdf
#' @export
#' @method summarise grouped_ffdf
summarise.grouped_ffdf <- function(.data, ...){
  # TODO check is .data$vars match current index
  
  cols <- named_dots(...)
  stop("Not implemented")
  grouped_ffdf(
    data = out,
    vars = .data$vars
  )
}

#' @rdname manip_grouped_ffdf
#' @export
#' @method mutate grouped_ffdf
mutate.grouped_ffdf <- function(.data, ..., inplace = FALSE) {
  data <- .data$obj
  keys <- deparse_all(.data$vars)
  if (!inplace) data <- clone(data)
  stop("Not implemented")
  grouped_ffdf(
    data = data,
    vars = .data$vars
  )
}

#' @rdname manip_grouped_ffdf
#' @export
#' @method arrange grouped_ffdf
arrange.grouped_ffdf <- function(.data, ...) {
  stop("Not implemented")
  grouped_ffdf(
    data = out,
    vars = .data$vars
  )
}

#' @rdname manip_grouped_ffdf
#' @export
#' @method select grouped_ffdf
select.grouped_ffdf <- function(.data, ...) {
  stop("Not implemented")
  grouped_ffdf(
    data = out,
    vars = .data$vars
  )
}


#' @export do grouped_ffdf
do.grouped_ffdf <- function(.data, .f, ...) {
  stop("Not implemented")
  eval(call, env)$out
}
