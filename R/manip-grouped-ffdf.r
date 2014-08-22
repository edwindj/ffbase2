#' Data manipulation for grouped data tables.
#'
#' @param .data a data table
#' @param ... variables interpreted in the context of \code{.data}
#' @param inplace if \code{FALSE} (the default) the data frame will be copied
#'   prior to modification to avoid changes propagating via reference.
#' @examples
#' data("baseball", package = "plyr")
#' baseball_ffdf <- as.ffdf(baseball)
#' players <- group_by(baseball_ffdf, id)
#
#' filter(players, g == max(g))
#' summarise(players, g = mean(g))
#' mutate(players, year = year - min(year) + 1)
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
#' @name manip_grouped_ffdf
NULL

#' @rdname manip_grouped_ffdf
#' @export
filter.grouped_ffdf <- function(.data, ..., env=parent.frame()) {
  expr <- and_expr(dots(...))
  groups <- group_size(.data)
  data_s <- data_sorted(.data)
  
  end <- cumsum(groups)
  begin <- head(c(1, end+1), -1)
  res_idx <- NULL
  
  for (i in seq_along(groups)){
    .data_w <- get_window(data_s, begin[i], end[i])
    idx <- ffwhich(.data_w, as.expression(expr), envir = env)
    idx[, add=TRUE] <- begin[i] - 1L
    res_idx <- ffappend(res_idx, idx)
  }
  grouped_ffdf(
    data = data_s[res_idx,,drop=FALSE],
    vars = groups(.data)
  )
}

#' @rdname manip_grouped_ffdf
#' @export
summarise.grouped_ffdf <- function(.data, ...){
  # TODO check is .data$vars match current index  
  cols <- named_dots(...)
  stop("Not implemented")
  grouped_ffdf(
    data = out,
    vars = groups(.data)
  )
}

#' @rdname manip_grouped_ffdf
#' @export
mutate.grouped_ffdf <- function(.data, ..., inplace = FALSE) {
  if (!inplace) .data <- clone(.data)
  stop("Not implemented")
  grouped_ffdf(
    data = .data,
    vars = groups(.data)
  )
}

#' @rdname manip_grouped_ffdf
#' @export
arrange.grouped_ffdf <- function(.data, ...) {
  vars <- select_vars(names(.data), ..., env = parent.frame(),
                      include = as.character(groups(.data)))
  idx <- ffdforder(.data[vars])
  grouped_ffdf(
    data = .data[idx,,drop=FALSE],
    vars = groups(.data)
  )
}

#' @export
do.grouped_ffdf <- function(.data, .f, ...) {
}

### testing...
# ds <- tbl_ffdf(mtcars)
# g <- group_by(ds, cyl)
# filter(g, gear == max(gear))
#arrange(g, am, carb)
