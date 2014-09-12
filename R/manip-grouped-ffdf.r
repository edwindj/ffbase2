#' Data manipulation for grouped data tables.
#'
#' @param .data a data table
#' @param ... variables interpreted in the context of \code{.data}
#' @param inplace if \code{FALSE} (the default) the data frame will be copied
#'   prior to modification to avoid changes propagating via reference.
#' @examples
#' species <- 
#'   iris %>% tbl_ffdf %>%
#'   group_by(Species)
#
#' filter(species, Petal.Width == max(Petal.Width))
#' summarise(species, Petal.Width=mean(Petal.Width))
#'
#' @name manip_grouped_ffdf
NULL

#' @rdname manip_grouped_ffdf
#' @importFrom ffbase ffappend 
#' @importFrom ffbase ffwhich
#' @export
filter.grouped_ffdf <- function(.data, ...) {
  expr <- and_expr(dots(...))
  
  out <- NULL
  filter_q <- substitute(filter(ch, expr))
  data_s <- data_sorted(.data)  
  .env <- parent.frame()
  for (i in grouped_chunks(.data)){
    ch <- grouped_df(data_s[i,,drop=FALSE], groups(.data))
    out <- ffdfappend(out, eval(filter_q, list(ch=ch), .env))
  }
  grouped_ffdf(
    data = out,
    vars = groups(.data), 
    is_sorted = TRUE
  )
}

#' @rdname manip_grouped_ffdf
#' @export
summarise.grouped_ffdf <- function(.data, ...){
  out <- NULL
  # TODO filter out unneeded variables...
  summarise_q <- substitute(summarise(.ch, ...))
  data_s <- data_sorted(.data)  
  .env <- parent.frame()
  for (i in grouped_chunks(.data)){
    ch <- grouped_df(data_s[i,,drop=FALSE], groups(.data))
    out <- ffdfappend(out, eval(summarise_q, list(.ch=ch), .env))
  }
  out
}

#' @rdname manip_grouped_ffdf
#' @export
mutate.grouped_ffdf <- function(.data, ..., inplace = FALSE) {
  if (!inplace) .data <- clone(.data)
  out <- NULL
  mutate_q <- substitute(mutate(.ch, ...))
  data_s <- data_sorted(.data)  
  .env <- parent.frame()
  for (i in grouped_chunks(.data)){
    ch <- grouped_df(data_s[i,,drop=FALSE], groups(.data))
    out <- ffdfappend(out, eval(mutate_q, list(.ch=ch), .env))
  }
  grouped_ffdf(
    data = out,
    vars = groups(.data),
    is_sorted = TRUE
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
    vars = groups(.data), 
    is_sorted = TRUE
  )
}

#' @export
do.grouped_ffdf <- function(.data, ...) {
  out <- NULL
  do_q <- substitute(do(.ch, ...))
  data_s <- data_sorted(.data)
  .env <- parent.frame()
  for (i in grouped_chunks(.data)){
    ch <- grouped_df(data_s[i,,drop=FALSE], groups(.data))
    out <- ffbase::ffdfappend(out, eval(do_q, list(.ch=ch), .env))
  }
  grouped_ffdf(
    data = out,
    vars = groups(.data),
    is_sorted = TRUE
  )
}

### testing...

# data("baseball", package = "plyr")
# players <-
# tbl_ffdf(baseball) %>%
# group_by(id) %>%
# filter(g==max(g))
# summarise(players, g = mean(g))
# mutate(players, year = year - min(year) + 1)
# arrange(players, id, desc(year))
# select(players, id:team)
#'
#' # All manip functions preserve grouping structure, except for summarise
#' # (for hopefully obvious reasons)
#' by_year <- mutate(players, cyear = year - min(year) + 1)
#' summarise(by_year, years = max(cyear))
#'
#' # You can also manually ungroup:
#' arrange(ungroup(by_year), id, year)