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
filter_.grouped_ffdf <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  out <- NULL
  data_s <- data_sorted(.data)
  for (i in grouped_chunks(.data)){
    ch <- grouped_df(data_s[i,,drop=FALSE], groups(.data))
    res <- filter_(ch, .dots=dots)
    if (is.null(out)){
      out <- as_ffdf(res)
    } else {
      out <- ffdfappend(out, res)
    }
  }
  grouped_ffdf(
    data = out,
    vars = groups(.data), 
    is_sorted = TRUE
  )
}

#' @rdname manip_grouped_ffdf
#' @export
summarise_.grouped_ffdf <- function(.data, ..., .dots){
  out <- NULL
  dots <- lazyeval::all_dots(.dots, ...)
  data_s <- data_sorted(.data)  
  for (i in grouped_chunks(.data)){
    ch <- grouped_df(data_s[i,,drop=FALSE], groups(.data))
    out <- ffdfappend(out, eval(summarise_q, list(.ch=ch), .env))
  }
  tbl_ffdf(out)
}

#' @rdname manip_grouped_ffdf
#' @export
mutate.grouped_ffdf <- function(.data, ..., inplace = FALSE) {
  #TODO only clone columns that are named in the ...
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
  out <- list()
  do_q <- substitute(do(.ch, ...))
  data_s <- data_sorted(.data)
  .env <- parent.frame()
  for (i in grouped_chunks(.data)){
    ch <- grouped_df(data_s[i,,drop=FALSE], groups(.data))
    out_ch <- eval(do_q, list(.ch=ch), .env)
    out[[length(out)+1]] <- out_ch
  }
  do.call(rbind, out)
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

#   mtcars %>%
#   tbl_ffdf() %>%
#   group_by(cyl) %>% 
#   do(mod = lm(mpg ~ disp, data=.))
