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
#' @export
filter_.grouped_ffdf <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  out <- NULL
  data_s <- data_sorted(.data)
  for (i in grouped_chunks(.data)){
    ch <- grouped_df(data_s[i,,drop=FALSE], groups(.data))
    res <- filter_(ch, .dots=dots)
    out <- append_to(out, res, check_structure=FALSE)
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
    res <- summarise_(ch, .dots = dots)
    out <- append_to(out, res, check_structure=FALSE)
  }
  out
}

#' @rdname manip_grouped_ffdf
#' @export
mutate.grouped_ffdf <- function(.data, ..., inplace = FALSE) {
  #TODO only clone columns that are named in the ...
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  if (!inplace){
    # only clone those vectors that are overwritten
    nms <- names(dots)
    nms <- nms[nms %in% names(.data)]
    if (length(nms)){
      .data[nms] <- clone(.data[nms])
    }
  }
  out <- NULL
  data_s <- data_sorted(.data)  
  for (i in grouped_chunks(.data)){
    ch <- grouped_df(data_s[i,,drop=FALSE], groups(.data))
    res <- mutate_(ch, .dots = dots)
    out <- append_to(out, res, check_structure=FALSE)
  }
  grouped_ffdf(
    data = out,
    vars = groups(.data),
    is_sorted = TRUE
  )
}

#' @rdname manip_grouped_ffdf
#' @export
arrange_.grouped_ffdf <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  
  if (length(dots) == 0){
    return(.data)
  }
  groups <- lazyeval::as.lazy_dots(groups(.data), env = lazyeval::common_env(dots))
  fforder_call <- lazyeval::make_call(quote(fforder), c(groups, dots))
  open(.data)
  idx <- lazyeval::lazy_eval(fforder_call, physical(.data))
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

#' @rdname manip_grouped_ffdf
#' @export
slice_.grouped_ffdf <- function(.data, ..., .dots){
  out <- NULL
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  data_s <- data_sorted(.data)
  for (i in grouped_chunks(.data)){
    ch <- grouped_df(data_s[i,,drop=FALSE], groups(.data))
    res <- slice_(ch, .dots = dots)
    out <- append_to(out, res, check_structure=FALSE)    
  }
  out
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
