distinct_.ffdf <- function(.data, vars = character()) {
  unique(.data)
}

distinct_.tbl_ffdf <- function(.data, vars = character()) {
  unique(.data)
}

distinct_.grouped_ffdf <- function(.data, vars = character()) {
  out <- NULL
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  data_s <- data_sorted(.data)
  for (i in grouped_chunks(.data)){
    ch <- grouped_df(data_s[i,,drop=FALSE], groups(.data))
    res <- distinct_(ch, .dots = dots)
    out <- append_to(out, res, check_structure=FALSE)
  }
  out
}