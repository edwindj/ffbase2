#' @export
distinct_.ffdf <- function(.data, vars = character()) {
}

#' @export
distinct_.tbl_ffdf <- function(.data, vars = character()) {
}

#' @export
distinct_.grouped_ffdf <- function(.data, vars = character()) {
  grouped_ffdf(distinct_.ffdf(.data, vars = vars), groups(.data))
}