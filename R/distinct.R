#' @importFrom ffbase unique.ffdf
distinct_.ffdf <- function(.data, vars = character()) {
  ffbase::unique.ffdf(.data)
}

#' @importFrom ffbase unique.ffdf
distinct_.tbl_ffdf <- function(.data, vars = character()) {
  ffbase::unique.ffdf(.data)
}

distinct_.grouped_ffdf <- function(.data, vars = character()) {
  grouped_ffdf(distinct_.ffdf(.data, vars = vars), groups(.data))
}