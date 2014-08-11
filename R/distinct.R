#' @export
#' @importFrom ffbase unique.ffdf
distinct_.ffdf <- function(.data, vars = character()) {
  ffbase::unique.ffdf(.data)
}

#' @export
#' @importFrom ffbase unique.ffdf
distinct_.tbl_ffdf <- function(.data, vars = character()) {
  ffbase::unique.ffdf(.data)
}

#' @export
distinct_.grouped_ffdf <- function(.data, vars = character()) {
  grouped_ffdf(distinct_.ffdf(.data, vars = vars), groups(.data))
}