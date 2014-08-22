#' Join ffdf tbls.
#'
#' See \code{\link[dplyr]{join}} for a description of the general purpose of the
#' functions.
#'
#' @inheritParams join
#' @param x,y tbls to join
#' @param ... Included for compatibility with generic; otherwise ignored.
#' @examples
#' if (require("RSQLite") && require("RSQLite.extfuns")) {
#' data("Batting", package = "Lahman")
#' data("Master", package = "Lahman")
#'
#' batting_ffdf <- tbl_ffdf(Batting)
#' person_ffdf <- tbl_ffdf(Master)
#'
#' # Inner join: match batting and person data
#' inner_join(batting_ffdf, person_ffdf)
#'
#' # Left join: keep batting data even if person missing
#' left_join(batting_ffdf, person_ffdf)
#'
#' # Semi-join: find batting data for top 4 teams, 2010:2012
#' grid <- expand.grid(
#'   teamID = c("WAS", "ATL", "PHI", "NYA"),
#'   yearID = 2010:2012)
#' top4 <- semi_join(batting_ffdf, grid, copy = TRUE)
#'
#' # Anti-join: find batting data with out player data
#' anti_join(batting_ffdf, person_ffdf)
#' }
#' @name join.tbl_ffdf
NULL

#' @export
#' @rdname join.tbl_ffdf
inner_join.ffdf <- function(x , y, by=NULL, copy = FALSE, ...){
  by <- by %||% common_by(x, y)
  if (!length(by)) 
    stop("no common variables")
  
  if (!is.ffdf(y)){
    y <- tbl_ffdf(y)
  }
  res <- NULL
  for (j in chunk(y)){
    y_chunk <- y[j,]
    for (i in chunk(x)){
      res <- ffdfappend(res, inner_join(x[i,], y_chunk))
    }
  }
  if (!is.null(res))
      tbl_ffdf(res)
}

#' @export
#' @rdname join.tbl_ffdf
left_join.ffdf  <- function(x,y,by, copy=FALSE, ...){  
}

#' @export
#' @rdname join.tbl_ffdf
semi_join.ffdf  <- function(x, y, by, ...){
}

#' @export
#' @rdname join.tbl_ffdf
anti_join.ffdf <- function(x, y, by, ...){
}


## testing
# iris_ffdf <- tbl_ffdf(iris)
# inner_join(iris_ffdf, iris)
