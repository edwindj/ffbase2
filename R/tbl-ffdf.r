#' Create a ffdf tbl object
#'
#' A ffdf source wraps a local ffdf.
#'
#' @export
#' @param data a ffdf data.frame, will be converted to ffdf using as.ffdf
#' @examples
#' ds <- tbl_ffdf(mtcars)
#' ds
tbl_ffdf <- function(data) {
  if (is.grouped_ffdf(data)) return(ungroup(data))
  
  if (!is.ffdf(data)){
    for (n in names(data)){
      if (is.character(data[[n]])){
        data[[n]] <- factor(data[[n]])
      }
    }
    data <- as.ffdf(data)
    # needed otherwise ff will start to act strangely
    rownames(data) <- NULL
  } 
  structure(data, class = c("tbl_ffdf", "tbl", class(data)))
}

#' @export
as.tbl.ffdf <- function(x, ...) {
  tbl_ffdf(x)
}

#' @export
tbl_vars.tbl_ffdf <- function(x) names(x)

#' @export
tbl_vars.ffdf <- function(x) names(x)

# Standard data frame methods --------------------------------------------------

#' Coerce data table to source.
#'
#' @export
#' @keywords internal
as.ffdf.tbl_ffdf <- function(x, keep.rownames = NULL) {
  if (!is.null(keep.rownames)) {
    warning("keep.rownames argument ignored", call. = FALSE)
  }
  x
}

#' @export as.data.frame tbl_ffdf
as.data.frame.tbl_ffdf <- function(x, row.names = NULL, optional = FALSE, ...) {
  if (!is.null(row.names)) warning("row.names argument ignored", call. = FALSE)
  if (!identical(optional, FALSE)) warning("optional argument ignored", call. = FALSE)
  as.data.frame.ffdf(x, ...)
}

#' @export print tbl_ffdf
print.tbl_ffdf <- function(x, ...) {
  cat("Source:     ffdf ", dim_desc(x), "\n", sep = "")
  cat("\n")
  # TODO add head and tail to ffdf
  trunc_mat(x)
}

#' @export head tbl_ffdf
head.tbl_ffdf <- function(x, n=6L, ...) x[seq_len(n), ] # NOTE no negative n supported!

#' @export tail tbl_ffdf
tail.tbl_ffdf <- function(x, n=6L, ...) tail(x, n=n, ...)