#' Create a ffdf tbl object
#'
#' This wraps a 'normal' ffdf object so it can be used with dplyr.
#' It also allows for storing ffdf object in directories/src or retrieving
#' a specific ffdf from a source.
#' 
#' When \code{data} and \code{src} are specified a \code{\link{copy_to}} 
#' will be executed.
#' When \code{src} and \code{from} are specified an \code{ffdf} will be loaded
#' from disk.
#' When \code{data} is specified without \code{src} a temporary ffdf will be created
#' in \code{fftempdir}.
#' @export
#' @param data a ffdf data.frame, will be converted to ffdf using as.ffdf
#' @param src (optional), if a directory name is specified then the ffdf will be 
#' saved there
#' @param name table to be loaded
#' @examples
#' ds <- tbl_ffdf(mtcars)
#' ds
#' @rdname tbl-ffdf
tbl_ffdf <- function(data, src=getOption("fftempdir"), name=deparse(substitute(data)), ...) {
  src_f <- src_ffdf(src)
  
  if (!missing(src)){
    if (!missing(data)){
      copy_to.src_ffdf(src_f, data, name=name, ... )
    }
    data = load_tbl(src_f, name)
  } else {
    name <- NULL
  }
  
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
  structure( data, class = c("tbl_ffdf", "tbl", class(data))
           , src = src_f
           , name = name)
}

#' @export
#' @rdname tbl-ffdf
as.tbl.ffdf <- function(x, ...) {
  tbl_ffdf(x)
}

#' @export
#' @rdname tbl-ffdf
tbl_vars.tbl_ffdf <- function(x) names(x)

#' @export
#' @rdname tbl-ffdf
tbl_vars.ffdf <- function(x) names(x)

# Standard data frame methods --------------------------------------------------

#' @export
#' @rdname tbl-ffdf
#' @inheritParams base::as.data.frame
as.data.frame.tbl_ffdf <- function(x, row.names = NULL, optional = FALSE, ...) {
  if (!is.null(row.names)) warning("row.names argument ignored", call. = FALSE)
  if (!identical(optional, FALSE)) warning("optional argument ignored", call. = FALSE)
  x[,,drop=FALSE]
}

#' @export
#' @rdname tbl-ffdf
print.tbl_ffdf <- function(x, ..., n=NULL) {
  open(x) # prevent screen printing
  src <- attr(x, "src")
  cat("Source:     ffdf ('",src$path,"/",attr(x, "name", exact=TRUE),"') ", dim_desc(x), "\n", sep = "")
  cat("\n")
  trunc_mat(x, n=n)
  close(x)
}

#' @rdname tbl-ffdf
#' @export
head.tbl_ffdf <- function(x, n=6L, ...) x[seq_len(n),, drop=FALSE] # NOTE no negative n supported!

#' @export
#' @rdname tbl-ffdf
#' @importFrom utils tail
tail.tbl_ffdf <- function(x, n=6L, ...) tail(x, n=n, ...)