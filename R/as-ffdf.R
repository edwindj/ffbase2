#' Convert a data.frame to a ffdf object
#' 
#' Improved version of as.ffdf:
#' - Automatically converts characters to factors: ff does not support character
#'   vectors
#' - allow for zero row data.frames
#' @export
#' @param x a data.frame or coercable to a data.frame
#' @param character_to_factor \code{logical} will coerce a character to factor, 
#' since those types cannot be stored in \code{ff}.
#' @param ... given to as.ffdf
as_ffdf <- function(x, character_to_factor=TRUE, ...){
  if (is.ffdf(x)){
    return(x)
  }
  
  if (!is.data.frame(x)){
    x <- as.data.frame(x)
  }
  
  is_char <- sapply(x, is.character)
  if (any(is_char)){
    if (!isTRUE(character_to_factor)){
      stop("'x' contains chararacter columns. 
           Set 'character_to_factor' to TRUE to convert 'x'", call.=FALSE)
    }
    x[is_char] <- lapply(x[is_char], factor)
  }
  as.ffdf(x, ...)
}

# test
# rm(iris)
# ir <- as_ffdf(iris)
# iris$Species <- as.character(iris$Species)
# ir <- as_ffdf(iris)
