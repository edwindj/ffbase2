#' Open or create ffdf source
#' 
#' An ffdf source is a directory in which ffdf tables will reside.
#' @param path \code{character} directory path
#' @export
src_ffdf <- function(path, ...){
  assert_that(is.string(path))
  
  dir.create(path, recursive = TRUE)
  
  structure(list(path=path), class=c("src_ffdf", "src"))
}

src_tbls.ffdf <- function(x){
  list.dirs(x$path, full.names=FALSE, recursive = FALSE)
}
