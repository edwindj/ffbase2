#' Open or create ffdf source
#' 
#' An ffdf source is a directory in which ffdf tables will reside.
#' @param path \code{character} directory path
#' @export
src_ffdf <- function(path, ...){
  assert_that(is.string(path))
  
  dir.create(path, recursive = TRUE)
  temp_path <- file.path(path, ".temp")
  dir.create(temp_path, recursive = TRUE)
  
  structure(
    list(
      path = path
    , temp_path = temp_path
    )
  , class=c("src_ffdf", "src")
  )
}

src_tbls.ffdf <- function(x){
  # need to filter out table names.
  list.dirs(x$path, full.names=FALSE, recursive = FALSE)
}
