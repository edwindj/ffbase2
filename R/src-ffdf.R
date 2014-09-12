#' Open or create ffdf source
#' 
#' An ffdf source is a directory in which ffdf tables will reside.
#' @param path \code{character} directory path
#' @export
src_ffdf <- function(path, ...){
  assert_that(is.string(path))
  
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  temp_path <- file.path(path, ".temp")
  dir.create(temp_path, showWarnings = FALSE, recursive = TRUE)
  
  structure(
    list(
      path = path
    , temp_path = temp_path
    )
  , class=c("src_ffdf", "src")
  )
}

#' @export
format.src_ffdf <- function(x, ...){
  tbls <- paste(src_tbls(x), collapse = ", ")
  paste0("src: ffdf ['",x$path,"']\n","tbls: ", tbls)
}

#' @export
src_tbls.src_ffdf <- function(x){
  # need to filter out table names.
  list.dirs(x$path, full.names=FALSE, recursive = FALSE)
}

load_tbl <- function(src, name, ...){
  table_schema <- file.path(src$path, name, "schema.Rds")
  tabl <- readRDS(table_schema)
  tbl_ffdf(tabl)
}

delete_tbl <- function(src, name, ...){
  x <- load_tbl(src, name)
  delete(x) # destroys any memorymappings
  table_path <- file.path(src$path, name)
  unlink(table_path, recursive = T, force = TRUE)
}