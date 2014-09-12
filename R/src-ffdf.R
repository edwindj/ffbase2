#' Open or create ffdf source
#' 
#' An ffdf source is a directory in which ffdf tables will reside.
#' @param path \code{character} directory path
#' @export
src_ffdf <- function(path, ...){
  if (inherits(path, "src_ffdf")){
    return(path)
  }
  assert_that(is.string(path))
  
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  #temp_path <- file.path(path, ".temp")
  #dir.create(temp_path, showWarnings = FALSE, recursive = TRUE)
  
  structure(
    list(
      path = path
    #, temp_path = temp_path
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
  dirname(list.files(x$path, "schema.Rds", recursive = TRUE))
}

#' @export
tbl.src_ffdf <- function(src, from, ...){
  tbl_ffdf(src=src, name=from, ...)
}

write_schema <- function(x, path, ...){
}

load_tbl <- function(src, name, ...){
  table_schema <- file.path(src$path, name, "schema.Rds")
  tabl <- readRDS(table_schema)
  tabl
}

delete_tbl <- function(src, name, ...){
  try({
    x <- load_tbl(src, name)
    delete(x) # destroy any memorymappings
  }, silent=T)
  table_path <- file.path(src$path, name)
  unlink(table_path, recursive = T, force = TRUE)
}