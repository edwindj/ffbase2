#' @export
copy_to.src_ffdf <- function( dest, df, name=deparse(substitute(df)), force=FALSE,                              ...){
  table_path = file.path(dest$path, name)
  
  if (file.exists(table_path)){
    if (force){
      x <- src_load_tbl(dest, name)
      delete(x)
      unlink(table_path, recursive = T)
    } else {
      stop("Directory: '", table_path
           , "' already exists. Use 'force=TRUE' or remove dir manually")
    }
  }
  dir.create(table_path, showWarnings = FALSE,recursive=T)
  columns <- lapply(names(df), function(n){
    cl <- df[[n]]
    # ff does not support character vectors.
    if (is.character(cl)){
      cl <- factor(cl)
    }
    col_path <- file.path(table_path, paste0(n, ".ff"))
    as.ff(cl, filename=col_path)
  })
  names(columns) <- names(df)
  res <- do.call(ffdf, columns)
  
  saveRDS(res, file = file.path(table_path, "schema.Rds"))
  # may be save a yaml schema?
  close(res)
  tbl_ffdf(res)
}

auto_copy.tbl_ffdf <- function(x, y, copy=FALSE, ...){
}
