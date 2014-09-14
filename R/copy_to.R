#' @export
#' @importFrom ff as.ff
copy_to.src_ffdf <- function( dest, df, name=deparse(substitute(df)), force=FALSE,                              ...){
  table_path = file.path(dest$path, name)
  
  if (file.exists(table_path)){
    if (force){
      delete_tbl(dest, name)
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
    ff::as.ff(cl, filename=col_path, overwrite=T)
  })
  names(columns) <- names(df)
  res <- do.call(ffdf, columns)
  close(res)
  
  saveRDS(res, file = file.path(table_path, "schema.Rds"))
  # may be save a yaml schema?
  tbl(dest, from=name)
}

