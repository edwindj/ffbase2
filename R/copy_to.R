#' @export
copy_to.src_ffdf <- function(dest, df, name=deparse(substitute(df)), ...){
  table_path = file.path(dest$path, name)
  dir.create(table_path, recursive=T)
  columns <- lapply(names(df), function(n){
    cl <- df[[n]]
    # ff does not support character vectors.
    if (is.character(cl)){
      cl <- factor(cl)
    }
    col_path <- file.path(table_path, paste0(n, ".ff"))
    as.ff(filename=col_path)
  })
  names(columns) <- names(df)
  res <- do.call(ffdf, columns)
  saveRDS(res, file = file.path(table_path, "schema.Rds"))
  # may be save a yaml schema?
  res
}


auto_copy.tbl_ffdf <- function(x, y, copy=FALSE, ...){
}