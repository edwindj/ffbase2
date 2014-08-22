# quick borrowing from dplyr

dots <- dplyr:::dots
commas <- dplyr:::commas
named_dots <- dplyr:::named_dots
deparse_all <- dplyr:::deparse_all
auto_name <- dplyr:::auto_name
auto_names <- dplyr:::auto_names
`%||%` <- dplyr:::`%||%`
common_by <- dplyr:::common_by

borrow_from_dplyr <- function(...){
  args <- deparse(substitute(list(...)))
  ns <- getNamespace("dplyr")
  dump(args, envir=getNamespace("dplyr"))
}

#borrow_from_dplyr(dots, commas, named_dots, deparse_all)

# right now do it naively
build_index <- function(group){
  idx <- fforder(group)
  
  rngs <- lapply(chunk(idx), function(i){
    r <- intrle(group[idx[i]])
    cbind(lengths=r$lengths,values=r$values)
  })
  rngs <- do.call(rbind, rngs)
  rngs <- structure(list(lengths=rngs[,1], values=rngs[,2]), class="rle")
  list(idx, ranges=rngs)
}

# quick testing....
# set.seed(42)
# group <- ff(sample(2, size=10, replace=TRUE))
# group[]
# build_index(group)