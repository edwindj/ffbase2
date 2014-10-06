borrow_from_dplyr <- function(...){
  args <- sapply(eval(substitute(alist(...))), deparse)
  f <- file("R/dplyr_internal.R", "wt")
  writeLines("# Borrowing internal functions from dplyr.", f)
  writeLines(paste0("# - ", args), f)
  writeLines("# Generated with:", f)
  writeLines(paste0("# ", deparse(sys.call())), f)
  close(f)
  dump(args, file = "R/dplyr_internal.R", append=TRUE,
       envir=getNamespace("dplyr"))
}

# borrow_from_dplyr( deparse_all, group_by_prepare, commas,
#                    common_by, `%||%`, names2, check_size,
#                    check_weight)
