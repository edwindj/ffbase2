# Borrowing internal functions from dplyr.
# - dots
# - commas
# - named_dots
# - deparse_all
# - auto_name
# - auto_names
# - common_by
# - %||%
# - sample_n_basic
# - names2
# Generated with:
# borrow_from_dplyr(dots, commas, named_dots, deparse_all, auto_name, 
#     auto_names, common_by, `%||%`, sample_n_basic, names2)
dots <-
function (...) 
{
    eval(substitute(alist(...)))
}
commas <-
function (...) 
paste0(..., collapse = ", ")
named_dots <-
function (...) 
{
    auto_name(dots(...))
}
deparse_all <-
function (x) 
{
    deparse2 <- function(x) paste(deparse(x, width.cutoff = 500L), 
        collapse = "")
    vapply(x, deparse2, FUN.VALUE = character(1))
}
auto_name <-
function (x) 
{
    names(x) <- auto_names(x)
    x
}
auto_names <-
function (x) 
{
    nms <- names2(x)
    missing <- nms == ""
    if (all(!missing)) 
        return(nms)
    deparse2 <- function(x) paste(deparse(x, 500L), collapse = "")
    defaults <- vapply(x[missing], deparse2, character(1), USE.NAMES = FALSE)
    nms[missing] <- defaults
    nms
}
common_by <-
function (x, y) 
{
    by <- intersect(tbl_vars(x), tbl_vars(y))
    message("Joining by: ", capture.output(dput(by)))
    by
}
`%||%` <-
function (x, y) 
if (is.null(x)) y else x
sample_n_basic <-
function (tbl, size, replace = FALSE, weight = NULL) 
{
    n <- nrow(tbl)
    weight <- check_weight(weight, n)
    assert_that(is.numeric(size), length(size) == 1, size >= 
        0)
    check_size(size, n, replace)
    idx <- sample.int(n, size, replace = replace, prob = weight)
    tbl[idx, , drop = FALSE]
}
names2 <-
function (x) 
{
    names(x) %||% rep("", length(x))
}
