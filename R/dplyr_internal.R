# Borrowing internal functions from dplyr.
# - dots
# - commas
# - named_dots
# - deparse_all
# - auto_name
# - auto_names
# - common_by
# - %||%
# - names2
# - check_size
# - check_weight
# Generated with:
# borrow_from_dplyr(dots, commas, named_dots, deparse_all, auto_name, 
#     auto_names, common_by, `%||%`, names2, check_size, check_weight)
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
names2 <-
function (x) 
{
    names(x) %||% rep("", length(x))
}
check_size <-
function (size, n, replace = FALSE) 
{
    if (size <= n || replace) 
        return()
    stop("Sample size (", size, ") greater than population size (", 
        n, ").", " Do you want replace = TRUE?", call. = FALSE)
}
check_weight <-
function (x, n) 
{
    if (is.null(x)) 
        return()
    if (!is.numeric(x)) {
        stop("Weights must be numeric", call. = FALSE)
    }
    if (any(x < 0)) {
        stop("Weights must all be greater than 0", call. = FALSE)
    }
    if (length(x) != n) {
        stop("Weights must be same length as data (", n, ")", 
            call. = FALSE)
    }
    x/sum(x)
}
