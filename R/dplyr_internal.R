# Borrowing internal functions from dplyr.
# - deparse_all
# - group_by_prepare
# - commas
# - common_by
# - %||%
# - names2
# - check_size
# - check_weight
# - distinct_vars
# Generated with:
# borrow_from_dplyr(deparse_all, group_by_prepare, commas, common_by, 
#     `%||%`, names2, check_size, check_weight, distinct_vars)
deparse_all <-
function (x) 
{
    deparse2 <- function(x) paste(deparse(x, width.cutoff = 500L), 
        collapse = "")
    vapply(x, deparse2, FUN.VALUE = character(1))
}
group_by_prepare <-
function (.data, ..., .dots, add = FALSE) 
{
    new_groups <- lazyeval::all_dots(.dots, ...)
    is_name <- vapply(new_groups, function(x) is.name(x$expr), 
        logical(1))
    has_name <- names2(new_groups) != ""
    needs_mutate <- has_name | !is_name
    if (any(needs_mutate)) {
        .data <- mutate_(.data, .dots = new_groups[needs_mutate])
    }
    new_groups <- lazyeval::auto_name(new_groups)
    groups <- lapply(names(new_groups), as.name)
    if (add) {
        groups <- c(groups(.data), groups)
    }
    groups <- groups[!duplicated(groups)]
    list(data = .data, groups = groups)
}
commas <-
function (...) 
paste0(..., collapse = ", ")
common_by <-
function (by = NULL, x, y) 
{
    if (!is.null(by)) {
        return(list(x = names(by) %||% by, y = unname(by)))
    }
    by <- intersect(tbl_vars(x), tbl_vars(y))
    if (length(by) == 0) {
        stop("No common variables. Please specify `by` param.", 
            call. = FALSE)
    }
    message("Joining by: ", capture.output(dput(by)))
    list(x = by, y = by)
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
distinct_vars <-
function (.data, ..., .dots) 
{
    dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
    needs_mutate <- vapply(dots, function(x) !is.name(x$expr), 
        logical(1))
    if (any(needs_mutate)) {
        .data <- mutate_(.data, .dots = dots[needs_mutate])
    }
    list(data = .data, vars = names(dots))
}
