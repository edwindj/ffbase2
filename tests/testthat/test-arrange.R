context("Arrange")

df1 <- expand.grid(
  a = sample(letters, 5),
  b = sample(letters, 5),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE)

df2 <- data.frame(
  a = rep(c(NA, 1, 2, 3), each = 4),
  b = rep(c(0L, NA, 1L, 2L), 4),
  c = c(NA, NA, NA, NA, letters[10:21]),
  d = rep( c(T, NA, F, T), each = 4),
  id = 1:16,
  stringsAsFactors = FALSE
)

df2 <- tbl_ffdf(df2)

equal_df <- function(x, y) {
  rownames(x) <- NULL
  rownames(y) <- NULL
  isTRUE(all.equal(x, y))
}

test_that("local arrange sorts missing values to end", {
  na_last <- function(x) {
    n <- length(x)
    all(is.na(x[(n - 3):n]))
  }
  
  # Numeric
  expect_true(na_last(arrange(df2, a)$a))
  expect_true(na_last(arrange(df2, desc(a))$a))
  
  # Integer
  expect_true(na_last(arrange(df2, b)$b))
  expect_true(na_last(arrange(df2, desc(b))$b))
  
  # Character
  expect_true(na_last(arrange(df2, c)$c))
  expect_true(na_last(arrange(df2, desc(c))$c))
  
  # Logical
  expect_true(na_last(arrange(df2, d)$d))
  expect_true(na_last(arrange(df2, desc(d))$d))
})

test_that("arrange uses the white list", {
  env <- environment()
  Period <- suppressWarnings( setClass("Period", contains = "numeric", where = env) )
  on.exit(removeClass("Period", where = env))
  
  df <- data.frame( p = Period(c(1, 2, 3)), x = 1:3 )
  expect_error(arrange(df, p))
  
})

test_that("arrange handles the case where ... is missing (#338)",{
  expect_equal(arrange(mtcars), mtcars)
})

test_that("arrange implements special case (#369)", {
  d1 <- mtcars %>% as.ffdf() %>% group_by(cyl,disp) %>% arrange() %>% as.data.frame()
  d2 <- mtcars %>% as.ffdf() %>% arrange(cyl, disp) %>% as.data.frame()
  
  expect_equal(d1, d2)
})
