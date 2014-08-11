context("tbl_ffdf")

test_that("tbl works",{
  ds <- tbl_ffdf(mtcars)
  expect_equal(class(ds), c("tbl_ffdf", "tbl", "ffdf"))
})

test_that("as.tbl works",{
  f <- as.ffdf(mtcars)
  ds <- as.tbl(f)
  expect_equal(class(ds), c("tbl_ffdf", "tbl", "ffdf"))
})