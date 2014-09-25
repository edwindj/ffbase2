context("tally")

test_that("Tally works",{
  iris_f <- tbl_ffdf(iris)
  expect_equal( as.data.frame(tally(iris_f))
              , tally(iris))
})

test_that("Grouped tally works",{
  expect <- 
    iris %>%
    group_by(Species) %>%
    tally
  object <- 
    tbl_ffdf(iris) %>%
    group_by(Species) %>%
    tally %>%
    as.data.frame()
  
  expect_equal( object, expect)
})