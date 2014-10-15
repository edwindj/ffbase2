context("tally")

test_that("Tally works",{
  expect <- iris %>% tally
  
  object <- tbl_ffdf(iris) %>%  tally %>%  as.data.frame()
  
  expect_equal( tbl_ffdf(iris) %>%  tally %>%  as.data.frame()
              , iris %>% tally)
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