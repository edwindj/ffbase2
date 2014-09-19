context("src")

DB = "testdb"

test_that("Create",{
  unlink(DB, recursive = T, force = T)
  db <- src_ffdf(DB)
  expect_true(file.exists(DB), "Directory created?")
})

test_that("Create table",{
  unlink(DB, recursive = T, force = T)
  mtc <- tbl_ffdf(mtcars, DB, name="mtc", force=T)
})