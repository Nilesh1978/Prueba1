library(testthat)
library(Prueba1)

test_that("Error in the output of make_filename",{
  year<-2000
  x<-make_filename(year)
  expect_that(is.vector(x),is_true())
})
