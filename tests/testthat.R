library(testthat)
library(mpipe)


library(dplyr)
library(magrittr)

expect_different <- function(a, b) {
  expect_false(isTRUE(all.equal(a, b)))
}

test_check("mpipe")
