library(testthat)
library(mpipe)


library(dplyr)
library(magrittr)


expect_different <- function(object, expected) {
  act <- quasi_label(enquo(object), arg = "object")


  expect(
    !isTRUE(all.equal(act$val, expected)),
    sprintf("%s (= %s) is equal to %s (and shouldn't be).", act$lab, act$val, expected)
  )

  invisible(act$val)
}

expect_file_created <- function(object, filename) {
  act <- quasi_label(enquo(object), arg = "object")

  expect(
    file.exists(filename),
    sprintf("%s was not created by %s", filename, act$lab)
  )

  invisible(act$val)
}

penguins <- palmerpenguins::penguins %>%
  transmute(species, sex, island,
    culmen_length = culmen_length_mm,
    culmen_depth = culmen_depth_mm
  ) %>%
  filter_all(~ !is.na(.))


test_check("mpipe")
