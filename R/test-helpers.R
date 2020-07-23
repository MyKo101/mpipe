expect_different <- function(object, expected) {
  act <- testthat::quasi_label(enquo(object), arg = "object")


  testthat::expect(
    !isTRUE(all.equal(act$val, expected)),
    sprintf("%s (= %s) is equal to %s (and shouldn't be).", act$lab, act$val, expected)
  )

  invisible(act$val)
}

expect_file_created <- function(object, filename) {
  act <- testthat::quasi_label(enquo(object), arg = "object")

  testthat::expect(
    file.exists(filename),
    sprintf("%s was not created by %s", filename, act$lab)
  )

  invisible(act$val)
}
