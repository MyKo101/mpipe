
e1 <- new.env()
e1$x <- 1:10
e1$y <- "hello world"

e2 <- copy_env(e1)


test_that("check environments are the same",{
  expect_equal(copy_env(e1),e1)
  expect_equal(e1,e2)
})
