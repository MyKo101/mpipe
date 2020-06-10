
e1 <- new.env()
e1$x <- 1:10
e1$y <- "hello world"

e2 <- copy_env(e1)


test_that("check environment contents stay the same",{
  expect_equal(copy_env(e1),e1)
  expect_equal(e1,e2)
})

test_that("check environment locations are the different",{
  expect_false(isTRUE(all.equal(format(e1),format(e2))))
})



f <- function(y){
  x <- y
  function() x
}

g1 <- f(10)

g2 <- copy_fun(g1)

eg1 <- environment(g1)
eg2 <- environment(g2)


test_that("check that the functions are equal",{
  expect_equal(g1,g2)
})

test_that("check environment contents the same",{
  expect_equal(copy_env(eg1),eg1)
  expect_equal(eg1,eg2)
})

test_that("check environment locations are the different",{
  expect_different(format(eg1),format(eg2))
})
