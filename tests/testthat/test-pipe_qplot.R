library(ggplot2)

test_that("returned value is same as input", {
  expect_equal(
    iris %>% pipe_qplot(Sepal.Length, Sepal.Width),
    iris
  )
})

test_that("does it warn when no output expected", {
  expect_warning(iris %>%
    pipe_qplot(Sepal.Length, Sepal.Width, print.plot = F))
})
