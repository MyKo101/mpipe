



test_that("predicate works implicitly",{

  expect_equal(iris %>%
                 if_branch(TRUE,
                           . %>%
                             filter(Species == "setosa"),
                           . %>%
                             filter(Species == "virginica")) %>%
                 pull(Sepal.Length) %>%
                 mean,
               mean(iris$Sepal.Length[iris$Species=="setosa"]))

  expect_equal(iris %>%
                 if_branch(FALSE,
                           . %>%
                             filter(Species == "setosa"),
                           . %>%
                             filter(Species == "virginica")) %>%
                 pull(Sepal.Length) %>%
                 mean,
               mean(iris$Sepal.Length[iris$Species=="virginica"]))

  expect_equal(iris %>%
                 if_branch(FALSE,
                           . %>%
                             filter(Species == "setosa")) %>%
                 pull(Sepal.Length) %>%
                 mean,
               mean(iris$Sepal.Length))

  f <- function(x)
  {
    x %>%
      if_branch(TRUE,
                . %>%
                  filter(Species == "setosa"),
                . %>%
                  filter(Species == "virginica")) %>%
      pull(Sepal.Length) %>%
      mean

  }

  expect_equal(f(iris),mean(iris$Sepal.Length[iris$Species=="setosa"]))


})

test_that("predicate works for external variable",{

  this_predicate <- TRUE
  expect_equal(iris %>%
                 if_branch(this_predicate,
                           . %>%
                             filter(Species == "setosa"),
                           . %>%
                             filter(Species == "virginica")) %>%
                 pull(Sepal.Length) %>%
                 mean,
               mean(iris$Sepal.Length[iris$Species=="setosa"]))

  f <- function(x)
  {
    this_predicate <- FALSE

    x %>%
      if_branch(this_predicate,
                . %>%
                  filter(Species == "setosa"),
                . %>%
                  filter(Species == "virginica")) %>%
      pull(Sepal.Length) %>%
      mean
  }

  expect_equal(f(iris),mean(iris$Sepal.Length[iris$Species=="virginica"]))


})

test_that("predicate statement is evaluated within data (uses eval_expr)",{

  expect_equal(iris %>%
                 if_branch(nrow(.) > 100,
                           . %>%
                             filter(Species == "setosa"),
                           . %>%
                             filter(Species == "virginica")) %>%
                 pull(Sepal.Length) %>%
                 mean,
               mean(iris$Sepal.Length[iris$Species=="setosa"]))

  expect_equal(iris %>%
                 if_branch(nrow(.) < 100,
                           . %>%
                             filter(Species == "setosa"),
                           . %>%
                             filter(Species == "virginica")) %>%
                 pull(Sepal.Length) %>%
                 mean,
               mean(iris$Sepal.Length[iris$Species=="virginica"]))

  f <- function(x)
  {
    x %>%
      if_branch(nrow(.) < 100,
                . %>%
                  filter(Species == "setosa"),
                . %>%
                  filter(Species == "virginica")) %>%
      pull(Sepal.Length) %>%
      mean
  }

  expect_equal(f(iris),mean(iris$Sepal.Length[iris$Species=="virginica"]))

})

test_that("predicate function is evaluated within data (uses eval_expr)",{

  expect_equal(iris %>%
                 if_branch(. %>%
                             nrow %>%
                             is_greater_than(100),
                           . %>%
                             filter(Species == "setosa"),
                           . %>%
                             filter(Species == "virginica")) %>%
                 pull(Sepal.Length) %>%
                 mean,
               mean(iris$Sepal.Length[iris$Species=="setosa"]))

  expect_equal(iris %>%
                 if_branch(. %>%
                             nrow %>%
                             is_less_than(100),
                           . %>%
                             filter(Species == "setosa"),
                           . %>%
                             filter(Species == "virginica")) %>%
                 pull(Sepal.Length) %>%
                 mean,
               mean(iris$Sepal.Length[iris$Species=="virginica"]))

  f <- function(x)
  {
    x %>%
      if_branch(. %>%
                  nrow %>%
                  is_greater_than(100),
                . %>%
                  filter(Species == "setosa"),
                . %>%
                  filter(Species == "virginica")) %>%
      pull(Sepal.Length) %>%
      mean

  }

  expect_equal(f(iris),mean(iris$Sepal.Length[iris$Species=="setosa"]))

})

test_that("works with enquo-ing of cond",{

  f <- function(x,cond)
  {
    x %>%
      if_branch(!!enquo(cond),
                . %>%
                  filter(Species == "setosa"),
                . %>%
                  filter(Species == "virginica")) %>%
      pull(Sepal.Length) %>%
      mean
  }

  expect_equal(f(iris,nrow(.) < 100),
               mean(iris$Sepal.Length[iris$Species=="virginica"]))


})



























