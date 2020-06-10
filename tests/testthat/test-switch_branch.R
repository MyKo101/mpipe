



test_that("case works implicitly with characters", {
  expect_equal(
    iris %>%
      switch_branch("setosa",
        setosa = . %>%
          filter(Species == "setosa"),
        virginica = . %>%
          filter(Species == "virginica")
      ) %>%
      pull(Sepal.Length) %>%
      mean(),
    mean(iris$Sepal.Length[iris$Species == "setosa"])
  )

  expect_equal(
    iris %>%
      switch_branch("virginica",
        setosa = . %>%
          filter(Species == "setosa"),
        virginica = . %>%
          filter(Species == "virginica")
      ) %>%
      pull(Sepal.Length) %>%
      mean(),
    mean(iris$Sepal.Length[iris$Species == "virginica"])
  )

  expect_equal(
    iris %>%
      switch_branch("versicolor",
        setosa = . %>%
          filter(Species == "setosa"),
        virginica = . %>%
          filter(Species == "virginica")
      ) %>%
      pull(Sepal.Length) %>%
      mean(),
    mean(iris$Sepal.Length)
  )

  f <- function(x) {
    x %>%
      switch_branch("setosa",
        setosa = . %>%
          filter(Species == "setosa"),
        virginica = . %>%
          filter(Species == "virginica")
      ) %>%
      pull(Sepal.Length) %>%
      mean()
  }

  expect_equal(f(iris), mean(iris$Sepal.Length[iris$Species == "setosa"]))
})

test_that("case works implicitly with numbers", {
  expect_equal(
    iris %>%
      switch_branch(
        1,
        . %>%
          filter(Species == "setosa"),
        . %>%
          filter(Species == "virginica")
      ) %>%
      pull(Sepal.Length) %>%
      mean(),
    mean(iris$Sepal.Length[iris$Species == "setosa"])
  )

  expect_equal(
    iris %>%
      switch_branch(
        2,
        . %>%
          filter(Species == "setosa"),
        . %>%
          filter(Species == "virginica")
      ) %>%
      pull(Sepal.Length) %>%
      mean(),
    mean(iris$Sepal.Length[iris$Species == "virginica"])
  )

  expect_equal(
    iris %>%
      switch_branch(
        3,
        . %>%
          filter(Species == "setosa"),
        . %>%
          filter(Species == "virginica")
      ) %>%
      pull(Sepal.Length) %>%
      mean(),
    mean(iris$Sepal.Length)
  )

  f <- function(x) {
    x %>%
      switch_branch(
        1,
        . %>%
          filter(Species == "setosa"),
        . %>%
          filter(Species == "virginica")
      ) %>%
      pull(Sepal.Length) %>%
      mean()
  }

  expect_equal(f(iris), mean(iris$Sepal.Length[iris$Species == "setosa"]))
})

test_that("case works for external variable with characters", {
  this_case <- "setosa"
  expect_equal(
    iris %>%
      switch_branch(this_case,
        setosa = . %>%
          filter(Species == "setosa"),
        virginica = . %>%
          filter(Species == "virginica")
      ) %>%
      pull(Sepal.Length) %>%
      mean(),
    mean(iris$Sepal.Length[iris$Species == "setosa"])
  )


  f <- function(x) {
    this_case <- "virginica"
    x %>%
      switch_branch(this_case,
        setosa = . %>%
          filter(Species == "setosa"),
        virginica = . %>%
          filter(Species == "virginica")
      ) %>%
      pull(Sepal.Length) %>%
      mean()
  }

  expect_equal(f(iris), mean(iris$Sepal.Length[iris$Species == "virginica"]))
})

test_that("case works for external variable with numbers", {
  this_case <- 1
  expect_equal(
    iris %>%
      switch_branch(
        this_case,
        . %>%
          filter(Species == "setosa"),
        . %>%
          filter(Species == "virginica")
      ) %>%
      pull(Sepal.Length) %>%
      mean(),
    mean(iris$Sepal.Length[iris$Species == "setosa"])
  )


  f <- function(x) {
    this_case <- 2
    x %>%
      switch_branch(
        this_case,
        . %>%
          filter(Species == "setosa"),
        . %>%
          filter(Species == "virginica")
      ) %>%
      pull(Sepal.Length) %>%
      mean()
  }

  expect_equal(f(iris), mean(iris$Sepal.Length[iris$Species == "virginica"]))
})

test_that("case statement is evaluated within data (uses eval_expr) with characters", {
  expect_equal(
    iris %>%
      switch_branch(names(.)[1],
        Sepal.Length = . %>%
          filter(Species == "setosa"),
        Petal.Width = . %>%
          filter(Species == "virginica")
      ) %>%
      pull(Sepal.Length) %>%
      mean(),
    mean(iris$Sepal.Length[iris$Species == "setosa"])
  )

  expect_equal(
    iris %>%
      switch_branch(names(.)[4],
        Sepal.Length = . %>%
          filter(Species == "setosa"),
        Petal.Width = . %>%
          filter(Species == "virginica")
      ) %>%
      pull(Sepal.Length) %>%
      mean(),
    mean(iris$Sepal.Length[iris$Species == "virginica"])
  )

  expect_equal(
    iris %>%
      switch_branch(names(.)[3],
        Sepal.Length = . %>%
          filter(Species == "setosa"),
        Petal.Width = . %>%
          filter(Species == "virginica")
      ) %>%
      pull(Sepal.Length) %>%
      mean(),
    mean(iris$Sepal.Length)
  )


  f <- function(x) {
    x %>%
      switch_branch(names(.)[1],
        Sepal.Length = . %>%
          filter(Species == "setosa"),
        Petal.Width = . %>%
          filter(Species == "virginica")
      ) %>%
      pull(Sepal.Length) %>%
      mean()
  }

  expect_equal(f(iris), mean(iris$Sepal.Length[iris$Species == "setosa"]))
})

test_that("case statement is evaluated within data (uses eval_expr) with numbers", {
  expect_equal(
    iris %>%
      switch_branch(
        floor(mean(iris$Sepal.Length) - 4),
        . %>%
          filter(Species == "setosa"),
        . %>%
          filter(Species == "virginica")
      ) %>%
      pull(Sepal.Length) %>%
      mean(),
    mean(iris$Sepal.Length[iris$Species == "setosa"])
  )


  f <- function(x) {
    x %>%
      switch_branch(floor(mean(iris$Sepal.Length) - 4),
        Sepal.Length = . %>%
          filter(Species == "setosa"),
        Petal.Width = . %>%
          filter(Species == "virginica")
      ) %>%
      pull(Sepal.Length) %>%
      mean()
  }

  expect_equal(f(iris), mean(iris$Sepal.Length[iris$Species == "setosa"]))
})

test_that("case function is evaluated within data (uses eval_expr) with characters", {
  expect_equal(
    iris %>%
      switch_branch(. %>%
        arrange(Sepal.Length) %>%
        slice(1) %>%
        pull(Species) %>%
        as.character(),
      setosa = . %>%
        filter(Species == "setosa"),
      virginica = . %>%
        filter(Species == "virginica")
      ) %>%
      pull(Sepal.Length) %>%
      mean(),
    mean(iris$Sepal.Length[iris$Species == "setosa"])
  )

  expect_equal(
    iris %>%
      switch_branch(. %>%
        arrange(Sepal.Length) %>%
        slice(150) %>%
        pull(Species) %>%
        as.character(),
      setosa = . %>%
        filter(Species == "setosa"),
      virginica = . %>%
        filter(Species == "virginica")
      ) %>%
      pull(Sepal.Length) %>%
      mean(),
    mean(iris$Sepal.Length[iris$Species == "virginica"])
  )

  expect_equal(
    iris %>%
      switch_branch(. %>%
        arrange(Sepal.Length) %>%
        slice(75) %>%
        pull(Species) %>%
        as.character(),
      setosa = . %>%
        filter(Species == "setosa"),
      virginica = . %>%
        filter(Species == "virginica")
      ) %>%
      pull(Sepal.Length) %>%
      mean(),
    mean(iris$Sepal.Length)
  )


  f <- function(x) {
    x %>%
      switch_branch(. %>%
        arrange(Sepal.Length) %>%
        slice(1) %>%
        pull(Species) %>%
        as.character(),
      setosa = . %>%
        filter(Species == "setosa"),
      virginica = . %>%
        filter(Species == "virginica")
      ) %>%
      pull(Sepal.Length) %>%
      mean()
  }

  expect_equal(f(iris), mean(iris$Sepal.Length[iris$Species == "setosa"]))
})

test_that("case function is evaluated within data (uses eval_expr) with numbers", {
  expect_equal(
    iris %>%
      switch_branch(
        . %>%
          arrange(Sepal.Length) %>%
          slice(1) %>%
          pull(Species) %>%
          as.numeric(),
        . %>%
          filter(Species == "setosa"),
        . %>%
          filter(Species == "virginica")
      ) %>%
      pull(Sepal.Length) %>%
      mean(),
    mean(iris$Sepal.Length[iris$Species == "setosa"])
  )

  expect_equal(
    iris %>%
      switch_branch(
        . %>%
          arrange(Sepal.Length) %>%
          slice(150) %>%
          pull(Species) %>%
          as.numeric(),
        . %>%
          filter(Species == "setosa"),
        . %>%
          filter(Species == "virginica")
      ) %>%
      pull(Sepal.Length) %>%
      mean(),
    mean(iris$Sepal.Length)
  )

  expect_equal(
    iris %>%
      switch_branch(
        . %>%
          arrange(Sepal.Length) %>%
          slice(75) %>%
          pull(Species) %>%
          as.numeric(),
        . %>%
          filter(Species == "setosa"),
        . %>%
          filter(Species == "virginica")
      ) %>%
      pull(Sepal.Length) %>%
      mean(),
    mean(iris$Sepal.Length[iris$Species == "virginica"])
  )


  f <- function(x) {
    x %>%
      switch_branch(
        . %>%
          arrange(Sepal.Length) %>%
          slice(1) %>%
          pull(Species) %>%
          as.numeric(),
        . %>%
          filter(Species == "setosa"),
        . %>%
          filter(Species == "virginica")
      ) %>%
      pull(Sepal.Length) %>%
      mean()
  }

  expect_equal(f(iris), mean(iris$Sepal.Length[iris$Species == "setosa"]))
})

test_that("works with enquo-ing of case with characters", {
  f <- function(x, cond) {
    x %>%
      switch_branch(!!enquo(cond),
        Sepal.Length = . %>%
          filter(Species == "setosa"),
        Petal.Width = . %>%
          filter(Species == "virginica")
      ) %>%
      pull(Sepal.Length) %>%
      mean()
  }

  expect_equal(
    f(iris, names(.)[1]),
    mean(iris$Sepal.Length[iris$Species == "setosa"])
  )
})

test_that("works with enquo-ing of case with numbers", {
  f <- function(x, cond) {
    x %>%
      switch_branch(!!enquo(cond),
        Sepal.Length = . %>%
          filter(Species == "setosa"),
        Petal.Width = . %>%
          filter(Species == "virginica")
      ) %>%
      pull(Sepal.Length) %>%
      mean()
  }

  expect_equal(
    f(iris, floor(mean(iris$Sepal.Length) - 4)),
    mean(iris$Sepal.Length[iris$Species == "setosa"])
  )
})

test_that("warning is thrown when no match is found", {
  expect_warning(iris %>%
    switch_branch("versicolor",
      setosa = . %>%
        filter(Species == "setosa"),
      virginica = . %>%
        filter(Species == "virginica"),
      warn = T
    ))

  expect_warning(iris %>%
    switch_branch(3,
      . %>%
        filter(Species == "setosa"),
      . %>%
        filter(Species == "virginica"),
      warn = T
    ))

  f <- function(x) {
    x %>%
      switch_branch("versicolor",
        setosa = . %>%
          filter(Species == "setosa"),
        virginica = . %>%
          filter(Species == "virginica"),
        warn = T
      )
  }

  expect_warning(f(iris))
})

test_that("error is thrown when evaluated as incompatible", {
  expect_error(iris %>%
    switch_branch(TRUE,
      setosa = . %>%
        filter(Species == "setosa"),
      virginica = . %>%
        filter(Species == "virginica"),
      warn = T
    ))

  f <- function(x) {
    x %>%
      switch_branch(TRUE,
        setosa = . %>%
          filter(Species == "setosa"),
        virginica = . %>%
          filter(Species == "virginica"),
        warn = T
      )
  }

  expect_error(f(iris))
})
