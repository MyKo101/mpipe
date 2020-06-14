



test_that("case works implicitly with characters", {
  expect_equal(
    penguins %>%
      switch_branch("Gentoo",
        Gentoo = . %>%
          filter(species == "Gentoo"),
        Adelie = . %>%
          filter(species == "Adelie")
      ) %>%
      pull(culmen_length) %>%
      mean(),
    mean(penguins$culmen_length[penguins$species == "Gentoo"])
  )

  expect_equal(
    penguins %>%
      switch_branch("Adelie",
        Gentoo = . %>%
          filter(species == "Gentoo"),
        Adelie = . %>%
          filter(species == "Adelie")
      ) %>%
      pull(culmen_length) %>%
      mean(),
    mean(penguins$culmen_length[penguins$species == "Adelie"])
  )

  expect_equal(
    penguins %>%
      switch_branch("Chinstrap",
        Gentoo = . %>%
          filter(species == "Gentoo"),
        Adelie = . %>%
          filter(species == "Adelie")
      ) %>%
      pull(culmen_length) %>%
      mean(),
    mean(penguins$culmen_length)
  )

  f <- function(x) {
    x %>%
      switch_branch("Gentoo",
        Gentoo = . %>%
          filter(species == "Gentoo"),
        Adelie = . %>%
          filter(species == "Adelie")
      ) %>%
      pull(culmen_length) %>%
      mean()
  }

  expect_equal(f(penguins), mean(penguins$culmen_length[penguins$species == "Gentoo"]))
})

test_that("case works implicitly with numbers", {
  expect_equal(
    penguins %>%
      switch_branch(
        1,
        . %>%
          filter(species == "Gentoo"),
        . %>%
          filter(species == "Adelie")
      ) %>%
      pull(culmen_length) %>%
      mean(),
    mean(penguins$culmen_length[penguins$species == "Gentoo"])
  )

  expect_equal(
    penguins %>%
      switch_branch(
        2,
        . %>%
          filter(species == "Gentoo"),
        . %>%
          filter(species == "Adelie")
      ) %>%
      pull(culmen_length) %>%
      mean(),
    mean(penguins$culmen_length[penguins$species == "Adelie"])
  )

  expect_equal(
    penguins %>%
      switch_branch(
        3,
        . %>%
          filter(species == "Gentoo"),
        . %>%
          filter(species == "Adelie")
      ) %>%
      pull(culmen_length) %>%
      mean(),
    mean(penguins$culmen_length)
  )

  f <- function(x) {
    x %>%
      switch_branch(
        1,
        . %>%
          filter(species == "Gentoo"),
        . %>%
          filter(species == "Adelie")
      ) %>%
      pull(culmen_length) %>%
      mean()
  }

  expect_equal(f(penguins), mean(penguins$culmen_length[penguins$species == "Gentoo"]))
})

test_that("case works for external variable with characters", {
  this_case <- "Gentoo"
  expect_equal(
    penguins %>%
      switch_branch(this_case,
        Gentoo = . %>%
          filter(species == "Gentoo"),
        Adelie = . %>%
          filter(species == "Adelie")
      ) %>%
      pull(culmen_length) %>%
      mean(),
    mean(penguins$culmen_length[penguins$species == "Gentoo"])
  )


  f <- function(x) {
    this_case <- "Adelie"
    x %>%
      switch_branch(this_case,
        Gentoo = . %>%
          filter(species == "Gentoo"),
        Adelie = . %>%
          filter(species == "Adelie")
      ) %>%
      pull(culmen_length) %>%
      mean()
  }

  expect_equal(f(penguins), mean(penguins$culmen_length[penguins$species == "Adelie"]))
})

test_that("case works for external variable with numbers", {
  this_case <- 1
  expect_equal(
    penguins %>%
      switch_branch(
        this_case,
        . %>%
          filter(species == "Gentoo"),
        . %>%
          filter(species == "Adelie")
      ) %>%
      pull(culmen_length) %>%
      mean(),
    mean(penguins$culmen_length[penguins$species == "Gentoo"])
  )


  f <- function(x) {
    this_case <- 2
    x %>%
      switch_branch(
        this_case,
        . %>%
          filter(species == "Gentoo"),
        . %>%
          filter(species == "Adelie")
      ) %>%
      pull(culmen_length) %>%
      mean()
  }

  expect_equal(f(penguins), mean(penguins$culmen_length[penguins$species == "Adelie"]))
})

test_that("case statement is evaluated within data (uses eval_expr) with characters", {
  expect_equal(
    penguins %>%
      switch_branch(names(.)[4],
        culmen_length = . %>%
          filter(species == "Gentoo"),
        culmen_depth = . %>%
          filter(species == "Adelie")
      ) %>%
      pull(culmen_length) %>%
      mean(),
    mean(penguins$culmen_length[penguins$species == "Gentoo"])
  )

  expect_equal(
    penguins %>%
      switch_branch(names(.)[5],
        culmen_length = . %>%
          filter(species == "Gentoo"),
        culmen_depth = . %>%
          filter(species == "Adelie")
      ) %>%
      pull(culmen_length) %>%
      mean(),
    mean(penguins$culmen_length[penguins$species == "Adelie"])
  )

  expect_equal(
    penguins %>%
      switch_branch(names(.)[1],
        culmen_length = . %>%
          filter(species == "Gentoo"),
        culmen_depth = . %>%
          filter(species == "Adelie")
      ) %>%
      pull(culmen_length) %>%
      mean(),
    mean(penguins$culmen_length)
  )


  f <- function(x) {
    x %>%
      switch_branch(names(.)[4],
        culmen_length = . %>%
          filter(species == "Gentoo"),
        culmen_depth = . %>%
          filter(species == "Adelie")
      ) %>%
      pull(culmen_length) %>%
      mean()
  }

  expect_equal(f(penguins), mean(penguins$culmen_length[penguins$species == "Gentoo"]))
})

test_that("case statement is evaluated within data (uses eval_expr) with numbers", {
  expect_equal(
    penguins %>%
      switch_branch(
        floor(mean(culmen_length) - 42),
        . %>%
          filter(species == "Gentoo"),
        . %>%
          filter(species == "Adelie")
      ) %>%
      pull(culmen_length) %>%
      mean(),
    mean(penguins$culmen_length[penguins$species == "Gentoo"])
  )


  f <- function(x) {
    x %>%
      switch_branch(floor(mean(culmen_length) - 41),
        culmen_length = . %>%
          filter(species == "Gentoo"),
        Petal.Width = . %>%
          filter(species == "Adelie")
      ) %>%
      pull(culmen_length) %>%
      mean()
  }

  expect_equal(f(penguins), mean(penguins$culmen_length[penguins$species == "Adelie"]))
})

test_that("case function is evaluated within data (uses eval_expr) with characters", {
  expect_equal(
    penguins %>%
      switch_branch(. %>%
        arrange(species) %>%
        slice(1) %>%
        pull(species) %>%
        as.character(),
      Gentoo = . %>%
        filter(species == "Gentoo"),
      Adelie = . %>%
        filter(species == "Adelie")
      ) %>%
      pull(culmen_length) %>%
      mean(),
    mean(penguins$culmen_length[penguins$species == "Adelie"])
  )

  expect_equal(
    penguins %>%
      switch_branch(. %>%
        arrange(species) %>%
        slice(333) %>%
        pull(species) %>%
        as.character(),
      Gentoo = . %>%
        filter(species == "Gentoo"),
      Adelie = . %>%
        filter(species == "Adelie")
      ) %>%
      pull(culmen_length) %>%
      mean(),
    mean(penguins$culmen_length[penguins$species == "Gentoo"])
  )

  expect_equal(
    penguins %>%
      switch_branch(. %>%
        arrange(species) %>%
        slice(200) %>%
        pull(species) %>%
        as.character(),
      Gentoo = . %>%
        filter(species == "Gentoo"),
      Adelie = . %>%
        filter(species == "Adelie")
      ) %>%
      pull(culmen_length) %>%
      mean(),
    mean(penguins$culmen_length)
  )


  f <- function(x) {
    x %>%
      switch_branch(. %>%
        arrange(species) %>%
        slice(1) %>%
        pull(species) %>%
        as.character(),
      Gentoo = . %>%
        filter(species == "Gentoo"),
      Adelie = . %>%
        filter(species == "Adelie")
      ) %>%
      pull(culmen_length) %>%
      mean()
  }

  expect_equal(f(penguins), mean(penguins$culmen_length[penguins$species == "Adelie"]))
})

test_that("case function is evaluated within data (uses eval_expr) with numbers", {
  expect_equal(
    penguins %>%
      switch_branch(
        . %>%
          arrange(species) %>%
          slice(1) %>%
          pull(species) %>%
          as.numeric(),
        . %>%
          filter(species == "Gentoo"),
        . %>%
          filter(species == "Adelie")
      ) %>%
      pull(culmen_length) %>%
      mean(),
    mean(penguins$culmen_length[penguins$species == "Gentoo"])
  )

  expect_equal(
    penguins %>%
      switch_branch(
        . %>%
          arrange(species) %>%
          slice(333) %>%
          pull(species) %>%
          as.numeric(),
        . %>%
          filter(species == "Gentoo"),
        . %>%
          filter(species == "Adelie")
      ) %>%
      pull(culmen_length) %>%
      mean(),
    mean(penguins$culmen_length)
  )

  expect_equal(
    penguins %>%
      switch_branch(
        . %>%
          arrange(species) %>%
          slice(200) %>%
          pull(species) %>%
          as.numeric(),
        . %>%
          filter(species == "Gentoo"),
        . %>%
          filter(species == "Adelie")
      ) %>%
      pull(culmen_length) %>%
      mean(),
    mean(penguins$culmen_length[penguins$species == "Adelie"])
  )


  f <- function(x) {
    x %>%
      switch_branch(
        . %>%
          arrange(species) %>%
          slice(1) %>%
          pull(species) %>%
          as.numeric(),
        . %>%
          filter(species == "Gentoo"),
        . %>%
          filter(species == "Adelie")
      ) %>%
      pull(culmen_length) %>%
      mean()
  }

  expect_equal(f(penguins), mean(penguins$culmen_length[penguins$species == "Gentoo"]))
})

test_that("works with enquo-ing of case with characters", {
  f <- function(x, cond) {
    x %>%
      switch_branch(!!enquo(cond),
        culmen_length = . %>%
          filter(species == "Gentoo"),
        culmen_depth = . %>%
          filter(species == "Adelie")
      ) %>%
      pull(culmen_length) %>%
      mean()
  }

  expect_equal(
    f(penguins, names(.)[4]),
    mean(penguins$culmen_length[penguins$species == "Gentoo"])
  )
})

test_that("works with enquo-ing of case with numbers", {
  f <- function(x, cond) {
    x %>%
      switch_branch(!!enquo(cond),
        culmen_length = . %>%
          filter(species == "Gentoo"),
        Petal.Width = . %>%
          filter(species == "Adelie")
      ) %>%
      pull(culmen_length) %>%
      mean()
  }

  expect_equal(
    f(penguins, floor(mean(penguins$culmen_length) - 42)),
    mean(penguins$culmen_length[penguins$species == "Gentoo"])
  )
})

test_that("warning is thrown when no match is found", {
  expect_warning(penguins %>%
    switch_branch("Chinstrap",
      Gentoo = . %>%
        filter(species == "Gentoo"),
      Adelie = . %>%
        filter(species == "Adelie"),
      warn = T
    ))

  expect_warning(penguins %>%
    switch_branch(3,
      . %>%
        filter(species == "Gentoo"),
      . %>%
        filter(species == "Adelie"),
      warn = T
    ))

  f <- function(x) {
    x %>%
      switch_branch("Chinstrap",
        Gentoo = . %>%
          filter(species == "Gentoo"),
        Adelie = . %>%
          filter(species == "Adelie"),
        warn = T
      )
  }

  expect_warning(f(penguins))
})

test_that("error is thrown when evaluated as incompatible", {
  expect_error(penguins %>%
    switch_branch(TRUE,
      Gentoo = . %>%
        filter(species == "Gentoo"),
      Adelie = . %>%
        filter(species == "Adelie"),
      warn = T
    ))

  f <- function(x) {
    x %>%
      switch_branch(TRUE,
        Gentoo = . %>%
          filter(species == "Gentoo"),
        Adelie = . %>%
          filter(species == "Adelie"),
        warn = T
      )
  }

  expect_error(f(penguins))
})
