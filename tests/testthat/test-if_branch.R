



test_that("predicate works implicitly", {
  expect_equal(
    penguins %>%
      if_branch(
        TRUE,
        . %>%
          filter(species == "Adelie"),
        . %>%
          filter(species == "Chinstrap")
      ) %>%
      pull(bill_length) %>%
      mean(),
    mean(penguins$bill_length[penguins$species == "Adelie"])
  )

  expect_equal(
    penguins %>%
      if_branch(
        FALSE,
        . %>%
          filter(species == "Adelie"),
        . %>%
          filter(species == "Chinstrap")
      ) %>%
      pull(bill_length) %>%
      mean(),
    mean(penguins$bill_length[penguins$species == "Chinstrap"])
  )

  expect_equal(
    penguins %>%
      if_branch(
        FALSE,
        . %>%
          filter(species == "Adelie")
      ) %>%
      pull(bill_length) %>%
      mean(),
    mean(penguins$bill_length)
  )

  f <- function(x) {
    x %>%
      if_branch(
        TRUE,
        . %>%
          filter(species == "Adelie"),
        . %>%
          filter(species == "Chinstrap")
      ) %>%
      pull(bill_length) %>%
      mean()
  }

  expect_equal(f(penguins), mean(penguins$bill_length[penguins$species == "Adelie"]))
})

test_that("predicate works for external variable", {
  this_predicate <- TRUE
  expect_equal(
    penguins %>%
      if_branch(
        this_predicate,
        . %>%
          filter(species == "Adelie"),
        . %>%
          filter(species == "Chinstrap")
      ) %>%
      pull(bill_length) %>%
      mean(),
    mean(penguins$bill_length[penguins$species == "Adelie"])
  )

  f <- function(x) {
    this_predicate <- FALSE

    x %>%
      if_branch(
        this_predicate,
        . %>%
          filter(species == "Adelie"),
        . %>%
          filter(species == "Chinstrap")
      ) %>%
      pull(bill_length) %>%
      mean()
  }

  expect_equal(f(penguins), mean(penguins$bill_length[penguins$species == "Chinstrap"]))
})

test_that("predicate statement is evaluated within data (uses eval_expr)", {
  expect_equal(
    penguins %>%
      if_branch(
        nrow(.) > 100,
        . %>%
          filter(species == "Adelie"),
        . %>%
          filter(species == "Chinstrap")
      ) %>%
      pull(bill_length) %>%
      mean(),
    mean(penguins$bill_length[penguins$species == "Adelie"])
  )

  expect_equal(
    penguins %>%
      if_branch(
        nrow(.) < 100,
        . %>%
          filter(species == "Adelie"),
        . %>%
          filter(species == "Chinstrap")
      ) %>%
      pull(bill_length) %>%
      mean(),
    mean(penguins$bill_length[penguins$species == "Chinstrap"])
  )

  f <- function(x) {
    x %>%
      if_branch(
        nrow(.) < 100,
        . %>%
          filter(species == "Adelie"),
        . %>%
          filter(species == "Chinstrap")
      ) %>%
      pull(bill_length) %>%
      mean()
  }

  expect_equal(f(penguins), mean(penguins$bill_length[penguins$species == "Chinstrap"]))
})

test_that("predicate function is evaluated within data (uses eval_expr)", {
  expect_equal(
    penguins %>%
      if_branch(
        . %>%
          nrow() %>%
          is_greater_than(100),
        . %>%
          filter(species == "Adelie"),
        . %>%
          filter(species == "Chinstrap")
      ) %>%
      pull(bill_length) %>%
      mean(),
    mean(penguins$bill_length[penguins$species == "Adelie"])
  )

  expect_equal(
    penguins %>%
      if_branch(
        . %>%
          nrow() %>%
          is_less_than(100),
        . %>%
          filter(species == "Adelie"),
        . %>%
          filter(species == "Chinstrap")
      ) %>%
      pull(bill_length) %>%
      mean(),
    mean(penguins$bill_length[penguins$species == "Chinstrap"])
  )

  f <- function(x) {
    x %>%
      if_branch(
        . %>%
          nrow() %>%
          is_greater_than(100),
        . %>%
          filter(species == "Adelie"),
        . %>%
          filter(species == "Chinstrap")
      ) %>%
      pull(bill_length) %>%
      mean()
  }

  expect_equal(f(penguins), mean(penguins$bill_length[penguins$species == "Adelie"]))
})

test_that("works with enquo-ing of cond", {
  f <- function(x, cond) {
    x %>%
      if_branch(
        !!enquo(cond),
        . %>%
          filter(species == "Adelie"),
        . %>%
          filter(species == "Chinstrap")
      ) %>%
      pull(bill_length) %>%
      mean()
  }

  expect_equal(
    f(penguins, nrow(.) < 100),
    mean(penguins$bill_length[penguins$species == "Chinstrap"])
  )
})
