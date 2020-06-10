library(rlang)

f1 <- . %>%
  add(1) %>%
  multiply_by(2)

f2 <- . %>%
  divide_by(5) %>%
  add(3)

h1 <- copy_fun(f1)
h2 <- copy_fun(f2)

g1 <- h1 + h2


test_that("fseq_get_env extracts environments correctly", {
  expect_equal(fseq_get_env(f1), environment(f1))
  expect_equal(fseq_get_env(f1, 0), environment(f1))
  expect_equal(fseq_get_env(f1, 1), environment(functions(f1)[[1]]))
  expect_equal(fseq_get_env(f1, 2), environment(functions(f1)[[2]]))
  expect_null(fseq_get_env(f1, 3))
})

test_that("fseq_print_env prints correctly", {
  print_expected <- paste0(
    "<environment: [A-Za-z0-9]*?>\\n",
    "parent: <environment: [A-Za-z0-9]*?>\\n",
    "bindings:\\n",
    " \\* freduce: <fn>\\n",
    " \\* `_fseq`: <S3: fseq>\\n",
    " \\* `_function_list`: <list>"
  )

  expect_output(fseq_print_env(f1), regexp = print_expected)
})

test_that("check environment contents stay the same", {
  expect_equal(fseq_get_env(h1, 1), fseq_get_env(g1, 1))
  expect_equal(fseq_get_env(h1, 2), fseq_get_env(g1, 2))
  expect_equal(fseq_get_env(h2, 1), fseq_get_env(g1, 3))
  expect_equal(fseq_get_env(h2, 2), fseq_get_env(g1, 4))
})

test_that("check environment locations are the different", {
  expect_different(
    format(fseq_get_env(h1, 1)),
    format(fseq_get_env(g1, 1))
  )
  expect_different(
    format(fseq_get_env(h1, 2)),
    format(fseq_get_env(g1, 2))
  )
  expect_different(
    format(fseq_get_env(h2, 1)),
    format(fseq_get_env(g1, 3))
  )
  expect_different(
    format(fseq_get_env(h2, 2)),
    format(fseq_get_env(g1, 4))
  )
})


g2 <- . %>%
  multiply_by(x) %>%
  add(y)

g2 <- copy_fun(g2)


test_that("variable can be inserted into fseq environments", {
  fun_var_env(g2, "x", 2)
  fun_var_env(g2, "y", 3)
  expect_equal(g2(2), 2 * 2 + 3)
  expect_equal(g2(3), 2 * 3 + 3)

  fun_var_env(g2, "x", 3)
  fun_var_env(g2, "y", 4)
  expect_equal(g2(2), 3 * 2 + 4)
  expect_equal(g2(3), 3 * 3 + 4)

  expect_equal(fseq_get_env(g2, 1)$x, 3)
  expect_equal(fseq_get_env(g2, 1)$y, 4)
  expect_equal(fseq_get_env(g2, 2)$x, 3)
  expect_equal(fseq_get_env(g2, 2)$y, 4)
})

g3 <- . %>%
  add(2) %>%
  multiply_by(3)

test_that("Errors are hit correctly when environments aren't suitable", {
  e1 <- new.env()
  expect_error(fseq_check_env(e1))


  e1$freduce <- 1
  e1$`_fseq` <- 2
  e1$`_function_list` <- 3

  expect_error(fseq_check_env(e1))

  e1$freduce <- function(x) x
  expect_error(fseq_check_env(e1))

  e1$freduce <- fseq_get_env(g3)$freduce
  expect_error(fseq_check_env(e1))

  e1$`_fseq` <- function(x, y) x + y
  expect_error(fseq_check_env(e1))

  e1$`_fseq` <- function(x) x
  expect_error(fseq_check_env(e1))

  e1$`_fseq` <- fseq_get_env(g3)$`_fseq`
  expect_error(fseq_check_env(e1))

  e1$`_function_list` <- list(1, 2, 3)
  expect_error(fseq_check_env(e1))

  e1$`_function_list` <- list(function(x, y) x + y, function(x, y) x * y)
  expect_error(fseq_check_env(e1))

  e1$`_function_list` <- fseq_get_env(g3)$`_function_list`

  expect_equal(fseq_get_env(g3), e1)
})
