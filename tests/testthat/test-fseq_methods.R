
f1 <- . %>%
  add(1) %>%
  multiply_by(2)

f2 <- . %>%
  divide_by(5) %>%
  add(3)

g1 <- f1 + f2
g2 <- f2 + f1

test_that("Check that addition works", {
  expect_equal(f2(f1(1)), (f1 + f2)(1))
  expect_equal(f2(f1(1)), g1(1))
  expect_equal(f1(f2(1)), (f2 + f1)(1))
  expect_equal(f1(f2(1)), g2(1))

  expect_equal(f2(f1(5)), (f1 + f2)(5))
  expect_equal(f2(f1(5)), g1(5))
  expect_equal(f1(f2(5)), (f2 + f1)(5))
  expect_equal(f1(f2(5)), g2(5))
})


test_that("Check is.fseq and is_fseq", {
  expect_true(is.fseq(f1))
  expect_true(is_fseq(f1))
  expect_true(is.fseq(f2))
  expect_true(is_fseq(f2))
  expect_true(is.fseq(g1))
  expect_true(is_fseq(g1))
  expect_true(is.fseq(g2))
  expect_true(is_fseq(g2))
  expect_true(is.fseq(f1 + f2))
  expect_true(is_fseq(f1 + f2))
  expect_true(is.fseq(f2 + f1))
  expect_true(is_fseq(f2 + f1))

  expect_false(is.fseq(mean))
  expect_false(is_fseq(mean))
  expect_false(is.fseq(sd))
  expect_false(is_fseq(sd))
})

test_that("length.fseq works", {
  expect_equal(length(f1), 2)
  expect_equal(length(f2), 2)
  expect_equal(length(g1), 4)
  expect_equal(length(g2), 4)
  expect_equal(length(f1 + f2), 4)
  expect_equal(length(f2 + f1), 4)
})
