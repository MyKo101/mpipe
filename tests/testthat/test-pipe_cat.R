
penguins <- clean_penguins()

test_that("return is same as input", {
  expect_equal(penguins %>% pipe_cat(""), penguins)
})

test_that("plain text is output", {
  expect_output(penguins %>%
    pipe_cat("hello"), "hello")
  expect_output(1:10 %>%
    pipe_cat("world"), "world")
})

test_that("evaluated text is output", {
  expect_output(
    penguins %>%
      pipe_cat("Average bill_length:", round(mean(bill_length), 3)),
    paste("Average bill_length:", round(mean(penguins$bill_length), 3))
  )
  expect_output(
    penguins %>%
      pipe_cat("Average Sepal.Width:", round(mean(bill_length), 3)),
    paste("Average Sepal.Width:", round(mean(penguins$bill_length), 3))
  )
  expect_output(
    penguins %>%
      pipe_cat("Number of rows:", nrow(.)),
    paste("Number of rows:", nrow(penguins))
  )
})

test_that("grouped tibbles are printed separately", {
  expect_output(
    penguins %>%
      group_by(species) %>%
      pipe_cat(as.character(species), "Average Bill Length:", round(mean(bill_length), 3), "\n"),
    regexp="(\\w*)(\\s+)Average Bill Length: (\\d*?)"
  )
})
