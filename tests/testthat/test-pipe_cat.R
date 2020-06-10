
test_that("return is same as input", {
  expect_equal(iris %>% pipe_cat(""), iris)
})

test_that("plain text is output", {
  expect_output(iris %>%
    pipe_cat("hello"), "hello")
  expect_output(1:10 %>%
    pipe_cat("world"), "world")
})

test_that("evaluated text is output", {
  expect_output(
    iris %>%
      pipe_cat("Average Sepal.Length:", round(mean(Sepal.Length), 3)),
    paste("Average Sepal.Length:", round(mean(iris$Sepal.Length), 3))
  )
  expect_output(
    iris %>%
      pipe_cat("Average Sepal.Width:", round(mean(Sepal.Length), 3)),
    paste("Average Sepal.Width:", round(mean(iris$Sepal.Length), 3))
  )
  expect_output(
    iris %>%
      pipe_cat("Number of rows:", nrow(.)),
    paste("Number of rows:", nrow(iris))
  )
})

test_that("grouped tibbles are printed separately", {
  grouped_out <- iris %>%
    group_by(Species) %>%
    summarise(mean_SL = round(mean(Sepal.Length), 3)) %>%
    mutate(output = paste(as.character(Species), "Average Sepal.Length:", mean_SL)) %>%
    pull(output) %>%
    paste0(collapse = "\\n") %>%
    gsub(".", "\\.", fixed = T)

  expect_output(
    iris %>%
      group_by(Species) %>%
      pipe_cat(as.character(Species), "Average Sepal.Length:", round(mean(Sepal.Length), 3), "\n"),
    grouped_out
  )
})
