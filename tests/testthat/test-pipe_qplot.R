library(ggplot2)

test_that("returned value is same as input", {
  expect_equal(
    penguins %>% pipe_qplot(culmen_length, culmen_depth),
    penguins
  )
})

test_that("does it warn when no output expected", {
  expect_warning(penguins %>%
    pipe_qplot(culmen_length, culmen_depth, print.plot = F))
})


plot_file <- tempfile(fileext = ".jpg")

test_that("saving plot works correctly", {
  expect_file_created(
    penguins %>%
      pipe_qplot(culmen_length, culmen_depth,
        save.options = list(
          filename = plot_file,
          width = 6,
          height = 3,
          units = "in"
        )
      ),
    plot_file
  )
})
