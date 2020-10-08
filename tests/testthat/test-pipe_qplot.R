
penguins <- clean_penguins()

test_that("returned value is same as input", {
  expect_equal(
    penguins %>% pipe_qplot(bill_length, bill_depth, theme = "light"),
    penguins
  )
})

test_that("does it warn when no output expected", {
  expect_warning(
    penguins %>%
      pipe_qplot(bill_length, bill_depth, print.plot = F)
  )
})

test_that("does it warn when unavailable theme is attempted", {
  expect_warning(
    penguins %>%
      pipe_qplot(bill_length, bill_depth, theme = "none")
  )
})


plot_file <- tempfile(fileext = ".jpg")

test_that("saving plot works correctly", {
  expect_file_created(
    penguins %>%
      pipe_qplot(bill_length, bill_depth,
        save.options = list(
          filename = plot_file,
          width = 6,
          height = 3,
          units = "in",
          plot = NULL
        )
      ),
    plot_file
  )
})
