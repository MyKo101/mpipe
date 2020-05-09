#' @name pipe-grab
#'
#' @title
#' Grabs a value from a pipeline
#'
#' @description
#' Allows a user to store a variable without
#' breaking out of the current pipeline. A wrapper for
#' `assign()`. Returns the original input unchanged
#'
#' @param .dat
#' the data being passed through the pipeline
#'
#' @param variable
#' character or symbol to be stored out of the pipeline
#'
#'
#'
pipe_grab <- function(.dat,variable)
{
  variable.name <- as.character(substitute(variable))

  c.frame <- parent.frame(1)

  assign(variable.name,.dat[[variable.name]],envir = c.frame)
  return(.dat)
}


tibble(iris) %>%
  mutate(Sepal.Area = Sepal.Width*Sepal.Length) %>%
  pipe_grab(Sepal.Area) %>%
  mutate(Petal.Area = Petal.Width*Petal.Length) %>%
  pipe_store("Petal_A","Petal.Area")

f <- function()
{
  tibble(iris) %>%
    mutate(Sepal.Area = Sepal.Width*Sepal.Length) %>%
    pipe_store(Sepal_A,Sepal.Area)


  return(Sepal_A)
}

g <- function()
{
  f()
}




