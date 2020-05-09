#' @name pipe_plot
#'
#' @title
#' Pipeable version of the `qplot()`
#'
#' @description
#' The `qplot()` function is traditionally used when your data is
#' not stored in a data.frame/tibble and are "loose" within your
#' environment and can be convenient if you are familiar with the
#' base `plot()`, but haven't quite learned the grammar of `ggplot2`.
#' It is a wrapper that creates a `ggplot()` style plot.
#'
#' However, creating a `ggplot()` object can be complex and
#' inconvenient if you want a simple plot (e.g. a basic histogram).
#' If also requires you to either break out of a pipeline, or ensure
#' your `ggplot()` functions are at the end of a pipe (or introduce
#' curly braces `{}` to your pipeline)
#'
#' The `pipe_qplot()` function will run a `qplot()` function for it's
#' side effects and return your original input unchanged
#'
#' @param .dat
#' the data being passed through the pipeline
#'
#' @param ...
#' arguments to be passed to the `qplot()` function
#'
#' @param save.options
#' list of values to be passed to `ggsave()` (if NULL, plot won't be saved)
#'
#' @param print.plot
#' should the plot be displayed? Should only be used if `save.options` is not NULL
#'
#' @examples
#' tibble(iris) %>%
#' group_by(Species) %>%
#' pipe_qplot(Sepal.Length,
#'            fill=Species,
#'            geom="histogram",
#'            binwidth=0.1) %>%
#' summarise(mean = mean(Sepal.Length))
#'
#'
#'
pipe_qplot <- function(.dat,...,save.options=NULL,print.plot=T)
{
  if(is.null(save.options) & !print.plot)
  {
    rlang::warn("print.plot set to False in pipe_qplot() & no save.options supplied")
  } else {
    p <- eval(qplot(...),.dat)

    if(!is.null(save.options))
    {
      save.options <- append(save.options,values=list(plot=p))
      do.call("ggsave",save.options)
    }

    if(print.plot)
    {
      print(p)
    }
  }

  return(.dat)
}



















