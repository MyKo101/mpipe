#' @name pipe_qplot
#'
#' @title
#' Pipe-able version of the `qplot()`
#'
#' @description
#' The [`qplot()`][ggplot2::qplot()] function is traditionally used
#' when your data is not stored in a data.frame/tibble and are
#' "loose" within your environment and can be convenient if you are
#' familiar with the base `plot()`, but haven't quite
#' learned the grammar of `ggplot2`.
#' It is a wrapper that creates a [`ggplot()`][ggplot2::ggplot()]
#' style plot.
#'
#' However, creating a [`ggplot()`][ggplot2::ggplot()] object can be
#' complex and inconvenient, especially if you want a simple plot
#' (e.g. a basic histogram). It also requires you to either break
#' out of a pipeline, or ensure your [`ggplot()`][ggplot2::ggplot()]
#' functions are at the end of a pipe (or introduce curly braces
#' `{}` to your pipeline).
#'
#' Therefore, the `pipe_qplot()` function will run a
#' [`qplot()`][ggplot2::qplot()] function for it's side effects and
#' return your original input unchanged. It can also save your plot
#' if needed.
#'
#' @param data
#' the data being passed through the pipeline
#'
#' @param x,y,...
#' aesthetic arguments to be passed to the [`qplot()`][ggplot2::qplot()] function
#'
#' @param save.options
#' list of values to be passed to [`ggsave()`][ggplot2::ggsave()]
#' (if NULL, plot won't be saved)
#'
#' @param print.plot
#' should the plot be displayed? Should only be used if
#' `save.options` is not NULL
#'
#' @param  facets,margins,geom,xlim,ylim,log,main,xlab,ylab,asp,stat,position
#' see the [`qplot()`][ggplot2::qplot()] documentation for more
#' information regarding these arguments.
#'
#' @export
#'
#' @examples
#'
#' tibble::tibble(iris) %>%
#'   dplyr::group_by(Species) %>%
#'   pipe_qplot(Sepal.Length,
#'     fill = Species,
#'     geom = "histogram",
#'     binwidth = 0.1
#'   ) %>%
#'   dplyr::summarise(mean = mean(Sepal.Length))
pipe_qplot <- function(data, x, y, ..., facets = NULL, margins = FALSE, geom = "auto",
                       xlim = c(NA, NA), ylim = c(NA, NA), log = "",
                       main = NULL, xlab = NULL, ylab = NULL, asp = NA,
                       stat = stat, position = position,
                       save.options = NULL, print.plot = T) {
  if (is.null(save.options) & !print.plot) {
    rlang::warn("print.plot set to FALSE in pipe_qplot() &
                no save.options supplied. Nothing done")
  } else {
    parent <- parent.frame()
    env <- new.env(parent = parent)

    .call <- match.call()

    .args <- names(.call)

    if (any(.args %in% c("print.plot", "save.options", "data"))) {
      .call <- .call[-which(.args %in% c("print.plot", "save.options", "data"))]
    }


    .call[[1]] <- quote(ggplot2::qplot)
    quo_call <- rlang::quo()
    quo_call <- rlang::quo_set_expr(quo_call, .call)


    p <- eval_expr(data, !!quo_call)

    if (print.plot) {
      print(p)
    }

    if (!is.null(save.options)) {
      if (any(names(save.options) == "plot")) {
        save.options <- save.options[-which(names(save.options) == "plot")]
      }

      save.call <- as.call(c(quote(ggsave), save.options, plot = quote(p)))
      eval(save.call)
    }
  }
  return(data)
}
