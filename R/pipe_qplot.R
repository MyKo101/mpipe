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
#' @param theme
#' a character string that links to a function of the form
#' `theme_*()`, e.g. the agrument `theme="bw"` will apply `theme_bw()`
#'
#' @param  facets,margins,geom,xlim,ylim,log,main,xlab,ylab,asp,stat,position
#' see the [`qplot()`][ggplot2::qplot()] documentation for more
#' information regarding these arguments.
#'
#' @export
#'
#' @examples
#'
#' palmerpenguins::penguins %>%
#'   dplyr::group_by(species) %>%
#'   pipe_qplot(culmen_length_mm,
#'     fill = species,
#'     theme = "light",
#'     geom = "density",
#'     alpha = 0.5,
#'     binwidth = 0.1
#'   ) %>%
#'   dplyr::summarise(mean = mean(culmen_length_mm))
pipe_qplot <- function(data, x, y, ..., facets = NULL, margins = FALSE, geom = "auto",
                       xlim = c(NA, NA), ylim = c(NA, NA), log = "",
                       main = NULL, xlab = NULL, ylab = NULL, asp = NA,
                       stat = stat, position = position,
                       theme = NULL, save.options = NULL, print.plot = T) {
  requireNamespace("ggplot2", quietly = T)
  if (is.null(save.options) & !print.plot) {
    rlang::warn("print.plot set to FALSE in pipe_qplot() &
                no save.options supplied. Nothing done")
  } else {
    parent <- rlang::caller_env()
    env <- list2env(data, parent = parent)
    env[["data"]] <- data

    .call <- match.call()

    .args <- names(.call)

    if (any(.args %in% c("print.plot", "save.options", "theme"))) {
      .call <- .call[-which(.args %in% c("print.plot", "save.options", "theme"))]
    }
    ####

    .call[[1]] <- quote(ggplot2::qplot)

    quo_call <- rlang::quo()
    quo_call <- rlang::quo_set_expr(quo_call, .call)

    env[[".call"]] <- .call
    p <- eval(.call, env)

    if (!is.null(theme)) {
      theme_func_search <- tryCatch(match.fun(paste0("theme_", theme)),
        error = function(e) NULL
      )
      if (is.null(theme_func_search)) {
        theme_func_search <- tryCatch(match.fun(paste0("ggplot2::theme_", theme)),
          error = function(e) NULL
        )
      }
      if (!is.null(theme_func_search)) {
        p <- p + theme_func_search()
      }
    }

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
