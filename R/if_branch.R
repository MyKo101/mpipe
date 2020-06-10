#' @name if_branch
#'
#' @title
#' Perform an if/else-like branch in a pipeline
#'
#' @description
#' Allows the user to perform an if/else-like
#' branch without breaking out of a pipeline.
#' To maintain the flow of a pipeline, it is recommended
#' to use `fseq` style arguments (i.e. pipelines) for `fun` and
#' `elsefun`, however any function can be used.
#'
#' @param data
#' the data being passed through the pipeline
#'
#' @param predicate
#' logical statement, a function or an expression that
#' can be evaluated in the context of `data`
#' to decide which branch to follow
#'
#' @param fun
#' pipeline to follow if `predicate` evaluates as `TRUE`
#'
#' @param elsefun
#' pipeline to follow if `predicate` evaluates as `FALSE`, or
#' `NULL` if nothing is to happen to the original `data`.
#'
#' @export
#'
#' @examples
#'
#' 1 %>%
#'   magrittr::multiply_by(2) %>%
#'   if_branch(
#'     . %>% magrittr::equals(2),
#'     . %>%
#'       magrittr::multiply_by(3) %>%
#'       magrittr::add(2)
#'   ) %>%
#'   magrittr::multiply_by(2)
#'
#' tibble::tibble(x = rnorm(100), y = rnorm(100)) %>%
#'   dplyr::mutate(z = x + y) %>%
#'   if_branch(
#'     mean(z) > 0,
#'     . %>%
#'       pipe_cat("z is high\n\n"),
#'     . %>%
#'       dplyr::select(-z) %>%
#'       pipe_cat("z is low, so it was dropped\n\n")
#'   )
if_branch <- function(data, predicate, fun, elsefun = NULL) {
  parent <- rlang::caller_env()
  env <- new.env(parent = parent)

  predicate_eval <- eval_expr(data, !!enquo(predicate), env = env)

  if (predicate_eval) {
    fun(data)
  } else if (!is.null(elsefun)) {
    elsefun(data)
  } else {
    data
  }
}
