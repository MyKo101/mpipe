#' @name switch_branch
#'
#' @title
#' Perform an switch-like branch in a pipeline
#'
#' @description
#' Allows the user to perform a switch-like
#' branch without breaking out of a pipeline.
#' To maintain the flow of a pipeline, it is recommended
#' to use `fseq` style arguments (i.e. pipelines) for the cases,
#' however any function can be used. If no cases
#' match, then the original data is passed unchanged
#'
#' @param data
#' the data being passed through the pipeline.
#'
#' @param case
#' an expression to be evaluated in the context of `data` to
#' decide which branch to follow. Must evaluate to numeric or a
#' character string.
#'
#' @param ...
#' the list of alternatives. If `case` is numeric, then the `case`-th alternative
#' will be chosen (if it exists), if `case` is a character, then it will be
#' compared against the names of one of these alternatives.
#' If no character matches are found (or the numeric is out of range), then
#' the `data` will be returned untouched.
#'
#' @param warn
#' whether or not to warn that no cases were chosen
#'
#'
#' @export
#'
#' @examples
#'
#'
#' tibble::tibble(
#'   x = rnorm(10),
#'   y = sample(c("red", "blue", "yellow"),
#'     10,
#'     replace = TRUE
#'   )
#' ) %>%
#'   dplyr::arrange(x) %>%
#'   switch_branch(. %>%
#'     dplyr::slice(1) %>%
#'     dplyr::pull(y),
#'   red = . %>%
#'     pipe_cat("top was red\n") %>%
#'     dplyr::filter(y == "red"),
#'   blue = . %>%
#'     pipe_cat("top was blue\n") %>%
#'     dplyr::filter(x < 0)
#'   ) %>%
#'   dplyr::summarise(m.x = mean(x))
#'
#' palmerpenguins::penguins %>%
#'   dplyr::mutate(species = factor(species, levels = c("Gentoo", "Adelie", "Chinstrap"))) %>%
#'   dplyr::sample_n(1) %>%
#'   switch_branch(
#'     . %>%
#'       dplyr::pull(species) %>%
#'       as.numeric(),
#'     . %>%
#'       pipe_cat("Selected row is Gentoo\n"),
#'     . %>%
#'       pipe_cat("Selected row is Adelie\n"),
#'     . %>%
#'       pipe_cat("Selected row is Chinstrap\n")
#'   )
switch_branch <- function(data, case, ..., warn = F) {
  parent <- rlang::caller_env()
  env <- new.env(parent = parent)

  fs <- rlang::list2(...)

  original_data <- data

  if (dplyr::is_grouped_df(data)) data <- dplyr::ungroup(data)

  case_eval <- eval_expr(data, !!enquo(case), env = env)



  if (!is.character(case_eval) && !is.numeric(case_eval)) {
    rlang::abort("case must evaluate to character or numeric")
  }

  case_list <- names(fs)
  if (is.numeric(case_eval) && case_eval > length(fs)) {
    if (warn) rlang::warn(paste0("Only ", length(fs), "case(s) supplied, case evaluated to ", case_eval))
    chosen_f <- identity
  } else if (is.character(case_eval) && !(case_eval %in% names(fs))) {
    if (warn) rlang::warn(paste0("case evaluated to ", case_eval, " which was not supplied"))
    chosen_f <- identity
  } else {
    chosen_f <- fs[[case_eval]]
  }

  chosen_f(original_data)
}
