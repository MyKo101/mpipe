#' @name while_pipe
#'
#' @title
#' Perform an while-like loop in a pipeline
#'
#' @description
#' Allows the user to perform a while-like
#' loop without breaking out of a pipeline.
#' To maintain the flow of a pipeline, it is recommended
#' to use [`fseq`][] style arguments (i.e. pipelines) for the
#' `fun` argument.
#'
#' Within the `while_pipe()`, users can use the `.counter` variable
#' to reference how many iterations have been performed so far.
#'
#' @param data
#' the data being passed through the pipeline.
#'
#' @param cond
#' an expression to be evaluated in the context of `data` to
#' decide whether to perform `fun`. Should evaluate to a single
#' logical value or be coercible to one.
#'
#' @param fun
#' pipeline to perform until `cond` is `FALSE`.
#'
#'
#' @export
#'
#' @examples
#'
#' sample(100,1) %>%
#'   runif %>%
#'   pipe_cat("Current length: ",length(.),"\n") %>%
#'   while_pipe(length(.) > 1,
#'              . %>%
#'                diff %>%
#'                magrittr::divide_by(2))
#'
#'
#' tibble::tibble(x = runif(5)) %>%
#'   while_pipe(.counter <= 5,
#'              . %>%
#'                dplyr::mutate(!!paste0("x_",.counter) := x - x[.counter]))
#'
#'
#'


while_pipe <- function(data,cond,fun)
{
  c_env <- rlang::current_env()

  quo_cond <- enquo(cond)
  quo_cond <- rlang::quo_set_env(quo_cond,c_env)

  .counter <- 1L
  current_data <- data

  cond_eval <- eval_expr(current_data,!!quo_cond,.counter)


  new_fun <- copy_fun(fun)

  fun_var_env(new_fun,".counter",.counter)

  while(cond_eval)
  {
    fun_var_env(new_fun,".counter",.counter)
    current_data <- new_fun(current_data)

    .counter <- .counter+1L
    cond_eval <- eval_expr(current_data,!!quo_cond,.counter)
  }

  current_data

}









