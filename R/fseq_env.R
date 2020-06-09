#' @name fseq_env
#'
#' @title
#' Alter the environments of fseq functions
#'
#' @description
#' These functions allows access to the environments contained
#' within `fseq` objects. `fseq` objects are functions that have been
#' created using the pipeline with a `.` as the left-hand side.
#'
#' `fseq` functions contain a major environment for the overall
#' function (i.e the environment of `fn_fseq`) and multiple
#' minor environments. The minor environments are the
#' function-environments of each of functions that make up the
#' functional sequence.
#'
#' For example, if we define `a <- . %>% add(2) %>% multiply_by(3)`,
#' then `a()` will be a function that has an environment, this is the
#' major environment. Within `a()`, there is a list of functions that
#' make up the function sequence, `add(.,2)` and `multiply_by(.,3)`.
#' Each of these will also have their own environment, the minor
#' environments. Therefore, `a()` will have 3 environments associated
#' with it.
#'
#' @export
#'
#' @param fn_fseq
#' a function of class `fseq` (with an appropriate environment).
#'
#' @param i
#' a numeric indicating which function in the `fseq` to refer to. If
#' 0, then the parent `fseq` environment will be used.
#'

fseq_get_env <- function(fn_fseq,i)
{
  stopifnot(is_fseq(fn_fseq))
  if(rlang::is_missing(i)||i == 0)
  {
    environment(fn_fseq)
  } else if(i <= length(fn_fseq))
  {
    environment(environment(fn_fseq)[["_function_list"]][[i]])
  } else
  {
    NULL
  }
}

#' @rdname fseq_env
#' @export

fseq_print_env <- function(fn_fseq,i)
{
  stopifnot(is_fseq(fn_fseq))
  c_env <- fseq_get_env(fn_fseq,i)

  rlang::env_print(c_env)

  invisible(c_env)
}

#' @rdname fseq_env
#' @export
#'
#' @param new_env
#' environment to be assigned to the relevant environment in `fn_fseq`.

fseq_set_env <- function(fn_fseq,i,new_env)
{
  stopifnot(is_fseq(fn_fseq))
  if(rlang::is_missing(i)||i == 0)
  {
    fseq_check_env(new_env)
    environment(fn_fseq) <- new_env
  } else if(i <= length(fn_fseq))
  {
    environment(environment(fn_fseq)[["_function_list"]][[i]]) <- new_env
  }

  invisible(fn_fseq)
}

#' @rdname fseq_env
#' @export

fseq_check_env <- function(new_env)
{
  if(any(!c("freduce","_fseq","_function_list") %in% names(new_env)))
    rlang::abort(paste("environment assigned to fseq parent",
                        "must contain objects called",
                        "freduce, _fseq and _function_list"))

  c_freduce <- new_env$freduce
  if(!is.function(c_freduce))
    rlang::abort("freduce in new_env must be a function")

  if(any(!c("value","function_list") %in% names(formals(c_freduce))))
    rlang::abort("freduce in new_env must take value and function_list as arguments")

  c_fseq <- new_env$`_fseq`
  if(!is.function(c_fseq))
    rlang::abort("_fseq in new_env must be a function")

  if(length(formals(c_fseq)) != 1)
    rlang::abort("_fseq in new_env must take a single argument")

  c_function_list <- new_env$`_function_list`
  if(any(!vapply(c_function_list,is.function,logical(1))))
    rlang::abort("all elements of _function_list in new_env must be functions")

  if(any(!vapply(c_function_list,function(f) length(formals(f)) == 1,logical(1))))
    rlang::abort("all functions of _function_list in new_env must be take a single argument")


}

#' @rdname fseq_env
#' @export

fseq_copy_fun <- function(fn_fseq)
{
  new_fn_fseq <- fn_fseq

  for(i in 0:length(fn_fseq))
  {
    c_env <- fseq_get_env(new_fn_fseq,i)
    new_env <- copy_env(c_env)
    new_fn_fseq <- fseq_set_env(new_fn_fseq,i,new_env)
  }

  new_fn_fseq

}

#' @rdname fseq_env
#' @export
#'
#' @param fun
#' function to edit the environment of.
#'
#' @param variable
#' name of variable (as string) to assign value to within all
#' environments in `fun`.
#'
#' @param value
#' value to be assigned to `variable` in all environments in `fun`
#'
#'

fun_var_env <- function(fun,variable,value)
{
  stopifnot(is.function(fun))

  environment(fun)[[variable]] <- value

  if(is_fseq(fun))
  {
    for(i in 1:length(fun))
    {
      environment(environment(fun)[["_function_list"]][[i]])[[variable]] <- value
    }

  }



}


