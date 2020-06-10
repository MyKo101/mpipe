#' @name copy_env
#'
#' @title
#' Copy an environment or a function
#'
#' @description
#' By default, R does not create a copy of an environment when it is
#' assigned to a new variable. If `a` is an environment, `b <- a`
#' just makes `b` point to the same environment as `a`. This means
#' that any changes that happen to `b` also happen to `a` (or
#' rather, the environment that they point to). Sometimes, this
#' is not preferred and it would be better to explicitly make a
#' copy of the environment.
#'
#' The `copy_env()` function creates a new environment with the same
#' parent as `env`, and all the objects in `env` are copied
#' (in *their* usual way). The only exception to this is if `env`
#' contains an environment object, in which case, it will also be
#' copied using `copy_env()`. This also means that the only changes
#' occurring is the re-direction of pointers and so `copy_env()` is
#' surprisingly memory-light as it does not actually copy any large
#' objects.
#'
#' The `copy_fun()` function similarly creates a copy of a function,
#' including that applying `copy_env()` to that function's
#' environment. This is particularly useful for `fseq` which rely
#' heavily on their function environment
#'
#'
#' @param env
#' environment to be copied
#'
#' @param fun
#' function to be copied
#'
#' @export
#'
#' @examples
#' a <- new.env()
#' a$x <- 1
#' a$y <- "hello"
#' a$z <- data.frame(x = runif(20))
#'
#' b <- copy_env(a)
#' identical(a$z, b$z)
#' rlang::env_label(a) == rlang::env_label(b)
#'
#' f1 <- . %>%
#'   magrittr::add(2) %>%
#'   magrittr::multiply_by(2)
#' f2 <- f1
#' f3 <- copy_fun(f1)
#'
#' f1_env_label <- rlang::env_label(rlang::get_env(f1))
#' f2_env_label <- rlang::env_label(rlang::get_env(f2))
#' f3_env_label <- rlang::env_label(rlang::get_env(f3))
#' f1_env_label == f2_env_label
#' f1_env_label == f3_env_label
copy_env <- function(env) {
  obj_list <- ls(env)

  new_env <- rlang::new_environment(parent = parent.env(env))

  for (c_obj in obj_list)
  {
    if (!is.environment(env[[c_obj]])) {
      new_env[[c_obj]] <- env[[c_obj]]
    }
  }

  new_env
}

#' @rdname copy_env
#' @export
#'

copy_fun <- function(fun) {
  if (is_fseq(fun)) {
    new_fun <- fseq_copy_fun(fun)
  } else if (rlang::is_function(fun)) {
    new_fun <- fun
    environment(new_fun) <- copy_env(environment(fun))
  }

  new_fun
}
