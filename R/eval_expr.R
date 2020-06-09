#' @name eval_expr
#'
#' @title
#' Evaluates an expression within a data context
#'
#' @description
#' Uses the [eval_tidy()][rlang::eval_tidy()] function to evaluate an expression
#' within the context of the `data` and `env` arguments.
#' It provides two additional elements of flexibility:
#' * `data` does not need to be named, if it isn't named, it can be
#'   references by `.` or `.data`.
#' * `expr` can be a function (preferably a `fseq` object)
#'   that will be applied to `data`. What's more is that `expr`
#'   will be evaluated through `eval_tidy()` before being applied
#'   and so functions defined within `data` can also be used.
#'
#' Note the `data` argument is first (whereas in `eval_tidy()`, it is
#' second) to keep in line with the pipeline theme of `mpipe`.
#'
#' @param data
#' data within which to evaluate. Used as a data mask in `eval_tidy()`
#'
#'
#' @param expr
#' a call, function or expression to be evaluated within the context
#' of `data`
#'
#' @param ...
#' additional arguments to be attached to `data` during evaluation.
#'
#' @param env
#' the environment in which to evaluate the expression
#'
#' @param allow_NULL
#' logical. Can `eval_expr()` return a `NULL` value? By default, `NULL`s will
#' result in an error.
#'
#' @param verbose
#' logical. Should `eval_expr()` be chatty?
#'
#' @export
#'
#' @examples
#'
#' #Can do simple evaluations of functions on vectors
#' 1:10 %>% eval_expr(mean)
#'
#' #Or on a variable
#' x <- 1:10
#' eval_expr(x,mean)
#'
#' #or within a data.frame (or tibble)
#' tbl <- data.frame(x=1:10)
#' eval_expr(tbl,mean(x))
#'
#' #or a list
#' lst <- list(x=1:10)
#' eval_expr(lst,mean(x))
#'
#' #functions are applied to data
#' eval_expr(tbl,nrow)
#'
#' #but they are evaluated within data first
#' lst <- c(fun=mean,lst)
#' eval_expr(lst,fun(x))
#'
#' #additional named arguments can be passed in too
#' tbl %>%
#'   eval_expr(mean(x+y),y=5)
#'
#' x %>%
#'   eval_expr(. %>% magrittr::add(y) %>% mean,y=5)
#'
#'
#' #Environment scope:
#' lst <- list(x=1:3,y=1)
#' y <- 4
#'
#' e1 <- new.env(parent=emptyenv())
#' e1$y <- 3
#'
#' #additional arguments in data take priority over others
#' eval_expr(lst,y,y=2,env=e1)
#'
#' lst$y <- NULL
#' # then the ... argument:
#' eval_expr(lst,y,y=2,env=e1)
#'
#' #then in env
#' eval_expr(lst,y,env=e1)
#'
#' #without env, the calling environment is used
#' eval_expr(lst,y)
#'
#'
#'


eval_expr <- function(data,expr,...,env=NULL,allow_NULL=F,verbose=F)
{
  cat0 <- mutils::chatty(verbose)

  if(is.null(env))
  {
    cat0("No environment provided, so the caller environment is used")
    env <- rlang::caller_env()
  }

  cat0("Quoting expr")
  quo_expr <- enquo(expr)


  if(!rlang::is_named(data))
  {
    cat0("data doesn't have names, so it will be wrapped in a list")
    named_data <- list(. = data,.data=data,.env=env)
  } else
  {
    cat0("data has names")
    named_data <- as.list(data)
    named_data <- c(named_data,list(.=data,.data=data,.env=env))
  }

  cat0("Checking for `...`")
  dots <- rlang::dots_list(...,.homonyms = "first")
  dot_nms <- names(dots)
  dots <- dots[dot_nms != "" & !is.environment(dots)]

  if(length(dots) > 0)
  {
    cat0("Input has usable `...` argument")
    named_data <-c(named_data,dots[!names(dots) %in% names(named_data)])
  }

  cat0("Setting the environment of the quoted expr")
  quo_expr <- rlang::quo_set_env(quo_expr,env)

  cat0("Evaluating with eval_tidy()")
  res <- rlang::eval_tidy(quo_expr,named_data,env)

  if(is.function(res))
  {
    cat0("Output was a function, so it will be applied to the data")
    res <- res(data)
  }

  if(!allow_NULL && is.null(res))
  {
    rlang::abort("eval_expr() returning NULL. Should this be allowed?")
  }
  cat0("Thanks for listening")
  res

}


