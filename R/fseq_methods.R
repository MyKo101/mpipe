#' @name fseq_methods
#'
#' @title
#' Generic methods for fseq manipulations
#'
#' @description
#' When pipelines are created without an initial term, the
#' \code{\link{magrittr}} package (where the pipes originate)
#' creates a specialised type of function called a \code{fseq}.
#' However, \code{\link{magrittr}} doesn't do much with these
#' \code{fseq} objects.
#'
#' @param lhs
#' `fseq` to be performed first
#'
#' @param rhs
#' `fseq` to be performed second
#'
#' @examples
#' a <- . %>% `+`(2) %>% `/`(3)
#' b <- . %>% rep(3) %>% sum
#'
#' a + b
#'
#' (a+b)(2)
#' (b+a)(2)
#'
#'
#'
#' @export


`+.fseq` <- function(lhs,rhs)
{
  new_lhs <- copy_fun(lhs)

  new_function_list <- c(functions(new_lhs),
                         vector("list",length=length(rhs)))

  rhs_fl <- functions(rhs)

  for(i in 1:length(rhs))
  {
    new_function_list[[length(lhs)+i]] <- copy_fun(rhs_fl[[i]])
  }

  fun_var_env(new_lhs,"_function_list",new_function_list)

  new_lhs


}

#' @rdname fseq_methods
#' @export
#'
#' @examples
#' length(a)
#'
#' @param x
#' an `fseq` object to extract the length of. Here, `length(x)` is
#' defined as the number of functions in the sequence.

length.fseq <- function(x)
{
  length(magrittr::functions(x))
}

#' @rdname fseq_methods
#' @export
#'
#' @examples
#' is.fseq(a)
#'
#' @param object
#' object to be tested.
#'

is.fseq <- function(object)
{
  "fseq" %in% class(object)
}


#' @rdname fseq_methods
#' @export
#'
#' @examples
#' is_fseq(a)
#'

is_fseq <- function(object)
{
  "fseq" %in% class(object)
}

