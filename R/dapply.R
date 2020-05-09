#' @name dapply
#'
#' @title
#' Apply a function downwards through a nested list
#'
#' @description
#' `dapply()` works similarly to the traditional `apply()`
#' family of functions, except it will dig downwards into
#' a nested list. It will apply the `FUN` function to
#' the list and then, reapply it to an `element` in that
#' list. It repeats this until `element` is no longer found
#'
#'
#' @param X
#' a nested list
#'
#' @param element
#' a single integer or character used to refer to an element in a list
#' via the `[[`, `]]` subsetting, by default this will be the name of `X`
#'
#' @param FUN
#' the function to be applied to each list
#'
#' @param simplify
#' a function or `NULL`. If a function is supplied, it will be
#' applied to the results to simplify them
#'
#' @param rev
#' logical. Should the elements be returned in reverse order
#' (i.e. top element first or bottom element first)
#'
#'
#' @examples
#'
#' x.vect <- 1:5
#' y.vect <- letters[1:5]
#' z <- list(x=1,y="a")
#'
#' for(i in 2:5)
#' {
#'   z <- list(x = x.vect[i],
#'             y = y.vect[i],
#'             z = z)
#' }
#'
#' FUN <- function(X) c(X$x,X$y)
#'
#' dapply(z,FUN,simplify=rbind)


dapply <- function(X,FUN,...,element=as.character(substitute(X)),simplify=NULL,rev=F)
{
  FUN <- match.fun(FUN)
  res <- list()
  cX <- X
  count <- 1

  res[[count]] <- FUN(cX)

  while(length(cX)>1 && !is.null(cX[[element]]))
  {
    cX <- cX[[element]]
    count <- count+1
    res[[count]] <- FUN(cX)

  }
  if(rev) res <- rev(res)

  if(length(res) == 1) res <- res[[1]]
  else if(!is.null(simplify))
  {
    res2 <- res[[1]]
    for(i in 2:length(res))
    {
      res2 <- simplify(res2,res[[i]])
    }
    row.names(res2) <- NULL
    res <- res2
  }

  return(res)
}





