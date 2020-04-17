#' @name
#' pipe-func
#'
#' @title
#' Run a pipe as a function
#'
#' @description
#' Function to run a part of a pipeline in function notation,
#'  a wrapper around `%>%`
#'
#'@details
#' Converts the input `lhs` and `rhs` as call strings,
#' concatenates them with the general pipe
#'
#' @param lhs
#' The left hand side of the pipe
#'
#' @param rhs
#' The right had side of the pipe
#'
#' @return
#' evaluates the pipe `lhs %>% rhs`
#'
#' @export
#'

pipe_func <- function(lhs,rhs)
{
  requireNamespace("rlang",quietly=T)
  requireNamespace("magrittr",quietly=T)
  .qlhs <- rlang::enquos(lhs)
  .qrhs <- rlang::enquos(rhs)
  .slhs <- names(rlang::quos_auto_name(.qlhs))
  .srhs <- names(rlang::quos_auto_name(.qrhs))
  .scall <- paste0(.slhs," %>% ",.srhs)

  .res <- eval.parent(parse(text=.scall),1)
  return(.res)
}
