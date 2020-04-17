#'@name if-pipe
#'
#'@title
#' Function to branch within a pipeline
#'
#'@description
#' Provides the ability to branch your pipeline without breaking it
#'
#'@param .x
#' Input data that is being fed through the pipeline
#'
#'@param .predicate
#' Logical value or vector of logicals indicating whether
#' `.fun` or `.elsefun` should be ran
#'
#'@param .fun
#' Pipeline or list of pipeline functions to be ran if .predicate evaluates to `TRUE`
#'
#'@param .elsefun
#' Pipeline or list of pipeline functions to be ran if .predicate evaluates to `FALSE`
#' If `NULL`, no function is applied when `.predicate == FALSE`
#'
#' @export
#'
#' @examples
#' iris %>%
#'   if_pipe(T,
#'           mutate(Sepal.Area = Sepal.Length*Sepal.Width) %>%
#'             head,
#'           summarise(Sepal.Area = sum(Sepal.Length*Sepal.Width)))
#'
#' iris %>%
#'   if_pipe(F,
#'           mutate(Sepal.Area = Sepal.Length*Sepal.Width) %>%
#'             head,
#'           summarise(Sepal.Area = sum(Sepal.Length*Sepal.Width)))
#'

if_pipe <- function(.x,.predicate,.fun,.elsefun=NULL)
{
  requireNamespace("rlang",quietly=T)
  if(!is.logical(.predicate))
    stop(".predicate should be logical in ",.match.call())
  if(length(.predicate)>1)
    stop(".predicate should be length 1, not ",
         length(.predicate),
         " in",match.call())
  if(.predicate)
  {
    .res <- pipe_func(.x,!!rlang::enquo(.fun))
  } else if(!rlang::quo_is_null(enquo(.elsefun)))
  {
    .res <- pipe_func(.x,!!rlang::enquo(.elsefun))
  } else {
    .res <- .x
  }
  return(.res)
}

iris %>%
  if_pipe(T,
          mutate(Sepal.Area_AM = Sepal.Length*Sepal.Width) %>%
            head,
          summarise(Sepal.Area_PM = sum(Sepal.Length*Sepal.Width)))
