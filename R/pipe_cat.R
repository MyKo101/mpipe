#' @name pipe_cat
#'
#' @title
#' Pipe-able version of the `cat()`
#'
#' @description
#' The `pipe_cat()` function allows messages to be output to
#' the console or to an external file without the need to break out
#' of a pipeline to do so.
#'
#' @param data
#' the data being passed through the pipeline
#'
#' @param ...
#' arguments to be passed to the `cat()` function. Arguments will be
#' evaluated in the context of `data` before being passed to `cat()`
#'
#' @param file,sep,fill,labels,append
#' See the [`cat()`][base::cat()] documentation for more
#' information regarding these arguments.
#'
#'
#' @export
#'
#' @examples
#'
#' sample(100,1) %>%
#'   runif() %>%
#'   pipe_cat("Current length: ",length(.),"\n") %>%
#'   pipe_cat("Current average: ",mean(.),"\n") %>%
#'   pipe_cat("Current standard error: ",sd(.)/length(.),"\n") %>%
#'   pipe_cat("Returning mean:\n") %>%
#'   mean
#'
#' tibble::tibble(x = runif(10),
#'                y = runif(10)) %>%
#'   pipe_cat("Average x: ", mean(x),"\n") %>%
#'   pipe_cat("Current number of rows: ",nrow,"\n") %>%
#'   dplyr::mutate(z = x + y)
#'
#' iris %>%
#'   dplyr::mutate(Species = as.character(Species)) %>%
#'   pipe_cat("Total average Sepal Length: ",mean(Sepal.Length),"\n") %>%
#'   dplyr::group_by(Species) %>%
#'   pipe_cat(Species," average Sepal Length: ",mean(Sepal.Length),"\n")
#'
#'

pipe_cat <- function(data,...,file = "", sep = " ", fill = FALSE,
                     labels = NULL, append = FALSE)
{
  if(dplyr::is_grouped_df(data))
  {
    grp_tbl <- dplyr::group_keys(data)
    data_split <- dplyr::group_split(data,keep=F)

    if(length(data_split) != nrow(grp_tbl))
      rlang::abort("groups returned are inconsistent in pipe_cat")

    for(i in 1:length(data_split))
    {
      c_grp <- as.list(grp_tbl[i,])
      c_data <- data_split[[i]]

      grp_list <- c(c_grp,as.list(c_data))

      pipe_cat(grp_list, ..., file = file, sep = sep,
               fill = fill, labels = labels, append = append)
    }


  } else
  {
    parent <- rlang::caller_env()
    env <- rlang::new_environment(parent=parent)
    env[["data"]] <- data

    fs <- as.list(enquos(...))

    fs_eval <- rlang::new_list(length(fs))


    for(i in 1:length(fs))
    {
      fs_eval[[i]] <- eval_expr(data,!!fs[[i]],env=env)
    }

    cat_args <- list(file = file, sep = sep, fill = fill,
                     labels = labels, append = append)

    cat_call <- as.call(c(quote(cat),fs_eval,cat_args))

    eval(cat_call)
  }

  data
}

