% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/copy_env.R
\name{copy_env}
\alias{copy_env}
\alias{copy_fun}
\title{Copy an environment or a function}
\usage{
copy_env(env)

copy_fun(fun)
}
\arguments{
\item{env}{environment to be copied}

\item{fun}{function to be copied}
}
\description{
By default, R does not create a copy of an environment when it is
assigned to a new variable. If \code{a} is an environment, \code{b <- a}
just makes \code{b} point to the same environment as \code{a}. This means
that any changes that happen to \code{b} also happen to \code{a} (or
rather, the environment that they point to). Sometimes, this
is not preferred and it would be better to explicitly make a
copy of the environment.

The \code{copy_env()} function creates a new environment with the same
parent as \code{env}, and all the objects in \code{env} are copied
(in \emph{their} usual way). The only exception to this is if \code{env}
contains an environment object, in which case, it will also be
copied using \code{copy_env()}. This also means that the only changes
occurring is the re-direction of pointers and so \code{copy_env()} is
surprisingly memory-light as it does not actually copy any large
objects.

The \code{copy_fun()} function similarly creates a copy of a function,
including that applying \code{copy_env()} to that function's
environment. This is particularly useful for \code{fseq} which rely
heavily on their function environment
}
\examples{
a <- new.env()
a$x <- 1
a$y <- "hello"
a$z <- data.frame(x = runif(20))

b <- copy_env(a)
identical(a$z, b$z)
rlang::env_label(a) == rlang::env_label(b)

f1 <- . \%>\%
  magrittr::add(2) \%>\%
  magrittr::multiply_by(2)
f2 <- f1
f3 <- copy_fun(f1)

f1_env_label <- rlang::env_label(rlang::get_env(f1))
f2_env_label <- rlang::env_label(rlang::get_env(f2))
f3_env_label <- rlang::env_label(rlang::get_env(f3))
f1_env_label == f2_env_label
f1_env_label == f3_env_label
}
