
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mpipe <img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start --> [![R build
status](https://github.com/MyKo101/mpipe/workflows/R-CMD-check/badge.svg)](https://github.com/MyKo101/mpipe/actions)
[![Codecov test
coverage](https://codecov.io/gh/MyKo101/mpipe/branch/master/graph/badge.svg)](https://codecov.io/gh/MyKo101/mpipe?branch=master)
<!-- badges: end -->

The `mpipe` package is designed to add to extra functionality to the
“pipelining” of `tidyverse` style processes. When running a pipeline,
it is common to break out of a pipeline to perform other actions, and
then to commence a new pipeline. `mpipe` aims to permit a stronger use
of pipelining, whilst reducing the need to break out. This piece of
software is still under very active development and suggestions for
improvements are welcome.

## Installation

You can install the development version of `mpipe` from
[GitHub](https://github.com/MyKo101/mpipe) with:

``` r
# install.packages("devtools")
#devtools::install_github("MyKo101/mpipe")
library(mpipe)
```

The `mpipe` package is not currently available on CRAN.

## Side Effects

A large reason for breaking out of a pipeline is to do actions such as
plotting or providing feedback to the user. the `mpipe` package provides
two functions that are particularly useful for avoiding leaving a
pipeline. The way this is done is that the functions side effects are
activated, but the functions return an untouched version of the data it
was originally provided.

  - `pipe_qplot()` - allows the user to create `ggplot2` style plots
    using `qplot()` syntax (akin to the base R `plot()` syntax)
  - `pipe_cat()` - allows the user to output information to the console
    (or any other sink) in the much the same way that `cat()` does. Any
    calls/functions will be evaluated appropriately for this output.

<!-- end list -->

``` r
iris %>%
  tibble %>%
  select(Species,Sepal.Length,Sepal.Width) %>%
  pipe_qplot(Sepal.Length,Sepal.Width,col=Species) %>%
  pipe_cat("Total Average Sepal.Length: ",mean(Sepal.Length),"\n") %>%
  group_by(Species) %>%
  pipe_cat(as.character(Species)," Average Sepal.Length: ",mean(Sepal.Length),"\n") %>%
  summarise(Length_mean = mean(Sepal.Length),
            Width_mean = mean(Sepal.Width))
```

<img src="man/figures/README-side_effects_example-1.png" width="100%" />

    #> Total Average Sepal.Length:  5.843333 
    #> setosa  Average Sepal.Length:  5.006 
    #> versicolor  Average Sepal.Length:  5.936 
    #> virginica  Average Sepal.Length:  6.588
    #> # A tibble: 3 x 3
    #>   Species    Length_mean Width_mean
    #>   <fct>            <dbl>      <dbl>
    #> 1 setosa            5.01       3.43
    #> 2 versicolor        5.94       2.77
    #> 3 virginica         6.59       2.97

## Control Flow

One of the key aspects of programming is being able to manipulate which
lines of code are ran and when. This manipulation is called “Control
Flow” (i.e. you control the flow of the processes through your code).
The two main methods of this are via branches and loops. Branches force
our code to make a choice, where loops allow repetition of the same
lines of code.

In traditional R (as in many other languages), Branching is performed
using `if()` or `switch()` statements, and can also be vectorised with
functions such as `if_else()` and `case_when()`. Loops are implemented
using the `for()`, `while()` and `repeat()` statements and can
essentially be vectorised using the `apply()` and `map()` families of
functions.

### Branches

The `if_branch()` and `switch_branch()` functions. The `if_branch()`
function chooses whether to proceed with the `fun` branch or the
`elsefun` branch (if supplied) depending on how `predicate` is
evaluated.

``` r
 1 %>%
  multiply_by(2) %>%
  if_branch(. %>% equals(2),
            . %>%
            multiply_by(3) %>%
            add(2)) %>%
  multiply_by(2)
#> [1] 16

 2 %>%
  multiply_by(2) %>%
  if_branch(. %>% equals(2),
            . %>%
            multiply_by(3) %>%
            add(2)) %>%
  multiply_by(2)
#> [1] 8
```

Similarly, the `switch_branch()` function allows us to expand on this by
following a different path depending on the evaluation of `case`

``` r

set.seed(100)
iris %>%
  sample_n(nrow(.)) %>%
  switch_branch(. %>%
                  slice(1) %>%
                  pull("Species") %>%
                  as.character,
                setosa = . %>%
                  pipe_cat("setosa was one top\n") %>%
                  filter(Species == "setosa"),
                versicolor = . %>%
                  pipe_cat("versicolor was on top\n") %>%
                  filter(Species == "versicolor"),
                virginica = . %>%
                  pipe_cat("virginica was on top\n") %>%
                  filter(Species == "virginica")) %>%
  slice(1)
#> virginica was on top
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width   Species
#> 1          5.8         2.7          5.1         1.9 virginica

set.seed(1000)
iris %>%
  sample_n(nrow(.)) %>%
  switch_branch(. %>%
                  slice(1) %>%
                  pull("Species") %>%
                  as.character,
                setosa = . %>%
                  pipe_cat("setosa was one top\n") %>%
                  filter(Species == "setosa"),
                versicolor = . %>%
                  pipe_cat("versicolor was on top\n") %>%
                  filter(Species == "versicolor"),
                virginica = . %>%
                  pipe_cat("virginica was on top\n") %>%
                  filter(Species == "virginica")) %>%
  slice(1)
#> versicolor was on top
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
#> 1          5.8         2.7          4.1           1 versicolor

set.seed(10000)
iris %>%
  sample_n(nrow(.)) %>%
  switch_branch(. %>%
                  slice(1) %>%
                  pull("Species") %>%
                  as.character,
                setosa = . %>%
                  pipe_cat("setosa was one top\n") %>%
                  filter(Species == "setosa"),
                versicolor = . %>%
                  pipe_cat("versicolor was on top\n") %>%
                  filter(Species == "versicolor"),
                virginica = . %>%
                  pipe_cat("virginica was on top\n") %>%
                  filter(Species == "virginica")) %>%
  slice(1)
#> setosa was one top
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1          4.7         3.2          1.6         0.2  setosa
```

### Loops

In terms of loops, we have the `while_pipe()` function which acts
similar to a `while()` loop, but within a pipeline. It will repeatedly
apply the `fun` argument to the `data` until `cond` is not `TRUE`.
`cond` will be evaluated within the context of `data`. The
`while_pipe()` function also provides a `.counter` pronoun to keep track
of how many times the loop as been run.

``` r
 tibble::tibble(x = runif(5)) %>%
   while_pipe(.counter <= 5,
              . %>%
                dplyr::mutate(!!paste0("x_",.counter) := x - x[.counter]))
#> # A tibble: 5 x 6
#>        x    x_1   x_2    x_3    x_4    x_5
#>    <dbl>  <dbl> <dbl>  <dbl>  <dbl>  <dbl>
#> 1 0.795   0     0.772  0.133  0.241  0.342
#> 2 0.0230 -0.772 0     -0.638 -0.530 -0.430
#> 3 0.661  -0.133 0.638  0      0.108  0.208
#> 4 0.553  -0.241 0.530 -0.108  0      0.101
#> 5 0.453  -0.342 0.430 -0.208 -0.101  0
```

## Code of Conduct

Please note that the mpipe project is released with a [Contributor Code
of Conduct](https://michaelbarrowman.co.uk/mpipe/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
