
<!-- README.md is generated from README.Rmd. Please edit that file -->

# refactor

[![Project Status: Abandoned – Initial development has started, but
there has not yet been a stable, usable release; the project has been
abandoned and the author(s) do not intend on continuing
development.](https://www.repostatus.org/badges/latest/abandoned.svg)](https://www.repostatus.org/#abandoned)
[![Build
Status](https://travis-ci.org/jonmcalder/refactor.svg?branch=master)](https://travis-ci.org/jonmcalder/refactor)
[![codecov](https://codecov.io/gh/jonmcalder/refactor/branch/master/graph/badge.svg)](https://codecov.io/gh/jonmcalder/refactor)

**refactor** is an R package which aims to provide better handling of
factors. Though R does have a special data type for factors, it isn’t
always explicity catered for in commonly used R functions, which can
lead to unexpected and undesirable outcomes (e.g. see this [Win-Vector
blog
post](http://www.win-vector.com/blog/2014/09/factors-are-not-first-class-citizens-in-r/)).
This observation formed the inspiration for ‘re’-factor: which is
essentially to ‘re’-visit functions likely to be used with factor data
and where possible to wrap, extend or override them in order to better
cater for factor data, or at the very least to provide warnings when the
integrity of the data could be compromised by an operation.

## Installation

refactor is not available on CRAN but you can easily install the latest
development version from github using `devtools`:

``` r
# install.packages("devtools")
devtools::install_github("jonmcalder/refactor")
```

## Overview

Below are a few examples to illustrate some scenarios in which refactor
can improve on base R’s handling of factors.

Please see the vignette for more details:
`vignette("refactor", package = "refactor")`

#### Better factor creation with `cfactor()`

-   get warnings whenever provided factor levels don’t fully match those
    present in the data

``` r
string <- c("a", "b", "c")
factor(string, levels = c("b", "c", "d"))
#> [1] <NA> b    c   
#> Levels: b c d
cfactor(string, levels = c("b", "c", "d"))
#> Warning: the following levels were empty: 
#>  d
#> Warning: the following levels were removed: 
#>  a
#> [1] <NA> b    c   
#> Levels: b c d
```

-   numerical sequences in strings are detected and utilized to obtain
    correct orderings for factor levels

``` r
hard_to_detect <- c("EUR 21 - EUR 22", "EUR 100 - 101", "EUR 1 - EUR 10", "EUR 11 - EUR 20")
factor(hard_to_detect, ordered = TRUE)
#> [1] EUR 21 - EUR 22 EUR 100 - 101   EUR 1 - EUR 10  EUR 11 - EUR 20
#> 4 Levels: EUR 1 - EUR 10 < EUR 100 - 101 < ... < EUR 21 - EUR 22
cfactor(hard_to_detect, ordered = TRUE)
#> [1] EUR 21 - EUR 22 EUR 100 - 101   EUR 1 - EUR 10  EUR 11 - EUR 20
#> 4 Levels: EUR 1 - EUR 10 < EUR 11 - EUR 20 < ... < EUR 100 - 101
```

#### Extension of the generic `cut` method to handle (discrete) integer data

-   generate ‘natural’ intervals for factors when using cut on integer
    vectors
    -   e.g. `[1,3], [4,6], [7,9]` as opposed to
        `(0.5, 3.5], (3.5, 6.5], (6.5, 9.5]`
-   label factor levels more intuitively
    -   e.g. `1-3, 4-6, 7-9` as opposed to
        `[1,3], [4,6], [7,9], or (0,3], (3,6], (6-9]`

``` r
x_int <- 1:9
cut.default(x_int, breaks = 3)
#> [1] (0.992,3.67] (0.992,3.67] (0.992,3.67] (3.67,6.33]  (3.67,6.33] 
#> [6] (3.67,6.33]  (6.33,9.01]  (6.33,9.01]  (6.33,9.01] 
#> Levels: (0.992,3.67] (3.67,6.33] (6.33,9.01]
cut(x_int, breaks = 3)
#> [1] 1-3 1-3 1-3 4-6 4-6 4-6 7-9 7-9 7-9
#> Levels: 1-3 4-6 7-9
```

#### Extension of the generic `cut` method to handle (non-numeric) ordered data

-   `cut` usually requires numeric data, but refactor extends `cut` to
    handle other ordered data

``` r
some_letters <- factor(c('d','e','f','a','b','c','g','h','i'), ordered = TRUE)
cut(some_letters, breaks = c('a','c','f','i'), include.lowest = TRUE, ordered_result = TRUE)
#> [1] d-f d-f d-f a-c a-c a-c g-i g-i g-i
#> Levels: a-c < d-f < g-i
```

## Contributions

Suggestions and feedback are most welcome.

Feel free to [open an
issue](https://github.com/jonmcalder/refactor/issues) if you want to
request a feature or report a bug, and make a pull request if you can
contribute.
