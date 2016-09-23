
<!-- README.md is generated from README.Rmd. Please edit that file -->
refactor
========

[![Build Status](https://travis-ci.org/jonmcalder/refactor.svg?branch=master)](https://travis-ci.org/jonmcalder/refactor) [![codecov](https://codecov.io/gh/jonmcalder/refactor/branch/master/graph/badge.svg)](https://codecov.io/gh/jonmcalder/refactor)

> refactor is an R package which hopes to provide better handling of factors by attempting to address some of the known problems and short-comings present in R
> (e.g. as outlined in this [Win-Vector blog post](http://www.win-vector.com/blog/2014/09/factors-are-not-first-class-citizens-in-r/))

Installation
------------

refactor is not available on CRAN but you can easily install the latest version from github using `devtools`:

``` r
# install.packages("devtools")
devtools::install_github("jonmcalder/refactor")
```

Overview
--------

Below are some of the highlights.

#### Better factor creation:

-   sequences in strings are detected and used to order the factor levels automatically
    -   e.g. EUR11-20, EUR51-EUR60, EUR101-EUR110
-   get warnings whenever provided factor levels don't fully match those present in the data

#### Extension of the generic `cut` method to handle (discrete) integer data:

-   generate 'natural' intervals for factors when using cut on integer vectors
    -   e.g. `[1,3], [4,6], [7,9]` as opposed to `(0.5, 3.5], (3.5, 6.5], (6.5, 9.5]`
-   label factor levels more intuitively
    -   e.g. 1-3, 4-6, 7-9 as opposed to \[1,3\], \[4,6\], \[7,9\], or (0,3\], (3,6\], (7-9\]

Contributions
-------------

Any suggestions and/or feedback is most welcome.

Feel free to [open an issue](https://github.com/jonmcalder/refactor/issues) if you want to request a feature/report a bug, or make a pull request if you can contribute.
