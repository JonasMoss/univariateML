
<!-- README.md is generated from README.Rmd. Please edit that file -->

# univariateML <img src="man/figures/logo.png" align="right" width="100" height="70" />

[![Build
Status](https://travis-ci.org/JonasMoss/univariateML.svg?branch=master)](https://travis-ci.org/JonasMoss/univariateML)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/JonasMoss/univariateML?branch=master&svg=true)](https://ci.appveyor.com/project/JonasMoss/univariateML)
[![Coverage
Status](https://codecov.io/gh/JonasMoss/univariateML/branch/master/graph/badge.svg)](https://codecov.io/gh/JonasMoss/univariateML?branch=master)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![DOI](https://joss.theoj.org/papers/10.21105/joss.01863/status.svg)](https://doi.org/10.21105/joss.01863)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/univariateML)](https://cran.r-project.org/package=univariateML)

## Overview

[`univariateML`](https://univariateml.netlify.com/index.html) is an
`R`-package for user-friendly maximum likelihood estimation of a
[selection](https://univariateml.netlify.com/articles/distributions.html)
of parametric univariate densities. In addition to basic estimation
capabilities, this package support visualization through `plot` and
`qqmlplot`, model selection by `AIC` and `BIC`, confidence sets through
the parametric bootstrap with `bootstrapml`, and convenience functions
such as the density, distribution function, quantile function, and
random sampling at the estimated distribution parameters.

## Installation

Use the following command from inside `R` to install from CRAN.

``` r
install.packages("univariateML")
```

Or install the development version from Github.

``` r
# install.packages("devtools")
devtools::install_github("JonasMoss/univariateML")
```

## Usage

The core of `univariateML` are the `ml***` functions, where `***` is a
distribution suffix such as `norm`, `gamma`, or `weibull`.

``` r
library("univariateML")
mlweibull(egypt$age)
#> Maximum likelihood estimates for the Weibull model 
#>  shape   scale  
#>  1.404  33.564
```

Now we can visually assess the fit of the Weibull model to the `egypt`
data with a plot.

``` r
hist(egypt$age, freq = FALSE, xlab = "Mortality", main = "Egypt")
lines(mlweibull(egypt$age))
```

<img src="man/figures/README-weibull_plot-1.png" width="750px" />

## Implementations

Analytic formulae for the maximum likelihood estimates are used whenever
they exist. Most `ml***` functions without analytic solutions have a
custom made Newton-Raphson solver. These can be much faster than a naïve
solution using `nlm` or `optim`. For example, `mlbeta` has a large
speedup over the naïve solution using `nlm`.

``` r
# install.packages("microbenchmark")
set.seed(313)
x <- rbeta(500, 2, 7)

microbenchmark::microbenchmark(
  univariateML = univariateML::mlbeta(x),
  naive = nlm(function(p) -sum(dbeta(x, p[1], p[2], log = TRUE)), p = c(1, 1)))
#> Unit: microseconds
#>          expr       min        lq      mean    median        uq      max neval
#>  univariateML   925.401  1339.002  2309.141  1708.051  2065.551  41581.2   100
#>         naive 44287.601 47740.601 54418.306 51779.401 57391.700 104036.7   100
```

The maximum likelihood estimators in this package have all been subject
to testing, see the `tests` folder for details.

## Documentation

For an overview of the package and its features see the [overview
vignette](https://univariateml.netlify.com/articles/overview.html). For
a list of implemented densities see the start of the [details
vignette](https://univariateml.netlify.com/articles/distributions.html).
For an illustration of how this package can make an otherwise long and
laborious process much simpler, see the [copula
vignette](https://univariateml.netlify.com/articles/copula.html).

## How to Contribute or Get Help

Please read `CONTRIBUTING.md` for details about how to contribute or get
help.
