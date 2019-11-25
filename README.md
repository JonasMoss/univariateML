
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

## Overview

An `R`-package for fast, easy, and reliable maximum likelihood
estimation for a
[selection](https://univariateml.netlify.com/articles/distributions.html)
of parametric univariate densities. In addition to basic estimation
capabilities, this package support visualization through `plot` and
`qqmlplot`, model selection by `AIC` and `BIC`, confidence sets through
the parametric bootstrap with `bootstrapml`, and convenience functions
such as the density, distribution function, quantile function, and
random sampling at the estimated distribution parameters.

## Installation

Use the following command from inside `R`:

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
custom made Newton-Raphson solver. These can be much faster than the
naïve solvers using `nlm` or `optim`. For example, `mlbeta` has a large
speedup.

``` r
# install.packages("microbenchmark")
set.seed(313)
x <- rbeta(500, 2, 7)

microbenchmark::microbenchmark(
  univariateML = univariateML::mlbeta(x),
  naive = nlm(function(p) -sum(dbeta(x, p[1], p[2], log = TRUE)), p = c(1, 1)))
#> Unit: microseconds
#>          expr     min       lq      mean   median       uq     max neval
#>  univariateML   864.2  1102.90  1305.743  1192.30  1340.95  6007.9   100
#>         naive 36398.0 39097.35 41108.804 40530.15 43035.50 50693.3   100
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
