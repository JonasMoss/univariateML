
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
estimation for a [selection](/vignettes/distributions.html) of
parametric univariate densities. In addition to basic estimation
capabilities, this package support visualization through `plot` and
`qqmlplot`, model selection by `AIC` and `BIC`, and confidence sets
through the parametric bootstrap with `bootstrapml`.

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

## Documentation

For an overview of the package and its features see the [overview
vignette](https://univariateml.netlify.com/articles/overview.html). For
a list of implemented densities see the start of the [details
vignette](https://univariateml.netlify.com/articles/distributions.html).
For an illustration of how this package can make an otherwise long and
laborious process much simpler, see the [copula
vignette](https://univariateml.netlify.com/articles/copula.html).

## How to Contribute or Get Help

If you encounter a bug, have a feature request or need some help, open a
[Github issue](https://github.com/JonasMoss/univariateML/issues). If you
miss a particular estimator, make an issue explaining why its
implementation should be prioritized.

We encourage you to make a pull request if you wish to make a
contribution to this package. If you wish to contribute a new maximum
likelihood estimator to this package please read [this page
first](https://github.com/JonasMoss/univariateML/wiki/Adding-New-Densities).

This project follows a [Contributor Code of
Conduct](/CODE-OF-CONDUCT.md).
