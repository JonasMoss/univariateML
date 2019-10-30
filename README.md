
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
estimation for a selection of parameteric univariate densities. In
addition to basic estimation capabilities, this package support
visualization through `plot` and `qqmlplot`, model selection by `AIC`
and `BIC`, and confidence sets through the parametric bootstrap with
`bootstrapml`.

## Installation

From inside `R`, use one of the following commands:

``` r
# install.packages("devtools")
devtools::install_github("JonasMoss/univariateML")
```

## Example Usage

``` r
library("univariateML")
mlweibull(egypt$age)
#> Maximum likelihood estimates for the Weibull model 
#>  shape   scale  
#>  1.404  33.564
```

``` r
hist(egypt$age, freq = FALSE, xlab = "Mortality", main = "Egypt")
lines(mlweibull(egypt$age))
```

<img src="man/figures/README-weibull_plot-1.png" width="750px" />

## Documentation

For an overview of the package and its features see the [overview
vignette](/vignettes/overview.html). For a list of implemented densities
see the start of the [details vignette](/vignettes/distributions.html).
For an application of the package to copula modelling see the [copula
vignette](/vignettes/copula.html).

## How to Contribute or Get Help

If you encounter a bug, have a feature request or need some help, open a
[Github issue](https://github.com/JonasMoss/univariateML/issues). Create
a pull requests to contribute. This project follows a [Contributor Code
of
Conduct](https://www.contributor-covenant.org/version/1/4/code-of-conduct.md).

Please open an issue and or make a pull request if you want another
univariate distribution to be added to the package.
