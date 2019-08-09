
<!-- README.md is generated from README.Rmd. Please edit that file -->

# univariateML <img src="man/figures/logo.png" align="right" width="100" height="70" />

[![Build
Status](https://travis-ci.org/JonasMoss/univariateML.svg?branch=master)](https://travis-ci.org/JonasMoss/univariateML)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/JonasMoss/univariateML?branch=master&svg=true)](https://ci.appveyor.com/project/JonasMoss/univariateML)
[![Coverage
Status](https://codecov.io/gh/JonasMoss/univariateML/branch/master/graph/badge.svg)](https://codecov.io/gh/JonasMoss/univariateML?branch=master)
[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)

An `R` package for quick and easy maximum likelihood estimation of
univariate densities.

## Overview

This package provided fast and easy maximum likelihood estimation for a
selection of univariate densities. This is work in progress.

## Installation

From inside `R`, use one of the following commands:

``` r
# install.packages("devtools")
devtools::install_github("JonasMoss/univariateML")
```

## Examples

### Model Selection

The `airquality` contains daily air quality measurements in New York
from May to September 1973. To choose a model for the wind speed
measurements from this data set we will use AIC.

``` r
library("univariateML")
AIC(mlbetapr(airquality$Wind),
    mlexp(airquality$Wind),
    mlinvgamma(airquality$Wind),
    mlgamma(airquality$Wind),
    mllnorm(airquality$Wind),
    mlrayleigh(airquality$Wind),
    mlwald(airquality$Wind),
    mlweibull(airquality$Wind))
#>                             df       AIC
#> mlbetapr(airquality$Wind)    2  859.2844
#> mlexp(airquality$Wind)       1 1011.2883
#> mlinvgamma(airquality$Wind)  2  868.5739
#> mlgamma(airquality$Wind)     2  825.0259
#> mllnorm(airquality$Wind)     2  839.7377
#> mlrayleigh(airquality$Wind)  1  856.8326
#> mlwald(airquality$Wind)      2  846.0295
#> mlweibull(airquality$Wind)   2  820.9584
```

The superior AIC of the Weibull distribution makes it a reasonable
choice.

### Plotting

The package supports easy plotting of the fitted densities.

``` r
plot(mlbeta(USArrests$Rape/100), lwd = 2, lty = 2)
lines(mlkumar(USArrests$Rape/100), lwd = 2, col = "red")
rug(USArrests$Rape/100)
```

<img src="man/figures/README-plot_example-1.png" width="750px" />

### Random variate generation

By using already implemented random variable generators, it is possible
to do for instance parametric boostrap easily.

``` r
obj = mlgumbel(airquality$Temp)
n = length(airquality$Temp)
bootstraps = replicate(n = 1000, 
                       expr = mlgumbel(rml(n, obj)))

# Calculcate approximate 95% CIs.
quantile(bootstraps[1,  ], c(0.025, 0.975))
#>     2.5%    97.5% 
#> 71.44006 74.68014
quantile(bootstraps[2,  ], c(0.025, 0.975))
#>      2.5%     97.5% 
#>  8.512211 10.946314
```

You can also use density functions (`dml`), cumulative distribution
functions (`pml`), and quantile functions (`qml`).

## Implemented Densities

Maximum likelihood estimation has been implemented for the following
densites.

| Name                       | Package    | Parameters         | Density     | Support      |
| -------------------------- | ---------- | ------------------ | ----------- | ------------ |
| Normal distribution        | stats      | `mean`, `sd`       | `dnorm`     | ℝ            |
| Logistic distributon       | stats      | `location`,`scale` | `dlogis`    | ℝ            |
| Cauchy distributon         | stats      | `location`,`scale` | `dcauchy`   | ℝ            |
| Gumbel distribution        | extraDistr | `mu`, `sigma`      | `dgumbel`   | ℝ            |
| Laplace distribution       | extraDistr | `mu`, `sigma`      | `dlaplace`  | ℝ            |
| Exponential distribution   | stats      | `rate`             | `dexp`      | \[0, ∞)      |
| Lomax distribution         | extraDistr | `lambda`, `kappa`  | `dlomax`    | \[0, ∞)      |
| Rayleigh distribution      | extraDistr | `sigma`            | `drayleigh` | \[0, ∞)      |
| Gamma distribution         | stats      | `shape`,`rate`     | `dgamma`    | (0, ∞)       |
| Weibull distribution       | stats      | `shape`,`scale`    | `dweibull`  | (0, ∞)       |
| Log-normal distribution    | stats      | `meanlog`, `sdlog` | `dlnorm`    | (0, ∞)       |
| Inverse gamma distribution | extraDistr | `alpha`, `beta`    | `dinvgamma` | (0, ∞)       |
| Beta prime distribution    | extraDistr | `shape1`, `shape2` | `dbetapr`   | (0, ∞)       |
| Wald distribution          | extraDistr | `mu`, `lambda`     | `dwald`     | (0, ∞)       |
| Beta distibution           | stats      | `shape1`,`shape2`  | `dbeta`     | (0, 1)       |
| Kumaraswamy distribution   | extraDistr | `a`, `b`           | `dkumar`    | (0, 1)       |
| Uniform distribution       | stats      | `min`, `max`       | `dunif`     | \[min, max\] |
| Power distribution         | extraDistr | `alpha`, `beta`    | `dpower`    | \[0, a)      |
| Pareto distribution        | extraDistr | `a`, `b`           | `dpareto`   | \[b, ∞)      |

*Note:* The Lomax distribution has not been properly tested yet.

## How to Contribute or Get Help

If you encounter a bug, have a feature request or need some help, open a
[Github issue](https://github.com/JonasMoss/univariateML/issues). Create
a pull requests to contribute. This project follows a [Contributor Code
of
Conduct](https://www.contributor-covenant.org/version/1/4/code-of-conduct.md).

Please open an issue and or make a pull request if you want another
univariate distribution to be added to the package.
