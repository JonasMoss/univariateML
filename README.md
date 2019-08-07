
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

This package provided fast and robust maximum likelihood estimation for
a selection of univariate densities. This is work in progress.

## Installation

From inside `R`, use one of the following commands:

``` r
# install.packages("devtools")
devtools::install_github("JonasMoss/univariateML")
```

## Supported Densities

| Name                     | Package | Parameters        | Density |  |
| ------------------------ | ------- | ----------------- | ------- |  |
| Normal distribution      | stats   | `mean`, `sd`      | `dnorm` |  |
| Exponential distribution | stats   | `rate`            | `dexp`  |  |
| Beta distibution         | stats   | `shape1`,`shape2` | `dbeta` |  |
|                          |         |                   |         |  |
