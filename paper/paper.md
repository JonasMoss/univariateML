---
title: 'univariateML: An R package for maximum likelihood estimation of univariate densities'
tags:
  - R
  - statistics
  - maximum likelihood
  - density estimation
authors:
  - name: Jonas Moss
    orcid: 0000-0002-6876-6964
    affiliation: 1
affiliations:
 - name: University of Oslo
   index: 1
output:
  github_document:
    html_preview: true
bibliography: paper.bib
date: 1 November 2019
---

# Summary

`univariateML` is an R (R Core Team 2019) package for doing univariate
maximum likelihood estimation (Cam 1990). It supports more than 20
densities, the most popular generic functions such as `plot`, `AIC`, and
`confint`, and a simple parametric bootstrap (Efron and Tibshirani 1994)
interface.

When looking at univariate data it is natural to ask if there is a known
parametric density that fits the data well. The following example uses
the `egypt` (Pearson 1902) data set included in the package and a plot
of the Weibull and Gamma densities (Johnson, Kotz, and Balakrishnan
1995, Chapter 17 & 21).

``` r
# install.packages("univariateML")
library("univariateML")
hist(egypt$age, freq = FALSE, main = "Mortality in Egypt", xlab = "Mortality")
lines(mlweibull(egypt$age)) # Plots a Weibull fit.
lines(mlgamma(egypt$age), col = "red")  # Plots a Gamma fit.
```

![](paper_files/figure-gfm/figure-1.png)<!-- -->

Another natural question is to ask which among several models fits the
data best. This can be done using tools of model selection such as the
`AIC` (Akaike 1998).

``` r
AIC(mlweibull(egypt$age),
    mlgamma(egypt$age))
# >                      df      AIC
# > mlweibull(egypt$age)  2 1230.229
# > mlgamma(egypt$age)    2 1234.772
```

Problems involving estimation of univariate densities are common in
statistics. Estimation of univariate densities is used in for instance
exploratory data analysis, in the estimation of copulas (Ko, Hjort, and
Hobæk Haff 2019), as parametric starts in density estimation (Hjort and
Glad 1995; Moss and Tveten 2019), and is of interest in and of itself.

The purpose of `univariateML` is to make maximum likelihood estimation
quick, easy, and free of bugs. It has custom made optimizers for each
supported density. This is in contrast to the `mle` function in the
built-in `R` package `stats4`, which supports far more general maximum
likelihood estimation through numerical optimization on a supplied
negative log-likelihood function.

# References
