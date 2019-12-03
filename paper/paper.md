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
date: 25 November 2019
---

# Summary

`univariateML` is an R [@r] package for user-friendly univariate maximum 
likelihood estimation [@lecam1990ml]. It supports more than 20 densities, 
the most popular generic functions such as `plot`, `AIC`, and `confint`,
and a simple parametric bootstrap [@efron1994introduction] interface.

When looking at univariate data it is natural to ask if there is a known 
parametric density that fits the data well. The following example uses the 
`egypt` [@pearson1902egypt] data set included in the package and a plot of 
the Weibull and Gamma densities [@johnson1970continuous, Chapter 17 & 21]. 

``` r
# install.packages("univariateML")
library("univariateML")
hist(egypt$age, freq = FALSE, main = "Mortality", xlab = "Mortality")
lines(mlweibull(egypt$age)) # Plots a Weibull fit.
lines(mlgamma(egypt$age), col = "red")  # Plots a Gamma fit.
```

![](paper_files/figure-gfm/figure-1.png)<!-- -->

A natural question to ask is which among several models fits the data best.
This can be done using tools of model selection such as the `AIC` 
[@akaike1998information].

``` r
AIC(mlweibull(egypt$age),
    mlgamma(egypt$age))
```

    ##                      df      AIC
    ## mlweibull(egypt$age)  2 1230.229
    ## mlgamma(egypt$age)    2 1234.772

Problems involving estimation of univariate densities are common in statistics. 
Estimation of univariate densities is used in for instance exploratory data 
analysis, in the estimation of copulas [@ko2019focused], as parametric starts 
in density estimation [@hjort_glad_1995; @moss2019kdensity], and is of interest 
in and of itself. 

Analytic formulas for the maximum likelihood estimates are used whenever they 
exist. Most estimators without analytic solutions have  a custom made 
Newton-Raphson solver.  This is in contrast to the `mle` function 
in the built-in `R` package `stats4`, which supports more general maximum 
likelihood estimation through numerical optimization on a supplied negative 
log-likelihood function.

`Rfast` [@Rfast] is an `R` package with fast Newton-Raphson implementations of 
many univariate density estimators. `univariateML` differs from `Rfast` 
mainly in focus: While `univariateML`is focused on user-friendly univariate 
density estimation, `Rfast` aims to have the fastest possible implementations 
of many kinds of functions.

# References

