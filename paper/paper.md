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
date: 1 November 2019   
bibliography: paper.bib
---

# Summary

`univariateML` is an R [@r] package for doing univariate maximum likelihood estimation [@lecam1990ml]. 
It supports more than 20 densities, the most popular generic functions such as `plot`, `AIC`, and `confint`, and a simple parametric bootstrap [@efron1994introduction] interface.

When looking at univariate data it is natural to ask if there is a known 
parametric density that fits the data well. The following example uses the 
`egypt` [@pearson1902egypt] data set included in the package and a plot of the Weibull and Gamma
densities [@johnson1970continuous, Chapter 17 & 21]. 


``` r
# install.packages("univariateML")
library("univariateML")
hist(egypt$age, freq = FALSE, main = "Mortality", xlab = "Mortality")
lines(mlweibull(egypt$age)) # Plots a Weibull fit.
lines(mlgamma(egypt$age), col = "red")  # Plots a Gamma fit.
```

![Egypt mortality data](paper_files/figure-gfm/figure-1.png)

Another natural question to ask is which among several models fits the data best.
This can be done using tools of model selection such as the `AIC` [@akaike1998information].

``` r
AIC(mlweibull(egypt$age), mlgamma(egypt$age))
# >                      df      AIC
# > mlweibull(egypt$age)  2 1230.229
# > mlgamma(egypt$age)    2 1234.772
```

Problems involving estimation of univariate densities are common in statistics. 
Estimation of univariate densities is used in for instance exploratory data analysis, 
in the estimation of copulas [@ko2019focused],
as parametric starts in density estimation [@hjort_glad_1995; @moss2019kdensity], 
and is of interest in and of itself. 

The purpose of `univariateML` is to make maximum likelihood estimation quick, 
easy, and free of bugs. It has custom made optimizers for each supported density.
This is in contrast to the `mle` function in the built-in `R` package `stats4`,
which supports far more general maximum likelihood estimation through numerical
optimization on a supplied negative log-likelihood function.

# References
