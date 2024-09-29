---
title: "Overview of univariateML"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Overview of univariateML}
  %\VignetteEngine{knitr::knitr}
  %\VignetteEncoding{UTF-8}
---



When dealing with univariate data you want to do one or more of

* Find a good model for the data.
* Estimate parameters for your candidate models.
* Get an idea about the uncertainty of your estimates.

The `unvariateML` package has a fast and reliable functions to help you with 
these tasks. The core of the package are more than 20 functions for fast 
and thoroughly tested calculation of maximum likelihood estimates for univariate 
models. 

* Compare the fit of your candidate models with `AIC` or `BIC`.
* Look at QQ plots or PP plots of your data.
* Plot the data together with density estimates.
* Compute confidence intervals using parametric bootstrap.

This vignette shows you how to use the tools of `univariateML` to do exploratory
data analysis.

# Mortality in Ancient Egypt

The dataset `egypt` contains contains the age at death of 141 Roman era Egyptian
mummies. Our first task is to find a univariate model that fits this data.


``` r
library("univariateML")
head(egypt)
```

```
## # A tibble: 6 × 2
##     age sex  
##   <dbl> <chr>
## 1  1.5  male 
## 2  1.83 male 
## 3  2    male 
## 4  2    male 
## 5  3    male 
## 6  3    male
```

``` r
hist(egypt$age, main = "Mortality in Ancient Egypt", freq = FALSE)
```

![plot of chunk egypt](figure/egypt-1.png)

## Comparing Many Models with AIC
The [AIC](https://en.wikipedia.org/wiki/Akaike_information_criterion) is a handy 
and easy to use model selection tool, as it only depends on the log-likelihood and
number of parameters of the models. The \code{AIC} generic in `R` can take multiple
models, and the lower the \code{AIC} the better.

Since all the data is positive we will only try densities support on the positive
half-line. 
























