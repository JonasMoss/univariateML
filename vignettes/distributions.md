---
title: "Distributions"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Distributions}
  %\VignetteEngine{knitr::knitr}
  %\VignetteEncoding{UTF-8}
---



The implemented distributions are found in `univariateML_models`.


``` r
univariateML_models
```

```
##  [1] "beta"       "betapr"     "binom"      "burr"       "cauchy"    
##  [6] "dunif"      "exp"        "fatigue"    "gamma"      "ged"       
## [11] "geom"       "gompertz"   "gumbel"     "invburr"    "invgamma"  
## [16] "invgauss"   "invweibull" "kumar"      "laplace"    "lgamma"    
## [21] "lgser"      "llogis"     "lnorm"      "logis"      "logitnorm" 
## [26] "lomax"      "naka"       "nbinom"     "norm"       "paralogis" 
## [31] "pareto"     "pois"       "power"      "rayleigh"   "sged"      
## [36] "snorm"      "sstd"       "std"        "unif"       "weibull"   
## [41] "zip"        "zipf"
```

This package follows a naming convention for the `ml***` functions. To access the
documentation of the distribution associated with an `ml***` function, write `package::d***`. 
For instance, to find the documentation for the log-gamma distribution write


``` r
?actuar::dlgamma
```


Additional information about the models can found in `univariateML_metadata`.

``` r
univariateML_metadata[["mllgser"]]
```

```
## $model
## [1] "Logarithmic series"
## 
## $density
## [1] "extraDistr::dlgser"
## 
## $support
## Object of class Intervals
## 1 interval over Z:
## [1, Inf)
## 
## $names
## [1] "theta"
## 
## $default
## [1] 0.9
```

From the metadata you can read that 

* `mllgser` estimates the parameters `N` and `s`.
* Its a discrete distribution on $1,2,3,...$,
* Its density function is `extraDistr::dlgser`.

## Problematic Distributions
Some estimation procedures will fail under certain circumstances. Sometimes due to numerical problems,
but also because the maximum likelihood estimator fails to exist. Here is a possibly non-exhaustive list of known problematic distributions.

### Discrete distributions
* **Binomial**. The maximum likelihood estimator does not exist for underdispersed data (when $size$ is estimated). There is an increasing sequence of estimates $size$, $p$ so that the binomial likelihood converges to a Poisson, however.
* **Negative binomial.** The same sort of problem occurs with the negative binomial, which converges to a Poisson for some data sets.
* **Lomax.** Here we have convergence to an exponential for certain data sets.
* **Zipf.** The optimal shape parameter may be negative, which still defines a density, but is not supported by `extraDistr`.
* **Logarithic series distribution.** When all observations are $1$ the estimator does not exist, as the "actual" maximum likelihood estimator is the point mass on $0$.

### Continuous distributions
* **Gompertz.** Here we have a similar problem, with some parameters outside the range of the distribution converging to a density function with a different support. When the `b` parameter tends towards 0, the Gompertz tends towards an exponential. A failing estimation indicates the exponential has a better fit.
* **Lomax.** Here we have convergence to an exponential for certain data sets.
* **Burr**. The Burr distribution tends to the Pareto distribution when `shape1*shape2` converges to a constant while `shape2` tends to infinity.  
