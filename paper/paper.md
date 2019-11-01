univariateML: An R package for maximum likelihood estimation of
univariate densities
================

# Summary

`univariateML` is an R (R Core Team 2019) package for doing univariate
maximum likelihood estimation (Cam 1990). It supports more than 20
densities, the most popular generics such as `plot`, `AIC`, and
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

Another natural question is to ask which among several model fits the
data best. This can be done using tools of model selection such as the
`AIC` (Akaike 1998).

``` r
AIC(mlweibull(egypt$age),
    mlgamma(egypt$age))
```

    ##                      df      AIC
    ## mlweibull(egypt$age)  2 1230.229
    ## mlgamma(egypt$age)    2 1234.772

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

<div id="refs" class="references">

<div id="ref-akaike1998information">

Akaike, Hirotogu. 1998. “Information Theory and an Extension of the
Maximum Likelihood Principle.” In *Selected Papers of Hirotugu Akaike*,
199–213. Springer.

</div>

<div id="ref-lecam1990ml">

Cam, Lucien Le. 1990. “Maximum Likelihood: An Introduction.”
*International Statistical Review / Revue Internationale de Statistique*
58 (2): 153–71. <https://doi.org/10.2307/1403464>.

</div>

<div id="ref-efron1994introduction">

Efron, Bradley, and Robert J Tibshirani. 1994. *An Introduction to the
Bootstrap*. CRC press.

</div>

<div id="ref-hjort_glad_1995">

Hjort, Nils Lid, and Ingrid K Glad. 1995. “Nonparametric Density
Estimation with a Parametric Start.” *The Annals of Statistics*,
882–904. <https://doi.org/10.1214/aos/1176324627>.

</div>

<div id="ref-johnson1970continuous">

Johnson, Norman Lloyd, Samuel Kotz, and Narayanaswamy Balakrishnan.
1995. *Continuous Univariate Distributions*. 2nd ed. Vol. 1. Wiley.

</div>

<div id="ref-ko2019focused">

Ko, Vinnie, Nils Lid Hjort, and Ingrid Hobæk Haff. 2019. “Focused
Information Criteria for Copulas.” *Scandinavian Journal of Statistics*.
<https://doi.org/10.1111/sjos.12387>.

</div>

<div id="ref-moss2019kdensity">

Moss, Jonas, and Martin Tveten. 2019. “Kdensity: An R Package for Kernel
Density Estimation with Parametric Starts and Asymmetric Kernels.” *The
Journal of Open Source Software* 4.
<https://doi.org/10.21105/joss.01566>.

</div>

<div id="ref-pearson1902egypt">

Pearson, Karl. 1902. “On the Change in Expectation of Life in Man During
a Period of Circa 2000 Years.” *Biometrika* 2: 261–64.
<https://doi.org/10.2307/2331493>.

</div>

<div id="ref-r">

R Core Team. 2019. *R: A Language and Environment for Statistical
Computing*. Vienna, Austria: R Foundation for Statistical Computing.
<https://www.R-project.org/>.

</div>

</div>
