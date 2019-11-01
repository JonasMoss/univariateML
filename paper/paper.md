kdensity: An R package for kernel density estimation with parametric
starts and asymmetric kernels
================

# Summary

It is often necessary to estimate a probability density
non-parametrically, that is, without making strong parametric
assumptions such as normality. This R (R Core Team 2019) package
provides a non-parametric density estimator that can take advantage of
some of the knowledge the user has about the probability density.

Kernel density estimation (Silverman 2018) is a popular method for
non-parametric density estimation based on placing kernels on each data
point. Hjort and Glad (1995) extended kernel density estimation with
*parametric starts*. The parametric start is a parametric density that
is multiplied with the kernel estimate. When the data-generating density
is reasonably close to the parametric start density, kernel density
estimation with that parametric start will outperform ordinary kernel
density estimation.

Moreover, when estimating densities on the half-open interval
\(\left[0,\infty\right)\) and bounded intervals, such as
\(\left[0, 1\right]\), symmetric kernels are prone to serious boundary
bias that should be corrected (Marron and Ruppert 1994). Asymmetric
kernels have been designed to avoid boundary bias and many of them are
implemented in `kdensity` in addition to the classical symmetric
kernels. For the unit interval, the Gaussian copula kernel of Jones and
Henderson (2007) and the beta kernels of Chen (1999) are supported. The
gamma kernel of Chen (2000) is available for the half-open interval.

The supported non-parametric starts include the normal, Laplace, Gumbel,
exponential, gamma, log-normal, inverse Gaussian, Weibull, Beta, and
Kumaraswamy densities. The parameters of all parametric starts are
estimated using maximum likelihood. The implemented bandwidth selectors
are the classical bandwidth selectors from `stats`, unbiased
cross-validation, the Hermite polynomial method from Hjort and Glad
(1995), and the tailored bandwidth selector for the Gaussian copula
method of Jones and Henderson (2007). User-defined parametric starts,
kernels and bandwidth selectors can also be set.

Several `R` packages deal with kernel estimation, see Deng and Wickham
(2011) for an overview. While no other `R` package handles density
estimation with parametric starts, several packages supports methods
that handle boundary bias. Hu and Scarrott (2018) provides a variety of
boundary bias correction methods in the `bckden` functions. Nagler and
Vatter (2019) corrects for boundary bias using probit or logarithmically
transformed local polynomial kernel density estimation. Jones, Nguyen,
and McLachlan (2018) corrects for boundary bias on the half line using a
logarithmic transform. Duong (2019) supports boundary correction through
the `kde.boundary` function, while Wansouwé, Somé, and Kokonendji (2015)
corrects for boundary bias using asymmetric kernels.

The following example uses the `airquality` data set from the built-in R
package `datasets`. Since the data is positive we use Chen’s gamma
kernel. As the data is likely to be better approximated by a gamma
distribution than a uniform distribution, we use the gamma parametric
start. The plotted density is in figure 1, where the gamma distribution
with parameters estimated by maximum likelihood is in red and the
ordinary kernel density estimate in blue. Notice the boundary bias of
the ordinary kernel density estimator.

``` r
# install.packages("univariateML")
library("univariateML")
hist(egypt$age, freq = FALSE, main = "Mortality in Egypt", xlab = "Mortality")
lines(mlweibull(egypt$age)) # Plots a Weibull fit.
lines(mlgamma(egypt$age), col = "red")  # Plots a Gamma fit.
```

![](paper_files/figure-gfm/figure-1.png)<!-- -->

# References

<div id="refs" class="references">

<div id="ref-chen1999beta">

Chen, Song Xi. 1999. “Beta Kernel Estimators for Density Functions.”
*Computational Statistics & Data Analysis* 31 (2): 131–45.
<https://doi.org/10.1016/S0167-9473(99)00010-9>.

</div>

<div id="ref-chen2000probability">

———. 2000. “Probability Density Function Estimation Using Gamma
Kernels.” *Annals of the Institute of Statistical Mathematics* 52 (3):
471–80. <https://doi.org/10.1023/A:1004165218295>.

</div>

<div id="ref-deng2011">

Deng, Henry, and Hadley Wickham. 2011. “Density Estimation in R.”
<http://vita.had.co.nz/papers/density-estimation.pdf>.

</div>

<div id="ref-ks">

Duong, Tarn. 2019. *Ks: Kernel Smoothing*.
<https://CRAN.R-project.org/package=ks>.

</div>

<div id="ref-hjort_glad_1995">

Hjort, Nils Lid, and Ingrid K Glad. 1995. “Nonparametric Density
Estimation with a Parametric Start.” *The Annals of Statistics*,
882–904. <https://doi.org/10.1214/aos/1176324627>.

</div>

<div id="ref-evmix">

Hu, Yang, and Carl Scarrott. 2018. “evmix: An R Package for Extreme
Value Mixture Modeling, Threshold Estimation and Boundary Corrected
Kernel Density Estimation.” *Journal of Statistical Software* 84 (5):
1–27. <https://doi.org/10.18637/jss.v084.i05>.

</div>

<div id="ref-logKDE">

Jones, Andrew T, Hien Duy Nguyen, and Geoffrey J McLachlan. 2018.
“LogKDE: Log-Transformed Kernel Density Estimation.” *Journal of Open
Source Software* 3 (28): 870. <https://doi.org/10.21105/joss.00870>.

</div>

<div id="ref-jones2007miscellanea">

Jones, MC, and DA Henderson. 2007. “Kernel-Type Density Estimation on
the Unit Interval.” *Biometrika* 94 (4): 977–84.
<https://doi.org/10.1093/biomet/asm068>.

</div>

<div id="ref-marron1994transformations">

Marron, James Stephen, and David Ruppert. 1994. “Transformations to
Reduce Boundary Bias in Kernel Density Estimation.” *Journal of the
Royal Statistical Society: Series B (Methodological)* 56 (4): 653–71.
<https://doi.org/10.1111/j.2517-6161.1994.tb02006.x>.

</div>

<div id="ref-kde1d">

Nagler, Thomas, and Thibault Vatter. 2019. *Kde1d: Univariate Kernel
Density Estimation*. <https://CRAN.R-project.org/package=kde1d>.

</div>

<div id="ref-r">

R Core Team. 2019. *R: A Language and Environment for Statistical
Computing*. Vienna, Austria: R Foundation for Statistical Computing.
<https://www.R-project.org/>.

</div>

<div id="ref-silverman2018density">

Silverman, Bernard W. 2018. *Density Estimation for Statistics and Data
Analysis*. Routledge. <https://doi.org/10.1201/9781315140919>.

</div>

<div id="ref-Ake">

Wansouwé, W. E., S. M. Somé, and C. C. Kokonendji. 2015. *Ake:
Associated Kernel Estimations*.
<https://CRAN.R-project.org/package=Ake>.

</div>

</div>
