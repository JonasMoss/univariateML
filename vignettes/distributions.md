These are the currently implemented distributions.

<table style="width:100%;">
<colgroup>
<col style="width: 33%" />
<col style="width: 20%" />
<col style="width: 9%" />
<col style="width: 22%" />
<col style="width: 13%" />
</colgroup>
<thead>
<tr class="header">
<th>Name</th>
<th>univariateML function</th>
<th>Package</th>
<th>Parameters</th>
<th>Support</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>Cauchy distribution</td>
<td><code>mlcauchy</code></td>
<td>stats</td>
<td><code>location</code>,<code>scale</code></td>
<td><span class="math inline">ℝ</span></td>
</tr>
<tr class="even">
<td>Gumbel distribution</td>
<td><code>mlgumbel</code></td>
<td>extraDistr</td>
<td><code>mu</code>, <code>sigma</code></td>
<td><span class="math inline">ℝ</span></td>
</tr>
<tr class="odd">
<td>Laplace distribution</td>
<td><code>mllaplace</code></td>
<td>extraDistr</td>
<td><code>mu</code>, <code>sigma</code></td>
<td><span class="math inline">ℝ</span></td>
</tr>
<tr class="even">
<td>Logistic distribution</td>
<td><code>mllogis</code></td>
<td>stats</td>
<td><code>location</code>,<code>scale</code></td>
<td><span class="math inline">ℝ</span></td>
</tr>
<tr class="odd">
<td>Normal distribution</td>
<td><code>mlnorm</code></td>
<td>stats</td>
<td><code>mean</code>, <code>sd</code></td>
<td><span class="math inline">ℝ</span></td>
</tr>
<tr class="even">
<td>Student t distribution</td>
<td><code>mlstd</code></td>
<td>fGarch</td>
<td><code>mean</code>, <code>sd</code>, <code>nu</code></td>
<td><span class="math inline">ℝ</span></td>
</tr>
<tr class="odd">
<td>Generalized Error distribution</td>
<td><code>mlged</code></td>
<td>fGarch</td>
<td><code>mean</code>, <code>sd</code>, <code>nu</code></td>
<td><span class="math inline">ℝ</span></td>
</tr>
<tr class="even">
<td>Skew Normal distribution</td>
<td><code>mlsnorm</code></td>
<td>fGarch</td>
<td><code>mean</code>, <code>sd</code>, <code>xi</code></td>
<td><span class="math inline">ℝ</span></td>
</tr>
<tr class="odd">
<td>Skew Student t distribution</td>
<td><code>mlsstd</code></td>
<td>fGarch</td>
<td><code>mean</code>, <code>sd</code>, <code>nu</code>, <code>xi</code></td>
<td><span class="math inline">ℝ</span></td>
</tr>
<tr class="even">
<td>Skew Generalized Error distribution</td>
<td><code>mlsged</code></td>
<td>fGarch</td>
<td><code>mean</code>, <code>sd</code>, <code>nu</code>, <code>xi</code></td>
<td><span class="math inline">ℝ</span></td>
</tr>
<tr class="odd">
<td>Beta prime distribution</td>
<td><code>mlbetapr</code></td>
<td>extraDistr</td>
<td><code>shape1</code>, <code>shape2</code></td>
<td><span class="math inline">(0, ∞)</span></td>
</tr>
<tr class="even">
<td>Exponential distribution</td>
<td><code>mlexp</code></td>
<td>stats</td>
<td><code>rate</code></td>
<td><span class="math inline">[0, ∞)</span></td>
</tr>
<tr class="odd">
<td>Gamma distribution</td>
<td><code>mlgamma</code></td>
<td>stats</td>
<td><code>shape</code>,<code>rate</code></td>
<td><span class="math inline">(0, ∞)</span></td>
</tr>
<tr class="even">
<td>Inverse gamma distribution</td>
<td><code>mlinvgamma</code></td>
<td>extraDistr</td>
<td><code>alpha</code>, <code>beta</code></td>
<td><span class="math inline">(0, ∞)</span></td>
</tr>
<tr class="odd">
<td>Inverse Gaussian distribution</td>
<td><code>mlinvgauss</code></td>
<td>actuar</td>
<td><code>mean</code>, <code>shape</code></td>
<td><span class="math inline">(0, ∞)</span></td>
</tr>
<tr class="even">
<td>Inverse Weibull distribution</td>
<td><code>mlinvweibull</code></td>
<td>actuar</td>
<td><code>shape</code>, <code>rate</code></td>
<td><span class="math inline">(0, ∞)</span></td>
</tr>
<tr class="odd">
<td>Log-logistic distribution</td>
<td><code>mlllogis</code></td>
<td>actuar</td>
<td><code>shape</code>, <code>rate</code></td>
<td><span class="math inline">(0, ∞)</span></td>
</tr>
<tr class="even">
<td>Log-normal distribution</td>
<td><code>mllnorm</code></td>
<td>stats</td>
<td><code>meanlog</code>, <code>sdlog</code></td>
<td><span class="math inline">(0, ∞)</span></td>
</tr>
<tr class="odd">
<td>Lomax distribution</td>
<td><code>mllomax</code></td>
<td>extraDistr</td>
<td><code>lambda</code>, <code>kappa</code></td>
<td><span class="math inline">[0, ∞)</span></td>
</tr>
<tr class="even">
<td>Rayleigh distribution</td>
<td><code>mlrayleigh</code></td>
<td>extraDistr</td>
<td><code>sigma</code></td>
<td><span class="math inline">[0, ∞)</span></td>
</tr>
<tr class="odd">
<td>Weibull distribution</td>
<td><code>mlweibull</code></td>
<td>stats</td>
<td><code>shape</code>,<code>scale</code></td>
<td><span class="math inline">(0, ∞)</span></td>
</tr>
<tr class="even">
<td>Log-gamma distribution</td>
<td><code>mllgamma</code></td>
<td>actuar</td>
<td><code>shapelog</code>, <code>ratelog</code></td>
<td><span class="math inline">(1, ∞)</span></td>
</tr>
<tr class="odd">
<td>Pareto distribution</td>
<td><code>mlpareto</code></td>
<td>extraDistr</td>
<td><code>a</code>, <code>b</code></td>
<td><span class="math inline">[<em>b</em>, ∞)</span></td>
</tr>
<tr class="even">
<td>Beta distribution</td>
<td><code>mlbeta</code></td>
<td>stats</td>
<td><code>shape1</code>,<code>shape2</code></td>
<td><span class="math inline">(0, 1)</span></td>
</tr>
<tr class="odd">
<td>Kumaraswamy distribution</td>
<td><code>mlkumar</code></td>
<td>extraDistr</td>
<td><code>a</code>, <code>b</code></td>
<td><span class="math inline">(0, 1)</span></td>
</tr>
<tr class="even">
<td>Logit-normal</td>
<td><code>mllogitnorm</code></td>
<td>logitnorm</td>
<td><code>mu</code>, <code>sigma</code></td>
<td><span class="math inline">(0, 1)</span></td>
</tr>
<tr class="odd">
<td>Uniform distribution</td>
<td><code>mlunif</code></td>
<td>stats</td>
<td><code>min</code>, <code>max</code></td>
<td><span class="math inline">[min , max ]</span></td>
</tr>
<tr class="even">
<td>Power distribution</td>
<td><code>mlpower</code></td>
<td>extraDistr</td>
<td><code>alpha</code>, <code>beta</code></td>
<td><span class="math inline">[0, <em>a</em>)</span></td>
</tr>
</tbody>
</table>

This package follows a naming convention for the `ml***` functions. To
access the documentation of the distribution associated with an `ml***`
function, write `package::d***`. For instance, to find the documentation
for the log-gamma distribution write

    ?actuar::dlgamma

Problematic Distributions
-------------------------

### Lomax Distribution

The maximum likelihood estimator of the [Lomax
distribution](https://en.wikipedia.org/wiki/Lomax_distribution)
frequently fails to exist. For assume
$\\kappa\\to\\lambda^{-1}\\overline{x}^{-1}$ and *λ* → 0. The density
*λ**κ*(1+*λ**x*)<sup> − (*κ*+1)</sup> is approximately equal to
$\\lambda\\kappa\\left(1+\\lambda x\\right)^{-\\left(\\lambda^{-1}\\overline{x}^{-1}+1\\right)}$
when *λ* is small enough. Since
$\\lambda\\kappa\\left(1+\\lambda x\\right)^{-\\left(\\lambda^{-1}\\overline{x}^{-1}+1\\right)}\\to\\overline{x}^{-1}e^{-\\overline{x}^{-1}x}$,
the density converges to an exponential density.

    eps = 0.1
    x = seq(0, 3, length.out = 100)
    plot(dexp, 0, 3, xlab = "x", ylab = "Density", main = "Exponential and Lomax")
    lines(x, extraDistr::dlomax(x, lambda = eps, kappa = 1/eps), col = "red")

![](/data/dev/r/univariateML/vignettes/distributions_files/figure-markdown_strict/lomax-1.png)
