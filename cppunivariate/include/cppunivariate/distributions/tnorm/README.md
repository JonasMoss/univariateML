---
id: tnorm
name: Truncated Normal
support: {type: real, lower: -inf, upper: inf, closed: [false, false]}
params:
  - {name: mean, role: location, domain: "(-inf, inf)", default: 0, fixable: true}
  - {name: sd, role: scale, domain: "(0, inf)", default: 1, fixable: true}
  - {name: a, role: lower, domain: "[-inf, inf)", default: -inf, fixed: -inf}
  - {name: b, role: upper, domain: "(-inf, inf]", default: inf, fixed: inf}
r:
  density: extraDistr::dtnorm
  sampler: extraDistr::rtnorm
  control:
    a: Known lower truncation bound, held fixed; omitted or `NULL` means `-Inf`. Every observation must exceed it.
    b: Known upper truncation bound, held fixed; omitted or `NULL` means `Inf`. Every observation must be below it.
    reltol: Relative tolerance on the Newton step in the natural parameters `mean/sd^2` and `-1/(2*sd^2)` of standardized data.
    iterlim: Maximum number of Newton iterations.
log_density: -log(sd) - log(2*pi)/2 - (x-mean)**2/(2*sd**2) - log((erf((b-mean)/(sd*sqrt(2))) - erf((a-mean)/(sd*sqrt(2))))/2)
limits:
  - model: exp
    when: '$\mathrm{sd} \to \infty$ and $\mathrm{mean}/\mathrm{sd}^2 \to -1/(\bar x - a)$, with $a$ finite and $b = \infty$: the law of $a + Y$, $Y$ exponential'
    detect: 'is.infinite(b) && mean((x - mean(x))^2) >= (mean(x) - a)^2'
    status: ExponentialBoundary
  - model: exp
    when: '$\mathrm{sd} \to \infty$ and $\mathrm{mean}/\mathrm{sd}^2 \to 1/(b - \bar x)$, with $a = -\infty$ and $b$ finite: the law of $b - Y$, $Y$ exponential'
    detect: 'is.infinite(a) && mean((x - mean(x))^2) >= (b - mean(x))^2'
    status: ReflectedExponentialBoundary
  - name: truncated exponential
    when: '$\mathrm{sd} \to \infty$ and $\mathrm{mean}/\mathrm{sd}^2 \to r$, with $a$ and $b$ finite: the law on $(a, b)$ with density proportional to $e^{r x}$, uniform when $r = 0$'
    status: TruncatedExponentialBoundary
  - name: uniform
    fixed: [mean]
    when: '$\mathrm{sd} \to \infty$, with $a$ and $b$ finite: the uniform law on $(a, b)$'
    detect: 'mean((x - mean)^2) >= ((b - mean)^3 - (a - mean)^3) / (3 * (b - a))'
    status: UniformBoundary
references:
  - "Johnson, N. L., Kotz, S. and Balakrishnan, N. (1994). Continuous Univariate Distributions, Volume 1, second edition, Chapter 13.10. Wiley."
  - "Barndorff-Nielsen, O. (1978). Information and Exponential Families in Statistical Theory. Wiley."
examples: |
  set.seed(1)
  mltnorm(extraDistr::rtnorm(100, mean = 0.3, sd = 0.25, a = 0, b = 1), a = 0, b = 1)
  # One-sided truncation at zero.
  mltnorm(extraDistr::rtnorm(100, mean = 1, sd = 2, a = 0), a = 0, b = Inf)
  # Hold the parent standard deviation.
  mltnorm(extraDistr::rtnorm(100, mean = 1, sd = 2, a = 0), a = 0, b = Inf, sd = 2)
---

The truncated normal distribution is a normal distribution restricted to a known interval `(a, b)`; it models measurements that are only recorded, or only possible, inside that interval. `mean` and `sd` are the parameters of the parent normal distribution, not the mean and standard deviation of the truncated one.

## Details

The bounds `a` and `b` are known design quantities, held fixed and never estimated; they default to `-Inf` and `Inf`, giving the normal fit of `mlnorm`. `extraDistr::dtnorm` is zero at the bounds, so every observation must lie strictly between them.

For fixed bounds the family is an exponential family in $\mathrm{mean}/\mathrm{sd}^2$ and $-1/(2\,\mathrm{sd}^2)$ whose log-likelihood is strictly concave; the maximum depends on the data only through `mean(x)` and the population variance. It exists unless the sample is at least as dispersed as the member with $1/\mathrm{sd}^2 = 0$ and the same mean, an exponential or truncated exponential law that the parametrisation by `sd` cannot reach. This happens with positive probability for samples from a truncated normal, with small samples especially susceptible near an exponential limit; faces win ties within `1e-8` times the sample size. With `sd` held fixed the estimate of `mean` always exists. With `mean` held fixed and both bounds finite the limit is the uniform distribution on `(a, b)`; `mlunif` is a different model, which estimates its endpoints.

`extraDistr::dtnorm`, `ptnorm` and `qtnorm` subtract normal probabilities and fail when the interval lies far in a tail of the parent normal, which fits close to an escape can reach (see `KNOWN_ISSUES.md`); `logLik` is computed independently in C++.

For nonconstant data, let $m$ and $v$ be the empirical mean and variance
with divisor $n$. On finite bounds, the full natural parameter space is
all of $\mathbb R^2$, including positive quadratic coefficients (convex
exponents); the normal subfamily requires $\theta_2<0$. The interior
moment set is $a<m<b$ and $0<v<(m-a)(b-m)$. Within it, fit the unique
exponential tilt $e^{r x}/\int_a^b e^{r u}du$ with mean $m$, and denote
its variance by $v_r$. A finite normal MLE exists exactly when
$0<v<v_r$; $v\ge v_r$ gives the boundary supremum, rather than an
attained normal fit. For $r=0$, $m=(a+b)/2$ and $v_r=(b-a)^2/12$.
Equivalently, for $r\ne0$, write $w=b-a$:
$m=a+w/(1-e^{-rw})-1/r$ and
$v_r=1/r^2-w^2 e^{-rw}/(1-e^{-rw})^2$.

On $(a,\infty)$, the natural parameter space consists of
$\theta_2<0$ with arbitrary $\theta_1$, together with
$\theta_2=0,\theta_1<0$; no positive quadratic coefficient is
integrable. The moment interior is $m>a,v>0$, and the normal MLE
exists exactly when $v<(m-a)^2$. Reflect this condition to
$v<(b-m)^2$ on $(-\infty,b)$. On the whole line only
$\theta_2<0$ is integrable and every nonconstant sample has a normal
MLE. Constant samples with both parent parameters free have unbounded
likelihood as the scale collapses. With a held mean, the sufficient
statistic is the empirical squared distance from that mean; on a finite
interval its upper boundary test compares it with the uniform's squared
distance. On an infinite interval that expectation diverges and there is
no uniform escape. These conditions follow by profiling the strictly
concave natural likelihood and testing its derivative at zero quadratic
coefficient. Numerical face ties use the tolerance stated above.

## See also

`mlhnorm` for the half-normal with an estimated endpoint and `mlnorm` for the untruncated fit.
