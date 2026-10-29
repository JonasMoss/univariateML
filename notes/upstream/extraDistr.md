# Upstream defect reports for extraDistr 1.10.0.5

## Issue 1: Zero-inflated NB and binomial density and CDF underflow at extreme parameters

**Package version:** extraDistr 1.10.0.5

**Summary:** The zero-inflated negative binomial (`dzinb`, `pzinb`) and zero-inflated binomial (`dzib`, `pzib`) functions underflow to `-Inf` in their logarithmic outputs at large counts and shape parameters, and return `-Inf` for upper-tail log CDFs.

**Reproducers:**

```r
# dzinb log underflow
extraDistr::dzinb(10000, size = 2, mu = 0.5, pi = 0.1, log = TRUE)
# ACTUAL: -Inf
# EXPECTED: -6923.75 (or computed from stable log-ratio recurrence)

# pzinb upper-tail log CDF underflow
extraDistr::pzinb(100, size = 2, mu = 0.5, pi = 0.1, 
                  lower.tail = FALSE, log.p = TRUE)
# ACTUAL: -Inf
# EXPECTED: about -66.17

# dzib log underflow  
extraDistr::dzib(9999, size = 10000, alpha = 0.4, beta = 0.3, log = TRUE)
# ACTUAL: -Inf
# EXPECTED: finite (like dzinb)

# pzib upper-tail log CDF underflow
extraDistr::pzib(100, size = 10, alpha = 0.4, beta = 0.3, 
                 lower.tail = FALSE, log.p = TRUE)
# ACTUAL: -Inf
# EXPECTED: finite
```

**Expected values:** Computed via:
- Mathematical identity: log-density is the log of a sum of geometric or binomial mass and zero point mass.
- For dzinb at (x=10000, size=2, mu=0.5, pi=0.1): the nonzero component is `(1-pi) * dnbinom(x, size, mu)` in log space, which is large and negative but finite.
- For pzinb upper-tail log CDF: the survival probability near observation counts is nonzero and should not round to zero in log space.

**Cause:** 
The density functions likely compute the zero-inflated probability as a mixture of a point mass at zero and a base distribution. Computing `log(zero_probability + nonzero_probability)` can underflow if intermediate probabilities are exponentiated and then underflow before re-logging. A stable implementation uses log-space arithmetic throughout:
```
log_density = log(pi * indicator(x==0) + (1-pi) * exp(log_base_density))
```

For upper-tail log CDFs, the same issue occurs: the code likely computes `log(1 - CDF)` when the CDF is very close to 1, which underflows. The fix is to use `expm1` or equivalent stable complement computation in log space.

**Suggested fix:** 
Use stable log-mixture and log-complement arithmetic:
1. For density: implement `log1p(pi/(1-pi) * exp(log_pointmass - log_base))` when the base density is large, or use an explicit case for tiny pi.
2. For upper-tail CDF on log scale: use the relationship `log(1-p) = log1p(-p)` and compute via stable means, or transpose the CDF computation to avoid complementing very small probabilities.

---

## Issue 2: Tail extremes in quantile and density functions return non-finite values

**Package version:** extraDistr 1.10.0.5

**Summary:** Multiple families lose accuracy in their tails or at extreme parameters:
- Quantile functions at extreme probabilities round to `Inf` when finite values are expected.
- Density and CDF functions return `-Inf` in tails due to overflow of intermediate terms or use of log-underflowed values.
- Truncated-normal, truncated-Poisson, and similar functions have accuracy loss far in a tail.

**Reproducers:**

```r
# qgpd shape near zero: subtractive cancellation in q(p) = 1 - (1-p)^(-xi)
extraDistr::qgpd(0.5, xi = 1e-16)
# ACTUAL: 0
# EXPECTED: log(2) ≈ 0.6931471806

# qgev quantile 1 at negative shape should give finite upper endpoint
extraDistr::qgev(1, xi = -0.5)
# ACTUAL: Inf
# EXPECTED: 2 (the upper endpoint of the support)

# pgev upper-tail log CDF
extraDistr::pgev(1000, lower.tail = FALSE, log.p = TRUE)
# ACTUAL: -Inf
# EXPECTED: -1000 (survives very extreme values)

# dtnorm far in tail: a = 10, point at 10.05
extraDistr::dtnorm(10.05, a = 10, log = TRUE)
# ACTUAL: Inf
# EXPECTED: about 1.811

# qtnorm far in tail
extraDistr::qtnorm(0.5, a = 40, b = 41)
# ACTUAL: Inf
# EXPECTED: finite (between 40 and 41)

# dtpois far in tail
extraDistr::dtpois(101, lambda = 2, a = 100, b = 110, log = TRUE)
# ACTUAL: Inf
# EXPECTED: about -0.0198

# qfrechet extreme upper probability
extraDistr::qfrechet(-40, alpha = 2, lower.tail = FALSE, log.p = TRUE)
# ACTUAL: Inf
# EXPECTED: about 4.85e8

# Half-Cauchy overflow
extraDistr::dhcauchy(1e200, log = TRUE)
# ACTUAL: -Inf
# EXPECTED: about -921.486

# Truncated normal far in tail: upper interval far above mode
extraDistr::qtnorm(0.5, a = 40, b = 41)
# ACTUAL: Inf
# EXPECTED: finite between 40 and 41

# Discrete Weibull log tail
extraDistr::ddweibull(1000, q = 0.999, beta = 2, log = TRUE)
# ACTUAL: -Inf
# EXPECTED: about -1000.645

# Discrete normal log tail
extraDistr::ddnorm(-40, log = TRUE)
# ACTUAL: -Inf
# EXPECTED: finite

# Discrete gamma log tail
extraDistr::ddgamma(10, log = TRUE)
# ACTUAL: NA
# EXPECTED: finite
```

**Causes vary by family:**
1. **Shape near zero (GPD, GEV):** Subtractive `pow(...) - 1` loses digits when the exponent is tiny.
2. **Truncated distributions far in tails:** The code likely computes the density or CDF as a ratio of parent-distribution values. At the edge or far from the support, this can produce `Inf / Inf` or `0 / 0` indeterminacy that is not handled.
3. **Upper-tail log CDF:** Code implements `log(1 - CDF_lower)` which underflows when the lower CDF is very close to 1. Should use stable `log1p` or equivalent.
4. **Density in far tails:** Subtractive terms (e.g., `exp(a) - exp(b)` when both underflow) or taking logs of underflowed values.

**Suggested fixes:**
1. For shape near zero: use series expansions `(1 + x)^a ≈ 1 + a*x` for tiny `a`.
2. For truncated distributions: reparametrize via CDF differences in log space; avoid forming density as a ratio unless both are numerically stable.
3. For upper-tail log CDF: compute as `-expm1(log(CDF_lower))` or equivalent stable log-complement.
4. For far-tail densities: recognize underflowed intermediate forms and either use series/asymptotic expansions or return a stable approximation.

---

## Issue 3: Generalized Pareto quantile accuracy near zero shape

**Package version:** extraDistr 1.10.0.5

**Reproducer:**

```r
extraDistr::qgpd(0.5, xi = 1e-16)
# ACTUAL: 0
# EXPECTED: log(2) ≈ 0.6931471806
```

**Expected:** The GPD quantile is $q(p) = \sigma \left( (1-p)^{-\xi} - 1 \right) / \xi$ for $\xi \ne 0$. As $\xi \to 0$, this limit is $\sigma \log(1/(1-p)) = \sigma \log(1/0.5) = \sigma \log 2$. With $\sigma=1$, the result should be $\log 2$.

**Cause:** Subtractive cancellation: the code computes `((1-p)^(-xi) - 1) / xi` directly. When $\xi$ is tiny, both numerator and denominator are small, and the division loses precision. The correct approach is to use a series expansion $\frac{(1-p)^{-\xi} - 1}{\xi} \approx -\log(1-p) + \xi \log^2(1-p) / 2 + \ldots$ for small $\xi$.

**Suggested fix:** Detect tiny $\xi$ (e.g., `|xi| < 1e-10`) and use the series expansion, or use `expm1` or similar for the numerator.

---

## Issue 4: Generalized Pareto CDF underflow and upper-tail log CDF

**Package version:** extraDistr 1.10.0.5

**Reproducers:**

```r
# pgpd underflow at tiny x
extraDistr::pgpd(1e-20)
# ACTUAL: 0
# EXPECTED: about 1e-20

# pgpd upper-tail log CDF
extraDistr::pgpd(1000, xi = 0, lower.tail = FALSE, log.p = TRUE)
# ACTUAL: -Inf
# EXPECTED: -1000
```

**Cause:** 
- Underflow: computing `1 - exp(-x / sigma)` when `x / sigma` is huge loses the exponential in the subtraction.
- Upper-tail log: applying `log(1 - p)` when `p` is close to 1 underflows. Should use `log1p(-p)` or equivalent.

**Suggested fix:** 
Use stable subtraction and log-complement arithmetic.

---

## Issue 5: Triangular CDF returns NA at lower endpoint when mode is there

**Package version:** extraDistr 1.10.0.5

**Reproducer:**

```r
extraDistr::ptriang(0, a = 0, b = 1, c = 0)
# ACTUAL: NA
# EXPECTED: 0
```

**Expected:** At the lower endpoint of the support with mode at that endpoint, the CDF should be 0. This is the degenerate case.

**Cause:** Likely a division-by-zero or `NaN` in the formula when the mode equals the lower bound.

**Suggested fix:** Add a guard for the case `c == a` in the CDF computation.

---

## Issue 6: Discrete Laplace CDF branch selection and negative probabilities

**Package version:** extraDistr 1.10.0.5

**Reproducer:**

```r
extraDistr::pdlaplace(3, location = 5, prob = 0.5)
# ACTUAL: -0.3333333
# EXPECTED: 1/6 ≈ 0.16667
```

**Expected:** The CDF should always lie in [0, 1].

**Cause:** The code picks its branch based on the raw observation `q` instead of the centered observation `q - location`. This gives a negative probability.

**Suggested fix:** Use `q - location` to determine which branch to take.

---

## Issue 7: Discrete Weibull CDF does not floor its argument

**Package version:** extraDistr 1.10.0.5

**Reproducer:**

```r
extraDistr::pdweibull(0.5, q = 0.7, beta = 1)
# ACTUAL: 0.414
# EXPECTED: pdweibull(0, 0.7, 1) ≈ 0.3
```

**Expected:** The discrete Weibull is defined on non-negative integers. The CDF at a non-integer should equal the CDF at the integer floor of that argument.

**Cause:** The code does not floor its input before evaluation.

**Suggested fix:** Add `q <- floor(q)` at the start of the CDF function.

---

## Issue 8: Negative hypergeometric log density underflow and segfault

**Package version:** extraDistr 1.10.0.5

**Reproducers:**

```r
# Log density underflow at large count
extraDistr::dnhyper(1003, m = 1000, n = 1000, k = 3, log = TRUE)
# ACTUAL: -Inf
# EXPECTED: about -1369.14

# Segfault at zero urn (all parameters zero)
extraDistr::dnhyper(0, m = 0, n = 0, k = 0)
# ACTUAL: segmentation fault
# EXPECTED: handled error or defined result (probably undefined, but should not crash)
```

**Cause:** 
- Log underflow: Computes the hypergeometric mass as a ratio of large binomial coefficients. These underflow to zero before being log'd.
- Segfault: Likely an unchecked degenerate case (urn with zero items) causes a division by zero or invalid memory access.

**Suggested fix:**
- Use stable log-arithmetic throughout: compute log of factorials or use `lchoose` for the log binomial coefficients.
- Add guards for degenerate cases (m=0, n=0, k=0) and either error or return a defined value.

---

## Issue 9: Negative binomial and binomial beta-form log density and CDF accuracy loss

**Package version:** extraDistr 1.10.0.5

**Reproducers:**

```r
# Beta-binomial log accuracy at huge shape
extraDistr::dbnbinom(0, size = 2, alpha = 4e14, beta = 1e14, log = TRUE)
# ACTUAL: -0.46875
# EXPECTED: about -0.446 (like dnbinom(0, 2, 0.8, log=TRUE) = -0.446)

# Beta-binomial CDF exceeds 1 at huge shape
extraDistr::pbnbinom(0, size = 2, alpha = 4e14, beta = 1e14)
# ACTUAL: 2.805
# EXPECTED: <= 1

# Beta-binomial log accuracy
extraDistr::dbbinom(5, size = 10, alpha = 1e12, beta = 1e12, log = TRUE)
# ACTUAL: -1.401855
# EXPECTED: about -1.40204
```

**Expected:** At very large shape parameters, the beta-binomial and beta-negative-binomial approach their limiting negative binomial and binomial distributions. Near this limit, the densities should approach the limiting distribution's densities.

**Cause:** At huge shape parameters, the beta function becomes numerically unstable. Computing $\log \text{Beta}(a, b)$ via factorials or direct computation loses precision when both $a$ and $b$ are huge.

**Suggested fix:** Use `lbeta` from the math library, and ensure all intermediate computations use log-space arithmetic. For the limiting case, recognize when shape parameters are extreme and dispatch to the base distribution directly.

---

## Issue 10: Slash and Bhattacharjee densities have tail and center accuracy issues

**Package version:** extraDistr 1.10.0.5

**Reproducers:**

```r
# Slash CDF slope error near center
extraDistr::pslash(1e-10) - 0.5
# ACTUAL: 3.99e-11
# EXPECTED: about 1.99e-11 (off by factor of 2)

# Bhattacharjee log density subtraction
extraDistr::dbhatt(10, log = TRUE)
# ACTUAL: -Inf
# EXPECTED: about -44.3213
```

**Causes:**
- Slash: The CDF near zero may involve cancellation between similar terms.
- Bhattacharjee: The density is defined as a subtraction of two normal CDFs, scaled by their difference in support. At large values far from the mode, this becomes `(0 - 0) / small_difference`, giving `NaN` or `-Inf`.

**Suggested fix:**
- For Slash: use careful rearrangement to avoid subtractive cancellation near the mode.
- For Bhattacharjee: use the log-difference formula `log(a - b) = log(a) + log(1 - b/a)` when both `a` and `b` are small, or compute via a series expansion in the tail.

---

## Issue 11: Discrete normal and gamma log densities return -Inf

**Package version:** extraDistr 1.10.0.5

**Reproducers:**

```r
extraDistr::ddnorm(-40, log = TRUE)
# ACTUAL: -Inf
# EXPECTED: finite

extraDistr::ddgamma(10, log = TRUE)
# ACTUAL: NA (or -Inf)
# EXPECTED: finite
```

**Expected:** The discrete normal and discrete gamma densities are defined on the integers and should have finite log-densities at all points in their support.

**Cause:** These distributions are likely defined as differences of the parent CDF at consecutive integers. Computing `log(F(k) - F(k-1))` when both values underflow to zero gives `-Inf`.

**Suggested fix:** Use a stable log-difference formula or compute the density differently in the tails (e.g., via asymptotics or the probability mass formula if available).

---

## Issue 12: Half-Cauchy, Half-t, and Half-normal overflow and tail issues

**Package version:** extraDistr 1.10.0.5

**Reproducers:**

```r
# dhcauchy log overflow
extraDistr::dhcauchy(1e200, log = TRUE)
# ACTUAL: -Inf
# EXPECTED: about -921.486

# dht log 
extraDistr::dht(1e200, df = 3, log = TRUE)
# ACTUAL: -1840.18 (OK but potentially at edge of stability)
# EXPECTED: finite and stable

# phcauchy upper-tail log CDF
extraDistr::phcauchy(1e20, lower.tail = FALSE, log.p = TRUE)
# ACTUAL: -Inf
# EXPECTED: about -46.503

# pht upper-tail log CDF
extraDistr::pht(1e20, df = 3, lower.tail = FALSE, log.p = TRUE)
# ACTUAL: -Inf
# EXPECTED: negative (not -Inf)

# phnorm upper-tail log CDF
extraDistr::phnorm(1e20, lower.tail = FALSE, log.p = TRUE)
# ACTUAL: -Inf
# EXPECTED: negative (not -Inf)

# qhcauchy extreme upper probability
extraDistr::qhcauchy(-100, lower.tail = FALSE, log.p = TRUE)
# ACTUAL: 1.6e16
# EXPECTED: finite (not rounded away or to Inf)

# qht extreme upper probability
extraDistr::qht(-100, df = 3, lower.tail = FALSE, log.p = TRUE)
# ACTUAL: Inf
# EXPECTED: about 3.90e14

# qhnorm extreme upper probability
extraDistr::qhnorm(-100, lower.tail = FALSE, log.p = TRUE)
# ACTUAL: Inf
# EXPECTED: finite
```

**Causes:**
- Overflow in half-Cauchy: The square of a huge observation overflows before the log can be taken.
- Upper-tail log CDF: Code computes `log(1 - CDF)` directly, which underflows when CDF is close to 1.
- Quantile rounding: At extreme log-probabilities, the quantile computation rounds the inverse to `Inf` due to floating-point range limits.

**Suggested fixes:**
1. Use `log1p(-x^2)` or equivalent for the Cauchy component.
2. For upper-tail log CDF: compute via `log1p(-p)` or rewrite the CDF to directly give `log(S(x))` where S is the survival function.
3. For quantiles: carefully bound intermediate computations to stay within the representable range, and detect when the final answer would be huge, giving a large finite value or `Inf` appropriately.

---

## Non-reproducing or marginal defects

### Huber CDF overflow at extreme epsilon
- **Reproducer:** `extraDistr::phuber(-60, 0, 1, 40)` → `NA`
- **Status:** Returns `NA` instead of a value; unclear if this is the original issue or a symptom of a related problem.

### Skellam log density
- **Reproducer:** `extraDistr::dskellam(0, 1000, 1000, log = TRUE)` → `NA`
- **Status:** Returns `NA`; the KNOWN_ISSUES entry says it should be about -4.72, not NaN. This suggests the density computation itself has an issue.

### Zero-inflated binomial quantile at boundary
- **Reproducer:** `extraDistr::qzib(1, 10, 0.4, 1)` → `NA`
- **Expected:** 0 (the maximum support at `pi=1`)
- **Status:** Returns `NA` rather than the correct value.

### Zero-inflated NB quantile at boundary
- **Reproducer:** `extraDistr::qzinb(1, 2, 0.5, 1)` → `NA`
- **Expected:** 0 (all probability at zero when `pi=1`)
- **Status:** Returns `NA`.

