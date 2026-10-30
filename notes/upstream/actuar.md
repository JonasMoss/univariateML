# Upstream defect reports for actuar 3.3.7

## Issue 1: Zero-truncated/modified distribution densities and CDFs underflow to -Inf

**Package version:** actuar 3.3.7

**Summary:** The zero-truncated and zero-modified families (`dztnbinom`, `pztpois`, `qztbinom`, `qzmbinom`, `pzmlogarithmic`) lose accuracy at extreme counts and parameters, returning `-Inf` for log-densities and upper-tail log CDFs when finite values are expected.

**Reproducers:**

```r
# dztnbinom log underflow at large count
actuar::dztnbinom(10000, size = 2, prob = 0.4, log = TRUE)
# ACTUAL: -Inf
# EXPECTED: finite (log of the zero-truncated mass)

# pztpois upper-tail log CDF
actuar::pztpois(10000, lambda = 2, lower.tail = FALSE, log.p = TRUE)
# ACTUAL: -Inf
# EXPECTED: negative (survivorship in the far tail)

# pzmlogarithmic upper-tail log CDF cancellation
actuar::pzmlogarithmic(10000, prob = 0.4, pzt = 0.3, 
                       lower.tail = FALSE, log.p = TRUE)
# ACTUAL: -36.4
# EXPECTED: below -9000 (extreme tail probability)

# qztbinom at quantile 1
actuar::qztbinom(1, size = 10, prob = 0.4)
# ACTUAL: Inf
# EXPECTED: 10 (the upper support limit)
```

**Expected values:** 
- The zero-truncated distributions are well-defined on positive integers; their densities should be finite. The log-density is `log(d / (1 - d(0)))` where d is the base distribution's mass.
- Upper-tail CDFs should reflect the survival probability; even in the far tail, this is a well-defined positive (or zero) value, not `-Inf`.
- Quantile at p=1 should return the supremum of the support.

**Cause:** 
- Log underflow: The code likely exponentiates intermediate probabilities, which underflow to zero before re-logging. Stable log-arithmetic is needed.
- Upper-tail log CDF: Code computes `log(1 - CDF_lower)` directly, which underflows when the lower CDF is close to 1. Should use stable log-complement.
- Quantile boundary: At p=1, the code may not handle the limiting case properly.

**Suggested fix:** 
1. Use log-space arithmetic throughout for log-densities and log-CDFs.
2. For upper-tail log CDFs, use `log1p(-p)` or equivalent stable computation.
3. For boundary quantiles, add explicit handling for p=1 (return the supremum).

---

## Issue 2: Truncated Beta quantiles and CDFs saturate in the tails

**Package version:** actuar 3.3.7

**Summary:** The truncated beta distribution (`ptrbeta`, `qtrbeta`) loses accuracy at extreme quantiles and tail probabilities due to inversion through rounded intermediate beta values.

**Reproducers:**

```r
# ptrbeta saturation at extreme lower tail
actuar::ptrbeta(0.4, rate = 0.003, shape1 = 1000, shape2 = 0.002)
# ACTUAL: 0
# EXPECTED: about 0.0960

# ptrbeta upper-tail log extreme
actuar::ptrbeta(1e300, rate = 2, shape1 = 5, shape2 = 3, 
                lower.tail = FALSE, log.p = TRUE)
# ACTUAL: -Inf
# EXPECTED: about -6905.96

# qtrbeta at p=0.5
actuar::qtrbeta(0.5, rate = 0.01, shape1 = 1, shape2 = 1)
# ACTUAL: Inf
# EXPECTED: expm1(log(2)/0.01) ≈ 1.27e30

# qtrbeta upper-tail quantile
actuar::qtrbeta(0.001, rate = 0.06, shape1 = 50, shape2 = 0.04, 
                lower.tail = FALSE)
# ACTUAL: Inf
# EXPECTED: about 7.4
```

**Expected values:**
- The truncated beta CDF is related to the base beta CDF through a linear transformation. At extreme parameters, the inverse beta quantile can be very large, but should remain finite.
- The upper-tail quantile should give a finite value within the support of the truncated distribution.

**Cause:** 
The code likely inverts the truncated beta CDF by computing a beta quantile and transforming it. When the beta quantile is extreme, the intermediate rounded value (0 or 1) causes loss of precision. Further, the upper-tail log CDF is computed as `log(1 - p)` directly, which underflows.

**Suggested fix:**
1. Use higher precision for intermediate beta quantile calculations, or use series expansions near the boundary.
2. For upper-tail log CDF, use log-complement arithmetic.
3. Detect when the result would be extreme and return a large finite value rather than `Inf`, unless the support truly excludes that region.

---

## Issue 3: Transformed Gamma d/p/q/r lose accuracy at extreme shapes

**Package version:** actuar 3.3.7

**Summary:** The transformed gamma distribution (`dtrgamma`, `ptrgamma`) has density cancellation, CDF underflow, and quantile saturation at extreme shape parameters, producing incorrect and non-finite results.

**Reproducers:**

```r
# dtrgamma log accuracy at extreme shape
actuar::dtrgamma(1, shape1 = 1e12, shape2 = 1, scale = 1e-12, log = TRUE)
# ACTUAL: 12.8984375
# EXPECTED: 12.8966 (difference at `kFdTol` precision level)

# ptrgamma CDF underflow at extreme shape
actuar::ptrgamma(0.9, shape1 = 1e-6, shape2 = 1e6)
# ACTUAL: 0
# EXPECTED: 0.9
```

**Expected values:** 
- The transformed gamma is a family parameterized by two shapes. At extreme shape ratios, the distribution should remain well-behaved.
- The CDF at the median should be close to 0.5, not saturated at the boundary.

**Cause:** 
The log-density likely involves log-gamma differences that cancel when both arguments are huge. The CDF computation may involve gamma quantile inversion that saturates due to underflow/overflow.

**Suggested fix:**
1. Use asymptotic expansions or series for log-gamma differences when shapes are extreme.
2. For CDF: use a reparametrization or a more stable inversion method.

---

## Issue 4: Generalized Beta d/p/q/r lose accuracy near unit-gamma limit

**Package version:** actuar 3.3.7

**Summary:** The generalized beta distribution (`dgenbeta`, `pgenbeta`, `qgenbeta`) has precision loss and saturation at extreme shape parameters, especially near the unit-gamma limiting case.

**Reproducers:**

```r
# dgenbeta log precision
actuar::dgenbeta(0.5, shape1 = 0.0001, shape2 = 2, shape3 = 100, log = TRUE)
# ACTUAL: -3.918854482
# EXPECTED: precise (not checked against reference, but should be stable)

# pgenbeta log underflow
actuar::pgenbeta(1e-10, shape1 = 0.0001, shape2 = 2, shape3 = 100, log.p = TRUE)
# ACTUAL: -Inf
# EXPECTED: finite (small negative log-probability)

# qgenbeta quantile saturation at unit-gamma limit
actuar::qgenbeta(0.5, shape1 = 0.0001, shape2 = 2, shape3 = 100)
# ACTUAL: 0.0008268997
# EXPECTED: about 7.81e-31 (much smaller; the distribution is concentrated near zero when shape1 is tiny)
```

**Expected values:**
- At tiny `shape1`, the generalized beta should be heavily concentrated near zero. The quantile at p=0.5 should be correspondingly very small.
- Log-probabilities should not underflow to `-Inf` at moderate arguments.

**Cause:** 
The generalized beta involves beta-function ratios at extreme shapes. Intermediate beta quantiles may saturate due to rounding, and log computations may underflow.

**Suggested fix:**
1. Use `lbeta` and careful rearrangement for log-density.
2. For CDF/quantile: recognize the extreme-shape case and use asymptotic approximations or table-based methods.

---

## Issue 5: Poisson Inverse Gaussian log density incorrect near Poisson limit and at large counts

**Package version:** actuar 3.3.7

**Summary:** The Poisson Inverse Gaussian log density (`dpoisinvgauss`) is fundamentally incorrect near the Poisson limit (large shape parameter) and underflows at large counts, giving `log(2)` instead of a probability and `-Inf` instead of a finite log-mass.

**Reproducers:**

```r
# dpoisinvgauss near Poisson limit: unscaled recurrence
actuar::dpoisinvgauss(2, mean = 2, shape = 1e20, log = TRUE)
# ACTUAL: 0.6931471806 (which is log(2))
# EXPECTED: about -1.306852819 (the true log probability)

# dpoisinvgauss at large count with large shape (underflow)
actuar::dpoisinvgauss(1000, mean = 1000, shape = 1e12, log = TRUE)
# ACTUAL: -Inf
# EXPECTED: about -4.372900006
```

**Expected values:** 
These are computed in `notes/evaluations/2026-10-29-pig/analysis.md` using:
1. The stable log-ratio recurrence for the PMF.
2. Integration of the Poisson kernel against the inverse-Gaussian distribution.
3. Independent 100-decimal mpmath validation.

The function is part of the Poisson-inverse-Gaussian family, used in actuarial and econometric applications.

**Cause:** 
The implementation uses an unscaled recurrence formula that suffers from:
1. **Cancellation:** Computing `log p0` as `(1 - sqrt(1 + 2*mu²/lambda)) * lambda/mu` loses digits when `2*mu²/lambda` is small (large shape, near-Poisson regime).
2. **Underflow:** Exponentiating intermediate masses even when `log=TRUE` is requested; if both starting masses underflow, the recurrence stays zero.

The exact mechanisms are documented with a 990-point validation grid in the evaluation folder.

**Suggested fix:** 
A draft C patch is available in `notes/evaluations/2026-10-29-pig/actuar-logpmf.patch`. The fix implements:
1. Stable log-space initial masses using the recurrence `log p0 = -2*mu / (1 + sqrt(1 + 2*mu²/lambda))` and `log(p1/p0) = log(mu) - 0.5*log(1 + 2*mu²/lambda)`.
2. Log-addition of recurrence terms to avoid underflow.
3. Support for the Levy limit internally for validation.

The patch passes all 990 test points with a maximum absolute log-error of 3.02e-11.

---

## Issue 6: Filtered Pareto quantile upper-tail log probability rounding

**Package version:** actuar 3.3.7

**Summary:** The filtered Pareto quantile (`qfpareto`) rounds extreme upper-tail log-probabilities to `Inf`.

**Reproducer:**

```r
actuar::qfpareto(-40, min = 0, shape = 0.3, rate = 0.7, 
                 prob = 0.4, lower.tail = FALSE, log.p = TRUE)
# ACTUAL: Inf
# EXPECTED: about 6.89e81
```

**Expected value:** At very small survival probability (`log.p = -40`, i.e., `p ≈ 4e-18`), the quantile can be huge but should remain finite. The value 6.89e81 is representable in double precision (it is well below `1.8e308`).

**Cause:** 
The code likely computes the quantile by inverting the CDF on the log-probability scale, and rounds intermediate values too aggressively, causing the result to overflow to `Inf`.

**Suggested fix:** 
Ensure that intermediate quantile calculations stay within a representable range, and only round to `Inf` when the result genuinely exceeds the representable range or when the support is truly bounded above.

---

## Issue 7: Logarithmic distribution upper-tail log CDF cancellation

**Package version:** actuar 3.3.7

**Summary:** The zero-modified logarithmic CDF at extreme parameters in the upper tail has severe cancellation error, giving a value far from the correct one.

**Reproducer:**

```r
actuar::pzmlogarithmic(10000, prob = 0.4, pzt = 0.3, 
                       lower.tail = FALSE, log.p = TRUE)
# ACTUAL: -36.40032833
# EXPECTED: below -9000 (many orders of magnitude more negative)
```

**Expected value:** At count 10000 with `prob = 0.4`, the tail probability should be astronomically small. The log value should reflect this.

**Cause:** 
Likely the code computes `log(1 - CDF)` where the CDF is extremely close to 1, losing all significant digits.

**Suggested fix:** 
Rewrite the CDF to directly compute the log-survival probability using log-space arithmetic, avoiding the complement computation.

---

## Summary table

| Family | Functions | Defect | Severity |
|--------|-----------|--------|----------|
| Zero-truncated/modified | dztnbinom, pztpois, qztbinom, qzmbinom, pzmlogarithmic | Log underflow and boundary handling | High |
| Truncated Beta | ptrbeta, qtrbeta | Saturation in tails | High |
| Transformed Gamma | dtrgamma, ptrgamma | Shape-dependent accuracy loss | Medium |
| Generalized Beta | dgenbeta, pgenbeta, qgenbeta | Extreme shape saturation | Medium |
| Poisson Inverse Gaussian | dpoisinvgauss | Fundamental recurrence error | High (with patch available) |
| Filtered Pareto | qfpareto | Upper-tail log rounding | Medium |

---

## Notes

- A draft patch for `dpoisinvgauss` is available and has been validated against an independent 100-decimal mpmath implementation on a 990-point grid.
- The underlying causes are primarily numerical (cancellation, underflow, overflow) rather than algorithmic. Most can be addressed with stable arithmetic in log-space and careful handling of extreme parameters.

