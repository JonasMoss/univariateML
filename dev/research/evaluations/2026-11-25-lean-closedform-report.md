# Closed-form maximum likelihood in Lean

**TASK-308 · 2026-11-25**

Lean verifies the exact-real closed-form fits for normal, lognormal,
exponential, inverse Gaussian and Rayleigh samples, including every
supported single-held-coefficient case. Each fit characterization identifies
the entire set of feasible maximizers. All sample-size hypotheses refer to
nonempty, uncensored samples. [Lean module](../../lean/UnivariateML/Family/ClosedForm.lean)

## Discrepancies and scope

- **Rayleigh.** The brief's “some observation is positive” condition applies
  to exponential. For Rayleigh, a single zero makes the likelihood identically
  zero, including in a mixed sample with positive observations. Every positive
  scale then maximizes the likelihood. The fitter rejects that sample in both
  masks, as the dossier states. Lean models the endpoint using the ordinary
  likelihood, with the observation product as a factor. This preserves the
  zero that a real-valued logarithm cannot represent as minus infinity.
  [Rayleigh dossier](../../../univariateML-cpp/include/cppunivariate/distributions/rayleigh/README.md)
- **Normal.** The base existence paragraph leaves the nonempty-sample
  assumption implicit. Lean makes it explicit, in agreement with the
  fitter's rejection of empty input.
  [Normal dossier](../../../univariateML-cpp/include/cppunivariate/distributions/norm/README.md)
- **Inverse Gaussian.** The header's inverse-shape identity in terms of
  mean reciprocals applies at the fitted sample mean. For an arbitrary held
  mean, the shape is the sample size divided by the weighted residual sum
  evaluated at that held mean. Both formulas are proved in their respective
  settings.
  [Inverse Gaussian fitter](../../../univariateML-cpp/include/cppunivariate/distributions/invgauss/mle.hpp)
- Exponential and Rayleigh each have one coefficient. Their single-held
  mask fixes the whole parameter vector, so the corresponding theorem
  gives the likelihood evaluation on a singleton.
- The results cover the base families in exact arithmetic. Censoring,
  truncation, floating-point rounding and status propagation are outside
  these theorems. No requested base-family target is open.

## Proof effort per family

| Family | Proof effort |
| --- | --- |
| Normal | Built the shared strict-maximum argument using `log u < u - 1`, the positive-scale transformation and unboundedness witnesses. Centered the sum of squares and used the generic attained-profile lemma for the joint fit. |
| Lognormal | Transferred the normal theorems through the log map. Checked injectivity on positive data and carried the sample Jacobian through both maximization and unboundedness. |
| Exponential | Instantiated the shared log-rate maximum at the sample sum. Proved that a nonnegative sum is positive exactly when some datum is positive, and handled the all-zero case. |
| Inverse Gaussian | Expanded the weighted residual in reciprocal mean, completed the square and proved the alternate inverse-shape formula. Reused the log-rate and profiling arguments for the held and joint fits. |
| Rayleigh | Reused the positive-scale transformation and preserved the raw likelihood at zero. Proved positivity and the log-likelihood formula for held scale, and used two distinct positive scales to rule out uniqueness when a zero occurs. |

## Verification

The final standalone check, `lake env lean UnivariateML/Family/ClosedForm.lean`,
and final build, `lake build UnivariateML +UnivariateML.Family.ClosedForm`,
both exited zero on the workstation under the one-slot lock. The latter
reported `Build completed successfully (6692 jobs).` It compiled
`ClosedForm` without warnings and replayed existing warnings in
`Weibull.lean` (two long lines) and `GPD.lean` (one flexible tactic).
The explicit module target checks the new file while the root import is
reserved for the coordinator. The checks used the existing Mathlib cache.
The module has 34 public theorems and no proof placeholders.

## Theorems

The declarations below are copied from the module. Each namespace is under
`UnivariateML`. The `IsMaxOn` statements include parameter feasibility
where the domain restricts the coefficient. Each unboundedness theorem
rules out a global maximum, since a global maximum would provide a finite
upper bound. Exponentiation preserves the comparisons and takes unbounded
log-likelihood to unbounded likelihood.

### Normal

**`Normal.logLik_formula`**. The precision representation equals the normalized normal log-likelihood.

```lean
theorem logLik_formula (x : Fin n → ℝ) (μ : ℝ) {σ : ℝ} (_hσ : 0 < σ) :
    logLik x μ σ =
      -(n : ℝ) / 2 * log (2 * π) - n * log σ - residual x μ / (2 * σ ^ 2)
```

**`Normal.heldSd`**. At any held positive standard deviation, the only maximizing mean is the sample mean, including on constant data.

```lean
theorem heldSd (hn : 0 < n) (x : Fin n → ℝ) {σ : ℝ} (hσ : 0 < σ) (μ : ℝ) :
    IsMaxOn (fun t ↦ logLik x t σ) univ μ ↔ μ = mean x
```

**`Normal.heldMean`**. When any observation differs from the held mean, the only maximizing standard deviation is the residual root mean square with divisor `n`.

```lean
theorem heldMean (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ}
    (hx : ∃ i, x i ≠ μ) (σ : ℝ) :
    σ ∈ Ioi 0 ∧ IsMaxOn (logLik x μ) (Ioi 0) σ ↔ σ = sd x μ
```

**`Normal.heldMean_unbounded`**. If every observation equals the held mean, shrinking the standard deviation gives an unbounded objective.

```lean
theorem heldMean_unbounded (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ}
    (hx : ∀ i, x i = μ) :
    ¬ BddAbove (logLik x μ '' Ioi 0)
```

**`Normal.jointMLE`**. On nonconstant data, the whole maximizing set is the sample mean paired with the residual root mean square about that mean.

```lean
theorem jointMLE (hn : 0 < n) {x : Fin n → ℝ} (hx : ∃ i j, x i ≠ x j)
    (p : ℝ × ℝ) :
    p ∈ domain ∧ IsMaxOn (fun q : ℝ × ℝ ↦ logLik x q.1 q.2) domain p ↔
      p = (mean x, sd x (mean x))
```

**`Normal.constant_unbounded`**. On constant data, hold the mean at the common observation and shrink the standard deviation to obtain unbounded likelihood.

```lean
theorem constant_unbounded (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ¬ ∃ i j, x i ≠ x j) :
    ¬ BddAbove ((fun q : ℝ × ℝ ↦ logLik x q.1 q.2) '' domain)
```

**`Normal.existsUnique_mle_iff`**. A unique joint MLE exists exactly when the sample contains distinct observations.

```lean
theorem existsUnique_mle_iff (hn : 0 < n) (x : Fin n → ℝ) :
    (∃! p : ℝ × ℝ, p ∈ domain ∧
      IsMaxOn (fun q : ℝ × ℝ ↦ logLik x q.1 q.2) domain p) ↔ ∃ i j, x i ≠ x j
```

**`Normal.existsUnique_heldMean_iff`**. At a held mean, a unique scale MLE exists exactly when some residual is nonzero.

```lean
theorem existsUnique_heldMean_iff (hn : 0 < n) (x : Fin n → ℝ) (μ : ℝ) :
    (∃! σ, σ ∈ Ioi 0 ∧ IsMaxOn (logLik x μ) (Ioi 0) σ) ↔ ∃ i, x i ≠ μ
```

### Lognormal

**`Lognormal.heldSd`**. With log standard deviation held, the only maximizing log mean is the average of the logarithms.

```lean
theorem heldSd (hn : 0 < n) (x : Fin n → ℝ) (_hx : ∀ i, 0 < x i)
    {σ : ℝ} (hσ : 0 < σ) (μ : ℝ) :
    IsMaxOn (fun t ↦ logLik x t σ) univ μ ↔ μ = Normal.mean (logSample x)
```

**`Lognormal.heldMean`**. At a held log mean, a nonzero log residual gives the unique log standard deviation as the residual root mean square on the log scale.

```lean
theorem heldMean (hn : 0 < n) {x : Fin n → ℝ} (_hx : ∀ i, 0 < x i) {μ : ℝ}
    (h : ∃ i, log (x i) ≠ μ) (σ : ℝ) :
    σ ∈ Ioi 0 ∧ IsMaxOn (logLik x μ) (Ioi 0) σ ↔ σ = Normal.sd (logSample x) μ
```

**`Lognormal.jointMLE`**. For positive nonconstant observations, the unique joint fit is the normal fit of their logarithms.

```lean
theorem jointMLE (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (h : ∃ i j, x i ≠ x j) (p : ℝ × ℝ) :
    p ∈ Normal.domain ∧
      IsMaxOn (fun q : ℝ × ℝ ↦ logLik x q.1 q.2) Normal.domain p ↔
      p = (Normal.mean (logSample x), Normal.sd (logSample x) (Normal.mean (logSample x)))
```

**`Lognormal.heldMean_unbounded`**. If all logged observations equal the held log mean, shrinking the log standard deviation makes the likelihood unbounded.

```lean
theorem heldMean_unbounded (hn : 0 < n) {x : Fin n → ℝ} (_hx : ∀ i, 0 < x i) {μ : ℝ}
    (h : ∀ i, log (x i) = μ) :
    ¬ BddAbove (logLik x μ '' Ioi 0)
```

**`Lognormal.constant_unbounded`**. A positive constant sample gives unbounded likelihood in the joint problem.

```lean
theorem constant_unbounded (hn : 0 < n) {x : Fin n → ℝ} (_hx : ∀ i, 0 < x i)
    (h : ¬ ∃ i j, x i ≠ x j) :
    ¬ BddAbove ((fun q : ℝ × ℝ ↦ logLik x q.1 q.2) '' Normal.domain)
```

**`Lognormal.existsUnique_mle_iff`**. For positive data, a unique joint fit exists exactly when the original observations are nonconstant.

```lean
theorem existsUnique_mle_iff (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) :
    (∃! p : ℝ × ℝ, p ∈ Normal.domain ∧
      IsMaxOn (fun q : ℝ × ℝ ↦ logLik x q.1 q.2) Normal.domain p) ↔ ∃ i j, x i ≠ x j
```

**`Lognormal.existsUnique_heldMean_iff`**. At a held log mean, a unique scale MLE exists exactly when some logged observation differs from it.

```lean
theorem existsUnique_heldMean_iff (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) (μ : ℝ) :
    (∃! σ, σ ∈ Ioi 0 ∧ IsMaxOn (logLik x μ) (Ioi 0) σ) ↔ ∃ i, log (x i) ≠ μ
```

### Exponential

**`Exponential.mle`**. On nonnegative data with a positive observation, the only maximizing rate is the sample size divided by the sum.

```lean
theorem mle (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 ≤ x i)
    (h : ∃ i, 0 < x i) (r : ℝ) :
    r ∈ Ioi 0 ∧ IsMaxOn (logLik x) (Ioi 0) r ↔ r = rate x
```

**`Exponential.allZero_unbounded`**. When all observations are zero, the likelihood is unbounded as the rate grows.

```lean
theorem allZero_unbounded (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, x i = 0) :
    ¬ BddAbove (logLik x '' Ioi 0)
```

**`Exponential.heldRate`**. A held positive rate gives the stated real log-likelihood, including for all-zero data.

```lean
theorem heldRate (_hn : 0 < n) {x : Fin n → ℝ} (_hx : ∀ i, 0 ≤ x i)
    {r : ℝ} (hr : 0 < r) :
    r ∈ Ioi 0 ∧ IsMaxOn (logLik x) {r} r ∧
      logLik x r = n * log r - r * ∑ i, x i
```

**`Exponential.existsUnique_mle_iff`**. A nonnegative sample has a unique free-rate MLE exactly when at least one observation is positive.

```lean
theorem existsUnique_mle_iff (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 ≤ x i) :
    (∃! r, r ∈ Ioi 0 ∧ IsMaxOn (logLik x) (Ioi 0) r) ↔ ∃ i, 0 < x i
```

### Inverse Gaussian

**`InverseGaussian.logLik_formula`**. The log-likelihood includes the normalizing term and the parameter-independent sum of log observations.

```lean
theorem logLik_formula (x : Fin n → ℝ) (μ lam : ℝ) :
    logLik x μ lam = (n : ℝ) / 2 * log lam - (n : ℝ) / 2 * log (2 * π) -
      (3 / 2) * (∑ i, log (x i)) - lam / 2 * residual x μ
```

**`InverseGaussian.inverse_shape_at_mean`**. At the sample mean, the fitted inverse shape equals the average reciprocal minus the reciprocal of the average.

```lean
theorem inverse_shape_at_mean (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) :
    (shape x (mean x))⁻¹ = (∑ i, (x i)⁻¹) / n - (mean x)⁻¹
```

**`InverseGaussian.heldShape`**. At any held positive shape, the sample mean is the unique maximizing mean, including on constant data.

```lean
theorem heldShape (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {lam : ℝ} (hlam : 0 < lam) (μ : ℝ) :
    μ ∈ Ioi 0 ∧ IsMaxOn (fun t ↦ logLik x t lam) (Ioi 0) μ ↔ μ = mean x
```

**`InverseGaussian.heldMean`**. At a positive held mean with some nonzero residual, the unique maximizing shape is the sample size divided by the weighted squared residual sum.

```lean
theorem heldMean (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {μ : ℝ} (hμ : 0 < μ) (h : ∃ i, x i ≠ μ) (lam : ℝ) :
    lam ∈ Ioi 0 ∧ IsMaxOn (logLik x μ) (Ioi 0) lam ↔ lam = shape x μ
```

**`InverseGaussian.heldMean_unbounded`**. If every observation equals the held mean, increasing the shape gives unbounded likelihood.

```lean
theorem heldMean_unbounded (hn : 0 < n) {x : Fin n → ℝ} (_hx : ∀ i, 0 < x i)
    {μ : ℝ} (_hμ : 0 < μ) (h : ∀ i, x i = μ) :
    ¬ BddAbove (logLik x μ '' Ioi 0)
```

**`InverseGaussian.jointMLE`**. For positive nonconstant data, the only joint fit pairs the sample mean with the shape computed at that mean.

```lean
theorem jointMLE (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (h : ∃ i j, x i ≠ x j) (p : ℝ × ℝ) :
    p ∈ domain ∧ IsMaxOn (fun q : ℝ × ℝ ↦ logLik x q.1 q.2) domain p ↔
      p = (mean x, shape x (mean x))
```

**`InverseGaussian.constant_unbounded`**. On constant positive data, fixing the mean at the common value and increasing the shape makes the joint likelihood unbounded.

```lean
theorem constant_unbounded (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (h : ¬ ∃ i j, x i ≠ x j) :
    ¬ BddAbove ((fun q : ℝ × ℝ ↦ logLik x q.1 q.2) '' domain)
```

**`InverseGaussian.existsUnique_mle_iff`**. For positive data, a unique joint MLE exists exactly when the sample is nonconstant.

```lean
theorem existsUnique_mle_iff (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) :
    (∃! p : ℝ × ℝ, p ∈ domain ∧
      IsMaxOn (fun q : ℝ × ℝ ↦ logLik x q.1 q.2) domain p) ↔ ∃ i j, x i ≠ x j
```

**`InverseGaussian.existsUnique_heldMean_iff`**. At a held positive mean, a unique shape MLE exists exactly when some residual is nonzero.

```lean
theorem existsUnique_heldMean_iff (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {μ : ℝ} (hμ : 0 < μ) :
    (∃! lam, lam ∈ Ioi 0 ∧ IsMaxOn (logLik x μ) (Ioi 0) lam) ↔ ∃ i, x i ≠ μ
```

### Rayleigh

**`Rayleigh.scale_formula`**. The closed form is the square root of half the empirical second moment.

```lean
theorem scale_formula (x : Fin n → ℝ) :
    scale x = sqrt ((∑ i, x i ^ 2) / (2 * n))
```

**`Rayleigh.mle`**. On strictly positive data, this closed form is the only maximizing scale.

```lean
theorem mle (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) (σ : ℝ) :
    σ ∈ Ioi 0 ∧ IsMaxOn (likelihood x) (Ioi 0) σ ↔ σ = scale x
```

**`Rayleigh.likelihood_zero`**. One zero observation makes the raw likelihood zero at every scale.

```lean
theorem likelihood_zero {x : Fin n → ℝ} (hx : ∃ i, x i = 0) (σ : ℝ) :
    likelihood x σ = 0
```

**`Rayleigh.zero_ties`**. With a zero observation, every positive scale is a global maximizer.

```lean
theorem zero_ties {x : Fin n → ℝ} (hx : ∃ i, x i = 0) (σ : ℝ) :
    σ ∈ Ioi 0 ∧ IsMaxOn (likelihood x) (Ioi 0) σ ↔ σ ∈ Ioi 0
```

**`Rayleigh.heldScale`**. At a held positive scale and with positive data, the likelihood is positive and has the stated finite log-likelihood.

```lean
theorem heldScale (_hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {σ : ℝ} (hσ : 0 < σ) :
    σ ∈ Ioi 0 ∧ IsMaxOn (likelihood x) {σ} σ ∧ 0 < likelihood x σ ∧
      log (likelihood x σ) = (∑ i, log (x i)) - 2 * n * log σ -
        (∑ i, x i ^ 2) / (2 * σ ^ 2)
```

**`Rayleigh.existsUnique_mle_iff`**. A nonnegative sample has a unique scale MLE exactly when every observation is strictly positive.

```lean
theorem existsUnique_mle_iff (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 ≤ x i) :
    (∃! σ, σ ∈ Ioi 0 ∧ IsMaxOn (likelihood x) (Ioi 0) σ) ↔ ∀ i, 0 < x i
```
