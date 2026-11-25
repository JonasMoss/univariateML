# Lean evaluation of the folded-normal held-coefficient MLEs

TASK-302. All five targets are proved for a nonempty nonnegative sample in
exact real arithmetic. The public statements below are in
`UnivariateML.FoldedNormal`. [Lean module](../../lean/UnivariateML/Family/FoldedNormal.lean)

## Source discrepancies and formalization choices

- **The dossier still lists uniqueness at positive held location as unestablished.**
  The later research report proves it, and Lean verifies the full global
  uniqueness claim for \(D>0\). The C++ held-positive-location branch uses a
  scalar search with chord bounds and returns `IterationFailure` if its
  chord budget is exhausted. The proof establishes the exact arithmetic
  score bracket and uniqueness and gives no binary64 guarantee for that
  search. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/foldnorm/README.md)
  [Fitter](../../../univariateML-cpp/include/cppunivariate/distributions/foldnorm/mle.hpp)
  [Research derivation](2026-11-25-frechet-math-report.md#folded-normal)
- **Strict decrease of the held-scale normalized score needs a positive
  observation.** The job's target omits this qualification. The research
  report and dossier include it. With all observations zero the normalized
  score is identically \(-1\) for positive location. The likelihood still
  strictly decreases away from zero.
- The verified uniform bound is \(1/17\). The first three even terms of the
  cosh series give \(1+\cosh z\ge 2+z^2/2+z^4/24\). Expanding
  \((z^2-27/4)^2\ge0\) yields \(1+\cosh z\ge(17/16)z^2\), so each
  logistic correction contributes at most \(16/17\) to the derivative.
  This proof includes \(z=0\) directly.
- The objective is the mean log-likelihood with its Gaussian constant
  removed. A density identity verifies the shift, and strict likelihood
  inequalities express uniqueness. The held-scale proof extends
  \(\tanh(z)/z\) continuously at zero with value one and uses the resulting
  score value \(m_2/t-1\) to treat equality and bracket the positive root.

## 1. Density and variance score

For positive variance, the mean log of the two Gaussian components equals the Lean objective plus the parameter-independent normalization `-log (2 * π) / 2`.

```lean
theorem avg_log_density (hn : 0 < n) (x : Fin n → ℝ) (μ : ℝ) {t : ℝ} (ht : 0 < t) :
    avg (fun i ↦ log (density (x i) μ t)) = logLik x μ t - log (2 * π) / 2
```

The derivative in physical variance is \( -T(t)/(2t^2) \), which is the total log-likelihood derivative divided by sample size.

```lean
theorem hasDerivAt_logLik_variance (x : Fin n → ℝ) (μ : ℝ) {t : ℝ} (ht : 0 < t) :
    HasDerivAt (logLik x μ) (-T x μ t / (2 * t ^ 2)) t
```

## 2. Uniform derivative bound

The derivative of the residual is at least \(1/17\) for every positive variance. The bound holds even without sign restrictions on the observations or location.

```lean
theorem deriv_T_lower (hn : 0 < n) (x : Fin n → ℝ) (μ : ℝ) {t : ℝ} (ht : 0 < t) :
    (1 / 17 : ℝ) ≤ deriv (T x μ) t
```

## 3. Held-location maximum

If the mean squared residual \(D\) is positive, the score has a zero in \([D,m_2+\mu^2]\). This point is positive and strictly beats every other positive variance, so it is the unique global maximizer. The positive lower bound on the derivative of \(T\) also gives uniqueness of its zero.

```lean
theorem heldLocation (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 ≤ x i)
    {μ : ℝ} (hμ : 0 ≤ μ) (hD : 0 < D x μ) :
    ∃ r ∈ Icc (D x μ) (m₂ x + μ ^ 2),
      0 < r ∧ T x μ r = 0 ∧
        ∀ t > 0, t ≠ r → logLik x μ t < logLik x μ r
```

## 4. Collapse and the half-normal

When \(D=0\), the mean log-likelihood tends to positive infinity as variance decreases to zero. The Gaussian normalization shift and sample-size factor preserve divergence of the total log-likelihood for a nonempty sample. Positive variance tending to zero is equivalent to positive standard deviation tending to zero.

```lean
theorem heldLocation_unbounded {x : Fin n → ℝ} {μ : ℝ} (hD : D x μ = 0) :
    Tendsto (logLik x μ) (𝓝[>] 0) atTop
```

At zero location and positive second moment, the second moment is a feasible variance and strictly beats every other positive variance.

```lean
theorem halfNormal (hn : 0 < n) {x : Fin n → ℝ} (hm : 0 < m₂ x) :
    ∀ t > 0, t ≠ m₂ x → logLik x 0 t < logLik x 0 (m₂ x)
```

## 5. Held-scale maximum

At positive location the continuously extended score agrees with the normalized score in the source.

```lean
theorem locationScore_eq (x : Fin n → ℝ) {t μ : ℝ} (ht : 0 < t) (hμ : 0 < μ) :
    locationScore x t μ = avg (fun i ↦ x i * tanh (x i * μ / t)) / μ - 1
```

If any observation is positive, the score strictly decreases over nonnegative locations, with the continuous extension at zero included.

```lean
theorem locationScore_strictAnti (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 ≤ x i)
    (hpos : ∃ i, 0 < x i) {t : ℝ} (ht : 0 < t) :
    StrictAntiOn (locationScore x t) (Ici 0)
```

When the sample second moment is at most the held variance, zero strictly beats every positive location. This includes equality and the all-zero sample.

```lean
theorem heldScale_zero (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 ≤ x i)
    {t : ℝ} (ht : 0 < t) (hm : m₂ x ≤ t) :
    ∀ μ > 0, logLik x μ t < logLik x 0 t
```

Above the threshold, the score has a unique positive zero. Its location strictly beats every other nonnegative location.

```lean
theorem heldScale_positive (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 ≤ x i)
    {t : ℝ} (ht : 0 < t) (hm : t < m₂ x) :
    ∃ r > 0, locationScore x t r = 0 ∧
      (∀ μ > 0, locationScore x t μ = 0 → μ = r) ∧
      ∀ μ ≥ 0, μ ≠ r → logLik x μ t < logLik x r t
```

Zero is a maximizing location exactly when the sample second moment is at most the held variance.

```lean
theorem heldScale_zero_iff (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 ≤ x i)
    {t : ℝ} (ht : 0 < t) :
    IsMaxOn (fun μ ↦ logLik x μ t) (Ici 0) 0 ↔ m₂ x ≤ t
```

## Proof effort

| Target | Relative effort | Main proof work |
| --- | --- | --- |
| 1 | Moderate | Factor the Gaussian mixture, differentiate the reflected log-density, and collect the coefficient of the averaged logistic term. |
| 2 | Moderate | Differentiate the logistic correction, rewrite it using cosh, and derive the rational bound from the nonnegative series terms. |
| 3 | Moderate | Derive the two endpoint signs and apply the intermediate value theorem. Use the derivative signs on each side to prove strict global maximality. |
| 4 | Short | Bound the objective below by the diverging logarithmic term and evaluate the variance score at the half-normal solution. |
| 5 | Largest | Extend the tanh quotient at zero, prove strict decrease via \(\sinh(2z)>2z\), handle the all-zero data, and derive and bracket the location score. |

## Verification and open items

- On the workstation, `lake env lean UnivariateML/Family/FoldedNormal.lean`
  passed through the single-slot wrapper. Lean printed an informational
  `ring_nf` suggestion.
- On the workstation, `lake build` passed through the same wrapper.
  Lake reported `Build completed successfully (6598 jobs).` It used
  cached Mathlib and emitted two long-line warnings in the existing
  Weibull module. The standalone check covers FoldedNormal pending the
  coordinator's root import.
- All five targets are closed with no proof placeholders or added axioms.
  The integration check after the coordinator adds the import remains
  outside this job.
