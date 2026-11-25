# TASK-335 — Half-Cauchy and Gompertz MLEs in Lean

The [Lean module](../../lean/UnivariateML/Family/Misc.lean) proves the free and
held-parameter MLE classifications for both families in exact real arithmetic.
Every target is closed. The cutoff equalities give finite unattained suprema,
with distinct scale-zero and exponential limits.

## Dossier and fitter discrepancies

- **Gompertz concavity is a property of the log likelihood.** The dossier's
  Details calls the held-hazard likelihood strictly concave in the rate. Taken
  literally this is false. For one observation at 1 and held initial hazard 10,
  the continuous extension at rate zero has log-likelihood derivatives
  \(\ell'(0)=-4\) and \(\ell''(0)=-10/3\). The second derivative of the
  likelihood is then \(e^{\ell(0)}(16-10/3)>0\), and stays positive for small
  positive rates. Lean proves strict concavity for the held-hazard log
  likelihood and for the profiled log likelihood. The counterexample is an
  algebraic source check. [Gompertz dossier](../../../univariateML-cpp/include/cppunivariate/distributions/gompertz/README.md)
- **The opening Gompertz fitter comment omits equality.** With variance
  computed using divisor \(n\), the excluded face wins for
  \(\operatorname{sd}(x)\ge\bar{x}\), including \(x=(0,2)\). The
  dossier's existence statement and the free-face comparison in the fitter
  include equality. [Gompertz fitter](../../../univariateML-cpp/include/cppunivariate/distributions/gompertz/mle.hpp)
- The brief's shorthand about nonconstant Gompertz samples requires the
  dossier's strict moment condition. Lean states it as \(nQ<2S^2\), equivalent
  to \(\overline{x^2}<2\bar{x}^2\) for nonempty samples. The held-hazard
  condition is \(aQ/2<S\).
- The half-Cauchy classification agrees with the dossier and the half-t
  delegate. Lean represents the sample minimum as an attained lower bound, uses
  the multiplicity of the specified endpoint, and states the collapse in log
  units. Exponentiation gives the corresponding classification on the likelihood
  scale. The held-endpoint scale theorems also apply algebraically to the
  Cauchy-derived objective at any endpoint. For half-Cauchy interpretation the
  endpoint must satisfy the support condition in `domain`.
  [Half-Cauchy dossier](../../../univariateML-cpp/include/cppunivariate/distributions/hcauchy/README.md),
  [half-Cauchy fitter](../../../univariateML-cpp/include/cppunivariate/distributions/hcauchy/mle.hpp)

## Verification and effort

- Workstation, Lean 4.36.0, single-slot wrapper. Direct
  `lake env lean UnivariateML/Family/Misc.lean` passed. Lake's stricter linter
  then flagged three style issues in the new module. After those fixes,
  `lake build +UnivariateML.Family.Misc` passed with no warnings (7,167 jobs).
- The default `lake build` passed (7,396 jobs), with existing warnings in
  Discrete, Frechet, GPD, Kumaraswamy, LomaxTwoPoint and Weibull, and an
  informational tactic suggestion in FoldedNormal. The explicit module build
  covers the new file before the coordinator adds its root import. The runs
  used the installed Mathlib artifacts.
- Half-Cauchy took roughly five minutes of proof authoring and debugging.
  The held-endpoint work reused Cauchy's scale classification and collapse
  limits. The free-endpoint work proved strict increase over the support and
  lifted the scale result to the joint domain.
- Gompertz took roughly fifteen minutes of proof authoring and debugging.
  The held-rate proof used the strict logarithm inequality. The held-hazard
  proof differentiated the exposure integrals and gave a negative-score
  bracket. The joint proof added the positive weighted variance, the normalized
  tail-score limit, and the profile graph. Verification and CPU-slot waits were
  shared across the targets and are excluded from these estimates.
- The remaining integration step is the coordinator's import in
  `UnivariateML.lean`. Floating-point solver accuracy and margin decisions are
  outside the exact-real statements.

## Theorem statements

The statements below are copied from the module. Within each namespace the
sample has type `Fin n → ℝ`. In Gompertz, `joint` stores the rate first and the
initial hazard second.

### `UnivariateML.HalfCauchy`

For a held positive scale, the density product is positive exactly when the endpoint is at or below every observation.

```lean
theorem heldPair_support (x : Fin n → ℝ) {μ σ : ℝ} (hσ : 0 < σ) :
    (0 < ∏ i, density μ σ (x i)) ↔ (μ, σ) ∈ domain x
```

At a feasible held pair, the logarithm of the density product equals the stated objective.

```lean
theorem heldPair_logLik (x : Fin n → ℝ) {μ σ : ℝ} (hp : (μ, σ) ∈ domain x) :
    log (∏ i, density μ σ (x i)) = logLik x μ σ
```

The half-Cauchy log-scale score is identical to the held-location Cauchy score.

```lean
theorem score_eq_cauchy (x : Fin n → ℝ) (μ t : ℝ) :
    score x μ t = Cauchy.heldScore x μ t
```

The score is the derivative of the log likelihood with respect to log scale.

```lean
theorem hasDerivAt_log_scale (x : Fin n → ℝ) (μ t : ℝ) :
    HasDerivAt (fun u ↦ logLik x μ (exp u)) (score x μ t) t
```

The log-scale score strictly decreases whenever any observation differs from the endpoint.

```lean
theorem score_strictAnti (x : Fin n → ℝ) (μ : ℝ) (h : ∃ i, x i ≠ μ) :
    StrictAnti (score x μ)
```

Less than half the mass at the held endpoint gives one positive root, and exactly that scale maximizes the log likelihood.

```lean
theorem heldEndpoint_root (hn : 0 < n) (x : Fin n → ℝ) (μ : ℝ)
    (h : 2 * Cauchy.multiplicity x μ < n) :
    (∃! σ, 0 < σ ∧ score x μ (log σ) = 0) ∧
      ∀ σ > 0, IsMaxOn (logLik x μ) (Ioi 0) σ ↔ score x μ (log σ) = 0
```

A unique maximizing positive scale exists exactly when the held endpoint has less than half the sample mass.

```lean
theorem heldEndpoint_iff (hn : 0 < n) (x : Fin n → ℝ) (μ : ℝ) :
    (∃! σ, σ ∈ Ioi (0 : ℝ) ∧ IsMaxOn (logLik x μ) (Ioi 0) σ) ↔
      2 * Cauchy.multiplicity x μ < n
```

With scale held positive, the endpoint MLE is the attained sample minimum, including for a constant sample.

```lean
theorem heldScale (x : Fin n → ℝ) {m σ : ℝ} (hmin : ∀ i, m ≤ x i)
    (hm : ∃ j, x j = m) (hσ : 0 < σ) :
    ∀ μ, μ ≤ m →
      (IsMaxOn (fun v ↦ logLik x v σ) {v | v ≤ m} μ ↔ μ = m)
```

A feasible joint maximizer must use the minimum and a maximizing scale at that minimum, and every such pair maximizes jointly.

```lean
theorem joint_argmax (x : Fin n → ℝ) {m : ℝ} (hmin : ∀ i, m ≤ x i)
    (hm : ∃ j, x j = m) {p : ℝ × ℝ} (hp : p ∈ domain x) :
    IsMaxOn (fun q ↦ logLik x q.1 q.2) (domain x) p ↔
      p.1 = m ∧ IsMaxOn (logLik x m) (Ioi 0) p.2
```

A unique joint maximizer exists exactly when the sample minimum has less than half the mass.

```lean
theorem joint_iff (hn : 0 < n) (x : Fin n → ℝ) {m : ℝ} (hmin : ∀ i, m ≤ x i)
    (hm : ∃ j, x j = m) :
    (∃! p, p ∈ domain x ∧ IsMaxOn (fun q ↦ logLik x q.1 q.2) (domain x) p) ↔
      2 * Cauchy.multiplicity x m < n
```

With more than half the observations at the endpoint, the log likelihood diverges as the scale decreases to zero.

```lean
theorem majority_collapse (x : Fin n → ℝ) {μ : ℝ} (h : n < 2 * Cauchy.multiplicity x μ) :
    Tendsto (logLik x μ) (𝓝[>] 0) atTop
```

With exactly half the mass at the endpoint, the scale-zero limit is finite, exceeds every positive-scale value, and is their supremum.

```lean
theorem half_collapse (hn : 0 < n) (x : Fin n → ℝ) {μ : ℝ}
    (h : 2 * Cauchy.multiplicity x μ = n) :
    Tendsto (logLik x μ) (𝓝[>] 0) (𝓝 (Cauchy.collapseValue x μ + (n : ℝ) * log 2)) ∧
      (∀ σ > 0, logLik x μ σ < Cauchy.collapseValue x μ + (n : ℝ) * log 2) ∧
      sSup (logLik x μ '' Ioi 0) = Cauchy.collapseValue x μ + (n : ℝ) * log 2
```

The log-scale score tends to the sample size minus twice the endpoint multiplicity at small scales, and to minus the sample size at large scales.

```lean
theorem score_limits (x : Fin n → ℝ) (μ : ℝ) :
    Tendsto (score x μ) atBot (𝓝 ((n : ℝ) - 2 * Cauchy.multiplicity x μ)) ∧
      Tendsto (score x μ) atTop (𝓝 (-(n : ℝ)))
```

With exactly half the sample mass at the minimum, the same boundary value is the unattained supremum over both coefficients.

```lean
theorem joint_half_collapse (hn : 0 < n) (x : Fin n → ℝ) {m : ℝ}
    (hmin : ∀ i, m ≤ x i) (hm : ∃ j, x j = m) (h : 2 * Cauchy.multiplicity x m = n) :
    let L := fun p : ℝ × ℝ ↦ logLik x p.1 p.2
    let c := Cauchy.collapseValue x m + (n : ℝ) * log 2
    Tendsto (logLik x m) (𝓝[>] 0) (𝓝 c) ∧
      (∀ p ∈ domain x, L p < c) ∧ sSup (L '' domain x) = c
```

A feasible strict-majority endpoint makes the joint likelihood unbounded.

```lean
theorem joint_majority_unbounded (x : Fin n → ℝ) {μ : ℝ} (hx : ∀ i, μ ≤ x i)
    (h : n < 2 * Cauchy.multiplicity x μ) :
    ¬ BddAbove ((fun p : ℝ × ℝ ↦ logLik x p.1 p.2) '' domain x)
```

### `UnivariateML.Gompertz`

At rate zero, the integrated unit-hazard is the sample sum.

```lean
theorem integrated_zero (x : Fin n → ℝ) : integrated x 0 = sampleSum x
```

The integrated unit-hazard has derivative equal to half the sum of squares at rate zero.

```lean
theorem hasDerivAt_integrated_zero (x : Fin n → ℝ) :
    HasDerivAt (integrated x) (squareSum x / 2) 0
```

At a nonzero rate, the integrated unit-hazard equals the sum of the individual closed-form exposures.

```lean
theorem integrated_eq (x : Fin n → ℝ) {b : ℝ} (hb : b ≠ 0) :
    integrated x b = ∑ i, (exp (b * x i) - 1) / b
```

When the sample sum is positive, the unique conditional initial hazard is the sample size divided by integrated unit-hazard.

```lean
theorem heldRate (hn : 0 < n) (x : Fin n → ℝ) (hx : ∀ i, 0 ≤ x i)
    (hs : 0 < sampleSum x) (b : ℝ) :
    0 < hazard x b ∧
      ∀ a > 0, (IsMaxOn (fun u ↦ logLik x u b) (Ioi 0) a ↔ a = hazard x b)
```

Plugging the conditional initial hazard into the log likelihood gives the stated rate profile.

```lean
theorem profile_eq (hn : 0 < n) (x : Fin n → ℝ) (hx : ∀ i, 0 ≤ x i)
    (hs : 0 < sampleSum x) (b : ℝ) : logLik x (hazard x b) b = profile x b
```

With a positive held hazard and positive sample sum, the rate score strictly decreases.

```lean
theorem heldScore_strictAnti (x : Fin n → ℝ) (hx : ∀ i, 0 ≤ x i)
    (hs : 0 < sampleSum x) {a : ℝ} (ha : 0 < a) : StrictAnti (heldScore x a)
```

The rate profile score strictly decreases whenever the sample sum is positive.

```lean
theorem profileScore_strictAnti (hn : 0 < n) (x : Fin n → ℝ) (hx : ∀ i, 0 ≤ x i)
    (hs : 0 < sampleSum x) : StrictAnti (profileScore x)
```

With a positive held hazard and positive sample sum, the log likelihood is strictly concave in the rate.

```lean
theorem heldHazard_strictConcave (x : Fin n → ℝ) (hx : ∀ i, 0 ≤ x i)
    (hs : 0 < sampleSum x) {a : ℝ} (ha : 0 < a) :
    StrictConcaveOn ℝ univ (logLik x a)
```

With positive sample sum, the profiled log likelihood is strictly concave in the rate.

```lean
theorem profile_strictConcave (hn : 0 < n) (x : Fin n → ℝ) (hx : ∀ i, 0 ≤ x i)
    (hs : 0 < sampleSum x) : StrictConcaveOn ℝ univ (profile x)
```

The two limiting scores at the exponential face have the stated sample-moment formulas.

```lean
theorem scores_zero (x : Fin n → ℝ) (a : ℝ) :
    heldScore x a 0 = sampleSum x - a * (squareSum x / 2) ∧
      profileScore x 0 = sampleSum x - (n : ℝ) * (squareSum x / (2 * sampleSum x))
```

For positive held coefficients, the product of densities equals the exponential of the log likelihood.

```lean
theorem heldPair_likelihood (x : Fin n → ℝ) {a b : ℝ} (ha : 0 < a) (hb : 0 < b) :
    (∏ i, a * exp (b * x i - a * ((exp (b * x i) - 1) / b))) = exp (logLik x a b)
```

With a positive held hazard and positive sample sum, the unique rate MLE exists exactly when the score at zero is positive.

```lean
theorem heldHazard_iff (x : Fin n → ℝ) (hx : ∀ i, 0 ≤ x i)
    (hs : 0 < sampleSum x) {a : ℝ} (ha : 0 < a) :
    (∃! b, b ∈ Ioi (0 : ℝ) ∧ IsMaxOn (logLik x a) (Ioi 0) b) ↔
      a * squareSum x / 2 < sampleSum x
```

A nonpositive score at zero gives the excluded exponential face as the finite unattained supremum, at the held hazard.

```lean
theorem heldHazard_face (x : Fin n → ℝ) (hx : ∀ i, 0 ≤ x i)
    (hs : 0 < sampleSum x) {a : ℝ} (ha : 0 < a) (h : sampleSum x ≤ a * squareSum x / 2) :
    Tendsto (logLik x a) (𝓝[>] 0) (𝓝 (logLik x a 0)) ∧
      (∀ b > 0, logLik x a b < logLik x a 0) ∧
      sSup (logLik x a '' Ioi 0) = logLik x a 0
```

With all observations zero, the log likelihood equals the sample size times the log initial hazard, independently of rate.

```lean
theorem zero_logLik (x : Fin n → ℝ) (hx : ∀ i, x i = 0) (a b : ℝ) :
    logLik x a b = (n : ℝ) * log a
```

When all observations are zero and the hazard is held, every positive rate maximizes.

```lean
theorem heldHazard_zero (x : Fin n → ℝ) (hx : ∀ i, x i = 0) (a b : ℝ) :
    IsMaxOn (logLik x a) (Ioi 0) b
```

When all observations are zero and the rate is held, the likelihood grows without bound as the initial hazard increases.

```lean
theorem heldRate_zero (hn : 0 < n) (x : Fin n → ℝ) (hx : ∀ i, x i = 0) (b : ℝ) :
    Tendsto (fun a ↦ logLik x a b) atTop atTop
```

A unique initial hazard MLE at a held rate exists exactly when the sample sum is positive.

```lean
theorem heldRate_iff (hn : 0 < n) (x : Fin n → ℝ) (hx : ∀ i, 0 ≤ x i) (b : ℝ) :
    (∃! a, a ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun u ↦ logLik x u b) (Ioi 0) a) ↔
      0 < sampleSum x
```

At large rates, the profile score tends to the sample sum minus the sample size times the maximum observation.

```lean
theorem profileScore_atTop (x : Fin n → ℝ) (hx : ∀ i, 0 ≤ x i)
    (hs : 0 < sampleSum x) {m : ℝ} (hmax : ∀ i, x i ≤ m) (hm : ∃ j, x j = m) :
    Tendsto (profileScore x) atTop (𝓝 (sampleSum x - (n : ℝ) * m))
```

For nonconstant nonnegative data, the rate profile has a unique positive maximizer exactly on the strict moment side of the cutoff.

```lean
theorem profile_iff (hn : 0 < n) (x : Fin n → ℝ) (hx : ∀ i, 0 ≤ x i)
    (hne : ∃ i j, x i ≠ x j) :
    (∃! b, b ∈ Ioi (0 : ℝ) ∧ IsMaxOn (profile x) (Ioi 0) b) ↔
      (n : ℝ) * squareSum x < 2 * sampleSum x ^ 2
```

Every joint maximizing pair is a maximizing rate paired with its unique conditional hazard, and every such pair maximizes jointly.

```lean
theorem joint_argmax (hn : 0 < n) (x : Fin n → ℝ) (hx : ∀ i, 0 ≤ x i)
    (hs : 0 < sampleSum x) :
    {p ∈ domain | IsMaxOn (joint x) domain p} =
      (fun b ↦ (b, hazard x b)) '' {b ∈ Ioi (0 : ℝ) | IsMaxOn (profile x) (Ioi 0) b}
```

For nonconstant nonnegative data, there is a unique finite joint MLE exactly when the strict moment condition holds.

```lean
theorem joint_iff (hn : 0 < n) (x : Fin n → ℝ) (hx : ∀ i, 0 ≤ x i)
    (hne : ∃ i j, x i ≠ x j) :
    (∃! p, p ∈ domain ∧ IsMaxOn (joint x) domain p) ↔
      (n : ℝ) * squareSum x < 2 * sampleSum x ^ 2
```

At or above the moment cutoff, the profiled path approaches the exponential log likelihood with hazard equal to the reciprocal mean. Its value is the joint supremum and every positive-rate pair lies strictly below it.

```lean
theorem exponential_face (hn : 0 < n) (x : Fin n → ℝ) (hx : ∀ i, 0 ≤ x i)
    (hs : 0 < sampleSum x) (h : 2 * sampleSum x ^ 2 ≤ (n : ℝ) * squareSum x) :
    Tendsto (fun b ↦ joint x (b, hazard x b)) (𝓝[>] 0) (𝓝 (logLik x (n / sampleSum x) 0)) ∧
      (∀ p ∈ domain, joint x p < logLik x (n / sampleSum x) 0) ∧
      sSup (joint x '' domain) = logLik x (n / sampleSum x) 0
```

When all observations coincide, the joint log-likelihood image is unbounded. This covers every nonnegative constant sample.

```lean
theorem constant_unbounded (hn : 0 < n) (x : Fin n → ℝ) {c : ℝ}
    (hx : ∀ i, x i = c) : ¬ BddAbove (joint x '' domain)
```
