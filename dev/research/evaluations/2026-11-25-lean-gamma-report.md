# Lean evaluation of Gamma MLE existence and uniqueness

TASK-297. Lean verifies the Gamma likelihood results for nonempty, strictly
positive samples in exact real arithmetic. The module works in shape–rate
coordinates and uses the log-likelihood per observation.
[Lean module](../../lean/UnivariateML/Family/Gamma.lean)

## Dossier findings

**Target 5’s boundedness premise is false. The dossier and fitter correctly
classify the constant-sample likelihood as unbounded.** Along
$\beta=k/c$, the mean stays at the common observation $c$ and the
log-likelihood per observation grows without bound. At positive integer
shapes $m$, the proof gives
$\tfrac12\log m-\log c-\tfrac12\log(2\pi)+o(1)$ by factorial Stirling.
The weak limit of the distributions along the path is the point mass at
$c$, since their variance is $c^2/k$. The Lean theorems concern the divergence
of the likelihood along the path and the absence of a finite maximizer.
[Gamma dossier](../../../univariateML-cpp/include/cppunivariate/distributions/gamma/README.md)

The dossier’s scale is $\theta=1/\beta$. Holding either coordinate holds the
other, so the held-rate result applies to the held-scale parameterization.
The dossier qualifies mathematical uniqueness with floating-point and solver
tolerances. Uniqueness is an exact-likelihood property. The tolerances govern
how the implementation approximates its maximizer. In `mle.hpp`, the
Newton stopping-rule comment calls shape divided by mean the scale. That
expression is the rate. The code returns its reciprocal.
[Gamma fitter](../../../univariateML-cpp/include/cppunivariate/distributions/gamma/mle.hpp)

## Theorems

All statements are in `UnivariateML.Gamma`. The set `domain` is
`Ioi 0 ×ˢ Ioi 0` in shape–rate order.

### 1. Held shape

For positive shape $k$, the rate `rate x k` is $k/\bar x>0$. Every other positive rate gives a strictly smaller likelihood.

```lean
theorem heldShape (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {k : ℝ} (hk : 0 < k) :
    0 < rate x k ∧ ∀ β > 0, β ≠ rate x k → logLik x k β < profile x k
```

### 2. Likelihood and profile

The sufficient-statistic expression in the module equals the mean of the sample log-densities. Scaling by the positive sample size gives the total log-likelihood.

```lean
theorem logLik_eq_avg (hn : 0 < n) (x : Fin n → ℝ) (k β : ℝ) :
    logLik x k β = avg (fun i ↦ logDensity k β (x i))
```

The gap $s=\log\bar x-\overline{\log x}$ is nonnegative, with equality precisely when every pair of observations agrees.

```lean
theorem jensenGap_spec (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) :
    0 ≤ jensenGap x ∧ (jensenGap x = 0 ↔ ∀ i j, x i = x j)
```

Evaluating the objective at the maximizing rate gives $k\log k-k-\log\Gamma(k)-ks-\overline{\log x}$.

```lean
theorem profile_eq (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {k : ℝ} (hk : 0 < k) :
    profile x k = k * log k - k - log (Real.Gamma k) - k * jensenGap x - avgLog x
```

The profile derivative at every positive shape is $\log k-\psi(k)-s$. The module defines `digamma` as the real derivative of $\log\Gamma$.

```lean
theorem hasDerivAt_profile (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {k : ℝ} (hk : 0 < k) : HasDerivAt (profile x) (scoreGap k - jensenGap x) k
```

### 3. Shape score

The function $G(k)=\log k-\psi(k)$ is strictly decreasing over positive shapes.

```lean
theorem scoreGap_strictAnti : StrictAntiOn scoreGap (Ioi 0)
```

The score gap tends to positive infinity as the shape approaches zero from above.

```lean
theorem scoreGap_tendsto_zero : Tendsto scoreGap (𝓝[>] 0) atTop
```

The score gap tends to zero as shape increases without bound.

```lean
theorem scoreGap_tendsto_atTop : Tendsto scoreGap atTop (𝓝 0)
```

### 4. Joint MLE

If at least two observations differ, there is exactly one maximizing positive shape–rate pair.

```lean
theorem jointMLE (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) :
    ∃! p : ℝ × ℝ, p ∈ domain ∧ IsMaxOn (fun q ↦ logLik x q.1 q.2) domain p
```

### 5. Constant sample

At the common observation $c>0$, the log-likelihood per observation tends to positive infinity along $\beta=k/c$ as $k\to\infty$.

```lean
theorem constant_unbounded (hn : 0 < n) {c : ℝ} (hc : 0 < c) :
    Tendsto (fun k ↦ logLik (fun _ : Fin n ↦ c) k (k / c)) atTop atTop
```

The total likelihood, obtained by exponentiating the total log-likelihood, diverges to positive infinity along the same path. Its extended-real supremum is $+\infty$.

```lean
theorem constant_likelihood_tendsto (hn : 0 < n) {c : ℝ} (hc : 0 < c) :
    Tendsto (fun k ↦ exp ((n : ℝ) * logLik (fun _ : Fin n ↦ c) k (k / c))) atTop atTop
```

No positive shape–rate pair attains a maximum for a constant positive sample.

```lean
theorem constant_noMLE (hn : 0 < n) {c : ℝ} (hc : 0 < c) :
    ¬ ∃ p ∈ domain, IsMaxOn (fun q ↦ logLik (fun _ : Fin n ↦ c) q.1 q.2) domain p
```

### 6. Held rate

The fixed-rate log-likelihood is strictly concave in positive shape.

```lean
theorem heldRate_strictConcave (x : Fin n → ℝ) (β : ℝ) :
    StrictConcaveOn ℝ (Ioi 0) (fun t ↦ logLik x t β)
```

Holding a positive rate gives exactly one maximizing positive shape. This includes constant samples. The proof applies for any real values of the sample summaries in the objective.

```lean
theorem heldRate (x : Fin n → ℝ) {β : ℝ} (_hβ : 0 < β) :
    ∃! k, k ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun t ↦ logLik x t β) (Ioi 0) k
```

## Proof effort

| Target | Relative effort | Proof work |
| --- | --- | --- |
| 1 | Short | Normalize a candidate rate by the proposed optimum and use the strict logarithm inequality away from ratio one. |
| 2 | Moderate | Expand the sample mean, use strict Jensen with equal weights, substitute the rate, and differentiate locally on positive shapes. |
| 3 | Largest | Convex secant bounds for log-Gamma give the large-shape limit. Differentiating the Gamma recurrence gives $G(k)-G(k+1)=k^{-1}-\log(1+k^{-1})$. For two ordered shapes, the difference of their gaps decreases under common integer shifts and tends to zero. Its first step gives strictness. The recurrence and $k\log k\to0$ give divergence at zero. |
| 4 | Moderate | Use the score signs and a compact interval to obtain a profile maximum. The tails are monotone, and the profile is strictly concave. The strict held-shape result transfers uniqueness to the pair. |
| 5 | Moderate | Strict increase of the constant-sample profile extends the divergence from factorial Stirling at integer shapes to real shapes. Positive sample size and exponentiation transfer divergence to the total likelihood. |
| 6 | Short | Combining strict decrease of $G$ with strict increase of $\log k$ gives strict increase of $\psi$. The held-rate score changes sign and the same compactness argument gives attainment. |

## Verification and open items

- Workstation, one reserved CPU slot. `lake env lean UnivariateML/Family/Gamma.lean` passed with no diagnostics.
- Workstation, one reserved CPU slot. `lake build +UnivariateML.Family.Gamma UnivariateML` completed successfully (7242 jobs). Gamma emitted no warnings. The build replayed two existing line-length warnings in `Weibull.lean` at lines 639 and 675. The explicit Gamma target builds the module before its root import is added.
- All likelihood proof obligations are closed. The module contains no admitted lemmas. The distributional point-mass limit and floating-point solver properties are outside the Lean statements.
