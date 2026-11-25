# Burr XII uniqueness with the first shape or scale held

TASK-322 proves all three targets in [`BurrHeld.lean`](../../lean/UnivariateML/Family/BurrHeld.lean). The proofs use exact real arithmetic for a nonempty sample. The held-first-shape coordinates are `(c,b)` and the held-scale shape coordinates are `(c,k)`.

## Dossier and source differences

**Likelihood normalization.** The November 25 derivation gives the endpoint value per observation after dropping the data-only term. Lean proves that value exactly. To recover the dossier’s total log likelihood, subtract the mean of the log observations and multiply by the sample size. Both likelihoods have the same maximizers. The normalization is verified by the two `logLik_eq_*` theorems below. [November 25 derivation](2026-11-25-burr-math-report.md#single-holds) · [Burr dossier](../../../univariateML-cpp/include/cppunivariate/distributions/burr/README.md#held-scale-certificate-task-1164)

One early dossier paragraph states the Pareto result for nonconstant data. The result also applies to constant data strictly above the held scale, as the dossier’s existence metadata and certificate section indicate. Constant data at or below scale give unbounded likelihood. The R notes mention Weibull and Pareto together immediately after describing the held-scale path. The dedicated certificate section and the held-scale C++ dispatch restrict this slice to the Pareto boundary or unboundedness, in agreement with the proved cases. [Burr dossier](../../../univariateML-cpp/include/cppunivariate/distributions/burr/README.md) · [Held-scale dispatch](../../../univariateML-cpp/include/cppunivariate/distributions/burr/mle.hpp) · [Collapse derivation](2026-11-02-burr-collapse-report.md)

## Proofs and effort

| Target | Proof work |
| --- | --- |
| 1. First shape held | The two distinct log observations yield strict concavity and a linear lower bound on the combined softplus penalty. Compact superlevels bound power away from zero and control both power and intercept from above in absolute value. The generic coercive-maximization theorem gives existence and uniqueness. For constant data, one explicit path makes the log likelihood diverge. |
| 2. Scale held with data below scale | This took the most formalization effort. The proof bounds the second derivative of the log of the softplus-to-power ratio on the negative side, uses log-sum-exp to combine log-convex terms, and derives strict profile concavity. One escape bound covers samples with a nonnegative maximum log ratio and samples lying wholly below scale. A scalar log inequality gives the unique fiber optimizer, and generic profiling lifts the profile maximum to the joint fit. The constant-data case at or below scale is also proved. |
| 3. Scale held with all data at or above scale | The proof tracks the zero-log-ratio terms in the finite-sum limit. The identity `G = c * avg z + R` gives the limit of `G / c` and a strict finite-power upper bound. The generic feasible-path lemma supplies the least-upper-bound inequality. |

The November 6 derivation supplies the held-scale curvature argument. The Lean proof establishes the same exponential inequality with the quadratic lower bound for `exp (u / 2)` and uses the same endpoint conditions. [November 6 held-scale derivation](2026-11-06-burr-scale-report.md)

## Lean statements

All statements are in `UnivariateML.BurrHeld`, with an implicit sample size `{n : ℕ}`. The normalized Pareto value is `-1 - log (avg z) - ((zeroCount z : ℝ) / n) * log 2`.

### First shape held

Two distinct log observations make the affine predictors distinguish any two different parameter pairs. The likelihood is strictly concave in positive power and unrestricted intercept.

```lean
theorem first_strictConcave (hn : 0 < n) {y : Fin n → ℝ} {k : ℝ} (hk : 0 < k)
    (hvar : ∃ i j, y i ≠ y j) : StrictConcaveOn ℝ firstDomain (firstLik y k)
```

The likelihood tends to minus infinity along the feasible subtype’s cocompact filter. This includes power tending to zero as well as escape in power or intercept.

```lean
theorem first_coercive (hn : 0 < n) {k : ℝ} (hk : 0 < k) {y : Fin n → ℝ}
    (hvar : ∃ i j, y i ≠ y j) :
    Tendsto (fun p : firstDomain ↦ firstLik y k p) (cocompact firstDomain) atBot
```

For positive held first shape and nonconstant data there is exactly one finite maximizer in power and intercept.

```lean
theorem first_existsUnique (hn : 0 < n) {k : ℝ} (hk : 0 < k) {y : Fin n → ℝ}
    (hvar : ∃ i j, y i ≠ y j) :
    ∃! p, p ∈ firstDomain ∧ IsMaxOn (firstLik y k) firstDomain p
```

For constant log data the path with intercept equal to minus power times the common log observation makes the objective arbitrarily large.

```lean
theorem first_constant_unbounded (hn : 0 < n) (k y₀ : ℝ) :
    ¬ BddAbove (firstLik (fun _ : Fin n ↦ y₀) k '' firstDomain)
```

### Scale held and finite maximization

At each positive power, the inverse mean softplus of the scaled log ratios is feasible and is the unique maximizing first shape.

```lean
theorem scale_bestK (hn : 0 < n) (z : Fin n → ℝ) {c : ℝ} (hc : 0 < c) :
    bestK z c ∈ Ioi (0 : ℝ) ∧
      IsMaxOn (fun k ↦ scaleLik z (c, k)) (Ioi 0) (bestK z c) ∧
      ∀ k ∈ Ioi (0 : ℝ), IsMaxOn (fun u ↦ scaleLik z (c, u)) (Ioi 0) k → k = bestK z c
```

The profile is strictly concave on positive powers for every nonempty sample, including constant samples.

```lean
theorem scale_strictConcave (hn : 0 < n) (z : Fin n → ℝ) :
    StrictConcaveOn ℝ (Ioi 0) (scaleProfile z)
```

With distinct log ratios and at least one negative log ratio, the profile tends to minus infinity at both excluded ends of the positive-power domain.

```lean
theorem scale_coercive (hn : 0 < n) {z : Fin n → ℝ}
    (hvar : ∃ i j, z i ≠ z j) (hneg : ∃ i, z i < 0) :
    Tendsto (fun c : Ioi (0 : ℝ) ↦ scaleProfile z c) (cocompact (Ioi (0 : ℝ))) atBot
```

Under the same data conditions the scalar profile attains its maximum at exactly one positive power.

```lean
theorem scale_profile_existsUnique (hn : 0 < n) {z : Fin n → ℝ}
    (hvar : ∃ i j, z i ≠ z j) (hneg : ∃ i, z i < 0) :
    ∃! c, c ∈ Ioi 0 ∧ IsMaxOn (scaleProfile z) (Ioi 0) c
```

The maximizing power and its unique fiber optimizer give exactly one joint maximum over both positive shapes.

```lean
theorem scale_existsUnique (hn : 0 < n) {z : Fin n → ℝ}
    (hvar : ∃ i j, z i ≠ z j) (hneg : ∃ i, z i < 0) :
    ∃! p, p ∈ shapeDomain ∧ IsMaxOn (scaleLik z) shapeDomain p
```

With scale held and every log ratio equal to the same nonpositive value, the two-shape likelihood is unbounded above.

```lean
theorem scale_constant_unbounded (hn : 0 < n) {z₀ : ℝ} (hz₀ : z₀ ≤ 0) :
    ¬ BddAbove (scaleLik (fun _ : Fin n ↦ z₀) '' shapeDomain)
```

### Scale held and the Pareto endpoint

As power increases without bound, the profile converges to the normalized Pareto value, including the penalty for data at the scale.

```lean
theorem scale_pareto_limit (hn : 0 < n) {z : Fin n → ℝ}
    (hz : ∀ i, 0 ≤ z i) (hpos : ∃ i, 0 < z i) :
    Tendsto (scaleProfile z) atTop (𝓝 (paretoValue z))
```

That limit is the least upper bound of the joint likelihood over both positive shapes, and every finite feasible likelihood is strictly smaller.

```lean
theorem scale_pareto_supremum (hn : 0 < n) {z : Fin n → ℝ}
    (hz : ∀ i, 0 ≤ z i) (hpos : ∃ i, 0 < z i) :
    IsLUB (scaleLik z '' shapeDomain) (paretoValue z) ∧
      ∀ p ∈ shapeDomain, scaleLik z p < paretoValue z
```

No finite pair of positive shapes maximizes the held-scale likelihood when all log ratios are nonnegative and some are positive.

```lean
theorem scale_pareto_no_max (hn : 0 < n) {z : Fin n → ℝ}
    (hz : ∀ i, 0 ≤ z i) (hpos : ∃ i, 0 < z i) :
    ¬ ∃ p ∈ shapeDomain, IsMaxOn (scaleLik z) shapeDomain p
```

### Connection to the original likelihood

The mean log-density equals the normalized held-scale objective minus the mean log observation.

```lean
theorem logLik_eq_scale (hn : 0 < n) (x : Fin n → ℝ) (k c s : ℝ) :
    logLik x k c s = scaleLik (logRatios x s) (c, k) - avg (fun i ↦ log (x i))
```

The same mean log-density is recovered in the held-first-shape coordinates using the intercept equal to minus power times log scale.

```lean
theorem logLik_eq_first (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {s : ℝ} (hs : 0 < s) (k c : ℝ) :
    logLik x k c s = firstLik (fun i ↦ log (x i)) k (c, -(c * log s)) -
      avg (fun i ↦ log (x i))
```

At positive power, the unique positive scale corresponding to a real intercept is the exponential of minus intercept divided by power.

```lean
theorem first_intercept_iff {c s b : ℝ} (hc : 0 < c) (hs : 0 < s) :
    -(c * log s) = b ↔ s = exp (-b / c)
```

## Validation and remaining scope

All checks ran on the workstation in the local single-slot wrapper with `nice -n 10`.

- `lake env lean UnivariateML/Family/BurrHeld.lean` passed without diagnostics.
- `lake build +UnivariateML.Family.BurrHeld` passed without diagnostics for the new module.
- The final default `lake build` exited zero and reported 7,362 jobs. Existing modules emitted lint warnings in Weibull, GPD, Fréchet, and LomaxTwoPoint, and a tactic suggestion in FoldedNormal.

All requested mathematical claims are closed, with no proof placeholders. The stationary-point counts mentioned in the November 25 report and the fitter’s floating-point acceptance logic are outside the three formalization targets. The root module import is reserved for the coordinator, so the explicit module build is the integration check for this file.
