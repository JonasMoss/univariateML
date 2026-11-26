# Transformed gamma held-coordinate fits and the log-gamma transfer

TASK-372 · 2026-11-26 · Base `2d89fd4a`

[Lean module](../../lean/UnivariateML/Family/TransformedGamma.lean)

## Corrections and source findings

- **Fixed-scale concavity is in positive power.** Target 3 applies log-power to a fixed-scale objective. The dossiers put the log-power claim in the separate profile obtained by eliminating scale. Lean proves strict concavity of the fixed-scale objective in power and disproves concavity in log-power on an off-endpoint singleton. For the singleton `x = 2`, held scale `a = 1`, and held Gamma shape `k = 20 / log 2`, the average of the log-power objective at `0` and `2 * log 2` exceeds its value at their midpoint by 5. The exponential map is a bijection from real log-power to positive power, so uniqueness of a maximizer transports in either direction. [Transformed gamma dossier](../../../univariateML-cpp/include/cppunivariate/distributions/trgamma/README.md) · [Inverse transformed gamma dossier](../../../univariateML-cpp/include/cppunivariate/distributions/invtrgamma/README.md)
- **Constant samples distinguish the scale-free masks.** The scale is uniquely fitted on every nonempty positive sample when both shapes are held. Holding just one shape leaves a second free parameter, and the likelihood is unbounded for constant samples. Both interpretations of Target 1 are covered, including the reciprocal paths. This agrees with the dossiers and the November 25 report. [November 25 mathematics report](2026-11-25-trgamma-math-report.md)
- **The held-shape fitter comment overstates the right-tail score limit.** In the held-scale branch of `trgamma_fixed_scale_log`, take a singleton with centered log-observation `z = -1`. The power score is `1/p - k + exp(-p)` and tends to the finite negative value `-k`. The November 25 report gives the correct cases. The source bracket uses the negative sign, which this example still supplies. The reciprocal fitter delegates to the same branch on oriented logs. [Transformed gamma fitter](../../../univariateML-cpp/include/cppunivariate/distributions/trgamma/mle.hpp) · [Reciprocal fitter](../../../univariateML-cpp/include/cppunivariate/distributions/invtrgamma/mle.hpp) · [November 25 mathematics report](2026-11-25-trgamma-math-report.md)
- **The log-gamma claims agree with Gamma.** The public parameters are Gamma shape and rate of the logs. The sample support is strictly above one. The fixed shape/rate-ratio path in the dossier transfers directly. [Log-gamma dossier](../../../univariateML-cpp/include/cppunivariate/distributions/lgamma/README.md) · [Log-gamma fitter](../../../univariateML-cpp/include/cppunivariate/distributions/lgamma/mle.hpp)

The objectives are mean log likelihoods. Multiplication by the positive sample size preserves comparisons and divergence on the nonempty samples used here. Pair-valued theorems use the positive product domain `Gamma.domain` and give the coordinate order in the reading below. The proofs concern exact real arithmetic.

## Proof and verification

With power held, the Gamma sample is `exp (p * log (x i))`, and the positive rate is `exp (-p * log a)`. The scale-to-rate map is bijective. The Jacobian is independent of the free coordinates. With Gamma shape alone held, the log-data loss is `exp t - k * t`. Strict convexity and a positive linear lower bound allow the generic location–scale existence and uniqueness result. For a held scale and Gamma shape the same lower bound makes every power superlevel set lie in a compact subinterval of positive powers whenever some observation differs from scale. Strict concavity gives uniqueness. At the exceptional sample the objective is log power plus a constant. The reciprocal and log-gamma proofs transport maxima by the existing transfer lemmas.

On the workstation with Lean 4.36.0, the final direct check exited 0. The subsequent Lake build exited 0 with `Build completed successfully (7454 jobs).` The new module built without diagnostics. Lake replayed pre-existing diagnostics from other modules. Both commands ran in the foreground under the single-slot reservation from `dev/lean`.

```sh
LEAN_NUM_THREADS=1 /tmp/umvl-cpu/run nice -n 10 sh -c '
  /home/jonas/.elan/bin/lake env lean UnivariateML/Family/TransformedGamma.lean &&
  /home/jonas/.elan/bin/lake build UnivariateML +UnivariateML.Family.TransformedGamma
'
```

The explicit module target checks the new file before the coordinator adds the root import. The proofs contain no placeholders or added axioms. Mathlib artifacts were reused.

## Effort by target

Approximate hands-on minutes, excluding waits for shared CPU slots and build execution.

| Target | Effort | Work |
| --- | ---: | --- |
| 1 | 4 min | Rate bijection, explicit scale, two single-shape branches, and concentration paths. |
| 2 | 1 min | Powered sample likelihood identity and held-rate Gamma transfer. |
| 3 | 6 min | Strict concavity, compact superlevel sets, exact endpoint exception, and exact log-power counterexample. |
| 4 | 3 min | Reciprocal likelihood identity, parameter maps, and transfer of the finite fits and divergence paths. |
| 5 | 2 min | Gamma transfer for the joint and held fits, the fixed-ratio path, and exclusion of constants. |
| 6 | 2 min | Compared scope, coordinates, and right-tail claims against the dossiers, fitter source, and prior mathematics report. |

## Open questions outside the held-coordinate targets

The two-free-shape and all-free global comparisons remain at the scope of the November 25 report. General uniqueness there still requires excluding equal global values at distinct interior stationary points or constructing and certifying an exact tie. The same report records a reciprocal held-endpoint diagnostic trace discrepancy and a binary64 tiny-power scale restoration discrepancy from source replay. This job checks exact likelihood claims. [November 25 mathematics report](2026-11-25-trgamma-math-report.md)

## Theorem statements and readings

Statements are copied from the module, with `n : ℕ` in scope.

### `UnivariateML.TransformedGamma`

The powered observations have the Gamma objective at the transformed rate, plus the power-map Jacobian.

```lean
theorem logLik_power (hn : 0 < n) (x : Fin n → ℝ) (k p a : ℝ) :
    logLik x k p a =
      Gamma.logLik (powerSample x p) k (powerRate p a) +
        (log p + (p - 1) * Gamma.avgLog x)
```

The explicit scale is positive and strictly beats every other feasible scale.

```lean
theorem heldShapes_scale (hn : 0 < n) (x : Fin n → ℝ) {k p : ℝ}
    (hk : 0 < k) (hp : 0 < p) :
    0 < scale x k p ∧ ∀ a > 0, a ≠ scale x k p →
      logLik x k p a < logLik x k p (scale x k p)
```

Holding both shapes gives one scale even for a singleton or a constant sample.

```lean
theorem heldShapes (hn : 0 < n) {x : Fin n → ℝ} (_hx : ∀ i, 0 < x i)
    {k p : ℝ} (hk : 0 < k) (hp : 0 < p) :
    ∃! a, a ∈ Ioi (0 : ℝ) ∧ IsMaxOn (logLik x k p) (Ioi 0) a
```

Holding scale and power gives one Gamma shape on all positive samples.

```lean
theorem heldScalePower (hn : 0 < n) {x : Fin n → ℝ} (_hx : ∀ i, 0 < x i)
    {a p : ℝ} (_ha : 0 < a) (_hp : 0 < p) :
    ∃! k, k ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun t ↦ logLik x t p a) (Ioi 0) k
```

Holding power gives one joint fit in shape–scale order on nonconstant data.

```lean
theorem heldPower (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) {p : ℝ} (hp : 0 < p) :
    ∃! q : ℝ × ℝ, q ∈ Gamma.domain ∧
      IsMaxOn (fun r ↦ logLik x r.1 p r.2) Gamma.domain q
```

For a constant sample, the optimal scale at each shape follows the Gamma fixed-mean path and the likelihood diverges.

```lean
theorem heldPower_constant_unbounded (hn : 0 < n) (c : ℝ) {p : ℝ} (hp : 0 < p) :
    Tendsto (fun k ↦ logLik (fun _ : Fin n ↦ c) k p
      (scale (fun _ : Fin n ↦ c) k p)) atTop atTop
```

Holding Gamma shape gives one joint fit in power–scale order on nonconstant data.

```lean
theorem heldShape (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) {k : ℝ} (hk : 0 < k) :
    ∃! q : ℝ × ℝ, q ∈ Gamma.domain ∧
      IsMaxOn (fun r ↦ logLik x k r.1 r.2) Gamma.domain q
```

For a constant sample, fixing scale at the common value while power grows gives divergence.

```lean
theorem heldShape_constant_unbounded (hn : 0 < n) (c k : ℝ) :
    Tendsto (fun p ↦ logLik (fun _ : Fin n ↦ c) k p c) atTop atTop
```

At held scale and Gamma shape the objective is strictly concave on positive powers.

```lean
theorem heldScaleShape_strictConcave (x : Fin n → ℝ) (k a : ℝ) :
    StrictConcaveOn ℝ (Ioi 0) (fun p ↦ logLik x k p a)
```

Any observation off the held scale ensures a unique finite power.

```lean
theorem heldScaleShape (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {k a : ℝ} (hk : 0 < k) (ha : 0 < a) (hne : ∃ i, x i ≠ a) :
    ∃! p, p ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun t ↦ logLik x k t a) (Ioi 0) p
```

If all observations equal scale, the objective diverges with power.

```lean
theorem heldScaleShape_unbounded (hn : 0 < n) {x : Fin n → ℝ} {k a : ℝ}
    (he : ∀ i, x i = a) :
    Tendsto (fun p ↦ logLik x k p a) atTop atTop
```

An attained finite power exists exactly when some observation differs from scale.

```lean
theorem heldScaleShape_exists_iff (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {k a : ℝ} (hk : 0 < k) (ha : 0 < a) :
    (∃ p ∈ Ioi (0 : ℝ), IsMaxOn (fun t ↦ logLik x k t a) (Ioi 0) p) ↔
      ∃ i, x i ≠ a
```

This positive singleton away from its held scale violates the concavity inequality in log-power.

```lean
theorem logPower_not_concave :
    ¬ ConcaveOn ℝ univ
      (fun t ↦ logLik (fun _ : Fin 1 ↦ 2) (20 / log 2) (exp t) 1)
```

### `UnivariateML.InverseTransformedGamma`

Reciprocating observations and scale changes the objective only by the reciprocal-map Jacobian.

```lean
theorem logLik_reciprocal (x : Fin n → ℝ) (k p a : ℝ) :
    logLik x k p a =
      TransformedGamma.logLik (reciprocal x) k p a⁻¹ + (-2 * Gamma.avgLog x)
```

Both held shapes give one reciprocal-family scale, including on constant samples.

```lean
theorem heldShapes (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {k p : ℝ} (hk : 0 < k) (hp : 0 < p) :
    ∃! a, a ∈ Ioi (0 : ℝ) ∧ IsMaxOn (logLik x k p) (Ioi 0) a
```

Held power gives one joint fit in shape–scale order on nonconstant data.

```lean
theorem heldPower (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) {p : ℝ} (hp : 0 < p) :
    ∃! q : ℝ × ℝ, q ∈ Gamma.domain ∧
      IsMaxOn (fun r ↦ logLik x r.1 p r.2) Gamma.domain q
```

Held Gamma shape gives one joint fit in power–scale order on nonconstant data.

```lean
theorem heldShape (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) {k : ℝ} (hk : 0 < k) :
    ∃! q : ℝ × ℝ, q ∈ Gamma.domain ∧
      IsMaxOn (fun r ↦ logLik x k r.1 r.2) Gamma.domain q
```

The reciprocal of the original fixed-mean scale path gives divergence on constants.

```lean
theorem heldPower_constant_unbounded (hn : 0 < n) (c : ℝ) {p : ℝ} (hp : 0 < p) :
    Tendsto (fun k ↦ logLik (fun _ : Fin n ↦ c) k p
      (TransformedGamma.scale (fun _ : Fin n ↦ c⁻¹) k p)⁻¹) atTop atTop
```

At a constant sample, scale can remain at the common value while power grows without bound.

```lean
theorem heldShape_constant_unbounded (hn : 0 < n) (c k : ℝ) :
    Tendsto (fun p ↦ logLik (fun _ : Fin n ↦ c) k p c) atTop atTop
```

Holding scale and power gives one reciprocal-family Gamma shape, including on constants.

```lean
theorem heldScalePower (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {a p : ℝ} (ha : 0 < a) (hp : 0 < p) :
    ∃! k, k ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun t ↦ logLik x t p a) (Ioi 0) k
```

The reciprocal held-scale, held-shape objective is strictly concave in positive power.

```lean
theorem heldScaleShape_strictConcave (x : Fin n → ℝ) (k a : ℝ) :
    StrictConcaveOn ℝ (Ioi 0) (fun p ↦ logLik x k p a)
```

Any observation off the held scale ensures one finite reciprocal-family power.

```lean
theorem heldScaleShape (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {k a : ℝ} (hk : 0 < k) (ha : 0 < a) (hne : ∃ i, x i ≠ a) :
    ∃! p, p ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun t ↦ logLik x k t a) (Ioi 0) p
```

At a sample equal to scale, the reciprocal-family objective diverges with power.

```lean
theorem heldScaleShape_unbounded (hn : 0 < n) {x : Fin n → ℝ} {k a : ℝ}
    (he : ∀ i, x i = a) :
    Tendsto (fun p ↦ logLik x k p a) atTop atTop
```

The reciprocal family has the same endpoint-only exclusion for the finite held-scale, held-shape fit.

```lean
theorem heldScaleShape_exists_iff (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {k a : ℝ} (hk : 0 < k) (ha : 0 < a) :
    (∃ p ∈ Ioi (0 : ℝ), IsMaxOn (fun t ↦ logLik x k t a) (Ioi 0) p) ↔
      ∃ i, x i ≠ a
```

### `UnivariateML.LogGamma`

The Gamma objective on the logarithms differs only by the log transformation’s Jacobian.

```lean
theorem logLik_transfer (hn : 0 < n) (x : Fin n → ℝ) (k β : ℝ) :
    logLik x k β = Gamma.logLik (fun i ↦ log (x i)) k β + (-Gamma.avgLog x)
```

Holding the rate gives one shape on every supported sample, including constants.

```lean
theorem heldRate (hn : 0 < n) {x : Fin n → ℝ} (_hx : ∀ i, 1 < x i)
    {β : ℝ} (hβ : 0 < β) :
    ∃! k, k ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun t ↦ logLik x t β) (Ioi 0) k
```

Holding the shape gives one rate on every supported sample, including constants.

```lean
theorem heldShape (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 1 < x i)
    {k : ℝ} (hk : 0 < k) :
    ∃! β, β ∈ Ioi (0 : ℝ) ∧ IsMaxOn (logLik x k) (Ioi 0) β
```

A nonconstant sample above one gives one joint fit in shape–rate order.

```lean
theorem jointMLE (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 1 < x i)
    (hne : ∃ i j, x i ≠ x j) :
    ∃! q : ℝ × ℝ, q ∈ Gamma.domain ∧
      IsMaxOn (fun r ↦ logLik x r.1 r.2) Gamma.domain q
```

At a common observation above one, the dossier path with rate equal to shape divided by the log observation gives divergence.

```lean
theorem constant_unbounded (hn : 0 < n) {c : ℝ} (hc : 1 < c) :
    Tendsto (fun k ↦ logLik (fun _ : Fin n ↦ c) k (k / log c)) atTop atTop
```

For observations above one, a finite joint maximum exists exactly in the nonconstant case.

```lean
theorem joint_exists_iff (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 1 < x i) :
    (∃ q ∈ Gamma.domain, IsMaxOn (fun r ↦ logLik x r.1 r.2) Gamma.domain q) ↔
      ∃ i j, x i ≠ x j
```
