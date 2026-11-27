# TASK-399 competitive escapes in Lean

The new modules contain 44 checked theorems. The all-free discretized gamma
range criterion is proved, as are the PGW held-scale criterion and the
PGW held-shape criterion using finite stationary witnesses. The Fréchet
collapse classification covers both half and majority minimum multiplicity
on the fitted branch. The sections below identify the remaining global
comparisons. [Atlas fragment](../existence-atlas/lean-map.d/lean-allfree2.csv)

## Source clarifications

The GEV atlas describes the lower endpoint as at the sample minimum on the
shape-one collapse path. At finite feasible parameters the endpoint is
strictly below that minimum. The gap tends to zero along the Lean path.
[GEV dossier](../../../univariateML-cpp/include/cppunivariate/distributions/gev/README.md#details)

The PGW fixed-threshold hazard limit and the endpoint density trace differ in
what they measure. At the threshold the cumulative hazard tends to zero.
Each observation tied there contributes half the density factor obtained
by moving the scale toward the threshold from below at the rate in
`free_threshold_path`. [PGW math report](2026-11-25-pgw-math-report.md)

## Lean checks

The workstation ran `lake env lean` for every added module and then
`lake build` with temporary imports under the local single-slot lock.
The imports were removed and `git diff --exit-code dev/lean/UnivariateML.lean`
passed. No Mathlib rebuild or C++, R, or JS suite was run.

```text
Build completed successfully (7926 jobs).
```

The theorem counts below describe the proof effort per target. The Lean
statements are copied from the checked modules. The independent atlas
coverage column is empty.

## Power generalized Weibull

Threshold domination and the endpoint trace come from the softplus correction. Moving the free scale below the limiting threshold gives the unpenalized path. For a scale above a sample observation, the two-observation log-ratio bound confines the shape and then confines the exponent on each competitive superlevel. The held-shape chart extends to the Gompertz face and proves the stationary-witness criterion with finite equality.

**Open.** The all-free criterion still needs a uniform bound by the optimized threshold envelope at the sample minimum as shape diverges and scale varies. The held-shape proof compares all finite stationary points with the whole Gompertz face. The analytic collar and a finite stationary list, including degenerate roots, remain open in Lean.

**Effort.** 16 public theorems. [Lean module](../../lean/UnivariateML/Family/AllFreePGW.lean)

### `UnivariateML.AllFreePGW.held_scale_threshold_path`

A fixed scale at or below every observation gives the threshold limit. Each observation equal to the scale contributes a factor of one half.

```lean
theorem held_scale_threshold_path {x : Fin n → ℝ} {a k : ℝ} (ha : 0 < a)
    (hk : 0 < k) (hx : ∀ i, a ≤ x i) :
    Tendsto (fun b : ℝ ↦ ShapeFacesPGW.logLik x b a (k / b)) atTop
      (𝓝 (thresholdLogLik x a k))
```

### `UnivariateML.AllFreePGW.held_scale_logDensity_lt_threshold`

At or below the minimum, each finite density loses to the threshold density at the same product of shape and exponent.

```lean
theorem held_scale_logDensity_lt_threshold {a x b k : ℝ} (ha : 0 < a)
    (hx : a ≤ x) (hb : 0 < b) (hk : 0 < k) :
    ShapeFacesPGW.logDensity b a (k / b) x <
      log k - log x - (if x = a then log 2 else 0) +
        k * log (x / a) - (exp (k * log (x / a)) - 1)
```

### `UnivariateML.AllFreePGW.held_scale_no_max_of_le_all`

A positive held scale at or below all observations has no finite maximizer. This includes constant samples and ties at the scale.

```lean
theorem held_scale_no_max_of_le_all (hn : 0 < n) {x : Fin n → ℝ} {a : ℝ}
    (ha : 0 < a) (hx : ∀ i, a ≤ x i) :
    ¬ ∃ p ∈ Weibull.domain,
      IsMaxOn (fun q : ℝ × ℝ ↦ ShapeFacesPGW.logLik x q.1 a q.2) Weibull.domain p
```

### `UnivariateML.AllFreePGW.gompertz_path`

The power-transformed Gompertz face along a path with fixed shape.

```lean
theorem gompertz_path {x : Fin n → ℝ} {b r : ℝ} (hb : 0 < b) (hr : 0 < r)
    (hx : ∀ i, 0 < x i) :
    Tendsto (fun h : ℝ ↦ ShapeFacesPGW.logLik x b ((h * r) ^ (-1 / b)) h⁻¹)
      (𝓝[>] 0) (𝓝 (gompertzLogLik x b r))
```

### `UnivariateML.AllFreePGW.threshold_hazard_path`

The cumulative hazard on the fixed-scale threshold path, on both sides of the threshold and at equality.

```lean
theorem threshold_hazard_path {x a k : ℝ} (hx : 0 < x) (ha : 0 < a) (hk : 0 < k) :
    Tendsto (fun b : ℝ ↦ (1 + (x / a) ^ b) ^ (k / b) - 1) atTop
      (𝓝 (if a ≤ x then (x / a) ^ k - 1 else 0))
```

### `UnivariateML.AllFreePGW.held_scale_constant_below`

Constants strictly below the held scale concentrate by sending the shape to infinity and taking the reciprocal powered ratio as exponent.

```lean
theorem held_scale_constant_below {a x : ℝ} (hn : 0 < n) (hx : 0 < x) (ha : x < a) :
    Tendsto (fun b : ℝ ↦ ShapeFacesPGW.logLik (fun _ : Fin n ↦ x)
      b a ((x / a) ^ b)⁻¹) atTop atTop
```

### `UnivariateML.AllFreePGW.held_shape_narrow_no_max`

At a held positive shape, a powered observation-to-maximum ratio at least one half for every observation excludes every finite maximizer.

```lean
theorem held_shape_narrow_no_max (hn : 0 < n) {x : Fin n → ℝ} {b X : ℝ}
    (hb : 0 < b) (hx : ∀ i, 0 < x i) (hX : ∀ i, x i ≤ X)
    (hnarrow : ∀ i, 1 / 2 ≤ (x i / X) ^ b) :
    ¬ ∃ p ∈ Weibull.domain,
      IsMaxOn (fun q : ℝ × ℝ ↦ ShapeFacesPGW.logLik x b q.1 q.2) Weibull.domain p
```

### `UnivariateML.AllFreePGW.held_scale_attains`

For nonconstant data with a held scale above some observation, joint optimization over the shape and exponent attains a finite maximum.

```lean
theorem held_scale_attains {x : Fin n → ℝ} {a : ℝ} (ha : 0 < a)
    (hx : ∀ i, 0 < x i) {i j : Fin n} (hij : x i < x j) (hia : x i < a) :
    ∃ p ∈ Weibull.domain,
      IsMaxOn (fun q : ℝ × ℝ ↦ ShapeFacesPGW.logLik x q.1 a q.2) Weibull.domain p
```

### `UnivariateML.AllFreePGW.held_scale_exists_iff`

Holding the scale leaves a finite maximizer exactly for nonconstant positive data with an observation strictly below the scale.

```lean
theorem held_scale_exists_iff (hn : 0 < n) {x : Fin n → ℝ} {a : ℝ}
    (ha : 0 < a) (hx : ∀ i, 0 < x i) :
    (∃ p ∈ Weibull.domain,
      IsMaxOn (fun q : ℝ × ℝ ↦ ShapeFacesPGW.logLik x q.1 a q.2) Weibull.domain p) ↔
      (∃ i j, x i ≠ x j) ∧ ∃ i, x i < a
```

### `UnivariateML.AllFreePGW.held_shape_attains_iff_face`

With shape held, a finite fit attains exactly when some finite witness ties or beats every Gompertz likelihood. The witness need not beat the face strictly.

```lean
theorem held_shape_attains_iff_face {x : Fin n → ℝ} (hn : 0 < n)
    (hx : ∀ i, 0 < x i) {b : ℝ} (hb : 0 < b) :
    (∃ p ∈ Weibull.domain,
      IsMaxOn (fun q : ℝ × ℝ ↦ ShapeFacesPGW.logLik x b q.1 q.2) Weibull.domain p) ↔
      ∃ a > 0, ∃ c > 0, ∀ r > 0, gompertzLogLik x b r ≤ ShapeFacesPGW.logLik x b a c
```

### `UnivariateML.AllFreePGW.held_shape_attains_iff_stationary`

The held-shape criterion can be tested at finite stationary points. The nonstrict face comparison preserves finite equality.

```lean
theorem held_shape_attains_iff_stationary {x : Fin n → ℝ} (hn : 0 < n)
    (hx : ∀ i, 0 < x i) {b : ℝ} (hb : 0 < b) :
    (∃ p ∈ Weibull.domain,
      IsMaxOn (fun q : ℝ × ℝ ↦ ShapeFacesPGW.logLik x b q.1 q.2) Weibull.domain p) ↔
      ∃ a > 0, ∃ c > 0,
        deriv (fun s ↦ ShapeFacesPGW.logLik x b s c) a = 0 ∧
        deriv (ShapeFacesPGW.logLik x b a) c = 0 ∧
        ∀ r > 0, gompertzLogLik x b r ≤ ShapeFacesPGW.logLik x b a c
```

### `UnivariateML.AllFreePGW.threshold_unique`

The threshold face has a unique positive exponent whenever at least one observation lies strictly above the held threshold.

```lean
theorem threshold_unique {x : Fin n → ℝ} {a : ℝ} (ha : 0 < a)
    (hx : ∀ i, a ≤ x i) (hne : ∃ i, a < x i) :
    ∃! k, k ∈ Ioi (0 : ℝ) ∧ IsMaxOn (thresholdLogLik x a) (Ioi 0) k
```

### `UnivariateML.AllFreePGW.gompertz_unique`

The power-transformed Gompertz face has a unique optimizing scale at each positive held shape, even on constant data.

```lean
theorem gompertz_unique (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {b : ℝ} :
    ∃! r, r ∈ Ioi (0 : ℝ) ∧ IsMaxOn (gompertzLogLik x b) (Ioi 0) r
```

### `UnivariateML.AllFreePGW.held_scale_threshold_supremum`

The optimized threshold likelihood is the strict all-free-in-shape-and- exponent supremum when the fixed scale is at or below the minimum and some observation is strictly above the scale.

```lean
theorem held_scale_threshold_supremum (hn : 0 < n) {x : Fin n → ℝ} {a : ℝ}
    (ha : 0 < a) (hx : ∀ i, a ≤ x i) (hne : ∃ i, a < x i) :
    ∃ k > 0,
      IsLUB ((fun p : ℝ × ℝ ↦ ShapeFacesPGW.logLik x p.1 a p.2) '' Weibull.domain)
        (thresholdLogLik x a k) ∧
      ∀ p ∈ Weibull.domain, ShapeFacesPGW.logLik x p.1 a p.2 < thresholdLogLik x a k
```

### `UnivariateML.AllFreePGW.gompertz_hazard_path`

The cumulative hazard on the fixed-shape Gompertz path converges at every positive observation.

```lean
theorem gompertz_hazard_path {b r x : ℝ} (hb : 0 < b) (hr : 0 < r) (hx : 0 < x) :
    Tendsto (fun h : ℝ ↦ (1 + (x / (h * r) ^ (-1 / b)) ^ b) ^ h⁻¹ - 1)
      (𝓝[>] 0) (𝓝 (exp (r * x ^ b) - 1))
```

### `UnivariateML.AllFreePGW.free_threshold_path`

With scale free, approaching the threshold from below at log distance `h` while shape grows as `h⁻²` attains the unpenalized endpoint likelihood.

```lean
theorem free_threshold_path {x : Fin n → ℝ} {a k : ℝ} (ha : 0 < a)
    (hk : 0 < k) (hx : ∀ i, a ≤ x i) :
    Tendsto (fun h : ℝ ↦ ShapeFacesPGW.logLik x (h^2)⁻¹ (a * exp (-h)) (k * h^2))
      (𝓝[>] 0) (𝓝 (∑ i, (log k - log (x i) + k * log (x i / a) -
        (exp (k * log (x i / a)) - 1))))
```

## Transformed gamma and inverse transformed gamma

Gamma recurrence and a squeeze on the exponential loss give the power path, including endpoint ties. Stirling's formula on integer shapes and the second-order expansion of the exponential give a lognormal path. Reciprocation transfers both paths with the Jacobian. The positive-gap face rate is optimized by a strict logarithmic inequality.

**Open.** The paths establish reachability. Exhaustive profile bounds, the optimized lognormal comparison in a common chart, the signs near both profile endpoints, and the finite stationary-root criterion remain open in Lean. The lognormal construction establishes reachability along integer gamma shapes.

**Effort.** 5 public theorems. [Lean module](../../lean/UnivariateML/Family/AllFreeTransformedGamma.lean)

### `UnivariateML.AllFreeTransformedGamma.power_path`

The free scale approaches the endpoint while leaving zero exponential loss even for observations tied at the endpoint.

```lean
theorem power_path {x : Fin n → ℝ} {A d : ℝ} (hd : 0 < d) (_hA : 0 < A)
    (hx : ∀ i, 0 < x i ∧ x i ≤ A) :
    Tendsto (fun q : ℝ ↦ TransformedGamma.logLik x (d * q) q⁻¹
      (exp (log A - q * log q))) (𝓝[>] 0)
      (𝓝 (log d - Gamma.avgLog x + Gamma.avg (fun i ↦ d * (log (x i) - log A))))
```

### `UnivariateML.AllFreeTransformedGamma.inverse_power_path`

The reciprocal path gives the Pareto log likelihood, including lower-endpoint ties.

```lean
theorem inverse_power_path {x : Fin n → ℝ} {A d : ℝ} (hd : 0 < d) (hA : 0 < A)
    (hx : ∀ i, A ≤ x i) :
    Tendsto (fun q : ℝ ↦ InverseTransformedGamma.logLik x (d * q) q⁻¹
      (exp (log A⁻¹ - q * log q))⁻¹) (𝓝[>] 0)
      (𝓝 (log d - Gamma.avgLog x + Gamma.avg (fun i ↦ d * (log A - log (x i)))))
```

### `UnivariateML.AllFreeTransformedGamma.lognormal_path`

Integer gamma shapes tending to infinity give a lognormal likelihood path at any fixed log location and positive log scale.

```lean
theorem lognormal_path (hn : 0 < n) (x : Fin n → ℝ) (μ : ℝ) {σ : ℝ} (hσ : 0 < σ) :
    Tendsto (fun j : ℕ ↦ TransformedGamma.logLik x j (1 / (σ * sqrt j))
      (exp (μ - log j * (σ * sqrt j)))) atTop
      (𝓝 (Gamma.avg (fun i ↦ -log σ - log (2 * π) / 2 - log (x i) -
        ((log (x i) - μ) / σ)^2 / 2)))
```

### `UnivariateML.AllFreeTransformedGamma.inverse_lognormal_path`

Reciprocation gives the lognormal likelihood at reflected log location.

```lean
theorem inverse_lognormal_path (hn : 0 < n) (x : Fin n → ℝ) (μ : ℝ)
    {σ : ℝ} (hσ : 0 < σ) :
    Tendsto (fun j : ℕ ↦ InverseTransformedGamma.logLik x j (1 / (σ * sqrt j))
      (exp (-μ - log j * (σ * sqrt j)))⁻¹) atTop
      (𝓝 (Gamma.avg (fun i ↦ -log σ - log (2 * π) / 2 - log (x i) -
        ((log (x i) - μ) / σ)^2 / 2)))
```

### `UnivariateML.AllFreeTransformedGamma.face_rate_optimized`

When the mean gap to the supported endpoint in oriented log units is positive, its reciprocal is the unique rate optimizing the face.

```lean
theorem face_rate_optimized {D : ℝ} (hD : 0 < D) :
    ∃! d, d ∈ Set.Ioi (0 : ℝ) ∧
      IsMaxOn (fun r ↦ log r - r * D) (Set.Ioi 0) d
```

## Generalized gamma

The constant-sample concentration proof covers any fixed real shape, including zero. For either sign the fixed-location path preserves the endpoint trace. The inverse-scale objective gives strict concavity and compact superlevels at every fixed shape when a residual is nonzero. The original two-point module already handles equal-multiplicity two-level data.

**Open.** The general all-free comparison still needs the signed profile at the normal seam and uniform bounds at both ends. The joint held-location comparison still needs uniform joint bounds, including mixed signs and zero residuals. The written held-location report itself leaves that corner classification open.

**Effort.** 3 public theorems. [Lean module](../../lean/UnivariateML/Family/AllFreeGenGamma.lean)

### `UnivariateML.AllFreeGenGamma.constant_concentration`

With location equal to the common log observation, taking scale to zero makes log likelihood unbounded for every fixed shape, including zero.

```lean
theorem constant_concentration {n : ℕ} (hn : 0 < n) (μ Q : ℝ)
    {x : Fin n → ℝ} (hx : ∀ i, x i = exp μ) :
    Tendsto (fun σ : ℝ ↦ GenGammaTwoPoint.sampleLogLik x (μ, σ, Q))
      (𝓝[>] 0) atTop
```

### `UnivariateML.AllFreeGenGamma.held_location_face_path`

For either orientation, the fixed-location path tends to the power or Pareto log likelihood when all observations are on the supported side. At zero residual the exponential loss still vanishes.

```lean
theorem held_location_face_path {n : ℕ} {x : Fin n → ℝ} {μ d : ℝ}
    (hd : 0 < d) (ε : ℝ) (hε : |ε| = 1)
    (hx : ∀ i, ε * (log (x i) - μ) ≤ 0) :
    Tendsto (fun q : ℝ ↦ GenGammaTwoPoint.sampleLogLik x
      (μ, d * sqrt q, ε / sqrt q)) (𝓝[>] 0)
      (𝓝 ((∑ i, (-log d + ε * (log (x i) - μ) / d - log (x i))) / n))
```

### `UnivariateML.AllFreeGenGamma.held_location_fixed_shape_scale`

At fixed location and fixed real shape, any nonzero log residual gives a unique positive scale maximizer, including the normal shape.

```lean
theorem held_location_fixed_shape_scale {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ} (μ Q : ℝ)
    (hne : ∃ i, log (x i) ≠ μ) :
    ∃! σ, σ ∈ Ioi (0 : ℝ) ∧
      IsMaxOn (fun s ↦ GenGammaTwoPoint.sampleLogLik x (μ, s, Q)) (Ioi 0) σ
```

## Beta prime

Convex secants of log Gamma and its recurrence bound the ratio normalizer for either face. The proof gives a path at every fixed positive finite shape and face scale. Reciprocal symmetry transfers the gamma proof to inverse gamma. The fixed-scale beta concentration proof also gives all-free divergence for constants.

**Open.** The all-free ratio profile still needs a uniform concentration bound, continuity of the nuisance optima through the two endpoint ratios, and global comparison with the optimized faces. Exact tied maxima from the source report remain open in Lean.

**Effort.** 4 public theorems. [Lean module](../../lean/UnivariateML/Family/AllFreeBetaPrime.lean)

### `UnivariateML.AllFreeBetaPrime.reciprocal_logLik`

Inversion exchanges the two shapes and inverts the scale.

```lean
theorem reciprocal_logLik {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {b : ℝ} (hb : 0 < b) (p q : ℝ) :
    BetaPrime.logLik x b (p, q) =
      BetaPrime.logLik (fun i ↦ (x i)⁻¹) b⁻¹ (q, p) - ∑ i, 2 * log (x i)
```

### `UnivariateML.AllFreeBetaPrime.constant_unbounded`

Constant data have unbounded beta-prime likelihood even at any fixed positive scale.

```lean
theorem constant_unbounded (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) (he : ∀ i j, x i = x j) :
    ¬ BddAbove {v | ∃ b > 0, ∃ p ∈ Beta.domain, BetaPrime.logLik x b p = v}
```

### `UnivariateML.AllFreeBetaPrime.gamma_path`

At any fixed positive first shape, sending the second shape and scale to infinity at fixed ratio gives the corresponding gamma likelihood.

```lean
theorem gamma_path {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {s p : ℝ} (hs : 0 < s) (hp : 0 < p) :
    Tendsto (fun h : ℝ ↦ BetaPrime.logLik x (s / h) (p, h⁻¹))
      (𝓝[>] 0) (𝓝 (∑ i, Gamma.logDensity p s⁻¹ (x i)))
```

### `UnivariateML.AllFreeBetaPrime.inverse_gamma_path`

At any fixed positive second shape, reciprocal symmetry gives the inverse-gamma likelihood as the first shape diverges and scale decreases.

```lean
theorem inverse_gamma_path {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {s q : ℝ} (hs : 0 < s) (hq : 0 < q) :
    Tendsto (fun h : ℝ ↦ BetaPrime.logLik x (h / s) (h⁻¹, q))
      (𝓝[>] 0)
      (𝓝 ((∑ i, Gamma.logDensity q s⁻¹ (x i)⁻¹) - ∑ i, 2 * log (x i)))
```

## Generalized extreme value

The unrestricted spike follows by fixing shape at minus two and using an attained sample maximum. Shape one gives divergence on strict-majority minima and the finite half-minimum collapse path on exactly half.

**Open.** The fitted-branch sub-half case still needs uniform nuisance bounds across the whole shape interval and upper semicontinuity at the closed reverse-exponential endpoint. The half case still needs the global comparison across negative and zero shapes and the equality characterization at minus one. The collapse path alone cannot decide the balanced exception.

**Effort.** 3 public theorems. [Lean module](../../lean/UnivariateML/Family/AllFreeGEV.lean)

### `UnivariateML.AllFreeGEV.unrestricted_unbounded`

Every nonempty sample has unbounded likelihood if shapes below minus one are allowed. An attained sample maximum may have any multiplicity.

```lean
theorem unrestricted_unbounded (hn : 0 < n) (x : Fin n → ℝ) :
    ¬ BddAbove {v | ∃ ξ : ℝ, ∃ p ∈ GEV.domain x ξ, GEV.logLik x ξ p = v}
```

### `UnivariateML.AllFreeGEV.majority_unbounded`

On the fitted branch, a strict majority at the minimum suffices for unboundedness along shape one. Constants are included.

```lean
theorem majority_unbounded (hn : 0 < n) {x : Fin n → ℝ} {m : ℝ}
    (hm : ∀ i, m ≤ x i) (hz : ∃ i, x i = m)
    (hc : n < 2 * Frechet.minimumCount (GEV.lowerGaps x m)) :
    ¬ BddAbove {v | ∃ ξ ∈ Icc (-1 : ℝ) 1, ∃ p ∈ GEV.domain x ξ, GEV.logLik x ξ p = v}
```

### `UnivariateML.AllFreeGEV.half_minimum_collapse_path`

At equal minimum and nonminimum counts, the shape-one path tends to the stated finite total log likelihood. The lower endpoint stays strictly below the minimum at finite parameters.

```lean
theorem half_minimum_collapse_path (hn : 0 < n) {x : Fin n → ℝ} {m : ℝ}
    (hm : ∀ i, m ≤ x i) (hz : ∃ i, x i = m)
    (hc : n = 2 * Frechet.minimumCount (GEV.lowerGaps x m)) :
    (∀ e > 0, GEV.collapsePath x m 1 e ∈ GEV.domain x 1) ∧
    Tendsto (GEV.collapsePath x m 1) (𝓝[>] 0) (𝓝 (m, 0)) ∧
    Tendsto (fun e ↦ GEV.logLik x 1 (GEV.collapsePath x m 1 e)) (𝓝[>] 0)
      (𝓝 ((n : ℝ) * (log 2 - 1) - 2 * ∑ i, log (x i - m)))
```

## Fréchet

The logarithm difference quotient gives the Gumbel path. The shape-one collapse handles majorities. At exactly half, the strict two-density bound follows from convexity of sinh and comparison with cosh. Summing over all minimum and nonminimum pairs gives the strict bound without assuming the nonminimum observations are distinct.

**Open.** For strict sub-half minima, compact competitive bounds uniformly in reciprocal shape and the finite/Gumbel comparison remain open in Lean. The two-level and multilevel ties and the inward derivative tests also remain open.

**Effort.** 5 public theorems. [Lean module](../../lean/UnivariateML/Family/AllFreeFrechet.lean)

### `UnivariateML.AllFreeFrechet.gumbel_path`

Keep `σ / λ = a` and `μ + σ = b` as shape diverges. The Fréchet log likelihood converges to Gumbel at the same location and scale.

```lean
theorem gumbel_path (x : Fin n → ℝ) (m b : ℝ) {a : ℝ} (ha : 0 < a) :
    Tendsto (fun h : ℝ ↦ Frechet.logLik (GEV.lowerGaps x m) h⁻¹ (m - b + a / h) (a / h))
      (𝓝[>] 0) (𝓝 (GEV.logLik x 0 (b, a)))
```

### `UnivariateML.AllFreeFrechet.unrestricted_unbounded`

Allowing all positive shapes makes the likelihood unbounded whenever the nonnegative gaps include a zero. A small fixed shape already suffices.

```lean
theorem unrestricted_unbounded (hn : 0 < n) {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) (hz : ∃ i, g i = 0) :
    ¬ BddAbove {v | ∃ lam > 0, ∃ p ∈ Frechet.domain, Frechet.logLik g lam p.1 p.2 = v}
```

### `UnivariateML.AllFreeFrechet.pair_logLik_lt`

The product of two Fréchet densities at distance `d` is strictly below the half-minimum collapse bound on the fitted shape branch.

```lean
theorem pair_logLik_lt {a s e d : ℝ} (ha : 1 ≤ a) (hs : 0 < s)
    (he : 0 < e) (hd : 0 < d) :
    Frechet.logDensity a s e + Frechet.logDensity a s (e + d) <
      2 * (log 2 - 1 - log d)
```

### `UnivariateML.AllFreeFrechet.half_minimum_supremum`

At exactly half minimum multiplicity the fitted Fréchet likelihood has a finite strict supremum, approached along shape one.

```lean
theorem half_minimum_supremum {g : Fin n → ℝ} (hn : 0 < n)
    (hg : ∀ i, 0 ≤ g i) (hc : n = 2 * Frechet.minimumCount g) :
    let A := (n : ℝ) * (log 2 - 1) - 2 * ∑ i, log (g i)
    IsLUB {v | ∃ a ≥ 1, ∃ p ∈ Frechet.domain, Frechet.logLik g a p.1 p.2 = v} A ∧
    (∀ a ≥ 1, ∀ p ∈ Frechet.domain, Frechet.logLik g a p.1 p.2 < A) ∧
    Tendsto (Frechet.profile g 1) (𝓝[>] 0) (𝓝 A)
```

### `UnivariateML.AllFreeFrechet.majority_unbounded`

A strict majority of minimum observations causes divergence on the fitted branch, already at Fréchet shape one.

```lean
theorem majority_unbounded {g : Fin n → ℝ} (hn : 0 < n)
    (hg : ∀ i, 0 ≤ g i) (hz : ∃ i, g i = 0)
    (hc : n < 2 * Frechet.minimumCount g) :
    ¬ BddAbove {v | ∃ a ≥ 1, ∃ p ∈ Frechet.domain, Frechet.logLik g a p.1 p.2 = v}
```

## Discretized gamma

The range criterion and the two saturated values are proved. Gamma recurrence supplies the second central moment for the cell-concentration bound. Intermediate values of the CDF select the adjacent-cell rates. A small-shape bound, separation of occupied cells at large shape, and a density envelope for compact shape sets give a compact competitive rectangle. Dominated convergence gives continuity on that rectangle.

**Scope.** The all-free range criterion includes zero counts and ties. The adjacent path uses exact CDF inversion and concentration. Cardinality of finite maximizers and the curvature certificate lie outside this range and collapse result.

**Effort.** 8 public theorems. [Lean module](../../lean/UnivariateML/Family/AllFreeDGamma.lean)

### `UnivariateML.AllFreeDGamma.all_zero_supremum`

With both coefficients free, the all-zero likelihood has supremum one, which no positive finite parameter pair reaches.

```lean
theorem all_zero_supremum {n : ℕ} (hn : 0 < n) :
    IsLUB {v | ∃ a > 0, ∃ r > 0,
      Discrete3DGamma.likelihood (fun _ : Fin n ↦ 0) a r = v} 1 ∧
    ∀ a > 0, ∀ r > 0, Discrete3DGamma.likelihood (fun _ : Fin n ↦ 0) a r < 1
```

### `UnivariateML.AllFreeDGamma.adjacent_mass_lt_one`

Two adjacent cells together have strictly less than unit probability at every finite positive parameter pair, including the cells zero and one.

```lean
theorem adjacent_mass_lt_one {a r : ℝ} (ha : 0 < a) (hr : 0 < r) (j : ℕ) :
    Discrete3DGamma.cellProb a r j + Discrete3DGamma.cellProb a r (j + 1) < 1
```

### `UnivariateML.AllFreeDGamma.cell_concentration`

Keep the latent mean strictly inside a specified cell as shape tends to infinity. The fitted mass of that cell converges to one.

```lean
theorem cell_concentration (j : ℕ) {m : ℝ} (hm : (j : ℝ) < m) (hm' : m < j + 1) :
    Tendsto (fun a : ℝ ↦ Discrete3DGamma.cellProb a (a / m) j) atTop (𝓝 1)
```

### `UnivariateML.AllFreeDGamma.constant_supremum`

The all-free supremum for a constant count is one, including when the common count is positive. No finite shape and rate attain it.

```lean
theorem constant_supremum {n : ℕ} (hn : 0 < n) (j : ℕ) :
    IsLUB {v | ∃ a > 0, ∃ r > 0,
      Discrete3DGamma.likelihood (fun _ : Fin n ↦ j) a r = v} 1 ∧
    ∀ a > 0, ∀ r > 0, Discrete3DGamma.likelihood (fun _ : Fin n ↦ j) a r < 1
```

### `UnivariateML.AllFreeDGamma.adjacent_collapse_path`

For two adjacent cells, a rate chosen to give the prescribed CDF at their shared endpoint realizes arbitrary positive limiting proportions.

```lean
theorem adjacent_collapse_path (j : ℕ) {w : ℝ} (hw : w ∈ Ioo (0 : ℝ) 1) :
    ∃ r : ℝ → ℝ, (∀ᶠ a in atTop, 0 < r a) ∧
      Tendsto (fun a ↦ a / r a) atTop (𝓝 ((j : ℝ) + 1)) ∧
      Tendsto (fun a ↦ Discrete3DGamma.cellProb a (r a) j) atTop (𝓝 w) ∧
      Tendsto (fun a ↦ Discrete3DGamma.cellProb a (r a) (j + 1)) atTop (𝓝 (1 - w))
```

### `UnivariateML.AllFreeDGamma.adjacent_supremum`

The excluded saturated supremum for counts from two adjacent cells is the product of the empirical cell proportions, with their multiplicities.

```lean
theorem adjacent_supremum {n : ℕ} (x : Fin n → ℕ) (j : ℕ)
    (hx : ∀ i, x i = j ∨ x i = j + 1)
    (hl : ∃ i, x i = j) (hu : ∃ i, x i = j + 1) :
    let m := (Finset.univ.filter fun i ↦ x i = j).card
    let k := (Finset.univ.filter fun i ↦ x i ≠ j).card
    let w := (m : ℝ) / (m + k)
    IsLUB {v | ∃ a > 0, ∃ r > 0, Discrete3DGamma.likelihood x a r = v}
      (w ^ m * (1 - w) ^ k) ∧
    ∀ a > 0, ∀ r > 0, Discrete3DGamma.likelihood x a r < w ^ m * (1 - w) ^ k
```

### `UnivariateML.AllFreeDGamma.separated_attains`

Two occupied cells separated by at least one cell force a compact positive-likelihood superlevel, yielding a finite joint maximizer.

```lean
theorem separated_attains {n : ℕ} {x : Fin n → ℕ} {i j : Fin n} (hij : x i + 1 < x j) :
    ∃ a > 0, ∃ r > 0, ∀ b > 0, ∀ t > 0,
      Discrete3DGamma.likelihood x b t ≤ Discrete3DGamma.likelihood x a r
```

### `UnivariateML.AllFreeDGamma.all_free_exists_iff`

A nonempty count sample has a finite all-free fit exactly when its integer range is at least two. Zero counts and multiplicities are retained.

```lean
theorem all_free_exists_iff {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) :
    (∃ a > 0, ∃ r > 0, ∀ b > 0, ∀ t > 0,
      Discrete3DGamma.likelihood x b t ≤ Discrete3DGamma.likelihood x a r) ↔
      ∃ i j, x i + 1 < x j
```
