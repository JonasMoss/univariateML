# TASK-395 · Burr XII all-free likelihood and transfers

## Results

1. **Competitive escapes.** For strictly positive, nonconstant observations, the first shape partitions the bound. A sufficiently small shape gives a strict Pareto upper bound regardless of power and scale. At first shape `k ≥ 2n`, the conditional intercept score bounds each shifted exponential by `2n`. The softplus error then bounds every likelihood at that shape by the optimal Weibull value plus `2n²/k` in mean units. If the first shape stays in a positive compact interval, a two-observation spread bound contains every finite superlevel in a compact feasible set. Together these statements cover any escaping net, including simultaneous changes in all coefficients. Both optimized values are approached by feasible paths. [Closure report](2026-11-03-burr-closure-report.md) · [Burr math report](2026-11-25-burr-math-report.md)
2. **Attainment and equality.** A finite Burr maximum exists iff a finite point reaches or exceeds the larger optimized face value. If a witness at equality is already a global maximum, it attains the result. Otherwise a better finite point defines a strict superlevel and compact containment yields a global maximum. The converse follows from the two feasible limit paths. With neither a tie nor an improvement at any finite point, the better face gives the unattained supremum. [Burr equality analysis](2026-11-25-burr2-math-report.md#a-finite-maximum-equal-to-a-winning-weibull-face)
3. **Transfers.** The physical inverse Burr and held-location Pareto IV theorems carry both the attainment criterion and every compact superlevel bound above the larger face. The comparator uses log reciprocals relative to the original sample maximum for inverse Burr and log excesses relative to the smallest positive excess for Pareto IV. The statements include the exact conversion from mean log-data likelihood to total physical likelihood. The tied-shape restriction `k = c` keeps every finite superlevel compact for nonconstant data, since a small tied shape suppresses the log prefactor and a large tied shape is penalized by the sample spread. Thus both paralogistic families attain a finite maximum for every nonconstant positive sample. [Transfer report](2026-11-25-transfer-math-report.md) · [Pareto IV report](2026-11-25-pareto34-math-report.md)
4. **Constants and two levels.** Burr, paralogistic and inverse paralogistic have unbounded likelihood on any nonempty positive constant sample. Existing declarations supply the inverse Burr and held-location Pareto IV constant cases. For any nonconstant sample with at least half its observations at the minimum, the Pareto supremum is exact and strictly unattained. The proof uses a symmetric softplus chord bound, the covariance inequality for oppositely ordered finite sums, and Jensen's inequality. This covers two observations with distinct values and the corresponding two-level samples with the stated multiplicity. The result transfers to inverse Burr with a majority at the maximum and to Pareto IV with location held. For the report's two-level paralogistic sample with one observation at 1 and 9,999 at the higher value, the proof constructs a crossing of two compact-region heights with global maximizers on opposite sides of shape `1/10000`. It uses analytic witness comparisons at both endpoints and strict loss at the region seam. The inverse sample has corresponding distinct maximizers. [Majority theorem in the closure report](2026-11-03-burr-closure-report.md#6-a-global-minimum-tie-nonexistence-theorem) · [Paralogistic tie proof](2026-11-25-paralogis-math-report.md#all-free-fit)

## Source deviations and open entries

- **Escape labels.** The task brief includes log-logistic and Lomax among the all-free escape names. The Burr XII dossier lists Weibull and Pareto for nonconstant all-free samples. Log-logistic occurs at the interior first shape `k = 1`. Lomax occurs at the interior power `c = 1` and is the powered-data family for the held-power analysis. The optimized Pareto trace here uses an endpoint approaching the sample minimum from below, which gives full endpoint density. Holding scale exactly at the minimum instead incurs the separate endpoint penalty. [Burr dossier](../../../univariateML-cpp/include/cppunivariate/distributions/burr/README.md) · [Pareto density trace](2026-11-03-burr-closure-report.md#the-pareto-density-trace)
- **Escape scope.** The Lean statement bounds sample likelihood along every escape. Classifying weak limits of probability laws, including defective laws, and computing every individual Pareto endpoint density trace remain outside this result.
- **Paralogistic tie cardinality and crossing uniqueness remain open in Lean.** The construction proves at least two finite global maximizers for some exact `K ∈ (7/5, 8/5)` in the source sample family. The report's narrower-band strict curvature bounds and the signed derivative comparison of band heights still need formalization to certify exactly two and a unique crossing. Both inverse conclusions carry the same scope. [Band curvature and crossing](2026-11-25-paralogis-math-report.md#strict-curvature-and-the-tie)
- **Exact Burr tie sample construction remains open in Lean.** The equality rule covers every finite witness, including a tie with one or both faces. The specific examples with two finite Burr co-winners above both faces or with a finite winner tied to Weibull require the regional interval covers, signed tail exclusion and height crossings from the source. The exact-real criterion and transfers here are independent of those numerical enclosures. [Burr global ties](2026-11-25-burr2-math-report.md)
- **Constant case reuse.** The inverse Burr and held-location Pareto IV constant branches already have complete Lean declarations. [Inverse Burr constant-sample theorem](../../lean/UnivariateML/Family/ShapeFacesInverseBurr.lean#L385) · [Pareto IV constant-sample theorem](../../lean/UnivariateML/Family/ShapeFacesParetoIV.lean#L217)

## Verification and effort

The final direct check of `UnivariateML/Family/BurrFree.lean` passed on the workstation. The final `lake build` completed successfully with 7,872 jobs and exit status 0. The new module built without warnings in that run. Existing modules emitted linter warnings. The temporary `public import UnivariateML.Family.BurrFree` was removed afterward and `git diff --exit-code -- dev/lean/UnivariateML.lean` passed. The Lean commands ran through the one-slot runner with `nice -n 10`. The direct file checks used existing Mathlib artifacts and built the needed repository-module dependencies locally.

| Target | Approximate hands-on effort | Main proof work |
|---|---:|---|
| 1. Competitive escapes | 9 min | Pareto normalization and path, Weibull profiling and large-shape error, compact box for bounded first shape |
| 2. Attainment | 3 min | Reachable face lower bounds, strict superlevel maximum and finite equality |
| 3. Transfers | 8 min | Physical coefficient charts, exact Jacobians, compact containment on the tied-shape restriction |
| 4. Constant and two-level samples | 18 min | Constant paths, majority bound and transfers, exact two-level height crossing |
| Final integration | 4 min | Direct check, whole-library builds, import removal and atlas registration |

[Lean module](../../lean/UnivariateML/Family/BurrFree.lean) · [Atlas rows](../existence-atlas/lean-map.d/lean-burrfree.csv)

## Lean declarations

The declarations below are in `UnivariateML.BurrFree`, with `{n : ℕ}` and the module's namespace openings. In the log-data chart, a parameter triple has order first shape, power, intercept. In the physical chart its last entry is scale.

### `UnivariateML.BurrFree.pareto_optimized`

The Pareto exponent is the reciprocal mean log excess, with optimized mean log likelihood minus the log mean excess minus one.

```lean
theorem pareto_optimized {y : Fin n → ℝ} (hy : 0 < BurrHeld.avg y) {a : ℝ}
    (ha : 0 < a) :
    log a - a * BurrHeld.avg y ≤ paretoValue y ∧
      log (BurrHeld.avg y)⁻¹ - (BurrHeld.avg y)⁻¹ * BurrHeld.avg y = paretoValue y
```

### `UnivariateML.BurrFree.small_shape_lt_pareto`

When n k ≤ k + 1, every Burr point at shape k is strictly below the optimized Pareto value.

```lean
theorem small_shape_lt_pareto (hn : 0 < n) {y : Fin n → ℝ}
    (hy : 0 < BurrHeld.avg y) {j : Fin n} (hj : y j = 0)
    {k c b : ℝ} (hk : 0 < k) (hc : 0 < c) (hsmall : (n : ℝ) * k ≤ k + 1) :
    likelihood y (k, c, b) < paretoValue y
```

### `UnivariateML.BurrFree.pareto_path`

Increasing power, decreasing first shape and moving the endpoint upwards from below reaches the full Pareto likelihood trace.

```lean
theorem pareto_path (hn : 0 < n) {y : Fin n → ℝ}
    (hy : ∀ i, 0 ≤ y i) (hmean : 0 < BurrHeld.avg y) :
    Tendsto (fun t : ℝ ↦ likelihood y ((avg y)⁻¹ / t^2, t^2, t))
      atTop (𝓝 (paretoValue y))
```

### `UnivariateML.BurrFree.weibull_path`

With power and adjusted intercept fixed, the Burr likelihood converges to the Weibull likelihood as the first shape diverges.

```lean
theorem weibull_path (hn : 0 < n) (y : Fin n → ℝ) (c b : ℝ) :
    Tendsto (fun k : ℝ ↦ likelihood y (k, c, b - log k)) atTop (𝓝 (weibullLik y c b))
```

### `UnivariateML.BurrFree.weibull_profile_value`

At fixed Weibull power the intercept is minus the log mean powered observation, yielding the closed-form profile.

```lean
theorem weibull_profile_value (hn : 0 < n) (y : Fin n → ℝ) (c : ℝ) :
    let A := BurrHeld.avg (fun i ↦ exp (c*y i))
    let b := -log A
    weibullLik y c b = log c+c*BurrHeld.avg y-log A-1 ∧
      ∀ a, weibullLik y c a ≤ weibullLik y c b
```

### `UnivariateML.BurrFree.weibull_optimized`

A positive finite power and finite intercept achieve the largest Weibull value for nonconstant data.

```lean
theorem weibull_optimized (hn : 0 < n) {y : Fin n → ℝ}
    (hne : ∃ i j, y i ≠ y j) :
    ∃ c > 0, ∃ b, ∀ d > 0, ∀ a, weibullLik y d a ≤ weibullLik y c b
```

### `UnivariateML.BurrFree.large_shape_le_weibull`

For first shape k ≥ 2n, every Burr likelihood is bounded by the optimal Weibull value plus 2n²/k in mean units.

```lean
theorem large_shape_le_weibull (hn : 0 < n) {y : Fin n → ℝ}
    (hne : ∃ i j, y i ≠ y j) {W : ℝ}
    (hW : ∀ c > 0, ∀ b, weibullLik y c b ≤ W)
    {k c b : ℝ} (hk : 0 < k) (hc : 0 < c) (hlarge : 2 * (n : ℝ) ≤ k) :
    likelihood y (k,c,b) ≤ W + 2 * (n : ℝ)^2/k
```

### `UnivariateML.BurrFree.bounded_shape_escape`

If the first shape stays in a positive compact interval, every escaping net has likelihood tending to minus infinity.

```lean
theorem bounded_shape_escape (hn : 0 < n) {y : Fin n → ℝ}
    (hne : ∃ i j, y i ≠ y j) {a b : ℝ} (ha : 0 < a) (hab : a ≤ b)
    {ι : Type*} {l : Filter ι} {p : ι → ℝ × ℝ × ℝ}
    (hp : ∀ᶠ t in l, p t ∈ domain ∧ (p t).1 ∈ Icc a b)
    (hescape : ∀ K : Set (ℝ × ℝ × ℝ), IsCompact K → K ⊆ domain →
      ∀ᶠ t in l, p t ∉ K) :
    Tendsto (fun t ↦ likelihood y (p t)) l atBot
```

### `UnivariateML.BurrFree.competitive_superlevel_compact`

Every likelihood superlevel strictly above both optimized faces lies in a compact subset of the finite domain.

```lean
theorem competitive_superlevel_compact (hn : 0 < n) {y : Fin n → ℝ}
    (hne : ∃ i j, y i ≠ y j) (hmean : 0 < BurrHeld.avg y) {j : Fin n} (hj : y j = 0)
    {W M : ℝ} (hW : ∀ c > 0, ∀ b, weibullLik y c b ≤ W)
    (hP : paretoValue y < M) (hWM : W < M) :
    ∃ K : Set (ℝ × ℝ × ℝ), IsCompact K ∧ K ⊆ domain ∧
      ∀ p ∈ domain, M ≤ likelihood y p → p ∈ K
```

### `UnivariateML.BurrFree.escape_eventually_lt`

Every escaping net is eventually below each number strictly greater than the better optimized face.

```lean
theorem escape_eventually_lt (hn : 0 < n) {y : Fin n → ℝ}
    (hne : ∃ i j, y i ≠ y j) (hmean : 0 < BurrHeld.avg y) {j : Fin n} (hj : y j = 0)
    {W : ℝ} (hW : ∀ c > 0, ∀ b, weibullLik y c b ≤ W)
    {ι : Type*} {l : Filter ι} {p : ι → ℝ × ℝ × ℝ}
    (hp : ∀ᶠ t in l, p t ∈ domain)
    (hescape : ∀ K : Set (ℝ × ℝ × ℝ), IsCompact K → K ⊆ domain →
      ∀ᶠ t in l, p t ∉ K) {M : ℝ} (hM : max (paretoValue y) W < M) :
    ∀ᶠ t in l, likelihood y (p t) < M
```

### `UnivariateML.BurrFree.jointMLE_iff`

On nonconstant log data, a finite point tying or beating the better optimized face is equivalent to finite attainment.

```lean
theorem jointMLE_iff (hn : 0 < n) {y : Fin n → ℝ}
    (hy : ∀ i, 0 ≤ y i) (hne : ∃ i j, y i ≠ y j)
    (hmean : 0 < BurrHeld.avg y) {j : Fin n} (hj : y j = 0)
    {c b : ℝ} (hc : 0 < c) (hW : ∀ d > 0, ∀ a, weibullLik y d a ≤ weibullLik y c b) :
    (∃ p ∈ domain, IsMaxOn (likelihood y) domain p) ↔
      ∃ p ∈ domain, max (paretoValue y) (weibullLik y c b) ≤ likelihood y p
```

### `UnivariateML.BurrFree.diagonal_superlevel_compact`

For tied shapes and nonconstant data, every finite likelihood superlevel is contained in a compact feasible set.

```lean
theorem diagonal_superlevel_compact (hn : 0 < n) {y : Fin n → ℝ}
    (hne : ∃ i j, y i ≠ y j) (M : ℝ) :
    ∃ K : Set (ℝ × ℝ), IsCompact K ∧ K ⊆ BurrHeld.firstDomain ∧
      ∀ p ∈ BurrHeld.firstDomain, M ≤ likelihood y (p.1,p.1,p.2) → p ∈ K
```

### `UnivariateML.BurrFree.diagonal_MLE`

The tied-shape log-data model has a finite maximizer on every nonconstant sample.

```lean
theorem diagonal_MLE (hn : 0 < n) {y : Fin n → ℝ}
    (hne : ∃ i j, y i ≠ y j) :
    ∃ p ∈ BurrHeld.firstDomain,
      IsMaxOn (fun q ↦ likelihood y (q.1,q.1,q.2)) BurrHeld.firstDomain p
```

### `UnivariateML.BurrFree.physical_criterion_and_escape`

For nonconstant positive observations the finite-witness criterion and the compact escape bound apply to the physical Burr coefficients.

```lean
theorem physical_criterion_and_escape (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) (hne : ∃ i j, x i ≠ x j)
    {j : Fin n} (hj : ∀ i, x j ≤ x i) :
    let y := logExcess x j
    let f := fun p : ℝ × ℝ × ℝ ↦ BurrHeld.logLik x p.1 p.2.1 p.2.2
    let J := BurrHeld.avg (fun i ↦ log (x i))
    ∃ c > 0, ∃ b, (∀ d > 0, ∀ t, weibullLik y d t ≤ weibullLik y c b) ∧
    let B := max (paretoValue y) (weibullLik y c b)
    ((∃ p ∈ physicalDomain, IsMaxOn f physicalDomain p) ↔
      ∃ p ∈ physicalDomain, B-J ≤ f p) ∧
    ∀ M > B, ∃ K : Set (ℝ × ℝ × ℝ), IsCompact K ∧ K ⊆ physicalDomain ∧
      ∀ p ∈ physicalDomain, M-J ≤ f p → p ∈ K
```

### `UnivariateML.BurrFree.paretoIV_criterion_and_escape`

With threshold held, the criterion and escape bound use positive excesses and both translated faces, with the log-excess Jacobian.

```lean
theorem paretoIV_criterion_and_escape (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ}
    (hx : ∀ i, μ < x i) (hne : ∃ i j, x i ≠ x j)
    {j : Fin n} (hj : ∀ i, x j ≤ x i) :
    let e := ShapeFacesParetoIV.excess x μ
    let y := logExcess e j
    let f := fun p : ℝ × ℝ × ℝ ↦ ShapeFacesParetoIV.logLik x μ p.1 p.2.1 p.2.2
    let J := ∑ i, log (e i)
    ∃ c > 0, ∃ b, (∀ d > 0, ∀ t, weibullLik y d t ≤ weibullLik y c b) ∧
    let B := max (paretoValue y) (weibullLik y c b)
    ((∃ p ∈ physicalDomain, IsMaxOn f physicalDomain p) ↔
      ∃ p ∈ physicalDomain, n*B-J ≤ f p) ∧
    ∀ M > B, ∃ K : Set (ℝ × ℝ × ℝ), IsCompact K ∧ K ⊆ physicalDomain ∧
      ∀ p ∈ physicalDomain, n*M-J ≤ f p → p ∈ K
```

### `UnivariateML.BurrFree.inverse_criterion_and_escape`

For nonconstant inverse Burr data the criterion and escape bound use reciprocal observations, reciprocal scale and the transformed likelihood units.

```lean
theorem inverse_criterion_and_escape (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) (hne : ∃ i j, x i ≠ x j)
    {j : Fin n} (hj : ∀ i, x i ≤ x j) :
    let y := logExcess (Transfer.reciprocal x) j
    let f := fun p : ℝ × ℝ × ℝ ↦ ShapeFacesInverseBurr.logLik x p.1 p.2.1 p.2.2
    let J := ∑ i, log (x i)
    ∃ c > 0, ∃ b, (∀ d > 0, ∀ t, weibullLik y d t ≤ weibullLik y c b) ∧
    let B := max (paretoValue y) (weibullLik y c b)
    ((∃ p ∈ physicalDomain, IsMaxOn f physicalDomain p) ↔
      ∃ p ∈ physicalDomain, n*B-J ≤ f p) ∧
    ∀ M > B, ∃ K : Set (ℝ × ℝ × ℝ), IsCompact K ∧ K ⊆ physicalDomain ∧
      ∀ p ∈ physicalDomain, n*M-J ≤ f p → p ∈ K
```

### `UnivariateML.BurrFree.paralogistic_MLE_and_escape`

On every nonconstant positive sample, paralogistic has a finite maximizer and compact containment at every finite likelihood level.

```lean
theorem paralogistic_MLE_and_escape (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) (hne : ∃ i j, x i ≠ x j) :
    let f := fun p : ℝ × ℝ ↦ Paralogistic.logLik x p.1 p.2
    (∃ p ∈ Weibull.domain, IsMaxOn f Weibull.domain p) ∧
    ∀ M : ℝ, ∃ K : Set (ℝ × ℝ), IsCompact K ∧ K ⊆ Weibull.domain ∧
      ∀ p ∈ Weibull.domain, M ≤ f p → p ∈ K
```

### `UnivariateML.BurrFree.inverse_paralogistic_MLE_and_escape`

On every nonconstant positive sample, inverse paralogistic has a finite maximizer and compact containment at every finite likelihood level.

```lean
theorem inverse_paralogistic_MLE_and_escape (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) (hne : ∃ i j, x i ≠ x j) :
    let f := fun p : ℝ × ℝ ↦ InvParalogistic.logLik x p.1 p.2
    (∃ p ∈ Weibull.domain, IsMaxOn f Weibull.domain p) ∧
    ∀ M : ℝ, ∃ K : Set (ℝ × ℝ), IsCompact K ∧ K ⊆ Weibull.domain ∧
      ∀ p ∈ Weibull.domain, M ≤ f p → p ∈ K
```

### `UnivariateML.BurrFree.constant_unbounded`

A nonempty positive constant Burr sample has unbounded likelihood.

```lean
theorem constant_unbounded (hn : 0 < n) {v : ℝ} (hv : 0 < v) :
    ¬ BddAbove ((fun p : ℝ × ℝ × ℝ ↦
      BurrHeld.logLik (fun _ : Fin n ↦ v) p.1 p.2.1 p.2.2) '' physicalDomain)
```

### `UnivariateML.BurrFree.paralogistic_constant_unbounded`

A nonempty positive constant paralogistic sample has unbounded likelihood.

```lean
theorem paralogistic_constant_unbounded (hn : 0 < n) {v : ℝ} (hv : 0 < v) :
    ¬ BddAbove ((fun p : ℝ × ℝ ↦ Paralogistic.logLik (fun _ : Fin n ↦ v) p.1 p.2) ''
      Weibull.domain)
```

### `UnivariateML.BurrFree.inverse_paralogistic_constant_unbounded`

A nonempty positive constant inverse paralogistic sample has unbounded likelihood.

```lean
theorem inverse_paralogistic_constant_unbounded (hn : 0 < n) {v : ℝ} (hv : 0 < v) :
    ¬ BddAbove ((fun p : ℝ × ℝ ↦ InvParalogistic.logLik (fun _ : Fin n ↦ v) p.1 p.2) ''
      Weibull.domain)
```

### `UnivariateML.BurrFree.majority_minimum_lt_pareto`

At least half the sample at the minimum strictly excludes every finite Burr point relative to the optimized Pareto value.

```lean
theorem majority_minimum_lt_pareto (hn : 0 < n) {y : Fin n → ℝ}
    (hy : ∀ i, 0 ≤ y i) (hmean : 0 < BurrHeld.avg y)
    (hw : (n : ℝ) / 2 ≤ ((Finset.univ.filter (fun i ↦ y i = 0)).card : ℝ))
    {p : ℝ × ℝ × ℝ} (hp : p ∈ domain) :
    likelihood y p < paretoValue y
```

### `UnivariateML.BurrFree.two_level_lt_pareto`

For two distinct log levels, a majority at the lower level, including equality, strictly excludes all finite Burr points.

```lean
theorem two_level_lt_pareto (hn : 0 < n) {y : Fin n → ℝ} {r : ℝ} (hr : 0 < r)
    (hy : ∀ i, y i = 0 ∨ y i = r) (hmean : 0 < BurrHeld.avg y)
    (hw : (n : ℝ) / 2 ≤ ((Finset.univ.filter (fun i ↦ y i = 0)).card : ℝ))
    {p : ℝ × ℝ × ℝ} (hp : p ∈ domain) :
    likelihood y p < paretoValue y
```

### `UnivariateML.BurrFree.majority_minimum_physical`

At least half the nonconstant sample at the minimum gives the optimized Pareto supremum in physical likelihood units, strictly above every finite point.

```lean
theorem majority_minimum_physical (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) (hne : ∃ i j, x i ≠ x j)
    {j : Fin n} (hj : ∀ i, x j ≤ x i)
    (hm : (n : ℝ) / 2 ≤ ((Finset.univ.filter (fun i ↦ x i = x j)).card : ℝ)) :
    let f := fun p : ℝ × ℝ × ℝ ↦ BurrHeld.logLik x p.1 p.2.1 p.2.2
    let P := paretoValue (logExcess x j)-BurrHeld.avg (fun i ↦ log (x i))
    sSup (f '' physicalDomain) = P ∧ ∀ p ∈ physicalDomain, f p < P
```

### `UnivariateML.BurrFree.paretoIV_majority_minimum`

With threshold held, a majority at the minimum gives the unattained shifted-Pareto supremum.

```lean
theorem paretoIV_majority_minimum (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ}
    (hx : ∀ i, μ < x i) (hne : ∃ i j, x i ≠ x j)
    {j : Fin n} (hj : ∀ i, x j ≤ x i)
    (hm : (n : ℝ) / 2 ≤ ((Finset.univ.filter (fun i ↦ x i = x j)).card : ℝ)) :
    let e := ShapeFacesParetoIV.excess x μ
    let f := fun p : ℝ × ℝ × ℝ ↦ ShapeFacesParetoIV.logLik x μ p.1 p.2.1 p.2.2
    let P := n*paretoValue (logExcess e j)-∑ i, log (e i)
    sSup (f '' physicalDomain) = P ∧ ∀ p ∈ physicalDomain, f p < P
```

### `UnivariateML.BurrFree.inverse_majority_maximum`

A majority at the maximum gives the unattained upper-endpoint power supremum for inverse Burr.

```lean
theorem inverse_majority_maximum (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) (hne : ∃ i j, x i ≠ x j)
    {j : Fin n} (hj : ∀ i, x i ≤ x j)
    (hm : (n : ℝ) / 2 ≤ ((Finset.univ.filter (fun i ↦ x i = x j)).card : ℝ)) :
    let y := logExcess (Transfer.reciprocal x) j
    let f := fun p : ℝ × ℝ × ℝ ↦ ShapeFacesInverseBurr.logLik x p.1 p.2.1 p.2.2
    let P := n*paretoValue y-∑ i, log (x i)
    sSup (f '' physicalDomain) = P ∧ ∀ p ∈ physicalDomain, f p < P
```

### `UnivariateML.BurrFree.paralogistic_two_level_tie`

The stated two-level family includes an exact sample at which global maximizers exist on both sides of shape 1/10000.

```lean
theorem paralogistic_two_level_tie :
    ∃ K ∈ Ioo (7/5 : ℝ) (8/5),
      let x : Fin 10000 → ℝ := fun i ↦ if i = 0 then 1 else exp (100000000*K)
      let f := fun p : ℝ × ℝ ↦ Paralogistic.logLik x p.1 p.2
      ∃ p ∈ Weibull.domain, ∃ q ∈ Weibull.domain,
        p.1 < 1/10000 ∧ 1/10000 < q.1 ∧
        IsMaxOn f Weibull.domain p ∧ IsMaxOn f Weibull.domain q
```

### `UnivariateML.BurrFree.inverse_paralogistic_two_level_tie`

The reciprocal two-level family includes an exact sample at which global maximizers exist on both sides of shape 1/10000.

```lean
theorem inverse_paralogistic_two_level_tie :
    ∃ K ∈ Ioo (7/5 : ℝ) (8/5),
      let x : Fin 10000 → ℝ := fun i ↦ if i = 0 then 1 else exp (-(100000000*K))
      let f := fun p : ℝ × ℝ ↦ InvParalogistic.logLik x p.1 p.2
      ∃ p ∈ Weibull.domain, ∃ q ∈ Weibull.domain,
        p.1 < 1/10000 ∧ 1/10000 < q.1 ∧
        IsMaxOn f Weibull.domain p ∧ IsMaxOn f Weibull.domain q
```

