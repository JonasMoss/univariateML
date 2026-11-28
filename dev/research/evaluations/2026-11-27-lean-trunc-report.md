# TASK-451 · Truncated exponential families and zero modifiers in Lean

The finite-window attainment theorem, the three lower-only count criteria, the zero-modification reduction, the fixed-carrier inflation boundary certificate, and the CMB counterexample are checked in Lean. The family applications and their exact scopes are recorded in the [modifier Lean map](2026-11-27-lean-trunc-data/lean-map.csv). It contains 68 links to reviewed written claims. The `scope` column carries the claim key. Each `coverage` cell is blank.

## Results and effort by target

| Target | Checked result | Proof work |
| --- | --- | --- |
| 1 · Finite windows | Convexity of the finite log partition, strict convexity along any direction that changes retained log-weight ratios, and attainment iff the empirical statistic lies in the relative interior of the conditional convex support. Maximizers differ only by a direction constant on the retained statistics. The scalar case gives unique interior fits between distinct extreme values and the negative-tilt version replaces the upper threshold with the carrier mean. | Reused `ExpFamily.mle_exists_iff` on the finset subtype. Used normalized-weight Jensen inequalities for strictness, the shift identity for flat directions, and the strictly decreasing derivative for the scalar tilt cap. |
| 2 · Lower-only count windows | With the lower atom excluded, Poisson, geometric, and positive held-size NB each fit uniquely iff the count mean exceeds the first retained count. At equality there is no interior maximizer and each retained mass tends to the corresponding mass of the lower atom. | Centered the reciprocal likelihood at the observed mean. Proved strict convexity term by term. A single carrier term controls the left tail at positive excess mean. A high-count term controls the Poisson right tail. Divergent partial sums of the carrier control the geometric and NB right ends. Used Mathlib’s real binomial series for NB and dominated convergence for the atom limits. |
| 3 · Zero modifiers | Product factorization, exact Bernoulli profiling and transfer of the maximizing set, additive-inflation feasibility, the equivalent nonnegative zero tilt below weight one, and a global fixed-carrier boundary certificate. For the reviewed CMB histogram, exact rational comparisons give the unique inflated winner at size three with empirical zero mass one eighth. | Reused the library’s binomial likelihood theorem for Bernoulli endpoints. Proved weight clipping by strict monotonicity on either side of the empirical fraction. Proved the boundary certificate by the exponential supporting inequality and exact sums of mass and first moment. Used a rational first comparison and a size-by-size bound for the CMB tail. |
| 4 · Reviewed family applications | The new declarations cover bounded and lower-only count windows in the scopes below, positive truncated and modified fits for all four requested families, the two free-inflation branches for geometric, the sufficient ordinary Poisson boundary certificate, compatible positive-data attainment for Poisson and held-size NB, and held-size inflated binomial attainment including the exact size-one maximizing curve. | Connected the generic proofs to each family’s carrier and explicit coefficient domain. Used closed tagged atom endpoints for bounded fits, the shifted geometric and positive-Poisson library results, the logarithmic carrier for positive NB at size zero, and compact positive likelihood superlevels or closed coefficient intervals for the inflation attainment proofs. |

The finite existence result works in a finite-dimensional real inner-product space. A positive sample size multiplier and the product of observed carrier weights have no effect on the maximizing set. The statements use per-observation reduced log likelihoods or their positive reciprocal, with the carrier factor suppressed. In the scalar window statements the data enter as the observed mean. For zero modifiers, `z` and `k` are the counts of zeros and positives, and `S` is the sum of the positives. These theorems concern the likelihood and its parameter domain. They make no claim about floating-point solvers or fitter tolerances.

For lower-only windows, the series index starts at the first retained count, and the centered objective uses the excess above that count. Natural tilts range over all reals for Poisson and over negative reals for geometric and NB. For finite windows, `Sum.inl` tags a finite natural tilt, `Sum.inr false` the lower atom, and `Sum.inr true` the upper atom. An extension flag admits an atom where the model declares that extension. The lower-only closed chart uses `none` for the atom. A constant direction shifts the partition by its common projection. The stated likelihood shift uses the same projection at the observed mean.

The exact CMB example uses dispersion one, held probability (317/500), one zero and seven twos. Size two uniquely wins for the positive conditional sample, and its parent zero mass exceeds (1/8). At size three the parent zero mass is below (1/8). The weight (1/8) therefore requires positive inflation. Lean verifies that this fit beats every zero-inflation fit and every other feasible size and weight in the chart. This is the correction to the unrestricted boundary shortcut. The supporting certificate relies on a fixed carrier and a common natural-tilt chart. [Correction and review](../../roadmap/2026-11-11-zero-and-truncation-modifiers.md) · [Exact counterexample source](2026-11-27-zero-mod-math-report.md#cmb-all-free-inflated) · [Independent review](2026-11-27-mod-rev2-report.md#proof-errors-and-scope-corrections)

## Scope and source discrepancies

- The reviewed count-window criteria require an explicit retained-singleton branch. The generic singleton theorem supplies flatness at its retained statistic, and the positive-binomial size-one statements give the flat truncated and modified parent fits. Family-specific compatibility of every singleton hold is outside the formalized scope. The reviewed zero-only correction is represented by the general all-zero union or flat-slice theorems.
- Ordinary positive-size NB and the declared size-zero logarithmic positive component are different slices. The latter is represented by its own carrier in the zero-truncated and zero-modified models. The theorem assumes that declared extension as the size-zero definition. It does not identify it with the ordinary size-zero zero atom, or prove convergence as size tends to zero.
- The bounded geometric and positive-size NB upper failure is at or above the mean of the normalized untilted carrier. This includes equality and reaches the uniform or normalized rising-factorial law. The Lean geometric criterion leaves the finite uniform mean in sum form. The lower-only criteria and atom limits are stated in the open chart, so admitting the atom requires the separate closed-domain branch.
- For inflation, held zero mass and held inflation are different constraints. The clipped Bernoulli theorem solves the first-coordinate-fixed profiling problem for free inflation. The CMB example shows why clipping at a particular parent does not settle competitions across parent designs. For held inflation the Poisson, NB, and binomial attainment theorems use the actual held-inflation likelihood.
- The inflation attainment theorems for Poisson and held NB size prove compactness at positive data. The all-zero free-inflation boundary is handled by the generic all-zero union. With Poisson rate positive and inflation held below one, the reviewed all-zero escape to rate zero needs its own family theorem. A held-inflation ZIP tie cannot be inferred from the free-inflation binomial product curve.

[Truncation and zero-modifier source](2026-11-27-trunc-expfam-math-report.md) · [First review](2026-11-27-mod-rev1-report.md#proof-errors-and-grading-corrections) · [Held-modifier review](2026-11-27-mod-rev3-report.md#proof-errors-and-grading-corrections)

## Open claims and dependencies

| Reviewed scope | What is still needed |
| --- | --- |
| Known bounded geometric and held-size NB windows at the lower mean, and general singleton windows for all four families | Join the strict scalar fit and limiting laws to every allowed endpoint in the physical probability chart. The positive-data bounded criteria already cover means strictly between distinct retained endpoints. The held-size-zero NB known-window chart still needs its separate compatibility analysis. |
| Free-size binomial, known truncation and all three zero modifiers | Formalize the fixed-size profile, the signed tail and finite-prefix comparisons against the matched Poisson or ZIP face, and account for equality and ties across sizes. The fixed-size theorems settle individual slices. They do not provide a uniform cross-size witness or bound. |
| Binomial with probability held, including the zero-modified composition with held zero mass | Establish growth and decay of the size profiles on nonsingleton windows, then include the compatible endpoint and singleton cases. The proofs here concern the different regime of held size. |
| Negative binomial with size free, including held probability, zero weight or inflation | Establish the divided positive normalizer and analytic profile through size zero and inverse size zero, the nonzero first-order Poisson gap, and face comparison or finite candidate enumeration under each hold. The lower-only proof establishes the fixed-size normalizer result and does not compare sizes. |
| Compatible held `p0` for Poisson, geometric, and held-size binomial or NB | Translate each reviewed compatibility statement into a concrete sample predicate and instantiate the positive-constant multiplier lemma. The transfer is proved generically and the relevant conditional fits are proved. The family map records the free-weight declarations. |
| Zero-modified failure rows in the four requested regimes | The positive-sample fits attain, with all-zero flatness and held-weight compatibility available from the generic factorization. A direct family theorem excluding other positive-height limits under each hold would need pointwise boundary control in the physical parameter chart. |
| Geometric with inflation held | Prove strict concavity of the held-inflation scalar log likelihood, its two endpoint limits on positive samples, and the sliced maximizing sets at all zeros. The formalized two-branch proof optimizes inflation freely and cannot be restricted to a held slice. |
| Poisson free-inflation uniqueness on positives, including the conditional branch | The profiled boundary certificate and positive-data compactness are proved. Closing uniqueness everywhere needs strict two-coordinate concavity on the summable Poisson carrier, the likelihood identity for the zero tilt, and the conditional branch or seam argument with that identity. |
| Poisson with inflation held, all-zero escape and the exact mixed-sample tie | Prove the excluded zero-rate supremum as a limit for the all-zero likelihood, and formalize the two stationary branches and their unique height crossing for the tie. The compactness theorem does cover positive-sample attainment. |
| Positive held-size NB free-inflation uniqueness and held-size binomial free-inflation uniqueness for size at least two | Attainment is proved. Prove strict joint concavity throughout the identifiable two-tilt chart and link it to the actual PMF, then compare the fitted interior with the admitted binomial atoms. At binomial size one the distinct product-curve problem is settled. |
| Binomial and NB with both size and inflation held, stationary candidate sets | Attainment is proved on the relevant compatible slices. Exact ranking still requires the derivative polynomial or an analytic superlevel root-finiteness argument. |
| CMB with arbitrary held probability and dispersion | The exact example refutes the shortcut and identifies the unique winner for its histogram. Other CMB rows require the cross-design candidate and face comparisons under their respective holds. |

## Verification

- The three changed files each passed `nice -n 10 /home/jonas/.elan/bin/lake env lean <file>` through the single-slot lock on the workstation.
- The default `lake build` passed on the workstation through the four-slot lock with the three temporary public imports in the library root. It completed **8422 jobs** and reused the existing Mathlib cache. Lake emitted non-fatal lint warnings, including formatting and tactic-style warnings in the new files.
- The temporary root imports were removed and `git diff --exit-code -- dev/lean/UnivariateML.lean` passed in the build command after restoration. The new declarations contain no proof placeholders. No runtime or numerical suites were run.

## Lean theorem statements

The unqualified names in each code block resolve in the namespace shown above it. Statements are copied from the checked source files.

### `UnivariateML.Truncation`

#### `finite_exists_iff`

A finite log likelihood attains precisely for a mean statistic in the relative interior of the conditional convex support.

```lean
theorem finite_exists_iff [FiniteDimensional ℝ E] (hA : A.Nonempty) (hh : ∀ x ∈ A, 0 < h x) (m : E) :
    (∃ θ, IsMaxOn (logLik A T h m) univ θ) ↔
      m ∈ intrinsicInterior ℝ (convexHull ℝ (T '' (A : Set X)))
```

#### `convex_partition`

The finite log partition is convex in the natural parameter.

```lean
theorem convex_partition (hA : A.Nonempty) (hh : ∀ x ∈ A, 0 < h x) :
    ConvexOn ℝ univ (partition A T h)
```

#### `strict_partition_segment`

Jensen’s inequality for the partition is strict exactly on the stated nonflat segments. The shift theorem handles the complementary case.

```lean
theorem strict_partition_segment (hA : A.Nonempty) (hh : ∀ x ∈ A, 0 < h x)
    {θ φ : E} (hd : ∃ x ∈ A, ∃ y ∈ A, ⟪θ - φ, T x⟫ ≠ ⟪θ - φ, T y⟫)
    {a b : ℝ} (ha : 0 < a) (hb : 0 < b) (hab : a + b = 1) :
    partition A T h (a • θ + b • φ) < a * partition A T h θ + b * partition A T h φ
```

#### `maximizers_constant_direction`

Two finite maximizers can differ only by a direction whose dot product with each retained statistic is constant.

```lean
theorem maximizers_constant_direction (hA : A.Nonempty) (hh : ∀ x ∈ A, 0 < h x)
    {m θ φ : E} (hθ : IsMaxOn (logLik A T h m) univ θ)
    (hφ : IsMaxOn (logLik A T h m) univ φ) :
    ∀ x ∈ A, ∀ y ∈ A, ⟪θ - φ, T x⟫ = ⟪θ - φ, T y⟫
```

#### `partition_shift`

A constant tilt shift adds that constant to the log normalizer.

```lean
theorem partition_shift (d θ : E) {c : ℝ} (hA : A.Nonempty)
    (hh : ∀ x ∈ A, 0 < h x) (hd : ∀ x ∈ A, ⟪d, T x⟫ = c) :
    partition A T h (θ + d) = partition A T h θ + c
```

#### `logLik_shift`

When the observed mean has the same projection as the retained statistics, the constant shift also preserves the likelihood.

```lean
theorem logLik_shift (d θ m : E) (hA : A.Nonempty)
    (hh : ∀ x ∈ A, 0 < h x) (hd : ∀ x ∈ A, ⟪d, T x⟫ = ⟪d, m⟫) :
    logLik A T h m (θ + d) = logLik A T h m θ
```

#### `scalar_existsUnique_iff`

On a real-valued window with distinct extremes, the mean must lie strictly between them.

```lean
theorem scalar_existsUnique_iff {A : Finset X} {T : X → ℝ} {h : X → ℝ}
    (hh : ∀ x ∈ A, 0 < h x) {l u : X} (hl : l ∈ A) (hu : u ∈ A)
    (hlu : T l < T u) (hb : ∀ x ∈ A, T x ∈ Icc (T l) (T u)) (m : ℝ) :
    (∃! θ, IsMaxOn (logLik A T h m) univ θ) ↔ m ∈ Ioo (T l) (T u)
```

#### `scalar_negative_existsUnique_iff`

For a window restricted to negative natural tilts, the excluded zero tilt replaces the upper endpoint in the scalar criterion.

```lean
theorem scalar_negative_existsUnique_iff {A : Finset X} {T h : X → ℝ}
    (hh : ∀ x ∈ A, 0 < h x) {l u : X} (hl : l ∈ A) (hu : u ∈ A)
    (hlu : T l < T u) (hb : ∀ x ∈ A, T x ∈ Icc (T l) (T u)) {m : ℝ}
    (hm : m ∈ Ioo (T l) (T u)) :
    (∃! θ, θ < 0 ∧ IsMaxOn (logLik A T h m) (Iio 0) θ) ↔ m < carrierMean A T h
```

#### `scalar_negative_upper_failure`

When the empirical mean reaches or exceeds the carrier mean, the excluded zero tilt is strictly better than all feasible negative tilts.

```lean
theorem scalar_negative_upper_failure {A : Finset X} {T h : X → ℝ}
    (hh : ∀ x ∈ A, 0 < h x) {l u : X} (hl : l ∈ A) (hu : u ∈ A)
    (hlu : T l < T u) {m : ℝ} (hm : carrierMean A T h ≤ m) :
    StrictMonoOn (logLik A T h m) (Iic 0) ∧
      ∀ t < 0, ¬ IsMaxOn (logLik A T h m) (Iio 0) t
```

#### `singleton_logLik`

A singleton window has constant parameter-dependent log likelihood at its statistic.

```lean
theorem singleton_logLik (x : X) (hx : 0 < h x) (θ : E) :
    logLik {x} T h (T x) θ = -log (h x)
```

#### `modified_factorization`

A product of zero-modified masses factors into the Bernoulli zero-weight likelihood and the positive conditional likelihood.

```lean
theorem modified_factorization {P : Type*} {k : ℕ} (z : ℕ) (x : Fin k → ℕ)
    (g : P → ℕ → ℝ) (p : P × ℝ) :
    modifiedLikelihood z x g p =
      (p.2 ^ z * (1 - p.2) ^ k) * ∏ i, g p.1 (x i)
```

#### `bernoulli_max_iff`

The Bernoulli zero-weight maximizer is the empirical zero fraction, including zero and one.

```lean
theorem bernoulli_max_iff {z k : ℕ} (hn : 0 < z + k) {w : ℝ}
    (hw : w ∈ Icc (0 : ℝ) 1) :
    IsMaxOn (bernoulli z k) (Icc 0 1) w ↔ w = (z : ℝ) / (z + (k : ℝ))
```

#### `held_modified_max_iff`

Compatible held zero weight contributes a positive constant.

```lean
theorem held_modified_max_iff {P : Type*} {D : Set P} {L : P → ℝ}
    {z k : ℕ} {w : ℝ} (hw : 0 < bernoulli z k w) {p : P} :
    IsMaxOn (fun t ↦ bernoulli z k w * L t) D p ↔ IsMaxOn L D p
```

#### `bernoulli_strictMono`

The Bernoulli likelihood increases strictly to its empirical mode from the left.

```lean
theorem bernoulli_strictMono {z k : ℕ} (hn : 0 < z+k) :
    StrictMonoOn (bernoulli z k) (Icc 0 (zeroFraction z k))
```

#### `bernoulli_strictAnti`

The Bernoulli likelihood decreases strictly after its empirical mode.

```lean
theorem bernoulli_strictAnti {z k : ℕ} (hn : 0 < z+k) :
    StrictAntiOn (bernoulli z k) (Icc (zeroFraction z k) 1)
```

#### `bernoulli_clip`

At a fixed parent zero mass, the unique fitted inflated zero weight is the clipped empirical zero fraction.

```lean
theorem bernoulli_clip {z k : ℕ} (hn : 0 < z+k) {f w : ℝ} (hf : f ∈ Icc (0 : ℝ) 1)
    (hw : w ∈ Icc f 1) :
    IsMaxOn (bernoulli z k) (Icc f 1) w ↔ w = max f (zeroFraction z k)
```

#### `factorized_max_iff`

For nonnegative factors with at least one positive product, the joint maximizing set is the Cartesian product of the individual maximizing sets.

```lean
theorem factorized_max_iff {P : Type*} (D : Set P) (W : Set ℝ) (L : P → ℝ)
    (B : ℝ → ℝ) {p : P} {w : ℝ} (hp : p ∈ D) (hw : w ∈ W)
    (hL : ∀ q ∈ D, 0 ≤ L q) (hB : ∀ v ∈ W, 0 ≤ B v)
    (hpos : ∃ q ∈ D, ∃ v ∈ W, 0 < B v * L q) :
    IsMaxOn (fun t : P × ℝ ↦ B t.2 * L t.1) (D ×ˢ W) (p, w) ↔
      IsMaxOn L D p ∧ IsMaxOn B W w
```

#### `modified_max_iff`

The maximizing set of a zero-modified fit is the product of the conditional maximizing set and the singleton empirical zero weight.

```lean
theorem modified_max_iff {P : Type*} {z k : ℕ} (hn : 0 < z + k)
    {D : Set P} {L : P → ℝ} (hL : ∀ p ∈ D, 0 ≤ L p)
    (hpos : ∃ p ∈ D, 0 < L p) {p : P} {w : ℝ} (hp : p ∈ D) (hw : w ∈ Icc 0 1) :
    IsMaxOn (fun t : P × ℝ ↦ bernoulli z k t.2 * L t.1) (D ×ˢ Icc 0 1) (p, w) ↔
      IsMaxOn L D p ∧ w = (z : ℝ) / (z + (k : ℝ))
```

#### `modified_unique`

Raising a positive conditional likelihood to the positive sample size and adjoining the optimized Bernoulli factor preserves uniqueness.

```lean
theorem modified_unique {P : Type*} {D : Set P} {L : P → ℝ} {z k : ℕ} (hk : 0 < k)
    (hL : ∀ p ∈ D, 0 ≤ L p) (hwit : ∃ p ∈ D, 0 < L p)
    (hmax : ∃! p, p ∈ D ∧ IsMaxOn L D p) :
    ∃! t : P × ℝ, t ∈ D ×ˢ Icc 0 1 ∧
      IsMaxOn (fun t : P × ℝ ↦ bernoulli z k t.2 * L t.1 ^ k) (D ×ˢ Icc 0 1) t
```

#### `inflation_weight_iff`

The zero mass achievable by ordinary additive inflation is precisely the closed interval from the parent zero mass to one.

```lean
theorem inflation_weight_iff {f w : ℝ} (_hf : 0 ≤ f) (hf' : f < 1) :
    (∃ rho ∈ Icc (0 : ℝ) 1, w = f + (1 - f) * rho) ↔ w ∈ Icc f 1
```

#### `inflation_tilt_iff`

A nonnegative log tilt of the zero carrier has exactly the inflation weights below one that are at least the parent zero mass.

```lean
theorem inflation_tilt_iff {f w : ℝ} (hf : 0 < f) (hf' : f < 1) :
    (∃ d ≥ 0, w = f * exp d / ((1-f) + f * exp d)) ↔ w ∈ Ico f 1
```

#### `fixed_carrier_inflation_boundary`

The ordinary mean fit is a global zero-inflation boundary optimum if its parent zero mass is at least the empirical zero fraction. The comparison is in the common count and zero log-tilt coordinates.

```lean
theorem fixed_carrier_inflation_boundary {P : ℕ → ℝ} {m z : ℝ}
    (hP : ∀ k, 0 ≤ P k) (h0 : 0 < P 0)
    (hMass : HasSum P 1) (hMean : HasSum (fun k : ℕ ↦ P k * k) m)
    (hz : z ≤ P 0) (a d : ℝ) (hd : 0 ≤ d)
    (hs : Summable (fun k : ℕ ↦ P k * exp (a * k + if k = 0 then d else 0))) :
    a*m + z*d ≤ log (∑' k : ℕ, P k * exp (a * k + if k = 0 then d else 0))
```

#### `concave_boundary_max`

A nonpositive inward first variation in every feasible direction suffices for a global boundary maximum of a concave objective.

```lean
theorem concave_boundary_max {V : Type*} [AddCommGroup V] [Module ℝ V]
    {D : Set V} {L : V → ℝ} (hL : ConcaveOn ℝ D L) {p : V} (hp : p ∈ D)
    (hd : ∀ q ∈ D, ∃ s ≤ 0,
      HasDerivAt (fun t : ℝ ↦ L ((1-t) • p + t • q)) s 0) :
    IsMaxOn L D p
```

### `UnivariateML.TruncCount`

#### `lower_window_fit`

An open log-tilt interval extending to the left has a unique minimum at a strictly positive excess mean, provided the right end has a barrier above the value at every interior tilt.

```lean
theorem lower_window_fit {D : Set ℝ} (hD : Convex ℝ D) (hopen : IsOpen D)
    (htail : ∀ᶠ t : ℝ in atBot, t ∈ D) {c : ℕ → ℝ} (hc : ∀ k, 0 < c k) {m : ℝ}
    (hm : 0 ≤ m) (hs : ∀ t ∈ D, Summable (fun k : ℕ ↦ c k * exp t ^ k))
    (hRight : ∀ t ∈ D, ∃ u ∈ D, t < u ∧ centeredSum c m t < centeredSum c m u) :
    (∃! t, t ∈ D ∧ IsMinOn (centeredSum c m) D t) ↔ 0 < m
```

#### `lower_atom`

At the lower sample mean the conditional distribution approaches the first retained atom. This limit holds for each fixed retained count.

```lean
theorem lower_atom {c : ℕ → ℝ} (hc : ∀ k, 0 < c k) {b : ℝ}
    (hs : Summable (fun k : ℕ ↦ c k * exp b ^ k)) (j : ℕ) :
    Tendsto (fun t : ℝ ↦ c j * exp t ^ j / (∑' k : ℕ, c k * exp t ^ k))
      atBot (𝓝 (if j = 0 then 1 else 0))
```

#### `poisson_lower_fit`

On a Poisson lower-only window with its lower atom excluded, a unique rate fits exactly when the mean exceeds the retained lower bound.

```lean
theorem poisson_lower_fit (a : ℕ) {m : ℝ} (hm : (a : ℝ) ≤ m) :
    (∃! t, IsMinOn (centeredSum (poissonCarrier a) (m - a)) univ t) ↔ (a : ℝ) < m
```

#### `poisson_lower_atom`

The Poisson lower-only window approaches the first retained atom as rate tends to zero in log-rate coordinates.

```lean
theorem poisson_lower_atom (a j : ℕ) :
    Tendsto (fun t : ℝ ↦ poissonCarrier a j * exp t ^ j /
      (∑' k : ℕ, poissonCarrier a k * exp t ^ k)) atBot (𝓝 (if j = 0 then 1 else 0))
```

#### `geom_lower_fit`

On a geometric lower-only window with the lower atom excluded, a unique probability fits exactly when the mean exceeds the lower cut.

```lean
theorem geom_lower_fit (a : ℕ) {m : ℝ} (hm : (a : ℝ) ≤ m) :
    (∃! t, t < 0 ∧ IsMinOn (centeredSum (fun _ ↦ 1) (m - a)) (Iio 0) t) ↔
      (a : ℝ) < m
```

#### `geom_lower_atom`

The geometric lower-only window approaches the first retained atom.

```lean
theorem geom_lower_atom (j : ℕ) :
    Tendsto (fun t : ℝ ↦ exp t ^ j / (∑' k : ℕ, exp t ^ k)) atBot
      (𝓝 (if j = 0 then 1 else 0))
```

#### `nbinom_lower_fit`

At positive held size, an interior NB fit on a lower-only window exists uniquely exactly when the mean exceeds its first retained count.

```lean
theorem nbinom_lower_fit {r : ℝ} (hr : 0 < r) (a : ℕ) {m : ℝ} (hm : (a : ℝ) ≤ m) :
    (∃! t, t < 0 ∧ IsMinOn (centeredSum (fun k ↦ negBinomCarrier r (a+k)) (m - a))
      (Iio 0) t) ↔ (a : ℝ) < m
```

#### `nbinom_lower_atom`

Positive-size NB approaches the atom at the retained lower bound.

```lean
theorem nbinom_lower_atom {r : ℝ} (hr : 0 < r) (a j : ℕ) :
    Tendsto (fun t : ℝ ↦ negBinomCarrier r (a+j) * exp t ^ j /
      (∑' k : ℕ, negBinomCarrier r (a+k) * exp t ^ k)) atBot
      (𝓝 (if j = 0 then 1 else 0))
```

#### `poisson_lower_failure`

All observations at the first retained count exclude a finite positive Poisson rate if the lower atom is unavailable.

```lean
theorem poisson_lower_failure (a : ℕ) (t : ℝ) :
    ¬ IsMinOn (centeredSum (poissonCarrier a) 0) univ t
```

#### `geom_lower_failure`

All observations at the first retained count exclude a finite interior geometric probability if the lower atom is unavailable.

```lean
theorem geom_lower_failure {t : ℝ} (ht : t < 0) :
    ¬ IsMinOn (centeredSum (fun _ ↦ 1) 0) (Iio 0) t
```

#### `nbinom_lower_failure`

All observations at the first retained count exclude a finite interior NB probability if the lower atom is unavailable.

```lean
theorem nbinom_lower_failure {r : ℝ} (hr : 0 < r) (a : ℕ) {t : ℝ} (ht : t < 0) :
    ¬ IsMinOn (centeredSum (fun k ↦ negBinomCarrier r (a+k)) 0) (Iio 0) t
```

#### `nbinom_zt_fit`

Positive conditioning for held NB size admits the lower atom. At size zero the positive component uses the logarithmic carrier.

```lean
theorem nbinom_zt_fit {r : ℝ} (hr : 0 ≤ r) {m : ℝ} (hm : 1 ≤ m) :
    let c := if r = 0 then logSeriesCarrier else fun k ↦ negBinomCarrier r (1+k)
    ∃! p, p ∈ closedLowerDomain (Iio 0) ∧
      IsMaxOn (closedLowerLikelihood c (m-1)) (closedLowerDomain (Iio 0)) p
```

#### `poisson_bounded_fit`

The unique positive-rate fit on a nonsingleton bounded Poisson window exists exactly at strictly interior means.

```lean
theorem poisson_bounded_fit {l u : ℕ} (hlu : l < u) (m : ℝ) :
    (∃! t, IsMaxOn (finiteLogLik l u (poissonCarrier 0) m) univ t) ↔
      m ∈ Ioo (l : ℝ) (u : ℝ)
```

#### `binom_bounded_fit`

Fixed binomial size restricts the retained window to counts at most that size. A nonsingleton window has exactly one interior probability fit iff the mean lies strictly between its retained endpoints.

```lean
theorem binom_bounded_fit {l u N : ℕ} (hlu : l < u) (huN : u ≤ N) (m : ℝ) :
    (∃! t, IsMaxOn (finiteLogLik l u (fun k ↦ (N.choose k : ℝ)) m) univ t) ↔
      m ∈ Ioo (l : ℝ) (u : ℝ)
```

#### `nbinom_bounded_fit`

At positive held NB size the upper threshold is the finite window's mean under its untilted carrier.

```lean
theorem nbinom_bounded_fit {r : ℝ} (hr : 0 < r) {l u : ℕ} (hlu : l < u)
    {m : ℝ} (hm : m ∈ Ioo (l : ℝ) (u : ℝ)) :
    (∃! t, t < 0 ∧ IsMaxOn (finiteLogLik l u (negBinomCarrier r) m) (Iio 0) t) ↔
      m < Truncation.carrierMean (Finset.Icc l u) (fun k ↦ (k : ℝ)) (negBinomCarrier r)
```

#### `geom_bounded_fit`

The geometric upper threshold is the mean of the retained discrete uniform distribution. This is expressed as the mean of the unit carrier.

```lean
theorem geom_bounded_fit {l u : ℕ} (hlu : l < u) {m : ℝ}
    (hm : m ∈ Ioo (l : ℝ) (u : ℝ)) :
    (∃! t, t < 0 ∧ IsMaxOn (finiteLogLik l u (fun _ ↦ 1) m) (Iio 0) t) ↔
      m < Truncation.carrierMean (Finset.Icc l u) (fun k ↦ (k : ℝ)) (fun _ ↦ 1)
```

#### `geom_bounded_upper_failure`

At or above the uniform-window mean every geometric tilt is improved by approaching the excluded zero tilt.

```lean
theorem geom_bounded_upper_failure {l u : ℕ} (hlu : l < u) {m : ℝ}
    (hm : Truncation.carrierMean (Finset.Icc l u) (fun k ↦ (k : ℝ)) (fun _ ↦ 1) ≤ m) :
    StrictMonoOn (finiteLogLik l u (fun _ ↦ 1) m) (Iic 0) ∧
      ∀ t < 0, ¬ IsMaxOn (finiteLogLik l u (fun _ ↦ 1) m) (Iio 0) t
```

#### `nbinom_bounded_upper_failure`

At or above the normalized carrier mean, the positive-size NB window has no finite probability maximizer.

```lean
theorem nbinom_bounded_upper_failure {r : ℝ} (hr : 0 < r) {l u : ℕ} (hlu : l < u) {m : ℝ}
    (hm : Truncation.carrierMean (Finset.Icc l u) (fun k ↦ (k : ℝ)) (negBinomCarrier r) ≤ m) :
    StrictMonoOn (finiteLogLik l u (negBinomCarrier r) m) (Iic 0) ∧
      ∀ t < 0, ¬ IsMaxOn (finiteLogLik l u (negBinomCarrier r) m) (Iio 0) t
```

#### `finite_lower_atom`

The finite-window distribution tends to its lower retained atom at natural tilt minus infinity.

```lean
theorem finite_lower_atom {l u j : ℕ} {h : ℕ → ℝ} (hlu : l ≤ u)
    (hj : j ∈ Finset.Icc l u) (hh : 0 < h l) :
    Tendsto (finiteCountMass l u h j) atBot (𝓝 (if j = l then 1 else 0))
```

#### `finite_upper_atom`

The finite-window distribution tends to its upper retained atom at natural tilt plus infinity.

```lean
theorem finite_upper_atom {l u j : ℕ} {h : ℕ → ℝ} (hlu : l ≤ u)
    (hj : j ∈ Finset.Icc l u) (hh : 0 < h u) :
    Tendsto (finiteCountMass l u h j) atTop (𝓝 (if j = u then 1 else 0))
```

#### `finite_zero_tilt_limit`

At the excluded zero tilt the finite conditional law is the normalized carrier, including the uniform law for a unit carrier.

```lean
theorem finite_zero_tilt_limit {l u j : ℕ} {h : ℕ → ℝ} (hlu : l ≤ u)
    (hh : ∀ k ∈ Finset.Icc l u, 0 < h k) :
    Tendsto (finiteCountMass l u h j) (𝓝[<] 0)
      (𝓝 (h j / ∑ k ∈ Finset.Icc l u, h k))
```

#### `finite_count_fit`

For a nonsingleton finite count window with unrestricted finite tilts, attainment includes precisely the admitted mean-matching atoms.

```lean
theorem finite_count_fit {l u : ℕ} (hlu : l < u) {h : ℕ → ℝ}
    (hh : ∀ k ∈ Finset.Icc l u, 0 < h k) {m : ℝ} (hm : m ∈ Icc (l : ℝ) (u : ℝ))
    (lower upper : Bool) :
    (∃! p, p ∈ finiteCountDomain lower upper ∧
      IsMaxOn (finiteCountLikelihood l u h m) (finiteCountDomain lower upper) p) ↔
      m ∈ Ioo (l : ℝ) (u : ℝ) ∨ (m = l ∧ lower = true) ∨ (m = u ∧ upper = true)
```

#### `poisson_bounded_attainment`

On a finite Poisson window, zero rate is available at lower count zero or when a lower-atom extension has explicitly been selected.

```lean
theorem poisson_bounded_attainment {l u : ℕ} (hlu : l < u) {m : ℝ}
    (hm : m ∈ Icc (l : ℝ) (u : ℝ)) (extension : Bool) :
    let admitted := (l == 0) || extension
    (∃! p, p ∈ finiteCountDomain admitted false ∧
      IsMaxOn (finiteCountLikelihood l u (poissonCarrier 0) m)
        (finiteCountDomain admitted false) p) ↔
      m ∈ Ioo (l : ℝ) (u : ℝ) ∨ (m = l ∧ admitted = true)
```

#### `binom_bounded_attainment`

On a finite binomial window, the upper atom belongs to the parent at the held size and the lower atom belongs at zero or by extension.

```lean
theorem binom_bounded_attainment {l u N : ℕ} (hlu : l < u) (huN : u ≤ N)
    {m : ℝ} (hm : m ∈ Icc (l : ℝ) (u : ℝ)) (extension : Bool) :
    let lower := (l == 0) || extension
    let upper := u == N
    (∃! p, p ∈ finiteCountDomain lower upper ∧
      IsMaxOn (finiteCountLikelihood l u (fun k ↦ (N.choose k : ℝ)) m)
        (finiteCountDomain lower upper) p) ↔
      m ∈ Ioo (l : ℝ) (u : ℝ) ∨ (m = l ∧ lower = true) ∨ (m = u ∧ upper = true)
```

#### `binom_zt_fit`

The positive fixed-size binomial slice admits both atoms. At size at least two its maximizing probability is unique.

```lean
theorem binom_zt_fit {N : ℕ} (hN : 1 < N) {m : ℝ} (hm : m ∈ Icc (1 : ℝ) N) :
    ∃! p, p ∈ finiteCountDomain true true ∧
      IsMaxOn (finiteCountLikelihood 1 N (fun k ↦ (N.choose k : ℝ)) m)
        (finiteCountDomain true true) p
```

#### `binom_zt_size_one`

With binomial size one every positive-conditional probability parameter gives unit likelihood at the only retained count.

```lean
theorem binom_zt_size_one (p : Sum ℝ Bool) :
    finiteCountLikelihood 1 1 (fun k ↦ ((1 : ℕ).choose k : ℝ)) 1 p = 1
```

#### `binom_zm_size_one`

With positive counts at binomial size one the positive component is flat and only the zero weight is identified.

```lean
theorem binom_zm_size_one {z k : ℕ} (hk : 0 < k) {w : ℝ} (hw : w ∈ Icc 0 1)
    (p : Sum ℝ Bool) :
    let D := finiteCountDomain true true
    let L := finiteCountLikelihood 1 1 (fun j ↦ ((1 : ℕ).choose j : ℝ)) 1
    IsMaxOn (fun t : Sum ℝ Bool × ℝ ↦ Truncation.bernoulli z k t.2 * L t.1 ^ k)
      (D ×ˢ Icc 0 1) (p,w) ↔ w = Truncation.zeroFraction z k
```

#### `nbinom_zm_fit`

The fixed-size zero-modified NB fit is unique when positives occur, including the logarithmic-series slice at held size zero.

```lean
theorem nbinom_zm_fit {r : ℝ} (hr : 0 ≤ r) {m : ℝ} (hm : 1 ≤ m) {z k : ℕ} (hk : 0 < k) :
    let c := if r = 0 then logSeriesCarrier else fun j ↦ negBinomCarrier r (1+j)
    let D := closedLowerDomain (Iio 0)
    ∃! p : Option ℝ × ℝ, p ∈ D ×ˢ Icc 0 1 ∧
      IsMaxOn (fun t : Option ℝ × ℝ ↦
        Truncation.bernoulli z k t.2 * closedLowerLikelihood c (m-1) t.1 ^ k)
        (D ×ˢ Icc 0 1) p
```

#### `binom_zm_fit`

The fixed-size zero-modified binomial fit is unique with positives at size at least two.

```lean
theorem binom_zm_fit {N : ℕ} (hN : 1 < N) {m : ℝ} (hm : m ∈ Icc (1 : ℝ) N)
    {z k : ℕ} (hk : 0 < k) :
    let D := finiteCountDomain true true
    let L := finiteCountLikelihood 1 N (fun j ↦ (N.choose j : ℝ)) m
    ∃! p : Sum ℝ Bool × ℝ, p ∈ D ×ˢ Icc 0 1 ∧
      IsMaxOn (fun t : Sum ℝ Bool × ℝ ↦ Truncation.bernoulli z k t.2 * L t.1 ^ k)
        (D ×ˢ Icc 0 1) p
```

### `UnivariateML.TruncZero`

#### `modified_all_zero`

For all-zero data, every admissible parent parameter maximizes at zero weight one, and the fitted zero weight is unique.

```lean
theorem modified_all_zero {P : Type*} {D : Set P} {z : ℕ} (hz : 0 < z)
    {p : P} {w : ℝ} (hp : p ∈ D) (hw : w ∈ Icc 0 1) :
    IsMaxOn (fun t : P × ℝ ↦ bernoulli z 0 t.2) (D ×ˢ Icc 0 1) (p, w) ↔ w = 1
```

#### `poisson_zt_fit`

Mean matching on the positive sample, with the declared zero-rate atom at one, gives a unique zero-truncated Poisson fit.

```lean
theorem poisson_zt_fit {m : ℝ} (hm : 1 ≤ m) :
    ∃! rate, rate ∈ Ici (0 : ℝ) ∧ IsMaxOn (likelihood m) (Ici 0) rate
```

#### `geom_zt_fit`

The positive-geometric optimum is the reciprocal positive mean, including the all-ones atom at probability one.

```lean
theorem geom_zt_fit {m : ℝ} (hm : 1 ≤ m) :
    ∃! p, p ∈ Ioc (0 : ℝ) 1 ∧ IsMaxOn (likelihood (m - 1)) (Ioc 0 1) p
```

#### `poisson_zm_fit`

With positives, zero-modified Poisson has a unique joint fit on its closed rate and zero-weight domain.

```lean
theorem poisson_zm_fit {z k : ℕ} (hk : 0 < k) {m : ℝ} (hm : 1 ≤ m) :
    ∃! t : ℝ × ℝ, t ∈ Ici 0 ×ˢ Icc 0 1 ∧ IsMaxOn
      (fun t : ℝ × ℝ ↦ bernoulli z k t.2 * Discrete.ZeroTruncatedPoisson.likelihood m t.1 ^ k)
      (Ici 0 ×ˢ Icc 0 1) t
```

#### `geom_zm_fit`

With positives, zero-modified geometric has a unique joint fit.

```lean
theorem geom_zm_fit {z k : ℕ} (hk : 0 < k) {m : ℝ} (hm : 1 ≤ m) :
    ∃! t : ℝ × ℝ, t ∈ Ioc 0 1 ×ˢ Icc 0 1 ∧ IsMaxOn
      (fun t : ℝ × ℝ ↦ bernoulli z k t.2 * Discrete.Geometric.likelihood (m-1) t.1 ^ k)
      (Ioc 0 1 ×ˢ Icc 0 1) t
```

#### `geom_zi_fit`

With positives, free geometric inflation fits the positive conditional solution if feasible, and otherwise fits the ordinary parent with no inflation. The seam belongs to both descriptions.

```lean
theorem geom_zi_fit {z k d : ℕ} (hk : 0 < k) :
    let a := 1 - zeroFraction z k
    let q := if a ≤ zeroFraction d k then zeroFraction d k
      else zeroFraction (d+k) (z+k)
    let w := max (1-q) (zeroFraction z k)
    (q,w) ∈ inflatedGeomDomain ∧ IsMaxOn (inflatedGeom z k d) inflatedGeomDomain (q,w) ∧
      ∀ t ∈ inflatedGeomDomain, IsMaxOn (inflatedGeom z k d) inflatedGeomDomain t → t = (q,w)
```

#### `binom_zi_attainment`

Both coefficients of the fixed-size inflated binomial range over closed unit intervals, so its likelihood attains its maximum.

```lean
theorem binom_zi_attainment {k : ℕ} (N z : ℕ) (x : Fin k → ℕ) :
    ∃ t ∈ (Icc (0 : ℝ) 1 ×ˢ Icc (0 : ℝ) 1),
      IsMaxOn (inflatedBinom N z x) (Icc 0 1 ×ˢ Icc 0 1) t
```

#### `binom_zi_held_inflation_attainment`

With inflation held, the fixed-size binomial likelihood attains on the closed probability interval.

```lean
theorem binom_zi_held_inflation_attainment {k : ℕ} (N z : ℕ) (x : Fin k → ℕ) (rho : ℝ) :
    ∃ p ∈ Icc (0 : ℝ) 1, IsMaxOn (fun p ↦ inflatedBinom N z x (p,rho)) (Icc 0 1) p
```

#### `binom_zi_size_one`

At binomial size one the full inflated maximizing set is the Bernoulli product curve.

```lean
theorem binom_zi_size_one {z k : ℕ} (hn : 0 < z+k) {x : Fin k → ℕ} (hx : ∀ i, x i = 1)
    {p rho : ℝ} (hp : p ∈ Icc 0 1) (hrho : rho ∈ Icc 0 1) :
    IsMaxOn (inflatedBinom 1 z x) (Icc 0 1 ×ˢ Icc 0 1) (p,rho) ↔
      (1-rho)*p = 1 - zeroFraction z k
```

#### `inflated_all_zero`

With only zeros, a free inflated zero mass is optimal exactly when the parent is already a zero atom or inflation is one.

```lean
theorem inflated_all_zero {z : ℕ} (hz : 0 < z) {f rho : ℝ}
    (hf : f ∈ Icc (0 : ℝ) 1) (hrho : rho ∈ Icc (0 : ℝ) 1) :
    (rho + (1-rho)*f)^z = 1 ↔ f = 1 ∨ rho = 1
```

#### `poisson_zi_positive_attainment`

With positives, a ZIP fit with free inflation attains at a finite positive rate. The same compactness argument applies at any held inflation strictly below one.

```lean
theorem poisson_zi_positive_attainment {z k S : ℕ} (hk : 0 < k) (hS : 0 < S)
    {W : Set ℝ} (hW : IsCompact W) (hvalid : W ⊆ Icc 0 1)
    {rho : ℝ} (hrho : rho ∈ W) (hrho' : rho < 1) :
    ∃ t ∈ (Ioi (0 : ℝ) ×ˢ W),
      IsMaxOn (inflatedPoisson z k S) (Ioi 0 ×ˢ W) t
```

#### `poisson_zi_boundary`

For ZIP, the ordinary mean rate with zero inflation is globally optimal when its zero mass dominates the observed zero fraction. The likelihood comparison is expressed in feasible log-tilt increments.

```lean
theorem poisson_zi_boundary {rate z : ℝ} (hr : 0 < rate) (hz : z ≤ exp (-rate))
    (a d : ℝ) (hd : 0 ≤ d) :
    a*rate + z*d ≤ log (∑' j : ℕ, poissonMass rate j * exp (a*j + if j = 0 then d else 0))
```

#### `nbinom_zi_positive_attainment`

At positive held NB size, compatible positive data attain in the interior probability domain, both with inflation free and with any compatible fixed inflation.

```lean
theorem nbinom_zi_positive_attainment {r : ℝ} (hr : 0 < r) {z k S : ℕ}
    (hk : 0 < k) (hS : 0 < S) {W : Set ℝ} (hW : IsCompact W)
    (hvalid : W ⊆ Icc 0 1) {rho : ℝ} (hi : rho ∈ W) (hi' : rho < 1) :
    ∃ t ∈ (Ioo (0 : ℝ) 1 ×ˢ W),
      IsMaxOn (inflatedNegBinom r z k S) (Ioo 0 1 ×ˢ W) t
```

#### `cmb_inflation_counterexample`

At held probability 317/500 and dispersion one, the zero-modified conditional winner is size two and its parent zero mass is infeasible. A size-three fit with positive inflation beats every zero-inflation fit on the same held slice for one zero and seven twos.

```lean
theorem cmb_inflation_counterexample :
    (∀ n : ℕ, n ≠ 0 → cmbConditionalTwo n < cmbConditionalTwo 0) ∧
      (1/8 : ℝ) < cmbFailureProb^2 ∧ cmbFailureProb^3 < (1/8 : ℝ) ∧
      (∀ n : ℕ, cmbFailureProb^(n+2) * cmbMassTwo n ^ 7 <
        bernoulli 1 7 (1/8) * cmbConditionalTwo 1 ^ 7)
```

#### `cmb_inflated_unique`

The full constrained CMB likelihood of the counterexample is maximized uniquely at size three and empirical zero weight one eighth.

```lean
theorem cmb_inflated_unique :
    let D : Set (ℕ × ℝ) := {t | t.2 ∈ Icc (cmbFailureProb^(t.1+2)) 1}
    let L (t : ℕ × ℝ) := bernoulli 1 7 t.2 * cmbConditionalTwo t.1 ^ 7
    (1, (1/8 : ℝ)) ∈ D ∧
      ∀ t ∈ D, t ≠ (1, (1/8 : ℝ)) → L t < L (1, (1/8 : ℝ))
```

