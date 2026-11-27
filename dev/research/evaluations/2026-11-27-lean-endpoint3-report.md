# TASK-432 Lean proofs for threshold, endpoint and beta families

The checked results cover the fixed-shape Feller–Pareto, Pareto III and Pareto IV joint threshold and scale optima, the fixed-scale threshold branches, the reflected nonstandard-beta width criteria, and the held-shape beta-prime maximizing escapes. The declaration inventory gives the exact scope of each result. The atlas fragment leaves independent coverage grades blank. [Atlas fragment](../existence-atlas/lean-map.d/lean-endpoint3.csv)

## Checks

The final workstation `lake build` exited zero with all 15 Endpoint3 modules temporarily imported. Lake reported `Build completed successfully (8106 jobs).` The import file was restored and `git diff --exit-code -- dev/lean/UnivariateML.lean` exited zero. File-level checks used `nice -n 10 /home/jonas/.elan/bin/lake env lean` through the workstation single-slot lock. The build used the existing Mathlib cache. All new proofs are closed.

## Source discrepancies and conventions

- The `pareto2 / scale+shape / failure` subunit lower-power branch is impossible in the dossier parameterization. Pareto II fixes the lower power to one. The formal result identifies the sample minimum as the unique threshold for every positive held scale and shape. [Threshold proof, parameterization](2026-11-25-thresh-math-report.md#structural-spikes)
- Several atlas failure claims repeat wording about simultaneous vanishing scale and upper beta shape across different hold masks. The Pareto III held-scale result uses the available subunit threshold spike. For nonstandard beta with one shape held and both endpoints free, a subunit free-end shape gives the spike. With one endpoint held, the inward-distance chart first checks the held-end factor. [Threshold proof](2026-11-25-thresh-math-report.md#beta-with-one-endpoint-held)
- The Pareto IV threshold chart uses the finite inside-support density at unit-power contact. Its equality with the dossier’s real log formula is proved on strictly containing thresholds. At contact the chart supplies the continuous extension. The Pareto IV, inverse Pareto and beta-prime face theorems state sample likelihoods or their finite comparisons. Weak convergence is a separate obligation where the source claims it.
- The inverse Pareto result uses the exact reciprocal Lomax no-witness test. The scale-zero Feller–Pareto result gives a sharp strict least upper bound through the existing envelope theorem and retains its existential presentation of the height. The general C++ detector margins and the zero-exponent law limit remain separate. [Coverage review](2026-11-27-atlas-coverage4a-report.md)

## Results and effort by target

| Target | Checked result and proof work |
| --- | --- |
| Feller–Pareto fixed regular coefficients | Joined the unit-power contact optimum and the unique superunit normalized-score root. Transferred to the stated Pareto III and IV kernels. |
| Feller–Pareto, Pareto III and Pareto IV with shapes held | Joined spike, negative exponent and finite zero-exponent supremum. For uniqueness, differentiated in log scale, obtained the common odds-sum equation at maxima, and proved strict ordering of normalized scores across two threshold gaps. The comparison uses the single crossing of the two vectors of scaled excesses and monotonic reweighting. |
| Feller–Pareto and Pareto IV with scale and lower shape and power held | Balanced the beta normalizer against the positive-gap penalty to confine free upper shape, then confined threshold gaps on each superlevel. Proved a coupled constant ray and joined it with the subunit spike. At unit lower power, used strict convexity of the log beta normalizer on the exact contact slice. |
| Pareto II, Pareto III and Pareto IV path and contact targets | Identified Pareto II contact. Joined the two Pareto IV structural rays and computed the fixed-product trace before multiplying over observations. Proved the constant-excess diverging-power rays with scale at or above the excess in the appropriate holds. |
| Lomax transfers | Transferred the two-level singleton to Pareto II, inverse Pareto and held-power Pareto IV using the reciprocal scale, reciprocal data and powered excess maps. The exact inverse-exponential failure also gives a fitted-rate realizing path. |
| Nonstandard beta | Joined the spike, zero-width collapse, negative distances and extended held-contact cases. Reduced the two reflected free-held-shape criteria to generalized beta at unit power. Covered both endpoints free with fixed shapes and transferred the fixed-mean two-shape ray when both endpoints are held. |
| Generalized beta with upper shape and power held | Used both normalizer tails and the sample maximum to prove compact regular superlevels in physical coordinates. Joined the constant and subunit divergent branches with the exact finite criterion. |
| Other generalized-beta endpoint holds | Joined the fixed-shape contact and score-root uniqueness, identified contact for the unique joint power and endpoint optimum at unit upper shape, and proved uniform subunit/contact necessity with first shape and power fixed. Used the fixed-increment gamma ratio for the held-scale concentration and gamma likelihood rays. |
| GB2 and threshold-held Feller–Pareto | Joined the two-held-shape GB2 criterion with constant divergence. Transferred both gamma likelihood rays by taking powered excesses and retaining the Jacobian. |
| Beta-prime | Proved uniform held-shape superlevel bounds using the small-shape recurrence, fixed-increment gamma ratio and real Bernoulli inequality. Combined compactness and strict gamma rate optimality to classify entire maximizing sequences. Used the reciprocal scale map for the other orientation. Proved the finite witness comparison including equality and the two-shape constant ray. |

## Open main keys

Each entry identifies the missing step at the scope of the atlas claim. The full target with a scientific law limit, a numerical certificate or an almost-everywhere uniqueness clause requires the corresponding additional statement.

| Family and hold | Keys | Missing proof |
| --- | --- | --- |
| Genbeta `scale` | criterion, failure, uniqueness | Need a uniform upper bound over the entire positive-level exterior with both beta shapes moving, then fit both faces and compare to stationary candidates. The gamma likelihood ray fixes the product exactly. The moving power needs all joint contact penalties. Analytic power profiling and exclusion of stationary accumulation require signed collars and the count-stratum uniqueness argument. |
| Genbeta `scale+shape1` | criterion, failure, uniqueness | Constant ray is checked. To confine the profiled power, bound the small-power `log c + R - log R` term, bound the large-power term by the gap between mean and minimum log distances, and control the upper shape on bounded power intervals. The curvature step still needs the trigamma bound for the Schur correction. |
| Genbeta `scale+shape2` | criterion, failure, uniqueness | The existing unit-shape ridge and the fixed-product gamma likelihood ray leave the signed exclusion of the power face above one, the full exterior for nonunit shape, and generic uniqueness at four or more levels. |
| Genbeta `shape1`, `shape3` | failure | There are subunit spikes and reachable regular constant collapses. The general necessity statement still needs uniform log-concave bounds on nonconstants and the joint rate and weak-law tests with the precise hold substituted. |
| Genbeta `shape1+shape2` | failure | The earlier constant and subunit paths and the checked optimizer location leave uniform compactness of regular nonconstant superlevels in the physical endpoint and power chart. |
| Genbeta `shape1+shape2+shape3` | failure | Every divergent subunit interior path was shown to approach contact. The regular branches still need an explicit all-sequence exclusion of positive-level escapes and the extended density evaluation at contact. |
| Genbeta `shape1+shape3` | failure | Uniform exhaustion by the subunit branch and convergence to the observed endpoint are checked. The expansion of the diverging contact terms for simultaneous changes in shape and gap, and the weak-law limit for upper shape tending to zero, remain open. |
| Genbeta `shape2` | criterion, failure, uniqueness | Need the shifted-gamma and moving-power global envelope including equality, the exact attained unit-shape ridge escape sequences, and the superunit count-stratum uniqueness proof. |
| Genbeta `shape2+shape3` | uniqueness | The regular superlevels are compact. At upper shape greater than one, the trigamma difference must dominate the conditional endpoint-profile Hessian correction. |
| Nonstandard beta `max+shape1`, `min+shape2` | uniqueness | The reflected criteria and failure branches are checked. The strictly downward profile-score crossing still requires the trigamma comparison in the free held-side shape. |
| Feller–Pareto `min`, `min+shape1`, `min+shape3` | criterion, failure, uniqueness | Need compactification in log mode, peak, power and two rates, convergence of the normalizers and full contact traces, and optimization of the surviving faces. The checked gamma rays establish reachability on the corresponding held slices. Constant divergence and conditional power–scale uniqueness were already available. The finite stationary-component list needs definability or a replacement argument, and generic uniqueness additionally needs count-stratum score separation. |
| Feller–Pareto `scale+shape2+shape3`, Pareto IV `scale+shape2` | uniqueness | Unit lower power is checked. The superunit branch needs local envelope regularity and separation of data scores on the count strata with at least five levels. |
| Pareto II `min`, inverse Pareto `all free`, Pareto IV `min+shape2` | uniqueness | The exact two-level singleton and the earlier three-level ties leave almost-everywhere uniqueness on the count strata. Candidate data scores are rational functions of the observations. The missing step is to pass from their separation to the measure-zero exceptional set through differentiability of the optimized value. |
| Pareto IV `min` | uniqueness | Need the all-free Burr crossing above both faces, including exclusion of other competitors, and the almost-everywhere theorem using the free-power score family. The two-level Lomax transfer holds only with power fixed. |
| GB2 `all free`, `shape1`, `shape3` | criterion, failure, uniqueness | Both gamma orientations now have likelihood rays. The all-free positive-level classification still needs the normal and two-sided and one-sided log-exponential limits, a bound on the density peak and mode, and exhaustion under joint rates with contact traces. On the single-shape holds only the applicable gamma and one-sided face survive in the written proof. All three masks need the stationary comparison and the count-stratum uniqueness argument, and the held-shape crossing examples need separated global winners. |
| Beta-prime `all free` | criterion, failure, uniqueness | The held-shape bounds do not control simultaneous shape movement. Need the compact ratio profile with both nuisance shapes moving, optimized gamma and inverse-gamma face continuity, the four-way equality construction and the almost-everywhere uniqueness argument. |
| Beta-prime `shape1`, `shape2` | criterion, uniqueness | The exact finite witness comparison includes equality and transfers by inversion. Its finite stationary list still needs analytic extension of the optimized scale, a strict exterior sign even when the first terms vanish, and interior root isolation. Generic uniqueness and the named global crossing remain open. |

## Path and detector keys

| Family and hold | Disposition |
| --- | --- |
| Genbeta `scale` and `scale+shape2`, `path:2` | Checked likelihood normalization at fixed product. The arbitrary convergent-product weak gamma law remains open. |
| Genbeta `scale`, `path:3` | Open. Need the moving upper endpoint, including the joint trace at ties and weak convergence on either side of contact. |
| Genbeta `scale+shape1`, `path:1`, `limit:1` | The coupled constant-data likelihood ray is checked. Weak collapse of the fitted laws and the exclusion of all nonconstant escapes are separate. The round-five `scale` and `scale+shape3` constant-data results supply their other feasible directions. |
| Pareto II `min`, inverse Pareto `all free`, `limit:1` | Open as numerical detectors. Need a theorem connecting the implemented finite-search bound and the specified margin to whole-domain comparison. The inverse Pareto exact no-witness predicate has a checked strict supremum. |
| Inverse Pareto `case:small-support` | The pointwise attained two-level singleton transfers. Any additional certification of the numerical fit or margin needs the implemented acceptance predicate. |
| Pareto III `min`, `min+scale`, `path:1`, `limit:1` | The round-five explicit constant ray applies when scale can equal the common excess. |
| Pareto IV `all free`, `path:1`; `min`, `min+scale`, `path:4`; `min+scale`, `path:5` | Checked both structural rays and the fixed-product Pareto sample trace including all ties and observations below scale. |
| Pareto IV `min`, `min+scale`, `min+shape1`, `min+scale+shape1`, `path:2` | The diverging-power likelihood rays are checked at scale equal to the common excess, and with variable upper shape at held scale above that excess. The earlier finite criteria for these holds give the applicable complement. Point-mass convergence of the laws is separate. |
| Pareto IV `min+scale`, `limit:5`; `min+shape1`, `min+scale+shape1`, `limit:2` | Exact branch predicates and the strict supremum or no-fit conclusion are in the round-five criteria and escape theorem. This pass adds explicit paths. The separate atlas detector records still need review. |
| Pareto IV `min`, `min+shape2`, `path:3` | The round-five transformed Weibull result uses the literal held-power product on the excesses. |
| Feller–Pareto `min`, `min+shape1+shape3`, `path:3`, `limit:3`; GB2 `all free`, `path:3`, `limit:3` | The round-five fixed-beta-shape, diverging-power likelihood paths give the constant branch. The added gamma rays support the separate transformed-gamma face paths. |
| Nonstandard beta `max+min`, `limit:1` | The affine fixed-mean concentration ray leaves both endpoints held and both shapes free. The existing criterion supplies the nonconstant complement. |
| Beta-prime `all free`, `path:3`; `shape1`, `path:1`, `path:4`; `shape2`, `path:2`, `path:5` | The constant two-shape ray and both held-shape likelihood limits are checked. The held-shape results allow any joint path with the prescribed positive rate limit. |
| Beta-prime `all free`, `limit:6`, `limit:7`; `shape1`, `limit:4`; `shape2`, `limit:5` | The all-free signed collars, skewness comparison and numerical margins remain open. The held-shape finite comparison is exact, including equality. The signed stationary-list and acceptance-certificate steps are open. |

The open-key references are the [round-five endpoint inventory](2026-11-27-lean-endpoint2-report.md#open-keys), [round-five shape obligations](2026-11-27-lean-shape2-report.md#remaining-proof-obligations), [coverage review A](2026-11-27-atlas-coverage4a-report.md#written-claims-awaiting-full-lean-coverage), [coverage review B](2026-11-27-atlas-coverage4b-report.md#written-claims-awaiting-full-lean-coverage), [generalized beta proofs](2026-11-27-genbeta-atlas-math-report.md), [held threshold proofs](2026-11-27-lsheld-atlas-math-report.md), [criterion proofs](2026-11-27-crit-open-math-report.md), and [uniqueness proofs](2026-11-27-uniq-shape-math-report.md).

## Checked public statements

58 public theorems are listed below.

### `UnivariateML.Endpoint3BetaPrime.gamma_of_shape_rate_limit`

With first shape held, every path with diverging second shape and second shape divided by scale converging to a positive rate has the corresponding gamma likelihood limit. [Endpoint3BetaPrime.lean](../../lean/UnivariateML/Family/Endpoint3BetaPrime.lean#L40)

```lean
theorem gamma_of_shape_rate_limit {ι : Type*} {l : Filter ι} {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {p r : ℝ} (hp : 0 < p) (hr : 0 < r)
    {q s : ι → ℝ} (hq : Tendsto q l atTop) (hs : ∀ᶠ j in l, 0 < s j)
    (hrate : Tendsto (fun j ↦ q j / s j) l (𝓝 r)) :
    Tendsto (fun j ↦ BetaPrime.logLik x (s j) (p, q j)) l
      (𝓝 (∑ i, Gamma.logDensity p r (x i)))
```

### `UnivariateML.Endpoint3BetaPrime.inverse_gamma_of_shape_rate_limit`

With second shape held, the reciprocal path with diverging first shape and first shape times scale tending to a positive rate gives the inverse-gamma likelihood, including the data Jacobian. [Endpoint3BetaPrime.lean](../../lean/UnivariateML/Family/Endpoint3BetaPrime.lean#L100)

```lean
theorem inverse_gamma_of_shape_rate_limit {ι : Type*} {l : Filter ι}
    {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) {q r : ℝ} (hq : 0 < q) (hr : 0 < r)
    {p s : ι → ℝ} (hp : Tendsto p l atTop) (hs : ∀ᶠ j in l, 0 < s j)
    (hrate : Tendsto (fun j ↦ p j * s j) l (𝓝 r)) :
    Tendsto (fun j ↦ BetaPrime.logLik x (s j) (p j, q)) l
      (𝓝 ((∑ i, Gamma.logDensity q r (x i)⁻¹) - ∑ i, 2 * log (x i)))
```

### `UnivariateML.Endpoint3BetaPrime.held_first_superlevel_bounds`

At any fixed finite likelihood level for a held first shape, the second shape is bounded below and its ratio to scale lies in a compact positive interval. This holds for every nonempty positive sample. [Endpoint3BetaPrime.lean](../../lean/UnivariateML/Family/Endpoint3BetaPrime.lean#L151)

```lean
theorem held_first_superlevel_bounds (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {p : ℝ} (hp : 0 < p) (M : ℝ) :
    ∃ ε > 0, ∃ lo > 0, ∃ hi > 0, ∀ q > 0, ∀ s > 0,
      M ≤ BetaPrime.logLik x s (p, q) → ε ≤ q ∧ q / s ∈ Icc lo hi
```

### `UnivariateML.Endpoint3BetaPrime.held_first_maximizing_escape`

For a maximizing sequence escaping every compact subset of the positive shape and scale domain at held first shape, the second shape must diverge and its ratio to scale must converge to the fitted gamma rate. Consequently the sequence has exactly the fitted gamma value. [Endpoint3BetaPrime.lean](../../lean/UnivariateML/Family/Endpoint3BetaPrime.lean#L277)

```lean
theorem held_first_maximizing_escape (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {p : ℝ} (hp : 0 < p) {q s : ℕ → ℝ}
    (hq : ∀ j, 0 < q j) (hs : ∀ j, 0 < s j)
    (hesc : ∀ K : Set (ℝ × ℝ), IsCompact K → K ⊆ Beta.domain →
      ∀ᶠ j in atTop, (q j, s j) ∉ K)
    {M : ℝ} (hM : ∀ u > 0, ∀ v > 0, BetaPrime.logLik x v (p, u) ≤ M)
    (hlim : Tendsto (fun j ↦ BetaPrime.logLik x (s j) (p, q j)) atTop (𝓝 M)) :
    let r := Gamma.rate x p
    Tendsto q atTop atTop ∧ Tendsto (fun j ↦ q j / s j) atTop (𝓝 r) ∧
      M = ∑ i, Gamma.logDensity p r (x i)
```

### `UnivariateML.Endpoint3BetaPrime.held_second_maximizing_escape`

At held second shape, every maximizing escape is the inverse-gamma face on the reciprocal sample. The first shape diverges and its product with scale converges to the fitted inverse-gamma parameter. [Endpoint3BetaPrime.lean](../../lean/UnivariateML/Family/Endpoint3BetaPrime.lean#L353)

```lean
theorem held_second_maximizing_escape (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {q : ℝ} (hq : 0 < q) {p s : ℕ → ℝ}
    (hp : ∀ j, 0 < p j) (hs : ∀ j, 0 < s j)
    (hesc : ∀ K : Set (ℝ × ℝ), IsCompact K → K ⊆ Beta.domain →
      ∀ᶠ j in atTop, (p j, s j) ∉ K)
    {M : ℝ} (hM : ∀ u > 0, ∀ v > 0, BetaPrime.logLik x v (u, q) ≤ M)
    (hlim : Tendsto (fun j ↦ BetaPrime.logLik x (s j) (p j, q)) atTop (𝓝 M)) :
    let r := Gamma.rate (fun i ↦ (x i)⁻¹) q
    Tendsto p atTop atTop ∧ Tendsto (fun j ↦ p j * s j) atTop (𝓝 r) ∧
      M = (∑ i, Gamma.logDensity q r (x i)⁻¹) - ∑ i, 2 * log (x i)
```

### `UnivariateML.Endpoint3BetaPrime.held_first_face_comparison`

At held first shape, a finite maximum exists if and only if a finite point reaches the fitted gamma value. This includes finite equality and all constant samples. The conclusion uses a finite point as witness and does not require a finite stationary list. [Endpoint3BetaPrime.lean](../../lean/UnivariateML/Family/Endpoint3BetaPrime.lean#L429)

```lean
theorem held_first_face_comparison (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {p : ℝ} (hp : 0 < p) :
    let L := fun v : ℝ × ℝ ↦ BetaPrime.logLik x v.2 (p, v.1)
    let G := ∑ i, Gamma.logDensity p (Gamma.rate x p) (x i)
    (∃ v ∈ Beta.domain, IsMaxOn L Beta.domain v) ↔
      ∃ v ∈ Beta.domain, G ≤ L v
```

### `UnivariateML.Endpoint3BetaPrime.held_second_face_comparison`

At held second shape, finite attainment is equivalent to a finite point reaching the fitted inverse-gamma value. Equality is included. The comparison uses the exact reciprocal parameter map. [Endpoint3BetaPrime.lean](../../lean/UnivariateML/Family/Endpoint3BetaPrime.lean#L497)

```lean
theorem held_second_face_comparison (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {q : ℝ} (hq : 0 < q) :
    let L := fun v : ℝ × ℝ ↦ BetaPrime.logLik x v.2 (v.1, q)
    let r := Gamma.rate (fun i ↦ (x i)⁻¹) q
    let G := (∑ i, Gamma.logDensity q r (x i)⁻¹) - ∑ i, 2 * log (x i)
    (∃ v ∈ Beta.domain, IsMaxOn L Beta.domain v) ↔
      ∃ v ∈ Beta.domain, G ≤ L v
```

### `UnivariateML.Endpoint3BetaPrime.constant_both_shapes_path`

Both beta-prime shapes diverge on the symmetric fixed-scale ray at a constant positive observation, and the sample likelihood diverges. [Endpoint3BetaPrime.lean](../../lean/UnivariateML/Family/Endpoint3BetaPrime.lean#L539)

```lean
theorem constant_both_shapes_path (hn : 0 < n) {v : ℝ} (hv : 0 < v) :
    let x : Fin n → ℝ := fun _ ↦ v
    Tendsto (fun t : ℝ ↦ BetaPrime.logLik x v (t / 2, t / 2)) atTop atTop
```

### `UnivariateML.Endpoint3FellerPareto.fixed_shapes_failure`

The strict-interior threshold spike and the negative scale exponent give unbounded likelihood. At regular lower power and zero exponent the sharp finite envelope is the strictly unattained supremum. [Endpoint3FellerPareto.lean](../../lean/UnivariateML/Family/Endpoint3FellerPareto.lean#L26)

```lean
theorem fixed_shapes_failure {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hz : ∃ i, d i = 0) {a b c K : ℝ} (hb : 0 < b) (hc : 0 < c) :
    let L := logLik d a b c K
    let D := domain a
    ((a < 1 ∨ exponent d b < 0) → ¬ BddAbove (L '' D)) ∧
      ((1 ≤ a ∧ exponent d b = 0) →
        ∃ C : ℝ, IsLUB (L '' D) C ∧ ∀ v ∈ D, L v < C)
```

### `UnivariateML.Endpoint3FellerPareto.constant_upper_shape_path`

With scale, lower beta shape and power fixed, a constant sample concentrates as the upper beta shape increases and the threshold gap shrinks. [Endpoint3FellerPareto.lean](../../lean/UnivariateML/Family/Endpoint3FellerPareto.lean#L48)

```lean
theorem constant_upper_shape_path (hn : 0 < n) {p c s : ℝ}
    (hp : 0 < p) (hc : 0 < c) (hs : 0 < s) :
    Tendsto (fun q ↦ Endpoint2FellerPareto.interiorLogLik (fun _ : Fin n ↦ 0)
      p q c s (s * (q⁻¹) ^ c⁻¹)) atTop atTop
```

### `UnivariateML.Endpoint3FellerPareto.held_scale_lower_shape_power_failure`

With fixed scale, lower beta shape and power, the subunit lower power and constant-data branches both have unbounded likelihood. [Endpoint3FellerPareto.lean](../../lean/UnivariateML/Family/Endpoint3FellerPareto.lean#L92)

```lean
theorem held_scale_lower_shape_power_failure {d : Fin n → ℝ}
    (hd : ∀ i, 0 ≤ d i) (hz : ∃ i, d i = 0) {p c s : ℝ}
    (hp : 0 < p) (hc : 0 < c) (hs : 0 < s)
    (h : c * p < 1 ∨ ∀ i, d i = 0) :
    ∀ B : ℝ, ∃ t > 0, ∃ q > 0, B < Endpoint2FellerPareto.interiorLogLik d p q c s t
```

### `UnivariateML.Endpoint3FellerPareto.fixed_regular_threshold_uniqueness`

The unique closed threshold at lower power one, and the unique positive score root above one, in the common threshold kernel. Pareto II uses lower power one, Pareto III sets both powers equal, and Pareto IV sets the lower power equal to the transformation power. [Endpoint3FellerPareto.lean](../../lean/UnivariateML/Family/Endpoint3FellerPareto.lean#L114)

```lean
theorem fixed_regular_threshold_uniqueness {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hz : ∃ i, d i = 0) {a b c s K : ℝ} (hb : 0 < b) (hc : 0 < c) (hs : 0 < s) :
    let L := Endpoint2FellerPareto.logLik d a b c s K
    let S := Endpoint2FellerParetoRegular.normalizedScore d a b c s
    (a = 1 → IsMaxOn L (Ici 0) 0 ∧ ∀ t ∈ Ici 0, IsMaxOn L (Ici 0) t → t = 0) ∧
    (1 < a → (∃! t, t ∈ Ioi 0 ∧ S t = 0) ∧
      ∀ t ∈ Ioi 0, IsMaxOn L (Ioi 0) t ↔ S t = 0)
```

### `UnivariateML.Endpoint3FellerParetoFreeScale.fixed_shapes_unique`

At attained fixed shapes, the joint scale and threshold are unique. At lower power one the threshold is closed contact, and at strictly larger lower power the threshold gap is positive. [Endpoint3FellerParetoFreeScale.lean](../../lean/UnivariateML/Family/Endpoint3FellerParetoFreeScale.lean#L263)

```lean
theorem fixed_shapes_unique {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hz : ∃ i, d i = 0) {a b c K : ℝ} (hb : 0 < b) (hc : 0 < c)
    (ha : 1 ≤ a) (hE : 0 < exponent d b) :
    ∃! v, v ∈ domain a ∧ IsMaxOn (logLik d a b c K) (domain a) v ∧
      (v.2 = 0 ↔ a = 1)
```

### `UnivariateML.Endpoint3FellerParetoHeldScale.regular_fit`

If the sample is nonconstant and the lower power is at least one, the threshold and upper shape have a finite maximizer. The unit lower power allows closed threshold contact. [Endpoint3FellerParetoHeldScale.lean](../../lean/UnivariateML/Family/Endpoint3FellerParetoHeldScale.lean#L90)

```lean
theorem regular_fit {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i) (hz : ∃ i, d i = 0)
    (hne : ∃ i, 0 < d i) {p c s : ℝ} (hp : 0 < p) (hc : 0 < c) (hs : 0 < s)
    (ha : 1 ≤ c * p) :
    let D := Ioi (0 : ℝ) ×ˢ (if c * p = 1 then Ici (0 : ℝ) else Ioi 0)
    let L := fun v : ℝ × ℝ ↦ Endpoint2FellerPareto.interiorLogLik d p v.1 c s v.2
    ∃ v ∈ D, IsMaxOn L D v
```

### `UnivariateML.Endpoint3FellerParetoHeldScale.criterion`

At held scale, lower shape and power, attainment holds exactly at regular lower power on a nonconstant sample. [Endpoint3FellerParetoHeldScale.lean](../../lean/UnivariateML/Family/Endpoint3FellerParetoHeldScale.lean#L264)

```lean
theorem criterion {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i) (hz : ∃ i, d i = 0)
    {p c s : ℝ} (hp : 0 < p) (hc : 0 < c) (hs : 0 < s) :
    let D := Ioi (0 : ℝ) ×ˢ (if c * p = 1 then Ici (0 : ℝ) else Ioi 0)
    let L := fun v : ℝ × ℝ ↦ Endpoint2FellerPareto.interiorLogLik d p v.1 c s v.2
    (∃ v ∈ D, IsMaxOn L D v) ↔ 1 ≤ c * p ∧ ∃ i, 0 < d i
```

### `UnivariateML.Endpoint3FellerParetoHeldScale.unit_lower_power_unique`

When the held lower power is one, every nonconstant sample has one maximizing upper shape and threshold. The threshold is closed contact at the sample minimum. [Endpoint3FellerParetoHeldScale.lean](../../lean/UnivariateML/Family/Endpoint3FellerParetoHeldScale.lean#L289)

```lean
theorem unit_lower_power_unique {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hz : ∃ i, d i = 0) (hne : ∃ i, 0 < d i) {p c s : ℝ}
    (hp : 0 < p) (hc : 0 < c) (hs : 0 < s) (hunit : c * p = 1) :
    let D := Ioi (0 : ℝ) ×ˢ Ici (0 : ℝ)
    let L := fun v : ℝ × ℝ ↦ Endpoint2FellerPareto.interiorLogLik d p v.1 c s v.2
    ∃! v, v ∈ D ∧ IsMaxOn L D v ∧ v.2 = 0
```

### `UnivariateML.Endpoint3GB2.held_beta_shapes_criterion`

With positive held beta shapes and a positive nonempty sample, the free power and scale attain exactly on nonconstant data. On constants the same held-shape likelihood is unbounded. [Endpoint3GB2.lean](../../lean/UnivariateML/Family/Endpoint3GB2.lean#L31)

```lean
theorem held_beta_shapes_criterion {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {p q : ℝ} (hp : 0 < p) (hq : 0 < q) :
    let L := fun v : ℝ × ℝ ↦ Shape2GB2.logLik x v.2 v.1 (p, q)
    ((∃ v ∈ Beta.domain, IsMaxOn L Beta.domain v) ↔ ∃ i j, x i ≠ x j) ∧
      ((∀ i j, x i = x j) → ¬ BddAbove (L '' Beta.domain))
```

### `UnivariateML.Endpoint3GB2.feller_pareto_gamma_face_paths`

Both transformed-gamma likelihood faces of Feller–Pareto, expressed on the powered positive excesses. The upper shape can diverge while the lower shape is held, and the reciprocal ray holds the upper shape while the lower shape diverges. [Endpoint3GB2.lean](../../lean/UnivariateML/Family/Endpoint3GB2.lean#L60)

```lean
theorem feller_pareto_gamma_face_paths {n : ℕ} {x : Fin n → ℝ} {μ c p q r : ℝ}
    (hx : ∀ i, μ < x i) (hc : 0 < c) (hp : 0 < p) (hq : 0 < q) (hr : 0 < r) :
    let y := ShapeFacesFellerPareto.poweredExcess x μ c
    let J := ShapeFacesFellerPareto.powerJacobian x μ c
    (Tendsto (fun t : ℝ ↦ ShapeFacesFellerPareto.logLikelihood x μ c ((t / r) ^ c⁻¹) (p, t))
      atTop (𝓝 ((∑ i, Gamma.logDensity p r (y i)) + J))) ∧
    (Tendsto (fun t : ℝ ↦ ShapeFacesFellerPareto.logLikelihood x μ c ((r / t) ^ c⁻¹) (t, q))
      atTop (𝓝 ((∑ i, Gamma.logDensity q r (y i)⁻¹) - ∑ i, 2 * log (y i) + J)))
```

### `UnivariateML.Endpoint3GB2.gamma_face_paths`

At any fixed positive power, GB2 has both transformed-gamma likelihood limits, with either beta shape fixed along the appropriate joint ray. The Jacobian restores the original data coordinates. [Endpoint3GB2.lean](../../lean/UnivariateML/Family/Endpoint3GB2.lean#L109)

```lean
theorem gamma_face_paths {n : ℕ} {x : Fin n → ℝ} {c p q r : ℝ}
    (hx : ∀ i, 0 < x i) (hc : 0 < c) (hp : 0 < p) (hq : 0 < q) (hr : 0 < r) :
    let y := fun i ↦ (x i) ^ c
    let J := ∑ i, (log c + (c - 1) * log (x i))
    (Tendsto (fun t : ℝ ↦ Shape2GB2.logLik x ((t / r) ^ c⁻¹) c (p,t))
      atTop (𝓝 ((∑ i, Gamma.logDensity p r (y i)) + J))) ∧
    (Tendsto (fun t : ℝ ↦ Shape2GB2.logLik x ((r / t) ^ c⁻¹) c (t,q))
      atTop (𝓝 ((∑ i, Gamma.logDensity q r (y i)⁻¹) - ∑ i, 2 * log (y i) + J)))
```

### `UnivariateML.Endpoint3GeneralizedBeta.subunit_divergence_requires_contact`

At fixed shapes, every divergent strict-interior subunit-shape sequence approaches the sample maximum. Away from that endpoint the positive spike terms have a common finite upper bound. [Endpoint3GeneralizedBeta.lean](../../lean/UnivariateML/Family/Endpoint3GeneralizedBeta.lean#L34)

```lean
theorem subunit_divergence_requires_contact {ι : Type*} {l : Filter ι}
    {x : Fin n → ℝ} {M p q c : ℝ} (hx : ∀ i, 0 < x i) (hle : ∀ i, x i ≤ M)
    (hp : 0 < p) (hq : q < 1) (hc : 0 < c) {b : ι → ℝ}
    (hb : ∀ᶠ j in l, M < b j) (hdiv : Tendsto (fun j ↦ logLik x (b j) p q c) l atTop) :
    Tendsto b l (𝓝 M)
```

### `UnivariateML.Endpoint3GeneralizedBeta.fixed_shapes_endpoint_uniqueness`

The sole finite endpoint fit is sample contact at unit upper shape. Above one, a unique interior endpoint solves the normalized score. [Endpoint3GeneralizedBeta.lean](../../lean/UnivariateML/Family/Endpoint3GeneralizedBeta.lean#L71)

```lean
theorem fixed_shapes_endpoint_uniqueness (hn : 0 < n) {x : Fin n → ℝ} {M p q c : ℝ}
    (hx : ∀ i, 0 < x i) (hle : ∀ i, x i ≤ M) (heq : ∃ i, x i = M)
    (hp : 0 < p) (hc : 0 < c) :
    let L := fun b ↦ logLik x b p q c
    (q = 1 → IsMaxOn L (Ici M) M ∧ ∀ b ∈ Ici M, IsMaxOn L (Ici M) b → b = M) ∧
    (1 < q → (∃! b, b ∈ Ioi M ∧ IsMaxOn L (Ioi M) b) ∧
      ∀ b ∈ Ioi M, IsMaxOn L (Ioi M) b ↔
        Endpoint2BetaWidth.score (unitData x M c) q p ((M / b) ^ c) = 0)
```

### `UnivariateML.Endpoint3GeneralizedBeta.held_scale_first_shape_constant_path`

A constant positive sample strictly below the held endpoint concentrates along the written joint upper-shape and power ray with first shape fixed. [Endpoint3GeneralizedBeta.lean](../../lean/UnivariateML/Family/Endpoint3GeneralizedBeta.lean#L90)

```lean
theorem held_scale_first_shape_constant_path (hn : 0 < n) {v b p : ℝ}
    (hv : 0 < v) (hb : v < b) (hp : 0 < p) :
    Tendsto (fun c ↦ logLik (fun _ : Fin n ↦ v) b p
      (p * exp (c * log (b / v))) c) atTop atTop
```

### `UnivariateML.Endpoint3GeneralizedBeta.held_endpoint_gamma_path`

At a held endpoint, increasing the first shape while keeping its product with power equal to the rate gives the gamma face on log distances. The right side includes the Jacobian on the original data. [Endpoint3GeneralizedBeta.lean](../../lean/UnivariateML/Family/Endpoint3GeneralizedBeta.lean#L187)

```lean
theorem held_endpoint_gamma_path {x : Fin n → ℝ} {b q r : ℝ}
    (hx : ∀ i, 0 < x i ∧ x i < b) (hq : 0 < q) (hr : 0 < r) :
    let d := fun i ↦ log (b / x i)
    Tendsto (fun p ↦ logLik x b p q (r / p)) atTop
      (𝓝 (∑ i, (q * log r + (q - 1) * log (d i) -
        r * d i - log (Gamma q) - log (x i))))
```

### `UnivariateML.Endpoint3GeneralizedBeta.fixed_beta_shapes_joint_uniqueness`

At fixed beta shapes every attained endpoint and power fit is unique. Its endpoint is sample contact at unit upper shape and is strictly above the sample at greater upper shape. [Endpoint3GeneralizedBeta.lean](../../lean/UnivariateML/Family/Endpoint3GeneralizedBeta.lean#L234)

```lean
theorem fixed_beta_shapes_joint_uniqueness (hn : 0 < n) {x : Fin n → ℝ} {M p q : ℝ}
    (hx : ∀ i, 0 < x i) (hle : ∀ i, x i ≤ M) (heq : ∃ i, x i = M)
    (hp : 0 < p) :
    let D := Ioi (0 : ℝ) ×ˢ (if q = 1 then Ici M else Ioi M)
    let L := fun v : ℝ × ℝ ↦ logLik x v.2 p q v.1
    (∃ v ∈ D, IsMaxOn L D v) →
      ∃! v, v ∈ D ∧ IsMaxOn L D v ∧ (v.2 = M ↔ q = 1)
```

### `UnivariateML.Endpoint3GeneralizedBetaHeldPower.regular_superlevel_compact`

With held regular upper shape and held power, every finite superlevel is compact in first-shape and physical endpoint coordinates on nonconstant positive data. Unit upper shape includes closed contact. [Endpoint3GeneralizedBetaHeldPower.lean](../../lean/UnivariateML/Family/Endpoint3GeneralizedBetaHeldPower.lean#L112)

```lean
theorem regular_superlevel_compact (hn : 0 < n) {x : Fin n → ℝ} {M q c : ℝ}
    (hx : ∀ i, 0 < x i) (hle : ∀ i, x i ≤ M) (heq : ∃ i, x i = M)
    (hne : ∃ i, x i < M) (hq : 1 ≤ q) (hc : 0 < c) (C : ℝ) :
    let D := Ioi (0 : ℝ) ×ˢ (if q = 1 then Ici M else Ioi M)
    let L := fun v : ℝ × ℝ ↦ logLik x v.2 v.1 q c
    IsCompact {v ∈ D | C ≤ L v}
```

### `UnivariateML.Endpoint3GeneralizedBetaHeldPower.constant_first_shape_path`

On constant positive samples, a held upper shape and power allow concentration as the first shape increases and the endpoint approaches the common value from above. [Endpoint3GeneralizedBetaHeldPower.lean](../../lean/UnivariateML/Family/Endpoint3GeneralizedBetaHeldPower.lean#L270)

```lean
theorem constant_first_shape_path (hn : 0 < n) {v q c : ℝ}
    (hv : 0 < v) (hq : 0 < q) (hc : 0 < c) :
    Tendsto (fun p ↦ logLik (fun _ : Fin n ↦ v) (v / (1 - p⁻¹) ^ c⁻¹) p q c)
      atTop atTop
```

### `UnivariateML.Endpoint3GeneralizedBetaHeldPower.criterion`

With fixed upper shape and power, a finite positive maximum exists exactly for nonconstant positive data and upper shape at least one. The regular nonconstant branch has compact finite superlevels. [Endpoint3GeneralizedBetaHeldPower.lean](../../lean/UnivariateML/Family/Endpoint3GeneralizedBetaHeldPower.lean#L319)

```lean
theorem criterion {x : Fin n → ℝ} {M q c : ℝ}
    (hx : ∀ i, 0 < x i) (hle : ∀ i, x i ≤ M) (heq : ∃ i, x i = M)
    (hq : 0 < q) (hc : 0 < c) :
    let D := Ioi (0 : ℝ) ×ˢ (if q = 1 then Ici M else Ioi M)
    let L := fun v : ℝ × ℝ ↦ logLik x v.2 v.1 q c
    ((∃ v ∈ D, IsMaxOn L D v) ↔ 1 ≤ q ∧ ∃ i, x i < M) ∧
      ((q < 1 ∨ ∀ i, x i = M) → ¬ BddAbove (L '' D)) ∧
      ((1 ≤ q ∧ ∃ i, x i < M) → ∀ C, IsCompact {v ∈ D | C ≤ L v})
```

### `UnivariateML.Endpoint3GeneralizedBetaSingular.divergent_sequences_require_subunit_contact`

At fixed positive first shape and power, any divergent interior likelihood sequence eventually has subunit upper shape and approaches the sample maximum. The bounds are uniform over upper shape and apply also to constants and samples with ties at the maximum. [Endpoint3GeneralizedBetaSingular.lean](../../lean/UnivariateML/Family/Endpoint3GeneralizedBetaSingular.lean#L90)

```lean
theorem divergent_sequences_require_subunit_contact {ι : Type*} {l : Filter ι}
    {n : ℕ} {x : Fin n → ℝ} {M p c : ℝ} (hx : ∀ i, 0 < x i)
    (hle : ∀ i, x i ≤ M) (heq : ∃ i, x i = M)
    (hp : 0 < p) (hc : 0 < c) {b q : ι → ℝ}
    (hb : ∀ᶠ j in l, M < b j) (hq : ∀ᶠ j in l, 0 < q j)
    (hdiv : Tendsto (fun j ↦ logLik x (b j) p (q j) c) l atTop) :
    (∀ᶠ j in l, q j < 1) ∧ Tendsto b l (𝓝 M)
```

### `UnivariateML.Endpoint3InversePareto.inverse_exponential_failure`

When the finite inverse Pareto witness test fails, the fitted inverse exponential value is a strict least upper bound, reached in the limit along the fitted-rate path. This includes constant positive samples. [Endpoint3InversePareto.lean](../../lean/UnivariateML/Family/Endpoint3InversePareto.lean#L22)

```lean
theorem inverse_exponential_failure {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i)
    (hw : ¬ ∃ u ∈ Ioo 0 (Lomax.uBar (Transfer.reciprocal x)),
      Lomax.psi (Transfer.reciprocal x) u ≤ 0) :
    let L := fun p : ℝ × ℝ ↦ Transfer.InvPareto.logLik x p.1 p.2
    let r := (Lomax.avg (Transfer.reciprocal x))⁻¹
    let C := Transfer.InvPareto.bestInvExp x
    IsLUB (L '' Lomax.domain) C ∧
      (∀ p ∈ Lomax.domain, L p < C) ∧
      Tendsto (fun k ↦ L (k, r / k)) atTop (𝓝 C)
```

### `UnivariateML.Endpoint3LomaxTransfer.paretoII_two_level_unique`

An attained two-level Pareto II fit at a held threshold is unique. [Endpoint3LomaxTransfer.lean](../../lean/UnivariateML/Family/Endpoint3LomaxTransfer.lean#L51)

```lean
theorem paretoII_two_level_unique (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ}
    (hx : ∀ i, μ < x i) (h2 : ∃ a b : ℝ, ∀ i, x i = a ∨ x i = b) :
    let L := fun v : ℝ × ℝ ↦ Transfer.ParetoII.logLik x μ v.1 v.2
    (∃ v ∈ Lomax.domain, IsMaxOn L Lomax.domain v) →
      ∃! v, v ∈ Lomax.domain ∧ IsMaxOn L Lomax.domain v
```

### `UnivariateML.Endpoint3LomaxTransfer.inverse_pareto_two_level_unique`

An attained two-level inverse Pareto fit is unique. [Endpoint3LomaxTransfer.lean](../../lean/UnivariateML/Family/Endpoint3LomaxTransfer.lean#L67)

```lean
theorem inverse_pareto_two_level_unique (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) (h2 : ∃ a b : ℝ, ∀ i, x i = a ∨ x i = b) :
    let L := fun v : ℝ × ℝ ↦ Transfer.InvPareto.logLik x v.1 v.2
    (∃ v ∈ Lomax.domain, IsMaxOn L Lomax.domain v) →
      ∃! v, v ∈ Lomax.domain ∧ IsMaxOn L Lomax.domain v
```

### `UnivariateML.Endpoint3LomaxTransfer.paretoIV_two_level_unique`

An attained two-level Pareto IV fit at held threshold and power is unique. Raising the positive excesses to the held power preserves the two levels and transfers to the Lomax singleton. [Endpoint3LomaxTransfer.lean](../../lean/UnivariateML/Family/Endpoint3LomaxTransfer.lean#L81)

```lean
theorem paretoIV_two_level_unique (hn : 0 < n) {x : Fin n → ℝ} {μ c : ℝ}
    (hx : ∀ i, μ < x i) (hc : 0 < c) (h2 : ∃ a b : ℝ, ∀ i, x i = a ∨ x i = b) :
    let L := fun v : ℝ × ℝ ↦ ShapeFacesParetoIV.logLik x μ v.1 c v.2
    (∃ v ∈ Lomax.domain, IsMaxOn L Lomax.domain v) →
      ∃! v, v ∈ Lomax.domain ∧ IsMaxOn L Lomax.domain v
```

### `UnivariateML.Endpoint3NonstandardBeta.fixed_shapes_failure`

With both endpoints free, a subunit shape permits a spike and a constant sample permits concentration. The two mechanisms both give unbounded likelihood using strictly containing endpoints. [Endpoint3NonstandardBeta.lean](../../lean/UnivariateML/Family/Endpoint3NonstandardBeta.lean#L26)

```lean
theorem fixed_shapes_failure (hn : 0 < n) (x : Fin n → ℝ) {a b : ℝ}
    (h : (a < 1 ∨ b < 1) ∨ ∃ v, ∀ i, x i = v) :
    let D : Set (ℝ × ℝ) := {p | p.1 < p.2 ∧ ∀ i, x i ∈ Ioo p.1 p.2}
    let L : ℝ × ℝ → ℝ := fun p ↦ NonstandardBeta.logLik x p.1 p.2 (a, b)
    ¬ BddAbove (L '' D)
```

### `UnivariateML.Endpoint3NonstandardBeta.fixed_shapes_criterion`

A positive finite endpoint fit exists exactly for nonconstant data with both shapes at least one. Endpoints may touch their sample extremes at the corresponding unit shape. [Endpoint3NonstandardBeta.lean](../../lean/UnivariateML/Family/Endpoint3NonstandardBeta.lean#L45)

```lean
theorem fixed_shapes_criterion (hn : 0 < n) {x : Fin n → ℝ} {m M a b : ℝ}
    (hx : ∀ i, x i ∈ Icc m M) (hmin : ∃ i, x i = m) (hmax : ∃ i, x i = M) :
    let D := {v ∈ Endpoint2NonstandardBetaAffine.endpointDomain m M a b | v.1 < v.2}
    let L := fun v : ℝ × ℝ ↦ NonstandardBeta.logLik x v.1 v.2 (a, b)
    (∃ v ∈ D, IsMaxOn L D v) ↔ m < M ∧ 1 ≤ a ∧ 1 ≤ b
```

### `UnivariateML.Endpoint3NonstandardBeta.fixed_width_shapes_failure`

When one end and both shapes are held, the negative-distance and held-contact cases are tested before finite likelihood. With positive held-end factors, subunit free-end shape spikes at a positive maximum distance, and zero distances at held shape one concentrate by shrinking the width. Distances are measured inward from either held endpoint. [Endpoint3NonstandardBeta.lean](../../lean/UnivariateML/Family/Endpoint3NonstandardBeta.lean#L78)

```lean
theorem fixed_width_shapes_failure (hn : 0 < n) {z : Fin n → ℝ} {a b Z : ℝ}
    (hle : ∀ i, z i ≤ Z) (hmax : ∃ i, z i = Z) :
    let F := Endpoint2NonstandardBetaContacts.strictWidthLikelihood z a b
    let L := Endpoint2NonstandardBeta.widthLogLik z a b
    let D := Endpoint2NonstandardBeta.finiteWidthDomain z a b
    ((∃ i, z i < 0) → ∀ W, F W = 0) ∧
    ((∀ i, 0 ≤ z i) → (∃ i, z i = 0) → ∀ W > max Z 0,
      (b < 1 → F W = ⊤) ∧ (1 < b → F W = 0) ∧
      (b = 1 → 0 < F W ∧ F W < ⊤)) ∧
    (Endpoint2NonstandardBeta.heldCompatible z b →
      ((a < 1 ∧ 0 < Z) ∨ Z = 0) → ¬ BddAbove (L '' D))
```

### `UnivariateML.Endpoint3NonstandardBeta.held_endpoint_fixed_free_shape_failure`

With the free-end shape held, negative distances are incompatible, held-end contact permits infinite likelihood, and a positive constant sample or subunit free-end shape gives unbounded likelihood. The inward-distance formulation covers either held endpoint. [Endpoint3NonstandardBeta.lean](../../lean/UnivariateML/Family/Endpoint3NonstandardBeta.lean#L171)

```lean
theorem held_endpoint_fixed_free_shape_failure {z : Fin n → ℝ} {Z a : ℝ}
    (hle : ∀ i, z i ≤ Z) (hmax : ∃ i, z i = Z) (ha : 0 < a) :
    let D := {v : ℝ × ℝ | 0 < v.1 ∧
      v.2 ∈ Endpoint2NonstandardBeta.finiteWidthDomain z a v.1}
    let L := fun v : ℝ × ℝ ↦ Endpoint2NonstandardBeta.widthLogLik z a v.1 v.2
    ((∃ i, z i < 0) → D = ∅) ∧
    ((∀ i, 0 ≤ z i) → (∃ i, z i = 0) → ∀ W > max Z 0,
      Endpoint2NonstandardBetaContacts.strictWidthLikelihood z a (1 / 2) W = ⊤) ∧
    ((∀ i, 0 < z i) → (a < 1 ∨ ∀ i, z i = Z) → ¬ BddAbove (L '' D))
```

### `UnivariateML.Endpoint3NonstandardBeta.held_endpoint_fixed_free_shape_criterion`

The free width and held-end shape admit a finite fit exactly when all inward distances are positive and nonconstant and the held free-end shape is at least one. The strict-width test excludes infinite held-end contact before comparison on the finite domain. [Endpoint3NonstandardBeta.lean](../../lean/UnivariateML/Family/Endpoint3NonstandardBeta.lean#L209)

```lean
theorem held_endpoint_fixed_free_shape_criterion {z : Fin n → ℝ} {Z a : ℝ}
    (hle : ∀ i, z i ≤ Z) (hmax : ∃ i, z i = Z) (ha : 0 < a) :
    let D := {v : ℝ × ℝ | 0 < v.1 ∧
      v.2 ∈ Endpoint2NonstandardBeta.finiteWidthDomain z a v.1}
    let L := fun v : ℝ × ℝ ↦ Endpoint2NonstandardBeta.widthLogLik z a v.1 v.2
    let FiniteContact := ∀ b > 0, ∀ W > max Z 0,
      Endpoint2NonstandardBetaContacts.strictWidthLikelihood z a b W < ⊤
    (FiniteContact ∧ ∃ v ∈ D, IsMaxOn L D v) ↔
      (∀ i, 0 < z i) ∧ (∃ i, z i < Z) ∧ 1 ≤ a
```

### `UnivariateML.Endpoint3NonstandardBeta.held_endpoints_constant_shape_ray`

With both endpoints held around a common interior observation, the fixed-mean two-shape ray has unbounded sample likelihood. [Endpoint3NonstandardBeta.lean](../../lean/UnivariateML/Family/Endpoint3NonstandardBeta.lean#L264)

```lean
theorem held_endpoints_constant_shape_ray (hn : 0 < n) {l r x : ℝ}
    (hl : l < x) (hr : x < r) :
    let u := (x - l) / (r - l)
    Tendsto (fun t ↦ NonstandardBeta.logLik (fun _ : Fin n ↦ x) l r
      (t * u, t * (1 - u))) atTop atTop
```

### `UnivariateML.Endpoint3NonstandardBeta.one_shape_fixed_failure`

With both endpoints free, holding either positive shape still leaves a subunit shape available at the other endpoint. It gives an unbounded likelihood on every nonempty sample. [Endpoint3NonstandardBeta.lean](../../lean/UnivariateML/Family/Endpoint3NonstandardBeta.lean#L280)

```lean
theorem one_shape_fixed_failure (hn : 0 < n) (x : Fin n → ℝ) (b : ℝ) :
    let D : Set (ℝ × ℝ) := {p | p.1 < p.2 ∧ ∀ i, x i ∈ Ioo p.1 p.2}
    (¬ BddAbove ((fun p : ℝ × ℝ ↦ NonstandardBeta.logLik x p.1 p.2 (b, 1 / 2)) '' D)) ∧
      (¬ BddAbove ((fun p : ℝ × ℝ ↦ NonstandardBeta.logLik x p.1 p.2 (1 / 2, b)) '' D))
```

### `UnivariateML.Endpoint3NonstandardBeta.free_end_shape_failure`

If the moving-end shape is free, one subunit choice suffices. Negative distances give zero likelihood, a tied held endpoint has the zero, finite, or infinite factor dictated by its shape, and positive held-end factors permit an unbounded moving-end likelihood. [Endpoint3NonstandardBeta.lean](../../lean/UnivariateML/Family/Endpoint3NonstandardBeta.lean#L291)

```lean
theorem free_end_shape_failure (hn : 0 < n) {z : Fin n → ℝ} {Z : ℝ}
    (hle : ∀ i, z i ≤ Z) (hmax : ∃ i, z i = Z) :
    let F := fun b W ↦ Endpoint2NonstandardBetaContacts.strictWidthLikelihood z (1 / 2) b W
    ((∃ i, z i < 0) → ∀ b W, F b W = 0) ∧
    ((∀ i, 0 ≤ z i) → (∃ i, z i = 0) → ∀ b W, max Z 0 < W →
      (b < 1 → F b W = ⊤) ∧ (1 < b → F b W = 0) ∧
        (b = 1 → 0 < F b W ∧ F b W < ⊤)) ∧
    (∀ b, Endpoint2NonstandardBeta.heldCompatible z b →
      ¬ BddAbove (Endpoint2NonstandardBeta.widthLogLik z (1 / 2) b ''
        Endpoint2NonstandardBeta.finiteWidthDomain z (1 / 2) b))
```

### `UnivariateML.Endpoint3ParetoII.threshold_contact_iff`

At held scale and shape the fitted Pareto II threshold is exactly the sample minimum, represented here by zero threshold gap. [Endpoint3ParetoII.lean](../../lean/UnivariateML/Family/Endpoint3ParetoII.lean#L28)

```lean
theorem threshold_contact_iff {n : ℕ} {d : Fin n → ℝ}
    (hd : ∀ i, 0 ≤ d i) (hz : ∃ i, d i = 0) {q s : ℝ} (hq : 0 < q) (hs : 0 < s)
    {t : ℝ} (ht : 0 ≤ t) :
    IsMaxOn (fun u ↦ Transfer.ParetoII.logLik d (-u) q s) (Ici 0) t ↔ t = 0
```

### `UnivariateML.Endpoint3ParetoIII.held_scale_shape_threshold`

With both Pareto III regular coefficients held, the unique threshold is contact at unit shape and the unique normalized-score root is the maximizer above unit shape. [Endpoint3ParetoIII.lean](../../lean/UnivariateML/Family/Endpoint3ParetoIII.lean#L36)

```lean
theorem held_scale_shape_threshold {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hz : ∃ i, d i = 0) {c s : ℝ} (hc : 0 < c) (hs : 0 < s) :
    let L := fun t ↦ ParetoIII.logLik d (-t) (c, s)
    let S := Endpoint2FellerParetoRegular.normalizedScore d c c c s
    (c = 1 → IsMaxOn L (Ici 0) 0 ∧ ∀ t ∈ Ici 0, IsMaxOn L (Ici 0) t → t = 0) ∧
    (1 < c → (∃! t, t ∈ Ioi 0 ∧ S t = 0) ∧
      ∀ t ∈ Ioi 0, IsMaxOn L (Ioi 0) t ↔ S t = 0)
```

### `UnivariateML.Endpoint3ParetoIII.held_scale_failure`

With Pareto III scale held and shape free, a feasible subunit shape gives unbounded likelihood along strictly containing thresholds. [Endpoint3ParetoIII.lean](../../lean/UnivariateML/Family/Endpoint3ParetoIII.lean#L50)

```lean
theorem held_scale_failure {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hz : ∃ i, d i = 0) {s : ℝ} (hs : 0 < s) :
    ∀ B : ℝ, ∃ t > 0, ∃ c > 0, B < ParetoIII.logLik d (-t) (c, s)
```

### `UnivariateML.Endpoint3ParetoIII.held_shape_failure`

With Pareto III shape held, either the subunit spike or negative collapse exponent gives divergence. The regular zero-exponent face has a strictly unattained finite likelihood supremum. [Endpoint3ParetoIII.lean](../../lean/UnivariateML/Family/Endpoint3ParetoIII.lean#L63)

```lean
theorem held_shape_failure {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hz : ∃ i, d i = 0) {c : ℝ} (hc : 0 < c) :
    let D := Endpoint2FellerParetoFreeScale.domain c
    let L := fun v : ℝ × ℝ ↦ ParetoIII.logLik d (-v.2) (c, v.1)
    let E := Endpoint2FellerParetoFreeScale.exponent d c
    ((c < 1 ∨ E < 0) → ¬ BddAbove (L '' D)) ∧
      ((1 ≤ c ∧ E = 0) → ∃ C : ℝ, IsLUB (L '' D) C ∧ ∀ v ∈ D, L v < C)
```

### `UnivariateML.Endpoint3ParetoIII.held_shape_unique`

When the held Pareto III shape admits a finite fit, its scale and threshold are unique. The threshold reaches the minimum exactly at unit shape. [Endpoint3ParetoIII.lean](../../lean/UnivariateML/Family/Endpoint3ParetoIII.lean#L80)

```lean
theorem held_shape_unique {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hz : ∃ i, d i = 0) {c : ℝ} (hc : 0 < c) (ha : 1 ≤ c)
    (hE : 0 < Endpoint2FellerParetoFreeScale.exponent d c) :
    let D := Endpoint2FellerParetoFreeScale.domain c
    let L := fun v : ℝ × ℝ ↦ ParetoIII.logLik d (-v.2) (c, v.1)
    ∃! v, v ∈ D ∧ IsMaxOn L D v ∧ (v.2 = 0 ↔ c = 1)
```

### `UnivariateML.Endpoint3ParetoIV.held_scale_power_failure`

At fixed scale and power, either a subunit power or a constant sample makes the objective unbounded. This holds on strictly positive threshold gaps, so including the finite unit-power contact gives the same conclusion. [Endpoint3ParetoIV.lean](../../lean/UnivariateML/Family/Endpoint3ParetoIV.lean#L64)

```lean
theorem held_scale_power_failure {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hz : ∃ i, d i = 0) {c s : ℝ} (hc : 0 < c) (hs : 0 < s)
    (h : c < 1 ∨ ∀ i, d i = 0) :
    ∀ B : ℝ, ∃ t > 0, ∃ k > 0, B < ShapeFacesParetoIV.logLik d (-t) k c s
```

### `UnivariateML.Endpoint3ParetoIV.fixed_scale_pareto_trace`

At fixed scale and exactly fixed shape product, a Pareto IV density below cutoff tends to zero, at cutoff to half the Pareto density, and above cutoff to the Pareto density. Products retain every tied factor. [Endpoint3ParetoIV.lean](../../lean/UnivariateML/Family/Endpoint3ParetoIV.lean#L164)

```lean
theorem fixed_scale_pareto_trace {x : Fin n → ℝ} {μ r s : ℝ}
    (hx : ∀ i, μ < x i) (hr : 0 < r) (hs : 0 < s) :
    let y := fun i ↦ x i - μ
    Tendsto (fun c : ℝ ↦ exp (ShapeFacesParetoIV.logLik x μ (r / c) c s)) atTop
      (𝓝 (∏ i, if y i < s then 0 else if y i = s then r / (2 * y i)
        else r / y i * exp (-(r * log (y i / s)))))
```

### `UnivariateML.Endpoint3ParetoIV.threshold_likelihood_transfer`

On strictly containing thresholds the regular kernel agrees with the physical Pareto IV likelihood. At feasible unit-power contact the same kernel evaluates the continuous density from inside support. [Endpoint3ParetoIV.lean](../../lean/UnivariateML/Family/Endpoint3ParetoIV.lean#L195)

```lean
theorem threshold_likelihood_transfer {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    {k c s t : ℝ} (_hk : 0 < k) (hc : 0 < c) (hs : 0 < s) (ht : 0 < t) :
    ShapeFacesParetoIV.logLik d (-t) k c s = thresholdLogLik d k c s t
```

### `UnivariateML.Endpoint3ParetoIV.structural_spike_and_collapse`

Both structural Pareto IV rays are feasible. A subunit lower power permits threshold spiking at every positive held scale and upper shape. Free scale and upper shape also permit the simultaneous collapse at every positive power. [Endpoint3ParetoIV.lean](../../lean/UnivariateML/Family/Endpoint3ParetoIV.lean#L214)

```lean
theorem structural_spike_and_collapse {d : Fin n → ℝ}
    (hd : ∀ i, 0 ≤ d i) (hz : ∃ i, d i = 0) :
    (∀ k c s : ℝ, 0 < k → 0 < c → c < 1 → 0 < s →
      Tendsto (fun t ↦ ShapeFacesParetoIV.logLik d (-t) k c s) (𝓝[>] 0) atTop) ∧
    (∀ c : ℝ, 0 < c → Tendsto
      (fun T ↦ ShapeFacesParetoIV.logLik d (-exp (-T)) T⁻¹ c (exp (-T))) atTop atTop)
```

### `UnivariateML.Endpoint3ParetoIV.held_scale_power_criterion`

At fixed scale and power, Pareto IV attains exactly on nonconstant samples with power at least one, including closed unit-power contact. [Endpoint3ParetoIV.lean](../../lean/UnivariateML/Family/Endpoint3ParetoIV.lean#L225)

```lean
theorem held_scale_power_criterion {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hz : ∃ i, d i = 0) {c s : ℝ} (hc : 0 < c) (hs : 0 < s) :
    let D := Ioi (0 : ℝ) ×ˢ (if c = 1 then Ici (0 : ℝ) else Ioi 0)
    let L := fun v : ℝ × ℝ ↦ thresholdLogLik d v.1 c s v.2
    (∃ v ∈ D, IsMaxOn L D v) ↔ 1 ≤ c ∧ ∃ i, 0 < d i
```

### `UnivariateML.Endpoint3ParetoIV.held_scale_shapes_threshold`

With Pareto IV scale and shapes held, unit power maximizes at closed threshold contact. Higher power maximizes at the unique interior normalized-score root. [Endpoint3ParetoIV.lean](../../lean/UnivariateML/Family/Endpoint3ParetoIV.lean#L258)

```lean
theorem held_scale_shapes_threshold {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hz : ∃ i, d i = 0) {k c s : ℝ} (hk : 0 < k) (hc : 0 < c) (hs : 0 < s) :
    let L := thresholdLogLik d k c s
    let S := Endpoint2FellerParetoRegular.normalizedScore d c (c * k) c s
    (c = 1 → IsMaxOn L (Ici 0) 0 ∧ ∀ t ∈ Ici 0, IsMaxOn L (Ici 0) t → t = 0) ∧
    (1 < c → (∃! t, t ∈ Ioi 0 ∧ S t = 0) ∧
      ∀ t ∈ Ioi 0, IsMaxOn L (Ioi 0) t ↔ S t = 0)
```

### `UnivariateML.Endpoint3ParetoIV.held_shapes_failure`

With the Pareto IV shapes held and scale free, subunit power or negative collapse exponent makes the likelihood unbounded. At regular power and zero exponent the finite envelope is strictly unattained. [Endpoint3ParetoIV.lean](../../lean/UnivariateML/Family/Endpoint3ParetoIV.lean#L270)

```lean
theorem held_shapes_failure {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hz : ∃ i, d i = 0) {k c : ℝ} (hk : 0 < k) (hc : 0 < c) :
    let D := Endpoint2FellerParetoFreeScale.domain c
    let L := fun v : ℝ × ℝ ↦ thresholdLogLik d k c v.1 v.2
    let E := Endpoint2FellerParetoFreeScale.exponent d (c * k)
    ((c < 1 ∨ E < 0) → ¬ BddAbove (L '' D)) ∧
      ((1 ≤ c ∧ E = 0) → ∃ C : ℝ, IsLUB (L '' D) C ∧ ∀ v ∈ D, L v < C)
```

### `UnivariateML.Endpoint3ParetoIV.held_scale_constant_power_path`

At a held threshold and scale, a constant positive excess at or below scale gives unbounded likelihood along diverging power and upper shape equal to the reciprocal powered excess ratio. [Endpoint3ParetoIV.lean](../../lean/UnivariateML/Family/Endpoint3ParetoIV.lean#L282)

```lean
theorem held_scale_constant_power_path (hn : 0 < n) {μ x s : ℝ}
    (hx : μ < x) (hs : 0 < s) (hle : x - μ ≤ s) :
    Tendsto (fun c ↦ ShapeFacesParetoIV.logLik (fun _ : Fin n ↦ x) μ
      (((x - μ) / s) ^ c)⁻¹ c s) atTop atTop
```

### `UnivariateML.Endpoint3ParetoIV.constant_excess_fixed_upper_shape_path`

At a constant positive excess, a fixed positive upper shape permits divergence as the power grows with scale equal to the common excess. The scale may be fixed at that value or chosen on the ray. [Endpoint3ParetoIV.lean](../../lean/UnivariateML/Family/Endpoint3ParetoIV.lean#L317)

```lean
theorem constant_excess_fixed_upper_shape_path (hn : 0 < n) {μ x k : ℝ}
    (hx : μ < x) :
    Tendsto (fun c ↦ ShapeFacesParetoIV.logLik (fun _ : Fin n ↦ x) μ k c (x - μ))
      atTop atTop
```

### `UnivariateML.Endpoint3ParetoIV.held_shapes_joint_unique`

When a finite Pareto IV fit with both shapes held exists, the joint scale and threshold are unique. The threshold reaches sample contact exactly at unit power. [Endpoint3ParetoIV.lean](../../lean/UnivariateML/Family/Endpoint3ParetoIV.lean#L336)

```lean
theorem held_shapes_joint_unique {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hz : ∃ i, d i = 0) {k c : ℝ} (hk : 0 < k) (hc : 0 < c) (ha : 1 ≤ c)
    (hE : 0 < Endpoint2FellerParetoFreeScale.exponent d (c * k)) :
    let D := Endpoint2FellerParetoFreeScale.domain c
    let L := fun v : ℝ × ℝ ↦ thresholdLogLik d k c v.1 v.2
    ∃! v, v ∈ D ∧ IsMaxOn L D v ∧ (v.2 = 0 ↔ c = 1)
```

### `UnivariateML.Endpoint3ParetoIV.held_scale_unit_power_unique`

At fixed scale and unit power, the Pareto IV optimum is unique on nonconstant data and has closed threshold contact. [Endpoint3ParetoIV.lean](../../lean/UnivariateML/Family/Endpoint3ParetoIV.lean#L350)

```lean
theorem held_scale_unit_power_unique {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hz : ∃ i, d i = 0) (hne : ∃ i, 0 < d i) {s : ℝ} (hs : 0 < s) :
    let D := Ioi (0 : ℝ) ×ˢ Ici (0 : ℝ)
    let L := fun v : ℝ × ℝ ↦ thresholdLogLik d v.1 1 s v.2
    ∃! v, v ∈ D ∧ IsMaxOn L D v ∧ v.2 = 0
```

### `UnivariateML.Endpoint3GammaRatio.log_gamma_ratio`

The fixed positive increment of log Gamma is asymptotic to the increment times the log of the argument. The two neighboring unit secants bound the intermediate secant by log convexity. [Endpoint3GammaRatio.lean](../../lean/UnivariateML/Generic/Endpoint3GammaRatio.lean#L27)

```lean
theorem log_gamma_ratio (p : ℝ) (hp : 0 < p) :
    Tendsto (fun x ↦ log (Gamma (x + p)) - log (Gamma x) - p * log x)
      atTop (𝓝 0)
```

### `UnivariateML.Endpoint3GammaRatio.log_gamma_ratio_at_zero`

At zero shape, the gamma normalizer ratio divided by the small shape has log limit zero. This uses the gamma recurrence at zero, so it applies to fixed increments of any positive size. [Endpoint3GammaRatio.lean](../../lean/UnivariateML/Generic/Endpoint3GammaRatio.lean#L66)

```lean
theorem log_gamma_ratio_at_zero {p : ℝ} (hp : 0 < p) :
    Tendsto (fun q : ℝ ↦ log (Gamma (p + q)) - log (Gamma q) -
      log q - log (Gamma p)) (𝓝[>] 0) (𝓝 0)
```
