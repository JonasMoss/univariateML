# TASK-489 elementary-family Lean proofs

The atlas has 31 written main-question claims in the selected families.
The checks below cover 25 public declarations across twelve modules.
The mapping fragment leaves every coverage field blank for independent review.
The target ledger records the work on each claim and the precise open proof steps.

## Source qualifications

- For GPD with threshold held and shape below minus one, upper contact at
  **positive scale** is possible when the maximum excess (M) is positive.
  With all excesses zero, the contact equation forces scale zero. The
  unboundedness conclusion still holds on that sample by scale collapse.
  With location free one can instead choose a threshold strictly below the
  common observation and obtain positive-scale upper contact.
  [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/gpd/README.md#details)
- For triangular with both valid endpoints held, zero likelihood for
  every mode leaves mathematical maximizers of value zero on the compact
  interval. The fitter's compatibility condition requires a positive
  maximum. The Lean statement formalizes that predicate on the likelihood.
  [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/triang/README.md#details)
- The Pareto II lower power is identically one. Its fitted threshold at
  fixed scale and shape is included, and a receding threshold has zero
  limiting likelihood. Pareto III has the subunit-shape threshold spike.
  [Endpoint calculations](2026-11-25-thresh-math-report.md#fixed-regular-coefficients)

## Target ledger

| Family | Held | Question | Lean work and remaining proof |
| --- | --- | --- | --- |
| hyper | all free | criterion | **Open.** The held-design comparison and existing uniform Poisson envelope supply pieces. The underdispersion grid cutoff, the bounded-rate quadratic exclusion at equidispersion, and positive-mean Poisson-height irrationality are missing from the joined grid test. |
| hyper | all free | failure | **Open.** The existing fixed-design binomial paths cover a fixed smaller design. The jointly growing Poisson convergence, rate-zero and infinite-rate exclusions, and exhaustion of the maximizing deterministic traces on constants are missing. |
| hyper | all free | uniqueness | **Open.** Existing constant saturation gives the deterministic sets. The dispersion-split grid proof and rational comparison and assembly of all face-equal winners remain, including a general exact mixed-binary saturation and population-ray theorem. |
| hyper | k | criterion | Proved the finite-height threshold criterion by reachable binomial faces and the existing strict-witness cutoff. **Open.** The two eventual offsets per residue still need the adjacent-ratio expansion at order N⁻² and its exact sign bounds. The existing affine face polynomial then decides equality on those rays. |
| hyper | k | uniqueness | **Open.** The residue offsets and deterministic and mixed-binary winner formulas must be joined to a complete maximizing-set procedure with all equality rays retained. The attainment theorem addresses presence of winners. |
| hyper | m | criterion | Proved transport of the height criterion via exact support and likelihood symmetry. **Open.** The finite prefix and residue test depends on the corresponding draws-held result. |
| hyper | m | uniqueness | **Open.** Transport of the full winner set, including the explicit even-population ray and arbitrary face equality, needs the draws-held set theorem. |
| nhyper | all free | criterion | Proved finite aggregation of order-theoretic slice heights and equality retention. Existing support results cover constants and incompatible mixed zeros. **Open.** To make the joined criterion effective, compute each positive stopping-count supremum using the prefix, residue signs and shifted negative-binomial face. |
| nhyper | all free | failure | **Open.** Existing support algebra excludes mixed zeros. The required population-divergence proof extracts a fixed stopping count and a convergent marked proportion, handles both proportions zero and one, and identifies all winning limits at the largest slice height. It must include the constant and zero branches. |
| nhyper | all free | uniqueness | Converted exact saturation to the full constant argmax, including the ((1,1)) example and all compatible zero-count parameters. On positive data a finite point wins exactly when it reaches the largest slice supremum. **Open.** A rational, terminating fixed-stopping-count comparison is needed to compute that set. |
| nhyper | m+r | failure | Extracted the flat zero-stop branch and the zero limit of the positive-stop branch from the existing likelihood-ratio result. The excess population coordinate is (N-m). |
| nhyper | r | criterion | **Open.** Need the two eventual composition offsets per residue, their rational face-comparison polynomials and their nonzero proof by root multiplicity. Include the deterministic and zero-stop cases, and retain finite ties with the face. |
| nhyper | r | failure | **Open.** Need the shifted negative-binomial face limit and global uniform deficit bound for every composition, with the strict winner decision following from the exact polynomial and finite prefix comparison. |
| nhyper | r | uniqueness | **Open.** Existing fixed-population ratios supply composition winners. A bound covering all winning populations and a single full-set theorem including the flat zero-stop branch still depend on the fixed-stop finite comparison. |
| gpd | mu | failure | Joined fitted collapse and positive-data attainment. Extended the subuniform interior divergence proof to ties with at least one positive excess, and used exact constant scale collapse for the all-zero sample. |
| gpd | mu+sigma | failure | Joined attainment, closed uniform contact, all-zero flatness, the infinite-shape exclusion for a positive excess, and the vanishing likelihood at finite nonuniform upper contact. |
| gpd | mu+sigma | uniqueness | **Open.** Existing all-zero flatness and closed-uniform uniqueness settle M ≤ s. At (M>s) the missing step is the derivative integral with kernel ((1+bt)^{-2}), followed by the strict kernel-ratio comparison across (t=s/M). |
| gpd | sigma | failure | Proved finite attainment and necessity of a constant sample with location tending to its common value for a positive infinite-shape trace. An explicit formula gives the finite scaled-location-gap limits. **Open in the expanded source analysis.** The infinite scaled-gap case, uniform receding-location bound, and full envelopes for tangential approaches to uniform contact remain to be joined. |
| gpd | sigma | uniqueness | Converted the feasible uniform set to the closed physical interval and included the constant-data shape ridge. **Open.** Beyond that interval the location improvement is available, but the nonuniform scalar shape singleton needs the same kernel-ratio proof as for held location and scale. |
| gpd | xi | failure | Joined constant collapse, negative-exponent divergence, the finite strictly unattained critical supremum, and subuniform interior unboundedness. The source also gives moving-threshold trace details and edge exclusions. The joined theorem states the main failure alternatives and critical global bound. |
| gpd | xi | uniqueness | Combined the fitted existence and uniqueness criterion with location profiling to the minimum. Added a closed reference support chart with infinite log likelihood at compatible upper contact, and proved the exact reference argmax including constants. |
| triang | all free | uniqueness | Proved the full knot argmax and uniqueness of every profiled endpoint pair, using the held-mode optimum and held-endpoint knot characterization. Constant collapse excludes every finite winner. **Open.** Need the monotone fitted height as a function of opposite-side count fraction on two levels, then the gradient separation and measure-zero tie theorem on strata with three or more distinct levels. |
| triang | a+b | failure | Proved compactness, equivalence of positive attainment to support compatibility, and an identically zero supported product for both kinds of incompatible sample. Lean uses the mathematical compatibility condition. |
| pareto2 | min | uniqueness | Joined the almost-everywhere chart, two-level transfer and exact exceptional sample at threshold zero. The fixed-count chart uses inverse scale and drops parameter-independent terms. |
| pareto2 | scale+shape | failure | Reused exact minimum contact and proved zero limiting probability product at a receding threshold by a uniform log-density bound across the observed excesses. |
| pareto3 | scale | failure | Exposed the subunit-shape blowup as a threshold limit at every positive held scale, using the existing threshold spike on minimum-centered data. |
| lomax | all free | uniqueness | Joined the fixed-count almost-everywhere result, the pointwise two-level theorem and the exact two-winner sample with its specified three levels. |
| invpareto | all free | uniqueness | Joined the fixed-count reciprocal chart and the two-level comparison. Transferred the exact Lomax tie and its face comparison to the explicit reciprocal sample. |
| binom | all free | uniqueness | Proved the exact positive-constant singleton by the binomial mass bound. Joined it with the zero axes, finite profile graph and dispersion test. **Open.** The adjacent likelihood ratio and single-crossing sufficiency, exclusion of nonadjacent ties, and prime-power valuation argument are still missing. |
| dgamma | all free | uniqueness | Proved finite candidate selection from analytic score and compact zero confinement, using isolated zeros and the stationary condition. **Open.** The Gamma-interval rate root must be proved analytic, and the two score tails must give the compact cutoff. The zero-cell variance, standardized covariance trace bound and explicit K ≥ 2²⁸ histogram theorem remain unformalized. |
| skellam | all free | uniqueness | Proved the positive-index Poisson supporting bound, its strict convolution consequence, reflection, and the supporting inequality for adjacent nonnegative counts. Existing results give constants and zero-mean one-magnitude uniqueness. **Open.** The one-magnitude theorem with unequal sign counts needs the positive-rate profile crossing, and the general analytic score needs its isolation and tail proof, including the compatible Poisson seam. |

## Validation

- Each new module passed its standalone Lean check on the workstation through
  the single-slot wrapper.
- The final aggregate `lake build` on the workstation completed successfully
  with 8514 jobs and all Base7 imports in scope. The temporary imports were
  removed and `git diff --exit-code dev/lean/UnivariateML.lean` passed.
- `python3 dev/tools/existence_atlas.py --check` passed.
- The new declarations have no proof placeholders.

## Checked declarations

### `Base7Hyper.held_draw_attains_iff`

**hyper · k.** Attainment is equivalent to a compatible finite urn reaching the optimized binomial height. All compatible draws-held samples. The finite prefix and residue algorithm remains open. [Lean source](../../lean/UnivariateML/Family/Base7Hyper.lean#L24)

```lean
theorem held_draw_attains_iff {h : ℕ} (hh : 0 < h) (x : Fin h → ℕ) (k : ℕ)
    (hx : ∀ i, x i ≤ k) {p : ℝ} (hp : p ∈ Icc 0 1)
    (hmax : IsMaxOn (fun q ↦ ∏ i, binomialFace k (x i) q) (Icc 0 1) p) :
    let D := {v : ℕ × ℕ | v.1 ≤ v.2 ∧ k ≤ v.2 ∧
      ∀ i, Compatible v.1 v.2 k (x i)}
    let L := fun v : ℕ × ℕ ↦ likelihood v.1 v.2 k x
    (∃ v ∈ D, IsMaxOn L D v) ↔ ∃ v ∈ D, (∏ i, binomialFace k (x i) p) ≤ L v
```

### `Base7Hyper.held_mark_attains_iff`

**hyper · m.** Attainment is equivalent to a compatible finite urn reaching the optimized binomial height. All compatible marked-count-held samples. The finite prefix and residue algorithm remains open. [Lean source](../../lean/UnivariateML/Family/Base7Hyper.lean#L51)

```lean
theorem held_mark_attains_iff {h : ℕ} (hh : 0 < h) (x : Fin h → ℕ) (m : ℕ)
    (hx : ∀ i, x i ≤ m) {p : ℝ} (hp : p ∈ Icc 0 1)
    (hmax : IsMaxOn (fun q ↦ ∏ i, binomialFace m (x i) q) (Icc 0 1) p) :
    let D := {v : ℕ × ℕ | v.1 ≤ v.2 ∧ m ≤ v.2 ∧
      ∀ i, Compatible m v.2 v.1 (x i)}
    let L := fun v : ℕ × ℕ ↦ likelihood m v.2 v.1 x
    (∃ v ∈ D, IsMaxOn L D v) ↔ ∃ v ∈ D, (∏ i, binomialFace m (x i) p) ≤ L v
```

### `Base7NHyper.held_mark_stop_failure`

**nhyper · m+r.** The zero-stop branch is flat at one and the positive-stop branch tends to zero with growing population. Population is the fixed marked count plus the unmarked count. [Lean source](../../lean/UnivariateML/Family/Base7NHyper.lean#L25)

```lean
theorem held_mark_stop_failure {h m r : ℕ} (hh : 0 < h) (x : Fin h → ℕ)
    (hf : ∃ j, ∀ i, Compatible (m + j) m r (x i)) :
    (r = 0 ∧ (∀ i, x i = 0) ∧ ∀ j, totalLikelihood x (m + j) m r = 1) ∨
    (0 < r ∧ Tendsto (fun j : ℕ ↦ totalLikelihood x (m + j) m r) atTop (𝓝 0))
```

### `Base7NHyper.constant_maximizers_iff`

**nhyper · all free.** The complete maximizing set on constant data is the saturated positive set or the zero-stop set. Constant samples, including zero. The nonconstant fixed-stopping-count slice algorithm remains open. [Lean source](../../lean/UnivariateML/Family/Base7NHyper.lean#L34)

```lean
theorem constant_maximizers_iff {h : ℕ} (hh : 0 < h) (N m r c : ℕ) :
    (∀ A B C, totalLikelihood (fun _ : Fin h ↦ c) A B C ≤
      totalLikelihood (fun _ : Fin h ↦ c) N m r) ↔
    m ≤ N ∧ r ≤ m ∧ r = c ∧ (c = 0 ∨ m = N)
```

### `Base7NHyper.positive_stopping_slice_maximizers`

**nhyper · all free.** A compatible finite urn on positive samples wins exactly when it reaches the largest of finitely many stopping-count slice suprema. Slice suprema are defined order-theoretically. An effective finite computation of each slice is still open. [Lean source](../../lean/UnivariateML/Family/Base7NHyper.lean#L56)

```lean
theorem positive_stopping_slice_maximizers {h : ℕ} (x : Fin h → ℕ) {i : Fin h}
    (hi : ∀ j, x i ≤ x j) (hx : 0 < x i) :
    let D := {p : ℕ × ℕ × ℕ | ∀ j, Compatible p.1 p.2.1 p.2.2 (x j)}
    let L := fun p : ℕ × ℕ × ℕ ↦ totalLikelihood x p.1 p.2.1 p.2.2
    let S := Finset.Icc 1 (x i)
    let V := fun r : ℕ ↦ {v : ℝ | ∃ N m, (N, m, r) ∈ D ∧ v = L (N, m, r)}
    ∃ r ∈ S, (∀ s ∈ S, sSup (V s) ≤ sSup (V r)) ∧
      ∀ p ∈ D, IsMaxOn L D p ↔ L p = sSup (V r)
```

### `Base7GPD.held_threshold_failure`

**gpd · mu.** Threshold ties give unbounded fitted collapse, positive excesses attain, and the unrestricted subuniform shape branch is unbounded. The reference branch uses finite interior log likelihood. [Lean source](../../lean/UnivariateML/Family/Base7GPD.lean#L96)

```lean
theorem held_threshold_failure {n : ℕ} (hn : 0 < n) (y : Fin n → ℝ)
    (hy : ∀ i, 0 ≤ y i) :
    ((∃ i, y i = 0) →
      (∀ t : ℝ, exp (-t) ∈ scaleDomain y (2 * n)) ∧
      Tendsto (fun t : ℝ ↦ logLik y (exp (-t)) (2 * n)) atTop atTop) ∧
    ((∀ i, 0 < y i) → ∃ s ξ, -1 ≤ ξ ∧ s ∈ scaleDomain y ξ ∧
      ∀ κ ≥ -1, ∀ t ∈ scaleDomain y κ, logLik y t κ ≤ logLik y s ξ) ∧
    (∀ ξ < -1, ∀ B : ℝ, ∃ s ∈ scaleDomain y ξ, B < logLik y s ξ)
```

### `Base7GPD.held_threshold_scale_failure`

**gpd · mu+sigma.** There is attainment, with vanishing likelihood at nonuniform upper contact and at infinite shape for any positive excess. Includes the flat all-zero sample and closed uniform contact. [Lean source](../../lean/UnivariateML/Family/Base7GPD.lean#L120)

```lean
theorem held_threshold_scale_failure {n : ℕ} (y : Fin n → ℝ)
    (hy : ∀ i, 0 ≤ y i) {s : ℝ} (hs : 0 < s) :
    let D := shapeDomain y s
    let L := logLik y s
    (∃ ξ ∈ D, IsMaxOn L D ξ) ∧
    ((-1 : ℝ) ∈ D ↔ ∀ i, y i ≤ s) ∧
    ((∀ i, y i = 0) → ∀ ξ ∈ D, L ξ = -(n : ℝ) * log s) ∧
    (∀ ξ : ℕ → ℝ, (∀ᶠ k in atTop, ξ k ∈ D) → Tendsto ξ atTop atTop →
      (∃ i, 0 < y i) → Tendsto (L ∘ ξ) atTop atBot) ∧
    (∀ (ξ : ℕ → ℝ) (κ : ℝ), -1 < κ →
      (∀ᶠ k in atTop, ξ k ∈ D) → Tendsto ξ atTop (𝓝 κ) →
      (∃ i, 0 < y i ∧ s + κ * y i = 0) → Tendsto (L ∘ ξ) atTop atBot)
```

### `Base7GPD.held_shape_maximizers`

**gpd · xi.** Finite interior fits follow the shape criterion and are unique at the sample minimum. The subuniform extended-real argmax is exactly the compatible upper-contact set. The reference chart explicitly gives compatible upper contacts infinite log likelihood. [Lean source](../../lean/UnivariateML/Family/Base7GPD.lean#L149)

```lean
theorem held_shape_maximizers {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) {i j : Fin n}
    (hi : ∀ k, x i ≤ x k) (hj : ∀ k, x k ≤ x j) (ξ : ℝ) :
    let D := {p : ℝ × ℝ | p.2 ∈ scaleDomain (fun k ↦ x k - p.1) ξ}
    let L := fun p : ℝ × ℝ ↦ logLik (fun k ↦ x k - p.1) p.2 ξ
    let R := {p : ℝ × ℝ | 0 < p.2 ∧
      ∀ k, 0 ≤ x k - p.1 ∧ 0 ≤ p.2 + ξ * (x k - p.1)}
    let E := fun p : ℝ × ℝ ↦
      if ∃ k, p.2 + ξ * (x k - p.1) = 0 then (⊤ : WithTop ℝ) else (L p : WithTop ℝ)
    ((∃ p ∈ D, IsMaxOn L D p) ↔
      -1 ≤ ξ ∧ x i < x j ∧ (ξ ≤ 0 ∨ 0 < collapseExponent (fun k ↦ x k - x i) ξ)) ∧
    (∀ p ∈ D, IsMaxOn L D p → p.1 = x i ∧
      ∀ q ∈ D, IsMaxOn L D q → q = p) ∧
    (ξ < -1 → ∀ p, (p ∈ R ∧ IsMaxOn E R p) ↔
      p.1 ≤ x i ∧ p.1 < x j ∧ p.2 = -ξ * (x j - p.1))
```

### `Base7GPD.held_shape_failure`

**gpd · xi.** Fixed-shape constant collapse and negative-exponent collapse diverge, critical collapse is finite and unattained, and the unrestricted subuniform interior is unbounded. The critical bound ranges over every finite location and scale. [Lean source](../../lean/UnivariateML/Family/Base7GPD.lean#L221)

```lean
theorem held_shape_failure {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) {i : Fin n}
    (hi : ∀ k, x i ≤ x k) (ξ : ℝ) :
    let y := fun k ↦ x k - x i
    let V := {v | ∃ m s, s ∈ scaleDomain (fun k ↦ x k - m) ξ ∧
      v = logLik (fun k ↦ x k - m) s ξ}
    ((∀ k, x k = x i) → Tendsto (fun s ↦ logLik y s ξ) (𝓝[>] 0) atTop) ∧
    (0 < ξ → collapseExponent y ξ < 0 →
      Tendsto (fun s ↦ logLik y s ξ) (𝓝[>] 0) atTop) ∧
    ((∃ k, x i < x k) → 0 < ξ → collapseExponent y ξ = 0 →
      let H := ∑ k, if y k = 0 then (0 : ℝ) else -(1 + 1 / ξ) * log (ξ * y k)
      IsLUB V H ∧ ∀ v ∈ V, v < H) ∧
    (ξ < -1 → ¬ BddAbove V)
```

### `Base7GPD.held_scale_failure`

**gpd · sigma.** Held scale always gives a finite optimum. An infinite-shape path with likelihood bounded below requires constant data and location converging to the common value. The positive trace necessity covers arbitrary compatible sequences. [Lean source](../../lean/UnivariateML/Family/Base7GPD.lean#L254)

```lean
theorem held_scale_failure {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) {s : ℝ} (hs : 0 < s) :
    (∃ m ξ, ξ ∈ shapeDomain (fun i ↦ x i - m) s ∧
      ∀ q κ, κ ∈ shapeDomain (fun i ↦ x i - q) s →
        logLik (fun i ↦ x i - q) s κ ≤ logLik (fun i ↦ x i - m) s ξ) ∧
    (∀ (m ξ : ℕ → ℝ) (B : ℝ),
      (∀ᶠ t in atTop, ξ t ∈ shapeDomain (fun i ↦ x i - m t) s) →
      Tendsto ξ atTop atTop →
      (∀ᶠ t in atTop, B ≤ logLik (fun i ↦ x i - m t) s (ξ t)) →
      ∃ M, (∀ i, x i = M) ∧ Tendsto m atTop (𝓝 M))
```

### `Base7GPD.held_scale_constant_infinite_shape_trace`

**gpd · sigma.** On constant data a finite limit of the scaled location gap gives the explicit infinite-shape log likelihood trace. Finite scaled-gap limits at held positive scale. [Lean source](../../lean/UnivariateML/Family/Base7GPD.lean#L296)

```lean
theorem held_scale_constant_infinite_shape_trace {n : ℕ} (M : ℝ) {s : ℝ} (hs : 0 < s)
    (m ξ : ℕ → ℝ) (hξ : Tendsto ξ atTop atTop) {v : ℝ} (hv : 0 ≤ v)
    (hgap : Tendsto (fun t ↦ ξ t * ((M - m t) / s)) atTop (𝓝 v)) :
    Tendsto (fun t ↦ logLik (fun _ : Fin n ↦ M - m t) s (ξ t)) atTop
      (𝓝 (-(n : ℝ) * (log s + log (1 + v))))
```

### `Base7GPD.held_scale_uniform_argmax`

**gpd · sigma.** The complete argmax on uniform-feasible data consists of the uniform location interval and the constant sample ridge. The nonuniform singleton for span above scale remains open. [Lean source](../../lean/UnivariateML/Family/Base7GPD.lean#L321)

```lean
theorem held_scale_uniform_argmax {n : ℕ} (x : Fin n → ℝ) {s : ℝ} (hs : 0 < s)
    {i j : Fin n} (hi : ∀ k, x i ≤ x k) (hj : ∀ k, x k ≤ x j) (hspan : x j - x i ≤ s) :
    let D := {p : ℝ × ℝ | p.2 ∈ shapeDomain (fun k ↦ x k - p.1) s}
    let L := fun p : ℝ × ℝ ↦ logLik (fun k ↦ x k - p.1) s p.2
    ∀ p, (p ∈ D ∧ IsMaxOn L D p) ↔
      (p.2 = -1 ∧ p.1 ∈ Icc (x j - s) (x i)) ∨
        (-1 ≤ p.2 ∧ p.1 = x i ∧ x i = x j)
```

### `Base7Triangular.held_endpoint_failure`

**triang · a+b.** With valid endpoints the mode interval is compact and the positive-likelihood compatibility test is exact. The compatibility predicate models the mathematical condition used by the API. [Lean source](../../lean/UnivariateML/Family/Base7Triangular.lean#L24)

```lean
theorem held_endpoint_failure {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {a b : ℝ} (hab : a < b) :
    let D := Icc a b
    let L := fun c ↦ supportLikelihood x (a, b, c)
    let compatible := (∀ i, x i ∈ D) ∧ ¬ ((∃ i, x i = a) ∧ (∃ i, x i = b))
    IsCompact D ∧
    ((∃ c ∈ D, IsMaxOn L D c ∧ 0 < L c) ↔ compatible) ∧
    (¬ compatible → ∀ c ∈ D, L c = 0)
```

### `Base7Triangular.all_free_knot_maximizers`

**triang · all free.** Constants have no winner. On nonconstant samples each observed knot has a unique endpoint fit and the highest knot fits are exactly the global winners. The two-level count comparison and count-stratum nullity remain open. [Lean source](../../lean/UnivariateML/Family/Base7Triangular.lean#L90)

```lean
theorem all_free_knot_maximizers {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) :
    let D := jointDomain x
    let L := jointLikelihood x
    let W := {p ∈ D | IsMaxOn L D p}
    ((∀ i j, x i = x j) → W = ∅) ∧
    ((∃ i j, x i ≠ x j) →
      ∃ w : Fin n → ℝ × ℝ,
        (∀ i, w i ∈ widthDomain x (x i) ∧
          IsMaxOn (widthLikelihood x (x i)) (widthDomain x (x i)) (w i) ∧
          ∀ v ∈ widthDomain x (x i),
            IsMaxOn (widthLikelihood x (x i)) (widthDomain x (x i)) v → v = w i) ∧
        W = {p | ∃ i, p = (x i - (w i).1, x i + (w i).2, x i) ∧
          ∀ j, widthLikelihood x (x j) (w j) ≤ widthLikelihood x (x i) (w i)})
```

### `Base7ParetoII.held_scale_shape_failure`

**pareto2 · scale+shape.** At held positive scale and shape, lower contact at the sample minimum attains and the receding threshold tends to zero likelihood. The minimum is shifted to zero and the shift variable is the threshold gap. [Lean source](../../lean/UnivariateML/Family/Base7ParetoII.lean#L20)

```lean
theorem held_scale_shape_failure {n : ℕ} {d : Fin n → ℝ}
    (hd : ∀ i, 0 ≤ d i) (hz : ∃ i, d i = 0) {q s : ℝ} (hq : 0 < q) (hs : 0 < s) :
    let L := fun t ↦ ParetoII.logLik d (-t) q s
    IsMaxOn L (Ici 0) 0 ∧ Tendsto (fun t ↦ exp (L t)) atTop (𝓝 0)
```

### `Base7ParetoIIMin.uniqueness`

**pareto2 · min.** On positive excesses, the almost-everywhere and pointwise two-level uniqueness hold, with a two-winner example at threshold zero. The almost-everywhere branch uses the inverse-scale coordinate. [Lean source](../../lean/UnivariateML/Family/Base7ParetoIIMin.lean#L73)

```lean
theorem uniqueness (μ : ℝ) :
    (∀ {n : ℕ} (w : Fin n → ℕ), (∀ i, 0 < w i) →
      ∀ᵐ x : Fin n → ℝ ∂volume, (∃ i j, x i ≠ x j) →
        ∀ p q, UniqueAE.Winner (AE2Lomax.domain μ) (AE2Lomax.likelihood (1) μ w) x p →
          UniqueAE.Winner (AE2Lomax.domain μ) (AE2Lomax.likelihood (1) μ w) x q → p = q) ∧
    (∀ {n : ℕ}, 0 < n → ∀ x : Fin n → ℝ, (∀ i, μ < x i) →
      (∃ a b : ℝ, ∀ i, x i = a ∨ x i = b) →
      let L := fun v : ℝ × ℝ ↦ Transfer.ParetoII.logLik x μ v.1 v.2
      (∃ v ∈ Lomax.domain, IsMaxOn L Lomax.domain v) →
        ∃! v, v ∈ Lomax.domain ∧ IsMaxOn L Lomax.domain v) ∧
    (
    ∃ a ∈ Ioo ((2 : ℝ) ^ 512)⁻¹ ((2 : ℝ) ^ 32)⁻¹,
      let x : Fin 21 → ℝ := fun i ↦ if i.val = 0 then a else if i.val ≤ 10 then 1 else 100
      (∀ i, 0 < x i) ∧ ∃ p q : ℝ × ℝ, p.2 < q.2 ∧
        {v ∈ Lomax.domain | IsMaxOn (fun b : ℝ × ℝ ↦
          Transfer.ParetoII.logLik x 0 b.1 b.2) Lomax.domain v} = {p, q} ∧
        Transfer.ParetoII.bestShiftedExp x 0 < Transfer.ParetoII.logLik x 0 q.1 q.2)
```

### `Base7ParetoIII.held_scale_threshold_collapse`

**pareto3 · scale.** Any positive subunit shape yields a divergent threshold approach at each positive held scale. The minimum is shifted to zero and the threshold approaches it strictly from below. [Lean source](../../lean/UnivariateML/Family/Base7ParetoIII.lean#L19)

```lean
theorem held_scale_threshold_collapse {n : ℕ} {d : Fin n → ℝ}
    (hd : ∀ i, 0 ≤ d i) (hz : ∃ i, d i = 0) {c s : ℝ}
    (hc : 0 < c) (hc' : c < 1) (hs : 0 < s) :
    Tendsto (fun t ↦ ParetoIII.logLik d (-t) (c, s)) (𝓝[>] 0) atTop
```

### `Base7Lomax.uniqueness`

**lomax · all free.** Count strata have almost-everywhere uniqueness, attained two-level samples have a unique fit, and the displayed three-level sample has two winners. The almost-everywhere branch omits parameter-independent terms. [Lean source](../../lean/UnivariateML/Family/Base7Lomax.lean#L25)

```lean
theorem uniqueness :
    (∀ {n : ℕ} (w : Fin n → ℕ), (∀ i, 0 < w i) →
      ∀ᵐ x : Fin n → ℝ ∂volume, (∃ i j, x i ≠ x j) →
        ∀ p q, UniqueAE.Winner (AE2Lomax.domain 0) (AE2Lomax.likelihood (1) 0 w) x p →
          UniqueAE.Winner (AE2Lomax.domain 0) (AE2Lomax.likelihood (1) 0 w) x q → p = q) ∧
    (∀ {n : ℕ}, 0 < n → ∀ x : Fin n → ℝ, (∀ i, 0 < x i) →
      (∃ a b : ℝ, ∀ i, x i = a ∨ x i = b) →
      let L := fun v : ℝ × ℝ ↦ Lomax.logLik x v.1 v.2
      (∃ v ∈ Lomax.domain, IsMaxOn L Lomax.domain v) →
        ∃! v, v ∈ Lomax.domain ∧ IsMaxOn L Lomax.domain v) ∧
    (
    ∃! a, a ∈ Ioo ((2 : ℝ) ^ 512)⁻¹ ((2 : ℝ) ^ 32)⁻¹ ∧
      let x : Fin 21 → ℝ := fun i ↦ if i.val = 0 then a else if i.val ≤ 10 then 1 else 100
      ∃ p ∈ Lomax.domain, ∃ q ∈ Lomax.domain,
        p.2 < 1024 ∧ 1024 < q.2 ∧
        IsMaxOn (fun v : ℝ × ℝ ↦ Lomax.logLik x v.1 v.2) Lomax.domain p ∧
        IsMaxOn (fun v : ℝ × ℝ ↦ Lomax.logLik x v.1 v.2) Lomax.domain q ∧
        (∀ b ∈ Lomax.domain,
          IsMaxOn (fun v : ℝ × ℝ ↦ Lomax.logLik x v.1 v.2) Lomax.domain b → b = p ∨ b = q) ∧
        Lomax.bestExp x < Lomax.logLik x p.1 p.2)
```

### `Base7InversePareto.uniqueness`

**invpareto · all free.** Count strata have almost-everywhere uniqueness, attained two-level samples have a unique fit, and a positive sample has exactly two winners. The almost-everywhere branch uses reciprocal coordinates in the likelihood. [Lean source](../../lean/UnivariateML/Family/Base7InversePareto.lean#L65)

```lean
theorem uniqueness :
    (∀ {n : ℕ} (w : Fin n → ℕ), (∀ i, 0 < w i) →
      ∀ᵐ x : Fin n → ℝ ∂volume, (∃ i j, x i ≠ x j) →
        ∀ p q, UniqueAE.Winner (AE2Lomax.domain 0) (AE2Lomax.likelihood (-1) 0 w) x p →
          UniqueAE.Winner (AE2Lomax.domain 0) (AE2Lomax.likelihood (-1) 0 w) x q → p = q) ∧
    (∀ {n : ℕ}, 0 < n → ∀ x : Fin n → ℝ, (∀ i, 0 < x i) →
      (∃ a b : ℝ, ∀ i, x i = a ∨ x i = b) →
      let L := fun v : ℝ × ℝ ↦ Transfer.InvPareto.logLik x v.1 v.2
      (∃ v ∈ Lomax.domain, IsMaxOn L Lomax.domain v) →
        ∃! v, v ∈ Lomax.domain ∧ IsMaxOn L Lomax.domain v) ∧
    (
    ∃ a ∈ Ioo ((2 : ℝ) ^ 512)⁻¹ ((2 : ℝ) ^ 32)⁻¹,
      let x : Fin 21 → ℝ := fun i ↦ (if i.val = 0 then a else if i.val ≤ 10 then 1 else 100)⁻¹
      (∀ i, 0 < x i) ∧ ∃ p q : ℝ × ℝ, p.2 < q.2 ∧
        {v ∈ Lomax.domain | IsMaxOn (fun b : ℝ × ℝ ↦
          Transfer.InvPareto.logLik x b.1 b.2) Lomax.domain v} = {p, q} ∧
        Transfer.InvPareto.bestInvExp x < Transfer.InvPareto.logLik x p.1 p.2)
```

### `Base7Binomial.all_free_maximizer_branches`

**binom · all free.** The two constant branches have explicit winner sets, and the positive-mean profile graph is nonempty exactly below equidispersion. The nonconstant adjacent-ratio comparison, adjacency, and prime-power exclusion remain open. [Lean source](../../lean/UnivariateML/Family/Base7Binomial.lean#L82)

```lean
theorem all_free_maximizer_branches {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) :
    let L := fun p : ℕ × ℝ ↦ sampleLikelihood x p.1 p.2
    let W := {p ∈ parameterDomain | IsMaxOn L parameterDomain p}
    ((∀ i, x i = 0) → W = {p ∈ parameterDomain | p.1 = 0 ∨ p.2 = 0}) ∧
    (∀ c : ℕ, 0 < c → (∀ i, x i = c) → W = {(c, 1)}) ∧
    (0 < mean x →
      W = (fun N : ℕ ↦ (N, mean x / N)) ''
        {N ∈ feasible x | IsMaxOn (profile x) (feasible x) N} ∧
      (W.Nonempty ↔ variance x < mean x) ∧
      (variance x < mean x → W.Finite))
```

### `Base7DGamma.finite_profile_candidates`

**dgamma · all free.** Conditional on analytic score and compact confinement, the stationary shapes form a finite candidate set whose highest members are exactly the attained winners. The family-specific analytic profile, score confinement and trace criterion remain open. [Lean source](../../lean/UnivariateML/Family/Base7DGamma.lean#L26)

```lean
theorem finite_profile_candidates (P : ℝ → ℝ) {a b : ℝ} (ha : 0 < a)
    (han : AnalyticOnNhd ℝ (deriv P) (Ioi 0))
    (hne : ∃ t > 0, deriv P t ≠ 0)
    (hcut : ∀ t > 0, deriv P t = 0 → t ∈ Icc a b)
    (hatt : ∃ t ∈ Ioi 0, IsMaxOn P (Ioi 0) t) :
    let C := {t ∈ Icc a b | deriv P t = 0}
    C.Finite ∧ ∀ t, (t ∈ Ioi 0 ∧ IsMaxOn P (Ioi 0) t) ↔
      t ∈ C ∧ IsMaxOn P C t
```

### `Base7Skellam.poisson_supporting_sum_shift`

**skellam · all free.** The Poisson supporting inequality gives a strict finite-rate bound for all positive index shifts. Analytic step in the one-sided supporting test. [Lean source](../../lean/UnivariateML/Family/Base7Skellam.lean#L37)

```lean
theorem poisson_supporting_sum_shift {n : ℕ} (k : Fin n → ℕ) (w : Fin n → ℝ)
    (h : ∀ t ≥ 0, (∑ i, w i * poissonMass t (k i)) ≤ 1) :
    ∀ j : ℕ, 0 < j → ∀ t ≥ 0, (∑ i, w i * poissonMass t (k i + j)) ≤ 1 - exp (-t)
```

### `Base7Skellam.nonnegative_supporting_unique`

**skellam · all free.** For nonnegative counts of positive mean passing the Poisson supporting test, the maximum is unique at the Poisson face. Nonnegative orientation and positive mean. [Lean source](../../lean/UnivariateML/Family/Base7Skellam.lean#L99)

```lean
theorem nonnegative_supporting_unique {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) {d : ℝ}
    (hd : 0 < d) (hmean : d = (∑ i, (x i : ℝ)) / n)
    (hs : ∀ t ≥ 0, (∑ i, poissonMass t (x i) / ((n : ℝ) * poissonMass d (x i))) ≤ 1) :
    let y : Fin n → ℤ := fun i ↦ x i
    (d, 0) ∈ domain ∧ IsMaxOn (likelihood y) domain (d, 0) ∧
      ∀ p ∈ domain, IsMaxOn (likelihood y) domain p → p = (d, 0)
```

### `Base7Skellam.nonpositive_supporting_unique`

**skellam · all free.** For nonpositive counts of negative mean passing the reflected supporting test, the maximum is unique at the reflected Poisson face. Nonpositive orientation and strictly negative signed mean. [Lean source](../../lean/UnivariateML/Family/Base7Skellam.lean#L251)

```lean
theorem nonpositive_supporting_unique {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) {d : ℝ}
    (hd : 0 < d) (hmean : d = (∑ i, (x i : ℝ)) / n)
    (hs : ∀ t ≥ 0, (∑ i, poissonMass t (x i) / ((n : ℝ) * poissonMass d (x i))) ≤ 1) :
    let y : Fin n → ℤ := fun i ↦ -(x i : ℤ)
    (0, d) ∈ domain ∧ IsMaxOn (likelihood y) domain (0, d) ∧
      ∀ p ∈ domain, IsMaxOn (likelihood y) domain p → p = (0, d)
```

### `Base7Skellam.adjacent_supporting_inequality`

**skellam · all free.** Every positive-mean sample on adjacent nonnegative counts passes the supporting inequality. Nonnegative orientation; combines with nonnegative_supporting_unique. [Lean source](../../lean/UnivariateML/Family/Base7Skellam.lean#L194)

```lean
theorem adjacent_supporting_inequality {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) {j : ℕ}
    (hx : ∀ i, x i = j ∨ x i = j + 1) {d : ℝ}
    (hd : 0 < d) (hmean : d = (∑ i, (x i : ℝ)) / n) :
    ∀ t ≥ 0, (∑ i, poissonMass t (x i) / ((n : ℝ) * poissonMass d (x i))) ≤ 1
```
