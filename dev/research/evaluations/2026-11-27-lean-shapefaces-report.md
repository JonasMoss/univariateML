# Lean formalization of shape-family likelihoods

TASK-390 · 2026-11-27

Named proof placeholders: none.

Lean checked 67 public theorems across the six family modules. The [atlas fragment](../existence-atlas/lean-map.d/lean-shapefaces.csv) records one row per theorem. The results include exact Burr transfers, the held-power and held-scale face comparisons for inverse Burr and Pareto IV, the fully classified shifted Gompertz held-rate fit, and the unique PGW exponent fit with scale and shape held.

## Source scope and discrepancies

- The positive-excess hypotheses in the Pareto IV and Feller–Pareto transfers are essential for the likelihood interpretation. Pareto IV's held-scale Pareto face also holds the threshold. At equality with the held scale, the inherited Burr envelope includes the endpoint factor of one half per tied observation. The inverse Burr transfer carries that same factor at its upper endpoint. [Atlas audit, sections 1–3](2026-11-27-atlas-audit-report.md)
- The exponentiated Weibull detector records describe comparisons stronger than the one-sided paths alone establish. The mathematical all-free criterion needs the common-point likelihood envelope and a finite equality rule. Across the exponentiated Weibull and Pareto IV records, a limit path alone gives a lower bound on the available envelope value. [Exponentiated Weibull report](2026-11-25-expweibull-math-report.md) · [Pareto IV report](2026-11-25-pareto34-math-report.md)
- The PGW dossier lacks an existence block. The eight-mask statement comes from the mathematical report. The Weibull identity covers exponent one. The affine proof covers fixed positive exponent in general, and the scalar results cover every pair of fixed positive coefficients. [PGW report](2026-11-25-pgw-math-report.md)
- The Feller–Pareto narrow-span exclusion holds threshold and power, with scale and both beta shapes free. The atlas has a `min+scale+shape2` row for the fixed-scale problem and no separate `min+shape2` row. The fragment uses `generic` for the powered-excess and narrow-span results, since they allow the scale to vary. The checked narrow-span theorem excludes finite MLEs. The global signed face choice also needs the face comparison and compactness in the report. [Feller–Pareto report](2026-11-25-fpareto-math-report.md)

## Proof effort and open entries

| Family | Proof work checked | Open |
| --- | --- | --- |
| Inverse Burr | Reconciled the summed Burr likelihood with the averaged held-coordinate objective. Proved the reciprocal scale bijections and kept the full reciprocal Jacobian. Transferred the Lomax witness, the inverse Weibull comparison, and the held-scale power envelope. Proved the constant-data concentration branches and the scalar power fit by strict concavity and compact superlevels. | The all-free transfer needs a formal Burr competitive-escape theorem and optimized face values before it gives the atlas attainment criterion. The exact nonuniqueness examples need certified Burr maximizers. |
| Pareto IV | Reused the held-power and one-coordinate Burr results on excesses. Checked the two-shape fit at held scale, its complementary Pareto face with at least one strictly positive relative log, and the positive-scale bijection for the held-first-shape fit. Checked constant concentration at held threshold and transferred the scalar power fit from the reciprocal inverse Burr problem. | The held-threshold all-free result still needs the Burr joint envelope and tie constructions. Free-threshold masks need the endpoint spike and the simultaneous scale/shape escape, the normalized threshold-score argument at fixed scale, and the fixed-shape joint threshold/scale comparison, including the equality in the multiplicity exponent. |
| Exponentiated Weibull | Proved the log-density curvature and a linear two-sided tail bound, enclosed every affine superlevel in a compact box, and transferred the unique maximum to physical parameters for every positive held exponent. The exponent-only fit uses a strict log inequality. Held-shape attainment follows from explicit compact bounds on the profiled scale superlevels. Proved common paths to both fully optimized face values, including repeated observations at the power endpoint. | The envelope upper bound over arbitrary escaping sequences, the held-scale branches, the other scalar masks, and the real finite tie remain open in Lean. |
| Shifted Gompertz | Bracketed the strictly decreasing shape score with an explicit upper rate-dependent shape bound. At its zero endpoint, used a strict log inequality and handled all-zero data separately. Used continuity of the inward score for the exponential local maximum and strict monotonicity of the survival sum for the crossing-rate implication. For held shape, bounded the strictly decreasing rate score at both ends. Proved compact joint superlevels using one uniform rate bound and a linear upper bound in shape once the rate is bounded. Verified the constant-data path has value \(n(\log b-1)\). | The exact two-maximizer crossing and general cardinality are open. |
| PGW | Checked the Weibull identity and the nonconstant joint fit at exponent one, and proved constant concentration for arbitrary held positive exponent. For held scale and shape, differentiated the physical likelihood, bounded the score on either side of its unique root, and applied the generic score theorem. Proved curvature and coercivity in the fixed-exponent affine chart, then used its two linear restrictions to cover the remaining scalar fits at arbitrary exponent. | The all-free, held-shape, and held-scale joint decisions need the two competitive escape bounds, including the half-density endpoint factor, followed by the finite-critical-value and strict-face comparisons. |
| Feller–Pareto | Proved the power/Jacobian identity and the fixed-scale beta-prime reduction. Derived the uniform log Taylor bound and logistic standardization, then obtained the digamma difference bounds from the Gamma recurrence and its large-shape limit. The three stationary scores give the coefficient 69, which excludes finite MLEs on physical data under the narrow-span condition. | A strict global face decision needs the compact profiled superlevel argument and the centered hyperbolic-sine sign comparison. The powered-excess transfer does not cover free-threshold endpoint masks. |

The singleton domains require no likelihood comparison. No family-specific singleton theorems were added.

## Verification

- With temporary `public import` lines for all six modules in `dev/lean/UnivariateML.lean`, ran `/tmp/umvl-cpu/run nice -n 10 env LEAN_NUM_THREADS=1 /home/jonas/.elan/bin/lake build` from `dev/lean/`. The root build passed with 7,480 jobs and linter warnings. All 67 theorems remain free of proof placeholders.
- Restored the root file and ran `git diff --exit-code dev/lean/UnivariateML.lean`. It returned zero with no diff.
- The checker in this branch predates `--check`. Ran `python3 dev/tools/existence_atlas.py --check` in a worktree-local scratch copy of the checker and atlas data from TASK-391 commit `68fcd92b`, with this job's fragment and the worktree's dossier and proof sources. It returned zero with no diagnostics.

## Public theorem statements

The namespace of each result is recorded in the atlas fragment.

### invburr

#### `logLik_reciprocal`

The summed inverse Burr likelihood is the reciprocal Burr likelihood plus the data Jacobian.

```lean
theorem logLik_reciprocal {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (k c : ℝ) {s : ℝ} (hs : 0 < s) :
    logLik x k c s = Burr.logLik (reciprocal x) c k s⁻¹ + reciprocalJac x
```

#### `joint_maximizers`

Inverting the scale bijects the full finite maximizer sets.

```lean
theorem joint_maximizers {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) :
    BijOn reciprocateScale
      {p ∈ domain | IsMaxOn (fun q : ℝ × ℝ × ℝ ↦ logLik x q.1 q.2.1 q.2.2) domain p}
      {p ∈ domain | IsMaxOn
        (fun q : ℝ × ℝ × ℝ ↦ Burr.logLik (reciprocal x) q.2.1 q.1 q.2.2) domain p}
```

#### `held_shapes_unique`

With both shapes fixed there is a unique positive scale estimate.

```lean
theorem held_shapes_unique (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {k c : ℝ} (hk : 0 < k) (hc : 0 < c) :
    ∃! s, s ∈ Ioi (0 : ℝ) ∧ IsMaxOn (logLik x k c) (Ioi 0) s
```

#### `held_scale_power_unique`

With scale and power fixed there is a unique positive first-shape estimate.

```lean
theorem held_scale_power_unique (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {c s : ℝ} (hc : 0 < c) (hs : 0 < s) :
    ∃! k, k ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun k ↦ logLik x k c s) (Ioi 0) k
```

#### `held_power_iff`

With power fixed, finite attainment is equivalent to the Lomax witness on inversely powered data.

```lean
theorem held_power_iff (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {c : ℝ} (hc : 0 < c) :
    (∃ p ∈ Lomax.domain,
      IsMaxOn (fun q : ℝ × ℝ ↦ logLik x q.1 c q.2) Lomax.domain p) ↔
    ∃ u ∈ Ioo 0 (Lomax.uBar (Burr.powered (reciprocal x) c)),
      Lomax.psi (Burr.powered (reciprocal x) c) u ≤ 0
```

#### `held_power_face`

Without the Lomax witness, the inverse Weibull face strictly exceeds every finite fit and gives the supremum.

```lean
theorem held_power_face (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {c : ℝ} (hc : 0 < c)
    (hw : ¬ ∃ u ∈ Ioo 0 (Lomax.uBar (Burr.powered (reciprocal x) c)),
      Lomax.psi (Burr.powered (reciprocal x) c) u ≤ 0) :
    sSup ((fun p : ℝ × ℝ ↦ logLik x p.1 c p.2) '' Lomax.domain) =
      Burr.bestWeibull (reciprocal x) c + reciprocalJac x ∧
    ∀ p ∈ Lomax.domain, logLik x p.1 c p.2 <
      Burr.bestWeibull (reciprocal x) c + reciprocalJac x
```

#### `held_scale_unique`

A nonconstant sample with an observation above the held scale has a unique finite two-shape fit.

```lean
theorem held_scale_unique (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {s : ℝ} (hs : 0 < s) (hne : ∃ i j, x i ≠ x j) (habove : ∃ i, s < x i) :
    ∃! p, p ∈ BurrHeld.shapeDomain ∧
      IsMaxOn (fun q : ℝ × ℝ ↦ logLik x q.2 q.1 s) BurrHeld.shapeDomain p
```

#### `held_scale_face`

When all observations are at or below scale and one is below it, the reciprocal Pareto face gives a strictly unattained supremum.

```lean
theorem held_scale_face (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {s : ℝ} (hs : 0 < s) (hle : ∀ i, x i ≤ s) (hlt : ∃ i, x i < s) :
    let z := BurrHeld.logRatios (reciprocal x) s⁻¹
    let value := BurrHeld.paretoValue z + (reciprocalJac x / n - BurrHeld.avg (fun i ↦ log (reciprocal x i)))
    IsLUB ((fun q : ℝ × ℝ ↦ logLik x q.2 q.1 s / n) '' BurrHeld.shapeDomain) value ∧
      ∀ q ∈ BurrHeld.shapeDomain, logLik x q.2 q.1 s / n < value
```

#### `held_first_unique`

A nonconstant sample has a unique finite power and scale with first shape held.

```lean
theorem held_first_unique (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {k : ℝ} (hk : 0 < k) (hne : ∃ i j, x i ≠ x j) :
    ∃! p, p ∈ BurrHeld.shapeDomain ∧
      IsMaxOn (fun q : ℝ × ℝ ↦ logLik x k q.1 q.2) BurrHeld.shapeDomain p
```

#### `held_first_scale_unique`

With first shape and scale held there is a unique power when some observation differs from scale.

```lean
theorem held_first_scale_unique (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {k s : ℝ} (hk : 0 < k) (hs : 0 < s)
    (hne : ∃ i, x i ≠ s) :
    ∃! b, b ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun c ↦ logLik x k c s) (Ioi 0) b
```

#### `held_first_constant_path`

At constant data, taking the scale to be the common value gives a diverging likelihood as power increases.

```lean
theorem held_first_constant_path (hn : 0 < n) {v k : ℝ} (hv : 0 < v) (_hk : 0 < k) :
    Tendsto (fun b ↦ logLik (fun _ : Fin n ↦ v) k b v) atTop atTop
```

#### `joint_constant_unbounded`

The all-free inverse Burr likelihood is unbounded on constant positive samples.

```lean
theorem joint_constant_unbounded (hn : 0 < n) {v : ℝ} (hv : 0 < v) :
    ¬ BddAbove ((fun p : ℝ × ℝ × ℝ ↦ logLik (fun _ : Fin n ↦ v) p.1 p.2.1 p.2.2) ''
      domain)
```

#### `held_first_scale_unbounded`

When all observations equal the held scale, the likelihood diverges as the free power increases.

```lean
theorem held_first_scale_unbounded (hn : 0 < n) {x : Fin n → ℝ} {k s : ℝ}
    (hk : 0 < k) (hs : 0 < s) (he : ∀ i, x i = s) :
    Tendsto (fun b ↦ logLik x k b s) atTop atTop
```

#### `held_scale_constant_unbounded`

At or above the held scale, constant data give unbounded likelihood with both shapes free.

```lean
theorem held_scale_constant_unbounded (hn : 0 < n) {v s : ℝ}
    (hs : 0 < s) (hsv : s ≤ v) :
    ¬ BddAbove ((fun p : ℝ × ℝ ↦ logLik (fun _ : Fin n ↦ v) p.2 p.1 s) ''
      BurrHeld.shapeDomain)
```

### pareto4

#### `logLik_excess`

With threshold held, the likelihood is Burr on the excesses.

```lean
theorem logLik_excess (x : Fin n → ℝ) (μ k c s : ℝ) :
    logLik x μ k c s = Burr.logLik (excess x μ) c k s
```

#### `held_power_iff`

Held power and threshold give finite attainment exactly under the powered-excess Lomax witness.

```lean
theorem held_power_iff (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ}
    (hx : ∀ i, μ < x i) {c : ℝ} (hc : 0 < c) :
    (∃ p ∈ Lomax.domain,
      IsMaxOn (fun q : ℝ × ℝ ↦ logLik x μ q.1 c q.2) Lomax.domain p) ↔
    ∃ u ∈ Ioo 0 (Lomax.uBar (Burr.powered (excess x μ) c)),
      Lomax.psi (Burr.powered (excess x μ) c) u ≤ 0
```

#### `held_power_face`

Without the Lomax witness, the shifted Weibull value is the supremum and every finite fit is strictly below it.

```lean
theorem held_power_face (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ}
    (hx : ∀ i, μ < x i) {c : ℝ} (hc : 0 < c)
    (hw : ¬ ∃ u ∈ Ioo 0 (Lomax.uBar (Burr.powered (excess x μ) c)),
      Lomax.psi (Burr.powered (excess x μ) c) u ≤ 0) :
    sSup ((fun p : ℝ × ℝ ↦ logLik x μ p.1 c p.2) '' Lomax.domain) =
      Burr.bestWeibull (excess x μ) c ∧
    ∀ p ∈ Lomax.domain, logLik x μ p.1 c p.2 < Burr.bestWeibull (excess x μ) c
```

#### `held_shapes_unique`

Both held shapes and positive excesses give a unique positive scale.

```lean
theorem held_shapes_unique (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ}
    (hx : ∀ i, μ < x i) {k c : ℝ} (hk : 0 < k) (hc : 0 < c) :
    ∃! s, s ∈ Ioi (0 : ℝ) ∧ IsMaxOn (logLik x μ k c) (Ioi 0) s
```

#### `held_scale_power_unique`

Holding threshold, scale and power gives a unique positive first shape.

```lean
theorem held_scale_power_unique (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ}
    (hx : ∀ i, μ < x i) {c s : ℝ} (hc : 0 < c) (hs : 0 < s) :
    ∃! k, k ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun k ↦ logLik x μ k c s) (Ioi 0) k
```

#### `held_scale_unique`

Nonconstant excesses with one below the held scale give a unique finite two-shape estimate.

```lean
theorem held_scale_unique (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ} (hx : ∀ i, μ < x i)
    {s : ℝ} (hs : 0 < s) (hne : ∃ i j, x i ≠ x j) (habove : ∃ i, excess x μ i < s) :
    ∃! p, p ∈ BurrHeld.shapeDomain ∧
      IsMaxOn (fun q : ℝ × ℝ ↦ logLik x μ q.2 q.1 s) BurrHeld.shapeDomain p
```

#### `held_scale_face`

If every excess is at or above scale and one strictly above, the shifted Pareto face gives the strictly unattained supremum.

```lean
theorem held_scale_face (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ} (hx : ∀ i, μ < x i)
    {s : ℝ} (hs : 0 < s) (hle : ∀ i, s ≤ excess x μ i) (hlt : ∃ i, s < excess x μ i) :
    let z := BurrHeld.logRatios (excess x μ) s
    let value := BurrHeld.paretoValue z + (-BurrHeld.avg (fun i ↦ log (excess x μ i)))
    IsLUB ((fun q : ℝ × ℝ ↦ logLik x μ q.2 q.1 s / n) '' BurrHeld.shapeDomain) value ∧
      ∀ q ∈ BurrHeld.shapeDomain, logLik x μ q.2 q.1 s / n < value
```

#### `held_first_unique`

Nonconstant positive excesses give a unique power and scale with first shape held.

```lean
theorem held_first_unique (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ} (hx : ∀ i, μ < x i)
    {k : ℝ} (hk : 0 < k) (hne : ∃ i j, x i ≠ x j) :
    ∃! p, p ∈ BurrHeld.shapeDomain ∧
      IsMaxOn (fun q : ℝ × ℝ ↦ logLik x μ k q.1 q.2) BurrHeld.shapeDomain p
```

#### `held_first_scale_unique`

With threshold, scale and first shape held there is a unique power when one excess differs from scale.

```lean
theorem held_first_scale_unique (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ}
    (hx : ∀ i, μ < x i) {k s : ℝ} (hk : 0 < k) (hs : 0 < s)
    (hne : ∃ i, excess x μ i ≠ s) :
    ∃! b, b ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun c ↦ logLik x μ k c s) (Ioi 0) b
```

#### `held_first_constant_path`

At constant positive excesses, putting scale at that excess gives unbounded likelihood as power increases.

```lean
theorem held_first_constant_path (hn : 0 < n) {v μ k : ℝ}
    (hv : μ < v) (_hk : 0 < k) :
    Tendsto (fun b ↦ logLik (fun _ : Fin n ↦ v) μ k b (v - μ)) atTop atTop
```

#### `held_threshold_constant_unbounded`

With threshold held and three free regular coefficients, constant positive excesses give unbounded likelihood.

```lean
theorem held_threshold_constant_unbounded (hn : 0 < n) {v μ : ℝ} (hv : μ < v) :
    ¬ BddAbove ((fun p : ℝ × ℝ × ℝ ↦ logLik (fun _ : Fin n ↦ v) μ p.1 p.2.1 p.2.2) ''
      (Ioi 0 ×ˢ BurrHeld.shapeDomain))
```

#### `held_first_scale_unbounded`

When every excess equals the held scale, the free power produces unbounded likelihood.

```lean
theorem held_first_scale_unbounded (hn : 0 < n) {x : Fin n → ℝ} {μ k s : ℝ}
    (hk : 0 < k) (hs : 0 < s) (he : ∀ i, excess x μ i = s) :
    Tendsto (fun b ↦ logLik x μ k b s) atTop atTop
```

#### `held_scale_constant_unbounded`

With threshold and scale held, constant positive excesses at or below scale give unbounded likelihood.

```lean
theorem held_scale_constant_unbounded (hn : 0 < n) {v μ s : ℝ}
    (hv : μ < v) (hvs : v - μ ≤ s) :
    ¬ BddAbove ((fun p : ℝ × ℝ ↦ logLik (fun _ : Fin n ↦ v) μ p.2 p.1 s) ''
      BurrHeld.shapeDomain)
```

### expweibull

#### `exponent_one_joint`

At exponent one the nonconstant positive sample has the unique joint Weibull fit.

```lean
theorem exponent_one_joint (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) :
    ∃! p : ℝ × ℝ, p ∈ Weibull.domain ∧
      IsMaxOn (fun q : ℝ × ℝ ↦ logLik x q.1 q.2 1) Weibull.domain p
```

#### `held_shape_scale_mle`

For positive data with scale and shape fixed, the exponent has a unique finite positive optimum.

```lean
theorem held_shape_scale_mle (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {a b : ℝ} (ha : 0 < a) :
    ∃! c, c ∈ Ioi (0 : ℝ) ∧ IsMaxOn (logLik x b a) (Ioi 0) c
```

#### `affine_strictConcave`

Two distinct log observations give strict concavity in affine log-data coordinates.

```lean
theorem affine_strictConcave {c : ℝ} (hc : 0 < c) {y : Fin n → ℝ}
    (hne : ∃ i j, y i ≠ y j) : StrictConcaveOn ℝ affineDomain (affineLik y c)
```

#### `affine_coercive`

The fixed-exponent affine likelihood tends to minus infinity outside compact subsets of its domain.

```lean
theorem affine_coercive (hn : 0 < n) {c : ℝ} (hc : 0 < c) {y : Fin n → ℝ}
    (hne : ∃ i j, y i ≠ y j) :
    Tendsto (fun p : affineDomain ↦ affineLik y c p) (cocompact affineDomain) atBot
```

#### `affine_held_exponent_mle`

The nonconstant log sample has one finite maximizer in the affine parameters.

```lean
theorem affine_held_exponent_mle (hn : 0 < n) {c : ℝ} (hc : 0 < c) {y : Fin n → ℝ}
    (hne : ∃ i j, y i ≠ y j) :
    ∃! p, p ∈ affineDomain ∧ IsMaxOn (affineLik y c) affineDomain p
```

#### `held_exponent_mle`

Holding any positive exponent gives a unique finite scale and shape for nonconstant positive data.

```lean
theorem held_exponent_mle (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {c : ℝ} (hc : 0 < c) (hne : ∃ i j, x i ≠ x j) :
    ∃! p : ℝ × ℝ, p ∈ Weibull.domain ∧
      IsMaxOn (fun q ↦ logLik x q.1 q.2 c) Weibull.domain p
```

#### `held_shape_attains`

Holding a positive shape gives a finite scale and exponent maximizer on nonconstant positive data.

```lean
theorem held_shape_attains (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {b : ℝ} (hb : 0 < b) (hne : ∃ i j, x i ≠ x j) :
    ∃ p ∈ Weibull.domain,
      IsMaxOn (fun q : ℝ × ℝ ↦ logLik x b q.1 q.2) Weibull.domain p
```

#### `power_face_path`

A common parameter path approaches the power likelihood, including top ties.

```lean
theorem power_face_path {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {k m : ℝ} (hk : 0 < k) (hm : ∀ i, log (x i) ≤ m) :
    Tendsto (fun t : ℝ ↦ logLik x (t ^ 2) (exp (m + t⁻¹)) (k / t ^ 2)) atTop
      (𝓝 (∑ i, (log k + k * (log (x i) - m) - log (x i))))
```

#### `inverse_weibull_face_path`

A common parameter path approaches the inverse Weibull likelihood.

```lean
theorem inverse_weibull_face_path {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {k : ℝ} (hk : 0 < k) (μ : ℝ) :
    Tendsto (fun t : ℝ ↦ logLik x (k / t) (exp (μ - log t / (k / t))) (exp t)) atTop
      (𝓝 (Weibull.Inverse.logLik x k (exp μ)))
```

#### `optimized_power_face`

The best power face uses the largest log observation and its explicit fitted exponent. One common path reaches its value.

```lean
theorem optimized_power_face {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) {m : ℝ} (hm : ∀ i, log (x i) ≤ m)
    (hcontact : ∃ i, log (x i) = m) :
    let D := ∑ i, (m - log (x i))
    let k := (n : ℝ) / D
    let P := (n : ℝ) * (log k - 1) - ∑ i, log (x i)
    0 < k ∧
      (∀ a l : ℝ, (∀ i, log (x i) ≤ a) → 0 < l →
        (∑ i, (log l + l * (log (x i) - a) - log (x i))) ≤ P) ∧
      Tendsto (fun t : ℝ ↦ logLik x (t ^ 2) (exp (m + t⁻¹)) (k / t ^ 2)) atTop (𝓝 P)
```

#### `optimized_inverse_weibull_face`

The inverse Weibull optimum is approached by a common exponentiated Weibull path.

```lean
theorem optimized_inverse_weibull_face (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) (hne : ∃ i j, x i ≠ x j) :
    ∃ p ∈ Weibull.domain,
      IsMaxOn (fun q : ℝ × ℝ ↦ Weibull.Inverse.logLik x q.1 q.2) Weibull.domain p ∧
      Tendsto (fun t : ℝ ↦ logLik x (p.1 / t)
        (exp (log p.2 - log t / (p.1 / t))) (exp t)) atTop
        (𝓝 (Weibull.Inverse.logLik x p.1 p.2))
```

### sgomp

#### `held_rate_zero_strict`

At a held rate with nonpositive inward score, every positive shape loses strictly to shape zero.

```lean
theorem held_rate_zero_strict {x : Fin n → ℝ} (hx : ∀ i, 0 ≤ x i)
    {b : ℝ} (hb : 0 < b) (hscore : (n : ℝ) ≤ 2 * survivalSum x b)
    {η : ℝ} (hη : 0 < η) (hn : 0 < n) :
    logLik x b η < logLik x b 0
```

#### `held_rate_unique`

Every held positive rate gives a unique maximizing shape, and it is zero exactly when the inward score is nonpositive.

```lean
theorem held_rate_unique (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 ≤ x i)
    {b : ℝ} (hb : 0 < b) :
    ∃ η ∈ Ici (0 : ℝ), IsMaxOn (logLik x b) (Ici 0) η ∧
      (∀ t ∈ Ici (0 : ℝ), IsMaxOn (logLik x b) (Ici 0) t → t = η) ∧
      (η = 0 ↔ (n : ℝ) ≤ 2 * survivalSum x b)
```

#### `exponential_strict`

At held shape zero and positive sample sum, the sample-size-over-sum rate strictly beats every other positive rate.

```lean
theorem exponential_strict {x : Fin n → ℝ} (hn : 0 < n) (hS : 0 < ∑ i, x i)
    {b : ℝ} (hb : 0 < b) (hne : b ≠ exponentialRate x) :
    logLik x b 0 < logLik x (exponentialRate x) 0
```

#### `all_zero_rate_unbounded`

For an all-zero sample, the likelihood grows without bound at every held shape.

```lean
theorem all_zero_rate_unbounded (hn : 0 < n) (η : ℝ) :
    Tendsto (fun b ↦ logLik (fun _ : Fin n ↦ 0) b η) atTop atTop
```

#### `exponential_strict_local`

A strictly negative inward score at the exponential estimate gives a strict local maximum.

```lean
theorem exponential_strict_local {x : Fin n → ℝ} (hx : ∀ i, 0 ≤ x i)
    (hn : 0 < n) (hS : 0 < ∑ i, x i)
    (hscore : (n : ℝ) < 2 * survivalSum x (exponentialRate x)) :
    ∀ᶠ b in 𝓝 (exponentialRate x), ∀ η ≥ 0,
      (b, η) ≠ (exponentialRate x, 0) →
      logLik x b η < logLik x (exponentialRate x) 0
```

#### `exponential_strict_local_below_crossing`

If the exponential rate is below the shape-score crossing, the exponential pair is a strict local maximum.

```lean
theorem exponential_strict_local_below_crossing {x : Fin n → ℝ} (hx : ∀ i, 0 ≤ x i)
    (hn : 0 < n) (hS : 0 < ∑ i, x i) {β : ℝ}
    (hβ : (n : ℝ) = 2 * survivalSum x β) (hb : exponentialRate x < β) :
    ∀ᶠ b in 𝓝 (exponentialRate x), ∀ η ≥ 0,
      (b, η) ≠ (exponentialRate x, 0) →
      logLik x b η < logLik x (exponentialRate x) 0
```

#### `held_shape_rate_unique`

Holding any admissible shape gives one positive rate when the sample sum is positive.

```lean
theorem held_shape_rate_unique (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 ≤ x i)
    {η : ℝ} (hη : 0 ≤ η) (hS : 0 < ∑ i, x i) :
    ∃! b, b ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun t ↦ logLik x t η) (Ioi 0) b
```

#### `joint_attains`

Every nonconstant nonnegative sample has a finite global shifted Gompertz maximizer.

```lean
theorem joint_attains (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 ≤ x i) (hne : ∃ i j, x i ≠ x j) :
    ∃ p ∈ jointDomain,
      IsMaxOn (fun q : ℝ × ℝ ↦ logLik x q.1 q.2) jointDomain p
```

#### `joint_constant_unbounded`

At constant data the coupled exponential-in-rate shape path makes the log likelihood diverge.

```lean
theorem joint_constant_unbounded (hn : 0 < n) (v : ℝ) (_hv : 0 ≤ v) :
    Tendsto (fun b ↦ logLik (fun _ : Fin n ↦ v) b (exp (b * v))) atTop atTop
```

### pgw

#### `exponent_one_logLik`

At exponent one the likelihood equals Weibull.

```lean
theorem exponent_one_logLik {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (b : ℝ) {a : ℝ} (ha : 0 < a) : logLik x b a 1 = Weibull.logLik x b a
```

#### `exponent_one_joint`

At exponent one nonconstant positive data have a unique scale and shape fit.

```lean
theorem exponent_one_joint (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) :
    ∃! p : ℝ × ℝ, p ∈ Weibull.domain ∧
      IsMaxOn (fun q : ℝ × ℝ ↦ logLik x q.1 q.2 1) Weibull.domain p
```

#### `held_exponent_constant_unbounded`

For constant data and any held positive exponent, fixing scale at the common value and increasing shape gives unbounded likelihood.

```lean
theorem held_exponent_constant_unbounded (hn : 0 < n) {t c : ℝ} (ht : 0 < t) (_hc : 0 < c) :
    Tendsto (fun b ↦ logLik (fun _ : Fin n ↦ t) b t c) atTop atTop
```

#### `held_scale_shape_unique`

For positive data and held scale and shape, there is exactly one maximizing positive exponent.

```lean
theorem held_scale_shape_unique (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {b a : ℝ} (_hb : 0 < b) (ha : 0 < a) :
    ∃! c, c ∈ Ioi (0 : ℝ) ∧ IsMaxOn (logLik x b a) (Ioi 0) c
```

#### `affine_strictConcave`

The affine likelihood is strictly concave on its positive-shape domain for every nonempty sample and positive exponent.

```lean
theorem affine_strictConcave (hn : 0 < n) {c : ℝ} (hc : 0 < c) (y : Fin n → ℝ) :
    StrictConcaveOn ℝ affineDomain (affineLik y c)
```

#### `affine_coercive`

For nonconstant data the affine likelihood tends to minus infinity on compact escape.

```lean
theorem affine_coercive (hn : 0 < n) {c : ℝ} (hc : 0 < c) {y : Fin n → ℝ}
    (hne : ∃ i j, y i ≠ y j) :
    Tendsto (fun p : affineDomain ↦ affineLik y c p) (cocompact affineDomain) atBot
```

#### `affine_held_exponent_mle`

The nonconstant log sample has a unique finite affine maximum at every positive held exponent.

```lean
theorem affine_held_exponent_mle (hn : 0 < n) {c : ℝ} (hc : 0 < c) {y : Fin n → ℝ}
    (hne : ∃ i j, y i ≠ y j) :
    ∃! p, p ∈ affineDomain ∧ IsMaxOn (affineLik y c) affineDomain p
```

#### `held_exponent_mle`

The fixed-exponent chart yields one finite shape and scale for every nonconstant positive sample.

```lean
theorem held_exponent_mle (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {c : ℝ} (hc : 0 < c) (hne : ∃ i j, x i ≠ x j) :
    ∃! p : ℝ × ℝ, p ∈ Weibull.domain ∧
      IsMaxOn (fun q ↦ logLik x q.1 q.2 c) Weibull.domain p
```

#### `held_exponent_shape_unique`

Arbitrary held positive exponent and shape give a unique positive scale.

```lean
theorem held_exponent_shape_unique (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {b c : ℝ} (hb : 0 < b) (hc : 0 < c) :
    ∃! a, a ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun t ↦ logLik x b t c) (Ioi 0) a
```

#### `held_exponent_scale_unique`

Arbitrary held positive exponent and scale give a unique shape if some observation differs from the held scale.

```lean
theorem held_exponent_scale_unique (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {a c : ℝ} (ha : 0 < a) (hc : 0 < c)
    (hne : ∃ i, x i ≠ a) :
    ∃! b, b ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun t ↦ logLik x t a c) (Ioi 0) b
```

#### `held_exponent_scale_unbounded`

When all observations equal the held scale, increasing the free shape makes the log likelihood diverge.

```lean
theorem held_exponent_scale_unbounded (hn : 0 < n) {x : Fin n → ℝ}
    {a c : ℝ} (ha : 0 < a) (hc : 0 < c) (he : ∀ i, x i = a) :
    Tendsto (fun b ↦ logLik x b a c) atTop atTop
```

### fpareto

#### `logLik_powered_excess`

At positive excesses the held-power likelihood is beta prime on powered excesses plus the power Jacobian.

```lean
theorem logLik_powered_excess {x : Fin n → ℝ} {μ : ℝ} (hx : ∀ i, μ < x i)
    (a : ℝ) {b : ℝ} (hb : 0 < b) (p : ℝ × ℝ) :
    logLikelihood x μ a b p =
      BetaPrime.logLik (poweredExcess x μ a) (b ^ a) p + powerJacobian x μ a
```

#### `held_power_scale_iff`

With threshold, power and scale held, the two beta shapes have a unique finite optimum exactly for nonconstant positive excesses.

```lean
theorem held_power_scale_iff (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ}
    (hx : ∀ i, μ < x i) {a b : ℝ} (ha : 0 < a) (hb : 0 < b) :
    (∃! p, p ∈ Beta.domain ∧ IsMaxOn (logLikelihood x μ a b) Beta.domain p) ↔
      ∃ i j, x i ≠ x j
```

#### `narrow_span_moment_bound`

The four analytic estimates at a stationary point would imply the bound on the third central log moment.

```lean
theorem narrow_span_moment_bound {D h μ₃ Δ T₃ : ℝ}
    (hD : 0 < D) (hh : 0 < h) (hhD : 1 / (8 * D ^ 2) ≤ h)
    (hcubic : |T₃ - μ₃| ≤ 21 * D ^ 4)
    (hsample : |Δ + T₃ / 3| ≤ 16 * D ^ 4 / 3)
    (hshape : |Δ| ≤ 1 / (6 * h ^ 2)) :
    |μ₃| ≤ 69 * D ^ 4
```

#### `digamma_difference_bounds`

The Gamma score-gap difference has the stated half-reciprocal term and a bounded quadratic correction.

```lean
theorem digamma_difference_bounds {z N : ℝ} (hz : 0 < z) (hN : z ≤ N) :
    0 ≤ Gamma.scoreGap z - Gamma.scoreGap N - (1 / (2 * z) - 1 / (2 * N)) ∧
    Gamma.scoreGap z - Gamma.scoreGap N - (1 / (2 * z) - 1 / (2 * N)) ≤
      1 / (12 * z ^ 2) - 1 / (12 * N ^ 2)
```

#### `normalized_sample_cubic_bound`

A centered standardized sample has the stated normalized cubic log remainder.

```lean
theorem normalized_sample_cubic_bound (hn : 0 < n) {D m : ℝ}
    (hD : 0 < D) (hD' : D ≤ 1 / 100) (hm₀ : 0 < m) (hm₁ : m < 1)
    (T : Fin n → ℝ) (hT : ∀ i, |T i| ≤ 2 * D) (hcenter : ∑ i, T i = 0) :
    |leftGap T m / (1 - m) ^ 2 - rightGap T m / m ^ 2 +
      (∑ i, T i ^ 3) / n / 3| ≤ 16 * D ^ 4 / 3
```

#### `normalized_score_moment_bound`

The two normalized Gamma score equations force the third central log moment bound given the logistic standardization estimates.

```lean
theorem normalized_score_moment_bound (hn : 0 < n) {D m N : ℝ}
    (hD : 0 < D) (hD' : D ≤ 1 / 100) (hm₀ : 0 < m) (hm₁ : m < 1) (hN : 0 < N)
    (T v : Fin n → ℝ) (hT : ∀ i, |T i| ≤ 2 * D) (hcenter : ∑ i, T i = 0)
    (hv : ∀ i, |v i| ≤ D) (hdiff : ∀ i, |T i - v i| ≤ 3 * D ^ 2)
    (hleft : leftGap T m = Gamma.scoreGap (m * N) - Gamma.scoreGap N)
    (hright : rightGap T m = Gamma.scoreGap ((1 - m) * N) - Gamma.scoreGap N) :
    |(∑ i, v i ^ 3) / n| ≤ 69 * D ^ 4
```

#### `logistic_standardization`

The log-observation spread controls the standardized logistic deviations uniformly in log scale.

```lean
theorem logistic_standardization (hn : 0 < n) {D L U : ℝ}
    (hD : 0 < D) (hD' : D ≤ 1 / 100) (hspan : U - L ≤ D)
    (s : ℝ) (y : Fin n → ℝ) (hy : ∀ i, y i ∈ Icc L U) :
    let m := avg (fun i ↦ logistic s (y i))
    let v := fun i ↦ y i - avg y
    let T := fun i ↦ (logistic s (y i) - m) / (m * (1 - m))
    0 < m ∧ m < 1 ∧ (∑ i, T i = 0) ∧
      (∀ i, |v i| ≤ D) ∧ (∀ i, |T i - v i| ≤ 3 * D ^ 2) ∧ ∀ i, |T i| ≤ 2 * D
```

#### `narrow_span_stationary_bound`

At every finite stationary point the third central moment of the powered log data satisfies the bound.

```lean
theorem narrow_span_stationary_bound (hn : 0 < n) {D L U : ℝ}
    (hD : 0 < D) (hD' : D ≤ 1 / 100) (hspan : U - L ≤ D)
    (y : Fin n → ℝ) (hy : ∀ i, y i ∈ Icc L U)
    (s : ℝ) {p q : ℝ} (hp : 0 < p) (hq : 0 < q)
    (hs : deriv (fun t ↦ chartLik y t p q) s = 0)
    (hp' : deriv (fun t ↦ chartLik y s t q) p = 0)
    (hq' : deriv (fun t ↦ chartLik y s p t) q = 0) :
    |(∑ i, (y i - avg y) ^ 3) / n| ≤ 69 * D ^ 4
```

#### `held_power_narrow_span_no_mle`

A third central log moment exceeding the narrow-span bound excludes every finite physical fit with threshold and power held.

```lean
theorem held_power_narrow_span_no_mle (hn : 0 < n) {x : Fin n → ℝ} {μ a D L U : ℝ}
    (hx : ∀ i, μ < x i) (ha : 0 < a)
    (hD : 0 < D) (hD' : D ≤ 1 / 100) (hspan : U - L ≤ D)
    (hy : ∀ i, log (poweredExcess x μ a i) ∈ Icc L U)
    (hskew : 69 * D ^ 4 < |(∑ i, (log (poweredExcess x μ a i) -
      (∑ j, log (poweredExcess x μ a j)) / n) ^ 3) / n|) :
    ¬ ∃ z ∈ Ioi (0 : ℝ) ×ˢ Beta.domain,
      IsMaxOn (fun w : ℝ × (ℝ × ℝ) ↦ logLikelihood x μ a w.1 w.2)
        (Ioi (0 : ℝ) ×ˢ Beta.domain) z
```
