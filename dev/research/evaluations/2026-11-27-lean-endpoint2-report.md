# Lean threshold and endpoint proofs

TASK-419, branch `codex/lean-endpoint2`. Lean checked 71 public theorems in 13 modules. The inventory identifies proof routes covering 179 of the 231 requested keys, including explicit parameter specializations and combinations of branch theorems. 52 keys remain open below. The atlas fragment leaves all coverage decisions to independent review. [Coverage targets](2026-11-27-atlas-coverage3-report.md#written-claims-awaiting-full-lean-coverage) · [Atlas fragment](../existence-atlas/lean-map.d/lean-endpoint2.csv)

## Proofs and checks

| Family | Requested keys | Proof routes covering the stated branches | Open |
| --- | ---: | ---: | ---: |
| `fpareto` | 66 | 53 | 13 |
| `pareto2` | 15 | 14 | 1 |
| `pareto3` | 16 | 15 | 1 |
| `pareto4` | 46 | 38 | 8 |
| `nsbeta` | 37 | 31 | 6 |
| `genbeta` | 51 | 28 | 23 |

The workstation ran the focused Lean checks through the single-slot lock. It then built the umbrella with temporary imports for all added modules, using one pinned CPU. Lake completed all 8004 jobs successfully. The `--old` flag kept the existing Mathlib cache in use. The umbrella was restored byte for byte and passed `git diff --exit-code UnivariateML.lean`.

```sh
cd dev/lean
for file in UnivariateML/Generic/Endpoint2*.lean UnivariateML/Family/Endpoint2*.lean; do
  /tmp/umvl-cpu/run nice -n 10 /home/jonas/.elan/bin/lake env lean "$file" || exit
done
# With the temporary public imports present
cpu=$(python3 -c 'import os; print(min(os.sched_getaffinity(0)))')
/tmp/umvl-cpu/run taskset -c "$cpu" nice -n 10 /home/jonas/.elan/bin/lake --old build
```

## Scope and source discrepancies

- Pareto II all-free divergence uses a coupled scale and threshold path at a small fixed shape. The lower power is one in this family, so the fixed-scale threshold spike belongs to the other Pareto slices. The held-threshold `limit:1` target is written in terms of the numerical margin. The exact no-witness comparison already in Lean and the new threshold-contact criterion leave that detector obligation open.
- The nonstandard-beta `shape1` and `shape2` failure cells use threshold-family wording about simultaneous scale and shape collapse. The supported proof mechanism here is the free-endpoint spike at a subunit free shape. For the one-held-end cases, zero or infinite held-side density factors are excluded from the positive finite domain before considering the moving endpoint.
- At a fixed Pareto IV scale, the log likelihood at the Pareto trace includes one `-log 2` per at-scale observation. The row `min+scale / limit:5` uses that contact trace. The literal fixed-scale path at a specified constant product of shapes remains a separate obligation.
- The archived Feller–Pareto and Pareto IV formulas for positive excesses are interior charts. The normalized threshold kernel supplies the finite R7a density at unit lower power and zero gap, including the inverse-scale factor.
- The proofs use nonempty exact-real samples. Threshold gaps are normalized by the sample minimum where indicated. Nonstandard-beta affine coordinates use the positive sample range. Finite positive domains include contact exactly at a unit endpoint power. `path` results are joint sample log-likelihood limits. Distributional convergence is a separate claim where the source explicitly asks for it.

The principal written arguments are the threshold proof, the held-threshold Feller–Pareto compactification, and the nonstandard and generalized beta hold-mask analyses. [Thresholds](2026-11-25-thresh-math-report.md) · [Feller–Pareto held profiles](2026-11-27-lsheld-atlas-math-report.md#fpareto-held-min) · [Nonstandard beta](2026-11-25-nsbeta-math-report.md) · [Generalized beta](2026-11-27-genbeta-atlas-math-report.md)

## Effort by target group

| Target group | Proof work |
| --- | --- |
| Structural threshold masks | Split all minimum ties from the continuous remainder. Proved the fixed-shape scale-collapse exponent and the simultaneous upper-shape/scale route using the Gamma recurrence and the comparison of linear and logarithmic growth. |
| Fixed regular threshold | Proved the weighted rearrangement inequality directly as a double sum. Used analytic score signs and the intermediate value theorem to locate the unique interior root. Closed contact follows from monotonicity at lower power one. |
| Fixed shapes with scale free | Bounded the log kernel and its logarithm-weighted form, built compact positive superlevels, and computed the sharp equality envelope using the one-observation mode from the fixed-scale theorem. |
| Held-threshold Feller–Pareto | Used the powered beta-prime transfer, established compact power/intercept levels, and proved strict concavity in the two free coordinates. Read the exterior compactification by modes and tail rates to identify the unresolved face and stationary-component obligations. |
| Pareto II | Resolved the zero-excess branch beside the existing Lomax criterion, profiled shape at fixed scale, and checked all three signs of the collapse exponent at held shape. Transferred both maximizers of the exact three-level sample by reciprocal scale. |
| Pareto III and IV | Specialized the threshold kernel and constant path, transferred the exact Pareto IV maximizing set and the powered Weibull path, and combined the existing held-scale and held-shape branch results with their complements. |
| Nonstandard beta | Proved and transported the shared inverse-width score, including tied farthest observations and zero distances. Maximized the regular two-endpoint density on a closed affine triangle and proved uniqueness by the midpoint inequality, with direct width minimization for uniform data. |
| Generalized beta | Transferred fixed-power fits to beta, reduced the fixed-shape moving endpoint to the shared width score, and proved exact hyperbolic ridges. For both beta shapes held, bounded power and log-endpoint superlevels and used strict concavity. Reviewed the remaining joint-rate and face proofs for the open masks. |

## Open keys

| Family and held mask | Keys | Remaining proof |
| --- | --- | --- |
| `fpareto` · `min` | `criterion` | Construct the compactified positive-level envelope, prove the face supremum is reached in the envelope, and compare it to the stationary heights. |
| `fpareto` · `min+shape1` | `criterion` | Construct and optimize the inverse transformed-gamma and upper power faces and formalize the stationary-height comparison. |
| `fpareto` · `min+shape3` | `criterion` | Construct and optimize the transformed-gamma and lower Pareto faces and formalize the stationary-height comparison. |
| `fpareto` · `scale+shape2+shape3` | `criterion` | Prove uniform compact positive superlevels with the upper beta shape free, including the lower-power-one regime. |
| `fpareto` · `min` | `failure` | Classify every positive nonconstant escape using the mode, peak, power and two tail rates. Prove normalizer convergence and the full contact trace. |
| `fpareto` · `min+shape1` | `failure` | Show the inverse transformed-gamma and upper power faces exhaust nonconstant positive escapes, including the moving-cutoff trace. The constant ray is checked. |
| `fpareto` · `min+shape3` | `failure` | Show the transformed-gamma and lower Pareto faces exhaust nonconstant positive escapes, including the moving-cutoff trace. The constant ray is checked. |
| `fpareto` · `scale+shape2+shape3` | `failure` | Formalize the constant-sample path coupling the upper shape to the shrinking threshold gap. The subunit fixed-coefficient spike is checked. |
| `fpareto` · `min` | `uniqueness` | Prove finiteness and constant heights of the connected stationary components. Conditional power-scale uniqueness is checked. |
| `fpareto` · `min+shape1` | `uniqueness` | Prove stationarity is definable in the required o-minimal expansion and deduce finitely many components. The conditional maximizer is checked. |
| `fpareto` · `min+shape3` | `uniqueness` | Prove stationarity is definable in the required o-minimal expansion and deduce finitely many components. The conditional maximizer is checked. |
| `fpareto` · `scale+shape2+shape3` | `uniqueness` | Complete the zero-aware beta-shape profile at lower power one and transfer the unique contact optimizer. |
| `fpareto` · `shape1+shape2+shape3` | `uniqueness` | Prove the profiled normalized-score ordering. One direct route compares two scale roots at distinct gaps, uses the equal sums of the logistic coordinates, and weights their single-crossing difference by inverse excess. This step still needs a formal strict inequality. |
| `pareto2` · `min` | `limit:1` | Connect the dossier's numerical profile bound and its 10⁻⁸ n margin to the exact whole-domain detector. The existing exact no-witness implication and the new contact criterion cover different predicates. |
| `pareto3` · `shape` | `uniqueness` | Transfer the two-coordinate profiled score and prove the scale and threshold optimizer is unique at E>0. |
| `pareto4` · `scale+shape2` | `criterion` | Formalize the compact threshold profile with the first shape free at fixed scale and power. |
| `pareto4` · `scale+shape2` | `failure` | Give the constant concentration path with first shape and threshold coupled, using the Burr normalizer. The fixed-shape subunit spike is checked. |
| `pareto4` · `scale+shape2` | `uniqueness` | Make the contact reduction at power one explicit and transport the zero-aware Pareto II shape optimum. |
| `pareto4` · `shape1+shape2` | `uniqueness` | Complete the normalized-score ordering at distinct fitted scales and deduce global uniqueness from the two stationarity equations. |
| `pareto4` · `min` | `path:4` | Formalize the literal fixed-scale path with constant shape product and the per-tie factor at the cutoff, including zero limiting likelihood when an excess is below the cutoff. |
| `pareto4` · `min+scale` | `path:2` | Give the explicit diverging-power path with the required shape adjustment when the common excess lies strictly below held scale. The likelihood is already proved unbounded on that branch. |
| `pareto4` · `min+scale` | `path:4`, `path:5` | Formalize the literal fixed-scale path with constant shape product and the per-tie factor at the cutoff, including zero limiting likelihood when an excess is below the cutoff. |
| `nsbeta` · `max+shape1` | `criterion` | Profile the free held-side shape as a function of width and prove compact level sets on nonconstant positive distances. |
| `nsbeta` · `min+shape2` | `criterion` | Profile the free held-side shape as a function of width and prove compact level sets on nonconstant positive distances. |
| `nsbeta` · `max+shape1` | `failure` | Formalize mixed width-shape concentration at constants and the extended held-contact factors and joint boundary behavior. |
| `nsbeta` · `min+shape2` | `failure` | Formalize mixed width-shape concentration at constants and the extended held-contact factors and joint boundary behavior. |
| `nsbeta` · `max+shape1` | `uniqueness` | Formalize the trigamma lower bound and use it to make every profile-score crossing strictly downward. |
| `nsbeta` · `min+shape2` | `uniqueness` | Formalize the trigamma lower bound and use it to make every profile-score crossing strictly downward. |
| `genbeta` · `scale` | `criterion` | Prove the exhaustive positive-level envelope and the fitted gamma versus moving-power versus stationary-value criterion. |
| `genbeta` · `scale+shape1` | `criterion` | Prove the two power-end profile bounds with first shape and endpoint fixed. |
| `genbeta` · `scale+shape2` | `criterion` | Compare stationary values to the applicable fitted gamma and fixed-endpoint-power faces for q≠1, on constants and nonconstants. |
| `genbeta` · `shape2` | `criterion` | Prove the q>1 shifted-gamma/power envelope and stationary comparison, including equality. |
| `genbeta` · `shape2+shape3` | `criterion` | Prove coercivity for fixed power and q>1 with free first shape and endpoint, and close the corresponding constant branch. |
| `genbeta` · `scale` | `failure` | Prove only gamma and moving power survive for nonconstant data, including the complete contact trace. |
| `genbeta` · `scale+shape1` | `failure` | Prove the fixed-first-shape concentration ray and exclude every nonconstant maximizing escape. |
| `genbeta` · `scale+shape2` | `failure` | Formalize the signed exclusion of the fixed-endpoint power face for q>1 and the remaining q≠1 alternatives. |
| `genbeta` · `shape1` | `failure` | Prove the necessary joint divergence rates and exhaustive maximizing escapes under this exact hold, including the permitted q≥1 constant branch where applicable. The subunit spike and empty finite maximizing set are checked. |
| `genbeta` · `shape1+shape2` | `failure` | Expose compact-superlevel control as a full no-maximizing-escape statement in physical coordinates for q≥1 on nonconstants. Both divergent branches and exact finite attainment are checked. |
| `genbeta` · `shape1+shape2+shape3` | `failure` | Prove that every divergent strict-interior sequence approaches the sample maximum and that all other escapes lose, in addition to the checked spike and maximizing root. |
| `genbeta` · `shape1+shape3` | `failure` | Prove the necessary joint divergence rates and exhaustive maximizing escapes under this exact hold, including the permitted q≥1 constant branch where applicable. The subunit spike and empty finite maximizing set are checked. |
| `genbeta` · `shape2` | `failure` | Classify all shifted-gamma and power maximizing escapes for q>1 and formalize the q=1 escape along the attained ridge. |
| `genbeta` · `shape2+shape3` | `failure` | Formalize the large-first-shape constant collapse and the no-escape result at nonconstant data. |
| `genbeta` · `shape3` | `failure` | Prove the necessary joint divergence rates and exhaustive maximizing escapes under this exact hold, including the permitted q≥1 constant branch where applicable. The subunit spike and empty finite maximizing set are checked. |
| `genbeta` · `scale` | `uniqueness` | Prove analytic profiling in power and isolated zeros on the region strictly above both faces. |
| `genbeta` · `scale+shape1` | `uniqueness` | Prove the strict power-profile curvature inequality after maximizing the second shape. |
| `genbeta` · `shape2+shape3` | `uniqueness` | Bound the positive profile Hessian correction by the strictly larger trigamma difference for q>1. |
| `genbeta` · `scale` | `path:2` | Prove the coupled beta-to-gamma normalization limit as p increases and the power vanishes, with fixed positive product and upper shape. |
| `genbeta` · `scale` | `path:3` | Prove the moving-cutoff weak limit and full joint contact trace with log(q)/power tending to the cutoff displacement. |
| `genbeta` · `scale+shape1` | `path:1`, `limit:1` | Construct the constant-data concentration ray with first shape and endpoint fixed, using the coupled upper-shape and power normalizer bound. |
| `genbeta` · `scale+shape2` | `path:2` | Prove the coupled beta-to-gamma normalization limit as p increases and the power vanishes, with fixed positive product and upper shape. |

## Public theorem statements

### `UnivariateML.Endpoint2FellerPareto.threshold_spike`

A subunit lower power gives divergence as the threshold approaches the minimum from below, with all ties at that minimum included. [Endpoint2FellerPareto.lean](../../lean/UnivariateML/Family/Endpoint2FellerPareto.lean#L39)

```lean
theorem threshold_spike {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hz : ∃ i, d i = 0) {a b c s K : ℝ} (ha : a < 1) (hc : 0 < c)
    (hs : 0 < s) :
    Tendsto (logLik d a b c s K) (𝓝[>] 0) atTop
```

### `UnivariateML.Endpoint2FellerPareto.unbounded_of_threshold_path`

Every model admitting the full subunit-power threshold path has an unbounded objective, hence no finite maximizer. The ambient model may include closed threshold contact and additional free parameters. [Endpoint2FellerPareto.lean](../../lean/UnivariateML/Family/Endpoint2FellerPareto.lean#L92)

```lean
theorem unbounded_of_threshold_path {P : Type*} {D : Set P} {L : P → ℝ}
    {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i) (hz : ∃ i, d i = 0)
    {a b c s K : ℝ} (ha : a < 1) (hc : 0 < c) (hs : 0 < s) (path : ℝ → P)
    (hpath : ∀ t > 0, path t ∈ D ∧ L (path t) = logLik d a b c s K t) :
    ¬ BddAbove (L '' D) ∧ ¬ ∃ p ∈ D, IsMaxOn L D p
```

### `UnivariateML.Endpoint2FellerPareto.threshold_contact_unique`

At lower power one, closed contact uniquely maximizes over the threshold gap for any fixed positive scale, power, and upper tail power. [Endpoint2FellerPareto.lean](../../lean/UnivariateML/Family/Endpoint2FellerPareto.lean#L114)

```lean
theorem threshold_contact_unique (hn : 0 < n) {d : Fin n → ℝ}
    (hd : ∀ i, 0 ≤ d i) {b c s K : ℝ} (hb : 0 < b) (hc : 0 < c) (hs : 0 < s) :
    ∃! t, t ∈ Ici (0 : ℝ) ∧ IsMaxOn (logLik d 1 b c s K) (Ici 0) t
```

### `UnivariateML.Endpoint2FellerPareto.free_lower_shape_or_power`

If either the lower beta shape or the power is free, the finite interior log likelihood is unbounded with any valid holds on scale and upper shape. The parameter `t` is the positive gap between threshold and sample minimum. [Endpoint2FellerPareto.lean](../../lean/UnivariateML/Family/Endpoint2FellerPareto.lean#L151)

```lean
theorem free_lower_shape_or_power {d : Fin n → ℝ}
    (hd : ∀ i, 0 ≤ d i) (hz : ∃ i, d i = 0)
    (P C : Option ℝ) (hfree : P = none ∨ C = none)
    (hP : 0 < P.getD 1) (hC : 0 < C.getD 1) {q s : ℝ} (hq : 0 < q) (hs : 0 < s) :
    ∀ M : ℝ, ∃ p q' c s' t : ℝ,
      0 < p ∧ P.getD p = p ∧ 0 < q' ∧ q' = q ∧ 0 < c ∧ C.getD c = c ∧
        s' = s ∧ 0 < t ∧ M < interiorLogLik d p q' c s' t
```

### `UnivariateML.Endpoint2FellerPareto.constant_excess_power_path`

For a constant positive excess, keep its scale and both beta shapes fixed. The ratio of every excess to the scale is one, so increasing the power adds exactly `n * log c` to the log likelihood. [Endpoint2FellerPareto.lean](../../lean/UnivariateML/Family/Endpoint2FellerPareto.lean#L182)

```lean
theorem constant_excess_power_path (hn : 0 < n) {s : ℝ} (hs : 0 < s)
    (p q B : ℝ) :
    Tendsto
      (fun c ↦ logLik (fun _ : Fin n ↦ s) (c * p) (c * q) c s
        (log c - log s - B) 0)
      atTop atTop
```

### `UnivariateML.Endpoint2FellerPareto.scale_collapse`

Along the joint scale and threshold ray `t = s`, a minimum contributes coefficient `-1` and a larger observation contributes `b` to the leading `log s` term. A negative sum gives divergence, including constant samples. [Endpoint2FellerPareto.lean](../../lean/UnivariateML/Family/Endpoint2FellerPareto.lean#L211)

```lean
theorem scale_collapse {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    {a b c K : ℝ} (hc : 0 < c)
    (hE : (∑ i, if d i = 0 then (-1 : ℝ) else b) < 0) :
    Tendsto (fun s ↦ logLik d a b c s (K - log s) s) (𝓝[>] 0) atTop
```

### `UnivariateML.Endpoint2FellerPareto.vanishing_scale_upper_shape`

With scale and upper beta shape free, the simultaneous path `q = 1/T`, `s = t = exp(-T)` diverges with arbitrary fixed positive lower beta shape and power. Ties at the minimum contribute individually. [Endpoint2FellerPareto.lean](../../lean/UnivariateML/Family/Endpoint2FellerPareto.lean#L264)

```lean
theorem vanishing_scale_upper_shape {d : Fin n → ℝ}
    (hd : ∀ i, 0 ≤ d i) (hz : ∃ i, d i = 0)
    {p c : ℝ} (hp : 0 < p) (hc : 0 < c) :
    Tendsto (fun T ↦ interiorLogLik d p T⁻¹ c (exp (-T)) (exp (-T))) atTop atTop
```

### `UnivariateML.Endpoint2FellerPareto.structural_unbounded`

All structural free-threshold masks have unbounded likelihood and an empty finite maximizing set. This uses either a subunit lower power or the simultaneous vanishing upper-shape and scale path. [Endpoint2FellerPareto.lean](../../lean/UnivariateML/Family/Endpoint2FellerPareto.lean#L375)

```lean
theorem structural_unbounded {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hz : ∃ i, d i = 0) (P Q C S : Option ℝ)
    (hP : 0 < P.getD 1) (hQ : 0 < Q.getD 1) (hC : 0 < C.getD 1) (hS : 0 < S.getD 1)
    (hfree : (P = none ∨ C = none) ∨ (Q = none ∧ S = none)) :
    let L := fun v : ThresholdParameters ↦
      interiorLogLik d v.lowerShape v.upperShape v.power v.scale v.gap
    ¬ BddAbove (L '' thresholdDomain P Q C S) ∧
      ¬ ∃ v ∈ thresholdDomain P Q C S, IsMaxOn L (thresholdDomain P Q C S) v
```

### `UnivariateML.Endpoint2FellerParetoFreeScale.zero_exponent_envelope`

At a zero scale-collapse exponent, the supremum equals the sharp contact envelope and is strictly above all finite points. [Endpoint2FellerParetoFreeScale.lean](../../lean/UnivariateML/Family/Endpoint2FellerParetoFreeScale.lean#L188)

```lean
theorem zero_exponent_envelope {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hz : ∃ i, d i = 0) {a b c K : ℝ} (ha : 1 ≤ a) (hb : 0 < b) (hc : 0 < c)
    (hE : exponent d b = 0) :
    ∃ C : ℝ, IsLUB (logLik d a b c K '' domain a) C ∧
      ∀ v ∈ domain a, logLik d a b c K v < C
```

### `UnivariateML.Endpoint2FellerParetoFreeScale.negative_exponent_unbounded`

A negative collapse exponent gives unbounded likelihood in the positive finite domain, including constant samples. [Endpoint2FellerParetoFreeScale.lean](../../lean/UnivariateML/Family/Endpoint2FellerParetoFreeScale.lean#L244)

```lean
theorem negative_exponent_unbounded {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    {a b c K : ℝ} (hc : 0 < c) (hE : exponent d b < 0) :
    ¬ BddAbove (logLik d a b c K '' domain a)
```

### `UnivariateML.Endpoint2FellerParetoFreeScale.fixed_shapes_criterion`

A finite joint threshold and scale maximum exists exactly for lower power at least one and positive scale-collapse exponent. [Endpoint2FellerParetoFreeScale.lean](../../lean/UnivariateML/Family/Endpoint2FellerParetoFreeScale.lean#L422)

```lean
theorem fixed_shapes_criterion {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hz : ∃ i, d i = 0) {a b c K : ℝ} (hb : 0 < b) (hc : 0 < c) :
    (∃ v ∈ domain a, IsMaxOn (logLik d a b c K) (domain a) v) ↔
      1 ≤ a ∧ 0 < exponent d b
```

### `UnivariateML.Endpoint2FellerParetoHeld.conditional_power_scale`

At fixed beta shapes, every nonconstant positive-excess sample has a unique fitted power and scale. This applies under either separate shape hold when conditioning on the remaining beta shape. [Endpoint2FellerParetoHeld.lean](../../lean/UnivariateML/Family/Endpoint2FellerParetoHeld.lean#L267)

```lean
theorem conditional_power_scale (hn : 0 < n) {x : Fin n → ℝ} {μ p q : ℝ}
    (hx : ∀ i, μ < x i) (hp : 0 < p) (hq : 0 < q) (hne : ∃ i j, x i ≠ x j) :
    let D : Set (ℝ × ℝ) := Ioi 0 ×ˢ Ioi 0
    let L := fun v : ℝ × ℝ ↦ ShapeFacesFellerPareto.logLikelihood x μ v.1 v.2 (p,q)
    ∃! v, v ∈ D ∧ IsMaxOn L D v
```

### `UnivariateML.Endpoint2FellerParetoHeld.held_power_scale_criterion`

Holding threshold, power and scale leaves a two-shape beta-prime fit. The constant branch has unbounded likelihood. [Endpoint2FellerParetoHeld.lean](../../lean/UnivariateML/Family/Endpoint2FellerParetoHeld.lean#L286)

```lean
theorem held_power_scale_criterion (hn : 0 < n) {x : Fin n → ℝ} {μ c s : ℝ}
    (hx : ∀ i, μ < x i) (hc : 0 < c) (hs : 0 < s) :
    let L := ShapeFacesFellerPareto.logLikelihood x μ c s
    ((∃ p ∈ Beta.domain, IsMaxOn L Beta.domain p) ↔ ∃ i j, x i ≠ x j) ∧
    ((∀ i j, x i = x j) → ¬ BddAbove (L '' Beta.domain))
```

### `UnivariateML.Endpoint2FellerParetoHeld.constant_sample_path`

Fixed beta shapes concentrate at any positive common excess by increasing power with scale set to that excess. [Endpoint2FellerParetoHeld.lean](../../lean/UnivariateML/Family/Endpoint2FellerParetoHeld.lean#L309)

```lean
theorem constant_sample_path (hn : 0 < n) {v μ : ℝ} (hv : μ < v) (p q : ℝ) :
    Tendsto (fun c ↦ ShapeFacesFellerPareto.logLikelihood (fun _ : Fin n ↦ v)
      μ c (v - μ) (p,q)) atTop atTop
```

### `UnivariateML.Endpoint2FellerParetoHeld.held_shapes_criterion`

Holding threshold and both beta shapes gives a finite fit exactly on nonconstant positive excesses. Every attained fit is unique. [Endpoint2FellerParetoHeld.lean](../../lean/UnivariateML/Family/Endpoint2FellerParetoHeld.lean#L324)

```lean
theorem held_shapes_criterion (hn : 0 < n) {x : Fin n → ℝ} {μ p q : ℝ}
    (hx : ∀ i, μ < x i) (hp : 0 < p) (hq : 0 < q) :
    let D : Set (ℝ × ℝ) := Ioi 0 ×ˢ Ioi 0
    let L := fun v : ℝ × ℝ ↦ ShapeFacesFellerPareto.logLikelihood x μ v.1 v.2 (p,q)
    ((∃ v ∈ D, IsMaxOn L D v) ↔ ∃ i j, x i ≠ x j) ∧
      ((∃ i j, x i ≠ x j) → ∃! v, v ∈ D ∧ IsMaxOn L D v)
```

### `UnivariateML.Endpoint2FellerParetoRegular.score_strictAnti`

The normalized threshold score is strictly decreasing on the interior, including on constant samples and samples with tied minima. [Endpoint2FellerParetoRegular.lean](../../lean/UnivariateML/Family/Endpoint2FellerParetoRegular.lean#L95)

```lean
theorem score_strictAnti (hn : 0 < n) {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    {a b c s : ℝ} (hab : 0 < a + b) (hc : 0 < c) (hs : 0 < s) :
    StrictAntiOn (normalizedScore d a b c s) (Ioi 0)
```

### `UnivariateML.Endpoint2FellerParetoRegular.interior_threshold_fit`

At lower power above one the normalized score has exactly one interior root, and that root is the unique likelihood maximum. [Endpoint2FellerParetoRegular.lean](../../lean/UnivariateML/Family/Endpoint2FellerParetoRegular.lean#L262)

```lean
theorem interior_threshold_fit {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hz : ∃ i, d i = 0) {a b c s K : ℝ} (ha : 1 < a)
    (hb : 0 < b) (hc : 0 < c) (hs : 0 < s) :
    let L := Endpoint2FellerPareto.logLik d a b c s K
    (∃! t, t ∈ Ioi (0 : ℝ) ∧ normalizedScore d a b c s t = 0) ∧
      ∀ t ∈ Ioi 0, IsMaxOn L (Ioi 0) t ↔ normalizedScore d a b c s t = 0
```

### `UnivariateML.Endpoint2FellerParetoRegular.fixed_scale_shapes_fit`

Holding all regular coefficients gives a unique finite threshold exactly at lower power at least one. The subunit branch is unbounded, and the unit lower power has a maximum at closed contact. [Endpoint2FellerParetoRegular.lean](../../lean/UnivariateML/Family/Endpoint2FellerParetoRegular.lean#L338)

```lean
theorem fixed_scale_shapes_fit {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hz : ∃ i, d i = 0) {a b c s K : ℝ} (hb : 0 < b) (hc : 0 < c) (hs : 0 < s) :
    let D := if a = 1 then Ici (0 : ℝ) else Ioi 0
    let L := Endpoint2FellerPareto.logLik d a b c s K
    ((∃ t ∈ D, IsMaxOn L D t) ↔ 1 ≤ a) ∧
      (1 ≤ a → ∃! t, t ∈ D ∧ IsMaxOn L D t) ∧
      (a < 1 → ¬ BddAbove (L '' D)) ∧
      (a = 1 → IsMaxOn L D 0)
```

### `UnivariateML.Endpoint2GeneralizedBeta.held_first_shape_fit`

With scale, power and first beta shape fixed, the free second beta shape always has a unique maximum on strictly interior samples. [Endpoint2GeneralizedBeta.lean](../../lean/UnivariateML/Family/Endpoint2GeneralizedBeta.lean#L56)

```lean
theorem held_first_shape_fit (hn : 0 < n) {x : Fin n → ℝ} {b c p : ℝ}
    (hb : 0 < b) (hc : 0 < c) (hp : 0 < p) (hx : ∀ i, x i ∈ Ioo (0 : ℝ) b) :
    ∃! q, q ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun v ↦ logLik x b p v c) (Ioi 0) q
```

### `UnivariateML.Endpoint2GeneralizedBeta.held_second_shape_fit`

With scale, power and second beta shape fixed, the free first beta shape always has a unique maximum on strictly interior samples. [Endpoint2GeneralizedBeta.lean](../../lean/UnivariateML/Family/Endpoint2GeneralizedBeta.lean#L64)

```lean
theorem held_second_shape_fit (hn : 0 < n) {x : Fin n → ℝ} {b c q : ℝ}
    (hb : 0 < b) (hc : 0 < c) (hq : 0 < q) (hx : ∀ i, x i ∈ Ioo (0 : ℝ) b) :
    ∃! p, p ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun v ↦ logLik x b v q c) (Ioi 0) p
```

### `UnivariateML.Endpoint2GeneralizedBeta.held_scale_power_constant_path`

Holding scale and power gives the beta fixed-mean concentration ray on any constant interior sample. [Endpoint2GeneralizedBeta.lean](../../lean/UnivariateML/Family/Endpoint2GeneralizedBeta.lean#L72)

```lean
theorem held_scale_power_constant_path (hn : 0 < n) {b c v : ℝ} (hb : 0 < b) (hc : 0 < c)
    (hv : v ∈ Ioo (0 : ℝ) b) :
    let u := (v / b) ^ c
    Tendsto (fun t ↦ logLik (fun _ : Fin n ↦ v) b (t * u) (t * (1 - u)) c)
      atTop atTop
```

### `UnivariateML.Endpoint2GeneralizedBeta.held_scale_power_criterion`

With scale and power fixed, the two beta shapes have a unique global maximum exactly on nonconstant interior data. Constants have no finite fit. [Endpoint2GeneralizedBeta.lean](../../lean/UnivariateML/Family/Endpoint2GeneralizedBeta.lean#L86)

```lean
theorem held_scale_power_criterion (hn : 0 < n) {x : Fin n → ℝ} {b c : ℝ}
    (hb : 0 < b) (hc : 0 < c) (hx : ∀ i, x i ∈ Ioo (0 : ℝ) b) :
    let L : ℝ × ℝ → ℝ := fun p ↦ logLik x b p.1 p.2 c
    ((∃ p ∈ Beta.domain, IsMaxOn L Beta.domain p) ↔ ∃ i j, x i ≠ x j) ∧
      ((∃! p, p ∈ Beta.domain ∧ IsMaxOn L Beta.domain p) ↔ ∃ i j, x i ≠ x j)
```

### `UnivariateML.Endpoint2GeneralizedBeta.endpoint_spike`

A subunit upper beta shape gives a free-endpoint spike on every positive sample, with all tied maxima included. [Endpoint2GeneralizedBeta.lean](../../lean/UnivariateML/Family/Endpoint2GeneralizedBeta.lean#L178)

```lean
theorem endpoint_spike {x : Fin n → ℝ} {M p q c : ℝ} (hx : ∀ i, 0 < x i)
    (hle : ∀ i, x i ≤ M) (heq : ∃ i, x i = M) (hq : q < 1) (hc : 0 < c) :
    Tendsto (fun t ↦ logLik x (M / (1 - t) ^ c⁻¹) p q c) (𝓝[>] 0) atTop
```

### `UnivariateML.Endpoint2GeneralizedBeta.free_second_shape_unbounded`

A positive allowed first shape and power suffice for a divergent free-scale path whenever the second shape is free. The sets encode arbitrary holds on the first shape or power. [Endpoint2GeneralizedBeta.lean](../../lean/UnivariateML/Family/Endpoint2GeneralizedBeta.lean#L192)

```lean
theorem free_second_shape_unbounded {x : Fin n → ℝ} {M p c : ℝ} (hx : ∀ i, 0 < x i)
    (hle : ∀ i, x i ≤ M) (heq : ∃ i, x i = M) (hp : 0 < p) (hc : 0 < c)
    {A C : Set ℝ} (hA : p ∈ A) (hC : c ∈ C) :
    let D : Set (ℝ × ℝ × ℝ × ℝ) := {v | (M < v.1 ∨ (v.1 = M ∧ v.2.2.1 = 1)) ∧
      0 < v.2.1 ∧ v.2.1 ∈ A ∧ 0 < v.2.2.1 ∧ 0 < v.2.2.2 ∧ v.2.2.2 ∈ C}
    let L : ℝ × ℝ × ℝ × ℝ → ℝ := fun v ↦ logLik x v.1 v.2.1 v.2.2.1 v.2.2.2
    ¬ BddAbove (L '' D) ∧ ¬ ∃ v ∈ D, IsMaxOn L D v
```

### `UnivariateML.Endpoint2GeneralizedBeta.fixed_shapes_endpoint_fit`

When only the endpoint is free, a finite optimum exists exactly at upper shape at least one. At one it is closed contact. Above one the chart score characterizes the unique interior optimum. [Endpoint2GeneralizedBeta.lean](../../lean/UnivariateML/Family/Endpoint2GeneralizedBeta.lean#L225)

```lean
theorem fixed_shapes_endpoint_fit (hn : 0 < n) {x : Fin n → ℝ} {M p q c : ℝ}
    (hx : ∀ i, 0 < x i) (hle : ∀ i, x i ≤ M) (heq : ∃ i, x i = M)
    (hp : 0 < p) (hc : 0 < c) :
    let D := if q = 1 then Ici M else Ioi M
    let L := fun b ↦ logLik x b p q c
    ((∃ b ∈ D, IsMaxOn L D b) ↔ 1 ≤ q) ∧
    (q = 1 → IsMaxOn L D M ∧ ∀ b ∈ D, IsMaxOn L D b → b = M) ∧
    (1 < q → ∃! b, b ∈ D ∧ IsMaxOn L D b)
```

### `UnivariateML.Endpoint2GeneralizedBeta.fixed_shapes_endpoint_score`

At fixed upper shape above one, the unique endpoint optimum is the interior zero of the transformed score. [Endpoint2GeneralizedBeta.lean](../../lean/UnivariateML/Family/Endpoint2GeneralizedBeta.lean#L289)

```lean
theorem fixed_shapes_endpoint_score (hn : 0 < n) {x : Fin n → ℝ} {M p q c b : ℝ}
    (hx : ∀ i, 0 < x i) (hle : ∀ i, x i ≤ M) (heq : ∃ i, x i = M)
    (hp : 0 < p) (hq : 1 < q) (hc : 0 < c) (hb : M < b) :
    IsMaxOn (fun s ↦ logLik x s p q c) (Ioi M) b ↔
      Endpoint2BetaWidth.score (unitData x M c) q p ((M / b) ^ c) = 0
```

### `UnivariateML.Endpoint2GeneralizedBeta.held_scale_ridge`

At held scale and upper shape one, the full maximizing set is the product ridge. Constant interior samples satisfy the same formula. [Endpoint2GeneralizedBeta.lean](../../lean/UnivariateML/Family/Endpoint2GeneralizedBeta.lean#L356)

```lean
theorem held_scale_ridge (hn : 0 < n) {x : Fin n → ℝ} {b : ℝ}
    (hx : ∀ i, x i ∈ Ioo (0 : ℝ) b) (p : ℝ × ℝ) (hp : p ∈ Beta.domain) :
    IsMaxOn (fun u : ℝ × ℝ ↦ logLik x b u.1 1 u.2) Beta.domain p ↔
      p.1 * p.2 = (n : ℝ) / logDistanceSum x b
```

### `UnivariateML.Endpoint2GeneralizedBeta.free_endpoint_ridge`

With free scale and upper shape one, nonconstant data have the complete maximizing ridge at the closed sample maximum. [Endpoint2GeneralizedBeta.lean](../../lean/UnivariateML/Family/Endpoint2GeneralizedBeta.lean#L390)

```lean
theorem free_endpoint_ridge (hn : 0 < n) {x : Fin n → ℝ} {M : ℝ}
    (hx : ∀ i, 0 < x i) (hle : ∀ i, x i ≤ M) (hne : ∃ i, x i < M)
    (u : ℝ × ℝ × ℝ) (hu : u ∈ Ici M ×ˢ Beta.domain) :
    IsMaxOn (fun v : ℝ × ℝ × ℝ ↦ logLik x v.1 v.2.1 1 v.2.2)
        (Ici M ×ˢ Beta.domain) u ↔
      u.1 = M ∧ u.2.1 * u.2.2 = (n : ℝ) / logDistanceSum x M
```

### `UnivariateML.Endpoint2GeneralizedBeta.held_scale_two_point_ridge`

The binary sample at held scale one realizes the specified exact product ridge. [Endpoint2GeneralizedBeta.lean](../../lean/UnivariateML/Family/Endpoint2GeneralizedBeta.lean#L451)

```lean
theorem held_scale_two_point_ridge (p : ℝ × ℝ) (hp : p ∈ Beta.domain) :
    let x : Fin 2 → ℝ := ![1/2, 1/4]
    IsMaxOn (fun u : ℝ × ℝ ↦ logLik x 1 u.1 1 u.2) Beta.domain p ↔
      p.1 * p.2 = 2 / (3 * log 2)
```

### `UnivariateML.Endpoint2GeneralizedBeta.free_endpoint_two_point_ridge`

The binary sample whose maximum is one realizes the specified exact closed-contact ridge with the endpoint free. [Endpoint2GeneralizedBeta.lean](../../lean/UnivariateML/Family/Endpoint2GeneralizedBeta.lean#L468)

```lean
theorem free_endpoint_two_point_ridge (u : ℝ × ℝ × ℝ)
    (hu : u ∈ Ici (1 : ℝ) ×ˢ Beta.domain) :
    let x : Fin 2 → ℝ := ![1/2, 1]
    IsMaxOn (fun v : ℝ × ℝ × ℝ ↦ logLik x v.1 v.2.1 1 v.2.2)
        (Ici (1 : ℝ) ×ˢ Beta.domain) u ↔
      u.1 = 1 ∧ u.2.1 * u.2.2 = 2 / log 2
```

### `UnivariateML.Endpoint2GeneralizedBeta.constant_sample_free_power_path`

With fixed beta shapes and free power, a constant sample collapses at a shrinking positive log distance from the free endpoint. [Endpoint2GeneralizedBeta.lean](../../lean/UnivariateML/Family/Endpoint2GeneralizedBeta.lean#L487)

```lean
theorem constant_sample_free_power_path (hn : 0 < n) {v : ℝ} (hv : 0 < v) (p q : ℝ) :
    Tendsto (fun c ↦ logLik (fun _ : Fin n ↦ v) (v * exp c⁻¹) p q c) atTop atTop
```

### `UnivariateML.Endpoint2GeneralizedBeta.constant_sample_contact_rate_path`

At free-scale contact, upper shape one permits arbitrary concentration on a constant sample with either held positive power or held first shape through the corresponding positive product. [Endpoint2GeneralizedBeta.lean](../../lean/UnivariateML/Family/Endpoint2GeneralizedBeta.lean#L509)

```lean
theorem constant_sample_contact_rate_path (hn : 0 < n) {v : ℝ} (hv : 0 < v) :
    Tendsto (fun r ↦ logLik (fun _ : Fin n ↦ v) v r 1 1) atTop atTop
```

### `UnivariateML.Endpoint2GeneralizedBetaJoint.fixed_shapes_joint_regular`

For held positive first shape and held upper shape above one, every nonconstant positive sample has exactly one maximizing power and strictly containing endpoint. [Endpoint2GeneralizedBetaJoint.lean](../../lean/UnivariateML/Family/Endpoint2GeneralizedBetaJoint.lean#L274)

```lean
theorem fixed_shapes_joint_regular (hn : 0 < n) {x : Fin n → ℝ} {M p q : ℝ}
    (hx : ∀ i, 0 < x i) (hle : ∀ i, x i ≤ M) (heq : ∃ i, x i = M)
    (hne : ∃ i, x i < M) (hp : 0 < p) (hq : 1 < q) :
    let D := Ioi (0 : ℝ) ×ˢ Ioi M
    let L := fun v : ℝ × ℝ ↦ Endpoint2GeneralizedBeta.logLik x v.2 p q v.1
    ∃! v, v ∈ D ∧ IsMaxOn L D v
```

### `UnivariateML.Endpoint2GeneralizedBetaJoint.fixed_shapes_joint_criterion`

Holding both beta shapes gives a finite endpoint and power fit exactly for a nonconstant positive sample with upper shape at least one. The solution is unique, with closed endpoint precisely at shape one. [Endpoint2GeneralizedBetaJoint.lean](../../lean/UnivariateML/Family/Endpoint2GeneralizedBetaJoint.lean#L294)

```lean
theorem fixed_shapes_joint_criterion (hn : 0 < n) {x : Fin n → ℝ} {M p q : ℝ}
    (hx : ∀ i, 0 < x i) (hle : ∀ i, x i ≤ M) (heq : ∃ i, x i = M) (hp : 0 < p) :
    let D := Ioi (0 : ℝ) ×ˢ (if q = 1 then Ici M else Ioi M)
    let L := fun v : ℝ × ℝ ↦ Endpoint2GeneralizedBeta.logLik x v.2 p q v.1
    ((∃ v ∈ D, IsMaxOn L D v) ↔ 1 ≤ q ∧ ∃ i, x i < M) ∧
      ((1 ≤ q ∧ ∃ i, x i < M) → ∃! v, v ∈ D ∧ IsMaxOn L D v)
```

### `UnivariateML.Endpoint2NonstandardBeta.held_contact_classification`

Positive finite factors are possible exactly when the sample is strictly inside at the held end or the held shape permits closed contact. [Endpoint2NonstandardBeta.lean](../../lean/UnivariateML/Family/Endpoint2NonstandardBeta.lean#L46)

```lean
theorem held_contact_classification (z : Fin n → ℝ) (b : ℝ) :
    ¬ heldCompatible z b ↔ (∃ i, z i < 0) ∨ (b ≠ 1 ∧ ∃ i, z i = 0)
```

### `UnivariateML.Endpoint2NonstandardBeta.fixed_shapes_fit`

With fixed positive shapes and a positive farthest distance, the regular free endpoint has a unique maximizing width. This includes zeros when the held-side shape is one. [Endpoint2NonstandardBeta.lean](../../lean/UnivariateML/Family/Endpoint2NonstandardBeta.lean#L102)

```lean
theorem fixed_shapes_fit (hn : 0 < n) {z : Fin n → ℝ} {a b Z : ℝ}
    (hZ : 0 < Z) (hz : heldCompatible z b) (hle : ∀ i, z i ≤ Z)
    (hcontact : ∃ i, z i = Z) (ha : 1 ≤ a) (hb : 0 < b) :
    let D := if a = 1 then Ici Z else Ioi Z
    (∃! W, W ∈ D ∧ IsMaxOn (widthLogLik z a b) D W) ∧
      (a = 1 → IsMaxOn (widthLogLik z a b) D Z) ∧
      (1 < a → ∀ W ∈ D, IsMaxOn (widthLogLik z a b) D W ↔
        Endpoint2BetaWidth.score (fun i ↦ z i / Z) a b (Z / W) = 0)
```

### `UnivariateML.Endpoint2NonstandardBeta.fixed_shapes_spike`

For subunit free-side shape and positive held-side factors, width approaching the farthest observation gives unbounded likelihood. [Endpoint2NonstandardBeta.lean](../../lean/UnivariateML/Family/Endpoint2NonstandardBeta.lean#L170)

```lean
theorem fixed_shapes_spike {z : Fin n → ℝ} {a b Z : ℝ} (hZ : 0 < Z)
    (hz : heldCompatible z b) (hle : ∀ i, z i ≤ Z) (hcontact : ∃ i, z i = Z)
    (ha : a < 1) :
    Tendsto (fun t ↦ widthLogLik z a b (Z / (1 - t))) (𝓝[>] 0) atTop
```

### `UnivariateML.Endpoint2NonstandardBeta.zero_distances_collapse`

At held-side shape one and with every observation at the held end, shrinking width gives unbounded likelihood. [Endpoint2NonstandardBeta.lean](../../lean/UnivariateML/Family/Endpoint2NonstandardBeta.lean#L184)

```lean
theorem zero_distances_collapse (hn : 0 < n) (a : ℝ) :
    Tendsto (widthLogLik (fun _ : Fin n ↦ 0) a 1) (𝓝[>] 0) atTop
```

### `UnivariateML.Endpoint2NonstandardBeta.fixed_shapes_criterion`

The fixed-shape criterion covers contact at either end as well as incompatible distances and the all-zero sample. [Endpoint2NonstandardBeta.lean](../../lean/UnivariateML/Family/Endpoint2NonstandardBeta.lean#L200)

```lean
theorem fixed_shapes_criterion (hn : 0 < n) {z : Fin n → ℝ} {a b Z : ℝ}
    (hle : ∀ i, z i ≤ Z) (hZeq : ∃ i, z i = Z) (hb : 0 < b) :
    (∃ W ∈ finiteWidthDomain z a b,
      IsMaxOn (widthLogLik z a b) (finiteWidthDomain z a b) W) ↔
      0 < Z ∧ 1 ≤ a ∧ heldCompatible z b
```

### `UnivariateML.Endpoint2NonstandardBeta.free_endpoint_shape_no_fit`

Free shape at the moving endpoint precludes a positive finite fit, whether the opposite shape is held or free. The domain excludes held contacts whose fixed factor is zero or infinite. [Endpoint2NonstandardBeta.lean](../../lean/UnivariateML/Family/Endpoint2NonstandardBeta.lean#L263)

```lean
theorem free_endpoint_shape_no_fit (hn : 0 < n) {z : Fin n → ℝ} {Z : ℝ}
    (hle : ∀ i, z i ≤ Z) (heq : ∃ i, z i = Z) (B : Set ℝ) :
    let D : Set (ℝ × ℝ × ℝ) :=
      {v | 0 < v.2.1 ∧ 0 < v.2.2 ∧ v.2.2 ∈ B ∧
        v.1 ∈ finiteWidthDomain z v.2.1 v.2.2}
    let L := fun v : ℝ × ℝ × ℝ ↦ widthLogLik z v.2.1 v.2.2 v.1
    ¬ ∃ v ∈ D, IsMaxOn L D v
```

### `UnivariateML.Endpoint2NonstandardBeta.free_endpoints_unbounded`

Any subunit shape produces a free-endpoint spike. The other endpoint can be taken strictly outside the sample, even for constant data. [Endpoint2NonstandardBeta.lean](../../lean/UnivariateML/Family/Endpoint2NonstandardBeta.lean#L319)

```lean
theorem free_endpoints_unbounded (hn : 0 < n) (x : Fin n → ℝ) {a b : ℝ}
    (h : a < 1 ∨ b < 1) :
    let D : Set (ℝ × ℝ) := {p | p.1 < p.2 ∧ ∀ i, x i ∈ Ioo p.1 p.2}
    let L : ℝ × ℝ → ℝ := fun p ↦ NonstandardBeta.logLik x p.1 p.2 (b, a)
    ¬ BddAbove (L '' D)
```

### `UnivariateML.Endpoint2NonstandardBeta.free_endpoints_constant_collapse`

Both endpoints can shrink symmetrically around a constant sample with any fixed positive beta shapes. [Endpoint2NonstandardBeta.lean](../../lean/UnivariateML/Family/Endpoint2NonstandardBeta.lean#L371)

```lean
theorem free_endpoints_constant_collapse (hn : 0 < n) (a b v : ℝ) :
    Tendsto (fun w ↦ NonstandardBeta.logLik (fun _ : Fin n ↦ v)
      (v - w / 2) (v + w / 2) (b, a)) (𝓝[>] 0) atTop
```

### `UnivariateML.Endpoint2NonstandardBeta.held_endpoints_constant_unbounded`

With both endpoints fixed, the beta shape-concentration limit transfers exactly for every constant interior sample. [Endpoint2NonstandardBeta.lean](../../lean/UnivariateML/Family/Endpoint2NonstandardBeta.lean#L390)

```lean
theorem held_endpoints_constant_unbounded (hn : 0 < n) {l r v : ℝ}
    (hl : l < v) (hr : v < r) :
    ¬ BddAbove (NonstandardBeta.logLik (fun _ : Fin n ↦ v) l r '' Beta.domain)
```

### `UnivariateML.Endpoint2NonstandardBetaAffine.fixed_shapes_affine_fit`

For fixed shapes at least one, nonconstant samples have a unique fit in the affine endpoint chart, including closed uniform contact. [Endpoint2NonstandardBetaAffine.lean](../../lean/UnivariateML/Family/Endpoint2NonstandardBetaAffine.lean#L92)

```lean
theorem fixed_shapes_affine_fit {z : Fin n → ℝ} (hz : ∀ i, z i ∈ Icc (0 : ℝ) 1)
    (hzero : ∃ i, z i = 0) (hone : ∃ i, z i = 1) {a b : ℝ} (ha : 1 ≤ a) (hb : 1 ≤ b) :
    ∃! p, p ∈ domain a b ∧ IsMaxOn (logLik z a b) (domain a b) p
```

### `UnivariateML.Endpoint2NonstandardBetaAffine.fixed_shapes_endpoint_fit`

On nonconstant samples, both regular fixed shapes give a unique positive finite endpoint pair in the original coordinates. [Endpoint2NonstandardBetaAffine.lean](../../lean/UnivariateML/Family/Endpoint2NonstandardBetaAffine.lean#L327)

```lean
theorem fixed_shapes_endpoint_fit {x : Fin n → ℝ} {m M a b : ℝ} (hspan : m < M)
    (hx : ∀ i, x i ∈ Icc m M) (hmin : ∃ i, x i = m) (hmax : ∃ i, x i = M)
    (ha : 1 ≤ a) (hb : 1 ≤ b) :
    let D := endpointDomain m M a b
    ∃! v, v ∈ D ∧ IsMaxOn (fun v : ℝ × ℝ ↦ NonstandardBeta.logLik x v.1 v.2 (a,b)) D v
```

### `UnivariateML.Endpoint2NonstandardBetaContacts.incompatible_zero`

Negative distances violate support for every strictly containing width. [Endpoint2NonstandardBetaContacts.lean](../../lean/UnivariateML/Family/Endpoint2NonstandardBetaContacts.lean#L47)

```lean
theorem incompatible_zero (z : Fin n → ℝ) {a b W : ℝ}
    (h : ∃ i, z i < 0) : strictWidthLikelihood z a b W = 0
```

### `UnivariateML.Endpoint2NonstandardBetaContacts.contact_likelihood`

When some data touch the held endpoint, its subunit shape gives an infinite likelihood, its superunit shape gives zero, and unit shape gives a finite positive likelihood. These rules apply on either reflected side. [Endpoint2NonstandardBetaContacts.lean](../../lean/UnivariateML/Family/Endpoint2NonstandardBetaContacts.lean#L56)

```lean
theorem contact_likelihood {z : Fin n → ℝ} {a b W : ℝ}
    (hW : 0 < W) (hz : ∀ i, 0 ≤ z i ∧ z i < W) (hcontact : ∃ i, z i = 0) :
    (b < 1 → strictWidthLikelihood z a b W = ⊤) ∧
      (1 < b → strictWidthLikelihood z a b W = 0) ∧
      (b = 1 → 0 < strictWidthLikelihood z a b W ∧ strictWidthLikelihood z a b W < ⊤)
```

### `UnivariateML.Endpoint2ParetoII.free_threshold_diverges`

Any nonempty sample, centered at a sample minimum, has a fixed positive shape for which the free-threshold Pareto II likelihood diverges. [Endpoint2ParetoII.lean](../../lean/UnivariateML/Family/Endpoint2ParetoII.lean#L108)

```lean
theorem free_threshold_diverges {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hz : ∃ i, d i = 0) :
    ∃ q > 0, Tendsto (fun s ↦ Transfer.ParetoII.logLik d (-s) q s) (𝓝[>] 0) atTop
```

### `UnivariateML.Endpoint2ParetoII.free_threshold_unbounded`

The unrestricted threshold, shape and scale domain has unbounded likelihood and no finite maximizer, with all sample ties included. [Endpoint2ParetoII.lean](../../lean/UnivariateML/Family/Endpoint2ParetoII.lean#L117)

```lean
theorem free_threshold_unbounded {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hz : ∃ i, d i = 0) :
    let D : Set (ℝ × ℝ × ℝ) := Ici 0 ×ˢ Lomax.domain
    let L : ℝ × ℝ × ℝ → ℝ := fun p ↦ Transfer.ParetoII.logLik d (-p.1) p.2.1 p.2.2
    ¬ BddAbove (L '' D) ∧ ¬ ∃ p ∈ D, IsMaxOn L D p
```

### `UnivariateML.Endpoint2ParetoII.held_threshold_criterion`

Exact held-threshold existence criterion, including compatible observations equal to the threshold. [Endpoint2ParetoII.lean](../../lean/UnivariateML/Family/Endpoint2ParetoII.lean#L133)

```lean
theorem held_threshold_criterion (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ}
    (hx : ∀ i, μ ≤ x i) :
    (∃ p ∈ Lomax.domain,
      IsMaxOn (fun q : ℝ × ℝ ↦ Transfer.ParetoII.logLik x μ q.1 q.2) Lomax.domain p) ↔
    (∀ i, μ < x i) ∧
      ∃ u ∈ Ioo 0 (Lomax.uBar (Transfer.ParetoII.excess x μ)),
        Lomax.psi (Transfer.ParetoII.excess x μ) u ≤ 0
```

### `UnivariateML.Endpoint2ParetoII.held_scale_shape_threshold`

With fixed positive scale and shape the closed sample minimum is the unique maximizing threshold. [Endpoint2ParetoII.lean](../../lean/UnivariateML/Family/Endpoint2ParetoII.lean#L164)

```lean
theorem held_scale_shape_threshold (hn : 0 < n) {d : Fin n → ℝ}
    (hd : ∀ i, 0 ≤ d i) {q s : ℝ} (hq : 0 < q) (hs : 0 < s) :
    ∃! t, t ∈ Ici (0 : ℝ) ∧
      IsMaxOn (fun u ↦ Transfer.ParetoII.logLik d (-u) q s) (Ici 0) t
```

### `UnivariateML.Endpoint2ParetoII.held_scale_fit`

At fixed scale, a sample with at least two different observations has a unique finite threshold and shape fit. With all gaps zero the likelihood instead grows without bound at closed contact. [Endpoint2ParetoII.lean](../../lean/UnivariateML/Family/Endpoint2ParetoII.lean#L181)

```lean
theorem held_scale_fit (hn : 0 < n) {d : Fin n → ℝ}
    (hd : ∀ i, 0 ≤ d i) {s : ℝ} (hs : 0 < s) :
    let D : Set (ℝ × ℝ) := Ici 0 ×ˢ Ioi 0
    let L : ℝ × ℝ → ℝ := fun p ↦ Transfer.ParetoII.logLik d (-p.1) p.2 s
    ((∃ i, 0 < d i) → ∃! p, p ∈ D ∧ IsMaxOn L D p) ∧
      ((∀ i, d i = 0) → ¬ BddAbove (L '' D))
```

### `UnivariateML.Endpoint2ParetoII.held_shape_fit`

The fixed-shape threshold/scale fit is unique exactly when the scale-collapse exponent is positive. Its threshold is closed contact. For negative exponent the likelihood diverges, and at zero exponent the finite contact limit strictly exceeds every feasible finite value. [Endpoint2ParetoII.lean](../../lean/UnivariateML/Family/Endpoint2ParetoII.lean#L309)

```lean
theorem held_shape_fit (hn : 0 < n) {d : Fin n → ℝ}
    (hd : ∀ i, 0 ≤ d i) {q : ℝ} (hq : 0 < q) :
    let D : Set (ℝ × ℝ) := Ici 0 ×ˢ Ioi 0
    let L : ℝ × ℝ → ℝ := fun p ↦ Transfer.ParetoII.logLik d (-p.1) q p.2
    (0 < exponent d q → ∃ s > 0, IsMaxOn L D (0, s) ∧
      ∀ p ∈ D, IsMaxOn L D p → p = (0, s)) ∧
      (exponent d q < 0 → ¬ BddAbove (L '' D)) ∧
      (exponent d q = 0 → ∃ C : ℝ,
        Tendsto (fun s ↦ L (0, s)) (𝓝[>] 0) (𝓝 C) ∧ ∀ p ∈ D, L p < C)
```

### `UnivariateML.Endpoint2ParetoII.held_shape_zero_exponent_sup`

At equality in the scale-collapse exponent the contact limit is the exact, strictly unattained supremum. [Endpoint2ParetoII.lean](../../lean/UnivariateML/Family/Endpoint2ParetoII.lean#L459)

```lean
theorem held_shape_zero_exponent_sup (hn : 0 < n) {d : Fin n → ℝ}
    (hd : ∀ i, 0 ≤ d i) {q : ℝ} (hq : 0 < q) (hE : exponent d q = 0) :
    let D : Set (ℝ × ℝ) := Ici 0 ×ˢ Ioi 0
    let L : ℝ × ℝ → ℝ := fun p ↦ Transfer.ParetoII.logLik d (-p.1) q p.2
    ∃ C : ℝ, IsLUB (L '' D) C ∧ ∀ p ∈ D, L p < C
```

### `UnivariateML.Endpoint2ParetoII.held_threshold_three_level_tie`

The exact three-level Lomax crossing transfers to a held Pareto II threshold of zero. Pareto II uses reciprocal scale coordinates. [Endpoint2ParetoII.lean](../../lean/UnivariateML/Family/Endpoint2ParetoII.lean#L477)

```lean
theorem held_threshold_three_level_tie :
    ∃ a ∈ Ioo ((2 : ℝ) ^ 512)⁻¹ ((2 : ℝ) ^ 32)⁻¹,
      let x : Fin 21 → ℝ := fun i ↦ if i.val = 0 then a else if i.val ≤ 10 then 1 else 100
      ∃ p q : ℝ × ℝ, p ≠ q ∧
        {v ∈ Lomax.domain | IsMaxOn (fun b : ℝ × ℝ ↦
          Transfer.ParetoII.logLik x 0 b.1 b.2) Lomax.domain v} = {p, q}
```

### `UnivariateML.Endpoint2ParetoIII.threshold_spike`

With subunit shape and any fixed scale, all observations tied at the sample minimum contribute to the free-threshold spike. [Endpoint2ParetoIII.lean](../../lean/UnivariateML/Family/Endpoint2ParetoIII.lean#L43)

```lean
theorem threshold_spike {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hz : ∃ i, d i = 0) {c s : ℝ} (hc : 0 < c) (hsub : c < 1) (hs : 0 < s) :
    Tendsto (fun t ↦ ParetoIII.logLik d (-t) (c, s)) (𝓝[>] 0) atTop
```

### `UnivariateML.Endpoint2ParetoIII.free_threshold_unbounded`

Freeing the threshold and shape gives an unbounded objective for any positive scale, hence also when scale is free. [Endpoint2ParetoIII.lean](../../lean/UnivariateML/Family/Endpoint2ParetoIII.lean#L52)

```lean
theorem free_threshold_unbounded {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hz : ∃ i, d i = 0) {s : ℝ} (hs : 0 < s) :
    let D : Set (ℝ × ℝ) := Ici 0 ×ˢ Ioi 0
    let L : ℝ × ℝ → ℝ := fun p ↦ ParetoIII.logLik d (-p.1) (p.2, s)
    ¬ BddAbove (L '' D) ∧ ¬ ∃ p ∈ D, IsMaxOn L D p
```

### `UnivariateML.Endpoint2ParetoIII.constant_excess_path`

Increasing shape at the scale of a positive constant excess makes the held-threshold sample log likelihood diverge. [Endpoint2ParetoIII.lean](../../lean/UnivariateML/Family/Endpoint2ParetoIII.lean#L66)

```lean
theorem constant_excess_path (hn : 0 < n) {v μ : ℝ} (hv : μ < v) :
    Tendsto (fun c ↦ ParetoIII.logLik (fun _ : Fin n ↦ v) μ (c, v - μ)) atTop atTop
```

### `UnivariateML.Endpoint2ParetoIV.threshold_spike`

The moving-threshold spike holds with arbitrary fixed positive scale and upper shape whenever the power is below one. [Endpoint2ParetoIV.lean](../../lean/UnivariateML/Family/Endpoint2ParetoIV.lean#L48)

```lean
theorem threshold_spike {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hz : ∃ i, d i = 0) {k c s : ℝ} (hk : 0 < k) (hc : 0 < c) (hsub : c < 1)
    (hs : 0 < s) :
    Tendsto (fun t ↦ ShapeFacesParetoIV.logLik d (-t) k c s) (𝓝[>] 0) atTop
```

### `UnivariateML.Endpoint2ParetoIV.simultaneous_collapse`

With scale and upper shape free, the Pareto IV likelihood diverges along the simultaneous small-scale, small-shape path at any fixed power. [Endpoint2ParetoIV.lean](../../lean/UnivariateML/Family/Endpoint2ParetoIV.lean#L62)

```lean
theorem simultaneous_collapse {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hz : ∃ i, d i = 0) {c : ℝ} (hc : 0 < c) :
    Tendsto (fun T ↦ ShapeFacesParetoIV.logLik d (-exp (-T)) T⁻¹ c (exp (-T)))
      atTop atTop
```

### `UnivariateML.Endpoint2ParetoIV.held_scale_criterion`

With threshold and scale fixed, attainment is exactly the branch with nonconstant positive excesses and an observation below scale. [Endpoint2ParetoIV.lean](../../lean/UnivariateML/Family/Endpoint2ParetoIV.lean#L72)

```lean
theorem held_scale_criterion (hn : 0 < n) {x : Fin n → ℝ} {μ s : ℝ}
    (hx : ∀ i, μ < x i) (hs : 0 < s) :
    let D := BurrHeld.shapeDomain
    let L := fun p : ℝ × ℝ ↦ ShapeFacesParetoIV.logLik x μ p.2 p.1 s
    (∃ p ∈ D, IsMaxOn L D p) ↔
      (∃ i j, x i ≠ x j) ∧ (∃ i, x i - μ < s)
```

### `UnivariateML.Endpoint2ParetoIV.held_scale_escape`

Held threshold and scale give the exact Pareto contact trace when all excesses reach scale and some exceed it. A constant excess at or below scale instead allows an unbounded fit. [Endpoint2ParetoIV.lean](../../lean/UnivariateML/Family/Endpoint2ParetoIV.lean#L112)

```lean
theorem held_scale_escape (hn : 0 < n) {x : Fin n → ℝ} {μ s : ℝ}
    (hx : ∀ i, μ < x i) (hs : 0 < s) :
    let D := BurrHeld.shapeDomain
    let L := fun p : ℝ × ℝ ↦ ShapeFacesParetoIV.logLik x μ p.2 p.1 s / n
    let z := BurrHeld.logRatios (ShapeFacesParetoIV.excess x μ) s
    let B := BurrHeld.paretoValue z - BurrHeld.avg (fun i ↦ log (x i - μ))
    (((∀ i, s ≤ x i - μ) ∧ (∃ i, s < x i - μ)) →
      IsLUB (L '' D) B ∧ ∀ p ∈ D, L p < B) ∧
    ((∃ v, (∀ i, x i = v) ∧ v - μ ≤ s) → ¬ BddAbove (L '' D))
```

### `UnivariateML.Endpoint2ParetoIV.held_first_criterion`

With threshold and first shape fixed, the existence criterion is nonconstant positive excesses. The constant branch has a divergent path. [Endpoint2ParetoIV.lean](../../lean/UnivariateML/Family/Endpoint2ParetoIV.lean#L140)

```lean
theorem held_first_criterion (hn : 0 < n) {x : Fin n → ℝ} {μ k : ℝ}
    (hx : ∀ i, μ < x i) (hk : 0 < k) :
    let L := fun p : ℝ × ℝ ↦ ShapeFacesParetoIV.logLik x μ k p.1 p.2
    (∃ p ∈ BurrHeld.shapeDomain, IsMaxOn L BurrHeld.shapeDomain p) ↔
      ∃ i j, x i ≠ x j
```

### `UnivariateML.Endpoint2ParetoIV.held_first_scale_criterion`

With threshold, scale and first shape fixed, the remaining power has a finite maximum precisely when some positive excess differs from scale. [Endpoint2ParetoIV.lean](../../lean/UnivariateML/Family/Endpoint2ParetoIV.lean#L166)

```lean
theorem held_first_scale_criterion (hn : 0 < n) {x : Fin n → ℝ} {μ k s : ℝ}
    (hx : ∀ i, μ < x i) (hk : 0 < k) (hs : 0 < s) :
    let L := fun c ↦ ShapeFacesParetoIV.logLik x μ k c s
    (∃ c ∈ Ioi (0 : ℝ), IsMaxOn L (Ioi 0) c) ↔ ∃ i, x i - μ ≠ s
```

### `UnivariateML.Endpoint2ParetoIV.burr_maximizers_transfer`

The entire Burr maximizing set on the excesses is the Pareto IV maximizing set at a held threshold. Every finite tie transfers exactly. [Endpoint2ParetoIV.lean](../../lean/UnivariateML/Family/Endpoint2ParetoIV.lean#L184)

```lean
theorem burr_maximizers_transfer (x : Fin n → ℝ) (μ : ℝ)
    (D : Set (ℝ × ℝ × ℝ)) :
    {v ∈ D | IsMaxOn (fun w : ℝ × ℝ × ℝ ↦
        ShapeFacesParetoIV.logLik x μ w.1 w.2.1 w.2.2) D v} =
      {v ∈ D | IsMaxOn (fun w : ℝ × ℝ × ℝ ↦
        Transfer.Burr.logLik (ShapeFacesParetoIV.excess x μ) w.2.1 w.1 w.2.2) D v}
```

### `UnivariateML.Endpoint2ParetoIV.held_threshold_weibull_path`

The held-power shifted Weibull path, expressed on the original observations via their excesses. [Endpoint2ParetoIV.lean](../../lean/UnivariateML/Family/Endpoint2ParetoIV.lean#L194)

```lean
theorem held_threshold_weibull_path {x : Fin n → ℝ} {μ c r : ℝ}
    (hx : ∀ i, μ < x i) (hc : 0 < c) (hr : 0 < r) :
    let e := ShapeFacesParetoIV.excess x μ
    Tendsto (fun k ↦ ShapeFacesParetoIV.logLik x μ k c ((k / r) ^ c⁻¹)) atTop
      (𝓝 (Lomax.expLogLik (Transfer.Burr.powered e c) r + Transfer.Burr.powerJac e c))
```

### `UnivariateML.Endpoint2ParetoIV.held_power_one_three_level_tie`

The exact Pareto II crossing also gives exactly two Pareto IV maximizers when the threshold is zero and the power is one. [Endpoint2ParetoIV.lean](../../lean/UnivariateML/Family/Endpoint2ParetoIV.lean#L204)

```lean
theorem held_power_one_three_level_tie :
    ∃ a ∈ Ioo ((2 : ℝ) ^ 512)⁻¹ ((2 : ℝ) ^ 32)⁻¹,
      let x : Fin 21 → ℝ := fun i ↦ if i.val = 0 then a else if i.val ≤ 10 then 1 else 100
      ∃ p q : ℝ × ℝ, p ≠ q ∧
        {v ∈ Lomax.domain | IsMaxOn (fun b : ℝ × ℝ ↦
          ShapeFacesParetoIV.logLik x 0 b.1 1 b.2) Lomax.domain v} = {p, q}
```

### `UnivariateML.Endpoint2ParetoIV.held_threshold_failure`

With a held threshold, constants diverge. On nonconstant excesses, if the finite maximum fails to exist, the larger fitted Weibull and Pareto values give the strict, unattained supremum. [Endpoint2ParetoIV.lean](../../lean/UnivariateML/Family/Endpoint2ParetoIV.lean#L237)

```lean
theorem held_threshold_failure {x : Fin n → ℝ} {μ : ℝ}
    (hx : ∀ i, μ < x i) (j : Fin n) (hj : ∀ i, x j ≤ x i) :
    let D := BurrFree.physicalDomain
    let f := fun p : ℝ × ℝ × ℝ ↦ ShapeFacesParetoIV.logLik x μ p.1 p.2.1 p.2.2 / n
    let e := ShapeFacesParetoIV.excess x μ
    let y := BurrFree.logExcess e j
    let J := BurrHeld.avg (fun i ↦ log (e i))
    ((∀ i, x i = x j) → ¬ BddAbove (f '' D)) ∧
      ((∃ i k, x i ≠ x k) → ∃ c > 0, ∃ u,
        (∀ v > 0, ∀ w, BurrFree.weibullLik y v w ≤ BurrFree.weibullLik y c u) ∧
        ((¬ ∃ p ∈ D, IsMaxOn f D p) →
          IsLUB (f '' D) (max (BurrFree.paretoValue y) (BurrFree.weibullLik y c u) - J) ∧
          ∀ p ∈ D, f p < max (BurrFree.paretoValue y) (BurrFree.weibullLik y c u) - J))
```

### `UnivariateML.Endpoint2BetaWidth.spike`

A subunit free-endpoint shape makes the finite log likelihood diverge as the endpoint approaches any tied farthest observations. [Endpoint2BetaWidth.lean](../../lean/UnivariateML/Generic/Endpoint2BetaWidth.lean#L44)

```lean
theorem spike {z : Fin n → ℝ} (hz : ∀ i, z i ∈ Icc (0 : ℝ) 1)
    (hcontact : ∃ i, z i = 1) {a : ℝ} (ha : a < 1) (b : ℝ) :
    Tendsto (fun t ↦ logLik z a b (1 - t)) (𝓝[>] 0) atTop
```

### `UnivariateML.Endpoint2BetaWidth.contact_max`

At free-endpoint shape one, the unique finite maximizer is closed contact. [Endpoint2BetaWidth.lean](../../lean/UnivariateML/Generic/Endpoint2BetaWidth.lean#L84)

```lean
theorem contact_max (hn : 0 < n) {z : Fin n → ℝ} {b : ℝ} (hb : 0 < b) :
    IsMaxOn (logLik z 1 b) (Ioc (0 : ℝ) 1) 1 ∧
      ∀ u ∈ Ioc (0 : ℝ) 1, IsMaxOn (logLik z 1 b) (Ioc 0 1) u → u = 1
```

### `UnivariateML.Endpoint2BetaWidth.interior_max`

Above free-endpoint shape one, the maximum is at the unique interior score root. The theorem allows zeros and repeats in the normalized data. [Endpoint2BetaWidth.lean](../../lean/UnivariateML/Generic/Endpoint2BetaWidth.lean#L106)

```lean
theorem interior_max (hn : 0 < n) {z : Fin n → ℝ} (hz : ∀ i, z i ∈ Icc (0 : ℝ) 1)
    (hcontact : ∃ i, z i = 1) {a b : ℝ} (ha : 1 < a) (hb : 0 < b) :
    (∃! u, u ∈ Ioo (0 : ℝ) 1 ∧ score z a b u = 0) ∧
      ∀ u ∈ Ioo (0 : ℝ) 1,
        IsMaxOn (logLik z a b) (Ioo 0 1) u ↔ score z a b u = 0
```
