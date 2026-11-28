# Generalized beta and nonstandard beta Lean results

TASK-496 · Branch `codex/lean-genbeta`

The [base9 open-target table](2026-11-28-lean-base9-report.md#open-targets) and the ordered task list name **15 atlas claims** across the two families, although the task heading says fourteen. This report accounts for all 15. Five outcome claims have joined theorems. Ten remain open at the full scope stated in the atlas. The [mapping fragment](../existence-atlas/lean-map.d/lean-genbeta.csv) records 20 checked public theorems across 35 rows. Coverage cells are empty for independent review.

## Claims and effort

The effort column records the proof work for each target. Work on the common endpoint chart and the digamma comparison serves several claims.

| Family | Held | Claim | Status | Effort and result |
|---|---|---|---|---|
| nsbeta | `max+shape1` | Uniqueness | Closed | Developed the reflected inward-distance fit and checked it on the entire attained branch. The unit case gives closed width contact. Above one, the generalized-beta chart with power one gives a unique pair. The joined theorem handles held-end contact, incompatible distances, constants and subunit free-end shape through the existing finite-attainment criterion. |
| nsbeta | `min+shape2` | Uniqueness | Closed | Checked the same likelihood transfer and joint uniqueness in forward inward distances. Ties at the sample maximum distance are permitted. The finite-contact test includes samples with zero distances before any uniqueness conclusion. |
| genbeta | `scale+shape1` | Criterion | Closed | Proved a uniform small-power bound, a large-power bound with decay coefficient proportional to the gap between mean and minimum log distance, and uniform upper-shape bounds on each remaining power interval. Compact superlevels and the generic escape theorem give attainment on nonconstants. The existing constant ray gives the reverse implication. |
| genbeta | `scale+shape1` | Failure | Open | Proved compact superlevels on nonconstants and both parameter limits on all divergent constant paths. Checked the exact necessary and sufficient joint likelihood rate on constants using the fixed-increment gamma ratio. Proving weak concentration at the observation still requires controlling both tails of the powered beta variable as its upper shape tends to infinity. |
| genbeta | `scale+shape1` | Uniqueness | Open | Reduced the target to the conditional upper-shape score and examined the Schur-complement bound. The finite-difference digamma comparison established here gives the logarithmic-ratio curvature bound, which is too weak for the required inequality `q I > 1 - exp(-A)`. That stronger inequality and the differentiated conditional power profile remain to be formalized. |
| genbeta | `scale+shape2` | Criterion | Open | Checked the unit ridge against the existing theorem and proved strict improvement over the fixed-endpoint power face at every positive rate when the upper shape exceeds one. The nonunit constant branches still require the strict exponential-mixture and tangent-gamma comparisons. The nonconstant branches require uniform compactness of the fitted rate through both power ends, followed by the fitted-face and stationary-value comparison. |
| genbeta | `scale+shape2` | Failure | Open | The small-first-shape normalizer gain and exponential decay of the sample gap terms give a finite witness that excludes the power face above one. The fitted-rate necessity along the gamma path and the power path below one requires the joint upper bounds on the rate as power tends to either end. The all-branch failure statement depends on those bounds. |
| genbeta | `shape2+shape3` | Uniqueness | Closed | Proved a shared finite-difference digamma inequality by telescoping the Gamma score gap. Cauchy–Schwarz controls the endpoint odds, so the two score equations cannot hold at distinct maximizing pairs. Transferred the result through the endpoint chart and joined it to the closed unit-shape fit. The existing criterion excludes constants and the subunit branch. |
| genbeta | `scale` | Criterion | Open | Reviewed the small-power gamma chart and the large-power elimination of the second shape. The fixed-first-shape bounds proved here do not control a varying first shape. The remaining argument needs locally uniform gamma ratios in the varying increment, a concavity argument on a neighborhood of the fitted rate and shape, and optimization of the moving-power face before the stationary comparison. |
| genbeta | `scale` | Failure | Open | Traced equality in the large-power envelope to both `q r_a → 0` and `log(q r_a)/a → 0`, where `r_a` is the mean negative log gap. The checked free-endpoint trace does not control this held-endpoint moving cutoff. The two equality conditions, fitted gamma parameter necessity and weak collapse with both beta shapes changing remain to be formalized. |
| genbeta | `shape2` | Criterion | Open | Checked the exact unit contact ridge and isolated the nonunit branches using the subunit rate and the compact fixed-shape chart. Above one the proof still needs compact positive rate bounds uniform in the varying first shape and power. Both shifted-gamma and power faces then need optimized values and a joined stationary comparison. |
| genbeta | `shape2` | Failure | Open | Proved the subunit contact rate test jointly in the other parameters. The unit ridge theorem identifies all finite winners. Above one the free-power problem still needs the joint envelope that confines the rate and slack, the shifted-gamma limit, and the power-face equality condition requiring the scaled contact slack to diverge. The constant regular branch with both first shape and power free also needs its concentration argument. |
| genbeta | `shape1` | Failure | Open | Bounded the subunit normalized log-distance density by the reciprocal log distance and proved a necessary and sufficient contact rate without freezing the power or the upper shape. For nonconstants the proof still needs the two-site log-concavity bound uniform over the regular branch to show eventual subunit shape. Constants on the regular branch still require the varying-shape collapse proof and the limiting-law cases. |
| genbeta | `shape3` | Failure | Open | The checked subunit divergence criterion specializes to fixed power while both beta shapes move. The two-site bound is also needed here to exclude the regular branch on nonconstants. The constant regular branch still requires collapse with changing beta shapes and endpoint and a link to the separated shape-rate limits in the source. |
| genbeta | `shape1+shape2` | Failure | Closed | Joined the infinite exact contact set, the strict-support subunit spike at every positive power and its joint divergence criterion, compact regular superlevels for nonconstants, and the regular constant collapse ray and necessity. The collapse statement is pointwise for every interior beta quantile in the common fixed-beta coupling. |

## Source scope

The principal sources were the [genbeta atlas proof](2026-11-27-genbeta-atlas-math-report.md#genbeta-regimes), the [generalized-beta limit classification](2026-11-25-genbeta-limits-math-report.md#rate-conditions-and-laws), the [held-first-shape bounds](2026-11-07-genbeta-cert-data/bounds.md), and the [one-endpoint beta proof](2026-11-25-thresh-math-report.md#beta-with-one-endpoint-held).

The shared endpoint chart uses log distances from the sample maximum and includes the sample log Jacobian when transferred to physical data. The subunit rate test retains the exact beta normalizer. This allows joint movement of the shapes and power, including tied observations, without selecting one of the source normalizer expansions. The strict-support rate and the extended likelihood at exact contact have separate hypotheses in the joined statement. For the fixed-shape constant branch, the proof states collapse as convergence under a common beta coupling. An explicit statement in a topology on probability measures and the complete menu of subunit weak limits remain separate from the checked likelihood classification.

For the regular uniqueness targets, the written sources use trigamma and a differentiated conditional profile. The checked proof compares finite digamma differences and the two necessary score equations. Existence comes from the prior compact-superlevel theorem and contact criterion. A standalone strict concavity theorem for the profiled first shape remains separate. The uniqueness statements allow ties at the free-side contact and use the existing held-end contact convention.

The strict fixed-endpoint power-face witness for held upper shape above one is valid at every positive rate and on constant as well as nonconstant strictly interior samples. The free-endpoint power face is a different comparison because a fitted threshold can approach the data. Its global comparison remains open in Lean. No additional discrepancy in the mathematical claims was established.

## Verification

- The workstation aggregate `lake build --fail-fast` completed successfully with **8,580 jobs**, including all five new modules. It ran in the foreground through `/tmp/umvl-cpu/run4`, with process affinity limited to four cores. The final log contains no warning from the new modules. The cached Mathlib build was reused.
- The focused `lake env lean` checks and the public-declaration axiom audit passed. The 20 mapped theorems depend only on `propext`, `Classical.choice` and `Quot.sound`.
- `python3 dev/tools/existence_atlas.py --check` passed. The temporary imports were removed and `git diff --exit-code -- dev/lean/UnivariateML.lean` passed.

## Public Lean statements

### GBNonstandardBeta


#### `width_shape_strictConcave`

Conditional maximization over the held-end shape is strictly concave. [GBNonstandardBeta.lean](../../../dev/lean/UnivariateML/Family/GBNonstandardBeta.lean#L41)

```lean
theorem width_shape_strictConcave (hn : 0 < n) (z : Fin n → ℝ)
    {a : ℝ} (ha : 0 < a) (W : ℝ) :
    StrictConcaveOn ℝ (Ioi 0) (fun b ↦ widthLogLik z a b W)
```

#### `unit_width_unique`

At unit free-end shape the finite fit is unique, with closed endpoint contact. [GBNonstandardBeta.lean](../../../dev/lean/UnivariateML/Family/GBNonstandardBeta.lean#L86)

```lean
theorem unit_width_unique (hle : ∀ i, z i ≤ Z) (hmax : ∃ i, z i = Z) :
    let D := {v : ℝ × ℝ | 0 < v.1 ∧ v.2 ∈ finiteWidthDomain z 1 v.1}
    let L := fun v : ℝ × ℝ ↦ widthLogLik z 1 v.1 v.2
    let FiniteContact := ∀ b > 0, ∀ W > max Z 0,
      Endpoint2NonstandardBetaContacts.strictWidthLikelihood z 1 b W < ⊤
    (FiniteContact ∧ (∃! v, v ∈ D ∧ IsMaxOn L D v) ∧
      ∀ v ∈ D, IsMaxOn L D v → v.2 = Z) ↔
      (∀ i, 0 < z i) ∧ (∃ i, z i < Z)
```

#### `held_endpoint_fixed_free_shape_unique`

The finite maximizing width and held-end shape are unique on the full attained branch. [GBNonstandardBeta.lean](../../../dev/lean/UnivariateML/Family/GBNonstandardBeta.lean#L165)

```lean
theorem held_endpoint_fixed_free_shape_unique (hle : ∀ i, z i ≤ Z)
    (hmax : ∃ i, z i = Z) (ha : 0 < a) :
    let D := {v : ℝ × ℝ | 0 < v.1 ∧ v.2 ∈ finiteWidthDomain z a v.1}
    let L := fun v : ℝ × ℝ ↦ widthLogLik z a v.1 v.2
    let FiniteContact := ∀ b > 0, ∀ W > max Z 0,
      Endpoint2NonstandardBetaContacts.strictWidthLikelihood z a b W < ⊤
    (FiniteContact ∧ ∃! v, v ∈ D ∧ IsMaxOn L D v) ↔
      (∀ i, 0 < z i) ∧ (∃ i, z i < Z) ∧ 1 ≤ a
```

### GBHeldScaleShape1


#### `held_scale_shape1_compact_superlevels`

Every finite likelihood superlevel on nonconstant strictly interior samples is compact. [GBHeldScaleShape1.lean](../../../dev/lean/UnivariateML/Family/GBHeldScaleShape1.lean#L376)

```lean
theorem held_scale_shape1_compact_superlevels (hn : 0 < n)
    {x : Fin n → ℝ} {b p : ℝ} (hx : ∀ i, x i ∈ Ioo (0 : ℝ) b)
    (hp : 0 < p) (hne : ∃ i j, x i ≠ x j) (L : ℝ) :
    IsCompact {v ∈ Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ) | L ≤ logLik x b p v.1 v.2}
```

#### `held_scale_shape1_criterion`

Finite attainment is equivalent to nonconstant data at fixed endpoint and positive first shape. [GBHeldScaleShape1.lean](../../../dev/lean/UnivariateML/Family/GBHeldScaleShape1.lean#L434)

```lean
theorem held_scale_shape1_criterion (hn : 0 < n) {x : Fin n → ℝ} {b p : ℝ}
    (hx : ∀ i, x i ∈ Ioo (0 : ℝ) b) (hp : 0 < p) :
    let D := Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ)
    let L := fun v : ℝ × ℝ ↦ logLik x b p v.1 v.2
    (∃ v ∈ D, IsMaxOn L D v) ↔ ∃ i j, x i ≠ x j
```

#### `held_scale_shape1_constant_rate`

Constant-data divergence forces both free parameters to infinity and satisfies the joint rate test. [GBHeldScaleShape1.lean](../../../dev/lean/UnivariateML/Family/GBHeldScaleShape1.lean#L531)

```lean
theorem held_scale_shape1_constant_rate {ι : Type*} {l : Filter ι} (hn : 0 < n)
    {x b p : ℝ} (hx : 0 < x) (hb : x < b) (hp : 0 < p)
    {q a : ι → ℝ} (hdom : ∀ᶠ i in l, 0 < q i ∧ 0 < a i) :
    let y := log (b / x)
    let rate := fun i ↦ log (a i) + p * log (q i * exp (-(a i * y))) -
      log (Gamma p) + (q i - 1) * log (1 - exp (-(a i * y)))
    Tendsto (fun i ↦ logLik (fun _ : Fin n ↦ x) b p (q i) (a i)) l atTop ↔
      Tendsto q l atTop ∧ Tendsto a l atTop ∧ Tendsto rate l atTop
```

#### `held_scale_power_face_beaten`

At upper shape above one, feasible points at sufficiently large power strictly beat the fixed-endpoint power face at the same rate. [GBHeldScaleShape1.lean](../../../dev/lean/UnivariateML/Family/GBHeldScaleShape1.lean#L661)

```lean
theorem held_scale_power_face_beaten (hn : 0 < n) {x : Fin n → ℝ}
    {b q r : ℝ} (hx : ∀ i, x i ∈ Ioo (0 : ℝ) b) (hq : 1 < q) (hr : 0 < r) :
    ∀ᶠ a : ℝ in atTop,
      (n : ℝ) * log r - r * (∑ i, log (b / x i)) - ∑ i, log (x i) <
        logLik x b (r / a) q a
```

### GBEndpointTrace


#### `endpoint_trace_error`

The subunit contact trace bounds the joint likelihood with a uniform error. [GBEndpointTrace.lean](../../../dev/lean/UnivariateML/Family/GBEndpointTrace.lean#L81)

```lean
theorem endpoint_trace_error {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    {p q a s : ℝ} (hq : 0 < q) (hq₁ : q ≤ 1) (ha : 0 < a) (hs : 0 < s) :
    0 ≤ endpointTrace d p q a s - endpointObjective d p q a s ∧
    endpointTrace d p q a s - endpointObjective d p q a s ≤
      ∑ i, (if d i = 0 then 0 else log (1 + s / d i))
```

#### `endpoint_trace_divergence`

On strict-support approach to the endpoint in the subunit regime, divergence is equivalent to divergence of the contact trace. [GBEndpointTrace.lean](../../../dev/lean/UnivariateML/Family/GBEndpointTrace.lean#L110)

```lean
theorem endpoint_trace_divergence {ι : Type*} {l : Filter ι}
    {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i) {p q a s : ι → ℝ}
    (hdom : ∀ᶠ j in l, 0 < q j ∧ q j ≤ 1 ∧ 0 < a j ∧ 0 < s j)
    (hs : Tendsto s l (𝓝 0)) :
    Tendsto (fun j ↦ endpointObjective d (p j) (q j) (a j) (s j)) l atTop ↔
      Tendsto (fun j ↦ endpointTrace d (p j) (q j) (a j) (s j)) l atTop
```

#### `endpoint_objective_eq`

The endpoint chart objective is the physical log likelihood plus the sample log Jacobian. [GBEndpointTrace.lean](../../../dev/lean/UnivariateML/Family/GBEndpointTrace.lean#L141)

```lean
theorem endpoint_objective_eq {x : Fin n → ℝ} {M b p q a : ℝ}
    (hx : ∀ i, 0 < x i) (hM : 0 < M) (hb : 0 < b) :
    logLik x b p q a =
      endpointObjective (fun i ↦ log (M / x i)) p q a (log (b / M)) -
        ∑ i, log (x i)
```

#### `fixed_beta_shapes_compact_superlevels`

Nonconstant samples have compact superlevels at fixed beta shapes with upper shape at least one. [GBEndpointTrace.lean](../../../dev/lean/UnivariateML/Family/GBEndpointTrace.lean#L173)

```lean
theorem fixed_beta_shapes_compact_superlevels {d : Fin n → ℝ}
    (hd : ∀ i, 0 ≤ d i) (hzero : ∃ i, d i = 0) (hpos : ∃ i, 0 < d i)
    {p q : ℝ} (hp : 0 < p) (hq : 1 ≤ q) (L : ℝ) :
    let D := Ioi (0 : ℝ) ×ˢ (if q = 1 then Ici 0 else Ioi 0)
    IsCompact {v ∈ D | L ≤ endpointObjective d p q v.1 v.2}
```

#### `unit_endpoint_ridge`

Every maximizing unit-shape point is on the closed-contact rate ridge. [GBEndpointTrace.lean](../../../dev/lean/UnivariateML/Family/GBEndpointTrace.lean#L324)

```lean
theorem unit_endpoint_ridge (hn : 0 < n) {d : Fin n → ℝ}
    (hd : ∀ i, 0 ≤ d i) (hpos : ∃ i, 0 < d i) :
    let D := Ioi (0 : ℝ) ×ˢ (Ioi (0 : ℝ) ×ˢ Ici (0 : ℝ))
    let F := fun v : ℝ × ℝ × ℝ ↦ endpointObjective d v.1 1 v.2.1 v.2.2
    ∀ v ∈ D, IsMaxOn F D v ↔ v.2.2 = 0 ∧ v.1 * v.2.1 = (n : ℝ) / (∑ i, d i)
```

#### `held_power_unit_endpoint_unique`

Holding the power leaves a unique maximizing pair at unit upper shape. [GBEndpointTrace.lean](../../../dev/lean/UnivariateML/Family/GBEndpointTrace.lean#L393)

```lean
theorem held_power_unit_endpoint_unique (hn : 0 < n) {d : Fin n → ℝ}
    (hd : ∀ i, 0 ≤ d i) (hpos : ∃ i, 0 < d i) {a : ℝ} (ha : 0 < a) :
    let D := Ioi (0 : ℝ) ×ˢ Ici (0 : ℝ)
    let F := fun v : ℝ × ℝ ↦ endpointObjective d v.1 1 a v.2
    ∀ v ∈ D, IsMaxOn F D v ↔ v = ((n : ℝ) / (a * ∑ i, d i), 0)
```

#### `fixed_beta_shapes_constant_collapse`

Every divergent constant-data path at fixed regular beta shapes converges pointwise to the same beta-quantile limit. [GBEndpointTrace.lean](../../../dev/lean/UnivariateML/Family/GBEndpointTrace.lean#L433)

```lean
theorem fixed_beta_shapes_constant_collapse {ι : Type*} {l : Filter ι}
    (hn : 0 < n) {p q : ℝ} (hp : 0 < p) (hq : 1 ≤ q) {a s : ι → ℝ}
    (hdom : ∀ᶠ j in l, 0 < a j ∧ s j ∈ (if q = 1 then Ici 0 else Ioi 0))
    (hdiv : Tendsto (fun j ↦ endpointObjective (fun _ : Fin n ↦ 0) p q (a j) (s j))
      l atTop) :
    Tendsto a l atTop ∧ Tendsto s l (𝓝 0) ∧
      ∀ u ∈ Ioo (0 : ℝ) 1, Tendsto (fun j ↦ exp (s j) * u ^ (a j)⁻¹) l (𝓝 1)
```

#### `subunit_endpoint_upper`

The log-distance density at subunit upper shape is uniformly bounded by the reciprocal distance. [GBEndpointTrace.lean](../../../dev/lean/UnivariateML/Family/GBEndpointTrace.lean#L568)

```lean
theorem subunit_endpoint_upper {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    {p q a s : ℝ} (hp : 0 < p) (hq : 0 < q) (hq₁ : q ≤ 1)
    (ha : 0 < a) (hs : 0 < s) :
    endpointObjective d p q a s ≤ -(n : ℝ) * log s
```

#### `subunit_endpoint_divergence_iff`

Subunit divergence is equivalent to endpoint approach and divergence of the joint contact trace. [GBEndpointTrace.lean](../../../dev/lean/UnivariateML/Family/GBEndpointTrace.lean#L601)

```lean
theorem subunit_endpoint_divergence_iff {ι : Type*} {l : Filter ι}
    {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i) {p q a s : ι → ℝ}
    (hdom : ∀ᶠ j in l, 0 < p j ∧ 0 < q j ∧ q j < 1 ∧ 0 < a j ∧ 0 < s j) :
    Tendsto (fun j ↦ endpointObjective d (p j) (q j) (a j) (s j)) l atTop ↔
      Tendsto s l (𝓝 0) ∧
      Tendsto (fun j ↦ endpointTrace d (p j) (q j) (a j) (s j)) l atTop
```

#### `fixed_beta_shapes_failure`

Fixed shapes yield the subunit spike, collapse on regular constant data, and compact superlevels on regular nonconstant data. [GBEndpointTrace.lean](../../../dev/lean/UnivariateML/Family/GBEndpointTrace.lean#L676)

```lean
theorem fixed_beta_shapes_failure (hn : 0 < n) {d : Fin n → ℝ}
    (hd : ∀ i, 0 ≤ d i) (hzero : ∃ i, d i = 0) {p q : ℝ} (hp : 0 < p) (hq : 0 < q) :
    let D := Ioi (0 : ℝ) ×ˢ (if q = 1 then Ici 0 else Ioi 0)
    (∀ a s, endpointExtended d p q a s = ⊤ ↔ 0 < a ∧ s = 0 ∧ q < 1) ∧
    (q < 1 → (∀ a > 0, Tendsto (endpointObjective d p q a) (𝓝[>] 0) atTop) ∧
      ∀ {l : Filter ℕ} {a s : ℕ → ℝ}, (∀ᶠ j in l, (a j, s j) ∈ D) →
        (Tendsto (fun j ↦ endpointObjective d p q (a j) (s j)) l atTop ↔
          Tendsto s l (𝓝 0) ∧
          Tendsto (fun j ↦ endpointTrace d p q (a j) (s j)) l atTop)) ∧
    (1 ≤ q → ((∃ i, 0 < d i) →
      ∀ L : ℝ, IsCompact {v ∈ D | L ≤ endpointObjective d p q v.1 v.2}) ∧
      ((∀ i, d i = 0) →
        Tendsto (fun a : ℝ ↦ endpointObjective d p q a a⁻¹) atTop atTop ∧
        ∀ {l : Filter ℕ} {a s : ℕ → ℝ}, (∀ᶠ j in l, (a j, s j) ∈ D) →
          Tendsto (fun j ↦ endpointObjective d p q (a j) (s j)) l atTop →
          Tendsto a l atTop ∧ Tendsto s l (𝓝 0) ∧
          ∀ u ∈ Ioo (0 : ℝ) 1, Tendsto (fun j ↦ exp (s j) * u ^ (a j)⁻¹) l (𝓝 1)))
```

### GBBetaCurvature


#### `digamma_difference_strict`

The positive-increment digamma difference decreases strictly faster than the matching log ratio. [GBBetaCurvature.lean](../../../dev/lean/UnivariateML/Generic/GBBetaCurvature.lean#L109)

```lean
theorem digamma_difference_strict {a x y : ℝ} (ha : 0 < a) (hx : 0 < x) (hxy : x < y) :
    (log (x + a) - log x) - (log (y + a) - log y) <
      (Gamma.digamma (x + a) - Gamma.digamma x) -
      (Gamma.digamma (y + a) - Gamma.digamma y)
```

#### `endpoint_profile_at_most_one`

The regular endpoint chart has at most one joint maximizer. [GBBetaCurvature.lean](../../../dev/lean/UnivariateML/Generic/GBBetaCurvature.lean#L293)

```lean
theorem endpoint_profile_at_most_one (hn : 0 < n) {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    {q : ℝ} (hq : 1 < q) :
    let D := Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ)
    ∀ v ∈ D, ∀ w ∈ D, IsMaxOn (endpointProfile d q) D v →
      IsMaxOn (endpointProfile d q) D w → v = w
```

### GBHeldPower


#### `held_power_unique`

With fixed upper shape and power, finite attainment implies a unique pair, at closed contact when the upper shape is one. [GBHeldPower.lean](../../../dev/lean/UnivariateML/Family/GBHeldPower.lean#L54)

```lean
theorem held_power_unique (hn : 0 < n) (hx : ∀ i, 0 < x i)
    (hle : ∀ i, x i ≤ M) (hmax : ∃ i, x i = M) (hq : 0 < q) (ha : 0 < a) :
    let D := Ioi (0 : ℝ) ×ˢ (if q = 1 then Ici M else Ioi M)
    let L := fun v : ℝ × ℝ ↦ logLik x v.2 v.1 q a
    ((∃ v ∈ D, IsMaxOn L D v) ↔ (∃! v, v ∈ D ∧ IsMaxOn L D v)) ∧
      (q = 1 → ∀ v ∈ D, IsMaxOn L D v →
        v = ((n : ℝ) / (a * ∑ i, log (M / x i)), M))
```
