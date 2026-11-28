# TASK-501. Tukey lambda base claims in Lean

Six of the thirteen atlas claims have joined theorems at their stated scope. They are the criterion and failure claims for each of `lambda+sigma`, `sigma`, and `lambda+mu`. Seven claims have open entries below. The checked results include the exact negative-shape fixed-location collapse coefficient, strict-majority divergence on the fitted branch, divergence at two joint held-location faces, and unrestricted positive-shape divergence. The atlas fragment has 19 declarations and empty coverage cells for independent review. [Atlas fragment](../existence-atlas/lean-map.d/lean-tlambda.csv)

## Claims and proof effort

| Held | Claim | Status | Work and remaining proof |
|---|---|---|---|
| all free | criterion | Open | Proved strict-majority divergence at shape −1 for every majority atom, including a singleton. Derived the exact fixed-location negative-shape trace. The strict sub-half case still needs a joint bound uniform in shape and moving location as scale vanishes. The exact-half case needs the strict small-scale exclusion uniform over the fitted interval, the explicit compact box and its attained maximum, and the comparison with the largest half-atom coefficient. The written proof handles balanced two-level samples through the pair inequality, which also remains to be formalized. |
| all free | failure | Open | Proved the majority path and the unrestricted-shape clause. The unrestricted argument uses an exponential central factor and a uniform lower bound on each other unscaled factor, which suffices to beat the scale normalization. On the fitted branch, the remaining bound must exclude large-scale and infinite-location corners jointly and show, at every half-multiplicity atom, that joint drift of location and shape toward −1 preserves the proposed upper bound. The fixed-shape trace gives the matching coefficient along the centered slice only. |
| lambda | criterion | Open | Derived the negative exponent alternatives and critical comparison on the fixed-location slice. To lift existence to free location, the criterion needs joint positive superlevels for the nonnegative shapes and for the all-positive-exponent negative case. At critical shape and multiplicity it needs a strict exclusion collar at each critical atom, a common compact rectangle and the comparison including equality. The collar must handle both `abs (mu − t) ≤ sigma/a` using the shifted tail denominator and its complement using strict radial loss at a central observation. |
| lambda | failure | Open | Proved negative-exponent divergence and the exact centered trace, including its strict finite-scale upper bound. The moving-location upper envelope is missing. It requires central modal control and uniform noncentral denominator bounds near each candidate atom, together with exclusion of a collapse point outside the sample. Logistic joint collapse needs the sum-of-absolute-residuals bound. Positive held shape needs the joint support bound. Infinite location requires a bound uniform over scales, including when scale also tends to infinity. |
| lambda+mu | criterion | Proved | Joined the positive-support graph theorem to the logistic and negative profiles. The nonpositive profiles use the continuous inverse of the strictly increasing quantile. The logarithmic likelihood tends to minus infinity on the cocompact filter of the positive scale domain in the attaining cases, so the generic escape theorem gives the maximum. Negative exponents below zero and at zero are excluded by the explicit divergence and critical-supremum proofs. |
| lambda+mu | failure | Proved | Proved a joint statement with the large-scale envelope for every fixed shape. For the negative branch, the normalized tail identity gives the coefficient and strict upper bound at criticality. In the logistic branch an off-center residual suppresses collapse exponentially. In the positive branch it provides a positive scale floor by support. A centered sample at nonnegative shape diverges. |
| lambda+mu | uniqueness | Open | Proved uniqueness of the interior probability coordinates for each given nonpositive shape and scale, so the comparison reduces to a scalar profile. Uniqueness of its maximizer still needs the ordered negative-shape log-scale elasticity and its score identity, strict log-likelihood concavity in inverse scale for the sub-uniform nonnegative branch, and the strict improvement to the contact scale for shapes above one, including the cross-term inequality above shape two. The uniform endpoint cases also need to be joined into the theorem. |
| lambda+sigma | criterion | Proved | Proved that the nonpositive quantile covers the real line, so every finite location has a positive witness. Compact positive superlevels give attainment. Reused the positive-shape support criterion for strict feasibility below one and closed feasibility at and above one. |
| lambda+sigma | failure | Proved | Proved the compact superlevel bound at both shape signs and projected it to absolute location. The same bound upgrades a positive witness to a positive maximum, and the joined support criterion gives the exact incompatibility condition. |
| mu | criterion | Open | Proved that any tie at the held location gives unbounded likelihood by choosing arbitrarily negative shape and then arbitrarily small scale. The no-tie direction still needs the full positive-superlevel compactness argument for the joint shape and scale chart, using bounds at finite limiting shape and at negative-infinite shape. |
| mu | failure | Open | Proved divergence at the negative-infinite-shape, zero-scale corner and at every finite negative critical point approached from more negative shapes. The strict negative-exponent theorem also supplies fixed-shape paths. The missing upper exhaustion needs the bound on the modal density as shape diverges at positive scale, the joint `abs residual * density ≤ 1/(2a)` bound at no-tie negative-infinite shape, and a uniform zero-scale tail estimate near finite shapes, including a neighborhood of zero. The remaining classifications must be joined with the fitted upper endpoint and the all-centered branch. |
| sigma | criterion | Proved | Lifted the likelihood graph to a compact box in shape, probabilities and weights. At a positive limiting level the nonuniform probabilities remain interior. At shape one the closed uniform weights dominate the limiting weights, including simultaneous contacts. A logistic witness gives a positive maximum on the lifted compactification and hence on the fitted graph. |
| sigma | failure | Proved | Projected the compact positive superlevels onto absolute location. The joined statement retains shape −1 at all finite locations and describes the closed support graph up to and including shape one. The trace proof handles the upper envelope at simultaneous moving contacts. |

The fixed-shape and fixed-scale criterion names indices representing the sample minimum and maximum. Such indices exist for every finite nonempty sample, including constant data and tied extrema. The positive-shape chart uses the R7a closed-support weight, with zero endpoint weight strictly below shape one and positive endpoint weight at and above one. A strictly exterior point is represented by the absence of a graph point. All attainment statements require a positive likelihood. No mathematical disagreement with the two written source claims was found. The scope differences for the outstanding claims are listed in the table. [Held-mask proof](2026-11-27-tlambda-atlas-math-report.md) · [All-free proof](2026-11-27-crit-open-math-report.md#tlambda-held-all-free)

## Verification

- The workstation aggregate `lake build` passed with **8,634 jobs** after temporarily importing all four new modules. A final incremental aggregate with the shared escape lemma and module documentation also passed with **8,634 jobs**. Both used the one-slot wrapper and `nice -n 10`. Lake emitted linter warnings.
- The atlas checker exited zero on the workstation after the fragment was regenerated. The fragment contains one row per public theorem, with source lines and signatures.
- The temporary imports were removed and `git diff --exit-code -- dev/lean/UnivariateML.lean` exited zero. The new source has no proof placeholders or new axioms.

## Public Lean declarations

### `UnivariateML.TLHeldScale.nonpositive_quantile_surjective`

Every real residual has an interior quantile coordinate at nonpositive shape. [Lean source](../../lean/UnivariateML/Family/TLHeldScale.lean#L77)

```lean
theorem nonpositive_quantile_surjective {l : ℝ} (hl : l ≤ 0) (z : ℝ) : ∃ p ∈ Ioo 0 1, Q l p = z
```

### `UnivariateML.TLHeldScale.weight_continuousOn`

At fixed shape the probability weight is continuous on the closed unit interval. [Lean source](../../lean/UnivariateML/Family/TLHeldScale.lean#L92)

```lean
theorem weight_continuousOn (l : ℝ) : ContinuousOn (weight l) (Icc 0 1)
```

### `UnivariateML.TLHeldScale.compact_location_superlevel`

Every positive superlevel is compact with shape and scale held. [Lean source](../../lean/UnivariateML/Family/TLHeldScale.lean#L255)

```lean
theorem compact_location_superlevel {n : ℕ} (i : Fin n) (x : Fin n → ℝ) {l s c : ℝ}
    (hs : 0 < s) (hc : 0 < c) :
    IsCompact (graph x l s ∩ {q | c ≤ graphLikelihood l s q})
```

### `UnivariateML.TLHeldScale.held_shape_scale_criterion`

At nonpositive shape attainment is automatic, and at positive shape it is equivalent to exact support compatibility. [Lean source](../../lean/UnivariateML/Family/TLHeldScale.lean#L298)

```lean
theorem held_shape_scale_criterion {n : ℕ} (x : Fin n → ℝ) {l s : ℝ}
    (hs : 0 < s) (i j : Fin n) (hi : ∀ k, x i ≤ x k) (hj : ∀ k, x k ≤ x j) :
    (∃ q ∈ graph x l s, 0 < graphLikelihood l s q ∧
      IsMaxOn (graphLikelihood l s) (graph x l s) q) ↔
    if l ≤ 0 then True else if l < 1 then x j-x i < 2*(s/l) else x j-x i ≤ 2*(s/l)
```

### `UnivariateML.TLHeldScale.held_shape_scale_failure`

Infinite location has zero envelope, and absence of a positive-likelihood fit is exactly support incompatibility. [Lean source](../../lean/UnivariateML/Family/TLHeldScale.lean#L313)

```lean
theorem held_shape_scale_failure {n : ℕ} (x : Fin n → ℝ) {l s : ℝ}
    (hs : 0 < s) (i j : Fin n) (hi : ∀ k, x i ≤ x k) (hj : ∀ k, x k ≤ x j) :
    (∀ c > 0, ∃ M : ℝ, ∀ q ∈ graph x l s, M < |q.1| → graphLikelihood l s q < c) ∧
    ((¬ ∃ q ∈ graph x l s, 0 < graphLikelihood l s q ∧
      IsMaxOn (graphLikelihood l s) (graph x l s) q) ↔
      (¬ ∃ q ∈ graph x l s, 0 < graphLikelihood l s q)) ∧
    ((¬ ∃ q ∈ graph x l s, 0 < graphLikelihood l s q) ↔
      0 < l ∧ (if l < 1 then 2*(s/l) ≤ x j-x i else 2*(s/l) < x j-x i))
```

### `UnivariateML.TLHeldScale.fitted_scale_superlevel`

The fitted held-scale graph has compact positive superlevels, including when shape and support contacts move jointly. [Lean source](../../lean/UnivariateML/Family/TLHeldScale.lean#L538)

```lean
theorem fitted_scale_superlevel {n : ℕ} (x : Fin n → ℝ) (i : Fin n) {s c : ℝ}
    (hs : 0 < s) (hc : 0 < c) :
    IsCompact (fittedGraph x s ∩ {q | c ≤ fittedLikelihood s q})
```

### `UnivariateML.TLHeldScale.fitted_scale_criterion`

Every finite nonempty sample has a positive maximum at an eligible finite shape and location. [Lean source](../../lean/UnivariateML/Family/TLHeldScale.lean#L560)

```lean
theorem fitted_scale_criterion {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) {s : ℝ}
    (hs : 0 < s) :
    ∃ q ∈ fittedGraph x s, 0 < fittedLikelihood s q ∧
      IsMaxOn (fittedLikelihood s) (fittedGraph x s) q
```

### `UnivariateML.TLHeldScale.fitted_scale_failure`

Infinite location has zero envelope, and both shape endpoints and closed support contacts belong to the finite graph. [Lean source](../../lean/UnivariateML/Family/TLHeldScale.lean#L584)

```lean
theorem fitted_scale_failure {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) {s : ℝ}
    (hs : 0 < s) :
    (∀ c > 0, ∃ M : ℝ, ∀ q ∈ fittedGraph x s, M < |q.2.1| → fittedLikelihood s q < c) ∧
    (∀ m : ℝ, ∃ p : Fin n → ℝ, (-1,m,p) ∈ fittedGraph x s ∧
      0 < fittedLikelihood s (-1,m,p)) ∧
    (∀ l ∈ Ioc (0 : ℝ) 1, ∀ m : ℝ,
      (∃! p : Fin n → ℝ, (l,m,p) ∈ fittedGraph x s) ↔ ∀ j, |x j-m| ≤ s/l)
```

### `UnivariateML.TLHeldLocation.negative_held_location_criterion`

At a negative held shape, scale attainment is equivalent to a positive collapse exponent. [Lean source](../../lean/UnivariateML/Family/TLHeldLocation.lean#L205)

```lean
theorem negative_held_location_criterion {n : ℕ} (x : Fin n → ℝ) (m : ℝ) (hn : 0 < n)
    {a : ℝ} (ha : 0 < a) :
    (∃ s > 0, ∃ p : Fin n → ℝ, (m,p) ∈ graph x (-a) s ∧ 0 < graphLikelihood (-a) s (m,p) ∧
      ∀ t > 0, ∀ q : Fin n → ℝ, (m,q) ∈ graph x (-a) t →
        graphLikelihood (-a) t (m,q) ≤ graphLikelihood (-a) s (m,p)) ↔
    0 < ((n : ℝ)-multiplicity x m)/a - multiplicity x m
```

### `UnivariateML.TLHeldLocation.held_shape_location_criterion`

The joined scale attainment criterion covers negative, logistic, and positive held shape. [Lean source](../../lean/UnivariateML/Family/TLHeldLocation.lean#L304)

```lean
theorem held_shape_location_criterion {n : ℕ} (x : Fin n → ℝ) (m l : ℝ) (hn : 0 < n) :
    (∃ s > 0, ∃ p : Fin n → ℝ, (m,p) ∈ graph x l s ∧ 0 < graphLikelihood l s (m,p) ∧
      ∀ t > 0, ∀ q : Fin n → ℝ, (m,q) ∈ graph x l t →
        graphLikelihood l t (m,q) ≤ graphLikelihood l s (m,p)) ↔
    if l < 0 then 0 < ((n : ℝ)-multiplicity x m)/(-l) - multiplicity x m else
      ∃ i, x i ≠ m
```

### `UnivariateML.TLHeldLocation.held_shape_location_failure`

Infinite scale has zero envelope and the joined zero-scale classification gives zero, infinity, or the precise unattained critical value. [Lean source](../../lean/UnivariateML/Family/TLHeldLocation.lean#L419)

```lean
theorem held_shape_location_failure {n : ℕ} (x : Fin n → ℝ) (m l : ℝ) (hn : 0 < n) :
    (∀ c > 0, ∃ R : ℝ, ∀ s > R, ∀ p : Fin n → ℝ, (m,p) ∈ graph x l s →
      graphLikelihood l s (m,p) < c) ∧
    (if l < 0 then
      let e : ℝ := ((n : ℝ)-multiplicity x m)/(-l) - multiplicity x m
      (0 < e → Vanishes x m l) ∧ (e < 0 → Diverges x m l) ∧
      (e = 0 →
        let B := ∏ j, if x j = m then weight l (1/2) else
          1/((-l)*|x j-m|)^(((-l)+1)/(-l))
        let D : Set (ℝ × (Fin n → ℝ)) := {q | 0 < q.1 ∧ (m,q.2) ∈ graph x l q.1}
        let L : ℝ × (Fin n → ℝ) → ℝ := fun q ↦ graphLikelihood l q.1 (m,q.2)
        sSup (L '' D) = B ∧ (∀ q ∈ D, L q < B) ∧
          ∃ p : ℝ → Fin n → ℝ, (∀ s > 0, (m,p s) ∈ graph x l s) ∧
            Tendsto (fun s ↦ graphLikelihood l s (m,p s)) (𝓝[>] 0) (𝓝 B))
    else
      ((∃ i, x i ≠ m) → Vanishes x m l) ∧ ((∀ i, x i = m) → Diverges x m l))
```

### `UnivariateML.TLCollapse.negative_tail_normalized`

The negative-shape tail factor converges to its strict finite-scale bound after normalization. [Lean source](../../lean/UnivariateML/Family/TLCollapse.lean#L112)

```lean
theorem negative_tail_normalized {a d : ℝ} (ha : 0 < a) (hd : d ≠ 0)
    (p : ℝ → ℝ) (hp : ∀ s > 0, p s ∈ Ioo 0 1 ∧ Q (-a) (p s) = d/s) :
    (∀ s > 0, weight (-a) (p s) / s^((a+1)/a) < 1/(a*|d|)^((a+1)/a)) ∧
    Tendsto (fun s ↦ weight (-a) (p s) / s^((a+1)/a)) (𝓝[>] 0)
      (𝓝 (1/(a*|d|)^((a+1)/a)))
```

### `UnivariateML.TLCollapse.fixed_location_negative_trace`

The fixed-location normalized likelihood converges to the exact collapse coefficient and is strictly below it when a residual is nonzero. [Lean source](../../lean/UnivariateML/Family/TLCollapse.lean#L185)

```lean
theorem fixed_location_negative_trace {n : ℕ} (x : Fin n → ℝ) (m : ℝ) {a : ℝ}
    (ha : 0 < a) (p : ℝ → Fin n → ℝ)
    (hp : ∀ s > 0, (m,p s) ∈ graph x (-a) s) :
    let e : ℝ := ((n : ℝ)-multiplicity x m)/a - multiplicity x m
    let B := ∏ j, if x j = m then weight (-a) (1/2) else
      1/(a*|x j-m|)^((a+1)/a)
    (∀ s > 0, (∃ j, x j ≠ m) → graphLikelihood (-a) s (m,p s)/s^e < B) ∧
    Tendsto (fun s ↦ graphLikelihood (-a) s (m,p s)/s^e) (𝓝[>] 0) (𝓝 B)
```

### `UnivariateML.TLCollapse.critical_held_location_supremum`

At the critical exponent the collapse coefficient is the supremum and every positive scale is strictly below it. [Lean source](../../lean/UnivariateML/Family/TLCollapse.lean#L276)

```lean
theorem critical_held_location_supremum {n : ℕ} (x : Fin n → ℝ) (m : ℝ) (hn : 0 < n)
    {a : ℝ} (ha : 0 < a)
    (he : ((n : ℝ)-multiplicity x m)/a - multiplicity x m = 0) :
    let B := ∏ j, if x j = m then weight (-a) (1/2) else
      1/(a*|x j-m|)^((a+1)/a)
    let D : Set (ℝ × (Fin n → ℝ)) := {q | 0 < q.1 ∧ (m,q.2) ∈ graph x (-a) q.1}
    let L : ℝ × (Fin n → ℝ) → ℝ := fun q ↦ graphLikelihood (-a) q.1 (m,q.2)
    sSup (L '' D) = B ∧ ∀ q ∈ D, L q < B
```

### `UnivariateML.TLCollapse.negative_exponent_unbounded`

A negative exponent gives arbitrary likelihood at arbitrarily small positive scales. [Lean source](../../lean/UnivariateML/Family/TLCollapse.lean#L330)

```lean
theorem negative_exponent_unbounded {n : ℕ} (x : Fin n → ℝ) (m : ℝ) {a : ℝ}
    (ha : 0 < a) (he : ((n : ℝ) - multiplicity x m)/a - multiplicity x m < 0) :
    ∀ B : ℝ, ∀ δ > (0 : ℝ), ∃ s ∈ Ioo (0 : ℝ) δ, ∃ p : Fin n → ℝ,
      (m,p) ∈ graph x (-a) s ∧ B < graphLikelihood (-a) s (m,p)
```

### `UnivariateML.TLCollapse.majority_unbounded`

A strict majority atom gives unrestricted growth at arbitrarily small scale already at fitted shape minus one. [Lean source](../../lean/UnivariateML/Family/TLCollapse.lean#L423)

```lean
theorem majority_unbounded {n : ℕ} (x : Fin n → ℝ) (m : ℝ)
    (h : (n : ℝ) < 2 * multiplicity x m) :
    ∀ B : ℝ, ∀ δ > (0 : ℝ), ∃ s ∈ Ioo (0 : ℝ) δ, ∃ p : Fin n → ℝ,
      (m,p) ∈ graph x (-1) s ∧ B < graphLikelihood (-1) s (m,p)
```

### `UnivariateML.TLCollapse.held_location_tie_corner`

A tie at held location gives infinite envelope at the joint infinite-negative-shape and zero-scale corner. [Lean source](../../lean/UnivariateML/Family/TLCollapse.lean#L433)

```lean
theorem held_location_tie_corner {n : ℕ} (x : Fin n → ℝ) (m : ℝ)
    (i : Fin n) (hi : x i = m) :
    ∀ A B : ℝ, ∀ δ > (0 : ℝ), ∃ a > A, 0 < a ∧
      ∃ s ∈ Ioo (0 : ℝ) δ, ∃ p : Fin n → ℝ,
        (m,p) ∈ graph x (-a) s ∧ B < graphLikelihood (-a) s (m,p)
```

### `UnivariateML.TLCollapse.held_location_critical_joint_collapse`

Approaching a finite critical shape from more negative shapes gives infinite joint envelope at zero scale. [Lean source](../../lean/UnivariateML/Family/TLCollapse.lean#L455)

```lean
theorem held_location_critical_joint_collapse {n : ℕ} (x : Fin n → ℝ) (m : ℝ) {a₀ : ℝ}
    (h₀ : 0 < a₀) (hk : 0 < multiplicity x m)
    (he : ((n : ℝ)-multiplicity x m)/a₀ - multiplicity x m = 0) :
    ∀ B : ℝ, ∀ δ > (0 : ℝ), ∃ a ∈ Ioo a₀ (a₀+δ),
      ∃ s ∈ Ioo (0 : ℝ) δ, ∃ p : Fin n → ℝ,
        (m,p) ∈ graph x (-a) s ∧ B < graphLikelihood (-a) s (m,p)
```

### `UnivariateML.TLUnrestricted.unrestricted_unbounded`

With unrestricted shape every nonempty sample gives unbounded likelihood through positive shapes. [Lean source](../../lean/UnivariateML/Family/TLUnrestricted.lean#L56)

```lean
theorem unrestricted_unbounded {n : ℕ} (x : Fin n → ℝ) (i : Fin n) :
    ∀ B : ℝ, ∃ l > 1, ∃ s > 0, ∃ p : Fin n → ℝ,
      (x i,p) ∈ heldScaleGraph x l s ∧ B < graphLikelihood l s (x i,p)
```
