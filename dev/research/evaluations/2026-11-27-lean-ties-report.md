# Exact maximum-likelihood ties in Lean

TASK-400 · 2026-11-27 · Base revision `fc24c05b`

The three-value Lomax construction and its inverse Pareto and held-threshold Pareto II transfers have exactly two global maximizers. The all-free and held-scale paralogistic constructions each have one crossing with two global maximizers, including after reciprocation. Lean also proves a unique half-normal transition on the folded-normal path and a global tie with the exponential member on a wider shifted Gompertz bracket. The held-alpha beta-negative-binomial result uses the explicit likelihood of the two counts 0 and 200.

## Source discrepancy

The beta-negative-binomial report and dossier describe 861 segments as closed by the golden-sample certificate. The retained JSON reports 861 interval evaluations and 431 accepted terminal intervals. Evaluations include subdivisions. The source also qualifies the golden-sample likelihood comparisons as conditional on 60-digit arithmetic with a margin. [Written report](2026-11-06-bnbinom-cert-report.md#concrete-failure-of-labeled-uniqueness) · [Retained certificate output](2026-11-06-bnbinom-cert-data/nonuniqueness.json)

## Results and proof effort

| Target | Lean source lines | Proof and scope |
| --- | ---: | --- |
| 1. Lomax and transfers | 1175 | Exact rational log bounds reverse the order of the compact regional heights across the source dyadic interval. Rolle's theorem and Descartes' rule exclude a third maximizer. A comparison of the sensitivity of two fixed rates proves uniqueness of the sample parameter. Both likelihood transfers preserve the complete two-element maximizing set. |
| 2. Paralogistic and inverse paralogistic | 1672 | Eleven shape intervals cover the held-scale complement uniformly across the source outlier range. The signs or strict witness comparison on each interval follow from rational bounds. For the all-free route, hard-profile losses confine shapes to the two bands. Intercept score bounds place winners in the source rectangles, strict concavity gives at most one winner in each rectangle, and a comparison of the adjusted likelihoods proves that the crossing is unique. The reciprocal maps cover arbitrary positive samples on both routes. |
| 3. Shifted Gompertz | 402 | A uniform separator at rate 117/200 places the exponential optimum below a compact competing region. The witness at outlier 8.1 and the envelope at 128 reverse the likelihood comparison. Uniform rate and shape bounds account for all feasible points at the crossing. |
| 4. Folded normal | 775 | A uniform cubic bound for the location score excludes nearby positive stationary fits along the whole sample path. At the zero-observation endpoint the same argument covers every possible positive fit. The exact variance comparison supplies a witness at one fifth. After normalization, strict ordering of the weighted log-cosh term proves uniqueness of the transition. |
| 5. Held-alpha beta-negative-binomial | 664 | The probability product of the witness (1, 100) exceeds 1/70000. Exact factorial comparisons exclude the first five diagonal intervals. The envelope at successive integers then decreases by an algebraic inequality. Separate bounds on both ends of the positive shape quadrant prove attainment for the explicit model, and symmetry gives the swapped pair. |

[Lomax source](2026-11-25-lomax-uniq-math-report.md#exact-tie-construction) · [Paralogistic source](2026-11-25-paralogis-math-report.md) · [Shifted Gompertz source](2026-11-25-sgomp-math-report.md#exact-separated-maxima-and-the-crossing) · [Folded-normal source](2026-11-25-foldnorm-math-report.md#exact-and-near-ties) · [Beta-negative-binomial source](2026-11-06-bnbinom-cert-report.md#concrete-failure-of-labeled-uniqueness)

## Open obligations

- **Shifted Gompertz.** The constructed outlier lies in (8.1, 128). The source places its unique crossing in (8.60, 8.62) and counts one positive-shape winner there. The narrow enclosure still needs a verified positive-shape profile comparison at both endpoints and the five rate-region score and curvature bounds. The source's strict height-order argument also remains to be formalized. The present global tie proves attainment of both kinds of fit at equality. [Source enclosures](2026-11-25-sgomp-math-report.md#exact-separated-maxima-and-the-crossing)
- **Folded normal.** The transition parameter is unique. The number of positive-location winners at equality remains open. The source's real crossing requires existence of one positive winner. [Source claim](2026-11-25-foldnorm-math-report.md#exact-and-near-ties)
- **Held-alpha beta-negative-binomial.** The exact two-count example differs from the historical 500-count golden sample. The golden-sample interval arithmetic remains unformalized. The new proof works on the explicit Gamma probability product. Linking it to `held_alpha_two_of_diagonal_exclusion` requires a public identity for the imported `logMass`, which is marked `@[no_expose]`. The two-count proof therefore supplies a self-contained global tie for the explicit model. [Imported mass definition](../../lean/UnivariateML/Family/Discrete3BNBinom.lean#L797) · [Conditional theorem](../../lean/UnivariateML/Family/HeldSweepBNBinom.lean#L28)
- **Burr and exponentiated Weibull.** These were contingent on the requested first five source comparisons. The remaining shifted Gompertz and golden-sample obligations leave that condition open.

## Verification

All six individual `lake env lean` checks exited zero on the workstation through the single-slot wrapper. The root build with all six temporary imports also exited zero and reported 7,932 jobs. After the lint cleanup, the three touched modules and the integrated build passed again. Mathlib was reused from the existing cache. The final log contains no new-module warnings, and `git diff --exit-code -- dev/lean/UnivariateML.lean` confirmed that the temporary imports were removed.

From `dev/lean`, the integrated command with the six imports present was

```sh
/tmp/umvl-cpu/run nice -n 10 env LEAN_NUM_THREADS=1 /home/jonas/.elan/bin/lake --old build
```

The thirteen declarations are listed in the atlas fragment. Coverage remains blank for independent review. [Atlas fragment](../existence-atlas/lean-map.d/lean-ties.csv)

## Public declarations

### `UnivariateML.TiesLomax.three_level_global_tie`

A unique observation in the dyadic interval gives the three-level Lomax sample exactly two global maximizers above the exponential limit. [Lean declaration](../../lean/UnivariateML/Family/TiesLomax.lean#L1069)

```lean
theorem three_level_global_tie :
    ∃! a, a ∈ Ioo ((2 : ℝ) ^ 512)⁻¹ ((2 : ℝ) ^ 32)⁻¹ ∧
      let x : Fin 21 → ℝ := fun i ↦ if i.val = 0 then a else if i.val ≤ 10 then 1 else 100
      ∃ p ∈ Lomax.domain, ∃ q ∈ Lomax.domain,
        p.2 < 1024 ∧ 1024 < q.2 ∧
        IsMaxOn (fun v : ℝ × ℝ ↦ Lomax.logLik x v.1 v.2) Lomax.domain p ∧
        IsMaxOn (fun v : ℝ × ℝ ↦ Lomax.logLik x v.1 v.2) Lomax.domain q ∧
        (∀ b ∈ Lomax.domain,
          IsMaxOn (fun v : ℝ × ℝ ↦ Lomax.logLik x v.1 v.2) Lomax.domain b → b = p ∨ b = q) ∧
        bestExp x < Lomax.logLik x p.1 p.2
```

### `UnivariateML.TiesLomax.inverse_pareto_global_tie`

The reciprocal Lomax sample has exactly two inverse Pareto maximizers above the inverse-exponential limit. [Lean declaration](../../lean/UnivariateML/Family/TiesLomax.lean#L1090)

```lean
theorem inverse_pareto_global_tie :
    ∃ x : Fin 21 → ℝ, (∀ i, 0 < x i) ∧
      ∃ p q : ℝ × ℝ, p.2 < q.2 ∧
        {v ∈ Lomax.domain | IsMaxOn (fun b : ℝ × ℝ ↦
          Transfer.InvPareto.logLik x b.1 b.2) Lomax.domain v} = {p, q} ∧
        Transfer.InvPareto.bestInvExp x < Transfer.InvPareto.logLik x p.1 p.2
```

### `UnivariateML.TiesLomax.pareto_ii_global_tie`

For every held threshold there is a Pareto II sample with exactly two global maximizers above the shifted exponential limit. [Lean declaration](../../lean/UnivariateML/Family/TiesLomax.lean#L1126)

```lean
theorem pareto_ii_global_tie (μ : ℝ) :
    ∃ x : Fin 21 → ℝ, (∀ i, μ < x i) ∧
      ∃ p q : ℝ × ℝ, p.2 < q.2 ∧
        {v ∈ Lomax.domain | IsMaxOn (fun b : ℝ × ℝ ↦
          Transfer.ParetoII.logLik x μ b.1 b.2) Lomax.domain v} = {p, q} ∧
        Transfer.ParetoII.bestShiftedExp x μ < Transfer.ParetoII.logLik x μ q.1 q.2
```

### `UnivariateML.TiesParalogistic.reciprocal_joint_maximizers`

Reciprocation is a bijection of the complete joint maximizing sets on all positive samples. [Lean declaration](../../lean/UnivariateML/Family/TiesParalogistic.lean#L29)

```lean
theorem reciprocal_joint_maximizers {n : ℕ} {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) :
    BijOn (fun p : ℝ × ℝ ↦ (p.1, p.2⁻¹))
      {p ∈ Weibull.domain | IsMaxOn
        (fun q : ℝ × ℝ ↦ InvParalogistic.logLik x q.1 q.2) Weibull.domain p}
      {p ∈ Weibull.domain | IsMaxOn
        (fun q : ℝ × ℝ ↦ Paralogistic.logLik (Transfer.reciprocal x) q.1 q.2)
        Weibull.domain p}
```

### `UnivariateML.TiesParalogistic.reciprocal_held_scale_maximizers`

Reciprocal data and reciprocal held scale preserve the complete maximizing set of shapes. [Lean declaration](../../lean/UnivariateML/Family/TiesParalogistic.lean#L51)

```lean
theorem reciprocal_held_scale_maximizers {n : ℕ} {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {s : ℝ} (_hs : 0 < s) :
    {a ∈ Ioi 0 | IsMaxOn (fun b ↦ InvParalogistic.logLik x b s) (Ioi 0) a} =
      {a ∈ Ioi 0 | IsMaxOn
        (fun b ↦ Paralogistic.logLik (Transfer.reciprocal x) b s⁻¹) (Ioi 0) a}
```

### `UnivariateML.TiesParalogistic.held_scale_global_tie`

The held-scale two-observation construction has a unique crossing and two global maximizers in the specified shape bands. [Lean declaration](../../lean/UnivariateML/Family/TiesParalogistic.lean#L440)

```lean
theorem held_scale_global_tie :
    ∃! r, r ∈ Ioo (3 / 1000 : ℝ) (5 / 1000) ∧
      ∃ a ∈ Icc (2 : ℝ) 4, ∃ b ∈ Icc (6500 : ℝ) 10000,
        {k ∈ Ioi 0 | IsMaxOn (fun t ↦ Paralogistic.logLik
          (fun i ↦ exp (tieLogs r i)) t 1) (Ioi 0) k} = {a, b}
```

### `UnivariateML.TiesParalogistic.inverse_held_scale_global_tie`

The reciprocal two-observation sample has the same unique crossing and the same two maximizing shapes. [Lean declaration](../../lean/UnivariateML/Family/TiesParalogistic.lean#L464)

```lean
theorem inverse_held_scale_global_tie :
    ∃! r, r ∈ Ioo (3 / 1000 : ℝ) (5 / 1000) ∧
      ∃ a ∈ Icc (2 : ℝ) 4, ∃ b ∈ Icc (6500 : ℝ) 10000,
        {k ∈ Ioi 0 | IsMaxOn (fun t ↦ InvParalogistic.logLik
          (fun i ↦ exp (-(tieLogs r i))) t 1) (Ioi 0) k} = {a, b}
```

### `UnivariateML.TiesParalogisticJoint.joint_global_tie`

The source two-level all-free sample has a unique crossing with exactly two global maximizers. [Lean declaration](../../lean/UnivariateML/Family/TiesParalogisticJoint.lean#L1127)

```lean
theorem joint_global_tie :
    ∃! K, K ∈ Ioo (7 / 5 : ℝ) (8 / 5) ∧
      let x : Fin 10000 → ℝ := fun i ↦ if i = 0 then 1 else exp (100000000 * K)
      ∃ p q : ℝ × ℝ, p.1 < 1 / 10000 ∧ 1 / 10000 < q.1 ∧
        {v ∈ Weibull.domain | IsMaxOn (fun b : ℝ × ℝ ↦
          Paralogistic.logLik x b.1 b.2) Weibull.domain v} = {p, q}
```

### `UnivariateML.TiesParalogisticJoint.inverse_joint_global_tie`

The reciprocal all-free sample has exactly two global maximizers at its unique crossing. [Lean declaration](../../lean/UnivariateML/Family/TiesParalogisticJoint.lean#L1176)

```lean
theorem inverse_joint_global_tie :
    ∃! K, K ∈ Ioo (7 / 5 : ℝ) (8 / 5) ∧
      let x : Fin 10000 → ℝ := fun i ↦ if i = 0 then 1 else exp (-(100000000 * K))
      ∃ p q : ℝ × ℝ, p.1 < 1 / 10000 ∧ 1 / 10000 < q.1 ∧
        {v ∈ Weibull.domain | IsMaxOn (fun b : ℝ × ℝ ↦
          InvParalogistic.logLik x b.1 b.2) Weibull.domain v} = {p, q}
```

### `UnivariateML.TiesShiftedGompertz.exponential_positive_shape_tie`

Nine ones and one outlier yield a global tie between the exponential member and a separated positive-shape fit. [Lean declaration](../../lean/UnivariateML/Family/TiesShiftedGompertz.lean#L324)

```lean
theorem exponential_positive_shape_tie :
    ∃ m ∈ Ioo (81 / 10 : ℝ) 128,
      let x : Fin 10 → ℝ := fun i ↦ if i = 0 then m else 1
      let f := fun p : ℝ × ℝ ↦ logLik x p.1 p.2
      let p₀ := (exponentialRate x, 0)
      p₀ ∈ jointDomain ∧ IsMaxOn f jointDomain p₀ ∧
        ∃ b > (117 / 200 : ℝ), ∃ η > (0 : ℝ),
          (b, η) ∈ jointDomain ∧ IsMaxOn f jointDomain (b, η)
```

### `UnivariateML.TiesFoldedNormal.half_normal_positive_location_tie`

There is a unique transition in the sample path with 99 repeated observations and one 1 where the half-normal ties a positive-location fit. [Lean declaration](../../lean/UnivariateML/Family/TiesFoldedNormal.lean#L756)

```lean
theorem half_normal_positive_location_tie :
    ∃! e, e ∈ Ioo (0 : ℝ) (1 / 5) ∧
      let x : Fin 100 → ℝ := fun i ↦ if i = 0 then 1 else e
      let S : Set (ℝ × ℝ) := Ici 0 ×ˢ Ioi 0
      let f := fun p : ℝ × ℝ ↦ logLik x p.1 p.2
      (0, m₂ x) ∈ S ∧ IsMaxOn f S (0, m₂ x) ∧
        ∃ μ > (0 : ℝ), ∃ t > (0 : ℝ), IsMaxOn f S (μ, t)
```

### `UnivariateML.TiesBNBinom.explicit_diagonal_exclusion`

At held alpha 2 the shapes (1, 100) beat every diagonal point on counts (0, 200). [Lean declaration](../../lean/UnivariateML/Family/TiesBNBinom.lean#L353)

```lean
theorem explicit_diagonal_exclusion :
    let p (r b : ℝ) (k : ℕ) :=
      (Gamma (2 + r) * Gamma (2 + b) * Gamma (r + k) * Gamma (b + k)) /
        (Gamma (2 + r + b + k) * Gamma r * Gamma b * k.factorial)
    ∀ d > 0, p d d 0 * p d d 200 < p 1 100 0 * p 1 100 200
```

### `UnivariateML.TiesBNBinom.explicit_labeled_global_tie`

The explicit likelihood of (0, 200) at alpha 2 has two distinct swapped global maximizers. [Lean declaration](../../lean/UnivariateML/Family/TiesBNBinom.lean#L627)

```lean
theorem explicit_labeled_global_tie :
    let mass (r b : ℝ) (k : ℕ) :=
      (Gamma (2 + r) * Gamma (2 + b) * Gamma (r + k) * Gamma (b + k)) /
        (Gamma (2 + r + b + k) * Gamma r * Gamma b * k.factorial)
    let S : Set (ℝ × ℝ) := Ioi 0 ×ˢ Ioi 0
    let f := fun p : ℝ × ℝ ↦ mass p.1 p.2 0 * mass p.1 p.2 200
    ∃ p ∈ S, p.1 ≠ p.2 ∧ IsMaxOn f S p ∧ p.swap ∈ S ∧ IsMaxOn f S p.swap
```
