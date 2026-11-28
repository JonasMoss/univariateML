# Extreme-value existence and failure in Lean

TASK-495 · 2026-11-28

All seven atlas targets are proved for nonempty samples at their stated scope.
The three modules add 21 public theorems covering the joined criteria, failure
alternatives, and the envelopes and compactness results they use. The
[Lean map fragment](../existence-atlas/lean-map.d/lean-ev.csv) leaves coverage
blank for independent review.

## Results and effort by target

| Family and regime | Claim | Checked result and proof work |
| --- | --- | --- |
| GEV, location held | Criterion | `EVGEV.held_location_criterion`. The shared scale bounds give compact superlevels for a negative residual, or for nonnegative residuals with a strict minority of zeros. The joined theorem uses the existing half-contact and majority results for the remaining cases. This required the left-tail bound and adaptation of the compactness argument to a held location, including a hold strictly below all observations. |
| GEV, all free | Criterion | `EVGEV.all_free_criterion`. The strict minimum-minority branch attains. The half branch attains exactly for two distinct values with equal counts, and the majority branch never attains. The main work was the joint scale bounds, location containment, and upper semicontinuity across the closed reverse-exponential endpoint. |
| GEV, all free | Failure | `EVGEV.all_free_failure`. The unrestricted likelihood is unbounded. The fitted shape-one path diverges at majority contact and reaches the finite least upper bound at half contact. `all_free_half_supremum` gives the exact equality parameters. The main work was the signed-shape pair inequality, its Gumbel seam, and the equality conditions after summing minimum–nonminimum pairs. |
| Fréchet, all free | Criterion | `EVFrechet.all_free_criterion`. The unrestricted likelihood is unbounded. On the fitted branch, a finite MLE exists exactly if the minimum has strict minority multiplicity and a positive reciprocal shape reaches the optimized Gumbel height. The main work was the exact GEV transport, projection of compact nuisance superlevels, and continuity at zero. |
| Fréchet, all free | Failure | `EVFrechet.all_free_failure`. Majority contact diverges. The exact-half collapse supremum is strictly above every finite fit and the optimized Gumbel value. Below half, Gumbel is the unattained supremum exactly when all positive reciprocal shapes lose strictly. The proof combines existing collapse paths, the new GEV pair bound, and the continuous compactification. |
| Exponentiated Weibull, all free | Criterion | `EVExpWeibull.all_free_classification`. Constants are unbounded. For nonconstant data the likelihood is bounded and a finite maximizer exists exactly when some finite point reaches the higher optimized face. Most work went into the compact positive shape strips, two uniform tail bounds, and matching profile limits. |
| Exponentiated Weibull, all free | Failure | `EVExpWeibull.all_free_classification`. If every finite point is strictly below the higher face, that height is the unattained supremum. The theorem identifies each winning face by comparing its height to the other, including equal-height faces, and joins this with constant concentration. This uses the same tail and contact proof as the criterion, plus the compactified comparison and both least-upper-bound directions. |

## Proof structure and source comparison

**GEV.** In log-rate coordinates, write the two rates as
`exp (v + u)` and `exp (v - u)`. The shape-dependent distance factor is
`sinh (ξ * u) / ξ` for nonzero shape and `u` at zero. Convexity bounds
both factors by `sinh u < cosh u`. A log inequality then gives the strict
pair comparison on interior support. At shape −1, equality requires upper
contact and half-gap scale. Summing all minimum–nonminimum pairs with equal
weight gives the sharp half-tie comparison and forces every upper gap to
equal twice the scale.

Pairing an observation with the location gives the quadratic distance
envelope. The left-residual log-rate lower bound gives arbitrary polynomial
decay on the left. For a strict minimum minority, a location above the
minimum by a fixed distance triggers the left bound. At locations below
that cutoff the more-than-half nonminimum observations give the quadratic
bound with a positive net power of scale. This handles repeated nonminimum
values as well. Compactness uses the support-extended likelihood and its
joint upper semicontinuity, transported from the existing one-observation
held-scale theorem. The aggregate at positive scale uses the
upper-semicontinuous compact maximum theorem, which includes the finite
reverse-exponential contact.

The dossier overview says the half-tie supremum is approached as shape
tends to one. The checked path holds shape exactly at one and shrinks scale.
The dossier details give that endpoint path explicitly. Free and held
location use different collapse heights and different balanced two-value
scales. [GEV dossier](../../../univariateML-cpp/include/cppunivariate/distributions/gev/README.md#half-minimum-pair-bound) · [Held-location proof](2026-11-25-gev-math-report.md#location-held)

**Fréchet.** Data are represented by their gaps from an attained minimum.
For positive reciprocal shape, the Fréchet and positive GEV objectives
have the same attainable values under the exact nuisance transformation.
Every superlevel of the extended optimized profile is the projection of a
compact GEV likelihood superlevel restricted to reciprocal shapes in
`[0, 1]`. A feasible fixed nuisance optimizer gives lower
semicontinuity by local continuity of its log likelihood, including at
zero. This yields continuity of the extended profile and permits the
generic compactification theorems to retain a finite witness at face
equality. The source writes the scalar profile per observation. The Lean
profile uses the total log likelihood, so it preserves all comparisons.
[Fréchet source proof](2026-11-25-frechet-math-report.md#collapse-and-the-gumbel-comparison)

**Exponentiated Weibull.** The exponent is optimized exactly as
`n / H`, with `H` the sum of negative log cdf values at the transformed
observations. The nuisance coordinate is the transformed sample minimum.
On a positive compact shape interval, above any finite likelihood level,
that coordinate is bounded away from zero and infinity. For large shape
the transformed maximum is at most linear in shape on any retaining
superlevel. The negative log cdf sum then gives a uniform upper envelope
matching the optimized power value at the closed endpoint. The common
path from above supplies the matching lower limit for every top tie.
For small shape, a retaining fit forces the transformed minimum to
infinity while its product with shape stays bounded. Bounding the negative
log cdf below by the exponential survivor and comparing the transformed
gaps to the inverse-Weibull gaps gives the uniform upper envelope.
The existing optimized inverse-Weibull path gives the matching lower
limit. A continuous change of variable adds the two limiting profile
values to a closed interval for the exact attained-or-escaped comparison.

The written proof obtains exhaustion using normalized quantiles. The
formal proof uses bounds on the exponent-profiled objective that are
uniform in the remaining scale coordinate. It establishes the two
optimized profile limits and the boundedness and comparison claims.
A classification of the weak distributional limits of arbitrary parameter
sequences is outside these atlas targets. The claims use exact
real-arithmetic comparisons. The dossier's numerical face detectors
remain separate evidence. [Exponentiated-Weibull source proof](2026-11-25-expweibull-math-report.md#faces-and-finite-attainment) · [Joint envelope derivation](2026-11-11-ew-theory2-report.md#joint-face-likelihoods-and-attainment)

No target-scale discrepancy requiring a weaker theorem was found. All
seven target statements are settled. Numerical certificates and the
separate uniqueness rows remain outside TASK-495.

## Verification

- Each of the three new files passed `lake env lean` on the workstation.
- Workstation `lake build` passed with temporary aggregate imports.
  Lake reported **8,570 jobs**. The build used the single-slot wrapper and
  single-core affinity. Existing modules emitted lint warnings.
- The temporary imports were removed. `git diff --exit-code dev/lean/UnivariateML.lean` passed.
- Lean's axiom audit covered all 21 new public theorems and found only
  `propext`, `Classical.choice`, and `Quot.sound`.
- `python3 dev/tools/existence_atlas.py --check` passed on the workstation.

## Checked theorem statements

The statements below are copied from the modules, with proof bodies removed.

### GEV

**UnivariateML.EVGEV.strict_pair_rate_bound**

In log-rate coordinates a strictly supported pair is below the sharp reverse-exponential bound. [EVGEV.lean](../../lean/UnivariateML/Family/EVGEV.lean#L65)

```lean
theorem strict_pair_rate_bound {ξ u v d s : ℝ} (hξ : ξ ∈ Icc (-1 : ℝ) 1)
    (hu : 0 ≤ u) (hd : 0 < d) (hs : 0 < s)
    (hgap : d = 2 * s * exp (-ξ * v) * distanceFactor ξ u) :
    -2 * log s + 2 * (1 + ξ) * v - (exp (v + u) + exp (v - u)) <
      2 * (log 2 - 1 - log d)
```

**UnivariateML.EVGEV.reverse_pair_bound**

A reverse-exponential pair ties the bound exactly at upper contact with half-gap scale. [EVGEV.lean](../../lean/UnivariateML/Family/EVGEV.lean#L90)

```lean
theorem reverse_pair_bound {d s gap : ℝ} (hd : 0 < d) (hs : 0 < s) (hg : 0 ≤ gap) :
    -2 * log s - (d + 2 * gap) / s ≤ 2 * (log 2 - 1 - log d) ∧
    (-2 * log s - (d + 2 * gap) / s = 2 * (log 2 - 1 - log d) ↔
      gap = 0 ∧ s = d / 2)
```

**UnivariateML.EVGEV.all_free_half_supremum**

At exact half minimum multiplicity the fitted supremum equals the collapse value. Its finite equality cases are exactly the balanced two-value reverse-exponential fits. [EVGEV.lean](../../lean/UnivariateML/Family/EVGEV.lean#L229)

```lean
theorem all_free_half_supremum {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ} {m : ℝ}
    (hm : ∀ i, m ≤ x i) (hc : n = 2 * Frechet.minimumCount (GEV.lowerGaps x m)) :
    let D : Set (ℝ × (ℝ × ℝ)) := {q | q.1 ∈ Icc (-1 : ℝ) 1 ∧ q.2 ∈ GEV.domain x q.1}
    let L := fun q : ℝ × (ℝ × ℝ) ↦ GEV.logLik x q.1 q.2
    let A := (n : ℝ) * (log 2 - 1) - 2 * ∑ i, log (x i - m)
    IsLUB (L '' D) A ∧
    (∀ q ∈ D, L q = A ↔ q.1 = -1 ∧
      ∀ j, m < x j → q.2.1 + q.2.2 = x j ∧ q.2.2 = (x j - m) / 2) ∧
    ((∃ q ∈ D, IsMaxOn L D q) ↔ ∃ d > 0, ∀ j, m < x j → x j - m = d)
```

**UnivariateML.EVGEV.all_free_failure**

The unrestricted fit is unbounded. The fitted branch diverges at majority contact and has the sharp finite collapse supremum at half contact, with the balanced two-value exception. [EVGEV.lean](../../lean/UnivariateML/Family/EVGEV.lean#L347)

```lean
theorem all_free_failure {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) :
    let D : Set (ℝ × (ℝ × ℝ)) := {q | q.1 ∈ Icc (-1 : ℝ) 1 ∧ q.2 ∈ GEV.domain x q.1}
    let L := fun q : ℝ × (ℝ × ℝ) ↦ GEV.logLik x q.1 q.2
    (¬ BddAbove {v | ∃ ξ : ℝ, ∃ p ∈ GEV.domain x ξ, GEV.logLik x ξ p = v}) ∧
    ∀ m : ℝ, (∀ i, m ≤ x i) → (∃ i, x i = m) →
      let k := Frechet.minimumCount (GEV.lowerGaps x m)
      let A := (n : ℝ) * (log 2 - 1) - 2 * ∑ i, log (x i - m)
      (n < 2 * k →
        Tendsto (fun e ↦ GEV.logLik x 1 (GEV.collapsePath x m 1 e)) (𝓝[>] 0) atTop ∧
        ¬ BddAbove (L '' D) ∧ ¬ ∃ q ∈ D, IsMaxOn L D q) ∧
      (n = 2 * k →
        Tendsto (fun e ↦ GEV.logLik x 1 (GEV.collapsePath x m 1 e)) (𝓝[>] 0) (𝓝 A) ∧
        IsLUB (L '' D) A ∧
        ((∃ q ∈ D, IsMaxOn L D q) ↔ ∃ d > 0, ∀ j, m < x j → x j - m = d))
```

**UnivariateML.EVGEV.fitted_logDensity_envelopes**

Uniform density envelopes give a scale bound, a quadratic distance bound and arbitrary polynomial left-tail decay. [EVGEV.lean](../../lean/UnivariateML/Family/EVGEV.lean#L504)

```lean
theorem fitted_logDensity_envelopes {ξ x : ℝ} {p : ℝ × ℝ}
    (hξ : ξ ∈ Icc (-1 : ℝ) 1) (hs : 0 < p.2)
    (ha : if ξ = -1 then 0 ≤ GEV.supportArg ξ x p else 0 < GEV.supportArg ξ x p) :
    logCell ξ x p ≤ -log p.2 + 2 * log 2 ∧
    (x ≠ p.1 → logCell ξ x p ≤ log p.2 + 2 * log 2 - 1 - 2 * log |x - p.1|) ∧
    (∀ N : ℕ, x < p.1 → logCell ξ x p ≤ -log p.2 + (N + 2 : ℝ) * log (N + 2) -
      (N : ℝ) * log ((p.1 - x) / p.2)) :=
  ⟨logCell_near hξ ha, fun h ↦ logCell_far hξ hs h ha, fun N h ↦ logCell_left N hξ hs h ha⟩

private theorem logLik_large {n : ℕ} (x : Fin n → ℝ) {ξ : ℝ} {p : ℝ × ℝ}
    (hξ : ξ ∈ Icc (-1 : ℝ) 1) (hp : p ∈ GEV.domain x ξ) :
    GEV.logLik x ξ p ≤ (n : ℝ) * (-log p.2 + 2 * log 2)
```

**UnivariateML.EVGEV.fitted_upperSemicontinuous**

The joint fitted likelihood extended by zero is upper semicontinuous at positive scale. [EVGEV.lean](../../lean/UnivariateML/Family/EVGEV.lean#L765)

```lean
theorem fitted_upperSemicontinuous {n : ℕ} (x : Fin n → ℝ) :
    UpperSemicontinuousOn (likelihood x) chart
```

**UnivariateML.EVGEV.all_free_compact**

An attained strict minimum minority gives compact free physical likelihood superlevels. [EVGEV.lean](../../lean/UnivariateML/Family/EVGEV.lean#L855)

```lean
theorem all_free_compact {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ} {m : ℝ}
    (hm : ∀ i, m ≤ x i) (hz : ∃ i, x i = m)
    (hk : 2 * Frechet.minimumCount (GEV.lowerGaps x m) < n) {e : ℝ} (he : 0 < e) :
    IsCompact (chart ∩ likelihood x ⁻¹' Ici e)
```

**UnivariateML.EVGEV.held_location_compact**

With location held, a negative residual or a strict zero minority gives compact positive superlevels. [EVGEV.lean](../../lean/UnivariateML/Family/EVGEV.lean#L864)

```lean
theorem held_location_compact {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ} {m : ℝ}
    (h : (∃ i, x i < m) ∨ 2 * Frechet.minimumCount (GEV.lowerGaps x m) < n)
    {e : ℝ} (he : 0 < e) :
    IsCompact ((chart ∩ {q : Parameters | q.2.1 = m}) ∩ likelihood x ⁻¹' Ici e)
```

**UnivariateML.EVGEV.all_free_criterion**

A fitted maximum exists exactly at strict minority contact or for a balanced two-value sample at half contact. [EVGEV.lean](../../lean/UnivariateML/Family/EVGEV.lean#L898)

```lean
theorem all_free_criterion {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ} {m : ℝ}
    (hm : ∀ i, m ≤ x i) (hz : ∃ i, x i = m) :
    let k := Frechet.minimumCount (GEV.lowerGaps x m)
    (∃ q ∈ domain x, IsMaxOn (fun r ↦ GEV.logLik x r.1 r.2) (domain x) q) ↔
      2 * k < n ∨ (n = 2 * k ∧ ∃ d > 0, ∀ i, m < x i → x i - m = d)
```

**UnivariateML.EVGEV.held_location_criterion**

With location held, a fitted maximum exists with a negative residual or strict zero minority, or at nonnegative half contact with all positive residuals equal. [EVGEV.lean](../../lean/UnivariateML/Family/EVGEV.lean#L923)

```lean
theorem held_location_criterion {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ} (m : ℝ) :
    let k := Frechet.minimumCount (GEV.lowerGaps x m)
    let D : Set (ℝ × ℝ) := {p | p.1 ∈ Icc (-1 : ℝ) 1 ∧ (m, p.2) ∈ GEV.domain x p.1}
    let L := fun p : ℝ × ℝ ↦ GEV.logLik x p.1 (m, p.2)
    (∃ p ∈ D, IsMaxOn L D p) ↔
      (∃ i, x i < m) ∨ 2 * k < n ∨
        ((∀ i, m ≤ x i) ∧ n = 2 * k ∧ ∃ d > 0, ∀ i, m < x i → x i - m = d)
```

### Fréchet

**UnivariateML.EVFrechet.profile_eq_frechet**

At positive reciprocal shape the GEV-seam profile equals the original Fréchet nuisance supremum. [EVFrechet.lean](../../lean/UnivariateML/Family/EVFrechet.lean#L72)

```lean
theorem profile_eq_frechet {n : ℕ} {g : Fin n → ℝ} (hg : ∀ i, 0 ≤ g i)
    (hz : ∃ i, g i = 0) {b : ℝ} (hb : 0 < b) :
    profile g b = sSup {v | ∃ p ∈ Frechet.domain, Frechet.logLik g b⁻¹ p.1 p.2 = v}
```

**UnivariateML.EVFrechet.profile_compact**

A strict minimum minority gives compact superlevels of the extended reciprocal-shape profile. [EVFrechet.lean](../../lean/UnivariateML/Family/EVFrechet.lean#L111)

```lean
theorem profile_compact {n : ℕ} (hn : 0 < n) {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) (hz : ∃ i, g i = 0)
    (hk : 2 * Frechet.minimumCount g < n) (v : ℝ) :
    IsCompact (Icc (0 : ℝ) 1 ∩ profile g ⁻¹' Ici v)
```

**UnivariateML.EVFrechet.profile_continuous**

A strict minimum minority gives continuity of the extended profile including its optimized Gumbel seam. [EVFrechet.lean](../../lean/UnivariateML/Family/EVFrechet.lean#L186)

```lean
theorem profile_continuous {n : ℕ} (hn : 0 < n) {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) (hz : ∃ i, g i = 0)
    (hk : 2 * Frechet.minimumCount g < n) :
    ContinuousOn (profile g) (Icc (0 : ℝ) 1)
```

**UnivariateML.EVFrechet.all_free_criterion**

The unrestricted likelihood is unbounded. On the fitted branch a maximum exists exactly at strict minimum minority with a positive reciprocal shape reaching optimized Gumbel. [EVFrechet.lean](../../lean/UnivariateML/Family/EVFrechet.lean#L278)

```lean
theorem all_free_criterion {n : ℕ} (hn : 0 < n) {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) (hz : ∃ i, g i = 0) :
    (¬ BddAbove {v | ∃ a > 0, ∃ p ∈ Frechet.domain, Frechet.logLik g a p.1 p.2 = v}) ∧
    ((∃ v, IsGreatest (fittedValues g) v) ↔ 2 * Frechet.minimumCount g < n ∧
      ∃ b ∈ Ioc (0 : ℝ) 1, profile g 0 ≤ profile g b)
```

**UnivariateML.EVFrechet.all_free_failure**

Majority ties diverge. Half ties give a strict finite collapse bound above Gumbel. With a strict minority, Gumbel is the unattained supremum exactly when all positive reciprocal shapes lose. [EVFrechet.lean](../../lean/UnivariateML/Family/EVFrechet.lean#L299)

```lean
theorem all_free_failure {n : ℕ} (hn : 0 < n) {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) (hz : ∃ i, g i = 0) :
    let k := Frechet.minimumCount g
    let A := (n : ℝ) * (log 2 - 1) - 2 * ∑ i, log (g i)
    (n < 2 * k → ¬ BddAbove (fittedValues g)) ∧
    (n = 2 * k → IsLUB (fittedValues g) A ∧
      (∀ v ∈ fittedValues g, v < A) ∧
      Tendsto (Frechet.profile g 1) (𝓝[>] 0) (𝓝 A) ∧ profile g 0 < A) ∧
    (2 * k < n → ((∀ b ∈ Ioc (0 : ℝ) 1, profile g b < profile g 0) ↔
      IsLUB (fittedValues g) (profile g 0) ∧ ¬ ∃ v, IsGreatest (fittedValues g) v))
```

### Exponentiated Weibull

**UnivariateML.EVExpWeibull.exponent_profile**

At fixed shape and scale, the conditional exponent is optimized by n divided by the transformed loss sum. [EVExpWeibull.lean](../../lean/UnivariateML/Family/EVExpWeibull.lean#L169)

```lean
theorem exponent_profile {x : Fin n → ℝ} (hn : 0 < n) (hx : ∀ i, 0 < x i)
    {b a m : ℝ} (ha : 0 < a) :
    let d : Fin n → ℝ := fun i ↦ log (x i) - m
    let u := exp (b * (m - log a))
    let c₀ := (n : ℝ) / H d b u
    0 < c₀ ∧ ShapeFacesExpWeibull.logLik x b a c₀ =
      jointProfile d b u - ∑ i, log (x i) ∧
      ∀ c > 0, ShapeFacesExpWeibull.logLik x b a c ≤
        ShapeFacesExpWeibull.logLik x b a c₀
```

**UnivariateML.EVExpWeibull.strip_compact**

On any compact positive shape interval the joint exponent-profiled superlevels are compact. [EVExpWeibull.lean](../../lean/UnivariateML/Family/EVExpWeibull.lean#L252)

```lean
theorem strip_compact {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i) (hne : ∃ i, 0 < d i)
    {j : Fin n} (hj : d j = 0) {a B v : ℝ} (ha : 0 < a) :
    IsCompact ((Icc a B ×ˢ Ioi (0 : ℝ)) ∩
      (fun p : ℝ × ℝ ↦ jointProfile d p.1 p.2) ⁻¹' Ici v)
```

**UnivariateML.EVExpWeibull.large_shape_upper**

At large shape the optimized closed power value bounds the joint profile up to arbitrary tolerance, uniformly over scale. [EVExpWeibull.lean](../../lean/UnivariateML/Family/EVExpWeibull.lean#L366)

```lean
theorem large_shape_upper {d : Fin n → ℝ} {D : ℝ} (hd : ∀ i, d i ≤ D)
    (hD : 0 < D) {j l : Fin n} (hj : d j = 0) (hl : d l = D) {v : ℝ}
    (hv : powerValue d D < v) :
    ∀ᶠ b : ℝ in atTop, ∀ u > 0, jointProfile d b u < v
```

**UnivariateML.EVExpWeibull.small_shape_upper**

At small shape the optimized inverse-Weibull value bounds the joint profile up to arbitrary tolerance, uniformly over scale. [EVExpWeibull.lean](../../lean/UnivariateML/Family/EVExpWeibull.lean#L486)

```lean
theorem small_shape_upper {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i) (hne : ∃ i, 0 < d i)
    {j : Fin n} (hj : d j = 0) {I v : ℝ}
    (hI : ∀ k > 0, inverseProfile d k ≤ I) (hv : I < v) :
    ∀ᶠ b : ℝ in 𝓝[>] 0, ∀ u > 0, jointProfile d b u < v
```

**UnivariateML.EVExpWeibull.profile_face_limits**

The optimized shape profile converges to the optimal inverse-Weibull and closed power values at the respective ends, including tied upper contacts. [EVExpWeibull.lean](../../lean/UnivariateML/Family/EVExpWeibull.lean#L711)

```lean
theorem profile_face_limits {x : Fin n → ℝ} (hn : 0 < n) (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) {M : ℝ} (hM : ∀ i, log (x i) ≤ M)
    (htop : ∃ i, log (x i) = M) :
    ∃ p ∈ Weibull.domain,
      IsMaxOn (fun q : ℝ × ℝ ↦ Weibull.Inverse.logLik x q.1 q.2) Weibull.domain p ∧
      Tendsto (shapeProfile x) (𝓝[>] 0) (𝓝 (Weibull.Inverse.logLik x p.1 p.2)) ∧
      Tendsto (shapeProfile x) atTop (𝓝 (physicalPowerValue x M))
```

**UnivariateML.EVExpWeibull.all_free_classification**

Constants are unbounded. On nonconstant data a maximum exists exactly when a finite point reaches the better face. If all finite points lose, the better face is the unattained supremum with both labels valid at equal heights. [EVExpWeibull.lean](../../lean/UnivariateML/Family/EVExpWeibull.lean#L959)

```lean
theorem all_free_classification {x : Fin n → ℝ} (hn : 0 < n) (hx : ∀ i, 0 < x i)
    {M : ℝ} (hM : ∀ i, log (x i) ≤ M) (htop : ∃ i, log (x i) = M) :
    let N := ∃ i j, x i ≠ x j
    (¬ N → ¬ BddAbove (finiteValues x) ∧ ¬ ∃ v, IsGreatest (finiteValues x) v) ∧
    (N → ∃ p ∈ Weibull.domain,
      IsMaxOn (fun q : ℝ × ℝ ↦ Weibull.Inverse.logLik x q.1 q.2) Weibull.domain p ∧
      let I := Weibull.Inverse.logLik x p.1 p.2
      let P := physicalPowerValue x M
      BddAbove (finiteValues x) ∧
      ((∃ v, IsGreatest (finiteValues x) v) ↔ ∃ v ∈ finiteValues x, max I P ≤ v) ∧
      ((∀ v ∈ finiteValues x, v < max I P) ↔
        IsLUB (finiteValues x) (max I P) ∧ ¬ ∃ v, IsGreatest (finiteValues x) v) ∧
      ((∀ v ∈ finiteValues x, v < max I P) →
        (IsLUB (finiteValues x) I ↔ P ≤ I) ∧ (IsLUB (finiteValues x) P ↔ I ≤ P)))
```
