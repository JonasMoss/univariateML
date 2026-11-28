# Lean zero modifiers and finite count windows

28 November 2026 · TASK-481 · branch `codex/lean-trunc3`

The job adds 61 checked theorems across five Lean modules and 146 mappings to the reviewed modifier claims. The atlas fragment leaves every coverage cell empty for independent grading. All the results below are exact. [Atlas fragment](../existence-atlas/modifier-lean-map.d/lean-trunc3.csv) · [Reviewed claims](../existence-atlas/modifier-proofs.csv) · [Review and work plan](2026-11-28-atlas-modlean-report.md#written-claims-within-reach-of-the-next-lean-round)

## Zero modification and inflation

The 13 transfer theorems apply to beta-binomial, beta-negative binomial, CMP, discrete gamma, discrete Weibull and Poisson inverse Gaussian. A caller supplies the admissible parent set, restricted to the compatible holds and sample, and the positive conditional likelihood. The unrestricted unit interval represents free inflation and a singleton represents a held inflation value. For modification the free and held mass branches are explicit. With positives, the maximizer equivalences carry all conditional ties. For all zeros, the held modification theorem uses the empty product and the free theorem fixes the zero mass at one. On a positive-likelihood slice, the free inflation coefficient clips at zero. When a conditional winner is feasible, the converse identifies every inflated winner. Held inflation below one preserves the parent ordering on zero-only data, and the no-zero theorems recover the parent ordering with either a free or a compatible held value. [Transfer module](../../lean/UnivariateML/Family/Trunc3Zero.lean) · [Written zero-modifier proofs](2026-11-27-zero-mod-math-report.md#likelihood-and-comparison-rules)

**Open.** The conditional maximizing hypothesis for a transfer to an actual fit still has to be discharged at each hold. In particular it calls for the beta-binomial design comparison, the beta-negative binomial recurrence and canonical height comparison, the joint CMP point, adjacent and geometric dispatch, the discrete gamma grouped-mass profile, the discrete Weibull conditional profile and Pareto comparison, and the PIG conditional Bessel profile and face comparison. The modification limit theorem transports a given compatible path and its limiting height. To establish the written failure rows, the face paths still have to be constructed and exhaustively compared for the corresponding held parent charts. For inflation, the clipped objective and feasible-winner argument settle the transfer branch. The complementary branch still needs the joined score and the comparison of the critical values with compatible parent escapes, including the held nonzero inflation score. The transferred zero-only ranking also leaves each family's parent zero-only attainment test to its parent chart. These are conditional transfer results for the six families.

## Finite signed zeta

The retained cells are the closed integer window from one through the upper cut. The log likelihood is the scalar tilt on log count at the negative of the signed exponent. The sample theorem derives it from the cell masses. On a nonsingleton window, the empirical log mean is strictly between the extrema exactly when there is a finite fit, and that fit is unique. The uniform log mean admits exponent zero. The singleton is flat. At either endpoint-only sample, the corresponding signed limit gives the point law and likelihood supremum one, which is strictly above every finite-exponent likelihood. [Zeta module](../../lean/UnivariateML/Family/Trunc3Zeta.lean) · [Written zeta proof](2026-11-27-trunc-expfam-math-report.md#zeta-free-truncateknown)

## CMB and CMP windows

**CMB.** The statistic pairs count with log binomial coefficient at fixed integer size. The canonical criterion is the intrinsic interior of the conditional statistic hull, using the cells from one through the fixed size for positive truncation. The adjacent slopes and lower chord establish noncollinearity and identify both coordinates on carriers with three cells. A mixed two-cell sample admits the full empirical log-odds line. At held odds a flat log-binomial statistic leaves dispersion unidentified. The scalar slices use the count or log-binomial statistic, and the odds endpoint limits distinguish retained extremes from genuine parent endpoints. An included deterministic endpoint strictly beats finite canonical coefficients on its compatible endpoint-only sample at a nonsingleton. The exposed-face statements give the law and supremum along a specified outward ray. The design comparison preserves every tie when the slice suprema have been supplied. [CMB module](../../lean/UnivariateML/Family/Trunc3CMB.lean) · [Known cuts](2026-11-27-trunc-discrete-math-report.md#cmb-all-free-truncateknown) · [Positive cuts](2026-11-27-zero-mod-math-report.md#cmb-all-free-truncated)

**Open.** The CMB hull inequalities identify the consecutive upper edges and the lower chord, and the exposure theorem takes the maximizing cell set of a direction as input. The proof does not assemble an exhaustive boundary dispatch over all allowed sizes. In particular, for scalar fixed odds the extreme sets and their laws still need to be assembled into the global fixed-design fit set, with the actual odds weights on a tied central set. The slice comparison needs the resulting exact heights and their attainment bits. The proof also does not give a single closed-chart theorem combining the finite canonical criterion, singleton, short-window and deterministic endpoint branches for every hold.

**CMP.** Held positive dispersion uses the count statistic. On finite windows the full signed log-rate chart and true zero atom give the count interval test. Held zero dispersion restricts the geometric chart to rate below one. The uniform mean is the excluded upper threshold on a finite window. Held positive rate instead uses log factorial with negative natural tilt. The finite-window results give the interior criterion, the included or excluded geometric edge, the minimal-statistic face and the flat zero–one fibre. The zero-rate theorem gives atom compatibility for every finite window. On the positive ray both one-hold charts have an attainment and uniqueness criterion. At held positive rate, strict convexity of the centered normalizer gives a unique minimizer for a positive empirical log-factorial mean. A dominated secant calculation gives the geometric boundary sign when the rate is below one. At a held rate of one or more, the normalizer diverges towards zero dispersion and every fixed positive cell loses its mass. [Finite and held-dispersion CMP](../../lean/UnivariateML/Family/Trunc3CMP.lean) · [Held-rate positive ray](../../lean/UnivariateML/Family/Trunc3CMPRay.lean) · [Known cuts](2026-11-27-trunc-discrete-math-report.md#cmp-lambda-truncateknown) · [Positive cuts](2026-11-27-zero-mod-math-report.md#cmp-all-free-truncated)

**Open.** The finite zero-dispersion criterion is stated for interior sample means with separate uniform-escape and zero-rate compatibility lemmas. A single statement joining those branches with lower-extreme nonattainment when zero is absent remains. The positive-ray held-rate criterion uses the centered normalizer directly. The explicit positive-dispersion moment equation and divergence of the factorial expectation at an excluded geometric rate remain to be stated and proved. The fixed-cell mass limit supplies the latter argument's concentration input.

## Source wording and effort

The fixed-size CMB known-cut criterion mentions comparison across permitted sizes. With size held there is one permitted design, so the canonical and endpoint criteria are within that slice. The separate slice comparison is for free size. The zeta endpoint-escape wording presupposes a compatible sample, so the empirical log mean lies in the closed range of the retained statistic. The two supremum statements use the endpoint-only cases explicitly. The written transfer and distribution formulas used here required no mathematical correction.

| Target | Work |
| --- | --- |
| Six zero modifiers | Reused the Bernoulli factorization, moved between effective zero mass and inflation coefficient, checked saturated ties in both directions, and transferred limits and zero-only rankings. |
| Zeta | Derived the conditional sample log likelihood, signed optimum and uniform case. Proved both limits by centering the exponent at the concentrating cell, then compared the finite likelihood with its limiting supremum. |
| CMB and CMP | Proved CMB rank and short-window fibres with the count and log-binomial coordinates, including supporting lines, the lower chord and the design tie comparison. For CMP, handled finite-window endpoint cases and used summability, domination, compactness and strict convexity on the positive ray. Most of the proof work was on this target, in particular the ray barrier and one-sided geometric score. |

## Verification

The five modules passed `lake env lean` on the workstation, using the shared one-slot lock and `nice -n 10`. A first umbrella build compiled the project dependencies against the existing Mathlib cache. The final `lake build` on the workstation, with temporary imports for all five modules, completed successfully with 8,482 Lake jobs. The build emitted style lints for whitespace, module headers and line length. The temporary imports were removed after the build and `git diff --exit-code -- dev/lean/UnivariateML.lean` passed. The workstation atlas check `python3 dev/tools/existence_atlas.py --check` passed with all coverage fields blank. No simbox runs were needed.

## Lean statements

### `UnivariateML.Trunc3CMB.canonical_fit`

The finite canonical fit criterion on a retained fixed-size carrier.

```lean
theorem canonical_fit {N : ℕ} {A : Finset (Fin (N+1))} (hA : A.Nonempty)
    (m : CMP.Plane) :
    (∃ θ, IsMaxOn (Truncation.logLik A statistic (fun _ ↦ 1) m) univ θ) ↔
      m ∈ intrinsicInterior ℝ (convexHull ℝ (statistic '' (A : Set (Fin (N+1)))))
```

### `UnivariateML.Trunc3CMB.positive_fit`

On a positive window the same canonical criterion uses the hull with zero removed.

```lean
theorem positive_fit {N : ℕ} (hN : 0 < N) (m : CMP.Plane) :
    let A := Finset.univ.filter (fun k : Fin (N+1) ↦ 0 < k.val)
    (∃ θ, IsMaxOn (Truncation.logLik A statistic (fun _ ↦ 1) m) univ θ) ↔
      m ∈ intrinsicInterior ℝ (convexHull ℝ (statistic '' (A : Set (Fin (N+1)))))
```

### `UnivariateML.Trunc3CMB.pair_fibre`

For an empirical statistic on the segment between two retained cells, the whole maximizing fibre is the line of constant log weight ratio.

```lean
theorem pair_fibre {N : ℕ} (l u : Fin (N+1)) {a b : ℝ} (hab : a+b = 1)
    {θ φ : CMP.Plane}
    (hθ : IsMaxOn (Truncation.logLik {l,u} statistic (fun _ ↦ 1)
      (a • statistic l + b • statistic u)) univ θ) :
    IsMaxOn (Truncation.logLik {l,u} statistic (fun _ ↦ 1)
      (a • statistic l + b • statistic u)) univ φ ↔
      ⟪φ, statistic u⟫ - ⟪φ, statistic l⟫ = ⟪θ, statistic u⟫ - ⟪θ, statistic l⟫
```

### `UnivariateML.Trunc3CMB.pair_empirical_odds`

For two different retained cells with positive empirical proportions the optimum consists of every coefficient on the empirical-odds line.

```lean
theorem pair_empirical_odds {N : ℕ} {l u : Fin (N+1)} (hne : l ≠ u)
    {a b : ℝ} (ha : 0 < a) (hb : 0 < b) (hab : a+b = 1) (φ : CMP.Plane) :
    IsMaxOn (Truncation.logLik {l,u} statistic (fun _ ↦ 1)
      (a • statistic l + b • statistic u)) univ φ ↔
      ⟪φ, statistic u⟫ - ⟪φ, statistic l⟫ = log (b/a)
```

### `UnivariateML.Trunc3CMB.singleton`

A singleton carrier has a flat finite canonical likelihood.

```lean
theorem singleton {N : ℕ} (k : Fin (N+1)) (θ : CMP.Plane) :
    Truncation.logLik {k} statistic (fun _ ↦ 1) (statistic k) θ = 0
```

### `UnivariateML.Trunc3CMB.held_dispersion_fit`

Fixed dispersion on a finite retained window. The endpoint atom is included only when it is an actual parent endpoint.

```lean
theorem held_dispersion_fit {N l u : ℕ} (hlu : l < u) (_hu : u ≤ N)
    (ν : ℝ) {m : ℝ} (hm : m ∈ Icc (l : ℝ) (u : ℝ)) :
    let h := fun k : ℕ ↦ exp (ν * log (N.choose k : ℝ))
    let D := TruncCount.finiteCountDomain (decide (l = 0)) (decide (u = N))
    (∃! p, p ∈ D ∧ IsMaxOn (TruncCount.finiteCountLikelihood l u h m) D p) ↔
      m ∈ Ioo (l : ℝ) (u : ℝ) ∨ (m = l ∧ l = 0) ∨ (m = u ∧ u = N)
```

### `UnivariateML.Trunc3CMB.endpoint_admission`

A deterministic parent endpoint contributes a conditional atom exactly when its retained mass is nonzero.

```lean
theorem endpoint_admission {N : ℕ} (A : Finset (Fin (N+1))) (upper : Bool)
    (k : Fin (N+1)) :
    let e := if upper then Fin.last N else (0 : Fin (N+1))
    let w := fun j : Fin (N+1) ↦ if j = e then (1 : ℝ) else 0
    (0 < ∑ j ∈ A, w j ↔ e ∈ A) ∧
      (e ∈ A → w k / (∑ j ∈ A, w j) = if k = e then 1 else 0)
```

### `UnivariateML.Trunc3CMB.endpoint_strictly_wins`

On a nonsingleton retained carrier an endpoint-only sample has strictly smaller likelihood at every finite canonical point than at its compatible deterministic endpoint.

```lean
theorem endpoint_strictly_wins {N n : ℕ} (hn : 0 < n)
    {A : Finset (Fin (N+1))} (upper : Bool)
    (h : ∃ j ∈ A, j ≠ if upper then Fin.last N else (0 : Fin (N+1)))
    (he : (if upper then Fin.last N else (0 : Fin (N+1))) ∈ A) (θ : CMP.Plane) :
    let e := if upper then Fin.last N else (0 : Fin (N+1))
    let w := fun j : Fin (N+1) ↦ exp ⟪θ, statistic j⟫
    (w e / (∑ j ∈ A, w j))^n < 1
```

### `UnivariateML.Trunc3CMB.held_odds_fit`

At fixed interior odds, the scalar statistic is log choose. When it varies, the interior empirical interval is exactly the criterion.

```lean
theorem held_odds_fit {N : ℕ} {A : Finset (Fin (N+1))}
    (η m : ℝ) {l u : Fin (N+1)} (hl : l ∈ A) (hu : u ∈ A)
    (hlt : Discrete3CMB.height N l < Discrete3CMB.height N u)
    (hb : ∀ k ∈ A, Discrete3CMB.height N k ∈
      Icc (Discrete3CMB.height N l) (Discrete3CMB.height N u)) :
    (∃! ν, IsMaxOn (Truncation.logLik A (Discrete3CMB.height N)
      (fun k ↦ exp (η * k.val)) m) univ ν) ↔
      m ∈ Ioo (Discrete3CMB.height N l) (Discrete3CMB.height N u)
```

### `UnivariateML.Trunc3CMB.held_odds_flat`

At fixed odds a constant log-binomial statistic makes dispersion flat, including the equal-height pair at the centre of an odd design.

```lean
theorem held_odds_flat {N : ℕ} {A : Finset (Fin (N+1))}
    (hA : A.Nonempty) (η m ν : ℝ) (hm : ∀ k ∈ A, Discrete3CMB.height N k = m) :
    IsMaxOn (Truncation.logLik A (Discrete3CMB.height N)
      (fun k ↦ exp (η * k.val)) m) univ ν
```

### `UnivariateML.Trunc3CMB.held_dispersion_limits`

At fixed dispersion either odds endpoint concentrates on the corresponding retained extreme, even if the parent endpoint is excluded.

```lean
theorem held_dispersion_limits {N l u j : ℕ} (hlu : l ≤ u) (ν : ℝ)
    (hj : j ∈ Finset.Icc l u) :
    let h := fun k : ℕ ↦ exp (ν * log (N.choose k : ℝ))
    Tendsto (TruncCount.finiteCountMass l u h j) atBot (𝓝 (if j = l then 1 else 0)) ∧
    Tendsto (TruncCount.finiteCountMass l u h j) atTop (𝓝 (if j = u then 1 else 0))
```

### `UnivariateML.Trunc3CMB.adjacent_strict`

The adjacent log-binomial slopes decrease strictly over each three-cell block of a fixed-size carrier.

```lean
theorem adjacent_strict {N k : ℕ} (hk : k+2 ≤ N) :
    log (N.choose (k+2) : ℝ) - log (N.choose (k+1) : ℝ) <
      log (N.choose (k+1) : ℝ) - log (N.choose k : ℝ)
```

### `UnivariateML.Trunc3CMB.adjacent_exposure`

Each consecutive pair is exposed from above by the line through its two log-binomial statistics. This persists after restricting the carrier.

```lean
theorem adjacent_exposure {N c : ℕ} (hc : c < N) (k : Fin (N+1)) :
    let slope := log ((N-c : ℕ) : ℝ) - log ((c : ℝ)+1)
    let gap := Discrete3CMB.height N k - log (N.choose c : ℝ) - slope*((k.val : ℝ)-c)
    gap ≤ 0 ∧ (gap = 0 ↔ k.val = c ∨ k.val = c+1)
```

### `UnivariateML.Trunc3CMB.lower_chord`

Every retained interior cell is strictly above the chord through two retained extremes. The chord therefore exposes only its endpoints.

```lean
theorem lower_chord {N : ℕ} {l c u : Fin (N+1)}
    (hl : l.val < c.val) (hu : c.val < u.val) :
    ((u.val : ℝ)-l.val) * Discrete3CMB.height N c >
      ((u.val : ℝ)-c.val) * Discrete3CMB.height N l +
      ((c.val : ℝ)-l.val) * Discrete3CMB.height N u
```

### `UnivariateML.Trunc3CMB.identified`

On three cells with distinct successive secant slopes, two canonical maximizers must be equal.

```lean
theorem identified {N : ℕ} {A : Finset (Fin (N+1))}
    {l c u : Fin (N+1)} (hl : l ∈ A) (hc : c ∈ A) (hu : u ∈ A)
    (hdet : ((c.val : ℝ)-l.val) * (Discrete3CMB.height N u - Discrete3CMB.height N c) ≠
      ((u.val : ℝ)-c.val) * (Discrete3CMB.height N c - Discrete3CMB.height N l))
    {m θ φ : CMP.Plane}
    (hθ : IsMaxOn (Truncation.logLik A statistic (fun _ ↦ 1) m) univ θ)
    (hφ : IsMaxOn (Truncation.logLik A statistic (fun _ ↦ 1) m) univ φ) : θ = φ
```

### `UnivariateML.Trunc3CMB.three_cell_unique`

Three different retained cells identify both finite canonical coordinates, since their log-binomial statistics are noncollinear.

```lean
theorem three_cell_unique {N : ℕ} {A : Finset (Fin (N+1))}
    {l c u : Fin (N+1)} (hl : l ∈ A) (hc : c ∈ A) (hu : u ∈ A)
    (hlc : l.val < c.val) (hcu : c.val < u.val)
    {m θ φ : CMP.Plane}
    (hθ : IsMaxOn (Truncation.logLik A statistic (fun _ ↦ 1) m) univ θ)
    (hφ : IsMaxOn (Truncation.logLik A statistic (fun _ ↦ 1) m) univ φ) : θ = φ
```

### `UnivariateML.Trunc3CMB.exposed_window_mass`

An exposed set of the conditional count/log-binomial hull carries the limiting conditional tilt on that set.

```lean
theorem exposed_window_mass {N : ℕ} {A : Finset (Fin (N+1))}
    (θ d : CMP.Plane) (c : ℝ) (F : Finset A) (k : A) (hk : k ∈ F)
    (he : ∀ j ∈ F, ⟪d, statistic j.val⟫ = c)
    (hn : ∀ j ∉ F, ⟪d, statistic j.val⟫ < c) :
    Tendsto (fun t ↦ Discrete3CMB.rayMass
      (fun j : A ↦ ⟪θ, statistic j.val⟫) (fun j : A ↦ ⟪d, statistic j.val⟫-c) t k)
      atTop (𝓝 (exp ⟪θ, statistic k.val⟫ / ∑ j ∈ F, exp ⟪θ, statistic j.val⟫))
```

### `UnivariateML.Trunc3CMB.exposed_window_supremum`

For observations retained by an exposed conditional face, its likelihood is the strict limiting height along the exposing ray.

```lean
theorem exposed_window_supremum {N n : ℕ} {A : Finset (Fin (N+1))}
    (hN : 0 < n) (θ d : CMP.Plane) (c : ℝ) (F : Finset A) (x : Fin n → A)
    (hx : ∀ i, x i ∈ F) (he : ∀ j ∈ F, ⟪d, statistic j.val⟫ = c)
    (hn : ∀ j ∉ F, ⟪d, statistic j.val⟫ < c) (hout : ∃ j, j ∉ F) :
    let base := fun j : A ↦ ⟪θ, statistic j.val⟫
    let slope := fun j : A ↦ ⟪d, statistic j.val⟫-c
    let height := ∏ i, exp (base (x i)) / ∑ j ∈ F, exp (base j)
    let L := fun t ↦ ∏ i, Discrete3CMB.rayMass base slope t (x i)
    IsLUB (range L) height ∧ ∀ t, L t < height
```

### `UnivariateML.Trunc3CMB.size_slice_winners`

Comparing each slice's least upper bound retains all tied size and parameter combinations that actually reach the greatest height.

```lean
theorem size_slice_winners {P : Type*} {S : Set ℕ} {D : ℕ → Set P}
    {L : ℕ → P → ℝ} {height : ℕ → ℝ}
    (hsup : ∀ N ∈ S, IsLUB (L N '' D N) (height N))
    {N : ℕ} {p : P} (hN : N ∈ S) (hp : p ∈ D N) :
    IsMaxOn (fun t : ℕ × P ↦ L t.1 t.2) {t | t.1 ∈ S ∧ t.2 ∈ D t.1} (N,p) ↔
      L N p = height N ∧ IsMaxOn height S N
```

### `UnivariateML.Trunc3CMP.zero_rate_compatibility`

The finite window admits the zero-rate parent atom exactly when zero is retained, and it gives positive likelihood exactly on all zeros. This branch is independent of the dispersion.

```lean
theorem zero_rate_compatibility {l u n : ℕ} (x : Fin n → ℕ) :
    let w := fun k : ℕ ↦ if k = 0 then (1 : ℝ) else 0
    (0 < ∑ k ∈ Finset.Icc l u, w k) ∧ (∀ i, 0 < w (x i)) ↔
      l = 0 ∧ ∀ i, x i = 0
```

### `UnivariateML.Trunc3CMP.singleton_fibres`

A retained singleton has flat objectives under either hold throughout the compatible positive-rate chart.

```lean
theorem singleton_fibres (k : ℕ) (η ν t : ℝ) :
    Truncation.logLik {k} (fun j ↦ (j : ℝ)) (dispersionCarrier ν) k t =
        -log (dispersionCarrier ν k) ∧
      Truncation.logLik {k} CMP.logFactorial (rateCarrier η) (CMP.logFactorial k) t =
        -log (rateCarrier η k)
```

### `UnivariateML.Trunc3CMP.held_dispersion_finite`

At positive held dispersion, a finite window has the full signed log-rate chart, with the zero-rate atom included exactly at lower cell zero.

```lean
theorem held_dispersion_finite {l u : ℕ} (hlu : l < u) {ν : ℝ} (_hν : 0 < ν)
    {m : ℝ} (hm : m ∈ Icc (l : ℝ) (u : ℝ)) :
    let D := TruncCount.finiteCountDomain (decide (l = 0)) false
    (∃! p, p ∈ D ∧ IsMaxOn
      (TruncCount.finiteCountLikelihood l u (dispersionCarrier ν) m) D p) ↔
      m ∈ Ioo (l : ℝ) (u : ℝ) ∨ (m = l ∧ l = 0)
```

### `UnivariateML.Trunc3CMP.held_zero_dispersion_finite`

At zero dispersion, the finite geometric chart excludes zero tilt; on interior sample means its unique finite fit requires a mean below the uniform mean.

```lean
theorem held_zero_dispersion_finite {l u : ℕ} (hlu : l < u)
    {m : ℝ} (hm : m ∈ Ioo (l : ℝ) (u : ℝ)) :
    (∃! t, t < 0 ∧ IsMaxOn
      (Truncation.logLik (Finset.Icc l u) (fun k ↦ (k : ℝ)) (fun _ ↦ 1) m)
      (Iio 0) t) ↔ m < ((l : ℝ) + u) / 2
```

### `UnivariateML.Trunc3CMP.held_zero_dispersion_failure`

At and above the uniform mean the finite zero-dispersion objective strictly improves towards the excluded geometric endpoint.

```lean
theorem held_zero_dispersion_failure {l u : ℕ} (hlu : l < u)
    {m : ℝ} (hm : ((l : ℝ) + u) / 2 ≤ m) :
    ∀ t < 0, ¬ IsMaxOn
      (Truncation.logLik (Finset.Icc l u) (fun k ↦ (k : ℝ)) (fun _ ↦ 1) m)
      (Iio 0) t
```

### `UnivariateML.Trunc3CMP.held_dispersion_finite_atoms`

At positive held dispersion, either extreme rate concentrates at the corresponding retained cell. Admission is governed by the parent chart.

```lean
theorem held_dispersion_finite_atoms {l u j : ℕ} (hlu : l ≤ u)
    (hj : j ∈ Finset.Icc l u) (ν : ℝ) :
    Tendsto (TruncCount.finiteCountMass l u (dispersionCarrier ν) j) atBot
      (𝓝 (if j = l then 1 else 0)) ∧
    Tendsto (TruncCount.finiteCountMass l u (dispersionCarrier ν) j) atTop
      (𝓝 (if j = u then 1 else 0))
```

### `UnivariateML.Trunc3CMP.held_zero_dispersion_uniform`

As the geometric log rate approaches its excluded endpoint zero, the finite window tends to the uniform distribution.

```lean
theorem held_zero_dispersion_uniform {l u j : ℕ} (hlu : l ≤ u) :
    Tendsto (TruncCount.finiteCountMass l u (fun _ ↦ 1) j) (𝓝[<] 0)
      (𝓝 (1 / ((Finset.Icc l u).card : ℝ)))
```

### `UnivariateML.Trunc3CMP.held_rate_interior`

At fixed log rate, positive dispersion is negative tilt on the factorial statistic. For an interior empirical statistic the positive dispersion root exists uniquely below the geometric carrier mean.

```lean
theorem held_rate_interior {l u : ℕ} (hlu : l ≤ u) (η : ℝ)
    (hlu' : CMP.logFactorial l < CMP.logFactorial u) {m : ℝ}
    (hm : m ∈ Ioo (CMP.logFactorial l) (CMP.logFactorial u)) :
    (∃! t, t < 0 ∧ IsMaxOn
      (Truncation.logLik (Finset.Icc l u) CMP.logFactorial (rateCarrier η) m) (Iio 0) t)
      ↔ m < Truncation.carrierMean (Finset.Icc l u) CMP.logFactorial (rateCarrier η)
```

### `UnivariateML.Trunc3CMP.held_rate_minimum_failure`

At the minimal empirical factorial statistic, dispersion can always increase the likelihood on any nonflat finite held-rate window.

```lean
theorem held_rate_minimum_failure {l u : ℕ} (hlu : l ≤ u) (η : ℝ)
    (hlu' : CMP.logFactorial l < CMP.logFactorial u) :
    let D := if η < 0 then Iic (0 : ℝ) else Iio 0
    ∀ t ∈ D, ¬ IsMaxOn
      (Truncation.logLik (Finset.Icc l u) CMP.logFactorial (rateCarrier η)
        (CMP.logFactorial l)) D t
```

### `UnivariateML.Trunc3CMP.held_rate_geometric`

At or above the geometric mean statistic the zero-dispersion endpoint wins. Its inclusion in the parent chart requires negative log rate.

```lean
theorem held_rate_geometric {l u : ℕ} (hlu : l ≤ u) (η : ℝ)
    (hlu' : CMP.logFactorial l < CMP.logFactorial u) {m : ℝ}
    (hm : Truncation.carrierMean (Finset.Icc l u) CMP.logFactorial (rateCarrier η) ≤ m) :
    let D := if η < 0 then Iic (0 : ℝ) else Iio 0
    ∀ t ∈ D, IsMaxOn
      (Truncation.logLik (Finset.Icc l u) CMP.logFactorial (rateCarrier η) m) D t
      ↔ t = 0 ∧ η < 0
```

### `UnivariateML.Trunc3CMP.held_rate_geometric_limit`

In the finite held-rate chart the zero-dispersion limit preserves the rate carrier. The same limit applies at excluded rates.

```lean
theorem held_rate_geometric_limit {l u j : ℕ} (hlu : l ≤ u) (η : ℝ) :
    Tendsto (fun t : ℝ ↦
      rateCarrier η j * exp (t * CMP.logFactorial j) /
      ∑ k ∈ Finset.Icc l u, rateCarrier η k * exp (t * CMP.logFactorial k))
      (𝓝[<] 0) (𝓝 (rateCarrier η j / ∑ k ∈ Finset.Icc l u, rateCarrier η k))
```

### `UnivariateML.Trunc3CMP.held_rate_minimum_limit`

Infinite dispersion at fixed rate selects the minimal factorial statistic set and conditions the rate carrier on it.

```lean
theorem held_rate_minimum_limit {l u : ℕ} (η : ℝ)
    (k : Finset.Icc l u)
    (hk : CMP.logFactorial k.val = CMP.logFactorial l) :
    let A := Finset.Icc l u
    let F : Finset A := Finset.univ.filter (fun j ↦ CMP.logFactorial j.val = CMP.logFactorial l)
    Tendsto (fun t ↦ Discrete3CMB.rayMass (fun j : A ↦ η * j.val)
      (fun j : A ↦ CMP.logFactorial l - CMP.logFactorial j.val) t k)
      atTop (𝓝 (exp (η * k.val) / ∑ j ∈ F, exp (η * j.val)))
```

### `UnivariateML.Trunc3CMP.minimum_factorial_cells`

The minimal factorial face consists of the lower retained cell, and also cell one if the lower cell is zero and one is retained.

```lean
theorem minimum_factorial_cells {l u k : ℕ} (hk : k ∈ Finset.Icc l u) :
    CMP.logFactorial k = CMP.logFactorial l ↔ k = l ∨ (l = 0 ∧ k = 1)
```

### `UnivariateML.Trunc3CMP.held_rate_binary_flat`

On the two-cell window zero and one, the factorial statistic is flat and every admissible dispersion ties at held positive rate.

```lean
theorem held_rate_binary_flat (η t : ℝ) :
    IsMaxOn (Truncation.logLik (Finset.Icc (0 : ℕ) 1) CMP.logFactorial (rateCarrier η) 0)
      (if η < 0 then Iic 0 else Iio 0) t
```

### `UnivariateML.Trunc3CMP.held_dispersion_positive`

On the positive ray, positive held dispersion gives a unique finite log-rate optimum exactly for a positive excess mean over one.

```lean
theorem held_dispersion_positive {ν : ℝ} (hν : 0 < ν) {m : ℝ} (hm : 0 ≤ m) :
    (∃! t, IsMinOn (TruncCount.centeredSum (fun k ↦ dispersionCarrier ν (k+1)) m)
      univ t) ↔ 0 < m
```

### `UnivariateML.Trunc3CMP.held_dispersion_positive_atom`

At positive held dispersion, the low-rate limit on the positive ray is the atom at one (index zero after shifting the carrier).

```lean
theorem held_dispersion_positive_atom {ν : ℝ} (hν : 0 < ν) (j : ℕ) :
    Tendsto (fun t : ℝ ↦ dispersionCarrier ν (j+1) * exp t ^ j /
      ∑' k : ℕ, dispersionCarrier ν (k+1) * exp t ^ k)
      atBot (𝓝 (if j = 0 then 1 else 0))
```

### `UnivariateML.Trunc3CMP.held_rate_positive_atom`

On the positive ray, at any held positive rate the infinite-dispersion limit is the atom at one. A positive-dispersion series supplies domination.

```lean
theorem held_rate_positive_atom (η : ℝ) (j : ℕ) :
    Tendsto (fun ν ↦ CMP.weight η ν (j+1) / ∑' k : ℕ, CMP.weight η ν (k+1))
      atTop (𝓝 (if j = 0 then 1 else 0))
```

### `UnivariateML.Trunc3CMP.held_zero_dispersion_positive`

On the positive ray at held zero dispersion, use the positive geometric fit with the parent probability-one endpoint excluded.

```lean
theorem held_zero_dispersion_positive {m : ℝ} (hm : 1 ≤ m) :
    (∃! p, p ∈ Ioo (0 : ℝ) 1 ∧
      IsMaxOn (Discrete.Geometric.likelihood (m-1)) (Ioo 0 1) p) ↔ 1 < m
```

### `UnivariateML.Trunc3CMPRay.held_rate_excluded_geometric`

At a held rate at least one, each positive conditional cell loses all mass as dispersion tends to zero.

```lean
theorem held_rate_excluded_geometric {η : ℝ} (hη : 0 ≤ η) (j : ℕ) :
    Tendsto (fun t ↦ term η 0 t j / centered η 0 t) (𝓝[<] 0) (𝓝 0)
```

### `UnivariateML.Trunc3CMPRay.held_rate_admitted_geometric`

At a held rate below one the positive conditional masses tend to the normalized shifted geometric carrier as dispersion tends to zero.

```lean
theorem held_rate_admitted_geometric {η : ℝ} (hη : η < 0) (j : ℕ) :
    Tendsto (fun t ↦ term η 0 t j / centered η 0 t) (𝓝[<] 0)
      (𝓝 (term η 0 0 j / centered η 0 0))
```

### `UnivariateML.Trunc3CMPRay.held_rate_positive_fit`

The held-rate positive ray has exactly one optimum for a strictly positive mean log factorial and none for an all-ones sample.

```lean
theorem held_rate_positive_fit (η : ℝ) {m : ℝ} (hm : 0 ≤ m) :
    (∃! t, t ∈ domain η ∧ IsMinOn (centered η m) (domain η) t) ↔ 0 < m
```

### `UnivariateML.Trunc3CMPRay.held_rate_geometric_edge`

At a held rate below one, the admitted geometric boundary wins exactly when its centered factorial moment is nonpositive.

```lean
theorem held_rate_geometric_edge {η m : ℝ} (hη : η < 0) (hm : 0 ≤ m) :
    IsMinOn (centered η m) (domain η) 0 ↔
      (∑' k : ℕ, exp (η*(k+1 : ℕ)) * (CMP.logFactorial (k+1)-m)) ≤ 0
```

### `UnivariateML.Trunc3Zero.modified_held`

With a compatible held zero mass the positive-part maximizers carry over exactly. With all zeros the empty product makes every parent tie.

```lean
theorem modified_held {P : Type*} {D : Set P} {z k : ℕ} {w : ℝ}
    (hw : 0 < bernoulli z k w) (x : Fin k → ℕ) (g : P → ℕ → ℝ) (p : P) :
    IsMaxOn (fun q ↦ modifiedLikelihood z x g (q,w)) D p ↔
      IsMaxOn (fun q ↦ ∏ i, g q (x i)) D p
```

### `UnivariateML.Trunc3Zero.modified_free`

Free modification pairs every positive-part maximizer with the empirical zero fraction.

```lean
theorem modified_free {P : Type*} {D : Set P} {z k : ℕ} (hn : 0 < z+k)
    {L : P → ℝ} (hL : ∀ q ∈ D, 0 ≤ L q) (hpos : ∃ q ∈ D, 0 < L q)
    {p : P} {w : ℝ} (hp : p ∈ D) (hw : w ∈ Icc 0 1) :
    IsMaxOn (fun t : P × ℝ ↦ bernoulli z k t.2 * L t.1) (D ×ˢ Icc 0 1) (p,w)
      ↔ IsMaxOn L D p ∧ w = zeroFraction z k
```

### `UnivariateML.Trunc3Zero.modified_zero`

On all-zero data every compatible parent coefficient fits at free zero mass one.

```lean
theorem modified_zero {P : Type*} {D : Set P} {z : ℕ} (hz : 0 < z)
    {p : P} {w : ℝ} (hp : p ∈ D) (hw : w ∈ Icc 0 1) :
    IsMaxOn (fun t : P × ℝ ↦ bernoulli z 0 t.2) (D ×ˢ Icc 0 1) (p,w) ↔ w = 1
```

### `UnivariateML.Trunc3Zero.modified_face_limit`

Whenever a compatible path has a positive-part likelihood limit, its zero-modified limit is the product with the limiting Bernoulli factor.

```lean
theorem modified_face_limit {P : Type*} {F : Filter P} {z k : ℕ}
    {w L : P → ℝ} {w₀ L₀ : ℝ}
    (hw : Tendsto w F (𝓝 w₀)) (hL : Tendsto L F (𝓝 L₀)) :
    Tendsto (fun t ↦ bernoulli z k (w t) * L t) F (𝓝 (bernoulli z k w₀ * L₀))
```

### `UnivariateML.Trunc3Zero.inflated_clip`

For a parent with a nonzero positive part, free inflation clips the effective zero fraction at the parent zero mass.

```lean
theorem inflated_clip {z k : ℕ} (hn : 0 < z+k) {f rho L : ℝ}
    (hf : 0 ≤ f) (hf' : f < 1) (hrho : rho ∈ Icc 0 1) (hL : 0 < L) :
    IsMaxOn (fun r ↦ bernoulli z k (f + (1-f)*r) * L) (Icc 0 1) rho ↔
      f + (1-f)*rho = max f (zeroFraction z k)
```

### `UnivariateML.Trunc3Zero.inflated_transfer`

A conditional maximizer transfers whenever its empirical effective zero mass can be realized. The inflation set may be a singleton or the full unit interval.

```lean
theorem inflated_transfer {P : Type*} {D : Set P} {R : Set ℝ}
    {z k : ℕ} (hn : 0 < z+k) {f L : P → ℝ}
    (hf : ∀ q ∈ D, f q ∈ Icc 0 1) (hR : R ⊆ Icc 0 1)
    (hL : ∀ q ∈ D, 0 ≤ L q)
    {p : P} {rho : ℝ} (hp : p ∈ D) (hr : rho ∈ R)
    (hmax : IsMaxOn L D p)
    (he : f p + (1-f p)*rho = zeroFraction z k) :
    IsMaxOn (fun t : P × ℝ ↦ bernoulli z k (f t.1 + (1-f t.1)*t.2) * L t.1)
      (D ×ˢ R) (p,rho)
```

### `UnivariateML.Trunc3Zero.inflated_saturated_max_iff`

Once a positive-part winner can realize the empirical effective zero weight, all inflated winners are precisely the positive-part winners that realize it. The allowed inflation set can also be a singleton.

```lean
theorem inflated_saturated_max_iff {P : Type*} {D : Set P} {R : Set ℝ}
    {z k : ℕ} (hn : 0 < z+k) {f L : P → ℝ}
    (hf : ∀ q ∈ D, f q ∈ Icc 0 1) (hR : R ⊆ Icc 0 1)
    (hL : ∀ q ∈ D, 0 ≤ L q) (hpos : ∃ q ∈ D, 0 < L q)
    {q : P} {r : ℝ} (hq : q ∈ D) (hr : r ∈ R)
    (hmax : IsMaxOn L D q) (he : f q + (1-f q)*r = zeroFraction z k)
    {p : P} {rho : ℝ} (hp : p ∈ D) (hrho : rho ∈ R) :
    IsMaxOn (fun t : P × ℝ ↦ bernoulli z k (f t.1 + (1-f t.1)*t.2) * L t.1)
      (D ×ˢ R) (p,rho) ↔
    IsMaxOn L D p ∧ f p + (1-f p)*rho = zeroFraction z k
```

### `UnivariateML.Trunc3Zero.inflated_free_feasible`

If a positive-part winner has parent zero mass below the empirical fraction, the displayed free inflation coefficient realizes it.

```lean
theorem inflated_free_feasible {P : Type*} {D : Set P} {z k : ℕ} (hk : 0 < k)
    {f L : P → ℝ} (hf : ∀ q ∈ D, f q ∈ Icc 0 1)
    (hL : ∀ q ∈ D, 0 ≤ L q) {p : P} (hp : p ∈ D)
    (hmax : IsMaxOn L D p) (hfit : f p ≤ zeroFraction z k) :
    let rho := (zeroFraction z k - f p)/(1-f p)
    rho ∈ Icc 0 1 ∧
      IsMaxOn (fun t : P × ℝ ↦ bernoulli z k (f t.1 + (1-f t.1)*t.2) * L t.1)
        (D ×ˢ Icc 0 1) (p,rho)
```

### `UnivariateML.Trunc3Zero.inflated_zero`

Free inflation on all zeros attains at inflation one or an included parent zero atom.

```lean
theorem inflated_zero {z : ℕ} (hz : 0 < z) {f rho : ℝ}
    (hf : f ∈ Icc (0 : ℝ) 1) (hrho : rho ∈ Icc (0 : ℝ) 1) :
    (rho + (1-rho)*f)^z = 1 ↔ f = 1 ∨ rho = 1
```

### `UnivariateML.Trunc3Zero.inflated_zero_held`

Holding inflation below one preserves the zero-only parent ranking.

```lean
theorem inflated_zero_held {z : ℕ} (hz : 0 < z) {f g rho : ℝ}
    (hf : 0 ≤ f) (hg : 0 ≤ g) (hrho : rho ∈ Ico 0 1) :
    (rho + (1-rho)*f)^z ≤ (rho + (1-rho)*g)^z ↔ f ≤ g
```

### `UnivariateML.Trunc3Zero.inflated_factorization`

Inflated parent masses factor through the positive conditional law when the parent positive mass is nonzero.

```lean
theorem inflated_factorization {k : ℕ} (z : ℕ) (x : Fin k → ℕ)
    (f rho : ℝ) (hf : f < 1) (mass : ℕ → ℝ) :
    (f + (1-f)*rho)^z * (∏ i, (1-rho)*mass (x i)) =
      bernoulli z k (f + (1-f)*rho) * ∏ i, mass (x i)/(1-f)
```

### `UnivariateML.Trunc3Zero.inflated_no_zeros`

Without zeros, free inflation keeps the parent optimum at zero inflation. Strictly positive parent likelihood excludes positive inflation.

```lean
theorem inflated_no_zeros {P : Type*} {D : Set P} {k : ℕ} (hk : 0 < k)
    {L : P → ℝ} (hL : ∀ q ∈ D, 0 ≤ L q) {p : P} {rho : ℝ}
    (hp : p ∈ D) (hr : rho ∈ Icc 0 1) (hpos : ∃ q ∈ D, 0 < L q) :
    IsMaxOn (fun t : P × ℝ ↦ (1-t.2)^k * L t.1) (D ×ˢ Icc 0 1) (p,rho)
      ↔ IsMaxOn L D p ∧ rho = 0
```

### `UnivariateML.Trunc3Zero.inflated_no_zeros_held`

Without zeros, any held inflation below one preserves the ordering of parent likelihoods.

```lean
theorem inflated_no_zeros_held {P : Type*} {D : Set P} {k : ℕ} {L : P → ℝ}
    {rho : ℝ} (hr : rho < 1) (p : P) :
    IsMaxOn (fun q ↦ (1-rho)^k * L q) D p ↔ IsMaxOn L D p
```

### `UnivariateML.Trunc3Zeta.sample_logLik`

On a nonempty sample the sum of log masses depends on the data through the empirical log count.

```lean
theorem sample_logLik {N n : ℕ} (hN : 1 ≤ N) (hn : 0 < n) (x : Fin n → Finset.Icc 1 N) (s : ℝ) :
    (∑ i, log (mass N (x i).val s)) =
      (n : ℝ) * logLik N ((∑ i, log ((x i).val : ℝ))/(n : ℝ)) s
```

### `UnivariateML.Trunc3Zeta.signed_fit`

A nontrivial window fits exactly at interior empirical log means, with a unique signed exponent.

```lean
theorem signed_fit {N : ℕ} (hN : 1 < N) (m : ℝ) :
    (∃ s, IsMaxOn (logLik N m) univ s) ∧
      (∀ s t, IsMaxOn (logLik N m) univ s → IsMaxOn (logLik N m) univ t → s = t)
      ↔ m ∈ Ioo 0 (log (N : ℝ))
```

### `UnivariateML.Trunc3Zeta.uniform_fit`

The finite exponent zero fits at the uniform mean log count.

```lean
theorem uniform_fit {N : ℕ} (hN : 1 ≤ N) :
    let A := Finset.Icc 1 N
    let m := (∑ k ∈ A, log (k : ℝ)) / (A.card : ℝ)
    IsMaxOn (logLik N m) univ 0
```

### `UnivariateML.Trunc3Zeta.singleton`

A compatible singleton puts unit mass at one for every exponent.

```lean
theorem singleton (s : ℝ) : mass 1 1 s = 1 ∧ IsMaxOn (logLik 1 0) univ s
```

### `UnivariateML.Trunc3Zeta.lower_atom`

At positive infinite exponent the window concentrates at one.

```lean
theorem lower_atom {N j : ℕ} (hj : j ∈ Finset.Icc 1 N) :
    Tendsto (mass N j) atTop (𝓝 (if j = 1 then 1 else 0))
```

### `UnivariateML.Trunc3Zeta.upper_atom`

At negative infinite exponent the window concentrates at its upper cell.

```lean
theorem upper_atom {N j : ℕ} (hj : j ∈ Finset.Icc 1 N) :
    Tendsto (mass N j) atBot (𝓝 (if j = N then 1 else 0))
```

### `UnivariateML.Trunc3Zeta.endpoint_supremum`

Endpoint-only samples on a nontrivial window have supremum one, approached by the appropriate signed exponent and unattained at finite exponents.

```lean
theorem endpoint_supremum {N n : ℕ} (hN : 1 < N) (hn : 0 < n)
    {j : ℕ} (hj : j = 1 ∨ j = N) :
    IsLUB (range (fun s ↦ mass N j s ^ n)) 1 ∧ ∀ s, mass N j s ^ n < 1
```

