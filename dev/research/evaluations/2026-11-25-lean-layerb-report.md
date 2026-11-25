# Generic optimization lemmas for univariateML

**TASK-289 · 2026-11-25**

Lean verifies all five generic targets in exact arithmetic and re-proves
held-shape Lomax uniqueness in a separate demo. The statements below are
copied from `UnivariateML.Generic` and `UnivariateML.Generic.Demo`.

## Verification

The standalone `lake env lean` checks passed for all six new modules on
the workstation, including rechecks after two style fixes. The final
`lake build` passed without warnings and reported
`Build completed successfully (6682 jobs).` The runs held one slot
through `/tmp/umvl-cpu/run` and used the existing Mathlib cache.
The new proofs contain no `sorry`, `admit`, `axiom` or `native_decide`.

## Lean statements

### 1. Decreasing score

Mathlib’s `intermediate_value_Icc'` finds the zero from a sign bracket.
`StrictAntiOn.injOn` gives uniqueness. The mean-value lemmas
`strictMonoOn_of_deriv_pos` and `strictAntiOn_of_deriv_neg` give strict
improvement toward the zero on either side. The limit wrappers obtain
the bracket from eventual signs on nontrivial filters. The positive
parameter variant uses `Real.exp_log` and `Real.log_exp` to transfer the
root and the optimum between the original and log coordinates.
[Lean module](../../lean/UnivariateML/Generic/Score.lean)

```lean
theorem existsUnique_score_zero_of_signs {f score : ℝ → ℝ} {D : Set ℝ}
    (hconv : Convex ℝ D)
    (hd : ∀ t ∈ D, HasDerivAt f (score t) t) (hc : ContinuousOn score D)
    (hs : StrictAntiOn score D) {a b : ℝ}
    (ha : a ∈ D) (hb : b ∈ D) (hab : a ≤ b)
    (hsa : 0 < score a) (hsb : score b < 0) :
    (∃! t, t ∈ D ∧ score t = 0) ∧
      ∀ t ∈ D, IsMaxOn f D t ↔ score t = 0

theorem existsUnique_score_zero_of_limits {f score : ℝ → ℝ} {D : Set ℝ}
    {left right : Filter ℝ} [left.NeBot] [right.NeBot]
    (hconv : Convex ℝ D)
    (hd : ∀ t ∈ D, HasDerivAt f (score t) t) (hc : ContinuousOn score D)
    (hs : StrictAntiOn score D)
    (hlD : ∀ᶠ t in left, t ∈ D) (hrD : ∀ᶠ t in right, t ∈ D)
    {u v : ℝ} (hu : 0 < u) (hv : v < 0)
    (hl : Tendsto score left (𝓝 u)) (hr : Tendsto score right (𝓝 v)) :
    (∃! t, t ∈ D ∧ score t = 0) ∧
      ∀ t ∈ D, IsMaxOn f D t ↔ score t = 0

theorem existsUnique_score_zero_real {f score : ℝ → ℝ}
    (hd : ∀ t, HasDerivAt f (score t) t) (hc : Continuous score) (hs : StrictAnti score)
    {u v : ℝ} (hu : 0 < u) (hv : v < 0)
    (hl : Tendsto score atBot (𝓝 u)) (hr : Tendsto score atTop (𝓝 v)) :
    (∃! t, score t = 0) ∧ ∀ t, IsMaxOn f univ t ↔ score t = 0

theorem existsUnique_score_zero_Ioo {f score : ℝ → ℝ} {a b : ℝ} (hab : a < b)
    (hd : ∀ t ∈ Ioo a b, HasDerivAt f (score t) t) (hc : ContinuousOn score (Ioo a b))
    (hs : StrictAntiOn score (Ioo a b)) {u v : ℝ} (hu : 0 < u) (hv : v < 0)
    (hl : Tendsto score (𝓝[Ioo a b] a) (𝓝 u))
    (hr : Tendsto score (𝓝[Ioo a b] b) (𝓝 v)) :
    (∃! t, t ∈ Ioo a b ∧ score t = 0) ∧
      ∀ t ∈ Ioo a b, IsMaxOn f (Ioo a b) t ↔ score t = 0

theorem existsUnique_log_score_zero {f score : ℝ → ℝ}
    (hd : ∀ t, HasDerivAt (fun s ↦ f (Real.exp s)) (score t) t)
    (hc : Continuous score) (hs : StrictAnti score)
    {u v : ℝ} (hu : 0 < u) (hv : v < 0)
    (hl : Tendsto score atBot (𝓝 u)) (hr : Tendsto score atTop (𝓝 v)) :
    (∃! x, 0 < x ∧ score (Real.log x) = 0) ∧
      ∀ x > 0, IsMaxOn f (Ioi 0) x ↔ score (Real.log x) = 0
```

### 2. Attained fibers and joint maxima

Mathlib’s `IsMaxOn` supplies the comparison at each feasible point.
The joint comparison composes the fiber inequality with the profile
inequality. A joint maximum is also a maximum in its own fiber, so
unique fiber optima identify its nuisance parameter. Comparing with all
profiled pairs identifies its outer parameter as a profile maximizer.
The objectives can take values in any preorder.
[Lean module](../../lean/UnivariateML/Generic/Profile.lean)

```lean
theorem isMaxOn_prod_of_profile {Θ Φ V : Type*} [Preorder V]
    {L : Θ → Φ → V} {T : Set Θ} {F : Set Φ} {best : Θ → Φ} {t : Θ}
    (hfiber : ∀ θ ∈ T, best θ ∈ F ∧ IsMaxOn (L θ) F (best θ))
    (ht : t ∈ T) (hprofile : IsMaxOn (fun θ ↦ L θ (best θ)) T t) :
    (t, best t) ∈ T ×ˢ F ∧ IsMaxOn (fun p : Θ × Φ ↦ L p.1 p.2) (T ×ˢ F) (t, best t)

theorem joint_maximizers_eq_profile_graph {Θ Φ V : Type*} [Preorder V]
    {L : Θ → Φ → V} {T : Set Θ} {F : Set Φ} {best : Θ → Φ}
    (hfiber : ∀ θ ∈ T, best θ ∈ F ∧ IsMaxOn (L θ) F (best θ))
    (hunique : ∀ θ ∈ T, ∀ φ ∈ F, IsMaxOn (L θ) F φ → φ = best θ) :
    {p ∈ T ×ˢ F | IsMaxOn (fun p : Θ × Φ ↦ L p.1 p.2) (T ×ˢ F) p} =
      (fun θ ↦ (θ, best θ)) '' {θ ∈ T | IsMaxOn (fun θ ↦ L θ (best θ)) T θ}
```

### 3. Feasible path limits

Mathlib’s `le_of_tendsto` compares the limit with every upper bound.
`le_csSup` supplies the comparison for a bounded image. The disjunction
with unboundedness lets callers apply the result before proving
`BddAbove`. Feasibility is required eventually along the path.
[Lean module](../../lean/UnivariateML/Generic/Limit.lean)

```lean
theorem limit_le_upperBound {X ι : Type*} {L : X → ℝ} {D : Set X}
    {l : Filter ι} [l.NeBot] {p : ι → X} {c u : ℝ}
    (hp : ∀ᶠ i in l, p i ∈ D) (hc : Tendsto (fun i ↦ L (p i)) l (𝓝 c))
    (hu : u ∈ upperBounds (L '' D)) : c ≤ u

theorem limit_le_sSup {X ι : Type*} {L : X → ℝ} {D : Set X}
    {l : Filter ι} [l.NeBot] {p : ι → X} {c : ℝ}
    (hp : ∀ᶠ i in l, p i ∈ D) (hc : Tendsto (fun i ↦ L (p i)) l (𝓝 c))
    (hB : BddAbove (L '' D)) : c ≤ sSup (L '' D)

theorem not_bddAbove_or_limit_le_sSup {X ι : Type*} {L : X → ℝ} {D : Set X}
    {l : Filter ι} [l.NeBot] {p : ι → X} {c : ℝ}
    (hp : ∀ᶠ i in l, p i ∈ D) (hc : Tendsto (fun i ↦ L (p i)) l (𝓝 c)) :
    ¬ BddAbove (L '' D) ∨ c ≤ sSup (L '' D)
```

### 4. Attainment and nonattainment on a compactification

Mathlib’s `IsCompact.exists_isMaxOn` supplies a maximum of the extension.
`IsCompact.exists_isMaxOn_mem_subset` already handles a feasible witness
that strictly beats the boundary. The weak inequality here also handles
a tie. If the compact maximum is excluded, the witness attains its value.

For nonattainment, the strictly dominating boundary value must be
reachable. Take the identity on the compact set `{0, 1}` with feasible
set `{0}`. The excluded value is higher and the feasible maximum is
attained. The theorem requires the dominating point to lie in
`closure D`, as it does in a dense compactification. Mathlib’s
`mem_closure_iff_nhdsWithin_neBot` and `le_of_tendsto` then show that
every feasible upper bound lies above the boundary value. The
nonattainment result also holds for a noncompact superset.
[Lean module](../../lean/UnivariateML/Generic/Compactification.lean)

```lean
theorem exists_isMaxOn_of_boundary_le {X : Type*} [TopologicalSpace X]
    {L : X → ℝ} {D K : Set X} (hDK : D ⊆ K) (hK : IsCompact K)
    (hL : ContinuousOn L K) {x : X} (hx : x ∈ D)
    (hboundary : ∀ b ∈ K \ D, L b ≤ L x) :
    ∃ y ∈ D, IsMaxOn L D y

theorem sSup_eq_and_not_exists_isMaxOn_of_boundary_gt {X : Type*} [TopologicalSpace X]
    {L : X → ℝ} {D K : Set X} (hDK : D ⊆ K) (hL : ContinuousOn L K)
    {b : X} (hb : b ∈ K) (hcl : b ∈ closure D)
    (hboundary : ∀ x ∈ D, L x < L b) :
    sSup (L '' D) = L b ∧ ¬ ∃ x ∈ D, IsMaxOn L D x
```

### 5. Coercive escape and strict concavity

Mathlib already gives cocompact attainment via
`Continuous.exists_forall_ge` and strict-concavity uniqueness via
`StrictConcaveOn.eq_of_isMaxOn`. The wrappers apply attainment on the
feasible subtype and return a feasible `IsMaxOn`. On a finite-dimensional
open domain, `ConcaveOn.continuousOn` discharges continuity. The broader
statement accepts continuity explicitly. The filter `cocompact D` on
the subtype captures leaving every compact subset of the feasible
domain, including escape to infinity on unbounded domains.
[Lean module](../../lean/UnivariateML/Generic/Escape.lean)

```lean
theorem exists_isMaxOn_of_tendsto_cocompact {X : Type*} [TopologicalSpace X]
    {L : X → ℝ} {D : Set X} (hD : D.Nonempty) (hL : ContinuousOn L D)
    (hescape : Tendsto (fun x : D ↦ L x) (cocompact D) atBot) :
    ∃ x ∈ D, IsMaxOn L D x

theorem existsUnique_isMaxOn_of_strictConcaveOn_of_tendsto_cocompact
    {X : Type*} [TopologicalSpace X] [AddCommMonoid X] [Module ℝ X]
    {L : X → ℝ} {D : Set X} (hD : D.Nonempty) (hL : ContinuousOn L D)
    (hconc : StrictConcaveOn ℝ D L)
    (hescape : Tendsto (fun x : D ↦ L x) (cocompact D) atBot) :
    ∃! x, x ∈ D ∧ IsMaxOn L D x

theorem existsUnique_isMaxOn_of_isOpen_of_strictConcaveOn_of_tendsto_cocompact
    {X : Type*} [NormedAddCommGroup X] [NormedSpace ℝ X] [FiniteDimensional ℝ X]
    {L : X → ℝ} {D : Set X} (hD : D.Nonempty) (hopen : IsOpen D)
    (hconc : StrictConcaveOn ℝ D L)
    (hescape : Tendsto (fun x : D ↦ L x) (cocompact D) atBot) :
    ∃! x, x ∈ D ∧ IsMaxOn L D x
```

### 6. Lomax demonstration

The demo re-proves `Lomax.heldShape_unique` from the pilot’s derivative,
strict score monotonicity and endpoint limits. It checks continuity as a
finite sum with positive denominators. The log-score lemma handles the
bracket, the optimization and the change back to positive rate. The
demo uses no existing Lomax maximization result.
[Lean module](../../lean/UnivariateML/Generic/Demo.lean)

```lean
theorem heldShape_unique {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {κ : ℝ} (hk : 0 < κ) :
    ∃! rate, rate ∈ Ioi (0 : ℝ) ∧ IsMaxOn (Lomax.logLik x κ) (Ioi 0) rate
```

## Reuse in open family proofs

These are applications of the lemmas once each family supplies the
stated hypotheses. [Open mathematics](../../STATUS.md#open-mathematics-from-the-job-reports)

| Family problem | Reusable proof step | Family-specific work still open |
| --- | --- | --- |
| Discretised gamma free fit | Lift the unique rate fiber to the joint argmax graph. Use the score bracket on slices with a strictly decreasing score and opposite signs. | General profiled-shape quasi-concavity, the stationary curvature inequality and sufficient checks for cells containing zero. |
| Burr variants and Fréchet free shape | Transfer maximizing shapes to joint maxima when nuisance maxima are attained and unique in a common chart. Use the path bound for reachable faces such as the Fréchet Gumbel limit. | Exclude or order finite roots and faces. Fixed physical folded-normal location still requires its own profile. |
| Generalized beta, GB2 and Feller–Pareto | Lift attained inner optima. Bound the supremum from feasible face paths. A finite witness suffices for attainment once the continuous compactification and boundary bounds are proved. | Uniform signed gamma neighborhoods, cancellation-seam bounds, profile exhaustion and exact finite/face comparisons. |
| Student and translated half-t | Use the score lemma on strictly decreasing conditional log-scale scores with opposite signs. Apply the escape theorem to nuisance problems with coercivity and strict concavity in a convex chart. Use path limits for the normal and collapse competitors. | Degenerate roots, moving nuisances and exact competing values. Multiple Student shape maxima preclude a strictly decreasing score over the whole shape domain. |
| Lomax displaced roots and the GPD uniform tie | Use the limit and compactification lemmas for the Lomax exponential endpoint. Lift maximizing GPD profile roots in a chart with unique attained fibers and compare with the attainable uniform candidate. | Bound displaced Lomax roots and count all GPD roots at the tied value. |

No generic proof obligations remain open for TASK-289.
