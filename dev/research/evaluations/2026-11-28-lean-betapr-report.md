# TASK-499 — Beta-prime and inverse-Burr escape and attainment

The held-shape beta-prime criteria, both all-free beta-prime claims, and the joined all-free inverse-Burr failure claim are kernel-checked. The two inverse-Burr screens establish strict dominance by their respective faces. Finite contact with the higher face counts as attainment in the criteria.

The worktree is `codex/lean-betapr` at base `309bf1f8`. The atlas fragment records 28 public theorems. Its coverage fields are empty for independent review. [Atlas fragment](../existence-atlas/lean-map.d/lean-betapr.csv) · [Previous open targets](2026-11-28-lean-base9-report.md#open-targets)

## Scope and effort

The times are approximate hands-on proof and iteration time. The all-free beta-prime claims share the concentration envelope and moving-face analysis. The two inverse-Burr screens share the reciprocal chart and the dual comparison.

| Atlas target | Main result | Effort |
| --- | --- | --- |
| Beta prime, `shape1`, criterion | `BPHeld.held_first_criterion` gives a finite stationary list for every positive hold and includes finite equality with gamma. | About 10 minutes for the scale equation, analytic inverse and the two score tails. |
| Beta prime, `shape2`, criterion | `BPHeld.held_second_criterion` transports the list and its derivatives through exact inversion. | About 3 minutes for the reciprocal comparison and derivative identity. |
| Beta prime, all free, criterion | `BPJoint.all_free_criterion` compares the interior ratio profile with the larger of the two optimized face heights. Constants do not attain. | About 7 minutes beyond the shared envelope for compact profiling, continuity and the equality comparison. |
| Beta prime, all free, failure | `BPJoint.all_free_failure` proves divergence on constants and subsequential exhaustion by the two gamma faces on nonconstant data. | About 8 minutes for the uniform exteriors, moving-face limits and escape argument. |
| Inverse Burr, all free, failure, power screen | `BPInverseBurr.inverse_all_free_failure` uses the finite normalized-log convex-order test. The exact reference and its strict comparison to both finite fits and inverse Weibull are proved. | About 10 minutes for the dual reference proof, finite convex order and transport. |
| Inverse Burr, all free, failure, inverse-Weibull screen | The same joined theorem uses the written signed seam, entropy cutoff and finite dual cover. It also admits an alternative seam bound from fitted Weibull weights. | About 15 minutes for the three-region comparison, the directional Hessian proof and the exact example. |

## Beta prime with one shape held

At fixed positive first shape, the fitted scale is the unique positive zero of the log-scale score. Solving the scale equation for the free shape makes it a rational, strictly increasing function of the scale and gives an analytic local inverse. The profile derivative is the partial shape score at that scale. The small-shape limit of the score multiplied by the free shape is the sample count, so the score is strictly positive near zero.

For the large-shape tail, write (S(q)) for the total profiled score and (A(h)) for the data logarithm term, analytic at (h=1/q=0). The digamma recurrence gives
[
S(q+1)-S(q)
= nigl((p+q)^{-1}-q^{-1}igr)
  -igl(A(1/(q+1))-A(1/q)igr).
]
The right-hand side is analytic in (h) at zero. If it vanished identically there, the score would be constant on each unit-step tail. The score tends to zero, so it would then vanish on an open tail, contradicting its small-shape sign by analyticity. Its unit-step difference therefore has a strict eventual sign. Monotone unit steps to the zero limit give the opposite strict sign for the score. This proof covers noninteger and integer (p) with the same argument. The source's all-order Laplace expansion is an alternative route to the tail sign.

The two signs confine the zeros to a positive compact interval, and isolated zeros give finiteness. The existing matched-face comparison turns the list into an exact criterion. The reciprocal data have the same profiled derivative in the free shape after a fixed Jacobian shift, so the stationary set and weak height comparison both transport. These statements include constant samples. [Held-shape source](2026-11-27-crit-open-math-report.md#betapr-held-shape1) · [Reciprocal source](2026-11-27-crit-open-math-report.md#betapr-held-shape2)

## All-free beta prime

The completed chart uses (r=p/(p+q)), (k=pq/(p+q)), and (m=log b+log(p/q)). Splitting the beta integral at (r) bounds the beta normalizer below by (r^p(1-r)^q(1/p+1/q)). Thus the log mode height is at most (log k). The centered deviance is bounded below uniformly in (r) by (|z|+exp(-|z|)-1). Two distinct log observations give a positive lower bound on their combined loss for every (m). The resulting likelihood upper bound excludes (k) near zero and large (k) at every fixed superlevel. The same deviance inequality then bounds (m). The source's sharper large-concentration Stirling bound is unnecessary for these conclusions.

At (r=0), the gamma limit allows both (k) and (m) to move to their limits. Log-Gamma secants bound the moving shape increment, and exact inversion gives the limit at (r=1). These limits complete the likelihood continuously on the cylinder with (0le rle1) and (k>0). The two endpoint slices range over the positive gamma shape and rate and their inverse-gamma counterparts.

Each completed superlevel is compact. A subsequence of every competitive sequence reaches either the interior or one of those faces with the corresponding likelihood limit. An escape from all compact subsets of the interior rules out the first case. Subsequence language allows sequences to alternate between tied ends. A common compact nuisance set works for the entire closed ratio interval, and the maximum on each slice gives a continuous profile. The slice maximum uses `Generic/Escape.lean`; attainment at a finite contact point and strict face dominance use `Generic/Compactification.lean`. [Joint beta-prime source](2026-11-25-betapr-math-report.md#compact-scalar-decision-for-all-free-data)

## All-free inverse Burr

The Burr chart uses (y_i=log(x_{max}/x_i)). In the original units the summed inverse-Burr objective equals (n) times the mean Burr chart objective minus (sum_ilog x_i). This positive affine relation preserves strict inequalities, contact and both endpoint heights. The joined theorem starts with the positive sample and chooses a sample maximum internally. Its nonconstant branch permits power and inverse-Weibull heights to tie. Its constant branch proves unboundedness.

**Power.** The exact seven-log reference is the one in the written proof. Its strict finite-fit comparison uses the small-shape bound through (1/6) and two analytic dual-weight choices on the remaining shapes, meeting at (11/4). An exponential-mean lower bound from the two top observations proves strict separation from Weibull. Convex order of the normalized logs transports both comparisons. The finite test uses stop-loss inequalities at the union of the normalized sample and reference values. The equivalence to convex order is proved by interpolating any convex function on that finite support as an affine function plus nonnegative hinge terms. Repeated observations keep their multiplicities in the means.

**Inverse Weibull.** A zero-log count (m) gives strict exclusion through (m/(n-m)) when inverse Weibull exceeds power. The source seam uses the directional Hessian bound in the nuisance ball, the fitted Weibull score conditions and the explicit quadratic upper remainder. The proof checks the Hessian lower bound as a quadratic-form inequality, with determinant over trace available as an explicit two-dimensional bound. On the compact remainder, each interval has exact admissible weights at its endpoints, with the logarithmic chord error ((u-l)^2/(8l^2)). The same transport accepts a second signed seam from direct dual weighting of the fitted Weibull scores. A separate exact sample with one observation at (1) and seven at (e^{-1}) proves strict inverse-Weibull escape and strict separation from power. [Inverse Burr reciprocity](2026-11-25-burr-math-report.md#inverse-and-shifted-burr) · [Written sample checks](2026-11-27-checkable-math-report.md#invburr-held-all-free)

## Source scope and remaining work

The atlas claims above are proved at their stated scope. The held beta-prime proof establishes finiteness and exact contact as a mathematical comparison. Computing a general exact stationary-height tie decision remains open. The all-free beta-prime criterion needs comparison of an interior ratio height with the endpoint heights, without a joint uniqueness assertion.

The inverse-Burr screen theorems consume exact certificate inequalities. The original 571-interval inverse-Weibull certificate on (U(9/10)) remains in the written numerical evidence. The eight-observation example here has a separate analytic proof. No change to an audited mathematical claim was needed.

## Verification

All three standalone `lake env lean` checks passed on the workstation through the single-slot runner. The aggregate `lake build` passed with **8,610 jobs**, with the three temporary root imports present. The final formatting-only rebuild also passed with **8,610 jobs** and no warnings in the new modules. Existing modules emitted linter warnings. The temporary imports were removed and `git diff --exit-code dev/lean/UnivariateML.lean` passed. `python3 dev/tools/existence_atlas.py --check` passed.

## Public Lean statements

The statements below are copied from source. Each block uses the namespace and local notation of its module. In `BPInverseBurr`, `avg` is the empirical mean and `L`, `P`, and `W` are the existing Burr log-chart likelihood, optimized Pareto value, and Weibull log-chart likelihood.

### Held beta-prime shape criteria

#### `UnivariateML.BPHeld.scale_score_root`

The positive scale-score zero is the unique conditional scale maximum. [Lean source](../../lean/UnivariateML/Family/BPHeld.lean#L89)

```lean
theorem scale_score_root (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {p q : ℝ} (hp : 0 < p) (hq : 0 < q) :
    ∃ b > 0, IsMaxOn (fun s ↦ BetaPrime.logLik x s (p, q)) (Ioi 0) b ∧
      ∀ s > 0, scaleScore x p q s = 0 ↔ s = b
```

#### `UnivariateML.BPHeld.analytic_scale_profile`

The conditional scale varies analytically with the positive free shape. [Lean source](../../lean/UnivariateML/Family/BPHeld.lean#L173)

```lean
theorem analytic_scale_profile (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {p : ℝ} (hp : 0 < p) :
    ∃ b : ℝ → ℝ, AnalyticOnNhd ℝ b (Ioi 0) ∧
      ∀ q > 0, 0 < b q ∧ scaleScore x p q (b q) = 0 ∧
        IsMaxOn (fun s ↦ BetaPrime.logLik x s (p, q)) (Ioi 0) (b q)
```

#### `UnivariateML.BPHeld.profile_score_tails`

The profile score is analytic, positive near zero, and strictly signed at infinity for every positive held shape. [Lean source](../../lean/UnivariateML/Family/BPHeld.lean#L482)

```lean
theorem profile_score_tails (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {p : ℝ} (hp : 0 < p) {b : ℝ → ℝ}
    (hban : AnalyticOnNhd ℝ b (Ioi 0))
    (hb : ∀ q > 0, 0 < b q ∧ scaleScore x p q (b q) = 0) :
    let S := fun q ↦ shapeScore x p q (b q)
    AnalyticOnNhd ℝ S (Ioi 0) ∧
      (∀ᶠ q in 𝓝[>] 0, 0 < S q) ∧
      ((∀ᶠ q in atTop, 0 < S q) ∨ (∀ᶠ q in atTop, S q < 0))
```

#### `UnivariateML.BPHeld.held_first_criterion`

A finite maximum exists exactly when one of the finitely many stationary profile heights ties or exceeds the matched gamma height. [Lean source](../../lean/UnivariateML/Family/BPHeld.lean#L661)

```lean
theorem held_first_criterion (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {p : ℝ} (hp : 0 < p) :
    let L := fun v : ℝ × ℝ ↦ BetaPrime.logLik x v.2 (p, v.1)
    let G := ∑ i, Gamma.logDensity p (Gamma.rate x p) (x i)
    ∃ b : ℝ → ℝ, AnalyticOnNhd ℝ b (Ioi 0) ∧
      (∀ q > 0, 0 < b q ∧ IsMaxOn (fun s ↦ L (q, s)) (Ioi 0) (b q)) ∧
      let P := fun q ↦ L (q, b q)
      let C := {q | 0 < q ∧ deriv P q = 0}
      C.Finite ∧ ((∃ v ∈ Beta.domain, IsMaxOn L Beta.domain v) ↔
        ∃ q ∈ C, G ≤ P q)
```

#### `UnivariateML.BPHeld.held_second_criterion`

Reciprocity gives a finite stationary list and the same exact test against the matched inverse-gamma height. [Lean source](../../lean/UnivariateML/Family/BPHeld.lean#L705)

```lean
theorem held_second_criterion (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {q : ℝ} (hq : 0 < q) :
    let y := fun i ↦ (x i)⁻¹
    let J := ∑ i, 2 * log (x i)
    let L := fun v : ℝ × ℝ ↦ BetaPrime.logLik x v.2 (v.1, q)
    let G := (∑ i, Gamma.logDensity q (Gamma.rate y q) (y i)) - J
    ∃ b : ℝ → ℝ, AnalyticOnNhd ℝ b (Ioi 0) ∧
      (∀ p > 0, 0 < b p ∧ IsMaxOn (fun s ↦ L (p, s)) (Ioi 0) (b p)) ∧
      let P := fun p ↦ L (p, b p)
      let C := {p | 0 < p ∧ deriv P p = 0}
      C.Finite ∧ ((∃ v ∈ Beta.domain, IsMaxOn L Beta.domain v) ↔
        ∃ p ∈ C, G ≤ P p)
```

### Joint beta-prime compactification

#### `UnivariateML.BPJoint.peak_le_log_concentration`

The log-density at the mode is at most log harmonic concentration uniformly in the ratio. [Lean source](../../lean/UnivariateML/Family/BPJoint.lean#L107)

```lean
theorem peak_le_log_concentration {p q : ℝ} (hp : 0 < p) (hq : 0 < q) :
    peak p q ≤ log (concentration p q)
```

#### `UnivariateML.BPJoint.superlevel_envelope`

At every fixed likelihood level, positive harmonic concentration is bounded away from zero and infinity and log mode is bounded uniformly in the ratio. [Lean source](../../lean/UnivariateML/Family/BPJoint.lean#L324)

```lean
theorem superlevel_envelope (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) (A : ℝ) :
    ∃ lo hi R : ℝ, 0 < lo ∧
      ∀ v ∈ domain, A ≤ objective x v → v.2 ∈ Icc lo hi ×ˢ Icc (-R) R
```

#### `UnivariateML.BPJoint.continuous_completed`

The completed likelihood is continuous across both ends with concentration and log mode allowed to vary. [Lean source](../../lean/UnivariateML/Family/BPJoint.lean#L659)

```lean
theorem continuous_completed (x : Fin n → ℝ) (hx : ∀ i, 0 < x i) :
    ContinuousOn (completedObjective x) completedDomain
```

#### `UnivariateML.BPJoint.compact_superlevels`

Every completed superlevel is compact for a nonconstant positive sample. [Lean source](../../lean/UnivariateML/Family/BPJoint.lean#L756)

```lean
theorem compact_superlevels (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) (A : ℝ) :
    IsCompact {v ∈ completedDomain | A ≤ completedObjective x v}
```

#### `UnivariateML.BPJoint.competitive_subsequence`

Each competitive sequence has a subsequence converging to an interior point or to a proper gamma or inverse-gamma face, with likelihood convergence. [Lean source](../../lean/UnivariateML/Family/BPJoint.lean#L788)

```lean
theorem competitive_subsequence (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) (u : ℕ → ℝ × ℝ × ℝ) (hu : ∀ j, u j ∈ domain)
    (hbound : ∃ A, ∀ᶠ j in atTop, A ≤ objective x (u j)) :
    ∃ v ∈ completedDomain, ∃ φ : ℕ → ℕ, StrictMono φ ∧
      Tendsto (u ∘ φ) atTop (𝓝 v) ∧
      Tendsto (fun j ↦ objective x (u (φ j))) atTop (𝓝 (completedObjective x v)) ∧
      (v ∈ domain ∨ (v.1 = 0 ∧ completedObjective x v = gammaValue x v.2) ∨
        (v.1 = 1 ∧ completedObjective x v = inverseGammaValue x v.2))
```

#### `UnivariateML.BPJoint.common_profile_compact`

One compact nuisance set contains a maximum at every ratio, including both faces. [Lean source](../../lean/UnivariateML/Family/BPJoint.lean#L844)

```lean
theorem common_profile_compact (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) :
    ∃ K : Set (ℝ × ℝ), IsCompact K ∧ K ⊆ Ioi 0 ×ˢ univ ∧
      ∀ r ∈ Icc (0 : ℝ) 1, ∃ v ∈ K,
        IsMaxOn (fun z ↦ completedObjective x (r, z)) (Ioi 0 ×ˢ univ) v
```

#### `UnivariateML.BPJoint.all_free_criterion`

Constants have no finite maximum and nonconstant samples attain exactly when an interior profile height reaches the larger optimized face height. [Lean source](../../lean/UnivariateML/Family/BPJoint.lean#L976)

```lean
theorem all_free_criterion (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) :
    (∃ v ∈ domain, IsMaxOn (objective x) domain v) ↔
      (∃ i j, x i ≠ x j) ∧
        ∃ r ∈ Ioo (0 : ℝ) 1, max (ratioProfile x 0) (ratioProfile x 1) ≤ ratioProfile x r
```

#### `UnivariateML.BPJoint.strictly_winning_face`

Strict domination of every interior ratio makes the larger face the unattained supremum. [Lean source](../../lean/UnivariateML/Family/BPJoint.lean#L994)

```lean
theorem strictly_winning_face (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) (hne : ∃ i j, x i ≠ x j)
    (hlose : ∀ r ∈ Ioo (0 : ℝ) 1, ratioProfile x r < max (ratioProfile x 0) (ratioProfile x 1)) :
    sSup (objective x '' domain) = max (ratioProfile x 0) (ratioProfile x 1) ∧
      ¬ ∃ v ∈ domain, IsMaxOn (objective x) domain v
```

#### `UnivariateML.BPJoint.continuous_ratio_profile`

The optimized ratio profile is continuous on the closed ratio interval. [Lean source](../../lean/UnivariateML/Family/BPJoint.lean#L1030)

```lean
theorem continuous_ratio_profile (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) : ContinuousOn (ratioProfile x) (Icc 0 1)
```

#### `UnivariateML.BPJoint.all_free_failure`

Constants diverge, and every competitive escape from the interior has a subsequence converging to a gamma or inverse-gamma face. [Lean source](../../lean/UnivariateML/Family/BPJoint.lean#L1075)

```lean
theorem all_free_failure (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) :
    ((∀ i j, x i = x j) → ¬ BddAbove (objective x '' domain)) ∧
    ((∃ i j, x i ≠ x j) → ∀ (u : ℕ → ℝ × ℝ × ℝ) (hu : ∀ j, u j ∈ domain),
      (∃ A, ∀ᶠ j in atTop, A ≤ objective x (u j)) →
      Tendsto (fun j ↦ (⟨u j, hu j⟩ : domain)) atTop (cocompact domain) →
      ∃ v ∈ completedDomain, ∃ φ : ℕ → ℕ, StrictMono φ ∧
        Tendsto (u ∘ φ) atTop (𝓝 v) ∧
        ((v.1 = 0 ∧ Tendsto (fun j ↦ objective x (u (φ j))) atTop (𝓝 (gammaValue x v.2))) ∨
         (v.1 = 1 ∧ Tendsto (fun j ↦ objective x (u (φ j)))
          atTop (𝓝 (inverseGammaValue x v.2)))))
```

### Inverse-Burr failure and strict comparisons

#### `UnivariateML.BPInverseBurr.dual_upper`

Exact dual weights at a fixed shape bound the likelihood for every finite power and intercept. [Lean source](../../lean/UnivariateML/Family/BPInverseBurr.lean#L127)

```lean
theorem dual_upper (hn : 0 < n) (y w : Fin n → ℝ) {k c : ℝ} (b : ℝ)
    (hk : 0 < k) (hc : 0 < c) (hw : ∀ i, w i ∈ Icc (0 : ℝ) 1)
    (havg : avg w = 1 / (1 + k)) (ha : 0 < dualRate y w k) :
    L y (k, c, b) ≤ dualValue y w k
```

#### `UnivariateML.BPInverseBurr.entropy_small_shape`

The number of observations at zero yields a strict small-shape power bound including its cutoff. [Lean source](../../lean/UnivariateML/Family/BPInverseBurr.lean#L176)

```lean
theorem entropy_small_shape (hn : 0 < n) {y : Fin n → ℝ}
    (hy : 0 < avg y) {k c b : ℝ} (hk : 0 < k) (hc : 0 < c)
    (hsmall : (n : ℝ) * k ≤ (1 + k) * BurrHeld.zeroCount y) :
    L y (k, c, b) < P y
```

#### `UnivariateML.BPInverseBurr.power_reference_dominance`

The exact seven-log reference has strict power dominance over every finite Burr fit. [Lean source](../../lean/UnivariateML/Family/BPInverseBurr.lean#L238)

```lean
theorem power_reference_dominance :
    ∀ v ∈ BurrFree.domain, L powerReference v < P powerReference
```

#### `UnivariateML.BPInverseBurr.finite_normalized_order_iff`

Checking stop-loss heights on the union of both normalized supports is equivalent to empirical convex order. [Lean source](../../lean/UnivariateML/Family/BPInverseBurr.lean#L471)

```lean
theorem finite_normalized_order_iff {m : ℕ} (hn : 0 < n) (hm : 0 < m)
    {y : Fin n → ℝ} {z : Fin m → ℝ} (hy : 0 < avg y) (hz : 0 < avg z) :
    FiniteNormalizedOrder y z ↔ normalizedConvexOrder y z
```

#### `UnivariateML.BPInverseBurr.inverse_power_convex_order`

Normalized reciprocal logs dominating the reference imply that power strictly exceeds every finite fit and inverse Weibull. [Lean source](../../lean/UnivariateML/Family/BPInverseBurr.lean#L697)

```lean
theorem inverse_power_convex_order (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) (hne : ∃ i k, x i ≠ x k) {j : Fin n} (hj : ∀ i, x i ≤ x j) :
    let y := BurrFree.logExcess (Transfer.reciprocal x) j
    let F := fun p : ℝ × ℝ × ℝ ↦ ShapeFacesInverseBurr.logLik x p.1 p.2.1 p.2.2
    let B := n * P y - ∑ i, log (x i)
    normalizedConvexOrder y powerReference →
    (∀ c > 0, ∀ b, W y c b < P y) ∧
    IsLUB (F '' BurrFree.physicalDomain) B ∧ ∀ p ∈ BurrFree.physicalDomain, F p < B
```

#### `UnivariateML.BPInverseBurr.weibull_dual_seam`

A uniform second-moment inequality on the fitted Weibull weights yields an explicit signed tail cutoff. [Lean source](../../lean/UnivariateML/Family/BPInverseBurr.lean#L793)

```lean
theorem weibull_dual_seam (hn : 0 < n) (y : Fin n → ℝ) {cW bW V K : ℝ}
    (hcW : 0 < cW) (hW : ∀ d > 0, ∀ t, W y d t ≤ W y cW bW)
    (hK : 0 < K) (hV : ∀ i, exp (cW * y i + bW) ≤ V)
    (hgap : avg (fun i ↦ exp (cW * y i + bW) ^ 2) / 2 + V / (1 + K) < 1) :
    ∀ {k c b : ℝ}, K ≤ k → 0 < c → L y (k, c, b) < W y cW bW
```

#### `UnivariateML.BPInverseBurr.dual_cover_upper`

The finite entropy-and-chord cover strictly excludes all shapes in the covered interval. [Lean source](../../lean/UnivariateML/Family/BPInverseBurr.lean#L977)

```lean
theorem dual_cover_upper (hn : 0 < n) {y : Fin n → ℝ} {l u B : ℝ}
    (hcover : DualCover y l u B) {k c b : ℝ} (hk : k ∈ Icc l u) (hc : 0 < c) :
    L y (k, c, b) < B
```

#### `UnivariateML.BPInverseBurr.inverse_weibull_comparison`

The fitted-weight seam, entropy cutoff, and finite dual cover transport to strict inverse-Weibull escape. [Lean source](../../lean/UnivariateML/Family/BPInverseBurr.lean#L1036)

```lean
theorem inverse_weibull_comparison (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) (hne : ∃ i k, x i ≠ x k) {j : Fin n} (hj : ∀ i, x i ≤ x j)
    {cW bW K V : ℝ} (hcW : 0 < cW) (hK : 0 < K) :
    let y := BurrFree.logExcess (Transfer.reciprocal x) j
    let F := fun p : ℝ × ℝ × ℝ ↦ ShapeFacesInverseBurr.logLik x p.1 p.2.1 p.2.2
    let B := n * W y cW bW - ∑ i, log (x i)
    (∀ c > 0, ∀ b, W y c b ≤ W y cW bW) →
    P y < W y cW bW →
    (∀ i, exp (cW * y i + bW) ≤ V) →
    avg (fun i ↦ exp (cW * y i + bW) ^ 2) / 2 + V / (1 + K) < 1 →
    DualCover y (zeroCutoff y) K (W y cW bW) →
    IsLUB (F '' BurrFree.physicalDomain) B ∧ ∀ p ∈ BurrFree.physicalDomain, F p < B
```

#### `UnivariateML.BPInverseBurr.source_signed_seam_upper`

The written Hessian, score, and remainder inequalities bound the whole inverse-shape collar strictly below Weibull. [Lean source](../../lean/UnivariateML/Family/BPInverseBurr.lean#L1386)

```lean
theorem source_signed_seam_upper (hn : 0 < n) (y : Fin n → ℝ) {a b h : ℝ}
    (ha : 0 < a) (hh : 0 < h) (hW : ∀ c > 0, ∀ b', W y c b' ≤ W y a b)
    (hsource : SourceSignedSeam y a b h) :
    ∀ {k c b' : ℝ}, 1 / h ≤ k → 0 < c → L y (k, c, b') < W y a b
```

#### `UnivariateML.BPInverseBurr.det_over_trace_bound`

For the two-dimensional positive definite nuisance form, determinant over trace is a positive lower bound. [Lean source](../../lean/UnivariateML/Family/BPInverseBurr.lean#L1525)

```lean
theorem det_over_trace_bound {a b c : ℝ} (ha : 0 < a) (hc : 0 < c)
    (hdet : 0 < a * c - b ^ 2) :
    let μ := (a * c - b ^ 2) / (a + c)
    0 < μ ∧ ∀ d e : ℝ, μ * (d ^ 2 + e ^ 2) ≤ a * d ^ 2 + 2 * b * d * e + c * e ^ 2
```

#### `UnivariateML.BPInverseBurr.inverse_weibull_source_comparison`

The three separation checks in the written proof certify strict inverse-Weibull escape in physical coefficients. [Lean source](../../lean/UnivariateML/Family/BPInverseBurr.lean#L1551)

```lean
theorem inverse_weibull_source_comparison (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) (hne : ∃ i k, x i ≠ x k) {j : Fin n} (hj : ∀ i, x i ≤ x j)
    {cW bW h : ℝ} (hcW : 0 < cW) (hh : 0 < h) :
    let y := BurrFree.logExcess (Transfer.reciprocal x) j
    let F := fun p : ℝ × ℝ × ℝ ↦ ShapeFacesInverseBurr.logLik x p.1 p.2.1 p.2.2
    let B := n * W y cW bW - ∑ i, log (x i)
    (∀ c > 0, ∀ b, W y c b ≤ W y cW bW) →
    P y < W y cW bW → SourceSignedSeam y cW bW h →
    DualCover y (zeroCutoff y) (1 / h) (W y cW bW) →
    IsLUB (F '' BurrFree.physicalDomain) B ∧ ∀ p ∈ BurrFree.physicalDomain, F p < B
```

#### `UnivariateML.BPInverseBurr.inverse_all_free_failure`

Constants diverge. Otherwise strict exclusion at the larger face gives nonattainment and finite contact gives attainment. Both sample screens certify their respective strict winners. [Lean source](../../lean/UnivariateML/Family/BPInverseBurr.lean#L1572)

```lean
theorem inverse_all_free_failure (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) :
    let F := fun p : ℝ × ℝ × ℝ ↦ ShapeFacesInverseBurr.logLik x p.1 p.2.1 p.2.2
    let J := ∑ i, log (x i)
    let D := BurrFree.physicalDomain
    ((∀ i k, x i = x k) → ¬ BddAbove (F '' D)) ∧
    ((∃ i k, x i ≠ x k) → ∃ j : Fin n, (∀ i, x i ≤ x j) ∧
      let y := BurrFree.logExcess (Transfer.reciprocal x) j
      ∃ cW > 0, ∃ bW, (∀ c > 0, ∀ b, W y c b ≤ W y cW bW) ∧
        let B := n * max (P y) (W y cW bW) - J
        ((∃ p ∈ D, IsMaxOn F D p) ↔ ∃ p ∈ D, B ≤ F p) ∧
        ((¬ ∃ p ∈ D, IsMaxOn F D p) → IsLUB (F '' D) B ∧ ∀ p ∈ D, F p < B) ∧
        (FiniteNormalizedOrder y powerReference →
          W y cW bW < P y ∧ IsLUB (F '' D) (n * P y - J) ∧ ∀ p ∈ D, F p < n * P y - J) ∧
        (∀ h > 0, P y < W y cW bW → SourceSignedSeam y cW bW h →
          DualCover y (zeroCutoff y) (1 / h) (W y cW bW) →
          IsLUB (F '' D) (n * W y cW bW - J) ∧ ∀ p ∈ D, F p < n * W y cW bW - J) ∧
        (∀ K > 0, ∀ V, P y < W y cW bW →
          (∀ i, exp (cW * y i + bW) ≤ V) →
          avg (fun i ↦ exp (cW * y i + bW) ^ 2) / 2 + V / (1 + K) < 1 →
          DualCover y (zeroCutoff y) K (W y cW bW) →
          IsLUB (F '' D) (n * W y cW bW - J) ∧ ∀ p ∈ D, F p < n * W y cW bW - J))
```

#### `UnivariateML.BPInverseBurr.inverse_weibull_exact_example`

One observation at one and seven observations at the reciprocal of Euler's number give a strictly winning inverse-Weibull face. [Lean source](../../lean/UnivariateML/Family/BPInverseBurr.lean#L1809)

```lean
theorem inverse_weibull_exact_example :
    let x : Fin 8 → ℝ := fun i ↦ if i = 0 then 1 else exp (-1)
    let y := BurrFree.logExcess (Transfer.reciprocal x) 0
    let F := fun p : ℝ × ℝ × ℝ ↦ ShapeFacesInverseBurr.logLik x p.1 p.2.1 p.2.2
    ∃ a > 0, ∃ b, (∀ c > 0, ∀ t, W y c t ≤ W y a b) ∧ P y < W y a b ∧
      let B := 8 * W y a b - ∑ i, log (x i)
      IsLUB (F '' BurrFree.physicalDomain) B ∧ ∀ p ∈ BurrFree.physicalDomain, F p < B
```
