# Lean base likelihood claims for discrete families

TASK-490 · 2026-11-28 · 18 public theorems in seven modules for 46 written atlas questions.

The new results cover the fixed-shape, fixed-size beta-binomial endpoint
statements, the CMB fixed-probability and fixed-size comparisons, the CMP
attainment and uniqueness criteria, the held-mean PIG classification, and
the held-probability and held-size negative-binomial failure statements.
The beta-negative-binomial results establish transport under the size–beta
swap and the all-zero branches for the two symmetric holds.

The [atlas fragment](../existence-atlas/lean-map.d/lean-base8.csv) leaves coverage
blank for independent review.

## Verification

All seven files passed the standalone Lean check on the workstation.
The final aggregate `lake build` passed with all seven imports and 8516 jobs.
The build emitted Lean lint warnings. The temporary imports were restored
and the root file diff was empty. The atlas check passed with exit code zero.
The new files contain no proof placeholders.

## Lean statements

### `UnivariateML.Base8BBinom.held_alpha_size_failure`

Joins the two point-mass directions, strict endpoint-constant suprema and size-zero flatness.

Atlas target · `alpha+size failure`.

[Lean source](../../lean/UnivariateML/Family/Base8BBinom.lean#L18)

```lean
theorem held_alpha_size_failure {m n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∀ i, x i ≤ m) {a : ℝ} (ha : 0 < a) :
    (m = 0 → ∀ b > 0, likelihood m x a b = 1) ∧
    (0 < m →
      ((∀ i, x i = 0) → Tendsto (likelihood m x a) atTop (𝓝 1)) ∧
      ((∀ i, x i = m) → Tendsto (likelihood m x a) (𝓝[>] 0) (𝓝 1)) ∧
      (((∀ i, x i = 0) ∨ (∀ i, x i = m)) →
        IsLUB (likelihood m x a '' Ioi 0) 1 ∧ ∀ b > 0, likelihood m x a b < 1))
```

### `UnivariateML.Base8BBinom.held_beta_size_failure`

Gives both count-mass limits, strict endpoint-constant likelihood suprema and size-zero flatness.

Atlas target · `beta+size failure`.

[Lean source](../../lean/UnivariateML/Family/Base8BBinom.lean#L33)

```lean
theorem held_beta_size_failure {m n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∀ i, x i ≤ m) {b : ℝ} (hb : 0 < b) :
    (m = 0 → ∀ a > 0, likelihood m x a b = 1) ∧
    (0 < m →
      (∀ k ≤ m,
        Tendsto (fun a ↦ Discrete5BBinom.countMass m k a b) (𝓝[>] 0)
          (𝓝 (if k = 0 then 1 else 0)) ∧
        Tendsto (fun a ↦ Discrete5BBinom.countMass m k a b) atTop
          (𝓝 (if k = m then 1 else 0))) ∧
      (((∀ i, x i = 0) ∨ (∀ i, x i = m)) →
        IsLUB ((fun a ↦ likelihood m x a b) '' Ioi 0) 1 ∧ ∀ a > 0, likelihood m x a b < 1))
```

### `UnivariateML.Base8CMB.held_dispersion_failure`

Every feasible fixed-size probability profile and its capped global comparison attain.

Atlas target · `nu failure`.

[Lean source](../../lean/UnivariateML/Family/Base8CMB.lean#L17)

```lean
theorem held_dispersion_failure {n B : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (ν : ℝ) (hx : ∀ i, x i ≤ B) :
    (∀ N ≤ B, (∀ i, x i ≤ N) →
      ∃ p ∈ Icc (0 : ℝ) 1,
        IsMaxOn (fun q ↦ sizeLikelihood x N q ν) (Icc 0 1) p) ∧
    ∃ N ≤ B, (∀ i, x i ≤ N) ∧ ∃ p ∈ Icc (0 : ℝ) 1,
      ∀ J ≤ B, (∀ i, x i ≤ J) → ∀ q ∈ Icc (0 : ℝ) 1,
        sizeLikelihood x J q ν ≤ sizeLikelihood x N p ν
```

### `UnivariateML.Base8CMB.held_probability_dispersion_maximizers`

All sizes at the winning height maximize, with only the smallest size at zero dispersion and interior probability.

Atlas target · `nu+prob uniqueness`.

[Lean source](../../lean/UnivariateML/Family/Base8CMB.lean#L46)

```lean
theorem held_probability_dispersion_maximizers {n B : ℕ} (hn : 0 < n)
    (x : Fin n → ℕ) (p ν : ℝ) (hB : ∀ i, x i ≤ B) :
    let D : Set ℕ := {N | N ≤ B ∧ Compatible x N p}
    (D.Nonempty → ∃ v : ℝ, ∀ N ∈ D,
      IsMaxOn (fun J ↦ sizeLikelihood x J p ν) D N ↔ sizeLikelihood x N p ν = v) ∧
    (ν = 0 → p ∈ Ioo (0 : ℝ) 1 →
      ∀ N ∈ D, IsMaxOn (fun J ↦ sizeLikelihood x J p ν) D N ↔ N = Finset.univ.sup x)
```

### `UnivariateML.Base8CMB.held_size_failure`

Joins strict saturated suprema for the three fixed-size excluded support patterns.

Atlas target · `size failure`.

Full closed probability chart, including the two losing endpoint atoms.

[Lean source](../../lean/UnivariateML/Family/Base8CMB.lean#L98)

```lean
theorem held_size_failure {N n : ℕ} (hN : 2 ≤ N) (hn : 0 < n)
    (x : Fin n → Fin (N+1)) :
    let D := Icc (0 : ℝ) 1 ×ˢ (univ : Set ℝ)
    let L := fun q : ℝ × ℝ ↦ closedLikelihood x q.1 q.2
    let Excluded := fun v : ℝ ↦ IsLUB (L '' D) v ∧ ∀ p ∈ D, L p < v
    (∀ k : Fin (N+1), 0 < k.val → k.val < N → (∀ i, x i = k) → Excluded 1) ∧
    (∀ l u : Fin (N+1), u.val = l.val+1 → (∀ i, x i = l ∨ x i = u) →
      0 < Discrete3CMB.multiplicity x l → 0 < Discrete3CMB.multiplicity x u →
      Excluded (pairHeight (Discrete3CMB.multiplicity x l) (Discrete3CMB.multiplicity x u))) ∧
    ((∀ i, x i = 0 ∨ x i = Fin.last N) →
      0 < Discrete3CMB.multiplicity x 0 → 0 < Discrete3CMB.multiplicity x (Fin.last N) →
      Excluded (pairHeight (Discrete3CMB.multiplicity x 0)
        (Discrete3CMB.multiplicity x (Fin.last N))))
```

### `UnivariateML.Base8CMB.held_size_maximizers`

Size-zero, Bernoulli and point-mass fibers are explicit, and the identifiable interior has exactly one fit.

Atlas target · `size uniqueness`.

[Lean source](../../lean/UnivariateML/Family/Base8CMB.lean#L154)

```lean
theorem held_size_maximizers {N n : ℕ} (hn : 0 < n) (x : Fin n → Fin (N+1)) :
    let D := Icc (0 : ℝ) 1 ×ˢ (univ : Set ℝ)
    let L := fun q : ℝ × ℝ ↦ closedLikelihood x q.1 q.2
    (N = 0 → ∀ q ∈ D, IsMaxOn L D q) ∧
    (N = 1 → ∀ q ∈ D, IsMaxOn L D q ↔
      q.1 = (Discrete3CMB.multiplicity x (Fin.last N) : ℝ) /
        (Discrete3CMB.multiplicity x (Fin.last N) + Discrete3CMB.multiplicity x 0)) ∧
    (2 ≤ N →
      ((∀ i, x i = 0) → ∀ q ∈ D, IsMaxOn L D q ↔ q.1 = 0) ∧
      ((∀ i, x i = Fin.last N) → ∀ q ∈ D, IsMaxOn L D q ↔ q.1 = 1) ∧
      ((¬ ∀ i, x i = 0) → (¬ ∀ i, x i = Fin.last N) →
        ((∃! q, q ∈ D ∧ IsMaxOn L D q) ↔
          FinishCMB.empirical x ∈ interior (convexHull ℝ (range (FinishCMB.statistic N))))))
```

### `UnivariateML.Base8CMBHeldProbability.held_probability_size_classification`

Strictly interior empirical height gives a unique dispersion fit. Flat size and probability cases retain every dispersion.

Atlas targets · `prob+size criterion`, `prob+size uniqueness`.

[Lean source](../../lean/UnivariateML/Family/Base8CMBHeldProbability.lean#L99)

```lean
theorem held_probability_size_classification {N n : ℕ} (hn : 0 < n)
    (x : Fin n → Fin (N+1)) {p : ℝ} (hp : p ∈ Icc 0 1) :
    let f := closedLikelihood x p
    let m := (∑ i, height N (x i))/(n : ℝ)
    let Inner := (∃ k, height N k < m) ∧ (∃ k, m < height N k)
    ((p = 0 ∨ p = 1 ∨ N ≤ 1) → ∀ t, IsMaxOn f univ t) ∧
    ((p ∈ Ioo 0 1 ∧ 2 ≤ N) →
      ((∃ t, IsMaxOn f univ t) ↔ Inner) ∧
      (Inner → ∃! t, IsMaxOn f univ t))
```

### `UnivariateML.Base8CMBHeldProbability.held_probability_size_failure`

Extremal empirical heights give sharp, strictly excluded centered or endpoint limits.

Atlas target · `prob+size failure`.

[Lean source](../../lean/UnivariateML/Family/Base8CMBHeldProbability.lean#L230)

```lean
theorem held_probability_size_failure {N n : ℕ} (hn : 0 < n) (hN : 2 ≤ N)
    (x : Fin n → Fin (N+1)) {p : ℝ} (hp : p ∈ Ioo 0 1) :
    let m := (∑ i, height N (x i))/(n : ℝ)
    let F := Finset.univ.filter (fun k : Fin (N+1) ↦ height N k = m)
    let η := log (p/(1-p))
    let v := ∏ i, exp (η*(x i).val)/(∑ j ∈ F, exp (η*j.val))
    let f := closedLikelihood x p
    ((∀ k, height N k ≤ m) →
      (∀ k : Fin (N+1), k ∈ F ↔ k.val = N/2 ∨ k.val = N-N/2) ∧
      Tendsto f atTop (𝓝 v) ∧ IsLUB (range f) v ∧ ∀ t, f t < v) ∧
    ((∀ k, m ≤ height N k) →
      (∀ k : Fin (N+1), k ∈ F ↔ k.val = 0 ∨ k.val = N) ∧
      Tendsto (fun t ↦ f (-t)) atTop (𝓝 v) ∧ IsLUB (range f) v ∧ ∀ t, f t < v)
```

### `UnivariateML.Base8CMBHeldProbability.held_probability_capped`

The capped profile comparison keeps all attained ties and reaches a winning excluded height by its signed path when all attained profiles lose.

Atlas targets · `prob criterion`, `prob failure`, `prob uniqueness`.

[Lean source](../../lean/UnivariateML/Family/Base8CMBHeldProbability.lean#L376)

```lean
theorem held_probability_capped {n B : ℕ} (hn : 0 < n)
    (x : Fin n → ℕ) {p : ℝ} (hp : p ∈ Icc 0 1) :
    let D : Set ℕ := {N | N ≤ B ∧ Discrete4CMB.Compatible x N p}
    let L := fun q : ℕ × ℝ ↦ Discrete4CMB.sizeLikelihood x q.1 p q.2
    let Att := fun N : ℕ ↦ ∃ t, IsMaxOn (fun u ↦ L (N,u)) univ t
    D.Nonempty →
    ∃ H : ℕ → ℝ, ∃ V : ℝ,
      (∀ N ∈ D, IsLUB (range (fun t ↦ L (N,t))) (H N)) ∧
      IsLUB (L '' (D ×ˢ univ)) V ∧
      (∀ q ∈ D ×ˢ univ, IsMaxOn L (D ×ˢ univ) q ↔ L q = H q.1 ∧ H q.1 = V) ∧
      ((∃ q ∈ D ×ˢ univ, IsMaxOn L (D ×ˢ univ) q) ↔
        ∃ N ∈ D, Att N ∧ (∀ J ∈ D, ¬ Att J → H J ≤ H N)) ∧
      ((∀ N ∈ D, Att N → H N < V) →
        ∃ N ∈ D, H N = V ∧ ¬ Att N ∧
          (Tendsto (fun t ↦ L (N,t)) atTop (𝓝 V) ∨
            Tendsto (fun t ↦ L (N,-t)) atTop (𝓝 V)))
```

### `UnivariateML.Base8BNBinom.held_beta_size_maximizers_swap`

Size-beta symmetry transports the whole maximizing set between the two holds.

Atlas targets · `beta uniqueness`, `size uniqueness`.

Transfer result. The target algebraic and compact-profile arguments remain open.

[Lean source](../../lean/UnivariateML/Family/Base8BNBinom.lean#L16)

```lean
theorem held_beta_size_maximizers_swap {n : ℕ} (x : Fin n → ℕ) (b : ℝ) :
    let D := Ioi (0 : ℝ) ×ˢ Ioi 0
    {p ∈ D | IsMaxOn (fun q : ℝ × ℝ ↦ logLikelihood x q.1 q.2 b) D p} =
      {p ∈ D | IsMaxOn (fun q : ℝ × ℝ ↦ logLikelihood x b q.2 q.1) D p}
```

### `UnivariateML.Base8BNBinom.held_beta_size_zero_suprema`

Under either symmetric hold, the zero-atom path reaches the strict all-zero likelihood supremum.

Atlas targets · `beta failure`, `size failure`.

All-zero branch. Positive-count escape exhaustion remains open.

[Lean source](../../lean/UnivariateML/Family/Base8BNBinom.lean#L27)

```lean
theorem held_beta_size_zero_suprema {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∀ i, x i = 0) {b : ℝ} (hb : 0 < b) :
    let D := Ioi (0 : ℝ) ×ˢ Ioi 0
    let L := fun q : ℝ × ℝ ↦ exp (logLikelihood x q.1 q.2 b)
    let R := fun q : ℝ × ℝ ↦ exp (logLikelihood x b q.2 q.1)
    (IsLUB (L '' D) 1 ∧ ∀ p ∈ D, L p < 1) ∧
    (IsLUB (R '' D) 1 ∧ ∀ p ∈ D, R p < 1) ∧
    ∀ a > 0, Tendsto (fun r ↦ L (r,a)) (𝓝[>] 0) (𝓝 1) ∧
      Tendsto (fun r ↦ R (r,a)) (𝓝[>] 0) (𝓝 1)
```

### `UnivariateML.Base8CMP.all_free_failure`

Sharp excluded adjacent heights, attained zero and geometric cases, and compact superlevels off the adjacent edges.

Atlas target · `all free failure`.

The sharp heights and compactness classify competitive likelihood values. Pointwise law exhaustion on the degenerate branches is separate.

[Lean source](../../lean/UnivariateML/Family/Base8CMP.lean#L563)

```lean
theorem all_free_failure {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) :
    let L := fun p : ℝ × ℝ ↦ likelihood x p.1 p.2
    ((∀ i, x i = 0) → ∀ p ∈ rateDomain, IsMaxOn L rateDomain p ↔ p.1 = 0) ∧
    ((∃ i, x i ≠ 0) → ∀ c, (∀ i, x i = c ∨ x i = c+1) →
      ∃ q ∈ Icc (0 : ℝ) 1, IsMaxOn (adjacentHeight x c) (Icc 0 1) q ∧
        IsLUB (L '' rateDomain) (adjacentHeight x c q) ∧
        ∀ p ∈ rateDomain, L p < adjacentHeight x c q) ∧
    ((∃ i, x i ≠ 0) → (¬ ∃ c, ∀ i, x i = c ∨ x i = c+1) →
      ∀ v : ℝ, IsCompact {p ∈ domain | v ≤ logLik x p}) ∧
    (∃ (j : ℕ) (y : Fin j → ℕ), (∃ i, y i ≠ 0) ∧
      IsMaxOn (fun p : ℝ × ℝ ↦ likelihood y p.1 p.2) rateDomain ((1/2 : ℝ),0))
```

### `UnivariateML.Base8CMP.geometric_optimal`

The mean-matching geometric member is globally optimal when its log-factorial moment is at most the empirical value.

Atlas target · `all free failure`.

Sufficiency direction of the geometric threshold.

[Lean source](../../lean/UnivariateML/Family/Base8CMP.lean#L422)

```lean
theorem geometric_optimal {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) {r : ℝ}
    (hr : r ∈ Ioo 0 1)
    (hm : (∑ i, (x i : ℝ)) / (n : ℝ) = r/(1-r))
    (ht : (1-r) * (∑' k, r^k * logFactorial k) ≤
      (∑ i, logFactorial (x i)) / (n : ℝ)) :
    IsMaxOn (fun p : ℝ × ℝ ↦ likelihood x p.1 p.2) rateDomain (r,0)
```

### `UnivariateML.Base8CMP.all_free_uniqueness`

Every attained positive-count fit is unique, and all zeros retain arbitrary dispersion at zero rate.

Atlas target · `all free uniqueness`.

[Lean source](../../lean/UnivariateML/Family/Base8CMP.lean#L141)

```lean
theorem all_free_uniqueness {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) :
    let L := fun p : ℝ × ℝ ↦ likelihood x p.1 p.2
    ((∀ i, x i = 0) → ∀ p ∈ rateDomain, IsMaxOn L rateDomain p ↔ p.1 = 0) ∧
    ((∃ i, x i ≠ 0) → ∀ p ∈ rateDomain, IsMaxOn L rateDomain p →
      ∀ q ∈ rateDomain, IsMaxOn L rateDomain q → p = q)
```

### `UnivariateML.Base8CMP.all_free_criterion`

All zeros attain, and every other sample attains precisely when it is not supported on a single adjacent pair.

Atlas target · `all free criterion`.

[Lean source](../../lean/UnivariateML/Family/Base8CMP.lean#L371)

```lean
theorem all_free_criterion {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) :
    let L := fun p : ℝ × ℝ ↦ likelihood x p.1 p.2
    (∃ p ∈ rateDomain, IsMaxOn L rateDomain p) ↔
      (∀ i, x i = 0) ∨ ¬ ∃ c, ∀ i, x i = c ∨ x i = c+1
```

### `UnivariateML.Base8PIG.held_mean_classification`

If any count is positive, the variance-excess condition is equivalent to existence and the fitted shape is unique. Zeros have only the shape-zero maximizer.

Atlas targets · `mean criterion`, `mean uniqueness`.

Positive-data physical shapes are represented by images of the compact-profile maximizers.

[Lean source](../../lean/UnivariateML/Family/Base8PIG.lean#L19)

```lean
theorem held_mean_classification {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    {μ : ℝ} (hμ : 0 < μ) :
    let m := (∑ i, (x i : ℝ)) / (n : ℝ)
    let v := (∑ i, ((x i : ℝ) - m)^2) / (n : ℝ)
    let D := Ioc (0 : ℝ) 1
    let W := fun lam : ℝ ↦ ∃ w ∈ D, shape μ w = lam ∧
      IsMaxOn (normalizedProfile x μ) D w
    ((∀ i, x i = 0) →
      ∀ lam ∈ Ici (0 : ℝ), IsMaxOn (HeldSweepPIG.zeroLikelihood n μ) (Ici 0) lam ↔ lam = 0) ∧
    ((∃ i, 0 < x i) →
      ((∃ lam, W lam) ↔ m < v + (m - μ)^2) ∧
      (m < v + (m - μ)^2 → ∃! lam, W lam))
```

### `UnivariateML.Base8NBinom.held_probability_failure`

The infinite-size limit is zero at interior probability and one on the flat compatible probability-one ray.

Atlas target · `prob failure`.

[Lean source](../../lean/UnivariateML/Family/Base8NBinom.lean#L17)

```lean
theorem held_probability_failure {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    {p : ℝ} (hp : p ∈ Ioc 0 1) (hc : p = 1 → ∀ i, x i = 0) :
    (p = 1 →
      (∀ r ∈ Ici (0 : ℝ), likelihood x r p = 1) ∧
      Tendsto (fun r ↦ likelihood x r p) atTop (𝓝 1)) ∧
    (p < 1 →
      Tendsto (fun r ↦ likelihood x r p) atTop (𝓝 0) ∧
      ∃ r ∈ Ici (0 : ℝ), 0 < likelihood x r p ∧
        IsMaxOn (fun s ↦ likelihood x s p) (Ici 0) r)
```

### `UnivariateML.Base8NBinom.held_size_failure`

The probability-zero limit is zero at positive size and one on the flat compatible size-zero fit.

Atlas target · `size failure`.

[Lean source](../../lean/UnivariateML/Family/Base8NBinom.lean#L35)

```lean
theorem held_size_failure {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    {r : ℝ} (hr : 0 ≤ r) (hc : r = 0 → ∀ i, x i = 0) :
    (r = 0 →
      (∀ p ∈ Ioc (0 : ℝ) 1, likelihood x r p = 1) ∧
      Tendsto (likelihood x r) (𝓝[>] 0) (𝓝 1)) ∧
    (0 < r →
      Tendsto (likelihood x r) (𝓝[>] 0) (𝓝 0) ∧
      ∃ p ∈ Ioc (0 : ℝ) 1, 0 < likelihood x r p ∧
        IsMaxOn (likelihood x r) (Ioc 0 1) p)
```

## Open proof obligations and effort

Effort below describes the proof work and source comparison for each target.
The joined results use public family lemmas. The open targets have no
placeholder declarations.

| Family and hold | Target | Status and proof obligation |
| --- | --- | --- |
| bbinom, all free | criterion | **Open.** Compared the held-size dispersion theorem against the written profile rule. Prove the saturated-data branches on the unrestricted size domain, the finite underdispersed prefix and the binomial suffix maximum. Join the NB and Poisson comparisons, using algebraic finite heights and Poisson transcendence at equidispersion. |
| bbinom, all free | failure | **Open.** Checked existing fixed-size binomial, small-concentration and endpoint-mean paths. Need the simultaneous growing-size subsequence result with NB, Poisson and defective zero-atom limits, and the all-zero attainment and escape branches. |
| bbinom, all free | uniqueness | **Open.** Compared the written two-peak example with the stated global scope. Need the maximizing ridges, elimination of positive constants and endpoint-only histograms, the effective cutoffs under the stated tail conditions and the exact separated-peak comparison for (7,8,13). |
| bbinom, alpha | criterion | **Open.** Read the fixed-size existence proof and the growing-size NB comparison. Need uniqueness of each conditional beta root and a uniform exterior bound at fixed alpha to promote an ordinate at or above the NB height to an attained winner. |
| bbinom, alpha | failure | **Open.** Traced the available bounded-size paths. Need the lower and upper estimates for beta/size tending to zero and infinity, the gamma-ratio limit for a finite positive ratio, and identification of the unique fitted NB or all-zero maximizing escape. |
| bbinom, alpha | uniqueness | **Open.** Checked the integer-hold factorial cancellation and the written real-size extension. Need the rational objective identity, semialgebraic monotonicity decomposition and integer candidate recovery, the explicit binary and zero branches, and the uniform quadratic correction at NB equidispersion. |
| bbinom, alpha+size | failure | **Checked.** Joined the exposed likelihood limits to the strict supremum and flat branch. |
| bbinom, alpha+size | uniqueness | **Open.** The public theorem gives fixed-size existence. Formalize the one-crossing Laplace kernel for the score, including monotonicity of the normalized finite geometric sums and strict exponential tilting at a root. |
| bbinom, beta | criterion | **Open.** Compared the fixed-size reflected attainment result to the free-size claim. Need a positive-count uniform large-size bound and extraction of a maximizing size. |
| bbinom, beta | failure | **Open.** Established both reflected point-mass directions at a fixed size. Need the uniform large-size vanishing result on positive counts and the all-zero product estimate in terms of alpha times log size. |
| bbinom, beta | uniqueness | **Open.** Read the successive-size ratio and the lower bound on the conditional root. Need the reflected root uniqueness proof, the eventual decrease comparison for the profile and an explicit recovery of all tied sizes. |
| bbinom, beta+size | failure | **Checked.** Derived the two mixing-mean limits and applied the public count-mass result, then joined the strict sample supremum and size-zero branch. |
| bbinom, beta+size | uniqueness | **Open.** The fixed-alpha score crossing is missing. Reflect it using the public model parameterization to cover the fixed-beta root. |
| bbinom, size | criterion | **Open.** Located the variance iff for samples with an interior count. Need the mixed endpoint strict loss proof and the exact Bernoulli and size-zero joins. |
| bbinom, size | failure | **Open.** Located the strict binomial inequality at nonpositive dispersion and separate endpoint atom paths. Need the saturated mixed-endpoint bound and the joint positive-size boundary conclusion. |
| bbinom, size | uniqueness | **Open.** Compared the written algebraic and curvature arguments. Need the rational argmax formula and elimination, finiteness of stationary candidates, uniqueness through size eight and in the endpoint-rich class, and the explicit small-size ridges. |
| cmb, all free | criterion | **Open.** Compared the public fixed-size hull result with the cap predicates. Need to map every feasible size into the correct hull stratum and prove compact attainment across the capped profiles. |
| cmb, all free | failure | **Open.** Located the saturated fixed-size results and the mixed-binary maximizer. Need the cross-size upper bounds for both exceptional histograms and an adjacent-face equality proof for mixed binary data in the same capped chart. |
| cmb, all free | uniqueness | **Open.** Located the explicit constant and binary results. Need the global maximizing-set join and full-support saturation and uniqueness at sizes two and three and at rational dispersion. |
| cmb, nu | failure | **Checked.** Transported the fixed-design probability profile to integer observations and joined it to the cap maximum. |
| cmb, nu | uniqueness | **Open.** Read the algebraic mean equation and continuity crossing. Need positive-root isolation for rational holds and an exact proof of the profile-height crossing for (7,8,13). The public zero-dispersion comparison supplies the smallest-size result for nonconstant data. |
| cmb, nu+prob | uniqueness | **Checked.** Converted the finite maximum into an iff at a common height and joined the zero-dispersion singleton. |
| cmb, prob | criterion | **Checked.** Scanned every compatible size and compared each sharp profile height. Existence is equivalent to an attained profile at or above every excluded height, including equality. |
| cmb, prob | failure | **Checked.** A finite scan attains its largest profile height. If every attained profile loses, a winning excluded profile is reached by one of its signed dispersion paths. The fixed-size failure theorem identifies the sign and the law. |
| cmb, prob | uniqueness | **Checked.** Characterized every joint winner by membership in its own maximizing fiber and equality of its height to the global height, retaining flat fibers and ties. |
| cmb, prob+size | criterion | **Checked.** Centered the height at its empirical mean. The reciprocal likelihood is an increasing power of a finite exponential sum. Opposite-signed shifts make its sublevel compact, while a one-sided shift gives strict monotonicity. |
| cmb, prob+size | failure | **Checked.** Identified the height minimizers by the unit binomial coefficients and the maximizers by the consecutive ratio and symmetry. Used the exposed-mass lemma to prove both signed limits and strict suprema. |
| cmb, prob+size | uniqueness | **Checked.** One nonzero cell shift gives a strictly improving midpoint between two distinct putative minimizers. Joined the compatible boundary, Bernoulli and size-zero flat cases. |
| cmb, size | failure | **Checked.** Joined the three sharp natural-chart suprema and transported them to the closed probability chart, checking zero likelihood at both physical endpoints. |
| cmb, size | uniqueness | **Checked.** Transferred uniqueness in the natural chart, treated both endpoint constants by their saturation iff and joined the size-zero and Bernoulli fibers. |
| bnbinom, all free | criterion | **Open.** Compared the existing all-zero supremum and coordinate score theorems to the canonical comparison. Need joint escape exhaustion, the optimized face rule and the compactness argument taking stationary heights at or above the face through the seam. |
| bnbinom, all free | failure | **Open.** Located the individual zero-atom recovery paths. Need the pointwise subsequence classification for jointly varying parameters, including defective mass, and the active NB/Poisson face comparison. |
| bnbinom, alpha | uniqueness | **Open.** Located the verified (0,200) label tie and the written golden cover. Need the binary gamma-product strict diagonal minimization and increasing diagonal score factors, and formal diagonal exclusion on the golden histogram. |
| bnbinom, beta | criterion | **Open.** Proved the all-zero held result. Need beta-mixture concentration through all corners and strict derivative at the conditional score root to obtain the stationary-profile criterion, including face equality. |
| bnbinom, beta | failure | **Open.** Proved the strict all-zero supremum and a zero-atom path. Need positive-count exhaustion using joint beta-mixture limits, then identify the unique optimized NB law. |
| bnbinom, beta | uniqueness | **Open.** Proved exact transport of the argmax set under the swap. Need integer-hold rational mass reduction and quantifier elimination, compact stationary-set finiteness, and the uniform second-order comparison at NB equidispersion. |
| bnbinom, size | criterion | **Open.** The argmax transport is checked. Need the held-beta criterion and a transport of the stationary-height and face quantities. |
| bnbinom, size | failure | **Open.** Proved the all-zero branch by the swap. Need the held-beta positive-count exhaustion and the transferred optimized law. |
| bnbinom, size | uniqueness | **Open.** Proved exact transport of the entire maximizing set. Need the integer-hold and compact stationary-set work for the held-beta target and its transferred tail expansion. |
| cmp, all free | criterion | **Checked.** Used the strict empirical gap above the adjacent chord to bound dispersion, then used a cell below and one above the empirical mean to bound log rate. Maximized over a closed truncated hypograph defined by every finite normalizer bound. |
| cmp, all free | failure | **Checked for likelihood heights.** Joined the sharp adjacent height, the all-zero fiber, an explicit geometric optimum and compact superlevels off the adjacent edges. Necessity of the geometric threshold and pointwise exhaustion of maximizing sequences on degenerate data remain separate obligations if the claim is read as a law-by-law sequence classification. |
| cmp, all free | uniqueness | **Checked.** Summed the pointwise strict midpoint comparison after normalizing both cell arrays. Cells zero, one and two identify the natural parameters. Transferred the strict midpoint improvement to the rate chart and joined the all-zero fiber. |
| pig, mean | criterion | **Checked.** Extracted the iff from the existing strict Poisson exclusion and unique positive-variance-excess fit. Joined the exact zero-data maximizer. |
| pig, mean | uniqueness | **Checked.** Transported the positive-data winner via the compact coordinate's physical shape map and joined the zero-shape result. |
| nbinom, prob | failure | **Checked.** Joined the positive attained maximum and vanishing interior tail with the compatible flat endpoint, including its infinite-size limit. |
| nbinom, size | failure | **Checked.** Joined the positive attained maximum and vanishing interior limit with the compatible flat size-zero fit, including its probability-zero limit. |

## Source scope and discrepancies

- The bare phrase “endpoint-only samples do not attain” in the beta-binomial
  held-size criterion applies at size at least two. Mixed size-one samples
  attain on the Bernoulli ridge, and compatible size-zero samples attain
  everywhere. The written source gives both exceptions.
- The beta-binomial all-free proof treats unrestricted mathematical size.
  The public all-free route described in the written uniqueness report
  supports only the all-zero branch. No result here changes the public
  route or imposes that restriction on the open mathematical targets.
- The CMB source already records an odd-center support discrepancy in the
  fitter witness. At size three and held probability (3/4), a singleton
  at count two belongs to the centered pair ({1,2}) and gives limiting
  likelihood (3/4). The cited fitter witness uses ({2,3}) and (1/4).
  The fixed-size failure join in this job concerns free probability and
  does not address that implementation issue.
- For negative binomial, a maximizing sequence can run to infinite size at
  probability one on compatible data. For size zero, probability can
  approach zero with the same attained height. These are both flat
  branches. The new statements make the tied limits explicit.
- The CMB cap in the new theorems is a natural number parameter (B).
  Taking (B=10^7) gives the reported implementation domain.
- For PIG, positive-count physical winners are expressed as images of
  compact-coordinate maximizers. The public `shape_bijOn` theorem
  identifies that chart with the entire nonnegative physical shape domain.
