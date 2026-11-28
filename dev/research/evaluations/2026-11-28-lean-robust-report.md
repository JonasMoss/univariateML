# Huber, Bhattacharjee and EMG base claims in Lean

TASK-502 · 2026-11-28

The 17 requested atlas comparisons have checked Lean statements. The proofs include constant and displaced-constant samples, ties at support contacts, and equality of a finite optimum with an excluded face. The atlas fragment records 31 public theorems, including the bounds and transfers used by the comparisons. Its coverage cells are empty for independent review. [Atlas fragment](../existence-atlas/lean-map.d/lean-robust.csv)

## Source discrepancies

- The Huber dossier's fit note says unresolved comparisons and work exhaustion stop. Its existence prose says a finite point passing the coefficient checks can be returned uncertified when the set comparison remains unresolved. The latter agrees with the task's fallback rule. The Lean criterion compares exact likelihoods and makes no claim about whether a numerical comparison closes. [Huber dossier](../../../univariateML-cpp/include/cppunivariate/distributions/huber/README.md#L81)
- The EMG normal detector still describes an unexcluded normal face as an unresolved stop. The dossier's later prose retains a successful accuracy-checked finite result as uncertified, as the task requires. The held-location theorem uses a strict loss to the **larger** compatible face height for nonattainment. [EMG detector](../../../univariateML-cpp/include/cppunivariate/distributions/emg/README.md#L43) · [EMG fallback](../../../univariateML-cpp/include/cppunivariate/distributions/emg/README.md#L177) · [Review 6a](2026-11-27-atlas-coverage6a-report.md#source-discrepancies-and-earlier-evidence)
- The Bhattacharjee uniform value depends on which parameters can move. At a held positive width equal to half the range, the endpoint fractions penalize the supremum. At held width and centre, each exact endpoint contributes one half. When width can move, the widening path reaches the full uniform value. These are the three distinct comparisons in the written proof. [Bhattacharjee written proof](2026-11-25-bhatt-math-report.md#existence-and-the-faces)

## Atlas claims and effort

Each comparison below refers to one public theorem. A theorem can give both the attainment and failure statements for its mask. Effort estimates are approximate active proof and debugging time. Work shared by two claims in a mask is counted once.

| Family | Held | Claim | Public theorem | Proof work |
|---|---|---|---|---|
| Huber | all free | criterion | `RBHuber.all_free_comparison` | ~5 min with failure. Bound the effective scale and location uniformly, then use the conditional profile derivative at the joint winner. |
| Huber | all free | failure | `RBHuber.all_free_comparison` | Shared. Transfer the optimized normal and Laplace values and add fixed-shape constant collapse. |
| Huber | `mu` | criterion | `RBHuber.held_location_comparison` | ~3 min with failure. Restrict the same box to a singleton centre and transfer the profile to the scale fibre. |
| Huber | `mu` | failure | `RBHuber.held_location_comparison` | Shared. Separate the zero-residual collapse and compare the matched faces at RMS and mean absolute deviation. |
| Huber | `mu+sigma` | failure | `RBHuber.held_location_scale_failure` | ~1 min. Join both cutoff ends with the existing exact nonattainment result. |
| Huber | `sigma` | criterion | `RBHuber.held_scale_criterion` | ~3 min with failure. Bound large location and cutoff on competitive superlevels, then obtain stationarity. |
| Huber | `sigma` | failure | `RBHuber.held_scale_failure` | Shared. Use the normal at the mean and the strict constant-data comparison. |
| Bhattacharjee | all free | criterion | `RBBhatt.all_free_comparison` | ~4 min with failure. Prove compactness across the included normal seam and use the dimensionless-width profile. |
| Bhattacharjee | all free | failure | `RBBhatt.all_free_comparison` | Shared. Use the midrange radius and widening path, and include normal collapse at radius zero. |
| Bhattacharjee | `a` | failure | `RBBhatt.held_width_failure` | ~1 min. Join strict cover, range equality with both endpoint counts, and the width-zero branch. |
| Bhattacharjee | `a` | uniqueness | `RBBhatt.held_width_unique` | ~4 min with the held-centre uniqueness result. Prove negative joint curvature of log Gaussian interval mass and transfer the strict midpoint improvement. |
| Bhattacharjee | `a+mu` | failure | `RBBhatt.held_width_centre_failure` | ~1 min. Join the half-weight contact envelope with zero-residual collapse. |
| Bhattacharjee | `a+mu` | uniqueness | `RBBhatt.held_width_centre_unique` | Shared. The inverse-scale midpoint respects the held centre. The zero-width case uses normal scale uniqueness. |
| Bhattacharjee | `mu` | criterion | `RBBhatt.held_centre_comparison` | ~2 min with failure. Specialize the uniform box to the maximum absolute held-centre residual. |
| Bhattacharjee | `mu` | failure | `RBBhatt.held_centre_comparison` | Shared. Include zero radius and allow a positive radius for a sample constant away from the held centre. |
| EMG | `lambda` | failure | `RBEMG.held_rate_failure` | ~5 min. Bound both location tails and the small-scale region, prove the joint CDF contact limit, and join it to the compatible moving path. |
| EMG | `mu` | failure | `RBEMG.held_location_failure` | ~6 min. Control the large-rate, small-scale corner and the two strips near the compatible faces, then join finite supremum and unbounded concentration. |

The proofs use the exact mathematical comparisons in the written sources. [All-free Huber](2026-11-27-crit-open-math-report.md#huber-held-all-free) · [Held Huber](2026-11-27-lsheld-atlas-math-report.md#huber) · [Bhattacharjee](2026-11-25-bhatt-math-report.md#finite-stationary-width-criterion-proved) · [Held EMG](2026-11-27-lsheld-atlas-math-report.md#emg)

## Proof scope

- The Huber effective scale is `s / min c 1`. Its log-likelihood upper bound controls both sides of the scale interval and the location. Small cutoff uses the Laplace chart with a uniform additive error of at most `n * c^2`. Large cutoff puts all residuals into the quadratic core on a competitive superlevel. Held scale uses the Gaussian comparison at the mean.
- The Bhattacharjee width bound first restricts the width strictly below the covering radius on every level above the uniform height. One observation then stays a positive distance outside the smoothing interval, and a Gaussian tail times the other peak bounds excludes scale collapse uniformly. The Gaussian-average formula gives joint continuity through `a = 0`. The two uniqueness theorems use strict curvature only at positive held width and the normal member at width zero.
- The EMG bounds use the rate and Gaussian peaks at the outer ends. At held rate the limiting CDF value `w` contributes `w^k` at a minimum tied `k` times. Finite scaled contact gives the Gaussian CDF of that scaled displacement, and displacements tending to negative or positive infinity give weights zero or one. At held location the two moving-parameter trace lemmas preserve the matched normal and the exponential's half weights, and the joint collapse lemma excludes the remaining positive-height corner. The face heights are expressed as suprema of the compatible one-parameter traces.
- The profile criteria use `HasDerivAt` on the supremum over the conditional nuisance domain. At a joint winner, the fixed-nuisance slice and the winning height sandwich the profile increment. The criterion therefore tests a genuinely stationary finite shape, even with a set-valued conditional location. Bhattacharjee also admits the included zero-width member. The extrema indices in the Bhattacharjee and EMG statements are available on every nonempty finite sample. The statements apply with any choice of tied index.
- The compact rectangle lemmas give eventual strict loss below every level above the best face on the feasible subtype's cocompact filter. Maximizing sequences at higher heights have subsequences converging in the feasible domain. Reachable face paths establish the exact supremum when every finite point loses.

The existence statements are exact finite-parameter comparisons. Global stationary-root cardinality for the free-shape profiles, effective root isolation and exact arithmetic tests for equal heights remain open in this formalization. The written Huber root count uses semialgebraic nuisance selection and Pfaffian-closure definability. The written Bhattacharjee count uses analytic nuisance dependence through width zero and a strict tail cutoff. The almost-everywhere uniqueness claims are outside TASK-502.

## Verification

The individual Lean checks and the aggregate `lake build` passed on the workstation through the single-slot launcher. The final aggregate build included all four new modules and completed successfully with **8,642 jobs**. The temporary imports were removed and `git diff --exit-code -- dev/lean/UnivariateML.lean` exited zero. The new modules have no `sorry`, `admit`, `axiom` or `native_decide`. `python3 dev/tools/existence_atlas.py --check` exited zero.

## `UnivariateML.RBHuber`

### `held_location_scale_failure`

Small cutoff gives vanishing likelihood, and failure of attainment means that the matched normal is the exact, strictly unbeaten supremum. [Lean source](../../lean/UnivariateML/Family/RBHuber.lean#L24)

```lean
theorem held_location_scale_failure {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) (m : ℝ)
    {s : ℝ} (hs : 0 < s) :
    let L := fun c ↦ logLikelihood x c m s
    let B := Normal.logLik x m s
    Tendsto L (𝓝[>] 0) atBot ∧ Tendsto L atTop (𝓝 B) ∧
      ((¬ ∃ c ∈ Ioi (0 : ℝ), IsMaxOn L (Ioi 0) c) ↔
        (∀ c ∈ Ioi (0 : ℝ), L c < B) ∧ IsLUB (L '' Ioi 0) B)
```

### `held_scale_superlevel`

For any log likelihood level strictly above the held-scale Gaussian value at the mean, one compact rectangle contains the joint superlevel. The bounds apply to both coordinates independently of any choice of conditional location maximizer. [Lean source](../../lean/UnivariateML/Family/RBHuber.lean#L71)

```lean
theorem held_scale_superlevel {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {s J : ℝ} (hs : 0 < s) (hJ : Normal.logLik x (Normal.mean x) s < J) :
    ∃ K : Set (ℝ × ℝ), IsCompact K ∧ K ⊆ Ioi 0 ×ˢ univ ∧
      ∀ p ∈ Ioi (0 : ℝ) ×ˢ (univ : Set ℝ),
        J ≤ logLikelihood x p.1 p.2 s → p ∈ K
```

### `held_scale_criterion`

A finite conditional location optimum with zero profile derivative and height at least the held-scale normal value exists exactly when the joint likelihood attains a maximum. [Lean source](../../lean/UnivariateML/Family/RBHuber.lean#L195)

```lean
theorem held_scale_criterion {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {s : ℝ} (hs : 0 < s) :
    let D := Ioi (0 : ℝ) ×ˢ (univ : Set ℝ)
    let L := fun p : ℝ × ℝ ↦ logLikelihood x p.1 p.2 s
    (∃ p ∈ D, IsMaxOn L D p) ↔ ∃ c > 0, ∃ m : ℝ,
      IsMaxOn (fun u ↦ logLikelihood x c u s) univ m ∧
      HasDerivAt (fun t ↦ sSup ((fun u ↦ logLikelihood x t u s) '' univ)) 0 c ∧
      Normal.logLik x (Normal.mean x) s ≤ logLikelihood x c m s
```

### `held_scale_failure`

At held scale only the normal can be an unattained supremum. For constant data every finite point strictly loses to that face. [Lean source](../../lean/UnivariateML/Family/RBHuber.lean#L227)

```lean
theorem held_scale_failure {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {s : ℝ} (hs : 0 < s) :
    let D := Ioi (0 : ℝ) ×ˢ (univ : Set ℝ)
    let L := fun p : ℝ × ℝ ↦ logLikelihood x p.1 p.2 s
    let N := Normal.logLik x (Normal.mean x) s
    ((¬ ∃ p ∈ D, IsMaxOn L D p) ↔
      (∀ p ∈ D, L p < N) ∧ IsLUB (L '' D) N) ∧
    (∀ m : ℝ, (∀ j, x j = m) → ∀ p ∈ D, L p < N)
```

### `laplace_envelope`

Uniform small-cutoff comparison in the Laplace scale chart. It holds for arbitrary location, arbitrary positive scale, and all ties. [Lean source](../../lean/UnivariateML/Family/RBHuber.lean#L256)

```lean
theorem laplace_envelope {n : ℕ} (x : Fin n → ℝ) (m : ℝ)
    {c b : ℝ} (hc : 0 < c) (hb : 0 < b) :
    logLikelihood x c m (c*b) ≤ Laplace.logLik x m b + (n : ℝ)*c^2
```

### `effective_scale_bound`

Coercive upper bound in an effective scale that equals the Gaussian scale at large cutoff and the Laplace scale at small cutoff. [Lean source](../../lean/UnivariateML/Family/RBHuber.lean#L286)

```lean
theorem effective_scale_bound {n : ℕ} (x : Fin n → ℝ) (m : ℝ)
    {c s : ℝ} (hc : 0 < c) (hs : 0 < s) :
    let t := s / min c 1
    let A := min (log (sqrt (2*π))) (log 2 - 1/2)
    logLikelihood x c m s ≤
      -(n : ℝ)*log t + (n : ℝ)*(1/2-A) - (∑ j, |x j - m|)/t
```

### `free_scale_superlevel`

A common compact box contains the joint superlevel for all feasible locations, once the level is strictly above both compatible faces. The location set can be the whole line or a held singleton. [Lean source](../../lean/UnivariateML/Family/RBHuber.lean#L373)

```lean
theorem free_scale_superlevel {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {S : Set ℝ} (hS : IsClosed S) {D N E J : ℝ} (hD : 0 < D)
    (hdev : ∀ m ∈ S, D ≤ ∑ j, |x j - m|)
    (hnorm : ∀ m ∈ S, ∀ s > 0, Normal.logLik x m s ≤ N)
    (hlap : ∀ m ∈ S, ∀ b > 0, Laplace.logLik x m b ≤ E)
    (hN : N < J) (hE : E < J) :
    ∃ K : Set (ℝ × (ℝ × ℝ)), IsCompact K ∧ K ⊆ Ioi 0 ×ˢ (S ×ˢ Ioi 0) ∧
      ∀ p ∈ Ioi (0 : ℝ) ×ˢ (S ×ˢ Ioi 0),
        J ≤ logLikelihood x p.1 p.2.1 p.2.2 → p ∈ K
```

### `all_free_comparison`

The all-free Huber criterion and failure classification. On nonconstant samples it compares a conditional optimum with zero profile derivative to the two fitted faces. Constant samples collapse unboundedly. [Lean source](../../lean/UnivariateML/Family/RBHuber.lean#L574)

```lean
theorem all_free_comparison {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) :
    let A := Ioi (0 : ℝ) ×ˢ LocationScale.domain
    let L := fun p : ℝ × (ℝ × ℝ) ↦ logLikelihood x p.1 p.2.1 p.2.2
    let N := Normal.logLik x (Normal.mean x) (Normal.sd x (Normal.mean x))
    let mE := Laplace.lowerMedian x hn
    let E := Laplace.logLik x mE (Laplace.meanAbsDev x mE)
    let B := max N E
    ((∃ p ∈ A, IsMaxOn L A p) ↔
      (∃ i j, x i ≠ x j) ∧ ∃ c > 0, ∃ m s : ℝ, 0 < s ∧
        IsMaxOn (fun q : ℝ × ℝ ↦ logLikelihood x c q.1 q.2) LocationScale.domain (m,s) ∧
        HasDerivAt (fun t ↦ sSup ((fun q : ℝ × ℝ ↦ logLikelihood x t q.1 q.2) ''
          LocationScale.domain)) 0 c ∧ B ≤ logLikelihood x c m s) ∧
    ((¬ ∃ p ∈ A, IsMaxOn L A p) ↔
      ((¬ ∃ i j, x i ≠ x j) ∧ ¬ BddAbove (L '' A)) ∨
      ((∃ i j, x i ≠ x j) ∧ (∀ p ∈ A, L p < B) ∧ IsLUB (L '' A) B))
```

### `held_location_comparison`

The held-location Huber criterion and failure classification. At nonzero residuals the finite conditional scale and zero profile derivative are compared to the matched-location faces. Zero residuals collapse. [Lean source](../../lean/UnivariateML/Family/RBHuber.lean#L635)

```lean
theorem held_location_comparison {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) (m : ℝ) :
    let A := Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ)
    let L := fun p : ℝ × ℝ ↦ logLikelihood x p.1 m p.2
    let N := Normal.logLik x m (Normal.sd x m)
    let E := Laplace.logLik x m (Laplace.meanAbsDev x m)
    let B := max N E
    ((∃ p ∈ A, IsMaxOn L A p) ↔
      (∃ i, x i ≠ m) ∧ ∃ c > 0, ∃ s > 0,
        IsMaxOn (logLikelihood x c m) (Ioi 0) s ∧
        HasDerivAt (fun t ↦ sSup (logLikelihood x t m '' Ioi 0)) 0 c ∧
        B ≤ logLikelihood x c m s) ∧
    ((¬ ∃ p ∈ A, IsMaxOn L A p) ↔
      ((∀ j, x j = m) ∧ ¬ BddAbove (L '' A)) ∨
      ((∃ j, x j ≠ m) ∧ (∀ p ∈ A, L p < B) ∧ IsLUB (L '' A) B))
```

## `UnivariateML.RBBhatt`

### `held_width_centre_failure`

The held-centre problem either collapses on the normal seam, has a strictly unattained penalized uniform envelope, or has a finite maximum. [Lean source](../../lean/UnivariateML/Family/RBBhatt.lean#L24)

```lean
theorem held_width_centre_failure {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {a m : ℝ} (ha : 0 ≤ a) :
    (¬ ∃ s ∈ Ioi (0 : ℝ), IsMaxOn (likelihood x a m) (Ioi 0) s) ↔
      (a = 0 ∧ (∀ i, x i = m) ∧
        Tendsto (likelihood x a m) (𝓝[>] 0) atTop) ∨
      (0 < a ∧ (∀ i, |x i - m| ≤ a) ∧
        let u := ∏ i, (if |x i - m| = a then (1 : ℝ)/2 else 1)/(2*a)
        IsLUB (likelihood x a m '' Ioi 0) u ∧ ∀ s > 0, likelihood x a m s < u)
```

### `held_zero_width_unique`

Width zero uses precisely the normal argmax. Its attained location and scale are unique. [Lean source](../../lean/UnivariateML/Family/RBBhatt.lean#L43)

```lean
theorem held_zero_width_unique {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {p : ℝ × ℝ} (hp : p ∈ Normal.domain)
    (hm : IsMaxOn (fun q : ℝ × ℝ ↦ likelihood x 0 q.1 q.2) Normal.domain p) :
    ∃! q, q ∈ Normal.domain ∧
      IsMaxOn (fun q : ℝ × ℝ ↦ likelihood x 0 q.1 q.2) Normal.domain q
```

### `held_width_failure`

For held positive width, the exact unattained height uses the unpenalized uniform in strict coverage and endpoint fractions at equality. At width zero constant data collapse through the normal family. [Lean source](../../lean/UnivariateML/Family/RBBhatt.lean#L59)

```lean
theorem held_width_failure {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {a : ℝ} (ha : 0 ≤ a) :
    let D := Normal.domain
    let L := fun p : ℝ × ℝ ↦ likelihood x a p.1 p.2
    (¬ ∃ p ∈ D, IsMaxOn L D p) ↔
      (a = 0 ∧ ¬ BddAbove (L '' D)) ∨
      (0 < a ∧ ∃ i j : Fin n, (∀ k, x i ≤ x k) ∧ (∀ k, x k ≤ x j) ∧
        x j - x i ≤ 2*a ∧
        let z := (Finset.univ.filter (fun k ↦ x k = x i)).card
        let o := (Finset.univ.filter (fun k ↦ x k = x j)).card
        let w := (z : ℝ) / ((z : ℝ) + o)
        let u := if x j - x i = 2*a then w^z * (1-w)^o / (2*a)^n
          else (1/(2*a))^n
        IsLUB (L '' D) u ∧ ∀ p ∈ D, L p < u)
```

### `held_zero_width_centre_unique`

The attained held-centre normal scale is unique. [Lean source](../../lean/UnivariateML/Family/RBBhatt.lean#L120)

```lean
theorem held_zero_width_centre_unique {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    (m : ℝ) (h : ∃ s ∈ Ioi (0 : ℝ), IsMaxOn (likelihood x 0 m) (Ioi 0) s) :
    ∃! s, s ∈ Ioi (0 : ℝ) ∧ IsMaxOn (likelihood x 0 m) (Ioi 0) s
```

### `free_width_superlevel`

Every level strictly above the compatible uniform height has its joint superlevel in a compact box, including at the normal seam. The radius lower bound applies to all eligible centres. [Lean source](../../lean/UnivariateML/Family/RBBhatt.lean#L320)

```lean
theorem free_width_superlevel {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {S : Set ℝ} (hS : IsClosed S) {r J : ℝ} (hr : 0 < r)
    (hradius : ∀ m ∈ S, ∃ i, r ≤ |x i - m|) (hJ : (1 / (2 * r)) ^ n < J) :
    ∃ K : Set (ℝ × (ℝ × ℝ)), IsCompact K ∧ K ⊆ Ici 0 ×ˢ (S ×ˢ Ioi 0) ∧
      ∀ p ∈ Ici (0 : ℝ) ×ˢ (S ×ˢ Ioi 0),
        J ≤ likelihood x p.1 p.2.1 p.2.2 → p ∈ K
```

### `all_free_comparison`

Free centre and width compare the conditional stationary heights with the uniform covering the sample range. Constant data collapse on the included normal seam. [Lean source](../../lean/UnivariateML/Family/RBBhatt.lean#L495)

```lean
theorem all_free_comparison {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    (i j : Fin n) (hi : ∀ k, x i ≤ x k) (hj : ∀ k, x k ≤ x j) :
    let r := (x j-x i)/2
    let D := Ici (0 : ℝ) ×ˢ Normal.domain
    let L := fun p : ℝ × (ℝ × ℝ) ↦ likelihood x p.1 p.2.1 p.2.2
    let B := (1 / (2 * r)) ^ n
    ((∃ p ∈ D, IsMaxOn L D p) ↔
      0 < r ∧ ∃ a ≥ 0, ∃ m s : ℝ, 0 < s ∧ ConditionalCandidate x univ a m s ∧
        B ≤ likelihood x a m s) ∧
    ((¬ ∃ p ∈ D, IsMaxOn L D p) ↔
      (r = 0 ∧ ¬ BddAbove (L '' D)) ∨
      (0 < r ∧ (∀ p ∈ D, L p < B) ∧ IsLUB (L '' D) B))
```

### `held_centre_comparison`

At held centre, the covering radius is the largest absolute residual, including constant samples displaced from the held centre. [Lean source](../../lean/UnivariateML/Family/RBBhatt.lean#L529)

```lean
theorem held_centre_comparison {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) (m : ℝ)
    (i : Fin n) (hi : ∀ j, |x j - m| ≤ |x i - m|) :
    let r := |x i - m|
    let D := Ici (0 : ℝ) ×ˢ ({m} ×ˢ Ioi (0 : ℝ))
    let L := fun p : ℝ × (ℝ × ℝ) ↦ likelihood x p.1 p.2.1 p.2.2
    let B := (1 / (2 * r)) ^ n
    ((∃ p ∈ D, IsMaxOn L D p) ↔
      0 < r ∧ ∃ a ≥ 0, ∃ s > 0, ConditionalCandidate x {m} a m s ∧
        B ≤ likelihood x a m s) ∧
    ((¬ ∃ p ∈ D, IsMaxOn L D p) ↔
      (r = 0 ∧ ¬ BddAbove (L '' D)) ∨
      (0 < r ∧ (∀ p ∈ D, L p < B) ∧ IsLUB (L '' D) B))
```

### `held_positive_width_strict_midpoint`

At positive held physical width, a midpoint in inverse scale and scaled location strictly increases the log likelihood above the mean of the two distinct endpoint fits. [Lean source](../../lean/UnivariateML/Family/RBBhatt.lean#L697)

```lean
theorem held_positive_width_strict_midpoint {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {a : ℝ} (ha : 0 < a) {S : Set ℝ} (hS : Convex ℝ S)
    {p q : ℝ × ℝ} (hp : p ∈ S ×ˢ Ioi (0 : ℝ)) (hq : q ∈ S ×ˢ Ioi (0 : ℝ))
    (hne : p ≠ q) :
    ∃ z ∈ S ×ˢ Ioi (0 : ℝ),
      (log (likelihood x a p.1 p.2)+log (likelihood x a q.1 q.2))/2 <
        log (likelihood x a z.1 z.2)
```

### `held_width_unique`

Every attained held-width fit is unique, including the width-zero normal member and every sample with endpoint ties. [Lean source](../../lean/UnivariateML/Family/RBBhatt.lean#L785)

```lean
theorem held_width_unique {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) {a : ℝ} (ha : 0 ≤ a)
    (h : ∃ p ∈ Normal.domain,
      IsMaxOn (fun q : ℝ × ℝ ↦ likelihood x a q.1 q.2) Normal.domain p) :
    ∃! p, p ∈ Normal.domain ∧
      IsMaxOn (fun q : ℝ × ℝ ↦ likelihood x a q.1 q.2) Normal.domain p
```

### `held_width_centre_unique`

Every attained scale fit at held width and centre is unique. [Lean source](../../lean/UnivariateML/Family/RBBhatt.lean#L798)

```lean
theorem held_width_centre_unique {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {a : ℝ} (ha : 0 ≤ a) (m : ℝ)
    (h : ∃ s ∈ Ioi (0 : ℝ), IsMaxOn (likelihood x a m) (Ioi 0) s) :
    ∃! s, s ∈ Ioi (0 : ℝ) ∧ IsMaxOn (likelihood x a m) (Ioi 0) s
```

## `UnivariateML.RBEMG`

### `held_rate_contact_limit`

On approaching zero scale, a bounded limiting location contributes the exponential prefactor. An observation strictly above or below that location contributes a CDF factor of one or zero, respectively. At contact the factor is the limiting CDF evaluated at the scaled location displacement. [Lean source](../../lean/UnivariateML/Family/RBEMG.lean#L25)

```lean
theorem held_rate_contact_limit {α : Type*} {f : Filter α} {n : ℕ} (x : Fin n → ℝ)
    (r : ℝ) {m s : α → ℝ} {c w : ℝ} (hm : Tendsto m f (𝓝 c))
    (hs : Tendsto s f (𝓝[>] 0))
    (hw : Tendsto (fun t ↦ Φ ((c - m t) / s t)) f (𝓝 w)) :
    Tendsto (fun t ↦ ∏ j, density r (m t) (s t) (x j)) f
      (𝓝 (∏ j, r * exp (-r * (x j - c)) *
        (if c < x j then 1 else if x j < c then 0 else w)))
```

### `held_rate_superlevel`

At fixed positive rate all joint superlevels above the exponential face are confined to a compact location and positive-scale box. [Lean source](../../lean/UnivariateML/Family/RBEMG.lean#L146)

```lean
theorem held_rate_superlevel {n : ℕ} (x : Fin n → ℝ) {i : Fin n}
    {r J : ℝ} (hr : 0 < r) (hJ : (∏ j, r * exp (-r * (x j - x i))) < J) :
    ∃ K : Set (ℝ × ℝ), IsCompact K ∧ K ⊆ univ ×ˢ Ioi 0 ∧
      ∀ p ∈ (univ : Set ℝ) ×ˢ Ioi (0 : ℝ),
        J ≤ (∏ j, density r p.1 p.2 (x j)) → p ∈ K
```

### `held_rate_minimum_contact`

At the sample minimum the joint contact penalty is one copy of the limiting Gaussian CDF value per observation tied there. [Lean source](../../lean/UnivariateML/Family/RBEMG.lean#L249)

```lean
theorem held_rate_minimum_contact {α : Type*} {f : Filter α} {n : ℕ}
    (x : Fin n → ℝ) {i : Fin n} (hi : ∀ j, x i ≤ x j) (r : ℝ)
    {m s : α → ℝ} {w : ℝ} (hm : Tendsto m f (𝓝 (x i)))
    (hs : Tendsto s f (𝓝[>] 0))
    (hw : Tendsto (fun t ↦ Φ ((x i - m t) / s t)) f (𝓝 w)) :
    Tendsto (fun t ↦ ∏ j, density r (m t) (s t) (x j)) f
      (𝓝 ((∏ j, r * exp (-r * (x j - x i))) *
        w^((Finset.univ.filter (fun j ↦ x j = x i)).card)))
```

### `held_rate_failure`

At fixed positive rate, the scale-zero face gives the only possible unattained supremum. The joined statement includes the full moving path and the joint contact penalty for sequences approaching the minimum. [Lean source](../../lean/UnivariateML/Family/RBEMG.lean#L270)

```lean
theorem held_rate_failure {n : ℕ} (x : Fin n → ℝ) {i : Fin n}
    (hi : ∀ j, x i ≤ x j) {r : ℝ} (hr : 0 < r) :
    let D := univ ×ˢ Ioi (0 : ℝ)
    let L := fun p : ℝ × ℝ ↦ ∏ j, density r p.1 p.2 (x j)
    let B := ∏ j, r * exp (-r * (x j - x i))
    Tendsto (fun t : ℝ ↦ L (x i - t⁻¹, (t⁻¹)^2)) atTop (𝓝 B) ∧
    (∀ (m s : ℕ → ℝ) (w : ℝ), Tendsto m atTop (𝓝 (x i)) →
      Tendsto s atTop (𝓝[>] 0) → Tendsto (fun t ↦ Φ ((x i - m t) / s t)) atTop (𝓝 w) →
      Tendsto (fun t ↦ L (m t,s t)) atTop
        (𝓝 (B*w^((Finset.univ.filter (fun j ↦ x j = x i)).card)))) ∧
    ((¬ ∃ p ∈ D, IsMaxOn L D p) ↔
      (∀ p ∈ D, L p < B) ∧ IsLUB (L '' D) B)
```

### `held_location_joint_collapse`

The small-Gaussian-scale, large-rate corner has zero limiting likelihood whenever a residual at the held location is nonzero. [Lean source](../../lean/UnivariateML/Family/RBEMG.lean#L529)

```lean
theorem held_location_joint_collapse {α : Type*} {f : Filter α} {n : ℕ}
    (x : Fin n → ℝ) (m : ℝ) {i : Fin n} (hi : x i ≠ m) {r s : α → ℝ}
    (hr : Tendsto r f atTop) (hs : Tendsto s f (𝓝[>] 0)) :
    Tendsto (fun t ↦ ∏ j, density (r t) m (s t) (x j)) f (𝓝 0)
```

### `held_location_normal_limit`

The fixed-location Gaussian face remains valid for jointly moving rate and scale when the Gaussian scale has a positive limit. [Lean source](../../lean/UnivariateML/Family/RBEMG.lean#L546)

```lean
theorem held_location_normal_limit {α : Type*} {f : Filter α} {n : ℕ}
    (x : Fin n → ℝ) (m : ℝ) {r s : α → ℝ} {s₀ : ℝ} (hs₀ : 0 < s₀)
    (hr : Tendsto r f atTop) (hs : Tendsto s f (𝓝 s₀)) :
    Tendsto (fun t ↦ ∏ j, density (r t) m (s t) (x j)) f (𝓝 (normalTrace x m s₀))
```

### `held_location_exponential_limit`

The matched-location exponential trace is joint in Gaussian scale and rate. Observations at the held location retain half weight. [Lean source](../../lean/UnivariateML/Family/RBEMG.lean#L582)

```lean
theorem held_location_exponential_limit {α : Type*} {f : Filter α} {n : ℕ}
    (x : Fin n → ℝ) (m : ℝ) {r s : α → ℝ} {r₀ : ℝ}
    (hr : Tendsto r f (𝓝 r₀)) (hs : Tendsto s f (𝓝[>] 0)) :
    Tendsto (fun t ↦ ∏ j, density (r t) m (s t) (x j)) f (𝓝 (exponentialTrace x r₀ m))
```

### `held_location_superlevel`

With location held and a nonzero residual, every likelihood level above both optimized faces has a compact rate-scale superlevel. [Lean source](../../lean/UnivariateML/Family/RBEMG.lean#L830)

```lean
theorem held_location_superlevel {n : ℕ} (x : Fin n → ℝ) (m : ℝ) {i : Fin n}
    (hi : x i ≠ m) {J : ℝ}
    (hJ : max (sSup (normalTrace x m '' Ioi 0))
      (sSup ((fun r ↦ exponentialTrace x r m) '' Ioi 0)) < J) :
    ∃ K : Set (ℝ × ℝ), IsCompact K ∧ K ⊆ Ioi 0 ×ˢ Ioi 0 ∧
      ∀ p ∈ Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ),
        J ≤ (∏ j, density p.1 m p.2 (x j)) → p ∈ K
```

### `held_location_failure`

At held location failure is exactly unbounded concentration when all residuals vanish, or strict loss to the larger of the two boundary heights. [Lean source](../../lean/UnivariateML/Family/RBEMG.lean#L950)

```lean
theorem held_location_failure {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) (m : ℝ) :
    let D := Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ)
    let L := fun p : ℝ × ℝ ↦ ∏ j, density p.1 m p.2 (x j)
    let B := max (sSup (normalTrace x m '' Ioi 0))
      (sSup ((fun r ↦ exponentialTrace x r m) '' Ioi 0))
    (¬ ∃ p ∈ D, IsMaxOn L D p) ↔
      ((∀ j, x j = m) ∧
        Tendsto (fun s : ℝ ↦ L (s⁻¹,s)) (𝓝[>] 0) atTop) ∨
      ((∃ j, x j ≠ m) ∧ (∀ p ∈ D, L p < B) ∧ IsLUB (L '' D) B)
```

## `UnivariateML.RBProfile`

### `exists_isMaxOn_of_compact_superlevel`

A compact set inside the feasible domain that contains the entire superlevel of one feasible witness suffices for global attainment. [Lean source](../../lean/UnivariateML/Generic/RBProfile.lean#L20)

```lean
theorem exists_isMaxOn_of_compact_superlevel {X : Type*} [TopologicalSpace X]
    {L : X → ℝ} {D K : Set X} (hK : IsCompact K) (hsub : K ⊆ D)
    (hL : ContinuousOn L D) {p : X} (hp : p ∈ D)
    (hcut : ∀ q ∈ D, L p ≤ L q → q ∈ K) :
    ∃ q ∈ D, IsMaxOn L D q
```

### `eventually_lt_cocompact_of_compact_superlevel`

Compact superlevels give eventual strict loss below the level on the cocompact filter of the feasible subtype. [Lean source](../../lean/UnivariateML/Generic/RBProfile.lean#L37)

```lean
theorem eventually_lt_cocompact_of_compact_superlevel {X : Type*} [TopologicalSpace X]
    {L : X → ℝ} {D K : Set X} {J : ℝ} (hK : IsCompact K) (hsub : K ⊆ D)
    (hcut : ∀ q ∈ D, J ≤ L q → q ∈ K) :
    ∀ᶠ q : D in cocompact D, L q < J
```

### `profile_hasDerivAt_zero`

At a joint maximum the optimized profile is stationary if the fixed-nuisance slice is stationary. The slice and the maximum height sandwich the profile difference, even when the profile is defined by a supremum and the nuisance argmax is set-valued. [Lean source](../../lean/UnivariateML/Generic/RBProfile.lean#L53)

```lean
theorem profile_hasDerivAt_zero {X : Type*} {D : Set X} {U : Set ℝ}
    {f : ℝ → X → ℝ} {c : ℝ} {p : X} (hp : p ∈ D) (hc : U ∈ 𝓝 c)
    (hmax : ∀ t ∈ U, ∀ q ∈ D, f t q ≤ f c p)
    (hd : HasDerivAt (fun t ↦ f t p) 0 c) :
    HasDerivAt (fun t ↦ sSup (f t '' D)) 0 c
```

