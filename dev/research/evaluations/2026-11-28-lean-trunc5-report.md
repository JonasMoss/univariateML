# TASK-487 · CMB and CMP truncation windows in Lean

Worktree `codex/lean-trunc5`, base `81aed46b`. This adds 20 checked theorems in seven Lean modules and 66 atlas mappings. Coverage grades are blank for independent review. The scope is known cuts, estimated cuts and zero truncation. [Atlas fragment](../existence-atlas/modifier-lean-map.d/lean-trunc5.csv) · [Independent review](2026-11-28-atlas-modcov2-report.md#open-formalization)

## Results and effort

| Target | Checked result | Approximate active proof and check time |
| --- | --- | --- |
| CMB fixed windows and design comparison | The canonical hull criterion now includes endpoint atoms in the same sample likelihood. The mixed two-cell chart gives the entire empirical-odds line. A supplied maximizing tilt on a proper exposed face gives the global slice height, with an exact attainment test for endpoint tags. At held interior odds the height at a scalar extreme uses the odds carrier restricted to the entire extreme set. A constant sample at a retained count extreme at held dispersion has height one. Finite size comparison preserves every tie and requires at least one attaining slice to report an MLE. | About 12 minutes |
| CMB positive sample `(1,2)` | The product of the two probabilities is at most one quarter. Equality forces a two-cell support and equal probabilities. This gives exactly size two and public odds `2^ν` for every cap that permits size two. | About 5 minutes |
| CMP scalar and geometric windows | The held-rate finite chart covers flat windows, lower-extreme nonattainment, the interior branch and the geometric threshold, with both limiting laws. On the positive ray the interior fit matches the mean log factorial, and the factorial moment diverges at the excluded geometric edge. In the all-free finite chart the mean-matched geometric candidate passes precisely at a nonpositive inward score, including equality. | About 11 minutes |
| CMP positive point and adjacent faces | Point and adjacent paths realize the sharp heights, and each admissible positive-rate pair is strictly below the best two-cell height. Any pointwise limit at that height puts all mass on the pair and has a maximizing split. Two observed cells separated by at least two bound both coefficients on a positive likelihood superlevel set. The excluded geometric edge loses all fixed-cell mass, giving compact superlevels, attainment and uniqueness for the remaining positive samples. | About 13 minutes |
| Estimated cuts and atlas | Both families have an exact tight-window transfer for all three cut designs. It includes unbounded known sides, singleton likelihood one and every admissible cut that removes only zero parent weight. Two distinct nominal real cuts realize the tight retained set at every attained coefficient fit. | About 6 minutes |

The times are rounded across interleaved work and exclude the aggregate build wait.

## Source correspondence

I found no mathematical discrepancy in the source branches checked. The positive CMB ray and the bounded known window `[1,2]` have different size comparisons. On the positive ray each size above two retains additional positive-mass cells at finite odds and dispersion. The known window can clip those cells, which is why the public size-two selection theorem is mapped to zero truncation. For CMP, the finite geometric edge at rate at least one has a finite conditional height. On the positive ray its normalizer diverges and its fixed-cell probabilities vanish. The finite and ray theorems carry their respective inclusion conditions. [Known-window proofs](2026-11-27-trunc-discrete-math-report.md#cmb-all-free-truncateknown) · [Positive-window proofs](2026-11-27-zero-mod-math-report.md#cmb-all-free-truncated) · [Estimated-window proofs](2026-11-28-trunc-est2-math-report.md#cmb-finite-carrier-limits)

The CMB ridge is in public probability coordinates. Other CMB windows use effective retained cells, finite log odds and endpoint tags. The tags identify endpoint laws, with dispersion irrelevant there. The held-rate CMP theorems minimize a centered normalizer or maximize its scalar log likelihood. Signed dispersion is `t = -ν`. The estimated-cut transfer uses common unnormalized parent weights and extended real nominal bounds. Holds and the CMB size cap can enter through the coefficient domain supplied to that theorem.

## Open formalization

| Reviewed scope | Checked attempt and remaining obligation |
| --- | --- |
| CMB fixed-size and free-size dispatch across all holds | The tagged chart and the exposed-face bound reduce the problem to the optimal tilt on a supplied face. Unrestricted two-cell tilts and held-odds extreme heights are checked. A complete sample-level dispatch still needs to derive the exposing set and the optimized face law directly from the observed histogram, including the lower polygon edge. It also needs to connect every fixed-size branch, including held endpoint odds and the positive-dispersion interior count fit, to the root set and height used in the size comparison. The separate named held-odds cross-size tie still needs its algebraic equality at sizes two and three and the successor inequality for every larger size. |
| CMB estimated-cut escapes | The exact cut transfer retains all ties once the tight-window coefficient fit is known. The exposed-face proof uses a supplied direction. Exhaustion for arbitrary finite-carrier sequences requires reconstructing that direction from limiting log ratios, including full-carrier limits along unidentified directions. The finite size comparison similarly assumes the slice heights have been supplied. |
| CMP held-rate charts | The finite theorem joins the chart tests and reaches both marginal laws. The atomic chart is represented by the compatibility conjunct. A combined public parameter domain should include the whole zero-rate dispersion fiber and state the interior finite-window matching equation in the same theorem. The positive-ray join takes the empirical statistic as a real input and states interior matching as a necessary equation. The all-ones equivalence, an explicit winning-height assertion for that atom, and a root-sufficiency formulation are still open in that join. |
| CMP held dispersion and all-free known windows | The geometric candidate comparison assumes the count match and identifies its exact score and attained or excluded height. The nonadjacent positive-ray compactness proof uses divergence of the excluded geometric normalizer. It cannot be used unchanged on a finite window, where that edge has finite height. A finite all-free theorem still needs the geometric count root, the inward branch, the point and mixed-pair dispatch and the zero-rate fiber in one chart. A joint held-dispersion theorem still needs the included zero-rate atom, the lower and upper endpoint laws and the excluded uniform threshold, including equality at the threshold. |
| CMP all-free positive-window moment test | The attained-versus-escape classification is checked directly by the sample range. It remains to derive the two joint moment equations or the mean-matched geometric KKT branch from that result. The sharp adjacent-height theorem uses a maximizing two-cell split, and the limit theorem classifies every pointwise limit at that height. The explicit empirical split and subsequence extraction from an arbitrary maximizing sequence are separate obligations. |
| Estimated-cut family conclusions | Tightening and the nominal-cut fibers are checked for every supplied hold domain. The resulting coefficient test is expressed through the tight-window maximizing set. Each ledger row that also specifies a family-specific root or exhaustive limit description still needs that fixed-window theorem substituted into the transfer. |

## Verification

The seven files passed foreground `lake env lean` checks on the workstation through `/tmp/umvl-cpu/run`. The foreground aggregate `lake build` with all seven temporary imports finished successfully with **8,506 jobs**. It emitted linter warnings. The run used the cached Mathlib artifacts. The temporary imports were restored after the build and `git diff --exit-code dev/lean/UnivariateML.lean` passed. The final `python3 dev/tools/existence_atlas.py --check` and `git diff --check` passed.

## Theorem statements

The scopes below identify the questions targeted by each declaration. The atlas fragment records the exact held-mask and modifier mappings.

### `UnivariateML.Trunc5CMB.positive_one_two`

For (1,2), exactly size two wins, with public odds = 2^nu. The cap is any integer at least two. Endpoints and unsupported designs give zero likelihood. [Trunc5CMB.lean, line 106](../../lean/UnivariateML/Family/Trunc5CMB.lean#L106)

Targets `cmb` `zero:truncated` for `all free` (uniqueness).

```lean
theorem positive_one_two {cap : ℕ} (hcap : 2 ≤ cap) {N : ℕ} {p ν : ℝ} :
    ((N,p,ν) ∈ {q : ℕ × ℝ × ℝ | q.1 ≤ cap ∧ q.2.1 ∈ Icc 0 1} ∧
      IsMaxOn (fun q : ℕ × ℝ × ℝ ↦ mixedLikelihood q.1 q.2.1 q.2.2)
        {q | q.1 ≤ cap ∧ q.2.1 ∈ Icc 0 1} (N,p,ν)) ↔
      N = 2 ∧ 0 < p ∧ p < 1 ∧ p / (1-p) = (2 : ℝ)^ν
```

### `UnivariateML.Trunc5CMB.finite_size_comparison`

Compare the greatest CMB slice heights and retain all tied attaining coefficient fibers. The per-size suprema and the sets that reach them are hypotheses. [Trunc5CMB.lean, line 146](../../lean/UnivariateML/Family/Trunc5CMB.lean#L146)

Targets `cmb` `truncate:known` for `all free`, `nu`, `nu+prob`, `prob` (criterion, uniqueness). `cmb` `zero:truncated` for `all free`, `nu`, `nu+prob`, `prob` (criterion).

```lean
theorem finite_size_comparison {n : ℕ} (x : Fin n → ℕ)
    (S : Finset ℕ) (hS : S.Nonempty) (A : ℕ → Finset ℕ)
    (D roots : ℕ → Set (Sum ℝ Bool × ℝ)) (H : ℕ → ℝ)
    (hH : ∀ N ∈ S, IsLUB (sliceLikelihood x (A N) N '' D N) (H N))
    (hroot : ∀ N ∈ S, roots N = {p ∈ D N | sliceLikelihood x (A N) N p = H N}) :
    let D' := {p : ℕ × (Sum ℝ Bool × ℝ) | p.1 ∈ S ∧ p.2 ∈ D p.1}
    let L' := fun p : ℕ × (Sum ℝ Bool × ℝ) ↦ sliceLikelihood x (A p.1) p.1 p.2
    ∃ M ∈ S, IsMaxOn H (↑S : Set ℕ) M ∧ IsLUB (L' '' D') (H M) ∧
      (∀ N p, ((N,p) ∈ D' ∧ IsMaxOn L' D' (N,p)) ↔
        N ∈ S ∧ H N = H M ∧ p ∈ roots N) ∧
      ((∃ p ∈ D', IsMaxOn L' D' p) ↔ ∃ N ∈ S, H N = H M ∧ (roots N).Nonempty)
```

### `UnivariateML.Trunc5CMBChart.fixed_size_maxima`

The fixed-size canonical hull criterion includes the true conditional endpoint atoms. The sample is indexed by the effective retained carrier. The finite chart uses log odds. [Trunc5CMBChart.lean, line 80](../../lean/UnivariateML/Family/Trunc5CMBChart.lean#L80)

Targets `cmb` `truncate:known` for `size` (criterion, uniqueness). `cmb` `zero:truncated` for `size` (criterion).

```lean
theorem fixed_size_maxima {N n : ℕ} (hn : 0 < n) {A : Finset (Fin (N+1))}
    (x : Fin n → A) :
    let L := sampleLikelihood x
    let D := chart A
    let f := Truncation.logLik A Trunc3CMB.statistic (fun _ ↦ 1) (statisticMean x)
    let constantEndpoint := fun b ↦ endpoint N b ∈ A ∧ ∀ i, (x i).val = endpoint N b
    (∀ b, (Sum.inr b ∈ D ∧ IsMaxOn L D (.inr b)) ↔ constantEndpoint b) ∧
    (∀ θ, IsMaxOn L D (.inl θ) ↔ IsMaxOn f univ θ ∧
      (∀ b, constantEndpoint b → ∀ k ∈ A, k = endpoint N b)) ∧
    ((∃ p ∈ D, IsMaxOn L D p) ↔ (∃ b, constantEndpoint b) ∨
      statisticMean x ∈ intrinsicInterior ℝ
        (convexHull ℝ (Trunc3CMB.statistic '' (A : Set (Fin (N+1))))))
```

### `UnivariateML.Trunc5CMBChart.fixed_size_face_height`

The optimized proper exposed face gives the exact slice height, with its endpoint attainment test. The exposing direction, support of the sample and a face maximizing tilt are hypotheses. [Trunc5CMBChart.lean, line 178](../../lean/UnivariateML/Family/Trunc5CMBChart.lean#L178)

Targets `cmb` `truncate:known` for `size` (criterion, failure). `cmb` `zero:truncated` for `size` (criterion, failure).

```lean
theorem fixed_size_face_height {N n : ℕ} (hn : 0 < n) {A : Finset (Fin (N+1))}
    (x : Fin n → A) (F : Finset A) (hx : ∀ i, x i ∈ F)
    (θ d : CMP.Plane) (c : ℝ)
    (he : ∀ j ∈ F, ⟪d, Trunc3CMB.statistic j.val⟫ = c)
    (hlt : ∀ j ∉ F, ⟪d, Trunc3CMB.statistic j.val⟫ < c) (hout : ∃ j, j ∉ F)
    (hθ : IsMaxOn (faceLikelihood F x) univ θ) :
    let H := faceLikelihood F x θ
    IsLUB (sampleLikelihood x '' chart A) H ∧
    (∀ φ, sampleLikelihood x (.inl φ) < H) ∧
    (∀ b, Sum.inr b ∈ chart A →
      (sampleLikelihood x (.inr b) = H ↔ ∀ i, (x i).val = endpoint N b))
```

### `UnivariateML.Trunc5CMBChart.fixed_size_mixed_pair`

On a mixed two-cell carrier the maximizing set is the empirical-odds line. The empirical statistic is given as a positive convex combination of the two statistics. [Trunc5CMBChart.lean, line 316](../../lean/UnivariateML/Family/Trunc5CMBChart.lean#L316)

Targets `cmb` `truncate:known` for `size` (criterion, uniqueness). `cmb` `zero:truncated` for `size` (criterion).

```lean
theorem fixed_size_mixed_pair {N n : ℕ} (hn : 0 < n)
    {l u : Fin (N+1)} (hne : l ≠ u) (x : Fin n → ({l,u} : Finset (Fin (N+1))))
    (hl : ∃ i, (x i).val = l) (hu : ∃ i, (x i).val = u)
    {a b : ℝ} (ha : 0 < a) (hb : 0 < b) (hab : a+b = 1)
    (hm : statisticMean x = a • Trunc3CMB.statistic l + b • Trunc3CMB.statistic u) :
    ∀ p ∈ chart ({l,u} : Finset (Fin (N+1))), IsMaxOn (sampleLikelihood x) (chart {l,u}) p ↔
      ∃ θ, p = .inl θ ∧
        ⟪θ, Trunc3CMB.statistic u⟫ - ⟪θ, Trunc3CMB.statistic l⟫ = log (b/a)
```

### `UnivariateML.Trunc5CMBOdds.held_interior_odds`

The fixed interior-odds dispersion fit is flat, identified, or an excluded extreme-set likelihood. The minimum and maximum cells are supplied. The held odds are finite and strictly positive. [Trunc5CMBOdds.lean, line 71](../../lean/UnivariateML/Family/Trunc5CMBOdds.lean#L71)

Targets `cmb` `truncate:known` for `prob+size` (criterion, failure, uniqueness). `cmb` `zero:truncated` for `prob+size` (criterion, failure).

```lean
theorem held_interior_odds {N n : ℕ} (hn : 0 < n) {A : Finset (Fin (N+1))}
    (x : Fin n → A) (η : ℝ) (lo hi : A)
    (hb : ∀ k : A, Discrete3CMB.height N k.val ∈
      Icc (Discrete3CMB.height N lo.val) (Discrete3CMB.height N hi.val)) :
    let T := fun k : A ↦ Discrete3CMB.height N k.val
    let m := (∑ i, T (x i)) / n
    let L := oddsLikelihood x η
    (T lo = T hi → ∀ ν, IsMaxOn L univ ν) ∧
    (T lo < T hi → ((∃! ν, IsMaxOn L univ ν) ↔ m ∈ Ioo (T lo) (T hi))) ∧
    (T lo < T hi → ∀ sign : ℝ, sign = 1 ∨ sign = -1 →
      let c := if sign = 1 then T hi else T lo
      let F := Finset.univ.filter (fun k : A ↦ T k = c)
      (∀ i, T (x i) = c) →
      let H := ∏ i, exp (η*(x i).val.val) / ∑ k ∈ F, exp (η*k.val.val)
      IsLUB (range L) H ∧ ∀ ν, L ν < H)
```

### `UnivariateML.Trunc5CMBOdds.held_dispersion_extreme`

A constant sample at a retained count extreme has height one and attains only at the true endpoint. The retained window has at least two cells. Limits use signed log odds. [Trunc5CMBOdds.lean, line 150](../../lean/UnivariateML/Family/Trunc5CMBOdds.lean#L150)

Targets `cmb` `truncate:known` for `nu+size` (criterion, failure, uniqueness). `cmb` `zero:truncated` for `nu+size` (criterion, failure).

```lean
theorem held_dispersion_extreme {N l u n : ℕ} (hlu : l < u) (_huN : u ≤ N) (hn : 0 < n)
    (ν : ℝ) (upper : Bool) :
    let k := if upper then u else l
    let h := fun j : ℕ ↦ exp (ν * log (N.choose j : ℝ))
    let f := TruncCount.finiteCountMass l u h k
    let L := fun p : Sum ℝ Bool ↦ match p with
      | .inl t => f t ^ n
      | .inr b => if b = upper then 1 else 0
    let D := TruncCount.finiteCountDomain (decide (l = 0)) (decide (u = N))
    Tendsto f (if upper then atTop else atBot) (𝓝 1) ∧
    IsLUB (L '' D) 1 ∧
    (∀ p ∈ D, IsMaxOn L D p ↔ p = .inr upper) ∧
    ((∃ p ∈ D, IsMaxOn L D p) ↔ (if upper then u = N else l = 0))
```

### `UnivariateML.Trunc5CMP.held_rate_finite_window`

At held rate, the finite chart joins the zero-rate compatibility test, flat window, interior and geometric branches. The finite ordinary window and empirical sample are explicit. The atomic compatibility test is a separate conjunct. [Trunc5CMP.lean, line 111](../../lean/UnivariateML/Family/Trunc5CMP.lean#L111)

Targets `cmp` `truncate:known` for `lambda` (criterion, failure, uniqueness).

```lean
theorem held_rate_finite_window {l u n : ℕ} (hlu : l ≤ u) (hn : 0 < n)
    (x : Fin n → ℕ) (hx : ∀ i, x i ∈ Finset.Icc l u) (η : ℝ) :
    let A := Finset.Icc l u
    let T := CMP.logFactorial
    let h := Trunc3CMP.rateCarrier η
    let m := (∑ i, T (x i)) / n
    let g := Truncation.carrierMean A T h
    let L := Truncation.logLik A T h m
    let D := Trunc3CMPRay.domain η
    ((0 < ∑ k ∈ A, (if k = 0 then (1 : ℝ) else 0)) ∧
      (∀ i, 0 < (if x i = 0 then (1 : ℝ) else 0)) ↔ l = 0 ∧ ∀ i, x i = 0) ∧
    (T l = T u → ∀ t ∈ D, IsMaxOn L D t) ∧
    (T l < T u →
      ((∃! t, t ∈ D ∧ IsMaxOn L D t) ↔ T l < m ∧ (m < g ∨ η < 0)) ∧
      (m = T l → ∀ t ∈ D, ¬ IsMaxOn L D t) ∧
      (m < g → ∀ t ∈ D, IsMaxOn L D t → t < 0) ∧
      (g ≤ m → ∀ t ∈ D, IsMaxOn L D t ↔ t = 0 ∧ η < 0)) ∧
    (∀ j, Tendsto (fun t : ℝ ↦ h j * exp (t*T j) /
      ∑ k ∈ A, h k * exp (t*T k)) (𝓝[<] 0) (𝓝 (h j / ∑ k ∈ A, h k))) ∧
    (∀ k ∈ A, T k = T l ↔ k = l ∨ (l = 0 ∧ k = 1)) ∧
    (∀ j ∈ A, let F := A.filter (fun k ↦ T k = T l)
      Tendsto (fun t ↦ h j * exp (t*T j) / ∑ k ∈ A, h k * exp (t*T k))
        atBot (𝓝 ((if j ∈ F then h j else 0) / ∑ k ∈ F, h k)))
```

### `UnivariateML.Trunc5CMP.finite_geometric_comparison`

On at least three cells the mean-matched geometric chart wins exactly at the nonpositive inward score. The geometric match is a hypothesis. Best height and strictness are stated through the centered partition. [Trunc5CMP.lean, line 227](../../lean/UnivariateML/Family/Trunc5CMP.lean#L227)

Targets `cmp` `truncate:known` for `all free` (criterion, failure, uniqueness).

```lean
theorem finite_geometric_comparison {l u : ℕ} (hlu : l+2 ≤ u) (m f η : ℝ)
    (hmean : ∑ k ∈ Finset.Icc l u, exp (η*((k:ℝ)-m)) * ((k:ℝ)-m) = 0) :
    let Z := finiteJointPartition l u m f
    let S := ∑ k ∈ Finset.Icc l u, exp (η*((k:ℝ)-m)) * (CMP.logFactorial k-f)
    let D := {p : ℝ × ℝ | p.2 < 0 ∨ p.2 = 0 ∧ p.1 < 0}
    (IsMinOn Z {p | p.2 ≤ 0} (η,0) ↔ S ≤ 0) ∧
    (S ≤ 0 → (∀ q : ℝ × ℝ, q.2 ≤ 0 → q ≠ (η,0) → Z (η,0) < Z q) ∧
      IsGLB (Z '' D) (Z (η,0)) ∧
      (∀ q ∈ D, IsMinOn Z D q ↔ q = (η,0) ∧ η < 0))
```

### `UnivariateML.Trunc5CMPFaces.positive_adjacent_supremum`

An adjacent-supported positive sample has the strict best two-cell likelihood as its supremum. The pair starts at a positive count. Constant samples are included. [Trunc5CMPFaces.lean, line 112](../../lean/UnivariateML/Family/Trunc5CMPFaces.lean#L112)

Targets `cmp` `zero:truncated` for `all free` (failure).

```lean
theorem positive_adjacent_supremum {n c : ℕ} (hn : 0 < n) (hc : 0 < c)
    (x : Fin n → ℕ) (hx : ∀ i, x i = c ∨ x i = c+1) :
    ∃ q ∈ Icc (0 : ℝ) 1, IsMaxOn (Discrete5CMP.adjacentHeight x c) (Icc 0 1) q ∧
      IsLUB (positiveLikelihood x '' CMP.domain) (Discrete5CMP.adjacentHeight x c q) ∧
      ∀ p ∈ CMP.domain, positiveLikelihood x p < Discrete5CMP.adjacentHeight x c q
```

### `UnivariateML.Trunc5CMPFaces.positive_point_path`

Every positive point mass is reached by an explicit admissible all-free path. This path statement is cellwise at positive counts. [Trunc5CMPFaces.lean, line 184](../../lean/UnivariateML/Family/Trunc5CMPFaces.lean#L184)

Targets `cmp` `zero:truncated` for `all free` (failure).

```lean
theorem positive_point_path (c : ℕ) (hc : 0 < c) (k : ℕ) :
    let s := (log (c : ℝ)+log ((c : ℝ)+1))/2
    Tendsto (fun t : ℝ ↦ positiveMass (t*s,1+t) k) atTop
      (𝓝 (if k = c then 1 else 0))
```

### `UnivariateML.Trunc5CMPFaces.positive_nonadjacent_attainment`

A gap of at least two between observed cells makes every positive superlevel set compact. This states attainment and rules out all competitive escapes on nonadjacent positive samples. [Trunc5CMPFaces.lean, line 364](../../lean/UnivariateML/Family/Trunc5CMPFaces.lean#L364)

Targets `cmp` `zero:truncated` for `all free` (criterion, failure).

```lean
theorem positive_nonadjacent_attainment {n : ℕ} (x : Fin n → ℕ) (hx : ∀ i, 0 < x i)
    (i j : Fin n) (hij : x i+1 < x j) :
    (∀ ε > 0, IsCompact {p ∈ CMP.domain | ε ≤ positiveLikelihood x p}) ∧
    ∃ p ∈ CMP.domain, IsMaxOn (positiveLikelihood x) CMP.domain p
```

### `UnivariateML.Trunc5CMPFaces.positive_window_unique`

Every finite all-free positive-window maximizer is unique. The admissible geometric edge is included in the natural-coordinate domain. [Trunc5CMPFaces.lean, line 509](../../lean/UnivariateML/Family/Trunc5CMPFaces.lean#L509)

Targets `cmp` `zero:truncated` for `all free` (uniqueness).

```lean
theorem positive_window_unique {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    {p q : ℝ × ℝ} (hp : p ∈ CMP.domain) (hq : q ∈ CMP.domain)
    (hP : IsMaxOn (positiveLikelihood x) CMP.domain p)
    (hQ : IsMaxOn (positiveLikelihood x) CMP.domain q) : p = q
```

### `UnivariateML.Trunc5CMPFaces.positive_window_attainment`

Positive sample range at least two is equivalent to attainment and uniqueness. All other samples have the excluded point or adjacent height. This criterion uses observed range. The joint interior moment and mean-matched geometric equations are open. [Trunc5CMPFaces.lean, line 534](../../lean/UnivariateML/Family/Trunc5CMPFaces.lean#L534)

Targets `cmp` `zero:truncated` for `all free` (criterion, failure, uniqueness).

```lean
theorem positive_window_attainment {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∀ i, 0 < x i) :
    ((∃ p ∈ CMP.domain, IsMaxOn (positiveLikelihood x) CMP.domain p) ↔
      ∃ i j, x i+1 < x j) ∧
    ((∃! p, p ∈ CMP.domain ∧ IsMaxOn (positiveLikelihood x) CMP.domain p) ↔
      ∃ i j, x i+1 < x j) ∧
    (∀ c > 0, (∀ i, x i = c ∨ x i = c+1) →
      ∃ q ∈ Icc (0 : ℝ) 1, IsMaxOn (Discrete5CMP.adjacentHeight x c) (Icc 0 1) q ∧
        IsLUB (positiveLikelihood x '' CMP.domain) (Discrete5CMP.adjacentHeight x c q) ∧
        ∀ p ∈ CMP.domain, positiveLikelihood x p < Discrete5CMP.adjacentHeight x c q)
```

### `UnivariateML.Trunc5CMPFaces.positive_adjacent_winning_limit`

Every pointwise limit at the best adjacent-supported likelihood is the maximizing point or adjacent law. The conclusion concerns any pointwise limit along an admissible path at the supremum. [Trunc5CMPFaces.lean, line 597](../../lean/UnivariateML/Family/Trunc5CMPFaces.lean#L597)

Targets `cmp` `zero:truncated` for `all free` (failure).

```lean
theorem positive_adjacent_winning_limit {ι : Type*} {F : Filter ι} [NeBot F]
    {n c : ℕ} (hn : 0 < n) (hc : 0 < c) (x : Fin n → ℕ)
    (hx : ∀ i, x i = c ∨ x i = c+1) {p : ι → ℝ × ℝ}
    (hp : ∀ᶠ t in F, p t ∈ CMP.domain) {g : ℕ → ℝ}
    (hg : ∀ k > 0, Tendsto (fun t ↦ positiveMass (p t) k) F (𝓝 (g k)))
    (hwin : Tendsto (fun t ↦ positiveLikelihood x (p t)) F
      (𝓝 (sSup (positiveLikelihood x '' CMP.domain)))) :
    g c = 1-g (c+1) ∧ g (c+1) ∈ Icc (0 : ℝ) 1 ∧
    IsMaxOn (Discrete5CMP.adjacentHeight x c) (Icc 0 1) (g (c+1)) ∧
    ∀ k > 0, k ≠ c → k ≠ c+1 → g k = 0
```

### `UnivariateML.Trunc5CMPRay.held_rate_interior_match`

A positive-dispersion optimum at held positive rate matches the factorial statistic. Interior necessity in the centered-partition chart. [Trunc5CMPRay.lean, line 102](../../lean/UnivariateML/Family/Trunc5CMPRay.lean#L102)

Targets `cmp` `zero:truncated` for `lambda` (criterion).

```lean
theorem held_rate_interior_match (η m : ℝ) {t : ℝ} (ht : t < 0)
    (H : IsMinOn (centered η m) (domain η) t) :
    factorialMean η t = m
```

### `UnivariateML.Trunc5CMPRay.held_rate_excluded_factorial_mean`

The factorial moment diverges at the excluded zero-dispersion edge at held rate at least one. This is a boundary barrier in the scalar fitting criterion. [Trunc5CMPRay.lean, line 128](../../lean/UnivariateML/Family/Trunc5CMPRay.lean#L128)

Targets `cmp` `zero:truncated` for `lambda` (criterion).

```lean
theorem held_rate_excluded_factorial_mean {η : ℝ} (hη : 0 ≤ η) :
    Tendsto (factorialMean η) (𝓝[<] 0) atTop
```

### `UnivariateML.Trunc5CMPRay.held_rate_positive_window`

The held-rate positive ray joins the interior moment result, geometric score, and excluded point law. The empirical mean log factorial is supplied as a real number. The centered normalizer is minimized. [Trunc5CMPRay.lean, line 190](../../lean/UnivariateML/Family/Trunc5CMPRay.lean#L190)

Targets `cmp` `zero:truncated` for `lambda` (criterion, failure).

```lean
theorem held_rate_positive_window (η : ℝ) {m : ℝ} (hm : 0 ≤ m) :
    let D := domain η
    let L := centered η m
    ((∃! t, t ∈ D ∧ IsMinOn L D t) ↔ 0 < m) ∧
    (∀ t < 0, IsMinOn L D t → factorialMean η t = m) ∧
    (η < 0 → (IsMinOn L D 0 ↔
      (∑' k : ℕ, exp (η*(k+1 : ℕ)) * (CMP.logFactorial (k+1)-m)) ≤ 0)) ∧
    (m = 0 → ¬ ∃ t ∈ D, IsMinOn L D t) ∧
    (∀ j, Tendsto (fun t ↦ CMP.weight η t (j+1) / ∑' k : ℕ, CMP.weight η t (k+1))
      atTop (𝓝 (if j = 0 then 1 else 0))) ∧
    (0 ≤ η → Tendsto (factorialMean η) (𝓝[<] 0) atTop)
```

### `UnivariateML.Trunc5Estimated.cmb_estimated_windows`

Tight nominal cuts give the fixed-window criterion and every compatible coefficient and cut tie. Fixed-window coefficient optimization is expressed through its maximizing set. Holds and size cap enter through D. [Trunc5Estimated.lean, line 206](../../lean/UnivariateML/Family/Trunc5Estimated.lean#L206)

Targets `cmb` `truncate:lower-estimated` for `all free`, `nu`, `nu+prob`, `nu+prob+size`, `nu+size`, `prob`, `prob+size`, `size` (criterion, uniqueness). `cmb` `truncate:upper-estimated` for `all free`, `nu`, `nu+prob`, `nu+prob+size`, `nu+size`, `prob`, `prob+size`, `size` (criterion, uniqueness). `cmb` `truncate:both-estimated` for `all free`, `nu`, `nu+prob`, `nu+prob+size`, `nu+size`, `prob`, `prob+size`, `size` (criterion, uniqueness).

```lean
theorem cmb_estimated_windows {n : ℕ} (hn : 0 < n) {x : Fin n → ℕ} {lo hi : ℕ}
    (hm : ∃ i, x i = lo) (hh : ∃ i, x i = hi) (hb : ∀ i, lo ≤ x i ∧ x i ≤ hi)
    (D : Set (ℕ × (Sum ℝ Bool × ℝ))) {d : Design} {fixed w : Window}
    (hw : allows d fixed w) (hx : ∀ i, x i ∈ cells w) :
    EstimatedClaim x lo hi cmbWeight D d fixed
```

### `UnivariateML.Trunc5Estimated.cmp_estimated_windows`

Tight nominal cuts give the fixed-window criterion and every coefficient and cut tie, including the zero-rate parent. Fixed-window coefficient optimization is expressed through its maximizing set. Holds enter through D. [Trunc5Estimated.lean, line 217](../../lean/UnivariateML/Family/Trunc5Estimated.lean#L217)

Targets `cmp` `truncate:lower-estimated` for `all free`, `lambda`, `lambda+nu`, `nu` (criterion, uniqueness). `cmp` `truncate:upper-estimated` for `all free`, `lambda`, `lambda+nu`, `nu` (criterion, uniqueness). `cmp` `truncate:both-estimated` for `all free`, `lambda`, `lambda+nu`, `nu` (criterion, uniqueness).

```lean
theorem cmp_estimated_windows {n : ℕ} (hn : 0 < n) {x : Fin n → ℕ} {lo hi : ℕ}
    (hm : ∃ i, x i = lo) (hh : ∃ i, x i = hi) (hb : ∀ i, lo ≤ x i ∧ x i ≤ hi)
    {D : Set (Option ℝ × ℝ)} (hD : D ⊆ cmpDomain) {d : Design} {fixed w : Window}
    (hw : allows d fixed w) (hx : ∀ i, x i ∈ cells w) :
    EstimatedClaim x lo hi cmpWeight D d fixed
```

