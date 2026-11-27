# TASK-433 · Discrete likelihood proofs in Lean

The second pass adds 16 checked theorems in eight modules. The strongest joins give the full maximizing sets for negative hypergeometric holds `N`, `N+m`, `N+r`, `m` and `m+r` on compatible data. The hypergeometric fixed-design escape proofs include both deterministic endpoints. The affine-ray comparisons settle the polynomial sign and equality stage once the residue offsets are given. The new [atlas fragment](../existence-atlas/lean-map.d/lean-discrete5.csv) leaves coverage blank for independent review.

## Scope and source wording

- The negative-binomial held-probability entry says “No escape.” On compatible zero data at held probability one, every size has likelihood one, so maximizing sequences can diverge. The theorem keeps this ray and proves strict tail loss at every interior hold. The size-zero probability fiber is also flat on compatible data.
- The hypergeometric escape theorems classify scalar sample-likelihood limits on feasible diverging sequences. They use a shared limiting proportion along a subsequence. The existing pointwise falling-factorial results describe the associated binomial laws. The new polynomial theorem covers one specified affine ray at a time. It leaves the asymptotic bracketing that selects and exhausts winning residues open.
- The beta-binomial endpoint result uses an explicit rising-factorial mass, and the beta-negative-binomial zero-atom result uses the zero mass and exact successive-cell ratios. The corresponding round-four mass definitions are opaque to downstream modules, so the new file does not formalize a definitional bridge to those declarations. The beta-negative-binomial theorem concerns pointwise recovery on the declared coordinate paths. It does not exhaust varying-coefficient escape sequences.
- The CMP sharp-height theorem includes the geometric submodel in the comparison domain. A positive constant is an endpoint of the two-cell problem and gives height one. For mixed adjacent counts the theorem uses the maximum of the binary likelihood on the closed unit interval. Nonadjacent attainment and uniqueness remain open.
- All sample theorems with a nonempty-sample hypothesis use the atlas convention for observed samples. The fixed-cell path theorems include size zero when it is supported.

## Checked targets and proof effort

| Family | Hold | Claim | Proof effort |
| --- | --- | --- | --- |
| `nhyper` | `N, N+m, N+r, m` | uniqueness | Joined the compatibility branch, the first crossing, exact adjacent ties and comparison across stopping counts where applicable. |
| `nhyper` | `m+r` | criterion, failure, uniqueness | Joined zero-stop flatness, positive-stop crossing and tail loss on the actual feasible ray. |
| `nbinom` | `prob, size` | failure | Proved the excluded tail limits and strict loss wherever the held fiber is nonflat. |
| `hyper` | `k, m` | failure | Proved exhaustiveness and reachability for the scalar likelihood limits, including binomial endpoints. |
| `hyper` | `k, m` | criterion, uniqueness ingredients | Cleared the denominator and proved the eventual exact comparison on a specified affine ray. The full tests remain open below. |
| `bbinom` | `size` | path:3 | Proved both endpoint-mean limits with no concentration bound. |
| `bnbinom` | `all free, beta, size` | path:3 | Proved countwise limits on all three fixed-other-coordinate paths and therefore on each permitted held slice. |
| `cmb` | `nu` | path:1 | Proved both deterministic probability endpoints with the single-cell size-zero convention. |
| `cmp` | `nu` | failure | Proved zero limiting likelihood at the excluded rate end in both dispersion branches. |
| `cmp` | `all free` | limit:1, limit:2 | Computed the sharp global supremum on positive constants and adjacent-supported positive-count samples. |

## Remaining targets

The ledger groups masks when the same formal obstruction remains. “Prior Lean result” records a matching result found in the round-four review inventory. The new fragment makes no coverage assignment.

| Family | Hold | Claim | Status | Proof effort and remaining obligation |
| --- | --- | --- | --- | --- |
| `bbinom` | `all free` | criterion, failure, uniqueness | Open | The checked endpoint bounds control fixed size only. The all-free proof also needs bounded-size compactification, a growing-size classification exhausting NB and Poisson, and the signed cutoff and exact face equality tests. The multiple-peak example needs a separate rational replay in Lean. |
| `bbinom` | `alpha` | criterion, failure, uniqueness | Open | The fixed-size result supplies attainment on the regular branch. Missing are the shape-profile single-crossing proof, the optimized fixed-shape NB recovery and closure at growing size, and integer-alpha semialgebraic decomposition with an equality test. |
| `bbinom` | `beta` | criterion, failure, uniqueness | Open | The reflected fixed-size result supplies attainment on regular slices. Need a uniform vanishing bound at growing size with beta fixed, plus proof that the retained positive slices each have one critical alpha maximizer. |
| `bbinom` | `alpha+size, beta+size` | failure, uniqueness | Open | Round four supplies fixed-alpha endpoint limits and reflected branch classifications. A joined pointwise reflected limit and the strict exponential tilt of the held-shape score remain to be checked. The positive fixed-size uniqueness argument needs monotonicity of the ratio of two exponential losses and the unique-zero transfer. |
| `bbinom` | `size` | criterion, failure, uniqueness, path:2, limit:2, case:cardinality-small-size, case:cardinality-four-to-eight | Open | The interior-data dispersion criterion is already checked. The endpoints-only strict global height comparison, flat size zero and Bernoulli shape ray still need one theorem. Cardinality requires the low-size algebraic root argument and an exact finite description of all candidates at general size. |
| `hyper` | `k, m` | criterion, uniqueness | Open | The checked polynomial stage handles positive, negative and identically equal comparison on a given affine ray. The likelihood remains to be bracketed by the two adjacent offsets in every residue class using the order-two expansion, with feasibility and prefix/witness cutoffs joined to deterministic cases. |
| `hyper` | `all free` | criterion, failure, uniqueness | Open | Read the all-free reduction against the checked mass envelope and fixed-design exhaustion. Still needed are a uniform sample bound optimized over the Poisson mean, the signed large-design corner, and the finite/slice comparison including the constant branch and finite equality. |
| `nhyper` | `all free` | criterion, failure, uniqueness | Open | The saturated constants and mixed-zero incompatibility are already checked. The positive nonconstant branch needs the positive fixed-stop residue test and a maximum over exact heights including shifted-NB suprema, restoring the full-mass factors. |
| `nhyper` | `r` | criterion, failure, uniqueness | Open | The zero-stop ray and positive-stop crossings are available. Missing are the second-order residue bracketing, nonzero comparison-polynomial proof by root multiplicities, and the exact equality test against the optimized shifted NB height. |
| `bnbinom` | `all free` | criterion, failure, path:1, path:2, limit:1, limit:2, limit:3 | Open | The three zero-atom paths are checked in the recurrence representation; round four proves strict zero-sample nonattainment. The remaining work is the full positive-count closure, optimization and NB/Poisson selection by dispersion, the canonical stationary comparison with equality, and exhaustion of joint zero-atom escapes. |
| `bnbinom` | `alpha` | uniqueness | Open | Round four proves the attainment criterion. The binary seam inequality and equality case, and rational size/beta witness versus the complete seam, are still needed for the cardinality claim. |
| `bnbinom` | `beta, size` | criterion, failure, uniqueness, path:1, limit:1, limit:3 | Open | The reflection gives both declared zero-atom recovery routes. Need the joint fixed-shape NB limit with fitted mean, exhaustive exclusion of remaining positive-count ends, and a finite-real-algebraic test on integer holds covering exact face equality. |
| `cmb` | `all free` | criterion, failure, uniqueness, path:3, path:4, limit:3, limit:4 | Open | The checked constant and binary cases and fixed-design exposed rays do not identify the occupancy of each compatible design. Need the finite moment hull, support-pattern classification, capped exact comparison and full-support saturation tests. |
| `cmb` | `nu` | uniqueness | Open | The capped fit and zero-dispersion preference are checked in round four. The rational dispersion case still needs an algebraic odds root and exact comparisons, including all tied heights and the real-hold coexistence example. |
| `cmb` | `nu+prob` | uniqueness | Prior Lean result | The finite exact argmax characterization and zero-dispersion minimal-size branch are already supplied by the round-four capped results, including the designated compatibility treatment. |
| `cmb` | `prob` | criterion, failure, uniqueness, path:2, path:3, path:4, limit:2, limit:3, limit:4 | Open | The remaining step combines each scalar-dispersion profile, including both excluded faces and the flat designs, into one finite comparison retaining attained equality and excluding strictly losing profiles. |
| `cmb` | `prob+size` | criterion, failure, uniqueness, path:2, path:3, path:4, limit:2, limit:3, limit:4 | Open | The fixed-size endpoint and even/odd center suprema are available from exposed-ray lemmas. Need the arbitrary-design log-choose extrema, interior score-root bijection, and the joined constant-statistic and boundary-probability branches. |
| `cmb` | `size` | failure, uniqueness, limit:2, limit:3, limit:4 | Open | The fixed-size adjacent, endpoints and interior-constant rays are checked separately. Need the exhaustive occupied-support to moment-hull-face identification and strict uniqueness throughout its identifiable interior, with all flat coordinates retained. |
| `cmp` | `all free` | criterion, failure, uniqueness | Open | The sharp adjacent supremum includes all positive constants. The full claim still needs attainment and uniqueness for nonadjacent data on the countably infinite support, including the geometric face, and an exhaustion theorem for maximizing escapes. |
| `cmp` | `all free` | path:1, path:2 | Prior Lean result | Round four gives the pointwise positive-atom and adjacent-cell paths. The new sharp comparison supplies their global two-cell and constant supremum heights. |
| `pig` | `mean` | criterion, uniqueness, path:1, limit:1 | Open | The variance comparison and uniqueness are checked in the compact shape chart. Need the physical-coordinate Bessel-polynomial identity for the public likelihood and the Poisson recovery on that scale. The existing cell gain is marked no_expose, so the algebra cannot directly unfold it from a downstream module. |
| `pig` | `shape, all free` | shape failure and uniqueness; all-free path:1 and limit:1 | Open | Positive held-shape branches and the zero-shape compatible ray are separate. Need the joined shape-zero case and the countwise Poisson limit and selection in the unbounded joint chart. |
| `binom` | `all free` | uniqueness | Open | The finite underdispersed argmax graph, zero axes and overdispersed nonattainment are available. Need exact integer adjacent ratio, the continuous-size monotonic branch theorem ruling out nonadjacent ties, and a joined statement with the positive constant. |
| `dgamma` | `all free` | failure, uniqueness | Open | Read the analytic-root isolation argument against the current saturation theorem. Still needed are analyticity of the profile, sign-separated score tails yielding finiteness, and the trace test with its uniform two-cell bound. |
| `skellam` | `all free` | uniqueness | Open | Constants and the both-sign mean-zero single-magnitude result are checked. Need the one-sided supporting-polynomial inequality, strict shifted convolution comparison, and transfer on both orientations including adjacent counts. |
| `hyper` | `N` | criterion, failure | Prior Lean result | The round-four feasibility and finite-grid attainment statements treat compatible data at fixed population. |
| `nhyper` | `N, N+m, N+r, m` | criterion, failure | Prior Lean result | Round four proves compatibility-grid attainment for the finite holds and uniform tail loss on positive data for held composition. The new maximizer joins record the zero and positive branches. |
| `nbinom` | `prob, size` | criterion, uniqueness | Prior Lean result | The mass-scale attainment and uniqueness results in round four provide the positive benchmark used by the new boundary proofs. |
| `bnbinom` | `alpha` | criterion | Prior Lean result | Round four proves attainment on a nonempty sample iff a positive count occurs. |
| `cmb` | `nu, nu+prob, nu+size` | criterion, failure, and nu+size uniqueness | Prior Lean result | Round four proves the attained closed probability profiles and capped comparisons. The new endpoint theorem supplies the countwise path at finite held dispersion. |
| `skellam` | `mu1, mu2` | criterion, failure, uniqueness | Prior Lean result | The closed-axis round-four held-rate theorems already join the positive and zero hold branches. |

Sources reviewed for this ledger include the [round-four target ledger](2026-11-27-lean-discrete4-report.md#target-ledger), [coverage review A](2026-11-27-atlas-coverage4a-report.md#written-claims-awaiting-full-lean-coverage), [coverage review B](2026-11-27-atlas-coverage4b-report.md#written-claims-awaiting-full-lean-coverage), [criterion proofs](2026-11-27-crit-open-math-report.md), [held-mask proofs](2026-11-27-uniq-atlas-math-report.md) and [discrete cardinality proofs](2026-11-27-uniq-discrete-math-report.md).

## Checked Lean statements

### `UnivariateML.Discrete5NHyper.held_population_composition`

Compatible zeros force stop zero. On positive draws the first crossing and its exact tie rule give the complete stopping-count maximizing set. Derived the feasible stopping interval from compatibility and joined its zero singleton to the existing positive ratio crossing. [Lean source](../../lean/UnivariateML/Family/Discrete5NHyper.lean#L21)

```lean
theorem held_population_composition {h N m : ℕ} (hh : 0 < h) (x : Fin h → ℕ)
    (hf : ∃ r, ∀ i, Compatible N m r (x i)) :
    let D := {r | ∀ i, Compatible N m r (x i)}
    ((∀ i, x i = 0) ∧ D = {0}) ∨
      ((∀ i, 0 < x i) ∧
        let l := max 1 (Finset.univ.sup x - (N-m))
        let u := min m (Finset.univ.inf' ⟨⟨0,hh⟩,Finset.mem_univ _⟩ x)
        D = Icc l u ∧
        ∃ b ∈ D, (b < u → stoppingRatio x N m b ≤ 1) ∧
          (∀ j ∈ Ico l u, j < b → 1 < stoppingRatio x N m j) ∧
          ∀ j ∈ D, IsMaxOn (fun r ↦ totalLikelihood x N m r) D j ↔
            j = b ∨ (j = b+1 ∧ b < u ∧ stoppingRatio x N m b = 1))
```

### `UnivariateML.Discrete5NHyper.held_population_stopping`

Compatible zero stop gives a flat composition interval. Positive stop gives the first crossing and exactly the tied successor. Derived the feasible composition interval and transferred the crossing to it. At zero stop the likelihood is one across the entire interval. [Lean source](../../lean/UnivariateML/Family/Discrete5NHyper.lean#L82)

```lean
theorem held_population_stopping {h N r : ℕ} (hh : 0 < h) (x : Fin h → ℕ)
    (hf : ∃ m, ∀ i, Compatible N m r (x i)) :
    let D := {m | ∀ i, Compatible N m r (x i)}
    (r = 0 ∧ (∀ i, x i = 0) ∧ D = Iic N ∧
      ∀ m ∈ D, totalLikelihood x N m r = 1) ∨
    (0 < r ∧ (∀ i, r ≤ x i) ∧
      let u := N - NegHypergeometric.maxBlackDraws (fun i ↦ x i-r)
      D = Icc r u ∧
      ∃ b ∈ D, (b < u → compositionRatio x N r b ≤ 1) ∧
        (∀ j ∈ Ico r u, j < b → 1 < compositionRatio x N r j) ∧
        ∀ j ∈ D, IsMaxOn (fun m ↦ totalLikelihood x N m r) D j ↔
          j = b ∨ (j = b+1 ∧ b < u ∧ compositionRatio x N r b = 1))
```

### `UnivariateML.Discrete5NHyper.held_population_maximizers`

Cross-slice comparisons of the first crossing and its tied successor give the full maximizing set. Compatible zeros give all compositions at stop zero. Compared only slice crossings and their tied successors, using the full mass across stopping counts. Proved both inclusions for the resulting set. [Lean source](../../lean/UnivariateML/Family/Discrete5NHyper.lean#L140)

```lean
theorem held_population_maximizers {h N : ℕ} (hh : 0 < h) (x : Fin h → ℕ)
    (hf : ∃ m r, ∀ i, Compatible N m r (x i)) :
    let D := {p : ℕ × ℕ | ∀ i, Compatible N p.1 p.2 (x i)}
    let L := fun p : ℕ × ℕ ↦ totalLikelihood x N p.1 p.2
    let U := fun r ↦ N - NegHypergeometric.maxBlackDraws (fun i ↦ x i-r)
    ((∀ i, x i = 0) ∧ {p ∈ D | IsMaxOn L D p} = {p | p.1 ≤ N ∧ p.2 = 0}) ∨
    ((∀ i, 0 < x i) ∧ ∃ b : ℕ → ℕ,
      let feasibleStop := {r | ∃ m, (m,r) ∈ D}
      let candidates := {p ∈ D | p.1 = b p.2 ∨
        (p.1 = b p.2+1 ∧ b p.2 < U p.2 ∧ compositionRatio x N p.2 (b p.2) = 1)}
      (∀ r ∈ feasibleStop, (b r,r) ∈ D ∧
        (b r < U r → compositionRatio x N r (b r) ≤ 1) ∧
        (∀ j ∈ Ico r (U r), j < b r → 1 < compositionRatio x N r j)) ∧
      {p ∈ D | IsMaxOn L D p} = {p ∈ candidates | IsMaxOn L candidates p})
```

### `UnivariateML.Discrete5NHyper.held_composition_stopping`

At positive stop the first crossing gives one or two winners and the population tail vanishes. Stop zero is flat. Joined the feasible black-count ray, its first-crossing rule and its vanishing population tail to the flat zero-stop branch. [Lean source](../../lean/UnivariateML/Family/Discrete5NHyper.lean#L227)

```lean
theorem held_composition_stopping {h m r : ℕ} (hh : 0 < h) (x : Fin h → ℕ)
    (hf : ∃ n, ∀ i, Compatible (m+n) m r (x i)) :
    let D := {n | ∀ i, Compatible (m+n) m r (x i)}
    let L := fun n ↦ totalLikelihood x (m+n) m r
    (r = 0 ∧ (∀ i, x i = 0) ∧ D = univ ∧ ∀ n, L n = 1) ∨
    (0 < r ∧
      let y := fun i ↦ x i-r
      D = Ici (NegHypergeometric.maxBlackDraws y) ∧
      Tendsto L atTop (𝓝 0) ∧
      ∃ b ∈ D, NegHypergeometric.whiteRatio r m (m+b+1) y ≤ 1 ∧
        (∀ j ∈ D, j < b → 1 < NegHypergeometric.whiteRatio r m (m+j+1) y) ∧
        ∀ j, (IsMaxOn L D j ∧ j ∈ D) ↔ j = b ∨
          (j = b+1 ∧ NegHypergeometric.whiteRatio r m (m+b+1) y = 1))
```

### `UnivariateML.Discrete5NBinom.held_size_boundary`

At positive size the probability-zero limit loses strictly to an attained fit. At size zero the compatible likelihood is flat. Factored the likelihood into a finite rising product and two real powers. The positive power of probability yields zero at the excluded endpoint. [Lean source](../../lean/UnivariateML/Family/Discrete5NBinom.lean#L62)

```lean
theorem held_size_boundary {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) {r : ℝ}
    (hr : 0 ≤ r) (hc : r = 0 → ∀ i, x i = 0) :
    (r = 0 ∧ ∀ p ∈ Ioc (0 : ℝ) 1, likelihood x r p = 1) ∨
      (0 < r ∧ Tendsto (likelihood x r) (𝓝[>] 0) (𝓝 0) ∧
        ∃ p ∈ Ioc (0 : ℝ) 1, 0 < likelihood x r p ∧
          IsMaxOn (likelihood x r) (Ioc 0 1) p)
```

### `UnivariateML.Discrete5NBinom.held_probability_boundary`

At interior probability the infinite-size limit loses strictly to an attained fit. At probability one the compatible size ray is flat. Bound the sample likelihood by the checked limit of a polynomial times exponential decay. Positive likelihood at the attained fit gives strict tail loss. [Lean source](../../lean/UnivariateML/Family/Discrete5NBinom.lean#L117)

```lean
theorem held_probability_boundary {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    {p : ℝ} (hp : p ∈ Ioc 0 1) (hc : p = 1 → ∀ i, x i = 0) :
    (p = 1 ∧ ∀ r ∈ Ici (0 : ℝ), likelihood x r p = 1) ∨
      (p < 1 ∧ Tendsto (fun r ↦ likelihood x r p) atTop (𝓝 0) ∧
        ∃ r ∈ Ici (0 : ℝ), 0 < likelihood x r p ∧
          IsMaxOn (fun s ↦ likelihood x s p) (Ici 0) r)
```

### `UnivariateML.Discrete5Hyper.held_draw_escape_iff`

For fixed draws the convergent likelihoods of compatible diverging urns are exactly the closed binomial heights. Extracted a convergent marked-fraction subsequence in the closed unit interval and used both the falling-factorial limit and the feasible recovery path. [Lean source](../../lean/UnivariateML/Family/Discrete5Hyper.lean#L31)

```lean
theorem held_draw_escape_iff {h : ℕ} (hh : 0 < h) (x : Fin h → ℕ) (k : ℕ)
    (hx : ∀ i, x i ≤ k) (L : ℝ) :
    (∃ A N : ℕ → ℕ, (∀ t, A t ≤ N t) ∧
      (∀ t i, Compatible (A t) (N t) k (x i)) ∧ Tendsto N atTop atTop ∧
      Tendsto (fun t ↦ likelihood (A t) (N t) k x) atTop (𝓝 L)) ↔
    ∃ p ∈ Icc (0 : ℝ) 1, L = ∏ i, binomialFace k (x i) p
```

### `UnivariateML.Discrete5Hyper.held_mark_escape_iff`

For fixed marks the convergent likelihoods of compatible diverging urns are exactly the closed binomial heights. Applied the same subsequence argument to the draw fraction using the fixed-mark limit and recovery path. [Lean source](../../lean/UnivariateML/Family/Discrete5Hyper.lean#L49)

```lean
theorem held_mark_escape_iff {h : ℕ} (hh : 0 < h) (x : Fin h → ℕ) (m : ℕ)
    (hx : ∀ i, x i ≤ m) (L : ℝ) :
    (∃ A N : ℕ → ℕ, (∀ t, A t ≤ N t) ∧
      (∀ t i, Compatible m (N t) (A t) (x i)) ∧ Tendsto N atTop atTop ∧
      Tendsto (fun t ↦ likelihood m (N t) (A t) x) atTop (𝓝 L)) ↔
    ∃ p ∈ Icc (0 : ℝ) 1, L = ∏ i, binomialFace m (x i) p
```

### `UnivariateML.Discrete5BBinom.endpoint_mixing_paths`

The count masses converge to either corresponding atom as the mixing mean approaches the endpoint, uniformly over the total concentration. Bound positive cell masses by the mixing mean times a count-dependent constant. Squeezed the lower endpoint mass between the complementary mean to the size power and one, then reflected. [Lean source](../../lean/UnivariateML/Family/Discrete5BBinom.lean#L125)

```lean
theorem endpoint_mixing_paths {ι : Type*} {l : Filter ι} {a b : ι → ℝ}
    (hpos : ∀ᶠ t in l, 0 < a t ∧ 0 < b t) {m k : ℕ} (hk : k ≤ m) :
    (Tendsto (fun t ↦ a t/(a t+b t)) l (𝓝 0) →
      Tendsto (fun t ↦ countMass m k (a t) (b t)) l (𝓝 (if k = 0 then 1 else 0))) ∧
    (Tendsto (fun t ↦ a t/(a t+b t)) l (𝓝 1) →
      Tendsto (fun t ↦ countMass m k (a t) (b t)) l (𝓝 (if k = m then 1 else 0)))
```

### `UnivariateML.Discrete5BNBinom.zero_atom_count_paths`

With the others fixed, shrinking size or beta or increasing alpha gives pointwise convergence to the zero atom. Multiplied the existing zero-mass limits by the finite successive-count ratios. Every positive cell contains the vanishing first ratio. [Lean source](../../lean/UnivariateML/Family/Discrete5BNBinom.lean#L43)

```lean
theorem zero_atom_count_paths (k : ℕ) {r a b : ℝ} (hr : 0 < r) (ha : 0 < a) (hb : 0 < b) :
    Tendsto (fun t ↦ countMass k t a b) (𝓝[>] 0) (𝓝 (if k = 0 then 1 else 0)) ∧
    Tendsto (fun t ↦ countMass k r a t) (𝓝[>] 0) (𝓝 (if k = 0 then 1 else 0)) ∧
    Tendsto (fun t ↦ countMass k r t b) atTop (𝓝 (if k = 0 then 1 else 0))
```

### `UnivariateML.Discrete5CMB.held_dispersion_probability_endpoints`

At finite held dispersion, probability approaching either endpoint gives the corresponding atom on every cell, including size zero. Rescaled the finite odds weights by the same power of one minus probability. The resulting polynomial weights are continuous at both endpoints and their normalizers are one there. [Lean source](../../lean/UnivariateML/Family/Discrete5CMB.lean#L90)

```lean
theorem held_dispersion_probability_endpoints {N : ℕ} (ν : ℝ) (j : Fin (N+1)) :
    Tendsto (fun p ↦ closedLikelihood (fun _ : Fin 1 ↦ j) p ν)
      (𝓝[Icc (0 : ℝ) 1] 0) (𝓝 (if j = 0 then 1 else 0)) ∧
    Tendsto (fun p ↦ closedLikelihood (fun _ : Fin 1 ↦ j) p ν)
      (𝓝[Icc (0 : ℝ) 1] 1) (𝓝 (if j = Fin.last N then 1 else 0))
```

### `UnivariateML.Discrete5CMP.held_dispersion_tail`

The excluded rate end has limiting likelihood zero, at infinity with positive dispersion and at one with zero dispersion. For positive dispersion, compared each cell weight with the next weight to obtain an inverse-rate bound. At zero dispersion used the geometric normalizer and then took the finite sample product. [Lean source](../../lean/UnivariateML/Family/Discrete5CMP.lean#L64)

```lean
theorem held_dispersion_tail {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    {ν : ℝ} (hν : 0 ≤ ν) :
    if ν = 0 then Tendsto (fun r ↦ likelihood x r ν) (𝓝[<] 1) (𝓝 0)
    else Tendsto (fun r ↦ likelihood x r ν) atTop (𝓝 0)
```

### `UnivariateML.Discrete5CMP.adjacent_sharp_supremum`

On nonzero adjacent-supported samples, the maximized two-cell likelihood is the exact unattained supremum, including positive constants. Conditioned on the two candidate cells for the upper bound, recovered each interior binary proportion along an exposed ray, and extended the height comparison to closed binary probabilities. Used prior nonattainment for strictness. [Lean source](../../lean/UnivariateML/Family/Discrete5CMP.lean#L126)

```lean
theorem adjacent_sharp_supremum {n c : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∀ i, x i = c ∨ x i = c+1) (hx0 : ∃ i, x i ≠ 0) :
    ∃ q ∈ Icc (0 : ℝ) 1, IsMaxOn (adjacentHeight x c) (Icc 0 1) q ∧
      IsLUB ((fun p : ℝ × ℝ ↦ likelihood x p.1 p.2) '' rateDomain) (adjacentHeight x c q) ∧
      ∀ p ∈ rateDomain, likelihood x p.1 p.2 < adjacentHeight x c q
```

### `UnivariateML.Discrete5HyperResidue.affine_face_tail`

After a finite Cauchy cutoff, the exact comparison on a specified rational affine composition ray is decided by the leading sign or the zero polynomial. Cleared the positive denominator with rational falling-factorial polynomials. Used the Cauchy root bound and leading sign for the eventual comparison. [Lean source](../../lean/UnivariateML/Family/Discrete5HyperResidue.lean#L152)

```lean
theorem affine_face_tail {h : ℕ} (hh : 0 < h) (k : ℕ) (x : Fin h → ℕ)
    (a b face : ℚ) :
    ∃ (P : ℚ[X]) (B : ℕ), ∀ m N : ℕ, B ≤ N → (m : ℚ) = a*N+b →
      (∀ i, Compatible m N k (x i)) →
      (((face : ℝ) < likelihood m N k x ↔ 0 < P.leadingCoeff) ∧
       (likelihood m N k x = (face : ℝ) ↔ P = 0) ∧
       (likelihood m N k x < (face : ℝ) ↔ P.leadingCoeff < 0))
```

### `UnivariateML.Discrete5HyperResidue.affine_face_tail_held_mark`

After a finite Cauchy cutoff, the exact comparison on a specified rational affine composition ray is decided by the leading sign or the zero polynomial. Transported the polynomial comparison through the exact draw/mark symmetry and proved compatibility is unchanged by the swap. [Lean source](../../lean/UnivariateML/Family/Discrete5HyperResidue.lean#L167)

```lean
theorem affine_face_tail_held_mark {h : ℕ} (hh : 0 < h) (m : ℕ) (x : Fin h → ℕ)
    (a b face : ℚ) :
    ∃ (P : ℚ[X]) (B : ℕ), ∀ k N : ℕ, B ≤ N → (k : ℚ) = a*N+b →
      (∀ i, Compatible m N k (x i)) →
      (((face : ℝ) < likelihood m N k x ↔ 0 < P.leadingCoeff) ∧
       (likelihood m N k x = (face : ℝ) ↔ P = 0) ∧
       (likelihood m N k x < (face : ℝ) ↔ P.leadingCoeff < 0))
```

### `UnivariateML.Discrete5NHyper.held_composition_maximizers`

On positive data the full maximizing set is the exact comparison of the finite per-stop crossing candidates. On compatible zeros all zero-stop urns win. Constructed at most two candidates per positive stop, proved their union finite and compared the original likelihood on that union. [Lean source](../../lean/UnivariateML/Family/Discrete5NHyper.lean#L273)

```lean
theorem held_composition_maximizers {h m : ℕ} (hh : 0 < h) (x : Fin h → ℕ)
    (hf : ∃ n r, ∀ i, Compatible (m+n) m r (x i)) :
    let D := {p : ℕ × ℕ | ∀ i, Compatible (m+p.1) m p.2 (x i)}
    let L := fun p : ℕ × ℕ ↦ totalLikelihood x (m+p.1) m p.2
    let R := fun (r j : ℕ) ↦ NegHypergeometric.whiteRatio r m (m+j+1) (fun i ↦ x i-r)
    ((∀ i, x i = 0) ∧ {p ∈ D | IsMaxOn L D p} = {p | p.2 = 0}) ∨
    ((∀ i, 0 < x i) ∧ ∃ b : ℕ → ℕ,
      let feasibleStop := {r | ∃ n, (n,r) ∈ D}
      let candidates := {p ∈ D | p.1 = b p.2 ∨
        (p.1 = b p.2+1 ∧ R p.2 (b p.2) = 1)}
      (∀ r ∈ feasibleStop, (b r,r) ∈ D ∧ R r (b r) ≤ 1 ∧
        (∀ j, (j,r) ∈ D → j < b r → 1 < R r j)) ∧
      candidates.Finite ∧
      {p ∈ D | IsMaxOn L D p} = {p ∈ candidates | IsMaxOn L candidates p})
```


## Build

All eight new modules passed individual `lake env lean` checks on the workstation. The final workstation `lake build` ran through the single CPU slot with all eight temporary root imports and exited zero, reporting **8098 jobs**. The build reported style warnings. Mathlib was reused from the shared cache. The temporary root imports were removed and `git diff --exit-code -- dev/lean/UnivariateML.lean` passed. No C++, R or JavaScript tests were run. No commits or board edits were made.
