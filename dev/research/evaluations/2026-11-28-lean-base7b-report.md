# TASK-491 elementary-family Lean proofs, second pass

The second pass added 44 checked declarations in 10 modules. The mapping fragment leaves coverage blank for independent review. The ledger describes the proved scope and the exact obligations still open. [Atlas mapping](../existence-atlas/lean-map.d/lean-base7b.csv)

## Source qualifications

- The hypergeometric dossier metadata leaves the nonconstant all-free criterion and uniqueness unknown. The newer written ledger supplies a dispersion-split procedure. This pass uses the latter as the Lean target. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/hyper/README.md#details) · [Written criterion](2026-11-27-crit-open-math-report.md#hyper-held-all-free)
- The fixed-draw and fixed-mark proofs use a finite band and a table of integer intercepts. For an intercept `e`, the ray satisfies `d*m = a*N + e`, where `a` is the count sum and `d` is the sample length times the held design. The congruence is checked on the residues from zero through `d - 1`. The table can contain more than the two sharp offsets in the source. Its signs still give the exact prefix and ray comparison, and the zero polynomial retains every equality on that ray. The cutoff is stated existentially in Lean. The band bounds and the existing polynomial root bound give the two cutoffs used in the proof. [Written fixed-draw procedure](2026-11-27-uniq-atlas-math-report.md#hyper-held-k)
- For negative hypergeometric positive constants, a maximizing escape eventually has stopping count equal to the observation and marked fraction tending to one. Exact finite saturation requires `m = N`. The face is written as `r/t` times a binomial probability for `r` marks in `t` draws. This is the shifted negative-binomial mass when `r > 0` and `t ≥ r`. The tail comparison uses a wider offset table than the two sharp offsets in the written proof. It excludes every far-tail equality when an excess is positive and restores the full combinatorial constants before comparing slices. The strict-witness bound uses the maximum observed count, the sum of observed counts, and the rational gap above the face. [Written escape classification](2026-11-27-crit-open-math-report.md#nhyper-held-all-free) · [Written fixed-stop test](2026-11-25-nhyper-math-report.md#an-exact-finite-test-including-equality-and-escape)
- GPD shape uniqueness uses three-point comparisons of integrated secants. The positive denominator and `(t − 1)²` in the kernel difference give strictness through the exponential shape. Held-scale escape statements are in log-likelihood units, and the uniform contact theorem gives the upper envelope and a recovery path with location fixed. [Written GPD proof](2026-11-27-lsheld-atlas-math-report.md#gpd-held-musigma)
- The Skellam tail uses the radial series differential equation and vanishing atom heights. Each weighted radial flux eventually decreases. A persistent nonnegative flux would make its log potential nondecreasing, contradicting the vanishing atom bound. The analytic score extends through zero in the mean-line coordinate. The finite-candidate theorem compares exact real heights. Deciding equality of distinct critical heights for arbitrary integer histograms remains open in the source. [Written Skellam theorem](2026-11-25-skellam2-math-report.md#reflection-and-a-uniqueness-theorem)

## Target ledger

| Family | Held | Question | Atlas claim | Effort and result |
| --- | --- | --- | --- | --- |
| hyper | all free | criterion | Constants attain and on nonconstant data the exact dispersion-split grid and slice comparison attains iff its top height includes a finite co-winner, with finite Poisson equality excluded. | Proved an envelope uniform in rate and used the existing binomial profile to produce the actual underdispersion cutoff. **Open.** Combine the remaining finite grid with the held-design slice heights. At and above equidispersion, prove the uniform signed expansion on bounded rates, including the quadratic term when variance equals mean. The exact rational versus positive-mean Poisson comparison also needs the irrationality of the exponential of the negative integer count sum. [Written proof](2026-11-27-crit-open-math-report.md#hyper-held-all-free) |
| hyper | all free | failure | On nonconstant data the only positive escapes are binomial at bounded smaller design and Poisson at jointly growing designs. On constants the maximizing escaping laws are the corresponding deterministic binomial and, for zero counts, the rate-zero Poisson. | Proved a two-sided depletion squeeze and the full finite-rate Poisson product limit. Used compactification of the rate and subsequence extraction for each fixed-design orientation. The theorem includes vanishing rate on positive data, infinite rate and deterministic saturated traces on constants. [Written proof](2026-11-27-crit-open-math-report.md#hyper-held-all-free) |
| hyper | all free | uniqueness | With population unrestricted the maximizing set is computable by a rational residue and finite grid procedure including face equality. Constants have the explicit saturated sets in the report. Not unique in general: (0,1) attains at every even population with one design count one and the other half the population, in either order. | Proved exact saturation for every mixed-binary histogram using the two-cell likelihood bound and the strict deficit from a feasible third cell. Proved the full `(0, 1)` even-population example. **Open.** Assemble general winners from the dispersion-split grid and all competing fixed-design slice heights after the exterior cutoff and Poisson comparison. [Written proof](2026-11-27-uniq-discrete-math-report.md#hyper-held-all-free) |
| hyper | k | criterion | With draws held, feasible data attain iff a strict finite witness, finite prefix equality or identically equal residue ray reaches the fitted binomial height, including deterministic endpoint cases. | Replaced the adjacent-ratio expansion by two exact logarithm bounds. Used decreasing adjacent ratios to confine every tail slice winner, then cleared comparisons on finitely many affine rays. The joined theorem includes both fitted proportion endpoints and decides strict witnesses and equality by finite prefix and congruence tests. [Written proof](2026-11-27-crit-open-math-report.md#hyper-held-k) |
| hyper | k | uniqueness | Not unique in general: for x=(0,1) and k=1, every even N with m=N/2 maximizes. The maximizing set for arbitrary compatible data is computable by rational prefix and residue comparisons, with deterministic urns and equal-height rays retained. | Compared global maxima over a finite box above a strict witness, or retained the finite prefix equalities and every identically equal compatible tail ray. Joined the exact saturated constant set with both comparisons and the fixed-one even-population example. [Written proof 1](2026-11-27-uniq-atlas-math-report.md#hyper-held-k) · [Written proof 2](2026-11-27-crit-open-math-report.md#hyper-held-k) |
| hyper | m | criterion | With marked count held, the design swap gives the exact prefix, residue and equality test for attainment on every compatible sample. | Transported support compatibility and likelihood by design symmetry. The joined marked-count theorem includes endpoints and the interior finite test. [Written proof](2026-11-27-crit-open-math-report.md#hyper-held-m) |
| hyper | m | uniqueness | Not unique in general: for x=(0,1) and m=1, every even N with k=N/2 maximizes. The maximizing set is computable by the swapped rational prefix and residue procedure, including deterministic urns and equal-height rays. | Transported the full constant, finite-box and equality-ray winner clauses and the fixed-one even-population example. [Written proof 1](2026-11-27-uniq-atlas-math-report.md#hyper-held-m) · [Written proof 2](2026-11-27-crit-open-math-report.md#hyper-held-m) |
| nhyper | all free | criterion | Constants attain and mixed zeros and positives are infeasible, while positive nonconstant data attain iff some finite co-winner occurs at the largest exact fixed-stopping-count slice height. | Proved that each fixed-stop slice height is rational and determined by a finite search or its fitted face. Compared the heights over one through the minimum observed count, retaining a finite co-winner at the greatest height even when another slice ties only at its face. Joined saturation for constants and infeasibility of mixed zeros. [Written proof](2026-11-27-crit-open-math-report.md#nhyper-held-all-free) |
| nhyper | all free | failure | Mixed zero and positive draws are infeasible; on positive nonconstant data every maximizing escape reaches a winning shifted negative-binomial slice, including ties with a finite fit; constant positive draws c allow saturation at r=c with m/N tending to one; all feasible zero-draw sequences have the attained height one. | Extracted a fixed stopping count and a convergent marked proportion. Proved the endpoint exclusions for positive nonconstants and that the reached slice has the global height, including equality with finite winners. On positive constants proved eventual exact stopping count and convergence of the entire marked fraction to one. Joined flat compatible zeros and infeasible mixed zeros. [Written proof 1](2026-11-27-uniq-atlas-math-report.md#nhyper-held-all-free) · [Written proof 2](2026-11-27-crit-open-math-report.md#nhyper-held-all-free) |
| nhyper | all free | uniqueness | Not unique in general: with all parameters free, (1,1) attains at every r=1, m=N>=1. More generally a positive constant c saturates exactly at r=c, m=N>=c, and zero constants at every feasible r=0. For positive nonconstant data the maximizing set is computable by exact rational comparison of fixed-stopping-count slices. | Joined the exact saturated constant sets with the finite rational slice comparison and all co-winners, including slices that tie at the greatest height. [Written proof 1](2026-11-27-uniq-atlas-math-report.md#nhyper-held-all-free) · [Written proof 2](2026-11-27-crit-open-math-report.md#nhyper-held-all-free) |
| nhyper | r | criterion | For positive held stopping count and feasible data, the finite population tail test decides attainment including equality. Holding zero requires all zero data and gives attainment. | Bracketed the finite population slice winners by exact log inequalities. Matched the normalized polynomials to the original likelihood, excluded identically equal rays by root multiplicity, and used a common Cauchy cutoff and integer congruences. Joined the prefix and residue comparison with saturated samples and flat compatible zeros. [Written proof](2026-11-25-nhyper-math-report.md#an-exact-finite-test-including-equality-and-escape) |
| nhyper | r | failure | Compare the exact finite candidate list with the shifted negative-binomial supremum; if strictly better than finite candidates it wins. | Proved the shifted negative-binomial product limit and reachability for the entire closed marked-fraction interval. The optimized face has rational height, and it is strictly greater than every finite likelihood exactly when the finite comparison finds neither a strict winner nor a finite equality. [Written proof](2026-11-25-nhyper-math-report.md#an-exact-finite-test-including-equality-and-escape) |
| nhyper | r | uniqueness | The maximizing set is all of the winning finite population/composition pairs in the exact search, with a flat family on compatible zeros when r=0. | When a strict witness exists, a falling-factorial bound and Bernoulli’s inequality give an explicit rational cutoff for the search box containing every maximizer. Otherwise every winner is a finite equality in the prefix. The joined theorem also includes the all-marked saturation set and the flat compatible zero-stop set. [Written proof](2026-11-25-nhyper-math-report.md#an-exact-finite-test-including-equality-and-escape) |
| gpd | mu+sigma | uniqueness | All fitted shapes maximize on all-zero excesses, and every other compatible sample has exactly one shape maximizer. | Proved the integral representation and strict secant comparison in the nonuniform branch. Joined it with all-zero flatness and the closed uniform case. [Written proof](2026-11-27-lsheld-atlas-math-report.md#gpd-held-musigma) |
| gpd | sigma | failure | No boundary escape prevents attainment, and the only positive infinite-shape trace is at the common minimum of a constant sample. | Proved a uniform receding-location loss bound, vanishing at infinite scaled gap and a nonuniform recovery path at closed uniform contact. Joined these with finite attainment, finite nonuniform contact and all positive infinite-shape branches. [Written proof](2026-11-27-lsheld-atlas-math-report.md#gpd-held-sigma) |
| gpd | sigma | uniqueness | Uniform locations fill the feasible interval when it exists, constant data also admit all fitted shapes at their common value, and a span above scale gives a unique nonuniform fit. | Profiled location strictly to the sample minimum for span above scale and applied the secant comparison there. Joined the singleton with the uniform interval and the constant shape ridge. [Written proof](2026-11-27-lsheld-atlas-math-report.md#gpd-held-sigma) |
| triang | all free | uniqueness | The full maximizing set consists of the observed knots of greatest fitted likelihood with their unique endpoint pairs. Constants have no finite winner. On two levels the heavier knot wins or both win when balanced, including (0,1). Ties are null on each fixed count stratum with at least three levels. | Proved the arbitrary two-level count ordering by comparing the same occupied width at the two knots. Proved the strict likelihood ratio inequality in the extreme-versus-interior gradient calculation. **Open.** Put the profiled endpoint parameters on the fixed-count level strata, prove the envelope gradients including moving extreme contacts, and apply the null-set argument to every pair of distinct knots. [Written proof](2026-11-27-uniq-shape-math-report.md#triang-held-all-free) |
| binom | all free | uniqueness | All zeros maximize on the size-zero and probability-zero axes. A positive constant M gives (M,1). Otherwise, with size unrestricted, the maximizing set is empty at variance at least the mean. Below that threshold it is exactly the pairs (N,mean/N) at feasible sizes with incoming ratio at least one and outgoing ratio at most one, omitting the incoming test at the minimum size. This is a singleton or an adjacent pair whose lower size is not a prime power. | Proved the exact adjacent integer comparison, including the zero-failure-power convention, and derived the neighbor and adjacency rules from a strict middle-point hypothesis. **Open.** Prove that hypothesis for the positive-mean nonconstant profile, which the source obtains from DeRiggi, and formalize the prime-power exclusion from the integer valuations. The first derivative at the Poisson end and the dispersion existence theorem alone leave both steps open. [Written proof](2026-11-27-uniq-discrete-math-report.md#binom-held-all-free) |
| dgamma | all free | uniqueness | Attained fits have a finite analytic candidate list. The sample-dependent trace criterion proves uniqueness whenever it passes. On two cells 0 and K the criterion passes for each fixed positive zero fraction below one when K is sufficiently large, and uniformly for zero fractions between one quarter and three quarters at K >= 2^28. | Proved the arithmetic trace bound on the explicit `K ≥ 2²⁸` rectangle and the positive small-shape limit from the written moment lower bound. **Open.** Prove that lower bound for the actual score, the analytic implicit rate root and both actual score collars. The zero-cell inverse-branch hazard argument, the covariance identities, the uniform trace domination and the fixed-zero-fraction asymptotic still need formalization. [Written proof](2026-11-27-uniq-discrete-math-report.md#dgamma-held-all-free) |
| skellam | all free | uniqueness | The Poisson maximizer is unique on one-sided adjacent counts and whenever the exact one-sided supporting-polynomial test passes. Constants and samples of one observed magnitude with both signs also have unique fits. The general analytic profile has finitely many candidates. | Generalized the one-magnitude crossing to unequal sign counts by a positive integrating factor. Proved the radial series is entire, every mean-line score is analytic through the compatible Poisson seam, and the tail is strictly negative. The joined theorem selects the whole finite candidate set and includes constants with the origin specified for all zeros, adjacent one-sided counts and both supporting-test orientations. [Written proof](2026-11-27-uniq-discrete-math-report.md#skellam-held-all-free) |

## Validation

- Every new module passed `lake env lean` on the workstation through the single-slot wrapper with `nice -n 10`.
- The aggregate workstation `lake build` completed successfully with **8546 jobs**, with the temporary Base7b imports in scope. The build emitted nonblocking Lean linter warnings. The original root file was restored and `git diff --exit-code dev/lean/UnivariateML.lean` passed.
- `python3 dev/tools/existence_atlas.py --check` passed. All 44 coverage fields in the new fragment are blank.
- Lean’s axiom audit checked every mapped declaration. The dependency lists use only the standard Lean kernel axioms.

## Checked declarations

### `UnivariateML.Base7bHyper.large_design_cutoff`

**hyper · all free · criterion.** Any threshold strictly above the fitted Poisson height excludes all compatible urns with two sufficiently large designs. [Lean source](../../lean/UnivariateML/Family/Base7bHyper.lean#L131)

```lean
theorem large_design_cutoff {h : ℕ} (hh : 0 < h) (x : Fin h → ℕ)
    (hX : 0 < ∑ i, x i) {B : ℝ}
    (hB : let μ : ℝ := (∑ i, (x i : ℝ)) / h
          (∏ i, μ ^ x i / (x i).factorial * exp (-μ)) < B) :
    ∃ S : ℕ, ∀ K N k : ℕ, S ≤ K → S ≤ k →
      (∀ i, Compatible K N k (x i)) → likelihood K N k x < B
```

### `UnivariateML.Base7bHyper.joint_rate_extremes`

**hyper · all free · failure.** With two growing designs, infinite rate gives zero likelihood. Rate zero gives zero likelihood on samples containing a positive count. [Lean source](../../lean/UnivariateML/Family/Base7bHyper.lean#L157)

```lean
theorem joint_rate_extremes {h : ℕ} (hh : 0 < h) (x : Fin h → ℕ)
    (K N k : ℕ → ℕ) (hK : Tendsto K atTop atTop) (hk : Tendsto k atTop atTop)
    (hc : ∀ᶠ j in atTop, ∀ i, Compatible (K j) (N j) (k j) (x i)) :
    let rate := fun j ↦ (K j : ℝ) * k j / N j
    ((0 < ∑ i, x i) → Tendsto rate atTop (𝓝 0) →
      Tendsto (fun j ↦ likelihood (K j) (N j) (k j) x) atTop (𝓝 0)) ∧
    (Tendsto rate atTop atTop →
      Tendsto (fun j ↦ likelihood (K j) (N j) (k j) x) atTop (𝓝 0))
```

### `UnivariateML.Base7bHyper.joint_poisson_limit`

**hyper · all free · failure.** Jointly growing compatible designs with a finite rate limit give the Poisson product, including at rate zero. [Lean source](../../lean/UnivariateML/Family/Base7bHyper.lean#L444)

```lean
theorem joint_poisson_limit {h : ℕ} (x : Fin h → ℕ)
    (K N k : ℕ → ℕ) (hK : Tendsto K atTop atTop) (hk : Tendsto k atTop atTop)
    (hc : ∀ᶠ j in atTop, ∀ i, Compatible (K j) (N j) (k j) (x i))
    {r : ℝ} (hr : Tendsto (fun j ↦ (K j : ℝ) * k j / N j) atTop (𝓝 r)) :
    Tendsto (fun j ↦ likelihood (K j) (N j) (k j) x) atTop
      (𝓝 (∏ i, r ^ x i / (x i).factorial * exp (-r)))
```

### `UnivariateML.Base7bHyper.positive_population_escape`

**hyper · all free · failure.** Every positive population escape has a fixed-design binomial subsequence or a jointly growing Poisson subsequence. Saturated constant traces are deterministic. [Lean source](../../lean/UnivariateML/Family/Base7bHyper.lean#L557)

```lean
theorem positive_population_escape {h : ℕ} (hh : 0 < h) (x : Fin h → ℕ)
    (K N k : ℕ → ℕ) (hc : ∀ t i, Compatible (K t) (N t) (k t) (x i))
    (hN : Tendsto N atTop atTop) {L : ℝ} (hL : 0 < L)
    (hl : Tendsto (fun t ↦ likelihood (K t) (N t) (k t) x) atTop (𝓝 L)) :
    (∃ d : ℕ, (∀ i, x i ≤ d) ∧ ∃ p ∈ Icc (0 : ℝ) 1, ∃ f : ℕ → ℕ,
      Tendsto f atTop atTop ∧
      (((∀ j, k (f j) = d) ∧ Tendsto (fun j ↦ (K (f j) : ℝ) / N (f j)) atTop (𝓝 p)) ∨
       ((∀ j, K (f j) = d) ∧ Tendsto (fun j ↦ (k (f j) : ℝ) / N (f j)) atTop (𝓝 p))) ∧
      L = (∏ i, binomialFace d (x i) p) ∧
      (∀ c, (∀ i, x i = c) → L = 1 →
        d = 0 ∨ (p = 0 ∧ c = 0) ∨ (p = 1 ∧ c = d))) ∨
    (∃ r ≥ (0 : ℝ), ∃ f : ℕ → ℕ, Tendsto f atTop atTop ∧
      Tendsto (K ∘ f) atTop atTop ∧ Tendsto (k ∘ f) atTop atTop ∧
      Tendsto (fun j ↦ (K (f j) : ℝ) * k (f j) / N (f j)) atTop (𝓝 r) ∧
      ((0 < ∑ i, x i) → 0 < r) ∧
      L = (∏ i, r ^ x i / (x i).factorial * exp (-r)) ∧
      (∀ c, (∀ i, x i = c) → L = 1 → r = 0 ∧ c = 0))
```

### `UnivariateML.Base7bHyper.underdispersion_corner_cutoff`

**hyper · all free · criterion.** Underdispersion gives a highest binomial face above Poisson and excludes the two-large-design corner at that height. [Lean source](../../lean/UnivariateML/Family/Base7bHyper.lean#L648)

```lean
theorem underdispersion_corner_cutoff {h : ℕ} (hh : 0 < h) (x : Fin h → ℕ)
    (hX : 0 < ∑ i, x i) (hv : BinomialSize.variance x < BinomialSize.mean x) :
    let μ := BinomialSize.mean x
    ∃ d ∈ BinomialSize.feasible x, ∃ S : ℕ,
      IsMaxOn (BinomialSize.profile x) (BinomialSize.feasible x) d ∧
      (∏ i, μ ^ x i / (x i).factorial * exp (-μ)) <
        (∏ i, binomialFace d (x i) (μ / d)) ∧
      ∀ K N k : ℕ, S ≤ K → S ≤ k → (∀ i, Compatible K N k (x i)) →
        likelihood K N k x < ∏ i, binomialFace d (x i) (μ / d)
```

### `UnivariateML.Base7bHyperBinary.mixed_binary_maximizers`

**hyper · all free · uniqueness.** Mixed binary histograms attain exactly when one design is one and the other design proportion is the empirical fraction of ones. [Lean source](../../lean/UnivariateML/Family/Base7bHyperBinary.lean#L157)

```lean
theorem mixed_binary_maximizers {h : ℕ} (x : Fin h → ℕ)
    (hx : ∀ i, x i = 0 ∨ x i = 1) (hzero : ∃ i, x i = 0) (hone : ∃ i, x i = 1) :
    let D := {p : ℕ × ℕ × ℕ | ∀ i, Compatible p.1 p.2.1 p.2.2 (x i)}
    let L := fun p : ℕ × ℕ × ℕ ↦ likelihood p.1 p.2.1 p.2.2 x
    let o := (Finset.univ.filter fun i ↦ x i = 1).card
    let z := (Finset.univ.filter fun i ↦ x i = 0).card
    ∀ p ∈ D, IsMaxOn L D p ↔
      (p.2.2 = 1 ∧ p.1 * (z + o) = o * p.2.1) ∨
        (p.1 = 1 ∧ p.2.2 * (z + o) = o * p.2.1)
```

### `UnivariateML.Base7bHyperBinary.binary_pair_even_population`

**hyper · all free · uniqueness.** The sample (0,1) attains on both labeled even-population rays. The two labels coincide at population two. [Lean source](../../lean/UnivariateML/Family/Base7bHyperBinary.lean#L261)

```lean
theorem binary_pair_even_population :
    let x : Fin 2 → ℕ := fun i ↦ i.val
    let D := {p : ℕ × ℕ × ℕ | ∀ i, Compatible p.1 p.2.1 p.2.2 (x i)}
    let L := fun p : ℕ × ℕ × ℕ ↦ likelihood p.1 p.2.1 p.2.2 x
    (∀ p ∈ D, IsMaxOn L D p ↔
      (p.2.2 = 1 ∧ 2 * p.1 = p.2.1) ∨ (p.1 = 1 ∧ 2 * p.2.2 = p.2.1)) ∧
    (∀ d > 0, (d, 2 * d, 1) ∈ D ∧ IsMaxOn L D (d, 2 * d, 1) ∧
      (1, 2 * d, d) ∈ D ∧ IsMaxOn L D (1, 2 * d, d))
```

### `UnivariateML.Base7bHyperResidue.held_draw_ratio_brackets`

**hyper · k · criterion.** Exact log bounds force strict adjacent-ratio signs outside a fixed-width band. [Lean source](../../lean/UnivariateML/Family/Base7bHyperResidue.lean#L125)

```lean
theorem held_draw_ratio_brackets {h N K k : ℕ} (x : Fin h → ℕ)
    (hx : ∀ i, x i ≤ k) (hK : k ≤ K) (hN : K + k < N)
    (hS : 0 < ∑ i, (x i : ℝ)) (hT : 0 < ∑ i, ((k : ℝ) - x i)) :
    let p : ℝ := (∑ i, (x i : ℝ)) / ((∑ i, (x i : ℝ)) + ∑ i, ((k : ℝ) - x i))
    ((K : ℝ) + 1 + k < p * N → 1 < compositionRatio K N k x) ∧
    (p * N + k < (K : ℝ) → compositionRatio K N k x < 1)
```

### `UnivariateML.Base7bHyperResidue.held_draw_population_band`

**hyper · k · criterion.** Both possible winners at every sufficiently large population lie within a fixed integer band around the fitted binomial composition. [Lean source](../../lean/UnivariateML/Family/Base7bHyperResidue.lean#L195)

```lean
theorem held_draw_population_band {h : ℕ} (hh : 0 < h) (k : ℕ) (x : Fin h → ℕ)
    (hx : ∀ i, x i ≤ k) (hS : 0 < ∑ i, (x i : ℝ)) (hT : 0 < ∑ i, ((k : ℝ) - x i)) :
    let p : ℝ := (∑ i, (x i : ℝ)) / ((∑ i, (x i : ℝ)) + ∑ i, ((k : ℝ) - x i))
    ∃ B : ℕ, ∀ N ≥ B, ∀ M ∈ compositionDomain N k x,
      IsMaxOn (fun m ↦ likelihood m N k x) (compositionDomain N k x) M →
      ⌊p * N⌋₊ - (k + 2) < M ∧ M ≤ ⌊p * N⌋₊ + (k + 2)
```

### `UnivariateML.Base7bHyperResidue.held_draw_affine_tail_table`

**hyper · k · criterion.** A bounded rational affine table contains every tail slice winner. Its polynomials give the exact comparison against any rational height. [Lean source](../../lean/UnivariateML/Family/Base7bHyperResidue.lean#L291)

```lean
theorem held_draw_affine_tail_table {h : ℕ} (hh : 0 < h) (k : ℕ) (x : Fin h → ℕ)
    (hx : ∀ i, x i ≤ k) (hS : 0 < ∑ i, (x i : ℝ)) (hT : 0 < ∑ i, ((k : ℝ) - x i))
    (face : ℚ) :
    let a : ℕ := ∑ i, x i
    let d := h * k
    let E := Finset.Icc (-((d * (k + 3) : ℕ) : ℤ)) ((d * (k + 3) : ℕ) : ℤ)
    ∃ (B : ℕ) (P : ℤ → ℚ[X]),
      (∀ N ≥ B, ∀ m ∈ compositionDomain N k x,
        IsMaxOn (fun v ↦ likelihood v N k x) (compositionDomain N k x) m →
        ∃ e ∈ E, (d : ℤ) * m = (a : ℤ) * N + e) ∧
      ∀ N ≥ B, ∀ (m : ℕ) (e : ℤ), e ∈ E → (d : ℤ) * m = (a : ℤ) * N + e →
        (∀ i, Compatible m N k (x i)) →
        (((face : ℝ) < likelihood m N k x ↔ 0 < (P e).leadingCoeff) ∧
         (likelihood m N k x = (face : ℝ) ↔ P e = 0) ∧
         (likelihood m N k x < (face : ℝ) ↔ (P e).leadingCoeff < 0))
```

### `UnivariateML.Base7bHyperResidue.held_draw_finite_test_interior`

**hyper · k · criterion.** At interior fitted proportion a finite prefix and a congruence test on polynomial signs decide attainment. A zero polynomial qualifies. [Lean source](../../lean/UnivariateML/Family/Base7bHyperResidue.lean#L465)

```lean
theorem held_draw_finite_test_interior {h : ℕ} (hh : 0 < h) (k : ℕ) (x : Fin h → ℕ)
    (hx : ∀ i, x i ≤ k) (hS : 0 < ∑ i, (x i : ℝ)) (hT : 0 < ∑ i, ((k : ℝ) - x i)) :
    let a : ℕ := ∑ i, x i
    let d := h * k
    let E := Finset.Icc (-((d * (k + 3) : ℕ) : ℤ)) ((d * (k + 3) : ℕ) : ℤ)
    let D := {v : ℕ × ℕ | v.1 ≤ v.2 ∧ k ≤ v.2 ∧ ∀ i, Compatible v.1 v.2 k (x i)}
    let L := fun v : ℕ × ℕ ↦ likelihood v.1 v.2 k x
    let F : ℚ := ∏ i, (k.choose (x i) : ℚ) * ((a : ℚ) / d) ^ x i *
      (1 - (a : ℚ) / d) ^ (k - x i)
    ∃ (B : ℕ) (P : ℤ → ℚ[X]),
      ((∃ v ∈ D, IsMaxOn L D v) ↔
        (∃ N < B, ∃ m, (m, N) ∈ D ∧ (F : ℝ) ≤ L (m, N)) ∨
        (∃ e ∈ E, (∃ j < d, (d : ℤ) ∣ (a : ℤ) * j + e) ∧
          (0 < (P e).leadingCoeff ∨ P e = 0)))
```

### `UnivariateML.Base7bHyperResidue.held_draw_finite_test`

**hyper · k · criterion.** Both fitted proportion endpoints attain. The interior branch uses the finite prefix and congruence test. [Lean source](../../lean/UnivariateML/Family/Base7bHyperResidue.lean#L535)

```lean
theorem held_draw_finite_test {h : ℕ} (hh : 0 < h) (k : ℕ) (x : Fin h → ℕ)
    (hx : ∀ i, x i ≤ k) :
    let a : ℕ := ∑ i, x i
    let d := h * k
    let E := Finset.Icc (-((d * (k + 3) : ℕ) : ℤ)) ((d * (k + 3) : ℕ) : ℤ)
    let D := {v : ℕ × ℕ | v.1 ≤ v.2 ∧ k ≤ v.2 ∧ ∀ i, Compatible v.1 v.2 k (x i)}
    let L := fun v : ℕ × ℕ ↦ likelihood v.1 v.2 k x
    let F : ℚ := ∏ i, (k.choose (x i) : ℚ) * ((a : ℚ) / d) ^ x i *
      (1 - (a : ℚ) / d) ^ (k - x i)
    (((∀ i, x i = 0) ∨ (∀ i, x i = k)) → ∃ v ∈ D, IsMaxOn L D v) ∧
    ((∃ i, 0 < x i) → (∃ i, x i < k) →
      ∃ (B : ℕ) (P : ℤ → ℚ[X]),
        ((∃ v ∈ D, IsMaxOn L D v) ↔
          (∃ N < B, ∃ m, (m, N) ∈ D ∧ (F : ℝ) ≤ L (m, N)) ∨
          (∃ e ∈ E, (∃ j < d, (d : ℤ) ∣ (a : ℤ) * j + e) ∧
            (0 < (P e).leadingCoeff ∨ P e = 0))))
```

### `UnivariateML.Base7bHyperResidue.held_mark_finite_test`

**hyper · m · criterion.** Design exchange transports the complete finite prefix and congruence attainment test, including both endpoints. [Lean source](../../lean/UnivariateML/Family/Base7bHyperResidue.lean#L584)

```lean
theorem held_mark_finite_test {h : ℕ} (hh : 0 < h) (k : ℕ) (x : Fin h → ℕ)
    (hx : ∀ i, x i ≤ k) :
    let a : ℕ := ∑ i, x i
    let d := h * k
    let E := Finset.Icc (-((d * (k + 3) : ℕ) : ℤ)) ((d * (k + 3) : ℕ) : ℤ)
    let D := {v : ℕ × ℕ | v.1 ≤ v.2 ∧ k ≤ v.2 ∧ ∀ i, Compatible k v.2 v.1 (x i)}
    let L := fun v : ℕ × ℕ ↦ likelihood k v.2 v.1 x
    let F : ℚ := ∏ i, (k.choose (x i) : ℚ) * ((a : ℚ) / d) ^ x i *
      (1 - (a : ℚ) / d) ^ (k - x i)
    (((∀ i, x i = 0) ∨ (∀ i, x i = k)) → ∃ v ∈ D, IsMaxOn L D v) ∧
    ((∃ i, 0 < x i) → (∃ i, x i < k) →
      ∃ (B : ℕ) (P : ℤ → ℚ[X]),
        ((∃ v ∈ D, IsMaxOn L D v) ↔
          (∃ N < B, ∃ m, (m, N) ∈ D ∧ (F : ℝ) ≤ L (m, N)) ∨
          (∃ e ∈ E, (∃ j < d, (d : ℤ) ∣ (a : ℤ) * j + e) ∧
            (0 < (P e).leadingCoeff ∨ P e = 0))))
```

### `UnivariateML.Base7bHyperResidue.held_draw_interior_winners`

**hyper · k · uniqueness.** A strict witness confines the winners to a finite box. Otherwise every prefix equality and all compatible identically equal tail rays win. [Lean source](../../lean/UnivariateML/Family/Base7bHyperResidue.lean#L639)

```lean
theorem held_draw_interior_winners {h : ℕ} (hh : 0 < h) (k : ℕ) (x : Fin h → ℕ)
    (hx : ∀ i, x i ≤ k) (hS : 0 < ∑ i, (x i : ℝ)) (hT : 0 < ∑ i, ((k : ℝ) - x i)) :
    let a : ℕ := ∑ i, x i
    let d := h * k
    let E := Finset.Icc (-((d * (k + 3) : ℕ) : ℤ)) ((d * (k + 3) : ℕ) : ℤ)
    let D := {v : ℕ × ℕ | v.1 ≤ v.2 ∧ k ≤ v.2 ∧ ∀ i, Compatible v.1 v.2 k (x i)}
    let L := fun v : ℕ × ℕ ↦ likelihood v.1 v.2 k x
    let F : ℚ := ∏ i, (k.choose (x i) : ℚ) * ((a : ℚ) / d) ^ x i *
      (1 - (a : ℚ) / d) ^ (k - x i)
    ((∃ v ∈ D, (F : ℝ) < L v) → ∃ C : ℕ,
      let box := {v ∈ D | v.2 < C}
      ∀ v, (v ∈ D ∧ IsMaxOn L D v) ↔ v ∈ box ∧ IsMaxOn L box v) ∧
    ((∀ v ∈ D, L v ≤ (F : ℝ)) → ∃ (B : ℕ) (P : ℤ → ℚ[X]),
      ∀ v ∈ D, IsMaxOn L D v ↔
        (v.2 < B ∧ L v = (F : ℝ)) ∨
        (B ≤ v.2 ∧ ∃ e ∈ E, (d : ℤ) * v.1 = (a : ℤ) * v.2 + e ∧ P e = 0))
```

### `UnivariateML.Base7bHyperResidue.held_draw_constant_winners`

**hyper · k · uniqueness.** Constant observations select exactly the saturated urns, including all compatible zero-draw urns. [Lean source](../../lean/UnivariateML/Family/Base7bHyperResidue.lean#L717)

```lean
theorem held_draw_constant_winners {h : ℕ} (hh : 0 < h) {c k : ℕ} (hc : c ≤ k) :
    let x := fun _ : Fin h ↦ c
    let D := {v : ℕ × ℕ | v.1 ≤ v.2 ∧ k ≤ v.2 ∧ ∀ i, Compatible v.1 v.2 k (x i)}
    let L := fun v : ℕ × ℕ ↦ likelihood v.1 v.2 k x
    ∀ v ∈ D, IsMaxOn L D v ↔
      (c = 0 ∧ (v.1 = 0 ∨ k = 0)) ∨ (v.1 = v.2 ∧ k = c) ∨ (k = v.2 ∧ v.1 = c)
```

### `UnivariateML.Base7bHyperResidue.held_draw_winners`

**hyper · k · uniqueness.** The joined fixed-draw result gives every constant, finite-box or equality-ray winner and the complete fixed-one even-population example. [Lean source](../../lean/UnivariateML/Family/Base7bHyperResidue.lean#L786)

```lean
theorem held_draw_winners {h : ℕ} (hh : 0 < h) (k : ℕ) (x : Fin h → ℕ)
    (hx : ∀ i, x i ≤ k) :
    let a : ℕ := ∑ i, x i
    let d := h * k
    let E := Finset.Icc (-((d * (k + 3) : ℕ) : ℤ)) ((d * (k + 3) : ℕ) : ℤ)
    let D := {v : ℕ × ℕ | v.1 ≤ v.2 ∧ k ≤ v.2 ∧ ∀ i, Compatible v.1 v.2 k (x i)}
    let L := fun v : ℕ × ℕ ↦ likelihood v.1 v.2 k x
    let F : ℚ := ∏ i, (k.choose (x i) : ℚ) * ((a : ℚ) / d) ^ x i *
      (1 - (a : ℚ) / d) ^ (k - x i)
    (∀ c : ℕ, (∀ i, x i = c) → ∀ v ∈ D, IsMaxOn L D v ↔
      (c = 0 ∧ (v.1 = 0 ∨ k = 0)) ∨ (v.1 = v.2 ∧ k = c) ∨ (k = v.2 ∧ v.1 = c)) ∧
    ((∃ i j, x i ≠ x j) →
      ((∃ v ∈ D, (F : ℝ) < L v) → ∃ C : ℕ,
        let box := {v ∈ D | v.2 < C}
        ∀ v, (v ∈ D ∧ IsMaxOn L D v) ↔ v ∈ box ∧ IsMaxOn L box v) ∧
      ((∀ v ∈ D, L v ≤ (F : ℝ)) → ∃ (B : ℕ) (P : ℤ → ℚ[X]),
        ∀ v ∈ D, IsMaxOn L D v ↔
          (v.2 < B ∧ L v = (F : ℝ)) ∨
          (B ≤ v.2 ∧ ∃ e ∈ E, (d : ℤ) * v.1 = (a : ℤ) * v.2 + e ∧ P e = 0))) ∧
    (let y : Fin 2 → ℕ := fun i ↦ i.val
     let D₀ := {v : ℕ × ℕ | ∀ i, Compatible v.1 v.2 1 (y i)}
     let L₀ := fun v : ℕ × ℕ ↦ likelihood v.1 v.2 1 y
     (∀ v ∈ D₀, IsMaxOn L₀ D₀ v ↔ 2 * v.1 = v.2) ∧
     ∀ d > 0, (d, 2 * d) ∈ D₀ ∧ IsMaxOn L₀ D₀ (d, 2 * d))
```

### `UnivariateML.Base7bHyperResidue.held_mark_winners`

**hyper · m · uniqueness.** Design exchange transports every winner, including all constant and equality cases and the fixed-one even-population example. [Lean source](../../lean/UnivariateML/Family/Base7bHyperResidue.lean#L837)

```lean
theorem held_mark_winners {h : ℕ} (hh : 0 < h) (k : ℕ) (x : Fin h → ℕ)
    (hx : ∀ i, x i ≤ k) :
    let a : ℕ := ∑ i, x i
    let d := h * k
    let E := Finset.Icc (-((d * (k + 3) : ℕ) : ℤ)) ((d * (k + 3) : ℕ) : ℤ)
    let D := {v : ℕ × ℕ | v.1 ≤ v.2 ∧ k ≤ v.2 ∧ ∀ i, Compatible k v.2 v.1 (x i)}
    let L := fun v : ℕ × ℕ ↦ likelihood k v.2 v.1 x
    let F : ℚ := ∏ i, (k.choose (x i) : ℚ) * ((a : ℚ) / d) ^ x i *
      (1 - (a : ℚ) / d) ^ (k - x i)
    (∀ c : ℕ, (∀ i, x i = c) → ∀ v ∈ D, IsMaxOn L D v ↔
      (c = 0 ∧ (v.1 = 0 ∨ k = 0)) ∨ (v.1 = v.2 ∧ k = c) ∨ (k = v.2 ∧ v.1 = c)) ∧
    ((∃ i j, x i ≠ x j) →
      ((∃ v ∈ D, (F : ℝ) < L v) → ∃ C : ℕ,
        let box := {v ∈ D | v.2 < C}
        ∀ v, (v ∈ D ∧ IsMaxOn L D v) ↔ v ∈ box ∧ IsMaxOn L box v) ∧
      ((∀ v ∈ D, L v ≤ (F : ℝ)) → ∃ (B : ℕ) (P : ℤ → ℚ[X]),
        ∀ v ∈ D, IsMaxOn L D v ↔
          (v.2 < B ∧ L v = (F : ℝ)) ∨
          (B ≤ v.2 ∧ ∃ e ∈ E, (d : ℤ) * v.1 = (a : ℤ) * v.2 + e ∧ P e = 0))) ∧
    (let y : Fin 2 → ℕ := fun i ↦ i.val
     let D₀ := {v : ℕ × ℕ | ∀ i, Compatible 1 v.2 v.1 (y i)}
     let L₀ := fun v : ℕ × ℕ ↦ likelihood 1 v.2 v.1 y
     (∀ v ∈ D₀, IsMaxOn L₀ D₀ v ↔ 2 * v.1 = v.2) ∧
     ∀ d > 0, (d, 2 * d) ∈ D₀ ∧ IsMaxOn L₀ D₀ (d, 2 * d))
```

### `UnivariateML.Base7bNHyper.held_stop_face_limit`

**nhyper · r · failure.** A fixed positive stopping count gives the shifted negative-binomial face along every convergent marked proportion. [Lean source](../../lean/UnivariateML/Family/Base7bNHyper.lean#L63)

```lean
theorem held_stop_face_limit {h r : ℕ} (hr : 0 < r) (x : Fin h → ℕ)
    {m N : ℕ → ℕ} {p : ℝ} (hp : p ∈ Icc 0 1) (hN : Tendsto N atTop atTop)
    (hm : ∀ᶠ j in atTop, m j ≤ N j)
    (hprop : Tendsto (fun j ↦ (m j : ℝ) / N j) atTop (𝓝 p)) :
    Tendsto (fun j ↦ totalLikelihood x (N j) (m j) r) atTop
      (𝓝 (∏ i, ((r : ℝ) / x i) * Discrete4Hyper.binomialFace (x i) r p))
```

### `UnivariateML.Base7bNHyper.held_stop_face_reachable`

**nhyper · r · failure.** Each marked fraction in the closed unit interval is realized by a compatible population path. [Lean source](../../lean/UnivariateML/Family/Base7bNHyper.lean#L86)

```lean
theorem held_stop_face_reachable {h r : ℕ} (hr : 0 < r) (x : Fin h → ℕ)
    (hx : ∀ i, r ≤ x i) {p : ℝ} (hp : p ∈ Icc 0 1) :
    ∃ m N : ℕ → ℕ, Tendsto N atTop atTop ∧
      (∀ t i, Compatible (N t) (m t) r (x i)) ∧
      Tendsto (fun t ↦ (m t : ℝ) / N t) atTop (𝓝 p) ∧
      Tendsto (fun t ↦ totalLikelihood x (N t) (m t) r) atTop
        (𝓝 (∏ i, ((r : ℝ) / x i) * Discrete4Hyper.binomialFace (x i) r p))
```

### `UnivariateML.Base7bNHyper.held_stop_uniform_cutoff`

**nhyper · r · failure.** A height strictly above every fixed-stop face excludes every composition at sufficiently large population. [Lean source](../../lean/UnivariateML/Family/Base7bNHyper.lean#L111)

```lean
theorem held_stop_uniform_cutoff {h r : ℕ} (hh : 0 < h) (hr : 0 < r) (x : Fin h → ℕ)
    {B : ℝ} (hB : ∀ p ∈ Icc (0 : ℝ) 1,
      (∏ i, ((r : ℝ) / x i) * Discrete4Hyper.binomialFace (x i) r p) < B) :
    ∃ N₀ : ℕ, ∀ N ≥ N₀, ∀ m, (∀ i, Compatible N m r (x i)) →
      totalLikelihood x N m r < B
```

### `UnivariateML.Base7bNHyper.positive_nonconstant_maximizing_escape`

**nhyper · all free · failure.** A positive nonconstant maximizing path has an interior fixed-stop face at the winning slice supremum. [Lean source](../../lean/UnivariateML/Family/Base7bNHyper.lean#L143)

```lean
theorem positive_nonconstant_maximizing_escape {h : ℕ} (x : Fin h → ℕ)
    {i : Fin h} (_hi : ∀ j, x i ≤ x j) (hx : 0 < x i)
    (hne : ∃ j, x i < x j)
    (N m r : ℕ → ℕ) (hc : ∀ t j, Compatible (N t) (m t) (r t) (x j))
    (hN : Tendsto N atTop atTop) {L : ℝ} (hL : 0 < L)
    (hl : Tendsto (fun t ↦ totalLikelihood x (N t) (m t) (r t)) atTop (𝓝 L))
    (hwin : ∀ A B C, (∀ j, Compatible A B C (x j)) → totalLikelihood x A B C ≤ L) :
    ∃ s ∈ Finset.Icc 1 (x i), ∃ p ∈ Ioo (0 : ℝ) 1, ∃ f : ℕ → ℕ,
      Tendsto f atTop atTop ∧ (∀ t, r (f t) = s) ∧
      Tendsto (fun t ↦ (m (f t) : ℝ) / N (f t)) atTop (𝓝 p) ∧
      L = (∏ j, ((s : ℝ) / x j) * Discrete4Hyper.binomialFace (x j) s p) ∧
      IsLUB {v | ∃ A B, (∀ j, Compatible A B s (x j)) ∧
        v = totalLikelihood x A B s} L
```

### `UnivariateML.Base7bNHyper.all_free_maximizing_escape`

**nhyper · all free · failure.** The all-free maximizing escape classification includes incompatible mixed zeros, flat compatible zeros, positive constants and the winning interior slices. [Lean source](../../lean/UnivariateML/Family/Base7bNHyper.lean#L282)

```lean
theorem all_free_maximizing_escape {h : ℕ} (hh : 0 < h) (x : Fin h → ℕ) :
    ((∃ i j, x i = 0 ∧ 0 < x j) → ¬ ∃ N m r, ∀ k, Compatible N m r (x k)) ∧
    (∀ (N m r : ℕ → ℕ), (∀ t i, Compatible (N t) (m t) (r t) (x i)) →
      Tendsto N atTop atTop → ∀ L : ℝ,
      Tendsto (fun t ↦ totalLikelihood x (N t) (m t) (r t)) atTop (𝓝 L) →
      (∀ A B C, (∀ j, Compatible A B C (x j)) → totalLikelihood x A B C ≤ L) →
      ((∀ i, x i = 0) ∧ (∀ t, r t = 0) ∧ L = 1) ∨
      (∃ c > 0, (∀ i, x i = c) ∧ L = 1 ∧ (∀ᶠ t in atTop, r t = c) ∧
        Tendsto (fun t ↦ (m t : ℝ) / N t) atTop (𝓝 1)) ∨
      (∃ i, (∀ j, x i ≤ x j) ∧ 0 < x i ∧ (∃ j, x i < x j) ∧
        ∃ s ∈ Finset.Icc 1 (x i), ∃ p ∈ Ioo (0 : ℝ) 1, ∃ f : ℕ → ℕ,
          Tendsto f atTop atTop ∧ (∀ t, r (f t) = s) ∧
          Tendsto (fun t ↦ (m (f t) : ℝ) / N (f t)) atTop (𝓝 p) ∧
          L = (∏ j, ((s : ℝ) / x j) * Discrete4Hyper.binomialFace (x j) s p) ∧
          IsLUB {v | ∃ A B, (∀ j, Compatible A B s (x j)) ∧
            v = totalLikelihood x A B s} L))
```

### `UnivariateML.Base7bNHyperResidue.affine_face_polynomial_nonzero`

**nhyper · r · criterion.** If a draw count exceeds the held positive stop, monic affine numerator and denominator products cannot be proportional. [Lean source](../../lean/UnivariateML/Family/Base7bNHyperResidue.lean#L64)

```lean
theorem affine_face_polynomial_nonzero {h r : ℕ} (hr : 0 < r)
    (y : Fin h → ℕ) (hy : ∃ i, 0 < y i) {a b : ℚ} (ha : 0 < a) (hb : 0 < b)
    (c q : ℚ) (hq : q ≠ 0) :
    let U := ∏ i : Fin h,
      ((∏ j ∈ Finset.range r, (X - C (((j : ℚ) - c) / a))) *
       (∏ j ∈ Finset.range (y i), (X - C (((j : ℚ) + c) / b))))
    let D := ∏ i : Fin h, ∏ j ∈ Finset.range (r + y i), (X - C (j : ℚ))
    U - C q * D ≠ 0
```

### `UnivariateML.Base7bNHyperResidue.affine_face_comparison`

**nhyper · r · criterion.** The normalized polynomial evaluation gives the exact strict or equal likelihood comparison on each compatible affine ray. [Lean source](../../lean/UnivariateML/Family/Base7bNHyperResidue.lean#L161)

```lean
theorem affine_face_comparison {h r : ℕ} (hr : 0 < r) (y : Fin h → ℕ)
    (hy : ∃ i, 0 < y i) {a b : ℚ} (ha : 0 < a) (hb : 0 < b) (hab : a + b = 1)
    (c : ℚ) :
    let F : ℚ := ∏ i, ((r + y i - 1).choose (r - 1) : ℚ) * a ^ r * b ^ y i
    ∃ P : ℚ[X], P ≠ 0 ∧ ∀ N m : ℕ, (m : ℚ) = a * N + c →
      (∀ i, Discrete4NHyper.Compatible N m r (r + y i)) →
      let L := Discrete4NHyper.totalLikelihood (fun i ↦ r + y i) N m r
      (0 < P.eval (N : ℚ) ↔ (F : ℝ) < L) ∧
      (P.eval (N : ℚ) = 0 ↔ L = (F : ℝ)) ∧
      (P.eval (N : ℚ) < 0 ↔ L < (F : ℝ))
```

### `UnivariateML.Base7bNHyperResidue.held_stop_ratio_brackets`

**nhyper · r · criterion.** Exact logarithm bounds determine the outer signs of every compatible consecutive composition ratio outside the given interval. [Lean source](../../lean/UnivariateML/Family/Base7bNHyperResidue.lean#L345)

```lean
theorem held_stop_ratio_brackets {h N r m Y : ℕ} (y : Fin h → ℕ)
    (hy : ∀ i, y i ≤ Y) (hrm : r ≤ m) (hmN : m + Y < N)
    (hS : 0 < (h : ℝ) * r) (hT : 0 < ∑ i, (y i : ℝ)) :
    let p := ((h : ℝ) * r) / ((h : ℝ) * r + ∑ i, (y i : ℝ))
    let R := Discrete4NHyper.compositionRatio (fun i ↦ r + y i) N r m
    ((m : ℝ) + 1 + Y < p * N → 1 < R) ∧ (p * N + r < (m : ℝ) → R < 1)
```

### `UnivariateML.Base7bNHyperResidue.held_stop_population_band`

**nhyper · r · criterion.** Every sufficiently large fixed-population slice winner lies within the given integer offset band. [Lean source](../../lean/UnivariateML/Family/Base7bNHyperResidue.lean#L391)

```lean
theorem held_stop_population_band {h r : ℕ} (hh : 0 < h) (hr : 0 < r) (y : Fin h → ℕ)
    (hy : ∃ i, 0 < y i) :
    let x := fun i ↦ r + y i
    let Y := NegHypergeometric.maxBlackDraws y
    let Z := max Y r + 2
    let p := ((h : ℝ) * r) / ((h : ℝ) * r + ∑ i, (y i : ℝ))
    ∃ B : ℕ, ∀ N ≥ B,
      let D := {m : ℕ | ∀ i, Discrete4NHyper.Compatible N m r (x i)}
      ∀ m ∈ D, IsMaxOn (fun m ↦ Discrete4NHyper.totalLikelihood x N m r) D m →
        ⌊p * N⌋₊ - Z < m ∧ m ≤ ⌊p * N⌋₊ + Z + 1
```

### `UnivariateML.Base7bNHyperResidue.held_stop_affine_tail_table`

**nhyper · r · criterion.** A common Cauchy cutoff reduces all compatible tail rays to leading-coefficient signs and excludes tail equality to the face. [Lean source](../../lean/UnivariateML/Family/Base7bNHyperResidue.lean#L521)

```lean
theorem held_stop_affine_tail_table {h r : ℕ} (hh : 0 < h) (hr : 0 < r) (y : Fin h → ℕ)
    (hy : ∃ i, 0 < y i) :
    let x := fun i ↦ r + y i
    let a := h * r
    let b := ∑ i, y i
    let d := a + b
    let Z := max (NegHypergeometric.maxBlackDraws y) r + 2
    let E := Finset.Icc (-((d * (Z + 2) : ℕ) : ℤ)) ((d * (Z + 2) : ℕ) : ℤ)
    let D := fun N : ℕ ↦ {m : ℕ | ∀ i, Discrete4NHyper.Compatible N m r (x i)}
    let L := fun N m ↦ Discrete4NHyper.totalLikelihood x N m r
    let F : ℚ := ∏ i, ((x i - 1).choose (r - 1) : ℚ) *
      ((a : ℚ) / d) ^ r * ((b : ℚ) / d) ^ y i
    ∃ (B : ℕ) (P : ℤ → ℚ[X]),
      (∀ N ≥ B, ∀ m ∈ D N, IsMaxOn (L N) (D N) m →
        ∃ e ∈ E, (d : ℤ) * m = (a : ℤ) * N + e) ∧
      ∀ N ≥ B, ∀ m ∈ D N, ∀ e ∈ E, (d : ℤ) * m = (a : ℤ) * N + e →
        ((F : ℝ) < L N m ↔ 0 < (P e).leadingCoeff) ∧
        L N m ≠ (F : ℝ) ∧
        (L N m < (F : ℝ) ↔ (P e).leadingCoeff < 0)
```

### `UnivariateML.Base7bNHyperResidue.held_stop_optimized_face`

**nhyper · r · failure.** The full shifted negative-binomial face is maximized at the rational fitted marked fraction. Its height includes the restoring combinatorial factor. [Lean source](../../lean/UnivariateML/Family/Base7bNHyperResidue.lean#L620)

```lean
theorem held_stop_optimized_face {h r : ℕ} (hh : 0 < h) (hr : 0 < r) (y : Fin h → ℕ)
    (hy : ∃ i, 0 < y i) :
    let a := h * r
    let b := ∑ i, y i
    let d := a + b
    let p := ((a : ℚ) / d : ℚ)
    let q := ((b : ℚ) / d : ℚ)
    let F : ℚ := ∏ i, ((r + y i - 1).choose (r - 1) : ℚ) * p ^ r * q ^ y i
    let G := fun t : ℝ ↦ ∏ i, ((r : ℝ) / (r + y i : ℕ)) *
      Discrete4Hyper.binomialFace (r + y i) r t
    ((p : ℝ) ∈ Icc 0 1) ∧ G (p : ℝ) = (F : ℝ) ∧
      IsMaxOn G (Icc 0 1) (p : ℝ)
```

### `UnivariateML.Base7bNHyperResidue.held_stop_explicit_cutoff`

**nhyper · r · uniqueness.** An explicit bound evaluated by rational arithmetic from a strict finite witness excludes every higher population, uniformly over compositions. [Lean source](../../lean/UnivariateML/Family/Base7bNHyperResidue.lean#L698)

```lean
theorem held_stop_explicit_cutoff {h r : ℕ} (hr : 0 < r) (y : Fin h → ℕ)
    {F W : ℝ} (hW : 0 < W) (hFW : F < W)
    (hF : ∀ p ∈ Icc (0 : ℝ) 1,
      (∏ i, ((r : ℝ) / (r + y i : ℕ)) * Discrete4Hyper.binomialFace (r + y i) r p) ≤ F) :
    let x := fun i ↦ r + y i
    let X := Finset.univ.sup x
    let T := ∑ i, x i
    ∀ N : ℕ, X < N → (W * (T : ℝ) * X) / (W - F) < (N : ℝ) →
      ∀ m ≤ N, (∀ i, Discrete4NHyper.Compatible N m r (x i)) →
        Discrete4NHyper.totalLikelihood x N m r < W
```

### `UnivariateML.Base7bNHyperResidue.held_stop_positive_decision`

**nhyper · r · criterion.** The exact prefix and eligible tail residues detect every strict witness. Otherwise the finite equality prefix contains the complete maximizing set. [Lean source](../../lean/UnivariateML/Family/Base7bNHyperResidue.lean#L866)

```lean
theorem held_stop_positive_decision {h r : ℕ} (hh : 0 < h) (hr : 0 < r) (y : Fin h → ℕ)
    (hy : ∃ i, 0 < y i) :
    let x := fun i ↦ r + y i
    let a := h * r
    let b := ∑ i, y i
    let d := a + b
    let Z := max (NegHypergeometric.maxBlackDraws y) r + 2
    let E := Finset.Icc (-((d * (Z + 2) : ℕ) : ℤ)) ((d * (Z + 2) : ℕ) : ℤ)
    let D := {v : ℕ × ℕ | ∀ i, Discrete4NHyper.Compatible v.1 v.2 r (x i)}
    let L := fun v : ℕ × ℕ ↦ Discrete4NHyper.totalLikelihood x v.1 v.2 r
    let F : ℚ := ∏ i, ((x i - 1).choose (r - 1) : ℚ) *
      ((a : ℚ) / d) ^ r * ((b : ℚ) / d) ^ y i
    ∃ (B : ℕ) (P : ℤ → ℚ[X]),
      let strict := (∃ N < B, ∃ m, (N, m) ∈ D ∧ (F : ℝ) < L (N, m)) ∨
        (∃ e ∈ E, (∃ j < d, (d : ℤ) ∣ (a : ℤ) * j + e) ∧ 0 < (P e).leadingCoeff)
      (strict ↔ ∃ v ∈ D, (F : ℝ) < L v) ∧
      (strict → ∃ C : ℕ, let box := {v ∈ D | v.1 < C}
        (∃ v ∈ box, IsMaxOn L box v ∧ (F : ℝ) < L v) ∧
        ∀ v, (v ∈ D ∧ IsMaxOn L D v) ↔ v ∈ box ∧ IsMaxOn L box v) ∧
      (¬ strict → IsLUB (L '' D) (F : ℝ) ∧
        ∀ v ∈ D, IsMaxOn L D v ↔ v.1 < B ∧ L v = (F : ℝ))
```

### `UnivariateML.Base7bNHyperResidue.held_stop_complete_search`

**nhyper · r · uniqueness.** The joined search includes flat zeros, positive saturated samples, a finite rational search with all ties, and strict face dominance on nonattainment. [Lean source](../../lean/UnivariateML/Family/Base7bNHyperResidue.lean#L1033)

```lean
theorem held_stop_complete_search {h : ℕ} (hh : 0 < h) (x : Fin h → ℕ) (r : ℕ)
    (hf : ∃ N m, ∀ i, Discrete4NHyper.Compatible N m r (x i)) :
    let D := {v : ℕ × ℕ | ∀ i, Discrete4NHyper.Compatible v.1 v.2 r (x i)}
    let L := fun v : ℕ × ℕ ↦ Discrete4NHyper.totalLikelihood x v.1 v.2 r
    let p : ℚ := ((h * r : ℕ) : ℚ) / ((∑ i, x i : ℕ) : ℚ)
    let F : ℚ := ∏ i, ((x i - 1).choose (r - 1) : ℚ) * p ^ r * (1 - p) ^ (x i - r)
    (r = 0 ∧ (∀ i, x i = 0) ∧ (∀ v ∈ D, L v = 1 ∧ IsMaxOn L D v) ∧
      IsLUB (L '' D) 1) ∨
    (0 < r ∧ (∀ i, x i = r) ∧ (r, r) ∈ D ∧
      (∀ v ∈ D, IsMaxOn L D v ↔ v.1 = v.2) ∧ IsLUB (L '' D) 1) ∨
    (0 < r ∧ (∃ i, r < x i) ∧ ∃ (H : ℚ) (C : ℕ),
      IsLUB (L '' D) (H : ℝ) ∧ (F : ℝ) ≤ (H : ℝ) ∧
      (∀ v ∈ D, IsMaxOn L D v ↔ v.1 < C ∧ L v = (H : ℝ)) ∧
      ((∃ v ∈ D, IsMaxOn L D v) ↔ ∃ v ∈ D, v.1 < C ∧ L v = (H : ℝ)) ∧
      ((¬ ∃ v ∈ D, v.1 < C ∧ L v = (H : ℝ)) →
        H = F ∧ ∀ v ∈ D, L v < (F : ℝ)))
```

### `UnivariateML.Base7bNHyperResidue.all_free_rational_search`

**nhyper · all free · uniqueness.** The joined rational search gives all deterministic urns, excludes incompatible mixed zeros, and retains every feasible finite urn at the greatest rational slice height. [Lean source](../../lean/UnivariateML/Family/Base7bNHyperResidue.lean#L1155)

```lean
theorem all_free_rational_search {h : ℕ} (hh : 0 < h) (x : Fin h → ℕ) :
    let D := {v : ℕ × ℕ × ℕ | ∀ j, Discrete4NHyper.Compatible v.1 v.2.1 v.2.2 (x j)}
    let L := fun v : ℕ × ℕ × ℕ ↦ Discrete4NHyper.totalLikelihood x v.1 v.2.1 v.2.2
    let V := fun r : ℕ ↦ {z : ℝ | ∃ N m, (N, m, r) ∈ D ∧ z = L (N, m, r)}
    ((∃ i j, x i = 0 ∧ 0 < x j) → D = ∅) ∧
    (∀ c : ℕ, (∀ i, x i = c) →
      ((c, c, c) ∈ D ∧ IsMaxOn L D (c, c, c)) ∧
      ∀ v ∈ D, IsMaxOn L D v ↔ v.2.2 = c ∧ (c = 0 ∨ v.2.1 = v.1)) ∧
    (∀ i, (∀ j, x i ≤ x j) → 0 < x i → (∃ j, x i < x j) →
      let S := Finset.Icc 1 (x i)
      ∃ (H : ℕ → ℚ) (C : ℕ → ℕ) (r : ℕ), r ∈ S ∧ (∀ s ∈ S, H s ≤ H r) ∧
        (∀ s ∈ S, IsLUB (V s) (H s : ℝ)) ∧
        (∀ v ∈ D, IsMaxOn L D v ↔ v.2.2 ∈ S ∧ v.1 < C v.2.2 ∧ L v = (H r : ℝ)) ∧
        ((∃ v ∈ D, IsMaxOn L D v) ↔
          ∃ s ∈ S, H s = H r ∧ ∃ N m, N < C s ∧ (N, m, s) ∈ D ∧ L (N, m, s) = (H r : ℝ)))
```

### `UnivariateML.Base7bGPD.held_threshold_scale_uniqueness`

**gpd · mu+sigma · uniqueness.** All fitted shapes win at all-zero excesses. Every other compatible sample has exactly one maximizing shape. [Lean source](../../lean/UnivariateML/Family/Base7bGPD.lean#L198)

```lean
theorem held_threshold_scale_uniqueness {n : ℕ} (y : Fin n → ℝ)
    (hy : ∀ i, 0 ≤ y i) {s : ℝ} (hs : 0 < s) :
    let D := shapeDomain y s
    let L := logLik y s
    ((∀ i, y i = 0) → D = Ici (-1) ∧ ∀ ξ ∈ D, IsMaxOn L D ξ) ∧
    ((∃ i, 0 < y i) → ∃! ξ, ξ ∈ D ∧ IsMaxOn L D ξ)
```

### `UnivariateML.Base7bGPD.held_scale_uniqueness`

**gpd · sigma · uniqueness.** The exact uniform interval and constant ridge cover feasible uniform data. Beyond the held scale, a unique nonuniform fit occurs at the sample minimum. [Lean source](../../lean/UnivariateML/Family/Base7bGPD.lean#L223)

```lean
theorem held_scale_uniqueness {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {s : ℝ} (hs : 0 < s) {i j : Fin n}
    (hi : ∀ k, x i ≤ x k) (hj : ∀ k, x k ≤ x j) :
    let D := {p : ℝ × ℝ | p.2 ∈ shapeDomain (fun k ↦ x k - p.1) s}
    let L := fun p : ℝ × ℝ ↦ logLik (fun k ↦ x k - p.1) s p.2
    (x j - x i ≤ s → ∀ p, (p ∈ D ∧ IsMaxOn L D p) ↔
      (p.2 = -1 ∧ p.1 ∈ Icc (x j - s) (x i)) ∨
        (-1 ≤ p.2 ∧ p.1 = x i ∧ x i = x j)) ∧
    (s < x j - x i → (∃! p, p ∈ D ∧ IsMaxOn L D p) ∧
      ∀ p ∈ D, IsMaxOn L D p → p.1 = x i ∧ -1 < p.2)
```

### `UnivariateML.Base7bGPD.held_scale_boundary_classification`

**gpd · sigma · failure.** The held-scale boundary theorem covers finite attainment, receding location, nonuniform contact, uniform contact and all constant scaled-gap limits. [Lean source](../../lean/UnivariateML/Family/Base7bGPD.lean#L428)

```lean
theorem held_scale_boundary_classification {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {s : ℝ} (hs : 0 < s) :
    let D := fun m ↦ shapeDomain (fun i ↦ x i - m) s
    let L := fun m ↦ logLik (fun i ↦ x i - m) s
    (∃ m a, a ∈ D m ∧ ∀ q b, b ∈ D q → L q b ≤ L m a) ∧
    (∀ m a : ℕ → ℝ, (∀ᶠ t in atTop, a t ∈ D (m t)) →
      (Tendsto m atTop atBot → Tendsto (fun t ↦ L (m t) (a t)) atTop atBot) ∧
      (∀ B : ℝ, Tendsto a atTop atTop → (∀ᶠ t in atTop, B ≤ L (m t) (a t)) →
        ∃ M, (∀ i, x i = M) ∧ Tendsto m atTop (𝓝 M)) ∧
      (∀ q b : ℝ, Tendsto m atTop (𝓝 q) → Tendsto a atTop (𝓝 b) → -1 < b →
        (∃ j, 0 < x j - q ∧ s + b * (x j - q) = 0) →
          Tendsto (fun t ↦ L (m t) (a t)) atTop atBot)) ∧
    (∀ M : ℝ, (∀ i, x i = M) → ∀ m a : ℕ → ℝ, Tendsto a atTop atTop →
      let gap := fun t ↦ a t * ((M - m t) / s)
      (∀ v ≥ 0, Tendsto gap atTop (𝓝 v) →
        Tendsto (fun t ↦ L (m t) (a t)) atTop (𝓝 (-(n : ℝ) * (log s + log (1 + v))))) ∧
      (Tendsto gap atTop atTop → Tendsto (fun t ↦ L (m t) (a t)) atTop atBot)) ∧
    (∀ m a, a ∈ D m → L m a ≤ -(n : ℝ) * log s) ∧
    (∀ m, (∀ i, x i ∈ Icc m (m + s)) →
      (-1 : ℝ) ∈ D m ∧ L m (-1) = -(n : ℝ) * log s ∧
      (∀ᶠ a in 𝓝[>] (-1 : ℝ), a ∈ D m) ∧
      Tendsto (L m) (𝓝[>] (-1 : ℝ)) (𝓝 (-(n : ℝ) * log s)))
```

### `UnivariateML.Base7bTriangular.two_level_knot_comparison`

**triang · all free · uniqueness.** The relative heights of two fitted knots equal their count ordering. Balanced counts retain both knots. [Lean source](../../lean/UnivariateML/Family/Base7bTriangular.lean#L57)

```lean
theorem two_level_knot_comparison {n : ℕ} (x : Fin n → ℝ) {a b : ℝ} (hab : a < b)
    (hx : ∀ i, x i = a ∨ x i = b) (ha : ∃ i, x i = a) (hb : ∃ i, x i = b)
    {u v : ℝ × ℝ} (hu : u ∈ widthDomain x a) (hv : v ∈ widthDomain x b)
    (hmu : IsMaxOn (widthLikelihood x a) (widthDomain x a) u)
    (hmv : IsMaxOn (widthLikelihood x b) (widthDomain x b) v) :
    let A := (Finset.univ.filter (fun i ↦ x i = a)).card
    let B := (Finset.univ.filter (fun i ↦ x i = b)).card
    (widthLikelihood x a u ≤ widthLikelihood x b v ↔ A ≤ B)
```

### `UnivariateML.Base7bTriangular.extreme_knot_ratio_lt_one`

**triang · all free · uniqueness.** The extreme-versus-interior gradient equation forces the interior height ratio strictly below one in the algebraic comparison. [Lean source](../../lean/UnivariateML/Family/Base7bTriangular.lean#L159)

```lean
theorem extreme_knot_ratio_lt_one {I J : ℕ} (hI : 0 < I)
    {a b d : ℝ} (hd : 0 < d) (ha : d < a) (hb : 0 < b)
    (hscore : (I : ℝ) / a = (J : ℝ) / b) :
    ((b + d) / (a + b)) ^ (I + J) *
      (1 - d / a) ^ I * (1 + d / b) ^ J < 1
```

### `UnivariateML.Base7bBinomial.adjacent_profile_le_iff`

**binom · all free · uniqueness.** The sign of the outgoing binomial profile difference is the sign of the displayed adjacent integer comparison. [Lean source](../../lean/UnivariateML/Family/Base7bBinomial.lean#L57)

```lean
theorem adjacent_profile_le_iff {n N : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hmean : 0 < mean x) (hN : N ∈ feasible x) :
    let S : ℕ := ∑ i, x i
    let F := n * N - S
    let G := F + n
    profile x (N + 1) ≤ profile x N ↔
      N ^ (n * N) * G ^ G ≤
        (N + 1) ^ (n * N) * n ^ n * F ^ F * ∏ i, (N + 1 - x i)
```

### `UnivariateML.Base7bBinomial.neighbor_test_of_strict_middle`

**binom · all free · uniqueness.** Strict middle-point comparisons imply the two neighbor tests and rule out separated co-winners. The strict middle-point premise remains explicit. [Lean source](../../lean/UnivariateML/Family/Base7bBinomial.lean#L121)

```lean
theorem neighbor_test_of_strict_middle {n : ℕ} (x : Fin n → ℕ)
    (hmid : ∀ a b c : ℕ, a ∈ feasible x → a < b → b < c →
      min (profile x a) (profile x c) < profile x b) :
    (∀ N ∈ feasible x, IsMaxOn (profile x) (feasible x) N ↔
      profile x (N + 1) ≤ profile x N ∧
      ∀ M ∈ feasible x, M + 1 = N → profile x M ≤ profile x N) ∧
    (∀ a ∈ feasible x, ∀ c ∈ feasible x, a < c →
      IsMaxOn (profile x) (feasible x) a → IsMaxOn (profile x) (feasible x) c → c = a + 1)
```

### `UnivariateML.Base7bDGamma.two_cell_trace_arithmetic`

**dgamma · all free · uniqueness.** The proposed trace expression is below one for zero fraction one quarter through three quarters and separation at least 2^28. [Lean source](../../lean/UnivariateML/Family/Base7bDGamma.lean#L27)

```lean
theorem two_cell_trace_arithmetic {K : ℕ} (hK : 2 ^ 28 ≤ K)
    {w : ℝ} (hw₀ : 1 / 4 ≤ w) (hw₁ : w ≤ 3 / 4) :
    let m := (1 - w) * (K : ℝ)
    let δ := log (w + m) - (1 - w) * log (K : ℝ)
    let A := 1 / δ
    let B := max |1 / (K : ℝ) - 1 / (m + 1)| |1 / ((K : ℝ) + 1) - 1 / m|
    let G := (1 + (1 + A)⁻¹ ^ 2) / 2
    3 < δ ∧ A ∈ Set.Ioo 0 (1 / 3) ∧
      (w + (A ^ 2 / 4) * (1 - w) * B ^ 2) / G + A / (4 * m ^ 2) < 1
```

### `UnivariateML.Base7bDGamma.small_shape_score_pos_of_moment_bound`

**dgamma · all free · uniqueness.** The written lower bound based on the zero-cell moment implies a strictly positive small-shape score collar. [Lean source](../../lean/UnivariateML/Family/Base7bDGamma.lean#L107)

```lean
theorem small_shape_score_pos_of_moment_bound (P ψ : ℝ → ℝ)
    {m w : ℝ} (_hm : 0 < m) (hw : w < 1) (hψ : ContinuousAt ψ 1)
    (hbound : ∀ a > 0, log a - log (m + 1) +
      (1 - w * exp (a / m)) / a - ψ (1 + a) ≤ deriv P a) :
    ∀ᶠ a : ℝ in 𝓝[>] 0, 0 < deriv P a
```

### `UnivariateML.Base7bSkellam.finite_mean_line_candidates`

**skellam · all free · uniqueness.** Positive roots of the analytic mean-line score and the compatible Poisson endpoint form a finite list. Every highest candidate wins. [Lean source](../../lean/UnivariateML/Family/Base7bSkellam.lean#L760)

```lean
theorem finite_mean_line_candidates {n : ℕ} (hn : 0 < n) (x : Fin n → ℤ) :
    let d := (∑ i, (x i : ℝ)) / n
    let R := fun t : ℝ ↦ (t + max d 0, t + max (-d) 0)
    let G := fun (k : ℕ) (u : ℝ) ↦
      ∑' j : ℕ, u ^ j / (j.factorial : ℝ) / ((j + k).factorial : ℝ)
    let S := fun t : ℝ ↦ ∑ i, (((x i).natAbs : ℝ) - (2 * t + |d|) +
      2 * (t * (t + |d|)) * G ((x i).natAbs + 1) (t * (t + |d|)) /
        G (x i).natAbs (t * (t + |d|)))
    let P := fun t ↦ likelihood x (R t)
    let C := {t : ℝ | (0 < t ∧ S t = 0) ∨ (t = 0 ∧ 0 < P t)}
    C.Finite ∧ ∀ q, (q ∈ domain ∧ IsMaxOn (likelihood x) domain q) ↔
      ∃ t ∈ C, q = R t ∧ IsMaxOn P C t
```

### `UnivariateML.Base7bSkellam.one_magnitude_unique`

**skellam · all free · uniqueness.** Samples with both signs of one observed magnitude have a unique winner, including unequal sign counts. [Lean source](../../lean/UnivariateML/Family/Base7bSkellam.lean#L847)

```lean
theorem one_magnitude_unique {n k : ℕ} (hn : 0 < n) (x : Fin n → ℤ)
    (hx : ∀ i, x i = (k : ℤ) ∨ x i = -(k : ℤ))
    (hpos : ∃ i, 0 < x i) (hneg : ∃ i, x i < 0) :
    ∃! p, p ∈ domain ∧ IsMaxOn (likelihood x) domain p
```

### `UnivariateML.Base7bSkellam.all_free_candidate_uniqueness`

**skellam · all free · uniqueness.** The joined result covers the finite candidate list, constants, the origin on zeros, both signs of one magnitude and both Poisson supporting orientations. [Lean source](../../lean/UnivariateML/Family/Base7bSkellam.lean#L980)

```lean
theorem all_free_candidate_uniqueness {n : ℕ} (hn : 0 < n) (x : Fin n → ℤ) :
    let d := (∑ i, (x i : ℝ)) / n
    let R := fun t : ℝ ↦ (t + max d 0, t + max (-d) 0)
    let G := fun (k : ℕ) (u : ℝ) ↦
      ∑' j : ℕ, u ^ j / (j.factorial : ℝ) / ((j + k).factorial : ℝ)
    let S := fun t : ℝ ↦ ∑ i, (((x i).natAbs : ℝ) - (2 * t + |d|) +
      2 * (t * (t + |d|)) * G ((x i).natAbs + 1) (t * (t + |d|)) /
        G (x i).natAbs (t * (t + |d|)))
    let P := fun t ↦ likelihood x (R t)
    let C := {t : ℝ | (0 < t ∧ S t = 0) ∨ (t = 0 ∧ 0 < P t)}
    (C.Finite ∧ ∀ q, (q ∈ domain ∧ IsMaxOn (likelihood x) domain q) ↔
      ∃ t ∈ C, q = R t ∧ IsMaxOn P C t) ∧
    ((∃ c, ∀ i, x i = c) → ∃! p, p ∈ domain ∧ IsMaxOn (likelihood x) domain p) ∧
    ((∀ i, x i = 0) → ((0, 0) : ℝ × ℝ) ∈ domain ∧
      IsMaxOn (likelihood x) domain (0, 0) ∧
      ∀ p ∈ domain, IsMaxOn (likelihood x) domain p → p = (0, 0)) ∧
    ((∃ k : ℕ, (∀ i, x i = (k : ℤ) ∨ x i = -(k : ℤ)) ∧
      (∃ i, 0 < x i) ∧ (∃ i, x i < 0)) →
        ∃! p, p ∈ domain ∧ IsMaxOn (likelihood x) domain p) ∧
    (∀ z : Fin n → ℕ, let d₀ : ℝ := (∑ i, (z i : ℝ)) / n
      0 < d₀ →
      ((∀ t ≥ 0, (∑ i, poissonMass t (z i) / ((n : ℝ) * poissonMass d₀ (z i))) ≤ 1) ∨
        (∃ j : ℕ, ∀ i, z i = j ∨ z i = j + 1)) →
      ((x = fun i ↦ (z i : ℤ)) → (d₀, 0) ∈ domain ∧ IsMaxOn (likelihood x) domain (d₀, 0) ∧
        ∀ p ∈ domain, IsMaxOn (likelihood x) domain p → p = (d₀, 0)) ∧
      ((x = fun i ↦ -(z i : ℤ)) → (0, d₀) ∈ domain ∧ IsMaxOn (likelihood x) domain (0, d₀) ∧
        ∀ p ∈ domain, IsMaxOn (likelihood x) domain p → p = (0, d₀)))
```
