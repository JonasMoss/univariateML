# Exponential-family MLE existence and CMP

**TASK-336 · 2026-11-25**

Lean proves the finite-support relative-interior criterion, with unique natural
parameters under full affine span. For countable CMP it proves nonattainment on
adjacent samples that contain a nonzero observation, including positive constant
samples. It also identifies every maximizer on all-zero samples.

## Dossier comparison

- The dossier's phrase “samples supported on two adjacent integers” needs
  an explicit exclusion for all-zero data if “supported on” denotes containment.
  All-zero data lie on the adjacent edge from zero to one, and the full family
  attains the maximum at its separate rate-zero atom. The fitter makes this
  distinction by handling all-zero data before checking the sample range.
  The mathematical joint-fit claims agree under that reading.
  [CMP dossier](../../../univariateML-cpp/include/cppunivariate/distributions/cmp/README.md)
  [CMP fitter](../../../univariateML-cpp/include/cppunivariate/special/cmp.hpp#L449)
- The dossier gives unbounded count support, while the fitter rejects any
  observation above 2,147,483,647 at input validation. The Lean statements use
  the exact model on all natural counts.
  [CMP dossier](../../../univariateML-cpp/include/cppunivariate/distributions/cmp/README.md)
  [Fitter input gate](../../../univariateML-cpp/include/cppunivariate/special/cmp.hpp#L457)

## Finite-support statements

The support index is a nonempty finite type and every indexed carrier weight is
positive. A support point with zero weight can be omitted from that type.
The natural parameter and statistic are in a real inner product space.
The two attainment statements also assume finite dimension. The domain
is `univ`, and `logLik` is the linear term at the supplied mean minus the
log-partition function. The mean can be any vector.

The finite theorem uses `intrinsicInterior`, Mathlib's relative interior.
Full affine span gives ordinary interior and identifies the natural parameter.
For a constant statistic equal to the observed mean, the objective is constant
over the entire natural-parameter space. This is why uniqueness of the natural
parameter needs the spanning condition. The finite index assumption specifies
finite support, which is stronger than finiteness of the total carrier mass.
[Barndorff-Nielsen, *Information and Exponential Families*](https://onlinelibrary.wiley.com/doi/book/10.1002/9781118857281)
[Geyer, §3.3](https://www.stat.umn.edu/geyer/gdor/phase3.pdf)

The concavity proof applies the convexity of the exponential to normalized
log weights. Strictness follows when some normalized log weight differs.
For attainment, recenter the support at the observed mean and project onto
the span of the centered statistics. A ball in the centered hull gives a
linear lower bound for the log partition in the parameter norm on that span.
Continuity and coercivity give a minimum through `Generic.Escape`.
Orthogonal projection preserves the centered log partition. Outside the hull,
strict separation gives an improving direction. At a relative boundary point,
a nonzero supporting functional on the span gives weak inequalities with a
strict inequality at some support point.
[Finite-family module](../../lean/UnivariateML/Generic/ExpFamily.lean)

`concave_logLik` gives concavity over the entire natural-parameter space.

```lean
theorem concave_logLik (T : ι → E) {h : ι → ℝ} (hh : ∀ i, 0 < h i) (m : E) :
    ConcaveOn ℝ univ (logLik T h m)
```

`strictConcave_logLik` gives strict concavity when the sufficient statistics affinely span the natural-parameter space.

```lean
theorem strictConcave_logLik (T : ι → E) {h : ι → ℝ} (hh : ∀ i, 0 < h i)
    (hspan : affineSpan ℝ (range T) = ⊤) (m : E) :
    StrictConcaveOn ℝ univ (logLik T h m)
```

`unique_mle` identifies any two maximizing natural parameters under the same spanning condition.

```lean
theorem unique_mle (T : ι → E) {h : ι → ℝ} (hh : ∀ i, 0 < h i)
    (hspan : affineSpan ℝ (range T) = ⊤) {m θ φ : E}
    (hθ : IsMaxOn (logLik T h m) univ θ) (hφ : IsMaxOn (logLik T h m) univ φ) :
    θ = φ
```

`improves_in_direction` shows that adding a direction which pairs nonpositively with every centered statistic and strictly negatively with at least one increases the objective at every parameter.

```lean
theorem improves_in_direction (T : ι → E) {h : ι → ℝ} (hh : ∀ i, 0 < h i)
    (m θ d : E) (hle : ∀ i, ⟪d, T i - m⟫ ≤ 0) (hlt : ∃ i, ⟪d, T i - m⟫ < 0) :
    logLik T h m θ < logLik T h m (θ + d)
```

`mle_exists_iff` characterizes attainment by membership of the observed statistic mean in the relative interior of the convex hull.

```lean
theorem mle_exists_iff (T : ι → E) {h : ι → ℝ} (hh : ∀ i, 0 < h i) (m : E) :
    (∃ θ, IsMaxOn (logLik T h m) univ θ) ↔
      m ∈ intrinsicInterior ℝ (convexHull ℝ (range T))
```

`mle_existsUnique_iff` gives exactly one maximizing natural parameter at every relative-interior mean when the support affinely spans the space.

```lean
theorem mle_existsUnique_iff (T : ι → E) {h : ι → ℝ} (hh : ∀ i, 0 < h i)
    (hspan : affineSpan ℝ (range T) = ⊤) (m : E) :
    (∃! θ, IsMaxOn (logLik T h m) univ θ) ↔
      m ∈ intrinsicInterior ℝ (convexHull ℝ (range T))
```

## CMP statements

The canonical parameter is (log rate, negative dispersion). The countable
log-likelihood takes (log rate, dispersion) in `domain`, which includes zero
dispersion when the log rate is negative. The full likelihood uses
`rateDomain` in the original coefficients and includes the zero-rate atom.

The summability proof uses the adjacent-weight ratio. At positive dispersion
the ratio is eventually at most exp(−1). At zero dispersion it is the
geometric ratio. The slopes of the log factorial between consecutive counts
are strictly increasing, so a chord through consecutive counts has a strictly
positive gap at every other count. Increasing dispersion in the chord direction
preserves the contributions of the observed counts after removing a common
factor and strictly decreases the normalized partition sum. The per-observation
log-likelihood is bounded by zero. This gives a real, unattained supremum
without a cutoff or a summability assumption. The separate probability
calculation at rate zero gives the full-domain results.
[CMP module](../../lean/UnivariateML/Family/CMP.lean)

`weight_eq_exp_inner` expresses the CMP weight as the exponential of the product pairing of the canonical parameter and the sufficient statistic.

```lean
theorem weight_eq_exp_inner (lam ν : ℝ) (k : ℕ) :
    weight (log lam) ν k = exp ⟪naturalParameter lam ν, statistic k⟫
```

`summable_weight` proves convergence of the positive-rate normalizer on the whole modeled domain, including the geometric member.

```lean
theorem summable_weight {η ν : ℝ} (h : 0 < ν ∨ ν = 0 ∧ η < 0) :
    Summable (weight η ν)
```

`adjacent_supporting_line` puts every log factorial on or above the chord through consecutive counts, with equality exactly at the endpoints.

```lean
theorem adjacent_supporting_line (c k : ℕ) :
    logFactorial c + log ((c : ℝ) + 1) * ((k : ℝ) - c) ≤ logFactorial k ∧
    (logFactorial c + log ((c : ℝ) + 1) * ((k : ℝ) - c) = logFactorial k ↔
      k = c ∨ k = c + 1)
```

`adjacent_improves` finds a strict improvement to every positive-rate fit for a sample on an adjacent edge. The witness adds one to dispersion and adds the chord slope to log rate.

```lean
theorem adjacent_improves {n c : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∀ i, x i = c ∨ x i = c + 1)
    (p : ℝ × ℝ) (hp : p ∈ domain) :
    ∃ q ∈ domain, logLik x p < logLik x q
```

`adjacent_unattained_supremum` gives a finite least upper bound for positive-rate log likelihoods and puts every feasible likelihood strictly below it.

```lean
theorem adjacent_unattained_supremum {n c : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∀ i, x i = c ∨ x i = c + 1) :
    IsLUB (logLik x '' domain) (sSup (logLik x '' domain)) ∧
      ∀ p ∈ domain, logLik x p < sSup (logLik x '' domain)
```

`adjacent_mean_mem_frontier` places the empirical mean sufficient statistic on the boundary of the convex hull of the countable statistic support.

```lean
theorem adjacent_mean_mem_frontier {n c : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∀ i, x i = c ∨ x i = c + 1) :
    ((n : ℝ)⁻¹ • ∑ i, statistic (x i)) ∈ frontier (convexHull ℝ (range statistic))
```

`all_zero_maximizers` identifies exactly the rates that maximize the full likelihood on all-zero data. Every nonnegative finite dispersion is feasible at rate zero.

```lean
theorem all_zero_maximizers {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) (hx : ∀ i, x i = 0)
    (p : ℝ × ℝ) (hp : p ∈ rateDomain) :
    IsMaxOn (fun q : ℝ × ℝ ↦ likelihood x q.1 q.2) rateDomain p ↔ p.1 = 0
```

`adjacent_full_unattained_supremum` gives a real supremum with no maximizing pair in the full family for nonzero samples on an adjacent edge. This includes a positive constant sample by taking the constant as the lower endpoint.

```lean
theorem adjacent_full_unattained_supremum {n c : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∀ i, x i = c ∨ x i = c + 1) (hx0 : ∃ i, x i ≠ 0) :
    IsLUB ((fun p : ℝ × ℝ ↦ likelihood x p.1 p.2) '' rateDomain)
      (sSup ((fun p : ℝ × ℝ ↦ likelihood x p.1 p.2) '' rateDomain)) ∧
    ¬ ∃ p ∈ rateDomain, IsMaxOn (fun q : ℝ × ℝ ↦ likelihood x q.1 q.2) rateDomain p
```

`truncated_adjacent_not_mem_intrinsicInterior` uses the finite existence theorem to exclude the entire supporting line from the relative interior of the hull for a truncation containing both counts and at least three support points.

```lean
theorem truncated_adjacent_not_mem_intrinsicInterior {K c : ℕ} (hK : 2 ≤ K)
    (hc : c + 1 ≤ K) (u : ℝ) :
    (1 - u) • statistic c + u • statistic (c + 1) ∉
      intrinsicInterior ℝ (convexHull ℝ (range (fun k : Fin (K + 1) ↦ statistic k)))
```

## Open extensions

- **Generic countable-support criterion.** A general version needs explicit
  conditions on the partition domain and a proof that the candidate delivered
  by convex duality lies in the modeled parameter domain, including any
  admissible boundary. The specialized CMP summability and nonattainment
  proofs are complete.
- **Remaining CMP regimes.** A full classification would need the
  fixed-mean profile and justification of its derivatives under the infinite
  sum, the lower-edge and geometric moment limits, and the corresponding
  boundary optimality comparison. Held-parameter fits require their own
  one-dimensional score and endpoint arguments.
- **Values and limit laws.** The formal statements give a real least upper
  bound and strict nonattainment. Identifying its exact value and the
  limiting probability law requires passing to the limit in the normalized
  weights along a path chosen for the empirical adjacent proportions, or
  along a direction exposing a single count for a positive constant sample.

## Verification

All runs were on the workstation through the single-slot CPU lock with
`nice -n 10`, from `dev/lean/`.

| Check | Result |
| --- | --- |
| `lake env lean UnivariateML/Generic/ExpFamily.lean` | Passed without diagnostics |
| `lake env lean UnivariateML/Family/CMP.lean` | Passed without diagnostics |
| `lake build +UnivariateML.Generic.ExpFamily +UnivariateML.Family.CMP` | Passed, 6,565 jobs in the build graph |
| `lake build` | Passed, 7,402 jobs in the build graph. Existing GPD, Weibull, Fréchet, Discrete, Kumaraswamy and LomaxTwoPoint modules emitted lint warnings. |

The explicit module build verifies the two new modules independently of
the umbrella import reserved for the coordinator. Mathlib's prebuilt artifacts
were reused. Both proofs compile with no admitted lemmas.

## Proof effort

| Target | Relative effort | Main work |
| --- | --- | --- |
| 1. Finite support | High | Normalized-weight convexity, affine spanning, relative-interior separation and coercivity after projection. |
| 2. CMP | High | Infinite-series normalization, discrete chord inequalities, strict likelihood improvement, and the separate zero-rate atom. The finite truncation reuses the generic criterion. |
| 3. Dossier comparison | Low | Matched the exact claims to the all-zero and adjacent branches and inspected the fitter's input guard. |

The final checks ran sequentially and included waits for the shared CPU lock.
