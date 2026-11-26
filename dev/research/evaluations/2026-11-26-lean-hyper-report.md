# TASK-371 — Hypergeometric scalar likelihoods and discrete uniform in Lean

The scalar likelihood and endpoint results are kernel-checked in [Hypergeometric.lean](../../lean/UnivariateML/Family/Hypergeometric.lean). The worktree is `codex/lean-hyper` at base `2d89fd4a`.

## Source gaps

- **The maximal-count atom applies to held draws or held successes.** Write `B = 2^53 - 1`. Holding `k = B` forces `N = B` and all-zero compatibility then forces `m = 0`. Holding `m = B` gives the swapped result. Holding only `N = B` leaves at least the distinct maximizing designs `(m,k) = (0,B)` and `(B,0)` for positive `B`. The R notes call both relevant holds “design counts,” although the dossier classifies `N` and `k` as designs. The regression test actually holds `m` or `k`. [Hyper dossier](../../../univariateML-cpp/include/cppunivariate/distributions/hyper/README.md#r-notes) · [R regression](../../../bindings/r/distributions/hyper/test.R#L222)
- **Per-observation equality is sufficient for an exact adjacent tie.** The necessary and sufficient condition compares the products across the sample. For `N = 5`, `k = 2`, and observations `0, 2`, the ratios from `m = 2` to `m = 3` are `1/3` and `3`. Lean proves equality and witnesses a non-unit factor. The fitter’s per-observation check cannot certify equality by this cancellation. The existing R regression records the comparison as unresolved. [Exact tie check](../../../univariateML-cpp/include/cppunivariate/distributions/hyper/mle.hpp#L777) · [R regression](../../../bindings/r/distributions/hyper/test.R#L235)
- **The population existence statement assumes feasibility and the unbounded parameter domain.** Each observation must be at most both held counts. For positive held counts and all-zero data, the likelihood increases to one on the unbounded ray. Truncating the ray at a feasible count cap makes that cap the unique mathematical maximizer. The fitter’s `AllZero` status describes the unbounded-model escape. The limit metadata lists only `m` under `fixed`, while its detection prose requires both `m` and `k` held. [Hyper dossier](../../../univariateML-cpp/include/cppunivariate/distributions/hyper/README.md) · [Scalar population route](../../../univariateML-cpp/include/cppunivariate/distributions/hyper/mle.hpp#L721)
- **The dossier’s face-stop sentence is stale.** The R notes say unresolved face comparisons stop without an estimate. The current Details and the fitter allow an uncertified finite witness when one is available. The older certificate report describes the earlier stop behavior. [Hyper dossier](../../../univariateML-cpp/include/cppunivariate/distributions/hyper/README.md#r-notes) · [Fitter fallback](../../../univariateML-cpp/include/cppunivariate/distributions/hyper/mle.hpp#L760) · [November 1 certificate report](2026-11-01-hyper-certs-report.md)
- **The discrete-uniform fit note’s “degenerate” wording is ambiguous.** Constant nonempty samples give valid one-point intervals, as the existence block, Details, implementation and Lean theorem agree. The fitter rejects empty samples and invalid or incompatible endpoints. [Discrete-uniform dossier](../../../univariateML-cpp/include/cppunivariate/distributions/dunif/README.md) · [Fitter](../../../univariateML-cpp/include/cppunivariate/distributions/dunif/mle.hpp)

## Verification

Both runs finished with exit code zero on the workstation through `/tmp/umvl-cpu/run`. The final build used one CPU and the existing Mathlib cache.

| Check from `dev/lean` | Result |
|---|---|
| `lake env lean UnivariateML/Family/Hypergeometric.lean` | Passed. The checker flagged a redundant tactic, which was removed before the final build. |
| `lake build UnivariateML +UnivariateML.Family.Hypergeometric` | Passed, 7,454 jobs. This built the library root and explicitly built the new module. Lake emitted non-failing style warnings, including whitespace and the scoped classical instance in the new module. |

The proofs contain no placeholders or added axioms. The coordinator’s root import remains pending. The exact statements concern the mathematical likelihood and leave certification of floating-point comparisons and classification of incidental joint-grid ties open. The swap theorem applies to a maximizer on a swap-invariant domain and identifies its distinct partner regardless of any further joint ties. [November 22 flag report](2026-11-22-hyper-flags-report.md)

## Effort by target

| Target | Proof and review effort |
|---|---|
| Scalar regimes | Largest share. Derived the adjacent mass identities, strict composition ordering, population sign crossing via concavity of the logarithm, first-crossing maxima, and fixed-margin limits via descending factorials. |
| Exact ties and symmetry | Cleared positive denominators for the three scalar routes, checked cancellation on a concrete sample, and transported maxima under the margin swap. |
| Capped atom | Used the support constraints at the cap and proved two distinct maxima when only population is held. |
| Discrete uniform | Encoded integer sample extrema and proved the strict ordering by interval width for each compatible held set. |
| Source review | Compared dossier metadata and prose against the fitters, R regressions, and earlier evaluation reports. CPU-slot waits dominated elapsed verification time. |

## Theorem statements

Hypergeometric observations are `x : Fin h → ℕ`, with success count `K`, population `N`, and draw count `n`. The discrete-uniform sample is `x : Fin h → ℤ`. In the copied statements, `h` is the implicit sample size from the corresponding namespace.

### Hypergeometric mass and symmetry

On the support, the factorial formula is the usual ratio of binomial coefficients.

```lean
theorem mass_choose {K N n x : ℕ} (hc : Compatible K N n x) :
    mass K N n x = (K.choose x : ℝ) * (N-K).choose (n-x) / N.choose n
```

Exchanging successes and draws preserves the mass, including outside the support.

```lean
theorem mass_swap (K N n x : ℕ) : mass K N n x = mass n N K x
```

The exchange preserves the product likelihood for every sample.

```lean
theorem likelihood_swap (K N n : ℕ) (x : Fin h → ℕ) :
    likelihood K N n x = likelihood n N K x
```

An off-diagonal maximizer on a swap-invariant search domain has a different maximizing partner at the same population.

```lean
theorem distinct_swap_maximizers (x : Fin h → ℕ) (D : Set (ℕ × ℕ × ℕ))
    (hD : ∀ {K N n}, (K,N,n) ∈ D → (n,N,K) ∈ D) {K N n : ℕ}
    (hp : (K,N,n) ∈ D) (hne : K ≠ n)
    (hm : IsMaxOn (fun p : ℕ × ℕ × ℕ ↦ likelihood p.1 p.2.1 p.2.2 x) D (K,N,n)) :
    (n,N,K) ∈ D ∧ (n,N,K) ≠ (K,N,n) ∧
      IsMaxOn (fun p : ℕ × ℕ × ℕ ↦ likelihood p.1 p.2.1 p.2.2 x) D (n,N,K)
```

### Adjacent ratios and equality

With population and draws held, the adjacent composition likelihood is the earlier likelihood times the explicit product ratio.

```lean
theorem composition_adjacent {K N n : ℕ} {x : Fin h → ℕ} (hn : n ≤ N)
    (hc : K ∈ compositionDomain N n x) (hd : K+1 ∈ compositionDomain N n x) :
    likelihood (K+1) N n x = likelihood K N n x * compositionRatio K N n x
```

With population and successes held, the adjacent draw likelihood uses the swapped composition ratio.

```lean
theorem draws_adjacent {K N n : ℕ} {x : Fin h → ℕ} (hK : K ≤ N)
    (hc : n ∈ compositionDomain N K x) (hd : n+1 ∈ compositionDomain N K x) :
    likelihood K N (n+1) x = likelihood K N n x * compositionRatio n N K x
```

With successes and draws held, the adjacent population likelihood is the earlier likelihood times the explicit population ratio.

```lean
theorem population_adjacent {K N n : ℕ} {x : Fin h → ℕ}
    (hc : N ∈ populationDomain K n x) :
    likelihood K (N+1) n x = likelihood K N n x * populationRatio K N n x
```

Adjacent feasible compositions tie exactly when the displayed integer products agree.

```lean
theorem composition_tie_iff {K N n : ℕ} {x : Fin h → ℕ} (hn : n ≤ N)
    (hc : K ∈ compositionDomain N n x) (hd : K+1 ∈ compositionDomain N n x) :
    likelihood (K+1) N n x = likelihood K N n x ↔
      (∏ i, (K+1)*(N+x i-K-n)) = ∏ i, (K+1-x i)*(N-K)
```

Adjacent feasible draw counts tie according to the swapped integer identity.

```lean
theorem draws_tie_iff {K N n : ℕ} {x : Fin h → ℕ} (hK : K ≤ N)
    (hc : n ∈ compositionDomain N K x) (hd : n+1 ∈ compositionDomain N K x) :
    likelihood K N (n+1) x = likelihood K N n x ↔
      (∏ i, (n+1)*(N+x i-n-K)) = ∏ i, (n+1-x i)*(N-n)
```

Adjacent feasible populations tie exactly when the displayed integer products agree.

```lean
theorem population_tie_iff {K N n : ℕ} {x : Fin h → ℕ}
    (hc : N ∈ populationDomain K n x) :
    likelihood K (N+1) n x = likelihood K N n x ↔
      (∏ _ : Fin h, (N+1-K)*(N+1-n)) = ∏ i, (N+1)*(N+1+x i-K-n)
```

The sample with observations zero and two has equal likelihood at compositions two and three, and at least one observation has a non-unit adjacent ratio.

```lean
theorem mixed_composition_tie :
    likelihood 3 5 2 (![0, 2] : Fin 2 → ℕ) = likelihood 2 5 2 ![0, 2] ∧
      (3 * (5+0-2-2) : ℕ) ≠ (3-0)*(5-2)
```

### Scalar maximizers and escape

When the held draw count is positive, the composition ratios strictly decrease across feasible adjacent steps. Swapping margins gives the draw-count result.

```lean
theorem composition_ratio_strict {K J N n : ℕ} {x : Fin h → ℕ} (hh : 0 < h)
    (hn : 0 < n) (hKJ : K < J)
    (hK : K ∈ compositionDomain N n x) (hK' : K+1 ∈ compositionDomain N n x)
    (hJ : J ∈ compositionDomain N n x) (hJ' : J+1 ∈ compositionDomain N n x) :
    compositionRatio J N n x < compositionRatio K N n x
```

With both margins positive, after a population ratio reaches one or below every later ratio is strictly below one.

```lean
theorem population_single_crossing {K n N J : ℕ} {x : Fin h → ℕ} (hh : 0 < h)
    (hK : 0 < K) (hn : 0 < n) (hc : N ∈ populationDomain K n x) (hNJ : N < J)
    (H : populationRatio K N n x ≤ 1) : populationRatio K J n x < 1
```

A nonempty feasible composition interval with positive held draws has a first maximizer. The only possible additional maximizer is its feasible successor at ratio one.

```lean
theorem composition_maximizers {N n : ℕ} (x : Fin h → ℕ) (hh : 0 < h)
    (hn : 0 < n) (hN : n ≤ N) (hne : (compositionDomain N n x).Nonempty) :
    ∃ k ∈ compositionDomain N n x, ∀ j,
      (j ∈ compositionDomain N n x ∧ IsMaxOn (fun K ↦ likelihood K N n x)
        (compositionDomain N n x) j) ↔
      j = k ∨ (j = k+1 ∧ k+1 ∈ compositionDomain N n x ∧ compositionRatio k N n x = 1)
```

A nonempty feasible draw interval with positive held successes has the same cardinality and adjacent tie rule.

```lean
theorem draws_maximizers {N K : ℕ} (x : Fin h → ℕ) (hh : 0 < h)
    (hK : 0 < K) (hN : K ≤ N) (hne : (compositionDomain N K x).Nonempty) :
    ∃ k ∈ compositionDomain N K x, ∀ j,
      (j ∈ compositionDomain N K x ∧ IsMaxOn (fun n ↦ likelihood K N n x)
        (compositionDomain N K x) j) ↔
      j = k ∨ (j = k+1 ∧ k+1 ∈ compositionDomain N K x ∧ compositionRatio k N K x = 1)
```

With zero draws and compatible data, every composition gives likelihood one. Swapping margins covers zero successes when draws are free.

```lean
theorem composition_flat {N K : ℕ} (x : Fin h → ℕ) (hK : K ≤ N) (hz : ∀ i, x i = 0) :
    likelihood K N 0 x = 1
```

On a compatible population ray either zero held margin makes the likelihood identically one.

```lean
theorem population_flat {K N n : ℕ} {x : Fin h → ℕ} (hc : N ∈ populationDomain K n x)
    (hz : K = 0 ∨ n = 0) : likelihood K N n x = 1
```

As population grows with both margins fixed, each mass converges to the corresponding mass of the point distribution at zero.

```lean
theorem population_mass_limit (K n x : ℕ) :
    Tendsto (fun N : ℕ ↦ mass K N n x) atTop (𝓝 (if x = 0 then 1 else 0))
```

The sample likelihood converges to one for all-zero data and zero otherwise.

```lean
theorem population_likelihood_limit (K n : ℕ) (x : Fin h → ℕ) :
    Tendsto (fun N ↦ likelihood K N n x) atTop (𝓝 (if (∀ i, x i = 0) then 1 else 0))
```

With compatible data containing a positive observation, the unrestricted population fit attains its maximum at one integer or at two consecutive integers tied by the ratio.

```lean
theorem population_maximizers {K n : ℕ} (x : Fin h → ℕ) (hh : 0 < h)
    (hK : 0 < K) (hn : 0 < n) (hs : ∀ i, x i ≤ K ∧ x i ≤ n)
    (hx : ∃ i, 0 < x i) :
    ∃ k ∈ populationDomain K n x, ∀ j,
      (j ∈ populationDomain K n x ∧ IsMaxOn (fun N ↦ likelihood K N n x)
        (populationDomain K n x) j) ↔
      j = k ∨ (j = k+1 ∧ populationRatio K k n x = 1)
```

With both margins positive and all observations zero, the likelihood strictly increases to its least upper bound of one, which no finite feasible population attains.

```lean
theorem population_zero_escape {K n : ℕ} (x : Fin h → ℕ) (hh : 0 < h)
    (hK : 0 < K) (hn : 0 < n) (hz : ∀ i, x i = 0) :
    StrictMonoOn (fun N ↦ likelihood K N n x) (populationDomain K n x) ∧
    Tendsto (fun N ↦ likelihood K N n x) atTop (𝓝 1) ∧
    IsLUB ((fun N ↦ likelihood K N n x) '' populationDomain K n x) 1 ∧
    ¬ ∃ N ∈ populationDomain K n x,
      IsMaxOn (fun N ↦ likelihood K N n x) (populationDomain K n x) N
```

On the feasible capped population ray, the cap is the unique maximizer in the positive-margin all-zero case.

```lean
theorem capped_zero_population_maximizer {K n B : ℕ} (x : Fin h → ℕ) (hh : 0 < h)
    (hK : 0 < K) (hn : 0 < n) (hz : ∀ i, x i = 0)
    (hB : B ∈ populationDomain K n x) (j : ℕ) :
    (j ∈ populationDomain K n x ∩ Iic B ∧ IsMaxOn (fun N ↦ likelihood K N n x)
      (populationDomain K n x ∩ Iic B) j) ↔ j = B
```

For compatible samples the unrestricted population fit fails to attain a maximum exactly in the positive-margin all-zero case.

```lean
theorem population_nonattainment_iff {K n : ℕ} (x : Fin h → ℕ) (hh : 0 < h)
    (hs : ∀ i, x i ≤ K ∧ x i ≤ n) :
    (¬ ∃ N ∈ populationDomain K n x,
      IsMaxOn (fun N ↦ likelihood K N n x) (populationDomain K n x) N) ↔
      0 < K ∧ 0 < n ∧ ∀ i, x i = 0
```

### Maximal-count atoms

Holding draws at the cap pins population to that cap and gives likelihood one only at zero successes.

```lean
theorem capped_held_draw_zero_atom {B K N : ℕ} (x : Fin h → ℕ) (hh : 0 < h)
    (hBN : B ≤ N) (hNB : N ≤ B) (hz : ∀ i, x i = 0) :
    likelihood K N B x = (if K = 0 then 1 else 0)
```

Holding successes at the cap pins population to that cap and gives likelihood one only at zero draws.

```lean
theorem capped_held_success_zero_atom {B n N : ℕ} (x : Fin h → ℕ) (hh : 0 < h)
    (hBN : B ≤ N) (hNB : N ≤ B) (hz : ∀ i, x i = 0) :
    likelihood B N n x = (if n = 0 then 1 else 0)
```

Holding only a positive population leaves distinct all-zero maximizing designs with either margin equal to zero.

```lean
theorem capped_population_zero_tie {B : ℕ} (x : Fin h → ℕ) (hB : 0 < B)
    (hz : ∀ i, x i = 0) :
    (0,B) ≠ (B,0) ∧
      IsMaxOn (fun p : ℕ × ℕ ↦ likelihood p.1 B p.2 x) univ (0,B) ∧
      IsMaxOn (fun p : ℕ × ℕ ↦ likelihood p.1 B p.2 x) univ (B,0)
```

### Discrete uniform

An interval contains every observation exactly when its lower endpoint is at most the sample minimum and its upper endpoint is at least the sample maximum.

```lean
theorem compatible_iff (x : Fin h → ℤ) (hh : 0 < h) (a b : ℤ) :
    compatible x a b ↔ a ≤ sampleMin x hh ∧ sampleMax x hh ≤ b
```

Exactly the compatible intervals have positive likelihood.

```lean
theorem likelihood_pos_iff (x : Fin h → ℤ) (hh : 0 < h) (a b : ℤ) :
    0 < likelihood x a b ↔ compatible x a b
```

The pair of sample extremes is the unique joint maximizer.

```lean
theorem joint_maximizer (x : Fin h → ℤ) (hh : 0 < h) (p : ℤ × ℤ) :
    IsMaxOn (fun q : ℤ × ℤ ↦ likelihood x q.1 q.2) univ p ↔
      p = (sampleMin x hh, sampleMax x hh)
```

An admissible held lower endpoint leaves the sample maximum as the unique maximizing upper endpoint.

```lean
theorem held_lower_maximizer (x : Fin h → ℤ) (hh : 0 < h) {a : ℤ}
    (ha : a ≤ sampleMin x hh) (b : ℤ) :
    IsMaxOn (likelihood x a) univ b ↔ b = sampleMax x hh
```

An admissible held upper endpoint leaves the sample minimum as the unique maximizing lower endpoint.

```lean
theorem held_upper_maximizer (x : Fin h → ℤ) (hh : 0 < h) {b : ℤ}
    (hb : sampleMax x hh ≤ b) (a : ℤ) :
    IsMaxOn (fun j ↦ likelihood x j b) univ a ↔ a = sampleMin x hh
```

When a held endpoint excludes an observation, every choice of the opposite endpoint has likelihood zero.

```lean
theorem incompatible_held_zero (x : Fin h → ℤ) (hh : 0 < h) {a b : ℤ}
    (H : sampleMin x hh < a ∨ b < sampleMax x hh) : likelihood x a b = 0
```
