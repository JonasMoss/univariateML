# Lean verification of the negative hypergeometric scalar MLE

TASK-333 · 2026-11-25 · worktree base `a8919f50`

Both integer single crossings, the complete maximizing sets, the held-black
escape classification, and the adjacent rational tie criteria are proved in
Lean. The standalone module check and the final library build passed on the
workstation. [Lean module](../../lean/UnivariateML/Family/NegHypergeometric.lean)

## Source check

**The scalar proofs hold. The opening summary needs an attainment qualifier.**
The held-black all-earliest case is itself nonflat and has an empty maximizing
set. The singleton-or-adjacent-pair conclusion applies to the nonflat scalar
cases that attain their supremum, as in the body of the source report. The
held-black zero-black case is flat and has the entire feasible ray as its
maximizing set. [Source math report](2026-11-25-nhyper-math-report.md)

In the held-white proof the old zero is in the common domain of both log
factors. The normalized difference has a maximum at that zero, where the later
factor is already strictly below one. This verifies the strict comparison
needed to exclude a second equality. In the held-black integral proof a
specific interval giving strictness is
`max(r,n) < z < r+n`. Its length is positive and the source's weight
function is the negative sample size throughout it. Lean obtains the same
strict comparison using the derivative of a scaled difference of logarithms
and comparing its increments. [Source math report](2026-11-25-nhyper-math-report.md)

**Fitter and dossier scope.** The fitter takes holds in `(m,N,r)`, so
holding `N-m` is a mathematical constraint outside its public hold interface.
The dossier's reference to a unique constant-sample maximizer at the count
ceiling means the case where the observed count itself equals the ceiling.
For a smaller positive constant count, the ceiling is one of several
maximizers. The code makes the condition explicit with
`out.unique = c.min == hyper_detail::kMaxCount`.
[Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/nhyper/README.md)
[Fitter](../../../univariateML-cpp/include/cppunivariate/distributions/nhyper/mle.hpp)

## Scope and proof choices

The sample `y : Fin h → ℕ` records black draws, and the observation in total
draws is `r + y i`. The fitted cases assume a nonempty sample and positive
`r`. Here `m` counts white balls and `n` counts black balls. The ratios
use the source coordinates `A = m+n+1` and `u = m+1-r`. The crossings
are proved on the feasible real ratio domains and then applied to the
integer rays. Adding the held count to the reported maximizing counts gives
the maximizing populations.

The held-white attainment proof uses a sufficient finite cutoff from
the sign of every factor. The held-black attainment proof uses convergence
to zero when any observation exceeds the earliest draw. Positivity and
that limit rule out increasing forever. The source's sharper bisection
brackets from its factor bound and geometric-mean bound are outside the
formal statements. The omitted binomial factors multiply the reduced
sample likelihood by a common positive constant at fixed `r`.

## Theorem statements and readings

The statements below are copied from `UnivariateML.NegHypergeometric`,
where the implicit sample length is `{h : ℕ}`.

### Likelihood and feasibility

The product of the stopped-draw masses is the reduced likelihood times a positive factor common to all urns.

```lean
theorem mass_product_eq (r : ℕ) (y : Fin h → ℕ) :
    ∃ c : ℝ, 0 < c ∧ ∀ m n, (∏ i, mass r m n (y i)) = c * likelihood r m n y
```

Every feasible urn has positive reduced likelihood.

```lean
theorem likelihood_pos {r m n : ℕ} (y : Fin h → ℕ) (hm : r ≤ m)
    (hn : ∀ i, y i ≤ n) : 0 < likelihood r m n y
```

The feasible black counts are precisely the integers at or above the largest observed black-draw count.

```lean
theorem white_feasible_iff (y : Fin h → ℕ) (n : ℕ) :
    n ∈ Ici (maxBlackDraws y) ↔ ∀ i, y i ≤ n
```

Adding one black ball multiplies the reduced likelihood by the held-white ratio.

```lean
theorem white_adjacent {r m n : ℕ} (y : Fin h → ℕ) (_hr : 0 < r) (hm : r ≤ m)
    (hn : ∀ i, y i ≤ n) :
    likelihood r m (n+1) y = likelihood r m n y * whiteRatio r m (m+n+1) y
```

Adding one white ball multiplies the reduced likelihood by the held-black ratio.

```lean
theorem black_adjacent {r m n : ℕ} (y : Fin h → ℕ) (_hr : 0 < r) (hm : r ≤ m)
    (hn : ∀ i, y i ≤ n) :
    likelihood r (m+1) n y = likelihood r m n y * blackRatio r n (m+1-r) y
```

### Held white count

Once the ratio is at most one, every later ratio is strictly less than one. This includes the case where the held white count equals the stopping count.

```lean
theorem white_single_crossing (hh : 0 < h) (y : Fin h → ℕ) {r m A B : ℝ}
    (hr : 0 < r) (hm : r ≤ m) (hA : m < A) (hAB : A < B)
    (hy : ∀ i, (y i : ℝ) < A-m) (hle : whiteRatio r m A y ≤ 1) :
    whiteRatio r m B y < 1
```

The first nonpositive comparison exists. Its index is the smaller maximizer. The successor is also a maximizer exactly when the ratio at that index equals one. This characterizes every feasible maximizer.

```lean
theorem white_maximizers {r m : ℕ} (hh : 0 < h) (y : Fin h → ℕ)
    (hr : 0 < r) (hm : r ≤ m) :
    ∃ k ≥ maxBlackDraws y, whiteRatio r m (m+k+1) y ≤ 1 ∧
      (∀ j, maxBlackDraws y ≤ j → j < k → 1 < whiteRatio r m (m+j+1) y) ∧
      ∀ j, IsMaxOn (fun n ↦ likelihood r m n y) (Ici (maxBlackDraws y)) j ∧
        j ∈ Ici (maxBlackDraws y) ↔
        j = k ∨ (j = k+1 ∧ whiteRatio r m (m+k+1) y = 1)
```

### Held black count

With black balls present, a ratio at or below one is followed only by ratios strictly below one.

```lean
theorem black_single_crossing (hh : 0 < h) (y : Fin h → ℕ) {r n u v : ℝ}
    (hr : 0 < r) (hn : 0 < n) (hu : 0 < u) (huv : u < v)
    (hy : ∀ i, (y i : ℝ) ≤ n) (hle : blackRatio r n u y ≤ 1) :
    blackRatio r n v y < 1
```

If some observation includes a black draw, the first nonpositive comparison exists. It and its successor exhaust the possible maximizers, with the successor included exactly at equality.

```lean
theorem black_maximizers {r n : ℕ} (hh : 0 < h) (y : Fin h → ℕ)
    (hr : 0 < r) (hn : ∀ i, y i ≤ n) (hy : ∃ i, 0 < y i) :
    ∃ k ≥ r, blackRatio r n (k+1-r) y ≤ 1 ∧
      (∀ j, r ≤ j → j < k → 1 < blackRatio r n (j+1-r) y) ∧
      ∀ j, IsMaxOn (fun m ↦ likelihood r m n y) (Ici r) j ∧ j ∈ Ici r ↔
        j = k ∨ (j = k+1 ∧ blackRatio r n (k+1-r) y = 1)
```

With zero black balls, every feasible white count gives likelihood one.

```lean
theorem black_flat {r : ℕ} (y : Fin h → ℕ) (hy : ∀ i, y i = 0) {m : ℕ}
    (hm : r ≤ m) : likelihood r m 0 y = 1
```

As the white count grows at fixed black count, the stopped-draw mass converges pointwise to the point mass at the earliest draw.

```lean
theorem black_mass_pointwise_limit (r n y : ℕ) :
    Tendsto (fun m : ℕ ↦ mass r m n y) atTop (𝓝 (if y = 0 then 1 else 0))
```

The reduced sample likelihood tends to one when all observations are earliest draws and to zero otherwise.

```lean
theorem black_likelihood_limit {r n : ℕ} (y : Fin h → ℕ) :
    Tendsto (fun m : ℕ ↦ likelihood r m n y) atTop
      (𝓝 (if (∀ i, y i = 0) then 1 else 0))
```

With black balls present and only earliest draws observed, the likelihood strictly increases to its unattained supremum of one. The supremum bound uses `Generic.limit_le_sSup`.

```lean
theorem black_escape {r n : ℕ} (hh : 0 < h) (y : Fin h → ℕ)
    (hr : 0 < r) (hn : 0 < n) (hy : ∀ i, y i = 0) :
    StrictMonoOn (fun m ↦ likelihood r m n y) (Ici r) ∧
    Tendsto (fun m ↦ likelihood r m n y) atTop (𝓝 1) ∧
    sSup ((fun m ↦ likelihood r m n y) '' Ici r) = 1 ∧
    ¬ ∃ m ∈ Ici r, IsMaxOn (fun m ↦ likelihood r m n y) (Ici r) m
```

For a feasible sample, the held-black ray lacks a finite maximizer exactly when black balls are present and every observation stops at the earliest draw.

```lean
theorem black_nonattainment_iff {r n : ℕ} (hh : 0 < h) (y : Fin h → ℕ)
    (hr : 0 < r) (hn : ∀ i, y i ≤ n) :
    (¬ ∃ m ∈ Ici r, IsMaxOn (fun m ↦ likelihood r m n y) (Ici r) m) ↔
      0 < n ∧ ∀ i, y i = 0
```

### Exact adjacent ties

For adjacent feasible black counts, equality of likelihoods is equivalent to equality of the two natural-number products shown.

```lean
theorem white_tie_iff {r m n : ℕ} (y : Fin h → ℕ) (hr : 0 < r)
    (hm : r ≤ m) (hn : ∀ i, y i ≤ n) :
    likelihood r m (n+1) y = likelihood r m n y ↔
      (∏ i, (n+1)*(m+n+1-(r+y i))) = ∏ i, (m+n+1)*(n+1-y i)
```

For adjacent feasible white counts, equality of likelihoods is equivalent to equality of the two natural-number products shown.

```lean
theorem black_tie_iff {r m n : ℕ} (y : Fin h → ℕ) (hr : 0 < r)
    (hm : r ≤ m) (hn : ∀ i, y i ≤ n) :
    likelihood r (m+1) n y = likelihood r m n y ↔
      (∏ i, (m+1)*(m+n+1-(r+y i))) = ∏ _ : Fin h, (m+1-r)*(m+n+1)
```

## Effort by target

| Target | Proof work |
|---|---|
| 1. Held-white crossing and attainment | Substantial. The derivative comparison uses two positive denominator quotients and monotonicity on either side of the old zero. A shared integer-ray argument then transfers the first ratio crossing to the full maximizing set. |
| 2. Held-black crossing and exact escape | Largest share. The crossing uses a strict comparison of log increments. Separate falling-factorial limits in the stopping count and black-draw count prove the point-mass limit and support the attainment and nonattainment branches. |
| 3. Rational tie criterion | Moderate. Both adjacent recurrences follow from Mathlib's descending-factorial shift identity. Positive denominators allow cancellation, and natural-number casts identify the exact integer products. |
| 4. Source audit | Checked the feasible domains, strict inequalities, degenerate cases and the distinction between scalar mathematical holds and the public fitter interface. |

## Final verification and remaining work

Both checks ran sequentially in the foreground from `dev/lean` on the
workstation through `/tmp/umvl-cpu/run` and `nice -n 10`.

- `/home/jonas/.elan/bin/lake env lean UnivariateML/Family/NegHypergeometric.lean`
  exited zero without warnings. The new file contains no unfinished proofs
  or added axioms.
- `/home/jonas/.elan/bin/lake build` exited zero and reported
  `Build completed successfully (7396 jobs).` It emitted existing lint
  warnings in other family modules. The build reused Mathlib.

There are no open proof obligations among the requested scalar targets.
The coordinator owns the root import. The standalone check therefore
verifies the new module, and the default build verifies the current
library import graph. Joint estimation, the zero stopping-count model,
and floating-point certification are outside these scalar theorems.
