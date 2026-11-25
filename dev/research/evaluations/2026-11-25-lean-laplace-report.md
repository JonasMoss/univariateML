# Lean evaluation of the Laplace maximizer sets

TASK-298. All four targets are proved in exact real arithmetic for nonempty
samples. The statements below are in `UnivariateML.Laplace`. The function
`orderStat` uses Mathlib's sorting permutation with zero-based ranks.
The joint parameter order is location, then scale.
[Lean module](../../lean/UnivariateML/Family/Laplace.lean)

## Dossier findings

**The even-sample sentence in Details needs an existence qualifier for a
joint fit.** The interval includes both central order statistics. Every
location in it can maximize the likelihood with scale held positive. With
scale free, the same interval describes the joint maximizing locations
only for nonconstant samples. The dossier's existence and collapse
metadata already state the constant-sample exception correctly. There is
no mathematical discrepancy with those statements.
[Laplace dossier](../../../univariateML-cpp/include/cppunivariate/distributions/laplace/README.md)

The joint theorem gives a closed location interval at one scale. This
matches the fitter's box on a successful non-unique fit. The fitter
chooses the upper median as its representative and marks the fitted scale
axis as rounded when scale is free. The Lean box describes the exact
real-valued optimizer and leaves floating-point evaluation and the
rounded-axis representation outside the proof.
[Laplace fitter](../../../univariateML-cpp/include/cppunivariate/distributions/laplace/mle.hpp)
[Maximizer set](../../../univariateML-cpp/include/cppunivariate/core/maximizer_set.hpp)

## 1. Held scale

```lean
theorem heldScale (hn : 0 < n) (x : Fin n → ℝ) {b : ℝ} (hb : 0 < b) :
    {μ | IsMaxOn (fun t ↦ logLik x t b) univ μ} = medianSet x hn
```

With positive scale fixed, a location maximizes the log-likelihood exactly when it belongs to the median set.

```lean
theorem medianSet_odd (hn : 0 < n) (x : Fin n → ℝ) {m : ℕ} (h : n = 2 * m + 1) :
    medianSet x hn = {orderStat x ⟨m, by omega⟩}
```

An odd sample has one median, the order statistic at one-based rank `m+1`.

```lean
theorem medianSet_even (hn : 0 < n) (x : Fin n → ℝ) {m : ℕ} (h : n = 2 * m) :
    medianSet x hn = Icc (orderStat x ⟨m - 1, by omega⟩) (orderStat x ⟨m, by omega⟩)
```

An even sample has the closed median interval between one-based ranks `m` and `m+1`, including a singleton if those values coincide.

This took the most proof work. Pairing each sorted observation with its
reverse rank reduces the argument to the distance between two endpoints.
The central interval lies inside every such pair's interval, and one pair
has exactly those endpoints. Leaving the central interval therefore
strictly increases at least one summand. Reindexing the sum by the two
permutations accounts for the factor of two.

## 2. Held location

```lean
theorem heldLocation (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ} (hd : 0 < meanAbsDev x μ) :
    {b ∈ Ioi 0 | IsMaxOn (logLik x μ) (Ioi 0) b} = {meanAbsDev x μ}
```

With location fixed and positive mean absolute deviation, that deviation is the only maximizing positive scale.

```lean
theorem heldLocation_unbounded (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ}
    (hd : meanAbsDev x μ = 0) :
    ∀ M : ℝ, ∃ b > 0, M < logLik x μ b
```

At zero deviation, for every real threshold a positive scale gives a log-likelihood above it.

```lean
theorem meanAbsDev_zero_iff (hn : 0 < n) (x : Fin n → ℝ) (μ : ℝ) :
    meanAbsDev x μ = 0 ↔ ∀ i, x i = μ
```

Deviation at the fixed location vanishes exactly when all observations equal that location.

This took a strict comparison using `log r < r - 1` for positive
`r ≠ 1`, applied to the ratio of the deviation to the candidate scale.
For zero deviation, an explicit exponential choice of scale exceeds any
given bound.

## 3. Joint maximizers and collapse

```lean
theorem meanAbsDev_eq_of_mem_medianSet (hn : 0 < n) {x : Fin n → ℝ} {μ ν : ℝ}
    (hμ : μ ∈ medianSet x hn) (hν : ν ∈ medianSet x hn) :
    meanAbsDev x μ = meanAbsDev x ν
```

The mean absolute deviation has the same value at every median.

```lean
theorem joint (hn : 0 < n) {x : Fin n → ℝ}
    (hd : 0 < meanAbsDev x (lowerMedian x hn)) :
    jointMaximizers x = medianSet x hn ×ˢ {meanAbsDev x (lowerMedian x hn)}
```

If the minimum deviation is positive, all joint maximizers form the product of the median interval and the singleton containing that deviation.

```lean
theorem joint_graph (hn : 0 < n) {x : Fin n → ℝ}
    (hd : 0 < meanAbsDev x (lowerMedian x hn)) :
    jointMaximizers x = {p | p.1 ∈ medianSet x hn ∧ p.2 = meanAbsDev x p.1}
```

Equivalently, a pair maximizes the joint log-likelihood exactly when its location is a median and its scale is the deviation at that location.

```lean
theorem minMeanAbsDev_zero_iff (hn : 0 < n) (x : Fin n → ℝ) :
    meanAbsDev x (lowerMedian x hn) = 0 ↔ ∃ c : ℝ, ∀ i, x i = c
```

The deviation at the lower median is zero exactly for a constant sample.

```lean
theorem joint_unbounded (hn : 0 < n) {x : Fin n → ℝ}
    (hd : meanAbsDev x (lowerMedian x hn) = 0) :
    ∀ M : ℝ, ∃ p ∈ domain, M < logLik x p.1 p.2
```

If the minimum deviation is zero, the joint log-likelihood is unbounded above.

```lean
theorem joint_exists_iff (hn : 0 < n) (x : Fin n → ℝ) :
    (jointMaximizers x).Nonempty ↔ ¬ ∃ c : ℝ, ∀ i, x i = c
```

A global maximizer with positive scale exists exactly for nonconstant samples.

This took the next largest share of proof work. A joint maximizer
must maximize along both coordinate slices. For the converse, changing an
arbitrary location to a median at the same scale can only improve the
likelihood, and optimizing scale at that median gives the joint maximum.
The held-location escape at the lower median handles the zero-deviation
case.

## 4. Cardinality

```lean
theorem joint_unique_iff (hn : 0 < n) {x : Fin n → ℝ}
    (hd : 0 < meanAbsDev x (lowerMedian x hn)) :
    (∃! p, p ∈ jointMaximizers x) ↔ lowerMedian x hn = upperMedian x hn
```

On the domain of joint existence, there is exactly one maximizing pair if and only if the two central values used by the fitter agree.

```lean
theorem joint_unique_iff_odd_or_tied (hn : 0 < n) {x : Fin n → ℝ}
    (hd : 0 < meanAbsDev x (lowerMedian x hn)) :
    (∃! p, p ∈ jointMaximizers x) ↔
      Odd n ∨ ∃ (m : ℕ) (hm : n = 2 * m),
        orderStat x ⟨m - 1, by omega⟩ = orderStat x ⟨m, by omega⟩
```

On that same domain, uniqueness is equivalent to odd sample size or equality of the two central order statistics in an even sample.

This was a short consequence of the segment theorem. Either endpoint
gives a maximizer, so uniqueness forces their locations to agree.
When they agree, the segment is a singleton. The parity statement
then follows from the two central rank formulas.

## Verification and open items

Both final runs passed on the workstation through the single-slot CPU wrapper.

- `lake env lean UnivariateML/Family/Laplace.lean` finished without warnings.
- `lake build UnivariateML UnivariateML/Family/Laplace.lean` finished with
  `Build completed successfully (6602 jobs).` It reused the Mathlib cache
  and emitted two existing long-line warnings in `Weibull.lean` at lines
  639 and 675. The Laplace module was built explicitly while the library
  root kept its existing imports.

All four mathematical targets are closed. The Laplace proofs contain no
`sorry`, `admit`, `axiom`, or `native_decide`. Adding the root
import remains the coordinator's integration step.
