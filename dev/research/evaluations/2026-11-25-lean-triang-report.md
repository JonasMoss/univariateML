# Lean evaluation of the triangular mode MLE

TASK-329 proves the fixed-endpoint results in exact real arithmetic for a
nonempty sample. The statements are in `UnivariateML.Triangular`, with
`{n : ℕ}` and `{a b : ℝ}` implicit. The likelihood factors out the common
peak height raised to the sample size. The sorting permutation gives
zero-based ranks and retains repeated observations.
[Lean module](../../lean/UnivariateML/Family/Triangular.lean)

## Endpoint qualification and source scope

**The unconditional containment requested for samples in the closed interval
is false.** If the sample contains both endpoints, every feasible mode has
zero likelihood. For example, with sample `(0, 1)` on `[0, 1]`, even the
unobserved mode `1/2` is an algebraic maximizer. The fitter reports
`Incompatible` in this case, as the dossier specifies. On compatible data,
the result is stronger than containment in observations plus endpoints.
Every maximizer is an observation, and a single observed endpoint forces
the mode there. Strict log-likelihood convexity applies to interior samples.
With endpoint observations, intervals of zero likelihood have extended
log-likelihood minus infinity. The dossier’s convexity sentence is read in
the regime of positive likelihood.
[Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/triang/README.md)
[Fitter](../../../univariateML-cpp/include/cppunivariate/distributions/triang/mle.hpp)

Nguyen and McLachlan §1 state Oliver’s order-statistic result on the unit
interval with interior modes. The formalization uses arbitrary held
endpoints, the dossier’s endpoint-mode density convention, and permits
repeated observations. It evaluates every rank. The narrower rank screen
that Nguyen and McLachlan attribute to Oliver for distinct observations is
outside the requested product criterion. Oliver’s bibliographic record
was accessible, but the original full text could not be retrieved from
JSTOR or the publisher. The source comparison uses the later explicit
statement.
[Oliver (1972)](https://www.jstor.org/stable/2682867)
[Nguyen and McLachlan (2016), §1](https://arxiv.org/pdf/1602.03692)

## 1. Convexity between observations

```lean
theorem logLik_strictConvexOn_gap (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, x i ∈ Ioo a b) {l u : ℝ} (hgap : ∀ i, x i ≤ l ∨ u ≤ x i) :
    StrictConvexOn ℝ (Icc l u) (logLik a b x)
```

With interior data, on a closed interval whose interior contains no data,
the log-likelihood is strictly convex. Each summand is a constant minus
the logarithm of a positive affine distance to one endpoint. The branch
formulas still hold at observations on the boundary of the gap. The
finite sum is strict because the sample is nonempty.

Effort was moderate. The proof uses Mathlib’s strict concavity of `log`,
affine identities for both branches, and a strict finite-sum inequality.

## 2. Attainment, the maximizer set, and the rank criterion

```lean
theorem likelihood_pos_iff {x : Fin n → ℝ} (hab : a < b)
    (hx : ∀ i, x i ∈ Icc a b) {c : ℝ} (hc : c ∈ Icc a b) :
    0 < likelihood a b x c ↔
      (∀ i, x i = a → c = a) ∧ (∀ i, x i = b → c = b)
```

A mode has positive likelihood exactly when every observed endpoint
coincides with it. An observation at one endpoint alone therefore makes
that endpoint the sole mode with positive likelihood.

```lean
theorem likelihood_zero_of_both_endpoints {x : Fin n → ℝ} (hab : a < b)
    (ha : ∃ i, x i = a) (hb : ∃ i, x i = b) (c : ℝ) :
    likelihood a b x c = 0
```

When both endpoints occur, the likelihood is zero throughout the closed
mode interval.

```lean
theorem isMaxOn_iff_observation {x : Fin n → ℝ} (hab : a < b) (hn : 0 < n)
    (hx : ∀ i, x i ∈ Icc a b) (hcompatible : ¬ ((∃ i, x i = a) ∧ (∃ i, x i = b)))
    {c : ℝ} (hc : c ∈ Icc a b) :
    IsMaxOn (likelihood a b x) (Icc a b) c ↔
      (∃ i, x i = c) ∧ ∀ i, likelihood a b x (x i) ≤ likelihood a b x c
```

For compatible data, a feasible mode maximizes the likelihood exactly
when it is a sample value and its likelihood dominates that of every
sample value.

```lean
theorem exists_isMaxOn {x : Fin n → ℝ} (hab : a < b) (hn : 0 < n)
    (hx : ∀ i, x i ∈ Icc a b) :
    ∃ c ∈ Icc a b, IsMaxOn (likelihood a b x) (Icc a b) c
```

The algebraic maximum on the closed mode interval is always attained.
On compatible data it is attained at an observation. In the both-endpoints
case the entire interval attains zero.

```lean
theorem rankWeight_eq_products {x : Fin n → ℝ} (r : Fin n)
    (hr : orderStat x r ∈ Ioo a b) :
    rankWeight a b x r =
      (∏ i ∈ Finset.univ.filter (· < r), (orderStat x i - a) / (orderStat x r - a)) *
      (∏ i ∈ Finset.univ.filter (r < ·), (b - orderStat x i) / (b - orderStat x r))
```

For an interior order statistic, its likelihood divided by the common
peak height to the power `n` is the product of the lower-rank and
upper-rank ratios in the brief. A repeated observation at that mode
contributes one. The general `rankWeight` also assigns one to tied
observations at endpoint modes, where a written ratio can be zero over zero.

```lean
theorem isMaxOn_iff_orderStat {x : Fin n → ℝ} (hab : a < b) (hn : 0 < n)
    (hx : ∀ i, x i ∈ Icc a b) (hcompatible : ¬ ((∃ i, x i = a) ∧ (∃ i, x i = b)))
    {c : ℝ} (hc : c ∈ Icc a b) :
    IsMaxOn (likelihood a b x) (Icc a b) c ↔
      ∃ r, orderStat x r = c ∧ ∀ s, rankWeight a b x s ≤ rankWeight a b x r
```

On compatible data, a feasible mode is globally maximizing exactly when
it equals an order statistic whose normalized likelihood is greatest
among all ranks. The common scaling factor is positive, so it drops out
of every comparison.

This target took most of the proof work. Mathlib’s finite extrema find
the adjacent observations, and its strict convexity comparison bounds
the likelihood between them. Outside the sample range, each factor
strictly improves toward the nearest extreme observation. For endpoint
observations the proof uses positivity directly. Attainment then reduces
to a maximum over a finite set. The project’s generic profile,
compactification, and score lemmas do not enter this finite argument.
The rank proof reindexes by Mathlib’s sorting permutation and factors the
product by rank.

## 3. Exact rational tie

```lean
theorem rational_tie :
    {c ∈ Icc (0 : ℝ) 1 | IsMaxOn (likelihood 0 1 ![1/4, 3/4]) (Icc 0 1) c} =
      ({1/4, 3/4} : Set ℝ)
```

For the sample `(1/4, 3/4)` on `[0, 1]`, those two distinct order
statistics are exactly the maximizing modes. The weight at each is
`1/3` and the likelihood at each is `4/3`. This required only a short
exact-arithmetic check once the finite comparison theorem was available.

## 4. Free endpoints

The dossier states attainment for all-free nonconstant finite samples and
documents width collapse when a constant sample is compatible with
collapse and at least one endpoint is free. The fitter enumerates
observation modes and profiles the free endpoints, using concave endpoint
solves and interval bounds. The dossier also gives a separate free-endpoint
tie for the sample `(0, 1)`. This was a scope and source check. The Lean
theorems cover held endpoints and leave the endpoint profiles, collapse
argument, and numerical comparisons outside the formalization.
[Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/triang/README.md)
[Fitter](../../../univariateML-cpp/include/cppunivariate/distributions/triang/mle.hpp)

## Verification

The final workstation runs used `/tmp/umvl-cpu/run` with one Lean thread
and `nice -n 10`.

- `lake env lean UnivariateML/Family/Triangular.lean` passed without warnings.
- `lake build UnivariateML +UnivariateML.Family.Triangular` reported
  `Build completed successfully (7386 jobs).` The new module built without
  warnings. The build replayed existing warnings in GPD, Weibull, Fréchet,
  and LomaxTwoPoint. It reused the Mathlib cache. The module was named
  explicitly because the coordinator owns the root import.

All requested fixed-endpoint proof targets are closed without proof
placeholders.
