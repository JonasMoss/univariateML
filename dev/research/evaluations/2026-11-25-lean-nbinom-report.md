# Negative binomial likelihood in Lean

**TASK-314 · 2026-11-25**

For a nonempty sample with at least one positive count, Lean verifies a unique
joint MLE exactly when the variance with divisor $n$ exceeds the mean.
At or below the threshold, every finite interior pair has log likelihood
strictly below the Poisson supremum. Lean also verifies the held-size and
held-probability fits, including the respective degenerate optima for
all-zero data. [Lean module](../../lean/UnivariateML/Family/NegBinomial.lean)

## Source discrepancies

**The equidispersion comment in `mle.hpp:138` reverses the score sign.**
With the score defined as the derivative of the profiled log likelihood, the
score is strictly positive at every finite positive size when variance equals
the mean. The return branch and the dossier agree on the Poisson supremum.
The opening comment also attributes the criterion to Aragón, Eberly and
Eberly (1992), whereas the dossier uses the Brøns and Jensen construction
and flags a published objection to the 1992 argument.
[Header](../../../univariateML-cpp/include/cppunivariate/distributions/nbinom/mle.hpp)
[Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/nbinom/README.md#existence-and-uniqueness-proof)

## 1. Held size

With positive size and positive mean, the probability `prob x r` is
$r/(r+\bar x)$. It lies in $(0,1)$ and strictly beats every other
interior probability. The zero-sample boundary result is given in §4.

```lean
theorem heldSize (hn : 0 < n) {x : Fin n → ℕ} (hm : 0 < mean x)
    {r : ℝ} (hr : 0 < r) :
    prob x r ∈ Ioo (0 : ℝ) 1 ∧
      ∀ p ∈ Ioo (0 : ℝ) 1, p ≠ prob x r → logLik x r p < profile x r
```

## 2. Profile score and Poisson limit

`profile_hasDerivAt` gives the derivative of the log likelihood after
substituting the maximizing probability. The derivative equals
$\sum_{i,j<x_i}(r+j)^{-1}-n\log(1+\bar x/r)$.
`profile_tendsto_poisson` gives the limit at infinite size, including
the factorial normalizer, at the fitted Poisson mean $\bar x$.

```lean
theorem profile_hasDerivAt (hn : 0 < n) {x : Fin n → ℕ} (hm : 0 < mean x)
    {r : ℝ} (hr : 0 < r) : HasDerivAt (profile x) (score x r) r

theorem profile_tendsto_poisson {x : Fin n → ℕ} (hm : 0 < mean x) :
    Tendsto (profile x) atTop (𝓝 (poisson x))
```

## 3. Joint existence, uniqueness and escape

`mean_pos_iff` relates the positive-mean hypothesis to the presence of a
positive count. `jointMLE_iff` gives the exact overdispersion criterion.
`poisson_supremum` identifies the least upper bound and gives strict loss
at every feasible pair in the complementary case, including equality.

```lean
theorem mean_pos_iff (hn : 0 < n) (x : Fin n → ℕ) :
    0 < mean x ↔ ∃ i, 0 < x i

theorem jointMLE_iff (hn : 0 < n) {x : Fin n → ℕ} (hm : 0 < mean x) :
    (∃! q : ℝ × ℝ, q ∈ domain ∧
      IsMaxOn (fun q ↦ logLik x q.1 q.2) domain q) ↔ mean x < variance x

theorem poisson_supremum (hn : 0 < n) {x : Fin n → ℕ} (hm : 0 < mean x)
    (hv : variance x ≤ mean x) :
    (∀ q ∈ domain, logLik x q.1 q.2 < poisson x) ∧
      sSup ((fun q : ℝ × ℝ ↦ logLik x q.1 q.2) '' domain) = poisson x
```

The proof uses the dossier's telescoping decomposition. For each positive
count, Lean shows the strict decrease of the remainder after subtracting
the linear part of $T_q$. Taking the sum gives strict increase of
$C(r)=r(r+m)^2\,\ell_{\rm prof}''(r)$, where $m=\bar x$, with finite
limit $n(S^2-m)$. This is the dossier's curvature ratio multiplied by
$m$. The formal proof obtains its monotonicity directly from the
telescoped terms. It uses the sign of $C$, the near-zero positive score,
and the score limit of zero at infinity to prove the threshold and the
unique crossing. The dossier obtains the same conclusion through the
weighted-average ratio $G$.

## 4. Held probability and the all-zero likelihood

`heldProb` gives a unique maximizing positive size for any held interior
probability and positive-mean sample. The reciprocal terms give a strictly
decreasing score and the endpoint signs give a root. For all-zero data,
`zeroLikelihood_eq_exp_logLik` identifies the extended likelihood with the
exponentiated finite-sum formula on the interior.
`zeroLikelihood_eq_one_iff` identifies exactly the two maximizing faces.
In particular, holding positive size selects $p=1$ and holding interior
probability selects $r=0$.

```lean
theorem heldProb (hn : 0 < n) {x : Fin n → ℕ} (hm : 0 < mean x)
    {p : ℝ} (hp : p ∈ Ioo (0 : ℝ) 1) :
    ∃! r, r ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun t ↦ logLik x t p) (Ioi 0) r

theorem zeroLikelihood_eq_exp_logLik (x : Fin n → ℕ) (hx : ∀ i, x i = 0)
    {r p : ℝ} (hp : 0 < p) :
    zeroLikelihood n r p = exp (logLik x r p)

theorem zeroLikelihood_eq_one_iff (hn : 0 < n) {r p : ℝ} (hr : 0 ≤ r)
    (hp : 0 < p) (hp' : p ≤ 1) :
    zeroLikelihood n r p ≤ 1 ∧ (zeroLikelihood n r p = 1 ↔ r = 0 ∨ p = 1)
```

## Scope and effort

The statements concern exact real arithmetic and natural-number counts.
For positive-mean data, the declared domain is (r>0, 0<p<1), as in
the brief. The all-zero extension uses (r\ge 0, 0<p\le 1).
The positive-data point-mass faces discussed in the dossier lie outside
the declared domain. The dossier's curvature construction suffices for
the stated targets. No proof obligations are open. Integration into the
root import list is assigned to the coordinator.

| Target | Relative effort and proof work |
| --- | --- |
| Held size | Small. A strict log-ratio inequality and cancellation at the conditional optimum. |
| Profile score and limit | Moderate. Finite-sum differentiation, local log identities, and normalization in reciprocal size to take the Poisson limit. |
| Joint criterion | Largest. Centered telescoping, strict monotonicity, a rational limit, the endpoint score signs, and the transfer from the profile to the joint likelihood. |
| Held probability and zero data | Moderate. A strict finite-sum comparison, a lower bound from the first reciprocal term, a sign bracket, and the real-power boundary calculation. |

## Verification

The final standalone `lake env lean UnivariateML/Family/NegBinomial.lean`
check passed on the workstation. The final
`lake build UnivariateML +UnivariateML.Family.NegBinomial` passed on the
workstation with **6698 jobs**. It built the new module without warnings
and replayed existing style warnings in GPD and Weibull. Both checks used
`/tmp/umvl-cpu/run` and the existing Mathlib cache. The module contains
no proof placeholders or added axioms.
