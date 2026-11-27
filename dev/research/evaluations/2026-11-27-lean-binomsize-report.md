# Binomial maximum likelihood with unknown size

TASK-380, worktree branch `codex/lean-binomsize`, base `0bfa829c`.

Lean proves the dossier’s **exactly when** for a positive mean and variance
computed with divisor `m`. It also proves strict domination by the Poisson
face at every finite feasible size when `μ ≤ v`, including equality.
All four mathematical targets are closed.
[Lean module](../../lean/UnivariateML/Family/BinomialSize.lean) · [Binomial dossier](../../../univariateML-cpp/include/cppunivariate/distributions/binom/README.md)

**Sign correction in the brief.** In the expansion
`ℓ(N) = ℓ∞ + c/N + o(1/N)`, the coefficient for the total log-likelihood
is `c = m(μ − v)/2`. The proof takes the derivative in inverse size at
zero. This gives the first-order little-o remainder and suffices for
attainment. The proposed quadratic-order remainder is outside the Lean
statement. The dossier uses the correct dispersion direction.

**Dossier wording about ties.** Its phrase “Integer maximizers need not be
unique” is proved for held probability in the existing discrete module.
If read as asserting that a positive-mean sample can have multiple
both-free maximizers, the existence of such a sample remains open in the
repository. The tie study gives an exact criterion for specified sizes
and bounded exclusions. An explicit positive-mean both-free tie or a
theorem ruling out all such ties would settle the claim.
[Held-probability results](../../lean/UnivariateML/Family/Discrete.lean) · [Tie study](2026-11-25-binom-ties-math-report.md)

## Proof and effort by target

| Target | Work |
| --- | --- |
| 1. Poisson limit | Rewrote the binomial coefficient as a descending factorial, verified the log-profile identity including the constant endpoint, and used continuity at inverse size zero. |
| 2. Strict improvement and attainment | Used L’Hôpital’s rule for the second-order limit of the entropy term, computed the inverse-size derivative, and maximized over the finite initial segment that can beat a witness above the face. Reused the fixed-size theorem and the generic profile graph to transfer finiteness to parameter pairs. |
| 3. Strict bound through equality | This took the most proof work. Established a power-sum trapezoid bound and its strict square case, applied size-biased Jensen, and compared convergent logarithmic series term by term. Used the generic limit-path bound to identify the supremum and prove nonattainment. |
| 4. All-zero sample | Simplified the likelihood to `(1 − p)^(Nm)` and characterized equality with one on the closed probability interval. |

For target 3, write `t = 1/N` and `S = Σᵢ xᵢ = mμ`. Feasibility and
`0 < μ ≤ v` imply `μ < N`, since equality with size would force every
count to equal that size and the variance to vanish. In inverse size the
profile differs from the face by an entropy correction minus a
falling-factorial logarithmic loss. At power `t^r` for `r ≥ 1`, the
correction has coefficient `mμ^(r+1)/[r(r+1)]`. The loss has coefficient
`(Σᵢ Σⱼ₌₀^(xᵢ−1) j^r)/r`.

For a positive count `k`, the average of `j^r` over `j = 0,…,k−1`
is at least `(k−1)^r/(r+1)`. Lean proves this by pairing reflected terms
in the geometric sum for each difference of consecutive powers and using
the chord bound between the grid endpoints. With weights `xᵢ/S`, the
weighted mean of `xᵢ−1` is at least `μ` by the dispersion hypothesis.
Jensen’s inequality then gives the coefficient ordering. For squares the
grid sum exceeds `k(k−1)^2/3` by `k(k−1)/6`. Some count is greater
than one under the same hypothesis, so the squared term gives strictness
even when the first-order term cancels.

The existence criterion agrees with DeRiggi’s published result. The proof
uses the coefficient comparison and the limit without a profile shape
assumption. Profile unimodality, the location of ties, and floating-point
accuracy of the fitter are outside the formal results. The dossier’s
escape test `mS + S² ≤ mΣᵢ xᵢ²` is algebraically the same inequality.
[DeRiggi (1983)](https://doi.org/10.1080/01621459.1983.10477949) · [Olkin, Petkau and Zidek (1981)](https://doi.org/10.1080/01621459.1981.10477697) · [C++ fitter](../../../univariateML-cpp/include/cppunivariate/distributions/binom/mle.hpp)

## Lean statements

All names are in `UnivariateML.BinomialSize`. The set `feasible x`
contains sizes at least every count. The set `parameterDomain`
is the product of the natural sizes and the closed unit interval.

### Profile identity and Poisson limit

```lean
theorem profile_eq_binomial_logLik {m N : ℕ} (x : Fin m → ℕ) (hm : 0 < m)
    (hmean : 0 < mean x) (hN : 0 < N) (hx : N ∈ feasible x) :
    profile x N = ∑ i, (log (N.choose (x i) : ℝ) +
      (x i : ℝ) * log (mean x / N) + ((N : ℝ) - x i) * log (1 - mean x / N))
```

At a feasible positive size, the inverse-size formula equals the ordinary binomial log-likelihood at the empirical mean divided by size. This includes the constant-data endpoint.

```lean
theorem exp_profile_eq_sampleLikelihood {m N : ℕ} (x : Fin m → ℕ) (hm : 0 < m)
    (hmean : 0 < mean x) (hN : N ∈ feasible x) :
    exp (profile x N) = Discrete.Binomial.sampleLikelihood x N (mean x / N)
```

Exponentiating the log-profile recovers the original product of binomial masses at the conditional maximizer.

```lean
@[simp] theorem inverseProfile_zero {m : ℕ} (x : Fin m → ℕ) :
    inverseProfile x 0 = poissonLogLik x
```

At inverse size zero, the extended profile takes the Poisson log-likelihood value.

```lean
theorem profile_tendsto_poisson {m : ℕ} (x : Fin m → ℕ) (hm : 0 < m) :
    Tendsto (profile x) atTop (𝓝 (poissonLogLik x))
```

The profile over integer sizes converges to the Poisson log-likelihood at the sample mean.

### A finite maximum under underdispersion

```lean
theorem inverseProfile_hasDerivAt {m : ℕ} (x : Fin m → ℕ) (hm : 0 < m) :
    HasDerivAt (inverseProfile x) ((m : ℝ) * (mean x - variance x) / 2) 0
```

The derivative of the total log-likelihood in inverse size at zero is half the sample size times the difference between the mean and the variance.

```lean
theorem eventually_poisson_lt_profile {m : ℕ} (x : Fin m → ℕ) (hm : 0 < m)
    (hv : variance x < mean x) :
    ∀ᶠ N in atTop, poissonLogLik x < profile x N
```

If the mean exceeds the variance, all sufficiently large integer sizes strictly beat the Poisson value.

```lean
theorem finite_profile_maximizers_of_underdispersion {m : ℕ} (x : Fin m → ℕ)
    (hm : 0 < m) (hv : variance x < mean x) :
    {N ∈ feasible x | IsMaxOn (profile x) (feasible x) N}.Nonempty ∧
      {N ∈ feasible x | IsMaxOn (profile x) (feasible x) N}.Finite
```

The set of maximizing feasible sizes is nonempty and finite.

```lean
theorem finite_joint_maximizers_of_underdispersion {m : ℕ} (x : Fin m → ℕ)
    (hm : 0 < m) (hmean : 0 < mean x) (hv : variance x < mean x) :
    {q ∈ parameterDomain | IsMaxOn (fun q ↦ Discrete.Binomial.sampleLikelihood x q.1 q.2)
      parameterDomain q}.Finite
```

The set of maximizing size and probability pairs is finite as well.

### Strict Poisson dominance and the existence criterion

```lean
theorem profile_lt_poisson_of_overdispersion {m N : ℕ} (x : Fin m → ℕ) (hm : 0 < m)
    (hmean : 0 < mean x) (hv : mean x ≤ variance x) (hx : N ∈ feasible x) :
    profile x N < poissonLogLik x
```

If the positive mean is at most the variance, every finite feasible size lies strictly below the Poisson value. The inequality includes equality of mean and variance.

```lean
theorem profile_sSup_eq_poisson {m : ℕ} (x : Fin m → ℕ) (hm : 0 < m)
    (hmean : 0 < mean x) (hv : mean x ≤ variance x) :
    sSup (profile x '' feasible x) = poissonLogLik x
```

The Poisson value is the supremum in that regime.

```lean
theorem profile_maximizer_iff_underdispersion {m : ℕ} (x : Fin m → ℕ) (hm : 0 < m)
    (hmean : 0 < mean x) :
    {N ∈ feasible x | IsMaxOn (profile x) (feasible x) N}.Nonempty ↔
      variance x < mean x
```

For positive mean, a finite maximizing size exists exactly when the mean exceeds the variance.

```lean
theorem joint_maximizers_eq_profile_graph {m : ℕ} (x : Fin m → ℕ) (hm : 0 < m)
    (hmean : 0 < mean x) :
    {q ∈ parameterDomain | IsMaxOn (fun q ↦ Discrete.Binomial.sampleLikelihood x q.1 q.2)
      parameterDomain q} =
      (fun N : ℕ ↦ (N, mean x / N)) '' {N ∈ feasible x | IsMaxOn (profile x) (feasible x) N}
```

Every joint maximum has a maximizing size and its unique conditional probability. Each maximizing size gives a joint maximum. The comparison spans all natural sizes and probabilities in the closed unit interval.

```lean
theorem joint_mle_iff_underdispersion {m : ℕ} (x : Fin m → ℕ) (hm : 0 < m)
    (hmean : 0 < mean x) :
    {q ∈ parameterDomain | IsMaxOn (fun q ↦ Discrete.Binomial.sampleLikelihood x q.1 q.2)
      parameterDomain q}.Nonempty ↔ variance x < mean x
```

For positive mean, the original binomial likelihood attains a joint maximum exactly when the mean exceeds the variance.

### All-zero data

```lean
theorem all_zero_maximizers {m : ℕ} (hm : 0 < m) (q : ℕ × ℝ)
    (hq : q ∈ parameterDomain) :
    IsMaxOn (fun p ↦ Discrete.Binomial.sampleLikelihood (fun _ : Fin m ↦ 0) p.1 p.2)
      parameterDomain q ↔ q.1 = 0 ∨ q.2 = 0
```

For a nonempty all-zero sample, the maximizing pairs are exactly those with size zero or probability zero. Both parameters are unidentified.

## Verification

Both final commands ran on the workstation through the one-slot lock and
`nice -n 10`, using Lean 4.36.0.

- `/home/jonas/.elan/bin/lake env lean UnivariateML/Family/BinomialSize.lean`
  exited 0 with no diagnostics.
- `/home/jonas/.elan/bin/lake build UnivariateML +UnivariateML.Family.BinomialSize`
  exited 0. Lake reported `Build completed successfully (7466 jobs).`
  The new module built without warnings. The library emitted existing
  linter warnings from other modules.

The final build named the new module explicitly because the coordinator
owns the root import. No proof holes or new axioms were introduced.
