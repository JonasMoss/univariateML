# Lean evaluation of GEV with shape held

**TASK-334 · 2026-11-25**

On nonconstant data, two finite GEV maximizers at the same held shape coincide.
Every shape from −1 through zero attains its maximum. A positive held shape
attains its maximum exactly when $\xi k<n-k$, where $k$ counts sample minima.
Equality gives a finite unattained supremum. Reversing the strict inequality
gives unbounded likelihood, as does $\xi<-1$.
[Lean module](../../lean/UnivariateML/Family/GEV.lean)

## Dossier findings and scope

- **The endpoint-gap root statement needs a qualification at $\xi=-1$.**
  The general all-free cover paragraph includes −1 in its interval and then
  refers to a unique monotone endpoint-gap root at each nonzero shape. At −1,
  the log likelihood profiled over scale for upper endpoint $M+e$ is
  $-n[1+\log(M-\bar x+e)]$. It decreases strictly for $e\ge0$ and attains its
  maximum at the closed endpoint $e=0$. On the positive branch a root requires
  the strict multiplicity inequality, which follows from the paragraph's
  assumptions of fewer than half the observations at the minimum and $\xi\le1$.
  [GEV dossier](../../../univariateML-cpp/include/cppunivariate/distributions/gev/README.md#general-all-free-shape-cover)
- **The fixed-shape classification agrees with the math report and the
  dossier's positive-shape threshold and supremum.** A balanced two-value
  sample at held $\xi=1$ has a finite unattained supremum. The dossier's
  attained balanced two-value case uses the reverse-exponential member
  at $\xi=-1$ when shape is free. The dossier's family-wide
  `uniqueness: unknown` has broader scope than the fixed-shape result.
  [Math report](2026-11-25-gev-math-report.md) · [GEV dossier](../../../univariateML-cpp/include/cppunivariate/distributions/gev/README.md)
- **Endpoint convention.** The real-valued Lean objective is interpreted on
  `domain x xi`. At −1 it includes contact with log density $-\log\sigma$.
  At other shapes it uses strict support. Zero-density contact cannot yield
  a finite maximum. For $\xi<-1$ the dossier specifies infinite endpoint
  density. The theorem proves unboundedness along a strict-support path.
  With both nuisance coefficients free, `gev_mle_impl` rejects that held shape
  as `InvalidShape`.
  [GEV dossier](../../../univariateML-cpp/include/cppunivariate/distributions/gev/README.md) · [Fitter](../../../univariateML-cpp/include/cppunivariate/distributions/gev/mle.hpp)
- The results concern exact real arithmetic and uncensored data, including
  nonconstant samples of size two. The critical positive-shape value is a
  total log likelihood and matches the location-and-scale-free branch of
  `gev_record_critical_collapse`. The floating-point comparison, margin,
  and coefficient accuracy are outside the formal claims.
  [Fitter](../../../univariateML-cpp/include/cppunivariate/distributions/gev/mle.hpp)

## Proofs and effort by target

| Target | Effort and proof |
| --- | --- |
| Joint uniqueness | The negative interior required the most new work. With upper endpoint $E=\mu-\sigma/\xi$, use $q=-\xi/\sigma>0$, $b=q(E-M)>0$, and residuals $b+q(M-x_i)$. Strict convexity of $t^a-(a-1)\log t$, $a=-1/\xi>1$, and strict concavity of $\log q$ give strict joint concavity. At zero, a short likelihood and domain identification reuses `LocScaleInstances.gumbel_existsUnique_mle`. At positive shape the Fréchet monotone-root theorem gives uniqueness. |
| Existence and collapse | The main new existence argument is negative-interior coercivity. The loss is at least $t$ by a power tangent inequality and $\log t\le t-1$. A positive sample gap controls large $q$ and $b$. A zero sample gap bounds $b$ away from zero, and $n\log q$ bounds $q$ away from zero on each superlevel. `Generic.Escape` gives attainment. The reverse exponential uses a separate short maximization with closed $b=0$. Below −1 the log term of an observation at the maximum diverges at fixed scale, while the other terms stay bounded below. Positive shape required the coordinate bijection and transport of all three Fréchet cases. A power-sum bound also gives convergence of the profiled path to zero scale. |
| Dossier comparison | Reviewed the held-shape table, support metadata, cover paragraph, and fitter checks. The gap-root statement at −1 is the only mathematical discrepancy found in scope. |

For positive shape the Fréchet shape is $a=1/\xi$, its scale is
$s=\sigma/\xi$, and its lower endpoint is $m-e=\mu-\sigma/\xi$. The map
$(e,s)\mapsto(m-e+s,\xi s)$ preserves the total log likelihood.
`Frechet.collapseExponent (lowerGaps x m) xi⁻¹` is $a(n-k)-k$. The sum in
the critical value can run over all observations because Lean defines
`Real.log 0 = 0`. It equals the sum over observations strictly above $m$.

## Checked theorem statements

The declarations are in `UnivariateML.GEV`. The sample is `x : Fin n → ℝ`.
When used, `m` and `M` are an attained minimum and maximum. A pair `p` uses
the original $(\mu,\sigma)$ coordinates. `IsMLE` combines domain membership
and `IsMaxOn`. `Nonconstant x` means that two observations differ.

### `fixed_shape_unique`

Two finite maximizing location–scale pairs at the same held shape on nonconstant data are equal.

```lean
theorem fixed_shape_unique (hx : LocationScale.Nonconstant x) {p q : ℝ × ℝ}
    (hp : IsMLE x xi p) (hq : IsMLE x xi q) : p = q
```

### `fixed_shape_exists_iff`

On nonconstant data the endpoint shape −1 is included. At positive shape, attainment also requires the strict minimum-count inequality.

```lean
theorem fixed_shape_exists_iff (hx : LocationScale.Nonconstant x)
    (hm : ∀ i, m ≤ x i) (hz : ∃ i, x i = m) :
    (∃ p, IsMLE x xi p) ↔ -1 ≤ xi ∧
      (0 < xi → xi * Frechet.minimumCount (lowerGaps x m) <
        n - Frechet.minimumCount (lowerGaps x m))
```

### `negative_unbounded`

Below −1, a fixed-scale path with the upper endpoint approaching the sample maximum exceeds every real log-likelihood bound.

```lean
theorem negative_unbounded (hlo : xi < -1) (hM : ∀ i, x i ≤ M) (hz : ∃ i, x i = M) :
    ¬ BddAbove (logLik x xi '' domain x xi)
```

### `reverse_exponential_unique`

The unique fit at −1 has location equal to the sample mean and scale equal to the maximum minus the mean. The fitted upper endpoint is the sample maximum.

```lean
theorem reverse_exponential_unique (hx : LocationScale.Nonconstant x)
    (hM : ∀ i, x i ≤ M) (hz : ∃ i, x i = M) :
    IsMLE x (-1) ((∑ i, x i) / n, M - (∑ i, x i) / n) ∧
      ∀ p, IsMLE x (-1) p → p = ((∑ i, x i) / n, M - (∑ i, x i) / n)
```

### `negative_interior_unique`

Every held shape strictly between −1 and zero has exactly one location–scale MLE for a nonconstant sample.

```lean
theorem negative_interior_unique (hx : LocationScale.Nonconstant x)
    (hlo : -1 < xi) (hxi : xi < 0) (hM : ∀ i, x i ≤ M) (hz : ∃ i, x i = M) :
    ∃! p, IsMLE x xi p
```

### `zero_unique`

The Gumbel member has exactly one location–scale MLE for a nonconstant sample.

```lean
theorem zero_unique (hx : LocationScale.Nonconstant x) : ∃! p, IsMLE x 0 p
```

### `positive_exists_iff`

For positive held shape, attainment is equivalent to the dossier's strict inequality in the number of sample minima.

```lean
theorem positive_exists_iff (hn : 0 < n) (hxi : 0 < xi) (hm : ∀ i, m ≤ x i)
    (hz : ∃ i, x i = m) :
    (∃ p, IsMLE x xi p) ↔
      xi * Frechet.minimumCount (lowerGaps x m) < n - Frechet.minimumCount (lowerGaps x m)
```

### `positive_unique`

A positive collapse exponent gives exactly one finite GEV fit by the Fréchet coordinate bijection.

```lean
theorem positive_unique (hn : 0 < n) (hxi : 0 < xi) (hm : ∀ i, m ≤ x i)
    (hz : ∃ i, x i = m) (hE : 0 < Frechet.collapseExponent (lowerGaps x m) xi⁻¹) :
    ∃! p, IsMLE x xi p
```

### `positive_collapse_eq`

At zero exponent the displayed value is the least upper bound, and every feasible fit has strictly smaller total log likelihood.

```lean
theorem positive_collapse_eq (hn : 0 < n) (hxi : 0 < xi) (hm : ∀ i, m ≤ x i)
    (hz : ∃ i, x i = m) (hE : Frechet.collapseExponent (lowerGaps x m) xi⁻¹ = 0) :
    let A := n * (log (1 + xi⁻¹) - 1) - (1 + xi⁻¹) * ∑ i, log (x i - m)
    IsLUB (logLik x xi '' domain x xi) A ∧ ∀ p ∈ domain x xi, logLik x xi p < A
```

### `positive_unbounded`

A negative collapse exponent rules out every finite upper bound.

```lean
theorem positive_unbounded (hn : 0 < n) (hxi : 0 < xi) (hm : ∀ i, m ≤ x i)
    (hz : ∃ i, x i = m) (hE : Frechet.collapseExponent (lowerGaps x m) xi⁻¹ < 0) :
    ¬ BddAbove (logLik x xi '' domain x xi)
```

### `positive_collapse_path`

The Fréchet-optimal scale at each positive gap gives a feasible GEV path to location $m$ and zero scale. At zero exponent its log likelihood tends to the displayed supremum. At a negative exponent it tends to positive infinity.

```lean
theorem positive_collapse_path (hn : 0 < n) (hxi : 0 < xi) (hm : ∀ i, m ≤ x i)
    (hz : ∃ i, x i = m) :
    (∀ e > 0, collapsePath x m xi e ∈ domain x xi) ∧
    Tendsto (collapsePath x m xi) (𝓝[>] 0) (𝓝 (m, 0)) ∧
    (Frechet.collapseExponent (lowerGaps x m) xi⁻¹ = 0 →
      Tendsto (fun e ↦ logLik x xi (collapsePath x m xi e)) (𝓝[>] 0)
        (𝓝 (n * (log (1 + xi⁻¹) - 1) - (1 + xi⁻¹) * ∑ i, log (x i - m)))) ∧
    (Frechet.collapseExponent (lowerGaps x m) xi⁻¹ < 0 →
      Tendsto (fun e ↦ logLik x xi (collapsePath x m xi e)) (𝓝[>] 0) atTop)
```

### `constant_unbounded`

On a nonempty constant sample, setting location to the common value and shrinking scale gives unbounded likelihood at any held shape.

```lean
theorem constant_unbounded (hn : 0 < n) (hc : ∀ i, x i = m) :
    ¬ BddAbove (logLik x xi '' domain x xi)
```

## Verification and remaining work

The standalone `lake env lean UnivariateML/Family/GEV.lean` check passed on
the workstation. The final `lake build UnivariateML +UnivariateML.Family.GEV`
passed there (7,398 jobs, exit 0), explicitly building the new module
alongside the default library target. The new module emitted no warnings.
Lake replayed existing diagnostics from other modules. Both checks ran
through the workstation's single-slot wrapper in the foreground. The build
used the existing Mathlib cache.

Every stated result is proved without placeholders or added axioms.
The negative-interior theorem establishes the unique joint nuisance fit
by strict concavity and coercivity. Separate monotonicity of the negative
gap ratio, joint continuity in shape at zero, and floating-point
certification are outside these statements. The coordinator's library-root
import is the remaining integration step.
