# Four discrete likelihoods in Lean

**TASK-328 · 2026-11-25**

Lean verifies the finite MLE conditions, the maximizing parameters, and the
unattained boundary suprema for log-series, infinite zeta, Yule–Simon, and
discrete Laplace. Discrete Laplace includes the full set of maximizing
integer locations. [Lean module](../../lean/UnivariateML/Family/Discrete2.lean)

## Source discrepancy

The zeta dossier's truncation metadata claims strict concavity while allowing
a singleton window, $N=1$. In that window the law is a point mass for every
exponent, so its log likelihood is constant and the exponent is unidentified.
The Details section gives the correct qualification $N>1$. The infinite-support
existence statement agrees with the Lean result.
[Zeta dossier](../../../univariateML-cpp/include/cppunivariate/distributions/zeta/README.md)

## Scope and proof choices

The sample is nonempty and lies on the corresponding count lattice.
Log-series, zeta, and discrete Laplace use per-observation log likelihood.
Yule–Simon uses the finite beta product, with $x!/x=(x-1)!$ supplying the
dossier's $\Gamma(x)$. Log-series uses the solver's transformed mean and the
derivative in theta. Discrete Laplace restricts the existing continuous
Laplace median theorem to integer locations.

The excluded boundary supremum of zero on the log scale corresponds to the
likelihood tending to one for the observed sample. The probability of the
observed common value tends to one along each boundary path. Holding all
parameters at feasible values leaves a singleton and gives direct likelihood
evaluation.

The four exact-real MLE targets have no open proof obligations. Finite
truncation, other modifiers, and certification of the floating-point
searches and approximations are outside this job's scope.

## Log-series

Namespace `UnivariateML.LogSeries`.

`modelMean_spec`. The transformed mean increases continuously from one to infinity in $u=-\log(1-\theta)$.

```lean
theorem modelMean_spec :
    StrictMonoOn modelMean (Ioi 0) ∧ ContinuousOn modelMean (Ioi 0) ∧
      Tendsto modelMean (𝓝[>] 0) (𝓝 1) ∧ Tendsto modelMean atTop atTop
```

`mle`. Above empirical mean one, exactly one parameter matches the empirical mean and maximizes the likelihood.

```lean
theorem mle {x : Fin n → ℕ} (hx : 1 < empiricalMean x) :
    (∃! θ ∈ Ioo (0 : ℝ) 1, modelMean (coordinate θ) = empiricalMean x) ∧
      ∀ θ ∈ Ioo (0 : ℝ) 1, IsMaxOn (logLik x) (Ioo 0 1) θ ↔
        modelMean (coordinate θ) = empiricalMean x
```

`allOnes_boundary`. All ones give an unattained supremum of zero as theta decreases to zero.

```lean
theorem allOnes_boundary (hn : 0 < n) {x : Fin n → ℕ} (hx : ∀ i, x i = 1) :
    Tendsto (logLik x) (𝓝[>] 0) (𝓝 0) ∧ sSup (logLik x '' Ioo 0 1) = 0 ∧
      ¬ ∃ θ ∈ Ioo 0 1, IsMaxOn (logLik x) (Ioo 0 1) θ
```

`existsUnique_mle_iff`. The unique finite fit exists exactly when the empirical mean exceeds one.

```lean
theorem existsUnique_mle_iff (hn : 0 < n) {x : Fin n → ℕ} (hx : ∀ i, 0 < x i) :
    (∃! θ, θ ∈ Ioo (0 : ℝ) 1 ∧ IsMaxOn (logLik x) (Ioo 0 1) θ) ↔ 1 < empiricalMean x
```

## Infinite zeta

Namespace `UnivariateML.Zeta`.

`normalizer_atOne`. Harmonic partial sums force the normalizer to diverge as the exponent decreases to one.

```lean
theorem normalizer_atOne : Tendsto normalizer (𝓝[>] 1) atTop
```

`normalizer_atTop`. Dominated convergence gives a limiting normalizer of one at infinite exponent.

```lean
theorem normalizer_atTop : Tendsto normalizer atTop (𝓝 1)
```

`derivative`. The exponent score is the model log-count mean minus the empirical log-count mean.

```lean
theorem derivative (x : Fin n → ℕ) {s : ℝ} (hs : 1 < s) :
    HasDerivAt (logLik x) (modelMean s - empiricalMeanLog x) s
```

`modelMean_strictAnti`. The model log mean decreases strictly. Its derivative is the negative of the log-count variance. The variance is positive because counts one and two have positive weights and different logarithms.

```lean
theorem modelMean_strictAnti : StrictAntiOn modelMean (Ioi 1)
```

`modelMean_atTop`. Dominated convergence gives a limiting log-count mean of zero.

```lean
theorem modelMean_atTop : Tendsto modelMean atTop (𝓝 0)
```

`modelMean_atOne`. Steep secants of the negative log normalizer give unbounded derivatives, and score monotonicity propagates the bound as the exponent decreases to one.

```lean
theorem modelMean_atOne : Tendsto modelMean (𝓝[>] 1) atTop
```

`mle`. A positive empirical log mean gives exactly one mean-matching root, the unique maximum.

```lean
theorem mle {x : Fin n → ℕ} (hx : 0 < empiricalMeanLog x) :
    (∃! s, 1 < s ∧ modelMean s = empiricalMeanLog x) ∧
      ∀ s > 1, IsMaxOn (logLik x) (Ioi 1) s ↔ modelMean s = empiricalMeanLog x
```

`allOnes_boundary`. For all ones, increasing the exponent without bound approaches the unattained supremum of zero.

```lean
theorem allOnes_boundary {x : Fin n → ℕ} (hx : ∀ i, x i = 1) :
    Tendsto (logLik x) atTop (𝓝 0) ∧ sSup (logLik x '' Ioi 1) = 0 ∧
      ¬ ∃ s ∈ Ioi 1, IsMaxOn (logLik x) (Ioi 1) s
```

`existsUnique_mle_iff`. The unique finite fit exists exactly when the empirical log mean is positive, equivalently when a count exceeds one.

```lean
theorem existsUnique_mle_iff (hn : 0 < n) {x : Fin n → ℕ} (hx : ∀ i, 0 < x i) :
    (∃! s, s ∈ Ioi (1 : ℝ) ∧ IsMaxOn (logLik x) (Ioi 1) s) ↔ 0 < empiricalMeanLog x
```

## Yule–Simon

Namespace `UnivariateML.YuleSimon`.

`mle`. A count above one gives a unique root of the strictly decreasing log-shape score and that root is the MLE. The finite-sum score limits bracket the root.

```lean
theorem mle (hn : 0 < n) {x : Fin n → ℕ} (hx : ∀ i, 0 < x i)
    (h : ∃ i, 1 < x i) :
    (∃! ρ, 0 < ρ ∧ logShapeScore x (log ρ) = 0) ∧
      ∀ ρ > 0, IsMaxOn (logLik x) (Ioi 0) ρ ↔ logShapeScore x (log ρ) = 0
```

`allOnes_boundary`. For all ones, infinite shape is the limit path to the unattained supremum of zero.

```lean
theorem allOnes_boundary (hn : 0 < n) {x : Fin n → ℕ} (hx : ∀ i, x i = 1) :
    Tendsto (logLik x) atTop (𝓝 0) ∧
      sSup (logLik x '' Ioi 0) = 0 ∧ ¬ ∃ ρ ∈ Ioi 0, IsMaxOn (logLik x) (Ioi 0) ρ
```

`existsUnique_mle_iff`. A unique finite shape fit exists exactly when some count exceeds one.

```lean
theorem existsUnique_mle_iff (hn : 0 < n) {x : Fin n → ℕ} (hx : ∀ i, 0 < x i) :
    (∃! ρ, ρ ∈ Ioi (0 : ℝ) ∧ IsMaxOn (logLik x) (Ioi 0) ρ) ↔ ∃ i, 1 < x i
```

## Discrete Laplace

Namespace `UnivariateML.DiscreteLaplace`.

`scale_mle`. Positive mean absolute deviation gives the unique maximizing scale $d/(\sqrt{1+d^2}+1)$, the interior root of $d(1-b^2)=2b$.

```lean
theorem scale_mle {d : ℝ} (hd : 0 < d) :
    scale d ∈ Ioo (0 : ℝ) 1 ∧
      ∀ b ∈ Ioo (0 : ℝ) 1, IsMaxOn (logLik d) (Ioo 0 1) b ↔ b = scale d
```

`zero_boundary`. Zero deviation gives an unattained supremum of zero as the scale decreases to zero.

```lean
theorem zero_boundary :
    Tendsto (logLik 0) (𝓝[>] 0) (𝓝 0) ∧
      sSup (logLik 0 '' Ioo 0 1) = 0 ∧ ¬ ∃ b ∈ Ioo 0 1, IsMaxOn (logLik 0) (Ioo 0 1) b
```

`heldLocation_existsUnique_iff`. A held integer location has a unique interior scale fit exactly when some residual is nonzero.

```lean
theorem heldLocation_existsUnique_iff (hn : 0 < n) (x : Fin n → ℤ) (μ : ℤ) :
    (∃! b, b ∈ Ioo (0 : ℝ) 1 ∧ IsMaxOn (logLik (deviation x μ)) (Ioo 0 1) b) ↔
       ∃ i, x i ≠ μ
```

`heldScale`. At each held interior scale, the maximizing locations are exactly the lattice points in the sample median interval.

```lean
theorem heldScale (hn : 0 < n) (x : Fin n → ℤ) {b : ℝ} (hb : b ∈ Ioo (0 : ℝ) 1)
    (μ : ℤ) :
    IsMaxOn (fun t ↦ logLik (deviation x t) b) univ μ ↔
      (μ : ℝ) ∈ Laplace.medianSet (fun i ↦ (x i : ℝ)) hn
```

`joint`. For nonconstant data, the maximizing pairs are exactly the integer medians paired with their fitted scales.

```lean
theorem joint (hn : 0 < n) {x : Fin n → ℤ} (hx : ∃ i j, x i ≠ x j) {p : ℤ × ℝ}
    (hp : p ∈ domain) :
    IsMaxOn (jointLogLik x) domain p ↔
      (p.1 : ℝ) ∈ Laplace.medianSet (fun i ↦ (x i : ℝ)) hn ∧
        p.2 = scale (deviation x p.1)
```

`joint_constant_boundary`. For constant data, setting location to the observed common value gives the path to the unattained joint supremum as scale decreases to zero.

```lean
theorem joint_constant_boundary (hn : 0 < n) {x : Fin n → ℤ} {μ : ℤ} (hx : ∀ i, x i = μ) :
    Tendsto (fun b ↦ jointLogLik x (μ, b)) (𝓝[>] 0) (𝓝 0) ∧
      sSup (jointLogLik x '' domain) = 0 ∧ ¬ ∃ p ∈ domain, IsMaxOn (jointLogLik x) domain p
```

`heldLocation_boundary`. The same escape applies when location is held at the common observation.

```lean
theorem heldLocation_boundary (hn : 0 < n) {x : Fin n → ℤ} {μ : ℤ} (hx : ∀ i, x i = μ) :
    Tendsto (logLik (deviation x μ)) (𝓝[>] 0) (𝓝 0) ∧
      sSup (logLik (deviation x μ) '' Ioo 0 1) = 0 ∧
      ¬ ∃ b ∈ Ioo 0 1, IsMaxOn (logLik (deviation x μ)) (Ioo 0 1) b
```

`joint_exists_iff`. The joint finite fit exists exactly for nonconstant samples.

```lean
theorem joint_exists_iff (hn : 0 < n) (x : Fin n → ℤ) :
    (∃ p ∈ domain, IsMaxOn (jointLogLik x) domain p) ↔ ∃ i j, x i ≠ x j
```

`median_scale_eq`. Every integer median has the same mean absolute deviation and therefore the same scale.

```lean
theorem median_scale_eq (hn : 0 < n) {x : Fin n → ℤ} {μ ν : ℤ}
    (hμ : (μ : ℝ) ∈ Laplace.medianSet (fun i ↦ (x i : ℝ)) hn)
    (hν : (ν : ℝ) ∈ Laplace.medianSet (fun i ↦ (x i : ℝ)) hn) :
    scale (deviation x μ) = scale (deviation x ν)
```

`joint_unique_iff`. For nonconstant data the joint fit is unique exactly when both central ranks give the same integer.

```lean
theorem joint_unique_iff (hn : 0 < n) {x : Fin n → ℤ} (hx : ∃ i j, x i ≠ x j) :
    (∃! p, p ∈ domain ∧ IsMaxOn (jointLogLik x) domain p) ↔
      Laplace.lowerMedian (fun i ↦ (x i : ℝ)) hn =
        Laplace.upperMedian (fun i ↦ (x i : ℝ)) hn
```

## Effort and validation

| Family | Proof effort | Main work |
| --- | --- | --- |
| Log-series | Moderate | Strict convexity of the exponential, mean limits, and the boundary log-ratio limit |
| Infinite zeta | Largest | Summable bounds, termwise differentiation, variance, and both score limits |
| Yule–Simon | Smallest | Finite sums, rational term limits, and the generic log-score theorem |
| Discrete Laplace | Moderate | Closed-form scale, integer medians, and joint and held boundary cases |

The final standalone `lake env lean UnivariateML/Family/Discrete2.lean`
exited zero with no diagnostics. The final default `lake build` completed
successfully with 7,384 jobs. It emitted linter warnings in existing modules
(`Frechet`, `Weibull`, `LomaxTwoPoint`, and `GPD`). Both checks ran on the
workstation through the single-slot wrapper under `nice -n 10`. The
standalone check elaborated the new file directly. Its import into the
default library root is reserved for the coordinator.
