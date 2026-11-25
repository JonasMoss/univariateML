# TASK-327 · Exact likelihood transfers in Lean

The proofs cover Nakagami, logit-normal, inverse exponential, and translated half-normal in [Transfers2.lean](../../lean/UnivariateML/Family/Transfers2.lean). All comparisons use exact real arithmetic.

## Dossier discrepancies

- The logit-normal `fit.note` names `loginorm/README.md` and `loginorm/tnorm_mle.hpp`. The relevant sources are the logit-normal dossier and `logitnorm/mle.hpp`. Its collapse metadata says `zero scale`, while the C++ status is `LogitnormStatus::Unbounded`. The stated mathematical collapse condition agrees with the proof. [Logit-normal dossier](../../../univariateML-cpp/include/cppunivariate/distributions/logitnorm/README.md) · [Fitter](../../../univariateML-cpp/include/cppunivariate/distributions/logitnorm/mle.hpp)
- Nakagami’s `limits` metadata describes the held-scale route alone. Its existence paragraph also states the joint constant-sample collapse. Both use the same path, with the second moment equal to the common squared observation and the shape increasing without bound. [Nakagami dossier](../../../univariateML-cpp/include/cppunivariate/distributions/naka/README.md)
- The inverse-exponential and half-normal existence statements agree with the corresponding exact-real proofs. [Inverse-exponential dossier](../../../univariateML-cpp/include/cppunivariate/distributions/invexp/README.md) · [Half-normal dossier](../../../univariateML-cpp/include/cppunivariate/distributions/hnorm/README.md)

## Scope and verification

The fit results assume a nonempty sample. The half-normal log objective is compared on endpoints at or below every observation, where the likelihood is positive. The folded density is zero at incompatible endpoints, so those endpoints cannot compete with a positive likelihood. For Nakagami, the gamma base module uses a rate and a mean log likelihood, so the transfer adjusts both the parameterization and the normalization. The fixed logit-normal and half-normal coefficient pairs can be evaluated directly with their likelihood identities.

All four requested targets are settled. Floating-point fit accuracy and certification are outside the scope of these proofs. The coordinator’s import is pending. The final Lake command explicitly included the new module alongside the existing library.

Both final checks passed on the workstation through `/tmp/umvl-cpu/run` with `nice -n 10`, from `dev/lean/`.

- `/home/jonas/.elan/bin/lake env lean UnivariateML/Family/Transfers2.lean` exited 0 with no diagnostics.
- `/home/jonas/.elan/bin/lake build UnivariateML +UnivariateML.Family.Transfers2` exited 0 and reported `Build completed successfully (7388 jobs).` The new module built in 5.9 seconds. Lake replayed existing diagnostics from other modules.

## Effort

These are approximate hands-on times by target, excluding the shared build and report.

| Target | Effort | Main work |
| --- | ---: | --- |
| Nakagami | 4 min | Parameter and objective transfer, both collapse cases, and the constrained shape score |
| Logit-normal | 2 min | Logit injectivity, objective identity, and normal fit transfers |
| Inverse exponential | 1 min | Rate identity, harmonic-mean transfer, and held evaluation |
| Half-normal | 3 min | Supported folding, scale collapse, endpoint monotonicity, and joint fit |

## Nakagami

The following declarations are in `UnivariateML.Nakagami`.

### `logLik_square`

Squaring the data gives a gamma likelihood with the same shape, gamma scale Ω/m, and gamma rate m/Ω. The total log likelihood multiplies the gamma mean log likelihood by n and adds the log of the Jacobian 2x for every observation.

```lean
theorem logLik_square (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {m Ω : ℝ} (hm : 0 < m) (hΩ : 0 < Ω) :
    logLik x m Ω = n * Gamma.logLik (squared x) m (m / Ω) + ∑ i, log (2 * x i)
```

### `jointMLE`

Every nonconstant positive sample has one global maximizer over positive shape and second moment. Squaring is injective on positive observations, and the gamma parameter map is an involution on positive pairs.

```lean
theorem jointMLE (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) :
    ∃! p : ℝ × ℝ, p ∈ domain ∧ IsMaxOn (fun q ↦ logLik x q.1 q.2) domain p
```

### `heldShape`

At any positive held shape the empirical second moment is the unique maximizing scale. Constant data are included.

```lean
theorem heldShape (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {m : ℝ} (hm : 0 < m) (Ω : ℝ) :
    Ω ∈ Ioi 0 ∧ IsMaxOn (logLik x m) (Ioi 0) Ω ↔ Ω = Gamma.avg (squared x)
```

### `constant_unbounded`

When all squares equal the held second moment, the log likelihood tends to positive infinity as shape increases.

```lean
theorem constant_unbounded (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {Ω : ℝ} (hΩ : 0 < Ω) (h : ∀ i, squared x i = Ω) :
    Tendsto (fun m ↦ logLik x m Ω) atTop atTop
```

### `heldScale`

For a positive held second moment with at least one differing square, the mean square-ratio deviance is positive. The gamma score gap crosses that value once, and the crossing is exactly the global maximizing shape. The proof uses the derivative of the constrained gamma likelihood, Darboux’s theorem, and the gamma gap’s strict decrease and endpoint limits.

```lean
theorem heldScale (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {Ω : ℝ} (hΩ : 0 < Ω) (h : ∃ i, squared x i ≠ Ω) :
    (∃! m, m ∈ Ioi 0 ∧ Gamma.scoreGap m = heldGap x Ω) ∧
    ∀ m ∈ Ioi 0, IsMaxOn (fun k ↦ logLik x k Ω) (Ioi 0) m ↔
      Gamma.scoreGap m = heldGap x Ω
```

### `joint_coefficients`

At the joint maximum the second moment is empirical, and the shape solves the gamma score equation with the Jensen gap of the squared sample.

```lean
theorem joint_coefficients (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) {p : ℝ × ℝ} (hp : p ∈ domain)
    (hm : IsMaxOn (fun q ↦ logLik x q.1 q.2) domain p) :
    p.2 = Gamma.avg (squared x) ∧ Gamma.scoreGap p.1 = Gamma.jensenGap (squared x)
```

### `joint_unbounded`

For a constant positive sample, choosing the common squared value as the second moment gives unbounded joint log likelihood.

```lean
theorem joint_unbounded (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (h : ¬ ∃ i j, x i ≠ x j) :
    ¬ BddAbove ((fun q ↦ logLik x q.1 q.2) '' domain)
```

### `bothHeld`

Holding positive shape and second moment leaves a singleton feasible set and one likelihood evaluation.

```lean
theorem bothHeld {m Ω : ℝ} (hm : 0 < m) (hΩ : 0 < Ω) (x : Fin n → ℝ) :
    domain ∩ {(m, Ω)} = {(m, Ω)} ∧ IsMaxOn (fun q ↦ logLik x q.1 q.2) {(m, Ω)} (m, Ω)
```

## Logit-normal

The following declarations are in `UnivariateML.LogitNormal`.

### `logLik_logit`

The normal log likelihood on logits gains a data-only correction, the sum of the log Jacobians of the logit transformation.

```lean
theorem logLik_logit (x : Fin n → ℝ) (μ σ : ℝ) :
    logLik x μ σ = Normal.logLik (logits x) μ σ - ∑ i, log (x i * (1 - x i))
```

### `logLik_formula`

On the open unit interval, the transformed objective expands to the dossier’s log normalizer, logit residual sum of squares, and both log Jacobian terms.

```lean
theorem logLik_formula {x : Fin n → ℝ} (hx : ∀ i, x i ∈ Ioo 0 1)
    (μ : ℝ) {σ : ℝ} (hσ : 0 < σ) :
    logLik x μ σ = -(n : ℝ) / 2 * log (2 * π) - n * log σ -
      (∑ i, (logit (x i) - μ) ^ 2) / (2 * σ ^ 2) -
      ∑ i, log (x i) - ∑ i, log (1 - x i)
```

### `jointMLE`

For a nonconstant sample in the open unit interval, the joint fit is the logit mean and the root mean squared logit residual about that mean.

```lean
theorem jointMLE (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, x i ∈ Ioo 0 1)
    (hne : ∃ i j, x i ≠ x j) (p : ℝ × ℝ) :
    p ∈ Normal.domain ∧
      IsMaxOn (fun q : ℝ × ℝ ↦ logLik x q.1 q.2) Normal.domain p ↔
    p = (Normal.mean (logits x), Normal.sd (logits x) (Normal.mean (logits x)))
```

### `constant_unbounded`

Constant observations yield unbounded joint log likelihood.

```lean
theorem constant_unbounded (hn : 0 < n) {x : Fin n → ℝ} (h : ¬ ∃ i j, x i ≠ x j) :
    ¬ BddAbove ((fun q : ℝ × ℝ ↦ logLik x q.1 q.2) '' Normal.domain)
```

### `heldScale`

At a positive held scale the unique maximizing location is the logit mean, including on a constant sample.

```lean
theorem heldScale (hn : 0 < n) (x : Fin n → ℝ) {σ : ℝ} (hσ : 0 < σ) (μ : ℝ) :
    IsMaxOn (fun t ↦ logLik x t σ) univ μ ↔ μ = Normal.mean (logits x)
```

### `existsUnique_heldLocation_iff`

At a held location there is a unique positive scale fit exactly when at least one logit differs from that location.

```lean
theorem existsUnique_heldLocation_iff (hn : 0 < n) (x : Fin n → ℝ) (μ : ℝ) :
    (∃! σ, σ ∈ Ioi 0 ∧ IsMaxOn (logLik x μ) (Ioi 0) σ) ↔ ∃ i, logits x i ≠ μ
```

### `heldLocation`

When the held location leaves a nonzero residual, the fitted scale is the root mean squared residual about it.

```lean
theorem heldLocation (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ}
    (h : ∃ i, logits x i ≠ μ) (σ : ℝ) :
    σ ∈ Ioi 0 ∧ IsMaxOn (logLik x μ) (Ioi 0) σ ↔ σ = Normal.sd (logits x) μ
```

### `existsUnique_mle_iff`

The joint fit exists uniquely exactly when the sample in the open unit interval is nonconstant.

```lean
theorem existsUnique_mle_iff (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, x i ∈ Ioo 0 1) :
    (∃! p : ℝ × ℝ, p ∈ Normal.domain ∧
      IsMaxOn (fun q : ℝ × ℝ ↦ logLik x q.1 q.2) Normal.domain p) ↔ ∃ i j, x i ≠ x j
```

### `heldLocation_unbounded`

If all logits equal the held location, the scale slice has unbounded log likelihood.

```lean
theorem heldLocation_unbounded (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ}
    (h : ∀ i, logits x i = μ) : ¬ BddAbove (logLik x μ '' Ioi 0)
```

## Inverse exponential

The following declarations are in `UnivariateML.InverseExponential`.

### `logLik_reciprocal`

The inverse-exponential scale is the exponential rate on reciprocal data. The Jacobian contributes the data-only term −2 times the sum of log observations.

```lean
theorem logLik_reciprocal (x : Fin n → ℝ) (θ : ℝ) :
    logLik x θ = Exponential.logLik (reciprocal x) θ - 2 * ∑ i, log (x i)
```

### `mle`

Every nonempty positive sample, including every constant positive sample, has the harmonic mean as its unique scale fit.

```lean
theorem mle (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) (θ : ℝ) :
    θ ∈ Ioi 0 ∧ IsMaxOn (logLik x) (Ioi 0) θ ↔ θ = scale x
```

### `heldScale`

Holding a positive scale leaves only evaluation at that scale.

```lean
theorem heldScale (x : Fin n → ℝ) {θ : ℝ} (hθ : 0 < θ) :
    Ioi (0 : ℝ) ∩ {θ} = {θ} ∧ IsMaxOn (logLik x) {θ} θ
```

## Translated half-normal

The following declarations are in `UnivariateML.HalfNormal`.

### `logLik_normal`

At an endpoint compatible with the observations, the folded density doubles the centered normal density on the residuals. The objective gains n log 2, including the endpoint under the dossier’s density convention.

```lean
theorem logLik_normal (x : Fin n → ℝ) (μ : ℝ) {σ : ℝ} (hσ : 0 < σ) :
    logLik x μ σ = Normal.logLik (residuals x μ) 0 σ + n * log 2
```

### `heldLocation`

At a compatible held endpoint the unique scale is the root mean squared residual when some observation exceeds the endpoint. Setting μ to zero gives the ordinary half-normal.

```lean
theorem heldLocation (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ}
    (_hx : ∀ i, μ ≤ x i) (h : ∃ i, μ < x i) (σ : ℝ) :
    σ ∈ Ioi 0 ∧ IsMaxOn (logLik x μ) (Ioi 0) σ ↔ σ = scale x μ
```

### `heldLocation_unbounded`

When all observations equal the held endpoint, the log likelihood is unbounded over positive scale.

```lean
theorem heldLocation_unbounded (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ}
    (hx : ∀ i, x i = μ) : ¬ BddAbove (logLik x μ '' Ioi 0)
```

### `existsUnique_heldLocation_iff`

For a compatible held endpoint the scale fit exists uniquely exactly when some observation exceeds the endpoint.

```lean
theorem existsUnique_heldLocation_iff (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ}
    (hx : ∀ i, μ ≤ x i) :
    (∃! σ, σ ∈ Ioi 0 ∧ IsMaxOn (logLik x μ) (Ioi 0) σ) ↔ ∃ i, μ < x i
```

### `heldScale`

For each positive held scale the sample minimum is the unique maximizing endpoint. This also holds for singleton and constant samples.

```lean
theorem heldScale (hn : 0 < n) (x : Fin n → ℝ) {σ : ℝ} (hσ : 0 < σ) (μ : ℝ) :
    μ ∈ {t | ∀ i, t ≤ x i} ∧
      IsMaxOn (fun t ↦ logLik x t σ) {t | ∀ i, t ≤ x i} μ ↔
    μ = Endpoint.sampleMin hn x
```

### `jointMLE`

For nonconstant data the unique joint fit is the sample minimum and the root mean squared residual about it.

```lean
theorem jointMLE (hn : 0 < n) {x : Fin n → ℝ} (hne : ∃ i j, x i ≠ x j) (p : ℝ × ℝ) :
    p ∈ domain x ∧ IsMaxOn (fun q ↦ logLik x q.1 q.2) (domain x) p ↔
    p = (Endpoint.sampleMin hn x, scale x (Endpoint.sampleMin hn x))
```

### `constant_unbounded`

For constant data the scale can collapse at the sample minimum, giving unbounded joint log likelihood.

```lean
theorem constant_unbounded (hn : 0 < n) {x : Fin n → ℝ} (h : ¬ ∃ i j, x i ≠ x j) :
    ¬ BddAbove ((fun q ↦ logLik x q.1 q.2) '' domain x)
```

### `existsUnique_mle_iff`

Exactly the nonconstant nonempty samples give a unique joint fit.

```lean
theorem existsUnique_mle_iff (hn : 0 < n) (x : Fin n → ℝ) :
    (∃! p : ℝ × ℝ, p ∈ domain x ∧ IsMaxOn (fun q ↦ logLik x q.1 q.2) (domain x) p) ↔
    ∃ i j, x i ≠ x j
```
