# Lean evaluation of Weibull and inverse Weibull MLEs

TASK-294. All six targets are proved for nonempty, strictly positive,
uncensored samples in exact real arithmetic. The Lean module uses `lam`
for scale λ and `domain` for the product of the two positive half-lines in
shape–scale order. [Lean module](../../lean/UnivariateML/Family/Weibull.lean)

The parameterizations follow the two dossiers.
[Weibull dossier](../../../univariateML-cpp/include/cppunivariate/distributions/weibull/README.md)
[Inverse Weibull dossier](../../../univariateML-cpp/include/cppunivariate/distributions/invweibull/README.md)

## Dossier findings

**The inverse Weibull `limits` front matter omits the all-free
constant-sample escape.** The prose and existence metadata state this
case correctly. Lean proves that the log-likelihood tends to positive infinity
with scale fixed at the common observation while shape increases without
bound. The Weibull dossier already lists this escape for both the all-free
and held-scale fits.

The Weibull dossier writes its profile score per observation. The inverse
Weibull dossier’s $-n/k^2$ term uses the total log-likelihood score. With
weights normalized to sum to one, the total score derivative is
$-n/k^2-n\operatorname{Var}_w(z)$. The per-observation derivative is
$-1/k^2-\operatorname{Var}_w(z)$. Lean states the per-observation score and
its relation to the derivative of the total profile explicitly for both
families.

Both dossiers describe the joint C++ fit through a Gumbel delegate. The
proof instead profiles scale directly in the original likelihood and uses
the profile score. Inverse Weibull is transferred through reciprocation.
The conclusions match the dossiers’ existence statements for uncensored
positive samples. Floating-point reconstruction, numerical certificates,
and right-censored fits are outside this result.

## Weibull

The following statements are in `UnivariateML.Weibull`.

### 1. Held shape

The proposed scale is positive, its $k$th power equals the sample mean of $x_i^k$, and
every other positive scale gives a strictly smaller log-likelihood. Constant samples are
included.

```lean
theorem heldShape (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {k : ℝ} (hk : 0 < k) :
    0 < scale x k ∧ (scale x k) ^ k = avg (fun i ↦ (x i) ^ k) ∧
      ∀ lam > 0, lam ≠ scale x k → logLik x k lam < profile x k
```

### 2. Shape profile

The derivative of the total profiled log-likelihood is the sample size times the per-
observation score. The score uses exponential weights $e^{k\log x_i}=x_i^k$.

```lean
theorem hasDerivAt_profile (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {k : ℝ} (hk : 0 < k) : HasDerivAt (profile x) (n * score x k) k
```

The score strictly decreases for positive shapes. The weighted log mean has a
nonnegative derivative given by its weighted variance, and $1/k$ strictly decreases.

```lean
theorem score_strictAnti (hn : 0 < n) (x : Fin n → ℝ) :
    StrictAntiOn (score x) (Ioi 0)
```

### 3. Score limits and joint MLE

The score tends to positive infinity as shape decreases to zero.

```lean
theorem score_tendsto_zero (hn : 0 < n) (x : Fin n → ℝ) :
    Tendsto (score x) (𝓝[>] 0) atTop
```

Taking any maximizer of the observations as the reference, the score tends to the mean
log observation minus that largest log observation. Tied maxima share the limiting
weight.

```lean
theorem score_tendsto_atTop (x : Fin n → ℝ) {j : Fin n}
    (hj : ∀ i, x i ≤ x j) (hx : ∀ i, 0 < x i) :
    Tendsto (score x) atTop (𝓝 (avg (logSample x) - log (x j)))
```

For nonconstant observations the limiting score is strictly negative.

```lean
theorem score_limit_neg (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {j : Fin n} (hj : ∀ i, x i ≤ x j) (hne : ∃ a b, x a ≠ x b) :
    avg (logSample x) - log (x j) < 0
```

The score zero gives the unique maximizing shape of the profile. Recovering its unique
maximizing scale gives the unique joint MLE over positive shape and scale.

```lean
theorem jointMLE (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ a b, x a ≠ x b) :
    ∃! p : ℝ × ℝ, p ∈ domain ∧ IsMaxOn (fun q ↦ logLik x q.1 q.2) domain p
```

### 4. Constant samples

For a constant positive sample, fixing scale at the common observation and sending shape
to infinity drives the log-likelihood to positive infinity.

```lean
theorem constant_unbounded (hn : 0 < n) {c : ℝ} (hc : 0 < c) :
    Tendsto (fun k ↦ logLik (fun _ : Fin n ↦ c) k c) atTop atTop
```

### 5. Held scale

At any positive scale, the log-likelihood is strictly concave in positive shape,
including the constant case.

```lean
theorem heldScale_strictConcave (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {lam : ℝ} (hl : 0 < lam) :
    StrictConcaveOn ℝ (Ioi 0) (fun k ↦ logLik x k lam)
```

If any observation differs from the held scale, the shape score has a zero and there is
a unique maximizing positive shape.

```lean
theorem heldScale (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {lam : ℝ} (hl : 0 < lam) (hne : ∃ i, x i ≠ lam) :
    ∃! k, k ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun t ↦ logLik x t lam) (Ioi 0) k
```

When every observation equals the held scale, the log-likelihood diverges to positive
infinity with shape. Strict concavity still holds.

```lean
theorem heldScale_unbounded (hn : 0 < n) {x : Fin n → ℝ} {lam : ℝ} (hl : 0 < lam)
    (he : ∀ i, x i = lam) :
    Tendsto (fun k ↦ logLik x k lam) atTop atTop
```

## 6. Inverse Weibull

The statements below are in `UnivariateML.Weibull.Inverse`. The reciprocal observations
and reciprocal scale give a Weibull log-likelihood, with the parameter-free log Jacobian
$-2\sum_i\log x_i$ added.

```lean
theorem logLik_reciprocal {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (k : ℝ) {lam : ℝ} (hl : 0 < lam) :
    logLik x k lam = Weibull.logLik (reciprocal x) k lam⁻¹ - 2 * ∑ i, log (x i)
```

At held shape the unique maximizing inverse Weibull scale is positive and satisfies
$\lambda^{-k}=n^{-1}\sum_i x_i^{-k}$.

```lean
theorem heldShape (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {k : ℝ} (hk : 0 < k) :
    0 < scale x k ∧ (scale x k) ^ (-k) = avg (fun i ↦ (x i) ^ (-k)) ∧
      ∀ lam > 0, lam ≠ scale x k → logLik x k lam < profile x k
```

The parameter-free shift leaves the derivative of the reciprocal-data Weibull profile unchanged.

```lean
theorem hasDerivAt_profile (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {k : ℝ} (hk : 0 < k) :
    HasDerivAt (profile x) (n * score x k) k
```

The inverse Weibull profile score strictly decreases on positive shapes.

```lean
theorem score_strictAnti (hn : 0 < n) (x : Fin n → ℝ) :
    StrictAntiOn (score x) (Ioi 0)
```

The inverse Weibull score tends to positive infinity at zero from the right.

```lean
theorem score_tendsto_zero (hn : 0 < n) (x : Fin n → ℝ) :
    Tendsto (score x) (𝓝[>] 0) atTop
```

The large-shape limit is the smallest log observation minus the mean log observation,
because reciprocation reverses the order.

```lean
theorem score_tendsto_atTop (x : Fin n → ℝ) {j : Fin n}
    (hj : ∀ i, x j ≤ x i) (hx : ∀ i, 0 < x i) :
    Tendsto (score x) atTop (𝓝 (log (x j) - avg (logSample x)))
```

That limit is strictly negative for nonconstant positive observations.

```lean
theorem score_limit_neg (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {j : Fin n} (hj : ∀ i, x j ≤ x i) (hne : ∃ a b, x a ≠ x b) :
    log (x j) - avg (logSample x) < 0
```

Reciprocating the Weibull scale transfers the unique joint maximizer for nonconstant observations.

```lean
theorem jointMLE (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ a b, x a ≠ x b) :
    ∃! p : ℝ × ℝ, p ∈ domain ∧ IsMaxOn (fun q ↦ logLik x q.1 q.2) domain p
```

At their common scale, constant inverse Weibull observations give log-likelihood
diverging to positive infinity with shape.

```lean
theorem constant_unbounded (hn : 0 < n) {c : ℝ} (hc : 0 < c) :
    Tendsto (fun k ↦ logLik (fun _ : Fin n ↦ c) k c) atTop atTop
```

At positive held scale the inverse Weibull log-likelihood is strictly concave in positive shape.

```lean
theorem heldScale_strictConcave (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {lam : ℝ} (hl : 0 < lam) :
    StrictConcaveOn ℝ (Ioi 0) (fun k ↦ logLik x k lam)
```

At held scale, any observation different from that scale suffices for a unique
maximizing positive shape.

```lean
theorem heldScale (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {lam : ℝ} (hl : 0 < lam) (hne : ∃ i, x i ≠ lam) :
    ∃! k, k ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun t ↦ logLik x t lam) (Ioi 0) k
```

If every observation equals the held scale, the inverse Weibull log-likelihood diverges
to positive infinity with shape.

```lean
theorem heldScale_unbounded (hn : 0 < n) {x : Fin n → ℝ} {lam : ℝ} (hl : 0 < lam)
    (he : ∀ i, x i = lam) :
    Tendsto (fun k ↦ logLik x k lam) atTop atTop
```

## Proof effort

| Target | Relative effort | Main proof work |
| --- | --- | --- |
| 1 | Moderate | Rewrite the scale-dependent likelihood in terms of a positive ratio and apply the strict inequality `log t < t - 1` away from one. |
| 2 | Moderate | Differentiate the profile and the finite weighted mean. Reexpress the quotient-rule numerator as a sum of weighted squares. |
| 3 | Largest | Factor out the largest exponential weight, track tied maxima in the limit, bracket the score zero, and lift the strict profile maximum to both coefficients. |
| 4 | Short | At the common scale the likelihood reduces to a positive multiple of `log k` plus a constant. |
| 5 | Moderate | Each data term of the score is nonincreasing. One nonzero log ratio gives a negative upper bound on the sum for shapes at least one. The reciprocal term vanishes, so the same score-root argument gives existence and uniqueness. |
| 6 | Moderate | Establish the additive Jacobian identity and transfer scale, maximizers, concavity, unbounded paths, and the score limits through reciprocation. |

## Verification and open items

- On the workstation, `lake env lean UnivariateML/Family/Weibull.lean`
  passed without warnings through the single-slot wrapper.
- On the workstation, `lake build` passed without warnings through the
  same wrapper. Lake reported `Build completed successfully (6594 jobs).`
  It used cached Mathlib and built the Lomax module and the library root.
  The standalone check covers Weibull pending its import by the coordinator.
- All requested mathematical results are closed, with no proof placeholders
  or added axioms. There are no open items within the six targets.
