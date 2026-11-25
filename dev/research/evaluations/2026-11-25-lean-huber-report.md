# Held-shape Huber maximizer sets

TASK-337 · 2026-11-25

[Lean module](../../lean/UnivariateML/Family/Huber.lean) · [Huber dossier](../../../univariateML-cpp/include/cppunivariate/distributions/huber/README.md) · [C++ fitter](../../../univariateML-cpp/include/cppunivariate/distributions/huber/mle.hpp) · [Transfer report](2026-11-25-transfer-math-report.md)

## Fitter discrepancy

**The fitter returns one location and does not expose the full set or a nonuniqueness flag.** This matches the dossier's midpoint convention. It differs from the transfer report's call to return the full interval with a nonunique indication. The comparison is from source inspection. `huber_location` uses the even-sample central half-gap test with `>= epsilon*sigma` and returns the midpoint. `huber_fixed_solve` applies the same rule after the Newton solve. The final `huber_mle` polish tests the gap in scale-normalized coordinates, holds location when the private `flat` test succeeds, and checks that the gap still accommodates the polished scale. The `HuberResult` fields store one coefficient vector, and the `Ok` branch maps it to an ordinary estimate. The internal test includes equality. At exact equality the midpoint is the unique location. The strict inequality in the transfer report is the criterion for **nonuniqueness**.

## Scope and source fidelity

All theorems concern exact real arithmetic. The sample is `Fin n → ℝ` and the fitting statements use positive held shape and nonempty data. The objective is the dossier's log likelihood after dropping the parameter-independent normalization at held shape. The inverse chart is `(η, ν) = (1/σ, μ/σ)`; the physical joint-set theorems use pairs `(μ, σ)`.

Write `a = Laplace.lowerMedian x hn` and `b = Laplace.upperMedian x hn`. The eroded interval is `Icc (a + ε * σ) (b - ε * σ)`. An odd sample has equal central ranks, so a positive-width interval requires even size. When the interval is nonempty, the sorted lower half has clipped score `-ε` and the sorted upper half has clipped score `ε`, including the endpoints. Conversely, two distinct locations with the same maximal value must have identical clipped scores observation by observation. Each score must then be saturated, and the location equation balances the tails. The proof reuses the Laplace median characterization to identify the central bounds. These statements sharpen the dossier's possibility of a flat location interval without requiring additional assumptions on ties.

No held-shape mathematical claim remains open among the requested targets. Free shape and the floating-point soundness of the C++ selector and certificate remain outside the formalization. With both nuisance coefficients supplied, the feasible slice is a single point and there is no optimization.

## Theorems

Declarations below are copied from `UnivariateML.Huber`, with the proofs omitted. The module binds `{n : ℕ}`.

### Loss, concavity and attainment

```lean
theorem rho_eq_piecewise {ε z : ℝ} (hε : 0 ≤ ε) :
    rho ε z = if |z| ≤ ε then z ^ 2 / 2 else ε * (|z| - ε / 2)
```

The clipped-score expression is exactly the Huber loss used in C++, including at the transition.

```lean
theorem inverse_concave {ε : ℝ} (hε : 0 < ε) (x : Fin n → ℝ) :
    ConcaveOn ℝ LocationScale.inverseDomain (inverseLogLik ε x)
```

At positive held shape, the log likelihood is concave in inverse scale and scaled location.

```lean
theorem joint_exists {ε : ℝ} (hε : 0 < ε) {x : Fin n → ℝ}
    (hx : LocationScale.Nonconstant x) : (jointMaximizers ε x).Nonempty
```

Two distinct observations suffice for a finite joint maximum. Other observations may repeat. This uses the linear lower bound on the loss and the generic location–scale escape theorem.

```lean
theorem joint_constant_unbounded {ε : ℝ} (hn : 0 < n) (m : ℝ) :
    ¬ BddAbove (logLik ε (fun _ : Fin n ↦ m) '' LocationScale.domain)
```

For a nonempty constant sample and free location and scale, scale collapse at the common observation makes the likelihood unbounded.

### Joint maximizer set

```lean
theorem inverse_maximizers_convex {ε : ℝ} (hε : 0 < ε) (x : Fin n → ℝ) :
    Convex ℝ (inverseMaximizers ε x)
```

The full maximizer set is convex in the inverse chart.

```lean
theorem inverse_scale_eq (hn : 0 < n) {ε : ℝ} (hε : 0 < ε) {x : Fin n → ℝ}
    {p q : ℝ × ℝ} (hp : p ∈ inverseMaximizers ε x) (hq : q ∈ inverseMaximizers ε x) :
    p.1 = q.1
```

Any two attained maxima have the same inverse scale. Strictness comes from the logarithm even when the loss is affine along the location direction.

```lean
theorem joint_eq_product (hn : 0 < n) {ε : ℝ} (hε : 0 < ε) {x : Fin n → ℝ}
    {p : ℝ × ℝ} (hp : p ∈ jointMaximizers ε x) :
    jointMaximizers ε x = locationMaximizers ε x p.2 ×ˢ {p.2}
```

One attained maximizer identifies the common scale. The full joint set is the product of the held-scale location set and that scale.

```lean
theorem joint_interval (hn : 0 < n) {ε : ℝ} (hε : 0 < ε) {x : Fin n → ℝ}
    {p : ℝ × ℝ} (hp : p ∈ jointMaximizers ε x)
    (hgap : (erodedMedian ε x hn p.2).Nonempty) :
    jointMaximizers ε x = erodedMedian ε x hn p.2 ×ˢ {p.2}
```

If the eroded central interval at the common scale is nonempty, the joint set is exactly that interval at the common scale.

```lean
theorem joint_unique_iff (hn : 0 < n) {ε : ℝ} (hε : 0 < ε) {x : Fin n → ℝ}
    {p : ℝ × ℝ} (hp : p ∈ jointMaximizers ε x) :
    (∃! q, q ∈ jointMaximizers ε x) ↔
      Laplace.upperMedian x hn - Laplace.lowerMedian x hn ≤ 2 * (ε * p.2)
```

The joint maximum is unique exactly when the two central order statistics differ by at most two transition widths at the common scale.

### Held scale and held location

```lean
theorem mem_locationMaximizers_iff {ε σ : ℝ} (hε : 0 < ε) (hσ : 0 < σ)
    (x : Fin n → ℝ) (μ : ℝ) :
    μ ∈ locationMaximizers ε x σ ↔ locationScore ε x σ μ = 0
```

At held positive scale, the sum of the clipped standardized residuals is zero exactly at the maximizing locations.

```lean
theorem heldScale_exists (_hn : 0 < n) {ε σ : ℝ} (hε : 0 < ε) (hσ : 0 < σ)
    (x : Fin n → ℝ) : (locationMaximizers ε x σ).Nonempty
```

Every nonempty sample, including a constant sample, has a maximizing location at held positive scale.

```lean
theorem heldScale_interval (hn : 0 < n) {ε σ : ℝ} (hε : 0 < ε) (hσ : 0 < σ)
    {x : Fin n → ℝ} (hgap : (erodedMedian ε x hn σ).Nonempty) :
    locationMaximizers ε x σ = erodedMedian ε x hn σ
```

If eroding the central interval by one transition width at each end leaves an interval, that entire closed interval is the location maximizer set.

```lean
theorem heldScale_unique_iff (hn : 0 < n) {ε σ : ℝ} (hε : 0 < ε) (hσ : 0 < σ)
    (x : Fin n → ℝ) :
    (∃! μ, μ ∈ locationMaximizers ε x σ) ↔
      Laplace.upperMedian x hn - Laplace.lowerMedian x hn ≤ 2 * (ε * σ)
```

The maximizing location is unique for a gap no wider than two transition widths. At equality the eroded interval consists of its midpoint. For a narrower gap the unique location is characterized by the score equation.

```lean
theorem heldLocation_existsUnique {ε : ℝ} (hε : 0 < ε) {x : Fin n → ℝ} {μ : ℝ}
    (hx : ∃ i, x i ≠ μ) :
    ∃! σ, 0 < σ ∧ IsMaxOn (fun s ↦ logLik ε x (μ, s)) (Ioi 0) σ
```

With location held, any nonzero residual gives a unique positive maximizing scale. The proof brackets a root of the dimensionless scale score and uses the strict logarithm inequality to prove global optimality.

```lean
theorem heldLocation_unbounded (hn : 0 < n) {ε μ : ℝ} {x : Fin n → ℝ}
    (hx : ∀ i, x i = μ) :
    ∀ M : ℝ, ∃ σ > 0, M < logLik ε x (μ, σ)
```

With location held at every observation and scale free, every finite likelihood level is exceeded by some positive scale.

## Verification and effort

All final checks ran on the workstation through the shared single-slot runner with `nice -n 10`.

| Check | Result |
| --- | --- |
| `lake env lean UnivariateML/Family/Huber.lean` | Passed, with no diagnostics. |
| `lake build UnivariateML.Family.Huber` | Passed, 6675 jobs reported by Lake. This directly built the new module. |
| `lake build` | Passed, 7402 jobs reported by Lake. Existing modules emitted linter diagnostics. The default root module is awaiting the coordinator's Huber import. |

The new module has no proof placeholders or custom axioms. No C++, R or JavaScript checks were run.

| Target | Effort |
| --- | --- |
| 1. Concavity and existence | Moderate. Proved the clipped-loss identity, supporting inequality and linear lower bound, then reused the generic location–scale results. |
| 2. Convexity and exact sets | Largest. Proved common scale via strict logarithmic concavity, agreement of clipped scores via a quadratic gap, and both directions of the eroded central-interval characterization. |
| 3. Held scale and held location | Substantial. Derived the location score and used two root brackets, a strict scale comparison, and a direct zero-residual collapse argument. |
| 4. Fitter comparison | Short source review. Traced the midpoint selection, final polish and public result representation against the two source descriptions. |

Per-target elapsed times were not tracked. Shared CPU slot waits were part of the verification wall time.
