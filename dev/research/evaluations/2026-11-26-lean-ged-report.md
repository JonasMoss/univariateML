# GED fixed-shape likelihood in Lean

TASK-369 · Base `2d89fd4a` · Namespace `UnivariateML.Ged`

The module proves all four fixed-shape targets in exact real arithmetic. It uses the standard deviation as the public scale and includes the shape-dependent normalization constant in the total log likelihood. [Ged.lean](../../lean/UnivariateML/Family/Ged.lean)

## Source findings

- **The dossier's free-shape uniqueness wording is too broad.** Its existence block calls global uniqueness with free shape unknown. The math report gives an all-free binary64 example whose entire joint maximizing set is a nontrivial median interval at shape one and a common scale. This rules out universal joint uniqueness. The example leaves open a different question about the number of maximizing shape values in the fully free fit. [GED existence block](../../../univariateML-cpp/include/cppunivariate/distributions/ged/README.md) · [All-free median interval](2026-11-25-ged-math-report.md#exact-finiteuniform-equality-and-an-all-free-median-interval)
- **The dossier's closing description of unresolved comparisons omits the successful-search fallback.** The Details section and the math report distinguish an unresolved certificate from a failed search. The current `ged_uncertified` can return an accuracy-checked search result marked uncertified when the available comparison bounds permit it. [GED Details](../../../univariateML-cpp/include/cppunivariate/distributions/ged/README.md#details) · [GED fitter](../../../univariateML-cpp/include/cppunivariate/distributions/ged/mle.hpp#L526) · [Math report](2026-11-25-ged-math-report.md#what-this-changes-for-the-fitter)
- The fixed-shape claims agree with the math report. The shape-one kink allows nonunique locations, and their common standard deviation is √2 times the mean absolute deviation. Strict inverse-coordinate concavity above shape one requires a nonempty sample. Attainment also requires nonconstancy when both location and scale are free. [Fixed-shape formulas](2026-11-25-ged-math-report.md#existence-on-the-eight-masks)

## Parameterization and proof scope

`residualSum ν x μ` is the sum of the absolute residuals raised to ν. The candidate `bestScale ν x μ` is

```text
sqrt(Γ(3/ν) / Γ(1/ν)) * (ν * residualSum ν x μ / n)^(1/ν).
```

Equivalently, this is the residual power mean times `ν^(1/ν) * sqrt(Γ(3/ν) / Γ(1/ν))`. `LocationScale.domain` is the set of location and positive standard-deviation pairs. `LocationScale.Nonconstant x` means that two observations differ. The inverse chart orders the coordinates as `(b, u) = (1/σ, μ/σ)` to match the generic library. This reverses the order of the coordinates in the dossier. The scale formula, collapse, and held-scale loss comparison also apply to positive shapes below one.

The convex-loss instance uses strict convexity of the absolute power above one and the triangle inequality at one. The linear lower bound `c * (|z| - 1)`, with `c = sdFactor ν ^ ν > 0`, lets the generic location–scale existence theorem use its cocompact escape argument. The Laplace module supplies the median interval and equality of the absolute deviation across that interval. The conditional-scale proof uses the strict logarithm tangent inequality at `(bestScale / σ)^ν`. Two fixed-coordinate comparisons give the joint graph directly. [LocationScale](../../lean/UnivariateML/Generic/LocationScale.lean) · [Escape](../../lean/UnivariateML/Generic/Escape.lean) · [Laplace](../../lean/UnivariateML/Family/Laplace.lean)

## Theorems

### Inverse-scale likelihood

The bijection to reciprocal-scale coordinates preserves the normalized log likelihood.

```lean
theorem inverseLogLik_eq (ν : ℝ) (x : Fin n → ℝ) (p : LocationScale.domain) :
    inverseLogLik ν x (LocationScale.inverseEquiv p) = logLik ν x p
```

The normalized log likelihood is concave in reciprocal-scale coordinates at every fixed shape at least one.

```lean
theorem concave_inverseLogLik {ν : ℝ} (hν : 1 ≤ ν) (x : Fin n → ℝ) :
    ConcaveOn ℝ LocationScale.inverseDomain (inverseLogLik ν x)
```

Above shape one this concavity is strict on every nonempty sample, including constant samples.

```lean
theorem strictConcave_inverseLogLik {ν : ℝ} (hν : 1 < ν) (x : Fin n → ℝ) (hn : 0 < n) :
    StrictConcaveOn ℝ LocationScale.inverseDomain (inverseLogLik ν x)
```

### Joint attainment and location set

Nonconstant data have an attained joint maximum at every fixed shape at least one.

```lean
theorem joint_exists {ν : ℝ} (hν : 1 ≤ ν) {x : Fin n → ℝ}
    (hx : LocationScale.Nonconstant x) :
    (jointMaximizers ν x).Nonempty
```

Above shape one this joint maximum is unique.

```lean
theorem joint_existsUnique {ν : ℝ} (hν : 1 < ν) {x : Fin n → ℝ}
    (hx : LocationScale.Nonconstant x) :
    ∃! p, p ∈ jointMaximizers ν x
```

For a nonempty sample and a fixed shape at least one, an attained joint maximum exists exactly when two observations differ.

```lean
theorem joint_exists_iff {ν : ℝ} (hν : 1 ≤ ν) (hn : 0 < n) (x : Fin n → ℝ) :
    (jointMaximizers ν x).Nonempty ↔ LocationScale.Nonconstant x
```

For nonconstant data, the complete maximizing set consists of the residual-loss minimizers paired with their conditional standard deviations.

```lean
theorem joint_graph {ν : ℝ} (hν : 0 < ν) (hn : 0 < n) {x : Fin n → ℝ}
    (hx : LocationScale.Nonconstant x) :
    jointMaximizers ν x =
      {p | IsMinOn (residualSum ν x) univ p.1 ∧ p.2 = bestScale ν x p.1}
```

At shape one this set is the full closed median interval at one common standard deviation. The lower median gives a representative for evaluating that standard deviation.

```lean
theorem joint_one (hn : 0 < n) {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x) :
    jointMaximizers 1 x =
      Laplace.medianSet x hn ×ˢ {sqrt 2 * Laplace.meanAbsDev x (Laplace.lowerMedian x hn)}
```

### Scale collapse

At a location shared by every observation, the log likelihood tends to positive infinity as the scale tends to zero from above.

```lean
theorem collapse {ν : ℝ} (hν : 0 < ν) (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ}
    (hx : ∀ i, x i = μ) :
    Tendsto (fun σ ↦ logLik ν x (μ, σ)) (𝓝[>] 0) atTop
```

The likelihood itself also tends to positive infinity along that path.

```lean
theorem collapse_likelihood {ν : ℝ} (hν : 0 < ν) (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ}
    (hx : ∀ i, x i = μ) :
    Tendsto (fun σ ↦ exp (logLik ν x (μ, σ))) (𝓝[>] 0) atTop
```

### One coefficient held

If any residual is nonzero, the explicit conditional standard deviation is positive and is the unique maximizer over positive scales.

```lean
theorem heldLocation {ν : ℝ} (hν : 0 < ν) (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ}
    (hx : ∃ i, x i ≠ μ) :
    0 < bestScale ν x μ ∧
      ∀ σ > 0, IsMaxOn (fun s ↦ logLik ν x (μ, s)) (Ioi 0) σ ↔ σ = bestScale ν x μ
```

At a held positive standard deviation, the maximizing locations are exactly the minimizers of the residual-power sum.

```lean
theorem heldScale (ν : ℝ) (x : Fin n → ℝ) {σ μ : ℝ} (hν : 0 < ν) (hσ : 0 < σ) :
    IsMaxOn (fun m ↦ logLik ν x (m, σ)) univ μ ↔ IsMinOn (residualSum ν x) univ μ
```

For shape above one and a nonempty sample, exactly one location maximizes at positive held scale, even on constant data.

```lean
theorem heldScale_existsUnique {ν : ℝ} (hν : 1 < ν) (hn : 0 < n) (x : Fin n → ℝ)
    {σ : ℝ} (hσ : 0 < σ) :
    ∃! μ, IsMaxOn (fun m ↦ logLik ν x (m, σ)) univ μ
```

At shape one and positive held scale, the full maximizing location set is the closed median interval.

```lean
theorem heldScale_one (hn : 0 < n) (x : Fin n → ℝ) {σ : ℝ} (hσ : 0 < σ) :
    {μ | IsMaxOn (fun m ↦ logLik 1 x (m, σ)) univ μ} = Laplace.medianSet x hn
```

At shape one the explicit scale formula simplifies to the mean absolute deviation times √2.

```lean
theorem bestScale_one (x : Fin n → ℝ) (μ : ℝ) :
    bestScale 1 x μ = sqrt 2 * Laplace.meanAbsDev x μ
```

## Effort per target

| Target | Proof effort |
|---|---|
| 1. Concavity | Proved strict convexity of the absolute-power loss, including equal absolute values at opposite inputs, then specialized the generic inverse-coordinate results. |
| 2. Joint existence, uniqueness, and location set | Supplied the linear bound, transferred attainment and uniqueness across normalization, and used the two held-coordinate comparisons and the Laplace median results to characterize the full maximizing set. |
| 3. Collapse | Reduced the zero-residual log likelihood to a constant minus `n * log σ` and took its right-hand limit. Composed with exponential for the likelihood. |
| 4. Held coefficients | Expanded the normalized likelihood by residual sum, proved the analytic scale optimizes strictly, and proved strict convexity and attainment of the held-scale location objective. |
| 5. Source review | Compared the fixed-shape claims and scale convention with the math report and fitter. The two dossier wording issues are recorded above. |

The main proof work was the strict absolute-power inequality, the analytic conditional scale, and the exact location–scale graph. CPU-slot waiting accounted for most of the elapsed time.

## Validation and open items

Both final checks exited zero on the workstation. They ran sequentially from `dev/lean/` in the foreground under `/tmp/umvl-cpu/run`, with CPU affinity set to one core and `LEAN_NUM_THREADS=1`.

- `nice -n 10 /home/jonas/.elan/bin/lake env lean UnivariateML/Family/Ged.lean` checked all 15 declarations without diagnostics.
- `nice -n 10 /home/jonas/.elan/bin/lake build` completed successfully with 7,452 jobs and replayed existing lint warnings in other modules. The direct check covers GED because the root import awaits coordinator integration.

The preparatory Lake build used the one-slot lock, but its `-Kjobs=1` configuration flag did not enforce a one-core CPU limit. All subsequent checks used CPU affinity. The builds used the existing Mathlib artifacts.

There are no open proof obligations for the requested fixed-shape targets. Free-shape comparisons, below-one location-set classification, and floating-point certificate soundness remain outside the module's scope.
