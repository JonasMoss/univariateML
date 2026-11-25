# Log-logistic MLE by logistic transfer

TASK-340 · 2026-11-25

Lean proves the exact likelihood identity, joint existence and uniqueness for nonconstant positive samples, and both one-parameter fits with their precise existence conditions. The theorems use public coefficient order `(shape, scale)` and total log likelihood. [Lean module](../../lean/UnivariateML/Family/LogLogistic.lean)

## Dossier findings

- **Held-scale exception.** The brief's unconditional wording for the two held fits requires this exception. A held scale admits a unique shape estimate exactly when some observation differs from that scale. A constant sample at a different value still admits the fit. The dossier states this correctly, and its reported unbounded case agrees with the fitter's constant-data branch. [Log-logistic dossier](../../../univariateML-cpp/include/cppunivariate/distributions/llogis/README.md#details) · [Fitter branch](../../../univariateML-cpp/include/cppunivariate/core/transformed_location.hpp#L44)
- **Implementation wording.** The dossier's centered `log1p` expression describes the near-anchor calculation. The implementation also uses a log of the ratio for normal finite quotients outside that branch and a difference of logs for extreme quotients. The exact likelihood and existence claims agree with the dossier. The Lean proof obtains uniqueness from strict concavity and establishes held-case attainment with explicit bounds. [Log-logistic dossier](../../../univariateML-cpp/include/cppunivariate/distributions/llogis/README.md#details) · [Log-ratio implementation](../../../univariateML-cpp/include/cppunivariate/core/transformed_data.hpp#L42)
- **Pareto III.** With the threshold held and all excesses strictly positive, the same conclusions apply to those excesses with the same shape and scale. The free-threshold problem has a separate endpoint argument in the Pareto III dossier. [Pareto III dossier](../../../univariateML-cpp/include/cppunivariate/distributions/pareto3/README.md#details)

## Lean statements

Namespace `UnivariateML.LogLogistic` with `n : ℕ`. Here `domain` is the set of pairs with both coefficients positive. `LocationScale.domain` permits any logistic location and a positive logistic scale.

**`toLogistic_bijOn`.** The positive log-logistic pair `(β, α)` maps bijectively to the logistic pair `(μ, σ) = (log α, 1/β)`. The inverse is `(1/σ, exp μ)`.

```lean
theorem toLogistic_bijOn : BijOn toLogistic domain LocationScale.domain
```

**`logLik_transfer`.** On positive observations and feasible coefficients, the total log-logistic log likelihood is the logistic log likelihood of the logged observations minus the sum of their logs. The correction is the log Jacobian and depends only on the sample.

```lean
theorem logLik_transfer {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {p : ℝ × ℝ} (hp : p ∈ domain) :
    logLik x p =
      LocationScale.logLik logisticLoss (fun i ↦ log (x i)) (toLogistic p) -
        ∑ i, log (x i)
```

**`jointMLE`.** Every nonconstant positive sample has exactly one joint maximizer. Taking logs preserves distinct observations, so the result follows from the existing logistic MLE theorem and the generic transfer of unique maximizers.

```lean
theorem jointMLE {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) (hne : Nonconstant x) :
    ∃! p, p ∈ domain ∧ IsMaxOn (logLik x) domain p
```

**`constant_unbounded`.** For a nonempty sample equal to a positive value `t`, the joint log likelihood has no finite upper bound. The feasible slice with scale fixed at `t` already has this property.

```lean
theorem constant_unbounded (hn : 0 < n) {t : ℝ} (ht : 0 < t) :
    ¬ BddAbove (logLik (fun _ : Fin n ↦ t) '' domain)
```

**`heldShape_unique`.** Every nonempty positive sample has a unique scale fit when shape is held positive, including constant samples. The logistic objective is strictly concave in log scale, and one observation suffices to bound the relevant superlevel set.

```lean
theorem heldShape_unique (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {β : ℝ} (hb : 0 < β) :
    ∃! α, α ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun α ↦ logLik x (β, α)) (Ioi 0) α
```

**`heldScale_unique`.** A positive held scale gives a unique shape fit when at least one observation differs from it. A single nonzero log residual bounds the objective from above by `n * log β - δ * β` for some `δ > 0`, which gives a compact positive interval containing the relevant superlevel set.

```lean
theorem heldScale_unique (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {α : ℝ} (ha : 0 < α) (hne : ∃ i, x i ≠ α) :
    ∃! β, β ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun β ↦ logLik x (β, α)) (Ioi 0) β
```

**`heldScale_unbounded`.** When every observation equals the held scale, the shape-only log likelihood is unbounded. The proof exhibits a positive shape exceeding each proposed upper bound using the constant-sample log likelihood `n * (log β - log α - 2 * log 2)`.

```lean
theorem heldScale_unbounded (hn : 0 < n) {α : ℝ} (ha : 0 < α) :
    ¬ BddAbove ((fun β ↦ logLik (fun _ : Fin n ↦ α) (β, α)) '' Ioi 0)
```

## Effort and verification

| Target | Relative effort | Proof work |
| --- | --- | --- |
| 1. Likelihood transfer | Moderate | Prove the parameter bijection, then reconcile the dossier's positive real power with the logistic loss and sum the log Jacobians. |
| 2. Joint MLE and constant data | Low | Transfer the existing unique logistic MLE. Prove constant-data divergence on the slice that holds scale at the common value. |
| 3. Held fits | Highest | Reuse the generic strict concavity of the inverse-scale objective on both affine slices. Prove compact bounds for the two superlevel sets and transfer the resulting maximizers. |
| 4. Dossier comparison | Low | Check the exact cases and compare the stated calculation with the log-logistic and Pareto III fitter routes. |

All requested exact-arithmetic targets are proved. Floating-point certification, the both-held evaluation route, and free-threshold Pareto III are outside these theorem statements.

All checks ran on the workstation through the single-slot wrapper using the existing Mathlib cache.

- `lake env lean UnivariateML/Family/LogLogistic.lean` passed without diagnostics.
- `lake build +UnivariateML.Family.LogLogistic` passed with 7,265 jobs and no diagnostics from the new module.
- Final plain `lake build` passed with 7,404 jobs. It emitted existing diagnostics in Fréchet, FoldedNormal, Weibull, Kumaraswamy, LomaxTwoPoint, Discrete and GPD. The explicit module build covers LogLogistic before the coordinator adds its root import.

The new module contains no proof placeholders.
