# Endpoint maximum likelihood in Lean

TASK-309 · 2026-11-25

**The moving endpoints must be closed for the fitted extremes to attain the
likelihood maximum.** The uniform dossier uses `[a,b]`, Pareto uses
`[b,∞)`, and power uses `(0,a]`. Lean also proves nonattainment for the
open uniform interval with both endpoints free and for either positive
family with its moving endpoint open and free. Each proof improves a
positive likelihood by moving an endpoint strictly toward the relevant
sample extreme. A positive feasible likelihood also beats every
zero-likelihood point. [Lean module](../../lean/UnivariateML/Family/Endpoint.lean)

## Dossier findings

- The uniform collapse detector says no held endpoint “lies strictly outside
  the observation”. The precise conditions are that a free endpoint remains,
  every held endpoint is compatible, and the candidate width is zero. For
  example, holding the lower endpoint above a constant sample gives zero
  likelihood for every valid upper endpoint. The fitter checks compatibility
  before width, in agreement with the proof.
  [Uniform dossier and fitter](../../../univariateML-cpp/include/cppunivariate/distributions/unif/README.md),
  [mle.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/unif/mle.hpp)
- Pareto records the unbounded path in `limits:` under the held-endpoint
  mask. The same fixed-endpoint path is feasible when both coefficients are
  free, as the existence prose and fitter state.
  [Pareto dossier and fitter](../../../univariateML-cpp/include/cppunivariate/distributions/pareto/README.md),
  [mle.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/pareto/mle.hpp)
- Power omits the unbounded path from `limits:`. Its existence prose,
  details, and fitter cover the case in both masks with free shape. It occurs
  when every observation equals the fitted or held upper endpoint.
  [Power dossier and fitter](../../../univariateML-cpp/include/cppunivariate/distributions/power/README.md),
  [mle.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/power/mle.hpp)

## Scope and proof

The statements use exact real arithmetic and nonempty, uncensored samples.
Pareto and power require positive observations for a positive likelihood.
The formal pair order is endpoint then shape, namely `(b, α)` for Pareto
and `(a, b)` for power. An option holds its coefficient when it is `some`
and leaves it free when it is `none`. Each `domain` contains the valid
parameter pairs obeying those holds. The likelihood extends the exponential
of the log-likelihood by zero outside sample support. A nonempty all-zero
domain makes every point a mathematical maximizer and yields the package's
incompatibility diagnostic. With both coefficients held and valid, the domain
is a singleton and the likelihood is evaluated at that pair.

Uniform uses the strict likelihood comparison induced by width. Pareto
and power use the same strict endpoint comparison with opposite signs on
the log coordinate. At the best endpoint, the inequality
`log t < t - 1` for positive `t ≠ 1` gives the strict shape comparison
at the explicit maximizer. The proofs therefore use direct inequalities
for the two comparisons. Unboundedness is witnessed by constructing a
feasible parameter pair above any proposed likelihood bound.

## Verification

The standalone workstation check `lake env lean UnivariateML/Family/Endpoint.lean`
passed without diagnostics. The final workstation command
`lake build UnivariateML +UnivariateML.Family.Endpoint` reported
`Build completed successfully (6692 jobs).` The endpoint module was built
without warnings. Lake replayed existing style warnings from Weibull and GPD.
The explicit endpoint target compiles the new module while the library root
import remains with the coordinator. Both checks used one workstation slot
through `/tmp/umvl-cpu/run`, with `nice -n 10` and one Lean thread.
Mathlib was reused from the existing cache. The endpoint file has no
`sorry`, `admit`, `axiom` or `native_decide` declarations or tactics.

## Effort and remaining work

| Target | Proof effort |
| --- | --- |
| Uniform | Moderate. Proved attainment of both sample extremes, strict width comparison, both one-endpoint holds, the collapse construction and the open-interval comparison. |
| Pareto | Largest share. Built the shared endpoint comparison, shape inequality, mask proof, gap criterion and unboundedness construction. |
| Power | Smaller incremental share. Reused the Pareto proof with reversed endpoint order, verified the power log-ratio formula, and constructed a feasible point beyond the sample maximum for the open-support case. |

No mathematical proof obligations from TASK-309 remain open.
Floating-point evaluation and Pareto's right-censored extension are outside
these statements.

## Lean statements

### Uniform

Namespace `UnivariateML.Endpoint.Uniform`.

**`mle`**. A compatible positive-width candidate is the unique fit for any mask. Its free endpoints are the corresponding sample extremes.

```lean
theorem mle (hn : 0 < n) {x : Fin n → ℝ} {lower upper : Option ℝ}
    (hwidth : (fit hn x lower upper).1 < (fit hn x lower upper).2)
    (hs : fit hn x lower upper ∈ support x) :
    (fit hn x lower upper ∈ domain lower upper ∧
      IsMaxOn (likelihood x) (domain lower upper) (fit hn x lower upper)) ∧
      ∃! p, p ∈ domain lower upper ∧ IsMaxOn (likelihood x) (domain lower upper) p
```

**`jointMLE`**. A nonconstant sample gives a unique all-free fit.

```lean
theorem jointMLE (hn : 0 < n) {x : Fin n → ℝ} (hne : ∃ i j, x i ≠ x j) :
    ∃! p, p ∈ domain none none ∧ IsMaxOn (likelihood x) (domain none none) p
```

**`heldLower`**. A compatible held lower endpoint below the sample maximum gives a unique fit with upper endpoint equal to the sample maximum.

```lean
theorem heldLower (hn : 0 < n) {x : Fin n → ℝ} {a : ℝ}
    (hs : a ≤ sampleMin hn x) (hw : a < sampleMax hn x) :
    ∃! p, p ∈ domain (some a) none ∧
      IsMaxOn (likelihood x) (domain (some a) none) p
```

**`heldUpper`**. A compatible held upper endpoint above the sample minimum gives a unique fit with lower endpoint equal to the sample minimum.

```lean
theorem heldUpper (hn : 0 < n) {x : Fin n → ℝ} {b : ℝ}
    (hs : sampleMax hn x ≤ b) (hw : sampleMin hn x < b) :
    ∃! p, p ∈ domain none (some b) ∧
      IsMaxOn (likelihood x) (domain none (some b)) p
```

**`bothHeld`**. A valid fully held interval leaves exactly one pair to evaluate.

```lean
theorem bothHeld {a b : ℝ} (h : a < b) : domain (some a) (some b) = {(a, b)}
```

**`incompatible_zero`**. One observation excluded by a held endpoint makes the likelihood zero throughout the parameter domain.

```lean
theorem incompatible_zero {x : Fin n → ℝ} {lower upper : Option ℝ}
    (h : (∃ a i, lower = some a ∧ x i < a) ∨ (∃ b i, upper = some b ∧ b < x i))
    {p : ℝ × ℝ} (hp : p ∈ domain lower upper) : likelihood x p = 0
```

**`collapse_unbounded`**. A compatible zero-width candidate and at least one free endpoint make the likelihood unbounded.

```lean
theorem collapse_unbounded (hn : 0 < n) {x : Fin n → ℝ} {lower upper : Option ℝ}
    (hfree : lower = none ∨ upper = none)
    (heq : (fit hn x lower upper).1 = (fit hn x lower upper).2)
    (hs : fit hn x lower upper ∈ support x) :
    ¬ BddAbove (likelihood x '' domain lower upper)
```

**`constant_unbounded`**. Constant data make the all-free likelihood unbounded.

```lean
theorem constant_unbounded (hn : 0 < n) {x : Fin n → ℝ} {c : ℝ} (hx : ∀ i, x i = c) :
    ¬ BddAbove (likelihood x '' domain none none)
```

**`open_no_mle`**. An open interval with both endpoints free has no likelihood maximizer.

```lean
theorem open_no_mle (hn : 0 < n) (x : Fin n → ℝ) :
    ¬ ∃ p, p ∈ domain none none ∧ IsMaxOn (openLikelihood x) (domain none none) p
```

### Pareto and power

Namespace `UnivariateML.Endpoint.ParetoPower`.

**`pareto_gap`**. For Pareto the shared gap is the sum of the log observation-to-endpoint ratios.

```lean
theorem pareto_gap {x : Fin n → ℝ} {b : ℝ} (hx : ∀ i, 0 < x i) (hb : 0 < b) :
    gap .pareto x b = ∑ i, log (x i / b)
```

**`power_gap`**. For power the shared gap is the sum of the log endpoint-to-observation ratios.

```lean
theorem power_gap {x : Fin n → ℝ} {a : ℝ} (hx : ∀ i, 0 < x i) (ha : 0 < a) :
    gap .power x a = ∑ i, log (a / x i)
```

**`gap_pos_iff`**. At a compatible positive endpoint the gap is positive exactly when some observation differs from that endpoint.

```lean
theorem gap_pos_iff (family : Family) {x : Fin n → ℝ} {e a : ℝ} (he : 0 < e)
    (hs : (e, a) ∈ support family x) : 0 < gap family x e ↔ ∃ i, x i ≠ e
```

**`mle`**. The fitted pair is the unique compatible maximum under any admissible mask. A free endpoint is the sample minimum for Pareto or maximum for power. A free shape is the sample size divided by the positive gap.

```lean
theorem mle (family : Family) (hn : 0 < n) {x : Fin n → ℝ} {endpoint shape : Option ℝ}
    (he : 0 < (fit family hn x endpoint shape).1)
    (hs : fit family hn x endpoint shape ∈ support family x)
    (ha : match shape with
      | some a => 0 < a
      | none => 0 < gap family x (fit family hn x endpoint shape).1) :
    (fit family hn x endpoint shape ∈ domain endpoint shape ∧
      IsMaxOn (likelihood family x) (domain endpoint shape)
        (fit family hn x endpoint shape)) ∧
      ∃! p, p ∈ domain endpoint shape ∧
        IsMaxOn (likelihood family x) (domain endpoint shape) p
```

**`jointMLE`**. A positive nonconstant sample has a unique all-free maximum in each family.

```lean
theorem jointMLE (family : Family) (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) (hne : ∃ i j, x i ≠ x j) :
    ∃! p, p ∈ domain none none ∧ IsMaxOn (likelihood family x) (domain none none) p
```

**`heldShape`**. A positive held shape gives a unique endpoint fit, even on constant data.

```lean
theorem heldShape (family : Family) (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {a : ℝ} (ha : 0 < a) :
    ∃! p, p ∈ domain none (some a) ∧
      IsMaxOn (likelihood family x) (domain none (some a)) p
```

**`heldEndpoint`**. A compatible positive held endpoint gives a unique shape fit if some observation differs from it. The second coordinate in the support hypothesis is a placeholder because sample support depends only on the endpoint.

```lean
theorem heldEndpoint (family : Family) (hn : 0 < n) {x : Fin n → ℝ} {e : ℝ}
    (he : 0 < e) (hs : (e, 0) ∈ support family x) (hne : ∃ i, x i ≠ e) :
    ∃! p, p ∈ domain (some e) none ∧
      IsMaxOn (likelihood family x) (domain (some e) none) p
```

**`bothHeld`**. A valid fully held pair leaves exactly one parameter pair to evaluate.

```lean
theorem bothHeld {e a : ℝ} (he : 0 < e) (ha : 0 < a) :
    domain (some e) (some a) = {(e, a)}
```

**`incompatible_zero`**. An observation below a held Pareto endpoint or above a held power endpoint forces zero likelihood throughout the domain.

```lean
theorem incompatible_zero (family : Family) {x : Fin n → ℝ} {e : ℝ} {shape : Option ℝ}
    (h : ∃ i, family.orient (x i) < family.orient e) {p : ℝ × ℝ}
    (hp : p ∈ domain (some e) shape) : likelihood family x p = 0
```

**`constant_unbounded`**. With free shape and a compatible endpoint mask that permits the common observation as endpoint, constant data yield unbounded likelihood.

```lean
theorem constant_unbounded (family : Family) (hn : 0 < n) {x : Fin n → ℝ}
    {endpoint : Option ℝ} {c : ℝ} (hc : 0 < c) (he : endpoint.getD c = c)
    (hx : ∀ i, x i = c) :
    ¬ BddAbove (likelihood family x '' domain endpoint none)
```

**`open_no_mle`**. With the moving endpoint excluded and free, neither family attains a maximum, whether shape is free or held.

```lean
theorem open_no_mle (family : Family) (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {shape : Option ℝ} :
    ¬ ∃ p, p ∈ domain none shape ∧ IsMaxOn (openLikelihood family x) (domain none shape) p
```
