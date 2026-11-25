# TASK-338 · Paralogistic held-coefficient results

2026-11-25

For both distributions, holding shape gives a unique positive scale, including for constant data. Holding scale gives an attained shape maximum on every nonempty positive sample. The report’s full negative-log-ratio condition gives uniqueness. The two bands in the held-scale tie construction cross at a unique sample parameter. Identifying that crossing as an exact two-point global maximizing set requires the compact exclusion hypothesis stated below. [Lean module](../../lean/UnivariateML/Family/Paralogistic.lean) · [Source mathematics](2026-11-25-paralogis-math-report.md)

## Dossier discrepancies

- The dossiers describe general uniqueness as unknown or unestablished. Read as a statement about universal uniqueness on the joint and held-scale routes, this is out of date. The source report supplies exact counterexamples to universal uniqueness on both routes. Its sufficient conditions address which particular samples have a singleton maximum. [Paralogistic dossier](../../../univariateML-cpp/include/cppunivariate/distributions/paralogis/README.md) · [Inverse dossier](../../../univariateML-cpp/include/cppunivariate/distributions/invparalogis/README.md) · [Source mathematics](2026-11-25-paralogis-math-report.md)
- The inverse dossier’s Details and Global certificate describe certificate closure as mandatory and say that exhaustion stops with `CertificateBudget`. Its own front matter and current implementation preserve successful uncertified fits. The joint and held-scale routes check the accuracy of such results on the original inverse sample, and the joint near-constant offset route can return an uncertified fit directly. The Lean theorems concern exact real likelihoods. [Inverse dossier](../../../univariateML-cpp/include/cppunivariate/distributions/invparalogis/README.md) · [Inverse fitter](../../../univariateML-cpp/include/cppunivariate/distributions/invparalogis/mle.hpp)

## `UnivariateML.Paralogistic`

The objective is the mean log likelihood, defined by setting the two Burr shapes equal. All likelihood comparisons agree with those for the total log likelihood when the sample is nonempty.

### Held shape

- `interceptScore_strictAnti` proves strict decrease in the intercept $b=-a\log s$ at each positive shape.
- `heldShape` gives exactly one maximizing positive scale. The intercept score has limits $1$ and $-a$, so the generic score theorem supplies a unique maximizing root.
- `heldShape_iff_score_zero` characterizes the physical-scale maximum by the intercept score equation.
- `heldShape_constant` gives the maximizing scale $c\exp(\log(a)/a)$ when every observation is $c>0$.

```lean
theorem interceptScore_strictAnti (hn : 0 < n) (y : Fin n → ℝ) {a : ℝ} (ha : 0 < a) :
    StrictAnti (interceptScore y a)

theorem heldShape (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {a : ℝ} (ha : 0 < a) :
    ∃! s, s ∈ Ioi 0 ∧ IsMaxOn (logLik x a) (Ioi 0) s

theorem heldShape_iff_score_zero (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {a s : ℝ} (ha : 0 < a) (hs : 0 < s) :
    IsMaxOn (logLik x a) (Ioi 0) s ↔
      interceptScore (fun i ↦ log (x i)) a (-a * log s) = 0

theorem heldShape_constant (hn : 0 < n) {c a : ℝ} (hc : 0 < c) (ha : 0 < a) :
    IsMaxOn (logLik (fun _ : Fin n ↦ c) a) (Ioi 0) (c * exp (log a / a))
```

### Held scale

- `shapeLik_strictConcave` verifies the source’s strict curvature bound. For log ratios $y_i$, `negativeSpread y` averages $(-y_i)^{-1}$ over the negative entries with zeros for all other entries and denominator $n$.
- `heldScale_exists` gives a positive maximizing shape for every nonempty positive sample, including a constant sample at any held scale. For shapes at least one, the penalty from a single observation grows at least linearly. A zero log ratio supplies the positive coefficient `softplus 0`. The small-shape tail is bounded by $2\log a$.
- `heldScale` gives uniqueness under the full strict spread inequality.
- `heldScale_above` specializes uniqueness to observations all at or above the held scale.

```lean
theorem shapeLik_strictConcave (y : Fin n → ℝ) (h : negativeSpread y < exp 2 / 4) :
    StrictConcaveOn ℝ (Ioi 0) (shapeLik y)

theorem heldScale_exists (hn : 0 < n) {x : Fin n → ℝ} (_hx : ∀ i, 0 < x i)
    {s : ℝ} (_hs : 0 < s) :
    ∃ a ∈ Ioi 0, IsMaxOn (fun a ↦ logLik x a s) (Ioi 0) a

theorem heldScale (hn : 0 < n) {x : Fin n → ℝ} (_hx : ∀ i, 0 < x i)
    {s : ℝ} (_hs : 0 < s) (h : negativeSpread (logRatios x s) < exp 2 / 4) :
    ∃! a, a ∈ Ioi 0 ∧ IsMaxOn (fun a ↦ logLik x a s) (Ioi 0) a

theorem heldScale_above (hn : 0 < n) {x : Fin n → ℝ} {s : ℝ} (hs : 0 < s)
    (h : ∀ i, s ≤ x i) :
    ∃! a, a ∈ Ioi 0 ∧ IsMaxOn (fun a ↦ logLik x a s) (Ioi 0) a
```

### Held-scale tie

- `heldScale_band_crossing` proves that the maxima on the two report bands have equal heights at exactly one $r\in(3/1000,5/1000)$. The proof uses analytically bounded endpoint witnesses, continuity of the compact-band maxima, and opposite strict monotonicity of the two heights. It uses the same sample family and bands as the source.
- `heldScale_tie_of_band_bounds` proves that this two-point sample has exactly two global maximizers, one in each disjoint band, under the stated compact-cover hypothesis. Lean proves strict concavity within both bands and excludes both shape tails by comparison to the feasible shape $3$. The source excludes those tails via score signs.

```lean
theorem heldScale_band_crossing :
    ∃! r, r ∈ Ioo (3 / 1000 : ℝ) (5 / 1000) ∧
      bandHeight (Icc 2 4) r = bandHeight (Icc 6500 10000) r

theorem heldScale_tie_of_band_bounds
    (hcover : ∀ r ∈ Icc (3 / 1000 : ℝ) (5 / 1000), ∀ a ∈ Icc (1 : ℝ) 20000,
      a ∉ Icc (2 : ℝ) 4 → a ∉ Icc (6500 : ℝ) 10000 →
      shapeScore (tieLogs r) a = 0 → shapeLik (tieLogs r) a < shapeLik (tieLogs r) 3) :
    ∃ r ∈ Ioo (3 / 1000 : ℝ) (5 / 1000),
      ∃ a ∈ Icc (2 : ℝ) 4, ∃ b ∈ Icc (6500 : ℝ) 10000,
        {k ∈ Ioi 0 | IsMaxOn (fun t ↦ logLik (fun i ↦ exp (tieLogs r i)) t 1) (Ioi 0) k} =
          {a, b}
```

**Open.** Discharge `hcover` for the source’s exact example. For every $r\in[3/1000,5/1000]$, every stationary shape in $[1,20000]$ outside the two bands must lose strictly to shape $3$. The source checker divides the sample-parameter interval into 40 rational slabs and uses 838 cell evaluations, including subdivisions. Each accepted cell excludes a score zero or bounds its likelihood strictly below the shape-3 witness. Closing the theorem requires a Lean-verified cover and certified real exponential and logarithm bounds for the accepted cells, or an analytic replacement. Joint existence, joint small-spread uniqueness, and the joint tie were outside this held-coefficient job. [Source interval checker](2026-11-25-paralogis-math-data/certify.py)

## `UnivariateML.InvParalogistic`

- `logLik_reciprocal` gives the inverse likelihood at the same shape as the parent likelihood on reciprocated observations and scale, plus the mean log Jacobian. That term is independent of both coefficients.
- `heldShape` transfers the unique positive scale. `heldShape_constant` gives the scale $c\exp(-\log(a)/a)$ for constant inverse data.
- `heldScale_exists` transfers unconditional attainment. `heldScale` transfers uniqueness with inverse log ratios $\log(s/x_i)$. `heldScale_below` gives the corresponding one-sided condition for observations all at or below the held scale.

```lean
theorem logLik_reciprocal {x : Fin n → ℝ} (_hx : ∀ i, 0 < x i)
    (a s : ℝ) :
    logLik x a s = Paralogistic.logLik (reciprocal x) a s⁻¹ +
      avg (fun i ↦ -2 * log (x i))

theorem heldShape (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {a : ℝ} (ha : 0 < a) :
    ∃! s, s ∈ Ioi 0 ∧ IsMaxOn (logLik x a) (Ioi 0) s

theorem heldShape_constant (hn : 0 < n) {c a : ℝ} (hc : 0 < c) (ha : 0 < a) :
    IsMaxOn (logLik (fun _ : Fin n ↦ c) a) (Ioi 0) (c * exp (-(log a / a)))

theorem heldScale_exists (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {s : ℝ} (hs : 0 < s) :
    ∃ a ∈ Ioi 0, IsMaxOn (fun a ↦ logLik x a s) (Ioi 0) a

theorem heldScale (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {s : ℝ} (hs : 0 < s)
    (h : Paralogistic.negativeSpread (fun i ↦ log (s / x i)) < exp 2 / 4) :
    ∃! a, a ∈ Ioi 0 ∧ IsMaxOn (fun a ↦ logLik x a s) (Ioi 0) a

theorem heldScale_below (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {s : ℝ} (hs : 0 < s) (h : ∀ i, x i ≤ s) :
    ∃! a, a ∈ Ioi 0 ∧ IsMaxOn (fun a ↦ logLik x a s) (Ioi 0) a
```

## Effort and verification

| Target | Work |
|---|---|
| Held shape | Differentiated the mean criterion, proved the two score limits, reused the generic real score theorem, mapped the root back to positive scale, and evaluated it on constant data. |
| Held scale | Differentiated twice, proved the sharp auxiliary bound $u^2 e^{-u}\le 4/e^2$, summed the per-observation curvature bounds, and proved coercivity independently of the uniqueness predicate. |
| Reciprocal transfer | Proved the likelihood identity and used the existing parameter-bijection lemmas for attainment, the unique optimizer, and the constant-data witness. |
| Held-scale tie | Proved both endpoint comparisons, band-height continuity and strict monotonicity, strict concavity in both bands, and exclusion of the two tails. Reduced the unconditional global tie to the compact cover above. |

The fixed-kernel location-scale lemma does not apply to the held-scale shape problem because the shape changes the kernel. The proof instead reuses the generic coercivity result and the Burr likelihood identity. [Generic location-scale lemmas](../../lean/UnivariateML/Generic/LocationScale.lean) · [Generic coercivity lemmas](../../lean/UnivariateML/Generic/Escape.lean) · [Burr held-coefficient module](../../lean/UnivariateML/Family/BurrHeld.lean)

All final checks ran on the workstation through `/tmp/umvl-cpu/run`.

| Check | Result |
|---|---|
| `lake env lean UnivariateML/Family/Paralogistic.lean` | Exit 0 with no diagnostics on the final file. |
| `lake build +UnivariateML.Family.Paralogistic` | Passed, 7,269 jobs, with no module warnings on the final file. |
| Default `lake build` | Passed, 7,402 jobs, with warnings in existing modules. |

The default build uses the existing root imports. The explicit module target checks the new file pending the coordinator’s import. The direct check and explicit build were rerun after wrapping six proof lines flagged by Lake’s line-length linter.
