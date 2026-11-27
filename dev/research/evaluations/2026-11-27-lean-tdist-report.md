# Student t held-shape scale and collapse in Lean

TASK-378 · 2026-11-27 · `codex/lean-tdist` · base `0bfa829c`

## Dossier findings

**The `lst` limits entry overstates the numerical stop at joint equality below shape one.**
The fixed-shape fitter computes the exponent at the largest atom and checks
for a negative exponent first. When that exponent is zero and `df < 1`,
it still searches. It can return a successful result marked uncertified or a
proved symmetry outcome. The equality-specific numerical-failure branch
runs when the search has no successful candidate. The dossier should also
make clear that joint equality refers to the largest atom. In the sample
`[0, 1, 1]` at shape `1/2`, the atom at zero has exponent zero while the
atom at one has exponent `−3/2` and makes the joint likelihood unbounded.
This example follows from the proved negative-exponent theorem and direct
substitution. The fitter behavior was checked by reading the source.
[lst dossier](../../../univariateML-cpp/include/cppunivariate/distributions/lst/README.md) ·
[lst fitter](../../../univariateML-cpp/include/cppunivariate/distributions/lst/mle.hpp)

**The shape-domain qualifiers matter for both collapse infima.** The `std`
claim that the infimum `2n − 3k` is approached only as shape decreases to
two requires `k < n`. When `k = n`, the exponent is `−n` at every shape
and the infimum is attained throughout the domain. The strict-layer schema
restricts the associated outcome check to nonconstant data. The `lst` infimum
`n − 2k` applies to free shape with free location, where shape is at least
one. When location is held and estimated shape ranges over all positive
values, the infimum at that location is `−k` for `k < n`, approached as
shape decreases to zero. The `lst` Details already describe that exemption.
[std dossier](../../../univariateML-cpp/include/cppunivariate/distributions/std/README.md) ·
[lst dossier](../../../univariateML-cpp/include/cppunivariate/distributions/lst/README.md) ·
[dossier schema](../../../univariateML-cpp/include/cppunivariate/distributions/README_SCHEMA.md#strict-layer-keys-under-existence)

## Scope and proof

The sample is `Fin n → ℝ`. The definitions use the normalized ordinary
Student t density, including its gamma factors. The held-location results
use the dossier's nonzero-residual hypothesis for strict decrease. For a
nonempty all-tied sample at the held location, the score is constant at
`−n` and the likelihood diverges at zero scale. At fixed shape, the
standard-deviation parameterization is a positive linear change of scale.

The proofs use exact real arithmetic. The joint collapse theorem takes a
constant-location path through an atom. Its conclusion needs only one
negative exponent. The optional normal-face proof is pointwise and permits
varying ordinary scale with a positive limit. Total-variation convergence,
the separate two-degree-of-freedom face, optimized shape comparisons and
the fitters' floating-point certificates are outside the assigned claims.
[Lean module](../../lean/UnivariateML/Family/TDist.lean)

## Verification and effort

The workstation direct check and the final Lake build both exited zero.
Lake reported `Build completed successfully (7464 jobs).` The final build
had no diagnostics from `TDist.lean` and replayed warnings from existing
modules. The explicit module target checks the new file before the
coordinator adds its import. The warm rerun followed three line wraps
flagged by the Lake style linter. The worktree used the existing Mathlib cache.

```sh
cd dev/lean
/tmp/umvl-cpu/run env LEAN_NUM_THREADS=1 nice -n 10 bash -c '
  /home/jonas/.elan/bin/lake env lean UnivariateML/Family/TDist.lean &&
  /home/jonas/.elan/bin/lake build UnivariateML +UnivariateML.Family.TDist
'
```

| Target | Proof effort |
| --- | --- |
| 1. Held location and shape | Differentiated in log scale, split off the zero residuals for the lower limit, and used the finite-sum limit at infinity. Reused `Generic.existsUnique_log_score_zero` for the positive case and `Generic.limit_le_sSup` for the critical case. Strict decrease bounds every path approaching the critical supremum. Substituted the dossier's standard-deviation formula directly and transported the results through the positive factor. |
| 2. Collapse and free location | Isolated the coefficient of log scale and proved continuity of the remaining finite sum at zero. Exponentiation gives the exact positive leading constant. Used a single atom path to refute an arbitrary joint upper bound in each parameterization. |
| 3. Normal face | Derived log-gamma ratio bounds from two midpoint inequalities and the gamma recurrence. Squeezed the normalizer and combined it with Mathlib's log-kernel limit and continuity of the scale map. |
| 4. Source comparison | Compared the statements, limit metadata, and relevant fitter branches. Separated the exact held-location result from the joint numerical fallback and checked the shape-domain qualifiers for the infima. |

The assigned mathematical targets are settled. The coordinator's root
import remains. **Open in this formalization:** joint critical equality
below shape one. The fixed-location expansion supplies a lower bound on
the joint supremum there. Deciding joint attainment requires a uniform
bound over moving locations and scales or a proof that an interior
maximizer beats every feasible collapse value.

## Public theorem statements

The statements below are copied from `UnivariateML.TDist`. Here
`exponent x μ ν` is `ν * ((n : ℝ) - multiplicity x μ) - multiplicity x μ`.

### `hasDerivAt_logLik`

The derivative with respect to log ordinary scale is the specified log-scale score.

```lean
theorem hasDerivAt_logLik (x : Fin n → ℝ) (μ : ℝ) {ν : ℝ} (hν : 0 < ν) (t : ℝ) :
    HasDerivAt (fun s ↦ logLik x μ ν (exp s)) (scaleScore x μ ν (exp t)) t
```

### `scaleScore_strictAntiOn`

The score decreases strictly on positive scales as soon as one residual is nonzero.

```lean
theorem scaleScore_strictAntiOn (x : Fin n → ℝ) (μ : ℝ) {ν : ℝ} (hν : 0 < ν)
    (hx : ∃ i, x i ≠ μ) : StrictAntiOn (scaleScore x μ ν) (Ioi 0)
```

### `scaleScore_limits`

The score limits are the collapse exponent at zero and minus the sample size at infinity. The limits also hold when every residual is zero.

```lean
theorem scaleScore_limits (x : Fin n → ℝ) (μ : ℝ) {ν : ℝ} (hν : 0 < ν) :
    Tendsto (scaleScore x μ ν) (𝓝[>] 0) (𝓝 (exponent x μ ν)) ∧
      Tendsto (scaleScore x μ ν) atTop (𝓝 (-(n : ℝ)))
```

### `heldLocation_maximum`

A positive maximizing scale exists exactly when the exponent is positive. In that case the maximizer is the unique positive score zero.

```lean
theorem heldLocation_maximum (x : Fin n → ℝ) (μ : ℝ) {ν : ℝ} (hν : 0 < ν)
    (hx : ∃ i, x i ≠ μ) :
    ((∃ a ∈ Ioi 0, IsMaxOn (logLik x μ ν) (Ioi 0) a) ↔ 0 < exponent x μ ν) ∧
      (0 < exponent x μ ν →
        (∃! a, 0 < a ∧ scaleScore x μ ν a = 0) ∧
        ∀ a > 0, IsMaxOn (logLik x μ ν) (Ioi 0) a ↔ scaleScore x μ ν a = 0)
```

### `collapse_power`

Dividing the likelihood by the stated power of scale yields a finite, strictly positive limit at zero. This establishes the leading power, including for all-tied data.

```lean
theorem collapse_power (x : Fin n → ℝ) (μ : ℝ) {ν : ℝ} (hν : 0 < ν) :
    Tendsto (fun a ↦ likelihood x μ ν a / a ^ exponent x μ ν) (𝓝[>] 0)
      (𝓝 (exp (collapseLogValue x μ ν))) ∧ 0 < exp (collapseLogValue x μ ν)
```

### `heldLocation_equality`

At zero exponent the log likelihood approaches the finite supremum at zero scale, stays strictly below it at every positive scale, and can approach it only along paths tending to zero scale.

```lean
theorem heldLocation_equality (x : Fin n → ℝ) (μ : ℝ) {ν : ℝ} (hν : 0 < ν)
    (hx : ∃ i, x i ≠ μ) (he : exponent x μ ν = 0) :
    Tendsto (logLik x μ ν) (𝓝[>] 0) (𝓝 (collapseLogValue x μ ν)) ∧
      sSup (logLik x μ ν '' Ioi 0) = collapseLogValue x μ ν ∧
      (∀ a > 0, logLik x μ ν a < collapseLogValue x μ ν) ∧
      ∀ {ι : Type*} {l : Filter ι} {a : ι → ℝ},
        (∀ᶠ i in l, 0 < a i) →
        Tendsto (fun i ↦ logLik x μ ν (a i)) l (𝓝 (collapseLogValue x μ ν)) →
        Tendsto a l (𝓝[>] 0)
```

### `heldLocation_likelihood_equality`

The likelihood itself has the corresponding finite unattained supremum obtained by exponentiating the limiting log likelihood.

```lean
theorem heldLocation_likelihood_equality (x : Fin n → ℝ) (μ : ℝ) {ν : ℝ}
    (hν : 0 < ν) (hx : ∃ i, x i ≠ μ) (he : exponent x μ ν = 0) :
    Tendsto (likelihood x μ ν) (𝓝[>] 0) (𝓝 (exp (collapseLogValue x μ ν))) ∧
      sSup (likelihood x μ ν '' Ioi 0) = exp (collapseLogValue x μ ν) ∧
      ∀ a > 0, likelihood x μ ν a < exp (collapseLogValue x μ ν)
```

### `exponent_all_tied`

If every observation is at the held location, the exponent is minus the sample size at each shape.

```lean
theorem exponent_all_tied (x : Fin n → ℝ) (μ ν : ℝ) (hx : ∀ i, x i = μ) :
    exponent x μ ν = -(n : ℝ)
```

### `collapse_unbounded`

At a negative exponent the likelihood diverges as the scale tends to zero through positive values.

```lean
theorem collapse_unbounded (x : Fin n → ℝ) (μ : ℝ) {ν : ℝ} (hν : 0 < ν)
    (he : exponent x μ ν < 0) : Tendsto (likelihood x μ ν) (𝓝[>] 0) atTop
```

### `joint_unbounded`

One sample atom with a negative exponent suffices for unbounded likelihood over free location and positive scale.

```lean
theorem joint_unbounded (x : Fin n → ℝ) {ν : ℝ} (hν : 0 < ν)
    (h : ∃ i, exponent x (x i) ν < 0) :
    ¬ BddAbove ((fun p : ℝ × ℝ ↦ likelihood x p.1 ν p.2) '' (univ ×ˢ Ioi 0))
```

### `stdLogLik_eq`

The formula using standard deviation equals the ordinary-scale formula at the rescaled value.

```lean
theorem stdLogLik_eq (x : Fin n → ℝ) (μ : ℝ) {ν σ : ℝ} (hν : 2 < ν) (hσ : 0 < σ) :
    stdLogLik x μ ν σ = logLik x μ ν (ordinaryScale ν σ)
```

### `std_reparametrization`

At held shape above two, the coordinate change preserves attainable log-likelihood values and carries the maximizing scales in either direction.

```lean
theorem std_reparametrization (x : Fin n → ℝ) (μ : ℝ) {ν : ℝ} (hν : 2 < ν) :
    stdLogLik x μ ν '' Ioi 0 = logLik x μ ν '' Ioi 0 ∧
      ∀ σ > 0, IsMaxOn (stdLogLik x μ ν) (Ioi 0) σ ↔
        IsMaxOn (logLik x μ ν) (Ioi 0) (ordinaryScale ν σ)
```

### `std_scaleScore`

The log-standard-deviation derivative is the ordinary score evaluated at the associated scale. The score is strictly decreasing and has the same endpoint limits.

```lean
theorem std_scaleScore (x : Fin n → ℝ) (μ : ℝ) {ν : ℝ} (hν : 2 < ν)
    (hx : ∃ i, x i ≠ μ) :
    let S := scaleScore x μ ν ∘ ordinaryScale ν
    (∀ t, HasDerivAt (fun u ↦ stdLogLik x μ ν (exp u)) (S (exp t)) t) ∧
      StrictAntiOn S (Ioi 0) ∧
      Tendsto S (𝓝[>] 0) (𝓝 (exponent x μ ν)) ∧
      Tendsto S atTop (𝓝 (-(n : ℝ)))
```

### `std_heldLocation`

The positive, zero and negative exponent conclusions carry over to standard deviation. The positive case has a unique maximizing standard deviation.

```lean
theorem std_heldLocation (x : Fin n → ℝ) (μ : ℝ) {ν : ℝ} (hν : 2 < ν)
    (hx : ∃ i, x i ≠ μ) :
    ((∃! σ, σ ∈ Ioi 0 ∧ IsMaxOn (stdLogLik x μ ν) (Ioi 0) σ) ↔ 0 < exponent x μ ν) ∧
      (exponent x μ ν = 0 →
        Tendsto (stdLogLik x μ ν) (𝓝[>] 0) (𝓝 (collapseLogValue x μ ν)) ∧
        sSup (stdLogLik x μ ν '' Ioi 0) = collapseLogValue x μ ν ∧
        ∀ σ > 0, stdLogLik x μ ν σ < collapseLogValue x μ ν) ∧
      (exponent x μ ν < 0 →
        Tendsto (fun σ ↦ exp (stdLogLik x μ ν σ)) (𝓝[>] 0) atTop)
```

### `std_equality_path`

At zero exponent every eventually positive standard-deviation path that approaches the log-likelihood supremum converges to zero.

```lean
theorem std_equality_path {ι : Type*} {l : Filter ι} {σ : ι → ℝ}
    (x : Fin n → ℝ) (μ : ℝ) {ν : ℝ} (hν : 2 < ν) (hx : ∃ i, x i ≠ μ)
    (he : exponent x μ ν = 0) (hσ : ∀ᶠ i in l, 0 < σ i)
    (hlim : Tendsto (fun i ↦ stdLogLik x μ ν (σ i)) l (𝓝 (collapseLogValue x μ ν))) :
    Tendsto σ l (𝓝[>] 0)
```

### `std_joint_unbounded`

The negative-exponent atom also makes the joint likelihood unbounded over free mean and standard deviation at held shape above two.

```lean
theorem std_joint_unbounded (x : Fin n → ℝ) {ν : ℝ} (hν : 2 < ν)
    (h : ∃ i, exponent x (x i) ν < 0) :
    ¬ BddAbove ((fun p : ℝ × ℝ ↦ exp (stdLogLik x p.1 ν p.2)) '' (univ ×ˢ Ioi 0))
```

### `density_tendsto_normal`

At fixed location and observation, the ordinary Student t density converges to the normal density as shape tends to infinity whenever the ordinary scale tends to a positive value. A constant ordinary scale is a special case.

```lean
theorem density_tendsto_normal (μ y : ℝ) {a : ℝ → ℝ} {s : ℝ}
    (hs : 0 < s) (ha : Tendsto a atTop (𝓝 s)) :
    Tendsto (fun ν ↦ density μ ν (a ν) y) atTop (𝓝 (normalDensity μ s y))
```

### `std_density_tendsto_normal`

At fixed standard deviation and mean, the associated ordinary Student scale tends to that standard deviation, giving the stated pointwise normal limit.

```lean
theorem std_density_tendsto_normal (μ y : ℝ) {σ : ℝ} (hσ : 0 < σ) :
    Tendsto (fun ν ↦ density μ ν (ordinaryScale ν σ) y) atTop (𝓝 (normalDensity μ σ y))
```
