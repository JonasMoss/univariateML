# TASK-422 Lean proofs for Student likelihoods

Base `61bfcadf`. The worktree adds 59 public theorems in eight modules and 78 rows to the [atlas fragment](../existence-atlas/lean-map.d/lean-student.csv). Each row gives the theorem's scope. The coverage column is empty for independent review.

The proofs resolve the held-df `std` existence and uniqueness questions, the fixed-shape and fixed-scale location questions for both Student parameterizations, the fixed-mean and fixed-sd `std` criterion, the held-location `lst` criterion, the named skew-t likelihood paths, the stated held-endpoint and held-scale half-t comparisons, and the joint slash attainment and half-tie comparison. The tables record the remaining proof obligations across the 81 main questions, 35 limit-path keys and 18 face-comparison keys.

## Verification

Direct file checks used Lean 4.36.0 on the workstation through `/tmp/umvl-cpu/run nice -n 10 /home/jonas/.elan/bin/lake env lean <file>` from `dev/lean`. The final workstation `lake build` with temporary imports of all eight modules completed successfully with **7,994 jobs**. Lake emitted advisory style diagnostics, including whitespace warnings in the new modules. The build exited zero. The temporary imports were removed and `git diff --exit-code -- dev/lean/UnivariateML.lean` exited zero. A source scan found no `sorry`, `admit`, `axiom` or `native_decide` in the new modules.

## Scope and source notes

- The unrestricted positive-df skew-t divergence theorem and the proposed finite global counterexamples concern different df domains. The reflected counterexamples in the source use the fitted restriction `ν ≥ 1`. The source correction withdraws the two-scale uniqueness examples for `alpha+xi` and `alpha+nu+xi` because their objective had an extra factor of scale. The subsequent prose still describes those examples. The new scale criteria use the corrected objective. [Skew-t correction and scope](2026-11-27-st-atlas-math-report.md)
- The fixed-df midpoint argument uses a strict term for `ν > 1`. At `ν = 1`, the existing Cauchy theorem supplies uniqueness with strict half masses and the exact balanced two-point arc. At subunit df, the positive-exponent attainment proof still applies. The critical branch needs the full face comparison. [Fixed-shape derivation](2026-11-07-std-held-data/derivation.md#joint-critical-atom-at-held-shape-at-least-one) · [Cauchy theorems](../../lean/UnivariateML/Family/Cauchy.lean)
- The skew-t CDF is defined as the normalized integral of the Student kernel. The normalization cancels the positive density constant and permits direct proofs of symmetry, strict monotonicity, and the two tail limits. The new path results concern joint likelihoods. The shaped normal path uses dominated convergence, and the two infinite-slant paths include observations exactly at the fixed location. Moving contacts remain a separate global-envelope problem. [StudentSkew.lean](../../lean/UnivariateML/Family/StudentSkew.lean)
- The `lst` held-location theorem expresses the conditional RMS Gaussian height as the half-normal product divided by `2^n`. Derivatives in the scalar comparisons are taken on the likelihood scale. Positivity makes their interior stationary conditions equivalent to log-likelihood stationarity.
- The slash proof uses the vertical trace as an attained boundary limit, bounds all moving paths by that trace, and separates equality at a finite point from strict loss everywhere. The paired-block sufficient test already has a Lean proof using the corrected block bound. [Slash correction](2026-11-25-slash2-math-report.md#correction-to-the-paired-collapse-proof) · [Paired collapse theorem](../../lean/UnivariateML/Family/SkewLSSlash.lean#L719)

## Main questions and proof effort

A cell marked **Open** gives the remaining proof step for that question. An em dash marks a question outside this job's main-question list.

### Standard Student

| Held | Criterion | Failure | Uniqueness |
| --- | --- | --- | --- |
| all free | **Open.** The supercritical atom witness is checked. The strict and critical branches need a compact joint profile with the optimized normal and Student-two or collapse heights. | **Open.** The positive-scale Student-two path and supercritical divergence are checked. Exhaustion needs joint shape, location and scale limits, including the zero-scale normal corner and shape approach to a critical exponent from below. | **Open.** The nuisance fit is unique at each eligible fixed shape. The exact joint real crossing still needs the simultaneous profile bounds on the two shape intervals, both endpoint comparisons and exclusion of intermediate and tail winners. |
| mean | **Open.** Fixed-location scale profiling is available. The remaining step is the moving-shape limit of the fitted scale at critical df two, followed by the exact comparison with the same-mean RMS normal. | **Open.** The held-mean supercritical counterpath is checked. The finite lower-face selection and joint corner bounds as scale and shape vary remain. | **Open.** The joint crossing still requires the certified shape and interpolation bounds with the location held at zero. |
| mean+sd | Checked by the scalar compact interval comparison and the derivative at an interior maximum. Equality with a face is included. | Checked by exact decomposition into the df-two exponent, a remainder with a finite limit, and the matched normal limit at infinity. The argument includes all tie counts. | **Open.** The exact scalar real crossing requires the two interval maxima to exchange order and the separator and both tails to lose uniformly in the sample interpolation. |
| nu | Checked by a uniform small-scale bound over a positive separation of distinct atoms, a compact rectangle, and exclusion at nonpositive exponent. Testing all atoms is equivalent to testing the largest. | Checked by the fixed-shape remote-end estimates, local zero-scale envelope, and strict critical joint bound. The existing negative-exponent vertical path covers divergence. | Checked by the strict hyperbolic midpoint improvement, excluding the all-tied exception through divergence, then changing to the positive sd coordinate. |
| nu+sd | Checked by continuity and coercivity of the location log likelihood. | Checked by the two zero limits at infinite location. | Checked by the exact quartic minimization for `(-2, 2)`, giving both maximizers and their full set. |
| sd | **Open.** The held-mean collapse calculation is available. Allowing the mean to move needs uniform df-two control through the atom layer and the optimized normal comparison. | **Open.** The lower limit with location along the atom is checked. The moving-location df-two collar bound and uniform normal-end exclusion remain. | **Open.** The `(-100, 100)` proof needs a compact upper envelope above the normal and lower faces, plus the exact witness comparison that excludes every centered value. |

### Ordinary location–scale Student

| Held | Criterion | Failure | Uniqueness |
| --- | --- | --- | --- |
| all free | **Open.** Fixed-df strict-mass fits and the df-one Cauchy classification are checked. The variable-df joint compactification and actual optimized normal and critical face comparison remain. | — | **Open.** The geodesic and Cauchy results supply the centered nuisance reduction for the exact crossing sample. Lean still needs the profile comparison and tail certificates along the full sample interpolation. |
| df | **Open at critical subunit df.** Positive and negative exponents and the critical branches above one or at one are settled. At `0 < df < 1` and zero largest-atom exponent, the remaining step is a compact comparison of the actual stationary candidates with the largest critical value. | Checked by the uniform remote-end estimates and the local small-scale exponent bounds. At equality the trace is a boundary value even if a finite subunit point can exceed it. | **Open for the exact subunit counterexample.** The balanced Cauchy arc and uniqueness under strict mass at df at least one are checked. For `(-100, -99, 0, 99, 100)` at df one half, the centered scale root must still be bounded between 1979 and 1981 in squared kernel scale, then compared by the rational fourth-power witness. |
| df+sigma | Checked by the coercive location log likelihood for every positive held df and scale. | Checked by the two zero limits at infinite location. | Checked by exact quartic minimization for `(-5, 5)` at df one and scale three, yielding the full two-point argmax. |
| mu | Checked by the unique conditional scale, a uniform zero-df bound with no zero residuals, profiled normal convergence and the weak face inequality. A zero residual gives a divergent path. | **Open for full joint escape classification.** The tie divergence witness, uniform tie-free zero-df bound and profile normal end are checked or reused. Joint zero-scale and zero-df rates, approach to a critical shape from below, and the all-zero residual normal corner still need a single exhaustion theorem. | **Open.** The exact crossing must rule out both the small positive-df branch and the large-df tail for the whole interpolation. |
| sigma | **Open.** Location profiling is available at every fixed df. The remaining step is the joint shape comparison with the optimized normal at held scale and a feasible Cauchy candidate at the floor. | **Open.** Uniform location escape with shape varying and the matched Gaussian limit at all finite limiting locations are needed to exhaust the excluded ends. | **Open.** The exact `(-100, 100)` case needs the normalizer derivative bound over the off-center profile and a tail comparison beyond the centered join. |

### Azzalini skew Student

| Held | Criterion | Failure | Uniqueness |
| --- | --- | --- | --- |
| all free | Checked on unrestricted positive df by choosing an observation, a sufficiently small positive fixed df, zero slant and a vertical collapse path. | Checked for fixed positive df and finite slant by the collapse-power factorization and the strictly positive limiting CDF product. | **Open.** The reflected `±1, ±2^128` construction needs a finite witness above every symmetric fit and above the optimized skew-normal, half-t, half-normal and collapse envelopes on the fitted domain. |
| alpha | **Open.** The negative-exponent path is checked. The nondivergent branch needs the maximized moving critical-displacement trace and exact comparison with the fixed-slant skew-normal. | **Open.** Matched vertical paths are checked. The remaining step is exhaustion when location, scale and df all move, especially the critical displacement and df-rate layer. | **Open for the exact sample.** The symmetric fixed-shape uniqueness argument is available through the ordinary Student nuisance results. The proposed sample still needs the strict comparison of its shape-floor value with every larger shape. |
| alpha+nu | **Open.** Negative-exponent divergence is checked. Strict-mass compactness and the comparison at the maximum of the moving critical layer are needed for finite slant. | **Open.** Vertical critical and negative paths are checked. The atom displacement layer still needs its optimized upper bound and exclusion of other joint escapes. | Checked by transfer of the exact balanced Cauchy argmax at zero slant and df one. |
| alpha+nu+xi | Checked by continuity on positive scale, the two scale limits, and a compact interval whenever a finite witness reaches the critical trace. | Checked by the negative, critical and positive exponent regimes at scale zero and vanishing at infinite scale. | — |
| alpha+xi | **Open.** The majority divergence witness follows from the fixed-shape path at df one. The remaining step is the compact shape-scale comparison with both the matched skew-normal and critical df-one envelope. | **Open.** The fixed-parameter traces are checked. A joint approach in shape and scale needs the critical rate analysis and the large-shape zero-scale exclusion. | — |
| nu | **Open.** The negative-exponent path is checked. The finite comparison needs the optimized half-t moving-contact and critical collapse values and a joint compactness theorem. | **Open.** The half-t path is checked with fixed contact. Moving contacts with simultaneous slant and scale escape still need the bounds attaining each envelope value. | **Open.** The reflected sample at df one requires the profile bounds excluding centered and boundary competitors. |
| nu+xi | **Open.** The scale-only criterion is checked at each finite slant. The joint proof needs uniform small and large scale bounds with slant free, strict critical loss for each finite scale, and uniform suppression of both slant tails when the data have both signs. | **Open.** Divergent and critical vertical paths are checked. For the exact free-slant result, the critical trace must be optimized over slant and every finite point shown to lose at criticality. Positive exponent also requires the held-contact orientation argument. | Checked by pairing via a residual-reversing permutation. Multiplying each skew factor with its reflected factor gives a strict loss for nonzero slant, and the symmetric scale optimum is unique. |
| omega | **Open.** The fixed-nuisance shape and slant limits are checked. A uniform boundedness and compact-superlevel proof with held scale must optimize the moving-contact faces and account for their intersections. | **Open.** A moving location requires evaluation of half-t and half-normal contact factors jointly with df and slant, beyond the checked fixed-contact paths. | **Open.** The exact reflected construction with scale `√5/2` needs the comparison to all boundary envelopes and the exclusion of centered winners. |
| xi | **Open.** The vertical majority witness and both fixed-location slant limits are checked. The remaining comparison joins the normal, oriented contact and critical collapse envelopes across all jointly escaping shape, scale and slant values. | **Open.** Fixed-contact paths are checked. A joint rate analysis and envelope exhaustion are needed when the scale and shape or slant move together. | **Open.** The paired residual argument selects slant zero at each fixed df with a positive exponent. The exact sample still needs the global df-one comparison with every other fitted shape. |

### Half-Student

| Held | Criterion | Failure | Uniqueness |
| --- | --- | --- | --- |
| all free | — | — | **Open.** The existing floor and stationary comparison supplies eligible shapes, and the fixed-shape theory supplies one conditional fit. A finite stationary root list and its highest-value argmax characterization remain. |
| mu | Checked by the scale score, the small-df residual envelope, the profiled normal limit and the stationary witness argument. Endpoint ties give divergence. | **Open for full joint escape classification.** The tie-free no-fit supremum is exactly the RMS half-normal. Exhaustion along arbitrary scale and shape paths still needs the uniform large-df small-scale bound and the positive-scale matched face identification. | **Open.** The unique conditional scale is available. Finiteness needs the analytic root isolation argument and a strict eventual derivative sign at the normal end. |
| mu+nu | Checked for every positive held shape, including all-tied data. | Checked by the exact critical supremum and divergent negative-exponent path at the held endpoint. | — |
| mu+sigma | Checked by the scalar stationary comparison with the matched half-normal. | Checked by the Gamma recurrence at df zero, a peak bound uniform in the residuals, and the normal limit at infinity. | **Open.** The one-magnitude Laplace-transform sign-change argument with optional ties and the general finite-root argument remain. |
| nu | Checked by the exact fixed-shape scale criterion at the sample minimum. | Checked by the critical joint supremum, strict loss everywhere finite at equality, and negative-exponent divergence. | — |
| nu+sigma | Checked by strict residual monotonicity and a finite sample minimum. | Checked by the zero limit at location minus infinity and the included positive contact at the minimum. | Checked by strict improvement from every smaller compatible endpoint. |
| sigma | Checked by profiling the endpoint to the minimum and comparing the included df floor or stationary shape with the matched half-normal. | The matched half-normal is checked as the exact no-fit supremum. A separate moving-location estimate would classify all unprofiled escape paths. | **Open.** The endpoint is unique at each shape. The one-magnitude one-crossing proof and selection of its constrained root or floor have not been formalized. |

### Slash

| Held | Criterion | Failure | Uniqueness |
| --- | --- | --- | --- |
| all free | Checked. The compact superlevel proof handles strict masses, critical equality and strict majority. | Checked. A majority causes divergence, and at half mass loss at all finite points is equivalent to nonattainment with the largest critical value as the supremum. The existing paired-block theorem gives its sufficient strict comparison. | **Open.** The paired collapsing case is available. The exact same-summary off-point witness and an unrestricted multimodality example still need their global comparisons. |
| mu | — | Checked by the existing log-likelihood factorization and a joint sample-to-residual wrapper. | — |
| sigma | Checked by continuity including zero residual and an upper level set bounded in location. | Both infinite-location limits are zero and a positive finite fit attains. | Checked by a strict center-versus-witness comparison for `(-10, 10)`, then reflection of a global maximizer. |

## Limit paths and face comparisons

| Family and held values | Keys | Result or remaining proof |
| --- | --- | --- |
| std, all free | `path:1`, `path:6` | A supercritical atom has a fixed admissible df with a divergent ordinary-scale path. At fixed positive ordinary scale, df decreasing to two gives the Student-two likelihood with sd diverging. |
| std, all free | `limit:1`, `limit:5`, `limit:6` | The supercritical direction of limit 1 is checked. The full detector and finite-profile coverage comparisons remain, including a sound finite-interval cover, tail remainders at the actual Gaussian face, and the independent positive-scale Student-two lower comparison when scale is free. |
| std, mean+nu | `path:2`, `path:3`, `limit:3` | Checked in the ordinary-scale coordinate. The fixed positive multiplier transports the results to the sd coordinate. |
| std, mean+sd | `path:4`, `limit:4` | The exact lower path and weak scalar attainment comparison are checked. The algorithmic certificate still needs a verified derivative enclosure and a finite cover comparing every relevant interior maximum with both faces. |
| std, nu | `path:2`, `path:3`, `limit:3` | The negative vertical path uses the existing `TDist.collapse_unbounded` and held-df reparameterization. The new moving critical path bound and strict joint supremum handle equality. |
| lst, all free | `limit:1`, `limit:4` | The negative-exponent vertical path and ordinary normal path are available. The complete detector comparisons still need the fitted-domain atom threshold, a joint compact profile cover and a uniform normal-tail remainder. |
| lst, df | `path:2`, `path:3`, `limit:3` | The existing negative vertical path and the new critical envelope give both scale paths. The joint critical decision is checked above df one and the Cauchy branch at one. The subunit candidate comparison remains open. |
| st, all free | `path:1` through `path:5` | Checked at the specified fixed location and scale. The two simultaneous limits apply under arbitrary relative speeds of df and slant and include the tied contact factor. |
| st, all free, alpha, alpha+nu, alpha+nu+xi, alpha+xi, nu, nu+xi, xi | `path:6`, `path:7`, `limit:6` | The negative and critical vertical paths are checked for every finite slant. The negative-exponent detector supplies divergence at any reachable atom with the relevant held df or at the fitted floor. The full converse across moving-nuisance escapes in the free-shape masks remains part of the open global-envelope work. |
| ht, all free | `path:1`, `path:2` | Checked with explicit eventual feasibility. The fixed-location and fixed-scale normal path uses the existing half-normal limit. A strict majority gives the divergent path at the included shape floor. |
| ht, nu | `path:2`, `path:3`, `limit:2` | Checked at the sample minimum. The joint compatible likelihood is unbounded exactly at negative exponent, and the critical scale path reaches its strict unattained supremum. |
| slash, all free | `path:1` | Checked. The finite scaled-displacement trace is explicit, and the vertical trace is the sharp upper envelope over all approaching location and scale paths. |

## Lean statements

### StudentCauchy

**`UnivariateML.StudentCauchy.lst_df_one_attains_iff`**

At df one, strict half masses or a balanced Cauchy pair give a finite fit. [Lean source](../../lean/UnivariateML/Family/StudentCauchy.lean#L45)

```lean
theorem lst_df_one_attains_iff {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) :
    (∃ p ∈ Cauchy.domain, IsMaxOn (fun w : ℝ × ℝ ↦ TDist.logLik x w.1 1 w.2) Cauchy.domain p) ↔
      (∀ a, 2 * TDist.multiplicity x a < n) ∨
        ∃ a b, a ≠ b ∧ 2 * TDist.multiplicity x a = n ∧ ∀ i, x i = a ∨ x i = b
```

**`UnivariateML.StudentCauchy.lst_df_one_unique_iff`**

The df-one joint fit is unique exactly with strict half masses. [Lean source](../../lean/UnivariateML/Family/StudentCauchy.lean#L57)

```lean
theorem lst_df_one_unique_iff {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) :
    (∃! p : ℝ × ℝ, p ∈ Cauchy.domain ∧
      IsMaxOn (fun w : ℝ × ℝ ↦ TDist.logLik x w.1 1 w.2) Cauchy.domain p) ↔
      ∀ a, 2 * TDist.multiplicity x a < n
```

**`UnivariateML.StudentCauchy.lst_df_one_balanced_argmax`**

The full balanced Cauchy argmax is the upper open semicircle. [Lean source](../../lean/UnivariateML/Family/StudentCauchy.lean#L70)

```lean
theorem lst_df_one_balanced_argmax {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    {a b : ℝ} (hab : a < b) (ha : 2 * TDist.multiplicity x a = n)
    (hx : ∀ i, x i = a ∨ x i = b) (p : ℝ × ℝ) :
    (p ∈ Cauchy.domain ∧
      IsMaxOn (fun w : ℝ × ℝ ↦ TDist.logLik x w.1 1 w.2) Cauchy.domain p) ↔
      a < p.1 ∧ p.1 < b ∧ 0 < p.2 ∧ p.2^2 = (p.1 - a) * (b - p.1)
```

### StudentFaces

**`UnivariateML.StudentFaces.std_student_two_path`**

The ordinary-scale Student-two likelihood is approached with diverging standard deviation. [Lean source](../../lean/UnivariateML/Family/StudentFaces.lean#L45)

```lean
theorem std_student_two_path {n : ℕ} (x : Fin n → ℝ) (μ : ℝ) {a : ℝ}
    (ha : 0 < a) :
    let σ := fun ν : ℝ ↦ a * sqrt (ν / (ν - 2))
    Tendsto σ (𝓝[>] 2) atTop ∧
      Tendsto (fun ν ↦ exp (TDist.stdLogLik x μ ν (σ ν))) (𝓝[>] 2)
        (𝓝 (TDist.likelihood x μ 2 a))
```

**`UnivariateML.StudentFaces.std_fixed_sd_collapse`**

At df two the held-mean and held-sd likelihood tends to zero, a finite positive trace, or infinity as the exponent changes sign. [Lean source](../../lean/UnivariateML/Family/StudentFaces.lean#L141)

```lean
theorem std_fixed_sd_collapse {n : ℕ} (x : Fin n → ℝ) (μ : ℝ) {σ : ℝ}
    (hσ : 0 < σ) :
    let L := fun ν : ℝ ↦ exp (TDist.stdLogLik x μ ν σ)
    (0 < TDist.exponent x μ 2 → Tendsto L (𝓝[>] 2) (𝓝 0)) ∧
    (TDist.exponent x μ 2 = 0 → Tendsto L (𝓝[>] 2) (𝓝 (exp (fixedSdCollapseLog x μ)))) ∧
    (TDist.exponent x μ 2 < 0 → Tendsto L (𝓝[>] 2) atTop)
```

**`UnivariateML.StudentFaces.std_fixed_sd_normal`**

The upper shape endpoint tends to the Gaussian at the same held mean and standard deviation. [Lean source](../../lean/UnivariateML/Family/StudentFaces.lean#L193)

```lean
theorem std_fixed_sd_normal {n : ℕ} (x : Fin n → ℝ) (μ : ℝ) {σ : ℝ}
    (hσ : 0 < σ) :
    Tendsto (fun ν ↦ exp (TDist.stdLogLik x μ ν σ)) atTop
      (𝓝 (∏ i, TDist.normalDensity μ σ (x i)))
```

**`UnivariateML.StudentFaces.std_held_mean_df_collapse`**

A negative exponent gives divergence and a zero exponent gives a strictly unattained finite supremum at held mean and df. [Lean source](../../lean/UnivariateML/Family/StudentFaces.lean#L205)

```lean
theorem std_held_mean_df_collapse {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (μ : ℝ) {ν : ℝ} (hν : 2 < ν) :
    (TDist.exponent x μ ν < 0 →
      Tendsto (TDist.likelihood x μ ν) (𝓝[>] 0) atTop) ∧
    (TDist.exponent x μ ν = 0 →
      Tendsto (TDist.likelihood x μ ν) (𝓝[>] 0) (𝓝 (exp (TDist.collapseLogValue x μ ν))) ∧
      sSup (TDist.likelihood x μ ν '' Ioi 0) = exp (TDist.collapseLogValue x μ ν) ∧
      ∀ a > 0, TDist.likelihood x μ ν a < exp (TDist.collapseLogValue x μ ν))
```

**`UnivariateML.StudentFaces.std_supercritical_atom`**

Above two thirds at a reachable atom, an admissible df has a divergent scale path. [Lean source](../../lean/UnivariateML/Family/StudentFaces.lean#L225)

```lean
theorem std_supercritical_atom {n : ℕ} (x : Fin n → ℝ) (μ : ℝ)
    (hm : 2 * n < 3 * TDist.multiplicity x μ) :
    ∃ ν > 2, Tendsto (TDist.likelihood x μ ν) (𝓝[>] 0) atTop
```

**`UnivariateML.StudentFaces.lst_critical_joint`**

At held df above one, a critical atom determines a finite unattained joint supremum. [Lean source](../../lean/UnivariateML/Family/StudentFaces.lean#L345)

```lean
theorem lst_critical_joint {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    {b ν : ℝ} (hν : 1 < ν) (he : TDist.exponent x b ν = 0) :
    IsLUB ((fun m : ℝ × ℝ ↦ TDist.likelihood x m.1 ν m.2) '' (univ ×ˢ Ioi 0))
      (exp (TDist.collapseLogValue x b ν)) ∧
      ∀ m a : ℝ, 0 < a → TDist.likelihood x m ν a < exp (TDist.collapseLogValue x b ν)
```

**`UnivariateML.StudentFaces.std_fixed_sd_criterion`**

A finite shape maximum exists iff the mass is at most critical and a stationary shape reaches both boundary values. [Lean source](../../lean/UnivariateML/Family/StudentFaces.lean#L459)

```lean
theorem std_fixed_sd_criterion {n : ℕ} (x : Fin n → ℝ) (μ : ℝ) {σ : ℝ} (hσ : 0 < σ) :
    let L := fun v ↦ exp (TDist.stdLogLik x μ v σ)
    let lower := if TDist.exponent x μ 2 = 0 then exp (fixedSdCollapseLog x μ) else 0
    let normal := ∏ i, TDist.normalDensity μ σ (x i)
    (∃ v ∈ Ioi 2, IsMaxOn L (Ioi 2) v) ↔
      0 ≤ TDist.exponent x μ 2 ∧
        ∃ v > 2, HasDerivAt L 0 v ∧ max lower normal ≤ L v
```

**`UnivariateML.StudentFaces.half_fixed_endpoint_scale_ends`**

Held endpoint and scale give zero at df zero and half-normal at infinite df, including endpoint ties. [Lean source](../../lean/UnivariateML/Family/StudentFaces.lean#L534)

```lean
theorem half_fixed_endpoint_scale_ends {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (μ : ℝ) {s : ℝ} (hs : 0 < s) :
    Tendsto (fun v ↦ SkewLSHalfT.likelihood x μ v s) (𝓝[>] 0) (𝓝 0) ∧
      Tendsto (fun v ↦ SkewLSHalfT.likelihood x μ v s) atTop
        (𝓝 (∏ i, 2*SkewLSGaussian.φ ((x i-μ)/s)/s))
```

**`UnivariateML.StudentFaces.half_fixed_endpoint_scale_criterion`**

With endpoint and scale held, a stationary shape must reach the matched half-normal for attainment. [Lean source](../../lean/UnivariateML/Family/StudentFaces.lean#L561)

```lean
theorem half_fixed_endpoint_scale_criterion {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (μ : ℝ) {s : ℝ} (hs : 0 < s) :
    let L := fun v ↦ SkewLSHalfT.likelihood x μ v s
    (∃ v ∈ Ioi 0, IsMaxOn L (Ioi 0) v) ↔
      ∃ v > 0, HasDerivAt L 0 v ∧ (∏ i, 2*SkewLSGaussian.φ ((x i-μ)/s)/s) ≤ L v
```

**`UnivariateML.StudentFaces.half_held_endpoint_criterion`**

At held endpoint, endpoint ties exclude attainment and otherwise a stationary fit must reach the RMS half-normal. [Lean source](../../lean/UnivariateML/Family/StudentFaces.lean#L652)

```lean
theorem half_held_endpoint_criterion {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) (μ : ℝ) :
    let D := (Ioi (0 : ℝ)) ×ˢ Ioi (0 : ℝ)
    let L := fun p : ℝ × ℝ ↦ SkewLSHalfT.likelihood x μ p.1 p.2
    (∃ p ∈ D, IsMaxOn L D p) ↔
      (∀ i, x i ≠ μ) ∧ ∃ ν > 0, ∃ s > 0,
        TDist.scaleScore x μ ν s = 0 ∧
        HasDerivAt (fun v ↦ SkewLSHalfT.likelihood x μ v s) 0 ν ∧
        SkewLSHalfT.normalFace x μ ≤ SkewLSHalfT.likelihood x μ ν s
```

**`UnivariateML.StudentFaces.lst_held_location_criterion`**

With location held, attainment is equivalent to having no ties at that location and a stationary shape-scale fit reaching the RMS Gaussian. [Lean source](../../lean/UnivariateML/Family/StudentFaces.lean#L728)

```lean
theorem lst_held_location_criterion {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) (μ : ℝ) :
    let D := Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ)
    let L := fun p : ℝ × ℝ ↦ TDist.likelihood x μ p.1 p.2
    (∃ p ∈ D, IsMaxOn L D p) ↔
      (∀ i, x i ≠ μ) ∧ ∃ ν > 0, ∃ s > 0,
        TDist.scaleScore x μ ν s = 0 ∧
        HasDerivAt (fun v ↦ TDist.likelihood x μ v s) 0 ν ∧
        SkewLSHalfT.normalFace x μ / (2 : ℝ)^n ≤ TDist.likelihood x μ ν s
```

**`UnivariateML.StudentFaces.half_held_endpoint_unattained_supremum`**

For tie-free held-endpoint data, the RMS half-normal is the exact supremum when no finite fit exists. [Lean source](../../lean/UnivariateML/Family/StudentFaces.lean#L770)

```lean
theorem half_held_endpoint_unattained_supremum {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (μ : ℝ) (hx : ∀ i, x i ≠ μ)
    (h : ¬ ∃ p ∈ Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ),
      IsMaxOn (fun q : ℝ × ℝ ↦ SkewLSHalfT.likelihood x μ q.1 q.2)
        (Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ)) p) :
    IsLUB ((fun q : ℝ × ℝ ↦ SkewLSHalfT.likelihood x μ q.1 q.2) ''
      (Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ))) (SkewLSHalfT.normalFace x μ)
```

**`UnivariateML.StudentFaces.lst_fixed_df_remote_ends`**

At held shape both large scale and remote location give zero likelihood, uniformly in the other coordinate. [Lean source](../../lean/UnivariateML/Family/StudentFaces.lean#L816)

```lean
theorem lst_fixed_df_remote_ends {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    {ν : ℝ} (hν : 0 < ν) {ι : Type*} {l : Filter ι} {m s : ι → ℝ}
    (hs : ∀ᶠ j in l, 0 < s j)
    (ht : Tendsto s l atTop ∨ Tendsto m l (cocompact ℝ)) :
    Tendsto (fun j ↦ TDist.likelihood x (m j) ν (s j)) l (𝓝 0)
```

**`UnivariateML.StudentFaces.lst_fixed_df_local_collapse`**

At small scale and fixed limiting location, positive exponent forces zero and zero exponent gives a sharp finite envelope. [Lean source](../../lean/UnivariateML/Family/StudentFaces.lean#L888)

```lean
theorem lst_fixed_df_local_collapse {n : ℕ} (x : Fin n → ℝ) (b : ℝ) {ν : ℝ}
    (hν : 0 < ν) {ι : Type*} {l : Filter ι} {m a : ι → ℝ}
    (hm : Tendsto m l (𝓝 b)) (ha : Tendsto a l (𝓝[>] 0)) :
    (0 < TDist.exponent x b ν →
      Tendsto (fun j ↦ TDist.likelihood x (m j) ν (a j)) l (𝓝 0)) ∧
    (TDist.exponent x b ν = 0 →
      Tendsto (TDist.likelihood x b ν) (𝓝[>] 0) (𝓝 (exp (TDist.collapseLogValue x b ν))) ∧
      ∀ B > exp (TDist.collapseLogValue x b ν),
        ∀ᶠ j in l, TDist.likelihood x (m j) ν (a j) < B)
```

**`UnivariateML.StudentFaces.half_held_scale_criterion`**

With scale held, the shape floor or a finite stationary shape at the minimum must reach its matched half-normal value. [Lean source](../../lean/UnivariateML/Family/StudentFaces.lean#L955)

```lean
theorem half_held_scale_criterion {n : ℕ} (x : Fin n → ℝ) {i : Fin n}
    (hmin : ∀ j, x i ≤ x j) {s : ℝ} (hs : 0 < s) :
    let D := {p : ℝ × ℝ | (∀ j, p.1 ≤ x j) ∧ 1 ≤ p.2}
    let L := fun p : ℝ × ℝ ↦ SkewLSHalfT.likelihood x p.1 p.2 s
    let P := fun v ↦ SkewLSHalfT.likelihood x (x i) v s
    let H := ∏ j, 2 * SkewLSGaussian.φ ((x j-x i)/s)/s
    (∃ p ∈ D, IsMaxOn L D p) ↔
      ∃ v ≥ 1, (v = 1 ∨ HasDerivAt P 0 v) ∧ H ≤ P v
```

**`UnivariateML.StudentFaces.half_held_scale_unattained_supremum`**

If no held-scale finite point maximizes, the matched half-normal at the minimum is the exact supremum. [Lean source](../../lean/UnivariateML/Family/StudentFaces.lean#L993)

```lean
theorem half_held_scale_unattained_supremum {n : ℕ} (x : Fin n → ℝ) {i : Fin n}
    (hmin : ∀ j, x i ≤ x j) {s : ℝ} (hs : 0 < s)
    (h : ¬ ∃ p ∈ {p : ℝ × ℝ | (∀ j, p.1 ≤ x j) ∧ 1 ≤ p.2},
      IsMaxOn (fun q ↦ SkewLSHalfT.likelihood x q.1 q.2 s)
        {p | (∀ j, p.1 ≤ x j) ∧ 1 ≤ p.2} p) :
    IsLUB ((fun p : ℝ × ℝ ↦ SkewLSHalfT.likelihood x p.1 p.2 s) ''
      {p | (∀ j, p.1 ≤ x j) ∧ 1 ≤ p.2})
      (∏ j, 2 * SkewLSGaussian.φ ((x j-x i)/s)/s)
```

### StudentGeodesic

**`UnivariateML.StudentGeodesic.lst_held_df_unique`**

Any two joint ordinary Student maxima at held df above one coincide. [Lean source](../../lean/UnivariateML/Family/StudentGeodesic.lean#L148)

```lean
theorem lst_held_df_unique {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) {ν : ℝ} (hν : 1 < ν)
    {p q : ℝ × ℝ} (hp : 0 < p.2) (hq : 0 < q.2)
    (hP : IsMaxOn (fun z : ℝ × ℝ ↦ TDist.logLik x z.1 ν z.2) (univ ×ˢ Ioi 0) p)
    (hQ : IsMaxOn (fun z : ℝ × ℝ ↦ TDist.logLik x z.1 ν z.2) (univ ×ˢ Ioi 0) q) :
    p = q
```

**`UnivariateML.StudentGeodesic.std_held_df_unique`**

Any two attained joint std location-scale maxima at held df above two coincide. [Lean source](../../lean/UnivariateML/Family/StudentGeodesic.lean#L212)

```lean
theorem std_held_df_unique {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) {ν : ℝ}
    (hν : 2 < ν) {p q : ℝ × ℝ} (hp : 0 < p.2) (hq : 0 < q.2)
    (hP : IsMaxOn (fun z : ℝ × ℝ ↦ TDist.stdLogLik x z.1 ν z.2) (univ ×ˢ Ioi 0) p)
    (hQ : IsMaxOn (fun z : ℝ × ℝ ↦ TDist.stdLogLik x z.1 ν z.2) (univ ×ˢ Ioi 0) q) :
    p = q
```

### StudentHalf

**`UnivariateML.StudentHalf.held_shape_scale_endpoint`**

The sample minimum is the unique maximizing endpoint with positive shape and scale held. [Lean source](../../lean/UnivariateML/Family/StudentHalf.lean#L42)

```lean
theorem held_shape_scale_endpoint {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    {ν s : ℝ} (hν : 0 < ν) (hs : 0 < s) :
    ∃ i : Fin n, (∀ j, x i ≤ x j) ∧
      ∀ μ, ((∀ j, μ ≤ x j) ∧ IsMaxOn (fun m ↦ likelihood x m ν s) {m | ∀ j, m ≤ x j} μ) ↔
        μ = x i
```

**`UnivariateML.StudentHalf.held_shape_scale_escape`**

The likelihood vanishes at endpoint minus infinity. The minimum is feasible with positive likelihood. [Lean source](../../lean/UnivariateML/Family/StudentHalf.lean#L64)

```lean
theorem held_shape_scale_escape {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    {ν s : ℝ} (hν : 0 < ν) (hs : 0 < s) :
    Tendsto (fun m ↦ likelihood x m ν s) atBot (𝓝 0) ∧
      ∃ i : Fin n, x i ∈ {m | ∀ j, m ≤ x j} ∧ 0 < likelihood x (x i) ν s
```

**`UnivariateML.StudentHalf.held_endpoint_shape_criterion`**

With endpoint and positive shape held, a scale maximum exists exactly at positive exponent. [Lean source](../../lean/UnivariateML/Family/StudentHalf.lean#L89)

```lean
theorem held_endpoint_shape_criterion {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (μ : ℝ) {ν : ℝ} (hν : 0 < ν) :
    (∃ s ∈ Ioi 0, IsMaxOn (likelihood x μ ν) (Ioi 0) s) ↔
      0 < TDist.exponent x μ ν
```

**`UnivariateML.StudentHalf.held_endpoint_shape_failure`**

Negative exponent gives a divergent path and equality gives a finite unattained supremum. [Lean source](../../lean/UnivariateML/Family/StudentHalf.lean#L114)

```lean
theorem held_endpoint_shape_failure {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (μ : ℝ) {ν : ℝ} (hν : 0 < ν) :
    (TDist.exponent x μ ν < 0 → Tendsto (likelihood x μ ν) (𝓝[>] 0) atTop) ∧
    (TDist.exponent x μ ν = 0 →
      Tendsto (likelihood x μ ν) (𝓝[>] 0) (𝓝 (collapseValue x μ ν)) ∧
      IsLUB (likelihood x μ ν '' Ioi 0) (collapseValue x μ ν) ∧
      ∀ a > 0, likelihood x μ ν a < collapseValue x μ ν)
```

**`UnivariateML.StudentHalf.held_shape_collapse`**

The held-shape vertical paths at the minimum have the corresponding divergent or critical limits, with an exact critical joint supremum. [Lean source](../../lean/UnivariateML/Family/StudentHalf.lean#L125)

```lean
theorem held_shape_collapse {n : ℕ} (x : Fin n → ℝ) {i : Fin n}
    (hmin : ∀ j, x i ≤ x j) {ν : ℝ} (hν : 0 < ν) :
    (TDist.exponent x (x i) ν < 0 → Tendsto (likelihood x (x i) ν) (𝓝[>] 0) atTop) ∧
    (TDist.exponent x (x i) ν = 0 →
      Tendsto (likelihood x (x i) ν) (𝓝[>] 0) (𝓝 (collapseValue x (x i) ν)) ∧
      IsLUB ((fun p : ℝ × ℝ ↦ likelihood x p.1 ν p.2) ''
        {p | (∀ j, p.1 ≤ x j) ∧ 0 < p.2}) (collapseValue x (x i) ν) ∧
      ∀ m a : ℝ, (∀ j, m ≤ x j) → 0 < a → likelihood x m ν a < collapseValue x (x i) ν)
```

**`UnivariateML.StudentHalf.half_normal_feasible_path`**

At a feasible endpoint and positive scale, the infinite-shape path stays feasible and reaches the matched half-normal. [Lean source](../../lean/UnivariateML/Family/StudentHalf.lean#L139)

```lean
theorem half_normal_feasible_path {n : ℕ} (x : Fin n → ℝ) (μ : ℝ)
    (hμ : ∀ j, μ ≤ x j) {s : ℝ} (hs : 0 < s) :
    Tendsto (fun ν ↦ likelihood x μ ν s) atTop
      (𝓝 (∏ i, 2*SkewLSGaussian.φ ((x i-μ)/s)/s)) ∧
      ∀ᶠ ν in atTop, (μ,ν,s) ∈ FinishHalfT.domain x
```

**`UnivariateML.StudentHalf.held_shape_criterion`**

With shape held and endpoint free, a compatible joint fit exists exactly at positive exponent at the minimum. [Lean source](../../lean/UnivariateML/Family/StudentHalf.lean#L150)

```lean
theorem held_shape_criterion {n : ℕ} (x : Fin n → ℝ) {i : Fin n} (hmin : ∀ j, x i ≤ x j)
    {ν : ℝ} (hν : 0 < ν) :
    let D : Set (ℝ × ℝ) := {p | (∀ j, p.1 ≤ x j) ∧ 0 < p.2}
    (∃ p ∈ D, IsMaxOn (fun p ↦ likelihood x p.1 ν p.2) D p) ↔
      0 < TDist.exponent x (x i) ν
```

**`UnivariateML.StudentHalf.held_shape_unbounded_iff`**

At held shape the joint likelihood is unbounded exactly at negative exponent at the minimum. [Lean source](../../lean/UnivariateML/Family/StudentHalf.lean#L179)

```lean
theorem held_shape_unbounded_iff {n : ℕ} (x : Fin n → ℝ) {i : Fin n}
    (hmin : ∀ j, x i ≤ x j) {ν : ℝ} (hν : 0 < ν) :
    (¬ BddAbove ((fun p : ℝ × ℝ ↦ likelihood x p.1 ν p.2) ''
      {p | (∀ j, p.1 ≤ x j) ∧ 0 < p.2})) ↔ TDist.exponent x (x i) ν < 0
```

**`UnivariateML.StudentHalf.all_free_majority_path`**

A strict majority at the minimum gives a feasible divergent path at the fitted shape floor. [Lean source](../../lean/UnivariateML/Family/StudentHalf.lean#L201)

```lean
theorem all_free_majority_path {n : ℕ} (x : Fin n → ℝ) {i : Fin n}
    (hmin : ∀ j, x i ≤ x j) (hm : n < 2 * TDist.multiplicity x (x i)) :
    Tendsto (likelihood x (x i) 1) (𝓝[>] 0) atTop ∧
      ∀ᶠ a in 𝓝[>] (0 : ℝ), (x i, (1 : ℝ), a) ∈ FinishHalfT.domain x
```

### StudentHeldShape

**`UnivariateML.StudentHeldShape.lst_strict_atoms_attain`**

Strictly positive exponents at every atom give a joint fit at any held positive df. [Lean source](../../lean/UnivariateML/Family/StudentHeldShape.lean#L139)

```lean
theorem lst_strict_atoms_attain (x : Fin n → ℝ) (hn : 0 < n) {ν : ℝ} (hν : 0 < ν)
    (hE : ∀ i, 0 < TDist.exponent x (x i) ν) :
    ∃ p ∈ (univ ×ˢ Ioi (0 : ℝ)),
      IsMaxOn (fun z : ℝ × ℝ ↦ TDist.logLik x z.1 ν z.2) (univ ×ˢ Ioi 0) p
```

**`UnivariateML.StudentHeldShape.std_held_df_attains_iff`**

Joint std attainment at held df is equivalent to positive exponent at every sample atom. [Lean source](../../lean/UnivariateML/Family/StudentHeldShape.lean#L207)

```lean
theorem std_held_df_attains_iff (x : Fin n → ℝ) (hn : 0 < n) {ν : ℝ} (hν : 2 < ν) :
    (∃ p ∈ (univ ×ˢ Ioi (0 : ℝ)),
      IsMaxOn (fun z : ℝ × ℝ ↦ TDist.stdLogLik x z.1 ν z.2) (univ ×ˢ Ioi 0) p) ↔
      ∀ i, 0 < TDist.exponent x (x i) ν
```

**`UnivariateML.StudentHeldShape.std_critical_joint`**

At a critical atom and held df above two, the joint std supremum is finite and unattained. [Lean source](../../lean/UnivariateML/Family/StudentHeldShape.lean#L254)

```lean
theorem std_critical_joint (x : Fin n → ℝ) (hn : 0 < n) {b ν : ℝ}
    (hν : 2 < ν) (he : TDist.exponent x b ν = 0) :
    IsLUB ((fun z : ℝ × ℝ ↦ exp (TDist.stdLogLik x z.1 ν z.2)) '' (univ ×ˢ Ioi 0))
      (exp (TDist.collapseLogValue x b ν)) ∧
    ∀ m σ : ℝ, 0 < σ → exp (TDist.stdLogLik x m ν σ) < exp (TDist.collapseLogValue x b ν)
```

### StudentLocation

**`UnivariateML.StudentLocation.std_location_ends`**

The likelihood tends to zero at both infinite locations. [Lean source](../../lean/UnivariateML/Family/StudentLocation.lean#L71)

```lean
theorem std_location_ends (x : Fin n → ℝ) (hn : 0 < n) {ν σ : ℝ}
    (hν : 2 < ν) (hσ : 0 < σ) :
    Tendsto (fun μ ↦ exp (TDist.stdLogLik x μ ν σ)) atBot (𝓝 0) ∧
      Tendsto (fun μ ↦ exp (TDist.stdLogLik x μ ν σ)) atTop (𝓝 0)
```

**`UnivariateML.StudentLocation.std_location_attainment`**

Every nonempty sample attains at a finite location. [Lean source](../../lean/UnivariateML/Family/StudentLocation.lean#L85)

```lean
theorem std_location_attainment (x : Fin n → ℝ) (hn : 0 < n) {ν σ : ℝ}
    (hν : 2 < ν) (hσ : 0 < σ) :
    ∃ μ, IsMaxOn (fun m ↦ TDist.stdLogLik x m ν σ) univ μ
```

**`UnivariateML.StudentLocation.std_two_point_maximizers`**

At df three and sd one, the sample (-2, 2) has exactly the locations ±√3 as maximizers. [Lean source](../../lean/UnivariateML/Family/StudentLocation.lean#L135)

```lean
theorem std_two_point_maximizers (m : ℝ) :
    IsMaxOn (fun u ↦ TDist.stdLogLik ![-2, 2] u 3 1) univ m ↔
      m = sqrt 3 ∨ m = -sqrt 3
```

**`UnivariateML.StudentLocation.lst_location_ends`**

Both infinite locations have vanishing likelihood at fixed df and scale. [Lean source](../../lean/UnivariateML/Family/StudentLocation.lean#L147)

```lean
theorem lst_location_ends (x : Fin n → ℝ) (hn : 0 < n) {ν a : ℝ}
    (hν : 0 < ν) (ha : 0 < a) :
    Tendsto (fun μ ↦ TDist.likelihood x μ ν a) atBot (𝓝 0) ∧
      Tendsto (fun μ ↦ TDist.likelihood x μ ν a) atTop (𝓝 0)
```

**`UnivariateML.StudentLocation.lst_location_attainment`**

Each nonempty sample has a finite location maximum. [Lean source](../../lean/UnivariateML/Family/StudentLocation.lean#L157)

```lean
theorem lst_location_attainment (x : Fin n → ℝ) (hn : 0 < n) {ν a : ℝ}
    (hν : 0 < ν) (ha : 0 < a) :
    ∃ μ, IsMaxOn (fun m ↦ TDist.logLik x m ν a) univ μ
```

**`UnivariateML.StudentLocation.lst_two_point_maximizers`**

At df one and scale three, the sample (-5, 5) has exactly the locations ±4 as maximizers. [Lean source](../../lean/UnivariateML/Family/StudentLocation.lean#L165)

```lean
theorem lst_two_point_maximizers (m : ℝ) :
    IsMaxOn (fun u ↦ TDist.logLik ![-5, 5] u 1 3) univ m ↔ m = 4 ∨ m = -4
```

### StudentSkew

**`UnivariateML.StudentSkew.skew_collapse_power`**

Removing the scale power from the vertical skew-t likelihood leaves an explicit limiting CDF product. [Lean source](../../lean/UnivariateML/Family/StudentSkew.lean#L178)

```lean
theorem skew_collapse_power {n : ℕ} (x : Fin n → ℝ) (μ α : ℝ) {ν : ℝ}
    (hν : 0 < ν) :
    Tendsto (fun a ↦ likelihood x μ ν α a / a ^ TDist.exponent x μ ν) (𝓝[>] 0)
      (𝓝 (collapseTrace x μ ν α))
```

**`UnivariateML.StudentSkew.skew_negative_path`**

Negative exponent gives infinite vertical likelihood for any finite held slant and positive df. [Lean source](../../lean/UnivariateML/Family/StudentSkew.lean#L192)

```lean
theorem skew_negative_path {n : ℕ} (x : Fin n → ℝ) (μ α : ℝ) {ν : ℝ}
    (hν : 0 < ν) (he : TDist.exponent x μ ν < 0) :
    Tendsto (likelihood x μ ν α) (𝓝[>] 0) atTop
```

**`UnivariateML.StudentSkew.skew_critical_path`**

A critical vertical path converges to the finite positive CDF-weighted collapse value. [Lean source](../../lean/UnivariateML/Family/StudentSkew.lean#L207)

```lean
theorem skew_critical_path {n : ℕ} (x : Fin n → ℝ) (μ α : ℝ) {ν : ℝ}
    (hν : 0 < ν) (he : TDist.exponent x μ ν = 0) :
    Tendsto (likelihood x μ ν α) (𝓝[>] 0) (𝓝 (collapseTrace x μ ν α)) ∧
      0 < collapseTrace x μ ν α
```

**`UnivariateML.StudentSkew.symmetric_collapse_unbounded`**

Zero slant reduces negative-exponent collapse to the ordinary Student path. [Lean source](../../lean/UnivariateML/Family/StudentSkew.lean#L219)

```lean
theorem symmetric_collapse_unbounded {n : ℕ} (x : Fin n → ℝ) (μ : ℝ) {ν : ℝ}
    (hν : 0 < ν) (he : TDist.exponent x μ ν < 0) :
    Tendsto (likelihood x μ ν 0) (𝓝[>] 0) atTop
```

**`UnivariateML.StudentSkew.unrestricted_unbounded`**

Every nonempty sample has unbounded joint likelihood on the unrestricted positive-df domain. [Lean source](../../lean/UnivariateML/Family/StudentSkew.lean#L228)

```lean
theorem unrestricted_unbounded {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) :
    ¬ BddAbove ((fun p : ℝ × ℝ × ℝ × ℝ ↦ likelihood x p.1 p.2.1 p.2.2.1 p.2.2.2) ''
      {p | 0 < p.2.1 ∧ 0 < p.2.2.2})
```

**`UnivariateML.StudentSkew.skew_slant_paths`**

At fixed location, shape and scale, the two slant limits are the oriented half-t products with the correct contact factor. [Lean source](../../lean/UnivariateML/Family/StudentSkew.lean#L283)

```lean
theorem skew_slant_paths {n : ℕ} (x : Fin n → ℝ) (μ : ℝ) {ν a : ℝ}
    (hν : 0 < ν) (ha : 0 < a) :
    Tendsto (fun α ↦ likelihood x μ ν α a) atTop
      (𝓝 (TDist.likelihood x μ ν a *
        ∏ i, if μ < x i then 2 else if x i = μ then 1 else 0)) ∧
    Tendsto (fun α ↦ likelihood x μ ν α a) atBot
      (𝓝 (TDist.likelihood x μ ν a *
        ∏ i, if x i < μ then 2 else if x i = μ then 1 else 0))
```

**`UnivariateML.StudentSkew.skew_normal_path`**

At fixed location, scale and finite slant, the likelihood tends to its matched skew-normal product. [Lean source](../../lean/UnivariateML/Family/StudentSkew.lean#L460)

```lean
theorem skew_normal_path {n : ℕ} (x : Fin n → ℝ) (μ α : ℝ) {a : ℝ} (ha : 0 < a) :
    Tendsto (fun ν ↦ likelihood x μ ν α a) atTop
      (𝓝 ((∏ i, TDist.normalDensity μ a (x i)) *
        ∏ i, 2*SkewLSGaussian.Φ (α*(x i-μ)/a)))
```

**`UnivariateML.StudentSkew.skew_joint_infinite_paths`**

Simultaneously infinite df and signed slant give the two oriented half-normal products. [Lean source](../../lean/UnivariateML/Family/StudentSkew.lean#L476)

```lean
theorem skew_joint_infinite_paths {n : ℕ} (x : Fin n → ℝ) (μ : ℝ) {a : ℝ} (ha : 0 < a)
    {ι : Type*} {l : Filter ι} {ν α : ι → ℝ} (hν : Tendsto ν l atTop) :
    (Tendsto α l atTop →
      Tendsto (fun j ↦ likelihood x μ (ν j) (α j) a) l
        (𝓝 ((∏ i, TDist.normalDensity μ a (x i)) *
          ∏ i, if μ < x i then 2 else if x i = μ then 1 else 0))) ∧
    (Tendsto α l atBot →
      Tendsto (fun j ↦ likelihood x μ (ν j) (α j) a) l
        (𝓝 ((∏ i, TDist.normalDensity μ a (x i)) *
          ∏ i, if x i < μ then 2 else if x i = μ then 1 else 0)))
```

**`UnivariateML.StudentSkew.held_slant_df_location_criterion`**

With only scale free, positive exponent yields attainment and critical attainment is equivalent to reaching the trace at a finite scale. [Lean source](../../lean/UnivariateML/Family/StudentSkew.lean#L626)

```lean
theorem held_slant_df_location_criterion {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (μ α : ℝ) {ν : ℝ} (hν : 0 < ν) :
    (∃ a ∈ Ioi 0, IsMaxOn (likelihood x μ ν α) (Ioi 0) a) ↔
      0 < TDist.exponent x μ ν ∨
        (TDist.exponent x μ ν = 0 ∧ ∃ a > 0, collapseTrace x μ ν α ≤ likelihood x μ ν α a)
```

**`UnivariateML.StudentSkew.held_slant_df_location_ends`**

The only two scalar scale ends have the three collapse regimes at zero and vanish at infinity. [Lean source](../../lean/UnivariateML/Family/StudentSkew.lean#L652)

```lean
theorem held_slant_df_location_ends {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (μ α : ℝ) {ν : ℝ} (hν : 0 < ν) :
    (TDist.exponent x μ ν < 0 → Tendsto (likelihood x μ ν α) (𝓝[>] 0) atTop) ∧
    (TDist.exponent x μ ν = 0 →
      Tendsto (likelihood x μ ν α) (𝓝[>] 0) (𝓝 (collapseTrace x μ ν α)) ∧
      0 < collapseTrace x μ ν α) ∧
    (0 < TDist.exponent x μ ν → Tendsto (likelihood x μ ν α) (𝓝[>] 0) (𝓝 0)) ∧
    Tendsto (likelihood x μ ν α) atTop (𝓝 0)
```

**`UnivariateML.StudentSkew.skew_cauchy_arc`**

At zero slant and df one, the sample (-1, 1) has precisely the upper unit semicircle of joint maximizers. [Lean source](../../lean/UnivariateML/Family/StudentSkew.lean#L665)

```lean
theorem skew_cauchy_arc (p : ℝ × ℝ) :
    (p ∈ Cauchy.domain ∧
      IsMaxOn (fun q : ℝ × ℝ ↦ likelihood ![-1,1] q.1 1 0 q.2) Cauchy.domain p) ↔
      0 < p.2 ∧ p.1^2+p.2^2 = 1
```

**`UnivariateML.StudentSkew.paired_held_df_location_unique`**

At positive exponent, residuals paired by a sign-reversing permutation give exactly one maximum, at zero slant. [Lean source](../../lean/UnivariateML/Family/StudentSkew.lean#L741)

```lean
theorem paired_held_df_location_unique {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) (μ : ℝ)
    (σ : Equiv.Perm (Fin n)) (hpair : ∀ i, x (σ i) - μ = -(x i-μ))
    {ν : ℝ} (hν : 0 < ν) (he : 0 < TDist.exponent x μ ν) :
    ∃ s > 0, ∀ p : ℝ × ℝ,
      (p ∈ univ ×ˢ Ioi 0 ∧
        IsMaxOn (fun q : ℝ × ℝ ↦ likelihood x μ ν q.1 q.2) (univ ×ˢ Ioi 0) p) ↔
      p = (0,s)
```

### StudentSlash

**`UnivariateML.StudentSlash.held_scale_attainment`**

Any positive held scale gives a location maximum and zero likelihood at both infinite-location ends. [Lean source](../../lean/UnivariateML/Family/StudentSlash.lean#L110)

```lean
theorem held_scale_attainment (x : Fin n → ℝ) (hn : 0 < n) {s : ℝ} (hs : 0 < s) :
    (∃ m, IsMaxOn (fun u ↦ likelihood x u s) univ m) ∧
      Tendsto (fun m ↦ likelihood x m s) atBot (𝓝 0) ∧
      Tendsto (fun m ↦ likelihood x m s) atTop (𝓝 0)
```

**`UnivariateML.StudentSlash.held_scale_multiple_maxima`**

At scale one, the sample (-10, 10) has at least two distinct reflected global location maximizers. [Lean source](../../lean/UnivariateML/Family/StudentSlash.lean#L125)

```lean
theorem held_scale_multiple_maxima :
    ∃ m : ℝ, m ≠ -m ∧
      IsMaxOn (fun u ↦ likelihood ![-10,10] u 1) univ m ∧
      IsMaxOn (fun u ↦ likelihood ![-10,10] u 1) univ (-m)
```

**`UnivariateML.StudentSlash.held_location_failure`**

A half tie at the held location gives a finite unattained supremum and a strict majority gives divergence. [Lean source](../../lean/UnivariateML/Family/StudentSlash.lean#L164)

```lean
theorem held_location_failure (x : Fin n → ℝ) (hn : 0 < n) (m : ℝ) :
    let r := fun i ↦ x i-m
    (n < 2 * ties r → Tendsto (logLik r) (𝓝[>] 0) atTop) ∧
    (n = 2 * ties r → Tendsto (logLik r) (𝓝[>] 0) (𝓝 (collapseLog r)) ∧
      IsLUB (logLik r '' Ioi 0) (collapseLog r) ∧
      ∀ s > 0, logLik r s < collapseLog r)
```

**`UnivariateML.StudentSlash.moving_half_tie`**

The half-tie trace for finite scaled displacement has an explicit displacement factor. [Lean source](../../lean/UnivariateML/Family/StudentSlash.lean#L261)

```lean
theorem moving_half_tie {ι : Type*} {l : Filter ι} (x : Fin n → ℝ) (b t : ℝ)
    (he : n = 2 * ties (fun i ↦ x i-b)) {m s : ι → ℝ}
    (hm : Tendsto m l (𝓝 b)) (hs : Tendsto s l (𝓝[>] 0))
    (ht : Tendsto (fun j ↦ (m j-b)/s j) l (𝓝 t)) :
    Tendsto (fun j ↦ likelihood x (m j) (s j)) l
      (𝓝 (collapseValue x b *
        (if t = 0 then 1 else (1-exp (-(t^2/2)))/(t^2/2)) ^ (ties fun i ↦ x i-b)))
```

**`UnivariateML.StudentSlash.joint_majority_unbounded`**

A strict majority of observations at an atom gives unbounded likelihood. [Lean source](../../lean/UnivariateML/Family/StudentSlash.lean#L464)

```lean
theorem joint_majority_unbounded (x : Fin n → ℝ) (b : ℝ)
    (h : n < 2 * ties (fun i ↦ x i-b)) :
    ¬ BddAbove ((fun p : ℝ × ℝ ↦ likelihood x p.1 p.2) '' (univ ×ˢ Ioi 0))
```

**`UnivariateML.StudentSlash.joint_attainment_iff`**

At most half mass is necessary, and attainment holds exactly when a positive-scale witness reaches all critical faces. [Lean source](../../lean/UnivariateML/Family/StudentSlash.lean#L482)

```lean
theorem joint_attainment_iff (x : Fin n → ℝ) (hn : 0 < n) :
    (∃ p ∈ (univ ×ˢ Ioi (0 : ℝ)),
      IsMaxOn (fun q : ℝ × ℝ ↦ likelihood x q.1 q.2) (univ ×ˢ Ioi 0) p) ↔
      (∀ b, 2 * ties (fun i ↦ x i-b) ≤ n) ∧
      ∃ m s : ℝ, 0 < s ∧
        ∀ b, n = 2 * ties (fun i ↦ x i-b) → collapseValue x b ≤ likelihood x m s
```

**`UnivariateML.StudentSlash.half_tie_envelope`**

The vertical collapse limit is the sharp joint half-tie envelope across moving locations. [Lean source](../../lean/UnivariateML/Family/StudentSlash.lean#L517)

```lean
theorem half_tie_envelope (x : Fin n → ℝ) (b : ℝ)
    (he : n = 2 * ties (fun i ↦ x i-b)) :
    Tendsto (fun s ↦ likelihood x b s) (𝓝[>] 0) (𝓝 (collapseValue x b)) ∧
      ∀ B > collapseValue x b, ∀ᶠ p : ℝ × ℝ in (𝓝 b) ×ˢ (𝓝[>] 0),
        likelihood x p.1 p.2 < B
```

**`UnivariateML.StudentSlash.joint_half_tie_failure`**

With a half-tied atom and no majority, strict loss at every finite point is equivalent to failure, with supremum at the largest collapse value. [Lean source](../../lean/UnivariateML/Family/StudentSlash.lean#L528)

```lean
theorem joint_half_tie_failure (x : Fin n → ℝ) (hn : 0 < n)
    (hle : ∀ b, 2 * ties (fun i ↦ x i-b) ≤ n)
    (hh : ∃ b, n = 2 * ties (fun i ↦ x i-b)) :
    let D := univ ×ˢ Ioi (0 : ℝ)
    let F := fun p : ℝ × ℝ ↦ likelihood x p.1 p.2
    ∃ b, n = 2 * ties (fun i ↦ x i-b) ∧
      (∀ z, n = 2 * ties (fun i ↦ x i-z) → collapseValue x z ≤ collapseValue x b) ∧
      ((¬ ∃ p ∈ D, IsMaxOn F D p) ↔
        IsLUB (F '' D) (collapseValue x b) ∧ ∀ p ∈ D, F p < collapseValue x b)
```

