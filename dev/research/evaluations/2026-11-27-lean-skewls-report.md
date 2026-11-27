# Lean likelihood results for five location–scale families

TASK-389. The six modules contain 80 public theorems, including the shared Gaussian results. The family statements use the actual normalized densities. The [atlas fragment](../existence-atlas/lean-map.d/lean-skewls.csv) records their hypotheses, regimes and scope.

## Proof status

All theorems are proved under their stated sample and parameter hypotheses. The EMG theorem `critical_value_criterion` additionally takes three explicit hypotheses on a compactification of the actual shape profile. They are continuity on the closed chart (`hP`), differentiability on its interior (`hd`), and agreement with the interior EMG profile (`hprofile`). That theorem is a conditional stationary-value comparison.

There are no unproved Gaussian assumptions. `SkewLSSN.held_slant` uses the proved `SkewLSGaussian.cdf_strictLogConcave`. `SkewLSEMG.normal_path` uses the proved `SkewLSGaussian.mills`. Both signs of the skew-normal half-normal limit use the proved Gaussian CDF tails and reflection. The axiom check found no `sorryAx` among the 80 declarations.

## Source discrepancies

- **A delegate evaluated at a closed endpoint can overstate the parent likelihood.** Bhattacharjee with held width one on sample `(-1, 1)` has a joint uniform trace bounded by 1/16. The closed uniform delegate gives 1/4 at those observations. The Lean envelope uses one shared endpoint weight and its complement. The skew-normal and EMG fixed-endpoint paths likewise carry a factor one half at a tie. Their moving-endpoint paths recover the full density. [Atlas audit](2026-11-27-atlas-audit-report.md#3-contradictions-and-scope-mismatches) · [Bhattacharjee face analysis](2026-11-25-bhatt-math-report.md#existence-and-the-faces)
- **The original slash paired-block proof drops a factor of four.** The checked proof uses the corrected three-region inequality for the actual density ratio. It gives strict domination only for samples made of opposite-residual pairs with two tied observations per pair. A numerical likelihood margin does not establish free-centre nonattainment for a general half tie. [Slash correction](2026-11-25-slash2-math-report.md#correction-to-the-paired-collapse-proof)
- **The EMG dossier's existence caveat understates the written all-free result.** The written report gives a finite stationary-list comparison and exact real co-winners. The Lean hold-mask comparisons use actual finite witnesses and include equality with an excluded face. The all-free stationary theorem still requires the profile hypotheses above. [Written EMG criterion](2026-11-25-emg-math-report.md#exact-finite-comparison-and-per-input-certificates)
- **The half-t atlas leaves shape-and-scale-held open.** The dossier and written result give the sample minimum as the unique feasible endpoint maximizer. Lean verifies this for any positive held shape and scale. The fitted floor of one applies to free shape in the all-free return domain. [Half-t dossier](../../../univariateML-cpp/include/cppunivariate/distributions/ht/README.md#details) · [Half-t face analysis](2026-11-25-halft-math-report.md#profile-and-the-two-faces)

## Proof effort and remaining claims

| Target | Proof work | Open in Lean |
| --- | --- | --- |
| Gaussian CDF | Identified the CDF with the integrated Mathlib density and differentiated it by the fundamental theorem of calculus. Strict monotonicity follows from positivity of the density. Used the CDF limits and reflection to obtain the half weight. Applied L’Hôpital to the lower tail divided by the Gaussian exponential over its argument. The resulting Mills limit gives the left limit of the function whose derivative is the CDF, and its positivity gives strict log-concavity. | The CDF facts requested for the family proofs are all discharged. |
| Bhattacharjee | When the range exceeds twice the held width, an extreme observation loses its interval mass uniformly at small scale. Gaussian and uniform peak bounds and a location-tail bound confine a likelihood superlevel to a compact rectangle. For a compatible centre, characterized the exact unattained uniform supremum by one compact maximization over complementary endpoint weights. With Gaussian scale held, the normalized Gaussian interval average is continuous through width zero and the joint or held-centre maximum is attained. | The envelope theorem leaves the maximizing weight in variational form. At range equality the closed form comes from maximizing the product of the lower-endpoint weight to its tie count and the complementary weight to the upper-endpoint tie count. Held-width uniqueness and the free-width finite stationary-list results still need Gaussian interval endpoint curvature and profile analysis. |
| Skew normal | Transferred fixed-slant maximization through the normalized log density to the strictly convex Gaussian-plus-log-CDF loss. Checked the fixed-centre half-normal trace and both exterior moving-centre paths. A uniform upper bound along any path to a fixed positive-scale endpoint matches the reachable unpenalized product when the support is compatible. Constant data collapse at every finite slant. | The full all-free stationary-shape comparison still needs nuisance confinement along arbitrary escaping paths and a global analysis of the profile. The fixed-slant transfer is the joint location–scale case specified in the continuation. |
| EMG faces and holds | Derived both fixed-location face paths from the closed-form density and obtained the full translated-exponential value by drifting the location below the sample minimum. Proved the Gaussian peak by comparing weighted CDF derivatives and the exponential peak by splitting on standardized residual size. Strict log-CDF curvature gives a unique location maximum when both rate and scale are held. The other five nontrivial hold masks have exact finite-witness iff statements. Their sufficiency proofs confine strict winners to compact sets and retain equality with excluded faces. For held location, the small-scale and large-rate corner is controlled by exponential and Gaussian tails at a nonzero residual. The zero-residual case has a diverging path. | Cardinality outside the proved location and fixed-dimensionless-shape slices is separate. For the all-free stationary result, still needed are a continuous compactification with the fitted face values, differentiability of the actual interior profile, and finiteness of stationary shapes. The written finiteness argument uses the Pfaffian closure and o-minimality. |
| Slash | Differentiated the normalized log likelihood in log scale, proved score monotonicity from strict exponential secant slopes, and established both score limits. The held-centre result classifies strict sub-half, exact-half and majority ties. Proved the rational bound for the removable density factor and the corrected three-region strict inequality, then multiplied the paired blocks and checked their common vertical limit. | A generic free-centre half tie requires the separate comparison with all finite candidates. The opposite-pair proof does not establish domination for residuals that merely have zero reciprocal sum. |
| Half-t | Transferred the complete positive, zero and negative exponent cases from `TDist`, including all-tied data, with the actual factor of two per observation. Proved endpoint profiling, majority collapse at the fitted floor and the held-endpoint free-shape divergence when there is a tie. Gamma log-convex midpoint bounds give the normalizer limit, and bounds from the exact scale score give the optimized RMS limit. At an exact half tie, the optimizing scale vanishes as shape approaches one and its exponent times log scale tends to zero. Continuity of positive scale roots and both boundary limits give an exact all-free finite-witness criterion. | The finite-witness theorem does not enumerate stationary shapes. The written finite root-list theorem still needs interior profile analyticity and exclusion of accumulating roots at the half-tie and normal ends. The other free-shape hold masks have the partial scope stated in their individual results. |

## Validation

On the workstation, each module passed `lake env lean` individually. The final `lake build` covered the existing `UnivariateML` library and the six explicitly named modules and reported 7,830 jobs with exit status zero. The axiom audit imported the built modules and checked all 80 public declarations. Lake replayed style warnings in existing modules. No new module emitted a warning in the final build.

## Checked Lean statements

### `UnivariateML.SkewLSGaussian.phi_formula`

The standard normal density has the usual normalized exponential formula.

Unconditional auxiliary fact about Mathlib standard Gaussian measure, used across the face and fit results.

```lean
theorem phi_formula (x : ℝ) : φ x = (1 / sqrt (2 * π)) * exp (-(x^2 / 2))
```

### `UnivariateML.SkewLSGaussian.phi_pos`

Every finite point has strictly positive Gaussian density.

Unconditional auxiliary fact about Mathlib standard Gaussian measure, used across the face and fit results.

```lean
theorem phi_pos (x : ℝ) : 0 < φ x
```

### `UnivariateML.SkewLSGaussian.phi_integrable`

The standard normal density is integrable.

Unconditional auxiliary fact about Mathlib standard Gaussian measure, used across the face and fit results.

```lean
theorem phi_integrable : Integrable φ
```

### `UnivariateML.SkewLSGaussian.phi_hasDerivAt`

The Gaussian density has derivative minus its argument times the density.

Unconditional auxiliary fact about Mathlib standard Gaussian measure, used across the face and fit results.

```lean
theorem phi_hasDerivAt (x : ℝ) : HasDerivAt φ (-x * φ x) x
```

### `UnivariateML.SkewLSGaussian.cdf_integral`

The Mathlib CDF is the Gaussian density integral over the left half-line.

Unconditional auxiliary fact about Mathlib standard Gaussian measure, used across the face and fit results.

```lean
theorem cdf_integral (x : ℝ) : Φ x = ∫ t in Iic x, φ t
```

### `UnivariateML.SkewLSGaussian.cdf_hasDerivAt`

The Gaussian CDF has the standard normal density as its derivative.

Unconditional auxiliary fact about Mathlib standard Gaussian measure, used across the face and fit results.

```lean
theorem cdf_hasDerivAt (x : ℝ) : HasDerivAt Φ (φ x) x
```

### `UnivariateML.SkewLSGaussian.cdf_continuous`

The Gaussian CDF is continuous.

Unconditional auxiliary fact about Mathlib standard Gaussian measure, used across the face and fit results.

```lean
theorem cdf_continuous : Continuous Φ
```

### `UnivariateML.SkewLSGaussian.cdf_strictMono`

The Gaussian CDF is strictly increasing.

Unconditional auxiliary fact about Mathlib standard Gaussian measure, used across the face and fit results.

```lean
theorem cdf_strictMono : StrictMono Φ
```

### `UnivariateML.SkewLSGaussian.cdf_pos`

The Gaussian CDF is strictly positive at every finite argument.

Unconditional auxiliary fact about Mathlib standard Gaussian measure, used across the face and fit results.

```lean
theorem cdf_pos (x : ℝ) : 0 < Φ x
```

### `UnivariateML.SkewLSGaussian.cdf_lt_one`

The Gaussian CDF is strictly less than one at every finite argument.

Unconditional auxiliary fact about Mathlib standard Gaussian measure, used across the face and fit results.

```lean
theorem cdf_lt_one (x : ℝ) : Φ x < 1
```

### `UnivariateML.SkewLSGaussian.cdf_atBot`

The Gaussian CDF tends to zero at negative infinity.

Unconditional auxiliary fact about Mathlib standard Gaussian measure, used across the face and fit results.

```lean
theorem cdf_atBot : Tendsto Φ atBot (𝓝 0)
```

### `UnivariateML.SkewLSGaussian.cdf_atTop`

The Gaussian CDF tends to one at positive infinity.

Unconditional auxiliary fact about Mathlib standard Gaussian measure, used across the face and fit results.

```lean
theorem cdf_atTop : Tendsto Φ atTop (𝓝 1)
```

### `UnivariateML.SkewLSGaussian.mills`

The scaled negative Gaussian tail tends to the standard normal density at zero.

Unconditional auxiliary fact about Mathlib standard Gaussian measure, used across the face and fit results.

```lean
theorem mills :
    Tendsto (fun y ↦ y * exp (y ^ 2 / 2) * Φ (-y)) atTop (𝓝 (1 / sqrt (2 * π)))
```

### `UnivariateML.SkewLSGaussian.cdf_reflect`

Reflection takes the Gaussian CDF to its complement.

Unconditional auxiliary fact about Mathlib standard Gaussian measure, used across the face and fit results.

```lean
theorem cdf_reflect (x : ℝ) : Φ (-x) = 1 - Φ x
```

### `UnivariateML.SkewLSGaussian.cdf_zero`

The Gaussian CDF at zero is one half.

Unconditional auxiliary fact about Mathlib standard Gaussian measure, used across the face and fit results.

```lean
theorem cdf_zero : Φ 0 = 1/2
```

### `UnivariateML.SkewLSGaussian.phi_add_mul_cdf_pos`

The Gaussian density plus the argument times the CDF is strictly positive.

Unconditional auxiliary fact about Mathlib standard Gaussian measure, used across the face and fit results.

```lean
theorem phi_add_mul_cdf_pos (x : ℝ) : 0 < φ x + x * Φ x
```

### `UnivariateML.SkewLSGaussian.cdf_strictLogConcave`

The logarithm of the Gaussian CDF is strictly concave.

Unconditional auxiliary fact about Mathlib standard Gaussian measure, used across the face and fit results.

```lean
theorem cdf_strictLogConcave : StrictConcaveOn ℝ univ (fun x ↦ log (Φ x))
```

### `UnivariateML.SkewLSBhatt.strict_cover_uniform`

Strict coverage gives the unattained uniform likelihood envelope.

Strict coverage. The equality case uses coverage_uniform_envelope.

```lean
theorem strict_cover_uniform {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ} {a m : ℝ}
    (ha : 0 < a) (hx : ∀ i, |x i - m| < a) :
    IsLUB (likelihood x a m '' Ioi 0) ((1 / (2 * a)) ^ n) ∧
      ∀ m' s : ℝ, 0 < s → likelihood x a m' s < (1 / (2 * a)) ^ n
```

### `UnivariateML.SkewLSBhatt.held_centre_uniform_path`

At a held centre, every tied endpoint contributes one half to the uniform trace. Every strictly covered observation contributes the full uniform density.

```lean
theorem held_centre_uniform_path {n : ℕ} {x : Fin n → ℝ} {a m : ℝ}
    (ha : 0 < a) (hx : ∀ i, |x i - m| ≤ a) :
    Tendsto (likelihood x a m) (𝓝[>] 0)
      (𝓝 (∏ i, ((if |x i - m| = a then 1/2 else 1) / (2*a))))
```

### `UnivariateML.SkewLSBhatt.held_scale_attainment`

Holding a positive Gaussian scale guarantees attainment, including at the zero-width normal seam.

```lean
theorem held_scale_attainment {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {s : ℝ} (hs : 0 < s) :
    ∃ p ∈ (Ici 0 ×ˢ (univ : Set ℝ)),
      IsMaxOn (fun p : ℝ × ℝ ↦ likelihood x p.1 p.2 s) (Ici 0 ×ˢ univ) p
```

### `UnivariateML.SkewLSBhatt.held_scale_centre_attainment`

With both Gaussian scale and centre held, the maximum over nonnegative width is attained.

```lean
theorem held_scale_centre_attainment {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    (m : ℝ) {s : ℝ} (hs : 0 < s) :
    ∃ a ∈ Ici (0 : ℝ), IsMaxOn (fun w ↦ likelihood x w m s) (Ici 0) a
```

### `UnivariateML.SkewLSBhatt.coverage_uniform_envelope`

At positive held width, any compatible centre gives the uniform envelope by maximizing the joint complementary endpoint trace. No finite fit reaches the envelope.

At range equality the theorem optimizes the joint trace over a shared complementary endpoint weight.

```lean
theorem coverage_uniform_envelope {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ}
    {a c : ℝ} (ha : 0 < a) (hx : ∀ i, |x i - c| ≤ a) :
    ∃ w ∈ Icc (0 : ℝ) 1, IsMaxOn (uniformTrace x a c) (Icc 0 1) w ∧
      IsLUB ((fun p : ℝ × ℝ ↦ likelihood x a p.1 p.2) '' (univ ×ˢ Ioi 0))
        (uniformTrace x a c w) ∧
      ∀ m s : ℝ, 0 < s → likelihood x a m s < uniformTrace x a c w
```

### `UnivariateML.SkewLSBhatt.positive_width_iff`

At held positive width, a finite maximum exists exactly when the sample range exceeds twice that width. The nonattainment direction includes equality.

The range test is written as the existence of an observed pair separated by more than twice the held width.

```lean
theorem positive_width_iff {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) {a : ℝ} (ha : 0 < a) :
    (∃ p ∈ (univ ×ˢ Ioi (0 : ℝ)),
      IsMaxOn (fun p : ℝ × ℝ ↦ likelihood x a p.1 p.2) (univ ×ˢ Ioi 0) p) ↔
      ∃ i j, 2*a < x j - x i
```

### `UnivariateML.SkewLSBhatt.constant_normal_seam`

The attained zero-width seam makes the actual constant-sample likelihood unbounded on a nonempty sample.

```lean
theorem constant_normal_seam {n : ℕ} (hn : 0 < n) (m : ℝ) :
    ¬ BddAbove (likelihood (fun _ : Fin n ↦ m) 0 m '' Ioi 0)
```

### `UnivariateML.SkewLSSN.held_slant`

At fixed finite slant, every nonconstant sample has exactly one location–scale MLE for the Mathlib Gaussian skew density.

```lean
theorem held_slant {n : ℕ} {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x) (α : ℝ) :
    ∃! p, p ∈ LocationScale.domain ∧ IsMaxOn (logLik α x) LocationScale.domain p
```

### `UnivariateML.SkewLSSN.constant_collapse`

A constant sample has unbounded log likelihood at every fixed finite slant.

```lean
theorem constant_collapse (α : ℝ) {n : ℕ} (hn : 0 < n) (m : ℝ) :
    ¬ BddAbove (logLik α (fun _ : Fin n ↦ m) '' LocationScale.domain)
```

### `UnivariateML.SkewLSSN.half_normal_path`

With location and scale held, the right half-normal limit retains a factor one half at each observation exactly at the location.

```lean
theorem half_normal_path {n : ℕ} (x : Fin n → ℝ) (ξ : ℝ) {ω : ℝ} (hω : 0 < ω) :
    Tendsto (fun α : ℝ ↦ ∏ i, density α ξ ω (x i)) atTop
      (𝓝 (∏ i, (2 * φ ((x i - ξ)/ω) / ω) *
        (if ξ < x i then 1 else if x i < ξ then 0 else 1/2)))
```

### `UnivariateML.SkewLSSN.right_moving_half_normal`

A right half-normal face at an endpoint at or below the sample is reachable without endpoint penalties by moving the location from outside.

```lean
theorem right_moving_half_normal {n : ℕ} (x : Fin n → ℝ) {c ω : ℝ}
    (hω : 0 < ω) (hx : ∀ i, c ≤ x i) :
    Tendsto (fun t : ℝ ↦ ∏ i, density (t^2) (c-t⁻¹) ω (x i)) atTop
      (𝓝 (halfNormalEnvelope x c ω))
```

### `UnivariateML.SkewLSSN.left_moving_half_normal`

A left half-normal face is reachable from outside the sample by the reflected path.

```lean
theorem left_moving_half_normal {n : ℕ} (x : Fin n → ℝ) {c ω : ℝ}
    (hω : 0 < ω) (hx : ∀ i, x i ≤ c) :
    Tendsto (fun t : ℝ ↦ ∏ i, density (-(t^2)) (c+t⁻¹) ω (x i)) atTop
      (𝓝 (halfNormalEnvelope x c ω))
```

### `UnivariateML.SkewLSSN.half_normal_envelope_upper`

Along any feasible location–scale path converging to a positive-scale endpoint, the unpenalized half-normal product bounds the limiting likelihood from above, uniformly over slants.

Local upper bound along converging location and positive scale. Global profile confinement is separate.

```lean
theorem half_normal_envelope_upper {n : ℕ} (x : Fin n → ℝ) {c ω : ℝ} (hω : 0 < ω)
    {ι : Type*} {l : Filter ι} {p : ι → ℝ × ℝ} (hp : Tendsto p l (𝓝 (c, ω)))
    (α : ι → ℝ) {b : ℝ} (hb : halfNormalEnvelope x c ω < b) :
    ∀ᶠ i in l, (∏ j, density (α i) (p i).1 (p i).2 (x j)) < b
```

### `UnivariateML.SkewLSEMG.exponential_path`

With rate and location held, the scale-zero path has half the translated-exponential density at the endpoint.

```lean
theorem exponential_path (rate μ x : ℝ) :
    Tendsto (fun σ ↦ density rate μ σ x) (𝓝[>] 0)
      (𝓝 (rate * exp (-rate * (x - μ)) *
        (if μ < x then 1 else if x < μ then 0 else 1/2)))
```

### `UnivariateML.SkewLSEMG.normal_path`

With Gaussian location and positive scale held, infinite rate gives the normal density at that location and scale.

```lean
theorem normal_path (μ x : ℝ) {σ : ℝ} (hσ : 0 < σ) :
    Tendsto (fun rate ↦ density rate μ σ x) atTop (𝓝 (φ ((x - μ)/σ) / σ))
```

### `UnivariateML.SkewLSEMG.held_rate_scale`

A positive held rate and normal scale leave a unique finite location fit on every nonempty sample.

```lean
theorem held_rate_scale {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {rate σ : ℝ} (hr : 0 < rate) (hσ : 0 < σ) :
    ∃! μ, IsMaxOn (locationLogLik x rate σ) univ μ
```

### `UnivariateML.SkewLSEMG.normal_dominates_at_held_location`

With location held weakly above an observation, every finite rate gives a smaller density than the corresponding fixed-scale normal limit.

```lean
theorem normal_dominates_at_held_location {rate σ μ x : ℝ}
    (hr : 0 < rate) (hσ : 0 < σ) (hx : x ≤ μ) :
    density rate μ σ x < φ ((x-μ)/σ) / σ
```

### `UnivariateML.SkewLSEMG.held_location_scale_normal_supremum`

With location at or above all observations and scale fixed, the normal likelihood is the unattained rate-profile supremum.

```lean
theorem held_location_scale_normal_supremum {n : ℕ} (hn : 0 < n)
    (x : Fin n → ℝ) {μ σ : ℝ} (hσ : 0 < σ) (hx : ∀ i, x i ≤ μ) :
    IsLUB ((fun rate : ℝ ↦ ∏ i, density rate μ σ (x i)) '' Ioi 0)
      (∏ i, φ ((x i-μ)/σ)/σ) ∧
    ∀ rate > 0, (∏ i, density rate μ σ (x i)) < ∏ i, φ ((x i-μ)/σ)/σ
```

### `UnivariateML.SkewLSEMG.held_location_rate_at_sample`

At a constant sample's held location, with positive rate held, the density supremum over scale is half the rate and is unattained.

```lean
theorem held_location_rate_at_sample {rate : ℝ} (hr : 0 < rate) (m : ℝ) :
    IsLUB ((fun σ ↦ density rate m σ m) '' Ioi 0) (rate/2) ∧
      ∀ σ > 0, density rate m σ m < rate/2
```

### `UnivariateML.SkewLSEMG.held_rate_at_sample`

For constant data and held positive rate, varying location and scale approaches the exponential peak, which no finite fit reaches.

```lean
theorem held_rate_at_sample {rate : ℝ} (hr : 0 < rate) (m : ℝ) :
    IsLUB ((fun p : ℝ × ℝ ↦ density rate p.1 p.2 m) '' (univ ×ˢ Ioi 0)) rate ∧
      ∀ μ σ : ℝ, 0 < σ → density rate μ σ m < rate
```

### `UnivariateML.SkewLSEMG.constant_collapse`

At a constant observation a fixed dimensionless shape and shrinking Gaussian scale yield an unbounded density. The path holds location fixed.

```lean
theorem constant_collapse (m : ℝ) :
    Tendsto (fun σ ↦ density σ⁻¹ m σ m) (𝓝[>] 0) atTop
```

### `UnivariateML.SkewLSEMG.constant_sample_unbounded`

At the held common observation, the all-free rate and scale path makes the likelihood unbounded for every nonempty constant sample.

```lean
theorem constant_sample_unbounded {n : ℕ} (hn : 0 < n) (m : ℝ) :
    Tendsto (fun σ ↦ ∏ _ : Fin n, density σ⁻¹ m σ m) (𝓝[>] 0) atTop
```

### `UnivariateML.SkewLSEMG.gaussian_peak_strict`

The Gaussian peak strictly bounds every finite EMG density at a held Gaussian scale.

```lean
theorem gaussian_peak_strict {rate μ σ x : ℝ} (hr : 0 < rate) (hσ : 0 < σ) :
    density rate μ σ x < φ 0 / σ
```

### `UnivariateML.SkewLSEMG.held_scale_at_sample`

For a constant sample and held Gaussian scale, the normal peak is the unattained supremum over location and rate, stated per observation.

```lean
theorem held_scale_at_sample (m : ℝ) {σ : ℝ} (hσ : 0 < σ) :
    IsLUB ((fun p : ℝ × ℝ ↦ density p.2 p.1 σ m) '' (univ ×ˢ Ioi 0)) (φ 0/σ) ∧
    ∀ μ rate : ℝ, 0 < rate → density rate μ σ m < φ 0/σ
```

### `UnivariateML.SkewLSEMG.held_location_above_no_fit`

When every observation is at or below a held location, raising the rate toward the normal face improves on any finite rate, even if scale is free.

```lean
theorem held_location_above_no_fit {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    (μ : ℝ) (hx : ∀ i, x i ≤ μ) :
    ¬ ∃ p ∈ (Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ)),
      IsMaxOn (fun p : ℝ × ℝ ↦ ∏ i, density p.1 μ p.2 (x i)) (Ioi 0 ×ˢ Ioi 0) p
```

### `UnivariateML.SkewLSEMG.held_rate_location_criterion`

With rate and location held, Gaussian scale is attained exactly when some finite positive scale reaches the exponential face trace.

Scalar face comparison includes the exact half weight at a held endpoint.

```lean
theorem held_rate_location_criterion {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {rate μ : ℝ} (hr : 0 < rate) :
    (∃ σ ∈ Ioi (0 : ℝ), IsMaxOn (fun s ↦ ∏ i, density rate μ s (x i)) (Ioi 0) σ) ↔
      ∃ σ > 0, exponentialTrace x rate μ ≤ ∏ i, density rate μ σ (x i)
```

### `UnivariateML.SkewLSEMG.held_rate_location_below_attainment`

If the held rate is positive and the held location is strictly below the sample, an interior scale exceeds the exponential face and a maximum exists.

```lean
theorem held_rate_location_below_attainment {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {rate μ : ℝ} (hr : 0 < rate) (hx : ∀ i, μ < x i) :
    ∃ σ ∈ Ioi (0 : ℝ), IsMaxOn (fun s ↦ ∏ i, density rate μ s (x i)) (Ioi 0) σ
```

### `UnivariateML.SkewLSEMG.held_rate_location_above_observation`

If an observation lies to the left of the held location, the exponential face vanishes and a positive scale attains the maximum.

```lean
theorem held_rate_location_above_observation {n : ℕ} (x : Fin n → ℝ)
    {rate μ : ℝ} (hr : 0 < rate) {i : Fin n} (hi : x i < μ) :
    ∃ σ ∈ Ioi (0 : ℝ), IsMaxOn (fun s ↦ ∏ j, density rate μ s (x j)) (Ioi 0) σ
```

### `UnivariateML.SkewLSEMG.held_location_scale_criterion`

With location and Gaussian scale held, a finite rate attains the maximum exactly when it reaches the fixed normal face value.

```lean
theorem held_location_scale_criterion {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {μ σ : ℝ} (hσ : 0 < σ) :
    (∃ rate ∈ Ioi (0 : ℝ), IsMaxOn (fun r ↦ ∏ i, density r μ σ (x i)) (Ioi 0) rate) ↔
      ∃ rate > 0, (∏ i, φ ((x i-μ)/σ)/σ) ≤ ∏ i, density rate μ σ (x i)
```

### `UnivariateML.SkewLSEMG.held_scale_criterion`

With Gaussian scale held, a finite location and rate fit exists exactly when some finite point reaches the optimized Gaussian face.

Exact finite-witness criterion including equality with the optimized Gaussian face.

```lean
theorem held_scale_criterion {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {σ : ℝ} (hσ : 0 < σ) :
    (∃ p ∈ (univ ×ˢ Ioi (0 : ℝ)),
      IsMaxOn (fun p : ℝ × ℝ ↦ ∏ i, density p.2 p.1 σ (x i)) (univ ×ˢ Ioi 0) p) ↔
      ∃ μ rate : ℝ, 0 < rate ∧
        sSup (range (fun m ↦ normalTrace x m σ)) ≤ ∏ i, density rate μ σ (x i)
```

### `UnivariateML.SkewLSEMG.moving_exponential_face`

At held positive rate and free location and scale, the translated exponential face is attained as a limit at the sample minimum.

```lean
theorem moving_exponential_face {n : ℕ} (x : Fin n → ℝ) {c : ℝ}
    (hc : ∀ i, c ≤ x i) (rate : ℝ) :
    Tendsto (fun t : ℝ ↦ ∏ i, density rate (c-t⁻¹) ((t⁻¹)^2) (x i)) atTop
      (𝓝 (∏ i, rate*exp (-rate*(x i-c))))
```

### `UnivariateML.SkewLSEMG.held_rate_criterion`

At held positive rate, a finite fit exists exactly when a finite location and scale reach the optimized translated-exponential face.

Exact finite-witness criterion including equality with the translated-exponential face at the sample minimum.

```lean
theorem held_rate_criterion {n : ℕ} (x : Fin n → ℝ) {i : Fin n}
    (hi : ∀ j, x i ≤ x j) {rate : ℝ} (hr : 0 < rate) :
    (∃ p ∈ (univ ×ˢ Ioi (0 : ℝ)),
      IsMaxOn (fun p : ℝ × ℝ ↦ ∏ j, density rate p.1 p.2 (x j)) (univ ×ˢ Ioi 0) p) ↔
      ∃ μ s : ℝ, 0 < s ∧ (∏ j, rate*exp (-rate*(x j-x i))) ≤
        ∏ j, density rate μ s (x j)
```

### `UnivariateML.SkewLSEMG.held_location_criterion`

For a held location with a nonzero residual, finite attainment is equivalent to a finite point reaching both optimized conditional faces.

At least one residual is nonzero. The all-zero case follows from constant_sample_unbounded at the held centre.

```lean
theorem held_location_criterion {n : ℕ} (x : Fin n → ℝ) (μ : ℝ)
    {i : Fin n} (hi : x i ≠ μ) :
    (∃ p ∈ (Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ)),
      IsMaxOn (fun p : ℝ × ℝ ↦ ∏ j, density p.1 μ p.2 (x j)) (Ioi 0 ×ˢ Ioi 0) p) ↔
      ∃ rate s : ℝ, 0 < rate ∧ 0 < s ∧
        max (sSup (normalTrace x μ '' Ioi 0))
          (sSup ((fun r ↦ exponentialTrace x r μ) '' Ioi 0)) ≤
        ∏ j, density rate μ s (x j)
```

### `UnivariateML.SkewLSEMG.fixed_shape_fit`

For a nonconstant sample every fixed positive dimensionless shape has a unique finite location and scale optimizer.

This holds dimensionless shape, not either physical rate or physical Gaussian scale.

```lean
theorem fixed_shape_fit {n : ℕ} {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x)
    {k : ℝ} (hk : 0 < k) :
    ∃! p, p ∈ LocationScale.domain ∧ IsMaxOn (shapeLogLik x k) LocationScale.domain p
```

### `UnivariateML.SkewLSEMG.critical_value_criterion`

Given a differentiable continuous compactification of the actual EMG profile, an interior stationary value reaching both endpoints is equivalent to a joint finite maximizer. The endpoints may tie the finite fit.

Requires a named hypothesis hprofile identifying the actual profiled likelihood, plus continuity of P on the closed chart and differentiability on its interior. It does not establish face identification for this joint profile or finiteness of stationary shapes.

```lean
theorem critical_value_criterion {n : ℕ} {x : Fin n → ℝ}
    (hx : LocationScale.Nonconstant x) {P : ℝ → ℝ}
    (hP : ContinuousOn P (Icc 0 1)) (hd : DifferentiableOn ℝ P (Ioo 0 1))
    (hprofile : EqOn P (interiorProfile x) (Ioo 0 1)) :
    (∃ t ∈ Ioo 0 1, HasDerivAt P 0 t ∧ max (P 0) (P 1) ≤ P t) ↔
      ∃ q ∈ (Ioo 0 1 ×ˢ LocationScale.domain),
        IsMaxOn (fun p : ℝ × (ℝ × ℝ) ↦ shapeLogLik x ((1-p.1)/p.1) p.2)
          (Ioo 0 1 ×ˢ LocationScale.domain) q
```

### `UnivariateML.SkewLSSlash.scale_score_strictAnti`

The held-location log-scale score strictly decreases unless all residuals vanish.

```lean
theorem scale_score_strictAnti {n : ℕ} {r : Fin n → ℝ} (hr : ∃ i, r i ≠ 0) :
    StrictAntiOn (scaleScore r) (Ioi 0)
```

### `UnivariateML.SkewLSSlash.log_scale_derivative`

The scale score is the derivative in log scale of the actual likelihood.

```lean
theorem log_scale_derivative {n : ℕ} (r : Fin n → ℝ) (t : ℝ) :
    HasDerivAt (fun y ↦ logLik r (exp y)) (scaleScore r (exp t)) t
```

### `UnivariateML.SkewLSSlash.log_scale_score_limits`

The log-scale score tends to the collapse exponent and to minus the sample size at the two ends of the log-scale line.

```lean
theorem log_scale_score_limits {n : ℕ} (r : Fin n → ℝ) :
    Tendsto (fun t ↦ scaleScore r (exp t)) atBot (𝓝 ((n : ℝ) - 2 * ties r)) ∧
      Tendsto (fun t ↦ scaleScore r (exp t)) atTop (𝓝 (-(n : ℝ)))
```

### `UnivariateML.SkewLSSlash.held_centre_scale_iff`

At held centre, the unique scale maximizer exists exactly when fewer than half the observations are tied there.

```lean
theorem held_centre_scale_iff {n : ℕ} (hn : 0 < n) (r : Fin n → ℝ) :
    (∃ s ∈ Ioi (0 : ℝ), IsMaxOn (logLik r) (Ioi 0) s) ↔ 2 * ties r < n
```

### `UnivariateML.SkewLSSlash.held_centre_scale_unique`

In the attained regime the maximizing scale is unique and is exactly the positive zero of the scale score.

```lean
theorem held_centre_scale_unique {n : ℕ} (hn : 0 < n) (r : Fin n → ℝ)
    (he : 2 * ties r < n) :
    (∃! s, s ∈ Ioi (0 : ℝ) ∧ IsMaxOn (logLik r) (Ioi 0) s) ∧
      ∀ s > 0, IsMaxOn (logLik r) (Ioi 0) s ↔ scaleScore r s = 0
```

### `UnivariateML.SkewLSSlash.held_centre_unbounded`

A strict majority at the held location sends the log likelihood to positive infinity as scale tends to zero.

```lean
theorem held_centre_unbounded {n : ℕ} (r : Fin n → ℝ) (he : n < 2 * ties r) :
    Tendsto (logLik r) (𝓝[>] 0) atTop
```

### `UnivariateML.SkewLSSlash.held_centre_half_tie`

A half tie at the held location gives the finite, unattained scale-zero log-likelihood supremum.

```lean
theorem held_centre_half_tie {n : ℕ} (hn : 0 < n) (r : Fin n → ℝ) (he : n = 2 * ties r) :
    Tendsto (logLik r) (𝓝[>] 0) (𝓝 (collapseLog r)) ∧
      IsLUB (logLik r '' Ioi 0) (collapseLog r) ∧
      ∀ s > 0, logLik r s < collapseLog r
```

### `UnivariateML.SkewLSSlash.half_tie_path`

The vertical half-tie path pairs each tied observation with one nonzero residual and converges to the product of the cancelled factors.

```lean
theorem half_tie_path {k : ℕ} (d : Fin k → ℝ) (hd : ∀ i, d i ≠ 0) :
    Tendsto (fun s ↦ ∏ i, density 0 s * density (d i) s) (𝓝[>] 0)
      (𝓝 (∏ i, (1 / sqrt (2 * π))^2 / (2*(d i)^2)))
```

### `UnivariateML.SkewLSSlash.half_tie_strict`

On the held-centre half-tie slice every positive scale is strictly below the vertical trace, stated using the normalized likelihood.

```lean
theorem half_tie_strict {k : ℕ} (hk : 0 < k) (d : Fin k → ℝ)
    (hd : ∀ i, d i ≠ 0) {s : ℝ} (hs : 0 < s) :
    (∏ i, density 0 s * density (d i) s) <
      ∏ i, (1 / sqrt (2 * π))^2 / (2*(d i)^2)
```

### `UnivariateML.SkewLSSlash.paired_block_lt_collapse`

The corrected opposite-pair inequality holds at every finite centre and positive scale.

```lean
theorem paired_block_lt_collapse {d s : ℝ} (hd : 0 < d) (hs : 0 < s) (m : ℝ) :
    pairedBlock d m s < pairedTrace d
```

### `UnivariateML.SkewLSSlash.paired_collapse_supremum`

If every off-point residual is paired with its opposite and each such pair has two tied observations, the vertical limit is the strict global supremum over all centres and positive scales.

Exact opposite-pair blocks with two tied observations per pair. The proof uses the corrected three-region bound.

```lean
theorem paired_collapse_supremum {k : ℕ} (hk : 0 < k) (d : Fin k → ℝ) (hd : ∀ i, 0 < d i) :
    IsLUB ((fun p : ℝ × ℝ ↦ ∏ i, pairedBlock (d i) p.1 p.2) '' (univ ×ˢ Ioi 0))
      (∏ i, pairedTrace (d i)) ∧
    ∀ m s : ℝ, 0 < s → (∏ i, pairedBlock (d i) m s) < ∏ i, pairedTrace (d i)
```

### `UnivariateML.SkewLSHalfT.endpoint_profile`

Every feasible location below the sample minimum is strictly worse at the same positive scale and held positive shape.

```lean
theorem endpoint_profile {n : ℕ} (x : Fin n → ℝ) {i : Fin n} {ν s : ℝ}
    (hν : 0 < ν) (hs : 0 < s) (hm : ∀ j, x i ≤ x j) :
    ∃! μ, μ ∈ {m | ∀ j, m ≤ x j} ∧
      IsMaxOn (fun m ↦ logLik x m ν s) {m | ∀ j, m ≤ x j} μ
```

### `UnivariateML.SkewLSHalfT.held_endpoint`

The held-endpoint scale maximum exists exactly at positive collapse exponent and is then unique. Support compatibility is imposed separately.

The scale slice is the half-Student likelihood at a compatible held endpoint. The theorem includes all-tied nonempty samples.

```lean
theorem held_endpoint {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) (μ : ℝ) {ν : ℝ}
    (hν : 0 < ν) :
    (∃! s, s ∈ Ioi 0 ∧ IsMaxOn (logLik x μ ν) (Ioi 0) s) ↔
      0 < TDist.exponent x μ ν
```

### `UnivariateML.SkewLSHalfT.held_shape`

With free endpoint and held shape, the exact existence criterion uses the multiplicity at the sample minimum. Every attained fit is unique.

```lean
theorem held_shape {n : ℕ} (x : Fin n → ℝ) {i : Fin n} {ν : ℝ}
    (hν : 0 < ν) (hm : ∀ j, x i ≤ x j) :
    (∃! p : ℝ × ℝ, p ∈ {p | (∀ j, p.1 ≤ x j) ∧ 0 < p.2} ∧
      IsMaxOn (fun p ↦ logLik x p.1 ν p.2)
        {p | (∀ j, p.1 ≤ x j) ∧ 0 < p.2} p) ↔
      0 < TDist.exponent x (x i) ν
```

### `UnivariateML.SkewLSHalfT.held_endpoint_equality`

At critical equality, the half-Student likelihood has a finite unattained supremum along the positive-scale slice.

```lean
theorem held_endpoint_equality {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) (μ : ℝ) {ν : ℝ}
    (hν : 0 < ν) (he : TDist.exponent x μ ν = 0) :
    Tendsto (likelihood x μ ν) (𝓝[>] 0) (𝓝 (collapseValue x μ ν)) ∧
      IsLUB (likelihood x μ ν '' Ioi 0) (collapseValue x μ ν) ∧
      ∀ a > 0, likelihood x μ ν a < collapseValue x μ ν
```

### `UnivariateML.SkewLSHalfT.held_shape_equality`

At critical equality, the same finite unattained supremum holds with the endpoint free on its compatible domain.

```lean
theorem held_shape_equality {n : ℕ} (x : Fin n → ℝ) {i : Fin n} {ν : ℝ}
    (hν : 0 < ν) (hm : ∀ j, x i ≤ x j) (he : TDist.exponent x (x i) ν = 0) :
    IsLUB ((fun p : ℝ × ℝ ↦ likelihood x p.1 ν p.2) ''
      {p | (∀ j, p.1 ≤ x j) ∧ 0 < p.2}) (collapseValue x (x i) ν) ∧
      ∀ μ s : ℝ, (∀ j, μ ≤ x j) → 0 < s →
        likelihood x μ ν s < collapseValue x (x i) ν
```

### `UnivariateML.SkewLSHalfT.held_endpoint_collapse`

At negative exponent, the normalized likelihood diverges as scale tends to zero.

```lean
theorem held_endpoint_collapse {n : ℕ} (x : Fin n → ℝ) (μ : ℝ) {ν : ℝ}
    (hν : 0 < ν) (he : TDist.exponent x μ ν < 0) :
    Tendsto (likelihood x μ ν) (𝓝[>] 0) atTop
```

### `UnivariateML.SkewLSHalfT.held_endpoint_free_shape_unbounded`

With a compatible held endpoint and any endpoint tie, there is a positive shape at which the scale-collapse path diverges.

```lean
theorem held_endpoint_free_shape_unbounded {n : ℕ} (x : Fin n → ℝ) (μ : ℝ)
    (htie : 0 < TDist.multiplicity x μ) :
    ∃ ν > 0, Tendsto (likelihood x μ ν) (𝓝[>] 0) atTop
```

### `UnivariateML.SkewLSHalfT.majority_at_floor`

At the fitted shape floor one, a strict majority at the sample minimum makes the all-free likelihood unbounded.

```lean
theorem majority_at_floor {n : ℕ} (x : Fin n → ℝ) {i : Fin n}
    (h : n < 2 * TDist.multiplicity x (x i)) :
    Tendsto (likelihood x (x i) 1) (𝓝[>] 0) atTop
```

### `UnivariateML.SkewLSHalfT.eligible_shape_profile`

In the fitted all-free shape range, a sub-half endpoint tie has an attained scale profile at the floor and at every larger shape. An exact half tie has an attained profile at every shape strictly above the floor.

```lean
theorem eligible_shape_profile {n : ℕ} (x : Fin n → ℝ) {i : Fin n}
    (hm : ∀ j, x i ≤ x j) {ν : ℝ} (hν : 1 ≤ ν)
    (hcount : 2 * TDist.multiplicity x (x i) ≤ n)
    (hstrict : 1 < ν ∨ 2 * TDist.multiplicity x (x i) < n) :
    ∃! p : ℝ × ℝ, p ∈ {p | (∀ j, p.1 ≤ x j) ∧ 0 < p.2} ∧
      IsMaxOn (fun p ↦ logLik x p.1 ν p.2)
        {p | (∀ j, p.1 ≤ x j) ∧ 0 < p.2} p
```

### `UnivariateML.SkewLSHalfT.half_normal_path`

At fixed compatible endpoint and positive scale, the half-Student likelihood approaches the translated half-normal likelihood as shape tends to infinity.

Compatible held endpoint and fixed positive scale. The optimized limit is profiled_half_normal_limit.

```lean
theorem half_normal_path {n : ℕ} (x : Fin n → ℝ) (μ : ℝ)
    {s : ℝ} (hs : 0 < s) :
    Tendsto (fun ν ↦ likelihood x μ ν s) atTop
      (𝓝 (∏ i, 2*φ ((x i-μ)/s)/s))
```

### `UnivariateML.SkewLSHalfT.scale_root_normal_limit`

Any eventually positive sequence of exact held-shape scale roots tends to the Gaussian RMS scale as the degrees of freedom tend to infinity.

```lean
theorem scale_root_normal_limit {n : ℕ} (x : Fin n → ℝ) (μ : ℝ)
    (hx : ∃ i, x i ≠ μ) {s : ℝ → ℝ}
    (hroot : ∀ᶠ ν in atTop, 0 < s ν ∧ TDist.scaleScore x μ ν (s ν) = 0) :
    Tendsto s atTop (𝓝 (rmsScale x μ))
```

### `UnivariateML.SkewLSHalfT.profiled_half_normal_limit`

At a compatible endpoint with a nonzero residual, the optimized scale profile reaches the half-normal value at the endpoint RMS scale.

```lean
theorem profiled_half_normal_limit {n : ℕ} (x : Fin n → ℝ) (μ : ℝ)
    (hx : ∃ i, x i ≠ μ) :
    Tendsto (scaleProfile x μ) atTop
      (𝓝 (∏ i, 2*φ ((x i-μ)/rmsScale x μ)/rmsScale x μ))
```

### `UnivariateML.SkewLSHalfT.profiled_half_tie_limit`

At an exact half tie, the optimized scale profile approaches the Cauchy-floor scale-zero value as shape decreases to one from above.

```lean
theorem profiled_half_tie_limit {n : ℕ} (x : Fin n → ℝ) (μ : ℝ)
    (hx : ∃ i, x i ≠ μ) (he : TDist.exponent x μ 1 = 0) :
    Tendsto (scaleProfile x μ) (𝓝[>] (1 : ℝ)) (𝓝 (collapseValue x μ 1))
```

### `UnivariateML.SkewLSHalfT.free_endpoint_profile_upperBounds`

At any positive shape the free-endpoint profile has the same upper bounds as the scale profile at the sample minimum.

```lean
theorem free_endpoint_profile_upperBounds {n : ℕ} (x : Fin n → ℝ)
    {i : Fin n} {ν : ℝ} (hν : 0 < ν) (hm : ∀ j, x i ≤ x j) :
    upperBounds ((fun p : ℝ × ℝ ↦ likelihood x p.1 ν p.2) ''
      {p | (∀ j, p.1 ≤ x j) ∧ 0 < p.2}) =
      upperBounds (likelihood x (x i) ν '' Ioi 0)
```

### `UnivariateML.SkewLSHalfT.joint_majority_unbounded`

In the fitted all-free domain, a strict majority at the sample minimum gives an unbounded likelihood.

```lean
theorem joint_majority_unbounded {n : ℕ} (x : Fin n → ℝ) {i : Fin n}
    (hm : ∀ j, x i ≤ x j) (h : n < 2 * TDist.multiplicity x (x i)) :
    ¬ BddAbove ((fun p : ℝ × (ℝ × ℝ) ↦ likelihood x p.1 p.2.1 p.2.2) ''
      {p | (∀ j, p.1 ≤ x j) ∧ 1 ≤ p.2.1 ∧ 0 < p.2.2})
```

### `UnivariateML.SkewLSHalfT.scale_profile_continuous`

The exact positive-scale profile is continuous at every positive shape with positive collapse exponent.

```lean
theorem scale_profile_continuous {n : ℕ} (x : Fin n → ℝ) (μ : ℝ)
    (hx : ∃ i, x i ≠ μ) {ν₀ : ℝ}
    (hν₀ : 0 < ν₀) (he₀ : 0 < TDist.exponent x μ ν₀) :
    ContinuousAt (scaleProfile x μ) ν₀
```

### `UnivariateML.SkewLSHalfT.joint_attainment_criterion`

Below or at a half tie, a finite fit in the all-free domain exists exactly when an eligible finite shape reaches both excluded face values.

Nonmajority minimum ties and fitted shape at least one. The finite-witness comparison does not assert stationary-list finiteness.

```lean
theorem joint_attainment_criterion {n : ℕ} (x : Fin n → ℝ) {i : Fin n}
    (hm : ∀ j, x i ≤ x j) (hcount : 2 * TDist.multiplicity x (x i) ≤ n) :
    (∃ p ∈ {p : ℝ × (ℝ × ℝ) | (∀ j, p.1 ≤ x j) ∧ 1 ≤ p.2.1 ∧ 0 < p.2.2},
      IsMaxOn (fun p : ℝ × (ℝ × ℝ) ↦ likelihood x p.1 p.2.1 p.2.2)
        {p | (∀ j, p.1 ≤ x j) ∧ 1 ≤ p.2.1 ∧ 0 < p.2.2} p) ↔
      ∃ ν ≥ (1 : ℝ), 0 < TDist.exponent x (x i) ν ∧
        max (normalFace x (x i)) (collapseFace x (x i)) ≤ scaleProfile x (x i) ν
```
