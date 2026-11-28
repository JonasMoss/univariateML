# GB2-tree Lean evaluation

TASK-497 covers the criterion and failure claims for eight hold masks in transformed gamma, inverse transformed gamma, GB2 and Feller–Pareto. This pass establishes both uniform power-end likelihood bounds for transformed gamma and its inverse, an exact feasible-witness attainment comparison for each, and shared GB2 peak, mode and tail-rate bounds. It also establishes the two moving held-shape gamma traces, the two full cutoff paths and their Feller–Pareto transfers. The atlas criterion and failure rows retain open obligations at their stated scope. The new atlas fragment leaves every coverage field empty for independent review.

## What the bounds establish

- The transformed-gamma reduction uses the Gamma fit on the powered sample. The conditional score-gap root gives the shape, and the scale has a closed form. For small power the sample gap divided by squared power tends to half the empirical log variance. The generalized-gamma continuity theorem at the normal seam supplies the real-shape normalization limit. The inequality `log u ≤ u - 1` then gives a uniform upper bound after maximizing over shape. At large power, the largest powered observation bounds the sample gap and the Gamma recurrence gives the limit of shape times score gap. Neither argument needs a unique sample extremum.
- The transformed-gamma witness criterion compares the realized power and lognormal face heights against all positive finite parameters. A strictly higher witness confines the power search to a closed bounded interval on which the conditional profile is continuous. If a witness has exactly the face height and no strictly higher witness exists, it already maximizes. Reciprocation transfers this argument with the log Jacobian and reflects the upper endpoint into the Pareto lower endpoint.
- The shared log-concave bound integrates over the portion of the chord on which the log density stays within one unit of the modal height. The resulting inequality has an additional factor of `exp 1` compared with the written full-chord inequality. It suffices for uniform peak and mode bounds because `log H / H` vanishes as the peak `H` diverges. GB2 supplies normalization by the sigmoid substitution into the beta integral. Integrating pointwise exponential lower bounds on both half-lines gives the harmonic rate bound. The tail-rate lower bounds exclude power tending to zero when either beta shape is fixed.
- For a fixed lower beta shape, the Gamma ratio and the limit of `log (1 + u) / u` yield the transformed-gamma log likelihood even when the mode and power vary along the path. At large power, taking the upper shape to be rate divided by power and the log scale below the limiting cutoff by log power divided by power sends the softplus penalty to zero at every tied cutoff. Exact reflection supplies both opposite-orientation statements. The cutoff path statements allow every positive rate, including the optimized reciprocal mean log gap.
- Feller–Pareto on strictly positive excesses identifies the finite objective with GB2 parameter by parameter. This transfers both cutoff paths with the physical excess Jacobian. Constants diverge along a path that keeps both beta shapes fixed, so that path is feasible under all three threshold-held masks. The source claims here concern positive excesses, so observations exactly equal to the held threshold are outside these statements. A tied power or Pareto contact in log excesses is permitted throughout.

## Open atlas obligations and effort

| Family and hold mask | Criterion | Failure |
|---|---|---|
| `trgamma`, all free | **Open at atlas scope.** Proved the joined nonconstant equivalence against a finite feasible witness, including equality, and positive-constant unboundedness. Formalize the differentiated power profile and strict exterior score signs, including the fourth-order zero-skew case and the sixth-order balanced two-level case, then use analyticity to prove that the stationary candidate list is finite. | **Open.** Proved the uniform upper envelopes at both ends of the power axis and reused their realizing paths. To classify every maximizing parameter escape, show nuisance superlevels are compact when power is confined to a positive compact interval, and characterize asymptotically optimal shape and scale along the two ends. |
| `invtrgamma`, all free | **Open at atlas scope.** Transferred the full feasible-witness equivalence and constant branch with the reciprocal Jacobian. The finite stationary list must be transported once the score-sign proof is available. | **Open.** Transferred both uniform envelopes, retaining ties in the fitted Pareto comparison. The existing inverse power path realizes the full Pareto contact. Still needed are nuisance compactness and the necessary trace along arbitrary escaping shape and scale sequences, including the common exponential-loss penalty for repeated minimum observations. |
| `gb2`, `shape1` | **Open.** Established reflection, tail rates bounded away from zero, the moving inverse transformed-gamma path and full power cutoff path. Need the upper bounds in the reflected rate chart and a compact stationary-height comparison against both optimized faces. | **Open.** The lower bound on the held-shape tail rate excludes small power. At large power, still show the free lower shape tends to zero, its product with power tends to a positive finite rate along a subsequence, and the reflected softplus penalty controls arbitrary cutoff contacts. The constructed path realizes the unpenalized trace. Necessity of concentration for arbitrary diverging constant-data paths also remains open. |
| `gb2`, `shape3` | **Open.** Established the corresponding forward rate bound, moving transformed-gamma path and full Pareto cutoff path. Need the uniform exteriors to justify comparison with the two optimized face heights, including finite equality. | **Open.** For large power, derive the bounds on the free upper shape from the peak factor at zero and infinity, then classify supported, unsupported and exact-contact subsequences with the common softplus penalty. The moving gamma trace covers the prescribed finite-power branch. The full joint constant-data concentration implication remains open. |
| `gb2`, all free | **Open.** Started from normalized densities and proved uniform peak, mode and rate bounds. Need the upper boundary envelope on the compactified chart, normal and gamma height optimization and the asymmetric-Laplace rate and sample-knot comparison. | **Open.** For bounded power, still bound curvature above and below uniformly in the shape ratio, and carry the normalized density limit through the normal seam and both varying-shape gamma branches. For large power, still prove the integrated slope limits, exclude two infinite rates, establish convergence of the normalizing integrals and bound every supported contact trace. The established peak and rate bounds are the input to those steps. |
| `fpareto`, `min` | **Open.** Proved the parameter-identical excess transfer. Need the complete GB2 face-height comparison and finiteness of stationary-component heights. The written proof invokes o-minimal definability of the positive Gamma function and piecewise differentiable paths in the stationary components. That argument has no Lean implementation here. | **Open.** Proved constant divergence and the strict-support objective identity. Need the full all-free GB2 classification and upper bound at moving contacts, then transport it using that identity. |
| `fpareto`, `min+shape1` | **Open.** Proved the held-upper-shape excess path and reflected objective transfer. Need the optimized inverse transformed-gamma and power comparison and finite stationary-component heights. | **Open.** Constant divergence and the full tied upper-cutoff trace are checked. Need the converse from reflected held-shape GB2 exhaustion, including arbitrary tied contacts. |
| `fpareto`, `min+shape3` | **Open.** Proved the held-lower-shape excess path and forward objective transfer. Need the optimized transformed-gamma and Pareto comparison and finite stationary-component heights. | **Open.** Constant divergence and the full tied lower-cutoff trace are checked. Need the converse from held-lower-shape GB2 exhaustion, including arbitrary tied contacts. |

No contradiction with the cited source claims was found in the limits formalized here. The feasible-witness equivalence is a scope deviation from the finite stationary-list claim. The peak bound uses a different constant from the source, and path realization establishes reachability rather than exhaustion. The compact power comparison uses Mathlib's compact maximum theorem directly. The remaining joint sequence exclusions are needed before applying the repository's generic escape or compactification lemmas at full claim scope.

## Validation

The workstation aggregate `lake build` completed successfully with 8604 jobs including temporary imports of all five modules. The imports were removed and `git diff --exit-code -- dev/lean/UnivariateML.lean` passed. Each new module also passed `lake env lean` individually. The atlas checker passed with the new fragment. The new modules contain no admitted proof declarations. Work stayed in the assigned worktree and no commits were made.

## Public Lean statements

### `GBTTrGamma`

**scaleProfile_spec** (trgamma `all free`). The closed-form scale attains the shape-and-power profile and bounds every other positive scale. Criterion, nuisance scale.

```lean
theorem scaleProfile_spec (hn : 0 < n) (x : Fin n → ℝ) {k p : ℝ}
    (hk : 0 < k) (hp : 0 < p) :
    TransformedGamma.logLik x k p (TransformedGamma.scale x k p) = scaleProfile x k p ∧
      ∀ a > 0, TransformedGamma.logLik x k p a ≤ scaleProfile x k p
```

**conditional_score** (trgamma `all free`). The conditional shape is the unique positive inverse-digamma-gap fit for each fixed positive power. Criterion, nuisance shape.

```lean
theorem conditional_score (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) {p : ℝ} (hp : 0 < p) :
    ∃! k : ℝ, 0 < k ∧ UnivariateML.Gamma.scoreGap k =
      UnivariateML.Gamma.jensenGap (TransformedGamma.powerSample x p) ∧
      IsMaxOn (fun t ↦ scaleProfile x t p) (Ioi 0) k
```

**gap_div_power_atTop** (trgamma `all free`). The powered Jensen gap grows at the mean log distance from the sample maximum, with ties allowed. Failure, power exterior.

```lean
theorem gap_div_power_atTop (hn : 0 < n) (x : Fin n → ℝ) (j : Fin n)
    (hj : ∀ i, log (x i) ≤ log (x j)) :
    Tendsto (fun p : ℝ ↦
      UnivariateML.Gamma.jensenGap (TransformedGamma.powerSample x p) / p) atTop
      (𝓝 (log (x j) - UnivariateML.Gamma.avgLog x))
```

**power_profile_atTop** (trgamma `all free`). The fitted profile tends to the optimized power value at infinite power. Failure, power exterior.

```lean
theorem power_profile_atTop (hn : 0 < n) (x : Fin n → ℝ) (j : Fin n)
    (hj : ∀ i, log (x i) ≤ log (x j))
    (hD : 0 < log (x j) - UnivariateML.Gamma.avgLog x)
    {k : ℝ → ℝ} (hk : ∀ p > 0, 0 < k p)
    (hs : ∀ p > 0, UnivariateML.Gamma.scoreGap (k p) =
      UnivariateML.Gamma.jensenGap (TransformedGamma.powerSample x p)) :
    Tendsto (fun p ↦ scaleProfile x (k p) p) atTop
      (𝓝 (log (log (x j) - UnivariateML.Gamma.avgLog x)⁻¹ -
        UnivariateML.Gamma.avgLog x - 1))
```

**power_exterior** (trgamma `all free`). Every sufficiently large power has a uniform upper envelope at the fitted power value plus arbitrary slack. Failure, power exterior.

```lean
theorem power_exterior (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) (j : Fin n) (hj : ∀ i, log (x i) ≤ log (x j))
    {ε : ℝ} (hε : 0 < ε) :
    ∀ᶠ p : ℝ in atTop, ∀ k > 0, ∀ a > 0,
      TransformedGamma.logLik x k p a <
        log (log (x j) - UnivariateML.Gamma.avgLog x)⁻¹ -
          UnivariateML.Gamma.avgLog x - 1 + ε
```

**gap_div_power_sq_at_zero** (trgamma `all free`). The squared-power-normalized Jensen gap tends to half the log variance. Failure, lognormal exterior.

```lean
theorem gap_div_power_sq_at_zero (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) (hne : ∃ i j, x i ≠ x j) :
    Tendsto (fun t : ℝ ↦
      UnivariateML.Gamma.jensenGap (TransformedGamma.powerSample x t) / t ^ 2) (𝓝[>] 0)
      (𝓝 (UnivariateML.Gamma.avg (fun i ↦
        (log (x i) - UnivariateML.Gamma.avgLog x) ^ 2) / 2))
```

**lognormal_exterior** (trgamma `all free`). The fitted lognormal height bounds the likelihood in a sufficiently small-power collar, uniformly over shape and scale. Failure, lognormal exterior.

```lean
theorem lognormal_exterior (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) {ε : ℝ} (hε : 0 < ε) :
    let V := UnivariateML.Gamma.avg (fun i ↦ (log (x i) - UnivariateML.Gamma.avgLog x)^2)
    ∀ᶠ t : ℝ in 𝓝[>] 0, ∀ k > 0, ∀ a > 0,
      TransformedGamma.logLik x k t a <
        -log (2 * π) / 2 - log V / 2 - 1 / 2 - UnivariateML.Gamma.avgLog x + ε
```

**attains_of_face_witness** (trgamma `all free`). A finite witness at or above both exterior heights implies a finite maximum, including equality. Criterion, sufficiency.

```lean
theorem attains_of_face_witness (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) (j : Fin n) (hj : ∀ i, log (x i) ≤ log (x j)) :
    let V := UnivariateML.Gamma.avg (fun i ↦ (log (x i) - UnivariateML.Gamma.avgLog x)^2)
    let N := -log (2 * π) / 2 - log V / 2 - 1 / 2 - UnivariateML.Gamma.avgLog x
    let P := log (log (x j) - UnivariateML.Gamma.avgLog x)⁻¹ -
      UnivariateML.Gamma.avgLog x - 1
    (∃ k > 0, ∃ t > 0, ∃ a > 0, max N P ≤ TransformedGamma.logLik x k t a) →
      ∃ k > 0, ∃ t > 0, ∃ a > 0, ∀ k' > 0, ∀ t' > 0, ∀ a' > 0,
        TransformedGamma.logLik x k' t' a' ≤ TransformedGamma.logLik x k t a
```

**attainment_witness_criterion** (trgamma `all free`). The feasible-witness comparison is an equivalence on nonconstants, and the likelihood is unbounded on constants. Criterion, finite feasibility rather than finite stationary enumeration.

```lean
theorem attainment_witness_criterion (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) (j : Fin n) (hj : ∀ i, log (x i) ≤ log (x j)) :
    let V := UnivariateML.Gamma.avg (fun i ↦ (log (x i) - UnivariateML.Gamma.avgLog x)^2)
    let N := -log (2 * π) / 2 - log V / 2 - 1 / 2 - UnivariateML.Gamma.avgLog x
    let P := log (log (x j) - UnivariateML.Gamma.avgLog x)⁻¹ -
      UnivariateML.Gamma.avgLog x - 1
    ((∃ i j, x i ≠ x j) →
      ((∃ k > 0, ∃ t > 0, ∃ a > 0, ∀ k' > 0, ∀ t' > 0, ∀ a' > 0,
          TransformedGamma.logLik x k' t' a' ≤ TransformedGamma.logLik x k t a) ↔
        ∃ k > 0, ∃ t > 0, ∃ a > 0, max N P ≤ TransformedGamma.logLik x k t a)) ∧
    ((∀ i j, x i = x j) →
      ∀ C : ℝ, ∃ k > 0, ∃ t > 0, ∃ a > 0, C < TransformedGamma.logLik x k t a)
```

### `GBTInvTrGamma`

**pareto_exterior** (invtrgamma `all free`). Reciprocity turns the large-power bound into the fitted Pareto upper envelope at the sample minimum. Failure, Pareto exterior.

```lean
theorem pareto_exterior {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) (j : Fin n) (hj : ∀ i, x j ≤ x i)
    {ε : ℝ} (hε : 0 < ε) :
    ∀ᶠ p : ℝ in atTop, ∀ k > 0, ∀ a > 0,
      InverseTransformedGamma.logLik x k p a <
        log (Gamma.avgLog x - log (x j))⁻¹ - Gamma.avgLog x - 1 + ε
```

**lognormal_exterior** (invtrgamma `all free`). Reciprocity transports the uniform small-power bound using the same log variance and the original-data Jacobian. Failure, lognormal exterior.

```lean
theorem lognormal_exterior {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) {ε : ℝ} (hε : 0 < ε) :
    let V := Gamma.avg (fun i ↦ (log (x i) - Gamma.avgLog x)^2)
    ∀ᶠ t : ℝ in 𝓝[>] 0, ∀ k > 0, ∀ a > 0,
      InverseTransformedGamma.logLik x k t a <
        -log (2 * π) / 2 - log V / 2 - 1 / 2 - Gamma.avgLog x + ε
```

**attainment_witness_criterion** (invtrgamma `all free`). The witness comparison and constant branch transport to the inverse model with the correct two face heights. Criterion, finite feasibility rather than finite stationary enumeration.

```lean
theorem attainment_witness_criterion {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) (j : Fin n) (hj : ∀ i, x j ≤ x i) :
    let V := Gamma.avg (fun i ↦ (log (x i) - Gamma.avgLog x)^2)
    let N := -log (2 * π) / 2 - log V / 2 - 1 / 2 - Gamma.avgLog x
    let P := log (Gamma.avgLog x - log (x j))⁻¹ - Gamma.avgLog x - 1
    ((∃ i j, x i ≠ x j) →
      ((∃ k > 0, ∃ t > 0, ∃ a > 0, ∀ k' > 0, ∀ t' > 0, ∀ a' > 0,
          InverseTransformedGamma.logLik x k' t' a' ≤
            InverseTransformedGamma.logLik x k t a) ↔
        ∃ k > 0, ∃ t > 0, ∃ a > 0, max N P ≤ InverseTransformedGamma.logLik x k t a)) ∧
    ((∀ i j, x i = x j) →
      ∀ C : ℝ, ∃ k > 0, ∃ t > 0, ∃ a > 0, C < InverseTransformedGamma.logLik x k t a)
```

### `GBTLogConcave`

**peak_mul_distance_le** (gb2 `all free`). A normalized log-concave density bounds peak times distance by an affine function of the pointwise log loss. Failure, shared normalization bound.

```lean
theorem peak_mul_distance_le {f : ℝ → ℝ} {m y : ℝ}
    (hconc : ConcaveOn ℝ univ f) (hm : f y ≤ f m)
    (hi : Integrable (fun z ↦ exp (f z)) volume)
    (hn : (∫ z, exp (f z)) = 1) :
    exp (f m) * |y - m| ≤ exp 1 * (1 + (f m - f y))
```

**sample_superlevel** (gb2 `all free`). Every nonconstant log-sample superlevel bounds the mode and bounds the peak above and away from zero. Failure, shared superlevel bound.

```lean
theorem sample_superlevel {n : ℕ} (hn : 0 < n) (y : Fin n → ℝ)
    (hne : ∃ i j, y i ≠ y j) (c : ℝ) :
    ∃ B M : ℝ, ∀ {f : ℝ → ℝ} {m : ℝ},
      ConcaveOn ℝ univ f → (∀ x, f x ≤ f m) →
      Integrable (fun z ↦ exp (f z)) volume → (∫ z, exp (f z)) = 1 →
      c ≤ (∑ i, f (y i)) / n → exp c ≤ exp (f m) ∧ exp (f m) ≤ B ∧ |m| ≤ M
```

### `GBTGB2`

**reciprocal_logLik** (gb2 `shape1`). Reciprocating the data and scale swaps the held shape, with a sample-only log Jacobian. Criterion and failure, shape1 and shape3 transfer.

```lean
theorem reciprocal_logLik {n : ℕ} {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {s : ℝ} (hs : 0 < s) (b p q : ℝ) :
    Shape2GB2.logLik (fun i ↦ (x i)⁻¹) s⁻¹ b (q, p) =
      Shape2GB2.logLik x s b (p, q) + ∑ i, 2 * log (x i)
```

**log_sample_superlevel** (gb2 `all free`). Normalized GB2 peaks and log modes obey uniform bounds on each nonconstant-sample superlevel. Failure, joint peak and mode.

```lean
theorem log_sample_superlevel {n : ℕ} (hn : 0 < n) (y : Fin n → ℝ)
    (hne : ∃ i j, y i ≠ y j) (c : ℝ) :
    ∃ B M : ℝ, ∀ (v : ℝ) {a p q : ℝ}, 0 < a → 0 < p → 0 < q →
      c ≤ (∑ i, logDensity v a p q (y i)) / n →
      let m := v + log (p / q) / a
      exp c ≤ exp (logDensity v a p q m) ∧
        exp (logDensity v a p q m) ≤ B ∧ |m| ≤ M
```

**logLik_log_chart** (gb2 `all free`). The original GB2 objective differs from the log-sample objective by the additive log Jacobian. Criterion and failure, chart identification.

```lean
theorem logLik_log_chart {n : ℕ} {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {s : ℝ} (hs : 0 < s) (a p q : ℝ) :
    Shape2GB2.logLik x s a (p, q) + ∑ i, log (x i) =
      ∑ i, logDensity (log s) a p q (log (x i))
```

**peak_rate_bound** (gb2 `all free`). The density at any point is at most the harmonic combination of the two one-sided rates. Failure, rate lower bounds.

```lean
theorem peak_rate_bound (v m : ℝ) {a p q : ℝ} (ha : 0 < a) (hp : 0 < p) (hq : 0 < q) :
    exp (logDensity v a p q m) * ((a * p)⁻¹ + (a * q)⁻¹) ≤ 1
```

**superlevel_tail_rates** (gb2 `all free`). Both rates are bounded below by the exponentiated mean log-likelihood level. At either fixed beta shape this forces power away from zero. Failure, all-free and held-shape rate bounds.

```lean
theorem superlevel_tail_rates {n : ℕ} (hn : 0 < n) (y : Fin n → ℝ)
    {v a p q c : ℝ} (ha : 0 < a) (hp : 0 < p) (hq : 0 < q)
    (hlevel : c ≤ (∑ i, logDensity v a p q (y i)) / n) :
    exp c ≤ a * p ∧ exp c ≤ a * q
```

**pareto_cutoff_path** (gb2 `shape3`). With lower shape held, moving the scale below the limiting cutoff realizes the whole Pareto endpoint density at all ties. Failure, Pareto face reachability.

```lean
theorem pareto_cutoff_path {n : ℕ} (y : Fin n → ℝ) {p μ D : ℝ}
    (hp : 0 < p) (hD : 0 < D) (hy : ∀ i, μ ≤ y i) :
    Tendsto (fun a : ℝ ↦
      ∑ i, logDensity (μ - log a / a) a p (D / a) (y i)) atTop
      (𝓝 (∑ i, (log D - D * (y i - μ))))
```

**power_cutoff_path** (gb2 `shape1`). Reflection realizes the whole upper-cutoff power density at all ties with upper shape held. Failure, power face reachability.

```lean
theorem power_cutoff_path {n : ℕ} (y : Fin n → ℝ) {q μ A : ℝ}
    (hq : 0 < q) (hA : 0 < A) (hy : ∀ i, y i ≤ μ) :
    Tendsto (fun a : ℝ ↦
      ∑ i, logDensity (μ + log a / a) a (A / a) q (y i)) atTop
      (𝓝 (∑ i, (log A - A * (μ - y i))))
```

**held_lower_gamma_trace** (gb2 `shape3`). With lower shape fixed and upper shape diverging, positive limiting power and a finite limiting mode give the transformed-gamma trace. Failure, moving gamma trace.

```lean
theorem held_lower_gamma_trace {ι : Type*} {l : Filter ι} {n : ℕ}
    (y : Fin n → ℝ) {p a₀ m₀ : ℝ} (hp : 0 < p) (ha₀ : 0 < a₀)
    {a q m : ι → ℝ} (ha : Tendsto a l (𝓝 a₀))
    (hq : Tendsto q l atTop) (hm : Tendsto m l (𝓝 m₀)) :
    Tendsto (fun j ↦ ∑ i, logDensity (m j - log (p / q j) / a j) (a j) p (q j) (y i)) l
      (𝓝 (∑ i, (log a₀ + p * log p - log (Real.Gamma p) +
        p * a₀ * (y i - m₀) - p * exp (a₀ * (y i - m₀)))))
```

**held_upper_gamma_trace** (gb2 `shape1`). The reflected moving limit gives the inverse transformed-gamma trace with upper shape held. Failure, moving inverse-gamma trace.

```lean
theorem held_upper_gamma_trace {ι : Type*} {l : Filter ι} {n : ℕ}
    (y : Fin n → ℝ) {q a₀ m₀ : ℝ} (hq : 0 < q) (ha₀ : 0 < a₀)
    {a p m : ι → ℝ} (ha : Tendsto a l (𝓝 a₀))
    (hp : Tendsto p l atTop) (hm : Tendsto m l (𝓝 m₀)) :
    Tendsto (fun j ↦ ∑ i, logDensity (m j - log (p j / q) / a j) (a j) (p j) q (y i)) l
      (𝓝 (∑ i, (log a₀ + q * log q - log (Real.Gamma q) +
        q * a₀ * (m₀ - y i) - q * exp (a₀ * (m₀ - y i)))))
```

### `GBTFellerPareto`

**logLik_positive_excess** (fpareto `min`). Translating to positive excesses preserves the entire objective in all three requested hold masks. Criterion and failure, strict-support transfer.

```lean
theorem logLik_positive_excess {n : ℕ} {x : Fin n → ℝ} {μ s : ℝ}
    (hx : ∀ i, μ < x i) (hs : 0 < s) (b : ℝ) (sh : ℝ × ℝ) :
    ShapeFacesFellerPareto.logLikelihood x μ b s sh =
      Shape2GB2.logLik (fun i ↦ x i - μ) s b sh
```

**pareto_excess_path** (fpareto `min+shape3`). The held-lower-shape Pareto path transports with the original-excess Jacobian and all tied contacts. Failure, Pareto path.

```lean
theorem pareto_excess_path {n : ℕ} {x : Fin n → ℝ} {μ p D m : ℝ}
    (hx : ∀ i, μ < x i) (hp : 0 < p) (hD : 0 < D)
    (hm : ∀ i, m ≤ log (x i - μ)) :
    Tendsto (fun a : ℝ ↦ ShapeFacesFellerPareto.logLikelihood x μ a
      (exp (m - log a / a)) (p, D / a)) atTop
      (𝓝 ((∑ i, (log D - D * (log (x i - μ) - m))) - ∑ i, log (x i - μ)))
```

**power_excess_path** (fpareto `min+shape1`). The held-upper-shape power path transports with the original-excess Jacobian and all tied contacts. Failure, power path.

```lean
theorem power_excess_path {n : ℕ} {x : Fin n → ℝ} {μ q A m : ℝ}
    (hx : ∀ i, μ < x i) (hq : 0 < q) (hA : 0 < A)
    (hm : ∀ i, log (x i - μ) ≤ m) :
    Tendsto (fun a : ℝ ↦ ShapeFacesFellerPareto.logLikelihood x μ a
      (exp (m + log a / a)) (A / a, q)) atTop
      (𝓝 ((∑ i, (log A - A * (m - log (x i - μ)))) - ∑ i, log (x i - μ)))
```

**constant_excess_concentration** (fpareto `min`). The likelihood diverges at a constant positive excess as power grows with both shapes already fixed. Failure and criterion, constant branch in all three masks.

```lean
theorem constant_excess_concentration {n : ℕ} (hn : 0 < n) {μ x : ℝ} (hx : μ < x)
    (sh : ℝ × ℝ) :
    Tendsto (fun a : ℝ ↦ ShapeFacesFellerPareto.logLikelihood (fun _ : Fin n ↦ x)
      μ a (x - μ) sh) atTop atTop
```
