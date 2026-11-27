# TASK-396 · Lean likelihood criteria

The EMG, translated half-t and Azzalini skew-normal likelihoods now have exact all-free attainment criteria in Lean. The beta-negative-binomial score theorems and the Conway–Maxwell–binomial ray theorems establish their sign and exposure conditions for the family formulas. The Skellam likelihood reduces to the signed-mean profile for arbitrary observed magnitudes. The atlas fragment records 37 public theorems. [Coverage review](2026-11-27-atlas-coverage-report.md) · [Atlas fragment](../existence-atlas/lean-map.d/lean-finish.csv)

## Source discrepancy

**The Skellam atlas overstates its uniqueness evidence.** The all-free uniqueness claim includes every integer sample with both signs and empirical mean zero. The earlier Lean uniqueness proof requires one observed positive magnitude. The written report leaves the general integer tie question open. The new diagonal reduction holds at any number of magnitudes. Ruling out ties still requires comparing distinct positive radial maximizers, or finding an exact integer counterexample. The real-weight ties in the report concern a different sample domain. The fragment leaves the uniqueness coverage as it was. [Atlas](../existence-atlas/atlas.csv) · [Skellam written analysis](2026-11-25-skellam2-math-report.md#open)

## Proof scope and open entries

- **EMG.** The compactification uses total log likelihood and the chart with dimensionless shape `k = (1-t)/t`. The normal endpoint uses joint Mills-ratio convergence in inverse-scale coordinates. Strict concavity keeps the nuisance optimizers near the normal optimum. At the exponential endpoint a bound uniform in location and scale gives the upper limit. A moving location realizes the full translated-exponential value at tied minima. Two bounds on the profile increment give interior differentiability. Finite equality with either face is included, and constant data give an unbounded log likelihood. A Lean proof of the written finite stationary-list theorem still needs analyticity and the definability argument. The exact real tie examples retain their written proofs. [EMG written analysis](2026-11-25-emg-math-report.md#exact-finite-comparison-and-per-input-certificates)
- **Half-t.** The fitted floor is one. A strict majority at the minimum gives divergence. In the bounded branch, the candidate must have positive collapse exponent and be either the floor or an interior stationary shape. The exact half-tie floor collapse enters as an excluded face. The comparison uses likelihood products and keeps finite equality. Counting stationary shapes and certifying particular tied maximizing sets remain separate from this criterion. [Half-t written analysis](2026-11-25-halft-math-report.md)
- **Skew normal.** The model is the Azzalini family `sn`. The Gaussian envelope gives compact confinement on every log-likelihood superlevel, uniformly in slant. A log-CDF tangent at zero gives the strict right cutoff, and reflection gives the left cutoff. The moving-location paths reach the optimized half-normal faces at the sample extremes and their RMS scales. The exact comparison supplies both a finite-attainment criterion and the unattained supremum. A Lean proof of finiteness of the stationary list still needs analytic profile regularity and isolation of zeros. Exact reflected-tie examples and floating-point detector accuracy are separate claims. [Skew-normal written analysis](2026-11-25-sn-math-report.md#strict-tail-exclusion-and-exact-sample-criterion)
- **Beta-negative binomial.** The statements give the actual derivative signs, unique root and unique maximizer in each positive-count, one-free-shape mask. The log-mass expression is restated from the dossier. The old module hides the body of its log mass and the concrete score lemmas across imports, so this module checks the required gamma and integral arguments locally and reuses the public Laplace root theorem. The old all-zero face theorem covers the complementary sample branch. The all-free comparison with negative-binomial and Poisson faces remains outside this result. [Earlier Lean module](../../lean/UnivariateML/Family/Discrete3BNBinom.lean) · [Beta-negative-binomial dossier](../../../univariateML-cpp/include/cppunivariate/distributions/bnbinom/README.md)
- **CMB.** The direction of each ray is computed from the log-binomial statistic. The adjacent-edge slope is its factorial-log increment, the point direction averages two adjoining edge directions, and negative dispersion at fixed odds exposes the endpoints. The ray statements retain arbitrary finite base weights. The earlier global face results give saturated comparisons over the full canonical chart. The new hull proof establishes full affine span and gives the open canonical criterion and the closed probability criterion at held size at least two. The size-zero and Bernoulli branches remain in the earlier module. An exhaustive integer-support characterization of the hull interior, the remaining scalar held-mask comparisons and the variable-size classifications retain their existing proof status. [CMB written analysis](2026-11-25-cmb-math-report.md#held-size) · [Earlier Lean module](../../lean/UnivariateML/Family/Discrete3CMB.lean)
- **Skellam.** The fixed-product tilt factors out of the convolution as a common multiplier. This gives the signed-mean condition at every joint maximizer, including on a compatible Poisson axis. For balanced two-sign data it puts every maximizer on the positive diagonal, and joint attainment supplies one there. The optional work formalizes the reduction beyond one magnitude. The radial curvature and finiteness arguments, weighted coexistence examples and general integer uniqueness remain separate. [Skellam written analysis](2026-11-25-skellam2-math-report.md#reflection-and-a-uniqueness-theorem)

## Effort

Approximate working intervals include reading, proof construction and individual Lean iterations. The integration check was shared across targets.

| Target | Working time | Main proof work |
| --- | --- | --- |
| EMG | 15–20 min | Two optimized endpoint limits, concave optimizer stability and the envelope derivative |
| Half-t | 5–8 min | Log-scale profile derivative, majority bound and eligible floor comparison |
| Skew normal | 5–8 min | Uniform compact confinement, signed slant cutoffs and half-normal comparison |
| Beta-negative binomial | 3–5 min | Local reconstruction of the mass derivative and its one-crossing kernels |
| CMB | 6–10 min | CMB exposure directions, affine span and the closed held-size comparison |
| Skellam | 4–6 min | Convolution tilt, signed-mean maximizers and the positive diagonal reduction |

## Verification

All new files passed `lake env lean` from `dev/lean` on the workstation through `/tmp/umvl-cpu/run` with `nice -n 10`. The final root build used the same workstation and slot wrapper, pinned to one core. **Build completed successfully (7884 jobs).** The final run reused the Mathlib cache and replayed existing linter warnings. All temporary root imports were removed, and `git diff --exit-code -- dev/lean/UnivariateML.lean` passed. All new proofs are complete Lean terms.

## EMG statements

### `UnivariateML.FinishEMG.exponential_endpoint`

As dimensionless shape tends to zero, the optimized likelihood converges to the exponential fit on the sample shifted by its minimum. [Lean source](../../lean/UnivariateML/Family/FinishEMG.lean#L105)

```lean
theorem exponential_endpoint {n : ℕ} {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x)
    (i : Fin n) (hi : ∀ j, x i ≤ x j) :
    Tendsto (rateProfile x) (𝓝[>] 0)
      (𝓝 (Exponential.logLik (fun j ↦ x j-x i) (Exponential.rate (fun j ↦ x j-x i))))
```

### `UnivariateML.FinishEMG.normal_endpoint`

As dimensionless shape tends to infinity, the optimized likelihood converges to the ordinary all-free normal fit. [Lean source](../../lean/UnivariateML/Family/FinishEMG.lean#L354)

```lean
theorem normal_endpoint {n : ℕ} {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x) :
    Tendsto (rateProfile x) atTop (𝓝 (Normal.logLik x (Normal.mean x)
      (Normal.sd x (Normal.mean x))))
```

### `UnivariateML.FinishEMG.compactification`

The assigned face values extend the actual profile continuously on the closed chart, and the interior profile is differentiable. [Lean source](../../lean/UnivariateML/Family/FinishEMG.lean#L459)

```lean
theorem compactification {n : ℕ} {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x)
    (i : Fin n) (hi : ∀ j, x i ≤ x j) :
    ContinuousOn (compactProfile x i) (Icc 0 1) ∧
      DifferentiableOn ℝ (compactProfile x i) (Ioo 0 1) ∧
      EqOn (compactProfile x i) (interiorProfile x) (Ioo 0 1)
```

### `UnivariateML.FinishEMG.all_free_criterion`

A nonconstant sample has a finite fit exactly when an interior stationary value reaches both optimized faces. [Lean source](../../lean/UnivariateML/Family/FinishEMG.lean#L468)

```lean
theorem all_free_criterion {n : ℕ} {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x)
    (i : Fin n) (hi : ∀ j, x i ≤ x j) :
    (∃ t ∈ Ioo 0 1, HasDerivAt (compactProfile x i) 0 t ∧
      max (compactProfile x i 0) (compactProfile x i 1) ≤ compactProfile x i t) ↔
      ∃ q ∈ (Ioo 0 1 ×ˢ LocationScale.domain),
        IsMaxOn (fun p : ℝ × (ℝ × ℝ) ↦ shapeLogLik x ((1-p.1)/p.1) p.2)
          (Ioo 0 1 ×ˢ LocationScale.domain) q
```

### `UnivariateML.FinishEMG.constant_log_unbounded`

With a common observation, shrinking the scale at fixed dimensionless shape makes the log likelihood unbounded. [Lean source](../../lean/UnivariateML/Family/FinishEMG.lean#L480)

```lean
theorem constant_log_unbounded {n : ℕ} (hn : 0 < n) (m : ℝ) :
    ¬ BddAbove ((fun p : ℝ × (ℝ × ℝ) ↦
      shapeLogLik (fun _ : Fin n ↦ m) ((1-p.1)/p.1) p.2) ''
        (Ioo 0 1 ×ˢ LocationScale.domain))
```

### `UnivariateML.FinishEMG.unattained_face_supremum`

If a nonconstant sample has no finite maximizer, the larger optimized face is its exact unattained supremum. [Lean source](../../lean/UnivariateML/Family/FinishEMG.lean#L501)

```lean
theorem unattained_face_supremum {n : ℕ} {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x)
    (i : Fin n) (hi : ∀ j, x i ≤ x j)
    (h : ¬ ∃ q ∈ (Ioo 0 1 ×ˢ LocationScale.domain),
      IsMaxOn (fun p : ℝ × (ℝ × ℝ) ↦ shapeLogLik x ((1-p.1)/p.1) p.2)
        (Ioo 0 1 ×ˢ LocationScale.domain) q) :
    IsLUB ((fun p : ℝ × (ℝ × ℝ) ↦ shapeLogLik x ((1-p.1)/p.1) p.2) ''
      (Ioo 0 1 ×ˢ LocationScale.domain)) (max (compactProfile x i 0) (compactProfile x i 1))
```

## Half-t statements

### `UnivariateML.FinishHalfT.all_free_unbounded_iff`

The all-free likelihood is unbounded exactly when a strict majority of observations equal the minimum. [Lean source](../../lean/UnivariateML/Family/FinishHalfT.lean#L219)

```lean
theorem all_free_unbounded_iff {n : ℕ} (x : Fin n → ℝ) {i : Fin n}
    (hi : ∀ j, x i ≤ x j) :
    (¬ BddAbove ((fun p : ℝ × (ℝ × ℝ) ↦ likelihood x p.1 p.2.1 p.2.2) '' domain x)) ↔
      n < 2 * TDist.multiplicity x (x i)
```

### `UnivariateML.FinishHalfT.all_free_criterion`

Finite attainment is equivalent to at most half minimum ties and an eligible floor or stationary shape that reaches both faces. [Lean source](../../lean/UnivariateML/Family/FinishHalfT.lean#L237)

```lean
theorem all_free_criterion {n : ℕ} (x : Fin n → ℝ) {i : Fin n}
    (hi : ∀ j, x i ≤ x j) :
    (∃ p ∈ domain x, IsMaxOn (fun p ↦ likelihood x p.1 p.2.1 p.2.2) (domain x) p) ↔
      2 * TDist.multiplicity x (x i) ≤ n ∧
      ∃ ν ≥ (1 : ℝ), 0 < TDist.exponent x (x i) ν ∧
        (ν = 1 ∨ HasDerivAt (scaleProfile x (x i)) 0 ν) ∧
        max (normalFace x (x i)) (collapseFace x (x i)) ≤ scaleProfile x (x i) ν
```

### `UnivariateML.FinishHalfT.unattained_face_supremum`

Below or at a half tie, failure of finite attainment leaves the larger of the half-normal and applicable scale-zero values as the exact supremum. [Lean source](../../lean/UnivariateML/Family/FinishHalfT.lean#L306)

```lean
theorem unattained_face_supremum {n : ℕ} (x : Fin n → ℝ) {i : Fin n}
    (hi : ∀ j, x i ≤ x j) (hc : 2 * TDist.multiplicity x (x i) ≤ n)
    (h : ¬ ∃ p ∈ domain x,
      IsMaxOn (fun p ↦ likelihood x p.1 p.2.1 p.2.2) (domain x) p) :
    IsLUB ((fun p : ℝ × (ℝ × ℝ) ↦ likelihood x p.1 p.2.1 p.2.2) '' domain x)
      (max (normalFace x (x i)) (collapseFace x (x i)))
```

## Skew normal statements

### `UnivariateML.FinishSN.objective_transfer`

The objective in the inverse-scale chart is exactly the physical skew-normal log likelihood. [Lean source](../../lean/UnivariateML/Family/FinishSN.lean#L49)

```lean
theorem objective_transfer {n : ℕ} (x : Fin n → ℝ) (α : ℝ)
    (p : LocationScale.domain) :
    objective x α (LocationScale.inverseEquiv p) = logLik α x p
```

### `UnivariateML.FinishSN.nuisance_confinement`

A compact nuisance set contains every parameter choice in a fixed log-likelihood superlevel, uniformly over slant. [Lean source](../../lean/UnivariateML/Family/FinishSN.lean#L88)

```lean
theorem nuisance_confinement {n : ℕ} {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x)
    (L : ℝ) : ∃ K : Set LocationScale.inverseDomain, IsCompact K ∧
      ∀ (α : ℝ) (p : LocationScale.inverseDomain), L ≤ objective x α p → p ∈ K
```

### `UnivariateML.FinishSN.right_tail`

The entire sufficiently positive slant tail lies strictly below the optimized right half-normal face. [Lean source](../../lean/UnivariateML/Family/FinishSN.lean#L173)

```lean
theorem right_tail {n : ℕ} {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x)
    (i : Fin n) (hi : ∀ j, x i ≤ x j) :
    ∃ A > 0, ∀ α ≥ A, ∀ p ∈ LocationScale.inverseDomain, objective x α p < face x (x i)
```

### `UnivariateML.FinishSN.left_tail`

The entire sufficiently negative slant tail lies strictly below the optimized left half-normal face. [Lean source](../../lean/UnivariateML/Family/FinishSN.lean#L239)

```lean
theorem left_tail {n : ℕ} {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x)
    (i : Fin n) (hi : ∀ j, x j ≤ x i) :
    ∃ A > 0, ∀ α ≤ -A, ∀ p ∈ LocationScale.inverseDomain, objective x α p < face x (x i)
```

### `UnivariateML.FinishSN.right_face_limit`

The optimized right face is reached along a moving-location path, with full weight at tied minima. [Lean source](../../lean/UnivariateML/Family/FinishSN.lean#L300)

```lean
theorem right_face_limit {n : ℕ} {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x)
    (i : Fin n) (hi : ∀ j, x i ≤ x j) :
    Tendsto (fun t : ℝ ↦ logLik (t^2) x (x i-t⁻¹,Normal.sd x (x i))) atTop
      (𝓝 (face x (x i)))
```

### `UnivariateML.FinishSN.left_face_limit`

The optimized left face is reached along a moving-location path, with full weight at tied maxima. [Lean source](../../lean/UnivariateML/Family/FinishSN.lean#L311)

```lean
theorem left_face_limit {n : ℕ} {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x)
    (i : Fin n) (hi : ∀ j, x j ≤ x i) :
    Tendsto (fun t : ℝ ↦ logLik (-(t^2)) x (x i+t⁻¹,Normal.sd x (x i))) atTop
      (𝓝 (face x (x i)))
```

### `UnivariateML.FinishSN.all_free_criterion`

A nonconstant sample attains a finite maximum exactly when a finite point reaches the larger optimized half-normal likelihood. [Lean source](../../lean/UnivariateML/Family/FinishSN.lean#L337)

```lean
theorem all_free_criterion {n : ℕ} {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x)
    (i j : Fin n) (hi : ∀ k, x i ≤ x k) (hj : ∀ k, x k ≤ x j) :
    (∃ p : ℝ × LocationScale.inverseDomain,
      IsMaxOn (fun ap : ℝ × LocationScale.inverseDomain ↦ objective x ap.1 ap.2) univ p) ↔
      ∃ (α : ℝ) (p : LocationScale.inverseDomain),
        max (face x (x i)) (face x (x j)) ≤ objective x α p
```

### `UnivariateML.FinishSN.unattained_face_supremum`

If a nonconstant sample has no finite fit, the larger optimized half-normal value is its supremum. [Lean source](../../lean/UnivariateML/Family/FinishSN.lean#L370)

```lean
theorem unattained_face_supremum {n : ℕ} {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x)
    (i j : Fin n) (hi : ∀ k, x i ≤ x k) (hj : ∀ k, x k ≤ x j)
    (h : ¬ ∃ p : ℝ × LocationScale.inverseDomain,
      IsMaxOn (fun ap : ℝ × LocationScale.inverseDomain ↦ objective x ap.1 ap.2) univ p) :
    IsLUB (range (fun ap : ℝ × LocationScale.inverseDomain ↦ objective x ap.1 ap.2))
      (max (face x (x i)) (face x (x j)))
```

### `UnivariateML.FinishSN.stationary_criterion`

The finite witness can equivalently be required to lie at a stationary slant of the nuisance profile. [Lean source](../../lean/UnivariateML/Family/FinishSN.lean#L477)

```lean
theorem stationary_criterion {n : ℕ} {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x)
    (i j : Fin n) (hi : ∀ k, x i ≤ x k) (hj : ∀ k, x k ≤ x j) :
    (∃ p : ℝ × LocationScale.inverseDomain,
      IsMaxOn (fun ap : ℝ × LocationScale.inverseDomain ↦ objective x ap.1 ap.2) univ p) ↔
      ∃ α : ℝ, HasDerivAt (profile x) 0 α ∧ max (face x (x i)) (face x (x j)) ≤ profile x α
```

### `UnivariateML.FinishSN.profile_face_limits`

The profiled likelihood has the right and left optimized half-normal limits at positive and negative infinite slant. [Lean source](../../lean/UnivariateML/Family/FinishSN.lean#L512)

```lean
theorem profile_face_limits {n : ℕ} {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x)
    (i j : Fin n) (hi : ∀ k, x i ≤ x k) (hj : ∀ k, x k ≤ x j) :
    Tendsto (profile x) atTop (𝓝 (face x (x i))) ∧
      Tendsto (profile x) atBot (𝓝 (face x (x j)))
```

### `UnivariateML.FinishSN.constant_unbounded`

A nonempty constant sample gives unbounded all-free likelihood. [Lean source](../../lean/UnivariateML/Family/FinishSN.lean#L541)

```lean
theorem constant_unbounded {n : ℕ} (hn : 0 < n) (m : ℝ) :
    ¬ BddAbove (range (fun ap : ℝ × LocationScale.inverseDomain ↦
      objective (fun _ : Fin n ↦ m) ap.1 ap.2))
```

## Beta-negative binomial statements

### `UnivariateML.FinishBNBinom.alpha_score`

With positive size and beta held and at least one positive count, the alpha score crosses zero exactly once, from positive to negative. The root is the unique maximum. [Lean source](../../lean/UnivariateML/Family/FinishBNBinom.lean#L915)

```lean
theorem alpha_score {n : ℕ} (hn : 0 < n) {x : Fin n → ℕ} (hx : ∃ i, 0 < x i)
    {r b : ℝ} (hr : 0 < r) (hb : 0 < b) :
    let f := fun t ↦ averageLogMass x r t b
    ∃ a > 0, HasDerivAt f 0 a ∧
      (∀ t ∈ Ioo 0 a, 0 < deriv f t) ∧ (∀ t > a, deriv f t < 0) ∧
      (∀ t > 0, IsMaxOn f (Ioi 0) t ↔ t = a)
```

### `UnivariateML.FinishBNBinom.size_score`

With positive alpha and beta held and at least one positive count, the size score crosses zero exactly once, from positive to negative. The root is the unique maximum. [Lean source](../../lean/UnivariateML/Family/FinishBNBinom.lean#L936)

```lean
theorem size_score {n : ℕ} (hn : 0 < n) {x : Fin n → ℕ} (hx : ∃ i, 0 < x i)
    {a b : ℝ} (ha : 0 < a) (hb : 0 < b) :
    let f := fun t ↦ averageLogMass x t a b
    ∃ r > 0, HasDerivAt f 0 r ∧
      (∀ t ∈ Ioo 0 r, 0 < deriv f t) ∧ (∀ t > r, deriv f t < 0) ∧
      (∀ t > 0, IsMaxOn f (Ioi 0) t ↔ t = r)
```

### `UnivariateML.FinishBNBinom.beta_score`

With positive size and alpha held and at least one positive count, symmetry gives the same unique score root and maximum for beta. [Lean source](../../lean/UnivariateML/Family/FinishBNBinom.lean#L968)

```lean
theorem beta_score {n : ℕ} (hn : 0 < n) {x : Fin n → ℕ} (hx : ∃ i, 0 < x i)
    {r a : ℝ} (hr : 0 < r) (ha : 0 < a) :
    let f := fun t ↦ averageLogMass x r a t
    ∃ b > 0, HasDerivAt f 0 b ∧
      (∀ t ∈ Ioo 0 b, 0 < deriv f t) ∧ (∀ t > b, deriv f t < 0) ∧
      (∀ t > 0, IsMaxOn f (Ioi 0) t ↔ t = b)
```

## Conway–Maxwell–binomial statements

### `UnivariateML.FinishCMB.adjacent_ray`

Increasing dispersion and subtracting the log-binomial increment from the log-odds direction exposes the chosen adjacent pair. Along the ray the conditional product is the strict supremum. [Lean source](../../lean/UnivariateML/Family/FinishCMB.lean#L206)

```lean
theorem adjacent_ray {N n : ℕ} (hN : 2 ≤ N) (hn : 0 < n) (x : Fin n → Fin (N + 1))
    {l u : Fin (N + 1)} (hlu : u.val = l.val + 1) (hx : ∀ i, x i = l ∨ x i = u)
    (η ν : ℝ) :
    let F : Finset (Fin (N + 1)) := {l,u}
    let a := edgeSlope N l.val
    (∀ k ∈ F, Tendsto (fun t ↦ mass N (η-t*a) (ν+t) k) atTop
      (𝓝 (conditionalMass η ν F k))) ∧
    let U := ∏ i, conditionalMass η ν F (x i)
    IsLUB (range (fun t ↦ likelihood x (η-t*a) (ν+t))) U ∧
      ∀ t, likelihood x (η-t*a) (ν+t) < U
```

### `UnivariateML.FinishCMB.point_ray`

The midpoint of two adjacent directions isolates an interior cell. Its mass and the ray supremum for a constant sample are one. [Lean source](../../lean/UnivariateML/Family/FinishCMB.lean#L235)

```lean
theorem point_ray {N n : ℕ} (hn : 0 < n) (x : Fin n → Fin (N + 1))
    {k : Fin (N + 1)} (hk₀ : 0 < k.val) (hkN : k.val < N) (hx : ∀ i, x i = k)
    (η ν : ℝ) :
    let a := pointSlope N k.val
    Tendsto (fun t ↦ mass N (η+t*a) (ν+t) k) atTop (𝓝 1) ∧
    IsLUB (range (fun t ↦ likelihood x (η+t*a) (ν+t))) 1 ∧
      ∀ t, likelihood x (η+t*a) (ν+t) < 1
```

### `UnivariateML.FinishCMB.endpoints_ray`

Decreasing dispersion at fixed log odds isolates both endpoints. The strict ray supremum is the base likelihood conditioned on the two endpoints. [Lean source](../../lean/UnivariateML/Family/FinishCMB.lean#L268)

```lean
theorem endpoints_ray {N n : ℕ} (hN : 2 ≤ N) (hn : 0 < n)
    (x : Fin n → Fin (N + 1)) (hx : ∀ i, x i = 0 ∨ x i = Fin.last N) (η ν : ℝ) :
    let F : Finset (Fin (N + 1)) := {0,Fin.last N}
    (∀ k ∈ F, Tendsto (fun t ↦ mass N η (ν-t) k) atTop (𝓝 (conditionalMass η ν F k))) ∧
    let U := ∏ i, conditionalMass η ν F (x i)
    IsLUB (range (fun t ↦ likelihood x η (ν-t))) U ∧
      ∀ t, likelihood x η (ν-t) < U
```

### `UnivariateML.FinishCMB.held_size_open_hull`

At size at least two the finite canonical fit is unique exactly when the empirical moment vector is inside the moment hull. [Lean source](../../lean/UnivariateML/Family/FinishCMB.lean#L391)

```lean
theorem held_size_open_hull {N n : ℕ} (hN : 2 ≤ N) (hn : 0 < n)
    (x : Fin n → Fin (N + 1)) :
    (∃! θ : EuclideanSpace ℝ (Fin 2),
      IsMaxOn (fun θ ↦ likelihood x (θ 0) (θ 1)) univ θ) ↔
      empirical x ∈ interior (convexHull ℝ (range (statistic N)))
```

### `UnivariateML.FinishCMB.held_size_closed_criterion`

At size at least two, allowing probability endpoints adds precisely the two endpoint-constant cases to the interior-hull attainment criterion. [Lean source](../../lean/UnivariateML/Family/FinishCMB.lean#L458)

```lean
theorem held_size_closed_criterion {N n : ℕ} (hN : 2 ≤ N) (hn : 0 < n)
    (x : Fin n → Fin (N + 1)) :
    (∃ p ∈ Icc (0 : ℝ) 1 ×ˢ (univ : Set ℝ),
      IsMaxOn (fun p : ℝ × ℝ ↦ closedLikelihood x p.1 p.2) (Icc 0 1 ×ˢ univ) p) ↔
      (∀ i, x i = 0) ∨ (∀ i, x i = Fin.last N) ∨
        empirical x ∈ interior (convexHull ℝ (range (statistic N)))
```

## Skellam statements

### `UnivariateML.FinishSkellam.likelihood_tilt`

Changing the log rate ratio while holding the product fixed multiplies the sample likelihood by an explicit exponential factor. [Lean source](../../lean/UnivariateML/Family/FinishSkellam.lean#L57)

```lean
theorem likelihood_tilt {n : ℕ} (x : Fin n → ℤ) (a b u : ℝ) :
    likelihood x (a*exp u,b*exp (-u)) =
      exp ((n : ℝ)*(a+b-a*exp u-b*exp (-u)) + (∑ i, (x i : ℝ))*u) *
        likelihood x (a,b)
```

### `UnivariateML.FinishSkellam.maximizing_rate_difference`

At every joint maximum the rate difference equals the signed sample mean. The argument also covers feasible axes. [Lean source](../../lean/UnivariateML/Family/FinishSkellam.lean#L67)

```lean
theorem maximizing_rate_difference {n : ℕ} (hn : 0 < n) (x : Fin n → ℤ)
    (p : ℝ × ℝ) (hp : p ∈ domain) (hm : IsMaxOn (likelihood x) domain p) :
    p.1-p.2 = (∑ i, (x i : ℝ))/n
```

### `UnivariateML.FinishSkellam.mean_zero_two_signs`

A balanced two-sign sample has a positive diagonal maximizer and every maximizer lies on the diagonal, for any magnitude histogram. [Lean source](../../lean/UnivariateML/Family/FinishSkellam.lean#L145)

```lean
theorem mean_zero_two_signs {n : ℕ} (hn : 0 < n) (x : Fin n → ℤ) (hmean : ∑ i, x i = 0)
    (hpos : ∃ i, 0 < x i) (hneg : ∃ i, x i < 0) :
    (∃ r > 0, IsMaxOn (likelihood x) domain (r,r)) ∧
      ∀ p ∈ domain, IsMaxOn (likelihood x) domain p → 0 < p.1 ∧ p.1 = p.2
```

### `UnivariateML.FinishSkellam.swapped_maximizers_equal`

Swapping rates cannot produce a distinct maximizer of the same signed sample. [Lean source](../../lean/UnivariateML/Family/FinishSkellam.lean#L160)

```lean
theorem swapped_maximizers_equal {n : ℕ} (hn : 0 < n) (x : Fin n → ℤ)
    (p : ℝ × ℝ) (hp : p ∈ domain) (hm : IsMaxOn (likelihood x) domain p)
    (hs : IsMaxOn (likelihood x) domain (p.2, p.1)) :
    (p.2, p.1) = p
```

### `UnivariateML.FinishSkellam.mean_zero_swap`

On any mean-zero sample, exchanging positive rates preserves the likelihood. [Lean source](../../lean/UnivariateML/Family/FinishSkellam.lean#L171)

```lean
theorem mean_zero_swap {n : ℕ} (x : Fin n → ℤ) (hmean : ∑ i, x i = 0)
    {a b : ℝ} (ha : 0 < a) (hb : 0 < b) :
    likelihood x (b,a) = likelihood x (a,b)
```

### `UnivariateML.FinishSkellam.fixed_product_fit`

Any positive rate pair matching the signed mean strictly beats every nonzero log-ratio tilt on its fixed-product slice. [Lean source](../../lean/UnivariateML/Family/FinishSkellam.lean#L183)

```lean
theorem fixed_product_fit {n : ℕ} (hn : 0 < n) (x : Fin n → ℤ)
    {a b : ℝ} (ha : 0 < a) (hb : 0 < b)
    (hmean : a - b = (∑ i, (x i : ℝ)) / n) (u : ℝ) (hu : u ≠ 0) :
    likelihood x (a*exp u,b*exp (-u)) < likelihood x (a,b)
```

### `UnivariateML.FinishSkellam.mean_profile_maximizers`

Maximizing on the closed mean-matching set yields exactly the same maximizing set as the closed quadrant. [Lean source](../../lean/UnivariateML/Family/FinishSkellam.lean#L201)

```lean
theorem mean_profile_maximizers {n : ℕ} (hn : 0 < n) (x : Fin n → ℤ)
    (p : ℝ × ℝ) :
    let C := {q ∈ domain | q.1-q.2 = (∑ i, (x i : ℝ))/n}
    (p ∈ domain ∧ IsMaxOn (likelihood x) domain p) ↔
      p ∈ C ∧ IsMaxOn (likelihood x) C p
```

## Profile lemmas used by EMG, half-t and skew normal statements

### `UnivariateML.FinishProfile.tendsto_optimizer`

Joint convergence near a strict interior optimizer confines every competing nuisance choice to every neighborhood of that optimizer. [Lean source](../../lean/UnivariateML/Generic/FinishProfile.lean#L44)

```lean
theorem tendsto_optimizer {A E : Type*} [NormedAddCommGroup E] [NormedSpace ℝ E]
    [FiniteDimensional ℝ E] {l : Filter A} {D : Set E} (hD : IsOpen D)
    {f : A → E → ℝ} {g : E → ℝ} {p : E} {q : A → E} (hp : p ∈ D)
    (hjoint : ∀ y ∈ D, Tendsto (fun av : A × E ↦ f av.1 av.2)
      (l ×ˢ 𝓝 y) (𝓝 (g y)))
    (hstrict : ∀ y ∈ D, y ≠ p → g y < g p)
    (hconc : ∀ᶠ a in l, ConcaveOn ℝ D (f a))
    (hq : ∀ᶠ a in l, q a ∈ D ∧ f a p ≤ f a (q a)) :
    Tendsto q l (𝓝 p)
```

### `UnivariateML.FinishProfile.hasDerivAt_profile`

A strict joint derivative and convergence of the nuisance optimizer give the profile derivative by comparing two secants with fixed nuisance coordinates. [Lean source](../../lean/UnivariateML/Generic/FinishProfile.lean#L78)

```lean
theorem hasDerivAt_profile {E : Type*} [NormedAddCommGroup E] [NormedSpace ℝ E]
    {f : ℝ → E → ℝ} {P : ℝ → ℝ} {t : ℝ} {p : E} {q : ℝ → E}
    {D : (ℝ × E) →L[ℝ] ℝ}
    (hd : HasStrictFDerivAt (fun ap : ℝ × E ↦ f ap.1 ap.2) D (t,p))
    (hq : Tendsto q (𝓝 t) (𝓝 p)) (hP : P t = f t p)
    (hmax : ∀ᶠ a in 𝓝 t, f a p ≤ P a ∧ P a = f a (q a) ∧ f t (q a) ≤ P t) :
    HasDerivAt P (D (1,0)) t
```
