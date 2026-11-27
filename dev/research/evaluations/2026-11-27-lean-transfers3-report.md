# TASK-381 · Exact transfers for held Pareto III, four-parameter beta and beta prime fits

All three held-fit targets are proved in [Transfers3.lean](../../lean/UnivariateML/Family/Transfers3.lean). Each statement uses a finite real sample indexed by `Fin n`. Fit claims explicitly require a nonempty sample and strict interior support. The results are in exact real arithmetic.

## Dossier findings

- **Pareto III.** The existence block’s sentence on constant excesses needs its parameter scope made explicit. The likelihood is unbounded when shape and scale are free or when shape is free and the held scale equals the common excess. A held positive shape always gives a unique finite scale. With a held scale different from the common excess, a constant sample gives a unique finite shape. The Details section already gives these distinctions. The existence block also ends by attributing the conditional conclusions to strict concavity. Strict concavity supplies uniqueness, and the log-logistic source supplies attainment on the eligible slices. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/pareto3/README.md)
- **Four-parameter beta.** The existence block and Details agree on the held-endpoint claims, including the one-held-shape fits for constant data. The Details sentence on attainment cites strict concavity alone. The existence block also invokes interiority of the mean sufficient statistic, and Lean obtains attainment from coercivity in the beta source. The top-level `uniqueness: unknown` has wider scope than the held-endpoint result. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/nsbeta/README.md)
- **Beta prime.** The existence block states the held-scale existence criterion, and Details explicitly gives the fixed-scale concentration escape for constant data with both shapes free. That divergence could be stated directly in the held-scale portion of the existence summary. The Details paragraph starting with a held scale then discusses estimation of scale with both shapes held. That sentence uses a free scale, as the existence block makes clear. The top-level `uniqueness: unknown` has wider scope than the held-scale result. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/betapr/README.md)

## Scope and open items

The formal statements use total log likelihood. They present the one-free-parameter cases as unique global maximizers. Positive held shapes, positive held scales and ordered endpoints are explicit where relevant. The affine and fractional-linear transformations preserve the distinction between constant and nonconstant samples. Unboundedness is stated separately so the constant-sample direction of each existence criterion also rules out a finite tied maximum.

No requested target remains open. Free thresholds, free endpoints, beta-prime fits with free scale, fully held likelihood evaluation and floating-point certification are outside this job.

## Lean verification

Both final commands ran on the workstation from `dev/lean/` under the single-slot lock and `nice -n 10`.

- `lake env lean UnivariateML/Family/Transfers3.lean` passed without diagnostics.
- `lake build UnivariateML +UnivariateML.Family.Transfers3` completed successfully (7464 jobs). The explicit module target includes the new file while the aggregate import awaits the coordinator. The new module built without diagnostics. The aggregate build emitted warnings in existing modules.

The new module contains no proof placeholders or added axioms.

## Pareto III

The main work was checking the dossier’s parameterization against the log-logistic formula and scoping both constant-data cases. The joint and held-slice fits use the corresponding log-logistic theorems.

### `UnivariateML.ParetoIII.logLik_transfer`

```lean
theorem logLik_transfer (x : Fin n → ℝ) (μ : ℝ) (p : ℝ × ℝ) :
    logLik x μ p = LogLogistic.logLik (excess x μ) p
```

Subtracting the held threshold preserves the total log likelihood and leaves shape and scale in their original coordinates.

### `UnivariateML.ParetoIII.joint_unbounded`

```lean
theorem joint_unbounded (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ}
    (hx : ∀ i, μ < x i) (h : ¬ ∃ i j, x i ≠ x j) :
    ¬ BddAbove (logLik x μ '' LogLogistic.domain)
```

For a constant sample strictly above the held threshold, the joint shape and scale log likelihood is unbounded above.

### `UnivariateML.ParetoIII.jointMLE_iff`

```lean
theorem jointMLE_iff (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ} (hx : ∀ i, μ < x i) :
    (∃! p, p ∈ LogLogistic.domain ∧ IsMaxOn (logLik x μ) LogLogistic.domain p) ↔
      ∃ i j, x i ≠ x j
```

With location held, the free shape and scale have a unique finite MLE exactly when two observations differ.

### `UnivariateML.ParetoIII.heldShape_unique`

```lean
theorem heldShape_unique (hn : 0 < n) {x : Fin n → ℝ} {μ γ : ℝ}
    (hx : ∀ i, μ < x i) (hγ : 0 < γ) :
    ∃! σ, σ ∈ Ioi 0 ∧ IsMaxOn (fun s ↦ logLik x μ (γ, s)) (Ioi 0) σ
```

With location and a positive shape held, every nonempty interior sample has a unique fitted scale.

### `UnivariateML.ParetoIII.heldScale_unbounded`

```lean
theorem heldScale_unbounded (hn : 0 < n) {x : Fin n → ℝ} {μ σ : ℝ}
    (hσ : 0 < σ) (h : ∀ i, x i - μ = σ) :
    ¬ BddAbove ((fun γ ↦ logLik x μ (γ, σ)) '' Ioi 0)
```

With location and scale held, the free-shape log likelihood is unbounded when every excess equals that scale.

### `UnivariateML.ParetoIII.heldScale_unique_iff`

```lean
theorem heldScale_unique_iff (hn : 0 < n) {x : Fin n → ℝ} {μ σ : ℝ}
    (hx : ∀ i, μ < x i) (hσ : 0 < σ) :
    (∃! γ, γ ∈ Ioi 0 ∧ IsMaxOn (fun k ↦ logLik x μ (k, σ)) (Ioi 0) γ) ↔
      ∃ i, x i - μ ≠ σ
```

With location and a positive scale held, one finite shape maximizes the likelihood exactly when some excess differs from that scale.


## Four-parameter beta

The main work was proving interior support, injectivity and the affine change-of-variables identity. This target also introduced the private beta transfer proofs reused below. Beta supplies the attainment, uniqueness and concentration arguments.

### `UnivariateML.NonstandardBeta.logLik_transfer`

```lean
theorem logLik_transfer (x : Fin n → ℝ) {a b : ℝ} (hab : a < b) (p : ℝ × ℝ) :
    logLik x a b p = Beta.logLik (unitData x a b) p + -((n : ℝ) * log (b - a))
```

The affine transformation to the unit interval subtracts one log width per observation from the transformed beta log likelihood.

### `UnivariateML.NonstandardBeta.jointMLE_iff`

```lean
theorem jointMLE_iff (hn : 0 < n) {x : Fin n → ℝ} {a b : ℝ} (hab : a < b)
    (hx : ∀ i, x i ∈ Ioo a b) :
    (∃! p, p ∈ Beta.domain ∧ IsMaxOn (logLik x a b) Beta.domain p) ↔ ∃ i j, x i ≠ x j
```

With both endpoints held, the two shapes have a unique finite MLE exactly when two original observations differ.

### `UnivariateML.NonstandardBeta.joint_unbounded`

```lean
theorem joint_unbounded (hn : 0 < n) {x : Fin n → ℝ} {a b : ℝ} (hab : a < b)
    (hx : ∀ i, x i ∈ Ioo a b) (h : ¬ ∃ i j, x i ≠ x j) :
    ¬ BddAbove (logLik x a b '' Beta.domain)
```

Constant interior data give unbounded likelihood when both shapes are free.

### `UnivariateML.NonstandardBeta.heldFirst`

```lean
theorem heldFirst (hn : 0 < n) {x : Fin n → ℝ} {a b h : ℝ} (hab : a < b)
    (hx : ∀ i, x i ∈ Ioo a b) (hh : 0 < h) :
    ∃! q, q ∈ Ioi 0 ∧ IsMaxOn (fun z ↦ logLik x a b (h, z)) (Ioi 0) q
```

Holding the first shape at any positive value leaves a unique finite estimate of the second shape, including for constant data.

### `UnivariateML.NonstandardBeta.heldSecond`

```lean
theorem heldSecond (hn : 0 < n) {x : Fin n → ℝ} {a b h : ℝ} (hab : a < b)
    (hx : ∀ i, x i ∈ Ioo a b) (hh : 0 < h) :
    ∃! p, p ∈ Ioi 0 ∧ IsMaxOn (fun z ↦ logLik x a b (z, h)) (Ioi 0) p
```

Holding the second shape at any positive value leaves a unique finite estimate of the first shape, including for constant data.


## Beta prime

The main work was the fractional-linear log-density algebra and proving injectivity by cross multiplication at positive scale. The common beta transfer then supplied the joint and held-shape conclusions.

### `UnivariateML.BetaPrime.logLik_transfer`

```lean
theorem logLik_transfer {x : Fin n → ℝ} {σ : ℝ} (hσ : 0 < σ)
    (hx : ∀ i, 0 < x i) (p : ℝ × ℝ) :
    logLik x σ p = Beta.logLik (unitData x σ) p + logJacobian x σ
```

At fixed positive scale, the map x ↦ x/(σ + x) adds the total of the log Jacobians log(σ/(σ + x)²) to the transformed beta log likelihood.

### `UnivariateML.BetaPrime.jointMLE_iff`

```lean
theorem jointMLE_iff (hn : 0 < n) {x : Fin n → ℝ} {σ : ℝ} (hσ : 0 < σ)
    (hx : ∀ i, 0 < x i) :
    (∃! p, p ∈ Beta.domain ∧ IsMaxOn (logLik x σ) Beta.domain p) ↔ ∃ i j, x i ≠ x j
```

At fixed scale, the two shapes have a unique finite MLE exactly when two original observations differ.

### `UnivariateML.BetaPrime.joint_unbounded`

```lean
theorem joint_unbounded (hn : 0 < n) {x : Fin n → ℝ} {σ : ℝ} (hσ : 0 < σ)
    (hx : ∀ i, 0 < x i) (h : ¬ ∃ i j, x i ≠ x j) :
    ¬ BddAbove (logLik x σ '' Beta.domain)
```

Constant positive data give unbounded likelihood at fixed scale when both shapes are free.

### `UnivariateML.BetaPrime.heldFirst`

```lean
theorem heldFirst (hn : 0 < n) {x : Fin n → ℝ} {σ h : ℝ} (hσ : 0 < σ)
    (hx : ∀ i, 0 < x i) (hh : 0 < h) :
    ∃! q, q ∈ Ioi 0 ∧ IsMaxOn (fun z ↦ logLik x σ (h, z)) (Ioi 0) q
```

With positive scale and the first shape held, the second shape has one finite MLE, including for constant data.

### `UnivariateML.BetaPrime.heldSecond`

```lean
theorem heldSecond (hn : 0 < n) {x : Fin n → ℝ} {σ h : ℝ} (hσ : 0 < σ)
    (hx : ∀ i, 0 < x i) (hh : 0 < h) :
    ∃! p, p ∈ Ioi 0 ∧ IsMaxOn (fun z ↦ logLik x σ (z, h)) (Ioi 0) p
```

With positive scale and the second shape held, the first shape has one finite MLE, including for constant data.
