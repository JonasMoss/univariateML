# Lean formalization of GPD unboundedness, scale uniqueness, and the exact joint tie

**TASK-288 · 2026-11-25**

Lean proves all five targets in exact real arithmetic. The three-point sample
in the dossier has two distinct global maximizers with total log-likelihood
zero. For every nonempty sample of positive excesses, the shape constraint
$\xi\ge-1$ gives an attained joint maximum. [Lean module](../../lean/UnivariateML/Family/GPD.lean)

## Dossier findings and proof choices

The checked mathematical claims agree with the dossier, including the
attained uniform endpoint and the exact joint tie. The dossier's arithmetic
wording says unresolved certificate comparisons stop. TASK-288 instead
specifies retaining a successful candidate with an uncertified flag when
the certificate cannot settle the comparison and has no proof that the
candidate is wrong. The Lean statements concern exact likelihood
maximization. [GPD dossier](../../../univariateML-cpp/include/cppunivariate/distributions/gpd/README.md),
[C++ parameterization](../../../univariateML-cpp/include/cppunivariate/distributions/gpd/mle.hpp),
[Wrapper failure handling](../../validation/diagnostics.md#wrapper-failure-handling-rule-r6)

- **Free threshold.** The proof uses $\mu=\min_i x_i$, $\sigma=e^{-t}$
  and the fixed shape $\xi=2n$. One minimum observation contributes $t$ and
  the other terms together contribute at least $-t/2$ minus a sample-dependent
  constant. The dossier takes $\xi=t$ along its path.
- **Held shape below the floor.** At maximum excess $M$, the scale
  $\sigma=-\xi M+e^{-t}$ keeps the endpoint strictly above the sample.
  The logarithm of the support factor at $M$ is $-t-\log\sigma$, which
  makes its contribution diverge.
- **Joint tie.** An exact comparison at $k=1/2$ proves
  $a(1/2)>7/100>a(0)>0$. It uses Mathlib's rational bounds on $e$.
  This replaces the dossier's test using $a'(0)$. Finite tail exclusions
  give compactness. Near $-1$ the function is negative, and for sufficiently
  large $k$ it is below $a(0)$. These exclusions suffice for attainment.
  For a feasible nonuniform pair at $b=\xi/\sigma$, choose $k$ with
  $b=b(k)$ and set $\tau=e^{-1-k}$. Monotonicity in the smaller excess gives
  $D(a_*,b)\ge\tau$. The inequality $\log(\tau/\sigma)\le\tau/\sigma-1$
  then bounds the mean log-likelihood by
  $-\log\tau-1-b\tau=0$. The proof compares the actual likelihood at each
  scale, including scales whose unconstrained conditional optima would lie
  below the shape floor.
- **General joint attainment.** The proof uses the profile coordinate
  $v=\xi/\sigma$ and the mean divided logarithm $D(v)$. This coordinate
  agrees with the dossier's $b$ when the maximum excess is one. With maximum
  excess $M$ and minimum excess $m$, the proof restricts the conditional
  candidates to $[(e^{-n}-1)/M,\,(e^{M/m+1}-1)/m]$ with $vD(v)\ge-1$.
  The left tail violates the conditional shape floor and the right tail
  has a profile bound from the minimum excess. Both tails are dominated
  by the uniform member at scale $M$. The continuous profile attains its
  maximum on the remaining compact set. The proof uses this comparison
  in place of compactness in the original parameters via the density envelope.

## Lean statements

These declarations are in namespace `UnivariateML.GPD`, with
`variable {n : ℕ}` in scope. The sample `y` consists of excesses
over the threshold. The set `scaleDomain y ξ` requires positive scale,
nonnegative excesses, and finite log-likelihood support, including the
closed uniform upper endpoint.

### 1. Free-threshold unboundedness

For any nonempty sample, an observed threshold admits a feasible scale and shape above every prescribed log-likelihood level.

```lean
theorem freeThreshold_unbounded (hn : 0 < n) (x : Fin n → ℝ) :
    ∃ μ, (∃ j, μ = x j) ∧ ∀ C : ℝ, ∃ σ ξ,
      σ ∈ scaleDomain (fun i ↦ x i - μ) ξ ∧
        C < logLik (fun i ↦ x i - μ) σ ξ
```

### 2. Unboundedness below shape −1

For any held shape below −1 and any nonempty positive sample, feasible scales yield arbitrarily large log-likelihood.

```lean
theorem shapeBelow_unbounded (hn : 0 < n) {y : Fin n → ℝ} (hy : ∀ i, 0 < y i)
    {ξ : ℝ} (hξ : ξ < -1) (C : ℝ) :
    ∃ σ ∈ scaleDomain y ξ, C < logLik y σ ξ
```

### 3. Held-shape scale maximizers

Above shape −1, the scaled scale score is continuous and strictly decreasing on the feasible interval. It crosses zero once, giving a unique scale maximizer. At shape −1, the sample maximum is feasible and is the unique scale maximizer.

```lean
theorem heldShape_scale_unique (hn : 0 < n) {y : Fin n → ℝ} (hy : ∀ i, 0 < y i)
    {ξ : ℝ} (hξ : -1 < ξ) :
    ∃! σ, σ ∈ scaleDomain y ξ ∧
      IsMaxOn (fun s ↦ logLik y s ξ) (scaleDomain y ξ) σ

theorem uniform_scale_unique (hn : 0 < n) {y : Fin n → ℝ} (hy : ∀ i, 0 < y i) :
    ∃ j, (∀ i, y i ≤ y j) ∧
      ∀ σ, (σ ∈ scaleDomain y (-1) ∧
        IsMaxOn (fun s ↦ logLik y s (-1)) (scaleDomain y (-1)) σ) ↔ σ = y j
```

### 4. Exact joint tie

The tie-construction function is continuous across zero and throughout the interval. It attains a strictly positive maximum below one at a nonzero shape. For the resulting three excesses, the interior and uniform parameter pairs are feasible, both give zero, and every feasible pair at shape at least −1 gives at most zero.

```lean
theorem a_continuous : ContinuousOn a (Ioi (-1 : ℝ))

theorem a_attains_max : ∃ k ∈ Ioi (-1 : ℝ),
    k ≠ 0 ∧ a k ∈ Ioo (0 : ℝ) 1 ∧ IsMaxOn a (Ioi (-1)) k

theorem exact_joint_tie :
    ∃ k ∈ Ioi (-1 : ℝ), k ≠ 0 ∧ a k ∈ Ioo (0 : ℝ) 1 ∧
      IsMaxOn a (Ioi (-1)) k ∧
      exp (-1 - k) ∈ scaleDomain (tieSample (a k)) k ∧
      (1 : ℝ) ∈ scaleDomain (tieSample (a k)) (-1) ∧
      logLik (tieSample (a k)) (exp (-1 - k)) k = 0 ∧
      logLik (tieSample (a k)) 1 (-1) = 0 ∧
      ∀ ξ ≥ -1, ∀ σ ∈ scaleDomain (tieSample (a k)) ξ,
        logLik (tieSample (a k)) σ ξ ≤ 0
```

### 5. General joint attainment

Every nonempty positive sample has a feasible pair that maximizes the likelihood jointly under the shape floor.

```lean
theorem joint_exists (hn : 0 < n) {y : Fin n → ℝ} (hy : ∀ i, 0 < y i) :
    ∃ σ ξ, -1 ≤ ξ ∧ σ ∈ scaleDomain y ξ ∧
      ∀ κ ≥ -1, ∀ s ∈ scaleDomain y κ, logLik y s κ ≤ logLik y σ ξ
```

## Proof effort

| Target | Effort | Main work |
| --- | --- | --- |
| 1 | Moderate | Track the sample minimum and separate one exact contribution from the finite-sum bound. |
| 2 | Moderate | Establish strict support along the path and bound the endpoint term and remaining terms separately. |
| 3 | Substantial | Differentiate the likelihood on its feasible scale interval, bracket the score separately for negative and nonnegative shape, and lift the derivative signs to a strict maximum. The uniform case follows from monotonicity of the logarithm. |
| 4 | Largest | Prove continuity at zero, establish the exact comparison at one half and compact attainment, and bound every feasible scale using the profile-coordinate identity. |
| 5 | Substantial | Extend the divided logarithm continuously at zero, bound both profile tails, and lift the compact profile maximum or the uniform member back to the original parameters. |

## Verification and open items

- The final workstation run of `lake env lean UnivariateML/Family/GPD.lean`
  passed without warnings through the single-slot wrapper.
- The final workstation `lake build` passed without warnings through the
  same wrapper and reported `Build completed successfully (6594 jobs).`
  It built the Lomax module and the existing library root using cached Mathlib.
  The standalone command checks GPD directly. The coordinator owns the
  GPD import at the library root.
- All five targets are proved. The module has zero `sorry`, `admit`,
  `axiom`, or `native_decide` declarations.
