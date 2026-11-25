# Lean formalization of Lomax existence and the exponential limit

**TASK-284 · 2026-11-25**

Lean proves all six targets for exact real arithmetic. For every nonempty
positive sample, a finite Lomax maximum exists exactly when some
$0<u<\bar u$ satisfies $\psi(u)\le0$. The weak inequality includes
an interior likelihood that ties the exponential endpoint. Without a
witness, every finite Lomax likelihood is strictly below the exponential
supremum. [Lean module](../../lean/UnivariateML/Family/Lomax.lean)

## Dossier findings

**The exact profile gap and bounded criterion agree with the dossier.**
With $S=\sum_i\log(1+\lambda x_i)$ and $A=S/n$, the maximized Lomax
log-likelihood is $n(\log\lambda-\log A-1-A)$. The exponential maximum
is $n(-\log\bar x-1)$, giving the exact gap $n(\log q-\log A-A)$
for $q=\lambda\bar x$. The additive constant that restores the full
profile from the C++ helper is $n\log n-n$, consistent with that helper's
“up to a constant” comment. The constant-data loss and the two
held-coefficient conclusions also agree.
[Lomax dossier](../../../univariateML-cpp/include/cppunivariate/distributions/lomax/README.md),
[C++ profile](../../../univariateML-cpp/include/cppunivariate/distributions/lomax/mle.hpp)

**The dossier's numerical policy differs from the job's fallback rule.**
The dossier says unresolved certificate comparisons and budget exhaustion
stop. The job requires retaining a successful search result as uncertified
when the certificate cannot close and has no proof the result is wrong.
The dossier also explicitly permits the $10^{-8}n$ exponential-stop margin
to reject a mathematical MLE that ties or narrowly improves on the
exponential value. The Lean proofs concern exact real maximization.
[Lomax dossier](../../../univariateML-cpp/include/cppunivariate/distributions/lomax/README.md),
[Wrapper failure handling](../../validation/diagnostics.md#wrapper-failure-handling-rule-r6)

The pointwise limit includes the continuous log-density extension at
$x=0$, as requested. The optimization theorems assume strictly positive
data. For the right tail the existence proof uses strict negativity
beyond the dossier's finite cutoff. The dossier's stronger
right-tail divergence statement and its numerical profile bounds are
outside these targets.
[Lomax dossier](../../../univariateML-cpp/include/cppunivariate/distributions/lomax/README.md)

## Lean statements

The declarations are copied from namespace `UnivariateML.Lomax`, with
`variable {n : ℕ}` in scope. Here `rate` is $\lambda$, `shape x rate`
is the profiled shape, `bestExp x` evaluates the exponential
log-likelihood at the reciprocal mean, `gap x q` is the
per-observation profile improvement, and `domain` consists of positive
shape and rate pairs.

### 1. Exponential limit

The first theorem proves convergence along any filter on which shape
tends to infinity and the product of shape and rate tends to a positive
rate. The second fixes the product at that rate. The third sums the
pointwise limit over the sample.

```lean
theorem logDensity_tendsto_of_mul {ι : Type*} {l : Filter ι} {κ rate : ι → ℝ}
    {r x : ℝ} (hr : 0 < r) (hx : 0 ≤ x) (hk : Tendsto κ l atTop)
    (hp : Tendsto (fun i ↦ κ i * rate i) l (𝓝 r)) :
    Tendsto (fun i ↦ logDensity (κ i) (rate i) x) l (𝓝 (expLogDensity r x))

theorem logDensity_tendsto {r x : ℝ} (hr : 0 < r) (hx : 0 ≤ x) :
    Tendsto (fun κ : ℝ ↦ logDensity κ (r / κ) x) atTop (𝓝 (expLogDensity r x))

theorem logLik_tendsto (x : Fin n → ℝ) (hx : ∀ i, 0 ≤ x i) {r : ℝ} (hr : 0 < r) :
    Tendsto (fun κ : ℝ ↦ logLik x κ (r / κ)) atTop (𝓝 (expLogLik x r))
```

### 2. Shape profile and exact gap

The reciprocal sample mean maximizes the exponential log-likelihood.
The Lomax log-likelihood is strictly concave in positive shape. At each
positive rate, the explicit shape $1/A$ is positive and strictly beats
every other positive shape. The last theorem gives the exact total
profile improvement over the best exponential.

```lean
theorem bestExp_isMaxOn (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) :
    IsMaxOn (expLogLik x) (Ioi 0) (avg x)⁻¹

theorem shape_strictConcave (hn : 0 < n) (x : Fin n → ℝ) (rate : ℝ) :
    StrictConcaveOn ℝ (Ioi (0 : ℝ)) (fun κ ↦ logLik x κ rate)

theorem shape_profile (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {rate : ℝ} (hrate : 0 < rate) :
    0 < shape x rate ∧
      ∀ κ > 0, κ ≠ shape x rate → logLik x κ rate < logLik x (shape x rate) rate

theorem profile_gap (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {rate : ℝ} (hrate : 0 < rate) :
    logLik x (shape x rate) rate - bestExp x = n * gap x (rate * avg x)
```

### 3. Lower bound for the supremum

The fixed-product path implies that every upper bound on the Lomax
log-likelihood is at least the best exponential value. The proof
establishes boundedness of the likelihood image before using the real
supremum.

```lean
theorem bestExp_le_sup (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) :
    bestExp x ≤ sSup ((fun p : ℝ × ℝ ↦ logLik x p.1 p.2) '' domain)
```

### 4. Finite maximum and unattained exponential supremum

The change of variable $q=u e^u$ covers all positive $q$. In that
coordinate, $A(q)=u+\psi(u)$, so $\psi(u)\le0$ is equivalent to a
nonnegative gap. The strict bound $\psi(u)>\log(u/\bar u)$ excludes the
entire tail $u\ge\bar u$. The normalized observations have mean one,
which gives $A'(0)=1$ and the continuous gap extension at zero. The
profile maximum on the closed interval either occurs at a positive
coordinate or has gap zero at the left endpoint. In the second case an
interior sign witness also achieves the maximum. Without a sign witness,
the second theorem gives the exact supremum and a strict inequality
at every finite positive parameter pair.

```lean
theorem exists_max_iff (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) :
    (∃ p ∈ domain, IsMaxOn (fun p : ℝ × ℝ ↦ logLik x p.1 p.2) domain p) ↔
      ∃ u ∈ Ioo 0 (uBar x), psi x u ≤ 0

theorem exponential_sup_of_no_witness (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (h : ¬ ∃ u ∈ Ioo 0 (uBar x), psi x u ≤ 0) :
    sSup ((fun p : ℝ × ℝ ↦ logLik x p.1 p.2) '' domain) = bestExp x ∧
      ∀ p ∈ domain, logLik x p.1 p.2 < bestExp x
```

### 5. Constant positive data

The first theorem gives the exact per-observation profile loss and
proves it strictly positive at every positive rate. The second rules out
a finite all-free maximum. Together with target 4, these imply the
exponential outcome for every nonempty positive constant sample.

```lean
theorem constant_profile_loss (hn : 0 < n) {c rate : ℝ} (hc : 0 < c) (hr : 0 < rate) :
    let x : Fin n → ℝ

theorem constant_no_max (hn : 0 < n) {c : ℝ} (hc : 0 < c) :
    ¬ ∃ p ∈ domain, IsMaxOn (fun p : ℝ × ℝ ↦ logLik (fun _ : Fin n ↦ c) p.1 p.2) domain p
```

### 6. Held coefficients

Holding rate gives a unique maximizing positive shape. Holding
shape, the log-rate derivative is strictly decreasing and tends to
$n$ and $-n\kappa$ at the respective endpoints. The last theorem uses
the unique score zero and strict monotonicity of the log-likelihood on
each side to obtain the unique maximizing positive rate.

```lean
theorem heldRate_unique (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {rate : ℝ} (hr : 0 < rate) :
    ∃! κ, κ ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun k ↦ logLik x k rate) (Ioi 0) κ

theorem heldShape_score_strictAnti (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {κ : ℝ} (hk : 0 < κ) :
    (∀ t, HasDerivAt (fun s ↦ logLik x κ (exp s)) (logRateScore x κ t) t) ∧
      StrictAnti (logRateScore x κ)

theorem heldShape_score_limits {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) (κ : ℝ) :
    Tendsto (logRateScore x κ) atBot (𝓝 (n : ℝ)) ∧
      Tendsto (logRateScore x κ) atTop (𝓝 (-(n : ℝ) * κ))

theorem heldShape_unique (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {κ : ℝ} (hk : 0 < κ) :
    ∃! rate, rate ∈ Ioi (0 : ℝ) ∧ IsMaxOn (logLik x κ) (Ioi 0) rate
```

## Proof effort and reuse

| Target | Effort | What drove it |
| --- | --- | --- |
| 1 | Moderate | The derivative of the logarithm at one gives the normalized limit. The moving-product proof also tracks eventual positivity and separates the zero observation. |
| 2 | Moderate | Mathlib provides strict log concavity. Finite sums and the normalization account for most of the proof work. |
| 3 | Short once boundedness is available | A convergent path gives a lower bound on every upper bound. The compact profile proof supplies boundedness for `sSup`. |
| 4 | Largest | Covering all positive rates, proving the geometric-mean cutoff, and lifting a compact profile optimum to two positive coefficients all matter. The boundary tie requires its own branch. |
| 5 | Short | The exact gap specializes by constant averaging. Applying the strict logarithm inequality at $(1+z)^{-1}$ proves positivity. |
| 6 | Moderate | Holding rate reuses the shape profile. Holding shape requires derivative and limit calculations, an intermediate-value argument, and monotonicity on either side of the root. |

The most useful layer B lemmas would lift attained fiberwise optima to
a joint optimum, transfer a limit along a feasible path to a bound on
the supremum, and turn a continuous compactification with an interior
witness tying or beating the boundary value into attainment. A reusable
score lemma would turn a continuous, strictly decreasing derivative with
opposite limiting signs into a unique global maximum. The family module
keeps these arguments local for this pilot.

## Verification and open questions

- The final `lake env lean UnivariateML/Family/Lomax.lean` passed
  without warnings on the workstation through the single-slot wrapper.
- The final `lake build` passed without warnings in the same
  environment. Lake reported `Build completed successfully (6594 jobs).`
  It rebuilt the Lomax module and library root using cached Mathlib.
- All six targets are proved. There are no `sorry`, `admit`,
  `axiom`, or `native_decide` declarations.
- Joint uniqueness remains open in the dossier. The exact criterion
  settles attainment and leaves open whether distinct positive rates
  can give tied global profile maxima. The numerical two-point
  transition and per-input floating-point certificate were outside this
  formalization.
