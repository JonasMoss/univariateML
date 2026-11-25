# Lean verification of fixed-threshold fatigue uniqueness

**TASK-310 · 2026-11-25**

For every nonempty, nonconstant positive shifted sample, Lean proves there is a
unique joint Birnbaum–Saunders MLE over positive shape and scale, with
$H<\hat\beta<(H+A)/2$. It also verifies the conditional RMS shape, the sign and
simplicity of the single profile-score root, and the constant-data divergence.
The proof in §1 of the fatigue math report is correct. [Lean module](../../lean/UnivariateML/Family/Fatigue.lean) · [Source proof](2026-11-25-fatigue-math-report.md#1-fixed-threshold-free-shape-and-scale)

## Dossier discrepancy

**The near-constant scale formula in the dossier is a leading-order
approximation written as an equality.** In the original near-constant analysis,
$\varepsilon=\log(t_3/t_1)$. Normalize the triple to $(1,1,r^3)$ with
$r=e^{\varepsilon/3}>1$. At the proposed scale $b=r$, set $k=r+r^{-1}$
and $d=(r-1)/(r+1)$. The profiled squared shape is
$Q=(k-2)(k+4)/3$, and its derivative with respect to $\log b$ at $r$,
holding the shifted data fixed, is $(r-r^{-1})(2-k)/3$. The derivative
of the profiled mean log-likelihood is therefore
$$
\frac{d(k-2)}{6k}+\frac{r-r^{-1}}{2(k+4)}>0.
$$
The unique maximizing scale lies strictly above $r$ for every positive
log gap, however small. The original analysis obtains the scale and shape
approximations by minimizing a quadratic expansion. If the dossier's
“relative amount” is instead read as $t_3/t_1-1$, its proposed scale
exceeds the arithmetic mean by $e^x>1+x$ for $x>0$, and the verified
bracket puts the MLE below the arithmetic mean. This calculation is
analytic. The Lean module verifies the general score and bracket.
[Fatigue dossier](../../../univariateML-cpp/include/cppunivariate/distributions/fatigue/README.md) · [Near-constant analysis](2026-11-15-near-constant-report.md)

## Scope and verification

Lean takes the shifted observations directly as `Fin n → ℝ` with explicit
nonemptiness and positivity. The objective is the normalized **mean**
log-likelihood. Multiplication by the positive sample size recovers the
sum with the same maximizers and score signs. The joint pair is ordered
`(scale, shape)` to match the generic profiling interface. The centered
reciprocal variance proves the strict form of the source's Cauchy–Schwarz
step. For the scale maximum, the proof applies the intermediate value
theorem to `F` and uses `G` to propagate the signs across the domain.
The generic score theorem assumes monotonicity of the actual derivative;
here the monotone function is a sign-equivalent rescaling. The two generic
profiling lemmas transfer the scale maximum and the fiber uniqueness to
the joint problem.

Both final Lean commands exited successfully on the workstation, through
`/tmp/umvl-cpu/run` with `nice -n 10` and one Lean thread.

- `lake env lean UnivariateML/Family/Fatigue.lean` passed without warnings.
- `lake build UnivariateML +UnivariateML.Family.Fatigue` reported
  `Build completed successfully (6692 jobs).` The explicit module target
  compiled Fatigue ahead of the coordinator's root import. The build
  replayed existing line-length warnings in Weibull and a tactic-linter
  warning in GPD. Fatigue built without warnings and used the existing
  Mathlib cache.

All five targets are closed. The module contains no `sorry`, `admit`,
`axiom` or `native_decide`. The root import is left to the coordinator.
The theorem concerns exact arithmetic. Floating-point coefficient
certification and the free-threshold and held-shape routes lie outside
the scope of TASK-310.

## Checked Lean statements

### 1. Conditional shape and density fidelity

The first statement identifies the objective with the dossier's density.
The next statements express the shape formula as a mean of squared
standardized residuals and as the dossier's RMS. For nonconstant data the
square is positive at every positive scale. The held-scale theorem also
applies to constant samples away from their common value. It certifies
strict improvement over every other positive shape whenever the square
is positive.

Effort was moderate. Finite-sum linearity and the two residual forms
required explicit algebra. A single strict logarithm inequality then
gave the conditional maximum.

```lean
theorem logLik_eq_avg_logDensity (hn : 0 < n) (t : Fin n → ℝ) {a : ℝ}
    (ha : 0 < a) (b : ℝ) :
    logLik t a b = avg (fun i ↦ logDensity a b (t i))

theorem shapeSq_eq_mean_square (hn : 0 < n) {t : Fin n → ℝ} (ht : ∀ i, 0 < t i)
    {b : ℝ} (hb : 0 < b) :
    shapeSq t b = avg (fun i ↦ (t i - b) ^ 2 / (t i * b))

theorem shape_eq_rms (hn : 0 < n) {t : Fin n → ℝ} (ht : ∀ i, 0 < t i)
    {b : ℝ} (hb : 0 < b) :
    shape t b = sqrt (avg (fun i ↦ (sqrt (t i / b) - sqrt (b / t i)) ^ 2))

theorem shapeSq_pos_iff (hn : 0 < n) {t : Fin n → ℝ} (ht : ∀ i, 0 < t i)
    {b : ℝ} (hb : 0 < b) :
    0 < shapeSq t b ↔ ∃ i, t i ≠ b

theorem shapeSq_pos (hn : 0 < n) {t : Fin n → ℝ} (ht : ∀ i, 0 < t i)
    (hne : ∃ i j, t i ≠ t j) {b : ℝ} (hb : 0 < b) : 0 < shapeSq t b

theorem heldScale_unique (t : Fin n → ℝ) {b : ℝ} (hq : 0 < shapeSq t b) :
    0 < shape t b ∧ (shape t b) ^ 2 = shapeSq t b ∧
      ∀ a > 0, a ≠ shape t b → logLik t a b < profile t b
```

### 2. Profile derivative

The derivative equals the reported numerator times an explicitly positive
factor. Consequently it has the same sign and the same zeros as `F`
on the positive scale domain.

Effort was moderate. The main steps were differentiating the simplified
profile on a positive neighborhood and checking the common-denominator
identity in Lean.

```lean
theorem hasDerivAt_profile (hn : 0 < n) {t : Fin n → ℝ} (ht : ∀ i, 0 < t i)
    (hne : ∃ i j, t i ≠ t j) {b : ℝ} (hb : 0 < b) :
    HasDerivAt (profile t)
      (reciprocalMean t b / (H t * b * shapeSq t b) * F t b) b

theorem scale_score_factor_pos (hn : 0 < n) {t : Fin n → ℝ} (ht : ∀ i, 0 < t i)
    (hne : ∃ i j, t i ≠ t j) {b : ℝ} (hb : 0 < b) :
    0 < reciprocalMean t b / (H t * b * shapeSq t b)
```

### 3. Shifted harmonic mean and decreasing rescaled score

The reciprocal moment ratio is the derivative of the shifted harmonic
mean. For distinct observations it exceeds one, which gives the strict
linear bound from zero. The rescaled score has the reported derivative,
strictly negative above the harmonic mean.

Effort was moderate. The strict inequality needed a positive summand
in the centered second moment, followed by a finite-sum expansion.

```lean
theorem hasDerivAt_xi (hn : 0 < n) {t : Fin n → ℝ} {b : ℝ}
    (ht : ∀ i, 0 < t i + b) : HasDerivAt (xi t) (momentRatio t b) b

theorem xi_deriv_gt_one (hn : 0 < n) {t : Fin n → ℝ} {b : ℝ}
    (ht : ∀ i, 0 < t i + b) (hne : ∃ i j, t i ≠ t j) : 1 < deriv (xi t) b

theorem xi_gt_add_H (hn : 0 < n) {t : Fin n → ℝ} (ht : ∀ i, 0 < t i)
    (hne : ∃ i j, t i ≠ t j) {b : ℝ} (hb : 0 < b) : b + H t < xi t b

theorem hasDerivAt_G (hn : 0 < n) {t : Fin n → ℝ} (ht : ∀ i, 0 < t i)
    {b : ℝ} (hb : H t < b) :
    HasDerivAt (G t)
      (-(H t * (A t - H t)) / (b - H t) ^ 2 + 1 - momentRatio t b) b

theorem G_deriv_neg (hn : 0 < n) {t : Fin n → ℝ} (ht : ∀ i, 0 < t i)
    (hne : ∃ i j, t i ≠ t j) {b : ℝ} (hb : H t < b) : deriv (G t) b < 0
```

### 4. Joint MLE and bracket

The scale score has one positive root in the strict bracket, with
opposite signs on its two sides. Its derivative as a numerator is
negative at that root. The joint theorem gives a feasible maximizing
pair in the bracket and equates every feasible joint maximizer to it.

This target had the most proof steps. The endpoint signs and the strict
decrease of `G` give the root and its sign pattern. The mean-value
theorem gives strict profile improvement toward the root from either
side. Generic profiling handles the joint comparison.

```lean
theorem scale_root_signs (hn : 0 < n) {t : Fin n → ℝ} (ht : ∀ i, 0 < t i)
    (hne : ∃ i j, t i ≠ t j) :
    ∃ b ∈ Ioo (H t) ((H t + A t) / 2), F t b = 0 ∧
      (∀ s ∈ Ioo 0 b, 0 < F t s) ∧ (∀ s ∈ Ioi b, F t s < 0)

theorem F_deriv_neg_at_root (hn : 0 < n) {t : Fin n → ℝ} (ht : ∀ i, 0 < t i)
    (hne : ∃ i j, t i ≠ t j) {b : ℝ} (hb : H t < b) (hz : F t b = 0) :
    deriv (F t) b < 0

theorem jointMLE (hn : 0 < n) {t : Fin n → ℝ} (ht : ∀ i, 0 < t i)
    (hne : ∃ i j, t i ≠ t j) :
    ∃ p : ℝ × ℝ, p ∈ domain ∧ IsMaxOn (fun q ↦ logLik t q.2 q.1) domain p ∧
      p.1 ∈ Ioo (H t) ((H t + A t) / 2) ∧
      ∀ q ∈ domain, IsMaxOn (fun r ↦ logLik t r.2 r.1) domain q → q = p
```

### 5. Constant samples and held-scale collapse

The RMS square is zero precisely when every observation equals the
held scale. At zero RMS the held-scale mean log-likelihood diverges to
positive infinity as shape approaches zero from above. The constant
sample at its common scale is the joint problem's feasible diverging path.

Effort was small after proving the residual identity. At zero RMS the
shape dependence reduces to the negative logarithm.

```lean
theorem shapeSq_zero_iff (hn : 0 < n) {t : Fin n → ℝ} (ht : ∀ i, 0 < t i)
    {b : ℝ} (hb : 0 < b) :
    shapeSq t b = 0 ↔ ∀ i, t i = b

theorem heldScale_unbounded (t : Fin n → ℝ) {b : ℝ} (hq : shapeSq t b = 0) :
    Tendsto (fun a ↦ logLik t a b) (𝓝[>] 0) atTop

theorem constant_unbounded (hn : 0 < n) {c : ℝ} (hc : 0 < c) :
    Tendsto (fun a ↦ logLik (fun _ : Fin n ↦ c) a c) (𝓝[>] 0) atTop
```
