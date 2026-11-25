# Cauchy location–scale MLE in Lean

**TASK-311 · 2026-11-25 · base `ea15eecd`**

The joint MLE exists uniquely exactly when every atom has strictly less than
half the sample mass. Equal masses at two distinct values give the full
upper semicircle of joint maximizers. An unbalanced half-mass atom gives a
finite unattained supremum. A strict majority gives divergence. The two
held-parameter cases are also verified.
[Lean module](../../lean/UnivariateML/Family/Cauchy.lean)

## Discrepancies with the brief and dossier

- The brief's shorthand that an atom of at least half the mass gives no
  finite maximizer needs the balanced two-point exception. The dossier's
  existence statement includes it, and the Lean statements recover the
  entire semicircle.
- The dossier's `limits` entry uses `NoMLE` for the collapse detector,
  which includes equality at half mass. The fitter uses `Unattained` at
  equality outside the balanced exception and `NoMLE` above half mass.
- The held-location summary describes a strictly decreasing score. The
  earlier existence argument correctly requires a nonzero residual for
  strict decrease of the score. If every residual vanishes, the log-scale
  score is the constant `-n` and the log-likelihood still strictly decreases.

[Cauchy dossier](../../../univariateML-cpp/include/cppunivariate/distributions/cauchy/README.md) ·
[Cauchy fitter](../../../univariateML-cpp/include/cppunivariate/distributions/cauchy/mle.hpp)

## Verification

All runs finished on the workstation through `/tmp/umvl-cpu/run`, with
`nice -n 10` and the existing Mathlib cache. The standalone
`lake env lean UnivariateML/Family/Cauchy.lean` check passed without
diagnostics. The explicit `lake build +UnivariateML.Family.Cauchy` target
passed without warnings and reported `Build completed successfully (6599 jobs).`
The final default `lake build` reported
`Build completed successfully (6690 jobs).` It replayed the pre-existing
line-length warnings in Weibull and flexible-tactic warning in GPD.
The explicit target covers Cauchy while the coordinator handles the root import.

The module contains no `sorry`, `admit`, `axiom` or `native_decide`.
No proof obligations remain for the four targets. The theorems cover the
exact real likelihood. The floating-point search, score gates, and
coefficient-enclosure claims are outside their scope.

## Statements and evidence

The statements are copied from `UnivariateML.Cauchy`. The sample is
`Fin n → ℝ`, the joint domain is `univ ×ˢ Ioi 0`, and
`multiplicity x a` counts observations equal to `a`. The log-density is
`log s - log π - log ((m - y)^2 + s^2)`, and `logLik` sums it.

### 1. Held positive scale

Each location strictly outside the sample range improves when moved to the
nearer endpoint. Continuity gives a minimum for the negative log-likelihood
on the closed interval, and the global argmax is a closed level set inside
that interval. For two equally frequent values, the two-factor product
reduces to a square in the squared distance from the midpoint. This extra
calculation gives the exact argmax for every held scale in the balanced case.

`heldScale_argmax` says the full maximizing set is nonempty, compact and
identical to the maximizing set on the sample range. `heldScale_exists`
supplies attainment without requiring the caller to choose extreme
observations.

```lean
theorem heldScale_argmax (hn : 0 < n) (x : Fin n → ℝ) {s : ℝ} (hs : 0 < s)
    {i j : Fin n} (hi : ∀ k, x i ≤ x k) (hj : ∀ k, x k ≤ x j) :
    let A := {m | IsMaxOn (fun t ↦ logLik x t s) univ m}
    A.Nonempty ∧ IsCompact A ∧
      A = {m ∈ Icc (x i) (x j) | IsMaxOn (fun t ↦ logLik x t s) (Icc (x i) (x j)) m}

theorem heldScale_exists (hn : 0 < n) (x : Fin n → ℝ) {s : ℝ} (hs : 0 < s) :
    ∃ m, IsMaxOn (fun t ↦ logLik x t s) univ m
```

`balanced_heldScale_argmax` describes the entire maximizing set by its
squared distance from the midpoint. It gives two locations below half the
gap and just the midpoint at or above half the gap. The fitter tracks
isolated balanced ties when the numerical search resolves them.

```lean
theorem balanced_heldScale_argmax (hn : 0 < n) (x : Fin n → ℝ) {a b s : ℝ} (hab : a < b)
    (ha : 2 * multiplicity x a = n) (hx : ∀ i, x i = a ∨ x i = b) (hs : 0 < s) :
    {m | IsMaxOn (fun t ↦ logLik x t s) univ m} =
      {m | (m - (a + b) / 2)^2 = max 0 (((b - a) / 2)^2 - s^2)}
```

### 2. Held location

Differentiating in log scale gives a sum of terms of the form
`2 * r² / (r² + exp(t)²) - 1`, where `r` is a residual. The
two endpoint limits are `n - 2*k` and `-n`, where `k` is the
multiplicity at the held location. The proof supplies the derivative,
strict score decrease when a nonzero residual exists, and the two limits
to `Generic.existsUnique_log_score_zero`. For the other side of the
threshold, the proof shows a negative score throughout, including the
constant-score case.

`heldLocation_unique` identifies the score root with the unique
maximizing scale. `heldLocation_noMLE` proves strict likelihood decrease
and no positive maximizer at or above half mass. `heldLocation_iff`
gives the threshold as a single equivalence.

```lean
theorem heldLocation_unique (hn : 0 < n) (x : Fin n → ℝ) (m : ℝ)
    (hm : 2 * multiplicity x m < n) :
    (∃! s, 0 < s ∧ heldScore x m (log s) = 0) ∧
      ∀ s > 0, IsMaxOn (logLik x m) (Ioi 0) s ↔ heldScore x m (log s) = 0

theorem heldLocation_noMLE (hn : 0 < n) (x : Fin n → ℝ) (m : ℝ)
    (hm : n ≤ 2 * multiplicity x m) :
    StrictAntiOn (logLik x m) (Ioi 0) ∧
      ¬ ∃ s ∈ Ioi (0 : ℝ), IsMaxOn (logLik x m) (Ioi 0) s

theorem heldLocation_iff (hn : 0 < n) (x : Fin n → ℝ) (m : ℝ) :
    (∃! s, s ∈ Ioi (0 : ℝ) ∧ IsMaxOn (logLik x m) (Ioi 0) s) ↔
      2 * multiplicity x m < n
```

### 3. Joint fit under strict half mass

Attainment and uniqueness required separate estimates. For attainment,
choose a positive distance `d ≤ 1` at most half the gap between any
two distinct sample values. At a location within `d` of a sample value,
all other sample values are at least `d` away. If the location is outside
those neighborhoods, all residuals are at least `d` in magnitude. In
either case the strict mass bound gives a uniform lower bound
`n * log(d²) - log(s)` for the sum of log reciprocal-density factors at
small scale. The reciprocal factor `((m-y)²+s²)/s` is at least both
`s` and `|m-y|`, which gives the large-scale and far-location cutoffs.
Mathlib's compact minimum theorem on the resulting rectangle gives a
joint maximizer.

Uniqueness uses the determinant-normalized sum of the two quadratic
forms representing any two feasible pairs. Write `Qp`, `Qw`, and
`Qz` for the reciprocal-density factors at the original pairs and the
resulting midpoint. The proof obtains
`K² * (Qp(y) * Qw(y) - Qz(y)²) = P(y)²`, with `K > 0` and
`P` quadratic or linear. If the two pairs are distinct, the polynomial
cannot vanish at three distinct observations. Strict half mass guarantees
three distinct values, so the midpoint log-likelihood exceeds the mean
of the two original log-likelihoods. This excludes two maximizers.

`jointMLE` is the main existence and uniqueness theorem.
`joint_exists_iff` gives both ways to attain a joint maximum.
`jointMLE_iff` excludes uniqueness in the balanced case by constructing
two points with different locations on the maximizing semicircle.

```lean
theorem jointMLE (hn : 0 < n) (x : Fin n → ℝ) (hx : ∀ a, 2 * multiplicity x a < n) :
    ∃! p : ℝ × ℝ, p ∈ domain ∧ IsMaxOn (fun w ↦ logLik x w.1 w.2) domain p

theorem joint_exists_iff (hn : 0 < n) (x : Fin n → ℝ) :
    (∃ p ∈ domain, IsMaxOn (fun w ↦ logLik x w.1 w.2) domain p) ↔
      (∀ a, 2 * multiplicity x a < n) ∨
        ∃ a b, a ≠ b ∧ 2 * multiplicity x a = n ∧ ∀ i, x i = a ∨ x i = b

theorem jointMLE_iff (hn : 0 < n) (x : Fin n → ℝ) :
    (∃! p : ℝ × ℝ, p ∈ domain ∧ IsMaxOn (fun w ↦ logLik x w.1 w.2) domain p) ↔
      ∀ a, 2 * multiplicity x a < n
```

### 4. Collapse and the balanced exception

The collapse calculation splits the log-likelihood into
`(n - 2*k) * log(s)` and a remainder with a finite limit. At half mass,
the number of observations away from the atom equals the number at it.
Each off-atom reciprocal factor can therefore be multiplied by the
atom's factor. The product equals the squared observation gap plus
`(((m-a)*(m-xᵢ)+s²)/s)²`. The square gives the global inequality and its
equality condition. `Generic.limit_le_upperBound` uses the feasible
collapse path to identify the least upper bound.

For the maximizing-path result, each nonnegative log penalty is bounded
by the total gap to the supremum, so each penalty and each square
vanish in the limit. Subtracting two distinct off-atom residual
expressions forces `(m-a)/s → 0`, and substituting back yields `s → 0`
and `m → a`.

`majority_collapse` gives divergence to positive infinity in log
likelihood, and `majority_no_jointMLE` rules out any feasible maximizer.

```lean
theorem majority_collapse (x : Fin n → ℝ) {a : ℝ} (ha : n < 2 * multiplicity x a) :
    Tendsto (fun s ↦ logLik x a s) (𝓝[>] 0) atTop

theorem majority_no_jointMLE (x : Fin n → ℝ) {a : ℝ} (ha : n < 2 * multiplicity x a) :
    ¬ ∃ p ∈ domain, IsMaxOn (fun w ↦ logLik x w.1 w.2) domain p
```

`half_collapse` gives the finite log limit, equal to
`-n * log(π) - Σ_{xᵢ ≠ a} log((a-xᵢ)²)`. `half_bound` gives the precise
pointwise value and equality test. `half_supremum` identifies the joint
supremum and gives an equivalent test for a feasible pair to attain it.

```lean
theorem half_collapse (x : Fin n → ℝ) {a : ℝ} (ha : 2 * multiplicity x a = n) :
    Tendsto (fun s ↦ logLik x a s) (𝓝[>] 0) (𝓝 (collapseValue x a))

theorem half_bound (x : Fin n → ℝ) {a m s : ℝ} (hs : 0 < s)
    (ha : 2 * multiplicity x a = n) :
    logLik x m s ≤ collapseValue x a ∧
    (logLik x m s = collapseValue x a ↔
      ∀ i, x i ≠ a → (m - a) * (m - x i) + s^2 = 0)

theorem half_supremum (x : Fin n → ℝ) {a : ℝ} (ha : 2 * multiplicity x a = n) :
    sSup ((fun p ↦ logLik x p.1 p.2) '' domain) = collapseValue x a ∧
    ∀ p ∈ domain, IsMaxOn (fun w ↦ logLik x w.1 w.2) domain p ↔
      ∀ i, x i ≠ a → (p.1 - a) * (p.1 - x i) + p.2^2 = 0
```

`half_no_jointMLE` excludes attainment if there are two distinct
off-atom observations. `half_maximizing_path_collapse` forces convergence
to the atom at zero scale along every eventually feasible path approaching
the supremum, with displacement small relative to scale.
`balanced_joint_argmax` recovers exactly the positive-scale semicircle
when all off-atom observations coincide at the other endpoint.

```lean
theorem half_no_jointMLE (x : Fin n → ℝ) {a : ℝ} (ha : 2 * multiplicity x a = n)
    {i j : Fin n} (hi : x i ≠ a) (hj : x j ≠ a) (hij : x i ≠ x j) :
    ¬ ∃ p ∈ domain, IsMaxOn (fun w ↦ logLik x w.1 w.2) domain p

theorem half_maximizing_path_collapse {ι : Type*} {l : Filter ι} {p : ι → ℝ × ℝ}
    (x : Fin n → ℝ) {a : ℝ} (ha : 2 * multiplicity x a = n)
    {i j : Fin n} (hi : x i ≠ a) (hj : x j ≠ a) (hij : x i ≠ x j)
    (hp : ∀ᶠ z in l, p z ∈ domain)
    (hL : Tendsto (fun z ↦ logLik x (p z).1 (p z).2) l (𝓝 (collapseValue x a))) :
    Tendsto p l (𝓝 (a, 0)) ∧ Tendsto (fun z ↦ ((p z).1 - a) / (p z).2) l (𝓝 0)

theorem balanced_joint_argmax (hn : 0 < n) (x : Fin n → ℝ) {a b : ℝ} (hab : a < b)
    (ha : 2 * multiplicity x a = n) (hx : ∀ i, x i = a ∨ x i = b) :
    {p ∈ domain | IsMaxOn (fun w ↦ logLik x w.1 w.2) domain p} =
      {p : ℝ × ℝ | a < p.1 ∧ p.1 < b ∧ 0 < p.2 ∧ p.2^2 = (p.1 - a) * (b - p.1)}
```

## Effort by target

| Target | Proof work |
| --- | --- |
| Held scale | Compactness, strict improvement outside the data range, and the balanced quartic calculation. |
| Held location | A derivative, strict comparisons, two score limits, and application of the generic score theorem. |
| Joint strict half mass | The largest algebraic and analytic portion. A uniform lower-scale bound and compact rectangle establish existence. The normalized midpoint and quadratic root argument establish uniqueness. |
| Collapse | Exact multiplicity split, two asymptotic limits, the sharp half-mass equality test, and a squeeze argument for arbitrary maximizing paths. |
