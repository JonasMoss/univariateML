# Lomax joint uniqueness on two-point samples

TASK-301 · 2026-11-25

Lean proves that a nonempty positive sample taking at most two values has a
unique joint Lomax maximizer whenever it attains a finite maximum. It also
proves the four-observation example has two separated strict local profile
maxima. [Lean module](../../lean/UnivariateML/Family/LomaxTwoPoint.lean)

## Scope and source precision

The source report's weighted calculation permits arbitrary positive
normalized real weights. A finite sample realizes exactly **rational**
normalized multiplicity weights. The Lean cubic result covers real
weights, and the joint likelihood theorem is stated for finite samples.
Constant samples are included in its hypothesis on support. The separate
`Lomax.constant_no_max` theorem rules out attainment for them.

The proof of joint uniqueness uses equal endpoint heights and Rolle's
theorem to get an intervening stationary point. This also handles a
hypothetical flat interval between maximizers. The stretch proof uses
analyticity for strictness and a 12-term Taylor enclosure after reducing
log arguments by powers of two. The source calculation uses a different
log series. Both approaches certify the signs over the reals.

The Lomax dossier's general uniqueness entry does not record the
two-support case. The source report also exhibits a finite global tie for
a different sample. The four-observation Lean theorem concerns local
maxima. [Source report](2026-11-25-lomax-uniq-math-report.md) ·
[Lomax dossier](../../../univariateML-cpp/include/cppunivariate/distributions/lomax/README.md)

## Checked statements

The following declarations are in `UnivariateML.LomaxTwoPoint`. The
unqualified sample average, likelihood, parameter domain and existence
criterion use the definitions in `UnivariateML.Lomax`.

### Score identities

```lean
theorem profile_logRate_deriv (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) (z : ℝ) :
    HasDerivAt (fun t ↦ F x (exp t)) (U x (exp z) / Ar x (exp z) * H x (exp z)) z
```

The per-observation gap from the fitted exponential has log-rate derivative `(U / Ar) * H`. At positive rates the prefactor is positive.

```lean
theorem score_logRate_deriv (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) (z : ℝ) :
    HasDerivAt (fun t ↦ H x (exp t)) (B x (exp z) - C x (exp z) / U x (exp z)^2) z
```

The zero-equivalent score has log-rate derivative `B - C / U^2`. Here `U` is the mean reciprocal of `1 + r*x`, `B = 1 - U`, and `C` is the mean of `r*x/(1+r*x)^2`.

### Cubic and root bound

The cubic has coefficients given by the following definition.

```lean
def cubic (m b d r : ℝ) : ℝ :=
  m * (b - m) - d + (m * b^2 - d * (b + 3*m))*r -
    d * (b*m + 2*d)*r^2 - b*d^2*r^3
```

```lean
theorem numerator_eq (m b d r : ℝ) :
    let D := 1 + (b + m)*r + d*r^2
    let N := 1 + b*r
    (D - N)*N^2 - r*D*(N*(b+m+2*d*r) - b*D) = r^2 * cubic m b d r
```

The polynomial numerator factors as the squared rate times the cubic.

```lean
theorem cubic_strictConcave {m b d : ℝ} (hm : 0 < m) (hb : 0 < b) (hd : 0 < d) :
    StrictConcaveOn ℝ (Ioi 0) (cubic m b d)
```

Positive mean, complementary mean and product make the cubic strictly concave on the positive half-line.

```lean
theorem cubic_no_three_zeros {m b d : ℝ} (hm : 0 < m) (hb : 0 < b) (hd : 0 < d)
    {a c e : ℝ} (ha : 0 < a) (hac : a < c) (hce : c < e)
    (h₁ : cubic m b d a = 0) (h₂ : cubic m b d c = 0) (h₃ : cubic m b d e = 0) :
    False
```

Three distinct positive zeros are impossible. The value at the middle root would have to lie strictly above the chord connecting the other two.

```lean
theorem weighted_cubic {a c w : ℝ} (ha : 0 < a) (hc : 0 < c)
    (hw : 0 < w) (hw' : w < 1) :
    let m := w*a + (1-w)*c
    let b := a+c-m
    let d := a*c
    let D := fun r : ℝ ↦ (1+r*a)*(1+r*c)
    let N := fun r : ℝ ↦ w*(1+r*c) + (1-w)*(1+r*a)
    (∀ r, (D r - N r)*(N r)^2 - r*D r*(N r*(a+c+2*d*r) - b*D r)
      = r^2 * cubic m b d r) ∧ StrictConcaveOn ℝ (Ioi 0) (cubic m b d)
```

Two positive support points with any positive normalized real weights give the displayed numerator and a strictly concave cubic.

```lean
theorem two_point_score_deriv (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {a c : ℝ} (ha : 0 < a) (hc : 0 < c) (hs : ∀ i, x i = a ∨ x i = c)
    {r : ℝ} (hr : 0 < r) :
    let m := avg x
    let b := a+c-m
    let d := a*c
    let D := 1 + (b+m)*r + d*r^2
    let N := 1 + b*r
    r * deriv (H x) r = r^2 * cubic m b d r / (D*N^2)
```

For a sample supported on two positive points, the log-rate derivative of the score is the cubic multiplied by a positive factor. The theorem uses `r * deriv (H x) r` for the derivative in log-rate coordinates.

### Joint uniqueness

```lean
theorem joint_unique (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hs : ∃ a c : ℝ, ∀ i, x i = a ∨ x i = c)
    (he : ∃ p ∈ domain, IsMaxOn (fun q : ℝ × ℝ ↦ logLik x q.1 q.2) domain p) :
    ∃! p : ℝ × ℝ, p ∈ domain ∧ IsMaxOn (fun q ↦ logLik x q.1 q.2) domain p
```

If a nonempty positive sample takes at most two values and a finite shape-rate pair attains the global maximum, exactly one pair does. The two support witnesses may coincide.

```lean
theorem joint_unique_iff (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hs : ∃ a c : ℝ, ∀ i, x i = a ∨ x i = c) :
    (∃! p : ℝ × ℝ, p ∈ domain ∧ IsMaxOn (fun q ↦ logLik x q.1 q.2) domain p) ↔
      ∃ u ∈ Ioo 0 (uBar x), psi x u ≤ 0
```

On the same samples, the existing Lomax existence criterion is equivalent to existence of a unique joint maximum. The inequality includes equality.

### Separated strict local maxima

```lean
theorem exists_two_strict_local_maxima :
    ∃ x : Fin 4 → ℝ, (∀ i, 0 < x i) ∧
      ∃ u ∈ Ioo (1/100000 : ℝ) (1/1000), ∃ v ∈ Ioo (1/10 : ℝ) 1,
        (∀ᶠ t in 𝓝[≠] u, F x t < F x u) ∧ (∀ᶠ t in 𝓝[≠] v, F x t < F x v)
```

The witness is the sample `(1, 3000, 3000, 22000)`. Each bracket contains a rate whose profile gap strictly exceeds those at all other rates sufficiently near it.

## Proof effort

| Target | Relative effort | Main proof work |
| --- | --- | --- |
| 1 | Moderate | Differentiate finite averages and clear the positive denominators. Compose with the exponential to obtain log-rate derivatives. |
| 2 | Moderate | Expand the numerator, differentiate the cubic twice, and use a strict chord inequality to rule out a third positive root. Express sample averages by a multiplicity weight. |
| 3 | Largest | Transfer joint maximizers to the profile with `shape_profile` and `profile_gap`. Apply Rolle to the equal-height profile endpoints, then to the three positive score roots and the zero at rate zero. Reuse `exists_max_iff` for the attainment criterion. |
| 4 | Substantial | Enclose the logs with rational Taylor bounds, rule out bracket endpoints using derivative signs, and apply the identity theorem for real analytic functions to make the interior maxima strict. |

## Verification

Both final checks ran on the workstation in `dev/lean`, in sequence
through the single-slot wrapper.

- `lake env lean UnivariateML/Family/LomaxTwoPoint.lean` exited zero
  without warnings. This check covers the new module pending its import
  by the coordinator.
- `lake build` exited zero with `Build completed successfully (6598 jobs).`
  It replayed existing line-length warnings at lines 639 and 675 of
  `UnivariateML/Family/Weibull.lean`.

All four targets are closed. The proofs use no placeholders or added axioms.
