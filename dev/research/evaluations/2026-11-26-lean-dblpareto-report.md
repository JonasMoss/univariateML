# Double Pareto exact kink scan in Lean

TASK-370 · 2026-11-26 · `codex/lean-dblpareto`

## Source findings

**The two-level endpoint tie requires equal multiplicities.** With log levels
\(u<v\), a sample of size \(n\), and \(k\) copies of \(u\), the endpoint deviations
are \((n-k)(v-u)/n\) at the Pareto face and \(k(v-u)/n\) at the power face. The
faces tie exactly when \(2k=n\). Every two-level sample has no finite MLE.
For a sample of exactly two distinct observations the tie claim is correct.
The dossier and the 24 November report need this qualification if their
wording is read as covering samples with repeats. The 25 November report
already gives the multiplicity correction.
[Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/dblpareto/README.md) ·
[24 November report](2026-11-24-dblpareto-report.md#likelihood-and-existence) ·
[25 November report](2026-11-25-dblpareto-math-report.md)

**The dossier's displayed negative log-likelihood profile is strictly concave
on a gap.** The *log-likelihood* profile is strictly convex. For mean positive
deviations \(L,R\), the reduced mean profile is
\(-1-2\log(\sqrt L+\sqrt R)\). The corresponding total log likelihood is
\(-n-2n\log(\sqrt L+\sqrt R)-\sum_i\log x_i\). The sign of the convexity claim
depends on which of these objectives is being described.
[Dossier details](../../../univariateML-cpp/include/cppunivariate/distributions/dblpareto/README.md#details)

**The positive-deviation uniqueness clause applies to a held kink.** Both
positive deviations give one conditional rate pair. With the kink free,
global attainment requires an interior observed knot whose profile is at
least as high as every scanned profile, including both faces. Several
distinct winning interior knots give several finite maximizers. An
interior winner remains a finite maximizer if it ties a face. The face
height itself is unattained at its own kink.
[Lean module](../../lean/UnivariateML/Family/DoublePareto.lean)

The proofs use exact real arithmetic on `Fin n → ℝ` log observations. The
physical-data identity identifies the data-only Jacobian term. The rate
bound comes from the logarithm tangent inequality and the squared difference
of the scaled rates \(\alpha\sqrt L\) and \(\beta\sqrt R\). Square-root concavity
on affine deviations and logarithm concavity give the strict gap inequality.
The limit proof reuses `Generic.limit_le_upperBound` to compare candidate
face heights with upper bounds for the feasible likelihood.

## Verification and effort

The direct Lean check passed without diagnostics on the workstation. The
final `lake build` passed with 7,456 jobs and no diagnostics from
`DoublePareto.lean`. Lake replayed existing warnings in other modules.
The explicit module target builds the new file before its coordinator-owned
root import is added. The last rerun followed two line wraps requested by
the style linter.

```sh
cd dev/lean
/tmp/umvl-cpu/run env LEAN_NUM_THREADS=1 nice -n 10 bash -c '
  /home/jonas/.elan/bin/lake env lean UnivariateML/Family/DoublePareto.lean &&
  /home/jonas/.elan/bin/lake build UnivariateML +UnivariateML.Family.DoublePareto
'
```

| Target | Proof effort |
| --- | --- |
| 1. Closed rates and kink scan | Derived the sharp rate bound and its equality conditions. Proved affine deviations, strict gap convexity, and strict improvement to the nearest extreme outside the data range. One repair pass addressed square-zero and ordered-addition lemmas. |
| 2. Endpoint limits | Proved one rate-divergence path and obtained the other by reflection. Used eventual feasibility for the upper-bound comparison. One repair pass made the pointwise division and zero-distance rewrites explicit. |
| 3. Constant and two-level samples | Used an explicit equal-rate path for constant data. Computed the endpoint deviations from multiplicities and specialized the tie to two observations. The case proofs needed no separate repair pass. |
| 4. Existence and uniqueness | Combined conditional rate uniqueness with strict unattainment at a zero deviation, then characterized the entire joint maximizer set by the scan. |
| 5. Held-rate kink set | Proved the weighted-count conditions from one-sided supporting lines and exact slopes in adjacent gaps. Used compactness for existence and strict count changes beyond a balanced gap for its endpoints. Two repair passes addressed mean unfolding and the reflected left perturbation. |
| 6. Source reconciliation | Compared the dossier, both reports, and the fitter's limit and maximizer encoding. Recorded the multiplicity and sign qualifications above. |

## Remaining work

All TASK-370 mathematical targets are settled. The coordinator's root
import remains. At the implementation level, the C++ result carries a single
`limit_detail` face witness even when both faces maximize. General equality
of non-reflected knot profiles and platform-wide bounds for floating-point
comparisons remain open implementation questions described in the 25 November
report. The verified theorem set covers two free or two held rates.
[Fitter](../../../univariateML-cpp/include/cppunivariate/distributions/dblpareto/mle.hpp) ·
[Existing open questions](2026-11-25-dblpareto-math-report.md#open)

## Lean statements

The statements below are copied from `UnivariateML.DoublePareto`. The
implicit sample size is `{n : ℕ}`.

### Likelihood, rates, and scan

**`mean_logDensity_eq`**. The physical-data mean log likelihood equals the reduced log-scale likelihood minus the mean log observation.

```lean
theorem mean_logDensity_eq {x : Fin n → ℝ} (hn : 0 < n) (hx : ∀ i, 0 < x i)
    {scale : ℝ} (hs : 0 < scale) (a b : ℝ) :
    mean (fun i ↦ logDensity (x i) scale a b) =
      logLik (fun i ↦ log (x i)) (log scale) a b - mean (fun i ↦ log (x i))
```

**`objective_le_height`**. The profile bounds every positive rate pair. Equality requires the scaled rates to balance and the normalized harmonic rate to equal one.

```lean
theorem objective_le_height {L R a b : ℝ} (hL : 0 ≤ L) (hR : 0 ≤ R)
    (h : 0 < L + R) (ha : 0 < a) (hb : 0 < b) :
    objective L R a b ≤ height L R ∧
      (objective L R a b = height L R ↔
        a * sqrt L = b * sqrt R ∧ a * b / (a + b) * radius L R ^ 2 = 1)
```

**`fitted_rates`**. At two positive deviations the closed-form pair is feasible and reaches the profile.

```lean
theorem fitted_rates {L R : ℝ} (hL : 0 < L) (hR : 0 < R) :
    (leftRate L R, rightRate L R) ∈ rates ∧
      objective L R (leftRate L R) (rightRate L R) = height L R
```

**`conditional_maximizers`**. The closed-form pair is the only conditional maximizer at positive deviations.

```lean
theorem conditional_maximizers {L R : ℝ} (hL : 0 < L) (hR : 0 < R) (p : ℝ × ℝ) :
    (p ∈ rates ∧ IsMaxOn (rateLik L R) rates p) ↔
      p = (leftRate L R, rightRate L R)
```

**`profile_strictConvex_gap`**. The extended profile is strictly convex on a closed gap bounded by or lying between sample observations. The endpoints may carry face values.

```lean
theorem profile_strictConvex_gap (z : Fin n → ℝ) {lo hi : ℝ}
    (hz : ∀ i, z i ≤ lo ∨ hi ≤ z i) (i j : Fin n)
    (hiL : z i ≤ lo) (hiR : hi ≤ z j) :
    StrictConvexOn ℝ (Icc lo hi) (profile z)
```

**`scan_supremum`**. A best observed profile gives the least upper bound over all finite kinks and rates.

```lean
theorem scan_supremum {z : Fin n → ℝ} (hz : Nonconstant z)
    (j : Fin n) (hj : ∀ i, profile z (z i) ≤ profile z (z j)) :
    IsLUB (jointLik z '' domain) (profile z (z j))
```

**`joint_maximizers`**. Every finite global maximizer is an interior observed kink tied for the best scanned value, paired with its unique fitted rates. Every such pair is a global maximizer.

```lean
theorem joint_maximizers {z : Fin n → ℝ} (hz : Nonconstant z) (p : ℝ × (ℝ × ℝ)) :
    (p ∈ domain ∧ IsMaxOn (jointLik z) domain p) ↔
      (∃ j, z j = p.1) ∧ (∃ i, z i < p.1) ∧ (∃ i, p.1 < z i) ∧
        (∀ i, profile z (z i) ≤ profile z p.1) ∧
        p.2 = (leftRate (left z p.1) (right z p.1), rightRate (left z p.1) (right z p.1))
```

**`joint_exists_iff`**. On nonconstant data a finite joint MLE exists exactly when an interior observed kink wins the scan.

```lean
theorem joint_exists_iff {z : Fin n → ℝ} (hz : Nonconstant z) :
    (∃ p ∈ domain, IsMaxOn (jointLik z) domain p) ↔
      ∃ j : Fin n, (∃ i, z i < z j) ∧ (∃ i, z j < z i) ∧
        ∀ i, profile z (z i) ≤ profile z (z j)
```

### Faces and held-kink existence

**`pareto_face`**. At zero left deviation and positive right deviation, the fitted Pareto height exceeds every finite pair and is approached with the right rate held at the reciprocal deviation.

```lean
theorem pareto_face {R : ℝ} (hR : 0 < R) :
    (∀ a b : ℝ, 0 < a → 0 < b → objective 0 R a b < height 0 R) ∧
    height 0 R = log R⁻¹ - 1 ∧
    Tendsto (fun a : ℝ ↦ objective 0 R a R⁻¹) atTop (𝓝 (height 0 R))
```

**`power_face`**. At zero right deviation and positive left deviation, the fitted power height exceeds every finite pair and is approached with the left rate held at the reciprocal deviation.

```lean
theorem power_face {L : ℝ} (hL : 0 < L) :
    (∀ a b : ℝ, 0 < a → 0 < b → objective L 0 a b < height L 0) ∧
    height L 0 = log L⁻¹ - 1 ∧
    Tendsto (fun b : ℝ ↦ objective L 0 L⁻¹ b) atTop (𝓝 (height L 0))
```

**`sample_minimum_pareto`**. A kink at the sample minimum of nonconstant data has the stated Pareto limit and strict finite-rate bound.

```lean
theorem sample_minimum_pareto {z : Fin n → ℝ} {lo : ℝ}
    (hlo : ∀ i, lo ≤ z i) (h : ∃ i, lo < z i) :
    (∀ a b : ℝ, 0 < a → 0 < b → logLik z lo a b < profile z lo) ∧
    profile z lo = log (right z lo)⁻¹ - 1 ∧
    Tendsto (fun a : ℝ ↦ logLik z lo a (right z lo)⁻¹) atTop (𝓝 (profile z lo))
```

**`sample_maximum_power`**. A kink at the sample maximum of nonconstant data has the stated power limit and strict finite-rate bound.

```lean
theorem sample_maximum_power {z : Fin n → ℝ} {hi : ℝ}
    (hhi : ∀ i, z i ≤ hi) (h : ∃ i, z i < hi) :
    (∀ a b : ℝ, 0 < a → 0 < b → logLik z hi a b < profile z hi) ∧
    profile z hi = log (left z hi)⁻¹ - 1 ∧
    Tendsto (fun b : ℝ ↦ logLik z hi (left z hi)⁻¹ b) atTop (𝓝 (profile z hi))
```

**`left_pos_iff`**. The left deviation is positive exactly when some observation is strictly below the kink.

```lean
theorem left_pos_iff (z : Fin n → ℝ) (c : ℝ) :
    0 < left z c ↔ ∃ i, z i < c
```

**`right_pos_iff`**. The right deviation is positive exactly when some observation is strictly above the kink.

```lean
theorem right_pos_iff (z : Fin n → ℝ) (c : ℝ) :
    0 < right z c ↔ ∃ i, c < z i
```

**`held_kink_exists_iff`**. A finite conditional rate maximum exists exactly when both deviations are positive.

```lean
theorem held_kink_exists_iff {L R : ℝ} (hL : 0 ≤ L) (hR : 0 ≤ R) :
    (∃ p ∈ rates, IsMaxOn (rateLik L R) rates p) ↔ 0 < L ∧ 0 < R
```

**`held_kink_unique_iff`**. The conditional maximum is uniquely attained under exactly the same condition.

```lean
theorem held_kink_unique_iff {L R : ℝ} (hL : 0 ≤ L) (hR : 0 ≤ R) :
    (∃! p, p ∈ rates ∧ IsMaxOn (rateLik L R) rates p) ↔ 0 < L ∧ 0 < R
```

**`held_sample_exists_iff`**. In sample terms a held kink admits finite fitted rates exactly when observations lie strictly on both sides.

```lean
theorem held_sample_exists_iff (z : Fin n → ℝ) (c : ℝ) :
    (∃ p ∈ rates, IsMaxOn (rateLik (left z c) (right z c)) rates p) ↔
      (∃ i, z i < c) ∧ (∃ i, c < z i)
```

### Constant and two-level samples

**`zero_distances_unbounded`**. With zero deviations on both sides, positive rates can exceed any proposed bound on the log likelihood.

```lean
theorem zero_distances_unbounded (M : ℝ) :
    ∃ a b : ℝ, 0 < a ∧ 0 < b ∧ M < objective 0 0 a b
```

**`constant_unbounded`**. A kink at the common observation value makes the constant-sample likelihood unbounded.

```lean
theorem constant_unbounded (c M : ℝ) :
    ∃ a b : ℝ, 0 < a ∧ 0 < b ∧ M < logLik (fun _ : Fin n ↦ c) c a b
```

**`two_level_no_mle`**. Two distinct levels exclude finite attainment at every multiplicity.

```lean
theorem two_level_no_mle {z : Fin n → ℝ} {lo hi : ℝ} (h : lo < hi)
    (hlo : lo ∈ range z) (hhi : hi ∈ range z) (hz : ∀ i, z i = lo ∨ z i = hi) :
    ¬ ∃ p ∈ domain, IsMaxOn (jointLik z) domain p
```

**`two_level_supremum`**. The larger of the two endpoint profile values is the global least upper bound.

```lean
theorem two_level_supremum {z : Fin n → ℝ} {lo hi : ℝ} (h : lo < hi)
    (hlo : lo ∈ range z) (hhi : hi ∈ range z) (hz : ∀ i, z i = lo ∨ z i = hi) :
    IsLUB (jointLik z '' domain) (max (profile z lo) (profile z hi))
```

**`two_level_tie_iff`**. The endpoints tie exactly when the lower level accounts for half the observations.

```lean
theorem two_level_tie_iff {z : Fin n → ℝ} {lo hi : ℝ} (h : lo < hi)
    (hlo : lo ∈ range z) (hhi : hi ∈ range z) (hz : ∀ i, z i = lo ∨ z i = hi) :
    profile z lo = profile z hi ↔ 2 * countAtOrBelow z lo = n
```

**`two_observations_faces_tie`**. For two ordered distinct observations the endpoint values tie.

```lean
theorem two_observations_faces_tie {z : Fin 2 → ℝ} (h : z 0 < z 1) :
    profile z (z 0) = profile z (z 1)
```

### Held-rate maximizers

**`held_rates_maximizers`**. The exact kink set satisfies the weighted quantile inequalities using the strict and weak counts at each kink.

```lean
theorem held_rates_maximizers (z : Fin n → ℝ) (hn : 0 < n) {a b : ℝ}
    (ha : 0 < a) (hb : 0 < b) (c : ℝ) :
    IsMaxOn (fun d ↦ logLik z d a b) univ c ↔
      (a + b) * (countBelow z c : ℝ) ≤ b * n ∧
        b * n ≤ (a + b) * (countAtOrBelow z c : ℝ)
```

**`held_rates_exists`**. Every nonempty sample has a finite maximizing kink when both rates are positive and held.

```lean
theorem held_rates_exists (z : Fin n → ℝ) (hn : 0 < n) {a b : ℝ}
    (ha : 0 < a) (hb : 0 < b) : ∃ c, IsMaxOn (fun d ↦ logLik z d a b) univ c
```

**`held_rates_interval`**. If a gap between observed endpoints has balanced weights, its full closed interval is the complete kink set.

```lean
theorem held_rates_interval {z : Fin n → ℝ} (hn : 0 < n) {a b lo hi : ℝ}
    (ha : 0 < a) (hb : 0 < b) (h : lo < hi) (hlo : lo ∈ range z) (hhi : hi ∈ range z)
    (hz : ∀ i, z i ≤ lo ∨ hi ≤ z i)
    (hbal : (a + b) * (countAtOrBelow z lo : ℝ) = b * n) :
    {c | IsMaxOn (fun d ↦ logLik z d a b) univ c} = Icc lo hi
```
