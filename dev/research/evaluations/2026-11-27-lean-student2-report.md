# TASK-431 Student likelihoods, second Lean pass

The second pass formalizes the Student, location-scale Student, half-t, slash and skew-t boundary and candidate comparisons, exact nonuniqueness witnesses, and family-specific score inputs for the generic uniqueness arguments.

## Verification

On the workstation, all 14 `Student2` modules passed the final standalone `lake env lean` checks. The final `lake build` with temporary imports completed successfully with 8106 jobs. Both checks ran under `nice -n 10` through the single-slot wrapper. The build emitted linter warnings. The temporary imports were removed, and `git diff --exit-code -- dev/lean/UnivariateML.lean` passed.

The 126 checked public theorems have 162 entries in the atlas fragment, including declarations mapped to more than one held regime. Every coverage cell is blank for independent review. The new modules have no admitted proofs.

## Checked theorems

### `UnivariateML.Student2HeldDf.std_held_df_cardinality`

For held df above two the maximizing set is a singleton under the strict mass test and empty otherwise. The statement tests every observed atom. Since the exponent is linear and strictly decreasing in multiplicity, this is the source’s largest-atom test. The proof combines existing attainment and geodesic midpoint uniqueness, with both cardinality directions checked.

```lean
theorem std_held_df_cardinality {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) {ν : ℝ}
    (hν : 2 < ν) :
    let D := (univ : Set ℝ) ×ˢ Ioi (0 : ℝ)
    let f := fun z : ℝ × ℝ ↦ TDist.stdLogLik x z.1 ν z.2
    let strictMass := ∀ i, (TDist.multiplicity x (x i) : ℝ) <
      ν * ((n : ℝ) - TDist.multiplicity x (x i))
    ((∃! p, p ∈ D ∧ IsMaxOn f D p) ↔ strictMass) ∧
      ((∀ p ∈ D, ¬ IsMaxOn f D p) ↔ ¬ strictMass)
```

### `UnivariateML.Student2FixedMeanSd.std_fixed_mean_sd_ends`

With both mean and positive sd held, the likelihood tends to zero, a finite positive collapse height, or infinity as df tends down to two, according to whether the held-mean count is below, equal to, or above two thirds. As df goes to infinity it tends to the matched Gaussian likelihood. The proof joins the existing endpoint theorems and translates the exponent sign to natural-number count inequalities. The critical height uses the existing log expression.

```lean
theorem std_fixed_mean_sd_ends {n : ℕ} (x : Fin n → ℝ) (μ : ℝ) {σ : ℝ}
    (hσ : 0 < σ) :
    let L := fun ν : ℝ ↦ Real.exp (TDist.stdLogLik x μ ν σ)
    let k := TDist.multiplicity x μ
    (3 * k < 2 * n → Tendsto L (𝓝[>] 2) (𝓝 0)) ∧
    (3 * k = 2 * n → Tendsto L (𝓝[>] 2)
      (𝓝 (Real.exp (StudentFaces.fixedSdCollapseLog x μ)))) ∧
    (2 * n < 3 * k → Tendsto L (𝓝[>] 2) atTop) ∧
    Tendsto L atTop (𝓝 (∏ i, TDist.normalDensity μ σ (x i)))
```

### `UnivariateML.Student2Slash.joint_majority_and_half_failure`

A strict majority makes likelihood unbounded. If a half-mass atom exists and there is no majority, nonattainment is equivalent to strict loss at every finite point against the highest collapse trace, which is then the supremum. The sufficient paired condition permits any center and any ordering, expressed through an index equivalence. The proof reindexes the likelihood into products of four densities, translates the center, and pulls the strict paired bound back to the joint chart.

```lean
theorem joint_majority_and_half_failure {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) :
    let D := (univ : Set ℝ) ×ˢ Ioi (0 : ℝ)
    let F := fun p : ℝ × ℝ ↦ StudentSlash.likelihood x p.1 p.2
    let k := fun b ↦ SkewLSSlash.ties (fun i ↦ x i - b)
    ((∃ b, n < 2 * k b) → ¬ BddAbove (F '' D)) ∧
    ((∀ b, 2 * k b ≤ n) → (∃ b, n = 2 * k b) →
      ∃ b, n = 2 * k b ∧
        (∀ z, n = 2 * k z → StudentSlash.collapseValue x z ≤ StudentSlash.collapseValue x b) ∧
        ((¬ ∃ p ∈ D, IsMaxOn F D p) ↔
          IsLUB (F '' D) (StudentSlash.collapseValue x b) ∧
          ∀ p ∈ D, F p < StudentSlash.collapseValue x b)) ∧
    (∀ {r : ℕ}, 0 < r → ∀ (d : Fin r → ℝ), (∀ i, 0 < d i) →
      ∀ (e : Fin r × Fin 4 ≃ Fin n) (b : ℝ),
      (∀ i, x (e (i, 0)) = b ∧ x (e (i, 1)) = b ∧
        x (e (i, 2)) = b - d i ∧ x (e (i, 3)) = b + d i) →
      ¬ ∃ p ∈ D, IsMaxOn F D p)
```

## Scope and wording

The proofs use finite indexed samples, so multiplicity counts, zero residuals and ties are included whenever the source allows them. The fixed-mean and fixed-sd theorem gives both excluded shape ends of its scalar chart. The separate `limit:4` certificate also asks for a finite interval comparison.

**Source discrepancy.** The older skew-t scale derivation writes a leading \(w^2\) in the large-slant normalized likelihood. Grouping the 201 densities yields \(w\). The Lean witness uses the corrected expression and the wider second-level band from the repaired uniqueness report. The outlier stays symbolic throughout the proof. [Original derivation](2026-11-27-st-atlas-math-report.md#st-held-all-free) · [Repaired crossing](2026-11-27-uniq-locscale-math-report.md#repair-of-the-skew-t-scale-crossing)

## Target dispositions and proof effort

The criterion and failure theorems below cover the requested held regimes, including the fitted shape floors, constant data, atom contact, and equality in the face comparison. The table records the remaining uniqueness and certificate obligations and the concrete work on each. Family score inputs are separate from the almost-everywhere theorem under development in `Generic/UniqueAE.lean`.

| Family and holds | Checked result and remaining obligation |
| --- | --- |
| std, nu | Combined strict atom mass, geodesic uniqueness and the sharp critical upper bound. The joined failure result also checks vanishing at remote ends and positive-exponent atoms. |
| std, all free | Proved moving-shape and moving-atom limits, a superlevel compactness argument, the joint candidate test, and a strict Gaussian inward witness using an explicit sixth-order remainder. The finite, Gaussian and collapse score bounds meet the four-level threshold. The exact joint real crossing still needs both endpoint order certificates, the separator and the lower-shape collar. |
| std, mean | Specialized the actual joint trace and candidate comparison to held mean. Two distinct nonzero residual magnitudes separate finite, Gaussian and collapse scores. The exact held-mean real crossing needs the two joint endpoint order certificates and separator. |
| std, mean+sd | Checked the three lower-end mass cases and the Gaussian end, plus the two-magnitude score input. The critical two-thirds collapse log-score tends to minus infinity. To certify the scalar crossing, the remaining estimates are the opposite strict orders on the two compact shape intervals, the uniform separator, and the quantitative low-shape bound for the specific sample. |
| std, nu+sd | Cleared the location score and classified all one- and two-level counts and half-gaps, including equality. The quadratic intersection bound gives the three-level input. |
| std, sd | Checked the joined criterion and all escaping competitors via the held-standard-deviation chart. At half-gap 100 the shape-three point beats all centered finite fits and all matched Gaussian values. The four-level finite and Gaussian score bounds are checked. |
| lst, df | Checked the joint criterion and failure, including the balanced Cauchy and subunit critical branches. The three-level score input and reflected subunit example are checked. |
| lst, df+sigma | Cleared the location score to a nonzero polynomial and proved the exact finite-root likelihood comparison. The all-count classification and quadratic score bound also apply in ordinary scale coordinates. |
| lst, all free | Checked the criterion and boundary classifier, the quartic Gaussian inward test, and nonattainment for constant or two-level data. The four-level score input is checked. The exact joint real crossing still needs both endpoint order certificates, the separator and lower-shape collar. |
| lst, mu | Joined the held-location failure cases, including endpoint contacts. Proved strict normal escape with one nonzero magnitude and separated the finite, Gaussian and collapse scores at two magnitudes. The exact held-location crossing still needs the two joint endpoint orders and separator. |
| lst, sigma | Checked the joined criterion, exhaustion by the held-scale normal, the four-level score input and the full exact reflected pair at ordinary scale one and half-gap 100. |
| ht, all free | Checked the exact set of maximizing candidates, tie retention and unique eligible scale. The squared-width chart gives analyticity and local finiteness of stationary points on compact interior sets unless the profile is constant. The critical collapse derivative tends to minus infinity. The subcritical shape floor excludes sufficiently small widths. A checked reduction leaves eventual exclusion of score zeros at the normal end as the sole additional premise for finite stationary lists. |
| ht, mu | Checked the held-endpoint failure classification, the finite and normal score bound at two positive magnitudes and strict normal escape at one magnitude. |
| ht, mu+sigma | Derived the signed Laplace representation and the exact moment threshold, including escape at threshold equality and the all-zero branch. The positive one-magnitude shape is unique if attained. Two positive magnitudes separate finite and normal scores. |
| ht, sigma | Checked endpoint profiling, the moment threshold and the one- and two-level uniqueness statements. The finite and normal scores separate given two positive residual magnitudes after conditioning on the minimum. |
| slash, all free | Checked the joined majority and half-mass failure result, the exact paired sufficient test, analytic finite-score identification, and two symbolic witnesses above collapse on opposite sides of the atom. Every point on the vertical collapse slice loses strictly. Compact superlevels on each side give two local maxima. The finite score cannot agree locally with a collapse-score pole. |
| slash, sigma | Checked strict improvement over the midpoint and two reflected global winners at magnitude 100. The finite analytic score input is checked. The reflected argument works on the open half-gap region above four scales. |
| st, all free | Checked the fitted-domain criterion and all failure classes, with both half-face orientations and moving contacts. Finite analytic score separation is checked. At the exact symbolic reflected sample, the shape-one witness beats all reflection-fixed fits and all excluded faces, giving two distinct finite global winners. |
| st, alpha | Checked the criterion and all failure classes, finite score separation and exact zero-slant transfer. The joint crossing needs its two remaining interval order certificates and separator. |
| st, alpha+nu | Checked the criterion and all failure classes, finite analytic score separation and the balanced Cauchy arc. |
| st, alpha+nu+xi | Checked finite analytic score separation and the exact fixed-shape, large-slant crossing, with the outlier handled symbolically. |
| st, alpha+xi | Checked the criterion, all failure classes and finite score separation. The fixed-Cauchy crossing transfers to free shape. A uniform CDF ratio handles the moving argument near shape one, and rational and cubic density bounds exclude the remaining finite shapes. The outlier exponent is handled symbolically. |
| st, nu | Checked the criterion, failure and finite score separation. At held df one the exact reflected witness beats every centered width and both oriented half-t envelopes. |
| st, nu+xi | Checked the joint criterion, strict critical loss, all failure cases, finite analytic score separation and the paired singleton. |
| st, omega | Checked the criterion and all failure classes, with finite analytic score separation. The exact reflected example at held scale \(\sqrt{5}/2\) follows from the fitted-domain witness and the uniform face bounds. |
| st, xi | Checked the criterion, all failure classes and finite analytic score separation. Proved the pairing inequality and zero-slant transfer. Exact nonuniqueness needs the paired Student crossing enclosures across the fitted domain. |

The joint crossing uses opposite strict orderings of the low- and high-shape profiles at the two samples. Lean checks the compact real-crossing implication. For the remaining numerical inequalities, the first analytic route tried the uniform normalizer bounds \(A_\infty-1/(4q)\le A(q)\le A_\infty\). At the proposed separator \(q=30\), the possible error from that bracket alone is \(1/120\) per observation, wider than the source's roughly \(10^{-6}\) endpoint gaps. The unresolved comparisons include \(P_0(5.5)>\sup_{[30,4\cdot10^6]}P_0\), \(P_1(158)>\sup_{[2,30]}P_1\), the uniform separator \(P_t(158)>P_t(30)\), and the positive derivative on \([.05,2]\) for the joint sample. The scalar gaps still to certify are \(P_0(8.25)>\sup_{[30,10^7]}P_0\), \(P_{1/8}(161)>\sup_{[2.00001,30]}P_{1/8}\), and the uniform \(P_t(135)>P_t(30)\). The cubic normalizer enclosure and the two-unit log-Gamma step check. This reduces the failed comparison to the rational logarithm bounds and exact scale-root brackets on the written cover cells. The joint endpoint covers have 323 and 164 boxes, the derivative collar has 512 boxes, and the scalar endpoint covers have 642 and 259 boxes. The delimited mathematical gap is certifying those interval comparisons in Lean, including the uniform separator. The abstract crossing theorem does not settle the exact sample witnesses.

For the half-t normal end, I tried the squared-width analytic chart and a polynomial lower bound on the Laplace kernel of the log-normalizer score. The cube correction gives a fourth-order error in the score. A finite-order bound of this kind only decides the sign if the first coefficients fail to cancel. The remaining lemma is that the shape score has some nonzero finite-order coefficient in reciprocal width for every eligible sample. The source proves this using the factorial-weighted Taylor coefficients of the sigmoid Laplace integral. A Lean proof still needs the all-orders remainder estimate and the contradiction between the divergent factorial-weighted series and the analytic data contribution.

The generic conclusion for each family waits for the shared theorem and its applicability hypotheses. In skew-t, Lean identifies all finite parameters from the spatial score and separates finite, collapse and skew-normal scores. Local agreement with an oriented half-Student score identifies the base Student fit with zero slant. For a compatible sample with a strict-support observation, the oriented face then has strictly greater likelihood. The remaining applicability obligation is the shape-varying smoothness of all spatial derivatives where the shared theorem needs it.

## Path and face-comparison keys

| Family and holds | Checked scope and remaining comparison |
| --- | --- |
| std, all free | The existing `path:1` and `path:6` witnesses are complemented by the joined detector and actual lower-face comparison for `limit:1` and `limit:6`. For `limit:5`, the uniform Gaussian remainder and strict inward test check. The finite tube cover and certified interval implementation remain. |
| std, mean+nu and nu | The sharp held-df upper bound and moving critical collapse formulas accompany the earlier `path:2` and `path:3` witnesses. The standard-deviation chart is explicit in the joined fixed-df decision, which applies at the held mean as well. |
| std, mean+sd | `path:4` is included in the fixed-mean and fixed-sd lower-end calculation. The signed critical-collar score is checked for `limit:4`. The finite interval cover remains. |
| lst, df | The joined decision adds moving collapse, strict positive mass and the critical subunit candidate comparison to vertical `path:2` and `path:3`, including both critical branches of `limit:3`. |
| lst, all free | The joined criterion includes the fitted-floor converse for `limit:1`. The Gaussian tail and strict inward test check for `limit:4`. The finite tube cover and certified interval implementation remain. |
| ht, all free and nu | The round-five `path:1`–`path:3` cover the explicit paths and endpoint profiling. The joined `limit:2` theorem gives exact divergence detectors and admissible paths for free or held endpoint and held shape. The held-endpoint failure theorem adds free-shape escape exhaustion. |
| slash, all free | `path:1` has the moving-location trace and upper bound from round five. The joined failure theorem adds the majority and largest half-trace decision with the exact paired sufficient test. |
| st, all free and held masks | Joined excursion and criterion theorems add both slant orientations, the moving-contact envelopes and the optimized moving critical layers to `path:1` through `path:7` and `limit:1`–`limit:3`, `limit:6` and `limit:7`. The finite-interval certificates for the exact uniqueness examples remain separate. |

[Round-five proof ledger](2026-11-27-lean-student-report.md) · [Coverage 4a](2026-11-27-atlas-coverage4a-report.md) · [Coverage 4b](2026-11-27-atlas-coverage4b-report.md)

## Continuation checks

### `UnivariateML.Student2HeldDf.std_held_df_failure`

Negative exponent gives a divergent vertical path. Critical exponent gives a sharp unattained finite supremum. Remote and positive-exponent ends vanish. Positive held df above two, nonempty finite sample and arbitrary indicated escape filters. Individual check passed.

```lean
theorem std_held_df_failure {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) {ν : ℝ}
    (hν : 2 < ν) :
    let L := fun m s ↦ exp (TDist.stdLogLik x m ν s)
    ((∀ b, TDist.exponent x b ν < 0 → Tendsto (L b) (𝓝[>] 0) atTop)) ∧
    (∀ b, TDist.exponent x b ν = 0 →
      let C := exp (TDist.collapseLogValue x b ν)
      Tendsto (L b) (𝓝[>] 0) (𝓝 C) ∧
      IsLUB ((fun p : ℝ × ℝ ↦ L p.1 p.2) '' (univ ×ˢ Ioi 0)) C ∧
      ∀ m s : ℝ, 0 < s → L m s < C) ∧
    (∀ {ι : Type*} {l : Filter ι} {m s : ι → ℝ}, (∀ᶠ j in l, 0 < s j) →
      (Tendsto s l atTop ∨ Tendsto m l (cocompact ℝ)) →
      Tendsto (fun j ↦ L (m j) (s j)) l (𝓝 0)) ∧
    (∀ {ι : Type*} {l : Filter ι} {m s : ι → ℝ} {b : ℝ},
      Tendsto m l (𝓝 b) → Tendsto s l (𝓝[>] 0) →
      (0 < TDist.exponent x b ν → Tendsto (fun j ↦ L (m j) (s j)) l (𝓝 0)) ∧
      (TDist.exponent x b ν = 0 → ∀ B > exp (TDist.collapseLogValue x b ν),
        ∀ᶠ j in l, L (m j) (s j) < B))
```

### `UnivariateML.Student2MovingCollapse.moving_shape_collapse`

Joint collapse at positive limiting shape vanishes at positive exponent and has the vertical upper bound at criticality when shape approaches from above. Finite limiting location and positive limiting shape in ordinary scale coordinates. The critical branch is an upper bound. Individual check passed.

```lean
theorem moving_shape_collapse {n : ℕ} (x : Fin n → ℝ) (b : ℝ) {q : ℝ} (hq : 0 < q)
    {ι : Type*} {l : Filter ι} {m a v : ι → ℝ}
    (hm : Tendsto m l (𝓝 b)) (ha : Tendsto a l (𝓝[>] 0))
    (hv : Tendsto v l (𝓝 q)) :
    let L := fun j ↦ TDist.likelihood x (m j) (v j) (a j)
    (0 < TDist.exponent x b q → Tendsto L l (𝓝 0)) ∧
    (TDist.exponent x b q = 0 → (∀ᶠ j in l, q ≤ v j) →
      ∀ B > exp (TDist.collapseLogValue x b q), ∀ᶠ j in l, L j < B)
```

### `UnivariateML.Student2MovingCollapse.critical_vertical_rate`

A vertical critical collapse reaches the exact positive boundary height when the shape displacement times log scale tends to zero. Ordinary scale at positive limiting df. The rate hypothesis specifies the sharp path. Individual check passed.

```lean
theorem critical_vertical_rate {n : ℕ} (x : Fin n → ℝ) (b : ℝ) {q : ℝ} (hq : 0 < q)
    (he : TDist.exponent x b q = 0) {ι : Type*} {l : Filter ι} {a v : ι → ℝ}
    (ha : Tendsto a l (𝓝[>] 0)) (hv : Tendsto v l (𝓝 q))
    (hr : Tendsto (fun j ↦ (v j - q) * log (a j)) l (𝓝 0)) :
    Tendsto (fun j ↦ TDist.likelihood x b (v j) (a j)) l
      (𝓝 (exp (TDist.collapseLogValue x b q)))
```

### `UnivariateML.Student2Normal.moving_normal_face`

At infinite df and finite location and positive scale limits the likelihood tends to the matched Gaussian product. Moving location, ordinary scale and df. Individual check passed.

```lean
theorem moving_normal_face {n : ℕ} (x : Fin n → ℝ)
    {ι : Type*} {l : Filter ι} {m a v : ι → ℝ} {b s : ℝ} (hs : 0 < s)
    (hm : Tendsto m l (𝓝 b)) (ha : Tendsto a l (𝓝 s)) (hv : Tendsto v l atTop) :
    Tendsto (fun j ↦ TDist.likelihood x (m j) (v j) (a j)) l
      (𝓝 (∏ i, TDist.normalDensity b s (x i)))
```

### `UnivariateML.Student2Normal.zero_scale_normal_corner`

At infinite df and zero scale the product vanishes when at least one limiting residual is nonzero. Moving location and ordinary scale. In the no-supercritical-atom regime every limiting location has a nonzero residual. Individual check passed.

```lean
theorem zero_scale_normal_corner {n : ℕ} (x : Fin n → ℝ)
    {ι : Type*} {l : Filter ι} {m a v : ι → ℝ} {b : ℝ}
    (hm : Tendsto m l (𝓝 b)) (ha : Tendsto a l (𝓝[>] 0)) (hv : Tendsto v l atTop)
    (hx : ∃ i, x i ≠ b) :
    Tendsto (fun j ↦ TDist.likelihood x (m j) (v j) (a j)) l (𝓝 0)
```

### `UnivariateML.Student2Normal.moving_shape_remote_ends`

Likelihood vanishes at infinite location or infinite ordinary scale uniformly over df at least one. Ordinary scale, arbitrary filters and nonempty samples. Individual check passed.

```lean
theorem moving_shape_remote_ends {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    {ι : Type*} {l : Filter ι} {m a v : ι → ℝ}
    (ha : ∀ᶠ j in l, 0 < a j) (hv : ∀ᶠ j in l, 1 ≤ v j)
    (h : Tendsto a l atTop ∨ Tendsto m l (cocompact ℝ)) :
    Tendsto (fun j ↦ TDist.likelihood x (m j) (v j) (a j)) l (𝓝 0)
```

### `UnivariateML.Student2Free.std_all_free_criterion`

A stationary feasible candidate at or above every actual boundary height gives attainment exactly in the absence of a supercritical atom. Uses the positive bijection from standard deviation to ordinary scale, and compares the entire face images, which is equivalent to comparing their suprema. The sufficient derivative-at-normal shortcut is separate. Individual check passed.

```lean
theorem std_all_free_criterion {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) :
    let D : Set (ℝ × ℝ × ℝ) := {p | 0 < p.2.1 ∧ 2 < p.2.2}
    let L := fun p : ℝ × ℝ × ℝ ↦ TDist.likelihood x p.1 p.2.2 p.2.1
    (∃ p ∈ D, IsMaxOn L D p) ↔
      (∀ b, 3 * TDist.multiplicity x b ≤ 2 * n) ∧
      ∃ p ∈ D,
        fderiv ℝ (fun z : ℝ × ℝ × ℝ ↦ TDist.logLik x z.1 z.2.2 z.2.1) p = 0 ∧
        (∀ b s, 0 < s → (∏ i, TDist.normalDensity b s (x i)) ≤ L p) ∧
        (∀ b s, 0 < s → TDist.likelihood x b 2 s ≤ L p) ∧
        (∀ b, TDist.exponent x b 2 = 0 → exp (TDist.collapseLogValue x b 2) ≤ L p)
```

### `UnivariateML.Student2Free.std_all_free_failure`

Unboundedness is equivalent to a supercritical atom. Otherwise the supremum of the Gaussian, Student-two and critical traces is the unattained supremum precisely when all finite fits lose to it. The critical case includes positive-scale Student-two points in the face set, with each such point strictly losing to the critical trace by StudentFaces.lst_critical_joint. Individual check passed.

```lean
theorem std_all_free_failure {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) :
    let D : Set (ℝ × ℝ × ℝ) := {p | 0 < p.2.1 ∧ 2 < p.2.2}
    let L := fun p : ℝ × ℝ × ℝ ↦ TDist.likelihood x p.1 p.2.2 p.2.1
    let F : Set ℝ := {y |
      (∃ b a, 0 < a ∧ y = ∏ i, TDist.normalDensity b a (x i)) ∨
      (∃ b a, 0 < a ∧ y = TDist.likelihood x b 2 a) ∨
      (∃ b, TDist.exponent x b 2 = 0 ∧ y = exp (TDist.collapseLogValue x b 2))}
    ((¬ BddAbove (L '' D)) ↔ ∃ b, TDist.exponent x b 2 < 0) ∧
    ((∀ b, 0 ≤ TDist.exponent x b 2) →
      IsLUB F (sSup F) ∧
      ((¬ ∃ p ∈ D, IsMaxOn L D p) ↔
        IsLUB (L '' D) (sSup F) ∧ ∀ p ∈ D, L p < sSup F))
```

### `UnivariateML.Student2Free.std_held_mean_decision`

At the held mean, a supercritical atom is exactly the unbounded case. At lower mass, a feasible stationary point reaching the matched Gaussian and lower face gives a finite fit. The excluded height is the exact supremum if every finite fit loses. The ordinary scale is free and the face comparison respects the held mean, including constant data away from that mean. Individual check passed.

```lean
theorem std_held_mean_decision {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) (b : ℝ) :
    let D := Ioi (0 : ℝ) ×ˢ Ioi (2 : ℝ)
    let L := fun p : ℝ × ℝ ↦ TDist.likelihood x b p.2 p.1
    let E := TDist.exponent x b 2
    let F : Set ℝ := {y | (∃ a > 0, y = ∏ i, TDist.normalDensity b a (x i)) ∨
      (∃ a > 0, y = TDist.likelihood x b 2 a) ∨
      (E = 0 ∧ y = exp (TDist.collapseLogValue x b 2))}
    ((¬ BddAbove (L '' D)) ↔ E < 0) ∧
    ((∃ p ∈ D, IsMaxOn L D p) ↔ 0 ≤ E ∧ ∃ p ∈ D,
      fderiv ℝ (fun q : ℝ × ℝ ↦ TDist.logLik x b q.2 q.1) p = 0 ∧ sSup F ≤ L p) ∧
    (0 ≤ E → IsLUB F (sSup F) ∧
      ((¬ ∃ p ∈ D, IsMaxOn L D p) ↔
        IsLUB (L '' D) (sSup F) ∧ ∀ p ∈ D, L p < sSup F))
```

### `UnivariateML.Student2Free.std_held_sd_decision`

A supercritical atom gives divergence. Otherwise a stationary finite fit exists exactly when it reaches the same-sd Gaussian and eligible critical collapse heights, including equality. The ordinary scale shrinks at the df-two end, so the positive-scale Student-two face is absent. The statement uses the physical sd hold in its likelihood and derivative. Individual check passed.

```lean
theorem std_held_sd_decision {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) {σ : ℝ} (hσ : 0 < σ) :
    let D := (univ : Set ℝ) ×ˢ Ioi (2 : ℝ)
    let L := fun p : ℝ × ℝ ↦ exp (TDist.stdLogLik x p.1 p.2 σ)
    let E := fun b ↦ TDist.exponent x b 2
    let F : Set ℝ := {y | (∃ b, y = ∏ i, TDist.normalDensity b σ (x i)) ∨
      (∃ b, E b = 0 ∧ y = exp (StudentFaces.fixedSdCollapseLog x b))}
    ((¬ BddAbove (L '' D)) ↔ ∃ b, E b < 0) ∧
    ((∃ p ∈ D, IsMaxOn L D p) ↔ (∀ b, 0 ≤ E b) ∧ ∃ p ∈ D,
      fderiv ℝ (fun q : ℝ × ℝ ↦ TDist.stdLogLik x q.1 q.2 σ) p = 0 ∧ sSup F ≤ L p) ∧
    ((∀ b, 0 ≤ E b) → IsLUB F (sSup F) ∧
      ((¬ ∃ p ∈ D, IsMaxOn L D p) ↔
        IsLUB (L '' D) (sSup F) ∧ ∀ p ∈ D, L p < sSup F))
```

### `UnivariateML.Student2LocationScaleHeldDf.lst_held_df_criterion`

No negative exponent and a stationary candidate reaching all critical collapse heights are equivalent to attainment. The critical subunit case uses compact superlevels. Strict-mass comparisons are vacuous. Individual check passed.

```lean
theorem lst_held_df_criterion {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) {ν : ℝ} (hν : 0 < ν) :
    let D := (univ : Set ℝ) ×ˢ Ioi (0 : ℝ)
    let L := fun p : ℝ × ℝ ↦ TDist.likelihood x p.1 ν p.2
    (∃ p ∈ D, IsMaxOn L D p) ↔
      (∀ b, 0 ≤ TDist.exponent x b ν) ∧
      ∃ p ∈ D, fderiv ℝ (fun q : ℝ × ℝ ↦ TDist.logLik x q.1 ν q.2) p = 0 ∧
        ∀ b, TDist.exponent x b ν = 0 → exp (TDist.collapseLogValue x b ν) ≤ L p
```

### `UnivariateML.Student2LocationScaleHeldDf.lst_held_df_decision`

The joined decision distinguishes all strict masses, the balanced Cauchy pair and a feasible subunit stationary candidate reaching the largest critical height. Every held positive df is included. Individual check passed.

```lean
theorem lst_held_df_decision {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) {ν : ℝ} (hν : 0 < ν) :
    let D := (univ : Set ℝ) ×ˢ Ioi (0 : ℝ)
    let L := fun p : ℝ × ℝ ↦ TDist.likelihood x p.1 ν p.2
    (∃ p ∈ D, IsMaxOn L D p) ↔
      (∀ b, 0 < TDist.exponent x b ν) ∨
      (ν = 1 ∧ ∃ a b, a ≠ b ∧ 2 * TDist.multiplicity x a = n ∧
        ∀ i, x i = a ∨ x i = b) ∨
      (ν < 1 ∧ (∀ b, 0 ≤ TDist.exponent x b ν) ∧
        ∃ p ∈ D, fderiv ℝ (fun q : ℝ × ℝ ↦ TDist.logLik x q.1 ν q.2) p = 0 ∧
          ∀ b, TDist.exponent x b ν = 0 → exp (TDist.collapseLogValue x b ν) ≤ L p)
```

### `UnivariateML.Student2LocationScaleHeldDf.lst_held_df_failure`

Negative exponent is equivalent to global unboundedness. Remote ends and positive-exponent collapses vanish, while critical collapse has a sharp finite vertical trace and joint upper envelope. Moving location and scale at every fixed positive df. Individual check passed.

```lean
theorem lst_held_df_failure {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) {ν : ℝ} (hν : 0 < ν) :
    let L := fun m a ↦ TDist.likelihood x m ν a
    ((¬ BddAbove ((fun p : ℝ × ℝ ↦ L p.1 p.2) '' (univ ×ˢ Ioi 0))) ↔
      ∃ b, TDist.exponent x b ν < 0) ∧
    (∀ b, TDist.exponent x b ν < 0 → Tendsto (L b) (𝓝[>] 0) atTop) ∧
    (∀ {ι : Type*} {l : Filter ι} {m a : ι → ℝ}, (∀ᶠ j in l, 0 < a j) →
      (Tendsto a l atTop ∨ Tendsto m l (cocompact ℝ)) →
      Tendsto (fun j ↦ L (m j) (a j)) l (𝓝 0)) ∧
    (∀ b {ι : Type*} {l : Filter ι} {m a : ι → ℝ},
      Tendsto m l (𝓝 b) → Tendsto a l (𝓝[>] 0) →
      (0 < TDist.exponent x b ν → Tendsto (fun j ↦ L (m j) (a j)) l (𝓝 0)) ∧
      (TDist.exponent x b ν = 0 →
        Tendsto (L b) (𝓝[>] 0) (𝓝 (exp (TDist.collapseLogValue x b ν))) ∧
        ∀ B > exp (TDist.collapseLogValue x b ν), ∀ᶠ j in l, L (m j) (a j) < B))
```

### `UnivariateML.Student2Free.lst_all_free_decision`

The fitted chart includes the Cauchy floor. A strict majority diverges. Otherwise a stationary or Cauchy candidate must reach the Gaussian and critical collapse faces, and equality counts. Ordinary scale and all fitted df at least one. The floor candidate is a conditional nuisance maximizer. Individual check passed.

```lean
theorem lst_all_free_decision {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) :
    let D : Set (ℝ × ℝ × ℝ) := {p | 0 < p.2.1 ∧ 1 ≤ p.2.2}
    let L := fun p : ℝ × ℝ × ℝ ↦ TDist.likelihood x p.1 p.2.2 p.2.1
    let F : Set ℝ := {y | (∃ b s, 0 < s ∧ y = ∏ i, TDist.normalDensity b s (x i)) ∨
      (∃ b, TDist.exponent x b 1 = 0 ∧ y = exp (TDist.collapseLogValue x b 1))}
    ((¬ BddAbove (L '' D)) ↔ ∃ b, n < 2 * TDist.multiplicity x b) ∧
    ((∃ p ∈ D, IsMaxOn L D p) ↔ (∀ b, 2 * TDist.multiplicity x b ≤ n) ∧ ∃ p ∈ D,
      ((1 < p.2.2 ∧
          fderiv ℝ (fun z : ℝ × ℝ × ℝ ↦ TDist.logLik x z.1 z.2.2 z.2.1) p = 0) ∨
        (p.2.2 = 1 ∧ ∀ b s, 0 < s → TDist.likelihood x b 1 s ≤ L p)) ∧ sSup F ≤ L p) ∧
    ((∀ b, 2 * TDist.multiplicity x b ≤ n) → IsLUB F (sSup F) ∧
      ((¬ ∃ p ∈ D, IsMaxOn L D p) ↔ IsLUB (L '' D) (sSup F) ∧ ∀ p ∈ D, L p < sSup F))
```

### `UnivariateML.Student2MovingCollapse.held_location_positive_shape_collapse`

Negative limiting exponent diverges at finite positive limiting shape. A critical exponent diverges or vanishes with the signed shape-log-scale rate. Fixed location and moving ordinary scale and shape; critical rates tending to either infinity are covered. Individual check passed.

```lean
theorem held_location_positive_shape_collapse {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (b : ℝ) {q : ℝ} (hq : 0 < q) {ι : Type*} {l : Filter ι} {a v : ι → ℝ}
    (ha : Tendsto a l (𝓝[>] 0)) (hv : Tendsto v l (𝓝 q)) :
    let L := fun j ↦ TDist.likelihood x b (v j) (a j)
    let rate := fun j ↦ (v j - q) * log (a j)
    (TDist.exponent x b q < 0 → Tendsto L l atTop) ∧
    (TDist.exponent x b q = 0 →
      (Tendsto rate l atTop → Tendsto L l atTop) ∧
      (Tendsto rate l atBot → Tendsto L l (𝓝 0)))
```

### `UnivariateML.Student2MovingCollapse.held_location_zero_shape_vanishes`

At small df the joint likelihood vanishes for tie-free data at any positive scale, and for all data when scale stays bounded away from zero. Ordinary scale, held location and arbitrary positive-shape paths. Individual check passed.

```lean
theorem held_location_zero_shape_vanishes {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (b : ℝ) {ι : Type*} {l : Filter ι} {a v : ι → ℝ}
    (ha : ∀ᶠ j in l, 0 < a j) (hv : Tendsto v l (𝓝[>] 0))
    (h : (∀ i, x i ≠ b) ∨ ∃ s > 0, ∀ᶠ j in l, s ≤ a j) :
    Tendsto (fun j ↦ TDist.likelihood x b (v j) (a j)) l (𝓝 0)
```

### `UnivariateML.Student2MovingCollapse.held_location_all_tied_normal_corner`

At the all-tied normal corner every positive-scale approach to zero scale and infinite df diverges. The tie location is held. Individual check passed.

```lean
theorem held_location_all_tied_normal_corner {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (b : ℝ) (hx : ∀ i, x i = b) {ι : Type*} {l : Filter ι} {a v : ι → ℝ}
    (ha : Tendsto a l (𝓝[>] 0)) (hv : Tendsto v l atTop) :
    Tendsto (fun j ↦ TDist.likelihood x b (v j) (a j)) l atTop
```

### `UnivariateML.Student2MovingCollapse.held_location_zero_shape_divergent_path`

A contact at held location produces a divergent path with df and scale both tending to zero. The exponential scale path works for any positive contact count, including the all-tied sample. Individual check passed.

```lean
theorem held_location_zero_shape_divergent_path {n : ℕ} (x : Fin n → ℝ) (b : ℝ)
    (hk : 0 < TDist.multiplicity x b) :
    Tendsto (fun t : ℝ ↦ TDist.likelihood x b t (exp (-(t^2)⁻¹))) (𝓝[>] 0) atTop
```

### `UnivariateML.Student2MovingCollapse.held_location_critical_below_path`

A critical positive shape has a divergent path from below when df is unrestricted. The location is held and the path is eventually positive in both df and scale. Individual check passed.

```lean
theorem held_location_critical_below_path {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (b : ℝ) {q : ℝ} (hq : 0 < q) (he : TDist.exponent x b q = 0) :
    Tendsto (fun t : ℝ ↦ TDist.likelihood x b (q-t) (exp (-(t^2)⁻¹))) (𝓝[>] 0) atTop
```

### `UnivariateML.Student2MovingCollapse.lst_held_location_failure`

At held location, contacts give divergence and the only surviving tie-free face is Gaussian. The joined statement specifies all shape and scale corners, including a divergent path from below a critical positive df. With no finite attainment and no contacts the exact supremum is the RMS Gaussian height. The separate signed-rate theorem gives the critical path behavior. Individual check passed.

```lean
theorem lst_held_location_failure {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) (b : ℝ) :
    let D := Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ)
    let L := fun v a ↦ TDist.likelihood x b v a
    ((¬ BddAbove ((fun p : ℝ × ℝ ↦ L p.1 p.2) '' D)) ↔ ∃ i, x i = b) ∧
    ((∀ i, x i ≠ b) → (¬ ∃ p ∈ D, IsMaxOn (fun q : ℝ × ℝ ↦ L q.1 q.2) D p) →
      IsLUB ((fun p : ℝ × ℝ ↦ L p.1 p.2) '' D) (SkewLSHalfT.normalFace x b / (2 : ℝ)^n)) ∧
    (∀ {ι : Type*} {l : Filter ι} {a v : ι → ℝ}, Tendsto a l atTop →
      (∀ᶠ j in l, 0 < v j) → Tendsto (fun j ↦ L (v j) (a j)) l (𝓝 0)) ∧
    (∀ {ι : Type*} {l : Filter ι} {a v : ι → ℝ}, (∀ᶠ j in l, 0 < a j) →
      Tendsto v l (𝓝[>] 0) →
      ((∀ i, x i ≠ b) ∨ ∃ s > 0, ∀ᶠ j in l, s ≤ a j) →
      Tendsto (fun j ↦ L (v j) (a j)) l (𝓝 0)) ∧
    ((∃ i, x i = b) → Tendsto (fun t : ℝ ↦ L t (exp (-(t^2)⁻¹))) (𝓝[>] 0) atTop) ∧
    (∀ q > 0, TDist.exponent x b q = 0 →
      Tendsto (fun t : ℝ ↦ L (q-t) (exp (-(t^2)⁻¹))) (𝓝[>] 0) atTop) ∧
    (∀ {ι : Type*} {l : Filter ι} {a v : ι → ℝ} {q : ℝ}, 0 < q →
      Tendsto a l (𝓝[>] 0) → Tendsto v l (𝓝 q) →
      ((0 < TDist.exponent x b q → Tendsto (fun j ↦ L (v j) (a j)) l (𝓝 0)) ∧
        (TDist.exponent x b q < 0 → Tendsto (fun j ↦ L (v j) (a j)) l atTop))) ∧
    (∀ {ι : Type*} {l : Filter ι} {a v : ι → ℝ} {s : ℝ}, 0 < s →
      Tendsto a l (𝓝 s) → Tendsto v l atTop →
      Tendsto (fun j ↦ L (v j) (a j)) l (𝓝 (∏ i, TDist.normalDensity b s (x i)))) ∧
    (∀ {ι : Type*} {l : Filter ι} {a v : ι → ℝ}, Tendsto a l (𝓝[>] 0) →
      Tendsto v l atTop →
      ((∃ i, x i ≠ b) → Tendsto (fun j ↦ L (v j) (a j)) l (𝓝 0)) ∧
      ((∀ i, x i = b) → Tendsto (fun j ↦ L (v j) (a j)) l atTop))
```

### `UnivariateML.Student2Free.lst_held_scale_decision`

The only competitive excluded face is the Gaussian at the supplied scale with mean at the sample average. A stationary point or a conditional Cauchy maximum reaches the face exactly when a fit exists. The fitted df domain includes one. Positive-scale constant samples and finite face equality are included. Individual check passed.

```lean
theorem lst_held_scale_decision {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) {s : ℝ} (hs : 0 < s) :
    let D := (univ : Set ℝ) ×ˢ Ici (1 : ℝ)
    let L := fun p : ℝ × ℝ ↦ TDist.likelihood x p.1 p.2 s
    let N := ∏ i, TDist.normalDensity ((∑ j, x j) / n) s (x i)
    BddAbove (L '' D) ∧
    ((∃ p ∈ D, IsMaxOn L D p) ↔ ∃ p ∈ D,
      ((1 < p.2 ∧ fderiv ℝ (fun q : ℝ × ℝ ↦ TDist.logLik x q.1 q.2 s) p = 0) ∨
        (p.2 = 1 ∧ ∀ b, TDist.likelihood x b 1 s ≤ L p)) ∧ N ≤ L p) ∧
    ((¬ ∃ p ∈ D, IsMaxOn L D p) ↔ IsLUB (L '' D) N ∧ ∀ p ∈ D, L p < N)
```

### `UnivariateML.Student2MovingCollapse.critical_vertical_finite_rate`

A finite shape-log-scale rate shifts the log critical height by the off-atom count times the rate. Held location and any positive finite critical df, including approach from either side. Individual check passed.

```lean
theorem critical_vertical_finite_rate {n : ℕ} (x : Fin n → ℝ) (b : ℝ) {q : ℝ} (hq : 0 < q)
    (he : TDist.exponent x b q = 0) {ι : Type*} {l : Filter ι} {a v : ι → ℝ}
    (ha : Tendsto a l (𝓝[>] 0)) (hv : Tendsto v l (𝓝 q))
    {r : ℝ} (hr : Tendsto (fun j ↦ (v j - q) * log (a j)) l (𝓝 r)) :
    Tendsto (fun j ↦ TDist.likelihood x b (v j) (a j)) l
      (𝓝 (exp (((n : ℝ) - TDist.multiplicity x b) * r + TDist.collapseLogValue x b q)))
```

### `UnivariateML.Student2MovingCollapse.half_held_endpoint_failure`

A held-endpoint contact causes divergence. Without contacts, the only positive excluded face is the half-normal, optimized at the RMS scale. For a compatible endpoint, the theorem specifies the joint shape-and-scale limits and critical finite-rate layer. The support hold is explicit. Individual check passed.

```lean
theorem half_held_endpoint_failure {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) (b : ℝ)
    (_hsupport : ∀ i, b ≤ x i) :
    let D := Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ)
    let L := fun v a ↦ SkewLSHalfT.likelihood x b v a
    ((¬ BddAbove ((fun p : ℝ × ℝ ↦ L p.1 p.2) '' D)) ↔ ∃ i, x i = b) ∧
    ((∀ i, x i ≠ b) → (¬ ∃ p ∈ D, IsMaxOn (fun q : ℝ × ℝ ↦ L q.1 q.2) D p) →
      IsLUB ((fun p : ℝ × ℝ ↦ L p.1 p.2) '' D) (SkewLSHalfT.normalFace x b)) ∧
    (∀ {ι : Type*} {l : Filter ι} {a v : ι → ℝ}, Tendsto a l atTop →
      (∀ᶠ j in l, 0 < v j) → Tendsto (fun j ↦ L (v j) (a j)) l (𝓝 0)) ∧
    (∀ {ι : Type*} {l : Filter ι} {a v : ι → ℝ}, (∀ᶠ j in l, 0 < a j) →
      Tendsto v l (𝓝[>] 0) →
      ((∀ i, x i ≠ b) ∨ ∃ s > 0, ∀ᶠ j in l, s ≤ a j) →
      Tendsto (fun j ↦ L (v j) (a j)) l (𝓝 0)) ∧
    ((∃ i, x i = b) → Tendsto (fun t : ℝ ↦ L t (exp (-(t^2)⁻¹))) (𝓝[>] 0) atTop) ∧
    (∀ {ι : Type*} {l : Filter ι} {a v : ι → ℝ} {q : ℝ}, 0 < q →
      Tendsto a l (𝓝[>] 0) → Tendsto v l (𝓝 q) →
      ((0 < TDist.exponent x b q → Tendsto (fun j ↦ L (v j) (a j)) l (𝓝 0)) ∧
        (TDist.exponent x b q < 0 → Tendsto (fun j ↦ L (v j) (a j)) l atTop) ∧
        (TDist.exponent x b q = 0 → ∀ r : ℝ,
          Tendsto (fun j ↦ (v j-q) * log (a j)) l (𝓝 r) →
          Tendsto (fun j ↦ L (v j) (a j)) l
            (𝓝 ((2 : ℝ)^n * exp (((n : ℝ) - TDist.multiplicity x b) * r +
              TDist.collapseLogValue x b q)))))) ∧
    (∀ {ι : Type*} {l : Filter ι} {a v : ι → ℝ} {s : ℝ}, 0 < s →
      Tendsto a l (𝓝 s) → Tendsto v l atTop →
      Tendsto (fun j ↦ L (v j) (a j)) l (𝓝 ((2 : ℝ)^n * ∏ i, TDist.normalDensity b s (x i)))) ∧
    (∀ {ι : Type*} {l : Filter ι} {a v : ι → ℝ}, Tendsto a l (𝓝[>] 0) →
      Tendsto v l atTop →
      ((∃ i, x i ≠ b) → Tendsto (fun j ↦ L (v j) (a j)) l (𝓝 0)) ∧
      ((∀ i, x i = b) → Tendsto (fun j ↦ L (v j) (a j)) l atTop))
```

### `UnivariateML.Student2Skew.fitted_majority_and_vanishing_ends`

On the fitted df branch, a strict majority is exactly the unbounded branch. Remote and strictly subcritical ends vanish uniformly in slant. The upper estimate is the ordinary Student product times two to the sample size. Positive half-faces and optimized critical displacement are separate. Individual check passed.

```lean
theorem fitted_majority_and_vanishing_ends {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) :
    let D : Set (ℝ × ℝ × ℝ × ℝ) := {p | 0 < p.2.1 ∧ 1 ≤ p.2.2.1}
    let L := fun p : ℝ × ℝ × ℝ × ℝ ↦ StudentSkew.likelihood x p.1 p.2.2.1 p.2.2.2 p.2.1
    ((¬ BddAbove (L '' D)) ↔ ∃ b, n < 2 * TDist.multiplicity x b) ∧
    (∀ {ι : Type*} {l : Filter ι} {m a v α : ι → ℝ},
      (∀ᶠ j in l, 0 < a j) → (∀ᶠ j in l, 1 ≤ v j) →
      (Tendsto a l atTop ∨ Tendsto m l (cocompact ℝ)) →
      Tendsto (fun j ↦ StudentSkew.likelihood x (m j) (v j) (α j) (a j)) l (𝓝 0)) ∧
    (∀ {ι : Type*} {l : Filter ι} {m a v α : ι → ℝ} {b q : ℝ},
      Tendsto m l (𝓝 b) → Tendsto a l (𝓝[>] 0) → Tendsto v l (𝓝 q) →
      0 < q → 0 < TDist.exponent x b q →
      Tendsto (fun j ↦ StudentSkew.likelihood x (m j) (v j) (α j) (a j)) l (𝓝 0)) ∧
    (∀ {ι : Type*} {l : Filter ι} {m a v α : ι → ℝ} {b : ℝ},
      Tendsto m l (𝓝 b) → Tendsto a l (𝓝[>] 0) → Tendsto v l atTop →
      (∃ i, x i ≠ b) →
      Tendsto (fun j ↦ StudentSkew.likelihood x (m j) (v j) (α j) (a j)) l (𝓝 0))
```

### `UnivariateML.Student2Skew.moving_skew_normal_face`

The infinite-df face with finite slant and positive scale is the matched skew-normal along jointly moving nuisance paths. Both finite location and scale are allowed to move, as is slant. Individual check passed.

```lean
theorem moving_skew_normal_face {n : ℕ} (x : Fin n → ℝ)
    {ι : Type*} {l : Filter ι} {m a v α : ι → ℝ} {b s β : ℝ}
    (hs : 0 < s) (hm : Tendsto m l (𝓝 b)) (ha : Tendsto a l (𝓝 s))
    (hv : Tendsto v l atTop) (hα : Tendsto α l (𝓝 β)) :
    Tendsto (fun j ↦ likelihood x (m j) (v j) (α j) (a j)) l
      (𝓝 ((∏ i, TDist.normalDensity b s (x i)) *
        ∏ i, 2 * SkewLSGaussian.Φ (β * ((x i-b)/s))))
```

### `UnivariateML.Student2Skew.moving_finite_skew_face`

The joint skew Student likelihood is continuous when all parameters converge at admissible finite df and positive scale. A local Student envelope dominates the CDF integral even for held subunit df. Individual check passed.

```lean
theorem moving_finite_skew_face {n : ℕ} (x : Fin n → ℝ)
    {ι : Type*} {l : Filter ι} {m a v α : ι → ℝ} {b s q β : ℝ}
    (hs : 0 < s) (hq : 0 < q)
    (hm : Tendsto m l (𝓝 b)) (ha : Tendsto a l (𝓝 s))
    (hv : Tendsto v l (𝓝 q)) (hα : Tendsto α l (𝓝 β)) :
    Tendsto (fun j ↦ likelihood x (m j) (v j) (α j) (a j)) l
      (𝓝 (likelihood x b q β s))
```

### `UnivariateML.Student2Skew.moving_oriented_faces`

The positive-scale slant limits are the oriented half-Student or half-normal products with the limiting contact factor. Both orientations, moving location and scale, and finite or infinite contact rate at finite or infinite df. Individual check passed.

```lean
theorem moving_oriented_faces {n : ℕ} (x : Fin n → ℝ)
    {ι : Type*} {l : Filter ι} {m a v α : ι → ℝ} {b s ε : ℝ} {t : EReal}
    (hs : 0 < s) (hm : Tendsto m l (𝓝 b)) (ha : Tendsto a l (𝓝 s))
    (hε : ε^2 = 1) (hα : Tendsto (fun j ↦ ε * α j) l atTop)
    (hc : Tendsto (fun j ↦ ((α j * (b-m j) *
      sqrt ((v j+1)/(v j*(a j)^2+(b-m j)^2)) : ℝ) : EReal)) l (𝓝 t)) :
    (∀ q > 0, Tendsto v l (𝓝 q) →
      Tendsto (fun j ↦ likelihood x (m j) (v j) (α j) (a j)) l
        (𝓝 (TDist.likelihood x b q s *
          ∏ i, if 0 < ε*(x i-b) then 2 else if x i = b then
            EReal.rec 0 (fun u ↦ 2 * studentCDF (q+1) u) 2 t else 0))) ∧
    (Tendsto v l atTop → Tendsto (fun j ↦ likelihood x (m j) (v j) (α j) (a j)) l
      (𝓝 ((∏ i, TDist.normalDensity b s (x i)) *
        ∏ i, if 0 < ε*(x i-b) then 2 else if x i = b then
          EReal.rec 0 (fun u ↦ 2 * SkewLSGaussian.Φ u) 2 t else 0)))
```

### `UnivariateML.Student2MovingCollapse.moving_critical_atom_base`

The symmetric base of the skew Student likelihood has an exact critical atom layer with both displacement and df rate. Infinite displacement at a shape floor or a negative infinite rate gives zero. This is the symmetric product. Combining with the skew CDF factor supplies the skew layer. Individual check passed.

```lean
theorem moving_critical_atom_base {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (b : ℝ) {q : ℝ} (hq : 0 < q) (he : TDist.exponent x b q = 0)
    {ι : Type*} {l : Filter ι} {m a v : ι → ℝ}
    (hm : Tendsto m l (𝓝 b)) (ha : Tendsto a l (𝓝[>] 0)) (hv : Tendsto v l (𝓝 q)) :
    let L := fun j ↦ TDist.likelihood x (m j) (v j) (a j)
    let δ := fun j ↦ (m j-b)/a j
    let ρ := fun j ↦ (v j-q)*log (a j)
    let k : ℝ := TDist.multiplicity x b
    (∀ d r : ℝ, Tendsto δ l (𝓝 d) → Tendsto ρ l (𝓝 r) →
      Tendsto L l (𝓝 (exp ((n-k)*r + TDist.collapseLogValue x b q -
        (q+1)/2*k*log (1+d^2/q))))) ∧
    (Tendsto ρ l atBot → Tendsto L l (𝓝 0)) ∧
    ((∀ᶠ j in l, q ≤ v j) → Tendsto (fun j ↦ (δ j)^2) l atTop →
      Tendsto L l (𝓝 0))
```

### `UnivariateML.Student2Skew.moving_critical_skew_faces`

At a critical atom, the finite- and infinite-slant joint layer values are the symmetric displacement-and-rate trace times the corresponding CDF factor. Finite scaled displacement and finite signed df rate. The infinite-slant clause allows infinite contact rates as well. Individual check passed.

```lean
theorem moving_critical_skew_faces {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    {ι : Type*} {l : Filter ι} {m a v α : ι → ℝ} {b q d r : ℝ}
    (hq : 0 < q) (he : TDist.exponent x b q = 0)
    (hm : Tendsto m l (𝓝 b)) (ha : Tendsto a l (𝓝[>] 0)) (hv : Tendsto v l (𝓝 q))
    (hd : Tendsto (fun j ↦ (m j-b)/a j) l (𝓝 d))
    (hr : Tendsto (fun j ↦ (v j-q)*log (a j)) l (𝓝 r)) :
    let K := exp (((n : ℝ) - TDist.multiplicity x b)*r + TDist.collapseLogValue x b q -
      (q+1)/2 * TDist.multiplicity x b * log (1+d^2/q))
    let L := fun j ↦ likelihood x (m j) (v j) (α j) (a j)
    (∀ β, Tendsto α l (𝓝 β) → Tendsto L l (𝓝 (K * atomSlantFactor x b q β d))) ∧
    (∀ ε : ℝ, ε^2 = 1 → Tendsto (fun j ↦ ε * α j) l atTop →
      ∀ t : EReal, Tendsto (fun j ↦ ((α j * (b-m j) *
        sqrt ((v j+1)/(v j*(a j)^2+(b-m j)^2)) : ℝ) : EReal)) l (𝓝 t) →
      Tendsto L l (𝓝 (K * ∏ i, if 0 < ε*(x i-b) then 2 else if x i = b then
        EReal.rec 0 (fun u ↦ 2 * studentCDF (q+1) u) 2 t else 0)))
```

### `UnivariateML.Student2Skew.fitted_all_free_decision`

Strict majority divergence, the exact finite-candidate comparison, and the unattained excluded supremum are joined on fitted df at least one. Face heights use sharp moving contacts and both critical slant classes. The proof exhausts all escapes by compactness of every superlevel strictly above them. Stationarity is expressed using fderiv. Individual check passed.

```lean
theorem fitted_all_free_decision {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) :
    let D : Set (ℝ × ℝ × ℝ × ℝ) := {p | 0 < p.2.1 ∧ 1 ≤ p.2.2.1}
    let L := fun p : ℝ × ℝ × ℝ × ℝ ↦ likelihood x p.1 p.2.2.1 p.2.2.2 p.2.1
    let B := sSup (fittedFaces x)
    ((¬ BddAbove (L '' D)) ↔ ∃ b, n < 2 * TDist.multiplicity x b) ∧
    ((∃ p ∈ D, IsMaxOn L D p) ↔ (∀ b, 2 * TDist.multiplicity x b ≤ n) ∧ ∃ p ∈ D,
      ((1 < p.2.2.1 ∧ fderiv ℝ (fun q ↦ log (L q)) p = 0) ∨
        (p.2.2.1 = 1 ∧ ∀ b a β, 0 < a → likelihood x b 1 β a ≤ L p)) ∧ B ≤ L p) ∧
    ((∀ b, 2 * TDist.multiplicity x b ≤ n) →
      IsLUB (fittedFaces x) B ∧ (∀ A > B, IsCompact {p ∈ D | A ≤ L p}) ∧
      ((¬ ∃ p ∈ D, IsMaxOn L D p) ↔ IsLUB (L '' D) B ∧ ∀ p ∈ D, L p < B))
```

### `UnivariateML.Student2Skew.held_alpha_decision`

The fixed-slant normal and critical displacement heights give the exact finite-value comparison, including equality. The atom test uses the fitted df-one floor. Individual check passed.

```lean
theorem held_alpha_decision {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (β : ℝ) :
    let D := fixedSlantDomain none none
    let L := fun p : ℝ × ℝ × ℝ ↦ likelihood x p.1 p.2.2 β p.2.1
    let E := fun b ↦ TDist.exponent x b 1
    let F := fixedSlantFaces x β none none
    let B := sSup F
    ((¬ BddAbove (L '' D)) ↔ ∃ b ∈ locations none, E b < 0) ∧
    ((∃ p ∈ D, IsMaxOn L D p) ↔ (∀ b ∈ locations none, 0 ≤ E b) ∧
      ∃ p ∈ D, fixedSlantCandidate x β none none p ∧ B ≤ L p) ∧
    ((∀ b ∈ locations none, 0 ≤ E b) → IsLUB F B ∧
      (∀ A > B, IsCompact {p ∈ D | A ≤ L p}) ∧
      ((¬ ∃ p ∈ D, IsMaxOn L D p) ↔ IsLUB (L '' D) B ∧ ∀ p ∈ D, L p < B))
```

### `UnivariateML.Student2Skew.held_alpha_df_decision`

With slant and df held, a negative exponent diverges and a critical displacement is the only nonzero excluded height. All positive held df are included. Strict mass gives attainment since the excluded height is zero. Individual check passed.

```lean
theorem held_alpha_df_decision {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (β : ℝ) {q : ℝ} (hq : 0 < q) :
    let D := fixedSlantDomain none (some q)
    let L := fun p : ℝ × ℝ × ℝ ↦ likelihood x p.1 p.2.2 β p.2.1
    let E := fun b ↦ TDist.exponent x b q
    let F := fixedSlantFaces x β none (some q)
    let B := sSup F
    ((¬ BddAbove (L '' D)) ↔ ∃ b ∈ locations none, E b < 0) ∧
    ((∃ p ∈ D, IsMaxOn L D p) ↔ (∀ b ∈ locations none, 0 ≤ E b) ∧
      ∃ p ∈ D, fixedSlantCandidate x β none (some q) p ∧ B ≤ L p) ∧
    ((∀ b ∈ locations none, 0 ≤ E b) → IsLUB F B ∧
      (∀ A > B, IsCompact {p ∈ D | A ≤ L p}) ∧
      ((¬ ∃ p ∈ D, IsMaxOn L D p) ↔ IsLUB (L '' D) B ∧ ∀ p ∈ D, L p < B))
```

### `UnivariateML.Student2Skew.held_alpha_location_decision`

With slant and location held, the matched skew-normal and held-contact critical collapse supply the exact comparison. Only exponent at the held location is tested, and a feasible df-floor candidate is included. Individual check passed.

```lean
theorem held_alpha_location_decision {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (β b : ℝ) :
    let D := fixedSlantDomain (some b) none
    let L := fun p : ℝ × ℝ × ℝ ↦ likelihood x p.1 p.2.2 β p.2.1
    let E := fun b ↦ TDist.exponent x b 1
    let F := fixedSlantFaces x β (some b) none
    let B := sSup F
    ((¬ BddAbove (L '' D)) ↔ ∃ y ∈ locations (some b), E y < 0) ∧
    ((∃ p ∈ D, IsMaxOn L D p) ↔ (∀ y ∈ locations (some b), 0 ≤ E y) ∧
      ∃ p ∈ D, fixedSlantCandidate x β (some b) none p ∧ B ≤ L p) ∧
    ((∀ y ∈ locations (some b), 0 ≤ E y) → IsLUB F B ∧
      (∀ A > B, IsCompact {p ∈ D | A ≤ L p}) ∧
      ((¬ ∃ p ∈ D, IsMaxOn L D p) ↔ IsLUB (L '' D) B ∧ ∀ p ∈ D, L p < B))
```

### `UnivariateML.Student2Skew.fixed_slant_decision`

The common held-slant theorem specializes to all three requested fixed-slant masks. Optional location and shape restrict both feasible points and escape faces. Individual check passed.

```lean
theorem fixed_slant_decision {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (β : ℝ) (μ ν : Option ℝ) (hq : 0 < ν.getD 1) :
    let D := fixedSlantDomain μ ν
    let L := fun p : ℝ × ℝ × ℝ ↦ likelihood x p.1 p.2.2 β p.2.1
    let E := fun b ↦ TDist.exponent x b (ν.getD 1)
    let F := fixedSlantFaces x β μ ν
    let B := sSup F
    ((¬ BddAbove (L '' D)) ↔ ∃ b ∈ locations μ, E b < 0) ∧
    ((∃ p ∈ D, IsMaxOn L D p) ↔ (∀ b ∈ locations μ, 0 ≤ E b) ∧
      ∃ p ∈ D, fixedSlantCandidate x β μ ν p ∧ B ≤ L p) ∧
    ((∀ b ∈ locations μ, 0 ≤ E b) → IsLUB F B ∧
      (∀ A > B, IsCompact {p ∈ D | A ≤ L p}) ∧
      ((¬ ∃ p ∈ D, IsMaxOn L D p) ↔ IsLUB (L '' D) B ∧ ∀ p ∈ D, L p < B))
```

### `UnivariateML.Student2Skew.held_df_decision`

Held positive df leaves oriented half-t and critical collapse as the eligible nonzero excluded heights. The candidate comparison and equality rule are exact. Location, scale and slant are free. The sharp moving-contact factor is two. Individual check passed.

```lean
theorem held_df_decision {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) {q : ℝ} (hq : 0 < q) :
    let D := freeSlantDomain none (some q) none
    let L := fun p : ℝ × ℝ × ℝ × ℝ ↦ likelihood x p.1 p.2.2.1 p.2.2.2 p.2.1
    let E := fun b ↦ TDist.exponent x b q
    let F := freeSlantFaces x none (some q) none
    let B := sSup F
    ((¬ BddAbove (L '' D)) ↔ (none : Option ℝ) = none ∧ ∃ b ∈ locations none, E b < 0) ∧
    ((∃ p ∈ D, IsMaxOn L D p) ↔ ((none : Option ℝ) = none → ∀ b ∈ locations none, 0 ≤ E b) ∧
      ∃ p ∈ D, freeSlantCandidate x none (some q) none p ∧ B ≤ L p) ∧
    (((none : Option ℝ) = none → ∀ b ∈ locations none, 0 ≤ E b) → IsLUB F B ∧
      (∀ A > B, IsCompact {p ∈ D | A ≤ L p}) ∧
      ((¬ ∃ p ∈ D, IsMaxOn L D p) ↔ IsLUB (L '' D) B ∧ ∀ p ∈ D, L p < B))
```

### `UnivariateML.Student2Skew.free_slant_decision`

The common decision with slant free removes each ineligible face under nuisance holds. Includes held df, location and scale in arbitrary combinations, with positive held df and scale. Individual check passed.

```lean
theorem free_slant_decision {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (μ ν ω : Option ℝ) (hq : 0 < ν.getD 1) (hs : 0 < ω.getD 1) :
    let D := freeSlantDomain μ ν ω
    let L := fun p : ℝ × ℝ × ℝ × ℝ ↦ likelihood x p.1 p.2.2.1 p.2.2.2 p.2.1
    let E := fun b ↦ TDist.exponent x b (ν.getD 1)
    let F := freeSlantFaces x μ ν ω
    let B := sSup F
    ((¬ BddAbove (L '' D)) ↔ ω = none ∧ ∃ b ∈ locations μ, E b < 0) ∧
    ((∃ p ∈ D, IsMaxOn L D p) ↔ (ω = none → ∀ b ∈ locations μ, 0 ≤ E b) ∧
      ∃ p ∈ D, freeSlantCandidate x μ ν ω p ∧ B ≤ L p) ∧
    ((ω = none → ∀ b ∈ locations μ, 0 ≤ E b) → IsLUB F B ∧
      (∀ A > B, IsCompact {p ∈ D | A ≤ L p}) ∧
      ((¬ ∃ p ∈ D, IsMaxOn L D p) ↔ IsLUB (L '' D) B ∧ ∀ p ∈ D, L p < B))
```

### `UnivariateML.Student2Skew.student_reverse_hazard_bound`

The Student reverse hazard satisfies the strict power-tail inequality at every finite argument. The bound controls the sign of each noncontact contribution in the critical skew scale derivative. Individual check passed.

```lean
theorem student_reverse_hazard_bound {q : ℝ} (hq : 0 < q) (z : ℝ) :
    0 < q + z * ((TDist.density 0 q 1 z / ∫ y, TDist.density 0 q 1 y) / studentCDF q z)
```

### `UnivariateML.Student2Skew.held_location_critical_strict_scale`

At critical mass, every fixed finite slant strictly loses as positive scale increases. The proof differentiates the exact log likelihood and uses the strict Student reverse-hazard bound. Individual check passed.

```lean
theorem held_location_critical_strict_scale {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (b α : ℝ) {q : ℝ} (hq : 0 < q) (he : TDist.exponent x b q = 0) :
    StrictAntiOn (likelihood x b q α) (Ioi 0)
```

### `UnivariateML.Student2Skew.held_df_location_decision`

At held df and location, attainment is equivalent to positive exponent and both residual signs. Negative exponent diverges. Critical exponent and the single-sign positive case have an unattained excluded supremum. Both orientations and all contact counts are included, and the eligible symmetric conditional scale is unique. Individual check passed.

```lean
theorem held_df_location_decision {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (b : ℝ) {q : ℝ} (hq : 0 < q) :
    let D := freeSlantDomain (some b) (some q) none
    let L := fun p : ℝ × ℝ × ℝ × ℝ ↦ likelihood x p.1 p.2.2.1 p.2.2.2 p.2.1
    let E := TDist.exponent x b q
    let mixed := (∃ i, b < x i) ∧ (∃ i, x i < b)
    let B := sSup (freeSlantFaces x (some b) (some q) none)
    ((∃ p ∈ D, IsMaxOn L D p) ↔ 0 < E ∧ mixed) ∧
    ((¬ BddAbove (L '' D)) ↔ E < 0) ∧
    (E = 0 ∨ (0 < E ∧ ¬ mixed) →
      IsLUB (L '' D) B ∧ ∀ p ∈ D, L p < B) ∧
    (0 < E → ∃! a, 0 < a ∧ TDist.scaleScore x b q a = 0)
```

### `UnivariateML.Student2Skew.held_scale_decision`

At positive held scale the likelihood is bounded and the matched skew-normal and both oriented half families supply the excluded heights. The face comparison includes equality, constant data, and moving-location contacts. The collapse branches are absent. Individual check passed.

```lean
theorem held_scale_decision {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) {s : ℝ} (hs : 0 < s) :
    let D := freeSlantDomain none none (some s)
    let L := fun p : ℝ × ℝ × ℝ × ℝ ↦ likelihood x p.1 p.2.2.1 p.2.2.2 p.2.1
    let F := freeSlantFaces x none none (some s)
    let B := sSup F
    BddAbove (L '' D) ∧
    ((∃ p ∈ D, IsMaxOn L D p) ↔ ∃ p ∈ D, freeSlantCandidate x none none (some s) p ∧ B ≤ L p) ∧
    IsLUB F B ∧ (∀ A > B, IsCompact {p ∈ D | A ≤ L p}) ∧
      ((¬ ∃ p ∈ D, IsMaxOn L D p) ↔ IsLUB (L '' D) B ∧ ∀ p ∈ D, L p < B)
```

### `UnivariateML.Student2Skew.held_location_decision`

At held location a majority of contacts gives divergence. Otherwise the matched normal, oriented half faces and critical held-location collapse serve in the exact comparison. The contact weight is one for the one-sided faces. Only a collapse at the held location is reachable. Individual check passed.

```lean
theorem held_location_decision {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) (b : ℝ) :
    let D := freeSlantDomain (some b) none none
    let L := fun p : ℝ × ℝ × ℝ × ℝ ↦ likelihood x p.1 p.2.2.1 p.2.2.2 p.2.1
    let E := TDist.exponent x b 1
    let F := freeSlantFaces x (some b) none none
    let B := sSup F
    ((¬ BddAbove (L '' D)) ↔ E < 0) ∧
    ((∃ p ∈ D, IsMaxOn L D p) ↔ 0 ≤ E ∧
      ∃ p ∈ D, freeSlantCandidate x (some b) none none p ∧ B ≤ L p) ∧
    (0 ≤ E → IsLUB F B ∧ (∀ A > B, IsCompact {p ∈ D | A ≤ L p}) ∧
      ((¬ ∃ p ∈ D, IsMaxOn L D p) ↔ IsLUB (L '' D) B ∧ ∀ p ∈ D, L p < B))
```

### `UnivariateML.Student2LocationScore.two_level_cost_classification`

The weighted two-level Student cost has the exact reflected pair at balanced wide gaps. Its minimizer is unique in every other case, including zero counts and zero gap. The variable Q is the squared kernel width, and the levels are translated to zero and D. Individual check passed.

```lean
theorem two_level_cost_classification {Q D k l : ℝ} (hQ : 0 < Q) (hD : 0 ≤ D)
    (hk : 0 ≤ k) (hl : 0 ≤ l) (hn : 0 < k+l) :
    ((k = l ∧ Q < (D/2)^2) →
      ∀ m, IsMinOn (twoLevelCost Q D k l) univ m ↔
        m = D/2 + sqrt ((D/2)^2-Q) ∨ m = D/2 - sqrt ((D/2)^2-Q)) ∧
    (¬ (k = l ∧ Q < (D/2)^2) → ∃! m, IsMinOn (twoLevelCost Q D k l) univ m) ∧
    (k = 0 → ∀ m, IsMinOn (twoLevelCost Q D k l) univ m ↔ m = D) ∧
    (l = 0 → ∀ m, IsMinOn (twoLevelCost Q D k l) univ m ↔ m = 0)
```

### `UnivariateML.Student2LocationScore.std_two_level_all_counts`

For any sample supported on two ordered levels, the maximum is a singleton except at balanced wide gaps, when it is exactly the stated reflected pair. All multiplicities, zero-count singleton strata, and the threshold equality are included. Individual check passed.

```lean
theorem std_two_level_all_counts {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    {b c ν σ : ℝ} (hbc : b < c) (hν : 2 < ν) (hσ : 0 < σ)
    (hx : ∀ i, x i = b ∨ x i = c) :
    let Q := (ν-2)*σ^2
    let h := (c-b)/2
    let k := TDist.multiplicity x b
    let l := TDist.multiplicity x c
    let best := fun m ↦ IsMaxOn (fun u ↦ TDist.stdLogLik x u ν σ) univ m
    ((k = l ∧ Q < h^2) →
      ∀ m, best m ↔ m = b+h+sqrt (h^2-Q) ∨ m = b+h-sqrt (h^2-Q)) ∧
    (¬ (k = l ∧ Q < h^2) → ∃! m, best m) ∧
    (k = 0 → ∀ m, best m ↔ m = c) ∧ (l = 0 → ∀ m, best m ↔ m = b)
```

### `UnivariateML.Student2LocationScore.lst_held_shape_scale_finite_roots`

Every held-df/scale location maximum is exactly a real root minimizing the kernel product. The polynomial is nonzero and its real roots form a finite nonempty set. All nonempty samples, including constants and arbitrary repeated levels. Individual check passed.

```lean
theorem lst_held_shape_scale_finite_roots {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    {ν s : ℝ} (hν : 0 < ν) (hs : 0 < s) :
    let Q := ν*s^2
    let P := locationNumerator x Q
    let R := {m : ℝ | P.IsRoot m}
    let K := fun m ↦ ∏ i, ((m-x i)^2+Q)
    P ≠ 0 ∧ R.Finite ∧ R.Nonempty ∧
      ∀ m, IsMaxOn (fun u ↦ TDist.logLik x u ν s) univ m ↔
        m ∈ R ∧ ∀ u ∈ R, K m ≤ K u
```

### `UnivariateML.Student2Half.all_free_maximizing_candidates`

Every all-free half-t maximizer is at the sample minimum and is an eligible score candidate. The entire maximizing set consists of the candidates tied at the top that reach both excluded faces. Every eligible shape has one conditional scale. The statement does not assert finiteness of the stationary list. The attempted normal-end proof requires the all-orders dominated derivative and zero Taylor radius of the log-Gamma half-step correction, as well as the strict critical collar. Individual check passed.

```lean
theorem all_free_maximizing_candidates {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    {i : Fin n} (hi : ∀ j, x i ≤ x j) (p : ℝ × ℝ × ℝ) :
    let L := fun p : ℝ × ℝ × ℝ ↦ likelihood x p.1 p.2.1 p.2.2
    (p ∈ FinishHalfT.domain x ∧ IsMaxOn L (FinishHalfT.domain x) p) ↔
      2*TDist.multiplicity x (x i) ≤ n ∧ Candidate x (x i) p ∧
        max (normalFace x (x i)) (collapseFace x (x i)) ≤ L p ∧
        ∀ q, Candidate x (x i) q → L q ≤ L p
```

### `UnivariateML.Student2Half.log_gamma_half_step_derivative`

The derivative of the log-Gamma half-step difference is the inverse-logistic Laplace integral. Uses the Gamma recurrence, convexity, and the alternating recurrence for the difference between the integrals and derivatives. Individual check passed.

```lean
theorem log_gamma_half_step_derivative {v : ℝ} (hv : 0 < v) :
    HasDerivAt (fun u ↦ log (Gamma ((u+1)/2)) - log (Gamma (u/2)))
      (∫ t in Ioi (0 : ℝ), exp (-v*t)/(1+exp (-t))) v
```

### `UnivariateML.Student2Half.laplace_score_strict_crossing`

One negative-to-positive kernel crossing forces any nonpositive Laplace score to be strictly negative at every larger shape. The result gives the strict score comparison once the half-t kernel is shown to have the required sign pattern. Individual check passed.

```lean
theorem laplace_score_strict_crossing {K : ℝ → ℝ} {b v w : ℝ} (hb : 0 < b) (hv : v < w)
    (hneg : ∀ t ∈ Ioo 0 b, K t < 0) (hpos : ∀ t ∈ Ioi b, 0 ≤ K t)
    (iv : IntegrableOn (fun t ↦ exp (-v*t)*K t) (Ioi 0))
    (iw : IntegrableOn (fun t ↦ exp (-w*t)*K t) (Ioi 0))
    (hscore : (∫ t in Ioi (0 : ℝ), exp (-v*t)*K t) ≤ 0) :
    (∫ t in Ioi (0 : ℝ), exp (-w*t)*K t) < 0
```

### `UnivariateML.Student2Half.held_endpoint_scale_one_magnitude`

At held endpoint and scale, all-zero or one-magnitude data with optional contacts have at most one maximizing shape. The proof covers the open positive-shape domain and all finite shape floors. It uses the signed Laplace kernel and the strict weight-ratio comparison. The closed-form threshold for existence and the a.e. claim on heterogeneous residual strata are separate. Individual check passed.

```lean
theorem held_endpoint_scale_one_magnitude {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    {b d s f : ℝ} (hs : 0 < s)
    (hx : ∀ i, x i = b ∨ x i = b+d) :
    Set.Subsingleton {v : ℝ | v ∈ Ioi 0 ∩ Ici f ∧
      IsMaxOn (fun u ↦ likelihood x b u s) (Ioi 0 ∩ Ici f) v}
```

### `UnivariateML.Student2Half.held_scale_one_magnitude`

At held scale, the maximizing endpoint is the sample minimum, and data with at most one positive residual magnitude have at most one maximizing endpoint–shape pair. The statement covers one and two distinct sample levels, ties, and the fitted floor. Generic uniqueness at higher level counts remains delegated. Individual check passed.

```lean
theorem held_scale_one_magnitude {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    {i : Fin n} (hi : ∀ j, x i ≤ x j) {d s : ℝ} (hs : 0 < s)
    (hx : ∀ j, x j = x i ∨ x j = x i+d) :
    let D := {p : ℝ × ℝ | (∀ j, p.1 ≤ x j) ∧ 1 ≤ p.2}
    let L := fun p : ℝ × ℝ ↦ likelihood x p.1 p.2 s
    Set.Subsingleton {p ∈ D | IsMaxOn L D p}
```

### `UnivariateML.Student2Half.held_endpoint_one_magnitude_escape`

If all observations have one positive residual magnitude, every finite endpoint-held fit loses to some larger shape at the unique conditional scale. The strict comparison holds with or without a finite shape floor. Individual check passed.

```lean
theorem held_endpoint_one_magnitude_escape {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    {b r f : ℝ} (hr : 0 < r) (hx : ∀ i, x i = b+r)
    {q s : ℝ} (hq : 0 < q) (hs : 0 < s) (hf : f ≤ q) :
    ∃ v > q, f ≤ v ∧ likelihood x b q s < likelihood x b v r
```

### `UnivariateML.Student2Slash.held_scale_reflected_maxima_100`

At held scale one, the sample (-100,100) has distinct reflected finite global maximizers. An exact strict endpoint witness excludes the reflection-fixed midpoint. The theorem does not count additional reflection orbits. Individual check passed.

```lean
theorem held_scale_reflected_maxima_100 :
    ∃ m : ℝ, m ≠ -m ∧
      IsMaxOn (fun u ↦ likelihood ![-100,100] u 1) univ m ∧
      IsMaxOn (fun u ↦ likelihood ![-100,100] u 1) univ (-m)
```

### `UnivariateML.Student2Slash.exact_off_point_witnesses`

The explicit points (-1/625,1/5) and (9/2,7/2) both beat the critical collapse height of the exact 20-point slash sample. The entire centered vertical slice loses strictly. The inequalities use rational lower Taylor sums for the exponential. This theorem gives the separated finite witnesses used for the local competition proof. Individual check passed.

```lean
theorem exact_off_point_witnesses :
    collapseValue offPointSample 0 < likelihood offPointSample (-1/625) (1/5) ∧
    collapseValue offPointSample 0 < likelihood offPointSample (9/2) (7/2) ∧
    ∀ s > 0, likelihood offPointSample 0 s < collapseValue offPointSample 0
```

### `UnivariateML.Student2Slash.slash_spatial_score_separation`

The slash spatial score is real analytic through the center, and identity on the observation line identifies both location and scale. This proves finite/finite score separation for the analytic incidence argument. The measure-zero conclusion is deferred to the shared uniqueness theorem. Individual check passed.

```lean
theorem slash_spatial_score_separation {m k s a : ℝ} (hs : 0 < s) (ha : 0 < a) :
    AnalyticOnNhd ℝ (spatialScore m s) univ ∧
    ((∀ y, spatialScore m s y = spatialScore k a y) → m = k ∧ s = a)
```

### `UnivariateML.Student2Slash.slash_spatial_score_separation`

In particular, at the same fixed scale, distinct slash locations have different analytic score functions. This verifies the family-specific separation needed for the three-level held-scale incidence result. Individual check passed.

```lean
theorem slash_spatial_score_separation {m k s a : ℝ} (hs : 0 < s) (ha : 0 < a) :
    AnalyticOnNhd ℝ (spatialScore m s) univ ∧
    ((∀ y, spatialScore m s y = spatialScore k a y) → m = k ∧ s = a)
```

### `UnivariateML.Student2LocationScore.student_spatial_score_separation`

Four distinct levels identify free Student kernel parameters from their spatial scores. At common shape, three suffice. This is the finite/finite score input for the indicated regimes. Almost-everywhere uniqueness requires the shared theorem. Individual check passed.

```lean
theorem student_spatial_score_separation {a b m k Q R : ℝ}
    (ha : 0 < a) (hb : 0 < b) (hQ : 0 < Q) (hR : 0 < R)
    (s : Finset ℝ) (hs : (if a = b then 2 else 3) < s.card)
    (hag : ∀ y ∈ s, spatialScore a m Q y = spatialScore b k R y) :
    a = b ∧ m = k ∧ Q = R
```

### `UnivariateML.Student2LocationScore.student_common_center_score_separation`

At a common center two nonzero levels with unequal absolute residuals identify the Student kernel parameters from the spatial scores. For half-t the formula applies inside the supported chart. With the endpoint profiled and held scale, at least three distinct levels leave two positive levels after conditioning on the minimum. Individual check passed.

```lean
theorem student_common_center_score_separation {a b m Q R y z : ℝ}
    (ha : 0 < a) (hQ : 0 < Q) (hR : 0 < R)
    (hy : y ≠ m) (hz : z ≠ m) (hgap : (y-m)^2 ≠ (z-m)^2)
    (hyag : spatialScore a m Q y = spatialScore b m R y)
    (hzag : spatialScore a m Q z = spatialScore b m R z) :
    a = b ∧ Q = R
```

### `UnivariateML.Student2LocationScore.student_normal_common_center_separation`

At a held center, finite Student and Gaussian scores cannot agree at two nonzero levels with unequal absolute residuals. This is the corresponding finite/Gaussian score input. Individual check passed.

```lean
theorem student_normal_common_center_separation {a m Q V y z : ℝ}
    (hQ : 0 < Q) (hV : 0 < V)
    (hy : y ≠ m) (hz : z ≠ m) (hgap : (y-m)^2 ≠ (z-m)^2)
    (hyag : spatialScore a m Q y = -(y-m)/V) :
    spatialScore a m Q z ≠ -(z-m)/V
```

### `UnivariateML.Student2LocationScore.student_normal_score_separation`

The cleared Student/Gaussian score difference is a monic cubic, so four distinct levels separate the scores. The same result applies with nuisance parameters held and at the finite Student-two face. Individual check passed.

```lean
theorem student_normal_score_separation {a m Q k V : ℝ} (hQ : 0 < Q) (hV : 0 < V)
    (s : Finset ℝ) (hs : 3 < s.card) :
    ¬ ∀ y ∈ s, spatialScore a m Q y = -(y-k)/V
```

### `UnivariateML.Student2LocationScore.student_collapse_score_separation`

A positive collapse trace score cannot agree with a finite Student score at three distinct noncontact levels. Its cleared quadratic is nonzero by evaluation at the collapse atom. Individual check passed.

```lean
theorem student_collapse_score_separation {a m Q b k : ℝ} (hQ : 0 < Q) (hk : 0 < k)
    (s : Finset ℝ) (hs : 2 < s.card) (hoff : ∀ y ∈ s, y ≠ b) :
    ¬ ∀ y ∈ s, spatialScore a m Q y = -k/(y-b)
```

### `UnivariateML.Student2LocationScore.student_spatial_score_derivative`

The kernel score is exactly the derivative with respect to the observation of the location–scale Student log density. The standard-deviation chart is its positive ordinary-scale reparametrization. Individual check passed.

```lean
theorem student_spatial_score_derivative {v m s : ℝ} (hv : 0 < v) (hs : 0 < s) (y : ℝ) :
    HasDerivAt (fun z ↦ log (TDist.density m v s z))
      (spatialScore (v+1) m (v*s^2) y) y
```

### `UnivariateML.Student2Skew.skew_scores_identified_on_neighborhood`

Equality of finite skew-t scores on any observation neighborhood identifies all four parameters, and therefore the free parameters in every held mask. The proof uses two polynomial-tail coefficients, bounded standardized log density, and real analyticity of the CDF from a complex local primitive. The incidence and a.e. conclusions await the shared theorem. Individual check passed.

```lean
theorem skew_scores_identified_on_neighborhood {m k q v α β s a z : ℝ}
    (hq : 0 < q) (hv : 0 < v) (hs : 0 < s) (ha : 0 < a)
    (he : ∀ᶠ y in 𝓝 z, spatialScore m q α s y = spatialScore k v β a y) :
    m = k ∧ q = v ∧ α = β ∧ s = a
```

### `UnivariateML.Student2Skew.skew_spatial_score_analytic`

The finite skew-t spatial score is analytic in the observation at all finite fitted parameters. The analytic primitive argument includes zero slant and the fitted shape floor. Individual check passed.

```lean
theorem skew_spatial_score_analytic {m q α s : ℝ} (hq : 0 < q) (hs : 0 < s) :
    AnalyticOnNhd ℝ (spatialScore m q α s) univ
```

### `UnivariateML.Student2Skew.skew_spatial_score_separation`

Global score identity uniquely determines all four finite skew-t parameters. Tail limits identify shape and slant. Standardized suprema and exclusion of nonconstant periods identify scale and location. Individual check passed.

```lean
theorem skew_spatial_score_separation {m k q v α β s a : ℝ}
    (hq : 0 < q) (hv : 0 < v) (hs : 0 < s) (ha : 0 < a)
    (he : ∀ y, spatialScore m q α s y = spatialScore k v β a y) :
    m = k ∧ q = v ∧ α = β ∧ s = a
```

### `UnivariateML.Student2LocationScore.student_collapse_common_center_separation`

A same-center collapse score cannot equal a finite Student score at two distinct nonzero residual magnitudes. This is the held-center finite/collapse comparison for the generic uniqueness argument. Individual check passed.

```lean
theorem student_collapse_common_center_separation {a m Q k y z : ℝ} (hQ : 0 < Q) (hk : 0 < k)
    (hy : y ≠ m) (hz : z ≠ m) (hgap : (y-m)^2 ≠ (z-m)^2)
    (hyag : spatialScore a m Q y = -k/(y-m)) :
    spatialScore a m Q z ≠ -k/(z-m)
```

### `UnivariateML.Student2LocationScaleHeldDf.reflected_subunit_global_maxima`

The exact five-level sample at shape one half has two reflected global maximizers with nonzero location. Strict atom mass proves existence. The centered conditional scale root lies between squared widths 1979 and 1981, and a rational fourth-power comparison beats it. Individual check passed.

```lean
theorem reflected_subunit_global_maxima :
    let L := fun p : ℝ × ℝ ↦ TDist.likelihood reflectedSubunitSample p.1 (1/2) p.2
    let D := (univ : Set ℝ) ×ˢ Ioi (0 : ℝ)
    ∃ m s : ℝ, m ≠ 0 ∧ 0 < s ∧ IsMaxOn L D (m,s) ∧ IsMaxOn L D (-m,s)
```

### `UnivariateML.Student2Normal.gaussian_log_likelihood_remainder`

A uniform quartic remainder bounds the Student log likelihood above its matched Gaussian at every positive scale and shape. This is the analytic normal-tail bound used by the profile cover. Uniform tube restrictions and the complete finite interval cover are separate comparisons. Individual check passed.

```lean
theorem gaussian_log_likelihood_remainder {n : ℕ} (x : Fin n → ℝ) (m : ℝ)
    {q s : ℝ} (hq : 0 < q) (hs : 0 < s) :
    TDist.logLik x m q s ≤
      (∑ i, log (TDist.normalDensity m s (x i))) +
        (q+1)/(4*q^2) * ∑ i, ((x i-m)/s)^4
```

### `UnivariateML.Student2Normal.normalizer_le_gaussian`

At every positive shape the Student unit-scale log peak is no larger than the Gaussian log peak. The direct midpoint log-Gamma proof does not require a high-shape limit. Individual check passed.

```lean
theorem normalizer_le_gaussian {q : ℝ} (hq : 0 < q) :
    TDist.logNormalizer q ≤ -(log (2*π)/2)
```

### `UnivariateML.Student2Half.student_log_normalizer_score_bounds`

The derivative of the Student log-normalizer lies strictly above zero and at most one over four times squared shape. The bound follows from the log-Gamma Laplace identity and the pointwise sigmoid bound. It is used by the held-scale branch comparison and the large-outlier df estimate. Individual check passed.

```lean
theorem student_log_normalizer_score_bounds {v : ℝ} (hv : 0 < v) :
    0 < deriv TDist.logNormalizer v ∧ deriv TDist.logNormalizer v ≤ 1/(4*v^2)
```

### `UnivariateML.Student2Crossing.separated_real_crossing`

A strict reversal between continuous compact branch maxima gives distinct global co-winners at an interior real parameter. The argument consumes pointwise branch separation and endpoint orders. It does not substitute for the interval enclosures of the Student radial sample. Individual check passed.

```lean
theorem separated_real_crossing {X : Type*} [TopologicalSpace X]
    (F : ℝ → X → ℝ) {a b B : ℝ} (hab : a < b) {L R : Set X}
    (hL : IsCompact L) (hR : IsCompact R)
    (nL : L.Nonempty) (nR : R.Nonempty)
    (hC : Continuous (fun p : ℝ × X ↦ F p.1 p.2))
    (hsep : ∀ t ∈ Set.Icc a b, ∀ u ∈ L ∩ R, F t u < B)
    (hhigh : ∀ t ∈ Set.Icc a b, B ≤ sSup (F t '' L))
    (ha : sSup (F a '' R) < sSup (F a '' L))
    (hb : sSup (F b '' L) < sSup (F b '' R)) :
    ∃ t ∈ Set.Ioo a b, ∃ u ∈ L \ R, ∃ v ∈ R \ L,
      IsMaxOn (F t) (L ∪ R) u ∧ IsMaxOn (F t) (L ∪ R) v
```

### `UnivariateML.Student2Crossing.large_slant_high_branch_bounds`

At large positive slant, the normalized separator and high-width branch are bounded uniformly in the outlier. Pure symbolic inequalities. The identification with the original likelihood at shape one is a separate theorem. Individual check passed.

```lean
theorem large_slant_high_branch_bounds {A R D w : ℝ}
    (hA : 0 < A) (hR : 0 < R) (hD : 0 < D) (hw : 8*A ≤ w) :
    normalizedSlantProfile A R D (8*A) ≤ 8*A*(9/8)^100 ∧
      normalizedSlantProfile A R D w ≤ D*(9/8)^100
```

### `UnivariateML.Student2Crossing.large_slant_left_witness`

The left-scale witness in the normalized three-level profile beats a fixed positive lower bound all along the sample band. Bernoulli and square-root inequalities are parameterized by the outlier. Individual check passed.

```lean
theorem large_slant_left_witness {A R D : ℝ} (hA : 1024 ≤ A)
    (hD : 1024*A ≤ D) (hR : D ≤ R) :
    A/512*(19/10)^100 < normalizedSlantProfile A R D (A/8)
```

### `UnivariateML.Student2Crossing.large_slant_right_witness`

At the far end of the sample band, the right-scale witness beats the prescribed rational lower bound. No evaluation of the large outlier is needed. Individual check passed.

```lean
theorem large_slant_right_witness {A R D : ℝ} (hA : 1024 ≤ A)
    (hD : 2048 ≤ D) (hR : D ≤ R) :
    D/512 < normalizedSlantProfile A R D (D/16)
```

### `UnivariateML.Student2Crossing.large_slant_normalized_crossing`

There is a three-level sample parameter at which the factored shape-one scale likelihood has two global co-winners, on opposite sides of the separator. The outlier is symbolic and can be twice raised to the prescribed power. The result uses exact branch comparisons and a compactified width. Individual check passed.

```lean
theorem large_slant_normalized_crossing {A R : ℝ} (hA : 1024 ≤ A)
    (hR : (2:ℝ)^150*A ≤ R) :
    ∃ D ∈ Ioo (1024*A) ((2:ℝ)^150*A), ∃ u v : ℝ,
      0 < u ∧ u < 8*A ∧ 8*A < v ∧
      IsMaxOn (normalizedSlantProfile A R D) (Ioi 0) u ∧
      IsMaxOn (normalizedSlantProfile A R D) (Ioi 0) v
```

### `UnivariateML.Student2Skew.student_cdf_two`

The standardized Student CDF at shape two has its exact closed form on the entire observation line. The proof identifies its normalization by the limit at positive infinity. Individual check passed.

```lean
theorem student_cdf_two (t : ℝ) :
    2*studentCDF 2 t = 1+t/sqrt (2+t^2)
```

### `UnivariateML.Student2Skew.skew_cauchy_single_term`

The skew-Student single likelihood factor at shape one is the skew-Cauchy rational and square-root formula. This is the likelihood identity used for the symbolic large-slant witness. Individual check passed.

```lean
theorem skew_cauchy_single_term {m a α y : ℝ} (ha : 0 < a) :
    exp (spatialLogDensity m 1 α a y) =
      exp (TDist.logNormalizer 1) * (a/(a^2+(y-m)^2)) *
        (1+α*(y-m)/sqrt (a^2+(1+α^2)*(y-m)^2))
```

### `UnivariateML.Student2Skew.skew_likelihood_spatial_product`

The sample likelihood is the product of the positive single-observation densities defining the analytic spatial scores. Valid for every finite slant and positive shape. Individual check passed.

```lean
theorem skew_likelihood_spatial_product {n : ℕ} (x : Fin n → ℝ)
    (m α : ℝ) {q s : ℝ} (hq : 0 < q) :
    StudentSkew.likelihood x m q α s =
      ∏ i, exp (spatialLogDensity m q α s (x i))
```

### `UnivariateML.Student2Skew.skew_collapse_score_separated`

The moving-collapse trace score cannot agree locally with a finite skew-Student score away from the atom. The analytic identity theorem after clearing the pole makes equality at the atom impossible. Individual check passed.

```lean
theorem skew_collapse_score_separated {m q α s b k z : ℝ} (hq : 0 < q) (hs : 0 < s)
    (hk : 0 < k) (hz : z ≠ b) :
    ¬ (∀ᶠ y in 𝓝 z, spatialScore m q α s y = -k/(y-b))
```

### `UnivariateML.Student2Skew.zero_slant_likelihood_eq`

At zero slant, every Student candidate has exactly the corresponding skew-Student likelihood. This is the held-zero-slant transfer in the joint real-crossing argument. Individual check passed.

```lean
theorem zero_slant_likelihood_eq {n : ℕ} (x : Fin n → ℝ) (m a : ℝ) {q : ℝ} (hq : 0 < q) :
    StudentSkew.likelihood x m q 0 a = TDist.likelihood x m q a
```

### `UnivariateML.Student2Skew.paired_likelihood_le_student`

At a held center with sign-paired residuals, zero slant dominates every slant at each finite shape and scale. This transports joined Student winners to the free-slant held-center chart once the underlying exact Student crossing is checked. Individual check passed.

```lean
theorem paired_likelihood_le_student {n : ℕ} (x : Fin n → ℝ) (m : ℝ)
    (e : Equiv.Perm (Fin n)) (he : ∀ i, x (e i)-m = -(x i-m))
    {q s : ℝ} (hq : 0 < q) (α : ℝ) :
    StudentSkew.likelihood x m q α s ≤ TDist.likelihood x m q s
```

### `UnivariateML.Student2Half.held_endpoint_scale_moment_decision`

With one positive magnitude or all endpoint contacts, a held-scale held-endpoint fit on a positive floor attains uniquely exactly when the weighted moment exceeds one. The equality case escapes. The proof uses eventual negativity of the Laplace score when its kernel has negative derivative at zero. Individual check passed.

```lean
theorem held_endpoint_scale_moment_decision {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    {b d s f : ℝ} (hs : 0 < s) (hf : 0 < f) (hx : ∀ i, x i = b ∨ x i = b+d) :
    let Q := d^2/s^2
    let w := ((Finset.univ.filter (fun i ↦ x i ≠ b)).card : ℝ)/n
    (∃! v, v ∈ Ici f ∧ IsMaxOn (fun u ↦ likelihood x b u s) (Ici f) v) ↔ 1 < w*(Q^2-2*Q)
```

### `UnivariateML.Student2Normal.std_unit_density_remote_upper`

The centered Student density at held unit standard deviation is at most nine divided by cubed residual, uniformly over all admissible finite shapes. The proof splits at shape three, including the two-shape collapse collar. This bound is sufficient for the exact reflected magnitude-100 witness. Individual check passed.

```lean
theorem std_unit_density_remote_upper {q r : ℝ} (hq : 2 < q) (hr : 0 < r) :
    TDist.density 0 q (TDist.ordinaryScale q 1) r ≤ 9/r^3
```

### `UnivariateML.Student2SkewWitness.skew_cauchy_large_slant_scale_crossing`

At the exact atlas slant, shape and location, the 201-point sample with the symbolic outlier has two distinct finite global maximizing scales. The outlier bound uses monotonicity of natural powers. The interior second level is defined by the real branch-height crossing, and the checked identity transfers it to the original skew-Student likelihood. Individual check passed.

```lean
theorem skew_cauchy_large_slant_scale_crossing :
    let A : ℝ := 2^20
    let R : ℝ := 2^50000
    ∃ D ∈ Ioo (1024*A) ((2:ℝ)^150*A), ∃ u v : ℝ,
      0 < u ∧ u < 8*A ∧ 8*A < v ∧
      IsMaxOn (StudentSkew.likelihood (largeSlantSample D R) 0 1 A) (Ioi 0) u ∧
      IsMaxOn (StudentSkew.likelihood (largeSlantSample D R) 0 1 A) (Ioi 0) v
```

### `UnivariateML.Student2WidthWitness.lst_fixed_scale_reflected_pair`

At scale one and the exact magnitude-100 pair, the entire location–shape argmax consists of the two reflected shape-one candidates. The low-shape profile decreases strictly by the normalizer derivative bound. All shapes at least 10000 lose by an elementary logarithmic tail inequality. Individual check passed.

```lean
theorem lst_fixed_scale_reflected_pair (p : ℝ × ℝ) :
    let D := (univ : Set ℝ) ×ˢ Ici (1 : ℝ)
    let L := fun z : ℝ × ℝ ↦ TDist.logLik ![-100,100] z.1 z.2 1
    (p ∈ D ∧ IsMaxOn L D p) ↔
      p.2 = 1 ∧ (p.1 = sqrt 9999 ∨ p.1 = -sqrt 9999)
```

### `UnivariateML.Student2WidthWitness.std_fixed_sd_reflected_maxima`

At unit standard deviation, the exact magnitude-100 pair has distinct reflected finite global maxima. The shape-three witness beats the full centered chart by the remote density bound and beats every Gaussian face. The two point counts exclude critical collapse. Individual check passed.

```lean
theorem std_fixed_sd_reflected_maxima :
    let L := fun p : ℝ × ℝ ↦ exp (TDist.stdLogLik ![-100,100] p.1 p.2 1)
    let D := (univ : Set ℝ) ×ˢ Ioi (2 : ℝ)
    ∃ m q : ℝ, m ≠ 0 ∧ 2 < q ∧ IsMaxOn L D (m,q) ∧ IsMaxOn L D (-m,q)
```

### `UnivariateML.Student2Half.held_scale_moment_decision`

The held-scale half-t one- or two-level fit attains exactly above the moment threshold, with the sample minimum as its unique endpoint. This is the exact low-level branch on the fitted floor. Individual check passed.

```lean
theorem held_scale_moment_decision {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    {i : Fin n} (hi : ∀ j, x i ≤ x j) {d s : ℝ} (hs : 0 < s)
    (hx : ∀ j, x j = x i ∨ x j = x i+d) :
    let Q := d^2/s^2
    let w := ((Finset.univ.filter (fun j ↦ x j ≠ x i)).card : ℝ)/n
    let D := {p : ℝ × ℝ | (∀ j, p.1 ≤ x j) ∧ 1 ≤ p.2}
    let L := fun p : ℝ × ℝ ↦ likelihood x p.1 p.2 s
    (∃! p, p ∈ D ∧ IsMaxOn L D p) ↔ 1 < w*(Q^2-2*Q)
```

### `UnivariateML.Student2Half.kernelProfile_eq_scaleProfile`

The rational squared-width chart evaluates the exact optimized scale likelihood and gives its unique eligible scale. The chart uses the scale-score equation as an explicit parametrization. Individual check passed.

```lean
theorem kernelProfile_eq_scaleProfile {n : ℕ} (x : Fin n → ℝ) (b : ℝ)
    (hx : ∃ i, x i ≠ b) {u : ℝ} (hu : 0 < u) :
    let v := kernelShape x b u
    let s := sqrt (u/v)
    0 < v ∧ 0 < s ∧ TDist.scaleScore x b v s = 0 ∧ 0 < TDist.exponent x b v ∧
      kernelProfile x b u = scaleProfile x b v
```

### `UnivariateML.Student2Half.kernelProfile_analytic`

The scale-optimized half-t likelihood is real analytic in positive squared kernel width. The proof uses Gamma analyticity away from its poles. Individual check passed.

```lean
theorem kernelProfile_analytic {n : ℕ} (x : Fin n → ℝ) (b : ℝ)
    (hx : ∃ i, x i ≠ b) :
    AnalyticOnNhd ℝ (kernelProfile x b) (Ioi 0)
```

### `UnivariateML.Student2Half.kernelProfile_compact_stationary`

Each compact positive-width interval has finitely many critical points unless the complete analytic profile is constant. This handles interior accumulation. It does not assert the critical or normal-end derivative collars needed for global root-list finiteness. Individual check passed.

```lean
theorem kernelProfile_compact_stationary {n : ℕ} (x : Fin n → ℝ) (b : ℝ)
    (hx : ∃ i, x i ≠ b) {a c : ℝ} (ha : 0 < a) :
    Set.Finite {u ∈ Icc a c | deriv (kernelProfile x b) u = 0} ∨
      ∀ u > 0, ∀ v > 0, kernelProfile x b u = kernelProfile x b v
```

### `UnivariateML.Student2SkewWitness.skew_held_df_one_reflected_maxima`

At held df one, the symmetric atlas sample has distinct reflected finite global winners. All supported oriented faces and every reflection-fixed fit are beaten by the symbolic witness for outliers at least 100, including the exact magnitude two to the power 128. Individual check passed.

```lean
theorem skew_held_df_one_reflected_maxima {R : ℝ} (hR : 100 ≤ R) :
    let D := Student2Skew.freeSlantDomain none (some 1) none
    let L := fun p : ℝ × ℝ × ℝ × ℝ ↦ StudentSkew.likelihood (reflectedSample R)
      p.1 p.2.2.1 p.2.2.2 p.2.1
    ∃ m s α : ℝ, 0 < s ∧ (m ≠ 0 ∨ α ≠ 0) ∧
      IsMaxOn L D (m,s,1,α) ∧ IsMaxOn L D (-m,s,1,-α)
```

### `UnivariateML.Student2SkewWitness.skew_cauchy_reflected_witness_beats_center`

The skew-Cauchy witness beats every centered zero-slant width at the specified symmetric sample. The width comparison is uniform in the remote outlier. The new joined held-df theorem also handles both oriented faces. Individual check passed.

```lean
theorem skew_cauchy_reflected_witness_beats_center {R : ℝ} (hR : 100 ≤ R) :
    ∀ a > 0, StudentSkew.likelihood (reflectedSample R) 0 1 0 a <
      StudentSkew.likelihood (reflectedSample R) (1/2) 1 (-1/2) (sqrt 5/2)
```

### `UnivariateML.Student2WidthWitness.lst_held_location_one_magnitude_escape`

At held location with one nonzero absolute residual, every finite fit is beaten at a larger shape and the common residual scale. The result holds for arbitrary signs, even if both signed levels occur. Individual check passed.

```lean
theorem lst_held_location_one_magnitude_escape {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    {m r q s : ℝ} (hr : 0 < r) (hq : 0 < q) (hs : 0 < s)
    (hx : ∀ i, (x i-m)^2 = r^2) :
    ∃ v > q, TDist.likelihood x m q s < TDist.likelihood x m v r
```

### `UnivariateML.Student2WidthWitness.lst_at_most_two_levels_no_fit`

Every one- or two-level sample fails to attain in the fitted ordinary Student chart. Unbalanced masses diverge. A balanced sample has a centered fixed-shape optimum, with the Cauchy arc including the midpoint. A larger shape beats it. Individual check passed.

```lean
theorem lst_at_most_two_levels_no_fit {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    {a b : ℝ} (hx : ∀ i, x i = a ∨ x i = b) :
    let D := {p : ℝ × ℝ × ℝ | 0 < p.2.1 ∧ 1 ≤ p.2.2}
    let L := fun p : ℝ × ℝ × ℝ ↦ TDist.likelihood x p.1 p.2.2 p.2.1
    ¬ ∃ p ∈ D, IsMaxOn L D p
```

### `UnivariateML.Student2WidthWitness.student_shape_derivative_bound_one_two`

The finite base Student shape score is bounded by the exact logarithmic expression on the interval from one to two. This is the outlier penalty in the free-df skew comparison. Uniform shape and argument bounds for the CDF factor are separate. Individual check passed.

```lean
theorem student_shape_derivative_bound_one_two {q z : ℝ} (h1 : 1 ≤ q) (h2 : q ≤ 2) :
    deriv (fun v ↦ log (TDist.density 0 v 1 z)) q ≤ 5/4-log (1+z^2/2)/2
```

### `UnivariateML.Student2Inward.normalizer_ge_gaussian_sub`

The log normalizer is within one over four times shape below its Gaussian limit. Integrating the derivative upper bound is expressed using a monotone corrected normalizer and its limit. Individual check passed.

```lean
theorem normalizer_ge_gaussian_sub {q : ℝ} (hq : 0 < q) :
    -(log (2*π)/2)-1/(4*q) ≤ TDist.logNormalizer q
```

### `UnivariateML.Student2Inward.ordinary_gaussian_inward_witness`

A positive ordinary-scale Gaussian inward coefficient gives a strict finite witness at arbitrarily large shape. The proof keeps a uniform explicit sixth-order remainder. The fitted-scale case simplifies the quadratic sum at the RMS scale. The existing weak face comparison is still applied when faces other than the Gaussian compete. Individual check passed.

```lean
theorem ordinary_gaussian_inward_witness {n : ℕ} (x : Fin n → ℝ) (m : ℝ)
    {s : ℝ} (hs : 0 < s)
    (hK : 0 < ∑ i, (((x i-m)/s)^4-2*((x i-m)/s)^2-1))
    (floor : ℝ) :
    ∃ v > max floor 1, (∑ i, log (TDist.normalDensity m s (x i))) <
      TDist.logLik x m v s
```

### `UnivariateML.Student2Inward.standard_gaussian_inward_witness`

A positive held-standard-deviation Gaussian inward coefficient gives a strict finite witness at arbitrarily large shape. This is the sample-checkable inward test in the actual held standard-deviation chart. Individual check passed.

```lean
theorem standard_gaussian_inward_witness {n : ℕ} (x : Fin n → ℝ) (m : ℝ)
    {s : ℝ} (hs : 0 < s)
    (hK : 0 < ∑ i, (((x i-m)/s)^4-6*((x i-m)/s)^2+3))
    (floor : ℝ) :
    ∃ v > max floor 2, (∑ i, log (TDist.normalDensity m s (x i))) <
      TDist.stdLogLik x m v s
```

### `UnivariateML.Student2Normal.student_density_rational_upper`

A uniform rational ordinary Student tail bound over the fitted q ≥ 1 branch. This bounds the Student factor before multiplying the skew CDF. Individual check passed.

```lean
theorem student_density_rational_upper {m q s y : ℝ} (hq : 1 ≤ q) (hs : 0 < s) :
    TDist.density m q s y ≤ 2*s/(s^2+(y-m)^2)
```

### `UnivariateML.Student2Normal.student_density_cubic_remote_upper`

A cubic remote residual bound valid for every q ≥ 2. This is a uniform density estimate for the large-shape part of the symbolic reflection comparison. Individual check passed.

```lean
theorem student_density_cubic_remote_upper {m q s y : ℝ} (hq : 2 ≤ q) (hs : 0 < s)
    (hy : y ≠ m) :
    TDist.density m q s y ≤ 6*s^2/|y-m|^3
```

### `UnivariateML.Student2Slash.held_scale_open_reflected_maxima`

Every two-observation sample with half-gap greater than four times the held positive scale has at least two reflected global winners. This proves an open stratum and includes the atlas magnitude-100 example. Individual check passed.

```lean
theorem held_scale_open_reflected_maxima {s b h : ℝ} (hs : 0 < s) (hh : 4*s < h) :
    ∃ m : ℝ, m ≠ b ∧
      IsMaxOn (fun u ↦ likelihood ![b-h,b+h] u s) univ m ∧
      IsMaxOn (fun u ↦ likelihood ![b-h,b+h] u s) univ (2*b-m)
```

### `UnivariateML.Student2Slash.slash_collapse_score_separated`

The finite analytic spatial score cannot match the pole of a noncontact half-collapse score on any neighborhood. Family-specific input for finite versus collapse comparisons. Individual check passed.

```lean
theorem slash_collapse_score_separated {m s b z : ℝ} (hs : 0 < s) (hz : z ≠ b) :
    ¬ (∀ᶠ y in 𝓝 z, spatialScore m s y = -2/(y-b))
```

### `UnivariateML.Student2Slash.exact_off_point_local_maxima`

The explicit sample has a local maximum on each side of the vertical collapse slice. Both exceed the collapse trace. Uniform small-scale, large-scale and far-location bounds give compact superlevels on both closed half-charts. Individual check passed.

```lean
theorem exact_off_point_local_maxima :
    ∃ p q : ℝ × ℝ, p.1 < 0 ∧ 0 < q.1 ∧ 0 < p.2 ∧ 0 < q.2 ∧
      IsLocalMax (fun z : ℝ × ℝ ↦ likelihood offPointSample z.1 z.2) p ∧
      IsLocalMax (fun z : ℝ × ℝ ↦ likelihood offPointSample z.1 z.2) q
```

### `UnivariateML.Student2Skew.fitted_student_density_mass`

The ordinary standardized Student density integrates to one for fitted shape at least one. This derives the normalized Student CDF denominator by a Gaussian and Gamma mixture. Individual check passed.

```lean
theorem fitted_student_density_mass {q : ℝ} (hq : 1 ≤ q) :
    (∫ x : ℝ, TDist.density 0 q 1 x) = 1
```

### `UnivariateML.Student2Half.critical_kernelShape_at_zero`

The analytic squared-width shape chart has value one and strictly positive derivative at exactly half endpoint contact. This checks the critical asymptotic chart used in the stationary collar. Individual check passed.

```lean
theorem critical_kernelShape_at_zero {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) (b : ℝ)
    (hc : 2*TDist.multiplicity x b = n) :
    kernelShape x b 0 = 1 ∧ AnalyticAt ℝ (kernelShape x b) 0 ∧
      0 < deriv (kernelShape x b) 0
```

### `UnivariateML.Student2WidthWitness.student_squared_width_shape_score`

At fixed squared kernel width the shape derivative is a log-Gamma Laplace integral term minus half the data log sum. This supplies the exact score for shape collars and real-crossing comparisons. Individual check passed.

```lean
theorem student_squared_width_shape_score {n : ℕ} (x : Fin n → ℝ) (m : ℝ)
    {u q : ℝ} (hu : 0 < u) (hq : 0 < q) :
    HasDerivAt (fun v ↦ TDist.logLik x m v (sqrt (u/v)))
      ((n:ℝ)*(∫ t in Ioi (0:ℝ), exp (-q*t)/(1+exp (-t))) -
        (∑ i, log (1+(x i-m)^2/u))/2) q
```

### `UnivariateML.Student2Half.critical_kernelProfile_log_derivative`

The log-profile derivative is eventually negative and tends to minus infinity on the exact half-contact collapse branch. This supplies the critical collapse collar for stationary root isolation. Individual check passed.

```lean
theorem critical_kernelProfile_log_derivative {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (b : ℝ) (hc : 2*TDist.multiplicity x b = n) :
    Tendsto (deriv (fun u ↦ log (kernelProfile x b u))) (𝓝[>] 0) atBot
```

### `UnivariateML.Student2WidthWitness.ordinary_log_density_shape_deriv`

The actual ordinary Student shape score is expressed through the log normalizer derivative and two elementary data terms. Unit-scale coordinates for uniform shape comparisons. Individual check passed.

```lean
theorem ordinary_log_density_shape_deriv {q z : ℝ} (hq : 0 < q) :
    HasDerivAt (fun v ↦ log (TDist.density 0 v 1 z))
      (deriv TDist.logNormalizer q-log (1+z^2/q)/2+
        (q+1)*z^2/(2*q*(q+z^2))) q
```

### `UnivariateML.Student2Skew.fitted_student_cdf_shape_ratio`

A finite fitted-shape increment gives a uniform fixed-argument CDF ratio upper bound. Derived by integrating the pointwise kernel ratio over the full half-line. Individual check passed.

```lean
theorem fitted_student_cdf_shape_ratio {q v : ℝ} (hq : 1 ≤ q) (hv : q ≤ v) (z : ℝ) :
    studentCDF v z ≤ exp ((5/4:ℝ)*(v-q))*studentCDF q z
```

### `UnivariateML.Student2Skew.fitted_student_cdf_argument_ratio`

At positive arguments the fitted Student CDF grows at most proportionally to the argument. The density monotonicity on positive residuals yields the derivative sign for the ratio. Individual check passed.

```lean
theorem fitted_student_cdf_argument_ratio {q a b : ℝ} (hq : 1 ≤ q) (ha : 0 < a) (hab : a ≤ b) :
    studentCDF q b ≤ (b/a)*studentCDF q a
```

### `UnivariateML.Student2Skew.positive_slant_cdf_shape_floor_ratio`

From shape one on the positive-residual chart, the fixed-argument and moving-argument CDF ratio bounds combine to give an exponent of three halves times the shape increment. The bound is uniform in the observed positive residual and positive scale. Individual check passed.

```lean
theorem positive_slant_cdf_shape_floor_ratio {q A s x : ℝ} (hq : 1 ≤ q)
    (hA : 0 < A) (hs : 0 < s) (hx : 0 < x) :
    studentCDF (q+1) (A*x*sqrt ((q+1)/(q*s^2+x^2))) ≤
      exp ((3/2:ℝ)*(q-1))*studentCDF 2 (A*x*sqrt (2/(s^2+x^2)))
```

### `UnivariateML.Student2Skew.cdf_bounds`

The normalized Student CDF lies in the unit interval. Used in the uniform fitted likelihood comparison. Individual check passed.

```lean
theorem cdf_bounds (ν z : ℝ) : 0 ≤ StudentSkew.studentCDF ν z ∧
    StudentSkew.studentCDF ν z ≤ 1
```

### `UnivariateML.Student2Skew.cdf_pos`

At every positive shape the normalized Student CDF is positive at finite arguments. Used in the log comparison and score arguments. Individual check passed.

```lean
theorem cdf_pos {ν : ℝ} (hν : 0 < ν) (z : ℝ) : 0 < studentCDF ν z
```

### `UnivariateML.Student2SkewWitness.skew_fitted_reflected_maxima`

The symbolic reflected sample has distinct finite global maxima on the fitted free-df chart, both with free scale and with scale sqrt(5)/2 held. The shape-one witness exceeds all centered fits and all excluded faces. Individual check passed.

```lean
theorem skew_fitted_reflected_maxima {R : ℝ} (hR : (2:ℝ)^128 ≤ R) (ω : Option ℝ)
    (hω : ω = none ∨ ω = some (sqrt 5/2)) :
    let D := Student2Skew.freeSlantDomain none none ω
    let L := fun p : ℝ × ℝ × ℝ × ℝ ↦ StudentSkew.likelihood (reflectedSample R)
      p.1 p.2.2.1 p.2.2.2 p.2.1
    ∃ m s q α : ℝ, (m,s,q,α) ∈ D ∧ (m ≠ 0 ∨ α ≠ 0) ∧
      IsMaxOn L D (m,s,q,α) ∧ IsMaxOn L D (-m,s,q,-α)
```

### `UnivariateML.Student2SkewWitness.skew_large_slant_scale_shape_crossing`

At the atlas slant and location, the actual crossing has two separated global scales on the df-one floor even when df is fitted. Higher finite shapes are strictly beaten by a floor point. Exponent inequalities are symbolic in the enormous outlier. Individual check passed.

```lean
theorem skew_large_slant_scale_shape_crossing :
    let A : ℝ := 2^20
    let R : ℝ := 2^50000
    ∃ D ∈ Ioo (1024*A) ((2:ℝ)^150*A), ∃ u v : ℝ,
      0 < u ∧ u < 8*A ∧ 8*A < v ∧
      IsMaxOn (fun p : ℝ × ℝ ↦ StudentSkew.likelihood (largeSlantSample D R) 0 p.2 A p.1)
        (Ioi 0 ×ˢ Ici 1) (u,1) ∧
      IsMaxOn (fun p : ℝ × ℝ ↦ StudentSkew.likelihood (largeSlantSample D R) 0 p.2 A p.1)
        (Ioi 0 ×ˢ Ici 1) (v,1)
```

### `UnivariateML.Student2Half.student_log_normalizer_score_cubic_lower`

The cubic sigmoid bound gives an explicit lower bound for the large-shape normalizer score. The error paired with the upper bound is one over eight times the fourth power of shape. Individual check passed.

```lean
theorem student_log_normalizer_score_cubic_lower {q : ℝ} (hq : 0 < q) :
    1/(4*q^2)-1/(8*q^4) ≤ deriv TDist.logNormalizer q
```

### `UnivariateML.Student2Inward.normalizer_le_gaussian_cubic`

The cubic normalizer enclosure is within one over 24 times shape cubed of the leading correction. This is the terminal special-function enclosure for the exact interval comparisons. Individual check passed.

```lean
theorem normalizer_le_gaussian_cubic {q : ℝ} (hq : 0 < q) :
    TDist.logNormalizer q ≤ -(log (2*π)/2)-1/(4*q)+1/(24*q^3)
```

### `UnivariateML.Student2Inward.normalizer_step_two`

The log normalizer recurrence steps by two with an explicit logarithmic correction. Supports descending the terminal enclosure to the exact crossing shapes. Individual check passed.

```lean
theorem normalizer_step_two {q : ℝ} (hq : 0 < q) :
    TDist.logNormalizer (q+2) =
      TDist.logNormalizer q+log (1+1/(q*(q+2)))/2
```

### `UnivariateML.Student2Inward.normalizer_descended_bounds`

An arbitrary finite two-unit descent yields the cubic terminal normalizer enclosure at any positive shape. This implements the special-function part of the source interval checker, with residual width controlled by the shifted shape. Individual check passed.

```lean
theorem normalizer_descended_bounds {q : ℝ} (hq : 0 < q) (j : ℕ) :
    let r : ℝ := q+2*j
    let S := ∑ i ∈ Finset.range j, log (1+1/((q+2*i)*(q+2*i+2)))/2
    (-(log (2*π)/2)-1/(4*r)-S ≤ TDist.logNormalizer q ∧
      TDist.logNormalizer q ≤ -(log (2*π)/2)-1/(4*r)+1/(24*r^3)-S)
```

### `UnivariateML.Student2Inward.gamma_half_step_score_recurrence`

The exact digamma-half-step score recurrence follows by differentiating the Gamma recurrence. This is the derivative enclosure descent used by the interval checker. Individual check passed.

```lean
theorem gamma_half_step_score_recurrence {q : ℝ} (hq : 0 < q) :
    (∫ t in Ioi (0:ℝ), exp (-q*t)/(1+exp (-t))) =
      (∫ t in Ioi (0:ℝ), exp (-(q+2)*t)/(1+exp (-t)))+1/(q*(q+1))
```

### `UnivariateML.Student2Skew.skew_normal_score_separated`

A finite skew-t score cannot agree with a same- or different-hold skew-normal score on an observation neighborhood. Analytic continuation and Gaussian versus polynomial tail decay prove this comparison. Slant and nuisance location and scale are arbitrary. Individual check passed.

```lean
theorem skew_normal_score_separated {m q α s b a β z : ℝ} (hq : 0 < q) (hs : 0 < s)
    (ha : 0 < a) :
    ¬ (∀ᶠ y in 𝓝 z, spatialScore m q α s y = deriv (skewNormalSpatialLogDensity b a β) y)
```

### `UnivariateML.Student2Skew.skew_half_student_score_identified`

Finite and half-Student scores agree locally only at the same base Student parameters and zero slant. The oriented support restriction changes the height and leaves the base score on the strict-support branch. Individual check passed.

```lean
theorem skew_half_student_score_identified {m q α s b v a z : ℝ}
    (hq : 0 < q) (hv : 0 < v) (hs : 0 < s) (ha : 0 < a)
    (he : ∀ᶠ y in 𝓝 z, spatialScore m q α s y =
      deriv (fun t ↦ log (TDist.density b v a t)) y) :
    m = b ∧ q = v ∧ α = 0 ∧ s = a
```

### `UnivariateML.Student2Half.scale_collapse_divergence_detector`

The three half-t scale-collapse detectors have both directions and supply exact admissible scale-zero paths. The joined theorem covers held positive shape, held endpoint with shape unrestricted, and free endpoint with fitted floor. Individual check passed.

```lean
theorem scale_collapse_divergence_detector {n : ℕ} (x : Fin n → ℝ) {i : Fin n}
    (hi : ∀ j, x i ≤ x j) {v b : ℝ} (hv : 0 < v) (hb : ∀ j, b ≤ x j) :
    let H := SkewLSHalfT.likelihood x
    let k := TDist.multiplicity x (x i)
    ((¬ BddAbove ((fun p : ℝ × ℝ ↦ H p.1 v p.2) ''
        {p | (∀ j, p.1 ≤ x j) ∧ 0 < p.2})) ↔
      TDist.exponent x (x i) v < 0) ∧
    (TDist.exponent x (x i) v < 0 → Tendsto (H (x i) v) (𝓝[>] 0) atTop) ∧
    ((¬ BddAbove ((fun p : ℝ × ℝ ↦ H b p.1 p.2) '' (Ioi 0 ×ˢ Ioi 0))) ↔
      ∃ j, x j = b) ∧
    ((∃ j, x j = b) → ∃ q > 0, Tendsto (H b q) (𝓝[>] 0) atTop) ∧
    ((¬ BddAbove ((fun p : ℝ × ℝ × ℝ ↦ H p.1 p.2.1 p.2.2) '' FinishHalfT.domain x)) ↔
      n < 2*k) ∧
    (n < 2*k → Tendsto (H (x i) 1) (𝓝[>] 0) atTop)
```

### `UnivariateML.Student2Half.all_free_stationary_finite_of_normal_collar`

Once the right normal collar excludes score zeros, the actual stationary candidate set is finite, including the exact critical-mass case. The only remaining premise for unconditional finiteness is normal-end exclusion. Individual check passed.

```lean
theorem all_free_stationary_finite_of_normal_collar {n : ℕ} (x : Fin n → ℝ)
    (hn : 0 < n) (b : ℝ) (hc : 2*TDist.multiplicity x b ≤ n)
    (hTail : ∀ᶠ u in atTop, deriv (kernelProfile x b) u ≠ 0) :
    Set.Finite {u : ℝ | 0 < u ∧ 1 ≤ kernelShape x b u ∧ deriv (kernelProfile x b) u = 0}
```

### `UnivariateML.Student2Half.std_fixed_mean_sd_critical_log_score`

At exactly two-thirds held-mean mass the held-sd log score tends to minus infinity at df two. This is the signed punctured critical collapse collar. Individual check passed.

```lean
theorem std_fixed_mean_sd_critical_log_score {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (b : ℝ) {s : ℝ} (hs : 0 < s) (hc : 3*TDist.multiplicity x b = 2*n) :
    Tendsto (deriv (fun q ↦ TDist.stdLogLik x b q s)) (𝓝[>] 2) atBot
```

### `UnivariateML.Student2Skew.skew_half_student_equal_score_strict_height`

If the oriented half-Student score and the finite score agree on a neighborhood, the compatible oriented face strictly beats the finite likelihood whenever there is a strict-support observation. This excludes equal-score finite/half-Student ties on the relevant noncontact strata. Individual check passed.

```lean
theorem skew_half_student_equal_score_strict_height {n : ℕ} (x : Fin n → ℝ)
    {m q α s b v a z ε c : ℝ}
    (hq : 0 < q) (hv : 0 < v) (hs : 0 < s) (ha : 0 < a) (hc : 1 ≤ c)
    (hSupport : ∀ i, x i = b ∨ 0 < ε*(x i-b)) (hSome : ∃ i, 0 < ε*(x i-b))
    (he : ∀ᶠ y in 𝓝 z, spatialScore m q α s y =
      deriv (fun t ↦ log (TDist.density b v a t)) y) :
    likelihood x m q α s < TDist.likelihood x b v a * orientedWeight x b ε c
```
