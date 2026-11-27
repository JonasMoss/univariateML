# TASK-426 · Lean location–scale likelihoods

The generic theorem is checked, including a countable compact comparison cover and a Student application. The family results include exact tests for attainment and failure, entire maximizer sets, and joint limit paths. The atlas fragment has 130 rows for 118 distinct declarations. Each row leaves the coverage grade empty. The statements below record the particular branches proved by each declaration. [Atlas fragment](../existence-atlas/lean-map.d/lean-locscale.csv) · [Assignment coverage table](2026-11-27-atlas-coverage3-report.md#written-claims-awaiting-full-lean-coverage)

## Generic uniqueness

A uniform bound on the sample derivative gives a Lipschitz constant on every compact convex sample piece. A countable ball cover gives local Lipschitz regularity on an open sample domain. Rademacher's theorem and the minimum of the difference between the envelope and an attaining likelihood give the common sample derivative. For two global winners, the proof applies this argument to the maximum of their two compact-piece envelopes. Countability makes the exceptional set independent of the winners. An open feasible domain over a locally compact, second-countable parameter chart supplies the compact comparison pieces. The explicit cover version can also compare candidates across charts.

The separation corollary works with a map from parameters to winning classes. Taking the identity gives parameter uniqueness. For Student with degrees of freedom and scale held, equality of the scores of two distinct locations forces every observed value to solve one quadratic. Three fixed sample coordinates are almost surely distinct. A compact interval between the sample extrema supplies attainment. The application therefore gives an almost surely unique finite location for every sample size at least three, in the denominator-scale parameterization from the written proof. The stronger Student result with only degrees of freedom held uses geodesic convexity. The other written pilots are the all-free skew Student and Prentice generalized gamma, with additional separation questions on fixed strata and at faces. [Generic proof](../../lean/UnivariateML/Generic/UniqueAE.lean) · [Written theorem and pilots](2026-11-27-uniq-generic-math-report.md) · [Independent review](2026-11-27-atlas-urev1-report.md)

The shared affine transfer transports maximizers under translation, positive scaling and reflection of the observations. A negative affine slope reflects the standardized loss argument, and the log likelihood changes by the common Jacobian term. The parameter-bijection and reachable-face lemmas also apply to family-specific charts. [Shared transfers](../../lean/UnivariateML/Family/LocScaleCommon.lean)

## Family results and open proof obligations

Effort estimates are rounded work windows for related targets. Late additions and shared checks overlap some windows.

| Family | Checked results at the stated scopes | Open proof obligations | Effort |
| --- | --- | --- | --- |
| EMG | All five held masks have an exact attainment or nonattainment comparison when combined with the existing `SkewLSEMG` criteria. The faces include moving location at held rate, fixed-location half-contact penalties, the RMS normal at held location, and the normal at the mean at held scale. The all-zero residual and constant-data branches have explicit results. The two-window crossing argument and its limiting polynomial are checked. | All-free uniqueness still needs the exact sample construction, differentiated uniform expansion and exclusion outside the two competing windows. For `lambda`, `lambda+mu`, `mu`, `mu+sigma` and `sigma`, the full stationary candidate-list claims need finiteness proofs. The special singleton branches still need their positive-residual shape comparison, single crossing in rate, or nonpositive-residual scale argument, as appropriate. | About 30 min |
| Skew normal | Constant collapse at held slant. Complete attainment, failure and cardinality for `alpha+omega`, `alpha+xi`, `omega+xi` and `xi`, with the exact tied half-normal factors and all-zero branches. Held scale on constants has two equal excluded faces. Reflection transfers any symmetric-sample winner of nonzero slant. | For `omega` on nonconstant data, prove the profiled stationary comparison with the larger half-normal face and the strict-versus-equal decision. For all-free and `omega` uniqueness, transfer the written strict likelihood witnesses into Lean to obtain an attained winner with nonzero slant. The held-scale witness must use the exact seven-observation sample. | About 15 min |
| Bhattacharjee | Exact criterion and face failure at held width, including the equality envelope already in `HeldSweepBhatt`. Both branches at held width and centre, including normal collapse and the tied endpoint product. Unique location at held width and scale, including width zero. Constant all-free collapse and a moving-width path reaching the full uniform product. | At `a` and `a+mu), prove uniqueness on the positive-width free-scale branch. At `all free` and `mu), prove compact stationary-width comparison with the optimized uniform, and construct the exact normal/positive-width crossings. At `mu+sigma` and `sigma`, prove the second-moment detector and the held-scale two-width tie. | About 20 min |
| Huber | All-free constant divergence at fixed cutoff, and both normal and Laplace paths. At `mu+sigma`, exact comparison with the matched normal, a vanishing small-cutoff end, finitely many stationary shapes and uniqueness for equal residual magnitudes. At `mu`, conditional scale uniqueness. At `sigma`, strict constant normal selection and the complete exact maximizing set for `(-10,10)`. | For nonconstant all-free data, prove finite profiled stationary shapes and exclude an attained flat location interval. At `mu), prove both optimized face comparisons and profiled stationary finiteness. At `sigma) on general data, prove the profiled stationary criterion and exhaust the free-location ends. The direct finite-root proof here uses affine tail scores with held nuisance coordinates. The corresponding profiled scores require additional arguments. | About 30 min |
| Tukey lambda | Positive held shape and scale have the strict support criterion below one and the closed criterion at or above one. The quantile vector is unique on compatible support. Positive held shape and location have the exact residual criterion, with a constant-data collapse path. | For `all free`, prove compact attainment on the strict sub-half fitted branch, unrestricted divergence and the full plateau or tied-mode claim. For `lambda`, prove the joint collapse classification, the uniqueness range and the exact reflected shape-five example. For `lambda+mu`, prove the nonpositive branches, negative-shape exponent and unique scale. For `lambda+sigma`, prove the nonpositive branches and the two exact maximizers at shape three halves. For `mu` and `sigma`, prove all three main claims, including the exact uniform crossings and the large-gap reflected maximizers. | About 20 min |
| Triangular | All listed mathematical main claims. The all-free finite product attains exactly on nonconstant samples and has the exact reflected pair on `(0,1)`. Held-endpoint maxima reduce to all winning knots with their unique widths. Held mode and common held endpoint-mode have unique endpoints on attainment. The closed held-endpoints mode problem includes the all-zero product when both endpoints are observed. | The theorem about zero product describes the mathematical basis for the API rejection. The runtime API branch is outside the Lean model. | About 20 min |
| Generalized Pareto | Held threshold attains exactly for strictly positive excesses on the fitted branch, with divergence on threshold ties. Held positive scale attains for every compatible fixed- or free-threshold sample. The full uniform-feasible maximizing sets, including all-zero excesses, are checked. At held shape, finite attainment and uniqueness cover all samples and shape branches. The critical positive-shape value is an exact joint unattained supremum, and the moving threshold trace gives the contact penalty. | For `mu+sigma` and `sigma`, the singleton beyond the uniform support width needs the signed integral score and its strict kernel-ratio comparison. For the full boundary descriptions, prove the sharp approach to the included uniform contact, the uniform bound for thresholds tending to minus infinity at variable shape, and the sharp constant-sample infinite-shape moving trace. At held `xi`, the full joint escape classification still needs the uniformly vanishing infinite-scale and receding-threshold ends. For `xi<-1`, interior finite-density divergence is checked for every sample. The extended-real R7a upper-contact argmax set and its full moving endpoint trace need a separate extended-real density model. | About 25 min across both passes |
| Double Pareto | Both one-held-rate masks have an exact existence criterion, exact unattained face when all eligible knots lose, and the full set of winning finite knots. Exact equality with a face is sufficient. Each eligible knot has a unique conditional rate. The constancy branch follows from the empty eligible set. | The written log-data formulation is used directly. | About 12 min |
| Folded normal | Exact all-free nonconstant criterion and both scale-collapse detectors. The proof bounds the profiled variance below uniformly by one pair of distinct observations and bounds the fitted location by the sample maximum. | The likelihood uses variance and the averaged log likelihood from `FoldedNormal`. | About 5 min |
| Shifted Gompertz | Both requested criteria, including the all-free constant divergence and the zero-sum branch at fixed shape. | The fitted shape includes zero, so the held-shape criterion also covers the exponential member. | About 3 min |

The generic proof, Student application, shared transfers and setup took about 30 min. The dependency build and final integrated rerun took about 13 min on the workstation. [EMG and Huber written held proofs](2026-11-27-lsheld-atlas-math-report.md) · [Tukey lambda written proof](2026-11-27-tlambda-atlas-math-report.md) · [Additional exact uniqueness claims](2026-11-27-uniq-atlas-math-report.md)

## Nested path and comparison keys

| Family and mask | Path and comparison evidence |
| --- | --- |
| `emg`, `all free` | `path:1` and `path:2` are checked at fixed physical nuisance coordinates, as products. The exponential trace includes a half-density factor at each tied endpoint. `limit:1` and `limit:2` remain open at the full all-free detector scope. They require pointwise domination or exhaustive finite-profile and signed-tail exclusion before a face can be selected. |
| `bhatt`, `a` and `a+mu` | `path:1` is checked at positive fixed width, including zero outside support, half density at contacts and full density in the interior. For `a`, the moving-centre envelope and its exact range-equality height are in `SkewLSBhatt.coverage_uniform_envelope` and `HeldSweepBhatt.range_equality_height`. For `a+mu`, the full held-centre detector and exact supremum follow from `held_width_centre_failure` and `HeldSweepBhatt.held_centre_supremum`. These give the held-coordinate comparisons in `limit:1`. |
| `bhatt`, `all free` and `mu` | `path:1` has a moving positive-width path reaching the full uniform height at support contacts. `limit:1` still needs the finite stationary-width and whole-neighbourhood comparison against the uniform and normal. |
| `tlambda`, `lambda+mu` | `path:1` is checked for constant data at positive held shape. The general mixed-tie negative-shape path and the critical detector in `limit:1` are open. The latter includes the finite compact competitor comparison in the written half-tie proof. |
| `tlambda`, `lambda+sigma` | `path:2` and `limit:2` are checked as exact positive-shape support incompatibility criteria, with the zero versus positive endpoint distinction at shape one. |
| `triang`, `all free`, `a`, `b`, `a+c`, `b+c`, `c` | The `path:1` collapse paths and `limit:1` detectors use the common observation and positive width. The corresponding attainment criteria identify the complementary branches. |
| `triang`, `a`, `b`, `a+b`, `a+c`, `b+c` | `path:2` and `limit:2` use the exact zero product for an excluded observation or an observed held endpoint at which the mode cannot lie. With both endpoints held and observed, the entire closed mode interval has zero product. |
| `gpd`, `mu` | `path:2` gives divergence at a fixed chosen positive shape when shape is free, and the exact leading exponent at additionally held positive shape. `limit:2` combines the threshold detector with positive-shape divergence, the critical finite supremum, and the nonconstant criterion in the profiled scale problem. The fixed-threshold data and fixed-shape minimum-profile formulations are stated separately. |
| `dblpareto`, `left_rate` and `right_rate` | `path:2` and `path:1` respectively are checked for fixed kinks at or beyond the relevant sample extreme. `limit:2` and `limit:1` are the exact power and Pareto comparisons with every eligible knot, including constants. |
| `foldnorm`, `all free` and `mu` | Both `limit:1` detectors are exact. With location free the sample must be constant. With location held each observation must equal the held value. |

## Source wording and scope

- The triangular dossier's fitting predicate is positive likelihood. An incompatible sample may have zero likelihood everywhere on a valid held-mode interval. The Lean statements distinguish that case from a positive maximum and show the zero product explicitly. [Triangular dossier](../../../univariateML-cpp/include/cppunivariate/distributions/triang/README.md)
- The GPD threshold-collapse key combines a free shape at held threshold and an additionally held positive shape. The free-shape path can fix a sufficiently large positive shape when there is any threshold tie. The critical finite trace belongs to the additionally held positive-shape case. The fitted shape floor and the reference-model extended-real contact convention are distinct. [GPD dossier](../../../univariateML-cpp/include/cppunivariate/distributions/gpd/README.md) · [GPD held-shape proof](2026-11-27-lsheld-atlas-math-report.md#gpd-held-xi)
- The all-free EMG detector text in the atlas still mentions an unresolved stop after an inconclusive comparison. The task specifies an uncertified fallback when the search returns a result and the proof cannot decide the comparison. The path and comparison theorems here are exact likelihood statements. [Atlas](../existence-atlas/atlas.csv) · [EMG mathematical comparison](2026-11-25-emg-math-report.md#exact-finite-comparison-and-per-input-certificates)

## Verification

The workstation ran the direct Lean checks and both integrated builds through the single-slot runner with `LEAN_NUM_THREADS=1` and `nice -n 10`. The final `lake build` with all 12 temporary imports exited zero and reported **8,150 jobs**. The build emitted source-style linter warnings and informational tactic suggestions. It did not rebuild Mathlib. The temporary imports were removed and `git diff --exit-code -- dev/lean/UnivariateML.lean` exited zero. There are no proof placeholders or additional axioms in the new modules.

## Checked theorem statements

Each code block copies the declaration from its checked Lean module.

### UnivariateML.UniqueAE

#### `UnivariateML.UniqueAE.envelope_lipschitzOn`

A compact parameter piece has a Lipschitz envelope on each compact convex sample region on which the likelihood and its sample derivative are continuous.

```lean
theorem envelope_lipschitzOn {K : Set P} {C : Set E} {L : P → E → ℝ}
    {D : P → E → E →L[ℝ] ℝ}
    (hK : IsCompact K) (hne : K.Nonempty) (hC : IsCompact C) (hc : Convex ℝ C)
    (hL : ContinuousOn (fun z : P × E ↦ L z.1 z.2) (K ×ˢ C))
    (hD : ContinuousOn (fun z : P × E ↦ D z.1 z.2) (K ×ˢ C))
    (hd : ∀ p ∈ K, ∀ x ∈ C, HasFDerivAt (L p) (D p x) x) :
    ∃ M : ℝ≥0, LipschitzOnWith M (envelope L K) C
```

[Lean source](../../lean/UnivariateML/Generic/UniqueAE.lean#L58)

#### `UnivariateML.UniqueAE.ae_piece_derivative`

Almost everywhere in the interior of a compact convex sample region, all winners of the compact piece have the derivative of its envelope.

```lean
theorem ae_piece_derivative {K : Set P} {C : Set E} {L : P → E → ℝ}
    {D : P → E → E →L[ℝ] ℝ}
    (hK : IsCompact K) (hne : K.Nonempty) (hC : IsCompact C) (hc : Convex ℝ C)
    (hL : ContinuousOn (fun z : P × E ↦ L z.1 z.2) (K ×ˢ C))
    (hD : ContinuousOn (fun z : P × E ↦ D z.1 z.2) (K ×ˢ C))
    (hd : ∀ p ∈ K, ∀ x ∈ C, HasFDerivAt (L p) (D p x) x) :
    ∀ᵐ x ∂μ, x ∈ interior C → ∀ p ∈ K, L p x = envelope L K x →
      D p x = fderiv ℝ (envelope L K) x
```

[Lean source](../../lean/UnivariateML/Generic/UniqueAE.lean#L103)

#### `UnivariateML.UniqueAE.envelope_locallyLipschitzOn`

The envelope of a compact piece is locally Lipschitz wherever the sample likelihood and its derivative are jointly continuous.

```lean
theorem envelope_locallyLipschitzOn {K : Set P} {U : Set E} {L : P → E → ℝ}
    {D : P → E → E →L[ℝ] ℝ}
    (hK : IsCompact K) (hne : K.Nonempty) (hU : IsOpen U)
    (hL : ContinuousOn (fun z : P × E ↦ L z.1 z.2) (K ×ˢ U))
    (hD : ContinuousOn (fun z : P × E ↦ D z.1 z.2) (K ×ˢ U))
    (hd : ∀ p ∈ K, ∀ x ∈ U, HasFDerivAt (L p) (D p x) x) :
    LocallyLipschitzOn U (envelope L K)
```

[Lean source](../../lean/UnivariateML/Generic/UniqueAE.lean#L126)

#### `UnivariateML.UniqueAE.ae_envelope_derivative`

One exceptional null set suffices for the whole open sample region.

```lean
theorem ae_envelope_derivative {K : Set P} {U : Set E} {L : P → E → ℝ}
    {D : P → E → E →L[ℝ] ℝ}
    (hK : IsCompact K) (hne : K.Nonempty) (hU : IsOpen U)
    (hL : ContinuousOn (fun z : P × E ↦ L z.1 z.2) (K ×ˢ U))
    (hD : ContinuousOn (fun z : P × E ↦ D z.1 z.2) (K ×ˢ U))
    (hd : ∀ p ∈ K, ∀ x ∈ U, HasFDerivAt (L p) (D p x) x) :
    ∀ᵐ x ∂μ, x ∈ U → ∀ p ∈ K, L p x = envelope L K x →
      D p x = fderiv ℝ (envelope L K) x
```

[Lean source](../../lean/UnivariateML/Generic/UniqueAE.lean#L142)

#### `UnivariateML.UniqueAE.ae_winners_derivative_of_cover`

For a countable compact-piece cover, almost every sample has one common sample derivative across all parameters attaining its global supremum.

```lean
theorem ae_winners_derivative_of_cover {ι : Type*} [Countable ι]
    {S : Set (P × E)} {L : P → E → ℝ} {D : P → E → E →L[ℝ] ℝ}
    (B : ι → Piece S)
    (hcover : ∀ z ∈ S, ∃ j, z.1 ∈ (B j).parameter ∧ z.2 ∈ interior (B j).sample)
    (hL : ContinuousOn (fun z : P × E ↦ L z.1 z.2) S)
    (hD : ContinuousOn (fun z : P × E ↦ D z.1 z.2) S)
    (hd : ∀ p x, (p, x) ∈ S → HasFDerivAt (L p) (D p x) x) :
    ∀ᵐ x ∂μ, ∀ p q, Winner S L x p → Winner S L x q → D p x = D q x
```

[Lean source](../../lean/UnivariateML/Generic/UniqueAE.lean#L185)

#### `UnivariateML.UniqueAE.ae_winning_class_of_cover`

If equal sample derivatives identify the winning class almost everywhere, then almost every sample has at most one winning class. Use the identity map for uniqueness of the parameter itself.

```lean
theorem ae_winning_class_of_cover {ι A : Type*} [Countable ι]
    {S : Set (P × E)} {L : P → E → ℝ} {D : P → E → E →L[ℝ] ℝ}
    (B : ι → Piece S)
    (hcover : ∀ z ∈ S, ∃ j, z.1 ∈ (B j).parameter ∧ z.2 ∈ interior (B j).sample)
    (hL : ContinuousOn (fun z : P × E ↦ L z.1 z.2) S)
    (hD : ContinuousOn (fun z : P × E ↦ D z.1 z.2) S)
    (hd : ∀ p x, (p, x) ∈ S → HasFDerivAt (L p) (D p x) x)
    (cls : P → A) (hsep : ∀ᵐ x ∂μ, ∀ p q, Winner S L x p → Winner S L x q →
      D p x = D q x → cls p = cls q) :
    ∀ᵐ x ∂μ, ∀ p q, Winner S L x p → Winner S L x q → cls p = cls q
```

[Lean source](../../lean/UnivariateML/Generic/UniqueAE.lean#L245)

#### `UnivariateML.UniqueAE.exists_countable_piece_cover`

An open candidate–sample domain in a locally compact second-countable chart admits a countable cover by compact parameter pieces and sample balls.

```lean
theorem exists_countable_piece_cover [LocallyCompactSpace P] [SecondCountableTopology P]
    {S : Set (P × E)} (hS : IsOpen S) :
    ∃ b : Set (Piece S), b.Countable ∧
      ∀ z ∈ S, ∃ B ∈ b, z.1 ∈ B.parameter ∧ z.2 ∈ interior B.sample
```

[Lean source](../../lean/UnivariateML/Generic/UniqueAE.lean#L261)

#### `UnivariateML.UniqueAE.ae_winners_derivative`

In an open candidate–sample domain over a locally compact second-countable parameter chart, all global winners have equal sample derivatives almost everywhere. A countable disjoint union of such charts can be used as one chart.

```lean
theorem ae_winners_derivative [LocallyCompactSpace P] [SecondCountableTopology P]
    {S : Set (P × E)} {L : P → E → ℝ} {D : P → E → E →L[ℝ] ℝ}
    (hS : IsOpen S)
    (hL : ContinuousOn (fun z : P × E ↦ L z.1 z.2) S)
    (hD : ContinuousOn (fun z : P × E ↦ D z.1 z.2) S)
    (hd : ∀ p x, (p, x) ∈ S → HasFDerivAt (L p) (D p x) x) :
    ∀ᵐ x ∂μ, ∀ p q, Winner S L x p → Winner S L x q → D p x = D q x
```

[Lean source](../../lean/UnivariateML/Generic/UniqueAE.lean#L284)

#### `UnivariateML.UniqueAE.ae_winning_class`

Sample derivatives that separate the winning classes give uniqueness of the winning class almost everywhere in an open chart.

```lean
theorem ae_winning_class [LocallyCompactSpace P] [SecondCountableTopology P]
    {A : Type*} {S : Set (P × E)} {L : P → E → ℝ} {D : P → E → E →L[ℝ] ℝ}
    (hS : IsOpen S)
    (hL : ContinuousOn (fun z : P × E ↦ L z.1 z.2) S)
    (hD : ContinuousOn (fun z : P × E ↦ D z.1 z.2) S)
    (hd : ∀ p x, (p, x) ∈ S → HasFDerivAt (L p) (D p x) x)
    (cls : P → A) (hsep : ∀ᵐ x ∂μ, ∀ p q, Winner S L x p → Winner S L x q →
      D p x = D q x → cls p = cls q) :
    ∀ᵐ x ∂μ, ∀ p q, Winner S L x p → Winner S L x q → cls p = cls q
```

[Lean source](../../lean/UnivariateML/Generic/UniqueAE.lean#L300)

#### `UnivariateML.UniqueAE.student_held_df_scale_ae_unique`

With degrees of freedom and scale held, every attained Student fit is unique for Lebesgue-almost every sample of size at least three.

Almost-everywhere result for at least three observations. The atlas two-observation counterexample is a different claim.

```lean
theorem student_held_df_scale_ae_unique {a : ℝ} (ha : 0 < a) (hn : 3 ≤ n) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ c d : ℝ,
      IsMaxOn (fun p ↦ studentLocationLogLik a p x) univ c →
      IsMaxOn (fun p ↦ studentLocationLogLik a p x) univ d → c = d
```

[Lean source](../../lean/UnivariateML/Generic/UniqueAE.lean#L418)

#### `UnivariateML.UniqueAE.student_held_df_scale_ae_existsUnique`

With degrees of freedom and scale held, the Student location MLE exists and is unique for Lebesgue-almost every sample of size at least three.

Almost-everywhere existence and uniqueness for n ≥ 3, alongside the two-point atlas case.

```lean
theorem student_held_df_scale_ae_existsUnique {a : ℝ} (ha : 0 < a) (hn : 3 ≤ n) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∃! c : ℝ,
      IsMaxOn (fun p ↦ studentLocationLogLik a p x) univ c
```

[Lean source](../../lean/UnivariateML/Generic/UniqueAE.lean#L472)


### UnivariateML.LocScaleCommon

#### `UnivariateML.LocScaleCommon.isMaxOn_iff_of_bijOn`

Reparameterization transports the full maximizing set.

```lean
theorem isMaxOn_iff_of_bijOn {P Q R : Type*} [Preorder R]
    {S : Set P} {T : Set Q} {f : P → R} {g : Q → R} {e : P → Q}
    (he : BijOn e S T) (hvalue : ∀ p ∈ S, f p = g (e p)) {p : P} (hp : p ∈ S) :
    IsMaxOn f S p ↔ IsMaxOn g T (e p)
```

[Lean source](../../lean/UnivariateML/Family/LocScaleCommon.lean#L26)

#### `UnivariateML.LocScaleCommon.locationScale_affine_maximizers`

A nonzero affine change of the observations transports all location–scale maximizers. Negative slopes also reverse the standardized loss argument.

```lean
theorem locationScale_affine_maximizers {n : ℕ} (g : ℝ → ℝ) (x : Fin n → ℝ)
    (d : ℝ) {b : ℝ} (hb : b ≠ 0) {S : Set (ℝ × ℝ)}
    (hS : S ⊆ LocationScale.domain) {p : ℝ × ℝ} (hp : p ∈ S) :
    let e := fun q : ℝ × ℝ ↦ (d+b*q.1, |b| *q.2)
    IsMaxOn (LocationScale.logLik g x) S p ↔
      IsMaxOn (LocationScale.logLik (fun z ↦ g ((|b|/b)*z)) (fun i ↦ d+b*x i))
        (e '' S) (e p)
```

[Lean source](../../lean/UnivariateML/Family/LocScaleCommon.lean#L41)

#### `UnivariateML.LocScaleCommon.nonattainment_iff_of_face`

An attainment criterion at a reachable height also characterizes nonattainment by the exact least upper bound and strict loss at every feasible point.

```lean
theorem nonattainment_iff_of_face {P A R : Type*} [LinearOrder R] [TopologicalSpace R]
    [OrderClosedTopology R] {S : Set P} {L : P → R} {B : R}
    {l : Filter A} [l.NeBot] {path : A → P}
    (hpath : ∀ᶠ a in l, path a ∈ S)
    (hlim : Tendsto (L ∘ path) l (𝓝 B))
    (hcriterion : (∃ p ∈ S, IsMaxOn L S p) ↔ ∃ p ∈ S, B ≤ L p) :
    (¬ ∃ p ∈ S, IsMaxOn L S p) ↔ (∀ p ∈ S, L p < B) ∧ IsLUB (L '' S) B
```

[Lean source](../../lean/UnivariateML/Family/LocScaleCommon.lean#L80)


### UnivariateML.LocScaleEMG

#### `UnivariateML.LocScaleEMG.crossing_polynomial_peaks`

The limiting scaled EMG polynomial has exactly two equal global peaks on the nonnegative shape axis.

Exact limiting-polynomial calculation for the two finite co-winners. The EMG sample construction remains open in Lean.

```lean
theorem crossing_polynomial_peaks {v : ℝ} (hv : 0 ≤ v) :
    crossingPolynomial v ≤ 16 / 23 ∧
      (crossingPolynomial v = 16 / 23 ↔ v = 1 ∨ v = 2)
```

[Lean source](../../lean/UnivariateML/Family/LocScaleEMG.lean#L38)

#### `UnivariateML.LocScaleEMG.two_finite_of_window_crossing`

A continuous crossing of two disjoint compact windows gives two finite all-free EMG maximizers, provided the rest of the profile stays below the larger window value throughout the crossing.

Conditional on the two-window crossing and exterior comparison. Does not construct an exact EMG sample.

```lean
theorem two_finite_of_window_crossing {n : ℕ} {a b : ℝ} (hab : a ≤ b)
    (x : ℝ → Fin n → ℝ) (i : Fin n)
    (hx : ∀ A ∈ Icc a b, LocationScale.Nonconstant (x A))
    (hi : ∀ A ∈ Icc a b, ∀ j, x A i ≤ x A j)
    {K J : Set ℝ} (hK : IsCompact K) (hJ : IsCompact J)
    (hneK : K.Nonempty) (hneJ : J.Nonempty) (hdis : Disjoint K J)
    (hKsub : K ⊆ Ioo 0 1) (hJsub : J ⊆ Ioo 0 1)
    (hwindow : ContinuousOn (fun A ↦
      sSup (compactProfile (x A) i '' J) - sSup (compactProfile (x A) i '' K)) (Icc a b))
    (ha : sSup (compactProfile (x a) i '' J) ≤ sSup (compactProfile (x a) i '' K))
    (hb : sSup (compactProfile (x b) i '' K) ≤ sSup (compactProfile (x b) i '' J))
    (hgap : ∀ A ∈ Icc a b, ∀ t ∈ Icc (0 : ℝ) 1, t ∉ K → t ∉ J →
      compactProfile (x A) i t ≤ max (sSup (compactProfile (x A) i '' K))
        (sSup (compactProfile (x A) i '' J))) :
    ∃ A ∈ Icc a b,
      let S := Ioo (0 : ℝ) 1 ×ˢ LocationScale.domain
      let L := fun p : ℝ × (ℝ × ℝ) ↦ shapeLogLik (x A) ((1-p.1)/p.1) p.2
      ∃ p ∈ S, ∃ q ∈ S, p ≠ q ∧ IsMaxOn L S p ∧ IsMaxOn L S q
```

[Lean source](../../lean/UnivariateML/Family/LocScaleEMG.lean#L86)

#### `UnivariateML.LocScaleEMG.held_rate_nonattainment`

Without a finite fit at held rate, every finite location and scale strictly loses to the compatible moving exponential. That face is the least upper bound, including for constant observations.

```lean
theorem held_rate_nonattainment {n : ℕ} (x : Fin n → ℝ) {i : Fin n}
    (hi : ∀ j, x i ≤ x j) {rate : ℝ} (hr : 0 < rate) :
    let S := (univ : Set ℝ) ×ˢ Ioi (0 : ℝ)
    let L := fun p : ℝ × ℝ ↦ ∏ j, density rate p.1 p.2 (x j)
    let B := ∏ j, rate * Real.exp (-rate * (x j - x i))
    (¬ ∃ p ∈ S, IsMaxOn L S p) ↔ (∀ p ∈ S, L p < B) ∧ IsLUB (L '' S) B
```

[Lean source](../../lean/UnivariateML/Family/LocScaleEMG.lean#L129)

#### `UnivariateML.LocScaleEMG.held_rate_joint_exponential_trace`

As scale tends to zero at held rate, every tied contact contributes the same CDF factor. The three options are a finite standardized contact, a contact approached from below at a greater distance than the scale, and the opposite infinite standardized contact.

Finite limiting location and convergent standardized contact, including both infinite contact limits.

```lean
theorem held_rate_joint_exponential_trace {α : Type*} {l : Filter α} {n : ℕ}
    (x : Fin n → ℝ) {rate c penalty : ℝ} {s μ : α → ℝ}
    (hs : Tendsto s l (𝓝[>] 0)) (hμ : Tendsto μ l (𝓝 c))
    (hcontact : (∃ h : ℝ, penalty = Φ h ∧ Tendsto (fun t ↦ (c - μ t) / s t) l (𝓝 h)) ∨
      (penalty = 1 ∧ Tendsto (fun t ↦ (c - μ t) / s t) l atTop) ∨
      (penalty = 0 ∧ Tendsto (fun t ↦ (c - μ t) / s t) l atBot)) :
    Tendsto (fun t ↦ ∏ j, density rate (μ t) (s t) (x j)) l
      (𝓝 (∏ j, rate * Real.exp (-rate * (x j - c)) *
        (if c < x j then 1 else if x j = c then penalty else 0)))
```

[Lean source](../../lean/UnivariateML/Family/LocScaleEMG.lean#L154)

#### `UnivariateML.LocScaleEMG.held_rate_location_scale_ends`

For held rate and location, the zero-scale trace includes half density at each contact, and the infinite-scale trace vanishes.

Product trace at fixed location and rate, including every tied half-density contact.

```lean
theorem held_rate_location_scale_ends {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {rate μ : ℝ} (hr : 0 < rate) :
    Tendsto (fun s ↦ ∏ j, density rate μ s (x j)) (𝓝[>] 0) (𝓝 (exponentialTrace x rate μ)) ∧
      Tendsto (fun s ↦ ∏ j, density rate μ s (x j)) atTop (𝓝 0)
```

[Lean source](../../lean/UnivariateML/Family/LocScaleEMG.lean#L195)

#### `UnivariateML.LocScaleEMG.held_location_scale_rate_ends`

For held location and scale, zero rate gives zero and infinite rate gives the matched normal product.

Product path at fixed location and positive scale.

```lean
theorem held_location_scale_rate_ends {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {μ σ : ℝ} (hσ : 0 < σ) :
    Tendsto (fun rate ↦ ∏ j, density rate μ σ (x j)) (𝓝[>] 0) (𝓝 0) ∧
      Tendsto (fun rate ↦ ∏ j, density rate μ σ (x j)) atTop (𝓝 (normalTrace x μ σ))
```

[Lean source](../../lean/UnivariateML/Family/LocScaleEMG.lean#L211)

#### `UnivariateML.LocScaleEMG.held_rate_location_nonattainment`

At fixed rate and location, failure of a finite maximum is strict loss to the zero-scale face, which is then the exact supremum.

```lean
theorem held_rate_location_nonattainment {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {rate μ : ℝ} (hr : 0 < rate) :
    let S := Ioi (0 : ℝ)
    let L := fun s ↦ ∏ j, density rate μ s (x j)
    let B := exponentialTrace x rate μ
    (¬ ∃ p ∈ S, IsMaxOn L S p) ↔ (∀ p ∈ S, L p < B) ∧ IsLUB (L '' S) B
```

[Lean source](../../lean/UnivariateML/Family/LocScaleEMG.lean#L226)

#### `UnivariateML.LocScaleEMG.held_location_scale_nonattainment`

At fixed location and scale, failure of a finite maximum is strict loss to the infinite-rate normal face, which is then the exact supremum.

```lean
theorem held_location_scale_nonattainment {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {μ σ : ℝ} (hσ : 0 < σ) :
    let S := Ioi (0 : ℝ)
    let L := fun rate ↦ ∏ j, density rate μ σ (x j)
    let B := normalTrace x μ σ
    (¬ ∃ p ∈ S, IsMaxOn L S p) ↔ (∀ p ∈ S, L p < B) ∧ IsLUB (L '' S) B
```

[Lean source](../../lean/UnivariateML/Family/LocScaleEMG.lean#L238)

#### `UnivariateML.LocScaleEMG.held_scale_mean_criterion`

At held Gaussian scale, a finite location and rate attain exactly when a finite fit reaches the matched normal value at the sample mean.

```lean
theorem held_scale_mean_criterion {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {σ : ℝ} (hσ : 0 < σ) :
    let S := (univ : Set ℝ) ×ˢ Ioi (0 : ℝ)
    let L := fun p : ℝ × ℝ ↦ ∏ j, density p.2 p.1 σ (x j)
    let B := normalTrace x (Normal.mean x) σ
    (∃ p ∈ S, IsMaxOn L S p) ↔ ∃ p ∈ S, B ≤ L p
```

[Lean source](../../lean/UnivariateML/Family/LocScaleEMG.lean#L289)

#### `UnivariateML.LocScaleEMG.held_scale_nonattainment`

At held Gaussian scale, nonattainment occurs exactly when the normal face at the sample mean is strictly above every finite candidate.

```lean
theorem held_scale_nonattainment {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {σ : ℝ} (hσ : 0 < σ) :
    let S := (univ : Set ℝ) ×ˢ Ioi (0 : ℝ)
    let L := fun p : ℝ × ℝ ↦ ∏ j, density p.2 p.1 σ (x j)
    let B := normalTrace x (Normal.mean x) σ
    (¬ ∃ p ∈ S, IsMaxOn L S p) ↔ (∀ p ∈ S, L p < B) ∧ IsLUB (L '' S) B
```

[Lean source](../../lean/UnivariateML/Family/LocScaleEMG.lean#L302)

#### `UnivariateML.LocScaleEMG.held_location_exponential_value`

With location held and a nonzero residual, the exponential face is maximized at the exponential rate of the residuals when compatible. If any residual is negative, its whole face is zero.

Optimization of the exponential face on the nonzero-residual branch.

```lean
theorem held_location_exponential_value {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {μ : ℝ} (hx : ∃ i, x i ≠ μ) :
    let B := if ∀ j, μ ≤ x j then
      exponentialTrace x (Exponential.rate (fun j ↦ x j - μ)) μ else 0
    IsGreatest ((fun r ↦ exponentialTrace x r μ) '' Ioi 0) B
```

[Lean source](../../lean/UnivariateML/Family/LocScaleEMG.lean#L341)

#### `UnivariateML.LocScaleEMG.held_location_zero_residual_collapse`

With location held, all-zero residuals give unbounded likelihood along the fixed standardized-rate collapse.

```lean
theorem held_location_zero_residual_collapse {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {μ : ℝ} (hx : ∀ i, x i = μ) :
    ¬ BddAbove ((fun p : ℝ × ℝ ↦ ∏ j, density p.1 μ p.2 (x j)) '' (Ioi 0 ×ˢ Ioi 0))
```

[Lean source](../../lean/UnivariateML/Family/LocScaleEMG.lean#L371)

#### `UnivariateML.LocScaleEMG.held_location_rms_criterion`

At held location, a finite fit requires a nonzero residual and a finite value reaching both optimized face values.

```lean
theorem held_location_rms_criterion {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) (μ : ℝ) :
    let S := Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ)
    let L := fun p : ℝ × ℝ ↦ ∏ j, density p.1 μ p.2 (x j)
    let N := normalTrace x μ (Normal.sd x μ)
    let E := if ∀ j, μ ≤ x j then
      exponentialTrace x (Exponential.rate (fun j ↦ x j - μ)) μ else 0
    (∃ p ∈ S, IsMaxOn L S p) ↔ (∃ i, x i ≠ μ) ∧ ∃ p ∈ S, max N E ≤ L p
```

[Lean source](../../lean/UnivariateML/Family/LocScaleEMG.lean#L384)

#### `UnivariateML.LocScaleEMG.held_location_face_supremum`

For a nonzero held-location residual, if no finite point wins, the larger optimized normal and exponential face is the exact supremum.

Nonzero-residual branch.

```lean
theorem held_location_face_supremum {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) (μ : ℝ)
    (hx : ∃ i, x i ≠ μ) :
    let S := Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ)
    let L := fun p : ℝ × ℝ ↦ ∏ j, density p.1 μ p.2 (x j)
    let N := normalTrace x μ (Normal.sd x μ)
    let E := if ∀ j, μ ≤ x j then
      exponentialTrace x (Exponential.rate (fun j ↦ x j - μ)) μ else 0
    (¬ ∃ p ∈ S, IsMaxOn L S p) ↔ (∀ p ∈ S, L p < max N E) ∧ IsLUB (L '' S) (max N E)
```

[Lean source](../../lean/UnivariateML/Family/LocScaleEMG.lean#L406)

#### `UnivariateML.LocScaleEMG.held_scale_constant_strict`

At held Gaussian scale and constant data, every feasible fit strictly loses to the normal at the common observation.

Constant-data branch.

```lean
theorem held_scale_constant_strict {n : ℕ} (hn : 0 < n) (m : ℝ)
    {σ : ℝ} (hσ : 0 < σ) :
    ∀ μ rate : ℝ, 0 < rate →
      (∏ _ : Fin n, density rate μ σ m) < normalTrace (fun _ : Fin n ↦ m) m σ
```

[Lean source](../../lean/UnivariateML/Family/LocScaleEMG.lean#L453)


### UnivariateML.LocScaleSN

#### `UnivariateML.LocScaleSN.reflected_winner`

If a symmetric sample has a nonzero-slant finite winner, reflection at zero produces a distinct finite winner of the same height.

Conditional on a nonzero-slant winning fit on a symmetric sample.

```lean
theorem reflected_winner {n : ℕ} (x : Fin n → ℝ) (e : Fin n ≃ Fin n)
    (hx : ∀ j, x (e j) = -x j) {α ξ ω : ℝ} (hα : α ≠ 0)
    (hm : IsMaxOn (fun q : ℝ × (ℝ × ℝ) ↦ logLik q.1 x q.2)
      (univ ×ˢ LocationScale.domain) (α, ξ, ω)) :
    (α, ξ, ω) ≠ (-α, -ξ, ω) ∧
      IsMaxOn (fun q : ℝ × (ℝ × ℝ) ↦ logLik q.1 x q.2)
        (univ ×ˢ LocationScale.domain) (-α, -ξ, ω)
```

[Lean source](../../lean/UnivariateML/Family/LocScaleSN.lean#L48)

#### `UnivariateML.LocScaleSN.held_slant_constant_failure`

At fixed slant, a constant sample has no finite fit and an unbounded log likelihood.

```lean
theorem held_slant_constant_failure {n : ℕ} (hn : 0 < n) (m α : ℝ) :
    (¬ BddAbove (logLik α (fun _ : Fin n ↦ m) '' LocationScale.domain)) ∧
    ¬ ∃ p ∈ LocationScale.domain,
      IsMaxOn (logLik α (fun _ : Fin n ↦ m)) LocationScale.domain p
```

[Lean source](../../lean/UnivariateML/Family/LocScaleSN.lean#L66)

#### `UnivariateML.LocScaleSN.held_slant_scale_existsUnique`

With positive scale and finite slant held, every nonempty sample has exactly one maximizing location.

```lean
theorem held_slant_scale_existsUnique {n : ℕ} (hn : 0 < n) (α : ℝ)
    {ω : ℝ} (hω : 0 < ω) (x : Fin n → ℝ) :
    ∃! ξ, IsMaxOn (fun m ↦ logLik α x (m,ω)) univ ξ
```

[Lean source](../../lean/UnivariateML/Family/LocScaleSN.lean#L179)

#### `UnivariateML.LocScaleSN.held_slant_location_existsUnique_of_nonzero`

For fixed slant and location, exactly one positive scale maximizes as soon as a residual is nonzero.

Nonzero residual branch, with collapse covered separately.

```lean
theorem held_slant_location_existsUnique_of_nonzero {n : ℕ} (hn : 0 < n)
    (x : Fin n → ℝ) (ξ α : ℝ) (hx : ∃ i, x i ≠ ξ) :
    ∃! ω, ω ∈ Ioi 0 ∧ IsMaxOn (fun s ↦ logLik α x (ξ,s)) (Ioi 0) ω
```

[Lean source](../../lean/UnivariateML/Family/LocScaleSN.lean#L290)

#### `UnivariateML.LocScaleSN.held_slant_location_zero_residual_failure`

With location and slant held, zero residuals cause unbounded scale collapse.

```lean
theorem held_slant_location_zero_residual_failure {n : ℕ} (hn : 0 < n)
    (x : Fin n → ℝ) (ξ α : ℝ) (hx : ∀ i, x i = ξ) :
    ¬ BddAbove ((fun s ↦ logLik α x (ξ,s)) '' Ioi 0)
```

[Lean source](../../lean/UnivariateML/Family/LocScaleSN.lean#L318)

#### `UnivariateML.LocScaleSN.held_slant_location_criterion`

With slant and location held, scale attains exactly when a residual is nonzero. The maximum is unique throughout that branch.

```lean
theorem held_slant_location_criterion {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) (ξ α : ℝ) :
    (∃ ω ∈ Ioi 0, IsMaxOn (fun s ↦ logLik α x (ξ,s)) (Ioi 0) ω) ↔ ∃ i, x i ≠ ξ
```

[Lean source](../../lean/UnivariateML/Family/LocScaleSN.lean#L338)

#### `UnivariateML.LocScaleSN.held_location_scale_maximizers`

With location and scale held, both strict residual signs give exactly one maximizer and all-zero residuals give the whole slant line. A lone strict residual sign precludes a finite maximum.

```lean
theorem held_location_scale_maximizers {n : ℕ} (x : Fin n → ℝ) (ξ : ℝ)
    {ω : ℝ} (hω : 0 < ω) :
    let M := fun α ↦ IsMaxOn (fun β ↦ logLik β x (ξ,ω)) univ α
    let Signs := (∃ j, ξ < x j) ∧ (∃ j, x j < ξ)
    let Zero := ∀ j, x j = ξ
    ((∃ α, M α) ↔ Signs ∨ Zero) ∧ (Signs → ∃! α, M α) ∧ (Zero → ∀ α, M α)
```

[Lean source](../../lean/UnivariateML/Family/LocScaleSN.lean#L451)

#### `UnivariateML.LocScaleSN.held_location_scale_one_sign_failure`

Under either single strict residual sign, the held-location and held-scale supremum is the compatible half-normal trace with half density per contact. Every finite slant loses strictly.

```lean
theorem held_location_scale_one_sign_failure {n : ℕ} (x : Fin n → ℝ) {ξ ω : ℝ} (hω : 0 < ω)
    (hside : ((∀ j, ξ ≤ x j) ∧ ∃ j, ξ < x j) ∨ ((∀ j, x j ≤ ξ) ∧ ∃ j, x j < ξ)) :
    IsLUB (range (fun α : ℝ ↦ ∏ j, density α ξ ω (x j))) (oneSidedEnvelope x ξ ω) ∧
      ∀ α : ℝ, (∏ j, density α ξ ω (x j)) < oneSidedEnvelope x ξ ω
```

[Lean source](../../lean/UnivariateML/Family/LocScaleSN.lean#L516)

#### `UnivariateML.LocScaleSN.held_location_criterion_unique`

With location held, a finite fit exists exactly when both strict residual signs occur. In that case the full fit is unique.

```lean
theorem held_location_criterion_unique {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) (ξ : ℝ) :
    let S := (univ : Set ℝ) ×ˢ Ioi (0 : ℝ)
    let M := fun p : ℝ × ℝ ↦ p ∈ S ∧
      IsMaxOn (fun q : ℝ × ℝ ↦ logLik q.1 x (ξ,q.2)) S p
    let Signs := (∃ j, ξ < x j) ∧ (∃ j, x j < ξ)
    ((∃ p, M p) ↔ Signs) ∧ (Signs → ∃! p, M p)
```

[Lean source](../../lean/UnivariateML/Family/LocScaleSN.lean#L595)

#### `UnivariateML.LocScaleSN.held_location_one_sign_failure`

At held location with one strict residual sign, the compatible half-normal trace is optimized at the residual RMS. All finite joint fits lose strictly to that reachable bound.

One strict sign, with any ties.

```lean
theorem held_location_one_sign_failure {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) (ξ : ℝ)
    (hside : ((∀ j, ξ ≤ x j) ∧ ∃ j, ξ < x j) ∨ ((∀ j, x j ≤ ξ) ∧ ∃ j, x j < ξ)) :
    let L := fun p : ℝ × ℝ ↦ ∏ j, density p.1 ξ p.2 (x j)
    let S := (univ : Set ℝ) ×ˢ Ioi (0 : ℝ)
    let B := oneSidedEnvelope x ξ (Normal.sd x ξ)
    IsLUB (L '' S) B ∧ ∀ p ∈ S, L p < B
```

[Lean source](../../lean/UnivariateML/Family/LocScaleSN.lean#L655)

#### `UnivariateML.LocScaleSN.held_location_zero_residual_failure`

With location held at all observations, free scale permits collapse even if the slant is held at any chosen finite value along the path.

All-zero residual branch.

```lean
theorem held_location_zero_residual_failure {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    (ξ : ℝ) (hx : ∀ j, x j = ξ) :
    ¬ BddAbove ((fun p : ℝ × ℝ ↦ logLik p.1 x (ξ,p.2)) '' ((univ : Set ℝ) ×ˢ Ioi (0 : ℝ)))
```

[Lean source](../../lean/UnivariateML/Family/LocScaleSN.lean#L692)

#### `UnivariateML.LocScaleSN.held_scale_constant_faces`

Constant observations at held scale give the same excluded height on both moving half-normal faces, and every finite location and slant loses.

Constant-sample branch.

```lean
theorem held_scale_constant_faces {n : ℕ} (hn : 0 < n) (m : ℝ) {ω : ℝ} (hω : 0 < ω) :
    let B := (2 * φ 0 / ω)^n
    (∀ α ξ : ℝ, (∏ _ : Fin n, density α ξ ω m) < B) ∧
    Tendsto (fun t : ℝ ↦ ∏ _ : Fin n, density (t^2) (m-t⁻¹) ω m) atTop (𝓝 B) ∧
    Tendsto (fun t : ℝ ↦ ∏ _ : Fin n, density (-(t^2)) (m+t⁻¹) ω m) atTop (𝓝 B)
```

[Lean source](../../lean/UnivariateML/Family/LocScaleSN.lean#L706)


### UnivariateML.LocScaleBhatt

#### `UnivariateML.LocScaleBhatt.all_free_constant_failure`

Width zero includes the normal collapse on every nonempty constant sample, so the fully free likelihood is unbounded.

Constant-data branch.

```lean
theorem all_free_constant_failure {n : ℕ} (hn : 0 < n) (m : ℝ) :
    let x : Fin n → ℝ := fun _ ↦ m
    let S := Ici (0 : ℝ) ×ˢ Normal.domain
    let L := fun p : ℝ × (ℝ × ℝ) ↦ likelihood x p.1 p.2.1 p.2.2
    ¬ BddAbove (L '' S)
```

[Lean source](../../lean/UnivariateML/Family/LocScaleBhatt.lean#L35)

#### `UnivariateML.LocScaleBhatt.held_width_criterion`

For nonnegative held width, attainment at zero width is precisely the ordinary normal criterion. At positive width the range must exceed twice the held width.

```lean
theorem held_width_criterion {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) {a : ℝ} (ha : 0 ≤ a) :
    (∃ p ∈ Normal.domain,
      IsMaxOn (fun p : ℝ × ℝ ↦ likelihood x a p.1 p.2) Normal.domain p) ↔
        if a = 0 then (∃ i j, x i ≠ x j) else (∃ i j, 2*a < x j - x i)
```

[Lean source](../../lean/UnivariateML/Family/LocScaleBhatt.lean#L51)

#### `UnivariateML.LocScaleBhatt.held_width_strict_cover_failure`

When the held positive width strictly covers the sample range, the unpenalized uniform likelihood is the exact supremum and every finite fit loses strictly.

Strict coverage. Endpoint equality is already in HeldSweepBhatt.range_equality_height.

```lean
theorem held_width_strict_cover_failure {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {a : ℝ} (ha : 0 < a) {i j : Fin n} (hlo : ∀ k, x i ≤ x k) (hhi : ∀ k, x k ≤ x j)
    (hspan : x j - x i < 2*a) :
    let L := fun p : ℝ × ℝ ↦ likelihood x a p.1 p.2
    let B := (1 / (2*a))^n
    IsLUB (L '' Normal.domain) B ∧ ∀ p ∈ Normal.domain, L p < B
```

[Lean source](../../lean/UnivariateML/Family/LocScaleBhatt.lean#L71)

#### `UnivariateML.LocScaleBhatt.held_zero_width_centre_criterion`

At held width zero and held centre, a finite scale fit exists exactly when at least one residual is nonzero, and the fit is unique.

Zero-width normal seam.

```lean
theorem held_zero_width_centre_criterion {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) (m : ℝ) :
    (∃! s, s ∈ Ioi 0 ∧ IsMaxOn (likelihood x 0 m) (Ioi 0) s) ↔ ∃ j, x j ≠ m
```

[Lean source](../../lean/UnivariateML/Family/LocScaleBhatt.lean#L97)

#### `UnivariateML.LocScaleBhatt.held_positive_width_uniform_trace`

At positive held width and any fixed centre, the zero-scale trace includes one half at every endpoint contact and vanishes if any observation is outside the closed support.

Fixed positive width, with outside-support zero and every endpoint half factor.

```lean
theorem held_positive_width_uniform_trace {n : ℕ} (x : Fin n → ℝ) {a m : ℝ} (ha : 0 < a) :
    Tendsto (likelihood x a m) (𝓝[>] 0) (𝓝
      (if ∀ i, |x i-m| ≤ a then ∏ i, (if |x i-m| = a then (1 : ℝ)/2 else 1)/(2*a) else 0))
```

[Lean source](../../lean/UnivariateML/Family/LocScaleBhatt.lean#L147)

#### `UnivariateML.LocScaleBhatt.held_positive_width_centre_criterion`

At positive held physical width and held centre, an attained scale exists exactly when some residual magnitude exceeds the width.

Positive-width branch.

```lean
theorem held_positive_width_centre_criterion {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {a m : ℝ} (ha : 0 < a) :
    (∃ s ∈ Ioi 0, IsMaxOn (likelihood x a m) (Ioi 0) s) ↔ ∃ j, a < |x j - m|
```

[Lean source](../../lean/UnivariateML/Family/LocScaleBhatt.lean#L211)

#### `UnivariateML.LocScaleBhatt.held_width_scale_attains`

Fixed positive Gaussian scale and nonnegative width yield an attained centre for every nonempty sample.

```lean
theorem held_width_scale_attains {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {a s : ℝ} (ha : 0 ≤ a) (hs : 0 < s) :
    ∃ m, IsMaxOn (fun t ↦ likelihood x a t s) univ m
```

[Lean source](../../lean/UnivariateML/Family/LocScaleBhatt.lean#L224)

#### `UnivariateML.LocScaleBhatt.held_width_scale_existsUnique`

Fixed width and positive Gaussian scale give a unique centre. The positive-width proof uses strict log concavity of Gaussian interval mass.

```lean
theorem held_width_scale_existsUnique {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {a s : ℝ} (ha : 0 ≤ a) (hs : 0 < s) :
    ∃! m, IsMaxOn (fun t ↦ likelihood x a t s) univ m
```

[Lean source](../../lean/UnivariateML/Family/LocScaleBhatt.lean#L372)

#### `UnivariateML.LocScaleBhatt.moving_width_uniform_path`

A positive-radius covering interval can reach the full uniform height at all observations, including tied endpoints, when the width can move.

Moving-width branch reaching the full uniform height. Moving-width branch at the held centre.

```lean
theorem moving_width_uniform_path {n : ℕ} (x : Fin n → ℝ) {c r : ℝ}
    (hr : 0 < r) (hx : ∀ j, |x j - c| ≤ r) :
    Tendsto (fun t : ℝ ↦ likelihood x (r+t⁻¹) c ((t⁻¹)^2)) atTop
      (𝓝 ((1/(2*r))^n))
```

[Lean source](../../lean/UnivariateML/Family/LocScaleBhatt.lean#L433)

#### `UnivariateML.LocScaleBhatt.held_zero_width_centre_collapse`

On the zero-width normal member, a sample constant at the held centre collapses unboundedly as the normal scale vanishes.

Witness at the common center for the held zero-width branch. The zero-width branch of the held-centre failure.

```lean
theorem held_zero_width_centre_collapse {n : ℕ} (hn : 0 < n) (m : ℝ) :
    Tendsto (likelihood (fun _ : Fin n ↦ m) 0 m) (𝓝[>] 0) atTop
```

[Lean source](../../lean/UnivariateML/Family/LocScaleBhatt.lean#L471)

#### `UnivariateML.LocScaleBhatt.held_width_centre_failure`

The full held-width-and-centre failure classification, including the zero-width normal collapse and the covered uniform with contact penalties.

Failure detector at every nonnegative width. The positive-width envelope value is HeldSweepBhatt.held_centre_supremum.

```lean
theorem held_width_centre_failure {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {a m : ℝ} (ha : 0 ≤ a) :
    (¬ ∃ s ∈ Ioi (0 : ℝ), IsMaxOn (likelihood x a m) (Ioi 0) s) ↔
      (a = 0 ∧ ∀ i, x i = m) ∨ (0 < a ∧ ∀ i, |x i-m| ≤ a)
```

[Lean source](../../lean/UnivariateML/Family/LocScaleBhatt.lean#L481)


### UnivariateML.LocScaleHuber

#### `UnivariateML.LocScaleHuber.constant_scale_collapse`

Constant data at the common location have unbounded normalized likelihood even with any positive tuning cutoff held along the path.

Also applies at a fixed location when every residual vanishes.

```lean
theorem constant_scale_collapse {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {m c : ℝ} (hc : 0 < c) (hx : ∀ j, x j = m) :
    ∀ B : ℝ, ∃ s > 0, B < logLikelihood x c m s
```

[Lean source](../../lean/UnivariateML/Family/LocScaleHuber.lean#L67)

#### `UnivariateML.LocScaleHuber.all_free_constant_failure`

With both location and scale free on constant data, the normalized likelihood is unbounded and has no finite maximizing tuple.

Constant-data branch.

```lean
theorem all_free_constant_failure {n : ℕ} (hn : 0 < n) (m : ℝ) :
    let x : Fin n → ℝ := fun _ ↦ m
    let S := Ioi (0 : ℝ) ×ˢ LocationScale.domain
    let L := fun p : ℝ × (ℝ × ℝ) ↦ logLikelihood x p.1 p.2.1 p.2.2
    (¬ BddAbove (L '' S)) ∧ ¬ ∃ p ∈ S, IsMaxOn L S p
```

[Lean source](../../lean/UnivariateML/Family/LocScaleHuber.lean#L78)

#### `UnivariateML.LocScaleHuber.normal_face_path`

At fixed location and positive scale, increasing cutoff without bound gives the matched normal log likelihood.

Fixed-location and fixed-scale path to the matched normal.

```lean
theorem normal_face_path {n : ℕ} (x : Fin n → ℝ) (m : ℝ) {s : ℝ} (hs : 0 < s) :
    Tendsto (fun c ↦ logLikelihood x c m s) atTop (𝓝 (Normal.logLik x m s))
```

[Lean source](../../lean/UnivariateML/Family/LocScaleHuber.lean#L125)

#### `UnivariateML.LocScaleHuber.laplace_face_path`

Shrinking Huber cutoff with scale proportional to cutoff gives the matched Laplace log likelihood.

Fixed-location path with scale proportional to the cutoff.

```lean
theorem laplace_face_path {n : ℕ} (x : Fin n → ℝ) (m : ℝ) {b : ℝ} (hb : 0 < b) :
    Tendsto (fun c ↦ logLikelihood x c m (c*b)) (𝓝[>] 0) (𝓝 (Laplace.logLik x m b))
```

[Lean source](../../lean/UnivariateML/Family/LocScaleHuber.lean#L180)

#### `UnivariateML.LocScaleHuber.held_scale_small_shape`

At positive held scale the small-cutoff likelihood vanishes, even when all residuals are zero. This is stated in log likelihood units.

Vanishing likelihood in log likelihood units.

```lean
theorem held_scale_small_shape {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) (m : ℝ)
    {s : ℝ} (hs : 0 < s) :
    Tendsto (fun c ↦ logLikelihood x c m s) (𝓝[>] 0) atBot
```

[Lean source](../../lean/UnivariateML/Family/LocScaleHuber.lean#L251)

#### `UnivariateML.LocScaleHuber.held_location_scale_criterion`

At held location and scale, finite tuning attains exactly when the loss reduction matches or exceeds the normalizer penalty at some cutoff.

```lean
theorem held_location_scale_criterion {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) (m : ℝ)
    {s : ℝ} (hs : 0 < s) :
    (∃ c ∈ Ioi (0 : ℝ), IsMaxOn (fun t ↦ logLikelihood x t m s) (Ioi 0) c) ↔
      ∃ c > 0, (∑ j, (max (|(x j-m)/s|-c) 0)^2/2) ≥ n * log (normalizer c / sqrt (2*π))
```

[Lean source](../../lean/UnivariateML/Family/LocScaleHuber.lean#L360)

#### `UnivariateML.LocScaleHuber.held_location_scale_nonattainment`

With both nuisances held, any failure to attain has the matched normal as the exact supremum, strictly above all finite cutoffs.

```lean
theorem held_location_scale_nonattainment {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) (m : ℝ)
    {s : ℝ} (hs : 0 < s) :
    let L := fun c ↦ logLikelihood x c m s
    let B := Normal.logLik x m s
    (¬ ∃ c ∈ Ioi (0 : ℝ), IsMaxOn L (Ioi 0) c) ↔
      (∀ c ∈ Ioi (0 : ℝ), L c < B) ∧ IsLUB (L '' Ioi 0) B
```

[Lean source](../../lean/UnivariateML/Family/LocScaleHuber.lean#L372)

#### `UnivariateML.LocScaleHuber.held_location_conditional_scale`

A nonzero residual gives exactly one conditional scale at each positive cutoff with location held.

Unique conditional scale. Finiteness of the stationary cutoff list is separate.

```lean
theorem held_location_conditional_scale {n : ℕ} (x : Fin n → ℝ) (m : ℝ) {c : ℝ}
    (hc : 0 < c) (hx : ∃ i, x i ≠ m) :
    ∃! s, s ∈ Ioi 0 ∧ IsMaxOn (logLikelihood x c m) (Ioi 0) s
```

[Lean source](../../lean/UnivariateML/Family/LocScaleHuber.lean#L383)

#### `UnivariateML.LocScaleHuber.held_scale_constant_strict`

At held scale and constant data every positive cutoff and location strictly loses to the normal at the common observation.

Constant-data branch.

```lean
theorem held_scale_constant_strict {n : ℕ} (hn : 0 < n) (m : ℝ) {s : ℝ} (hs : 0 < s) :
    ∀ c > 0, ∀ u : ℝ, logLikelihood (fun _ : Fin n ↦ m) c u s <
      Normal.logLik (fun _ : Fin n ↦ m) m s
```

[Lean source](../../lean/UnivariateML/Family/LocScaleHuber.lean#L401)

#### `UnivariateML.LocScaleHuber.pair_winning_shape_and_interval`

The held-unit-scale symmetric pair has a winning cutoff in `(0,1/2)`. At that cutoff its entire location maximizing set is the stated interval.

The location interval at a winning cutoff.

```lean
theorem pair_winning_shape_and_interval :
    ∃ c ∈ Ioo (0 : ℝ) (1/2),
      ∀ m : ℝ,
        IsMaxOn (fun p : ℝ × ℝ ↦ logLikelihood separatedPair p.1 p.2 1) (Ioi 0 ×ˢ univ) (c,m) ↔
          m ∈ Icc (-10+c) (10-c)
```

[Lean source](../../lean/UnivariateML/Family/LocScaleHuber.lean#L546)

#### `UnivariateML.LocScaleHuber.held_location_scale_stationary_finite`

For a nonempty sample at held location and scale, the tuning score has finitely many roots, including any roots at observed-magnitude joins.

Finiteness via at most two zeros per affine tail score.

```lean
theorem held_location_scale_stationary_finite {n : ℕ} (hn : 0 < n) (z : Fin n → ℝ) :
    {c ∈ Ioi 0 | (n : ℝ)*(tailShare c/c) - ∑ i, max (|z i|-c) 0 = 0}.Finite
```

[Lean source](../../lean/UnivariateML/Family/LocScaleHuber.lean#L731)

#### `UnivariateML.LocScaleHuber.held_location_scale_hasDerivAt`

The normalized cutoff derivative at held location and scale, including the joins where a residual switches between tail and core.

Stationarity formula valid through residual-magnitude joins.

```lean
theorem held_location_scale_hasDerivAt {n : ℕ} (x : Fin n → ℝ) (m : ℝ)
    {s c : ℝ} (hs : 0 < s) (hc : 0 < c) :
    HasDerivAt (fun t ↦ logLikelihood x t m s)
      ((n : ℝ)*(tailShare c/c) - ∑ i, max (|(x i-m)/s|-c) 0) c
```

[Lean source](../../lean/UnivariateML/Family/LocScaleHuber.lean#L786)

#### `UnivariateML.LocScaleHuber.held_location_scale_finite_critical`

On every nonempty sample at held location and scale, the set of stationary finite cutoffs of the normalized likelihood is finite.

Finite set of stationary cutoffs of the actual normalized objective.

```lean
theorem held_location_scale_finite_critical {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    (m : ℝ) {s : ℝ} (hs : 0 < s) :
    {c ∈ Ioi 0 | deriv (fun t ↦ logLikelihood x t m s) c = 0}.Finite
```

[Lean source](../../lean/UnivariateML/Family/LocScaleHuber.lean#L810)

#### `UnivariateML.LocScaleHuber.held_location_scale_equal_magnitude_unique`

At held location and scale with equal absolute residuals, any attained Huber cutoff is unique.

All equal-magnitude cases, including no possible finite winner when the common magnitude vanishes.

```lean
theorem held_location_scale_equal_magnitude_unique {n : ℕ} (hn : 0 < n)
    (x : Fin n → ℝ) (m : ℝ) {s R : ℝ} (hs : 0 < s)
    (hR : ∀ i, |(x i-m)/s| = R)
    {a b : ℝ} (ha : 0 < a) (hb : 0 < b)
    (hma : IsMaxOn (fun c ↦ logLikelihood x c m s) (Ioi 0) a)
    (hmb : IsMaxOn (fun c ↦ logLikelihood x c m s) (Ioi 0) b) : a = b
```

[Lean source](../../lean/UnivariateML/Family/LocScaleHuber.lean#L819)

#### `UnivariateML.LocScaleHuber.separated_pair_full_maximizers`

At unit scale, the exact Huber maximizer set for `(-10,10)` has one cutoff in `(0,1/2)` and the complete closed interval of locations `[-10+c, 10-c]` at that cutoff.

Exact two-point example and entire argmax set.

```lean
theorem separated_pair_full_maximizers :
    ∃ c ∈ Ioo (0 : ℝ) (1/2),
      ∀ p ∈ (Ioi (0 : ℝ) ×ˢ (univ : Set ℝ)),
        IsMaxOn (fun q : ℝ × ℝ ↦ logLikelihood separatedPair q.1 q.2 1) (Ioi 0 ×ˢ univ) p ↔
          p.1 = c ∧ p.2 ∈ Icc (-10+c) (10-c)
```

[Lean source](../../lean/UnivariateML/Family/LocScaleHuber.lean#L914)


### UnivariateML.LocScaleTukeyLambda

#### `UnivariateML.LocScaleTukeyLambda.held_positive_shape_scale_criterion`

With positive held shape and scale, positive-likelihood attainment requires strict support compatibility below shape one and closed support compatibility at and above shape one.

Positive shape, with both endpoint conventions.

```lean
theorem held_positive_shape_scale_criterion (x : Fin n → ℝ) {lam s : ℝ}
    (hlam : 0 < lam) (hs : 0 < s) (i j : Fin n)
    (hi : ∀ k, x i ≤ x k) (hj : ∀ k, x k ≤ x j) :
    (∃ q ∈ heldScaleGraph x lam s,
      0 < graphLikelihood lam s q ∧ IsMaxOn (graphLikelihood lam s) (heldScaleGraph x lam s) q) ↔
      if lam < 1 then x j - x i < 2*(s/lam) else x j - x i ≤ 2*(s/lam)
```

[Lean source](../../lean/UnivariateML/Family/LocScaleTukeyLambda.lean#L182)

#### `UnivariateML.LocScaleTukeyLambda.held_positive_shape_scale_location_graph`

At positive shape, an admissible held-scale location has exactly one quantile vector precisely when every residual lies in the closed support.

Quantile-graph compactification of the positive-shape branch.

```lean
theorem held_positive_shape_scale_location_graph (x : Fin n → ℝ) {lam s : ℝ}
    (hlam : 0 < lam) (hs : 0 < s) (m : ℝ) :
    (∃! p : Fin n → ℝ, (m,p) ∈ heldScaleGraph x lam s) ↔
      ∀ j, |x j-m| ≤ s/lam
```

[Lean source](../../lean/UnivariateML/Family/LocScaleTukeyLambda.lean#L264)

#### `UnivariateML.LocScaleTukeyLambda.constant_positive_shape_collapse`

Constant data at the held location collapse at every held positive shape along a vanishing scale.

Constant-at-held-location path. Mixed-tie asymptotics are separate.

```lean
theorem constant_positive_shape_collapse {n : ℕ} (hn : 0 < n) (m lam : ℝ) (_hlam : 0 < lam) :
    let p : ℝ × (Fin n → ℝ) := (m,fun _ ↦ 1/2)
    (∀ s > 0, p ∈ heldScaleGraph (fun _ ↦ m) lam s) ∧
      Tendsto (fun s ↦ graphLikelihood lam s p) (𝓝[>] 0) atTop
```

[Lean source](../../lean/UnivariateML/Family/LocScaleTukeyLambda.lean#L296)

#### `UnivariateML.LocScaleTukeyLambda.held_positive_shape_location_criterion`

At positive held shape and held location, a positive-likelihood scale maximizer exists exactly when some sample residual is nonzero.

Strictly positive held shape only.

```lean
theorem held_positive_shape_location_criterion {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {lam : ℝ} (hlam : 0 < lam) (m : ℝ) :
    (∃ s > 0, ∃ p : Fin n → ℝ, (m,p) ∈ heldScaleGraph x lam s ∧
      0 < graphLikelihood lam s (m,p) ∧
      ∀ t > 0, ∀ q : Fin n → ℝ, (m,q) ∈ heldScaleGraph x lam t →
        graphLikelihood lam t (m,q) ≤ graphLikelihood lam s (m,p)) ↔
    ∃ i, x i ≠ m
```

[Lean source](../../lean/UnivariateML/Family/LocScaleTukeyLambda.lean#L315)


### UnivariateML.LocScaleTriangular

#### `UnivariateML.LocScaleTriangular.held_mode_attains`

With the mode held, every sample with a nonzero residual has an attained positive-likelihood fit, allowing either side width to be zero.

Existence on nonconstant compatible data.

```lean
theorem held_mode_attains (hn : 0 < n) (x : Fin n → ℝ) (c : ℝ)
    (hx : ∃ i, x i ≠ c) :
    ∃ p ∈ widthDomain x c, 0 < widthLikelihood x c p ∧
      IsMaxOn (widthLikelihood x c) (widthDomain x c) p
```

[Lean source](../../lean/UnivariateML/Family/LocScaleTriangular.lean#L147)

#### `UnivariateML.LocScaleTriangular.held_mode_collapse`

A constant sample at the held mode has unbounded width collapse through admissible endpoint modes. Both orientations are available.

Explicit collapse path at held mode.

```lean
theorem held_mode_collapse (hn : 0 < n) (m : ℝ) :
    let x : Fin n → ℝ := fun _ ↦ m
    (∀ s > 0, (0,s) ∈ widthDomain x m ∧ (s,0) ∈ widthDomain x m) ∧
    Tendsto (fun s ↦ widthLikelihood x m (0,s)) (𝓝[>] 0) atTop ∧
    Tendsto (fun s ↦ widthLikelihood x m (s,0)) (𝓝[>] 0) atTop
```

[Lean source](../../lean/UnivariateML/Family/LocScaleTriangular.lean#L182)

#### `UnivariateML.LocScaleTriangular.all_free_collapse`

Nonempty constant samples have an unbounded all-free likelihood.

Explicit collapse path for constant data.

```lean
theorem all_free_collapse (hn : 0 < n) (m : ℝ) :
    let x : Fin n → ℝ := fun _ ↦ m
    ¬ BddAbove (jointLikelihood x '' jointDomain x)
```

[Lean source](../../lean/UnivariateML/Family/LocScaleTriangular.lean#L226)

#### `UnivariateML.LocScaleTriangular.all_free_criterion`

The all-free triangular maximum is attained exactly for nonconstant samples. Every positive comparison reduces to an observed mode knot.

```lean
theorem all_free_criterion (hn : 0 < n) (x : Fin n → ℝ) :
    (∃ p ∈ jointDomain x, 0 < jointLikelihood x p ∧ IsMaxOn (jointLikelihood x) (jointDomain x) p)
      ↔ ∃ i j, x i ≠ x j
```

[Lean source](../../lean/UnivariateML/Family/LocScaleTriangular.lean#L257)

#### `UnivariateML.LocScaleTriangular.held_endpoints_failure`

With both endpoints held the closed mode interval always contains a maximizer. Occupying both endpoints makes every mode likelihood zero.

Compact mode domain and zero likelihood with both endpoint contacts.

```lean
theorem held_endpoints_failure (hn : 0 < n) (x : Fin n → ℝ) {a b : ℝ} (hab : a < b)
    (hx : ∀ j, x j ∈ Icc a b) :
    (∃ c ∈ Icc a b, IsMaxOn (likelihood a b x) (Icc a b) c) ∧
      ((∃ j, x j = a) → (∃ j, x j = b) → ∀ c ∈ Icc a b, likelihood a b x c = 0)
```

[Lean source](../../lean/UnivariateML/Family/LocScaleTriangular.lean#L290)

#### `UnivariateML.LocScaleTriangular.two_observation_reflected_tie`

On `(0,1)`, the two reflected endpoint-mode triples both maximize the all-free likelihood and have product likelihood `16/27`.

```lean
theorem two_observation_reflected_tie :
    let p : ℝ × (ℝ × ℝ) := (0,3/2,0)
    let q : ℝ × (ℝ × ℝ) := (-1/2,1,1)
    p ≠ q ∧ p ∈ jointDomain twoObservations ∧ q ∈ jointDomain twoObservations ∧
      jointLikelihood twoObservations p = 16/27 ∧ jointLikelihood twoObservations q = 16/27 ∧
      IsMaxOn (jointLikelihood twoObservations) (jointDomain twoObservations) p ∧
      IsMaxOn (jointLikelihood twoObservations) (jointDomain twoObservations) q
```

[Lean source](../../lean/UnivariateML/Family/LocScaleTriangular.lean#L336)

#### `UnivariateML.LocScaleTriangular.held_lower_mode_collapse`

Fixing the lower endpoint at the common observation and mode still allows unbounded collapse.

```lean
theorem held_lower_mode_collapse (hn : 0 < n) (m : ℝ) :
    let x : Fin n → ℝ := fun _ ↦ m
    ¬ BddAbove (widthLikelihood x m '' heldLowerModeDomain x m m)
```

[Lean source](../../lean/UnivariateML/Family/LocScaleTriangular.lean#L384)

#### `UnivariateML.LocScaleTriangular.held_lower_mode_criterion`

Holding the lower endpoint and mode gives finite attainment on compatible data except a constant sample at their common value.

```lean
theorem held_lower_mode_criterion (hn : 0 < n) (x : Fin n → ℝ) (a c : ℝ) :
    (∃ p ∈ heldLowerModeDomain x a c, 0 < widthLikelihood x c p ∧
      IsMaxOn (widthLikelihood x c) (heldLowerModeDomain x a c) p) ↔
    a ≤ c ∧ (∀ j, a ≤ x j) ∧ (∀ j, x j = a → c = a) ∧ (c ≠ a ∨ ∃ j, x j ≠ a)
```

[Lean source](../../lean/UnivariateML/Family/LocScaleTriangular.lean#L398)

#### `UnivariateML.LocScaleTriangular.held_upper_mode_criterion`

The upper-endpoint and mode criterion follows by reflecting the lower-endpoint and mode fit, preserving its height and support contacts.

```lean
theorem held_upper_mode_criterion (hn : 0 < n) (x : Fin n → ℝ) (b c : ℝ) :
    (∃ p ∈ heldUpperModeDomain x b c, 0 < widthLikelihood x c p ∧
      IsMaxOn (widthLikelihood x c) (heldUpperModeDomain x b c) p) ↔
    c ≤ b ∧ (∀ j, x j ≤ b) ∧ (∀ j, x j = b → c = b) ∧ (c ≠ b ∨ ∃ j, x j ≠ b)
```

[Lean source](../../lean/UnivariateML/Family/LocScaleTriangular.lean#L493)

#### `UnivariateML.LocScaleTriangular.held_mode_criterion`

Holding only the mode permits a positive maximum exactly when some observation differs from that mode.

```lean
theorem held_mode_criterion (hn : 0 < n) (x : Fin n → ℝ) (c : ℝ) :
    (∃ p ∈ widthDomain x c, 0 < widthLikelihood x c p ∧
      IsMaxOn (widthLikelihood x c) (widthDomain x c) p) ↔ ∃ i, x i ≠ c
```

[Lean source](../../lean/UnivariateML/Family/LocScaleTriangular.lean#L518)

#### `UnivariateML.LocScaleTriangular.held_upper_mode_collapse`

Fixing the upper endpoint at the common observation and mode still allows unbounded collapse.

```lean
theorem held_upper_mode_collapse (hn : 0 < n) (m : ℝ) :
    let x : Fin n → ℝ := fun _ ↦ m
    ¬ BddAbove (widthLikelihood x m '' heldUpperModeDomain x m m)
```

[Lean source](../../lean/UnivariateML/Family/LocScaleTriangular.lean#L534)

#### `UnivariateML.LocScaleTriangular.held_lower_collapse`

Constant observations at a held lower endpoint have an unbounded likelihood as the upper endpoint approaches them along the mode.

```lean
theorem held_lower_collapse (hn : 0 < n) (m : ℝ) :
    let x : Fin n → ℝ := fun _ ↦ m
    ¬ BddAbove (jointLikelihood x '' heldLowerDomain x m)
```

[Lean source](../../lean/UnivariateML/Family/LocScaleTriangular.lean#L556)

#### `UnivariateML.LocScaleTriangular.held_lower_criterion`

Holding the lower endpoint gives a positive maximum exactly when all observations are compatible and at least one is strictly above it.

```lean
theorem held_lower_criterion (hn : 0 < n) (x : Fin n → ℝ) (a : ℝ) :
    (∃ p ∈ heldLowerDomain x a, 0 < jointLikelihood x p ∧
      IsMaxOn (jointLikelihood x) (heldLowerDomain x a) p) ↔
    (∀ j, a ≤ x j) ∧ ∃ j, x j ≠ a
```

[Lean source](../../lean/UnivariateML/Family/LocScaleTriangular.lean#L574)

#### `UnivariateML.LocScaleTriangular.held_lower_winner_is_knot`

Any positive global winner at a held lower endpoint has observation mode. The fixed-endpoint comparison applies to that winner's width.

Necessary knot condition.

```lean
theorem held_lower_winner_is_knot (hn : 0 < n) (x : Fin n → ℝ) {a : ℝ}
    {p : ℝ × (ℝ × ℝ)} (hp : p ∈ heldLowerDomain x a) (hpos : 0 < jointLikelihood x p)
    (hm : IsMaxOn (jointLikelihood x) (heldLowerDomain x a) p) : ∃ j, x j = p.2.2
```

[Lean source](../../lean/UnivariateML/Family/LocScaleTriangular.lean#L631)

#### `UnivariateML.LocScaleTriangular.held_upper_criterion`

Holding the upper endpoint gives a positive maximum exactly when all observations are compatible and at least one lies strictly below it.

```lean
theorem held_upper_criterion (hn : 0 < n) (x : Fin n → ℝ) (b : ℝ) :
    (∃ p ∈ heldUpperDomain x b, 0 < jointLikelihood x p ∧
      IsMaxOn (jointLikelihood x) (heldUpperDomain x b) p) ↔
    (∀ j, x j ≤ b) ∧ ∃ j, x j ≠ b
```

[Lean source](../../lean/UnivariateML/Family/LocScaleTriangular.lean#L679)

#### `UnivariateML.LocScaleTriangular.held_upper_collapse`

The joint constant-sample collapse is also feasible with its upper endpoint held at the common observation.

```lean
theorem held_upper_collapse (hn : 0 < n) (m : ℝ) :
    let x : Fin n → ℝ := fun _ ↦ m
    ¬ BddAbove (jointLikelihood x '' heldUpperDomain x m)
```

[Lean source](../../lean/UnivariateML/Family/LocScaleTriangular.lean#L704)

#### `UnivariateML.LocScaleTriangular.held_upper_winner_is_knot`

Any positive global winner at a held upper endpoint has observation mode.

Necessary knot condition.

```lean
theorem held_upper_winner_is_knot (hn : 0 < n) (x : Fin n → ℝ) {b : ℝ}
    {p : ℝ × (ℝ × ℝ)} (hp : p ∈ heldUpperDomain x b) (hpos : 0 < jointLikelihood x p)
    (hm : IsMaxOn (jointLikelihood x) (heldUpperDomain x b) p) : ∃ j, x j = p.2.2
```

[Lean source](../../lean/UnivariateML/Family/LocScaleTriangular.lean#L720)

#### `UnivariateML.LocScaleTriangular.held_lower_mode_unique`

A positive maximizer with lower endpoint and mode held has a unique upper width, including a zero right width if no residual is on the right.

```lean
theorem held_lower_mode_unique (hn : 0 < n) (x : Fin n → ℝ) {a c : ℝ}
    {p q : ℝ × ℝ} (hp : p ∈ heldLowerModeDomain x a c) (hq : q ∈ heldLowerModeDomain x a c)
    (hLp : 0 < widthLikelihood x c p) (hLq : 0 < widthLikelihood x c q)
    (hpm : IsMaxOn (widthLikelihood x c) (heldLowerModeDomain x a c) p)
    (hqm : IsMaxOn (widthLikelihood x c) (heldLowerModeDomain x a c) q) : p = q
```

[Lean source](../../lean/UnivariateML/Family/LocScaleTriangular.lean#L895)

#### `UnivariateML.LocScaleTriangular.held_upper_mode_unique`

A positive maximizer with upper endpoint and mode held has a unique lower width.

```lean
theorem held_upper_mode_unique (hn : 0 < n) (x : Fin n → ℝ) {b c : ℝ}
    {p q : ℝ × ℝ} (hp : p ∈ heldUpperModeDomain x b c) (hq : q ∈ heldUpperModeDomain x b c)
    (hLp : 0 < widthLikelihood x c p) (hLq : 0 < widthLikelihood x c q)
    (hpm : IsMaxOn (widthLikelihood x c) (heldUpperModeDomain x b c) p)
    (hqm : IsMaxOn (widthLikelihood x c) (heldUpperModeDomain x b c) q) : p = q
```

[Lean source](../../lean/UnivariateML/Family/LocScaleTriangular.lean#L928)

#### `UnivariateML.LocScaleTriangular.held_mode_unique`

At held mode, every positive global maximizer has the same two widths. One-sided samples include the zero width on the unused side.

```lean
theorem held_mode_unique (hn : 0 < n) (x : Fin n → ℝ) {c : ℝ}
    {p q : ℝ × ℝ} (hp : p ∈ widthDomain x c) (hq : q ∈ widthDomain x c)
    (hLp : 0 < widthLikelihood x c p) (hLq : 0 < widthLikelihood x c q)
    (hpm : IsMaxOn (widthLikelihood x c) (widthDomain x c) p)
    (hqm : IsMaxOn (widthLikelihood x c) (widthDomain x c) q) : p = q
```

[Lean source](../../lean/UnivariateML/Family/LocScaleTriangular.lean#L961)

#### `UnivariateML.LocScaleTriangular.held_lower_knot_maximizers`

For a held lower endpoint, positive maxima are exactly the observed knot fits whose heights dominate all feasible widths at all knots. Equality retains every winning knot. The width at each winning knot is unique by `held_lower_mode_unique`.

```lean
theorem held_lower_knot_maximizers (hn : 0 < n) (x : Fin n → ℝ) {a : ℝ}
    {p : ℝ × (ℝ × ℝ)} (hp : p ∈ heldLowerDomain x a) (hpos : 0 < jointLikelihood x p) :
    IsMaxOn (jointLikelihood x) (heldLowerDomain x a) p ↔
      (∃ j, x j = p.2.2) ∧
      ∀ q ∈ heldLowerDomain x a, (∃ j, x j = q.2.2) → jointLikelihood x q ≤ jointLikelihood x p
```

[Lean source](../../lean/UnivariateML/Family/LocScaleTriangular.lean#L1078)

#### `UnivariateML.LocScaleTriangular.held_upper_knot_maximizers`

For a held upper endpoint, positive maxima likewise retain exactly the equal winning knots. The width at each winning knot is unique by `held_upper_mode_unique`.

```lean
theorem held_upper_knot_maximizers (hn : 0 < n) (x : Fin n → ℝ) {b : ℝ}
    {p : ℝ × (ℝ × ℝ)} (hp : p ∈ heldUpperDomain x b) (hpos : 0 < jointLikelihood x p) :
    IsMaxOn (jointLikelihood x) (heldUpperDomain x b) p ↔
      (∃ j, x j = p.2.2) ∧
      ∀ q ∈ heldUpperDomain x b, (∃ j, x j = q.2.2) → jointLikelihood x q ≤ jointLikelihood x p
```

[Lean source](../../lean/UnivariateML/Family/LocScaleTriangular.lean#L1090)

#### `UnivariateML.LocScaleTriangular.held_endpoint_incompatible_zero`

An excluded observation or an observed endpoint different from the mode makes the entire likelihood zero.

Zero at incompatible support, including impossible held contacts.

```lean
theorem held_endpoint_incompatible_zero (x : Fin n → ℝ) (a b c : ℝ)
    (hx : ∃ j, x j < a ∨ b < x j ∨ (x j = a ∧ c ≠ a) ∨ (x j = b ∧ c ≠ b)) :
    supportLikelihood x (a,b,c) = 0
```

[Lean source](../../lean/UnivariateML/Family/LocScaleTriangular.lean#L1107)


### UnivariateML.LocScaleGPD

#### `UnivariateML.LocScaleGPD.held_threshold_tie_path`

A threshold tie at held location permits divergence within the fitted shape domain, along a fixed positive shape and vanishing scale.

Free shape remains at a chosen value on the path.

```lean
theorem held_threshold_tie_path (hn : 0 < n) (y : Fin n → ℝ) (hy : ∀ i, 0 ≤ y i)
    {j : Fin n} (hj : y j = 0) :
    let ξ := 2 * (n : ℝ)
    (∀ t : ℝ, exp (-t) ∈ scaleDomain y ξ) ∧
      Tendsto (fun t : ℝ ↦ logLik y (exp (-t)) ξ) atTop atTop
```

[Lean source](../../lean/UnivariateML/Family/LocScaleGPD.lean#L46)

#### `UnivariateML.LocScaleGPD.held_positive_shape_collapse_trace`

The exact leading term of the log likelihood at zero scale and positive held shape. A zero excess contributes exponent `-1`; each positive excess contributes `1 / ξ`.

Exact leading exponent and multiplier at positive held shape.

```lean
theorem held_positive_shape_collapse_trace (y : Fin n → ℝ) (hy : ∀ i, 0 ≤ y i)
    {ξ : ℝ} (hξ : 0 < ξ) :
    let exponent := ∑ i, if y i = 0 then (-1 : ℝ) else 1/ξ
    let constant := ∑ i, if y i = 0 then (0 : ℝ) else -(1+1/ξ)*log (ξ*y i)
    Tendsto (fun s ↦ logLik y s ξ - exponent*log s) (𝓝[>] 0) (𝓝 constant)
```

[Lean source](../../lean/UnivariateML/Family/LocScaleGPD.lean#L93)

#### `UnivariateML.LocScaleGPD.held_threshold_criterion`

For a nonempty nonnegative excess sample, the shape-restricted joint fit attains exactly when every excess is strictly positive.

```lean
theorem held_threshold_criterion (hn : 0 < n) (y : Fin n → ℝ) (hy : ∀ i, 0 ≤ y i) :
    (∃ s ξ, -1 ≤ ξ ∧ s ∈ scaleDomain y ξ ∧
      ∀ κ ≥ -1, ∀ t ∈ scaleDomain y κ, logLik y t κ ≤ logLik y s ξ) ↔
        ∀ i, 0 < y i
```

[Lean source](../../lean/UnivariateML/Family/LocScaleGPD.lean#L125)

#### `UnivariateML.LocScaleGPD.held_scale_all_zero`

For all-zero excesses, every fitted shape at the held scale has the same likelihood and is a maximizer.

All-zero branch.

```lean
theorem held_scale_all_zero (s : ℝ) (hs : 0 < s) (y : Fin n → ℝ) (hy : ∀ i, y i = 0) :
    shapeDomain y s = Ici (-1) ∧
      ∀ ξ ∈ shapeDomain y s, logLik y s ξ = -(n : ℝ)*log s
```

[Lean source](../../lean/UnivariateML/Family/LocScaleGPD.lean#L176)

#### `UnivariateML.LocScaleGPD.held_scale_uniform_unique`

With a positive excess and all excesses within the held scale, the closed uniform member is the unique maximizing shape.

Positive excesses within a closed uniform support.

```lean
theorem held_scale_uniform_unique (y : Fin n → ℝ) {s : ℝ} (hs : 0 < s)
    (hy : ∀ i, y i ∈ Icc 0 s) (hne : ∃ i, 0 < y i) :
    (-1 : ℝ) ∈ shapeDomain y s ∧ IsMaxOn (logLik y s) (shapeDomain y s) (-1) ∧
      ∀ ξ ∈ shapeDomain y s, ξ ≠ -1 → logLik y s ξ < logLik y s (-1)
```

[Lean source](../../lean/UnivariateML/Family/LocScaleGPD.lean#L187)

#### `UnivariateML.LocScaleGPD.held_scale_uniform_maximizers`

At held scale, whenever a uniform support can cover the sample, the entire maximizing set is the feasible uniform locations together with all fitted shapes at a common observation, if there is one.

Uniform-feasible branch, including constants.

```lean
theorem held_scale_uniform_maximizers (x : Fin n → ℝ) {s : ℝ} (hs : 0 < s)
    (hw : ∃ m, ∀ i, x i - m ∈ Icc 0 s) :
    ∀ m ξ, (ξ ∈ shapeDomain (fun i ↦ x i - m) s ∧
      ∀ m' ξ', ξ' ∈ shapeDomain (fun i ↦ x i - m') s →
        logLik (fun i ↦ x i - m') s ξ' ≤ logLik (fun i ↦ x i - m) s ξ) ↔
      (ξ = -1 ∧ ∀ i, x i - m ∈ Icc 0 s) ∨ (-1 ≤ ξ ∧ ∀ i, x i = m)
```

[Lean source](../../lean/UnivariateML/Family/LocScaleGPD.lean#L218)

#### `UnivariateML.LocScaleGPD.held_threshold_scale_attains`

At held positive scale and threshold, every nonempty nonnegative excess sample has an attained maximum on the fitted shape domain.

All compatible samples, including ties and uniform support contacts.

```lean
theorem held_threshold_scale_attains (y : Fin n → ℝ) (hy : ∀ i, 0 ≤ y i)
    {s : ℝ} (hs : 0 < s) :
    ∃ ξ ∈ shapeDomain y s, IsMaxOn (logLik y s) (shapeDomain y s) ξ
```

[Lean source](../../lean/UnivariateML/Family/LocScaleGPD.lean#L300)

#### `UnivariateML.LocScaleGPD.nonuniform_location_strict`

Moving a feasible nonuniform location toward the sample minimum strictly raises the likelihood at fixed scale and shape.

Strict profile to sample minimum on every nonuniform feasible slice.

```lean
theorem nonuniform_location_strict (hn : 0 < n) (x : Fin n → ℝ)
    {s ξ m M : ℝ} (hξ : ξ ≠ -1) (h : ξ ∈ shapeDomain (fun i ↦ x i-m) s)
    (hM : ∀ i, M ≤ x i) (hm : m < M) :
    ξ ∈ shapeDomain (fun i ↦ x i-M) s ∧
      logLik (fun i ↦ x i-m) s ξ < logLik (fun i ↦ x i-M) s ξ
```

[Lean source](../../lean/UnivariateML/Family/LocScaleGPD.lean#L439)

#### `UnivariateML.LocScaleGPD.held_scale_attains`

Every nonempty sample admits a joint threshold and shape maximum at positive held scale on the fitted branch.

Full threshold-and-shape attainment with held scale.

```lean
theorem held_scale_attains (hn : 0 < n) (x : Fin n → ℝ) {s : ℝ} (hs : 0 < s) :
    ∃ m ξ, ξ ∈ shapeDomain (fun i ↦ x i-m) s ∧
      ∀ q κ, κ ∈ shapeDomain (fun i ↦ x i-q) s →
        logLik (fun i ↦ x i-q) s κ ≤ logLik (fun i ↦ x i-m) s ξ
```

[Lean source](../../lean/UnivariateML/Family/LocScaleGPD.lean#L463)

#### `UnivariateML.LocScaleGPD.infinite_shape_positive_excess`

At held scale, every feasible infinite-shape path for which some excess stays uniformly positive has log likelihood tending to minus infinity. The threshold may move along the path.

Vanishing with at least one positive excess. All-zero trace is held_scale_all_zero. Uniform upper bound when any moving excess stays positive.

```lean
theorem infinite_shape_positive_excess {A : Type*} {l : Filter A} (x : Fin n → ℝ)
    {s : ℝ} (hs : 0 < s) (m ξ : A → ℝ) (hξ : Tendsto ξ l atTop)
    (hvalid : ∀ᶠ t in l, ξ t ∈ shapeDomain (fun i ↦ x i-m t) s)
    {j : Fin n} {d : ℝ} (hd : 0 < d) (hj : ∀ᶠ t in l, d ≤ x j-m t) :
    Tendsto (fun t ↦ logLik (fun i ↦ x i-m t) s (ξ t)) l atBot
```

[Lean source](../../lean/UnivariateML/Family/LocScaleGPD.lean#L495)

#### `UnivariateML.LocScaleGPD.infinite_shape_nonconstant`

Every infinite-shape path at held scale vanishes for nonconstant samples, including when the feasible threshold varies.

Infinite-shape end on any feasible moving-threshold path for nonconstant data.

```lean
theorem infinite_shape_nonconstant {A : Type*} {l : Filter A} (x : Fin n → ℝ)
    {s : ℝ} (hs : 0 < s) (m ξ : A → ℝ) (hξ : Tendsto ξ l atTop)
    (hvalid : ∀ᶠ t in l, ξ t ∈ shapeDomain (fun i ↦ x i-m t) s)
    (hne : ∃ i j, x i < x j) :
    Tendsto (fun t ↦ logLik (fun i ↦ x i-m t) s (ξ t)) l atBot
```

[Lean source](../../lean/UnivariateML/Family/LocScaleGPD.lean#L526)

#### `UnivariateML.LocScaleGPD.constant_fixed_shape_collapse`

At the common observation, zero-scale collapse on constant data diverges at every fixed shape, with positive scales remaining feasible.

The constant-data branch; path at the common observation.

```lean
theorem constant_fixed_shape_collapse (hn : 0 < n) (ξ : ℝ) :
    (∀ s > 0, s ∈ scaleDomain (fun _ : Fin n ↦ 0) ξ) ∧
      Tendsto (fun s ↦ logLik (fun _ : Fin n ↦ 0) s ξ) (𝓝[>] 0) atTop
```

[Lean source](../../lean/UnivariateML/Family/LocScaleGPD.lean#L539)

#### `UnivariateML.LocScaleGPD.held_uniform_shape_unique`

At the fitted uniform shape, for nonconstant data the unique maximum has location at the sample minimum and scale equal to the range.

Exact fitted uniform branch, with closed upper support.

```lean
theorem held_uniform_shape_unique (hn : 0 < n) (x : Fin n → ℝ)
    {i j : Fin n} (hi : ∀ k, x i ≤ x k) (hj : ∀ k, x k ≤ x j) (hspan : x i < x j) :
    ∀ m s, (s ∈ scaleDomain (fun k ↦ x k-m) (-1) ∧
      ∀ q t, t ∈ scaleDomain (fun k ↦ x k-q) (-1) →
        logLik (fun k ↦ x k-q) t (-1) ≤ logLik (fun k ↦ x k-m) s (-1)) ↔
      m = x i ∧ s = x j-x i
```

[Lean source](../../lean/UnivariateML/Family/LocScaleGPD.lean#L552)

#### `UnivariateML.LocScaleGPD.held_shape_below_uniform_unbounded`

Below the fitted shape floor, finite-density likelihoods are unbounded for every nonempty sample, including constants.

Finite-density points below the fitted branch, including constant samples.

```lean
theorem held_shape_below_uniform_unbounded (hn : 0 < n) (x : Fin n → ℝ)
    {ξ : ℝ} (hξ : ξ < -1) :
    ∃ m, (∀ i, m < x i) ∧ ∀ B : ℝ, ∃ s ∈ scaleDomain (fun i ↦ x i-m) ξ,
      B < logLik (fun i ↦ x i-m) s ξ
```

[Lean source](../../lean/UnivariateML/Family/LocScaleGPD.lean#L587)

#### `UnivariateML.LocScaleGPD.held_scale_nonuniform_upper_contact`

Approaching a finite nonuniform upper support contact at held scale forces the likelihood to zero, even when the threshold moves.

Joint finite nonuniform upper contact, including when a threshold is held. Joint finite nonuniform upper contact with moving threshold.

```lean
theorem held_scale_nonuniform_upper_contact {A : Type*} {l : Filter A} (x : Fin n → ℝ)
    {s κ M : ℝ} (hs : 0 < s) (hκ : -1 < κ) (m ξ : A → ℝ)
    (hm : Tendsto m l (𝓝 M)) (hξ : Tendsto ξ l (𝓝 κ))
    (hvalid : ∀ᶠ t in l, ξ t ∈ shapeDomain (fun i ↦ x i-m t) s)
    {j : Fin n} (hj : 0 < x j-M) (hcontact : s+κ*(x j-M) = 0) :
    Tendsto (fun t ↦ logLik (fun i ↦ x i-m t) s (ξ t)) l atBot
```

[Lean source](../../lean/UnivariateML/Family/LocScaleGPD.lean#L598)

#### `UnivariateML.LocScaleGPD.held_nonuniform_scale_criterion`

At a fixed nonuniform fitted shape, nonzero nonnegative excess data have a unique scale optimum precisely on the positive-exponent branch for positive shape, and always at nonpositive shape.

Exact conditional scale comparison above the uniform floor.

```lean
theorem held_nonuniform_scale_criterion (hn : 0 < n) (y : Fin n → ℝ) (hy : ∀ i, 0 ≤ y i)
    (hnz : ∃ i, 0 < y i) {ξ : ℝ} (hξ : -1 < ξ) :
    (∃! s, s ∈ scaleDomain y ξ ∧ IsMaxOn (fun a ↦ logLik y a ξ) (scaleDomain y ξ) s) ↔
      ξ ≤ 0 ∨ 0 < collapseExponent y ξ
```

[Lean source](../../lean/UnivariateML/Family/LocScaleGPD.lean#L904)

#### `UnivariateML.LocScaleGPD.held_nonuniform_shape_criterion`

With fitted shape strictly above the uniform, the full joint threshold and scale fit exists exactly on the scale-attainment branch at the sample minimum. Whenever it exists it is unique.

Threshold profiled to the minimum, nonconstant branch above the uniform floor.

```lean
theorem held_nonuniform_shape_criterion (hn : 0 < n) (x : Fin n → ℝ)
    {i : Fin n} (hi : ∀ j, x i ≤ x j) (hnc : ∃ j, x i < x j) {ξ : ℝ} (hξ : -1 < ξ) :
    let y := fun j ↦ x j-x i
    (∃! p : ℝ × ℝ, p.2 ∈ scaleDomain (fun j ↦ x j-p.1) ξ ∧
      ∀ m s, s ∈ scaleDomain (fun j ↦ x j-m) ξ →
        logLik (fun j ↦ x j-m) s ξ ≤ logLik (fun j ↦ x j-p.1) p.2 ξ) ↔
      ξ ≤ 0 ∨ 0 < collapseExponent y ξ
```

[Lean source](../../lean/UnivariateML/Family/LocScaleGPD.lean#L916)

#### `UnivariateML.LocScaleGPD.held_positive_shape_critical_face`

At a positive held shape with zero collapse exponent, the scale supremum is the finite zero-scale trace and every positive scale loses.

Critical exponent at additionally held positive shape. Finite unattained zero-scale value at the observed minimum.

```lean
theorem held_positive_shape_critical_face (y : Fin n → ℝ) (hy : ∀ i, 0 ≤ y i)
    (hnz : ∃ i, 0 < y i) {ξ : ℝ} (hξ : 0 < ξ) (hA : collapseExponent y ξ = 0) :
    let H := ∑ i, if y i = 0 then (0 : ℝ) else -(1+1/ξ)*log (ξ*y i)
    IsLUB ((fun s ↦ logLik y s ξ) '' Ioi 0) H ∧
      ∀ s > 0, logLik y s ξ < H
```

[Lean source](../../lean/UnivariateML/Family/LocScaleGPD.lean#L978)

#### `UnivariateML.LocScaleGPD.held_positive_shape_negative_exponent`

A negative collapse exponent at positive held shape gives unbounded likelihood along the zero-scale path.

Divergence when the exponent is negative.

```lean
theorem held_positive_shape_negative_exponent (y : Fin n → ℝ) (hy : ∀ i, 0 ≤ y i)
    {ξ : ℝ} (hξ : 0 < ξ) (hA : collapseExponent y ξ < 0) :
    Tendsto (fun s ↦ logLik y s ξ) (𝓝[>] 0) atTop
```

[Lean source](../../lean/UnivariateML/Family/LocScaleGPD.lean#L1006)

#### `UnivariateML.LocScaleGPD.held_shape_finite_criterion`

The full held-shape finite-likelihood criterion. A fit exists exactly on the fitted shape branch, on nonconstant data, and, at positive shape, when the exponent at the sample minimum is positive. Every such fit is unique.

All fitted and rejected branches for finite likelihood. The outside-branch extended-real argmax is separate.

```lean
theorem held_shape_finite_criterion (hn : 0 < n) (x : Fin n → ℝ) {i j : Fin n}
    (hi : ∀ k, x i ≤ x k) (hj : ∀ k, x k ≤ x j) (ξ : ℝ) :
    let y := fun k ↦ x k-x i
    let Fit := fun p : ℝ × ℝ ↦ p.2 ∈ scaleDomain (fun k ↦ x k-p.1) ξ ∧
      ∀ m s, s ∈ scaleDomain (fun k ↦ x k-m) ξ →
        logLik (fun k ↦ x k-m) s ξ ≤ logLik (fun k ↦ x k-p.1) p.2 ξ
    ((∃ p, Fit p) ↔ -1 ≤ ξ ∧ x i < x j ∧ (ξ ≤ 0 ∨ 0 < collapseExponent y ξ)) ∧
      ∀ p q, Fit p → Fit q → p = q
```

[Lean source](../../lean/UnivariateML/Family/LocScaleGPD.lean#L1019)

#### `UnivariateML.LocScaleGPD.held_positive_shape_joint_critical_face`

The critical positive-shape trace is the exact joint supremum when location is free. The path at the observed minimum realizes its height, and every feasible finite tuple loses strictly.

Exact joint least upper bound at critical exponent.

```lean
theorem held_positive_shape_joint_critical_face (hn : 0 < n) (x : Fin n → ℝ)
    {i : Fin n} (hi : ∀ j, x i ≤ x j) (hnc : ∃ j, x i < x j)
    {ξ : ℝ} (hξ : 0 < ξ) (hA : collapseExponent (fun j ↦ x j-x i) ξ = 0) :
    let y := fun j ↦ x j-x i
    let H := ∑ j, if y j = 0 then (0 : ℝ) else -(1+1/ξ)*log (ξ*y j)
    IsLUB {v | ∃ m s, s ∈ scaleDomain (fun j ↦ x j-m) ξ ∧
      v = logLik (fun j ↦ x j-m) s ξ} H ∧
    ∀ m s, s ∈ scaleDomain (fun j ↦ x j-m) ξ → logLik (fun j ↦ x j-m) s ξ < H
```

[Lean source](../../lean/UnivariateML/Family/LocScaleGPD.lean#L1082)

#### `UnivariateML.LocScaleGPD.moving_threshold_positive_shape_trace`

The zero-scale log likelihood at positive shape, with the threshold moving toward an observed minimum, has the same leading scale exponent and the exact additional logarithmic contact penalty.

Joint threshold and scale path with the exact contact penalty.

```lean
theorem moving_threshold_positive_shape_trace {A : Type*} {l : Filter A}
    (y : Fin n → ℝ) (hy : ∀ i, 0 ≤ y i) {ξ : ℝ} (hξ : 0 < ξ) (s δ : A → ℝ)
    (hs : Tendsto s l (𝓝[>] 0)) (hδ : Tendsto δ l (𝓝 0))
    (hδ₀ : ∀ᶠ t in l, 0 ≤ δ t) :
    let H := ∑ i, if y i = 0 then (0 : ℝ) else -(1+1/ξ)*log (ξ*y i)
    Tendsto (fun t ↦ logLik (fun i ↦ y i+δ t) (s t) ξ -
      collapseExponent y ξ*log (s t) +
      ∑ i, if y i = 0 then (1+1/ξ)*log (1+ξ*(δ t/s t)) else 0) l (𝓝 H)
```

[Lean source](../../lean/UnivariateML/Family/LocScaleGPD.lean#L1118)


### UnivariateML.LocScaleDoublePareto

#### `UnivariateML.LocScaleDoublePareto.held_left_winner_is_eligible`

For a held left rate, every joint winner is an observation knot with positive right deviation. The conditional right rate is uniquely determined.

```lean
theorem held_left_winner_is_eligible {z : Fin n → ℝ} (hn : 0 < n) {a b c : ℝ}
    (ha : 0 < a) (hb : 0 < b)
    (hm : ∀ d r, 0 < r → logLik z d a r ≤ logLik z c a b) :
    c ∈ range z ∧ 0 < right z c ∧
      ∃! r, r ∈ Ioi (0 : ℝ) ∧ IsMaxOn (objective (left z c) (right z c) a) (Ioi 0) r
```

[Lean source](../../lean/UnivariateML/Family/LocScaleDoublePareto.lean#L99)

#### `UnivariateML.LocScaleDoublePareto.held_left_supremum`

With the left rate held, the exact global supremum is the maximum of the one reachable power face and the finite conditional-knot values.

```lean
theorem held_left_supremum {z : Fin n → ℝ} (hn : 0 < n) {a : ℝ} (ha : 0 < a)
    {u : Fin n} (hu : ∀ i, z i ≤ z u) :
    let V := fun c b ↦ logLik z c a b
    let E := log a - a*left z (z u)
    let K := {v : ℝ | ∃ i r, 0 < right z (z i) ∧ 0 < r ∧
      IsMaxOn (objective (left z (z i)) (right z (z i)) a) (Ioi 0) r ∧ v = V (z i) r}
    K.Finite ∧ IsLUB (insert E K) (sSup (insert E K)) ∧
    IsLUB {v | ∃ c b, 0 < b ∧ v = V c b} (sSup (insert E K))
```

[Lean source](../../lean/UnivariateML/Family/LocScaleDoublePareto.lean#L122)

#### `UnivariateML.LocScaleDoublePareto.held_left_maximizers`

The complete left-held winning set consists of exactly the eligible conditional knot winners at or above the excluded power face and every other eligible conditional knot value.

```lean
theorem held_left_maximizers {z : Fin n → ℝ} (hn : 0 < n) {a : ℝ} (ha : 0 < a)
    {u : Fin n} (hu : ∀ i, z i ≤ z u) (c b : ℝ) :
    let eligible := fun d r ↦ d ∈ range z ∧ 0 < right z d ∧ 0 < r ∧
      IsMaxOn (objective (left z d) (right z d) a) (Ioi 0) r
    (0 < b ∧ ∀ d r, 0 < r → logLik z d a r ≤ logLik z c a b) ↔
      eligible c b ∧ log a - a*left z (z u) ≤ logLik z c a b ∧
        ∀ d r, eligible d r → logLik z d a r ≤ logLik z c a b
```

[Lean source](../../lean/UnivariateML/Family/LocScaleDoublePareto.lean#L178)

#### `UnivariateML.LocScaleDoublePareto.held_left_power_face`

Below the power face at every eligible conditional knot, the supremum is exactly that face and is unattained, including constants.

```lean
theorem held_left_power_face {z : Fin n → ℝ} (hn : 0 < n) {a : ℝ} (ha : 0 < a)
    {u : Fin n} (hu : ∀ i, z i ≤ z u) :
    let E := log a - a*left z (z u)
    ((∀ c b, 0 < b → logLik z c a b < E) ∧
      IsLUB {v | ∃ c b, 0 < b ∧ v = logLik z c a b} E) ↔
      ∀ i b, 0 < right z (z i) → 0 < b →
        IsMaxOn (objective (left z (z i)) (right z (z i)) a) (Ioi 0) b →
        logLik z (z i) a b < E
```

[Lean source](../../lean/UnivariateML/Family/LocScaleDoublePareto.lean#L204)

#### `UnivariateML.LocScaleDoublePareto.held_left_power_path`

At any kink at or above the sample maximum, sending the right rate to infinity reaches the power face with the left rate unchanged.

Any fixed kink at or above the sample maximum.

```lean
theorem held_left_power_path {z : Fin n → ℝ} {a c : ℝ} (ha : 0 < a)
    (hc : ∀ i, z i ≤ c) :
    Tendsto (logLik z c a) atTop (𝓝 (log a - a*left z c))
```

[Lean source](../../lean/UnivariateML/Family/LocScaleDoublePareto.lean#L232)

#### `UnivariateML.LocScaleDoublePareto.held_right_maximizers`

Reflection gives the complete right-held winning set, with the Pareto face at the sample minimum and a unique left rate at every knot.

```lean
theorem held_right_maximizers {z : Fin n → ℝ} (hn : 0 < n) {b : ℝ} (hb : 0 < b)
    {l : Fin n} (hl : ∀ i, z l ≤ z i) (c a : ℝ) :
    let eligible := fun d r ↦ d ∈ range z ∧ 0 < left z d ∧ 0 < r ∧
      IsMaxOn (fun t ↦ objective (left z d) (right z d) t b) (Ioi 0) r
    (0 < a ∧ ∀ d r, 0 < r → logLik z d r b ≤ logLik z c a b) ↔
      eligible c a ∧ log b - b*right z (z l) ≤ logLik z c a b ∧
        ∀ d r, eligible d r → logLik z d r b ≤ logLik z c a b
```

[Lean source](../../lean/UnivariateML/Family/LocScaleDoublePareto.lean#L254)

#### `UnivariateML.LocScaleDoublePareto.held_right_pareto_path`

Sending the left rate to infinity at any kink at or below the sample minimum reaches the right-held Pareto face.

Any fixed kink at or below the sample minimum.

```lean
theorem held_right_pareto_path {z : Fin n → ℝ} {b c : ℝ} (hb : 0 < b)
    (hc : ∀ i, c ≤ z i) :
    Tendsto (fun a ↦ logLik z c a b) atTop (𝓝 (log b - b*right z c))
```

[Lean source](../../lean/UnivariateML/Family/LocScaleDoublePareto.lean#L289)

#### `UnivariateML.LocScaleDoublePareto.held_right_pareto_face`

If every eligible conditional knot loses, the right-held Pareto face is the exact unattained global supremum.

```lean
theorem held_right_pareto_face {z : Fin n → ℝ} (hn : 0 < n) {b : ℝ} (hb : 0 < b)
    {l : Fin n} (hl : ∀ i, z l ≤ z i) :
    let E := log b - b*right z (z l)
    ((∀ c a, 0 < a → logLik z c a b < E) ∧
      IsLUB {v | ∃ c a, 0 < a ∧ v = logLik z c a b} E) ↔
      ∀ i a, 0 < left z (z i) → 0 < a →
        IsMaxOn (fun t ↦ objective (left z (z i)) (right z (z i)) t b) (Ioi 0) a →
        logLik z (z i) a b < E
```

[Lean source](../../lean/UnivariateML/Family/LocScaleDoublePareto.lean#L299)

#### `UnivariateML.LocScaleDoublePareto.held_left_criterion`

A left-held MLE exists exactly when some eligible knot at its conditional right rate reaches the power face. Equality suffices.

```lean
theorem held_left_criterion {z : Fin n → ℝ} (hn : 0 < n) {a : ℝ} (ha : 0 < a)
    {u : Fin n} (hu : ∀ i, z i ≤ z u) :
    (∃ c b, 0 < b ∧ ∀ d r, 0 < r → logLik z d a r ≤ logLik z c a b) ↔
      ∃ i b, 0 < right z (z i) ∧ 0 < b ∧
        IsMaxOn (objective (left z (z i)) (right z (z i)) a) (Ioi 0) b ∧
        log a - a*left z (z u) ≤ logLik z (z i) a b
```

[Lean source](../../lean/UnivariateML/Family/LocScaleDoublePareto.lean#L335)

#### `UnivariateML.LocScaleDoublePareto.held_right_criterion`

A right-held MLE exists exactly when some eligible knot at its conditional left rate reaches the Pareto face. Equality suffices.

```lean
theorem held_right_criterion {z : Fin n → ℝ} (hn : 0 < n) {b : ℝ} (hb : 0 < b)
    {l : Fin n} (hl : ∀ i, z l ≤ z i) :
    (∃ c a, 0 < a ∧ ∀ d r, 0 < r → logLik z d r b ≤ logLik z c a b) ↔
      ∃ i a, 0 < left z (z i) ∧ 0 < a ∧
        IsMaxOn (fun t ↦ objective (left z (z i)) (right z (z i)) t b) (Ioi 0) a ∧
        log b - b*right z (z l) ≤ logLik z (z i) a b
```

[Lean source](../../lean/UnivariateML/Family/LocScaleDoublePareto.lean#L360)


### UnivariateML.LocScaleFoldedNormal

#### `UnivariateML.LocScaleFoldedNormal.joint_criterion`

The all-free folded normal has a finite MLE exactly on nonconstant nonempty nonnegative samples.

The scale-collapse detector is the complementary branch of the criterion.

```lean
theorem joint_criterion (hn : 0 < n) (x : Fin n → ℝ) (hx : ∀ i, 0 ≤ x i) :
    (∃ μ t, 0 ≤ μ ∧ 0 < t ∧ ∀ a b, 0 ≤ a → 0 < b → logLik x a b ≤ logLik x μ t) ↔
    ∃ i j, x i ≠ x j
```

[Lean source](../../lean/UnivariateML/Family/LocScaleFoldedNormal.lean#L125)

#### `UnivariateML.LocScaleFoldedNormal.held_location_collapse_detector`

At held location the collapse detector is exactly the event that every observation equals that location.

```lean
theorem held_location_collapse_detector (hn : 0 < n) (x : Fin n → ℝ)
    (hx : ∀ i, 0 ≤ x i) {μ : ℝ} (hμ : 0 ≤ μ) :
    (¬ ∃ t > 0, ∀ b > 0, logLik x μ b ≤ logLik x μ t) ↔ ∀ i, x i = μ
```

[Lean source](../../lean/UnivariateML/Family/LocScaleFoldedNormal.lean#L139)


### UnivariateML.LocScaleShiftedGompertz

#### `UnivariateML.LocScaleShiftedGompertz.joint_criterion`

With rate and shape free, the shifted Gompertz fit attains precisely on nonconstant nonnegative samples.

```lean
theorem joint_criterion (hn : 0 < n) (x : Fin n → ℝ) (hx : ∀ i, 0 ≤ x i) :
    (∃ p ∈ jointDomain,
      IsMaxOn (fun q : ℝ × ℝ ↦ logLik x q.1 q.2) jointDomain p) ↔
    ∃ i j, x i ≠ x j
```

[Lean source](../../lean/UnivariateML/Family/LocScaleShiftedGompertz.lean#L25)

#### `UnivariateML.LocScaleShiftedGompertz.held_shape_criterion`

At held fitted shape, the shifted Gompertz rate attains precisely when the sample sum is positive.

```lean
theorem held_shape_criterion (hn : 0 < n) (x : Fin n → ℝ) (hx : ∀ i, 0 ≤ x i)
    {η : ℝ} (hη : 0 ≤ η) :
    (∃ b ∈ Ioi (0 : ℝ), IsMaxOn (fun r ↦ logLik x r η) (Ioi 0) b) ↔ 0 < ∑ i, x i
```

[Lean source](../../lean/UnivariateML/Family/LocScaleShiftedGompertz.lean#L43)

