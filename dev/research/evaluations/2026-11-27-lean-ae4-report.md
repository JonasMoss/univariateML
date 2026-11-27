# TASK-446 · Almost-everywhere uniqueness for two-piece and location-scale fits

The new Lean modules prove four-level finite-winner uniqueness for skew Student with free shape and any requested combination of mean, variance and skew holds. At held mean and unit skew, two unequal nonzero squared radii suffice. The analytic incidence argument proves the five outstanding EMG finite-winner comparisons and both skew-normal comparisons at their stated level counts. The Bhattacharjee result at held center and Gaussian scale includes the normal at width zero. The SGED and GED results apply to the strictly superunit chart. Tukey lambda and Huber have checked intermediate results.

The hypotheses use natural-number counts, fixed before varying the level values, and Lebesgue measure on `Fin n → ℝ`. In the pointwise intersection arguments, `Function.Injective x` selects the distinct-level chart. In the analytic arguments the null result holds on the full level space and hence restricts to ordered distinct levels. At most one finite winner means exactly one whenever there is an attained finite maximum.

## Source scope

- **Tukey lambda, held shape and scale.** The three-level uniqueness statement needs to exclude both uniform shapes, one and two. Calling out only shape one would include shape two, where the prior checked uniform interval can have positive length on open strata. The source gives both exceptions explicitly. [Written proof](2026-11-27-uniq-locscale-math-report.md#tlambda-held-lambdasigma)
- **Pinned coordinates.** A likelihood with a permanently fixed observation can have a term independent of the free level vector and dependent on fitted parameters. The new EMG theorems include any fixed observations through a continuous likelihood offset. The Bhattacharjee theorem treats the nonempty freely varying chart, with a separate exact result for the all-center stratum. [Incidence scope](2026-11-27-uniq-locscale-math-report.md#analytic-incidence-bound)
- **Huber.** For two finite clips that each have a tail observation, the largest absolute entry of their common score vector equals both tail slopes. This gives the tail-slope equality directly. The strict core hypotheses still require the path improvement at a finite maximum. [Clipped scores](2026-11-27-uniq-locscale-math-report.md#huber-clipped-score-separation)
- The ExpSum transfer theorems concern positive-support families outside this job. The skew-Student cubic and SGED logarithmic ratio give direct, stronger pointwise bounds for the relevant score equations. The analytic families use the implicit incidence theorem with a countable partition by multiplicity vector. [Existing incidence theorem](../../lean/UnivariateML/Generic/AE1Incidence.lean)

## Effort and remaining obligations by regime

**Checked** below describes the stated part of a target. **Open** records a separate obligation. Existing results from TASK-437 are identified where they already cover the generic conclusion. [First pass](2026-11-27-lean-ae1-report.md)

### Skew Student

The signed score excludes intersections between distinct joins. On an exterior the Lean proof cancels the linear term between two cubic equations. The residual quadratic factor has strict sign on the side with smaller far shape. On the other side a third root would force a strictly positive factor to vanish. Equal heights identify the total width if one side has no noncontact observations. The continuity proof includes the Gamma normalizer and the moment restrictions.

| Held | Effort and result | Open |
| --- | --- | --- |
| all free | Checked four-level uniqueness and single-level nonattainment by a strict width improvement. Tracked the three- and two-level comparisons to the missing digamma difference bound. Mathlib supplies the recurrence and duplication formulas for complex digamma, and lists the Gauss integral representation as open. | Three equally replicated levels require both width-score equations, the digamma bound that excludes a join contact, and the monotone scalar reconstruction. Two levels also require the strictly positive shape derivative at standardized radius one. Ordinary sizes two and three remain open. The six-level reflected example needs the strict global height comparison. |
| mean | Checked the four-level theorem on the closed mean constraint within the positive chart. | The six-level witness must beat every reflection-fixed fit and applicable end at held mean zero. |
| mean+nu | Prior three-level result applies to the fixed-mean chart. Compared the component inventory against the previous Lean result. | Semialgebraic stationary-component decomposition, comparison with compatible ends and the shape-31 six-level witness. |
| mean+nu+sd | Prior three-level result includes the closed variance constraint. | The fixed-moment stationary inventory and strict comparison with the forced half ends for sd \(9/8\). |
| mean+nu+xi | Prior three-level result covers finite uniqueness. The existing outward profile has a single unweighted scale-root criterion. | Transport the scale-score and collapse criterion to positive counts, and establish the finite root inventory for the full signed case. |
| mean+sd | Checked four-level uniqueness with both moment constraints. | Strict six-level height comparison at mean zero and sd \(9/8\). |
| mean+sd+xi | Checked four-level uniqueness and the two-radius unit-skew result with held variance. | The stated finite uniqueness and sharper unit-skew conclusion are covered. |
| mean+xi | Checked four-level uniqueness and the two-radius unit-skew result with free variance. | The stated finite uniqueness and sharper unit-skew conclusion are covered. |
| nu | Prior three-level result applies to both widths and join free. | The component/end inventory and shape-31 strict reflected comparison. |
| nu+sd | Prior three-level result applies to the variance constraint. | The stationary-component inventory and the strict half-end and centered exclusions for the stated witness. |
| nu+xi | Prior three-level result applies to the width-ratio constraint. At unit skew this is the ordinary fixed-df Student chart. | Public-coordinate transport of the pointwise ordinary Student theorem and the general component/end inventory. |
| sd | Checked four-level uniqueness on the closed variance constraint within the positive chart. | Six-level global height exclusions at sd \(9/8\). |
| sd+xi | Checked four-level uniqueness. At unit skew the two-level witness cannot be obtained from this score count. | Shape-profile denominator minimization and strict centered/face comparison near half-gap 100. |
| xi | Checked four-level uniqueness on the fixed width-ratio slice. | The stated finite uniqueness is covered. |

[Skew-Student intersection and held analyses](2026-11-27-uniq-skew2-math-report.md#skew-student-separation) · [Three-level stationary calculation](2026-11-27-uniq-generic-math-report.md#pilot-2-skew-student-all-free)

### SGED and GED

The superunit chart uses explicit Gamma-ratio expressions for the held mean, variance and squared skew. For distinct joins the logarithmic exterior score difference has derivative numerator \(uD+(u-v)r\). One application of Rolle rules out two zeros where \(u\ge v>0\). Two applications rule out three zeros on the other exterior. At a common join, subtracting the log-magnitude equations at two positive radii recovers the power. The sample derivative through the join was checked using separate derivative proofs on the two closed half-lines. This argument reaches shape strictly above one. At power one or below, using the same fixed-join derivative at a contacted observation would be invalid.

| Family | Held | Effort and result | Open |
| --- | --- | --- | --- |
| SGED | all free | Checked a.e. uniqueness among superunit candidates. The power-one score assembly fails at a contact because the two one-sided derivatives differ. | Exclude balanced globally winning gaps via the skew derivative. Build the moving-knot chart, including its contact gradient, and compare with the superunit chart. The reflected example requires strict global bounds. |
| SGED | mean | The superunit comparison applies to locally closed mean restrictions. A fixed join chart does not preserve the mean when the contact moves. | Construct the contact chart solving the mean equation via total width and skew. Exclude mixed superunit/Laplace equality and prove the reflected witness. |
| SGED | mean+nu | Checked the three-level fixed superunit comparison on locally closed charts. The subunit exterior count alone omits up to two moving contacts. | Build the two contact charts preserving the held mean and prove the five-level comparison. The power-one exact example is separate. |
| SGED | mean+sd | Checked the superunit comparison for locally closed restrictions. Here the moving contact equation must preserve both moments. | Establish the local inverse in skew for the two-moment constraint, compare the noncontact gradients, and verify the specified-sd witness. |
| SGED | mean+sd+xi | Checked four-level superunit uniqueness. Unit skew with held moments also has the earlier two-radius theorem, including the feasible power-one branch. | Compare arbitrary skew across power one, and show the stated two optimized heights coincide at the exact GED crossing. |
| SGED | mean+xi | Checked the superunit chart. The earlier unit-skew theorem gives the two-radius comparison including power one. | For nonunit skew solve for total width at a moving Laplace contact. The exact high-multiplicity \(h_*\) crossing needs bounds on both optimized profiles. |
| SGED | nu | Checked three-level finite uniqueness for held superunit shape. At a power-one or subunit knot the free-join derivative cannot be used. | For subunit knots, show that the difference of profiled knot losses is analytic and nonzero on a third-level interval. Compare exact profiled knots and half ends, including the power-one witness. |
| SGED | nu+sd | Checked the fixed superunit chart at three levels. The subunit smooth score count does not handle its required moving knots. | The two moving-knot charts, the exclusion of balanced optimal gaps at power one, and the held-sd reflected witness. |
| SGED | sd | Checked the superunit comparison. The mixed comparison requires a moving Laplace knot that preserves the variance restriction. | Build that chart and exclude mixed and Laplace/Laplace clashes, then exclude reflection-fixed and half-end optima at the held-sd witness. |
| SGED | sd+xi | Checked superunit uniqueness in the locally closed chart. A pointwise location interval at power one can share noncontact gradients. | Represent the entire interval as a class, compare its height and gradient with the superunit branch, and prove the strict small-sd plateau bound and exact two-point example. |
| SGED | xi | Checked superunit uniqueness in the locally closed chart. Power-one profiling is flat in location across a balanced gap. | A differentiable profiled median chart and its mixed comparison, plus the strict eight-observation example and its neighborhood. |
| GED | all free | Checked the symmetric superunit specialization with free variance. A median contact falls outside the fixed-parameter derivative theorem. | Construct the profiled weighted median chart, compare it with the smooth branch, and show that superunit and uniform profiles lose at the eight-observation witness. |
| GED | mean | Prior two-radius finite uniqueness applies. The generic proof does not give a shape-root list. | Exact stationary/profile comparison against the centered uniform and the \(h_*\) real crossing. |
| GED | mean+sd | Prior two-radius finite uniqueness applies. | Verify the two optimized heights at \(a_*,b_0\) and exclude other shapes and uniform contacts. |
| GED | sd | Checked the symmetric superunit specialization with held variance. | Compare the weighted median class against all superunit winners. The strict shape-one bound at sd \(1/10\) and its open plateau remain open. |

[SGED and GED score and contact calculations](2026-11-27-uniq-skew2-math-report.md#sged-and-ged-separation)

### EMG

Real analyticity of the Gaussian CDF comes from a primitive of the entire Gaussian kernel on the complex plane. The right spatial score limit identifies the rate. At equal rates, equal scores make the two log-CDF terms differ by a constant, and the right limit forces that constant to zero. Strict monotonicity of the CDF then identifies the two coefficients of its affine argument. The analytic incidence wrapper checks the diagonal sample derivative for each root-multiplicity stratum. Pair dimensions are six, four and two as appropriate.

| Held | Effort and result | Open |
| --- | --- | --- |
| all free | Checked seven-level finite uniqueness. | The moment-adjusted \(X_n\) crossing needs the exact two-window height, compactness and exclusion of additional winners. |
| lambda | Checked five-level finite uniqueness. | The stationary scales and unique conditional location at each one. The score incidence argument gives no root count for the profiled scale derivative. |
| lambda+mu | Checked three-level finite uniqueness. | Formalize strict log-scale concavity on the nonpositive class and the finite scale list for other samples. |
| mu | Checked five-level finite uniqueness. | The eliminated-score comparison on the constant-positive class and the finite stationary-shape inventory. |
| mu+sigma | Prior result proves strict score ordering and finite uniqueness on freely varying nonempty charts. | Pinned-coordinate transport, the fixed positive constant residual Laplace-transform single crossing and the finite rate inventory. |
| sigma | Checked five-level finite uniqueness. | The finite stationary rate list after the conditional location solve. |

[EMG scores and regime results](2026-11-27-uniq-locscale-math-report.md#emg-held-all-free)

### Tukey lambda

The quantile has a checked analytic local inverse at each interior probability and each fixed nonzero shape. The logarithmic shape-zero continuation is also jointly analytic in shape and probability, via the Taylor factorization of the real exponential. Analyticity in the observation alone does not imply that the competing-pair sample block is invertible after taking multiplicity derivatives. The latter needs regularity jointly in the free chart parameters and different score germs on the intersection of the supports.

| Held | Effort and result | Open |
| --- | --- | --- |
| all free | Checked the fixed nonzero-shape interior inverse and the jointly analytic quantile continuation through zero. | Prove the parameter-dependent inverse chart, compare the uniform and collapse charts, and formalize balanced-pair dominance and majority-atom divergence. |
| lambda | Checked the nonzero-shape analytic local inverse and positive quantile derivative. | Convert the inverse to joint smooth score charts, add closed positive-density contacts with their reduced pair dimension, and prove the pointwise interval through \(8/3\) and reflected shape-five witness. |
| lambda+sigma | Prior result checks exact uniform branches at shapes one and two. Checked the nonzero-shape interior chart. | Nonuniform score identification on the overlap, moving-contact Fubini, the exact endpoint maximum at shape \(3/2\), and the curvature and global existence steps at shape \(-1\). |
| mu | Checked the scalar inverse needed on the interior of the nonzero-shape chart. | Build the parameter-dependent inverse through zero, compare with the optimized active-radius uniform and prove the exact \(t_*\) height equality and unequal spatial gradients. |
| sigma | The support-compatible shape-one interval follows from the prior fixed-uniform chart. The local inverse gives the strict-interior prerequisite outside that branch. | Prove dominance of the feasible uniform against all fitted shapes, outside-uniform incidence, and the exact wide-span reflected pair. |

[Tukey lambda support and contact charts](2026-11-27-uniq-locscale-math-report.md#tukey-lambda-support-charts)

### Bhattacharjee

The ratio of paired Gaussian integrals is the conditional mean of the uniform displacement. Lean checks strict ordering of the truncated weighted averages by splitting at the smaller width. At held center and scale, a fixed integration interval supplies a continuous likelihood and sample derivative through width zero. On a nonempty freely varying chart at least one residual is nonzero almost everywhere. The score at that residual identifies width including zero.

| Held | Effort and result | Open |
| --- | --- | --- |
| all free | The fixed-interval chart resolves continuity at width zero in the held-center calculation. The tilted score comparison at fixed location and scale does not identify competing pairs with both varying. | Analytic score regularity jointly in three coordinates, distinctness via moment or tail identification, seven-level incidence and the normal/positive-width global crossing at \(t_N\). |
| mu | Checked width ordering at fixed Gaussian scale. It cannot cancel Gaussian scale when the two candidates have different scales. | Joint two-coordinate chart and five-level incidence including the normal, and the exact held-center crossing bounds. |
| mu+sigma | Checked a.e. finite uniqueness on nonempty free-level charts with width zero included. The all-center sample has the unique normal by strict Gaussian averaging. | General pinned nonzero levels alongside fixed atoms, the exact normal threshold from the held-center second moment, and the two-width global crossing at \(d_*\). |
| sigma | Checked the held-center strict score inequality. With varying center the argument no longer compares the same residual. | Center–width score identity theorem and five-level incidence, the centered second-moment equivalence and the full free-center two-width example. |

[Bhattacharjee width analysis](2026-11-27-uniq-locscale-math-report.md#bhatt-held-musigma)

### Huber

Equal score vectors identify the tail slope when each candidate has a tail. Two distinct strict core levels then determine the common affine central score. At a held center a single off-center strict core level suffices. The normalizer reciprocal in the source's constant-tail-slope path is strictly increasing by the checked bound \(R(c)\le 1/c\).

| Held | Effort and result | Open |
| --- | --- | --- |
| all free | Checked score identification given two strict core levels and a tail for each candidate, and the normalizer curvature step. | Complete the moving-join path, including feasibility at exact cutoffs and the stationary improvement contradiction for zero or one strict core level. Then assemble the joint chart and finite stationary list. |
| mu | Checked score identification with one nonzero strict core level and the same curvature inequality. | Use the fixed-center path to exclude an attained fit with no nonzero strict core, then prove the one-magnitude nonattainment and stationary inventory. |
| mu+sigma | Prior proof checks the strict-tail uniqueness result. Existing equal-magnitude and finite-root results are in the unweighted chart. | Transport these results to positive counts and any pinned-level charts. |
| sigma | Prior theorem checks the generic cutoff. Existing exact interval and separated two-point results apply in their stated charts. | The weighted interval transport and the open plateau at every even size. |

[Huber path and analysis](2026-11-27-uniq-locscale-math-report.md#huber-clipped-score-separation)

### Skew normal

The favorable and unfavorable normalized spatial score tails recover the scale and slant. The favorable tail offset then recovers location. The Gaussian CDF is analytic on the whole real line, including zero slant. The analytic incidence argument applies directly in six or four pair coordinates.

| Held | Effort and result | Open |
| --- | --- | --- |
| all free | Checked seven-level finite uniqueness. Existing reflection lemmas transfer a known nonzero-slant winner to a second one. | For the specified seven-observation example, show strict domination of the reflection-fixed and moving half-normal traces so that the reflection lemma applies. |
| omega | Checked five-level finite uniqueness. | Strict comparison with the optimized zero-slant and moving half-normal competitors at \(\omega=250\) for the specified reflected example. |

[Skew-normal witnesses](2026-11-27-uniq-locscale-math-report.md#sn-held-all-free)

## Checked public Lean statements

### `AE4Analytic`

**`UnivariateML.AE4Analytic.ae_no_analytic_incidence`**. Analytic roots with finite orders give a null sample projection above the parameter dimension. General incidence tool used for EMG and skew-normal. [AE4Analytic.lean](../../lean/UnivariateML/Generic/AE4Analytic.lean#L52)

```lean
theorem ae_no_analytic_incidence (hdn : d < n)
    (S : Set ((Fin d → ℝ) × (Fin n → ℝ))) (f : (Fin d → ℝ) × ℝ → ℝ)
    (ha : ∀ z ∈ S, ∀ i, AnalyticAt ℝ f (z.1, z.2 i))
    (hn : ∀ z ∈ S, ∀ i, ¬ ∀ᶠ t in 𝓝 (z.2 i), f (z.1, t) = 0) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ p, (p, x) ∈ S → ∃ i, f (p, x i) ≠ 0
```

**`UnivariateML.AE4Analytic.ae_unique_analytic_scores`**. Identifiable analytic spatial scores imply a.e. uniqueness on fixed-count level charts above twice the parameter dimension. General weight and winner assembly used for EMG and skew-normal. [AE4Analytic.lean](../../lean/UnivariateML/Generic/AE4Analytic.lean#L113)

```lean
theorem ae_unique_analytic_scores (hdn : d + d < n)
    (w : Fin n → ℕ) (hw : ∀ i, 0 < w i)
    (A : Set (Fin d → ℝ)) (hA : IsOpen A)
    (L g : (Fin d → ℝ) → ℝ → ℝ)
    (hL : ContinuousOn (fun z : (Fin d → ℝ) × ℝ ↦ L z.1 z.2) (A ×ˢ univ))
    (hg : ∀ p ∈ A, ∀ t, AnalyticAt ℝ (fun z : (Fin d → ℝ) × ℝ ↦ g z.1 z.2) (p, t))
    (hDeriv : ∀ p ∈ A, ∀ t, HasDerivAt (L p) (g p t) t)
    (hsep : ∀ p ∈ A, ∀ q ∈ A, (∀ t, g p t = g q t) → p = q) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ p ∈ A, ∀ q ∈ A,
      IsMaxOn (fun r ↦ ∑ i, (w i : ℝ) * L r (x i)) A p →
      IsMaxOn (fun r ↦ ∑ i, (w i : ℝ) * L r (x i)) A q → p = q
```

**`UnivariateML.AE4Analytic.ae_unique_analytic_scores_with_offset`**. Fixed observations may enter as a continuous offset without changing the free-level threshold. General chart extension used for the EMG holds. [AE4Analytic.lean](../../lean/UnivariateML/Generic/AE4Analytic.lean#L213)

```lean
theorem ae_unique_analytic_scores_with_offset (hdn : d + d < n)
    (w : Fin n → ℕ) (hw : ∀ i, 0 < w i)
    (A : Set (Fin d → ℝ)) (hA : IsOpen A)
    (L g : (Fin d → ℝ) → ℝ → ℝ) (C : (Fin d → ℝ) → ℝ)
    (hL : ContinuousOn (fun z : (Fin d → ℝ) × ℝ ↦ L z.1 z.2) (A ×ˢ univ))
    (hC : ContinuousOn C A)
    (hg : ∀ p ∈ A, ∀ t, AnalyticAt ℝ (fun z : (Fin d → ℝ) × ℝ ↦ g z.1 z.2) (p, t))
    (hDeriv : ∀ p ∈ A, ∀ t, HasDerivAt (L p) (g p t) t)
    (hsep : ∀ p ∈ A, ∀ q ∈ A, (∀ t, g p t = g q t) → p = q) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ p ∈ A, ∀ q ∈ A,
      IsMaxOn (fun r ↦ C r + ∑ i, (w i : ℝ) * L r (x i)) A p →
      IsMaxOn (fun r ↦ C r + ∑ i, (w i : ℝ) * L r (x i)) A q → p = q
```

### `AE4Gaussian`

**`UnivariateML.AE4Gaussian.analyticAt_phi`**. The standard Gaussian density is real analytic. Analytic score prerequisite. [AE4Gaussian.lean](../../lean/UnivariateML/Generic/AE4Gaussian.lean#L24)

```lean
theorem analyticAt_phi (x : ℝ) : AnalyticAt ℝ φ x
```

**`UnivariateML.AE4Gaussian.analyticAt_Phi`**. The standard Gaussian CDF is real analytic. Analytic score prerequisite. [AE4Gaussian.lean](../../lean/UnivariateML/Generic/AE4Gaussian.lean#L30)

```lean
theorem analyticAt_Phi (x : ℝ) : AnalyticAt ℝ Φ x
```

### `AE4SSTD`

**`UnivariateML.AE4SSTD.ae_unique_four_levels`**. Four distinct levels suffice for a.e. uniqueness of any attained finite skew-Student fit on a locally closed chart. Generic denominator-width chart. Equal-count three-level data and exact ties are separate. [AE4SSTD.lean](../../lean/UnivariateML/Family/AE4SSTD.lean#L436)

```lean
theorem ae_unique_four_levels (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) (hn : 4 ≤ n)
    (A : Set Parameter) (hA : IsLocallyClosed A)
    (hparam : ∀ p ∈ A, 2 < p.1 ∧ 0 < p.2.2.1 ∧ 0 < p.2.2.2) :
    ∀ᵐ x : Fin n → ℝ ∂volume, Function.Injective x →
      ∀ p ∈ A, ∀ q ∈ A, IsMaxOn (fun r ↦ logLik w r x) A p →
        IsMaxOn (fun r ↦ logLik w r x) A q → p = q
```

**`UnivariateML.AE4SSTD.ae_unique_with_holds`**. With shape free and any combination of mean, variance or skew held, four distinct levels suffice for a.e. finite uniqueness. Use singleton sets for held values, including variance and squared skew. Auxiliary and exact exceptional-sample claims are separate. [AE4SSTD.lean](../../lean/UnivariateML/Family/AE4SSTD.lean#L520)

```lean
theorem ae_unique_with_holds (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) (hn : 4 ≤ n)
    (M V K : Set ℝ) (hM : IsClosed M) (hV : IsClosed V) (hK : IsClosed K) :
    ∀ᵐ x : Fin n → ℝ ∂volume, Function.Injective x →
      ∀ p ∈ domain M V K, ∀ q ∈ domain M V K,
        IsMaxOn (fun r ↦ logLik w r x) (domain M V K) p →
        IsMaxOn (fun r ↦ logLik w r x) (domain M V K) q → p = q
```

**`UnivariateML.AE4SSTD.held_mean_unit_skew_ae_unique`**. At a held mean and unit skew, two unequal nonzero residual radii suffice for a.e. finite uniqueness. Variance can be held or free. [AE4SSTD.lean](../../lean/UnivariateML/Family/AE4SSTD.lean#L531)

```lean
theorem held_mean_unit_skew_ae_unique (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) (m : ℝ)
    (V : Set ℝ) (hV : IsClosed V) :
    ∀ᵐ x : Fin n → ℝ ∂volume,
      (∃ i j, x i ≠ m ∧ x j ≠ m ∧ (x i-m)^2 ≠ (x j-m)^2) →
      ∀ p ∈ domain {m} V {1}, ∀ q ∈ domain {m} V {1},
        IsMaxOn (fun r ↦ logLik w r x) (domain {m} V {1}) p →
        IsMaxOn (fun r ↦ logLik w r x) (domain {m} V {1}) q → p = q
```

**`UnivariateML.AE4SSTD.constant_nonattainment`**. All-free skew-Student fits cannot attain at a single level of positive count. One-level branch. Two-level nonattainment is separate. [AE4SSTD.lean](../../lean/UnivariateML/Family/AE4SSTD.lean#L582)

```lean
theorem constant_nonattainment {n : ℕ} (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) (hn : 0 < n)
    (x : Fin n → ℝ) (c : ℝ) (hx : ∀ i, x i = c) (p : Parameter)
    (hp : p ∈ domain univ univ univ) :
    ¬ IsMaxOn (fun q ↦ logLik w q x) (domain univ univ univ) p
```

### `AE4SGED`

**`UnivariateML.AE4SGED.join_eq_of_four_scores`**. Equal scores at four distinct levels identify the join of a positive-width superunit two-piece generalized-error law. Pointwise score comparison on the superunit branch. [AE4SGED.lean](../../lean/UnivariateML/Family/AE4SGED.lean#L185)

```lean
theorem join_eq_of_four_scores {p q : Parameter}
    (hp : 1 < p.1 ∧ 0 < p.2.2.1 ∧ 0 < p.2.2.2)
    (hq : 1 < q.1 ∧ 0 < q.2.2.1 ∧ 0 < q.2.2.2)
    {a b c d : ℝ} (hab : a < b) (hbc : b < c) (hcd : c < d)
    (ea : spatialScore p a = spatialScore q a) (eb : spatialScore p b = spatialScore q b)
    (ec : spatialScore p c = spatialScore q c) (ed : spatialScore p d = spatialScore q d) :
    p.2.1 = q.2.1
```

**`UnivariateML.AE4SGED.superunit_ae_unique`**. Four distinct levels suffice for a.e. uniqueness among superunit fits on a locally closed chart. For each hold, instantiate the chart by the corresponding superunit slice. Leaves shape-one comparisons open. [AE4SGED.lean](../../lean/UnivariateML/Family/AE4SGED.lean#L507)

```lean
theorem superunit_ae_unique {n : ℕ} (w : Fin n → ℕ) (hw : ∀ i, 0 < w i)
    (hn : 4 ≤ n) (A : Set Parameter) (hA : IsLocallyClosed A)
    (hpar : ∀ p ∈ A, 1 < p.1 ∧ 0 < p.2.2.1 ∧ 0 < p.2.2.2) :
    ∀ᵐ x : Fin n → ℝ ∂volume, Function.Injective x →
      ∀ p ∈ A, ∀ q ∈ A, IsMaxOn (fun r ↦ logLik w r x) A p →
        IsMaxOn (fun r ↦ logLik w r x) A q → p = q
```

**`UnivariateML.AE4SGED.held_nu_superunit_ae_unique`**. At a held shape above one, three distinct levels suffice for a.e. finite uniqueness on a locally closed chart. For mean or sd holds use the corresponding closed restriction of the positive-width chart. Shape at or below one is separate. [AE4SGED.lean](../../lean/UnivariateML/Family/AE4SGED.lean#L518)

```lean
theorem held_nu_superunit_ae_unique {n : ℕ} (w : Fin n → ℕ) (hw : ∀ i, 0 < w i)
    (hn : 3 ≤ n) (A : Set Parameter) (hA : IsLocallyClosed A) {shape : ℝ} (hs : 1 < shape)
    (hpar : ∀ p ∈ A, p.1 = shape ∧ 0 < p.2.2.1 ∧ 0 < p.2.2.2) :
    ∀ᵐ x : Fin n → ℝ ∂volume, Function.Injective x →
      ∀ p ∈ A, ∀ q ∈ A, IsMaxOn (fun r ↦ logLik w r x) A p →
        IsMaxOn (fun r ↦ logLik w r x) A q → p = q
```

**`UnivariateML.AE4SGED.superunit_ae_unique_with_holds`**. Four-level a.e. uniqueness among superunit SGED fits with any combination of the indicated holds. Use singleton sets for held population moments and squared skew. Mixed shape-one comparisons are separate. [AE4SGED.lean](../../lean/UnivariateML/Family/AE4SGED.lean#L589)

```lean
theorem superunit_ae_unique_with_holds {n : ℕ} (w : Fin n → ℕ) (hw : ∀ i, 0 < w i)
    (hn : 4 ≤ n) (M V K : Set ℝ) (hM : IsClosed M) (hV : IsClosed V) (hK : IsClosed K) :
    ∀ᵐ x : Fin n → ℝ ∂volume, Function.Injective x →
      ∀ p ∈ momentDomain univ M V K, ∀ q ∈ momentDomain univ M V K,
        IsMaxOn (fun r ↦ logLik w r x) (momentDomain univ M V K) p →
        IsMaxOn (fun r ↦ logLik w r x) (momentDomain univ M V K) q → p = q
```

**`UnivariateML.AE4SGED.held_nu_superunit_ae_unique_with_holds`**. Three-level a.e. uniqueness at a held superunit shape, with optional additional holds. The moment constraints are explicit. Shape one and subunit charts are separate. [AE4SGED.lean](../../lean/UnivariateML/Family/AE4SGED.lean#L600)

```lean
theorem held_nu_superunit_ae_unique_with_holds {n : ℕ} (w : Fin n → ℕ) (hw : ∀ i, 0 < w i)
    (hn : 3 ≤ n) {shape : ℝ} (hs : 1 < shape)
    (M V K : Set ℝ) (hM : IsClosed M) (hV : IsClosed V) (hK : IsClosed K) :
    ∀ᵐ x : Fin n → ℝ ∂volume, Function.Injective x →
      ∀ p ∈ momentDomain {shape} M V K, ∀ q ∈ momentDomain {shape} M V K,
        IsMaxOn (fun r ↦ logLik w r x) (momentDomain {shape} M V K) p →
        IsMaxOn (fun r ↦ logLik w r x) (momentDomain {shape} M V K) q → p = q
```

### `AE4GED`

**`UnivariateML.AE4GED.superunit_ae_unique`**. At four distinct levels GED has a.e. at most one superunit winner, with variance either held or free. Comparison of two superunit candidates only. Leaves shape-one median comparisons and exact ties open. [AE4GED.lean](../../lean/UnivariateML/Family/AE4GED.lean#L36)

```lean
theorem superunit_ae_unique {n : ℕ} (w : Fin n → ℕ) (hw : ∀ i, 0 < w i)
    (hn : 4 ≤ n) (V : Set ℝ) (hV : IsClosed V) :
    ∀ᵐ x : Fin n → ℝ ∂volume, Function.Injective x →
      ∀ p ∈ domain V, ∀ q ∈ domain V,
        IsMaxOn (fun r ↦ AE4SGED.logLik w r x) (domain V) p →
        IsMaxOn (fun r ↦ AE4SGED.logLik w r x) (domain V) q → p = q
```

### `AE4EMG`

**`UnivariateML.AE4EMG.all_free_ae_unique`**. At seven freely varying levels, an attained finite EMG fit is almost everywhere unique. Finite location, Gaussian scale and rate. The exact crossing is separate. [AE4EMG.lean](../../lean/UnivariateML/Family/AE4EMG.lean#L212)

```lean
theorem all_free_ae_unique (w : Fin n → ℕ) (hw : ∀ i, 0 < w i)
    (v : Fin kfixed → ℕ) (y : Fin kfixed → ℝ) (hn : 7 ≤ n) :
    ∀ᵐ x : Fin n → ℝ ∂volume,
      ∀ p ∈ domain (fun p : Fin 3 → ℝ ↦ p 1) (fun p ↦ p 2),
      ∀ q ∈ domain (fun p : Fin 3 → ℝ ↦ p 1) (fun p ↦ p 2),
        IsMaxOn (fun r ↦ logLik w v y (r 0) (r 1) (r 2) x)
          (domain (fun p : Fin 3 → ℝ ↦ p 1) (fun p ↦ p 2)) p →
        IsMaxOn (fun r ↦ logLik w v y (r 0) (r 1) (r 2) x)
          (domain (fun p : Fin 3 → ℝ ↦ p 1) (fun p ↦ p 2)) q → p = q
```

**`UnivariateML.AE4EMG.held_lambda_ae_unique`**. At held rate, at five freely varying levels, an attained finite EMG fit is almost everywhere unique. Stationary scale inventory is separate. [AE4EMG.lean](../../lean/UnivariateML/Family/AE4EMG.lean#L231)

```lean
theorem held_lambda_ae_unique (w : Fin n → ℕ) (hw : ∀ i, 0 < w i)
    (v : Fin kfixed → ℕ) (y : Fin kfixed → ℝ)
    (r : ℝ) (hn : 5 ≤ n) :
    ∀ᵐ x : Fin n → ℝ ∂volume,
      ∀ p ∈ (domain (fun p : Fin 2 → ℝ ↦ p 1) (fun _ ↦ r)),
      ∀ q ∈ (domain (fun p : Fin 2 → ℝ ↦ p 1) (fun _ ↦ r)),
        IsMaxOn (fun p ↦ logLik w v y (p 0) (p 1) r x)
          (domain (fun p : Fin 2 → ℝ ↦ p 1) (fun _ ↦ r)) p →
        IsMaxOn (fun p ↦ logLik w v y (p 0) (p 1) r x)
          (domain (fun p : Fin 2 → ℝ ↦ p 1) (fun _ ↦ r)) q → p = q
```

**`UnivariateML.AE4EMG.held_mu_ae_unique`**. At held location, at five freely varying levels, an attained finite EMG fit is almost everywhere unique. The constant-positive class and stationary inventory are separate. [AE4EMG.lean](../../lean/UnivariateML/Family/AE4EMG.lean#L250)

```lean
theorem held_mu_ae_unique (w : Fin n → ℕ) (hw : ∀ i, 0 < w i)
    (v : Fin kfixed → ℕ) (y : Fin kfixed → ℝ)
    (m : ℝ) (hn : 5 ≤ n) :
    ∀ᵐ x : Fin n → ℝ ∂volume,
      ∀ p ∈ (domain (fun p : Fin 2 → ℝ ↦ p 0) (fun p ↦ p 1)),
      ∀ q ∈ (domain (fun p : Fin 2 → ℝ ↦ p 0) (fun p ↦ p 1)),
        IsMaxOn (fun p ↦ logLik w v y m (p 0) (p 1) x)
          (domain (fun p : Fin 2 → ℝ ↦ p 0) (fun p ↦ p 1)) p →
        IsMaxOn (fun p ↦ logLik w v y m (p 0) (p 1) x)
          (domain (fun p : Fin 2 → ℝ ↦ p 0) (fun p ↦ p 1)) q → p = q
```

**`UnivariateML.AE4EMG.held_sigma_ae_unique`**. At held Gaussian scale, at five freely varying levels, an attained finite EMG fit is almost everywhere unique. The finite stationary rate list is separate. [AE4EMG.lean](../../lean/UnivariateML/Family/AE4EMG.lean#L269)

```lean
theorem held_sigma_ae_unique (w : Fin n → ℕ) (hw : ∀ i, 0 < w i)
    (v : Fin kfixed → ℕ) (y : Fin kfixed → ℝ)
    (s : ℝ) (hn : 5 ≤ n) :
    ∀ᵐ x : Fin n → ℝ ∂volume,
      ∀ p ∈ (domain (fun _ : Fin 2 → ℝ ↦ s) (fun p ↦ p 1)),
      ∀ q ∈ (domain (fun _ : Fin 2 → ℝ ↦ s) (fun p ↦ p 1)),
        IsMaxOn (fun p ↦ logLik w v y (p 0) s (p 1) x)
          (domain (fun _ : Fin 2 → ℝ ↦ s) (fun p ↦ p 1)) p →
        IsMaxOn (fun p ↦ logLik w v y (p 0) s (p 1) x)
          (domain (fun _ : Fin 2 → ℝ ↦ s) (fun p ↦ p 1)) q → p = q
```

**`UnivariateML.AE4EMG.held_lambda_mu_ae_unique`**. At held rate and location, at three freely varying levels, an attained finite EMG fit is almost everywhere unique. The weighted likelihood also allows any fixed observations. The nonpositive pointwise class is separate. [AE4EMG.lean](../../lean/UnivariateML/Family/AE4EMG.lean#L288)

```lean
theorem held_lambda_mu_ae_unique (w : Fin n → ℕ) (hw : ∀ i, 0 < w i)
    (v : Fin kfixed → ℕ) (y : Fin kfixed → ℝ)
    (r m : ℝ) (hn : 3 ≤ n) :
    ∀ᵐ x : Fin n → ℝ ∂volume,
      ∀ p ∈ (domain (fun p : Fin 1 → ℝ ↦ p 0) (fun _ ↦ r)),
      ∀ q ∈ (domain (fun p : Fin 1 → ℝ ↦ p 0) (fun _ ↦ r)),
        IsMaxOn (fun p ↦ logLik w v y m (p 0) r x)
          (domain (fun p : Fin 1 → ℝ ↦ p 0) (fun _ ↦ r)) p →
        IsMaxOn (fun p ↦ logLik w v y m (p 0) r x)
          (domain (fun p : Fin 1 → ℝ ↦ p 0) (fun _ ↦ r)) q → p = q
```

### `AE4SN`

**`UnivariateML.AE4SN.all_free_ae_unique`**. At seven freely varying levels, an attained finite skew-normal fit is almost everywhere unique. Finite fits only. Exact reflected sample and half-normal comparison are separate. [AE4SN.lean](../../lean/UnivariateML/Family/AE4SN.lean#L250)

```lean
theorem all_free_ae_unique (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) (hn : 7 ≤ n) :
    ∀ᵐ x : Fin n → ℝ ∂volume,
      ∀ p ∈ domain (fun p : Fin 3 → ℝ ↦ p 1), ∀ q ∈ domain (fun p : Fin 3 → ℝ ↦ p 1),
        IsMaxOn (fun r ↦ logLik w (r 0) (r 1) (r 2) x) (domain (fun p ↦ p 1)) p →
        IsMaxOn (fun r ↦ logLik w (r 0) (r 1) (r 2) x) (domain (fun p ↦ p 1)) q → p = q
```

**`UnivariateML.AE4SN.held_omega_ae_unique`**. At held positive scale and five freely varying levels, an attained finite skew-normal fit is almost everywhere unique. Finite fits only. Exact reflected sample and half-normal comparison are separate. [AE4SN.lean](../../lean/UnivariateML/Family/AE4SN.lean#L268)

```lean
theorem held_omega_ae_unique (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) (hn : 5 ≤ n)
    {s : ℝ} (hs : 0 < s) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ p q : Fin 2 → ℝ,
      IsMaxOn (fun r ↦ logLik w (r 0) s (r 1) x) univ p →
      IsMaxOn (fun r ↦ logLik w (r 0) s (r 1) x) univ q → p = q
```

### `AE4Bhatt`

**`UnivariateML.AE4Bhatt.tiltedMean_strictMono`**. At a positive residual the conditional mean of the uniform displacement is strictly increasing in positive width. Strict weighted integral comparison. [AE4Bhatt.lean](../../lean/UnivariateML/Family/AE4Bhatt.lean#L129)

```lean
theorem tiltedMean_strictMono {d s : ℝ} (hd : 0 < d) (hs : 0 < s) :
    StrictMonoOn (tiltedMean d s) (Ioi 0)
```

**`UnivariateML.AE4Bhatt.spatialScore_strictMono`**. At a positive residual the spatial score is strictly increasing in positive width. Pointwise positive-width comparison. [AE4Bhatt.lean](../../lean/UnivariateML/Family/AE4Bhatt.lean#L193)

```lean
theorem spatialScore_strictMono {d s : ℝ} (hd : 0 < d) (hs : 0 < s) :
    StrictMonoOn (spatialScore d s) (Ioi 0)
```

**`UnivariateML.AE4Bhatt.held_mu_sigma_ae_unique`**. At held location and Gaussian scale the finite width is a.e. unique on nonempty freely varying charts, including width zero. Fixed residual atoms, the normal threshold and two-width witness are separate. [AE4Bhatt.lean](../../lean/UnivariateML/Family/AE4Bhatt.lean#L365)

```lean
theorem held_mu_sigma_ae_unique {n : ℕ} (w : Fin n → ℕ) (hw : ∀ i, 0 < w i)
    (hn : 0 < n) (m : ℝ) {s : ℝ} (hs : 0 < s) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ a ∈ Ici 0, ∀ b ∈ Ici 0,
      IsMaxOn (fun r ↦ logLik w m s r x) (Ici 0) a →
      IsMaxOn (fun r ↦ logLik w m s r x) (Ici 0) b → a = b
```

**`UnivariateML.AE4Bhatt.held_mu_sigma_all_center_unique`**. At a held positive Gaussian scale with every observation at the held center, the normal is the unique width. Exact on the all-center stratum. [AE4Bhatt.lean](../../lean/UnivariateML/Family/AE4Bhatt.lean#L432)

```lean
theorem held_mu_sigma_all_center_unique {n : ℕ} (w : Fin n → ℕ)
    (hw : ∀ i, 0 < w i) (hn : 0 < n) (m : ℝ) {s : ℝ} (hs : 0 < s)
    (x : Fin n → ℝ) (hx : ∀ i, x i = m) :
    ∃! a : ℝ, a ∈ Ici 0 ∧ IsMaxOn (fun r ↦ logLik w m s r x) (Ici 0) a
```

### `AE4Huber`

**`UnivariateML.AE4Huber.parameters_eq_of_scores`**. Two equal Huber score vectors with a tail in each candidate and two distinct strict core levels identify all three coordinates. Conditional on the core and tail hypotheses. [AE4Huber.lean](../../lean/UnivariateML/Family/AE4Huber.lean#L86)

```lean
theorem parameters_eq_of_scores {n : ℕ} {x : Fin n → ℝ} {m B K m' B' K' : ℝ}
    (hB : 0 < B) (hK : 0 < K) (hK' : 0 < K')
    (ht : ∃ i, K ≤ |B * (m - x i)|) (ht' : ∃ i, K' ≤ |B'*(m'-x i)|)
    {i j : Fin n} (hij : x i ≠ x j)
    (hi : |B * (m - x i)| < K) (hj : |B * (m - x j)| < K)
    (he : ∀ i, score m B K (x i) = score m' B' K' (x i)) :
    m = m' ∧ B = B' ∧ K = K'
```

**`UnivariateML.AE4Huber.held_mu_parameters_eq_of_scores`**. At held center, equal Huber score vectors with a tail in each candidate and one off-center strict core level identify both parameters. Conditional on the core and tail hypotheses. [AE4Huber.lean](../../lean/UnivariateML/Family/AE4Huber.lean#L108)

```lean
theorem held_mu_parameters_eq_of_scores {n : ℕ} {x : Fin n → ℝ} {m B K B' K' : ℝ}
    (hK : 0 < K) (hK' : 0 < K')
    (ht : ∃ i, K ≤ |B * (m - x i)|) (ht' : ∃ i, K' ≤ |B'*(m - x i)|)
    {i : Fin n} (hi : x i ≠ m) (hc : |B * (m - x i)| < K)
    (he : ∀ j, score m B K (x j) = score m B' K' (x j)) :
    B = B' ∧ K = K'
```

**`UnivariateML.AE4Huber.pathReciprocal_strictMono`**. The reciprocal factor in the constant-tail-slope path strictly increases in positive cutoff. Normalizer curvature step. Path admissibility and optimality exclusion are separate. [AE4Huber.lean](../../lean/UnivariateML/Family/AE4Huber.lean#L151)

```lean
theorem pathReciprocal_strictMono : StrictMonoOn pathReciprocal (Ioi 0)
```

### `AE4TukeyLambda`

**`UnivariateML.AE4TukeyLambda.quantile_analytic`**. At a fixed nonzero shape the Tukey lambda quantile is analytic at interior probabilities. Interior chart prerequisite. [AE4TukeyLambda.lean](../../lean/UnivariateML/Family/AE4TukeyLambda.lean#L42)

```lean
theorem quantile_analytic {lam p : ℝ} (hl : lam ≠ 0) (hp : p ∈ Ioo (0 : ℝ) 1) :
    AnalyticAt ℝ (quantile lam) p
```

**`UnivariateML.AE4TukeyLambda.exists_analytic_inverse`**. At a fixed nonzero shape the CDF has a local analytic inverse-quantile chart at every interior probability. Interior only. Moving contacts and shape-zero continuation are separate. [AE4TukeyLambda.lean](../../lean/UnivariateML/Family/AE4TukeyLambda.lean#L63)

```lean
theorem exists_analytic_inverse {lam p : ℝ} (hl : lam ≠ 0) (hp : p ∈ Ioo (0 : ℝ) 1) :
    ∃ G : ℝ → ℝ, AnalyticAt ℝ G (quantile lam p) ∧ G (quantile lam p) = p ∧
      (∀ᶠ z in 𝓝 (quantile lam p), quantile lam (G z) = z)
```

**`UnivariateML.AE4TukeyLambda.quantileExtended_analytic`**. The shape-zero quantile continuation is jointly real analytic in shape and interior probability. The parameter-dependent inverse and contacts are separate. [AE4TukeyLambda.lean](../../lean/UnivariateML/Family/AE4TukeyLambda.lean#L121)

```lean
theorem quantileExtended_analytic (lam p : ℝ) (hp : p ∈ Ioo (0 : ℝ) 1) :
    AnalyticAt ℝ (fun z : ℝ × ℝ ↦ quantileExtended z.1 z.2) (lam,p)
```

## Verification

Direct elaboration used the workstation one-slot lock and the pinned Lean toolchain. I temporarily added the AE4 imports to the aggregate module and ran `lake build` through the same lock with `LEAN_NUM_THREADS=1`. The final aggregate build exited successfully and reported 8,378 jobs, with linter warnings. I restored the aggregate source file and checked that its Git diff is empty. The checked AE4 declarations contain no admitted goals.
