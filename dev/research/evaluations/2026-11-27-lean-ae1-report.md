# TASK-437 · Almost-everywhere likelihood uniqueness in Lean

Worktree `codex/lean-ae1` at base `3273608a`, 2026-11-27.

## Checked scope

The new Lean theorems establish the finite-winner uniqueness conclusion for
six held-shape two-piece Student regimes, two held-mean GED regimes, two Huber
regimes and EMG with location and scale held. Two SGED theorems cover the unit-skew
subcases with held mean. A Tukey-lambda theorem gives the full location set on
the two held uniform branches. The atlas supplement grades these rows against
the complete atlas statements, including their exact examples and ancillary
candidate-set claims. [Atlas supplement](../existence-atlas/lean-map.d/lean-ae1.csv)

Each weighted likelihood uses a vector of positive integer counts fixed before
taking the Lebesgue almost-everywhere quantifier. The coordinates are the free
level values. In the proofs that require distinctness, restricting to any
ordered level chart discharges the injectivity premise. The conclusions compare
two putative global finite winners and therefore also apply on samples where
attainment is established independently. Pinned level values would require
their own chart.

- **GED.** Away from a level equal to the held mean, multiplying the spatial
  score by the residual and taking logarithms gives an affine equation in the
  log residual radius. Two unequal radii identify both coefficients. The
  exceptional equalities are translates of proper linear subspaces. The domain
  includes shape one by using the closed shape subtype as the parameter space.
  The held-sd result uses kernel width `sd / Ged.sdFactor a`.
  [Lean module](../../lean/UnivariateML/Family/AE1GED.lean)
- **SGED at unit skew.** The half-width fraction is one half, and the held mean
  equals the join. The change of scale from GED kernel width $b$ to SGED inverse
  total width $1/(2b)$ preserves the weighted likelihood and is an involution.
  This proves the unit-skew finite comparison at two free levels.
  [Lean module](../../lean/UnivariateML/Family/AE1SGED.lean)
- **Two-piece Student at fixed shape.** Squared positive parts give a derivative
  at the fitted join as well as off it. Distinct joins give at most one
  score intersection on either exterior. With a common join, a strict
  occupied side determines its width, and the equal value determines an
  unoccupied width through the log peak. The public moment chart uses the
  denominator widths $a,b$, so the ordinary total width is
  $(a+b)/\sqrt{\nu}$. Fixing the mean, squared sd or squared skew selects a
  closed level set inside the open positive-width chart. The additive
  shape-only log-peak constant cancels at held shape.
  [Lean module](../../lean/UnivariateML/Family/AE1SSTD.lean)
- **Huber.** The clipped score is continuous at both joins. At a finite
  winning cutoff the cutoff derivative forces a strict tail. At held mean
  and scale that tail rules out a larger cutoff with the same score. At
  held scale and distinct cutoffs, all shared score values must lie in the
  larger cutoff's core. A shared interior value of the smaller clip forces
  equal centers, and a strict tail then separates. In the remaining case all
  common scores are one of two constants, which the larger clip can reach
  at at most two levels. The location may vary within a winning cutoff class.
  [Lean module](../../lean/UnivariateML/Family/AE1Huber.lean)
- **EMG at held location and scale.** The inverse Mills-ratio derivative
  exceeds minus one. The Lean proof uses monotonicity and the Gaussian
  minus-infinity limit for two auxiliary expressions in `φ` and `Φ`, in
  place of evaluating the truncated Gaussian variance. The finite spatial
  score therefore strictly decreases with rate at every observation.
  [Lean module](../../lean/UnivariateML/Family/AE1EMG.lean)
- **Tukey lambda at held uniform shape and scale.** At shapes one and two
  the observation weights on the closed quantile graph are constant. The
  maximum location set is the intersection of the closed intervals
  $[x_j-s/\lambda,x_j+s/\lambda]$, including the empty and singleton cases.
  [Lean module](../../lean/UnivariateML/Family/AE1TukeyLambda.lean)
- **Analytic incidence.** A separate checked theorem covers the joint null
  projection step once the equal-gradient locus has been partitioned into
  countably many smooth systems with invertible sample Jacobian. It uses
  Mathlib's implicit function theorem, a Lindelöf subcover on each stratum
  and Hausdorff dimension of differentiable images. The hypotheses still
  need verification for the prospective analytic family pairs. This shared
  theorem has no family row in the atlas supplement.
  [Lean module](../../lean/UnivariateML/Generic/AE1Incidence.lean)

## Effort and remaining obligations by target

Each row records the part carried through and the specific remaining proof.
The analytic checks use the Mathlib lemmas
`AnalyticOnNhd.preimage_zero_mem_codiscreteWithin` for scalar zero sets and
`volume_preserving_piFinSuccAbove` for a potential one-coordinate Fubini
argument. A scalar null set for each *fixed* parameter pair alone would leave
an uncountable union over pairs. This is the reason to use the joint
multiplicity-stratum argument for the analytic families. The new implicit
incidence theorem supplies the projection step when its Jacobian hypotheses
are available. [Score incidence source](2026-11-27-uniq-locscale-math-report.md#analytic-incidence-bound)

### Two-piece Student

For the free-shape rows I checked the algebraic exterior bound in scratch
Lean. At two positive roots $r,s$, subtracting the two cubic equations after
multiplying by the other radius removes the linear coefficient. When the
farther shape is no greater than the nearer one, the remaining factor after
$r-s$ is strictly negative. When the farther shape is larger, a third
positive root $t$ would force
$(s-t)(A r s t+D)=0$ with positive leading and constant coefficients $A,D$.
Both exclusions kernel-check for exact reals. The four-level chart assembly
and the variable-shape normalizer are separate from these polynomial checks.
[Common score computation](2026-11-27-uniq-skew2-math-report.md#skew-student-separation)

| Held | Effort and remaining proof |
| --- | --- |
| all free | Checked the one- and two-root exterior exclusions. Open are the free-shape chart assembly, the equal-count three-level stationary reduction, its no-contact digamma inequality, and one- and two-level nonattainment. The six-level example requires a formal strict comparison with the reflection-fixed family and the boundary maxima. |
| mean | Checked the same exterior calculation. Open are continuity on the varying-shape mean constraint and the finite sample-score assembly. The six-level global height exclusion with mean zero remains open. |
| mean+nu | Checked three-level finite uniqueness on the fixed mean chart. Open are the semialgebraic stationary-component comparison and the strict bound for the reflected shape-31 example. |
| mean+nu+sd | Checked three-level finite uniqueness on the two moment equalities. Open are the scalar stationary-component and forced half-contact comparisons, and the reflection witness beating both limits at sd $9/8$. |
| mean+nu+xi | Checked three-level finite uniqueness on the fixed mean and skew chart. The existing unweighted `TwoPiece2Student.heldMeanShapeSkew` and `heldMeanShapeSkew_outward` give the root and collapse prescriptions in their own coordinates. Their weighted transport is open. |
| mean+sd | Checked the exterior algebra. Open are the two-moment free-shape domain, including its positive variance factor, and the common-join variable-shape separation. The exact reflected finite pair is open. |
| mean+sd+xi | At unit skew the common-join equations are linear in squared radius after cross multiplication. Open are the variable-shape chart class conclusion at unit skew and the moving join under asymmetric skew. |
| mean+xi | Reduces to the same common-join equations at unit skew. Open are the chart derivative and finite-winner conclusion, including the coupling of free scale to the join at asymmetric skew. |
| nu | Checked three-level finite uniqueness with both widths and the join free. Open are the stationary-component and exact shape-31 reflected comparison. |
| nu+sd | Checked three-level finite uniqueness under the variance equality. Open are the stationary-component profile, half-end comparison and six-level reflected example. |
| nu+xi | Checked three-level finite uniqueness under the width-ratio equality. The ordinary Student held-df result exists in `StudentGeodesic.std_held_df_unique`. Its transfer to the unit-skew exact pointwise claim and the general component inventory remain open. |
| sd | Checked the exterior algebra. Open are the fitted variable-shape variance slice and four-level score separation on it, and the six-level reflected example at held sd. |
| sd+xi | The exterior count addresses the intended four-level generic argument. Open are the two-hold chart and the exact shape-dependent two-observation denominator minimization plus the strict centered height bound needed for the reflected winners. |
| xi | Checked the exterior algebra. Open are the free-shape, fixed-width-ratio chart and the finite-winner theorem. |

Existing exact fixed-mean, shape and skew results are in
[TwoPiece2Student](../../lean/UnivariateML/Family/TwoPiece2Student.lean).
The all-free special-level argument is in the
[generic mathematics report](2026-11-27-uniq-generic-math-report.md#pilot-2-skew-student-all-free).

### SGED and GED

On a common exterior, the derivative of the log score ratio has numerator
$u d+(u-v)r$, where $u,v$ are the two powers minus one. I reduced its
root count to a monotone branch and at most one turning point. A second
application of Rolle is needed for three sample levels on the same exterior.
The subunit case cannot use the free-join sample derivative at a knot. At
power one the two one-sided derivatives at the join are different. Both
require a contact chart with the joined observation composed into the
parameter and, where appropriate, comparisons of full median classes.
[Power and contact calculations](2026-11-27-uniq-skew2-math-report.md#sged-and-ged-separation)

| Family | Held | Effort and remaining proof |
| --- | --- | --- |
| SGED | all free | Reduced smooth separation to Rolle on the two exteriors. Open are its Lean count and the joint power-one comparison, including exclusion of a winning balanced gap by the skew derivative. The reflected witness needs a strict global bound. |
| SGED | mean | At a Laplace contact the mean equation couples the join to total width and skew. A superunit-vs-Laplace comparison needs the moving-contact chart with its noncontact scores. The mean-zero reflected comparison is open. |
| SGED | mean+nu | The common exterior log derivative is monotone at equal nonunit power. Open below one are the two contact charts that preserve the held mean and the five-level count after removing contacts. The above-one chart and exact shape-one reflected comparison also remain open. |
| SGED | mean+sd | The contact equation must preserve both moments. Open are its smooth local solution in skew, the superunit-vs-contact gradient comparison, and the reflected example at the specified sd. |
| SGED | mean+sd+xi | Checked the two-level unit-skew theorem. Off unit skew the join moves with shape through the variance constraint. Open are the four-level comparison and the exact global crossing at $a_*,b_0$. |
| SGED | mean+xi | Checked the two-level unit-skew theorem. Open off unit skew are the four-level mixed-shape comparison and the Laplace contact parameterization through total width. The large-multiplicity $h_*$ crossing is open. |
| SGED | nu | Above one, the equal-power exterior reduction applies. Below one, the profiled knot loss requires showing its analytic difference is nonzero along a third-level interval using the unbounded one-sided derivative. Open are this scalar analytic/Fubini proof, the power-one comparison, and the reflected finite example. |
| SGED | nu+sd | Equal-power comparisons still need the two moving-knot charts below one. Open at power one are exclusion of balanced optimal gaps and the resulting knot chart. The exact reflected example with the specified sd is open. |
| SGED | sd | Open are the four-level mixed-power comparison with the variance relation and moving Laplace knot. The exact witness requires separating the held-variance reflection-fixed and half-end optima. |
| SGED | sd+xi | At power one the balancing equation determines the candidate location interval at fixed width. Open are comparison of the resulting class with a superunit winner, and a strict small-sd bound proving the open plateaus and the exact ((-1,1)) witness. |
| SGED | xi | The power-one scale profile is constant across a balanced gap. Open are the moving median chart and mixed-power separation, plus the strict global shape-one comparison at the eight-observation witness and its neighborhood. |
| GED | all free | A sample coordinate equal to a median location makes the power-one fixed-parameter derivative invalid. Open are the profiled median chart, its comparison against the superunit chart, and the strict uniform and superunit height bounds at the eight-observation sample. |
| GED | mean | Checked generic finite uniqueness. Open are the stationary-root and centered-uniform comparisons and the $h_*$ global crossing. |
| GED | mean+sd | Checked generic finite uniqueness. The real crossing $a_*,b_0$ still needs its two optimized heights and exclusion of the other shapes and uniform contact. |
| GED | sd | The median piece needs the same kink handling as all free. The exact pair at sd $1/10$ and the open plateau class require the strict power-one height bound across the whole fitted shape range. |

### Location-scale families

| Family | Held | Effort and remaining proof |
| --- | --- | --- |
| EMG | all free | Identified the six pair coordinates for the seven-level incidence proof. Open are real analyticity with parameters, nonidentity via cumulants and the spatial-score normalization argument, and the two global winners at the moment-adjusted crossing. |
| EMG | lambda | The pair dimension drops to four. Open are the analytic multiplicity strata and finite stationary scales for the location-profiled objective. The conditional location derivative alone gives no scale-root count. |
| EMG | lambda+mu | The pair dimension drops to two. Open are nonidentity of the two scale scores with held rate and location, their three-level incidence, and the log-scale concavity and stationary-scale comparison on the respective residual classes. |
| EMG | mu | The free shape/scale pair incidence has dimension four. Open are its analytic charts and the eliminated-score proof for the constant-positive class, plus the general finite stationary-shape list. |
| EMG | mu+sigma | Checked strict rate ordering of the spatial scores and generic uniqueness with any positive number of freely varying levels. Open are pinned-coordinate chart transport, the pointwise Laplace-transform single crossing for constant positive residuals, and the stationary-rate inventory. |
| EMG | sigma | The location–rate pairs have dimension four. Open are analytic parameter separation and the proof of finitely many stationary rates after solving conditional location. |
| Tukey lambda | all free | Split the chart into smooth interior and tight uniform. Open are the variable-shape inverse-quantile regularity at zero, seven-level incidence and comparison with the optimized uniform. Balanced two-level dominance and majority-atom divergence are separate. |
| Tukey lambda | lambda | Fixed shape leaves four nuisance pair coordinates before contacts. Open are the pair-dimension reduction at closed contacts, the pointwise singleton proof through $8/3$, and the strict off-center witness at shape five. |
| Tukey lambda | lambda+sigma | Checked the exact uniform branches. For shape $-1$ the scratch Lean check verified $Q(9/10)=80/9$ and $2(Q'')^2-Q'''Q'>0$ at that quantile using rational arithmetic. Transfer to the log likelihood and proof of distinct global winners remain open. So do nonuniform three-level separation and the strict endpoint optimum at shape $3/2$. |
| Tukey lambda | mu | The uniform uses the active absolute radius. Open are the interior inverse-quantile incidence and uniform comparison, including the exact $t_*$ crossing and different gradients there. |
| Tukey lambda | sigma | The fixed shape-one support set is checked. Open are strict dominance of that feasible uniform over the full free-shape branch, five-level incidence outside uniform feasibility, and the exact wide-span reflected winners. |
| Bhattacharjee | all free | The included normal needs either the width-zero continuation of the scaled integral or a separate chart. Open are smoothness and identifiability for six pair coordinates, seven-level incidence, and the normal/positive-width global crossing at $t_N$. |
| Bhattacharjee | mu | Holding center leaves four pair coordinates. Open are the integral-chart regularity and five-level incidence at the normal seam, and transfer of the exact symmetric crossing through conditional-center optimality. |
| Bhattacharjee | mu+sigma | Reduced score separation to strict ordering of the truncated weighted averages of $v\tanh(dv/\sigma^2)$. Splitting the integrals at the smaller width gives the core inequality. Open are its strict integral comparison and sample differentiation at all widths, then the moment criterion and the two-width $d_*$ construction. |
| Bhattacharjee | sigma | The center–width pair chart has four coordinates. Open are the five-level incidence including width zero, the centered second-moment comparison, and the proof that the two $d_*$ widths win at center zero even when center is free. |
| Huber | all free | The clipped derivative and strict-tail argument are checked at fixed scale. Open for free scale are the path improvement after preserving the gradient of a sole strict core level, and the resulting four-level joint separation and shape inventory. |
| Huber | mu | Strict-tail detection works at each scale. Open are exclusion of no nonzero strict core at an attained fit, scale identification at a common cutoff-to-scale ratio, and the one-magnitude nonattainment and stationary-shape statements. |
| Huber | mu+sigma | Checked generic uniqueness. The existing `LocScaleHuber.held_location_scale_finite_critical` and `held_location_scale_equal_magnitude_unique` cover the unweighted statements. Transport of those results to weighted and pinned charts is open. |
| Huber | sigma | Checked the generic winning cutoff. Existing `Huber.heldScale_interval` gives the conditional interval, and `LocScaleHuber.separated_pair_full_maximizers` checks the exact ((-10,10)) example. The perturbation inequality producing an open set at every even size and weighted interval transport remain open. |
| Skew normal | all free | Pair dimension six. Open are the real analytic Gaussian CDF chart and score nonidentity from the favorable and unfavorable tail coefficients, then seven-level incidence and the strict finite-versus-half witness on the seven-observation sample. |
| Skew normal | omega | Pair dimension four. Open are analytic five-level separation and a strict comparison with the moving half-normal traces at the specified reflected sample and held scale. |

Tukey source proofs use the [support and contact charts](2026-11-27-uniq-locscale-math-report.md#tukey-lambda-support-charts).
The two inherited Huber interval proofs are in [Huber](../../lean/UnivariateML/Family/Huber.lean)
and [LocScaleHuber](../../lean/UnivariateML/Family/LocScaleHuber.lean).

## Source scope

No contradiction in a target statement was established. The ordinary
positive-width and smooth-core comparisons give statements about finite
winners. Applications at support contacts, fixed atoms, profiled medians and
finite-width seams require their corresponding chart hypotheses. In
particular, a nonzero derivative in a smooth chart cannot be assigned to a
fixed-parameter Laplace join by choosing one side. The source addresses such
candidates with composed charts. [Generic source](2026-11-27-uniq-generic-math-report.md#kinks-and-support-contacts)

## Lean declarations

### `UnivariateML.AE1GED`

```lean
theorem held_mean_ae_unique (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) (m : ℝ)
    (hn : 2 ≤ n) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ p q : ℝ × ℝ,
      p ∈ Ici 1 ×ˢ Ioi 0 → q ∈ Ici 1 ×ˢ Ioi 0 →
      IsMaxOn (fun r ↦ logLik w m r x) (Ici 1 ×ˢ Ioi 0) p →
      IsMaxOn (fun r ↦ logLik w m r x) (Ici 1 ×ˢ Ioi 0) q → p = q
```

```lean
theorem held_mean_sd_ae_unique (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) (m sd : ℝ)
    (hsd : 0 < sd) (hn : 2 ≤ n) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ p q : ℝ, p ∈ Ici 1 → q ∈ Ici 1 →
      IsMaxOn (fun a ↦ heldSdLogLik w m sd a x) (Ici 1) p →
      IsMaxOn (fun a ↦ heldSdLogLik w m sd a x) (Ici 1) q → p = q
```


### `UnivariateML.AE1SGED`

```lean
theorem held_mean_unit_skew_ae_unique (w : Fin n → ℕ) (hw : ∀ i, 0 < w i)
    (m : ℝ) (hn : 2 ≤ n) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ p ∈ Ici 1 ×ˢ Ioi 0, ∀ q ∈ Ici 1 ×ˢ Ioi 0,
      IsMaxOn (fun r ↦ unitSkewLogLik w m r x) (Ici 1 ×ˢ Ioi 0) p →
      IsMaxOn (fun r ↦ unitSkewLogLik w m r x) (Ici 1 ×ˢ Ioi 0) q → p = q
```

```lean
theorem held_mean_sd_unit_skew_ae_unique (w : Fin n → ℕ) (hw : ∀ i, 0 < w i)
    (m sd : ℝ) (hsd : 0 < sd) (hn : 2 ≤ n) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ p q : ℝ, p ∈ Ici 1 → q ∈ Ici 1 →
      IsMaxOn (fun a ↦ unitSkewHeldSdLogLik w m sd a x) (Ici 1) p →
      IsMaxOn (fun a ↦ unitSkewHeldSdLogLik w m sd a x) (Ici 1) q → p = q
```


### `UnivariateML.AE1SSTD`

```lean
theorem held_nu_ae_unique (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) {ν : ℝ} (hν : 2 < ν)
    (hn : 3 ≤ n) (A : Set (ℝ × ℝ × ℝ)) (hA : IsLocallyClosed A)
    (hwidth : ∀ p ∈ A, 0 < p.2.1 ∧ 0 < p.2.2) :
    ∀ᵐ x : Fin n → ℝ ∂volume, Function.Injective x →
      ∀ p ∈ A, ∀ q ∈ A, IsMaxOn (fun r ↦ fixedShapeLogLik w ν r x) A p →
        IsMaxOn (fun r ↦ fixedShapeLogLik w ν r x) A q → p = q
```

```lean
theorem held_nu_with_holds_ae_unique (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) {ν : ℝ} (hν : 2 < ν)
    (hn : 3 ≤ n) (M V K : Set ℝ) (hM : IsClosed M) (hV : IsClosed V)
    (hK : IsClosed K) :
    ∀ᵐ x : Fin n → ℝ ∂volume, Function.Injective x →
      ∀ p ∈ heldNuDomain ν M V K, ∀ q ∈ heldNuDomain ν M V K,
        IsMaxOn (fun r ↦ fixedShapeLogLik w ν r x) (heldNuDomain ν M V K) p →
        IsMaxOn (fun r ↦ fixedShapeLogLik w ν r x) (heldNuDomain ν M V K) q → p = q
```


### `UnivariateML.AE1Huber`

```lean
theorem held_mu_sigma_ae_unique (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) (hn : 0 < n)
    (m : ℝ) {s : ℝ} (hs : 0 < s) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ c d : ℝ, 0 < c → 0 < d →
      IsMaxOn (fun t ↦ logLik w m s t x) (Ioi 0) c →
      IsMaxOn (fun t ↦ logLik w m s t x) (Ioi 0) d → c = d
```

```lean
theorem held_sigma_ae_unique_cutoff (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) (hn : 3 ≤ n)
    {s : ℝ} (hs : 0 < s) :
    ∀ᵐ x : Fin n → ℝ ∂volume, Function.Injective x →
      ∀ p q : ℝ × ℝ, scaleWinner w s x p → scaleWinner w s x q → p.1 = q.1
```


### `UnivariateML.AE1EMG`

```lean
theorem held_mu_sigma_ae_unique (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) (hn : 0 < n)
    (m : ℝ) {s : ℝ} (hs : 0 < s) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ p ∈ Ioi 0, ∀ q ∈ Ioi 0,
      IsMaxOn (fun r ↦ logLik w m s r x) (Ioi 0) p →
      IsMaxOn (fun r ↦ logLik w m s r x) (Ioi 0) q → p = q
```


### `UnivariateML.AE1TukeyLambda`

```lean
theorem held_uniform_shape_scale_locations {n : ℕ} (x : Fin n → ℝ)
    {lam s : ℝ} (hlam : lam = 1 ∨ lam = 2) (hs : 0 < s) (m : ℝ) :
    (∃ p : Fin n → ℝ, (m, p) ∈ heldScaleGraph x lam s ∧
      IsMaxOn (graphLikelihood lam s) (heldScaleGraph x lam s) (m, p)) ↔
      ∀ j, m ∈ Icc (x j - s / lam) (x j + s / lam)
```


### `UnivariateML.AE1Incidence`

```lean
theorem ae_no_implicit_incidence {d n : ℕ} (hdn : d < n) {ι : Type*} [Countable ι]
    (S : ι → Set ((Fin d → ℝ) × (Fin n → ℝ)))
    (F : ι → ((Fin d → ℝ) × (Fin n → ℝ)) → (Fin n → ℝ))
    (hzero : ∀ i, ∀ z ∈ S i, F i z = 0)
    (hsmooth : ∀ i, ∀ z ∈ S i, ContDiffAt ℝ 1 (F i) z)
    (hinv : ∀ i, ∀ z ∈ S i,
      (fderiv ℝ (F i) z ∘L ContinuousLinearMap.inr ℝ (Fin d → ℝ) (Fin n → ℝ)).IsInvertible) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ i p, (p, x) ∉ S i
```

## Verification

- All seven added modules passed individual checks with
  `/tmp/umvl-cpu/run env LEAN_NUM_THREADS=1 nice -n 10 /home/jonas/.elan/bin/lake env lean <file>`
  on the workstation.
- The final workstation `lake build` passed with the seven temporary imports
  and reported `Build completed successfully (8340 jobs).` The build emitted
  existing library warnings and two new style warnings, one for a long
  incidence proof line and one for a goal-changing `show` in the SGED
  reparameterization.
- The temporary imports were removed and
  `git diff --exit-code -- dev/lean/UnivariateML.lean` passed.
