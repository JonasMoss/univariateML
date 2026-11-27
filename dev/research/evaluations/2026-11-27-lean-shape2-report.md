# TASK-423 positive-support shape likelihoods in Lean

The worktree adds 57 checked public theorems in 12 Lean modules. They include
the exact exponentiated Weibull held-scale criterion and finite maximizing
set, fatigue divergence with tied minima, the generalized-gamma fixed-shape
masks, transformed-gamma contact traces and strict finite witnesses, the GEV
held-minimum half-tie comparison, and exact fixed-power Burr and inverse Burr
ties. The open entries below give the remaining mathematical or formalization
obligations in the requested scope. [Atlas fragment](../existence-atlas/lean-map.d/lean-shape2.csv)

## Source clarifications

- The fatigue dossier highlights divergence with a unique minimum. Its
  threshold path gives a divergent contribution at every tied minimum.
  The Lean result proves unboundedness for every nonempty sample, including
  constant samples. [Fatigue dossier](../../../univariateML-cpp/include/cppunivariate/distributions/fatigue/README.md#existence)
- The short transformed-gamma uniqueness claim at zero mean oriented log
  excess needs the written condition that some residual is nonzero. If
  every observation equals scale, the new concentration theorem gives
  divergence. The reciprocal result has the same qualification. The checked
  Pareto trace also confirms one unit of log-likelihood loss per contact
  relative to the ordinary closed-endpoint density, as noted in the
  source's diagnostic discussion. [Transformed-gamma report](2026-11-25-trgamma-math-report.md#held-scale-and-both-shapes-free)
- The literal fixed-endpoint Burr and inverse Burr power/Pareto paths carry
  a density factor for every contact. The optimized all-free face is
  reached with a moving endpoint. In the GEV all-free shape-one limit a
  finite feasible support endpoint likewise lies strictly below the minimum
  and approaches it. [Prior Burr path proofs](../../lean/UnivariateML/Family/JoinsBurr.lean)
  [Prior GEV scope clarification](2026-11-27-lean-allfree2-report.md#source-clarifications)
- The Burr held-power theorem proves the existential tie for every positive
  held power using the 21-point Lomax crossing. The written Burr report
  uses a different four-point crossing. At inverse Burr power one the new
  theorem uses exactly the reciprocal 21-point sample specified in the atlas.
  [Burr example](2026-11-25-burr-math-report.md#held-power-has-an-exact-global-tie)
  [Inverse Burr example](2026-11-27-uniq-atlas-math-report.md#invburr-held-shape2)

## Checks

The workstation ran the individual modules through `/tmp/umvl-cpu/run`
using `nice -n 10 /home/jonas/.elan/bin/lake env lean`. The integrated
`lake build` used the same single slot and temporary public imports for
the 12 modules. The root file was restored and
`git diff --exit-code dev/lean/UnivariateML.lean` passed.

```text
Build completed successfully (8002 jobs).
```

The final build used cached Mathlib and existing project dependencies.
The Lean proof sources contain no placeholder declarations. The atlas
fragment has 60 rows because three declarations apply to two masks.
Coverage remains blank for independent review.

## Main questions

The entries account for all 139 questions in the task table. Checked
entries use the theorems below or the explicitly identified prior Lean
results. Each named gap describes the additional proof work for that
scope.

| Family | Held | Attainment | Failure | Uniqueness or maximizing set |
| --- | --- | --- | --- | --- |
| `expweibull` | `all free` | Open, All-free face exhaustion | Constant path checked, Open, All-free face exhaustion | Open, All-free exponentiated Weibull tie |
| `expweibull` | `exponent` | Checked | Checked | — |
| `expweibull` | `exponent+scale` | Checked | Checked | Checked |
| `expweibull` | `exponent+shape` | Checked | Checked | Checked |
| `expweibull` | `scale` | Checked | Checked, including every contact | Checked maximizing set |
| `expweibull` | `shape` | Checked | Checked | Open, Held-shape curvature |
| `pgw` | `all free` | Open, All-free PGW faces and stationary list | Open, All-free PGW faces and stationary list | Open, All-free PGW faces and stationary list |
| `pgw` | `exponent` | Checked | — | — |
| `pgw` | `exponent+scale` | Checked | — | — |
| `pgw` | `scale` | — | Checked with existing threshold comparison | Open, PGW held-scale roots |
| `pgw` | `shape` | Stationary-witness iff in existing Lean, Open, PGW held-shape finite retention | Checked on the stated narrow branch | Open, PGW held-shape finite retention |
| `fatigue` | `all free` | Checked | Checked, tied minima included | Checked vacuity |
| `fatigue` | `alpha` | Open, Fatigue held-shape joint escape | Constant branch checked, Open, Fatigue held-shape joint escape | Open, Fatigue stationary components |
| `fatigue` | `alpha+beta` | Checked | Checked | Open, Fatigue held-shape-and-scale uniqueness |
| `fatigue` | `alpha+mu` | Checked | Checked | Open, Fatigue reciprocal scale pair |
| `fatigue` | `beta` | Open, Fatigue held-scale rational profile | Open, Fatigue held-scale rational profile | Open, Fatigue held-scale rational profile |
| `gengamma` | `all free` | — | — | Open, Generalized-gamma reflected all-free maxima |
| `gengamma` | `Q` | Checked | Checked | Checked |
| `gengamma` | `Q+mu` | Checked | Checked | Checked |
| `gengamma` | `Q+sigma` | Checked | Checked finite attainment | Checked |
| `gengamma` | `mu` | Checked at the stated fixed-Q scope | — | Open, Generalized-gamma held-location signed lists |
| `gengamma` | `mu+sigma` | Open, Generalized-gamma free shape at held scale | Open, Generalized-gamma free shape at held scale | Open, Generalized-gamma free shape at held scale |
| `gengamma` | `sigma` | Open, Generalized-gamma free shape at held scale | Open, Generalized-gamma free shape at held scale | Open, Generalized-gamma reflected held-scale maxima |
| `trgamma` | `all free` | Open, All-free transformed-gamma profile | Paths in existing Lean, Open, All-free transformed-gamma profile | Open, Transformed-gamma finite-power equality |
| `trgamma` | `scale` | Concentration branch checked, Open, Transformed-gamma held-scale compactness | Finite trace comparison checked on its support, Open, Transformed-gamma held-scale compactness | Checked with the written nonzero-residual condition |
| `invtrgamma` | `all free` | Open, All-free transformed-gamma profile | Paths in existing Lean, Open, All-free transformed-gamma profile | Open, Inverse transformed-gamma finite pair |
| `invtrgamma` | `scale` | Concentration branch checked, Open, Transformed-gamma held-scale compactness | Finite trace comparison checked on its support, Open, Transformed-gamma held-scale compactness | Checked with the written nonzero-residual condition |
| `betapr` | `all free` | Open, Beta-prime compact ratio comparison | Constant divergence in existing Lean, Open, Beta-prime compact ratio comparison | Open, Beta-prime exact four-way equality |
| `betapr` | `shape1` | Constant branch checked, Open, Beta-prime nonconstant single-held shape | Constant face checked, Open, Beta-prime nonconstant single-held shape | Open, Beta-prime conditional tie |
| `betapr` | `shape1+shape2` | Checked | Checked finite attainment | Checked |
| `betapr` | `shape2` | Constant branch checked, Open, Beta-prime nonconstant single-held shape | Constant face checked | Open, Beta-prime conditional tie |
| `gev` | `all free` | Majority branch in existing Lean, Open, GEV all-free minimum-multiplicity test | Majority and unrestricted divergence in existing Lean, Open, GEV all-free minimum-multiplicity test | Open, GEV separated global ties |
| `gev` | `mu` | Half branch checked, Open, GEV held-location compactness | Half branch checked and majority path in existing Lean | Open, GEV separated global ties |
| `gev` | `sigma` | — | Existing Lean proves compact superlevels and closed-endpoint attainment | Open, GEV separated global ties |
| `frechet` | `all free` | Unrestricted, half and majority branches in existing Lean, Open, Fréchet sub-half Gumbel comparison | Half and majority branches in existing Lean, Open, Fréchet sub-half Gumbel comparison | Open, Fréchet all-free maximizing shapes |
| `frechet` | `lambda+sigma` | Open, Fréchet held shape and scale | Open, Fréchet held shape and scale | Open, Fréchet held shape and scale |
| `frechet` | `mu` | Checked | Checked | Checked |
| `frechet` | `mu+sigma` | Checked | Checked | Checked |
| `frechet` | `sigma` | Constant failure checked, Open, Fréchet held-scale nonconstant tail | Constant branch checked, Open, Fréchet held-scale nonconstant tail | Open, Fréchet held-scale global tie |
| `gb2` | `all free` | Checked at the stated constant scope | Checked at the stated constant scope | — |
| `gb2` | `scale+shape1+shape2` | Checked | Checked finite attainment | Checked |
| `gb2` | `scale+shape2` | Checked | Checked | Checked |
| `gb2` | `scale+shape2+shape3` | Checked | Checked finite attainment | Checked |
| `gb2` | `shape1` | Open, GB2 single-held beta shape face test | Constant case checked, Open, GB2 single-held beta shape face test | Open, GB2 Burr crossing examples |
| `gb2` | `shape1+shape3` | Checked | Checked | Checked |
| `gb2` | `shape3` | Open, GB2 single-held beta shape face test | Constant case checked, Open, GB2 single-held beta shape face test | Open, GB2 Burr crossing examples |
| `burr` | `all free` | — | — | Open, Burr and inverse Burr all-free global tie |
| `burr` | `scale+shape1` | Checked | Checked | Checked |
| `burr` | `shape2` | — | — | Checked via a different exact sample |
| `invburr` | `all free` | — | Checked exact two-face comparison | Open, Burr and inverse Burr all-free global tie |
| `invburr` | `scale` | Checked | Checked with existing power comparison | — |
| `invburr` | `scale+shape1` | Checked | — | — |
| `invburr` | `shape1` | Checked | — | — |
| `invburr` | `shape2` | — | — | Checked on the specified sample |

The PGW threshold comparison and stationary-witness equivalence,
GEV compact held-scale superlevels, and Fréchet fitted half and majority
branches use the existing developments. The new GEV theorem settles
exact half contact at held location, including its strict
nonattainment on unbalanced positive residuals. [PGW proofs](../../lean/UnivariateML/Family/AllFreePGW.lean)
[GEV held-scale proofs](../../lean/UnivariateML/Family/JoinsGEV.lean)
[Fréchet all-free collapse proofs](../../lean/UnivariateML/Family/AllFreeFrechet.lean)

## Remaining proof obligations

- **All-free face exhaustion.** The existing face paths and optimized face values give the two necessary lower bounds. The finite-attainment equivalence also needs uniform bounds on the profiled shape ends while the nuisance scale and exponent vary. No exhausting bound for all competitive escape sequences was checked. [Written proof](2026-11-25-expweibull-math-report.md#faces-and-finite-attainment)
- **All-free exponentiated Weibull tie.** The moving-sample proof requires both regional maximum values to vary continuously and to cross above both faces, with the separating region excluded. The source interval comparisons and the uniform exterior exclusions remain unformalized. [Written proof](2026-11-25-expweibull-math-report.md#two-finite-local-maxima-and-an-exact-real-global-tie)
- **Held-shape curvature.** The attainment result and constant path are checked. Uniqueness needs logarithmic convexity of the summed negative log CDF factors and the bound on their second derivative that makes the profiled objective strictly concave. [Written proof](2026-11-25-expweibull-math-report.md#held-shape-and-the-scalar-profile)
- **All-free PGW faces and stationary list.** Existing Lean constructs both face paths and excludes escapes for several fixed masks. The all-free result needs the optimized threshold upper bound uniform in moving scale, the nuisance profile across the Gompertz seam, and the signed collars that leave a finite stationary list. [Written proof](2026-11-25-pgw-math-report.md#gompertz-decisions-and-the-finite-critical-list)
- **PGW held-scale roots.** The finite fit and the unique conditional exponent already have Lean proofs. To characterize the entire maximizing set, the exponent score must be profiled analytically and its induced shape score isolated on the compact witness superlevel. [Written proof](2026-11-27-uniq-atlas-math-report.md#pgw-held-scale)
- **PGW held-shape finite retention.** The existing stationary-witness equivalence compares every finite stationary point with the Gompertz face. The analytic collar at the face and the finite root list that retains every tied maximum remain open. The narrower strict Gompertz winner is checked here. [Written proof](2026-11-25-pgw-math-report.md#gompertz-decisions-and-the-finite-critical-list)
- **Fatigue held-shape joint escape.** The constant collapse is checked. The fixed-shape log-density bound was proved for the double holds. In the joint scale and threshold problem it still needs to be summed with the mixed minimum and positive-gap penalty, giving a uniform lower-threshold bound of order minus the reciprocal square root of the gap. [Written proof](2026-11-27-lsheld-atlas-math-report.md#fatigue-held-alpha)
- **Fatigue stationary components.** Both stationary equations must be cleared over the positive gap and scale domain. Finiteness of the semialgebraically connected components and constancy of the likelihood along their piecewise smooth paths remain open. [Written proof](2026-11-27-lsheld-atlas-math-report.md#fatigue-held-alpha)
- **Fatigue held-shape-and-scale uniqueness.** The threshold tails and attainment are checked. The remaining proof needs the strict sign of the exponentially weighted score derivative when the held shape squared is at most one half, and the positive-root count of the cubic on constant data at arbitrary positive shape. [Written proof](2026-11-27-lsheld-atlas-math-report.md#fatigue-held-alphabeta)
- **Fatigue reciprocal scale pair.** The fixed-threshold scale ends and existence are checked. The constant-sample uniqueness split still needs the exact reduction to the scale-ratio sum and the reconstruction of both reciprocal maximizers for held shape greater than two. [Written proof](2026-11-27-lsheld-atlas-math-report.md#fatigue-held-alpha)
- **Fatigue held-scale rational profile.** The unique conditional shape from the existing fixed-threshold likelihood is a starting point. The profile must be extended to the threshold edge with the exact exponent n minus three times the minimum count, including the equality branch. Clearing the score denominators, excluding an identically zero numerator, and certifying the exact sample (0,1,1) remain open. [Written proof](2026-11-27-lsheld-atlas-math-report.md#fatigue-held-beta)
- **Generalized-gamma reflected all-free maxima.** The signed family identity transfers reflected maxima once they exist away from Q zero. The specified replicated sample still needs a finite witness above the seam and both endpoint faces, together with uniform signed compactness and a finite stationary comparison. [Written proof](2026-11-27-uniq-atlas-math-report.md#gengamma-held-all-free)
- **Generalized-gamma held-location signed lists.** Fixed-Q scale existence and uniqueness are checked, including Q zero and zero residual collapse. Optimized-scale limits at both signed shape ends, competition with the attained normal seam and the exact stationary lists remain open, especially for samples with mixed signs and contacts. [Written proof](2026-11-27-uniq-atlas-math-report.md#gengamma-held-mu)
- **Generalized-gamma free shape at held scale.** The fixed-Q location optimum is checked. The signed shape optimization needs the real-argument Gamma-mode bound and continuity through Q zero. A strict comparison with the normal mode using positivity of the Stirling remainder would also give the all-zero-residual singleton. [Written proof](2026-11-10-gg-theory-report.md#mu-and-sigma-held-q-free)
- **Generalized-gamma reflected held-scale maxima.** The held-scale two-point example needs a checked value excluding Q zero and a proof of an attained nonzero signed optimum. Reflection then supplies its distinct partner. [Written proof](2026-11-27-uniq-atlas-math-report.md#gengamma-held-sigma)
- **Generalized-gamma moving signed paths.** The existing fixed-location signed paths include equality at the fixed endpoint. To obtain the all-free moving-location paths, the log-density asymptotic has to control the scaled distance to the endpoint at contacts. [Written proof](2026-11-10-gg-theory-report.md#free-fit-signed-profile-and-certificate)
- **All-free transformed-gamma profile.** Existing Lean gives power, Pareto and canonical lognormal paths and rate optimization. The full finite-attainment test still needs the optimized signed profile, the Stirling and digamma comparisons near the normal seam, the reversed small-shape estimate at large power and the finite root isolation, including the balanced moment case. [Written proof](2026-11-25-trgamma-math-report.md#signed-exteriors-and-finite-root-count)
- **Transformed-gamma finite-power equality.** The exact equality sample requires the signed regional height crossover and a certificate excluding better points outside the candidate region. Reachability of the boundary value alone is already checked. [Written proof](2026-11-25-trgamma-math-report.md#exact-finitepower-equality-over-the-reals)
- **Transformed-gamma held-scale compactness.** Both orientations have checked constant concentration, finite endpoint witnesses, and a unique centered noncollapsed fit. The general bounded branch still needs uniform lower-power and off-support upper-power control with gamma shape optimized, and exhaustion by the best supported trace when the sample is on the face support. [Written proof](2026-11-25-trgamma-math-report.md#held-scale-and-both-shapes-free)
- **Inverse transformed-gamma finite pair.** Reciprocation preserves likelihood rankings and the centered fit. The all-free replicated outlier construction still needs an interior witness separated from both faces, uniform regional bounds over the deformation and the crossing itself. [Written proof](2026-11-27-uniq-atlas-math-report.md#invtrgamma-held-all-free)
- **Beta-prime compact ratio comparison.** Existing Lean gives both face paths and unboundedness for constants. The all-free comparison needs uniform concentration control with both beta shapes moving, continuity of the optimized nuisance values through both endpoints and the signed exterior and finite comparisons. [Written proof](2026-11-25-betapr-math-report.md#compact-scalar-decision-for-all-free-data)
- **Beta-prime exact four-way equality.** The two finite maxima and both faces require the source parameter construction, exact endpoint heights and strict exclusion of every other ratio. This is stronger than the checked constant gamma comparison. [Written proof](2026-11-25-betapr-math-report.md#exactly-two-finite-maxima-tied-with-both-faces)
- **Beta-prime nonconstant single-held shape.** The checked constant comparisons use a strict Gamma remainder inequality. The nonconstant case needs existence of the optimal scale through the compact ratio chart, exterior control at the other end and the signed profile comparison, including vanishing leading coefficient. [Written proof](2026-11-25-betapr-math-report.md#one-shape-held)
- **Beta-prime conditional tie.** The reported two-peak counterexample depends on interval separation of the basins and a signed crossing of the winning heights. Its compact exclusion and the transferred inverse-gamma version are open. [Written proof](2026-11-25-betapr-math-report.md#one-shape-held)
- **GEV all-free minimum-multiplicity test.** Existing Lean covers the endpoint spike and majority divergence and constructs the half-collapse trace. The full comparison still needs compact positive superlevels for strict sub-half minima and the all-shape half-sample pair bound with equality only at the balanced reverse-exponential fit. [Written proof](2026-11-27-lean-allfree2-report.md)
- **GEV separated global ties.** The half-contact result identifies a single compatible endpoint fit. The nonunique all-free, held-location and held-scale examples still require the uniform interval exclusions, the endpoint versus interior regional height comparisons, and the exact crossings. [Written proof](2026-11-25-gev-math-report.md#global-ties-with-public-scale-or-location-held)
- **GEV held-location compactness.** The held-minimum half case is checked. With a negative residual or strict sub-half contact, the small-scale bound must be uniform across the fitted shape interval. Upper semicontinuity at closed reverse-exponential contact must also be transported to jointly varying scale. [Written proof](2026-11-25-gev-math-report.md#location-held)
- **Fréchet sub-half Gumbel comparison.** The older all-free proof supplies unrestricted divergence, majority divergence and the exact half-collapse supremum. A uniform compact bound for the location-scale profile as reciprocal shape tends to zero and the optimized Gumbel value comparison remain open, including equality with a finite dominance witness. [Written proof](2026-11-25-frechet-math-report.md#collapse-and-the-gumbel-comparison)
- **Fréchet all-free maximizing shapes.** For two levels the strict interior comparison in the transformed gap chart remains open. The multilevel tie needs the two bracketed regional height maxima, exclusion of the separator and positive inward derivative at the Gumbel face. [Written proof](2026-11-25-frechet-math-report.md#finite-nonuniqueness)
- **Fréchet held shape and scale.** The existing held-shape proof profiles scale before optimizing location. With scale fixed, the relevant monotone ratio is the sum of distances to power minus shape minus one divided by the reciprocal-distance sum. Its monotonicity and both sign limits for the location score, including all constants, remain to be checked. [Written proof](2026-11-25-frechet-math-report.md#fréchet-single-holds)
- **Fréchet held-scale nonconstant tail.** The constant collapse is checked. Nonconstant attainment needs a uniform lower enclosure of the location root and the range-dependent bound at infinite shape, plus bounded-shape location ends and exclusion of a proper Gumbel face with physical scale fixed. [Written proof](2026-11-25-frechet-math-report.md#fréchet-single-holds)
- **Fréchet held-scale global tie.** The held-scale interval crossing needs verified opposing endpoint height signs, separator exclusion on the whole data range, and a uniform upper bound above the retained shape interval. [Written proof](2026-11-25-frechet-math-report.md#fréchet-single-holds)
- **GB2 single-held beta shape face test.** The constant branch follows the checked power concentration path. On nonconstant data the compactification must control the density peak and mode, exhaust both competitive escapes and include the contact penalty before comparing the stationary values. [Written proof](2026-11-27-genbeta-atlas-math-report.md#gb2-held-shape1)
- **GB2 Burr crossing examples.** The specified samples require the all-free Burr finite crossing above both faces, with shape3 held at one and shape1 held at one on the reciprocal data. The checked fixed-power Burr ties concern a different mask. [Written proof](2026-11-27-genbeta-atlas-math-report.md#gb2-held-shape3)
- **Burr and inverse Burr all-free global tie.** Existing Lean gives the attainment test, and the reciprocal supremum comparison is checked here. The global tie needs the uniform witness over the replicated bulk and outlier deformation, exclusion of the separator and both tail regions, and opposite regional height signs. The reciprocal maximizing-set transfer can then be applied. [Written proof](2026-11-25-burr2-math-report.md#two-finite-global-maximizers-above-both-faces)

## Limit paths, face comparisons and scoped results

The 81 entries come from the written-claims table in the
coverage review. Conditions such as fixed product, optimized trace and
fitter-specific arithmetic appear separately when their proofs have
different scope. [Coverage review](2026-11-27-atlas-coverage3-report.md#written-claims-awaiting-full-lean-coverage)

| Family | Held | Key | Proof work and scope |
| --- | --- | --- | --- |
| `expweibull` | `all free` | `path:1` | The concentration path and its divergence are checked at the relevant holds. |
| `expweibull` | `exponent` | `path:1` | The concentration path and its divergence are checked at the relevant holds. |
| `expweibull` | `exponent+scale` | `path:1` | The concentration path and its divergence are checked at the relevant holds. |
| `expweibull` | `scale` | `path:1` | The constant path at or above held scale is checked. |
| `expweibull` | `scale` | `path:2` | The held-scale path and strict optimized supremum are checked, with an explicit penalty for each contact. |
| `expweibull` | `scale` | `path:3` | The held-scale path and strict optimized supremum are checked, with an explicit penalty for each contact. |
| `expweibull` | `shape` | `path:1` | The concentration path and its divergence are checked at the relevant holds. |
| `expweibull` | `all free` | `limit:1` | The concentration path and its divergence are checked at the relevant holds. |
| `expweibull` | `exponent` | `limit:1` | The concentration path and its divergence are checked at the relevant holds. |
| `expweibull` | `exponent+scale` | `limit:1` | The concentration path and its divergence are checked at the relevant holds. |
| `expweibull` | `scale` | `limit:1` | The constant path at or above held scale is checked. |
| `expweibull` | `scale` | `limit:2` | The held-scale path and strict optimized supremum are checked, with an explicit penalty for each contact. |
| `expweibull` | `scale` | `limit:3` | The held-scale path and strict optimized supremum are checked, with an explicit penalty for each contact. |
| `expweibull` | `shape` | `limit:1` | The concentration path and its divergence are checked at the relevant holds. |
| `gengamma` | `all free` | `path:1` | Existing Lean gives the fixed-endpoint signed paths. open for Generalized-gamma moving signed paths for joint moving locations and contact traces. |
| `gengamma` | `all free` | `path:2` | Existing Lean gives the fixed-endpoint signed paths. open for Generalized-gamma moving signed paths for joint moving locations and contact traces. |
| `trgamma` | `all free` | `path:2` | Existing transformed-gamma hold-specific concentration paths give admissible divergence. |
| `trgamma` | `scale` | `path:1` | The fixed positive product path and strict finite witness are checked. An arbitrary convergent shape-power product remains open. |
| `trgamma` | `scale` | `path:2` | The canonical concentration path is checked including equality at held scale. |
| `trgamma` | `scale+shape1` | `path:2` | Existing transformed-gamma hold-specific concentration paths give admissible divergence. |
| `trgamma` | `shape1` | `path:2` | Existing transformed-gamma hold-specific concentration paths give admissible divergence. |
| `trgamma` | `shape2` | `path:2` | Existing transformed-gamma hold-specific concentration paths give admissible divergence. |
| `trgamma` | `all free` | `limit:2` | Existing transformed-gamma hold-specific concentration paths give admissible divergence. |
| `trgamma` | `scale` | `limit:2` | The canonical concentration path is checked including equality at held scale. |
| `trgamma` | `scale+shape1` | `limit:2` | Existing transformed-gamma hold-specific concentration paths give admissible divergence. |
| `trgamma` | `shape1` | `limit:2` | Existing transformed-gamma hold-specific concentration paths give admissible divergence. |
| `trgamma` | `shape2` | `limit:2` | Existing transformed-gamma hold-specific concentration paths give admissible divergence. |
| `invtrgamma` | `all free` | `path:2` | Existing transformed-gamma hold-specific concentration paths give admissible divergence. |
| `invtrgamma` | `scale` | `path:1` | The fixed positive product path and strict finite witness are checked. An arbitrary convergent shape-power product remains open. |
| `invtrgamma` | `scale` | `path:2` | The canonical concentration path is checked including equality at held scale. |
| `invtrgamma` | `scale+shape1` | `path:2` | Existing transformed-gamma hold-specific concentration paths give admissible divergence. |
| `invtrgamma` | `shape1` | `path:2` | Existing transformed-gamma hold-specific concentration paths give admissible divergence. |
| `invtrgamma` | `shape2` | `path:2` | Existing transformed-gamma hold-specific concentration paths give admissible divergence. |
| `invtrgamma` | `all free` | `limit:2` | Existing transformed-gamma hold-specific concentration paths give admissible divergence. |
| `invtrgamma` | `scale` | `limit:2` | The canonical concentration path is checked including equality at held scale. |
| `invtrgamma` | `scale+shape1` | `limit:2` | Existing transformed-gamma hold-specific concentration paths give admissible divergence. |
| `invtrgamma` | `shape1` | `limit:2` | Existing transformed-gamma hold-specific concentration paths give admissible divergence. |
| `invtrgamma` | `shape2` | `limit:2` | Existing transformed-gamma hold-specific concentration paths give admissible divergence. |
| `betapr` | `all free` | `path:3` | Existing Lean gives unboundedness at fixed scale. The explicit simultaneous shape ray in beta-prime coordinates remains open. |
| `betapr` | `shape1` | `path:1` | Existing gamma path at unit held lower shape gives the stated fixed-ratio limit. |
| `betapr` | `shape1` | `path:4` | Existing Lean gives the canonical fixed-ratio path for each positive held lower shape. A varying ratio with the same limit remains open. |
| `betapr` | `shape2` | `path:2` | Existing inverse-gamma path at unit held upper shape gives the stated fixed-product limit. |
| `betapr` | `shape2` | `path:5` | Existing Lean gives the canonical fixed-product path for each positive held upper shape. A varying product with the same limit remains open. |
| `betapr` | `all free` | `limit:6` | open for Beta-prime compact ratio comparison for exact optimized comparisons. The branch using a numerical skewness margin needs separate arithmetic analysis. |
| `betapr` | `all free` | `limit:7` | open for Beta-prime compact ratio comparison for exact optimized comparisons. The branch using a numerical skewness margin needs separate arithmetic analysis. |
| `betapr` | `shape1` | `limit:4` | The exact constant branch is checked. The nonconstant signed tail and interior exclusions remain open for Beta-prime nonconstant single-held shape. |
| `betapr` | `shape2` | `limit:5` | The constant branch is checked by reciprocity. The nonconstant signed tail and interior exclusions remain open for Beta-prime nonconstant single-held shape. |
| `gev` | `all free` | `path:2` | Existing Lean proves a fixed-shape-one collapse path with a vanishing positive support gap. Joint rate dependence remains open. |
| `gev` | `all free` | `limit:2` | open for GEV all-free minimum-multiplicity test for the exact half-collapse comparison across all fitted shapes and free locations. |
| `gev` | `mu` | `limit:1` | The exact half-contact trace, strict comparison away from equality and balanced reverse-exponential attainment are checked. |
| `gev` | `mu` | `case:held-minimum-half` | The exact half-contact trace, strict comparison away from equality and balanced reverse-exponential attainment are checked. |
| `frechet` | `mu+sigma` | `path:2` | The held-scale concentration path and the held-threshold all-contact predicate are checked. |
| `frechet` | `sigma` | `path:2` | The held-scale concentration path and the held-threshold all-contact predicate are checked. |
| `frechet` | `all free` | `limit:3` | open for Fréchet sub-half Gumbel comparison for sub-half-minimum compactness and the exact optimized Gumbel comparison. |
| `frechet` | `mu+sigma` | `limit:2` | The held-scale concentration path and the held-threshold all-contact predicate are checked. |
| `frechet` | `sigma` | `limit:2` | The held-scale concentration path and the held-threshold all-contact predicate are checked. |
| `frechet` | `all free` | `case:strict-minimum-witness` | open for Fréchet sub-half Gumbel comparison for sub-half-minimum compactness and the exact optimized Gumbel comparison. |
| `gb2` | `all free` | `path:3` | The constant-sample concentration path and its unboundedness are checked. |
| `gb2` | `all free` | `limit:3` | The constant-sample concentration path and its unboundedness are checked. |
| `burr` | `all free` | `path:4` | Existing Lean gives the literal fixed-endpoint path with its contact penalty and the moving-endpoint path reaching the optimized face. |
| `burr` | `scale+shape1` | `path:1` | The all-contact concentration path and its divergence are checked. |
| `burr` | `shape2` | `path:2` | Existing Lean gives the chart path at fixed limiting Weibull scale. General moving ratios in physical scale remain open. |
| `burr` | `all free` | `limit:3` | Existing Lean gives the exact two-face criterion and supremum. The numerical certificate conditions in the dossier remain open as Lean implementation checks. |
| `burr` | `all free` | `limit:4` | Existing Lean gives the exact two-face criterion and supremum. The numerical certificate conditions in the dossier remain open as Lean implementation checks. |
| `burr` | `scale` | `limit:1` | Existing BurrHeld scaled constant result gives divergence at and below held scale. |
| `burr` | `scale+shape1` | `limit:1` | The all-contact concentration path and its divergence are checked. |
| `burr` | `shape1` | `limit:1` | Existing JoinsBurr constant path proves divergence with first shape held. |
| `burr` | `shape2` | `limit:2` | Existing Lean gives the exact transformed-witness comparison. The fitter tolerance and uncertified fallback remain open as Lean implementation checks. |
| `invburr` | `all free` | `path:1` | The inherited fixed-scale-at-contact path diverges with any fixed positive first shape. |
| `invburr` | `all free` | `path:3` | Existing reciprocal likelihood identity plus the Burr face path gives the canonical chart limit. A direct joint theorem over varying physical ratios remains open. |
| `invburr` | `all free` | `path:4` | The reciprocal of the literal fixed-endpoint path carries the contact penalty. A direct physical-limit declaration remains open. |
| `invburr` | `scale` | `path:1` | The diverging power and conditional first-shape path are checked at or above held scale. |
| `invburr` | `scale` | `path:5` | Existing Lean proves the reciprocal profile limit and optimized value, with contact. Simultaneous shape-product convergence in the same public held-scale path theorem remains open. |
| `invburr` | `scale+shape1` | `path:1` | The inherited fixed-scale-at-contact path diverges with any fixed positive first shape. |
| `invburr` | `shape1` | `path:1` | The inherited fixed-scale-at-contact path diverges with any fixed positive first shape. |
| `invburr` | `shape2` | `path:2` | Existing reciprocal likelihood identity plus the Burr face path gives the canonical chart limit. A direct joint theorem over varying physical ratios remains open. |
| `invburr` | `all free` | `limit:3` | The exact optimized two-face comparison is checked. The arithmetic-specific fitter certificate remains open. |
| `invburr` | `all free` | `limit:4` | The exact optimized two-face comparison is checked. The arithmetic-specific fitter certificate remains open. |
| `invburr` | `scale` | `limit:1` | The diverging power and conditional first-shape path are checked at or above held scale. |
| `invburr` | `scale+shape1` | `limit:1` | The inherited joint path and the checked attainment iff give the exact constant-branch outcome at the relevant holds. |
| `invburr` | `shape1` | `limit:1` | The inherited joint path and the checked attainment iff give the exact constant-branch outcome at the relevant holds. |

## Checked Lean statements

The statements below are copied from the checked modules. The summaries
give the observation and parameter scope used in the atlas fragment.

### expweibull

**`UnivariateML.Shape2ExpWeibull.constant_concentration`**

Increasing shape with scale at the common observation gives divergence for any positive held exponent. Constant branch; fixed exponent witness. [Lean source](../../lean/UnivariateML/Family/Shape2ExpWeibull.lean#L43)

```lean
theorem constant_concentration (hn : 0 < n) {x₀ c : ℝ} (hx₀ : 0 < x₀)
    (_hc : 0 < c) :
    Tendsto (fun b ↦ logLik (fun _ : Fin n ↦ x₀) b x₀ c) atTop atTop
```

**`UnivariateML.Shape2ExpWeibull.held_exponent_attains_iff`**

A finite held-exponent fit exists exactly on nonconstant positive data. [Lean source](../../lean/UnivariateML/Family/Shape2ExpWeibull.lean#L50)

```lean
theorem held_exponent_attains_iff (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {c : ℝ} (hc : 0 < c) :
    (∃ p : ℝ × ℝ, p ∈ Weibull.domain ∧
      IsMaxOn (fun q ↦ logLik x q.1 q.2 c) Weibull.domain p) ↔
      ∃ i j, x i ≠ x j
```

**`UnivariateML.Shape2ExpWeibull.held_exponent_scale`**

The free shape is unique off the all-contact sample. All contacts give divergence. [Lean source](../../lean/UnivariateML/Family/Shape2ExpWeibull.lean#L279)

```lean
theorem held_exponent_scale (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {a c : ℝ} (ha : 0 < a) (hc : 0 < c) :
    ((∃ i, x i ≠ a) →
      ∃! b, b ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun b ↦ logLik x b a c) (Ioi 0) b) ∧
    ((∀ i, x i = a) → Tendsto (fun b ↦ logLik x b a c) atTop atTop)
```

**`UnivariateML.Shape2ExpWeibull.held_exponent_shape_mle`**

A positive held exponent and shape leave a unique scale, including for constants. [Lean source](../../lean/UnivariateML/Family/Shape2ExpWeibull.lean#L299)

```lean
theorem held_exponent_shape_mle (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {b c : ℝ} (hb : 0 < b) (hc : 0 < c) :
    ∃! a, a ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun a ↦ logLik x b a c) (Ioi 0) a
```

**`UnivariateML.Shape2ExpWeibull.held_shape_constant_concentration`**

A joint scale and exponent path gives divergence at any positive held shape on constant data. Constant branch. [Lean source](../../lean/UnivariateML/Family/Shape2ExpWeibull.lean#L376)

```lean
theorem held_shape_constant_concentration (hn : 0 < n) {x₀ b : ℝ}
    (hx₀ : 0 < x₀) (hb : 0 < b) :
    Tendsto (fun t ↦ logLik (fun _ : Fin n ↦ x₀) b
      (x₀ * exp (-log t / b)) (exp t)) atTop atTop
```

**`UnivariateML.Shape2ExpWeibull.held_shape_attains_iff`**

A finite held-shape fit exists exactly on nonconstant positive data. [Lean source](../../lean/UnivariateML/Family/Shape2ExpWeibull.lean#L400)

```lean
theorem held_shape_attains_iff (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {b : ℝ} (hb : 0 < b) :
    (∃ p ∈ Weibull.domain,
      IsMaxOn (fun q : ℝ × ℝ ↦ logLik x b q.1 q.2) Weibull.domain p) ↔
      ∃ i j, x i ≠ x j
```

**`UnivariateML.Shape2ExpWeibull.held_scale_power_path`**

The held-scale path recovers the power trace for every eventually positive exponent with shape times exponent tending to a positive constant. Includes every endpoint contact. [Lean source](../../lean/UnivariateML/Family/Shape2ExpWeibull.lean#L503)

```lean
theorem held_scale_power_path {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {a k : ℝ} (ha : 0 < a) (hk : 0 < k) (hle : ∀ i, x i ≤ a)
    {c : ℝ → ℝ} (hc : ∀ᶠ b in atTop, 0 < c b)
    (hbc : Tendsto (fun b ↦ b * c b) atTop (𝓝 k)) :
    Tendsto (fun b ↦ logLik x b a (c b)) atTop (𝓝 (heldPowerTrace x a k))
```

**`UnivariateML.Shape2ExpWeibull.held_scale_power_supremum`**

When some observation lies below the hold and none exceeds it, the optimized trace strictly beats every finite fit. Includes constant data strictly below the hold. [Lean source](../../lean/UnivariateML/Family/Shape2ExpWeibull.lean#L527)

```lean
theorem held_scale_power_supremum (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {a : ℝ} (ha : 0 < a) (hle : ∀ i, x i ≤ a)
    (hlt : ∃ i, x i < a) :
    let D := ∑ i, -log (x i / a)
    let P := heldPowerTrace x a ((n : ℝ) / D)
    IsLUB ((fun p : ℝ × ℝ ↦ logLik x p.1 a p.2) '' Weibull.domain) P ∧
      ∀ b c, 0 < b → 0 < c → logLik x b a c < P
```

**`UnivariateML.Shape2ExpWeibull.held_scale_constant_concentration`**

Constant samples at or above the held scale admit the specified joint shape and exponent escape. Constant branch. [Lean source](../../lean/UnivariateML/Family/Shape2ExpWeibull.lean#L598)

```lean
theorem held_scale_constant_concentration (hn : 0 < n) {x₀ a : ℝ}
    (ha : 0 < a) (hxa : a ≤ x₀) :
    Tendsto (fun b ↦ logLik (fun _ : Fin n ↦ x₀) b a
      (exp (exp (b * log (x₀ / a))))) atTop atTop
```

**`UnivariateML.Shape2ExpWeibull.held_scale_attains_iff`**

A finite held-scale fit exists exactly for nonconstant data with an observation above scale. [Lean source](../../lean/UnivariateML/Family/Shape2ExpWeibull.lean#L793)

```lean
theorem held_scale_attains_iff (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {a : ℝ} (ha : 0 < a) :
    (∃ p ∈ Weibull.domain,
      IsMaxOn (fun q : ℝ × ℝ ↦ logLik x q.1 a q.2) Weibull.domain p) ↔
      (∃ i j, x i ≠ x j) ∧ ∃ i, a < x i
```

**`UnivariateML.Shape2ExpWeibull.held_scale_maximizer_set`**

The entire maximizing set comes from the highest finite stationary shapes in the witness superlevel and their uniquely profiled exponents. Attained branch. [Lean source](../../lean/UnivariateML/Family/Shape2ExpWeibull.lean#L911)

```lean
theorem held_scale_maximizer_set (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {a : ℝ} (ha : 0 < a)
    (hne : ∃ i j, x i ≠ x j) (habove : ∃ i, a < x i) :
    let d := fun i ↦ log (x i / a)
    let W := {b ∈ Ioi (0 : ℝ) | heldScaleProfile d 1 ≤ heldScaleProfile d b}
    let S := {b ∈ W | deriv (heldScaleProfile d) b = 0}
    IsCompact W ∧ S.Finite ∧ S.Nonempty ∧
      ∀ p ∈ Weibull.domain,
        IsMaxOn (fun q : ℝ × ℝ ↦ logLik x q.1 a q.2) Weibull.domain p ↔
        p.1 ∈ S ∧ p.2 = (n : ℝ) / heldScaleRate d p.1 ∧
          IsMaxOn (heldScaleProfile d) S p.1
```

### pgw

**`UnivariateML.Shape2PGW.held_exponent_attains_iff`**

A finite held-exponent fit exists exactly for nonconstant positive data. [Lean source](../../lean/UnivariateML/Family/Shape2PGW.lean#L27)

```lean
theorem held_exponent_attains_iff (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {c : ℝ} (hc : 0 < c) :
    (∃ p ∈ Weibull.domain,
      IsMaxOn (fun q ↦ logLik x q.1 q.2 c) Weibull.domain p) ↔
      ∃ i j, x i ≠ x j
```

**`UnivariateML.Shape2PGW.held_exponent_scale_attains_iff`**

A finite free shape exists exactly when some observation differs from scale. [Lean source](../../lean/UnivariateML/Family/Shape2PGW.lean#L48)

```lean
theorem held_exponent_scale_attains_iff (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {a c : ℝ} (ha : 0 < a) (hc : 0 < c) :
    (∃ b ∈ Ioi (0 : ℝ), IsMaxOn (fun b ↦ logLik x b a c) (Ioi 0) b) ↔
      ∃ i, x i ≠ a
```

**`UnivariateML.Shape2PGW.held_scale_constant_at_or_below`**

A constant sample at or below scale admits a diverging positive-shape path. Constant concentration branch. [Lean source](../../lean/UnivariateML/Family/Shape2PGW.lean#L65)

```lean
theorem held_scale_constant_at_or_below (hn : 0 < n) {t a : ℝ}
    (ht : 0 < t) (hle : t ≤ a) :
    ∃ c : ℝ → ℝ, (∀ b, 0 < c b) ∧
      Tendsto (fun b ↦ logLik (fun _ : Fin n ↦ t) b a (c b)) atTop atTop
```

**`UnivariateML.Shape2PGW.held_shape_narrow_supremum`**

The optimized Gompertz value is a strict supremum when all powered ratios to the maximum are at least one half. Includes constant samples by taking the bound X at the common observation. [Lean source](../../lean/UnivariateML/Family/Shape2PGW.lean#L77)

```lean
theorem held_shape_narrow_supremum (hn : 0 < n) {x : Fin n → ℝ} {b X : ℝ}
    (hb : 0 < b) (hx : ∀ i, 0 < x i) (hX : ∀ i, x i ≤ X)
    (hnarrow : ∀ i, 1 / 2 ≤ (x i / X) ^ b) :
    ∃ r > 0,
      IsLUB ((fun p : ℝ × ℝ ↦ logLik x b p.1 p.2) '' Weibull.domain)
        (AllFreePGW.gompertzLogLik x b r) ∧
      ∀ p ∈ Weibull.domain, logLik x b p.1 p.2 < AllFreePGW.gompertzLogLik x b r
```

### fatigue

**`UnivariateML.Shape2Fatigue.all_free_threshold_path`**

Every tied minimum has diverging log density and the other terms have finite limits on the same path. [Lean source](../../lean/UnivariateML/Family/Shape2Fatigue.lean#L89)

```lean
theorem all_free_threshold_path {x : Fin n → ℝ} {m : ℝ} (_hm : ∀ i, m ≤ x i) :
    (∀ i, x i = m →
      Tendsto (fun r : ℝ ↦ Fatigue.logDensity (sqrt r) r⁻¹ (x i - (m - (r⁻¹) ^ 2)))
        atTop atTop) ∧
    (∀ i, m < x i →
      Tendsto (fun r : ℝ ↦ Fatigue.logDensity (sqrt r) r⁻¹ (x i - (m - (r⁻¹) ^ 2)))
        atTop (𝓝 (log (x i - m) - 3 * log (x i - m) / 2 -
          log 2 - log (2 * π) / 2 - (x i - m) / 2)))
```

**`UnivariateML.Shape2Fatigue.all_free_unbounded`**

Every nonempty real sample has arbitrarily high finite log likelihood. [Lean source](../../lean/UnivariateML/Family/Shape2Fatigue.lean#L107)

```lean
theorem all_free_unbounded (hn : 0 < n) (x : Fin n → ℝ) :
    ∀ M : ℝ, ∃ (a b μ : ℝ), 0 < a ∧ 0 < b ∧ (∀ i, μ < x i) ∧
      M < Fatigue.logLik (fun i ↦ x i - μ) a b
```

**`UnivariateML.Shape2Fatigue.all_free_no_max`**

There is no maximum over the admissible shape, scale and threshold space for any nonempty sample. [Lean source](../../lean/UnivariateML/Family/Shape2Fatigue.lean#L142)

```lean
theorem all_free_no_max (hn : 0 < n) (x : Fin n → ℝ) :
    ¬ ∃ p : ℝ × ℝ × ℝ,
      p ∈ {q | 0 < q.1 ∧ 0 < q.2.1 ∧ ∀ i, q.2.2 < x i} ∧
      IsMaxOn (fun q : ℝ × ℝ × ℝ ↦ Fatigue.logLik (fun i ↦ x i - q.2.2) q.1 q.2.1)
        {q | 0 < q.1 ∧ 0 < q.2.1 ∧ ∀ i, q.2.2 < x i} p
```

**`UnivariateML.Shape2Fatigue.held_alpha_mu_scale_ends`**

With shape and threshold held, both scale ends send the likelihood to zero. [Lean source](../../lean/UnivariateML/Family/Shape2Fatigue.lean#L208)

```lean
theorem held_alpha_mu_scale_ends (hn : 0 < n) {t : Fin n → ℝ}
    (ht : ∀ i, 0 < t i) {a : ℝ} (ha : 0 < a) :
    Tendsto (Fatigue.logLik t a) (𝓝[>] 0) atBot ∧
      Tendsto (Fatigue.logLik t a) atTop atBot
```

**`UnivariateML.Shape2Fatigue.held_alpha_mu_attains`**

With shape and threshold held, positive scale attains for all nonempty positive excess samples. [Lean source](../../lean/UnivariateML/Family/Shape2Fatigue.lean#L273)

```lean
theorem held_alpha_mu_attains (hn : 0 < n) {t : Fin n → ℝ}
    (ht : ∀ i, 0 < t i) {a : ℝ} (ha : 0 < a) :
    ∃ b ∈ Ioi 0, IsMaxOn (Fatigue.logLik t a) (Ioi 0) b
```

**`UnivariateML.Shape2Fatigue.held_alpha_beta_threshold_ends`**

With shape and scale held, the likelihood vanishes at contact and at the infinitely receding threshold. Coordinates are gaps from an attained minimum. [Lean source](../../lean/UnivariateML/Family/Shape2Fatigue.lean#L334)

```lean
theorem held_alpha_beta_threshold_ends (hn : 0 < n) {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) (hz : ∃ i, g i = 0) {a b : ℝ} (ha : 0 < a) (hb : 0 < b) :
    Tendsto (fun δ ↦ Fatigue.logLik (fun i ↦ δ + g i) a b) (𝓝[>] 0) atBot ∧
      Tendsto (fun δ ↦ Fatigue.logLik (fun i ↦ δ + g i) a b) atTop atBot
```

**`UnivariateML.Shape2Fatigue.held_alpha_beta_attains`**

With shape and scale held, there is an interior threshold optimum for every nonempty sample. Coordinates are gaps from an attained minimum. [Lean source](../../lean/UnivariateML/Family/Shape2Fatigue.lean#L371)

```lean
theorem held_alpha_beta_attains (hn : 0 < n) {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) (hz : ∃ i, g i = 0) {a b : ℝ} (ha : 0 < a) (hb : 0 < b) :
    ∃ δ ∈ Ioi (0 : ℝ),
      IsMaxOn (fun d ↦ Fatigue.logLik (fun i ↦ d + g i) a b) (Ioi 0) δ
```

**`UnivariateML.Shape2Fatigue.held_alpha_constant_collapse`**

At held positive shape the tied sample can collapse with scale equal to the distance from threshold to the observations. Constant branch. [Lean source](../../lean/UnivariateML/Family/Shape2Fatigue.lean#L385)

```lean
theorem held_alpha_constant_collapse (hn : 0 < n) {a : ℝ} (ha : 0 < a) :
    Tendsto (fun d : ℝ ↦ Fatigue.logLik (fun _ : Fin n ↦ d) a d)
      (𝓝[>] 0) atTop
```

### gengamma

**`UnivariateML.Shape2GenGamma.held_Q_mle`**

Every fixed real Q gives a unique location and scale on nonconstant log data and scale collapse on constant log data. Includes Q = 0. [Lean source](../../lean/UnivariateML/Family/Shape2GenGamma.lean#L214)

```lean
theorem held_Q_mle (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) (Q : ℝ) :
    ((∃ i j, log (x i) ≠ log (x j)) →
      ∃! p : ℝ × ℝ, p ∈ Normal.domain ∧
        IsMaxOn (fun s ↦ sampleLogLik x (s.1, s.2, Q)) Normal.domain p) ∧
    ((∀ i j, log (x i) = log (x j)) →
      ∃ μ, Tendsto (fun σ ↦ sampleLogLik x (μ, σ, Q)) (𝓝[>] 0) atTop)
```

**`UnivariateML.Shape2GenGamma.held_Q_mu_scale_classification`**

Every fixed real Q and location gives a unique scale off all-zero residuals and divergence otherwise. Fixed-Q scale slice for the mu mask. [Lean source](../../lean/UnivariateML/Family/Shape2GenGamma.lean#L31)

```lean
theorem held_Q_mu_scale_classification (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) (μ Q : ℝ) :
    ((∃ i, log (x i) ≠ μ) →
      ∃! σ, σ ∈ Ioi (0 : ℝ) ∧
        IsMaxOn (fun s ↦ sampleLogLik x (μ, s, Q)) (Ioi 0) σ) ∧
    ((∀ i, log (x i) = μ) →
      Tendsto (fun σ ↦ sampleLogLik x (μ, σ, Q)) (𝓝[>] 0) atTop)
```

**`UnivariateML.Shape2GenGamma.held_Q_sigma_location_mle`**

A unique location exists with scale and shape held, including for constants and Q = 0. [Lean source](../../lean/UnivariateML/Family/Shape2GenGamma.lean#L56)

```lean
theorem held_Q_sigma_location_mle (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {σ : ℝ} (hσ : 0 < σ) (Q : ℝ) :
    ∃! μ, IsMaxOn (fun m ↦ sampleLogLik x (m, σ, Q)) univ μ
```

### trgamma

**`UnivariateML.Shape2TrGamma.held_scale_power_path`**

With positive shape-power product fixed, the power trace loses one log unit at each contact before averaging. Canonical fixed-product path. [Lean source](../../lean/UnivariateML/Family/Shape2TrGamma.lean#L46)

```lean
theorem held_scale_power_path {x : Fin n → ℝ} {a d : ℝ}
    (_ha : 0 < a) (hd : 0 < d) (hx : ∀ i, 0 < x i) (hle : ∀ i, x i ≤ a) :
    Tendsto (fun q : ℝ ↦ logLik x (d * q) q⁻¹ a) (𝓝[>] 0)
      (𝓝 (log d - Gamma.avgLog x + d * Gamma.avg (fun i ↦ log (x i) - log a) -
        Gamma.avg (fun i ↦ if x i = a then 1 else 0)))
```

**`UnivariateML.Shape2TrGamma.held_scale_power_trace_beaten`**

Each positive-rate held-endpoint trace lies strictly below a finite point. Samples on the power support, including contact. [Lean source](../../lean/UnivariateML/Family/Shape2TrGamma.lean#L148)

```lean
theorem held_scale_power_trace_beaten {x : Fin n → ℝ} {a d : ℝ}
    (_ha : 0 < a) (hd : 0 < d) (hx : ∀ i, 0 < x i) (hle : ∀ i, x i ≤ a) :
    ∃ k > 0, ∃ p > 0,
      log d - Gamma.avgLog x + d * Gamma.avg (fun i ↦ log (x i) - log a) -
        Gamma.avg (fun i ↦ if x i = a then 1 else 0) < logLik x k p a
```

**`UnivariateML.Shape2TrGamma.held_scale_centered_mle`**

On the noncollapsed zero-mean log-residual slice, shape and power each have a unique optimum. All-contact data instead concentrate. [Lean source](../../lean/UnivariateML/Family/Shape2TrGamma.lean#L85)

```lean
theorem held_scale_centered_mle (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {a : ℝ} (ha : 0 < a) (hne : ∃ i, x i ≠ a)
    (hmean : Gamma.avg (fun i ↦ log (x i) - log a) = 0) :
    ∃! q : ℝ × ℝ, q ∈ Gamma.domain ∧
      IsMaxOn (fun r ↦ logLik x r.1 r.2 a) Gamma.domain q
```

**`UnivariateML.Shape2TrGamma.held_scale_constant_concentration`**

Positive constants at or above scale give unbounded likelihood. Both shapes free. [Lean source](../../lean/UnivariateML/Family/Shape2TrGamma.lean#L200)

```lean
theorem held_scale_constant_concentration (hn : 0 < n) {v a : ℝ}
    (ha : 0 < a) (hva : a ≤ v) :
    Tendsto (fun p ↦ logLik (fun _ : Fin n ↦ v) (exp (p * (log v - log a))) p a)
      atTop atTop
```

### invtrgamma

**`UnivariateML.Shape2InvTrGamma.held_scale_pareto_path`**

The reciprocal fixed-product path gives the Pareto trace with the full contact penalty. Canonical fixed-product path. [Lean source](../../lean/UnivariateML/Family/Shape2InvTrGamma.lean#L55)

```lean
theorem held_scale_pareto_path {x : Fin n → ℝ} {a d : ℝ}
    (ha : 0 < a) (hd : 0 < d) (hx : ∀ i, a ≤ x i) :
    Tendsto (fun q : ℝ ↦ logLik x (d * q) q⁻¹ a) (𝓝[>] 0)
      (𝓝 (log d - Gamma.avgLog x + d * Gamma.avg (fun i ↦ log a - log (x i)) -
        Gamma.avg (fun i ↦ if x i = a then 1 else 0)))
```

**`UnivariateML.Shape2InvTrGamma.held_scale_pareto_trace_beaten`**

Each positive-rate held-endpoint Pareto trace lies strictly below a finite point. Samples on the Pareto support, including contact. [Lean source](../../lean/UnivariateML/Family/Shape2InvTrGamma.lean#L69)

```lean
theorem held_scale_pareto_trace_beaten {x : Fin n → ℝ} {a d : ℝ}
    (ha : 0 < a) (hd : 0 < d) (hx : ∀ i, a ≤ x i) :
    ∃ k > 0, ∃ p > 0,
      log d - Gamma.avgLog x + d * Gamma.avg (fun i ↦ log a - log (x i)) -
        Gamma.avg (fun i ↦ if x i = a then 1 else 0) < logLik x k p a
```

**`UnivariateML.Shape2InvTrGamma.held_scale_centered_mle`**

On the noncollapsed zero-mean oriented log-residual slice, the inverse fit is unique. All-contact data instead concentrate. [Lean source](../../lean/UnivariateML/Family/Shape2InvTrGamma.lean#L41)

```lean
theorem held_scale_centered_mle (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {a : ℝ} (ha : 0 < a) (hne : ∃ i, x i ≠ a)
    (hmean : Gamma.avg (fun i ↦ log a - log (x i)) = 0) :
    ∃! q : ℝ × ℝ, q ∈ Gamma.domain ∧
      IsMaxOn (fun r ↦ logLik x r.1 r.2 a) Gamma.domain q
```

**`UnivariateML.Shape2InvTrGamma.held_scale_constant_concentration`**

Positive constants at or below scale give unbounded likelihood. Both shapes free. [Lean source](../../lean/UnivariateML/Family/Shape2InvTrGamma.lean#L82)

```lean
theorem held_scale_constant_concentration (hn : 0 < n) {v a : ℝ}
    (hv : 0 < v) (hva : v ≤ a) :
    Tendsto (fun p ↦ logLik (fun _ : Fin n ↦ v) (exp (p * (log a - log v))) p a)
      atTop atTop
```

### betapr

**`UnivariateML.Shape2BetaPrime.beta_logistic_loss_geometry`**

The fixed-beta-shape residual loss is strictly convex with a linear lower bound. Log-coordinate geometry also used by GB2. [Lean source](../../lean/UnivariateML/Family/Shape2BetaPrime.lean#L46)

```lean
theorem beta_logistic_loss_geometry {p q : ℝ} (hp : 0 < p) (hq : 0 < q) :
    StrictConvexOn ℝ univ (logisticLoss p q) ∧
      ∀ z, min p q * |z| ≤ logisticLoss p q z
```

**`UnivariateML.Shape2BetaPrime.held_shapes_scale_mle`**

With both beta shapes held the positive scale is unique for every nonempty positive sample. [Lean source](../../lean/UnivariateML/Family/Shape2BetaPrime.lean#L77)

```lean
theorem held_shapes_scale_mle (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {p q : ℝ} (hp : 0 < p) (hq : 0 < q) :
    ∃! s, s ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun t ↦ BetaPrime.logLik x t (p, q)) (Ioi 0) s
```

**`UnivariateML.Shape2BetaPrime.held_first_constant_gamma`**

For positive constants the optimized gamma face strictly beats all finite fits with lower beta shape held. Constant branch. [Lean source](../../lean/UnivariateML/Family/Shape2BetaPrime.lean#L228)

```lean
theorem held_first_constant_gamma (hn : 0 < n) {t p : ℝ} (ht : 0 < t) (hp : 0 < p) :
    let x := fun _ : Fin n ↦ t
    let V := (n : ℝ) * Gamma.logDensity p (p / t) t
    IsLUB {y | ∃ s > 0, ∃ q > 0, BetaPrime.logLik x s (p, q) = y} V ∧
      ∀ s > 0, ∀ q > 0, BetaPrime.logLik x s (p, q) < V
```

**`UnivariateML.Shape2BetaPrime.held_second_constant_inverse_gamma`**

For positive constants the optimized inverse-gamma face strictly beats all finite fits with upper beta shape held. Constant branch. [Lean source](../../lean/UnivariateML/Family/Shape2BetaPrime.lean#L253)

```lean
theorem held_second_constant_inverse_gamma (hn : 0 < n) {t q : ℝ}
    (ht : 0 < t) (hq : 0 < q) :
    let x := fun _ : Fin n ↦ t
    let V := (n : ℝ) * (Gamma.logDensity q (q * t) t⁻¹ - 2 * log t)
    IsLUB {y | ∃ s > 0, ∃ p > 0, BetaPrime.logLik x s (p, q) = y} V ∧
      ∀ s > 0, ∀ p > 0, BetaPrime.logLik x s (p, q) < V
```

### gev

**`UnivariateML.Shape2GEV.held_minimum_half_bound`**

At exact half contact the joint upper bound is saturated only at the closed reverse-exponential endpoint with equal positive residuals. Fitted shape interval and exact half contact. [Lean source](../../lean/UnivariateML/Family/Shape2GEV.lean#L144)

```lean
theorem held_minimum_half_bound (hn : 0 < n) {x : Fin n → ℝ} {m : ℝ}
    (hm : ∀ i, m ≤ x i)
    (hc : n = 2 * (Finset.univ.filter (fun i ↦ x i = m)).card)
    {ξ s : ℝ} (hξ : ξ ∈ Icc (-1 : ℝ) 1) (hs : (m, s) ∈ GEV.domain x ξ) :
    GEV.logLik x ξ (m, s) ≤ halfValue x m ∧
      (GEV.logLik x ξ (m, s) = halfValue x m ↔
        ξ = -1 ∧ ∀ i, m < x i → x i - m = s)
```

**`UnivariateML.Shape2GEV.held_minimum_half_classification`**

The exact half-contact trace is a supremum and attains precisely for equal positive residuals. Fitted shape interval and exact half contact. [Lean source](../../lean/UnivariateML/Family/Shape2GEV.lean#L227)

```lean
theorem held_minimum_half_classification (hn : 0 < n) {x : Fin n → ℝ} {m : ℝ}
    (hm : ∀ i, m ≤ x i)
    (hc : n = 2 * (Finset.univ.filter (fun i ↦ x i = m)).card) :
    let D : Set (ℝ × ℝ) := {q | q.1 ∈ Icc (-1 : ℝ) 1 ∧ (m, q.2) ∈ GEV.domain x q.1}
    let L := fun q : ℝ × ℝ ↦ GEV.logLik x q.1 (m, q.2)
    IsLUB (L '' D) (halfValue x m) ∧
      ((∃ q ∈ D, IsMaxOn L D q) ↔ ∃ s > 0, ∀ i, m < x i → x i - m = s)
```

### frechet

**`UnivariateML.Shape2Frechet.held_mu_classification`**

The held-threshold inverse-Weibull transfer gives a unique shape and scale on nonconstant positive excesses and divergence otherwise. [Lean source](../../lean/UnivariateML/Family/Shape2Frechet.lean#L37)

```lean
theorem held_mu_classification (hn : 0 < n) {t : Fin n → ℝ} (ht : ∀ i, 0 < t i) :
    ((∃ i j, t i ≠ t j) →
      ∃! p : ℝ × ℝ, p ∈ Weibull.domain ∧
        IsMaxOn (fun q ↦ Frechet.logLik t q.1 0 q.2) Weibull.domain p) ∧
    ((∀ i j, t i = t j) →
      ∃ s > 0, Tendsto (fun k ↦ Frechet.logLik t k 0 s) atTop atTop)
```

**`UnivariateML.Shape2Frechet.held_mu_sigma_classification`**

With a positive held scale and threshold, the free shape is unique off the all-contact sample and diverges otherwise. [Lean source](../../lean/UnivariateML/Family/Shape2Frechet.lean#L63)

```lean
theorem held_mu_sigma_classification (hn : 0 < n) {t : Fin n → ℝ}
    (ht : ∀ i, 0 < t i) {s : ℝ} (hs : 0 < s) :
    ((∃ i, t i ≠ s) →
      ∃! k, k ∈ Ioi (0 : ℝ) ∧
        IsMaxOn (fun q ↦ Frechet.logLik t q 0 s) (Ioi 0) k) ∧
    ((∀ i, t i = s) → Tendsto (fun k ↦ Frechet.logLik t k 0 s) atTop atTop)
```

**`UnivariateML.Shape2Frechet.held_sigma_constant_concentration`**

Positive held scale allows a threshold giving unbounded shape on a constant sample. Constant branch. [Lean source](../../lean/UnivariateML/Family/Shape2Frechet.lean#L77)

```lean
theorem held_sigma_constant_concentration (hn : 0 < n) {s m : ℝ} (hs : 0 < s) :
    (m - s < m) ∧
      Tendsto (fun k ↦ Frechet.logLik (fun _ : Fin n ↦ 0) k (m - (m - s)) s) atTop atTop
```

### gb2

**`UnivariateML.Shape2GB2.held_scale_power_upper_mle`**

Fixing scale, power and the upper beta shape leaves a unique lower shape for every positive sample. [Lean source](../../lean/UnivariateML/Family/Shape2GB2.lean#L48)

```lean
theorem held_scale_power_upper_mle (hn : 0 < n) (x : Fin n → ℝ)
    {s b q : ℝ} (_hx : ∀ i, 0 < x i) (_hs : 0 < s) (_hb : 0 < b) (hq : 0 < q) :
    ∃! p, p ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun t ↦ logLik x s b (t, q)) (Ioi 0) p
```

**`UnivariateML.Shape2GB2.held_scale_power_lower_mle`**

Fixing scale, power and the lower beta shape leaves a unique upper shape for every positive sample. [Lean source](../../lean/UnivariateML/Family/Shape2GB2.lean#L57)

```lean
theorem held_scale_power_lower_mle (hn : 0 < n) (x : Fin n → ℝ)
    {s b p : ℝ} (_hx : ∀ i, 0 < x i) (_hs : 0 < s) (_hb : 0 < b) (hp : 0 < p) :
    ∃! q, q ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun t ↦ logLik x s b (p, t)) (Ioi 0) q
```

**`UnivariateML.Shape2GB2.held_scale_power_classification`**

With scale and power held, the beta shape pair is unique on nonconstant data and has unbounded likelihood on constants. [Lean source](../../lean/UnivariateML/Family/Shape2GB2.lean#L66)

```lean
theorem held_scale_power_classification (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {s b : ℝ} (hs : 0 < s) (hb : 0 < b) :
    ((∃ i j, x i ≠ x j) →
      ∃! sh, sh ∈ Beta.domain ∧ IsMaxOn (logLik x s b) Beta.domain sh) ∧
    ((∀ i j, x i = x j) → ¬ BddAbove (logLik x s b '' Beta.domain))
```

**`UnivariateML.Shape2GB2.constant_concentration`**

Increasing power at scale equal to the common observation gives concentration at fixed beta shapes. Constant branch. [Lean source](../../lean/UnivariateML/Family/Shape2GB2.lean#L96)

```lean
theorem constant_concentration (hn : 0 < n) {t : ℝ} (ht : 0 < t)
    (sh : ℝ × ℝ) :
    Tendsto (fun b ↦ logLik (fun _ : Fin n ↦ t) t b sh) atTop atTop
```

**`UnivariateML.Shape2GB2.held_beta_shapes_mle`**

With the two beta shapes held, power and scale maximize uniquely on nonconstant positive data. Nonconstant branch. [Lean source](../../lean/UnivariateML/Family/Shape2GB2.lean#L109)

```lean
theorem held_beta_shapes_mle {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) {p q : ℝ} (hp : 0 < p) (hq : 0 < q) :
    ∃! u : ℝ × ℝ, u ∈ Beta.domain ∧
      IsMaxOn (fun v ↦ logLik x v.2 v.1 (p, q)) Beta.domain u
```

**`UnivariateML.Shape2GB2.all_free_constant_unbounded`**

All-free constants are unbounded above on the admissible domain. Constant branch. [Lean source](../../lean/UnivariateML/Family/Shape2GB2.lean#L145)

```lean
theorem all_free_constant_unbounded (hn : 0 < n) {t : ℝ} (ht : 0 < t) :
    ¬ BddAbove {v | ∃ s > 0, ∃ b > 0, ∃ sh ∈ Beta.domain,
      logLik (fun _ : Fin n ↦ t) s b sh = v}
```

### burr

**`UnivariateML.Shape2Burr.held_scale_first_classification`**

At fixed scale and first shape the optimum in power is unique off the all-contact sample, which instead concentrates. [Lean source](../../lean/UnivariateML/Family/Shape2Burr.lean#L32)

```lean
theorem held_scale_first_classification (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {k s : ℝ} (hk : 0 < k) (hs : 0 < s) :
    ((∃ i, x i ≠ s) →
      ∃! b, b ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun b ↦ Burr.logLik x b k s) (Ioi 0) b) ∧
    ((∀ i, x i = s) → Tendsto (fun b ↦ Burr.logLik x b k s) atTop atTop)
```

**`UnivariateML.Shape2Burr.held_power_exact_tie`**

Every positive held power admits an exact sample with precisely two finite maxima above the Weibull face. Uses the powered 21-point Lomax crossing. [Lean source](../../lean/UnivariateML/Family/Shape2Burr.lean#L80)

```lean
theorem held_power_exact_tie {b : ℝ} (hb : 0 < b) :
    ∃ x : Fin 21 → ℝ, (∀ i, 0 < x i) ∧
      ∃ p q : ℝ × ℝ, p ≠ q ∧
        {v ∈ Lomax.domain | IsMaxOn (fun r : ℝ × ℝ ↦ Burr.logLik x b r.1 r.2)
          Lomax.domain v} = {p, q} ∧
        Burr.bestWeibull x b < Burr.logLik x b p.1 p.2
```

### invburr

**`UnivariateML.Shape2InvBurr.all_free_failure`**

Constants diverge. When nonconstant data have no finite fit, the maximum of the optimized power and inverse-Weibull faces is the strict supremum. [Lean source](../../lean/UnivariateML/Family/Shape2InvBurr.lean#L166)

```lean
theorem all_free_failure (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (j : Fin n) (hj : ∀ i, x i ≤ x j) :
    let F := fun p : ℝ × ℝ × ℝ ↦ logLik x p.1 p.2.1 p.2.2 / n
    let y := BurrFree.logExcess (Transfer.reciprocal x) j
    ((∀ i, x i = x j) → ¬ BddAbove (F '' BurrFree.physicalDomain)) ∧
    ((∃ i k, x i ≠ x k) →
      ∃ c > 0, ∃ b, (∀ d > 0, ∀ t, BurrFree.weibullLik y d t ≤ BurrFree.weibullLik y c b) ∧
        ((¬ ∃ p ∈ BurrFree.physicalDomain, IsMaxOn F BurrFree.physicalDomain p) →
          let B := max (BurrFree.paretoValue y) (BurrFree.weibullLik y c b) -
            BurrHeld.avg (fun i ↦ log (x i))
          IsLUB (F '' BurrFree.physicalDomain) B ∧
            ∀ p ∈ BurrFree.physicalDomain, F p < B))
```

**`UnivariateML.Shape2InvBurr.held_scale_attains_iff`**

A held-scale finite fit exists exactly on nonconstant samples with an observation above scale. [Lean source](../../lean/UnivariateML/Family/Shape2InvBurr.lean#L64)

```lean
theorem held_scale_attains_iff (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {s : ℝ} (hs : 0 < s) :
    (∃ p ∈ BurrHeld.shapeDomain,
      IsMaxOn (fun q : ℝ × ℝ ↦ logLik x q.2 q.1 s) BurrHeld.shapeDomain p) ↔
      (∃ i j, x i ≠ x j) ∧ ∃ i, s < x i
```

**`UnivariateML.Shape2InvBurr.held_scale_constant_path`**

For a constant sample at or above scale a diverging power and its conditional first shape give unbounded likelihood. Constant branch. [Lean source](../../lean/UnivariateML/Family/Shape2InvBurr.lean#L246)

```lean
theorem held_scale_constant_path (hn : 0 < n) {v s : ℝ} (hs : 0 < s) (hsv : s ≤ v) :
    let k := fun b : ℝ ↦ (log (1 + (s / v) ^ b))⁻¹
    (∀ b, 0 < k b) ∧ Tendsto (fun b ↦ logLik (fun _ : Fin n ↦ v) (k b) b s) atTop atTop
```

**`UnivariateML.Shape2InvBurr.held_scale_first_attains_iff`**

At fixed scale and first shape a finite fit exists exactly off the all-contact sample. [Lean source](../../lean/UnivariateML/Family/Shape2InvBurr.lean#L30)

```lean
theorem held_scale_first_attains_iff (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {k s : ℝ} (hk : 0 < k) (hs : 0 < s) :
    (∃ b ∈ Ioi (0 : ℝ), IsMaxOn (fun b ↦ logLik x k b s) (Ioi 0) b) ↔
      ∃ i, x i ≠ s
```

**`UnivariateML.Shape2InvBurr.held_first_attains_iff`**

At fixed first shape a finite fit exists exactly for nonconstant positive data. [Lean source](../../lean/UnivariateML/Family/Shape2InvBurr.lean#L44)

```lean
theorem held_first_attains_iff (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {k : ℝ} (hk : 0 < k) :
    (∃ p ∈ BurrHeld.shapeDomain,
      IsMaxOn (fun q : ℝ × ℝ ↦ logLik x k q.1 q.2) BurrHeld.shapeDomain p) ↔
      ∃ i j, x i ≠ x j
```

**`UnivariateML.Shape2InvBurr.held_power_exact_tie`**

The exact reciprocal 21-point Lomax crossing has exactly two maxima above the inverse-Weibull face at power one. [Lean source](../../lean/UnivariateML/Family/Shape2InvBurr.lean#L109)

```lean
theorem held_power_exact_tie :
    ∃ a ∈ Ioo ((2 : ℝ) ^ 512)⁻¹ ((2 : ℝ) ^ 32)⁻¹,
      let x : Fin 21 → ℝ :=
        fun i ↦ (if i.val = 0 then a else if i.val ≤ 10 then (1 : ℝ) else 100)⁻¹
      (∀ i, 0 < x i) ∧ ∃ p q : ℝ × ℝ, p.2 < q.2 ∧
        {v ∈ Lomax.domain | IsMaxOn (fun r : ℝ × ℝ ↦ logLik x r.1 1 r.2)
          Lomax.domain v} = {p, q} ∧
        Transfer.InvPareto.bestInvExp x < logLik x p.1 1 p.2
```
