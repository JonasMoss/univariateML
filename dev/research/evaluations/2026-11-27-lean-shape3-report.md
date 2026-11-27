# Lean shape families, second pass

The new modules contain 46 checked public theorems. They establish 24 of the 68 main claims at the requested scope. The other 44 main claims have an open entry below. The numbered and scoped claims from the two coverage reviews have their own ledger. Coverage grades in the new atlas fragment are blank for independent review.

## Source discrepancies

- **Fréchet with held scale.** The uniqueness report and atlas describe the crossing using `(0,1,r_S)`. The cited interval certificate uses three zeros, four copies of `1/20` and one `r_S`. This identifies a mismatch between the claim and its citation. The new proof establishes compactness and conditional location uniqueness and leaves the crossing open.
- **Generalized gamma with moving mode.** In the positive or negative large-shape path, convergence of the mode and of the rate leaves a contact term free. The supported trace loses the limiting scaled exponential once per tied endpoint observation. The full endpoint density is realized when that term tends to zero. For example, writing `k = Q⁻²`, fixing oriented rate `A > 0`, and approaching mode `μ₀` via `μ(k) = μ₀ − ε k log(1/k)/A` produces a unit log penalty at contact in orientation `ε ∈ {−1, 1}`.
- **GEV half-collapse path.** The lower support endpoint in the checked open-support path tends to the sample minimum from below. The atlas wording “at the sample minimum” should be read as a limit for that path.

## Main claims

The last column records the reduction or Lean proof used for each target. Open entries identify the remaining proof obligation. The almost-everywhere bridge is assigned to the separate `Generic/UniqueAE.lean` job.

| Family | Held | Question | State | Proof effort and remaining obligation |
|---|---|---|---|---|
| gengamma | all free | uniqueness | Open | Checked three-level signed and signed/normal separation and the arbitrary-weight signed two-level profile bound. Still need the raw-sample transfer of that bound, the unequal-count normal comparison, and the measure bridge from UniqueAE. |
| gengamma | Q+sigma | failure | Checked | Derived a two-sided linear residual minorant, including the normal chart. |
| gengamma | mu | criterion | Checked | Derived a bounded-shape compact strip and small-shape rate bounds. Used ultrafilter compactness for arbitrary competitive escapes, recovered the face trace, and included the normal and stationary candidates. |
| gengamma | mu | failure | Checked | Derived a bounded-shape compact strip and small-shape rate bounds. Used ultrafilter compactness for arbitrary competitive escapes, recovered the face trace, and included the normal and stationary candidates. |
| gengamma | mu | uniqueness | Open | Checked the exact symmetric singleton and three-level finite-score separation. The signed profiled score needs nonzero near-zero and far-tail collars to make the stationary lists finite, and the almost-everywhere statement needs UniqueAE. |
| gengamma | mu+sigma | criterion | Checked | Checked the real-argument Stirling limit and Taylor seam, then combined continuity with the signed shape tails. |
| gengamma | mu+sigma | failure | Checked | Checked the real-argument Stirling limit and Taylor seam, then combined continuity with the signed shape tails. |
| gengamma | mu+sigma | uniqueness | Open | Checked the exact all-contact singleton by strict real Stirling error and the finite-score separation. The remaining strata need UniqueAE. |
| gengamma | sigma | criterion | Checked | Bounded the peak at large absolute shape and confined location uniformly on the remaining shape strip. |
| gengamma | sigma | failure | Checked | Bounded the peak at large absolute shape and confined location uniformly on the remaining shape strip. |
| gengamma | sigma | uniqueness | Open | Checked finite-score separation and uniform shape tails. The source’s (1/4,4) construction still needs a formal finite witness strictly above the normal fit and the open-set persistence argument, plus UniqueAE. |
| frechet | all free | criterion | Open | Joined the majority and exact-half cases. The held-scale tail argument cannot be transferred to free scale because the positive normalized gap can shrink along the Gumbel scaling. The strict-sub-half compactification needs a uniform two-nuisance Gumbel bound and the optimized-face comparison at equality. |
| frechet | all free | failure | Open | Joined the majority and exact-half cases. The held-scale tail argument cannot be transferred to free scale because the positive normalized gap can shrink along the Gumbel scaling. The strict-sub-half compactification needs a uniform two-nuisance Gumbel bound and the optimized-face comparison at equality. |
| frechet | all free | uniqueness | Open | Checked uniqueness of the held-coefficient nuisance optimum. It does not compare shape one with other positive reciprocal shapes. The two-level strict interior comparison and all-free regional height crossing remain open, along with the seven-level incidence input. |
| frechet | lambda+sigma | criterion | Checked | Compared tilted reciprocal moments, proved a strict score crossing, and checked the unique root with ties and constants included. |
| frechet | lambda+sigma | failure | Checked | Derived a near-contact loss from log ≤ linear and a uniform right-tail bound at held shape and scale. |
| frechet | lambda+sigma | uniqueness | Checked | Compared tilted reciprocal moments, proved a strict score crossing, and checked the unique root with ties and constants included. |
| frechet | sigma | criterion | Checked | Bounded the conditional location root, used a positive gap to control the shape tail, then localized all bounded-shape competitive values. The constant path holds location at the observation minus scale. |
| frechet | sigma | failure | Checked | Bounded the conditional location root, used a positive gap to control the shape tail, then localized all bounded-shape competitive values. The constant path holds location at the observation minus scale. |
| frechet | sigma | uniqueness | Open | Checked unique conditional location and compact confinement. The interval certificate must still exclude the separator and certify opposite signs for the two regional heights. The cited sample differs from the atlas sample. The five-level score-incidence proof remains open. |
| fatigue | alpha | criterion | Checked | Combined the minimum and a positive gap to get the uniform square-root penalty, built compact rectangles, and joined the constant collapse. |
| fatigue | alpha | failure | Checked | Combined the minimum and a positive gap to get the uniform square-root penalty, built compact rectangles, and joined the constant collapse. |
| fatigue | alpha | uniqueness | Open | The joint escape bound gives compactness. Clearing the two rational stationarity equations gives a semialgebraic set on the positive quadrant. A checked finiteness theorem for its semialgebraically connected components and constancy along piecewise smooth paths are missing, as is the incidence-to-measure step. |
| fatigue | alpha+beta | uniqueness | Open | Checked both pointwise cases by the decreasing damped score and a monotone positive-root equation for constants. The general stationary polynomial, finite retention and three-level incidence remain open. |
| fatigue | alpha+mu | uniqueness | Open | Checked the exact constant singleton and reciprocal pair. The nonconstant rational-root comparison, small-spacing persistence, and three-level measure conclusion remain open. |
| fatigue | beta | criterion | Open | Reduced the squared profiled likelihood to products in the threshold distance using the unique conditional shape. At d→0 the reciprocal part of Q contributes d⁻¹ and each minimum factor contributes d⁻³, giving the candidate exponent n−3j. The Lean limit with its positive leading coefficient and the equality-branch positive-root comparison remain open. |
| fatigue | beta | failure | Open | Reduced the squared profiled likelihood to products in the threshold distance using the unique conditional shape. At d→0 the reciprocal part of Q contributes d⁻¹ and each minimum factor contributes d⁻³, giving the candidate exponent n−3j. The Lean limit with its positive leading coefficient and the equality-branch positive-root comparison remain open. |
| fatigue | beta | uniqueness | Open | The conditional shape is unique at each fixed distance. Need the nonzero cleared stationary numerator and proof that the (0,1,1) polynomial has exactly one positive root with an attained fit, plus the five-level incidence input. |
| gev | all free | criterion | Open | The existing positive-shape transfer gives the majority path. The sharp held-location half comparison only applies when μ equals the minimum, so it does not bound free μ on all fitted shapes. A global half-pair bound with the balanced reverse-exponential equality condition and strict-sub-half compactness remain open. |
| gev | all free | failure | Open | The existing positive-shape transfer gives the majority path. The sharp held-location half comparison only applies when μ equals the minimum, so it does not bound free μ on all fitted shapes. A global half-pair bound with the balanced reverse-exponential equality condition and strict-sub-half compactness remain open. |
| gev | all free | uniqueness | Open | The fixed-shape nuisance solutions give candidates. Need verified regional height signs and exclusions over all nuisance optima for the exact crossing, and the seven-level finite/face incidence input. |
| gev | mu | criterion | Open | Checked exact half and majority. The held-scale compactness argument bounds location at fixed scale and does not bound variable scale at held location. Need the uniform negative-residual and strict-sub-half small-scale estimates, also at closed reverse-exponential contact. |
| gev | mu | failure | Checked | Joined the existing majority collapse and the sharp half-contact comparison with its equality case. |
| gev | mu | uniqueness | Open | Checked the half-contact equality case. The t_L construction needs a verified separation of the endpoint and interior regional maxima with the separator excluded, plus the five-level incidence input. |
| gev | sigma | failure | Checked | Joined the compact superlevels and full fit with the closed endpoint’s own attainable optimum. |
| gev | sigma | uniqueness | Open | Checked compactness and the closed endpoint fit. The competing interior maximum needs the verified regional height crossing and exclusions, plus the five-level incidence input. |
| pgw | all free | criterion | Open | The two known paths give reachable lower bounds. To turn them into a compactified comparison still need the upper bound for moving scale near the threshold, an optimized Gompertz-seam continuity bound, and signed exterior collars for the stationary list. No candidate count follows from reachability. |
| pgw | all free | failure | Open | The two known paths give reachable lower bounds. To turn them into a compactified comparison still need the upper bound for moving scale near the threshold, an optimized Gompertz-seam continuity bound, and signed exterior collars for the stationary list. No candidate count follows from reachability. |
| pgw | all free | uniqueness | Open | The two known paths give reachable lower bounds. To turn them into a compactified comparison still need the upper bound for moving scale near the threshold, an optimized Gompertz-seam continuity bound, and signed exterior collars for the stationary list. No candidate count follows from reachability. |
| pgw | scale | failure | Checked | Joined constant concentration and the strict threshold supremum, including the density factor on contacts. |
| pgw | scale | uniqueness | Open | Checked the three sample branches and used the unique conditional exponent. The induced shape score still needs a nonvanishing analytic isolation argument on the retained compact interval and five-level score incidence. |
| pgw | shape | criterion | Open | The prior equivalence compares every stationary point with Gompertz. That does not show a finite retained list. Need the eventual sign of the seam derivative, including vanishing leading coefficients, and the five-level incidence input. |
| pgw | shape | uniqueness | Open | The prior equivalence compares every stationary point with Gompertz. That does not show a finite retained list. Need the eventual sign of the seam derivative, including vanishing leading coefficients, and the five-level incidence input. |
| expweibull | all free | criterion | Open | Checked strict curvature in the conditional scale chart. Optimizing over exponent first leaves a profile with a moving H term. Need bounds uniform over the optimizing scale at both shape ends, including moving contacts, to match both optimized accessible face values. |
| expweibull | all free | failure | Open | Checked strict curvature in the conditional scale chart. Optimizing over exponent first leaves a profile with a moving H term. Need bounds uniform over the optimizing scale at both shape ends, including moving contacts, to match both optimized accessible face values. |
| expweibull | all free | uniqueness | Open | The unique scale/exponent fit at each fixed shape reduces the finite tie to comparing scalar shape maxima. The exact deformation still needs a uniform separator exclusion, both exterior exclusions, and opposing regional height signs above both faces. The seven-level incidence input remains open. |
| expweibull | scale | failure | Checked | Joined the constant path, power supremum and finite attainment branches. |
| expweibull | scale | uniqueness | Open | The earlier stationary-root set calculation gives the finite candidate comparison. The missing obligation is the five-level finite-score incidence bound and UniqueAE. |
| expweibull | shape | uniqueness | Checked | Proved both curvature inequalities for the log-CDF sum, followed by strict concavity and transfer to scale and exponent. |
| invtrgamma | all free | criterion | Open | The held-scale proof controls the shape at each power with fixed log excesses. It does not extend directly to moving scale. Need optimized signed-profile coercive bounds, including the normal seam and supported endpoint collar, and finite root isolation in the balanced moment case. |
| invtrgamma | all free | failure | Open | The held-scale proof controls the shape at each power with fixed log excesses. It does not extend directly to moving scale. Need optimized signed-profile coercive bounds, including the normal seam and supported endpoint collar, and finite root isolation in the balanced moment case. |
| invtrgamma | all free | uniqueness | Open | Checked three-level finite-score separation and both constant concentration routes. The exact replicated-outlier tie needs uniform regional profile bounds above both faces and a crossing. The low-level attainment exclusions and UniqueAE remain separate. |
| invtrgamma | scale | criterion | Checked | Profiled the gamma shape, bounded the power tails, and joined constant paths with the strictly defeated supported trace. The inverse proof uses the reciprocal Jacobian. |
| invtrgamma | scale | failure | Checked | Profiled the gamma shape, bounded the power tails, and joined constant paths with the strictly defeated supported trace. The inverse proof uses the reciprocal Jacobian. |
| invtrgamma | scale | uniqueness | Open | Checked finite-score separation and attainment, and reused the centered conditional singleton. The analytic optimized-shape stationary list needs exterior score signs and isolation of all roots, plus UniqueAE. |
| trgamma | all free | criterion | Open | The held-scale proof controls the shape at each power with fixed log excesses. It does not extend directly to moving scale. Need optimized signed-profile coercive bounds, including the normal seam and supported endpoint collar, and finite root isolation in the balanced moment case. |
| trgamma | all free | failure | Open | The held-scale proof controls the shape at each power with fixed log excesses. It does not extend directly to moving scale. Need optimized signed-profile coercive bounds, including the normal seam and supported endpoint collar, and finite root isolation in the balanced moment case. |
| trgamma | all free | uniqueness | Open | Checked three-level finite-score separation and both constant concentration routes. The exact replicated-outlier tie needs uniform regional profile bounds above both faces and a crossing. The low-level attainment exclusions and UniqueAE remain separate. |
| trgamma | scale | criterion | Checked | Profiled the gamma shape, bounded the power tails, and joined constant paths with the strictly defeated supported trace. The inverse proof uses the reciprocal Jacobian. |
| trgamma | scale | failure | Checked | Profiled the gamma shape, bounded the power tails, and joined constant paths with the strictly defeated supported trace. The inverse proof uses the reciprocal Jacobian. |
| trgamma | scale | uniqueness | Open | Checked finite-score separation and attainment, and reused the centered conditional singleton. The analytic optimized-shape stationary list needs exterior score signs and isolation of all roots, plus UniqueAE. |
| invburr | all free | failure | Open | The prior reciprocal proof identifies the two possible optimal face heights, and the majority-maximum class gives a strict comparison. The convex-order transfer still needs a verified reference-sample winner and a checked implication from finite integrated-rank comparisons to convex order. The inverse-Weibull exclusion requires the uniform dual/seam comparison. |
| invburr | all free | uniqueness | Open | Reciprocity transports maximizing sets. The replicated forward Burr crossing must first be checked, with a uniform separator and tail exclusion, and the four-level finite/face score separation remains open. |
| invburr | scale | failure | Checked | Joined the two nonattainment branches and the finite-fit criterion, retaining the trace at tied contacts. |
| invburr | shape2 | uniqueness | Open | Checked the attained two-level singleton by reciprocal transport. The exact reciprocal tie already exists in round five. The almost-everywhere bridge and transformed finite/face score input remain open. |
| burr | all free | uniqueness | Open | Clearing the two logistic score denominators gives the rates 0, c, c′, c+c′. The four-rate zero count with degenerate coefficients and the finite/face comparison are open. The replicated exact tie additionally needs the interval exterior and crossing proof. |
| burr | shape2 | uniqueness | Open | Checked the attained two-level singleton by powering the data and transferring the entire maximizing set. The prior exact tie remains available. The measure-theoretic conclusion needs the score transfer and UniqueAE. |
| lomax | all free | uniqueness | Open | Checked two-level finite/finite and finite/exponential score separation. The prior pointwise two-level theorem and exact 21-observation tie remain available. UniqueAE is the missing bridge for other count strata. |

## Numbered paths, face comparisons and scoped cases

The coverage reports list 70 distinct keys for these families. The ledger records 34 checked this pass, 21 with earlier Lean evidence and 15 open.

| Family | Held | Key | State | Evidence | Proof effort and remaining obligation |
|---|---|---|---|---|---|
| burr | all free | `limit:3` | Open | Strict cross-face and finite-profile certificate | The known face paths and strict sample subclasses leave the simultaneous dual/chord bound and signed seam enclosure to verify. |
| burr | all free | `limit:4` | Open | Strict cross-face and finite-profile certificate | The known face paths and strict sample subclasses leave the simultaneous dual/chord bound and signed seam enclosure to verify. |
| burr | all free | `path:4` | Checked this pass | Shape3Burr.held_endpoint_pareto_path | Took the arbitrary positive product limit at a fixed scale and retained the tied contact factors. |
| burr | scale | `limit:1` | Open | JoinsBurr.constant_path and BurrHeld.scale_constant_unbounded | Checked which earlier constant path survives each hold. A single theorem matching all scale and power alternatives under the indexed path remains to be joined. |
| burr | scale+shape1 | `limit:1` | Earlier Lean | UnivariateML.Shape2Burr.held_scale_first_classification | Reused the earlier independently graded full theorem. |
| burr | scale+shape1 | `path:1` | Earlier Lean | UnivariateML.Shape2Burr.held_scale_first_classification | Reused the earlier independently graded full theorem. |
| burr | shape1 | `limit:1` | Open | JoinsBurr.constant_path and BurrHeld.scale_constant_unbounded | Checked which earlier constant path survives each hold. A single theorem matching all scale and power alternatives under the indexed path remains to be joined. |
| burr | shape2 | `limit:2` | Open | Burr.exponential_sup_of_no_witness | The conditional exact comparison is available. The verified transformed-profile bound and arithmetic margin rule needed for the detector remain open. |
| burr | shape2 | `path:2` | Checked this pass | Shape3Burr.held_power_weibull_path | Used the log1p ratio at zero for arbitrary rescaled-scale convergence. |
| expweibull | all free | `limit:1` | Earlier Lean | UnivariateML.Shape2ExpWeibull.constant_concentration | Reused the earlier independently graded full theorem. |
| expweibull | all free | `path:1` | Earlier Lean | UnivariateML.Shape2ExpWeibull.constant_concentration | Reused the earlier independently graded full theorem. |
| expweibull | exponent | `limit:1` | Earlier Lean | UnivariateML.Shape2ExpWeibull.constant_concentration | Reused the earlier independently graded full theorem. |
| expweibull | exponent | `path:1` | Earlier Lean | UnivariateML.Shape2ExpWeibull.constant_concentration | Reused the earlier independently graded full theorem. |
| expweibull | exponent+scale | `limit:1` | Earlier Lean | UnivariateML.Shape2ExpWeibull.held_exponent_scale | Reused the earlier independently graded full theorem. |
| expweibull | exponent+scale | `path:1` | Earlier Lean | UnivariateML.Shape2ExpWeibull.held_exponent_scale | Reused the earlier independently graded full theorem. |
| expweibull | scale | `limit:1` | Earlier Lean | UnivariateML.Shape2ExpWeibull.held_scale_constant_concentration | Reused the earlier independently graded full theorem. |
| expweibull | scale | `limit:2` | Earlier Lean | UnivariateML.Shape2ExpWeibull.held_scale_power_supremum | Reused the earlier independently graded full theorem. |
| expweibull | scale | `limit:3` | Earlier Lean | UnivariateML.Shape2ExpWeibull.held_scale_power_supremum | Reused the earlier independently graded full theorem. |
| expweibull | scale | `path:1` | Earlier Lean | UnivariateML.Shape2ExpWeibull.held_scale_constant_concentration | Reused the earlier independently graded full theorem. |
| expweibull | scale | `path:2` | Earlier Lean | UnivariateML.Shape2ExpWeibull.held_scale_power_path | Reused the earlier independently graded full theorem. |
| expweibull | scale | `path:3` | Earlier Lean | UnivariateML.Shape2ExpWeibull.held_scale_power_path | Reused the earlier independently graded full theorem. |
| expweibull | shape | `limit:1` | Earlier Lean | UnivariateML.Shape2ExpWeibull.held_shape_constant_concentration | Reused the earlier independently graded full theorem. |
| expweibull | shape | `path:1` | Earlier Lean | UnivariateML.Shape2ExpWeibull.held_shape_constant_concentration | Reused the earlier independently graded full theorem. |
| frechet | all free | `case:strict-minimum-witness` | Open | Optimized Gumbel bound | The positive normalized gap in the held-scale proof can shrink with freely moving scale. A uniform optimized Gumbel bound and an equality-compatible compactification are missing. |
| frechet | all free | `limit:3` | Open | Optimized Gumbel bound | The positive normalized gap in the held-scale proof can shrink with freely moving scale. A uniform optimized Gumbel bound and an equality-compatible compactification are missing. |
| frechet | mu+sigma | `limit:2` | Earlier Lean | Shape2Frechet.held_mu_sigma_classification | Reused the classification at the exact held-location and scale contact. |
| frechet | mu+sigma | `path:2` | Earlier Lean | Shape2Frechet.held_mu_sigma_classification | Reused the classification at the exact held-location and scale contact. |
| frechet | sigma | `limit:2` | Checked this pass | Shape3Frechet.held_scale_classification | The all-contact constant path holds location at the observation minus scale. |
| frechet | sigma | `path:2` | Checked this pass | Shape3Frechet.held_scale_classification | The all-contact constant path holds location at the observation minus scale. |
| gengamma | all free | `path:1` | Checked this pass | Shape3GenGamma.moving_mode_supported_face | Derived both orientations with a moving mode, including the contact penalty. The zero-penalty path gives full endpoint density. |
| gengamma | all free | `path:2` | Checked this pass | Shape3GenGamma.moving_mode_supported_face | Derived both orientations with a moving mode, including the contact penalty. The zero-penalty path gives full endpoint density. |
| gev | all free | `limit:2` | Open | Global half-tie inequality | The checked half-collapse path yields a lower bound. Need a sharp upper comparison over the full fitted shape domain and its balanced-sample equality case. |
| gev | all free | `path:2` | Earlier Lean | AllFreeGEV.half_minimum_collapse_path | The support endpoint approaches the sample minimum from below. Literal equality with the open lower support endpoint puts the minimum outside the fitted support. |
| gev | mu | `case:held-minimum-half` | Checked this pass | Shape3GEV.held_location_contact_classification | Joined the majority and sharp half-contact cases with the balanced equality branch. |
| gev | mu | `limit:1` | Checked this pass | Shape3GEV.held_location_contact_classification | Joined the majority and sharp half-contact cases with the balanced equality branch. |
| invburr | all free | `limit:3` | Open | Strict cross-face and finite-profile certificate | The known face paths and strict sample subclasses leave the simultaneous dual/chord bound and signed seam enclosure to verify. |
| invburr | all free | `limit:4` | Open | Strict cross-face and finite-profile certificate | The known face paths and strict sample subclasses leave the simultaneous dual/chord bound and signed seam enclosure to verify. |
| invburr | all free | `path:1` | Open | ShapeFacesInverseBurr.held_first_constant_path, ShapeFacesInverseBurr.held_first_scale_unbounded and Shape3InvBurr.held_scale_classification | Matched the earlier reciprocal scale-free and fixed-scale contact paths and the new joined scale theorem. A single theorem combining the applicable fixed-scale and scale-free paths remains to be joined where the row asks for both. |
| invburr | all free | `path:3` | Checked this pass | Shape3Burr.inverse_held_power_weibull_path | Transported the varying-scale Weibull approach through reciprocity, with the power fixed at the limiting value. |
| invburr | all free | `path:4` | Checked this pass | Shape3Burr.inverse_held_endpoint_power_path | Transferred the fixed endpoint Pareto limit through reciprocals. |
| invburr | scale | `limit:1` | Earlier Lean | UnivariateML.Shape2InvBurr.held_scale_constant_path | Reused the earlier independently graded full theorem. |
| invburr | scale | `path:1` | Earlier Lean | UnivariateML.Shape2InvBurr.held_scale_constant_path | Reused the earlier independently graded full theorem. |
| invburr | scale | `path:5` | Checked this pass | Shape3Burr.inverse_held_endpoint_power_path | Kept held scale fixed and allowed the shape product to move. |
| invburr | scale+shape1 | `limit:1` | Open | ShapeFacesInverseBurr.held_first_constant_path, ShapeFacesInverseBurr.held_first_scale_unbounded and Shape3InvBurr.held_scale_classification | Matched the earlier reciprocal scale-free and fixed-scale contact paths and the new joined scale theorem. A single theorem combining the applicable fixed-scale and scale-free paths remains to be joined where the row asks for both. |
| invburr | scale+shape1 | `path:1` | Open | ShapeFacesInverseBurr.held_first_constant_path, ShapeFacesInverseBurr.held_first_scale_unbounded and Shape3InvBurr.held_scale_classification | Matched the earlier reciprocal scale-free and fixed-scale contact paths and the new joined scale theorem. A single theorem combining the applicable fixed-scale and scale-free paths remains to be joined where the row asks for both. |
| invburr | shape1 | `limit:1` | Open | ShapeFacesInverseBurr.held_first_constant_path, ShapeFacesInverseBurr.held_first_scale_unbounded and Shape3InvBurr.held_scale_classification | Matched the earlier reciprocal scale-free and fixed-scale contact paths and the new joined scale theorem. A single theorem combining the applicable fixed-scale and scale-free paths remains to be joined where the row asks for both. |
| invburr | shape1 | `path:1` | Open | ShapeFacesInverseBurr.held_first_constant_path, ShapeFacesInverseBurr.held_first_scale_unbounded and Shape3InvBurr.held_scale_classification | Matched the earlier reciprocal scale-free and fixed-scale contact paths and the new joined scale theorem. A single theorem combining the applicable fixed-scale and scale-free paths remains to be joined where the row asks for both. |
| invburr | shape2 | `path:2` | Checked this pass | Shape3Burr.inverse_held_power_weibull_path | Used the log1p ratio at zero for arbitrary rescaled-scale convergence. |
| invtrgamma | all free | `limit:2` | Checked this pass | Shape3TrGamma.inverse_constant_concentration_masks | Joined separate constant concentration paths for each free shape and the held-scale branches. |
| invtrgamma | all free | `path:2` | Checked this pass | Shape3TrGamma.inverse_constant_concentration_masks | Joined separate constant concentration paths for each free shape and the held-scale branches. |
| invtrgamma | scale | `limit:2` | Checked this pass | Shape3TrGamma.inverse_constant_concentration_masks | Checked both held-scale constant concentration branches. |
| invtrgamma | scale | `path:1` | Checked this pass | Shape3TrGamma.inverse_held_scale_varying_rate_trace and inverse_held_scale_trace_and_concentration | Computed the moving-rate trace and retained the exact loss at contact, then used the strict finite witness. |
| invtrgamma | scale | `path:2` | Checked this pass | Shape3TrGamma.inverse_constant_concentration_masks | Checked both held-scale constant concentration branches. |
| invtrgamma | scale+shape1 | `limit:2` | Checked this pass | Shape3TrGamma.inverse_constant_concentration_masks | Joined separate constant concentration paths for each free shape and the held-scale branches. |
| invtrgamma | scale+shape1 | `path:2` | Checked this pass | Shape3TrGamma.inverse_constant_concentration_masks | Joined separate constant concentration paths for each free shape and the held-scale branches. |
| invtrgamma | shape1 | `limit:2` | Checked this pass | Shape3TrGamma.inverse_constant_concentration_masks | Joined separate constant concentration paths for each free shape and the held-scale branches. |
| invtrgamma | shape1 | `path:2` | Checked this pass | Shape3TrGamma.inverse_constant_concentration_masks | Joined separate constant concentration paths for each free shape and the held-scale branches. |
| invtrgamma | shape2 | `limit:2` | Checked this pass | Shape3TrGamma.inverse_constant_concentration_masks | Joined separate constant concentration paths for each free shape and the held-scale branches. |
| invtrgamma | shape2 | `path:2` | Checked this pass | Shape3TrGamma.inverse_constant_concentration_masks | Joined separate constant concentration paths for each free shape and the held-scale branches. |
| trgamma | all free | `limit:2` | Checked this pass | Shape3TrGamma.constant_concentration_masks | Joined separate constant concentration paths for each free shape and the held-scale branches. |
| trgamma | all free | `path:2` | Checked this pass | Shape3TrGamma.constant_concentration_masks | Joined separate constant concentration paths for each free shape and the held-scale branches. |
| trgamma | scale | `limit:2` | Checked this pass | Shape3TrGamma.constant_concentration_masks | Checked both held-scale constant concentration branches. |
| trgamma | scale | `path:1` | Checked this pass | Shape3TrGamma.held_scale_varying_rate_trace and held_scale_trace_and_concentration | Computed the moving-rate trace and retained the exact loss at contact, then used the strict finite witness. |
| trgamma | scale | `path:2` | Checked this pass | Shape3TrGamma.constant_concentration_masks | Checked both held-scale constant concentration branches. |
| trgamma | scale+shape1 | `limit:2` | Checked this pass | Shape3TrGamma.constant_concentration_masks | Joined separate constant concentration paths for each free shape and the held-scale branches. |
| trgamma | scale+shape1 | `path:2` | Checked this pass | Shape3TrGamma.constant_concentration_masks | Joined separate constant concentration paths for each free shape and the held-scale branches. |
| trgamma | shape1 | `limit:2` | Checked this pass | Shape3TrGamma.constant_concentration_masks | Joined separate constant concentration paths for each free shape and the held-scale branches. |
| trgamma | shape1 | `path:2` | Checked this pass | Shape3TrGamma.constant_concentration_masks | Joined separate constant concentration paths for each free shape and the held-scale branches. |
| trgamma | shape2 | `limit:2` | Checked this pass | Shape3TrGamma.constant_concentration_masks | Joined separate constant concentration paths for each free shape and the held-scale branches. |
| trgamma | shape2 | `path:2` | Checked this pass | Shape3TrGamma.constant_concentration_masks | Joined separate constant concentration paths for each free shape and the held-scale branches. |

## Public Lean statements

### `UnivariateML.Shape3Burr.held_power_two_level_unique`

`dev/lean/UnivariateML/Family/Shape3Burr.lean:48`

Attained Burr fits at fixed positive power are unique whenever there are at most two observation levels.

- **burr, shape2.** Pointwise uniqueness on any attained sample with at most two levels. The exact ties and higher-level measure statement are separate.

```lean
theorem held_power_two_level_unique {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {c : ℝ} (hc : 0 < c)
    (h2 : ∃ a b : ℝ, ∀ i, x i = a ∨ x i = b)
    (he : ∃ p ∈ Lomax.domain, IsMaxOn
      (fun q : ℝ × ℝ ↦ Burr.logLik x c q.1 q.2) Lomax.domain p) :
    ∃! p, p ∈ Lomax.domain ∧ IsMaxOn
      (fun q : ℝ × ℝ ↦ Burr.logLik x c q.1 q.2) Lomax.domain p
```

### `UnivariateML.Shape3Burr.inverse_held_power_two_level_unique`

`dev/lean/UnivariateML/Family/Shape3Burr.lean:66`

The same two-level singleton conclusion holds for inverse Burr.

- **invburr, shape2.** Pointwise uniqueness on any attained sample with at most two levels. Uses reciprocal scale and reciprocal observations.

```lean
theorem inverse_held_power_two_level_unique {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {c : ℝ} (hc : 0 < c)
    (h2 : ∃ a b : ℝ, ∀ i, x i = a ∨ x i = b)
    (he : ∃ p ∈ Lomax.domain, IsMaxOn
      (fun q : ℝ × ℝ ↦ ShapeFacesInverseBurr.logLik x q.1 c q.2) Lomax.domain p) :
    ∃! p, p ∈ Lomax.domain ∧ IsMaxOn
      (fun q : ℝ × ℝ ↦ ShapeFacesInverseBurr.logLik x q.1 c q.2) Lomax.domain p
```

### `UnivariateML.Shape3Burr.held_endpoint_pareto_path`

`dev/lean/UnivariateML/Family/Shape3Burr.lean:121`

Burr has a Pareto trace for arbitrary moving positive shape products on the fixed-endpoint path. Ties at scale contribute a factor one half.

- **burr, all free.** path:4 with an exactly fixed endpoint and arbitrary limiting positive product of shapes. The fixed endpoint retains a one-half contact factor.

```lean
theorem held_endpoint_pareto_path {n : ℕ} {ι : Type*} {l : Filter ι}
    {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) {s d : ℝ} (hs : 0 < s) (hd : 0 < d)
    (hle : ∀ i, s ≤ x i) {c k : ι → ℝ} (hc : Tendsto c l atTop)
    (hkc : Tendsto (fun j ↦ k j * c j) l (𝓝 d)) (hk : ∀ᶠ j in l, 0 < k j) :
    Tendsto (fun j ↦ Burr.logLik x (c j) (k j) s) l
      (𝓝 (∑ i : Fin n, (log d - d * log (x i / s) - log (x i) -
        (if x i = s then log 2 else 0))))
```

### `UnivariateML.Shape3Burr.inverse_held_endpoint_power_path`

`dev/lean/UnivariateML/Family/Shape3Burr.lean:147`

The inverse Burr upper-endpoint power trace has the same one-half contact factor, with the orientation reversed.

- **invburr, all free.** path:4 with an exactly fixed endpoint and arbitrary limiting positive product of shapes. The fixed endpoint retains a one-half contact factor.
- **invburr, scale.** path:5 for every supported held scale. The fixed endpoint retains a one-half contact factor.

```lean
theorem inverse_held_endpoint_power_path {n : ℕ} {ι : Type*} {l : Filter ι}
    {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) {s d : ℝ} (hs : 0 < s) (hd : 0 < d)
    (hle : ∀ i, x i ≤ s) {c k : ι → ℝ} (hc : Tendsto c l atTop)
    (hkc : Tendsto (fun j ↦ k j * c j) l (𝓝 d)) (hk : ∀ᶠ j in l, 0 < k j) :
    Tendsto (fun j ↦ ShapeFacesInverseBurr.logLik x (k j) (c j) s) l
      (𝓝 (∑ i : Fin n, (log d - d * log (s / x i) - log (x i) -
        (if x i = s then log 2 else 0))))
```

### `UnivariateML.Shape3Burr.held_power_weibull_path`

`dev/lean/UnivariateML/Family/Shape3Burr.lean:170`

Held positive Burr power has its Weibull limit along every diverging first-shape path with a positive limiting rescaled scale.

- **burr, shape2.** path:2 with a moving scale and arbitrary positive limiting rescaled scale.

```lean
theorem held_power_weibull_path {n : ℕ} {ι : Type*} {l : Filter ι}
    {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) {c a : ℝ} (hc : 0 < c) (ha : 0 < a)
    {k s : ι → ℝ} (hk : Tendsto k l atTop) (hs : ∀ᶠ j in l, 0 < s j)
    (h : Tendsto (fun j ↦ s j * (k j) ^ (-c⁻¹)) l (𝓝 a)) :
    Tendsto (fun j ↦ Burr.logLik x c (k j) (s j)) l (𝓝 (Weibull.logLik x c a))
```

### `UnivariateML.Shape3Burr.inverse_held_power_weibull_path`

`dev/lean/UnivariateML/Family/Shape3Burr.lean:205`

The reciprocal Weibull path accepts arbitrary moving rescaled inverse Burr scales with a strictly positive limit.

- **invburr, shape2.** path:2 with a moving scale and arbitrary positive limiting rescaled scale.
- **invburr, all free.** path:3 with fixed positive power and arbitrary positive inverse-Weibull limiting scale.

```lean
theorem inverse_held_power_weibull_path {n : ℕ} {ι : Type*} {l : Filter ι}
    {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) {c a : ℝ} (hc : 0 < c) (ha : 0 < a)
    {k s : ι → ℝ} (hk : Tendsto k l atTop) (hs : ∀ᶠ j in l, 0 < s j)
    (h : Tendsto (fun j ↦ s j * (k j) ^ c⁻¹) l (𝓝 a)) :
    Tendsto (fun j ↦ ShapeFacesInverseBurr.logLik x (k j) c (s j)) l
      (𝓝 (Weibull.logLik (reciprocal x) c a⁻¹ + reciprocalJac x))
```

### `UnivariateML.Shape3ExpWeibull.held_scale_classification`

`dev/lean/UnivariateML/Family/Shape3ExpWeibull.lean:25`

All held-scale outcomes, with the strict upper bound on the power branch and an explicit diverging path on the constant branch.

- **expweibull, scale.** All positive samples, including all-contact and strictly-below-scale constants. The power branch includes the contact penalty.

```lean
theorem held_scale_classification {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {a : ℝ} (ha : 0 < a) :
    let L := fun p : ℝ × ℝ ↦ logLik x p.1 a p.2
    ((∃ p ∈ Weibull.domain, IsMaxOn L Weibull.domain p) ↔
      (∃ i j, x i ≠ x j) ∧ ∃ i, a < x i) ∧
    (∀ v ≥ a, (∀ i, x i = v) →
      Tendsto (fun b ↦ L (b, exp (exp (b * log (v / a))))) atTop atTop) ∧
    ((∀ i, x i ≤ a) → (∃ i, x i < a) →
      let D := ∑ i : Fin n, -log (x i / a)
      let P := heldPowerTrace x a ((n : ℝ) / D)
      IsLUB (L '' Weibull.domain) P ∧ ∀ p ∈ Weibull.domain, L p < P)
```

### `UnivariateML.Shape3ExpWeibull.held_shape_profile_strictConcave`

`dev/lean/UnivariateML/Family/Shape3ExpWeibull.lean:120`

The held-shape, exponent-profiled objective is strictly concave in the positive scale coordinate t. The additive sample constant is omitted.

- **expweibull, shape.** Strict curvature in the positive scale-profile coordinate. The next theorem transfers it to the physical fit.

```lean
theorem held_shape_profile_strictConcave {n : ℕ} (hn : 0 < n) (r : Fin n → ℝ)
    (hr : ∀ i, 0 < r i) (b : ℝ) :
    let h := fun t : ℝ ↦ (∑ i : Fin n, -log (1 - exp (-(r i) * t))) / n
    StrictConcaveOn ℝ (Ioi 0) (fun t ↦ log t - b * t + h t - log (h t))
```

### `UnivariateML.Shape3ExpWeibull.held_shape_unique_iff_nonconstant`

`dev/lean/UnivariateML/Family/Shape3ExpWeibull.lean:178`

For each positive held shape, a nonconstant positive sample has a unique scale and exponent fit. Constants have no finite maximizer.

- **expweibull, shape.** Unique finite fit precisely on nonconstant positive data.

```lean
theorem held_shape_unique_iff_nonconstant {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {b : ℝ} (hb : 0 < b) :
    (∃! p : ℝ × ℝ, p ∈ Weibull.domain ∧
      IsMaxOn (fun q : ℝ × ℝ ↦ logLik x b q.1 q.2) Weibull.domain p) ↔
      ∃ i j, x i ≠ x j
```

### `UnivariateML.Shape3Fatigue.held_alpha_attains_iff`

`dev/lean/UnivariateML/Family/Shape3Fatigue.lean:201`

With positive shape held, a finite threshold and scale maximize exactly when the sample with its minimum subtracted has a positive gap.

- **fatigue, alpha.** All nonempty samples in the gap-from-minimum chart. A positive gap is exactly nonconstancy.

```lean
theorem held_alpha_attains_iff {n : ℕ} (hn : 0 < n) {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) (hz : ∃ i, g i = 0) {a : ℝ} (ha : 0 < a) :
    let D : Set (ℝ × ℝ) := Ioi 0 ×ˢ Ioi 0
    let L := fun p : ℝ × ℝ ↦ Fatigue.logLik (fun i ↦ p.1 + g i) a p.2
    (∃ p ∈ D, IsMaxOn L D p) ↔ ∃ j, 0 < g j
```

### `UnivariateML.Shape3Fatigue.held_alpha_escape_classification`

`dev/lean/UnivariateML/Family/Shape3Fatigue.lean:233`

Held-shape constants collapse. On nonconstant samples the likelihood vanishes on escape from every compact subset of the joint chart.

- **fatigue, alpha.** Collapse on constants and cocompact escape exhaustion on nonconstants. Both threshold and scale may move.

```lean
theorem held_alpha_escape_classification {n : ℕ} (hn : 0 < n) {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) (hz : ∃ i, g i = 0) {a : ℝ} (ha : 0 < a) :
    let D : Set (ℝ × ℝ) := Ioi 0 ×ˢ Ioi 0
    ((∀ i, g i = 0) →
      Tendsto (fun d ↦ Fatigue.logLik (fun i ↦ d + g i) a d) (𝓝[>] 0) atTop) ∧
    ((∃ j, 0 < g j) →
      Tendsto (fun p : D ↦ Fatigue.logLik (fun i ↦ p.val.1 + g i) a p.val.2)
        (cocompact D) atBot)
```

### `UnivariateML.Shape3Fatigue.held_shape_threshold_constant_scales`

`dev/lean/UnivariateML/Family/Shape3Fatigue.lean:262`

At held shape and threshold, constant positive excesses give one scale when shape is at most two and two reciprocal scales above two.

- **fatigue, alpha+mu.** Exact constant-sample singleton or reciprocal pair. Nonconstant tied cases and almost-everywhere uniqueness remain separate.

```lean
theorem held_shape_threshold_constant_scales {n : ℕ} (hn : 0 < n)
    {a t : ℝ} (ha : 0 < a) (ht : 0 < t) :
    let r := a ^ 2 - 2
    let u := t * ((r - sqrt (r ^ 2 - 4)) / 2)
    let v := t * ((r + sqrt (r ^ 2 - 4)) / 2)
    let L := Fatigue.logLik (fun _ : Fin n ↦ t) a
    {b ∈ Ioi (0 : ℝ) | IsMaxOn L (Ioi 0) b} =
      if a ≤ 2 then {t} else {u, v}
```

### `UnivariateML.Shape3Fatigue.held_shape_scale_threshold_unique`

`dev/lean/UnivariateML/Family/Shape3Fatigue.lean:500`

The threshold fit at held shape and scale is a singleton for every sample when squared shape is at most one half, and for every constant sample at arbitrary positive shape.

- **fatigue, alpha+beta.** All constant samples and every sample at squared shape at most one half. The unrestricted stationary-root comparison and almost-everywhere bridge remain open.

```lean
theorem held_shape_scale_threshold_unique {n : ℕ} (hn : 0 < n)
    {g : Fin n → ℝ} (hg : ∀ i, 0 ≤ g i) (hz : ∃ i, g i = 0)
    {a b : ℝ} (ha : 0 < a) (hb : 0 < b)
    (h : a ^ 2 ≤ 1 / 2 ∨ ∀ i, g i = 0) :
    ∃! d, d ∈ Ioi (0 : ℝ) ∧ IsMaxOn
      (fun e ↦ Fatigue.logLik (fun i ↦ e + g i) a b) (Ioi 0) d
```

### `UnivariateML.Shape3Frechet.held_shape_scale_location_mle`

`dev/lean/UnivariateML/Family/Shape3Frechet.lean:187`

Every nonempty Fréchet sample, including constants, has a unique location optimum with positive shape and positive scale held.

- **frechet, lambda+sigma.** Unique feasible location for any positive held shape and scale, including all ties. Gap measured from the sample minimum.

```lean
theorem held_shape_scale_location_mle {n : ℕ} (hn : 0 < n) {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) (hz : ∃ i, g i = 0) {k s : ℝ} (hk : 0 < k) (hs : 0 < s) :
    ∃! d, d ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun e ↦ logLik g k e s) (Ioi 0) d
```

### `UnivariateML.Shape3Frechet.held_shape_scale_location_ends`

`dev/lean/UnivariateML/Family/Shape3Frechet.lean:249`

With shape and scale held the likelihood vanishes at both ends of the location chart, including when the sample consists entirely of ties.

- **frechet, lambda+sigma.** Both ends of the location interval, including constants.

```lean
theorem held_shape_scale_location_ends {n : ℕ} (hn : 0 < n) {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) (hz : ∃ i, g i = 0) {k s : ℝ} (hk : 0 < k) (hs : 0 < s) :
    Tendsto (fun d ↦ logLik g k d s) (𝓝[>] 0) atBot ∧
      Tendsto (fun d ↦ logLik g k d s) atTop atBot
```

### `UnivariateML.Shape3Frechet.held_scale_classification`

`dev/lean/UnivariateML/Family/Shape3Frechet.lean:535`

On the fitted branch with scale held, constants diverge. Every nonconstant sample has compact positive likelihood superlevels in the fitted shape and location gap, and attains a finite maximum.

- **frechet, sigma.** Collapsed constants and compact competitive superlevels for nonconstants on fitted shapes. The constant path uses the common observation minus the held scale.

```lean
theorem held_scale_classification {n : ℕ} (hn : 0 < n) {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) (hz : ∃ i, g i = 0) {s : ℝ} (hs : 0 < s) :
    let L := fun p : ℝ × ℝ ↦ logLik g p.1 p.2 s
    let D : Set (ℝ × ℝ) := Ici 1 ×ˢ Ioi 0
    ((∀ i, g i = 0) → Tendsto (fun k ↦ L (k, s)) atTop atTop) ∧
    ((∃ i, 0 < g i) → (∀ v, IsCompact (D ∩ L ⁻¹' Ici v)) ∧ ∃ p ∈ D, IsMaxOn L D p)
```

### `UnivariateML.Shape3Frechet.held_scale_attains_iff`

`dev/lean/UnivariateML/Family/Shape3Frechet.lean:585`

The fitted held-scale Fréchet likelihood attains exactly for nonconstant samples.

- **frechet, sigma.** Attainment iff nonconstant on the fitted branch.

```lean
theorem held_scale_attains_iff {n : ℕ} (hn : 0 < n) {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) (hz : ∃ i, g i = 0) {s : ℝ} (hs : 0 < s) :
    let L := fun p : ℝ × ℝ ↦ logLik g p.1 p.2 s
    let D : Set (ℝ × ℝ) := Ici 1 ×ˢ Ioi 0
    (∃ p ∈ D, IsMaxOn L D p) ↔ ∃ i j, g i ≠ g j
```

### `UnivariateML.Shape3Frechet.all_free_collapse_classification`

`dev/lean/UnivariateML/Family/Shape3Frechet.lean:609`

Unrestricted Fréchet diverges. On the fitted branch, a minimum majority also diverges, while exact half contact has an unattained finite supremum reached by the shape-one collapse.

- **frechet, all free.** Unrestricted shapes and the fitted branch at or above half minimum multiplicity. The strict-sub-half optimized Gumbel comparison remains open.

```lean
theorem all_free_collapse_classification {n : ℕ} (hn : 0 < n) {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) (hz : ∃ i, g i = 0) :
    let V := {v | ∃ k ≥ 1, ∃ p ∈ domain, logLik g k p.1 p.2 = v}
    let B := (n : ℝ) * (log 2 - 1) - 2 * ∑ i : Fin n, log (g i)
    (¬ BddAbove {v | ∃ k > 0, ∃ p ∈ domain, logLik g k p.1 p.2 = v}) ∧
    (n < 2 * minimumCount g → ¬ BddAbove V) ∧
    (n = 2 * minimumCount g → IsLUB V B ∧
      (∀ v ∈ V, v < B) ∧ Tendsto (profile g 1) (𝓝[>] 0) (𝓝 B))
```

### `UnivariateML.Shape3GEV.held_location_contact_classification`

`dev/lean/UnivariateML/Family/Shape3GEV.lean:25`

For a held location at or below the minimum, majority contact is unbounded. Exact half contact has a sharp finite supremum, attained precisely when all strictly positive residuals are equal.

- **gev, mu.** Majority and exact-half contacts at held location. The criterion for negative residuals or strict sub-half contact requires a different bound.

```lean
theorem held_location_contact_classification {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ}
    {m : ℝ} (hm : ∀ i, m ≤ x i) :
    let c := (Finset.univ.filter (fun i ↦ x i = m)).card
    let D : Set (ℝ × ℝ) := {q | q.1 ∈ Icc (-1 : ℝ) 1 ∧ (m, q.2) ∈ GEV.domain x q.1}
    let L := fun q : ℝ × ℝ ↦ GEV.logLik x q.1 (m, q.2)
    (n < 2 * c → ¬ BddAbove (L '' D)) ∧
    (n = 2 * c → IsLUB (L '' D) (Shape2GEV.halfValue x m) ∧
      ((∃ q ∈ D, IsMaxOn L D q) ↔ ∃ s > 0, ∀ i, m < x i → x i - m = s))
```

### `UnivariateML.Shape3GEV.held_scale_compact_and_closed_endpoint`

`dev/lean/UnivariateML/Family/Shape3GEV.lean:47`

At positive held scale, competitive positive superlevels are compact, and the reverse-exponential endpoint has its own attainable location optimum as a member of the fitted model.

- **gev, sigma.** Compact positive superlevels, unique endpoint-branch location, and full fitted-branch attainment. A better interior value may win over the closed endpoint.

```lean
theorem held_scale_compact_and_closed_endpoint {n : ℕ} (hn : 0 < n)
    (x : Fin n → ℝ) {s : ℝ} (hs : 0 < s) :
    let D := {p : ℝ × ℝ | p.2 ∈ Icc (-1 : ℝ) 1 ∧ (p.1, s) ∈ GEV.domain x p.2}
    (∀ e > 0, IsCompact (((univ : Set ℝ) ×ˢ Icc (-1 : ℝ) 1) ∩
      JoinsGEV.heldScaleLikelihood x s ⁻¹' Ici e)) ∧
    (∃! m, (m, s) ∈ GEV.domain x (-1) ∧
      IsMaxOn (fun u ↦ GEV.logLik x (-1) (u, s)) {u | (u, s) ∈ GEV.domain x (-1)} m) ∧
    (∃ p ∈ D, IsMaxOn (fun q ↦ GEV.logLik x q.2 (q.1, s)) D p)
```

### `UnivariateML.Shape3GenGamma.held_location_small_shape_limit`

`dev/lean/UnivariateML/Family/Shape3GenGamma.lean:153`

Positive finite limiting rates give the power or Pareto trace on the compatible half-line and zero likelihood off it. Zero residuals are included.

- **gengamma, mu.** Both signed orientations and all contacts at finite positive limiting rate. The joined escape theorem removes the limiting-rate assumption for competitive escapes.

```lean
theorem held_location_small_shape_limit {ι : Type*} {l : Filter ι}
    {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) (μ ε : ℝ) (hε : |ε| = 1)
    {k A : ι → ℝ} {a : ℝ}
    (hk : Tendsto k l (𝓝[>] 0)) (hA : Tendsto A l (𝓝 a)) (ha : 0 < a) :
    let z := fun i : Fin n ↦ ε * (log (x i) - μ)
    let L := fun j ↦ GenGammaTwoPoint.sampleLogLik x (μ, sqrt (k j) / A j, ε / sqrt (k j))
    ((∀ i, z i ≤ 0) →
      Tendsto L l (𝓝 (log a + (∑ i : Fin n, (a * z i - log (x i))) / n))) ∧
    ((∃ i, 0 < z i) → Tendsto L l atBot)
```

### `UnivariateML.Shape3GenGamma.moving_mode_supported_face`

`dev/lean/UnivariateML/Family/Shape3GenGamma.lean:187`

Along either signed moving-mode face, the limiting contact penalty is the limit of the scaled exponential at the endpoint. A zero penalty gives the full power or Pareto endpoint density.

- **gengamma, all free.** path:1 and path:2 on the compatible side, with a specified finite contact penalty. Full density at contact requires the penalty to tend to zero.

```lean
theorem moving_mode_supported_face {ι : Type*} {l : Filter ι} {n : ℕ} (hn : 0 < n)
    (x : Fin n → ℝ) (μ₀ ε : ℝ) (hε : |ε| = 1)
    {k A μ : ι → ℝ} {a c : ℝ}
    (hk : Tendsto k l (𝓝[>] 0)) (hA : Tendsto A l (𝓝 a)) (ha : 0 < a)
    (hμ : Tendsto μ l (𝓝 μ₀))
    (hc : Tendsto (fun j ↦ k j * exp (A j * (ε * (μ₀ - μ j)) / k j)) l (𝓝 c))
    (hs : ∀ i, ε * (log (x i) - μ₀) ≤ 0) :
    Tendsto (fun j ↦ GenGammaTwoPoint.sampleLogLik x (μ j, sqrt (k j) / A j, ε / sqrt (k j))) l
      (𝓝 (log a + (∑ i : Fin n,
        (a * (ε * (log (x i) - μ₀)) - log (x i) -
          (if ε * (log (x i) - μ₀) = 0 then c else 0))) / n))
```

### `UnivariateML.Shape3GenGamma.held_shape_scale_location_no_escape`

`dev/lean/UnivariateML/Family/Shape3GenGamma.lean:327`

With shape and positive scale fixed, mean log likelihood tends to minus infinity as location leaves compact sets, for every nonempty positive sample.

- **gengamma, Q+sigma.** The entire real location domain, including Q=0 and constant samples.

```lean
theorem held_shape_scale_location_no_escape {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {σ : ℝ} (hσ : 0 < σ) (Q : ℝ) :
    Tendsto (fun μ ↦ GenGammaTwoPoint.sampleLogLik x (μ, σ, Q)) (cocompact ℝ) atBot
```

### `UnivariateML.Shape3GenGamma.held_location_escape_classification`

`dev/lean/UnivariateML/Family/Shape3GenGamma.lean:651`

At a held mode, all-zero residuals allow concentration. Every finite log-likelihood limit on any escape with a nonzero residual is a supported power or Pareto trace at that mode. This includes all zero-residual contacts.

- **gengamma, mu.** Arbitrary escaping filters with a finite log-likelihood limit, and all-zero collapse. Both orientations, supported and unsupported residuals, including tied zeros.

```lean
theorem held_location_escape_classification {n : ℕ} (hn : 0 < n)
    (x : Fin n → ℝ) (hx : ∀ i, 0 < x i) (μ : ℝ) :
    let D := Ioi (0 : ℝ) ×ˢ (univ : Set ℝ)
    let L := fun p : D ↦ GenGammaTwoPoint.sampleLogLik x (μ, p.val.1, p.val.2)
    ((∀ i, log (x i) = μ) → ∀ Q,
      Tendsto (fun σ ↦ GenGammaTwoPoint.sampleLogLik x (μ, σ, Q)) (𝓝[>] 0) atTop) ∧
    ((∃ i, log (x i) ≠ μ) → ∀ {ι : Type} {l : Filter ι} [l.NeBot] {p : ι → D} {v : ℝ},
      Tendsto p l (cocompact D) → Tendsto (L ∘ p) l (𝓝 v) →
      ∃ ε : ℝ, |ε| = 1 ∧ ∃ a > 0,
        (∀ i, ε * (log (x i) - μ) ≤ 0) ∧
        v = log a + (∑ i : Fin n, (a * (ε * (log (x i) - μ)) - log (x i))) / n)
```

### `UnivariateML.Shape3GenGamma.held_scale_shape_ends`

`dev/lean/UnivariateML/Family/Shape3GenGamma.lean:681`

At positive held scale, both signed shape ends lose likelihood uniformly over all locations. The gamma shape k tends to zero when the absolute Prentice shape tends to infinity.

- **gengamma, sigma.** Positive held scale, arbitrary moving location and signed shape. The k chart equals Q⁻², so this also applies with location held.
- **gengamma, mu+sigma.** Both signed shape ends at fixed location and positive held scale.

```lean
theorem held_scale_shape_ends {ι : Type*} {l : Filter ι} {n : ℕ}
    (hn : 0 < n) (x : Fin n → ℝ) {σ : ℝ} (hσ : 0 < σ)
    {k ε μ : ι → ℝ} (hk : Tendsto k l (𝓝[>] 0))
    (hε : ∀ᶠ j in l, |ε j| = 1) :
    Tendsto (fun j ↦ GenGammaTwoPoint.sampleLogLik x (μ j, σ, ε j / sqrt (k j)))
      l atBot
```

### `UnivariateML.Shape3GenGamma.likelihood_continuous`

`dev/lean/UnivariateML/Family/Shape3GenGamma.lean:958`

The Prentice log likelihood is continuous in all coefficients on the positive-scale domain, including through the normal seam.

- **gengamma, all free.** Continuity over the positive-scale domain, including the normal seam. Analytic input for held-scale and held-location existence.

```lean
theorem likelihood_continuous {n : ℕ} (x : Fin n → ℝ) :
    ContinuousOn (GenGammaTwoPoint.sampleLogLik x) GenGammaTwoPoint.domain
```

### `UnivariateML.Shape3GenGamma.held_location_scale_attains`

`dev/lean/UnivariateML/Family/Shape3GenGamma.lean:1061`

Every nonempty generalized-gamma sample has a finite fit with location and positive scale held. The normal shape is included.

- **gengamma, mu+sigma.** Unconditional finite attainment at every nonempty positive sample and positive held scale.

```lean
theorem held_location_scale_attains {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    (μ : ℝ) {σ : ℝ} (hσ : 0 < σ) :
    ∃ Q, IsMaxOn (fun q ↦ GenGammaTwoPoint.sampleLogLik x (μ, σ, q)) univ Q
```

### `UnivariateML.Shape3GenGamma.held_scale_attains`

`dev/lean/UnivariateML/Family/Shape3GenGamma.lean:1079`

Every nonempty generalized-gamma sample has a finite free-location and free-shape fit with positive scale held.

- **gengamma, sigma.** Unconditional finite attainment with free location and shape.

```lean
theorem held_scale_attains {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {σ : ℝ} (hσ : 0 < σ) :
    ∃ p : ℝ × ℝ, IsMaxOn
      (fun q : ℝ × ℝ ↦ GenGammaTwoPoint.sampleLogLik x (q.1, σ, q.2)) univ p
```

### `UnivariateML.Shape3GenGamma.held_location_attains_iff`

`dev/lean/UnivariateML/Family/Shape3GenGamma.lean:1165`

A held-mode fit attains iff a fitted normal or a finite signed stationary point meets every compatible face value. If all residuals vanish the likelihood is unbounded, so this case has no finite fit.

- **gengamma, mu.** Normal or signed stationary witness, including equality with every compatible face. The statement ranges over all stationary points. It does not enumerate roots.

```lean
theorem held_location_attains_iff {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    (hx : ∀ i, 0 < x i) (μ : ℝ) :
    let D := Ioi (0 : ℝ) ×ˢ (univ : Set ℝ)
    let L := fun p : ℝ × ℝ ↦ GenGammaTwoPoint.sampleLogLik x (μ, p.1, p.2)
    (∃ p ∈ D, IsMaxOn L D p) ↔
    (∃ i, log (x i) ≠ μ) ∧
    ∃ p ∈ D,
      ((p.2 = 0 ∧ IsMaxOn (fun s ↦ L (s, 0)) (Ioi 0) p.1) ∨
        (p.2 ≠ 0 ∧ deriv (fun s ↦ L (s, p.2)) p.1 = 0 ∧
          deriv (fun q ↦ L (p.1, q)) p.2 = 0)) ∧
      ∀ ε, |ε| = 1 → ∀ a > 0, (∀ i, ε * (log (x i) - μ) ≤ 0) →
        log a + (∑ i : Fin n, (a * (ε * (log (x i) - μ)) - log (x i))) / n ≤ L p
```

### `UnivariateML.Shape3GenGamma.all_contact_held_location_scale_normal`

`dev/lean/UnivariateML/Family/Shape3GenGamma.lean:1260`

If every observation is at the held log location, fixing a positive scale leaves the normal shape as the unique maximizer.

- **gengamma, mu+sigma.** Exact all-contact singleton. The almost-everywhere claim on other strata needs UniqueAE.

```lean
theorem all_contact_held_location_scale_normal {n : ℕ} (hn : 0 < n)
    (x : Fin n → ℝ) {μ σ : ℝ} (hσ : 0 < σ) (hx : ∀ i, log (x i) = μ) :
    {Q : ℝ | IsMaxOn (fun q ↦ GenGammaTwoPoint.sampleLogLik x (μ, σ, q)) univ Q} = {0}
```

### `UnivariateML.Shape3GenGamma.held_location_symmetric_normal`

`dev/lean/UnivariateML/Family/Shape3GenGamma.lean:1320`

At a held log mode, sign-symmetric nonzero residuals have a single maximizing scale and shape pair, and its shape is normal.

- **gengamma, mu.** Exact singleton for every sign-symmetric nonzero residual multiset. Symmetry is expressed by an index permutation, retaining multiplicities.

```lean
theorem held_location_symmetric_normal {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) (μ : ℝ)
    (h : ∃ i, log (x i) ≠ μ)
    (e : Fin n ≃ Fin n) (he : ∀ i, log (x (e i)) - μ = -(log (x i) - μ)) :
    let D := Ioi (0 : ℝ) ×ˢ (univ : Set ℝ)
    let L := fun p : ℝ × ℝ ↦ GenGammaTwoPoint.sampleLogLik x (μ, p.1, p.2)
    ∃ s > 0, {p ∈ D | IsMaxOn L D p} = {(s, 0)}
```

### `UnivariateML.Shape3GenGamma.weighted_two_level_signed_profile_lt_face`

`dev/lean/UnivariateML/Family/Shape3GenGamma.lean:1439`

For two standardized log levels, arbitrary positive count weights still give a signed profile strictly below the best endpoint value. The equal-count balanced-pair theorem supplies the strict comparison.

- **gengamma, all free.** Two standardized levels with arbitrary strictly positive real weights. Signed profile comparison. It does not state the raw-sample two-level theorem or the normal comparison.

```lean
theorem weighted_two_level_signed_profile_lt_face {w t k : ℝ}
    (hw : 0 < w) (hw1 : w < 1) (ht : 0 < t) (hk : 0 < k) :
    let c := 2 * min w (1 - w)
    log t + (k * log k - k - log (Gamma k)) -
      k * (log (w * exp t + (1 - w) * exp (-t)) - (2 * w - 1) * t) < -1 - log c
```

### `UnivariateML.Shape3GenGamma.signed_score_separation`

`dev/lean/UnivariateML/Family/Shape3GenGamma.lean:1463`

Three distinct log observations identify the signed Prentice score parameters. This is a separation input for almost-everywhere uniqueness.

- **gengamma, all free.** Three distinct log levels identify two signed finite competitors. Finite-versus-finite input to UniqueAE.
- **gengamma, mu.** Three-level separation for the indicated restricted finite candidates. The probability conclusion and any exceptional-sample claims are separate.
- **gengamma, mu+sigma.** Three-level separation for the indicated restricted finite candidates. The probability conclusion and any exceptional-sample claims are separate.
- **gengamma, sigma.** Three-level separation for the indicated restricted finite candidates. The probability conclusion and any exceptional-sample claims are separate.

```lean
theorem signed_score_separation {k p A k' p' A' x y z : ℝ}
    (hp : p ≠ 0) (hp' : p' ≠ 0) (hA : 0 < A) (hA' : 0 < A')
    (hxy : x < y) (hyz : y < z)
    (h : ∀ t ∈ ({x, y, z} : Set ℝ),
      k * p - A * p * exp (p * t) = k' * p' - A' * p' * exp (p' * t)) :
    p = p' ∧ k = k' ∧ A = A'
```

### `UnivariateML.Shape3GenGamma.signed_normal_score_separation`

`dev/lean/UnivariateML/Family/Shape3GenGamma.lean:1509`

A signed score cannot coincide with a normal score at three distinct log observations, regardless of the normal intercept and slope.

- **gengamma, all free.** Three distinct log levels separate signed and normal scores. Signed-versus-normal input to UniqueAE.
- **gengamma, mu.** Three-level separation for the indicated restricted finite candidates. The probability conclusion and any exceptional-sample claims are separate.
- **gengamma, mu+sigma.** Three-level separation for the indicated restricted finite candidates. The probability conclusion and any exceptional-sample claims are separate.
- **gengamma, sigma.** Three-level separation for the indicated restricted finite candidates. The probability conclusion and any exceptional-sample claims are separate.

```lean
theorem signed_normal_score_separation {k p A c m x y z : ℝ}
    (hp : p ≠ 0) (hA : 0 < A) (hxy : x < y) (hyz : y < z) :
    ¬ ∀ t ∈ ({x, y, z} : Set ℝ), k * p - A * p * exp (p * t) = c + m * t
```

### `UnivariateML.Shape3InvBurr.held_scale_classification`

`dev/lean/UnivariateML/Family/Shape3InvBurr.lean:23`

Constants on or above the scale diverge. Samples on or below the scale with a strict inequality lose to the Pareto trace. In the remaining nonconstant branch a finite fit exists.

- **invburr, scale.** All held-scale outcomes, including the all-contact constant. Uses mean log likelihood for the strict power bound.

```lean
theorem held_scale_classification {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {s : ℝ} (hs : 0 < s) :
    let L := fun p : ℝ × ℝ ↦ logLik x p.2 p.1 s
    let z := BurrHeld.logRatios (reciprocal x) s⁻¹
    let B := BurrHeld.paretoValue z +
      (reciprocalJac x / n - BurrHeld.avg (fun i ↦ log (reciprocal x i)))
    ((∃ p ∈ BurrHeld.shapeDomain, IsMaxOn L BurrHeld.shapeDomain p) ↔
      (∃ i j, x i ≠ x j) ∧ ∃ i, s < x i) ∧
    (∀ v ≥ s, (∀ i, x i = v) → ¬ BddAbove (L '' BurrHeld.shapeDomain)) ∧
    ((∀ i, x i ≤ s) → (∃ i, x i < s) →
      IsLUB ((fun p ↦ L p / n) '' BurrHeld.shapeDomain) B ∧
        ∀ p ∈ BurrHeld.shapeDomain, L p / n < B)
```

### `UnivariateML.Shape3Lomax.score_separation`

`dev/lean/UnivariateML/Family/Shape3Lomax.lean:21`

Two distinct positive observations distinguish Lomax coefficients, and no constant exponential-face score agrees at both observations.

- **lomax, all free.** Two-level finite/finite and finite/exponential score separation. The measure-theoretic conclusion uses UniqueAE.

```lean
theorem score_separation {k s k' s' x y : ℝ} (hk : 0 < k)
    (hs : 0 < s) (hs' : 0 < s') (hx : 0 < x) (hxy : x < y) :
    ((-(1 + k) / (s + x) = -(1 + k') / (s' + x) ∧
      -(1 + k) / (s + y) = -(1 + k') / (s' + y)) → k = k' ∧ s = s') ∧
    (∀ c : ℝ, ¬ (-(1 + k) / (s + x) = c ∧ -(1 + k) / (s + y) = c))
```

### `UnivariateML.Shape3PGW.held_scale_classification`

`dev/lean/UnivariateML/Family/Shape3PGW.lean:21`

All held-scale outcomes, including concentration on constants at or below scale and the strict threshold supremum on the supported branch.

- **pgw, scale.** Attainment, constant concentration or strict threshold supremacy. The threshold value retains contact factors.

```lean
theorem held_scale_classification {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {a : ℝ} (ha : 0 < a) :
    let L := fun p : ℝ × ℝ ↦ logLik x p.1 a p.2
    ((∃ p ∈ Weibull.domain, IsMaxOn L Weibull.domain p) ↔
      (∃ i j, x i ≠ x j) ∧ ∃ i, x i < a) ∧
    (∀ v ≤ a, (∀ i, x i = v) →
      ∃ c : ℝ → ℝ, (∀ b, 0 < c b) ∧
        Tendsto (fun b ↦ L (b, c b)) atTop atTop) ∧
    ((∀ i, a ≤ x i) → (∃ i, a < x i) →
      ∃ k > 0, IsLUB (L '' Weibull.domain) (thresholdLogLik x a k) ∧
        ∀ p ∈ Weibull.domain, L p < thresholdLogLik x a k)
```

### `UnivariateML.Shape3TrGamma.held_scale_trace_and_concentration`

`dev/lean/UnivariateML/Family/Shape3TrGamma.lean:31`

With scale fixed, every supported power path has a finite improvement, including at ties with the scale. Constants at or above scale diverge.

- **trgamma, scale.** Strictly beaten supported endpoint traces and the constant concentration branch.

```lean
theorem held_scale_trace_and_concentration {n : ℕ} (hn : 0 < n)
    {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) {a : ℝ} (ha : 0 < a) :
    (∀ v ≥ a, (∀ i, x i = v) →
      Tendsto (fun p ↦ TransformedGamma.logLik x (exp (p * (log v - log a))) p a)
        atTop atTop) ∧
    ((∀ i, x i ≤ a) → ∀ d > 0,
      let B := log d - Gamma.avgLog x + d * Gamma.avg (fun i ↦ log (x i) - log a) -
        Gamma.avg (fun i ↦ if x i = a then 1 else 0)
      Tendsto (fun q : ℝ ↦ TransformedGamma.logLik x (d * q) q⁻¹ a) (𝓝[>] 0) (𝓝 B) ∧
      ∃ k > 0, ∃ p > 0, B < TransformedGamma.logLik x k p a)
```

### `UnivariateML.Shape3TrGamma.inverse_held_scale_trace_and_concentration`

`dev/lean/UnivariateML/Family/Shape3TrGamma.lean:52`

The reciprocal held-scale result has the same finite improvement of every supported Pareto trace. Its concentration branch reverses the order between the common observation and the held scale.

- **invtrgamma, scale.** Strictly beaten supported endpoint traces and the constant concentration branch.

```lean
theorem inverse_held_scale_trace_and_concentration {n : ℕ} (hn : 0 < n)
    {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) {a : ℝ} (ha : 0 < a) :
    (∀ v ≤ a, (∀ i, x i = v) →
      Tendsto (fun p ↦ InverseTransformedGamma.logLik x (exp (p * (log a - log v))) p a)
        atTop atTop) ∧
    ((∀ i, a ≤ x i) → ∀ d > 0,
      let B := log d - Gamma.avgLog x + d * Gamma.avg (fun i ↦ log a - log (x i)) -
        Gamma.avg (fun i ↦ if x i = a then 1 else 0)
      Tendsto (fun q : ℝ ↦ InverseTransformedGamma.logLik x (d * q) q⁻¹ a)
        (𝓝[>] 0) (𝓝 B) ∧
      ∃ k > 0, ∃ p > 0, B < InverseTransformedGamma.logLik x k p a)
```

### `UnivariateML.Shape3TrGamma.held_scale_varying_rate_trace`

`dev/lean/UnivariateML/Family/Shape3TrGamma.lean:116`

For every moving positive rate with finite positive limit, the held-scale transformed-gamma trace has its contact penalty.

- **trgamma, scale.** path:1 for arbitrary positive limiting products, including contact. The loss is one log unit per contact before averaging.

```lean
theorem held_scale_varying_rate_trace {ι : Type*} {l : Filter ι} {n : ℕ}
    {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) {a : ℝ} (ha : 0 < a)
    {k p : ι → ℝ} {d : ℝ} (hd : 0 < d)
    (hk : Tendsto k l (𝓝[>] 0)) (hkp : Tendsto (fun j ↦ k j * p j) l (𝓝 d))
    (hle : ∀ i, x i ≤ a) :
    Tendsto (fun j ↦ TransformedGamma.logLik x (k j) (p j) a) l
      (𝓝 (log d - Gamma.avgLog x + d * Gamma.avg (fun i ↦ log (x i) - log a) -
        Gamma.avg (fun i ↦ if x i = a then 1 else 0)))
```

### `UnivariateML.Shape3TrGamma.inverse_held_scale_varying_rate_trace`

`dev/lean/UnivariateML/Family/Shape3TrGamma.lean:133`

Reversing the log-excess orientation gives the varying-rate trace for held-scale inverse transformed gamma, with the same contact penalty.

- **invtrgamma, scale.** path:1 for arbitrary positive limiting products, including contact. The loss is one log unit per contact before averaging.

```lean
theorem inverse_held_scale_varying_rate_trace {ι : Type*} {l : Filter ι} {n : ℕ}
    {x : Fin n → ℝ} {a : ℝ} (ha : 0 < a)
    {k p : ι → ℝ} {d : ℝ} (hd : 0 < d)
    (hk : Tendsto k l (𝓝[>] 0)) (hkp : Tendsto (fun j ↦ k j * p j) l (𝓝 d))
    (hle : ∀ i, a ≤ x i) :
    Tendsto (fun j ↦ InverseTransformedGamma.logLik x (k j) (p j) a) l
      (𝓝 (log d - Gamma.avgLog x + d * Gamma.avg (fun i ↦ log a - log (x i)) -
        Gamma.avg (fun i ↦ if x i = a then 1 else 0)))
```

### `UnivariateML.Shape3TrGamma.held_scale_attains_iff`

`dev/lean/UnivariateML/Family/Shape3TrGamma.lean:455`

At a positive held scale, a transformed-gamma fit exists exactly outside the constant samples at or above the hold.

- **trgamma, scale.** Exact criterion for every nonempty positive sample and positive held scale. Profiles gamma shape to control the power ends.

```lean
theorem held_scale_attains_iff {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {a : ℝ} (ha : 0 < a) :
    (∃ p ∈ Gamma.domain, IsMaxOn
      (fun q : ℝ × ℝ ↦ TransformedGamma.logLik x q.1 q.2 a) Gamma.domain p) ↔
      ¬ ∃ v ≥ a, ∀ i, x i = v
```

### `UnivariateML.Shape3TrGamma.inverse_held_scale_attains_iff`

`dev/lean/UnivariateML/Family/Shape3TrGamma.lean:516`

The held-scale criterion transfers to inverse transformed gamma, reversing the side of the hold for constant concentration.

- **invtrgamma, scale.** Exact criterion for every nonempty positive sample and positive held scale. Profiles gamma shape to control the power ends.

```lean
theorem inverse_held_scale_attains_iff {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {a : ℝ} (ha : 0 < a) :
    (∃ p ∈ Gamma.domain, IsMaxOn
      (fun q : ℝ × ℝ ↦ InverseTransformedGamma.logLik x q.1 q.2 a) Gamma.domain p) ↔
      ¬ ∃ v ≤ a, ∀ i, x i = v
```

### `UnivariateML.Shape3TrGamma.constant_concentration_masks`

`dev/lean/UnivariateML/Family/Shape3TrGamma.lean:544`

On positive constants, either free shape can concentrate at a free scale. The held-scale concentration paths cover contact and the constant branch above the hold.

- **trgamma, all free.** path:2 and concentration with either shape held or with scale held on the collapsing side. The fixed-shape path includes scale contact.

```lean
theorem constant_concentration_masks {n : ℕ} (hn : 0 < n) {v : ℝ} (_hv : 0 < v) :
    let x := fun _ : Fin n ↦ v
    (∀ p > 0, Tendsto (fun k ↦ TransformedGamma.logLik x k p
      (TransformedGamma.scale x k p)) atTop atTop) ∧
    (∀ k > 0, Tendsto (fun p ↦ TransformedGamma.logLik x k p v) atTop atTop) ∧
    (∀ a > 0, a ≤ v →
      Tendsto (fun p ↦ TransformedGamma.logLik x (exp (p * (log v - log a))) p a)
        atTop atTop)
```

### `UnivariateML.Shape3TrGamma.inverse_constant_concentration_masks`

`dev/lean/UnivariateML/Family/Shape3TrGamma.lean:558`

The same concentration paths for the reciprocal family include held-scale contact and the constant branch below the hold.

- **invtrgamma, all free.** path:2 and concentration with either shape held or with scale held on the collapsing side. The fixed-shape path includes scale contact.

```lean
theorem inverse_constant_concentration_masks {n : ℕ} (hn : 0 < n) {v : ℝ} (hv : 0 < v) :
    let x := fun _ : Fin n ↦ v
    (∀ p > 0, Tendsto (fun k ↦ InverseTransformedGamma.logLik x k p
      (TransformedGamma.scale (fun _ : Fin n ↦ v⁻¹) k p)⁻¹) atTop atTop) ∧
    (∀ k > 0, Tendsto (fun p ↦ InverseTransformedGamma.logLik x k p v) atTop atTop) ∧
    (∀ a ≥ v,
      Tendsto (fun p ↦ InverseTransformedGamma.logLik x (exp (p * (log a - log v))) p a)
        atTop atTop)
```

### `UnivariateML.Shape3TrGamma.oriented_score_separation`

`dev/lean/UnivariateML/Family/Shape3TrGamma.lean:573`

Three distinct log levels identify power, gamma shape and scale from the transformed-gamma observation score. Taking negative signed power gives the inverse orientation in the same statement.

- **trgamma, all free.** Three-level finite-score separation in the appropriate signed orientation. The null-set conclusion and face comparisons are separate.
- **trgamma, scale.** Three-level finite-score separation in the appropriate signed orientation. The null-set conclusion and face comparisons are separate.
- **invtrgamma, all free.** Three-level finite-score separation in the appropriate signed orientation. The null-set conclusion and face comparisons are separate.
- **invtrgamma, scale.** Three-level finite-score separation in the appropriate signed orientation. The null-set conclusion and face comparisons are separate.

```lean
theorem oriented_score_separation {k p a k' p' a' x y z : ℝ}
    (hp : p ≠ 0) (hp' : p' ≠ 0) (ha : 0 < a) (ha' : 0 < a')
    (hxy : x < y) (hyz : y < z)
    (h : ∀ t ∈ ({x, y, z} : Set ℝ),
      k * p - p * exp (p * (t - log a)) =
        k' * p' - p' * exp (p' * (t - log a'))) :
    p = p' ∧ k = k' ∧ a = a'
```

## Lean checks

All ten files passed direct `lake env lean` checks on the workstation. The final workstation Lake build ran with all ten modules temporarily imported through `UnivariateML.lean` under the shared single CPU slot.

> Build completed successfully (8096 jobs).

The temporary imports were removed and `git diff --exit-code dev/lean/UnivariateML.lean` passed.
