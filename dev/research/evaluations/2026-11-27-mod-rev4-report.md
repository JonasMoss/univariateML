# Review of fixed truncation for location-scale families

TASK-454 reviewed all 91 proposals from TASK-449. The join file contains 91 written rows, with 86 accepted, five regraded and zero rejected. By direction it contains 32 iff, 47 exact and 12 one-sided claims. Two regrades correct the free half-Cauchy claims and retain their original direction. Three narrow cardinality claims to one-sided. All rows carry origin `TASK-449`. [Reviewed CSV](2026-11-27-mod-rev4-data/reviewed.csv) · [Source proposals](2026-11-27-trunc-ls-math-data/proofs.csv) · [Source proof](2026-11-27-trunc-ls-math-report.md)

**The free half-Cauchy failure list omits shrinking support at an upper-cut constant.** The criterion and failure rows now name this case. The separate held-scale result in TASK-449 had already recognized it. [Upper-cut half-Cauchy](#upper-cut-half-cauchy)

The review uses finite nonempty samples, valid holds and closed fixed windows with positive retained mass for feasible parameters. All 36 family and hold pairs have a parent in the base atlas and a matching eligible `truncate:known` entry in the modifier atlas. Infinite endpoints are unbounded. NA bounds describe estimated-window regimes, and the zero modifiers and their extra coefficients `p0` and `pi` are separate. An incompatible hold or a zero-mass retained event is an input failure. [Atlas README](../existence-atlas/README.md) · [Base overview](../existence-atlas/overview.md) · [Base atlas](../existence-atlas/atlas.csv) · [Base proofs](../existence-atlas/proofs.csv) · [Modifier atlas](../existence-atlas/modifier-atlas.csv) · [Modifier design](../../design/modifiers.md)

## Row decisions

| Family | Held | Question | Verdict | Reason |
| --- | --- | --- | --- | --- |
| cauchy | all free | criterion | accepted | The power envelope exhausts broad, exterior and sample-centered limits, and a finite witness at the largest finite face height suffices. |
| cauchy | all free | failure | accepted | Rescaling at the limiting center gives the majority and critical powers, while remote and exterior limits give uniform and inverse-square heights. |
| cauchy | location | criterion | accepted | Fixing the center leaves two scale ends with explicit joint likelihood values, including the critical equality. |
| cauchy | location | failure | accepted | The zero-scale limit is determined by the center's tie count or exterior distance, and infinite scale contributes only the bounded uniform. |
| cauchy | location+scale | criterion | accepted | A compatible fixed model with positive retained mass supplies a finite likelihood. |
| cauchy | location+scale | failure | accepted | The compatible fixed model has no free parameter along which to escape. |
| cauchy | location+scale | uniqueness | accepted | Fixing all parent coefficients leaves a singleton feasible set. |
| cauchy | scale | case:nonunique | accepted | The exact reflected sample gives a likelihood at location 4 strictly above both zero location and the uniform boundary, forcing a reflected pair of global maximizers. |
| cauchy | scale | criterion | accepted | Remote locations converge to bounded uniform or lose on unbounded windows, so finite equality with the bounded threshold attains. |
| cauchy | scale | failure | accepted | With scale fixed, the compact center limit stays feasible and the only positive excluded height is bounded uniform. |
| gumbel | all free | criterion | accepted | The transformed exponential separates the two slope signs and the uniform corner, and nonconstant data eliminate the concentration ends. |
| gumbel | all free | failure | accepted | The fixed-rate and broad-scale formulas give all integrable exponential faces, while a constant sample always has a divergent concentration path. |
| gumbel | mu | criterion | accepted | Holding location reduces the broad-scale face to bounded uniform and sharp concentration to the projection. |
| gumbel | mu | failure | accepted | The two rate ends can win only as divergent projection collapse or bounded uniform. |
| gumbel | mu+sigma | criterion | accepted | A compatible fixed model with positive retained mass supplies a finite likelihood. |
| gumbel | mu+sigma | failure | accepted | The compatible fixed model has no free parameter along which to escape. |
| gumbel | mu+sigma | uniqueness | accepted | Fixing all parent coefficients leaves a singleton feasible set. |
| gumbel | sigma | criterion | accepted | Strict decrease of the transformed exponential mean puts the unique root between its two endpoint means. |
| gumbel | sigma | failure | accepted | The two mean equalities send the transformed rate to infinity and zero respectively, with the stated collapse and decreasing-tilt likelihoods. |
| gumbel | sigma | uniqueness | accepted | Positive transformed variance makes the score strictly decreasing over its entire rate domain. |
| hcauchy | all free | criterion | regraded | The power comparison covers nondegenerate retained intervals, while constants at the upper cut require a separate exclusion because support can shrink. |
| hcauchy | all free | failure | regraded | The proposed list omits upper-cut shrinking support, which diverges even along a fixed positive scale path. |
| hcauchy | mu | criterion | accepted | The held endpoint fixes the retained interval and leaves exactly the scale ends accounted for by the power comparison. |
| hcauchy | mu | failure | accepted | Exterior inverse-square, effective uniform and the held-center collapse cases exhaust the positive excluded values. |
| hcauchy | mu+sigma | criterion | accepted | A compatible fixed model with positive retained mass supplies a finite likelihood. |
| hcauchy | mu+sigma | failure | accepted | The compatible fixed model has no free parameter along which to escape. |
| hcauchy | mu+sigma | uniqueness | accepted | Fixing all parent coefficients leaves a singleton feasible set. |
| hcauchy | sigma | case:left-ray-unique | accepted | With no finite lower cut, increasing location up to the sample minimum strictly improves the joint likelihood at fixed scale. |
| hcauchy | sigma | criterion | accepted | The finite branch endpoints and roots exhaust interior candidates, and the upper-cut constant is the only feasible shrinking-support exception. |
| hcauchy | sigma | failure | accepted | Only a bounded uniform from remote location or shrinking support at the finite upper cut can win at held scale. |
| hnorm | all free | criterion | accepted | Monotonicity reduces the moving-support branch to the sample minimum, and coercivity plus strict concavity give the restricted-normal comparison including equality. |
| hnorm | all free | failure | accepted | For nonconstants the quadratic wedge loses at infinity and its finite edge and the endpoint branch can only contribute exponential or uniform limits. |
| hnorm | all free | uniqueness | regraded | Each branch has at most one finite optimum, so ranking two branch optima establishes a cardinality bound under the component rule. |
| hnorm | mu | criterion | accepted | The strictly monotone Gaussian tilt mean ranges from the minimum squared residual to the uniform or infinite upper threshold. |
| hnorm | mu | failure | accepted | The lower equality concentrates at the lower effective cut, and the upper threshold is the bounded effective uniform. |
| hnorm | mu | uniqueness | accepted | The squared residual has positive variance throughout the retained interval, giving at most one scale root. |
| hnorm | mu+sigma | criterion | accepted | A compatible fixed model with positive retained mass supplies a finite likelihood. |
| hnorm | mu+sigma | failure | accepted | The compatible fixed model has no free parameter along which to escape. |
| hnorm | mu+sigma | uniqueness | accepted | Fixing all parent coefficients leaves a singleton feasible set. |
| hnorm | sigma | criterion | accepted | The scalar normal branch attains except at a lower-cut constant, and the moving-support branch attains except at an upper-cut constant. |
| hnorm | sigma | failure | accepted | Remote negative tilt can concentrate at the lower cut and shrinking support can concentrate at the upper cut, with positive-length finite ends feasible. |
| hnorm | sigma | uniqueness | regraded | The strictly monotone scalar score and the sample-minimum branch give at most two finite winners, so component ranking earns one-sided coverage. |
| laplace | all free | case:nonunique | accepted | The two fixed-rate candidates on the right ray rank strictly in favor of the plateau, whose rate has a unique optimum at 2. |
| laplace | all free | criterion | accepted | Finite-rate integrable location limits already occur on plateaus, and the other nonconstant losses leave only the bounded uniform threshold. |
| laplace | all free | failure | accepted | Concentration is unbounded for constants, while all positive nonconstant faces either have finite representatives or are bounded uniforms. |
| laplace | mu | criterion | accepted | The strictly decreasing residual mean ranges between the projection residual and its bounded uniform or infinite broad-scale value. |
| laplace | mu | failure | accepted | The two threshold equalities fall at opposite rate ends, with divergent density at the projection and finite uniform likelihood at the broad end. |
| laplace | mu | uniqueness | accepted | Positive variance of the absolute residual makes the rate score strictly decreasing. |
| laplace | mu+sigma | criterion | accepted | A compatible fixed model with positive retained mass supplies a finite likelihood. |
| laplace | mu+sigma | failure | accepted | The compatible fixed model has no free parameter along which to escape. |
| laplace | mu+sigma | uniqueness | accepted | Fixing all parent coefficients leaves a singleton feasible set. |
| laplace | sigma | criterion | accepted | On a proper window the gap maxima lie at explicit feasible locations or plateaus, and on the whole line any median attains. |
| laplace | sigma | failure | accepted | Each proper-window integrable tail has a finite representative and the whole-line median gives a finite fit. |
| laplace | sigma | uniqueness | accepted | Proper windows have explicit cut, sample and plateau candidates, while a whole-line window can also have a maximizing interior median gap. |
| logis | all free | criterion | accepted | Rate and location subsequences exhaust the tilt and collapse ends, including zero threshold on the whole line and finite equality elsewhere. |
| logis | all free | failure | accepted | The log-kernel bounds eliminate nonconstant small-scale collapse and leave only the optimized integrable tilt as a positive escape. |
| logis | location | criterion | accepted | The projection controls collapse, and only a bounded window has a positive broad-scale threshold to compare with a root. |
| logis | location | failure | accepted | The Lipschitz and collapse bounds account for both rate ends, with either divergent projection collapse or bounded uniform escape. |
| logis | location+scale | criterion | accepted | A compatible fixed model with positive retained mass supplies a finite likelihood. |
| logis | location+scale | failure | accepted | The compatible fixed model has no free parameter along which to escape. |
| logis | location+scale | uniqueness | accepted | Fixing all parent coefficients leaves a singleton feasible set. |
| logis | scale | case:nonunique | accepted | The two endpoint factors cancel the location dependence exactly for the sample of both window cuts. |
| logis | scale | criterion | accepted | The rational score covers every finite winner including the flat case, and its candidates must compete with the accessible exponential threshold or zero. |
| logis | scale | failure | accepted | Location escape at fixed rate yields only the integrable exponentials at positive height. |
| logis | scale | uniqueness | regraded | The nonzero cleared polynomial has finitely many positive roots and the zero polynomial gives a flat likelihood, so general stationary-height ranking earns one-sided coverage. |
| std | all free | case:divergent-tie | accepted | A fixed admissible df close enough to 2 makes the collapse exponent negative at the sample level. |
| std | mean | case:divergent-tie | accepted | The same fixed-df construction applies at the held mean when its tie count is supercritical. |
| std | mean+nu | criterion | accepted | The held mean and df restrict the two scale ends to the displayed power-family thresholds, including equality. |
| std | mean+nu | failure | accepted | The two scale ends give the bounded uniform and the exterior or tied-center power limits with exponent determined by held df. |
| std | mean+nu+sd | criterion | accepted | A compatible fixed model with positive retained mass supplies a finite likelihood. |
| std | mean+nu+sd | failure | accepted | The compatible fixed model has no free parameter along which to escape. |
| std | mean+nu+sd | uniqueness | accepted | Fixing all parent coefficients leaves a singleton feasible set. |
| std | mean+sd | case:divergent-tie | accepted | With both moments held, the natural lower-df path shrinks effective scale at the held mean and retains the strictly negative limiting exponent. |
| std | nu | criterion | accepted | Fixed df gives the uniform, exterior power and critical-collapse envelopes, and a finite candidate can tie the largest finite face. |
| std | nu | failure | accepted | The rescaled exponent yields the exact divergence test and the fixed power tail exhausts the remaining positive faces. |
| std | nu+sd | case:nonunique | accepted | At df 3 the exact reflected sample with sd 1 beats both the centered fit and the boundary, forcing at least two global maximizers. |
| std | nu+sd | criterion | accepted | With fixed effective scale the location tail gives uniform on bounded windows and zero on unbounded ones. |
| std | nu+sd | failure | accepted | Only remote location can escape with both df and sd held, and its positive limit is the bounded uniform. |
| std | sd | case:divergent-tie | accepted | At fixed sd, the path to df 2 shrinks effective scale and the centered rescaled integral has a finite positive limit. |
| unif | all free | criterion | accepted | Positive sample range supplies a shortest feasible retained interval, and zero range allows positive widths tending to zero. |
| unif | all free | failure | accepted | For a constant sample the retained width can shrink to zero around its value, giving a weak atom and divergent likelihood. |
| unif | all free | uniqueness | accepted | The optimal retained endpoints are the two sample extremes, and clipping leaves a free parent endpoint only at a coincident window cut. |
| unif | max | criterion | accepted | With effective upper cut fixed, the sample minimum is the optimal lower cut whenever the retained width is positive. |
| unif | max | failure | accepted | Zero optimal width gives a constant at the fixed effective upper cut and feasible widths approach zero from below. |
| unif | max | uniqueness | accepted | For an attained fit the parent lower endpoint is forced above the window cut and is a full ray of values at equality. |
| unif | max+min | criterion | accepted | A compatible fixed model with positive retained mass supplies a finite likelihood. |
| unif | max+min | failure | accepted | The compatible fixed model has no free parameter along which to escape. |
| unif | max+min | uniqueness | accepted | Fixing all parent coefficients leaves a singleton feasible set. |
| unif | min | criterion | accepted | With effective lower cut fixed, the sample maximum is the optimal upper cut whenever the retained width is positive. |
| unif | min | failure | accepted | Zero optimal width gives a constant at the fixed effective lower cut and feasible widths approach zero from above. |
| unif | min | uniqueness | accepted | For an attained fit the parent upper endpoint is forced below the window cut and is a full ray of values at equality. |

## Proof errors and scope corrections

### Upper-cut half-Cauchy

Take the closed window `[0,1]` and the sample `(1,1)`. Hold the scale along the path at 1 and send the location to `1-e` with `e` decreasing to zero. On the retained interval `[1-e,1]`, the scale-one kernel is `1/(1+(x-1+e)^2)`. Its integral is at most `e`, while its value at 1 is at least `1/2` for `0<e<=1`. Each observation density is at least `1/(2e)`. The product diverges and the conditional law converges weakly to the atom at 1. The proposed reference to a strict majority at a feasible center omits this case because setting the center to 1 gives zero retained mass. The expression for the best broad-scale uniform, `(b-m)^(-n)`, in the general power proof is expressly for nonconstant data and cannot be evaluated as a finite threshold here. [Power and shrinking-support derivation](2026-11-27-trunc-ls-math-report.md#power-tails-and-critical-collapse) · [Held-scale half-Cauchy](2026-11-27-trunc-ls-math-report.md#hcauchy-sigma-truncateknown)

This is the only extra degenerate-support end. Feasibility gives location at most the sample minimum. Except for a constant at the upper cut, the retained width on windows with a finite upper cut stays at least the distance from that minimum to the cut. For the other sequences the power envelope applies, with the half-law constraint giving a fixed inward integration segment after rescaling near collapse. This preserves the iff and exact grades for the corrected claims.

### Whole-line windows

A fixed window may have two infinite bounds under the modifier convention. The source report treats bounded windows and rays. On the whole line the conditional model is the parent. For fixed-scale Laplace the normalizer is independent of location, and minimizing the sum of absolute residuals gives the full sample median interval. For example, all locations in `[0,2]` maximize for sample `(0,2)` at any positive scale. The finite-gap strict convexity argument needs a finite cut, so the exact row now includes the median interval. For free-scale Laplace on the whole line the rate at a median maximizes `n log(r/2) - r sum(abs(x_i - median))`, attaining for nonconstant data and diverging for constants. [Laplace gap argument](2026-11-27-trunc-ls-math-report.md#laplace-sigma-truncateknown)

For logistic on the whole line, the broad-scale density is bounded by `r/4`, fixed positive rates give vanishing densities at escaping locations, and the sharp-rate bound for nonconstant data still holds by comparison with normalized Laplace. Consequently the only threshold for nonconstant data with free scale is zero, as it is for every sample with held scale. The projection is the held location for the held-location collapse test. The rational score uses `L=0` and `U=Inf` and has the same polynomial reduction. For Gumbel, the source's small-rate bound `q<=r` applies with infinite upper cut and the large-rate product bound still applies since `D=Inf` and `S` dominates the spread of the transformed sample. With rate tending to a finite positive value both remote locations lose by the ordinary parent density. Hence both-free nonconstant samples attain, and at held scale the transformed mean has an infinite upper threshold and lies strictly above zero. [Logistic likelihood](2026-11-27-trunc-ls-math-report.md#logis-all-free-truncateknown) · [Gumbel transform and bounds](2026-11-27-trunc-ls-math-report.md#gumbel-all-free-truncateknown)

For full-support power kernels on the whole line, the retained segment of length comparable to the quadratic radius still bounds the density at each observation by a constant over that radius. Consequently the broad and remote ends lose, and finite-center small-scale paths have the same interior collapse as on proper windows. For half families the whole-line effective interval is the same as on a right ray whose lower cut lies below the location. The unrestricted location branch reduces to the sample minimum, with the held-location formulas using its unbounded effective interval. Uniform continues to use the shortest effective interval, with no finite window cut able to clip a parent endpoint. [Power-tail bound](2026-11-27-trunc-ls-math-report.md#power-tails-and-critical-collapse) · [Half-normal branches](2026-11-27-trunc-ls-math-report.md#hnorm-all-free-truncateknown) · [Uniform endpoints](2026-11-27-trunc-ls-math-report.md#unif-all-free-truncateknown)

### Finite candidates and nonuniqueness

At a finite candidate whose likelihood equals the largest reachable excluded height, either the candidate already attains or a strictly better finite point confines a maximizing subsequence to the feasible domain. A finite maximum is necessarily a candidate and dominates the excluded height. This argument requires upper semicontinuity at retained support breakpoints and the face bounds for all escaping subsequences. Closed support makes location equal to the sample minimum feasible in the nondegenerate half-law branch. The Laplace and logistic bounds apply uniformly in the location at both scale ends. In particular, sending location and scale to infinity together cannot create a nonuniform tilt in these two families. For Gumbel the corresponding broad-scale joint path can give a positive increasing tilt, controlled by `v = r exp(r(mu-b))` at finite upper cut. For fixed-power kernels the diverging quadratic radius makes fixed-point log ratios vanish, including joint location and scale escapes. The finite-center critical collapse calculation compares the joint sample product and retains the outward cut factor, even when the weak law on that path is an atom. [Closure lemma](../ml-behaviour.tex#lem:closure) · [Conditional scalar and constrained tilt results](2026-11-27-trunc-expfam-math-report.md#general-theorem) · [Source face proofs](2026-11-27-trunc-ls-math-report.md#general-lemmas) · [Student scale convention](../../../univariateML-cpp/include/cppunivariate/distributions/std/README.md#details)

The held-scale logistic score is rational in the positive transformed location, with a nonzero cleared polynomial giving finitely many positive roots. The vanishing polynomial gives a flat likelihood. Ranking roots remains one-sided cardinality coverage. Likewise the two half-normal branches have at most one finite winner each, and comparing those branches gives a bound of two without deciding distinct ties. Held-scale Laplace has explicit maximizing candidates from cuts, sample locations and plateaus on proper windows and the median interval on the whole line.

The four proposed nonuniqueness examples are exact. For the Cauchy and held-df Student witnesses, the symmetric-window normalizer decreases strictly with positive location because its derivative is the difference of the radial kernel at the farther and nearer ends. The likelihood at 4 thus exceeds the centered likelihood by at least `(17^2/65)^p`, with `p=1,2` respectively. The whole-line normalizer bounds 4 and 2 give strict rational lower likelihood bounds above `1/200^2`. Compactness then gives a finite maximum away from zero, and reflection gives a second one. The Student hold is `nu=3, sd=1`, and the Cauchy hold is scale 1. For Laplace on the right ray, strict convexity of each interior location gap and the location score above the sample maximum reduce the fixed-rate comparison to the plateau and the upper sample point. The latter has ratio `(2-exp(-r))^(-2)<1` to the plateau, whose log likelihood `2 log(r)-r` has its sole maximum at 2. For logistic with held scale the endpoint sample cancels both transformed-location factors in the joint product, so every location is a global maximizer. [Exact arithmetic replay](2026-11-27-mod-rev4-data/exact_checks.py) · [Reflected witnesses](2026-11-27-trunc-ls-math-report.md#std-nusd-truncateknown) · [Laplace witness](2026-11-27-trunc-ls-math-report.md#laplace-all-free-truncateknown) · [Logistic witness](2026-11-27-trunc-ls-math-report.md#logis-scale-truncateknown)

## Rows not joined

Every proposed key is in the join file. No parent mask, modifier eligibility or written evidence anchor failed validation. The file is ready for the coordinator's join.

## Open by family

| Family | Open question |
| --- | --- |
| Laplace | Reduce the bounded all-free witness test to a direct data condition and characterize its complete location and rate maximizing set beyond the proved plateau example. |
| Logistic | Isolate and compare all potentially winning inverse-scale roots in the two free-scale masks, and characterize ties between distinct held-scale polynomial roots beyond the flat case. |
| Cauchy | Derive direct comparisons for the exterior and critical faces and characterize finite cardinality in all three nontrivial masks. |
| Gumbel | Determine the number and global values of rate roots in both free-scale masks, including any distinct maximizing rates. |
| Half-normal | Determine whether separated branch optima can have exactly equal globally winning likelihood, and characterize those samples if they exist. |
| Half-Cauchy | Resolve winning-root cardinality and simplify the exterior and critical thresholds, especially with the lower cut finite where the exterior branch is available. |
| Student | Characterize finite cardinality and critical-to-finite comparison with held df. For free df, bound the joint shape, location and effective-scale ends uniformly, especially the equality count at df 2 and varying-location Gaussian limits. The strict divergent-tie cases are proved. |
| Uniform | The retained-interval argument settles all four hold masks on positive-mass fixed windows. |

## Checks

On the workstation, the TASK-449 path script ran from a scratch copy and reproduced all 40 high-precision checks byte for byte. Those path comparisons illustrate the formulas. The accepted grades use the analytic bounds and identities in the proofs. The independent exact arithmetic replay passed for all four nonuniqueness examples. The join validator checked both atlas axes, every cited anchor, the schema and uniqueness of the reviewed keys. No builds were required. [Validator](2026-11-27-mod-rev4-data/validate.py)

The repository modifier-proof validator passed. The Markdown link checker reported 47 inherited broken destinations and one otherwise failing destination, the known `P+Q` false positive at line 461 of the beta-prime report. It found no broken destination in this review. The other known false positive under the Lean package cache was absent from this run.
