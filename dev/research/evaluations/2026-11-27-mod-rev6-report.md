# Independent review of truncation proofs for positive-shape families

TASK-456 reviewed 122 proposals from TASK-450 across nine families and 40 held masks. The [merge input](2026-11-27-mod-rev6-data/reviewed.csv) contains **104 rows** (95 accepted and 9 regraded). It contains 38 iff rows, 55 exact rows and 11 one-sided rows. Eighteen rows were rejected.

**Two escape classifications need correction.** Fréchet with shape held admits an omitted increasing exponential on an upper-only window. Pareto II with shape and scale free can have unbounded likelihood even while probability mass escapes to infinity. The respective rows below retain the scopes proved by the cited arguments.

The review uses the closed known window intersected with the parent support, with density from the inside at an included moving endpoint. For example, the Power example uses the known cut `[0,2]` and the parent excludes zero. Pareto II includes zero whenever zero is in the parent support and the known cut. Fréchet has zero density at a location contact for finite positive scale and shape. The free-shape restriction at one applies when location is free. All 122 keys have eligible parent masks. The previously reviewed Weibull shape-only regime is outside the proposal, as is Gamma. [TASK-450 proof](2026-11-27-trunc-shape-math-report.md) · [Atlas conventions](../existence-atlas/README.md) · [Modifier design](../../design/modifiers.md)

The scalar rows use the moment equation on a positive-width transformed interval with nonconstant statistic and a parameter-independent carrier. At a finite threshold the score keeps its strict sign in the admissible interior, including when the empirical mean equals the threshold. For the comparison rows, every regular height used in the retained scope is approachable or is the supremum of approachable heights. A feasible candidate equal to the best escape height ensures attainment. If a higher value exists, a positive superlevel above the escape bound confines a maximizing sequence to the feasible parameter region. Strict inequality against every finite candidate is required for a finite excluded face to win. [Scalar theorem](2026-11-27-trunc-expfam-math-report.md#scalar-and-two-coordinate-checks) · [Stationary comparison](2026-11-27-trunc-shape-math-report.md#conditional-likelihood-and-comparisons)

## Row verdicts

| Family | Held | Question | Verdict | Reason |
| --- | --- | --- | --- | --- |
| frechet | all free | case:half-at-lower-cut | accepted | The strict hyperbolic-sine pair bound is sharp on the joint small-gap and small-q path and was checked by rational enclosures. |
| frechet | all free | criterion | accepted | The majority divergence and sharp half-at-cut result give the stated necessary condition. |
| frechet | all free | failure | accepted | Shape one gives a negative contact exponent for strict minimum majorities and the normalized law collapses at the minimum. |
| frechet | all free | path:critical-contact | regraded | The exact critical exponent cancels at shape one, leaving a finite positive height on a fixed positive-q path, and the atlas requires a case key for this path. |
| frechet | all free | path:gumbel | regraded | The scaled log excess tends to the Gumbel coordinate and both the densities and the retained CDF difference converge, and the atlas requires a case key for this path. |
| frechet | lambda | criterion | regraded | The contact exponent and bounded-q envelope give the exhaustive comparison on windows with a finite lower cut. |
| frechet | lambda | failure | regraded | The stated list holds with a finite lower cut, and an upper-only window admits the omitted increasing exponential. |
| frechet | lambda | uniqueness | rejected | The score comparison gives a necessary candidate description without proving its cardinality or further structure. |
| frechet | lambda+mu | criterion | accepted | The negative exponential tilt in inverse-power excess has a strictly decreasing rate score and the stated endpoint means. |
| frechet | lambda+mu | failure | accepted | Zero rate yields the inverse-power carrier when integrable, and infinite rate concentrates at the included upper physical cut. |
| frechet | lambda+mu | uniqueness | accepted | The conditional scalar score is strictly monotone throughout the admissible tilt range. |
| frechet | lambda+mu+sigma | criterion | accepted | The specified vector is feasible exactly for a compatible sample in a positive-mass window. |
| frechet | lambda+mu+sigma | failure | accepted | An empty retained event or an incompatible observation prevents evaluation at positive likelihood. |
| frechet | lambda+mu+sigma | uniqueness | accepted | All coefficients are supplied and consequently the feasible positive-likelihood maximizing set is a singleton. |
| frechet | lambda+sigma | criterion | accepted | Contact below the upper cut vanishes at fixed scale and shape, and the far-left location gives the sole integrable uniform edge. |
| frechet | lambda+sigma | failure | accepted | Only the finite-window uniform can win at finite height, and constant data at the upper cut admit width collapse. |
| frechet | lambda+sigma | uniqueness | rejected | The score comparison gives a necessary candidate description without proving its cardinality or further structure. |
| frechet | mu | criterion | accepted | Reversing log excess gives the Gumbel kernel with both free coordinates and the signed tilt comparison includes equality. |
| frechet | mu | failure | accepted | The Gumbel slope analysis exhausts competitive distinct-data limits and free centering yields constant-sample collapse. |
| frechet | mu | uniqueness | rejected | The score comparison gives a necessary candidate description without proving its cardinality or further structure. |
| frechet | mu+sigma | criterion | accepted | The center is fixed in reversed log excess, so the projected-center and small-shape cases exhaust the ends. |
| frechet | mu+sigma | failure | accepted | Only bounded log-excess windows admit the zero-shape regular face, and large shape collapses at the projected scale. |
| frechet | mu+sigma | uniqueness | rejected | The score comparison gives a necessary candidate description without proving its cardinality or further structure. |
| frechet | sigma | criterion | accepted | The held scale leaves the decreasing exponential as the only positive regular limit, with feasible shape one included among candidates. |
| frechet | sigma | failure | accepted | Large shape at bounded location and sample contact vanish for distinct data, leaving the integrable far-left exponential and constant collapse. |
| frechet | sigma | uniqueness | rejected | The score comparison gives a necessary candidate description without proving its cardinality or further structure. |
| gompertz | all free | criterion | accepted | The physical Gumbel coordinate covers all joint paths in b and a/b, and a finite tie qualifies. |
| gompertz | all free | failure | accepted | The Gumbel escape cases leave signed exponentials where integrable or collapse of constant data. |
| gompertz | all free | uniqueness | rejected | The score comparison gives a necessary candidate description without proving its cardinality or further structure. |
| gompertz | a | criterion | accepted | The b-to-zero rate-a face and large-b cases exhaust both ends, with the all-zero plateau included. |
| gompertz | a | failure | accepted | The finite-upper all-zero case selects the excluded exponential, while an unrestricted upper end gives the exact all-zero plateau. |
| gompertz | a | uniqueness | regraded | The all-zero unrestricted-upper likelihood is exactly constant in b, giving a proved refutation while general cardinality stays open. |
| gompertz | a+b | criterion | accepted | The specified vector is feasible exactly for a compatible sample in a positive-mass window. |
| gompertz | a+b | failure | accepted | An empty retained event or an incompatible observation prevents evaluation at positive likelihood. |
| gompertz | a+b | uniqueness | accepted | All coefficients are supplied and consequently the feasible positive-likelihood maximizing set is a singleton. |
| gompertz | b | criterion | accepted | The statistic exp(bx) has a uniform transformed carrier and a strictly decreasing rate score. |
| gompertz | b | failure | accepted | Infinite rate gives lower collapse and zero rate on a finite transformed interval gives the positive exponential in x. |
| gompertz | b | uniqueness | accepted | The conditional scalar score is strictly monotone throughout the admissible tilt range. |
| invexp | all free | criterion | accepted | The reciprocal coordinate has a uniform carrier and the scalar exponential moment test applies on all retained intervals. |
| invexp | all free | failure | accepted | Infinite rate gives lower reciprocal collapse and zero rate gives the inverse-square density when integrable. |
| invexp | all free | uniqueness | accepted | The conditional scalar score is strictly monotone throughout the admissible tilt range. |
| invexp | scale | criterion | accepted | The specified vector is feasible exactly for a compatible sample in a positive-mass window. |
| invexp | scale | failure | accepted | An empty retained event or an incompatible observation prevents evaluation at positive likelihood. |
| invexp | scale | uniqueness | accepted | All coefficients are supplied and consequently the feasible positive-likelihood maximizing set is a singleton. |
| kumar | all free | criterion | accepted | The two small-parameter edges and their joint corner exhaust regular limits, and the large-shape estimates exclude distinct-data competitors. |
| kumar | all free | failure | accepted | The zero-zero corner is the integrable common reciprocal edge, while large shapes can give constant-sample collapse. |
| kumar | all free | uniqueness | rejected | The score comparison gives a necessary candidate description without proving its cardinality or further structure. |
| kumar | a | criterion | accepted | The log-survival transform gives a positive tilt with uniform carrier and the stated zero-tilt and upper-mean thresholds. |
| kumar | a | failure | accepted | Zero tilt on a finite transformed window gives the reciprocal carrier and infinite tilt collapses at the included lower physical cut. |
| kumar | a | uniqueness | accepted | The conditional scalar score is strictly monotone throughout the admissible tilt range. |
| kumar | b | criterion | accepted | The fixed-b small-a power and the large-a endpoint estimate control both ends, including constants away from the favored cut. |
| kumar | b | failure | accepted | The only integrable regular edge is the stated power, and large a spikes only at the included upper physical cut. |
| kumar | b | uniqueness | regraded | At b=1 the negative-log coordinate is a scalar exponential tilt with strictly decreasing rate score, while other held values remain open. |
| llogis | all free | criterion | accepted | The log-logistic slope classifies the zero-shape, diverging-center and sharp-transition paths, and equality permits a finite winner. |
| llogis | all free | failure | accepted | The slope bound excludes a competitive sharp transition for distinct data, and the other ends give signed powers or constant collapse. |
| llogis | all free | uniqueness | rejected | The score comparison gives a necessary candidate description without proving its cardinality or further structure. |
| llogis | scale | criterion | accepted | With center fixed, shape zero gives the log-uniform edge and infinite shape is controlled by projection onto the window. |
| llogis | scale | failure | accepted | The only integrable finite-height face is log-uniform, and a constant at the projected center has unbounded likelihood. |
| llogis | scale | uniqueness | rejected | The score comparison gives a necessary candidate description without proving its cardinality or further structure. |
| llogis | scale+shape | criterion | accepted | The specified vector is feasible exactly for a compatible sample in a positive-mass window. |
| llogis | scale+shape | failure | accepted | An empty retained event or an incompatible observation prevents evaluation at positive likelihood. |
| llogis | scale+shape | uniqueness | accepted | All coefficients are supplied and consequently the feasible positive-likelihood maximizing set is a singleton. |
| llogis | shape | criterion | accepted | The fixed-shape power kernel in x to the held shape reduces both parameter ends to explicit densities or zero. |
| llogis | shape | failure | accepted | The two translated-power limits pull back to the stated powers, eligible only when integrable. |
| llogis | shape | uniqueness | rejected | The score comparison gives a necessary candidate description without proving its cardinality or further structure. |
| lomax | all free | criterion | accepted | The joint rate-to-scale regimes, finite scale at shape zero and small-scale positive-data bound control all competing ends. |
| lomax | all free | failure | accepted | The normalized lower-cut kernel yields the three regular limits plus all-lower-cut divergence when the cut is included. |
| lomax | all free | uniqueness | rejected | The score comparison gives a necessary candidate description without proving its cardinality or further structure. |
| lomax | kappa | criterion | accepted | With shape held, only the two scale ends need comparison and their limiting likelihoods are explicit. |
| lomax | kappa | failure | accepted | Finite positive lower cut admits the truncated power, finite upper cut admits uniform, and the other ends vanish. |
| lomax | kappa | uniqueness | rejected | The score comparison gives a necessary candidate description without proving its cardinality or further structure. |
| lomax | kappa+lambda | criterion | accepted | The specified vector is feasible exactly for a compatible sample in a positive-mass window. |
| lomax | kappa+lambda | failure | accepted | An empty retained event or an incompatible observation prevents evaluation at positive likelihood. |
| lomax | kappa+lambda | uniqueness | accepted | All coefficients are supplied and consequently the feasible positive-likelihood maximizing set is a singleton. |
| lomax | lambda | criterion | accepted | The log excess transform is a negative tilt with zero-shape mean fixed by the transformed interval. |
| lomax | lambda | failure | accepted | Infinite shape collapses at an admissible lower cut and the integrable zero-shape edge gives the reciprocal kernel. |
| lomax | lambda | uniqueness | accepted | The conditional scalar score is strictly monotone throughout the admissible tilt range. |
| pareto2 | all free | criterion | accepted | The free pole and a sufficiently small held-along-path shape always give lower divergence, with width collapse when the minimum equals the upper cut. |
| pareto2 | all free | failure | regraded | The displayed paths prove divergence but do not exhaust maximizing escapes, since vanishing scale and shape can send mass to infinity. |
| pareto2 | all free | uniqueness | accepted | The divergent likelihood gives an empty maximizing set on every compatible sample. |
| pareto2 | min | criterion | accepted | The shifted Lomax comparison applies away from diverging threshold contact, where a fixed small shape already proves nonattainment. |
| pareto2 | min | failure | regraded | The stated paths prove divergence and the regular faces cover nondivergent samples, but joint zero shape and scale can send mass to infinity. |
| pareto2 | min | uniqueness | rejected | The score comparison gives a necessary candidate description without proving its cardinality or further structure. |
| pareto2 | min+scale | criterion | accepted | The held threshold and scale fix the log excess transform and its two scalar moment thresholds. |
| pareto2 | min+scale | failure | accepted | Closed threshold contact permits lower collapse and zero shape gives the translated reciprocal on finite windows. |
| pareto2 | min+scale | uniqueness | accepted | The conditional scalar score is strictly monotone throughout the admissible tilt range. |
| pareto2 | min+scale+shape | criterion | accepted | The specified vector is feasible exactly for a compatible sample in a positive-mass window. |
| pareto2 | min+scale+shape | failure | accepted | An empty retained event or an incompatible observation prevents evaluation at positive likelihood. |
| pareto2 | min+scale+shape | uniqueness | accepted | All coefficients are supplied and consequently the feasible positive-likelihood maximizing set is a singleton. |
| pareto2 | min+shape | criterion | accepted | The fixed-shape shifted kernel includes the exact threshold-contact height and the two regular scale endpoints. |
| pareto2 | min+shape | failure | accepted | The sign of the small-scale exponent distinguishes divergence, a finite atom envelope and vanishing, with the other scale end explicit. |
| pareto2 | min+shape | uniqueness | rejected | The score comparison gives a necessary candidate description without proving its cardinality or further structure. |
| pareto2 | scale | criterion | accepted | Monotonicity above the lower cut reduces that branch to the sample minimum, and the held-scale bound controls the remaining joint escapes. |
| pareto2 | scale | failure | accepted | The retained branches yield reciprocal and exponential faces, plus the stated constant-sample collapses. |
| pareto2 | scale | uniqueness | rejected | The score comparison gives a necessary candidate description without proving its cardinality or further structure. |
| pareto2 | scale+shape | criterion | accepted | The same threshold monotonicity leaves the sample minimum, the finite join and a single smooth branch to compare with uniform. |
| pareto2 | scale+shape | failure | accepted | The far-left threshold has a uniform face only for finite physical width, and a threshold approaching the upper cut gives width collapse. |
| pareto2 | scale+shape | uniqueness | rejected | The score comparison gives a necessary candidate description without proving its cardinality or further structure. |
| pareto2 | shape | criterion | accepted | Raising the threshold at fixed pole reduces the fit to fixed-shape lower-cut powers with a possible exact contact envelope. |
| pareto2 | shape | failure | accepted | The reduced scale ends and the shrinking-width branch give the uniform, finite contact and divergent cases. |
| pareto2 | shape | uniqueness | regraded | The unique maximizing pole follows from an exact factorization and a continuum of threshold-scale pairs realizes it. |
| power | all free | criterion | accepted | Profiling the effective upper endpoint reduces the problem to the two strict log-mean inequalities, with shrinking width excluded separately. |
| power | all free | failure | accepted | The endpoint profile distinguishes zero width, infinite shape at upper equality and the integrable log-uniform face at the lower threshold. |
| power | all free | uniqueness | accepted | Strict log-tilt concavity fixes shape and the endpoint profile gives the whole alpha ray when the sample maximum is the upper cut. |
| power | alpha | criterion | accepted | The fixed effective upper endpoint leaves a strictly concave positive log tilt with these two thresholds. |
| power | alpha | failure | accepted | The excluded zero shape gives log-uniform when integrable and infinite shape collapses at the included upper endpoint. |
| power | alpha | uniqueness | accepted | The conditional scalar score is strictly monotone throughout the admissible tilt range. |
| power | alpha+beta | criterion | accepted | The specified vector is feasible exactly for a compatible sample in a positive-mass window. |
| power | alpha+beta | failure | accepted | An empty retained event or an incompatible observation prevents evaluation at positive likelihood. |
| power | alpha+beta | uniqueness | accepted | All coefficients are supplied and consequently the feasible positive-likelihood maximizing set is a singleton. |
| power | beta | criterion | accepted | The positive kernel integral is strictly increasing in effective width and the smallest feasible width is attained exactly above zero. |
| power | beta | failure | accepted | An included lower-cut constant sample has a fixed positive numerator and arbitrarily small retained mass. |
| power | beta | uniqueness | accepted | The conditional kernel fixes the effective endpoint at the sample maximum and its inverse image in alpha is explicit. |
| weibull | all free | criterion | accepted | The log-coordinate Gumbel cases exhaust joint scale and shape escapes and the stationary comparison covers a finite tie. |
| weibull | all free | failure | accepted | Only eligible signed log tilts have positive regular limiting height for distinct data, while free centering gives constant collapse. |
| weibull | all free | uniqueness | rejected | The score comparison gives a necessary candidate description without proving its cardinality or further structure. |
| weibull | scale | criterion | accepted | Fixed log scale leaves only the small-shape face and the large-shape projected-center case. |
| weibull | scale | failure | accepted | The regular face is log-uniform precisely on bounded log windows, and constants at the projected scale collapse. |
| weibull | scale | uniqueness | rejected | The score comparison gives a necessary candidate description without proving its cardinality or further structure. |
| weibull | scale+shape | criterion | accepted | The specified vector is feasible exactly for a compatible sample in a positive-mass window. |
| weibull | scale+shape | failure | accepted | An empty retained event or an incompatible observation prevents evaluation at positive likelihood. |
| weibull | scale+shape | uniqueness | accepted | All coefficients are supplied and consequently the feasible positive-likelihood maximizing set is a singleton. |

## Proof errors and scope corrections

### Fréchet with shape held misses the upper-only exponential

The far-left-location step restricts regular increasing exponentials to finite windows. Take the known window `[-Inf,0]`, held shape one, location `-D` and scale `D²`. For fixed x ≤ 0 and D sufficiently large, the retained CDF is

```text
F_D(x)/F_D(0) = exp(D - D²/(D+x)) → exp(x).
```

The conditional density tends to exp(x) as well because its additional factor is D²/(D+x)² → 1. For the compatible distinct sample (−1, 0), the joint conditional likelihood tends to exp(−1), a strictly positive escape height. Thus Fatou's divergent-normalizer argument on an infinite **upper** side fails to exclude an integrable tilt on an infinite **lower** side. The criterion and failure entries retain the finite-lower-cut scope. The missing comparison needs the upper-only face added and its dominance or equality decided against finite competitors. [Cited step](2026-11-27-trunc-shape-math-report.md#frechet-lambda-truncateknown)

The same section's unqualified assertion that contact gives an atom also needs a restriction to paths of competitive height. For instance, set shape and scale to one, location to m−d, and take d down to zero at a lower cut m. Here q=1/d goes to infinity. On a window [m,U] with U>m the CDF at a fixed x>m tends to the nondegenerate shape-one Fréchet CDF with location m, conditioned on the window. The sample likelihood at contact tends to zero. The critical-envelope proof already bounds the large-q remainder, so the contact classification at positive height and the half-at-cut result still follow. [Contact calculation](2026-11-27-trunc-shape-math-report.md#frechet-lambda-truncateknown)

### Pareto II can diverge with escaping mass

Take sample (1,2), window `[1,Inf]`, threshold 1, scale s and shape k=s^(1/4). This path is feasible with the threshold free or held at 1. The retained event has probability one. The shifted survival at a positive excess y is (1+y/s)^(-k) → 1, since k log(1+y/s) → 0. The likelihood is

```text
(k/s)² (1+1/s)^(-k-1) ~ k²/s = s^(-1/2) → infinity.
```

Every bounded excess interval loses all its probability although the sample likelihood diverges. With k=h/log(1/s), h>0, the survival instead tends to exp(−h) at every positive y and the likelihood still diverges. A lower atom can therefore carry only part of the limiting mass on the extended line. This invalidates exhaustiveness for both proposed failure rows with free shape and scale. The fixed-small-shape witness still proves nonattainment when the free pole can contact the sample minimum. With threshold held it still proves divergence when the sample contacts that threshold and the remaining attainment comparison applies. [Lower-cut power argument](2026-11-27-trunc-shape-math-report.md#positive-scale-powers-with-a-lower-cut) · [Free-threshold argument](2026-11-27-trunc-shape-math-report.md#pareto2-free-truncateknown)

The Pareto II scale-held section also mentions a genuinely open lower cut at zero. That convention falls outside `truncate:known`. The same integrals apply for compatible positive observations, and at a closed zero cut the threshold endpoint density is evaluated from inside. With lower bound −Inf and free threshold, every finite threshold lies on the rising branch, so the lower-cut join is absent and the sample-minimum branch suffices. Neither clarification changes the scale-held verdicts. [Scale-held calculation](2026-11-27-trunc-shape-math-report.md#pareto2-scale-truncateknown)

### Cardinality and exact examples

Highest-valued score roots characterize where to look for a maximizer, but this recipe alone proves neither root finiteness nor the structure of the winning set. The proposed exact uniqueness grade is unsupported on the general comparison routes. The Kumaraswamy row with b held retains its proved scalar subcase b=1. The Gompertz and Pareto II examples give one-sided refutations in their respective regimes. Power gives the entire maximizing set whenever attained, so its exact grades stand.

- **Fréchet half at the cut.** The window mass dominates each minimum-to-gap CDF increment. Dividing each pair of densities by the square of its increment gives the strict bound using sinh(z)>z at positive z and sinh(λu)≥λ sinh(u) for λ≥1. Setting λ=1, d=m−μ down to zero and q down to zero gives the claimed height. For example, the CDF difference divided by q tends to one at lower-cut contact. Rational Taylor enclosures check strict separation at two rational samples and three integer shapes. The analytic inequality covers the full real shape domain. [Half-contact proof](2026-11-27-trunc-shape-math-report.md#frechet-free-truncateknown)
- **Power.** For (1,2) on the closed cut [0,2], every feasible α≥2 gives effective upper end two. The likelihood is β² 2^(−β−1). Its log has second derivative −2/β², so β=2/log(2) is globally optimal at both α=2 and α=3, and at every other feasible α. Holding β fixes the likelihood throughout the same ray. [Endpoint proof](2026-11-27-trunc-shape-math-report.md#power-free-truncateknown)
- **Pareto II.** With shape two held, (1,2) on [1,∞) has reduced likelihood 4s/(1+s)³. The numerator of its gap from 16/27, with denominator 27(1+s)³, is (2s−1)²(4s+16). Thus the sole winning s is 1/2, and thresholds 3/4 and 1 with scales 1/4 and 1/2, respectively, both realize it. [Pole proof](2026-11-27-trunc-shape-math-report.md#pareto2-shape-truncateknown)
- **Gompertz.** For (0) on [0,∞) with a=2 held, the normalizer is one and the exponent at zero vanishes exactly. The likelihood is two at every feasible b, including b=1 and b=2. [Endpoint proof](2026-11-27-trunc-shape-math-report.md#gompertz-a-truncateknown)

## Rows omitted from the merge input

All 18 omitted rows ask for uniqueness. The held masks are listed below. The existing modifier inventory has no eligible-parent failures to follow up. The original Fréchet `path:critical-contact` and `path:gumbel` keys are also absent as written because modifier proofs accept named `case:` keys. Their one-sided claims are included as `case:critical-contact` and `case:gumbel`.

| Family | Rejected uniqueness masks |
| --- | --- |
| Fréchet | lambda, lambda+sigma, mu, mu+sigma, sigma |
| Gompertz | all free |
| Kumaraswamy | all free |
| Log-logistic | all free, scale, shape |
| Lomax | all free, kappa |
| Pareto II | min, min+shape, scale, scale+shape |
| Weibull | all free, scale |

## Open questions by family

The stationary comparisons accepted here are analytic criteria. Implementable certificates for them would still need global root exclusion and control of ties, including ties approached from arbitrarily close to an excluded face. This is common to the comparison routes listed below.

| Family | Open for known windows in the reviewed domain |
| --- | --- |
| Fréchet | For the all-free regime, complete the compactification for minimum minorities and exact halves above the lower cut, including joint shape, location and scale escapes and optimization on the Gumbel face. With shape held, complete the upper-only comparison with the increasing exponential. Prove finite cardinality on the free and comparison masks. |
| Gompertz | Prove finite cardinality with both coefficients free and outside the exact all-zero plateau with a held. |
| Inverse exponential | No open attainment, failure or cardinality row in the reviewed domain. |
| Kumaraswamy | Prove finite cardinality in the all-free regime and with b held at values other than one. |
| Log-logistic | Prove finite cardinality with both coefficients free, scale held or shape held. |
| Lomax | Prove finite cardinality with both coefficients free or kappa held under the parameter-dependent conditional normalizer. |
| Pareto II | Classify the remaining divergent ends for free shape and scale, with threshold free or held. Prove finite cardinality for the min, min+shape, scale and scale+shape masks and characterize the winning poles with shape held beyond the proved example. |
| Power | No open attainment, failure or cardinality row in the reviewed domain. |
| Weibull | Prove finite cardinality with both coefficients free or scale held. |

## Checks

The workstation single-slot replay of the TASK-450 script reproduced all 68 numeric check records byte for byte. Those records use high precision and serve as spot checks. The [exact review script](2026-11-27-mod-rev6-data/check_exact.py) passed its rational identity and enclosure checks independently. The [review validator](2026-11-27-mod-rev6-data/validate.py) checked all 104 merge keys, eligible parent masks, provenance and evidence anchors. The atlas's own alternate modifier-proof input check passed with the two path results expressed as named cases.

The workstation link check found 47 destinations already recorded as inherited and flagged only the known false positive at `2026-11-25-betapr-math-report.md:461` as new. It reported no destination from this review. The job required no builds.
