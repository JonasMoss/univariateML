# Inference classification, 2026-10-31

This inventory assigns each of the 69 previously unreviewed families a
conditional classification or an explicitly stated open problem. An `open`
entry is not a finding of non-DQM. Most joint information ranks and estimator
limits remain open. The historical method scan is a name-based source
inventory, not a full semantic recovery.
The relevant model and classification requirements are in the
[inference roadmap](../../roadmap/2026-10-30-inference-and-dqm.md).

Source revision `b85422e7a006134909b24a5bce45f5caa0e3f871`; the existing
inventory generator is tied to model metadata at
`c61a4979c6169388dea97c6b821b3bd89e530dfd` and compares historical sources
at `bf5a3ac^`. No claim here establishes the asymptotic distribution of the
package's fitted estimate; consistency, the numerical optimizer and boundary
behavior need separate work. All likelihood derivatives below are derivatives
of the original-data log likelihood in the stated coordinates, and information
is per observation. No inference implementation is implied.

## Shared argument and families examined

For a regular exponential family on fixed countable support, a finite
normalizer in a neighborhood gives square-integrable scores and DQM via
termwise differentiation. Information is the covariance of the sufficient
statistics; it is nonsingular precisely when no nonzero linear combination of
them is constant on the support. A smooth one-to-one parameter change with
nonsingular Jacobian preserves these conclusions. For a finite support the
needed differentiation is immediate; for CMP with nu>0 the factorial term
dominates every exponential in k uniformly in a sufficiently small neighborhood.

| Family and free/held coordinates | DQM and finite information | Rank and likelihood | Evidence |
|---|---|---|---|
| CMP: lambda>0, nu>0; either or both free | Yes; fixed countable support | Full rank; smooth pointwise likelihood | Proved: statistics k and log(k!) are not affinely dependent on k=0,1,2. Natural coordinates are log(lambda), -nu. Lambda=0 and nu=0 are excluded boundary regimes. |
| CMB: known integer size N>=2, 0<p<1, finite nu; either or both free | Yes | Full rank; smooth pointwise likelihood | Proved: statistics k and log choose(N,k) are not affinely dependent: the first is nonconstant and the second is zero at 0 and N but positive at 1. N=1 identifies p only, and N=0 is a point mass. Unknown N has no Euclidean local coordinate. |
| Folded normal: mu>0, sigma>0, either or both free | Yes | Full rank; smooth pointwise likelihood | Proved: folding a normal is a fixed measurable transformation, preserving DQM; the two folded scores are independent by their growth as x tends to infinity. At mu=0 the mu score is zero, so the full information is singular; sigma remains identifiable. The nonnegative-mu domain is one-sided at zero. |
| Inverse exponential: scale>0 | Yes | Positive information; smooth likelihood | Proved: x maps to 1/x, giving an exponential with rate equal to scale; the transformation is fixed. |
| Cauchy: location real, scale>0, either or both free | Yes | Full rank; information diag(1,1)/(2 scale²); smooth likelihood | Proved: standardized scores are 2z/(scale(1+z²)) and (z²-1)/(scale(1+z²)); odd/even symmetry gives zero cross term and the Cauchy integrals give each diagonal. A locally integrable envelope for the square-root derivatives gives DQM. |

The folded-normal score in mu is `(-mu+x*tanh(x*mu/sigma²))/sigma²`;
the sigma score has a nonzero x² leading term as x tends to infinity,
whereas the mu score grows only linearly. Thus no nontrivial linear combination
vanishes almost everywhere for mu>0. At mu=0 symmetry makes the mu score zero;
this does not by itself give the boundary estimator's rate or limit law.

## Historical derivative evidence

The generator adds historical named-gradient paths and the outcome of each
available `quality/derived/<id>/hessian_report.json` from the historical
snapshot. These are finite symbolic-grid comparisons of historical cost
implementations, not a validation of per-observation scores, Fisher
information, the current parameterization, or inference calibration. Absence
of a named method is not absence of finite-difference derivatives. A full
audit of individual objective coordinates, normalization, profile semantics,
and special Fisher-instead-of-Hessian cases remains open. For example, the
historical beta-prime cost is a two-coordinate cost, omitting the current scale
coordinate; its historical report passes 68 comparisons, which cannot validate
the full current three-parameter model. Historical Laplace's location entry
must not be read as an ordinary observed Hessian; see the roadmap.

## Candidate first hypotheses

With independent observations under the correctly specified model, evaluate
normal mean equal to a specified value with variance fitted, gamma mean or
shape equal to a specified value with the other natural parameter fitted,
beta mean equal to a specified value with concentration fitted, and Poisson
rate equal to a positive specified value with no nuisance parameter. Compare
model Wald, score and profile likelihood procedures, and exact normal and
Poisson procedures where appropriate. Derivative checks, nuisance cross terms,
and simulation calibration are required before claiming availability. These
are interior nulls; misspecification-robust procedures require separate
conditions and calibration.

## Open work

The table below makes the remaining gaps explicit. In particular, joint rank
for many multiparameter models, full held-subset boundary analysis, and
estimator limits require further work. No numerical information-matrix spot
checks have been performed; positive rank claims below are instead supported
by the indicated analytical arguments. Historical optimizer coordinates and
objective scaling still require individual source-level verification.

## Shared arguments for the remaining families

The grade `proved` denotes the explicit argument given here or in the five-row
table above; `argued` identifies a route that still needs the indicated
family-specific domination details checked; `conditional` gives distinct
parameter regimes; `open` states a question for which no conclusion is
claimed. The CSV includes the exact coefficients and held designs. A DQM
assertion includes finite score covariance, but does not imply invertibility.
For a held subset, use the corresponding score subvector and principal
information block, subject to the stated support conditions. An open rank for
the full vector is not a claim that every submodel has deficient information.

**Location and scale.** For p(x)=f(z)/sigma, z=(x-mu)/sigma, an L2-continuous
derivative of sqrt(f) in location and scale establishes DQM. The scores are
`-f'(z)/(sigma*f(z))` and `-(1+z*f'(z)/f(z))/sigma`. The logistic, Gumbel,
Student and slash bases have finite squared score integrals; locally dominated
derivatives give the stated DQM (for Student use df>0, or df>2 in the
standardized-sd parametrization). Symmetric bases have an odd location score
and an even scale score, each nonzero, hence a nonsingular 2-by-2 block.
For Gumbel the two scores, proportional to `1-exp(-z)` and
`-1+z*(1-exp(-z))`, are linearly independent. Fitted degrees of freedom add a
separate rank question. Slash's apparent formula singularity at z=0 is removable.
The argument does not apply to a translated half-distribution's free endpoint.

**Fixed positive support and transformations.** On an open positive parameter
neighborhood, the indicated families have power or exponential tails and
scores bounded by integrable combinations of powers and logarithms; this is
an argument for DQM, not a proof of joint score independence. A fixed
one-to-one transformation preserves DQM and rank. In particular, log Weibull
is a reflected Gumbel with scale 1/shape; log-logistic is logistic;
reciprocals link inverse Weibull to Weibull and inverse gamma to gamma; and
the square of Nakagami is gamma with shape `shape` and scale `scale/shape`.
The inverse-Gaussian sufficient statistics are x and 1/x and its map from
(mean,shape) to their natural coefficients is locally invertible. In each case
the natural statistics are not affinely dependent on the support. The GB2,
Burr, beta-prime and transformed-gamma families have more shape/scale
coordinates: the tail argument does not establish their joint rank.

**Moving endpoints.** If the density at distance y from an endpoint behaves
as C y^(a-1), a shift by h loses mass of order |h|^a. For a<=2 this prevents
the required o(h²) support-loss condition when a<2; at a=2 the endpoint
score has a nonintegrable logarithmic divergence. When a>2, the squared
endpoint score is locally integrable and a dominated square-root derivative
argument gives DQM under the smoothness and tail conditions of the listed
power-endpoint models. Thus the relevant exponents are shape2 for generalized
beta and Pareto IV, shape for Pareto III, shape2*shape3 for Feller Pareto,
and the corresponding endpoint shape for nonstandard beta. Fixed endpoints
remove the moving-support direction. Fréchet and fatigue-life have exponential
decay at their finite endpoint, so a blanket non-DQM rule for thresholds would
be incorrect. Rank is not established by these arguments. For GEV and GPD see
the cited [Bücher–Segers paper](https://arxiv.org/html/1601.05702v3) and the
roadmap; the exact threshold and nuisance conditions there matter.

**Counts, truncation, and mixtures.** With fixed countable support, DQM follows
when a local summable envelope controls square-root derivatives. Geometric or
stretched-exponential tails, and power tails with positive exponent plus
logarithmic score factors, provide the indicated argument; the discretized
normal and gamma inherit DQM by fixed measurable binning. The resulting rank
can still be lost by binning and is marked open where not separately proved.
Finite-support truncation is smooth for fixed retained sets. A single retained
point has zero information; two or more give positive probability information
in one-parameter binomial or Poisson exponential submodels. Real-valued
truncation cutoffs are locally constant between integer crossings, and jump
at crossings; they are not regular continuous coordinates.

For zero modification, the zero indicator and the score within the positive
conditional distribution are orthogonal, yielding positive information for
the weight and for any identifiable positive-count parameter. For zero
inflation, ratios of positive-count probabilities can identify the base before
the zero mass identifies the weight; this proves the ZIP claim but must be
checked individually elsewhere. Inflation probabilities 0 and 1 are
one-sided boundary cases; at weight 1 the base parameters are unidentified.
Known finite design size one has only two possible outcomes, limiting rank to
one regardless of the number of free continuous parameters. Numerically
defined normalizers do not alter these mathematical arguments, but their
numerical derivatives and truncation errors need separate validation.

**Special models.** Huber's root density has matching first derivatives at its
moving knots, so piecewise differentiation gives an L2 argument; ordinary
second derivatives at the knots need separate care. EMG and Bhattacharjee
are convolutions of normal with, respectively, exponential and uniform
variables, supplying fixed-support DQM arguments away from the Bhattacharjee
zero-width boundary. At that boundary the width score vanishes by symmetry.
Two-piece models require a separate cusp calculation: for exponential power
shape nu the squared location score is locally of order |z|^(2nu-2),
integrable only for nu>1/2 in a cusp-moving direction. The standardized
two-piece normal (`snorm`) is a different family from Azzalini's `sn`.
Tukey lambda requires its quantile-dependent support to be handled directly;
the Poisson inverse Gaussian additionally needs a controlled score-tail bound.

Pointwise first derivatives are available away from moving endpoints and
piecewise knots in the indicated interiors. Second derivatives and information
identities are not asserted by DQM; knot contributions or support terms may be
missing. For each fitter, consistency, existence, the fit algorithm and its
accuracy, and the relevant local limit remain unproved here. Standard
root-n Gaussian MLE conclusions are candidates only after those checks and
positive information; nonregular and singular cases require different limits.

## Family-by-family classification

The first column names the dossier and the grade refers to the stated regime. Rank marked open is not assumed nonsingular. The model parameters are those in the CSV; all unmentioned boundary faces and estimator limits remain open.

| Family | Grade | Free coordinates and conditions | Rank or excluded regime | Argument |
|---|---|---|---|---|
| bbinom | argued | size N known integer >=2; alpha,beta>0 | open: prove score independence for N>=2; N=1 rank one; N=0 rank zero | finite |
| betapr | argued | shape1,shape2,scale>0 | open: establish joint shape/scale score independence | positive |
| bhatt | argued | sigma>0, a>0, mu real | open: joint rank; a=0 is normal boundary and a score vanishes | individual |
| bnbinom | argued | size,alpha,beta>0 | open: joint rank and parameter coincidences | count |
| burr | argued | shape1,shape2,scale>0 | open: joint shape/scale score independence | positive |
| cauchy | proved_interior | location real; scale>0; either or both free | nonsingular; diagonal information 1/(2*scale^2) | inference_1_report |
| cmb | proved_conditional | size known integer >=2, 0<prob<1, finite nu; both free or any held subset; size=1 and size=0 exceptional | nonsingular for size>=2; dispersion unidentified for size<=1 | inference_1_report |
| cmp | proved_interior | lambda>0, nu>0; both free or any held subset; boundary lambda=0 or nu=0 excluded | nonsingular; covariance of k and log(k!) | inference_1_report |
| dgamma | argued | shape,rate>0; fixed unit bins | open: joint rank of bin-probability scores | count |
| dnorm | argued | mean real, sd>0; fixed unit bins on all integers | open: joint rank of bin-probability scores | count |
| dweibull | argued | 0<shape1<1, shape2>0 | open: global rank of two score sequences | count |
| emg | argued | mu real, sigma,lambda>0 | open: rank of joint location/scale/rate scores | individual |
| fatigue | argued | alpha,beta>0; mu real; all free | open: joint score independence; mu fixed submodel included | flat_endpoint |
| foldnorm | proved_conditional | sigma>0; mu>0: both free; mu=0: one-sided boundary with zero mu score | nonsingular for mu>0; mu score zero at mu=0; scale information positive | inference_1_report |
| fpareto | conditional | shape1,shape2,shape3,scale>0; min known or free | open: rank; free min DQM if shape2*shape3>2, not DQM if <=2 | endpoint |
| frechet | argued | lambda,sigma>0; mu real, any subset | open: prove joint rank | flat_endpoint |
| gb2 | argued | shape1,shape2,shape3,scale>0 | open: joint rank and exceptional parameter surfaces | positive |
| genbeta | conditional | positive shapes and scale; scale held, or scale free with shape2>2 | open: rank; free scale non-DQM if shape2<=2 | endpoint |
| gompertz | argued | a,b>0 | open: rank; compare a and b scores | positive |
| gumbel | proved | mu real, sigma>0; either or both free | nonsingular: independent location and scale scores | locscale |
| huber | argued | mu real, sigma,epsilon>0; any subset | open: joint rank and pointwise Hessian treatment at knots | individual |
| invburr | argued | shape1,shape2,scale>0 | open: joint rank, inherited from Burr by reciprocal | positive |
| invexp | proved_interior | scale>0; reciprocal transform of exponential | nonsingular for scale | inference_1_report |
| invgamma | proved | alpha,beta>0; either or both free | nonsingular: reciprocal gamma | transform |
| invgauss | proved | mean,shape>0; either or both free | nonsingular: sufficient statistics x and 1/x affinely independent | positive |
| invparalogis | argued | shape,scale>0 | open: prove joint rank | positive |
| invpareto | argued | shape,scale>0 | open: prove joint rank | positive |
| invtrgamma | argued | shape1,shape2,scale>0 | open: joint rank; inherited from transformed gamma by reciprocal | positive |
| invweibull | proved | shape,scale>0 | nonsingular: log reciprocal is Gumbel with invertible parameter map | transform |
| kumar | argued | a,b>0; fixed endpoints 0,1 | open: prove joint score independence | positive |
| lgser | proved | 0<theta<1 | positive: variance of X / theta^2 | count_exp |
| llogis | proved | shape,scale>0 | nonsingular: log X logistic; invertible transformation | transform |
| logis | proved | location real, scale>0 | nonsingular: odd location score and nonzero even scale score | locscale |
| lomax | argued | lambda,kappa>0; support starts at fixed zero | open: prove score independence; no threshold coordinate in this parametrization | positive |
| lst | argued | mu real, sigma,df>0; df known or fitted | location/scale rank proved by odd/even scores; joint df rank open | student |
| naka | proved | shape,scale>0 | nonsingular: X squared gamma and parameter map invertible | transform |
| nbinom | argued | size>0, 0<prob<1; size is continuous | open: proof of joint rank; size=0 or prob=1 degenerate boundaries | count |
| nsbeta | conditional | positive shapes; min<max; endpoints known or free | open: rank; free min requires shape1>2, free max requires shape2>2; <=2 non-DQM | endpoint |
| paralogis | argued | shape,scale>0 | open: prove joint rank | positive |
| pareto3 | conditional | shape,scale>0; min known or free | open: rank; free min DQM if shape>2, non-DQM if shape<2 except shape=1; shape=2 non-DQM; shape=1 positive boundary non-DQM | endpoint |
| pareto4 | conditional | shape1,shape2,scale>0; min known or free | open: rank; free min DQM if shape2>2, non-DQM if shape2<=2 | endpoint |
| pig | open | mean>0, shape>0; lambda mixture representation; shape=0 boundary | open: prove score tail domination and joint rank | individual |
| sged | conditional | sd,xi,nu>0; cusp moves with free coordinates | open: rank; cusp-moving directions require nu>1/2; other held subsets need tangent check | cusp |
| sgomp | argued | b>0, eta>0 | open: joint rank; eta=0 one-sided exponential boundary | positive |
| skellam | argued | mu1,mu2>0 | open: joint rank; zero-rate boundaries change support | count |
| slash | argued | mu real, sigma>0 | nonsingular: odd location and nonzero even scale scores | locscale |
| snorm | argued | mean real, sd,xi>0; two-piece normal | open: joint rank including xi=1; do not conflate with Azzalini skew-normal | cusp |
| sstd | argued | mean real, sd,xi>0, nu>2 | open: full rank and symmetric xi=1 case | cusp |
| std | argued | mean real, sd>0, nu>2 | location/scale rank by odd/even scores; joint nu rank open | student |
| tbinom | proved | size N known, a,b fixed, 0<prob<1; retained integer set has >=2 points | positive for prob; singleton rank zero; unknown size discrete; a,b floor changes | trunc_count |
| tlambda | open | sigma>0, mu and lambda real; endpoints depend on lambda for lambda>0 | open: support-dependent DQM and rank, including lambda=0 and held subsets | individual |
| tnorm | proved | a<b fixed known, mean real, sd>0; positive truncation probability | nonsingular: x and x² independent on an interval; free finite bounds non-DQM | trunc_cont |
| tpois | proved | lambda>0; fixed a,b with >=2 retained nonnegative integers | positive for lambda; singleton rank zero; a,b floor changes | trunc_count |
| trgamma | argued | shape1,shape2,scale>0 | open: joint rank and parameter coincidences | positive |
| weibull | proved | shape,scale>0 | nonsingular: log X Gumbel with invertible parameter map | transform |
| yulesimon | argued | rho>0 | positive: score strictly varies with count; log-count moments finite | count |
| zeta | proved | s>1 | positive: variance of log X | count_exp |
| zib | conditional | N known integer >=2; 0<prob,pi<1 | open: rank for N>=2; N=1 rank one, N=0 rank zero | mixture |
| zinb | argued | size>0, 0<prob,pi<1 | open: joint rank and exceptional surfaces; boundary pi=0 or 1 is one-sided | mixture |
| zip | argued | lambda>0, 0<pi<1 | nonsingular: positive-count ratios identify lambda, zero identifies pi | mixture |
| zmbinom | conditional | N known >=2; 0<prob,p0<1 | nonsingular: conditional positive count varies with prob; N=1 only p0 identifiable | modified |
| zmgeom | proved | 0<prob,p0<1 | nonsingular: zero indicator and positive geometric score | modified |
| zmlogarithmic | proved | 0<prob,p0<1 | nonsingular: zero indicator and positive logarithmic score | modified |
| zmnbinom | argued | size>0, 0<prob,p0<1 | open: positive truncated negative-binomial joint rank | modified |
| zmpois | proved | lambda>0, 0<p0<1 | nonsingular: zero indicator and positive truncated Poisson score | modified |
| ztbinom | proved | N known >=2, 0<prob<1 | positive: conditional X varies; N=1 point mass | trunc_count |
| ztgeom | proved | 0<prob<1 | positive: shifted geometric | trunc_count |
| ztnbinom | argued | size>0, 0<prob<1 | open: joint size/prob rank | trunc_count |
| ztpois | proved | lambda>0 | positive: variance of positive count / lambda^2 | trunc_count |

## Historical inventory by family

These are named-method detections in the fixed historical snapshot, not proof of full derivative coverage. `no_named_method_found` does not rule out finite differences or methods named differently. The validation column reports historical Hessian grids only, and does not establish inference validity. Exact objective coordinates and normalizations remain open except where documented above.

| Family | Historical candidates (struct, dimension, methods) | Hessian grid |
|---|---|---|
| bbinom | no_named_method_found | no_historical_hessian_report_found |
| betapr | BetaPrimeCost(dim=2;gradient+hessian) | historical_symbolic_grid_passed; n=68 |
| bhatt | no_named_method_found | no_historical_hessian_report_found |
| bnbinom | no_named_method_found | no_historical_hessian_report_found |
| burr | BurrCost(dim=2;gradient+hessian) | historical_symbolic_grid_passed; n=68 |
| cauchy | CauchyCost(dim=2;gradient+hessian) | historical_symbolic_grid_passed; n=68 |
| cmb | no_named_method_found | no_historical_hessian_report_found |
| cmp | no_named_method_found | no_historical_hessian_report_found |
| dgamma | no_named_method_found | no_historical_hessian_report_found |
| dnorm | no_named_method_found | no_historical_hessian_report_found |
| dweibull | no_named_method_found | no_historical_hessian_report_found |
| emg | no_named_method_found | no_historical_hessian_report_found |
| fatigue | FatigueCost(dim=2;gradient+hessian) | historical_symbolic_grid_passed; n=68 |
| foldnorm | no_named_method_found | no_historical_hessian_report_found |
| fpareto | no_named_method_found | no_historical_hessian_report_found |
| frechet | no_named_method_found | no_historical_hessian_report_found |
| gb2 | no_named_method_found | no_historical_hessian_report_found |
| genbeta | no_named_method_found | no_historical_hessian_report_found |
| gompertz | GompertzCost(dim=2;gradient+hessian) | historical_symbolic_grid_passed; n=68 |
| gumbel | GumbelCost(dim=2;gradient+hessian) | historical_symbolic_grid_passed; n=68 |
| huber | no_named_method_found | no_historical_hessian_report_found |
| invburr | InvBurrCost(dim=2;gradient+hessian) | historical_symbolic_grid_passed; n=68 |
| invexp | no_named_method_found | no_historical_hessian_report_found |
| invgamma | InvGammaCost(dim=2;gradient+hessian) | historical_symbolic_grid_passed; n=68 |
| invgauss | InvGaussCost(dim=2;gradient+hessian) | historical_symbolic_grid_passed; n=68 |
| invparalogis | no_named_method_found | no_historical_hessian_report_found |
| invpareto | no_named_method_found | no_historical_hessian_report_found |
| invtrgamma | no_named_method_found | no_historical_hessian_report_found |
| invweibull | InvWeibullCost(dim=2;gradient+hessian) | historical_symbolic_grid_passed; n=68 |
| kumar | KumarCost(dim=2;gradient+hessian) | historical_symbolic_grid_passed; n=68 |
| lgser | LgserCost(dim=1;gradient+hessian) | historical_symbolic_grid_passed; n=17 |
| llogis | LogLogisticCost(dim=2;gradient+hessian) | historical_symbolic_grid_passed; n=68 |
| logis | LogisticCost(dim=2;gradient+hessian) | historical_symbolic_grid_passed; n=68 |
| lomax | LomaxCost(dim=2;gradient+hessian) | historical_symbolic_grid_passed; n=68 |
| lst | LstCost(dim=unknown;gradient) | no_historical_hessian_report_found |
| naka | NakaCost(dim=2;gradient+hessian) | historical_symbolic_grid_passed; n=68 |
| nbinom | NbinomCost(dim=2;gradient+hessian) | historical_symbolic_grid_passed; n=68 |
| nsbeta | no_named_method_found | no_historical_hessian_report_found |
| paralogis | ParalogisCost(dim=2;hessian) | historical_symbolic_grid_passed; n=68 |
| pareto3 | no_named_method_found | no_historical_hessian_report_found |
| pareto4 | no_named_method_found | no_historical_hessian_report_found |
| pig | no_named_method_found | no_historical_hessian_report_found |
| sged | SgedCost(dim=4;hessian) | historical_symbolic_grid_passed; n=272 |
| sgomp | no_named_method_found | no_historical_hessian_report_found |
| skellam | no_named_method_found | no_historical_hessian_report_found |
| slash | no_named_method_found | no_historical_hessian_report_found |
| snorm | SnormCost(dim=3;hessian) | historical_symbolic_grid_passed; n=153 |
| sstd | SstdCost(dim=4;hessian) | historical_symbolic_grid_passed; n=272 |
| std | StudentTCost(dim=3;hessian); StudentTTwoCost(dim=2;gradient) | historical_symbolic_grid_passed; n=153 |
| tbinom | no_named_method_found | no_historical_hessian_report_found |
| tlambda | no_named_method_found | no_historical_hessian_report_found |
| tnorm | no_named_method_found | no_historical_hessian_report_found |
| tpois | no_named_method_found | no_historical_hessian_report_found |
| trgamma | no_named_method_found | no_historical_hessian_report_found |
| weibull | WeibullCost(dim=2;gradient+hessian) | historical_symbolic_grid_passed; n=68 |
| yulesimon | no_named_method_found | no_historical_hessian_report_found |
| zeta | no_named_method_found | no_historical_hessian_report_found |
| zib | no_named_method_found | no_historical_hessian_report_found |
| zinb | no_named_method_found | no_historical_hessian_report_found |
| zip | ZipCost(dim=2;gradient+hessian) | historical_symbolic_grid_passed; n=68 |
| zmbinom | no_named_method_found | no_historical_hessian_report_found |
| zmgeom | no_named_method_found | no_historical_hessian_report_found |
| zmlogarithmic | no_named_method_found | no_historical_hessian_report_found |
| zmnbinom | no_named_method_found | no_historical_hessian_report_found |
| zmpois | no_named_method_found | no_historical_hessian_report_found |
| ztbinom | no_named_method_found | no_historical_hessian_report_found |
| ztgeom | no_named_method_found | no_historical_hessian_report_found |
| ztnbinom | no_named_method_found | no_historical_hessian_report_found |
| ztpois | no_named_method_found | no_historical_hessian_report_found |

## Reproducibility

The inventory was generated with `/tmp/umvl-cpu/run env TMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1 python3 dev/research/evaluations/2026-10-30-inference-inventory.py`. It reads committed source snapshot `c61a4979c6169388dea97c6b821b3bd89e530dfd` and the parent of `bf5a3ac`, while the worktree revision is `b85422e7a006134909b24a5bce45f5caa0e3f871`. A second generation compared byte-for-byte equal; a separate assertion checked 98 unique primary families, 20 exception rows, 69 report entries and zero `not_reviewed` entries. These checks passed. This is a documentation audit, not a coverage simulation or numerical derivative validation.
