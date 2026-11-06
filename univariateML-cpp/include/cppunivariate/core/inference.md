# Pilot inference: totals, nuisance parameters and test inversion

The implementation is `inference.hpp` plus the normal, gamma, beta and Poisson
`score.hpp` headers. The reproducible calibration is
`dev/validation/inference-4.py` from the development repository root; its retained
results and interpretation are in
`dev/research/evaluations/2026-10-31-inference-4-report.md`.

For independent observations the derivative contract gives total score S,
observed information H, Fisher F=n I and J=sum(s_i s_i^T). Covariances are
F^-1, H^-1 and H^-1 J H^-T with **no further division by n**. The full free
block includes every fitted nuisance coefficient. A held subset means known
constants, not nuisance elimination. Delta variance is g^T V g, with g in the
same packed free order. Gamma mean has g=(scale,shape); beta mean has
 g=(b,-a)/(a+b)^2. Wald uses (estimate-null)^2 / delta variance, compared with
a chi-square(1) cutoff. Its interval uses estimate +/- z * standard error;
no clipping or transformation is silently applied.

The SPD solve scales by maximum entry, checks symmetry and finite entries,
uses Cholesky, checks the existing eigenvalue condition diagnostic, and solves
against basis vectors. Negative pivots give Indefinite, zero pivots Singular;
ill conditioning at the existing sqrt(epsilon) eigenvalue-ratio threshold is
rejected. Natural-coordinate units affect this decision. There is no automatic
rescaling, regularization or pseudoinverse. Singular or ill-conditioned J is
allowed as sandwich meat because it is not inverted; indefinite/nonfinite J
fails. A zero delta variance is unavailable, not an exact confidence claim.
Dimension-zero covariance is empty; a zero-dimensional contrast fails.

At each null, fit the nuisance coefficients and evaluate S^T F^-1 S using the
**full** null information. At a constrained maximum nuisance score vanishes;
this is the efficient-score Rao statistic for one constraint. Profile LR is
2*(unrestricted logLik - constrained logLik). Negative beyond rounding
allowance, nonfinite, or failed fits fail. The low-level functions require
the caller to establish successful maximization. Family target wrappers do
that and preserve procedure-specific statuses; overall failure does not erase
a separately successful procedure. NumericalFailure also covers nonexistence
or a boundary outside the interior derivative contract in these wrappers.

Normal mean profiles sd by the existing held-mean fitter. Gamma shape profiles
scale analytically via the existing held-shape fitter. Gamma mean m fixes
scale=m/a and solves

    log(a)-psi(a)+1+mean(log x)-log(m)-mean(x)/m = 0.

Its derivative 1/a-trigamma(a)<0 proves strict concavity in a. Jensen and
log(r)+1-r<=0 give the limiting negative score except constant data (whose
unrestricted fitter fails). Beta mean m fixes (a,b)=k*(m,1-m) and solves

    m*(mean(log x)-psi(m*k))
      +(1-m)*(mean(log(1-x))-psi((1-m)*k))+psi(k) = 0.

The beta exponential-family likelihood is strictly concave in (a,b), hence
along this ray. For both new scalar solves, expand positive brackets until
opposite derivative signs certify a unique maximum, then bisect in log
coordinates to bracket width <=1e-12. No last iterate or bounded-search cap
is returned as an estimate. Exhaustion/nonfinite signs fail. Existing fitters
supply unrestricted fits and likelihood evaluation with both shapes held.
Stable beta increments and gamma log-minus-digamma come from existing helpers;
there is no new special-function backend or altered fitter.

Profile LR intervals invert LR<=critical. Normal mean has explicit width
sd_MLE*sqrt(expm1(critical/n)). Positive targets use log coordinates; beta mean
uses logit. Adaptive expansion must bracket both threshold crossings, then
bisection reduces coordinate bracket width to 1e-10*(1+abs(center)). Failure
to bracket/evaluate/converge returns a status, never a capped endpoint.
Unimodality arguments: normal/gamma mean profile derivative has sign(xbar-m);
gamma shape is a concave profile; beta likelihood superlevel sets are convex
in positive (a,b), whose ratio a/(a+b) image is an interval. Thus these are
connected LR sets, not an assumption inferred from a grid. These statements
are real-arithmetic proofs of structure, not certified floating-point bounds.

Normal fitted-variance Rao inversion has width
sd_MLE*sqrt(critical/(n-critical)) when n>critical. Poisson score inversion has
roots (2*total+critical +/- sqrt(critical*(4*total+critical)))/(2*n), using a
rationalized lower root. The exact normal interval is Student t with n-1 df,
width t_(1-alpha/2)*sd_MLE/sqrt(n-1). Poisson Garwood uses gamma quantiles:
lower=qgamma(alpha/2,total)/n (zero if total=0),
upper=qgamma(1-alpha/2,total+1)/n. Exact procedures require their model,
not just first two moments. A zero Poisson total is retained: exact, score
and LR intervals exist; interior Wald/sandwich do not.

## Why score inversion is not always an ordinary interval

Gamma mean with fitted shape illustrates a rejected approach: selecting only
the first two score-threshold crossings loses a remote accepted tail. At large
null m, the constrained a tends to zero like 1/log(m), while the Rao statistic
is n*a*(xbar-m)^2/m^2, which tends to zero. Thus an unbounded accepted tail
exists even when an intermediate null is rejected. We expose the pointwise
Rao statistic and calibrate its full inverted confidence **set**, not an
invented two-endpoint interval. A globally certified topology/endpoints for
gamma shape and beta mean Rao inversion remain open. Wald and LR intervals
are available for these targets. The calibration measures membership at truth,
so it neither requires nor assumes endpoint topology. No bounded interval
claim should be inferred from its score coverage column.

## Statistical scope

For the interior four-family models, finite sufficient-statistic covariance
and nonsingular natural-parameter Jacobians give DQM, finite full-rank model
information, including held submodels. These are distinct from sample Hessian
SPD, floating-point condition, MLE existence and estimator limit theorems.
Classical Wald/Rao/LR limits additionally require consistent interior MLEs and
the local likelihood expansion. Under misspecification the sandwich needs
independent identically distributed sampling, an interior identifiable KL
maximizer, nonsingular expected negative Hessian, finite score second moments,
and a valid estimating-equation expansion. H/n estimates sensitivity; F/n is
model information and generally not that sensitivity. No claim of robustness
follows merely from computing J. Calibration is finite numerical evidence,
not a uniform guarantee over n, parameters or alternatives.

## Exact pivots and median procedures (TASK-102.5, 2026-10-31)

The additional opt-in headers are `distributions/{laplace,pareto,exp,unif,norm,
triang}/inference.hpp`. They return `InferenceInterval` or explicitly named
median/test results; they do not change fitting, default estimates, derivative
matrices or R bindings. Every interval defaults to alpha=.05. Probabilities
must lie strictly between zero and one. Nonfinite observations, empty samples,
invalid known constants and unresolvable arithmetic return statuses. Repeated
observations are accepted by the formulas but do not establish the continuous
sampling assumptions. Zero spread/pivot denominators fail; exact sign intervals
alone deliberately permit infinite endpoints. `known_b` and `known_mean` use
NaN to request estimation, matching the held-parameter convention.

All exact statements below assume independent identically distributed data
from the stated model, except the sign interval, which only needs independent
continuous observations with the same unique median. They are finite-sample
statements in real arithmetic; Boost quantile evaluation is numerical, through
the existing special-function seam. Overflow/underflow that invalidates an
endpoint is a numerical failure, not a capped confidence limit. These
procedures do not provide joint Wald covariance for a moving endpoint.

### Laplace location, scale fitted

For density exp(-|x-mu|/sigma)/(2 sigma), sigma>0, the almost-everywhere
location score is sign(x-mu)/sigma. Its per-observation model Fisher information
is 1/sigma²; the scale score is (|x-mu|/sigma-1)/sigma, with the same diagonal
information and zero cross information. The model is DQM with full rank despite
the kink. The ordinary sample location Hessian is zero away from observations
and undefined at them, so it is unsuitable as sensitivity or information.

`laplace_location_wald_interval` uses the existing upper sample median and
fitted mean absolute deviation, with standard error sigma_hat/sqrt(n).
The sample median quantile expansion gives sqrt(n)*(median-mu) => N(0,sigma²);
consistency of the fitted scale and orthogonality justify the plug-in model
interval even though scale is fitted. The median choice for even n changes
neither this limit nor the exact sign procedure. This is model-only root-n
asymptotic inference, not an exact finite-sample confidence claim.

`median_sign_interval` inverts the two-sided sign test. If B counts observations
below the true median, B~Binomial(n,1/2). Choose the largest integer k>=1 with
Pr(B<=k-1)<=alpha/2 and return [X_(k),X_(n-k+1)]. The disjoint omitted events
have total probability 2 Pr(B<=k-1), proving coverage at least 1-alpha, usually
strictly greater because of discreteness. k=0 denotes the whole real line.
A monotone binary search chooses k using the beta-function binomial CDF;
there is no numerical maximization. Under positive density at the median,
finite endpoints converge at the root-n rate. Ties/atoms invalidate the stated
binomial equality; no automatic randomization or atom correction is applied.

`median_sandwich` exposes median, density, bandwidth and variance. It uses a
Gaussian KDE evaluated at the upper sample median, with

    h = 1.06 * (median(|X_i-median_hat|)/log(2)) * n^(-1/5),
    g_hat = sum(phi((X_i-median_hat)/h))/(n*h),
    variance_hat = 1/(4*n*g_hat^2).

The MAD/log(2) factor is the Laplace reference scale; 1.06 is a declared tuning
choice, not an optimal-bandwidth assertion. Zero/nonfinite MAD or density fails;
there is no hidden floor or fallback scale. Under iid continuous sampling with
a unique median m, g(m)>0 and g locally continuous, the median has root-n
normal limit with variance 1/(4g(m)^2). With a unique positive population MAD,
the bandwidth scale is consistent and h tends to zero while nh tends to infinity.
Under the corresponding local KDE consistency conditions, the interval is
asymptotically valid without a Laplace model. With two bounded continuous local
derivatives of g, KDE error at the median is O_p(n^(-2/5)); with a Lipschitz
cusp such as Laplace, bias is O(h) and total error is O_p(n^(-1/5)). Evaluating
at the estimated median does not change these orders under local empirical
process regularity. These are density-estimation orders, not coverage-error
rates. Only consistency is needed for the root-n studentized median limit.
Heavy tails need no second moment for the median or this MAD-based bandwidth;
the misspecified Laplace pseudo-scale additionally needs finite E|X-m|.

The median estimating function psi=1/2-1{X<=m} has per-observation sensitivity
A=g(m) and meat B=1/4. Thus A^-1 B A^-1/n gives the variance above. Equivalently
the Laplace location score has A=2g(m)/sigma and B=1/sigma². Fitting sigma does
not change the median expansion; using the model Fisher value for A under
misspecification is unjustified. No ordinary Hessian sandwich is substituted.

### Pareto shape and exponential rate

`pareto_shape_exact_interval(x,alpha,known_b)` uses density a*b^a*x^(-a-1),
a,b>0. If b is known, Y_i=log(X_i/b) are exponential(rate=a), so
2*a*sum(Y_i)~chi-square(2n). If b is unknown, write M=min(Y_i). The exponential
memoryless property leaves n-1 independent rate-a residuals above M, independent
of M. Therefore T=sum(log(X_i/min(X))) has Gamma(n-1,rate=a), and 2*a*T has
chi-square(2(n-1)). Divide the lower and upper chi-square quantiles by 2T.
The unknown-endpoint version requires n>=2 and T>0. The known-endpoint version
allows n=1 and requires a positive log sum. The shape is root-n asymptotically
normal while the fitted endpoint has rate n; the exact pivot avoids treating
the joint non-DQM model as regular. Close log ratios use log1p, with log
differences if the ratio would overflow. Both pivots are model-only.

`exp_rate_exact_interval` holds the origin at zero, fits the rate lambda, and
uses 2*lambda*sum(X_i)~chi-square(2n). Divide quantiles by twice the sum. It
allows n=1 and zero individual observations, but fails for zero total, negative
observations or overflowing total. No unknown origin is implicitly fitted.
The ordinary fixed-origin exponential model is DQM/full rank and its rate
estimator has root-n asymptotics; the interval is exact under that model.

### Continuous uniform endpoints and width

`unif_exact_interval` assumes both endpoints unknown and n>=2. Write L=min X,
U=max X, R=U-L, W=b-a. The joint extreme density on a<l<u<b is
n(n-1)(u-l)^(n-2)/W^n. Integrating gives R/W~Beta(n-1,2). Consequently the exact
equal-tail width interval is [R/q_(1-alpha/2), R/q_(alpha/2)]. For endpoints,
D=(L-a)/R has Pr(D>t)=(1+t)^(-(n-1)), t>=0. One proof changes variables to
(l,r) in the extreme density and integrates r up to W/(1+t). Reflection gives
the same law for (b-U)/R. With q_p=(1-p)^(-1/(n-1))-1, the intervals are

    a: [L-R*q_(1-alpha/2), L-R*q_(alpha/2)],
    b: [U+R*q_(alpha/2), U+R*q_(1-alpha/2)].

These have marginal coverage exactly 1-alpha with the other endpoint fitted;
the three intervals together are not a simultaneous 1-alpha confidence box.
No endpoint is clipped to sample extremes or a numerical domain.

`unif_endpoint_known_opposite_interval` covers the unknown endpoint when the
other is a known constant. For a known, (max X-a)/(b-a)~Beta(n,1); invert its
quantiles p^(1/n). Reflection supplies a when b is known. Subtracting the known
a, or subtracting the a interval from known b, gives the identical width
interval, so there is no separate width implementation for that setting.
All uniform interval errors/lengths have order 1/n. Free endpoints are non-DQM;
there is no finite joint Fisher matrix or root-n endpoint Wald assertion.

### Normal variance

`norm_variance_exact_interval` targets variance (sd squared, not sd). With
unknown fitted mean, Q=sum((X_i-Xbar)^2) and Q/variance~chi-square(n-1). With
known supplied mean mu, Q=sum((X_i-mu)^2) and the df is n. Return
[Q/q_(1-alpha/2), Q/q_(alpha/2)]. The mean-fitted version requires n>=2;
the known-mean version permits n=1. A centered online sum avoids cancellation
in sum(X_i²)-n*Xbar². Zero spread fails. Normal variance is a regular root-n
parameter for positive variance; the stated pivot is additionally exact under
normal sampling and does not assert robustness to nonnormal fourth moments.

### Triangular mode with known endpoints

For known a<b and interior a<c<b, the mode score is -1/(c-a) on x<c and
1/(b-c) on x>c. Integration gives I_cc=1/((c-a)(b-c)), finite and positive.
The density is continuous at c, giving DQM in this one free coordinate. The
ordinary pointwise log-density second derivative is positive on each piece;
its negative expectation is -I_cc. It omits the moving-kink contribution and
cannot supply inference bread. Free endpoint directions have infinite score
second moments and are outside this procedure.

`triang_mode_wald_interval` calls the unchanged certified finite-candidate
fitter with both endpoints held, checks successful interior mode, and uses
sqrt((c_hat-a)*(b-c_hat)/n). The model MLE is consistent: compact mode space,
identifiable density and a uniform likelihood law of large numbers separate
its unique population maximum. In a compact interior neighborhood the scores
are bounded and piecewise smooth; observations that change piece over a mode
increment h have probability O(|h|). This bounds the centered likelihood
remainder by O_p(sqrt(n*|h|^3)), which vanishes for h=O(n^-1/2), while the mean
quadratic term is -n*I_cc*h²/2. On a compact interior neighborhood the score
class has bounded variation and a bounded envelope, giving uniform empirical
score error O_p(n^(-1/2)). At a maximizing data knot the one-sided total scores
bracket zero with an O(1) jump; the nonzero population score slope therefore
localizes the consistent maximum at rate n^(-1/2). The local expansion then
gives the efficient root-n mode limit N(0,1/I_cc). This is a real-arithmetic model argument,
not a finite-n Wald coverage theorem; finite-sample convergence can be slow.
No clipping to [a,b] is applied.

`triang_mode_tests` returns the pointwise model score statistic S²/(n I_cc)
and LR from successful global fixed-endpoint fits with c free and held under
the null. A datum exactly at null c is rejected because its score is undefined;
this is probability zero under the stated continuous model. At an interior
null their asymptotic reference is chi-square(1). A simulated LR reference is
also possible because a,b,c completely specify the null. The calibration uses
an independent finite bank and its k=ceil(.95*(M+1))th ordered statistic as
critical value. For a continuous statistic, unconditional acceptance is
k/(M+1); conditional reference-bank uncertainty must also be reported.
The calibration is pointwise at declared modes, not an interpolated critical
surface or a certified connected LR interval. Generic simulated-LR inversion
and robust triangular sensitivity remain open. Under another density g, the
sensitivity would be g(c)*(1/(c-a)+1/(b-c))
-G(c)/(c-a)²-(1-G(c))/(b-c)², which need not equal model I_cc or even be positive.

A connected triangular LR interval is false in general. With known endpoints
0 and 1 and five observations each at .1 and .9, the two global mode maxima
are .1 and .9. The LR at .5 is 10*log(25/9)=10.2165124753, exceeding the
95% chi-square(1) cutoff, whereas LR at both maxima is zero. Thus the constant
cutoff confidence set has separated components. Small distinct perturbations
of the observations preserve the strict rejection at .5, so ties are not the
reason. This is an algebraic counterexample, independently replayed through
the production bridge in the retained `inference-5/topology.json` check.
Enumerating all components and controlling null-specific simulation error
remain necessary for a general simulated-LR confidence-set interface.

### Validation and retained choices

`dev/validation/inference-5.{sh,py}` and `inference-5-probe.cpp` reuse the
inference-4 streaming bridge, source-hashed cells and seeded resume design.
They compare actual production intervals to independent SciPy pivots/KDE,
compare triangular likelihoods to exhaustive sample-knot evaluation, and
estimate coverage with failures counted as noncoverage. Results, Monte Carlo
errors, workload and the TASK-102.6 availability handoff are retained in
`dev/research/evaluations/2026-10-31-inference-5-report.md`.

Decisions: retain the existing upper-median fitter, use a declared MAD-based
bandwidth without a numerical floor, use conservative nonrandomized sign
inversion, and keep uniform intervals marginal. No empirical Hessian rescue
for either kink model, no bounded search that returns a last iterate, and no
bootstrap validity inferred from availability. These choices preserve fitted
estimates and keep the inferential assumptions explicit.

The retained 5,000-replicate calibration at n=10,30,100,1000 supports using
exact pivots for their stated models and the conservative sign interval for
small-sample median inference. Laplace model Wald undercovers even under its
model on this grid (coverage 0.8334 at n=10 and 0.9372 at n=1000); under a
normal alternative its n=1000 coverage is 0.7896. The fixed KDE rule is close
to nominal under the normal alternative but conservative under Laplace, where
its cusp bias decays slowly. Neither asymptotic median interval should be
advertised as a finite-sample 95% guarantee. Triangular mode Wald also
undercovers (n=1000 coverage 0.9298, 0.9400, 0.9330 at relative modes .2,.5,.8).
Prefer null-specific simulation for triangular LR tests when the endpoints
are known; the retained finite reference bank has its own Monte Carlo error.
These numerical findings motivate explicit procedure selection and preserving
the exact alternatives; they are not proofs of coverage outside the tested
grid or a validated universal bandwidth choice.

## Finite-difference inference and family dispatch

`inference_numeric.hpp` provides the finite-difference engine, and
`inference_families.hpp` supplies family dispatch and explicit eligibility
checks. The caller must verify a successful interior fit, use the correct held
coordinates and declare whether endpoints or design coefficients are known.
The dispatch checks its documented regimes but cannot establish an estimator's limit law,
or the validity of a sandwich under misspecification. It does not calculate
Fisher information; Fisher covariance requests for nonempty free subsets fail.
`numeric_inference_checked` accepts an explicit family-supplied eligibility
assessment and permitted free-coordinate mask. Its default assessment rejects
unavailable families, and it propagates boundary and regime statuses and the
reason. The family adapters construct these assessments for their documented
scopes; other scopes return an unavailable status.

For each free coordinate, the numerical engine maps to an unconstrained real
coordinate: identity, log, logit, or log of the distance above a fixed bound.
For example, free Student-t `df` uses a shift of one when its location is also
free and that fitting restriction applies. A free GED shape uses a shift of
two in a moving-cusp smooth scope and a shift of one for the held-location fit
restriction; a held shape is not transformed. With internal coordinate `z`, the per-observation score uses central differences
at `h=1e-4*(1+abs(z))` and `h/2`, extrapolated as `(4 D(h/2)-D(h))/3` and divided
by `d theta/d z`. The observed information differentiates these natural scores
using internal steps `3e-4*(1+abs(z))` and half that value, Richardson
extrapolates those differences, divides by the corresponding Jacobian,
and symmetrizes mixed entries. Thus the score-dependent coordinate correction
is retained even away from a stationary point. It uses the total observed
information and the per-observation score outer-product sum with the existing
conditioning-aware covariance solve; no extra factor of sample size is added.

Under sufficient smoothness, exact arithmetic gives fourth-order truncation
for each Richardson difference, including the outer score derivative; floating-point cancellation, density evaluation error,
the nested difference and an ill-conditioned coordinate Jacobian can dominate.
These orders are not certified error bounds. A nonfinite result or a transformed
stencil whose natural values coincide fails. The pilot validator tests normal,
gamma, beta and Poisson at moderate seeded points and golden observations, not
extreme parameters or coverage and calibration. Separate standard, registry,
and conditional-regime validators extend the numerical comparisons to the
documented eligible families and golden fits. These finite checks are not
uniform error bounds or proofs of coverage and calibration.

`inference_families.hpp` selects the four analytic headers for normal, gamma,
beta and Poisson. Other implemented scopes are explicitly enumerated in
`inference_numeric_generated.hpp` and `inference_numeric_special.hpp`; the
former is regenerated and checked by `dev/validation/inference-family-registry.py`.
The special header checks held parameters, discrete designs and regime-specific
conditions. An unimplemented family returns `FamilyUnavailable`, and an
unavailable free subset returns `RegimeUnavailable` or `InvalidSubset` with a
reason. These interfaces do not establish successful estimation, consistency,
root-n asymptotics, rank in every regime or misspecification robustness. In
particular, a fitted support endpoint or integer design is not a known constant:
the caller must include it in the free mask, and the interface rejects it.
Numerical validation and calibration coverage remain scoped to their recorded
families and regimes, not implied by the existence of a dispatch branch.

## Pointwise smoothness for GED submodels

For GED, with the location held, its scale and shape log-density terms are
smooth for positive scale and shape, including an observation exactly at the
held location: the power contribution is identically zero there. When the
location is free, `|x-mean|^nu` has continuous second derivatives jointly in
the free coefficients locally at the cusp for `nu > 2`; if `nu = 2` is held,
the normal member is also smooth. The skew GED has the same `nu > 2` conclusion
at its moving join because the two one-sided power terms and their first two
join derivatives vanish there. When skew is held at one, skew GED reduces
to GED, including the held-location and held-shape normal submodels described
above. These pointwise smoothness statements describe the implemented derivative
scopes, rather than a necessary condition for regular inference. The interior
classification establishes DQM and full rank above nu = 1/2 for a moving
location; ordinary Hessian validity requires nu > 1, with separate joint
fitted-Hessian and numerical cusp-handling gaps for 1 < nu <= 2. The fit
restriction and procedure calibration remain separate checks.

## Known-threshold and fixed-shape submodels

With the threshold known, half-Cauchy scale and half-t scale/shape are fixed
foldings of their symmetric parent distributions. Their pointwise log densities
are smooth in the remaining positive coefficients. The Student-t shape score
has at most logarithmic tail growth, which is square-integrable for every
positive degrees of freedom; its scale score stays bounded. This supplies the
DQM and finite-information argument for those submodels, independently of the
non-DQM free-threshold direction. It is not a calibration result. GEV with its
shape fixed at zero is a Gumbel submodel, and GPD with its known threshold and
shape fixed at zero is a shifted exponential submodel. With positive GPD shape,
the support starts at that fixed threshold and the log density is smooth in the
positive shape and scale. The later moving-feature classification establishes regularity for xi > -1/2,
including free shape at zero, but the expanded support-aware numerical
derivative scopes and estimator calibration remain unimplemented.

## Finite-support rank and known-design submodels

For beta-binomial size `N >= 2`, write `s = alpha + beta` and
`m = alpha/s`. The first two factorial moments, divided by `N` and
`N*(N-1)`, are `m` and `m*m + m*(1-m)/(s+1)`. Their Jacobian with respect
to `(m,s)` is nonsingular for positive finite shapes, and the map from
`(alpha,beta)` to `(m,s)` is nonsingular. On the finite support, a zero
information direction would make every moment derivative zero, contradicting
that Jacobian. This proves full rank, conditional on known integer `N >= 2`;
it does not prove existence or asymptotics of the numerical estimator. For
`N=1` the distribution depends only on `m`: the joint information has rank
one, while either shape alone is identifiable if the other is known. At
`N=0` both shape scores vanish.

For CMB at known `N=1`, the law is Bernoulli and does not depend on `nu`;
probability alone is identifiable when `nu` is known. At `N=0` neither
coefficient is identified. At zero-inflated-binomial `N=1`, only
`(1-pi)*prob` is identified; either coefficient alone is identifiable when
the other is known and interior. A zero-modified binomial at `N=1` identifies
`p0` but not `prob`. A zero-truncated binomial or finite Zipf with known
`N=1`, and any truncated count model with a singleton retained support, is a
point mass and has zero information in its continuous coefficients. These
finite-support statements are exact rank arguments, not claims about the fit's
sampling distribution. A nonempty zero-information free block returns a
singular status rather than an inverse, after checking that the observations
lie in the model's discrete support.

## Requirements for a future R covariance method

The current `univariateML_construct` stores the estimates, likelihood, sample
size, call, metadata, `fixed` coefficient names and any restriction flags, but
not the checked observations. A later R `vcov` method needs the actual data
used by the fit (or an explicit, verified data input), including an explicit
policy for old serialized and data-free frozen objects. It must preserve which
coefficients were genuinely held before fitting, not relabel a fitted endpoint
or design as known. It must carry the fitter's boundary/restriction and failure
status to the inference call, map unavailable, singular, indefinite,
ill-conditioned and numerical statuses to clear R conditions, and name rows
and columns in README/free-coefficient order. The model and sandwich choices
must retain their distinct assumptions; computing either matrix does not by
itself establish estimator asymptotics or misspecification robustness.

## Known boundary submodels

Holding the Bhattacharjee width at `a = 0` gives a normal distribution: the
known-width mean and scale submodel is smooth and regular, while estimating
`a` at that boundary is not covered by this ordinary interior calculation.
Holding the zero-inflation probability at zero gives the corresponding
Poisson, negative-binomial or known-size binomial model. Holding the zero-mass
probability at zero in a zero-modified model gives its positive-truncated
count model. These are exact density identities, and their remaining free
coefficients inherit the corresponding DQM and pointwise smoothness under the
stated interior and design conditions. Holding the zero-inflation or zero-mass
probability at one instead gives a point mass at zero, so any remaining count
parameter has zero information. These claims condition on the boundary value
being known; they do not justify treating a fitted boundary estimate as known.

Additional known-boundary identities are useful when only the modification
probability is fitted. A zero-modified Poisson at known zero rate, geometric at
known unit probability, logarithmic-series at known zero parameter, or binomial
at known zero or unit probability has a positive component concentrated at one
(respectively at the binomial size for unit probability). Mixing that point with
zero gives an ordinary Bernoulli submodel for interior `p0`. A zero-inflated
known-size binomial at known unit probability likewise gives a Bernoulli
submodel for interior `pi`. With zero negative-binomial size held known, the
positive-truncated negative-binomial distribution is logarithmic-series with
parameter `1-prob`; the zero-modified version adds an independent zero-mass
parameter. With Conway--Maxwell--Poisson `nu = 0` held, the proper submodel
`0 < lambda < 1` is geometric. Shifted Gompertz with `eta = 0` held is
exponential. These identities establish the stated fixed-boundary submodels,
not the asymptotics of a fit that estimated the boundary coefficient.

## Pointwise smoothness of fixed-support scopes

For the fixed-support scopes in the generated registry, the stated interior
parameter conditions make the likelihood terms compositions of smooth
arithmetic, logarithms of positive quantities, exponentials, gamma or beta
functions at positive arguments, positive normal CDFs, and (for Skellam)
positive modified Bessel functions at positive arguments. Finite normalizers
are smooth sums; the Zipf/zeta normalizing series and their required
log-weighted derivatives converge locally uniformly in the stated regime.
Count-model case splits depend on the observed
integer, not on a free coefficient. With a threshold held, the same argument
applies to the stated Pareto and positive-support families for observations in
the interior support. Discretized normal and gamma masses are positive integrals
on fixed bins; derivatives pass under the bin integral on compact interior
parameter neighborhoods. For the gamma bin touching zero, the needed
logarithmic derivatives remain integrable because shape is bounded away from
zero locally. The slash density has the positive integral representation
`integral_0^1 u*phi(u*(x-mu)/sigma) du / sigma`, whose derivatives pass under
the finite integral; this also resolves the removable expression at `x = mu`.
These are pointwise real-arithmetic smoothness arguments, separate from the
cited DQM/tail arguments, rank and estimator consistency. They do not certify
the accuracy of finite-difference evaluations, especially near a boundary or
in extreme tails; those are checked numerically on the reported grids.

## Difference-step decision (2026-11-01)

An outer Richardson stencil with multiplier `1e-3` produced a maximum
per-observation scaled Hessian error `6.85e-5` on the seeded/golden skew-GED
check, exceeding the `2e-5` tolerance near its smooth but higher-derivative
cusp. Reducing the multiplier to `3e-4` gave `1.35e-6` on the same check.
The reference was the 40-digit mpmath driver
`dev/validation/inference-numeric-special.py`, seed 20261102, and its error
scale was `1 + abs(reference)`. This is finite-grid evidence for the chosen
step, not a universal error bound. The intermediate header was uncommitted and used `log(nu)` rather than the
later shifted coordinate. Reconstructing the earlier check therefore also
requires restoring that coordinate in an isolated copy; the earlier source
hash was not retained.

## Interior classification and covariance eligibility

The [interior classification](../../../../dev/roadmap/2026-10-30-inference-and-dqm.md)
separates DQM, finite information, rank and ordinary observed-Hessian validity.
The dossier `inference_numeric.available` now requires an implemented covariance
path for the stated subset. A mathematically regular expanded model stays
unavailable when its numerical derivative or fitted-Hessian validation is missing.
Both covariance types use the ordinary observed Hessian: sandwich covariance
cannot repair its omitted Laplace kink or the moving GED/SGED cusp at nu <= 1.

At sn alpha = 0 the null direction involves location and alpha; at bnbinom
size = beta it involves size and beta; at genbeta shape2 = 1 it involves
shape1 and shape3. Dispatch rejects exactly those subsets containing both
coordinates, whether the other coefficients are held or free. Holding either
coordinate removes that structural null direction. The ordinary Hessian can
look invertible in a finite sample on these sets, so numerical conditioning
alone is insufficient. The reviewed registry generator owns these guards.

The [application report](../../../../dev/research/evaluations/2026-11-06-infer-meta-report.md)
records the metadata changes, derivative checks and remaining procedure gaps.
The dated classification reports supply mathematical arguments; numerical
regression checks do not prove estimator asymptotics or coverage.
