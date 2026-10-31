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
