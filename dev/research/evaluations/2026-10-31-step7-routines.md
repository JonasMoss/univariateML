# Shared numerical contracts and retained choices (step 7)

Applies to source base `91d538253b6c448be78eedadde8abd9c44fb99c6`, plus the
arithmetic-preserving beta-header relocation in this worktree. Measurements,
commands and phase B decisions are in [the step-7 report](2026-10-31-step7-report.md).
The [generated index](2026-10-31-numerical-call-paths.md) and adjacent JSON
record source-level users; include reachability alone is not a runtime call.

## Lambert W (`core/lambert_w.hpp`)

The equation is `w exp(w)=z`. ZIP uses W0 at `z=-g exp(-g)`, where the
positive-count mean `g>=1`, so `z` lies in `[-1/e,0]`. For example `g=2`
gives a nonzero fitted rate `2+W0(-2 exp(-2))`. W−1 is currently unused;
logarithmic-series fitting deliberately solves `expm1(u)/u=mean(x)`
instead, avoiding cancellation of two nearly equal branch-point terms.

The retained implementation initializes W0 by a cubic Taylor polynomial
for `|z|<1e-3`, otherwise by `z` (negative) or `log1p(z)` (positive).
W−1 starts at `log(-z)`. The refinement uses a second-order correction to
the Newton step and stops on `|step| < tol*(1+|w|)` or the iteration budget.
The original header called this Fritsch iteration; that attribution is
unverified and the formula must not be confused with a convergence proof.
It has no branch-point expansion, domain guard, residual acceptance or
failure status. A zero budget returns the initializer. No fallback exists.
Branch-point conditioning diverges; a small equation residual there does
not bound forward error. The default 30 steps have no universal error bound.

Decision history: commit `f1f12455193a830c5f56f11f52265ddad919a066`
(2026-10-28) split the branches after the old `log(-z)` initializer selected
W−1 inside ZIP, producing the trivial zero-rate solution. It changed W0's
negative initializer to z and retained the old start as explicit W−1 for
lgser. Commit `4d1d5cb52c854050bd7c4fef5fa38477172cc386` (2026-10-29)
then replaced lgser's cancellation-prone formula during the edge-case audit.
This explains why branch initialization is protected behavior, but does not
prove convergence of the current correction formula.

Historical reason: a compact closed form for ZIP; no surviving evidence
establishes why this refinement was preferred to Boost. Preserve it pending
caller-level compatibility and edge-case review. This is a documented legacy
gap, not approval for new uncapped/unchecked searches. The reproducible probe
compares exact binary64 arguments with 100-digit mpmath, including endpoints,
subnormals and out-of-domain inputs. Boost's policy-controlled real branches,
branch-point conditioning and references (Corless et al.; Fukushima) are
summarized in [Boost's Lambert W documentation](https://www.boost.org/doc/libs/1_88_0/libs/math/doc/html/math_toolkit/lambert_w.html).
No new Lambert search is introduced in phase A.

## Scalar Newton (`core/newton.hpp`) and inverse digamma

Newton helpers accept the quotient `f/f'`, not the score itself. Gamma
uses absolute step convergence; fatigue uses relative change from the old
iterate. Different termination arithmetic changes iteration counts and
rounded fits. They preserve historical iteration order. They have neither
bracketing nor positivity enforcement; caller responsibilities include
domain, score validation and mapping failure to a status. A zero relative
iterate produces `0/0` even for a zero step. Absolute zero step can report
success regardless of the objective. NaN rejection applies to the next iterate in the relative variant and to
the step in the absolute variant; it is not a general validation of arbitrary
starts or callbacks. General finite-state validation and score checks are not
part of this contract. Consequently
neither helper is a drop-in replacement for a safeguarded root search.
Convergence is local numerical evidence, not existence or global optimality.
`tests/test_bfgs.cpp` and family `gamma/test.cpp`, `fatigue/test.cpp` exercise
optimizer/fitter behavior; they do not prove all-input convergence.

`inverse_digamma_scalar(x)` seeks the unique positive solution of `psi(y)=x`
(strict monotonicity follows from `psi'(y)=sum_(k>=0) 1/(y+k)^2 > 0`).
Kumaraswamy uses it to initialize a shape. Below `x+EulerGamma<-30` it
uses a reciprocal asymptotic polynomial; elsewhere it starts at
`1/log1p(exp(-x))` and takes two Newton steps, followed by up to two more
polishing steps. All derivatives now come from the Boost seam, not Rmath.
The `1e-14` residual is an early exit, not a final certificate; there is no
fallback on overflow or failed polishing. Large positive input can overflow
its initialization. No general error bound is established for four steps.
The step-7 residual grid and Kumaraswamy goldens are numerical evidence.
A safeguarded replacement must preserve initialization behavior or explicitly
measure downstream fit differences; no family changes are made here.

## Log-tail arithmetic and inversion (`special/tails.hpp`, `special/special.hpp`)

`log1mexp`, `log_sum_exp` and `log_diff_exp` retain small tails and explicitly
handle endpoints. For example `log1mexp(-1000)` is approximately `-exp(-1000)`
(the latter rounds to zero in binary64), while `log_neg_log1mexp(-1000)`
returns `-1000` without exponentiating. Its omitted relative correction below
−36 is under `1.2e-16`. Log-product, softplus, affine and support-residual
helpers preserve intermediate range; libm rounding is not enclosed.
The two probability-offset functions have different written constants and
one changes sign with the tail: keep their contracts distinct.
`fs_log1mexp` is already an alias to the shared implementation.

Normal tails use erfc below 8 and an 80-level Mills continued fraction
above it; normal quantiles use a local odd polynomial near the median and
otherwise bisect the smaller log tail. Mills and its slope preserve the
normal convolution terms without subtracting rounded probabilities.
The fixed depth and median polynomial have numerical truth-table evidence,
not interval error enclosures for every real input. The gamma-ratio helper
shifts positive arguments to at least 16 and applies Stirling through
inverse power 11; its stated omitted term is below `3e-17` per log gamma,
excluding arithmetic rounding and cancellation. Discrete series require a
caller-supplied bound on every remaining ratio and use a geometric tail
certificate, with a million-term budget returning NaN. At counts beyond
unit-spaced binary64, constant-ratio continuation is a numerical approximation.
Discrete quantiles require monotone valid tails and return the first crossing
(or adjacent representable value at enormous counts). Tail callback NaNs
are not universally checked: phase B failure-propagation review remains.

Two gamma-tail paths have different contracts. `gamma_log_tails` works in
log x, uses leading small-x asymptotics below −36, then Boost or a positive
series / Lentz fraction. Its current series/fraction loops do not report
budget exhaustion. `fs_gamma_p` works in x, uses extended Boost tails and
an asymptotic upper-tail expansion with a remainder acceptance condition.
`fs_beta_fraction` uses modified Lentz updates, `1e-300` denominator floors,
`4e-16` multiplicative-change stopping and a 10,000-step NaN failure.
That change test is not a rigorous remainder bound. `fs_t_p` combines it
with a log beta prefactor after Boost underflow; `fs_normal_p` uses a
12-term Mills expansion after backend underflow. `fs_log_quantile` brackets
a positive root by doubling and bisects to adjacent doubles, or returns
NaN at 1100 steps; overflow is represented by infinity. Its initial callback
and bracket validation need further scrutiny before broader reuse.

Gamma log-quantiles (150 steps) and normal quantiles (100 steps) currently
return their final midpoint without an explicit exhaustion flag. This is
legacy behavior, not a certified all-input claim. Do not consolidate these
with the explicitly failing beta solver merely because both bisect.
See [tail report](2026-10-29-tails-report.md),
[truth design](2026-10-29-dpqr-truth/README.md),
`tests/test_tails.cpp`, `tests/test_no_throw.cpp` and each family's truth tests.
Truth agreement is sampled evidence and does not prove all-domain accuracy.

The ordinary seam's `lgamma` is `std::lgamma`, and `lbeta(a,b)` is the
identity `lgamma(a)+lgamma(b)-lgamma(a+b)` for positive finite shapes.
It has no stable large-shape fallback: cancellation or overflow of `a+b`
can invalidate a mathematically finite answer. The no-throw Boost policy
controls Boost calls, not libm errno/floating-point flags. Ordinary normal,
logistic, gamma, beta and discrete CDF/quantile wrappers evaluate probabilities
in natural coordinates; logging or exponentiating afterwards cannot preserve
an underflowed log tail. They are not interchangeable with the log-coordinate
helpers merely because both expose a CDF or quantile.

Closed-form `dnorm`, `dpois`, `dgeom`, `dnbinom` and the log branch of `dgamma`
retain historical validation/rounding conventions and use libm log-gamma.
For example Poisson uses `k log(lambda)-lambda-lgamma(k+1)` with an explicit
zero-rate member and a `1e-7` integer-distance check; this expression can
cancel for large nearby k and lambda. The family Poisson implementation
has additional stable paths and must not be replaced by this helper without
comparison. The legacy shared `dgamma(x=0,shape<1,log=true)` returns −Inf,
where the interior density limit is +Inf; this is a concrete endpoint review
gap, not changed here. Geometric's rounding contract also differs from the
integer-distance checks in Poisson/NB. Existing family truth tables do not
establish correctness of every otherwise unused seam wrapper. No unchecked
wrapper-wide replacement is accepted in this phase.

## Relocated beta bridge (`special/beta_tails.hpp`)

Five direct include users are beta, GB2, genbeta, beta-prime and location-scale
t tails; additional families delegate through these. The old GB2 ownership
was historical placement, not a mathematical dependency. The namespace
`missing_detail` is retained to make this a layout-only change.

Inputs to `beta_log` are complementary log coordinates and positive shapes.
For `log x=-10000`, forming x first would lose the tail; the bridge keeps
`a log x + b log(1-x) - log B(a,b)` and the favourable continued fraction.
It uses Boost's directly computed probability when at least `1e-290` and
the coordinate is above the minimum normal logarithm, otherwise the fraction
continuation. It computes both tails and complements the smaller one.
`beta_logit_q` inverts in log odds, expands each bracket up to 1024 times,
validates the bracket and rejects NaN, then bisects up to 120 times.
It returns only on adjacent floating-point endpoints or an exactly represented
median; an exhausted budget yields NaN. Monotonicity holds mathematically
for positive beta shapes; floating-point fraction errors are not bounded.
No interval certification of the CDF is claimed. Public callers own parameter
validation; invalid shapes are not a supported internal-helper contract.
The fraction's own stopping limitation remains as above. Tests: beta, GB2,
genbeta, beta-prime, lst, ht, std and sstd truth/regression cases. The move
preserves every token except include spelling and explanatory comments.

## CMP and skew densities

`special/cmp.hpp` needs custom normalization and moments, not a substitute
Boost distribution call. It sums weights relative to their mode with
geometric moment-tail bounds; broad modes use Euler–Maclaurin with a bounded
remainder and quadrature checks, returning explicit work/numerical failures.
For `(lambda,nu)=(4,2)` the direct path supplies normalization and centered
moments to fitting. Applicable broad-mode thresholds, fallback, mathematical
bounds and independently generated references are in the
[CMP report](2026-10-29-cmp-normalizer-report.md) and header. Those are
real-arithmetic truncation bounds, not floating-point enclosures.
`tests/test_cmp.cpp` includes small/broad modes and failure statuses.
Keep this custom path: a Poisson approximation would change both model and
contract. No new CMP benchmark or accuracy claim is made by step 7.

`special/skewed_densities.hpp` implements the Fernandez–Steel transformation
of symmetric, variance-standardized normal, t and GED bases: side-dependent
scale xi and reciprocal xi, normalizer `2/(xi+1/xi)`, followed by moment
recentering/rescaling. Valid inputs require positive scales/xi, positive GED
shape, and t df>2. `sstd_sigma` uses log-gamma differences (raw gamma ratios
formerly overflowed above df 343); the t log normalizer switches at df 128
to a gamma-ratio asymptotic polynomial through inverse power 5. No
all-input rounding bound at that cutoff is established here. GED moment
ratios still use raw tgamma and can overflow for tiny shape. There is no
fallback or error enclosure; callers check finite objectives. These are
libm paths, not Boost wrappers. A replacement with Boost density calls must
preserve unit-variance standardization and shape-dependent shifts.
Family goldens and mpmath truth tests are sampled validation. See the
sstd dossier for the corrected historical absolute-moment scaling; the
header preserves the prior error's explanation. Keep pending an explicit
small-GED-shape range review.

## Mathematical references and provenance limits

[Minka, *Estimating a Dirichlet distribution*, Appendix C](https://tminka.github.io/papers/dirichlet/minka-dirichlet.pdf)
describes inverse-digamma Newton iteration with asymptotic initialization.
Its initializer and iteration count differ from this repository's routine;
its accuracy discussion does not certify our four-step variant. The original
source of our smoothing initializer and higher-order reciprocal coefficients
has not been established.

[DLMF §7.9](https://dlmf.nist.gov/7.9) gives error-function continued fractions;
[DLMF §7.12](https://dlmf.nist.gov/7.12) gives the complementary-error-function
asymptotic series. They are mathematical references for the Mills paths,
not proofs of the chosen 8/12/26 thresholds or 80/40/fixed-series depths.
[DLMF §8.17(v)](https://dlmf.nist.gov/8.17#v) gives incomplete-beta continued
fractions on the positive-shape probability domain. The chosen denominator
floor and change tolerance still need numerical analysis beyond those
identities. Provenance of the repository's exact fraction implementation is
not established here. The shared-routine note and phase A measurements retain
that uncertainty instead of inventing a historical source.

## Other shared core routines

BFGS uses Armijo backtracking and inverse-Hessian updates, exposing iteration,
line-search and bad-start statuses; relative objective, step or gradient
criteria can accept a local candidate. Finite differences use coordinate-scaled
central differences, requiring finite smooth objectives; they supply neither
observed-information validity nor global guarantees. Multistart selection
checks finite objectives and scores of failed searches with normalization
undone. It can improve candidate coverage but cannot certify a global optimum.
See [multistart report](2026-10-30-switchfix-multistart-report.md), header
controls and `tests/test_bfgs.cpp`. Reparametrizations are log/log-shift maps;
overflow and boundary representability remain caller responsibilities.
Reductions are ordered double summations without compensated accumulation;
changing order changes goldens. Tie collapse counts exact floating-point
sample minima; it does not infer approximate ties. These contracts explain
why numerical-layout cleanup must not silently replace these routines.
