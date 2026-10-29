# tlambda implementation report

Files added: `cppunivariate/include/cppunivariate/distributions/tlambda/`:
`README.md`, `mle.hpp`, `dpqr.hpp`, `adapter.cpp`, `dpqr.R`, `spec.py`,
`test.cpp`, `test.R`, `golden.json`. Only the new golden was generated.

Shared files changed separately:
- `cppunivariate/include/cppunivariate/cppunivariate.hpp`: header registration.
- `univariateML/CANONICAL_NAMES.md`: coefficient names.
- `univariateML/R/code_helpers.R`: guarded extraDistr quantile and sampler repairs.

Additional new file: `codex-report.md` (this report).
No commits or writing git commands. No board edits, dependency installs or subagents.

## Mathematics proved

Q'(u)=sigma*(u^(lambda-1)+(1-u)^(lambda-1)) is positive, giving a unique
CDF root and density 1/Q'(u). The analytic lambda=0 limit is logistic.
Positive shape has support [mu-sigma/lambda,mu+sigma/lambda]; nonpositive
shape has full real support. Differentiating Q' gives unimodality below 1,
U-shapes on (1,2), and central unimodality above 2.

The unrestricted location-scale likelihood is unbounded. Put mu at an
observation and sigma=lambda*A, with A strictly exceeding every distance
from mu. If its multiplicity is k, the centre density is
2^(lambda-2)/(lambda*A). At noncentral distance d, the smaller probability
has (1-u)^lambda -> abs(d)/A, hence sigma*Q'(u) ~ lambda*abs(d), and f(d) ~ 1/(lambda*abs(d)). Therefore
ell = k*lambda*log(2) - n*log(lambda) + O(1). The task's -(n-1)*log(lambda)
term omits the centre's 1/lambda factor; divergence is unchanged.

Fitted domain: the principal unimodal branch [-1,1]; fixed 2 is accepted
as an exact uniform alias. This avoids the U-shaped branch (1,2) and its
endpoint-density discontinuity at 1. Positive shapes have bounded support;
endpoint density is zero below 1, 1/(2*sigma) at 1, and 1/sigma above 1.
The uniform representations at 1 and 2 have scales differing by two.

An attained maximum exists for n>=3 and every multiplicity k<n/2.
Here is a uniform coercivity bound supporting the README argument.
Write b=1-lambda in [0,2], u<=1/2, D=u^(-b)+(1-u)^(-b),
and |Q(u)|=integral_u^(1/2) D(v)dv. D>=2, so f<=1/2.
For b in [3/2,2], integral v^(-b)dv <=2*u^(-b/2).
For b in [0,3/2], Holder interpolation between 0 and 3/2 gives the same
bound. The other integral is <=2, hence |Q|<=4*u^(-b/2) and Q^2/D<=16.
Thus f_mu,sigma(x)<=min(1/(2*sigma),16*sigma/(x-mu)^2).
Scale collapse near a location with k observations makes the likelihood
O(sigma^(n-2k)); diverging scale or location also makes it vanish.
Upper semicontinuity on [-1,1], including the uniform endpoint convention,
then gives compact upper level sets and attainment. For fixed negative
shape -a, the sharper collapse exponent is (n-k)/a-k.
For a>(n-k)/k this exponent is negative, so sufficiently negative fixed
shape also has unbounded likelihood through scale collapse; distinct
samples already fail when a>n-1. These conditions are sufficient; other
tie configurations are rejected.

The location likelihood is not generally unimodal on slices: fix lambda=-1
and a small positive sigma. Its central log-density curvature is
-3/(8*sigma^2), while all other observations contribute bounded location
scores near each distinct observation. Consequently each observation has a
nearby local location maximum (offset O(sigma^2)) as sigma tends to zero.
This proves multimodal location sections, not multiple joint maxima in all
three parameters. The implementation assumes no joint unimodality;
a complete joint-mode census remains open.

## Checked numerically

For x=(-2,-1,0,0.3,1,3), mu=0 and sigma=4*lambda, the
unrestricted log likelihoods at lambda=(2,40,80) are
(-12.4766,2.07364,25.7199). These use a different explicitly declared
sample from the task, whose six observations were not supplied. For n=6,
the asymptotic increase from 40 to 80 is 34*log(2)=23.567, consistent with
the task's reported increase of 23.6.

The quality check passed all three checks: implicit log likelihood matched
the golden exactly, standardized score was 4.47e-7 with positive observed
information, and the eight-start SciPy oracle improved log likelihood by
only 6.25e-12. The optimizer is a numerical multistart global search, with exact uniform candidates and a separately optimized -1 face;
it is not a mathematical global optimality certificate.

## Assumptions and open questions

Assumed: the multistart coverage is adequate beyond the tested samples.
There is no global-search certificate.

The proposed [-1,1] fitting restriction is a deliberate alternative to
[-1,2]: the latter includes a non-upper-semicontinuous uniform junction.
Evaluation still permits every finite real shape. Only lambda is fixable;
location/scale fixed subsets are explicitly unsupported by the adapter.
Whether to include the U-shaped branch with a different density convention
or a separate junction supremum status is left to the reviewer.

The numerical fitter rejects a nonfinite range or standardization that
merges originally distinct observations, rather than fitting rounded ties.
The endpoint solver uses a direct log-deficit equation for bounded tails;
ordinary quantile residuals are ill-conditioned there. The uniform support
is rounded outward when midpoint/scale representation requires it.

## Reference-package defects (ready-to-paste KNOWN_ISSUES rows)

| Package | Defect | Reproducer | Effect here |
|---|---|---|---|
| extraDistr 1.10.0.5 | `qtlambda`: Upper log-tail probabilities round their complement to one, returning infinity instead of a finite quantile. | `extraDistr::qtlambda(-100, 0, lower.tail=FALSE, log.p=TRUE)` returns `Inf`, expected `100`. | Package-owned quantile plus runtime `broken()` repair. |
| extraDistr 1.10.0.5 | `qtlambda`: Tiny nonzero shape loses digits by subtracting nearly equal powers. | `extraDistr::qtlambda(-1, 1e-12, lower.tail=FALSE, log.p=TRUE)` gives about `0.5413447`, versus the analytic limit `0.5413249`. | Stable `expm1` formula and runtime `broken()` repair. |
| extraDistr 1.10.0.5 | `rtlambda`: The sampler inherits tiny-shape cancellation. | `set.seed(1); a <- extraDistr::rtlambda(10,1e-12); set.seed(1); max(abs(a-qlogis(runif(10))))` is about `9.97e-5`, versus O(1e-12). | Owned inversion sampler and runtime `broken()` repair preserving the RNG state. |

## Fitted-shape simulation

20 independent samples per row; C++ mt19937_64 seed 20261029,
libstdc++ normal/uniform generators; logistic and Cauchy by inversion.
All 160 final fits returned finite likelihoods and successful statuses.
These are descriptive Monte Carlo checks, not a consistency result.

| sample | n | min | Q1 | median | Q3 | max | lambda=-1 | lambda=1 |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| normal | 50 | -0.1174 | 0.0625 | 0.1542 | 0.3169 | 0.4566 | 0 | 0 |
| logistic | 50 | -0.0947 | -0.0106 | 0.0713 | 0.1687 | 0.3107 | 0 | 0 |
| uniform | 50 | 1.0000 | 1.0000 | 1.0000 | 1.0000 | 1.0000 | 0 | 20 |
| Cauchy | 50 | -1.0000 | -0.9879 | -0.9031 | -0.7509 | -0.4505 | 5 | 0 |
| normal | 500 | 0.0615 | 0.1030 | 0.1305 | 0.1580 | 0.2263 | 0 | 0 |
| logistic | 500 | -0.1203 | -0.0206 | -0.0045 | 0.0642 | 0.1207 | 0 | 0 |
| uniform | 500 | 0.8883 | 1.0000 | 1.0000 | 1.0000 | 1.0000 | 0 | 17 |
| Cauchy | 500 | -1.0000 | -0.9456 | -0.8829 | -0.8523 | -0.7824 | 3 | 0 |

## Validation

- C++: 5/5 ctest targets passed on final sources, including 3 tlambda cases.
- Full `just test`: FAIL 0, WARN 0, SKIP 1, PASS 6729. This run includes
  all evaluation/fit/oracle tests before the final normalization input guard.
- After that guard and final nonfinite-result rejection: rebuilt package;
  the actual new R guard test plus strict golden parameter/logLik replay
  passed 3/3. The final sources contain 16 distribution test blocks.
- `just quality tlambda`: 3/3 checks passed; eight independent oracle starts.
- 160/160 simulation fits succeeded; 20/20 extra uniform rounding checks
  returned finite likelihoods. Final tail probe succeeded.
- Existing goldens did not change; only tlambda/golden.json was generated.
- `git diff --check` passed. No source changes remain unvalidated.

Scratch scripts and logs: `/tmp/umvl-codex/tlambda/`.

Artifact display: the local viewer returned HTTP 403 because this worktree
is outside its configured readable roots. The report remains in the worktree;
no viewer configuration or files outside the authorized roots were changed.
