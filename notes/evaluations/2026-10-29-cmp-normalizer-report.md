# CMP C++ implementation review

Implemented C++17 header-only evaluation and fitting; no R-facing changes.
No commits, R wiring, distribution directory, golden changes, or board edits.

## Files

- `special/cmp.hpp`: moments, numerical error bounds, sufficient statistics,
  joint profile fit, fixed-nu and fixed-lambda fits, explicit limit statuses.
- `tests/test_cmp.cpp`, `tests/cmp_reference.inc`: dedicated doctest target.
- `tests/generate_cmp_reference.py`: independent mpmath reference generator.
- `tests/CMakeLists.txt`: registers `cmp_tests`.
  Paths above are relative to `cppunivariate/include/cppunivariate/` for the
  header and `cppunivariate/` for tests.

## Mathematics: proved bounds and identities

For fixed mean, differentiating the eta score gives eta'(nu)=Cov(X,h)/Var(X).
Thus d E(h)/d nu = -[Var(h)-Cov(X,h)^2/Var(X)] < 0, strictly because h is
not affine on the full integer support. Its endpoints are g(m) at nu=0,
and b(m) as nu tends to infinity. Hence b(m)<t<g(m) is the necessary and
sufficient criterion for a unique finite joint MLE (m>0). Equality at the
lower endpoint means observations occupy adjacent integers; constants are
point masses. The geometric endpoint wins likelihood ties within 1e-8*n.

Direct evaluation uses log weights relative to the mode. It bounds both
monotone tails using the decreasing CMP ratios. If q bounds the remaining
ratio, sum j^p q^j (p=0,...,4) bounds mass and centered second moments.
Residual h-slope*X statistics also avoid subtracting the nearly collinear
covariance terms in the profile information. Summation may truncate the
finite lower tail using the same certificate rather than visiting zero.

Broad masses use a smooth Euler--Maclaurin window around z=exp(eta/nu),
with sigma=sqrt(z/nu), |x-z|<=16*sigma. Eligibility requires z>1e6,
sigma>=1e3 and 16*sigma<z/2. Work: 4096 quadrature evaluations.
The positive polygamma series gives, for j>=2 and x>=L,
|h^(j)(x)| <= (j-2)! [L^(1-j)+(j-1)*L^(-j)].
Absolute Taylor coefficients and convolution bound all six integrands'
derivatives. The order-16 EM remainder is bounded by
4/(2*pi)^16 times integral |f^(16)|. Endpoint bounds include every
Bernoulli-polynomial term, so integer alignment is unnecessary.
Each composite 16-point Gauss panel has error at most
width^33*(16!)^4/[33*(32!)^3] times sup |f^(32)|.
Stirling through x^-7 has remainder <=1/(1188*x^9); both endpoints'
errors enter the integration bound. The two outside tails use geometric
bounds with quadratic bounds on the log-factorial residual.
The smooth path returns Ok only if the combined estimate is below tol.
These are real-arithmetic truncation bounds, not interval roundoff bounds.

## Checked numerically

All 72 grid points plus (lambda,nu)=(4,2) pass, covering all four evaluation cases.
Inputs in references are exact binary64 eta and nu, avoiding a change
of distribution when taking log(lambda) or dividing by a small nu.
Reference precision is at least 70 decimal digits, increased with mode.
Small modes use independent loggamma sums; large modes use high-precision
integration of the loggamma summand. The latter is a numerical reference,
not a rigorous interval proof of the integral/discrete-sum difference.

R regression: 4248 passed, 0 failed, 0 warnings, 0 skips.
Existing quality validation: 69/69 passed. Existing goldens unchanged.
C++: 5/5 ctest targets; CMP 8/8 doctest cases, 619/619 assertions.
Includes independent coordinate-search optimization, Poisson recovery,
under/overdispersion, all statuses, fixed parameters, geometric likelihood
ties, adjacent support, and broad/narrow fits near 2^31. Grid assertions
require relative error <3e-15 for every returned moment and log Z.

Maximum relative errors (zero means identical rounded binary64):

| Regime (calls) | log Z | E X | Var X | E h | Var h | Cov |
|---|---:|---:|---:|---:|---:|---:|
| Direct (64) | 0 | 0 | 2.19e-16 | 1.63e-16 | 3.49e-16 | 2.46e-16 |
| EM (9) | 0 | 0 | 0 | 1.67e-16 | 0 | 0 |

Representative workstation timings (default CMake flags, no optimization;
individual call timings are printed by cmp_tests): direct calls 3.5 us–18.8 ms,
EM calls 2.1–2.9 ms; entire 73-call grid
0.0580 s; million-observation fit 0.476 s, 1418 summed terms.
Evaluation examples (lambda, nu, microseconds per call):
- (2, 1): 60.209
- (4, 2): 36.961
- (0.9, 0.05): 359.17
- (2, 0.075): 18807.1

## Assumptions / limitations / open questions

Floating-point rounding and libm accuracy are checked against references,
not enclosed with directed rounding. Tests currently target this Linux
workstation's extended-precision long double. Allocator failure is not
caught, consistent with existing vector-based core code.

The geometric log normalizer, mean and variance use closed forms; its
log-factorial moments are summed with a tail bound. Very broad geometric
inputs can exhaust the work budget (closed-form log Z, mean and variance
remain available on WorkLimit). Fixed-nu=0 fits use a separate exact path.
A block-Jensen lower bound on g(m)
permits high-count concentrated fits without enumerating that geometric.
There is no general accelerated geometric h-moment evaluator yet.
Modes above 9e15 outside the smooth regime report WorkLimit explicitly.
The API reports eta, since lambda itself can overflow at a finite fit.

## Later R wiring

Installation of COMPoissonReg remains the author's decision. Add the
complete distribution folder and adapter only after it is available.
Use lambda=exp(eta), nu, exposing both fixed-parameter controls. Map limits
to geometric, shifted Bernoulli, or point-mass messages. Use COMPoissonReg
`dcmp/pcmp/qcmp/rcmp`; pass log normalization and a common tightened control
list through the future r.args map: propose hybrid.tol=0 to disable the
default leading-asymptotic shortcut, truncate.tol=1e-14 (or tighter), and
ymax above the certified tail cutoff; confirm these on the installed version.
Validate the actual installed API and inverse-CDF inequalities first:
d/p/q currently use potentially different normalizer branches. The present
C++ work does not establish accuracy of those external functions.
