**SGED certification — TASK-116.12**

Implemented global certificates for held-shape submodels, using the task's
fallback provision. The default fitter and golden fixture are unchanged.
The family-wide `fit.certified` remains `no`: fits estimating either shape,
and asymmetric scale-only fits with held `nu < 1`, still lack a global
certificate. Their precise gaps are listed below.

Tested base: `bb734cd78b846a565d8835606328bb1285a82db6`, with uncommitted
changes identified by the [source manifest](2026-11-06-sged-cert-data/source-manifest.sha256).
No commit, board edit, dependency installation, or sub-agent was used.
All required final runs finished successfully.

The implementation is in [certificate.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/sged/certificate.hpp),
with dispatch in [mle.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/sged/mle.hpp).
The [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md)
contains the proofs, arithmetic contract, convergence criteria and failure
behavior. The sged-specific common binding now translates certificate
failures into structured `numerical`, `tie` and `budget` conditions.
`SgedResult::certified` identifies successful certified C++ results.

**Scope and mathematical results**

Here `mean` is the public mean, rather than the density's mode. Both shapes
are held finite in every newly certified path.

| Free coefficients | Result |
|---|---|
| none | Existing all-held evaluation; no optimization claim |
| mean | Certified for every positive held nu and xi |
| sd | Certified if held nu >= 1, or held xi = 1; otherwise open |
| nu | Open: held-moment shape profile |
| xi | Open: held-moment skew profile |
| mean, sd | Certified for every positive held nu and xi |
| mean, nu | Open: shape profile with sd held |
| mean, xi | Open: skew profile with sd held |
| sd, nu | Open: shape/scale profile with mean held |
| sd, xi | Open: skew/scale profile with mean held |
| nu, xi | Open: both shapes with both moments held |
| mean, sd, nu | Open: free shape with xi held |
| mean, sd, xi | Open: free skew with nu held |
| mean, nu, xi | Open: both shapes with sd held |
| sd, nu, xi | Open: both shapes with mean held |
| mean, sd, nu, xi | Open: unchanged default |

For mode `c`, ordinary kernel scale `h`, and fixed shapes, define
`Q(c) = sum_left xi^nu (c-x)^nu + sum_right xi^(-nu) (x-c)^nu`.
The likelihood is `C - n log(h) - Q(c)/h^nu`. With scale free,
`h^nu = nu Q(c)/n` profiles scale exactly; minimizing Q solves both the
location-only and joint location/scale problems.

For nu > 1, Q is strictly convex and its unique minimizing mode lies in the
sample range. Signed, compensated, logarithmically rescaled derivative
sums bracket that root. For nu = 1, cumulative slopes solve the weighted
median problem, with quantile `1/(1+xi^2)`. For nu < 1, strict concavity
between distinct observations and monotonicity outside the sample range
prove that every minimizing mode is an observation. Enumerating those
knots is therefore a global search, without a unimodality assumption.

Uniqueness fails in supported held submodels. For data `(-1,1)` and held
xi = 1, every held nu below one has two maximizing modes, at -1 and 1;
at nu = 1 the whole interval between them maximizes likelihood. These
counterexamples hold with scale held or profiled. They do not establish
nonuniqueness of an all-free fit. Exact and numerically unresolved ties
stop with candidate coefficients rather than selecting an arbitrary fit.

With mean held and nu >= 1, set `t = unit/sd`. The negative likelihood is
`F(t) = -n log(t) + sum V(a_i t + shift)`, up to constants. Each V is convex;
the logarithmic term makes F strictly convex. Unless every observation
equals the held mean, F diverges at both ends and has exactly one minimum.
The implementation brackets the monotone derivative F'(t), then bisects
in log t. This proof also covers nu = 1 joins through signed endpoints.
With xi = 1 held, the analytic scale profile works for every positive nu.

Constant data permit unbounded scale collapse when location is free, or
when every observation equals the held mean. With positive scale held,
the maximizing mode is unique even for a singleton. Uniform and half-GED
faces cannot be reached while both shapes are held finite, so the new
scopes do not require those face comparisons.

**Numerical contract and work**

Certification uses exact-arithmetic proofs conditional on computed
comparisons, with practical floating-point allowances. It does not claim
outward-rounded interval arithmetic.

- Smooth location roots require a normalized mode bracket at most 1e-9
  wide. Termwise distance bounds enclose the recovered mean and profiled
  sd; each public-coordinate width must be at most `1e-7*(1+abs(coef))`.
- Stored mean recovery must preserve normalized mode within 1e-9.
  A cancellation allowance of 32 machine epsilons on its summands must
  also fit within `1e-7*(1+abs(mean))`.
- Scale-only roots require a log-scale bracket at most 1e-9 wide and
  stored log-coordinate recovery within 1e-8.
- Score comparisons use compensated sums and a 512-epsilon allowance
  scaled by exponent conditioning and sum magnitude. Candidate likelihood
  allowances are `1e-10*(n+abs(logLik))`, widened to 1e-7 for nu < 1.
  Mode-coordinate and stored-coefficient likelihoods must agree within
  that allowance; otherwise the fitter stops numerically.
- Solver work is capped by `min(iterlim,100000)` steps. Scale tail
  bracketing shares that cap with bisection. Exhaustion is a numerical
  failure, never a successful last iterate. Knot enumeration allows
  `128*min(iterlim,100000)` evaluations; exhaustion is a structured budget
  stop with finite witnesses and a conservatively infinite global upper
  bound. Comparison ties retain the competing witnesses.

The fixed coefficient enclosures govern certified paths independently of
`gradtol`; legacy optimizers retain their existing tolerance behavior.
Nonfinite normalizers, unrepresentable coefficients, uncertain root
enclosures and failed reconstruction checks stop. No shared numerical
header, special function, d/p/q/r source, truth fixture, code generator or
sstd file was changed.

**Final validation**

The [machine-readable validation record](2026-11-06-sged-cert-data/validation.json)
and [compact test summary](2026-11-06-sged-cert-data/final-test-summary.txt)
record the final source and results. Workstation jobs used the required
slot wrappers, one BLAS/OpenMP thread per worker and nice level 10; simbox
jobs used the supplied helper. Temporary files stayed in the worktree.

| Final check | Machine | Result |
|---|---|---|
| Targeted sged doctest | Workstation | 29 cases, 4,208 assertions; no failures/skips |
| just cpp-test | simbox | 106/106; 43.95 s test time |
| UMVL_TEST_TIER=full just cpp-test | simbox | 127/127; 56.79 s |
| just cpp-test-no-long-double | simbox | 127/127; 56.48 s |
| just test | Workstation | PASS 20,911; FAIL 0; WARN 0; SKIP 154; 118.651 s elapsed |
| just test-full | simbox | PASS 43,731; FAIL 0; WARN 0; SKIP 0; 292.497 s elapsed |
| just js-build | Workstation | Passed |
| just js-test | Workstation | 432/432; no failures/skips |
| just site-check | Workstation | Passed: 99 family pages and C++17 snippets, MathML, links, graph and browser checks |

The dev skips are tier-selected tests, including four pre-existing sged
oracle/replication cases; full has no skips. R test warning counts above
are separate from existing compiler and roxygen diagnostics in the build
logs. The site browser check refreshed a historical screenshot; that
generated change was restored byte-for-byte. Large generated R object
files and site snippet binaries were removed after validation.

Workstation probes used GCC 15.2.0; simbox CMake used GCC 13.3.0.
R was 4.6.1 on both machines, with g++ 15 for the bindings. Node was
22.22.1; the independent oracle used mpmath 1.3.0 at 80 digits.

The independent gamma-moment references checked 125 deterministic cases:
121 returned fits passed coefficient and likelihood comparisons, and four
cases correctly stopped numerically without a certified estimate. The
[location/joint grid](2026-11-06-sged-cert-data/oracle-results.json)
has 90 cases over nine shapes, five skews and held/free scale; the
[asymmetric scale grid](2026-11-06-sged-cert-data/oracle-scale-results.json)
has 35 cases over seven shapes and five skews. The latter returned a
verified fit in every case.

Maximum coefficient errors, normalized by `1+abs(reference)`, were
1.38e-8 for mean and 4.78e-9 for sd in the location/joint grid, and
2.97e-10 for scale-only sd. Maximum likelihood errors normalized by
`n+abs(reference logLik)` were 3.18e-9 and 2.09e-9 respectively.
Coefficient tolerances were 2e-8; likelihood tolerances were 2e-7 for the
location/joint grid and 2e-8 for scale-only.

The four permitted stops all have nu = 0.4: xi = 0.3 with scale held or
free, and xi = 3 or 10 with scale held at 1.7. Some knot evaluations exceed
the cusp reconstruction allowance, so enumeration refuses a certificate.
These are status checks, not successful numerical maximizer comparisons.

Earlier checks exposed and resolved three issues: a density-identity
tolerance too tight at reconstructed cusps; four R assertions counting
data-frame columns instead of candidate rows; and a smooth-score/reference
accuracy assumption at a nu = 1 kink. The certificate now uses the settled
log-gamma moment helper and explicit reconstruction guards. Tests treat
kinks with likelihood comparisons rather than a smooth central score;
the nu = 1 scalar-reference likelihood tolerance is 2e-8. Final runs above
supersede those failures. Neither the default golden nor pre-existing C++
or R tests was changed.

**Timings and reproduction**

Paired native -O2 measurements use the unchanged 500-point golden data
(historical sampler seed 12764). Times below are CPU milliseconds per fit:
one default fit and ten repetitions of each held fit, under one slot.
They are finite workload measurements, not general runtime guarantees.

| Fit | Before | After |
|---|---:|---:|
| Default | 325.792 | 335.283 |
| mean free; sd=1, nu=3, xi=3 held | 620.824 | 0.3224 |
| mean/sd free; nu=3, xi=1 held | 380.530 | 0.3538 |
| sd free; mean=0, nu=3, xi=1 held | 9.3174 | 0.0335 |
| sd free; mean=0, nu=3, xi=3 held | 12.500 | 0.5282 |

The default coefficients and likelihood match the golden exactly; its
single-run timing difference is not evidence of a performance change.
All conditional coefficient changes are within golden tolerance, with
maximum normalized change below 1e-8. The
[paired results](2026-11-06-sged-cert-data/timing-results.json) retain wall
times, CPU times and coefficient/likelihood differences.

For a deterministic 100,000-point trigonometric workload with nu=4 and
xi=1.7, joint location/scale, location-only and asymmetric scale-only fits
all certified in 31, 31 and 32 steps. CPU times were 0.094, 0.070 and
0.110 seconds respectively; see
[large-workload output](2026-11-06-sged-cert-data/timings-large.txt).

From the repository root, with the task's temporary-directory environment:

```sh
/tmp/umvl-cpu/run bash dev/research/evaluations/2026-11-06-sged-cert-data/oracles.sh
/tmp/umvl-cpu/run bash dev/research/evaluations/2026-11-06-sged-cert-data/benchmark.sh
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-06-sged-cert-data/check-timings.py
```

The benchmark script reconstructs the pre-change fitter from the recorded
base with read-only `git show`. The retained C++ drivers also permit
standalone compilation with C++17, -O2, the core include directory and the
plain BH/Boost include path. `targeted-r.sh` reproduces the family-only
full-tier R check. Full-suite commands and machines are in the table above.

**Open cases**

1. **Any estimated nu or xi.** The twelve other nonempty free-subset
   patterns in the scope table retain their previous searches. They need
   global profile bounds, isolation of competing maxima and certified
   tail/face comparisons. Profiling side scales in the all-free model
   reduces its objective to a function of mode and shape involving
   `A^(1/(nu+1)) + B^(1/(nu+1))`; it does not establish unimodality or close
   the infinite-shape tail. Held moments further couple mode and side
   scales, preventing direct reuse of the held-shape certificate. Existing
   numerical uniform/half-GED comparisons and finite shape scans do not
   prove global optimality. The flagged sd-held constrained uniform
   endpoint and coupled-approach cases still require rate/bound arguments.
   General existence and uniqueness beyond the dossier's stated cases
   remain unresolved. The default was therefore left unchanged.
2. **Asymmetric scale-only with held nu < 1.** A maximum exists unless all
   observations equal the held mean, but the inverse-scale objective is
   no longer convex. Certifying this branch needs global root/kink
   isolation and tail bounds, followed by comparison of every surviving
   candidate. The legacy scalar scan remains, with no uniqueness claim.
3. **Large cusp workloads and floating-point reach.** Knot enumeration is
   quadratic in the number of distinct observations. Its large-n speed
   and default-budget behavior have not been established; the 100,000-point
   timing probe covers smooth paths only. Very small shapes can also fail
   the retained normalizer arithmetic, and cusp reconstruction can produce
   the documented numerical stops. No wider representable-range guarantee
   is claimed. No change to settled d/p/q/r or special-function code was
   required for the implemented certificates.

The report-display command was attempted after completion. The local viewer
rejected this worktree with HTTP 403 because its readable-root list omits
the worktree. No viewer configuration or files outside the worktree were
changed; the report and retained evidence are available at the paths above.
