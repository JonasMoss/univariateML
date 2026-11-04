# TASK-138: production C++ all-free Burr XII certificate

The production port is implemented and is the default all-free Burr fitter.
All final required development and full tiers passed locally before this report
was written. The port reproduces **all 50 retained research outcomes**: 49
global certificates and the expected subdivision-budget stop. An additional
binary64 seam proxy verifies the explicit tie diagnostic through C++ and R.
No commit or publication was made.

Tested base: `d4d26e5dd349b07b5b574d40465856574350869d`, branch `codex/burr-port`, with the
uncommitted implementation identified by the source and input SHA256 values in
[summary.json](2026-11-04-burr-port-data/summary.json). The work is confined to
this worktree. The [evidence directory](2026-11-04-burr-port-data/README.md)
contains the executable reproducer, independent checks and compact run records.

## Implementation and guarantees

[certificate.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/burr/certificate.hpp)
is header-only C++17, with status returns, fixed-size matrix algebra and no R,
printing, exceptions, raw-pointer interfaces or direct Boost calls. The
certificate-only diagnostic programs also compile with `-fno-exceptions`,
without Boost or R include directories.
[mle.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/burr/mle.hpp)
routes `burr_mle` and the all-free held overload to the certified engine.
The [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/burr/README.md)
updates method, certification, existence, limit detection and decision history.

The engine ports the [root-box derivation](2026-11-04-burr-root-data/derivation.md):
bracketed, safeguarded conditional proposals; feasible primal values; repaired
entropy-dual weights with one weight defined exactly by the sum constraint;
three-score contraction boxes; uniform nuisance containment and negative profile
curvature on a larger neighborhood; chord envelopes; small-k exclusion; and
signed or unsigned Weibull tail bounds. The Weibull power root is bracketed,
and its bracket is propagated to W, A, the fitted exponentials and seam
constants. The signed inequality is relative to that same exact W.

The 41-point reconnaissance and its root refinements are proposals. The
reconnaissance endpoint is not an estimation cap. A finite answer requires a
verified root box, a verified curvature neighborhood, strict improvement over
both faces and strict exclusion of every remaining shape interval. A missed
competing root prevents closure. Failed brackets, domains, nonfinite arithmetic
and linear solves return failure; rejected optional root proposals cannot
become unchecked estimates. A local score rescue cannot bypass the certificate.

The global cache budget is 4096 profiles. Inner proposal solves use at most
`min(maxit,120)` iterations; bracket expansion, root refinement, neighborhood
trials and tail searches also have explicit finite budgets. The C++
`burr_mle_certified` overload accepts a final `certificate_budget` argument.
R keeps the 4096-profile default; raising `iterlim` controls proposal solves,
not that separate global budget. Positive historical starting controls and
legacy gradient/relative tolerances remain accepted and validated; the
mandatory certificate reconnaissance and enclosure widths are deterministic.
Returned all-free iteration counts report attempted cached conditional profiles.

The mathematical guarantee is **certified in exact arithmetic, conditional on
the computed comparisons, with a stated practical floating-point margin**.
Elementary interval operations use `64*epsilon*(1+max(abs(lo),abs(hi)))`;
n-term sums use `64*epsilon*(n+1)*(1+sum(input magnitudes))`, with
`epsilon=2^-52`. The practical libm assumption is absolute error at most
`8*epsilon*max(1,abs(result))` for the stated elementary functions.
Root contraction and curvature tests require a further 0.1 norm margin.
These are practical arithmetic assumptions. This port supplies neither a
universal libm theorem nor a new exact rational global certificate.
The existing exact rational golden-root proof remains linked from the dossier.

The Hessian is the analytic Jacobian of the total log-likelihood score, used
for optimization and curvature. It supplies no inference or coverage claim.
A successful finite certificate proves a unique global maximum for that sample;
a general joint Burr uniqueness theorem remains unproved.

## Outcomes, diagnostics and compatibility

| Original retained workloads | Count |
|---|---:|
| Certified finite maximum | 23 |
| Certified Pareto supremum | 15 |
| Certified Weibull supremum | 11 |
| Expected subdivision-budget stop | 1 |

The separate seam proxy returns `CertificateTie`. Its A enclosure is
`[-1.07666409122e-08, 1.07666338067e-08]`.
This is a sign-uncertain binary64 workload, not evidence of exact zero slope.
All budget/tie outputs contain NaN coefficients and log likelihood, verified
by the reproducer and native cases. Face outcomes identify unattained Burr
suprema and cause the existing R recommendations to fit Weibull or Pareto.

The public outcomes are `Ok`, `WeibullBoundary`, `ParetoBoundary`,
`CertificateTie`, `CertificateBudget`, and `NumericalFailure`, alongside the
existing invalid-input, unbounded and iteration-failure outcomes. The
[binding-neutral adapter](../../../bindings/common/distributions/burr/fit.hpp)
has explicit error branches for every new failure. The existing thin Rcpp
adapter forwards these errors unchanged; the new
[R tests](../../../bindings/r/distributions/burr/test.R) exercise exported
`mlburr` calls for tie, budget, numerical failure and constant-sample collapse.

Burr held subsets retain their algorithms. InvBurr, Pareto IV and GB2 seeds
explicitly use the retained `burr_mle_legacy` compatibility path, keeping their
previous fitting behavior. InvBurr also maps the new failure statuses to its
iteration failure and preserves the all-free held overload's invalid-control
guard. A native regression covers that guard. Pareto II, Pareto III and
paralogistic held dispatches do not reach the new all-free path and need no
change. Their golden, held-subset and oracle regressions passed in the full
suites. No golden, tolerance, settled d/p/q/r, special-function seam, truth
fixture or code-generation script was changed. No changes to those settled
components are needed by this port.

## Accuracy and coordinate decision

The appended native cases freeze all 50 original binary64 workloads plus the
tie proxy. Existing golden, reciprocal, oracle and held-subset cases stay in
place. Additional cases check invalid arithmetic domains, singular inversion,
proposal exhaustion, a zero profile budget, sign-uncertain tail rejection and
extreme changes of units.

The independent 80-digit check re-evaluates all 49 decided likelihoods, all 23
finite coefficient vectors and their three-coordinate total scores. Maximum
relative discrepancy from the retained high-precision stationary roots is
`4.036e-14`; maximum absolute likelihood discrepancy is
`3.7e-13`. Check thresholds are `1e-8` relative
coefficient discrepancy, `1e-8` absolute likelihood error and `1e-7` total-score
norm. These numerical checks corroborate the implementation; the certificate
logic supplies the conditional mathematical guarantee. The unchanged frozen
golden passes its existing `1e-6` coefficient tolerance.

Two initial coordinate choices were rejected after actual guarded stops.
Standardizing root coordinates made the heavy-tail neighborhood too small for
global value exclusion. Unshifted original logs then failed the existing
golden-times-1000 equivariance test for the same reason. Translating original
logs by the proposed log scale retains useful rectangular neighborhoods and
propagates the translation through the interval inputs. The reproducible
[coordinate comparison](2026-11-04-burr-port-data/coordinates.json) shows:

| Sample | Standardized k half-width | Unshifted-log half-width | Chosen translated-log half-width |
|---|---:|---:|---:|
| Golden times 1000 | 0.004004 | 0.000001335 | 0.004004 |
| burr-k0.08-r3 | 0.000002110 | 0.0002110 | 0.0002110 |

This improves enclosure usefulness while retaining every certification check.
The final native tests additionally certify the golden after rescaling by
`1e-100` and `1e100` and reversing the observation order. The earlier research
quadrature curvature, with its documented wrong signs, is never used.

## Speed

The final optimized comparison alternates the unchanged legacy fitter with
the new public fitter on each original workload. It discards one warmup and
uses ten repetitions, taking a within-workload median and then a median across
all 50 original workloads, including the expected budget stop.

| Workload | Contemporaneous legacy C++ | Certified C++ | Ratio |
|---|---:|---:|---:|
| Median of original 50 | 2.29 ms | 7.74 ms | 3.38 |
| Golden | 26.53 ms | 77.17 ms | 2.91 |

The brief's earlier legacy reference was 1.7 ms median and 21 ms golden.
The final run is slower for both implementations, so the contemporaneous
comparison is the fair ratio. Runtime remains in the same order, with a
roughly threefold certification cost. These are local timing snapshots from
a single CPU slot on a shared workstation, rather than a cross-machine bound.

The [component microbenchmark](2026-11-04-burr-port-data/components.json)
measures 24.02 ms for a 41-point golden batch of conditional proposal
solves, and 26.20 ms for those same solves plus primal/dual enclosures.
Repeated conditional intercept/power solves dominate this batch. Additional
profiles and root proposals account for most of the new work. This comparison
is not an instrumented decomposition of every full fit.

## Final validation

All final commands finished in the foreground on the **local workstation**
through the prescribed locks, with nice priority 10 and BLAS/OpenMP pinned to
one thread. No simbox, sub-agents, dependency installation, board edits, Git
writes or publication were used. The board was read at session start.

| Final command | Result |
|---|---|
| `/tmp/umvl-cpu/run4 just cpp-test` | 106/106 dev shards passed |
| `/tmp/umvl-cpu/run4 just cpp-test-no-long-double` | 127/127 passed |
| `/tmp/umvl-cpu/run4 just test` | FAIL 0, WARN 0, SKIP 153, PASS 20511 |
| `/tmp/umvl-cpu/run4 env UMVL_TEST_TIER=full just cpp-test` | 127/127 full shards passed |
| `/tmp/umvl-cpu/run4 just test-full` | FAIL 0, WARN 0, SKIP 0, PASS 43328 |
| `/tmp/umvl-cpu/run sh dev/research/evaluations/2026-11-04-burr-port-data/run.sh` | All 51 outcome checks, independent likelihood/score/root checks, coordinate comparisons and timing runs passed |
| `/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-04-burr-port-data/validation.py` | Final logs parsed and checked; environment and log hashes recorded |

The 153 dev skips are expected tier skips. There are no permitted test failures
in these final runs. The asserted budget and tie results test safe refusal;
they do not count as winning-face decisions. Compact results and environment
are retained in [validation.json](2026-11-04-burr-port-data/validation.json).
Full temporary build logs are under `build/scratch/` and are not needed by
any report link.

Environment: Linux 7.0.0-38 x86-64, glibc 2.43, GCC 15.2.0, R 4.6.1,
BH 1.90.0.1, Rcpp 1.1.2, testthat 3.3.2, Python 3.14.5 and mpmath 1.3.0.
Reproduction commands and input identities are in the evidence README.

## Open within the permitted guarded outcomes

The nearly constant 20-observation workload still exhausts 4096 profiles under
the practical input-log allowances. A definitive finite/face result for that
input requires tighter enclosures or stronger exclusion bounds; none is claimed.
The seam proxy stops because its slope enclosure contains zero and this search
has no strictly better certified finite witness. Resolving exact finite/face
ties or all degenerate seam slopes requires additional mathematics or tighter
arithmetic. These explicit stops are supported production outcomes.

Other compilers, operating systems and libm implementations have not been run
in this job. The floating-point guarantee retains its stated conditional
arithmetic contract. There is no remaining implementation or diagnostic
propagation gap for the delivered default within that contract.

## Ancillary checks and artifact display

The changed-document check passed all 32 local links, Python/JSON syntax,
recorded source hashes and whitespace; `git diff --check` passed. These checks
are recorded in [final-checks.json](2026-11-04-burr-port-data/final-checks.json).
The repository-wide link scanner reported five unresolved destinations in
unchanged documents: two old inference screenshot paths under ignored build
output, `dev/research/boundary-points.pdf`, and two `/inference/` site-route
links interpreted as local paths by that scanner. No changed-document link
failed. Those unrelated documents and targets were not edited.

The requested artifact-pane `show` command was attempted after the report was
written. It returned HTTP 403: this worktree is outside the viewer's configured
readable roots. The report remains at its requested worktree path. No viewer
configuration, main checkout, other worktree or external destination was changed.
