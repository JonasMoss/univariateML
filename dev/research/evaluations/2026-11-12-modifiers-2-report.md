# TASK-191: modifier replacements and retirement audit

All 16 retiring families now have modifier replacements. The three missing
routes are implemented, retiring-wrapper advice is rerouted, and the retirement
audit and final workstation suites have finished. No family, wrapper or existing
test was removed. No commit, board change, dependency installation or publication
was made.

Baseline: `fe17e3b83d52b73c2240e17dd285820f53d47a64`, branch
`codex/modifiers-2`. Work was confined to this worktree. Frozen goldens,
`dpqr.hpp`, `special/`, truth tables, code generators, NEWS, STATUS,
KNOWN_ISSUES and the roadmap were left unchanged. Evidence and reproduction
scripts are in [the retained data directory](2026-11-12-modifiers-2-data/).

## Replacement calls and coefficient maps

The following calls take data `x`. Numeric old coefficients remain held;
old `NULL` coefficients remain free. Explicit old free window bounds map to
`NA` modifier endpoints. `p0` and `pi` keep their meanings and names.

| Retiring wrapper | Replacement | Coefficient/design map |
|---|---|---|
| `mltbinom` | `mlbinom(x, truncate=c(L,U), size=...)` | `L=max(0,floor(a)+1)`, `U=floor(b)`; retain size and prob |
| `mltnorm` | `mlnorm(x, truncate=c(a,b))` | retain mean and sd; bounds become window attributes |
| `mltpois` | `mlpois(x, truncate=c(L,U))` | `L=max(0,floor(a)+1)`, `U=floor(b)`; retain lambda |
| `mlzipf` | `mlzeta(x, truncate=c(1,N))` | retain s; free N becomes upper endpoint `NA` |
| `mlzib` | `mlbinom(x, zero="inflated", size=...)` | retain size, prob, pi |
| `mlzinb` | `mlnbinom(x, zero="inflated")` | retain size, prob, pi |
| `mlzip` | `mlpois(x, zero="inflated")` | retain lambda, pi |
| `mlzmbinom` | `mlbinom(x, zero="modified", size=...)` | retain size, prob, p0 |
| `mlzmgeom` | `mlgeom(x, zero="modified")` | retain prob, p0 |
| `mlzmlogarithmic` | `mllgser(x, zero="modified")` | new theta = old prob; retain p0 |
| `mlzmnbinom` | `mlnbinom(x, zero="modified")` | retain size, prob, p0 |
| `mlzmpois` | `mlpois(x, zero="modified")` | retain lambda, p0 |
| `mlztbinom` | `mlbinom(x, zero="truncated", size=...)` | retain size, prob |
| `mlztgeom` | `mlgeom(x, zero="truncated")` | retain prob |
| `mlztnbinom` | `mlnbinom(x, zero="truncated")` | retain size, prob |
| `mlztpois` | `mlpois(x, zero="truncated")` | retain lambda |

For free-size binomial fits pass `size=NULL` explicitly. Do not clip the supplied
upper window endpoint to a trial count: doing so changes the free-size search.
Infinite old bounds remain infinite. Integer windows are closed; old tbinom and
tpois use `a < X <= b`, which explains the lower-endpoint conversion. Normal
endpoint inclusion has no mass effect. The generated
[reduction inventory](../../validation/family-reductions.csv) records the maps.

Data-free constructors have a separate default issue: old `mlzipf()` defaults to
`s=1`, while the unchanged zeta constructor defaults to `s=2`. To recreate the
old default law use `mlzeta(s=1, truncate=c(1,10))`. Of 104 omission-subset
constructor checks, precisely two expose this difference: s omitted, with N
either omitted or supplied. All other mapped defaults agree. Every individual
call, coefficient vector and reason is in
[constructor-defaults.json](2026-11-12-modifiers-2-data/constructor-defaults.json).

## Implementation and mathematical evidence

### Free-size binomial windows

The modifier delegates to the tbinom design fitter, preserving integer-size
profiling, retained-support certificates, truncated-Poisson face comparisons,
named nonexistence and escape outcomes, and successful uncertified finite
fallbacks. This reuses the analysis and sufficient certificates in the
[tbinom dossier](../../../univariateML-cpp/include/cppunivariate/distributions/tbinom/README.md);
it does not claim a new theorem of general global uniqueness.

For held size, the conditional likelihood is concave in binomial natural
probability, with score proportional to the observed mean minus conditional
mean and derivative minus conditional variance. The generic bracketed solver
handles the held-size route. Holding every parent coefficient requires only
conditional-density evaluation. Estimated bounds tighten to the sample
extremes, since removing unobserved mass improves the likelihood at every
parent coefficient.

Controls now reach integer search, probability bracketing, certificate work and
fallback coefficient checks. Budgets use the documented work units times
`min(iterlim,100000)`. Free-size windows retain the reference 512-unit default;
held-size windows retain 400. The original public common fitter signature and
the original five-argument core design entry point remain available. Explicit
controls take a separate overload, avoiding an accidental legacy-path dispatch
when `iterlim=512`.

The dispatch regression was real: at held size `1e308`, data `0:3`, the explicit
400, 511 and 513 routes succeeded while 512 selected the failed old route.
All four now succeed, with probability agreeing with `mean(x)/size` within
`1e-10` relative error. The untouched original default entry still gives its
reference failure. [Before](2026-11-12-modifiers-2-data/work-controls-before.json),
[after](2026-11-12-modifiers-2-data/work-controls.json) and
[fitted-law evaluation](2026-11-12-modifiers-2-data/extreme-window-evaluation.json)
record the result and its Poisson-limit density/CDF/quantile comparison.

The frozen default replay contains **12,965 tbinom records with zero differences**
in the recorded coefficients, status, stop kind and certification flag. This is
not a claim of a bitwise log-likelihood snapshot comparison; likelihoods have
the independent old/new replay and existing assertions described below. See
[default comparison](2026-11-12-modifiers-2-data/tbinom-default-comparison.json).
Both complete optimized tbinom doctest runs passed: 37 cases and 4,806 assertions
per arithmetic mode, in [the retained result](2026-11-12-modifiers-2-data/tbinom-doctests.json).

### Finite zeta windows on the full real domain

The new [finite-window header](../../../univariateML-cpp/include/cppunivariate/distributions/zeta/truncation.hpp)
implements effective support `1,...,N`, integer `1 <= N <= INT_MAX`, for every
finite real s. The widened domain is declared in zeta's modifiers metadata;
the unconditioned zeta domain is unchanged.

The log likelihood is `-s*sum(log(x)) - n*log(H_N(s))`. Its score is
`n*(E_s[log(X)]-mean(log(x)))`, with derivative `-n*Var_s(log(X))`.
For N greater than one this proves strict concavity and a unique interior
maximum when the empirical log mean lies strictly between the endpoint logs.
All-one data have a point-mass supremum as s tends to positive infinity;
all-N data have one as s tends to negative infinity. These are named limit
stops, not finite MLEs. At N=1 the exponent is unidentified; a representative
is returned and a held exponent is preserved. When N is estimated, the smallest
support containing the data maximizes likelihood at every fixed s, since the
normalizer increases with N.

Negative-exponent sums are centered at N, with compensated mass and log-moment
sums. Concentrated laws use descending ratios and geometric remainder bounds;
broad laws use Euler--Maclaurin through B8 with an eighth-derivative remainder
check. The finite-interval derivative bound remains finite just above exponent
seven. Large positive exponents use a compensated direct sum with integral tail
bounds and `log1p` for tiny excess normalizers. Existing nonnegative finite
harmonic kernels are otherwise reused. Evaluated cells consume the raised-by-
iterlim enumeration budget. Smaller log tails and stable complements support
extreme conditional probabilities; failed comparisons return numerical failure.

The final shape check requires opposite score signs around the candidate with
radius `tol/(1+tol)*max(1,abs(s))`, padding each moment comparison by
`128*epsilon` times the magnitudes of the two compared log means. This radius
implies the accuracy contract's `tol*max(1,abs(true_s))` bound when the signs are
correct. The proof is in exact arithmetic conditional on the computed moment
and remainder comparisons, with stated practical floating-point margins; these
are not outward-rounded intervals. An unresolved local coefficient tolerance
is a search-accuracy failure, distinct from an unresolved global tbinom face
certificate, for which the successful uncertified fallback remains intact.

Two numerical checks motivated corrections. Naive accumulation of the empirical
log mean gave a candidate about `-1.1414e-10` on a complete uniform sample of
four million points, whose exact MLE is zero; compensated accumulation gives
about `-4.5475e-13`. Separately, rounded score bisection returned three wrong
`Ok` fits when `tol=1e-20`. The final score-neighbourhood check removes those
wrong `Ok` results and preserves the default fits. Held s=0 still succeeds at
that tolerance. See [large-sample evidence](2026-11-12-modifiers-2-data/large-zeta-reference.json),
[precision before](2026-11-12-modifiers-2-data/zeta-accuracy-controls-before.json)
and [precision after](2026-11-12-modifiers-2-data/zeta-accuracy-controls.json).

The independent 80-digit check covers 105 density/log-tail evaluations and four
MLEs, up to `N=INT_MAX`, including negative, zero and positive exponents. Maximum
absolute log-density, log-CDF and log-survival errors on these rows are
`3.20e-13`, `8.93e-14` and `3.36e-11`, respectively. All four MLE statuses are
explicitly checked, so a finite candidate carried by a failed fit cannot pass
the oracle. An additional 288 negative-exponent quantile comparisons cover both
tails and natural/log probabilities, down to `1e-100`. These numerical studies
are [law/MLE references](2026-11-12-modifiers-2-data/zeta-reference.json) and
[quantile references](2026-11-12-modifiers-2-data/zeta-quantile-reference.json),
with their scripts beside them. Complete optimized zeta doctests pass in both
arithmetic modes: 12 cases, 16,174 assertions each.

### Logarithmic zero modification and binding corrections

`mllgser(..., zero="modified")` delegates to the zmlogarithmic reference fitter
with old prob mapped to theta. The declared exception includes theta zero,
the attained two-point law. It retains all held/free subsets of theta and p0,
the reference all-zero handling, stops and certification scope. No lgser
`dpqr.hpp` change was needed. The hurdle likelihood separates the zero atom's
Bernoulli term from the positive logarithmic likelihood.

The audit also corrected modifier plumbing: all-held zero fits use the reference
density-only path; held-parent windows avoid unnecessary optimization; normal
free bounds resolve to sample extremes; and R certification attributes match
the reference wrapper's all-held behavior. Proper infinite-endpoint windows
now construct the private conditional evaluator directly, in its declared
coefficient order, rather than failing the old finite-design metadata gate.
Settled conditional d/p/q/r kernels remain unchanged. Finite windows retain
their ordinary fixed-model validation.

The JS seam exposes the same three routes and controls. A conditional TypeScript
control type fixes the empty-parent-control intersection that previously rejected
legal lgser and Poisson modifier options. The strict fixture passes against the
final build; [typescript.json](2026-11-12-modifiers-2-data/typescript.json) keeps
the original diagnostics and successful reruns. R and JS regression cases were
added, and C++ cases were appended to the existing files.

## Retirement audit

[Audit summary](2026-11-12-modifiers-2-data/audit-summary.json),
[hold coverage](2026-11-12-modifiers-2-data/hold-coverage.csv) and
[outcome coverage](2026-11-12-modifiers-2-data/outcome-coverage.csv) record
**25,427 data-fit comparisons**, all 16 families, all **104/104** supported
hold subsets on frozen goldens, and 197 observed reference outcome groups.
Inputs comprise 104 golden/hold cases, 1,576 fit-audit archive replays,
10,619 captured original C++ calls, 986 data calls from retained R tests and
12,142 added domain-edge combinations. The archive is the requested
2026-11-07 `cases.json.gz`; its source labels and indices remain in each case.

The original 16 C++ test sources were read from baseline with `git show` and
instrumented to capture inputs without deleting assertions. All 16 passed;
see [native capture results](2026-11-12-modifiers-2-data/native-test-summary.json).
The retained original R capture ran 171 test blocks without failures, errors or
warnings; the later capture including the new advice case ran 172. Native-only
optimizer controls remain covered by native assertions and the focused control
tests; the R replay does not invent unsupported public control arguments.

Each replay compares mapped parent coefficients, log likelihood, success versus
stop, certification (`NULL`, false or true), and the actual condition stop kind.
Coefficient differences use `1e-10*max(1,abs(old))`; likelihood differences use
`1e-10*max(1,abs(old_logLik))`. Raw old/new messages and values are retained in
[results.json.gz](2026-11-12-modifiers-2-data/results.json.gz), with complete
[inputs](2026-11-12-modifiers-2-data/cases.json.gz). This finite audit covers
the archived/tested outcomes and explicit edges; it is not an empirical proof
for every possible sample.

| Family | Fit comparisons | Differences |
|---|---:|---:|
| tbinom | 12,976 | 45 |
| tnorm | 999 | 2 |
| tpois | 626 | 21 |
| zipf | 314 | 38 |
| zib | 2,278 | 0 |
| zinb | 1,716 | 0 |
| zip | 282 | 0 |
| zmbinom | 1,810 | 0 |
| zmgeom | 311 | 0 |
| zmlogarithmic | 312 | 0 |
| zmnbinom | 1,698 | 0 |
| zmpois | 251 | 0 |
| ztbinom | 1,330 | 0 |
| ztgeom | 71 | 0 |
| ztnbinom | 380 | 0 |
| ztpois | 73 | 0 |
| **Total** | **25,427** | **106** |

Every difference has a case-level reason in
[difference-reasons.csv](2026-11-12-modifiers-2-data/difference-reasons.csv),
and its differing fields in [differences.csv.gz](2026-11-12-modifiers-2-data/differences.csv.gz).
There are no unclassified differences or unexplained regressions.

| Reason | Rows | Interpretation |
|---|---:|---|
| Proper unbounded window accepted | 46 | Old all-held metadata required finite bounds; the new conditional law is proper. |
| Widened exponent domain | 24 | Negative held/fitted s is now legal; an upper-endpoint-only sample now has an infinite-exponent supremum. |
| Positive full-window extension | 16 | `[1,Inf]` shares the positive-truncation route, including its attained boundary extensions. |
| Zeta coefficient refinement | 7 | Independent 80-digit scores favor the new root; all seven are individually checked. |
| Closed-window rounding | 6 | The modifier floors a noninteger upper endpoint; old N required an integer. |
| Estimated-window semantics | 5 | Modifier NA means estimate the endpoint; old held NA was rejected, or a design argument was required. |
| Held normal density improvement | 1 | Direct density evaluation avoids failed Newton work when parent coefficients are held. |
| Held-size binomial improvement | 1 | The bracketed solver resolves size `1e308`; checked against the analytic probability. |

[Seven refined-root references](2026-11-12-modifiers-2-data/zeta-replay-reference.json)
support the coefficient changes. All 106 likelihood pairs are listed in
[changed-logLik.csv](2026-11-12-modifiers-2-data/changed-logLik.csv). The narrow
normal case (archive row 7709, replay case 372) is checked independently at
250 digits, with absolute likelihood error `6.10e-13`; see
[the reference](2026-11-12-modifiers-2-data/narrow-normal-reference.json).

A separate **1,375-case frozen-law replay** materializes old defaults before
mapping, then compares coefficients, statuses, certification and d/log-upper-p/q
probes. Its 36 differences are 30 proper unbounded windows, two closed upper
endpoint roundings and four negative-exponent domain extensions. All are
classified in [frozen-difference-reasons.csv](2026-11-12-modifiers-2-data/frozen-difference-reasons.csv),
with [summary](2026-11-12-modifiers-2-data/frozen-audit-summary.json) and
[full records](2026-11-12-modifiers-2-data/frozen-audit.json.gz). The additional
104 raw constructor-default calls and their two s-default differences are
reported separately above, rather than silently materialized away.

## Limit and stop advice

The common tbinom, zib, zinb, zmbinom, zmnbinom, ztbinom and ztnbinom adapters
now name configured base calls wherever their advice named a retiring wrapper.
For example, positive Poisson advice is `mlpois(x, zero = "truncated")`;
finite-window Poisson advice supplies the converted closed bounds. Actual
infinite bounds print `Inf` in executable R syntax. No runtime `use ml...`
message still names one of the 16 retiring wrappers. Matching R expectations
were updated for the changed text, with an added advice-evaluation regression;
existing fit/stop assertions were retained.

## Final validation

All runs below completed in the foreground on the final implementation. Local
computations used the slot wrappers and their thread/niceness controls. Simbox
was tried for each long suite and returned 75; the documented workstation
fallback was used. The workstation no-long-double run was also performed as
separately required. [final-validation.json](2026-11-12-modifiers-2-data/final-validation.json)
records machines, compiler, final source hashes and log hashes.

| Check | Machine | Final result |
|---|---|---|
| `just cpp-test` | workstation, GCC 15 | 108/108 |
| `just cpp-test-no-long-double` | workstation, GCC 15 | 135/135 |
| `just test` | workstation | FAIL 0, WARN 0, SKIP 154, PASS 28,844 |
| `just test-full` | workstation | FAIL 0, WARN 0, SKIP 0, PASS 57,362 |
| `just js-build` and `just js-test` | workstation | build successful; 626/626, no skips |
| Strict TypeScript modifier fixture | workstation | exit 0 after final JS build |
| `just site-check` | workstation | green; 99 C++ snippets compile and link |
| `just relations` | workstation | 157 records, zero failures; includes declared-only records |
| Changed tbinom and zeta optimized doctests | workstation | both standard and no-long-double modes green |
| `git diff --check` | workstation | clean |

The complete C++ and R suites were repeated after the final shared binding and
accuracy fixes; the table does not substitute earlier suite results. JS and
site checks likewise include the final shape-accuracy guard. Subsequent changes
are evidence/report generation only.

### Open entries outside the completed implementation/audit

1. **Second compiler unavailable.** Simbox repeatedly returned exit 75 and its
   SSH port was unreachable. The requested GCC 13 no-long-double run therefore
   remains unperformed. What is missing is a reachable simbox and that one
   compiler check; workstation GCC 15 no-long-double is green.
2. **Optimized baseline tail defect.** An additional GNU 15 `-O2` probe reproduces
   unchanged Burr/inverse-Burr deep-tail failures: `-Inf` instead of about
   `-230.25850929941012`. `-O0` gives the reference value. The original isolated
   optimized tests each fail one assertion; hashes verify the relevant baseline
   files are unchanged. The required CMake recipes pass in their existing build
   configuration, and the changed families additionally pass at `-O2`. What is
   missing is an authorized fix in the separately owned dpqr/special/compiler
   seam, not a modifier workaround. Reproducer and hashes:
   [optimized_baseline.py](2026-11-12-modifiers-2-data/optimized_baseline.py),
   [results](2026-11-12-modifiers-2-data/optimized-baseline.json).
3. **Existing repository links.** The optional whole-repository Markdown link
   final check reports 45 pre-existing findings, including missing historical figures,
   literature assets and signature-parser false positives. Every affected source
   is byte-identical to baseline. The earlier count was 47; the site checks
   regenerated two historical screenshots. Authored links are checked separately with no
   new broken destinations. What is missing is unrelated historical-document
   cleanup; no such files were edited. See
   [source-checks.json](2026-11-12-modifiers-2-data/source-checks.json).
4. **Artifact pane root restriction.** The requested `codex-math-web/bin/show`
   was run on this finished report and returned HTTP 403: this worktree is
   outside the viewer's configured readable roots. The Markdown report exists
   at the requested path and all its local links resolve. What is missing is
   viewer registration of this worktree; no files or configuration outside the
   authorized worktree were changed to bypass it.

## Removal list for the separate, approved retirement step

This is an inventory and dependency plan, not a deletion performed by this job.
The exact [removal manifest](2026-11-12-modifiers-2-data/removal.json) names
179 family-owned files, all 16 wrappers, 32 site routes, all 34 relation edges,
13 NEWS reference lines and 1,470 external reference lines for review. Generated
website source mirrors are excluded from the authoritative reference list.
Historical mentions in evaluation evidence are provenance, not automatic
deletion targets.

**Required ordering.** First extract the still-used numerical fitters,
certificates and conditional evaluation helpers into surviving private/base
locations, and replace the private R conditional-family metadata dependencies.
The new modifier routes deliberately still call those kernels. In particular,
tbinom's design fitter/accuracy helpers, tpois/tnorm conditional kernels, the
zero-family fitters/evaluators and Zipf moments/quantiles cannot simply disappear
with their public family folders. Preserve frozen fixtures and useful original
tests under the surviving families or shared modifier validation before deleting
their old locations. Only then remove public registry entries and folders.

Remove the 16 wrapper names in the replacement table from R exports/help and
generated adapters, JS/WASM public registrations/types, C++ public umbrella
exposure, family selection and site inventories. Update bootstrap, plotting,
model-selection and shared inference test registries where they enumerate family
IDs. Keep intentional modifier inference/composition refusals; deleting a family
does not authorize enabling an unsupported inference route. Regenerate the R
package and site instead of hand-editing generated outputs. The original tbinom
compatibility entry points may retire only after their callers/tests have moved.

The site uses generic distribution/explorer page templates. Remove each old
family's generated `/distributions/<id>/` and
`/distributions/<id>/explore/` routes, catalogue entries, relation data and
published source assets. Do not delete generic page templates. No compatibility
alias or redirect is proposed by this job.

Retarget relation edges to **configured base laws**, retaining coefficient maps,
holds, zero arguments and windows. A blind replacement of IDs would create
false self-relations. Examples needing care are the size-zero logarithmic limit
of zero-truncated nbinom, the shifted geometric relation, finite uniform zeta
windows and unbounded normal windows. The exact old edges follow below.

NEWS needs a new 2.0 migration entry naming all 16 replacements and the strict-to-
closed endpoint map. The current references are listed below for editorial
review; versioned history can remain historical. In particular, the old Zipf
uniform-boundary claim must not be mechanically renamed to zeta: the widened
domain permits negative s or an infinite-exponent supremum. NEWS was not edited.

### Exact family-owned file inventory

Rehome necessary kernels, fixtures and assertions before removing these paths.

```text
bindings/common/distributions/tbinom/fit.hpp
bindings/common/distributions/tnorm/fit.hpp
bindings/common/distributions/tpois/fit.hpp
bindings/common/distributions/zib/fit.hpp
bindings/common/distributions/zinb/fit.hpp
bindings/common/distributions/zip/fit.hpp
bindings/common/distributions/zipf/fit.hpp
bindings/common/distributions/zmbinom/fit.hpp
bindings/common/distributions/zmgeom/fit.hpp
bindings/common/distributions/zmlogarithmic/fit.hpp
bindings/common/distributions/zmnbinom/fit.hpp
bindings/common/distributions/zmpois/fit.hpp
bindings/common/distributions/ztbinom/fit.hpp
bindings/common/distributions/ztgeom/fit.hpp
bindings/common/distributions/ztnbinom/fit.hpp
bindings/common/distributions/ztpois/fit.hpp
bindings/r/distributions/tbinom/adapter.cpp
bindings/r/distributions/tbinom/test.R
bindings/r/distributions/tnorm/adapter.cpp
bindings/r/distributions/tnorm/test.R
bindings/r/distributions/tpois/adapter.cpp
bindings/r/distributions/tpois/test.R
bindings/r/distributions/zib/adapter.cpp
bindings/r/distributions/zib/test.R
bindings/r/distributions/zinb/adapter.cpp
bindings/r/distributions/zinb/test.R
bindings/r/distributions/zip/adapter.cpp
bindings/r/distributions/zip/test.R
bindings/r/distributions/zipf/adapter.cpp
bindings/r/distributions/zipf/test.R
bindings/r/distributions/zmbinom/adapter.cpp
bindings/r/distributions/zmbinom/test.R
bindings/r/distributions/zmgeom/adapter.cpp
bindings/r/distributions/zmgeom/test.R
bindings/r/distributions/zmlogarithmic/adapter.cpp
bindings/r/distributions/zmlogarithmic/test.R
bindings/r/distributions/zmnbinom/adapter.cpp
bindings/r/distributions/zmnbinom/test.R
bindings/r/distributions/zmpois/adapter.cpp
bindings/r/distributions/zmpois/test.R
bindings/r/distributions/ztbinom/adapter.cpp
bindings/r/distributions/ztbinom/test.R
bindings/r/distributions/ztgeom/adapter.cpp
bindings/r/distributions/ztgeom/test.R
bindings/r/distributions/ztnbinom/adapter.cpp
bindings/r/distributions/ztnbinom/test.R
bindings/r/distributions/ztpois/adapter.cpp
bindings/r/distributions/ztpois/test.R
univariateML-cpp/include/cppunivariate/distributions/tbinom/README.md
univariateML-cpp/include/cppunivariate/distributions/tbinom/accuracy.hpp
univariateML-cpp/include/cppunivariate/distributions/tbinom/dpqr.hpp
univariateML-cpp/include/cppunivariate/distributions/tbinom/face_certificate.hpp
univariateML-cpp/include/cppunivariate/distributions/tbinom/golden.json
univariateML-cpp/include/cppunivariate/distributions/tbinom/mle.hpp
univariateML-cpp/include/cppunivariate/distributions/tbinom/spec.py
univariateML-cpp/include/cppunivariate/distributions/tbinom/test.cpp
univariateML-cpp/include/cppunivariate/distributions/tbinom/truth.json
univariateML-cpp/include/cppunivariate/distributions/tbinom/truth.py
univariateML-cpp/include/cppunivariate/distributions/tnorm/README.md
univariateML-cpp/include/cppunivariate/distributions/tnorm/dpqr.hpp
univariateML-cpp/include/cppunivariate/distributions/tnorm/golden.json
univariateML-cpp/include/cppunivariate/distributions/tnorm/mle.hpp
univariateML-cpp/include/cppunivariate/distributions/tnorm/test.cpp
univariateML-cpp/include/cppunivariate/distributions/tnorm/truth.json
univariateML-cpp/include/cppunivariate/distributions/tnorm/truth.py
univariateML-cpp/include/cppunivariate/distributions/tpois/README.md
univariateML-cpp/include/cppunivariate/distributions/tpois/dpqr.hpp
univariateML-cpp/include/cppunivariate/distributions/tpois/dpqr_core.hpp
univariateML-cpp/include/cppunivariate/distributions/tpois/golden.json
univariateML-cpp/include/cppunivariate/distributions/tpois/mle.hpp
univariateML-cpp/include/cppunivariate/distributions/tpois/profile.hpp
univariateML-cpp/include/cppunivariate/distributions/tpois/spec.py
univariateML-cpp/include/cppunivariate/distributions/tpois/test.cpp
univariateML-cpp/include/cppunivariate/distributions/tpois/truth.json
univariateML-cpp/include/cppunivariate/distributions/tpois/truth.py
univariateML-cpp/include/cppunivariate/distributions/zib/README.md
univariateML-cpp/include/cppunivariate/distributions/zib/dpqr.hpp
univariateML-cpp/include/cppunivariate/distributions/zib/face_certificate.hpp
univariateML-cpp/include/cppunivariate/distributions/zib/golden.json
univariateML-cpp/include/cppunivariate/distributions/zib/mle.hpp
univariateML-cpp/include/cppunivariate/distributions/zib/test.cpp
univariateML-cpp/include/cppunivariate/distributions/zib/truth.json
univariateML-cpp/include/cppunivariate/distributions/zib/truth.py
univariateML-cpp/include/cppunivariate/distributions/zinb/README.md
univariateML-cpp/include/cppunivariate/distributions/zinb/certificate.hpp
univariateML-cpp/include/cppunivariate/distributions/zinb/dpqr.hpp
univariateML-cpp/include/cppunivariate/distributions/zinb/global_certificate.hpp
univariateML-cpp/include/cppunivariate/distributions/zinb/golden.json
univariateML-cpp/include/cppunivariate/distributions/zinb/mle.hpp
univariateML-cpp/include/cppunivariate/distributions/zinb/test.cpp
univariateML-cpp/include/cppunivariate/distributions/zinb/truth.json
univariateML-cpp/include/cppunivariate/distributions/zinb/truth.py
univariateML-cpp/include/cppunivariate/distributions/zip/README.md
univariateML-cpp/include/cppunivariate/distributions/zip/certificate.hpp
univariateML-cpp/include/cppunivariate/distributions/zip/dpqr.hpp
univariateML-cpp/include/cppunivariate/distributions/zip/golden.json
univariateML-cpp/include/cppunivariate/distributions/zip/mle.hpp
univariateML-cpp/include/cppunivariate/distributions/zip/test.cpp
univariateML-cpp/include/cppunivariate/distributions/zip/truth.json
univariateML-cpp/include/cppunivariate/distributions/zip/truth.py
univariateML-cpp/include/cppunivariate/distributions/zipf/README.md
univariateML-cpp/include/cppunivariate/distributions/zipf/dpqr.hpp
univariateML-cpp/include/cppunivariate/distributions/zipf/golden.json
univariateML-cpp/include/cppunivariate/distributions/zipf/mle.hpp
univariateML-cpp/include/cppunivariate/distributions/zipf/reference.json
univariateML-cpp/include/cppunivariate/distributions/zipf/test.cpp
univariateML-cpp/include/cppunivariate/distributions/zipf/truth.json
univariateML-cpp/include/cppunivariate/distributions/zipf/truth.py
univariateML-cpp/include/cppunivariate/distributions/zmbinom/README.md
univariateML-cpp/include/cppunivariate/distributions/zmbinom/dpqr.hpp
univariateML-cpp/include/cppunivariate/distributions/zmbinom/golden.json
univariateML-cpp/include/cppunivariate/distributions/zmbinom/mle.hpp
univariateML-cpp/include/cppunivariate/distributions/zmbinom/test.cpp
univariateML-cpp/include/cppunivariate/distributions/zmbinom/truth.json
univariateML-cpp/include/cppunivariate/distributions/zmbinom/truth.py
univariateML-cpp/include/cppunivariate/distributions/zmgeom/README.md
univariateML-cpp/include/cppunivariate/distributions/zmgeom/dpqr.hpp
univariateML-cpp/include/cppunivariate/distributions/zmgeom/golden.json
univariateML-cpp/include/cppunivariate/distributions/zmgeom/mle.hpp
univariateML-cpp/include/cppunivariate/distributions/zmgeom/test.cpp
univariateML-cpp/include/cppunivariate/distributions/zmgeom/truth.json
univariateML-cpp/include/cppunivariate/distributions/zmgeom/truth.py
univariateML-cpp/include/cppunivariate/distributions/zmlogarithmic/README.md
univariateML-cpp/include/cppunivariate/distributions/zmlogarithmic/dpqr.hpp
univariateML-cpp/include/cppunivariate/distributions/zmlogarithmic/golden.json
univariateML-cpp/include/cppunivariate/distributions/zmlogarithmic/mle.hpp
univariateML-cpp/include/cppunivariate/distributions/zmlogarithmic/test.cpp
univariateML-cpp/include/cppunivariate/distributions/zmlogarithmic/truth.json
univariateML-cpp/include/cppunivariate/distributions/zmlogarithmic/truth.py
univariateML-cpp/include/cppunivariate/distributions/zmnbinom/README.md
univariateML-cpp/include/cppunivariate/distributions/zmnbinom/dpqr.hpp
univariateML-cpp/include/cppunivariate/distributions/zmnbinom/golden.json
univariateML-cpp/include/cppunivariate/distributions/zmnbinom/mle.hpp
univariateML-cpp/include/cppunivariate/distributions/zmnbinom/test.cpp
univariateML-cpp/include/cppunivariate/distributions/zmnbinom/truth.json
univariateML-cpp/include/cppunivariate/distributions/zmnbinom/truth.py
univariateML-cpp/include/cppunivariate/distributions/zmpois/README.md
univariateML-cpp/include/cppunivariate/distributions/zmpois/dpqr.hpp
univariateML-cpp/include/cppunivariate/distributions/zmpois/golden.json
univariateML-cpp/include/cppunivariate/distributions/zmpois/mle.hpp
univariateML-cpp/include/cppunivariate/distributions/zmpois/profile.hpp
univariateML-cpp/include/cppunivariate/distributions/zmpois/test.cpp
univariateML-cpp/include/cppunivariate/distributions/zmpois/truth.json
univariateML-cpp/include/cppunivariate/distributions/zmpois/truth.py
univariateML-cpp/include/cppunivariate/distributions/ztbinom/README.md
univariateML-cpp/include/cppunivariate/distributions/ztbinom/accuracy.hpp
univariateML-cpp/include/cppunivariate/distributions/ztbinom/certificate.hpp
univariateML-cpp/include/cppunivariate/distributions/ztbinom/dpqr.hpp
univariateML-cpp/include/cppunivariate/distributions/ztbinom/face_certificate.hpp
univariateML-cpp/include/cppunivariate/distributions/ztbinom/golden.json
univariateML-cpp/include/cppunivariate/distributions/ztbinom/mle.hpp
univariateML-cpp/include/cppunivariate/distributions/ztbinom/test.cpp
univariateML-cpp/include/cppunivariate/distributions/ztbinom/truth.json
univariateML-cpp/include/cppunivariate/distributions/ztbinom/truth.py
univariateML-cpp/include/cppunivariate/distributions/ztgeom/README.md
univariateML-cpp/include/cppunivariate/distributions/ztgeom/dpqr.hpp
univariateML-cpp/include/cppunivariate/distributions/ztgeom/golden.json
univariateML-cpp/include/cppunivariate/distributions/ztgeom/mle.hpp
univariateML-cpp/include/cppunivariate/distributions/ztgeom/test.cpp
univariateML-cpp/include/cppunivariate/distributions/ztgeom/truth.json
univariateML-cpp/include/cppunivariate/distributions/ztgeom/truth.py
univariateML-cpp/include/cppunivariate/distributions/ztnbinom/README.md
univariateML-cpp/include/cppunivariate/distributions/ztnbinom/accuracy.hpp
univariateML-cpp/include/cppunivariate/distributions/ztnbinom/certificate.hpp
univariateML-cpp/include/cppunivariate/distributions/ztnbinom/dpqr.hpp
univariateML-cpp/include/cppunivariate/distributions/ztnbinom/golden.json
univariateML-cpp/include/cppunivariate/distributions/ztnbinom/mle.hpp
univariateML-cpp/include/cppunivariate/distributions/ztnbinom/test.cpp
univariateML-cpp/include/cppunivariate/distributions/ztnbinom/truth.json
univariateML-cpp/include/cppunivariate/distributions/ztnbinom/truth.py
univariateML-cpp/include/cppunivariate/distributions/ztpois/README.md
univariateML-cpp/include/cppunivariate/distributions/ztpois/dpqr.hpp
univariateML-cpp/include/cppunivariate/distributions/ztpois/dpqr_core.hpp
univariateML-cpp/include/cppunivariate/distributions/ztpois/golden.json
univariateML-cpp/include/cppunivariate/distributions/ztpois/mle.hpp
univariateML-cpp/include/cppunivariate/distributions/ztpois/profile.hpp
univariateML-cpp/include/cppunivariate/distributions/ztpois/test.cpp
univariateML-cpp/include/cppunivariate/distributions/ztpois/truth.json
univariateML-cpp/include/cppunivariate/distributions/ztpois/truth.py
```

### Exact relation-edge inventory

| Source | Relation | Target | Original mapping and conditions |
|---|---|---|---|
| nbinom | special_case_of | zinb | `{"at": {"pi": 0, "prob": "prob", "size": "size"}}` |
| tpois | truncation_of | pois | `{"at": {"lambda": "lambda"}}` |
| zib | modification_of | binom | `{"at": {"prob": "prob", "size": "size"}, "mass": "pi", "type": "zero-inflated"}` |
| binom | special_case_of | zib | `{"at": {"pi": 0, "prob": "prob", "size": "size"}}` |
| binom | special_case_of | tbinom | `{"at": {"a": -1, "b": "size", "prob": "prob", "size": "size"}}` |
| lgser | special_case_of | zmlogarithmic | `{"at": {"p0": 0, "prob": "theta"}}` |
| lgser | special_case_of | ztnbinom | `{"at": {"prob": "1-theta", "size": 0}}` |
| ztbinom | modification_of | binom | `{"at": {"prob": "prob", "size": "size"}, "type": "zero-truncated"}` |
| ztbinom | special_case_of | zmbinom | `{"at": {"p0": 0, "prob": "prob", "size": "size"}}` |
| ztgeom | modification_of | geom | `{"at": {"prob": "prob"}, "type": "zero-truncated"}` |
| ztgeom | special_case_of | ztnbinom | `{"at": {"prob": "prob", "size": 1}}` |
| ztgeom | special_case_of | zmgeom | `{"at": {"p0": 0, "prob": "prob"}}` |
| ztgeom | transform_of | geom | `{"at": {"prob": "prob"}, "shift": 1, "transform": "shift"}` |
| ztgeom | truncation_of | dlaplace | `{"at": {"location": 1, "scale": "1-prob"}, "when": "prob < 1"}` |
| tbinom | truncation_of | binom | `{"at": {"prob": "prob", "size": "size"}}` |
| zinb | modification_of | nbinom | `{"at": {"prob": "prob", "size": "size"}, "mass": "pi", "type": "zero-inflated"}` |
| zinb | special_case_of | zmnbinom | `{"at": {"p0": "pi+(1-pi)*prob**size", "prob": "prob", "size": "size"}}` |
| ztnbinom | modification_of | nbinom | `{"at": {"prob": "prob", "size": "size"}, "type": "zero-truncated"}` |
| ztnbinom | special_case_of | zmnbinom | `{"at": {"p0": 0, "prob": "prob", "size": "size"}}` |
| tnorm | truncation_of | norm | `{"at": {"mean": "mean", "sd": "sd"}}` |
| dunif | transform_of | zipf | `{"at": {"N": "max-min+1", "s": 0}, "shift": "min-1", "transform": "shift"}` |
| geom | special_case_of | zmgeom | `{"at": {"p0": "prob", "prob": "prob"}}` |
| norm | special_case_of | tnorm | `{"at": {"a": "-Inf", "b": "Inf", "mean": "mean", "sd": "sd"}}` |
| zmbinom | modification_of | binom | `{"at": {"prob": "prob", "size": "size"}, "mass": "p0", "type": "zero-modified"}` |
| zipf | truncation_of | zeta | `{"at": {"s": "s"}, "when": "s > 1"}` |
| zmgeom | modification_of | geom | `{"at": {"prob": "prob"}, "mass": "p0", "type": "zero-modified"}` |
| zmgeom | special_case_of | zmnbinom | `{"at": {"p0": "p0", "prob": "prob", "size": 1}}` |
| ztpois | modification_of | pois | `{"at": {"lambda": "lambda"}, "type": "zero-truncated"}` |
| ztpois | special_case_of | zmpois | `{"at": {"lambda": "lambda", "p0": 0}}` |
| zmlogarithmic | modification_of | lgser | `{"at": {"theta": "prob"}, "mass": "p0", "type": "zero-modified"}` |
| zmlogarithmic | special_case_of | zmnbinom | `{"at": {"p0": "p0", "prob": "1-prob", "size": 0}}` |
| zmnbinom | modification_of | nbinom | `{"at": {"prob": "prob", "size": "size"}, "mass": "p0", "type": "zero-modified"}` |
| zmpois | modification_of | pois | `{"at": {"lambda": "lambda"}, "mass": "p0", "type": "zero-modified"}` |
| zip | modification_of | pois | `{"at": {"lambda": "lambda"}, "mass": "pi", "type": "zero-inflated"}` |

### NEWS reference lines for the release editor

All lines are in `bindings/r/NEWS.md` at the audited baseline. Review the whole associated bullet, not just the matched line.

| Line | Existing reference |
|---:|---|
| 31 | * Design quantities and thresholds have no default. `mlbinom`, `mlzib`, |
| 32 | `mlzmbinom` and `mlztbinom` require `size`; `mltnorm` requires `a` and |
| 58 | - `mltnorm(x, a = NULL, b = NULL)` estimates the bounds as the sample |
| 63 | nbinom and zinb (`size = 0`), pig (`shape = 0`), cmp (`lambda = 0`), |
| 72 | `mltbinom(x, a = 0, b = 10, size = NULL)`. Each family's README lists |
| 113 | `mlzinb()` (zero-inflated negative binomial), `mlgpd()` (generalized |
| 125 | reciprocal), `mltnorm()` (truncated normal with known bounds), |
| 175 | * `mltpois()` and `mltbinom()` (Poisson and binomial truncated to known |
| 176 | bounds `a < x <= b`; `mltbinom` with known `size`) and `mldlaplace()` |
| 226 | * Zero-truncated `mlztpois()`, `mlztgeom()`, `mlztnbinom()`, `mlztbinom()`; |
| 227 | zero-modified `mlzmpois()`, `mlzmgeom()`, `mlzmnbinom()`, `mlzmbinom()`, |
| 228 | `mlzmlogarithmic()`; zero-inflated binomial `mlzib()`. |
| 273 | `mlzipf()` returns the attained uniform member `s = 0` instead of an |

### Reproduction

Run from the repository root, setting `TMPDIR`, `JUST_TEMPDIR` and `XDG_RUNTIME_DIR` to `build/tmp` first. Use the slot wrappers; the replay shell script does not acquire a second slot itself.

```sh
/tmp/umvl-cpu/run sh dev/research/evaluations/2026-11-12-modifiers-2-data/replay.sh
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-12-modifiers-2-data/coverage.py
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-12-modifiers-2-data/zeta_reference.py
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-12-modifiers-2-data/zeta_quantile_reference.py
```

The other named evidence scripts regenerate their matching JSON records. The capture script obtains original C++ test sources with `git show fe17e3b8:<path>`; no baseline source tree archive is kept. Deterministically compressed replay inputs and before/after records are retained, and the reader helpers accept either unpacked JSON or the stored gzip files. All 16 old families remain available for the separately approved removal step.
