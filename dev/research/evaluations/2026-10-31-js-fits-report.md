# All-family shared fitting: TASK-124.4 part 2, 2026-10-31

Complete: all remaining 95 families migrated; all 98 families now expose JS
fitting through shared headers. No blocked families or unresolved fit-policy items.
Source: `codex/js-fits`, base `40054fecc469421d73bc85df48a78347963a6fcd`,
plus the uncommitted changes. No commits, merges/rebases, board edits, installs,
simbox runs or writes to another worktree. Numerical core and canonical goldens unchanged.
Final 210-file source-manifest SHA256 (excluding this report):
`e542c4ee55028f641d30f006a8e35366b501bef98c7e9a0222cfde6a267ee165`.
Manifest: `build/scratch/source-manifest.sha256`; detailed progress: `build/scratch/js-fits-progress.md`.

## Migration coverage

Alphabetical batches below; exp, gamma and gb2 were the three existing pilots.
Each batch's original R family test files passed unchanged, followed by an
aggregate wasm build and batch Node checks. Initial strict wasm coefficient
failures were resolved only through the explicitly approved test policies below.

| Batch | Families migrated | R assertions |
|---|---|---:|
| 01 | bbinom beta betapr bhatt binom bnbinom burr cauchy cmb cmp dgamma dlaplace dnorm dunif dweibull | 3190 |
| 02 | emg fatigue foldnorm fpareto frechet ged genbeta geom gev gompertz gpd gumbel hcauchy hnorm ht | 2965 |
| 03 | huber hyper invburr invexp invgamma invgauss invparalogis invpareto invtrgamma invweibull kumar laplace lgamma lgser llogis | 2654 |
| 04 | lnorm logis logitnorm lomax lst naka nbinom nhyper norm nsbeta paralogis pareto pareto2 pareto3 pareto4 | 3344 |
| 05 | pig pois power rayleigh sged sgomp skellam slash sn snorm sstd std tbinom tlambda tnorm | 4238 |
| 06 | tpois trgamma triang unif weibull yulesimon zeta zib zinb zip zipf zmbinom zmgeom zmlogarithmic zmnbinom | 2784 |
| 07 | zmpois ztbinom ztgeom ztnbinom ztpois | 343 |

Shared headers retain the original validation, core calls, status mappings,
score gates, diagnostics and warnings. All 95 R fitter signatures were compared
with their original adapters and are identical, including defaults. Reference-only
R exports remain in their adapters. Failed solves stop unless their original
adapter already allowed recovery through a passing score gate; no new unchecked
iterate, cap or mathematical certification was introduced.

`FitResult` gained warning strings, named restriction attributes, scalar/vector
extra fields, and presence flags for optional iterations/fixed/restricted fields.
`Held` gained source length for Huber's scalar-vector validation. Integer inputs
use `ispan`, with JS checking finite, integral, int32-representable values.
The R converter emits warnings before errors and preserves optional fields.
A shared stream formatter retains formatted diagnostics without host printing.
Generation discovers headers, validates held order and R/shared control defaults,
and emits all-family bindings, coverage, golden cases and TypeScript fit types
without family-specific generator branches. Explorer ranking considers every
admissible family; parameter-dependent support is checked by its fitter.

## Approved wasm comparison policies and numerical evidence

Claude (orchestrating the job) decided on 2026-10-31 and approved:
Bhattacharjee wasm relative coefficient tolerance `5e-7`; Burr wasm Approx
epsilon `1e-7`; and a new Pareto IV default-golden test, subsequently given its
own wasm Approx epsilon `5e-7`. Constants are declared in C++ test support and
read generically by JS generation. Approx retains scale 1. Native tolerances
are unchanged (Burr/Pareto IV `1e-8`; Bhattacharjee's existing platform rules),
and every likelihood tolerance is retained. Default estimates/controls are unchanged.

Independent 60-digit analytic calculations evaluate total log likelihood and
Euclidean total score in the declared free coefficients; Pareto IV's threshold
is held at zero. Delta is recomputed wasm minus recomputed golden likelihood.

| Family | Delta log likelihood | wasm score norm | golden score norm |
|---|---:|---:|---:|
| bhatt | +1.45560927417454e-14 | 1.56622339473297e-6 | 1.99424305524782e-6 |
| burr | +3.13313503641955e-13 | 5.70471605595033e-6 | 9.72554108610607e-6 |
| pareto4 | -1.07220946249447e-12 | 3.03308890866322e-5 | 3.78637041844022e-5 |

Maximum absolute reported-vs-recomputed wasm likelihood error: `2.57e-13`.
All scores are below `1e-3`. Claude accepted these as numerically equally good
points on flat likelihood ridges, rather than regressions. This is numerical
stationarity/likelihood evidence, not an existence or global-optimality proof.
Safe musl/JS-math optimization variants did not satisfy all initial comparisons;
normal musl/O2 remains selected. Reproducer and frozen outputs are durable:
[analytic check](../../../bindings/js/validation/check-wasm-scores.py),
[captured outputs](../../../bindings/js/validation/wasm-golden-results.json).
Run `python3 bindings/js/validation/check-wasm-scores.py` using existing mpmath.

## Module layout decision

Matched-source O2 builds: aggregate versus all 98 single-family modules;
each module includes its fitter and d/p/q/r. Sizes include glue where stated.
Five Node import/instantiation trials per module reuse process/OS/engine caches;
they are not network-transfer or cold-browser benchmarks.

| Layout | wasm B | glue B | combined gzip B | first / median load ms |
|---|---:|---:|---:|---:|
| Aggregate, all 98 | 1846670 | 31478 | 567804 | 24.42 / 23.27 |
| 98 separate modules, summed | 13249141 | 2980229 | 6690625 | not aggregated |
| exp only, smallest single | 33358 | 29343 | 26817 | 4.49 / 5.62 |

Retain one aggregate: ranking uses all families, with one cached import and
much less total transfer than 98 modules. Single-family initial transfer would
be smaller with separate modules; that tradeoff is explicit. Reproduce with
`/tmp/umvl-cpu/run4 bash bindings/js/measure.sh`; full trials are in
`build/scratch/module-layout.json`. Final wasm/glue match the audited site bytes exactly.

## Final validation and remaining external item

Local foreground runs used the CPU slot locks, nice 10, BLAS/OpenMP one thread,
worktree temp/cache paths, and four slots for heavy builds/full suites.
Environment: GCC 15.2.0, R 4.6.1, Node 22.22.1, Emscripten 6.0.13, C++17.

- `just cpp-test`: 6/6 targets; distribution_tests 927 cases, all passing, rerun after both approvals.
- `NOT_CRAN=true just test`: FAIL 0, WARN 0, SKIP 1, PASS 39040; optional `{sn}` absent.
- `just js-build`: PASS; final module byte-identical to the successful browser audit's module.
- `just js-test`: 400/400 PASS, zero failures/skips; 98 golden/held-subset/diagnostic groups plus existing tests.
- Truth/sampler audit: 225908 scalar comparisons, no failures; 20000 draws/point, seed 20261029 + point.
- `just site-check`: PASS; generated TypeScript declarations also passed `tsc --noEmit`.
- Chromium audit: 98/98 explorers PASS, n=30/seed=123; 88 fits and 10 valid limit/non-existence diagnostics.
- Pasted-data ranking: all 98 considered, 31 AIC results; remaining rows retain diagnostics, no unavailable fitters.
- 60-digit reproducer and `git diff --check`: PASS. Logs/results: `build/scratch/final-*`, `js-final-*`, `browser-*.json`.

R fitting logic did not change after its full pass; subsequent shared-header cleanup removed trailing whitespace only.
Regeneration still prints roxygen link diagnostics from `bindings/r/scripts/codegen_wrappers.R`;
that settled generator belongs to the parallel small-fixes job and was left unchanged.
Artifact-pane preview returned HTTP 403: this worktree is outside the viewer's
configured readable roots. Viewer configuration is outside this task's writable scope.
