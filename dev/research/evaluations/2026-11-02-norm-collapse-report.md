# norm-collapse: exact nonexistence checks and limit records

Worktree `codex/norm-collapse`, based on
`34caf07d52d35aa15166d4eb9252a5453904e85f`, with the uncommitted changes
listed below. Work and validation ran locally on 2026-11-01–02
(Europe/Oslo). No commits, publishing, installs, sub-agents, board edits or
other-worktree edits were made.

The requested fitter changes, limit inventory, mathematical documentation,
C++ tests, R tests and conformance expectations are implemented. All numerical
and interface suites passed. **The final validation set is not fully green:**
`just site-check` fails on four rendered pages belonging to two unchanged,
out-of-scope families. The precise cause, baseline evidence and required fix
are recorded under Open below. No failure is counted as a pass.

## Implemented behavior

| Family/path | Result |
|---|---|
| tnorm, free scale, constant observations at the projection of a held mean onto the interval | Exact data comparison returns `DegenerateData` before scale search; binding reports unbounded likelihood as `sd -> 0`. Includes held means equal to an endpoint, outside either endpoint, and untruncated/one-sided supports. |
| tnorm, free mean and scale, constant observations | Exact constant-data check returns `DegenerateData` before search. |
| tnorm, held positive scale and free mean, observations all at one finite endpoint | New `UnboundedLocation` status before location search; binding reports unbounded likelihood and parent-mean escape beyond that endpoint. Existing enum values are preserved by appending the status. |
| tnorm, profiled free bounds | These checks use the profiled sample-extreme bounds. Coincident bounds remain `InvalidBounds`; both-held parent coefficients still evaluate the likelihood on a nonempty interval. |
| snorm, fully free constant finite sample | Returns `Unbounded`, including a singleton, rather than `InvalidStart`. Empty/nonfinite data remain invalid; nonconstant profile calculations are unchanged. |
| foldnorm, lgamma, llogis | Existing collapse behavior is unchanged. New proved limit records match their existing statuses and diagnostics. |

The shared tnorm binding supplies the corrected messages to R and JS.
No R adapter signatures or defaults changed. No d/p/q/r, special-function,
truth-table, golden or code-generator source changed.

Six C++ cases were appended at the ends of the five family test files.
Six new R test blocks cover exact collapse statuses/messages and compatible
held-parameter exceptions. The conformance file checks the five families'
proved collapse records and tnorm's held-scale location-escape record.

Cheap controls include a nonconstant tnorm sample, interior constant data with
scale held, both-held parent coefficients, a constant sample away from the
projected held mean, and held-coefficient exceptions for the other families.
The existing C++ and R golden regressions passed unchanged. Full-tier held
fits and independent numerical comparisons also passed; these are finite
regression/optimizer comparisons, not proofs for arbitrary samples.

## Mathematical basis and inventory

The new checks are exact comparisons of finite input observations; they do
not use a variance threshold, parameter cap or numerical search.

For tnorm, fix a nonempty interval and let every observation equal its
closest support point to a held mean. At an interior mean the density grows
as `1/sigma`; at an endpoint equal to the mean it is asymptotic to
`2/(sqrt(2*pi)*sigma)`. If the lower endpoint is `a > mu`, put `d = a-mu`.
Dividing numerator and normalizing integral by the endpoint exponent gives

```text
f_sigma(a) = 1 / integral_0^(b-a) exp(-(2*d*u+u^2)/(2*sigma^2)) du
           ~ d/sigma^2,  sigma -> 0.
```

With positive scale held and `mu -> -infinity`, the same integral is
asymptotic to `sigma^2/(a-mu)`, so the endpoint density diverges.
Reflection proves the upper-endpoint statements. The opposite endpoint may
be infinite. Profiling a free bound first gives the same argument unless
the bounds coincide, which does not define a continuous density.

For snorm, the normal member with mean at the common observation proves
unboundedness as scale vanishes. For foldnorm, the normal component at
`x0 = mu > 0` alone equals `1/(sqrt(2*pi)*sigma)`; at zero the density is
twice this. For lgamma, set `y0 = log(x0) > 0`, shape `k` and rate `k/y0`.
Stirling's formula gives density at `x0` asymptotic to
`sqrt(k/(2*pi))/(x0*y0)`. For llogis, scale `x0` gives density
`shape/(4*x0)`. These arguments prove divergence; the tests verify detection
and binding behavior rather than proving the asymptotics.

Five records were added, all with `firmness: proved`:

- tnorm scale collapse: `DegenerateData`.
- tnorm endpoint location escape, `fixed: [sd]`: `UnboundedLocation`.
- foldnorm scale collapse: `Unbounded`.
- lgamma joint shape/rate collapse: `Degenerate`.
- llogis all-free collapse: `Unbounded`; the existing `[scale]` record remains.

Snorm's existing proved `Unbounded` record now agrees with the fully free
implementation. Collapse records are marked `kind: nonexistence` where added
or aligned. `holds:` remains structural and unchanged. The final site data
reports 281 limit records across 99 families.

Canonical arguments and changed behavior are retained in the five dossiers.
The tnorm fit note now distinguishes exact collapse tests from quadrature,
Newton termination and tolerant face comparisons (`certified: partial`).
Its obsolete warning that the fitter could return a tiny non-MLE scale was
replaced with the implemented behavior, retaining the link to the original
existence review. Lgamma and llogis fit notes explicitly retain convergence
limitations alongside their monotone-root arguments. Their existing
certification grades are preserved. No new finite-parameter search or new
global-search guarantee was introduced.

## Validation

All runs used the workstation, never simbox. Heavy builds and suites ran
under `/tmp/umvl-cpu/run4`; the final diagnostic/provenance probe used
`/tmp/umvl-cpu/run`. Both wrappers apply `nice -n 10`. There was at most one
active workload from this job. Four-slot builds used `-j4`; BLAS/OpenMP were
pinned to one thread per worker. The foreground commands and slot waits were
allowed to finish; no waiting command was interrupted.

Environment: Linux `7.0.0-38-generic`, x86_64; GCC 15.2.0
(`Ubuntu 15.2.0-16ubuntu1`); R 4.6.1 (2026-06-24); Rcpp 1.1.2;
BH 1.90.0.1; testthat 3.3.2; roxygen2 8.0.0; Python 3.14.5;
Node v22.22.1. Existing seeds, workloads and assertion tolerances were retained.
New collapse/status checks are exact; finite-fit controls test successful
status/finite likelihood. No new stochastic study was needed.

The common command environment was:

```sh
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp"
export XDG_RUNTIME_DIR="$PWD/build/tmp"
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
export CMAKE_BUILD_PARALLEL_LEVEL=4
mkdir -p build/tmp build/scratch
```

The required recipes were invoked with `just` inside the four-slot wrapper,
sequentially without nested slot reservations. `just test` used the default
`dev` tier; `just test-full` selected `full`. R runs set
`UMVL_DISTRIBUTIONS_DIR` to this worktree's canonical distribution directory,
`PKG_BUILD_EXTRA_FLAGS=false` and `TESTTHAT_CPUS=4`.

| Command/check | Machine | Result |
|---|---|---|
| `just cpp-test` | Local | **106/106 dev shards passed**; CTest 51.53 s. |
| `just regen` | Local | Passed. |
| Targeted R filter below | Local | **FAIL 0, WARN 0, SKIP 6, PASS 3131**; 73.3 s. Six full-tier skips. |
| `just cpp-test-no-long-double` | Local | **127/127 shards passed**; CTest 119.74 s. |
| `just test` | Local | **FAIL 0, WARN 0, SKIP 151, PASS 20156**; test runner elapsed 113.952 s. |
| `just test-full` | Local | **FAIL 0, WARN 0, SKIP 0, PASS 41759**; test runner elapsed 222.221 s. |
| `just js-build` | Local | Passed. |
| `just js-test` | Local | **413/413 passed**, no failures, cancellations or skips; 90.346 s. |
| `just relations` | Local | **157 edges, 0 failed**. |
| `just site-check` | Local | **Failed** on pre-existing malformed math in paralogis/invparalogis; detailed below. |
| Final R diagnostic probe | Local | All seven requested/related calls stopped with the corrected unbounded-likelihood messages. |
| `git diff --check` | Local | Passed. |
| Golden-source diff | Local | Empty; no fixtures regenerated. |

The targeted command after regeneration was:

```r
options(pkg.build_extra_flags = FALSE)
testthat::test_local(
  "univariateML-r",
  filter = "dist-(tnorm|snorm|foldnorm|lgamma|llogis)|conformance|golden_regression",
  stop_on_failure = TRUE
)
```

The diagnostic probe loaded the generated package and evaluated:

```r
mltnorm(rep(0, 3), a = 0, b = 1, mean = -1)
mltnorm(rep(1, 3), a = 0, b = 1, mean = 2)
mltnorm(rep(0, 3), a = NULL, b = 1, mean = -1)
mltnorm(rep(0, 3), a = 0, b = 1, sd = 1)
mltnorm(rep(1, 3), a = 0, b = NULL, sd = 1)
mlsnorm(rep(0, 3))
mlsnorm(0)
```

The first three report unbounded likelihood as `sd -> 0` and identify the
projected held mean. The next two identify the held-scale endpoint location
escape. Both snorm calls report scale collapse and unbounded likelihood.
No numerical estimate was returned in these probes.

The dev skips are tier/dependency skips, not passes; the full R tier had no
skips. No failures were accepted as permitted failures. Build output also
contains an unchanged binom self-comparison compiler warning, shell startup
messages about a missing `.cargo/env`, and Pagefind's informational UI notice;
these are distinct from R's zero-warning test results.

## Open: pre-existing out-of-scope site math failure

`just site-check` built 212 pages successfully and passed its inference
browser, theory numerical/browser, limit catalogue, covariance scopes,
relation graph, explorer and rendered-link checks. Its final KaTeX-error scan
then failed on exactly these four pages:

```text
site/dist/distributions/paralogis/index.html
site/dist/distributions/invparalogis/index.html
site/dist/lab/paralogis/index.html
site/dist/lab/invparalogis/index.html
```

The corresponding canonical README prose contains two occurrences per family
of U+0007 followed by `lpha`, where the intended LaTeX is `\alpha`. KaTeX
reports `Unexpected character` at the control character, for both the plain
and squared alpha expressions. This is not a failure in an owned dossier.

Both source files are byte-identical to the job's base revision. Read-only
`git rev-parse HEAD:<path>` and `git hash-object <path>` produced these matching
Git blob IDs:

```text
paralogis/README.md     19428abc403231faa689e55a60fca100ac6c8a85
invparalogis/README.md  235ae1db7284d162c1f4e96205329f597fdf6ae5
```

Required resolution: replace the four literal U+0007-plus-`lpha` strings with
`\alpha` in those two READMEs, then rerun `just site-check` locally through
`/tmp/umvl-cpu/run4`. No fitter, renderer or assertion change is needed.
The brief explicitly limits family edits to tnorm, snorm, foldnorm, lgamma
and llogis. Permission for these two additional README typo fixes was
requested; no response had arrived when this report was written. The files
remain untouched. There is no all-green final-site result to claim.

Existing numerical solver/global-certification limitations stated in the
five dossiers remain; this job's new nonexistence checks themselves require
no numerical certification margin. No requested collapse path remains open.

Artifact presentation: the prescribed `show` command was attempted after
writing the report. The viewer returned HTTP 403 because this worktree is
outside its configured readable roots. No viewer configuration or files
outside this worktree were changed; the report remains at the requested path.
