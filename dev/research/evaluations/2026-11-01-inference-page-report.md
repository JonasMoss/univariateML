# Inference availability after covariance (TASK-102.9)

2026-11-01. Completed in `codex/inference-page`, based on source revision
`7d6fe5361668c0ed49cf99b6d4198a933cb6bb1d`, with the uncommitted changes
listed below. No commits, publishing, board edits, dependency installations,
C++ source changes or R source changes were made.

## Result

The canonical catalog and `/inference/` now include one covariance row for
every family × `inference_numeric` regime: **179 rows across 98 families**.
There are **135 implemented**, **6 unreviewed**, and **38 invalid for the
procedure** covariance scopes. The complete catalog has **336 rows**, including
163 implemented scopes across covariance and the existing procedures.

`dev/validation/inference_availability.py` generates schema version 2 of
`dev/validation/inference-availability.json`. Availability, regime conditions,
classification links and unavailable reasons come from canonical README front
matter. Available means the C++ and R covariance API is implemented in that
scope; it does not promise that a matrix exists at every point. In particular,
smooth singular scopes retain their conditions and runtime rejection caveats.
Unavailable scopes whose reasons explicitly leave classification open are
unreviewed; the remaining unavailable scopes are invalid for this procedure.
The original reasons remain visible.

The generator discovers family evidence in the retained derivative-validator
JSON files and reads the pilot and simbox Monte Carlo JSON records. Displayed
run summaries retain sample size, truth, held/free coefficients, completion
status, successes and failure count. Family-specific interpretations are
extracted from the source report's follow-up tables and concluding bnbinom
paragraph, including later truth-point comparisons. No family availability
matrix or Monte Carlo result was transcribed into the page by hand.

Evidence sources are the [C++ report](2026-11-01-vcov-scores-report.md), its
[retained data](2026-11-01-vcov-scores-data/summary.json), its derivative
validation and both simbox follow-up sections, and the
[R interface report](2026-11-01-vcov-r-report.md). Near-singular sn/bhatt
members, bnbinom weak identification at n = 500, and invtrgamma finite-sample
bias remain explicit caveats.

## Page and mathematical scope

`site/src/pages/inference/index.astro` renders covariance beside the existing
procedure rows. A procedure filter selects covariance or other procedures and
combines with status, family and regime filters. Every row and native evidence
disclosure remains available without JavaScript. The page documents
`vcov(fit)` as default sandwich and `vcov(fit, type = "observed")`; the other
cataloged procedures retain accurate C++ only wording.

In natural free coordinates, let `H` be the negative total log-likelihood
Hessian and `J = sum_i s_i s_i^T` the sum of individual score cross-products.
Observed covariance is `H^-1`; sandwich covariance is `H^-1 J H^-T`.
These are total-likelihood quantities, with no additional sample-size factor.
Held coefficients are genuinely known; fitted nuisance coefficients remain
in the free matrix. Correct specification and regular estimator asymptotics
are needed for observed model inference. Robust sandwich inference additionally
needs finite score variance, an identifiable pseudo-true target and a suitable
estimating-equation expansion.

This job introduces no new regularity proof or numerical covariance algorithm.
DQM, finite information, rank, Hessian suitability and estimator asymptotics
remain separate claims in the linked classifications. Derivative comparisons
and Monte Carlo SE-versus-SD comparisons are numerical evidence at recorded
points, not proofs or coverage validation. Failures and conditioning limits
are retained rather than replaced with usable estimates.

## Acceptance checks and final runs

`site/scripts/check.py` checks one covariance row per README scope, exact
conditions and classification links, implemented counts versus availability,
and preservation of unavailable reasons. Every covariance evidence path and
anchor resolves locally. Monte Carlo summaries are checked against their JSON
sources. Supporting C++ and R symbols resolve. `site/scripts/inference-browser.mjs`
adds covariance and combined-filter checks with follow-up evidence disclosure.
The existing `site/scripts/data.py` projection already carries the new rows;
no second site-specific catalog was needed.

All final commands below ran locally on the workstation, separately and in the
foreground through `/tmp/umvl-cpu/run4`; each finished before the next began.
The wrapper supplies `nice -n 10`, four shared CPU slots and one BLAS/OpenMP
thread per worker. `TMPDIR`, `JUST_TEMPDIR` and `XDG_RUNTIME_DIR` pointed to
this worktree's `build/tmp`. No simbox runs were started. Site dependencies
were copied once from the allowed checkout before building.

| Command after `/tmp/umvl-cpu/run4` | Final result |
|---|---|
| `python3 dev/validation/inference_availability.py --check` | Pass; 336 canonical rows; freshness check performs no write |
| `just site-check` | Pass; 209 pages built; all metadata, evidence, C++17 snippets, MathML and browser checks green |
| `just cpp-test` | 105/105 dev shards pass; zero failures |
| `just test` | 19,945 pass; zero failures/warnings; 151 expected tier skips |
| `just test-full` | 41,413 pass; zero failures/warnings/skips |

Site-check recorded Python 3.14.5, SymPy 1.14.0 and GCC 15.2.0; browser checks
used Node 22.23.4. Desktop 1280 × 1100 and mobile 390 × 1000 checks passed,
including all status filters, covariance and combined filters, empty state,
reset, disclosures, no horizontal overflow, no page errors and all 336 rows
without JavaScript. The R covariance file passed 47 assertions in both tiers.

During iteration, generation caught an accidentally duplicated loop, which was
corrected before successful regeneration. The first site-check passed its build
and static checks but caught an overly broad browser locator matching hidden
rows; scoping it to the visible row fixed the test, and the full site-check
rerun passed. The C++ recipe's initial Boost lookup failed, then its existing
BH fallback configured and built successfully. Existing compiler warnings and
shell startup messages about the absent cargo environment file did not fail
the final commands.

## Open evidence and display limits

No implementation item remains open for this job. Coverage under every regime,
held subset and misspecification remains unvalidated by the upstream evidence;
the page says so. The second simbox follow-up's changed-truth and n = 5000
results are retained as report tables/prose in this revision, without separate
raw JSON files for those reruns. The catalog links that evidence explicitly
rather than manufacturing raw records or generalizing it to every scope.

The built page is `site/dist/inference/index.html`. An attempt to open it with
the prescribed artifact viewer received HTTP 403 because this worktree is
outside the viewer's configured readable roots. This does not affect the
successful local browser tests. No files were copied outside this worktree to
work around the viewer restriction.
