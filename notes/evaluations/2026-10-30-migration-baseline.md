# Migration baseline — TASK-101.1

Source: `5f11fb9e878219ac47a9a372e985eb69da6198e3` (`codex/baseline`).
Recorded 2026-10-30; no fitter, evaluation, metadata, fixture, or test source changes.

## Run record

Simbox ran `just cpp-test` and `just test` through
`/tmp/umvl-codex/simbox-run.sh` from this worktree's mirror.
During `just check`, its filesystem became read-only with I/O errors;
even basic commands segfaulted. Check was interrupted (exit 101), and quality
could not start. The main runner ended and log sync failed.
The complete four-check record below therefore uses the authorized **local fallback**,
through `/tmp/umvl-cpu/run`; remote observations are reported separately.
Both runs used the shared slot lock, `nice -n 10`, one build/BLAS/OpenMP thread,
`NOT_CRAN=true`, `$PWD/build/tmp`, and transient logs under `build/scratch/`.
No dependency installs, golden regeneration, source edits, commits, board edits, or publication.

Simbox: Intel i7-12700 (20 logical CPUs), Ubuntu 24.04.5 LTS,
WSL2 kernel 6.18.54.1-microsoft-standard-WSL2, x86_64.
CMake GNU C++ 13.3.0 (`/usr/bin/c++`), C++17, ccache off;
R `ccache g++-15`, ccache on (GCC 15 patch version unrecoverable).
Python 3.12.3; sympy 1.12; mpmath 1.2.1; scipy 1.11.4.
[simbox-environment.log](2026-10-30-migration-baseline/simbox-environment.log)
preserves observed values; [simbox-interruption.log](2026-10-30-migration-baseline/simbox-interruption.log)
preserves test summaries and infrastructure errors.

Local: Intel i7-1355U (12 logical CPUs), Ubuntu 26.04.1 LTS,
kernel 7.0.0-38-generic, x86_64; CMake GNU C++ 15.2.0, C++17, ccache off;
R `ccache g++` (15.2.0), ccache on. Python 3.14.5; sympy 1.14.0; mpmath 1.3.0; scipy 1.18.0.
Both: R 4.6.1 (2026-06-24), Rcpp 1.1.2, BH 1.90.0.1, testthat 3.3.2.
Local configuration: [environment.log](2026-10-30-migration-baseline/environment.log).

Workloads: repository tests and frozen fixtures; goldens record samples, seeds, samplers and fit arguments.
C++ truth checks consume existing tables and use 20,000 draws per parameter set, seed `20261029 + point`, log tolerance `1e-10`,
quantile relative tolerance `1e-9` or oracle brackets; CDF sampling tolerance
`5*sqrt(p*(1-p)/20000)+1/20000`.
Golden tolerances vary by test: shared `kTol=1e-10`, `kFdTol=3e-5`,
`kFitTol=1.220703e-4`; source SHA256s pin the actual inputs/tests.
Quality uses each golden sample, 30-digit mpmath log likelihood, logLik tolerance `1e-9*(1+abs(logLik))`, standardized score `<=0.001`,
and eight scipy starts (seed 20261028); oracle gap tolerance `1e-6*(1+abs(logLik))`.

## Results

Local `just cpp-test`: **6/6 pass**, 491.15 s; zero skipped cases.
[cpp-targets.log](2026-10-30-migration-baseline/cpp-targets.log) records:

| ctest target | result | doctest cases | assertions |
|---|---|---:|---:|
| bfgs_tests | pass | 6 | 21 |
| sample_tests | pass | 1 | 10,204 |
| distribution_tests | pass | 905 | 1,215,269 |
| no_throw_tests | pass | 3 | 56 |
| cmp_tests | pass | 8 | 619 |
| tails_tests | pass | 4 | 147 |
| Total | pass | 927 | 1,226,316 |

Truth-table `may_fail` cases: **none** at this revision; no permitted failures.
[may-fail.log](2026-10-30-migration-baseline/may-fail.log) records the source scan.
Simbox also passed 6/6 (254.11 s); its per-target assertion logs were lost.
Simbox testthat: `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 37713 ]`, 577.3 s;
no skips or warnings. Its interrupted check/absent quality are infrastructure gaps.

Local `just test`: `[ FAIL 0 | WARN 0 | SKIP 1 | PASS 37708 ]`.
Only skip: `test-dist-sn.R:135:3`, `Reason: {sn} is not installed`
(the five-assertion optional `sn::dsn` comparison). Local testthat uses its `Llm` reporter.
Generation emitted 976 unresolved-link diagnostics and three `-Wmisleading-indentation`
warnings: `dlaplace/dpqr.hpp:18` once; `tpois/dpqr_core.hpp:38` twice (via tbinom/tpois).
Every generation/compiler diagnostic is preserved verbatim in [test-generation.log](2026-10-30-migration-baseline/test-generation.log);
[test.log](2026-10-30-migration-baseline/test.log) records the separate testthat result.
Local `just check`: **Status: OK**; **0 ERROR, 0 WARNING, 0 NOTE**.
[00check log](2026-10-30-migration-baseline/R-CMD-check.log) preserves the status verbatim.
Check testthat: `[ FAIL 0 | WARN 0 | SKIP 1 | PASS 37708 ]` (same `sn` skip).
Manual checking and vignettes were omitted by the requested recipe/options;
`* checking files in ‘vignettes’ ... SKIPPED`. Regeneration repeated all 979 diagnostics ([log](2026-10-30-migration-baseline/check-generation.log)).
Local `just quality`: **98/98 pass**: 98 likelihood passes; 93 score/Hessian passes
plus five skips; 94 oracle passes plus four skips. [Per-family results](2026-10-30-migration-baseline/quality-families.log).
Both derivative and oracle checks skip dunif, hyper, nhyper and unif:
`no regular parameters`. Laplace derivatives skip: `nonsmooth location at an observation; observed Hessian undefined`.
Triang's kink coordinate `c` is excluded from derivatives even though its family passes.
All held/integer/endpoint/kink coordinates are in [quality-coverage.log](2026-10-30-migration-baseline/quality-coverage.log).
All four local recipes exited 0 ([exits.log](2026-10-30-migration-baseline/exits.log)); run ended 15:43:35 UTC.

## Source coverage

[coverage.tsv](2026-10-30-migration-baseline/coverage.tsv) lists every family,
truth matrix row counts, parameter-set counts, golden fixture counts and sample sizes.
98/98 families have `truth.json`: **35,122 values rows** (log density and both log CDF tails),
plus **25,168 quantile rows**. These are separate matrices, not assertion counts.
98/98 families have **one golden fixture each** (98 total); no goldens were regenerated.

The reviewed [boundary evidence](2026-10-30-migration-baseline/boundary-evidence.tsv)
records explicit formulas/values in `test.cpp` for **48 distinct families**:
**37 have evaluation evidence** and **11 have fitter-only closed forms**.
Evaluation evidence comprises 22 attained-parameter families, eight R7a density families, ten additional R7a
quantile-only families (three families overlap the first two groups).
Evaluation counts require an explicit boundary assertion (deterministic point sampling included);
they do not mean every boundary, intersection, tail or operation is checked.
Fitter-status-only tests and round trips are excluded; fitter-only formulas are counted separately.

- Evaluation at attained members: bhatt, binom, cmb, cmp, dunif, geom, gev, gpd, hyper, nbinom, nhyper, pig,
  skellam, tbinom, tlambda, tpois, triang, zib, zinb, zipf, zmbinom, ztbinom.
- R7a densities: genbeta, gev, gpd, hcauchy, nsbeta, power, tnorm, triang.
- R7a quantile-only evidence: fatigue, fpareto, frechet, hnorm, ht, pareto,
  pareto2, pareto3, pareto4, unif.

The 11 fitter-only families are foldnorm, pois, sgomp, zip, zmgeom,
zmlogarithmic, zmnbinom, zmpois, ztgeom, ztnbinom and ztpois (closed-form fitted
parameters or point-law log likelihoods, without the evaluation assertions counted above).

Metadata contains **71 holds entries**: **11 `not-implemented`**, **60 `no-mle`**.
These are structural entries, not counts of held subsets or failed samples.
[holds.log](2026-10-30-migration-baseline/holds.log) preserves every entry and reason.
There are **262 declared limits**; [limits.log](2026-10-30-migration-baseline/limits.log)
preserves family, model/name, path, status, condition and held-set qualifiers.
[source-sha256.log](2026-10-30-migration-baseline/source-sha256.log) identifies all
98 README/test/truth/golden inputs; [inventory.py](2026-10-30-migration-baseline/inventory.py)
reproduces counts and validates the reviewed boundary markers.

## Known gaps and interpretation

Goldens test regression against the package's frozen fitter output. Truth tables supply
independent numerical evidence at recorded parameter sets, not exhaustive domain/boundary coverage.
The boundary inventory is deliberately narrower than the earlier 98-family prose audit:
quantile endpoints do not establish density closure; sampler-only assertions do not check d/p/q.
The 61 families outside the evaluation union are not thereby defective or wholly untested.
Held-subset checks on golden data do not prove existence for arbitrary samples.
Quality limits are listed, **not executed**; skipped coordinates/checks remain coverage gaps.
Multistart optimization and numerical score/Hessian checks do not prove a global MLE,
DQM, regular inference, or interval/test calibration. This run adds no mathematical proof.
Check used the external dossier directory; isolated source exports were not tested.
Follow-ups: review roxygen link-resolution noise and the three compiler warnings in TASK-78's evaluation headers; no numerical fix is inferred.

## How to rerun

From this worktree root, with existing dependencies and the simbox wrapper available:

```sh
bash notes/evaluations/2026-10-30-migration-baseline/run.sh # simbox; add --local for locked fallback
bash notes/evaluations/2026-10-30-migration-baseline/run.sh --only cpp-test
bash notes/evaluations/2026-10-30-migration-baseline/run.sh --only test
bash notes/evaluations/2026-10-30-migration-baseline/run.sh --only check
bash notes/evaluations/2026-10-30-migration-baseline/run.sh --only quality
/tmp/umvl-cpu/run python3 notes/evaluations/2026-10-30-migration-baseline/inventory.py
```

The first command captures environment, all four recipes, ctest target counts and inventories.
Each command saves trimmed logs; append `--local` for the locked fallback.
The script rejects another commit or modified tracked sources; a new revision needs a new run record.
