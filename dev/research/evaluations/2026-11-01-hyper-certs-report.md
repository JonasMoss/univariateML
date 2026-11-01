# Hypergeometric growing-population searches

2026-11-01, updated for round 2. Tested source: `f9092af03bdaa982e9abc21f1a527db2d3f3113b` plus the uncommitted changes in this worktree. No golden values or generated d/p/q/r implementations were changed.

## Results

Hyper `{k}` and `{m}`, and nhyper `{r}` and `{}`, now search for finite maxima. At each population the fitter uses the existing unimodal composition search; free nhyper `r` is enumerated over `1,...,min(x)`. After exploring the first 256 feasible populations, a candidate exceeding the appropriate maximized binomial or negative-binomial face by `1e-9*(1+abs(face))` gives a bound-based cutoff. The search checks every feasible population below that cutoff, subject to the existing 10^7 evaluation budget and additional preflight limits. It returns numerical failure rather than a partial maximum when a work or representation limit is reached. The two dossiers give the inequalities and local first-order face coefficients.

The finite maximum is certified in exact arithmetic conditional on the profile comparisons and tail bound. The implementation's margin is a practical floating-point tolerance, **not a proved upper bound on rounding error**. Existing profile-search sign and tie comparisons are also numerical. Directed-rounding enclosures and validated error bounds remain open. The extra work guards limit held hyper size and maximum nhyper observation to 100,000 and bound the nhyper face/correction scans; they can reject a problem even when the nominal evaluation budget would suffice.

If exploration finds no strict improvement, the fitter stops with `FaceUnresolved`; a finite likelihood numerically matching the face produces `FaceTie`. The messages say that a larger finite maximum is not excluded and identify the limit model. Neither a finite exploratory search nor the tail bound proves that the face is the global supremum. In particular, `FaceTie` reports only a numerical match, not a proved attained global maximum. Exact attained cases are returned: a constant hyper sample, hyper with held size one (a Bernoulli family), zero-only nhyper with `r=0`, and constant nhyper equal to its held or free stopping count. Other finite ties to the face remain unresolved.

Hyper `{}` stays not implemented: the fixed-size correction is not uniform when both `m` and `k` grow. A bound covering growing binomial sizes and the Poisson face, along with a global face comparison, remains missing. No TASK-78-owned file was changed.

## Validation

The earlier seeded inequality check remains in [check_bounds.py](2026-11-01-hyper-certs-data/check_bounds.py): seed 121, 10,000 cases per inequality, populations through 1,000, tolerance `1e-9`; largest residuals `1.66e-12` hyper and `2.19e-12` nhyper. This checks algebraic consistency in double precision, not profile or rounding certification.

The new C++ tests compare finite fits against explicit likelihood grids through population 500 (hyper) and 300 (nhyper), including symmetric held sets, exact finite ties, face stops, and work-guard failures. R tests use seeds 12101 and 12102 with three samples each from populations 12 and 14, respectively; each supported subset agrees in log likelihood with independent grid enumeration through population 120, at tolerance `1e-8`. Additional deterministic reference grids extend through 500 and 300. The tests exercise the preflight numerical-work guard, **not exhaustion of the full 10^7 evaluation budget**. A separate [large-population check](2026-11-01-hyper-certs-data/large-population.R) uses seed 12103 and 150 observations from populations of 1,000. Both hyper holds returned log likelihood -227.2679, exceeding the enumerated best through population 200 (-227.2693); both nhyper subsets returned -288.8419, matching the grid maximum to the printed precision. [Recorded output](2026-11-01-hyper-certs-data/large-population.txt). The hyper result therefore lies beyond that comparison grid, and this check does not independently certify the entire tail. Broader near-face samples, adversarial numerical ties, and outward-rounded comparisons remain open coverage gaps.

The targeted R hyper/nhyper and conformance checks passed. An initial full C++ run had one failure because an old assertion expected `BothFree` for a now-supported subset; the expectation was updated and the complete suite rerun successfully. The initial targeted R run similarly identified two obsolete or unsuitable test expectations; both were corrected before the final runs.

## Final runs

Each command ran separately through `/tmp/umvl-cpu/run4` with BLAS and OpenMP pinned to one thread and temporary files under `build/tmp`. Logs are ephemeral under `build/scratch/`.

| Command | Result |
|---|---|
| `just cpp-test` | Pass, 105/105 development shards. |
| `just cpp-test-no-long-double` | Pass, 126/126 tests. |
| `just test` | Pass: 19,876 passes, 0 failures, 0 warnings, 151 tier skips. |
| `just test-full` | Pass: 41,322 passes, 0 failures, 0 warnings, 1 skip (optional `sn` unavailable). |
| `just js-build` | Pass. |
| `just js-test` | Pass, 400/400. |
| `just site-check` | Pass after the authorized copy of the existing `site/node_modules` into this worktree. |
