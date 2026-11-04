# TASK-140 research evidence

Companion to the [closure report](../2026-11-04-t-closure-report.md).
All calculations and regression suites ran locally through the shared CPU
locks. No production source or reference fixture was changed.

## Reproduce the final research checks

Run from the worktree root, using the already installed Python dependencies:

```sh
mkdir -p build/tmp build/scratch
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-04-t-closure-data/research.py
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-04-t-closure-data/verify.py
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-04-t-closure-data/continuation.py
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-04-t-closure-data/figures.py
```

- [research.py](research.py) independently evaluates the t likelihood and its
  shape derivative, solves conditional nuisance scores, checks containment,
  and records a geodesic score-to-value bound. It calls no production fitter.
  [checks.json](checks.json) contains the small explicit workloads, moments,
  conditional profiles and Python environment. Returned conditional points
  pass a total standardized score threshold of 1e-7; a failure stops the run.
- [verify.py](verify.py) checks the feasible negative-seam witness, zero-slope
  second and third terms, the critical-tie cusps, gamma remainder, shape chords,
  and stationary candidates. Exact rational checks isolate the nine location
  roots and prove the bounds needed for a finite-attainment std example.
  [verification.json](verification.json) retains the results and root boxes.
  Profile seam checks use 80 decimal digits; stationary refinement uses 75.
- [fixtures.json](fixtures.json) is the fixed discovery input. Its decimal
  observation strings, interpreted exactly, define the high-precision examples;
  the NumPy calculation uses their nearest binary64 values. The stationary
  candidates are re-solved independently, not accepted from this fixture.
  [multimodality.json](multimodality.json) stores the refined coordinates,
  total likelihoods, scores and Hessian inertia. These are numerical local
  candidates; there is no global-shape root-completeness claim.
- [continuation.py](continuation.py) performs the targeted 37-step weight
  continuation. [continuation.json](continuation.json) records that it found
  no pair of interior maxima both above ν = 2. This is not an absence theorem.
- [figures.py](figures.py) makes [profiles.png](profiles.png), the single compact
  report figure. Figure sampling is illustrative, not a certificate.
- [validation.json](validation.json) records the tested source revision,
  environment, commands, final suite counts and script/input hashes.
  [record.py](record.py) reconstructs that record from the final logs under
  `build/scratch/`; run it only after all four named regression logs exist and
  their commands have completed successfully. Its fixture-regeneration flag
  refers to production goldens/truth fixtures, not these research outputs.

The optional discovery run is:

```sh
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-04-t-closure-data/search.py
```

It uses seed 140 and up to 1,500 proposals, writes reconnaissance under
`build/scratch/`, and never substitutes the grid endpoint for an estimate.
Conditional failures discard that proposal; retained shape roots require
subsequent high-precision verification. Discovery is not needed to reproduce
the fixed retained examples.

## Regression commands

```sh
/tmp/umvl-cpu/run4 just cpp-test
/tmp/umvl-cpu/run4 just test
/tmp/umvl-cpu/run4 env UMVL_TEST_TIER=full just cpp-test
/tmp/umvl-cpu/run4 just test-full
```

The final actual execution reserved all four slots once around a foreground
script running these four recipes sequentially. Each output was captured under
`build/scratch/`. The locks pin OpenMP and BLAS to one thread; run4 permits four
CMake workers, and the recipes use four R workers. The record preserves log
SHA256 hashes and final summaries without retaining large compilation logs.

## Arithmetic and interpretation

Proofs and bounds are in the report. Rational polynomial isolation and positive
exp Taylor lower sums are exact arithmetic checks. mpmath root refinement,
Hessian signs and binary64 chord comparisons are numerical evidence. The chord
comparison allowance is 1e-8 absolute; the largest computed conditional value-gap
bound there is below 2.24e-10. Full stationary residuals must be below 1e-55.
Higher-order seam limits have the explicit 1e-3 (quadratic) and 1e-2 (cubic)
scaled convergence tolerances in verify.py, at the largest ν = 100,000.
Cusp checks at ε = 1e-12 require scale-ratio error below 1e-8 and
value-remainder/ε below 1e-7. A small score, a favorable Hessian or a clean
regression suite does not certify a full-shape global maximum.

`document-validation.json` records strict source/HTML math validation, local link checks, input-hash checks and the unchanged tracked-source diff. The artifact pane rejected the worktree path with HTTP 403; no viewer configuration or other checkout was changed.
