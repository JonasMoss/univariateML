# TASK-150.2.1: local accuracy of eight default fitters

Completed in `codex/acc-newton`, based on `c60d92ea`, on the workstation. No commits, pushes, package installs, board edits, or work in other checkouts were made.

The default golden fits of **lomax, kumar, weibull, gumbel, fatigue, cmb, pig, and foldnorm** meet the `1e-10` coefficient target in native, double-intermediate, R, and WASM execution. All **83** selected audit coefficient misses are repaired in all four modes, with no new stops in that replay. Every required final test suite and the site check finished green before this report was written.

## Default golden accuracy and regression policy

Each entry below is the largest componentwise relative coefficient error against the family's stored 50-digit reference. Returned binary64 values are converted exactly to Decimal before comparison at 70-digit precision. All coefficients in this golden comparison are nonzero. The before column was reproduced using headers and common bindings archived read-only from `c60d92ea`; the after columns use the final source.

| Family | Before native | After native | After double | After R | After WASM |
|---|---:|---:|---:|---:|---:|
| lomax | 7.857e-08 | 7.153e-17 | 9.071e-16 | 7.153e-17 | 9.071e-16 |
| kumar | 4.964e-09 | 1.753e-16 | 1.753e-16 | 1.753e-16 | 3.333e-16 |
| weibull | 2.959e-09 | 3.208e-15 | 3.208e-15 | 3.208e-15 | 3.208e-15 |
| gumbel | 2.333e-10 | 1.294e-15 | 1.294e-15 | 1.294e-15 | 1.294e-15 |
| fatigue | 3.140e-09 | 3.118e-13 | 3.118e-13 | 3.118e-13 | 3.118e-13 |
| cmb | 2.965e-10 | 5.585e-15 | 7.935e-14 | 5.585e-15 | 7.196e-14 |
| pig | 1.892e-10 | 4.429e-13 | 4.429e-13 | 4.429e-13 | 4.429e-13 |
| foldnorm | 2.743e-09 | 6.330e-15 | 6.330e-15 | 6.330e-15 | 6.330e-15 |

The worst new golden coefficient error is `4.429e-13` (pig). The eight golden JSON files, including data, stored estimates and reference strings, are byte-identical to the base. [Measurements](2026-11-10-acc-newton-data/measurements.json), [measurement code](2026-11-10-acc-newton-data/measure.py), and [frozen input hashes](2026-11-10-acc-newton-data/frozen-inputs.json) retain the calculation and inputs.

C++ `test_support.hpp` and the R golden policy now use these allowances. Generated JS golden cases inherit the C++ policy. The policies also pass the existing likelihood comparisons. These are empirical regression allowances; the fitters' stopping rules supply the local accuracy justification.

| Family | Previous allowance | New allowance | Multiple of worst new error |
|---|---:|---:|---:|
| lomax | 1.000e-07 | 2.000e-15 | 2.20 |
| kumar | 5.000e-09 | 5.000e-16 | 1.50 |
| weibull | 3.000e-09 | 5.000e-15 | 1.56 |
| gumbel | 3.000e-10 | 2.000e-15 | 1.55 |
| fatigue | 5.000e-09 | 5.000e-13 | 1.60 |
| cmb | 3.000e-10 | 2.000e-13 | 2.52 |
| pig | 2.000e-10 | 1.000e-12 | 2.26 |
| foldnorm | 3.000e-09 | 1.000e-14 | 1.58 |

No allowance was loosened. The first five families retain their existing `reltol`/`abstol` names, with defaults changed from `1.220703e-4` to `1e-10`; pig's `reltol` default changes from `1e-9` to `1e-10`. Cmb and foldnorm already defaulted to `1e-10`. The common bindings, R adapters, and family documentation agree. Control renaming remains outside this job.

## Local stopping rules

The existing global search and face-comparison procedures remain the outer decision procedures. Local polishing and error propagation now determine the coefficients returned from their selected candidate. The family dossiers record the family-specific equations and practical floating-point margins.

| Family | Final local acceptance and stable evaluation |
|---|---|
| [lomax](../../../univariateML-cpp/include/cppunivariate/distributions/lomax/README.md) | Analytic profile score and curvature give corrections in rate and log-kappa. Stable dimensionless moments and positive power series avoid subtraction near the exponential face. Acceptance checks both public coefficients and contraction/rounding resolution; a separately signed local enclosure with negative curvature provides an alternative. With kappa held, an FMA count contribution and logarithmically scaled logistic tails preserve very small score terms; an independently signed log-rate box controls the answer. |
| [kumar](../../../univariateML-cpp/include/cppunivariate/distributions/kumar/README.md) | Refine the unique signed shape root until both the shape interval and its induced second-shape interval are below one hundredth of the requested tolerance. A secant point stays inside the final box. Nonfinite accuracy controls remain rejected. |
| [weibull](../../../univariateML-cpp/include/cppunivariate/distributions/weibull/README.md) | The log-data Gumbel reduction controls reciprocal scale for shape and the exponential map for public scale. Relative `log1p` evaluations preserve concentrated samples. With scale held, the strictly decreasing analytic shape score is bracketed with separately checked signs, and relative log ratios avoid large-shape cancellation. |
| [gumbel](../../../univariateML-cpp/include/cppunivariate/distributions/gumbel/README.md) | A strictly increasing profiled scale score has derivative `1 + weighted_variance / sigma^2`. Its signed bracket bounds both scale and the monotone profiled location, using sigma as the location accuracy floor. The location enclosure also controls the exponential map needed by Weibull. Held-scale location weights and held-location score sums use compensated accumulation. |
| [fatigue](../../../univariateML-cpp/include/cppunivariate/distributions/fatigue/README.md) | Return the certificate's refined scale root directly, removing the loose legacy proposal overwrite. Scale bracket width and the induced alpha enclosure must both meet one hundredth of the requested tolerance. Existing near-constant moment-resolution checks remain in force. |
| [cmb](../../../univariateML-cpp/include/cppunivariate/distributions/cmb/README.md) | Polish in canonical logit-probability/nu coordinates using the analytic covariance information matrix. Require positive information on the free coordinates and small transformed componentwise Newton corrections with rapid contraction or rounding-level resolution. The probability correction includes the logistic derivative. Held coordinates do not impose irrelevant curvature conditions. Existing size enumeration and global acceptance remain in force. |
| [pig](../../../univariateML-cpp/include/cppunivariate/distributions/pig/README.md) | Refine the finite shape root inside its global enclosure to one hundredth of the requested log-shape tolerance, using separately signed neighbors when a midpoint rounds to zero. A shifted Bessel-degree recurrence and algebraic score cancellation preserve precision near the Poisson face; the original recurrence is retained in the tiny-mean regime. |
| [foldnorm](../../../univariateML-cpp/include/cppunivariate/distributions/foldnorm/README.md) | Use analytic curvature for the final scalar Newton correction, propagating the profiled variance relation into both mu and sigma. Well-separated modes use original-data normal moments, with a reflected-tail bound. A cancellation-free half-normal seam score and its derivative bound isolate a small positive mode when the rounded boundary candidate is a local minimum. |

Physical reconstruction checks cover normalization, exponentiation and subnormal output spacing. A failed accuracy check reports a numerical-accuracy outcome. Search exhaustion retains the existing failure handling. The refinements respect `maxit`-scaled work limits; cmb's extra table evaluations share its existing enumeration ledger. No density, probability, quantile, RNG, special-function, truth-table, or code-generation source was changed.

The bracket and curvature identities are analytic. Certificates follow the package standard: exact-arithmetic reasoning conditional on computed comparisons, with stated practical floating-point margins. High-precision comparisons below are independent numerical evidence on the recorded samples. Global claims continue to rely on the existing family certificates.

## Audit replay and additional conditional checks

The frozen replay contains all 83 coefficient cases selected for these families from the audit's `1e-10` view, retaining its refined reference coefficients and held values. Every baseline case misses the target. Every final case passes in native, double-intermediate, R and WASM execution; there are no failed fits or replacement stops. The worst final replay error is `2.91994e-11` in each mode.

| Family | Cases | Before worst error | After worst across four modes |
|---|---:|---:|---:|
| lomax | 3 | 2.976e-08 | 4.652e-15 |
| kumar | 4 | 7.106e-08 | 4.385e-13 |
| weibull | 14 | 2.142e-05 | 4.022e-15 |
| gumbel | 3 | 3.193e-07 | 5.040e-15 |
| fatigue | 3 | 2.586e-09 | 3.261e-13 |
| cmb | 15 | 2.675e-09 | 2.377e-14 |
| pig | 14 | 3.853e-10 | 2.920e-11 |
| foldnorm | 27 | 4.564e-06 | 9.821e-15 |

[Frozen cases and references](2026-11-10-acc-newton-data/replay-cases.json), [per-case results](2026-11-10-acc-newton-data/replay-measurements.json), and the before/after JSONL captures retain the replay. The replay scripts explicitly preserve cmb's free-size mode in R.

Four further conditional fits exercise coefficient amplification that a default golden alone would miss. Independent 100-digit roots, including quadratic oracles for the two-valued Lomax samples, give:

| Conditional fit | Before error | After worst across four modes |
|---|---:|---:|
| weibull-held-scale | 4.127e-06 | 2.992e-13 |
| lomax-held-kappa | 7.952e-03 | 1.980e-13 |
| lomax-wide-held-kappa | 1.000e+00 | 2.212e-14 |
| lomax-weighted-held-kappa | 3.000e+00 | 2.786e-14 |

The wide Lomax sample is `{1e-200, 1e200}` with kappa one. The weighted sample has three large observations and kappa equal to binary64 `1/3`; its small nonzero count residual must survive rounding. The latter rate is about `4.16334e183`. The concentrated held-scale Weibull shape is about `2.63512e11`. All four execution modes pass these checks. [Inputs](2026-11-10-acc-newton-data/held-cases.json), [independent roots](2026-11-10-acc-newton-data/held-reference.json), [oracle code](2026-11-10-acc-newton-data/held-reference.py), and [verified errors](2026-11-10-acc-newton-data/held-measurements.json) are retained.

Three additional conditional probes now return numerical-accuracy failures in all four modes, with specific binary64 lattice evidence. Write `u` for the smallest positive binary64 value:

- Gumbel, data `{0,u}` and sigma held at `u`: the fitted location is `0.379885493... u`, far from any representable location at the requested tolerance.
- Weibull, shape held at one and data `{u,2u}`: the fitted scale is exactly `1.5u`; the nearest positive binary64 scales miss by one third relatively.
- Lomax, kappa held near `1e158` and data near `{1e158,2e158}`: the fitted rate is about `6.66667e-317`. Exact rational arithmetic bounds the true root relative to the leading rate within `2/kappa`; even the nearest binary64 rate misses by `3.30663e-8` relatively.

The original subnormal Gumbel, Weibull, folded-normal and tiny-mean PIG probes are also retained in [quantization evidence](2026-11-10-acc-newton-data/seam-quantization.json). The focused C++, R and JS tests check their numerical-accuracy outcomes.

For foldnorm, a positive mode next to the half-normal face previously rounded to mu zero. The cancellation-free seam score recovers the recorded finite modes. At the near-zero quartic coefficient, exact binary-rational moment signs and a 100-digit root give a positive location about `2.99893e-8`, or `1.28466e-8` standard deviations: returning zero would miss the default target. The new `32 epsilon` quartic guard reports loss of numerical accuracy in this ill-conditioned region. The exact `{0,c}` exception with positive fraction at most one third retains the half-normal maximum, supported by the positive power-series comparison for `atanh(t) > 3t/(3-t^2)`. [Seam oracle and lattice checks](2026-11-10-acc-newton-data/seam-quantization.py) reproduce this evidence. These guarded cases are separate from the 83 audit cases, all of which continue to return fits.

The near-Poisson pig check has a separate 90-digit shape reference, about `6666.416994630708`, in [pig-face.json](2026-11-10-acc-newton-data/pig-face.json). Weak Lomax roots have separate [high-precision local checks](2026-11-10-acc-newton-data/lomax-local.json).

## Finished validation

All final runs used the **workstation**, through the shared CPU slot wrappers with thread limits and `nice -n 10`. Simbox was unavailable as specified in the brief. Native and double-intermediate captures used GCC 15.2.0. Required full tiers were run after the final numerical changes. A final prose correction was followed by a fresh site check.

| Final command/check | Result |
|---|---|
| Focused eight-family native doctest | 173 cases, 92,086 assertions passed |
| Focused eight-family double-intermediate doctest | 173 cases, 92,086 assertions passed |
| `just cpp-test` | 106/106 shards passed |
| `UMVL_TEST_TIER=full just cpp-test` | 130/130 shards passed |
| `just cpp-test-no-long-double` | 130/130 shards passed on workstation |
| `just test` | 23,031 passes; 0 failures, 0 warnings; 154 tier skips |
| `just test-full` | 45,846 passes; 0 failures, 0 warnings, 0 skips |
| `EMCC_CORES=1 just js-build` | exit 0 |
| `just js-test` | 601/601 passed |
| `just site-check` | passed, including 99 generated C++ snippets compiling and linking |
| Four-mode goldens, audit replay, conditional probes | passed |
| Source state, frozen inputs, append-only test checks, tighter policies, `git diff --check` | passed |

[checks.json](2026-11-10-acc-newton-data/checks.json) records commands, machines, final counts, source base, allowance factors and archived log hashes. Thirteen final logs are stored beside it as deterministic `.log.gz` files. [source-state.json](2026-11-10-acc-newton-data/source-state.json) freezes the modified tracked source files. Existing family C++ tests and the existing R/JS stop-test contents remain intact; additions are appended. Only the eight targeted golden policies and six changed default-control records were tightened in the R golden test file.

**Open display issue:** the required artifact-pane command was attempted after completion, but the viewer returned HTTP 403 because this worktree is outside its registered readable roots. Its registry includes the main checkout and omits `/home/jonas/Files/programming/univariateML.wt/acc-newton`. Display requires registering this worktree with the viewer. The report and all evidence are saved here; no other checkout or viewer configuration was changed. No numerical or test item remains open.

## Reproduction

Run from the repository root with the existing toolchains and dependencies. `run-native.sh` reserves its own slots for each computing command; invoke it **without** an outer slot wrapper. `run-before.sh` runs inside one outer slot and archives the base read-only into scratch.

```bash
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch
h=dev/research/evaluations/2026-11-10-acc-newton-data
/tmp/umvl-cpu/run bash "$h/run-before.sh"
bash "$h/run-native.sh"
```

After the R and JS builds used by the required suites, regenerate the binding captures and comparisons with:

```bash
/tmp/umvl-cpu/run Rscript "$h/capture.R" > "$h/after-r.jsonl"
/tmp/umvl-cpu/run Rscript "$h/replay.R" > "$h/replay-after-r.jsonl"
/tmp/umvl-cpu/run Rscript "$h/held-probes.R" > "$h/held-probes-r.jsonl"
/tmp/umvl-cpu/run node "$h/capture.mjs" > "$h/after-wasm.jsonl"
/tmp/umvl-cpu/run node "$h/replay.mjs" > "$h/replay-after-wasm.jsonl"
/tmp/umvl-cpu/run node "$h/held-probes.mjs" > "$h/held-probes-wasm.jsonl"
/tmp/umvl-cpu/run python3 "$h/measure.py"
/tmp/umvl-cpu/run python3 "$h/verify-held.py"
```

`verify.py` checks source hashes, frozen goldens, append-only tests, all four golden and replay modes, and the final test-log summaries. Its input logs live under `build/scratch/`; the archived copies named in `checks.json` can be decompressed there when validating a preserved bundle. The new native/double reproduction script, baseline reproduction, R captures, WASM captures and final verifiers all completed successfully for this report.
