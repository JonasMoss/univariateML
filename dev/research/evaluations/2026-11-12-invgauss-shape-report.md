# Inverse-Gaussian coefficient accuracy — TASK-150.2.8

Completed 2026-11-12 in `invgauss-shape`, branch `codex/invgauss-shape`.
Baseline: `c4ed89f6a4222e133c7524cf726fea4cb9369593`; changes are uncommitted.

**Both precision modes pass the coefficient contract on every tested free/held route.**
The largest measured relative coefficient error is 5.56e-16, against the 1e-10
threshold. All required local suites finished green before this report was written.

## Calculation and mathematical evidence

For positive observations, let `Q(m) = sum((x_i-m)^2/(m^2*x_i))`.
Its derivative is `Q'(m) = 2*(n*m-sum(x_i))/m^3`; its unique minimum is at the
continuous arithmetic mean. For positive Q, the shape likelihood is strictly
concave and uniquely maximized at `shape = n/Q`. Zero Q gives the documented
unbounded likelihood. This proves the closed forms in exact arithmetic for
both-free, mean-held and shape-held fits; both-held fits only evaluate likelihood.

The previous native free fit formed `x/mean - 1`, losing residual precision,
and evaluated shape using the rounded public mean. The double-only correction
was conditional on a likelihood-gap threshold. That threshold cannot enforce
coefficient accuracy when the likelihood is flat in relative shape error.

The shared implementation now scales the mean reduction to prevent sum overflow,
uses compensated reductions, subtracts before dividing, and retains the centered
continuous-mean offset in the free-shape residuals. It always computes that shape,
independently of likelihood improvement. A held mean uses that exact supplied
value; a held shape is preserved exactly. All routes validate finite successful
outputs. The reported likelihood is a compensated sum of the settled scalar
log density at the returned public coefficients; neighboring public means are
compared without replacing the continuous-mean shape target.

For archive cases 3835 and 3838, native shape changes from
`8.235308862764327e23` to `8.23530894536328e23`: relative error falls from
`1.00298548e-8` to zero against the rounded high-precision reference. Double-only
already passed those two cases and remains within `2.23e-16`. Their returned
logLik is `123.89205062899285` before and after. Holding the printed mean
`2.4377209513498226` instead gives the different, correct conditional shape
`8.235308941670034e23`. This distinction is covered by both C++ and R regressions
and explained in the dossier.

## Independent coefficient comparison

The [checker](2026-11-12-invgauss-shape-data/check.py) rebuilds the baseline fitter
with `git show`, without retaining a source-tree archive. It evaluates the direct
closed form independently with mpmath at 160 and 220 digits; the rounded reference
coefficients agree at both precisions. Inputs are exact binary64 observations.
Archive and golden observation order is preserved. Repeated stress observations
are expanded in C++, with exact multiplicities in the independent calculation.

Each build has 760 requests: all 100 archived inverse-Gaussian datasets, the frozen
golden, and 89 deterministic stress datasets, each under all four hold masks.
Thus 570 requests estimate at least one coefficient and 190 evaluate fully held
parameters. Stress sizes are 2, 5, 1,000 and 1,000,000; scales are 1e-300, 1e-150,
1, 1e150, 1e270, 1e300 and 1e307; relative spacings are 2^-52, 1e-12, 1e-6 and
0.25. The two largest scales use spacing 0.25 to keep the continuous shape finite.
An additional held-mean case puts mean 1e300 above data near 1e-300. The appended
million-observation C++ regressions also check cyclic observation order.

Errors below are the maximum relative error over free coefficients, against the
rounded independent references. Misses exceed 1e-10 or have nonfinite coefficients;
fully held values are checked separately for exact preservation.

| Build | Inputs | Requests | Before error | After error | Misses before → after |
|---|---|---:|---:|---:|---:|
| native | archive | 400 | 1e-08 | 2.22e-16 | 12 → 0 |
| native | golden | 4 | 0 | 2.22e-16 | 0 → 0 |
| native | stress | 356 | 0.999 | 2.22e-16 | 36 → 0 |
| double | archive | 400 | 4.28e-09 | 4.44e-16 | 20 → 0 |
| double | golden | 4 | 1.11e-15 | 2.22e-16 | 0 → 0 |
| double | stress | 356 | nonfinite | 5.55e-16 | 21 → 0 |

Before repair, double-only stress produced four nonfinite coefficient vectors:
two numerical-failure statuses and two held-shape core results incorrectly marked
Ok (their common adapter rejected the nonfinite likelihood). The largest finite
stress error was 0.665154. All 760 requests now return finite Ok results in each mode.
No successful case was replaced with a refusal.

| Route, 190 datasets each | Native after | Double-only after |
|---|---:|---:|
| Both free | 2.22e-16 | 5.55e-16 |
| Mean held; shape free | 0 | 4.44e-16 |
| Shape held; mean free | 2.22e-16 | 2.22e-16 |

Every held coordinate is preserved exactly, including the 190 fully held requests
per build. These are finite numerical comparisons, not an outward-rounded universal
floating-point error bound. The exact-arithmetic optimum proof and the measured
floating-point margin are separate evidence. Full inputs, references, statuses and
results are retained in [coefficients.json](2026-11-12-invgauss-shape-data/coefficients.json.gz).

## Completed validation

All runs used the workstation, GCC 15.2.0 and the required CPU locks, with low
priority and BLAS/OpenMP pinned to one thread. Builds/suites reserved four slots;
the independent checker and seam replays reserved one. R was 4.6.1. Simbox was
not used because the final job instruction explicitly says it is off. Double-only
means `BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS`.

| Check | Result |
|---|---|
| `just test-changed` (invgauss and transitive PIG) | 2 C++ shards; R 510 passed, 0 failures/warnings, 8 tier skips; selected JS 208/208; JS build passed |
| Independent before/after closed forms, both modes | 760 requests per build; 0 after misses or stops; exact held values |
| `seam-check.py --family invgauss`, native | 101/101 pass; maximum error 1.644e-16 |
| Same seam replay, `--no-long-double` | 101/101 pass; maximum error 1.648e-16 |
| `just cpp-test` | 108/108 pass |
| `UMVL_TEST_TIER=full just cpp-test` | 135/135 pass |
| `just cpp-test-no-long-double` | 135/135 pass, workstation GCC 15 |
| `just test` | 25,750 passed; 0 failures/warnings; 154 tier skips |
| `just test-full` | 52,508 passed; 0 failures/warnings/skips |
| `just site-check` | Pass: 99 family pages, browser checks, MathML, links, and 99 compiling/linking C++17 snippets |

The final broad suites ran once after the final source change; no late source
fix required a rerun. [Validation metadata](2026-11-12-invgauss-shape-data/validation.json)
records commands, compiler, source hashes and counters; compressed complete logs
are beside it under `logs/`. Seam JSONL, summaries and source-fingerprint provenance
are retained there for both modes.

To reproduce the independent comparison from the repository root, first set the
temporary-directory variables described in the job brief, then run:

```sh
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-12-invgauss-shape-data/check.py
```

For fresh seam outputs, use `dev/validation/seam-check.py --family invgauss --output
<new-path>` through `run`, once normally and once with `--no-long-double`; its
provenance guard requires a fresh output after source changes.

No commits, board edits, dependency installations, golden/truth rewrites, protected
probability-code edits, NEWS/status/known-issue edits, or roadmap changes were made.
No protected probability-code changes are needed. Open items for this job: none.

Artifact delivery limitation: the requested `bin/show` call returned HTTP 403
because this worktree is outside the viewer's configured readable roots. The
report remains at the requested path; displaying it in the pane requires this
worktree to be registered by the viewer. No main-checkout copy or viewer
configuration change was made. This is a display limitation, not a numerical or
validation gap.
