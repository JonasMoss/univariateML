# TASK-155: discrete quantiles above 2^53

The shared inversion, family routes, truth consumers, inverse samplers and
requested documentation are updated. All final C++, R and JS test runs passed. Truth tables,
truth generators and goldens are byte-identical to base `6977702d`. No commits,
board edits, package dependency installs or publishing actions were performed.

## Implementation

The shared bisection returns its checked upper endpoint when its bounds are
adjacent doubles. It retains endpoint checks, failed-CDF propagation, safe
midpoints, strict-progress checks and the exponent-derived work caps. Signed
expansion advances from the current bounds, including when the step is smaller
than one ULP or reaches `DBL_MAX`.

Above the exact integer range, callers compare the supplied probability in its
selected tail and scale. This covers the shared log-tail adapter and the direct
routes for discrete Laplace, Skellam, ZIB, CMP and truncated counts. The ordinary
integer comparisons and their established jump conventions remain in place.

Discretized normal, discretized gamma and discrete Weibull route large targets
through guarded inversion. Normal and gamma use it when a bounded seed
correction cannot certify the answer. Weibull checks the CDF at `2^53` so a seed
rounded below that boundary cannot bypass inversion of a larger target. The
geometric, discrete-uniform and Weibull inverse samplers also use guarded
inversion for large draws while retaining their ordinary draw streams.

The C++, R and JS truth consumers retain their oracle accuracy/bracket checks
and additionally require the predecessor property for large targets:

| Selected tail | At returned `h` | At previous double |
|---|---|---|
| Lower | `F(h) >= p` | `F(previous) < p` |
| Upper | `S(h) <= p` | `S(previous) > p` |

The same inequalities apply to log inputs. NaN exemptions were removed. R and
JS regressions draw 4,096 observations from zeta (`s = 1.1`) and Yule–Simon
(`rho = 0.1`), require finite draws including values above `2^53`, and compare
all draws exactly with quantiles of replayed uniforms. The generic R sampler
checks reject missing/NaN draws and retain support checks.

The four requested documentation locations are updated: the tails header,
R roxygen Details, JS README and decision log. Local R help was regenerated
and contains the requested text.

## What is certified

Conditional on the computed comparisons, a monotone CDF preserves the bracket
invariant: the lower bound misses the target and the upper bound reaches it.
Unit width certifies an ordinary integer answer; adjacent doubles certify the
first representable crossing. For signed supports, the predecessor is always
toward negative infinity and the exact integer range is measured by magnitude.

In exact arithmetic with accurate CDF comparisons, upward rounding of a finite
integer quantile above `2^53` has relative representation error at most
`2^-52`. The new property checks use strict comparisons without a tolerance
that accepts a neighboring double. Existing oracle tolerances remain in force.
CDF evaluation accuracy is a separate premise; the conditioning finding below
shows why this distinction matters.

### Overflow beyond the finite range

The frozen tables contain **105 finite large targets and 37 targets beyond
`DBL_MAX`**. For the latter, no finite double can satisfy the requested rule.
The implementation returns `+Inf` only after checking that `DBL_MAX` misses
the target and `+Inf` reaches it. Consumers verify both comparisons and require
a finite answer for finite oracle targets. A failed CDF evaluation still
returns NaN.

This range extension is necessary to reconcile the smallest-representable-value
rule with the instruction reserving NaN for invalid inputs or failed CDFs.
For example, for zeta with `s = 2`, the integral bound gives
`S(DBL_MAX) > 1 / (2 * (DBL_MAX + 1)) > exp(-1000)`; its upper log-probability
`-1000` therefore has no finite representable crossing. The `2^-52` finite
representation bound does not apply to overflow.

## Final validation

All runs completed in the foreground through the supplied CPU helpers, with
thread limits and lowered priority. Simbox C++ builds used GCC 13; workstation
C++ builds used GCC 15. R tiers used one worker on simbox, with the same
package generation and test runner as `just test` / `just test-full`.

| Machine | Check | Result |
|---|---|---|
| Workstation | `just cpp-test` | 106/106 shards |
| Workstation | `just cpp-test-no-long-double` | 129/129 shards |
| Simbox | `UMVL_TEST_TIER=full just cpp-test` | 129/129 shards |
| Simbox | `just cpp-test-no-long-double` | 129/129 shards |
| Workstation | `just js-build`, `just js-test` | 549/549 tests |
| Simbox | R development tier | 22,082 passes; 0 failures/warnings; 154 tier skips |
| Simbox | R full tier | 44,914 passes; 0 failures/warnings/skips |
| Workstation | All large frozen quantile targets | 142/142 predecessor checks |
| Workstation | `just regen` | Passed; generated help inspected |
| Workstation | Changed Markdown links | 3 files; 10 local destinations; 0 broken |

Regression coverage includes adjacency at `2^53`, negative large values,
overflowing bracket widths, failed evaluations, crossing zero from extreme
centres, tiny initial expansion steps, geometric tails, analytic-seed boundary
classification, and checked overflow. Earlier runs exposed legacy NaN
expectations and two R expectations; these were corrected before the final
runs. Fitter implementations and frozen fixtures were unchanged, and their
regressions passed.

Reproducible evidence:

- [Validation summary](2026-11-08-qdiscrete-round-data/validation-summary.json).
- [Large-target checker](2026-11-08-qdiscrete-round-data/check_finite_truth.py):
  run with `UMVL_INCLUDE_OVERFLOW=1` through `/tmp/umvl-cpu/run` to include all
  142 targets; the default checks the 105 finite targets.
- [Changed-Markdown link checker](2026-11-08-qdiscrete-round-data/check_report_links.py).
- [R tier launcher](2026-11-08-qdiscrete-round-data/run-r-tiers.sh): run through
  `simbox-run` with arguments `dev full`.

## Open findings

**Contract wording for overflow.** The task's unconditional requirement that
all large interior targets return finite values is impossible for the 37
frozen overflow targets. The concrete implementation and tests use the checked
`+Inf` extension above. What remains is author confirmation that the wording
should require finiteness whenever a finite crossing exists.

**Pre-existing discretized-normal CDF conditioning.** The brief leaves settled
CDF changes for reporting when they lie outside the inversion work. A separate
diagnostic confirms that `pdnorm` can overflow the subtraction in
`(floor(q) + 1 - mean) / sd` for extreme translated parameters. With
`mean = sd = 1e308` and target `.01`, inversion finds:

```text
h                     = -7.976931348623157e307
previous              = -7.976931348623158e307
computed F(h)          = 0.036112824971552479
computed F(previous)   = 0
stable F(h)            = 0.0361128249715525
stable F(previous)     = 0.0361128249715525
```

The computed predecessor property passes, while a stable standardization shows
that this is not the first crossing of the model's actual CDF. Consequently,
the representation guarantee is conditional on accurate CDF comparisons; the
new property checks do not independently establish that premise. Missing work
is range-safe discretized-normal standardization, retaining the integer `+1`
increment, and corresponding extreme-parameter CDF regressions. That CDF
implementation was left unchanged under the brief's scope rule.

The [diagnostic source](2026-11-08-qdiscrete-round-data/dnorm-cdf-conditioning.cpp)
and [recorded output](2026-11-08-qdiscrete-round-data/dnorm-cdf-conditioning.txt)
make this finding reproducible. Its reference evaluates the standard normal
CDF with `erfc` after scaling the operands before subtraction.

**Pre-existing repository link findings.** The whole-repository Markdown
checker reports six findings in four files unchanged from `6977702d`:

```text
dev/research/evaluations/2026-10-31-inference-6-report.md:133  desktop.png under build/scratch
dev/research/evaluations/2026-10-31-inference-6-report.md:135  mobile-evidence.png under build/scratch
dev/research/evaluations/2026-11-03-boundary-lit-report.md:5  boundary-points.pdf
univariateML-cpp/include/cppunivariate/distributions/nbinom/README.md:234  1+m/r
univariateML-cpp/notes/inference.md:3  /inference/
univariateML-cpp/notes/inference.md:95  /inference/
```

The missing old artifacts and the checker's handling of mathematical/site-route
references require separate cleanup. This job's changed Markdown passes the
same parser's targeted check. No unrelated source files were changed to hide
these findings.

**Artifact pane configuration.** The requested `show` command returned HTTP
403 because this worktree is outside the viewer's configured readable roots.
The helper supports only file-path display. Missing work is viewer registration
of this worktree; no global viewer configuration or other worktree was changed.
The report and all linked evidence remain in this worktree.
