# CMP work ledger — TASK-146

Completed on branch `codex/cmp-ledger`, based on
`d34a44cd1b0d7324e1f486d66e6b3f7e3db9210a`. All required numerical, binding and site validation
commands finished successfully. No commits, board edits, package installations,
default-control changes, or golden/truth changes were made.

## Implementation

`special/cmp.hpp` now provides `CmpWorkLedger`, a view of a caller-owned
`std::uint64_t` counter and allowance. Each direct summation term spends one
unit; Euler–Maclaurin quadrature spends its 4096 evaluation units. Failed
quadrature work remains charged when direct summation follows. A child receives
the remaining allowance without reserving a per-call slice or charging its work
twice. The moment and fitter work fields are also 64-bit on WASM.

Standalone CMP fitting owns an enumeration allowance of `100000 * I`, where
`I = max(0, min(maxit, 100000))`. Both nested mean solves and the outer solver
use the saturated solver control. The family entry point shares the same
ledger through proposal and acceptance. Both CMB delegation paths—the free
dispersion CMP fit and the held dispersion mean match—spend their parent's
existing ledger. The reservations that shrank as `iterlim` increased are gone.

Raw fitting exhaustion returns `CmpStatus::BudgetLimit` and retains available
completed candidates, likelihood bounds and `budget_exhausted`. Before a
completed fitting candidate exists, proper Poisson or geometric members supply
an analytic witness when compatible with the holds. A stopped fit has an
unresolved global likelihood upper bound of infinity and never becomes a
successful estimate. The existing domain failure envelope retains its structured
budget flag; the shared binding returns a budget condition with “Raising
iterlim may resolve this stop.”

Numerical evaluator refusals use `CmpStatus::NumericalFailure`;
The review removed the former `WorkLimit` name rather than keeping it as an alias. The
mode-above-9e15 refusal is numerical, including with an empty ledger. CMB's
binding now adds the iterlim advice only when its budget flag is set.

Pure CMP evaluation retains a fixed one-million-unit cap and maps exhaustion
to numerical failure. `cmp/dpqr.hpp` is unchanged. Fully held CMP calls in the
shared binding use that fixed allowance as well, covering the diagnostic
fallback from R's pure-evaluation constructor. R and JS therefore agree even
when fully held evaluation exhausts its cap at `iterlim = 1`.

Skellam's upper positive Bessel series has a one-million-term structural guard
and rejects nonfinite terms or totals with NaN. Its recurrence, addition order,
lower sum, approximation formulas and tail bounds are unchanged. The
[work-budget policy](../../design/work-budgets.md) now states the evaluator
exception and links this report. No older report was rewritten.

## Evidence and mathematical scope

The accounting invariant follows directly from checking a charge against
`limit - used` before addition: starting within the allowance, the counter
cannot overflow or exceed it. The largest standalone allowance is
10,000,000,000 units. This is an integer accounting argument, independent of
floating-point precision.

No summation formula or tail-bound formula changed. Existing truncation and
acceptance certificates retain their scope: exact-arithmetic guarantees
conditional on the computed comparisons, without an outward-rounded
floating-point proof. Candidate likelihood bands use the existing practical
margin `1e-9 * (n + abs(logLik))`. Regression tests and truth fixtures provide
numerical implementation evidence; they do not establish a new existence or
global-optimality theorem.

New C++ tests exercise consumption across calls above the 32-bit counter range,
an exhausted ledger with a feasible witness, fixed-cap numerical exhaustion,
huge-mode numerical refusal, solver saturation, identical CMB answers at
`iterlim = 100` and `2000`, and the Skellam nonfinite-series guard. R and JS
regressions cover the CMB comparison, budget diagnostics and huge-mode
classification; both also check fully held CMP exhaustion. Existing CMP budget
regressions retain a witness and recover when iterlim is raised.

All **297 golden.json, truth.json and truth.py files** across the 99 families
remain byte-for-byte identical to the base revision. Their hashes are retained
in [frozen-fixtures.json](2026-11-07-cmp-ledger-data/frozen-fixtures.json).

## Paired timings

The retained [harness](2026-11-07-cmp-ledger-data/timing.cpp) calls the shared
native bindings on the 500-observation golden data. The
[launcher](2026-11-07-cmp-ledger-data/timing.py) reconstructs baseline headers
from the pinned base revision and compiles both versions with C++17 and `-O2`.
Measurements ran on simbox under its nice/single-thread wrapper, using GCC
13.3. Values below are medians of three calls, excluding compilation and data
loading. CMP uses its default iterlim 100; CMB uses its unchanged default 2000.

| Workload | Before | After | Outcome in both versions |
|---|---:|---:|---|
| CMP golden fit | 2.389 ms | 2.228 ms | fit, identical log likelihood |
| CMB golden fit, size held at 10 | 0.362 ms | 0.336 ms | fit, identical log likelihood |
| CMB free size on golden data | 6.233 ms | 6.290 ms | fit, identical log likelihood |
| TASK-144 census construction: CMB, nu held at its golden estimate, size/prob free | 15.678 s | 13.066 s | budget stop |

The machine was shared, so these measurements are descriptive rather than a
controlled speedup claim. The held-nu workload remains a budget stop in both
versions. Complete observations and medians are in
[timing-paired.tsv](2026-11-07-cmp-ledger-data/timing-paired.tsv) and
[timing-summary.json](2026-11-07-cmp-ledger-data/timing-summary.json).

## Final validation

| Command | Machine | Result |
|---|---|---|
| `just cpp-test` (dev) | simbox, GCC 13.3 | 106/106 shards passed |
| `UMVL_TEST_TIER=full just cpp-test` | simbox, GCC 13.3 | 129/129 shards passed |
| `just cpp-test-no-long-double` | simbox, GCC 13.3 | 129/129 shards passed |
| `just cpp-test-no-long-double` | workstation, GCC 15.2 | 129/129 shards passed |
| `just test` | simbox | FAIL 0, WARN 0, SKIP 154, PASS 21371 |
| `just test-full` | simbox | FAIL 0, WARN 0, SKIP 0, PASS 44189 |
| `just js-build` and `just js-test` | workstation | build succeeded; 481/481 tests passed |
| `just site-check` | workstation | passed, including 99 compiled/linked C++17 snippets and browser checks |

The final R runs include the fully held binding correction. The C++ core was
unchanged after its final suites. The site check refreshed an existing tracked
screenshot; it was restored byte-for-byte afterward. All source hashes were
rechecked against the [source manifest](2026-11-07-cmp-ledger-data/source-manifest.json).

Machine-readable results are in
[validation.json](2026-11-07-cmp-ledger-data/validation.json). Compressed complete
logs are retained for [simbox C++](2026-11-07-cmp-ledger-data/final-simbox-green.log.gz),
[final R](2026-11-07-cmp-ledger-data/final-bindings-r.log.gz), and
[workstation validation](2026-11-07-cmp-ledger-data/final-workstation.log.gz).

The focused Markdown check passed: 2 changed files, 13 local destinations,
zero broken links. The repository-wide Markdown checker reported four pre-existing findings in
files byte-identical to the base revision:

- `dev/research/evaluations/2026-11-03-boundary-lit-report.md:5` refers to the
  absent generated `dev/research/boundary-points.pdf`.
- `nbinom/README.md:234` has the mathematical expression `1+m/r` recognized as
  a Markdown destination by the checker.
- `univariateML-cpp/notes/inference.md:3` and `:95` use the site-root URL
  `/inference/`, which the checker treats as a repository file.

Open numerical or implementation items: none. Documentation open: those four
repository-wide findings require the missing generated PDF and/or changes to
link handling or source outside this job's permitted scope. The job's changed
Markdown is checked separately using the same parser; results and byte-level
baseline evidence are retained in
[changed-markdown-links.json](2026-11-07-cmp-ledger-data/changed-markdown-links.json).

Presentation open: the artifact viewer rejected this worktree with HTTP 403
because it is outside every registered readable root. The `show` helper has no
per-file root-registration option; the viewer requires the worktree to be
registered through an agent thread or `CODEX_MATH_WEB_VIEW_ROOTS`. No file was
copied into another checkout and no viewer configuration was changed.
