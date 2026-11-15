# Step 5c job 1: rename-only controls (2026-11-15)

Card TASK-150.1.1, consolidation step 5c, job 1 of the
[controls inventory](2026-11-14-controls-inventory.md) (section 5). Decisions:
[decisions.md](../../roadmap/decisions.md), "Controls for 2.0 (step 5c)". The
edits are by a Claude agent; the checks and this report are by the
coordinating session, after the agent stopped while it waited for CPU slots.

## Scope and renames

These are the class A families of the inventory, minus pareto2 and slash,
which TASK-214.27 edits at the same time. No family in scope needed a default
change (class B behaviour), and none is A-grad.

Each control keeps its default value under its new name, in the dossier
`r.control`, `bindings/common/distributions/<id>/fit.hpp`,
`bindings/r/distributions/<id>/adapter.cpp`, the family's R test and, where
it was used, `test.cpp`.

| families | old name | new name |
|---|---|---|
| bbinom, cmb, cmp, dgamma, emg, foldnorm, frechet, gev, gumbel, invtrgamma, kumar, lomax, nsbeta, pareto3, pareto4, pig, sgomp, skellam, sn, st, trgamma, weibull, yulesimon, zeta | `reltol` | `tol` |
| fatigue | `abstol` | `tol` |
| expweibull, gengamma | (already `tol`) | |

The start values are removed, and the default path is the former no-start
path in each family:
- kumar `a0`: removed from the binding and from `mle.hpp` and
  `certificate.hpp`;
- weibull `shape0`: removed from the binding and from `weibull_mle`; callers
  in `weibull/censored.hpp`, `expweibull/mle.hpp` and `expweibull/test.cpp`
  are updated;
- lomax `lambda0`: removed from the binding only.

`lomax_mle` and `lomax_newton_candidate` keep a start parameter because
invpareto, betapr, pareto2 and `lomax/censored.hpp` pass one. `fit.hpp`
passes NaN, so default fits are unchanged.

The tests that exercised the starts now check that the old argument is
rejected (R), or compare fits across tolerances and validate held values
(C++: kumar and weibull, two tests each). No test was deleted.

Outside job 1, three changes were needed to keep the suites green:
- `dev/validation/diagnostics-audit.cpp` (compiled by
  `univariateML-cpp/tests/CMakeLists.txt`): its kumar and weibull route copies
  no longer pass a start;
- `bindings/r/tests/testthat/test_argument_names.R`: `shape0` is now
  expected to be rejected, so the test still checks exact matching;
- JS tests (`bindings/js/test.mjs`, `stop-test.mjs`): calls on renamed
  families pass `tol`. Calls through modifier routes, such as zeta with
  `truncate`, keep `reltol` until job 3. Other families' controls in shared
  test files are unchanged; for example `mlburr(abstol=)` in the pareto4
  test, and `mllomax(z, tol=)` in the invpareto test.

## Snapshot diff

Fit snapshots of the 27 families, in native and double-only builds, were
compared against the
[2026-11-15 baselines](2026-11-15-fit-snapshot-report.md): 3,433 cases per
mode. No estimate, likelihood, outcome or flag changes. Only 64 messages
differ per mode, and only where a message names the control: "positive
`reltol`" becomes "positive `tol`".

## Tests

All on the worktree, through `/tmp/umvl-cpu/run4`:

| suite | result |
|---|---|
| `just cpp-test` | 110/110 |
| `just cpp-test-no-long-double` | 154/154 |
| `just test-changed` (R part) | `[ FAIL 0 \| WARN 0 \| SKIP 157 \| PASS 36337 ]` |
| `just js-build` | passed |
| `just js-test` | 586/586 |

## Left for jobs 2 to 4

- **Job 2.** Rename other families' controls inside shared test files: for
  example `mlinvweibull(reltol=)` in the frechet and invtrgamma tests,
  `mlgamma`/`mlinvgamma(abstol=)`, and `mlburr(abstol=)`. Remove the start
  parameter from `lomax_mle`, `lomax_newton_candidate` and
  `lomax_cert_detail::fit` once invpareto and betapr drop their starts.
- **Job 3.** The modifier routes still take `reltol`; JS and R tests that
  call them (zeta with `truncate`, zib) follow when the routes are renamed.
- **Job 4.** The remaining copies in `diagnostics-audit.cpp`, the JS
  `fit-test.mjs` generic loop (which passes `gradtol` and `reltol` to every
  family), and the docs.
