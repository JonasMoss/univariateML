# TASK-239 — Model selection failure conditions

Worktree `codex/select-cond`, based on `eaea17a1`.

## Condition

When no candidate can be fitted, `model_select()` signals an error with the
ordered classes `univariateML_select_failed`, `univariateML_stop`, `error`,
and `condition`. The condition has a scalar `message`, a `NULL` `call`, and
`kind = "select_failed"`. The selector constructs the condition using the
same list-and-class convention as the existing R stops.
[`code_model_select.R:186`](../../../bindings/r/R/code_model_select.R#L186)

The `failures` list uses fitter names such as `mlnorm`, in candidate order.
When fitting was attempted, `reason = "all-fits-failed"` and each attempted
fitter's entry is its original condition. The aggregate keeps the existing
attempted-fit diagnostics and includes the reasons for any candidates skipped
before fitting. When no fit was attempted, `reason = "no-eligible-models"`.
Skipped entries are diagnostic strings naming the missing coefficients or
describing the unavailable censoring route. The missing-coefficients heading
remains in that case. Filtering out every candidate produces an empty list.
[`code_model_select.R:100`](../../../bindings/r/R/code_model_select.R#L100),
[`code_model_select.R:130`](../../../bindings/r/R/code_model_select.R#L130)

When at least one fit succeeds, the selector drops failed fits without issuing
a warning of its own. It preserves the existing fitter warnings and the
`skipped` attribute, including the attempted-fit diagnostics collected there
for censored data. There is no selector warning to attach failures to.
[`code_model_select.R:127`](../../../bindings/r/R/code_model_select.R#L127)

Both generated R help topics describe the condition and its payload.
[`code_model_select.R:18`](../../../bindings/r/R/code_model_select.R#L18),
[`outcomes.R:29`](../../../bindings/r/R/outcomes.R#L29)

## Verification

The R tests check the exact aggregate class vector and handler dispatch, the
two original `nomle` conditions with reason `unbounded` on constant data,
the per-model messages, missing required coefficients, a combined skip and
failure, and an empty type filter. The mixed Normal/Exponential case checks
silent selection in both return modes. The existing message assertions ran
unchanged. [`test_model_select.R:78`](../../../bindings/r/tests/testthat/test_model_select.R#L78)

`/tmp/umvl-cpu/run4 nice -n 10 just test-changed --no-long-double` exited 0.
The recipe classified the R changes as shared and selected all 86 families.
It ran the following checks sequentially on the workstation.

| Recipe step | Result |
|---|---|
| `just cpp-test` | 136/136 passed, GCC 15.2 |
| `just cpp-test-no-long-double` | 181/181 passed, GCC 15.2 |
| `just test` (R dev tier) | 37,793 passed, 157 skipped, zero failures and warnings |
| `just js-build` | Passed |
| `just js-test` | Passed |

The runner selected families, so the empty-selection fallback to
`just test-full` did not apply. The documentation changes were in R help
only, so the site check did not apply.

## Open

None.
