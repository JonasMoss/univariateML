# TASK-80 partial implementation

P1 is incomplete. This worktree is uncommitted and is not ready for review as
the completed phase.

## Changes

- `univariateML/scripts/load_distributions.R` reads `r.shift`;
  `codegen_metadata.R` emits shift mappings and accepts coefficient
  expressions in `r.args`, validating their variable names.
- `univariateML/R/code_helpers.R` evaluates those mappings, applies shifts
  to d/p/q/r, and preserves adapter-provided `restricted` attributes.
  `code_misc.R` and `scripts/goldens.R` translate simulated draws.
- `codegen_wrappers.R` forwards restrictions and distinguishes absent
  required arguments from explicit `NULL`. `code_generics.R` displays fixed
  and restricted coefficients.
- `test_parameter_infrastructure.R` adds synthetic expression/shift and
  restriction-display tests.

## Checks

- `just gen` completed successfully. `git diff --check` passed.
- `just test` reported `[ FAIL 0 | WARN 0 | SKIP 1 | PASS 8774 ]`,
  but an overlapping earlier run wrote to the same log and reported a
  working-directory restoration error. The combined log is
  `/tmp/umvl-p1-test.log`; this is not a clean authoritative test result.
- The previous synthetic probe result was not recovered.
- No numerical goldens were changed or revalidated.

## Outstanding

The schema and 98 front matters, migration script, P0 restriction exemptions,
adapter signature checks, required-default changes, fatigue and tnorm changes,
golden fit arguments, `model_select` skipped attribute, test-switch removal,
and metadata-driven conformance sweep remain to be done. The required-argument
message still uses the old schema and wording. The `simulate` rename is not
implemented. C++ and quality tests were not run. 
