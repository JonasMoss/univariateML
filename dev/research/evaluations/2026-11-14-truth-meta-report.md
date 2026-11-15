# TASK-188: truth tooling metadata, fingerprint policy, threshold alignment

Baseline master 7f12c88d, branch `claude/truth-meta`, nothing committed. Data and scripts:
[2026-11-14-truth-meta-data](2026-11-14-truth-meta-data/).

## 1. Hard-coded counts removed

| Where | Was | Now |
|---|---|---|
| `test_truth_metadata.py` `test_stored_fingerprints_and_no_timings` | 98 tables | table set equals the set of family folders (README.md), non-empty |
| `test_truth.py` `test_all_tables_select_public_default_and_frozen_golden_fit` (second catalogue count) | 98 folders | same derivation: folders with README equal folders with truth.json |
| `test_relations.py`, two R scripts (`load_distributions(root)==98`) | 98 | folders minus staged (`stage: dpqr`, i.e. pgw) families, which the loader skips; derived in R |
| `test_truth.py` `test_default_is_retained_when_stress_grid_reaches_cap` (grid 9 vs 17) | `len(pts)==9` | derived: `len(historical_points) + extreme points not already present`; historical part must be a prefix; the cap is checked as `<= 9` (8 plus the retained default) |
| `truth_report.py` | "/98 families" in generated text | count of README folders; also its text now says natural probabilities are checked in the normal range |
| `test_cmb_workload_keeps_both_support_endpoints` and `truth_workload_exclusion` hook in `truth.py` | cmb route hook | **removed**: no route defines the hook any more (cmb's `truth.json` has no excluded rows), so the test checked something that no longer exists; the dead `hasattr` branch in `apply_exclusions` and the README sentence about the CMB workload filter are removed too |

References to the 16 removed families in the truth tooling (`truth*.py`, `test_truth*.py`, family `truth.py`/`truth.json`): none. They remain only in the fit-oracle harness
`dev/validation/quality/reference.py`, `reference_integer.py` and in `dev/validation/fit-*.py`, `diagnostics.py` (dead branches, outside the truth tooling; open, see below).

## 2. Fingerprint policy

Chosen (recorded in `dev/validation/README.md#truth-table-fingerprint-policy`): `input_sha256` covers the table's own inputs (dossier log density, support, parameter specs, R args, golden estimates/args, family `truth.py` and `spec.py`) plus `GENERATOR_VERSION` (new constant in `truth.py`, bumped by hand when a shared-code change alters numbers). Shared sources (`truth.py`, `density.py`, `truth_reference.py`) are no longer hashed.

Alternatives: (1) hash everything (old; any comment edit stales all tables); (2) own inputs only, no version (a number-changing shared edit goes unnoticed); (3) own inputs plus version plus spot regeneration (chosen); (4) regenerate and compare all tables in the full tier (strongest, hours of mpmath).

Safety net implemented: `RegenerationTests` (`test_truth_metadata.py`) regenerates `exp` and `unif`; run through `just truth-check`, which is now a prerequisite of `just test-full`; skipped otherwise (`UMVL_TRUTH_REGEN=1`).

**The "byte-for-byte" premise does not hold today.** I regenerated `exp`, `unif`, `hyper`, `invexp`, `cauchy` with the current generator (and `exp` with the generator at c4005752 against its table at that commit): none is byte-identical. Causes: the stored tables predate the golden refresh of 2026-11-09 (the fitted sampler point differs in the last digits, e.g. exp rate 0.9568054574511545 vs golden ...549), and an older rounding of near-zero log tails (0.0 where the current generator gives -1e-100). Compared numerically (rtol 1e-9, atol 1e-12, `payload_close` in `truth_metadata.py`): exp, unif, invexp, cauchy agree; **`hyper` has 50 real differences** (e.g. quantile row 332: -69.08 vs -27.63, 4.0 vs 1.0). So the default regeneration check is numeric, and `UMVL_TRUTH_REGEN=bytes` demands byte equality, to become the steady state after a deliberate full regeneration. Payloads were not regenerated (task rule).

## 3. Fingerprints refreshed, payloads untouched

`python3 -m quality.truth_metadata` rewrote only `input_sha256` in 86 tables (all families; gengamma included, formerly deliberately stale). Verification against master, metadata fields removed, text and parsed JSON (`verify_payload.py`):

```
86 tables; fingerprint-only changes in 86, untouched files 0; payload mismatches/new 0
```
`git diff --stat` on the tables: 86 files, 86 insertions, 86 deletions (one line each).

## 4. Threshold alignment (TASK-199)

Consumers now check natural-scale probabilities when `exp(expected) >= 2.2250738585072014e-308` instead of `> 1e-300`: C++ `univariateML-cpp/tests/test_support.hpp`, R `bindings/r/tests/testthat/test-generated-dpqr.R`, JS `bindings/js/test.mjs` (tests only; no sources).

Result: **one failure, a finding.** `sged`, truth point 7: lower tail at x=-1.81893 and upper tail at x=2.54802, natural probability about 1e-300 (normal range): the sged probability function returns NaN (log scale is fine). Nothing loosened, `dpqr.hpp` untouched. R `just test` passes (the R run does not exercise point 7 at these rows or sged is handled there; not investigated). JS: see test runs.

## 5. Regeneration of the drifted tables (second session)

Of the 74 tables whose fitted sampler point differed from the frozen golden, 72 were regenerated earlier (whole table, or for beta, betapr, nsbeta, pig and sged the fitted point appended with `refresh_fit_point.py`, existing rows byte-identical). This session: `sged` done with `refresh_fit_point.py` (appended as point 24, verified). `golden_drift.py` now reports one table: **`st`** (6.05e-9), whose generator raises `ArithmeticError: skew-t angular quadrature/remainder bound` (the 1e-40 quadrature gate in `st/truth.py`) while bracketing a quantile at the fitted point (xi=-0.0365, omega=0.9204, alpha=0.1087, nu=4.8315). Not regenerated; the gate was not loosened.

**hyper verdict: the stored table was right and so is the regeneration.** The 50 "differences" are a row-order difference only: stored and regenerated `quantiles` are the same multiset of 352 rows (checked with `Counter`), and `values`/normalization are identical. All 336 finite quantile rows of the regenerated table agree with an exact check (`fractions` pmf sums for the cdf and survival function, mpmath at 60 digits for the comparison with exp(logp)); 0 mismatches. So no generator numerical bug; the generator emits the extreme-parameter (N=1e8) rows in a different order than when the table was written (likely set-ordering of the point or probability grid; not traced). The regenerated hyper table is kept (24 changed lines, row order only). `payload_close` compares positionally, so the numeric regeneration test would flag a pure reordering: if hyper is ever added to `RegenerationTests`, compare sorted rows.

## 6. Test runs (workstation; simbox off)

- `just truth-check`: 35 tests, one failure: `test_all_tables_select_public_default_and_frozen_golden_fit` for `st` (above). All else passes.
- `just cpp-test` (run4): 109/110, only `distribution_sged` (point 7, lower x=-1.81893 and upper x=2.54802: ours NaN, truth 1e-300). `just cpp-test-no-long-double`: 153/154, same single failure at the same rows.
- `just test` (R): `[ FAIL 0 | WARN 0 | SKIP 157 | PASS 36193 ]`.
- `just js-build` ok; `just js-test`: 582 pass, 1 fail: `sged generated truth and sampler` (same finding, TASK-220).
- `python3 dev/tools/check_links.py`: see below.

## Open entries

1. `st`: truth table cannot be regenerated at the current golden fit (quadrature gate, above); its sampler point stays 6e-9 off the golden, and `truth-check` fails on it until the `st/truth.py` gate or the quantile bracketing is fixed.
2. (resolved) hyper: no discrepancy, see section 5.
3. `sged` natural tails near 1e-300 return NaN (TASK-220); needs a `dpqr.hpp` fix.
4. Removed-family branches remain in `quality/reference*.py`, `fit-*.py`, `diagnostics.py` (outside truth tooling).
5. The `bytes` mode of the regeneration test and the version bump rule need a full regeneration (with st) to become meaningful.
