# Quality-harness cleanup after the modifier consolidation (TASK-222)

Layout-only. Python harness files only; no numerical or behavioural change for any existing family or route.

## Hits under `dev/validation` and `dev/tools`

The grep was word-bounded over `*.py`. `zip` is also the Python builtin; those hits (`zip(...)`) are ignored.

| file | hit | class | why |
|---|---|---|---|
| `diagnostics.py` | `id=='tpois'`, `id=='tbinom'` | dead, removed | `id` comes from existing family folders; neither exists |
| `seam_reference.py` | `ident=='tbinom'`, `'tpois'` (twice), `'tnorm'` | dead, removed | `ident` is a family folder name; the only other caller, `fit-density.py attribute()`, returns early when `DISTS/law/README.md` is missing, which is the case for every modifier law |
| `quality/reference.py` | `('binom','ztbinom','zib','zmbinom')` | dead in part, removed | now `id == 'binom'` |
| `quality/reference.py` | `'zipf'` in the closed-form tuple and the `N` branch | kept | `fit-refine.py` calls `reference()` on a shadow folder named after the route law (`zipf`); `test_reference.py` also uses it |
| `quality/reference_integer.py` | `zib`, `zmbinom`, `ztbinom` branches | dead, removed | only caller passes `binom`; other ids now raise |
| `fit-snapshot.py`, `fit-density.py`, `fit-refine.py` | `tpois tbinom tnorm zipf zmlogarithmic` | kept | route identifiers of `ROUTES` / modifier laws (windows, `theta` coefficient name) |
| `cert_replay_faces.py`, `cert_replay_roots.py` | `zipf`, `zip`, `tnorm`, `ztpois, zmpois`, `zmlogarithmic` | kept | certificate-replay route names and claim keys |
| `quality/relations.py`, `quality/test_relations.py` | `tnorm`, `zipf`, `zmlogarithmic`, `zm*`, `zt*`, `tpois`... | kept | kernel names of configured/private laws and the relation graph, still read by the tests (the tests pass on them) |
| `quality/test_truth.py` | `ztpois`, `tpois` | kept | fixtures under `univariateML-cpp/tests/modifiers/fixtures` |
| `quality/test_reference.py` | `zipf` | kept | test of the live `zipf` closed-form route; tests are not touched |
| `inference-numeric-special.py`, `inference-numeric-registry.py`, `inference-numeric-mc.py` | family lists and per-family branches (`tnorm tbinom tpois zmbinom zip zib zinb zmpois zmgeom zmlogarithmic zmnbinom ztnbinom ztbinom zipf`) | unclear, left | one-shot inference experiment drivers with hard-coded family lists; they are broken for the removed folders as a whole, not by a single branch. Retiring or porting them is a separate decision (they back dated evaluation reports) |
| `quality/truth_report.py` | text containing "zip" | not a hit | prose / builtin |
| other `zip(` hits | builtin | ignored | |

`dev/tools/*.py` has no hit besides the builtin; the `retirement` citation checks in `check_links.py` use no removed-id code branch.

## Removed

- `diagnostics.py`: the `tpois` coefficient override and the `tbinom` route rewrite.
- `seam_reference.py`: the `tbinom` and `tpois` precondition block, the `tpois` density, the `tnorm` density (the remaining chain starts at `bhatt`).
- `quality/reference.py`: `ztbinom`, `zib`, `zmbinom` from the integer-enumeration condition.
- `quality/reference_integer.py`: the `zib`/`zmbinom`/`ztbinom` branches; the `binom` path is unchanged (`face`, `profile`). Other ids raise `ValueError`. The now-unused first `lam`/`face` computation was left in place to keep the code path (including its division behaviour) identical.

## Regeneration test

`quality/truth_metadata.py` gains `canonical_payload(table)`; `quality/test_truth_metadata.py` (`RegenerationTests`, numeric mode) now compares `canonical_payload(fresh)` with `canonical_payload(stored)` through `payload_close`. The canonical form replaces every point index (in `normalization`, `values`, `quantiles`, `sampler_points`, `historical_sampler_points`) by the point's parameter vector, and sorts the points and rows as multisets keyed by inputs only: the point vector for `normalization`; vector and `x` for `values`; vector, lower, logp, input for `quantiles`. Output cells never enter the key, so last-digit drift cannot reorder rows; nothing is dropped, so a changed, missing or extra row, or a changed sampler slot, still fails. The `bytes` mode is untouched. A new test, `test_canonical_payload_ignores_point_and_row_order_only`, reverses the points and rows of the stored `exp` table (remapping indices) and requires equality, then checks six mutations (a changed value, a missing row, an extra row, a changed parameter, a changed point reference, swapped sampler slots) each fail.

## Checks

All through `/tmp/umvl-cpu/run nice -n 10`, one thread.

- `just truth-check`: 36 tests, 1 failure, `test_stored_fingerprints_and_no_timings` (`rayleigh/truth.json` `input_sha256` mismatch). Identical on unchanged master (35 tests, same single failure); not caused by this change.
- `python3 -m unittest discover -s quality -t .` (the Justfile has no discover recipe): 108 tests, 1 failure and 1 error: the same fingerprint test and `test_relations.ImmediateDeclarationTest.test_cli_fails_even_when_density_checks_would_pass`. Both are identical on master (107 tests); the new test passes.
- `py_compile` on all six touched files: OK.
- `check_links.py`: exit 0.
- Fit baselines, with `--known dev/validation/strict-known.json`, no `--add`/`--prune`: `fit-consistency` exit 0 (80 known, 0 new); `fit-density` exit 0 (0 failures); `fit-judge` exit 1 with 16 known and 1 new, `mode|both|gompertz|class|huge: native ok, double-only limit`. The `fit-judge` output is identical on unchanged master, so it is not caused by this change; it needs a decision on that entry (a `strict-known.json` change is outside this card).

## Files changed

- `dev/validation/diagnostics.py`
- `dev/validation/seam_reference.py`
- `dev/validation/quality/reference.py`
- `dev/validation/quality/reference_integer.py`
- `dev/validation/quality/truth_metadata.py`
- `dev/validation/quality/test_truth_metadata.py`
- `dev/research/evaluations/2026-11-15-quality-cleanup-report.md`
