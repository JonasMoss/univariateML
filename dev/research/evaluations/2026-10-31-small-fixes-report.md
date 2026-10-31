# Small fixes — 2026-10-31

Source: `e0433d4cde9fbd8c3b6ef29a24a6b8607d992b68` plus this uncommitted worktree.
All computations ran locally under `/tmp/umvl-cpu/run` or `run4`, with
BLAS/OpenMP pinned to one thread and the lock launcher's `nice -n 10`.
Temporary files and logs are in `build/`; no adapters, goldens or d/p/q/r changed.
Environment: Linux x86_64, GNU C++ 15.2.0 (C++17), R 4.6.1, roxygen2 8.0.0, Python 3.14.5.
Oracle/quality: mpmath 1.3.0, SymPy 1.14.0, PyYAML 6.0.3, NumPy 2.4.6, SciPy 1.18.0.
Existing numerical tolerances/seeds were retained; new status checks are exact.

## TASK-128 — exponential status

The free-rate exponential fit now reports `Unbounded` when the sample sum is
zero, preserving its infinite rate and log likelihood. The NaN-held-rate
delegate preserves this status; a held positive rate still evaluates normally.
The unchanged R adapter therefore retains its existing all-zero diagnostic.
The appended C++ regression checks these outputs and the held-rate likelihood.
Both new regressions passed; the existing R all-zero diagnostic test passed.

A source scan of all 98 `mle.hpp` files found the same clear nonfinite-estimate
case in the free Pareto fit on constant positive data. Its infinite shape and
log likelihood now carry `Unbounded`; an appended regression checks this.
These are analytic conclusions: the respective profiles are
`n log(rate) - rate sum(x)` and `n log(shape) - shape sum(log(x/min(x)))`
(up to constants). A zero sum leaves a strictly increasing, unbounded profile.
The existing exp and Pareto dossiers already document these conditions.

Nearby findings outside the requested nonfinite-estimate case: lognormal's
free fit drops the normal delegate's status on constant positive data, returning
finite `sdlog = 0` and infinite log likelihood with `Ok`; logit-normal returns
the analogous finite zero scale and has no status member. Empty-input default
returns also omit statuses in several older fitters (including exp, Pareto,
lognormal and invexp); invalid-data returns in invexp and Pareto also default
to `Ok`. These remain unchanged; this scan is not an exhaustive
numerical audit of every held subset or floating-point extreme.

## TASK-126 — reproducible truth metadata

`input_sha256` replaces the whole-README `density_sha256`. Its versioned,
canonical JSON input includes the log density, support, parameter names/domains
and all point-selection fields, R argument mappings, golden estimates/fixed
sets/fit and sampler arguments, local `truth.py` (and `spec.py` when selected),
and the shared generator, declarative density reader and reference code.
Dossier prose, references and presentation fields are excluded. External Python
library versions are run-environment information rather than source inputs.

Per-point elapsed times were removed from tables, and elapsed generation times
from historical family status JSONs. New status/timing logs and truth-report
outputs default to ignored `build/truth-audit/`. Existing historical benchmark
reports remain historical evidence. C++ `test_support.hpp`, R
`test-generated-dpqr.R`, quality and site consumers do not read the old hash or
per-point times, so their numerical readers require no changes.

`PYTHONPATH=dev/validation python3 -m quality.truth_metadata` migrates metadata
without evaluating or reserializing numerical rows. Comparing all 98 files
against `git show HEAD:<path>` after filtering only hash/timing metadata proved
that every other byte was preserved. Aggregate SHA-256 before and after:
`dbe98c35810728cfe7a6c2f17fdb991072a361bd18fab1e28bcb602b749a2d1d`.
The migration also checks parsed payload equality. Regression tests check all
98 fingerprints, absent timings, dependency changes and prose independence.

Metadata/oracle unit checks: `PYTHONPATH=dev/validation python3 -m unittest
quality.test_truth_metadata quality.test_truth quality.test_truth_gaps`: **33 passed**.
`just dpqr-truth exp norm geom` regenerated all three successfully; `cmp`
confirmed each whole file was byte-identical to its migrated snapshot:

- exp: `9e716df15834f01f2f300384bfd3ccdb536003f2f4a381cf3a13f9b112dd5408`
- norm: `c3cabdef4e15f70d21d5040c266d73bc0dfd4546d9055dc69773f75a41e0a2b5`
- geom: `b77d79d51802cd633ed04721331223fcf7b5dae2fc6e56e3b4c3f471c9939ee8`

Repeating the 98-table byte comparison after regeneration gave the same
aggregate payload hash above. Logs: `build/scratch/truth-{payload-final,
reproduction-proof,unit}.log`; regenerations: `build/scratch/truth-regen.log`.
Truth-report helper smoke check passed; new report/timing outputs stayed under `build/truth-audit/`.

## TASK-127 — roxygen links

A clean assembly deletes `man/*.Rd`; roxygen's first Markdown pass therefore
cannot discover local help topics. `codegen_links.R`, run after wrapper
generation, qualifies function-call links to functions defined in the generated
package. It handles copied source documentation too (including bootstrapml),
leaves external links intact, and lets roxygen emit ordinary local Rd links.
A fresh `just regen` reduced unresolved-topic diagnostics from **976 to 0**.
An Rd alias check resolved **976/976** local links; the two additional
unqualified links resolve to `stats::confint`. Before/after evidence is in
`build/scratch/regen-{before,after}.log` and `build/scratch/rd-links.log`.

Final validation: `just cpp-test`: **6/6**, including **928 distribution
cases / 1,281,417 assertions**. `NOT_CRAN=true just test`: **FAIL 0, WARN 0,
SKIP 1, PASS 39040**; the skip is the optional, uninstalled `{sn}` reference.
`just quality`: **98/98 passed**. `just check-standalone`: **Status: OK**,
including all Rd checks, cross-references, examples and vignette rebuilding.
Its private tests gave **FAIL 0, WARN 0, SKIP 3, PASS 39018**. Skips were
optional `{sn}`, the unavailable repository-codegen test, and the CRAN-gated
special-function test; the full local R run exercised the latter two.
`UMVL_CHECK_PARENT=$PWD/build/scratch` kept the isolated check leaf in this
worktree; Landlock denied reads of the checkout outside that leaf.
Final logs: `build/scratch/{cpp,r,quality,standalone}-final.log` and
`build/scratch/standalone-00check.log`. No commits or board edits were made.
