# Accuracy-stop routing: gengamma, cmb and dgamma

Completed the acc-route follow-up to TASK-150.4 against baseline
`9e4a368577f752f3387ece249203fc40f7e022ca`. The required numerical, binding and
site validation runs are green. Successful-fit replay is bitwise unchanged in both floating-point
modes. No commits were made.

## Changes

- **gengamma:** conditional root-resolution failures, joint conditional
  failures, local refinement and returned-coordinate accuracy gates now retain
  public `(mu, sigma, Q)` diagnostics through shape proposals and the final
  fitter. Held coefficients do not contribute to the accuracy requirement.
  Failed proposals cannot leave their accuracy kind on an accepted fit.
- **cmb:** log-odds projection, returned-coordinate moment-score gates and
  local polish now record public `(size, prob, nu)` witnesses. Integer size
  and held continuous coefficients are excluded from the accuracy scale.
  Free-size failure handling preserves the continuous-coordinate kind.
  Projection diagnostics reuse the evaluated table; they spend no additional
  enumeration budget. Earlier proper candidates survive numerical and budget
  refinement failures, including failure before the first local table.
- **dgamma:** final polish uses the diagnostic overload and records public
  `(shape, rate)`, respecting holds. Its existing rate-score roundoff gate
  records the propagated coefficient bound. Witness likelihoods are evaluated
  at the public coefficients.
- **Bindings:** each family's common `fit.hpp` forwards its populated detail
  through the existing stop mapper, giving R tolerance/conditioning classes
  under `univariateML_numerical` and JS `kind`/`attainable_tol`.

Nonfinite evaluations, unfinished searches without a finite noise bound, and
budget exhaustion retain their numerical or budget classification. CMB's
noise diagnostic additionally requires the remaining correction to be within
the practical noise allowance; a large unfinished correction cannot acquire
an accuracy kind merely because the caller supplied a tiny tolerance.

The shared polish still rejects requests below `4 * epsilon(double)`. For a
finite evaluation it now diagnoses that rejection as `Tolerance`, with
`attainable_tol` at least that method floor. `record_accuracy` preserves an
explicit method floor when recording the public witness, even when it exceeds
adjacent-double spacing or the default tolerance. Nonfinite evaluations remain
numerical. Gengamma converts its allocated local tolerance floor back to public
units, handles underflow of the local request, and rounds the converted floor
upward so an inverse-conversion rounding tie cannot erase the diagnosed kind.
The accuracy-contract and structured-stop documentation describe these method
floors.

Removed trgamma's unqualified `generalized gamma` alias, retaining its existing
`Stacy generalized gamma` qualification. Regeneration also refreshed the JS
modifier metadata with the previously missing gengamma entry. No generator
source was changed.

## Evidence and limits of the claims

The conditioning classifications use practical score-roundoff allowances
propagated through the local information: gengamma's existing Cholesky check,
CMB's moment covariance, and dgamma's rate-score derivative. They are numerical
diagnostics conditional on the computed comparisons and stated floating-point
margins, not outward-rounded intervals or new proofs of global optimality.
CMB's projection likelihood allowance also uses the exact-arithmetic bound
`|d log L / d eta| <= n * size` to account for conversion of log odds to the
returned probability. The finite witnesses are proper evaluated candidates;
they are not asserted to be verified MLEs after a failed search.

A method-floor tolerance stop does not prove that no representable maximizer
lies closer. It describes the refinement method's supported resolution.
Successful search formulas, accepted-step rules, default controls, goldens and
certificate/fallback decisions were not replaced.

Appended C++ regressions cover both kinds for all three families, holds,
integer-size exclusion, finite public likelihood witnesses, free-size routing,
and numerical/budget negative controls. Shared tests cover a request between
one spacing and the four-epsilon floor, preservation of an explicit larger
floor, held-coordinate exclusion and nonfinite evaluation. Gengamma additionally
tests an internal conditional bracket, subnormal tolerance underflow and the
conversion-rounding boundary (`unit = 1.147`). R and JS regressions exercise
tolerance and conditioning for every family, plus the internal gengamma and
free-size CMB routes. Existing tests were retained.

The [replay script](2026-11-12-acc-route-data/replay.py) rebuilds the pinned
baseline read-only using `git archive`, extracting headers only into ignored
scratch space. It checks all eight gengamma hold subsets, four continuous CMB
hold subsets with size fixed at 10, and four dgamma hold subsets on their golden
samples. All 16 fits succeeded in each mode. Hexadecimal coefficients,
log likelihoods, iteration counts, statuses and gengamma certification flags
matched exactly: **32/32 successful fits unchanged**. This is regression
evidence on those samples, not an independent MLE proof. Compiler identity and
output digests are in [replay.json](2026-11-12-acc-route-data/replay.json).

## Validation

Every completed run below used the **workstation**. Long-suite simbox attempts
returned exit 75, so their initial runs used the prescribed local `run4`
fallback. Parallel suites/builds used `run4`; standalone family checks and
replay used `run`, with the wrappers' niceness and thread limits.

| Check | Result |
|---|---|
| `just test-changed master`, C++ dev | 108/108 shards |
| Same run, R dev | 29,040 pass; 0 fail/warn; 155 tier skips |
| `UMVL_TEST_TIER=full just cpp-test` | 135/135 shards |
| `just cpp-test-no-long-double`, GCC 15 workstation | 135/135 shards |
| `just test-full` | 57,772 pass; 0 fail/warn/skip |
| Final gengamma + CMB standalone doctests, native | 93/93 cases; 266,461 assertions |
| Same final family doctests, forced double | 93/93 cases; 266,461 assertions |
| Dgamma public-witness polish regression, native | 1 case; 15 assertions |
| Final gengamma R family and selected shared loops, full tier | 547 pass; 0 fail/warn; 6 filtered empty-test skips |
| `just js-build` | Passed |
| Final `just js-test` | 627/627; no failures or skips |
| `just site-check` | Passed: 100 family pages and 100 compiled/linked C++17 examples, links, MathML and browser checks |
| Final successful-fit replay, native and forced double | 16/16 bitwise matches per mode |

The full suites ran once after the shared changes. Subsequent refinements were
confined to family diagnostics; the listed final family doctests and R rerun
cover those late changes, with full JS, site and replay checks afterward. No
shared code changed after the full suites. Generated R headers were compared
with the current fitter headers after regeneration. A machine-readable check
inventory and source digests are in
[checks.json](2026-11-12-acc-route-data/checks.json).

An additional repository-wide Markdown destination scan returned 45 missing
destinations in seven unchanged baseline files, including historical evaluation
inventories, local literature paths and old inference links. Each diagnostic's
source file was compared byte for byte with the pinned baseline. The changed
Markdown files and this report have no broken destinations. These unrelated
documentation issues were left untouched; the scan and scoped-check evidence
are recorded in
[documentation-links.json](2026-11-12-acc-route-data/documentation-links.json).

## Open validation item

**GCC 13 on simbox could not be exercised.** The initial no-long-double attempt
and two explicit retries returned `75` because simbox was unreachable, including
the retry after local validation. The complete GCC 15 workstation fallback and
the final forced-double family checks passed. The remaining cross-compiler
check is `/tmp/umvl-cpu/simbox-run just cpp-test-no-long-double` when simbox is
available. There are no deferred implementation items, and no changes to
`dpqr.hpp`, `special/`, truth tables, goldens or code-generator sources were
needed.
