# TASK-140.1: lst/std certificate budgets and comparison ties

All requested retained cases now have certified outcomes. The 31 frozen
samples give **62 certified family/sample decisions**, preserving all 60
previously certified decisions and closing both normal-0 budget refusals.
Both domains also certify the 4000-point quantile sample and both retained
multimodal examples. No requested retained comparison remains unresolved.
All final development, full, no-long-double, JavaScript and site runs finished
green before this report was written.

The source is branch `codex/t-close`, based on
`f30584cd1819744e130850bcb1e58a5d1c52ed28`, with uncommitted changes identified
in [validation.json](2026-11-06-t-close-data/validation.json). The
[environment record](2026-11-06-t-close-data/environment.json) verifies 361
native include/test and binding-source identities against the simbox snapshot.
No commit, Git write command, board edit, sub-agent, dependency installation,
publication or other-worktree edit was performed.

**Outcomes.** Shapes and profile counts below are native certificate results;
std uses the same ordinary-scale likelihood with its shape domain above two.

| Sample | Certified shape | lst profiles | std profiles |
|---|---:|---:|---:|
| Retained normal-0 | 218.257164105 | 129 | 132 |
| 4000-point t300 quantiles | 539.460572975 | 135 | 138 |
| Cauchy-stable-interior counterexample | 33.905406907 | 97 | 100 |
| Two-interior-maxima counterexample | 49.705241724 | 108 | 111 |

All eight entries are enclosed finite global maxima. The former multimodal
comparison ties are resolved by strict bounds, without declaring approximate
values equal or replacing the certificate with a local optimizer. The frozen
31-sample outcomes are now:

| Outcome | lst | std |
|---|---:|---:|
| Interior finite maximum | 10 | 7 |
| Attained restricted Cauchy maximum | 8 | 0 |
| Normal supremum | 11 | 12 |
| Excluded positive-scale t2 supremum | 0 | 11 |
| Bounded collapse supremum | 0 | 1 |
| Unbounded collapse | 2 | 0 |
| Budget, tie or numerical refusal | 0 | 0 |

[replay.json](2026-11-06-t-close-data/replay.json) contains all 68 outcomes:
62 retained decisions and six additional multimodal/quantile decisions. The
replay asserts preservation of every previously certified outcome.

**Implementation and mathematical scope.** Changes are confined to the
all-free engine in
[lst/certificate.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/lst/certificate.hpp),
appended lst/std C++ tests, and the corresponding std certificate regression
cases in R and JavaScript. Held-subset algorithms, fitter entrypoints, d/p/q/r,
special-function headers, truth fixtures, generators and goldens are unchanged.

The [derivation and algorithm note](2026-11-06-t-close-data/derivation.md)
records the formulas, conditions, search proposals, fallbacks, cost and refusal
rules. It extends the existing
[native arithmetic contract](2026-11-06-t-port-data/arithmetic.md) and
[root/endpoint derivation](2026-11-05-t-root-data/derivation.md):

- A verified local nuisance ball bounds the profile optimization gap by
  the squared score norm divided by twice a positive Hessian lower bound.
  Geodesic uniqueness identifies its interior minimum with the conditional
  global optimum. The original score/distance bound remains a fallback.
- Ordinary location, log scale and relative shape coordinates preserve
  analytic normal-seam cancellations in the Hessian. Verified neighborhoods
  contain the entire transformed original root box and every conditional
  optimum; positive full Hessians establish strict profile concavity there.
- Verified nuisance tubes on exterior shape intervals supply tighter chord
  curvature bounds through the profile Hessian Schur complement. A failed
  tube check retains the original global chord bound.
- The normal tail uses the empirical fourth moment, a verified nuisance ball
  and the integrated normalizer remainder to bound its excess likelihood by
  `max(0,A)*h + K*h^2` for every shape at least `1/h`. A strict comparison with
  the feasible finite witness excludes that entire tail. The original signed
  tail theorem and containment-based `C/v` fallback remain available.

The guarantee remains **certification in exact arithmetic conditional on the
computed comparisons, with the existing practical binary64 margins**. The
64-epsilon elementary allowances and pairwise-sum allowances are unchanged;
no outward-rounding claim is made. General shape unimodality is not assumed.
Root contraction, coefficient widths, the complete std sd transformation,
endpoint comparisons and terminal failure handling remain required. The
4096-profile default budget and numerical shape-domain guard are unchanged.
No capped or unchecked estimate is returned.

**Independent numerical checks and goldens.** The
[independent checks](2026-11-06-t-close-data/independent.json) verify 42 returned boxes
against independent roots: 17 retained interior fits, 19 positive-scale endpoint
fits, four multimodal winners and both quantile-domain results. The new
[80-digit quantile solve](2026-11-06-t-close-data/quantile-root.json) uses the
exact native binary64 observations and has maximum absolute score below
`1.7e-76`; its shape is approximately 539.460572974731.

[Arithmetic checks](2026-11-06-t-close-data/arithmetic.json) compare the new
ordinary-coordinate scores and Hessians against independent 80-digit
log-gamma objective differentiation: 39 boxes, 221 sampled centre/corner
points, 12 quantities per point, plus 39 new gamma-derivative enclosure
checks. Shapes range from 0.501 through `1e14`. These finite checks corroborate
the formulas and practical margins; the linked inequalities supply the
exact-arithmetic arguments.

Both golden files are byte-identical to the baseline. Every point in each
own-golden public coefficient box remains within the unchanged `3e-5`
relative tolerance: maximum errors are `5.442e-9` for lst and `8.926e-6` for
std. Full R validation now exercises the 4000-point independent optimizer
comparison instead of taking its previous budget-refusal branch.

**Cost and bounded refusals.**
[Paired measurements](2026-11-06-t-close-data/benchmark-final.json) use the exact
baseline header from f30584cd and the final header, C++17 `-O2` on the
workstation, one warmup and the median of three measured calls. Both versions
use identical observations and controls. The machine is shared; these are
wall-clock observations, not isolated benchmark precision.

| Workload | Baseline outcome/time | Final outcome/time |
|---|---|---|
| normal-0, lst | Budget, 2.554 s | Finite, 0.041 s |
| normal-0, std | Budget, 2.588 s | Finite, 0.041 s |
| quantile-4000, lst | Budget, 17.692 s | Finite, 2.224 s |
| quantile-4000, std | Budget, 7.023 s | Finite, 2.579 s |

Explicit budgets of 40 and 128 still stop terminally when insufficient,
preserving witnesses, no estimate and an infinite unknown global upper bound.
At budget 128 the quantile refusals fell from 5.550/5.464 s to 2.568/1.859 s.
Very small budgets are not uniformly faster: the extra proof checks can cost
more before refusal; the budget-40 quantile lst case rose from 0.451 to
1.391 s in these shared-machine measurements. No default-budget refusal
remains among the requested samples.

The first ordinary-coordinate version still needed 3993/3689 profiles and
about 32 s on the quantile sample with the old unsigned tail. The uniform
moment tail removed that long exterior search. Shape-aware neighborhood
nominations and testing Hessian positivity before the central score pass
reduced unsuccessful proof work. The
[search history](2026-11-06-t-close-data/search-history.json) preserves these
reasons and corrected harness attempts separately from successful final runs.

**Final validation.** Every foreground command finished with exit code zero;
no final failure was permitted. Workstation computations used the shared
CPU-slot wrappers; long runs used nice 10 and one BLAS/OpenMP thread per
worker. The local four-slot wait was allowed to finish. Temporary files and
logs stayed in this worktree.

| Command | Machine | Result |
|---|---|---|
| `just cpp-test` | simbox | 106/106 dev shards |
| `just test` | simbox | FAIL 0, WARN 0, SKIP 154, PASS 20821 |
| `just cpp-test-no-long-double` | simbox | 127/127 |
| `UMVL_TEST_TIER=full just cpp-test` | simbox | 127/127 |
| `just test-full` | simbox | FAIL 0, WARN 0, SKIP 0, PASS 43637 |
| `just js-build` | workstation | Completed |
| `just js-test` | workstation | 430/430; no skips |
| `just site-check` | workstation | 99 family pages/snippets, browser and MathML checks green |
| `just relations` | workstation | 157 records, 0 failed |
| Native replay, independent roots and ordinary arithmetic | workstation | Passed |

Development skips are tier omissions; the full R tier has none. Site checking
regenerated a pre-existing browser screenshot; its exact baseline bytes were
restored to keep unrelated artifacts unchanged. No fitter or regression-test source changed after the
final suite snapshot. GCC versions are 15.2 on the workstation and 13.3 on
simbox; both R environments use 4.6.1, with BH 1.90.0.1, Rcpp 1.1.2 and
testthat 3.3.2. Workstation Node is 22.22.1 and Emscripten is 6.0.13.

The [evidence index](2026-11-06-t-close-data/README.md) gives reproduction
commands. Source identities, exact commands, machines and log hashes are
retained in [validation.json](2026-11-06-t-close-data/validation.json).

**Open — artifact-pane registration.** The requested display attempt returned
HTTP 403: this worktree is outside the viewer's configured readable roots.
Displaying the report requires registering this worktree root with the viewer.
Configuration outside this worktree was not changed. The report and evidence
files remain at their requested paths; the attempt is recorded in
[final-checks.json](2026-11-06-t-close-data/final-checks.json).
