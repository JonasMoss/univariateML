# TASK-156: stable discretized-normal CDF at extreme parameters

Completed on 2026-11-08 in `codex/pdnorm-fix`. Discretized-normal density and CDF evaluation now recover finite standardized lattice points when direct subtraction overflows. Quantile inversion benefits through its existing CDF calls. The audit also found and fixed an overflowing-distance problem in discrete-Laplace log densities and tails. Every required final suite passed.

The normal implementation keeps the original `(x + offset - mean) / sd` arithmetic whenever its numerator is finite. For finite inputs whose difference overflows, it evaluates `((x/2 - mean/2)/sd)*2`. The midpoint and both interval endpoints in `ddnorm`, and the upper lattice endpoint in `pdnorm`, use this helper. At the magnitudes where this fallback is needed, offsets of one half or one have already rounded away in the original lattice addition. Infinite CDF arguments retain their existing endpoint behavior. The quantile algorithm and its search limits are unchanged.

In exact arithmetic, halving bounds the difference of any two finite binary64 endpoints by the largest finite binary64 value. Dividing before restoring the factor of two avoids the overflowing intermediate when the standardized result is representable. This algebraic argument establishes the overflow repair; the floating-point accuracy evidence below is numerical, without outward-rounded interval bounds.

Discrete Laplace has an analogous problem in `(x-location)*log(scale)`: the distance can overflow while the product is finite. Its density and CDF now multiply the half-distance before doubling, only when subtraction of finite inputs overflows. The original arithmetic remains in use for finite differences.

The family audit found:

| Family | Relevant arithmetic and disposition |
|---|---|
| Discretized normal | Translated standardized midpoint and endpoints; fixed in density and CDF, with inversion regressions. |
| Discrete Laplace | Translated geometric log distance; fixed in density and both CDF tails. |
| Discretized gamma | Positive rate multiplication, with no translated subtraction; unchanged. Added shape-one exponential CDF checks at `x=1e308, rate=1e-308`, and saturated ordinary-tail checks at rate two. |
| Discrete Weibull | Hazard already formed in the log domain; unchanged. Added extreme-lattice hazard checks at small shape and saturated-tail checks at shape two. |
| Discrete uniform | `dunif_log_count` already halves operands when its width overflows; unchanged. |

Normal, gamma and Weibull are the three dossiers declaring a continuous discretization relation. The audit concerns the premature-overflow pattern above; it is not a general accuracy certificate for every extreme special-function argument.

Independent evidence is kept in [the data directory](2026-11-08-pdnorm-fix-data/). [check.py](2026-11-08-pdnorm-fix-data/check.py) uses 100-digit mpmath arithmetic, a unit-cell normal density integral, and the geometric closed form; its values are in [reference.json](2026-11-08-pdnorm-fix-data/reference.json). The exact binary64 inputs are converted to mpmath values rather than replaced by decimal ideals. [run-probe.sh](2026-11-08-pdnorm-fix-data/run-probe.sh) reproduces the workstation evidence in both arithmetic modes using the existing BH headers; its output is [workstation-probe.txt](2026-11-08-pdnorm-fix-data/workstation-probe.txt).

At the reported old quantile `-7.976931348623157e307`, with mean and standard deviation `1e308`, the independent CDF is `0.03611282497155248489`. The old formula maps its predecessor to negative infinity and returns zero. The repaired CDF gives approximately `0.03611282497155248`. The repaired `.01` quantiles are:

| Workstation mode | Returned quantile | Computed CDF there | Computed CDF at predecessor |
|---|---:|---:|---:|
| Native long-double backend | `-1.3263478740408408e308` | `0.010000000000000014` | `0.009999999999999995` |
| No-long-double backend | `-1.326347874040841e308` | `0.010000000000000004` | `0.009999999999999995` |

The one-representable-value difference reflects backend rounding. These brackets establish minimality for the computed CDF comparisons, rather than an interval-certified comparison to the exact real CDF. Independent normal probabilities check their numerical accuracy. Further reference cases give normal log cell mass `-712.11514717537074343` at `x=-1e308, mean=sd=1e308`, and discrete-Laplace log tail `-2.2204460492503132285e292` at `x=-1e308, location=1e308, scale=nextafter(1,0)`.

Regressions were appended to the four family C++ test files. Normal cases cover both signs, both tails and probability scales, density, largest finite endpoints, infinite arguments, and quantile brackets. Laplace cases cover finite log tails and densities despite overflowing distance. R tests exercise vectorized parameters; JavaScript tests exercise the WASM path. The new C++ checks use practical relative tolerances of `2e-14` for the normal CDF references and `3e-15` for the large Laplace logarithms; these are numerical checks, not universal floating-point bounds.

Final runs used the shared slot wrappers locally and `simbox-run` remotely, with four build/test workers and single-threaded BLAS/OpenMP. C++ compilers were GCC 15.2 on the workstation and GCC 13.3 on simbox. All commands exited zero:

| Command | Machine | Final result |
|---|---|---:|
| `just cpp-test` | Workstation | 106/106 |
| `just cpp-test` | Simbox | 106/106 |
| `UMVL_TEST_TIER=full just cpp-test` | Simbox | 129/129 |
| `just cpp-test-no-long-double` | Workstation | 129/129 |
| `just cpp-test-no-long-double` | Simbox | 129/129 |
| `just test` | Simbox | 22,125 passes; 154 skips; zero failures/warnings |
| `just test-full` | Simbox | 44,957 passes; zero skips/failures/warnings |
| `just js-build` | Workstation | Passed |
| `just js-test` | Workstation | 551/551; zero skips |
| `just site-check` | Workstation | Passed; 99 family pages and C++17 snippets |

[The retained run summary](2026-11-08-pdnorm-fix-data/test-results.txt) records timings and log locations. Site validation regenerated one unrelated historical screenshot; its original bytes were restored. No truth table or golden file changed. Fitters, defaults, shared special functions and code generators are unchanged. No commits, board edits, package installs or publication were performed. Open items within TASK-156: none.

The additional repository-wide Markdown link check reported four pre-existing destinations: `../boundary-points.pdf` in `2026-11-03-boundary-lit-report.md`, `1+m/r` in the negative-binomial dossier, and two `/inference/` destinations in `univariateML-cpp/notes/inference.md`. All three affected files are byte-identical to `HEAD`. The two changed dossiers and this report passed a separate destination check: seven local destinations, zero broken.

Open delivery issue: the artifact-pane `show` command returned HTTP 403 because this worktree is outside the viewer's registered readable roots. Display requires registering `/home/jonas/Files/programming/univariateML.wt/pdnorm-fix` with that viewer. The report and evidence are complete in this worktree; no viewer configuration or files outside it were changed.
