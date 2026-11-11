**TASK-179 — Exponentiated Weibull d/p/q/r**

Implemented scalar C++ `dexpweibull`, `pexpweibull`, `qexpweibull` and `rexpweibull`, with both probability tails, natural/log probabilities, endpoint handling and inverse-transform sampling. All required workstation checks passed. Validation finished on 2026-11-11; the job retains its requested November 10 filenames. Base: `99d64df4`, branch `codex/ew-dpqr`; changes are uncommitted.

The [family directory](../../../univariateML-cpp/include/cppunivariate/distributions/expweibull/README.md) contains the five requested standard files: README, header, C++ tests and the two truth files. Coefficients are ordered `scale`, `shape`, `exponent`, corresponding to `a,b,c`. The first two names preserve Weibull's meanings; `exponent` identifies the CDF power separately from the Weibull shape. The fitter remains TASK-180.

For `h=(x/a)^b`, the exact log density is `log(b)+log(c)-log(x)+log(h)-h+(c-1)*log(1-exp(-h))`. The implementation retains `log(h)` when `h` underflows and combines its coefficient analytically. Below `log(h)=-36`, it uses the small-h expansion through the linear correction. Its omitted terms are below binary64 precision relative to the dominant log-density term; near cancellation at a finite density, their absolute size is negligible. Near `x=a`, `log1p((x-a)/a)` retains one-ulp displacements before multiplication by a large shape.

CDF evaluation retains `log(H)`, where `H=-log(F)=c*(-log(1-exp(-h)))`, through tail selection. This preserves log survival even when `H` is subnormal, and avoids losing the upper tail when the Weibull survival underflows before multiplication by a large exponent. Overflowed `b*log(x/a)` is factored in log coordinates. Quantiles invert using the two input log tails and the logarithm of `(-log(F))/c`; the final scale/power calculation stays in log coordinates. At zero, an FMA residual determines the sign of the exact binary64-parameter product `b*c-1`, avoiding a false finite endpoint density when the rounded product equals one.

These identities and endpoint limits follow algebraically from the stated CDF. The implementation's floating-point accuracy is supported by numerical comparisons, rather than an interval-arithmetic certificate:

- The independent mpmath route uses 80 digits and is checked by density integration and differentiation of its antiderivative. The table has **12 parameter points, 228 value rows and 528 quantile rows**, including both tails, log inputs down to −10000, endpoints, shapes 0.001 and 1000, and exponent `1e200`.
- Additional [100-digit references](2026-11-10-ew-dpqr-data/subnormal-exponent.json) check subnormal exponents, including the smallest positive double. Their log survivals at `x=1e-200, a=1, b=2` are approximately −737.6145749290254 and −730.001743898618. C++ regressions check these, reciprocal shapes `b=1e308,c=1e-308`, and near-scale displacements.
- The six family cases passed **15,061 assertions**. The sampler test checks exact agreement with the inverse using synchronized RNGs and uniform-PIT first/second moments over 10,000 draws. The table consumer also checks seeded sampling against independent CDF values.
- An [exact Weibull-reduction check](2026-11-10-ew-dpqr-data/weibull-reduction.cpp) returned the scale for upper log-probability −1 and exponent one, including Weibull shapes `1e-200` and `1e-308`.

The C++ truth consumer uses tolerance **5e-10**, with its standard log/natural-probability metrics; quantiles retain **1e-9**. At `b=1000,c=1e200,x=2.0122736097839593`, rounding the Weibull power amplifies to about `2.7e-10` relative error in a natural CDF near `1e-300`. This is the reason for the stated margin. When TASK-180 enables the bindings, their currently hard-coded `1e-10` natural-tail checks must accommodate this margin or the conditioned calculation must be refined.

The [SciPy checker](2026-11-10-ew-dpqr-data/scipy_check.py) independently compares six ordinary/stressed cases against SciPy 1.18.0 `exponweib`, with SciPy `a=our exponent`, SciPy `c=our shape`, `loc=0`, and the same scale. Log density, both log tails and quantiles agree with maximum scaled error **6.1965e-16**. [Per-case values](2026-11-10-ew-dpqr-data/scipy-check.json) are retained. SciPy is a cross-check, not the truth generator.

Small pipeline changes keep a dpqr-only family outside fitter-dependent generation until its `mle.hpp` arrives:

- R metadata loading and package-fixture assembly exclude the pending family. Metadata-only synthetic R generator fixtures retain their existing behavior.
- JS generation excludes families without a fitter header.
- Site derived data, family routes and inference eligibility exclude pending fitters.
- The fitter-quality command's default enumeration selects fitter dossiers.
- The truth reader uses simulation parameters only when both the fitter and fitted golden are absent; an existing golden is still read. The final table fingerprint was verified, and narrowing this fallback left the entire numerical payload unchanged.

These are eight existing-file edits, totaling 16 inserted and five removed lines. Existing fitters, family d/p/q/r headers, goldens and truth tables were not edited. Relations required no exclusion change.

| Final check | Machine | Result |
|---|---|---|
| `just dpqr-truth expweibull` | Workstation | Generated; all 12 points verified |
| `just cpp-test` | Workstation, GCC 15.2 | 107/107 passed |
| `just cpp-test-no-long-double` | Workstation, GCC 15.2 | 131/131 passed, including full slow tier |
| `just js-build`; `just js-test` | Workstation | Build passed; 601/601 tests passed |
| `just site-check` | Workstation | Passed |
| `just test` | Workstation, R 4.6.1 | 23,031 pass; 154 skip; 0 fail/warn |
| `just test-full` | Workstation, R 4.6.1 | 45,846 pass; 0 skip/fail/warn |
| `just relations` | Workstation | 157 records; 0 failed |
| Selected existing Python checks | Workstation, Python 3.14.5 | 10 passed |

All builds/tests used the supplied CPU-slot wrappers. Final native suites ran in separate one-slot reservations, allowing the R four-worker tier to remain within the six-core job budget. [Machine-readable results and source hashes](2026-11-10-ew-dpqr-data/validation-summary.json) and [short command receipts](2026-11-10-ew-dpqr-data/validation.txt) are retained.

**Open entries**

- **GCC 13 comparison:** `simbox-run just cpp-test-no-long-double` returned **75**, unreachable. The prescribed workstation fallback passed. A simbox run is still needed to obtain the second-compiler comparison.
- **Shared truth-metadata inventory audit:** the optional whole-module audit fails its hard-coded count of 98; master already contains 99 tables and this tree contains 100. A baseline-generator audit also found **87 already-stale fingerprints on master**, recorded in [baseline evidence](2026-11-10-ew-dpqr-data/metadata-baseline.json). The [failure receipt](2026-11-10-ew-dpqr-data/metadata-audit.txt) is retained. Shared generator edits also change its fingerprint dependency. Resolving the inventory audit requires removing the fixed cardinality assertion and a coordinated metadata-only fingerprint refresh of the settled tables; those files were left untouched under this brief. The new family's final fingerprint matches its inputs, and the other ten selected Python checks passed.
- **Artifact pane:** the required `bin/show` call returned HTTP **403** because this worktree is absent from the viewer's readable-root registry. Display requires a Nook thread registered with this worktree as its cwd, or this path added to `CODEX_MATH_WEB_VIEW_ROOTS`. That viewer configuration is outside the permitted worktree. The report and all linked evidence remain here.
