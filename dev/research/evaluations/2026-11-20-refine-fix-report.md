# Burr, Pareto III, and negative hypergeometric refinements

TASK-214.5 on `codex/refine-fix`, based on `1252894e`. The archived Burr and Pareto III coefficients agree with independent 80-digit stationary solves in both precision modes. The negative-hypergeometric fits satisfy all named nesting comparisons. Three of the four count fits have certificates at the package's floating-point margins.

## Per family

| Family | Change | Evidence |
|---|---|---|
| Burr | The baseline already formed median-relative logs with `log1p` for shape1 held. The location and power scores now share one precomputed vector of mean-centred relative logs. | Cases 638 and 639 agree with the stationary solutions to at most $1.84\times10^{-15}$ relative error. All 220 snapshots match the baseline in each precision mode. |
| Pareto III | The local polish uses the median observation as a log-excess origin when scale is free. It forms nearby log excesses as $\log1p((x-x_{\rm ref})/(x_{\rm ref}-\min))$ and restores the constant log offset in the fitted scale. | Cases 5590, 5591 and 5593 agree with the stationary solutions to at most $1.34\times10^{-15}$ relative error. |
| Negative hypergeometric | With composition held, the search enumerates feasible stopping counts and bisects the adjacent population likelihood ratio. The large-count free-composition search starts with the mode at $N=\max x_i$ and profiles the population at compositions obtained from adjacent-ratio searches on two population slices. It visits both ends of the stopping-count range within its work allowance. | Exact integer ratios give the line maxima and the nesting comparisons below. Certificates use separated endpoint signs or a likelihood upper bound to exclude other lines. |

Burr and Pareto III solve strictly concave conditional location and power likelihoods in the power and slope-scaled log-location coordinates. The reference uses the original binary64 observations as exact inputs.

For nhyper with fixed $m,r$, let $A=N+1$. Each observation contributes $1+(mx-rA)/[A(A-m-x+r)]$ to the adjacent likelihood ratio. Its logarithm vanishes at $x=rA/m$. For $m>r$, increasing $A$ shifts that zero to the right and decreases the derivative in $x$ proportionately more at larger $x$. Integrating on either side of the old zero proves that a nonpositive aggregate log ratio stays nonpositive. All factors are at most one by $A=m\max(x)/r$. With $m=r$, each factor reduces to $(A-r)/A$ and the maximum is at the first feasible population.

Bisection uses compensated integer product differences. A sign is certified when it is separated by $64\epsilon(1+\sum_i|\log R_i|)$, where $\epsilon$ is binary64 machine epsilon. Cross-line comparisons use $10^{-9}(1+|\ell|)$. For an unresolved fixed-$r$ line, $\sum_i\log(r/x_i)$ is an upper bound because the waiting-time mass equals $r/x_i$ times a hypergeometric mass. Case 5217 closes the free-composition comparison using a tighter upper bound for $m>r$ from the two adjacent hypergeometric masses. [Negative hypergeometric dossier](../../../univariateML-cpp/include/cppunivariate/distributions/nhyper/README.md)

## Signatures and callers

No starts were removed. Burr and Pareto III retain their existing entry points and callers. The nhyper C++ fitter takes a trailing `maxit = 100`. The common fit adapter takes `iterlim = 100`, which the R adapter and generated JavaScript runtime forward. Existing four-argument C++ calls in the diagnostics study and distribution tests use the default. The internal population searches receive the allowance explicitly. Population bisection uses the solver budget and stopping-count profiles use the certificate budget. The existing fixed-population grid uses the enumeration budget. [Work budgets](../../design/work-budgets.md)

## Named archive and strict entries

| Case | Reference check in both modes |
|---|---|
| Burr 638, 639 | Shape2 relative errors $+1.57\times10^{-15}$ and $-1.84\times10^{-15}$. |
| Pareto III 5590, 5591, 5593 | Shape relative errors $-5.64\times10^{-16}$, $+1.33\times10^{-15}$ and $-2.50\times10^{-16}$. All four known refinement entries pass. |
| nhyper 5161 | Fully free fit $(N,m,r)=(4503599627370495,4503524867960574,999988)$, $\ell=-14.200514946467$, uncertified. Exact ratios verify the population maximum at those held $m,r$. The fit exceeds all three more-held fits. |
| nhyper 5175 | With $m=20$ held, $(N,m,r)=(1000011,20,20)$, $\ell=-54.099554423934$, certified. It exceeds 5230. The earlier reference returned the second feasible population because its bisection assumed an increasing likelihood at the lower endpoint. The exact ratio is already below one there. |
| nhyper 5217 | With $r=3$ held, $(N,m,r)=(1000011,3,3)$, $\ell=-63.584610345864$, certified. It exceeds 5230. |
| nhyper 5230 | With $m=20,r=3$ held, $(N,m,r)=(6666697,20,3)$, $\ell=-70.661497920222$, certified. |

Both known nhyper nesting entries pass in both modes. The replay uses the ratios from [the original exact check](2026-11-15-refine-causes-data/exact_nhyper.py) with endpoint-aware bisection. It also exhausts $N=\max(x),\ldots,\max(x)+2(\max(x)-\min(x))$ for 5161 using exact stopping-count ratios. The best point in that window has $\ell=-14.448973792179$. All six one-coordinate neighbors of the returned point have lower likelihood at 80 digits. [Replay script](2026-11-20-refine-fix-data/check_archive.py) · [Native replay](2026-11-20-refine-fix-data/archive-native.jsonl) · [Double-only replay](2026-11-20-refine-fix-data/archive-double.jsonl)

## Snapshot comparison

The baseline is the frozen 2026-11-15 snapshot at the supplied starting revision. The comparison counts any change to the recorded result, including work counts and certification flags. Relative coefficient moves use the baseline magnitude and a unit denominator at zero.

| Family | Cases per mode | Changed native / double | Largest relative move native / double |
|---|---:|---:|---:|
| betapr | 212 | 0 / 0 | 0 / 0 |
| bhatt | 173 | 0 / 0 | 0 / 0 |
| burr | 220 | 0 / 0 | 0 / 0 |
| fpareto | 572 | 0 / 0 | 0 / 0 |
| gb2 | 572 | 0 / 0 | 0 / 0 |
| genbeta | 220 | 0 / 0 | 0 / 0 |
| gengamma | 28 | 0 / 0 | 0 / 0 |
| invburr | 220 | 0 / 0 | 0 / 0 |
| invparalogis | 116 | 0 / 0 | 0 / 0 |
| invtrgamma | 180 | 0 / 0 | 0 / 0 |
| nhyper | 147 | 29 / 29 | 4.5e+15 / 4.5e+15 |
| paralogis | 104 | 0 / 0 | 0 / 0 |
| pareto2 | 116 | 0 / 0 | 0 / 0 |
| pareto3 | 104 | 8 / 36 | 3.72e-09 / 3.16e-05 |
| pareto4 | 204 | 0 / 0 | 0 / 0 |
| trgamma | 204 | 0 / 0 | 0 / 0 |

Each mode covered 3,392 cases. Every outcome kind was preserved. Away from the named archive cases, all relative coefficient changes were below $10^{-11}$. The only nhyper coefficient moves were in 5161, 5175 and 5230. Cases 5175, 5217 and 5230 gained certificates in both modes. Pareto III's largest move was shape in 5591, and nhyper's was $m$ in 5161. [Comparison script](2026-11-20-refine-fix-data/diff_baseline.py) · [Native diff](2026-11-20-refine-fix-data/snapshot-native-diff.jsonl) · [Double-only diff](2026-11-20-refine-fix-data/snapshot-double-diff.jsonl)

## Test changes

- `pareto3/test.cpp` adds an exact-input coefficient regression for 5591.
- `nhyper/test.cpp` checks the population endpoints and line maximum, certificate flags, nesting, and exhaustion at `maxit = 1`.
- `bindings/r/distributions/nhyper/test.R` checks that `iterlim` reaches the population solver.
- The existing Burr regression for 638 and 639 passed in both builds. Existing tests and golden fixtures were retained.

## Validation

| Check | Machine | Result |
|---|---|---|
| Native and double-only fit snapshots | Workstation, GCC 15.2 | 16 families and 3,392 cases per mode |
| `just test-changed --no-long-double`, C++ | Workstation, GCC 15.2 | 16/16 passed native and 16/16 passed double-only |
| `just test-changed --no-long-double`, R development tier | Workstation | 4,897 passed, zero failures, one existing constant-sample nhyper warning, 36 skips |
| `just test-changed --no-long-double`, JavaScript | Workstation, Emscripten and Node | Build passed, 292/292 selected tests passed |
| `just quality burr pareto3 nhyper` | Workstation | All three passed |
| `just site-check` | Workstation | Passed, including browser checks and 85 C++17 examples |
| `python3 dev/tools/check_links.py` | Workstation | Zero new broken destinations, 47 inherited |

## Open

- **nhyper 5161 global comparison.** The finite solution is a coordinatewise maximum at the archived counts. The shifted-negative-binomial face at $r=999988$ exceeds its likelihood by about $4.15\times10^{-11}$, within the practical comparison margin. A global bound over the remaining population, composition and stopping counts is needed to distinguish an attained maximum from escape. The fit retains its uncertified flag.
- **Shared log-excess preprocessing.** The family-local Pareto III polish preserves the original differences once a log-logistic parent fit exists. A shared transform that rounds distinct shifted observations to the same excess before forming an anchor still needs an original-data centring path in the root-ulp work.
- **JavaScript declarations.** The runtime supports `iterlim` and the site build generated the corresponding declaration. The checked-in `family-types.d.ts` entry is pending in ctrl-js.
