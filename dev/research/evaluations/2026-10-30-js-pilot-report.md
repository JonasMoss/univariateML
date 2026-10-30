# WebAssembly pilot (TASK-124.1), 2026-10-30

Source: `codex/js-pilot`, base `25bf4767a076ffe865a0de0962919c1aaa0890b7`, plus this uncommitted patch. No commits, installs, board edits or website edits.
Environment: Linux x86-64; GCC 15.2.0; Emscripten 6.0.13; Node 22.22.1; BH 1.90.0-1 / Boost 1.90 headers. All builds/computations used `/tmp/umvl-cpu/run` or `run4`, nice 10, at most four cores; BLAS/OpenMP one thread; final Node tests/timings use `--v8-pool-size=1`. Scratch, cache and logs: `build/`.

## Seam and native evidence

Only `special/special.hpp` changes numerically relevant source: `ExtendedReal` is `double` when `BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS` is defined, otherwise `long double`. The eight extended normal/gamma/Student primitives use it for backend types, arguments, intermediates and exponent-range decisions. Algorithms and other long-double sites are unchanged.
`bash bindings/js/check-native-seam.sh` compares all eight primitives against HEAD: identical native object bytes at `-O0` and `-O2`. Final `just cpp-test`: 6/6 targets pass (922 distribution cases; 1,281,403 distribution assertions). Final `NOT_CRAN=true just test`: [ FAIL 0 | WARN 0 | SKIP 1 | PASS 39040 ] (one skip: optional `{sn}` package absent). Build logs contain inline-tail declaration warnings and roxygen unresolved dml/pml/qml/rml links; these are distinct from the R test warning count.
No golden, truth fixture, default fitter or R adapter changed. Object equality verifies the selected native primitives; passing regression tests verifies the tested workloads, not all possible floating-point inputs.

## wasm32 compile inventory

`source /home/jonas/emsdk/emsdk_env.sh; EM_CACHE=$PWD/build/em-cache /tmp/umvl-cpu/run python3 bindings/js/inventory.py` runs `em++ -std=c++17 -fsyntax-only` separately for every header, with core and BH includes. All 196 compile; no first errors or causes to report. This establishes compilation, not numerical coverage of 98 families. Full per-header stderr and JSON: `build/scratch/inventory/`.

| Family | mle / dpqr | Family | mle / dpqr |
|---|---|---|---|
| bbinom | compiles / compiles | logis | compiles / compiles |
| beta | compiles / compiles | logitnorm | compiles / compiles |
| betapr | compiles / compiles | lomax | compiles / compiles |
| bhatt | compiles / compiles | lst | compiles / compiles |
| binom | compiles / compiles | naka | compiles / compiles |
| bnbinom | compiles / compiles | nbinom | compiles / compiles |
| burr | compiles / compiles | nhyper | compiles / compiles |
| cauchy | compiles / compiles | norm | compiles / compiles |
| cmb | compiles / compiles | nsbeta | compiles / compiles |
| cmp | compiles / compiles | paralogis | compiles / compiles |
| dgamma | compiles / compiles | pareto | compiles / compiles |
| dlaplace | compiles / compiles | pareto2 | compiles / compiles |
| dnorm | compiles / compiles | pareto3 | compiles / compiles |
| dunif | compiles / compiles | pareto4 | compiles / compiles |
| dweibull | compiles / compiles | pig | compiles / compiles |
| emg | compiles / compiles | pois | compiles / compiles |
| exp | compiles / compiles | power | compiles / compiles |
| fatigue | compiles / compiles | rayleigh | compiles / compiles |
| foldnorm | compiles / compiles | sged | compiles / compiles |
| fpareto | compiles / compiles | sgomp | compiles / compiles |
| frechet | compiles / compiles | skellam | compiles / compiles |
| gamma | compiles / compiles | slash | compiles / compiles |
| gb2 | compiles / compiles | sn | compiles / compiles |
| ged | compiles / compiles | snorm | compiles / compiles |
| genbeta | compiles / compiles | sstd | compiles / compiles |
| geom | compiles / compiles | std | compiles / compiles |
| gev | compiles / compiles | tbinom | compiles / compiles |
| gompertz | compiles / compiles | tlambda | compiles / compiles |
| gpd | compiles / compiles | tnorm | compiles / compiles |
| gumbel | compiles / compiles | tpois | compiles / compiles |
| hcauchy | compiles / compiles | trgamma | compiles / compiles |
| hnorm | compiles / compiles | triang | compiles / compiles |
| ht | compiles / compiles | unif | compiles / compiles |
| huber | compiles / compiles | weibull | compiles / compiles |
| hyper | compiles / compiles | yulesimon | compiles / compiles |
| invburr | compiles / compiles | zeta | compiles / compiles |
| invexp | compiles / compiles | zib | compiles / compiles |
| invgamma | compiles / compiles | zinb | compiles / compiles |
| invgauss | compiles / compiles | zip | compiles / compiles |
| invparalogis | compiles / compiles | zipf | compiles / compiles |
| invpareto | compiles / compiles | zmbinom | compiles / compiles |
| invtrgamma | compiles / compiles | zmgeom | compiles / compiles |
| invweibull | compiles / compiles | zmlogarithmic | compiles / compiles |
| kumar | compiles / compiles | zmnbinom | compiles / compiles |
| laplace | compiles / compiles | zmpois | compiles / compiles |
| lgamma | compiles / compiles | ztbinom | compiles / compiles |
| lgser | compiles / compiles | ztgeom | compiles / compiles |
| llogis | compiles / compiles | ztnbinom | compiles / compiles |
| lnorm | compiles / compiles | ztpois | compiles / compiles |

## Binding and tests

`bindings/js/` contains embind C++ wrappers, an ES-module facade, private `package.json`, typed facade declarations and dependency-free `node:test` tests. `just js-build` emits `build/js/univariateML.{mjs,wasm,d.ts}` with `-O2 -sMODULARIZE -sEXPORT_ES6`; `--emit-tsd` works (low-level `val` arguments become `any`). No npm packages needed.
`await create()` exposes `exp`, `gamma`, `gb2`, each with `d/p/q(Float64Array, params, flags)`, `r(n, params, uint32Seed)`, `fit(Float64Array, held={}, control={})`. Params follow dossier order: `[rate]`, `[shape,scale]`, `[shape1,shape2,shape3,scale]`. Density accepts `log`; probability/quantile accept `lower` and `log`. Outputs are owned Float64Arrays, vectorised over observations, with one parameter tuple.
Fits return named estimates, logLik, iterations (zero for exp), fixed names and status `Ok`; held coefficients accept numbers or null/free. Gamma controls: `abstol=1.220703e-4, iterlim=100`; GB2: `gradtol=1e-7, iterlim=300`. R adapter failure/status messages are retained. Low-level C++ returns a diagnostic envelope; the public facade throws `Error` after C++ returns normally and releases buffers. Exponential observations are validated at the boundary; gamma/GB2 retain core validation and adapter diagnostics. No unchecked failed fit is returned.
The sampler constructs a seeded C++ `StdRng` per call; state remains in the binding, independent of JS RNG. Same-build seed repeatability is checked; cross-standard-library streams are not promised. Node is tested; browser loading is supported by the module configuration but untested.
Final `just js-test`: 11/11 Node tests pass, including all three goldens, 2,798 scalar truth/sampler comparisons, sampler validity/repeatability, every held subset, facade/vector flags and failure diagnostics. Full exhaustive failure record: `build/scratch/truth-results.json`; Node log: `build/scratch/js-final-test.log`.
Golden tolerances match native tests: exp/gamma estimates and logLik `1e-10` relative; GB2 estimates `1e-5`, logLik `1e-9`. Truth rules match `test_support.hpp`: log values `1e-10*max(1,abs(truth))`, natural tails above `1e-300` relative `1e-10`; quantiles relative `1e-9`, oracle absolute tolerance or bracket/lattice acceptance. Samplers use 20,000 draws at each of two points/family, seeds `20261029+point`, and the native five-standard-error CDF bound plus `1/n`.
**Every failing truth entry: none.** Thus there are no failing values/truths/errors to attribute to the double fallback. This is numerical evidence for these tables only, not a precision theorem or validation of the other 95 families. The tested gamma/GB2 tails largely use existing double/log-coordinate paths; seam compilation success does not establish extended-path accuracy everywhere.

## Sizes and timings

| Optimization | wasm bytes | JS bytes |
|---|---:|---:|
| `-O2` | 291,395 | 31,458 |
| `-O3` | 289,493 | 30,723 |
| `-Os` | 265,958 | 30,613 |

O2 marginal wasm cost from removing one family: exp 5,628 B, gamma 8,304 B, gb2 97,308 B. JS changes are only -18 to -22 B (output-filename differences); shared glue dominates.
Node O2 load: first 9.47 ms; ten subsequent loads median 7.23 ms (range 5.03–10.71).

| Golden fit | wasm O2 ms/fit | native doctest flags ms/fit | native O2 ms/fit |
|---|---:|---:|---:|
| exp | 0.003753 | 0.002404 | 0.000523 |
| gamma | 0.005422 | 0.008301 | 0.003036 |
| gb2 | 320.158661 | 353.366333 | 96.581470 |

`bindings/js/measure.sh` rebuilds sequentially at `-O2/-O3/-Os`, and at `-O2` with each family's exports removed. Marginal sizes include shared-code effects and are not additive forecasts for 95 more families. Sizes are uncompressed file bytes, not network transfer sizes.
Load timings include dynamic ES import plus instantiation on local disk: first load separately; ten subsequent distinct module imports share process/OS/engine caches, so they are warm measurements, not fresh-process cold starts. Fit timings exclude fixture I/O/module loading, include JS copying/result creation for wasm, and average 10,000 exp/gamma fits or three GB2 fits after one warmup. Native uses the same fixtures and core calls, compiled with doctest's default flags (`-std=c++17 -Wall -Wextra -Wpedantic`, no optimization); an additional `-O2` comparison separates build-mode effects. These small local timings are observations, not general performance guarantees.

## Reproduce and TASK-125

Set `TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp`; create that directory. Run `run4 env EMCC_CORES=4 bash bindings/js/measure.sh`, then `run4 env EMCC_CORES=4 bash bindings/js/validate.sh` (use absolute slot-lock paths above). The latter runs native seam comparison, `just cpp-test`, `NOT_CRAN=true just test`, `just js-build`, and `just js-test`; logs remain in `build/scratch/`. `check-native-seam.sh` copies headers under scratch and reads HEAD with `git show`; it never writes Git state.
TASK-125 should inventory why each non-seam long-double site exists (overflow range, reduction accuracy, cancellation, optimizer decisions), quantify software-binary128 cost on wasm, and compare against binary64-long-double platforms such as macOS arm64. Preserve each contract before replacing anything; add targeted extreme-tail and nearly-degenerate fit checks and all-family runtime truth/golden coverage. Native x86 long double, wasm binary128 arithmetic and double-only Boost are distinct regimes. No mathematical guarantees were added or proved here; numerical searches retain their existing certification/failure rules.
No other required source changes were found by compilation. Numerical changes to other long-double sites, `dpqr.hpp`, `tails.hpp`, fixtures and code generation are deferred; syntax success alone is not a reason to change them.
