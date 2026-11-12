# Private WebAssembly binding

`just js-build` generates embind d/p/q/r for every dossier, verifies the
uniform positional signatures checked by R's `codegen_dpqr.R`, and compiles
one ES module. No npm dependencies live here. Run builds through
`/tmp/umvl-cpu/run4` and tests through `/tmp/umvl-cpu/run`.

```js
import create from './bindings/js/index.mjs';
const api = await create();
const density = api.norm.d(new Float64Array([0, 1]), [0, 1]);
const sample = api.pois.r(100, [3], 123);
const fit = api.gamma.fit(new Float64Array([0.5, 1, 2]), {shape: 2});
```

Coefficient order comes from dossier front matter. `d` accepts `{log}`;
`p` and `q` accept `{lower, log}`. Results are owned `Float64Array`s.
For discrete models, `q` and inverse-CDF sampling return exact integers up to
2^53 (about 9.0e15); beyond that, the smallest double whose distribution
function reaches `p`. `NaN` means the evaluation failed.
Sampling uses a binding-owned C++ `StdRng`, seeded explicitly per call;
reproducibility is within a build, not across standard library implementations.

Fitting covers **every family with a fitter**. Validation, core calls, status mapping,
warnings and diagnostics live in `bindings/common/distributions/` and are
shared with the thin R adapters. Both `families` and `fitFamilies` enumerate
every family id; generation discovers shared headers without family-specific rules.

`fit(data, held = {}, control = {})` uses named held coefficients. Omitted
or null values are free. Controls and defaults are read from the shared
function signature and checked against R formals. Nonfinite supplied values
remain distinguishable from omitted holds. Successful fits contain
`estimates`, `logLik`, `iterations`, `fixed`, `warnings`, `restricted`, and
`status: 'Ok'`, plus family result fields where present.
Failures return an error envelope from C++, then the JS facade throws after
C++ temporaries have been destroyed. The shared headers contain no Rcpp,
exceptions, or printing. The R assembly copies them into `src/common/`.

Build output, generated C++, caches and scratch evidence live under `build/`.
Set `JS_FAMILIES=exp` (comma-separated IDs supported) to build a subset for
measurement; the default build always restores all generated exports.
`measure.sh` builds matched-source subsets, restores the all-family module,
and reruns Node tests; `measure-all.mjs <module names>` records raw/gzip sizes
and five Node loads.
This measures local import/instantiation, not network transfer. The measurement
requires every shared fitter and compares the aggregate with every single
family modules. For batch checks, `FIT_FAMILY=a,b node --test
bindings/js/fit-test.mjs` selects fit tests without rebuilding a subset.

`just js-test` (also `npm test` here) runs disjoint test groups with
`CMAKE_BUILD_PARALLEL_LEVEL` as the Node concurrency limit, defaulting to one.
Parallel runs distribute sixteen groups; a serial run uses one group.
`UMVL_TEST_FAMILIES=a,b` restricts generated family loops while retaining
hand-written cross-family cases. It audits every truth entry and sampler point for every selected family,
and every golden, held subsets and diagnostics. It records every failed
truth entry in `build/scratch/truth-results.json` without relaxing native
tolerances. The generated TypeScript facade exposes precise fit coefficients and controls
for all shared families.

Keep the generated `.wasm` alongside its `.mjs`, or pass Emscripten's
`locateFile` option. The site copies both and the generated coverage registry.

The approved wasm-only Bhattacharjee, Burr and Pareto IV coefficient tolerances
are read from
C++ test constants; native and likelihood tolerances remain unchanged.
`validation/check-wasm-scores.py` reproduces the 60-digit likelihood/analytic
score comparison against the frozen wasm outputs (requires existing mpmath).
This is numerical stationarity evidence, not a proof of global optimality.

## Covariance and standard errors

```js
const data = new Float64Array([0.5, 0.8, 1.1, 1.6, 2.2, 3.0]);
const fit = api.gamma.fit(data);
const robust = api.gamma.vcov(data, fit); // default: sandwich
const model = api.gamma.vcov(data, fit, 'observed');
const standardErrors = Object.fromEntries(
  robust.names.map((name, i) => [name, Math.sqrt(robust.matrix[i][i])])
);
console.log(standardErrors, model.condition, robust.warnings);
```

No additional division by sample size is needed. Near singular members (such
as sn near alpha = 0 with free location, or bhatt near a = 0), weak
identification at small sample sizes and heavy tails can make standard errors
unreliable even without a warning. See the companion site's `/inference/`
catalogue and `/articles/inference/` theory article for conditional scope and
the limits of the Monte Carlo evidence.

Covariance uses `api.gamma.vcov(data, fit, type = 'sandwich')`; `observed`
is also supported. The family-level method follows the existing family API
and takes observations explicitly instead of retaining a hidden data copy in
fits. Supply the original data and fit from that family, without modifying
estimates, coefficient order, fixed names or restrictions.
It returns `{matrix, names, held, type, condition, validity, warnings}`:
`matrix` contains owned Float64Array rows, and `names` labels both axes in
canonical free-coefficient order. All-held fits return an empty matrix.
Held coefficients are treated as known, not fitted nuisance parameters.
The condition number describes total observed information in natural units;
the R warning above 1e6 is returned in `warnings`, consistent with fit diagnostics.
Unavailable subsets, regimes, boundaries and information failures throw the
same reasons as R. This calls the existing C++ inference implementation;
no fitter behavior changes. Sandwich validity additionally requires independent
observations, finite score variance, nonsingular sensitivity and appropriate
estimator asymptotics; availability does not establish finite-sample coverage.

`just js-build` also builds the native covariance reference
`build/scratch/vcov-native` using the existing host compiler and Boost.
To rebuild it alone use
`c++ -std=c++17 -O2 -IunivariateML-cpp/include bindings/js/validation/vcov-native.cpp -o build/scratch/vcov-native`
(add the existing BH include path if system Boost is absent). They compare both
covariances at identical JS estimates against the native C++ entry point.

## Structured fit stops

Cauchy, Burr, inverse Burr and Lomax attach `kind` (`tie`, `limit`, `budget`,
`nomle`) to selected thrown `Error`s. `model`, `estimates` and `limit_loglik`
describe a selected fitted limit. `candidates` contains row objects with named
coefficients, a model id (empty for the requested family), and
`loglik_lower`/`loglik_upper`. Top-level bounds concern the global supremum.
Candidates are diagnostic witnesses, not returned MLEs; ties can be numerical
ambiguities. NaN denotes unavailable information and infinity an unclosed
bound. `segment_loglik_upper` is Burr's last unresolved interior segment bound,
which does not cover unclosed tails. Messages and successful fit results are unchanged. See
[the convention](../../dev/design/structured-stop-details.md).
