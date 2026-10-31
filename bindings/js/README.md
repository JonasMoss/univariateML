# Private WebAssembly binding

`just js-build` generates embind d/p/q/r for all 98 dossiers, verifies the
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
Sampling uses a binding-owned C++ `StdRng`, seeded explicitly per call;
reproducibility is within a build, not across standard library implementations.

Fitting covers **all 98 families**. Validation, core calls, status mapping,
warnings and diagnostics live in `bindings/common/distributions/` and are
shared with the thin R adapters. Both `families` and `fitFamilies` enumerate
all 98 IDs; generation discovers shared headers without family-specific rules.

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
requires all 98 shared fitters and compares the aggregate with all 98 single
family modules. For batch checks, `FIT_FAMILY=a,b node --test
bindings/js/fit-test.mjs` selects fit tests without rebuilding a subset.

`just js-test` audits every truth entry and sampler point for every family,
and all 98 goldens, held subsets and diagnostics. It records every failed
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
