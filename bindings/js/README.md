# Private WebAssembly pilot

This is a thin embind binding of the C++17 core, with no npm dependencies.
Build from the repository root with `just js-build`, then run `just js-test`.
On the shared workstation reserve slots with `/tmp/umvl-cpu/run4 just js-build`
and `/tmp/umvl-cpu/run just js-test`. The build activates emsdk from `$EMSDK` (default `~/emsdk`),
uses BH headers, and writes its module, wasm, and generated low-level TypeScript
into `build/js/`. Cache and temporary files also stay under `build/`.

```js
import create from './bindings/js/index.mjs';
const {gamma} = await create();
const fit = gamma.fit(new Float64Array([0.5, 1, 2]), {shape: 2});
const density = gamma.d(new Float64Array([0.5, 1]), [2, 0.5], {log: true});
const sample = gamma.r(100, [2, 0.5], 123);
```

Each family exposes `d`, `p`, `q`, `r`, and `fit`; arrays returned are owned
`Float64Array`s. Coefficient order is exp `[rate]`, gamma `[shape, scale]`,
gb2 `[shape1, shape2, shape3, scale]`. `p` and `q` accept `{lower, log}`
(default true/false), and `d` accepts `{log}` (default false).
`fit(data, held = {}, control = {})` uses named held coefficients; omitted
or null values are free. Controls follow the R adapters: gamma `abstol`
(default 1.220703e-4), `iterlim` (100); gb2 `gradtol` (1e-7), `iterlim` (300).
Results contain named `estimates`, `logLik`, `iterations` (0 for exp), `fixed`,
and `status: 'Ok'`. Failed fits throw JavaScript `Error`s with R adapter
messages. The low-level embind fit returns a diagnostic envelope; the facade
throws after C++ returns so its temporary buffers are freed. Exponential observations are validated at the boundary; gamma and GB2 use
the core validation and adapter diagnostics.

Sampling constructs a C++ `StdRng` with an explicit uint32 seed per call.
The state belongs to the binding and never touches JavaScript's RNG.
Repeated calls with the same seed reproduce samples on the same build;
C++ standard library implementations need not produce identical streams.

The ES module also supports browser loading: keep its `.wasm` alongside
`build/js/univariateML.mjs`, or pass an Emscripten `locateFile` option to
`create`. The pilot has been tested in Node, not a browser. The handwritten
`index.d.ts` describes the facade; `--emit-tsd` generates declarations for
all low-level embind functions (whose `val` arguments are typed `any`).

Reproduction tools (run within CPU slot locks): `inventory.py` after activating
emsdk and setting `EM_CACHE=$PWD/build/em-cache`; `measure.sh` for optimization
sizes, load timings and native/wasm fit timings. Measurements and exhaustive
truth failure records are written under `build/scratch/`. The report is
[the pilot evaluation](../../dev/research/evaluations/2026-10-30-js-pilot-report.md).
