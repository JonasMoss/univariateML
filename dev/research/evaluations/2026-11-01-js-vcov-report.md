# TASK-124.5: covariance in the JavaScript binding

Completed 2026-11-01 in `codex/js-vcov`, based on
`7d6fe5361668c0ed49cf99b6d4198a933cb6bb1d`, with uncommitted changes.
No commits, board edits, installs, core inference changes, R behavior changes,
site source changes, default-fit changes or golden changes were made.

The API is `api.gamma.vcov(data, fit, type = 'sandwich')`, with
`'observed'` as the other accepted type. This follows the existing family-level
fit/evaluation API and avoids retaining hidden observation copies in fit
objects. The caller supplies the original data and an unmodified successful
fit from the same family. Coefficients retain their original order; held
coefficients are known constants, not estimated nuisance parameters.

The result contains `matrix` (owned `Float64Array` rows), `names` (labels for
both axes, in free-coefficient order), `held`, `type`, `condition`, `validity`
and `warnings`. All-held fits have an empty matrix. The condition number is
that of total observed information in natural coordinates. Above 1e6 the exact
R conditioning message appears in `warnings`, matching the binding's existing
returned fit diagnostics. Core status failures throw the same reason strings
and validity conditions as R. Fitting restrictions stop before inference.

The embind entry point directly reuses `family_inference` and
`inference_covariance`, including the analytic norm/gamma/beta/pois routes.
It preserves the distinction between per-observation score cross-products,
total observed information, and the two covariance constructions. No extra
sample-size factor or optimizer inverse Hessian is used. The facade constructs
the mask using dossier order and `fit.fixed`. Generated coefficient metadata
lives in `families.mjs`, which existing site packaging already copies.
`index.d.ts` supplies `Covariance<P>` and `Family.vcov`; all 98 generated family
types inherit the method without a redundant per-family signature change.

Implementation and reproducible checks:

- [Binding](../../../bindings/js/bindings.cpp), [facade](../../../bindings/js/index.mjs),
  [generator](../../../bindings/js/generate.py), [types](../../../bindings/js/index.d.ts)
  and [API documentation](../../../bindings/js/README.md).
- [Node tests](../../../bindings/js/vcov-test.mjs) compare both types against
  the [native C++ entry point](../../../bindings/js/validation/vcov-native.cpp)
  at the exact estimates returned by JS, using canonical golden observations.
  Families: norm, gamma, beta, pois, exp, lnorm, betapr and binom. Tests cover
  free fits, each singly held coordinate and all-held fits where applicable;
  binomial uses a held size. Entrywise tolerance is
  `2e-4 * max(1e-6, abs(native))` (absolute floor 2e-10).
  Default-type equality, names, held coefficients, dimensions and owned output
  are also checked. No seeds or simulated data are used.
- Diagnostics cover unavailable uniform inference, free-location Laplace's
  unavailable regime, a free binomial size, restriction flags, boundary
  parameters, empty/nonfinite data, indefinite information, ill conditioning,
  the conditioning warning, invalid type and missing coefficient metadata.
- [TypeScript fixture](../../../bindings/js/validation/vcov-types.ts) checks
  named gamma coefficients, typed rows, invalid covariance kinds and an
  incompatible fit. `js-build` compiles the native probe using the existing
  host compiler and Boost headers; the main Node suite imports these tests.

Final completed runs (all on the workstation):

| Command | Result |
|---|---|
| `/tmp/umvl-cpu/run4 just js-build` | Pass; 98 families |
| `/tmp/umvl-cpu/run4 just js-test` | 409/409 pass; no failures or skips |
| `/tmp/umvl-cpu/run4 just test` | 19,945 passes; 151 tier skips; zero failures/warnings |
| `/tmp/umvl-cpu/run4 just test-full` | 41,413 passes; zero failures/warnings/skips |
| `/tmp/umvl-cpu/run4 just cpp-test` | 105/105 dev shards pass |
| `/tmp/umvl-cpu/run4 just site-check` | Pass; 98 family pages, browser hydration/controls, inference table, graph, MathML, links and 98 compiled C++ snippets |
| `/tmp/umvl-cpu/run4 node --v8-pool-size=1 --test bindings/js/vcov-test.mjs` | 9/9 pass before the final full-suite run |
| `/tmp/umvl-cpu/run node site/node_modules/typescript/bin/tsc --noEmit --strict --lib es2020 bindings/js/validation/vcov-types.ts` | Pass |
| `git diff --check` | Pass |

Every build/test ran in the foreground and was awaited. `run4` supplied the
shared four-slot limit, nice priority and single-thread BLAS/OpenMP settings.
`TMPDIR`, `JUST_TEMPDIR` and `XDG_RUNTIME_DIR` pointed to `build/tmp`.
Site dependencies were copied once from the main checkout as authorized.
The simbox attempt returned 75 (unreachable); R dev was rerun locally.
No successful run used simbox.

An initial site check failed because a separate coefficient registry module
was not copied by existing site packaging. Moving its generated export into
`families.mjs` fixed this; a fresh JS build, full JS suite and complete site
check then passed. A separate final-check attempt failed during sandbox startup
with transient ENOSPC, before tests started. Read-only diagnostics found 21 GB
free and ample inodes; ordinary commands recovered without cleanup. Those
checks were rerun successfully. Existing compiler warnings (including binomial
extended-real self-comparison) remain; no warning-producing core source changed.
The workstation environment was Node 22.22.1 (site-local Node 22.23.4), Python
3.14.5, R 4.6.1, GCC 15.2.0 and Emscripten 6.0.13.

Matched Wasm measurements, all 98 families, `-O2`:

| Artifact | Before | After | Change |
|---|---:|---:|---:|
| Wasm bytes | 1,892,073 | 2,435,819 | +543,746 (+28.74%) |
| gzip bytes | 570,863 | 611,667 | +40,804 (+7.15%) |

The baseline used `git show HEAD:bindings/js/bindings.cpp` saved under
`build/scratch/`, compiled against the same generated wrappers and headers.
Both commands used `-std=c++17 -O2 -Ibindings -Ibuild/js
-IunivariateML-cpp/include -I<existing BH include> --bind -sMODULARIZE
-sEXPORT_ES6 -sALLOW_MEMORY_GROWTH -sENVIRONMENT=web,node --emit-tsd <name>.d.ts`
and emitted adjacent `.mjs`/`.wasm` files. Gzip sizes use Python
`gzip.compress(bytes, mtime=0)` at its default level 9. Measurements describe
the Wasm asset, not all JS or network load times. Final Wasm SHA-256:
`303ae30de33b59b4b0268b1fadb7fa9bff0e08f36696237e398a0caedfe9681d`.

For identification of the tested uncommitted source, SHA-256 is
`e7e6f9721b22da43b767b997888cf8724f4e9dc02b0704498754482faeff0fa3`
for the concatenation of sorted relative names, NUL, file bytes, NUL under
`bindings/js`: `README.md`, `bindings.cpp`, `build.sh`, `families.mjs`,
`family-types.d.ts`, `generate.py`, `index.d.ts`, `index.mjs`, `test.mjs`,
`vcov-test.mjs`, `validation/vcov-native.cpp`, `validation/vcov-types.ts`.

These checks establish numerical binding agreement and regression behavior,
not new mathematical regularity proofs or coverage guarantees. Validity remains
that of the existing conditional C++ scopes and the assumptions described in
the [inference plan](../../roadmap/2026-10-30-inference-and-dqm.md) and
[R covariance report](2026-11-01-vcov-r-report.md). Sandwich inference still
needs independent observations, finite score variance, nonsingular sensitivity
and suitable estimator asymptotics; native agreement alone proves none of
these. The explicit-data API cannot verify that callers supplied the original
observations or an unmodified fit. No required task item remains open.
