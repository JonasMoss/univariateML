# Interactive family explorers — TASK-124.2 (2026-10-30)

Source: `codex/js-explorer`, base `1bd8c44e1d7650aacb864a338a0f9be0f11f2f0d`,
plus uncommitted changes. No numerical C++/R changes, defaults or fixture changes.
No commits, board edits, publishing, package installs or other worktree writes.
Existing `site/node_modules` was copied from the permitted checkout.

## Implementation

The family page mounts a Svelte `client:visible` island between Definition and
prose/theory. Hydration then imports `/binding/index.mjs`, which instantiates the
locally served wasm. There are no CDN assets or numerical algorithms in the site.
`bindings/js/families.mjs` is the single binding coverage registry, exported by
the facade and consumed by the site's prepare step. The content collection
supplies coefficient order, domains, simulation values and support directly.
No family coverage list was added to the site or the other job's derived data.

The prepare step builds the binding, copies facade/glue/wasm into the site's
public directory, and derives ignored `binding.json`. Missing emsdk (or explicit
`SITE_WASM=0`) emits a warning, omits islands and removes stale public assets.
A failed wasm build fails the build rather than silently hiding a broken binding.

Controls have labelled keyboard sliders, exact numeric inputs and held checkboxes.
Positive coefficients use logarithmic windows from simulate/20 to simulate*20;
exact inputs can reach any positive finite value. Density/CDF are handwritten
SVG paths, with no plotting dependency. Plot ranges initially use dossier values'
0.001 and 0.99 quantiles; axes stay fixed while dragging, with an explicit rescale.
Captions state both axes; quantiles and fit coefficients are also text.
Log density, seeded simulation, histogram and fitted density use binding outputs.
Fitting displays status, logLik, true/estimated values and binding diagnostics.
Failures never render estimates. Sample size is limited to 10,000 in this UI.

Pasted observations accept spaces, commas, semicolons and newlines. Every covered
family whose declared support admits the data is fitted, sorted by AIC; support
rejections and fit failures stay visible. AIC uses the number of free estimates.
The page states that AIC ranks fitted models and does not test them.
MathML and preformatted code have horizontal scrolling containers on mobile.

## Validation and bytes

All final commands finished green, through `/tmp/umvl-cpu/run4`, nice 10,
OMP/BLAS one thread, `EMCC_CORES=4`; caches, temporary files and logs in `build/`.
Environment: Node 22.23.4, Chromium 151.0.7922.34, Emscripten 6.0.13,
Python 3.14.5, SymPy 1.14.0, GCC 15.2.0, existing BH/Boost 1.90 headers.
Reproducer: `bash build/scratch/validate-explorer.sh` inside the slot lock.
Tested source hashes: `build/scratch/js-explorer-source.sha256`.

- `just site-check`: passes with wasm, with `EMSDK` pointing to a missing SDK,
  and again with wasm restored. Each checks 98 pages, MathML, local assets,
  links, explorer coverage/asset omission and 98 compiled/linked C++ snippets.
- `python3 site/scripts/browser-smoke.py`: passes in both modes and after
  restoration. Checks lazy requests, slider input and keyboard operation,
  seeded simulation (n=200, seed=123), a held shape, estimates, constant gamma
  data `[2,2,2,2]` with its unbounded-likelihood diagnostic and no estimate,
  pasted-data AIC order, no page errors/external requests, all three pages at
  390 px, scrollable wide math/code, preferences, search and no-JS behaviour.
- Binding `node --v8-pool-size=1 --test bindings/js/test.mjs`: 11/11 pass,
  including existing golden/truth/sampling/hold/diagnostic checks and the new
  registry/facade agreement assertion. No R or C++ numerical suite was needed.
- `git diff --check`: passes. Desktop explorer and mobile GB2 screenshots
  inspected under `build/scratch/site-browser/`. Fits remain synchronous;
  expensive GB2 fits can pause interaction. No new mathematical guarantee is claimed.

Raw, uncompressed bytes; paired builds differ only in explorer availability.
Inline island styles, props and hydration code are counted in HTML.

| Page | HTML with explorer | HTML omitted | Added HTML |
|---|---:|---:|---:|
| exp | 16,306 | 7,273 | 9,033 |
| gamma | 19,459 | 9,719 | 9,740 |
| gb2 | 24,862 | 13,642 | 11,220 |

Shared CSS is 4,457 B in both modes (217 B above the scaffold report's CSS,
including mobile scrolling rules). No plotting dependency: the entire minified
explorer component, including controls/fitting/ranking/SVG code, is 11,125 B.
Chromium observed only Gamma HTML + CSS initially (23,916 B), then **378,345 B**
on intersection: island/Svelte chunks 54,182 B; facade 1,174 B; registry 116 B;
Emscripten glue 31,478 B; wasm 291,395 B. This shared lazy payload is the same
for all three pages, cached by the browser; binding requests before visibility: 0.
Total dist: 3,177,035 B with wasm versus 2,821,083 B omitted. Shared catalog chunks
and search assets mean dist deltas differ from per-page request totals.
Measurements: `build/scratch/site-weights-{wasm,without-emscripten}.json` and
`build/scratch/site-request-weights-wasm.json`; final logs use `*-final.log`,
with omission logs `site-{check,browser}-without-emscripten.log`.

## Extension and stage 4

Adding a family requires low-level d/p/q/r/fit exports, its binding registry entry,
facade types and golden/truth/diagnostic tests. Its dossier supplies the existing
parameter/support metadata; no site coverage edit is required. The current UI
handles numeric interval bounds and real/integer supports (the three pilots all
have positive support). A future binding for coefficient-dependent support bounds
needs a binding-owned support resolver; the UI currently rejects unresolved bounds
rather than fitting data it cannot establish to be admissible.

Stage 4 can reuse the lazy loading seam, domain controls, held values, SVG paths,
status/diagnostic display, seeded sampling and browser harness. Any profile or
limit-model numerical evaluation should be added to the binding/core; no claim of
mathematical certification, fit optimality or model adequacy is added by this UI.

Artifact-pane `show` returned 403: this worktree is outside the viewer's readable
roots. No viewer configuration or files outside this worktree were changed.
