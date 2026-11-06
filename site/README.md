# Distribution papers

Astro static pages, with a Svelte catalog island and Pagefind full-text search.
All distribution facts come from the canonical C++ README dossiers. No authored
family catalog is maintained here. System serif fonts keep runtime requests local.

## Build

From the repository root (Python with PyYAML and SymPy, g++, Boost headers):

```sh
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp
export XDG_RUNTIME_DIR=$PWD/build/tmp npm_config_cache=$PWD/build/npm-cache
mkdir -p "$TMPDIR"
/tmp/umvl-cpu/run npm ci --prefix site --no-audit --no-fund
/tmp/umvl-cpu/run just site-check
```

Node and npm are pinned local dependencies because Astro 7 requires a newer
Node than the workstation default. Recipes prepend `site/node_modules/.bin`;
bootstrap `npm ci` may print engine warnings from the older host toolchain.
No global installation is needed. Set `BOOST_INCLUDE_DIR` for a custom Boost path;
the check also finds existing workstation BH headers without invoking R. Use `just site-dev` through the same slot lock
for the development server. Stop it when done. `just site-build` produces
`site/dist/`, including the Pagefind index; `just site-data` only derives JSON.
Pagefind search is available in production builds, not the Astro dev server.
Deploy the static output at the origin root; subpath hosting needs an explicit
Astro base and matching link/asset changes before release.

## Family routes

`/distributions/<id>/` is the wiki-style family page: definition and mini plot,
relations, fitting conditions, language tabs, implementation and source evidence.
`/distributions/<id>/explore/` provides simulation, fitting, held coefficients and
AIC ranking for families in the binding registry, linked in both directions.
Explorers are excluded from Pagefind; the canonical family page is indexed once.
The prototype `/lab/` route has been removed.

## Boundaries

- `src/content.config.ts`: canonical dossier collection, read in place.
- `scripts/data.py`: reuses the quality density parser (declarative source, not numerical `spec.py`); derives formulas,
  summaries and C++ usage from dossier metadata, headers and existing fixtures.
  The readable-density rule is deterministic and conservative; otherwise the
  exact parsed log-density is displayed. Formulas apply on declared support.
- `src/lib/markdown.ts`: one build-time Markdown → MathML pipeline for prose,
  notices and formulas. No runtime math renderer or math font downloads.
- `src/lib/notices.ts`: shared wording for limits, structural holds and restrictions.
- `articles.config.mjs`: optional article registrations; missing sources are omitted.
- `scripts/prepare.mjs`: logo and explicitly linked source evidence, served locally.
  Missing authored relative pointers remain text rather than invented links.
- `components/Catalog.svelte`: progressive enhancement; all entries render as HTML.
- `layouts/Page.astro`: shared persistent usage choice and implementation controls.
- `scripts/check.py`: output checks and compilation of every generated C++ example.

The header preference script runs in the head before paint and tolerates blocked
storage. Without JavaScript all usage panels and all catalog entries remain visible;
implementation sections retain native accessible disclosure controls. There are no
numerical algorithms in the site. Explorer components receive serializable
family metadata and call a separately versioned WebAssembly binding in client islands.

Optional browser verification uses an existing Chromium headless shell (auto-detected
in the workstation Playwright cache) or `BROWSER_EXECUTABLE`; it never downloads a
browser. Run `/tmp/umvl-cpu/run python3 site/scripts/browser-smoke.py` from the root.
It pins the browser to one CPU, tests the controls/filter/search/no-JS behaviour,
checks mobile overflow, and writes routine screenshots under `build/scratch/site-browser/`.
The exp, bbinom and burr desktop/mobile migration evidence is retained under
`dev/research/evaluations/2026-11-06-site-swap/`.
During `site-dev`, rerun `just site-data` after changing front matter; production
builds always regenerate it automatically.

Explorer islands use `bindings/js/families.mjs` for coverage and the content
collection for coefficient domains, simulation values and support. The prepare
step builds and copies the local binding into `public/binding/`; neither Svelte
nor WebAssembly is loaded until the island becomes visible. Emscripten is optional:
missing emsdk omits explorers with a warning; `SITE_WASM=0 just site-check` checks
that mode explicitly. Reserve all four slots for builds with Emscripten (`run4`),
since `EMCC_CORES=4` can use all four. Browser checks run separately in one slot.
