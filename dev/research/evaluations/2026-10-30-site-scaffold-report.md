# Site scaffold — TASK-101.6.2 (2026-10-30)

Source: `codex/site-scaffold`, base `25bf4767a076ffe865a0de0962919c1aaa0890b7`; uncommitted work.
No numerical algorithms, defaults, goldens, R sources or C++ headers changed.
No commits, publishing, package installs outside `site/`, or board edits.

## Structure and choices

- Astro 7.4.4 (npm stable tag checked on the task date), Svelte integration,
  static pages and Pagefind. Dependencies and local Node/npm are pinned and locked.
  Local Node 22.23.4 avoids changing the workstation's older Node installation.
- The content collection reads the 98 canonical README dossiers in place.
  Ignored JSON holds only derived presentation data; there is no authored catalog.
- Python reuses `quality.density.density_expression`, using declarative `log_density` rather than numerical `spec.py` oracles.
  It derives LaTeX and uses a deterministic conservative density readability rule:
  at most 55 symbolic operations and 700 LaTeX characters, with no unevaluated
  sums/integrals or piecewise density. Nonlinear logarithmic kernels are retained.
  Formulas are restricted to the dossier support and coefficient domains.
- One unified Markdown pipeline renders prose, notices and formulas to MathML.
  System serif fonts, local assets and no runtime math engine keep pages small.
- Notice wording is centralized in `src/lib/notices.ts`; required values, structural
  hold exceptions, limits and fitting restrictions retain their metadata scope.
- Usage comes from dossier R examples and real C++ fitter signatures; example
  observations come from existing golden fixtures, not invented family facts.
  Compilation checks signatures, not fit success or statistical validity.
- Implementation panels and decision history are native disclosures. Head scripts
  apply persistent language preferences before paint and tolerate unavailable storage.
  Without JavaScript all usage panels and catalog entries remain visible.
- Optional articles are registered in `articles.config.mjs`; the validation guide
  is included only when its source exists. Repository evidence links are local;
  existing file paths in inline code also become source links. Missing authored
  pointers remain text rather than invented links. Sources remain canonical.
- `site/README.md` documents bootstrap, commands, hosting assumptions and boundaries.
  `site-data`, `site-build`, `site-dev`, `site-check` are the only Justfile additions.

## Validation and measurements

Ran through `/tmp/umvl-cpu/run`, nice priority, one thread per slot; TMPDIR,
JUST_TEMPDIR, XDG_RUNTIME_DIR and npm cache stayed under `build/`.
Clean `npm ci --prefix site --no-audit --no-fund` passed (host-engine warnings
expected; recipes use the installed local Node). Python 3.14.5, SymPy 1.14.0,
g++ 15.2.0, Boost 1.90 from existing BH headers, Node 22.23.4, npm 12.2.1.
No R runtime or R/C++ numerical suites were used.

Final `just site-check` output (full log: `build/scratch/site-check-final.log`):
```text
PASS: 98 family pages; MathML on every family; no math runtime or external runtime assets; internal links and fragments resolve.
PASS: 98 generated C++ snippets compile and link as C++17 (including exp, gamma, gb2).
PASS: pilot and ten additional HTML spot checks: exp, gamma, gb2, binom, beta, hyper, sstd, gev, gpd, tnorm, pois, unif, pareto
```
All commands finished green. Final warm build: **4.33 s**; data generation 1.24 s,
Astro 1.63 s, Pagefind about 0.3 s (102 indexed pages). Final cached site-check:
**10.60 s**; earlier cold compile-and-link run: 134.07 s. Slot waiting is excluded.
Existing Chromium 151.0.7922.34 passed `python3 site/scripts/browser-smoke.py`:
persistent language, implementation switch, Svelte name/measure filters, Pagefind
results, no-JS panels and mobile overflow; no page errors or external requests.
Inspected desktop Gamma/mobile GB2 screenshots and all three pilot HTML files.
The real validation guide is absent here; both omission and a temporary registered
article fixture passed site-check. The fixture also checked inline/display MathML
and a canonical dossier link. The default registry was restored and rebuilt.

Raw bytes per page; JS includes inline code already counted in HTML and local
module dependencies. Lazy search index/wasm transfers are included only in total dist.
| Page | HTML | CSS | JS |
|---|---:|---:|---:|
| Landing | 3,379 | 4,240 | 1,239 |
| Exponential | 6,564 | 4,240 | 1,239 |
| Gamma | 9,010 | 4,240 | 1,239 |
| GB2 | 12,933 | 4,240 | 1,239 |
| Skew Student-t | 36,968 | 4,240 | 1,239 |
| Catalog | 100,741 | 4,240 | 49,623 |
| Search | 2,773 | 18,722 | 121,282 |
All family HTML sizes span 5,825–36,968 B; each uses the same CSS/JS above.
Total dist: **2,288,311 B (2.18 MiB)**, including local evidence and Pagefind.
Every page's weights are reproducible in `build/scratch/site-weights.json`.

Log-density fallback (21): cmb, cmp, dweibull, gev, gpd, huber, sged, snorm,
sstd, tbinom, tpois, triang, zib, zinb, zip, zipf, zmbinom, zmgeom,
zmlogarithmic, zmnbinom, zmpois. These retain piecewise or normalizer expressions.
Odd notices: sstd's 58 limit records yield a long block with repeated conditional
faces; GB2 has named faces with no package replacement. Both preserve the dossier
scope instead of inventing a model. Exp/gamma non-existence conditions live only
in prose and therefore have no metadata-generated notice. Later dossier work
could add those records. No family-specific notice exceptions were hard-coded.
The renderer normalizes adjacent multiline display fences (dgamma/dweibull).
Potential core follow-up, outside this job: tlambda's isolated header emitted
undefined-inline gamma warnings; examples include the public special seam first,
and all 98 now compile and link without those warnings. No core header was edited.

## Metadata and later stages

Wanted dossier fields: `summary`, `classification`, `key_references`, `aliases`.
The scaffold consumes these when present; otherwise it uses the first prose
paragraph and existing references, and omits the absent classification/aliases.
No classification, attribution or references were invented or dossiers edited.
More structured support-bound math and explicit C++ binding metadata would remove
presentation heuristics; any schema/header/codegen changes belong to later cards.

Stage 3 can mount explorer islands between Definition and theory, passing derived
serializable metadata separately from prose. It needs the versioned WebAssembly
binding, explicit model/signature coverage, status-to-diagnostic mapping, lazy local
module/wasm loading, and validated domain/support resolution for sliders and plots.
Fitting must preserve core failure statuses; the site must not reimplement fitters
or turn a failed/capped solve into an estimate. Browser interaction/accessibility
and wasm/golden checks then supplement the static scaffold checks.

Hosting is intentionally unconfigured: output assumes an origin-root deployment;
subpath hosting needs Astro base/link updates before release. Nothing was published.

Artifact preview: `build/scratch/site-preview.html` (self-contained Gamma export).
The pane's `show` API returned 403: this worktree is outside its readable roots;
no viewer configuration or files outside the permitted worktree were changed.
