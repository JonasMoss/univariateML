# Site swap — TASK-101.6.6

Completed 2026-11-06 in `codex/site-swap`, based on
`a43800a7e906e77bbd41a6d6d64cefd9e2aaee01`, with an uncommitted patch.
No commits, publication, package installs, board edits or R-facing changes.
The [source manifest](2026-11-06-site-swap-data/source-manifest.json) records
the tested source hashes and removed routes.

## Result

The wiki-style layout now serves all 99 families at `/distributions/<id>/`.
Both prototype routes (`/lab/` and `/lab/<id>/`) are removed. Relation and
limit links use the canonical family URLs; the static checker inspects every
rendered internal link and fragment, including catalog, graph, theory,
inference and article pages, and rejects obsolete prototype links.

All 99 binding-enabled families have a linked `/distributions/<id>/explore/`
page with the existing simulation, held coefficients, fitting, diagnostics,
plots and AIC ranking. Each explorer links back to its family and fitting
conditions. The family page retains the miniature plot and wiki infobox.
Language tabs and their persistent preference, implementation controls,
taxonomy and fit-certification notes, fitting notices, article links, full
references, decision history and source evidence remain available. Fit
restrictions and their held-set exemptions also render when an explicit
existence statement is present. Implementation prose has one location rather
than appearing twice.

Pagefind builds one index: 112 indexed pages (99 families plus 13 shared
pages). Explorer pages are explicitly excluded. All 211 generated HTML pages
are checked for resolving local links; no `/lab/` route remains.

## Final validation

All runs below used the **workstation**, through the shared CPU slot locks;
none used simbox. The wrappers pin BLAS/OpenMP to one thread and run at nice
priority. Temporary files stayed in `build/tmp`, with logs in `build/scratch`.
Environment: Python 3.14.5; site Node 22.23.4; shell Node 22.22.1;
Chromium 151.0.7922.34. The site uses build-time MathML.

| Command | Result | Retained evidence |
| --- | --- | --- |
| `/tmp/umvl-cpu/run4 just site-check` | PASS: 99 families, all relation/limit links, 287 article limit records, 9 fit domains, 338 inference scopes, 99 graph nodes/277 edges, 99 C++17 snippets compile and link; inference/theory browser checks pass | [Log](2026-11-06-site-swap-data/site-check.log) |
| `/tmp/umvl-cpu/run4 just js-build` | PASS | [Log](2026-11-06-site-swap-data/js-build.log) |
| `/tmp/umvl-cpu/run just js-test` | PASS: 423 tests; zero failures, cancellations or skips | [Log](2026-11-06-site-swap-data/js-test.log) |
| `/tmp/umvl-cpu/run python3 site/scripts/browser-smoke.py` | PASS: explorer sliders, held fits, failure diagnostics, sorted AIC, language persistence, implementation controls, exact discrete catalog filtering, Pagefind, no-JS usage and mobile rendering; no page errors or external requests | [Log](2026-11-06-site-swap-data/site-browser.log) |
| `/tmp/umvl-cpu/run taskset -c <one allowed CPU> site/node_modules/.bin/node bindings/js/browser-all.mjs` | PASS: all 99 explorers simulate and return a fit or diagnostic; all-family AIC ranking passes | [Summary](2026-11-06-site-swap-data/browser-all.log), [family results](2026-11-06-site-swap-data/browser-all.json), [ranking](2026-11-06-site-swap-data/browser-ranking.json) |

The browser workloads retain seed 123. The all-family and targeted discrete
simulation checks use 30 observations; the existing gamma smoke workload
retains its default 200. Numerical regression tolerances and fixtures are
unchanged. R/C++ numerical suites were not rerun: no fitting, d/p/q/r,
R-facing files or numerical test infrastructure changed. `git diff --check`
passes. No outstanding task items remain.

## Rendering review

Visually inspected the six viewport captures below at desktop 1200×1000 and
mobile 390×844. Desktop uses the two-column introduction/infobox; mobile
stacks the introduction and infobox. Captures show the top viewport; browser
checks also inspect the whole document for overflow with implementation
details open, and expand Burr’s held-limit details.

| Family | What was inspected | Desktop | Mobile |
| --- | --- | --- | --- |
| exp | Simple continuous density, infobox, contents and explorer link | [Capture](2026-11-06-site-swap/exp-1200.jpg) | [Capture](2026-11-06-site-swap/exp-390.jpg) |
| bbinom | Discrete PMF, integer design coefficient, profile/size-tail notes; explorer PMF stems, stepped CDF and seeded fit | [Capture](2026-11-06-site-swap/bbinom-1200.jpg) | [Capture](2026-11-06-site-swap/bbinom-390.jpg) |
| burr | Three-coefficient infobox, limit-model links and held-limit table; expanded implementation and references | [Capture](2026-11-06-site-swap/burr-1200.jpg) | [Capture](2026-11-06-site-swap/burr-390.jpg) |

The review exposed an unbroken reference URL causing Burr mobile overflow;
reference text now wraps. Wide mathematics and code retain local scrolling.
The browser checks were adapted to the current checkbox catalog facets and
the new explorer paths. Discrete SVG checks test visible plot containers,
PMF stems and CDF steps: vertical SVG stems have zero geometric width, so
Playwright’s element-visibility predicate is unsuitable for individual stems.

## Artifact pane limitation

The report and captures are written in this worktree. Opening the report with
the configured `codex-math-web/bin/show` command returned HTTP 403: the viewer’s
readable roots include the main checkout but omit this worktree. No viewer
configuration or files outside this worktree were changed. The worktree must
be registered with the viewer to display the report in its artifact pane; this
does not affect the completed site or validation results.
