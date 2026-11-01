# Theory visualisations — TASK-124.3

Completed 2026-11-01 in the `codex/theory-viz` worktree, against source revision
`fd905f1b1c155ad03c6dd2305124f3156421788e` plus the uncommitted changes described
here. All four requested pieces are complete. No commits, board edits, package
installs or simbox jobs were made.

## Delivered

The new **/theory/** route, linked from the header, contains:

1. A profile viewer with gamma, Lomax, Student t, negative binomial and conditional
   Burr presets. Each of 65 logarithmically spaced coefficient values is passed
   to the existing WebAssembly held fitter, which refits the remaining
   coefficients. The free fit's coefficients and log-likelihood are printed;
   a dot marks it when it is within the plotted range. A declared limit model's
   maximised likelihood is shown as a dashed horizontal line.
2. An escape gallery for Lomax → exponential, Student t → normal, negative
   binomial → Poisson and Burr → Pareto. All four presets actually produce the
   existing fitter's “use mlX” diagnostic. Each card can recompute its preset
   after lazy hydration.
3. Normal likelihood contours with the free fit, mean-held fit and sd-held fit
   marked, and the two held faces drawn. Users can change the held values,
   paste data or simulate a sample. The plot range includes the held fits.
4. Six build-time SVG figures (one viewer, four escapes, one contour), together
   with diagnostics, samples and numerical tables available without JavaScript.
   Hydration adds fitting controls. Loading failure retains the default content.

The viewer and normal example accept 3–500 pasted finite observations, reset to
presets, or simulate 60 observations with the displayed seed (default
20261101). Integer support and parameter-domain checks remain the binding's
responsibility. Invalid edits retain the last figure and show a diagnostic.
Failed held fits produce gaps and their messages, never substituted estimates.
Nonfinite or failed fit results are rejected by the profile presentation layer.

The endpoint example is **conditional**: Burr scale is held at 1, and the
comparison Pareto fit holds its lower endpoint `b` at 1. Its path has shape2
tending to infinity and shape1 × shape2 tending to the Pareto shape. Comparing
with a freely fitted Pareto endpoint would be a different comparison. The
checker verifies this held-scale limit against the canonical dossier.

Implementation: [route](../../../site/src/pages/theory/index.astro),
[component](../../../site/src/components/theory/Viewer.svelte),
[evaluation and SVG functions](../../../site/src/components/theory/model.mjs),
[numerical checks](../../../site/src/components/theory/check.mjs), and
[browser checks](../../../site/src/components/theory/browser.mjs).
Small changes to `site/scripts/check.py` integrate acceptance checks, and the
header adds Theory with wrapping navigation. No fitter, binding, golden,
explorer, family page, lab page or article was changed. Dependencies were copied
from the existing checkout as instructed.

## Numerical evidence and interpretation

Default gamma fit: shape 1.865186233839255, scale 1.1303607624175895,
log-likelihood −19.79471016670053. Every sampled gamma profile value is below
this free-fit likelihood (allowance 1e-7).

All four escape grids had **zero failed held fits**. At coefficient 10000:

| Preset | Maximised limit log-likelihood | Last profile minus limit | Fitter directs to |
|---|---:|---:|---|
| Lomax, 40 midpoint observations on (0, 2) | −40 | −0.001333727764 | mlexp |
| Student t, 40 symmetric midpoint observations | −34.772791646946345 | −0.001201513742 | mlnorm |
| Negative binomial, 12 counts repeating 1, 2, 3, 2 | −16.89862915760515 | −0.000899945080 | mlpois |
| Burr, 40 Pareto quantiles, scale held at 1 | −31.75431378039618 | approximately −4e-14 | mlpareto |

The checks require the final gap to be below 0.02 log-likelihood units and
smaller in absolute value than the first grid point's gap. These are numerical
checks of the displayed examples, not proofs of monotonicity, global
optimality or escape. The limit paths are the dossier declarations; the
messages are existing fitter decisions. No new mathematical theorem or
numerical maximisation algorithm is introduced. A grid endpoint is never
reported as an estimate.

The normal surface sums binding log densities on a 65 × 55 grid. Marching
squares interpolates contours at 0.5, 2, 5, 10 and 20 log-likelihood units below
the free fit. Closed-form checks verify the free mean and sd, the held-mean sd
`sqrt(mean((x - held_mean)^2))`, and the held-sd mean to absolute tolerance
1e-12. Held-fit likelihoods must not exceed the free likelihood. A distant
held mean also checks that fitted markers remain inside the plotting range.
These contours have no confidence-region interpretation.

Static/live SVG equality tests establish deterministic presentation of the
same fitting API. They are not independent validation of those fitters; the
existing JS, C++ and R suites provide separate regression evidence. Default
figures are computed using the built binding during Astro generation; building
this route requires the WebAssembly build, while viewing its defaults requires
no JavaScript or WebAssembly execution in the browser.

## Final validation

All builds/tests ran in the foreground under `/tmp/umvl-cpu/run` or `run4`,
with workstation thread limits, niceness and worktree-local temporary files.
The final commands were run under `run4` with:

```sh
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp
export XDG_RUNTIME_DIR=$PWD/build/tmp
just js-build
just js-test
just cpp-test
just test
just test-full
just site-check
```

| Command | Final result |
|---|---|
| `just js-build` | Pass, 98 generated families |
| `just js-test` | 400 passed; 0 failed, cancelled or skipped |
| `just cpp-test` | 105/105 dev shards passed |
| `just test` | FAIL 0; WARN 0; SKIP 151; PASS 19843 |
| `just test-full` | FAIL 0; WARN 0; SKIP 1; PASS 41289 |
| `just site-check` | Pass, including theory numerical/browser checks, existing inference browser checks, graph checks, all links/runtime checks and 98 C++17 snippets compiling/linking |

The full R tier's sole skip is the optional independent `{sn}` comparison
(`test-dist-sn.R:136`), because that package is absent. No package was installed.
The dev tier's 151 skips match the supplied baseline. There were no permitted
failures. Native no-long-double tests were not rerun for this site-only change.

One first site-check attempt failed in the new browser test: Playwright waited
for an offscreen disabled gallery button before scrolling, so `client:visible`
never hydrated it. The test now explicitly scrolls the button into view before
clicking. The targeted browser recheck and subsequent complete `just site-check`
both passed; the latter finished on 2026-11-01 at approximately 09:10 Oslo time.
The already-passing numerical suites were unaffected by this test-only fix.

Environment: Node 22.23.4 for site/browser checks, Emscripten 6.0.13,
Python 3.14.5, SymPy 1.14.0, g++ 15.2.0, R 4.6.1, and installed BH headers.
Final logs are under `build/scratch/theory-final-*.log`; the successful site
rerun is `build/scratch/theory-final-site-check-rerun.log`. Reproduction is also
available through the checked-in scripts called by `site-check`.

## Visual inspection

Playwright captured desktop (1280 × 1050) and mobile (390 × 844) screenshots
of all three sections and their plot close-ups, plus a mobile no-JavaScript
view: 13 images under `build/scratch/theory-viz/`. These were inspected. The
final page has a bounded reading width, wrapped controls/navigation and no
horizontal overflow; extra plot margin separates vertical titles from tick
labels on mobile. The free and held markers and the dashed limit lines are
visible in the final figures.

Browser checks cover hydration, all four gallery recomputations, family
changes, pasted data, seeded simulation reproducibility, reset, held-value
changes, invalid data and invalid held sd, preservation of the previous plot,
all six static figures and four escape messages without JavaScript, numerical
table disclosure, and absence of page errors. Numerical failure injection also
checks that an entirely failed profile displays diagnostics rather than a curve.

The screenshots were working files in the job worktree and were not kept; the
figures are reproduced by `just site-check` (build-time SVG on `/theory/`).

Open requested items: none. The optional `{sn}` reference comparison remains
unexecuted as recorded above; the finite plotting grids provide illustrations,
not additional mathematical guarantees.

Artifact-pane delivery was attempted with the prescribed `bin/show` command.
The viewer returned HTTP 403 because this worktree is outside its configured
readable roots. The report and screenshots remain in this worktree; viewer
configuration and files outside the worktree were not changed.
