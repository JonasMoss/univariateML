# TASK-102.10: vcov user documentation

Completed 2026-11-01 in `codex/vcov-docs`, based on
`65da739faecfaaa5728156aaec5c56d777e9397e`, with uncommitted documentation changes.
No code, default fits, goldens, generated d/p/q/r, NEWS, status, known-issue or
roadmap sources changed. No commits, board edits, package installations or
sub-agents were used. All writes stayed in this worktree.

## Delivered documentation

- [R vignette](../../../bindings/r/vignettes/inference.Rmd): fast seeded normal
  fit; default sandwich and observed covariance; standard errors; returned
  condition and validity attributes; held mean and all-held examples; a caught
  unsupported uniform-endpoint covariance error. It explains retained data,
  nuisance uncertainty, error categories, the scale-dependent conditioning
  warning above 1e6, and unreliable Wald inference near skew-normal alpha = 0,
  Bhattacharjee a = 0, weak identification and heavy tails. Supported conditional
  boundary submodels are distinguished from unsupported boundary fits.
- [Theory article](../../../univariateML-cpp/notes/inference.md), registered in
  [article configuration](../../../site/articles.config.mjs), renders at
  `/articles/inference/` and links to `/inference/`. It separates DQM, finite
  information, rank, Hessian suitability and estimator asymptotics. Formulas use
  free natural coordinates, total observed information H and per-observation
  cross-products J: observed covariance H inverse, sandwich H inverse times J
  times H inverse transpose, with no extra sample-size division. Held values
  are known; estimated nuisance coefficients remain in the joint covariance.
- [JS README](../../../bindings/js/README.md): a runnable gamma example for both
  covariance types and named standard errors, with sample-size normalization,
  conditioning and statistical limitations explained alongside the API contract.

The theory article summarizes the retained C++ report and simbox follow-ups,
including failures and conditioning on successful replications. It records the
near-singular sn/bhatt findings and beta-negative-binomial weak identification
at n=500 with improved agreement at n=5000. These are numerical comparisons of
standard errors with sampling variability, not DQM proofs, coverage experiments
or misspecification-robustness guarantees. Infinite observation moments alone
are not evidence of infinite score moments.

## Completed validation

Every build and test ran locally on the workstation through the shared slot
wrappers, in the foreground, and was awaited to completion. No run used simbox.
The wrappers supplied nice priority 10 and single-thread OpenMP/BLAS; `run4`
reserved all four slots. Temporary/runtime paths were `build/tmp`; logs and
scratch scripts are under `build/scratch` and are intentionally ignored.

| Command or check | Result |
|---|---|
| `/tmp/umvl-cpu/run4 just test` | PASS: 20,094 assertions, 0 failures, 0 warnings, 151 expected dev-tier skips |
| `/tmp/umvl-cpu/run Rscript build/scratch/render-vcov-docs.R` | PASS: new vignette rendered with all chunks evaluated; rerun passed after final boundary prose clarification |
| `/tmp/umvl-cpu/run4 just site-check` | PASS: 99 family pages, inference catalogue, internal links, graph, explorer checks, MathML and 99 compiled C++17 snippets |
| `/tmp/umvl-cpu/run node --v8-pool-size=1 build/scratch/vcov-docs-example.mjs` | PASS: README gamma example, both types, expected names and finite positive standard errors |
| Built article and vignette HTML inspection | PASS: MathML present, no MathJax/KaTeX runtime script; theory links to `/inference/`; final vignette contains current wording and evaluated diagnostic |
| `git diff --check` | PASS |

`just test` runs testthat and does not render vignettes in this checkout. The
additional explicit render used `pkgload::load_all("univariateML-r")` followed
by `rmarkdown::render("bindings/r/vignettes/inference.Rmd", output_dir =
normalizePath("build/scratch"), quiet = TRUE)`. The source selects
`rmarkdown::html_vignette`, `mathjax: null` and Pandoc `--mathml`. The only
change after the R suite was the boundary prose clarification; the final
explicit render evaluated every vignette example again.

The JS example used `[0.5, 0.8, 1.1, 1.6, 2.2, 3.0]`, with no random sampling.
Sandwich standard errors were shape 1.0731558203977516 and scale
0.1902028290415494; observed-information condition number was
222.10662735443614, with no returned warning. This checks the published example,
not calibration from six observations.

The site check reported Python 3.14.5, SymPy 1.14.0 and GCC 15.2.0, using the
existing BH Boost headers. It rebuilt the Wasm binding. An existing binomial
self-comparison compiler warning and shell notices about an absent optional
Cargo environment file appeared; neither caused a failed check. R test warnings
were zero. No full fitting suite was required because this job changes only
documentation and article registration.

Logs: `build/scratch/vcov-docs-r-test.log`, `vcov-docs-vignette.log`,
`vcov-docs-site-check.log`, and `vcov-docs-js-example.log`. Long waits for shared
slots were allowed to finish without interruption.

## Operational notes and remaining limits

The prescribed `cp -al` dependency copy failed with cross-device-link errors.
A normal `cp -a` copy of the existing site dependencies into this worktree
succeeded; no packages were installed.

The finished vignette HTML is `build/scratch/inference.html`; the built theory
page is `site/dist/articles/inference/index.html`. Attempts to show the
vignette and this report in the artifact pane returned HTTP 403 because the viewer's configured
readable roots omit this worktree. It was not copied outside the authorized
worktree. Pane display remains an environment limitation: the viewer needs this
worktree in its readable roots. The built files themselves passed validation.

No required documentation item remains open. Statistical evidence gaps described
in the source reports remain explicit: pointwise derivative checks and Monte
Carlo standard-error comparisons do not establish universal Wald coverage,
robustness under misspecification or validity near singular regimes. This job
adds no new mathematical proof or calibration study.
