# Shared theory articles — round 2, 2026-11-01

Source revision `fd905f1` plus this worktree's uncommitted changes. The four articles are [existence](../../../univariateML-cpp/notes/existence.md), [limits](../../../univariateML-cpp/notes/limits.md), [holds](../../../univariateML-cpp/notes/holds.md), and [restrictions](../../../univariateML-cpp/notes/restrictions.md). They contain approximately 1,117, 886, 726 and 707 words respectively, excluding the generated tables. Existing article registration, family links, and dossier-derived tables remain in place.

## Results and claim grades

- **Proved, under the stated hypotheses:** exponential-family convex-support and gamma, beta and Poisson examples; fixed-dimensionless-shape location-scale concavity, with attainment and uniqueness for EMG and Bhattacharjee; Weibull and logistic nonconstant-sample results; the Cauchy strict tie inequalities and two-point equality example; beta-binomial strict-overdispersion sufficiency and size-two necessity; the finite-window truncated-binomial tail bound; and Jonas Moss's Lomax criterion. Proof sketches and precise source links appear alongside the results. The fixed-shape result does not certify the outer profile or physically held rate and width.
- **Conjecture:** for beta-binomial fixed size at least three, underdispersion or equality with an interior count is conjectured to exclude a finite maximum. The cited investigation found no proof or counterexample.
- **Numerical:** the tbinom hybrid's completed certificate uses floating-point comparisons and tolerance; budget and other fallbacks remain uncertified. The Lomax search lacks a global certificate and uses a near-tie convention. The negative-binomial large-size diagnostic also has a documented certification gap. These are distinguished from the exact mathematical theorems.
- **Limits and interpretation:** the Hellinger/total-variation inequalities, weak continuous collapse, Student-to-normal, Lomax-to-exponential, negative-binomial-to-Poisson and endpoint examples are stated with scope. The KL projection interpretation explicitly requires suitable expectations and does not follow from Hellinger convergence alone. The held truncated-normal example derives its uniform face and distinguishes it from the free-mean truncated-exponential limit.
- **Policy:** R1–R8, the eight fit-domain declarations, the four restriction mechanisms and held-set exemptions are identified as policy or family-specific findings, rather than universal regularity or inference claims.

## Omissions and open matters

Individual density formulas, complete family-specific proofs and fitting algorithms remain in the dossiers and research source. The limit table groups records into package models and other limits or failure boundaries because the schema has no universal analytic edge classification. The open fixed-size beta-binomial necessity for $N\ge3$, free integer-size tail certificates, and globally certified Lomax search remain open; implementing fixes was outside this documentation task. Numerical tests do not prove global likelihood statements. A full numerical suite was not required for the prose-only second round.

## Visual review

Inspected the existence article at 1280 × 1000 and 390 × 844 viewport sizes, including close-ups of the Lomax proof. Both layouts were legible and the browser reported no page-level mobile overflow. MathML's `log q` and `log A(q)` now have explicit thin spaces; the browser measured approximately 3.61 pixels between each operator and argument at desktop size. New screenshots use distinct names so the round-one captures remain unchanged.

- [Desktop article](2026-11-01-theory-articles/round2-final-existence-desktop.png) and [desktop proof](2026-11-01-theory-articles/round2-final-proof-desktop.png)
- [Mobile article](2026-11-01-theory-articles/round2-final-existence-mobile.png) and [mobile proof](2026-11-01-theory-articles/round2-final-proof-mobile.png)

## Validation

On the workstation, `TMPDIR`, `JUST_TEMPDIR` and `XDG_RUNTIME_DIR` were set under `build/tmp`; the shared `/tmp/umvl-cpu/run4` wrapper used four slots, `nice -n 10`, and one-thread BLAS/OpenMP. The first round-two run executed the commands consecutively in `build/scratch/theory-round2-runs.sh`:

- `just cpp-test`: **pass**, 105/105 dev shards.
- `just test`: **pass**, 19,843 passes, 151 skips, zero failures and warnings.
- `just site-check`: **pass**, including four rendered articles, 264 limit rows, 98 held records, eight fit domains, links and fragments, build-time MathML, and 98 compiled C++ examples.
- `python3 dev/tools/check_links.py`: **pass**, 333 Markdown files, 549 local destinations, zero broken before this report update.
- Headless browser: **pass**, desktop and mobile captures, visible operator gaps, and no mobile page overflow.

After the initial screenshot showed `log A(q)` too close together, the article was adjusted and `just site-check`, the link checker, and the browser capture were rerun consecutively through `run4`; all passed. The C++ and R suites preceded that final prose-only spacing adjustment. Python 3.14.5, SymPy 1.14.0, g++ 15.2.0; Boost headers from installed BH. Logs are in ignored `build/scratch/theory-round2-*.log`. The artifact viewer rejected this worktree path as outside its configured readable roots; the report and screenshots remain available at the links above.
