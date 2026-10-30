# Inverse Burr / multistart R6 follow-up

Root cause: profiled Burr ranked all finite objectives before inspecting solver
status. Seed 3, n=200 selected a line-search failure just 6.66e-16 below a
converged candidate. Its total free-coordinate score passes R6, but failure
propagated through inverse Burr. Conversely, a bad-score failed candidate could
displace an acceptable candidate.

Fixed interiors: **burr, invburr, betapr, lst, std, ged, emg, bhatt, bnbinom**.
**pareto4** shares the Burr delegate; its limit-status regression is preserved.
Inverse Burr delegates to Burr; its fitter and adapter need no separate edit.
Burr, beta-prime, lst, std and GED use the shared BFGS candidate gate in
`core/multistart.hpp`. Candidates must have finite objective, coordinates and
score and must either converge or have total score norm <1e-3. Invalid starts
are rejected. Rank only acceptable candidates and promote a score-accepted
candidate to Ok. Restore the objective divisor for Burr and normalized std/GED
scores. Joint lst refinements meet their own gate independently of their seeds.
Rejected finite attempts remain available only for diagnostics and the existing
comparisons with independently fitted limits. This preserves Pareto/power,
Poisson and other face statuses, as well as finite estimates explicitly marked
failed. GED still errors on an exhausted interior search rather than returning
its analytic Laplace start as a fit.

EMG/Bhattacharjee Nelder-Mead multistarts now filter by acceptance before
ranking. Exhausted solves can pass the shared finite-difference total-score
gate, using actual log densities and only free fitter coordinates (including
free location), rather than screened face bounds. Beta-negative-binomial now
applies its existing documented average-score/range gate to each candidate
before ranking, rather than to the objective winner alone.

Audit exclusions: **sstd and sged** have the analogous objective-first pattern;
their mle.hpp/adapters were left untouched as requested. The other excluded
families had no matching multistart-status pattern in the inspected headers.
Scalar profile/grid optimizations were distinguished from multistart solves.

Regression: retained the unchanged default-control R seed-3 test. Added a C++
doctest embedding all 200 exact binary64 observations from the saved sample;
it checks Ok, independent-oracle estimates, log likelihood, and the total
profile score norm <1e-3. A shared-gate regression checks score rescue,
objective ranking, normalization, bad-score rejection, invalid starts,
nonfinite gradients, and the no-acceptable-candidate case.

Validation (all builds/suites through `/tmp/umvl-simbox/run`):
- `just cpp-test`: **6/6 CTest targets; 871 cases, 1,209,556 assertions passed**.
- `just quality burr invburr betapr lst std ged emg bhatt bnbinom pareto4`: **10/10 pass**.
- Full uncapped R suite after `just regen`: **FAIL 0 | WARN 0 | SKIP 0 | PASS 32393**.
- Every golden.json matches HEAD byte for byte; no golden/truth regeneration.

No commits, git writes, adapter edits, excluded-family edits, or board edits.
Evidence: `/tmp/umvl-codex/switch/follow-{cpp,cpp-last,quality,r}.log` and
`/tmp/umvl-codex/switch/follow-quality-report.md`.
