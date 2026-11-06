# ZINB certified fit — TASK-116.7

ZINB now declares `fit.certified: 'yes'`. Its default C++ fitter, R binding and JS binding cover every supported hold subset: a successful estimate has a global certificate and coefficient enclosure; an unresolved calculation stops with structured diagnostics. The historical mesh and likelihood-near-tie rule no longer decide existence or accept a fit.

All required final runs passed, including both full tiers. The existing golden coefficients are preserved, and `golden.json`, `dpqr.hpp`, `truth.py` and `truth.json` are byte-identical to the starting revision. No commits, board edits, package installations, pushes, main-checkout edits or other-worktree edits were made.

The base revision is `f30584cd1819744e130850bcb1e58a5d1c52ed28`, branch `codex/zinb-cert`. The tested uncommitted inputs are identified by [source-sha256.txt](2026-11-06-zinb-cert-data/source-sha256.txt), whose SHA-256 is `59516b2506de035bdaa3b87c54ee49e8a61b5bbb2a8a97ad6cc43083200df235`. The frozen inputs were checked again after the final suites. [validation.json](2026-11-06-zinb-cert-data/validation.json) records commands, machines, environment, counts, timings and independent-check summaries.

## Implementation and mathematical scope

The canonical [ZINB dossier](../../../univariateML-cpp/include/cppunivariate/distributions/zinb/README.md) contains the objective, derivatives, branch arguments, exterior bounds, floating-point margins, budgets and stopping rules. The public [mle.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/zinb/mle.hpp) retains the former numerical method as `zinb_candidate_mle`; [certificate.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/zinb/certificate.hpp) implements scalar certificates and [global_certificate.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/zinb/global_certificate.hpp) implements joint coverage and face comparisons.

| Held coefficients | Certified route |
|---|---|
| None | Joint finite/ZIP certificate, zero-deficit NB reduction, and certified positive-count/NB comparisons when needed |
| `pi` | Joint rectangle coverage and a ZIP boundary with the same inflation; `pi = 0` delegates to certified NB |
| `size` | Analytic inflation profile and a bracketed probability solve |
| `size, pi` | Exhaustive probability branches, with all isolated maxima compared |
| `prob` | A globally concave profiled size likelihood |
| `prob, pi` | Finite analytic size brackets and complete score/curvature segment coverage |
| `size, prob` | Analytic inflation maximizer |
| All three | Compatible likelihood evaluation |

All-zero data retain the attained representatives and supplied holds. Their free coefficients can be unidentified. Incompatible positive observations at a held zero-law endpoint stop as input errors. An attained NB member at `pi = 0` remains a successful fit.

For held size and interior held inflation, the probability score has at most three roots. A quadratic isolates its monotone branches, so both competing maxima are retained rather than trusting one root or an inner mesh. At held probability, reciprocal-sum and posterior-zero-weight bounds cover the entire size bracket. With inflation free, the NB and truncated-NB size scores meet continuously and are each strictly decreasing; this proves uniqueness for that held-probability subset. The held-size/free-inflation subset is also unique for positive data.

The joint certificate uses `t = -size * log(prob)` and works in `(log(size), log(t))`. Coercivity bounds exclude small size, small `t` and large `t`. A uniform comparison against the certified ZIP supremum closes the large-size tail when a finite witness wins. Hessian interval bounds then give Taylor likelihood upper bounds on every remaining rectangle. A successful cover excludes every point outside a strictly concave central rectangle; the padded score and negative-Hessian eigenvalue bound enclose its unique stationary maximum. A certified truncated-NB fit can supply one rescue start, which must pass this same joint certificate.

A separate signed-tail argument can select ZIP. Its first-order coefficient is `C = A - S * lambda / 2`, where `A = sum(x * (x - 1) / 2)`. A negative coefficient is combined with rate-region bounds and complete finite-rectangle coverage. It is never sufficient by itself. Held-inflation ZIP rates are also exhaustively isolated: their stationary-branch turning points solve a quadratic and can expose two maxima. Free-inflation ZIP uses the monotone conditional-mean solve.

With all coefficients free, stationary positive inflation matches both the empirical zero share and empirical mean. Mixed-Poisson Jensen inequalities therefore exclude positive-inflation stationary maxima under a padded empirical zero deficit. The remaining comparison is the certified NB fit and its Poisson face. Other positive-count reductions use the certified truncated-NB profile only when its coverage establishes that no feasible finite stationary maximum was omitted. An unresolved required delegate is not discarded.

These are exact-arithmetic certificates conditional on the computed comparisons, with stated practical binary64 margins. They are not outward-rounded intervals or universal libm error proofs. The main joint allowance is `4096 * epsilon * (1 + absolute term scale) * (1 + maximum_count * epsilon)`. Recurrences use compensated sums and bounded small-argument series. Near-point-mass calculations use positive-mass complements, stable indicator logarithms and output-representation guards. Uncertain joins, tangencies, arithmetic resolution and workload limits stop.

New joint root enclosures enforce approximately `2e-7` log-coordinate/absolute-inflation accuracy before any historical proposal is retained within an additional `2e-7`; delegated NB fits retain their parent's documented enclosure. Scalar brackets and analytic rounding enclosures are specified separately in the dossier. Existing golden tolerances were not relaxed.

## Stops, existence and uniqueness

`ZinbResult` appends `StopDetail` and a certificate-failure enum while retaining existing aggregate-field positions and status values. R and JS receive `limit`, `tie`, `budget`, `numerical` or `unresolved` conditions, with computed witnesses, likelihood bounds and named limiting coefficients. ZIP uses `(lambda, pi)` and preserves held inflation; Poisson uses `lambda`. Unclosed global upper bounds remain infinite. A last rectangle bound is explicitly local. An unresolved final certificate stops; a score exception never converts its witness into a fit.

Finite held size or held probability below one gives attainment for compatible data. General joint uniqueness remains unknown. Successful joint central-box certificates establish uniqueness for the particular sample; they do not assume a family-wide unimodality theorem. General held-size/held-inflation uniqueness is false: 99 zeros and one count of ten at size ten have two probability maxima and a crossover near inflation `0.7683947756875808`. Analytic branch counts and checked opposite peak-height signs establish an exact tie somewhere in the crossover interval; the displayed location is numerical. The returned tie class also includes comparisons unresolved at the numerical margin.

The default joint finite and face covers each allow `64 * min(iterlim, 100000)` rectangle evaluations; a rescue can repeat the finite cover. The held-probability scalar cover uses `32 * min(iterlim, 100000)`, and the positive-count delegate has its own budget. Reported work carries delegated work through stops. Counts above 100000, total counts above `2^52`, or unrepresentable/unresolved coordinates cause numerical/workload stops. No capped last iterate is returned as an estimate.

## Independent evidence and near-face behavior

The retained [scalar checker](2026-11-06-zinb-cert-data/check.py) uses seed 1167 and 70-digit mpmath arithmetic. Its [91 workloads](2026-11-06-zinb-cert-data/oracle.json) returned 90 finite fits and one expected comparison tie. Maximum relative coefficient error was `1.18e-10`, maximum absolute inflation error `4.11e-13`, and maximum total-likelihood error per observation `1.93e-15`. It also checked 288 uniform ZIP-tail inequalities.

The [joint checker](2026-11-06-zinb-cert-data/check_global.py), seed 11671, covers every golden hold subset, seeded joint samples, held-inflation faces, deliberately tiny budgets, competing modes and near-face regressions. Its [49 workloads](2026-11-06-zinb-cert-data/global-oracle.json) returned 31 finite fits, seven ZIP limits, five Poisson limits, five budget stops and one expected tie. Three budget stops deliberately use `iterlim = 1`. All successful coefficient checks passed at `2e-6`; the maximum observed relative error was `1.99e-7`, inflation error `3.98e-8`, and likelihood error per observation `5.52e-15`. The checker independently differentiates the likelihood in high precision and checks 216 Hessian/Taylor points, 1,944 ZIP-face rate-grid values and 288 signed first-order tail bounds. These numerical checks validate implementation details; global coverage follows from the analytic inequalities and branch arguments.

Two near-face workloads distinguish safe stopping from an existence claim:

* The seeded 5000-observation ZIP regression has first-order coefficient approximately `-47.5793041`. At the default budget it stops rather than asserting a ZIP winner. With `iterlim = 2000`, the required coverage completes and certifies ZIP; 136,333 combined evaluations are recorded. The negative coefficient alone had not proved that conclusion.
* The proved-finite regression has 5000 zeros, 3958 ones, 4063 twos and 1979 fours. Its positive conditional mean is two, its ZIP rate is approximately `1.5936242600400401`, and `C` is approximately `+0.7573995996`. The signed rate bracket in the C++ regression gives `C > 0.69`, and the analytic lower tail bound proves a finite likelihood improvement over the globally maximizing ZIP face. The historical proposer nevertheless reports `ZipLimit`. The new default reports a budget stop, preserving the face witness without returning the unresolved coefficients or claiming non-existence.

[Retries](2026-11-06-zinb-cert-data/global-retries.json) at `iterlim = 2000` and `20000` still stop on the proved-finite case, with 31,999 and 319,999 recorded delegate evaluations. **Open:** a sufficiently tight positive-count profile cover and joint coefficient enclosure for this concrete near-face maximum. Finer residual-score bounds or better-conditioned mean/dispersion coordinates are needed; increasing the tested budget alone did not settle it. This is a completion limitation, not an unsupported hold subset or an unchecked successful fit. General joint uniqueness and universal floating-point rounding guarantees also remain open.

The ZINB R held-subset oracle now recognizes classified terminal certificate stops; every successful fit still undergoes its independent likelihood comparison. Native, R and JS tests check competing-maximum payloads, budget/workload errors, preserved-inflation ZIP limits and Poisson coefficients. The near-point-mass zero-likelihood and zero-deficit delegate diagnostics have dedicated regressions.

## Timings

[Timing source](2026-11-06-zinb-cert-data/timing.cpp) and [measurements](2026-11-06-zinb-cert-data/timing.json) use C++17 `-O2`, one reserved workstation CPU, and the 500-observation golden. Values are median per-fit microseconds over three batches of 100 fits. Both paths use the current result layout; the comparison path is the retained historical proposer.

| Held coefficients | Retained proposer, µs | Certificate/default, µs |
|---|---:|---:|
| size | 9.36 | 5.62 |
| size, prob | 3.29 | 4.80 |
| size, pi | 74.85 | 5.54 |
| size, prob, pi | 3.07 | 4.57 |
| prob | 277.81 | 31.29 |
| prob, pi | 277.17 | 32.11 |
| None | 397.24 | 3667.31 |

The all-free certificate costs about 9.2 times the retained proposal on this workload. Several scalar branches are faster because their certificates replace nested mesh refinements. Work grows with recurrence length, conditioning and subdivision budgets.

## Final validation

All final numerical and binding inputs remained unchanged during these runs. The workstation used GCC 15.2.0, R 4.6.1, Rcpp 1.1.2, BH 1.90.0-1, Python 3.14.5, mpmath 1.3.0 and Node 22.22.1. Simbox used GCC 13.3.0 and R 4.6.1. Builds and test workloads used the supplied CPU reservations/runner, pinned BLAS/OpenMP threads and nice scheduling. No exit-75 fallback was needed.

| Final command | Machine | Result | Runner wall, s |
|---|---|---|---:|
| [just cpp-test](2026-11-06-zinb-cert-data/cpp-dev.txt) | simbox | 106/106 shards | 120.56 |
| [just cpp-test-no-long-double](2026-11-06-zinb-cert-data/cpp-no-long-double.txt) | simbox | 127/127 shards | 126.78 |
| [UMVL_TEST_TIER=full just cpp-test](2026-11-06-zinb-cert-data/cpp-full.txt) | simbox | 127/127 shards | 58.37 |
| [just test](2026-11-06-zinb-cert-data/r-dev.txt) | simbox | 20,845 pass; 0 fail/warn; 154 tier skips | 209.38 |
| [just test-full](2026-11-06-zinb-cert-data/r-full.txt) | simbox | 43,665 pass; 0 fail/warn/skip | 280.83 |
| [just js-build](2026-11-06-zinb-cert-data/js-build.txt) | workstation | pass | 42.80 |
| [just js-test](2026-11-06-zinb-cert-data/js-test.txt) | workstation | 432/432; 0 fail/skip | 105.71 |
| [just site-check](2026-11-06-zinb-cert-data/site-check.txt) | workstation | pass | 65.44 |

Runner wall times include reservation waits, mirroring, regeneration and builds. The final C++ test-only times were 38.46 s (dev), 49.10 s (no-long-double) and 53.30 s (full). R dev's 154 skips are tier gates; full has no skips. There are no permitted failures in the final runs. The no-long-double build uses `BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS`.

The site check passed all 99 generated C++17 snippets and its page/browser checks. The ZINB page contains 160 build-time MathML elements and no MathJax script. A legacy screenshot rewritten by the site check was restored to its starting bytes; no unrelated authored file remains changed.

Preliminary failures and successful revisions are retained separately: an overly conservative scalar root gate, missing include-order declaration, an unsupported testthat diagnostic argument, and the formerly heuristic seeded ZIP expectation were resolved before these final runs. The independent diagnostics also exposed the proposal-coordinate stopping issue and the need for a certified rescue start. No existing golden or coefficient tolerance was relaxed.

For reproduction, use the retained `compile_probe.py`, `compile_global.py`, `final_independent.py`, `timing.py`, `run_validation.py` and `final_suites.py`, with temporary directories under `build/tmp`. The runners execute foreground commands and record exit status, machine and wall time. Every command started for this job has finished.

## Artifact display

The report is saved in this worktree. The artifact viewer returned HTTP 403 because its readable-root list excludes `/home/jonas/Files/programming/univariateML.wt/zinb-cert`; refreshing the existing-thread list did not register it. The worktree needs to be added to the viewer's readable roots. No viewer configuration or files outside this worktree were changed.
