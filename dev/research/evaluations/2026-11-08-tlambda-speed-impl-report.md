# Tukey lambda profile fitter and fitting inverse — TASK-151.1

Implemented the profile fitter and safeguarded fitting inverse. Every required final suite finished green before this report was written. The worktree remains uncommitted on `codex/tlambda-speed`, based on `6977702df860d9fbc20cc681507b927cd8565f87`. Reproducible drivers, reference checks, source fingerprints and machine summaries are in the [evidence directory](2026-11-08-tlambda-speed-impl-data/README.md).

The legacy Nelder–Mead multistart has been removed from `tlambda_mle_impl`. All-free fitting, held-scale/free-location-and-shape fitting, held-location/free-scale-and-shape fitting, and negative held-shape joint nuisance fitting now use [profile.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/tlambda/profile.hpp). The existing scalar and shape-only certificates receive proposals through the same replacement. Public signatures and current controls are preserved. The certificate header, public d/p/q/r, special functions, truth tables, code generators and golden JSON are unchanged. Existing tie, collapse, support and restriction decisions remain in their existing routes. The family remains `certified: partial`; this change does not establish global coverage of the shape search or negative-shape multistart.

At fixed nonnegative sub-uniform shape, concavity in inverse scale and standardized location makes the stationary inner fit global, as proved in the [theory report](2026-11-07-tlambda-theory-report.md). Negative shapes use multiple starts, including a retained logistic or uniform scale for held-location profiles. The outer search uses envelope-theorem scores and safeguarded Illinois refinement, compares the lower shape face and exact uniform member, and checks nuisance variation across the contracted shape bracket. Held location has no fixed negative floor: the bound `|x-mu| f(x) < 1/(2a)` yields an incumbent-dependent cutoff for shape `-a`. Unrepresentable cutoffs and failed solves stop numerically.

The inner Newton system uses the equivalent local affine frame `dA=A*u, dB=B*u-v`. Its corrections directly measure relative scale and location in fitted-scale units. Compensated scores, a stable log-power curvature expansion and weighted tail curvature avoid cancellation and underflow in the error estimate. Acceptance requires finite positive observed information, small undamped corrections and recent full steps in the quadratic regime. Data are centred at the median or held location; original-coordinate reconstruction has a rounding check. A ledger prevents a failed start with a materially better finite likelihood from being silently discarded. The [method note](2026-11-08-tlambda-speed-impl-data/methods.md) gives the identities, stopping rules and work caps.

The fitting-only [fit_inverse.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/tlambda/fit_inverse.hpp) uses Halley in log probability, with centre, tail and endpoint-deficit equations. The deficit comes from the affine support residual. Logistic and shape -1 use their closed forms; the fitted branch uses one thread-safe first-use 33 by 65 seed table (17,160 bytes), and held shapes outside it use a closed-form seed. Nonfinite or out-of-bracket proposals fall back to bisection. A small residual is insufficient: acceptance also checks opposite signs within `64*epsilon*(1+abs(log p))`. These are conditional computed-sign enclosures with a practical floating-point margin, not outward-rounded intervals. No capped inverse iterate is returned.

The seeded comparison repeats the eleven prototype generators: normal, t3, t1.5, Cauchy, uniform, exponential, contaminated normal, bimodal normal, Tukey lambda -0.8 and 0.9, and quarter-rounded normal. Seeds are `777+n`, with 8, 6 and 2 replicates per generator. Each sample checks all four replaced routes; holds are mu=.123, sigma=1.07, or lambda=-.5. Both fitters use tolerance 1e-10 and iterlim 2000. All 704 fits succeeded, with no likelihood loss exceeding 1e-7. No case in this matrix improved likelihood by more than 1e-7. The largest loss was -5e-12; differences at this scale include different floating-point likelihood reductions. [Paired results](2026-11-08-tlambda-speed-impl-data/comparison-summary.json) retain every route.

| n | samples | paired route fits | failures | losses greater than 1e-7 |
|---:|---:|---:|---:|---:|
| 30 | 88 | 352 | 0 | 0 |
| 101 | 66 | 264 | 0 | 0 |
| 1000 | 22 | 88 | 0 | 0 |

On the frozen n=500 sample, the all-free fit is mu=0.047524707694102691, sigma=1.0988151608485901, lambda=0.18447846585099015. Independent 80-digit inversion and score checks against the supplied 50-digit reference give a maximum relative Newton correction of 3.39e-15 across the all-free, held-location and held-scale checks. The independently computed log likelihood is -911.409911423067282689698601762. New doctests enforce 1e-10 relative coefficient accuracy. [Reference checks](2026-11-08-tlambda-speed-impl-data/golden-checks.json) distinguish coordinate accuracy from global certification.

The extended held-location case uses 30 deterministic quantiles at generating shape -20, with mu held at zero. The new fit has sigma about 1.49064207154 and shape about -19.4773337072, beyond the old initial shape grid. A 90-digit stationary reference with positive observed information gives log likelihood -1052.70472640981057361148536352. Re-evaluating the legacy result independently gives -1200.98846422863445632984935791: the improvement is 148.28373781882388. Maximum free-coordinate relative errors are 4.82e-13 for C++ and 3.65e-13 for WebAssembly. [Held reference](2026-11-08-tlambda-speed-impl-data/held-checks.json) and [WASM check](2026-11-08-tlambda-speed-impl-data/wasm-held-checks.json) retain the values and input observations. This verifies a local maximum numerically; it is not a new global certificate.

The inverse passes 179 independent 420-digit mpmath bisections across heavy tails, the centre, shapes near zero and one, endpoint deficits through 2^-1074, and held shapes outside [-1,1]. Maximum normalized log-density error, `abs(error)/(1+abs(reference))`, is 1.06e-14. An actual affine case x=2, mu=2^-1073, sigma=1, shape=.5 retains deficit 2^-1074 even though the standardized observation rounds to the endpoint. Its log density is -744.44007192138122, agreeing with the independent closed-form value to 4.43e-14 absolute error. [Inverse checks](2026-11-08-tlambda-speed-impl-data/inverse-checks.json) and [affine check](2026-11-08-tlambda-speed-impl-data/affine-checks.json) give the references.

Public density agreement has one explicit convention exception: for shape just above one, a rounded endpoint alias can have a positive mathematical affine deficit. At x=1, mu=2^-1074 and sigma=shape=nextafter(1,2), the fitting inverse agrees with the interior reference -0.693147180559862805, while public `dtlambda` intentionally returns its endpoint alias value, approximately zero. The public convention and certified routes are unchanged. The [recorded witness](2026-11-08-tlambda-speed-impl-data/endpoint-alias-checks.json) must be retained when the public inverse is updated in the separate job.

The paired density driver measures nanoseconds per log density on 10,001 deterministic quantiles per shape, five repetitions, excluding generation. It includes first-use table construction once. All measured densities are finite; ordinary pointwise disagreements with public `dtlambda` are at its existing inversion precision.

| shape | public dtlambda, ns | fitting inverse, ns |
|---:|---:|---:|
| -2 | 683 | 617 |
| -1 | 138 | 160 |
| -0.7 | 715 | 520 |
| -0.05 | 714 | 483 |
| 0 | 100 | 100 |
| 0.14 | 749 | 394 |
| 0.7 | 845 | 377 |
| 0.99 | 841 | 380 |
| 1.5 | 831 | 538 |
| 3 | 895 | 535 |

Fit timings use the implementation report's deterministic driver: quantiles `(i+.5)/n` at mu=0, sigma=1, shape=.14; C++17/-O2, tolerance 1e-7 and iterlim 1000. Mask bits are held mu=1, sigma=2, lambda=4. The following are workstation wall seconds from the preserved before binary and final source. Shared load varied substantially; these are reproducible single-run measurements, not controlled benchmark confidence intervals.

| n | mask | held | before, s | after, s |
|---:|---:|---|---:|---:|
| 101 | 0 | none | 0.642456 | 0.0283904 |
| 101 | 1 | mu | 4.013e-06 | 3.922e-06 |
| 101 | 2 | sigma | 0.285296 | 0.0114359 |
| 101 | 3 | mu, sigma | 9.99649 | 9.67302 |
| 101 | 4 | lambda | 0.00360792 | 0.00376655 |
| 101 | 5 | mu, lambda | 0.00353321 | 0.00365172 |
| 101 | 6 | sigma, lambda | 0.000648939 | 0.000643034 |
| 101 | 7 | all | 0.000298582 | 0.000297401 |
| 500 | 0 | none | 2.53483 | 0.0601541 |
| 500 | 1 | mu | 0.647145 | 0.0617162 |
| 500 | 2 | sigma | 1.43336 | 0.0324848 |
| 500 | 3 | mu, sigma | 82.6325 | 48.5079 |
| 500 | 4 | lambda | 0.0401747 | 0.0231923 |
| 500 | 5 | mu, lambda | 0.0401261 | 0.0231843 |
| 500 | 6 | sigma, lambda | 0.00709486 | 0.00403564 |
| 500 | 7 | all | 0.00343233 | 0.00194744 |
| 1000 | 0 | none | 6.78919 | 0.142578 |
| 10000 | 0 | none | 48.5803 | 1.48841 |
| 100000 | 0 | none | 833.476 | 22.679 |

At n=101, mask 1 is correctly unbounded because the deterministic sample contains the held location; its timing measures that decision. The all-free n=100,000 run changes from 833.476 to 22.679 seconds, about 37 times faster in these runs. The scalar certificate bounds were not changed, so timing variation in certified masks is not evidence of a bound improvement. The shape-only route remains much slower than profile fitting. [Before CSV](2026-11-08-tlambda-speed-impl-data/timing-before.csv) and [after CSV](2026-11-08-tlambda-speed-impl-data/timing-after.csv) and aggregate seeded-route timings are retained.

All required final runs completed successfully on the frozen source fingerprint. Native suites reserved four slots; numerical drivers used one. The final web batch used one reserved, pinned CPU. All ran under nice +10 with BLAS/OpenMP pinned to one thread. Simbox stayed available; no exit-75 fallback was needed. Workstation C++ used GCC 15.2; simbox core tests used GCC 13.3, and its R package used g++-15. R was 4.6.1 and workstation Node 22.22.1.

| command | machine | final result |
|---|---|---|
| `just cpp-test` | workstation | 106/106 |
| `just cpp-test-no-long-double` | workstation | 129/129 |
| `just cpp-test` | simbox | 106/106 |
| `just cpp-test-no-long-double` | simbox | 129/129 |
| `just test` | simbox | PASS 22079, FAIL 0, WARN 0, SKIP 154 |
| `just test-full` | simbox | PASS 44911, FAIL 0, WARN 0, SKIP 0 |
| `just js-build`; `just js-test` | workstation | build green; 547/547, no skips |
| `just site-check` | workstation | 99 MathML family pages; 99 C++17 examples; browser checks green |
| focused tlambda cases | workstation | 25 cases, 3927 assertions; slow held subset covered by full no-long-double suites |

[validation.json](2026-11-08-tlambda-speed-impl-data/validation.json), [source fingerprints](2026-11-08-tlambda-speed-impl-data/source-manifest.sha256), and the retained machine summaries record the final runs. Nonfatal compiler/bootstrap notices are distinct from R warnings, which were zero. Dev-tier R skips are tier exclusions; the full tier has none. No command remains running.

Two test expectations required correction. The strict JS shared-golden check rejected the improved coefficients because `golden.json` still stores legacy estimates. The test now uses the supplied independent reference at 1e-10, preserving the strict mu/scale checks and tightening the shape check. The JSON is untouched for TASK-149.1. An unrelated frozen-model R test expected an error for an infinite geometric median, while the existing implementation explicitly permits infinite quantiles. It now compares directly with `stats::qgeom`; nonfinite likelihood rejection is retained. Neither correction relaxes a numerical tolerance. Initial failures and their reference evidence are retained in the evidence directory.

The remaining limitations and separate work are precise:

1. Global shape-mode coverage and global negative-shape nuisance optimization remain unproved. The grid, multistart and high-precision comparisons are numerical evidence; the family remains partially certified. No scale-concavity proof or certificate-bound change was made.
2. At held shape -20 with both nuisance coefficients free, the same 30-quantile stress sample stops numerically at tolerance 1e-10 and iterlim 2000. Establishing the required location/scale correction in that regime still needs better numerical conditioning or precision. The result is `IterationFailure`, mapped to a structured numerical stop, rather than a capped estimate or a nonexistence claim. The successful held-location fit is separately verified above.
3. The public inverse and its rounded-endpoint convention are outside this job. A later `dtlambda` change must handle the recorded alias witness explicitly. `dpqr.hpp`, `special/`, truth files and generated d/p/q/r were not edited.
4. Scalar-box and shape-only bound improvements, the negative-shape scale-concavity proof and generated table constants remain separate work. The existing certificate header is byte-identical. The temporary JS reference override can be removed once TASK-149.1 supplies the corrected shared golden estimates.

The final `show` attempt returned HTTP 403 because this worktree is outside the viewer's configured readable roots. Pane display requires registering this worktree in that external configuration. The completed report and evidence remain at the linked worktree paths; no viewer configuration or other checkout was changed. [Viewer result](2026-11-08-tlambda-speed-impl-data/viewer.json).
