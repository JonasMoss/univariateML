# GB2 and Feller–Pareto free-shape certificates — TASK-116.41

Delivered global certificates for the two non-unit, held-power conditional shape/scale problems, with Feller–Pareto transport, certified limit faces and structured stops. Seven larger mixed subsets have the precise open entries below. Both families retain `fit.certified: partial`; neither dossier claims a certified joint default fit.

The public GB2 order in this tree is **`(q,a,p,b)` = `(shape1,shape2,shape3,scale)`**. Here `a` is power, `p` the lower-tail beta shape and `q` the upper-tail beta shape. Sets below use these mathematical names.

Base: `f204b94e`, branch `codex/gb2-cert`. No commits, board edits, package installations, pushes or publications were made. The [12-file source manifest](2026-11-08-gb2-cert-data/source-sha256.txt) identifies the implementation, dossiers and tests. Its hashes matched before and after the final simbox sequence and were checked against the workstation files.

## Delivered subsets and proof

| Free coefficients | Held coefficients | Result |
|---|---|---|
| `{q,b}` | `p,a` | Conditional beta-prime certificate; transformed-gamma face |
| `{p,b}` | `q,a` | Reciprocal conditional certificate; inverse transformed-gamma face |
| The same subsets above a threshold | The corresponding coefficients and `min` | Identical certificate on positive excesses |

For a fixed log center `m`, set `y = exp(a*(log(x)-m))` and `s = exp(a*(log(b)-m))`. The transformed observations have beta-prime parameters `(p,q,s)`. Their log likelihood differs from GB2 by the parameter-independent Jacobian sum `sum(log(a) - log(x) + a*(log(x)-m))`. The scale map is a bijection for every held positive power. Thus the [beta-prime conditional theorem](2026-11-07-betapr-prof-report.md) transfers the global search, exterior bounds and competing-mode comparisons, without assuming profile unimodality.

A gamma face `(k,theta)` becomes transformed gamma `(k,a,exp(m + log(theta)/a))`; inverse gamma has the same scale map into inverse transformed gamma. **The held beta shape and power remain held in the proposed limit.** In public names, held GB2 `shape3` maps to transformed-gamma `shape1`; held GB2 `shape1` maps to inverse transformed-gamma `shape1`.

Implementation is in [power_certificate.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/gb2/power_certificate.hpp), called by the existing public [GB2 fitter](../../../univariateML-cpp/include/cppunivariate/distributions/gb2/mle.hpp). The canonical derivation and arithmetic allowances are in the [GB2 dossier](../../../univariateML-cpp/include/cppunivariate/distributions/gb2/README.md).

Certification is in exact arithmetic conditional on the computed comparisons, with practical floating-point margins; it is not outward-rounded interval arithmetic. The parent verifies a negative-curvature root enclosure, excludes dominated or monotone segments and bounds both shape exteriors. Its shape enclosure is within `1e-6` relative width. Before taking a scale to the power `1/a`, GB2 reconstructs a conservative scale enclosure from every surviving segment endpoint and its width below `1e-10`. The transported scale enclosure must also be within `1e-6` relative width. Small powers therefore cannot silently amplify an adequate parent enclosure into an inaccurate GB2 scale.

Transport adds `128*epsilon*(1 + sum(abs(J_i)) + n*(1 + abs(a*m)))` to likelihood bounds and checks representability and reconstruction error. Collapsed distinct observations, overflowing Jacobian arithmetic, invalid witnesses and insufficient transported precision stop. Parent arithmetic failures retain `SearchUnresolved`; transport failures use `NumericalFailure`. Neither can return an estimate. These are golden-accuracy certificates, not a new guarantee of arbitrary user-requested coefficient precision.

The parent ledger remains `128 * min(iterlim,100000)` certificate evaluations, with its solver iteration caps. No outer search or independent evaluation allowance is added. The retained [budget reproducer](2026-11-08-gb2-cert-data/budget.cpp) stops at exactly **8,192 evaluations** for `iterlim=64`, retaining 1,295 witnesses; see [output](2026-11-08-gb2-cert-data/budget.txt).

## Feller–Pareto and diagnostic corrections

[Feller–Pareto](../../../univariateML-cpp/include/cppunivariate/distributions/fpareto/mle.hpp) fits positive excesses with unit translation Jacobian. Only finite family witnesses append held `min`. All transformed-gamma face witnesses, including generic budget witnesses, now remain in excess-data coordinates. The existing held-scale endpoint subtraction guards are preserved; the new conditional route also rejects nonfinite subtraction and subtraction that merges distinct observations.

The binding-neutral [GB2](../../../bindings/common/distributions/gb2/fit.hpp) and [Feller–Pareto](../../../bindings/common/distributions/fpareto/fit.hpp) adapters expose transformed-gamma faces with structured details, so R and JS share routing. An existing held-endpoint Feller–Pareto **power** stop was mislabeled **Pareto**; its model and coefficient names are now `power`, `alpha`, `beta`. Empty, unselected inverse-gamma metadata from the reciprocal parent is cleared during GB2 transport.

C++, R and JS regressions cover finite fits, face coordinates, held coefficients and model identity. C++ additionally covers both reciprocal directions, reconstructed likelihood bounds, competing modes, subtraction collapse, overflowing arithmetic and generic budget face coordinates. Existing tests were not weakened.

## Nonuniqueness

The conditional beta-prime counterexample transfers exactly through `x=y^(1/a)`, and reciprocation transfers it to the other held-shape problem. Its continuity bracket and uniform likelihood gaps therefore prove an intervening sample with two separated global maxima, conditional on the parent certificate comparisons. No new uniqueness assumption is introduced.

For held `p=0.22690393135372058`, held `a=2`, and the nine observations generated by [modes.cpp](2026-11-08-gb2-cert-data/modes.cpp):

| Last-observation perturbation `t` | Certified winning `q` | Scale `b` |
|---:|---:|---:|
| 5.6463 | 0.1688336661 | 1.0073839558 |
| 5.6466 | 0.08352743166 | 0.01735964786 |

The rounded crossing near `t=5.646438395028704` returns a tie stop with both finite witnesses. It is not asserted to be an exact tie of those rounded observations. [modes.txt](2026-11-08-gb2-cert-data/modes.txt) records both direct and reciprocal results; the exact-tie existence argument is the [parent bracket proof](2026-11-07-betapr-prof-report.md#nonuniqueness) plus the bijective transformation.

## Accuracy, unchanged fixtures and timings

An independent [80-digit score solve](2026-11-08-gb2-cert-data/high_precision.py) checks held powers `0.7, 1, 2` in both directions on the retained eight-point workload. Maximum relative errors are **4.3e-12 in the free shape** and **9.6e-12 in scale**; total likelihood differences are at most `3.6e-15`. These are numerical comparisons, separate from the global certificate. See [reference values](2026-11-08-gb2-cert-data/high-precision.json), [errors](2026-11-08-gb2-cert-data/accuracy.json) and the additional [SciPy comparison](2026-11-08-gb2-cert-data/oracle.json).

The two golden files, two density headers and four truth sources/tables are byte-for-byte unchanged from the base; [hash inventory](2026-11-08-gb2-cert-data/settled-file-sha256.json). Default algorithms, controls and successful golden fits were preserved. Neither the special-function seam nor d/p/q/r generators were edited. No shared core header changed; shared JS tests were appended only.

Paired workstation timings use C++17, `-O2`, GCC 15.2, BH 1.90, one reserved CPU slot and `nice -n 10`. The baseline is reconstructed by read-only `git archive f204b94e`. Queue and compilation time are excluded. Eight-point entries average 30 calls; large entries are one call on 12,500 repetitions of those eight points, exercising all 100,000 rows. Both have held power `2` and held lower beta shape `2`.

| Family | Observations | Before | Certificate |
|---|---:|---:|---:|
| GB2 | 8 | 0.123 ms | 0.082 ms |
| GB2 | 100,000 | 0.907 s | 0.580 s |
| Feller–Pareto, `min=2` | 8 | 0.125 ms | 0.081 ms |
| Feller–Pareto, `min=2` | 100,000 | 0.913 s | 0.554 s |

The paired coefficient drift is below `4e-9` relative. [timings.sh](2026-11-08-gb2-cert-data/timings.sh), [before.txt](2026-11-08-gb2-cert-data/before.txt) and [after.txt](2026-11-08-gb2-cert-data/after.txt) include the other powers and reciprocal cases. These shared-workstation measurements are workload timings, not worst-case budget bounds.

## Final validation

Every listed final command finished successfully. Builds and tests ran in the foreground through the prescribed slot helpers, with numerical worker threads pinned to one. The workstation used `run4`; long simbox suites used `simbox-run`, with C++ parallelism set to four. Temporary files stayed under the worktree's `build/`.

| Command | Machine | Result |
|---|---|---|
| `just cpp-test` | simbox, GCC 13.3 | 106/106 |
| `UMVL_TEST_TIER=full just cpp-test` | simbox, GCC 13.3 | 129/129 |
| `just cpp-test-no-long-double` | simbox, GCC 13.3 | 129/129 |
| `just cpp-test-no-long-double` | workstation, GCC 15.2 | 129/129 |
| `just test` | simbox, R 4.6.1 / Rcpp GCC 15 | FAIL 0, WARN 0, SKIP 154, PASS 21,574 |
| `just test-full` | simbox, R 4.6.1 / Rcpp GCC 15 | FAIL 0, WARN 0, SKIP 0, PASS 44,389 |
| `just js-build`, `just js-test` | workstation | build passed; 500/500 tests |
| `just site-check` | workstation | 99-family site, snippets, MathML, links and browser checks passed |

Evidence: [validation.json](2026-11-08-gb2-cert-data/validation.json), [simbox results](2026-11-08-gb2-cert-data/simbox-validation.txt), [workstation web results](2026-11-08-gb2-cert-data/workstation-web.txt), [workstation portability results](2026-11-08-gb2-cert-data/workstation-no-long-double.txt), and environment records for [workstation](2026-11-08-gb2-cert-data/environment-local.json) and [simbox](2026-11-08-gb2-cert-data/environment-simbox.json). The site check's regenerated historical screenshot was restored, leaving the intended change scope.

## Open subsets and exact blockers

The retained [outer-profile derivation](2026-11-08-gb2-cert-data/open-analysis.md) gives the monotone inner shape score, its Schur-complement profile curvature and the fixed-endpoint power/Pareto paths. It supplies reductions, not a certificate for the following seven subsets outside existing exact unit-shape routes.

| Free subset | Missing certificate component |
|---|---|
| `{p,a}` with `q,b` held | Isolate every root of the scalar power profile after the unique inner `p` solve; bound its exterior and give signed uniform remainders for the fixed-upper-endpoint power face, including endpoint ties. Separate concavity does not establish profile concavity. |
| `{q,a}` with `p,b` held | Reciprocal version of the preceding bounds and fixed-endpoint Pareto comparisons. |
| `{p,q,b}` with `a` held | Exactly the still-numerical three-free-parameter beta-prime problem. The conditional certificate is not uniform in its held shape; coupled large-shape gamma/inverse-gamma neighborhoods and their lognormal corner remain unbounded by it. |
| `{p,q,a}` with `b` held | Quantitative bounds for the joint small-power/large-shape lognormal corner and large-power/small-shape power, Pareto and double-Pareto neighborhoods, plus a certified finite search. |
| `{p,a,b}` with `q` held | Bounds uniform in free power and lower shape for the inverse transformed-gamma and moving-endpoint paths, and global finite-profile root isolation. A remainder valid at one held power is insufficient. |
| `{q,a,b}` with `p` held | Reciprocal bounds for the transformed-gamma and moving-endpoint paths, with a certified finite search. |
| `{p,q,a,b}` | Compact likelihood-relevant bounds covering all preceding joint paths and intersections, including the coupled large-shape lognormal corner, followed by a certified interior search. Existing numerical face comparisons do not exhaust this closure. |

These remain mathematical/algorithmic open entries, not structural `holds:` exceptions or proofs of nonexistence. Their existing numerical searches, including the GB2 default, were preserved as instructed and are not newly certified. The same seven blockers transfer to Feller–Pareto with `min` held. Its 16 free-`min` subsets remain structural unsupported stops; no claim of universal nonexistence for those held-coefficient submodels is made.

## Artifact-pane display

The report was submitted to `codex-math-web/bin/show`. The viewer returned HTTP 403 because this worktree is outside its configured readable roots. Pane display requires registering this worktree with the viewer, or adding it to `CODEX_MATH_WEB_VIEW_ROOTS`. No configuration or files outside this worktree were changed. The report and all evidence remain saved at the requested paths.
