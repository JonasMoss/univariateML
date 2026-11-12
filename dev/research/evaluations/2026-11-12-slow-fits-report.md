# TASK-205: slow CMB and discrete-gamma fits

All three requested fits now terminate with structured stops and feasible likelihood witnesses. CMB 837 and 915 stop on the existing normalizer budget. Discrete gamma 997 stops because its endpoint-score arithmetic cannot establish the requested coefficient accuracy. No capped candidate is returned as an estimate.

The baseline is `c4ed89f6a4222e133c7524cf726fea4cb9369593`. All runs used the workstation, GCC 15.2, the prescribed slot wrappers, one BLAS/OpenMP thread per worker, and nice level 10. Simbox was off as specified in the brief. Changes remain uncommitted.

| Archived case | Fresh baseline C++ replay | Final median, three repeats | Final observed range | Outcome |
|---|---:|---:|---:|---|
| cmb 837, all free | 80.55 s | 8.50 s | 4.48–16.15 s | budget, feasible candidate retained |
| cmb 915, prob and nu held | external 120 s guard | 1.58 s | 1.53–1.80 s | budget, feasible candidate retained |
| dgamma 997, all free | external 120 s guard | 0.081 s | 0.079–0.223 s | numerical precision stop, feasible anchor retained |

These are direct optimized C++ fit times under concurrent local build/test load, not timing assertions. The earlier seam audit used a different driver; in this fresh optimized baseline, cmb 837 completed before the external guard. An expired replay guard is not a fitter status. [Timing records](2026-11-12-slow-fits-data/benchmark.json), [timing summary](2026-11-12-slow-fits-data/timings-summary.json), and [compiler/source provenance](2026-11-12-slow-fits-data/provenance.json) are retained.

The direct C++ profiles used `-O1 -pg` with 12-second sampling windows. CMB spent its work constructing dense support tables, repeatedly evaluating log-binomial coefficients and building unused cumulative tails. Its support-cell budget was already enforced, but a million-cell table made each solve expensive. Discrete gamma spent almost all its work in gamma-tail evaluations inside repeated conditional rate solves; the initial scan had entered 29 profile evaluations and about 17,915 cell-density evaluations within the sampling window. Local iteration caps restarted at each profile point, and cancellation in the endpoint score prevented reliable convergence. The [profile probe](2026-11-12-slow-fits-data/profile.cpp) and [gamma profile output](2026-11-12-slow-fits-data/profile-997.txt) provide the call evidence; the CMB outputs are [837](2026-11-12-slow-fits-data/profile-837.txt) and [915](2026-11-12-slow-fits-data/profile-915.txt).

The [CMB fitter](../../../univariateML-cpp/include/cppunivariate/distributions/cmb/mle.hpp) now uses fitting-only tables on supports of at least 100000. Positive dispersion gives concave log weights and a retained modal interval; nonpositive dispersion gives convex log weights and retained endpoint intervals. The crossing cutoff bounds omitted mass and raw moments through order two by `1e-24` relative to the modal weight. This follows from the finite number of cells and the bounds on the two sufficient statistics, stated in the dossier. It is an exact-arithmetic inequality conditional on computed crossings, with the existing practical floating-point margins. Density/CDF implementations remain separate. Every table still spends its full `N+1` cells, so the default allowance stays 200 million cells rather than expanding when normalization becomes cheaper. Small-support table arithmetic is preserved.

The [discrete-gamma fitter](../../../univariateML-cpp/include/cppunivariate/distributions/dgamma/mle.hpp) uses its proved conditional-mean rate bracket at large shape and applies the existing shape-tail bound before remote initializer evaluations. Searches initialized above shape `1e6` share a rate-solver allowance of `min(maxit,100000)`; ordinary initializers retain their per-profile caps. A precision diagnostic propagates a practical allowance of 64 machine epsilons times gamma-prefix magnitudes through the endpoint subtraction. A score allowance larger than `tol * observed log-rate information` stops the search with the evaluated anchor. This is a numerical precision diagnostic, not a nonexistence proof or an outward-rounded enclosure. Witness likelihood intervals use a corresponding prefix-magnitude allowance. The binding preserves the witness in an `univariateML_numerical` condition with a precision-specific message. Other existing dgamma iteration failures also acquire this structured numerical condition; their core enum is unchanged.

An independent **60-digit scaled cell quadrature**, without incomplete-gamma evaluation, checks the stopped gamma anchor. The computed mean log-rate score is 8.34777; quadrature gives −0.416587. Their difference, 8.76436, exceeds the requested score scale of 5.57938. The implemented practical allowance is 78867.48. Quadrature also places the anchor log likelihood at −14.29831684237, inside the widened witness interval [−14.49547224563, −14.10117136055]. These are high-precision numerical comparisons at this point, not uniform special-function error guarantees. See the [checker](2026-11-12-slow-fits-data/precision-check.py), [results](2026-11-12-slow-fits-data/precision-check.json), and [structured witnesses](2026-11-12-slow-fits-data/witnesses.json).

The archive replay covers **all 168 cmb/dgamma rows**. All 123 successful fits—80 CMB and 43 discrete gamma—preserve coefficients, likelihoods, iteration counts and core statuses exactly in the 17-digit output. All 43 previously completed core stop statuses, iteration counts, budget flags and candidate counts also match. The replay does not compare their complete stop payloads. Only the two baseline timeouts acquire terminal fitter outcomes. CMB 863 also becomes faster while preserving its existing budget stop. [Before](2026-11-12-slow-fits-data/before.jsonl), [final](2026-11-12-slow-fits-data/final.jsonl), and [comparison](2026-11-12-slow-fits-data/comparison.json) are retained. Goldens and default controls were not changed.

The exhaustive R run exposed an ordinary shape-30 reference sample whose remote grid probes initially spent the shared allowance. The family-only correction scopes that allowance to large-shape initializers. The original R reference test was retained. An appended C++ regression uses the same 150-observation sample; its final coefficients and likelihood match the baseline exactly. [Fixture and before/final results](2026-11-12-slow-fits-data/full-reference-samples.json) record this check. Other appended regressions inspect ledger counts, the one-evaluation precision stop, scalable solver caps, witness likelihoods, and agreement of fitting-only CMB tables with dense tables. They contain no elapsed-time assertions.

| Validation | Machine | Result |
|---|---|---|
| `just cpp-test` | workstation | 108/108 development shards passed |
| `just test` | workstation | 25738 pass; 0 fail, 0 warn, 154 skip |
| `just test-full`, initial run | workstation | 52488 pass; the one dgamma failure above; 0 warn, 0 skip |
| Post-fix dgamma full CTest shard | workstation | passed; standalone dgamma check: 20 cases, 26217 assertions |
| Post-fix full-tier R dgamma and filtered shared checks | workstation | 452 pass; 0 fail, 0 warn, 6 empty-test skips |
| `just cpp-test-no-long-double`, final source | workstation | 135/135 shards passed |
| `just js-build` | workstation | passed |
| `just js-test` | workstation | 615/615 passed |
| `just site-check` | workstation | passed |

All final rechecks are green. Following the brief's late-fix policy, the family-only correction was checked with dgamma's full doctest/testthat checks; the broad R suites were not repeated. The final double-width C++, JS and site runs used the corrected source. [Command results and durations](2026-11-12-slow-fits-data/final-suites.json) and [compressed logs](2026-11-12-slow-fits-data/validation-logs/) preserve the initial failure and the successful rechecks.

**Open evaluator follow-up, outside this task's permitted files:** at the gamma witness, the existing public cell-density evaluator's summed log likelihood differs from 60-digit cell quadrature by approximately `4.96e-6`. A stable large-shape gamma-cell evaluator and corresponding high-precision truth coverage need investigation. The settled `dpqr.hpp`, `special/`, `truth.*`, code generators and generated density sources were not edited. The fitter now stops explicitly in the demonstrated precision-limited case; this job does not claim to repair the evaluator's accuracy over that parameter region. No TASK-205 fit or validation item remains pending.

Reproduce from the worktree root with:

```sh
/tmp/umvl-cpu/run bash dev/research/evaluations/2026-11-12-slow-fits-data/reproduce.sh
```

The script rebuilds baseline headers from the recorded commit into ignored scratch space, runs baseline/current archive replays, checks the independent precision witness, and profiles the three original cases. It retains no archived source tree in the evaluation data. The two suite runners alongside it record the original full runs and the targeted late recheck plus remaining suites.

The artifact-pane handoff was attempted after validation. The viewer returned HTTP 403 because this worktree is outside its configured readable roots. The report and evidence remain in this worktree; no viewer configuration or other checkout was changed.
