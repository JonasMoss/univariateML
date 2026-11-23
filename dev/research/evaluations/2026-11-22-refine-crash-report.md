# Reference exceptions in fit-refine

TASK-232 · Worktree baseline `896d9dee` · Checks completed 2026-11-23

The final run judged 216 cases across the two precision modes. All 216 passed, 36 were open, and none raised a reference exception. Sixty-two of the 66 former crashes yielded coefficient comparisons. The four remaining cases exceeded the error bounds of their finite-series oracles. [Final log](2026-11-22-refine-crash-data/after.log), [case records](2026-11-22-refine-crash-data/after.jsonl)

## Classification and cache

Unexpected Python exceptions get an `exception` record and a summary entry with the precision mode, family, source, and case. They make the strict checker exit nonzero independently of the fitter failure ratchet. A crashed reference produced no verdict on the fitter, so a known fitter failure cannot absorb the crash or be marked fixed on its evidence. The wall-clock alarm, numerical exclusions, and cases with no independently refinable coefficient retain `open` and a reason. An injected arithmetic exception verified the reference wrapper, the cache-only summary, and exit status 1. [Reference wrapper](../../validation/fit-refine.py#L222), [verdicts](../../validation/fit-refine.py#L507), [verification log](2026-11-22-refine-crash-data/check-records.log)

The gzip JSONL ledger now supports an optional `exception` field and a `profile_revision` marker. The revised conditional reference keys include the fitted coordinates on which the result depends. A control with sizes 8 and 9 on the same zero-modified binomial input produced different keys and different probability references. The targeted run replaced 63 legacy references and reused the other applicable entries. Documented integer budget and applicability errors from the shared oracle load as explicit opens. Parallel workers handle different families, with the driver writing the cache and applying the verdicts. [Cache keys and migration](../../validation/fit-refine.py#L105), [validation guide](../../validation/README.md#strict-layer-checks-with-a-density-or-a-refinement)

## Crash causes and outcomes

- **Triangular, 18 archive cases per mode.** In case `008159`, for example, `a = c`. Common-subexpression elimination in the generated Hessian hoisted a division from the unoccupied triangular branch and evaluated it at zero. At the reported endpoint mode the matching endpoint maximizes the likelihood by minimizing the support width. For a free opposite endpoint, writing the likelihood in reciprocal width gives a strictly concave one-dimensional objective. The reference brackets the continuous root and scores its neighboring binary64 endpoints at high precision. All 36 former crashes passed this conditional endpoint check. A separate replay also matched the public endpoints in the near-constant, offset, and subnormal classes, including both reflected subnormal candidates. [Old Hessian](../../validation/quality/reference.py#L205), [endpoint profile](../../validation/refine_profiles.py#L21), [earlier triangular finding](2026-11-16-equiv-f-report.md)
- **Zero-modified binomial, 11 archive cases per mode.** Case `008799` has `p0 = 0`. Substitution introduced `log(0)` in the inactive zero branch, and SymPy's mpmath printer rejected the resulting `ComplexInfinity`. At `prob = 1`, as in `008795`, the positive-count term also needs the point-mass formula at `x = size`. The reference now separates the Bernoulli zero indicator from the conditional positive likelihood. The empirical zero fraction is exact, and the strictly increasing positive conditional mean brackets a free probability at fixed size. Constant positive counts above one identify the size when probability is free. All 22 former crashes passed, along with 12 formerly open cases for which the old cache recorded an integer envelope or enumeration exclusion. [Old symbolic compilation](../../validation/quality/reference.py#L182), [factorized reference](../../validation/refine_profiles.py#L75)
- **Negative hypergeometric, `005161` and `005217` in both modes.** After integer enumeration, symbolic substitution evaluated a constant log-gamma as the logarithm of a huge factorial. The generated function tried to print that factorial as a decimal integer and exceeded Python's 4,300-digit limit. The replacement compares exact integer products for the adjacent-composition likelihood ratio, which decreases in composition at fixed population and stopping count. The conditional optimum equals the reported `m` in both modes, at 4,503,524,867,960,574 for `005161` and 3 for `005217`. The fresh fit of `005161` uses a different population and stopping count from the historical cached start. [Old symbolic compilation](../../validation/quality/reference.py#L182), [composition profile](../../validation/refine_profiles.py#L132), [fresh fit details](2026-11-22-refine-crash-data/case-details.log)
- **CMP, `000957` in both modes.** With `lambda` held at 4.8929110545673415 and counts `[100, 100, 100, 101, 100]`, the 128-term normalizer has a relative tail bound of 0.07085 at the reported dispersion. This exceeds the oracle's `1e-25` threshold and triggered its assertion. The preflight now records a series-guard open. A longer normalizer with error bounds sufficient for the score and information would be needed to judge this fit. [Original assertion](../../../univariateML-cpp/include/cppunivariate/distributions/cmp/spec.py#L12), [preflight](../../validation/refine_profiles.py#L170)
- **Discrete gamma, `001027` in both modes.** With `rate` held at 0.9720518551425348 and the same counts, the geometric remainder is about `5.00e-8` and `9.59e-8` of the cell masses at 100 and 101. Both exceed the 64-term oracle's `1e-20` threshold. The preflight records a series-guard open. An adaptive cell-mass representation with derivative error control is needed for this case. [Original assertion](../../../univariateML-cpp/include/cppunivariate/distributions/dgamma/spec.py#L24), [preflight](../../validation/refine_profiles.py#L170), [guard calculations](2026-11-22-refine-crash-data/case-details.log)

The original exceptions were reproduced using the historical starts. The pre-change code could already recover some zero-modified binomial cases when forced to rerun them, as the trace for `008799` shows. Its cache-only path continued to report the saved exception as open. [Original traces](2026-11-22-refine-crash-data/original-traces.log)

## Before and after

Counts combine native and double-only snapshots. A case is judged when at least one independent coefficient comparison is made. The before column counts the old cached crashes and four negative-hypergeometric records with no comparisons according to that definition. The old summary included some opens in its printed judged count. Both runs covered the same 252 case keys. [Before records](2026-11-22-refine-crash-data/before.jsonl), [before summary](2026-11-22-refine-crash-data/before.log), [after records](2026-11-22-refine-crash-data/after.jsonl)

| Family or route | Run | Judged | Passed | Failed | Open by reason | Exceptions |
| --- | --- | ---: | ---: | ---: | --- | ---: |
| `binom` | Before | 28 | 28 | 0 | 12 held-probability tail envelope, 2 integer enumeration budget | 0 |
| `binom` | After | 28 | 28 | 0 | 12 held-probability tail envelope, 2 integer enumeration budget | 0 |
| `binom+zero=modified` | Before | 8 | 8 | 0 | 10 held-probability tail envelope, 2 integer enumeration budget | 22 |
| `binom+zero=modified` | After | 42 | 42 | 0 | 0 | 0 |
| `cmp` | Before | 28 | 28 | 0 | 4 case guards at 120 s, 8 family exclusions after two guards | 2 |
| `cmp` | After | 28 | 28 | 0 | 4 case guards at 120 s, 8 family exclusions after two guards, 2 series guards | 0 |
| `dgamma` | Before | 38 | 38 | 0 | 2 case guards at 120 s | 2 |
| `dgamma` | After | 38 | 38 | 0 | 2 case guards at 120 s, 2 series guards | 0 |
| `nhyper` | Before | 34 | 34 | 0 | 4 without an independently refinable coefficient | 4 |
| `nhyper` | After | 38 | 38 | 0 | 4 without an independently refinable coefficient | 0 |
| `triang` | Before | 6 | 6 | 0 | 0 | 36 |
| `triang` | After | 42 | 42 | 0 | 0 | 0 |
| **Total** | **Before** | **142** | **142** | **0** | **44** | **66** |
| **Total** | **After** | **216** | **216** | **0** | **36** | **0** |

There were no newly judged contract failures. The largest triangular difference among the repaired cases was in `008161` in both modes. The fitted `a` was 0.13204753454835227 and the reference was 0.13204753454410495, a difference of `4.2473e-12` against an allowance of `1e-10`. On zero-modified binomial case `011161` in both modes, the fitted and reference probabilities were 0.9749999999998511 and 0.9749999999998512. [Coefficient comparisons](2026-11-22-refine-crash-data/after.jsonl)

## Checks

Fresh GCC 15.2 workstation snapshots used `fit-snapshot.py --jobs 4` under `run4` with separate native, double-only, and modifier-route invocations. The old checker read those snapshots with the historical cache. The revised checker used `run4 ... fit-refine.py --jobs 4 --case-limit 120` with the two precision-mode inputs, then matched a single-slot cache-only replay. The recorded guard exclusions for unrelated cases came from the existing ledger. [Snapshot hashes and case details](2026-11-22-refine-crash-data/case-details.log), [warm summary](2026-11-22-refine-crash-data/warm.log), [record checker](2026-11-22-refine-crash-data/check_records.py)

The workstation `just test-changed --no-long-double` invocation expanded to all families because the selector treats the validation harness as shared. Native C++ passed 134 of 134, double-only C++ passed 179 of 179, the R development tier had 37,329 passes and 157 skips, and the JavaScript build and 591 tests passed. [Test summary](2026-11-22-refine-crash-data/test-changed.log)

## Open

- The CMP and discrete-gamma series exclusions remain open at the case IDs above. The cached case and family budget exclusions and the ordinary-binomial tail exclusions remain as listed in the table.
- The negative-hypergeometric check settles composition conditional on population and stopping count. Cases `005168` and `005174` hold composition and still need an independent joint population and stopping-count reference.
- A separate global comparison across triangular candidate modes would extend the endpoint checks. For zero-modified binomial, 26 records still have an unchecked free integer size because the positive-count profile lacks an independent global tail envelope. Each of those records has a checked probability or zero-mass coefficient.
