# TASK-197: backward probability acceptance for continuous quantiles

Implemented the backward probability test in the C++, R and JS truth consumers, appended the requested Frechet regression, and documented the rule in the validation guide. All required suites and the final independent audits passed. No active stored quantile row changes its acceptance at this baseline.

Baseline: `7825e912b3e5177254853cc020d52b662faeeaa8`, branch `codex/quantile-rule`. Runs completed on 2026-11-12. The counts below use this worktree’s unchanged active truth tables; they do not include another job’s subsequent truth regeneration.

## Consumer change

- [C++ consumer](../../../univariateML-cpp/tests/test_support.hpp): after the relative/floor and bracket tests fail, evaluate the family CDF at the computed quantile with the row’s original tail and scale.
- [R consumer](../../../bindings/r/tests/testthat/test-generated-dpqr.R): apply the same fallback to the failing continuous rows using the family’s generated `p` function and the current parameters.
- [JS consumer](../../../bindings/js/test.mjs): apply the same fallback through the family’s own WASM `p` binding.

The extra acceptance condition is `abs(CDF(q_hat) - input) <= 1e-12 * abs(input)` for natural inputs, or `1e-12 * max(1, abs(input))` for log inputs. The implementation requires finite computed and reference quantiles, an interior probability input, and a finite CDF value. Endpoint and nonfinite acceptance retain their previous rules. Relative quantile tolerances, zero floors, brackets, ambiguous lattice endpoints and the large-discrete minimality gate are unchanged; discrete families never use the new fallback.

The [validation guide](../../validation/README.md) adds one sentence describing the new rule. No family d/p/q/r code, special functions, truth generator/table, code generator, default fitter or golden was changed.

## Per-family deciding-row counts

A deciding row fails every existing forward rule and passes the new probability rule. The standalone audit extracts the actual d/p/q adapters from the family tests and checks every active continuous quantile row. It repeats the same forward predicate, including floors and brackets, before evaluating any backward candidate. The R and JS audits call the actual generated bindings; the R audit covers every available row, a superset of the consumer’s tier sampling.

| Observer | Continuous families | Quantile rows | Forward failures | Newly accepted |
|---|---:|---:|---:|---:|
| C++ native | 66 | 36,988 | 0 | 0 |
| C++ no-long-double macro | 66 | 36,988 | 0 | 0 |
| JS/WASM | 64 | 35,448 | 0 | 0 |
| R generated functions | 55 | 32,012 | 0 | 0 |

**Deciding stored rows: none in any family or observer.** Thus there are no newly forgiven stored-reference errors and no deciding stored rows with insufficient CDF accuracy. The complete counts are listed below and in [family-counts.csv](2026-11-11-quantile-rule-data/family-counts.csv). A dash means the family is outside that generated binding’s consumer, rather than an unmeasured zero.

| Family | C++ | C++ noLD | R | JS |
|---|---:|---:|---:|---:|
| beta | 0 | 0 | — | 0 |
| betapr | 0 | 0 | 0 | 0 |
| bhatt | 0 | 0 | 0 | 0 |
| burr | 0 | 0 | 0 | 0 |
| cauchy | 0 | 0 | — | 0 |
| emg | 0 | 0 | 0 | 0 |
| exp | 0 | 0 | — | 0 |
| expweibull | 0 | 0 | — | — |
| fatigue | 0 | 0 | 0 | 0 |
| foldnorm | 0 | 0 | 0 | 0 |
| fpareto | 0 | 0 | 0 | 0 |
| frechet | 0 | 0 | 0 | 0 |
| gamma | 0 | 0 | — | 0 |
| gb2 | 0 | 0 | 0 | 0 |
| ged | 0 | 0 | 0 | 0 |
| genbeta | 0 | 0 | 0 | 0 |
| gengamma | 0 | 0 | — | — |
| gev | 0 | 0 | 0 | 0 |
| gompertz | 0 | 0 | 0 | 0 |
| gpd | 0 | 0 | 0 | 0 |
| gumbel | 0 | 0 | 0 | 0 |
| hcauchy | 0 | 0 | 0 | 0 |
| hnorm | 0 | 0 | 0 | 0 |
| ht | 0 | 0 | 0 | 0 |
| huber | 0 | 0 | 0 | 0 |
| invburr | 0 | 0 | 0 | 0 |
| invexp | 0 | 0 | 0 | 0 |
| invgamma | 0 | 0 | 0 | 0 |
| invgauss | 0 | 0 | 0 | 0 |
| invparalogis | 0 | 0 | 0 | 0 |
| invpareto | 0 | 0 | 0 | 0 |
| invtrgamma | 0 | 0 | 0 | 0 |
| invweibull | 0 | 0 | 0 | 0 |
| kumar | 0 | 0 | 0 | 0 |
| laplace | 0 | 0 | 0 | 0 |
| lgamma | 0 | 0 | 0 | 0 |
| llogis | 0 | 0 | 0 | 0 |
| lnorm | 0 | 0 | — | 0 |
| logis | 0 | 0 | — | 0 |
| logitnorm | 0 | 0 | 0 | 0 |
| lomax | 0 | 0 | 0 | 0 |
| lst | 0 | 0 | 0 | 0 |
| naka | 0 | 0 | 0 | 0 |
| norm | 0 | 0 | — | 0 |
| nsbeta | 0 | 0 | 0 | 0 |
| paralogis | 0 | 0 | 0 | 0 |
| pareto | 0 | 0 | 0 | 0 |
| pareto2 | 0 | 0 | 0 | 0 |
| pareto3 | 0 | 0 | 0 | 0 |
| pareto4 | 0 | 0 | 0 | 0 |
| power | 0 | 0 | 0 | 0 |
| rayleigh | 0 | 0 | 0 | 0 |
| sged | 0 | 0 | 0 | 0 |
| sgomp | 0 | 0 | 0 | 0 |
| slash | 0 | 0 | 0 | 0 |
| sn | 0 | 0 | 0 | 0 |
| snorm | 0 | 0 | 0 | 0 |
| sstd | 0 | 0 | 0 | 0 |
| st | 0 | 0 | 0 | 0 |
| std | 0 | 0 | 0 | 0 |
| tlambda | 0 | 0 | 0 | 0 |
| tnorm | 0 | 0 | 0 | 0 |
| trgamma | 0 | 0 | 0 | 0 |
| triang | 0 | 0 | 0 | 0 |
| unif | 0 | 0 | — | 0 |
| weibull | 0 | 0 | — | 0 |

JS coverage omits: `expweibull`, `gengamma`. R coverage omits: `beta`, `cauchy`, `exp`, `expweibull`, `gamma`, `gengamma`, `lnorm`, `logis`, `norm`, `unif`, `weibull`. The existing R consumer excludes stats-dispatched families and absent generated functions; neither binding’s family registry was changed.

The empty deciding-row lists are retained as [native](2026-11-11-quantile-rule-data/native-decisions.json), [noLD](2026-11-11-quantile-rule-data/double-decisions.json), [R](2026-11-11-quantile-rule-data/r-decisions.json) and [JS](2026-11-11-quantile-rule-data/js-decisions.json). Excluded/archived truth rows are not active consumer inputs and are not counted.

## Frechet probe: genuine ill-conditioning

The appended [C++ regression](../../../univariateML-cpp/include/cppunivariate/distributions/frechet/test.cpp) uses exactly `qfrechet(-7.3890561137087625, 1e9, -999999998, 1e9, true, true)`. It checks the backward log-probability property and the absolute error bound `4 * epsilon * max(1, abs(mu + sigma))`; here the characteristic center `mu + sigma` is 2. This is a regression case, not a truth-table row.

The native C++, noLD, R and JS observations agree bit for bit for this probe. Independent evaluations use exact binary64 inputs in the existing mathematical Frechet route at 80 and 160 decimal digits. The direct inverse `mu + sigma * exp(-log(-input) / lambda)` also verifies the supplied reference root, whose nearest double is within 0.452 ulp of the high-precision root.

| Quantity | Result |
|---|---:|
| Independent quantile | `-1.9991476033510408592152e-18` |
| Computed quantile | `-1.2456318291555971e-16` |
| Relative forward error | `61.3081` |
| Absolute quantile error | `1.22564e-16` |
| Center-based absolute error bound | `1.77636e-15` |
| Own log CDF at the computed quantile | `-7.3890561137087634` |
| Independent backward probability error | `1.22564e-16` |
| Own CDF error against the independent CDF at q_hat | `2.36216e-18` |
| 80/160-digit CDF agreement | `1.21164e-82` |
| Relative quantile condition number | `5.00213e+17` |

Both probability-error figures and the precision-agreement figure are normalized on the log input’s scale, `max(1, abs(input))`. The condition number uses the independent density and the derivative of the selected log tail at the reference root. Its magnitude explains the large relative coordinate error: the root is nearly zero while the probability residual is near binary64 rounding error. The independently evaluated probability residual is more than 8,000 times smaller than the allowed `1e-12`, so the own-CDF check is not concealing a wrong probability at this probe.

This is high-precision numerical evidence, not an outward-rounded error enclosure or a proof of CDF accuracy throughout every family’s domain. There are no other deciding stored rows requiring a CDF accuracy claim. Raw probe observations, reference comparisons and conditioning metrics are retained for [native](2026-11-11-quantile-rule-data/native-probe-verified.json), [noLD](2026-11-11-quantile-rule-data/double-probe-verified.json), [R](2026-11-11-quantile-rule-data/r-probe-verified.json) and [JS](2026-11-11-quantile-rule-data/js-probe-verified.json). The R evidence writer converts string-encoded parameters exactly as the consumer does and explicitly verifies 17-digit numeric round trips.

## Finished validation

| Command | Machine | Final result |
|---|---|---|
| `just js-build` | Workstation | Exit 0 |
| `just js-test` | Workstation | 615/615; zero failures |
| `just cpp-test` | Workstation fallback, GCC 15 | 108/108 dev shards |
| `just cpp-test-no-long-double` | Workstation fallback, GCC 15 | 135/135 shards, including full labels |
| `just test` | Workstation fallback | FAIL 0, WARN 0, SKIP 154, PASS 25,600 |
| `just test-full` | Workstation fallback | FAIL 0, WARN 0, SKIP 0, PASS 52,358 |

Simbox was unreachable and `simbox-run` returned 75 before its native suite batch could start. The same batch ran locally through `run4`, as required by the fallback instruction. The no-long-double target therefore includes the required workstation run; no GCC 13/simbox result is claimed. See the [availability log](2026-11-11-quantile-rule-data/simbox-attempt.log).

Local builds and parallel suites reserved four slots and used at most four workers; the independent evidence work used one slot. BLAS/OpenMP were pinned to one thread and the wrappers applied nice priority 10. Commands ran in the foreground and were waited to completion, including CPU reservation waits. The native batch held its reservation only across sequential builds/suites. Temporary files and scratch logs stayed under this worktree’s `build/`.

No production fix followed these final suites. The R audit checker was corrected for numeric decoding and full-precision evidence serialization, then its complete row audit, round-trip assertions and high-precision reference checks passed. No suite or implementation tolerance was changed. Site checks were not triggered: no family dossier or public fitter signature changed.

The [validation summary](2026-11-11-quantile-rule-data/validation-summary.log) and [run records](2026-11-11-quantile-rule-data/runs.tsv) retain results, execution machines and timestamps; wrapper timestamps include reservation waits. Full compressed logs: [js-build](2026-11-11-quantile-rule-data/js-build.log.gz), [js-test](2026-11-11-quantile-rule-data/js-test.log.gz), [cpp-test](2026-11-11-quantile-rule-data/cpp-test.log.gz), [cpp-test-no-long-double](2026-11-11-quantile-rule-data/cpp-test-no-long-double.log.gz), [test](2026-11-11-quantile-rule-data/test.log.gz), [test-full](2026-11-11-quantile-rule-data/test-full.log.gz).

## Reproduction and source integrity

From the worktree root, after building the R and JS bindings:

```sh
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
/tmp/umvl-cpu/run bash dev/research/evaluations/2026-11-11-quantile-rule-data/evidence.sh
```

The [observer and reference checker](2026-11-11-quantile-rule-data/audit.py), [R observer](2026-11-11-quantile-rule-data/audit-r.R) and [JS observer](2026-11-11-quantile-rule-data/audit-js.mjs) rebuild or call the real family kernels and retain all decision lists. The evidence run checks every active continuous row, then independently checks every new decision and the separate regression probe. The [provenance record](2026-11-11-quantile-rule-data/provenance.json) records the baseline, compiler/interpreter versions, source and observed-binary hashes, and hashes for the unchanged family d/p/q/r, truth and golden files. No source trees were archived.

Open implementation or CDF-accuracy findings for TASK-197: none. Truth tables, default fits, goldens, settled family kernels and the forbidden project-status files are unchanged. No commit, push, publication or board edit was made.
