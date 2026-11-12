# TASK-204: protected densities and large-shape beta tails

All requested cases are closed. The C++ density defects in beta-binomial,
loggamma and lognormal are repaired; the seven beta-tail rows are active again
for beta, beta-prime, Feller–Pareto and GB2. The four Pareto IV cases were already
closed by the inherited shifted-ratio correction and now have permanent
regressions. Discrete gamma has a bounded, centered unit-cell integral.

The baseline is `217ab8b533cac25ccc8eba8d66eccde1b376f0b4`. Changes are
uncommitted on `codex/dpq-tails`. No fitter, golden, binding, code generator,
zero-modified family, board, status file or roadmap was edited.
[Source checks and hashes](2026-11-12-dpq-tails-data/source-policy.json) record
the 23 modified tracked files and verify that existing C++ tests remain an
unchanged prefix and dossier front matter is unchanged.

The changes address the arithmetic at its source:

- **bbinom, case 75:** large counts with total shape below 16 use the stable
  gamma-ratio mass identity already used by the fitter. This removes the long
  accumulation of cancelling rising-factorial logarithms.
- **lgamma, cases 4435 and 4438:** a two-word logarithm survives multiplication
  by `ratelog` and subtraction of `shapelog`. The density uses the resulting
  centered gamma deviance near the mean. The ordinary gamma path remains for
  remote tails and small shapes.
- **lnorm, cases 4601, 4604 and 4611:** subtraction of `meanlog` takes place
  before rounding the two-word logarithm to the standardized residual.
- **pareto4, cases 5665, 5666, 5668 and 5669:** the baseline already passes all
  four. Only regression cases were added for this family.
- **beta tails:** below the backend probability threshold `1e-290`, the old
  prefactor subtracted large log-beta terms from large weighted logarithms.
  The shared helper now uses the centered beta logit density as prefactor.
  A positive hypergeometric series supplies an explicit tail remainder on its
  guarded branch. All four affected families inherit this change.
- **dgamma:** smooth cells are integrated in physical coordinates, retaining
  `rate * midpoint - shape` with FMA. This avoids both rounded transformed
  endpoints and subtraction of nearby gamma tails.

The beta expansion is an exact incomplete-beta identity. For `a,b>=16`, its
successive ratios are `r_n=x*(a+b+n)/(a+1+n)`. They decrease because `b>1`.
The branch requires `x*(1+b/a)<0.999`; after term `c_n`, the entire omitted
positive sum is bounded by `c_n*r_n/(1-r_n)`. Return requires that bound to be
at most `1e-14` times the compensated partial sum. Thus the stopping test
bounds the remaining series, rather than checking only its last update.

For gamma cells, let `m=x+1/2` and `s=(a-1)/m-r`. On the complex circle of
physical radius two, the normalized kernel is bounded by
`M=exp(2*abs(s)+(a-1)*(2/m)^2/(2*(1-2/m)))`. Cauchy's coefficient inequality
bounds the remainder after degree 32 over the half-width `1/2` by
`M*(1/4)^33/(1-1/4)`. The implementation admits representable midpoints with
`a>=16`, `x>4`, `x<2^52`, a resolved centered displacement and `log(M)<=10`;
it additionally checks a relative integral remainder of `1e-13`.

These are exact-arithmetic truncation bounds conditional on the computed
comparisons. Floating-point accuracy is supported separately by independent
100-digit density definitions and scaled gamma-cell quadrature at the exact
binary64 inputs. The stricter internal remainder targets leave room within
the `1e-10` evaluation contract.

There are eleven appended C++ regression cases covering 62 observations.
The [reference generator](2026-11-12-dpq-tails-data/regressions.py) and
[reference rows](2026-11-12-dpq-tails-data/density-references.json) retain the
definitions and inputs. Across both native modes, the 124 density checks have
maximum normalized log error `1.8713e-15`.
[Density results](2026-11-12-dpq-tails-data/density-regressions.json) and
[WASM results](2026-11-12-dpq-tails-data/js-regressions.json) record the values.
All seven restored natural beta tails, including values below `1e-300`, pass
in both native modes and WASM; maximum relative error is `1.4439e-11`.
[Tail results](2026-11-12-dpq-tails-data/beta-regressions.json) and
[reference preservation](2026-11-12-dpq-tails-data/reference-preservation.json)
show that no oracle value, parameter point, quantile, normalization or sampler
point was replaced. The historical oracle-gaps measurement ledger remains
unchanged; the live exclusions in the four truth routes and tables are lifted.

The historical `5e-6` discrete-gamma witness discrepancy is already much
smaller at this baseline, following the inherited gamma-endpoint repair.
At the exact stopped-case-997 witness, the baseline public evaluator differs
from the independent summed log mass by `4.8887e-11`. The new cell integral
agrees with its rounded 100-digit reference,
`-14.298316842370374`, at printed double precision.
[Witness replay](2026-11-12-dpq-tails-data/gamma-witness.json) records both
versions. The free fit in case 997 retains its existing precision stop.

The before/final replay covers **1,897 archive rows and nine goldens** across
all nine families: 1,244 successful outcomes and 662 stops. Every coefficient
and successful/stopped outcome is preserved. All nine golden outputs are
unchanged. Only two reported likelihoods change, because these rate-held
discrete-gamma fits use the repaired evaluator for their reported value:

| Archive case | Baseline logLik | Final logLik | Baseline absolute error | Final absolute error |
|---|---:|---:|---:|---:|
| dgamma 1025 | −39.20439046995113 | −39.204390469886675 | `6.4454e-11` | `1.0701e-15` |
| dgamma 1027 | −16.199612771090962 | −16.1996127710909 | `5.5427e-14` | `4.9687e-15` |

The errors above use independent 100-digit unit-cell quadrature.
[Likelihood checks](2026-11-12-dpq-tails-data/changed-likelihoods.json),
[fit comparison](2026-11-12-dpq-tails-data/fit-comparison.json),
[baseline replay](2026-11-12-dpq-tails-data/before.jsonl) and
[final replay](2026-11-12-dpq-tails-data/final.jsonl) retain the evidence.
The final seam replay has no mismatches or open fits. Two initial 30-second
audit guards were resolved by the final 180-second replay and
[baseline retries](2026-11-12-dpq-tails-data/baseline-retries.json); these guards
were external audit limits, never fitter statuses. This replay establishes
output preservation and density agreement, rather than a new proof of the
fitters' global optimality.

All final validation is green. Every requested simbox launch returned exit
75, so the prescribed workstation fallbacks were used. Both no-long-double
invocations therefore ran with workstation GCC 15.2. Slot wrappers controlled
all builds and numerical runs, with BLAS/OpenMP pinned to one thread per
worker. Command elapsed times include waiting for CPU reservations.

| Validation | Machine | Result |
|---|---|---|
| `just cpp-test` | workstation, simbox fallback | 108/108 passed |
| `UMVL_TEST_TIER=full just cpp-test` | workstation, simbox fallback | 135/135 passed |
| simbox-requested `just cpp-test-no-long-double` | workstation, simbox fallback | 135/135 passed |
| explicit workstation `just cpp-test-no-long-double` | workstation | 135/135 passed |
| `just test` | workstation, simbox fallback | 25,785 pass; 0 fail, 0 warn, 154 tier skips |
| `just test-full` | workstation, simbox fallback | 54,303 pass; 0 fail, 0 warn, 0 skip |
| `just js-build` | workstation | passed; all 99 families |
| `just js-test` | workstation | 618/618 passed |
| `just site-check` | workstation | passed |
| independent truth consumer, native | workstation | 29,308 checks; 0 failures |
| independent truth consumer, no long double | workstation | 29,308 checks; 0 failures |
| exact-case WASM regressions | workstation | 69/69 passed |

[Suite receipts](2026-11-12-dpq-tails-data/suites.json),
[compressed logs](2026-11-12-dpq-tails-data/validation-logs/),
[native truth receipt](2026-11-12-dpq-tails-data/native-summary.json),
[double truth receipt](2026-11-12-dpq-tails-data/double-summary.json) and
[independent evidence receipts](2026-11-12-dpq-tails-data/evidence-checks.json)
provide commands, return codes, counts and provenance. No numerical case in
the requested set remains open.

To reproduce the independent forward checks from this source tree:

```sh
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR"
task_data=dev/research/evaluations/2026-11-12-dpq-tails-data
/tmp/umvl-cpu/run python3 "$task_data/consumers.py" native
/tmp/umvl-cpu/run python3 "$task_data/consumers.py" double
/tmp/umvl-cpu/run python3 "$task_data/regressions.py"
/tmp/umvl-cpu/run python3 "$task_data/check-regressions.py"
/tmp/umvl-cpu/run python3 "$task_data/check-beta-regressions.py"
# After just js-build:
/tmp/umvl-cpu/run node --v8-pool-size=1 "$task_data/regressions.mjs"
```

The [suite driver](2026-11-12-dpq-tails-data/suites.py) runs every suite in the
foreground and implements the simbox fallback; invoke it without an outer
slot wrapper. [retry-baseline.py](2026-11-12-dpq-tails-data/retry-baseline.py)
rebuilds missing baseline headers from the recorded commit with `git archive`
inside ignored scratch space. No archived source tree is retained in the
evaluation data.
