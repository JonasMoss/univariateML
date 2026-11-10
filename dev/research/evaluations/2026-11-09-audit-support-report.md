# TASK-101.7.4: translation and public support restoration

Implementation and final validation completed on 10 November 2026 in `codex/audit-support`, based on master `594c491dd495892b9cffd8977fcc9809fcbec151`. No commits, pushes, dependency installations, board edits or changes to another worktree were made. The GEV work is confined to public restoration and original-data evaluation, as requested alongside `gev-perf`.

## Result and scope

EMG location-only fitting now solves in compensated local coordinates with a translation-independent location target and propagates public rounding into its coefficient and likelihood bounds. Triangular and GEV fits recheck original observations and evaluate the likelihood at the actual public coefficients before accepting them. Free coefficients move outward only within the permitted restoration margin; demonstrated likelihood loss beyond that margin produces a numerical failure.

The saved corpus contains **504 cases: 184 EMG, 168 triangular and 152 GEV**. All were replayed against the frozen master headers and the final headers, both at native defaults and at uniform `tol=1e-10, maxit=100`. Native defaults are EMG `1e-8, 500`, triangular `1e-12, 100`, and GEV `1e-10, 100`; these defaults were not changed.

| Family | Saved cases | Before Ok | After Ok | Before support/raw-likelihood defects | After defects |
|---|---:|---:|---:|---:|---:|
| EMG | 184 | 111 | 111 | 0 | 0 |
| Triangular | 168 | 166 | 158 | 10 | 0 |
| GEV | 152 | 144 | 141 | 8 | 0 |
| Total | 504 | 421 | 410 | 18 | 0 |

The defect column checks support and whether the reported likelihood belongs to the represented coefficients. It does **not** establish maximization: the two EMG ascent failures require the separate root checks below, and 39 unchanged held-search coefficient findings remain open. At uniform controls, Ok counts are 420 before and 409 after, again with 18 represented-point defects before and none after. The decline in Ok counts comprises ten triangular and three GEV demonstrated restoration losses, offset by two triangular fits restored from false incompatibility.

## Changes and evidence

### EMG

The location score subtracts the data minimum before subtracting the local location. Contraction uses `tol*sigma`, independently of the public offset. The initial upper bracket is the smaller of the data range and `n*lambda*sigma^2`: the normal hazard bound makes the score negative there. This also preserves a finite witness with a one-iteration budget. Public restoration includes coefficient rounding in the acceptance test and the concavity tangent likelihood bound; lower/upper public location endpoints are rounded outward.

Case **1375** reproduces master location `100000000.89101982`, reported log likelihood `-25.265991799126692`. The final location is `100000000.60179031`, with log likelihood `-24.976093561307962`. The independently bracketed root gives a public rounding error below `2.967e-9`.

Case **1374**, at scale `1e150`, improves the reported likelihood from `-1.6563463786084462e283` to `-3.7387832080153481e151`. The real root lies about `14.6465093962` above the minimum; that increment cannot be represented in a double at this offset, so the public location is the minimum itself. The local solve retains the increment, and the propagated public error satisfies the relative coefficient contract. A translation-independent local target does not imply an impossible absolute public rounding accuracy at a large offset.

The [400-digit checks](2026-11-09-audit-support-data/precision.json) give signed root brackets for both cases. Appended C++ checks also enclose the independently evaluated maximum likelihood in the reported bounds. All **37** prior EMG coefficient-only root comparisons pass at uniform `1e-10` controls. Case 1267 additionally changes from numerical failure to the existing named translated-exponential limit; no former EMG Ok becomes a stop.

### Triangular

Held-endpoint likelihood factors now use distances from the held endpoint, avoiding cancellation in `1-p*d`. Free modes are restored to the exact original observation rather than its affine round trip. Free support endpoints are moved outward when needed. The original-data density then supplies the returned raw likelihood, which must agree with the profile within a practical `256*epsilon` operand margin.

Cases **11130, 8289 and 8268** primarily exposed incorrect reported likelihoods. Their final reported values agree with independent evaluation at their public coefficients. Case **8195** now contains the observation that its former coefficients excluded. Cases **8270 and 11127** become finite Ok fits after the held-endpoint cancellation fix.

Cases **8165, 8168, 8172, 8173, 8194, 8213, 11117, 11118, 11119 and 11122** have a finite represented-endpoint loss beyond the restoration margin. Independent scalar endpoint optimization at 400 digits supplies feasible improvement witnesses, with gains from approximately `6.808e-11` to `0.00564341`; see [precision.json](2026-11-09-audit-support-data/precision.json). These return the appended `TriangStatus::NumericalFailure`, preserving the old enum values. The shared binding reports a numerical error explaining public coefficient likelihood loss, rather than an iteration-limit error.

Every one of the **168 triangular payloads** is identical with pruning enabled and disabled, separately before and after the change. Thus the observed corpus provides no evidence of a pruned-away maximum; the identified causes are arithmetic and public restoration. [Pruning comparison](2026-11-09-audit-support-data/pruning-validation.json).

### GEV

The internal search, `gev_loglik`, and `shape_certificate.hpp` are unchanged. A separate public likelihood evaluates the original observations with compensated differences and support residuals. Restoration checks every observation, moves only free location/scale coefficients, rechecks coefficient tolerances and compares the raw/public likelihood to the internal value. The likelihood comparison uses the larger of `1e-7*n` and `256*epsilon` times its operands. When a scale ulp would spoil a small accurately fitted scale after translation, an admissible free location ulp can repair support instead. Held values remain exact.

The closed `xi=-1` endpoint uses an error-free sum comparison before division. The extra case `x={-1,0,denorm_min}`, held `sigma=2, xi=-1`, formerly returned Ok at `mu=-2`, excluding the final observation: the divided residual had underflowed to negative zero. It now returns `mu=-1.9999999999999998`, which contains every observation. With all three coefficients held at the incompatible original values it returns InvalidData. This support comparison is independently checked with exact rational arithmetic in [subnormal-validation.json](2026-11-09-audit-support-data/subnormal-validation.json).

Saved cases **2682, 2695, 2707, 10949, 10964, 10965 and 10966** now have contained support and correct raw likelihoods. Cases **2708, 2772 and 10957** return numerical failure: independent feasible witnesses improve their represented likelihoods by approximately `0.3862943611`, `0.04331416546` and `0.000869649644`, respectively. The 400-digit evaluations are numerical evidence, not outward-rounded interval proofs. No GEV performance or conditional-search precision change is included.

The implemented certificates follow the repository standard: exact-arithmetic arguments conditional on computed comparisons, with the stated practical floating-point margins. The independent high-precision checks support those comparisons; they are distinguished from the exact-rational support checks.

## Individual regression cases

Each of the following 29 saved cases has appended C++ and R regression coverage. Existing test contents are preserved in full. Extra cases cover EMG finite witnesses and likelihood bounds, translated GEV scale preservation, large likelihood operands, and the negative-zero support hole; JavaScript covers the triangular numerical error and GEV negative-zero repair.

The likelihood columns are **reported** native-default values, rounded for this table. A former incorrect report can exceed the final correct likelihood without representing a better fit. A failure row's numeric payload is diagnostic, not an accepted estimate.

| Case | Family | Before | After | Before reported logLik | After raw logLik |
|---|---|---|---|---:|---:|
| 1374 | emg | Ok | Ok | -1.65634637861e+283 | -3.73878320802e+151 |
| 1375 | emg | Ok | Ok | -25.2659917991 | -24.9760935613 |
| 11130 | triang | Ok | Ok | -165.733255917 | -166.303867298 |
| 11122 | triang | Ok | NumericalFailure | 169.251402291 | 169.245758881 |
| 11119 | triang | Ok | NumericalFailure | 186.580081805 | 186.574438395 |
| 8194 | triang | Ok | NumericalFailure | 184.666753803 | 184.66323082 |
| 8289 | triang | Ok | Ok | -124.240497649 | -124.240633975 |
| 8173 | triang | Ok | NumericalFailure | 184.986039587 | 184.985923426 |
| 8268 | triang | Ok | Ok | -118.665886764 | -118.665960853 |
| 11118 | triang | Ok | NumericalFailure | 169.722327751 | 169.72230978 |
| 11117 | triang | Ok | NumericalFailure | 187.051007265 | 187.050989294 |
| 8195 | triang | Ok | Ok | -4135.37021479 | -4135.37021479 |
| 8165 | triang | Ok | NumericalFailure | 132.847920808 | 132.847920807 |
| 8168 | triang | Ok | NumericalFailure | 129.642646073 | 129.642646073 |
| 8172 | triang | Ok | NumericalFailure | 127.659289313 | 127.659289312 |
| 8213 | triang | Ok | NumericalFailure | 127.188314177 | 127.188314177 |
| 8270 | triang | Incompatible | Ok | -inf | -176.660456253 |
| 11127 | triang | Incompatible | Ok | -inf | -178.05780045 |
| 2682 | gev | Ok | Ok | -2.47366726845 | -2.47366726845 |
| 2695 | gev | Ok | Ok | 1.18886083843 | 1.18886083843 |
| 2707 | gev | Ok | Ok | 0.0531574061716 | 0.0531574061716 |
| 10949 | gev | Ok | Ok | 0.0221238082479 | 0.0221238082479 |
| 10966 | gev | Ok | Ok | -4.26856303674e+47 | -3.80027905975e+47 |
| 10965 | gev | Ok | Ok | -4.10363691148e+40 | -4.10055238668e+40 |
| 10957 | gev | Ok | NumericalFailure | 151.44206099 | 151.44119134 |
| 10964 | gev | Ok | Ok | -1.44534250765e+21 | -1.44534250718e+21 |
| 2708 | gev | Ok | NumericalFailure | -7603.21403888 | -7603.21403888 |
| 2772 | gev | Ok | NumericalFailure | -16071.0892899 | -16071.0892899 |
| 2788 | gev | InvalidData | InvalidData | -inf | -inf |

Full-precision results for every saved case are in [before.csv](2026-11-09-audit-support-data/before.csv) and [after.csv](2026-11-09-audit-support-data/after.csv); uniform-control results are in [before-contract.csv](2026-11-09-audit-support-data/before-contract.csv) and [after-contract.csv](2026-11-09-audit-support-data/after-contract.csv). CSV columns are index, family, native status, log likelihood and the three coefficients in family order. The corresponding [after validation](2026-11-09-audit-support-data/after-validation.json) and [uniform validation](2026-11-09-audit-support-data/after-contract-validation.json) record independent likelihoods and exact GEV support tests.

## Final runs

All rows marked pass below ran on the **workstation**, GNU C++ 15.2.0, using the shared slot wrappers, one BLAS/OpenMP thread per worker and `nice -n 10`. Full tiers ran after the final fitter change. The R package was regenerated by its normal test workflow. No dependency or system package was installed. The standard R test runner builds and installs univariateML only into its temporary test library under this worktree’s `build/tmp/`.

| Command | Final result | Evidence |
|---|---|---|
| Targeted EMG/triangular/GEV C++ binary | 120 tests, 41,387 assertions; 1 excluded/skipped test | [log](2026-11-09-audit-support-data/logs/targeted-final.log.gz) |
| `just cpp-test` | 106/106 | [log](2026-11-09-audit-support-data/logs/cpp-dev.log.gz) |
| `just test` | FAIL 0, WARN 0, SKIP 154, PASS 22,548 | [log](2026-11-09-audit-support-data/logs/r-dev.log.gz) |
| `just cpp-test-no-long-double` | 129/129 | [log](2026-11-09-audit-support-data/logs/no-long-double-local.log.gz) |
| `UMVL_TEST_TIER=full just cpp-test` | 129/129 | [log](2026-11-09-audit-support-data/logs/cpp-full.log.gz) |
| `just test-full` | FAIL 0, WARN 0, SKIP 0, PASS 45,383 | [log](2026-11-09-audit-support-data/logs/r-full.log.gz) |
| `just js-build`; `just js-test` | Build pass; 585/585 tests | [build](2026-11-09-audit-support-data/logs/js-build.log.gz), [tests](2026-11-09-audit-support-data/logs/js-test.log.gz) |
| `just site-check` | Pass, including 99 generated C++17 snippets and HTML/browser checks | [log](2026-11-09-audit-support-data/logs/site.log.gz) |
| Saved-case replay and independent checks | Both 504-case control sets pass; 168 exhaustive triangular comparisons match | [replay](2026-11-09-audit-support-data/logs/replay-final.log.gz), [asserting validator](2026-11-09-audit-support-data/logs/payload-validation.log.gz) |

The JavaScript compiler emits the existing unrelated `binom/mle.hpp` tautological-comparison warning; JavaScript tests pass. The site check rewrites an old screenshot outside this job; its original bytes were restored afterward.

## Open entries

1. **GEV held-search coefficient precision, outside this restoration change.** At uniform `1e-10` controls, 39 previous root-comparison findings remain, all at exactly the old coefficients: held masks 1 (8), 2 (4), 5 (19), 6 (8), where bits 1/2/4 denote location/scale/shape holds. The largest relative coefficient discrepancy is `4.3548150093281166e-8`. None is all-free. Settling these requires work on the conditional search and its coefficient accuracy, which this job explicitly leaves unchanged alongside `gev-perf`. [Cases and comparisons](2026-11-09-audit-support-data/remaining-coefficient-findings.json).
2. **Settled GEV density endpoint shortcut.** With `mu=1e8, sigma=1e-12, xi=-1` and three observations equal to `mu`, `mu+sigma` rounds to `mu`, but the actual support residual is 1: the observations are interior. The corrected native likelihood is `79.893063347785642`; settled `dgev` reports `82.893063347785642` because its rounded-endpoint shortcut uses `1/sigma` instead of `exp(-1)/sigma`. Needed change: make the density's endpoint shortcut depend on the actual compensated endpoint condition, with the same care for division underflow. `dpqr.hpp`, `special/`, truth tables and generated d/p/q/r were explicitly frozen, so they were not edited. [Reproducer](2026-11-09-audit-support-data/settled-seam.cpp), [result](2026-11-09-audit-support-data/settled-seam.json).
3. **Simbox GCC 13 validation unavailable.** Initial attempts encountered remote WSL filesystem I/O errors; subsequent SSH attempts timed out. Repeated final-source `simbox-run just cpp-test-no-long-double` attempts returned exit 75, invoking the requested local fallback, which passed 129/129. An actual GCC 13 result still requires simbox recovery and that command. [Initial health evidence](2026-11-09-audit-support-data/simbox-health.log), [timeout](2026-11-09-audit-support-data/simbox-unreachable.log), [final attempt](2026-11-09-audit-support-data/simbox-final.log).
4. **Existing repository-wide link findings.** The optional global Markdown link scan has 41 findings in five files unchanged from master. They concern an old PDF reference, saved audit snippets parsed as links, nbinom mathematical markup and inference-site routes. This job does not edit those files. Its modified dossiers and this report have separately checked local destinations. [Scoped check and baseline hashes](2026-11-09-audit-support-data/job-links-validation.json).


## Reproduction and provenance

From this worktree, run `bash dev/research/evaluations/2026-11-09-audit-support-data/run.sh`. It reserves one CPU slot, constructs the baseline from read-only `git show` at the pinned master commit, compiles both native replayers, reruns both control sets and exhaustive triangular comparisons, and executes the independent validators. It requires the already available C++ compiler, BH headers, Python and mpmath. Outputs and executables stay in this worktree; no Git write is needed. The settled-density reproducer intentionally confirms a known discrepancy rather than claiming that density is fixed.

[Provenance](2026-11-09-audit-support-data/provenance.json) records source/input SHA-256 hashes, control sets, final test counts, preserved existing test contents and unchanged goldens. The three family dossiers were updated. No settled density/tail implementation, truth fixture, code generator, NEWS, status, known-issues file or roadmap was changed.
