**TASK-150.2.3 — acc-allfree**

The six all-free default golden fits meet the `1e-10` coefficient accuracy target in native C++, double-intermediate C++, R and WASM. The largest measured golden coefficient error is `9.8434e-14`. All 16 all-free cases in the selected audit replay also meet `1e-10`, and all 251 selected cases still return fits. All eight required final validation commands finished successfully.

This job is based on `c60d92ea667e8dcb55ec6c6cbd1aaae474846d45`, on branch `codex/acc-allfree`. Final validation completed on 2026-11-11, Europe/Oslo. No commits were created.

**Golden measurements.** The table gives the largest relative coefficient error against each frozen, independently computed reference. References were computed at 50-digit working precision and stored to about 25 significant digits. Comparisons use the exact captured binary64 values. The before columns are fresh runs of c60d92ea; all after columns use the final numerical source. Every golden has empty `fit_args`, so these are default, all-free calls.

| Family | Before native | Before double | After native | After double | After R | After WASM | New allowance |
|---|---:|---:|---:|---:|---:|---:|---:|
| [slash](../../../univariateML-cpp/include/cppunivariate/distributions/slash/README.md) | 1.52e-5 | 1.52e-5 | 6.01e-15 | 9.84e-14 | 6.01e-15 | 9.84e-14 | 3e-13 |
| [sstd](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md) | 1.07e-5 | 4.12e-7 | 8.22e-14 | 1.81e-15 | 8.22e-14 | 2.66e-14 | 4e-13 |
| [sged](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md) | 1.85e-6 | 1.85e-6 | 1.16e-14 | 1.01e-15 | 1.16e-14 | 3.48e-15 | 3e-14 |
| [st](../../../univariateML-cpp/include/cppunivariate/distributions/st/README.md) | 7.78e-7 | 4.17e-7 | 2.21e-14 | 2.96e-15 | 2.21e-14 | 3.05e-15 | 1.2e-13 |
| [bnbinom](../../../univariateML-cpp/include/cppunivariate/distributions/bnbinom/README.md) | 7.00e-14 | 5.31e-13 | 3.00e-14 | 1.19e-14 | 3.00e-14 | 4.32e-14 | 1.5e-13 |
| [gb2](../../../univariateML-cpp/include/cppunivariate/distributions/gb2/README.md) | 2.38e-13 | 1.97e-7 | 2.42e-14 | 4.65e-15 | 2.42e-14 | 4.76e-15 | 8e-14 |

BNBINOM's golden was already within `1e-10` at this base revision. Its all-free audit case still missed by `2.63e-9`, and the previous stopping rule did not justify the tighter coefficient target. GB2's native golden was also accurate before this change; its double-intermediate golden exposed the larger miss.

The C++ and R allowances only tighten. Generated JS policy agrees with both. Each allowance is below six times its family's largest observed four-mode error. The [policy check](2026-11-10-acc-allfree-data/golden-policy-check.json) retains component errors and allowance ratios; [measurements](2026-11-10-acc-allfree-data/measurements.json) retain the unrounded results. WASM measurements use the actual bound JS functions. Site preparation subsequently rebuilt byte-identical WASM and JS files, as recorded in the [bundle identity check](2026-11-10-acc-allfree-data/site-wasm-identity.json).

**Stopping rule and arithmetic.** Regular all-free candidates receive a joint Newton refinement using analytic first and second chain derivatives of the likelihood. Acceptance requires positive definite observed information and checks of the undamped correction `H^-1 g` in public coefficient units. Two independently evaluated corrections must each be at most `tol/8`. The last full steps must show quadratic contraction or a binary64 rounding plateau; the corrected point receives another information and residual check. Step length, likelihood decrease and a gradient norm alone cannot accept the final fit.

The local certificate is conditional on the computed derivatives, information and comparisons. Its practical floating-point margins include `64 epsilon` times accumulated absolute likelihood terms for line-search comparisons, a `128 epsilon` conditioning screen, and nearby-score probes when amplification by inverse information matters. Those probes preserve score-coordinate correlations and require projected noise plus the remaining correction to fit within the reserved tolerance margin. Final back-transform checks also test coefficient representability. Iterative work uses the existing solver and certificate budget helpers.

Location error uses the fitted scale as its floor: sigma for slash, sd for SSTD/SGED, and omega for ST. ST alpha has floor one. Positive coefficients use relative error bounds, including the GB2 scale's coupled transformation. Existing public control names remain `gradtol` or `reltol`; their all-free accuracy default is now `1e-10`. Numerical accuracy failures have an explicit diagnostic through the existing failure statuses and bindings.

The family-specific changes are:

- **slash:** a joint location/log-scale correction replaces reliance on a scalar profile width. The density is differentiated through its regular expansion at zero. The appended subnormal-scale case exercises the representability failure path.
- **sstd:** the variance-standardizing moment correction is differentiated together with all four coefficients. Half-step gamma ratios and the normal-limit kernel are combined before differentiation. The shape error check includes the public nu offset of two.
- **sged:** the joint correction includes the GED moments and skewness transformation. The attained nu-one fit keeps its closed-form treatment and receives a one-sided natural-shape check. A likelihood tie cannot retain a restriction that the directional check proves wrong.
- **st:** Student probabilities are differentiated through central and tail incomplete-beta continued fractions, including degrees of freedom. Convergence checks cover the value, gradient and Hessian. Normal-limit reformulations avoid cancellation in the weak tail-shape direction. The nu-one restriction uses a three-dimensional correction and a natural-shape directional check.
- **bnbinom:** cancellation-resistant gamma increments and exact zero/one-count forms support the joint log-shape correction. The size/beta exchange convention is preserved. Global covers retain their previous coarse target; exported coefficient boxes retain the actual cover center and radius. A rounding-inconclusive scalar certificate retains its previous proposal after checked local refinement and returns it as uncertified.
- **gb2:** joint refinement propagates beta-shape, power and location errors into every public coefficient. Gamma-limit centering and the centered large-shape beta kernel remove leading cancellations before differentiation. With power and scale held, a beta mean/precision reduction propagates transformation uncertainty into the two shape errors. The held-subset certificate routes retain their scope.

All six refinements compensate sums of likelihood values, gradient components and Hessian components. This matters under replication: summation drift can dominate a tiny score even when individual derivatives are accurate. Global certification remains the existing family logic.

**Independent numerical checks.** The ordinary SSTD full-tier sample with fitted nu about `84.0887279588816` and a 100,000-observation replicated sample have independent 70-digit stationary roots with positive observed information. Replication multiplies the base likelihood by 100 and therefore preserves its maximizer. All eight additional default captures, two cases in four modes, pass `1e-10`; their largest normalized coefficient error is `3.72e-14`. The [reference calculation](2026-11-10-acc-allfree-data/check_sstd_stress.py), [references](2026-11-10-acc-allfree-data/sstd-stress-reference.json) and [mode comparisons](2026-11-10-acc-allfree-data/sstd-stress-measurements.json) retain the evidence.

ST also has a 55-digit near-normal reference with nu about `1272.65229995464`, checked at three affine scales by default C++ calls. Separate Student probability derivative references use 60 and 75 working digits; the large-q beta-normalizer reference uses 100 digits. These are reference precisions. Binary64 agreement is assessed by the appended C++ comparisons and fitted-coefficient checks. The independent roots and derivative comparisons are numerical evidence; they do not expand the scope of the existing global proofs. See the [ST root](2026-11-10-acc-allfree-data/st-near-normal-reference.json) and [derivative checks](2026-11-10-acc-allfree-data/derivative-checks.json).

**Audit replay.** The frozen selection contains 251 coefficient cases across these six families. Before and after replays use native C++ common bindings with their respective defaults. All cases return fits, with no new stops. The archived selection contains 16 all-free cases:

| Family | All selected cases | All-free cases | Largest all-free error before | After | Held-only discrepancies remaining |
|---|---:|---:|---:|---:|---:|
| slash | 19 | 12 | 2.49e-7 | 3.51e-14 | 7 |
| sstd | 35 | 0 | — | — | 35 |
| sged | 34 | 0 | — | — | 33 |
| st | 68 | 3 | 4.93e-7 | 3.54e-13 | 46 |
| bnbinom | 39 | 1 | 2.63e-9 | 1.12e-14 | 0 |
| gb2 | 56 | 0 | — | — | 0 |

The [replay summary](2026-11-10-acc-allfree-data/replay-summary.json) lists exact case indices. The [per-case comparisons](2026-11-10-acc-allfree-data/replay-analysis.json) retain masks and errors, and the [frozen cases](2026-11-10-acc-allfree-data/audit-cases.json) retain data, holds and reference candidates.

**Final validation.** Every command below finished with exit code zero on the workstation. Heavy commands reserved four CPU slots; captures and the JS test command used one. BLAS/OpenMP were pinned to one thread per worker and the slot wrappers supplied `nice -n 10`. Simbox was unavailable under this job's instructions. The double-intermediate suite ran locally with GCC 15 and `BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS`.

| Command | Machine | Final result |
|---|---|---|
| `just cpp-test` | Workstation | 106/106 dev shards |
| `just test` | Workstation | 23,010 passes; 0 failures, 0 warnings, 154 skips |
| `UMVL_TEST_TIER=full just cpp-test` | Workstation | 131/131 full shards |
| `just test-full` | Workstation | 45,825 passes; 0 failures, warnings or skips |
| `just cpp-test-no-long-double` | Workstation | 131/131 shards |
| `just js-build` | Workstation | Successful build |
| `just js-test` | Workstation | 595/595 tests |
| `just site-check` | Workstation | 99 C++17 snippets compile/link; HTML checks pass |

The [test ledger](2026-11-10-acc-allfree-data/final-tests.json), [concise final output](2026-11-10-acc-allfree-data/final-test-output.txt), and [source/toolchain manifest](2026-11-10-acc-allfree-data/manifest.json) record the final snapshot. The manifest verifies that frozen goldens and the restricted source paths are unchanged. No changes were made to dpqr, special functions, truth tables, production code-generation sources, std/lst, NEWS, status files or the roadmap. Existing test assertions keep their intent; new C++ regression cases are appended.

For reproduction, run [baseline.sh](2026-11-10-acc-allfree-data/baseline.sh) through `/tmp/umvl-cpu/run` from the repository root. It reads the pinned c60d92ea snapshot without changing Git state. After `just regen` and `just js-build` through the slot wrappers, run [recapture.sh](2026-11-10-acc-allfree-data/recapture.sh) through the one-slot wrapper. R capture uses pkgload and limits compilation to one make job. Then run [verify_measurements.py](2026-11-10-acc-allfree-data/verify_measurements.py) through the one-slot wrapper. The baseline generator was checked to emit exactly the retained before-capture input.

**Open — artifact pane registration.** The prescribed `show` command returned HTTP 403 because Nook's readable-root registry does not include this worktree. Displaying the completed report requires a Nook thread registered with `/home/jonas/Files/programming/univariateML.wt/acc-allfree` as its working directory. No viewer configuration or other checkout was changed.

**Open — held-only replay discrepancies, outside this all-free job.** The 121 remaining pure-relative discrepancies are slash 7, SSTD 35, SGED 33 and ST 46. Their exact indices, hold masks, inputs, archived reference candidates and measured errors are retained in the replay files linked above. They need independent held-route stationary-point and coefficient-accuracy adjudication, followed by held-route stopping-rule work where a miss is confirmed. This report makes no coefficient-accuracy claim for those 121 held-only cases; the requested held-subset certificate logic remains in place.
