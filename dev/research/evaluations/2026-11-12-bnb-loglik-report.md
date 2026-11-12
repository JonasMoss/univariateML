# TASK-212: bnbinom fitter log-likelihood accumulation

The successful fitter result now reports a frequency-weighted sum of the public log masses at its returned coefficients. All 240 before/after comparisons preserve coefficients, statuses, iteration counts and certification flags exactly. All final checks passed; no commits were made.

Baseline: `af824a98d78f2f2325c7a675c20c84c45e6bb826`, branch `codex/bnb-loglik`, with the uncommitted [`mle.hpp`](../../../univariateML-cpp/include/cppunivariate/distributions/bnbinom/mle.hpp) and [`test.cpp`](../../../univariateML-cpp/include/cppunivariate/distributions/bnbinom/test.cpp) changes. [Environment and source/input fingerprints](2026-11-12-bnb-loglik-data/environment.json) and [tested source stages](2026-11-12-bnb-loglik-data/source-stages.json) record the source identity.

## Implementation and evidence

Only `Ok` results are recomputed, after the search, certificate decisions and coefficient refinement. The search objective and comparisons retain their existing arithmetic. The two interchangeable count-shape coordinates are ordered only for density evaluation, preserving exact size/beta swap symmetry. Repeated counts are evaluated once and weighted by frequency.

For `r = size`, `a = alpha`, `b = beta`, the independent reference evaluates the exact identity

```text
log p(x) = logGamma(r+x) - logGamma(r) - logGamma(x+1)
           + logBeta(r+a, x+b) - logBeta(a,b).
```

The public density uses stable gamma ratios at large counts, avoiding cancellation between rising-factorial and factorial terms of order `x log(x)`. The identity is algebraic; the error measurements here are independent numerical evidence at the returned exact binary64 coefficients. They concern likelihood evaluation.

All changed rows were evaluated with a separate 100-decimal-digit mpmath log-gamma expression and rechecked at 200 digits with the beta-function identity. The references agree to at least 88 significant decimal places. Error throughout this report is `abs(logLik - reference) / max(1, abs(reference))`; the [accuracy contract](../../design/accuracy-contract.md#log-likelihood) requires at most `1e-10`.

## Replay census

The frozen archive supplies all 112 bnbinom cases. The frozen 500-observation golden (original seed 13086) is replayed with all eight held/free subsets, using its stored estimates for held values. Every fit uses C++ `tol = 1e-10`, `maxit = 600`. Both precision modes rebuild and replay the baseline and final fitter. No fixtures or stored golden estimates were regenerated.

| Precision | Paired cases | Changed logLik | Largest new normalized error |
|---|---:|---:|---:|
| native | 120 | 33 | 7.461e-15 |
| double | 120 | 67 | 7.196e-15 |

Coefficient and status equality is checked on the 17-digit round-trip serialization, including stopped rows. The scripts additionally require identical iteration counts and certification flags. [Complete comparison](2026-11-12-bnb-loglik-data/comparison.json), [200-digit rechecks](2026-11-12-bnb-loglik-data/reference-recheck.json) and [eight focused symmetry checks](2026-11-12-bnb-loglik-data/symmetry.json) retain the measurements.

## Case 579

Data: `[1000000,1000001,1000003,1000008,1000011]`; size `1.0672234787479615` and beta `5.36473383510874` held. Alpha remains `0.0825043451682516`.

| Mode | Before logLik | After logLik | Before error | After error |
|---|---:|---:|---:|---:|
| native | -86.553376730087109 | -86.55337672897528 | 1.285e-11 | 1.414e-16 |
| double | -86.553376740758225 | -86.55337672897528 | 1.361e-10 | 1.414e-16 |

Independent reference: `-86.5533767289752924644097719748706416897202757165785036492159526482699158095088148775514476`.

The appended regression requires `Ok`, unchanged held values, the expected alpha, reference agreement at `1e-10`, and density-sum agreement at `1e-14`. The latter also detects the original native-precision discrepancy. The case passed in both precision modes.

## Validation

| Final check | Machine | Result |
|---|---|---|
| C++ dev¹ | Workstation, GCC 15.2 | 108/108 shards passed; [log](2026-11-12-bnb-loglik-data/cpp-test.log) |
| R dev¹ | Workstation | 29000 pass, 155 skips, 0 failures/warnings; [log](2026-11-12-bnb-loglik-data/test.log) |
| R full | Workstation | 57824 pass, 0 skips, 0 failures/warnings; [log](2026-11-12-bnb-loglik-data/test-full.log) |
| C++ all shards, double-only, simbox fallback | Workstation, GCC 15.2 | 135/135 passed; [log](2026-11-12-bnb-loglik-data/cpp-test-no-long-double.log) |
| Explicit workstation double-only run | Workstation, GCC 15.2 | 135/135 passed; [log](2026-11-12-bnb-loglik-data/cpp-test-no-long-double-workstation.log) |
| Final targeted native + double C++ | Workstation | 2/2 selected shards in each mode; [component log](2026-11-12-bnb-loglik-data/targeted-before-js-retry.log) |
| Final targeted R dev | Workstation | 675 pass, 7 skips, 0 failures/warnings; same component log |
| JavaScript build | Workstation, Emscripten | Passed; same component log |
| Final JavaScript selected + common tests | Workstation | 212/212 passed, 0 skips; [log](2026-11-12-bnb-loglik-data/js-final.log) |

¹ C++ dev and R dev passed before the final reporting-only frequency grouping. After that adjustment, the changed-family C++ entries in both modes and the targeted R file/filtered shared checks were rerun. Full R and both all-shard double-only runs used the final source. The selected families were bnbinom and the conservative reverse-include selection yulesimon.

Simbox was initially unreachable; an R dev attempt later started there but lost its connection. The prescribed wrappers returned exit 75, and all final remote-requested runs completed through the authorized local fallback. There is no successful simbox/GCC 13 validation in this record. [Machine/exit-code ledger](2026-11-12-bnb-loglik-data/suites.tsv) and [full-run driver](2026-11-12-bnb-loglik-data/final_runs.sh) preserve these outcomes.

The first density-sum implementation exposed two failures in the existing exact size/beta swap test in double-only mode; ordering the evaluation coordinates fixed them without editing that test. [Initial failure log](2026-11-12-bnb-loglik-data/targeted-initial.log). The focused replay and final family suites passed afterward.

The first JS test invocation failed because I restored an unrelated generated gengamma metadata entry before testing. Regenerating the runtime metadata fixed those facade failures; the final JS retry passed, then the tracked generated file was restored byte-for-byte. [Cleanup record](2026-11-12-bnb-loglik-data/generated-metadata-drift.json). No JS source change remains.

Workstation environment: g++ (Ubuntu 15.2.0-16ubuntu1) 15.2.0; R version 4.6.1 (2026-06-24); BH 1.90.0.1; Rcpp 1.1.2; Python 3.14.5; mpmath 1.3.0. Exact OS, Boost header version, Node, Emscripten, installed SciPy version (unused), flags and hashes are in the environment record. Slot wrappers provided one-thread BLAS/OpenMP and nice level 10.

## Artifact viewer

The completed report could not be opened in the artifact pane: `bin/show` returned HTTP 403 because this worktree is outside the viewer’s registered readable roots. The remaining delivery item is to register this worktree through a Nook thread or the server’s `CODEX_MATH_WEB_VIEW_ROOTS` configuration. Viewer configuration and other worktrees were left unchanged. [Recorded viewer result](2026-11-12-bnb-loglik-data/viewer-status.json).

## Reproduction

```sh
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-12-bnb-loglik-data/replay.py
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-12-bnb-loglik-data/check_references.py
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-12-bnb-loglik-data/check_symmetry.py
```

[Replay source](2026-11-12-bnb-loglik-data/replay.py) reconstructs the baseline fitter with `git show` in ignored scratch and rejects changes to shared header dependencies. [Reference checker](2026-11-12-bnb-loglik-data/check_references.py) and [symmetry checker](2026-11-12-bnb-loglik-data/check_symmetry.py) are independent executable checks. [Report validation](2026-11-12-bnb-loglik-data/report-validation.json) records the completed matrix. The regression census is finite; it is not a proof over all datasets and parameter values. No implementation or replay items remain open.

## Every changed likelihood

References below are rounded to 22 significant decimal digits for display. The linked JSON retains 90-digit references and the 200-digit rechecks. `golden` means all free; in `golden-mask-k`, bits 1, 2 and 4 hold size, alpha and beta respectively at the frozen golden estimates. Errors use the unrounded high-precision reference.

### native

| Case | Before logLik | After logLik | Reference | Before error | After error |
|---|---:|---:|---:|---:|---:|
| 512 | -31.323575742390133 | -31.323575742390137 | -31.32357574239012911301 | 1.341e-16 | 2.475e-16 |
| 514 | -8.8964842800018271 | -8.8964842800018253 | -8.896484280001824197050 | 3.281e-16 | 1.285e-16 |
| 515 | -13.160593670439832 | -13.160593670439834 | -13.16059367043983043044 | 1.254e-16 | 2.604e-16 |
| 530 | -21.697850248513401 | -21.697850248513404 | -21.69785024851339317666 | 3.526e-16 | 5.164e-16 |
| 531 | -9.2854180736592795 | -9.2854180736592777 | -9.285418073659278097245 | 1.526e-16 | 3.870e-17 |
| 532 | -9.2854180736592795 | -9.2854180736592777 | -9.285418073659278097245 | 1.526e-16 | 3.870e-17 |
| 533 | -9.2854180736592795 | -9.2854180736592777 | -9.285418073659278097245 | 1.526e-16 | 3.870e-17 |
| 534 | -9.2854180736592795 | -9.2854180736592777 | -9.285418073659278097245 | 1.526e-16 | 3.870e-17 |
| 535 | -9.2854180736592795 | -9.2854180736592777 | -9.285418073659278097245 | 1.526e-16 | 3.870e-17 |
| 536 | -9.2854180736592795 | -9.2854180736592777 | -9.285418073659278097245 | 1.526e-16 | 3.870e-17 |
| 538 | -3.0171504465122241 | -3.0171504465122236 | -3.017150446512223810993 | 8.993e-17 | 5.725e-17 |
| 539 | -26.604219524120278 | -26.604219524120168 | -26.60421952412036670865 | 3.321e-15 | 7.461e-15 |
| 543 | -13.167981567784587 | -13.167981567784585 | -13.16798156778458321890 | 2.580e-16 | 1.231e-16 |
| 544 | -21.831438768996303 | -21.831438768996307 | -21.83143876899629540739 | 3.672e-16 | 5.299e-16 |
| 553 | -28.949785509692809 | -28.949785509692688 | -28.94978550969289671085 | 3.024e-15 | 7.196e-15 |
| 555 | -6.6544258469538713 | -6.6544258469538722 | -6.654425846953869294992 | 3.003e-16 | 4.337e-16 |
| 571 | -13.175524849975195 | -13.175524849975194 | -13.17552484997519203832 | 2.515e-16 | 1.167e-16 |
| 572 | -22.809365493566645 | -22.809365493566641 | -22.80936549356663561286 | 4.116e-16 | 2.558e-16 |
| 579 | -86.553376730087109 | -86.55337672897528 | -86.55337672897529246441 | 1.285e-11 | 1.414e-16 |
| 581 | -33.744374298737888 | -33.744374298737988 | -33.74437429873797940990 | 2.700e-15 | 2.478e-16 |
| 584 | -8.8562916775837497 | -8.8562916775837479 | -8.856291677583746786631 | 3.277e-16 | 1.271e-16 |
| 585 | -13.1599628749158 | -13.159962874915799 | -13.15996287491579707040 | 2.540e-16 | 1.190e-16 |
| 586 | -21.752405968189052 | -21.752405968189056 | -21.75240596818904484270 | 3.305e-16 | 4.938e-16 |
| 595 | -26.911810761422817 | -26.911810761422942 | -26.91181076142290602471 | 3.299e-15 | 1.322e-15 |
| 597 | -6.9947936327226428 | -6.9947936327226437 | -6.994793632722640602113 | 3.088e-16 | 4.358e-16 |
| 601 | -9.7832164559249826 | -9.7832164559249808 | -9.783216455924981170375 | 1.429e-16 | 3.865e-17 |
| 602 | -9.7832164559249826 | -9.7832164559249808 | -9.783216455924981170375 | 1.429e-16 | 3.865e-17 |
| 603 | -9.7832164559249826 | -9.7832164559249808 | -9.783216455924981170375 | 1.429e-16 | 3.865e-17 |
| 604 | -9.7832164559249826 | -9.7832164559249808 | -9.783216455924981170375 | 1.429e-16 | 3.865e-17 |
| 605 | -9.7832164559249826 | -9.7832164559249808 | -9.783216455924981170375 | 1.429e-16 | 3.865e-17 |
| 606 | -9.7832164559249826 | -9.7832164559249808 | -9.783216455924981170375 | 1.429e-16 | 3.865e-17 |
| 607 | -193.69450391863114 | -193.69450391752389 | -193.6945039175238518332 | 5.717e-12 | 1.917e-16 |
| 609 | -50.033713743375365 | -50.033713743375458 | -50.03371374337545047388 | 1.701e-15 | 1.452e-16 |

### double

| Case | Before logLik | After logLik | Reference | Before error | After error |
|---|---:|---:|---:|---:|---:|
| 500 | -8.8374886636323211 | -8.8374886636323247 | -8.837488663632299945221 | 2.394e-15 | 2.796e-15 |
| 501 | -13.149876067851441 | -13.149876067851444 | -13.14987606785146708624 | 1.998e-15 | 1.728e-15 |
| 512 | -31.323575742390158 | -31.32357574239014 | -31.32357574239012911301 | 9.281e-16 | 3.610e-16 |
| 514 | -8.8964842800018253 | -8.8964842800018271 | -8.896484280001824197050 | 1.285e-16 | 3.281e-16 |
| 515 | -13.160593670439823 | -13.160593670439829 | -13.16059367043983043044 | 5.494e-16 | 1.445e-16 |
| 526 | -31.307058128829247 | -31.307058128829222 | -31.30705812882921631768 | 9.904e-16 | 1.960e-16 |
| 527 | -6.618598029345149 | -6.6185980293451525 | -6.618598029345143568708 | 8.144e-16 | 1.351e-15 |
| 528 | -8.8383290309271345 | -8.8383290309271274 | -8.838329030927140662777 | 6.960e-16 | 1.500e-15 |
| 529 | -13.151795594366035 | -13.151795594366027 | -13.15179559436602974159 | 4.304e-16 | 2.449e-16 |
| 530 | -21.697850248513447 | -21.69785024851344 | -21.69785024851339317666 | 2.481e-15 | 2.154e-15 |
| 538 | -3.0171504465122254 | -3.017150446512225 | -3.017150446512223810993 | 5.315e-16 | 3.843e-16 |
| 539 | -26.604219524120737 | -26.604219524120339 | -26.60421952412036670865 | 1.391e-14 | 1.051e-15 |
| 540 | -31.340464092660685 | -31.340464092660678 | -31.34046409266068754085 | 7.390e-17 | 3.006e-16 |
| 541 | -6.6192484805144458 | -6.6192484805144449 | -6.619248480514432960969 | 1.941e-15 | 1.807e-15 |
| 542 | -8.9173174866046434 | -8.917317486604631 | -8.917317486604646475791 | 3.442e-16 | 1.739e-15 |
| 543 | -13.167981567784569 | -13.16798156778458 | -13.16798156778458321890 | 1.091e-15 | 2.816e-16 |
| 544 | -21.831438768996236 | -21.831438768996293 | -21.83143876899629540739 | 2.725e-15 | 1.210e-16 |
| 553 | -28.949785509693349 | -28.949785509692688 | -28.94978550969289671085 | 1.563e-14 | 7.196e-15 |
| 554 | -31.350233359968051 | -31.350233359968058 | -31.35023335996804134740 | 3.130e-16 | 5.397e-16 |
| 555 | -6.6544258469538669 | -6.6544258469538686 | -6.654425846953869294992 | 3.671e-16 | 1.002e-16 |
| 556 | -8.853089645933867 | -8.8530896459338777 | -8.853089645933874637692 | 8.574e-16 | 3.465e-16 |
| 557 | -13.155445996168043 | -13.155445996168044 | -13.15544599616803045825 | 9.195e-16 | 1.055e-15 |
| 558 | -21.687548194979041 | -21.68754819497903 | -21.68754819497894156295 | 4.563e-15 | 4.071e-15 |
| 568 | -31.350236904136885 | -31.350236904136882 | -31.35023690413687622186 | 2.841e-16 | 1.708e-16 |
| 569 | -6.7015727692685063 | -6.7015727692685072 | -6.701572769268513538472 | 1.078e-15 | 9.456e-16 |
| 570 | -8.9670087747364864 | -8.9670087747364846 | -8.967008774736487318411 | 1.038e-16 | 3.019e-16 |
| 571 | -13.175524849975185 | -13.175524849975188 | -13.17552484997519203832 | 5.575e-16 | 2.878e-16 |
| 572 | -22.809365493566606 | -22.809365493566617 | -22.80936549356663561286 | 1.302e-15 | 8.345e-16 |
| 573 | -9.172265846884244 | -9.1722658468842422 | -9.172265846884243847151 | 1.188e-17 | 1.818e-16 |
| 574 | -9.172265846884244 | -9.1722658468842422 | -9.172265846884243847151 | 1.188e-17 | 1.818e-16 |
| 575 | -9.172265846884244 | -9.1722658468842422 | -9.172265846884243847151 | 1.188e-17 | 1.818e-16 |
| 576 | -9.172265846884244 | -9.1722658468842422 | -9.172265846884243847151 | 1.188e-17 | 1.818e-16 |
| 577 | -9.172265846884244 | -9.1722658468842422 | -9.172265846884243847151 | 1.188e-17 | 1.818e-16 |
| 578 | -9.172265846884244 | -9.1722658468842422 | -9.172265846884243847151 | 1.188e-17 | 1.818e-16 |
| 579 | -86.553376740758225 | -86.55337672897528 | -86.55337672897529246441 | 1.361e-10 | 1.414e-16 |
| 581 | -33.74437429873791 | -33.744374298737988 | -33.74437429873797940990 | 2.068e-15 | 2.478e-16 |
| 583 | -6.6731644275658262 | -6.6731644275658315 | -6.673164427565831443147 | 7.897e-16 | 8.882e-18 |
| 584 | -8.8562916775837515 | -8.8562916775837444 | -8.856291677583746786631 | 5.282e-16 | 2.741e-16 |
| 585 | -13.159962874915797 | -13.159962874915808 | -13.15996287491579707040 | 1.599e-17 | 7.939e-16 |
| 586 | -21.752405968189024 | -21.752405968188977 | -21.75240596818904484270 | 9.761e-16 | 3.099e-15 |
| 587 | -9.7783432073877758 | -9.7783432073877776 | -9.778343207387784457117 | 8.804e-16 | 6.987e-16 |
| 588 | -9.7783432073877758 | -9.7783432073877776 | -9.778343207387784457117 | 8.804e-16 | 6.987e-16 |
| 589 | -9.7783432073877758 | -9.7783432073877776 | -9.778343207387784457117 | 8.804e-16 | 6.987e-16 |
| 590 | -9.7783432073877758 | -9.7783432073877776 | -9.778343207387784457117 | 8.804e-16 | 6.987e-16 |
| 591 | -9.7783432073877758 | -9.7783432073877776 | -9.778343207387784457117 | 8.804e-16 | 6.987e-16 |
| 592 | -9.7783432073877758 | -9.7783432073877776 | -9.778343207387784457117 | 8.804e-16 | 6.987e-16 |
| 594 | -3.3051012323948123 | -3.3051012323948132 | -3.305101232394815303884 | 8.939e-16 | 6.252e-16 |
| 595 | -26.911810761422672 | -26.911810761422942 | -26.91181076142290602471 | 8.711e-15 | 1.322e-15 |
| 597 | -6.9947936327226259 | -6.9947936327226223 | -6.994793632722640602113 | 2.104e-15 | 2.612e-15 |
| 598 | -9.1015220555845531 | -9.1015220555845513 | -9.101522055584570105920 | 1.871e-15 | 2.067e-15 |
| 599 | -13.183540038880892 | -13.183540038880889 | -13.18354003888091006825 | 1.363e-15 | 1.633e-15 |
| 600 | -24.891356172743645 | -24.891356172743635 | -24.89135617274370292383 | 2.319e-15 | 2.747e-15 |
| 601 | -9.7832164559249772 | -9.7832164559249755 | -9.783216455924981170375 | 4.018e-16 | 5.834e-16 |
| 602 | -9.7832164559249772 | -9.7832164559249755 | -9.783216455924981170375 | 4.018e-16 | 5.834e-16 |
| 603 | -9.7832164559249772 | -9.7832164559249755 | -9.783216455924981170375 | 4.018e-16 | 5.834e-16 |
| 604 | -9.7832164559249772 | -9.7832164559249755 | -9.783216455924981170375 | 4.018e-16 | 5.834e-16 |
| 605 | -9.7832164559249772 | -9.7832164559249755 | -9.783216455924981170375 | 4.018e-16 | 5.834e-16 |
| 606 | -9.7832164559249772 | -9.7832164559249755 | -9.783216455924981170375 | 4.018e-16 | 5.834e-16 |
| 607 | -193.69450392722041 | -193.69450391752389 | -193.6945039175238518332 | 5.006e-11 | 1.917e-16 |
| 608 | -7.0422987739576781 | -7.0422987739576772 | -7.042298773957680777859 | 3.781e-16 | 5.042e-16 |
| 609 | -50.033713743375245 | -50.033713743375458 | -50.03371374337545047388 | 4.115e-15 | 1.452e-16 |
| golden | -1275.6474863695582 | -1275.6474863695585 | -1275.647486369558616740 | 2.976e-16 | 1.193e-16 |
| golden-mask-2 | -1275.6474863695576 | -1275.6474863695582 | -1275.647486369558616740 | 8.323e-16 | 2.976e-16 |
| golden-mask-3 | -1275.6474863695585 | -1275.6474863695589 | -1275.647486369558616740 | 1.193e-16 | 2.371e-16 |
| golden-mask-5 | -1275.6474863695587 | -1275.6474863695585 | -1275.647486369558616740 | 5.890e-17 | 1.193e-16 |
| golden-mask-6 | -1275.6474863695582 | -1275.6474863695591 | -1275.647486369558616740 | 2.976e-16 | 4.154e-16 |
| golden-mask-7 | -1275.6474863695589 | -1275.6474863695585 | -1275.647486369558616740 | 2.371e-16 | 1.193e-16 |
