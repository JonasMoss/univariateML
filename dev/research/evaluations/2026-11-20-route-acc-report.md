# Modifier route accuracy and the truncated-binomial escape

TASK-150.5 and the truncated-binomial entry of TASK-214.5. Baseline source
`1252894e88e09010c19599ce88f6f366d97a2495`.

The modifier routes now use the caller's tolerance to accept free
coefficients. The archived truncated-binomial fit 7325 now escapes to the
truncated-Poisson model with a certificate. In both precision modes, all
2,054 canonical fits matched the stored baseline and all 1,950 route cases
were comparable. Each route replay had the same 24 changes of outcome kind.
Twenty-three truncated-normal cases became estimates. Case 7325 became a
named limit.

## Accuracy rules

The width and comparison arguments hold in exact arithmetic conditional on
the computed signs. The implementation uses practical floating-point margins
for those signs and for public coefficient arithmetic. The truncated-normal
local fallback uses an applied Newton correction at positive-definite
information and checks contraction of successive steps. Global certification
and local coefficient accuracy are separate claims.

| Route | Previous rule | Acceptance at the requested tolerance and justification | Regression added |
| --- | --- | --- | --- |
| Truncated normal | The enclosure used a fixed `1e-8` in public units. | Scale and center the dual enclosure, then map its corners to public mean and scale. Both public widths must meet `tol`. Use compensated moments and center the held-mean tail score at the nearest endpoint. The local information step can establish coefficient accuracy where the global enclosure remains open. `norm/tnorm_mle.hpp:585–690` | R mean and scale at two tolerances. C++ archive scale with an exterior held mean. |
| General Poisson window | The route checked one root and returned the legacy fit from a separate 120-step loop. | Return the controlled monotone conditional mean root and its likelihood. The signed recurrence bracket bounds the relative rate error. `bindings/common/modifiers.hpp:127`, `pois/tpois_mle.hpp:37`, `pois/tpois_profile.hpp:43` | R reference for a finite window. |
| Zero-truncated and zero-modified Poisson | Fixed 100-step bisection with unguarded score signs and an unchecked zero fraction. | The positive-mean equation is monotone. Signed updates exceed the arithmetic margin, including near the zero-rate boundary. The signed enclosure controls the reported rate even when remaining work yields a finer representative. For free `p0`, use the FMA residual of the represented count ratio and a rounded-up error. `pois/ztpois_mle.hpp:12–76`, `core/zero_modified_fit.hpp:15–25`, `pois/zmpois_mle.hpp:33` | R positive-rate tests for both routes and a free zero-fraction check. |
| Zero-inflated Poisson | The held-inflation solver used a fixed relative rate width. | Require the requested width in a signed peak bracket. For free inflation, check the Lambert W rate against a signed positive-mean enclosure and propagate that enclosure through the monotone inflation formula. The optional tighter refinement compares peaks. `pois/zip_certificate.hpp:23–91`, `pois/zip_mle.hpp:149–194` | R high-precision references for the free and held-inflation fits. |
| Zero-modified logarithmic | Fixed mean residual and replay width. | Bracket the transformed monotone mean equation and probe both signs in public theta units. The primary solve and replay share a work allowance. For near-one theta and a short budget, a large-mean start and one Newton step can close the public sign check. `lgser/zmlogarithmic_mle.hpp:8–60` | R theta against an independent root at two tolerances. |
| Truncated count windows and integer-size binomial profile | One bracket update included ambiguous recurrence scores. The binomial profile had no corresponding public probability check. | Require a signed margin at both recurrence endpoints, with error scaled by the weighted path length. Center the integer-size score at its recurrence anchor and include probability-conversion rounding in the public width. `core/truncation.hpp:29–160`, `binom/tbinom_mle.hpp:106–162` | R fixed-size binomial conditional-probability reference. |
| Zero-modified and zero-truncated negative binomial | The free zero fraction and the attained size-zero boundary could bypass interior checks. | Check the count-ratio residual even when size and probability are held. At size zero solve the logarithmic mean equation in the public probability complement, then evaluate the likelihood at the returned probability. Exact point-mass coordinates need no polishing. `nbinom/zmnbinom_mle.hpp:24`, `nbinom/ztnbinom_accuracy.hpp:10–21`, `nbinom/ztnbinom_mle.hpp:92–104` | R zero-fraction resolution condition with candidate and R size-zero probability reference. |
| Censored Weibull | The joint solve checked a shape bracket without a scale enclosure. The scale-held all-censored branch could conflate local resolution with certification. | Bound the variation of log scale over the log-shape bracket using the entropy of the normalized powers. With scale held, compare score noise divided by shape curvature with `tol` and retain the candidate on conditioning failure. `weibull/censored.hpp:76–97, 162–174, 243–267` | C++ joint shape and scale roots and a scale-held conditioning case. |
| Truncated-binomial size escape | An exhausted coverage search could replace a better finite witness with its first visited size. | Keep the stronger finite witness. For a common window ending at the sample maximum, use convex order to certify a strictly increasing profile when the empirical put inequalities pass. A profile still rising at the size cap can yield a margin-based escape with its certificate flag unset. `binom/tbinom_mle.hpp:175–209, 351–372, 548–666` | C++ archive 7325 and 7353 together through the controlled fitter. |

The iterative caps are based on the work-budget units for solver iterations,
certificate evaluations, and enumeration cells. The existing wholly held
Poisson likelihood evaluation retains its evaluation-only normalizer cap.
The reference values for the small examples come from independent 70-digit
score equations. [Reference script](2026-11-20-route-acc-data/references.py) ·
[Reference values](2026-11-20-route-acc-data/references.txt)

## Snapshot comparisons

The comparison uses the stored four-mode baseline. “Changed” counts complete
result records, including iteration counts and diagnostic payloads. The
coefficient column gives the largest relative move among estimate-to-estimate
records. A zero denotes exact coefficient agreement. Held coefficients are
included in the comparison.

| Route | Compared per mode | Changed native / double | Max coefficient move native / double |
| --- | ---: | ---: | ---: |
| Binomial window | 263 | 132 / 132 | 1.788e-12 / 1.788e-12 |
| Zero-inflated binomial | 151 | 0 / 0 | 0 / 0 |
| Zero-modified binomial | 151 | 0 / 4 | 0 / 0 |
| Zero-truncated binomial | 87 | 0 / 3 | 0 / 0 |
| Zero-modified geometric | 79 | 0 / 0 | 0 / 0 |
| Zero-truncated geometric | 39 | 0 / 0 | 0 / 0 |
| Zero-modified logarithmic | 79 | 2 / 2 | 1.110e-16 / 1.110e-16 |
| Zero-inflated negative binomial | 151 | 0 / 0 | 0 / 0 |
| Zero-modified negative binomial | 151 | 0 / 1 | 0 / 0 |
| Zero-truncated negative binomial | 87 | 0 / 2 | 0 / 0 |
| Truncated normal | 285 | 34 / 34 | 2.019e-9 / 2.019e-9 |
| Poisson window | 139 | 75 / 75 | 7.130e-12 / 7.130e-12 |
| Zero-inflated Poisson | 79 | 2 / 5 | 4.790e-16 / 2.771e-14 |
| Zero-modified Poisson | 79 | 32 / 32 | 2.328e-16 / 8.770e-16 |
| Zero-truncated Poisson | 51 | 34 / 34 | 2.328e-16 / 8.770e-16 |
| Truncated zeta | 79 | 0 / 0 | 0 / 0 |

All 75 altered Poisson-window records have a new iteration count. Thirty
also moved a coefficient. The largest move was archive 7917,
`926275.88608270139 → 926275.88608930598`, against the independent conditional
root `926275.88608930635169739`. The largest binomial-window move was the
probability in 7487. The largest truncated-normal moves were archive 7819's
mean and 7736's scale. The new mean `-45848869.694494121` agrees with the
65-digit score root `-45848869.69449414195409`. The new scale
`4.9328687484671587e-5` agrees with `4.93286874846715707459e-5`. The
truncated-normal scale move was $4.12324\times10^{-10}$ relative in both
modes. [Normal archive calculation](2026-11-20-route-acc-data/tnorm_archive.py) ·
[Normal reference values](2026-11-20-route-acc-data/tnorm-archive-reference.txt) ·
[Poisson rate calculation](2026-11-20-route-acc-data/tbinom-exact-ratios.txt)

Every outcome-kind change was one of the following in both modes.

- **Binomial window 7325, estimate to certified limit.** The previous finite
  estimate was improvable, as shown below.
- **Truncated normal, numerical failure to certified estimate.** Compensated
  moment differences, the boundary-centered score for a held exterior mean,
  and an enclosure in coefficient units resolve the previous convergence
  failures. Cases `007675 007676 007688 007689 007691 007692 007720 007721
  007723 007724 007727 007737 007753 007756 007784 007785 007787 007788
  007791 007803 007804 007817 007820`.

The censored-Weibull path is covered by C++ tests. It has no separate
modifier-route snapshot entry. Canonical snapshots of the requested families
and the families that transitively include the changed headers compared
2,054 records across 18 families in each mode, with zero differences.
[Native routes](2026-11-20-route-acc-data/routes-native-diff.txt) ·
[Double-only routes](2026-11-20-route-acc-data/routes-double-diff.txt) ·
[Native families](2026-11-20-route-acc-data/native-diff.txt) ·
[Double-only families](2026-11-20-route-acc-data/double-diff.txt) ·
[Comparison script](2026-11-20-route-acc-data/diff_baseline.py)

## Truncated-binomial archive cases

The old proposal for 7325 had already reached the Poisson face, with a
finite witness at size 11,224,310,574 and log likelihood
−12.253430678463486. The signed-cutoff stage returned about
$4.3767864\times10^{15}$. Coverage of the finite prefix exhausted its
51.2-million-cell allowance at 48,000,376 charged cells and eight profile
evaluations, as the next charge would exceed the budget. The coverage
callback had replaced the best witness with its first finite-size
evaluation. The fallback then promoted that witness at size 1,000,011
to an uncertified estimate. [Native and double C++ trace](2026-11-20-route-acc-data/tbinom-archive.txt) ·
[Trace source](2026-11-20-route-acc-data/tbinom_archive.cpp)

The free window for 7325 ends at the sample maximum. For each feasible
real size $N$, the conditional binomial and the conditional Poisson have
the same support and are matched to the empirical mean. The Poisson-to-binomial
weight ratio is log-convex in count. Together with normalization and equal
means, this gives the two crossings that order their convex expectations.
The empirical distribution exceeds the fitted Poisson in convex order.
The code checks each put against a conservative lower Poisson rate whose
conditional mean is below the target with a rounding margin. At the
independent root the smallest interior put gap is 0.07734525. The derivative
of the profile is the difference between empirical and fitted expectations
of the strictly convex function $k\mapsto\sum_{j<k}1/(N-j)$. It is
positive throughout. This proves the escape in exact arithmetic conditional
on the put comparisons, without needing the finite-prefix coverage.

| Check | Conditional log likelihood |
| --- | ---: |
| 7325 old size-1,000,011 witness, exact rational weight ratios | −17.426030371974598 |
| 7353 finite witness evaluated in the narrower free window, exact ratios | −13.240047870332363 |
| 7325 Poisson face at the represented high-precision rate, exact ratios | −12.253430677034801 |
| 7325 returned face, native and double-only | −12.253430677034792 |
| 7353 returned estimate, native and double-only | −13.958493336451182 |

Both precision modes now return a certified `tpois` escape for 7325.
Case 7353 holds the lower endpoint at zero and returns size 1,000,035.
Its wider conditioning window includes the narrower one, so its maximum
is bounded by the free-window supremum. The direct strict nesting judge
reports no entry for either case in either mode. The strict-known file
was left alone for batch validation.
[Exact-ratio script](2026-11-20-route-acc-data/tbinom_archive.py) ·
[Exact-ratio output](2026-11-20-route-acc-data/tbinom-exact-ratios.txt) ·
[Nesting judge driver](2026-11-20-route-acc-data/check_nesting.py) ·
[Nesting result](2026-11-20-route-acc-data/strict-nesting.txt)

## Tests added

| File and line | Added check |
| --- | --- |
| `bindings/r/modifier-tests/tnorm.R:150` | Public mean and scale at `1e-6` and `1e-13`. |
| `bindings/r/modifier-tests/tpois.R:109` | Controlled finite-window rate at both tolerances. |
| `bindings/r/modifier-tests/ztpois.R:98` | Positive-count rate reference at both tolerances. |
| `bindings/r/modifier-tests/zmpois.R:115` | Rate reference and free zero fraction at both tolerances. |
| `bindings/r/modifier-tests/zip.R:174` | Free rate and inflation, and rate with inflation held, at both tolerances. |
| `bindings/r/modifier-tests/zmlogarithmic.R:115` | Positive logarithmic theta at both tolerances. |
| `bindings/r/modifier-tests/tbinom.R:274` | Integer-size conditional probability at both tolerances. |
| `bindings/r/modifier-tests/zmnbinom.R:178` | Held size and probability with `c(0,1,2)` at `1e-20`, including the one-third candidate and the `1.85037e-17` resolution floor. |
| `bindings/r/modifier-tests/ztnbinom.R:160` | Size-zero logarithmic probability at both tolerances. |
| `norm/test.cpp:716` | Archive held-mean scale with a centered tail score at `1e-13`. |
| `binom/test.cpp:675` | Certified limit likelihood for 7325 and the finite 7353 witness below it. |
| `weibull/test.cpp:392` | Joint censored-Weibull shape and public scale at both tolerances. |
| `weibull/test.cpp:404` | Scale-held all-censored conditioning outcome and candidate shape. |

Existing tests were retained.

## Validation runs

All completed runs used the workstation. Simbox exited 75 on the attempted
R full and native C++ runs, so both fell back to the local four-slot runner.
The shared-header selection from `just test-changed --no-long-double
--dry-run` expanded to all families. The full-tier R run and the named
C++ and JavaScript recipes covered that selection while avoiding a second,
overlapping R dev run.

| Check | Workstation result |
| --- | --- |
| `just test-full` and final affected contexts | Initial run recorded 65,778 passing assertions, 29 warnings, one skip and two assertions expecting the previous Poisson-window diagnostics. After restoring both diagnostics and the late logarithmic-route change, the full-tier `modifiers` and `modifier-kernel-zmlogarithmic` rerun passed 10,168 assertions with zero warnings, skips or failures. The initial full run covered all modifier kernel contexts. |
| `just cpp-test` and final affected targets | Initial 109/110. The old logarithmic small-budget assertion failed. After the late fix, `distribution_lgser`, `modifier_zmlogarithmic`, `no_throw_tests` and `diagnostics_tests` all passed. |
| `just cpp-test-no-long-double` | 154/154 on GCC 15.2. |
| `just js-build` and `just js-test` | Build passed and all 586 tests passed. |
| `just site-check` | Passed, including all 85 C++17 snippets and the HTML checks. |
| `just truth-check` | 36 tables passed in the earlier preflight. |
| `python3 dev/tools/check_links.py` | Zero new broken links, 45 inherited. |
| Four fit snapshot runs with `--jobs 4` | Completed. Both route modes compared 1,950 records and both canonical modes compared 2,054. |
| Direct nesting replay | Passed in both precision modes. |

The targeted late reruns above cover the two final corrections.
[Native route provenance](2026-11-20-route-acc-data/routes-native-provenance.json) ·
[Double-only route provenance](2026-11-20-route-acc-data/routes-double-provenance.json) ·
[Native family provenance](2026-11-20-route-acc-data/native-provenance.json) ·
[Double-only family provenance](2026-11-20-route-acc-data/double-provenance.json)

## Open

The private no-control C++ truncated-binomial overload still has regression
tests that pin the historical witness. The public controlled route and the
explicit-control fitter use the new search and escape. Retiring that private
behavior would require a decision to replace those historical tests.

The 7325 proof uses the common-support hypothesis. For windows extending
above the sample maximum, the cap decision uses the computed comparison
margin and can carry an uncertified escape. The 7353 result is a finite
witness with an uncertified global claim. Its consistency with 7325 follows
from nested windows.
