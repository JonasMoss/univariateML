# TASK-116.1: delegates of certified parents

Completed on 2026-11-05 in codex/deleg-cert, based on fe4573e9b02a950a1440039a8395c30ff2f25926. The final fitting-source fingerprint, per-file hashes, commands, workloads and results are recorded in [validation.json](2026-11-05-deleg-cert-data/validation.json); toolchain versions are in [environment.json](2026-11-05-deleg-cert-data/environment.json). [validate.sh](2026-11-05-deleg-cert-data/validate.sh) reproduces the required suites.

Pareto II, Pareto III and inverse Pareto now have fit.certified = yes for every supported free subset. Burr, inverse Burr, Pareto IV, paralogistic, inverse paralogistic, Fréchet, beta-prime, GB2 and Feller–Pareto remain partial, with their numerical subsets named. Threshold estimation support is unchanged. No golden changed, and no family had to be stopped for a golden discrepancy.

| Family | Subset | Before | After | Reason |
|---|---|---|---|---|
| Burr | Power shape2 held; shape1 and scale free | Already called certified Lomax, but copied only the selected exponential face and collapsed certificate failures | Transports finite/face witnesses, all likelihood bounds and certificate failure categories; detects transformed-data underflow | Exact normalized power transformation to Lomax |
| Burr | Scale held; both shapes free | Historical numerical profile | Retained; aggregate tag remains partial for this reason | No certified parent reduction for this profile |
| Inverse Burr | Joint fit and held-power subsets | Joint certificate and reciprocal transport existed; held certificate failures became generic iteration failures | Preserves budget, tie and numerical categories through the held delegate, including the complete transformed Lomax payload | Reciprocal Burr, including its Jacobian and inverse-Weibull face |
| Inverse Burr | Scale held; both shapes free | Historical numerical profile | Retained and explicitly named in the partial note | Same unresolved fixed-scale problem as Burr |
| Pareto II | Min held; every supported shape/scale subset | Joint Lomax delegate; held subsets dispatched through Burr; partial metadata | Every subset uses the corresponding Lomax overload; complete diagnostics; yes | Translation, shape = kappa and scale = 1/lambda |
| Pareto III | Min held; every supported shape/scale subset | Joint log-logistic delegate; held subsets dispatched through Burr; partial metadata | Every subset uses log-logistic; yes | Exact log-logistic excess distribution |
| Pareto IV | Min held; joint and shape-held subsets | Burr delegates, with stop payload discarded | Transports Burr payload and appends min to interior witnesses; certified joint and held-power subsets | Translation has unit Jacobian |
| Pareto IV | Min and scale held; both shapes free | Numerical fixed-scale Burr profile | Retained; partial | Parent profile has no global certificate |
| Inverse Pareto | Every supported shape/scale subset | Certified Lomax numerics already used, but diagnostics were discarded and notes described an uncertified search | Complete reciprocal transport; inverse-exponential face model; yes | Reciprocal Lomax with public coefficient order (kappa, lambda) |
| Paralogistic | Shape held | Burr with both shapes held: monotone scale solve or all-held evaluation | Retained and identified as the certified subset; partial | Holding the common shape removes the linked-shape constraint |
| Paralogistic | Shape free, jointly or with scale held | Joint BFGS or historical scalar shape search | Retained and explicitly named as numerical | The free common shape links two Burr coefficients; an unconstrained Burr certificate does not solve this constraint |
| Inverse paralogistic | Corresponding held/free common-shape subsets | Reciprocal paralogistic delegation | Same certified held-shape subset and numerical free-shape subsets, documented precisely; partial | Reciprocation preserves the parent's scope; it does not remove the constraint |
| Fréchet | Mu held; any further lambda/sigma holds | Joint inverse-Weibull delegate; held cases used separate closed-form or scalar routines | Held cases now use the corresponding inverse-Weibull overload; partial overall | Exact inverse-Weibull excess distribution |
| Fréchet | Mu free, with lambda/sigma free or held | Numerical location/profile searches | Retained and explicitly named | The excess transformation then depends on a free parameter |
| Beta-prime | Shape1 held at 1, or shape2 held at 1, including further holds | General beta-prime shape/scale profiles | Direct Lomax or inverse-Pareto delegate, with coefficient and stop transport; partial outside these submodels | Exact unit-beta-shape identities |
| GB2 | Shape3 held at 1, or shape1 held at 1, including further holds | General GB2 search | Burr or inverse Burr in the matching held subset; complete payload; partial | Exact density identity; the parent fixed-scale two-shape case remains numerical |
| Feller–Pareto | Min held and either unit-beta-shape submodel | GB2 excess delegate without stop transport | Inherits the GB2 reductions; appends min to interior witnesses and retains face diagnostics; partial | Translation followed by the exact GB2 reduction |

The distinction in the first two rows matters: held-power Burr was already calling the certified Lomax entrypoint. Removing its stale “numerical Weibull comparison” explanation does not make Burr fully certified, because the separate fixed-scale two-free-shape profile remains numerical.

**Transformations and certification.** The following identities are algebraic. Global certification is inherited from the parent algorithm; the regression tests do not independently prove global optimality. The contract remains certification in exact arithmetic conditional on computed comparisons, with the parent's practical floating-point margin. No outward-rounded interval claim is made. See the [Lomax dossier](../../../univariateML-cpp/include/cppunivariate/distributions/lomax/README.md), [Burr dossier](../../../univariateML-cpp/include/cppunivariate/distributions/burr/README.md), and [shared stop-detail note](../../design/structured-stop-details.md).

For held Burr power c, let M = max(log(x)) and y = exp(c*(log(x)-M)). The transformations are:

    log Jacobian = n*log(c) + (c-1)*sum(log(x)) - c*n*M
    Lomax (lambda, kappa) -> Burr (kappa, c, exp(M-log(lambda)/c))
    exponential rate r -> Weibull (shape=c, scale=exp(M-log(r)/c))

This additive likelihood shift is applied to every candidate interval, the global lower/upper bounds, the selected-face likelihood and the unresolved-segment upper bound. Positive transformed observations that underflow to zero stop with NumericalFailure. Successful held-power fits check finite positive transformed coefficients and finite likelihood.

For reciprocation y = 1/x, the additional total log Jacobian is -2*sum(log(x)). Inverse Pareto maps Lomax (lambda, kappa) to (shape, scale) = (kappa, lambda); an exponential rate becomes the inverse-exponential scale. The new InvExp stop model supplies the correct model id and coefficient name in both R and JS. Inverse Burr also reciprocates the Burr scale and maps Weibull to inverse Weibull.

GB2 in public order (q, a, p, b) becomes Burr (q, a, b) when p is held at 1, and inverse Burr (p, a, b) when q is held at 1. Beta-prime (alpha, beta, scale) becomes Lomax with kappa = beta and lambda = 1/scale when alpha is held at 1, and inverse Pareto (alpha, scale) when beta is held at 1. A free beta shape fitted near 1 never activates these reductions.

Translation has unit Jacobian. Interior witnesses include the child's held threshold in public coefficient order. Shifted face coefficients remain parameters of the model on excesses, as the diagnostics explicitly recommend fitting x - min. In particular, an excess Pareto endpoint is not silently relabelled as the endpoint of an unshifted Pareto on raw observations. Unavailable bounds remain unavailable. Stopped witnesses never become successful estimates.

New status/model enum cases preserve existing numeric enum values. Parent failures stop through the bindings; numerical failure and unresolved search are not classified as non-existence. The Pareto III constant-sample regression also checks that holding shape gives a finite scale fit instead of an unbounded-fit status.

**Final validation.** All runs were local, through the shared slot launchers under nice -n 10. Builds/full suites reserved four slots, with BLAS/OpenMP/MKL pinned to one thread per worker, CMake and ctest parallelism four. Documentation generation used one slot. All build/test commands completed in the foreground.

| Command | Final result |
|---|---|
| just cpp-test | 106/106 dev shards passed |
| just cpp-test-no-long-double | 127/127 shards passed |
| just test | FAIL 0; WARN 0; SKIP 154; PASS 20,622 |
| just js-build | Passed; one compiler warning in unchanged binom/mle.hpp:155 |
| just js-test | 421/421 passed; no failures or skips |
| UMVL_TEST_TIER=full just cpp-test | 127/127 shards passed |
| just test-full | FAIL 0; WARN 0; SKIP 0; PASS 43,437 |
| just gen | Passed for final evidence-citation text |
| git diff --check | Passed |

The 154 R dev skips all explicitly require the full tier; the full run skipped none. The full C++ and R tiers each ran once after implementation was settled. The final WASM artifact was rebuilt and retested against the final enum layout. The compiler warning is the existing tautological comparison in the unmodified binomial header. There are no permitted test failures.

New checks use deterministic constants, shifted samples, reciprocals, held power 2, the budget sample (1,1,40,59), extreme reduction failures, and the committed Burr/Lomax goldens. They cover both unit-beta-shape branches, coefficient order, selected faces, budget witness fields, Jacobian-adjusted likelihoods, and density evaluations at finite returned fits. JS face coefficient/likelihood checks use absolute tolerance 1e-10; finite C++ density likelihood checks use relative tolerance 1e-8, and Burr golden/reciprocal-GB2 coefficient checks use 1e-6. Exact parent-dispatch identities and named status/model fields have their committed test assertions. Existing suite seeds and fixtures were retained.

An intermediate JS compile failed after a malformed diagnostic string was introduced during stop wrapping. That string was corrected; the final build and tests above passed. Initially missing CPU launchers caused an infrastructure wait before computation; no slot was held while waiting for them. Final source hashes and compact results are retained with the report rather than only in ignored build logs. All fitter/binding changes are covered by the final checks; citation-only fit-note text was regenerated separately.

**Open mathematical certificates reflected in partial tags.**

- Burr, inverse Burr, and their Pareto IV/unit-beta-shape delegates with scale held and both parent shapes free still need a global profile certificate, including complete root/tail and face comparisons.
- Paralogistic and inverse paralogistic with the common shape free need certificates for the constrained joint profile and the held-scale scalar shape search. The unconstrained Burr winner is not a certificate for equal free shapes.
- Fréchet with mu free needs global location/profile bounds, including the relevant collapse/limit faces. A certified fixed-location inverse-Weibull fit does not certify the location search.
- General beta-prime joint fits and one-held non-unit-shape profiles with scale free retain numerical searches and face comparisons.
- GB2 and Feller–Pareto free-beta-shape profiles with power or scale free retain numerical multibasin/face searches outside the exact unit-shape reductions. Their known concave fixed-beta-shape inner problems and fixed-power/scale beta-shape likelihoods do not certify the remaining outer profiles.

These subsets were inspected and deliberately left partial rather than assigned an invented certificate. No fitting defaults, goldens, settled d/p/q/r or truth files, special-function code, code generators, protected status/roadmap/NEWS files, or other worktrees were edited. No commit or board edit was made.
