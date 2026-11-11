# Continuous-family uncertified fallback — TASK-168.2

The original seven-family sections through Reproduction retain the evidence from commit `4a497d0d`. The [Delegate follow-up](#delegate-follow-up) records the current implementation on merged base `6d8a1f06` and the all-green local validation. Open items describe the current state.

The seven owned families (`burr`, `invburr`, `pareto4`, `paralogis`, `betapr`, `bhatt`, `ged`) now retain a successful, unrefuted search when its certificate cannot close, and expose the per-fit certification flag through the common binding. Bhatt's R adapter no longer duplicates the shared conversion. Proved stops and failed or refuted searches remain stops.

The final 1,189-case replay changes **66 stops into uncertified fits**. All **796 previously certified coefficient vectors and log likelihoods are text-identical** in the replay. The final seven-family focused checks pass in both workstation arithmetic modes; R dev/full, JS and site checks pass. Global C++ runs each retain one failure in the explicitly excluded GB2 delegate. The required GCC 13 simbox run could not execute because the machine remained unreachable. These are recorded precisely under Open items.

Worktree: `fallback-cont`, branch `codex/fallback-cont`, base and unchanged HEAD `af295963493ce0b73f678d1316e5c7355914aa89`. Validation finished on 2026-11-11 UTC. No commit was made.

## Implementation and evidence

- **Burr and its reciprocal/shifted delegates:** joint, held-scale and held-power certificate failures retain the existing finite proposer after its convergence/score check, physical likelihood check and feasible-bound comparison. InvBurr and Pareto IV transport `certified`. Distinct raw observations whose logarithms coalesce are a numerical representation failure, not a constant-data nonexistence proof.
- **Paralogis:** both joint and held-scale unclosed covers retain the converged proposal. The reported likelihood must lie in its physical witness interval; a strictly better certificate witness still rejects it. A separate unit case exhausts the cover allowance while preserving the independently converged search.
- **Beta-prime:** joint and conditional unclosed certificates use the existing grid/BFGS proposal with held coordinates removed, a total-score gate and physical finite likelihood/bound checks. Non-unit proposals retain the existing large-shape arithmetic guard and reject a point on that artificial cap. Exactly held unit shapes use `log B(1,k) = -log k`, the free-shape digamma increment `1/k`, separately evaluated complementary logistic terms, and a normalized Lomax scalar nomination; they can return finite coefficients above `1e6`. Normalizing on log data avoids overflowing a Lomax rate when the reciprocal beta-prime scale remains representable.
- **Bhatt:** the already-present fallback also handles an unclosed normal-member inclusion and an unproved uniform preference. The latter reruns the finite-only search. The normal member remains subject to physical likelihood and residual-score checks. Existing Bhatt R tests were left unchanged; only new cases were appended, and the final full suite passes through `common_result` after removal of the adapter shim.
- **GED:** unclosed free-shape, held-sd uniform, numerical root-overlap and conditional point-score checks retain a successful finite search. An artificial upper grid endpoint is rejected. Proved Laplace median intervals remain stops. Exact reflected subunit-shape knot ties also require exclusion of a possible central knot; reflection alone does not prove nonuniqueness against an unresolved central competitor. Physical normalization failures remain failures.

The common bindings emit `extra {"certified",0/1}` on every successful route that can be uncertified. Shared R `common_result` supplies a logical element and `attr(estimates,"certified")`; JS carries 0/1. No per-family R shim was added. `FALSE` means that the search passed its convergence or score gate, while global comparison or coefficient inclusion did not close; it makes no global or coefficient-accuracy claim. The convention is documented in [README_SCHEMA.md](../../../univariateML-cpp/include/cppunivariate/distributions/README_SCHEMA.md) and [Wrapper failure handling](../../validation/diagnostics.md#wrapper-failure-handling-rule-r6).

All seven dossiers were updated with their family-specific scope and comparisons. Controls, golden files, settled d/p/q/r, `special/`, truth tables and code-generation scripts were not changed. New proof work uses the existing `certificate_budget(maxit)` convention; no public control was added.

### Strict reachable-face checks

A delegate's likelihood-margin preference for an exponential face is not a proof that the face wins. The owned transformed-family check closes the whole positive profile or leaves the finite search uncertified. For mean-one transformed data, write `M2 = E(Y²)`, `A(q) = E(log(1+qY))`. The exact-arithmetic bounds are

```text
h(q) = log(q/A(q)) - A(q)
A(q) >= q/(1+q*M2/2)
h(q) <= log(1+t) - (2/M2)*t/(1+t),  t=q*M2/2.
```

The lower bound follows from `log(1+t) >= 2t/(2+t)` and weighted Jensen. When `M2 < 2`, the last upper bound decreases strictly from zero for `0 < t < 2/M2-1`, closing the left tail. Existing Lomax chord bounds cover the remaining compact interval. With `z=log(q)` and `L=-E(log Y)`, the right tail uses `h <= L-log(z-L)`. The implementation requires strict negative comparisons with the stated practical floating-point allowances. This is certification in exact arithmetic conditional on the computed comparisons, not outward-rounded interval arithmetic or a universal libm error bound.

For `x={1,1,1,1,6+delta}`, `M2-2 = (20*delta+3*delta²)/(10+delta)²`: positive delta gives an initially improving finite profile and rules out an exponential winner. On the exact binary64 sample ending in `6.0001`, the independent [80-digit stationary-root comparison](2026-11-10-fallback-cont-data/strict-face-reference.json) finds log-likelihood improvement `3.74991679854024e-10`. This is numerical corroboration of a finite point beating the face, not a global certificate for its stationary root; the analytic sign argument is separate. The [reproducer](2026-11-10-fallback-cont-data/strict_face_reference.py) records that distinction.

An unclosed reachable face remains a likelihood lower bound, even when its coefficients cannot be encoded. It is kept as an unselected witness when representable; it is never mislabeled as the selected global limit. A separated bound can refute a search without proving which model wins globally. For `u=1e-320`, `{u,u,u,u,6u}`, short-work search must therefore keep the finite bound even though the exponential rate overflows. The [pre-fix record](2026-11-10-fallback-cont-data/tiny-face-pre-fix.log) shows an unsafe point below the face by about `3.17e-6`, against a practical margin about `1.67e-9`; the final short-work route rejects it. The final default-work route is finite, unrefuted within the margin and explicitly uncertified.

The additional [96-case face-refutation sweep](2026-11-10-fallback-cont-data/face-refutation-sweep.json) has no returned point refuted beyond its practical margin. This is numerical regression evidence, not proof of global optimality for uncertified results.

## Stop-path audit

“Unclosed certificate” below denotes a former stop cause and the new fallback trigger. If the independent search fails or a separated feasible bound refutes it, a stop remains and can retain the original certificate diagnostic kind. Consequently a residual `tie` or `budget` count is not itself a claim of proved nonuniqueness or nonexistence. Direct input validation has no core status. Defensive binding guards are listed separately from ordinary core statuses.

The table is generated from the retained [stop inventory](2026-11-10-fallback-cont-data/stop-inventory.tsv). In particular, the existing held-common-shape Paralogis dispatcher can map a numerical Burr delegate failure to `InvalidStart`/ordinary error; that path is a search/representation failure, not an unclosed certificate or a proof that positive data are invalid.

| Family | Status / binding kind | Trigger | Classification and final behavior |
|---|---|---|---|
| burr | InvalidInit / ordinary error | Empty or too-short joint sample; nonpositive/nonfinite data, held values or starts; invalid controls | proved stop (invalid input): stop |
| burr | Unbounded / nomle | Constant joint sample, or constant held-shape sample with freely collapsing power/scale | proved nonexistence: stop |
| burr | AllGeOne / limit:pareto | Held scale, all observations >= scale with at least one strictly greater (endpoint ties retain their penalty) | proved limit: stop |
| burr | WeibullBoundary / limit:weibull | Closed joint Weibull-face comparison, or strictly closed held-power exponential-face comparison | proved limit: stop |
| burr | ParetoBoundary / limit:pareto | Closed joint Pareto-face comparison | proved limit: stop |
| burr | CertificateTie / tie | Overlapping finite/face comparisons, uncertain tail signs, unresolved held-scale endpoint or coefficient widths | unclosed certificate: try converged finite search; return Ok,false unless refuted or search fails |
| burr | CertificateBudget / budget | Joint/held-scale/Lomax certificate allowance expires | unclosed certificate: same fallback |
| burr | NumericalFailure / numerical | Unclosed certificate arithmetic/brackets/linear solves | unclosed certificate: same fallback |
| burr | NumericalFailure / numerical | Held-shape logarithms merge distinct observations; held-power transformation underflows; reconstructed coefficients/likelihood nonfinite or inconsistent; feasible lower bound refutes search | search failure or refuted candidate: stop |
| burr | IterationFailure / unresolved | maxit=1 joint search, monotone held-coordinate root does not converge, or Lomax search fails | search failure: stop |
| invburr | InvalidInit / ordinary error | Invalid data, controls, held coefficients or starts | proved stop (invalid input): stop |
| invburr | Unbounded / nomle | Reciprocal Burr collapse or held-first-shape location-scale collapse | proved nonexistence: stop |
| invburr | AllLeOne / limit:power | Held scale, all observations <= scale (with strict inequality somewhere) | proved limit: stop |
| invburr | InvWeibullBoundary / limit:invweibull | Reciprocal Burr closed Weibull comparison | proved limit: stop |
| invburr | PowerBoundary / limit:power | Reciprocal Burr closed Pareto comparison | proved limit: stop |
| invburr | CertificateTie,CertificateBudget,NumericalFailure / tie,budget,numerical | Reciprocal Burr certificate does not close | unclosed certificate: delegate fallback and transport flag |
| invburr | NumericalFailure / numerical | Reciprocal overflow/underflow, artificial reciprocal ties, rounded held-scale equality, invalid physical recovery; transformed location solve fails | search/representation failure: stop |
| invburr | IterationFailure / unresolved | Reciprocal delegate search fails | search failure: stop |
| pareto4 | No core status / ordinary error | min not held, invalid min/holds, observations outside held support | proved stop (unsupported/invalid input): stop |
| pareto4 | InvalidInit / ordinary error | Invalid excess data or controls | proved stop (invalid input): stop |
| pareto4 | Unbounded / nomle | Constant excess joint sample or delegate collapse | proved nonexistence: stop |
| pareto4 | AllGeOne / limit:pareto | All excesses >= held scale; no held-endpoint Pareto binding is available | proved limit: stop, retaining held endpoint and tie penalty |
| pareto4 | WeibullBoundary,ParetoBoundary / limit:weibull,pareto | Closed Burr face comparison on excesses | proved limit: stop, shifted-data recommendation |
| pareto4 | CertificateTie,CertificateBudget,NumericalFailure / tie,budget,numerical | Excess Burr certificate does not close | unclosed certificate: delegate fallback and transport flag |
| pareto4 | NumericalFailure / numerical | Excess overflow, artificial ties or rounded held-scale equality; centered restoration failure | search/representation failure: stop |
| pareto4 | IterationFailure / unresolved | Excess/centered delegate search fails | search failure: stop |
| paralogis | InvalidStart / ordinary error | Invalid data, controls, held values or starts | proved stop (invalid input): stop |
| paralogis | Unbounded / nomle | Constant sample with common shape and scale free | proved nonexistence: stop |
| paralogis | CertificateBudget / budget | Compact exclusion allowance exhausted after a successful proposal | unclosed certificate: return proposal Ok,false |
| paralogis | NumericalFailure / numerical | Root inclusion, tail bounds or compact cover does not close | unclosed certificate: return proposal Ok,false |
| paralogis | NumericalFailure / numerical | Normalization overflow, invalid/nonfinite coefficients or likelihood, inconsistent witness, strictly better certificate point | search failure or refuted candidate: stop |
| paralogis | BfgsFailure / numerical | BFGS fails its convergence/score gate, held-scale shape bracket/solve fails | search failure: stop |
| betapr | InvalidStart / ordinary error | Invalid positive data, controls, holds or starts; too-short joint/fixed-scale sample | proved stop (invalid input): stop |
| betapr | InvalidStart / nomle | Constant positive sample with both shapes free | proved nonexistence: stop |
| betapr | ExpBoundary,InvExpBoundary / limit:exp,invexp | Unit held shape selects an exponential limit with a strictly negative finite-profile cover | proved limit: stop |
| betapr | GammaBoundary,InvGammaBoundary / limit:gamma,invgamma (ordinary error only when payload absent) | Closed conditional or joint gamma/inverse-gamma face comparison | proved limit: stop |
| betapr | BfgsFailure / budget | Certificate work allowance exhausted with finite witnesses | unclosed certificate: try converged free-coordinate search, return Ok,false unless refuted |
| betapr | BfgsFailure / tie | Numerical root/face likelihood intervals overlap; rounded crossing is not an exact tie proof | unclosed certificate: same fallback |
| betapr | BfgsFailure / numerical | Unclosed exterior, root inclusion, signed tail, or coefficient enclosure | unclosed certificate: same fallback |
| betapr | BfgsFailure / numerical | One shape held, scale free, constant data, with unrepresentable gamma/inverse-gamma limit coefficients | proved escape refutes every finite candidate: preserve stop when the limit coefficients cannot be encoded |
| betapr | BfgsFailure / unresolved,numerical,budget | Reciprocal transformation/recovery or underflowed score-statistic failure; beta/shape/scale root or BFGS convergence/score failure, search reaches shape guard, nonfinite or unrepresentable result, refuting lower bound | search failure or refuted candidate: stop |
| bhatt | InvalidInput / ordinary error | Invalid data/controls/holds | proved stop (invalid input): stop |
| bhatt | Unbounded / nomle | Constant data at free/held centre with sigma and a free; normal delegate collapse at a=0 | proved nonexistence: stop |
| bhatt | NormalLimit,UniformLimit / limit:norm,unif | Closed normal/uniform comparison (compatible held parameters retained) | proved face comparison: stop; normal a=0 is attained Ok |
| bhatt | BudgetExhausted,Unresolved,Tie,NumericalFailure,IterationFailure / budget,unresolved,tie,numerical | Unclosed width cover, face comparison or coefficient enclosure | already tries existing search and returns Ok,false when successful and unrefuted |
| bhatt | NumericalFailure / numerical | Held a=0 normal member coefficient inclusion is below the practical margin | unclosed certificate: retain the closed-form normal search Ok,false after physical likelihood and residual-score checks |
| bhatt | NumericalFailure / numerical | Physical likelihood mismatch, wrong rounded normal winner, inconsistent uniform endpoint encoding, finite lower bound refutes search | refuted/representation failure: stop |
| bhatt | BudgetExhausted,Unresolved,Tie,NumericalFailure,IterationFailure / budget,unresolved,tie,numerical | Existing fallback fails or has no valid finite candidate; a=0 normal delegate fails | search failure: stop, retaining certificate diagnostics |
| GED | InvalidStart,InvalidShape / ordinary error | Invalid data/controls/holds or unavailable positive-variance fit | proved stop (invalid input): stop |
| GED | InvalidStart or sigma=0 / nomle | All data equal fitted/held location with scale free | proved nonexistence: stop |
| GED | UniformBoundary / limit:unif | Closed free-scale uniform comparison, including constant data away from a held mean | proved limit: stop |
| GED | Nonunique / tie | Globally selected Laplace median interval, held-Laplace median interval or exact reflected subunit knot winners with the central knot excluded | proved distinct maxima: stop |
| GED | Nonunique / tie | Overlapping distinct finite root bounds or finite/uniform bounds, without an exact-tie proof | unclosed certificate: try existing finite search and return Ok,false unless refuted |
| GED | UniformUnresolved / numerical | Held-sd uniform tail/endpoint comparison does not close | unclosed certificate: same fallback, finite shape only |
| GED | BfgsFailure with budget_exhausted / budget | Shape cover or subunit enumeration allowance expires | unclosed certificate: same fallback if the existing search independently converges |
| GED | BfgsFailure / numerical,tie | Unclosed root/score/inclusion comparison | unclosed certificate: same fallback |
| GED | TransformFailure / numerical | Conditional smooth-location point score enclosure excludes zero without a physical representation failure | unclosed certificate: retain the converged conditional search if physical location/scale scores pass |
| GED | TransformFailure / numerical | Normalization/rounding loses physical likelihood/location, support or positive residual information; unrepresentable shape constants or scale | search/representation failure: stop |
| GED | BfgsFailure / numerical,budget,tie | No converged finite search, score failure, artificial grid endpoint, nonfinite candidate, or a strictly higher feasible lower bound | search failure or refuted candidate: stop |
| burr / betapr | WeibullBoundary / ExpBoundary / InvExpBoundary from delegate | Delegate prefers the exponential face only within its likelihood margin; strict owned cover cannot close | unclosed certificate: retain successful finite search Ok,false; failed search still stops |
| bhatt | UniformLimit selected only by the legacy search | Outer certificate unclosed; search face preference within its likelihood margin | unclosed certificate: run the existing finite-only search, return Ok,false when physically consistent and unrefuted; otherwise stop |
| bhatt / GED | Non-Ok statistical/numerical status / ordinary error | Invalid controls/data or finite_stop_payload rejects the typed diagnostic (invalid witnesses or unrepresentable limit) | proved invalid-input stop or search/representation failure; the binding suppresses an unsafe typed payload |
| betapr | BfgsFailure / unresolved | Unit-shape scalar/generic searches fail or their finite point lies below the reachable exponential/inverse-exponential face beyond the margin; for example {1,1,1,1,5.99998}, unit shape, iterlim16 | search failure or refuted search: stop; face remains a witness, not a selected global limit |
| betapr | BfgsFailure / unresolved | Unit-shape data {u,u,u,u,6u}, u=1e-320, iterlim16; a finite reachable-face likelihood still refutes the finite search even if its rate coefficient overflows | refuted search: stop; retain the finite likelihood lower bound without encoding invalid face coefficients |
| all seven | No core status / ordinary error | Binding/shared-R validation rejects invalid supplied holds, starts or controls before invoking a fitter; Pareto IV additionally requires a held finite threshold and strict held support | proved invalid-input or unsupported fit: stop |
| burr / invburr / pareto4 / betapr / GED | Residual core result / ordinary error | Nonfinite recovered coefficients reach the common binding finite-result guard (defensive; normal core return paths classify these earlier) | search/representation failure: stop; not an unclosed-certificate decision |
| paralogis | Residual core result / numerical | Nonfinite shape, scale or likelihood reaches the binding finite-result guard before optimizer-status handling | search/representation failure: stop |
| paralogis | InvalidStart / ordinary error | With otherwise valid positive data and holds, the held-common-shape dispatcher maps a numerical physical-recovery Burr delegate failure to InvalidStart | search/representation failure: stop; existing ordinary-error mapping retained |

## Before/after study

The replay contains every seven-family input from the frozen 2026-11-09 fit-audit rerun (1,108 rows; the rerun uses the frozen `2026-11-07-fit-audit-data/cases.json.gz` inputs), 42 seeded/golden rows, and 39 explicit route regressions. Seed **16802** supplies three 25-observation samples per family: lognormal for the five positive families and normal for Bhatt/GED. Each family's frozen golden data are also fitted at `iterlim` 16, 64 and 1000. Default study controls are `tol=1e-8`, `iterlim=1000`; route rows record their overrides. The route set includes the changed rounded-tie/budget samples, near-constant fits, reciprocal/shifted held-scale and held-power cases, strict coefficient requests, the central-knot case, and refuted short-work/tiny-scale candidates.

This is a diagnostic stress suite, not an estimate of a population failure rate. The frozen [cases](2026-11-10-fallback-cont-data/cases.json), [route cases](2026-11-10-fallback-cont-data/route-cases.json), [provenance](2026-11-10-fallback-cont-data/provenance.json), [baseline header archive](2026-11-10-fallback-cont-data/before-headers.tar.gz), [before outcomes](2026-11-10-fallback-cont-data/before.tsv) and [after outcomes](2026-11-10-fallback-cont-data/after.tsv) are retained. The case reconstruction check reproduces all 1,189 inputs exactly.

| Outcome | Before | After |
|---|---:|---:|
| Certified | 796 | 796 |
| Uncertified | 13 | 79 |
| Stop: limit | 271 | 259 |
| Stop: tie | 33 | 14 |
| Stop: budget | 5 | 2 |
| Stop: numerical | 61 | 27 |
| Stop: unresolved | 7 | 10 |
| Stop: nonexistence | 3 | 2 |

Per-family cells are **before → after**.

| Family | Certified | Uncertified | Limit | Tie | Budget | Numerical | Unresolved | Nonexistence |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| burr | 141 → 141 | 0 → 7 | 49 → 48 | 6 → 1 | 0 → 0 | 5 → 5 | 1 → 1 | 1 → 0 |
| invburr | 150 → 150 | 0 → 4 | 44 → 43 | 6 → 4 | 0 → 0 | 1 → 0 | 1 → 1 | 0 → 0 |
| pareto4 | 126 → 126 | 0 → 11 | 44 → 43 | 11 → 3 | 1 → 0 | 4 → 3 | 0 → 0 | 0 → 0 |
| paralogis | 79 → 79 | 0 → 13 | 0 → 0 | 0 → 0 | 0 → 0 | 13 → 0 | 0 → 0 | 0 → 0 |
| betapr | 135 → 135 | 0 → 15 | 41 → 33 | 5 → 4 | 4 → 2 | 11 → 4 | 5 → 8 | 0 → 0 |
| bhatt | 77 → 77 | 13 → 15 | 60 → 59 | 0 → 0 | 0 → 0 | 2 → 1 | 0 → 0 | 0 → 0 |
| ged | 88 → 88 | 0 → 14 | 33 → 33 | 5 → 2 | 0 → 0 | 25 → 14 | 0 → 0 | 2 → 2 |

The replay has 70 outcome transitions: 66 stops become uncertified fits, one incorrect Burr constant-data/nonexistence classification becomes a numerical log-coalescence failure, and three unproved unit-shape limit stops become rejected/refuted-search stops. Those three are the forward/inverse `{1,1,1,1,5.99998}` samples at `iterlim=16` and the tiny-scale unit-shape case at `iterlim=16`. A face lower bound refutes the finite point; an unclosed face comparison does not select the limit globally.

All 809 successful baseline rows and 875 successful final rows pass finite-coefficient, finite-likelihood and parameter-domain checks. The 796 baseline certified rows remain certified with coefficient and likelihood output text unchanged; the 13 previously exposed Bhatt uncertified results remain uncertified. These are replay comparisons, not a new global correctness proof for the 79 uncertified results. Full [transitions and samples](2026-11-10-fallback-cont-data/transitions.json) and the [summary](2026-11-10-fallback-cont-data/summary.json) are retained.

## Changed old stop expectations

Every changed existing stop-expectation site is listed below, using its old title to identify the diff. Proved-stop assertions were retained. New route, refutation, representation and logical-flag cases were appended; no other existing test was changed. Bhatt's existing R tests are unchanged.

| Existing test site | Sample and controls | Change and reason |
|---|---|---|
| [C++/burr](../../../univariateML-cpp/include/cppunivariate/distributions/burr/test.cpp): burr: certified retained workloads and guarded stops | nearly-constant fixture (20 observations) | CertificateTie -> Ok,false. Numerical finite/face overlap is not a proved tie. |
| [C++/burr](../../../univariateML-cpp/include/cppunivariate/distributions/burr/test.cpp): burr: certificate arithmetic and failure gates | {.2,.5,1,2,4}; explicit certificate allowance 0 | CertificateBudget / NaN coefficients -> Ok,false. The independent proposal succeeds despite no proof allowance. |
| [C++/burr](../../../univariateML-cpp/include/cppunivariate/distributions/burr/test.cpp): Burr held-scale sign uncertainty returns a tie with a finite witness | exp({-1.1,-1}), scale=1 | CertificateTie / NaN coefficients -> Ok,false. The held-scale sign comparison does not close; the scalar search succeeds. |
| [C++/betapr](../../../univariateML-cpp/include/cppunivariate/distributions/betapr/test.cpp): betapr conditional nonunique crossing stops with both witnesses | route-betapr-rounded-crossing; held shape1=.22690393135372058 | BfgsFailure -> Ok,false. Rounded crossing data do not prove exact equality; witnesses remain. |
| [C++/betapr](../../../univariateML-cpp/include/cppunivariate/distributions/betapr/test.cpp): betapr flat gamma-adjacent conditional maximum retains golden coefficients | route-conditional-budget64; held shape1=.66110553857791232, maxit64 | BfgsFailure/Budget -> Ok,false; returned iterations <=64 rather than 128*64. The returned iteration count belongs to the converged search; exhausted proof diagnostics remain. |
| [C++/betapr](../../../univariateML-cpp/include/cppunivariate/distributions/betapr/test.cpp): betapr all-free budget stops retain witnesses and never bless a proposal | golden betapr data; tol1e-6, maxit16 | BfgsFailure/Budget -> Ok,false. A successful independent finite proposal remains available. |
| [C++/betapr](../../../univariateML-cpp/include/cppunivariate/distributions/betapr/test.cpp): betapr strict all-free coefficient requests cannot bless a wider enclosure | golden betapr data; tol1e-14, maxit1000 | BfgsFailure/Arithmetic -> Ok,false. Coefficient inclusion is unclosed; the flag makes no coefficient-accuracy claim. |
| [C++/ged](../../../univariateML-cpp/include/cppunivariate/distributions/ged/test.cpp): ged: separated finite shape maxima stop with competing root witnesses | 78-point rounded two-root sample, sd=1, mean=0 or free | Nonunique -> Ok,false. Overlapping root bounds are not an exact-tie proof; the witnesses remain. |
| [R/burr](../../../bindings/r/distributions/burr/test.R): Burr global certificate stops preserve their diagnostics | exp(seq(-1e-6,1e-6,length.out=20)) | unresolved error -> successful fit, certified FALSE. Only the near-constant assertion changes; proved stops and failed-search assertions remain. |
| [R/burr](../../../bindings/r/distributions/burr/test.R): burr nearly-constant exposes structured tie diagnostics | literal 20-point nearly-constant sample | univariateML_stop/tie -> successful fit, certified FALSE. Unclosed finite/face comparison; successful finite search. |
| [R/burr](../../../bindings/r/distributions/burr/test.R): held-scale Burr refuses sign-uncertain profiles with a tie witness | exp(c(-1.1,-1)), scale=1 | univariateML_tie -> successful fit, certified FALSE. Unresolved sign is not a proof that the search is wrong. |
| [R/invburr](../../../bindings/r/distributions/invburr/test.R): invburr preserves certificate refusal diagnostics | reciprocal exp(seq(-1e-6,1e-6,length.out=20)) | unresolved error -> successful fit, certified FALSE. Reciprocal transport of the successful Burr fallback. |
| [R/invburr](../../../bindings/r/distributions/invburr/test.R): invburr nearly-constant exposes structured tie diagnostics | reciprocal literal 20-point nearly-constant sample | univariateML_stop/tie -> successful fit, certified FALSE. The reciprocal comparison is unclosed, not an exact-tie proof. |
| [R/pareto4](../../../bindings/r/distributions/pareto4/test.R): pareto4 preserves certificate refusal diagnostics | exp(seq(-1e-6,1e-6,length.out=20)), min=0 | unresolved error -> successful fit, certified FALSE. Excess-data Burr fallback preserves the held threshold. |
| [R/betapr](../../../bindings/r/distributions/betapr/test.R): nonunique conditional beta-prime peaks retain both witnesses | route-betapr-rounded-crossing; held shape1=.22690393135372058 | univariateML_tie -> successful fit, certified FALSE. The binary64 crossing sample does not prove exact equality. |
| [R/betapr](../../../bindings/r/distributions/betapr/test.R): beta-prime certificate budget stops preserve witnesses | route-conditional-budget64; seed8104 stream, held shape1=.66110553857791232, iterlim64 | univariateML_budget -> successful fit, certified FALSE. Successful proposal survives exhausted certificate allowance; error-advice assertion removed. |
| [R/ged](../../../bindings/r/distributions/ged/test.R): ged separated finite shape maxima carry both root witnesses | 78-point rounded two-root sample, sd=1, mean=0 or free | univariateML_tie -> successful fit, certified FALSE. Numerical root overlap does not establish distinct equal maxima. |
| [JS/burr/invburr](../../../bindings/js/stop-test.mjs): Burr and reciprocal nearly-constant structured tie | literal 20-point nearly-constant sample and reciprocal | throw kind tie -> status Ok, certified0, finite logLik. The successful binding now returns a fit rather than a stop payload. |
| [JS/betapr](../../../bindings/js/test.mjs): betapr nonunique conditional peaks retain two witnesses on a tie | route-betapr-rounded-crossing; held shape1=.22690393135372058 | throw kind tie -> status Ok, certified0, finite logLik. Rounded near-equality is an unclosed certificate. |
| [JS/betapr](../../../bindings/js/test.mjs): betapr flat gamma-adjacent profile retains coefficient accuracy in WASM | existing flat-profile sample; held shape1=.66110553857791232, iterlim64 | throw kind budget -> status Ok, certified0, finite logLik. The independent converged search survives exhausted proof work; certified default-fit assertions stay. |
| [JS/ged](../../../bindings/js/test.mjs): GED separated finite shape roots stop with both witnesses | 78-point rounded two-root sample, sd=1, mean=0 or free | throw kind tie -> status Ok, certified0, finite logLik. No proof of an exact tie at the rounded sample. |

The complete machine-readable [21-site inventory](2026-11-10-fallback-cont-data/changed-stop-expectations.tsv) is retained. Additional JS checks cover the owned continuous-family flags and the large unit-shape conditional search. Appended C++/R cases cover joint/held-scale/held-power propagation, failed versus refuted fallback, tight normal/shape inclusion, held-sd uniform comparisons, held-shape point-score fallback with free or held scale, and central-knot exclusion. Existing failed-optimizer checks continue to stop.

## Final validation

Every local build/test ran in the foreground through the shared slot wrapper, with `nice -n 10` and BLAS/OpenMP pinned by that wrapper. All executed final suites used the **workstation**. Simbox was retried repeatedly, including after the local R full tier, and returned 75/unreachable before a test could execute. Source identity is retained in the [39 fitting-header hashes](2026-11-10-fallback-cont-data/source-sha256.json) and [67 validation-input hashes](2026-11-10-fallback-cont-data/validation-source-sha256.json); the latter includes adapters, tests and documentation. There was no hash drift in the final scope check. The machine-readable [validation results](2026-11-10-fallback-cont-data/validation-results.json) record counts and timings.

| Command/check | Machine | Final result | Evidence |
|---|---|---|---|
| `UMVL_TEST_TIER=dev just cpp-test` | workstation, GCC 15.2 | 105/106 shards; only `distribution_gb2` fails; all seven owned families pass. Test time 497.15 s. | [log](2026-11-10-fallback-cont-data/cpp-dev-workstation.log.gz) |
| `just cpp-test-no-long-double` | workstation, GCC 15.2 | 129/130 shards; same sole GB2 failure; all seven owned families pass. Test time 443.66 s. | [log](2026-11-10-fallback-cont-data/cpp-no-long-double-workstation.log.gz) |
| Final seven-family focused C++ run | workstation, normal and no-long-double | Each mode: 242 cases, 60,381 assertions passed; 9 slow/oracle cases excluded by the focused filter. | [normal](2026-11-10-fallback-cont-data/targeted-workstation.log.gz), [double-only](2026-11-10-fallback-cont-data/targeted-no-long-double-workstation.log.gz) |
| `UMVL_TEST_TIER=dev just test` | workstation | FAIL 0, WARN 0, SKIP 154, PASS 23,077; elapsed 320.617 s. | [log](2026-11-10-fallback-cont-data/r-dev-workstation.log.gz) |
| `just test-full` | workstation | FAIL 0, WARN 0, SKIP 0, PASS 45,904; elapsed 638.346 s. | [log](2026-11-10-fallback-cont-data/r-full-workstation.log.gz) |
| `just js-build` | workstation | Pass. | [log](2026-11-10-fallback-cont-data/js-build-workstation.log.gz) |
| `just js-test` | workstation | 597/597 pass, no skips; 370.362 s. | [log](2026-11-10-fallback-cont-data/js-test-workstation.log.gz) |
| `just site-check` | workstation | Pass: 99 family pages with build-time MathML, all 99 C++17 snippets compile/link, internal links/assets and browser checks pass. | [log](2026-11-10-fallback-cont-data/site-check-workstation.log.gz) |
| Case reconstruction, baseline/final replay and result checks | workstation, one slot | 1,189 inputs reproduced; 66 rescues; 796 certified numeric outputs unchanged; all successful rows finite/in-domain. | [summary](2026-11-10-fallback-cont-data/summary.json) |
| `simbox-run just cpp-test-no-long-double` | simbox | Exit 75: unreachable; required GCC 13 run remains open. | [last attempt](2026-11-10-fallback-cont-data/simbox-no-long-double.log) |
| Scoped Markdown links and `git diff --check` | workstation | Owned-document links pass; whitespace check passes. | [scoped checker](2026-11-10-fallback-cont-data/check_changed_links.py) |

The broad C++ runs used the final fitting headers. The last additions were test-only route assertions; the final 242-case runs cover those additions in both arithmetic modes, and both final R tiers include their R counterparts. `sged` and `genbeta` dependency shards also pass in both broad C++ runs. A scratch-launcher edit introduced a stray shell command after the dev command completed, so its saved dev status is 127; the completed CTest/`just` log records the actual dev exit 8 from the sole GB2 failure. The no-long-double command also completed with exit 8. The [runner note](2026-11-10-fallback-cont-data/cpp-runner-status.txt) preserves this distinction rather than treating the launcher status as a fitter failure.

The repository-wide Markdown scanner reports 44 broken destinations across 606 files/3,948 destinations, all outside this job's changed documents. They include scanner false positives on math in frozen audit tables and missing pre-existing local PDF links. No unrelated file was edited to silence them. The [global scan](2026-11-10-fallback-cont-data/markdown-links-repository.log.gz) is retained; the same scanner scoped to the nine changed documents passes all 40 destinations. The final scoped check including this report passes all 100 destinations across 10 files, with none broken. Site checking regenerated a historical screenshot as a known test side effect; its original HEAD bytes were restored.

## Open items

1. **Protected Pareto III density arithmetic.** `pareto3/dpqr.hpp` subtracts nearly equal absolute logarithms on tightly spaced data. For `{1000,1000.0000000000001,1000.0000000000002}`, held `min=0`, the unchanged certified fit has shape `17416525445767648`, scale `1000.0000000000001`, and stored log likelihood `73.223750676989127`; a 100-digit evaluation at those exact binary64 data and coefficients gives `85.60164977708311227`. The error is about `-12.377899100094`. Other certified grid cases also exhibit this problem. A centered-log-ratio density implementation and matching truth fixtures are needed. Those settled surfaces are protected by the task and were left unchanged. The [independent oracle](2026-11-10-fallback-cont-data/delegate_likelihood_oracle.py) and [all grid results](2026-11-10-fallback-cont-data/delegate-likelihood-oracle.json) record both certified and uncertified cases. The corrected uncertified fitter path passes this oracle; no claim of independently verified likelihoods is made for the unchanged certified grid cases.
2. **GCC 13 cross-machine coverage.** Simbox has a read-only filesystem and is unavailable for the cross-compiler run. All required follow-up suites, including no-long-double, pass locally under GCC 15.2 as requested. A GCC 13 no-long-double result requires a writable, available simbox.

3. **Artifact-pane preview configuration.** The supported `show` command returns HTTP 403 because the active worktree is outside the viewer's readable roots. Preview requires registering this directory in the viewer service; that configuration is outside the authorized worktree and remains untouched. The report is available at its requested path. The [current display diagnostic](2026-11-10-fallback-cont-data/delegate-artifact-display.txt) records the rejection.

## Reproduction

Run from this worktree, using the shared wrapper and worktree temporary directory as in the job instructions:

```sh
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
mkdir -p "$TMPDIR"
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-10-fallback-cont-data/build_cases.py --check
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-10-fallback-cont-data/study.py before
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-10-fallback-cont-data/study.py after
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-10-fallback-cont-data/analyze.py
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-10-fallback-cont-data/face_refutation_sweep.py
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-10-fallback-cont-data/strict_face_reference.py
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-10-fallback-cont-data/check_changed_links.py
```

The [study driver](2026-11-10-fallback-cont-data/study.py) compiles the frozen baseline or current headers and common bindings with GCC `-O2 -std=c++17`; Boost is found through the already-installed BH include directory. It performs no package installation. The [case builder](2026-11-10-fallback-cont-data/build_cases.py) leaves the frozen baseline archive untouched. The face sweep is additional numerical regression evidence and does not upgrade an uncertified result to certified.

## Delegate follow-up

The follow-up uses merged base `6d8a1f06`, including the accuracy jobs, and finishes on 2026-11-11 UTC. No commit was made. All requested delegate families were checked; **every required local suite passes**. The original seven-family evidence remains separate from the new `delegate-*` evidence.

### Delegate behavior and remaining stops

| Family | Parent / affected route | Result and stop classification |
|---|---|---|
| GB2 | Beta-prime with held power; Burr with unit `p` | Both transport the parent's `certified`. A successful uncertified beta-prime parent bypasses certificate-only scale enclosure and roundoff-width gates. Invalid unselected witness metadata is discarded without refusing a finite uncertified fit. Actual nonfinite scale/likelihood, failed parent search and proved parent stops still stop. The old numerical refusal was an unclosed certificate. |
| SGED | GED with `xi=1` held and free shape | `GedStatus::Ok` transports coefficients, likelihood, restrictions, diagnostics and the parent's flag. `Ok,false` no longer becomes `CertificateFailure` / numerical. Certified parent routes and parent stop mappings are unchanged. |
| InvParalogis | Reciprocal Paralogis: joint, held scale and held shape | The C++ result now carries and transports the flag on each handoff; common bindings emit it. Existing reciprocal representation failures, invalid inputs, proved nonexistence and failed parent searches remain stops. No successful false parent is relabeled certified. |
| Pareto III | Log-logistic excess fit, then coefficient correction | Failed correction (`IterationFailure`, ordinary binding error) or scale projection (`AccuracyFailure`) retains an accepted uncertified parent. A successful correction is retained only if its finite, stably evaluated physical likelihood is at least the parent's. The flag remains false. Certified correction routes are unchanged; a failed parent search still stops. |
| FPareto | Shifted GB2 | Its core already transports GB2's flag; the common binding now emits it. It inherits the successful rounded-power and unit-`p` fallbacks instead of the former GB2 numerical refusal. Actual failed/refuted searches and proved stops remain stops. |
| InvPareto | Reciprocal Lomax | The current parent is Lomax, whose successful routes are always certified and have no fallback flag. There is no false parent to drop. Existing successful transport, limit selection and failed-search behavior are retained and tested. |
| Pareto II | Shifted Lomax | The same always-certified Lomax handoff applies; successful fits and parent stops are unchanged and tested. |
| Trgamma | Burr helper inclusion, not a parent fit call | It includes Burr profile helpers but does not delegate a result to a changed fitter. No flag handoff or fallback refusal exists to change. Its independent successful fit and budget failure are retained and tested. |

GB2, SGED, InvParalogis, Pareto III and FPareto expose `extra {"certified",0/1}` through the shared conversion. No R adapter shim was added. The [expanded dependency probe](2026-11-10-fallback-cont-data/dependency_probe.cpp), [merged-base output](2026-11-10-fallback-cont-data/delegate-probe-before.tsv) and [final output](2026-11-10-fallback-cont-data/dependency-probe.tsv) cover all eight families. Probe flag `-1` means that the always-certified result type omits the field, not an uncertified fit.

The merged Lomax solver also exposed three inherited held-power regressions on `{1,1,1,1,6.0001}`, power 1, tolerance `1e-14`, in Burr, InvBurr and Pareto IV. It supplies a finite near-stationary point while reporting a coefficient-accuracy/iteration failure. The owned Burr fallback now accepts that point only when the total log-shape/log-rate score norm is below `1e-3`; positive finite coefficients, physical likelihood and refuting-face checks still apply, and the result is uncertified. An unchecked finite point fails the appended negative score-gate test. Lomax itself was not edited, and the three existing regression expectations were retained.

### Rounded crossing and regression coverage

Only one existing stop expectation changes in this follow-up: [GB2's held-power nonunique-profile test](../../../univariateML-cpp/include/cppunivariate/distributions/gb2/test.cpp). At `t=5.646438395028704`, its sample is the square root of `{1.9901008294052028e-5,2.209857117039297e-5,.0002450722434289249,12.288342947780086,942.976056905759,16.239512580216314,2.898690421966857,1.2684036923492177,400970.63293690106*exp(t)}`, with power 2 and `p=.22690393135372058`. `CertificateTie` becomes `Ok,false`. The parent's comparison records overlapping numerical enclosures/possible winners; it does not prove exact equality at this rounded crossing. Both separated witnesses and the neighboring samples' winner assertions remain. Proved-stop assertions are unchanged.

Appended C++ and R cases cover all eight requested delegates; R also checks the rounded GB2 crossing directly. C++ cases check transport identities, both InvParalogis search routes, retained Pareto III parents and six tightly spaced Pareto III grids. The appended JS test checks successful false flags for GB2, FPareto, SGED, InvParalogis and Pareto III. The final focused run passes **435 cases / 94,046 assertions**. GB2, SGED, InvParalogis, Pareto III and Burr dossier notes describe the family-specific behavior.

### Affected study replay

The [604 frozen cases](2026-11-10-fallback-cont-data/delegate-cases.json) contain 476 requested-family stress cases from the frozen audit, 80 symmetric replays of the GED-parent study (including seed 16802), 24 golden/budget cases, 21 explicit dependency probes and the three inherited held-power regressions. The [provenance](2026-11-10-fallback-cont-data/delegate-provenance.json) records the merged baseline archive and case hashes. The [driver](2026-11-10-fallback-cont-data/delegate_study.py) compiles the frozen merged-base headers or current headers; it leaves the original seven-family replay untouched.

| Outcome | Merged base | Final |
|---|---:|---:|
| Certified, including implicit flags | 394 | 376 |
| Uncertified | 3 | 40 |
| Stop: limit | 180 | 180 |
| Stop: numerical | 14 | 3 |
| Stop: tie | 4 | 2 |
| Stop: nonexistence | 1 | 1 |
| Stop: unresolved | 4 | 1 |
| Stop: ordinary/unclassified binding error | 4 | 1 |

**19 stops become uncertified successes:** 11 SGED, three Pareto III, one GB2, one FPareto and the three inherited held-power regressions. **18 lost flags are corrected:** 13 InvParalogis, three SGED, one GB2 and one FPareto. All **376 retained-certified numeric outputs are text-identical**. All 416 final successful rows have finite in-domain coefficients and finite likelihoods. Residual stop kinds alone are not proofs of nonuniqueness; failed parent searches can retain certificate diagnostics. The remaining ordinary Pareto III stop is the unsuccessful golden search at `iterlim=16`.

Two previously uncertified Pareto III grid fits retain their coefficients but correct their stored likelihoods: center 1000 / four adjacent doubles changes `95.176374677150307` to `112.56899816357473`; center `1e100` / four adjacent doubles changes `-780.36234786664204` to `-782.97711110994487`. A 100-digit physical evaluation confirms the corrected likelihoods. Comparisons use that independent baseline likelihood, rather than the inaccurate old stored value. Every final uncertified grid likelihood agrees within `1e-10`; this is numerical likelihood evidence, not a global certificate. The unchanged certified density issue is specified under Open items.

The [summary](2026-11-10-fallback-cont-data/delegate-summary.json), [full transitions and samples](2026-11-10-fallback-cont-data/delegate-transitions.json), [numeric changes](2026-11-10-fallback-cont-data/delegate-numeric-changes.json), [before](2026-11-10-fallback-cont-data/delegate-before.tsv) and [after](2026-11-10-fallback-cont-data/delegate-after.tsv) outcomes, and [comparison checker](2026-11-10-fallback-cont-data/analyze_delegates.py) are retained. No certified golden coefficients or controls were changed.

### Final local validation

All commands ran in the foreground on the **workstation**, through the shared slot wrappers, under `nice -n 10` with BLAS/OpenMP pinned. Simbox was unavailable with a read-only filesystem; the revised instruction requests local runs. The [validation manifest](2026-11-10-fallback-cont-data/delegate-validation-results.json) records the results. All [131 source hashes](2026-11-10-fallback-cont-data/delegate-source-sha256.json) match. Numerical sources were unchanged throughout the final C++/R/JS/site runs.

| Required command | Final result | Log |
|---|---|---|
| `UMVL_TEST_TIER=dev just cpp-test` | 106/106; 492.78 s | [C++ dev](2026-11-10-fallback-cont-data/delegate-cpp-dev-workstation.log.gz) |
| `UMVL_TEST_TIER=full just cpp-test` | 133/133; 502.08 s | [C++ full](2026-11-10-fallback-cont-data/delegate-cpp-full-workstation.log.gz) |
| `just cpp-test-no-long-double` | 133/133; 532.28 s | [double-only](2026-11-10-fallback-cont-data/delegate-cpp-no-long-double-workstation.log.gz) |
| `UMVL_TEST_TIER=dev just test` | FAIL 0, WARN 0, SKIP 154, PASS 23,211; 306.612 s | [R dev](2026-11-10-fallback-cont-data/delegate-r-dev-workstation.log.gz) |
| `just test-full` | FAIL 0, WARN 0, SKIP 0, PASS 46,038; 849.742 s | [R full](2026-11-10-fallback-cont-data/delegate-r-full-workstation.log.gz) |
| `just js-build` | Pass | [JS build](2026-11-10-fallback-cont-data/delegate-js-build-workstation.log.gz) |
| `just js-test` | 610/610; no failures/skips; 299.900 s | [JS tests](2026-11-10-fallback-cont-data/delegate-js-test-workstation.log.gz) |
| `just site-check` | Pass: 99 pages, MathML, 99 compiling C++17 snippets, graph/links/browser checks | [Site](2026-11-10-fallback-cont-data/delegate-site-check-workstation.log.gz) |

The [15-family focused log](2026-11-10-fallback-cont-data/delegate-targeted-workstation.log.gz) and [study/oracle log](2026-11-10-fallback-cont-data/delegate-study-workstation.log.gz) are retained too. The scoped Markdown and whitespace checks pass. The site's historical screenshot side effect was restored to HEAD bytes. No protected d/p/q/r, truth, special-function or code-generation source was changed.

To reproduce the new replay after setting the worktree temporary-directory environment:

```sh
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-10-fallback-cont-data/delegate_study.py before
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-10-fallback-cont-data/delegate_study.py after
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-10-fallback-cont-data/delegate_likelihood_oracle.py
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-10-fallback-cont-data/analyze_delegates.py
```
