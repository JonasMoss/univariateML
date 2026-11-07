# Tukey lambda certificates — TASK-116.27

The implementation certifies all scalar free subsets and all nuisance fits at held nonnegative shape, or returns a structured limit, tie, budget or numerical stop. Exact negative-shape collapse decisions and unresolved equality guards are implemented. General multidimensional free-shape fits and negative-shape joint location-scale fits remain uncertified; the family flag is therefore `partial`. Ordinary default estimates and frozen fixtures are preserved.

Source base: `017b9f8ec3bed7610ccd41c62738bce1ea987316`, branch `codex/tlambda-impl`. No commits or board edits were made. The final tested source hashes, environments, machine results and reproducible drivers are retained in [the evidence directory](2026-11-07-tlambda-impl-data/README.md) and [validation.json](2026-11-07-tlambda-impl-data/validation.json).

## Implemented scope

“Certified” below means a conditional global likelihood certificate within the requested tolerance, with numerical failure or a budget stop when the comparison cannot be established. It does not imply unique coefficients. All-held evaluation is not a fitting certificate.

| Free coefficients | Held regime | Result |
|---|---|---|
| none | any valid finite shape | evaluation |
| mu | lambda >= 0 | concavity, uniform plateau, support endpoints or scalar boxes |
| mu | lambda < 0 | scalar boxes, including multiple modes |
| sigma | lambda >= 0 | concavity or exact minimum-support scale |
| sigma | lambda < 0 | exact collapse decisions or scalar boxes |
| lambda | mu and sigma held | scalar boxes, incumbent-derived negative cutoff and uniform comparison |
| mu, sigma | lambda >= 0 | concavity below 1; exact uniforms at 1 and 2; scalar contact curve above 1 |
| mu, sigma | lambda < 0 | exact collapse decisions where proved; unresolved equalities stop numerically; other searches remain uncertified |
| mu, lambda | sigma held | legacy search remains uncertified |
| sigma, lambda | mu held | collapse is unbounded if an observation equals held mu; remaining search uncertified |
| mu, sigma, lambda | fitted branch | legacy search remains uncertified; half ties stop numerically with collapse and feasible uniform witnesses |

The implementation lives in [certificate.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/tlambda/certificate.hpp), included by `mle.hpp`. Routing and error classification live in the binding-neutral `bindings/common/distributions/tlambda/fit.hpp`, so R and JavaScript use the same decisions. No shared core header needed modification. New family C++ and R tests and shared JavaScript stop tests are appended. The dossier records the scope and numerical contract. The settled d/p/q/r, special functions, truth files, goldens, generated d/p/q/r and protected project files were not edited.

## Mathematical basis and numerical contract

The [theory report](2026-11-07-tlambda-theory-report.md), [proofs](2026-11-07-tlambda-theory-data/proofs.md) and [subset inventory](2026-11-07-tlambda-theory-data/subsets.md) supply the concavity, inverse quantile and collapse arguments. At fixed 0 <= lambda < 1, strict concavity in A=1/sigma and B=-mu/sigma permits a tangent upper bound over an explicit compact superlevel region. Newton supplies proposals; acceptance requires the global value gap and a relative inverse-scale/standardized-location Newton step below 1e-9. A held location imposes an affine constraint.

At 1 and 2, exact uniform support geometry replaces optimization. At 1 < lambda < 2, convexity reduces a held-scale location fit to its two support contacts. With scale free, monotonicity reduces to minimum support scale, leaving a scalar contact curve when location is free.

The implementation extends that scale reduction to lambda > 2. Put u+v=1 and D=u^(lambda-1)+v^(lambda-1). Expanding gives

```
D² - (u^lambda-v^lambda)(u^(lambda-2)-v^(lambda-2))
 = 2u^(lambda-1)v^(lambda-1) + u^lambda v^(lambda-2)
   + v^lambda u^(lambda-2) > 0.
```

Hence Q D'/D² < (lambda-1)/lambda < 1, and each log density has strictly negative log-scale derivative. The smallest feasible scale is optimal. The modal density envelope bounds the contact-curve location interval. This is an algebraic proof; the additional 20 numerical identity checks are regression evidence.

For lambda=-a<0, the envelope min(M/sigma, C sigma^(1/a)/|x-mu|^(1+1/a)), M=2^(-a-2), C=a^(-1-1/a), supplies finite scalar superlevel bounds. Inverse-quantile enclosures bound density on boxes. Shape-only fitting uses an incumbent-derived cutoff from the modal envelope rather than a fixed negative cutoff. No scalar search assumes unimodality. The likelihood certificate bounds the objective gap by n*reltol subject to the arithmetic floor; it is not a general coefficient confidence interval.

At a location occupied k times, the collapse exponent is (n-k)/a-k. Its negative sign proves unboundedness. With location held, equality is an unattained supremum. For free location at held lambda=-1, the half-tie pairing inequality proves domination by the collapse face. Other unresolved equality configurations return a numerical stop with a face witness. A free-shape half tie also retains a feasible uniform witness and stops numerically; fixed-shape nonattainment alone does not decide the free-shape supremum.

Certificates follow the package standard: exact arithmetic conditional on computed comparisons, with practical 512*epsilon*(1+|v|) padding. They are not outward-rounded interval proofs. Inverse roots retain analytic brackets in -log(p), contracting both root width and log-density width. Compensated likelihood/score sums and padding by uncancelled normalization/density magnitudes protect cancellation; inverse-bracket score uncertainty enters the tangent bound. Box geometry padding propagates through inversion, including the non-Lipschitz density behavior near positive-shape endpoints.

Fit feasibility uses the accurate affine support residual rather than rounded d/p/q/r endpoint aliases. Uniform witnesses are checked against the ideal span/radius optimum: adjacent binary64 observations can have an unrepresentable midpoint and require a wider represented interval. Such a material gap stops numerically. A separate regression with 1024 zeros, lambda=-998 and sigma=2^-1000 checks cancellation where the exact total log likelihood is zero.

Every solver/inverse loop uses min(iterlim,100000) iterations; certificate work uses 128*min(iterlim,100000) evaluations. Exhausting the latter returns budget witnesses. Solver non-convergence, invalid enclosures and undecidable comparisons return numerical stops. Failed local searches do not acquire certificates through their unchecked approximate likelihoods.

## Non-uniqueness

The theory and appended tests retain three counterexamples:

* At lambda=1, sigma=1 and data (-1/4,1/4), every mu in [-3/4,3/4] maximizes likelihood.
* At lambda=3/2, sigma=1 and the same data, the two contacts mu=+/-5/12 are equal global maxima; the fitter returns a structured two-candidate tie.
* At lambda=-1, sigma=1 and data +/-80/9, the midpoint is a strict local minimum and symmetry gives at least two global maxima. Scalar boxes certify the value without claiming unique coordinates.

## Validation

All final commands completed successfully against the final numerical source manifest. The ordinary C++ clean rebuild ran locally after simbox mirror exit 75; no-long-double and R used one serial mirror on simbox. Concurrent mirrors had earlier collided on a temporary site build file, so the final remote workload was serialized. No command remains running.

| Final check | Machine | Result |
|---|---|---|
| clean C++ full tier; C++ dev tier | workstation fallback | 129/129; 106/106 |
| clean C++ no-long-double full tier | simbox | 129/129 |
| `just test` | simbox | PASS 21025, FAIL 0, WARN 0, SKIP 154 |
| `just test-full` | simbox | PASS 43849, FAIL 0, WARN 0, SKIP 0 |
| focused tlambda doctest | workstation | 21 cases, 3832 assertions pass |
| `just js-build`; `just js-test` | workstation | build green; 440/440, no skips |
| `just site-check` | workstation | green; 99 MathML family pages and 99 C++17 snippets |
| independent comparisons and explicit golden check | workstation | green |

The focused binary deliberately excludes the slow held-subset golden case; the full suite and explicit golden driver cover it. R dev skips are tier-selected coverage, not failures; the full tier has no skips. Compiler/CMake notices are retained separately from R warnings (zero). Workstation: GCC 15.2, R 4.6.1, Node 22.22.1, Python 3.14.5, mpmath 1.3.0. Simbox: GCC 13.3, R 4.6.1, WSL2. Computation used the prescribed slot locks, nice +10 and thread limits. Source hashes were compared before and after the final native, C++ and R runs; the final JS source hashes also match.

The independent checker uses 100-digit mpmath: 57 inverse checks, 16 fit checks, six scalar certificates, 126 grid comparisons and 20 positive-shape identity checks pass. Maximum free score magnitude is 6.09e-10 and log-likelihood discrepancy 3.64e-15. The largest normalized inverse sign deficit is 3.08e-14, within the practical padding; these are not strict directed-rounding enclosures. Independent grid comparisons support numerical behavior, not the globality proof.

Against the unchanged n=500 fixture, masks 0,3,4,5,6,7 pass coefficient tolerance 1e-6*(1+abs(coef)) and absolute likelihood tolerance 1e-7. Maximum certified-subset coefficient drift is 6.75e-9; maximum overall coefficient drift is 4.14e-8 and likelihood drift 6.83e-13. Controls were reltol=1e-8, iterlim=2000. The fixture's seeded data are reused, not regenerated. Full source/fixture identities and results are in [golden-check.json](2026-11-07-tlambda-impl-data/golden-check.json).

Development checks exposed and resolved small-shape inversion cancellation, insufficient root contraction near the mode, unsigned sample-size negation in a proposal, infeasible uniform proposals being misclassified, normalization cancellation and represented support gaps. Existing error-message compatibility was preserved while adding structured stops. Finite grids, local convergence and unchecked capped searches were rejected as certification criteria.

## Timings and open work

The deterministic timing driver uses tlambda(0,1,.14) quantiles (i+.5)/n, excluding generation, with C++17/-O2, reltol=1e-7 and iterlim=1000 on the workstation. Mask bits denote held mu=1, sigma=2, lambda=4.

| n | held mask | before seconds | after seconds |
|---|---:|---:|---:|
| 101 | 0 | 0.40019 | 0.38341 |
| 101 | 4 | 0.01456 | 0.00279 |
| 101 | 5 | 0.00246 | 0.00270 |
| 101 | 6 | 0.00565 | 0.00049 |
| 100000 | 0 | 477.75352 | 392.05704 |

Default coefficients and likelihood are bit-identical between these runs. Its search mathematics is unchanged, so the elapsed variation is not evidence of an algorithmic speedup; the retained pilot took 522.995 seconds. Free fitting remains slow. In the separate n=500 golden run, shape-only certification took 115.406 seconds and 113161 evaluations; held-shape joint and scalar fits took about 0.0066–0.0112 seconds.

The following entries remain open:

1. **Multidimensional global bounds.** Free shape with a free nuisance and negative held shape with both nuisances free retain uncertified searches. A real box-envelope experiment at n=31 exhausted 127999 evaluations with 52094 unresolved boxes: free-shape global gap 0.645258 versus requested 3.1e-6. The negative fixed-shape joint experiment left 51548 boxes and gap 0.0172914 at the same budget. [Probe outputs](2026-11-07-tlambda-impl-data/box-probe.json) and [negative-shape output](2026-11-07-tlambda-impl-data/negative-box-probe.json) retain the evidence. These experiments do not prove impossibility; they show why the basic box kernel cannot replace frozen ordinary fits at the default budget. Sharper correlated bounds, profile bounds or certified curvature are needed. No capped estimate was labeled certified.
2. **General free-location collapse equality.** Held location and the proved held lambda=-1 half-tie case are settled. Other equality cases need a global face comparison; they currently stop numerically with the missing comparison explicit, rather than declaring a false non-maximizer or unattained supremum.
3. **Scalability.** The n=500 shape-only cost needs stronger bounds before large-sample use can be considered efficient. No high-n shape-only scalability claim is made. The default n=100000 census remains several minutes per fit.

These gaps prevent a `yes` certification flag. The shipped scope is stated in the dossier and result flags, and the evidence distinguishes proofs, regression passes, numerical comparisons, tier skips and missing coverage. No inference or asymptotic calibration claim is added.
