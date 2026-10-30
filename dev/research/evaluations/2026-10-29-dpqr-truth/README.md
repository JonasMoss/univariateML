# Independent d/p/q/r truth tables

Generated: 94/98 families, 544 parameter points, 30778 x rows and 23936 quantile rows.

`python -m quality truth [ids]` and `just dpqr-truth [ids]` generate tables. Run either command through `/tmp/umvl-cpu/run`, as for other repository computations. It returns nonzero when any requested family is a gap. No C++, R, stats, extraDistr, actuar or scipy probability functions are used as truth.

Parameter rule: sampler arguments and fitted estimates, then one coordinate at a time. Distances to a single finite domain bound are multiplied by 0.1 and 10; bounded intervals move 90% towards either edge; unrestricted coordinates are multiplied by 0.1 and 10 (zero moves to -1 and 1). Integer, design, fixed and endpoint parameters stay at fitted golden values. Points are deduplicated and capped at eight. Family-local special points take priority: GEV and GPD xi, and Tukey lambda lambda, use -5e-10, 0, +5e-10. Triangular endpoints and the Frechet lower endpoint are held by local routes. Local routes also mark the two negative-binomial size parameters mislabeled trials as continuous. Later coordinates can receive only sampler/fit coverage when the eight-point cap is reached.

The shared parser preserves the quality harness's existing spec.py preference. Truth normally uses the exact README expression because several existing specs are approximations valid only at the fitted golden. Every exception and support correction lives beside its family in truth.py; README front matter is untouched.

Numerics: at least 80 decimal digits (up to 600 for small shapes). Continuous normalization integrates the declarative density in a logarithmic support chart, split at independent quantiles and local kinks, with tail mass 1e-32 omitted at each end; the integral must differ from 1 by at most 1e-25 and the quadrature error estimate must be at most 1e-28. Antiderivative derivatives are checked against the density. Family-local beta and latent-normal changes of variable avoid endpoint cancellation; their omitted masses are bounded explicitly. Tukey lambda integrates its implicit density in quantile coordinates. Positive discrete sums use finite sums, telescoping identities, exact special functions or a geometric remainder below 1e-45. At enormous integer x, explicit lower/upper series bounds instead certify relative error below 1e-35 on the stored log value. Tables are written atomically only after every point passes verification.

Both tails are evaluated through the smaller tail, using expm1 for complements. Roots are bracketed in a logarithmic support coordinate; discrete roots use integer bisection. Natural and logarithmic inputs are rounded to binary64 BEFORE computing their answers. Quantile rows store the rounded true answer and independent adjacent-double brackets. Overflow requires signed infinity; underflow rounds to zero or the finite support endpoint. Endpoint densities are one-sided limits of the declared formula. JSON numbers round-trip binary64 (at most 17 significant digits); nonfinite values are strings.

Grid: both-tail quantiles at 1e-300, 1e-100, 1e-30, 1e-12, 1e-6, .01, .25, .5; support edges and adjacent doubles; 1e100 and 1e300 in support. Discrete tables add up to 200 body integers and a noninteger. Quantile inputs additionally include log p=-800 and -10000 and p=0,1.

C++ checks use 1e-10 relative error on log values (absolute when magnitude <=1), 1e-10 relative on natural probabilities >1e-300, and 1e-9 relative on quantiles or the independently stored double bracket. Each parameter point normally uses 20000 StdRng draws with seed 20261029+point; body CDF checks use five binomial standard errors plus 1/n. The standalone audit records failures even when doctest permits the case to fail.

`summary.csv` contains all observed failures, worst parameter-point index and input, and ours/truth values. Probe columns retain its counts for comparison only: its grid and 1e-6 tolerance differ, and its README documents false positives. Oracle gaps and unaudited families are explicit rows, never zero failures.

Observed failing families: beta, betapr, bhatt, binom, burr, cauchy, dgamma, dnorm, dunif, exp, fatigue, gamma, gb2, ged, geom, gev, gompertz, gpd, gumbel, hcauchy, hnorm, ht, huber, invburr, invexp, invgamma, invgauss, invparalogis, invpareto, invtrgamma, invweibull, kumar, laplace, lgamma, lgser, llogis, lnorm, logis, lomax, lst, nbinom, norm, paralogis, pareto, pois, power, rayleigh, slash, std, tlambda, tnorm, trgamma, triang, unif, weibull, zinb, zip, zmnbinom, zmpois, ztbinom, ztnbinom, ztpois.

Cases decorated may_fail: beta, betapr, bhatt, binom, burr, cauchy, dgamma, dnorm, dunif, exp, fatigue, gamma, ged, geom, gev, gompertz, gpd, gumbel, hcauchy, hnorm, ht, huber, invburr, invexp, invgamma, invgauss, invparalogis, invpareto, invtrgamma, invweibull, kumar, laplace, lgamma, lgser, llogis, lnorm, logis, lomax, lst, nbinom, norm, paralogis, pareto, pois, power, rayleigh, slash, std, tlambda, tnorm, trgamma, triang, unif, weibull, zinb, zip, zmnbinom, zmpois, ztbinom, ztnbinom, ztpois.

Density-only APIs (tests untouched as requested): fpareto, frechet, gb2, genbeta, nsbeta, pareto2, pareto3, pareto4, pig.

Sampler checks: 490 parameter points, 9,686,000 draws; 0 failing empirical CDF or validity checks. Total sampler time 13.453 seconds. Per-family counts and timings are in sampling.csv. Reduced sampling: cmp: 1000 draws/point, 7.368 seconds. The reason is documented in the corresponding family test.cpp.

Probe comparison: 37 failing families agree with the earlier probe; 25 additional failing families appear on the larger grid and tighter tolerances. Audited probe failures now passing: none. The half-Cauchy oracle uses its direct lower atan tail, avoiding the extreme-lower false positives from recovering it by subtraction.

Support metadata differs from the actual density support in 27 families at the first point: bbinom, binom, cmb, dunif, fpareto, frechet, genbeta, gev, gpd, hnorm, hyper, nhyper, nsbeta, pareto, pareto2, pareto3, pareto4, power, tbinom, tlambda, tpois, triang, unif, zib, zipf, zmbinom, ztbinom. Full first-point bounds are in support-corrections.json; no front matter was edited.

## Gaps

- bnbinom: ValueError: gap: 3F2(1) tail needs a uniform remainder bound across alpha=0.2 and enormous integer targets; generic finite summation is not certified.
- pig: ValueError: gap: Bessel-K count tail needs a uniform series remainder beyond the golden-only polynomial spec.
- skellam: ValueError: gap: two-sided Bessel-I series needs certified tail bounds at extreme quantiles; the golden-only spec cannot certify this grid.
- sn: ValueError: gap: skew-normal smaller tail needs cancellation-free two-dimensional Gaussian integration with a validated error estimate.

## Validation

Recorded 2026-10-29T17:43:57.251375+01:00, base fab96c1: 5/5 ctest targets passed (68.54s). Distribution tests: 468 passed, 3 skipped oracle gaps; 61 cases permit known failures.
R: FAIL 0 | WARN 0 | SKIP 1 | PASS 8402; the skip is the optional sn package. Python: 19 tests passed, including five closed-form validations to 1e-15 relative error and extreme-count/half-tail regression checks. Original/current quality results for norm and zip are identical.
Standalone C++ audit: 95 cases, 445914 assertions; 9795 numerical mismatches and 2 unexpected Boost exception assertions. These feed the fix queue; ctest permits only the recorded failing family cases. Worst normalization residual: 2e-32; no NaN cells. Details: validation.json.
