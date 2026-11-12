# Fitter logLik accuracy across families — TASK-150.2.7

Job dated 2026-11-11; final checks finished 2026-11-12 (Europe/Oslo).
Worktree: seam-check, branch codex/seam-check. Baseline: 3b2a16fed4787c71b96bf23b1cb86569973b0a9c.

**All 20 owned fitter-side returned-point logLik errors are fixed, across ten families.**
Fresh native refits cover 11,185 archived datasets and 99 goldens. Of 8,757 returned fits,
none puts an observation outside its exact support. Three fitter errors owned by other
jobs and ten protected density errors remain. Six fits have explicit guarded-open
entries below. An additional double-only inverse-Gaussian maximum error is fixed.

The required C++ suites and full R suite passed. After family-only late fixes, the
affected native, R and JS checks passed; full suites were not repeated. No commits,
golden rewrites, dependency installations, board edits or protected probability-code
changes were made.

## Census and method

The input archive is the existing
[frozen fit-audit corpus](2026-11-07-fit-audit-data/cases.json.gz), together with each
current family's golden sample. Every dataset was refitted with the current
binding-neutral adapter and its existing controls and holds; archived estimates
were not reused. Gengamma is probability-only at this revision and has no fitter
or golden, so the fitter census contains 99 families.

The reusable [checker](../../validation/seam-check.py) compares reported logLik with
a compensated sum of the settled scalar log densities at the returned public
coefficients. The threshold is 1e-10 times max(1, abs(logLik)). Each disagreement
is attributed using an [independent density reference](../../validation/seam_reference.py)
at exact binary64 data and coefficients: at least 100 digits, raised for large
parameters up to 1,600 digits, and repeated with another 50 digits.

All 32 original disagreements and all 13 retained disagreements have resolved
independent values. After fixes, an existing independent value was reused only
when the data and coefficient bit patterns were identical; the changed beta and
nonstandard-beta points were recomputed at 200 and 250 digits. See
[reference-reconciliation.json](2026-11-11-seam-check-data/reference-reconciliation.json).
Support bounds are compared using exact rational arithmetic on binary64 inputs,
including transformed endpoints and integer truncation.

| Outcome | Before | After |
|---|---:|---:|
| Reported value agrees with summed density | 8725 | 8744 |
| Disagreement requiring attribution | 32 | 13 |
| Native stop | 2514 | 2521 |
| Guarded fit without a terminal result | 13 | 6 |
| Fitter-side errors, all owners | 23 | 3 |
| Fitter-side errors owned by this job | 20 | 0 |
| Density-side errors | 10 | 10 |
| Returned fits outside support | 0 / 8757 | 0 / 8757 |

One beta-binomial case originally had both a fitter and a density error. Its
fitter value is now correct, but its protected density remains wrong; this is why
20 fitter repairs remove 19 raw disagreements. Every previously returned fit still
returns a fit, and every previously terminal native stop is preserved. Seven
previous guard expirations yielded native stops in the longer retry, rather than
being converted into stops by this checker.

This is a likelihood-value and support audit. It does not establish coefficient
accuracy or global search certification for every returned fit. In particular,
correcting a final likelihood does not independently certify every comparison
inside an existing discrete search.

## Changes and per-fixed-family evidence

The following errors are relative to the independent density at the public
point, not relative to the continuous likelihood maximum.

| Family | Errors before → after | Worst error before | Worst error after | Correction |
|---|---:|---:|---:|---|
| bbinom | 1 → 0 | 6.72e-10 | 1.72e-16 | Gamma-ratio identity for large counts and small held shapes; compensated rising-factorial sums; tiny-shape endpoint regression. |
| beta | 1 → 0 | 1.57e-10 | 0 | Accurate public-shape density sum, including held fits; adjacent shape representations when the internal continuous value misses the public-point target. |
| binom | 1 → 0 | 2.09e-10 | 0 | Accurate public PMF for the held-probability result; ordinary free fits retained. |
| invgauss | 2 → 0 | 1.93e-10 | 1.15e-16 | Evaluate the public density sum. Double-only polish uses the centered continuous mean, continuous MLE shape, and analytic maximum gap. |
| nbinom | 1 → 0 | 1.26e-10 | 0 | Accurate held-probability and supplied-candidate likelihood, including final certificate evaluation. |
| nsbeta | 1 → 0 | 1.57e-10 | 0 | Inherited beta correction; binding and native delegate regressions. |
| tnorm | 9 → 0 | 1.22e-07 | 5.75e-16 | Re-evaluate the returned coefficients and actual estimated bounds with the accurate truncated density. |
| tpois | 1 → 0 | 4.34e-10 | 0 | Re-evaluate the final rate and actual estimated truncation bounds. |
| zmbinom | 2 → 0 | 4.74e-08 | 2.35e-15 | Inherited zero-truncated-binomial correction; binding and native delegate regressions. |
| ztbinom | 1 → 0 | 4.74e-08 | 2.35e-15 | Accurate normalized binomial PMF at the final public coefficients. |


C++ regressions were appended for all ten families, and R regressions cover
the binding paths. Existing test files remain exact prefixes of their edited
versions. No existing expectation was weakened. The large-count beta-binomial R
test leaves size free so that it exercises the fitter rather than the all-held
density shortcut.

All 99 native golden coefficient vectors remain bitwise unchanged. Six golden
refit likelihood payloads change only in roundoff, by at most 2.27e-12 in
absolute value; fixture files are unchanged. The only native census coefficient
changes are beta case 100 and its nonstandard-beta delegate, case 10786: shape2
moves up one ulp. The additional double-only inverse-Gaussian replay changes
the two concentrated duplicates to restore the maximum; its golden is unchanged.

### Maximum-value checks and mathematical scope

For beta case 100, merely recomputing the density at the old public shapes would
lose 1.6031e-10 relative to the maximum. The smooth maximum has no cusp allowance.
The neighboring shape2 representation reduces that loss to 4.8285e-12.
The independently evaluated maximum is 135.27888712924001718478; the final
public-point value is 135.27888712858683. Both digamma scores were solved at
180 and 240 digits with an analytic polygamma Jacobian; objective values agree
to 100 digits. Strict concavity gives uniqueness of an exact stationary point;
the root and values here are numerical evidence, not outward-rounded intervals.
See [beta-maximized-value-final.json](2026-11-11-seam-check-data/beta-maximized-value-final.json) and
[beta-max-check.py](2026-11-11-seam-check-data/beta-max-check.py). The
[beta dossier](../../../univariateML-cpp/include/cppunivariate/distributions/beta/README.md)
records the mean-ratio sensitivity.

For inverse Gaussian, Q(m)=sum((x-m)^2/(m^2*x)) has derivative
2*(n*m-sum(x))/m^3. Thus the exact mean is its unique minimum, and the
continuous MLE shape is n/Q(mean). The exact likelihood gap can be written as
(n/2)*(u-1-log(u)+log(Q(public_mean)/Q(exact_mean))),
where u=public_shape*Q(public_mean)/n. The double-only correction evaluates
this gap with centered residuals and log1p terms, avoiding subtraction of close
Q values. It compares neighboring public means at the rounded continuous MLE
shape. A computed gap exceeding the likelihood target triggers replacement;
unresolved calculations preserve the existing result.

In cases 3835 and 3838 the old double-only public mean was one ulp away from
the nearest mean representation. Its reported value was 1.4575e-10 below the
true maximum, even after density-sum agreement had been restored. The final
relative maximum-value error is 9.05e-12;
the final double-only shape error is 7.284e-17.
The exact-input continuous maximum is 123.89205063011402599215.
The native extended-precision likelihood has relative maximum error
9.05e-12. Independent 100/160-digit closed-form
checks, adjacent-representation checks and the actual WASM result are retained in
[invgauss-maximized-value.json](2026-11-11-seam-check-data/invgauss-maximized-value.json), [invgauss-maximized-value-before-polish.json](2026-11-11-seam-check-data/invgauss-maximized-value-before-polish.json),
[invgauss-max-check.py](2026-11-11-seam-check-data/invgauss-max-check.py) and [invgauss-wasm.json](2026-11-11-seam-check-data/invgauss-wasm.json).
The [inverse-Gaussian dossier](../../../univariateML-cpp/include/cppunivariate/distributions/invgauss/README.md)
records its leading mean-rounding loss.

These are exact-arithmetic likelihood identities conditional on the computed
comparisons, with the practical floating-point target 1e-10. The high-precision
checks establish margins for these regressions; they are not interval proofs
covering all future inputs. No new non-Lipschitz allowance was needed for an
owned family.

## Retained issues and ownership

### Fitter likelihood errors owned elsewhere

| Family | Archive indices | Holds | Relative fitter error |
|---|---|---|---:|
| burr | 636, 639 | shape1=1.334637717604056 | 1.97645e-7 |
| genbeta | 2568 | shape3=4.084996007320842, scale=1 | 1.45156e-10 |

Their independent density values agree with the settled probability layer.
No source file owned by those jobs was edited. Complete ownership exclusions:

- sged-acc: sged, sstd, snorm.
- acc-fb-cont: burr, invburr, pareto4, paralogis, invparalogis, betapr, gb2,
  fpareto, pareto3, ged, genbeta, bhatt, sn.
- ztpois-pmf: ztpois.
- gg-fit: gengamma, trgamma, invtrgamma. Gengamma has no fitter at this revision.

Other measured families on that list have no returned-point fitter likelihood
miss in this census; native stops and guarded cases remain visible in the table.

### Protected density defects

These ten cases are independently attributed to the density rather than to the
current fitter. Needed changes belong to the protected probability layer.

| Family | Archive indices | Worst relative density error | Needed follow-up |
|---|---|---:|---|
| bbinom | 75 | 2.05e-09 | Accurate density evaluation at the retained exact inputs; keep the independently correct fitter value. |
| lgamma | 4435, 4438 | 2.79e-07 | Accurate density evaluation at the retained exact inputs; keep the independently correct fitter value. |
| lnorm | 4601, 4604, 4611 | 1.69e-07 | Accurate density evaluation at the retained exact inputs; keep the independently correct fitter value. |
| pareto4 | 5665, 5666, 5668, 5669 | 3.35e-06 | Accurate density evaluation at the retained exact inputs; keep the independently correct fitter value. |


Beta-binomial specifically needs the stable large-count PMF identity now used
by its fitter. Density-based all-held shortcuts inherit a remaining density
defect; they were not made to agree by corrupting a correct fitter value.

There is also a separately measured R stats::dbeta problem, outside the native
ten-case count. At the final beta case-100 coefficients, R 4.6.1 gives a density
sum of 135.27891231404931, versus the independent/native value
135.27888712858683: relative error 1.862e-07.
The R regression therefore checks the exact coefficients and independent value.
An accurate R density route or upstream repair is needed; the protected d/p/q/r
and code generators were left unchanged. See [r-beta-density.json](2026-11-11-seam-check-data/r-beta-density.json).

### Coefficient finding outside this likelihood-only correction

The same closed-form check establishes a native extended-precision inverse-Gaussian
shape error of 1.003e-08 in cases 3835/3838:
the retained native shape is 8.235308862764327e23, while the continuous MLE is
approximately 8.2353089453632802e+23. Its likelihood nevertheless
meets the maximum-value target. This existing coefficient error is explicitly
not closed by a likelihood-value check. The native coefficients were retained
under this job's default-fit freeze. A coefficient-accuracy follow-up must use
the centered continuous-mean shape calculation in the native branch and validate
its coefficient contract. The double-only fits changed for the likelihood repair
now satisfy the independent shape check as well.

### Six guarded-open fits

Each case below was actually retried with a 120-second wall-clock guard.
No terminal native outcome was obtained, so there is no public point or logLik
to evaluate. A guard expiration is neither a native budget status nor a proof of
nonexistence. Closing these entries requires a terminal default-control fit,
or runtime analysis that makes that fit obtainable. The input and retry evidence
is retained, including the three final Feller-Pareto retries in the six-family
beta-dependent replay.

| Family | Archive index | Dataset | Holds | Missing |
|---|---:|---|---|---|
| cmb | 837 | large-count | none | terminal native result after 120 s |
| cmb | 915 | large-count | prob=0.2777362750324292, nu=1.683599466457963 | terminal native result after 120 s |
| dgamma | 997 | large-count | none | terminal native result after 120 s |
| fpareto | 1599 | scale-1e-150 | min=0 | terminal native result after 120 s |
| fpareto | 1600 | scale-1e+150 | min=0 | terminal native result after 120 s |
| fpareto | 10717 | support-lower-1e-200 | min=0 | terminal native result after 120 s |


## Per-family before/after table

Counts are refits, not unique data arrays: distinct held subsets and archived
duplicates remain distinct cases. A dagger marks ownership elsewhere.
Density counts include a case attributed to both sides before the fixes.

| Family | Refits | Fitter errors before/after | Density errors before/after | Native stops before/after | Guarded before/after |
|---|---:|---:|---:|---:|---:|
| bbinom | 126 | 1 / 0 | 1 / 1 | 12 / 12 | 0 / 0 |
| beta | 89 | 1 / 0 | 0 / 0 | 0 / 0 | 0 / 0 |
| betapr † | 185 | 0 / 0 | 0 / 0 | 48 / 48 | 0 / 0 |
| bhatt † | 145 | 0 / 0 | 0 / 0 | 57 / 57 | 0 / 0 |
| binom | 65 | 1 / 0 | 0 / 0 | 2 / 2 | 0 / 0 |
| bnbinom | 113 | 0 / 0 | 0 / 0 | 35 / 35 | 0 / 0 |
| burr † | 193 | 2 / 2 | 0 / 0 | 52 / 52 | 0 / 0 |
| cauchy | 73 | 0 / 0 | 0 / 0 | 4 / 4 | 0 / 0 |
| cmb | 113 | 0 / 0 | 0 / 0 | 28 / 30 | 4 / 2 |
| cmp | 57 | 0 / 0 | 0 / 0 | 11 / 11 | 0 / 0 |
| dgamma | 57 | 0 / 0 | 0 / 0 | 12 / 12 | 1 / 1 |
| dlaplace | 57 | 0 / 0 | 0 / 0 | 8 / 8 | 0 / 0 |
| dnorm | 57 | 0 / 0 | 0 / 0 | 8 / 8 | 0 / 0 |
| dunif | 65 | 0 / 0 | 0 / 0 | 6 / 6 | 0 / 0 |
| dweibull | 57 | 0 / 0 | 0 / 0 | 11 / 11 | 0 / 0 |
| emg | 185 | 0 / 0 | 0 / 0 | 73 / 73 | 0 / 0 |
| exp | 43 | 0 / 0 | 0 / 0 | 0 / 0 | 0 / 0 |
| fatigue | 89 | 0 / 0 | 0 / 0 | 15 / 15 | 0 / 0 |
| foldnorm | 97 | 0 / 0 | 0 / 0 | 0 / 0 | 0 / 0 |
| fpareto † | 545 | 0 / 0 | 0 / 0 | 262 / 264 | 5 / 3 |
| frechet | 181 | 0 / 0 | 0 / 0 | 27 / 27 | 0 / 0 |
| gamma | 97 | 0 / 0 | 0 / 0 | 0 / 0 | 0 / 0 |
| gb2 † | 545 | 0 / 0 | 0 / 0 | 273 / 273 | 0 / 0 |
| ged † | 145 | 0 / 0 | 0 / 0 | 48 / 48 | 0 / 0 |
| genbeta † | 193 | 1 / 1 | 0 / 0 | 70 / 73 | 3 / 0 |
| geom | 29 | 0 / 0 | 0 / 0 | 0 / 0 | 0 / 0 |
| gev | 153 | 0 / 0 | 0 / 0 | 11 / 11 | 0 / 0 |
| gompertz | 85 | 0 / 0 | 0 / 0 | 14 / 14 | 0 / 0 |
| gpd | 81 | 0 / 0 | 0 / 0 | 1 / 1 | 0 / 0 |
| gumbel | 85 | 0 / 0 | 0 / 0 | 1 / 1 | 0 / 0 |
| hcauchy | 101 | 0 / 0 | 0 / 0 | 2 / 2 | 0 / 0 |
| hnorm | 83 | 0 / 0 | 0 / 0 | 0 / 0 | 0 / 0 |
| ht | 201 | 0 / 0 | 0 / 0 | 73 / 73 | 0 / 0 |
| huber | 145 | 0 / 0 | 0 / 0 | 76 / 76 | 0 / 0 |
| hyper | 117 | 0 / 0 | 0 / 0 | 10 / 10 | 0 / 0 |
| invburr † | 193 | 0 / 0 | 0 / 0 | 48 / 48 | 0 / 0 |
| invexp | 45 | 0 / 0 | 0 / 0 | 0 / 0 | 0 / 0 |
| invgamma | 97 | 0 / 0 | 0 / 0 | 0 / 0 | 0 / 0 |
| invgauss | 101 | 2 / 0 | 0 / 0 | 0 / 0 | 0 / 0 |
| invparalogis † | 89 | 0 / 0 | 0 / 0 | 5 / 5 | 0 / 0 |
| invpareto | 89 | 0 / 0 | 0 / 0 | 21 / 21 | 0 / 0 |
| invtrgamma † | 153 | 0 / 0 | 0 / 0 | 40 / 40 | 0 / 0 |
| invweibull | 85 | 0 / 0 | 0 / 0 | 1 / 1 | 0 / 0 |
| kumar | 81 | 0 / 0 | 0 / 0 | 10 / 10 | 0 / 0 |
| laplace | 73 | 0 / 0 | 0 / 0 | 0 / 0 | 0 / 0 |
| lgamma | 81 | 0 / 0 | 2 / 2 | 0 / 0 | 0 / 0 |
| lgser | 29 | 0 / 0 | 0 / 0 | 0 / 0 | 0 / 0 |
| llogis | 89 | 0 / 0 | 0 / 0 | 0 / 0 | 0 / 0 |
| lnorm | 101 | 0 / 0 | 3 / 3 | 0 / 0 | 0 / 0 |
| logis | 73 | 0 / 0 | 0 / 0 | 2 / 2 | 0 / 0 |
| logitnorm | 89 | 0 / 0 | 0 / 0 | 0 / 0 | 0 / 0 |
| lomax | 77 | 0 / 0 | 0 / 0 | 12 / 12 | 0 / 0 |
| lst | 145 | 0 / 0 | 0 / 0 | 64 / 64 | 0 / 0 |
| naka | 97 | 0 / 0 | 0 / 0 | 3 / 3 | 0 / 0 |
| nbinom | 65 | 1 / 0 | 0 / 0 | 12 / 12 | 0 / 0 |
| nhyper | 125 | 0 / 0 | 0 / 0 | 12 / 12 | 0 / 0 |
| norm | 73 | 0 / 0 | 0 / 0 | 0 / 0 | 0 / 0 |
| nsbeta | 101 | 1 / 0 | 0 / 0 | 20 / 20 | 0 / 0 |
| paralogis † | 77 | 0 / 0 | 0 / 0 | 2 / 2 | 0 / 0 |
| pareto | 83 | 0 / 0 | 0 / 0 | 0 / 0 | 0 / 0 |
| pareto2 | 89 | 0 / 0 | 0 / 0 | 17 / 17 | 0 / 0 |
| pareto3 † | 77 | 0 / 0 | 0 / 0 | 0 / 0 | 0 / 0 |
| pareto4 † | 177 | 0 / 0 | 4 / 4 | 49 / 49 | 0 / 0 |
| pig | 57 | 0 / 0 | 0 / 0 | 20 / 20 | 0 / 0 |
| pois | 29 | 0 / 0 | 0 / 0 | 0 / 0 | 0 / 0 |
| power | 85 | 0 / 0 | 0 / 0 | 0 / 0 | 0 / 0 |
| rayleigh | 49 | 0 / 0 | 0 / 0 | 0 / 0 | 0 / 0 |
| sged † | 257 | 0 / 0 | 0 / 0 | 157 / 157 | 0 / 0 |
| sgomp | 89 | 0 / 0 | 0 / 0 | 11 / 11 | 0 / 0 |
| skellam | 57 | 0 / 0 | 0 / 0 | 0 / 0 | 0 / 0 |
| slash | 65 | 0 / 0 | 0 / 0 | 3 / 3 | 0 / 0 |
| sn † | 161 | 0 / 0 | 0 / 0 | 74 / 74 | 0 / 0 |
| snorm † | 129 | 0 / 0 | 0 / 0 | 63 / 63 | 0 / 0 |
| sstd † | 257 | 0 / 0 | 0 / 0 | 157 / 157 | 0 / 0 |
| st | 257 | 0 / 0 | 0 / 0 | 148 / 148 | 0 / 0 |
| std | 145 | 0 / 0 | 0 / 0 | 56 / 56 | 0 / 0 |
| tbinom | 241 | 0 / 0 | 0 / 0 | 19 / 19 | 0 / 0 |
| tlambda | 177 | 0 / 0 | 0 / 0 | 23 / 23 | 0 / 0 |
| tnorm | 257 | 9 / 0 | 0 / 0 | 60 / 60 | 0 / 0 |
| tpois | 117 | 1 / 0 | 0 / 0 | 0 / 0 | 0 / 0 |
| trgamma † | 177 | 0 / 0 | 0 / 0 | 44 / 44 | 0 / 0 |
| triang | 169 | 0 / 0 | 0 / 0 | 8 / 8 | 0 / 0 |
| unif | 95 | 0 / 0 | 0 / 0 | 12 / 12 | 0 / 0 |
| weibull | 97 | 0 / 0 | 0 / 0 | 1 / 1 | 0 / 0 |
| yulesimon | 17 | 0 / 0 | 0 / 0 | 4 / 4 | 0 / 0 |
| zeta | 17 | 0 / 0 | 0 / 0 | 3 / 3 | 0 / 0 |
| zib | 129 | 0 / 0 | 0 / 0 | 0 / 0 | 0 / 0 |
| zinb | 129 | 0 / 0 | 0 / 0 | 32 / 32 | 0 / 0 |
| zip | 57 | 0 / 0 | 0 / 0 | 3 / 3 | 0 / 0 |
| zipf | 57 | 0 / 0 | 0 / 0 | 1 / 1 | 0 / 0 |
| zmbinom | 129 | 2 / 0 | 0 / 0 | 4 / 4 | 0 / 0 |
| zmgeom | 57 | 0 / 0 | 0 / 0 | 0 / 0 | 0 / 0 |
| zmlogarithmic | 57 | 0 / 0 | 0 / 0 | 0 / 0 | 0 / 0 |
| zmnbinom | 129 | 0 / 0 | 0 / 0 | 26 / 26 | 0 / 0 |
| zmpois | 57 | 0 / 0 | 0 / 0 | 0 / 0 | 0 / 0 |
| ztbinom | 65 | 1 / 0 | 0 / 0 | 2 / 2 | 0 / 0 |
| ztgeom | 17 | 0 / 0 | 0 / 0 | 0 / 0 | 0 / 0 |
| ztnbinom | 65 | 0 / 0 | 0 / 0 | 15 / 15 | 0 / 0 |
| ztpois † | 29 | 0 / 0 | 0 / 0 | 0 / 0 | 0 / 0 |


## Validation and machines

| Check | Machine | Result |
|---|---|---|
| just cpp-test | simbox, GCC 13 | 108/108 dev shards passed |
| just cpp-test-no-long-double | simbox, GCC 13 | 135/135 passed |
| just cpp-test-no-long-double | workstation, GCC 15 | 135/135 passed |
| Late beta-dependent C++ checks, both modes | workstation, GCC 15 | 200/200 cases per mode, 45,870 assertions per mode |
| Final inverse-Gaussian C++ checks, both modes | workstation, GCC 15 | 15/15 cases per mode; maximum and double-only shape assertions passed |
| Final inverse-Gaussian refits, both modes | workstation, GCC 15 | 101/101 density-seam passes per mode |
| just test-full | workstation after simbox exit 75 | FAIL 0, WARN 0, SKIP 0, PASS 52,372 |
| Final affected R dev files, beta and inverse Gaussian | workstation | FAIL 0, WARN 0, SKIP 2, PASS 38 |
| just site-check after both dossier changes | workstation | passed, including 99 generated C++ snippets |
| Beta-dependent JS follow-up | workstation, rebuilt WASM | 54/54 passed |
| Final inverse-Gaussian and facade JS follow-up | workstation, rebuilt WASM | 5/5 passed |
| Final WASM inverse-Gaussian witness | workstation | status, maximum value, shape and density-sum assertions passed |

The full R dev run initially had 25,611 passes, no warnings, 154 tier skips,
and one failure in this job's new beta test: it incorrectly used the inaccurate
stats::dbeta value as its oracle. The new test was corrected to the independently
evaluated point, without changing an existing expectation. The subsequent full
R run and final affected dev files are green.

The initial complete JS run passed 614/615 tests and exposed an overly tight
nonstandard-beta golden roundoff regression. Retaining the valid continuous
likelihood on ordinary beta samples fixed it; the 54 affected tests then passed.
The later inverse-Gaussian correction was checked with its five affected tests
and an exact-input WASM witness. JS tests were not edited.

The full suites were run once, followed by affected-family checks after late
fixes, as instructed. The final GCC 13 inverse-Gaussian attempt returned simbox
exit 75; current GCC 15 targeted checks supply its prescribed local fallback.
The earlier complete GCC 13 run is retained distinctly from that late fallback.
Elapsed times in run records include CPU-slot waits and are not benchmarks.

Run metadata: [final-runs.json](2026-11-11-seam-check-data/final-runs.json), [final-invgauss-runs.json](2026-11-11-seam-check-data/final-invgauss-runs.json),
[final-r-retest-runs.json](2026-11-11-seam-check-data/final-r-retest-runs.json), [final-js-targeted-runs.json](2026-11-11-seam-check-data/final-js-targeted-runs.json),
[late-invgauss-runs.json](2026-11-11-seam-check-data/late-invgauss-runs.json). Complete retained logs:
[log-index.json](2026-11-11-seam-check-data/log-index.json). The source-policy check confirms 20 append-only test
files and untouched protected paths: [source-policy-check.json](2026-11-11-seam-check-data/source-policy-check.json).

The final diff check is clean. Local destinations in all four edited/new
Markdown files pass the scoped link check. The repository-wide checker reports
45 existing findings, including private missing literature files and parser
false positives; each affected file is byte-identical to the baseline.
See [link-check.json](2026-11-11-seam-check-data/link-check.json) and the retained repository-wide log.

## Reusing the checker

Run from the repository root with existing validation dependencies:

```sh
mkdir -p build/tmp
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
/tmp/umvl-cpu/run python3 dev/validation/seam-check.py --output build/scratch/seam-check/fresh.jsonl
/tmp/umvl-cpu/run python3 dev/validation/seam-check.py --family beta,tnorm --output build/scratch/seam-check/subset.jsonl
/tmp/umvl-cpu/run python3 dev/validation/seam-check.py --family beta --case 100 --timeout 120 --no-long-double --output build/scratch/seam-check/beta-double.jsonl
```

The family filter accepts active fitter IDs; repeated --case options select
archive indices and omit goldens. The default wall-clock guard is 30 seconds.
--no-long-double selects the library's double-only mode.
Outputs are resumable JSONL plus summary and source-provenance JSON. Source,
selection or timeout changes require a fresh output. Compiled probe names also
include the source fingerprint, so separate source versions cannot overwrite a
live probe. Completed retained JSONL is losslessly gzipped; restore it to an
unchanged scratch run before resuming, or use a new output.

The checker was checked against five exact-support boundary probes, four error
gate probes, and deliberate stale-provenance reuse; see [check-checker.json](2026-11-11-seam-check-data/check-checker.json)
and [checker-provenance-validation.json](2026-11-11-seam-check-data/checker-provenance-validation.json).
The baseline was compiled before any production edit from the clean starting
revision. Its driver and hash record are retained without archiving source trees:
[baseline-provenance.json](2026-11-11-seam-check-data/baseline-provenance.json), [baseline-driver.cpp.gz](2026-11-11-seam-check-data/baseline-driver.cpp.gz).
The read-only [reproduce-baseline.sh](2026-11-11-seam-check-data/reproduce-baseline.sh) reconstructs that revision into
ignored scratch with git archive for a fresh replay.

Primary census evidence: [before.jsonl.gz](2026-11-11-seam-check-data/before.jsonl.gz), [final.jsonl.gz](2026-11-11-seam-check-data/final.jsonl.gz),
[final-summary.json](2026-11-11-seam-check-data/final-summary.json). The final six-family dependent replay is
[after-current-beta.jsonl.gz](2026-11-11-seam-check-data/after-current-beta.jsonl.gz); the final inverse-Gaussian double-only
replay is [after-invgauss-double.jsonl.gz](2026-11-11-seam-check-data/after-invgauss-double.jsonl.gz). All retained files and
their hashes are listed in [evidence-index.json](2026-11-11-seam-check-data/evidence-index.json).
