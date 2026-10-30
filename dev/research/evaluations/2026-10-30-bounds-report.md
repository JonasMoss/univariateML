# TASK-78.14 — boundary closure audit

Audited all 98 families against README domains, fit restrictions, held-parameter paths, and attained fitter statuses. Named `limits:` escapes remain errors, not evaluation members. No signature, oracle, metadata, adapter, or golden changes; no commits or board edits.

## Changes and checks

- **binom:** the attained point sampler (prob=1) could return a zero-mass nominal endpoint for a zero uniform draw. Evaluate the point law directly without drawing. Zero-uniform closed-form doctest; R stays on stats for this family.
- **cmb:** the attained point sampler (prob=1) could return a zero-mass nominal endpoint for a zero uniform draw. Evaluate the point law directly without drawing. Zero-uniform closed-form doctest; R d/p/q/r fitted-object regression.
- **zib:** the attained point sampler (prob=1, pi=0) could return a zero-mass nominal endpoint for a zero uniform draw. Evaluate the point law directly without drawing. Zero-uniform closed-form doctest; R d/p/q/r fitted-object regression.
- **ztbinom:** the attained point sampler (prob=1) could return a zero-mass nominal endpoint for a zero uniform draw. Evaluate the point law directly without drawing. Zero-uniform closed-form doctest; R d/p/q/r fitted-object regression.
- **zmbinom:** the attained point sampler (prob=1, p0=0) could return a zero-mass nominal endpoint for a zero uniform draw. Evaluate the point law directly without drawing. Zero-uniform closed-form doctest; R d/p/q/r fitted-object regression.
- **tbinom:** the attained point sampler (prob=0/1 with the atom included) could return a zero-mass nominal endpoint for a zero uniform draw. Evaluate the point law directly without drawing. Zero-uniform closed-form doctest; R d/p/q/r fitted-object regression.
- **genbeta:** finite upper density at shape2=1 formed shape1*shape3 before taking its log, overflowing for finite shapes. Sum the factor logs instead. Closed-form B(a,1)=1/a doctest includes an overflowing product; R d/p/q endpoint regression.
- **dunif:** singleton inverse sampling returned min-1 when the uniform draw was zero (also possible after rounding a tiny draw). Return the attained point directly and validate parameters. Closed-form d/p/q regression, a zero-uniform sampler regression, and R fitted-object checks.
- **nbinom:** size zero was rejected by both `special::dnbinom` and family d/p/q; sampler entered Gamma(0). Accept the exact delta-zero law, including both tails/log scales, all quantile endpoints, and deterministic zero draws. Closed-form family doctest and R `dml`/`pml`/`qml` regression (R uses stats for this family).
- **gpd:** density at the finite upper endpoint was zero for xi=-1. Return 1/sigma there; also implement the exact zero/infinite endpoint limits for other negative xi. Doctests use the uniform law, both scales/tails, endpoint quantiles, deep log probabilities and inverse-transform sampling; R fitted-object endpoint regression.
- **gev:** same density defect at xi=-1. Return 1/sigma and the appropriate other negative-xi endpoint limits. Doctests use mu+sigma-sigma*Exp(1), both scales/tails, endpoint quantiles and sampling; R fitted-object endpoint regression.
- **tnorm:** density excluded a and b. Include finite bounds using the normalized inside normal density; infinite observations still have zero density. Replace two legacy zero-at-bound assertions. Closed-form phi/(Phi(b)-Phi(a)) doctest and R density/CDF/quantile regression.
- **cmp:** legacy `cmp_mle` still classified the attained geometric face as an escape and rejected lambda=0. Align it with the family semantics; reduce `cmp_domain_mle` to a forwarding wrapper. Existing caller/status tests updated; family closed-form geometric/point-law regression added. Public signatures unchanged.
- **sstd:** reject unacceptable interior multistart candidates before ranking, with total-score normalization restored in the shared R6 gate; promote score-accepted candidates. Gate independent df-two face refinements too; retain failed valid points as lower likelihood bounds for limit comparisons and as independently checked face-refinement seeds. Existing golden, score, hold and limit tests plus quality harness.
- **sged:** objective ranking admitted failed bad-score candidates. Apply the shared finite-coordinate/objective/total-score R6 gate before ranking and promote accepted candidates. Analytic attained Laplace face retained; with no accepted interior candidate, a better failed attempt prevents an unchecked Laplace fallback. A normal-grid one-step regression checks this, alongside the existing R failure-path test. Existing golden, score, hold and limit tests plus quality harness.

GPD/GEV general endpoint detection uses the compensated support residual, not equality to a rounded quotient. The existing binary64 endpoint-neighbor truth regressions caught that distinction during development. The finite-density xi=-1 member uses the rounded mu+sigma endpoint consistently for density, both CDF tails/log scales, and endpoint quantiles; a 0.1+0.7 regression checks inward rounding.

## Attained parameter faces: audited correct unless changed above

Only included finite domain ends and finite fit restrictions count, except the explicitly allowed infinite truncation bounds. Integer lower ends such as size=1 also have their ordinary finite-law formulas. Intersections must satisfy the joint domain and conditioning event.

- **bbinom:** size=0 gives delta-zero.
- **bhatt:** a=0 gives the parent normal.
- **binom:** size=0, prob=0/1 give the corresponding point laws.
- **cmb:** size=0/10000000, prob=0/1; finite enumeration handles the cap and point laws, including intersections.
- **cmp:** lambda=0 gives delta-zero; nu=0 with lambda<1 gives geometric. Legacy API changed above; d/p/q/r already correct.
- **dunif:** min=max gives a singleton; sampler changed above.
- **foldnorm:** mu=0 gives half-normal.
- **frechet:** fit bound lambda=1 is the ordinary inverse-exponential member; held mu permits positive lambda below one.
- **ged:** fit bound nu=1 gives standardized Laplace.
- **geom:** prob=1 gives delta-zero.
- **gev:** fit bounds xi=-1/+1 give reversed exponential/shifted inverse exponential; upper density changed above.
- **gpd:** fit bound xi=-1 gives uniform; upper density changed above.
- **ht:** fit bound nu=1 gives half-Cauchy; held mu permits ordinary positive nu below one.
- **hyper:** N=0, m=0/N, k=0/N; valid joint combinations give their finite hypergeometric/point laws.
- **lst:** fit bound df=1 gives location-scale Cauchy; held mu permits ordinary positive df below one.
- **nbinom:** size=0 and prob=1 give delta-zero; size-zero implementation changed above.
- **nhyper:** N=0, m=0/N, r=0/m; valid joint combinations give singleton/negative-hypergeometric laws.
- **pig:** shape=0 gives delta-zero; prior fix retained.
- **pois:** lambda=0 gives delta-zero.
- **sged:** fit bound nu=1 gives standardized skew Laplace; d/p/q/r already correct.
- **sgomp:** eta=0 gives exponential.
- **skellam:** either mean zero gives signed Poisson; both zero give delta-zero.
- **tbinom:** size=0, prob=0/1 only when the conditioning event contains the atom; otherwise no attained fit.
- **tlambda:** fit bounds lambda=-1/+1 use the ordinary quantile law/uniform member.
- **tnorm:** a=-Inf and/or b=Inf give one-sided or untruncated normal; finite endpoint density changed above.
- **tpois:** lambda=0 only with zero included; singleton intervals at positive rate give delta at the included count.
- **triang:** c=a/b gives the decreasing/increasing triangular member.
- **zib:** size=0, prob=0/1, pi=0/1 give binomial/zero/zero-plus-size laws.
- **zinb:** size=0, prob=1, pi=0/1 give NB or delta-zero; prior fix retained.
- **zip:** pi=0/1 gives Poisson/delta-zero; lambda remains strictly positive.
- **zipf:** N=1 gives delta-one; s=0 gives finite uniform.
- **zmbinom:** size=1, prob=0/1, p0=0/1 give Bernoulli/conditional binomial/zero-plus-size laws.
- **zmgeom:** prob=1, p0=0/1 give Bernoulli/truncated geometric/delta-zero.
- **zmlogarithmic:** prob=0, p0=0/1 give Bernoulli/logarithmic/delta-zero.
- **zmnbinom:** size=0 gives zero-modified logarithmic with parameter 1-prob; prob=1 gives Bernoulli; p0=0/1 gives the positive component/delta-zero.
- **zmpois:** lambda=0 gives Bernoulli; p0=0/1 gives truncated Poisson/delta-zero.
- **ztbinom:** size=1 or prob=0 gives delta-one; prob=1 gives delta-size.
- **ztgeom:** prob=1 gives delta-one.
- **ztnbinom:** size=0 gives logarithmic with parameter 1-prob; prob=1 gives delta-one, including their intersection.
- **ztpois:** lambda=0 gives delta-one.

## Support closure and remaining families

- **Already correct coefficient-dependent continuous endpoints:** hnorm/hcauchy/ht at mu; power at alpha; pareto at b; pareto2, pareto3, pareto4 and fpareto at min; nsbeta at min/max; fatigue/frechet at mu; unif at min/max; triang at a/b including c=a/b; tlambda at both finite endpoints. Formulas distinguish zero, finite and singular limits; p/q use the same endpoints in both tails and scales. GPD lower endpoint already correct; finite upper endpoint, GEV/tnorm closure and genbeta endpoint log range changes described above.
- **No additional attained parameter boundary; ordinary open-domain formulas audited:** beta, betapr, bnbinom, burr, cauchy, dgamma, dlaplace, dnorm, dweibull, emg, exp, gamma, gb2, gompertz, gumbel, huber, invburr, invexp, invgamma, invgauss, invparalogis, invpareto, invtrgamma, invweibull, kumar, laplace, lgamma, lgser, llogis, lnorm, logis, logitnorm, lomax, naka, norm, paralogis, rayleigh, slash, sn, snorm, sstd, std, trgamma, weibull, yulesimon, zeta. Endpoint-only families are in the preceding line; faces are in the preceding inventory.

## Validation

- Final `just cpp-test` on simbox: **6/6 CTest targets pass; 925 cases, 1,225,081 assertions** (distribution target: 903 cases, 1,214,034 assertions).
- Full uncapped R suite after `just regen` on simbox: **FAIL 0 | WARN 0 | SKIP 0 | PASS 34,235** (476.0 s).
- `just quality cmp sstd sged`: **3/3 pass** on simbox after the exhausted-search regression fix.
- Catalog probe: **186 parameter cases** over all 98 default laws and declared numeric/symbolic closed faces, using valid joint parameters; both tails/scales at the median and sampler finiteness. **Zero invalid results** at valid joint parameter values. A second probe also checks zero-uniform sampling whenever the integer law is a point mass; no discrepancies.
- **98/98 golden.json and 98/98 truth.json byte-identical to HEAD**; `git diff --check` clean.
- Independent metadata endpoint probe: **1,068 checks, 0 failures**, covering both tails/scales, finite endpoint quantiles, and declared zero/finite/infinite density limits across shape branches.
- Final logs: `/tmp/umvl-codex/bounds/{cpp-final,r-final,final-quality}.log`; probes and inventories are in the same scratch directory.

Open questions: none identified. Invalid CMP geometric joint parameters and zero-mass truncation events are deliberately excluded, consistent with fitters and README prose. Quantiles retain the established nominal lattice-endpoint conventions of binomial-derived finite laws. No README domain edits needed.
