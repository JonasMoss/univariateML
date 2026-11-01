# Firmness audit of declared limits

1 November 2026. Source revision `d8a62683b425204adcd3569ba7d1d30cc8140fc9`, with the uncommitted dossier and site-check changes in this worktree. The inventory covers all 98 dossiers: 82 declare limits and 16 have no limit records. The 269 declared records are listed below in dossier order; the ordinal identifies the record within its family.

| Grade | Records |
|---|---:|
| proved | 174 |
| argued | 72 |
| numerical | 23 |
| conjecture | 0 |

Firmness describes the evidence for the stated boundary, including the declared held set, rather than certification of the numerical detection procedure. A proof of a limiting path does not prove that the face is the global likelihood supremum. The existing `numerical: true` fields were retained independently of firmness. The cited arguments and proofs are mathematical evidence; the tests below are regression and rendering checks, not proofs of existence or global optimality.

## Corrected declarations

- **GB2 and shifted GB2 (`fpareto`), six records each:** specified the joint scale and shape rates for transformed-gamma, inverse transformed-gamma, lognormal, Pareto, power and double-Pareto paths. The lognormal entry gives a symmetric large-shape path; it is a sufficient path to every lognormal parameter pair, not a claim that all lognormal approaches are symmetric. The [GB2 derivation](2026-10-28-gb2.md) proves these paths; this does not establish exhaustion of all maximizing escapes.
- **Beta-negative-binomial, negative-binomial face:** specified the positive fixed shape in each of the two interchangeable paths. **Point mass at zero:** specified that the other two coefficients stay positive and fixed for each displayed one-parameter path. Without these qualifications a different joint divergence can have another limit. See the [discrete-family analysis](2026-10-29-p0-discrete2.md).
- **Pareto IV, held-scale Pareto face:** corrected the support condition to `x - min >= scale`, since the fitted Burr delegate acts on the shifted data and the limiting endpoint is `min + scale`; see the [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/pareto4/README.md).
- **Skew GED, two default half-family records:** added the missing right and left translated half-GED names; their directions and statuses are unchanged.

The site check previously hard-coded the old Lomax grade; its assertion now expects the proved grade while still checking the separate numerical-detection label. The binomial, negative-binomial and Lomax grades reflect the existence criteria cited in their dossiers; fixed-power Burr and inverse Burr inherit the Lomax criterion after transformation. The skew-normal half-face grades use the profile derivation in [ML behaviour](../ml-behaviour.tex).

## Open mathematical and numerical qualifications

- **Conditional skew families:** the [conditional skew report](2026-10-29-p2b-skew-report.md) derives the collapse exponents and describes the held-moment faces, but does not certify the global numerical comparisons. In particular, the `sstd` tie-equality cases require a global interior-versus-boundary comparison; the `sged` case where a fixed uniform width equals the sample range requires a joint endpoint-rate analysis. The scale-tail guard is a numerical failure condition, not a proof of an escape.
- **GB2 and generalized beta:** the individual paths have the indicated evidence, but completeness of all maximizing joint escapes is not established. The [open-existence analysis](2026-10-31-open-existence-report.md) gives a mixed weak limit for generalized beta and proves only that its displayed path cannot win for strictly interior data; a general mixed-rate classification remains open.
- **Face comparisons and numerical detection:** a tolerance, a finite grid, or a multistart search does not prove that no finite maximizer ties or beats a face. This affects, among others, the Lomax, beta-binomial, skew-family and generalized-beta comparisons. The grades do not certify optimizer outputs or floating-point signs. For negative binomial, see the [follow-up](2026-11-01-existence-fu-report.md) for the remaining numerical bracket and sign-certification gap.
- **Boundary density values:** distributional convergence does not by itself settle likelihood values at observations exactly on a moving endpoint. The family analyses retain their stated endpoint conventions and equality qualifications.

## Validation

All runs were local in this worktree, separately through `/tmp/umvl-cpu/run4`, with `TMPDIR`, `JUST_TEMPDIR`, and `XDG_RUNTIME_DIR` set to `build/tmp`; the wrapper pinned BLAS/OpenMP threads to one and ran at `nice -n 10`. No new random simulation was used for this audit. The reference source revision above plus the uncommitted changes in this worktree were tested.

| Command | Result |
|---|---|
| `just test` | Pass: 19,898 expectations, 0 failures, 0 warnings, 151 skips (dev tier). |
| `just test-full` | Pass: 41,366 expectations, 0 failures, 0 warnings, 0 skips. |
| `just cpp-test` | Pass: 105/105 dev shards. The initial Boost discovery attempt failed and the recipe successfully used its fallback; compiler warnings were emitted. |
| `just relations` | Pass: 156 edges, 0 failed; some relations are symbolic, some numerical, and declared closure points are not all checked. |
| `just site-check` | Pass: 269 rendered limit records, 98 family pages and browser checks; one existing binomial self-comparison compiler warning. |

The site check used Python 3.14.5, SymPy 1.14.0 and GCC 15.2.0, and compiled the 98 generated C++ snippets. The R test used R 4.6.1. The earlier site failure was an assertion expecting the former Lomax grade; the assertion was updated and the final run passed. These checks do not add numerical evidence for a boundary or certify a global search.

## Record inventory

The table links each record to its canonical dossier and, where linked in the dossier, its supporting family analysis. “Fixed set unspecified” means the record has no `fixed` key; it does not assert that every held subset is covered. An empty `fixed` list is shown as “none held”.

| Family | Record | Grade | Source |
|---|---|---|---|
| bbinom | 1. binom (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/bbinom/README.md); [analysis](2026-10-31-open-existence-report.md) |
| bbinom | 2. two-point law on 0 and n (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/bbinom/README.md); [analysis](2026-10-31-open-existence-report.md) |
| bbinom | 3. binom (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/bbinom/README.md); [analysis](2026-10-31-open-existence-report.md) |
| beta | 1. point-mass collapse (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/beta/README.md); [analysis](2026-10-29-p0-bounded.md) |
| betapr | 1. positive log-variance (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/betapr/README.md); [analysis](2026-10-29-p0-halfthreshold.md) |
| betapr | 2. gamma (shape1 held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/betapr/README.md); [analysis](2026-10-29-p0-halfthreshold.md) |
| betapr | 3. invgamma (shape2 held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/betapr/README.md); [analysis](2026-10-29-p0-halfthreshold.md) |
| betapr | 4. gamma (fixed set unspecified) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/betapr/README.md); [analysis](2026-10-29-p0-halfthreshold.md) |
| betapr | 5. invgamma (fixed set unspecified) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/betapr/README.md); [analysis](2026-10-29-p0-halfthreshold.md) |
| bhatt | 1. unif (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/bhatt/README.md); [analysis](2026-10-29-p0-locscale.md) |
| binom | 1. pois (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/binom/README.md) |
| bnbinom | 1. nbinom (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/bnbinom/README.md); [analysis](2026-10-29-p0-discrete2.md) |
| bnbinom | 2. pois (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/bnbinom/README.md); [analysis](2026-10-29-p0-discrete2.md) |
| bnbinom | 3. pois (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/bnbinom/README.md); [analysis](2026-10-29-p0-discrete2.md) |
| burr | 1. unbounded likelihood (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/burr/README.md); [analysis](2026-10-29-p0-genbeta.md) |
| burr | 2. weibull (shape2 held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/burr/README.md); [analysis](2026-10-29-p0-genbeta.md) |
| burr | 3. weibull (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/burr/README.md); [analysis](2026-10-29-p0-genbeta.md) |
| burr | 4. pareto (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/burr/README.md); [analysis](2026-10-29-p0-genbeta.md) |
| burr | 5. pareto (scale held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/burr/README.md); [analysis](2026-10-29-p0-genbeta.md) |
| cauchy | 1. scale collapse (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/cauchy/README.md); [analysis](2026-10-29-p0-locscale.md) |
| cmb | 1. cmp (nu held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/cmb/README.md); [analysis](2026-10-29-p0-binomial.md) |
| cmb | 2. point mass (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/cmb/README.md); [analysis](2026-10-29-p0-binomial.md) |
| cmb | 3. adjacent two-point law (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/cmb/README.md); [analysis](2026-10-29-p0-binomial.md) |
| cmb | 4. endpoint two-point law (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/cmb/README.md); [analysis](2026-10-29-p0-binomial.md) |
| cmp | 1. adjacent two-point law (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/cmp/README.md); [analysis](2026-10-29-p0-discrete2.md) |
| cmp | 2. point mass (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/cmp/README.md); [analysis](2026-10-29-p0-discrete2.md) |
| dgamma | 1. point mass (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/dgamma/README.md); [analysis](2026-10-29-p0-discrete2.md) |
| dgamma | 2. shifted Bernoulli (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/dgamma/README.md); [analysis](2026-10-29-p0-discrete2.md) |
| dgamma | 3. point mass at zero (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/dgamma/README.md); [analysis](2026-10-29-p0-discrete2.md) |
| dlaplace | 1. point mass at location (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/dlaplace/README.md) |
| dnorm | 1. point mass (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/dnorm/README.md); [analysis](2026-10-29-p0-discrete2.md) |
| dnorm | 2. shifted Bernoulli (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/dnorm/README.md); [analysis](2026-10-29-p0-discrete2.md) |
| dnorm | 3. zero-scale limit (mean held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/dnorm/README.md); [analysis](2026-10-29-p0-discrete2.md) |
| dweibull | 1. degenerate (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/dweibull/README.md); [analysis](2026-10-29-p0-discrete2.md) |
| dweibull | 2. degenerate (shape2 held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/dweibull/README.md); [analysis](2026-10-29-p0-discrete2.md) |
| dweibull | 3. binom (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/dweibull/README.md); [analysis](2026-10-29-p0-discrete2.md) |
| dweibull | 4. adjacent two-point (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/dweibull/README.md); [analysis](2026-10-29-p0-discrete2.md) |
| dweibull | 5. Bernoulli(shape1) (shape1 held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/dweibull/README.md); [analysis](2026-10-29-p0-discrete2.md) |
| emg | 1. norm (fixed set unspecified) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/emg/README.md); [analysis](2026-10-29-p0-skewreal.md) |
| emg | 2. translated exponential (fixed set unspecified) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/emg/README.md); [analysis](2026-10-29-p0-skewreal.md) |
| exp | 1. unbounded likelihood (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/exp/README.md); [analysis](2026-10-29-p0-positive.md) |
| fatigue | 1. point mass (mu held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/fatigue/README.md); [analysis](2026-10-29-p0-halfthreshold.md) |
| fatigue | 2. point mass (beta, mu held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/fatigue/README.md); [analysis](2026-10-29-p0-halfthreshold.md) |
| fpareto | 1. unbounded likelihood (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/fpareto/README.md); [analysis](2026-10-28-gb2.md) |
| fpareto | 2. shifted trgamma; subtract min before fitting (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/fpareto/README.md); [analysis](2026-10-28-gb2.md) |
| fpareto | 3. shifted invtrgamma; subtract min before fitting (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/fpareto/README.md); [analysis](2026-10-28-gb2.md) |
| fpareto | 4. shifted lnorm; subtract min before fitting (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/fpareto/README.md); [analysis](2026-10-28-gb2.md) |
| fpareto | 5. shifted pareto; subtract min before fitting (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/fpareto/README.md); [analysis](2026-10-28-gb2.md) |
| fpareto | 6. shifted power; subtract min before fitting (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/fpareto/README.md); [analysis](2026-10-28-gb2.md) |
| fpareto | 7. shifted double Pareto; subtract min before fitting (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/fpareto/README.md); [analysis](2026-10-28-gb2.md) |
| frechet | 1. fixed-shape collapse (lambda held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/frechet/README.md); [analysis](2026-10-29-p0-extreme.md) |
| frechet | 2. shape divergence (sigma held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/frechet/README.md); [analysis](2026-10-29-p0-extreme.md) |
| frechet | 3. gumbel (fixed set unspecified) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/frechet/README.md); [analysis](2026-10-29-p0-extreme.md) |
| frechet | 4. free collapse (fixed set unspecified) | numerical | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/frechet/README.md); [analysis](2026-10-29-p0-extreme.md) |
| gamma | 1. unbounded likelihood (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/gamma/README.md); [analysis](2026-10-29-p0-positive.md) |
| gb2 | 1. unbounded likelihood (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/gb2/README.md); [analysis](2026-10-28-gb2.md) |
| gb2 | 2. trgamma (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/gb2/README.md); [analysis](2026-10-28-gb2.md) |
| gb2 | 3. invtrgamma (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/gb2/README.md); [analysis](2026-10-28-gb2.md) |
| gb2 | 4. lnorm (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/gb2/README.md); [analysis](2026-10-28-gb2.md) |
| gb2 | 5. pareto (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/gb2/README.md); [analysis](2026-10-28-gb2.md) |
| gb2 | 6. power (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/gb2/README.md); [analysis](2026-10-28-gb2.md) |
| gb2 | 7. double Pareto (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/gb2/README.md); [analysis](2026-10-28-gb2.md) |
| ged | 1. scale collapse (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/ged/README.md); [analysis](2026-10-29-p0-locscale.md) |
| ged | 2. unif (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/ged/README.md); [analysis](2026-10-29-p0-locscale.md) |
| genbeta | 1. unbounded likelihood (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/genbeta/README.md); [analysis](2026-10-29-p0-genbeta.md) |
| genbeta | 2. unit gamma on x/scale (fixed set unspecified) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/genbeta/README.md); [analysis](2026-10-29-p0-genbeta.md) |
| genbeta | 3. power (fixed set unspecified) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/genbeta/README.md); [analysis](2026-10-29-p0-genbeta.md) |
| gev | 1. scale collapse (mu held) | numerical | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/gev/README.md); [analysis](2026-10-29-p0-extreme.md) |
| gev | 2. scale collapse (fixed set unspecified) | numerical | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/gev/README.md); [analysis](2026-10-29-p0-extreme.md) |
| gompertz | 1. unbounded likelihood (b held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/gompertz/README.md); [analysis](2026-10-29-p0-positive.md) |
| gompertz | 2. exponential face (a held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/gompertz/README.md); [analysis](2026-10-29-p0-positive.md) |
| gompertz | 3. exp (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/gompertz/README.md); [analysis](2026-10-29-p0-positive.md) |
| gpd | 1. threshold scale collapse (mu held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/gpd/README.md); [analysis](2026-10-29-p0-extreme.md) |
| gumbel | 1. scale collapse (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/gumbel/README.md); [analysis](2026-10-29-p0-locscale.md) |
| hcauchy | 1. unbounded scale collapse (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/hcauchy/README.md); [analysis](2026-10-29-p0-halfthreshold.md) |
| hcauchy | 2. finite unattained scale-zero supremum (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/hcauchy/README.md); [analysis](2026-10-29-p0-halfthreshold.md) |
| hnorm | 1. scale collapse (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/hnorm/README.md); [analysis](2026-10-29-p0-halfthreshold.md) |
| ht | 1. half-normal with the same endpoint; use mlhnorm(x, mu = 0) after a held translation, or mlhnorm(x, mu = NULL) with a free one (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/ht/README.md); [analysis](2026-10-29-p0-halfthreshold.md) |
| ht | 2. unbounded scale collapse (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/ht/README.md); [analysis](2026-10-29-p0-halfthreshold.md) |
| ht | 3. finite unattained scale-zero supremum (nu held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/ht/README.md); [analysis](2026-10-29-p0-halfthreshold.md) |
| huber | 1. scale collapse (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/huber/README.md); [analysis](2026-10-29-p0-locscale.md) |
| huber | 2. norm (none held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/huber/README.md); [analysis](2026-10-29-p0-locscale.md) |
| huber | 3. laplace (none held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/huber/README.md); [analysis](2026-10-29-p0-locscale.md) |
| hyper | 1. degenerate (m held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/hyper/README.md); [analysis](2026-10-29-p0-binomial.md) |
| invburr | 1. unbounded likelihood (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/invburr/README.md); [analysis](2026-10-29-p0-genbeta.md) |
| invburr | 2. invweibull (shape2 held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/invburr/README.md); [analysis](2026-10-29-p0-genbeta.md) |
| invburr | 3. invweibull (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/invburr/README.md); [analysis](2026-10-29-p0-genbeta.md) |
| invburr | 4. power (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/invburr/README.md); [analysis](2026-10-29-p0-genbeta.md) |
| invburr | 5. power (scale held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/invburr/README.md); [analysis](2026-10-29-p0-genbeta.md) |
| invgamma | 1. unbounded likelihood (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/invgamma/README.md); [analysis](2026-10-29-p0-positive.md) |
| invgauss | 1. unbounded likelihood (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/invgauss/README.md); [analysis](2026-10-29-p0-positive.md) |
| invgauss | 2. unbounded likelihood (mean held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/invgauss/README.md); [analysis](2026-10-29-p0-positive.md) |
| invparalogis | 1. unbounded likelihood (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/invparalogis/README.md); [analysis](2026-10-29-p0-genbeta.md) |
| invpareto | 1. invexp (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/invpareto/README.md); [analysis](2026-10-29-p0-positive.md) |
| invtrgamma | 1. fixed-endpoint Pareto face (scale held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/invtrgamma/README.md); [analysis](2026-10-29-p0-genbeta.md) |
| invtrgamma | 2. unbounded likelihood (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/invtrgamma/README.md); [analysis](2026-10-29-p0-genbeta.md) |
| invtrgamma | 3. lnorm (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/invtrgamma/README.md); [analysis](2026-10-29-p0-genbeta.md) |
| invtrgamma | 4. pareto (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/invtrgamma/README.md); [analysis](2026-10-29-p0-genbeta.md) |
| invweibull | 1. unbounded likelihood (scale held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/invweibull/README.md); [analysis](2026-10-29-p0-positive.md) |
| kumar | 1. point-mass collapse (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/kumar/README.md); [analysis](2026-10-29-p0-bounded.md) |
| laplace | 1. scale collapse (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/laplace/README.md); [analysis](2026-10-29-p0-locscale.md) |
| llogis | 1. unbounded likelihood (scale held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/llogis/README.md); [analysis](2026-10-29-p0-positive.md) |
| lnorm | 1. unbounded likelihood (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/lnorm/README.md); [analysis](2026-10-29-p0-positive.md) |
| lnorm | 2. unbounded likelihood (meanlog held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/lnorm/README.md); [analysis](2026-10-29-p0-positive.md) |
| logis | 1. scale collapse (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/logis/README.md); [analysis](2026-10-29-p0-locscale.md) |
| logitnorm | 1. zero-scale collapse (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/logitnorm/README.md); [analysis](2026-10-29-p0-bounded.md) |
| lomax | 1. exp (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/lomax/README.md) |
| lst | 1. scale collapse (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/lst/README.md); [analysis](2026-10-29-p0-locscale.md) |
| lst | 2. scale collapse (df held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/lst/README.md); [analysis](2026-10-29-p0-locscale.md) |
| lst | 3. tie equality boundary (df held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/lst/README.md); [analysis](2026-10-29-p0-locscale.md) |
| lst | 4. norm (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/lst/README.md); [analysis](2026-10-29-p0-locscale.md) |
| naka | 1. unbounded likelihood (scale held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/naka/README.md); [analysis](2026-10-29-p0-positive.md) |
| nbinom | 1. pois (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/nbinom/README.md) |
| norm | 1. scale collapse (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/norm/README.md); [analysis](2026-10-29-p0-locscale.md) |
| nsbeta | 1. unbounded likelihood (fixed set unspecified) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/nsbeta/README.md); [analysis](2026-10-29-p0-genbeta.md) |
| paralogis | 1. unbounded likelihood (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/paralogis/README.md); [analysis](2026-10-29-p0-genbeta.md) |
| pareto | 1. zero likelihood (b held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/pareto/README.md); [analysis](2026-10-29-p0-positive.md) |
| pareto | 2. unbounded likelihood (b held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/pareto/README.md); [analysis](2026-10-29-p0-positive.md) |
| pareto2 | 1. shifted exp; subtract min before fitting (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/pareto2/README.md); [analysis](2026-10-29-p0-genbeta.md) |
| pareto3 | 1. unbounded likelihood (fixed set unspecified) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/pareto3/README.md); [analysis](2026-10-29-p0-genbeta.md) |
| pareto4 | 1. unbounded likelihood (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/pareto4/README.md); [analysis](2026-10-29-p0-genbeta.md) |
| pareto4 | 2. shifted weibull; subtract min before fitting (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/pareto4/README.md); [analysis](2026-10-29-p0-genbeta.md) |
| pareto4 | 3. shifted pareto; subtract min before fitting (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/pareto4/README.md); [analysis](2026-10-29-p0-genbeta.md) |
| pareto4 | 4. shifted Pareto with its endpoint held at min + scale (scale held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/pareto4/README.md); [analysis](2026-10-29-p0-genbeta.md) |
| pig | 1. pois (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/pig/README.md); [analysis](2026-10-29-p0-counts.md) |
| pig | 2. pois (shape held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/pig/README.md); [analysis](2026-10-29-p0-counts.md) |
| power | 1. zero likelihood (alpha held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/power/README.md); [analysis](2026-10-29-p0-positive.md) |
| sged | 1. unresolved constrained uniform endpoint rates (sd held) | numerical | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sged | 2. unresolved constrained uniform endpoint rates (sd, xi held) | numerical | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sged | 3. unif (mean held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sged | 4. right translated half-family (mean held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sged | 5. left translated half-family (mean held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sged | 6. constant sample scale collapse (mean held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sged | 7. unif (sd held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sged | 8. right translated half-family (sd held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sged | 9. left translated half-family (sd held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sged | 10. right translated half-family (nu held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sged | 11. left translated half-family (nu held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sged | 12. constant sample scale collapse (nu held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sged | 13. unif (xi held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sged | 14. constant sample scale collapse (xi held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sged | 15. unif (mean, sd held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sged | 16. right translated half-family (mean, sd held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sged | 17. left translated half-family (mean, sd held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sged | 18. right translated half-family (mean, nu held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sged | 19. left translated half-family (mean, nu held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sged | 20. constant sample scale collapse (mean, nu held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sged | 21. unif (mean, xi held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sged | 22. constant sample scale collapse (mean, xi held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sged | 23. right translated half-family (sd, nu held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sged | 24. left translated half-family (sd, nu held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sged | 25. unif (sd, xi held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sged | 26. constant sample scale collapse (nu, xi held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sged | 27. right translated half-family (mean, sd, nu held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sged | 28. left translated half-family (mean, sd, nu held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sged | 29. unif (mean, sd, xi held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sged | 30. scale collapse (mean, nu, xi held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sged | 31. uncertified scale boundary (mean, nu, xi held) | numerical | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sged | 32. unif (fixed set unspecified) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sged | 33. right translated half-GED (fixed set unspecified) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sged | 34. left translated half-GED (fixed set unspecified) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| slash | 1. point mass collapse (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/slash/README.md); [analysis](2026-10-29-p0-locscale.md) |
| sn | 1. hnorm (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sn/README.md); [analysis](2026-10-29-p0-skewreal.md) |
| sn | 2. reflected translated half-normal (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sn/README.md); [analysis](2026-10-29-p0-skewreal.md) |
| snorm | 1. scale collapse (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/snorm/README.md); [analysis](2026-10-29-p0-skewreal.md) |
| snorm | 2. hnorm (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/snorm/README.md); [analysis](2026-10-29-p0-skewreal.md) |
| snorm | 3. reflected translated half-normal (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/snorm/README.md); [analysis](2026-10-29-p0-skewreal.md) |
| sstd | 1. right translated half-normal (mean held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 2. left translated half-normal (mean held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 3. right translated half-normal (sd held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 4. left translated half-normal (sd held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 5. right translated half-normal (mean, sd held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 6. left translated half-normal (mean, sd held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 7. tie collapse (mean held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 8. uncertified tie equality (mean held) | numerical | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 9. snorm (mean held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 10. two-piece t with 2 degrees of freedom (mean held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 11. right translated half-family (mean held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 12. left translated half-family (mean held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 13. tie collapse (sd held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 14. uncertified tie equality (sd held) | numerical | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 15. snorm (sd held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 16. right translated half-family (sd held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 17. left translated half-family (sd held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 18. tie collapse (nu held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 19. uncertified tie equality (nu held) | numerical | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 20. right translated half-family (nu held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 21. left translated half-family (nu held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 22. tie collapse (xi held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 23. uncertified tie equality (xi held) | numerical | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 24. snorm (xi held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 25. two-piece t with 2 degrees of freedom (xi held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 26. tie collapse (mean, sd held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 27. uncertified tie equality (mean, sd held) | numerical | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 28. snorm (mean, sd held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 29. right translated half-family (mean, sd held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 30. left translated half-family (mean, sd held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 31. tie collapse (mean, nu held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 32. uncertified tie equality (mean, nu held) | numerical | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 33. right translated half-family (mean, nu held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 34. left translated half-family (mean, nu held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 35. tie collapse (mean, xi held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 36. uncertified tie equality (mean, xi held) | numerical | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 37. snorm (mean, xi held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 38. two-piece t with 2 degrees of freedom (mean, xi held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 39. right translated half-family (sd, nu held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 40. left translated half-family (sd, nu held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 41. tie collapse (sd, xi held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 42. uncertified tie equality (sd, xi held) | numerical | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 43. snorm (sd, xi held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 44. tie collapse (nu, xi held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 45. uncertified tie equality (nu, xi held) | numerical | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 46. right translated half-family (mean, sd, nu held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 47. left translated half-family (mean, sd, nu held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 48. tie collapse (mean, sd, xi held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 49. uncertified tie equality (mean, sd, xi held) | numerical | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 50. snorm (mean, sd, xi held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 51. scale collapse (mean, nu, xi held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 52. scale-collapse equality (mean, nu, xi held) | numerical | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 53. snorm (fixed set unspecified) | numerical | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 54. hnorm (fixed set unspecified) | numerical | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 55. reflected translated half-normal (fixed set unspecified) | numerical | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 56. translated half-t (fixed set unspecified) | numerical | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 57. reflected translated half-t (fixed set unspecified) | numerical | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| sstd | 58. two-piece t with 2 degrees of freedom (fixed set unspecified) | numerical | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md); [analysis](2026-10-29-p2b-skew-report.md) |
| std | 1. scale collapse (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/std/README.md); [analysis](2026-10-29-p0-locscale.md) |
| std | 2. scale collapse (nu held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/std/README.md); [analysis](2026-10-29-p0-locscale.md) |
| std | 3. tie equality boundary (nu held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/std/README.md); [analysis](2026-10-29-p0-locscale.md) |
| std | 4. norm (fixed set unspecified) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/std/README.md); [analysis](2026-10-29-p0-locscale.md) |
| std | 5. lst (fixed set unspecified) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/std/README.md); [analysis](2026-10-29-p0-locscale.md) |
| tbinom | 1. point mass at lowest support point (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/tbinom/README.md); [analysis](2026-10-29-p0-binomial.md) |
| tbinom | 2. point mass at highest support point (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/tbinom/README.md); [analysis](2026-10-29-p0-binomial.md) |
| tbinom | 3. point mass at highest support point (prob held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/tbinom/README.md); [analysis](2026-10-29-p0-binomial.md) |
| tbinom | 4. tpois (none held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/tbinom/README.md); [analysis](2026-10-29-p0-binomial.md) |
| tbinom | 5. tpois (a held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/tbinom/README.md); [analysis](2026-10-29-p0-binomial.md) |
| tbinom | 6. tpois (b held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/tbinom/README.md); [analysis](2026-10-29-p0-binomial.md) |
| tbinom | 7. tpois (a, b held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/tbinom/README.md); [analysis](2026-10-29-p0-binomial.md) |
| tlambda | 1. scale collapse (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/tlambda/README.md); [analysis](2026-10-29-p0-locscale.md) |
| tlambda | 2. infeasible held support (sigma, lambda held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/tlambda/README.md); [analysis](2026-10-29-p0-locscale.md) |
| tnorm | 1. exp (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/tnorm/README.md); [analysis](2026-10-29-p0-skewreal.md) |
| tnorm | 2. exp (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/tnorm/README.md); [analysis](2026-10-29-p0-skewreal.md) |
| tnorm | 3. truncated exponential (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/tnorm/README.md); [analysis](2026-10-29-p0-skewreal.md) |
| tnorm | 4. uniform (mean held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/tnorm/README.md); [analysis](2026-10-29-p0-skewreal.md) |
| tpois | 1. point mass at lowest support point (b held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/tpois/README.md); [analysis](2026-10-29-p0-discrete2.md) |
| tpois | 2. point mass at highest support point (a held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/tpois/README.md); [analysis](2026-10-29-p0-discrete2.md) |
| trgamma | 1. fixed-endpoint power face (scale held) | argued | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/trgamma/README.md); [analysis](2026-10-29-p0-genbeta.md) |
| trgamma | 2. unbounded likelihood (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/trgamma/README.md); [analysis](2026-10-29-p0-genbeta.md) |
| trgamma | 3. lnorm (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/trgamma/README.md); [analysis](2026-10-29-p0-genbeta.md) |
| trgamma | 4. power (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/trgamma/README.md); [analysis](2026-10-29-p0-genbeta.md) |
| triang | 1. zero-width collapse (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/triang/README.md); [analysis](2026-10-29-p0-bounded.md) |
| triang | 2. incompatible held parameters (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/triang/README.md); [analysis](2026-10-29-p0-bounded.md) |
| unif | 1. zero-width collapse (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/unif/README.md); [analysis](2026-10-29-p0-bounded.md) |
| unif | 2. incompatible held endpoints (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/unif/README.md); [analysis](2026-10-29-p0-bounded.md) |
| weibull | 1. unbounded likelihood (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/weibull/README.md); [analysis](2026-10-29-p0-positive.md) |
| weibull | 2. unbounded likelihood (scale held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/weibull/README.md); [analysis](2026-10-29-p0-positive.md) |
| yulesimon | 1. point mass at one (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/yulesimon/README.md) |
| zeta | 1. point mass at one (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/zeta/README.md) |
| zib | 1. zip (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/zib/README.md); [analysis](2026-10-29-p0-binomial.md) |
| zib | 2. zip (pi held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/zib/README.md); [analysis](2026-10-29-p0-binomial.md) |
| zinb | 1. zip (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/zinb/README.md); [analysis](2026-10-29-p0-counts.md) |
| zinb | 2. pois (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/zinb/README.md); [analysis](2026-10-29-p0-counts.md) |
| zip | 1. pois (pi held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/zip/README.md); [analysis](2026-10-29-p0-counts.md) |
| zip | 2. undefined zero-rate reference (lambda held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/zip/README.md); [analysis](2026-10-29-p0-counts.md) |
| zipf | 1. point mass at one (N held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/zipf/README.md); [analysis](2026-10-29-p0-discrete2.md) |
| zmbinom | 1. zmpois (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/zmbinom/README.md); [analysis](2026-10-29-p0-binomial.md) |
| zmbinom | 2. zmpois (p0 held) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/zmbinom/README.md); [analysis](2026-10-29-p0-binomial.md) |
| zmnbinom | 1. zmpois (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/zmnbinom/README.md); [analysis](2026-10-29-p0-counts.md) |
| ztbinom | 1. ztpois (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/ztbinom/README.md); [analysis](2026-10-29-p0-binomial.md) |
| ztnbinom | 1. ztpois (fixed set unspecified) | proved | [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/ztnbinom/README.md); [analysis](2026-10-29-p0-counts.md) |
