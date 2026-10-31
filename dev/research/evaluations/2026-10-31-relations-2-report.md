# Family relations: immediate declarations

Date: 2026-10-31. Source base: `1a2feb0a2b5455ed8094cdbe4afb73ba320f442e` plus the uncommitted changes described here. No commits or git writes were made.

All 98 families were reviewed. The committed baseline has **60 edges**; the final immediate graph has **136**: **59 retained, 77 added, one old edge removed**. All 136 density checks and all 33 verifier regression tests pass. The R dev and site checks pass. There are 15 isolated families.

## Declaration policy

The previous 416-edge closure is rejected. Dossiers now contain hand-written immediate maps. Special cases use a transitive reduction; transform consequences through special cases remain implicit. No identity transforms, duplicate family/kind/transform arrows, or reverse inverse/negate pairs are declared. The graph page therefore renders 136 relation arrows rather than 416.

`just relations` enforces this policy before density verification. A breadth-first search finds alternative special-case paths. A second search tracks whether one transform of the same type has been used, allowing special-case chains on either side. These are family-level checks: changing coefficient-map spelling, a power, or a shift does not justify an additional arrow of the same type when that family connection is already derivable. Density verification separately checks the retained map. Errors include witness paths and make the command fail, even when the density identities would pass. Tests cover both chain directions, longer chains, duplicates and aliases, algebraic identities, inverse and negate pairs, cycles, and CLI failure.

Reciprocal pairs are declared on the inverse-family side: invexp→exp, invgamma→gamma, invweibull→weibull, invtrgamma→trgamma, invburr→burr, invparalogis→paralogis and invpareto→lomax. For the bounded power/Pareto pair the retained construction is power→pareto; its reverse is omitted. The principal uniform/Tukey-lambda member uses lambda=1; the lambda=2 alias remains a regression probe.

## Edges before and after

| Inventory | Count |
|---|---:|
| Committed baseline | 60 |
| Retained baseline edges | 59 |
| New immediate edges | 77 |
| Baseline edges removed as derivable | 1 |
| Final declarations | 136 |

The removed baseline edge is **naka transform_of gamma (power=2)**. It follows from **naka special_case_of trgamma** and **trgamma transform_of gamma (power=shape2)**. The trial nbinom→zmnbinom special-case shortcut was also omitted: nbinom→zinb→zmnbinom supplies the chain. Other machine-composed ancestors, identity transforms and reverse reciprocals from the rejected closure were discarded before selecting immediate candidates; none were regenerated into the dossiers.

## Added immediate edges

| Child | Parent | Kind / transform | Complete parent map | Verification |
|---|---|---|---|---|
| beta | genbeta | special_case | `shape1=shape1, shape2=shape2, shape3=1, scale=1` | symbolically |
| beta | nsbeta | special_case | `shape1=shape1, shape2=shape2, min=0, max=1` | symbolically |
| binom | tbinom | special_case | `size=size, prob=prob, a=-1, b=size` | numerically |
| binom | zib | special_case | `size=size, prob=prob, pi=0` | numerically |
| burr | pareto4 | special_case | `shape1=shape1, shape2=shape2, scale=scale, min=0` | symbolically |
| burr | lomax | transform/power; power=shape2 | `lambda=1/scale**shape2, kappa=shape1` | numerically |
| cauchy | lst | special_case | `mu=location, sigma=scale, df=1` | symbolically |
| dunif | bbinom | transform/shift; shift=min | `size=max-min, alpha=1, beta=1` | numerically |
| dunif | zipf | transform/shift; shift=min-1 | `N=max-min+1, s=0` | symbolically |
| exp | gpd | special_case | `mu=0, sigma=1/rate, xi=0` | numerically |
| exp | sgomp | special_case | `b=rate, eta=0` | symbolically |
| exp | laplace | transform/abs | `mu=0, sigma=1/rate` | numerically |
| frechet | gev | special_case | `mu=mu+sigma, sigma=sigma/lambda, xi=1/lambda` | numerically |
| gamma | naka | transform/power; power=0.5 | `shape=shape, scale=shape*scale` | numerically |
| gb2 | fpareto | special_case | `shape1=shape1, shape2=shape2, shape3=shape3, scale=scale, min=0` | symbolically |
| gb2 | betapr | transform/power; power=shape2 | `shape1=shape3, shape2=shape1, scale=scale**shape2` | numerically |
| genbeta | nsbeta | transform/power; power=shape3 | `shape1=shape1, shape2=shape2, min=0, max=scale**shape3` | numerically |
| geom | cmp | special_case | `lambda=1-prob, nu=0` | numerically |
| geom | zmgeom | special_case | `prob=prob, p0=prob` | symbolically |
| hcauchy | ht | special_case | `mu=mu, sigma=sigma, nu=1` | symbolically |
| hnorm | foldnorm | transform/shift; shift=mu | `mu=0, sigma=sigma` | symbolically |
| hnorm | naka | transform/shift; shift=mu | `shape=0.5, scale=sigma**2` | numerically |
| ht | fpareto | special_case | `shape1=nu/2, shape2=2, shape3=0.5, scale=sigma*sqrt(nu), min=mu` | numerically |
| invburr | gb2 | special_case | `shape1=1, shape2=shape2, shape3=shape1, scale=scale` | numerically |
| invburr | invpareto | transform/power; power=shape2 | `shape=shape1, scale=scale**shape2` | numerically |
| invexp | invweibull | special_case | `shape=1, scale=scale` | symbolically |
| invgamma | invtrgamma | special_case | `shape1=alpha, shape2=1, scale=beta` | numerically |
| invparalogis | paralogis | transform/inverse | `shape=shape, scale=1/scale` | numerically |
| invpareto | betapr | special_case | `shape1=shape, shape2=1, scale=scale` | numerically |
| invpareto | lomax | transform/inverse | `lambda=scale, kappa=shape` | numerically |
| invtrgamma | invgamma | transform/power; power=shape2 | `alpha=shape1, beta=scale**shape2` | numerically |
| invweibull | frechet | special_case | `lambda=shape, mu=0, sigma=scale` | numerically |
| invweibull | gumbel | transform/log | `mu=log(scale), sigma=1/shape` | numerically |
| kumar | beta | transform/power; power=a | `shape1=1, shape2=b` | numerically |
| laplace | ged | special_case | `mean=mu, sd=sqrt(2)*sigma, nu=1` | symbolically |
| lgser | zmlogarithmic | special_case | `prob=theta, p0=0` | numerically |
| lgser | ztnbinom | special_case | `size=0, prob=1-theta` | numerically |
| llogis | invburr | special_case | `shape1=1, shape2=shape, scale=scale` | numerically |
| llogis | pareto3 | special_case | `shape=shape, scale=scale, min=0` | numerically |
| logis | tlambda | special_case | `mu=location, sigma=scale, lambda=0` | numerically |
| lomax | betapr | special_case | `shape1=1, shape2=kappa, scale=1/lambda` | numerically |
| lomax | pareto2 | special_case | `shape=kappa, scale=1/lambda, min=0` | symbolically |
| naka | trgamma | special_case | `shape1=shape, shape2=2, scale=sqrt(scale/shape)` | numerically |
| nbinom | zinb | special_case | `size=size, prob=prob, pi=0` | numerically |
| nhyper | bbinom | transform/shift; shift=r | `size=(N-m)*min(1,r), alpha=max(1,r), beta=m-r+1` | numerically |
| norm | bhatt | special_case | `mu=mean, sigma=sd, a=0` | numerically |
| norm | ged | special_case | `mean=mean, sd=sd, nu=2` | symbolically |
| norm | tnorm | special_case | `mean=mean, sd=sd, a=-Inf, b=Inf` | numerically |
| nsbeta | genbeta | transform/shift; shift=min | `shape1=shape1, shape2=shape2, shape3=1, scale=max-min` | numerically |
| pareto | pareto2 | special_case | `shape=a, scale=b, min=b` | numerically |
| pareto | gpd | transform/log | `mu=log(b), sigma=1/a, xi=0` | numerically |
| pareto2 | gpd | special_case | `mu=min, sigma=scale/shape, xi=1/shape` | numerically |
| pois | cmp | special_case | `lambda=lambda, nu=1` | symbolically |
| pois | skellam | special_case | `mu1=lambda, mu2=0` | numerically |
| pois | skellam | transform/negate | `mu1=0, mu2=lambda` | numerically |
| power | nsbeta | special_case | `shape1=beta, shape2=1, min=0, max=alpha` | numerically |
| power | pareto | transform/inverse | `a=beta, b=1/alpha` | numerically |
| power | unif | transform/power; power=beta | `min=0, max=alpha**beta` | numerically |
| rayleigh | naka | special_case | `shape=1, scale=2*sigma**2` | numerically |
| snorm | sged | special_case | `mean=mean, sd=sd, nu=2, xi=xi` | numerically |
| std | lst | special_case | `mu=mean, sigma=sd*sqrt((nu-2)/nu), df=nu` | numerically |
| trgamma | gamma | transform/power; power=shape2 | `shape=shape1, scale=scale**shape2` | numerically |
| unif | gpd | special_case | `mu=min, sigma=max-min, xi=-1` | symbolically |
| unif | tlambda | special_case | `mu=(min+max)/2, sigma=(max-min)/2, lambda=1` | symbolically |
| unif | power | transform/shift; shift=min | `alpha=max-min, beta=1` | symbolically |
| weibull | exp | transform/power; power=shape | `rate=1/scale**shape` | numerically |
| weibull | gev | transform/negate | `mu=-scale, sigma=scale/shape, xi=-1/shape` | numerically |
| yulesimon | bnbinom | transform/shift; shift=1 | `size=1, alpha=rho, beta=1` | numerically |
| zinb | zmnbinom | special_case | `size=size, prob=prob, p0=pi+(1-pi)*prob**size` | numerically |
| zmgeom | zmnbinom | special_case | `size=1, prob=prob, p0=p0` | symbolically |
| zmlogarithmic | zmnbinom | special_case | `size=0, prob=1-prob, p0=p0` | numerically |
| ztbinom | zmbinom | special_case | `size=size, prob=prob, p0=0` | numerically |
| ztgeom | zmgeom | special_case | `prob=prob, p0=0` | numerically |
| ztgeom | ztnbinom | special_case | `size=1, prob=prob` | symbolically |
| ztgeom | geom | transform/shift; shift=1 | `prob=prob` | symbolically |
| ztnbinom | zmnbinom | special_case | `size=size, prob=prob, p0=0` | numerically |
| ztpois | zmpois | special_case | `lambda=lambda, p0=0` | numerically |

New-edge modes: 21 symbolic, 56 numerical. Whole graph: 32 symbolic, 104 numerical. Numerical agreement is regression evidence, not a proof over the entire parameter space. Symbolic equality proves the density identity where the expressions are defined; supports and boundary extensions require their stated conditions.

## Accepted, rejected and unsupported candidates


| Candidate | Disposition and reason |
|---|---|
| Poisson → Skellam at `(mu1,mu2)=(lambda,0)` and reflected Poisson → `(0,lambda)` | Added after specializing density and support. For integer k, I_abs(k)(2*sqrt(a*b)) is asymptotic to (a*b)**(abs(k)/2)/Gamma(abs(k)+1), giving the signed Poisson pmf on each zero-rate face and the point mass when both rates vanish. The verifier requires the recognized generic Bessel expression before applying this extension; tests compare small positive rates, check the zero corner and reject a changed generic density. Reflection reverses the integer endpoint-inclusion flags. |
| Normal → Bhattacharjee at `a=0` | Added after taking the exact analytic limit of the parent density; naive substitution gives `0/0`. |
| Logarithmic series → zero-truncated negative binomial; zero-modified logarithmic → zero-modified negative binomial | Added at size=0, with parent prob=1-theta / 1-prob. The analytic regularization uses Gamma(r)*(1-p**r) → -log(p); the zero-modified atom is preserved. |
| Zipf → Zeta | For **s>1** this is truncation at N, not a special case. Zipf permits `0<=s<=1`, where Zeta is not a probability law. No child-domain condition exists in relation syntax; no unconditional edge was declared. |
| Binomial → CMB at `nu=1` | Algebraically exact, but CMB declares `size<=10000000` and binomial has no such upper bound. The conditional edge needs a child-domain restriction. The verifier's seed neighbourhood alone would miss this issue. No dossier domains were altered. |
| Discrete uniform → CMB at `nu=0,prob=1/2,size=max-min`, shifted by min | Same size-domain obstacle. The uncapped beta-binomial (`alpha=beta=1`) and shifted Zipf (`s=0`) maps were added instead. |
| Negative hypergeometric → beta-binomial | Added including r=0: shift=r, size=(N-m)*min(1,r), alpha=max(1,r), beta=m-r+1. For positive integer r this is the usual shifted beta-binomial identity. For r=0 the map chooses size=0, alpha=1, beta=m+1, giving exactly the point mass at zero without an excluded shape. The verifier includes the dossier’s zero-stopping boundary and rejects invalid child draws such as r>m. |
| Geometric → discrete gamma / discrete Weibull | For `0<prob<1`, discrete gamma has `shape=1,rate=-log(1-prob)` and discrete Weibull has `shape1=1-prob,shape2=1`. At attained `prob=1`, the former needs infinite rate and the latter excludes `shape1=0`. Not whole-family edges. |
| Poisson / ZIP → zero-modified Poisson | True maps with p0=exp(-lambda) / pi+(1-pi)*exp(-lambda). Not declared: the R loader’s relation expression vocabulary excludes exp, and binding-loader edits are outside this job’s ownership. Add exp to that pure vocabulary in a separate binding change; no d/p/q/r change is needed. |
| Poisson → ZIP at `pi=0` | True for positive rate, but ZIP excludes the Poisson family's attained `lambda=0`. No unconditional edge. |
| Binomial → zero-modified binomial | The map `p0=(1-prob)^size` works for positive size; parent excludes the child's attained size zero. |
| Geometric / positive geometric as truncations of discrete Laplace | `location=0` / `1`, `scale=1-prob` works in the interior; `prob=1` requires the excluded discrete-Laplace scale zero. |
| Gompertz ↔ shifted Gompertz | Not the same family: Gompertz has super-exponential right tail `exp[-(a/b) exp(b*x)]`; shifted Gompertz has exponential right tail. Their names do not establish an exact map. Exponential is the shifted-Gompertz `eta=0` member (added); the Gompertz exponential path needs `b→0`, excluded by its domain. |
| Tukey lambda → normal / Cauchy | Normal approximation at a small positive shape and Cauchy-like tails at lambda=-1 are not exact family identities; the quantiles differ. |
| Uniform ↔ triangular | A nondegenerate triangular density is piecewise linear and cannot be constant. Endpoint-mode triangular members are affine beta(1,2) or beta(2,1), but neither entire triangular nor entire beta family is a special case of the other. |
| Reciprocal inverse-Pareto → Lomax with rate=1/scale | Rejected parameterization: inversion gives Lomax rate=scale, not its reciprocal. The corrected immediate map is declared; its reverse and inherited consequences are omitted. |
| Power → beta | `X/alpha` is beta(`beta`,1); arbitrary alpha prevents an unscaled special case. The scaled beta (`nsbeta`) edge and the power transform to uniform were added. |
| Standardized t → location-scale t, reverse | Forward map added: `df=nu,sigma=sd*sqrt((nu-2)/nu)`. Reverse would exclude location-scale t's `df<=2`. |
| Fréchet → GEV | Added with `mu_gev=mu+sigma,sigma_gev=sigma/lambda,xi=1/lambda`; positive-shape GEV support maps to the Fréchet threshold. |
| Normal / Laplace → Huber | Only limiting paths, not finite positive-epsilon members. Existing `limits` remain the authoritative asymptotic declarations. |
| Inverse Gaussian → inverse gamma | Only a limiting relationship when mean diverges (with inverse-gamma shape 1/2 and beta=shape/2); finite mean retains both x and 1/x terms in the exponent. |
| Uniform → Tukey lambda at lambda=2 | Exact uniform alias, tested but not declared. The principal lambda=1 map is retained as the single special-case arrow for this pair. |


## Proposals for additional syntax or kinds (not declarations)


- **Restricted edges / shared submodels:** attach a condition on child coefficients. This would express Zipf → Zeta (`s>1`), binomial → CMB (`size<=10000000`), shifted discrete uniform → CMB (width at most 10000000), geometric → discrete gamma/discrete Weibull (`prob<1`), geometric/positive geometric → discrete Laplace truncations (`prob<1`), Poisson → ZIP (`lambda>0`), and binomial → zero-modified binomial (`size>=1`). A shared-submodel representation could also connect endpoint-mode triangular to beta(1,2)/beta(2,1), and uniform to beta(1,1), Kumaraswamy(1,1) and power(shape=1) without pretending the entire child family has a fixed endpoint.
- **Independent sum/difference:** Skellam = Poisson(mu1) − Poisson(mu2); EMG = Normal(mu,sigma) + Exponential(lambda); Bhattacharjee = Normal(mu,sigma) + Uniform(-a,a); discrete Laplace = location + Geometric(1-scale) − Geometric(1-scale). Component independence and parameters must be recorded, including degenerate boundary components absent from nondegenerate catalog domains.
- **Mixing / random-scale constructions:** beta-binomial = Binomial(size,P), P~Beta(alpha,beta); beta-negative-binomial = negative binomial(size,P), P~Beta(alpha,beta); Poisson–inverse-Gaussian = Poisson(L), L~inverse Gaussian(mean,shape) in the positive-shape regime; negative binomial = gamma-mixed Poisson (positive size and nondegenerate probability); slash = mu + sigma*Z/U for independent standard normal Z and Uniform(0,1) U. Yule–Simon is also 1+Geometric(P), P~Beta(rho,1). A triangular law is a two-component mixture: weight (c-a)/(b-a) on nsbeta(2,1,min=a,max=c), and weight (b-c)/(b-a) on nsbeta(1,2,min=c,max=b); at endpoint modes the zero-weight collapsed component is omitted. For fatigue, X-mu is an equal mixture of inverse Gaussian(mean=beta,shape=beta/alpha**2) and the reciprocal of inverse Gaussian(mean=1/beta,shape=1/(beta*alpha**2)); the two kernels sum to the dossier factor (x-mu+beta). Existing zero-modification kinds already cover their own mixtures and need no new synonyms.
- **Discretization:** discrete normal = floor(Normal(mean,sd)); discrete gamma = floor(Gamma(shape,scale=1/rate)); discrete Weibull = floor(Weibull(shape2,scale=(-log(shape1))**(-1/shape2))). These change base measure and are neither truncations nor the allowed lattice transforms.
- **Reverse operators for existing transforms:** exp maps Normal(mean,sd) to lognormal(meanlog=mean,sdlog=sd), Gamma(shape,scale) to log-gamma(shapelog=shape,ratelog=1/scale), Logistic(location,scale) to log-logistic(shape=1/scale,scale=exp(location)), and Gumbel(mu,sigma) to inverse Weibull(shape=1/sigma,scale=exp(mu)). Inverse-logit maps Normal(mean,sd) to logit-normal(mu=mean,sigma=sd). These require new transform operators; the two exp-valued coefficient maps also need exp admitted by the R loader. They are proposals, not declarations.
- **General monotone / affine transforms and compositions:** Gompertz maps to Exponential(rate=a/b) by exp(b*X)-1, while Exponential(rate) maps to Gompertz(a=rate,b=1) by log(1+X); fatigue maps to standard normal by `(sqrt((X-mu)/beta)-sqrt(beta/(X-mu)))/alpha`; logit-normal maps to lognormal by X/(1-X); nonstandard beta maps to beta by `(X-min)/(max-min)`; generalized beta maps to beta by `(X/scale)**shape3`; beta maps to beta prime(shape1,shape2,scale=1) by X/(1-X); beta prime maps to beta by `X/(scale+X)`; GB2 maps to beta(shape3,shape1) by `(X/scale)**shape2/(1+(X/scale)**shape2)`; Pareto maps to exponential by `log(X/b)`; power maps to exponential by `-log(X/alpha)`; Weibull maps to exponential(rate=1) by `(X/scale)**shape`; inverse Weibull maps to exponential(rate=1) by `(scale/X)**shape`; GPD maps to exponential(rate=1) by `log(1+xi*(X-mu)/sigma)/xi`, continuously extended at xi=0. A general CDF transform would trivially connect every continuous family to uniform, so such extensions should be restricted to informative explicit constructions.
- **Joint conditioning:** hypergeometric(m,N,k) is the first of independent Binomial(m,p) and Binomial(N-m,p) counts conditioned on their sum being k, for any 0<p<1. This uses two catalog parents and a joint event, beyond one-parent support truncation.
- **Transformed truncation:** Gompertz is `-Y` conditional on `Y<=0`, with Y Gumbel(`mu=log(a/b)/b,sigma=1/b`). The current edge cannot combine reflection and conditioning. This is distinct from shifted Gompertz.
- **Maximum of independent variables:** shifted Gompertz at eta>0 is the maximum of Exponential(b) and Gumbel(`mu=log(eta)/b,sigma=1/b`); eta=0 is its already-declared exponential member.
- **Additional limits:** propose only where absent from current `limits`, and review the limiting regime separately; finite-parameter edges must not stand in for convergence. Candidates include Zipf → Zeta as N→infinity only for s>1, and the inverse-Gaussian → inverse-gamma limit above. No `limits` fields were edited in this task.


## Verifier mathematics and numerical corrections

The retained verifier corrections have focused regression tests:

- CMP at nu=0 or 1 resolves the declared infinite sum with SymPy `doit()` before floating evaluation. SymPy 1.14 `Sum.evalf(subs=...)` omitted the normalizer in the failing geometric probe (prob=1/2, x=6: -4.158883083359672 instead of -4.852030263919617). No child density is substituted as a shortcut.
- Zero-modified parents with identically zero p0 use positive support starting at one. A nonzero p0 retains zero in the support and fails a purported positive-support special case.
- Parent endpoints are simplified after applying the symbolic coefficient map and before floating substitution. This preserves the exact GEV endpoint mu-sigma/xi=0 for the reciprocal Weibull regression probe. Genuinely negative inverse supports still fail. That probe is retained as a test, without declaring a closure edge.
- Tukey lambda at zero uses its exact logistic quantile inversion; Bhattacharjee at zero width uses the analytic limit of the erf quotient. Truncated/zero-modified negative binomial at size zero uses Gamma(r)*(1-p**r)→-log(p), yielding the logarithmic series.
- Skellam zero-rate faces use the recognized Bessel expression and its small-argument asymptotic; tests compare small positive rates, both signs, the zero corner and rejection of an altered density. Integer endpoint inclusion reverses under negation.
- Negative hypergeometric at r=0 is the zero stopping time. Its immediate shifted beta-binomial map uses size=(N-m)*min(1,r), alpha=max(1,r), beta=m-r+1; for positive integer r this is the ordinary identity, and at zero it uses a size-zero parent with positive shapes.
- Sampling starts with the declared simulation seed, then rejects jittered values outside dependent child domains. Exactly 24 valid draws are required; inability to obtain them within 480 attempts fails explicitly. Tests retain the zero-stopping boundary and invalid-draw failure.

Coefficient maps are hand-simplified, with numeric constants stored as numbers. YAML key `N` is quoted because R YAML otherwise interprets it as a boolean. The old Nakagami/gamma and other derivable identities can still be used as numerical regression probes without adding graph arrows. No fitter, default, golden, d/p/q/r, schema, binding, or dossier prose was changed.

## Remaining isolated families

Counting only these four relation kinds, the specified base has 30 isolated families, not just the 18 named in the brief. The immediate graph has 15; limits and group membership do not count as relation edges. The following reasons concern the available exact relation kinds, rather than asserting that these laws have no mathematical connections.

- **cmb**: Binomial and discrete-uniform subfamilies need the CMB size cap; restricted edges are not expressible.
- **dgamma**: Floor-discretized gamma; the geometric intersection excludes its attained prob=1 boundary.
- **dlaplace**: Difference of independent geometrics; one-sided geometric truncations exclude prob=1.
- **dnorm**: Floor-discretized normal changes base measure; current transform kinds do not express it.
- **dweibull**: Floor-discretized Weibull; the geometric intersection excludes prob=1.
- **emg**: Independent normal-plus-exponential sum; component removal requires excluded zero/infinite parameters.
- **fatigue**: Exact normalizing monotone transform uses square roots, division and threshold; unavailable transform syntax.
- **gompertz**: Reflected, truncated Gumbel is exact; the current syntax cannot compose reflection with conditioning.
- **huber**: Normal and Laplace arise only through parameter limits, not finite positive epsilon.
- **hyper**: Without-replacement sampling differs from binomial; catalog intersections are restricted submodels or limits.
- **invgauss**: The natural GIG superfamily is absent; inverse-gamma connection is only a mean-to-infinity limit, and Poisson–inverse-Gaussian requires mixing syntax.
- **pig**: Poisson mixing with inverse Gaussian needs a mixing kind; its Poisson limit is not finite shape.
- **slash**: Normal divided by independent uniform needs a random-scale construction, absent from the schema.
- **triang**: Endpoint modes share affine beta submodels; the entire three-parameter family is neither beta nor uniform.
- **zeta**: Zipf truncation is exact only for s>1; unrestricted Zipf includes exponents for which Zeta is undefined.

## Validation

Final required commands ran in the foreground in one `/tmp/umvl-cpu/run4` allocation; the wrapper applies `nice -n 10`, one-thread BLAS/OpenMP settings and four build/test slots. Targeted checks used `/tmp/umvl-cpu/run`. `TMPDIR`, `JUST_TEMPDIR` and `XDG_RUNTIME_DIR` point to the worktree’s `build/tmp`. The existing site dependencies were copied with `cp -a` as authorized; no installation was performed.

| Check | Outcome |
|---|---|
| `just relations` | 136 edges; zero failures; immediate-declaration policy passes. |
| `PYTHONPATH=dev/validation python3 -m unittest quality.test_relations` | 33 tests; OK. |
| `just test` | **FAIL 0, WARN 0, SKIP 151, PASS 18887**; 150 full-tier exclusions and one missing optional `{sn}` reference comparison. |
| `just site-check` | **Passed**: 98 family pages and C++17 snippets; 98 graph nodes and 240 edges (136 relations plus 104 limits); deterministic SVG, resolving links and MathML checks. |
| Dossier scope / quoted-number checks | **Passed** for all 98 dossiers: other front matter and exact prose unchanged; 50 relation fields changed; no quoted numeric maps/arguments; `git diff --check` clean. |

Numerical constants: seed 20261031, 24 accepted draws per edge, relative log-density tolerance 2e-9, 35-digit conditional-mass evaluation. Continuous endpoint inclusion is compared modulo null sets; integer endpoints are exact.

Logs: `build/scratch/immediate-final-relations.log`, `immediate-unit.log`, `immediate-final-r.log`, `immediate-final-site.log`, `immediate-scope.log`; edge-level results and complete retained maps are in `build/quality/relations.json`. These ignored files support the durable maps, method and outcomes recorded here.

Environment: Linux 7.0.0-38 / x86_64 / glibc 2.43; Python 3.14.5, SymPy 1.14.0, PyYAML 6.0.3; R 4.6.1 (2026-06-24); Node v22.22.1; GCC/C++ 15.2.0. All final build/test commands exited zero. No separate C++ suite or R full-tier suite was run, per the job override.

Regeneration still emits the existing roxygen inline-code diagnostic for the literal `r = 3` default in generated negative-hypergeometric Details. This is separate from the zero testthat warning count. The unchanged template at `bindings/r/scripts/codegen_wrappers.R:156` should render that default as literal code without triggering inline-R evaluation; settled generator code was left untouched. The optional `{sn}` reference package was not installed. These are recorded coverage/documentation limitations, not failed test results.

No git write, commit, publication, board edit, package installation, simbox use, or edit to another worktree was performed. The report reuses the candidate decisions and proposals from `build/scratch/relations-2-report-v1.md`, with dispositions updated for the immediate graph. Future syntax proposals must also respect the immediate-declaration policy; they do not authorize reverse or transitive arrows.

The artifact-pane `show` command returned HTTP 403 because this worktree is outside the viewer’s configured readable roots. The completed report is at the requested path; no viewer settings or other worktree were changed.
