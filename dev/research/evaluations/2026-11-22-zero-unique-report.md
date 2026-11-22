# TASK-227 — Hold-aware checks for flat maxima

Strict now checks 165 additional samples per precision mode. The negative hypergeometric replay caught all five false uniqueness claims on b6a00871 and passed all 12 zero-sample cases on 1cfbc3dd. The wider replay found 92 violations per mode. These include 10 incorrect uniqueness flags, seven stops on attained maxima, and 75 estimates with incomplete payloads. Both C++ modes, full R and JS passed on the workstation.

## Inventory

The audit used each dossier's likelihood and fitted domain, the golden values held by the snapshot, and additional holds that expose conditional or boundary maxima. For observations all equal to $x$, the sample likelihood is a power of the mass or density at $x$. Let $B=2^{53}-1$. Intersect each set below with the holds and the fitted domain. “By slice” means TRUE exactly when the resulting set is a singleton. Probability-one sets have total log likelihood zero.

### Urn families

| Family | Class | Holds | Maximizer set and proof | Expected unique |
|---|---|---|---|---|
| nhyper | zeros | None or $r=0$ | $(m,N,0)$ with $0\le m\le N\le B$. Only $r=0$ permits stopping before any draw, and it does so with probability one. | FALSE |
| nhyper | zeros | $m=m_0$, with $r$ free or held at zero | $(m_0,N,0)$ for $m_0\le N\le B$, by the same zero-stopping argument. | TRUE iff $m_0=B$ |
| nhyper | zeros | $N=N_0$, with $r$ free or held at zero | $(m,N_0,0)$ for $0\le m\le N_0$. | TRUE iff $N_0=0$ |
| nhyper | zeros | $m,N$, with $r$ free or held at zero | The held $m,N$ with $r=0$. | TRUE |
| nhyper | constant $x>0$ | Free or holds admitting an atom | $m=N\ge x,\ r=x$. For $r>0$ the support runs from $r$ to $N-m+r$, so it is a singleton exactly when $m=N$. | By slice |
| hyper | zeros | Free or holds admitting an atom | $m=0$ or $k=0$, with $0\le m,k\le N\le B$. Positive composition and draw counts give positive probability of a success. | By slice |
| hyper | constant $x>0$ | Free or holds admitting an atom | $m=N,\ k=x$, or $k=N,\ m=x$. A positive count is deterministic when the population is all successes or the draw exhausts it. | By slice |
| hyper | constant $x=1$ | $N=5,k=2$ or $N=5,m=2$ | Free count 2 or 3, each with mass $3/5$. The adjacent composition ratio crosses one at $m+1=x(N+1)/k$. Swap $m,k$ for the other case. | FALSE |
| hyper | constant $x=1$ | $m=k=2$ | $N=3,4$, each with mass $2/3$. The adjacent population ratio minus one has sign $mk-x(N+1)$. | FALSE |
| nhyper | constant $x=2$ | $N=5,r=1$ | $m=2,3$, each with mass $3/10$. The adjacent composition ratio minus one has sign $r(N+1)-x(m+1)$. | FALSE |
| nhyper | constant $x=2$ | $m=2,r=1$ | $N=3,4$, each with mass $1/3$. The adjacent population ratio minus one has sign $xm-r(N+1)$. | FALSE |
| nhyper | constant $x=3$ | $m=2,N=5$ | $r=1,2$, each with mass $1/5$. Draw three succeeds with probability $2/5$. Given that success, the other success is equally likely before or after it. | FALSE |

The nhyper zero cases use $m_0=2$ and $N_0=5$ for the interior slices, and repeat the held $N=0$ and $m=B$ controls with $r$ held and free. The golden $N=50,r=3$ makes zeros infeasible. Its $x=7$ gives one composition maximum because $3(51)/7$ is nonintegral. For hyper, the golden $N=50,k=10$ gives the unique zero maximizer $m=0$ and the unique positive-constant maximum at $x=4$ since $4(51)/10$ is nonintegral. The feasible held-$B$ hyper controls force $N=B$ and the other draw or composition count to zero. With both $m,k>0$ held, the zero mass increases strictly in $N$, which rules out a flat set. For nhyper with only $r<x$ held, $\Pr(T=x)$ equals $r/x$ times the probability of exactly $r$ successes in the first $x$ draws. Its unique maximum occurs at $N=x,m=r$. [Negative hypergeometric predecessor](2026-11-22-nhyper-zero-report.md) · [Urn and triangular payloads](2026-11-22-hyper-flags-report.md)

### Other count families

| Family | Class | Holds | Maximizer set and proof | Expected unique |
|---|---|---|---|---|
| binom | zeros | Free or coordinate holds | $n=0$ or $p=0$ by the zero mass $(1-p)^n$. | By slice |
| binom | constant $x>0$ | Free | $n=x,p=1$, the only positive-count atom. | TRUE |
| binom, cmb | constant $x=1$ | $p=1/2$, and $\nu=1$ for cmb | $n=1,2$ each gives mass $1/2$. The adjacent size ratio crosses one at $p=x/(n+1)$. | FALSE |
| bbinom | zeros | Free or size zero, with optional shape holds | $n=0$ with the permitted positive shapes. A beta mixture has zero mass below one for $n>0$. | By slice |
| bbinom | constant $x=1$ | $\alpha=1,\beta=2$ | $n=1,2$ each gives mass $1/3$. The size ratio minus one has sign $x(\beta-1)-\alpha(n+1-x)$. | FALSE |
| cmb | zeros | Free, golden size 10, or other coordinate holds | $n=0$ or $p=0$ with the permitted dispersions. These are the zero atoms. | By slice |
| cmb | constant $x=3$ | Free or slices admitting an atom | $n=x,p=1$ and the permitted dispersions. Interior probabilities put positive mass elsewhere. | By slice |
| cmb | zeros or constant $x=1$ | $n=1,p=1/2$ | Every dispersion. Both binomial coefficients equal one. | FALSE |
| cmp | zeros | Free, $\lambda=0$, or $\nu=1$ | $\lambda=0$ and all permitted $\nu\ge0$. Only this rate gives mass one at zero. | By slice |
| nbinom | zeros | Free or coordinate holds | $n=0$ or $p=1$. At positive size the zero mass is $p^n$. | By slice |
| pig | zeros | Free or coordinate holds | Shape zero and every positive mean. The admitted zero-shape law is the atom at zero. | By slice |
| dweibull | zeros | $q=1/2$ | Every $\beta>0$. The zero mass is $1-q$, independent of shape. | FALSE |
| skellam | zeros or constant $x=1$ | Free | $(0,0)$ for zero and $(1,0)$ for one. Conditioning on the subtracted count bounds the mass strictly below the peak Poisson mass at $x>0$ whenever the subtracted rate is positive. Reflect for $x<0$. | TRUE |

With size 10 held, bbinom has an excluded supremum under the golden holds. Mixing strictly over $p\in(0,1)$ misses the maximum pure-binomial mass. For the interior constant at golden CMB size, increasing dispersion while tilting canonical odds can concentrate on the observation. CMP can likewise concentrate at $x>0$ by taking $\log\lambda/\nu$ between $\log x$ and $\log(x+1)$ as $\nu$ increases. The free positive constants in nbinom, pig and bnbinom have a Poisson supremum because nondegenerate Poisson mixing misses the peak mass at the common count. The remaining scalar nbinom profile with a compatible size or interior probability held is strictly concave for positive counts. Free $q$ in dweibull excludes the zero atom at $q=0$.

### Private modifier routes

| Route | Class | Holds | Maximizer set and proof | Expected unique |
|---|---|---|---|---|
| zib, zinb | zeros | Free or slices | $\pi=1$ or any base zero atom, from $\pi+(1-\pi)P_{\rm base}(0)$. | By slice |
| zip | zeros | Free, $\pi=1$, or rate held | $\pi=1,\lambda>0$. The ZIP rate domain excludes zero. | By slice |
| zmbinom, zmgeom, zmlogarithmic, zmnbinom, zmpois | zeros | Free, $p_0=1$, or $p_0=1/2$ | Every admissible positive-component parameter. Free $p_0$ maximizes at one. With $p_0$ held the log likelihood is $4\log p_0$. | FALSE with a free base coordinate |
| ztbinom, zmbinom | constant $x=1$ | Free, $n=1$, $p=0$, or $p=1/2$ | $n=1$ or $p=0$ gives the positive-conditioned atom at one. In zmbinom set $p_0=0$. | By slice |
| ztnbinom, zmnbinom | constant $x=1$ | Free or $p=1$ | $p=1$ and every permitted size. The positive-conditioned law includes that atom also at size zero. In zmnbinom set $p_0=0$. | FALSE |
| tbinom | zeros or constant $x=3$ | Singleton window, size 12 or free | Conditioning gives mass one for every parent with positive event mass. At $x=3$ this is $n\ge3,\ 0<p<1$ plus $n=3,p=1$. At $x=0$ this is $n\ge0,\ 0\le p<1$ plus $n=0,p=1$. | FALSE |
| tpois | zeros or constant $x=3$ | Singleton window | All positive rates, and also rate zero at $x=0$. Conditioning gives mass one. | FALSE |
| zipf | constant $x=1$ | $N=1$ | Every finite exponent in the signed window. The normalizer cancels the single term. | FALSE |

The golden binomial and Poisson windows exclude zeros and have interior positive medians. With binomial size fixed, or in Poisson, the conditional likelihood on a window with at least two support values is strictly concave in its natural coordinate. The golden ZIPF constant 2 has an infinite-exponent supremum. The truncated normal can concentrate even within a nonempty finite interval. All four positive-truncated routes exclude zeros. At one, ztgeom, ztpois and their corresponding hurdles choose a unique positive-component atom parameter. The logarithmic hurdle similarly selects $\theta=0$. The larger golden binomial constants choose size $x$ and $p=1$. The larger NB positive-conditioned constants select Poisson limits. For a positive sample the zero-inflation factor strictly decreases in its free weight, and the golden zinb ones select a Poisson limit.

### Continuous families

| Family | Class | Holds | Maximizer set and proof | Expected unique |
|---|---|---|---|---|
| gompertz | zeros | $a=1$ | Every $b>0$ because $f(0)=a$. | FALSE |
| gpd | zeros | $\mu=0,\sigma=1$ | Every fitted $\xi\ge-1$ since $f(\mu)=1/\sigma$. | FALSE |
| gpd | zeros | $\sigma=1,\xi=-1$, or only $\sigma=1$ | At $\xi=-1$, all $\mu\in[-1,0]$. With shape free, also all $\xi\ge-1$ at $\mu=0$. The density in this fitted domain is at most $1/\sigma$. At a strictly positive excess equality requires the uniform member. | FALSE |
| gev | zeros | $\mu=0,\sigma=1$ | Every $\xi\in[-1,1]$. At zero standardized residual $f=1/(e\sigma)$ including the Gumbel member. | FALSE |
| sn, st | zeros | $\xi=0,\omega=1$, also $\nu=1$ for st | Every finite $\alpha$. The skew multiplier is $2F(0)=1$. | FALSE |
| genbeta | constant $x=1/2$ | $q=1,b=1$ | $pa=1/\log 2$ for positive $p,a$. The power density $pa(x/b)^{pa}/x$ is strictly maximized in the product. | FALSE |
| tlambda | zeros | $\sigma=2$ and $\lambda=1$ or 2, or only $\sigma=2$ | In each uniform member, $\lvert\mu\rvert\le\sigma/\lambda$. With shape free, only $\lambda=1$ attains the bound. For $\lambda<1$, convexity of $u^{\lambda-1}$ gives the peak bound $2^{\lambda-2}/\sigma$. | FALSE |
| tlambda | zeros | $\sigma=3,\lambda=3/2$ | $\mu=\pm2$. The reciprocal of $\sigma(\sqrt u+\sqrt{1-u})$ is largest at either support contact. | FALSE |
| sged | zeros | Mean 0, SD 1, $\nu=9/4$ | Two reciprocal skews, one maximum on either side of unit skew by the centered profile below. | FALSE |
| triang | balanced pair $(0,1)$ | Free | $(a,b,c)=(0,3/2,0)$ and $(-1/2,1,1)$. The mode profile is maximized at an observation. For mode zero the optimal lower endpoint is zero and $4(b-1)/b^3$ is maximized at $b=3/2$. Reflection gives the second triple. | FALSE |
| triang | balanced pair $(1/4,3/4)$ | $a=0,b=1$ | Modes $1/4,3/4$. The pair likelihood between them is proportional to $1/[c(1-c)]$. Outside the pair the likelihood increases toward the observed range. | FALSE |

In centered SGED, put $A=1-\Gamma(2/\nu)^2/[\Gamma(1/\nu)\Gamma(3/\nu)]$, $C=[\Gamma(2/\nu)/\Gamma(1/\nu)]^\nu$ and $u=1-\min(\xi,1/\xi)^2$. The log density relative to unit skew is $\frac12\log(1-u+Au^2)-\log(1-u/2)-Cu^\nu$. For $u>0$ the derivative has the sign of $K-H(u)$, with $K=(4A-1)/(2\nu C)$ and $H(u)=u^{\nu-2}(2-u)(1-u+Au^2)$. Write $P(u)=(2-u)(1-u+Au^2)$ and $Q(u)=(\nu-2)P(u)+uP'(u)$, which controls the sign of $H'(u)$. The interval calculation gives $Q(0)>0$, $Q(1)<0$, $Q''>0$ on $[0,1]$, and $0=H(0)<K<H(1)$. Therefore $H$ rises and then falls and crosses $K$ exactly once. The crossing in $(0.0008,0.001)$ supplies the reciprocal maxima. The sign bounds establish the set, and the numerical root is used only for its coefficient comparison. [SGED interval check](2026-11-22-zero-unique-data/sged_profile.py) · [Sign margins](2026-11-22-zero-unique-data/sged-profile.txt)

### Remaining families and golden slices

| Families | Disposition and relevant hold changes |
|---|---|
| bhatt, cauchy, emg, foldnorm, frechet, ged, gumbel, hnorm, huber, laplace, logis, lst, norm, sgomp, slash, snorm, sstd, std, unif | A concentration or zero-width path makes both golden classes unbounded. Holding scale in an ordinary location-scale law with a strictly unimodal density leaves one location maximum. With a held nonzero residual, the ordinary normal, logistic, Cauchy, Laplace and fixed-shape Student scale profiles have one maximum. |
| beta, betapr, burr, expweibull, fatigue, fpareto, gamma, gb2, gengamma, invburr, invgamma, invgauss, invparalogis, invtrgamma, invweibull, kumar, lgamma, llogis, lnorm, logitnorm, naka, nsbeta, paralogis, pareto, pareto3, pareto4, power, trgamma, weibull | Zero is excluded by the golden support or endpoint fitting condition. The positive constant admits the dossier concentration path. Scalar beta and gamma shape fits with the other coefficient held have strictly decreasing scores. Simple Pareto and power endpoint slices either concentrate or pin the free endpoint at the observation. |
| exp, hcauchy, invexp, rayleigh | The positive constant has the unique one-coordinate maximum. For zero, exp and fixed-threshold hcauchy diverge as scale shrinks. Invexp and Rayleigh have no positive likelihood there. |
| ht, invpareto, lomax, pareto2 | Positive golden constants have half-normal, inverse-exponential or exponential suprema. Zeros are outside the fitting domain or cause unbounded concentration. |
| bnbinom, dgamma, dlaplace, dnorm, dunif | The first four have excluded point-mass or Poisson limits in the golden cases. Held discrete Laplace scale leaves the single integer median. Held normal scale selects the observed-cell midpoint and held mean gives collapse or a single peak of the Gaussian interval mass. For dgamma with a held rate and positive cell, the shape Hessian is the conditional variance of log gamma minus its unconditional variance. Log concavity of the log-gamma density makes interval conditioning a strict quantile contraction. The discrete uniform has the unique tightest feasible interval. |
| geom, lgser, pois, yulesimon, zeta | Geometric and Poisson have the unique solutions $p=1/(1+x)$ and $\lambda=x$. The other three exclude zero. The logarithmic golden constant 2 has a unique mean match, while ones approach the excluded zero parameter. Yule–Simon and zeta have infinite-parameter suprema at their golden constant of one. |
| pgw | The dossier has a density and no fitter. At a repeated positive value the free density has a shape-spike path with scale set to that value. There is no snapshot route. |

The original golden holds for the continuous families in the flat table leave concentration or support restrictions, except for the positive constant GPD slice. With threshold below $x$ and shape $\xi>-1$, the best scale is $x-\mu$. Its density there is strictly below the uniform value $1/(x-\mu)$, which is attained uniquely at $\xi=-1,\sigma=x-\mu$. Fully held feasible slices generally supply a single coefficient vector.

## Snapshot and rule

The 112 public and 53 private-route records per mode use the appended prefixes zeros-flat-, constant-flat-, and pair-flat-. The table fixes the sample, holds, maximizer set and maximized log likelihood before fitting. The snapshot probe takes the estimate diagnostic before the renderer clears it and retains it in the added records. [Expected sets](../../validation/flat_maxima.py) · [Snapshot driver](../../validation/fit-snapshot.py)

The flat-maximum rule requires a successful estimate in the proved set at the expected likelihood. It checks both serialized uniqueness flags. A non-singleton also needs a nonempty diagnostic and two distinct admissible candidate rows. Set membership establishes equal likelihood. For numerically represented isolated maxima the rule counts branches, so two approximations on the same branch count as one witness. The nhyper zero cases also require certification. [Rule](../../validation/fit-consistency.py) · [Rule list](../../validation/README.md)

## Before and after

| Scope | Native | Double-only |
|---|---|---|
| b6a00871, 12 negative hypergeometric zero cases | Five wrong TRUE flags and one additional payload failure | Same |
| 1cfbc3dd, identical zero cases | 12/12 passed | 12/12 passed |
| 1cfbc3dd, all added cases | 73/165 passed, 92 findings | Same keys and reasons |

The five old false claims were no holds, held $r=0$, held $m$, held $m,r=0$, and held positive $N$ with $r$ free. Holding positive $N$ and $r=0$ together already gave FALSE. Its old warning supplied no maximizer rows or outcome diagnostic, so the full rule flags that sixth case separately. The historical source was reconstructed with git archive b6a00871 in ignored scratch and fitted using the current snapshot driver and table. [Before](2026-11-22-zero-unique-data/strict-before.txt) · [Current zero-only run](2026-11-22-zero-unique-data/strict-after-nhyper.txt)

The current positive-constant nhyper slice with $m=2,r=1$ reports uniqueness although $N=3,4$ tie. Held-$B$ hyper controls incorrectly report non-uniqueness. Free-size binom and bbinom miss adjacent ties. The truncated and modified binomial controls with probability $1/2$ held correctly report a singleton. GEV, Gompertz and the three Tukey uniform slices give false uniqueness claims. The GPD, SGED, CMB adjacent-size and Tukey support-contact cases stop on attained maxima. The all-free beta-binomial zero slice stops at its structural dispatch exception. The remaining cases are missing candidate rows or diagnostics. Both triangular pairs and the skew-shape plateaus in sn and st passed. [Every finding by case and mode](2026-11-22-zero-unique-data/strict-current.txt)

## Snapshot preservation and suites

The comparison against the four frozen files preserved the order, labels, holds and serialized inputs of 11,905 public and 1,950 private records per mode. The focused runs replayed 132 old results per mode. The only difference from the older frozen results was the inherited triangular two-point candidate and warning change. A larger native run replayed 2,271 old results across 16 public families and found 39 differences against the older freeze. The original driver running the same 1cfbc3dd fitters reproduced all 39 exactly, including the triangular flag changes. The direct diff_baseline.py runs and the input check agree on the added keys and the inherited changes within their selections. [Comparison and new keys](2026-11-22-zero-unique-data/snapshot-comparison.txt) · [Comparison script](2026-11-22-zero-unique-data/check.py) · [Original-driver replay](2026-11-22-zero-unique-data/legacy-replay.txt)

| Check | Machine | Result |
|---|---|---|
| just test-changed --no-long-double, shared selection of 85 families | Workstation, GCC 15.2, four slots | Native C++ 134/134 and double-only C++ 179/179. |
| just test-full, for the shared runner's R stage | Workstation after simbox exit 75 | Truth checks 36/36. R had 66,181 passes, zero failures or warnings, and the existing ZIPF skip. |
| just js-build and just js-test | Workstation | Build passed and tests passed 587/587. |
| Snapshots, strict replays, comparisons and SGED interval check | Workstation, single slots | Both precision modes completed. The wide replay returned the findings above. |
| python3 dev/tools/check_links.py | Workstation | Zero new broken links. |

A scratch dispatcher selected the full R tier once because the shared test-changed path otherwise selects dev. The final Tukey scale-only addition needed its two snapshots and the affected strict and comparison checks. The final strict replay also verified the corrected singleton expectations for the two held-probability binomial modifier controls. Those cases reused the recorded fits because the inputs and serializer were identical. Importing the snapshot driver from an older-style replay layer with a symlinked source tree also passed. [Suite summary](2026-11-22-zero-unique-data/suite-results.txt)

## Open

- The owning fitters and adapters still have the recorded 92 cases per mode. Free-threshold GPD and all-free beta-binomial include finite cases behind structural dispatch exceptions. SGED supplies candidates in an unresolved stop even though the scalar profile and reflection prove equality.
- General free-size CMB and truncated binomial cases away from the listed slices still need exact comparison of equal design ordinates. The ordinary binomial and beta-binomial adjacent ratios settle the tested cases. The existing finite likelihood margins alone cannot distinguish exact equality.
- Alternative holds that leave coupled continuous profiles still need global shape bounds to exclude incidental equal-height maxima. In particular, the SGED centered proof does not classify its nonnormal held-moment profile away from the center, and the skew Student examples do not classify all its held-scale nuisance profiles.
