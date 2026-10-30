<!-- Research report by Codex (gpt-6.1-sol, low), 2026-10-28; reviewed by Claude. Input to decision D1 follow-ups (which limit models to add) and D6 (GED/SGED shape bound). -->

# Should univariateML fit its limit models?

Research, 2026-10-28. Changed **codex-report.md only**; read CLAUDE.md and both background notes. No package edits, builds, test suites, board operations, or Git writes. **Tests: 0 run**, as requested. Used **10 web search queries**; remaining web calls opened sources. Numerical checks: one Python process, BLAS/OpenMP/MKL pinned to one thread, under `nice -n 10`.

**Recommendation:** add a translated half-normal and a scale-parameterised Student t with an explicit df floor and optional fixed df; do not add a public fitter for every skew-family face. For D6 adopt **shape >= 1**, explicitly as a constrained model, and retain D1 messages for unattained uniform/half-law limits. A message may name a distribution without inventing a nonexistent `mlX`.

## Conventions and existence

These conclusions assume finite nonconstant data. Ties require the checks below; constant samples have scale-collapse degeneracy. Choose the continuous extension of a half-density at its endpoint, and include that endpoint in support: MLEs at order statistics depend on this density-version convention. Right and left orientations should be explicit API choices; if both are searched, compare both fits and permit ties.

Write an ordinary t as `t_df((x-c)/s)/s`, with **s a scale**, not sd. Write GED as `exp(-|(x-c)/a|^p)/(2*a*Gamma(1+1/p))`; p is the package's nu, and sd = `a*sqrt(Gamma(3/p)/Gamma(1/p))`. Two-piece densities use mode c, scales a,b, and density `2*g((x-c)/a)/(a+b)` on the left, replacing a by b on the right. These coordinates remain meaningful with infinite variance.

| Limit model | MLE existence, uniqueness and computation |
|---|---|
| Translated half-normal | For a specified right orientation, **unique**: `c=min(x)`, `s²=mean((x-c)²)`; reflect for left orientation. The density decreases away from c, so moving c toward the nearest observation improves every factor. Two orientations can tie. Closed form; standard half-normal with an estimated endpoint. |
| Translated half-t, free df | Same endpoint rule. At fixed df, solve `sum((df+1)*y²/(df*s²+y²))=n`, y the endpoint distances. The left side strictly decreases from `(df+1)*(n-k)` to 0; a unique positive scale exists iff **df > k/(n-k)**, k endpoint ties. Below it likelihood is unbounded; equality needs separate treatment. With a closed df floor strictly above this threshold, the likelihood is bounded and attains a maximum after including df=infinity (half-normal). Finite-df existence is not automatic; no uniqueness claim for the joint df fit. Profile scale, then globally search one df coordinate and compare the half-normal. For the actual sstd face use **df >= 2**, not arbitrary df>0. |
| Location-scale t, free df including <=2 | **df>0 unrestricted is ill posed**: fix c at a value occurring k times and let s shrink; `L ~ s^[df*(n-k)-k]`. A sufficient floor is **df_min > k_max/(n-k_max)**. For distinct n>=2 this is df_min>1/(n-1); practical **df>=1 with n>=3 and k_max<n/2** admits Cauchy and t2. df>=2 covers distinct n=2 too. Enforce the tie condition rather than silently changing the model with n. On this guarded domain, compactifying df by the Gaussian member gives attainment; finite df can still escape to normal. No general joint-df uniqueness guarantee or closed form; use df profiling with nuisance optimisation/EM and boundary comparison. |
| Two-piece t with df=2 | No shape-zero issue. Bounded under **k_max<2n/3**; a,b>0 still need not attain their supremum because one side can vanish. The closure including both translated half-t2 faces has a maximizer. No general uniqueness guarantee. Optimise c,a,b with df fixed, compare both analytic-endpoint half-t2 fits; no general closed form. A public fitter would itself need D1 half-law messages. |
| Translated half-GED, free p | Endpoint is min/max. At fixed p>0, **unique scale** `a^p=p*mean(y^p)`. With unrestricted p>0, the endpoint observations create the **same unbounded shape-zero profile** as GED. Any fixed p_min>0 removes this divergence; attainment in the closure additionally needs the uniform member at p=infinity. No general uniqueness guarantee in p. For p>=1, profile shape and compare p=1 (translated exponential) and the uniform limit. |

The threshold statements are direct likelihood derivations, not claims that an optimiser certifies a global maximum. Equality in the collapse exponent can give a finite, unattained collapse limit or nonuniqueness: do not accept it merely because the likelihood is not visibly diverging. The bounds prevent collapse; they do not remove Gaussian or half-law escapes. Hasannasab et al. [2] discuss algorithms and Gaussian limits, but their Section 3 existence theorem fixes the location: it is **not** a theorem for unrestricted free-location/free-df fitting.

## R evaluation functions and recognisability

| Model | Verified evaluation route and parameters |
|---|---|
| Translated half-normal | extraDistr `d/p/q/rhnorm`, `sigma=s`, endpoint zero [3]. Translate by c, and reflect for left orientation; c is **not** supplied by those functions. Familiar named law; estimating its endpoint is less usual than fixing it at zero. |
| Translated half-t | extraDistr `d/p/q/rht`, `nu=df`, `sigma=s`, endpoint zero [4]; translate/reflect externally. Familiar especially as a Bayesian scale prior, but that usage does not justify free-df MLE existence. |
| Location-scale t, including t2 | extraDistr `d/p/q/rlst(df,mu,sigma)` [5], or gamlss.dist `d/p/q/rTF(mu,sigma,nu)` with sigma a scale and nu>0 [6]. Base R central `dt/pt/qt/rt` plus affine transforms also suffices. Very standard; fGarch's variance-standardised `std` cannot represent df<=2. |
| Two-piece t2 | Author-hosted **twopiece v1.0 manual (2014)** documents `d/p/q/rtp4` with delta=2, `FUN=dt/pt/qt/rt`, `param="tp"`, mu=mode, par1=a, par2=b [7]. This establishes an implementation, not current CRAN availability. Recognisable to two-piece-distribution users, considerably less standard than Student t. |
| Translated half-GED | No dedicated half-GED implementation verified in this search. Compose gamlss.dist PE2 or fGarch GED: for y>=0, density `2*f(y)`, CDF `2*F(y)-1`, quantile `Q((1+u)/2)`; then translate/reflect. Equivalently `Y=a*G^(1/p)`, G~Gamma(1/p,1). A generalized-gamma special case; “translated half-GED” is not a widely standard standalone family name. |

Evaluation availability is separate from fitting: none of the extraDistr functions above estimates the endpoint or supplies these MLE algorithms. New fitters still need the repository's dossier, dpqr, metadata, R adapter, numerical validation and documentation; the closed-form half-normal has low mathematical effort but the usual integration work. A guarded t is moderate/high effort because global df/boundary handling matters more than evaluating the density. Half-t is moderate; two-piece t2 is high for a narrow family; bounded half-GED is low/moderate mathematically but adds a niche API.

## D6: a scientifically meaningful shape bound

For GED put `M_p(c)=mean(|x-c|^p)`. Profiling a gives

`ell(c,p)/n = -log(2)-log Gamma(1+1/p)-[log(p*M_p(c))+1]/p`.

At c equal to k observations, `log M_p = log((n-k)/n)+O(p)`, yielding the positive `-log((n-k)/n)/p` divergence as p->0. This is Tumlinson's obstruction [1]. It applies to translated half-GED (omit log 2) and SGED, which contains GED. Mean/sd standardisation cannot repair it.

**Any fixed p_min>0** bounds the GED likelihood on nonconstant data: at least one distance from any c is >= half the range, so `M_p(c)>=(range/2)^p/n`; the displayed profile then has a finite uniform upper bound for p>=p_min. The two-piece model is also bounded with such a floor: profile total width and side allocation, whose residual expression is `Q_p(c)=(A_p^(1/(p+1))+B_p^(1/(p+1)))^(p+1)`, with A_p/B_p the sidewise sums of distance powers. Neither side can remove the positive sample range. Closure includes half-laws and uniform.

**p>=1 is preferable to an arbitrary tiny epsilon:** GED (and finite-scale two-piece GED) is log-concave, Laplace is included exactly, and exponential and all lighter tails are retained. For fixed p>1 GED location uniquely minimizes a strictly convex sum of powers; at p=1 any sample median minimizes absolute deviations, so even the constrained family need not have a unique parameter MLE. Log-concavity in x does **not** make the joint parameter likelihood concave.

The global supremum can be at **p=1**, at a finite **p>1**, or at **p=infinity**, the ordinary uniform fit with logLik `-n*log(max(x)-min(x))`. With SGED also compare each half-GED face and the p=1 asymmetric-Laplace face, including exponential corners. At infinity the split point/skew is unidentified: there is no extra skew-uniform model. A finite-shape optimiser alone cannot decide which face wins. Use closed inequalities p>=1 (an attained Laplace member), not a log transform enforcing p>1.

Other defensible choices: **0<p_min<1** preserves sharper cusps/heavier tails but has no canonical cutoff and a harder nonsmooth location search; **p>=2** is interpretable as normal-or-lighter tails but discards Laplace; **1<=p<=p_max<infinity** removes uniform escape but an arbitrary cap can determine the fit. Fixing p a priori also works and should remain possible. Recommendation: explicit `shape_min=1` for ML, document the changed family, retain p>0 for density evaluation, and report D1 escapes after comparing boundaries. Do not present an unconstrained local maximum as an MLE.

Software is not a reason to retain the ill-posed domain. The inspected current CRAN-mirror sources for fGarch `gedFit` and `sgedFit` use `nlminb`, start at nu=2, and give nu lower bound **0**, upper Inf [8,9]; they can return local solutions without solving global existence. gamlss.dist PE uses default **log** shape link and validity nu>0; its source also offers **logshiftto1** [10], which enforces >1 rather than including the Laplace boundary. PE is mean/sd standardised; PE2 uses the direct exponential-power scale [11]. A small-shape curvature safeguard in PE is a numerical device, not a positive statistical shape floor.

## Small numerical checks (evidence, not global certificates)

Python/SciPy, no random draws. GED: optimise c on [min,max] with bounded scalar search (`xatol=1e-11`), also evaluate both endpoints; exact scale profile above. Shapes `{1} union geomspace(1.001,10000,220)`; compare exact uniform. These symmetric examples have a central minimizer for every p>=1.

| Data | Best finite-grid p; logLik | p=1 logLik | Uniform logLik |
|---|---:|---:|---:|
| `c(seq(-.02,.02,length=18),-10,10)` | 1; -34.052629 | -34.052629 | -59.914645 |
| `qnorm(((0:39)+.5)/40)` | 2.321086; -56.068606 | -58.415167 | -60.009963 |
| `seq(-1,1,length=40)` | 10000; -27.752437 | -41.012712 | -27.725887 |

For x=(0,1,2,4), c=0, the logLik slope against log(s) from s=1e-3 to 1e-6 was **-0.400000**, **0.500000**, **4.999999** for df=.2,.5,2, matching `3*df-1`. This demonstrates divergence at positive df, not just at df->0. The right half-normal scale is **2.291288**; half-t scalar roots at df=2,5,100 give **1.637525,1.981739,2.274435**, approaching that half-normal scale.

## Recommendation table

| Limit / proposed action | Decision | Reason |
|---|---|---|
| Translated half-normal (`mlhnorm`, orientation argument) | **add as mlX** | Standard law; unique closed-form endpoint/scale; directly makes snorm's D1 advice actionable. |
| Scale Student t (`mllst`, optional fixed df; guarded df>=1 by default) | **add as mlX** | Standard named family fills the infinite-variance gap; fixed df=2 handles std's face without another public id. Reject insufficient n/tie configurations; retain Gaussian D1. |
| Translated half-t, including half-t2 | **mention in the message only** | Legitimate limit but free-df fitting needs a floor, shape search and half-normal escape; an internal face solver is useful before another public family. |
| Two-piece t2 | **mention in the message only** | Specialist fixed-shape slice; requires boundary comparisons and can itself escape to half-t2. |
| Translated half-GED | **not worth it** | Unrestricted fit has no MLE; bounded version is a niche generalized-gamma slice. Describe it if constrained SGED escapes, without a fake `mlX` pointer. |
| Unrestricted GED/SGED | **not worth it** (as global ML) | No MLE; change the fitting domain explicitly to p>=1, while keeping full-domain evaluation. |

Unresolved reviewer choices: public names/orientation convention; accept df>=1 plus tie guards versus the narrower df>=2 default; accept D6's explicit family change. No certified general global classifier or joint-shape uniqueness theorem for the skew/half families was established here; optimizer output must not imply either.

## References actually inspected

1. Tumlinson (2015), *On the non-existence of maximum likelihood estimates for the extended exponential power distribution and its generalizations*, SPL 107, 111–114. DOI **[10.1016/j.spl.2015.08.009](https://doi.org/10.1016/j.spl.2015.08.009)**; DOI and theorem inspected in the [author-uploaded text](https://www.researchgate.net/publication/281497369_On_the_non-existence_of_maximum_likelihood_estimates_for_the_extended_exponential_power_distribution_and_its_generalizations).
2. Hasannasab, Hertrich, Laus & Steidl (2021), *Alternatives to the EM algorithm for ML estimation of location, scatter matrix, and degree of freedom of the Student t distribution*, Numerical Algorithms 87, 77–118. DOI **[10.1007/s11075-020-00959-w](https://doi.org/10.1007/s11075-020-00959-w)** seen on the [publisher's correction page](https://link.springer.com/article/10.1007/s11075-021-01156-z); mathematical scope inspected in the [author preprint, Section 3](https://arxiv.org/html/1910.06623v2).
3. [extraDistr half-normal manual](https://rdrr.io/cran/extraDistr/man/HalfNormal.html).
4. [extraDistr half-t manual](https://rdrr.io/cran/extraDistr/man/HalfT.html).
5. [extraDistr location-scale t manual](https://rdrr.io/cran/extraDistr/man/LocationScaleT.html).
6. [gamlss.dist TF source](https://raw.githubusercontent.com/cran/gamlss.dist/master/R/TF.R).
7. [twopiece author-hosted manual](https://warwick.ac.uk/fac/sci/statistics/staff/academic-research/steel/steel_homepage/software/twopiece-manual.pdf).
8. [fGarch gedFit source](https://raw.githubusercontent.com/cran/fGarch/master/R/dist-gedFit.R).
9. [fGarch sgedFit source](https://raw.githubusercontent.com/cran/fGarch/master/R/dist-sgedFit.R).
10. [gamlss.dist PE source](https://raw.githubusercontent.com/cran/gamlss.dist/master/R/PE.R).
11. [gamlss.dist PE/PE2 manual](https://stat.ethz.ch/CRAN/web/packages/gamlss.dist/refman/gamlss.dist.html).
