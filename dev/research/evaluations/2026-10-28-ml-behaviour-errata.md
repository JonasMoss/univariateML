# Referee report: ML behaviour, closure and fitter diagnostics

Review of `dev/research/ml-behaviour.tex`, in this worktree, 2026-10-29. Line numbers below refer to the unchanged source. Research only: no note, estimator, README, reference fix or golden was edited. The initial tracked diff and status were empty; the predecessor left scratch probes, not a partial report. I read those probes and independently checked the principal counterexamples. Distinguish **proved below**, **numerically checked**, **historical evidence**, and **not established**. A numerical optimizer is never used here as a proof that no better interior fit exists.

## Summary table

Severity: **critical** = incorrect mathematical conclusion used to reject/return a fit; **major** = incorrect classification, diagnostic or general theorem; **moderate** = missing qualification, proof or reproducibility; **minor** = stale narrative.

| Location | Claim | Verdict | Severity | Suggested fix |
|---|---|---|---|---|
| 38–62, Purpose | 42 families; six escape families; sstd still silent; GED never has an MLE | Stale and internally contradictory | Major | Inventory all 49 current IDs; distinguish unrestricted GED from fitted shape ≥1; remove superseded sstd and bug claims. |
| 67–75 | Hellinger definition, with point masses as exceptions | Formula correct; continuous point masses are not Hellinger limits | Major | Use ordinary Hellinger closure and discuss weak degeneracy separately. |
| 78–92 | Every maximizing sequence converges; nonclosure is needed for escape; excluded point masses only concern constant samples | Too strong / conflates topologies and likelihood continuity | Major | Define a finite boundary supremum through subsequential limits; separate divergence, weak concentration, and attained members; state assumptions for any necessity assertion. |
| 109–110; lomax README/mle.hpp | `mean(x^2)<2*mean(x)^2` implies exponential global supremum | **False; explicit counterexample** | Critical | Retain this only as an inward local score test; compare global profile maxima and exponential. Same bug in invpareto. |
| 111; gompertz README/mle.hpp | Overdispersion detects exponential boundary | Correct strictly; equality omitted | Major | Include population variance = mean². `c(0,2)` currently fails generically instead of naming exponential. |
| 112–113; 413–416 | nbinom detection exists only in adapter | Stale; strict check is in C++ | Minor | Update table; add equality regression (`c(0,2)` currently returns generic failure). |
| 114; binom README | Infinite size gives Poisson | Correct limit; a generic moment rule needs its own justification | Moderate | Separate verified limit from existence criterion and integer global optimization. Do not call status `Underdispersed` a mathematical characterization. |
| 115–143 | Free-scale burr/invburr Weibull/Pareto and reciprocal faces | Limit formulas correct | Moderate | State nonconstant data, endpoint versions, and that multistart face selection is not certification. |
| 120–143 | Free-scale beta-prime gamma / inverse-gamma; no nondegenerate lognormal face | Correct | — | Retain, with a concentration argument supplementing the log-variance calculation. |
| 145–159; 292–300 | Fixed-scale Burr expansion and reciprocal power limit; all ≥/≤ scale causes escape | Correct for nonconstant data, **but endpoint likelihood differs from ordinary face MLE** | Major | Preserve `−Z log 2`; do not equate supremum with the package Pareto/power fit when observations equal held scale. |
| 187–205 | Normal inward score `n(K−3)/4`; K≤3 not globally sufficient | Correct | — | Define variance-standardized z explicitly and retain “local.” |
| 197–198 | Two-cluster sample has supremum at t2 | Normal is decisively beaten; global location/scale/df claim not proved by that fact | Moderate | Give actual sample and likelihoods; label t2 selection numerical. |
| 207–226 | snorm Q-profile, degree-seven stationarity, endpoint rule | Correct algebra; finite precision is not a certified exact solver | Moderate | Distinguish algebraically exhaustive candidates from floating-point root isolation; retain attained interior ties. |
| 228–258 | sstd density, inward score and three faces | Correct regular face descriptions | Moderate | Define w sidewise; include tie collapse and qualify numerical global claims. |
| 239–241 | Half-t scale root is unique | Requires `nu(n−k)>k` at the endpoint | Major | State this condition; handle zero/negative limiting score before root search. |
| std/sstd fitters, 359–364 | Tie nonexistence dealt with by fixed-df paragraph | **Free-df paths also unbounded with heavy ties; currently misclassified/returned** | Critical | Test multiplicities before optimizing free df. A finite t2-face message is wrong when the original likelihood is unbounded. |
| 261–288 | GED/SGED shape-zero divergence and shape ≥1 restriction | Divergence and model restriction correct; heading and opening stale | Major | Describe actual constrained fit first; identify shape 1 as skew-Laplace for SGED. |
| 277–286 | Uniform limit and inward score −∞ | Correct; does not certify a global winner | Moderate | Keep local/global distinction; GED/SGED prefer attained ties, unlike several escape fitters. |
| 307–315 | hnorm endpoint MLE and external-density workaround | Formulas correct under closed-endpoint density; one-ULP shift is approximate | Moderate | State density-version convention and that stored endpoint is a numerical representation, not the exact order statistic. |
| 316–321 | lst is target for t2 escape | Model includes t2, but free-df `mllst` solves a larger problem | Moderate | Recommend `mllst(x, df=2)` to reproduce this face; free df may select Cauchy or normal. |
| 325–328 | invgauss MLE always finite; Lévy face | Correct for positive finite **nonconstant** data | Moderate | Add shape estimate and exclude constant samples; “meaningless” is not a mathematical diagnosis. |
| 334–335 | ZIP pi=0 when zeros are fewer than Poisson predicts | Correct; equality also returns pi=0 | Minor | State empirical zero fraction ≤ exp(−sample mean); handle all-zero nonidentifiability separately. |
| 336–337; zipf README/mle.hpp | Negative unconstrained Zipf shape means no MLE despite s≥0 | **False; uniform is attained; Newton can even reject positive roots** | Critical | Constrain shape at zero and safeguard Newton; reproducer has true s=0.00963717. |
| 338–344 | Nakagami m>0 and order-statistic boundary fits | Correct under declared domains | — | Retain; distinguish estimated support parameters from fixed support bounds. |
| 349–357 | Closed-family list; Kumar existence | Kumar proof correct for data strictly in (0,1), nonconstant; blanket closure list unproved | Moderate | Give parameter-domain qualifications and a separate closure argument, not an optimizer sweep. |
| Finite Zipf, omitted closure face | N→∞ at s>1 gives infinite Zipf/zeta | Missing nondegenerate Hellinger limit, though finite-sample N MLE is max(x) | Major | Add to “not closed but no corresponding escape”; document separately from attained s=0. |
| 366–386 | Three-parameter Birnbaum–Saunders path | **Correct**, stronger than stated | — | Extend to tied minima: divergence is `−2k log eps+O(1)`. Call half-mass collapse weak, not Hellinger. |
| 390–424 | Historical bug fixes | Most descriptions match current code; Cauchy no-MLE wording false at edev/validation/quality/n=2 | Major | Separate historical incidents from current defects; equality need not preclude attained Cauchy MLEs. |
| 430–445 | D1 / README limits / fixed parameters | Incomplete metadata and inconsistent tie conventions | Moderate | Add missing face names and fixed-subset qualifications; list current GPD/ZINB/invparalogis controls. |
| 140–141, 253–258, 438–440 | Escape counts, distribution-specific outcomes, 20 ms timing | Historical sample-specific evidence, not population statements | Moderate | Supply seeds, samples, revision, environment and oracle source; say “selected a face,” not “proved no interior maximum.” |
| Inventory | gpd, zinb, invexp, invpareto, invparalogis not systematically classified | Missing coverage | Major | Add rows, especially GPD attained floor, ZINB pi=0 versus ZIP/Poisson escape, and reciprocal Lomax bug. |

## 1. Definitions: topology does not control finite-sample likelihood

The Hellinger formula is standard. For probabilities dominated by a common measure, Hellinger convergence implies total variation convergence. If P is absolutely continuous and Q is a point mass, their affinity is zero and H²(P,Q)=1. Thus a shrinking normal cannot converge to its point mass in Hellinger distance, even if the dominating measure is enlarged to include that atom. A discrete family can converge in Hellinger distance to an atom (e.g. Poisson mean→0), so the continuous/discrete distinction matters.

Likewise the Birnbaum–Saunders path below has a **weak** atom-plus-density limit and does not have that limit in Hellinger distance. The statement that point masses are “approached only for degenerate samples” is inappropriate: distributional limits do not depend on a sample, and likelihood spikes can occur on nonconstant samples.

The escape definition also excludes a maximizing sequence that alternates between two equally good faces: it need not converge, though it has relevant subsequential limits. Prefer: a finite supremum is unattained, and specified escaping maximizing subsequences converge in a stated topology to boundary laws. Keep **unbounded likelihood**, **finite unattained supremum**, and **attained boundary member** separate. Closure in Hellinger distance alone does not supply compactness, upper semicontinuity of point evaluation, or a likelihood existence theorem. Densities equal almost everywhere can differ precisely at sample endpoints. The fixed-scale Burr example is a concrete warning that fitting the limit law need not recover the original family's supremum.

D1 is already decided at lines 430–432; lines 90–92 saying policy is open are stale.

## 2. Lomax: the moment check is not a global existence criterion

Write m_j=mean(x^j), S(lambda)=sum log(1+lambda*x), kappa_hat=n/S. The exact profile is

    ell(lambda) = n log lambda + n log(n/S) − S − n.

Its exponential endpoint is ell_E=−n(log m_1+1). Expansion gives

    ell(lambda) − ell_E
      = n lambda [m_2/(2 m_1) − m_1] + O(lambda²).

So m_2<2m_1² makes the exponential a **strict local** boundary maximum. It does not exclude a second, higher interior maximum.

Take x=(0.001,1,2), with m_2/m_1²=1.6655561. The exponential log likelihood is −3.00099983. At lambda≈1039.288, kappa≈0.1960814 the Lomax log likelihood is −2.34857657, a gain of about **0.652423**. Any one admissible parameter point with this gain disproves the claimed global boundary classification; no global optimizer certificate is necessary. Also ell→−∞ as lambda→∞ for strictly positive x, so this higher branch has a finite maximizer.

Reproducer after loading the assembled package:

```r
x <- c(.001, 1, 2)
mllomax(x) # incorrectly stops: MeanVarianceCondition; use mlexp
sum(extraDistr::dlomax(x, lambda=1039.288, kappa=.1960814, log=TRUE))
as.numeric(logLik(mlexp(x)))
mlinvpareto(1/x) # same false rejection, inherited from lomax_mle
```

Under y=1/x, log f_Y(y)=log f_X(x)+2 log x. Therefore the gain is unchanged: inverse-Pareto −14.77779 versus inverse-exponential −15.43022. Both READMEs repeat the faulty condition, as do their messages. Fix the mathematical condition before adding a numerical tolerance. Search all profile branches, compare with the endpoint, and retain attained ties. A scan alone is still not a theorem of global optimality.

## 3. Gompertz and discrete equality cases

For Gompertz put T(b)=sum integral_0^{x_i} exp(bt) dt. Profiling a gives

    ell(b) = n log n − n log T(b) + b sum x_i − n.

log T is a log-Laplace transform, so is convex; for a sample with some x_i>0 it is strictly convex. Thus ell is strictly concave. Its right derivative at zero is

    ell'(0+) = n[m_1 − m_2/(2m_1)].

Unlike the Lomax argument, here concavity makes the local criterion global. The exponential is the unattained supremum exactly when m_2≥2m_1²; equality belongs with the boundary too. `mlgompertz(c(0,2))` currently gives “Gompertz MLE failed,” although mean=population SD=1. All-zero data require separate treatment; the positive-mean derivation divides by T(0).

The nbinom strict-underdispersion check is now in `nbinom/mle.hpp`, contradicting the early table. Equality is not handled: method-of-moments initialization divides by zero and `mlnbinom(c(0,2))` gives a generic failure. For this exact sample the profiled NB/Poisson likelihood ratio has logarithm

    2 − (2r+1) log(1+1/r) < 0,   r>0,

with limit zero as r→∞. The inequality follows from log(1+t)>2t/(2+t). Thus this is a proved Poisson escape, deserving the same pointer as strict underdispersion. The binomial on the same sample also approaches Poisson from below:

    log(L_N/L_Pois) = 2 + (2N−1) log(1−1/N) < 0, N≥2.

`mlbinom(c(0,2))` does report the Poisson face. The note supplies no proof of the general integer-size existence criterion or of exhaustive maximization by the current Newton-and-integer-candidates method; the Poisson limit itself follows directly from the PMF/PGF. Do not infer a general theorem from this equality example.

## 4. Burr, inverse Burr and beta prime

### Correct free-scale limits

For Burr XII, S(x)=[1+(x/sigma)^c]^(-k). Setting sigma=lambda*k^(1/c) and taking k→∞ gives S→exp[−(x/lambda)^c]. Setting k=alpha/c and c→∞ gives Pareto survival 1 below sigma and (x/sigma)^(-alpha) above it. The displayed formulas and directions of scale movement are correct. Reciprocal transformation gives inverse Weibull with held effective scale sigma*k^(1/c), and power with upper endpoint sigma. The reciprocal Jacobian contributes −2 log x and does not affect likelihood comparisons between transformed candidates.

For free scale the Pareto MLE endpoint m=min(x) can be approached without losing the endpoint density: take sigma_c=m exp(−h_c/c), h_c→∞, h_c/c→0, and k=alpha/c. Then c log(m/sigma_c)=h_c→∞ while sigma_c→m. This is a different path from holding sigma=m exactly.

For beta prime X=sigma*G_a/G_b with unit-scale independent gammas, G_b/b→1, so sigma/b→theta gives Gamma(a,theta). Similarly sigma*a→beta with a→∞ gives InvGamma(b,beta). The note's scale conventions are correct. For fixed scale, Y=X/(s+X) is beta(a,b), with parameter-independent Jacobian. Strict concavity of the beta exponential-family log likelihood and its coercive profile on nonconstant interior data give the unique finite fixed-scale MLE.

Both a,b→∞ cannot give a nondegenerate lognormal: Var(log X)=trigamma(a)+trigamma(b)→0. Chebyshev implies concentration around E(log X)=log sigma+digamma(a)−digamma(b), regardless of how sigma moves. Tight limits therefore concentrate at a point (possibly at zero after exponentiation); otherwise mass escapes. A standardized or centered/rescaled **log variable** can have a Gaussian limit; that is not a limit of X obtained merely by changing its scale. Shapes 10/20, 100/200, 1000/2000 give log variances about .156437, .0150627, .00150063.

These are limit derivations, not proofs that the current multistart fitter locates every interior maximum. The Shao article itself emphasizes multiple maxima; a face comparison can invalidate a poor returned fit without certifying the winning face globally.

### Fixed-scale endpoint penalty: retain the note's own Z term

Let l_i=log(x_i/s), P=sum max(l_i,0), N=−sum min(l_i,0), and Z=#(x_i=s). Along k=alpha/c,

    ell = n log alpha − sum log x_i − c N − alpha P − Z log 2 + o(1).

This agrees with the note (s=1). The −Z log 2 does **not** disappear when profiling k. If all x_i≥s and data are nonconstant, P>0, alpha_hat=n/P, and

    sup ell_Burr,fixed-s = ell_Pareto,fixed-endpoint-s − Z log 2.

To see that this is the global supremum in this case, write S(c)=cP+R(c), with R(c)=Z log2+sum_{l_i>0}log(1+exp(−c l_i)). The profiled likelihood is a constant minus n log(P+R/c) minus R. Both R and R/c decrease, proving monotone increase to the stated supremum. If any l_i<0, this Pareto path loses likelihood exponentially. The fixed-scale profile tends to −∞ at c→0 and c→∞ for nonconstant data that are not all ≥s, giving a finite maximum. Constant data need their own degeneracy classification.

For x=(1,1.5,2,3), s=1, the Pareto fit is −3.800827; fixed-scale Burr approaches **−4.493974**, exactly log2 lower. At c=100, 10^4, 10^6 its values are approximately −4.506593, −4.494101, −4.493976. Thus “use mlpareto” identifies a distributional face, but its ordinary closed-endpoint likelihood is **not** the original supremum. The inverse-Burr power boundary has the same penalty at x=s.

The fixed-scale detection `all(x>=s)` / `all(x<=s)` is mathematically justified for nonconstant samples. The blanket assertion in the definitions that every observed escape equals the maximized face likelihood is false at these endpoints. All values exactly s give an unbounded spike, not this finite face fit.

## 5. Student t: valid scores, missing free-df tie obstruction

With z=(x−xbar)/sqrt(mean((x−xbar)^2)), the variance-standardized expansion is

    log f_nu(z) = log phi(z) + [z^4−6z²+3]/(4nu) + O(nu^−2).

The nuisance scores vanish at the normal MLE and sum z²=n, giving the note's inward profile score n(K−3)/4. In ordinary-t coordinates the polynomial is (w^4−2w²−1)/4, yielding the same profiled score. K<3 proves strict local boundary maximality; K>3 rules it out. K=3 needs higher orders.

The reproducible two-cluster sample is

```r
x <- c(seq(-1e-4,1e-4,length.out=30),
       seq(.9999,1.0001,length.out=10))
```

It has K≈2.33333337, normal logLik≈−23.27801305, and an ordinary t2 candidate near location 0, scale 1.13828e−4, logLik≈54.07217. This proves K≤3 is not globally sufficient; it does not by itself prove no finite nu>2 point exceeds that t2 candidate. The code selects the t2 face by multistart comparison. Code sd=s*sqrt(nu/(nu−2)) diverges along a nondegenerate t2 face; merely holding sd fixed as nu→2 instead collapses the distribution.

For fixed nu, write C_nu=Gamma((nu+1)/2)/(sqrt(nu*pi)*Gamma(nu/2)). At a location repeated k times, as ordinary scale s→0,

    L = C_nu^n * nu^((nu+1)(n−k)/2)
        * product_{x_i != c}|x_i−c|^(−nu−1)
        * s^[nu(n−k)−k] * (1+o(1)).

Thus the note's exponent and strict unboundedness condition are correct. At equality the limit is **finite**, and the exponent alone says nothing about whether a positive-scale MLE exists. Current fixed-df `ScaleBoundary` diagnostics deliberately avoid certifying it, which is appropriate.

But **free** nu>2 also allows this path whenever k>2(n−k): choose 2<nu<k/(n−k) and shrink scale. For n=11, k=8, nu=2.3, exponent=−1.1. The likelihood grows by about 5.06569 whenever scale shrinks by 100. Reproducer:

```r
x <- c(rep(0,8),1,2,3)
mlstd(x)             # returns finite coefficients on default iteration limit
mlstd(x,nu=2.3)      # correctly reports unboundedness
mlstd(x,nu=3)        # regular fixed-df fit, positive exponent
sapply(c(.01,.0001,.000001), function(s)
  sum(dt(x/s,df=2.3,log=TRUE)-log(s)))
mlsstd(c(rep(0,8),-1,1,2)) # incorrectly says finite t2 boundary
```

SGED is not implicated by this t-tail calculation. SSTD **is**, since xi=1 embeds STD. The free-STD return or free-SSTD finite-face diagnostic is a substantive code bug. Increasing `iterlim` can change STD's answer to a t2 message, but still does not identify unboundedness. Introduce a multiplicity precheck; distinguish the exact lower-df equality k=2(n−k), which does not imply strict divergence for nu>2.

The `mllst` model has df≥1 and includes the attained Cauchy member. Free-df conservative rejection of k≥n/2 is documented; strict k>n/2 permits genuine unboundedness. For a t2 face specifically use `mllst(x,df=2)`. Calling free-df `mllst(x)` can legitimately yield a different fit, as it does on the two-cluster sample.

## 6. Snorm and sstd

For the two-piece normal, f= sqrt(2/pi)/(a+b) * exp(−(x−c)^2/(2s_side²)). Its log likelihood is

    n log sqrt(2/pi) − n log(a+b) − A/(2a²) − B/(2b²).

The scale score equations give a/b=(A/B)^(1/3), a=A^(1/3)*sqrt((A^(1/3)+B^(1/3))/n), and the analogous b. Substitution gives the note's exact Q-profile and constants. Outside the sample range, moving c to the nearest extreme improves the one-sided fit.

Put u=sum_{x<c}(c−x), v=sum_{x>c}(x−c). Interior stationarity is u/A^(2/3)=v/B^(2/3). Cubing gives u³B²−v³A²=0. On each order-statistic interval u,v are affine and A,B quadratic, so degree≤7. Endpoints, knots, and all stationary roots suffice. An interior global minimizer gives an attained MLE even if it ties an endpoint. The implementation deliberately prefers interior exact ties in Q. Floating-point polynomial coefficients, bisection and cancellation tolerances mean “exact” should describe the reduction, not an interval-arithmetic guarantee about the computed answer. Complexity is O(n log n) sorting plus bounded-degree work per interval, at fixed numerical precision.

At the right face take r=a/b↓0. Reopening the empty side at c=min(x) changes normalization by −n log(1+r). The optimal small location displacement is O(r²) in the normal case, so the first inward derivative remains −n. Both half-normal faces are strict local boundary maxima, whether or not they win globally. This explains, but does not logically prove, particular optimizer failures.

For SSTD the stated density 2*t_nu((x−c)/s_side)/(a+b), xi=sqrt(b/a), is correct. Here w_i must explicitly mean residual divided by its **own side's scale**, not a mean/sd standardized residual. The common scale score at a fitted snorm face gives sum w²=n; the ordinary-t expansion above then gives (sum w⁴−3n)/4.

The nondegenerate regular faces are snorm, half-t (both orientations, df≥2), and two-piece t2; their intersections give the stated half-normal and half-t2 corners. A half-t density is decreasing away from its mode, so its fitted endpoint is the appropriate sample extreme. The half-t scale score

    S(s)=sum (nu+1)y_i²/(nu*s²+y_i²) − n

is strictly decreasing when some y_i>0, with S(infinity)=−n and S(0+)=(nu+1)(n−k)−n=nu(n−k)−k. A unique positive root exists **only when this last expression is positive**. Negative means unbounded scale collapse; equality needs separate analysis. The unconditional unique-root sentence omits this assumption.

The face routines are numerical: a grid plus local refinement in df need not find every stationary point; neither do seven BFGS starts establish global optimality. “Every interior fit matches the oracle” is an empirical comparison with another local search. It should not become “every sample has no interior maximum” when a numerical face wins.

## 7. GED/SGED: unrestricted theorem versus actual fitted model

In natural exponential-power coordinates use

    f(x)=p/[2a Gamma(1/p)] exp(−(|x−c|/a)^p),
    M_p(c)=mean |x−c|^p,   a_hat^p=p M_p(c),
    ell_p(c)/n=−log2−logGamma(1+1/p)−[log(p M_p(c))+1]/p.

Put c at a value repeated k<n times, q=(n−k)/n and A=mean_{x_i!=c}log|x_i−c|. Then log M_p=log q+pA+O(p²), and Stirling gives

    ell_p(c)/n=−log(q)/p + (log p)/2 − A − log2 − log(2pi)/2 + O(p).

The note's leading expression is correct and proves divergence for every nonconstant sample in the **unrestricted positive-shape** GED. SGED contains GED at xi=1, so it inherits the divergence. No numerical optimizer or citation is needed for this proof. The fixed-location/scale variance normalization is an invertible change of coordinates for each finite p and cannot repair it.

The actual fitters use **p≥1**. Shape one is attained: Laplace for GED, two-piece/skew-Laplace for SGED. For GED the location is any median and sd=sqrt(2)*mean absolute deviation. SGED must also fit skewness; it is not generally the symmetric Laplace. Its natural p=1 profile uses A=sum left distances, B=sum right distances,

    a=sqrt(A)(sqrt(A)+sqrt(B))/n, b=sqrt(B)(sqrt(A)+sqrt(B))/n.

The minimized quantity (sqrt(A)+sqrt(B))² is concave between knots, so all observation knots and the half-exponential endpoints suffice for that attained-shape comparison. Code does explicitly compare these candidates.

As p→∞ the symmetric standardized support becomes [mu−sqrt(3)*sd,mu+sqrt(3)*sd]. In SGED, natural left/right scales give a **single** uniform on [c−a,c+b]; skew only specifies an unidentified split point in that interval. For distinct extrema, with R=(max−min)/2 and eta=1/p, profiling gives

    ell_p = −n log(2R) + n eta[log eta − log(2/n)+gamma_E−1] + o(eta).

Thus the inward derivative is −∞. Repeated extrema change constants but not the leading eta log eta term. This proves a strict **local** uniform boundary maximum, not the global winner. Endpoint density evaluation needs the profiled/inflated-support sequence; evaluating a fixed natural scale at exactly the endpoints gives the wrong pointwise likelihood limit.

The GED/SGED source compares numerical finite candidates with uniform, and SGED also scans both half-GED shape profiles. This covers the claimed candidate types but is not a certified global search. Their tie convention prefers an attained finite member; several other families instead prefer faces within 1e−8*n. Any blanket statement “all faces win ties” needs this exception. Likewise the SGED README's generic description “exact Laplace fit” should say skew-Laplace here.

## 8. Kumaraswamy: existence proof is valid

Assume 0<x_i<1 and not all x_i equal. Let t_i=−log x_i>0, T=sum t_i, S(a)=sum log(1−exp(−a t_i)). The likelihood is

    ell(a,b)=n log a+n log b−(a−1)T+(b−1)S(a),
    b_hat(a)=−n/S(a).

At a↓0, S(a)=n log a+sum log t_i+O(a); substitution cancels n log a against −S, leaving

    ell_profile(a)=−n log|log a|+O(1) → −infinity.

At a→∞, if t_min has multiplicity r, S(a)=−r exp(−a t_min)(1+o(1)), so

    ell_profile(a)=a(n t_min−T)+n log a+O(1) → −infinity.

Nonconstancy makes n t_min−T<0. The profile is continuous on (0,infinity), hence has a finite maximum. The omitted n log a term is lower order, so the note's “like” statement is fine. This proves existence, not uniqueness. If all observations are equal, that linear coefficient vanishes and the profile diverges upward. Exact x=0 or 1 are excluded by the model and are not covered by the proof. Clipping simulated endpoints is a different dataset and must be disclosed.

## 9. Birnbaum–Saunders: the proposed unbounded path is correct

The displayed log density agrees with the transformation Z=(sqrt(t/beta)−sqrt(beta/t))/alpha and its derivative. On beta=eps, alpha²=s/eps, mu=x_min−c eps²/s:

* At a minimum, t=c eps²/s. The four logarithmic terms contribute `(1−3−1/2+1/2) log eps=−2 log eps`; the quadratic exponent tends to −1/(2c).
* At every other observation t→d>0 and the density tends to `exp(−d/(2s))/(2 sqrt(2pi*s*d))`.

Therefore ell=−2k log eps+O(1), where k is the multiplicity of the minimum. Unique minimum is sufficient but unnecessary: **every finite sample**, including tied minima, has this divergent path. In the weak limit half the mass is at the endpoint, and half has the law s*chi-square_1. This is not Hellinger convergence. Per decade, the log-likelihood increment tends to 2k log10=4.605170*k.

Fixing location avoids this particular obstruction. For positive nonconstant data, profiling alpha²=mean(x)/beta+beta*mean(1/x)−2 reduces to a one-dimensional beta profile. The beta→0 and beta→infinity endpoints have finite, subprobability pointwise limits; each improves when moving inward (the reciprocal identity swaps them), giving an interior maximizer. “Same defect as three-parameter lognormal” is reasonable at the level of unbounded likelihood, but does not assert identical weak limit mechanisms.

## 10. Other attained members, closure and missing families

For inverse Gaussian, expanding the exponent gives the Lévy density at mean→infinity with fixed shape lambda. Pointwise density convergence to a density of total mass one gives L1/Hellinger convergence by Scheffé. On positive nonconstant data,

    mu_hat=mean(x), lambda_hat=1/[mean(1/x)−1/mean(x)]>0.

Strict arithmetic–harmonic mean inequality makes lambda finite. Constant samples instead have lambda→infinity and unbounded likelihood. A large empirical mean from Lévy data is a legitimate finite inverse-Gaussian fit; “meaningless” needs a stated inferential criterion.

For ZIP, the derivative in pi at pi=0 after fitting lambda=mean(x) is n[r0 exp(mean(x))−1]. The attained Poisson member therefore wins when r0≤exp(−mean(x)); equality belongs with it. In the inflated branch, m=mean(x)/(1−r0)>1 and lambda=m+W_0(−m exp(−m)). The W_(−1) branch is the trivial zero root; the historical bug explanation is correct. All-zero samples have nonidentified parameters and must be treated separately.

Nakagami: X²~Gamma(m,scale=Omega/m), so integrability requires only m>0. At m=1/2 it is an attained half-normal, not an escape. Its m MLE is the gamma shape equation on x², with Omega_hat=mean(x²). The fitted m<1/2 examples are valid under the README's domain.

Uniform endpoints, discrete-uniform endpoints, Pareto minimum and power maximum are attained support-parameter MLEs for nonconstant samples with the declared closed-endpoint density versions. These should not be combined with open shape/skew faces. Single-point/constant cases can be unidentified or unbounded and need separate treatment.

### Finite Zipf has two separate omissions

For fixed N≥max(x), ell(s)=−s sum log x−n log H_N(s), and ell''(s)=−n Var_s(log K)<0 for N>1. On the declared s≥0 domain, if mean(log x)≥log(N!)/N the constrained optimum is **s=0**, attained uniform. A negative unconstrained root does not mean the MLE does not exist.

There is also a numerical false-negative bug: `mlzipf(rep(1:3,c(101,100,100)))` throws `ShapeNegative`, although mean(log x)=0.59526887 is below log(3!)/3=0.59725316 and the strictly concave likelihood has unique positive root **s≈0.009637173**. Unsafeguarded Newton starting at one overshoots below zero and treats the iterate as the optimum. Counts (31,30,30) similarly have root≈0.03182906 and are rejected. Safeguard the score root and evaluate the score at zero before classifying.

Separately, finite Zipf is **not Hellinger closed**: for any fixed s>1, N→infinity yields the infinite Zipf/zeta law p(k)=k^(−s)/zeta(s). In fact H²=1−sqrt(H_N(s)/zeta(s))→0. This is a nondegenerate limit outside the finite-N family. Yet for a finite sample increasing N beyond max(x) only increases the normalizer and reduces likelihood, so this face never wins. Add it alongside inverse Gaussian as a counterexample to “nonclosed implies escape.” No README limit status currently records it; lack of an escape detector here is not a fitter bug.

### Cauchy equality is not nonexistence

For x=(0,1),

    [(mu²+s²)((1−mu)²+s²)] − s² = (mu²−mu+s²)² ≥ 0.

Hence L≤1/pi², attained by every 0<mu<1, s²=mu(1−mu). Repeating both observations gives x=(0,0,1,1), L≤1/pi^4 with the same continuum of positive-scale MLEs. Both `mlcauchy(c(0,1))` and `mlcauchy(c(0,0,1,1))` reject with “MLE does not exist.” This message and the note's wording are false. A deliberate restriction to regular/unique fits should be described as such. Strict multiplicity>n/2 yields genuine unboundedness; equality is a distinct case. The current fixed-df lst documentation already recognizes that equality can have attained fits.

### Inventory of all 49 current IDs

| IDs | Referee disposition |
|---|---|
| norm, lnorm, logitnorm | Ordinary normal location/scale and fixed monotone transforms; regular nonconstant samples have finite ML; no omitted proper nondegenerate face identified. Weak point-mass limits are separate. |
| exp, rayleigh, invexp | Positive rate/scale models, invexp reciprocal of exp. No nondegenerate escape identified; invexp is absent from the note's systematic list. |
| pois, geom, lgser | Discrete parameter boundaries include point masses or loss of tightness; lgser all-ones is an unattained point-mass endpoint and errors. A closure statement alone does not specify these API choices. |
| gamma, lgamma, invgamma, naka | Gamma and fixed bijective transforms: lgamma here means exp(Gamma), support >1, not log(Gamma); naka uses x². Nonconstant interior data have finite ML. |
| weibull, invweibull, gumbel, logis, llogis, laplace, cauchy | Fixed-shape location/scale families on original or log scale, or reciprocals. No omitted proper nondegenerate face identified. Cauchy has the equality caveat above. |
| beta, betapr, kumar | Beta and fixed-scale beta-prime have finite interior MLE for nonconstant interior data; free beta-prime has gamma/inverse-gamma faces. Kumar existence proved above. |
| paralogis, invparalogis | Burr with equal shapes and its reciprocal; no nondegenerate omitted face identified. Fixed scale has finite profile maximum even on constant positive data. Free-scale constant samples can concentrate. The note labels closure an argument but supplies none. |
| unif, dunif, pareto, power, hnorm | Estimated support endpoint fits are attained under stated density versions; hnorm appears only as a limit target, not a systematic closure row. |
| lomax, invpareto, gompertz, invgauss | Exponential, inverse-exponential, exponential and Lévy limits respectively. Local/global error in first two; equality omission in Gompertz; no Lévy escape for positive nonconstant data. |
| burr, invburr | Free and fixed-scale faces checked above, including endpoint penalty. |
| std, lst, snorm, sstd | Normal/lower-df/skew faces as above; attained df=1 for lst; missing free-STD/SSTD tie collapse. |
| ged, sged | Constrained p≥1 ML, attained p=1, uniform and half-GED faces. Unrestricted p>0 theorem must be separated. |
| binom, nbinom, zip, zinb, zipf | Poisson / ZIP escapes, attained pi=0 and s=0; finite-Zipf zeta closure omitted. ZINB is not covered systematically in note. |
| gpd | Threshold fixed by user, xi≥−1/2; xi=0 exponential and xi=−1/2 floor are **attained members**. Unrestricted xi<−1 allows unbounded endpoint likelihood. Missing from note. |
| fatigue | Two fitted parameters with fixed location; free-location likelihood divergence proved above. |

This inventory does **not** turn “no face identified” into a new proved universal closure theorem. The blanket list at lines 349–351 lacks a sequence/compactness argument, especially for shape families. Its fixed-transform cases can be reduced to their underlying families by Hellinger invariance under a bijection. Beta shape→0 can have weak mixtures of endpoint atoms, which again are not Hellinger limits of continuous densities. A sweep of successful fits cannot establish closure.

For GPD the reason for the lower bound is immediate: near an upper endpoint with xi<−1, exponent −1/xi−1<0 makes the density diverge. At the implemented xi=−1/2 floor there is no such singularity; the code returns that attained member. ZINB similarly needs two kinds of rows: pi=0 is attained NB, while size→infinity gives ZIP or its attained Poisson corner. Holding size removes that infinite-size escape.

## 11. README/status audit and stale numerical evidence

The current READMEs advertise the expected named statuses for Burr/Weibull/Pareto, inverse Burr/inverse Weibull/power, beta-prime/gamma/inverse-gamma, STD normal/t2, snorm half-normal, SSTD snorm/half-normal/half-t/t2, GED/SGED uniform/half-GED, binomial/NB Poisson, Lomax/exponential, inverse-Pareto/inverse-exponential, and ZINB ZIP/Poisson. Source inspection confirms these branches exist. This is a structural match, not evidence that their data triggers are all correct.

Specific metadata issues:

* `sged/README.md` half-GED entries omit both `model` and `name`, although `README_SCHEMA.md` asks for a human-readable name when no package model exists. Add “translated half-GED” and its reflection.
* Under the schema's stated meaning, `fixed: [scale]` restricts a face to held-scale fits; no corresponding explicit “requires free scale” field is used for the beta-prime gamma/inverse-gamma entries or the free-scale Weibull branches. The prose explains this, but automated consumers must not apply these faces to held-scale fits. The existing Burr/invBurr paired entries clarify the Pareto/power cases only.
* NB's Poisson entry is not marked unavailable for fixed size; ZINB uses prose in `detect` for the same issue. STD/lst fixed-df tie entries are properly distinguished, but their normal entries likewise rely on prose/domain interpretation.
* Inverse Gaussian has no `limits:` entry for its Lévy closure, and finite Zipf has none for zeta. If `limits:` describes only *detected ML escapes*, say that in the schema; if it describes all nonclosure, these entries are missing.
* GED/SGED prefer attained ties. Snorm prefers an exact tied interior Q minimizer. STD/SSTD and several positive-shape families prefer approximate face ties. These are numerical policies, not exact nonexistence theorems.

The fixed-parameter list at lines 441–445 omits GPD xi and user threshold mu, ZINB size, and inverse-paralogistic scale. The list of seven newly present IDs relative to the note's original 42 is gpd, hnorm, lst, invexp, invpareto, invparalogis, zinb. Hnorm/lst are discussed as targets; the other five lack systematic coverage.

Historical numbers must remain labeled historical. The 45-sample SSTD oracle agreement, 23/30 face selections and 14/8/1 split do not have the full sample-generating script in the note, so I cannot independently reproduce those exact counts. The 3/6 Burr and 5/12 beta-prime counts likewise need seeds and generating parameters. The predecessor's different seeded study produced 8/30 and 12/30, which illustrates sample dependence rather than contradiction. Neither set is a proved frequency of nonexistence. The old sweep still holds fatigue location free, optimizes unrestricted GED shapes, and reflects old metadata/parameter counts; it is not a valid direct oracle for every current restricted fitter.

The note's `c_hat=97.4` mostly-Pareto example lacks its seed. A new seed-6 sample of 199 Pareto-like values and one .99 yields c≈212.4814 and k≈.01025869, agreeing with an independent R optimizer. This supports the qualitative numerical repair but does not reproduce 97.4. The example shape≈3.3 for half-t is also sample-specific: seed 1, rexp(200), selects half-t df≈6.409. “Normal data go to snorm” must mean the recorded sample, not every normal sample.

The illustrative GED profile values (−48.4,−65.3,−19.0 versus normal −46.0), historical huge-nu STD fit (54874), n=30 nu≈9, pre-fix SSTD overflow at 343, logLik gaps .075 / 2e−4, and inverse-Gaussian mean 1663 require the actual samples/revisions to reproduce exactly. Their mathematical mechanisms were checked; exact historical values should not be presented as freshly verified results. The code has replaced raw gamma ratios by log-gamma in SSTD; whether residual huge-nu cancellation matters is separate from detecting a normal face. The claimed 20 ms timing needs hardware, optimization/build flags and a benchmark command. This review does not promote it to a performance guarantee.

The Burr log-moment ratio has the stated restricted range: at fixed scale 1, E(log X)=−[digamma(k)+gamma_E]/c and Var(log X)=[pi²/6+trigamma(k)]/c². For 0<k<1, 0<−[digamma(k)+gamma_E]<1/k and trigamma(k)>1/k², hence r(k)<1. A ratio ≥1 cannot be inverted on that branch, consistent with the old pole mechanism. Historical fixes in section 4 match the relevant present code: stable ZIP W branch, xlogx endpoint handling, safeguarded Cauchy/logistic methods, no-throw special seam, and explicit open-support validation. Passing no-throw tests is useful evidence, not a proof about every floating-point input. The intro must no longer state these fixed failures as current ordinary-data behavior. The Cauchy existence wording remains wrong as demonstrated above.

## 12. Bibliographic scope

The face and divergence derivations above stand independently of citations. I checked publication identity against accessible publisher/author records: [Fernández–Steel 1998](https://doi.org/10.1080/01621459.1998.10474117), [Fernández–Steel 1999](https://doi.org/10.1093/biomet/86.1.153), [Shao 2004](https://www.sciencedirect.com/science/article/abs/pii/S0167947302003675), and [Tumlinson's author-uploaded article](https://www.researchgate.net/publication/281497369_On_the_non-existence_of_maximum_likelihood_estimates_for_the_extended_exponential_power_distribution_and_its_generalizations). The displayed bibliographic identities agree. The 1998 Bayesian paper supports the skew construction, not an exact ML face classifier; the 1999 unrestricted-df pitfalls must not be transferred without qualification to nu>2. Shao explicitly discusses competing limiting forms and multiple likelihood maxima. Tumlinson supports unrestricted exponential-power nonexistence; it does not imply nonexistence after restricting shape≥1. Kleiber–Kotz's specific page/theorem was not independently inspected; the Lomax limit is derived here, while the advertised global moment criterion is disproved here.

## 13. Reproducibility and checks

All computations longer than a few seconds used `/tmp/umvl-cpu/run`, with its one-thread environment and nice priority; no installs, subagents, board edits or git writes. Scratch scripts and outputs are under `/tmp/umvl-opus/proofcheck/`. Core scripts retained/reviewed from the handoff: `probe1.R`, `probe2.R`, `py/bs.py`, `py/faces.py`, `py/ged.py`, `py/kumar.py`, `py/twocluster.py`. New independent checks: `codex_math.py`, `codex_probe.R`.

Fresh baseline checks all passed:

| Command (all through the shared slot lock) | Result | Scratch log |
|---|---|---|
| `just test` | FAIL 0 / WARN 0 / SKIP 0 / PASS **1913**, exit 0 | `codex-test.log` |
| `just cpp-test` | **4/4** ctest targets passed, exit 0 | `codex-cpp.log` |
| `just quality` | **49/49** families passed, exit 0 | `codex-quality.log` |

These checks do not cover the mathematical counterexamples above; keeping the baseline green does not resolve those bugs.

Fresh numerical derivation checks:

| Check | Observed values |
|---|---|
| BS sample (.7,1.3,2.2,2.9,4.1,5.5), s=2,c=1 | ell=11.55605,16.16121,20.76638 at eps=1e−6,1e−7,1e−8; final decade increment **4.6051696**. Adding a second .7 gives increment **9.2096393** by eps=1e−5, approaching 4 log10. |
| Burr→Weibull | Max log-density errors .112857, .00117845, .0000117897 for k=1e2,1e4,1e6, c=1.7, lambda=1.4 on x=(.3,.8,1.1,1.7,2.5,4). |
| Inverse Burr→inverse Weibull | Errors .0495241,.000512503,.00000512682 for k=1e2,1e4,1e6, shape=1.5, effective scale=.8 on same x. |
| Beta prime→gamma | Errors .109025,.00114778,.0000114849 at b=1e2,1e4,1e6, a=1.7, sigma=.6b on same x. |
| Beta prime→inverse gamma | Errors .0110619,.000117483,.00000117426 at a=1e2,1e4,1e6, b=2.2, sigma=1.3/a. |
| Normal score | x=(−2,−.7,−.1,.2,.8,1.3), analytic score **−1.04153333**; fixed-normal-nuisance secants −1.04270218,−1.04164982 at nu=1e3,1e4. |
| GED p→0 | NumPy `default_rng(40).normal(0,.8,40)`, location=x[0], normal ell=−40.20521; profiled GED ell **−35.0488,177.3675,830.5931** at p=.01,.003,.001. Asymptotic values −34.7681,177.4520,830.6213. |
| Kumar tails | x=(.01,.1,.4,.8,.99); profile ell −3.30956,−8.45406,−14.34770 at a=1e−3,1e−9,1e−30; −59.99697,−370.89978,−767.20898 at a=10,50,100. |
| Free-df t tie divergence | n=11,k=8,nu=2.3, tied at 0, remaining 1,2,3: ell **−7.99691964,−2.93071600,2.13497126** at s=.01,1e−4,1e−6. |

The predecessor's `faces.py` initially compared a Pareto density at x=.3 below its .5 support endpoint and produced infinite log-density errors. I discarded that comparison and reran `faces2.py` entirely inside support: x=(.8,1.1,1.7,2.5,4), alpha=2.3, endpoint=.5; errors were ≤1.7e−12. Its free-scale path with fixed h=30 is only a finite-range illustration: the proof above instead uses h_c→infinity with h_c/c→0. The optional exhaustive `binom_eq.py` search was interrupted; no conclusion here depends on its unfinished enumeration. The exact equality inequalities above replace it.

Fresh R cases also confirm actual status/attainment behavior: seed 1, `rgamma(40,2)` selects beta-prime gamma; seed 2 selects inverse gamma. Seed 1 `rexp(200)` selects snorm half-normal, SSTD half-t df≈6.409, and SGED half-GED. Seed 2 `rnorm(200)` selects SSTD snorm and STD normal. Seed 4 `c(rnorm(40),30)` returns attained GED p=1 and attained SGED p=1,xi≈1.937. `seq(0,1,length.out=30)` selects uniform for GED and SGED. Fixed-df strict tie/equality diagnostics match their advertised statuses. Seed 19, `c(rep(0,30),rpois(70,3))` selects the ZINB ZIP face; seed 7 `runif(50)` returns GPD xi=−.5. Seed 5 `abs(rnorm(30))+2` gives hnorm stored-minus-reference logLik **7.1e−15**, endpoint shift **−4.44e−16**. `mlkumar(c(1e−12,1e−8,.01,.5,.99,1−1e−8,1−1e−12))` returns finite a≈.02313,b≈.09972.

The report's counterexamples are self-contained above, so they do not depend on preserving `/tmp`. No source/golden changes or commits were made.

An additional fresh R check confirmed `sads::dzipf(1:3,N=3,s=0)` gives three NaNs. If the attained Zipf uniform member is exposed, this reference-package defect needs the established runtime-checked workaround mechanism; it is not a reason to claim mathematical nonexistence. Seed 14 reciprocal Weibull selects inverse-Burr inverse-Weibull; seed 17 Pareto-like data selects Burr Pareto; seed 16 scaled power data selects inverse-Burr power, confirming the advertised D1 messages.
