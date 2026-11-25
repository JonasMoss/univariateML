# Integer-size profiles and the zero-truncated binomial moment test

TASK-293, base revision `ba3aca13`. A directed interval certificate on a compact rate range completes the proof that the moment envelope based on $G_N(\nu)$ is strictly below the mean-matched positive-Poisson face at every feasible finite size. At or above the moment boundary, the finite profile increases strictly toward the face. For every lower-only window with positive lower endpoint, integer unimodality holds when the matched Poisson rate is at most $6/5$. The exhaustive small-histogram search found no reversal. Outside the proved regimes the shape question remains open. Exact size ties occur, including in pure zero truncation.

This report uses the earlier profile, convergence, first-order coefficient, adjacent-count case and algebraic comparison results. The [previous report](2026-11-25-tbinom-math-report.md) gives those results and the $G_N$ reduction. The [size search report](2026-10-31-size-search-report.md#zero-truncated-binomial), [conditional-Poisson tail proof](2026-11-07-binom-faces-report.md#conditional-poisson-tail-proof), [archive cases](2026-11-20-route-acc-report.md#truncated-binomial-archive-cases) and [binomial dossier](../../../univariateML-cpp/include/cppunivariate/distributions/binom/README.md) describe the existing routes. DeRiggi's abstract describes a local maximum and a saddle for the positive binomial. The abstract supplies no sample or comparison at consecutive integers. [Article](https://doi.org/10.1016/0167-7152(94)90061-2), [correction](https://www.sciencedirect.com/science/article/pii/S0167715296000028).

## Lower-only integer profiles at small matched rates

**Proved.** Let the retained counts start at $L\ge1$, let $M$ be the observed maximum, and consider nonconstant data with both size and probability free. Write $P$ for the empirical law, $Y=X-L$, $m=E_PY$, $H=N-L$, $s=1/H$, and $z=Hp/(1-p)$. Let $Q$ be the conditional Poisson with rate $\lambda$ and mean $\mu=L+m$. Use log likelihoods per observation. The proof also applies to real histogram proportions.

For $H\ge M-L$ the shifted normalizer has the positive integral continuation
$$
 A_s(z)=L\int_0^1 (1-u)^{L-1}\exp F(s,zu)\,du,
 \qquad F(s,x)=\int_0^x\frac{dt}{1+st}.
$$
At integer $H$, expanding $(1+zu/H)^H$ gives the coefficient
$\prod_{j<y}(1-js)/(L+1)^{\overline y}$ of $z^y$, where the denominator is the rising factorial. At $s=0$ it gives the shifted Poisson weights. At real $H$ this integral serves to compare the integer profiles.

Let $\eta=\log z$, $\gamma=\log(1-s)$, and assume $M-L\ge2$. For each $u\in(0,1)$ the $\eta\eta$ entry of the Hessian of $F$ is $x/(1+sx)^2>0$, with $x=e^\eta u$. The Schur complement of that entry is
$$
 (1-s)\int_0^x\frac{t-(1-s)t^2}{(1+st)^2}\,dt.
$$
Indeed $F_s=-\int_0^x t(1+st)^{-2}dt$, $F_{s\eta}=-x^2/(1+sx)^2$, and the derivative of $t^3/(1+st)^2$ is $t^2/(1+st)^2+2t^2/(1+st)^3$. The unweighted integral of the numerator is positive for $0<x<3/[2(1-s)]$. The numerator changes sign at most once, from positive to negative. The decreasing weight $(1+st)^{-2}$ preserves positivity by comparing it with its value at that crossing. Thus $F$ is strictly jointly convex on $\eta+\gamma\le\log(3/2)$, including the boundary for $0<u<1$. Integration followed by Hölder's inequality gives strict joint convexity of $\log A_s(e^\eta)$ on the same region. Each observed $\log(1-j+j e^\gamma)$ in the log kernel is concave there for $j\ge1$, since its second derivative is $j(1-j)e^\gamma/(1-j+j e^\gamma)^2$. The log likelihood is therefore strictly jointly concave on this region for the feasible $\gamma$ interval.

To place the optimum inside the region, tilt the integration law of $u$ by $\exp F(s,zu)$ and set $W=zu/(1+szu)$, with upper endpoint $b=z/(1+sz)$. Its mean is the derivative of $\log A_s(z)$ in $\log z$. For finite $H$, its density on $[0,b]$ is proportional to
$$
 (b-w)^{L-1}(1-sw)^{-H-L-1}.
$$
At $s=0$, the density with the same $b$ is proportional to $(b-w)^{L-1}e^w$. The finite-to-limit ratio increases strictly in $w$ because its logarithmic derivative is $(H+L+1)/(H-w)-1>0$. At $b=\lambda<H$ the finite mean therefore exceeds $m$. The mean is strictly increasing in $z$ because $F$ is strictly convex in $\eta$ and the log integral preserves strict convexity. Thus the mean-matching optimum has $b<\lambda$ at finite $H$. If $\lambda\le6/5$ and $s\le1/2$, then $z(1-s)=b(1-s)/(1-sb)\le3/2$. For $b\le1$ the ratio is at most $b$, and for $b>1$ it is at most $b/(2-b)$.

The segment between any two mean-matching optima stays in the joint concavity region. Evaluating along that segment and then profiling proves strict concavity of the profile in $\gamma$. The grid $\gamma_N$ is strictly increasing. Its normalized successive differences strictly decrease, so there is at most one zero step and a strict decline excludes all later sizes. For $M=L+1$, the earlier adjacent-count convex-order result gives strict decrease from the first feasible size. Finally the derivative at $\gamma=0$ equals
$$
 \frac{g}{2},\qquad g=E_P(X)_2-E_Q(X)_2,
$$
by the earlier first-order coefficient and $1/N=-\gamma+O(\gamma^2)$. Thus $g\ge0$ gives strict increase to the face in the small-rate concavity regime. Negative $g$ ensures a finite winner above the face by the first-order result.

## The moment envelope for pure zero truncation

**Proved.** Suppose $L=1$, $\lambda>0$, and $\mu=\lambda/(1-e^{-\lambda})$. Let $m=\mu-1$, $h=\mu-\lambda$, and $\nu=\lambda\mu/m$. Feasibility for $g\ge0$ requires $N\ge\nu$, because the law tilted by $(X-1)/m$ then has mean at least $\nu$. Write $k=\lfloor\nu\rfloor$, $r=\nu-k$. The $G_N(\nu)$ bound is attained among real histograms at the boundary $g=0$ by
$$
 P_*\{k\}=\frac{m(1-r)}{k-1},\qquad
 P_*\{k+1\}=\frac{mr}{k},\qquad
 P_*\{1\}=1-P_*\{k\}-P_*\{k+1\}.
$$
A zero weight can be omitted. These are nonnegative since
$$
 \nu-\mu-1=\frac{1-\mu h}{m}>0,\qquad
 \mu h=\left(\frac{\lambda}{2\sinh(\lambda/2)}\right)^2<1.
$$
In particular $k>\mu$. The reweighted law is exactly the interpolation between $k$ and $k+1$ in the earlier $G_N$ bound. It suffices to prove that every finite profile for $P_*$ is strictly below the face. We prove strict increase to the face for $P_*$ and for every $P$ with $g\ge0$.

### Rates at most $6/5$

Every feasible $N$ is at least 3 because $\nu>\mu+1>2$. The small-rate concavity proof applies, with face derivative zero for $P_*$ and nonnegative for each $P$ under consideration.

### Rates from $6/5$ to $3$

In the shifted integral, $W_0$ at the Poisson optimum has density $e^w/(e^\lambda-1)$ on $[0,\lambda]$. Write $w_j=E W_0^j$. The first two derivatives of $A_0(z)$ in $z$ give $w_1=m$ and $w_2=E_Q(Y)_2=\lambda\mu-2m$. Thus $\nu=2+w_2/w_1$.

At the optimum for $s>0$, $W_s$ has the same mean as $W_0$ and the earlier density ratio argument gives $b<\lambda$. Here $H>\lambda$ along the continuation from any feasible size because $N\ge\nu>\mu+1$. The ratio of the densities increases on their common support and is zero above $b$. Equal mass and mean force the difference of the densities to have signs minus, plus, minus. Subtracting the affine chord through the crossings from any convex function proves $W_s\le_{\rm cx}W_0$.

The derivative in $s$ of the profiled rate-and-normalizer term $m\log z-\log A_s(z)$ is
$$
 E\int_0^{W_s}\frac{t}{1-st}\,dt
 \ \le\ E\int_0^{W_0}\frac{t}{1-st}\,dt.
$$
For the identity, differentiate at the optimizing $z$, where its score is zero, and in the resulting integral for $-F_s$ change variables from $x$ to $x/(1+sx)$. The inequality uses the convexity of the integrated kernel. The observed falling-factorial correction has derivative $-E_P\sum_{j<Y} j/(1-sj)$. For an increasing convex sequence $a_j$, its prefix averages $y^{-1}\sum_{j<y}a_j$ have increasing nonnegative increments. Twice each increment is a weighted average of the successive differences of $a_j$, with weights proportional to $j+1$. Jensen under the $Y/m$ tilt therefore gives the smallest correction cost at the two adjacent points bracketing the tilted mean $\nu-1$. The same lower bound holds at larger tilted means because the interpolant is increasing. Here the lower bound is attained by $P_*$.

Set $C_q=E_{P_*}\sum_{j<Y} j^q$. For $\nu\le5$, interpolation of the four prefix averages, at $Y=1,2,3,4$, gives
$$
 C_q=\frac{w_2}{2}
 +\frac{(w_2-w_1)_+}{3}(2^q-2)
 +\frac{(w_2-2w_1)_+}{12}(1-5\cdot2^q+3\cdot3^q),\qquad q\ge1.
$$
The terms of order zero in the two derivative series agree since $C_1=w_2/2$. For $s>0$, the difference between the correction cost derivative and the upper bound on the profiled normalizer gain derivative, divided by $s$, is at least
$$
 \sum_{q=2}^{32}s^{q-2}\left(C_q-\frac{w_{q+1}}{q+1}\right)
 -\frac{w_{34}s^{31}}{34(1-s\lambda)}. \tag{1}
$$
The empirical series remainder is nonnegative. The other remainder uses $W_0\le\lambda$ and a geometric sum. Formula (1) extends to $s=0$.

**Directed interval certificate.** The attached checker proves that the expression in (1) is positive on whole boxes, including their boundaries. Let $H_0$ be the starting feasible integer $N-1$ and continue over $H\ge H_0$. The ratio $w_2/w_1$ is the mean under density proportional to $we^w$ cut off at $\lambda$, so it increases in $\lambda$. Directed comparisons give $\nu(7/5)>3$ and $\nu(13/5)>4$. If $H_0=2$ we can omit the last two terms in $C_q$, and if $H_0=3$ we can omit the last one, because feasibility at $H_0$ makes those terms zero. The third rectangle keeps both terms. The checker also encloses $\nu<5$ throughout.

| Starting size $H_0$ | Rate range checked | Range of $s$ checked | Boxes | Smallest lower endpoint, rounded down |
|---|---|---|---:|---:|
| 2 | $[6/5,7/5]$ | $[0,1/2]$ | 6,400 | 0.11007 |
| 3 | $[6/5,13/5]$ | $[0,1/3]$ | 44,800 | 0.11184 |
| At least 4 | $[6/5,3]$ | $[0,1/4]$ | 28,800 | 0.11184 |

The rate intervals have width $1/500$, and the inverse-size intervals evenly divide each range into 64, 64 and 32 pieces, respectively. The checker encloses $w_j$ using
$$
 w_j=\frac{\lambda^{j+1}}{e^\lambda-1}
      \sum_{i=0}^{\infty}\frac{\lambda^i}{i!(j+i+1)}.
$$
It sums through degree 45 and bounds the omitted exponential terms by $3^{46}/[46!(1-3/47)]$. The same bound divided by $j+47$ covers the omitted integral terms. With those bounds the checker uses only basic directed interval operations on enclosures of rational numbers, including a Horner evaluation for (1). Hence the derivative in $s$ is strictly negative for every positive $s$ in the feasible continuation. [Interval certificate](2026-11-25-tbinom2-math-data/face_interval.py).

### Rates at least $3$

Use $\sigma=1/N$ here, and let $v=Np_N$ be the fitted unconditioned mean. The real continuation of the normalizer is positive by the preceding integral, and the score equation is
$$
 v=\mu(1-e^{-\tau}),\qquad
 \tau=-\frac{\log(1-\sigma v)}{\sigma},\qquad
 \lambda\le v<\mu .
$$
The last bounds follow because at fixed $v$, $\tau\ge v$, so the conditional mean $v/(1-e^{-\tau})$ is at most its Poisson counterpart. The score mean increases strictly in the odds. Write
$$
 T(a)=\int_0^1\frac{t}{1-at}\,dt,qquad
 \tau-v=\sigma v^2 T(\sigma v).
$$
At fixed $\tau$ the unshifted normalizer $(1+\sigma z)^{1/\sigma}-1=e^\tau-1$ is fixed, where in this paragraph $z=Np/(1-p)=(e^{\sigma\tau}-1)/\sigma$. Differentiating the profiled rate-and-normalizer term in $\sigma$ thus gives $\mu vT(\sigma v)$. Also
$$
 v-\lambda
 =h(1-e^{-(v-\lambda)})+\mu e^{-v}(1-e^{-(\tau-v)})
 \le h(v-\lambda)+h\sigma v^2T(\sigma v).
 \tag{2}
$$

The derivative of the empirical correction cost is $E_P\sum_{j<X} f(j)$, with $f(t)=t/(1-\sigma t)$. The trapezoid bound for convex $f$ gives $\sum_{j<k} f(j)\ge\int_0^{k-1}f(t)dt+f(k-1)/2$. Divide this bound by $k-1$ and apply Jensen under the $(X-1)/m$ tilt. The resulting function of $k-1$ is increasing and convex, as follows by expanding the positive power series of $f$. Set $K=\nu-1>\mu$. The empirical derivative is at least
$$
 m\left[(K+1)T(\sigma K)+\frac{\sigma K}{2}T'(\sigma K)\right].
$$
Here $KT(\sigma K)$ is the averaged integral term, and $1/[2(1-a)]=T(a)+a T'(a)/2$. Using $K>v$, monotonicity of $T'$ and $m(K+1)=\mu\lambda$, the difference from the model gain is at least
$$
 m\sigma[(K+1)(K-v)+v/2]T'(\sigma v)
 -\mu(v-\lambda)T(\sigma v).
$$
For $0\le a<1$, $T'(a)/T(a)^2\ge4/3$. In fact $T'=\int (t/(1-at))^2dt$, so Cauchy–Schwarz applied to the first, second and third moments makes $T'/T^2$ nondecreasing, with value $4/3$ at zero. By (2), the derivative gap is strictly positive if
$$
 \frac{h\mu^2}{1-h}<\frac43\left[\frac{\lambda(1-\mu h)}{m}+\frac m2\right]. \tag{3}
$$
To obtain this sufficient condition, replace $v$ by $\mu$ on the increasing side $hv^2/(1-h)$ and on the decreasing side $(4/3)\lambda[K-v+v/(2(K+1))]$.

For $\lambda\ge3$, $h$ decreases, and $h\mu^2=\mu^3e^{-\lambda}$ decreases because $\mu'/\mu=1/\lambda-1/(e^\lambda-1)<1/3$. Since $e^3>20$ by the exponential series, the left side of (3) is at most $10800/(16\cdot361)<2$. Throughout the range $\mu h\le180/361<1/2$, since $\bigl(\lambda/(2\sinh(\lambda/2))\bigr)^2$ decreases. Also $m>2$ and $\lambda>m$. The right side is therefore greater than 2. This proves the strictly negative profile derivative in $\sigma$. Together the three rate ranges prove the strict inequality for $G_N(\nu)$ and strict increase in integer size whenever $g\ge0$.

## Integer reversal searches

**Proved on the enumerated grid.** For each $L=1,\ldots,6$, each observed maximum $M=L+1,\ldots,L+5$, and each sample length $2,\ldots,8$, the exhaustive checker takes every nonconstant histogram on $L,\ldots,M$ with positive count at $M$. It evaluates sizes $M,\ldots,M+12$. There were 17,724 histograms and 212,688 consecutive differences, with zero unresolved signs and zero strict declines followed by any later rise. Each fitted odds root was bracketed by 145 dyadic refinements using exact integer signs of the shifted score polynomial. Comparison used 55-decimal-digit directed log-likelihood intervals. [Exhaustive checker](2026-11-25-tbinom2-math-data/histograms.py).

**Numerical.** The broader fixed-mean screen used 1,890 settings across nine lower endpoints from 1 to $10^6$, ten widths from 2 to 32, and 21 relative means from $10^{-5}$ to $1-10^{-5}$. At each mean it takes the convex hull of the two-atom histogram vertices and tests every pair among 12 steps with lower sizes $M$ through $M+100$ at the offsets in the script. It found zero verified reversals over 124,740 comparisons. The screen uses binary64 with a $10^{-11}$ per-observation threshold on each step, and routes any proposed histogram to the interval verifier. Separately, an 80-digit numerical evaluation of the $G_N$ bound covered 1,740 rate-size pairs, with rates from $10^{-9}$ to $10^4$ and sizes from $\lceil\nu\rceil$ to $10^8\lceil\nu\rceil$. Its largest per-observation excess was approximately $-9.25926\times10^{-37}$. [Wider reversal screen](2026-11-25-tbinom2-math-data/screen.py), [moment-envelope cross-check](2026-11-25-tbinom2-math-data/face.py).

**Proved reduction for the remaining signs.** At fixed $m$, write $V_H$ for the optimized shifted rate-and-normalizer term using unscaled odds and weights $(H)_y/(L+1)^{\overline y}$. Put $a_H=V_H-V_{H+1}$. The step is $E_P d_H(Y)-a_H$, where $d_H(y)=\log[(H+1)/(H+1-y)]$. For $y>0$, $q_H(y)=d_H(y)/y=\int_0^1dt/(H+1-ty)$. For $J>H$, $q_J$ is strictly concave as a function of $q_H$ on the common positive support. Indeed the logarithmic derivative of $q'_H$ in $y$ is the expectation of $2t/(H+1-ty)$ under weights proportional to $t/(H+1-ty)^2$. Both the integrand and the tilt toward larger $t$ increase as $H$ decreases. Thus $q'_J/q'_H$ decreases in $y$. Under the $Y/m$ tilt, a nonpositive step at $H$ implies $E q_H\le a_H/m$. The increasing concave polygon through $(q_H(y),q_J(y))$ for integer $y\ge1$ then bounds $E q_J$ by its value at $a_H/m$ within the knot range. Above the range the last knot is an upper bound and below it the premise is infeasible. A uniform comparison of that bound against $a_J/m$ would prove sign ordering. If its extremal adjacent-knot weights leave a nonnegative mass at zero upon undoing the tilt, the bound is attained at the fixed mean.

## Exact size ties

**Proved for a lower-only window.** In the positive-binomial model the exact sample $(1,1)$ has likelihood one for each $N\ge1$ at $p=0$, using the model's closure at zero. Indeed the probability of one is $\binom N1 p(1-p)^{N-1}/[1-(1-p)^N]\to1$ as $p\downarrow0$. For $N=1$ the support is a singleton and all $p\in[0,1]$ give this maximum. For $N>1$, interior probabilities also assign mass to counts above one, and $p=1$ gives the atom at $N$. The exact maximizing set is
$$
 (\{1\}\times[0,1])\ \cup\ ((\mathbb Z\cap[1,\infty))\times\{0\}).
$$
These are the two typed `Box` pieces already used by the zero-truncated binding. [Binding](../../../bindings/common/distributions/binom/ztbinom_fit.hpp).

**Proved with nonconstant data on a bounded window.** Hold the window at $[1,2]$ and take $(1,1,2)$. At every $N\ge2$ the conditional odds of 2 against 1 are $(N-1)p/[2(1-p)]$. Taking $p=1/N$ fits the empirical probabilities $2/3,1/3$ exactly and attains the multinomial likelihood bound. The full set is the graph of $Np=1$ over the integer axis $N\ge2$, with probability in $\left(0,1/2\right]$. This is a mixed integer-real relation. The current `Product` relation requires real axes and would need an extension for this graph. [Set schema](../../../univariateML-cpp/include/cppunivariate/core/maximizer_set.hpp).

For a nonconstant lower-only sample any attained maximizing set of sizes is finite. The prior result excludes a finite-to-face equality, and convergence to a strictly lower face bounds the set of maximizers. Each maximizing size has just one fitted probability. The prior algebraic comparison decides equality of any two finite likelihoods from the rational functions of their isolated odds roots. Interval overlap alone does not establish equality.

## Reproduction

All four final Python runs exited successfully on the workstation, each through one reserved CPU slot. From the worktree root run

```sh
export TMPDIR="$PWD/build/tmp"
mkdir -p "$TMPDIR"
data=dev/research/evaluations/2026-11-25-tbinom2-math-data
/tmp/umvl-cpu/run python3 -B "$data/histograms.py"
/tmp/umvl-cpu/run python3 -B "$data/screen.py"
/tmp/umvl-cpu/run python3 -B "$data/face.py"
/tmp/umvl-cpu/run python3 -B "$data/face_interval.py"
```

## What this changes for the fitter

- For nonconstant data with both size and probability free and effective window $[1,\infty$), compare $T=\sum X(X-1)/\sum X$ with the matched rate $\lambda$. A strictly positive padded comparison $T>\lambda$ proves the global escape without a finite prefix or a tail cutoff. The same sign follows from comparing $T/(1-e^{-T})$ with the sample mean. Return certified `PoissonLimit` for zero truncation or `TpoisLimit` for the matching window route. A strictly negative comparison proves that a finite winner exists above the face and leaves its size to the search. Exact equality is excluded for a nonconstant integer histogram by the prior transcendence argument. The all-ones case takes the attained set above.
- For any effective lower-only window with $L\ge1$, both parameters free and a certified upper bound $\lambda\le6/5$, successive step signs order the whole profile when $M\ge L+2$. A certified nonpositive step bounds the search on the right. A certified positive step bounds it on the left. Exact equality of adjacent sizes identifies the two co-maximizers. For $M=L+1$, the first size is the unique winning size by the earlier adjacent-count result. In either case, check the fitted probability at every returned size against the coefficient accuracy contract.
- Outside the proved shape cases retain global coverage for a finite-winner certificate. If a comparison or the certificate budget is inconclusive, preserve the successful accuracy-checked, unrefuted search result with certification withdrawn. A refuted candidate requires correction or the appropriate stop if the search cannot recover.
- Encode isolated exact ties as all the winning coefficient vectors in typed `Points`, with probability enclosures as needed. Use `Box` pieces for the all-ones example and an integer-indexed relation for a flat graph such as $p=1/N$. Mark the outcome as an attained estimate with known nonuniqueness and suppress ordinary single-estimate inference. Keep infinite window endpoints as design metadata when the typed point schema requires finite coordinates. A finite capacity would need extending if the proved maximizing set exceeded it.

## Open

1. **General lower-only integer unimodality.** The proofs cover small matched rates and the escaping moment regime of pure zero truncation. For the remaining rates and moment regimes, the missing step is a uniform comparison of successive signs, for example the polygon bound against $a_J/m$ above or a tighter bound that enforces nonnegative mass at zero. Alternatively, a strict crossing in the two-step histogram hull at a feasible mean would give a rational histogram counterexample after interval separation. Neither search yielded such a crossing.
2. **Nonconstant lower-only exact ties.** The attained lower-only example is constant, and the nonconstant example uses a bounded window. Whether two distinct integer sizes can tie at the maximum for a nonconstant lower-only histogram remains unsettled. For small rates only adjacent sizes can tie, so it would suffice there to prove algebraic inequality of the neighboring maximizing likelihoods or to isolate a shared value for a rational histogram. The finite search found no adjacent equalities in its grid.
