# GEV global ties, shape roots and held coefficients

TASK-319 · 2026-11-25 · Free shape in \([-1,1]\)

**Proved.** A four-observation sample has exactly two global maxima, one at the reverse-exponential boundary \(\xi=-1\) and one at \(0.32<\xi<0.42\). Separate constructions prove global nonuniqueness with scale held and with location held. The fixed-shape joint fit is unique whenever attained on nonconstant data. An explicit bound counts distinct stationary shapes on either open nonzero half of the all-free profile, including degenerate roots.

## Setup and prior results

Write \(m=\min x\), \(M=\max x\), \(\bar x=n^{-1}\sum_i x_i\), and \(k=\#\{x_i=m\}\). Values below are average log likelihoods unless specified. For \(\xi>0\), the Fréchet coordinates are shape \(1/\xi\), location \(\mu-\sigma/\xi\), and scale \(\sigma/\xi\). For \(\xi<0\), the reflected-Weibull coordinates are shape \(-1/\xi\), upper endpoint \(E=\mu-\sigma/\xi\), and scale \(-\sigma/\xi\). The prior monotone gap equations give a unique nuisance fit at each shape where it exists. Gumbel is an attained analytic seam. The lower shape bound is the attained reverse exponential at \((\mu,\sigma)=(\bar x,M-\bar x)\) in the all-free problem. The “signed uniform strip” in the uniqueness audit refers to this reverse-exponential member. The sampling regimes from the inference report leave these finite-sample comparisons to be decided separately. [Joint profile proof](2026-11-08-gev-allfree-data/joint-proof.md) · [Gap and two-point proofs](2026-11-08-gev-allfree-data/two-point-proof.md) · [Uniqueness audit](2026-11-25-uniq-cover-math-report.md#joint-shape-covers) · [Inference regimes](2026-11-25-infer2-math-report.md#extreme-values) · [GEV dossier](../../../univariateML-cpp/include/cppunivariate/distributions/gev/README.md)

Fréchet ties carry over as equal candidate values in the GEV positive-shape chart. The earlier finite–Gumbel construction \(x=(0^4,1^5,r)\), \(7/5\le r\le3/2\), has strictly negative shape score at zero throughout. At its Fréchet equality sample, small negative GEV shapes therefore beat both tied values. Any global GEV tie involving Gumbel requires a zero seam score and a global comparison on both sides. [Fréchet interval proof](2026-11-25-frechet-math-report.md)

## Exactly two all-free global maxima

**Proved by interval comparisons and continuity.** Use \(x(t)=(0,t,1,2-t)\) and
\[
I=[29494/100000,29497/100000],\qquad J=[8/25,21/50].
\]
On \(I\) the reverse-exponential value is \(R(t)=-1-\log(5/4-t)\). The all-free profile \(P(\xi,t)\) has inward endpoint scores and strictly negative curvature on \(J\), so it has one stationary shape \(b(t)\) there. The comparisons below exclude all shapes outside \(J\) except \(-1\). The difference \(P(b(t),t)-R(t)\) is positive at the lower end of \(I\) and negative at the upper end. Define \(t_*\) as the least zero of that continuous difference in \(I\). Then \(x(t_*)\) is an exact sample with precisely two global fits. They are \((3/4,5/4-t_*,-1)\) and the unique nuisance fit at \(b(t_*)\). At the two rational endpoints of \(I\) the winner is unique, interior on the left and reverse exponential on the right. [Executable interval proof](2026-11-25-gev-math-data/tie_certificate.py)

Use the prior pivot objective
\[
F=-\log v-\log\bar w-(1+\xi)\bar L-1,\qquad
v=\sigma+\xi(m-\mu),\quad z_i=(x_i-m)/v,\quad
L_i=\log(1+\xi z_i)/\xi,\quad w_i=e^{-L_i}.
\]
At zero \(L_i=z_i\). For \(\xi\ge-3/5\) use nuisance coordinate \(\log v\). For \(-99/100\le\xi\le-3/5\) use the reflected-Weibull profile in \(a=-1/\xi\) and \(e=E-M>0\). The shape change has positive derivative. Two opposite interval signs of the nuisance score uniformly enclose the unique nuisance optimizer in each rational data/shape rectangle. The pivot evaluation bounds \(L\) and \(L_\xi\) by monotone corners using the integrals of \((1+\xi s)^{-1}\) and \(-s(1+\xi s)^{-2}\) from zero to \(z\). On boxes with negative nuisance curvature the profile curvature is \(F_{\xi\xi}-F_{\xi,\log v}^2/F_{\log v,\log v}\). Point solves only propose brackets. [Prior jet derivation](2026-11-08-gev-allfree-data/joint-proof.md#minimum-pivot-and-derivatives)

For \(-1<\xi\le-99/100\), the negative gap score at \(e=1/100\) and monotonicity of \(e(a)\) give \(e<1/100\). At \(a=1\) the value at this gap is \(-1-\log(M-\bar x+e)<R(t)\). The tangent in \(a\) at fixed \(e\) has slope
\[
1+\overline{\log(e+M-x_i)}-
\frac{\sum_i(e+M-x_i)\log(e+M-x_i)}{\sum_i(e+M-x_i)}.
\]
The checker bounds the slope strictly below zero, using \(\log(1/100)\) for the zero-distance observation's log upper and \([(1/100)\log(1/100),0]\) for its \(e\log e\) term. Concavity in \(a\) makes the tangent an upper bound, so every interior point of the strip loses strictly to the face.

| Shape interval | Verified on all of \(I\) | Cells |
|---|---|---:|
| \([-0.99,-0.8]\) | Negative shape score or strict loss to \(R\) | 23 |
| \([-0.8,-0.6]\) | Strict loss to \(R\) | 59 |
| \([-0.6,0]\) | Strict loss to \(R\) | 1,555 |
| \([0,0.32]\) | Positive shape score or strict loss to \(R\) | 2,665 |
| \([0.42,1]\) | Negative shape score or strict loss to \(R\) | 1,337 |
| \(0.32,\ 0.42\) | Strict positive and strict negative scores, respectively | 1 each |
| \([0.32,0.42]\) | Strict negative profile curvature | 2,047 |

The first row starts below \(R\). A first upward crossing of \(R\) would contradict the negative score wherever the value reaches \(R\). At \(\xi=1\) a negative score points to a better feasible shape on the left. In the positive interior a nonzero score excludes a global maximizer. The data-endpoint difference enclosures are \([1.66914,1.67332]10^{-5}\) and \([-9.50232,-9.46058]10^{-6}\), widened for display. The checker uses 40-decimal-digit interval operations and exact rational subdivision endpoints. Numerically \(t_*\) is near \(0.2949591396535524\) and \(b(t_*)\) near \(0.3674826185927520\). The least-zero definition does not require a count of zeros in \(t\).

## Global ties with public scale or location held

**Proved.** A uniform losing strip also applies under each hold when the constrained reverse-exponential maximizer is unique and occurs at \(E=M\) along a compact family with a common compact likelihood superlevel. Write \(a=-1/\xi\). For held scale compare at the same \(E,\sigma\) and put \(z_i=(E-x_i)/\sigma\). The respective log densities at \(a\) and 1 are
\[
-\log\sigma+(a-1)\log(z_i/a)-(z_i/a)^a,\qquad -\log\sigma-z_i .
\]
The \(a\)-derivative of the power term is bounded uniformly on bounded \(z_i\ge0,\ 1\le a\le2\), including zero by continuity. At least one \(z_i=(E-M)/\sigma\). Hence for \(E-M>0\) small, the average difference is bounded above by \((a-1)[C+n^{-1}\log(E-M)]\) for a uniform finite \(C\). For held location compare at the same \(E,s\) with \(s=a\sigma=E-\mu\). With \(z_i=(E-x_i)/s\) the difference is \(\log a+(a-1)\overline{\log z}-\overline{z^a-z}\), yielding the same bound. At contact the interior shape has zero likelihood. For gaps bounded away from zero, failure of a uniform losing strip would yield by compactness a second face maximizer with \(E>M\), contrary to face uniqueness.

For held scale the prior density bound \(f(x)\le\min(\sigma^{-1},\sigma/(x-\mu)^2)\) gives compact containment of the location superlevel. For held location in the construction below, \(t\ge10^{-4}\) and \(\mu=3t/4\), so zero is uniformly to the left of the hold. More generally, for \(x<\mu\) write \(u=(\mu-x)/\sigma\) and \(y=(1-\xi u)^{-1/\xi}\). In support \(\log y=\int_0^u (1-\xi s)^{-1}\,ds\ge\log(1+u)\). For \(u\ge1\), \(h_\xi=y^{1+\xi}e^{-y}\le(1+u)^2e^{-(1+u)}\) by monotonicity of \(y^2e^{-y}\) beyond 2. This expression also bounds the zero density off support. It beats the other inverse-scale factors as \(\sigma\downarrow0\), and large \(\sigma\) loses by the common \(\sigma^{-1}\) bound. Thus the held-location superlevel is compact uniformly on the sample slice. [Prior constrained proof](2026-11-10-gev-held-data/proof.md#gev-compact-nuisance-regions)

A strict exceedance of the face persists locally because the better fit is in strict support and the face value is continuous. At a transition, a subsequence of better-than-face maximizers has a compact limit outside the losing strip. Upper semicontinuity of the likelihood makes this limit a global maximizer along with the face. Near an endpoint with a unique face winner, compactness and the strip exclude better-than-face fits by the same subsequence argument.

- **Scale held.** Hold \(\sigma=1\) on \(x=(0,r,r,r)\), \(4\le r\le100\). The face fit is \((r-1,1,-1)\) with value \(-r/4\). At \(r=4\) the hold contains the unique all-free two-point winner. At \(r=100\), the feasible \((\mu,\xi)=(0,1)\) gives total log likelihood \(-1-6\log101-3/101>-32\), above the face's \(-100\). Define \(r_S\) to be the infimum of the data parameters where a fit strictly beats the face. The strip and compactness imply \(4<r_S<100\). The exact sample at \(r_S\) has the face and at least one other finite global maximizer.
- **Location held.** On \(x=x(t)\), \(10^{-4}\le t\le1\), hold \(\mu=3t/4\). At \(-1\) the scale objective \(-\log\sigma-1+(\bar x-\mu)/\sigma\) decreases on \(\sigma\ge M-\mu\) since \(\bar x=3/4\ge\mu\). The face scale is \(2-t-3t/4\). At \(t=1\) the hold contains the unique all-free winner. At \(t=10^{-4}\), \((\sigma,\xi)=(1/100,1)\) beats even the all-free face. For the first two observations the positive-shape support arguments lie in \([1/2,1.01]\), giving densities above 30 by monotonicity of \(100u^{-2}e^{-1/u}\) there. The other support arguments lie in \([100,101]\) and \([200,201]\), giving densities above \(0.009\) and \(0.002\). The product exceeds \(0.0162\), whereas the all-free face product is below \(0.3^4\). Define \(t_L\) as the supremum of parameters where a fit strictly beats the constrained face. Then \(10^{-4}<t_L<1\) and the exact sample at \(t_L\) has the face and at least one other finite global maximizer. [Witness checks](2026-11-25-gev-math-data/witnesses.py)

## A bound on the all-free shape root count

**Proved.** Let \(d\) be the number of distinct observations and \(q=3d+2\). Each open nonzero shape half has at most \(18\,2^{q(q-1)/2}13^q\) distinct stationary points where the nuisance maximum exists. Use \(a=1/|\xi|>1\) and the gap \(e>0\). Write \(w_i=e+d_i\) where \(d_i=x_i-m\) on the positive side and \(d_i=M-x_i\) on the negative side. Put \(r_i=1/w_i\), \(l_i=\log w_i\), \(u_i=w_i^{\pm a}\), with \(+\) on the negative side and \(-\) on the positive side. With counts included in averages, the profiles are
\[
F_-=\log a+(a-1)\bar l-\log\bar u-1,\qquad
F_+=\log a-(a+1)\bar l-\log\bar u-1 .
\]
With \(W=\sum_i u_i\) including counts, \(WF_e\) and \(aWF_a\) are polynomial expressions of degree at most three in \(a,r_i,l_i,u_i\). On coordinates \(a=1+e^A,\ e=e^B\) in \(\mathbb R^2\), the two exponentials followed by the \(r_i,l_i,u_i\) form a Pfaffian chain of length \(q\) and differential degree at most four. For example \(\partial_B r_i=-er_i^2\), \(\partial_B l_i=er_i\), \(\partial_A u_i=\pm(a-1)l_i u_i\), and \(\partial_B u_i=\pm aer_i u_i\). The two-equation root bound with degrees \(3,3\) is \(9\,2^{q(q-1)/2}13^q\) for nondegenerate solutions. [Khovanskiĭ's bound, Theorem 2.11](https://www.cambridge.org/core/journals/forum-of-mathematics-sigma/article/partitioning-theorems-for-sets-of-semipfaffian-sets-with-applications/6432BD9F49B22A084E11A151E139743B)

The derivative of the monotone gap ratio is strictly signed, so \(F_{ee}<0\) at its unique root. The optimized \(F_a\) is analytic on each open half where that root exists. Along the negative branch \(e(a)\to0\) as \(a\downarrow1\), by the gap equation. Then \(F_a=1/a+\bar l-\sum_i u_i l_i/W\to-\infty\), since maximum observations contribute \(\log e\) to \(\bar l\) while their \(w_i^a\log w_i\) terms tend to zero. This precludes an identically zero score. At zero shape the pivot objective is analytic near its optimum and its inverse-scale curvature is strictly negative. The implicit function theorem joins the two analytic shape profiles through zero, so the positive profile is nonconstant as well. Its nuisance-attainment interval \(a(n-k)>k\) is connected.

For degenerate roots take any finite collection of disjoint compact intervals around distinct zeros of the optimized \(F_a\). By analyticity, each interval can have a nonzero endpoint value. For sufficiently small \(\delta>0\), either \(+\delta\) or \(-\delta\) is attained in each interval. Choose \(\delta\) so both levels are regular there, since the nonconstant analytic score has finitely many critical points on the chosen intervals. The equations \(WF_e=0\) and \(aW(F_a\pm\delta)=0\) retain their degree bounds. At a perturbed solution, nonzero nuisance curvature and nonzero derivative of the optimized score give a nonsingular Jacobian. Adding the bounds for the two regular levels bounds any finite collection of distinct roots. There is at most one additional seam shape; shape endpoints are counted separately.

## Existence and faces under each single hold

### Shape held

**Proved, nonconstant data.**

| Held shape | Joint location–scale outcome |
|---|---|
| \(\xi<-1\) | Unbounded by moving the upper endpoint to \(M\). |
| \(\xi=-1\) | Unique reverse exponential at \((\bar x,M-\bar x)\). |
| \(-1<\xi<0\) | Unique finite maximum by the reflected-Weibull gap equation. |
| \(\xi=0\) | Unique Gumbel fit by strict concavity and opposite endpoint scores in inverse scale after profiling location. |
| \(\xi>0\) | Unique finite maximum for \(a(n-k)>k\), \(a=1/\xi\). Equality gives finite unattained collapse, and \(a(n-k)<k\) gives unbounded collapse. |

For positive-shape equality the total supremum is \(n\log(na/k)-n-(a+1)\sum_{x_i>m}\log(x_i-m)\). The Fréchet transfer includes held shapes above one. For constant data with both nuisance coefficients free, shrinking scale at a fixed supported standardized residual gives unbounded likelihood. [Fréchet held-shape proof](2026-11-25-frechet-math-report.md#fréchet-single-holds) · [Negative gap and face proofs](2026-11-08-gev-allfree-data/two-point-proof.md)

### Scale held

**Proved.** A positive held scale gives attainment for every sample by the compact location superlevel and closed likelihood. At fixed negative interior shape, the location problem is strictly concave. At \(-1\) the solution is \(\mu=M-\sigma\), and at zero it is \(\mu=-\sigma\log\overline{e^{-x/\sigma}}\). At positive shape the corresponding Fréchet scale is fixed. The location score depends on the ratio \(S_{-a-1}/S_{-1}\), \(S_j=\sum_i(e+x_i-m)^j\), whose log derivative is \(-(a+1)Q_{-a-1}+Q_{-1}<0\), \(Q_j=S_{j-1}/S_j\). The ratio decreases from infinity to zero, giving one root, including for constant data. With shape free, \(r_S\) disproves global uniqueness. Constant data have a unique joint fit at \(\xi=-1,\mu=x_1-\sigma\), since this is the only way to attain the density bound \(h_\xi=1\) at every observation. Exact scale holds containing the unique all-free two-point winner inherit uniqueness. The two restriction shapes are attained candidates and Gumbel is an attained smooth member with the same hold. [Positive-shape location bounds](2026-11-08-gev-allfree-data/held-scale-region-proof.md#positive-shape-location-bounds-with-gev-scale-held)

### Location held

**Proved.** Put \(c=\#\{x_i=\mu\}\). If some \(x_i<\mu\), the exponential left-residual bound proved above rules out zero scale. If all \(x_i\ge\mu\) and \(2c<n\), the product is \(O(\sigma^{n-2c})\) near zero by the prior density bound. Both cases give compact superlevels and attainment. If all \(x_i\ge\mu\) and \(2c>n\), the \(\xi=1\) collapse is unbounded. At equality the prior held-minimum pair bound gives the total supremum \(-n/2-2\sum_{x_i>\mu}\log(x_i-\mu)\). The sole attained case has all nonminimum observations equal to \(M\), with \((\sigma,\xi)=(M-\mu,-1)\). These cases exhaust held-location existence. Exact location holds containing the unique all-free two-point winner inherit its uniqueness, while \(t_L\) disproves general uniqueness. [Held-minimum bound](../../../univariateML-cpp/include/cppunivariate/distributions/gev/README.md#sharp-density-bound-and-the-held-minimum-half-tie)

For nonconstant data the \(\xi=-1\) face maximizer is \(\sigma=\max(M-\mu,\mu-\bar x)\), from the derivative of its scale objective on the support domain. The fit has support slack when \(\mu-\bar x>M-\mu\). At zero and at \(-1<\xi<0\) the objective is strictly concave in \(q=1/\sigma\). It is \(\log q\) plus a mean of concave terms, respectively \(-q(x_i-\mu)-e^{-q(x_i-\mu)}\) and \((a-1)\log t_i-t_i^a\), \(t_i=1+\xi q(x_i-\mu),\ a=-1/\xi\). It vanishes in likelihood at open support edges, at \(q\downarrow0\), and at \(q\to\infty\) when feasible and some residual is nonzero. If all residuals vanish, likelihood is unbounded.

For fixed positive shape and all \(x_i\ge\mu\), the score in \(\log q\) equals
\[
1+\overline{(t_i-1)(t_i^{-1/\xi}-1-\xi)/(\xi t_i)} .
\]
It decreases strictly when any residual is positive, since \((t-1)/t\) increases and \(t^{-1/\xi}-1-\xi<0\) decreases on \(t\ge1\). Its limit at \(q\to\infty\) is \(\{c-a(n-c)\}/n\), \(a=1/\xi\), giving a unique finite root, finite unattained collapse or unbounded collapse as \(a(n-c)-c\) is positive, zero or negative. The equality limit follows from individual density asymptotics and the score is strictly positive at finite \(q\). When some \(x_i<\mu\), a positive held shape has an attained scale optimum by vanishing likelihood at both ends of the feasible scale interval.

For fixed \(\xi<-1\) and held \(\mu<M\), endpoint contact gives unbounded likelihood. If \(\mu\ge M\), \(t_i=1+|\xi|q(\mu-x_i)\ge1\), and the score is \(1+\overline{(a-1)(t_i-1)/t_i-a(t_i-1)t_i^{a-1}}\), \(0<a=-1/\xi<1\). It decreases strictly from 1 to \(-\infty\) if there is a nonzero residual, because \((t-1)t^{a-1}\) increases. Hence the finite scale is unique. All residuals zero give unbounded likelihood.

## Mathematics-only replays

**Numerical.** All four final Python checks ran green on the workstation, each through /tmp/umvl-cpu/run. The all-free replay uses 401 equally spaced binary64 shapes on \([-1,1]\) at ten data parameters and 70-digit nuisance bisection and public-coordinate likelihood checks. The held replays use 199 shapes on \([-0.98,1]\), 257 scale proposals per shape for held location, and 65-digit refinement. The interval checker uses 40-digit enclosures and a 60-digit proposal. The witness script uses 50-digit intervals and a 70-digit Fréchet replay.

| Replay | Result |
|---|---|
| All-free interior and \(-1\) | Crossing near \(t=0.2949591396535524\), interior shape near \(0.3674826185927520\). Exactly two winners by the interval proof. |
| Coinciding endpoint values | Near \(t=0.2838980914068437\), the \(\xi=\pm1\) values are about \(-0.9655140451\), below \(-0.9552730150\) at \(\xi=1/2\). |
| Held scale | Numerical crossing near \(r=18.40284932643715\), with competing \((\mu,\xi)\approx(0.5336597277319440,1)\). |
| Held location | Numerical crossing near \(t=0.3636338447870948\), with competing \((\sigma,\xi)\approx(0.3987362148896487,0.3726079235022182)\). |
| Fréchet finite–Gumbel | Seam score about \(-0.1729022884\); the profiled point \(\xi=-0.001\) beats both Fréchet values at the numerical tie. |
| Fréchet finite–finite vicinity | At \(x=(0,1,2.650471807225057)\), the GEV face is about \(-1.3602221552\), above the positive shape-one value \(-1.4700550441\). |

The held grids and root residuals give numerical candidate locations. The topological constructions prove at least two global maxima independently of those grids. Reproduce from the worktree root by setting TMPDIR to the worktree's build/tmp and running each script below as /tmp/umvl-cpu/run python3 -u followed by the script path. Set PYTHONDONTWRITEBYTECODE=1 to keep the evidence directory free of caches. [All-free replay](2026-11-25-gev-math-data/replay.py) · [Witnesses](2026-11-25-gev-math-data/witnesses.py) · [Interval checker](2026-11-25-gev-math-data/tie_certificate.py) · [Held replay](2026-11-25-gev-math-data/held_replay.py)

## What this changes for the fitter

- On exact \(x(t_*)\), return an attained certified estimate with known nonuniqueness and a two-point maximizer set. The \(\xi=-1\) member has the lower restriction active. At the rational data endpoints of \(I\), return a unique interior estimate on the left and a unique lower-restricted estimate on the right. On the exact held examples, the mathematically correct outcome is an attained nonunique estimate. Returning a complete set there still requires isolation of all outside competitors. Near a tie, a value gap inside the comparison margin leaves a successful search uncertified. Exact equality needs its own predicate, and a strictly better feasible point refutes the incumbent.
- On \((0,1,1,1)\) return the unique lower-restricted member \((3/4,1/4,-1)\), including under either compatible exact nuisance hold. Holding \(\xi=-1\) gives the same nuisance fit with shape held. On \((0,0,0,1)\) the all-free and held-minimum outcomes are unbounded. On \((0,0,1,2)\) both masks give finite unattained suprema, with different values. On \((0,0,1,1)\) both attain at \(\xi=-1\), with \((\mu,\sigma)=(1/2,1/2)\) all free and \((0,1)\) under the location hold. With \(\xi=1\) held instead, this balanced sample has finite unattained collapse.
- With location held and a strict left residual at distance \(\Delta\), bound the full likelihood by \(q^n(1+\Delta q)^2e^{-1-\Delta q}\) for \(q=1/\sigma\ge1/\Delta\). It decreases for \(q>(n+2)/\Delta\). A bracket against a feasible likelihood yields a compact scale certificate even when the multiplicity bound degenerates. Compute the actual held-coordinate reverse-exponential fit and distinguish contact from slack.
- Preserve the signed face strip, public coefficient inclusion and global comparison of separated root regions. The Pfaffian root count is too large for a solver budget. Grid peak counts supply no completeness predicate.

## Open

- An exact GEV tie with Gumbel or with two strictly interior shapes remains open here. A Gumbel construction needs a sample slice with both zero seam score and equality to a separated candidate, and a closed global comparison on both sides. The earlier Fréchet example fails the seam condition.
- Exact cardinality at \(r_S,t_L\) requires constrained score or value covers isolating the outside winners. The held numerical crossings supply proposals for those covers.
- The sharp negative-side root count remains open. The explicit bound leaves room for a substantially tighter theorem or a construction with two interior negative local maxima.
- For fixed positive shape and held location with a strict left residual, existence is settled but a general root count or uniqueness proof needs control of the combined scale score.
