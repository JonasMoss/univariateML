# Fréchet shape comparisons and folded-normal scale uniqueness

TASK-287 · 2026-11-25 · Source revision eb93bd902377549b5ad2b535063e1a96e2962046

**Proved.** The Fréchet likelihood can have tied global maxima with all coefficients free and with public scale held. In both masks, small rational samples also have two separated finite local maxima, one at the attained shape restriction. A three-level all-free sample family exhibits an exact tie between a unique finite maximum and the Gumbel limit. The all-free half-minimum collapse remains strictly above every finite member and the fitted Gumbel.

**Proved.** With folded-normal physical location held, the variance score has one zero whenever a finite fit exists. This covers every nonnegative held location and includes the analytic half-normal at zero. With both parameters free, the half-normal and a positive-location fit can be tied global maxima.

## Fréchet all free

Let \(m=\min x_i\), \(g_i=x_i-m\), and \(k=\#\{g_i=0\}\). The fitted branch has \(\lambda\ge1\). For \(k<n/2\), use reciprocal shape \(b=1/\lambda\), pivot \(v=b(m-\mu)\), and \(y=\log v\). Write \(z_i=g_i/v\) and \(t_i=\log(1+bz_i)/b\), with \(t_i=z_i\) at zero. A bar denotes the sample mean. The scale-profiled log likelihood per observation is
\[
L(b,y)=-y-(1+b)\overline{t}-\log\overline{\exp(-t)}-1.
\]
The conditional maximizer \(y(b)\) is unique. The finite public coefficients are \(\lambda=1/b\), \(\mu=m-v/b\) and \(\sigma=(v/b)\overline{\exp(-t)}^{-b}\). The GEV map sends these to location \(\mu+\sigma\), scale \(\sigma/\lambda\) and shape \(b\). The prior nuisance and boundary analysis gives the continuous profile \(P(b)=L(b,y(b))\) on \([0,1]\) and excludes other escapes when \(k<n/2\). At \(b=0\), \(P(0)\) is the optimized Gumbel likelihood. Directly, in inverse Gumbel scale \(s=1/v\), \(L(0,-\log s)=\log s-s\bar g-\log\overline{e^{-sg}}-1\) is strictly concave with second derivative \(-1/s^2-\operatorname{Var}_w(g)\), where \(w_i\propto e^{-sg_i}\). Its derivative tends to \(+\infty\) at zero and \(-\bar g<0\) at infinity. [Fréchet dossier](../../../univariateML-cpp/include/cppunivariate/distributions/frechet/README.md) · [GEV nuisance and shape cover](2026-11-08-gev-allfree-data/joint-proof.md) · [Prior uniqueness audit](2026-11-25-uniq-cover-math-report.md)

### Collapse and the Gumbel comparison

**Proved, prior.** If \(2k>n\), the shape-one collapse is unbounded. If \(2k=n\), the supremum is finite and unattained, with total log likelihood
\[
A=n\log 2-n-2\sum_{g_i>0}\log g_i .
\]
The Fréchet pair bound is strict at every finite parameter. For each minimum–nonminimum pair of distance \(d\), the Gumbel pair bound after optimizing location at scale \(v\) has the extra factor \([t/\cosh t]^2<1\), \(t=d/(2v)\). Multiplying over pairs and evaluating at the finite Gumbel optimum gives \(nP(0)<A\). Thus the equality \(2k=n\) selects collapse. For \(2k<n\) the nuisance profile includes the finite and Gumbel competitors without a collapse candidate. [Fréchet dossier](../../../univariateML-cpp/include/cppunivariate/distributions/frechet/README.md#details) · [GEV half-minimum pair bound](../../../univariateML-cpp/include/cppunivariate/distributions/gev/README.md#half-minimum-pair-bound)

**Proved, one-sided per-input tests for \(2k<n\).** A feasible finite log likelihood per observation at least \(P(0)\) proves finite attainment. In particular \(P(1)\ge P(0)\) suffices, including exact equality. The maximum on the compact shape interval either exceeds the Gumbel value at a positive shape or already attains the common value at the finite witness. Conversely, a cover proving \(P(b)<P(0)\) for each \(b>0\) gives a strict Gumbel escape. When both the Gumbel face and a finite point are known global winners, finite attainment holds. The finite point remains the only model member of the two.

A cheaper sufficient test uses the Gumbel fit alone. Evaluate \(z_i\) at \(y(0)\), set \(E_w\) to expectation under weights proportional to \(\exp(-z_i)\), and write \(C[f]=\bar f-E_w f\). The inward derivative is
\[
P'(0)=C[z^2]/2-\bar z.
\]
A positive sign proves finite attainment. At exact zero, the test extends to the second derivative
\[
\begin{aligned}
H_{yy}&=-1-\operatorname{Var}_w(z),\\
H_{yb}&=\bar z-C[z^2]-\tfrac12\operatorname{Cov}_w(z,z^2),\\
H_{bb}&=\overline{z^2}-\tfrac23 C[z^3]-\tfrac14\operatorname{Var}_w(z^2),\\
P''(0)&=H_{bb}-H_{yb}^2/H_{yy}.
\end{aligned}
\]
These follow by differentiating \(L\), using \(t_y=-z,\ t_b=-z^2/2,\ t_{bb}=2z^3/3,\ t_{yb}=z^2\) at zero. The \(y\)-score gives \(C[z]=1\), hence the stated \(H_{yy}<0\) and a smooth maximizing branch by the implicit function theorem. A positive curvature at zero score proves finite attainment. A negative first nonzero derivative only establishes a local Gumbel winner. The three-level example below has a finite global winner on part of its range even with a negative inward derivative. [Prior GEV profile derivatives and remainders](2026-11-08-gev-allfree-data/joint-proof.md)

**Proved, two-point comparison including equality.** Let the minimum fraction be \(p<1/2\), \(q=1-p\), and the gap be \(d>0\). The dossier's change of coordinates \(t=\log((M-\mu)/(m-\mu))/b\) makes the profiled objective strictly convex in \(b\) for fixed \(t\). Only \(b=0\) and \(b=1\) can maximize the closure. Use the explicit shape-one candidate and the unique scalar Gumbel root. At equality the shape-one candidate is the unique finite MLE because all \(0<b<1\) lose strictly and its nuisance maximum is unique. For a cheap strict decision, the value at a trial Gumbel scale is a lower bound, and the tangents at the ends of a signed inverse-scale root bracket provide upper bounds. [Two-point proof and explicit finite coefficients](2026-11-08-gev-allfree-data/two-point-proof.md)

On \(2/5\le p<1/2\) there is a single critical fraction \(p_*\). Define \(D(p)\) as the shape-one minus optimized Gumbel value per observation. The winning model is Gumbel for \(p<p_*\), shape one for \(p>p_*\), and the finite MLE ties the Gumbel face at exact \(p=p_*\). To see the single crossing, let \(t_0,t_1\) maximize the two endpoints in the dossier's \(t\)-chart. The Gumbel score gives \(t_0>1/q\ge5/3\). The explicit shape-one gap in units of \(d\) is \(h=p(1-2p)/(\sqrt{pq}+p)\le 1/10\), so \(t_1=\log(1+1/h)>2\). The derivative in \(t\) of the endpoint-value difference, at fixed \(p\), is \(K(t)-q\), with \(K(t)=(1-e^{-t})^{-1}-1/t\). The inequality \(2\sinh(t/2)>t\) makes \(K\) strictly increasing, and \(K(2)>3/5\). The Gumbel score at \(t_1\) is therefore negative, giving \(t_1>t_0\). If \(J(t)=t-(1-e^{-t})/(p+qe^{-t})\), the envelope derivative is \(D'(p)=t_1+J(t_1)-J(t_0)>0\). Indeed, \(J'(t)=1-e^{-t}/(p+qe^{-t})^2>0\) for \(t>2\log(q/p)\), by factoring \(p+qu^2-u=(u-1)(qu-p)\) with \(u=e^{-t/2}\). Both roots lie above that threshold. The interval checker proves opposite signs at \(2/5\) and \(49/100\), hence existence. Numerically \(p_*\approx0.4061045962090738\). The exact comparison applies to empirical fractions without assuming that this real critical fraction is rational. [Interval checks](2026-11-25-frechet-math-data/ties.py) · [High-precision replay](2026-11-25-frechet-math-data/replay.py)

**Proved, empirical finite–Gumbel equality.** Consider four zeros, five ones and one observation \(r\in[7/5,3/2]\). The interval certificate in the proof below excludes every \(0<b<1\) throughout this data range. Thus the sign of \(D_r=P(1)-P(0)\), obtained with two scalar nuisance solves, decides the comparison on the whole slice. It is negative at \(r=7/5\) and positive at \(r=3/2\). For an exact empirical equality sample, take \(r_*\) to be the least zero of \(D_r\) on the specified closed interval. Continuity and the strict endpoint signs give \(7/5<r_*<3/2\). The finite shape-one MLE is unique at \(r_*\), with the same likelihood as the optimized Gumbel. The numerical zero near \(1.461614073283693\) has \(D_r\) approximately \(-6.17\times10^{-10}\) and \(+6.17\times10^{-10}\) at offsets \(-10^{-8}\) and \(+10^{-8}\). The proof needs no count of the zeros of \(D_r\). [Interval checks](2026-11-25-frechet-math-data/ties.py) · [High-precision replay](2026-11-25-frechet-math-data/replay.py)

### Finite nonuniqueness

**Proved, rational sample with two local maxima.** For \(x=(0,1,13/5)\), the inward shape profile derivatives at \(b=1/50,\ 3/20,\ 1\) are respectively positive, negative and positive. These are strict interval comparisons. The first two signs put a maximum inside \((1/50,3/20)\), and the last gives a separate one-sided local maximum at \(b=1\). The nuisance root is unique and analytic, and the profile is analytic away from the endpoints with a nonzero derivative at the bracket ends, so an interior local maximum can be taken strict. Numerically the interior maximum lies at \(b\approx0.05330258993\), with log likelihood per observation \(-1.45396885\) versus \(-1.45923237\) at \(b=1\). [Signed brackets](2026-11-25-frechet-math-data/replay.py)

**Proved, exact global tie.** For \(x=(0,1,r)\), split the closed shape interval at \(b=13/20\). Define \(r_F\) to be the least \(r\in[13/5,27/10]\) at which the maxima on \([0,13/20]\) and \([13/20,1]\) are equal. The interval proof below shows that the left interval wins at \(13/5\), the right interval wins at \(27/10\), the separator loses throughout, and \(P'(0)>0\) throughout. Compactness and continuity make the two maxima continuous in \(r\). The specified \(r_F\) exists, and both interval winners there are finite with different shapes. The numerical crossing of the observed interior and shape-one branches is near \(r=2.650471807225057\), with the interior branch at \(b\approx0.1243608286348986\). These numbers locate the local branches and are separate from the exact definition via restricted global maxima. [Interval checks](2026-11-25-frechet-math-data/ties.py) · [High-precision replay](2026-11-25-frechet-math-data/replay.py)

## Fréchet single holds

**Proved, shape held, prior.** For any held \(\lambda>0\), let \(e=m-\mu>0\), \(S_j(e)=\sum_i(g_i+e)^{-j}\). After profiling \(\sigma=(n/S_\lambda)^{1/\lambda}\), the gap score has the sign of \(n\lambda S_{\lambda+1}/(S_\lambda S_1)-(\lambda+1)\). For nonconstant data the ratio decreases strictly from \(1/k\) to \(1/n\). Thus \(\lambda(n-k)>k\) gives a unique finite joint location–scale maximum. If \(\lambda(n-k)<k\), collapse is unbounded, including constant data. For exact equality the profile is strictly decreasing and the unattained total supremum is
\[
n\log(n\lambda/k)-n-(\lambda+1)\sum_{g_i>0}\log g_i .
\]
This covers held shapes below one as well. Holding shape and scale gives the separate unique scalar location root. Holding location gives the settled inverse-Weibull cases on strictly positive excesses, with every positive shape admissible. [Fréchet dossier](../../../univariateML-cpp/include/cppunivariate/distributions/frechet/README.md#details) · [Transfer report](2026-11-25-transfer-math-report.md)

**Proved, scale held, existence and faces.** Normalize the data by subtracting their minimum and dividing by the held scale, and measure the location gap \(e\) in those units. The prior result puts the unique conditional location root for every \(\lambda\ge1\) in \([(k/(2n))^{1/\lambda},1)\). For nonconstant data its range-dependent tail bound tends to \(-\infty\) as \(\lambda\to\infty\). For bounded shape the log likelihood also tends to \(-\infty\) as \(e\to0\) or \(e\to\infty\). Compact containment gives a finite maximum. Shape one is an attained member. At infinite shape the corresponding GEV scale is \(\sigma/\lambda\to0\), so the proper free-scale Gumbel face is unavailable. For constant data \(e=1\) gives the unbounded log density \(\log\lambda-1\) in the normalized units. An exact scale hold through a unique all-free finite winner inherits its certificate. [Held-scale region and tail proof](2026-11-10-gev-held-data/proof.md#fréchet-with-public-scale-held) · [Fréchet dossier](../../../univariateML-cpp/include/cppunivariate/distributions/frechet/README.md)

**Proved, scale held, nonuniqueness.** Hold \(\sigma=1\) and take three zeros, four copies of \(1/20\) and one observation \(r\). At the exact rational value \(r=40\), interval comparisons show profile derivatives of signs \(-,+,-\) at \(\lambda=1,\ 27/20,\ 7/4\). They give separated finite local maxima at the attained boundary and inside \((27/20,7/4)\). Numerically the interior maximum at \(r=40\) has \(\lambda\approx1.54326513\) and per-observation value \(-1.46839276\), compared with \(-1.46741329\) at shape one. At \(r=39\) the order reverses, with values \(-1.46044967\) and \(-1.46123699\).

For an exact global tie, let \(r_S\) be the least \(r\in[39,40]\) at which the maxima on \([1,6/5]\) and \([6/5,10]\) coincide. The interval proof shows that the right interval wins at 39, the left wins at 40, and the separator loses for every \(r\) in between. The tail above 10 loses uniformly. Continuity gives \(r_S\in(39,40)\) with distinct global maximizing shapes on either side of \(6/5\). The numerical crossing of the observed local branches is near \(r=39.43569793551154\), with interior \(\lambda\approx1.56130433019349\). [Interval checks](2026-11-25-frechet-math-data/ties.py) · [High-precision replay](2026-11-25-frechet-math-data/replay.py)

## Folded normal

### Held physical location, free scale

**Proved.** Let \(\mu\ge0\) be fixed, \(t=\sigma^2\), \(D=\overline{(x-\mu)^2}\), and \(m_2=\overline{x^2}\). For any nonempty nonnegative sample,
\[
\frac{1}{n}\frac{d\ell}{dt}=-\frac{T(t)}{2t^2},\qquad
T(t)=t-D-4\overline{\frac{\mu x}{1+\exp(2\mu x/t)}}.
\]
This differentiates the ordinary normal log likelihood plus the reflected log-density correction \(\log(1+\exp(-2\mu x/t))\). For \(z=2\mu x/t\), the derivative of a term \(4\mu x/(1+\exp z)\) in \(t\) is \(z^2/(1+\cosh z)\). For \(z>0\),
\[
\frac{1+\cosh z}{z^2}\ge\frac12+\frac{2}{z^2}+\frac{z^2}{24}
\ge\frac12+\frac1{\sqrt3}>1.
\]
At \(z=0\) the term derivative is zero. In particular \(T'(t)\ge\gamma=1-(1/2+1/\sqrt3)^{-1}>0\) globally. For \(D>0\) the endpoint signs \(T(D)\le0\le T(m_2+\mu^2)\) follow by bounding the logistic term between zero and \(2\mu x\). The root in this interval is the unique global maximum. Its ordinary variance curvature is strictly negative, and \(|T(t)|/\gamma\) bounds absolute variance error when the residual is known exactly.

If \(D=0\), every observation equals the held \(\mu\). The likelihood is unbounded as \(\sigma\to0\), by the direct normal component at its mean. If \(\mu=0\) and \(m_2>0\), the unique fit is the exact half-normal member \(\hat\sigma=\sqrt{m_2}\), with folding point zero. If \(\mu>0\), the half-normal is outside that held submodel. The proof works in physical variance throughout and accounts for the varying ratio \(\mu/\sigma\). [Folded-normal dossier](../../../univariateML-cpp/include/cppunivariate/distributions/foldnorm/README.md) · [Prior transfer question](2026-11-25-transfer-math-report.md#open)

### Other masks and the half-normal face

**Proved, scale held, prior.** The normalized positive-location score uses \(\overline{x\tanh(x\mu/t)}/\mu-1\), strictly decreasing for \(\mu>0\) if any observation is positive. The unique maximizer is exactly zero for \(m_2\le t\), including equality and the all-zero sample. Otherwise the maximizer is the unique positive root. Both holds leave likelihood evaluation. [Folded-normal dossier](../../../univariateML-cpp/include/cppunivariate/distributions/foldnorm/README.md)

**Proved, both free.** Constant samples give unbounded collapse. On nonconstant data the prior result proves attainment and gives the profile in \(a=\mu/\sigma^2\), with \(\sigma^2=2m_2/(1+\sqrt{1+4m_2a^2})\). The prior report proves that \(m_4<3m_2^2\) excludes the half-normal by a positive inward quartic term. The reverse strict inequality only makes it a local maximum, and equality requires higher terms. That report also documents a sample with two separated interior maxima. [Prior folded-normal analysis](2026-10-29-foldnorm-report.md)

An exact global tie with the half-normal is obtained from \(99\) copies of \(\epsilon\) and one \(1\). Define \(\epsilon_*\) as the smallest \(\epsilon\in[0,1/5]\) for which a positive \(a\) has profile value at least that of \(a=0\). At zero the previous \(\{0,c\}\) theorem gives a strict half-normal winner because the positive fraction is \(1/100\). At \(1/5\), rescale the prior example with 99 ones and one five. Its ordinary normal likelihood at its own MLE already exceeds the fitted half-normal since \(1.24>4(0.1584)\). The folded likelihood at those same parameters is at least the ordinary normal likelihood.

For \(p=99/100,\ q=1/100,\ w=\epsilon^2\), the fourth-to-squared-second-moment ratio is \((pw^2+q)/(pw+q)^2\). Its derivative in \(w\) is \(2pq(w-1)/(pw+q)^3<0\). Thus the ratio is uniformly greater than three on the entire interval, by its value at \(1/5\). The prior quartic expansion and a uniform Taylor remainder give some \(\delta>0\) with strict loss on \(0<a<\delta\) for every \(\epsilon\). Uniformity follows from the analytic profile formula on this compact data interval, with \(m_2\ge1/100\) and a quartic coefficient bounded away from zero. The prior derivative bound \(a>\bar x/\operatorname{Var}(x)\) excludes global maxima there, with \(\bar x/\operatorname{Var}(x)<40\) for every \(\epsilon\) here. The best value on \([\delta,40]\) minus the half-normal value is continuous, strictly negative at zero, and positive at \(1/5\). Its first nonnegative point belongs to \((0,1/5)\) and has value difference zero. At that exact \(\epsilon_*\), \(a=0\) and a positive \(a\) are global maxima. Numerically, in the rescaled family of 99 ones and one \(c=1/\epsilon\), the observed interior branch ties the half-normal near \(c=7.198510780435875\), \(a=2.609340882600587\) in those units. The two local branch values change order at offsets \(\pm10^{-8}\), with differences of magnitude \(1.05\times10^{-9}\) per observation. This numerical calculation compares the two specified branches. [Two-level half-normal result](../../../univariateML-cpp/include/cppunivariate/distributions/foldnorm/README.md#r-notes) · [Prior folded-normal analysis](2026-10-29-foldnorm-report.md) · [High-precision replay](2026-11-25-frechet-math-data/replay.py)

## Proof of the Fréchet interval comparisons

**Proved conditional on the interval arithmetic comparisons.** For either shape profile, at every datum and shape in a rectangle, bound the nuisance maximizer by testing two fixed nuisance values. Require a strictly positive nuisance score at the lower value and strictly negative at the upper value, using intervals throughout. The known unique nuisance maximum lies inside the bracket. Then evaluating likelihood on the rectangle times that nuisance interval bounds the optimized value. Evaluating a single feasible nuisance value bounds it below. On a slab with a strict profile-score sign, the maximizing shape is the appropriate slab endpoint. This follows from the envelope derivative because the nuisance score vanishes at the root.

The all-free nuisance score is \(-1+(1+b)\bar R-E_wR\), \(R_i=z_i/(1+bz_i)\), and the shape score is \(-\bar t-(1+b)\overline{t_b}+E_wt_b\), where \(w_i\propto e^{-t_i}\). To handle intervals through the seam, bound \(t_i=\int_0^{z_i}(1+bs)^{-1}ds\) and \(t_b=-\int_0^{z_i}s/(1+bs)^2ds\) by their opposite monotone corners, with their analytic values at \(b=0\). In the held-scale case use \(u=\lambda\log e\), \(d_i=g_i+\exp(u/\lambda)\) in normalized units, and \(H_i=d_i^{-\lambda}\). The likelihood per observation is \(\log\lambda-\overline{(1+\lambda)\log d+H}\), with nuisance score \(S_u=\overline{(e/d)(H-1-1/\lambda)}\) and shape score at fixed \(u\) equal to \(1/\lambda+\overline{(H-1)\log d}-uS_u/\lambda\). These formulas supply all rectangle comparisons. Binary64 bisection independently proposes nuisance centers. Those centers never determine the sign or a likelihood comparison.

| Check | Verified inequalities | Cells visited |
|---|---|---:|
| All free at \(r=13/5,27/10\) | Upper bounds exclude the right split interval at the first endpoint and the left at the second, against witnesses at \(b=0.053\) and \(b=1\) respectively. | 713, 3101 |
| All-free separator | \(P(13/20)<P(1)\) for every \(r\in[13/5,27/10]\). A separate check gives \(P'(0)>0\) throughout. | 123 for the separator |
| Held scale at \(r=39,40\) | Upper bounds exclude the left split interval at 39 and the right at 40, against witnesses at \(\lambda=1.575\) and \(\lambda=1\) respectively. | 357, 631 |
| Held-scale separator | \(P(6/5)<P(1)\) in the held-scale shape chart for every \(r\in[39,40]\). | 3 |
| Finite–Gumbel slice | Strict decrease on \([0,1/10]\), strict increase on \([9/10,1]\), and strict loss to an endpoint on \([1/10,9/10]\), uniformly for \(r\in[7/5,3/2]\). | 32139 |

The partitions use exact rational bisection. For endpoint exclusions split the tested shape interval until either its value upper loses to the witness or its score sign reduces the upper comparison to an endpoint. For separator and seam inequalities subdivide only \(r\). For the finite–Gumbel slice bisect whichever of the shape and data intervals is wider until the required strict sign or value comparison holds. For the held-scale tail, the normalized density is bounded above by \(\lambda+1\) at every observation. The observation \(r\ge39\) also has log density at most \(\log\lambda-(\lambda+1)\log39\) for all \(e>0\). Hence the per-observation upper bound \(\log(\lambda+1)-(\lambda+1)\log39/8\) strictly decreases beyond 10 and at 10 is strictly below a uniform feasible shape-one witness. These finite partitions and the tail account for the entire respective maximizations. The executable checker verifies the listed inequalities with 40-decimal-digit interval operations and asserts that each required subdivision closes. [Interval certificate](2026-11-25-frechet-math-data/ties.py)

## Numerical replay

**Numerical.** The workstation runs used one slot per script through \(\texttt{/tmp/umvl-cpu/run}\). The exploratory all-free grids use 101 or 1001 linear points on \([0,1]\), and the held-scale grids use 151 or 1001 logarithmic points on their stated shape ranges. The replay uses 1001 shape points on \([0,1]\) or \([1,10^5]\) in binary64, then reevaluates candidate roots and likelihoods with 65-decimal-digit mpmath. The rational local counterexamples also have direct 50-digit interval score checks at the listed brackets. Grid peaks provide proposals and do not certify uniqueness. The golden data use the stored binary64 observations. [Exploratory search](2026-11-25-frechet-math-data/search.py) · [Sample probes](2026-11-25-frechet-math-data/probes.py) · [High-precision replay](2026-11-25-frechet-math-data/replay.py)

| Sample or hold | High-precision replay |
|---|---|
| Fréchet golden, 500 observations, all free | \((\lambda,\mu,\sigma)\approx(1.780221601423467,0.06127683692505054,0.9001111496659627)\), total log likelihood \(-608.4546481271125\) |
| Fréchet golden, \(\sigma=1\) | Profile peak at \(\lambda\approx1.931702047090724\), per-observation value \(-1.218120071643648\) |
| Fréchet golden, \(\lambda=2\) | Gap \(0.4354835963740461\), scale \(1.005292170091686\), total log likelihood \(-609.3455030879203\) |
| Folded-normal golden, 500 observations, both free | \((\mu,\sigma)\approx(2.024853252831466,1.027217293842854)\), total log likelihood \(-688.1952105287487\) |
| Folded-normal golden, \(\mu=2\) or \(\sigma=1\) | \(\hat\sigma\approx1.02895164050239\) with location held, \(\hat\mu\approx2.027200473150468\) with scale held |

Both joint golden coefficient vectors agree with the stored high-precision references within \(10^{-21}\) in absolute error. The likelihood check uses \(10^{-20}\). Other transition checks:

- For \(x=(0,0,1,1)\) at held shapes \(1-10^{-6},1,1+10^{-6}\), the first two have exponents \(-2\times10^{-6},0\). The last has a finite maximizing gap about \(2.5000184\times10^{-7}\) and scale \(5.0000320\times10^{-7}\).
- On \((0,1,r)\) the Gumbel inward score vanishes numerically near \(r=2.563675532606423\), with profiled curvature about \(-0.14839548\). At offsets \(-10^{-8},+10^{-8}\), the scores are approximately \(-2.25\times10^{-9},+2.25\times10^{-9}\).
- With public scale one, \((0,0.1)\) and \((0,10^{-4})\) have numerical peaks at \(\lambda\approx24.5892\) and \(23994.1662\). At exact zero range the shape divergence is proved.
- On the constant folded-normal sample \(x=1\), the fixed locations \(0,10^{-12},0.99,1,1.01\) give variances near \(1,1,10^{-4}\), divergence, and \(10^{-4}\), respectively. For \(x=(0,1,1,1)\) with variance held just below \(3/4\) by \(10^{-10}\) the positive location is about \(1.5\times10^{-5}\). At equality and \(10^{-10}\) above, the location is exactly zero.

The final replay and interval certificate passed on the workstation. The search and probe scripts also exited successfully. To reproduce from the data directory, run each script in the foreground:

```sh
for script in search.py probes.py replay.py ties.py; do
    /tmp/umvl-cpu/run python3 -B "$script" || exit
done
```

## What this changes for the fitter

- **Fréchet all free.** Dispatch the exact minimum-multiplicity cases before fitting. Above half, report unbounded collapse. At half, report the finite unattained supremum with its log likelihood. Below half, a verified finite witness at least as good as the Gumbel fit proves attainment, and a strictly positive inward score is another cheap check. Certify a singleton only after excluding all other finite maximizers. Emit `GumbelLimit` only when every finite candidate loses. At a proved finite–Gumbel equality, retain the finite estimate and the tied face witness in diagnostics. Indicate a shape estimate at \(\lambda=1\) as restricted.
- **Fréchet held shape and held scale.** Keep the monotone location–scale certificate and exact collapse exponent for held shape. For held scale, keep the constant-data unbounded-shape stop, the nonconstant infinite-shape exclusion and the attained shape-one edge. Retain the global shape cover in both free-location, free-shape masks. Exact finite multiplicity requires a typed set of all maximizing members and an estimate with known nonuniqueness. The examples prove multiplicity on their exact real samples. A floating-point near tie needs a comparison margin and a distinct cardinality decision.
- **Folded normal held location.** Replace the global likelihood cover with a signed variance root on \([D,m_2+\mu^2]\) using stable logistic evaluation and the exact constant-equals-held-location check. Map the final variance bracket through the square root to certify public scale. The lower slope bound also permits residual-based enclosure. Use the analytic half-normal at held \(\mu=0\). For held scale keep the exact \(m_2\le \sigma^2\) rule. For both free, preserve half-normal and interior comparisons, including finite tie handling.
- **Inconclusive comparisons.** Keep a successful locally accurate search with its uncertified flag if the support, tail or candidate comparison cannot close within the budget or arithmetic margin. A proved contradiction, proved nonexistence or failed search keeps its existing stop semantics. A small stationary score alone gives no global or singleton certificate.

## Open

- For general Fréchet data with \(2k<n\), the inward score and curvature give only local information when their first nonzero term is negative. A cheap condition certifying a Gumbel win across an unrestricted data slice would need a global exclusion inequality beyond the two- and three-level comparisons. The existing slab cover supplies a per-input route.
- The exact finite tie constructions prove at least two separated maximizers. They leave the full cardinality and the number of crossings along the corresponding sample paths open. The numerical branch locations are useful for near-tie replay. Exact arithmetic or certified algebraic predicates would be needed to recognize an equality on an input representation, and converting the exact real samples to binary64 need not preserve it.
- For the joint folded normal, an inexpensive general half-normal winner test would need a uniform upper bound on all positive-\(a\) competitors. The strict quartic sign alone only decides the local face. The held-location variance theorem has no such comparison left.
