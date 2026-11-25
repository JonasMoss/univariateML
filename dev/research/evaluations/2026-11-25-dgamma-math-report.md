# Discretised gamma shape profiles

25 November 2026 · TASK-243 · base `eaea17a1a8072dadc1b65d65236e534ff7e0790d`

**Proved.** The rate-profiled log likelihood can fail concavity in shape, even for the exact sample `(1, 3)`. It is strictly unimodal for the positive-count samples satisfying the explicit inequality below. For a positive constant sample or two adjacent occupied cells, the profile is strictly increasing to its unattained supremum. Holding the rate gives a strictly concave shape likelihood and, whenever any count is positive, a unique finite maximum.

**Open.** Unimodality and quasi-concavity for general samples of range at least two. The numerical search found no resolved counterexample. The failure of concavity for `(1, 3)` does not establish a failure of quasi-concavity.

## Model and rate profile

The fitter uses \(p_k(a,r)=\Pr(k\le Y<k+1)\) with \(Y\sim\mathrm{Gamma}(a,r)\), shape \(a>0\) and rate \(r>0\). Let \(w_k\) be the empirical fractions, \(\bar k=\sum_k w_k k\), \(L=\sum_k w_k\log p_k\) the mean log likelihood, and \(P(a)=L(a,r(a))\) its rate profile. Write \(E_k\) and \(\operatorname{Cov}_k\) for conditional moments in cell \(k\). The inner uniqueness and the existence and face results used here are from [the dossier](../../../univariateML-cpp/include/cppunivariate/distributions/dgamma/README.md), [the P0 existence analysis](2026-10-29-p0-discrete2.md#dgamma) and [the TASK-117 reduction](2026-10-31-open-existence-report.md#6-discretized-gamma-profiled-shape). The [fitter](../../../univariateML-cpp/include/cppunivariate/distributions/dgamma/mle.hpp) adds subdivision to the grid and refinement discussed in TASK-117.

**Proved.** For \(\bar k>0\), \(r(a)\) is smooth and strictly increasing. To see this, put \(u=\log r\), \(T=\log Y\), and let \(f\) be the density of \(T\). Its log derivative in \(t\) is \(h(t)=a-r e^t\). The cell log-rate score is \(E_k h\), an average on a fixed interval \([l,b]\) in \(t\). Equivalently, \(p_k\) is the mass of the shifted interval \([l+u,b+u]\) under the unit-rate log-gamma density. Differentiating the average by shifting its endpoints gives
$$
\partial_u E_k h=\frac{[(h-E_kh)f]_l^b}{p_k}<0
$$
because \(h\) is strictly decreasing. Differentiating in shape at fixed rate and integrating by parts in \(t\) gives
$$
\partial_a E_k h
=1-\operatorname{Cov}_k(T,rY)
=\frac{[(t-E_kT)f(t)]_l^b}{p_k}>0.
$$
Both lower-end terms vanish when \(l=-\infty\). Thus the summed score \(G=L_u\) has \(G_u<0<G_a\). Inner existence gives the root, and the implicit derivative of \(u(a)\) is \(-G_a/G_u>0\). Analyticity locally follows by integration on each cell, with an exponentially decaying bound at \(t=-\infty\) for shape in a compact positive neighborhood. The analytic implicit function theorem applies to the root. The rate score also gives the dossier's bound
$$
m(a):=a/r(a)=\sum_k w_k E_kY,\qquad \bar k<m(a)<\bar k+1.
$$

### Held rate

**Proved.** In a fixed-rate fit every cell log probability has strictly negative shape curvature. In fact its second shape derivative is \(\operatorname{Var}_k(T)-\psi'(a)\) by differentiation of the conditional log integral. To compare variances, let \(F\) be the CDF of \(T\), \(Q=F^{-1}\), and \(I(v)=f(Q(v))\). Then \(I'(v)=(\log f)'(Q(v))\) and \(I''(v)=(\log f)''(Q(v))/f(Q(v))<0\). The continuous extension has \(I(0)=I(1)=0\). For a cell of mass \(p\in(0,1)\) starting at CDF level \(\alpha\), the conditional quantile is \(Q_I(v)=Q(\alpha+pv)\). Concavity with \(\alpha/(1-p)\in[0,1]\) gives \(I(\alpha+pv)>p I(v)\) for \(0<v<1\). If \(\alpha/(1-p)=v\), strictness follows from \(I(v)>0\) and \(p<1\). Consequently \(Q_I'(v)<Q'(v)\), so every quantile spacing strictly contracts. The variance is half the mean squared spacing of two independent uniform quantiles. This proves \(\operatorname{Var}_k(T)<\operatorname{Var}(T)=\psi'(a)\), including the zero cell. The known existence result now gives uniqueness for any sample with a positive count.

Separate concavity in shape and log rate cannot give joint concavity in those coordinates. At fixed \(a>0\),
$$
\log p_k(a,r)=a u-\log\Gamma(a)+\log\int_k^{k+1} y^{a-1}e^{-ry}\,dy.
$$
As \(u\to-\infty\) the Hessian has \(L_{uu}=O(r)\), \(L_{au}=1+O(r)\) and bounded \(L_{aa}\). The integral and its derivatives are bounded locally in positive shape even for the zero cell. The Hessian determinant tends to \(-1\).

## Shape score and a sufficient sample condition

**Proved.** Put \(d(x)=x-1-\log x\) and \(\Delta(a)=\log a-\psi(a)\). The envelope derivative and the rate equation give
$$
P'(a)=\Delta(a)-\sum_k w_k E_k d(Y/m(a)).
$$
For any occupied \(l,h\) with \(h>l+1\), set \(q=\sqrt{h/(l+1)}\) and \(d_*=\min(w_l,w_h)\min\{d(q),d(1/q)\}>0\). When \(m\le\sqrt{h(l+1)}\), \(Y/m\ge q\) throughout cell \(h\). Otherwise \(Y/m\le1/q\) throughout cell \(l\). In either case the conditional deviance average is at least \(d_*\). One pair suffices. Taking the largest bound over pairs improves it.

The digamma series expresses \(\Delta(a)\) as the limit of the left Riemann sum for \(1/(a+x)\) over \([0,N]\) minus its integral, using unit mesh. Comparison to the right Riemann sum gives \(0<\Delta(a)<1/a\). The trigamma series and strict trapezoidal overestimation of \(\int_0^\infty(a+x)^{-2}dx=1/a\) give \(\psi'(a)-1/a>1/(2a^2)\). In particular \(P'(a)<0\) for \(a\ge A:=1/d_*\). [NIST DLMF digamma series](https://dlmf.nist.gov/5.7.E6), [trigamma series](https://dlmf.nist.gov/5.15.E1).

At small shape \(P'(a)>0\) for any \(\bar k>0\). For the zero cell, the variable \(t=-\log Y\) has conditional kernel \(e^{-at-re^{-t}}\) on \((0,\infty)\). Its mean is at most \(e^r/a\), by bounding the numerator by \(1/a^2\) and the normalizer below by \(e^{-r}/a\). Positive cells contribute \(E_k\log Y\ge0\). The rate bracket and digamma recurrence therefore give
$$
P'(a)\ge \log a-\log(\bar k+1)+\frac{1-w_0e^{a/\bar k}}a-\psi(1+a).
$$
The right side tends to \(+\infty\). A usable lower cutoff \(B\le e^{-1}\) is any one for which
$$
B\log B+1-w_0 e^{B/\bar k}
-B\{\log(\bar k+1)+\max(0,\psi(1+B))\}>0.
$$
Indeed this bounds \(aP'(a)\) from below for \(0<a\le B\). Such a \(B\) always exists since \(w_0<1\). The two strict score tails and analyticity imply that there are finitely many stationary points for each sample of range at least two.

### Positive-count samples

**Proved sufficient condition.** Suppose all occupied cells have \(k\ge1\), the sample range is at least two, and
$$
\frac{A^2}{2}\sum_k w_k B_k^2+\frac{A}{4\bar k^2}<1,
\qquad
 B_k=\max\left\{
 \left|\frac1k-\frac1{\bar k+1}\right|,
 \left|\frac1{k+1}-\frac1{\bar k}\right|
 \right\}.
$$
Then the score has exactly one zero, which is simple, and \(P\) is strictly increasing before it and strictly decreasing after it. For example, `(10, 100)` passes with the left side below \(0.06\).

**Proof.** In natural coordinates \((a,-r)\) the Hessian of \(L\) is the weighted within-cell covariance of \((\log Y,Y)\) minus its unconditional covariance. This follows by differentiating the log of the cell integral and that of the full integral, using the same two sufficient statistics in each. At a profiled point use the equivalent statistics \((\log Y-Y/m,Y/m)\), holding the basis transformation fixed at that point. Their full covariance is diagonal with entries \(\psi'(a)-1/a\) and \(1/a\). This follows from \(\operatorname{Cov}(\log Y,Y)=1/r\), \(\operatorname{Var}(Y)=a/r^2\) and \(m=a/r\). On each unit cell the first statistic has range at most \(B_k\) by the derivative bound \( |1/y-1/m|\le B_k\). The other has range \(1/m\le1/\bar k\). A variable of range \(b\) has variance at most \(b^2/4\), by bounding squared deviations from the range midpoint. Standardize the weighted within-cell covariance by the full covariance. Its largest eigenvalue is at most its trace, and for \(a\le A\) the trace is bounded by the left side of the condition. Thus the Hessian is negative definite at every profiled point with \(a\le A\). The profile second derivative is the Schur complement \(L_{aa}-L_{ar}^2/L_{rr}\), strictly negative there. The two score tails establish existence and uniqueness of the root and its signs on the two sides.

## Shape concavity fails

**Proved counterexample to concavity in shape.** Use the exact sample `(1, 3)`. Here \(2<m(a)<3\). For \(m\in[2,3]\) put
$$
J(m)=\tfrac12\{d(2/m)+d(3/m)\},\qquad m_0=5/2.
$$
The gamma Chernoff bounds at the closest endpoints of the two occupied cells give \(L(a,a/m)\le-aJ(m)\). The derivative \(J'(m)=(m-m_0)/m^2\) gives a unique minimum at \(m_0\).

On any compact neighborhood of \(m_0\) in \((2,3)\), write the density as \(a^ae^{-a}y^{-1}e^{-a d(y/m)}/\Gamma(a)\). On each occupied cell the minimum of \(d(y/m)\) is at the closest endpoint \(y_*=2\) or \(3\). The magnitude of its derivative is bounded away from zero there and over the whole cell, uniformly in this neighborhood. Substitute \(s=a|y-y_*|\) in the cell integral. After removing \(e^{-a d(y_*/m)}\), the rescaled integrand tends to \(y_*^{-1}e^{-|1/m-1/y_*|s}\), uniformly for bounded \(s\), and is dominated by a constant times \(e^{-\delta s}\) for some \(\delta>0\). Integrating and applying Stirling's formula gives
$$
L(a,a/m)=-aJ(m)-\tfrac12\log a+C(m)+o(1)
$$
uniformly there, with \(C(m)\) continuous. The value at \(m_0\), compared to the Chernoff upper bound, forces \(m(a)\to m_0\) by the strict minimum of \(J\). Squeezing the profile between the value at \(m_0\) and the expansion at \(m(a)\) with \(J(m(a))\ge J(m_0)\) gives the same expansion for \(P(a)\) with \(m_0\). Thus
$$
\frac{P(a)+P(3a)}2-P(2a)\longrightarrow\tfrac12\log(2/\sqrt3)>0.
$$
This argument concerns the untransformed shape. The numerical replay below also finds a positive midpoint deficit at shapes \(100,200,300\). [NIST DLMF Stirling expansion](https://dlmf.nist.gov/5.11.E1).

## Single crossings and samples of range at most one

**Proved.** At fixed rate the density ratio for increasing shape is increasing in \(y\), so the ratio of cell probabilities is increasing in the cell index. At fixed shape the ratio for increasing rate is decreasing in both. This gives the two-parameter fixed-coordinate order-two total-positivity inequalities, reversing the rate ordering. Along the joint rate profile both parameters increase. The underlying density ratio is then proportional to \(y^{a_2-a_1}e^{-(r_2-r_1)y}\), with a single peak. For a fixed threshold \(c>0\), its difference from \(c\) has at most two sign changes in \(y\), hence so do the nonzero signed integrals \(p_k(a_2,r_2)-c\,p_k(a_1,r_1)\) over ordered cells. Each change of sign of the integrals requires a change of sign between points in the corresponding cells. This counts signs across cells for two parameter pairs. The profile score instead varies shape with empirical weights held fixed, so that count does not bound its sign changes.

For adjacent occupied cells \(j,j+1\), start with any \(a_1,r_1\) and increase shape to \(a_2\). At the original rate the lower-to-upper cell odds strictly decrease, by the monotone density ratio. At the new shape the odds strictly increase with rate and tend to infinity as rate increases. The divergence follows by comparing the integral over an interior subinterval of the lower cell with the integral over the upper cell, using their positive separation. There is therefore \(r_2>r_1\) matching the original odds. The two occupied cell probabilities are both multiplied by the same factor \(c\). The density ratio has one peak and its conditional mean is \(c\) in each cell. Its superlevel interval above \(c\) must meet both cells, and each cell also contains values below \(c\). Therefore the density ratio is below \(c\) outside their union, whose complement has positive mass. Integration over the full support gives \(1<c\). Taking \(r_1=r(a_1)\) proves strict increase of the profile.

For a positive constant \(k\), the endpoint formula for the rate score gives \(r(a)=a\log((k+1)/k)\). At this rate the density has kernel \(e^{a g(y)}dy/y\) where \(g(y)=\log y-y\log((k+1)/k)\). The occupied cell is the upper level set of \(g\) with threshold \(g(k)=g(k+1)\). The derivative of its log probability is \(E_k g-E g>0\), because the conditional means on the cell and its complement are strictly ordered. For all zeros the supremum over rate is zero in log likelihood at every shape and there is no attained inner maximum.

## Maximizing sequences

**Proved.** The dossier already gives compact positive likelihood levels for free range at least two and the held-parameter existence regimes. Every maximizing sequence in those regimes is eventually in a compact interior set, and its distance to the maximizing set tends to zero by continuity. The following necessary and sufficient descriptions sharpen the existing unattained-face constructions.

- **Positive constant \(j\), both free.** Any maximizing sequence has \(a\to\infty\) by the dossier's boundary exclusion for a sample containing a positive count. Define \(z_b=(rb-a)/\sqrt a\) at integer \(b\). The necessary and sufficient remaining conditions are \(z_j\to-\infty\) and \(z_{j+1}\to+\infty\). In particular \(r\to\infty\).
- **Two adjacent occupied cells \(j,j+1\), both free.** Put \(b=j+1\), and let \(w_j\in(0,1)\) be the observed lower fraction. The necessary and sufficient conditions are \(a\to\infty\) and \(z_b\to\Phi^{-1}(w_j)\). Consequently \(r\sim a/b\). To see necessity, the two probabilities must converge to the unique maximizer of the saturated log likelihood over their probability simplex, namely the two empirical fractions. It follows that the CDF at \(b\) tends to \(w_j\). Conversely, from the condition on \(z_b\), \(r/\sqrt a\to\infty\), so the two exterior endpoints bracket the two proportions in the limit.
- **All zeros, both free.** On a subsequence with \(a\to a_*\in(0,\infty)\), maximizing is equivalent to \(r\to\infty\). On one with \(a\to\infty\), it is equivalent to \(z_1\to+\infty\). On one with \(a\to0\), it is equivalent to \(a(-\log r)_+\to0\). These criteria apply on every extended-shape convergent subsequence, permitting paths that alternate between faces.
- **All zeros, one coefficient held.** With shape held, maximizing is equivalent to \(r\to\infty\). With rate held it is equivalent to \(a\to0\). These are the special cases of the all-zero criteria under the holds.

For the large-shape assertions, the standardized variable \((rY-a)/\sqrt a\) tends to standard normal, as follows from its moment generating function \(e^{-t\sqrt a}(1-t/\sqrt a)^{-a}\). Its CDFs converge uniformly since the limiting CDF is continuous. This justifies necessity and sufficiency even with moving thresholds. For the small-shape all-zero assertion, if \(r\) is bounded below the unit-rate gamma with mean \(a\) lies below \(r\) with probability tending to one by Markov's inequality. Along \(r\to0\) the bounds
$$
e^{-r}r^a/\Gamma(1+a)\ \le\ p_0\ \le\ r^a/\Gamma(1+a)
$$
follow by bounding \(e^{-y}\) in the defining gamma integral on \([0,r]\). More generally if \(a\to0,\ r\to0\) and \(a(-\log r)\to c\in[0,\infty]\), then \(p_0\to e^{-c}\), while every fixed positive cell loses its mass by the dossier's uniform bound. For \(c>0\) this small-shape corner cannot maximize even for all zeros.

## Numerical search and reproduction

**Proved search reduction.** At fixed \(a,r\), stationarity imposes two linear moment equalities on the nonnegative cell frequencies, as well as normalization. For a finite candidate cell set, vertices of the feasible frequency polytope are supported on at most three cells. The second derivative along a line of rate slope \(v\) is \(L_{aa}+2v L_{ar}+v^2L_{rr}\), affine in those frequencies. At joint stationarity \(L_{rr}<0\) from \(L_{uu}<0\), and the profile curvature is the maximum over \(v\) of this line curvature. A positive profile curvature anywhere in the polytope implies a positive curvature at some vertex, by fixing the witnessing \(v\). Such a strict stationary minimum would give a strict three-shape valley. Replacing feasible real frequencies by nearby rational frequencies preserves the strict valley by continuity of the profile at those three shapes. Clearing denominators would give an integer sample counterexample.

**Numerical.** [stationary_scan.py](2026-11-25-dgamma-math-data/stationary_scan.py) enumerates strictly positive three-cell frequency solutions at 85 geometric shapes from \(0.01\) to \(10^4\) and 100 geometric latent means from \(0.025\) to \(350\). The cell set is \(0\) through \(30\), plus \(40,60,100,200,500\). It uses binary64 and 96-node Gauss–Legendre conditional quadrature. The zero cell uses \(-\log Y\) and a finite quadrature cutoff. Digamma and trigamma are evaluated at 30 decimal digits then cast to binary64. The scan yielded 1,087,330 admissible numerical triples and zero with curvature above \(10^{-6}\) in log shape. Its largest nominal curvature was \(3.19834\times10^{-9}\). This exploratory quadrature can misclassify signs and weights in narrow tails. The lattice omits exact two-cell stationary solutions, triples outside the cell set and points between grid nodes.

**Numerical.** [verify.py](2026-11-25-dgamma-math-data/verify.py) independently solves the profile rate by 120 bisections in the conditional-mean bracket, then differentiates the incomplete-gamma interval probabilities with mpmath. At both 60 and 90 decimal digits the following score signs were observed on quarter-decade shape grids. The signs are listed in ascending shape order.

| Cell frequencies | Shape exponents for \(10^t\) | Consecutive signs |
|---|---|---|
| \(10:1,\ 100:1\) | \([-4,3]\) | 17 positive, 12 negative |
| \(0:1,\ 2:1\) | \([-4,3]\) | 17 positive, 12 negative |
| \(0:999998,\ 1:1,\ 2:1\) | \([-9,3]\) | 15 positive, 34 negative |
| \(0:200,\ 1:200,\ 2:1\) | \([-4,3]\) | 21 positive, 8 negative |
| \(0:3,\ 1:7\) | \([-4,1.75]\) | 24 positive |
| \(5:1\) | \([-4,3]\) | 29 positive |

The adjacent grid ends at \(10^{1.75}\) because the score at larger shape becomes too small for this subtraction-based replay at 60 digits. The midpoint deficit for `(1, 3)` at \(100,200,300\) was \(0.050812359800762070718\) per observation. On the two replayed triples with cells \(0,1,2\), high-precision reconstruction and differentiation reversed the binary64 positive signs. At shape \(0.372759372031494\) and mean \(0.025\), the log-shape curvature was \(-1.65436\times10^{-14}\) at both 60 and 90 digits. At the largest nominal binary64 result, shape \(6105.402296585326\) and mean \(1.1834318555465324\), it was \(-8.33311\times10^{-435}\) at 550 digits. These finite numerical checks have no outward error enclosures.

Both final scripts exited zero on the workstation, sequentially using one reserved CPU slot each. No randomness is used. Run from the worktree root.

```sh
mkdir -p build/tmp
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
export OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 MKL_NUM_THREADS=1
data=dev/research/evaluations/2026-11-25-dgamma-math-data
/tmp/umvl-cpu/run nice -n 10 python3 "$data/stationary_scan.py"
/tmp/umvl-cpu/run nice -n 10 python3 "$data/verify.py"
```

## What this changes for the fitter

- Keep `PointMass` and `TwoPointLimit` on the existing free-fit routes. The adjacent-cell limit has the observed Bernoulli weights and the lower occupied cell as its offset. For either held coefficient, the all-zero route keeps `PointMass`; a positive count gives an attained unique maximum under that hold.
- Held rate can use a bracketed shape score solve. For both parameters free, an affirmative sufficient-condition check allows the same scalar solve on the profile, with the proved shape-score tails for initialization. Verify both coefficient tolerances from the shape bracket and inner rate brackets. A rate bracket at each shape endpoint encloses all intervening optimal rates by monotonicity. In the existing subdivision, the signs \(G(A,r)>0\) and \(G(B,r)<0\) directly justify lower and upper rate cuts for a shape interval \([A,B]\), respectively.
- For general data, keep global subdivision. A possible uniqueness certificate covers all possible stationary shapes and, on each interval, either excludes zero from the profile score or proves strictly negative profile curvature. Strict crossing at every root gives uniqueness. The certificate needs practical precision margins for scores, inner brackets and curvature. If the global comparison remains unresolved after a checked, successful search, the user-directed fallback is an uncertified estimate. An unresolved coefficient accuracy check or a failed search cannot supply a successful candidate.

## Open

- Universal quasi-concavity, strict unimodality and uniqueness of the free fit remain open outside the proved sample conditions. One sufficient theorem would show strictly negative Schur-complement curvature for all stationary two- and three-cell frequency vertices over continuous positive shape and rate and arbitrary cell indices. The finite vertex scan does not establish that inequality, and exact degeneracies would need separate treatment. A strict three-shape valley with an upper bound at the middle shape and separated lower witnesses on both sides would instead refute quasi-concavity.
- The cheap curvature condition uses bounded \(\log Y\) oscillation within each cell. Its proof does not cover samples containing zero. A replacement using conditional log moments or a guaranteed profile curvature enclosure would be needed for a comparably cheap check covering those inputs.
