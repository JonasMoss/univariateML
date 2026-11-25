# Discretised gamma stationary curvature and zero counts

25 November 2026 · TASK-278 · base `bb391a57789c0f23ea7501668d07ce4deac09955`

**Proved.** A sample-only curvature check works with zero counts and takes one pass over the occupied cells. It proves strict unimodality and a unique finite fit when it passes. A second check encloses the curvature on profile intervals, including intervals for samples with many zeros.

**Open.** Universal quasi-concavity. The stationary inequality holds in the small-shape region proved below. The extended numerical search found no validated positive-curvature vertex. A reduction shows that universal *nonpositive* curvature at the vertices would suffice, including when a putative valley has a flat minimum.

## Model and stationary vertices

Write \(p_k(a,r)=\Pr(k\le Y<k+1)\) for \(Y\sim\operatorname{Gamma}(a,r)\) with shape \(a\) and rate \(r\), \(w_k\) for the observed fraction in cell \(k\), and \(\bar k=\sum_k w_k k\). Let \(L(a,r)=\sum_k w_k\log p_k(a,r)\). For \(\bar k>0\), write \(P(a)=L(a,r(a))\) for the rate profile and \(m(a)=a/r(a)\). TASK-243 proved that \(r(a)\) is analytic and strictly increasing, that \(\bar k<m(a)<\bar k+1\), and that the shape score is positive at small shape and negative at large shape for samples of range at least two. The dossier gives attainment in that regime. [TASK-243](2026-11-25-dgamma-math-report.md) · [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/dgamma/README.md)

**Proved.** Put \((\mu_k,\nu_k)=(E_kY,E_k\log Y)\), where \(E_k\) conditions on cell \(k\). The columns \((1,\mu_k,\nu_k)\) for three distinct cells are always linearly independent. In increasing cell order, their determinant is the expectation of the determinant with columns \((1,Y_k,\log Y_k)\) for independent conditional draws. The latter is strictly negative by the strictly decreasing secant slopes of \(\log y\). Integrability at zero holds for every \(a>0\). Thus a three-cell vertex has unique weights from the two score equations and normalization. A pair \(i<j\) is stationary with positive weights precisely when \(\mu_i<m<\mu_j\) and \((m,\psi(a)-\log r)\) lies on the line through its two centroids. Its lower weight is \((\mu_j-m)/(\mu_j-\mu_i)\). This gives a scalar determinant equation for \(m\) at fixed \(a\).

**Proved.** A stationary vertex cannot be supported on a singleton or an adjacent pair. For a positive singleton \(k\), the rate equation gives \(r=a\log((k+1)/k)\) by equality of the two endpoint fluxes. The function \(\log y-(r/a)y\) takes the same value at the endpoints and is strictly greater inside the cell than on its complement. Its conditional mean therefore exceeds its full mean, giving a positive directional score. The all-zero singleton has positive rate score at every finite rate. For an adjacent pair \(i,j=i+1\), take \(v=(\nu_j-\nu_i)/(\mu_j-\mu_i)>0\). The statistic \(s(y)=\log y-vy\) has the same conditional mean \(c\) on both cells. Each cell contains values above and below \(c\). Strict concavity and the single maximum of \(s\) give one root of \(s(y)=c\) inside each cell and \(s(y)<c\) outside their union. Thus \(Es(Y)<c\), and moving along \((da,dr)=(1,v)\) gives a positive score for any weights on the pair.

### Flat valleys and the sign test

**Proved.** Universal quasi-concavity on finite samples of range at least two is equivalent to \(P''(a)\le 0\) at every feasible stationary vertex over arbitrary finite cell sets and positive parameters. The frequency-polytope argument from TASK-243 gives one direction of the reduction. At stationarity \(L_{rr}<0\) and
$$
P''(a)=\max_{v\in\mathbb R}\left(L_{aa}+2vL_{ar}+v^2L_{rr}\right).
$$
If an admissible frequency vector has \(P''>0\), the line curvature at its maximizing \(v\) is positive at some vertex because it is affine in the frequencies. A positive vertex gives a strict profile valley. The inner root is continuous in the weights at each of the three witnessing shapes, so nearby rational weights preserve the valley and give an exact integer sample after clearing denominators.

For the other direction, a strict valley in an analytic profile gives a strict interior local minimum at \(a_*\). Write its score locally as \(c(a-a_*)^j+O((a-a_*)^{j+1})\) with \(j\ge1\) odd and \(c>0\). Add a cell \(k\) by replacing \(w\) with \((1-\epsilon)w+\epsilon e_k\). The perturbed rate root is locally analytic in \((a,\epsilon)\) by the implicit function theorem and \(L_{rr}<0\). For the perturbed profile score \(F(a,\epsilon)\), implicit differentiation at \((a_*,0)\) gives
$$
F_\epsilon=\ell_{k,a}-\frac{L_{ar}}{L_{rr}}\ell_{k,r},
\qquad \ell_k=\log p_k .
$$
Here \(\ell_{k,a}=\log k+O(1)\) and \(\ell_{k,r}=-k+O(1)\) as \(k\to\infty\) at the fixed parameter pair, by the cell bounds on \(\log Y\) and \(Y\). Hence some \(k\) has \(F_\epsilon\ne0\). Solving \(F(a,\epsilon(a))=0\) gives
$$
\epsilon(a)=-\frac{c}{F_\epsilon}(a-a_*)^j+O((a-a_*)^{j+1}).
$$
On one side of \(a_*\) this is positive, with \(F_a(a,\epsilon(a))=jc(a-a_*)^{j-1}+O((a-a_*)^j)>0\). This is an admissible stationary profile with positive curvature, which yields a positive vertex by the preceding argument. Thus even a flat valley forces a positive vertex. Absence of valleys implies strict unimodality in the attained regime because a nonconstant analytic quasi-concave profile is monotone on either side of its maximum and cannot have a plateau. Inner rate uniqueness then gives joint uniqueness.

## A sample-only curvature check

**Proved.** At a profiled point, use the statistics \(Z=\log Y-Y/m\) and \(X=Y/m\), holding this basis fixed when differentiating. The full covariance is diagonal with entries \(D(a)=\psi'(a)-1/a\) and \(1/a\). The Hessian in natural coordinates is the weighted within-cell covariance minus the full covariance. These identities and the profile score \(P'(a)=\log a-\psi(a)-\sum_k w_k E_k d(Y/m)\), with \(d(x)=x-1-\log x\), are from TASK-243. [TASK-243](2026-11-25-dgamma-math-report.md)

### Zero-cell variance

**Proved.** For every \(a,r>0\), \(\operatorname{Var}_0(Z)\le 1/a^2\). In the zero cell let \(t=-\log Y\), so \(t\ge0\) and the conditional kernel is \(e^{-a z(t)}\,dt\), where \(z(t)=t+e^{-t}/m=-Z\) and \(m=a/r\). Let \(z_*\) be its minimum on this half-line. The density of \(V=z(t)-z_*\) is proportional to \(e^{-av}h(v)\), where \(h(v)\) is the sum of \(1/|z'(t)|\) over the inverse branches at level \(v\). The function \(z\) is strictly convex. On each branch \(|z'|\) increases away from the minimizing end, and a branch that exits the support drops out of the sum. Thus \(h\) is nonincreasing, including when the minimum is at the endpoint.

For the resulting density \(f\) and survival \(S\), \(f(v+s)\le e^{-as}f(v)\) for \(s\ge0\) at interior levels, so \(S(v)\le f(v)/a\). The cumulative hazard \(-\log S\) increases with derivative at least \(a\) almost everywhere. Its inverse maps an exponential of rate 1 to \(V\) and has Lipschitz constant \(1/a\). The identity \(\operatorname{Var}(V)=E[(V-V')^2]/2\) for independent copies gives the variance bound. The argument also works for a positive cell by restricting \(t\) to its finite interval.

### Score cutoff and trace bound

**Proved.** Choose any two occupied cells \(l<h\) with \(h>l+1\), and define
$$
b=l+1,\qquad
m_*=\frac{w_l b+w_h h}{w_l+w_h},\qquad
d_* = w_l d(b/m_*)+w_h d(h/m_*),\qquad A=1/d_* .
$$
On \(b\le m\le h\), the pair's mean deviance is at least \(w_l d(b/m)+w_h d(h/m)\). The derivative of this lower bound is \((w_l+w_h)(m-m_*)/m^2\), so its minimum is the displayed \(d_*>0\). For \(m<b\), the upper cell alone bounds the deviance below by the value at \(b\), and for \(m>h\) the lower cell does the same at \(h\). Thus the tail bound \(\log a-\psi(a)<1/a\) gives \(P'(a)<0\) for \(a\ge A\).

For \(k\ge1\) let
$$
B_k=\max\left\{
\left|\frac1k-\frac1{\bar k+1}\right|,
\left|\frac1{k+1}-\frac1{\bar k}\right|
\right\},
\qquad G(a)=\tfrac12\bigl(1+(1+a)^{-2}\bigr).
$$
The range of \(Z\) on cell \(k\) is at most \(B_k\) by the derivative \(1/y-1/m\) and the bracket for \(m\). The range of \(X\) on every cell is at most \(1/\bar k\). The trigamma series and strict trapezoidal overestimation of the integral from index 1 give
$$
\psi'(a)>\frac1{a^2}+\frac1{a+1}+\frac1{2(a+1)^2},
\qquad a^2D(a)>G(a).
$$
Since the variance of a variable of range \(b'\) is at most \((b')^2/4\), for \(a\le A\) the within-cell covariance standardized by the full covariance has trace at most
$$
\mathcal T(A)=
\frac{w_0+(A^2/4)\sum_{k\ge1} w_k B_k^2}{G(A)}+
\frac{A}{4\bar k^2}.
$$
This uses the zero-cell bound and monotonicity of \(G\). The standardized covariance is positive semidefinite, so its largest eigenvalue is at most its trace. If \(\mathcal T(A)<1\), the Hessian is negative definite throughout this part of the profile and its Schur complement is strictly negative. The two strict score tails then give exactly one simple score zero, with a strict increase before it and strict decrease after it.

The check costs \(O(K)\) arithmetic for \(K\) occupied cells after aggregation, using the extreme occupied pair for \(A\). Searching all pairs for the strongest cutoff costs \(O(K^2)\). The trace uses arithmetic only, and the selected pair uses two deviance evaluations. The sample `(0, 1000)` passes by elementary bounds. Its \(d_*=\log(1001/(2\sqrt{1000}))>2\), so \(A<1/2\), \(G(A)>13/18\), and \(B_{1000}<1/500\). These bounds give \(\mathcal T(A)<0.70\).

### A continuous region of the stationary inequality

**Proved.** Every stationary point with \(\bar k\ge1\) and \(0<a\le e^{-3}\) has strictly negative profile curvature, with arbitrary zero fraction and cell indices. Put \(q=1-w_0\). In the zero cell the exponential density of \(t=-\log Y\) at rate \(a\) is tilted by the increasing function \(e^{-r e^{-t}}\), so \(E_0\log Y\le-1/a\). This follows from nonnegative covariance of two increasing functions of the same random variable. For the positive cells, Jensen's inequality and the rate equation give \(\sum_{k\ge1}w_k E_k\log Y\le q\log(m/q)\). The stationary shape equation and digamma recurrence now give
$$
q/a\ge \psi(1+a)-\log a+(1-q)\log m+q\log q>3/2.
$$
The last inequality uses \(m\ge1\), \(\psi(1+a)>-1\), and \(q\log q\ge-1/e\). In the pointwise trace bound use \(A=a\), \(B_k\le1\), and \(G(a)\ge1-a\). The trace is at most \([1-q(1-a^2/4)]/(1-a)+a/4<1\) because \(q(1-a^2/4)>5a/4\) in this region. The same trace bound also gives strict curvature for all profile points with \(\bar k\ge1\), \(w_0\le1/4\), and \(a\le1\), since \(G(a)\ge5/8\) there and the trace is at most \(0.95\).

## Curvature on a profile interval

**Proved.** On a shape interval \([A_0,B_0]\), the inner brackets at its endpoints and monotonicity of \(r(a)\) give a rectangle \([A_0,B_0]\times[R,S]\) containing the profile. For a cell \(k\), use the normalizer
$$
J_k(a,r)=\int_k^{k+1}(y/(k+1))^a e^{-r(y-k)}\,dy/y.
$$
The kernel decreases in both parameters. The conditional density anywhere in the rectangle is at most \(\rho_k=J_k(A_0,R)/J_k(B_0,S)\) times the anchor density at \((A_0,R)\). For any linear combination of \((\log Y,Y)\), bound its conditional variance by its expected squared deviation from the anchor mean, then use density domination. Thus the weighted within-cell covariance everywhere in the rectangle is bounded above in positive semidefinite order by \(Q=\sum_k w_k\rho_k C_k(A_0,R)\), where \(C_k\) is the cell covariance in this basis. All moments exist in the zero cell since \(A_0>0\).

The full covariance in this basis has diagonal \((\psi'(a),a/r^2)\) and cross term \(1/r\). Set
$$
t_*=\psi'(B_0)-Q_{11},\qquad y_*=\frac{A_0}{S^2}-Q_{22},
\qquad c_*=\max\{|1/R-Q_{12}|,\ |1/S-Q_{12}|\}.
$$
If \(t_*,y_*>0\) and \(t_*y_*>c_*^2\), the full covariance minus \(Q\) is positive definite throughout the rectangle. This proves \(P''<0\) on the shape interval.

**Proved.** For score exclusion, set \(U(a,r)=E_0\log Y+1/a\). Integration by parts in \(t=-\log Y\) gives
$$
aJ_0(a,r)=e^{-r}+r\int_0^\infty e^{-(a+1)t-r e^{-t}}\,dt .
$$
Then \(U=\partial_a\log(aJ_0)\) is the negative mean of \(t\) under the resulting exponential tilt of a measure with an atom at zero. Its derivative in \(a\) is a variance, so \(U\) increases in \(a\). Its derivative in \(r\) is \(-\operatorname{Cov}_0(\log Y,Y)\le0\). Both monotonicities also hold for each \(E_k\log Y\) in the positive cells by differentiating the cell integral. With \(M(a,r)=w_0U(a,r)+\sum_{k\ge1}w_k E_k\log Y\), the score is
$$
P'(a)=\log r(a)-\psi(1+a)+(1-w_0)/a+M(a,r(a)).
$$
A lower bound is \(\log R-\psi(1+B_0)+(1-w_0)/B_0+M(A_0,S)\). An upper bound uses \(\log S-\psi(1+A_0)+(1-w_0)/A_0+M(B_0,R)\). Combining the singular terms first reduces the interval uncertainty on zero-heavy samples.

Cover the compact interval between the small-shape score cutoff from TASK-243 and the upper cutoff \(A\). Each subinterval must exclude zero from the score or pass the curvature test. A successful cover forces a strict downward crossing at every root and hence exactly one root. With exact evaluations and shrinking rate-bracket errors, this subdivision terminates whenever every stationary curvature of the particular sample is strictly negative. Indeed each stationary point has a neighborhood where the continuous curvature certificate succeeds for sufficiently small boxes, and every other point has one where the score excludes zero. Compactness gives a finite cover. The moment and mass bounds cost \(O(K)\) evaluations per box, plus the inner rate solve for each new shape endpoint. The anchor requires the mass and first two shape derivatives, the \(Y,Y^2\) moments, and \(E_k(Y\log Y)\). They follow from integrals with shapes \(a,a+1,a+2\) and their shape derivatives. The number of boxes is sample-dependent.

## Numerical checks

**Numerical.** The [zero-cell check](2026-11-25-dgamma2-math-data/zero_curvature.py) passed 30 moment and trigamma comparisons at 70 decimal digits, on the product grid \(a\in\{0.03,0.2,1,5,20\}\) and \(m\in\{0.1,0.49,0.5,1,10,200\}\). Using the best weighted pair in each sample gave the following trace values. Exponents in the sample column denote repetitions.

| Sample | \(\mathcal T(A)\) |
|---|---:|
| \(0,2\) | 37.18023 |
| \(0,1000\) | 0.64977 |
| \(0,10,100\) | 0.52595 |
| \(0,100^{\times9}\) | 0.18718 |
| \(0^{\times998},10,100\) | 242.15388 |
| \(10,100\) | 0.00592 |

**Numerical.** The [interval replay](2026-11-25-dgamma2-math-data/profile_boxes.py) used geometric shape subdivision and 32 bisections per rate endpoint. Both score formulas completed at 50 and 80 digits with matching box counts at the two precisions.

| Sample | Score formula | Boxes visited | Score exclusions | Curvature boxes |
|---|---|---:|---:|---:|
| \(0,2\) | Direct conditional means | 219 | 83 | 27 |
| \(0,2\) | Combined singular terms | 195 | 79 | 19 |
| \(0^{\times998},10,100\) | Direct conditional means | 10,537 | 4,013 | 1,256 |
| \(0^{\times998},10,100\) | Combined singular terms | 131 | 65 | 1 |

The smallest normalized determinant margin \(1-c_*^2/(t_*y_*)\) in the combined runs was \(0.0005703054\) and \(0.5766811\), respectively. These mpmath checks use no outward rounding.

**Numerical.** The [vertex scan](2026-11-25-dgamma2-math-data/stationary_vertices.py) used 33 geometric shapes from \(10^{-4}\) to \(10^4\) and 65 geometric means from \(0.005\) to \(20{,}000\), plus offsets \(0.01,0.2,0.5,0.8,0.99\) from each candidate cell index. The cells were \(0,1,2,3,4,6,9,14,21,32,48,73,110,250,1000,10000\). The binary64 calculation used 96-node Gauss–Legendre panels in \(\log(y/m)\), truncating exploratory tails at a drop of 90 log units and splitting long zero-cell intervals at \(-30\). Digamma and trigamma were computed at 40 digits before conversion. Pairs were sought at sign changes of their stationarity determinant across consecutive mean marks. There were 116,630 nominal feasible triples and 4,543 nominal feasible pair roots. None had curvature divided by \(D(a)\) above \(10^{-6}\). The largest nominal triple values were about \(3\times10^{-14}\).

Replays from incomplete-gamma integrals at 80 and 120 digits gave normalized curvatures \(-0.0005730848653\) for the selected pair \((0,3)\) and \(-0.2095044356\) for \((2,4)\), and \(-7.440412197\times10^{-6}\) for the selected well-resolved triple \((0,1,2)\) and \(-0.002539158594\) for \((2,3,4)\). Additional separated pair controls \((0,100)\) and \((1,100)\) at shape 1 gave \(-0.8038273751\) and \(-0.9863200331\). Two apparent positive cases from the preliminary single-panel zero-cell integration lost their evidence at both precisions. The \((0,9,14)\) triple had a weight near \(-0.00257626\), and the \((0,6)\) pair had no sign change in its proposed bracket.

The two leading nominal triples at \(a=10000\) and \(m=1.8962678784765588\) required a calculation in terms of the omitted cell masses. For retained cells \(I\) and complement blocks \(J\), let \(s_i,H_i\) be the score and Hessian of a block log probability, with mass \(p_i\). Express each omitted score in barycentric coordinates of the retained scores, preserving the sum of coefficients. Multiplying those coordinates by the omitted masses gives the differences \(\delta_i=w_i-p_i\). Differentiating normalization twice gives the exact identity
$$
H_w=\sum_{i\in I}\delta_i H_i-\sum_{j\in J}p_j H_j
     -\sum_{i\in I\cup J}p_i s_i s_i^\mathsf T.
$$
The replay projects along a shape direction that annihilates the score of the second most probable cell. The dominant cell's score and Hessian are reconstructed from the normalization identities. This keeps the tiny curvature terms out of the subtraction of the main rank-one term. The normalized curvature was \(-3.862789875801203\times10^{-532}\) for \((0,1,2)\) and \(-3.862433518043963\times10^{-532}\) for \((1,2,3)\), with positive reconstructed weights and agreement at 80 and 120 digits. These are finite numerical replays.

All final Python commands exited zero on the workstation, each under one reserved CPU slot. From the worktree root:

```sh
mkdir -p build/tmp build/scratch
export TMPDIR="$PWD/build/tmp" PYTHONDONTWRITEBYTECODE=1
data=dev/research/evaluations/2026-11-25-dgamma2-math-data
cases=build/scratch/dgamma2-refined.json
/tmp/umvl-cpu/run python3 "$data/zero_curvature.py"
/tmp/umvl-cpu/run python3 "$data/stationary_vertices.py" "$cases"
/tmp/umvl-cpu/run python3 "$data/stationary_vertices.py" --replay "$cases"
/tmp/umvl-cpu/run python3 "$data/stationary_vertices.py" --saturation "$cases"
/tmp/umvl-cpu/run python3 "$data/profile_boxes.py"
/tmp/umvl-cpu/run python3 "$data/profile_boxes.py" --raw-score
```

## What this changes for the fitter

- Apply the sample-only test after the existing free-fit `PointMass` and `TwoPointLimit` routes. A strict pass with a practical arithmetic margin gives a unique global score root for range at least two. A bracketed solve between the score cutoffs can return `Ok` with the certified bit set once the shape bracket and the enclosing rate brackets at its endpoints meet the coefficient tolerance.
- For a sample that misses the trace test, a complete score-exclusion and curvature cover gives the same uniqueness certificate. Use the combined zero-cell score and reuse inner rate brackets. Charge box work to the certificate evaluation budget. The comparison of computed bounds needs the package's practical arithmetic margins, including margins on the sign of the determinant and the inner rate brackets. [Earlier certificate design and experiments](2026-11-02-dgamma-cert-report.md)
- For inconclusive checks, use the existing search and global comparison. If a certificate remains inconclusive or exhausts its allowance, return a successful checked candidate as uncertified. A failed search or a comparison that refutes the candidate requires rejection of that candidate under the existing failure rules. With rate held, the strictly concave shape solve already follows from TASK-243. [Diagnostics](../../validation/diagnostics.md#wrapper-failure-handling-rule-r6)

## Open

- Establish nonpositive Schur-complement curvature for every feasible stationary pair and triple over continuous positive shape and rate and arbitrary cell indices, or give a strict valley for an exact sample. A valley requires separated feasible lower bounds on the outside shapes and a profiled upper bound on the middle shape. The grid omits arbitrary indices, intermediate parameter values, and pair roots that do not cause a determinant sign change across the sampled marks. The compensated replays resolve specific near-saturated anchors at the stated precisions.
- The interval test's practical activation rate and numerical margins remain to be measured in an implementation. The two numerical covers and the exact-arithmetic test give a design and reproducible cases for that work.
