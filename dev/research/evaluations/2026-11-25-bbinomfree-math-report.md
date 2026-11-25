# Beta-binomial with free size and shapes

TASK-341, base `f25d97e2`. Underdispersion gives an exact finite reduction. A rational probability-ratio cover can exclude blocks of sizes and the entire tail when a witness clears the NB/Poisson face. The integer profile can have two separate finite peaks. Finite-size profile heights are algebraic, which gives an exact tie predicate and excludes equality with a positive-mean Poisson face.

## Fixed-size ordering and the active face

Let \(h\) be the sample length, \(M=\max x_i\), \(S=\sum x_i\), \(Q=\sum x_i^2\), \(\mu=S/h\), and \(v=Q/h-\mu^2\). Write \(L_N\) for the supremum likelihood at size \(N\), \(B_N\) for the likelihood of \(\operatorname{Bin}(N,\mu/N)\), and \(L_F=\exp F\) for the optimized infinite-size face. The prior closure theorem gives \(L_N\to L_F\). The finite-size closure consists of the beta-binomial and its binomial, endpoint-mixture and point-mass faces. The only growing-size faces retaining likelihood with positive counts are NB and Poisson. This is on the original count scale. Normalizing observations by \(N\) does not add a continuous beta face to that likelihood. [Closure theorem](2026-11-03-bbinom-closure-report.md)

**Proved from the fixed-size theorem.** With \(D=hS+S^2-hQ=h^2(\mu-v)\), put \(\Delta_N=S^2-ND\). At \(N\ge2\) with an interior count, \(\Delta_N>0\) gives attained \(L_N>B_N\). For \(\Delta_N\le0\), \(L_N=B_N\) and every finite positive shape pair is strictly below it, including at equality. Fixed-size uniqueness is known for sizes two and three and reflection-symmetric data. [Existence](2026-11-25-bbinom-math-report.md) · [Uniqueness](2026-11-25-bbinom2-math-report.md)

**Proved finite reduction for \(D>0\).** After the elementary cases below, only \(M\le N<K=\lceil S^2/D\rceil\) can attain with finite shapes. At all sizes \(N\ge\max(M,K)\) the profile equals the ordinary binomial profile. Find its optimum \(B=\max_{N\ge M} B_N\) by the certified binomial size search. It is strictly above the active infinite-size Poisson face. Compare \(B\) with the attained profiles in the finite prefix. Beyond \(\max(K-1,N_B)\), where \(N_B\) is the larger binomial maximizing size in case of a binomial tie, the binomial profile is strictly decreasing and no size can match the best value. If a binomial maximizing size lies before \(K\), its own beta-binomial profile strictly beats \(B\). An earlier size can still beat \(B\) when the binomial winner lies at or beyond \(K\). [Ordinary size-profile shape and exact adjacent comparison](2026-11-25-binom-ties-math-report.md)

**Proved face ordering for \(D\le0\).** For \(D<0\), the finite-shape NB optimum exceeds Poisson, and the binomial supremum is Poisson. For \(D=0\) and \(\mu>0\), both ordinary faces have Poisson supremum. After elementary dispatch, every finite size has an attained shape maximum. A witness strictly above \(L_F\) proves finite global attainment. A finite point equal to \(L_F\) also attains if it dominates all other finite candidates. [Closure and attainment criterion](2026-11-03-bbinom-closure-report.md#32-finite-size-and-finite-shapes-are-different-questions)

**Proved elementary dispatch.**

| Sample condition | Global result |
|---|---|
| All zeros | \(N=0\) and all \(a,b>0\) attain one. Every positive-size closure ties at one via an excluded point mass. |
| Constant \(M>0\) | Excluded point mass at size \(M\). For \(N>M\), even the maximum binomial kernel at count \(M\) is strictly below one, so all beta mixtures lose. |
| Both 0 and 1, \(M=1\) | Only \(N=1\) attains. The complete shape set is \(c(\mu,1-\mu)\), \(c>0\). |
| Support exactly \(\{0,M\}\), \(M\ge2\) | The excluded empirical endpoint mixture at size \(M\) strictly beats every larger finite-size closure and the infinite-size face. |
| Full support \(0,\ldots,M\), \(M\ge2\), empirical law equals a positive-shape beta-binomial | Unique attained global fit at \(N=M\), by saturation. |

The saturated likelihood is attained only by the empirical pmf. To test the last row, form \(s_j=(j+1)f_{j+1}/[(M-j)f_j]\) from the observed frequencies. Solve \(a+j=s_j(b+M-j-1)\) at \(j=0,1\), check positivity and the remaining ratios. Equal \(s_0,s_1>0\) would imply \(1=-s_0\), so a solution is unique. A larger finite model places mass outside the empirical support. At \(M=2\) with an interior observation, \(\Delta_2>0\) implies \(p^2<f_2<p\) for \(p=\mu/2\). The beta second moment \(f_2=p^2+p(1-p)/(1+a+b)\) then gives saturation. If \(\Delta_2\le0\), every feasible size is binomial. Thus any attained positive-count tie between different sizes requires \(M\ge3\). The exact samples \((0,1,2)\) and \((1,2)\) illustrate respectively a unique saturated fit and a strict excluded binomial winner at \(N=2\). [Earlier elementary examples](2026-11-03-bbinom-closure-report.md#5-explicit-witnesses-and-failed-conjectures)

## Rational bounds for blocks of sizes

**Proved.** Set \(k=1/N,\ d=N/(b+N),\ z=ad\). The exact probability ratio is
\[
R_j(k,z,d)=\frac{P(j+1)}{P(j)}
=\frac{(z+jd)(1-jk)}{(j+1)[1-(j+1)dk]}.
\]
For a size block \([K,H]\), with \(H=\infty\) allowed, use a normalization prefix \(M<J<K\). On a rectangle with \(0\le d\le1\), ratios with \(j<J\) increase in \(z,d\), and their derivative in \(k\) has sign \((j+1)d-j\) for a positive shape factor. Lower and upper bounds \(r_j^-,r_j^+\) therefore use two corners in \((z,d)\) and the appropriate endpoints of \([1/H,1/K]\). Normalization gives
\[
P(x)\le\left[
1+\sum_{i=0}^{x-1}\prod_{j=i}^{x-1}(r_j^+)^{-1}
 +\sum_{i=x+1}^J\prod_{j=x}^{i-1}r_j^-
\right]^{-1}.
\]
If an upper ratio before \(x\) is zero, its probability bound is zero. Multiply over observations with their multiplicities. A second bound uses the log likelihood of the normalized prefix. In \(\eta_j=\log R_j\), write \(T_i=\sum_{j<i}\eta_j\). The objective \(\sum_i h_i T_i-h\log\sum_{i=0}^J e^{T_i}\) is concave, where \(h_i\) are the observed counts in each cell. At a reference in the box the tangent coefficient is \(\sum_{i>j}h_i-h\sum_{i>j}e^{T_i}/\sum_i e^{T_i}\). Bound each signed log increment using \(\log y\le y-1\) with the upper ratio for a positive coefficient and the reciprocal lower ratio for a negative coefficient. If the total log increment bound is \(0\le t<1\), the likelihood multiplier is at most \(1/(1-t)\). Skip this tangent if it needs a reciprocal of zero. The prefix likelihood bounds the actual likelihood from above.

A reachable lower witness \(W>0\) permits a finite starting rectangle. Likelihood at least \(W\) requires \(P(M)\ge W\), so \(P(J)\le1\) excludes all \(z\ge Z\) when
\[
W\prod_{j=M}^{J-1}\frac{Z(1-j/K)}{j+1}>1.
\]
A finite rectangle cover of \([0,Z]\times[0,1]\) with all upper likelihoods below \(W\) excludes the block, including its finite-size shape limits by continuity. The number of operations in one box is independent of the length of the integer block.

**Proved termination in principle for \(W>L_F\).** Fix the compact rectangle using \(J=M+1\) and \(K\ge2(M+1)\) in the \(Z\)-bound. At \(k=0\) the normalized prefix likelihood decreases in \(J\). For \(z>0,d<1\), the limit is NB with shape \(z/d\) and mean \(z/(1-d)\), or Poisson with mean \(z\) at \(d=0\). The normalizing sums follow from the binomial series \((1-d)^{-z/d}\) and the exponential series. At \(d=1,z>0\) the normalizing sums diverge by the same positive binomial series at one. At \(z=0\) all positive observed counts have zero prefix probability. Thus the closed sets where prefix likelihood is at least \(W\) are nested with empty intersection in the compact rectangle. Some finite \(J\) gives a strict uniform gap at \(k=0\). For this \(J\), increase \(K\) and shrink the boxes. The probability bounds converge to the normalized prefix probabilities, including the zero-probability case at \(z=0\). The strict gap is therefore verifiable with a finite cover. This complements the prior explicit \(O(1/N)\) envelope. [Earlier tail bound](2026-11-03-bbinom-cert-report.md)

## Signed bounds when the infinite-size face can win

### An upper bound that equals the face at zero

**Proved.** For \(d<1\), put \(A(t)=1-d+dt\) and
\[
f(t)=k^{-1}\log[1+kz/A(t)]=\int_0^z\frac{dy}{A(t)+ky}.
\]
Use the integral at \(k=0\). The exact \(-\log P(0)\) is \(k\sum_{j=0}^{N-1} f(jk)\). The trapezoidal identity gives the log-likelihood upper bound
\[
U(k,z,d)=\sum_i\sum_{j<x_i}\log R_j
-h\left[\int_0^1 f(t)\,dt+\frac{k}{2}(f(0)-f(1))\right].
\]
In fact \(U-\ell=h(\mathrm{Trap}-\int f)\). Integrating by parts twice on a cell \([jk,(j+1)k]\), the trapezoid error is the integral of \(f''(t)(t-jk)((j+1)k-t)/2\). Since \(f''\ge0\) is decreasing, \(U_4=U-hk^2 f''(1)/12\) is also an upper bound, with error at most \(hk^2[f''(0)-f''(1)]/12\). Both upper bounds equal the NB or Poisson log likelihood at \(k=0\). Derivatives can be enclosed via the integral representation with positive denominators.

### Finite NB optimum

**Proved.** For \(v>\mu\) and unique NB shape optimum \(\widehat a\),
\[
\log L_N=F+T/N+o(1/N),\qquad
T=\frac{h[\mu^2-\widehat a(v-\mu)]}{2(\widehat a+\mu)}.
\]
The prior pointwise expansion gives the coefficient. Its uniform version on compact interior \((z,d)\) sets follows from smoothness of \(U\) and its \(O(k^2)\) approximation error. By closure and uniqueness of the NB optimum, the coordinates of maximizing shapes converge to that optimum. Evaluate the expansion at the limiting optimum for a lower bound. For the upper bound use it at a maximizing pair, whose face height is \(\le F\) and whose coefficient converges to \(T\). [Pointwise NB expansion](2026-11-02-bbinom-map-report.md#expansion-at-the-negative-binomial-face)

At the rational moment shape \(a_m=\mu^2/(v-\mu)\), the profiled score is
\[
g(a_m)=\sum_i\sum_{j<x_i}(a_m+j)^{-1}-h\log(1+\mu/a_m).
\]
The logarithm is transcendental by Hermite–Lindemann, so \(g(a_m)\ne0\). Its exact sign can be found by shrinking rational log bounds and is opposite the sign of \(T\), by the unique downward crossing of the NB score. Thus **the linear coefficient never vanishes on integer data with \(v>\mu\)**. Positive \(T\) proves that some finite witness beats NB. For negative \(T\), verify \(\partial_k U<0\) in a rectangle about the NB optimum and \(0\le k\le1/K\). At \(k=0\) the bound is \(\le F\), so this gives strict pointwise exclusion at positive \(k\). The complement has a strict exterior gap at \(k=0\) by uniqueness and the compact prefix argument, and is handled by the prefix cover after increasing \(K\). The remaining finite sizes still need comparison.

### Poisson at equidispersion

Write \(F_j=h^{-1}\sum_i(x_i)_{\underline j}\). At \(v=\mu\) the established joint Poisson expansion, in \(u=1/a,\ k=1/N\) and mean \(m=Na/(a+b)=\mu\), has quadratic \(B(u^2+k^2)+\mu^2uk/2\), where
\[
B=(\mu^3-F_3)/6-\mu^2/4<0,\qquad G=(\mu^3-F_3)/3.
\]
Profiling in \(u\) gives the coefficient \(B-\mu^4/(16B)\) at \(u=-\mu^2k/(4B)\), with sign \(G\). Profiling the mean leaves this coefficient intact because the derivative in \(m\) of the linear coefficient \(F_2/2-m\mu+m^2/2\) is zero at \(\mu\). \(G>0\) implies finite attainment and \(G<0\) a strictly losing large-size tail. A losing tail can still coexist with a finite winner. [Joint expansion and earlier examples](2026-11-03-bbinom-closure-report.md#42-equidispersion-a-sharp-local-threshold-and-a-global-sufficient-condition)

**Proved fourth-order test at \(G=0\).** Define
\[
\begin{split}
D_4 &= \mu^5/10+\mu^4/2-5\mu^3/6-\mu^2/4-3F_4/4-F_5/10,\\
H &= (F_4-\mu^4)/4+\mu^3/2+\mu^2/2,\qquad C_4=D_4-H^2/(4B).
\end{split}
\]
Then \((\log L_N-F)/h=C_4/N^4+o(N^{-4})\). A feasible path with the same coefficient is \(u=k-Hk^2/(2B),\ m=\mu\).

For the derivation, along \(u=k\) the ratio factor relative to Poisson is \((1-j^2k^2)/(1-m(j+1)k^2)\). Put \(x=N^2/m\). The zero log probability is \(2\log\Gamma(x)-\log\Gamma(x-N)-\log\Gamma(x+N)\). Its even Taylor terms through fourth order in \(k\) come from \(-N^2\psi'(x)-N^4\psi'''(x)/12-N^6\psi^{(5)}(x)/360\). Euler–Maclaurin on \(\psi^{(2r-1)}(x)=(2r-1)!\sum_{j\ge0}(x+j)^{-2r}\) gives respectively \(1/x+1/(2x^2)+1/(6x^3)\), \(2/x^3+3/x^4\), and \(24/x^5\), with omitted terms contributing \(O(k^6)\). The even Taylor remainder has the same order by the corresponding sum bound on the eighth derivative. This gives the zero-cell coefficients \(-m^2/2-m^3/6\) and \(-m^3/6-m^4/4-m^5/15\). For \(D_4\), add \((2h)^{-1}\sum_i\sum_{j<x_i}[-j^4+\mu^2(j+1)^2]\) to the latter and use \(F_2=\mu^2,\ F_3=\mu^3\). The mean derivative of the diagonal \(k^2\) coefficient is \(-\mu-\mu^2/2+h^{-1}\sum_i x_i(x_i+1)/2=0\).

For the \(u\) derivative, differentiate \(\log P(0)=\sum_{j=0}^{N-1}\log[1-mk/(1+mu\,jk)]\) at \(u=k\). With \(t=jk\), the derivative summand is \(m^2kt[1-mk(2t-1)+m^2k^2(3t^2-3t+1)+O(k^3)]\). The sums \(k\sum t,\ k\sum t(2t-1),\ k\sum t(3t^2-3t+1)\) equal \((1-k)/2,\ 1/6-k/2+k^2/3,\ 1/4+O(k)\). The zero-cell derivative's quadratic coefficient is \(m^3/2+m^4/4\). The corresponding log-ratio coefficient is \(j^3-m^2(j+1)\), by differentiating the exact factors \((1-jk)(1+ju)/[1+m(u-k)-m(j+1)uk]\). At \(m=\mu\) the constant and linear coefficients cancel by equidispersion and \(G=0\). Adding the quadratic terms gives \(H\).

For uniform profiling, substitute \(z=m/[1+m(u-k)],\ d=uz\) into \(U_4/h\). In a Poisson neighborhood this is smooth through \(k=u=0\). There \(f''(0)-f''(1)=O(d^3)\) by the integral for \(f\). Along \(u=k+O(k^2)\) its error is \(O(k^5)\), with \(u\) derivative error \(O(k^4)\). The derivative bound follows by differentiating the exact quadrature error with integrand \(f''(t)-f''(1)\), whose \(u\) derivative is \(O(d^2)\). The mean derivative error is \(O(k^5)\) there. In \((m-\mu,u-k)\) the Hessian tends to \(\operatorname{diag}(-1/\mu,2B)\). At the diagonal pivot the gradients are \(O(k^4)\) and \(Hk^2+O(k^3)\), and the mixed Hessian is \(O(k)\). The implicit function theorem and strict concavity in a sufficiently small rectangle give the upper bound's maximizing perturbations \(m-\mu=O(k^3)\) and \(u-k=-Hk^2/(2B)+O(k^3)\). By closure, maximizing sequences enter this Poisson neighborhood. The upper optimum and the plug-in lower witness then have the same fourth-order coefficient by completing the square. At the quadratic pivot the same argument establishes the \(k^2\) coefficient when \(G\ne0\).

For a signed cutoff at \(G<0\) or \(G=0,C_4<0\), verify a nuisance Hessian upper bound \(-\lambda I\), \(\lambda>0\), for \(U_4/h\) on a Poisson rectangle. Its maximum is at most the pivot height plus the squared pivot gradient norm divided by \(2\lambda\). At the respective quadratic and quartic pivots the corrections are \(O(k^4)\) and \(O(k^6)\). Taylor bounds can certify the sign after factoring out \(k^2\) or \(k^4\), with the exterior handled by the prefix cover.

| Exact critical sample with \(v=\mu\) and \(F_3=\mu^3\) | \(C_4\) | Consequence |
|---|---:|---|
| \((1,1,4)\) | \(23/15\) | Finite attainment. The fixed diagonal coefficient is \(-37/15\), so optimizing the offset reverses the sign. |
| \((0,0,1,1,1,3)\) | \(19/240\) | Finite attainment. |
| Frequencies \((9,8,6,0,1)\) from count zero | \(-7/30\) | Sufficiently large sizes lose. The finite prefix still needs comparison. |
| Frequencies \((614,845,80,250,10,1)\) from count zero | \(0\) | Fourth order gives no sign. |

## Nonunimodality and exact ties

**Proved nonunimodality and global singleton for \((1,1,5)\).** Rational log bounds locate the NB shape strictly between \(451/100\) and \(113/25\), above \(49/11\), the moment shape. The uniform \(\mathrm{BB}(5,1,1)\) has likelihood \(1/216>L_F\). Since \(T<0\), \(\log L_N<F\) for sufficiently large sizes, so convergence to \(F\) forces a later increase. For the global fit, the reproducer excludes \(N\ge64\) and blocks \([13,15]\), \([16,19]\), \([20,25]\), \([26,39]\), \([40,63]\) by the ratio test. It excludes \(N=6,\ldots,12\) using the compact fixed-size chart below. Each linear fraction in \(r\) is bounded at an endpoint of its rational interval after bounding its numerator in \(p\). All exclusions are below \(1/216\). At size five the score equations are
\[
3/a+\sum_{j=1}^4(a+j)^{-1}=c(t),\quad
2\sum_{j=0}^3(b+j)^{-1}=c(t),\quad
t=a+b,\quad c(t)=3\sum_{j=0}^4(t+j)^{-1}.
\]
Eliminating \(a\) after substituting \(b=t-a\) and clearing denominators gives a degree-55 resultant with 30 distinct real root intervals. For each positive interval, invert the two decreasing shape scores at the endpoints of the range of \(c\); discard if the resulting sum interval is disjoint from the candidate \(t\) interval. One survives. Fixed-size existence supplies a stationary pair, so this counts the unique maximum. Relative shape brackets narrower than \(10^{-24}\) enclose \(a\approx0.7239389970526029\), \(b\approx0.6534559754437617\).

**Proved two separate finite peaks.** The underdispersed sample \((7,8,13)\) has \(S^2/D=392/11\), so the binomial branch begins at 36. Exact binomial comparisons give \(B_{36}<B_{37}<B_{38}>B_{39}\). The feasible \(\mathrm{BB}(13,52/21,73/83)\) exceeds \(B_{38}\), the global binomial maximum. Thus a finite-shape global winner lies before 36, with a separate strict local peak at 38. The sample was found by screening the old numerical map and then checking the inequalities exactly. A second example has counts \(2,3,4,7\) with multiplicities \(32,32,17,32\). Its binomial branch starts at 452 with positive next increment and peaks at 474. The witness \(\mathrm{BB}(7,2,1)\) beats that binomial maximum by more than \(0.22335\) in log likelihood. Both samples have finite attainment.

**Proved exact comparison for specified sizes.** At \(N\ge1\) set \(p=a/(a+b),\ r=1/(1+a+b)\). In the compact square \(0\le p,r\le1\) the continuous rational probability is
\[
P_N(x)=\binom Nx
\frac{p^{[x>0]}(1-p)^{[x<N]}(1-r)^{[0<x<N]}
\prod_{j=1}^{x-1}[p(1-r)+jr]\prod_{j=1}^{N-x-1}[(1-p)(1-r)+jr]}
{\prod_{j=1}^{N-1}[1+(j-1)r]}.
\]
All denominators are positive. The formula asserting some feasible pair attains height \(y\) and none exceeds \(y\) becomes polynomial after clearing denominators. Real closed field quantifier elimination defines a singleton algebraic height. Apply this to two sizes for an exact \(<,=,>\) predicate, or isolate the heights separately and use polynomial root identity. This covers finite/finite, finite/binomial and excluded finite-size face comparisons. With an interior count at \(N\ge2\), the \(p\) score has a unique root for each interior \(r\) by strict concavity. The resulting likelihood profile is analytic and semialgebraic, with distinct limits at the binomial and endpoint boundaries. Its stationary set is finite, since the derivative zero set is semialgebraic and has no interval. Isolating the stationary roots and comparing heights is another exact route.

Poisson at \(\mu>0\) has likelihood \(e^{-S}\mu^S/\prod_i x_i!\), a nonzero rational times a transcendental number. It cannot equal any finite \(L_N\). The NB score proves the positive finite NB optimum shape is transcendental, but does not prove its height is. **Conditional on Schanuel's conjecture**, that height is transcendental too. Write \(s(a)=\log(1+\mu/a)\), rational in \(a\) at the score root. An algebraic height would put \(\exp(s(a))\) and \(\exp(ha s(a))\) in \(\overline{\mathbb Q}(a)\), by the NB likelihood formula. Their exponents are rationally linearly independent since \(a\) is transcendental and \(s(a)\ne0\). Schanuel would force transcendence degree at least two, against degree one. This conditional result does not supply a fitter test. [Tie policy](2026-11-25-ties-math-report.md)

## Verification

The [Python reproducer](2026-11-25-bbinomfree-math-data/check.py) finished green on the workstation through one CPU slot. Exact checks use rational arithmetic, resultants and rational root isolation. Log checks sum \(2\sum_{j\ge0}[(y-1)/(y+1)]^{2j+1}/(2j+1)\) with a geometric remainder bound. An early direct-box trial on \((2,2,3,3,4,7,7)\) at cutoff 4096 and prefix 28 reached its 12,000-box allowance. The earlier closure report already proves finite attainment for that sample.

| Exact tail check | \(K\) | \(J\) | Rectangles checked |
|---|---:|---:|---:|
| \((0,1,2)\), uniform witness | 64 | 12 | 161 |
| \((1,1,5)\), uniform witness | 64 | 16 | 625 |
| \((1,1,4)\), uniform witness | 64 | 16 | 623 |
| \((1,2)\), binomial height \(27/128\) | 64 | 12 | 31 |

The fixed-size exclusions for \((1,1,5)\) used 1,087–1,647 boxes per size and its five intermediate blocks used 557–1,021. The underdispersed map screen compared 18 candidates with a binomial peak beyond the dispersion cutoff, of which two cleared all exact inequalities.

**Numerical.** At 80 digits, stationary solutions for \((1,1,5)\) at \(N=10^2,10^3,10^4,10^5,10^6\) gave \(N(\ell-F)\) from \(-0.0139869176\) to \(-0.0153948997\), approaching \(-0.0153950104\). The quartic witness paths were checked at \(N=10^3,10^4,10^5\), also at 80 digits. On the zero-coefficient sample the scaled path differences were about \(-1.768\cdot10^{-6},-1.766\cdot10^{-8},-1.766\cdot10^{-10}\). They do not establish the optimized sign.

**Numerical tie screen.** At 60 digits, sorted nonconstant multisets of lengths 3–5 from \(0,\ldots,4\), with \(M=3\) or 4 and an interior count, were checked at \(M,M+1,M+2\). There were 304 stationary solves with residual at most \(10^{-40}\) and zero failed starts. Nonpositive-dispersion sizes used the exact binomial face. Among 267 shape/shape pairs the smallest absolute log gap was about \(0.00225150\) for \((0,1,1,1,3)\) at 4 and 5. Among 74 shape/binomial pairs it was \(0.00175981\) for \((1,2,2,4)\) at 4 and 5. The screen does not count all roots or exclude exact ties in general.

```sh
export TMPDIR="$PWD/build/tmp" OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 MKL_NUM_THREADS=1
mkdir -p "$TMPDIR"
/tmp/umvl-cpu/run nice -n 10 python3 -B dev/research/evaluations/2026-11-25-bbinomfree-math-data/check.py
```

## What this changes for the fitter

1. Dispatch the all-zero box, size-one Bernoulli ray, endpoint mixture, constant positive point mass, and exact empirical-law saturation before the search. Return the complete attained set in the first two cases and the named excluded limit with its witness for endpoint mixture and positive point mass.
2. For \(D>0\), evaluate the positive-dispersion prefix and compare with the certified ordinary binomial optimum. Return `BinomialLimit` with the winning size and probability only after proving the face wins. Keep tied binomial sizes in face diagnostics. At a proved finite/face equality, return the finite fit when it dominates the other regions. The excluded face adds no finite maximizer.
3. For \(D\le0\), compare with the NB or Poisson witness. A positive sign gives a route to a finite witness, after which either strict-gap tail bound applies. For a negative sign, verify a signed collar and a strict exterior cover before comparing the remaining finite sizes. Return a certified `mlnbinom` or `mlpois` limit only when every feasible finite region loses.
4. Carry fixed-size upper heights, feasible lower witnesses and public-shape brackets separately. Use the algebraic comparison as an optional exact check. Return a certified singleton only with coefficient accuracy and global uniqueness. A verified global fit with unknown cardinality uses `unique = NA`. Emit `unique = FALSE` and the complete typed set only for proved multiplicity with completeness. Overlapping heights alone stay in diagnostics.
5. Meter scans, box evaluations and root refinements with the assigned work units and scale-aware margins. A successful, accuracy-checked search keeps its result uncertified if the certificate allowance expires. A capped improving prefix or failed inner solve is a search failure. A separated bound that refutes the selected fit requires a checked surviving result or the mathematically justified status. The current general all-free route reports `Unsupported` after all-zero dispatch and has no existing general search result to use as a fallback. [Current entry point](../../../univariateML-cpp/include/cppunivariate/distributions/bbinom/mle.hpp) · [Failure handling](../../validation/diagnostics.md#wrapper-failure-handling-rule-r6)

## Open

- Whether a positive-count sample with \(M\ge3\) has distinct **attained** globally maximizing sizes remains open. The algebraic predicate decides equality for specified sizes. A witness requires exact equal heights and a global cover. A finite-shape winner exactly tying another size's binomial face also remains unexhibited. For the finite NB face, an unconditional height exclusion or a certified equality example is missing.
- At \(v=\mu,\ F_3=\mu^3,\ C_4=0\), the explicit histogram above needs a higher-order profiled comparison, or a remote witness above Poisson with a strict-gap tail cutoff.
- Production cost and floating-point margins for the block and signed collar covers remain unmeasured, particularly for the hard extended map cases. General fixed-size uniqueness for asymmetric sizes at least four remains TASK-275. [Hard map cases](2026-11-03-bbinom-cert-report.md#measurements-against-the-map) · [Fixed-size curvature problem](2026-11-25-bbinom2-math-report.md#open)
