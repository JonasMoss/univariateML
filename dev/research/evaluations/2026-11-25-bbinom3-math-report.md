# TASK-275 — Stationary curvature of the fixed-size beta-binomial

**Proved by exact polynomial certificates.** For every size \(4\le m\le8\), the concentration profile has strictly negative curvature at every finite stationary point, for arbitrary histograms and positive shapes. With an interior count and positive dispersion sign, the fixed-size, two-free-shape MLE is unique. Together with the earlier results, this covers every identifiable fixed size through eight.

**Proved for every size \(m\ge2\).** The same stationary curvature inequality holds if \(a+b\le1/3\), if \(\min(a,b)\ge256m^2\), or in the small-shape region given below. An integer check gives global uniqueness at any size if there is at least one interior observation and each endpoint frequency is at least four times the total interior frequency.

**Numerical.** An 80-digit search tested 302,644 triples at sizes up to 1,000 and found only negative curvature among the 18,676 retained feasible vertices. The unrestricted all-size inequality is conjectured.

## Setup

The worktree base is \(\texttt{bb391a57}\). Use the notation of [TASK-274](2026-11-25-bbinom2-math-report.md). Let \(X\) have the normalized histogram on \(0,\ldots,m\), let \(Y=m-X\), and put \(t=a+b\), \(p=a/t\), \(q=1-p\), \(s=1/t\). For \(k\ge0\) define
\[
H_r(k)=\sum_{j=0}^{k-1}(r+j)^{-1},\qquad
J_r(k)=\sum_{j=0}^{k-1}(r+j)^{-2}.
\]
At stationarity \(E H_a(X)=E H_b(Y)=C=H_t(m)\). The profile \(M(t)\) maximizes over \(a\) at fixed \(t\), and
\[
M''(t)=D-\frac{UW}{U+W},\qquad
D=J_t(m),\quad U=E J_a(X),\quad W=E J_b(Y).
\]
The stationary histogram constraints are affine and \(UW/(U+W)\) is concave in the weights. It suffices to test the vertices with at most three cells. A support with fewer cells is included by allowing zero weights in a triple.

For samples with an interior count, [TASK-244](2026-11-25-bbinom-math-report.md) proves finite existence exactly when \(\Delta=mnQ-(m-1)S^2-mnS>0\), where \(n\) is sample size, \(S=\sum_i x_i\) and \(Q=\sum_i x_i^2\). TASK-274 rules out finite stationary points when \(\Delta\le0\). For \(\Delta>0\) it gives positive profile score at \(s=0\) and negative score at \(s_{\max}=nH/n_I\), with \(H=\sum_{j=1}^{m-1}1/j\) and \(n_I=\#\{0<X_i<m\}\). Changing from \(t\) to \(s\) multiplies the second derivative at a stationary point by \(t^4\). Thus strict negative curvature at every zero gives exactly one crossing and a unique maximum.

## Exact proof for sizes four through eight

**Proved by integer arithmetic.** For each triple \(i<j<k\), define polynomials
\[
P_r=\prod_{l=0}^{m-1}(r+l),\quad F_{r,h}=P_r H_r(h),\quad
G_{r,h}=P_r'F_{r,h}-P_r F_{r,h}'=P_r^2 J_r(h),
\]
where primes differentiate in \(r\). On this triple use \(A_h=F_{a,h}\), \(B_h=F_{b,m-h}\) and set
\[
\begin{aligned}
E_0&=(A_i-A_j)(B_k-B_j)-(A_k-A_j)(B_i-B_j),\\
c&=P_a F_{t,m}-A_j P_t,\qquad d=P_b F_{t,m}-B_j P_t,\\
w_i^*&=c(B_k-B_j)-d(A_k-A_j),\\
w_k^*&=d(A_i-A_j)-c(B_i-B_j),\qquad
w_j^*=E_0P_t-w_i^*-w_k^* .
\end{aligned}
\]
The successive slopes of \(H_b(m-h)\) against \(H_a(h)\) decrease strictly with \(h\). Consequently \(E_0>0\), since the magnitude of the secant slope on \([j,k]\) is larger than on \([i,j]\). The three weights are \(w_h=w_h^*/(E_0P_t)\), by solving the two stationary score equations and the unit-sum constraint. Each \(w_h^*\) must be nonnegative.

Put \(U^*=\sum_h w_h^*G_{a,h}\), \(W^*=\sum_h w_h^*G_{b,m-h}\), with both sums on the triple. Multiplication of \(UW-D(U+W)\) by the positive denominator \(E_0^2P_t^3P_a^2P_b^2\) gives
\[
N=P_t U^* W^* - G_{t,m}E_0(P_b^2U^*+P_a^2W^*).
\]
The verifier strips positive content and factors of \(N\) whose nonzero monomial coefficients are all positive. Their signs are strict on \(a,b>0\). It tests the remaining \(N\) and the three negative weight numerators for strict positivity.

The following coefficient rules give the exact cover and handle its edges. For any polynomial \(P(a,b)\) of total degree \(d\), use \(y=1/(1+t)\) to map the positive quadrant to the open unit square in \((p,y)\). Test
\[
y^d P\bigl(p(1-y)/y,(1-p)(1-y)/y\bigr).
\]
Its monomial with coefficient \(c_{ij}\) contributes
\(c_{ij}p^i(1-p)^j y^{d-i-j}(1-y)^{i+j}\). Elevate the degree in \(p\) to \(d\) by multiplying by \((p+(1-p))^{d-i-j}\). The coefficient at Bernstein index \((i+l,d-i-j)\) receives
\[
\frac{c_{ij}\binom{d-i-j}{l}}{\binom{d}{i+l}\binom{d}{d-i-j}},
\qquad 0\le l\le d-i-j.
\]
The verifier clears these binomial denominators with one positive factor. It bisects rectangles at midpoints, alternating coordinates. For coefficients in one coordinate, the midpoint recurrence replaces adjacent \(u,v\) by \((u+v)/2\). The leftmost entry at each level gives the left coefficients and the rightmost entries in reverse give the right coefficients. This follows by applying the Bernstein binomial identity on each half. The verifier uses integer sums and rescales all entries by the same power of two.

A sign is accepted if all rectangle coefficients are nonnegative and at least one is positive. For every edge inside the open square, at least one coefficient on that edge must be positive, and every corner inside the open square must have its coefficient positive. The Bernstein basis is strictly positive on the relative interior of the corresponding face, which gives strict positivity at every point in scope, including subdivision boundaries. A leaf is closed by one positive negative-weight numerator or by positive \(N\). The former excludes feasibility and the latter proves the curvature inequality.

[The exact verifier](2026-11-25-bbinom3-math-data/bernstein.py) obtained the following leaf counts. It visits one triple per reflection class and requires closure by depth 24. No leaf reached that limit without a sign certificate.

| Size | Support classes | Excluded leaves | Positive curvature numerator leaves | Unresolved |
|---:|---:|---:|---:|---:|
| 4 | 6 | 25 | 26 | 0 |
| 5 | 10 | 42 | 53 | 0 |
| 6 | 19 | 119 | 125 | 0 |
| 7 | 28 | 191 | 188 | 0 |
| 8 | 44 | 318 | 289 | 0 |

Every positive shape pair belongs to a covered leaf for each triple. The stationary vertex reduction then proves the strict inequality for every stationary histogram at each listed size.

## Small concentration and one small shape

**Proved.** The following histogram-independent check suffices at stationarity:
\[
f_m(a)+f_m(b)<f_m(t),\qquad f_m(r)=H_r(m)/J_r(m).
\tag{1}
\]
For \(1\le k\le m\), \(J_r(k)/H_r(k)\) is the weighted mean of the reciprocals \((r+j)^{-1}\) on \(j<k\), with weights proportional to those reciprocals. Appending a term decreases the mean, so \(J_r(k)\ge H_r(k)/f_m(r)\), also valid at \(k=0\). Stationarity yields \(1/U+1/W\le(f_m(a)+f_m(b))/C\). Under (1) the right side is less than \(f_m(t)/C=1/D\).

Write \(f_m(r)=r+g(r)\), where \(g(r)\) is the mean of \(j\) under weights proportional to \((r+j)^{-2}\). Then
\[
\frac{g(r)}r=
\frac{\sum_{j=1}^{m-1} rj/(r+j)^2}{1+\sum_{j=1}^{m-1}r^2/(r+j)^2}.
\]
For \(0<r\le1/3\), the numerator's logarithmic derivative is a weighted mean of \((j-r)/(r(j+r))\), which is at least \((1-r)/(r(1+r))\ge3/2\). The denominator's logarithmic derivative is at most \(2r\sum_{j=1}^{m-1}1/j^2<4r\le4/3\). Thus \(g(r)/r\) increases strictly there. For \(t\le1/3\), multiplication by \(a\) and \(b\) gives \(g(a)+g(b)<g(t)\), proving (1).

A second explicit region allows large \(t\). Take \(a\le b\) by reflection and suppose
\[
aH\le \frac{2b}{m^2(t+1)^3}.
\tag{2}
\]
Then \(g(a)<a^2 H\). To bound the increment of \(g\), let \(\pi_j(r)=(r+j)^{-2}/J_r(m)\). Differentiating the mean gives the covariance of \(j\) with \(-2/(r+j)\), or the sum over \(i<j\) of \(\pi_i\pi_j(j-i)(2/(r+i)-2/(r+j))\). Retain the pair \(0,1\) and use \(J_r(m)\le m/r^2\) to obtain
\[
g'(r)\ge\frac{2r}{m^2(r+1)^3}.
\]
For \(b\le r\le t\) the right side is at least \(2b/[m^2(t+1)^3]\). Integration and (2) give \(g(t)-g(b)\ge a^2 H>g(a)\), again proving (1).

The check (1) itself can cover more shapes than either explicit bound. As both shapes grow, \(g(r)\to(m-1)/2\), so (1) eventually fails even with both shapes equal. That limitation is specific to the marginal prefix bound.

## Large shapes

**Proved.** If both \(a,b\ge256m^2\), every finite stationary point has strictly negative profile curvature. This is uniform in \(p\in(0,1)\).

Use the per-observation reduced log likelihood
\[
\ell(p,s)=E\left[\sum_{j<X}\log(p+js)+\sum_{j<Y}\log(q+js)\right]
          -\sum_{j<m}\log(1+js).
\]
Let \(T=\sum_{j<m}j=m(m-1)/2\), \(T_2=\sum_{j<m}j^2\), \(R=E\sum_{j<X}j\), \(B=E\sum_{j<Y}j\), \(u=E[X]/m\). At a stationary pair put \(x=R/p^2\), \(z=B/q^2\), and \(h=s/\min(p,q)=1/\min(a,b)\). Define \(e=R/p+B/q-T\) and \(d=R-B-T(p^2-q^2)=2T(u-p)\). Direct subtraction of \(\ell_s(p,s)=0\) from \(\ell_s(p,0)=e\) gives
\[
|e|\le sm(x+z+T).
\]
Indeed the numerator differences divided by \(s\) are bounded by \(E\sum_{j<X}j^2/p^2\le mx\) and \(mz\), and the denominator difference divided by \(s\) is at most \(T_2\le mT\). Subtracting the mean scores gives \(m(u-p)/(pq)=s(V_a-V_b)\), with \(V_a=E\sum_{j<X}j/[p(p+js)]\le x\) and \(V_b\le z\) defined by reflection. Hence \(|d|\le(m-1)spq(x+z)\).

The linear identities \(x-T=(q e+d)/p\), \(z-T=(p e-d)/q\) bound both deviations by \(2hm(x+z+T)\). If \(hm\le1/8\), summing these bounds gives \(x+z+T\le6T\), and each deviation is at most \(12hmT\le6hm^3\).

The inequality \(E[X(X-1-(m-1)p)^2]\ge0\) gives
\[
\frac{E\sum_{j<X}j^2}{p^2}
\ge \left(\frac43(m-1)p+\frac13\right)x-\frac{(m-1)^2 E[X]}3 .
\]
Add the reflected bound, using \(E[X]+E[Y]=m\). With \(x=z=T\) the right side is \(T_2+m(m-1)/6\). Its decrease when \(x,z\) deviate from \(T\) is at most \(8hm^4\), using the sum of the two nonnegative coefficients and the deviation bound. This shows \(\ell_{ss}(p,0)\le-m(m-1)/6+8hm^4\).

For \(r>0\), \(0\le r^{-2}-(r+js)^{-2}\le2js/r^3\). The increase in \(\ell_{ss}\) from 0 to \(s\) is at most \(2hm^2(x+z)\le6hm^4\), since \(j^3\le m^2j\) and the denominator terms decrease. The mixed derivative has absolute value at most
\[
|\ell_{ps}(p,0)|+2hm(x+z)\le 18hm^3
\]
because \(\ell_{ps}(p,0)=-x+z\). The mean curvature satisfies \(-\ell_{pp}\ge m/(1+ms)^2\ge m/4\) for \(ms\le1\). Hence the profiled \(s\)-curvature is bounded by
\[
\ell_{ss}+\frac{\ell_{ps}^2}{-\ell_{pp}}
\le -\frac{m(m-1)}6+14hm^4+1296h^2m^5 < 0
\]
when \(h\le1/(256m^2)\). For the last sign use \(m\ge2\), so \(m(m-1)/6\ge m^2/12\) and \(14/256+1296/(2\cdot256^2)<1/12\). The \(t\)-profile has the same sign at the stationary point.

## An endpoint-count test for uniqueness

**Proved.** At any fixed size \(m\ge2\), the histogram condition is
\[
n_I>0,\qquad \min(n_0,n_m)\ge 4n_I .
\tag{3}
\]
For all \(p\in(0,1)\), (3) implies \(\ell_s(p,s)>0\) on \(0\le s\le3\).

Put \(K(s)=\sum_{j=1}^{m-1} j/(1+js)^2\). For \(X=0\), the dispersion score contribution is strictly greater than \(pK(s)\), since its summands for \(j\ge1\) are \(pj/[(q+js)(1+js)]\). Reflection gives the strict lower bound \(qK(s)\) at \(X=m\). For \(0<k<m\) replace each positive numerator score term by the lower bound \(j/(1+js)\) and write \(v(j)=j/(1+js)\). The interior score is bounded below by \(-L\), where
\[
L=\sum_{j=0}^{k-1}\{v(m-k+j)-v(j)\}.
\]
Expand each difference as a sum of increments \(v(r+1)-v(r)\) for \(j\le r\le m-k+j-1\). For each \(r\), at most \(r+1\) increments occur. Therefore
\[
L\le \sum_{r=0}^{m-2}\frac{r+1}{(1+rs)(1+(r+1)s)}
\le (1+s)K(s).
\]
The second inequality uses \((1+(r+1)s)/(1+rs)\le1+s\). The total score is therefore strictly greater than \(K(s)[n_0p+n_mq-(1+s)n_I]\), which is nonnegative under (3) for \(0\le s\le3\).

Every stationary point lies at \(s>3\), hence \(a+b<1/3\), where the small-concentration proof gives strict negative curvature. The positive score at \(s=3\) and the negative score at \(s_{\max}\) give a root bracket and prove uniqueness. In particular the global maximum is finite and strictly beats the binomial face.

## Validation

All final runs passed on the workstation, in one slot each.

- [Bernstein verifier](2026-11-25-bbinom3-math-data/bernstein.py) closed all 107 reflection classes at sizes four through eight. The cover uses exact polynomial and integer arithmetic throughout.
- [Independent replay](2026-11-25-bbinom3-math-data/replay.py) checked 480 exact rational comparisons between the polynomial formulas, directly evaluated vertex formulas, and the Bernstein basis before and after bisection, at sizes four and five. It also checked 2,656 exact endpoint-count scores at sizes \(4,5,9,20,50\), including equality in (3) at either endpoint, and 946 exact feasible large-shape vertices at sizes \(4,5,8,12\). This tests the formulas and arithmetic on finite grids.
- [Vertex search](2026-11-25-bbinom3-math-data/check.py) checked the strict curvature sign using exact fractions on a grid at \(m=4,5,6,8,12\), including 1,202 feasible cases with \(t\le1/3\), 408 satisfying (2), and 214 at the deliberately large concentrations \(t=100m^2/\min(p,q)^4\). These counts may overlap.
- The same search ran at 80 decimal digits on sizes \(4,5,6,8,12,20,50,100,300,1000\). The shape grid was the Cartesian square of \(10^j\) for \(j=-14,-8,-4,-2,-1,0,1,2,4,8,14\), augmented by 24 log-scale shape pairs per size with seed 275. It enumerated every triple through size 12 and used reproducibly sampled, endpoint-enriched triples above size 12. Of 302,644 triples, 18,676 had all computed weights above \(10^{-55}\). The smallest observed \(UW/[D(U+W)]-1\) was \(1.49999999999994333\times10^{-42}\) at \(m=4\), cells \((0,3,4)\), \(a=10^{14}\), \(b=10^{-14}\). Exact fractions verified all three weights and the positive gap at that point.

From the worktree root, the three Python commands are:

~~~sh
mkdir -p build/tmp
export TMPDIR="$PWD/build/tmp" PYTHONDONTWRITEBYTECODE=1
data=dev/research/evaluations/2026-11-25-bbinom3-math-data
/tmp/umvl-cpu/run nice -n 10 python3 "$data/check.py"
/tmp/umvl-cpu/run nice -n 10 python3 "$data/bernstein.py" 4 5 6 7 8
/tmp/umvl-cpu/run nice -n 10 python3 "$data/replay.py"
~~~

## What this changes for the fitter

- After the special-size and endpoint-only decisions, the exact sign of \(\Delta\) gives the previously proved \(\texttt{BinomialLimit}\) route for \(\Delta\le0\) and finite attainment for \(\Delta>0\). For fixed \(2\le m\le8\) with two free shapes, the cheap uniqueness check is the integer size comparison. For \(\Delta>0\) bracket the unique root on \([0,s_{\max}]\). The Bernstein computation takes place offline.
- At any fixed size, (3) is a second cheap uniqueness check using three counts. With \(n_I>0\), the overflow-safe integer comparison is \(\lfloor\min(n_0,n_m)/4\rfloor\ge n_I\). It supplies the tighter bracket \([3,s_{\max}]\) and proves that the unique root beats the face.
- For a larger asymmetric fit outside (3), an interval in a score-root cover can use an analytic region in this report whenever its entire shape enclosure lies in that region. For example, \(s_l\ge3\) suffices for the small-concentration check. The large-shape check can use the lower bounds \(p_l/s_h\) and \((1-p_h)/s_h\) from a profiled mean bracket. Use the existing score exclusions and curvature bounds for the rest of the cover.
- Certified finite \(\texttt{Ok}\) still requires a root enclosure propagated to both public shapes, with the requested relative accuracy and numerical margins. On a larger case where the certificate fails to close, retain the successful search result as uncertified unless a certificate refutes it. Free-size comparisons require their own certificate.

## Open

For asymmetric histograms at \(m\ge9\), the universal strict curvature sign remains conjectured outside the proved shape and sample regions. The exact rectangle cover establishes each enumerated size separately. An all-size proof still needs a bound uniform in the vertex indices and the size, including mixed-scale limits with one shape bounded and the other diverging. The marginal prefix inequality (1) is too weak to give that bound on its own.

The numerical search produced no rational stationary histogram with positive profile curvature. If a feasible strictly positive-curvature vertex is found at rational shapes, clearing the rational weight denominators yields the exact histogram. TASK-274's endpoint signs then give a strict local maximum on each side of the stationary minimum. Separating their heights, and excluding any other stationary maxima before assigning the global winner, would require a further comparison.
