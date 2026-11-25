# TASK-274 — Uniqueness of the fixed-size beta-binomial fit

**Proved.** For size \(m=3\), every histogram in the finite-existence region has a unique maximizer, and the dispersion score has exactly one crossing with strictly negative profile curvature. The same conclusions hold at every \(m\ge2\) for reflection-symmetric histograms. For any histogram with an interior count and \(\Delta\le0\), the concentration profile increases strictly toward the binomial face, ruling out finite stationary points.

**Proved per input.** A rational score and curvature certificate can establish single crossing for asymmetric histograms at arbitrary fixed size. A polynomial elimination certificate gives a second exact route. **Numerical.** The search for positive stationary curvature found no counterexample. Universal uniqueness for asymmetric histograms at \(m\ge4\) remains open.

The worktree base is b51329d8. Write \(n_k\) for the frequency of count \(k\), \(n=\sum_k n_k\), \(X\) for a uniformly drawn observation, \(Y=m-X\), \(u=E[X]/m\) and \(v=1-u\). With \(S=\sum_k k n_k\) and \(Q=\sum_k k^2 n_k\), the exact dispersion sign is \(\Delta=mnQ-(m-1)S^2-mnS\). We use [TASK-244](2026-11-25-bbinom-math-report.md), which resolves the earlier [existence conjecture](2026-10-31-bbinom-existence-report.md). With an interior count, a finite MLE exists exactly when \(\Delta>0\). The beta mean is \(p=a/(a+b)\), concentration is \(t=a+b\), reciprocal concentration is \(s=1/t\), and intraclass correlation is \(\rho=s/(1+s)\).

## Stationary curvature and the reduction to three cells

Define the prefix sums \(H_r(k)=\sum_{j<k}(r+j)^{-1}\) and \(J_r(k)=\sum_{j<k}(r+j)^{-2}\), with sums starting at zero. Put
\[
A=E[H_a(X)],\quad B=E[H_b(Y)],\quad C=H_t(m),\quad
U=E[J_a(X)],\quad W=E[J_b(Y)],\quad D=J_t(m).
\]
The two shape scores per observation are \(A-C\) and \(B-C\). At fixed \(t\), the reduced log likelihood in \(a\in(0,t)\) has derivative \(A-B\) and second derivative \(-U-W<0\). An interior count gives opposite infinite endpoint derivatives. Thus the unique maximizing \(a(t)\) is analytic by the implicit function theorem. For the concentration profile \(M(t)\), differentiation of \(A=B\) gives
\[
a'(t)=\frac{W}{U+W},\qquad b'(t)=\frac{U}{U+W},\qquad
M'(t)=A-C,\qquad M''(t)=D-\frac{UW}{U+W}.
\]
For \(\Delta>0\), the prior face score at \(s=0\) is positive, and the large-\(s\) score is negative by the bound below. Thus \(M'\) is positive near \(t=0\) and negative near \(t=\infty\). If \(UW>D(U+W)\) at every zero, each zero crosses down and there is exactly one. Strict positive curvature at a zero would instead give a strict local minimum and force a strict local maximum on each side. The profile is analytic and nonconstant, so stationary points are isolated. Strict mean maximization lifts both profile maxima to strict two-shape local maxima.

At fixed positive \(a,b\), the real stationary histograms form the polytope
\[
w_k\ge0,\quad \sum_k w_k=1,\quad
\sum_k w_kH_a(k)=C,\quad \sum_k w_kH_b(m-k)=C.
\]
A vertex has at most three positive cells, since more would permit a nonzero perturbation on the support preserving all three equalities. Every three distinct cells give an invertible system. The successive slopes of \(H_b(m-k)\) against \(H_a(k)\) are \(-(a+k)/(b+m-1-k)\), strictly decreasing in \(k\), so no three cells are collinear. A vertex of smaller support can be represented by including zero weights. Since
\[
\frac{UW}{U+W}=\min_{\lambda\in[0,1]}\{\lambda^2 U+(1-\lambda)^2 W\},
\]
this quantity is concave in the histogram. A strict lower bound by \(D\) at all vertices therefore holds on the entire polytope.

Conversely, a feasible vertex with strictly positive curvature at rational \(a,b\) gives rational weights by the three-cell linear solve. Clearing denominators yields an exact integer histogram. It must have an interior count and \(\Delta>0\) by the next result and the endpoint check. For endpoint-only data, each endpoint probability strictly increases with \(s\) at fixed \(p\), because each factor \((p+js)/(1+js)\) or \((1-p+js)/(1+js)\) for \(j\ge1\) increases strictly. Thus endpoint-only data have no finite stationary point for \(m\ge2\).

## No finite stationary point for nonpositive dispersion

**Proved.** If \(m\ge2,\ 0<u<1,\ \Delta\le0\), the reduced likelihood increases along every line \((a,b)+d(u,v)\) as \(d\) increases within the positive quadrant. Its log derivative per observation is \(uA+vB-C\).

Use the kernel \(K_z(r)=(1-e^{-zr})/[z(1-e^{-r})]\) from TASK-244 and define \(R_a(z)=\int_0^\infty e^{-ar}K_z(r)\,dr\) for \(z\ge1\). At integer \(k\ge1\) this is \(H_a(k)/k\). The kernel is decreasing and convex in \(z\), so Jensen under \(X\)-size-biased weights and \(\operatorname{Var}(X)\le muv\) give
\[
\frac{A}{mu}\ge R_a(E[X^2]/E[X])\ge R_a(1+(m-1)u).
\]
The pointwise scaling inequality in TASK-244 gives \(K_{1+(m-1)u}(r/u)>K_m(r)\) for \(r>0\). Changing variable in \(R\) gives \(u R_a(1+(m-1)u)>R_{a/u}(m)\). Reflection gives the corresponding bound for \(B\). Convexity of \(c\mapsto R_c(m)\), an average of reciprocals, then yields
\[
uA+vB>m\{uR_{a/u}(m)+vR_{b/v}(m)\}\ge mR_{a+b}(m)=C.
\]
On the mean profile \(A=B\), so \(M'(t)>0\) at every finite \(t\), including at exact equidispersion. This uses the derivative kernel directly and strengthens the prior comparison with the binomial likelihood.

## Reflection symmetry at any fixed size

**Proved.** When \(n_k=n_{m-k}\), strict mean concavity gives \(a=b=t/2\). Define \(d=|2X-m|\) and \(z=e^{-r}\). Integrating the reciprocal terms gives
\[
M'(t)=\int_0^\infty e^{-tr}K(r)\,dr,\qquad
K(r)=2E\sum_{j<X}z^{2j}-\sum_{j<m}z^j.
\]
Multiply the kernel by \(1-z^2\) and use \(E[z^{2X}]=E[(z^{m-d}+z^{m+d})/2]\). This gives
\[
(1-z^2)K(r)=(1-z)(1-z^m)-E[z^{m-d}(1-z^d)^2].
\]
Hence \(K(r)=(1-z)P(z)/(1+z)\), where
\[
P(z)=\sum_{j=0}^{m-1}z^j-E\!\left[z^{m-d}\left(\sum_{j=0}^{d-1}z^j\right)^2\right].
\]
For \(j<m\), its coefficients are \(1-E[(j-m+d+1)_+]\), from the rising half of each squared geometric polynomial. They are nonincreasing there, and all subsequent coefficients are nonpositive. Also \(P(0)=\Pr(0<X<m)>0\) and \(P(1)=m-E[d^2]<0\) for \(\Delta>0\), since \(E[d^2]=4\operatorname{Var}(X)\). Dividing by \(z^j\) at the first negative coefficient leaves a strictly decreasing function on \(z>0\). Thus \(P\) has exactly one zero in \((0,1)\).

The kernel is negative below some \(r_0>0\) and positive above it. At any zero \(t_0\) of \(M'\), differentiation under the integral and subtraction of \(r_0M'(t_0)\) give
\[
M''(t_0)=-\int_0^\infty e^{-t_0r}(r-r_0)K(r)\,dr<0.
\]
The integrals converge because \(K\) is a finite sum of bounded exponentials. Existence supplies the zero and the curvature proves uniqueness. Checking reflection symmetry takes one pass over the histogram.

## Universal uniqueness at size three

**Proved by exact polynomial positivity.** The vertex reduction leaves only supports \(012\) and \(013\) up to reflection. For a support \(i<j<k\), put \(A_h=H_a(h)\), \(B_h=H_b(3-h)\), \(c=A_i-A_j,\ e=A_k-A_j,\ f=B_i-B_j,\ g=B_k-B_j\). Solve
\[
w_i=\frac{(C-A_j)g-(C-B_j)e}{cg-ef},\qquad
w_k=\frac{c(C-B_j)-f(C-A_j)}{cg-ef},\qquad
w_j=1-w_i-w_k.
\]
Use these weights in \(U=\sum w_hJ_a(h)\), \(W=\sum w_hJ_b(3-h)\). Let
\[
T=(a+1)(b+1)(b+2)t^3(t+1)^3(t+2)^2,\qquad h=3ab+2a+4b+2.
\]
Multiplying \(UW-D(U+W)\) by \(b^3T\) for \(012\) or \((a+2)h^2T\) for \(013\) gives polynomials \(N_{012}\) and \(N_{013}\). Both multipliers are positive.

For \(012\), \(w_1=-aF/[bt(t+1)(t+2)]\), where
\[
F=3a^2b+4a^2+5ab+6a-3b^3-3b^2+2b+2.
\]
This is increasing in \(a\ge0\). If \(b\le1/2\), then \(F(0,b)=(b+1)(2-3b^2)>0\). If \(b>1/2\) and \(b\le a+1/2\), use \(F(b-1/2,b)>0\) from \(F(a,a+1/2)=3a^2+21a/4+15/8\). Thus feasibility requires \(b>a+1/2\).

For \(013\), \(w_0=-b(b+2)G/[t(t+1)(t+2)h]\), where
\[
G=3a^3+3a^2-3ab^2-5ab-2a-4b^2-6b-2.
\]
At \(a=b+1+z\) this is \(2(b+1)+(6b^2+19b+13)z+(9b+12)z^2+3z^3>0\) for \(z\ge0\). Thus feasibility requires \(a<b+1\).

The following exact coefficient check certifies positivity over the feasible regions. The first row clears rational denominators and divides by positive integer content. In the second row the multiplier clears the substitution denominator. The weights and multipliers above define the polynomials without root calculations. The exact arithmetic is reproduced by [size_three.py](2026-11-25-bbinom2-math-data/size_three.py).

| Support | Polynomial after substitution | Nonzero coefficients | Smallest nonzero coefficient | Constant |
|---|---|---:|---:|---:|
| \(012\) | Primitive integer form of \(N_{012}(a,a+1/2+z)\) | 45 | 2304 | 6855 |
| \(013\) | \((1+z)^8N_{013}((b+1)z/(1+z),b)\) | 93 | 135 | 192 |

To obtain the rows, replace each \(a^i b^j\) in \(N_{012}\) by \(\sum_{l=0}^{j}\binom jl a^i(a+1/2)^{j-l}z^l\). In \(N_{013}\), replace it by \(b^j(b+1)^i z^i\sum_{l=0}^{8-i}\binom{8-i}{l}z^l\). The coefficients in the table are strictly positive wherever nonzero, including a positive constant in each row. These substitutions exhaust the required positive shape regions, with \(z>0\) each time. Reflection covers the other two supports. Concavity in the histogram now gives \(M''<0\) at every stationary point at \(m=3\), and the endpoint signs give uniqueness and single crossing.

## Per-input certificates for general histograms

**Proved.** Use total log likelihood and put \(A_j=\sum_{k>j}n_k,\ B_j=\sum_{k<m-j}n_k\). Then the reduced log likelihood is
\[
\ell(p,s)=\sum_{j=0}^{m-1}
  \{A_j\log(p+js)+B_j\log(1-p+js)-n\log(1+js)\}.
\]
Let \(g(s)=\ell_s(p(s),s)\) along the profiled mean and \(n_I=\sum_{k=1}^{m-1}n_k\). We have \(g(0)>0\) in the existence region. For \(s>0\), use \(js/(r+js)=1-r/(r+js)\) and \(\sum_{j=1}^{m-1}(A_j+B_j-n)=-n_I\). The numerator terms remaining after the constant subtract nonnegative quantities and \((1+js)^{-1}<(js)^{-1}\). Thus
\[
s\ell_s(p,s)<-n_I+nH/s,\qquad H=\sum_{j=1}^{m-1}1/j.
\]
This makes \(g<0\) at and above \(s_{\max}=nH/n_I\).

Cover \([0,s_{\max}]\) by closed intervals \([l,h]\). Bracket the profiled mean throughout each interval using the zeros of two decreasing functions. The lower bound on \(\ell_p\) uses \(s=h\) in the positive \(A_j\) terms and \(s=l\) in the negative \(B_j\) terms. The upper bound swaps them. At the resulting rectangle, bound the rational score and curvature terms by evaluating their denominators at the endpoints. Accept an interval if its enclosure of \(g\) excludes zero or if its bounds give
\[
\sup g'\le E+B^2/L<0,\quad
E\ge\sup\ell_{ss},\quad B\ge\sup|\ell_{ps}|,\quad 0<L\le\inf(-\ell_{pp}).
\]
The formula follows from \(g'=\ell_{ss}+\ell_{ps}^2/(-\ell_{pp})\). Subdivide any unresolved interval. When the cover closes, every root has negative derivative and the endpoint signs establish exactly one. The [rational prototype](2026-11-25-bbinom2-math-data/check.py) uses exact fractions for the interval decisions.

**Proved algebraic alternative.** Clear denominators of \(\ell_p,\ell_s\) to form polynomials \(f,g_*\), and divide both by their polynomial gcd before computing the resultant in \(p\). The removed common factor cannot vanish on the real profiled mean. If it did, \(\partial_p f\ne0\) there would imply the common factor defines the mean branch locally. Then \(g_*=0\) on an open segment and analyticity would make the score identically zero, in contradiction with the boundary signs. The quotients have a nonzero resultant, and every stationary dispersion is among its positive real roots. A single positive resultant root certifies uniqueness by existence and the unique mean. If more roots occur, real algebraic back-substitution into the two scores can remove inadmissible pairs. The likelihood, before taking logs, is rational in the shapes with integer powers, permitting exact comparisons and tie decisions at the remaining algebraic pairs. The [resultant prototype](2026-11-25-bbinom2-math-data/algebra.py) implements the single-positive-root test.

## Literature and free size

Griffiths is the original numerical MLE and household application reference. Tripathi, Gupta and Gurland describe Griffiths’s nonlinear solve and compare ML with moment and minimum chi-square estimators. Lee and Sabavala give the fixed-size shape score equations in Eq. (4) while developing Bayesian estimation in the mean and intraclass correlation. [Griffiths (1973)](https://doi.org/10.2307/2529131), [Tripathi, Gupta and Gurland (1994), §§1–2, 5–6](https://www.ism.ac.jp/editsec/aism/pdf/046_2_0317.pdf), [Lee and Sabavala (1987)](https://doi.org/10.1080/07350015.1987.10509600).

Levin and Reeds prove compound-multinomial unimodality in concentration for specified mean proportions by a variation-diminishing argument. Leeds and Gelfand distinguish this from the case with unknown proportions. At a joint stationary point the fixed-\(p\) curvature is \(D-p^2U-(1-p)^2W\). Profiling increases it by \((U+W)(p-W/(U+W))^2\), so negativity of the fixed-\(p\) curvature alone leaves the profiled sign open. The reflection proof is a self-contained specialization where the profiled mean is fixed. [Levin and Reeds (1977)](https://doi.org/10.1214/aos/1176343741), [Leeds and Gelfand (1989), §3](https://www.academia.edu/129677796/Estimation_for_dirichlet_mixed_models).

The profiled mean itself can be nonmonotone. At \(m=5\), counts \((2,1,0,3,0,2)\) give both limits of \(p(t)\) equal to \(1/2\) by the leading terms of the mean score. As \(t\to0\) the leading \(j=0\) terms use the proportions with \(X>0\) and \(Y>0\), which are equal here. As \(t\to\infty\) the profile tends to the empirical binomial mean. At \(t=2,\ p=1/2\) the total mean score in \(a\) is \(-1/12\), so the mean profile is nonconstant.

Fixed-size uniqueness certifies one integer size's profile. The integer ordering, endpoint and binomial faces at other sizes, and joint negative-binomial/Poisson tail require their own comparisons. The likelihood nonunimodality example in Gupta, Nguyen and Wang concerns the free-size question. [Free-size map](2026-11-02-bbinom-map-report.md), [joint tail report](2026-11-03-bbinom-cert-report.md), [Gupta, Nguyen and Wang (1999)](https://doi.org/10.2307/3316115).

## Validation

The final Python runs passed on the workstation, each through one CPU slot.

- The exact size-three polynomial check reproduced the 45 and 93 positive coefficients, minima and constants.
- The rational checker certified the five asymmetric examples below. It checked symmetric coefficient signs exactly on 4,217 positive-\(\Delta\) histograms with \(2\le m\le12\) and frequencies 0, 1 or 2 per independent reflection pair or center. Every 23rd eligible table passed the direct kernel identity at \(z=10^{-5},0.2,0.97,0.999999\) with 80 decimal digits.
- The resultant checker found one positive root for each of \((2,1,0,1)\), \((5,0,2,0,1)\), and \((1,0,1,4)\).
- The monotonicity check made 23,600 exact comparisons on every histogram with an interior count and \(\Delta\le0\) for \(2\le m\le6,\ 1\le n\le6\). There were 944 tables, including 47 at equality, with each shape in \(\{10^{-6},1/10,1,10,10^6\}\).
- The [stationary search](2026-11-25-bbinom2-math-data/search.py) drew 500 log-uniform shape pairs per \(m\in\{3,4,5,10,20,50,100,300,1000\}\) on \([10^{-3},10^4]^2\) with seed 274. It drew 300 triples per pair, enriching for endpoints and discarding repeated cells, singular solves and weights at most \(10^{-9}\). Among 195,649 feasible binary64 trials the smallest \(UW/[(U+W)D]\) was at \(m=10\), cells \((8,9,10)\), shapes about \((9552.2041664,0.00272037735)\). Replaying the binary64 shapes at 80 decimal digits gave ratio minus one \(1.3369914517752352\times10^{-10}>0\). The finite search supplies numerical evidence.

| Histogram | \(\Delta\) | Intervals visited | Score exclusions | Negative derivative |
|---|---:|---:|---:|---:|
| \((2,1,0,1)\) | 40 | 89 | 29 | 16 |
| \((1,1,0,0,2)\) | 141 | 129 | 44 | 21 |
| \((2,0,1,0,0,0,1)\) | 448 | 245 | 78 | 45 |
| \((5,0,0,1,0,2,0,0,0,0,1)\) | 7479 | 409 | 126 | 79 |
| \((8000,12000,6000,1001)\) | 216000 | 29 | 13 | 2 |

From the worktree root, run the scripts sequentially.

~~~sh
mkdir -p build/tmp
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp"
export XDG_RUNTIME_DIR="$PWD/build/tmp" PYTHONDONTWRITEBYTECODE=1
for script in check.py algebra.py underdispersion.py search.py size_three.py; do
    /tmp/umvl-cpu/run nice -n 10 python3 \
        "dev/research/evaluations/2026-11-25-bbinom2-math-data/$script" || exit
done
~~~

## What this changes for the fitter

- For \(m=3\) or a reflection-symmetric histogram at fixed \(m\), first apply the existing endpoint and exact dispersion decisions. With an interior count and \(\Delta>0\), bracket the unique dispersion root between the binomial face and \(s_{\max}\). At symmetry the mean is exactly \(1/2\). Once the root is enclosed, the proofs replace global likelihood comparisons. Existence and uniqueness establish that the root beats the face even when a numerical likelihood difference cannot be resolved.
- For other histograms with \(\Delta>0\), a completed score and curvature cover can establish the same single crossing. The existing tangent and chord certificate remains another acceptance route. A root bracket \([s_l,s_h]\) and a uniform mean bracket \([p_l,p_h]\) on it give \([p_l/s_h,p_h/s_l]\) for alpha and \([(1-p_h)/s_h,(1-p_l)/s_l]\) for beta when \(s_l>0\). A certified **Ok** still requires the requested relative accuracy in both public coefficients and the package's numerical margins.
- Use the existing certificate budget for interval evaluations. If the bounds fail to close and the existing search succeeds, retain its result marked uncertified unless a certificate proves it wrong. The exact nonpositive sign continues to give **BinomialLimit** after the endpoint routes. The fixed-size criteria do not order different integer sizes.

## Open

For asymmetric histograms at \(m\ge4\) in the existence region, neither universal uniqueness nor failure of single crossing has been established. A sufficient next theorem is \(UW>D(U+W)\) at every feasible three-cell stationary vertex for all positive \(a,b\). Alternatively, a feasible rational stationary histogram with \(UW<D(U+W)\) would give an exact example with two strict local maxima. Equality in the curvature test would require higher-order sign analysis, and two strict local maxima would leave equality of their heights for a separate comparison.
