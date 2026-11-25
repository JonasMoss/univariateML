# TASK-242 Skellam profile modes and interior certification

The joint mean-profiled likelihood can have two **interior** local maxima. The exact sample with 1,899 zeros, one minus one and 100 twenties gives three score reversals. A strict interval cover can still certify a unique global winner on an individual input. For the snapshot sample `wide`, elementary bounds isolate the winner at rates near $9.37\times10^{10}$ without paired-count enumeration. The stationary face for $(0,3,3)$ is the unique global maximum at $(2,0)$.

This is a math-only result from worktree revision `eaea17a1`. The prior reports supply attainment, the exact profiling reduction, the Poisson-mode tail and the existing curvature ceiling. The current search uses a likelihood-gap cover and a separate local polish. The gap alone leaves open a second maximum, and the polish cannot run at the paired-count mode in `wide`. [Original profile reduction](2026-10-29-skellam-report.md) · [Existence and uniqueness question](2026-10-31-open-existence-report.md#5-skellam-joint-uniqueness) · [Gap cover](2026-11-02-skellam-cert-report.md) · [Label fixes](2026-11-24-label-fixes-report.md#1-skellam)

## 1. Exact counterexamples to unimodality

Write $d=\bar x\ge0$ after reflection if needed, $k_i=|x_i|$, $t=\mu_2$, $u=t(t+d)$ and $z=2\sqrt u$. Along the exact profile $(d+t,t)$, the rate sum is $H=\sqrt{z^2+d^2}=2t+d$. Use $\ell$ for the mean log likelihood and $D=z\,d/dz$, the derivative in $\log z$. The prior reduction gives
$$
S=D\ell=\overline{h_k}-H,\qquad
h_k=k+r_k,\qquad r_k=zI_{k+1}(z)/I_k(z).
$$
The three coordinates $t,z,\log z$ are strictly increasing in one another in the interior, so their scores have the same sign.

**Proved bounds.** The Bessel power series gives $I'_k=(k/z)I_k+I_{k+1}$ and $I_k-I_{k+2}=2(k+1)I_{k+1}/z$ by coefficient comparison. Hence
$$
r_k=\frac{z^2}{2(k+1)+r_{k+1}}.
$$
Truncating after $m$ fractions with terminal value zero yields a lower bound for even $m$, an upper bound for odd $m$. Each map is decreasing and the true terminal ratio is strictly positive. The two adjacent truncations converge at any fixed finite $z$. In fact their difference is bounded by
$$
\frac{z^2}{2(k+m+1)}
\prod_{j=1}^{m}\frac{z^2}{4(k+j)^2},
$$
using the terminal upper bound and the Lipschitz constant of each map on the nonnegative half-line.

**Proved one-sided witness.** For 19 zeros and one 20 the inward score per observation is $-1/21$. At $t=1/2$ the two-level rational bound gives $S\ge271/6787>0$. At $t=32$ the 101-level upper bound is below $-0.3570$. Thus the profile decreases at the face, then rises and has an interior local maximum. The earlier numerical demonstration that the face loses also has a rational witness. At $t=1/2$ the likelihood ratio to the face exceeds
$$
\left(\frac{3/2}{68/25}\right)^{20}\left(\frac{121}{64}\right)^{19}>1.
$$
Here the paired-count series for $k=0$ is at least $1+3/4+(3/4)^2/4=121/64$, the series for $k=20$ is at least one, and $e<68/25$ follows by summing terms through $1/5!$ and bounding the remaining terms by a geometric series. The gain per observation there is **numerically** $0.0184644020692081$ at 60 decimals.

**Proved two-interior-mode witness.** Replicate that sample 100 times and change one zero to $-1$. Now $d=1999/2000$, the face has zero likelihood and $S(0+)=\overline{k}-d=1/1000$. At the following rational $t$ values the continued fractions use rational arithmetic throughout.

| $t$ | Depth | Exact comparison for $S$ |
| ---: | ---: | ---: |
| $1/20$ | 3 | $S < -0.001600$ |
| $1/2$ | 2 | $S > 0.04046$ |
| $32$ | 101 | $S < -0.3570$ |

For each positive-to-negative transition, the likelihood attains a maximum strictly inside the corresponding closed interval, with the first left endpoint chosen sufficiently close to zero. The profile is analytic and nonconstant in the interior, so these are strict local maxima in disjoint intervals. Strict maximization over the rate ratio at each $z$ also makes them strict local maxima in the two-rate space. This refutes single crossing even when both Poisson faces are impossible.

**Numerical search.** The script additionally scans 1,820 weighted triples on $\{0,\ldots,12\}$ at 60 decimals. It uses the four multiplicity vectors $(8,1,1)$, $(1,8,1)$, $(1,1,8)$ and $(1,1,1)$ and 61 $z$ values $e^{j/4}$ for $-32\le j\le28$. No grid trace has two positive-to-negative crossings. The constructive perturbation above gives the exact witness outside this grid of samples.

## 2. An interior certificate

### Moderate rates and the face neighborhood

**Proved bounds.** Define
$$
F_k(u)=\sum_{j\ge0}\frac{u^j}{j!(k+1)_j},\qquad
A_k(u)=\frac{F'_k(u)}{F_k(u)}
       =\frac{2}{2(k+1)+r_{k+1}},\qquad
B_k=A_k(A_{k+1}-A_k).
$$
Here $(k+1)_j$ is the rising factorial. Termwise differentiation first gives $F'_k=F_{k+1}/(k+1)$ and then $(\log F_k)''=B_k$. At zero $A_k(0)=1/(k+1)$ and $B_k(0)=-1/[(k+1)^2(k+2)]$. The conditional-count variance bound in the earlier cover gives $B_k\le0$ for $u>0$, so $A_k$ is decreasing. In a finite $t$ cell evaluate lower and upper continued-fraction bounds at the appropriate endpoints to enclose $A_k$. If $A_k\in[L_k,U_k]$, then
$$
B_k\in [L_k,U_k]\,[L_{k+1}-U_k,U_{k+1}-L_k]\ \cap\ (-\infty,0].
$$
The product is ordinary interval multiplication.

Put $p=\overline{(x)_+}$, $m=\overline{(-x)_+}$. The exact $t$ score $g$ and curvature $g'$ follow by differentiating the series representation of each mass.
$$
g=-2+\frac{p}{d+t}+\frac{m}{t}+(2t+d)\overline{A_k},\qquad
g'=-\frac{p}{(d+t)^2}-\frac{m}{t^2}
      +2\overline{A_k}+(2t+d)^2\overline{B_k}.
$$
Terms with zero numerator are identically zero, including at a feasible face. Substitution of the interval bounds gives two-sided computable enclosures. The earlier global ceiling is $M=2\overline{1/(k+1)}$. Near a feasible positive-mean face the cancellation-free form
$$
g(t)=g_0-\frac{t}{d+t}
     +\overline{2t A_k-d u A_k(0)A_k A_{k+1}}
$$
follows by subtracting $A_k(0)$ using the continued fraction. This separates evaluation of the potentially tiny rational $g_0$ from the increments. For a finite likelihood value, the positive series has a simple tail bound. With last retained term $a_j$ and $\rho=u/[(j+1)(j+k+1)]<1$, the missing sum is between zero and $a_j\rho/(1-\rho)$.

### Large rates without mode enumeration

**Proved bounds.** The Bessel series satisfies $z^2I_k''+zI_k'-(z^2+k^2)I_k=0$ by termwise differentiation. Let
$$
R=\sqrt{z^2+k^2},\quad q=z^2/R^2,\quad
b(q)=q/2-5q^2/8,\quad A=R-q/2+b(q)/R.
$$
The variable $A$ in this section approximates $h_k$, whereas $A_k$ above is a series derivative in $u$. The Riccati equation is $Dh_k=R^2-h_k^2$. Two global barriers are $R-1<h_k<R$ for $z>0$. For the upper barrier, $DR=qR>0$ at a putative contact and the series puts $h_k$ below $R$ near zero. For $k>0$ compare the quadratic coefficients $1/[2(k+1)]$ and $1/(2k)$. For $k=0$, $h_0=O(z^2)$ and $R=z$. The lower barrier starts strictly below $h_k$ near zero and has residual $qR-2R+1<0$ when $R>1$. For $R\le1$, positivity of $h_k$ suffices. First-contact comparison proves both bounds.

Using $Dq=2q(1-q)$, the residual of $A$ is
$$
E=DA+A^2-R^2
  =\frac{2q(1-q)b'(q)-2qb(q)}{R}+\frac{b(q)^2}{R^2},
\qquad |E|\le\frac{5}{8R}+\frac{1}{64R^2}.
$$
The constants follow from $|b|\le1/8$ and $|b'|\le3/4$. Writing $e=h_k-A$ gives $de/dz=-(h_k+A)e/z-E/z$. On $[z/2,z]$ for $z\ge32$, both $h_k$ and $A$ lie between $R-1$ and $R$, so $(h_k+A)/z\ge1$ pointwise there and $|e(z/2)|\le2$. At a point $v$ of that integration interval $|E(v)/v|\le1/v^2\le4/z^2$. The integrating factor therefore gives
$$
|h_k-A|\le2e^{-z/2}+4/z^2\le5/z^2
\qquad(z\ge32).
$$
This is a uniform error bound for all integer orders, independent of the paired-count mode.

To apply it in the profile, compute $\Delta=R-H=(k-d)(k+d)/(R+H)$, $q=1/(1+(k/z)^2)$ and, if needed, $1-q=(k/z)^2/(1+(k/z)^2)$. For each observation,
$$
\begin{aligned}
h_k-H&\in\Delta-q/2+b/R+[-5/z^2,5/z^2],\\
D(h_k-H)&\in
-\Delta(z/R)(z/H)-q(1-q)
+\frac{2q(1-q)b'-qb}{R}-E+[-10R/z^2,10R/z^2].
\end{aligned}
$$
For the curvature, use $Dh_k=DA-E-(h_k+A)e$ and $h_k+A\le2R$. These are constant-work expressions per observation. Evaluate them over cells by interval substitution or endpoint bounds. The centered formulas avoid subtracting two rate-sized quantities to get a score or curvature of order one. For general inputs with very large order relative to $z$, use the bounds only when they separate the comparisons.

### Root isolation and strict global exclusion

**Proved certificate.** Choose a feasible candidate point $c$, a relatively wide isolation neighborhood $J$, and a narrow bracket $B\subset\operatorname{int}J$. Use one consistent coordinate for derivatives and widths. Prove strictly negative curvature on $J$ and opposite endpoint score signs on $B$. This gives exactly one stationary point in $J$, strictly maximizing there. Map both endpoints of $B$ to rates and test the two relative coefficient errors, including representation of the returned rates. The mapping is monotone. For small $t$ convert via $t=z^2/[2(H+d)]$, avoiding subtraction.

Take a lower enclosure $L$ of the attainable value $\ell(c)$. Subdivide the finite complement of $J$. In a cell of midpoint $v$ and half-width $h$, an upper enclosure is
$$
U=\ell^+(v)+G h+M_+h^2/2,
$$
where $G$ bounds the absolute midpoint score and $M_+\ge0$ bounds the cell curvature. With a proved sign of the score on the cell, use its maximizing endpoint's upper bound instead. Each excluded cell must satisfy $U<L$. The finite cover can include $t=0$ by using the exact face or $\ell(a)+2a$ as an upper bound on $[0,a]$, since the $t$ score is at least $-2$ in the interior. The opposite face is infeasible or the all-zero case already handled by the constant route. On $t\ge T\ge1$, the earlier Poisson-mode bound gives $\ell\le -\tfrac12\log(2\pi\lfloor T\rfloor)$. Use strict comparison there as well. This proves a single global maximizer and isolates both its coefficients. Under practical floating-point arithmetic, use lower and upper bounds with evaluation allowances in all these tests. The numerical demonstration below uses 60-decimal interval arithmetic.

One can also work in likelihood differences: take the attainable point $c$ to have relative value zero, and exclude regions by proving their upper differences to be negative. In particular, if curvature is at most $-\kappa<0$ on $J$ in the $y=\log z$ coordinate, then at an endpoint $v$ in that coordinate,
$$
\ell(v)-\ell(c)\le S(c)(v-c)-\kappa(v-c)^2/2.
$$
In this inequality $c$ also denotes its coordinate. Proving monotonicity outward from each endpoint transfers those strict comparisons to the full complement. This form requires no absolute log-mass evaluation near the large-rate candidate.

**Numerical demonstrations.** The script uses the exact bounds above with 60-decimal mpmath intervals. It isolates both ordinary roots to a relative half-width of $4\times10^{-11}$ in $t$ and uses 256 curvature cells in each isolation neighborhood. The likelihood is measured relative to the face. The strict cover tested 60 cells for 17 zeros and 161 fours ($t\approx0.0000590583369088$, tail from $8$), and 312 cells for 19 zeros and one 20 ($t\approx6.32909729351$, tail from $64$). The latter cover excludes the competing local maximum on the Poisson face. Both finite covers use a feasible interior point and strict comparisons through the tail.

### The snapshot sample `wide`

The exact data are $(-8,-7,992,999992)$ and $d=250242.25$. The following is a **proved certificate conditional on the listed arithmetic comparisons**, checked at 60 decimal digits using mpmath intervals. Both Poisson faces are infeasible.

- For $0<z\le1000$, $\overline{R}-H\ge\overline{k}-d-z^2/(2d)>1$, so the coarse lower barrier proves $S>0$.
- For $1000\le z\le10^7$, write $K=999992$ and use
  $$
  \overline R-H\ge (R_K+3z)/4-H
  = \frac{K^2-16d^2+24d^2 z/(H+z)}{4(R_K+4H-3z)}.
  $$
  The numerator increases. On each of 14 successive cells formed by doubling the left endpoint and capping at $10^7$, use its value at the left endpoint and use $4(R_K+4H)$ at the right endpoint in the denominator. Each resulting lower bound exceeds one.
- On $[10^7,10^{11}]$, $R_k+z\le2.01z$ for all four orders, so
  $$
  \overline R-H\ge\frac{\overline{k^2}/2.01-d^2/2}{z}.
  $$
  At $10^{11}$ this lower bound exceeds $1/2+1/(8\cdot10^7)+5/10^{14}$. The large-rate score bound thus gives $S>0$ on the whole region.
- On $[4\cdot10^{11},\infty)$, $\overline R-H\le\overline{k^2}/(2z)$ and $q\ge0.99$. Substitution in the upper score bound gives $S<0$, using $z=4\cdot10^{11}$ in the terms that decrease to zero.
- On $J=[10^{11},4\cdot10^{11}]$ in $z$, enclose $DS$ on 30 cells of length $10^{10}$. The largest computed upper curvature is below $-0.2151$, so use $\kappa=0.19$ in the likelihood comparison. The two endpoint differences relative to the feasible bracket midpoint are bounded above by $-0.03745$ and $-0.05463$ per observation. Monotonicity on the outer regions extends both strict exclusions to all $z$ outside $J$.

The root enclosure is $187375062374<z_*<187375062375$ after 39 bisection evaluations. The rates at its midpoint are approximately $(93687656308.45855,93687406066.20855)$. Both rate intervals have relative width below $5.4\times10^{-12}$. **Numerically**, the binary64 centered score at the two bracket endpoints is $1.38639\times10^{-12}$ and $-1.28209\times10^{-12}$, separated by a practical allowance of $10^{-13}$ on either comparison. The interval prototype checks both scores against that allowance. Five independent 60-decimal Bessel evaluations also lie inside the corresponding analytical score and curvature enclosures. The allowance is tested here on the wide bracket endpoints. Production margins will also need to account for errors in computing the empirical mean and the curvature comparisons.

## 3. The stationary Poisson face

**Proved local decision.** For a feasible positive-mean face, $p=d,m=0$. If $g_0=0$, substituting the zero-product series derivatives into $g'$ gives
$$
g'(0)=1/d-d^2\overline{1/[(k+1)^2(k+2)]}.
$$
For $(0,3,3)$, the average in this formula is $7/40$, so $g'(0)=-1/5$. The per-observation inward likelihood difference is $-t^2/10+O(t^3)$. The sum log likelihood has quadratic coefficient $-3/10$. Hence the face strictly beats all sufficiently close interior rates. An interval $g'<0$ check reaching $t=0$ supplies a computable neighborhood.

**Proved global decision for this sample.** Here $z^2=4t(t+2)$ and $3S=r_0+2r_3-6t$. Let $U_k$ be the 17-level continued-fraction upper bound. For $0<t\le16$, form the reduced rational function $6t-U_0-2U_3=N(t)/Q(t)$ with positive leading denominator coefficient. $Q$ has nonnegative coefficients and positive constant coefficient. $N(t)=t^2P(t)$ with $P$ of degree 32. The coefficient of $y^j$ in $(1+y)^{32}P(16y/(1+y))$ is
$$
\sum_{i=0}^j[t^i]P(t)\,16^i\binom{32-i}{j-i}.
$$
All 33 coefficients are positive by exact integer calculation, with minimum $4807514767210792467038208000000$ under the reduced normalization in the script. Thus $S<0$ for $0<t<16$ and at $16$ by the leading transformed coefficient. For $t\ge16$ we have $z\ge32$, $q\ge0.99$ for the two orders and $\overline R-H\le3/z$. The large-rate upper bound gives $S\le 3/32-0.495+1/(8\cdot32)+5/32^2<0$. This gives strictly decreasing likelihood on the full positive ray. Profiling and face support then give the unique joint winner $(2,0)$, with the reflected result for $(0,-3,-3)$.

## Reproduction

Run from the worktree root. The workstation run exited zero. It used exact Fraction and SymPy comparisons for the rational witnesses and polynomial proof, and mpmath point and interval arithmetic at 60 decimal digits for the numerical results. No C++, R or site builds were run.

```sh
mkdir -p build/tmp
export TMPDIR="$PWD/build/tmp"
export OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 MKL_NUM_THREADS=1
/tmp/umvl-cpu/run nice -n 10 python3 -u dev/research/evaluations/2026-11-25-skellam-math-data/check.py
```

[Reproduction script](2026-11-25-skellam-math-data/check.py)

## What this changes for the fitter

- Joint interior results can carry `certified = TRUE` and `unique = TRUE` when the isolated root satisfies both coefficient tolerances and all competing regions are strictly excluded. In particular, local polish alone cannot decide global optimality even on two-sided samples. Keep the grid as a nomination mechanism.
- Use the centered large-rate score and curvature to bracket the scalar root and provide the coefficient accuracy gate without the differentiable paired-count series. Try monotonic exclusions with strict relative likelihood bounds before using the absolute-likelihood cover at large rates. The successful `wide` route supplies an interior estimate and a global certificate.
- For a feasible face with exact zero inward score, use the sign of the higher derivative with a checked neighborhood. A negative second derivative proves a local winner and allows strict global comparison. A positive one refutes the face. The exact $(0,3,3)$ result permits a unique certified attained Poisson fit. A computed near-zero score that could be positive does not justify a decreasing interval reaching zero.
- Apply solver, certificate and enumeration budgets separately using their existing work units. If a strict comparison or enclosure does not close, keep a successful, unrefuted search result with its uncertified and unknown-uniqueness flags. An enclosure proving a candidate wrong requires a verified replacement or failure. A failed search without a successful rescue still stops.

## Open

- Universal uniqueness of the **global** maximum for integer samples remains conjectured. Multiple strict local maxima disprove unimodality and interior-root uniqueness. Settling the global question requires an exclusion theorem for exact equality of the best separated peak values, or an integer sample with distinct, rigorously tied global winners.
- Inputs with both $g_0=g'(0)=0$, or with an interior maximum of zero second derivative, need higher-order tests or different isolation conditions. The strict-curvature certificate intentionally has no acceptance case for them.
- For production, calibrate score, mean and likelihood padding in native and double-only builds, with near-face cases whose required endpoint signs approach binary64 error. The prototype certifies those ordinary examples using 60-decimal evaluation. The `wide` endpoint test supports a centered binary64 score there. Runtime and closure frequency across the full fit snapshot remain to be measured in an implementation job.
