# TASK-273 Skellam global uniqueness and exact ties

Reflection fixes every Skellam maximizer for a reflection-symmetric sample. All 6,656 integer multisets in the bounded search have a unique global maximizer. Two different diagonal peaks can be arbitrarily difficult to distinguish by likelihood. The search certifies symmetric integer examples with opposite winners and gaps below $7\times10^{-22}$ per observation. Exact global ties exist for real-weight histograms on integer support, including with nonzero mean and between a Poisson face and an interior peak. Whether any of the coexistence weights is rational, and whether any integer sample has a global tie, remain open.

The worktree started at `b51329d8`. The profiling, attainment and basic interval bounds used here come from the prior work. [Skellam dossier](../../../univariateML-cpp/include/cppunivariate/distributions/skellam/README.md) · [Profile reduction and attainment](2026-10-29-skellam-report.md) · [Gap cover](2026-11-02-skellam-cert-report.md) · [Score, curvature and tail bounds](2026-11-25-skellam-math-report.md)

## Reflection and a uniqueness theorem

**Proved.** Let $w_k$ be the weight at magnitude $k$. Reflect the signed data if necessary and write $d=|\bar x|$, $t=\min(\mu_1,\mu_2)$, $z=2\sqrt{t(t+d)}$, $H=2t+d=\sqrt{z^2+d^2}$ and $D=z\,d/dz$. Every interior joint winner satisfies $\mu_1-\mu_2=\bar x$ in the original orientation. A feasible Poisson face can win only at its Poisson MLE, which has the same mean property. These facts apply to finite positive real weights as well. Thus if both $(a,b)$ and $(b,a)$ were winners, $a-b=b-a=\bar x$ would imply $a=b$. A sample symmetric about zero always has $\bar x=0$, and a nonconstant such sample excludes both faces. Any distinct winners would have the form $(t_1,t_1)$ and $(t_2,t_2)$ with $t_1\ne t_2$. Invariance of the likelihood under swapping rates actually holds for every zero-mean sample, since the signed data enter the interior likelihood only through their sum and their magnitude histogram.

For $z>0$ the profile and score per observation are
$$
\ell(z)=-H+d\log\!\frac{H+d}{z}+\sum_k w_k\log I_k(z),
\qquad S(z)=D\ell(z)=\sum_k w_k h_k(z)-H,
\qquad h_k(z)=zI'_k(z)/I_k(z).
$$
Thus samples with the same absolute mean and magnitude proportions have the same profile. The signed mean selects the orientation of the final rates. The maximizer set of any finite histogram is finite. In the interior the score is analytic and eventually strictly negative by the tail bound below. At the origin the profile score in $t$ is eventually positive for mixed signs by its positive $1/t$ term. For one-sided data the profile extends analytically to the feasible face, and accumulation of zeros there would force an identically zero score. The all-zero case has its known unique point-mass fit.

**Proved.** Every sample with a single observed magnitude $K$ has a unique fit. The constant cases are already covered by the dossier. Otherwise $0\le d<K$ and $S=h_K-H$ starts positive, with $S(0+)=K-d$, and is eventually negative. From $Dh_K=z^2+K^2-h_K^2$,
$$
DS=F(z)-S(S+2H),\qquad F(z)=K^2-d^2-z^2/H .
$$
The function $F$ is strictly decreasing from a positive value to $-\infty$. At the first zero of $S$, $F>0$ would imply an upward crossing. If $F=0$, then $D^2S=DF<0$ there, so $S$ would be negative immediately to its left. Both contradict positivity before the first zero. Hence the first zero has $F<0$, and all subsequent zeros would also be strict downward crossings. A second downward crossing requires an intervening return to nonnegative score, which this rules out. The sole root gives the unique global maximum on the profile and strict profiling lifts it to joint uniqueness.

## Bounded integer search

**Numerical certificate using rational and interval arithmetic.** The enumeration includes every unordered multiset with $2\le n\le6$ in $[-4,4]$ and with $2\le n\le4$ in $[-6,6]$, counting their overlap once. It covers 6,656 samples, of which 92 are reflection-symmetric, 342 have mean zero and 57 are constant. Deduplicating the nonconstant samples by exact $(d,(w_k)_k)$ leaves 2,690 profiles. The certificate found 2,689 profiles with one local maximum and one profile with two. The two-mode profile is that of $(-5,0,0,0)$ and its reflection. The feasible face has inward score $-1/96$, and the interior peak beats the face by approximately $0.015101006573045675$ per observation. The comparison of all feasible candidates gave one strict global winner for every profile. The constant cases follow the dossier proof.

The checker used the continued fraction and the $t$ score and curvature from TASK-242. At rational $t$ it evaluates $u=t(t+d)$ and propagates a lower and upper bound through $r_j=4u/[2(j+1)+r_{j+1}]$. The starting interval at the last order $J$ is $[0,4u/(2(J+1))]$. Each integer floor or ceiling rounds outward on a grid with spacing $2^{-224}$. This encloses $A_k=2/[2(k+1)+r_{k+1}]$. Between rational endpoints, monotonicity of $A_k$ and $A_k'=A_k(A_{k+1}-A_k)$ give cell enclosures. The remaining score and curvature operations use 60-decimal interval arithmetic.

Near zero, the mixed-sign score in $t$ is bounded below by $-2+\overline{(-x)_+}/t$. At a feasible face the checker encloses the score from zero, or uses the exact rational zero face score and a strict curvature sign on a neighborhood. In the far tail let $K=\max k$, $M_2=\sum_k w_k k^2$ and $R_k=\sqrt{z^2+k^2}$. If $z\ge\max(32,2K,2M_2+2)$, then $\sum_k w_k(R_k-H)\le M_2/(2z)<1/4$ and $z^2/R_k^2\ge4/5$. The TASK-242 Riccati upper bound gives
$$
S(z)<\tfrac14-\tfrac25+\tfrac{1}{8z}+\tfrac5{z^2}<0 .
$$
The checker starts the tail at a rational $t$ with $2t$ at least this large.

The intervening interval is fully covered by cells. A strict score sign excludes roots. A strict curvature sign and strict endpoint score signs determine whether the cell contains one root or zero. The checker bisects unclassified cells and fails if it exhausts the subdivision allowance. It refines each candidate peak bracket by 108 bisections when there are competing candidates, then bounds its likelihood over that entire bracket. The positive paired-count series has a geometric upper bound on its remainder and the checker takes interval logarithms only after adding this remainder enclosure. A competing face is evaluated exactly in $t$, with interval arithmetic for its log value. A winner is accepted only when its lower log likelihood exceeds every competitor's upper log likelihood.

**Numerical nomination.** A separate 60-decimal point scan used $K=10,20,40,80,160$, total weight $w=j/(100K)$ at magnitude $K$ for $j=50,\ldots,62$, and 161 nodes $z=\exp(i/16)$ for $-48\le i\le112$. The remaining mass was at zero and the two signs of $K$ had equal weight. The scan nominated two interior peaks and a switch in which peak wins at $K=40,80,160$. The interval checks below use $K=40$ and do not depend on resolution of that point grid.

## Mean-preserving deformations

**Proved.** For each $z>0$, $k\mapsto h_k(z)$ is strictly increasing and strictly discretely convex on nonnegative integers, and $x\mapsto h_{|x|}(z)$ is strictly discretely convex on all integers. To see this, use
$$
h_k(z)=k+\frac{z^2}{2(k+1)}+O(z^4),\qquad Dh_k=z^2+k^2-h_k^2.
$$
The increment $\delta_k=h_{k+1}-h_k$ starts in $(0,1)$ for sufficiently small $z>0$. At a first contact with zero, $D\delta_k=2k+1>0$, and at a first contact with one, $D\delta_k=2(k-h_k)<0$. Here $h_k>k$ follows from the positive Bessel series. Both derivative signs preclude contact from the interior of $(0,1)$. For $k\ge1$ the second difference $C_k=h_{k+1}+h_{k-1}-2h_k$ starts positive by strict convexity of $1/(k+1)$. At a first zero its two adjacent increments equal some $\delta\in(0,1)$ and $DC_k=2-2\delta^2>0$, again precluding contact. At the signed origin the second difference is $2(h_1-h_0)>0$.

Let $P$ be a histogram on finite signed integer support with extremes $a<b$, mean $\bar x$, and some weight strictly between $a$ and $b$. Let $Q$ be the two-point histogram on $\{a,b\}$ with the same mean. Along $P_\lambda=(1-\lambda)P+\lambda Q$, the mean stays fixed and $\ell_\lambda(z)=\ell_P(z)+\lambda G(z)$ with $G=\ell_Q-\ell_P$. The secant through the two extremes strictly dominates $h_{|x|}$ at all intermediate integers, so
$$
DG=E_Q h_{|X|}-E_P h_{|X|}>0 .
$$
When the face is feasible, $G$ has a finite continuous extension to $z=0$, giving strict increase from that endpoint as well. For $\lambda_2>\lambda_1$, the maximizers at $\lambda_2$ all lie weakly to the right of those at $\lambda_1$. Indeed, if $z_2<z_1$ were respective winners, then the likelihood difference favoring $z_1$ over $z_2$ would increase strictly from $\lambda_1$ to $\lambda_2$, contradicting optimality of $z_2$. The open intervals between the smallest and largest winners at distinct tie weights are disjoint. Each contains a rational point, so tie weights along this deformation are at most countable. On a fixed support and mean polytope of positive dimension, these rays from $Q$ give uniqueness for almost every real weight vector by integration in radial coordinates. The tie set is measurable, since a tie can be written as equality of the unrestricted maximum and two maxima over separated compact intervals with rational endpoints, taking a countable union over these intervals. Countability on a ray and measure zero in the polytope leave the rational-weight question open.

## Weighted coexistence and integer near-ties

### Two interior winners

**Proved from interval comparisons.** Put magnitude weight $w$ at 40, weight $\epsilon$ at 1 and $1-w-\epsilon$ at zero. Use equal signed weights $w/2$ at $\pm40$ and weights $(\epsilon\pm d)/2$ at $\pm1$. The case $(\epsilon,d)=(0,0)$ is reflection-symmetric. The case $(\epsilon,d)=(10^{-4},10^{-5})$ is asymmetric with strictly positive mean. All nonzero weights in these cases are on integer observations.

The following intervals have exact rational endpoints. The script encloses the score and curvature for the whole weight interval by enclosing them at its two endpoints, since at fixed $d$ both are affine in $w$. The resulting certificate gives exactly one stationary point in each $t$ window, in order a strict maximum, a strict minimum and a strict maximum, with a checked exclusion of all other roots and both faces.

| $(\epsilon,d)$ | Weight interval | Small peak $t$ | Minimum $t$ | Large peak $t$ |
| --- | --- | --- | --- | --- |
| $(0,0)$ | $[0.0152227,0.0152230]$ | $[0.651,0.691]$ | $[1.384,1.424]$ | $[3.369,3.409]$ |
| $(10^{-4},10^{-5})$ | $[0.0152219,0.0152222]$ | $[0.652,0.692]$ | $[1.384,1.424]$ | $[3.368,3.408]$ |

The large-minus-small peak value at the endpoints is enclosed with signs $(-,+)$ in each row. The approximate values are $(-8.56409\times10^{-6},9.58202\times10^{-6})$ and $(-6.11081\times10^{-6},1.20283\times10^{-5})$, respectively. Strict curvature makes the roots and their values analytic throughout each weight interval. Differentiating a peak value with respect to $w$ gives $\log(I_{40}(z)/I_0(z))$ there, since the root term vanishes. This function strictly increases in $z$ because $h_{40}>h_0$. Hence the peak-value difference strictly increases, so each row contains exactly one weight where the two peaks tie globally. This gives exactly two distinct joint winners at each of those real weights. **Numerically at 60 decimals**, the two coexistence weights are approximately $0.01522284158668345$ and $0.01522200106678294$. The first gives $t$ values near $0.67197626$ and $3.38994135$. Rationality of either weight is open.

**Certified integer near-ties.** For the symmetric family the checker uses continued-fraction neighbors of the numerical nomination for $w/2$ at denominator bounds $10^6$ and $10^{12}$. It verifies the signs by a fresh full profile cover and interval comparison on each exact rational histogram. The gap column is the rounded per-observation log likelihood of the large peak minus that of the small peak.

| Count at $0$ | Count at each of $\pm40$ | Gap |
| ---: | ---: | ---: |
| 349,977 | 2,705 | $-5.04928604\times10^{-10}$ |
| 645,355 | 4,988 | $+1.45046606\times10^{-11}$ |
| 114,841,637,861 | 887,620,131 | $-4.73467478\times10^{-22}$ |
| 926,766,879,000 | 7,163,054,741 | $+6.28828086\times10^{-22}$ |

If the symmetric coexistence weight is irrational, convergents to its half give infinitely many examples on both sides with per-observation gaps $O(n^{-2})$ and total log likelihood gaps $O(n^{-1})$. This follows from the usual convergent error bound and bounded derivatives of the peak values on the certified weight interval. If the weight is rational, clearing denominators gives an exact integer-sample tie. In either case, a fixed closeness threshold for peak heights does not decide equality or uniqueness.

### A Poisson face and an interior winner

**Proved.** Take nonnegative data on $\{0,1,20\}$ with weights $(19\lambda/20,1-\lambda,\lambda/20)$ for $0\le\lambda\le1$. The mean is one for every $\lambda$. At $\lambda=0$ the constant sample uniquely selects the Poisson face. At $\lambda=1$ the rational likelihood improvement from TASK-242 for 19 zeros and one 20 proves that an interior fit beats the face. The exact inward score at the face is the convex combination of $-1/2$ and $-1/21$. The global $t$ curvature ceiling is at most two, giving a uniformly decreasing neighborhood $[0,1/100]$. The uniform negative tail score permits the remaining search to be restricted to a fixed compact interval $[1/100,T]$ for a sufficiently large $T$. The maximum over that interval of $\ell_\lambda(t)-\ell_\lambda(0)$ is continuous and strictly increasing in $\lambda$. Strict increase follows from the preceding secant argument with endpoints $0$ and $20$, uniformly over the compact interval since $G(t)-G(0)>0$ there. Its sign reverses between zero and one, proving a unique real weight at which the face and at least one interior fit tie globally.

**Numerical nomination at 60 decimals and interval verification of nearby integer samples.** The face switch is near $\lambda=0.79657072002332530$, with an interior candidate near $t=3.29031272$. The exact integer samples with counts $(10{,}228{,}536,2{,}749{,}660,538{,}344)$ and $(8{,}739{,}449,2{,}349{,}360,459{,}971)$ at $(0,1,20)$ have certified interior-minus-face gaps per observation of approximately $-2.04528\times10^{-12}$ and $+8.96894\times10^{-13}$, respectively. Each has exactly one strict interior maximum and the feasible face. The first uniquely selects the face and the second uniquely selects the interior maximum.

## An arithmetic obstruction

**Proved using Siegel's Bessel quotient theorem.** For rational weights and rational $d$, every positive stationary argument $z$ is transcendental. Suppose $z>0$ were algebraic. Siegel's theorem at the nonzero algebraic argument $iz$, with $J_0(iz)=I_0(z)$ and $iJ'_0(iz)=I_1(z)$, makes $T=I_1(z)/I_0(z)$ transcendental. The Bessel order recurrence at this fixed $z$ writes $I_k/I_0=L_k(T)$, where
$$
L_0=1,\qquad L_1=T,\qquad L_{k+1}=L_{k-1}-(2k/z)L_k .
$$
For $k\ge1$ the coefficient of $T$ in $L_k$ is nonzero by alternating signs in this recurrence, starting with $1,-2/z$. At a root of $L_j$ with $j\ge1$, $L_{j+1}\ne0$ by backward recurrence to $L_0=1$. Starting with this nonzero $L_{j+1}$, the values from order $j+1$ onward alternate in sign and are all nonzero. Thus the linear polynomials for positive orders have distinct roots. If any positive order has positive weight, its term in
$$
R(T)=\sum_k w_k\bigl(k+zL_{k+1}(T)/L_k(T)\bigr)
$$
has a genuine pole at its root, with no cancellation by another term. If all weight is at zero, $R(T)=zT$ is already nonconstant. Therefore $R(T)=H$ would make the transcendental $T$ solve a nonzero polynomial over the algebraic numbers, since $H=\sqrt{z^2+d^2}$ is algebraic under the assumption. This contradicts Siegel's theorem. In particular, both free rates at an interior stationary point for integer data are transcendental, since an algebraic $t$ would make $z=2\sqrt{t(t+d)}$ algebraic. [Siegel's 1929 paper and translation](https://link.springer.com/book/10.1007/978-88-7642-520-2) · [Lorch and Muldoon, statement of the quotient theorem](https://doi.org/10.1155/S0161171295000706)

## Reproduction

The three Python checks exited zero on the workstation under one-slot reservations. They used exact rational arithmetic for histogram construction and the continued-fraction enclosures, mpmath 60-decimal intervals for the certificate operations, and 60-decimal point arithmetic for numerical nominations. The scope was mathematical, with changes confined to this report and its two evidence scripts.

~~~sh
mkdir -p build/tmp
export TMPDIR="$PWD/build/tmp"
data=dev/research/evaluations/2026-11-25-skellam2-math-data
/tmp/umvl-cpu/run python3 -u "$data/explore.py"
/tmp/umvl-cpu/run python3 -u "$data/certify.py" small
/tmp/umvl-cpu/run python3 -u "$data/certify.py" coexistence
~~~

[Point exploration](2026-11-25-skellam2-math-data/explore.py) · [Interval certificate and exact enumeration](2026-11-25-skellam2-math-data/certify.py)

## What this changes for the fitter

- In a free joint fit, enforce the sample mean through profiling. Reflection supplies no distinct pair to return. The equal-magnitude theorem permits `unique = TRUE` once the sole interior root has a bracket meeting the requested error in both rates, or directly for the constant route.
- On general inputs, set `certified = TRUE` and `unique = TRUE` when the coefficient inclusion checks pass and the strict global certificate excludes all competitors, including the feasible Poisson face and all separated interior peaks. A likelihood-gap tolerance alone supplies no uniqueness claim. The symmetric and one-sided integer histograms above exercise opposite winners across very small gaps.
- If a comparison or root enclosure remains unresolved, keep a successful, unrefuted search result with `certified = FALSE` and `unique = NA`. A strict bound that refutes the candidate requires a checked replacement or a failure. A failed search without a checked rescue stops through its status.
- If an exact tie is proved on an input and all other candidates are excluded, return `unique = FALSE` with every maximizing rate pair as a two-dimensional point in `MaximizerSet`, using separate coefficient enclosures where needed. Reuse the typed point representation used by `sn` and `snorm`. The theorem for Skellam concerns isolated point alternatives and gives no symmetry-based trigger for creating them.

## Open

- Determine whether any coexistence weight here is rational, or otherwise exclude ties for arbitrary rational histograms. Even when $d=0$, the missing separation result must address distinct positive stationary arguments $z_1,z_2$ and the equation
  $$
  e^{n(z_2-z_1)}=\prod_k\bigl(I_k(z_2)/I_k(z_1)\bigr)^{n_k}.
  $$
  The positive arguments are transcendental by the single-argument obstruction above. An algebraic-independence theorem at nonzero algebraic arguments does not decide this equality. The deformation proves strict crossing as a function of weight and leaves the arithmetic of the crossing weight undetermined.
- For the one-sided problem, the corresponding unresolved equality is between an interior critical value and the feasible face value. A counterexample in either regime needs a rational histogram together with an exact equality proof and global exclusion of any larger value. Finite separation checks can settle individual inputs with a strict gap. General uniqueness for integer samples remains conjectured.
