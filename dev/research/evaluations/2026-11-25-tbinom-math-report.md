# Free-size truncated binomial profiles and the Poisson face

TASK-279, base revision bb391a57. Bounded truncation can produce two strict integer local maxima, including when both bounds are free. The zero-truncated profile need not be log-concave in size. Its general integer unimodality remains open. For either kind of truncation, a per-input finite comparison can decide whether a finite fit attains the supremum. On the two modifier goldens it certifies sizes 13 and 8, respectively.

## Profile and limit

Let the retained integers be $L,\ldots,U$, with $L=\max(0,\lfloor a\rfloor+1)$ and $U=\lfloor b\rfloor$ or $\infty$. Let $c_k$ denote the observed frequencies, $P$ their empirical law, $M$ the maximum, $n$ the sample size, $S$ the sum, and $\mu=S/n$. For a free lower or upper bound, tightening the retained support to the corresponding sample extreme maximizes the likelihood for each $N,p$ because it decreases the normalizer and retains every observation. Thus the same fixed-window argument covers the four hold patterns after applying that reduction. Constant data attain likelihood one at size $M$ with probability one, including the singleton cases. Below we consider nonconstant data.

For $N\ge M$, set $s=1/N$, $z=Np/(1-p)$, and
$$
v_k(s)=\frac{1}{k!}\prod_{j=0}^{k-1}(1-js),\qquad
Z_N(z)=\sum_{k=L}^{\min(U,N)}v_k(s)z^k .
$$
The log-likelihood is $\sum_k c_k\log v_k(s)+S\log z-n\log Z_N(z)$. In $\log z$ its score is $n(\mu-E X)$ and its curvature is $-n\operatorname{Var}(X)$. Since $\mu$ is strictly between the retained endpoints, the unique root gives the profile $\ell_N$.

**Proved.** The profile converges to the maximized likelihood $\ell_\infty$ of the Poisson conditioned on the same window, and $Np_N\to\lambda$, where $\lambda$ is the unique Poisson rate matching $\mu$. For finite $U$ use the polynomial $Z(s,z)=\sum_{k=L}^U v_k(s)z^k$ near $s=0$. For $U=\infty$ use
$$
Z(s,z)=\exp\left(\frac{\log(1+sz)}s\right)-\sum_{k=0}^{L-1}v_k(s)z^k .
$$
At $s=1/N$ this is the binomial normalizer by the binomial theorem. At zero the removable quotient equals $z$. Both normalizers are analytic near $(0,\lambda)$. The Poisson score has strictly negative derivative in log rate, so the implicit function theorem gives the analytic branch $z(s)$ through $\lambda$. At reciprocal integers its root is the unique fixed-size optimum. This proves convergence and transfers the Taylor expansion of the branch to the integer profile.

## Integer profile shape

**Proved by interval separation.** Both exact frequency vectors below give a strict local maximum at the first feasible size, 4, and a second strict local maximum with two feasible neighbors. The displayed positive gaps are total log-likelihood differences. The integer score signs and the interval inclusions used to obtain the gaps are described below.

| Retained window | Frequencies in increasing count order | $\ell_4-\ell_5$ | Second peak $J$ | $\ell_J-\ell_{J-1}$ | $\ell_J-\ell_{J+1}$ |
|---|---|---:|---:|---:|---:|
| $[0,4]$ | $(2358,0,2927,4714,1)$ | 1.27284728 | 16 | 0.00293436 | 0.00452432 |
| $[1,4]$ | $(45145,4976,49611,268)$ | 14.44731647 | 19 | 0.00695897 | 0.01025305 |

In both examples the observed extremes are the retained endpoints. Holding either or both bounds at those endpoints or freeing them produces the same profiles. [Interval verification](2026-11-25-tbinom-math-data/check.py).

**Proved for narrow windows.** With two retained counts, the conditional model fits the histogram exactly at every feasible size, as does the Poisson face. With three retained counts $L,L+1,L+2$, the log of the ratio of the two outer weights' product to the middle weight squared is
$$
\gamma_N=\log\left(\frac{L+1}{L+2}\frac{N-L-1}{N-L}\right).
$$
It increases to $\gamma_\infty$. Absorbing the linear part of the log weights into the free log odds represents the three weights as $1,e^\theta,e^{2\theta+\gamma_N}$. At fixed $\gamma$ profile $\theta$ by matching the mean. The derivative of the resulting log-likelihood in $\gamma$ is $n(P\{L+2\}-Q_\gamma\{L+2\})$. Its second derivative is minus $n$ times the variance of the residual of the indicator of $L+2$ after linear projection on the count and a constant. The variance is strictly positive because the indicator is nonaffine on the three points with positive probabilities. Hence the profile is strictly concave in $\gamma$ and unimodal on the increasing grid $\gamma_N$. If $M=L+1$, the smallest size saturates the two observed counts and all subsequent steps decrease since $P\{L+2\}=0$. In a three-point window the Poisson face strictly wins exactly when $E_P(X)_2\ge E_Q(X)_2$ for the mean-matched Poisson $Q$. Indeed the sign of the derivative at $\gamma_\infty$ is the sign of this gap, since mass and mean agree and $(k)_2$ differs from an affine function by twice the top-point indicator. At equality the empirical law is exactly $Q$ and the concavity is strict.

**Proved sufficient condition across windows.** Suppose $Q_M$ is the conditional binomial at the first feasible size with its mean matched to $\mu$. If $P$ is smaller than $Q_M$ in convex order, the profile decreases strictly with size except on the flat two-count window. For $T>N$ the successive ratios of $Q_T(k)/Q_N(k)$ on the common support increase with $k$, since they are a constant times $(T-k)/(N-k)$. This ratio is log-convex and any new support lies above the common support. Equal masses and means imply the sign pattern of $Q_T-Q_N$ is positive, negative, positive, ignoring zeros, whenever the laws differ. Subtracting an affine chord from each convex test function at the two crossing locations proves $Q_N\le_{\rm cx}Q_T$. Thus $P\le_{\rm cx}Q_N$. The log probability ratio is convex on the support of $Q_N$, so
$$
\frac{\ell_T-\ell_N}{n}
 \le E_{Q_N}\log(Q_T/Q_N)
 \le \log Q_T(\operatorname{supp}Q_N)\le 0.
$$
The inequality is strict if support grows or the ratio on the common support varies. The sufficient condition holds whenever the observations occupy two adjacent counts, since their empirical law minimizes convex expectations at its mean. This complements the increasing-to-Poisson convex-order test already used for bounded windows. [Earlier convex-order certificate](2026-11-20-route-acc-report.md#truncated-binomial-archive-cases).

**Proved for zero truncation.** For the exact sample $(1,2)$, $\mu=3/2$ and $E_P(X)_2=1$. Its matched positive-Poisson rate satisfies $\lambda>2/3$ by monotonicity of $z/(1-e^{-z})$ and $e^{2/3}>9/5$. Thus the first coefficient $A$ below is strictly positive. Analyticity gives
$$
\ell_{N+1}-2\ell_N+\ell_{N-1}=2nA/N^3+O(N^{-4}),
$$
so the likelihood is eventually log-convex in size for this sample. General zero-truncated integer unimodality requires a separate sign-change argument. DeRiggi's positive-binomial abstract reports a local maximum and a saddle for a likelihood with continuous critical points. The accessible abstract gives neither the count vector nor the signs at consecutive integers. Earlier repository work records the same limitation. [DeRiggi (1994), publisher abstract](https://www.sciencedirect.com/science/article/abs/pii/0167715294900612), [prior size-search report](2026-10-31-size-search-report.md#zero-truncated-binomial).

## Poisson face comparison and equality

Let $Q$ be the mean-matched conditional Poisson and $g=E_P(X)_2-E_Q(X)_2$. Write $c_N(k)=\sum_{j<k}\log(1-j/N)$ for the log falling-factorial correction. Differentiating $\ell(s,z(s))$ at zero cancels the rate derivative by the score equation. The derivative of $\log v_k$ there is $-(k)_2/2$, and that of $\log Z$ is its $Q$ expectation. In the lower-only formula this also follows by differentiating the full binomial normalization at zero and subtracting the omitted terms. Hence
$$
\frac{\ell_N-\ell_\infty}{n}=\frac{A}{N}+O(N^{-2}),\qquad A=-g/2.
$$
A negative $g$ guarantees a finite fit above the face. A positive $g$ guarantees that every sufficiently large size is below the face. The positive-gap case still needs a finite-prefix comparison in general. On $[0,4]$ the exact histogram $(10400,0,1,60798,11201)$ has $\lambda=4$ and $g=1/41200$, yet size 64 exceeds the face by about $1.28340114$ total log-likelihood units. Moving two observations of 3 to 2 and 4 preserves the mean and increases the second factorial-moment sum by 2 from the scaled zero-gap example. The original $(104,0,0,608,112)$ has $g=0$ and size 64 exceeds the face by about $0.01299662$. At $g=0$ the empirical Poisson histogram $(48,48,24,8,2)$ instead gives a strictly winning rate-one face by the multinomial likelihood bound. A finite binomial on this window cannot reproduce its four successive Poisson weight ratios. [Interval verification](2026-11-25-tbinom-math-data/check.py), [existing signed and higher-order bounds](2026-11-07-binom-faces-report.md).

**Proved for $U=\infty$ and $L\ge1$.** The shifted Poisson sums give
$$
(\mu-\lambda)Z(0,\lambda)=\frac{\lambda^L}{(L-1)!},\qquad
E_Q(X)_2=\lambda(\mu-L+1)+(L-1)\mu.
$$
The first identity expresses $e^\lambda$ as a rational function of $\lambda$ over the rationals, since $Z(0,\lambda)=e^\lambda-\sum_{k<L}\lambda^k/k!$ and $\mu$ is rational. If the positive $\lambda$ were algebraic, the Hermite–Lindemann theorem would make $e^\lambda$ transcendental, a contradiction. The second identity then excludes $g=0$, which would make $\lambda$ rational. In particular the sign can be decided by comparing $\lambda$ with the rational threshold $(E_P(X)_2-(L-1)\mu)/(\mu-L+1)$ using directed rate brackets.

Nor can a finite maximized likelihood equal the face on this unbounded window. Each finite optimum has algebraic odds by its polynomial score, so its ordinary likelihood is algebraic. At the face the first displayed identity gives ordinary likelihood $C\lambda^{S-nL}(\mu-\lambda)^n$ with $C=((L-1)!)^n/\prod_k(k!)^{c_k}>0$ rational. This is a nonconstant rational-coefficient polynomial in $\lambda$, so its value is transcendental. For $L=0,U=\infty$ the ordinary-binomial criterion is $g=\operatorname{Var}_P(X)-\mu\ge0$ for escape, including equality. The ordinary Poisson likelihood is transcendental too, since its exponential factor is $e^{-S}$ with positive integer $S$. [Hermite–Lindemann proof](https://arxiv.org/abs/2306.14352), [ordinary-binomial criterion](https://www.tandfonline.com/doi/abs/10.1080/01621459.1983.10477949).

For bounded windows the first coefficient can vanish, and finite-to-face equality is an algebraic comparison. The flat two-count case is an attained equality. More generally the decision rule below treats a prefix tie with the face as an attained finite maximum and retains all finite co-maximizers. It deals separately with equality of a Taylor coefficient and equality of likelihoods.

## A finite decision certificate

**Proved for bounded windows, including higher-order cancellation.** On $0\le s\le 1/(2U)$ write the shifted normalizer $W(s,z)=\sum_{k=L}^U v_k(s)z^{k-L}$ and score polynomial $H=\sum_{k=L}^U(nk-S)v_k(s)z^{k-L}$. Multiplying the derivative of the profiled total log-likelihood in $s$ by $D(s)W(s,z)$, with $D=\prod_{1\le j<U}(1-js)>0$, gives the polynomial
$$
R(s,z)=W(s,z)\sum_k c_k D(s)\frac{v'_k(s)}{v_k(s)}
       -nD(s)W_s(s,z).
$$
Use the irreducible factor $H_*$ of $H$ vanishing at $(0,\lambda)$. Exactly one factor vanishes there because the score root is simple. Its zero branch is the profiled root throughout the small interval. To see this, the root cannot switch factors without two factors vanishing at a simple score root. If $H_*$ divides $R$ over $\mathbb Q(s)$, the branch is flat. Otherwise $\operatorname{Res}_z(H_*,R)$ is a nonzero polynomial. Strip its zero at the origin and choose a positive rational cutoff below the first positive root and at most $1/(2U)$. On the resulting positive interval the derivative has constant strict sign. Its sign at one reciprocal integer follows from rational root isolation of $H$ and $R$. Taking the reciprocal of the cutoff and rounding up gives an integer tail threshold.

**Proved for lower-only windows.** The nonzero $A$ gives a sign neighborhood of $s=0$. To enclose it, take a compact positive rate bracket around $\lambda$ with opposite endpoint score signs. Enclose the score at those endpoints and the partial $s$ derivative of the log-likelihood over the whole rectangle $[0,1/B]$ times the rate bracket. In the normalizer, write $\log(1+sz)/s=zF(sz)$ where $F(u)=\sum_{j\ge0}(-u)^j/(j+1)$. With $|u|\le r<1$, truncating after degree $d$ gives remainder bounds $r^{d+1}/(1-r)$ for $F$ and $r^d/(1-r)$ for $F'$. The other terms are finite products. Sufficiently fine rate brackets, interval arithmetic precision and a sufficiently large $B$ separate the signs by continuity. At each reciprocal integer the rate bracket contains the global probability optimum. A strict partial derivative bound valid for every rate in the bracket orders the profiled values as well, by taking the maximum over the same bracket at both arguments. This proves a tail orientation uniform across all the larger integer sizes.

The tail orientation gives a finite necessary and sufficient comparison.

- If the profile decreases with $N$ on $N\ge B$, compare all $M\le N\le B$. Its value at $B$ already exceeds the face and excludes every larger size.
- If it increases with $N$ on $N\ge B$, compare $M\le N<B$ with the face. All larger sizes are strictly below the face. Escape occurs exactly when every prefix value is strictly below it.
- If the tail is flat, compare $M\le N<B$ with its common value $\ell_\infty$. Include all sizes in the tail in the maximizing set when this value wins or ties.

At a finite size the shifted odds score has integer coefficients $(nk-S)\binom Nk$. Rational bisection encloses the only positive root. Intervals of the probabilities then enclose the total log-likelihood. To settle a genuine equality, compare the ordinary likelihoods as positive algebraic numbers, since each is a rational function of its isolated odds root. A bounded-window face has the same algebraic representation with weights $1/k!$. Algebraic arithmetic and real root isolation decide equality exactly. On a lower-only window, finite-to-face intervals eventually separate by the transcendence proof. Finite-to-finite ties use algebraic comparison on either window.

**Proved shortcut for a strict finite witness.** Retain $L,\ldots,K$ with $K\ge M$ and $K\le U$, and let $Q_K$ be its mean-matched Poisson. Lower-bound the binomial normalizer using only the retained terms and apply Jensen with $Q_K$. The common mean cancels the free log-rate term. For $N\ge2K$, the inequality $0\le-\log(1-y)-y\le y^2$ at $y=j/N$ yields
$$
\ell_N \le \ell_{\infty,K}
 + n\left(\frac{\max(0,(E_{Q_K}(X)_2-E_P(X)_2)/2)}{N}
 +\frac{E_{Q_K}(X)_3/3+E_{Q_K}(X)_2/2}{N^2}\right).
$$
This combines the retained-support method with the signed factorial remainder from the earlier face proof. The right side decreases with $N$. If it is below a finite witness at $B$, enumerate only through $B-1$. As $K$ increases on an unbounded window, $\ell_{\infty,K}\to\ell_\infty$ by the same mean-root bracketing argument using convergent Poisson sums. Thus a strict finite witness always permits such a cutoff in principle. [Prior retained-support comparison](2026-10-31-size-search-report.md), [signed Jensen proof](2026-11-07-binom-faces-report.md#conditional-poisson-tail-proof).

## Workstation Python checks

The final runs all finished successfully using one workstation slot each through the CPU wrapper. The interval comparisons use 65-decimal-digit mpmath interval arithmetic and 100 dyadic bisections with exact integer score signs for finite fits. The lower-only tail uses a 55-step face rate bracket and a degree-24 series with explicit remainder. Displayed values below are rounded, while the comparisons use interval endpoints.

| Golden input | Free-size winner | Fitted $p$ | Total log-likelihood | Evaluated sizes | Upper bound for all larger sizes | Prefix runner-up gap |
|---|---:|---:|---:|---|---:|---:|
| tbinom, $[1,10]$ | 13 | 0.369680581443 | −965.8595899796 | 10–79 | −979.1063462186 | 0.07085542 |
| ztbinom, $[1,\infty)$ | 8 | 0.409882080603 | −851.1963765337 | 8–27 | −855.3023679800 | 0.30273163 |

The tbinom golden holds size 12 in its stored fit. Releasing size for this comparison gives 13. The ztbinom golden already frees size. The retained support for its cutoff was $[1,14]$. The slower analytic tail separately certified a decreasing tail at $N\ge2048$ on that golden. For the exact zero-truncated sample $(1,4)$ it certified an increasing tail at $N\ge1024$, and every size 4–1023 was strictly below the face, at rate approximately $2.2316118840$. The bounded resultant verified both signs at exact first-order cancellation. On the prior sample $(242187,60672,121344,1488980,406274)$ with exact cancellation through order two it certifies that $\ell_N>\ell_\infty$ for all $N\ge15$. It also detects the flat two-count branch. [Golden and shape checks](2026-11-25-tbinom-math-data/check.py), [tail checks](2026-11-25-tbinom-math-data/tail.py).

**Numerical lower-only screens.** At fixed sample mean, a successive profile difference is affine in the empirical frequencies. The screen took the convex hull of its values at the vertices of the fixed-mean histogram polytope. Each vertex is supported on at most two atoms. For each pair of size steps it tested whether a negative step followed by a positive step was feasible. Binary64 screening at a $10^{-11}$ threshold found no reversals in 1,495 settings, using $L=1,2,3,5,10$, maxima $L+2$ through $L+24$, 13 relative means, and 12 step offsets between 0 and 105 above each maximum. [Screen and grid](2026-11-25-tbinom-math-data/search.py).

**Proved reduction for a possible zero-truncated moment-only criterion.** For $g\ge0$, reweight the empirical law by $(X-1)/(\mu-1)$. Its mean is at least $\nu=\lambda\mu/(\mu-1)$. On $2,\ldots,N$, $c_N(k)/(k-1)$ has a decreasing, concave linear interpolant $G_N$. Indeed it is the average of the first $k-1$ terms of the decreasing concave sequence $h_j=\log(1-j/N)$. For the prefix averages $a_m$, $2(a_{m+1}-a_m)$ is the weighted average of $h_{j+1}-h_j$, $1\le j\le m$, with weights proportional to $j$. These averages decrease as $m$ grows. Jensen then bounds $E_P c_N$ by $(\mu-1)G_N(\nu)$ whenever $\nu\le N$. If $\nu>N$, such a sample with $g\ge0$ is infeasible at size $N$. The rest of $\ell_N-\ell_\infty$ depends only on $\mu$ and $N$. Numerically, the bound gave no positive excess in the feasible part of a 1,751-mean binary64 grid from 1.001 to 14, taking ten sizes starting at $\lfloor\nu\rfloor+1$. The 70-digit cross-check used 13 means and the corresponding 130 sizes. Its largest per-observation excess was approximately $-2.93\times10^{-9}$. [Grid and high-precision cross-check](2026-11-25-tbinom-math-data/positive_boundary.py).

To reproduce from the worktree root, with the temporary directory configured as for other workstation jobs:

    data=dev/research/evaluations/2026-11-25-tbinom-math-data
    /tmp/umvl-cpu/run python3 -B "$data/check.py"
    /tmp/umvl-cpu/run python3 -B "$data/tail.py"
    /tmp/umvl-cpu/run python3 -B "$data/search.py"
    /tmp/umvl-cpu/run python3 -B "$data/positive_boundary.py"

## What this changes for the fitter

- Keep integer-prefix coverage for broad truncation windows. A first descent can miss a second strict local maximum. A first descent is sufficient on a three-count window because the transformed profile is strictly concave. A two-count window has an attained, nonunique optimum. The convex-order test against $Q_M$ is sufficient for a certified first-size winner on wider windows.
- For a strict finite winner, combine the existing retained-support certificate with the signed remainder and cover every smaller size or block. For a proposed escape, require a signed tail or an equivalent analytic bound and exclude the entire prefix. A positive first gap alone cannot give TpoisLimit or PoissonLimit. A negative gap refutes a face proposal. For a nonconstant lower-only sample there is no exact first-order cancellation or finite-to-face tie.
- For bounded-window ambiguity, polynomial factor selection and root isolation give an exact reference oracle even at higher-order cancellation. An exact finite tie remains an attained fit with nonuniqueness information. An interval overlap remains an unresolved comparison until the sign or equality is established. Under the existing fallback rule, a successful, accuracy-checked and unrefuted search result can be returned with certification withdrawn when certificate work runs out. A refuted candidate or a failed search requires the corresponding stop or a corrected candidate.

## Open

1. **Integer unimodality for zero truncation and other lower-only windows.** The missing step is a uniform ordering of successive-difference signs, or an exact histogram with an integer decline followed by a rise. The continuous counterexample in the literature, the eventual log-convexity proof, and the finite screens leave that question open.
2. **A moment-only global face test for pure zero truncation.** For $g\ge0$, proving the upper bound based on $G_N(\nu)$ is strictly below the face for every feasible $(N,\mu)$ would replace the prefix comparisons in this case. The numerical grid covers finitely many sizes and means. The signed tail plus prefix already gives a complete per-input decision.
3. **Production cost and ties.** The symbolic cutoff can be impractical for wide windows, and the prototype interval code has fixed working precision. Production use needs adaptive precision or explicit margins, tests for very small cutoff gaps and unrepresentable sizes, and a representation for exact size ties or a flat maximizing tail. The mathematical finite comparison gives no uniform bound on their computational cost.
