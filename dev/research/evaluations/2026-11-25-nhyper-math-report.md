# Negative hypergeometric population-size MLE under inverse sampling

TASK-325 · 2026-11-25 · worktree base `a2f9a313`

> **Correction (2026-11-26).** The singleton-or-adjacent-pair conclusion below
> applies to nonflat scalar likelihoods that attain their supremum. With a
> positive held black count and every observation at the earliest possible
> draw, the likelihood is nonflat and the maximizing set is empty. With a
> held black count of zero and those same observations, the likelihood is
> flat on the feasible ray. [Lean verification](2026-11-25-lean-nhyper-report.md#source-check)

**Proved.** With a positive stopping count and the white count held, every feasible sample has a finite integer MLE. With the black count held, the only escape is when the urn has black balls but all observations stop at the earliest possible draw. In both nonflat scalar problems the likelihood ratio crosses one at most once, and the full maximizing set is a singleton or two consecutive integers. With both counts unknown there is a finite exact test of attainment, equality with the negative-binomial face, and strict escape. The joint profile in population size can have multiple local peaks.

The [October nhyper report](2026-10-29-nhyper-report.md) establishes the stopped-draw density and the translated negative-binomial limit. The current [nhyper dossier](../../../univariateML-cpp/include/cppunivariate/distributions/nhyper/README.md) already proves the single crossing with the white count held and documents the growing-urn upper bound. Those routes postdate the wording of the [STATUS item](../../STATUS.md). The [hyper dossier](../../../univariateML-cpp/include/cppunivariate/distributions/hyper/README.md), [held-size certificates](2026-11-01-hyper-certs-report.md), and [fully free hyper analysis](2026-11-01-hyper-free-report.md) establish the upper-envelope approach used here when a finite witness beats the face. The new joint argument supplies a tail comparison even when no finite witness above the face has been found.

## Design and feasibility

Each of $h\ge1$ independent observations starts from the restored urn. Let $m$ and $n$ be its white and black counts, $N=m+n$, and let $r$ be the held number of whites required to stop. For $r>0$, write $y_i=T_i-r$, $Y=\max_i y_i$, $X=r+Y$, and $S=\sum_i y_i$. Feasibility requires integer $T_i\ge r$, $m\ge r$ and $n\ge Y$. The report uses the unbounded integer model. A fitted domain explicitly truncated at a count bound has a separate endpoint problem.

Write $(a)_k=\prod_{j=0}^{k-1}(a-j)$. The density from the October report in falling-factorial form is
$$
\Pr(T=t)=\binom{t-1}{r-1}\frac{(m)_r(n)_{t-r}}{(N)_t}.
$$
For comparisons at held $r$, the product of the binomial coefficients can be dropped. It is restored when reporting likelihoods or comparing different stopping counts. Denote the reduced likelihood by $L(m,n)$. All feasible finite points have positive likelihood. If $r=0$, only $T_i=0$ is feasible, and every admissible urn consistent with the holds maximizes at likelihood one.

## One count held

### White count held

**Proved in the nhyper dossier.** For fixed $m\ge r>0$, the feasible line starts at $N_{\min}=m+Y$. With $A=N+1$, the ratio for a unit increase in population is
$$
R_W(N)=\prod_i\frac{(A-m)(A-T_i)}{A(A-m-y_i)}.
$$
For $m=r$, this equals $((A-r)/A)^h<1$. For $m>r$, the dossier's single-crossing argument applies. In its derivative comparison, the later derivative relative to the earlier one strictly decreases in $T$. The integral comparison preserves a nonpositive log ratio, and moving the zero strictly to the right makes the later log ratio strictly negative. Each zero stays in the domain of its own log factor for feasible $A$ since $A>m$ and $m>r$. Each factor is at most one when $A\ge mX/r$. Thus the first $N\ge N_{\min}$ with $R_W(N)\le1$ is bracketed by
$$
N_{\min}\le N\le\max\{N_{\min},\lceil mX/r\rceil-1\}.
$$
It is the smaller maximizer. Include $N+1$ exactly when $R_W(N)=1$. The equality test is equality of the two integer products in the displayed ratio. The limit at fixed $m$ has zero likelihood on any finite sample because $(m)_r$ is constant and each mass is $O(N^{-r})$.

For a constant sample $T_i=t$, the sign is that of $mt-r(N+1)$. The smaller and larger maximizers are $\max(N_{\min},\lceil mt/r\rceil-1)$ and $\max(N_{\min},\lfloor mt/r\rfloor)$. This includes a single observation. For $r=2$, $m=5$, $T=(2,5)$, the maximizing populations are exactly $9,10$. At $N=9$ the two factors are $4/5$ and $5/4$.

### Black count held

**Proved.** For fixed $n\ge Y$ and $r>0$, use $m\ge r$ as the search coordinate, with $N=m+n$. At $n=0$ the feasible sample has every $T_i=r$, and the likelihood is one throughout. For $n>0$, put $u=m+1-r$ and $a_i=n-y_i$. The ratio for incrementing $m$, and hence $N$, is
$$
R_B(m)
=\prod_i\frac{(m+1)(m+n+1-T_i)}{(m+1-r)(m+n+1)}
=\prod_i\frac{1+a_i/u}{1+n/(u+r)}.
$$
Its logarithm is
$$
\log R_B(m)=\int_0^{r+n}\frac{H(z)}{u+z}\,dz,\qquad
H(z)=\sum_i\mathbf{1}_{0<z<a_i}-h\,\mathbf{1}_{r<z<r+n}.
$$
Here $H\ge0$ below $r$ and $H\le0$ above $r$. There is a negative interval of positive length, since all $a_i\le n$ and $r,n>0$. For $v>u$ the factor $(u+z)/(v+z)$ is strictly increasing in $z$. Comparing it to its value at $r$ on both intervals gives
$$
\int_0^{r+n}\frac{H(z)}{v+z}\,dz
<\frac{u+r}{v+r}\int_0^{r+n}\frac{H(z)}{u+z}\,dz .
$$
Consequently a nonpositive log ratio is followed by strictly negative log ratios.

If $S>0$, set $b=S/h$. The geometric mean of $u+a_i$ is at most $u+n-b$, so $R_B(m)\le1$ by $m+1\ge nr/b$. In fact the comparison after multiplication by the positive denominators has sign bounded by the sign of $nr-b(m+1)$. The first $m$ with $R_B(m)\le1$ is therefore found by bisection in
$$
[r, \max\{r,\lceil nr/b\rceil-1\}].
$$
It is the smaller maximizer, and $m+1$ is a second maximizer exactly when the integer numerator and denominator products agree. For all $y_i=y>0$ the factors have sign relative to one given by $nr-y(m+1)$, so the extreme maximizers in $m$ are $\max(r,\lceil nr/y\rceil-1)$ and $\max(r,\lfloor nr/y\rfloor)$. Add $n$ to convert to $N$. For $r=1$, $n=10$, $T=(3,8)$, the factors at $m=1$ are $3/2,2/3$, giving maximizing populations $11,12$.

If $S=0$ and $n>0$, all factors of $R_B$ exceed one. The distribution tends to the point mass at $r$ as $m\to\infty$ and the likelihood increases strictly to one. Hence there is no finite MLE, even at equality $T_i=r$ for every observation. This is the $p=1$ endpoint of the shifted negative binomial. With $S>0$ this endpoint instead has zero likelihood. The case $n=0$ attains the endpoint for every feasible $m$.

### What the floor formulas depend on

For general repeated samples the relevant sign is the sign of a difference of integer products, or equivalently of a polynomial evaluated at an integer. The polynomial test and the single crossing give an exact computable threshold even if the polynomial has other roots outside the relevant crossing. A Lincoln–Petersen-style rule using the sample mean, even supplemented by sample size and maximum, cannot do this in general. The following exact counterexamples were found by [exhaustive rational comparisons](2026-11-25-nhyper-math-data/explore.py). The bounds and the single-crossing proofs make the displayed results global.

| Hold and stopping count | Total draws $T$ | Maximizing $N$ |
|---|---|---:|
| $m=2, r=1$ | $1,3,3$ | 5 |
| $m=2, r=1$ | $2,2,3$ | 4 |
| $n=2, r=1$ | $1,1,1,3,3$ | 3 |
| $n=2, r=1$ | $1,1,2,2,3$ | 4 |

## Both counts unknown

### Faces and attained constants

**Proved.** If $S=0$ and $r>0$, the maximizing set is precisely $n=0, m\ge r$. For a constant $T_i=t>r$, the unique maximum is $N=t, m=r$. Indeed the earlier identity gives $\Pr(T=t)=(r/t)\Pr(H_t=r)\le r/t$, and the proposed urn attains equality. At $N>t$, a supported urn contains both colors. An arrangement with $r$ whites inside the first $t$ positions permits exchanging a white and a black across the boundary to alter that count. Otherwise both colors would be forced to agree across all pairs spanning the boundary, contrary to the presence of both. Hence $\Pr(H_t=r)<1$ there.

As $N\to\infty$, take subsequences on which $m/N\to p$. The falling-factorial formula gives pointwise convergence at every finite $t$ to the mass of $r+\operatorname{NB}(r,p)$ for $0<p<1$, and to a point mass at $r$ for $p=1$. At $p=0$ every finite draw count has limiting mass zero and the draws escape to infinity. The limit at fixed white count belongs to this last case. Compactness of the proportion interval accounts for every subsequential limit of the sample likelihood. In particular there is no separate binomial face for these stopped draws.

When $S>0$ the largest reduced likelihood among the limits is
$$
F=p^{hr}(1-p)^S,\qquad p=\frac{hr}{hr+S}=\frac{r}{\bar T}.
$$
The proportions can converge to this rational $p$ along feasible urns. Its log likelihood including the sample constant is the face value in the nhyper dossier. The existing upper envelope proves finite attainment whenever a finite $C>F$ is found. For example, with $c_N(t)=\sum_{i=0}^{t-1}\log(1-i/N)$, the reduced log likelihood satisfies
$$
\log L\le \log F-\sum_i c_N(T_i),\qquad
-\sum_i c_N(T_i)\le
\frac{\sum_i T_i(T_i-1)}{2(N-X+1)}quad(N\ge X).
$$
The second inequality follows from $-\log(1-z)\le z/(1-z)$ and $N-j\ge N-X+1$. Enumerating every population before the upper bound drops strictly below $\log C$ then finds all finite maxima.

### An exact finite test including equality and escape

**Proved.** The argument concerns $r>0, S>0$. At each fixed $N$, the composition ratio $L(m+1,N-m-1)/L(m,N-m)$ strictly decreases across feasible adjacent pairs by the October report. Its crossing identifies the entire slice maximum. The following construction localizes this crossing to at most two relevant compositions for each large $N$.

For a real proportion $w$ near $p$, define
$$
\begin{aligned}
f(w)&=hr\log w+S\log(1-w),\\
G(w)&=\frac{1}{2}\sum_i\left[T_i(T_i-1)
 -\frac{r(r-1)}{w}-\frac{y_i(y_i-1)}{1-w}\right],\\
I&=-f''(p)=hr/p^2+S/(1-p)^2>0.
\end{aligned}
$$
For bounded $z$, substitute $m=pN+z$ and $w=p+z/N$. Expand the logarithm of each falling-factorial factor in $1/N$. Since these are finite products and $p\in(0,1)$, the coefficients are smooth in $w$ near $p$, with a uniform remainder of order $N^{-3}$ when expanded through order two. Writing the coefficient of $N^{-2}$ at fixed $w$ as $J(w)$, the result at this substitution is
$$
\log L(pN+z,(1-p)N-z)
=f(p)+\frac{G(p)}{N}
+\frac{J(p)+G'(p)z-Iz^2/2}{N^2}
+O(N^{-3}).
$$
Here $f'(p)=0$, and the derivative $G'(p)$ is rational.
$$
G'(p)=\frac{1}{2}\left[\frac{hr(r-1)}{p^2}
 -\frac{\sum_i y_i(y_i-1)}{(1-p)^2}\right].
$$
Subtracting the expansions for $z+1$ and $z$ shows that the leading composition log-ratio coefficient at order $N^{-2}$ is $G'(p)-I(z+1/2)$.

Reduce $p=a/d$ to lowest terms. In each residue $e$ of $N$ modulo $d$, the fraction $s_e=\{pe\}$ is fixed. Write $m=\lfloor pN\rfloor+j$ and put
$$
k_e=\left\lfloor G'(p)/I+s_e-\tfrac{1}{2}\right\rfloor .
$$
At $j=k_e-1$ the leading ratio coefficient is strictly positive, and at $j=k_e+1$ it is strictly negative. For sufficiently large $N$, both ratios exist and have their stated signs. The decreasing composition ratio then forces every maximizing $m$ at that $N$ to have $j=k_e$ or $k_e+1$. This includes the case where the leading coefficient is exactly zero at $j=k_e$.

To compute a finite cutoff and compare to the face, use polynomials in $N$. For each offset $j$ set $c=d(j-s_e)\in\mathbb{Z}$, so $dm=aN+c$, $dn=(d-a)N-c$. At the two bracketing offsets use the composition-ratio sign polynomial
$$
(aN+c+d)^h\prod_i((d-a)N-c-dy_i)
 -(aN+c+d(1-r))^h((d-a)N-c)^h.
$$
At each of the two possible maximizing offsets use the face difference polynomial
$$
\begin{aligned}
P_{e,j}(N)={}&
\left[\prod_{i=0}^{r-1}(aN+c-di)\right]^h
 \prod_{\ell=1}^h\prod_{i=0}^{y_\ell-1}((d-a)N-c-di)\\
 &-a^{hr}(d-a)^S\prod_{\ell=1}^h (N)_{T_\ell}.
\end{aligned}
$$
Its sign is the sign of $L-F$ because the cleared denominator is positive on the feasible tail.

Every face difference polynomial is nonzero. If the two products were proportional, their roots with multiplicity would agree. Since $r>0, S>0$, the first product has both roots $-c/a$ and $c/(d-a)$. For $c\ne0$, one is negative, while all roots of the second product are nonnegative. For $c=0$, the multiplicity at zero of the first product is $h+\#\{i:y_i>0\}$, compared with $h$ for the second.

Choose $N_0$ large enough for feasibility of the bracketing ratios and strictly beyond all real roots of the nonzero polynomials just listed. One exact option is an integer above the maximum of their Cauchy bounds $1+\max_{i<D}|b_i/b_D|$ for polynomials $\sum_{i=0}^D b_iN^i$ of positive degree. For $|N|$ exceeding that bound the geometric-series bound on lower powers makes their sum smaller than the leading term. Nonzero constant polynomials have a fixed sign. The scripts use a second exact option. Double an integer $M$ until the coefficients after substituting $N=M+u$ all have the leading sign or are zero. This terminates because each coefficient is a multiple of a derivative at $M$, and each nonzero derivative eventually has the leading sign. Taking $N_0=M+1$ ensures strict sign for all $N\ge N_0$. The bracketing polynomials have their prescribed leading signs by the expansion. Thus for each residue the sign of the population profile minus $F$ on the tail is the larger of the two face-polynomial leading signs.

Let $E$ be the largest exact reduced likelihood in the finite prefix $X\le N<N_0$, taking $E=0$ for an empty prefix. Compute $E$ by taking the slice maxima. Then the following test is necessary and sufficient.

| Exact comparison | Outcome in the unbounded model |
|---|---|
| $E>F$ or any tail face polynomial has positive leading sign | A finite MLE exists and its likelihood strictly exceeds the face. A positive tail sign supplies a witness by taking the first population in that residue at or above $N_0$. Use the upper envelope and the witness to finish globally. |
| All tail face polynomials have negative leading sign and $E=F$ | A finite MLE exists, with the same likelihood as the face. All finite maximizers occur in the prefix. |
| All tail face polynomials have negative leading sign and $E<F$ | No finite MLE. The supremum is the shifted negative-binomial face. |

Every joint fit with $S>0$ that attains a maximum therefore has finitely many maximizing pairs. Exact ties within a slice require equality of the integer composition ratio at its first nonpositive comparison. To compare different slices, cross-multiply their reduced likelihoods and retain every vector tied at the established global level. The face equality test compares rational numbers at that same level and does not add an infinite population to a finite maximizer set.

### The first-order diagnostic and counterexamples

Let $b=S/h>0$ and $V=h^{-1}\sum_i(y_i-b)^2$. Substitution in the coefficient from the dossier gives $G(p)=h(r+b-rV/b)/2$. Thus $V<b(r+b)/r$ implies a finite witness above the face. A negative first-order coefficient need not imply escape. For $r=1$ and $T$ containing twenty-five ones and twelve fours, $G(p)=-1$ but the finite urn $(m,N)=(2,5)$ strictly exceeds $F$ by exact rational comparison.

The zero first-order case also gives both outcomes. For $r=1, T=(1,1,1,3)$, one has $p=2/3$ and $G(p)=0$. The two candidate offsets in every residue are $-1,0$. In the polynomial test the lower bracketing offsets give $c\in\{-8,-7,-6\}$ and the upper ones $c\in\{-2,-1,0\}$. The bracketing polynomials shifted by $N=9+u$ have respectively nonnegative and nonpositive coefficients with a nonzero leading coefficient. The six face polynomials for $c=-5,-4,-3,-2,-1,0$ shifted by the same amount have nonpositive coefficients with negative leading coefficients. This gives a strict tail comparison from $N=10$ onward. Direct rational evaluation for $3\le N<10$ gives $E/F=455625/458752<1$, proving escape.

For $r=1, T=(1,1,4)$, one again has $G(p)=0$. Maximizing over the feasible compositions at each of $N=4,5,6,7$ gives reduced profile divided by $F$ of $1, 128/125, 128/135, 1728/1715$, respectively. These are rational evaluations of the displayed mass formula. The equality at $N=4$ is an exact finite match to the face, while $N=5$ beats it. This proves finite attainment and disproves unimodality in $N$ for the joint profile.

## Reproducible checks

Both scripts finished successfully on the workstation, sequentially under the one-slot wrapper. The wrapper pins BLAS and OpenMP threads and applies niceness. There were no builds or library tests in this math-only job.

```sh
study=dev/research/evaluations/2026-11-25-nhyper-math-data
mkdir -p build/tmp
export TMPDIR="$PWD/build/tmp" PYTHONDONTWRITEBYTECODE=1
/tmp/umvl-cpu/run python3 "$study/explore.py"
/tmp/umvl-cpu/run python3 "$study/check.py"
```

- **Exact arithmetic.** [The exploratory script](2026-11-25-nhyper-math-data/explore.py) finds the two nonconstant scalar ties and the same-summary counterexamples. [The checker](2026-11-25-nhyper-math-data/check.py) compares independent hypergeometric-combination masses to adjacent ratios on 3,762 held-white and 2,523 held-black lines. The grid uses $r=1,2,3$, sample lengths 1–4 and black draws 0–5. Held whites range from $r$ to 7 and held blacks from $Y$ to 7 with $S>0$ on these black lines. The white lines cover feasible $n<45$. The black lines cover feasible $m<\max(45,\lfloor nr/b\rfloor+3)$. It checks crossings and all maximizing integers on each line. The exact joint test runs six samples at $r=1$, including the examples above and a second escape at $T=(1,1,1,6)$. The computed cutoffs range from 9 to 65. The checker enumerates every composition in each finite prefix and verifies the tail candidate formula against a full slice at the first tail population of each residue.
- **Numerical.** The checker evaluates both asymptotic coefficients with 80-digit mpmath log falling-factorials. All 150 seed-325 trials pass at absolute tolerance $10^{-13}$ for the scaled residuals. The grid uses $1\le r\le12$, 2–10 observations with $0\le y_i\le25$, $10^{24}\le N<10^{24}+1000$, and offsets within 30 integers of $\lfloor pN\rfloor$. This verifies the expansions numerically alongside their derivation.

## What this changes for the fitter

- **Hold the scientific design explicitly.** The held-white route can reuse the current search. A held-black route constrains $N-m=n$ and requires a public or internal way to pass that hold under rule C6. Changing the color labels also changes what observation stops sampling. Keep the current $(m,N,r)$ density signature. Invalid counts and empty support intersections are input failures. Apply the flat $r=0$ case before positive-$r$ search.
- **Certify scalar results by the two neighbor signs and support.** Use the exact brackets above, then distinguish a strict crossing, an attained boundary, and exact integer-product equality. Compensated log ratios with the existing practical sign margin can certify separated comparisons conditional on those signs. For ambiguous signs use exact comparison where budget permits, with factor cancellation or bounded integer products. A rounded zero alone supplies two comparison candidates and unknown uniqueness. A proved adjacent tie returns two exact `Points` in $(m,N,r)$ and the deterministic smaller-$N$ representative. With $n$ held, both $m$ and $N$ increase in that pair. For feasible $r=0$ data or a held black count of zero with $T_i=r$, return the full hold-sliced integer maximizing set and determine uniqueness after domain slicing. A held $n>0$ at $r=0$ requires the affine set $N-m=n$ in public coordinates, or a coordinate-aware representation of that set.
- **Use a limit outcome for proved nonattainment.** At $n>0, S=0, r>0$ with black count held, return the point mass at $r$, represented by the existing shifted-negative-binomial payload if it accepts $p=1$. In the joint third row of the test, return the shifted negative binomial with shift and size $r$ and probability $r/\bar T$. For either escape, the R referral is `mlnbinom(x - r, size = r)`. The joint equality row returns the complete finite maximizing set, with face equality in the comparison details. These are sample-dependent outcomes under R6 and belong in `limits` or its accompanying prose under A7. A computational representability cap cannot prove escape or certify a capped value for the unbounded model. For a deliberately truncated fitted domain, apply the comparisons on that domain and return its maximizing boundary if appropriate.
- **Use the tail test as a certificate over the existing joint search.** A strict finite witness and the old envelope remain the simpler attainment route. When they do not settle the comparison, the residue cutoff can exclude all large populations or generate a finite witness. Certify the finite result only after every remaining population and possible tie is covered. Return all globally tied integer vectors as exact `Points` when representable. If comparison signs overlap or the certificate allowance expires on ordinary data, keep a successful search estimate with `certified = FALSE` and expose unknown uniqueness when applicable. Stop if a separated feasible or reachable-limit comparison refutes that estimate and the search cannot supply a sound replacement, or if the search itself fails, carrying the actual reason. A proved limit uses the limit outcome. Fitters should charge search and certificate work to their documented `iterlim`-scaled budgets.

## Open

- The exact joint criterion can be expensive. It uses up to $d$ residue classes and polynomials whose degrees grow with the sum of draw counts. A practical implementation needs efficient sign bounds or grouping of residue classes, a comparison budget, and safe arithmetic at large counts. A cheaper global criterion in a few sample summaries remains open. The negative and zero first-order examples constrain such a criterion.
- A practical nonconstant fixture with an attained global equality $E=F$ would exercise that exact branch. The exhibited exact finite match is superseded at a different population. No claim is made here that global equality is possible or impossible for nonconstant data.
- There is no result here bounding the number of joint maximizing pairs by the current typed-set capacity. An implementation needs a capacity proof or a representation that can preserve every tied point. The held-black affine flat set also needs a representation decision if the public coordinates stay $(m,N,r)$.
