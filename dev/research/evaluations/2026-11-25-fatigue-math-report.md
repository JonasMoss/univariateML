# Fatigue profile root counts, value bounds and threshold faces

TASK-290 · Source baseline `ba3aca131ca165bdc10e1043d473bf289e8533f9`

With threshold held and shape free, **every nonconstant sample has exactly one scale score root and a unique joint MLE**. With shape held, there can be two strict local maxima. Two turning-point signs decide their number, and a single curvature test decides when a reciprocal sample has two exactly tied global maxima. A Taylor bound on the actual log likelihood gives a strict comparison for unequal peaks. With scale held and threshold free, the finite boundary case at minimum multiplicity $k=n/3$ reduces to an exact positive polynomial root test.

The [uniqueness-cover audit](2026-11-25-uniq-cover-math-report.md#fatigue) describes the current scaled-score cover and the limitations of its point-value and log-scale-distance comparisons. The [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/fatigue/README.md), [fitter](../../../univariateML-cpp/include/cppunivariate/distributions/fatigue/mle.hpp) and [certificate](../../../univariateML-cpp/include/cppunivariate/distributions/fatigue/certificate.hpp) give the current routes. The [near-constant analysis](2026-11-15-near-constant-report.md) and [offset implementation report](2026-11-16-near-constant-fix-report.md) distinguish the mathematical maximizer from evaluation at the rounded coefficients. The [scalar certificate report](2026-11-07-scalar-trio-report.md#fatigue-complete-rational-score-enumeration) supplies the earlier brackets and exact held-shape counterexamples. The [threshold analysis](2026-10-29-p0-halfthreshold.md#fatigue) supplies the singular paths and boundary limits.

## 1. Fixed threshold, free shape and scale

**Proved.** Let $t_i=x_i-\mu>0$, $A=\overline t$, $H=1/\overline{1/t}$, $b=\beta$, and $\xi(b)=1/\overline{1/(t+b)}$, where bars denote sample averages. Use the known profile $\hat\alpha(b)^2=A/b+b/H-2$ and known sign-equivalent scale score numerator
$$
F(b)=(b-H)^2+H(A-H)-(b-H)\xi(b).
$$
The unique positive root of this equation is also reported by [Ng, Kundu and Balakrishnan](https://www.sciencedirect.com/science/article/pii/S0167947302002542). The following proof gives a strictly decreasing rescaling and a tighter upper bracket.

For nonconstant data $A>H$. Below and at $H$, $F>0$ term by term. Above $H$, divide by $b-H$ and differentiate:
$$
G(b)=\frac{H(A-H)}{b-H}+(b-H)-\xi(b),\qquad
G'(b)=-\frac{H(A-H)}{(b-H)^2}
       +1-\frac{\overline{(t+b)^{-2}}}{\bigl(\overline{(t+b)^{-1}}\bigr)^2}<0.
$$
The moment ratio is at least one by Cauchy–Schwarz. Since $\xi(0)=H$ and $\xi'(b)>1$ for nonconstant data, $\xi(b)>b+H$ for $b>0$. Thus $F((H+A)/2)<0$, giving exactly one simple root $b_*$ in
$$
 H<b_*<(H+A)/2.
$$
The score changes from positive to negative there and has no other zero. For each $b$ the positive $\hat\alpha(b)$ uniquely maximizes in shape, so the joint maximum is unique. This includes arbitrarily concentrated nonconstant samples. The exact constant case retains its known divergence along $b=t_1,\ \alpha\downarrow0$. With scale held, the known RMS formula gives the unique shape unless the RMS is zero.

For sign computation, one can use $F=H(A-H)-(b-H)(\xi(b)-(b-H))$, including at $H$, while using $G$ to prove uniqueness. The identity
$$
 (A-H)/H=\overline{(t-H)^2/(tH)}
$$
shows where to avoid cancellation if an enclosure for $H$ is available. The theorem also applies directly to the offset likelihood. Valid original-data endpoint signs isolate its sole root even when floating-point moments round together.

## 2. Fixed threshold, held shape

### Number of local maxima

**Proved.** Write $a=\alpha>0$. Divide the known scaled score by $n$ and rearrange:
$$
 S(b)= A+a^2 b-b^2/H-2a^2\overline{\frac{bt}{b+t}}.
$$
The actual log-scale score is $nS(b)/(2a^2b)$. In particular $S(0)=A>0,\ S'(0)=-a^2<0,\ S(b)\to-\infty$, and $S'(b)\to-\infty$. The derivatives
$$
 S''(b)=-2/H+4a^2\overline{\frac{t^2}{(b+t)^3}},\qquad
 S'''(b)=-12a^2\overline{\frac{t^2}{(b+t)^4}}<0
$$
give at most three positive roots counted with multiplicity, by Rolle's theorem with multiplicities. The likelihood vanishes in both scale tails at fixed $a$.

The following is a necessary and sufficient sample condition for **two strict local maxima**.

1. Require $S''(0)>0$, equivalently $a^2>1/2$. Let $p$ be the unique positive zero of $S''$. It is bracketed by $0$ and $(2a^2 H\overline{t^2})^{1/3}$.
2. Require $S'(p)>0$. Then there are two zeros $l<p<r$ of $S'$. They can be bracketed on either side of $p$, since $S'(b)<0$ at $a^2H/2$ and beyond.
3. Require $S(l)<0<S(r)$.

Indeed $S''$ is strictly decreasing to $-2/H$. If either of the first two requirements fails, $S$ is strictly decreasing, possibly with zero derivative at one point. Otherwise $S$ decreases, increases, then decreases across $l,r$. The strict signs in step 3 give three simple roots, with the outer two decreasing crossings. In every other case there is one strict local maximum. If $S(l)=0$ or $S(r)=0$, that double score root gives no extremum. A triple score root can occur when $S(p)=S'(p)=0$ and has one decreasing sign change and a flat maximum. For $a\le2$, the known curvature test in log scale gives strict concavity directly, with the possible isolated zero of curvature at a constant sample and $a=2$. [Log-scale concavity in the transfer audit](2026-11-25-transfer-math-report.md)

For any sample, sufficiently large $a$ gives two local maxima. At fixed $b$, $S(b)/a^2$ tends to $b(2b/\xi(b)-1)$. This is negative at a sufficiently small fixed positive $b$ and positive at a sufficiently large fixed $b$. Together with $S(0)>0$ and the negative tail, these signs give three crossings for all sufficiently large $a$.

### Global count and reciprocal samples

**Proved.** With a single local maximum it is the unique global maximum. With two local maxima the global count is two precisely when their values are equal.

Suppose the multiset of $t_i$ is invariant under $t\mapsto c^2/t$ for some $c>0$. This includes all constant samples and all two-observation samples. Set $z_i=\log(t_i/c)$. In $v=\log(b/c)$, the log likelihood, up to a constant, is the sum of even functions of $v-z_i$, namely $\log\cosh((v-z_i)/2)-(\cosh(v-z_i)-1)/a^2$. The sum is even in $v$, and its curvature at zero changes sign at
$$
 a_c^2=4\,\frac{\sum_i \cosh z_i}{\sum_i \operatorname{sech}^2(z_i/2)}.
$$
If $a>a_c$, the center is a strict minimum. The vanishing tails and symmetry give two outer maxima, and the root count gives exactly two, with equal values and scales whose product is $c^2$. If $a<a_c$, the center is a strict maximum and a symmetric additional pair would contradict the bound of two local maxima. At equality the odd analytic score has a zero of order at least three at zero. The order is exactly three by the root count, which excludes all other roots. Thus $a\le a_c$ gives a unique maximum at $b=c$. Constant samples give $a_c=2$, recovering the exact result of the uniqueness-cover audit. For example, the nonconstant sample $(1/2,2)$ gives $a_c^2=45/8$.

**Proved algebraic tie test.** For algebraic $t_i,a^2$, positive score roots are algebraic because the rational score has a nonzero polynomial numerator. The squared likelihood at $b$ is proportional to
$$
 R(b)e^{-nQ(b)/a^2},\qquad
 R(b)=b^{-n}\prod_i(t_i+b)^2,\qquad Q(b)=A/b+b/H-2.
$$
For two distinct maxima $b_1,b_2$, equality holds if and only if $b_1b_2=AH$ and $R(b_1)=R(b_2)$. In fact a nonzero algebraic exponent difference has transcendental exponential by Hermite–Lindemann, whereas the ratio of $R$'s is algebraic. Thus equality forces $Q(b_1)=Q(b_2)$, which for distinct scales is equivalent to the product condition. The two algebraic equalities suffice. This test covers exact binary inputs, including shifted inputs treated as exact differences, and can use polynomial isolation and exact algebraic comparisons. [Hermite–Lindemann formalization](https://isa-afp.org/entries/Hermite_Lindemann.html)

For an exact asymmetric near-constant example take $a=3$, $t=(1,1,1+10^{-12})$. There are two local maxima and exactly one global maximum. For the constant sample at one, $S(1/2)=-3/4$ and $S(2)=3$. Changing the third observation by $10^{-12}$ preserves both signs. For these $b$, each summand $t-b^2/t+a^2b(2b/(b+t)-1)$ has absolute derivative in $t\ge1$ at most $13$. Hence the two signs, together with $S(0)>0$ and the negative tail, force three crossings. To exclude a tie, clear the score denominators with $(b+1)(b+1+10^{-12})$ and call the resulting polynomial $P$. Modulo seven, $AH=3$, and the monic reductions of $P(b)$ and $b^4 P(AH/b)$ are $b^4+2b^3+b^2+1$ and $b^4+2b^2+5b+4$. Their Euclidean algorithm gives $b^2+2b+5$ and $3b+5$ up to units, with $b^2+2b+5\equiv6$ at the root $b=3$ of the latter. Both reductions preserve degree, so the rational polynomials are coprime. Thus the two distinct maxima cannot have product $AH$. The reproducer checks this gcd over the rationals.

## 3. Rigorous upper bound on a bracket's maximum

**Proved in exact arithmetic.** Use $u=\log b$, $z_i=\log t_i$, and put
$$
 J(u)=\sum_i\log\cosh((u-z_i)/2),\qquad Q(u)=A e^{-u}+e^u/H-2.
$$
Here $Q(u)$ uses the log-scale coordinate for the same $Q$ as above. For held shape and free shape respectively the actual log likelihood at fixed $u$, up to constants independent of $u$, is
$$
 \ell_a(u)=J(u)-nQ(u)/(2a^2),\qquad
 \ell_p(u)=J(u)-(n/2)\log Q(u).
$$
The latter includes the effect of optimizing shape at every $u$. The needed derivatives are
$$
 J'(u)=\tfrac12\sum_i\tanh((u-z_i)/2),\quad
 J''(u)=\tfrac14\sum_i\operatorname{sech}^2((u-z_i)/2),\quad
 Q'=e^u/H-Ae^{-u},\quad Q''=Q+2.
$$
On a log-scale bracket $I=[u_L,u_R]$, take $C_I=\frac14\sum_i\operatorname{sech}^2(\operatorname{dist}(z_i,I)/2)$, or simply $C_I=n/4$. It bounds $J''$ from above. Find $q_{\max}$ by taking the larger endpoint $Q$, and $q_{\min}$ by evaluating $Q$ at $\log(AH)/2$ clipped to $I$. Convexity gives these extrema.

- **Held shape.** Use $K=C_I-n(q_{\min}+2)/(2a^2)$ as an upper bound for $\ell_a''$.
- **Free shape, nonconstant sample.** The logarithmic curvature numerator is
  $$
  Q''Q-(Q')^2 = 4(A/H-1)-2Q.
  $$
  Let $D_I=4(A/H-1)-2q_{\max}$. A lower bound for $(\log Q)''$ is $D_I/q_{\max}^2$ when $D_I\ge0$, and $D_I/q_{\min}^2$ when $D_I<0$. Subtract $n/2$ times this lower bound from $C_I$ to get $K$. In particular on $b\in[H,A]$, $q_{\max}\le A/H-1$ and $K=n/4-n/(A/H-1)$ suffices on the entire interval.

Set $m=(u_L+u_R)/2$, $\rho=(u_R-u_L)/2$, $s=|\ell'(m)|$, and
$$
 h=\begin{cases}
 \min(\rho,s/(-K)), & K<0,\\
 \rho, & K\ge0 .
 \end{cases}
$$
Taylor's theorem bounds $\ell(m+d)$ by $\ell(m)+\ell'(m)d+Kd^2/2$. Maximizing this quadratic over $|d|\le\rho$ gives the enclosure
$$
 \ell(m)\ \le\ \max_{I}\ell\ \le\ \ell(m)+sh+Kh^2/2.
$$
This costs one value and one score evaluation, with moment extrema and either the $O(n)$ distance sum or $n/4$. It bounds the value anywhere in the bracket, including the isolated root if the bracket is a signed maximum bracket. With a padded evaluation use a lower value at a feasible midpoint for the lower bound and upper entries for the midpoint value, absolute score and curvature in the upper formula. Padding, score signs and accuracy checks stay tied to the original observations. A common additive constant can be removed from all competitors. For concentrated samples evaluate $Q$ by $4\overline{\sinh^2((u-z_i)/2)}$ in offset coordinates. The determinant can use $A/H-1=Q(\log H)$ with a bounded $H$.

A complete cover with $L_{\mathrm{winner}}>U_j$ for every distinct competitor $j$ proves selection. Two brackets can be identified by a common proved monotone branch or a strict monotonicity proof on their union. Their midpoint separation alone cannot establish identity. The interval $[\sqrt{q_{\min}},\sqrt{q_{\max}}]$ also encloses induced shape throughout a free-shape root bracket, even when the minimum of $Q$ is inside and equal or similar endpoint shape values would understate shape uncertainty.

## 4. Free threshold

The dossier leaves these routes unsupported. Let $\delta=x_{(1)}-\mu>0$, $d_i=x_i-x_{(1)}$, and $k=\#\{i:d_i=0\}$.

- **Both shape and scale free. Proved in P0.** The path $b=\varepsilon,\ \alpha^2=s/\varepsilon,\ \delta=c\varepsilon^2/s$ gives $L\asymp\varepsilon^{-2k}$. Each minimum contributes a divergent density and the remaining observations have positive half-gamma component densities. Unboundedness holds for every nonempty sample, including tied minima. The finite normal face identified in P0 cannot compete with this spike in the unrestricted model. The same is true for the defective half-gamma and inverse-gamma scale faces at a fixed threshold and nonconstant data, where the inward profile signs proved in section 1 establish strict suboptimality. [P0 family analysis](2026-10-29-p0-halfthreshold.md#fatigue)
- **Shape held, scale free. Proved.** For a constant sample take $b=\delta$ as $\delta\downarrow0$, giving density $1/(a\sqrt{2\pi}\delta)$ at each observation. For a nonconstant sample take a positive gap $D=d_j>0$ and put $q_i=t_i/b+b/t_i-2$. The density is $\sqrt{q_i+4}\exp(-q_i/(2a^2))/(2a\sqrt{2\pi}t_i)$. Absorbing the square root into half of the exponential bounds the likelihood by $C_a\delta^{-n}\exp(-\sum_i q_i/(4a^2))$. As $\delta\downarrow0$, $\sum_i q_i\ge b/\delta+D/b-2n\ge 2\sqrt{D/\delta}-2n$, uniformly over scale. As $\delta\to\infty$, the prefactor tends to zero uniformly. For $\delta$ in a compact positive interval, $\sum_i q_i\to\infty$ uniformly in either scale tail. Thus a positive superlevel set is compact and an interior maximum exists.
- **Both shape and scale held. Proved.** At fixed $b$, the same bound vanishes as $\delta\downarrow0$ because the minimum contributes $b/\delta-2$ or more. The $\delta\to\infty$ bound still holds. An interior threshold maximum always exists.

### Scale held, shape free

**Proved.** A constant sample is unbounded by taking $\delta=b$, $\alpha\downarrow0$. For other samples $Q(\delta)=\overline{t_i/b+b/t_i-2}$ is strictly positive. Profiling $\alpha$ gives, up to the positive constant $(8\pi)^{-n}e^{-n}$, the squared maximized likelihood
$$
 W(\delta)=\frac{\prod_i (t_i+b)^2/t_i^3}{(bQ(\delta))^n},\qquad t_i=\delta+d_i.
$$
It tends to zero as $\delta\to\infty$. At $\delta\downarrow0$, $Q(\delta)\sim kb/(n\delta)$ gives the P0 rate $W(\delta)\sim C\delta^{n-3k}$, with $C>0$. Profiling bounds all paths with the same held scale, so this is the boundary envelope. If $3k<n$, a maximum exists by compact superlevel sets. If $3k>n$, the likelihood is unbounded.

For $3k=n$, define $W_0=\lim_{\delta\downarrow0}W(\delta)>0$ and take the numerator $P_0$ of $W-W_0$ after rational cancellation. The denominator has no positive zero, and $W-W_0$ is negative for all sufficiently large $\delta$. A finite MLE exists **if and only if $P_0$ has a strictly positive real root**. A zero is a feasible point attaining the edge value. If the profile ever exceeds the edge, it reaches a maximum in the interior, and continuity forces a zero before the negative tail. With no positive zero the profile stays below the edge everywhere. This also handles an attained tie with the face. For rational inputs, remove any factors at zero and count strictly positive roots by Sturm's theorem. The positive stationary roots of $W$ can then be isolated and compared by algebraic values if the fit is to be computed.

At $b=1$, the P0 examples have the following exact resolutions.

- $(0,1,1)$ has $W_0=432$ and $W(1/2)=2880$, so an interior maximum exists.
- $(0,1/10,1)$ has $W_0=130680$ and no strictly positive root of $W-W_0$. To reproduce the sign count, use $z=10\delta$, $V=(z+10)(z+11)(z+20)$, $B=3z^4-16z^3-209z^2+1710z+1000$. Substitution gives $W=27000000 V^2/B^3$. Use the monic square-free part of $(2200^2B^3-1000^3V^2)/z$ in the Sturm recurrence (start with the polynomial and its derivative, then use negative remainders). The signs at $0$ and $+\infty$ are `++---+--++--` and `+++---+--+--`, respectively, each with five variations. Therefore the finite likelihood supremum is approached only at the support edge in the profiled coordinate.

The equal-multiplicity edge has $\hat\alpha^2\sim kb/(n\delta)$, so it is a divergent-shape boundary value, even though the limiting sample likelihood is finite. Evaluating at $\mu=x_{(1)}$ with finite coefficients gives zero likelihood under the dossier's endpoint convention.

## Reproduction

Both final Python runs passed on the workstation through the single-slot lock. [Profile checker](2026-11-25-fatigue-math-data/profiles.py) · [Threshold checker](2026-11-25-fatigue-math-data/threshold.py)

```sh
export TMPDIR="$PWD/build/tmp"
mkdir -p "$TMPDIR"
/tmp/umvl-cpu/run python3 -u dev/research/evaluations/2026-11-25-fatigue-math-data/profiles.py
/tmp/umvl-cpu/run nice -n 10 python3 dev/research/evaluations/2026-11-25-fatigue-math-data/threshold.py
```

The **numerical** profile checks used 100 decimal digits, scales $10^{-12},1,10^{12}$, spreads $10^{-24},10^{-12},0.01,1,10^6,10^{24}$ and independently chosen endpoint multiplicities one or three. For each of the 72 free-shape samples the code bisects the root and checks three signed brackets, with 17 equally spaced points per bracket for the value and curvature bounds. It also checks a wider bracket outside $[H,A]$. The held-shape checks use $a=3$, three reciprocal pairs $(1,1+\varepsilon)$ with $\varepsilon=0,10^{-16},0.1$, and the asymmetric example in section 2. They check the bracket bounds on the same 17-point grid. For the asymmetric sample the numerical log-likelihood gap is $3.37403947222\times10^{-38}$, and the Taylor corrections on the narrow checked brackets are about $4.63\times10^{-48}$. The comparison separates the peaks at that precision. The checker uses a scaled $10^{-65}$ curvature comparison slack and a $10^{-65}$ log-value slack for its numerical grid checks, including rounded equalities. The exact held-shape checks use rational score polynomials. The threshold checker counts roots over the rationals and corroborates the P0 spike at 80 digits for $k=1,2,3$ and $\varepsilon=10^{-4},10^{-8},10^{-16},10^{-24}$.

## What this changes for the fitter

- **Fixed threshold, free shape and scale.** A single signed bracket suffices on every nonconstant sample, with $H$ and $(H+A)/2$ as exact bounds. Eliminate global peak comparison on this route. Return a certified unique estimate when original-data sign, coefficient enclosure and reconstruction checks pass. The same uniqueness proof covers the offset route. Keep its existing uncertified result if numerical enclosures fail and the search succeeds. Exact constant-sample collapse still reports unbounded likelihood.
- **Held shape.** At most two decreasing crossings require consideration. Use the turning-point conditions to certify the count, or exploit concavity for $a\le2$. Retain signed brackets and compare actual likelihood enclosures by strict separation. A proved reciprocal sample below or at its curvature threshold yields a singleton. Above it, retain the complete pair when both coefficient enclosures pass. An overlap without an exact tie proof retains the valid search result as uncertified with unknown uniqueness and keeps candidate brackets as diagnostics. Double stationary contacts alone do not create maximum candidates. A triple crossing still needs an accurate root enclosure.
- **Free threshold.** The all-free stop can name unboundedness for every nonempty sample. The held-shape/free-scale stop is a mathematical nonexistence only for constant samples. With scale held and shape free, route $3k>n$ to unboundedness, $3k<n$ to an attained search, and equality to an endpoint comparison that can distinguish finite unattainment from an attained tie. Both coefficients held give an attained threshold fit.

## Open

- The proofs leave the floating-point acceptance work to implementation. In particular, original-data enclosures near coalescing turning points, near the reciprocal curvature threshold and when threshold subtraction rounds distinct observations together need practical margins or higher precision. The scripts establish exact identities and high-precision checks, so they do not calibrate a binary64 margin.
- Exact algebraic isolation decides ties for rational inputs but can be expensive with many distinct observations. An efficient exact fallback within the fitter's work budget and a compact threshold search for the two-dimensional held-shape problem remain open implementation work. The unrestricted threshold results leave the competition with the normal face under a one-sided shape restriction open.
