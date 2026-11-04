# Derivation: the Burr–Pareto endpoint experiment

These are analytical results for iid, correctly specified observations. Write
`Y=log X`, `b=log s`, `alpha=kc` and `w=k/alpha=1/c`. Burr has density

$$p_{\alpha,b,w}(y)=\alpha\,q((y-b)/w)
 \exp\{-\alpha w\operatorname{sp}((y-b)/w)\},
\quad q(z)=(1+e^{-z})^{-1},\quad\operatorname{sp}(z)=\log(1+e^z).$$

At `w=0` its law is the shifted exponential
$g_{\alpha,b}(y)=\alpha e^{-\alpha(y-b)}1_{y\ge b}$.
Logarithms of original-data densities have the same Jacobian, so all likelihood
ratios below also hold for original X. Endpoint density values use the inside
limit, as required by R7a.

## 1. Hellinger speed, allowing the endpoint to move

Use $H^2(p,g)=\int(\sqrt p-\sqrt g)^2$ with no factor 1/2. Fix
$\alpha=\alpha_0$, take $b=b_0+aw$, and let $w\downarrow0$. Boundary-layer
substitution $z=(y-b_0)/w$ gives

$$H^2(p_{\alpha_0,b_0+aw,w},g_{\alpha_0,b_0})
 =\alpha_0 w J(a)+o(w)=kJ(a)+o(k),$$
$$J(a)=\int_{\mathbb R}\{\sqrt{q(z-a)}-1_{z\ge0}\}^2dz
 =a-4\log2+4\operatorname{atanh}\sqrt{q(-a)}.$$

For completeness, on $z<0$ the density's exponential factor tends to one and
is bounded by one. On $z>0$, suppressing the common factor sqrt(alpha0),
the root difference is

$$e^{-\alpha_0wz/2}(e^{\alpha_0wa/2}-1)
 +e^{-\alpha_0wz/2}e^{\alpha_0wa/2}(\sqrt{q(z-a)}-1)
 +R_w(z),\qquad
 |R_w(z)|\le Cw\operatorname{sp}(a-z)e^{-\alpha_0wz/2}.$$

The first term's difference from the limiting root contributes $O(w^2)$
to squared distance in the original y scale, while the remaining exponentially
decaying boundary terms admit dominated convergence after division by w.
Cross terms are $o(w)$. This proves the formula, uniformly for bounded a.
The primitive of $\sqrt{q(z)}$ is $2\operatorname{atanh}\sqrt{q(z)}$, which
gives the displayed integral. Its derivative is $1-2\sqrt{q(-a)}$ and its
unique minimum occurs at $a=\log3$:

$$\inf_a J(a)=3\log3-4\log2=\log(27/16)>0.$$

For $|a|\to\infty$, $J(a)\sim|a|$; endpoint shifts larger than w do not
improve the order. Changing alpha by $O(\sqrt k)$ adds the ordinary
nonnegative shape distance to first order, with boundary cross terms $o(k)$.
Thus optimizing the matching Pareto nuisance cannot remove the order-k
distance. This is the transverse width speed after matching the face.

The width is distinguishable at $k\asymp1/n$, because iid affinity is
$(1-H^2/2)^n$. At $k=o(1/n)$, with the matched endpoint and shape fixed,
total variation between the product experiments tends to zero. At $nk\to
\infty$, fixed bounded endpoint-to-width ratios are separated in Hellinger
distance. This is not DQM in k: Hellinger distance is of order $\sqrt k$.
Reparametrizing by $\sqrt k$ makes the distance linear but does not create a
strong L2 derivative: the mass concentrates into a Lebesgue-null endpoint.

## 2. The point process and its likelihood

Let the null be $g_{\alpha_0,b_0}$ and put

$$\alpha_n=\alpha_0(1+h/\sqrt n),\quad
 b_n=b_0+t/(n\alpha_0),\quad w_n=v/(n\alpha_0),\quad v\ge0.$$

The rescaled lower observations $Z_i=n\alpha_0(Y_i-b_0)$ converge as a
point process to a rate-one Poisson process on $[0,\infty)$. Under the local
alternative the intensity on the whole line is

$$\lambda_{t,v}(z)=q((z-t)/v),\qquad
\lambda_{t,0}(z)=1_{z\ge t}.$$

This follows directly from $n p_Y(b_0+z/(n\alpha_0))/(n\alpha_0)
\to\lambda_{t,v}(z)$ locally uniformly for v>0, with independent rare-event
counts on disjoint bounded sets. The negative tail is integrable, and the
difference from intensity one on the positive tail decays exponentially.
Consequently the finite-dimensional local experiments have the corresponding
Poisson limit, including their singular parts. A bulk central limit theorem
gives an independent standard normal observation for h. Independence follows
by removing a band of width L/n, applying the CLT to the remaining iid
observations, and then increasing L; finitely many endpoint observations have
vanishing contribution to the normalized shape score.

For data under the null and v>0, the exact finite-n boundary likelihood at
h=0 is

$$\ell_n(t,v)-\ell_n(0,0)
 =t-(1+v/n)\sum_{i=1}^n\operatorname{sp}((t-Z_i)/v).$$

Indeed $\log(p/g)=\alpha_0(b_n-b_0)
 -(1+\alpha_0w_n)\operatorname{sp}((b_n-y)/w_n)$.
For bounded t and v bounded away from zero the tail of the sum is uniformly negligible:
$\operatorname{sp}((t-z)/v)\le e^{(t-z)/v}$. Thus the limiting log likelihood
on the null-supported configurations is

$$\Lambda(t,v)=t-\sum_{z\in\Pi}\operatorname{sp}((t-z)/v),\qquad
\Lambda(t,0)=t\text{ if }t\le Z_{(1)},\quad-\infty\text{ otherwise}.$$

The h part is independently $hZ-h^2/2$. Its profiling cancels when the
Pareto shape is fitted under both hypotheses.

There is real singular mass. Since
$\int(\lambda_{t,v}-1_{z\ge0})dz=-t$, the likelihood on null configurations
is $e^{\Lambda(t,v)}$. But

$$E_0e^{\Lambda(t,v)}
 =\exp\{-v\log(1+e^{-t/v})\}<1.$$

The missing probability is the alternative probability of at least one
negative point. Hence this cannot be a Gaussian LAN experiment; there is no
mutual contiguity. The displayed likelihood is the absolutely continuous
part of the alternative, not a normalized density under the null.

## 3. Known and fitted endpoint hypotheses differ

If b is held at b0, $p_{\alpha,b_0,w}(y)\le g_{\alpha,b_0}(y)$ on
$y\ge b_0$, for every alpha and w. For data from the null the closure LR
is therefore exactly zero, even with alpha fitted. If an observation is below
b0, the null likelihood is zero. The resulting LR test detects such an
observation; its power at t=0,v>0 tends to $1-2^{-v}$, apart from optional
randomization on the zero-statistic event. This is a support test, not a
half-chi-square test. A zero null LR does not mean indistinguishability.

If b is fitted under the null and alternative, the null endpoint likelihood
supremum is $Z_{(1)}$. For a compact parameter restriction, the LR limit is
twice the difference between the supremum of $\Lambda$ over that restriction
(including its sharp-endpoint face) and the corresponding null supremum.
The natural unrestricted endpoint-experiment statistic is

$$T_\Pi=2\left[\sup_{t\in\mathbb R,v\ge0}\Lambda(t,v)-Z_{(1)}\right]
 =2\sup_{s\in\mathbb R,v\ge0}
 \left[s-\sum_j\operatorname{sp}((s-D_j)/v)\right],
\quad D_j=Z_{(j)}-Z_{(1)},$$

with zero included from v=0. Compact likelihood-process convergence here
means uniform convergence away from v=0, supplemented by the following
bound at v=0; it is not sup-norm convergence across endpoint density traces.
The first point alone proves no improvement is
possible for $0<v\le1$: if s<0 the expression is negative; if s>=0,
$\operatorname{sp}(s/v)\ge s/v\ge s$.

Improvement has positive probability for v>1. Choose s=4,v=2. The first-point
contribution is $4-\operatorname{sp}(2)>1.8$. Conditional on no further point
in (0,8), the expected remaining contribution is at most
$\int_8^\infty e^{(4-z)/2}dz=2e^{-2}<0.3$.
Markov's inequality gives probability at least 5/6 of a total remainder
less than 1.8. The event of no point in (0,8) has probability $e^{-8}$.
This proves $P(T_\Pi>0)\ge(5/6)e^{-8}>0$ without an optimizer or a Monte Carlo argument.
It also gives a fixed feasible sequence beating fitted Pareto with positive
limiting probability; the full-model probability of a Pareto winner cannot
tend to one.

The unrestricted point-process supremum is finite almost surely. To see the
large-v bound, write t=av and use the Poisson strong law on expanding
intervals. Uniformly for a in a fixed compact interval,

$$\Lambda(av,v)/v\longrightarrow
 f(a)=a-\int_0^\infty\operatorname{sp}(a-r)dr.$$

Its strictly concave maximum is attained at
$a=\log(e-1)$ since $f'(a)=1-\log(1+e^a)$. It is strictly negative:
the maximum lies at 0<a<1, and numerical evaluation is not needed:
at a<1, $f(a)\le a-a\log2-a^2/4-\pi^2/12<0$ follows from
$\log(1+e^x)\ge\log2+x/2$ and maximizing the quadratic.
For a negative with large magnitude the upper bound $\Lambda\le t$ suffices;
for large positive a the Poisson counts below t/2 force a negative bound
quadratic in a after division by v. These inequalities extend coercivity to
all a. Bounded v and t tending to either infinity are controlled in the same
way. At v tending to zero the first-point bound above applies.

**Global LR.** The separate [localization argument](burr-localization.md)
excludes remote maxima, proves tightness at the n^-1 endpoint/width scales,
and transfers $T_\Pi$ to the unrestricted mathematical Burr closure
likelihood supremum. The fixed-sample entropy bound alone did not supply
that argument. No certified numerical quantile of $T_\Pi$ is supplied here.
