# TASK-138: Burr XII closure and finite maximum likelihood

Completed 2026-11-03. Tested source: `86b51aad68743e6a05d91be9d4c888639971ef74`
on `codex/burr-closure`. All pre-existing tracked files are unchanged.
This report and its [small evidence directory](2026-11-03-burr-closure-data/README.md)
are the entire change; no commit was made.

## Result

For a nonconstant positive sample, **Weibull and Pareto are the only proper,
nondegenerate escape laws on the positive half-line**. There are no additional
likelihood-preserving defective mixtures or partial collapses. Weak convergence
alone nevertheless misses a likelihood detail: different approaches to the
same Pareto law leave different density values at observations tied for its
endpoint.

A finite all-free Burr MLE exists **if and only if some finite Burr point
reaches or exceeds the better optimized Weibull/Pareto likelihood**. Equality
must be included: a construction below proves that finite MLEs tied exactly
with Pareto really occur. The comparison admits an explicit compact
feasibility formulation and explicit, although very loose, large-shape bounds.

There is also a useful exact nonexistence theorem: **if at least half the
observations equal the minimum, the supremum is Pareto and every finite Burr
point is strictly worse**. In particular, every nonconstant two-observation
sample has no finite all-free Burr MLE. Nonexistence also persists in open
neighborhoods of these samples, so it is not confined to exact ties.

Neither local stability at Weibull nor comparison of the two face values
settles the general problem. Exact certificates below give a finite winner
with a negative Weibull seam slope, a Pareto winner with a positive slope,
and a finite winner where both faces have equal likelihood and are locally
stable. A separate zero-slope example needs the second-order term.

These are mathematical results and research checks, not a newly certified
production fitter. The existing [Burr dossier](../../../univariateML-cpp/include/cppunivariate/distributions/burr/README.md)
and [multistart implementation](../../../univariateML-cpp/include/cppunivariate/distributions/burr/mle.hpp)
remain unchanged, including their numerical boundary comparisons. No settled
d/p/q/r, truth fixture, generator, default fit, or golden needs changing for
this research result.

Shao's [2004 paper](https://doi.org/10.1016/S0167-9473(02)00367-5) already
discusses the two limiting families and multiple likelihood maxima. The
publisher preview was checked; full text was inaccessible. The proofs here
are self-contained, and no novelty or agreement with an unread theorem is
claimed. The earlier [P0 analysis](2026-10-29-p0-genbeta.md) and
[held-scale collapse report](2026-11-02-burr-collapse-report.md) provide the
repository context.

## 1. Coordinates and a unique conditional fit

Assume $n\ge2$ finite positive observations with at least two distinct values.
Write

$$
y_i=\log(x_i/x_{\min}),\quad \min_i y_i=0,\quad
R=\max_i y_i>0,\quad D=\sum_i y_i>0,
$$

and let $m<n$ count the zero log observations. Put
$b=\log(s/x_{\min})$, $a=-cb$, $z_i=a+cy_i$, and
$\operatorname{sp}(z)=\log(1+e^z)$.
The log-data density is

$$
g(y)=ck\frac{e^{a+cy}}{(1+e^{a+cy})^{k+1}},\qquad
\ell=n\log(ck)+\sum_i z_i-(k+1)\sum_i\operatorname{sp}(z_i).
$$

The original-data log likelihood is $\ell-\sum_i\log x_i$; that fixed
Jacobian is omitted throughout all comparisons. Every displayed likelihood
is a **total**, not a per-observation mean, unless explicitly divided by $n$.

For fixed $k>0$, $\ell(c,a)$ is strictly concave on $c>0$, $a\in\mathbb R$.
With $p_i=(1+e^{-z_i})^{-1}$ its negative Hessian is

$$
\begin{pmatrix}n/c^2&0\\0&0\end{pmatrix}
+(k+1)\sum_i p_i(1-p_i)
\begin{pmatrix}y_i^2&y_i\\y_i&1\end{pmatrix},
$$

which is positive definite. The envelope in the next section proves
coercivity for nonconstant data. Consequently the conditional maximum

$$
H(k)=\max_{c>0,\ a\in\mathbb R}\ell(k,c,a)
$$

exists, is unique, and varies smoothly with finite positive $k$. This reduces
the remaining global problem to one shape coordinate; it does **not** imply
that $H$ is concave or unimodal.

The conditional mode $y_0=(-a-\log k)/c$ lies strictly inside $(0,R)$:
the location-score summands $1-(k+1)p_i$ are positive below the mode and
negative above it, and must sum to zero.

An alternative exact reduction, useful for the minimum-tie theorem, profiles
$k$ first. Define

$$
S=\sum_i\operatorname{sp}(z_i),\quad
T=\sum_i\operatorname{sp}(-z_i),\quad \widehat k=n/S.
$$

Then

$$
\widetilde\ell(c,a)=n\log(nc/S)-n-T.                         \tag{1}
$$

## 2. An envelope and the complete weak closure

Let $\alpha=kc$ and $\lambda=\min(c,\alpha)$. The mode height is

$$
g(y_0)=c\left(\frac{k}{k+1}\right)^{k+1}\le\lambda.
$$

There is a uniform exponential envelope

$$
g(y)\le e\lambda\exp\{-\lambda|y-y_0|/2\}.                 \tag{2}
$$

Here is a proof with constants independent of both shapes. At displacement
$t$ from the mode, put $v=ct$. The log-density derivative is
$ck(1-e^v)/(k+e^v)$. At $t=2/\lambda$ it is at most
$-\lambda/2$; at $t=-2/\lambda$ it is at least $\lambda/2$.
For $k\ge1$, the relevant ratios are bounded below by
$(e^2-1)/(e^2+1)>1/2$. For $k\le1$, $|v|=2/k\ge2$ gives the same
lower bound. Concavity of $\log g$ extends these slopes outward.
Inside that central interval the mode-height bound proves (2).

For the sample,

$$
e^\ell\le\lambda^n,\qquad
e^\ell\le\lambda^n\exp\{n-\lambda R/2\}.                  \tag{3}
$$

The second inequality uses $\sum_i|y_i-y_0|\ge R$. In particular the
likelihood is bounded for every nonconstant sample. Any sequence with
likelihood bounded away from zero has $\lambda$ bounded above and below.
If $c$ and $\alpha$ stay bounded, its mode, and hence its location, also
stay bounded. This proves the conditional coercivity used above.

For completeness, compactify the observation space to $[0,\infty]$, or
equivalently the log axis to $[-\infty,\infty]$. Its entire weak closure is:

| Laws in the closure | How they arise |
|---|---|
| Finite-parameter Burr XII | Interior parameter limits |
| Weibull with positive shape and scale | $k\to\infty$, finite positive $c$, $s k^{-1/c}$ finite positive |
| Pareto with positive endpoint and exponent | $c\to\infty$, $kc\to\alpha\in(0,\infty)$, finite log endpoint |
| Point masses $\delta_t$, $0\le t\le\infty$ | Concentration or locations escaping to an end |
| $q\delta_0+(1-q)\delta_\infty$, $0\le q\le1$ | Spread escaping simultaneously to both ends |

To prove exhaustiveness, extract a subsequence on which $\lambda$ tends
to zero, a finite positive value, or infinity.

* If $\lambda\to0$, $g\le\lambda$ makes the probability of every bounded
  log interval tend to zero. Any weak limit is supported only at the two
  ends. Every mixture in the last row is achievable: take $k=1$, $c\to0$,
  and $a=\log\{q/(1-q)\}$, so the CDF at each finite log observation tends
  to $q$.
* If $\lambda\to\infty$, (2) gives mass outside distance $d$ of the mode
  at most $4e\exp(-\lambda d/2)$. An extended limit of the mode therefore
  gives a point mass.
* If $\lambda$ stays finite positive, at least one of $c,\alpha$ stays
  bounded. When both do, there is an interior Burr limit or an end point
  mass. When $c$ stays finite and $\alpha\to\infty$, put
  $\eta=a+\log k$. Finite $\eta$ gives
  $g(y)\to c e^{\eta+cy}\exp\{-e^{\eta+cy}\}$, a log-Weibull density;
  divergent $\eta$ gives an end point mass. When $c\to\infty$ and
  $\alpha$ stays finite, a finite $b$ gives survival
  $\exp\{-\alpha(y-b)_+\}$, the log-Pareto law. Divergent $b$ again
  gives an end point mass.

These cases include all subsequences; all displayed limits have explicit
approaching Burr sequences. In particular there is no extra proper
lognormal, inverse-Weibull, bounded-upper-support, or continuous-plus-escaped-
mass limit hidden in a joint rate of divergence.

For positive likelihood on a **nonconstant fixed sample**, (3) excludes the
first two cases. The remaining escape sequences must therefore have Weibull
or Pareto limits. For a constant sample at $x_0$, concentration is allowed:
$k=1$, $s=x_0$, $c\to\infty$ gives $f(x_0)=c/(4x_0)$ and unbounded
likelihood. If such a concentration sequence keeps positive likelihood at
$x_0$, (2) forces its mode to tend to $\log x_0$; it cannot hide a collapse
at a different observation.

### The Pareto density trace

Suppose $c\to\infty$, $kc\to\alpha>0$, and $b\to b_0$. Positive sample
likelihood requires $b_0\le0$. If $b_0<0$, every observation has the usual
Pareto density. If $b_0=0$ and

$$
\rho=\lim\frac{1}{1+\exp\{-c(0-b)\}},
$$

then the limiting density at each minimum is $\rho\alpha$, while at
$y_i>0$ it is $\alpha e^{-\alpha y_i}$. Positive likelihood requires
$0<\rho\le1$ and the total penalty is $m\log\rho$.

Every such $\rho$ has the **same weak Pareto law**. Holding $s=x_{\min}$
exactly gives $\rho=1/2$ and the familiar $-m\log2$ penalty. The all-free
upper trace has $\rho=1$: approach the endpoint from below with
$b\to0$ and $-cb\to\infty$. This distinction is essential when comparing
sample likelihoods and is consistent with the repository's closed-endpoint
Pareto convention.

## 3. Boundary optima and the exact attainment criterion

The best log-Pareto likelihood is explicit:

$$
P=n\log(n/D)-n,\qquad \widehat\alpha=n/D,
$$

with endpoint zero and upper density trace one. The best log-Weibull
likelihood is

$$
W=\max_{c>0}\left[n\log c+cD
-n\log\left\{n^{-1}\sum_i e^{cy_i}\right\}-n\right].       \tag{4}
$$

Its derivative is $n/c+D-n\sum_i y_i e^{cy_i}/\sum_i e^{cy_i}$ and its
second derivative is $-n/c^2-n\operatorname{Var}_{w(c)}(y)<0$.
The derivative changes from positive to negative, so its root and maximizer
are unique. Put $B=\max(P,W)$.

The conditional profile has endpoint limits

$$
\lim_{k\downarrow0}H(k)=P,\qquad \lim_{k\to\infty}H(k)=W.  \tag{5}
$$

The first limit also follows from the explicit two-sided bound below.
For the second, the envelope makes conditional maximizing parameters compact
in $(c,\eta)$ at any fixed likelihood level below $W$. The kernels converge
uniformly there to Weibull. Uniqueness of the Weibull maximum then identifies
the limiting maximum; a Weibull-approaching sequence supplies the lower bound.

**Attainment theorem.** For nonconstant positive data,

$$
\text{a finite Burr MLE exists}
\quad\Longleftrightarrow\quad
\exists k,c,s\text{ finite positive with }\ell\ge B.       \tag{6}
$$

Equivalently, $H(k)\ge B$ at some finite positive $k$.
Every escape has limiting likelihood at most $B$. If the unrestricted
supremum is above $B$, a maximizing sequence cannot escape and compactness
gives a finite maximizer. If the supremum equals $B$, a finite point at that
level is already a maximizer. Conversely the two face paths make every
finite global maximum at least $B$.

Strict improvement is a convenient sufficient condition, but section 8
proves that replacing $\ge$ by $>$ would lose real finite MLEs.

## 4. Pareto: an entropy bound and the moving-endpoint expansion

The Pareto face is locally stable for **every** nonconstant sample. This is
a global inequality near $k=0$, rather than a fitted numerical slope.

Let $r=n-m$ and $k_0=m/r$. The identity $S-T=na+cD$ gives

$$
\ell=n\log\alpha-\alpha D-nka-(k+1)T.
$$

Since $T\ge m\operatorname{sp}(-a)$, maximizing the first two terms over
$\alpha$ and the remaining bound over $a$ gives, for $0<k<k_0$,

$$
H(k)\le P+Q(k)<P,
$$

$$
Q(k)=A\log A+(M-A)\log(M-A)-M\log M,
\qquad A=nk,\quad M=m(1+k).                               \tag{7}
$$

Thus $Q=-M$ times the binary entropy at $A/M$. The maximizing comparison
path is

$$
\alpha=n/D,\qquad c=\frac{n}{kD},\qquad
a=\log\frac{m-rk}{nk}.
$$

Using $\operatorname{sp}(u)\le e^u$ for the nonminimum terms gives an
explicit matching lower bound:

$$
P+Q(k)-E(k)\le H(k)\le P+Q(k),
$$

$$
E(k)=(k+1)\frac{nk}{m-rk}
\sum_{y_i>0}\exp\left\{-\frac{ny_i}{kD}\right\}.          \tag{8}
$$

Consequently, for each fixed nonconstant sample,

$$
H(k)=P+nk\log k+nk\{\log(n/m)-1\}
+\frac{n(n-2m)}{2m}k^2+O(k^3).                            \tag{9}
$$

The error between $H$ and $P+Q$ is nonpositive and exponentially small,
$O(k e^{-\gamma/k})$, with $\gamma=n\min_{y_i>0}y_i/D>0$.
The leading deficit is $nk\log(1/k)$, not a finite linear seam slope.

At $k=k_0$, the bound before maximizing $a$ is also strictly below $P$:
its last two terms reduce to $-nk\operatorname{sp}(a)<0$.
Since the conditional maximum is attained, $H(k_0)<P$.
Therefore **every finite all-free MLE must have $k>m/(n-m)$**.

The matching scale approaches the endpoint from below:
$b=-a/c\sim-(kD/n)\log\{m/(nk)\}$. This explains both the logarithmic
loss and why the fixed-endpoint $\log2$ penalty disappears in the optimized
all-free limit.

## 5. A bounded sample criterion and explicit tail cutoffs

Equation (6) can be restricted to a known compact region; there is no need
to leave its nuisance-parameter domain unbounded. Define

$$
L=e^{B/n},\qquad
U=\frac{4n}{R}\left\{1+\log(2n/R)-B/n\right\},\qquad
C=\frac{U}{\min(1,k_0)}.                                  \tag{10}
$$

Any point with $\ell\ge B$ has $L\le\lambda\le U$.
For the upper bound, put $u=\lambda R/(2n)$ in (3).
The universal maximum of that bound is $(2n/R)^n$, so
$\beta=B/n-\log(2n/R)\le0$. For $u\ge2$ use $\log u\le u/2$;
then $\log u+1-u\ge\beta$ implies $u\le2(1-\beta)$.
The case $u<2$ satisfies the same bound.

Let $\xi=1/k$, $\eta=a+\log k$, and $v_i=e^{\eta+cy_i}$. Define the
continuous extension

$$
F(c,\eta,\xi)=n\log c+n\eta+cD
-(1/\xi+1)\sum_i\log(1+\xi v_i),\qquad \xi>0,
$$

$$
F(c,\eta,0)=n\log c+n\eta+cD-\sum_i v_i.                 \tag{11}
$$

The latter is exactly the Weibull log likelihood. A finite MLE exists
**if and only if** $F\ge B$ somewhere in

$$
L\le c\le C,\qquad -cR\le\eta\le0,\qquad
0<\xi<\frac{n-m}{m}.                                     \tag{12}
$$

To see necessity, replace any candidate reaching $B$ by its unique
conditional $(c,a)$ maximum. Section 4 excludes $k\le k_0$, while
$\lambda=c\min(1,k)$ and (10) bound $c$. The conditional mode lies in
$(0,R)$, giving $-cR<\eta<0$. Sufficiency follows directly from (6).
Closing the displayed intervals, including $\xi=0$, gives an explicit
compact elementary-function feasibility problem, with the Weibull boundary
already identified. This is an exact characterization, not a claim that
its equality cases are automatically easy to decide numerically.

There are also explicit large-$k$ bounds. At a conditional maximum with
$H(k)\ge B$ and $k\ge1$, $c\le U$ and the mode constraint gives
$v_i\le e^{UR}$. The elementary inequality
$\log(1+u)\ge u-u^2/2$ for $u\ge0$ yields

$$
H(k)\le W+\frac{n e^{2UR}}{k}
\quad\text{whenever }H(k)\ge B,\ k\ge1.                 \tag{13}
$$

Indeed $F-F(\cdot,\cdot,0)\le
\xi(1+\xi)\sum_i v_i^2/2\le\sum_i v_i^2/k$.
Two useful consequences are:

* If $P>W$, every finite fit reaching $P$ has
  $k\le\max\{1,n e^{2UR}/(P-W)\}$. Thus even the existence of a finite
  **tie** with a Pareto-winning face reduces to a compact finite-$k$ search.
* If a feasible finite witness exceeds $B$ by $\delta>0$, all
  $k\ge\max\{1,2n e^{2UR}/\delta\}$ have
  $H(k)\le B+\delta/2$ and cannot beat that witness.

These bounds are certified in exact arithmetic conditional on their stated
comparisons. They are deliberately coarse. For the negative-slope witness
below, the exact rational check proves $U<112$, witness gap $>0.06$, and
$(700/3)e^{224}<10^{100}$. Hence every $k\ge10^{100}$ is excluded, with
more than $0.03$ log-likelihood separation. The 75-digit evaluation of the
unrounded bound gives $\log_{10}K\approx99.3437$.
This is a genuine finite cutoff, but an impractical enumeration budget.
No capped estimate or new global fitting implementation is returned.

## 6. A global minimum-tie nonexistence theorem

Assume $m\ge n/2$. In (1), let $p_0=\operatorname{sigmoid}(a)$ and
$G=(\prod_i p_i)^{1/n}$. Then

$$
\widetilde\ell-P=n\log\frac{cDG}{S}.                     \tag{14}
$$

Let $q$ be the geometric mean of the $r=n-m$ nonminimum $p_i$.
Concavity of $\log\operatorname{sigmoid}$ gives

$$
\operatorname{sp}(a+cy_i)-\operatorname{sp}(a)
=c y_i\int_0^1\operatorname{sigmoid}(a+tcy_i)\,dt
\ge c y_i\sqrt{p_0p_i}.
$$

The weights $y_i$ and values $\sqrt{p_i}$ are similarly ordered, so their
weighted average is at least the unweighted average, which is at least
$\sqrt q$. Therefore

$$
S=n\operatorname{sp}(a)
+\sum_{y_i>0}\{\operatorname{sp}(a+cy_i)-\operatorname{sp}(a)\}
>cD\sqrt{p_0q}.
$$

Since $p_0\le q$ and $m/n\ge1/2$,
$G=p_0^{m/n}q^{r/n}\le\sqrt{p_0q}$. Equation (14) is strictly negative
for **every** finite $(c,a)$, and hence for every finite Burr parameter
triple. The approaching Pareto path establishes that the supremum is $P$.

There is also a strict Weibull gap. Put $p=r/n\le1/2$ and $u=cD/n$.
Jensen's inequality on the positive observations gives

$$
W(c)-P\le n\log\frac{u e^u}{1-p+p e^{u/p}}
\le n\log\frac{u}{\cosh u}< -\frac n2\log2.              \tag{15}
$$

For the middle inequality, $1-p+p e^{u/p}$ decreases with $p$, so it
is at least $(1+e^{2u})/2$. For the last,
$\cosh u\ge1+u^2/2\ge\sqrt2\,u$, with strictness for $u>0$.
The unique finite Weibull maximizer preserves the strict gap.

This nonexistence is stable under small data perturbations. Near a given
nonconstant majority-minimum sample, $P>W$ remains strict. Using just one
minimum excludes $k\le1/(n-1)$ uniformly; (13) excludes sufficiently large
$k$ uniformly. The remaining potentially competitive conditional parameters
lie in a common compact set. At the original sample its maximum is strictly
below $P$, so continuity excludes competitors throughout a neighborhood.
Such neighborhoods contain fully distinct samples. The tie theorem is a
simple sufficient criterion, not a necessary tie condition.

## 7. The Weibull seam, and certified failures of local classifiers

The power/GPD observation is exact:

$$
\Pr(X>x)=(1+\xi x^c/\sigma)^{-1/\xi},\qquad
\xi=1/k,\quad\sigma=s^c/k.
$$

It identifies the Weibull seam at $\xi=0$, but letting the power vary also
creates the separate Pareto escape. Extending to negative $\xi$ introduces
a different bounded-support model; those laws are not Burr closure members.

At the unique Weibull fit let $v_i=e^{\eta_W+c_W y_i}$, so
$\sum_i v_i=n$. Expansion of (11), including nuisance reoptimization, gives

$$
H(k)=W+\frac{A_W}{k}+\frac{C_W}{k^2}+O(k^{-3}),
\qquad A_W=\frac12\sum_i v_i^2-n.                         \tag{16}
$$

For clarity, in the natural coordinates $\theta=(c,\eta)$ define

$$
J=\begin{pmatrix}
n/c_W^2+\sum_i y_i^2v_i&\sum_i y_i v_i\\
\sum_i y_i v_i&n
\end{pmatrix},\qquad
d=\sum_i v_i(v_i-1)\begin{pmatrix}y_i\\1\end{pmatrix}.
$$

Then

$$
C_W=\sum_i\left(\frac{v_i^2}{2}-\frac{v_i^3}{3}\right)
+\frac12 d^T J^{-1}d.                                   \tag{17}
$$

$J$ is the negative **total observed Weibull likelihood Hessian**, not a
Fisher-information or covariance claim. The nuisance displacement is
$\theta-\theta_W=\xi J^{-1}d+O(\xi^2)$.
Conditional coercivity, uniqueness at $\xi=0$, and the implicit function
theorem justify expanding the conditional maximum itself, rather than only
one fixed-parameter path.

Thus $W\ge P$ together with $A_W>0$ is sufficient for a finite MLE.
So is $W\ge P$, $A_W=0$, $C_W>0$. Negative $A_W$ makes Weibull locally
stable but gives no global exclusion. At $c_W=1$, the first coefficient
reduces to $n\{\overline{x^2}/(2\bar x^2)-1\}$, as suggested in the
brief. In general the same expression must be applied to $x^{c_W}$.

### Finite winner despite negative slope

Take exact log data

$$
y=(0,5,7,8,8,15,15)/15.
$$

The fixed finite point $k=1$, $c=5.16$, $a=-2.816$ has
$s=x_{\min}\exp(352/645)$. Its likelihood and the optimized limits are:

| Quantity | Numerical value |
|---|---:|
| $P$ | $-2.84537862272227$ |
| $W$ | $-2.43846031048140$ |
| Fixed finite point | $-2.37508288368536$ |
| $A_W$ | $-0.05562070833942$ |
| $C_W$ | $0.29520353262431$ |

Exact rational certificates prove finite-minus-$W>0.063$,
finite-minus-$P>0.470$, and $A_W<-0.055$. The attainment theorem proves
a finite MLE exists. The displayed point is a feasible witness, **not a
claimed globally maximizing estimate**. Both boundary ends are locally
stable, yet neither is globally best.

### Positive slope despite global nonexistence

For $y=(0,0,1)$, the minimum-tie theorem proves every finite fit is below
$P=3\log3-3=0.295836866004329\ldots$. Yet
$W=-2.335394517137318\ldots$ and the exact certificate gives
$A_W>0.007$; numerically $A_W=0.007035372035697\ldots$.
The improving direction at Weibull cannot bridge the Pareto gap.

### Exactly zero first-order slope

Let $u=(0,5,7,8,8,15,15)/15$ and $v=(0,0,0,0,0,1,1)$, and take
$y(t)=(1-t)u+tv$. Exact endpoint signs give a zero of $A_W(t)$ inside

$$
0.0439243843<t_0<0.0439243846.
$$

Across that entire bracket the certificates give $W-P>0.11$ and
$C_W>0.30$. At any such zero, (16) therefore proves finite improvement and
a finite MLE. The numerical root is
$t_0\approx0.04392438441485763$, with $C_W\approx0.318132116623484$.
No uniqueness of the zero is needed for this argument.

### Raw coefficient of variation cannot classify attainment

The transformation $x_i\mapsto d x_i^p$, $d,p>0$, maps Burr to Burr with
the same $k$, power $c/p$, and transformed scale. On the log axis it adds
the same $-n\log p$ to every model's optimized likelihood. All attainment
decisions and seam signs are invariant.

For the finite negative-slope example, the empirical CV of $e^{p y}$ is
$0.0032991$ at $p=0.01$ and $1.55390$ at $p=10$. For the nonexistence
example $(0,0,1)$ it is respectively $0.0047219$ and $1.41402$.
Here empirical variance uses divisor $n$. Either outcome therefore occurs
on either side of raw CV one. These examples disprove the specified local
and moment classifiers; they do not prove that no simpler global criterion
can ever exist.

![Conditional Burr profiles, divided by sample size, relative to the better optimized limit. The blue and orange examples have finite points above both locally stable faces; the green example has no finite MLE despite a positive Weibull seam slope. The right panel resolves the small differences near the boundary likelihood.](2026-11-03-burr-closure-data/profiles.png)

The curves are checked **conditional numerical profiles** on a fixed grid,
not a global search certificate. The existence and nonexistence labels use
the proofs and exact finite certificates above.

## 8. Where limits meet; finite ties really occur

### Equal face values can hide a better finite fit

Take $y(t)=(0,t,7/15,8/15,8/15,1,1)$. Exact bounds give opposite signs
of $W-P$ at $t=0.23826$ and $t=0.23827$. Continuity gives a crossing
$t_*$ inside that interval. The numerical crossing is
$0.23826602716416793\ldots$.

Uniformly throughout the interval, the fixed finite point
$k=1$, $c=4.932$, $a=-2.624$ exceeds **both** $P$ and $W$ by more than
$0.026$, and $A_W<-0.087$. At $t_*$, $P=W$ but the global supremum is
strictly larger. Pareto is locally stable by (7), and Weibull by (16).
This is a certified three-way competition, not a new limiting family.

### A finite MLE can equal the best boundary likelihood

There is also an exact existence construction of finite/Pareto ties.
Fix the rational data family

$$
y(q)=(0,\ 0.23826q,\ 7q/15,\ 8q/15,\ 8q/15,\ 1,\ 1),
\qquad 0\le q\le1.                                      \tag{18}
$$

At $q=0$ five minima give strict nonexistence. At $q=1$ the preceding
finite point exceeds $P$ by more than $0.026$, while exact bounds give
$W-P<-0.00002$.

In fact $W(q)-P(q)$ is strictly increasing, so $P>W$ on the **whole**
path. To prove this, normalize the data to $r_i(q)=y_i(q)/D(q)$, whose
sum is one. If the four scaled interior values sum to $A$, each largest
partial sum has the form $1/(2+qA)$ or
$(2+qA_j)/(2+qA)$ with $A_j\le A$. These decrease with $q$.
Thus smaller $q$ gives a vector majorizing the one at larger $q$.
Strict convexity of the exponential, together with

$$
W-P=\max_{v>0}\left\{n\log v+v
-n\log\sum_i e^{v r_i(q)}\right\},
$$

proves the assertion.

All conditional fits reaching $P(q)$ now lie in a **common compact
finite-parameter set**. Using just one minimum excludes $k\le1/6$,
including at $q=0$. Uniformly $U<114$ and $P-W>0.00002$; (13) and an
exact exponential/log certificate exclude every $k\ge10^{106}$.
The bounds on $c$ and mode are uniform as well: $c<684$, $c$ has a
positive lower bound, and $-c\le\eta\le0$.

Let $\mathcal S$ be the set of $q$ with some finite point strictly above
$P(q)$. It is open and contains values near one. It contains no values
near zero: on the common compact set the strict majority-minimum inequality
at zero has a strictly positive likelihood gap, which persists by
continuity. Therefore $q_*=\inf\mathcal S$ belongs to $(0,1)$.

Choose improving conditional fits at $q_j\to q_*$. Compactness supplies
a finite limiting fit at least $P(q_*)$. Openness of $\mathcal S$ implies
$q_*\notin\mathcal S$, so **every** finite point is at most $P(q_*)$.
The limiting finite fit is consequently a global MLE **exactly equal** to
$P(q_*)>W(q_*)$.

This proves that equality in (6) is necessary. It also distinguishes
existence of a finite MLE from compactness of all maximizing sequences:
at this sample a finite maximizer and an escaping Pareto sequence coexist.
No numerical value of $q_*$ or globally optimal coefficient is asserted.
The six retained family evaluations corroborate the endpoint inequalities
and monotonicity; the global transition is proved by the argument above.
Small perturbations can separate the interior and upper ties while retaining
the strict endpoint comparisons and the same majorization proof. Thus the
finite/Pareto tie construction also applies to some fully distinct samples.

### The two probability-law faces meet only through collapse

A proper Weibull has positive density below every positive Pareto endpoint,
so the two nondegenerate law families do not intersect. On the positive
sample space their common proper weak limits are point masses: the Weibull
power and Pareto exponent diverge to concentrate at a finite common value.
On the extended space they also share the two-end mixtures. For Weibull,
let $c\to0$ with finite $\eta$, giving limiting CDF
$1-\exp(-e^\eta)$ at every finite log value. For Pareto, let
$\alpha\to0$, $b\to-\infty$, with $-\alpha b\to A>0$, giving
limiting CDF $1-e^{-A}$. These extended-space mixtures have zero likelihood
on the fixed positive sample and are irrelevant to (6).

More directly, set $x_i=x_0e^{\varepsilon t_i}$ for a fixed nonconstant
pattern $t$. The exact scaling identity is

$$
H_{\varepsilon t}(k)=H_t(k)-n\log\varepsilon,
\quad P_{\varepsilon t}=P_t-n\log\varepsilon,
\quad W_{\varepsilon t}=W_t-n\log\varepsilon.              \tag{19}
$$

For every $\varepsilon>0$ the classification and all likelihood gaps are
unchanged; the powers scale as $1/\varepsilon$. At $\varepsilon=0$ the
data are constant and the likelihood is unbounded. Weak collapse therefore
does not provide a new fixed-sample existence criterion.

## 9. Reproduction, final checks, and precise remaining questions

The [reproducer](2026-11-03-burr-closure-data/check.py) and
[exact certificate code](2026-11-03-burr-closure-data/cert.py) passed after
their final changes. Their SHA-256 hashes are retained in
[summary.json](2026-11-03-burr-closure-data/summary.json); the preview renderer
checks them against the actual source. The lasting numerical evidence is
[66 convergence rows](2026-11-03-burr-closure-data/checks.csv) and
[195 conditional profiles](2026-11-03-burr-closure-data/profiles.csv).

The research script uses 75 decimal digits, seed 138, and checked conditional
scores below $10^{-48}$. The Weibull solve uses a monotone expanded bracket,
260 bisections, and a final total-score check below $10^{-65}$.
The conditional joint solve stops on failure; strict concavity establishes
the meaning of its stationary point. No grid endpoint or unrestricted
numerical maximum is reported as an MLE.

Workloads comprise three samples over 65 log-spaced $k$ values each;
Pareto expansions at $k=0.01,0.003,0.001$; Weibull first/second orders at
$k=10^3,10^4,10^5$; Weibull paths through $10^6$; three Pareto endpoint
traces per sample; constant collapse and two-end weak-mixture paths; the
zero-slope second order; and six evaluations of (18). The convergence-row
tolerances are explicit in the CSV: up to $0.5$ for the second Pareto
coefficient at the largest tested $k$, $0.1$ for the generic second Weibull
coefficient, and $0.01$ for the zero-slope second coefficient. These are
finite-point convergence checks, not uniform remainder bounds. The proofs
and (8), rather than those tolerances, establish the asymptotic statements.

There are 180 seeded checks each of the mode envelope, entropy bound,
minimum-tie profile inequality, and logarithm lower bound: **720 passed**,
with numerical allowance $10^{-60}$. The compact-region and tail inequalities
were also checked against retained profiles. A loose astronomical tail bound
passing such a check is weak numerical evidence; its actual guarantee is
the derivation in section 5 and the separate exact constant certificates.

The exact code uses rational arithmetic, rounding every arithmetic operation
outward to multiples of $10^{-45}$. Positive exponentials use 180 Taylor
terms with a geometric remainder bound; negative arguments use reciprocals.
Logarithms use powers-of-two reduction and 60 atanh terms with an explicit
remainder. Strict Weibull-score signs bracket its root, and concave tangents
bound its optimized value. All decisive assertions compare exact rational
endpoints; JSON decimal displays are rounded summaries. An initial wider
zero-slope enclosure was inconclusive and was narrowed before the final
successful run. There are no permitted final research failures.

All final commands ran on the **local workstation**, in the foreground and
awaited to completion, one suite per command. The supplied slot runners set
nice priority 10 and pin BLAS/OpenMP to one thread per worker; the full-suite
runner reserves four slots. CPU-queue waits were left uninterrupted.
Environment: Linux 7.0.0-38-generic x86-64, GCC 15.2.0, R 4.6.1,
Python 3.14.5, mpmath 1.3.0, NumPy 2.4.6, SciPy 1.18.0.
No simbox, sub-agent, installation, board edit, git write, or publishing was
used. The existing board was read once at session start.

| Final run | Result |
|---|---|
| C++ dev | 106/106 passed |
| C++ full | 127/127 passed |
| R dev | 20,507 passes; 0 failures, 0 warnings, 153 tier skips |
| R full | 43,324 passes; 0 failures, 0 warnings, 0 skips |
| Final research reproducer | Exact certificates, 66 convergence rows, 195 profiles, 720 inequalities passed |
| Figure rebuild | Passed, using final retained profiles |

Commands from the worktree root:

~~~{.bash .literal-code}
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp
export XDG_RUNTIME_DIR=$PWD/build/tmp
mkdir -p "$TMPDIR" build/scratch
/tmp/umvl-cpu/run4 just cpp-test
/tmp/umvl-cpu/run4 just test
/tmp/umvl-cpu/run4 env UMVL_TEST_TIER=full just cpp-test
/tmp/umvl-cpu/run4 just test-full
/tmp/umvl-cpu/run4 python3 dev/research/evaluations/2026-11-03-burr-closure-data/check.py
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-03-burr-closure-data/plot.py
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-03-burr-closure-data/render.py
~~~

These suites establish regression health of the unchanged package. They do
not certify its present all-free multistart fitter globally. Scratch build
and test logs remain under the worktree's ignored build directory.

The [HTML preview](2026-11-03-burr-closure-data/preview.html) has 381 static
MathML elements and no scripts or remote presentation resources. Strict
source and HTML note validation report zero errors and zero code-math
candidates; all 22 local links across this report and the evidence README
resolve. The prescribed artifact-pane command returned HTTP 403 because
this worktree is outside the viewer's configured readable roots. The
finished files remain here; no viewer configuration, other worktree, or
external destination was changed to bypass that restriction.

**Open: practical global certification.** The compact domain and explicit
tails are proved, including an actual $10^{100}$ exclusion, but they are
too loose for direct enumeration. A production implementation still needs
efficient global upper bounds over shape intervals, certified conditional
solutions with propagated numerical allowances, and useful tail constants.
No missing escape family or interval-arithmetic requirement is being used
in place of that work. The existing boundary tolerance can reject genuine
ties or small strict improvements; this research-only task changes no policy.

**Open: a simpler general data classifier.** The bounded criterion (12) is
exact, and the sufficient conditions and counterexamples are sharp in their
stated senses. A scalar closed-form replacement is not established; nor is
an impossibility theorem for one. First- and second-order local face tests
cannot alone replace global comparison. Cases with both $A_W=C_W=0$ need
higher-order local analysis if one wants a seam-based shortcut. The global
criterion itself includes those cases and finite equality correctly.

**Open: joint uniqueness.** The unique conditional $(c,a)$ fit and unique
Weibull fit are proved. Uniqueness of a finite unrestricted Burr MLE is not.
The displayed profiles show multiple local maximizing regimes, not a proof
of two distinct finite global maximizers. No inference calibration or
estimator-asymptotic claim is made.
