# Uniform beta-binomial tail bounds and escape classification

This is an exact-arithmetic derivation, not a floating-point implementation
certificate. Let the observed integers be $x_1,\ldots,x_h$, $M=\max x_i\ge1$,
$S=\sum x_i$, and $m=\#\{i:x_i>0\}$. Write $t=a+b$, with $a,b>0$.
Products over observations retain multiplicities. Likelihoods below are products,
not log likelihoods. No assertion about an NB shape optimizer is needed to define
$L_* = \sup_{a>0,\mu>0}\prod_i q_{a,\mu}(x_i)$, where $q$ is the NB pmf.

## 1. A uniform binomial kernel comparison

Put $f_x(\lambda)=e^{-\lambda}\lambda^x/x!$. For $N>x$, division of the
binomial pmf by $f_x(Np)$ gives

$$
\frac{\Pr\{\operatorname{Bin}(N,p)=x\}}{f_x(Np)}
=\prod_{j=0}^{x-1}(1-j/N)\exp\{Np+(N-x)\log(1-p)\}.
$$

The exponent is maximized at $p=x/N$ (at zero if $x=0$). Dropping the
falling-product factor and expanding the maximum gives

$$
x+(N-x)\log(1-x/N)
=\sum_{k=2}^\infty\frac{x^k}{k(k-1)N^{k-1}}
\le g_x(N):=\frac{x^2}{2(N-x)}.
$$

Consequently the binomial kernel is at most $e^{g_x(N)}f_x(Np)$, uniformly
in $0\le p\le1$, with endpoints interpreted by continuity.

We will also need convergence in absolute error. For one Bernoulli$(p)$ and
one Poisson$(p)$, direct summation gives total variation
$p(1-e^{-p})\le p^2$. The product total variation is at most the sum of
component variations (expand the difference of product measures one factor
at a time), and applying the sum map cannot increase it. Thus the binomial
and Poisson$(Np)$ probabilities differ by at most $Np^2$.
For $Np\le R$ this is at most $R^2/N$. For $Np>R>x$, both kernels are at
most their bounds $(e^{g_x(N)},1)f_x(R)$, since $f_x$ decreases beyond $x$.
Taking $R=N^{1/4}$ proves

$$
\sup_{0\le p\le1}|\Pr\{\operatorname{Bin}(N,p)=x\}-f_x(Np)|\longrightarrow0.
$$

This is an absolute, pointwise-in-$x$ uniform statement, not relative error
control on the observed likelihood.

## 2. Gamma coupling with an error independent of both shapes

Take independent $U\sim\operatorname{Gamma}(a,1)$ and
$V\sim\operatorname{Gamma}(b,1)$. The change of variables
$(U,V)=(p w,(1-p)w)$, with Jacobian $w$, factorizes their joint density:
$p\sim\operatorname{Beta}(a,b)$, $w\sim\operatorname{Gamma}(t,1)$,
independently. In particular, $R=(U+V)/t$ has mean one and variance $1/t$.
The two Poisson intensities to compare are

$$
\Lambda=N\frac{U}{U+V},\qquad \Lambda'=\frac{N U}{t},\qquad
\Lambda'=R\Lambda.
$$

No independence between $R$ and $\Lambda'$ is needed for the following
pathwise inequality. Since

$$
\left|\frac{d f_x(\lambda)}{d\log\lambda}\right|
=|x-\lambda|f_x(\lambda)
\le x f_x(\lambda)+(x+1)f_{x+1}(\lambda)\le 2x+1=:C_x,
$$

on $R\ge1/2$ the kernel difference is at most
$2C_x|R-1|$; here $|\log R|\le2|R-1|$. On the complement the difference is
at most one. Cauchy--Schwarz and Chebyshev therefore give

$$
|E f_x(\Lambda)-E f_x(\Lambda')|
\le d_x(t):=\min\{1,2C_x/\sqrt t+4/t\}.
$$

Integration of the gamma density shows that $E f_x(\Lambda')$ is exactly
$q_{a,Na/t}(x)$. Combining with section 1 yields, for every pair of shapes,

$$
P_{N,a,b}(x)\le e^{g_x(N)}\{q_{a,Na/t}(x)+d_x(t)\}.
$$

For a likelihood bound it is useful to retain the mixed-Poisson probabilities
$z_i=E f_{x_i}(\Lambda)$, which lie in $[0,1]$, rather than multiply the
possibly greater-than-one expressions $q_i+d_i$. Telescoping the difference
of products of $z_i$ and $q_i$ gives

$$
\prod_iP_{N,a,b}(x_i)
\le e^{G_N}\{L_*+E(T)\}\quad(t\ge T),\qquad
G_N=\sum_i g_{x_i}(N),\quad E(T)=\sum_i d_{x_i}(T).
$$

This distinction is essential: bounding the product of $q_i+d_i$ by
$\prod_iq_i+\sum_i d_i$ would in general be invalid.

## 3. Bounded total concentration

The exact zero probability and probability ratios are

$$
P(0)=\prod_{j=0}^{N-1}\frac{b+j}{a+b+j},\qquad
\frac{P(x)}{P(0)}=\frac{(N)_{\mathrm{fall},x}(a)_x}{x!(b+N-x)_x}.
$$

Writing each logarithm in $-\log P(0)$ as
$\int_0^a(b+j+u)^{-1}du$ and using the decreasing-sum integral bound gives
$-\log P(0)\ge a\log(1+N/t)$. If $t\le T$ and $N>M$, then

$$
P(0)\le e^{-aH},\quad H=\log(1+N/T),\qquad
P(x)\le e^{-aH}\frac{a(T+1)_{x-1}}{x!}
                  \left(\frac N{N-M}\right)^x\quad(x\ge1).
$$

Thus, with $K(T)=\prod_{x_i>0}(T+1)_{x_i-1}/x_i!$,

$$
\prod_iP(x_i)\le B_N(T):=
K(T)\left(\frac N{N-M}\right)^S
\left(\frac m{eh\log(1+N/T)}\right)^m.
$$

The last step maximizes $a^m e^{-haH}$ over all positive $a$, enlarging
the domain. This bound includes arbitrarily small $a$ and $b$ and has no
uncontrolled small-shape remainder.

## 4. A uniform tail envelope and its exact scope

For every $T>0$ and $N>M$ we have proved

$$
\sup_{a,b>0} L(N,a,b)\le
U_N(T):=\max\{B_N(T),e^{G_N}[L_*+E(T)]\}.
$$

For fixed $T$ both branches decrease with $N$. The bound consequently
applies to the entire integer tail beginning at $N$, not only to that size.
One can replace $L_*$ by any certified upper bound. There is no
outward-rounded implementation or calibrated floating-point margin here.
The map's numerical NB maximum is not such a certified upper bound.

For fixed $T$, $B_N(T)\to0$ and $G_N\to0$. Letting first $N\to\infty$ and
then $T\to\infty$ gives a limsup of at most $L_*$. For any fixed NB shape
$a$ and mean $\mu$, choosing $b=Na/\mu$ gives convergence to that NB law;
this follows also from section 2, since $Na/(a+b)\to\mu$. Taking the
supremum over such fixed pairs proves

$$
\lim_{N\to\infty}\sup_{a,b>0}L(N,a,b)=L_*.
$$

In particular a finite candidate strictly above $L_*$ has an eventual
finite tail cutoff. This does not give a useful enumeration budget, a
correct limit-winner classification when the gap vanishes, or a numerical
upper bound on $L_*$ from an optimizer's stopping criterion.

There is a useful necessary test for whether *any* tuning $T$ can make
this particular envelope clear a candidate $L_c<1$ at a given $N$. Put
$D=L_c e^{-G_N}-L_*$. If $D\le0$, it cannot. Otherwise $E(T)<D<1$
forces every summand out of its clipped regime, so

$$
E(T)=\frac C{\sqrt T}+\frac{4h}T<D,\qquad C=4S+2h,
\qquad T>(C/D)^2.
$$

Since $B_N(T)$ increases with $T$, a value
$B_N((C/D)^2)\ge L_c$ rules out every tuning, not merely a searched grid.
Using an attainable face likelihood in place of $L_*$ makes this test
optimistic; it is sufficient for diagnosing failure of the envelope even
when the true face supremum has not been computed.

This gives a striking lower bound on the required size even for
$(0,1,2)$. Its exact empirical-likelihood maximum is $L_c=1/27$, whereas
the Poisson face alone attains $L_P=e^{-3}/2$. Necessarily
$T>T_0=[18/(L_c-L_P)]^2$. Dropping the factor $(N/(N-2))^3\ge1$ from
$B_N(T)$ gives the further necessary condition

$$
\log(1+N/T_0)>
\frac2{3e}\sqrt{\frac{T_0+1}{2L_c}}=:H_0,
\qquad N>T_0(e^{H_0}-1).
$$

The reproducer records this lower bound in base ten. It concerns this
specific envelope, not a lower bound on the difficulty of beta-binomial
fitting: the same sample is solved exactly by its empirical distribution.

## 5. Exhaustive positive-likelihood escapes at growing size

Here the earlier map's closure conjecture can be settled. Consider any
sequence $N\to\infty$ whose observed likelihood is bounded below by a
positive constant. Then $t\to\infty$: otherwise a subsequence with bounded
$t$ contradicts section 3. Sections 1 and 2 show, for every fixed integer
$x\ge0$, that the BB probability differs by $o(1)$ from
$q_{a,\mu}(x)$ with $\mu=Na/t$.

Take a subsequence along which $a$ has a limit in $[0,\infty]$.

* If $a\to0$, for any observed positive $x$ the NB probability is at most
  $(a)_x/x!\le a$ when $a\le1$. (The two probability-power factors in its
  pmf are at most one, and each remaining rising-product ratio is at most
  one.) This contradicts a positive lower bound on the sample likelihood.
* If $a\to a_0\in(0,\infty)$, then $\mu$ cannot approach zero or infinity:
  the explicit NB pmf tends to zero at any fixed positive $x$ in either
  case, uniformly for $a$ in a compact positive interval. A further
  subsequence has $\mu\to\mu_0\in(0,\infty)$ and the entire law tends to
  NB$(a_0,\mu_0)$.
* If $a\to\infty$, represent the NB as a Poisson mixture with intensity
  $\mu U/a$, $U\sim\operatorname{Gamma}(a,1)$. The same log-Lipschitz and
  Chebyshev argument as section 2 gives uniform-in-$\mu$ absolute error
  at most $d_x(a)$ against Poisson$(\mu)$. A fixed positive observed count
  rules out $\mu\to0$ or infinity. A subsequence therefore converges to
  a Poisson law of finite positive mean.

In the last two cases convergence at every fixed count to a proper law
implies tightness: choose a finite set carrying all but an arbitrary
amount of the limiting law's mass, and pass to the limit on that set.
There is no defective limit with additional mass lost at infinity while
positive sample likelihood persists. Oscillating sequences are covered by
the subsequence argument. The positive-count hypothesis is necessary;
all-zero data allow other paths. This proves the growing-size part of the
closure, without asserting uniqueness or a globally winning face.

For completeness, at bounded integer size pass to a constant-$N$
subsequence and write $p=a/t$. If $t\to\infty$, beta variance
$p(1-p)/(t+1)\to0$ gives the binomial at a limiting $p$. If $t\to0$,
$E[p(1-p)]=ab/[t(t+1)]\to0$, and a limiting mean gives a two-point mixture
at beta endpoints and hence counts $\{0,N\}$. If $t$ tends to a positive
finite value, an interior $p$ gives BB and boundary $p$ gives a point mass.
These cover the compactified parameters. At $N=1$ all shapes already
produce a Bernoulli law. At $N=0$ the count law is the point mass at zero.

## 6. Why an arbitrary Poisson-mixture bound does not settle the hard cases

A tempting shortcut is to replace $L_*$ by the likelihood supremum over
all Poisson mixing laws. Section 1 then supplies a much simpler tail
bound, but this enlarges the face in a material way. For a finite-shape NB
candidate $q$, put

$$
D(\lambda)=\sum_i f_{x_i}(\lambda)/q(x_i)-h.
$$

Under its gamma mixing law, $E D(\lambda)=0$. But $D$ is continuous and
not identically zero, since it tends to $-h$ at infinity. The gamma density
is positive everywhere on $(0,\infty)$, so $D$ is strictly positive
somewhere. At such a point the derivative at $w=0$ of
$\sum_i\log[(1-w)q(x_i)+wf_{x_i}(\lambda)]$ is positive. Therefore a
Poisson-mixture distribution strictly improves on every finite-shape NB
candidate. The numerical witnesses retained beside this derivation
quantify the issue for the map's six difficult extended samples. They
are actual feasible mixtures, not claims to have optimized that larger
class. Their likelihood gains alone obstruct that relaxation when they
exceed the finite BB candidate's gain.

## 7. Sharpening the three coarse steps

The envelope in section 4 is sufficient for the closure theorem but loses
accuracy in three places. The second research pass retains the following
proved improvements; `refine.py` measures them separately from the first pass.

### Second-order gamma cancellation

In section 2, $\Lambda=Np$ and $R$ are independent, since beta ratio and
gamma total are independent. Conditional on $\Lambda=\lambda$, expand
$f_x(\lambda R)$ at $R=1$. The linear term has expectation zero. Also

$$
|y^2 f_x''(y)|
=|x(x-1)-2xy+y^2|f_x(y)
\le x(x-1)+2x(x+1)+(x+1)(x+2)
=:K_x=4x^2+4x+2.
$$

Here $y f_x(y)=(x+1)f_{x+1}(y)$ and
$y^2 f_x(y)=(x+1)(x+2)f_{x+2}(y)$ justify the bounds. On $R\ge1/2$,
the Taylor remainder is at most $2K_x(R-1)^2$. On $R<1/2$, it is at most
$1+C_x|R-1|\le(4+2C_x)(R-1)^2$. The two events are disjoint, so using
$E(R-1)^2=1/t$ gives

$$
|E f_x(\Lambda')-E f_x(\Lambda)|
\le \widetilde d_x(t):=\min\{1,A_x/t\},\qquad
A_x=\max\{8x^2+8x+4,4x+6\}.
$$

No assumption about independence of $U$ and its total is used; the relevant
independence is precisely that of the beta ratio and the gamma total.

### Joint small-concentration optimization

Instead of replacing every $a+j$ by $T+j$, retain the shared alpha. Since
$(a)_x\le a(a+M)^{x-1}$ for $1\le x\le M$, section 3 gives

$$
L(N,a,b)\le
\frac{(N/(N-M))^S}{\prod_i x_i!}
 a^m(a+M)^{S-m}e^{-haH},\qquad H=\log(1+N/T),\quad t\le T.
$$

The logarithm of the last three factors is strictly concave in $a>0$.
Its unique maximum is at the positive root of

$$
hH a^2+(hHM-S)a-mM=0.
$$

Enlarging the domain to all $a>0$ gives a valid upper bound
$\widetilde B_N(T)$. This avoids the artificial factor $K(T)$ that caused
the enormous necessary cutoff in the first pass. For fixed $T$ the bound
still decreases with $N$, because its unmaximized expression does so for
every $a$. For fixed $N$ it increases with $T$.

One can sharpen this step further by retaining the exact rising products:
$\sum_i\log(a)_{x_i}-haH$ is strictly concave, with strictly decreasing
score $\sum_i\sum_{j=0}^{x_i-1}(a+j)^{-1}-hH$ and its root in
$(0,S/(hH)]$. The measured second pass uses the explicit quadratic bound
together with the harmonic bound below, not a numerical root for this
exact-product alternative.

To cancel the large-count factors more tightly, write
$(a)_x/x!=a\prod_{j=1}^{x-1}(1+a/j)/x$ and use
$\log(1+a/j)\le a/j$. Put
$J=\sum_{x_i>0}\sum_{j=1}^{x_i-1}1/j$. If $hH>J$, maximizing over $a$
gives the additional valid bound

$$
B^{\rm harm}_N(T)=
\left(\frac N{N-M}\right)^S
\frac1{\prod_{x_i>0}x_i}
\left(\frac m{e(h\log(1+N/T)-J)}\right)^m.
$$

If $hH\le J$, this bound is taken as infinity. It increases with $T$ and
decreases with $N$. In the final measured envelope, $\widetilde B_N(T)$
means the smaller of this harmonic bound and the quadratic bound above.
Both optimize jointly over all positive alpha, rather than replacing alpha
by the total-concentration cutoff.

### Normalize likelihood error against the empirical upper bound

Let $n_k=\#\{i:x_i=k\}$, $r_k=n_k/h$, and
$L_{\rm sat}=\prod_k r_k^{n_k}$. For any pmf $q$, expand
$\prod_i(q(x_i)+d_i)$. For a subset $J$ of removed observations, let
$j=|J|$ and $j_k$ be its counts. The unrestricted pmf likelihood bound
for the remaining observations gives

$$
\frac{\sup_q\prod_{i\notin J}q(x_i)}{L_{\rm sat}}
\le e^j\prod_k r_k^{-j_k}.
$$

For $j<h$, this follows from
$(h/(h-j))^{h-j}\le e^j$ and
$(n_k-j_k)^{n_k-j_k}\le n_k^{n_k-j_k}$; zero powers have value one.
For $j=h$ the inequality follows directly. Bound each nonempty expansion
term by this inequality, but retain the actual NB likelihood in the empty
term. Summing the terms yields

$$
\prod_i(q(x_i)+d_i)
\le \prod_iq(x_i)+L_{\rm sat}
\left\{\prod_i\left(1+\frac{e d_i}{r_{x_i}}\right)-1\right\}.
$$

The pointwise coupling comparison therefore proves the sharper uniform
integer-tail envelope

$$
\widetilde U_N(T)=\max\left\{
\widetilde B_N(T),\quad e^{G_N}\left[L_*+L_{\rm sat}
\left\{\prod_i\left(1+\frac{e\widetilde d_{x_i}(T)}{r_{x_i}}\right)-1\right\}\right]
\right\}.
$$

It is valid for every $T>0$, decreases in $N$ at fixed $T$, and has the
same limit $L_*$ by taking limits in the same order as section 4. The two
branches respectively increase and decrease with $T$ at fixed $N$.
Their crossing can consequently be found by bisection, without assuming a
likelihood profile is unimodal. The numerical tuning interval in the
reproducer is $\log T\in[-30,100]$, with 65 bisections. An unsearched
crossing outside that interval would require explicit treatment in a
production implementation; the script claims only the recorded numerical
diagnostics. The face values remain numerical candidates, so a numerical
clearance is not a certified fit, even when this stronger bound is small.

The first pass's $10^{586}$ lower cutoff concerns its coarse envelope.
It does **not** apply to this sharper envelope. The separate result files
prevent confusing that rejected bound with the stronger second pass.

There is also an $O(1/N)$ tail consequence for each fixed sample, with a
potentially large data-dependent constant. Set $T=cN$ for fixed $c>0$.
Then $H=\log(1+1/c)$ is constant. By choosing $c$ sufficiently small,
the harmonic bound's limit can be made smaller than any prescribed
positive likelihood, including $L_*$. Meanwhile
$\widetilde d_x(cN)=A_x/(cN)$ for large $N$, and $G_N=O(1/N)$.
The large-concentration branch is therefore $L_*+O(1/N)$, and eventually
dominates the small-concentration branch. This proves a joint-shape
correction without the held-alpha argument's unbounded $a^2$ factor.
It does not bound the constant by anything small enough for a practical
integer enumeration. The recorded hard cases quantify that distinction.
