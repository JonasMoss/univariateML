# Proofs and implementable reductions

This is research for TASK-116.15, on source
`31e2b66d395dd87068e1e204e934298407ea5955`. No claim below certifies the
current fitters. All likelihoods are products of densities (or their total
logarithms), with no parameter-transformation Jacobian. Data are finite,
nonempty, and holds are finite proper coefficients. Comparisons in a future
implementation follow the repository's conditional exact-arithmetic standard.

## 1. Two-piece coordinates for sstd

Put m=nu-2, p=(nu+1)/2, and

\[
 h=\frac{\Gamma((\nu-1)/2)}{\sqrt\pi\Gamma(\nu/2)},\qquad
 K=\frac{2\Gamma((\nu+1)/2)}{\sqrt\pi\Gamma(\nu/2)}.
\]

Use mode c and **denominator widths** a,b>0, so that

\[
 f(x)=\frac K{a+b}\left(1+\frac{(x-c)^2}{a_s^2}\right)^{-p},
 \quad a_s=a\ (x\le c),\quad a_s=b\ (x>c).                 \tag{1}
\]

These are sqrt(nu) times ordinary t half-scales, not standard deviations.
Substitution and half-line integration give exactly

\[
 M=c+h(b-a),\quad
 S^2=\frac{a^2-ab+b^2}{m}-h^2(b-a)^2,\quad \xi^2=b/a.     \tag{2}
\]

Indeed the positive half of the normalized symmetric kernel has zeroth
integral 1/2, first integral h/2, and second integral 1/(2m).
The mixture weights are a/(a+b), b/(a+b); cancellation of a+b gives (2).
Conversely take b=xi² a and solve the strictly positive quadratic variance
coefficient for a, then set c=M-h(b-a). This is a bijection for nu>2.
With w=a+b,d=b-a,

\[
 S^2=\frac{w^2}{4m}+\left(\frac3{4m}-h^2\right)d^2.      \tag{3}
\]

Both coefficients are positive. To prove the second, the unit-variance t
is a normal variance mixture of mean mixing variance one. Jensen's inequality
gives E|Z| <= sqrt(2/pi), hence m h² <= 2/pi < 3/4.
This also proves all variance constraints below are nondegenerate ellipses.

For fixed nu: held M is affine, held S is quadratic, held xi is the linear
constraint b=xi² a. All public hold sets therefore define smooth algebraic
constraints inside a,b>0; their intersections can be parametrized directly
from (2), eliminating held coordinates before solving. M and S are always
public mean and sd, never mode and kernel scale.

## 2. What actually separates, and what does not

On a cell with c between consecutive distinct observations let
D_L=sum log(1+(x-c)²/a²), and define D_R analogously with b.
Then ell=n log K-n log(a+b)-p(D_L+D_R). Data terms separate by side;
normalization couples the widths. It is incorrect to maximize the two side
likelihoods independently.
For example with c and nu fixed, set u=log a,v=log b and

\[
 H_L(a)=(\nu+1)\sum_L\frac{(x-c)^2}{a^2+(x-c)^2}.
\]

The a score is H_L(a)-na/(a+b), and similarly for b.
Each H decreases strictly if that side has nonzero distances. Fixing b,
the first score is strictly decreasing (its other term increases), giving
at most one conditional a maximum. If H_L(0+)>0 it has exactly one root;
if no nonzero left distance, the likelihood improves toward a=0.
The same holds with sides reversed. This proves conditional coordinate
uniqueness, **not** joint concavity or uniqueness after profiling c.

An exact useful 1D reduction at fixed c,nu has no public moment constraints:
write r=a/(a+b), w=a+b. Its log-w score is

\[
 -n+(\nu+1)\sum_i \frac{d_i^2}{w^2 r_s^2+d_i^2},
 \quad r_s=r\text{ or }1-r.
\]

For every r in (0,1), this decreases strictly and has a unique root iff
nu(n-k)>k, k=#{x=c}; bracket it by the min/max nonzero |d_i|/r_s times
sqrt((nu(n-k)-k)/n). At equality the score is negative at every finite w;
below equality the likelihood diverges at w=0. Thus optimize a scalar
ratio profile after the unique scale solve. A held public M or S generally
invalidates fixing c; use the constrained routes below instead.

## 3. Fixed-nu scalar root enumeration, including nonunit skew

### Only public mean free

The widths are fixed and c=M-h(b-a). The score in each c cell is

\[
 (\nu+1)\sum_i\frac{x_i-c}{a_s^2+(x_i-c)^2}.
\]

Clear the positive quadratic denominators: numerator degree <=2n-1.
Isolate **all** real roots in each cell, include observation seams, compare
all maxima. Both location tails have likelihood zero. The likelihood is C1
at each seam (individual residual score zero), so no maxima are missed.
Repeated observations can be grouped with multiplicity. Exact ties are
possible; see section 11.

### Only public sd free

Hold M,nu,xi. Let u=1/S, A=sigma_u (the standardized two-piece core sd),
B=mu_u=h sqrt(m)(xi-1/xi). Set z_i=A(x_i-M)u+B. Between its positive
breakpoints u=-B/[A(x_i-M)], fix chi_i=xi or 1/xi according to sign z_i.
With q_i=m chi_i²+z_i²>0,

\[
 \ell'(u)=n/u-(\nu+1)\sum_i A(x_i-M)z_i/q_i.             \tag{4}
\]

The cleared numerator is
n product q_i -(nu+1)u sum A(x_i-M)z_i product_{j!=i}q_j,
of degree <=2n. There are at most n positive seams, hence <=n+1 cells.
Use Sturm/subresultant isolation with coefficient sign certification; reject
spurious boundary roots, compare every root and seam, and compare u=infinity.
u=0 gives ell=-infinity. At infinity the coefficient of log u is
k-nu(n-k), with k=#{x=M}. Negative gives ell=-infinity; positive gives
unboundedness; zero gives a finite limit calculated term by term in (1).
A constant or degree-deficient polynomial is handled explicitly, not discarded.
No monotone or single-bracket assumption occurs in this algorithm.

### Only xi free, with M,S,nu held

Put r=xi²>0, Q(r)=(1-r+r²)/m-h²(r-1)²>0.
Then a=S/sqrt(Q), b=ra, c=M-hS(r-1)/sqrt(Q).
Adjoin y>0,y²=Q(r). All log-likelihood derivatives are algebraic rational
functions of (r,y); side seams have algebraic equations too. Differentiate
y'=Q'/(2y), clear positive denominators, isolate roots with y>0 and the
original side tests, and compare r=0 and r=infinity with the layer in section 6.
This gives a finite algebraic scalar route, including both held moments.
Squaring without checking y>0 and original equations is invalid.

### Several of M,S,xi free, nu held

Eliminate holds by (2), and use rational constrained scores on each c cell.
At most three free coordinates remain. Enumerate real critical components
with a polynomial-system method, including c=x_j strata and feasible
boundaries. A positive-dimensional critical component is possible: use a
semialgebraic path decomposition; the derivative of ell along every smooth
path in a connected critical component is zero, hence ell is constant on it.
Compare one representative per such component, rather than assuming isolated
roots. Include singular parametrization strata separately (or avoid them
with the original smooth public-coordinate chart). This is a finite candidate
structure, not an assertion that general CAD will be computationally cheap.
Coefficients h,nu need not be algebraic numbers: a numerical implementation
needs reliable signs for their computed values, and must stop on unresolved
coefficient/discriminant signs. A floating-point polynomial roots call is
not a replacement for root counts.

## 4. Uniform exterior exclusion: a lemma for either model

Suppose a family admits, for some C>0,q>1 and a common width w,

\[
 f(x)\le \frac Cw\min\{1,(w/|x-c|)^q\}.                 \tag{5}
\]

Let P0>0 be a feasible incumbent likelihood and delta one third of the
minimum distinct-data spacing. For one distinct datum the strict tie
condition below never holds; treat collapse directly. If |c-y|<=delta
for a data value y of multiplicity k, all other distances >=2delta and

\[
 L\le C^n w^{q(n-k)-n}(2\delta)^{-q(n-k)}.              \tag{6}
\]

Away from all these neighborhoods, L<=C^n w^{n(q-1)}delta^{-qn}.
Thus if every reachable k satisfies q(n-k)>n, a computable positive w_min
excludes all smaller widths uniformly: choose it small enough that each
right side in (6) and the away bound is <P0. Also w_max>C P0^{-1/n}
excludes larger widths. For location tails, maximizing each bound over w
bounds f by a constant/|x-c|, so a finite distance from the data excludes
all location escapes, uniformly in w. Comparisons can be done in log space.
These are data-dependent bounds, not guessed search caps.

If c is held, only the tie at that c is reachable. If public M is held
instead, sstd c->M as widths vanish in the compact shape regimes described
below; using the largest tie everywhere is conservative but safe.

## 5. A complete list of sstd escape coordinates in the strict tie regime

For this section use **ordinary t half-scales** A=a/sqrt(nu),B=b/sqrt(nu),
W=A+B and H=sqrt(nu)h. Formula (1) becomes
2 t_nu((x-c)/A_s)/W. The moment equations are

\[
 M=c+H(B-A),\quad
 S^2=\frac\nu{\nu-2}(A^2-AB+B^2)-H^2(B-A)^2.            \tag{7}
\]

Uniformly for nu>=2,

\[
 t_\nu(z)\le 3(1+z^2)^{-3/2}.                           \tag{8}
\]

Proof: c_nu=t_nu(0)<=1/sqrt(2pi), by Jensen applied to a gamma variable.
For nu>2 the maximum of (1+z²)^(3/2)t_nu(z) occurs at z²=(2nu-1)/(nu-2).
Its value is
c_nu (3nu/(nu+1))^(3/2)
    [nu(nu-2)/((nu-1)(nu+1))]^((nu-2)/2)
<=3^(3/2)/sqrt(2pi)<3. At nu=2 the supremum is the tail limit 1.
More generally, for q>=2 and nu>=q-1, the same differentiation gives
sup_z (1+z²)^(q/2)t_nu(z)<=q^(q/2)/sqrt(2pi).
For nu>q-1, the maximizing z² is ((q-1)nu-1)/(nu+1-q) and its value is
c_nu(q nu/(nu+1))^(q/2)
[nu(nu+1-q)/((nu-1)(nu+1))]^((nu+1-q)/2); the bracket is <=1.
At nu=q-1 take the continuous tail limit (q=2,nu=1 is the constant-ratio
Cauchy case). This supplies the uniform q=3+epsilon or q=2+epsilon
envelopes used to exclude df values separated from the critical collapse
shape in sections 10, 13 and 14.

Therefore (5) holds with C=6,q=3,W for **all skew ratios and nu>2**.
In fact the sharper envelope 6 W²/(W²+(x-c)²)^(3/2) also holds;
its maximum in W is 4/(sqrt(3)|x-c|).

If 3k_max<2n, section 4 confines c,W uniformly, independent of nu,xi.
Consequently every likelihood-relevant escaping sequence has a subsequence
with c finite, 0<W_min<=W<=W_max, r=A/W in [0,1], and nu in [2,infinity].
This proves **exhaustiveness of the following coordinate faces** in this
strict regime, rather than just checking separate fixed-parameter paths:

* nu=2, r interior: ordinary two-piece t2;
* r=0 or 1, finite nu: right/left half-t, with the endpoint layer below;
* nu=infinity, r interior: two-piece normal (snorm after moments);
* intersections: half-t2 and half-normal, with the endpoint layer.

Equation (7) gives the constraints on each face. If S is held, the nu=2
face with W>0 is impossible: A²-AB+B²>=W²/4 and the subtracted term is
bounded on the compact set. More explicitly H²<=2nu/[pi(nu-2)] is too
coarse near two; use H's bounded continuous value H(2)=sqrt(2) on [2,3]
to obtain a quantitative excluded nu neighborhood from (7). If M is held,
its finite face value is always retained. If xi is held, r is a fixed
interior value, so half-faces are absent.

The density converges uniformly on compact parameter sets away from the
thin-side endpoint layer. Its gamma constants and moments have continuous
limits H(2)=sqrt(2), H(infinity)=sqrt(2/pi). These statements follow directly
from the gamma integral at two and its Stirling expansion at infinity.
They prove the coordinate closure; they do **not** supply a floating-point
remainder implementation for all the faces. That remains an explicit
implementation obligation.

A useful explicit normal-kernel bound is global in the residual, so it also
handles arbitrary skew ratios before enforcing held moments. If
s=sqrt(V/nu), V~chi-square_nu, then t_nu(z)=E[s phi(sz)]. The normal
density satisfies sup |d phi(exp(u)z)/du|=2/(e sqrt(2pi)). Therefore

\[
 \sup_z|t_\nu(z)-\phi(z)|\le c_0\sqrt{2/\nu}
 +\frac1{e\sqrt{2\pi}}\sqrt{\psi_1(\nu/2)
             +[\psi(\nu/2)-\log(\nu/2)]^2}.             \tag{8a}
\]

Here E|s-1|<=E|V/nu-1|<=sqrt(2/nu), and the logarithmic moment is
bounded as in (15). Multiply (8a) by 2/W_min for a uniform two-piece
density error at the **same** ordinary coordinates c,A,B. This is a
computable starting point for shape-tail bounds. With public moments held,
the corresponding normal parameters must be remapped using (7); this
remapping is not uniform in residual divided by the shrinking half-scale.
Split off the endpoint neighborhoods and apply section 6 there. Merely
allowing small errors in held moments can spuriously erase the joint-normal
endpoint penalty and is not an adequate certificate.

For held nu>2 the sharper fixed-shape q=nu+1 envelope applies, with a
finite constant C_nu, so  k_max<nu n/(nu+1) suffices instead of 2n/3.

## 6. Endpoint layers of sstd: exact fixed-nu held-moment correction

Let a->0,b->b0>0,c->c0. Observations below c0 make L->0; observations
above c0 converge to the right half-t density. At x=c0, if c-c0=a t+o(a),
the density relative to the closed half-t endpoint is

\[
 F(t)=\begin{cases}(1+t^2)^{-p},&t>0,\\1,&t\le0,\end{cases}              \tag{9}
\]

with F(+infinity)=0. Proof: for t>0 the observation is on the shrinking
left side; for t<=0 it is on the broad side and its residual tends to zero.
All k observations equal to c0 contribute F(t)^k. Reflection gives the other
side. This distinction matters even though the law converges weakly.

For fixed nu and **both moments held**, set
v=1/m-h²>0 and d0=2h²-1/m. The variance constraint is
S²=v(a²+b²)+d0 ab. At a=0 it forces

\[
 b_0=S/\sqrt v,\quad c_0=M-hb_0,\quad
 b'(0)=-d_0/(2v),\quad c'(0)=\rho=h/(2mv)>0.              \tag{10}
\]

The implicit function theorem applies because 2vb0>0. Therefore, when an
observation equals c0, the **unique reachable endpoint multiplier** is
(1+rho²)^(-p), not 1. At nu=3, M=0,S=1 it is about 0.604 (see checks.json
for the evaluated value). Both other moment constraints and positivity
hold along this path. This is a proved counterexample to substituting the
closed half-t endpoint in all held-moment comparisons.

If only M is held, choose b=b0+a and keep c=c0; if only S is held choose
the implicit b(a) and keep c=c0; if neither is held keep c=c0 directly.
Thus in these three cases multiplier 1 is attainable, whenever the limiting
held constraints themselves are feasible. A held finite xi disallows this
face entirely. Half-face endpoints need not always be at the sample minimum:
for example with M held, c=M-hb couples the endpoint to b. Optimize over the
whole feasible face c<=min x, including interior endpoints.

When nu is free and both moments are held, put
j(nu)=sqrt(nu-2)h=E|standardized t|. Then
c0(nu)=M-S j/sqrt(1-j²) is **strictly decreasing** on (2,infinity).
Here is a proof that removes a possible degenerate finite-nu layer:
with x=(nu-1)/2>1/2,

\[
 \frac{d\log j}{d\nu}=\frac1{2(\nu-2)}
    -\frac12[\psi(x+1/2)-\psi(x)]>0.
\]

Indeed the digamma difference is the integral of
exp(-xt)/(1+exp(-t/2)), strictly less than
exp(-(x-1/2)t)/2 integrated over t>0, which equals 1/(2x-1)=1/(nu-2).
Thus c0'(nu) is finite and strictly negative at every finite interior nu.
At a right-half face value nu0, increase nu by O(a) to offset the positive
rho a term in (10) and set c=c0(nu0), or slightly below. The implicit
function theorem applies because c0'(nu0)!=0. Consequently **multiplier 1
is attainable at every finite-nu half face when nu is free**, even with
both moments held. The fixed-nu penalty must not be copied into this search.

At the joint normal corner use the **ordinary** thin half-scale A, not
a=sqrt(nu)A: v_layer=(c-c0)/A. For positive finite v_layer the endpoint
multiplier tends to exp(-v_layer²/2); for v_layer<=0 it is 1. Positive
v_layer->infinity gives zero, uniformly along nu->infinity by (8).
In the normal two-piece face with both moments held put
H=sqrt(2/pi), V=1-H². Implicit differentiation gives rho_N=H/(2V).
Since c0(nu)>c0(infinity) for every finite nu, both the finite-df shift
and the positive thin-scale shift approach the limiting endpoint from the
thin side. Taylor expansion in A, uniformly near 1/nu=0, gives

\[
 \liminf\frac{c-c_0(\infty)}A\ge\rho_N.
\]

Equality is attainable by sending nu to infinity fast enough that
c0(nu)-c0(infinity)=o(A). The **largest reachable endpoint multiplier**
at this corner is therefore exp(-rho_N²/2), also when nu is free.
The reflected corner has the identical multiplier. With only one or neither
moment held, the earlier constructions retain multiplier 1. The nu=2
half-face has no positive-width feasible point when S is held; without
that hold its endpoint multiplier 1 is attainable. This settles the
endpoint suprema throughout the strict-compactness closure, including its
intersections. It does not yet give uniform floating-point neighborhood
error bounds for deciding that a computed face actually wins.

## 7. Conditional st skewness: finite bracket without log-concavity

Write l=xi for st location (to avoid confusing it with sstd skew),
z_i=(x_i-l)/omega, m=nu+1, q_i=z_i sqrt(m/(nu+z_i²)), and

\[
 f_i=2t_\nu(z_i)T_m(\alpha q_i)/\omega,\quad
 U(\alpha)=\sum_i q_i R_m(\alpha q_i),\quad R_m=t_m/T_m.    \tag{11}
\]

If all nonzero q have one sign, ell is strictly monotone in alpha, so no
finite skew MLE exists: return the corresponding face stop. If all q=0,
ell is constant in alpha: structural nonidentifiability, with every alpha
a maximizer. These conclusions hold with the nuisance coordinates held.

If both signs occur, both skew tails have ell->-infinity. There is an
explicit finite bracket containing **all** stationary points. For a>0,

\[
 T_m(-a)\le t_m(a)\frac{m+a^2}{ma},\qquad
 R_m(-a)\ge\frac{ma}{m+a^2}.                              \tag{12}
\]

Proof: differentiate B(a)=t_m(a)(m+a²)/(ma); B'(a)=-t_m(a)(1+a^-2),
and B(infinity)=0. Integration gives B(a)>=T_m(-a).
For positive arguments T_m>=1/2, hence R_m(a)<=2 c_m(1+a²/m)^(-(m+1)/2).
For alpha>=sqrt(m)/min_{q!=0}|q|, write n_- for the number of negative q and

\[
 C_+=2c_m m^{(m+1)/2}\sum_{q_i>0}q_i^{-m}.
\]

Then U(alpha)<=C_+ alpha^(-(m+1))-m n_-/(2alpha).
Thus for

\[
 A_+=\max\left\{\sqrt m/\min|q|,
       (4C_+/(m n_-))^{1/m}\right\},                     \tag{13}
\]

U is strictly negative for every alpha>=A_+. Reflect q to obtain A_- and
a strictly positive score for alpha<=-A_-. Compute in logarithms to avoid
overflow. This is tail exclusion, not a claim of score monotonicity.

On [-A_-,A_+] the score is real analytic. It has finitely many zeros unless
identically zero: infinitely many zeros in a compact interval would have
an accumulation point in its analytic domain. The tail signs preclude the
identically-zero case. Use interval/Taylor root counts, including multiple
zeros, or certified function upper bounds on all unresolved subintervals.
A sign-change grid misses even-multiplicity stationary points. This gives
a complete finite-dimensional conditional route; an efficient root-count
implementation for the Student CDF is still required.

Log T_m is **not globally concave**: R'_m(h)=-R_m(h)[(m+1)h/(m+h²)+R_m(h)].
For h->-infinity, R_m(h)~m/|h|, so R'_m(h)~m/h²>0. The check at m=4,h=-100
confirms this numerically. Concavity of normal log-CDF cannot be imported.

## 8. Uniform nuisance compactness and complete st skew-face values

For nu>=1,

\[
 t_\nu(z)\le(1+z^2)^{-1},\quad
 f(x)\le2\omega/(\omega^2+(x-l)^2).                     \tag{14}
\]

Proof: the maximum of (1+z²)t_nu(z) is at |z|=1 (constant for nu=1),
and is <=2c_nu<=sqrt(2/pi)<1. Skewing multiplies density by at most two.
Hence section 4 applies with C=2,q=2, uniformly in alpha,nu>=1.
For 2k_max<n a feasible incumbent confines l and omega to an explicit
compact box. For location, each density <=1/|x-l| after maximizing (14)
in omega. The same proof applies with a held l using only its reachable tie.
For a fixed held 0<nu<1, maximize (1+z²)^((nu+1)/2)t_nu(z) to obtain a
finite C_nu and use q=nu+1; strict nu(n-k)>k is sufficient. It is not
necessary when scale is held.

Now restrict l,omega to that compact set and nu to a finite interval bounded
away from zero. As alpha->+infinity any limiting l>min x makes L->0.
For l<min x, the limit is the right half-t likelihood. At l=min x with k
ties, put t=alpha(min x-l)/omega. The endpoint factor is
[2T_(nu+1)(t sqrt((nu+1)/nu))]^k times [t_nu(0)/omega]^k.
If **l is free**, choose l=min x-alpha^-1/2 (omega finite) to attain the
closed half-t endpoint factor 2^k. If **l is held at min x**, t=0 identically
and the attainable factor is 1, so divide the closed half-t likelihood by
2^k. If held l<min x use the ordinary half-face; if held l>min x its
likelihood is zero. Reflection covers negative alpha. Holds on omega or
nu impose no extra coupling on these location perturbations. This proves
all skew-infinite face suprema under the direct public holds of st.

A finite likelihood lower bound supplies a finite skew cutoff on nuisance
boxes where at least one residual of each sign is separated from zero,
using (12)-(13). Boxes crossing an endpoint require the layer envelope just
given, not a cutoff based on a nearly zero residual.

## 9. Uniform st normal-face error, even with alpha unbounded

On the compact nuisance box let |z|<=Z and omega>=w0. A computable bound
valid for any alpha prevents a hidden alpha/nu corner. Put c0=1/sqrt(2pi),
C0=1/sqrt(2pi e), m=nu+1, and

\[
 E_\nu=|\log(t_\nu(0)/c_0)|+\frac{Z^2}{2\nu}
                  +\frac{(\nu+1)Z^4}{4\nu^2},\quad
 D_\nu=c_0(\exp(E_\nu)-1).
\]

The inequalities u-u²/2<=log(1+u)<=u show
sup_{|z|<=Z}|t_nu(z)-phi(z)|<=D_nu. For V~chi-square_m,
T_m(t)=E Phi(t sqrt(V/m)). Differentiation with respect to log scale gives
sup_t |Phi(at)-Phi(t)|<=C0 |log a|. Cauchy--Schwarz therefore gives

\[
 \sup_t|T_m(t)-\Phi(t)|\le e_m
 =\frac{C_0}2\sqrt{\psi_1(m/2)+[\psi(m/2)-\log(m/2)]^2}.   \tag{15}
\]

Applying the same scale bound to
r=sqrt((nu+1)/(nu+z²)) gives a uniform skew-factor error

\[
 d_\nu=e_{\nu+1}+\frac{C_0}2
 \max_{|z|\le Z}|\log((\nu+1)/(\nu+z^2))|.
\]

The maximum is attained at z²=0 or Z². Thus the st density and its
skew-normal limit differ by at most
eta_nu=(2/w0)(D_nu+c0 d_nu), **uniformly over all real alpha**.
Both densities are bounded by B=2c0/w0, so telescoping their product gives
|L_st-L_sn|<=n B^(n-1) eta_nu. The error tends to zero; use certified
gamma/digamma/trigamma bounds or their explicit remainder expansions to
turn this into a tail supremum for all nu>=N. A pointwise evaluated error
at N is not automatically a tail supremum.

This proves the normal face covers every joint alpha/nu escape once nuisance
compactness holds. The skew-normal face itself must be globally profiled
under the inherited holds and its half-normal endpoint convention from
section 8. An uncertified call to the current sn optimizer is insufficient.

## 10. Collapse equalities and where compactness genuinely fails

These cases are outside the strict inequalities in sections 5 and 8; they
must not be silently classified using the compact-box result.

For fixed nu in st and a reachable tie y of multiplicity k=nu(n-k), set
l=y+omega t, omega->0. With alpha finite,

\[
 L\to [2t_\nu(t)T_{\nu+1}(-\alpha t\sqrt{(\nu+1)/(\nu+t^2)})]^k
 \prod_{x_i\ne y}\left[2c_\nu\nu^{(\nu+1)/2}|x_i-y|^{-(\nu+1)}
 T_{\nu+1}(\operatorname{sgn}(x_i-y)\alpha\sqrt{\nu+1})\right]. \tag{16}
\]

If l is held at y set t=0; if free, t is another profile coordinate.
As |t|->infinity the bound from t_nu(t)^k forces this limit to zero, so
only bounded t matters for a positive boundary incumbent. Infinite alpha
requires the endpoint-layer limits from the CDF factors; simply setting
alpha=infinity before t->0 loses attainable values. Formula (16) is a
concrete two-dimensional collapse-face problem, not a decision that the
supremum is globally best. The analogous sstd fixed-nu formula follows
by substituting a=W r,b=W(1-r),c=y+W t in (1) and canceling the zero total
power of W; it retains the ratio r and layer t, with moment holds from (2).

For free st nu>=1 and k=n/2, any nonzero collapse limit must have nu->1:
for nu>=1+epsilon the same envelope argument with exponent 2+epsilon
forces zero (a finite uniform Student envelope exists on this df range).
If lambda=(nu-1)|log omega| has a finite limit, (16) at nu=1 is multiplied
by exp[-(n-k)lambda]<=1; lambda=infinity gives zero. Thus the best df-rate
factor is attained at lambda=0. Finite t,alpha limits are fully described
by (16); section 14 supplies the sign-case argument needed to control the
remaining t,alpha,nu rates. Quantitative implementation bounds still have
to be constructed from this reduced objective.

For sstd free nu>2, the critical tie k=2(n-k) similarly requires nu->2
on a nonzero ordinary-scale collapse path, and introduces
exp[-(n-k)(nu-2)|log W|]. Public S holds additionally fix the leading
relation between W and sqrt(nu-2) through (7). Public M holds constrain
t and the skew ratio. The scalar reduction in section 13 below settles these sstd corner
suprema; numerical remainder bounds and the global comparison still have
to be implemented. For k above the critical count, fixed skew
and suitable finite shape, or nu->2 when S is held, gives unboundedness
when the concentration point is reachable. For a held fixed nu, use its
own threshold, not the free-shape threshold. Scale held in st rules out
scale collapse altogether; sd held in sstd does not.

## 11. Counterexamples and an information correction

For x={-A,A}, held symmetric skew, and fixed kernel denominator D,
maximizing location likelihood minimizes
P(l)=l^4+2(D-A²)l²+(D+A²)².
For A²>D there are exactly two global maxima l=+-sqrt(A²-D), and the
midpoint is a local minimum. Set A=2,D=1: sstd with S=1,nu=3,xi=1;
st with omega=1,nu=1,alpha=0. Both have maxima +-sqrt(3). This is an
analytic counterexample, not just numerical multiple starts.

The old six-point fixed-skew sstd scale example is strengthened here:
check.py uses rational interval arithmetic, Machin's identity and alternating
series remainders to enclose pi, with nu=3,xi=10,M=0. At four rational
v=sigma_u/S values the scale scores have signs +,-,+,- in decreasing v
order (increasing log S). Rational data are the displayed decimal values
of the old fixture, rather than unspecified real numbers. Continuity gives
at least three stationary points and two separated local-maximum branches.
It proves non-unimodality, not global optimality or exhaustive root count.
The actual interval endpoints are retained in checks.json.

At alpha=0 and finite nu in st, location and skew scores are proportional
to z/(nu+z²) and z/sqrt(nu+z²). Their ratio is nonconstant; their finite
second moments give a positive-definite 2x2 block. This disproves a blanket
claim that finite-nu st has the same location/skew singularity as sn at
symmetry. It does not prove full four-parameter DQM or full information rank.
No inference implementation is changed by this research.

## 12. Certificate protocol for the follow-up

1. Validate data and holds; compute collapse counts and a feasible incumbent.
   Use the correct conditional reachability and df threshold.
2. Derive the finite nuisance bounds above. Do not use them at equality.
3. Partition sstd at c=x_j; eliminate public holds with (2). With held nu,
   isolate scalar roots or constrained polynomial critical components. With
   free nu, enclose a compact shape interval and compare the proved faces.
4. For st, profile alpha with (13) when residual signs are separated;
   otherwise use boxes and the endpoint envelope. Cover every remaining
   compact box by likelihood upper bounds, subdividing unresolved boxes.
5. Use explicit normal-face error bounds and compatible half-face profiles.
   Reject a face only with a uniform upper bound below an interior witness;
   declare an escape only after its feasible supremum beats all interior
   and other face bounds. A face equality without attainment resolution stops.
6. Maintain lower/upper log-likelihood bounds, isolated-root counts and all
   competing witnesses. Flat components and exact ties are legitimate outcomes.
   Coefficient uncertainty, unresolved layer feasibility or singular root
   counts produce numerical-failure stops, never a successful capped fit.
7. Certificate boxes/root-count evaluations use 128*min(iterlim,100000),
   solver refinements min(iterlim,100000). Retain witnesses on budget stops.
   No fixed truncation in nu,alpha,xi or log scale is an answer.

Compactness and continuity give convergence of refining upper/lower bounds
for strict-separated optima, provided the chosen enclosures converge and all
boundary layers are represented. They do not promise finite termination on
exact ties, degenerate roots, or unresolved endpoint equalities. The open
entries in the companion inventory identify which numerical enclosures and global
comparisons are needed before advertising a general family certificate.

## 13. Closing the sstd collapse-equality face

The sstd equality problem in section 10 admits a further exact reduction.
This section derives the scalar sstd face profile; section 14 supplies
the complementary st sign-case reduction.

Use denominator widths a=W r,b=W(1-r), r in [0,1]. Let y be a reachable
tie with k=nu0(n-k), and let n_L,n_R count the other observations below
and above y. Put p0=(nu0+1)/2 and K0=K(nu0). For fixed nu=nu0>2,
with S free, the complete collapse-face supremum is the maximum over
feasible r of

\[
 K_0^n\!\prod_{x_i\ne y}|x_i-y|^{-(\nu_0+1)}
 r^{(\nu_0+1)n_L}(1-r)^{(\nu_0+1)n_R} J(r)^k.          \tag{17}
\]

If M is free, J=1: the tied kernel factor is at most one and c=y attains
it. If M is held, the only reachable y is M and
c=y-h W(1-2r). Therefore let t=-h(1-2r)=(c-y)/W,
use r_t=r when t>=0 and r_t=1-r when t<0, and set
J=(1+(t/r_t)²)^(-p0). At r=1/2, t=0 and J=1. At r=0 or 1 the tied
observations are on the **broad** side, so J extends continuously as
(1+h²)^(-p0). Set 0^0=1 for a side containing no off-tie observations.
If xi is held, r=1/(1+xi²); otherwise optimize r in [0,1]. The derivative
of log (17) is rational on each side of r=1/2, so all stationary points
and endpoints are obtainable by polynomial root isolation. Without a held
M the optimal r is simply n_L/(n_L+n_R), including its boundary cases.
A held S and fixed nu preclude W->0, so this face is absent there.

Proof of completeness: outside a fixed neighborhood of y the off-tie
residuals stay uniformly separated from zero. Expanding their kernels in
(1) gives their powers of W and r_s with a relative error tending uniformly
to zero, also when r_s->0. The total W power is zero. The tied kernels
are <=1, maximized at c=y if M is free; with M held the above t is forced
exactly. Paths with |c-y|/W->infinity have tied kernel factor tending to
zero and cannot improve a positive face incumbent. The resulting upper
profile is continuous on the closed r interval, and every finite-r value
and boundary supremum is attainable by positive-width approximations.

For **free nu>2**, k=2(n-k) makes nu0=2 the only nonzero collapse shape:
a uniform q=3+epsilon Student envelope excludes all nu>=2+epsilon.
Use (17) with nu0=2, h(2)=1, K(2)=1. The extra factors are

\[
 W^{(\nu-2)(n-k)}
 r^{(\nu-2)n_L}(1-r)^{(\nu-2)n_R}\le1
\]

for W<=1. The coefficients and held-M tied factors converge uniformly
near two. Thus no coupled df/ratio rate exceeds (17). Its supremum is
attainable by taking nu-2 small enough that (nu-2)|log W|->0 and, if
needed, making it small relative to the logs of the ratio. If S is held,
the exact variance constraint gives

\[
 W^2=\frac{(\nu-2)S^2}
 {r^2-r(1-r)+(1-r)^2-(\nu-2)h^2(1-2r)^2}.             \tag{18}
\]

The denominator tends uniformly to a positive function >=1/4, hence
(nu-2)|log W|->0 automatically. Choose r tending to its desired maximizer
slowly enough to retain the other rate factors. Equation (18) enforces
S exactly, and the mode enforces any held M exactly. This proves the
same collapse supremum with held S. When xi is held simply use its fixed
ratio. Repeating (17) over every reachable equality tie exhausts these
collapse faces; the retained coordinate closure handles positive W.

This solves the sstd **mathematical equality-face value**, not the global
comparison or its numerical tolerance. A future fitter must still compare
this finite supremum with every interior/other-face candidate, and resolve
attainment separately. It must not declare “no MLE” merely because a
collapse limit exists. Uniform, computable remainder constants for finite
W and nu cutoffs are an implementation obligation; the displayed factors
and separated off-tie residuals provide their concrete starting point.

## 14. Closing the st collapse-equality layers by sign cases

The uniform moving-layer question in section 10 has a sign-case solution
at the level of the mathematical face supremum. Formula (16) is the exact reduced
objective at held nu with k=nu(n-k), or at nu0=1 for free nu and k=n/2.
Let C_y denote its product of off-tie **base** constants,

\[
 C_y=\prod_{x_i\ne y}c_{\nu_0}\nu_0^{(\nu_0+1)/2}
                           |x_i-y|^{-(\nu_0+1)}.
\]

If alpha is held, its off-tie skew factors in (16) are fixed. If l is free,
maximize the single tied factor
2t_nu0(t)T_(nu0+1)(-alpha t sqrt((nu0+1)/(nu0+t²)))
over t in R; if l is held, t=0. Its tails vanish, uniformly bounded by
2t_nu0(t); a positive incumbent therefore supplies a finite t bracket.
Analytic root counts or 1D upper bounds give a concrete scalar algorithm.

If alpha is free, distinguish the off-tie data signs:

* **Both signs present:** for bounded t, either sign of sufficiently large
  alpha makes at least one off-tie skew factor tend to zero uniformly.
  Its power tail bounds follow from (12) and the bound on t_m(a).
  All other skew factors are <=2 and tied base density <=c_nu0. Thus a
  positive face incumbent supplies finite alpha bounds. Large |t| is
  excluded by t_nu0(t)^k, uniformly in alpha. This leaves a compact 2D
  profile (t,alpha), or 1D alpha with held l (t=0). Retain every stationary
  branch with certified compact upper bounds.
* **All off-tie values on one side of y:** the global face supremum is
  2^n c_nu0^k C_y if l is free; with l held at y it is
  2^(n-k) c_nu0^k C_y. These bounds follow from T<=1 and
  t_nu0(t)<=c_nu0. For free l take alpha toward the correct infinity,
  t toward zero from the favorable side, and |alpha t|->infinity. For
  held l, t=0 forces the tied skew factor to one. The bounds are attained
  as path suprema, so no additional compact optimization is needed.

Here multiplicities are included in the products and in k. There is no
“no off-tie values” equality case for a nonempty sample: k=n cannot equal
nu(n-k). Feasibility of y is just y=l when location is held, otherwise any
sample value; holds on omega exclude collapse completely.

For completeness, the off-tie expansion used in (16) is uniform over alpha
in the necessary sense. For fixed df in a compact positive interval, the
Student CDF obeys sup_h |h R_m(h)|<infinity, uniformly in that df interval:
at negative infinity this tends to m, at positive infinity to zero, and
on finite intervals it is continuous. Multiplying its argument by
1+O(omega²) therefore changes its log by O(omega²), uniformly over all
alpha. With bounded t, the off-tie q arguments differ from
sgn(x-y)sqrt(nu+1) by just this relative factor. The base Student tail
expansion is likewise uniform in bounded t. Large t gives zero in the
complete product by its tied kernel bound and the nonnegative collapse
exponent. This proves the fixed-df reductions, including alpha layers.

With free nu, any nonzero collapse limit has nu->1, as shown in section 10.
On compact t,alpha sets, all reduced factors converge uniformly to (16) at
one; the df/scale rate multiplies by exp[-(n-k)lambda]<=1. If alpha escapes
and off-tie signs are mixed, a wrong-side CDF tends to zero uniformly for
nu near one: its argument has magnitude bounded below by a positive
constant times |alpha|, and Student tails in this compact df interval
have a uniform vanishing bound. If signs are one-sided, the explicit upper
bounds above apply uniformly and converge to their nu0=1 values. Large t
is uniformly negligible. Every resulting maximum or supremum is attainable
with lambda=0 (nu=1 is itself allowed, or approach it sufficiently fast).
Thus the sign cases exhaust the moving-alpha/df layers at equality.

This supplies a complete **mathematical equality-face route** for st.
Quantitative cutoff constants, root-count arithmetic, comparison with finite
maxima and exact tie handling still belong to the implementation. It does
not assert that equality forces non-attainment, or that either family has
a unique all-free maximum.
