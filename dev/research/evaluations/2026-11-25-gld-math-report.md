# FKML likelihood existence, escapes and structure

TASK-241 · 2026-11-25 · source revision `eaea17a1a8072dadc1b65d65236e534ff7e0790d`

The unrestricted likelihood is unbounded for every nonempty sample. On
\([-1,1]^2\), an MLE exists whenever every multiplicity is strictly below
half the sample size. A distinct pair has an exact uniform MLE on this
branch. If positive shapes above one are wanted, a separated extension
admits exactly two types of infinite-shape faces, a GPD and its reflection.
Full-model differentiability in quadratic mean holds exactly when both
shapes are below \(1/2\). The squared interior score has a different
threshold because positive endpoint density also breaks regularity through
support motion.

The starting results are the [FKML roadmap](../../roadmap/2026-11-08-quantile-defined-families.md#generalized-lambda-fkml),
[Tukey lambda dossier](../../../univariateML-cpp/include/cppunivariate/distributions/tlambda/README.md),
[Tukey lambda likelihood analysis](2026-11-07-tlambda-theory-report.md), and
[Tukey lambda moving-support analysis](2026-11-04-infer-moving-report.md#tukey-lambda).
The following claims are proved unless marked numerical.

## 1. Unbounded likelihood and collapse

Write \(a=\lambda_3,\ b=\lambda_4,\ \sigma=1/\lambda_2,\ \mu=\lambda_1\),
\(v=1-u\), and
\[
T_a(u)=-\int_u^1 t^{a-1}\,dt,\qquad
q(u)=T_a(u)-T_b(v),\qquad D(u)=q'(u)=u^{a-1}+v^{b-1}.
\]
Then \(Q=\mu+\sigma q\) and \(f(Q(u))=1/(\sigma D(u))\). For finite shapes
the lower endpoint is \(\mu-\sigma/a\) if \(a>0\), and is unbounded
otherwise. The upper endpoint is its reflected counterpart. Use the
one-sided interior density at a finite support endpoint and zero outside.
The lower endpoint density is \(0,\ 1/(2\sigma),\ 1/\sigma\) as \(a\)
lies in \((0,1)\), equals \(1\), or exceeds \(1\), respectively. In
particular, any fixed finite shapes have bounded standardized density.

**Symmetric spike.** The diagonal is exactly Tukey lambda. Apply its
proved spike path with \(a=b=m,\ \mu=t\) at an observed value, and
\(\sigma=mA\) with \(A>\max_i |x_i-t|\). If \(k\) observations equal \(t\),
the log likelihood is \(km\log 2-n\log m+O(1)\). This already proves
unboundedness for every nonempty sample, including distinct data.

**A one-shape spike with fixed scale.** Let \(b>1\) be fixed, \(a=m\to
\infty\), and fix \(\sigma>0\). Set \(u_m=1-m^{-1/2}\) and
\(\mu_m=x_{\max}-\sigma q(u_m)\). Then \(D(u_m)\sim m^{-(b-1)/2}\), since
the first term decays exponentially in \(\sqrt m\), and
\(\mu_m\to x_{\max}-\sigma/b\). Choose \(\sigma>b(x_{\max}-x_{\min})\).
All observations below \(x_{\max}\) converge to interior coordinates of
the limiting GPD. This follows by bracketing their inverses at fixed
\(u\) values on either side of the limiting inverse. Their density factors
converge to positive finite numbers. The log likelihood is
\(k_{\max}(b-1)\log(m)/2+O(1)\), with \(k_{\max}\) the multiplicity at the
maximum. Reflection gives the other spike. With either shape at most one,
\(D\ge1\), which rules out divergence at a scale bounded away from zero
in that region. Across \(a=1\) there is a distinct contact issue even for
finite shapes. At the lower endpoint the density for \(a>1\) tends to
\(1/\sigma\) as \(a\downarrow1\), whereas at \(a=1\) the endpoint density
is \(1/(2\sigma)\). The contact can be maintained by adjusting \(\mu\).
The likelihood need not be upper semicontinuous across this junction.

**Fixed-shape collapse, including mixed tails.** Set
\(\mu=t-\sigma q(u_0)\) for a fixed \(0<u_0<1\) and let \(\sigma\downarrow
0\). Write \(k\) for the multiplicity of \(t\) and \(n_-,n_+\) for the
counts strictly to either side. If the left shape is \(a=-A<0\), then
\(q(u)\sim-u^{-A}/A\) and \(D(u)\sim u^{-A-1}\) as \(u\downarrow0\).
At each observation \(x<t\), with \(r=1+1/A\),
\[
 f(x)\sim A^{-r}|x-t|^{-r}\sigma^{1/A}.
\]
Similarly \(b=-B<0\) gives a factor with exponent \(1/B\) on the right.
The tied factors are \(1/(\sigma D(u_0))\). If every occupied side is
heavy, the likelihood is asymptotic to a positive constant times
\(\sigma^{n_-/A+n_+/B-k}\), where an empty side contributes zero for any
shape. A zero exponent gives a finite path value. A negative exponent
gives unboundedness and a positive exponent gives zero. An occupied bounded
side is infeasible eventually. An occupied exponential side contributes
\(O(\sigma^{-1}\exp(-c/\sigma))\) for some \(c>0\), from
\(q(u)=\log u+O(1)\) or its reflection, so the product tends to zero. A
fixed endpoint \(u_0=0\) or \(1\) with positive endpoint density can also
anchor collapse when the occupied side is heavy. For instance, sample
\((0,1,2)\), \((a,b)=(1,-4)\), and \(\mu=\sigma\) put 0 on the lower
endpoint and give exponent \(-1/2\). Thus a bounded side and a sufficiently
heavy opposite tail already cause unboundedness on distinct data. One may
also obtain unboundedness for any sample by using sufficiently negative
fixed shapes and collapsing at any observed value.

## 2. Fitted branch and complete escape inventory

### Compact branch

**Theorem.** On \(K=[-1,1]^2\), with free location and scale, the
likelihood attains its positive maximum if the largest exact multiplicity
\(k_*<n/2\). Every maximizing sequence has a subsequence converging to a
finite FKML maximizer with positive scale. All parameter cluster points are
such maximizers. No point-mass or GPD escape competes under this condition.

For the proof, the following common bounds also cover the separated
extension below:
\[
 g(z)\le 1,\qquad z^2g(z)\le9,\qquad
 f(x)\le \min\left\{\frac1\sigma,\frac{9\sigma}{(x-\mu)^2}\right\},
 \qquad g(q(u))=D(u)^{-1}.
\]
They apply whenever both shapes are at least \(-1\), at least one is at
most 1, and \(+\infty\) is allowed under the convention that its term is
omitted. In fact \(D\ge1\) from the shape at most 1. For \(u\le1/2\),
\(|T_b(v)|\le \int_{1/2}^1 t^{-2}\,dt=1\). If
\(w=a-1\in[-2,0]\), then for \(t\in[u,1]\),
\(t^w u^{-w/2}\le\max(1,u/t^2)\). Its integral is at most 2, so
\(|T_a(u)|\le2u^{w/2}\le2\sqrt D\). If \(a>1\), then
\(|T_a(u)|\le1\), including \(T_\infty=0\). It follows that
\(|q(u)|\le3\sqrt D\). Reflect for \(u\ge1/2\) and pass to the finite
endpoints by limits.

Take a positive incumbent \(L_0\), available from the uniform fit.
For \(\sigma\le1\), let \(\delta\) be half the least gap between distinct
sample values. At most \(k_*\) observations have \(|x_i-\mu|<\delta\);
for every other observation the factor is at most \(9\sigma/\delta^2\).
Thus
\[
 L\le C \sigma^{n-2k_*},\qquad C=\max(1,9/\delta^2)^n.
\]
Along \(L\ge L_0\), this gives the lower scale bound
\(\min(1,(L_0/C)^{1/(n-2k_*)})\). The bound \(L\le\sigma^{-n}\) gives the
upper scale bound \(s_{\max}=L_0^{-1/n}\). If \(|\mu|>\max_i|x_i|+M\),
then \(L\le(9s_{\max}/M^2)^n\), excluding arbitrarily large locations.
These arguments exclude all nuisance escapes together, regardless of how the
shapes vary. In particular, even though vanishing scale with bounded location
can converge weakly to a point mass, its likelihood tends to zero.

For upper semicontinuity on the remaining compact set, consider a convergent
sequence of parameters and positive density factors. Pass to a subsequence
along which their inverse probabilities converge in \([0,1]\). Interior
inverses and densities are continuous. If \(u\to0\) and \(a\to a_0\le0\),
the standardized quantile tends to \(-\infty\), by bounding \(T_a(u)\)
above with \(T_a(\epsilon)\) for each fixed \(\epsilon>0\) and then taking
\(\epsilon\downarrow0\). If \(a_0>0\), \(T_a(u)\to-1/a_0\) and the
observation converges to the endpoint. For \(0<a_0<1\), \(D\to\infty\).
For \(a_0=1\), the approach on \(K\) has \(u^{a-1}\ge1\), so
\(\liminf D\ge2\), the endpoint value. The right endpoint is symmetric.
Together with the uniform density bound this gives upper semicontinuity
for the product. Compactness now proves the theorem.

**Tie boundary on \(K\).** A strict majority tie gives unbounded collapse
at \((-1,-1)\). For \(k_*=n/2\) and at least two distinct observations,
the only positive-likelihood noncompact sequences have, on a subsequence,
\(\sigma\to0\) and \(\mu\to t\) for a half-tied value \(t\). The largest
possible limiting likelihood there is
\[
 F_t=c_t^k\prod_{x_i\ne t}|x_i-t|^{-2},\qquad
 c_t=\begin{cases}
  1/8,&t\text{ has observations on both sides},\\
  1/2,&t\text{ is a sample extreme}.
 \end{cases}
\]
Indeed, if an occupied side has a limiting shape strictly above \(-1\),
its standardized \(z^2g(z)\) tends to zero as \(|z|\to\infty\) on that
side, uniformly near the limiting shape. To see this on the left, restrict
\(a-1\ge-2+\eta\) for some \(0<\eta<1\). The integral bound in the
compactness proof can then use \(\max(1,(t/\sqrt u)^{-2+\eta})\), whose
integral over \([u,1]\) is bounded by
\(1+u^{\eta/2}/(1-\eta)\), and whose integral over \([u,\epsilon]\) is
bounded by \(\epsilon+u^{\eta/2}/(1-\eta)\). On \([\epsilon,1]\) the
original integrand \(t^{a-1}/\sqrt D\) tends uniformly to zero if
\(D\to\infty\). On a tail with \(z\to-\infty\) this holds since
\(a\) must tend to a nonpositive shape and \(u\to0\). Let
\(\epsilon\downarrow0\). For \(a\to-1\), the exact expansion yields
\(z^2g(z)=(1+o(1))u^{a+1}/a^2\le1+o(1)\), uniformly along the sequence.
Multiplying by the common envelope for the other factors proves that a
positive collapse limit forces every occupied side to its \(-1\) bound.
If both sides are occupied, Jensen's inequality yields
\(\liminf\inf_u D(u)\ge8\) as \(a,b\to-1\). If just one side is occupied,
\(D\ge2\) on \(K\). The bounds give \(F_t\); the collapse paths from
section 1 realize it at \((-1,-1),\ \mu=t\) for an interior tie, or
\((1,-1),\ \mu=t+\sigma\) for a minimum, with reflection for a maximum.
Consequently a positive finite candidate strictly above all \(F_t\) proves
attainment. With two equally occupied values a uniform candidate gives
this improvement by a factor \(2^k\). The general finite-versus-collapse
comparison is open.

### A separated extension with GPD faces

For an optional larger fitting domain, take \(K\) together with
\([2,\infty)\times[-1,1]\) and \([-1,1]\times[2,\infty)\). Any fixed lower
cutoff greater than 1 can replace 2. The separation preserves upper
semicontinuity at the finite contact \(a=1\) or \(b=1\). Compactify the
added pieces by adjoining \(+\infty\) in their unbounded coordinate, using
\(1/a\) or \(1/b\) as its coordinate. The scale and location bounds apply
on this full compactification under the same strict tie condition.

| Limiting shapes on the extension | Limiting model |
|---|---|
| Both finite | A finite FKML member, with the same contact conventions. |
| \(a=+\infty,\ -1\le b\le1\) | GPD with threshold \(\mu\), scale \(\sigma\), and shape \(\xi=-b\). |
| \(b=+\infty,\ -1\le a\le1\) | Reflected GPD with upper threshold \(\mu\), scale \(\sigma\), and shape \(\xi=-a\). |

For \(a\to\infty\), \(T_a\to0\) uniformly with error at most \(1/a\),
and the first term of \(D\) tends to zero at every interior \(u\). The
limiting quantile is \(\mu-\sigma T_b(v)\), the claimed GPD. Its left
endpoint density is \(1/\sigma\), and its right endpoint density is zero
if \(0<b<1\) and \(1/\sigma\) if \(b=1\). To prove upper
semicontinuity at a finite \(a>1\) from this shape branch, the left
endpoint has \(D\to1\) as \(u\to0\). The same is true at \(a=+\infty\).
At the right endpoint the cases \(b<1\) are as on \(K\). At \(b=1\),
\(v^{b-1}\ge1\). The left term tends to 1 if \(a\) has a finite limit
and is bounded below by zero if \(a\to\infty\). These are the required
endpoint lower bounds for \(D\).

Every positive face likelihood is approachable from the finite family.
For \(b<1\) hold the nuisance coefficients fixed and let \(a\to\infty\).
At the lower contact \(x=\mu\), the inverse tends to 0 and \(D\to1\).
All other positive factors are interior. For \(b=1\), let
\(\sigma_a=\sigma(1+a^{-1/2})\) and keep \(\mu\) fixed. At the upper
contact \(x=\mu+\sigma\), the inverse satisfies
\(u=(1+a^{-1/2})^{-1}+O(1/a)\) since \(q(u)=u+T_a(u)\).
Thus \(a(1-u)\to\infty\) and \(D\to1\). At the lower contact and interior,
the previous arguments still apply. At fixed scale the upper contact would
instead have \(D=2\) at every finite \(a\). This is a density boundary
layer even though the quantile error tends uniformly to zero. Reflection
gives recovery at the other face.

The maximum over the compactification therefore equals the supremum over
finite parameters. Every cluster point of a maximizing sequence is a
listed maximizing parameter, by the nuisance bounds and upper
semicontinuity. Every listed maximizing parameter is reachable by a
maximizing sequence, by constant or recovery sequences. This proves
completeness on the extension for strict ties. A finite MLE exists if and
only if at least one compactified maximizer has finite shapes. For example,
a winning uniform face already has an exact finite representative.
For a face at fixed \(b<1\), its density decreases strictly above the
threshold, so at any given feasible \(\sigma\) the maximizing threshold
is the sample minimum. The uniform face minimizes scale at the sample span
and has that same threshold. Reflected faces use the sample maximum.

**Named laws.** The finite logistic is \((0,0)\), with both zero shapes
forced by its exponential tails. The two finite uniform pairs are
\((1,1)\) and \((2,2)\), with different scales for the same law.
To see completeness of the uniform aliases, differentiate \(D\).
If both power exponents are zero then \(D=2\). Otherwise
\((a-1)u^{a-2}=(b-1)v^{b-2}\) forces either both coefficients to be zero or
both exponents \(a-2,b-2\) to be zero, by logarithmic differentiation of
the nonzero identity. The uniform on the GPD face has \(b=1\); the
one-sided exponential has \(b=0\). A finite FKML cannot be a one-sided
exponential. Its unbounded tail would require shape zero and its quantile
derivative would have to be proportional to \(1/v\) on the right, which
is incompatible with \(D=1/v+u^{a-1}\) for a bounded left side. The
reflected statements follow by interchanging shapes.

When both shapes go to \(+\infty\) outside the extension, a tight weak
limit can only be a point mass. For \(0<u_0<u<v<v_0<1\), the spacing
\(Q(v)-Q(u)\) is a sum of nonnegative left and right power differences.
Relative to the respective terms in \(Q(v_0)-Q(u_0)\), each ratio tends to
zero, bounded asymptotically by powers of \(v/v_0<1\) and
\((1-u)/(1-u_0)<1\). Tightness bounds the outer spacing, even with varying
scale and location. Hence every fixed interior spacing vanishes. The
diagonal likelihood spike shows why weak convergence alone gives no control
of likelihood along such paths. Section 1 also shows that the GPD limit
with \(b>1\) can have diverging likelihood. The completeness claims above
concern the two proposed fitting regions and the stated sample conditions.

## 3. Fixed shapes and the shape profile

Let \(H=1/D\). Then \(\partial_z\log g(z)=H'(u)\) and
\(\partial_z^2\log g(z)=H(u)H''(u)\) at \(z=q(u)\).
The exact interior concavity test is \(DD''\ge2(D')^2\) for all
\(0<u<1\). The result extends to the closed support with the endpoint
values specified in section 1.

**Strictness and local sharpness of the roadmap result.** On \([0,1]^2\)
the roadmap's harmonic mean argument gives strict concavity except for
\((1,1)\). The harmonic mean has strictly positive partial derivatives
and is strictly concave along a line whose positive arguments change ratio.
Thus an exponent in \((0,1)\) gives strictness by composition, and at
corners other than \((1,1)\) the two affine arguments change ratio.
If \(a<0\), then \(H''(u)\sim (1-a)(-a)u^{-1-a}>0\) near the left tail,
by differentiating the leading power with the opposite term smooth there.
If \(1<a<2\), \(D''\to-\infty\) with leading term
\((a-1)(a-2)u^{a-3}\), so \(H''>0\). Reflect for \(b\). This makes the
square sharp against immediate extension across its sides.

**A log-concave region beyond the square.** For \(b=1/2\) and
\(p=a-1\ge16384\), let \(w=u^p v^{1/2}\). Then
\(H=v^{1/2}/(1+w)\). For \(j=0,1,2\),
\(u^{p-j}\le\exp(-pv/2)\), and
\(\sup_{s\ge0}s^r e^{-s}<1\) for \(r=1/2,3/2,5/2\).
Termwise differentiation gives \(w\le\sqrt{2/p}\),
\(|v w'|\le4/\sqrt p\) and \(|v^2 w''|\le9/\sqrt p\).
In the last two bounds the sums of constants are
\(2^{3/2}+\sqrt2/2<4\) and
\(2^{5/2}+2^{3/2}+\sqrt2/4<9\).
Differentiating \(H\) twice and multiplying by \(v^{3/2}\) gives
\[
 -\frac{1}{4(1+w)}
 +\frac{v w'-v^2w''}{(1+w)^2}
 +\frac{2(vw')^2}{(1+w)^3}
 \ \le\ -\frac18+\frac{13}{\sqrt p}+\frac{32}{p}<0.
\]
This proves strict log concavity also on that ray and its reflection.
The separate fixed pair \((2,2)\) is uniform.

**Inner optimization.** With \(A=1/\sigma,\ B=-\mu/\sigma\),
\(\ell=n\log A+\sum_i\log g(Ax_i+B)\). Feasible support imposes affine
constraints at fixed shape. The log-concave cases give a strictly
concave objective in \((A,B)\) by strictness in \(A\) from \(\log A\)
and in \(B\) at fixed \(A\) from \(\log g\). Uniform fits with both
nuisance parameters free maximize by placing the support exactly on the
sample span. For nonconstant data, existence at fixed shape on
\([0,1]^2\) follows from the common envelope for large scale and location,
and from at least one observation a fixed distance from \(\mu\) at small
scale. Both tails have bounded support or exponential decay, which beats
all other factors of order \(1/\sigma\). The same result on the proved
large-\(a\) ray follows from bounded support of fixed finite width.
For \(1<a,b<2\), \(D''<0\) and \(H''>0\). With scale held, any maximizing
location is an extreme-sample contact, by strict convexity of the log
likelihood between the two contacts. With scale free, the same argument at
each feasible scale reduces the joint search to two scalar contact curves.

**Joint location-scale nonuniqueness at held shapes.** The exact sample
is \(x_i=\pm(1\pm1/10000)\), all four sign combinations, with
\(a=b=-1\). This is inside \(K\) with strict ties. For \(z=q(u)\),
\[
 D=z^2+4+2\sqrt{4+z^2}.
\]
This identity follows by writing \(z=(2u-1)/(uv)\) and
\(1/(uv)=2+\sqrt{4+z^2}\). Thus at \(\mu=0\),
\(f(x)\le 1/(x^2/\sigma+4\sigma+2|x|)\le1/(6|x|)\).
The centered likelihood is at most \(1/[1296(1-10^{-8})^2]\).
At \((\mu,\sigma)=(1,1/100)\) the two near densities exceed 12 because
\(|z|=1/100\), and the two far densities exceed \(1/410\) because
\(|z|<201\) implies \(D<41000\). The resulting lower bound
\((12/410)^2\) exceeds the centered bound. Attainment at these fixed
shapes follows from the compactness proof with a positive incumbent at
the held shapes. Reflection then pairs every maximizing \(\mu\ne0\) with
another maximizing location.

**Shape profile.** For a distinct pair at distance \(d\), the uniform is
the unique full maximizer on \(K\), with \(\mu\) the midpoint and
\(\sigma=d/2\). In fact, \(D^{-1/2}\) is strictly
concave except at the uniform. Each power term \(D_i\) on \(K\) obeys
\(D_iD_i''\ge(3/2)(D_i')^2\); Cauchy–Schwarz gives
\(DD''\ge(3/2)(D')^2\). It is strict if a nonconstant exponent is
strictly between \(-2\) and 0. For nonconstant terms at exponent \(-2\)
the logarithmic derivatives of the two terms differ, including when one
is constant, so Cauchy–Schwarz is strict. For inverse probabilities
\(u_1<u_2\), integrating the reciprocal squared chord of \(D^{-1/2}\)
bounds \((\int_{u_1}^{u_2}D)^2/(D(u_1)D(u_2))\) by \((u_2-u_1)^2\),
strictly in the nonuniform positive-density case. Dividing by \(d^2\)
bounds the likelihood and proves the assertion. The strict bound at
nonuniform shapes holds for each finite nuisance pair. For a fixed such
shape the only additional supremum candidates are the half-tie faces,
whose values are bounded by \(1/(2d^2)\). Thus the profiled value also
has a unique global maximum.

For two distinct sample values with any multiplicities, the two-dimensional
profile on \(1<a,b<2\) is exactly \(((1/a+1/b)/d)^n\). The endpoints
both have standardized density 1, \(D\ge1\), and the smallest feasible
scale is \(d/(1/a+1/b)\). It is achieved with both observations at their
respective support endpoints. The profile strictly decreases in each
coordinate, so on any closed rectangle there it has exactly one local
and global maximum at the lower corner. On the unrestricted domain even
quasiconcavity fails. For the exact sample \((0,1)\), the value on the
diagonal at \(a=b=3/2\) is \(16/9\), at \(a=b=2\) it is 1, and for
sufficiently large \(a=b\) it exceeds \(16/9\) by the spike path. These
three collinear shape points violate the interval property for upper
level sets. This comparison uses global profile values at the first two
points and a feasible lower bound at the third.

## 4. Per-tail information and regularity

Here regular means differentiable in quadratic mean (DQM) with finite,
nonsingular information for the full, locally unconstrained four-parameter
model. The result is **regular if and only if \(a,b<1/2\)**. The integral of
the squared pointwise derivative inside the support, taken without the
moving-boundary contribution, has a separate classification.

For a parameter direction with quantile velocity
\(V=\dot\mu+\dot\sigma q+\sigma(\dot a\,\partial_a q+\dot b\,\partial_b q)\),
implicit differentiation of \(Q(u)=x\) gives \(\dot u=-V/Q'\), hence
\[
 S(Q(u))=-\left(\frac{V}{Q'}\right)'(u).
\]
Integrate \(S(Q(u))^2\) against \(du\). At a finite left endpoint set
\(V_0=\dot\mu-\dot\sigma/a+\sigma\dot a/a^2\).

| Left shape \(a\) | Interior score square integrable near \(u=0\)? | Endpoint displacement in full model |
|---|---|---|
| \(a\le0\) | Yes, all directions | No finite endpoint |
| \(0<a<1/2\) | Yes, all directions | DQM on this side |
| \(1/2\le a<1\) | Exactly when \(V_0=0\) | DQM fails at moving endpoint |
| \(a=1\) | Yes, all directions | Positive density jump, DQM fails |
| \(1<a\le3/2\) | Exactly when \(V_0=0\) | Positive density jump, DQM fails |
| \(a>3/2\) | Yes, all directions | Positive density jump, DQM fails |

The right table is obtained by reflection, with
\(V_1=\dot\mu+\dot\sigma/b-\sigma\dot b/b^2\).
For \(a\le0\), use \(D\ge u^{a-1}\),
\(V=O(1+u^a(1+|\log u|^2))\),
\(V'/D=O(1+|\log u|)\) and \(D'/D=O(1/u)\). This bounds the score by
\(O(1+|\log u|^2)\). For \(0<a<1\) the leading term for \(V_0\ne0\)
is \((a-1)V_0 u^{-a}/\sigma\). If \(V_0=0\), then
\(V=O(u^a(1+|\log u|)+u)\), and the score is only logarithmic.
At \(a=1\) the derivative of \(u^{a-1}\) in \(u\) vanishes, and
\(V'/D\) grows at most logarithmically. For \(1<a<2\), \(Q'\to\sigma\)
and the leading score for \(V_0\ne0\) is instead
\((a-1)V_0 u^{a-2}/\sigma\); if \(V_0=0\), \(V=O(u)\) and there is
no square divergence. At \(a\ge2\) the spatial derivative of \(D\)
is bounded near the endpoint. Squaring and integrating gives the
table, including logarithmic divergences at \(1/2\) and \(3/2\) for
directions moving the endpoint.

For DQM necessity, for \(0<a<1\) the physical distance from the lower
endpoint is \((\sigma/a)u^a+O(u)\). Translating the endpoint outward
by size \(h\) therefore adds mass asymptotic to
\((a|h|/\sigma)^{1/a}\) outside the old support. For \(a\ge1\) the
added mass is asymptotic to the positive endpoint density times
\(|h|\). A DQM expansion requires that added mass be \(o(h^2)\), since
both the old square root density and its score correction vanish there.
The test excludes all \(a\ge1/2\), including \(a=1\) and \(a>3/2\)
where the interior scores alone would give a false positive.

For sufficiency take a small compact neighborhood with
\(a,b\le\alpha<1/2\) for some \(0<\alpha<1/2\), and location and
positive scale bounded there. Uniformly near the left endpoint in
probability space,
\[
 |V/Q'|\le C u^{1-\alpha}(1+|\log u|^2),\qquad
 |S(Q(u))|\le C u^{-\alpha}(1+|\log u|^2)
\]
for unit directions. For \(a\le0\), the integral representation gives
\(|\partial_aT_a(u)|\le u^a|\log u|^2/2\); for \(0<a\le\alpha\),
the same bound with \(u^a\) replaced by 1 suffices, with the quotient
controlled by \(D\ge u^{a-1}\). The bounds on \(V'/D\) and \(D'/D\)
above hold uniformly by direct differentiation. Reflect at the right.
To handle support motion in the derivative, multiply \(\sqrt f\) by
a smooth cutoff in \(F(x)\), zero for \(u<\epsilon\) or
\(1-u<\epsilon\), one on \([2\epsilon,1-2\epsilon]\), and with derivative
\(O(1/\epsilon)\). This truncated function is continuously differentiable
into \(L^2(dx)\) on the neighborhood. Its derivatives have the usual
\(\sqrt f S/2\) term multiplied by the cutoff, plus a cutoff derivative
times \(-\sqrt f V/Q'\). The squared error of each derivative versus
\(\sqrt f S/2\) is \(O(\epsilon^{1-2\alpha}(1+|\log\epsilon|^4))\)
by the bounds in probability space. The functions themselves converge
uniformly in \(L^2(dx)\) since the omitted probability is at most
\(4\epsilon\). Uniform convergence of functions and derivatives gives
continuous \(L^2\) differentiability of the square root density, by
integrating the derivatives on line segments. This proves DQM.

For rank, zero score implies that \(V/Q'\) is constant, which must be
zero by the tail bounds. Differentiating \(V=0\) and dividing by \(\sigma\)
gives
\[
 0=cD+\dot a\,u^{a-1}\log u+\dot b\,v^{b-1}\log v,
 \qquad c=\dot\sigma/\sigma.
\]
Dividing by \(u^{a-1}\) and letting \(u\downarrow0\) forces first
\(\dot a=0\), then \(c=0\), since \(a<1/2\); the remaining equation
forces \(\dot b=0\) and then \(V=0\) forces \(\dot\mu=0\).
This proves full rank and hence full rank on any fixed free subset in the
regular region. A fitted shape constraint can still change the local
experiment at its boundary. In the nonregular region the table classifies
interior scores by direction; calibration also requires control of actual
endpoint displacement along the model being fitted.

## 5. Literature and reproducibility

The roadmap's reading was extended by a ten-query primary-source check
targeting FKML likelihood existence, unboundedness, and information.
Freimer, Kollia, Mudholkar and Lin (1988) introduce the family.
Su (2007) describes moment or percentile initialization followed by
numerical likelihood maximization in the accessible abstract. Su's GLDEX
paper gives the implementation and examples. The `gld` reference
manual documents inverse-quantile likelihood evaluation and the default
Nelder–Mead search with shape-grid inputs. These sources describe numerical
estimation. The inspected material did not supply an unrestricted
finite-sample existence theorem or an exhaustive escape analysis.
Chattopadhyay, Sarkar and Kuchibhotla §3.2 use this FKML quantile in a
confidence-band method and motivate it by the difficulties of ordinary
likelihood inference. Dedduwakumara, Prendergast and Staudte give a
density-quantile estimator. These are bounded literature findings from the
inspected material, rather than a priority claim.
[Freimer et al.](https://doi.org/10.1080/03610928808829820) ·
[Su, numerical ML abstract](https://research-repository.uwa.edu.au/en/publications/numerical-maximum-log-likelihood-estimation-for-generalized-lambd/) ·
[Su, GLDEX](https://www.jstatsoft.org/article/view/v021i09) ·
[`gld` manual](https://stat.ethz.ch/CRAN/web/packages/gld/refman/gld.html) ·
[Chattopadhyay et al. §3.2](https://arxiv.org/html/2511.23086v1#S3.SS2) ·
[Dedduwakumara et al.](https://arxiv.org/abs/1907.06336)

**Numerical checks.** The final foreground run exited zero on the
workstation through the one-slot wrapper. Python 3.14.5 and mpmath 1.3.0
used 110 decimal digits. The script checks the envelope and curvature on
1,625 points, using shape pairs from
\(\{-1,-1/2,0,1/2,1,2,4,16385,\infty\}\) with one shape at most one,
and probabilities \(j/20\) for \(j=1,\ldots,19\) with added tail
probabilities \(10^{-1},10^{-3},10^{-9},10^{-24}\) and their reflections.
It also checks two strict nonconcavity signs, conservative inversion
brackets, signed collapse exponents, a half-tie limit, the exact
nonuniqueness witness and its likelihood, both spike paths, and the
one-sided GPD face with the uniform-contact recovery. The log-probability
location-information bands start at 30, 50 and 70, each of width 1, at
\(b=0.3\) and the seven left shapes listed in the output. These are fixed
evaluations and quadratures. The two initial sharp GPD checks exposed the
\(O(1/a)\) quantile shift and then the fixed-contact density discontinuity.
The final checks test ordinary convergence and recover the full uniform face
with expanded support. No compiled or package suite was run.

    /tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-25-gld-math-data/check.py

[Reproducer](2026-11-25-gld-math-data/check.py) ·
[Recorded output](2026-11-25-gld-math-data/checks.json)

## What this changes for the fitter

- Treat unrestricted, all-free FKML as proved **nonexistence due to
  unbounded likelihood**. If offering a fitted version, declare the
  shape restrictions explicitly. On \(K\), use the strict exact
  multiplicity guard for existence, classify strict majority ties as
  unbounded, and solve a distinct pair directly via the sample-span
  uniform. For a half tie, compute the collapse-face witnesses and use a
  strict improvement to establish attainment. A comparison unresolved
  within arithmetic margins leaves a converged feasible search candidate
  uncertified, where such a candidate is available.
- For held log-concave shapes use the affine support constraints in
  \((A,B)\). A tangent bound on a bounded superlevel region gives a
  likelihood-gap check. Elsewhere, compactness provides search boxes under
  strict ties. \(q\) increases in \(a\), decreases in \(b\), and increases
  in \(u\), permitting monotone inverse brackets. \(D\) decreases in
  each shape at fixed \(u\), allowing lower bounds on \(D\) over inverse
  brackets. Enumerate exact positive-density contact strata and uniform
  members. A likelihood-value certificate alone does not give a
  coefficient certificate. To give the latter, enclose the maximizing set
  and check the coefficient tolerance. Return proved multiple maxima as a
  set or a region when it is a plateau. The nonuniqueness result rules out a
  general unique inner optimum.
- If the separated extension is adopted, compare the two constrained
  GPD faces explicitly. The right-tailed face has threshold \(x_{\min}\)
  and \(-1\le\xi\le1\), and reflection uses \(x_{\max}\). A proved
  face-only winner allows a named limit outcome. An attained uniform
  representative instead gives an estimate. The current all-free
  `mlgpd` interface does not implement this constrained comparison, and a
  generic call with threshold held at the minimum and shape free can stop
  at threshold collapse. The face route needs this specific shape range.
  A competing or unresolved face cannot justify rejecting an otherwise
  converged, feasible ordinary-data candidate; flag that candidate
  uncertified unless a comparison proves it wrong. Inversion or search
  failure cannot supply an estimate.
- Report ordinary joint DQM inference only in the proved regular regime,
  with a separate calibration decision at active fitted restrictions.
  Interior score finiteness on its own is insufficient. A GPD face with
  freely moving threshold has positive density at that threshold and is
  nonregular in the threshold direction.

## Open

1. **Half-tie equality on \(K\).** The exact collapse ceiling is known.
   When no finite candidate strictly improves it, a global
   finite-likelihood comparison is needed to distinguish nonattainment,
   finite equality and an undiscovered improvement. The numerical
   half-tie check follows one proved escape path and cannot make this
   comparison. The tie boundary of the optional extension also needs its
   own analysis.
2. **Shape profile on \(K\).** The distinct-pair global solution and the
   exact monotone rectangle above one leave the absence of secondary
   shape maxima on \(K\) open, including for the distinct pair. For larger
   samples a joint shape-profile inequality or a counterexample with
   certified profile bounds would settle a stronger result. The
   nonconcavity test and unrestricted counterexample do not decide it.
   The full fixed-shape log-concavity locus beyond the proved regions
   would require a global sign classification of \(DD''-2(D')^2\).
3. **Larger domains and face winners.** Outside the two fitting regions,
   negative-shape escapes with diverging scales or locations and coupling
   at shape junctions lack a complete inventory. For the separated
   extension under strict ties the faces are exhaustive, but it remains
   to exhibit a certified nonuniform face-only winning sample or prove
   those faces are always matched or beaten by a finite fit. That
   comparison needs sharp global likelihood bounds over the competing
   branch.
4. **Nonregular inference and implementation.** The square-root
   differentiability and rank arguments give the regular regime and the
   score table gives per-tail divergence. A mixed-rate limit theorem
   with both moving endpoints, possibly of different types, and
   held-parameter calibration outside DQM require additional work.
   A certificate implementation also needs numerical error allowances
   for inverse, endpoint and global comparisons, and a measured work
   budget.
