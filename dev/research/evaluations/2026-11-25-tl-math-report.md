# Tukey lambda branch boundaries, half ties, and uniqueness

TASK-240. The likelihood uses the closed-support density convention in the
[dossier](../../../univariateML-cpp/include/cppunivariate/distributions/tlambda/README.md).
Write \(g_\lambda\) for the standardized density, \(D_\lambda(u)=u^{\lambda-1}+(1-u)^{\lambda-1}\)
for the derivative of the standardized quantile \(Q_\lambda\), and \(f(x)=g_\lambda((x-\mu)/\sigma)/\sigma\).
Let \(x_-\), \(x_+\) be the sample extremes, \(W=x_+-x_-\), and
\(R(\mu)=\max_i|x_i-\mu|\). All sample sizes are positive. Existence
refers to a positive likelihood.

The U branch reduces to a scalar shape comparison when scale is free and
has an explicit shape solution when scale is held. At half ties on
\([-1,1]\), all scales below an explicit cutoff are strictly dominated
by the collapse value. Both attainment and nonattainment occur on
four-point samples, and exact equality with an attained maximum also
occurs. The fitted branch admits unique fits, exact plateaus and
isolated symmetric pairs of maximizers, including examples with free shape.

## U branch \(1<\lambda<2\)

### Free scale

**Proved.** For any fixed location with \(R=R(\mu)>0\), the optimum at a
given shape is \(\sigma=\lambda R\), by the scale reduction in the
[theory report](2026-11-07-tlambda-theory-report.md). With free location
and \(W>0\), the unique fixed-shape nuisance optimum has support
\([x_-,x_+]\). This strengthens the prior reduction to two contact
curves.

To see this, anchor the left endpoint at \(x_-\), write the width as
\(w=2\sigma/\lambda\), and use \(A(u)=\int_0^u D_\lambda(v)\,dv\). For
each distance \(d=x_i-x_-\) we have \(\sigma A(u)=d\), hence the
log-scale derivative along the anchored curve is
\[
 -1+D_\lambda'(u)A(u)/D_\lambda(u)^2 .
\]
For \(u<1/2\), \(D_\lambda\) is increasing and concave, so
\(A(u)\le uD_\lambda(u)\) and
\(uD_\lambda'(u)\le D_\lambda(u)-D_\lambda(0)<D_\lambda(u)\).
For \(u\ge 1/2\), the derivative of \(D_\lambda\) is nonpositive.
At the anchor itself the density is \(1/\sigma\). Continuity up to
the opposite contact gives strict improvement when the width is reduced
to \(W\). Reflection gives the same result on the right-contact curve.

The resulting support has midpoint \(\mu\) and fixed radius \(R\), with
\(R=W/2\) and \(\mu=(x_-+x_+)/2\) in the free-location case. Put
\(t_i=(x_i-\mu)/R\) and \(k=\#\{i:|t_i|=1\}\). Define
\[
 u_i^\lambda-(1-u_i)^\lambda=t_i,\qquad
 P(\lambda)=(\lambda R)^{-n}\prod_i D_\lambda(u_i)^{-1},\qquad
 J=2^k(2R)^{-n}.
\]
Use \(D_\lambda=1\) at contacts for the right-hand profile. Its lower
limit is \(J\), and its upper limit is \((2R)^{-n}<J\) since \(k\ge1\).
For interior observations the implicit inverse and density extend
analytically in shape through both 1 and 2. The contact factors
\((\lambda R)^{-1}\) also extend analytically. Thus the analytically
extended profile is nonconstant and its stationary roots on \((1,2)\)
are finite. The roots can be specified directly from the sample by
\[
 \frac{P'}P=-\frac{n}{\lambda}
 -\sum_{|t_i|<1}\frac{\partial_\lambda D_\lambda(u_i)
                           +D_\lambda'(u_i)u_i'}{D_\lambda(u_i)}=0,
 \qquad
 u_i'=-\frac{u_i^\lambda\log u_i-(1-u_i)^\lambda\log(1-u_i)}
                  {\lambda D_\lambda(u_i)} .
\]

**Exact sample comparison.** Take the largest profile value at these
roots, with an empty root list below \(J\). If it is smaller than
\(J\), the supremum is unattained at the junction. If it equals
\(J\), every root achieving \(J\) gives a finite MLE and the junction is
also a maximizing limit. If it exceeds \(J\), only finite maximizers
achieve the supremum. Escape to 2 is excluded in all three cases.
With free location and a constant sample, or held location equal to
the constant sample, the likelihood is unbounded as \(\sigma\downarrow0\).
A held location away from a constant sample has \(R>0\) and the same
scalar rule, with \(P=(\lambda R)^{-n}\).

**Exact examples.** For data entirely on the two support endpoints and
both nuisances free, \(P=(\lambda R)^{-n}\) and the junction wins.
The [TASK-117 pair example](2026-10-31-open-existence-report.md#2-tukey-lambda-the-u-branch-and-the-uniform-junction)
is a special case. For attainment, take endpoints \(-1,1\) and six
copies of \(t=(40^3-399^3)/401^3=-63457199/64481201\). At shape \(3/2\),
the interior probability is \((40/401)^2\), since
\(40^2+399^2=401^2\). Here \(P(3/2)/J=(2/3)^2(1604/1317)^6>1\).

### Held scale

**Proved.** For held \((\mu,\sigma)\), put \(C=\sigma/R(\mu)\),
taking \(C=+\infty\) for \(R=0\). For free location and held scale,
put \(C=2\sigma/W\), taking \(C=+\infty\) for \(W=0\).
For \(C\le1\), no positive likelihood is feasible. For \(1<C<2\),
the maximum is attained at \(\lambda=C\), with the prescribed location
or the unique sample midpoint, respectively. For \(C\ge2\) the
supremum is \(\sigma^{-n}\) and is reached only as
\(\lambda\uparrow2\), except with free location and constant data.
In that exception all shapes in \((1,2)\) attain the same value at
the two locations \(x_-\pm\sigma/\lambda\).

With location held, the smaller inverse probability is nonincreasing
in shape at fixed absolute residual, by the quantile monotonicity in the
[theory proofs](2026-11-07-tlambda-theory-data/proofs.md#1-quantile-coordinates-and-inversion).
For \(1<\lambda<2\), \(D_\lambda(p)\) increases in \(p\le1/2\) and
strictly decreases in shape at fixed interior \(p\). Thus the density
increases strictly with shape until contact. When the upper feasible
shape is 2, all densities converge to \(1/\sigma\).

With free location, strict location log convexity at fixed U shape
reduces the optimization to the two contacts, as established in the
[theory report](2026-11-07-tlambda-theory-report.md#proved-reductions).
Each contact branch increases strictly with shape on a nonconstant
sample. For the left anchor and an interior point let \(u\) solve
\(A(u)=d/\sigma\) at fixed \(d>0\). Use \(b=\lambda-1\),
\(v=1-u\) and \(T(u)=-\partial_\lambda D_\lambda(u)
=u^b(-\log u)+v^b(-\log v)>0\). Then
\[
 u_\lambda=\frac{\int_0^u T(t)\,dt}{D_\lambda(u)},\qquad
 \frac{d}{d\lambda}D_\lambda(u)=-T(u)+D_\lambda'(u)u_\lambda .
\]
For \(u\ge1/2\), the last term is nonpositive. For \(u<1/2\) use
\[
 \int_0^u T(t)\,dt\le
 u\left[\frac{u^b}{b+1}\left(-\log u+\frac1{b+1}\right)-\log v\right],
 \qquad \frac{uD_\lambda'(u)}{D_\lambda(u)}
 \le\frac{bu^b}{D_\lambda(u)}\le b/2 .
\]
After multiplication, the coefficient of \(u^b(-\log u)\) is
at most \((1+1/\log 2)/4<1\). The other term is at most
\((b/2)(-\log v)<v^b(-\log v)\). This proves strict decrease of
\(D_\lambda(u)\). Each branch has at least one interior nonanchor
observation strictly below \(C\) for nonconstant data. Continuity to the
upper feasible shape and reflection finish the argument. For constant
data all observations can be anchored, and every interior location
has smaller density.

### Limit objects and shapes above two

**Proved.** At the lower junction the laws converge weakly to the
uniform on the limiting support. Its likelihood at shape 1 under the
dossier convention is \(J/2^k\) when \(J\) comes from \(k\) contacts.
More generally, an endpoint probability \(p\to0\) on a moving support
can have \((\lambda-1)(-\log p)\to c\in[0,\infty]\). Its limiting
density is \(1/[R(1+e^{-c})]\), with exact contact giving \(c=\infty\).
The fixed support paths attain the upper junction value of the
likelihood. The weak limit follows from pointwise quantile convergence
on \(0<u<1\). The junction is a separate likelihood status even though
the weak limit is a named law. At 2 the limiting likelihood is exactly
that of the uniform with radius \(\sigma/2\), including contacts. If
shape 2 is included, this escape becomes an attained uniform fit.

Above 2, the dossier's free-location spike path makes the likelihood
unbounded when scale is free. At held location with free scale the
same path works if any observation equals the location
(\(\sigma=\lambda R\) works when \(R>0\)). Without such observations,
\[
 |x-\mu| f(x)=\frac{|u^\lambda-(1-u)^\lambda|}
                      {\lambda D_\lambda(u)}\le\frac1\lambda
\]
and the likelihood tends uniformly to zero in scale as
\(\lambda\to\infty\). The prior exact scale reduction for
\(\lambda>2\) then gives attainment on the closed range
\([2,\infty)\). Holding scale bounds admissible positive shape above
by \(C\), except when all observations can coincide with the location.
In that exception the central density grows exponentially at fixed
scale. In particular the U-branch reduction supplies no upper
restriction for a fully unrestricted fit.

## Exactly-half ties on \([-1,1]\)

Take \(n=2k>2\), maximal tie multiplicity \(k\), and both nuisances free.
For each value \(t\) repeated \(k\) times put
\[
 B_t=8^{-k}\prod_{x_i\ne t}|x_i-t|^{-2},\qquad B=\max_t B_t.
\]
These are precisely the positive collapse face values from the
[prior face analysis](2026-11-07-tlambda-theory-data/proofs.md#6-collapse-faces-including-equality).
That analysis also proves \(L(-1,\mu,\sigma)<B_t\) for every finite fit
at shape \(-1\), by pairing the copies of \(t\) with the other
observations. The [ties report](2026-11-21-tlambda-ties-report.md#distinct-pairs)
established the unique uniform solution for a distinct pair.

### A uniform strict cutoff at small scale

**Proved.** For a negatively shaped member write \(a=-\lambda\),
\(p\le1/2\) for the smaller tail, \(q=1-p\), \(H=|Q_{-a}(p)|\),
\(y=-\log p\), \(v=-\log q\), and \(\rho=p/q\). At fixed \(H\),
\[
 \frac{p_a}{p}
 =\frac{(y-1/a)-\rho^a(v-1/a)}{a(1+\rho^{a+1})}, \qquad
 \partial_a\log g_{-a}
 =-\frac{y+\rho^{a+1}v}{1+\rho^{a+1}}
   +\frac{(a+1)(1-\rho^{a+2})}{1+\rho^{a+1}}\frac{p_a}{p}.
\]
The first formula follows by differentiating
\(H=(p^{-a}-q^{-a})/a\) and using \(H_p=-D_{-a}\).
The second differentiates \(D_{-a}\) along the inverse.
Also \(p_a\ge0\) by the quantile monotonicity, including at the center.

On \(1/2\le a\le1\), the score is at least \(-4\) for \(y<4\).
For \(y\ge4\), use \(\rho\le1/16\), \(v<1/a\), and \(1/a\le2\).
The positive term is at least
\((480/289)(y-2)\), since \((a+1)/a\ge2\),
\(1-\rho^{a+2}\ge15/16\), and \(1+\rho^{a+1}\le17/16\).
The score is therefore at least \(y/2-4\). As \(H\le 2/p\),
if any observation has \(H>2e^{8n}\), the total score in \(a\)
is strictly positive throughout \([1/2,1]\).
This criterion holds for every location when
\(\sigma\le W/(8e^{8n})\), because at least one residual is at
least \(W/2\). The whole shape interval is then dominated by
the finite fit at \(-1\), which is strictly below \(B\).

For the remaining shapes \([-1/2,1]\), the prior Hölder envelope with
\(A=1/2\) gives \(f_i\le\min\{1/(2\sigma),8\sigma^2/|x_i-\mu|^3\}\).
Let \(\delta\) be half the minimum spacing between distinct values.
At least \(k\) observations have residual magnitude \(\ge\delta\).
The resulting bound is \(L\le(4\sigma/\delta^3)^k\). In particular,
\[
 \tau=\min\left\{\frac{W}{8e^{8n}},\ \frac{\delta^3B^{1/k}}8\right\}
 \quad\Longrightarrow\quad L<B\quad(0<\sigma\le\tau).
\]

### Compact comparison and examples

**Proved.** Let \(S=B^{-1/n}\), \(M=\sqrt{2S/B^{1/n}}\).
At \(\sigma\ge S\) the modal bound gives \(L\le B/2^n\).
At \(\sigma\le S\) and \(\mu\notin(x_- -M,x_++M)\) the uniform
tail bound \(f_i\le\sigma/|x_i-\mu|^2\) gives the same inequality.
Thus all finite candidates at level \(B\) or above are strictly
inside the nuisance sides of the compact box
\[
 K=[x_- -M,x_++M]\times[\tau,S]\times[-1,1].
\]
Treat an empty box as having no candidate. Upper semicontinuity on
the fitted branch gives a maximum \(V\) on a nonempty box.
The sample condition is \(V\ge B\) for attainment, with every finite
maximizer in \(K\). If \(V<B\), the supremum is \(B\) and is
unattained. This removes the scale-zero comparison from the search
over finite candidates. The compact box can be large.

Equivalently, compare \(B\) to the uniform optimum \(W^{-n}\)
and to positive-likelihood interior stationary values with
\(-1<\lambda<1\) and nuisance coordinates inside \(K\). The \(-1\)
face is strictly below \(B\). At positive likelihood and
\(0<\lambda<1\) all data are strictly inside support. Hence off
the uniform face the ordinary scores cover every candidate at or
above \(B\). The stationary values form a finite set, even if
a stationary locus has positive dimension. This follows from
definability and the piecewise smooth path argument below. This
comparison is an exact condition on the sample. Exact equality of
general critical values still lacks a practical decision procedure.

**Exact attainment example.** On \((-1,0,0,1)\) the uniform likelihood
is \(1/16>B=1/64\). Balanced two-cluster samples have the unique
uniform fit by the pair inequality applied to each copy of the
distinct pair.

**Exact nonattainment example.** Take \((0,0,\epsilon,1)\) with
\(\epsilon=10^{-20}\). Its face value is \(B=1/(64\epsilon^2)\).
One observation from the pair \((0,1)\) is at distance at least \(1/2\)
from any location. Another observation from the remaining copy of
0 and the observation \(\epsilon\) is at distance at least
\(\epsilon/2\). For \(\lambda\ge-1/2\), applying the \(8\sigma^2/d^3\)
tail bound to the first observation gives \(L\le8/\sigma\), and to
both gives \(L\le1024\sigma^2/\epsilon^3\). By splitting scales at
\(\sigma=\epsilon\), these imply \(L\le1024/\epsilon<B\).
For all shapes the \(d^{-2}\) bound on the first observation and
three modal bounds give \(L\le1/(2\sigma^2)\). Any candidate at
level \(B\) would have \(\sigma\le\sqrt{32}\epsilon<6\epsilon\),
giving \(H>1/(12\epsilon)>2e^{32}\) for the first observation.
The negative shape score comparison applies and gives \(L<B\)
throughout the remaining shapes as well.

**Proved finite equality at a transition.** In the same family with
\(\epsilon\in[10^{-20},1/2]\), one common compact nuisance box works
for the level-\(B(\epsilon)\) comparison by taking the smallest lower
cutoff and the largest upper cutoffs over the interval and including
the uniform fit. The compact maximum \(V(\epsilon)\) is continuous.
Joint upper semicontinuity gives the upper bound along convergent
sequences. For the lower bound, a nonuniform positive-likelihood
maximizer remains interior to its support and its likelihood is
continuous in \(\epsilon\). If a uniform maximizer wins, the
fixed range \([0,1]\) gives its same likelihood 1 throughout.
At the small endpoint \(V<B\), and at \(1/2\) the uniform likelihood
is \(1>B=1/16\). Consequently an interior \(\epsilon\) with \(V=B\)
exists. At that transition a finite MLE ties the collapse face.
This identifies an exact equality case by continuity, with its
sample coordinate specified implicitly.

## Uniqueness and maximizer sets on the fitted branch

### A uniform neighborhood and a positive-shape profile result

**Proved.** Suppose scale is free and the smallest feasible uniform
radius \(R_*>0\). It is \(W/2\) when location is free and \(R(\mu)\)
when location is held. Let \(U=(2R_*)^{-n}\). For
\(\lambda=1-h\), \(0<h<e^{-4n}\), every fit has \(L<U\).
Indeed, support requires \(\sigma\ge\lambda R_*\).
Any point with \(L\ge U\) would satisfy
\(\sigma\le R_*2^{-h}\le R_*\) by the modal bound. Its largest
standardized residual is at least 1. The associated quantile
deficit from the support endpoint is at most \(1/\lambda-1\le2h\).
Since \(D_\lambda\ge2\), the smaller tail there is \(p\le h\).
Use \(D_\lambda(p)\ge p^{-h}+1\) there and \(D_\lambda\ge2\)
elsewhere to obtain
\[
 \log(L/U)\le -n\log(1-h)
        +\log\frac{2}{1+h^{-h}}
 \le 2nh+\tfrac h2\log h<0 .
\]
Here the second term uses the arithmetic–geometric mean inequality.
The fixed-shape profile tends to \(U\) as shape tends to 1 from below.
For the lower limit take a uniform-optimal location and any fixed
scale \((1+\eta)R_*\) with \(\eta>0\), so every observation is
strictly inside support near shape 1, then let \(\eta\downarrow0\).
The preceding upper bound handles the upper limit.

By definable monotonicity the attained fixed-shape profile is strictly
increasing on some terminal interval below 1. Its limit and strict
inequality there exclude an eventual constant or decreasing profile.
This gives a one-sided unimodality result with a sample-dependent
interval. The explicit \(e^{-4n}\) interval certifies domination by
the uniform face across the whole interval, without certifying a
derivative sign at every point of that particular interval. A
strict global upper bound below \(U\) on the rest of the shape range
therefore suffices for unique all-free fitting, with the uniform
sample-range triple. Balanced two-cluster samples have this
uniqueness directly by pairing, with no shape search.

At fixed \(0\le\lambda<1\), the previous inverse-scale/location
concavity proof gives unique nuisance fits whenever a finite
maximum exists. For nonconstant data and scale free, on \((0,1)\)
the optimum in the inverse-scale coordinates has negative definite
nuisance Hessian. The \(n\log(1/\sigma)\) term supplies curvature in
inverse scale and the strict standardized log-density curvature
supplies curvature for the remaining location direction. The
positive-likelihood score equations and the implicit function
theorem give an analytic profile, also if location is held and
\(R_*>0\). It is nonconstant by the strict uniform comparison
and the limit. Definability and analyticity imply finitely many
stationary shapes on \((0,1)\).

### Exact held-scale uniqueness and nonuniqueness with free shape

**Proved.** If a held scale permits a uniform fit, the peak bound
\(g_\lambda\le2^{\lambda-2}<1/2\) for \(\lambda<1\) forces
shape 1. With location free the complete maximizing interval is
\([x_+-\sigma,x_-+\sigma]\). It is a singleton when \(W=2\sigma\).
With location held the uniform fit is unique whenever feasible.
For held uniform shape and other hold patterns the prior exact
support solution applies.

For a different regime set \(\sigma=1\) and \(d\ge10^{20}\).
With location and shape free, the sample \((0,0,d)\) has exactly
one maximizer and \((0,d)\) has exactly two. All winning shapes
are \(-1\). In particular free shape does not eliminate the
reflection tie on a fixed-scale fit. Rescaling gives the same
conditions in units of any held scale.

For the proof, the farthest standardized residual is always
at least \(d/2>2e^{8n}\), so the negative shape score above makes
the whole profile strictly increasing in \(a\in[1/2,1]\).
More explicitly the score has a uniform positive lower bound
in \(\mu\), obtained by replacing \(H\) in the score bound with
\(d/2\). The \(d^{-3}\) envelope gives the upper bounds
\(16/d^3\) and \(32/d^3\) for \(n=3\) and \(n=2\) on
\([-1/2,1]\). At \(\mu=0,\lambda=-1\), the likelihood exceeds
\(1/[8^{n-1}(d+4)^2]\), which beats both respective bounds.
This follows from
\[
 g_{-1}(z)=\frac1{s(s+2)},\qquad s=\sqrt{4+z^2},
\]
obtained by substituting \(1/[p(1-p)]=2+s\) into \(D_{-1}\).

At \(-1\) all maxima are in \([0,d]\) by radial monotonicity.
For \((0,0,d)\), on \([d/2,d]\) the likelihood is at most
\(2/d^4\). On \([4,d/2]\) it is at most \(4/(20^2d^2)\),
using \(g_{-1}(4)<1/20\). On \([1,4]\) it is at most
\(1/[9^2(d-4)^2]\), using \(g_{-1}(1)<1/9\).
All three bounds are below the witness at \(\mu=0\).
For \((0,d)\), restrict to \([0,d/2]\) by symmetry. On
\([5,d/2]\) use \(4/(37d^2)\) and \(g_{-1}(5)<1/37\).
On \([1,5]\) use \(1/[9(d-5)^2]\). Both are below the
witness. Thus each relevant side reduces to \([0,1]\) or
its reflection. On \(|z|\le1\),
\((\log g_{-1})''(z)\le-3/25\) since
\((\log s)''=(4-z^2)/(4+z^2)^2\) and
\((\log(s+2))''\ge0\) there. For \(|z|>0\)
the absolute log curvature is at most \(3/z^2\), by differentiating
\(-\log s-\log(s+2)\). The near-cluster negative curvature dominates
the far term by \(3/(d-1)^2<3/25\). The log likelihood is
strictly concave on each remaining interval. This yields one
maximizer in the asymmetric case and exactly the two reflected
maximizers in the symmetric case.

### What can be represented exactly

**Proved.** The likelihood and the score strata at positive likelihood
are definable in the ordered real exponential field. All the displayed
operations use field operations and exponentials (with log given
as the inverse of exp), with a separate logistic expression at zero
and a support case split. The inverse probabilities are uniquely
specified by their quantile equations. Definability of the entire
maximizer set and profile follows by quantifying over parameters.
By [Wilkie's o-minimality theorem](https://doi.org/10.1090/S0894-0347-96-00216-0)
and the cell decomposition and monotonicity consequences, the sets
have finitely many connected components and each admits a finite
decomposition into smooth cells. On a differentiable score stratum,
the value is constant on each connected component of the critical
set because any two points there can be joined by a piecewise
smooth definable path. This proves the critical-value finiteness
used in the half-tie comparison. It also proves finite numbers of
isolated maxima, but it supplies neither an effective root
isolator nor an equality oracle.

For an attained positive maximum on the fitted branch, the following
representations have distinct evidence requirements.

- **Singleton.** Analytic identification, or isolation of one optimum
  with the remaining search domain excluded and a strict curvature or
  other local uniqueness proof inside the isolator.
- **Location interval.** A uniform maximum with held scale and free
  location has the exact closed interval given above. The
  singleton-width case uses the singleton representation.
- **Finite point list.** Return all isolated winning roots if a complete
  root isolation and equality proof is available. A reflected pair for
  symmetric data with held scale can share a single isolator and its
  exact reflection once all other roots are excluded.
  Fixed U-shape held-scale support contacts have a separate exact
  two-contact candidate list, established in the prior theory.
- **Other positive-dimensional set.** A finite union of definable cells
  is a structural bound. A fitter could return such a set only with a
  verified parametrization or membership description. An unresolved
  comparison among roots or near-flat boxes is insufficient evidence
  of a finite tie or a continuum.

## A cheaper bound for free shape and nuisance coefficients

**Proved.** The earlier Hölder envelopes give a shared-scale bound
without quantile inversion. For a location interval \(I\), put
\(d_i=\operatorname{dist}(x_i,I)\). For a shape box inside \([-A,1]\),
\(0<A\le1\), use \(C=\max(1,A^{-1-1/A})\), and use
\(M_0=2^{\lambda_{\max}-2}\) for the modal constant. In
\(s=\log\sigma\),
\[
 \log L\le \sum_i \min\{\log M_0-s,\
             \log C +s/A -(1+1/A)\log d_i\}.
\]
When \(d_i=0\) keep just the first term. The right side is a
concave piecewise linear function. Its maximum on the box's scale
interval is at an endpoint, at a weighted breakpoint where the slope
changes sign, or on an interval between these when the slope is zero.
One sort and a slope scan, or weighted breakpoint selection, suffices.
The maximum retains the common scale and is bounded above by the
sum of observation-wise maxima. The modal constant can use the
box's actual upper shape. The shape and location box still needs
subdivision if the bound fails.

As an independent cheap bound, for any disjoint pairs of distinct
observations use \(f(x_i)f(x_j)\le|x_i-x_j|^{-2}\) by the
[principal-branch pair inequality](2026-11-21-tlambda-ties-report.md#distinct-pairs).
Unpaired observations retain their single-observation bounds.
For any fixed pairing, maximize the resulting shared-scale bound
in log scale as above, and take the minimum across the bounds
that were evaluated. These bounds can be used as early pruning
before inverse-density boxes or concavity tangents. They carry
no claim of resolving the observed \(n=31\) budget exhaustion.

**Numerical.** For \((-1,0,1)\), \(I=[-0.1,0.1]\), scale
\([0.01,1]\) and shape \([-1,1]\), the shared-scale version of
the \(M_0=1/2,C=1\) envelope is \(0.4849840748878927\), versus
\(30.86419753086420\) when optimizing scales separately. The
breakpoint calculation and a 1,001-point log-scale grid reproduce
the bound. This is an envelope comparison, with no timing claim.

## Reproduction

The final run passed on the workstation with one slot. The
[script](2026-11-25-tl-math-data/check.py) and
[output](2026-11-25-tl-math-data/check.txt) record exact
rational comparisons, 70-digit mpmath arithmetic for 45 anchored
U scores and 45 negative-shape scores, an independent fixed-residual
score differentiation, three collapse-scale probes, and the
shared-scale grid. The grids are numerical checks of the formulas.
The classifications and examples above use the proofs and exact
inequalities.

```sh
/tmp/umvl-cpu/run nice -n 10 python3 dev/research/evaluations/2026-11-25-tl-math-data/check.py
```

## What this changes for the fitter

- An optional U branch with free scale needs a one-dimensional shape
  certificate on a fixed support. With held scale its shape decision is
  the arithmetic comparison of \(C\) to 1 and 2, with the constant-data
  exception described above. Use a dedicated junction status when
  \(J\) strictly wins. An excluded shape-2 winner can direct the caller
  to the uniform of radius \(\sigma/2\). A proved finite tie with a
  junction returns the finite maximizing set.
- For all-free half ties, compare against \(B\) on \(K\). A feasible
  finite value at or above \(B\) proves existence. Strictly excluding
  all of \(K\) at level \(B\) certifies \(\texttt{CollapseLimit}\).
  A positive-tolerance tie alone establishes neither nonattainment nor
  equality. If a certificate stalls and a valid fit is available from
  the search, retain it as uncertified unless the certificate rules
  that output out. A failed search supplies no fallback estimate.
- The explicit collar lets a uniform candidate eliminate the final
  shape interval with free scale, even in the presence of contact
  rounding near 1. Use exact location intervals and certified point
  lists for proved nonuniqueness. A likelihood-gap enclosure alone
  cannot certify uniqueness or the set of maximizers.
- Try the shared-scale envelope before per-observation inverse boxes
  and preserve the stronger existing bounds. Treat its computational
  benefit as a measurement target.

## Open

- The free-scale U test reduces to finitely many scalar roots.
  A practical certified isolator, including multiple roots and exact
  equality with \(J\), remains to be developed.
- The half-tie comparison now has an explicit strict small-scale
  exclusion. Its general exact critical-value equality problem
  remains open. The cutoff can be very conservative. A smaller
  sample-dependent exponential score threshold or a direct likelihood
  inequality could materially reduce the compact search.
- Beyond the regimes proved here, general fitted-branch uniqueness
  and the existence of nonuniform positive-dimensional maximizing
  sets remain open. Proving strict shape-profile curvature or
  excluding all but finitely many isolated maxima would narrow the
  representations needed.
- The shared-scale envelope needs benchmarking on the earlier
  \(n=31\) cases. It has no proven bound on the number of boxes
  needed to resolve interior shape comparisons.
