# Pearson IV: derivations and certificate design

TASK-161. Mathematical results below are proved by the displayed arguments unless
marked **numerical**, **design**, or **open**. The prototypes use arbitrary precision;
none is production C++ or a binary64 error certificate. Notation: λ is location,
α>0 is scale, m>1/2, ν is skew, a=2m−1, C is the standardized density normalizer,
and n>0 observations are finite. Multiplicities include repetitions exactly.

## 1. Coordinates, normalization and scores

Write z=(x−λ)/α and

$$\log f= A(m,\nu)-\log\alpha-m\log(1+z^2)-\nu\arctan z,$$
$$A=(2m-2)\log2-\log\pi+2\Re\log\Gamma(m+i\nu/2)-\log\Gamma(2m-1).$$

Duplication of Γ proves equivalence with the supplied beta normalizer. Substitution
x=λ+α tan θ gives the angular weight cos(θ)^(a−1) exp(−νθ). Thus m>1/2
is precisely integrability; no moment assumptions are needed. The mode is
c=λ−αν/(2m). Recommend public `location, scale, m, nu`, with all four regular,
all defaulting to estimation. These preserve genuine location/scale equivariance,
and match PearsonDS without calling λ a mean or α a standard deviation.
The controls are `tol, iterlim`: the argument-name document's older controls
are explicitly superseded by its leading accuracy-contract notice.

| Convention | Exact map from the proposed coordinates |
|---|---|
| Heinrich | a=α, λ=λ, m=m, ν=ν; a in that source is not our tail index a. |
| PearsonDS | location=λ, scale=α, m=m, nu=ν, including ν=0 as an interior member. |
| Nagahara differential-equation convention | μ=λ, τ=α, b=m, δ=−ν/(2m); his −p′/p=(2b/τ)(z−δ)/(1+z²). |
| Location-scale Student t | ν=0, df=d=2m−1, mu=λ, scale σ=α/√d. A held α is **not** a held t scale when m varies. |

Nagahara's map follows the equation exposed by the publisher's abstract/index;
the full text was inaccessible. It does not assert that every alternate symbol
in his full paper has been inspected. Heinrich's convention follows the reviewed
literature report and the PearsonDS density, rather than a new full-text fetch.

In (λ,s=log α,m,ν), individual scores are

$$S_\lambda={2mz+\nu\over\alpha(1+z^2)},\qquad
 S_s=-1+{2mz^2+\nu z\over1+z^2},$$
$$S_m=A_m-\log(1+z^2),\qquad S_\nu=A_\nu-\arctan z,$$
$$A_m=2\Re\psi(m+i\nu/2)-2\psi(2m-1)+2\log2,
 \qquad A_\nu=-\Im\psi(m+i\nu/2).$$

At held λ,α, the natural statistics are log(1+z²), atan z. Their distribution
has no affine dependence, so the log partition Hessian is positive definite:
log likelihood is strictly concave in (m,ν), also in either alone, on the convex
domain m>1/2 (or m≥1). Strict concavity does not imply attainment at infinity.

At fixed shapes the scale score derivative is

$$\partial_s S_s=-{z[4mz+\nu(1-z^2)]\over(1+z^2)^2}.$$

It changes sign: m=1,ν=4,z=10 gives a positive derivative, whereas the ν=0
score is decreasing. Thus skew destroys the lst monotone scale reduction.
The scale-only likelihood can have several stationary points: take held
λ=0,m=1,ν=10 and data (−1,10). Its total log-scale score is
−2+(2−10α)/(α²+1)+(200+100α)/(α²+100), with signs +,−,+,− at
α=0,1,10,100 (the first is the right-hand limit). Continuity proves at
least three positive stationary roots, including two local maxima.
Nor is location
log likelihood globally concave: at ν=0, m=1,α=1, data (−2,2), its second
derivative at λ=0 is positive. Symmetry, vanishing tails and continuity imply
at least two global location maxima (in fact ±√3). No one-to-one continuous
location reparameterization on an interval can make this bimodal likelihood
concave, since concavity requires a connected maximum set. This rules out a
universal concavity claim; it does not rule out every special-subset chart.

## 2. Exact collapse, including equality and interacting rates

First hold m,ν finite. Let λ=t+αh with t an observed value repeated k times,
others d_i=x_i−t≠0. If h tends to a finite value,

$$L\sim \alpha^{e}\,C^n(1+h^2)^{-mk}e^{k\nu\arctan h}
 \prod_{d_i\ne0}|d_i|^{-2m}
 e^{-\nu\pi\sum_{d_i\ne0}\operatorname{sign}(d_i)/2},\quad
 e=a(n-k)-k. \tag{1}$$

This follows by expanding each nonzero residual separately. For a held location
t, h=0. For a free location, maximizing the central coefficient gives
h=ν/(2m), the mode-centred path. If |h|→∞ with ν bounded, the central
coefficient decreases to zero; no larger limsup is missed. An unobserved
limiting location has k=0. Thus e<0 proves unboundedness and e>0 excludes
this collapse; e=0 is a genuine finite likelihood face F_t(m,ν), given by (1)
with α^e removed. It is not automatically an unattained global maximum.

Held λ,ν=0 at e=0 is special: each noncentral factor is strictly less than
C α^a |d|^(−2m), while central factors are exactly C/α. Hence every finite
scale is below F_t: the supremum is unattained. For ν≠0,

$$\ell(\alpha)=\log F_t+\nu\alpha\sum_{d_i\ne0}d_i^{-1}
 -m\alpha^2\sum_{d_i\ne0}d_i^{-2}+O(\alpha^3). \tag{2}$$

A positive linear coefficient proves strict finite improvement and hence
attainment for the scalar problem. If it is negative, only local approach
from below follows; all positive stationary roots must still be compared.

A second exact case is m=1,ν=0 with free λ,α and k=n/2. Pair each occurrence
of t with a noncentral observation u. The identity

$$[(t-\lambda)^2+\alpha^2][(u-\lambda)^2+\alpha^2]
 -\alpha^2(u-t)^2=[(t-\lambda)(u-\lambda)+\alpha^2]^2$$

bounds each paired density product by 1/[π²(u−t)²]. It bounds the entire
likelihood by F_t. Equality is possible iff all noncentral observations
coincide at a single u: then every λ strictly between t,u with
α²=(λ−t)(u−λ) is a finite global maximum. With at least three distinct
values the inequalities cannot be equal simultaneously, and F_t is the
unattained supremum. This is both an equality decision and an exact
nonuniqueness counterexample. In particular n=2 already has a continuum.

There is an exact skewed extension at m=1. For two distinct observations
t<u put θ_t=atan((t−λ)/α), θ_u=atan((u−λ)/α),
Δ=θ_u−θ_t and φ=θ_u+θ_t. Then 0<Δ<π, |φ|<π−Δ and

$$f(t)f(u)={C^2\sin^2\Delta\over(u-t)^2}e^{-\nu\phi}.$$

For ν≠0 this is strictly below
C² sin²Δ exp[|ν|(π−Δ)]/(u−t)². The latter is maximized uniquely at
Δ*=atan2(2,|ν|), since its log derivative is 2cot Δ−|ν|.
Thus every finite fit is strictly below K_ν/(u−t)², where
K_ν=C² sin²Δ* exp[|ν|(π−Δ*)]. The bound is approached by collapse
at u if ν>0, or at t if ν<0, with the central point at the mode.
Pairing a half-tie at the sample maximum (ν>0) or minimum (ν<0)
with every remaining observation gives L<F_t for all finite λ,α,
and the same collapse path attains F_t as a supremum. This proves
nonattainment for that entire skewed equality class, including balanced
two-cluster data. The zero-skew continuum is a genuine exceptional case.

If ν is free with m=1 and a half-tie at either extreme, there is likewise
no finite MLE. For each nonzero |ν| the pairwise bound is approached by
choosing the sign which collapses at that extreme; every finite fit with
either sign is strictly below it. At ν=0, a plateau candidate is beaten
by small nonzero skew because (d/dv)log K_v at v=0+ equals π/2>0
(C is even and Δ*=π/2 there). Non-plateau zero-skew candidates are
already below their Cauchy face. This settles that free-skew equality
class without optimizing an arbitrary skew cutoff.

With location held, there is another elementary equality decision:
log(L/F_t)=−mΣ log(1+α²/d_i²)+νΣ atan(α/d_i).
If every noncentral νd_i≤0 this is strictly negative at every α>0,
so the face is the unattained global supremum. If νΣ1/d_i>0,
equation (2) instead proves attainment. Mixed-sign unresolved cases
retain the general all-root comparison rather than a guessed sign rule.

With free m≥1 and half ties, only m→1 can retain a nonzero bounded-skew
collapse value. If (m−1)|log α|→b≥0, (1) is multiplied by
exp[−2(n−k)b], at most one. For a held m with e=0, analogous approaches
from the allowed side give the same attenuation. Shape cannot be frozen
before taking this limit.

Free skew requires a further endpoint in F_t. When location is free,
|ν|→∞ in the optimized coefficient has the finite limit

$$F_t(m,\operatorname{sign}\nu\,\infty)=
 {(2m)^{2mk}e^{-2mk}\over\Gamma(a)^n}
 \prod_{d_i\ne0}|d_i|^{-2m},\tag{3}$$

provided every noncentral observation has sign(d_i)=−sign(ν); otherwise the
limit is zero. This uses a n−2mk=e=0 and Stirling's formula. It is the
collapsing type V mode, not a missing gamma face. With λ=t held, h=0 and
the skew-infinite equality coefficient is zero (k>0); (3) is ineligible.
For free m≥1, half ties, compare the supremum of F_t(1,ν) over the extended
skew line, not merely ν=0. Simultaneous m→1,ν→∞ are controlled by the
mode-centred chart below; the bounded-skew attenuation alone is insufficient.

More explicitly, in that chart write B_s=lim_(u→s∞)|u|^(2m)h(u).
Then B_s=C[D√(1−k²)]^a exp(−sνπ/2), with continuous limits at k=±1:
on the supported side B_s=D^(3a)/Γ(a), and on the other side B_s=0.
The modal value tends to D^(2a−1)exp(−D²)/Γ(a).
For bounded m, the expansion h(u)=B_s|u|^(−2m)[1+O(1/|u|)] is uniform
over k up to these endpoints: expand (4) in 1/u, whose denominator is
bounded away from zero for |u|>2D. Thus in a shrinking-w neighborhood
of a tied t the coefficient is h(0)^k_j B_+^(n_+) B_-^(n_-), times
the residual powers and w^e. At m→1 with half ties, w^e has limsup≤1.
Continuity and the uniform expansion exhaust the simultaneous skew/shape
corner and recover (3); a divergence in ν cannot amplify it further.

**Open equality decision E.** Outside the exact cases above,
there is no general analytic comparison of all finite candidates with all
F_t faces. For fixed shapes, polynomial score isolation gives an effective
strict-comparison route; an unresolved equality must stop. With free skew or
shape the critical-value problem is transcendental. A likelihood tolerance
tie cannot distinguish nonattainment from a finite maximum tied with a face.
The same attained-point issue applies to V/normal comparisons if only
value enclosures are known: interior points approach every accessible face,
so a value-gap certificate alone cannot prove an escape. A signed inward
neighborhood inequality plus exclusion of the remaining compact interior,
or an exact maximizing-set decision, is required. A negative V inward score
proves only local deterioration, whereas a positive score proves local
improvement. The proposed fitter must stop unresolved when those additional
comparisons have not been established.

## 3. A compactification which includes arbitrary diverging skew

Use internal coordinates

$$D=\sqrt{2m},\quad r={\nu\over2m},\quad
 k={r\over\sqrt{1+r^2}},\quad
 c=\lambda-\alpha r,\quad w={\alpha\sqrt{1+r^2}\over D}.$$

Here k is an internal skew coordinate, unrelated to multiplicity. In the
rest of this section write multiplicities k_j. The inverse is

$$\lambda=c+wDk,\quad\alpha=wD\sqrt{1-k^2},\quad
 \nu={D^2k\over\sqrt{1-k^2}}.$$

For u=(x−c)/w, the standardized log-density derivative is exactly

$$ (\log h)'(u)={-u\over q(u)},\qquad
 q(u)=1-2ku/D+u^2/D^2. \tag{4}$$

For m≥M>1/2, D≥√(2M), |k|<1, and
q(u)≤(1+|u|/√(2M))². Integrating (4) separately away from the mode gives

$$h(u)\le h(0)e^{2M}(1+|u|/\sqrt{2M})^{-2M}. \tag{5}$$

A uniform upper bound on h(0) follows without a special-function inequality:
choose b=min(1/2,√(2M)/2). On [−b,b], q≥1/4, so
h(u)≥h(0)exp(−2u²); normalization gives
h(0)≤[∫_(−b)^b exp(−2u²)du]^(−1). For M=1 one may use H=exp(1/2).
Consequently throughout m≥1, all ν,

$$ f(x)\le M_0/w,\quad f(x)\le C_0w/(x-c)^2,
 \qquad M_0=e^{1/2},\quad C_0=2e^{5/2}. \tag{6}$$

These intentionally loose constants are explicit. The inequality remains
true at each limit by taking limits. It proves more than a fixed-skew bound.

For an incumbent P=exp ℓ0>0, (6) bounds w≤M0 P^(−1/n).
Optimizing the two envelopes in w gives f≤√(M0 C0)/|x−c|, hence

$$\operatorname{dist}(c,[x_{\min},x_{\max}])
 \le\sqrt{M_0C_0}P^{-1/n}. \tag{7}$$

Let δ be one third of the minimum spacing between distinct data. In a
δ-neighborhood of t_j repeated k_j times,

$$L\le M_0^{k_j}C_0^{n-k_j}\delta^{-2(n-k_j)}w^{n-2k_j}. \tag{8}$$

Outside these neighborhoods use k_j=0. If 2k_max<n, this gives an explicit
positive lower w bound at the incumbent. Intersect the resulting compact
(c,w) box with holds, using logarithms for all bounds. This proves nuisance
compactness **uniformly over all m≥1 and all skew**. It does not require
finite moments or any uniqueness claim.

With c,w compact, compactify 1/D∈[0,1/√2], k∈[−1,1]. For finite D and
|k|<1 this is IV. At k=±1 the double root in (4) gives a reflected V
law supported on the mode side of its threshold; at 1/D=0 it gives a
normal law. At m→∞, q→1 uniformly on bounded u, uniformly in k.
The density relative to its modal value therefore tends to exp(−u²/2).
This also has a quantitative enclosure: for |u|≤R let
η=2R/D+R²/D²<1. Then |log[h(u)/h(0)]+u²/2|≤(u²/2)η/(1−η).
For any fixed M≤m, the integral outside [−R,R] of h/h(0) is at most
2 exp(2M)√(2M)/(2M−1)·(1+R/√(2M))^(1−2M), by (5).
The central Gaussian factors with these error multipliers and this tail
bound enclose the normalizing integral. Increase R, then D, to make both
errors small; this gives computable, although conservative, normal-face
likelihood enclosures rather than a qualitative limit used as a cutoff.
The integrable bound (5), divided by h(0), permits dominated convergence
of the normalizing integral: h(0)→1/√(2π). This proves the normal limit
uniformly over *all* skew rates, not only fixed ν or fixed r.

At finite D,k→±1, integrate (4) on compact intervals away from u=±D.
On the mode side it gives V; on the other side the arctangent difference
in the original density supplies exp(−π|ν|), forcing density to zero.
At the root, powers are dominated by exp(−constant/distance) from the
mode side and by the same crossing suppression from the other side.
Thus the density extends continuously with value zero at the threshold.
The same integrable bound normalizes this extension. These arguments give
continuity on compact data/c/w sets, including the normal/V corners.

It follows by compactness that, under strict ties, the likelihood maximum
in the extended space is attained. A finite IV maximizer exists if any
extended maximizer is interior in 1/D>0, |k|<1 (restriction m=1 is eligible).
A strict finite improvement over every accessible excluded face suffices.
An excluded face alone winning gives nonattainment; a finite tied maximizer
is still an MLE. This is a necessary and sufficient **variational decision**,
not a closed-form sample statistic and not yet a terminating exact-equality
algorithm. Do not label an optimizer's largest value a certificate.

When scale α is held, finite m bounds w below by α/√(2m). Thus collapse
can occur only with m→∞. For any nonconstant sample choose M so large that
(2M−1)(n−k_max)−k_max>0. Apply (5) with this M to exclude w→0 as in (8).
For bounded m, α itself excludes collapse. This proves nuisance compactness
for nonconstant samples even with majority ties when scale is held. Constant
samples with both shapes free can instead spike by m→∞, r=(λ−t)/α.
If ν is held and λ is free they spike with λ→t. With λ,ν both held, only
t=λ can spike as m→∞; otherwise likelihood vanishes at that end.

## 4. Faces, eligibility under holds, and the absence of a gamma face

### Type V

Take m fixed, α→0, ν=−sβ/α, β>0, λ→t, s=±1.
On y=s(x−t)>0,

$$ f_V(x)={\beta^a\over\Gamma(a)}y^{-a-1}e^{-\beta/y},\qquad
 \ell_V=na\log\beta-n\log\Gamma(a)-(a+1)\sum\log y_i
 -\beta\sum y_i^{-1}. \tag{9}$$

Elsewhere the limit density is zero, including at t. Stirling and
atan(y/α)=π/2−α/y+O(α³) prove pointwise convergence; normalization and
(5) prove the proper-law limit in the compact chart. Right support has
ν negative. A held location fixes t, a held m fixes a; a held positive
scale or a held finite skew forbids V. With β free and t,a held,
βhat=na/Σ1/y. With a free its profile score is
log a−ψ(a)=log(mean(1/y))−mean(log(1/y)), strictly decreasing from infinity
to zero on a>0: the RHS is positive except constant y. Thus a unique
finite a exists for nonconstant transformed data if it is in the eligible
domain, otherwise the restriction face must be checked. If all y equal,
a→∞ is a spike for a free; this is a degenerate sample, not a finite V fit.
Threshold fitting must remain part of the V boundary problem; fixing t=0
would compare the wrong model. Further t→±∞ limits can give normal as a→∞.

### Normal

Every m→∞ path with finite nonzero modal width and finite c converges
to N(c,w²), by section 3. Its likelihood is

$$\ell_N=-{n\over2}\log(2\pi)-n\log w
             -{1\over2w^2}\sum(x_i-c)^2. \tag{10}$$

If λ and ν are both held and α free, eligibility imposes c=λ.
If α,ν both held there is no positive-width normal face. If λ,α both held
there is no positive-width normal face even with ν free, since
w²=[α²+(λ−c)²]/(2m)→0 for bounded c. If λ is free and α held, a normal
face is eligible only with ν free: use r∼√(2m)w/α, λ=c+αr.
If α is free and ν free, c,w can be arbitrary even with λ held:
α²=2mw²−(λ−c)² and ν=2m(λ−c)/α. If α free,ν held,λ free,
c,w can also be arbitrary. These equations provide constructive paths
and prove the restrictions. A free c normal has c=sample mean;
with c held use the held mean. In both cases w²=mean squared residuals.
Zero residual variance gives a spike, not a proper normal boundary fit.

### Gamma is not a type IV boundary

Equation (4) proves the exhaustive nondegenerate closure on m≥1 after
nuisance compactness: finite m gives IV or V; m→∞ gives normal uniformly
in skew. There is no remaining gamma path. An independent check uses
f′/f=−(x−c)/Q(x), with
Q=[(x−λ)²+α²]/(2m), a globally positive quadratic. To obtain a finite
linear Pearson denominator by coefficient convergence, its leading
coefficient would tend to zero, but positivity implies
Q0≥Q1²/(4Q2); hence a nonzero finite linear coefficient is impossible.
This second argument alone assumes coefficient convergence; section 3's
compactification supplies the stronger closure argument. A gamma with finite
shape and free threshold is a boundary of other Pearson types, not IV.
Its hypothetical likelihood on y=s(x−t)>0 would be
−n log Γ(b)−nb log θ+(b−1)Σlog y−Σy/θ. It must **not** be compared as an
IV escape or used to justify an IV restriction. Gamma→normal when its own
shape diverges is of course compatible with the normal face, not a new face.

### Lower shape end and arbitrary spatial divergence

There is a useful exact bound, valid for every ν,α and m>1/2:

$$ |x-\lambda|f(x)\le a\quad(x\ne\lambda). \tag{11}$$

To prove it, first take νz≤0. For t≥|z| on that same side,
g(st)≥g(z)(t/|z|)^(−2m): the power ratio has this bound, and the
skew exponential increases outward. Its integral to infinity is
|z|g(z)/a≤1. On the unfavorable side use g(z)≤g(−z), to which the
favorable bound applies. Thus with λ held and no observations there,
L≤a^n Π|x_i−λ|^(−1)→0 as m↓1/2, uniformly over scale and skew.
At incumbent likelihood P, (11) gives the explicit cutoff
a≥[P·Π|x_i−λ|]^(1/n) whenever λ is held and unobserved.
With scale held the modal density has an explicit O(a)/α bound too.
For m≤1 and |r|≤1, the gamma product gives C(m,ν)≤C(m,0)≤a/2
(the beta integral is at least 2/a), and the modal skew factor is at
most exp(π/2). For |r|≥1 apply (11) at the mode, giving
g(mode)≤a/|r|≤a. Thus sup_x f(x)≤K a/α with K=exp(π/2)/2,
uniformly over λ,ν and 1/2<m≤1. In particular a≥αP^(1/n)/K
on that part of an incumbent superlevel set. This proves uniform
vanishing and supplies a computable lower-shape exclusion.
For any fixed m, (11) at the mode also gives
sup_x f(x)≤2ma/(α|ν|) for ν≠0, proving the held-scale divergent-skew
exclusion without holding location fixed.

If λ and scale are free, unrestricted m admits unbounded collapse for
every sample: choose a>0 small enough for any observed multiplicity to
make (1) negative. With λ held and any observation there, free scale
and free m do the same. Otherwise (11) excludes the lower end. Hence
m≥1 is the weakest n-independent bound protecting all multiplicities
strictly below n/2 with free location and scale: k/(n−k) approaches one
for arbitrarily near-half ties. Boundedness for two distinct observations
already requires a≥1. The bound is sufficient for strict ties by (8).
It prevents unbounded collapse;
it does **not** ensure a finite IV MLE because V/normal may win.

A justified structural exemption is held location: retain m>1/2, stop on
ties at that location if scale is free, and allow genuine V/normal escapes.
Held scale also removes the small-scale reason for the restriction, but
the parameter-policy scope may keep m≥1 until that exemption is adopted.
For an existence-oriented design this report recommends both exemptions,
while explicitly handling constant-sample shape spikes. No default or
metadata is changed here.

For held λ with no ties and m in a compact subinterval of (1/2,1), use
section 3 with M equal to the lower endpoint. A finite-m shrinking w
has c→λ (since λ−c=wDk), and (5) gives a positive exponent with no
central ties. For m≥1 the same holds. Near m→∞ use a larger fixed M
to handle any nonconstant sample. Along sequences with m→1/2, (11)
already forces likelihood to zero. This completes the extended-space
attainment argument for the unrestricted held-location/no-tie subsets.
Diverging original λ,α,ν are therefore neither new models nor dismissible
one at a time: their finite-likelihood interacting rates are exactly the
V and normal constructions above, or the collapse faces. Non-tight paths
with w→∞ or c→∞ are excluded by (6)–(7); w→0 is excluded in strict regimes
or treated as a collapse face. Point masses and zero-likelihood improper
limits are never eligible continuous fitted members.

## 5. Fixed-shape certificates and shape profiling

At held m,ν, a location-only score has zeros given by a polynomial of
degree at most 2n−1 after multiplying the positive quadratic denominators.
Every location maximum lies in
[xmin+αν/(2m), xmax+αν/(2m)]; outside, all observation scores have the
same sign. The lst location isolator can therefore be extended to the
additional skew numerator, including multiple roots and unresolved ties.
Do not retain its unshifted [xmin,xmax] bracket.

The scale-only score similarly becomes a polynomial of degree at most 2n
in α after denominator clearing. Include the endpoint α=0 face, even
when multiplication introduces spurious roots there; only positive roots
are eligible. For ν=0 the monotone lst scale score is recovered. For
ν≠0 isolate **all** positive stationary roots, compare likelihood bounds,
and retain budget/numerical failures. At fixed shapes with both nuisances
free the two rational score equations are algebraic. Use certified boxes
or elimination plus positive-dimensional components; do not assume a
finite list of simple roots (the Cauchy continuum disproves that).

At held nuisances, shape concavity is stronger than boxes and supplies
unique finite conditional maxima whenever attained. Profile ν at each m
by its strictly decreasing score: sample average atan z lies strictly
between ±π/2 and Eν atan z covers that interval as ν traverses R.
Then profile m or use joint concave optimization. If only ν is free this
always attains uniquely. If only m is free, m∞ can spike only when every
observation equals held λ; otherwise coercivity gives a unique maximum,
including m=1 when that restriction is active. Both shapes free with fixed
nuisances spike for any constant sample; nonconstant samples have a unique
finite maximum (lower end vanishes, m∞ concentration cannot cover two values).

**Design for other subsets.** Use c,log w, inverse D and k compactification
for global coverage, intersecting exact hold constraints before choosing
coordinates. Concave shape fits and rational fixed-shape fits provide
proposals and box improvements, never replace the global comparison.
Bounds (5)–(8),(11), observed spacings and a feasible incumbent supply
ranges. Branch over the at most four free dimensions, include m=1 where
eligible, the V face, the normal face and all eligible collapse faces.
Derivative/Taylor or interval bounds on the elementary terms and normalizer
bound the likelihood on each compact box. Fair subdivision gives a
positive likelihood-value-gap certificate on this compact closure.

This is **not** by itself the package's coefficient accuracy contract.
An `Ok` requires localization of every free coefficient within relative
tol of some attained maximizer, using a score-Jacobian lower singular-value
bound or a shrinking argmax enclosure. Use the contract's
tol·max(|θ_i|,s_i) convention, with location floor the fitted scale and
the default unit floor for sign-changing skew. Flat/continuum cases require an
exact identified maximizing set and a chosen member. If multiple boxes
remain or a limiting face ties without an existence decision, stop with
the witnesses, not `Ok`. The global finite-value boundary decision remains
open E in exact ties; there is no assumed shape-profile unimodality.

Structured outcomes should distinguish UnboundedCollapse (t,m,e witness),
UnboundedShapeConcentration (constant sample/path), TypeVLimit (orientation,
threshold,shape,β,ℓ bracket), NormalLimit (eligible mean/scale),
BoundaryComparisonUnresolved, NumericalFailure and Budget. Only a proved
winning normal face can say `use mlnorm`, with any inherited constraint
stated. V with a free threshold has no package delegate; say explicitly
that the supremum is a reflected/translated inverse-gamma boundary absent
from the package. Even held t≠0 or reflected V cannot be passed blindly
to `mlinvgamma`; no fictitious `use mlX` is allowed. A positive inward
V coefficient certifies improvement into IV locally, not a global MLE.

Every fitting loop has the shared 64-bit work ledger: solver iterations
min(iterlim,100000), certificate evaluations 128 times that quantity,
enumeration cells 100000 times it. Nested special-function costs must be
charged or separately documented under the evaluator exception. Numerical
comparisons use an explicit practical floating-point margin, initially
128ε times the accumulated term magnitude; this is an engineering proposal,
not a roundoff proof. Unresolvable comparisons fail safely. R6's score gate
cannot establish global maximality or dismiss a boundary winner.

## 6. Interior information and boundary inference

On a compact neighborhood of any finite interior parameter, m is bounded
below by m0>1/2, α away from zero, and λ,ν bounded. The density and its
square-root derivatives have an integrable envelope proportional to
(1+x²)^(−m0) [1+log²(1+x²)]. Location and log-scale scores are bounded;
skew is bounded; the m score grows only logarithmically. Smoothness and
dominated convergence give L2 continuity of the square-root derivative,
and the fundamental theorem of calculus gives DQM. Thus the Fisher
information E SSᵀ exists for every interior m>1/2, including infinite-mean
and infinite-variance members. No moments of X are used.

For full rank suppose a linear combination of the scores vanishes a.s.
Continuity and positive full support make it an identity in z. At ±∞,
the logarithmic m term first forces its coefficient to zero. The two
limits of atan then force the skew coefficient to zero. The remaining
identity, multiplied by 1+z², is

$$b[(2m-1)z^2+\nu z-1]+d(2mz+\nu)/\alpha=0.$$

Since 2m−1>0, b=0 and then d=0. All four scores are independent. Every
held-coordinate information principal submatrix is therefore positive
definite. At ν=0 the odd (location,skew) and even (scale,m) blocks are
orthogonal, but each block has rank two: symmetry is an interior Student-t
slice, not a skew-normal-style singularity. DQM/rank alone do not establish
existence, consistency of an unrestricted global MLE, or valid finite-sample
Wald coverage. At an imposed m=1 fit boundary ordinary unconstrained Wald
inference also requires modification.

For V let ε=α², ν=−sβ/α, λ=t and m=(a+1)/2. On y>0 expansion gives

$$\log f_{IV}=\log f_V+\epsilon H(y)+O(\epsilon^2),$$
$$H(y)={a(a^2-1)\over6\beta^2}-{a+1\over2y^2}+{\beta\over3y^3}. \tag{12}$$

Stirling's 1/|ν|² correction and the atan expansion give (12); taking
expectations with E y^(−j)=(a)_j/β^j proves E H=0. This independently
checks the sign convention behind Cheng's quadratic expansion. Profiling
V nuisances leaves ΣH at the V optimum as the inward score; positivity
implies strict improvement for sufficiently small ε>0.

The V three-parameter model (t,β,a) itself is DQM with full rank for all
a,β>0: its threshold is infinitely flat, all inverse moments exist, and
its score functions span log y,1/y,1/y² plus constants. No endpoint power
threshold such as gamma's applies. H contributes a nonzero 1/y³ term,
so the four-dimensional extended V chart has an independent, square-integrable
one-sided score. To justify DQM at ε=0, truncate y to [ε^γ,ε^(−γ)] with
small γ>0, apply the uniform expansion there, and bound small-y contributions
by exp(−β/(2y)) and the wrong-side leakage by exp(−c/√ε); large-y
contributions use the integrable inverse-power envelope. The same argument
with the square-root remainder proves one-sided L2 differentiability.
The ε≥0 boundary therefore has a cone experiment; a half-chi-square LR
limit needs the usual consistency/localization conditions and cannot be
inferred merely from a fitted boundary score. α itself has zero first
score in this chart at the boundary and is not root-n regular.

At the normal face c,w survive but m,ν,λ,α do not form an identifiable
finite chart. Four-parameter inverse-Hessian Wald inference in the original
coordinates is invalid there. Hermite skewness/kurtosis coordinates and
the feasible cone must be derived before giving a normal-face LR law.
**Open inference N:** no full normal-face local experiment or uniform
asymptotic test calibration is proved here. There is nevertheless a useful
formal reduction: put δ=1/D. Expanding (4) around the mode gives
log[h(u)/h(0)]=−u²/2−(2/3)kδu³+(1/4−k²)δ²u⁴+O(δ³) on bounded u.
After centering and variance standardization, the leading skewness is
−4kδ and excess kurtosis is 6(1+4k²)δ². Their formal root-n tangent
set is a free cubic Hermite direction and a nonnegative quartic direction.
This suggests, but does not prove, a cone rather than a four-coordinate
Wald limit. Missing are uniform L2/LAN remainders over δ,k joint rates,
the profiling/localization theorem, and exclusion of distant competing
maxima under normal sampling. A bounded-u density expansion alone cannot
justify a chi-square-mixture assertion. There is no gamma boundary
inference problem for IV; a separate free-threshold gamma family would
have its own endpoint regularity conditions.
