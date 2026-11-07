# Proofs and implementable bounds

All likelihoods below use the closed-support density convention R7a. Data are
finite, n>0, and sigma>0. Products and bounds should be evaluated in logs.
These are exact-arithmetic statements, not claims about the present fitter.

## 1. Quantile coordinates and inversion

Write v=1-u and

\[
 Q_l(u)=\int_{1/2}^u D_l(t)\,dt,
 \qquad D_l(u)=u^{l-1}+v^{l-1},\qquad
 g_l(Q_l(u))=D_l(u)^{-1}.
\]

The integral is also (u^l-v^l)/l, with its continuous logistic value at l=0.
D is strictly positive, so there is exactly one inverse in the open support.
For u>1/2, Q is strictly decreasing in l: differentiate under the integral;
each term in D_l's shape derivative is negative. The smaller-tail magnitude
h_l(p)=-Q_l(p), 0<p<=1/2, decreases strictly in both p and l.
This is monotonicity of the quantile, not of the density at fixed x.

Explicit initial brackets eliminate unbounded inversion searches. For
z=|x-mu|/sigma>0:

* l=-a<0: `(az+2^a)^(-1/a) <= p <= min(1/2,(az+1)^(-1/a))`.
  This follows from `1 <= (1-p)^(-a) <= 2^a` in
  `az=p^(-a)-(1-p)^(-a)`. Use logarithmic arithmetic.
* l=0: `p=1/(1+exp(z))`, evaluated by log1p/logaddexp.
* 0<l<1, with d=1-lz>0:
  `(d/2)^(1/l) <= p <= min(1/2,d^(1/l))`.
  Indeed `d=1-(1-p)^l+p^l`, and `p^l <= d <= p^l+p <= 2p^l`.
* l>1, d>0: `d/(l+1) <= p <= min(1/2,d)` because
  `p <= 1-(1-p)^l <= lp` and `p^l<=p`.
* l=1: `p=(1-z)/2`; l=2: `p=1/2-z`.

Support checks precede inversion. Zero residual gives p=1/2; a support
endpoint gives p=0 with the appropriate limiting density. Bisection contracts
these finite brackets, with safeguarded Newton optional. A bracket, rather
than an unchecked approximate inverse, is the certificate primitive.

For a parameter box with l in [L,U] and standardized magnitude in [z0,z1],
the inverse lies between the root of h_U(p)=z1 and the root of h_L(p)=z0.
An unattainable lower-tail root at the support edge is replaced by p=0.
If even h_L(0)<z0, the entire box is infeasible. Handle a residual interval
crossing zero by setting z0=0. The use of interval bounds loses correlation
but preserves containment. These bounds also work on observation strata.

## 2. Concavity that survives the inverse map

For l<1 put b=1-l>0. The log density as a function of z has curvature

\[
 (\log g_l)''(z)=-\frac{D D''-2(D')^2}{D^4},
\]

where primes on D differentiate u. Direct expansion gives

\[
DD''-2(D')^2=b\{(1-b)(u^{-2b-2}+v^{-2b-2})
 +(b+1)(u^{-b}v^{-b-2}+v^{-b}u^{-b-2})
 +4b u^{-b-1}v^{-b-1}\}.
\]

For 0<=l<1, 0<b<=1, every term is nonnegative and cross terms are
positive. Thus g_l is strictly log concave on its support. The extension
by zero outside its convex support is log concave. At l=1 the density is
constant on the support.

With A=1/sigma and B=-mu/sigma,

\[
 \ell(A,B)=n\log A+\sum_i\log g_l(Ax_i+B)
\]

is concave for fixed 0<=l<1. Holding mu imposes the affine constraint
B=-mu*A; holding sigma fixes A. For a nonconstant sample with both A,B
free it is strictly concave: n log A is strict in A, and any change in B
at fixed A is strict. The same proof gives uniqueness for either scalar
free coordinate whenever a finite optimum exists. A constant sample with
mu held away from that value also has a unique free-scale optimum. This
is a global result, not a local Hessian test. The negative log density
gives a convex minimization problem; stationary score, boundary exclusion,
and concavity certify its answer. l=0 is included.

For l<0, b>1, the leading endpoint term in the braces is negative.
The tails are log convex, so this route does not extend to negative shapes.
Although -log D_l(u) is concave separately in u (l<1) and in l (fixed u),
the equations x_i=mu+sigma*Q_l(u_i) are nonlinear constraints. Separate
latent-coordinate concavity proves no joint likelihood concavity or profile
unimodality. No such assumption is used below.

For 1<l<2, D''<0 and hence DD''-2(D')^2<0: log g is strictly convex.
It is increasing in |z|. With sigma held, the location likelihood is
strictly convex on its feasible interval and its maximum is at one of the
two interval endpoints. With mu held, every density decreases with sigma,
so the scale optimum is exactly `sigma=l*max_i|x_i-mu|` (if this radius
is nonzero). With both free, it suffices to search the two support-contact
strata `sigma=l*max_i|x_i-mu|`, partitioned at the midrange. Do not use
the principal-branch concavity routine here. For l>2 no concavity claim
is needed; use the compact box routine, including contact faces.

## 3. Uniform envelopes

For l in [-1,1],

\[
 g_l(z)\le\tfrac12,\qquad z^2g_l(z)\le1,
 \qquad f(x)\le\min\{(2\sigma)^{-1},\sigma/(x-\mu)^2\}. \tag{1}
\]

Proof: D has its minimum at u=1/2 for l<=1, giving
g<=2^(l-2)<=1/2. On the smaller tail put theta=(1-l)/2 and w=1-2p.
Holder's inequality on [p,1-p] gives

`h_l <= w^(1-theta) h_-1^theta`.

Subadditivity of t^theta gives `D_l >= D_-1^theta`.
Consequently `h_l^2/D_l <= w^(2(1-theta)) (h_-1^2/D_-1)^theta <=1`,
because `h_-1^2/D_-1=(1-2p)^2/(p^2+(1-p)^2)<=1`.
The endpoints theta=0,1 follow directly. This proves (1) without numerical
maximization of an envelope constant.

For a held negative shape l=-a, define

\[
 r=1+1/a,\quad c=1/a,\quad M=2^{-a-2},\quad C=a^{-r}.
\]

Then `g<=M` and `|z|^r g(z)<C` for z!=0. Indeed
`h=(p^-a-(1-p)^-a)/a < p^-a/a` and `D>=p^-a-1`.
Thus `f<=min(M/sigma,C sigma^c/|x-mu|^r)`.
For an interval of shapes [-A,1], A>0, the same Holder argument with
theta=(1-l)/(A+1), r=1+1/A gives the uniform constants

\[
 M=1/2,\quad C=\max(1,A^{-r}),\quad c=1/A. \tag{2}
\]

For any held positive shape, support gives |z|<=1/l and
`g<=M`, with M=1 for 1<l<=2 and M=2^(l-2) for l>=2;
for 0<l<=1 one can use M=2^(l-2). In particular
`z^2 g<=M/l^2`. All constants are finite for each held shape. These
positive-shape bounds are supplemented by the exact support constraint.

## 4. Explicit finite nuisance ranges

Take any positive feasible incumbent P=exp(ell0). For an envelope
`min(M/sigma,C sigma^c/|d|^r)`, r=c+1, define
`B=M^(c/r) C^(1/r)`. Optimizing the envelope over sigma gives
`f(x)<=B/|x-mu|`. Therefore:

* `sigma <= M P^(-1/n)` for every candidate at least as good as P.
* `dist(mu,[xmin,xmax]) <= B P^(-1/n)` for every such candidate.

The second inequality excludes diverging location even when scale varies.
These are safe search ranges, not recommended direct floating-point formulas.
Inflate slightly or use a strict target below ell0 when pruning equality.

For a free location and nonconstant sample let Delta be the minimum spacing
between distinct observed values and delta=Delta/3. Partition location into
the disjoint delta-neighborhoods of each distinct value, and their complement.
Inside neighborhood j of multiplicity k_j every other residual has absolute
value >=delta; hence

\[
 L\le M^{k_j} C^{n-k_j}\delta^{-r(n-k_j)}
        \sigma^{c(n-k_j)-k_j}. \tag{3}
\]

On the complement use k=0. If every exponent e_j is positive, (3) gives
the explicit lower scale bound `(P/coefficient_j)^(1/e_j)` in that
neighborhood; take the minimum over neighborhoods and complement for one
global box. With held mu and no ties at mu, the sharper bound is

`L <= C^n sigma^(cn) product_i |x_i-mu|^-r`.

With held mu and k ties, replace the coefficient in (3) by
`M^k C^(n-k) product_nonzero |x_i-mu|^-r`.

These arguments prove compact superlevel sets on [-1,1] when
`2*k_max<n`, or at held negative shape when `k_max<a^-1*(n-k_max)`.
For held scale no lower-scale exclusion is necessary. On the principal
branch, tails in mu vanish uniformly by (1).

For fixed 0<l, the support condition instead gives
`sigma>=l*range/2` with mu free, or `sigma>=l*max|x-mu|` with mu held.
Nonconstant data give a positive lower bound. For fixed l=0,
`g_0(z)<=exp(-|z|)` yields

`L<=sigma^-n exp(-S/sigma)`,

where S=min_mu sum|x_i-mu|>0 for a nonconstant sample, or S=sum|x_i-mu|>0
at a held location. This excludes collapse even with arbitrarily large
ties. For an explicit bound, take sigma<=S/n, where this bound increases
with sigma, and bisect for the incumbent crossing. If all observations
coincide with a freely eligible location, free-scale likelihood is unbounded.

## 5. The unrestricted negative-shape end with held location

This end cannot be replaced by a fixed numerical cutoff such as -16.
It has two different uniform bounds.

If sigma is held, `L <= (2^(-a-2)/sigma)^n` for l=-a<0,
regardless of the observations. Thus a is bounded above explicitly by
`-(ell0/n+log sigma)/log 2-2` on the incumbent superlevel set (take any
larger positive A, and include l>=0 separately).

If sigma is free and mu is held, let k be the tie count at mu. When k>0,
choose any fixed `a>(n-k)/k`. The scale-collapse exponent is negative;
likelihood is unbounded. When k=0, a joint shape/scale escape is excluded by

\[
 |x-\mu|f(x)=h_{-a}(p)/D_{-a}(p)<p/a\le1/(2a),
 \quad L<(2a)^{-n}\prod_i|x_i-\mu|^{-1}. \tag{4}
\]

Consequently choose
`A > (exp(-ell0) product_i |x_i-mu|^-1)^(1/n)/2`.
All l<-A are worse than the incumbent, uniformly over sigma. On [-A,1],
(2) and the no-tie form of (3) give a positive lower scale bound and the
usual upper bound. This proves existence for *every* nonempty sample with
no observation at held mu, including a constant sample away from mu.
With both mu and sigma held, the first bound proves attainment in shape.
There is no additional nonzero-likelihood law at l=-infinity in these cases.

## 6. Collapse faces, including equality

At a held negative shape l=-a and a fixed nonzero residual d,

\[
 f(x)\sim a^{-r}\sigma^{1/a}|d|^{-r},\qquad
 f(\mu)=2^{-a-2}/\sigma.
\]

With k observations at the collapsing location t, the exponent is
`e=(n-k)/a-k`, and the coefficient is

\[
 F_t(a)=2^{-(a+2)k}a^{-r(n-k)}
             \prod_{x_i\ne t}|x_i-t|^{-r}. \tag{5}
\]

For e<0 likelihood is unbounded. For e>0 it vanishes. For e=0 with
**held mu=t**, the strict bound in section 3 gives `L<F_t(a)` for every
positive scale, and the asymptotic attains this supremum only at sigma=0.
Thus this entire scalar equality case is proved nonattainment; no numerical
face comparison is needed. (For n>0 equality requires both k>0 and n-k>0.)

With **free mu**, an equality collapse has mu tending to some observed t.
If `(mu-t)/sigma -> v` is finite, replace the central factor in (5) by
`g_-a(-v)^k`, at most `2^(-(a+2)k)`. If this ratio diverges, the same
upper bound still holds, while noncentral distances converge to fixed
nonzero values. The bound of section 3 proves the limsup is <=F_t(a).
The value F_t(a) is attained as a face value along mu=t, sigma->0.
For a finite neighborhood |mu-t|<=eta<half the nearest distinct spacing,
replace every distance in (5) by `|x_i-t|-eta` to get an explicit upper
bound on the whole equality-collapse neighborhood, uniform in sigma.
It tends to F_t(a) as eta->0.

For shape free with location free, negative shapes are in [-1,0).
If `2*k_max>n`, l=-1 already makes likelihood unbounded. If
`2*k_max=n`, the only nonzero collapse faces occur at l=-1 and are

`F_t(1)=2^(-3k) product_noncentral |x_i-t|^-2`.

To prove exhaustion, exclude shapes bounded away from -1 by (3), which
then has a uniformly positive exponent. Near -1 use the exact negative
shape constants; they converge to (5), and the scale exponent is
nonnegative, so for sigma<=1 its contribution is <=1. Central factors
are bounded by their modal value. This proves the same face limsup,
including shape approaching -1 at rate 1/|log sigma|. A fixed-shape-only
asymptotic would not justify that assertion.

A finite computable upper bound near this free-shape face is obtained by
restricting a in [1-epsilon,1], taking the maximum of the continuous
coefficient `M(a)^k C(a)^(n-k) product(|d_i|-eta)^(-r(a))`, and dropping
the factor sigma^e<=1. Bound this scalar coefficient with interval
evaluation; it tends to F_t(1). The remaining shapes have a positive
power bound. Thus an incumbent strictly above all equality faces gives
finite lower scale bounds and an attained global maximum.

**Open exact decision:** with free mu, if no finite incumbent strictly beats
the largest equality face, the arguments above do not prove nonattainment
or rule out a finite tie with that face. They do give global upper bounds
arbitrarily close to it. A follow-up may stop with an unresolved numerical
comparison and face witnesses; it may not report CollapseLimit solely from
a tolerance tie. It may not return an approximate face as an attained MLE.
An analytic inequality or complete exact critical-value comparison is
missing in general for these free-location equality cases. The important
held l=-1 case is settled immediately below; section 9 describes a
concrete exact subroute and why it does not settle arbitrary held shapes.

### A sharper result at held l=-1

Here there is an exact stronger envelope:

`D_-1(u)-Q_-1(u)^2=2/[u(1-u)]>=8`,

so `f(x)<=sigma/[(x-mu)^2+8sigma^2]`, strictly unless x=mu.
For two distinct observations t,x, put A=t-mu, B=x-mu and s^2=8sigma^2.
The identity

`(A^2+s^2)(B^2+s^2)-s^2(A-B)^2=(AB+s^2)^2>=0`

therefore gives `f(t)f(x)<1/[8(x-t)^2]` at every finite positive scale.
If k=n/2 observations equal t, pair them with all noncentral observations.
Multiplication proves `L<F_t(1)` for *every* mu and sigma. The largest
collapse face is the unattained global supremum. This settles the entire
held-l=-1 equality case, including two balanced point clusters; a finite
tie is impossible. The proof does not extend to free shape: for example
g_1(0)=1/2 violates the envelope, and for data (0,1) the exact uniform fit
has likelihood 1, exceeding the l=-1 collapse face 1/8.

More generally, with two distinct values each repeated k times, the free
shape uniform candidate has likelihood `|x-t|^(-2k)`, whereas each collapse
face is `8^(-k)|x-t|^(-2k)`. This strict gap proves an attained free-shape
maximum, even though the sufficient strict tie guard fails. It does not
prove which finite shape maximizes. Thus identical balanced two-cluster
data have no finite MLE with l=-1 held, and do have one with l free.

## 7. Bounded endpoints and uniform members

For fixed 0<l<1, write delta=sigma/l-|x-mu|, measured in data units.
As delta decreases to zero,

\[
 p\sim(l\delta/\sigma)^{1/l},\quad
 f(x)\sim\sigma^{-1}(l\delta/\sigma)^{(1-l)/l}.
\]

Thus any observed endpoint has zero likelihood for l<1. For l>1,
the endpoint density is 1/sigma; the deficit is asymptotic to p and
contact strata can maximize the likelihood. l=1 has endpoint density
1/(2sigma); l=2 has density 1/sigma throughout. Both are uniform laws,
with halfwidth sigma/l. Handle these exact members before inversion.

When l increases to 1 from below simultaneously with p decreasing to zero,
put t=(1-l)*|log p|. Possible limiting densities are
`1/[sigma*(1+exp(t))]`, including zero when t=infinity. None exceeds
the l=1 value. Thus the principal-branch likelihood is upper
semicontinuous at the uniform face, including contact corners, but is
not continuous there. Any proof relying on a uniform endpoint expansion
for 0<l<1 as l->1 is invalid. The simple bound `D>=2` handles this corner.

The exact uniform subproblem with halfwidth w=sigma/l is:

* mu and sigma free, nonconstant sample: midrange mu and w=range/2.
* mu held, sigma free, nonzero radius: w=max|x_i-mu|.
* sigma held, mu free: every `mu in [xmax-w,xmin+w]` is a maximizer
  if that interval is nonempty; likelihood is `(2w)^(-n)`.
* both held: either feasible with that likelihood or infeasible.

When shape is free the uniform face is l=1 only. l=2 is eligible only
when held. Positive-width uniform fits at endpoints are attained, with
no epsilon inflation in the mathematical problem. Floating-point support
checks need a documented margin.

## 8. A global box certificate without a uniqueness assumption

In the strict regimes, sections 3--5 give a finite rectangular superlevel
region. Intersect it with held-coordinate planes and support conditions;
separate the exact uniform face and the lower fitted shape face. For held
l>1 also enumerate the two extreme-observation contact strata
`mu-sigma/l=xmin`, `mu+sigma/l=xmax`, and their intersection, eliminating
holds before choosing free coordinates. Evaluate zero-dimensional strata
directly. This includes a singleton feasible location interval when held
support width exactly equals sample range. The
remaining search dimension is at most three. Keep observation-stratum
boundaries when useful; there is no need to assume finitely many simple
stationary roots.

For each box:

1. Bound |x_i-mu|/sigma, discard impossible support intersections, and
   enclose the smaller inverse tail by section 1.
2. For principal shapes, D decreases in shape and decreases as p approaches
   1/2. Hence `D_U(p_max)` is a lower bound on every admissible D, and
   `1/[sigma_min*D_U(p_max)]` is a density upper bound. For a held shape
   outside the principal branch, minimize D over the inverse bracket:
   check its endpoints and 1/2 if present. D has no other stationary
   points except for the exactly constant uniform cases. Include endpoint
   limits. Sum logs of these density bounds.
3. Evaluate feasible proposals with bracketed inverse values, retaining a
   lower bound for each likelihood. A local optimizer supplies proposals
   only; failure never supplies a successful fit. Include every eligible
   exact uniform candidate/plateau, restriction face, and positive-density
   support-contact stratum. Proposals must be dense relative to each
   surviving feasible stratum, including any dimension-reduced one.
4. Discard boxes whose upper bound is below the incumbent lower bound plus
   the chosen likelihood tolerance. Split the longest suitably scaled
   coordinate of an unresolved box; retain all boxes tied for the maximum.
   Refine inverse brackets enough that their error also fits the tolerance.

In exact arithmetic these bounds converge to the density at every interior
point, to zero at fixed sub-uniform support contacts, and to at most the
uniform density at the joint l=1 contact corner. At an exterior point the
support test eventually discards the box. Held l>1 endpoints are continuous
relative to the closed support. Together with compactness, these properties
give finite termination for any *positive likelihood-value tolerance*,
using fair subdivision and a dense feasible proposal schedule. One proof
is to cover the compact parameter region by finitely many neighborhoods
whose box upper bounds are below the global supremum plus epsilon; a
feasible proposal within epsilon of that supremum eventually exists. The
separate uniform-face evaluations are essential to that last statement.

This certifies a value gap, not exact real-valued coordinates, a unique
maximizer, or a complete root list. Plateau fits are legitimate. If a face
excluded from the domain can tie the value, value-gap termination alone
does **not** certify existence: apply section 6's extra decision rule.

Use the package's exact-arithmetic/conditional-comparison standard. A practical
initial floating-point policy is per-observation likelihood tolerance
`tau=max(reltol,64*epsilon*(1+max_abs_log_density))`, with conservative
128*epsilon-scaled padding on computed box bounds and comparison terms.
This is a proposed engineering margin, **not a proved roundoff bound**;
calibrate it against high precision before adopting it. Cancellation,
underflow or a bracket whose comparison cannot be resolved must stop as
numerical failure, never silently prune. All search and inversion loops
use the shared budgets: 128*min(iterlim,100000) certificate evaluations;
min(iterlim,100000) solver iterations, 64-bit counters. Exhaustion produces
budget witnesses (unresolved boxes, incumbent, upper bounds), not success.
No fixed shape/scale cap may replace the derived bounds.

## 9. Exact rational held-shape route and its limitation

Every binary floating-point held l is a rational number m/d, d>0. Adjoin
positive variables s_i=u_i^(1/d), t_i=(1-u_i)^(1/d) satisfying
`s_i^d+t_i^d=1`. For m!=0, Q and D are rational expressions in s_i,t_i;
negative powers can be cleared with positive denominators on interior
strata. The equations `x_i=mu+sigma Q_l(u_i)` and score equations are
therefore polynomial after denominator clearing. At l=-1 they are
particularly small: `Q=(2u-1)/(u(1-u))`,
`D=(u^2+(1-u)^2)/(u^2(1-u)^2)`.
Also the likelihood itself, not just its scores, is rational in these
variables, mu and sigma. Thus real algebraic feasibility/optimization
can compare finite candidates to (5), including an exact equality test,
when data and held parameters are rational. Strict positivity and all
support/contact strata must be preserved. Positive-dimensional critical
sets cannot be dropped. This establishes finite algebraic structure for
each rational nonzero held shape, not a practical low-degree algorithm
for arbitrary binary64 denominators. l=0 is handled by concavity instead.

With free shape, powers u^l are not algebraic in this construction.
No effective finite polynomial reduction or proof of a bounded number of
shape-profile modes is supplied. The box route avoids that need for
positive-tolerance strict-existence cases, but does not settle the exact
free-shape equality-collapse decision. Implementing a general algebraic
decision engine is also outside a small C++17 header-only follow-up.

## 10. Exact uniqueness counterexamples

1. Held l=1, sigma=1, observations (-1/4,1/4): every mu in [-3/4,3/4]
   has the same global likelihood 1/4. The same phenomenon occurs at
   held l=2 with the corresponding halfwidth. An isolated-root certificate
   would miss this entire component.
2. Held l=3/2, sigma=1, observations (-1/4,1/4): the feasible location
   interval is [-5/12,5/12]. Section 2's strict convexity and symmetry
   give exactly two global maximizers, its endpoints. This is an exact
   bounded-support counterexample, not a numerical mode search.
3. Held l=-1, sigma=1, observations (-80/9,80/9): the midpoint has u=1/10
   or 9/10. Substitution in DD''-2(D')^2 gives a strictly negative
   rational number. Thus the location log likelihood has positive second
   derivative at zero, which is a strict local minimum. It is symmetric,
   continuous, and tends to minus infinity as |mu| tends to infinity;
   therefore it has at least two distinct global maxima. This shows
   nonuniqueness *inside* the fitted shape range without a uniform plateau.

These counterexamples disprove general uniqueness across all hold subsets.
They do not prove multiple maxima for every subset with shape free.

## 11. Scores and implementation order

For interior observations let E=partial_l D and H=partial_l Q. Then

\[
 \partial_\mu\ell=\sum_i\frac{D_i'}{\sigma D_i^2},\quad
 \partial_{\log\sigma}\ell=-n+\sum_i\frac{Q_i D_i'}{D_i^2},\quad
 \partial_l\ell=-\sum_i\left(\frac{E_i}{D_i}
                    -\frac{D_i'H_i}{D_i^2}\right).
\]

Here `E=u^(l-1)log u+v^(l-1)log v`,
`H=integral_(1/2)^u [t^(l-1)log t+(1-t)^(l-1)log(1-t)]dt`.
For l!=0 this integral equals
`[l*(u^l log u-v^l log v)-(u^l-v^l)]/l^2`;
at l=0 it is `(log(u)^2-log(v)^2)/2`.
Use series near zero rather than the subtractive expression. These are
total log-likelihood scores in (mu,log sigma,l); they are not information
matrices or inference claims. Verify derivatives independently before
using a score gate. At contact strata differentiate the stratum objective,
not these interior expressions at p=0.

An implementation can proceed in independently reviewable pieces:

1. Certification-specific inverse brackets and box upper bounds, leaving
   the settled d/p/q/r unchanged. Compare high-precision quantile-generated
   values, tiny shape, extremely small tails and endpoint/contact boxes.
2. Fixed nonnegative shape concavity, exact uniforms and the two exact
   1<l<2 scalar reductions; implement support feasibility before optimization.
3. Uniform envelopes and explicit superlevel bounds, including the joint
   held-location/free-shape cutoff. Save the incumbent and each exclusion
   inequality as certificate witnesses. This replaces arbitrary multistart
   cutoffs without changing which domain is eligible.
4. Scalar, two-dimensional and three-dimensional boxes under one shared
   work budget; proposals can reuse current starts but cannot decide success.
   Compare existing goldens without changing their acceptance baseline.
5. Exact collapse classifications, the held-l=-1 pairing inequality, and
   strict-improvement treatment of the remaining equality faces. Return
   an unresolved comparison when that is all the evidence establishes.

The general branch certificate is sufficient for positive-tolerance value
certification. Its speed, especially near l=1 support corners or in shallow
negative-shape valleys, is unmeasured. Analytic score/curvature pruning and
concave profiles can improve it, but are performance work rather than
prerequisites for the existence reductions proved here.
