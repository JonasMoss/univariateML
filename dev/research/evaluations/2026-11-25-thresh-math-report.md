# Existence with free thresholds and held coefficients

TASK-263. Base `7ed10c94`. The [mask table](2026-11-25-thresh-math-data/masks.csv)
covers all 39 nonempty held sets that leave a threshold free. Every
existence classification below is proved. The finite solutions are unique
except that uniqueness of the global maximum is unresolved for the
held-scale, free-upper-tail-shape profile, which provably can have two
local maxima. No in-scope positive finite supremum requires a named
limit model. The finite scale-collapse supremum at a tie equality is
unattained.

## Scope and notation

The mask column lists held coefficients using their public names.
Pareto II and III contribute three rows each, Pareto IV seven,
Feller–Pareto fifteen and nonstandard beta eleven. The first two,
Feller–Pareto and nonstandard beta route the corresponding free threshold
requests to unsupported outcomes in their public C++ entrypoints. Pareto IV
already reports unboundedness for the four structural masks in the CSV,
and for held power below one in its remaining three masks. The latter
three reach unsupported for held power at least one. The structural rows
are included so the table can also serve as a dispatch specification.
Fully free fits and fits with all thresholds held are outside the table.

Samples are finite and nonempty, and held coefficients are valid finite
members. For the Pareto families, let \(m=\min x_i\), \(t=m-\mu\ge0\),
\(d_i=x_i-m\), \(y_i=t+d_i\), and let \(k\) count the zero gaps. Use the
GB2 density for an excess \(y>0\),

$$
 f(y)=\frac{C}{s}\,g(y/s),\qquad
 C=\frac{c}{B(p,q)},\qquad
 g(r)=\frac{r^{a-1}}{(1+r^c)^{p+q}},\qquad a=cp,\quad b=cq.
$$

In Feller–Pareto, \((p,q,c,s)\) are
`(shape3, shape1, shape2, scale)`. Pareto IV fixes \(p=1\).
Pareto III further fixes \(q=1\), and Pareto II instead fixes \(c=1\).
The symbols \(a,b\) in the Pareto rows of the CSV denote lower and upper
powers, and \(E=b(n-k)-k\). All endpoint statements use R7a and the
dossiers' inside-support densities. A threshold outside the sample's
feasible range gives no positive likelihood.

The [original threshold report](2026-10-29-threshold-report.md) and
[P0 family analysis](2026-10-29-p0-genbeta.md) give the shift
identities, fixed-scale endpoint spikes, and trial-ray collapse exponent.
The [P0 half-threshold report](2026-10-29-p0-halfthreshold-report.md)
and [P2 implementation report](2026-10-29-p2-halfthreshold-report.md)
distinguish attained endpoint fits from scale-collapse comparisons.
Here a uniform bound decides the equality case without an interior
comparison. The [Pareto IV switch report](2026-11-04-pareto4-switch-report.md)
certifies the fit with the threshold held. That certificate and the
held-threshold Weibull, Pareto and GB2 face comparisons do not decide the
joint free-threshold problem.

## Structural spikes

**Proved.** The earlier fixed-scale spike applies whenever \(cp<1\).
As \(t\downarrow 0\), the minimum factors are of order
\(t^{k(cp-1)}\), and the other factors have positive limits.
A free \(c\) or \(p\) can always produce this inequality.

With \(q,s\) both free, the
[existing Pareto IV collapse](2026-11-14-core5b-c-data/mathematics.md)
extends to every fixed \(p,c>0\). Take \(q=1/T\), \(s=t=e^{-T}\).
Since \(1/B(p,q)\sim q\), each minimum density grows in proportion to
\(e^T/T\), and a nonminimum density is asymptotic to
\(c e^{-c}/(Td_i)\). The likelihood diverges as a positive constant
times \(e^{kT}/T^n\). This exhausts the structural masks in the
Pareto table and leaves only the three types below.

## Fixed regular coefficients

**Proved.** If \(a<1\), use the spike above. If \(a=1\), the density
strictly decreases in excess and the threshold is exactly \(m\).
For \(a>1\), write \(u_i=y_i^c/(s^c+y_i^c)\) and \(w_i=1/y_i\).
The threshold-gap score is \(\sum_i w_i H_i\), with
\(H_i=a-1-(p+q)c u_i\). Its normalized version is the weighted mean
\(\langle H\rangle_w\). Differentiate it with respect to \(t\).
The derivative is \(\langle H'\rangle_w-\operatorname{Cov}_w(H,w)\),
because \(w_i'=-w_i^2\). The first term is strictly negative.
Both \(H_i\) and \(w_i\) decrease when the gaps are sorted in
increasing order, so their covariance is nonnegative by the pairwise
covariance identity.

The normalized score starts at \(a-1\) as \(t\downarrow0\), and
ends at \(-1-b\). The likelihood is strictly unimodal with a unique
interior root. The largest and smallest observations give the threshold
bracket
$$
 \max(0,y_* -\max d_i)\le t\le y_*,
 \qquad y_*=s\left(\frac{a-1}{b+1}\right)^{1/c}.
$$
For constants the bracket collapses to \(t=y_*\). Signed bisection
of the normalized score suffices, with the width checked in public
threshold coordinates.

## Fixed shapes and free scale

**Proved.** For \(a<1\) the fixed-scale spike takes precedence.
For \(a\ge1\), whenever \(s,t>0\), or at the endpoint by continuity,

$$
 L(t,s)\le C^n s^E g(t/s)^k\prod_{d_i>0}d_i^{-1-b}.
$$

Each positive-gap factor satisfies \(f(y)<C s^b y^{-1-b}\) and
\(y\ge d_i\), while each minimum factor equals \(C g(t/s)/s\).
The function \(g\) is bounded, with unique maximum at \(r_*=0\)
for \(a=1\) or \(r_*^c=(a-1)/(b+1)\) for \(a>1\).
The path \(t=rs\) for fixed \(r>0\) makes the inequality
asymptotically an equality with \(g(r)\) in place of its maximum.

- \(E<0\) yields divergence. This includes every constant sample.
- \(E=0\) yields the exact unattained supremum
  \(C^n g(r_*)^k\prod_{d_i>0}d_i^{-1-b}\).
  Use \(t=r_*s\), or \(t=0\) when \(r_*=0\).
  Equality forces \(k<n\), so the upper bound is strict at every
  finite point. The laws collapse weakly to the point mass at the
  sample minimum and the limiting product includes positive gaps.
  This gives an `Unattained` diagnostic with the supremum.
- \(E>0\) forces uniform decay as \(s\downarrow0\).
  Decay as \(s\to\infty\) is uniform by boundedness of \(g\).
  For \(t\to\infty\), use the uniform bound
  \(f(y)\le C\sup_{r>0}[r g(r)]/y\).
  A positive upper level set is thus compact in \((t,s)\).
  Continuity on the closed threshold domain gives a finite maximum.

With \(a=1\), monotonicity in \(t\) at each scale puts the maximum at
\(m\). In the Pareto II cases this recovers the existing held-minimum
Lomax endpoint dispatch, including the exact equality face. With
\(a>1\) the endpoint has zero likelihood, so an attained maximum has
\(t>0\).

**Proved profile certificate.** For fixed positive \(t\), the
log-scale score is \(c[(p+q)\sum_i u_i-np]\), strictly decreasing
from \(ncq\) to \(-ncp\). Solve for the unique root. An explicit
bracket for \(s\) uses the smallest and largest \(y_i\) multiplied by
\((q/p)^{1/c}\). At \(t=0\) with \(a=1\), use the zero-aware
scale score. Its left limit is positive exactly when \(E>0\),
and a strictly positive bracket can be obtained by shrinking scale
until that sign is verified.

The derivative of the normalized profile score still has the covariance
term above. In addition,
$$
 u_i'=c u_i(1-u_i)(w_i-\bar w_v),\qquad
 \bar w_v=\frac{\sum_i v_i w_i}{\sum_i v_i},\qquad v_i=u_i(1-u_i).
$$
This follows by differentiating the scale equation \(\sum_i u_i=np/(p+q)\).
Consequently \(\sum_i w_i u_i'=c\sum_i v_i(w_i-\bar w_v)^2>0\)
for nonconstant data. Hence \(\langle H'\rangle_w<0\) and the
normalized profile score strictly decreases. This gives strict
unimodality for the interior fit and a certificate from nested signed
brackets. For \(a>1,E>0\), existence guarantees a positive score
somewhere near zero, and the score is negative for sufficiently large
\(t\) because \(u_i\to p/(p+q)\) as the excess ratios converge
uniformly to one. The positive root can be bracketed by shrinking
and expanding a data-based gap. The optimized scale increases in \(t\)
by the derivative formula, so scale roots at threshold bracket ends
also enclose the final scale.

## Fixed scale and free upper-tail shape

**Proved.** This covers the held-scale Pareto II mask and the
Pareto IV and Feller–Pareto masks with \(c,p,s\) fixed. If \(a<1\)
the ordinary threshold spike applies. For any constant sample,
take \(t\downarrow0\), \(q=(s/t)^c\).
The identities \(B(p,q)\sim\Gamma(p)q^{-p}\) and
\((1+1/q)^{-q}\to e^{-1}\) give
\(f(t)\sim c e^{-1}/(\Gamma(p)t)\).

For nonconstant data define
$$
 A(t)=n^{-1}\sum_i\log(1+(y_i/s)^c),\qquad
 \psi(p+q(t))-\psi(q(t))=A(t).
$$
The left side decreases strictly from infinity to zero, so the
fixed-threshold log likelihood has a unique shape root. This is
the held-shape beta equation on the GB2 coordinate. For \(p=1\)
it is \(q=1/A(t)\). In particular the held-scale Pareto II result
is the zero-aware Lomax shape formula at \(t=0\).
The root \(q(t)\) decreases in \(t\), and \(q(0)\) is finite
because a positive gap makes \(A(0)>0\).

If \(a=1\), the profile is strictly decreasing and the optimum
is at \(t=0\), with shape \(q(0)\). If \(a>1\), the profile tends to
zero in likelihood at \(t=0\), since \(q(t)\to q(0)\).
For \(t\ge s(a-1)^{1/c}\), every \(H_i<0\) by setting \(q=0\)
in its upper bound. Thus a finite interior maximum exists.
The score is positive for sufficiently small \(t>0\), uniformly
using \(q(t)\le q(0)\). An explicit lower bound for the score is

$$
 \frac{k}{t}
 \left[a-1-(p+q(0))c(t/s)^c\right]
 -(1+cq(0))\sum_{d_i>0}d_i^{-1}.
$$

Halve a prospective left bound until the bracketed expression is
positive and the whole lower bound is positive. Both inequalities
then hold for all smaller positive \(t\). The remaining compact
interval contains every optimum.

**Proved failure of unimodality.** Use two observations at \(0\)
and twenty at \(41/100\), with \(p=s=1\), \(c=48\) and \(q\) free.
The exact profile score at the following rational gaps falls in these
intervals.

| Gap \(t\) | Score interval |
|---|---|
| \(1/2\) | \((55,56)\) |
| \(14/25\) | \((-32,-31)\) |
| \(9/10\) | \((26,27)\) |
| \(99/100\) | \((-6,-5)\) |

These are rational bounds. For each \(y=t+d_i\), bound
\(\log(1+y^{48})\) by reducing the argument by powers of two into
\([1,2]\) and applying the same series to \(\log 2\).
With \(z=(x-1)/(x+1)\), sum the first 28 terms of
\(2\sum_{j\ge0}z^{2j+1}/(2j+1)\). The residual lies between zero and
\(2z^{57}/[57(1-z^2)]\) by a geometric bound. Inserting the log
bounds in \(q=22/\sum_i\log(1+y_i^{48})\) and then in the score,
which decreases in \(q\) at fixed \(t\), gives the displayed
inequalities by exact rational operations in [check.py](2026-11-25-thresh-math-data/check.py).
The two disjoint positive-to-negative brackets each contain a local
maximum by continuity and the signs at their endpoints.

**Global certificate.** For \(a>1\), use the exterior sign bounds first.
On a remaining interval \([l,r]\), enclose the shape by
\([q(r),q(l)]\). Bound the score and its derivative on the interval.
Discard intervals with sign exclusion, isolate roots with signed
brackets and strict curvature where available, and compare their
likelihood bounds. A likelihood upper bound for subdivision follows
directly from the explicit log density and the enclosed ranges of
\(t,q\). The score is analytic on the compact interior interval
by the implicit function theorem and has finitely many roots there.
A separated winner among simple roots can be certified by this method
with sufficiently fine subdivision. Finiteness alone gives no budget
bound for near-ties or multiple roots.

## Beta with both endpoints free

**Proved.** If a shape is free or below one, place the corresponding
endpoint near its sample extreme with the other endpoint strictly
beyond the data. Choose a shape below one if it is free. The original
endpoint spike argument applies. With both shapes held at least one
and constant data, shrink the interval symmetrically about the common
observation. Its beta coordinate stays at \(1/2\) and the reciprocal
width diverges.

For nonconstant data and fixed shapes \(a,b\ge1\), the [P0 existence
argument](2026-10-29-p0-genbeta.md#nsbeta) admits a concave certificate.
Let \(u_i=A+B(x_i-m)\), where \(B>0\) is inverse width and
\(0\le u_i\le1\). Up to an additive constant the log likelihood is
$$
 n\log B+(a-1)\sum_i\log u_i+(b-1)\sum_i\log(1-u_i).
$$
Use zero contribution for a zero exponent at a closed endpoint.
The constraints are affine, \(B\) is at most the reciprocal sample
range, and positive upper level sets bound \(B\) away from zero
by the \(n\log B\) term. Thus the maximum is attained. Concavity
is strict when either shape exceeds one: the first term is strict
in \(B\), and any remaining log term is strict in \(A\) at fixed
\(B\). If both shapes are one, maximizing \(B\) pins both endpoints
at the extremes. If \(a=1\) and \(b>1\), decreasing \(A\) at fixed
\(B\) improves the objective until the lower bound touches the
minimum. The reflected argument applies when \(b=1<a\).
A shape above one forbids contact since that density would be zero.

Either threshold can be profiled. Likelihood upper level sets are
convex in \((A,B)\) and their image under either endpoint map is an
interval. In fact each endpoint is a constant plus a ratio of an
affine function by \(B\), which on a segment interpolates between
the endpoint values with positive weights. Strict concavity gives
strict quasiconcavity of the profile except in the uniform case,
where the explicit profile is monotone towards the relevant extreme.
The cheaper certificate is the joint concave problem's score and
constraint conditions. With one shape equal to one, set that
endpoint directly and use the scalar fixed-shape width solve below.

## Beta with one endpoint held

Use reflection for the two orientations. Let \(z_i\) be the
nonnegative distance from the held endpoint towards the data, \(Z=\max z_i\),
and \(W\ge Z\) the fitted width with \(W>0\). The shape at the free
endpoint is \(a\), and that at the held endpoint is \(b\). Thus with
`min` held, \(a\) is `shape2`, and with `max` held it is `shape1`.
The CSV uses these rules before the strict-support rows.

**Held-endpoint rules, proved.**

- A negative \(z_i\) violates support for every width.
- If any \(z_i=0\), a fixed \(b<1\) or a free \(b\) allows an
  infinite held-endpoint density. Take \(W>Z\) and, if needed,
  choose \(b<1\). Every other factor is positive.
- If any \(z_i=0\) and held \(b>1\), the held-endpoint factor is
  zero for every width. The strictly containing free-endpoint
  likelihood and its limit at contact are zero. If the other
  endpoint also has an infinite factor at exact contact, the
  pointwise product there is undefined. The fitter can stop for
  lack of positive support, without interpreting that product as
  a spike.
- If held \(b=1\) and \(Z=0\), density equals \(a/W\) at every
  observation, giving unboundedness. If held \(b=1\) and \(Z>0\),
  retain the zeros in the fixed-\(b\) calculation below.

**Proved, fixed shape at the held endpoint.** For \(Z>0\) and
positive held-side likelihood, free \(a\) or held \(a<1\) gives
the free-endpoint spike by the earlier argument.
For fixed \(a\ge1\), the score in \(\log W\) is
$$
 -nb+(a-1)\sum_i z_i/(W-z_i).
$$
For \(a=1\) the width is \(Z\). For \(a>1\) the score decreases
strictly from infinity to \(-nb\), so there is a unique interior
maximum, found by signed bisection. A right bound is
\(W=Z(1+(a-1)/b)\) since every ratio is bounded by \(Z/(W-Z)\).
These formulas include positive constant distances and zeros when
\(b=1\).

**Proved, free shape at the held endpoint.** Here take all \(z_i>0\).
Free \(a\) belongs to a structural spike mask, so suppose \(a\) is
held. If \(a<1\) the same spike applies. For any constant positive
distance \(z\), set \(W=z(1+\varepsilon)\) and \(b=1/\varepsilon\).
The density is asymptotic to
\(e^{-1}/[z\Gamma(a)\varepsilon]\), by the large-\(b\)
beta normalizer, proving divergence even when \(a\ge1\).

For nonconstant distances let \(G=\exp(n^{-1}\sum_i\log z_i)\)
and \(h=\log(W/G)\). The conditional root \(b(h)\) is given
by \(\psi(b+a)-\psi(b)=h\) and strictly decreases in \(h\).
It stays finite at \(W=Z\), since \(G<Z\). For \(a=1\) the
profile score in \(h\) is \(-nb(h)\) and \(b(h)=1/h\),
so the endpoint is attained. For \(a>1\), the profile score per
observation is \(R-b\) with
$$
 R=(a-1)\operatorname{mean}_i r_i,\qquad r_i=z_i/(W-z_i).
$$
It starts positive near \(Z\) and ends negative since \(R=O(e^{-h})\)
and \(b(h)\sim1/h\) by the digamma pole at zero.
At a root, \(R'\le-b-b^2/(a-1)\) by the mean-square inequality.
Also \(b'=-1/D\) where
$$
 D=\psi_1(b)-\psi_1(b+a)
   =\int_0^\infty e^{-bv}\frac{v(1-e^{-av})}{1-e^{-v}}\,dv
   >\frac{a-1}{b(b+a-1)}.
$$
For the inequality, the integrand without \(e^{-bv}\) exceeds
\(1-e^{-(a-1)v}\) at every \(v>0\).
The integral identity also follows by integrating the positive
geometric series for \((1-e^{-v})^{-1}\). Thus \(R'-b'<0\)
at every root. The opposite exterior signs and strictly downward
crossings give exactly one root, and strict unimodality follows.
Nested signed brackets suffice and the monotonicity of \(b(h)\)
encloses the restored shape. Root uniqueness does not require
the profile to be concave away from the root.

## Reproduction

On the workstation the final command
`/tmp/umvl-cpu/run nice -n 10 python3 -u check.py` from the
[data directory](2026-11-25-thresh-math-data/) completed successfully.
The script checks all 39 masks against the enumerated subsets and
source declarations, verifies the Burr score bounds by rational
arithmetic, and runs the numerical checks at 90 decimal digits.
The numerical checks cover three scale-collapse exponents on
\(s=10^{-j}\) for \(j=5,10,20,30\), a 25-point normalized-score
grid \(t=10^{j/4}\) for \(-12\le j\le12\), and the mixed beta
width root for distances \((1,2,5,5)\) and \(a=2.5\).
The latter gives \(W\approx7.16467165641523531\) and
\(b\approx1.93841220040102753\), and the script also checks the
constant-sample path at \(\varepsilon=10^{-30}\). These numerical
checks corroborate the derivations. Only the rational sign checks
are used in the multimodality proof.

## What this changes for the fitter

- Route structural spikes directly to `Unbounded` after validation.
  Pareto IV already does this. For Feller–Pareto, the key dispatch is
  free \(p\) or \(c\), or joint free \(q,s\). Use the beta held-endpoint
  checks before selecting a singularity status. The table assumes no
  new R5 restrictions.
- With all regular Pareto coefficients held, use the exact endpoint or
  one signed score bracket. With scale free and shapes held, dispatch
  on \(a-1\) and \(E\), with a sign margin or exact-product residual near
  equality. Use `Unattained` with the scale-zero log supremum at
  \(E=0,a\ge1\). For \(E>0\), solve at the exact sample minimum if
  \(a=1\), or use nested threshold and scale brackets if \(a>1\).
- With scale held and only the upper-tail shape free, use the
  conditional beta-shape root. The \(a=1\) nonconstant case has a
  certified endpoint fit. The \(a>1\) nonconstant case needs
  global comparison on the bounded interval. A single converged
  stationary point cannot earn a global flag. An accurate successful
  search may be returned marked uncertified if its global certificate
  is unresolved under the fallback rule.
- For beta with both shapes held, solve the concave affine-coordinate
  problem or its scalar reductions. With one shape and the opposite
  endpoint held, use the explicit \(a=1\) endpoint fit or the monotone
  conditional shape and unique width root for \(a>1\). Reject
  incompatible held bounds before fitting. Return attained endpoints
  at the original sample extrema. The five existing density entrypoints
  already evaluate their closed endpoints. Recheck likelihood and
  coefficient accuracy at the restored public coordinates, including
  threshold subtraction and beta width restoration.

## Open

For fixed scale and free upper-tail shape, the compact comparison
settles existence but does not classify exact global ties, degenerate
stationary roots, or the cost of closing their likelihood bounds.
The proved two-mode example rules out a one-root certificate;
a practical solver still needs subdivision and tie handling under its
work budget. The simultaneous zero and infinite beta endpoint factors
have no positive-likelihood fit under the held-endpoint convention
above. Any policy that instead extends the likelihood by perturbing
the held coefficient would require a separate closure definition.
