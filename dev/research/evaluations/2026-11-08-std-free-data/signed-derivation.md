# Signed normal faces under actual Student holds

This adds a sufficient normal-face certificate to the scalar certificates in
[derivation.md](derivation.md). The certificate covers a whole infinite tail
and a compact complement. An unresolved certificate preserves the legacy
search and its uncertified finite/limit decision. A failed search remains a
failure; an evaluated candidate strictly beating a proposed face rejects that
face. Existing global scalar and symmetry proofs remain eligible.

## First-order coefficients

Put

    g(v) = log Gamma((v+1)/2) - log Gamma(v/2) - log(v/2)/2.

This is the gamma normalizer correction relative to normal. For ordinary
scale, h=1/v and q=z^2,

    log t_v(z) - log phi(z)
      = g(v) -(v+1)log(1+q/v)/2 + q/2
      = h(q^2-2q-1)/4 + O(h^2).

For std, z uses the held physical standard deviation. Write w=1/(v-2):

    log f_std(z;v) - log phi(z)
      = g(v) + log(v/(v-2))/2 -(v+1)log(1+q/(v-2))/2 + q/2
      = w(q^2-6q+3)/4 + O(w^2).

Since w=1/v+O(v^-2), the first coefficient is unchanged when the expansion
is written in inverse v. Thus at the constrained normal fit,

    c1_lst = sum(z_i^4-2z_i^2-1)/4,
    c1_std = sum(z_i^4-6z_i^2+3)/4.

The normal score in every free nuisance coordinate vanishes. Reoptimizing
those coordinates changes the profile only at second order (the envelope
theorem). With scale free, sum(z_i^2)=n at the conditional normal fit, so
both coefficients are n(b2-3)/4, where b2 is the fourth standardized moment
about the held centre. In this route free sd in std is an unrestricted
ordinary scale at each v>2; the profiles agree under their one-to-one scale
transformation. With scale held and location free, centre at the sample
mean and keep the actual supplied scale; sum(z_i^2) need not equal n.

A zero or rounding-uncertain coefficient makes no claim in this helper.
A negative coefficient alone does not prove a global normal winner: a finite
maximum can be higher on the compact complement.

## Uniform upper remainders

The existing gamma inequalities imply, for every v>0,

    g(v) <= -1/(4v) + 1/(24v^3).

Also log(1+u)>=u-u^2/2 for all u>=0. Consequently, for 0<h<=H,

    ell_lst - ell_normal <= h A(q) + h^2 R_lst,
    A(q) = sum(q_i^2-2q_i-1)/4,
    R_lst = sum(q_i^2)/4 + n H/24.

For std use w, 0<w<=H. The bounds

    -w/[4(1+2w)] <= -w/4+w^2/2,
    log(1+2w)/2 <= w,
    1/v^3 <= w^3

give

    ell_std - ell_normal <= w B(q) + w^2 R_std,
    B(q) = sum(q_i^2-6q_i+3)/4,
    R_std = n/2 + 3 sum(q_i^2)/4 + n H/24.

These upper bounds are uniform in q; they require no maximum residual
truncation. The implementation takes H<=1/8, hence nH/24<=n/192.

## Reoptimization of the free nuisance

Use p=log(s/sN) with location held, and p=(m-mN)/s with physical scale held.
On |p|<=rho, rho=1/20, the constrained Gaussian loss is at least kappa p^2/2:

    kappa = 2n exp(-2rho)  (free scale),
    kappa = n             (free location).

The scale formula follows by twice differentiating
n[p+(exp(-2p)-1)/2]. The location formula is exact because the sample mean
is the normal location fit. Let M bound the absolute derivative of the
first-order coefficient on this same nuisance interval. Its formulas are

    free scale:    A'(p) = sum q_i(p) - sum q_i(p)^2,
    free location: A'(p) = sum r_i(1-r_i^2),
                   B'(p) = sum r_i(3-r_i^2),

where r_i=z_i-p and q_i(p)=z_i^2 exp(-2p) for scale. The implementation
encloses these signed sums, retaining cancellations where possible. For
scale, sum z_i^2=n exactly at the reference Gaussian fit.

Taking R uniformly over the nuisance interval and completing the square,

    profile - LN <= h c1 + h^2 K,
    K = R + M^2/(2 kappa),

with h replaced by w for held sd. Choose H so c1_upper+H K_upper<0.
Every positive inverse shape at most H then lies strictly below normal,
provided every maximizing nuisance coordinate lies in this interval.

That last condition is checked, not assumed. With location held the scale
score

    G(h,p)=sum[(q_i(p)-1)/(1+q_i(p)h)]

is strictly decreasing in p, goes from positive to negative, and has one
root when all residuals are nonzero. Interval endpoint signs at -rho and
rho enclose every root for the entire h interval [0,H]. The exact derivative
is -2 sum[q_i(p)(1+h)/(1+q_i(p)h)^2].

With physical scale held, all stationary locations lie in the sample
range. Choosing H times the squared standardized sample range less than
one makes the likelihood strictly concave in location throughout that
range. The positive location score is

    G(h,p)=sum[(z_i-p)/(1+(z_i-p)^2 h)].

Uniform endpoint signs again enclose its unique root in [-rho,rho]. For
std this h is w, which uses ordinary t scale squared (v-2)s^2. The finite
cutoff is rounded upwards so there is no gap between the covered finite
interval and the proved infinite tail.

## Compact complement and lower faces

With location held and ordinary scale free, the positive scale score root
is unique. Conditional scale roots for every shape in a box are enclosed
by uniform endpoint score signs and interval Newton contraction. Numerical
Newton steps nominate a bracket only. The whole bracket and its root
inclusion are checked before they can exclude a candidate.

With location free, the finite compact cover uses boxes in location and
shape, covering all modes and branch changes. Every box is discarded only
when its likelihood upper bound is below the normal lower bound, or a
strict derivative sign excludes any maximizing point. The lower LST
shape-one face is retained whenever the derivative points toward it;
a negative shape derivative cannot discard that boundary. A positive
shape derivative excludes a lower-face maximum by improvement into the
domain. Every unresolved box is subdivided. Budget or arithmetic failure
returns an unproved result, not a capped fit or a guessed normal face.

For held location in LST, every residual must be nonzero for this helper.
The uniform-in-scale lower-shape bound from derivation.md gives

    sup_s ell(v)-LN <= n log(v)/2 - sum log|z_i| + n/2.

An explicitly checked cutoff excludes the entire v-down-to-zero region.
Std with mean held covers v=2 with its positive-scale ordinary t2 profile;
its boundary cannot be discarded by an inward negative shape score.

For std with sd held and location free, sorted-data window bounds exclude
the moving-location v-down-to-two region uniformly. For a window size j,
with delta=min_i(x_(i+j)-x_i)/4>0 and Q=(delta/s)^2,

    ell(2+w,m) <= C_j+(n-3j/2)log(w),
    C_j=-n log(s)-n log(2pi)/2+n log(3)/2
        -3(n-j)log(Q)/2,

for w<=min(1,Q). Only a strictly positive exponent and a checked,
representable cutoff below LN are accepted. Critical or unresolved lower
faces leave the legacy decision uncertified. All lower-face inequalities
are proved in derivation.md; none is inferred from the sign of c1.

If any finite value were at least LN, coercivity and the tail exclusions
would supply a finite global maximum on the compact complement (including
eligible lower faces). Its free scores would vanish, with the appropriate
one-sided condition on an attained lower shape face. Complete box coverage
excludes exactly those possibilities. This supplies the global conclusion;
the signed tail by itself supplies only a local face result.

## Arithmetic, work and evidence

All comparisons use the same conditional exact-arithmetic standard and
practical 64-epsilon interval operations/reduction margins as the existing
Student certificates. Outward rounding is not claimed. Work is metered
by 128*min(iterlim,100000) certificate evaluations. Inner nomination,
bracketing and contraction loops have the solver cap min(iterlim,100000).
An unsuccessful proof never changes a legacy result merely because its
certificate ran out of budget.

`signed-bounds.py` checks the uniform value inequalities and selected
reoptimized-scale envelopes at 80 digits; those checks are numerical,
not the proof of uniform validity. Native regressions check normal-face
closures and the zero-coefficient no-claim rule. The seeded coverage study
records every fit and every input, with explicit separate counts for
uncertified finite/face decisions and actual solver stops.

## Stable scaled compact scores

Direct interval products v^2 c'(v) and v^2 R(q/v) introduce dependencies
that grow with the relative width of the shape box. They are combined
before evaluation. The gamma inequalities imply

    1/4-1/(8v^2) <= v^2 c'(v) <= 1/4.

For held sd set a=(v-2)/v in [0,1]. Then

    (v-2)^2[c'(v)-1/(v(v-2))]
      = a^2/4-a+a^2[v^2 c'(v)-1/4].

The polynomial a^2/4-a decreases on [0,1]; endpoint evaluation and the
gamma remainder bound enclose the whole expression. These elementary
bounds are intersected with the existing gamma enclosures.

For R(u)=log(1+u)-u/(1+u),

    T(u)=R(u)/u^2=integral_0^1 t/(1+ut)^2 dt

is decreasing, including T(0)=1/2. Therefore
v^2 R(q/v)=q^2 T(q/v) (and the same identity with v-2) can be enclosed
without multiplying an independent v^2 interval by R. Existing stable
R evaluations supply endpoint bounds. At tiny u the exact global bounds
1/2-2u/3<=T(u)<=1/2 avoid underflow. A proved parent scale-root interval
also remains valid on every child shape interval; interval Newton
contracts that inherited interval without repeating the nomination.

The held-sd lower-face bound tries geometric window sizes and the
one-third, one-half and largest strict-atom window sizes. Each tried
window yields a valid sufficient bound. Exhaustive optimization of the
window size is unnecessary for validity; an unclosed bound leaves the
legacy result available. This avoids quadratic preprocessing in sample
size.

## Negative leading coefficients with a finite winner

These concrete examples prevent using the signed tail as a global face
selection by itself:

- LST: held mu=0, data consisting of three copies of each of -.001 and
  .001 and two copies of each of -1 and 1. The normal-fit coefficient is
  negative, but the finite point nu=.2, sigma=.001 beats normal.
- Std: held sd=sqrt(7/20), data with 26 zeros and seven copies of each of
  -1 and 1. The normal centre is zero and c1=-10/7. The finite point
  mean=0, nu=3, with the same held sd, beats normal.

`signed-bounds.py` records 80-digit likelihood gains, and native interval
regressions require the normal proof to refuse both examples. The std
example is also detected by a direct finite witness before a potentially
loose lower-face bound is expanded. Compact midpoint witnesses can
similarly disprove a proposed normal face. Such witnesses never supply
unchecked maximizing coefficients.

A strictly positive first coefficient, when enclosed away from zero,
also rules out normal by the first-order limit: sufficiently large finite
shape has higher likelihood at the normal nuisance coordinates (with the
allowed scale transformation when sd is free). This is a rejection of
that face, not a proof of a particular finite maximizer. Zero/uncertain
signs leave the legacy decision uncertified. Unjustifiable sub-epsilon
coefficient precision remains a numerical refusal when no scalar root
inclusion can establish it; uncertified fallback does not certify that
precision.

## Closing inherited brackets and scalar root enclosures

A scale-root bracket inherited from a broad shape box may be much wider
than the roots at a child shape interval. Uniform monotone score signs at
its midpoint, and at quarter points when the midpoint score overlaps zero,
shrink the bracket without losing a root for any shape in that child.
Interval Newton then uses the smaller bracket. Each additional score
point is charged to the certificate budget. Numerical nomination never
supplies root inclusion by itself.

Scalar mean-value root inclusion refreshes its curvature lower magnitude
on the current included interval. The original region still proves root
uniqueness; the refreshed derivative bound only strengthens the error
radius on a subset of that region. Scalar score terms are accumulated in
units of r^2 and curvature terms in units of r^3, with r=v or v-2. The
positive scale factor is divided out after reduction. This prevents the
absolute pairwise-reduction margin from overwhelming a small derivative
before the root-error calculation. No shared arithmetic header changes.

For small u the held-shape remainder also uses t=u/(1+u):

    R(u)=sum_{k=2}^18 t^k/k + remainder,
    0 <= remainder <= t^19/[19(1-t)].

For u<=1/8, t<=1/9. The positive polynomial has no cancellation and uses
the existing practical operation margin; this is a fixed algebraic
formula with an explicit remainder, not a capped iterative solve. The
monotone R(u)/u^2 endpoint enclosure is unchanged. The independent
80-digit checks include this remainder inequality. Flat scalar roots
whose coefficient interval still cannot reach tol retain the explicitly
uncertified legacy result; precision margins are not weakened.

## Occupied held locations in the free-scale helper

The current free-scale normal helper requires a strictly positive enclosure
for every squared residual in both parameterizations. In LST, an occupied
held location with free scale and shape already proves unboundedness and
is handled before this helper. In std, a subcritical occupied held location
can still admit a unique conditional scale maximum: the limiting scale
score is v(n-k)-k, positive uniformly for v>=2 when k<2(n-k).
Extending the checked root containment and lower-face integration to those
occupied std holds remains open. This is a restriction of the sufficient
certificate, not a claim of nonexistence; unresolved legacy decisions keep
their uncertified flag. Shape-only std collapse proofs use their separate
actual-held-sd analysis and are unaffected by this precondition.
