# Symmetric conditional sstd certificates

For xi held at exactly one the family is the variance-standardized Student t.
This is an equality of the entire likelihood, rather than an approximation
near symmetry. Hence the existing std all-free certificate transfers to
sstd with xi held at one, without an objective or coordinate Jacobian.
Finite candidate vectors gain a final coefficient xi=1; normal, ordinary-scale
t2 and collapse-path candidates retain their own model's coordinates.

## Held mean and shape, free scale, xi=1

Write d_i=x_i-mu, m=nu-2>0, l=log(sd), and k=#{d_i=0}.
The total log likelihood, apart from its fixed normalizer, is

    L(l) = -n*l - (nu+1)/2 * sum log(1+d_i^2*exp(-2*l)/m).

Its score is

    S(l) = -n + (nu+1)*sum d_i^2/(m*exp(2*l)+d_i^2).

Every nonzero-distance summand has strictly negative derivative. Therefore
S is strictly decreasing if there is a nonzero distance. Its limits are
nu*(n-k)-k at minus infinity and -n at plus infinity. If the former is
positive, exactly one root exists, L increases before it and decreases
after it, and that root is the unique global maximum. If it is negative,
L tends to positive infinity at scale zero. If it is zero, S is strictly
negative at every finite scale: no positive-scale maximizer exists, and

    sup L = n*C(nu) + (nu+1)*(n-k)/2*log(m)
            - (nu+1)*sum_{d_i!=0} log|d_i|,
    C(nu) = logGamma((nu+1)/2)-logGamma(nu/2)-log(pi*m)/2.

When all distances are zero, L is unbounded at scale zero.

For a positive score limit put p=n-k and

    offset = (log((nu*p-k)/n)-log(m))/2.

At l=log|d_i|+offset, the i-th nonzero score contribution equals n/p.
Consequently min(log|d_i|)+offset and max(log|d_i|)+offset bracket the root.
No guessed or capped search window is needed. Bisection stops on bracket
width, not on a small score. Logarithmic arithmetic avoids residual-square
overflow. The implementation checks representability, a practical score
margin 128*epsilon*n*(nu+1), and a final log-scale width
2e-12*(1+max(abs(endpoints))). A sign-uncertain midpoint only proposes a
narrower bracket; both new endpoint signs must pass the margin. Otherwise
it stops numerically. This is conditional exact-arithmetic certification
with practical floating-point comparisons, not outward-rounded intervals.
The local solve permits min(iterlim,100000) iterations; exhaustion stops.

## Location uniqueness fails with held scale and shapes

For observations -a,+a, xi=1, held sd=s and held nu, put c=(nu-2)*s^2.
Maximizing the likelihood over mu is equivalent to minimizing

    P(mu) = (c+(a-mu)^2)*(c+(a+mu)^2)
          = mu^4 + 2*(c-a^2)*mu^2 + (c+a^2)^2.

If a^2>c the two global maxima are mu=+-sqrt(a^2-c), with identical
likelihood, and mu=0 is a strict local minimum of likelihood. If a^2<=c,
mu=0 is the unique global maximum (quartic flatness at equality).
For a=2, nu=3, s=1 the maxima are +-sqrt(3).
This disproves uniqueness for every supported free subset. It says nothing
about uniqueness of a generic all-free skew-t fit.

## Why this does not close general skew fits

With xi != 1 held, the mean differs from the mode. The scale score now
contains shifted residuals A*d_i*exp(-l)+B, B != 0. The strictly decreasing
score proof above no longer applies. Holding public mean or sd while xi
or nu varies imposes curved constraints in the two-piece mode/side-scale
coordinates; the unconstrained std certificate cannot be reused there.
A complete certificate needs bounds over those constrained profiles,
all stationary branches and all coupled shape/skew/scale escapes. A finite
scan plus a score check supplies none of these exclusions. In particular,
existing nu=2, infinite-nu and half-t comparisons alone do not prove that
all joint escapes have been covered.

The two-observation analytic path is implemented for arbitrary translation:
center=(x1+x2)/2 and a=|x2-x1|/(2*sd); the discriminant is a^2-(nu-2).
The code avoids overflowing a same-sign midpoint and stops when the
standardized discriminant is nonfinite. Its practical comparison margin is
128*epsilon*(a^2+nu-2). A discriminant inside that margin, including represented zero, is a
numerical stop: a rounded ratio must not fabricate an exact quartic seam.
A positive discriminant produces an exact mathematical tie, with both finite
witnesses and finite global likelihood bounds using the documented 1e-10
relative likelihood margin. This is not a claim of generic uniqueness.

The collapse exponent comparison uses fma(nu,n-k,-k), rather than a rounded
product followed by subtraction. Neighboring representable df values above
and below the critical nu=3 case are tested separately; a strictly positive
but numerically difficult root may stop numerically, never as exact equality.
The scale path uses the established stable standardized-t normalizer from
special/skewed_densities.hpp without modifying that header. Equality-stop
likelihood bounds add 128*epsilon*(1+sum(abs(likelihood terms))+n) around the computed
supremum. The normalizer's existing numerical contract is inherited.

A reproducible numerical counterexample to scale-profile unimodality is in
[skew-scale.json](skew-scale.json), generated by [skew_scale.py](skew_scale.py)
with Python Random seed 11611, trial 8. With held mean=0, nu=3 and xi=10,
the six stored observations give scale-score roots at log(sd) approximately
3.76466556, 4.85013938 and 5.89476524, with maximum/minimum/maximum sign
changes. This is numerical evidence of multiple local maxima, rather than
an exact proof of a global tie. It concretely rules out using the symmetric
monotonicity argument or a single unimodal scalar refinement for general skew.
The two tied locations are evaluated inward from the ordered observations:
`inward=sd*c/(a+sqrt(a*a-c))`, with c=nu-2 in standardized coordinates.
This rationalizes `a-sqrt(a*a-c)` and avoids catastrophic cancellation in
`center-sd*sqrt(a*a-c)`. The {1,1e100}, held-sd=1, nu=3 case is a
regression check: a direct midpoint subtraction would return zero instead
of the mode near one, with an appreciable likelihood error.
The unique-location path additionally checks that the represented midpoint's
two residuals, in held-sd units, sum to zero within
128*epsilon*(1+abs(d0)+abs(d1)). Otherwise it stops numerically. For
{1,nextafter(1,2)}, sd=1e-20, nu=1e10, the exact midpoint lies between two
representable locations and rounding it produces a materially different
likelihood. A finite rounded coefficient is insufficient to certify this case.
