# Ordinary-coordinate bounds for the all-free t certificate

These strengthen the [native certificate](../2026-11-06-t-port-data/arithmetic.md)
and [root argument](../2026-11-05-t-root-data/derivation.md). They concern the
total negative log likelihood for optimization, without an inference claim.
All statements below are exact-arithmetic statements conditional on the
computed enclosures. Implementation retains the native binary64 practical
margins, not outward-rounded interval arithmetic.

Let F be the total negative likelihood, m the location, t=log(s) the ordinary
log scale, v the shape, z_i=(x_i-m)/s, u_i=z_i^2/v and D_i=1+u_i. Write
b(v)=c'(v)-1/(2v), with c as in the linked native note. The ordinary scores are

```
F_m = -(1+1/v) sum z_i/(s D_i),
F_t = sum [1-(1+1/v) z_i^2/D_i],
F_v = -n b(v) + sum [log1p(u_i)-u_i/D_i-u_i/(v D_i)]/2.
```

The Hessian entries, before multiplying its shape row/column by the fixed
coordinate scale v0, are

```
F_mm = (1+1/v) sum (1-u_i)/(s^2 D_i^2),
F_mt = 2(1+1/v) sum z_i/(s D_i^2),
F_tt = 2(1+1/v) sum z_i^2/D_i^2,
F_mv = sum z_i(1-z_i^2)/(s v^2 D_i^2),
F_tv = sum z_i^2(1-z_i^2)/(v^2 D_i^2),
F_vv = n [-b'(v)] + sum z_i^2(2+u_i-z_i^2)/(2 v^3 D_i^2).
```

This combines cancelling terms analytically before enclosure. In particular
neither the shape score nor F_vv subtracts O(1/v) or O(1/v^2) normal terms
numerically. The positive completely monotone function -b'(v) has expansion

```
1/(2v^3)-1/(2v^5)+3/(2v^7)-17/(2v^9),
absolute remainder <= 155/(2v^11).
```

The integrated alternating remainder is the same one already used for -c'';
removing its exact 1/(2v^2) term leaves this remainder unchanged. Its two-unit
recurrence adds (3v^2+6v+2)/(v^2(v+1)^2(v+2)^2). Monotonicity permits interval
endpoint evaluation. The native recurrence shifts to v>=32 as before.

A nuisance box of radius r in each coordinate contains the Euclidean ball
of radius r. If its nuisance Hessian is uniformly >= lambda I, lambda>0,
and the central nuisance score norm G is <lambda r, the radial derivative is
strictly outward on that ball. A minimum is interior. Geodesic uniqueness
from the existing t-profile theorem identifies it with the global conditional
minimum. Integrating strong convexity gives the certified optimization gap
G^2/(2lambda). The implementation takes the minimum of this bound and the
existing hyperbolic score/distance bound. If the ball check fails, the old
bound remains available; optimizer convergence does not accept anything.

The finite-root box is still certified in the original (m,log(s sqrt(v)),v/v0)
coordinates. Its entire interval transform into ordinary coordinates must be
strictly contained in the new uniform neighborhood. Nuisance inward bounds
hold for every shape there. A positive full ordinary Hessian implies strict
convexity of the profiled negative likelihood (positive Schur complement),
so its enclosed stationary root is the only profile maximum in that shape
neighborhood. This avoids the severe interval cancellation of the original
coordinate Hessian near normality. Shape neighborhood trials start at relative
radius .1 instead of .01; all original smaller trials remain. At nomination
shapes at least 100, two initial trials use absolute shape radii .1 and .03
(divided by v0 for the relative coordinate), avoiding expensive unsuccessful
coarse trials near normality. Full Hessian positivity is checked before the
central nuisance score pass, so rejected Hessian boxes need fewer passes. No general
unimodality is assumed outside the verified neighborhood.

For an exterior segment [a,b], a verified uniform nuisance ball contains every
conditional optimum. Let M bound F_vv above throughout its nuisance box and
shape interval. For f(v)=min_(m,t) F, its second derivative is
F_vv-F_vn F_nn^{-1} F_nv <= F_vv <= M. Consequently the profile likelihood
l=-f satisfies l''>=-M, and

```
l(v) <= linear interpolation of l(a),l(b) + max(0,M)*(b-a)^2/8.
```

The native code uses shape coordinate v/v0 and divides its Hessian entry by
v0^2. It takes the minimum with the existing globally valid n/a^2 chord
allowance. Failed uniform-ball checks fall back to the old bound. The refined
bound is attempted only for relative segment widths below .1, using endpoint
nuisance midpoints and two bounded radius trials. These are proposals for a
proof box; only the verified inward and Hessian bounds authorize its use.

For the normal tail, use exact fitted-normal coordinates (empirical mean zero,
variance one), k=1/v, and define

```
A=(sum z_i^4-3n)/4,
G=hypot(sum |z_i(1-z_i^2)|,sum |z_i^2(1-z_i^2)|).
```

For 0<=k<=h the existing signed-tail nuisance ball supplies a uniform lambda
and the strict inward check hG<lambda r. At the normal point the nuisance
score norm is at most kG, by the exact mean/variance identities. Also
log1p(u)>=u-u^2/2 and the integrated alternating gamma bound give

```
l(normal nuisance,v)-l(normal MLE) <= A k + (sum z_i^4) k^2/4 + n k^3/24.
```

Adding the nuisance optimization gap proves the uniform tail envelope

```
l(profile,v)-l(normal MLE) <= max(0,A) h + K h^2,
K=sum z_i^4/4 + n h/24 + G^2/(2lambda),  v>=1/h.
```

A strict comparison with the feasible finite witness excludes the whole tail.
The existing signed-tail theorem still returns its sharper zero bound when
applicable. The old containment C/v tail is retained as fallback. There is no
fitted shape cap or increase in the 4096-profile default work budget.

The new ball/Hessian checks add bounded O(n) passes per candidate. Their
purpose is fewer global subdivisions and much shorter normal tails; for
samples where they fail, they add work but cannot change acceptance. Explicit
small budgets still stop with the structured Budget status, witnesses and an
infinite unknown global upper bound. Coefficient width gates, endpoint
certificates, shape-domain guards and held-subset algorithms are unchanged.

`arithmetic.py` compares the ordinary derivatives and new gamma enclosure to
independent 80-digit differentiation, including sampled corners of nonpoint
boxes. `replay.py` preserves prior certified outcomes. These finite numerical
checks corroborate the formulas and practical margins; they do not prove a
floating-point enclosure theorem.
