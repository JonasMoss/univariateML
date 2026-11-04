# A conservative signed normal-tail inequality

This supplements the unsigned tail bound in [derivation.md](derivation.md).
It settles the existence of a signed terminal interval when the first seam
coefficient is strictly negative. It is deliberately conservative; it does
not complete a usable normal-winner certificate in binary64 arithmetic.

Standardize the data to empirical mean zero and variance one. Let `[L,U]`
be the nuisance scale containment for all `nu>=2`, enlarged to include 1,
and let `R=range(x)`, `M=R/L`. Location belongs to the sample range, which
also contains zero. In coordinates `theta=(mu,t=log(s))`, the fitted normal
likelihood loss is

```
D(theta) = n [t + (1+mu^2) exp(-2t)/2 - 1/2].
```

Along the segment from zero to t, `s<=U`, so the second derivative of
`t+exp(-2t)/2-1/2` is at least `2/U^2`. It follows that
`D(theta)>=c ||theta||^2`, with `c=n/(2U^2)`.

The first unprofiled seam coefficient is
`A(theta)=sum(z^4-2z^2-1)/4`. Throughout the containment rectangle,

```
|partial_mu A| <= n (M^3+M)/L,
|partial_t A| <= n (M^4+M^2).
```

Thus `A(theta)<=A(0)+G ||theta||`, where G is the Euclidean norm of the
two right-hand sides. The segment stays in the rectangle, so this is an
ordinary mean-value bound. The normalizer bound proved in derivation.md
and `log(1+u)>=u-u^2/2` give, for `xi<=1/2`,

```
ell_t(theta)-P_normal
 <= -c ||theta||^2 + xi[A(0)+G ||theta||]
      + xi^2 n M^4/4 + n xi^3/24
 <= A(0) xi + K xi^2,
K = G^2/(4c)+n M^4/4+n/48.
```

Every fixed-shape maximizer is contained in the rectangle, so the same
bound holds for `P(1/xi)-P_normal`. If `A(0)<0`, then
`N=max(2,2K/(-A(0)))` guarantees

```
P(nu)-P_normal <= A(0)/(2nu) < 0  for every nu>=N.
```

This theorem does not say that the normal beats smaller nu; the retained
negative-slope cluster counterexamples also satisfy it. The huge N comes
from the global nuisance rectangle and a global Lipschitz bound. It is a
proof that a terminal exclusion exists, rather than an effective seam box.
At A=0 it supplies no sign; the second coefficient in derivation.md or a
higher-order argument is needed. For all-free std, it does not resolve the
t2 face or the scale-collapse equality face.

`signed-tail.py` computes these conservative constants and checks the
threshold using independent 110-digit nuisance roots. Its floating-point
constants have not been wrapped in derived operational error bounds. This
numerical check is corroboration; the inequalities above give the
exact-arithmetic theorem. The main research branch-and-bound does not use
this new inequality: its normal-tail refusals remain explicit refusals.
To make it useful in a default fitter requires a tighter neighborhood near
the normal root and relative profile arithmetic in xi; subtracting full
likelihoods at this threshold cannot resolve the sign with binary64.

The later [nuisance-ball construction](local-tail.md) supplies a much tighter
terminal interval without the global rectangle constants. Its independent
checks are retained separately. Integrating that interval into an exhaustive
normal-winner calculation with propagated arithmetic allowances remains open.
