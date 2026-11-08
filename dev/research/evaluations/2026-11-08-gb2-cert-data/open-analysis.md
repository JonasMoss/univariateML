# Outer-profile audit

Public GB2 order in this tree is `(q,a,p,b)`, not `(p,a,q,b)`:
`shape1=q`, `shape2=a`, `shape3=p`. All sets below use mathematical names.

Let `w_i=log(x_i/b)`, `S_-(a)=sum softplus(-a*w_i)`,
`S_+(a)=sum softplus(a*w_i)`. With b held,

    L = n log(a) - n log B(p,q) - p S_-(a) - q S_+(a) - sum log x_i.

For q held the p score is strictly decreasing:

    L_p/n = psi(p+q)-psi(p)-S_-(a)/n,
    L_pp/n = trigamma(p+q)-trigamma(p) < 0.

It has a unique root for every finite a>0. Thus {p,a} is a scalar profile,
not a reason to run an unconstrained two-coordinate BFGS. The profile
curvature is `L_aa - L_ap^2/L_pp`; the positive second term prevents an
inference of concavity from the separate concavity of L in p or a.
Reciprocation supplies the {q,a} problem.

When all observations are below held b, a->infinity and p=r/a yields

    L -> n log(r) + r sum log(x_i/b) - sum log(x_i).

This is the fixed-upper-endpoint power likelihood. Observations exactly at
b add `-q log(2)` per tied observation. The reciprocal path gives the
fixed-endpoint Pareto face. Therefore an interior rectangle cannot be
certified by merely noticing decreasing likelihood on its sampled boundary.
A signed, uniform remainder on this optimizing path is still needed to
select the face, plus a coefficient enclosure and all-root isolation for
the finite profile. The existing Burr certificate has these only for the
exact unit opposite shape; near-unit estimated values cannot use it.

With a held and p,q,b all free, the transformation is precisely the *joint*
three-free-parameter beta-prime problem. The conditional beta-prime theorem
used here supplies no compact bound uniform in its held shape. Its gamma
and inverse-gamma neighborhoods meet a large-both-shapes lognormal corner.
The parent joint fit remains numerical, so delegation does not resolve
{p,q,b}.

For {p,q,a} with held b, small-power/large-shape concentration must be
excluded quantitatively, while the large-power small-shape paths can meet
power, Pareto and double-Pareto faces depending on the signs of w_i. For
{p,a,b} and {q,a,b}, allowing b to move adds transformed-gamma and endpoint
paths to the one-held-shape problem. Their simultaneous p,a,b (or q,a,b)
scalings cannot be covered by a remainder uniform only in a fixed a. The
all-free set contains every preceding obstruction and the coupled
large-shape lognormal corner.

These are missing certificate components, not proofs of nonexistence and
not structural `holds:` exceptions. Existing default fits and their goldens
are preserved by instruction. No global certificate is claimed for their
legacy score-gated numerical search. A future implementation must either
provide these bounds or stop without claiming a maximizer.
