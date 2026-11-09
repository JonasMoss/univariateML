# Mixed-power certification and remaining joint problems

Public GB2 order is (q,a,p,b); a is the power. The completed held-scale
{p,a} and {q,a} certificates are in shape_power_certificate.hpp. Their
canonical proof, coefficient precision and arithmetic allowances are in the
GB2 dossier. Finite constant-data regimes are included. The curvature search
in curvature.py / curvature.json is numerical evidence against profile
concavity, not evidence of multiple global maxima. The algorithm does not
assume concavity or unimodality of the outer power profile.

With w_i=log(x_i/b), S_minus=sum softplus(-a*w_i), the unique inner p root
satisfies psi(p+q)-psi(p)=S_minus/n. Its profile curvature is
L_aa-L_ap^2/L_pp and may be positive. Nonetheless L_aa<0 and L_pp<0 and
L_ap=sum w_i/(1+exp(a*w_i)) decreases in a. This gives the rectangle bounds
and all-stationary-point exclusion used in the new global search.

Two exact inequalities close the exteriors. H(p)=log(p*B(p,q)) is nonnegative
for q<=1, and H(p)>=-C*p for q>1, C=psi(q)-psi(1), by convexity. The
Gamma-ratio bound B(p,q)>=Gamma(q)/(p+q)^q follows by integrating
psi(t)<log(t). For crossing observations these give a linear large-power
likelihood upper bound. For all upper-side observations the bound is
n log(a)-q*a*D+K, D=sum(log(x_i)-min(log x))>0. All lower-side observations
have the power supremum with the pointwise endpoint penalty; the sign of
n*C-Z*log(2) decides domination versus a finite point above the face.
Small power is excluded uniformly by shape concavity and S_minus/n>=log(2)/2
on a sufficiently small interval. Thus neither an unchecked cap nor a
sampled boundary is used as evidence for a global estimate.

# Five remaining subsets

- {p,q,b}, a held: the joint beta-prime certificate is the separate
  betapr-joint job. No beta-prime files or interfaces were changed here.
  At fixed power there is no nondegenerate lognormal corner (the old
  gb2-cert report's assertion is superseded by the current job brief).
  Transport needs compatible joint global and coefficient enclosures,
  including control of scale error after raising it to 1/a.
- {p,q,a}, b held: both beta shapes can diverge as power tends to zero.
  With target lognormal location m and sd s, set K=2/(a*a*s*s),
  p=K*exp(a*(m-log b)/2), q=K*exp(-a*(m-log b)/2). Then the mean and
  variance of log X tend to m and s*s. A finite-n likelihood remainder
  uniform over this corner, plus large-power small-shape and exponential-
  shape paths, is still needed before a finite global search can close.
- {p,a,b}, q held: the new fixed-b profile is not a certificate over b.
  Its inverse-transformed-gamma face and its intersections with varying
  power need uniform signed comparisons, followed by a global two-coordinate
  search. Uniform small-power exclusion and eventual moving-power-endpoint
  domination are proved below; they do not close the gamma face.
- {q,a,b}, p held: reciprocal version of the preceding missing bounds;
  the transformed-gamma face replaces inverse transformed gamma.
- {p,q,a,b}: combines the preceding corners, including the genuine varying-
  power lognormal corner and moving endpoints. Complete compactness/face
  coverage and certified finite-region exclusion remain missing.

The exponential-shape paths in {p,q,a} really can move an effective
endpoint while b is held. For any 0<theta<b and r>0, take a->infinity,

    p=r/a, q=(b/theta)^a*exp(-(log a)^2).

Then q->infinity and q*(x/b)^a=exp(-(log a)^2)*(x/theta)^a.
The gamma ratio Gamma(q+p)/(Gamma(q)*q^p) tends to one, uniformly for
bounded p, and a/Gamma(p)->r. Substitution in the GB2 density gives the
power density r*x^(r-1)/theta^r at every fixed x<=theta, including the
endpoint, and zero at fixed x>theta. This is also a weak power limit:
write X=b*(G_p/G_q)^(1/a) with independent unit-scale gamma variables;
-p*log(G_p) tends to an exponential variable and G_q/q tends to one.
The factor exp(-(log a)^2) moves the finite transition just above theta
and removes the endpoint penalty without changing the weak limit.
Reciprocation gives a Pareto endpoint theta>b through
q=r/a and p=(theta/b)^a*exp(-(log a)^2). These explicit paths are not
uniform finite-n face comparison bounds. In particular, the fixed-q
large-power threshold below is not uniform in a varying exponential q.

These are algorithmic/mathematical open entries, not structural holds or
claims of nonexistence. The five existing numerical searches, including the
free GB2 default, are unchanged and not newly certified. Feller-Pareto with
held min inherits all statements on positive excesses.

# Useful bounds for a future held-q, free-scale certificate

These derivations are not used to claim the larger subset is certified.
They identify which exterior work is settled and which signed comparison
is still missing.

For fixed q, the maximum log-ratio density over p and location at fixed a
is bounded by a*q^q*exp(-q)/Gamma(q). Indeed its mode height divided by a
is p^p*q^q/((p+q)^(p+q)*B(p,q)); its derivative in p is
[log(p)-psi(p)]-[log(p+q)-psi(p+q)]>0 and its limit is the stated gamma
height. Thus n*log(a)+n*(q*log(q)-q-logGamma(q))-sum(log x) is a uniform
small-power likelihood bound, independent of free scale and free p.

For nonconstant data write y_i=log x_i, y_min=min y_i, y_max=max y_i,
S0=n*y_max-sum y_i>0 and D=sum y_i-n*y_min>0. Put y=log b,
Cplus=max(0,psi(q)-psi(1)). For y>=y_max the H bound gives the free-endpoint
power supremum plus a remainder O(1/a), uniformly in y. If
0<=d=y_max-y<=S0/(4*n), S_left>=S0-n*d and S_right>=d. For sufficiently
large a (a>=8*n*Cplus/S0 and a>=2*n*n/(q*S0)),

    L <= L_power + 2*n*n*Cplus/(a*S0).

For y<=y_max-S0/(4*n), split according to U=sum softplus(a*(y-y_i)).
If U>=n, maximizing the gamma-ratio bound over p+q>=q puts its maximum
at q and gives

    L <= n log(a) + K1 - q*a*S0/(4*n).

If U<n, a*(y-y_min)<n, and log(1+t)>=t/(1+t) gives
U>=exp(a*(y-y_min))/(1+exp(n)). The same gamma-ratio bound gives

    L <= n log(a) + K2 - q*a*D.

Both constants are finite and independent of y. Consequently the global
large-power profile tends to the freely fitted power supremum. It is
attainable by p=r/a and b=x_max*exp((log a)^2/a).

The retained maximum-observation terms strengthen this statement to
**eventual domination by the freely fitted power face for every fixed
q>0**, not only q<=1. Here is an exact-arithmetic proof; it is not yet a
free-scale fitting algorithm. Let k>=1 be the multiplicity of y_max,
C=Cplus, B=n/(a*S0), and let L_power=n*log(n/S0)-n-sum(y_i).
Using H(p)>=-C*p and r=a*p, write

    L <= n log(r) - r*(S_left-n*C/a) - q*S_plus - sum(y_i),

where S_left=sum((y-y_i)_+) and
S_plus=sum softplus(a*(y_i-y)). The omitted residual multiplying p is
nonnegative. For y>=y_max put u=a*(y-y_max)>=0. Maximizing r gives

    L-L_power <= -n*log(1+B*(u-C)) - q*k*log(1+exp(-u)).

The denominator is positive when a>n*C/S0. If u>=C, the right side is
strictly negative. If 0<=u<C, it is at most

    -n*log(1-B*C) - q*k*log(1+exp(-C)).

For C>0 this is strictly negative whenever

    a > n*C / (S0*(1-exp(-q*k*log(1+exp(-C))/n))).

For C=0 the u>=C case already covers the whole upper half-line.

For y_max-S0/(4*n)<=y<=y_max put v=a*(y_max-y)>=0. Then
S_left>=S0-n*v/a and S_plus>=k*log(1+exp(v)). If a>=4*n*C/S0,
B*(v+C)<=1/2 and therefore

    L-L_power <= -n*log(1-B*(v+C)) - q*k*log(1+exp(v))
              <= (2*n*B-q*k/2)*v + 2*n*B*C-q*k*log(2).

The last inequality uses -log(1-t)<=2*t on [0,1/2] and the supporting
tangent log(1+exp(v))>=log(2)+v/2. Hence this whole neighbourhood is
strictly below the face for all sufficiently large a; for example take

    a >= max(4*n*C/S0, 4*n*n/(q*k*S0),
             4*n*n*C/(q*k*S0*log(2))).

The bound is then at most -q*k*log(2)/2. The farther lower half-line is
covered by the preceding two linear-exponential bounds. Explicit constants
there may be taken as

    K1=n*q*log(q)-n*logGamma(q)-sum(y_i),
    K2=n*q*(log(n*q)-1+log(1+exp(n)))-n*logGamma(q)-sum(y_i).

Both n*log(a)-q*a*S0/(4*n)+K1 and n*log(a)-q*a*D+K2 tend to minus
infinity and are decreasing after their respective scalar turning points.
Thus a finite, data-dependent threshold excludes the full large-power
exterior uniformly in free scale. This closes the unsigned-remainder gap
above without assuming a profile shape or using a sampled boundary.
The missing free-scale pieces are the signed uniform gamma-face comparison
below, its joint-power seams, and the certified finite two-coordinate search.

At b->0, with a bounded away from zero and infinity, the inverse-
transformed-gamma face has scale b*p^(1/a). At its fitted scale, write
T_i=p*(b/x_i)^a, sum T_i=n*q. The first likelihood correction is

    (sum T_i^2 - n*q*(q+1))/(2*p).

This is an asymptotic calculation, not an implemented uniform remainder.
The missing comparison requires a signed remainder uniform in the
likelihood-relevant power neighbourhood, especially if this coefficient
vanishes. A fixed-a beta-prime comparison does not supply that uniformity.
The finite two-coordinate search also remains to be implemented.

Even the zero-first-correction seam needs profiling care. In regular face
coordinates theta, with T_i=(gamma/x_i)^a and t=1/p, the formal coefficients
of L=L0+t*A+t*t*B+... are the A above and

    B = -n*q*(2*q*q-3*q+1)/12 + q*sum(T_i^2)/2 - sum(T_i^3)/3.

If the face Hessian H at its maximum is nonsingular, profiling theta adds
-0.5*grad(A)'*H^{-1}*grad(A) to the second coefficient B. This contribution
is nonnegative when H is negative definite. Comparing B at fixed face
coefficients would omit it. These Taylor coefficients follow from the
log-gamma ratio and log(1+T/p) expansions; they are not uniform remainder
bounds or implemented certificates. Controlling the remainder and this
profile displacement, including zero first/second coefficients, is part
of the free-scale blocker.
