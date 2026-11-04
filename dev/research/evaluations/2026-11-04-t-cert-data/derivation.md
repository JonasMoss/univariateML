# Location-scale Student t: certificate ingredients

These are exact-arithmetic arguments for finite real samples. Floating-point
experiments are separate. Write ordinary scale as `s`; the `std` standard
deviation is `s sqrt(nu/(nu-2))`. Let `P(nu)=max_(mu,s) ell(nu,mu,s)` when
this maximum exists. All likelihoods here are **total** log likelihoods.

## Fixed shape is a geodesically convex problem

Set `a=s sqrt(nu)` and `D_i=(x_i-mu)^2+a^2`. Then

```
ell = n c(nu) + n nu log(a) - (nu+1)/2 sum log(D_i),
c(nu) = log Gamma((nu+1)/2) - log Gamma(nu/2) - log(sqrt(pi)).
```

On the upper half-plane with metric `(dmu^2+da^2)/a^2`, put
`b_i=log(D_i/a)` and `b_inf=-log(a)`. Direct covariant differentiation gives
`Hess b = g - db tensor db` and `||db||_g=1` for each of these functions.
Consequently

```
-ell + n c(nu) = (nu+1)/2 sum b_i + n(nu-1)/2 b_inf
```

is geodesically convex for `nu>=1`. For `nu>1` it is strictly convex
when the sample has at least two distinct values: the null directions of
`Hess b_inf` are vertical, and all `Hess b_i` can share that null direction
only if every observation equals the current location. For `nu=1`, three
distinct boundary points give strict convexity; two support points permit a
flat geodesic. Existence still requires the mass condition below. This is
not Euclidean convexity in `(mu,log(s))` and does not assert shape convexity.
A stationary point, when it exists, is therefore globally optimal at fixed
shape. A finite score tolerance alone does not locate that exact point.

## Uniform nuisance containment

The location score puts every attained nuisance maximum in `[min x,max x]`.
Let `R=max x-min x`, and choose an integer `k` with `1<=k<n`. Define
`d_k = min_i(x_(i+k)-x_(i))/2` for sorted observations. At most `k`
observations can have distance strictly less than `d_k` from any location.
The scale score and the remaining `n-k` observations imply

```
n = (nu+1) sum r_i^2/(nu s^2+r_i^2),
s^2 >= d_k^2 ((n-k)/n-k/(n nu)).
```

Also `s^2 <= (1+1/nu) mean(r_i^2) <= (1+1/nu) R^2`.
For all `nu>=v`, use the maximum positive lower bound over `k`, evaluated
at `v`, and upper bound `U=R sqrt(1+1/v)`. If the largest atom satisfies
`k_max < v(n-k_max)`, some lower bound is strictly positive. This works
for arbitrarily close distinct observations without choosing a search cap.
The boxes contain maximizers, rather than all likelihood superlevel sets.

The fixed-shape objective tends to minus infinity at every boundary when
all atom masses are strictly less than `nu/(nu+1)`: at a sample atom the
scale exponent is `nu(n-k)-k>0`; at other boundary points the exponent is
positive too; at infinite height or horizontal infinity the density vanishes.
Equivalently the positive Busemann weights have no boundary atom of half
their total mass or more, including the infinity atom. This proves attainment
and, with the strict convexity above, uniqueness in the strict-mass regime.

For a proposal `p=(mu_p,a_p)`, let `e=||grad(-ell)(p)||_g`. Convexity along
the geodesic to any maximizer `q` proves `ell(q)<=ell(p)+e dist(p,q)`.
Hyperbolic distance satisfies

```
cosh dist(p,q) = 1 + ((mu_p-mu_q)^2+(a_p-a_q)^2)/(2 a_p a_q).
```

Its maximum over the containment rectangle occurs at a corner: for fixed
height it is convex in location, and for fixed location it is a positive
linear combination of height and reciprocal height. Thus the driver has an
endpoint profile upper bound without treating solver convergence as proof.
The corresponding gradient norm in `(mu,log(s))` is
`sqrt(nu s^2 g_mu^2+g_log_s^2)`.

## A global envelope on every finite shape interval

For fixed `(mu,a)`, `ell-n c(nu)` is affine in `nu`. Its supremum is convex.
For `lo<=nu<=hi`, with linear interpolation weights `w,1-w`,

```
P(nu) <= w P(lo)+(1-w) P(hi)
         + n [c(nu)-w c(lo)-(1-w)c(hi)].
```

One need not solve for the worst interpolation gap. The digamma integral
(after substituting `t=2u`) gives

```
c'(nu) = integral_0^infinity exp(-nu u)/(1+exp(-u)) du,
0 < -c''(nu) < 1/nu^2.
```

The interpolation remainder therefore bounds the bracketed term by
`(hi-lo)^2/(8 lo^2)`. This envelope covers every nuisance maximum and every
shape mode. The sampled grid only proposes useful subdivision points.
It cannot certify a coefficient vector by itself: a near-optimal value need
not imply closeness to one selected root, and a maximizer neighborhood
requires a root/curvature certificate or coordinate exclusion.

## Unsigned bound on the entire normal tail

For `nu>=N>=2`, use the containment lower scale `L` valid for all `nu>=2`.
Cauchy--Schwarz for a Gamma variable gives
`Gamma(z+1/2)/Gamma(z)<=sqrt(z)`; hence the ordinary t normalizer is at
most `-log(2 pi)/2`. The inequality `log(1+u)>=u-u^2/2` for `u>=0`
gives, at every attained nuisance maximum,

```
ell_nu(mu,s) <= ell_normal(mu,s) + (1+1/N) sum z_i^4/(4nu)
P(nu) <= P_normal + C/nu,
C = 3 n (R/L)^4/8.
```

This proves uniform profile convergence to the fitted normal likelihood.
If a finite feasible likelihood exceeds the normal by `g>0`, taking
`N>2C/g` excludes the entire tail relative to that feasible likelihood.
`N` is a proved exclusion threshold, not a fitted df cap. The bound is loose
and cannot prove that the normal wins when no strictly better finite witness
exists. The conservative signed bound in [signed-tail.md](signed-tail.md)
settles a terminal interval when A is negative. It has not been integrated
into a numerically resolved whole-profile normal-winner certificate.

For stable evaluation at large nu, the same integral implies

```
-1/(4nu) <= c(nu)-log(nu)/2+log(2pi)/2
           <= -1/(4nu)+1/(24nu^3).
```

Indeed the derivative of the middle expression is the Laplace transform of
`tanh(u/2)/2`; integrating in nu gives minus the transform of
`tanh(u/2)/(2u)`. Use `u/2-u^3/24 <= tanh(u/2) <= u/2`, valid for all
nonnegative u (differentiate the lower inequality). The driver includes
this width in its computed likelihood allowance.

## Normal-seam expansion, including nuisance refitting

Let `xi=1/nu`, and let `z_i=(x_i-mean(x))/s_N`, where `s_N^2` has divisor
`n`. Put `m_j=mean(z^j)`. For arbitrary nonconstant samples use a sufficiently large lower shape bound
with positive containment; the driver uses two only in its strict-mass
workloads. The unique normal nuisance root has negative
likelihood Hessian `diag(n,2n)` in coordinates `(mu/s_N,log(s))`.
The implicit-function theorem supplies a smooth local nuisance root, and
uniform containment and uniform convergence force every global nuisance
maximizer onto that root for sufficiently small xi. Taylor expansion yields

```
P(1/xi) = P_normal + A xi + B xi^2 + O(xi^3),
A = n (m4-3)/4,
B = n [m4/4-m6/6+m3^2/2+(1-m4)^2/4].
```

Before profiling, the xi and xi-squared terms are
`sum(z^4-2z^2-1)/4` and `sum(z^4/4-z^6/6)`.
The gradient of the first term in normal nuisance coordinates is
`(-n m3,n(1-m4))`. The Schur correction is
`n m3^2/2+n(1-m4)^2/4`, proving the expression above. This is an asymptotic
expansion, not an implemented finite-interval remainder. A positive A proves
that some finite member beats the normal; a negative A proves only that the
normal wins locally. When A vanishes the sign of B can decide locally, but
neither coefficient establishes a global normal winner.

## Collapse and the lower face

At an atom of multiplicity k, ordinary scale collapse at fixed nu has
likelihood proportional to `s^(nu(n-k)-k)`. Thus all-free lst is unbounded
if `k>n/2`; all-free std is unbounded if `k>2n/3` by choosing a finite
nu just above two with negative exponent. Under strict inequalities in the
other direction, the preceding compact profile arguments apply down to the
lower face. For lst, nu=1 is an attained family member; for std, nu=2 is
excluded and its positive-scale profile is a genuine unattained t2 limit
(the reported sd would diverge).

At equality `k=q(n-k)`, where q is 1 or 2, a further scale-zero face is
possible. Its total log likelihood at atom b is

```
C_b = n log[Gamma((q+1)/2)/(Gamma(q/2) sqrt(q pi))]
      + (q+1)(n-k) log(q)/2 - (q+1) sum_(x_i!=b) log|x_i-b|.
```

Along mixed paths `(nu-q)|log(s)| -> t`, the extra factor is
`exp(-(n-k)t)<=1`. Center offsets of order s add a factor at most one;
larger offsets cannot improve this face. In particular equality does not
prove unboundedness or that the face beats every finite fit. The Cauchy
exception with two equal-mass support points has attained maxima as well.
A complete all-free classifier must compare these faces, both shape faces,
and every surviving finite maximum. The research driver explicitly leaves
equality cases open; it does not reinterpret the current early stop as a
mathematical classification.

## Arithmetic and scope

The exact-arithmetic inequalities above are proofs. The executable driver
uses binary64 NumPy/SciPy and heuristic practical error allowances, including
`256 epsilon (n+1)(1+abs(value)+sum(abs(x)))` on values and scores and the
stated gamma expansion interval. The allowances have not been derived for
every floating-point operation, solver, special function and extreme input.
Its value brackets are **research enclosures conditional on these computed
bounds**, not production certificates under the package standard. Independent
high-precision checks assess them on the retained workloads only.

No argument here proves joint uniqueness, general profile unimodality or
coverage of the existing production multistart search. No inference claim is
made. The cited primary comparison is Hasannasab et al. (2020),
[arXiv:1910.06623v2](https://arxiv.org/pdf/1910.06623): its abstract and
Section 3 discuss conditional existence and the normal limit; it does not
supply a certificate for this repository's all-free fitter. The arguments
and bounds in this note are supplied explicitly rather than inferred from
algorithm convergence in that paper.

## An exact counterexample to a global kurtosis decision

Take the twelve exact rational observations
`[-4,-3,-2,-1,1,2,3,4,-101,-99,99,101]/100`. Their fitted-normal
inward slope is exactly `A=-1170351/50160128<0`. At the explicit finite
point `nu=3, mu=0, s=1/3`, with empirical variance v, the log-likelihood
improvement over the fitted normal is

```
Delta = 6 log(24 v/pi)+6-2 sum log(1+3 x_i^2) > 49/100.
```

[exact.py](exact.py) proves the strict inequality using exact rational
arithmetic. Machin's identity encloses pi with alternating arctangent
series. Positive rational logarithms are reduced by powers of two to
`[1,2]`, then enclosed using the positive atanh series with its geometric
remainder. Forty-eight terms give an improvement enclosure narrower than
`1e-35`. No numerical optimizer, floating-point comparison or special-function
implementation is used in this proof. This one finite point belongs to
both all-free models, so the obstruction applies to lst and std alike.

The negative derivative makes the normal strictly locally optimal at the
seam; the explicit point beats it. Continuity therefore forces a profile
minimum between that point and the seam. A unimodal profile assumption, or
normal classification based solely on kurtosis at or below three, is invalid.
The nearby binary64 sample in inputs.json is separately checked with
high-precision arithmetic and with the research enclosure engine.

## Attainment criterion in the strict-mass regime

For lst assume `k_max<n/2`; for std assume `k_max<2n/3`. The nuisance
bounds apply uniformly down to the lower shape q=1 or q=2, respectively.
Fixed-shape strict convexity and attainment imply that P is continuous
there and on every bounded shape interval. The whole-tail bound gives
`P(nu)->P_normal` at infinity. Consequently:

- For lst, a finite global MLE exists if and only if some finite point
  with `nu>=1` reaches or exceeds `P_normal`.
- For std, a finite global MLE exists if and only if some finite point
  with `nu>2` reaches or exceeds `max(P_normal,P(2))`.

To prove sufficiency with strict improvement, remove neighborhoods of both
excluded ends in which P is lower than that feasible value, then maximize
on the remaining compact shape interval. For equality, either a strictly
higher point exists and the same argument applies, or the feasible equal
point is itself a global maximum. Necessity follows by taking the endpoint
limits. The attained Cauchy member stays in lst's admissible compact set;
it must not be treated as an excluded limit. The positive-scale t2 maximum
is excluded in std even though its value is well defined. Equality cannot
be discarded merely because one candidate lies on a limit face.

This is a profile-likelihood closure and attainment statement under the
stated strict-mass conditions, not a classification of every weak limit
under arbitrary parameter paths. Equality of atom masses requires the
additional collapse faces and exceptions discussed above; the driver does
not claim to settle those cases.

The exact checker also proves a zero-slope example. In the symmetric family
`[-4e,-3e,-2e,-e,e,2e,3e,4e,-1.01,-.99,.99,1.01]`, let `t=e^2` be the
positive root of `2304 t^2+1440.144 t-.0192=0`. Exact signs bracket this
unique positive root in `[1/100000,1/50000]`. The defining equation gives
`m4=3`, while symmetry gives `m3=0`. Rational bounds on the sixth moment
prove `B=21-2m6>2`. Hence the normal is beaten by finite members even at an
exactly zero first-order seam coefficient. The `seam-A-0.0` binary64 input
is a numerical proxy for this algebraic sample, not an assertion of exact
zero slope for its rounded observations.
