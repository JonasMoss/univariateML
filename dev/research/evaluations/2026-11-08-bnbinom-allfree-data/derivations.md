# BNB face certificate and finite-cover obligations

The implementation extends the supporting-polynomial proof, rather than
replacing it with a numerical comparison. Write `n[k]` for count frequencies,
`mu=sum(k*n[k])/n`, and use the fitted mean as an exact algebraic ratio.

For Poisson, the directional likelihood is

$$D(y)=e^{\mu(1-y)}\sum_k n_k y^k.$$

Its derivative has the sign of
`P(y)=sum(((k+1)*n[k+1]-mu*n[k])*y^k)`. For a held-size NB face,
`q=mu/(r+mu)` and

$$D(y)=\frac{(1-qy)^r}{(1-q)^r}\sum_k n_k y^k,\quad 0<y<1/q,$$

whose derivative has the sign of
`P(y)=sum(((k+1)*n[k+1]-(k+r)*q*n[k])*y^k)`.
Both have `P(1)=0` exactly. Prefix summation of the coefficients gives
`Q=P/(1-y)`. Strict positivity of Q for nonnegative y proves that D has
its unique maximum at 1, where D=n. Concavity of log then bounds the
likelihood under every mixture of the corresponding face law by its fitted
face likelihood. A finite BNB has a nondegenerate beta mixing law for NB,
and a nondegenerate gamma/beta mixture of Poisson rates. Integration is
strict because D<n off the fitted point. The face is approached by the
usual concentration paths, so it is an unattained supremum.

If Q has degree m, homogenize with `y=z/(1-z)`. The Bernstein coefficients
of `(1-z)^m*Q(z/(1-z))` are `Q[j]/choose(m,j)`. Positive coefficients
suffice for positivity on [0,1]; repeated de Casteljau subdivision proves
positivity for additional polynomials. We require strict endpoint bounds,
including z=1, a sufficient condition stronger than needed. Nothing is
inferred from an unsuccessful proof. In particular, NB proof failure does
not imply Poisson and polynomial-budget exhaustion does not exclude a
face. All new certified face statuses have the complete supporting proof.

Practical margins are recorded in the dossier and source. The proof is exact
arithmetic conditional on computed comparisons with those margins, following
the package standard; it does not claim outward-rounded interval arithmetic.
Polynomial construction, allocation degree and each subdivision coefficient
update are bounded by 128 times min(iterlim,100000). The sparse sign test
remains first. The fallback is reached only if that test fails.

For counts (n0,n1,n3)=(3,10,1), the Poisson quotient is
`(101-29*y+13*y^2)/14`, with discriminant `-4411/196`. For r=1,
the NB quotient is `(231-29*y+52*y^2)/27`, with discriminant
`-47207/729`. These prove positivity independently of code. Exact rational
Bernstein subintervals and 80-digit likelihood roots are recorded by oracle.py.

# Uniform inner profiles

The alpha-held certificate extends to a specified positive alpha interval.
At a fixed size/beta point, both coordinate scores increase with alpha:

$$\partial_a S_r=\psi_1(a+r)-E\psi_1(a+r+b+X)>0,$$
$$\partial_a S_b=\psi_1(a+b)-E\psi_1(a+r+b+X)>0.$$

The stationary rectangle is uniform: use the usual small-shape mass bound,
and resolve a negative size score at `(U,alpha_high,lower)` for the upper
cutoff. A finite witness lower bound combines its zero mass at alpha_low
with its count-ratio denominators at alpha_high. Monotonicity of these
factors makes that a lower bound throughout the slab.

A center-alpha certified global fit proposes a canonical root neighborhood.
Trigamma bounds enclose its Hessian uniformly in all three coordinates.
Endpoint score intervals and the same fixed preconditioner verify a single
contraction for every alpha in the slab; uniform negative definiteness makes
the enclosed root a strict conditional maximum. A full stationary rectangle
cover then excludes every box outside that neighborhood and its swap,
uniformly in alpha. Thus every conditional global maximizer throughout the
slab is enclosed. Other branches, unresolved seams and overly wide slabs
stop. Endpoint fits alone would not establish this continuum statement.
The finite proposal must also lie in the certified branch or its reflection;
a proof about another conditional mode cannot validate that proposal.

# Compactness for the full finite problem

The [joint derivations](../2026-11-08-bnbinom-joint-data/derivations.md)
prove the uniform exterior bounds used here. A finite witness lower bound L
strictly above upper bounds F_NB and F_Pois for the globally fitted faces
makes its superlevel compact. In brief:

1. The bound `p_x <= min(1,(r)_x/x!)` excludes r near zero and, by symmetry,
   beta near zero.
2. With `m=r*beta/(alpha+r+beta)`,
   `log p_x <= -m+log(m)_x-log(x!)`. Its likelihood bound decreases for
   m above the sample mean, giving an effective cutoff m<M.
3. With `lambda=r*beta/alpha`, the uniform Poisson error is at most
   `(n*lambda^2+sum(n_x*x*(x-1))/2)*(1/r+1/beta)`. If both interchangeable
   shapes exceed U>=4M, lambda<=2M, so a strict Poisson gap bounds their
   minimum above. Order r<=beta, giving r in [lower,U].
4. Stochastic monotonicity in NB size and Jensen give
   `p_x <= ((delta+1)_x/x!)*p^delta`, where delta=min(lower,1) and
   p=alpha/(alpha+beta). The positive-mass bound
   `P(X>0)<=max(U,1)*(1-p)` closes both ratio corners.
5. The uniform NB remainder C/(alpha+beta), with r replaced by U and the
   ratio cutoffs inserted, excludes alpha+beta>=S when L>F_NB.

Both large-shape cutoffs reserve a factor-two strict likelihood gap.
The resulting canonical box contains every global maximizer of a nonempty
compact superlevel, so a global maximum exists and is interior in the
original positive parameter space.

# NB face height enclosure

The delegated NB certificate brackets its unique size root within relative
5e-7 of its returned size. We use rho=1e-6 and re-evaluate its mean-profile
likelihood with stable log1p expressions and the gamma-increment helper.
This avoids treating a rounded success probability near one as an exact
profile optimum. For empirical mean mu, the profile curvature is

$$\ell''(r)/n=-E\sum_{k<X}(r+k)^{-2}+\mu/[r(r+\mu)],$$

so its absolute value is at most 2*mu/r^2. Taylor's theorem about the true
stationary size root bounds the height difference by
`n*mu*(rho/(1-rho))^2`, in addition to the evaluated likelihood margin.
The full finite certificate uses that upper bound for its strict NB-face gap.

# Complete stationary cover and coefficient accuracy

The cover preserves boxes crossing r=beta and omits only boxes strictly
outside the canonical half. It applies the following proved exclusions:

- The exterior mass and ratio bounds above, including the NB remainder made
  local to each covering box.
- Monotone corner score signs and scalar single crossing. The alpha mode
  increases with r and beta, so coarse signed corner brackets enclose it
  uniformly without requiring tiny coefficient tolerances at huge endpoints.
- The monotone-factor likelihood upper bound, using zero mass at alpha_high
  and count denominators at alpha_low.
- A preconditioned interval Newton enclosure of every stationary point.

For the last step let S be the average score, H its derivative, c a box
center, and M any fixed matrix. Every stationary point p in the box obeys

$$p=c-MS(c)+(I-M\bar H)(p-c),$$

where bar-H averages H on the segment between c and p. The segment is
inside the original box. Bounding H by decreasing trigamma therefore
bounds the right side for every stationary point. An empty intersection
excludes the box. Otherwise intersecting coordinates shrinks the possible
stationary set. Sequential coordinate intersections remain valid: all
surviving roots are in the smaller set, while the original Hessian bounds
still cover their segments to c. Computing M from an approximate inverse
does not affect this identity; it changes only how useful the enclosure is.

Newton proposes a finite center; its convergence is not an acceptance rule.
The cover accepts only boxes contained in the center's coordinate window
of relative radius min(tol,1e-8)/4. The other boxes must all be excluded.
Since a global maximizer exists and is stationary, closure puts every
canonical global maximizer in that window, and every other labeled global
maximizer in its reflection. The returned center is therefore within the
requested relative tolerance of a global maximizer in every coefficient.
Neither uniqueness nor a negative-definite three-dimensional Hessian is
assumed. An unfinished cover, unresolved arithmetic or inability to represent
the coefficient window supplies no certified coefficients. The public fitter
retains a successful legacy search result as uncertified in those cases.

The all-free finite route also attempts the uniform inner profile enclosure
about its refined alpha. Both swap-related witnesses and the diagonal
witness remain inspectable. The binding-neutral success result exposes
`global_loglik_interval`, `global_coefficient_lower/upper`, and the alpha
profile interval and mode envelope. The existing stop-detail upper bound
of zero is a valid universal bound; the added global interval is tighter.

# Floating-point margins and work

This is certified in exact arithmetic conditional on computed comparisons
with stated practical margins, not outward-rounded interval arithmetic.
The relevant margins are:

- Scores: 128 double eps times the absolute digamma-term sum.
- Alpha-interval Hessians: 512 eps times accumulated absolute terms; its
  witness lower bound uses 1e-9*n times the likelihood and zero-mass magnitudes.
- Full Hessians: 1024 eps times accumulated absolute terms.
- Preconditioned Hessian products: 64 eps times absolute product sums.
- Newton mapping: 4096 eps times one plus accumulated absolute mapping terms.
- Exterior likelihood comparisons: 1e-10*n*(1+abs(average likelihood)) plus
  1024 eps times absolute terms. Face bounds additionally use
  1e-9*n*(1+abs(average face likelihood)). The NB remainder has its existing
  512-eps inflation.

Cutoff tests and full-cover boxes share 128*min(iterlim,100000) evaluations.
Each coarse alpha bracket and Newton proposal has min(iterlim,100000) solver
steps. The center-alpha certificate and uniform alpha-slab cover each retain
their separately documented certificate allowance. Optional supporting
polynomial proofs have their own bounded coefficient-update allowance.
A failed legacy search returns no estimate. An unfinished certificate retains
the successful legacy search result, explicitly uncertified.

The root-margin probe computes the contribution of the current componentwise
score margins after preconditioning at the frozen candidate. Its largest
relative radius contribution is about 1.82e-10. This supports using a
2.5e-9 target radius at tol=1e-8; much tighter requested tolerances may stop
numerically. This is a limitation of the computed enclosures, not a theorem
that sharper scores or other methods cannot certify tighter coefficients.

# Nonuniqueness and remaining face mathematics

On the frozen sample the completed finite cover puts all canonical global
maxima in a small box near (1.0672234393,2.1419792203,5.3647342963).
That box and its reflected box are disjoint and neither intersects r=beta.
Existence plus exact symmetry therefore proves at least two distinct labeled
global maximizers. No identified-uniqueness theorem is asserted. The separate
diagonal-only cover closes all 857 boxes, with 22,988 score evaluations and
no unresolved boxes, independently supporting diagonal exclusion. It uses
existing score/mass margins and an additional absolute 1e-6 comparison margin.

An attained witness lower bound survives an unfinished joint cover. If that
bound strictly exceeds a legacy face upper bound, the fallback cannot select
the defeated face. The retained witness may supply a corrective finite result
only after passing the legacy score and range gates; it remains uncertified
until the global coefficient cover closes. A closed joint cover proves the
finite answer independently of whether the additional inner slab closes.

The 80-digit root and diagonal values are independent numerical checks, not
the global argument. The diagonal stationary value is about -1276.2838627,
below the certified finite maximum near -1275.6474864.

The absolute NB remainder C/(alpha+beta) cannot prove a winning face when
the strict witness gap vanishes. A general all-free winning-NB theorem still
needs a signed uniform neighborhood inequality and a complete finite-plus-face
comparison, including ties. Supporting polynomials likewise remain sufficient,
not necessary, for winning Poisson faces. These unresolved all-free cases retain the legacy finite or face comparison,
explicitly uncertified, with face and finite witnesses. A proved strict finite
advantage overrides a legacy face decision. Failed legacy solves still stop.
The family-wide certification grade is partial because these numerical
comparisons do not solve the general winning-face classification.

# Equal frequencies through four: a stronger Poisson theorem

The 0:4 fixture has D(0)=exp(2)>5, so its mixed-Poisson supporting
inequality does not hold. The compound-Poisson class supplies a different
global bound, preserving its Poisson result and the unchanged shared JS assertion.

A finite BNB is Poisson mixed by independent gamma variables
`G_r*G_beta/G_alpha`. Gamma densities are hyperbolically completely monotone
(HCM); products and ratios preserve this property, and an HCM density is a
generalized gamma convolution (GGC). These are Proposition 1 and Proposition 3
of [Behme and Bondesson, arXiv:1507.04017](https://arxiv.org/abs/1507.04017).
Their GGC Laplace representation gives the mixed-Poisson generating function

$$\log G(z)=c(z-1)+\sum_{k\ge1}(z^k-1)\int\frac{U(dt)}{k(1+t)^k}.$$

The total jump intensity is finite because `G(0)>0`. Thus every BNB law is
compound Poisson. This use of the cited results requires no shape restriction.

For equal positive frequencies at 0,...,4 it suffices to use one observation
at each value; replication scales the likelihood. Removing compound-Poisson
jumps larger than four increases every observed mass by the same factor.
Write the remaining jump intensities as A,u,v,w. A=0 gives zero likelihood
because the sample contains one. Tilting the intensities to
`(A*t,u*t^2,v*t^3,w*t^4)` maximizes this ray exactly when
`A+2u+3v+4w=2`. The derivative in log t is five times the difference
between the sample mean and this sum, so every law has a ray representative
on that simplex with likelihood at least as large.

Let B_j be the coefficient of z^j in `exp(A*z+u*z^2+v*z^3+w*z^4)`:

$$B_1=A,\quad B_2=A^2/2+u,\quad B_3=A^3/6+Au+v,$$
$$B_4=A^4/24+A^2u/2+u^2/2+Av+w.$$

Put `P=B1*B2*B3*B4` and substitute `A=2-2u-3v-4w`. Along the dispersion
ray `(u,v,w)->t*(u,v,w)` with A adjusted to preserve the mean,

$$t\ell'(t)P=N=5(u+2v+3w)P+(A-2)P_A+uP_u+vP_v+wP_w.$$

The exact rational checker constructs Q=-N, of degree 11. Set
`u=t1, v=2*t2/3, w=t3/2`, and homogenize with
`S=t0+t1+t2+t3`. On the simplex S=1, t_j>=0, multiplying this homogeneous
polynomial by S^4 leaves Q unchanged. The resulting degree-15 polynomial
has 759 positive coefficients and 57 zero coefficients. There are no
negative coefficients. The successive negative counts during degree
elevation are 21,11,4,1,0. This proves Q>=0 on the entire simplex in exact
arithmetic; it is not a mesh search.

The linear terms of Q are `(40*v+112*w)/9`. On the pure-u ray its first
nonzero term is `40*u^3/9`. Therefore every nonzero dispersion ray has a
strictly negative likelihood derivative near the Poisson endpoint and a
nonpositive derivative everywhere else. Poisson(2) is consequently the
unique maximizing compound-Poisson law. No finite BNB equals Poisson
because its mixing law is nondegenerate. The all-free Poisson supremum is
unattained and approached along the usual concentration path.

`compound_poisson_uniform_four_face` recognizes exactly equal positive
frequencies at 0,...,4 and is wired into the all-free face decision. Its
comparison is an exact integer-frequency identity; no computed polynomial
sign or floating likelihood comparison is used at runtime. The algebra is
reproduced by compound-poisson-proof.py, with its coefficient fingerprint
and exact-arithmetic summary in compound-poisson.json. Samples outside this
sufficient condition continue through the other certificates or the marked
legacy fallback.

