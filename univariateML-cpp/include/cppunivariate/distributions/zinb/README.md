---
id: zinb
inference_numeric:
  - regime: "negative-binomial member with inflation known"
    available: true
    condition: "finite size > 0 and 0 < prob < 1; pi = 0 held; size and/or prob free"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels"
  - regime: "known point-mass member"
    available: false
    condition: "finite size > 0 and 0 < prob < 1; pi = 1 held; remaining information is zero"
    reason: "The free information is structurally singular; neither observed nor sandwich covariance can be inverted"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels"
  - regime: "primary interior"
    available: true
    condition: "finite size > 0, 0 < prob < 1 and 0 < pi < 1; any regular subset free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Zero-inflated negative binomial
support: {type: integer, lower: 0, upper: inf, closed: [true, false]}
params:
- name: size
  symbol: 'r'
  role: shape
  domain: '[0, inf)'
  simulate: 1
  kind: regular
- name: prob
  symbol: 'p'
  role: probability
  domain: (0, 1]
  simulate: 0.5
  kind: regular
- name: pi
  symbol: '\pi'
  role: probability
  domain: '[0, 1]'
  simulate: 0.5
  kind: regular
r:
  density: dzinb
  sampler: rzinb
  control:
    reltol: "Relative accuracy of every free coefficient (default 1e-10)."
    iterlim: "Work limit per solve: 1 * min(iterlim, 100000) solver iterations; per certificate: 128 * min(iterlim, 100000) certificate evaluations."
    size: "Optional fixed finite nonnegative size, including fractional values; NULL estimates size."
    prob: "Optional held prob; NULL estimates it."
    pi: "Optional held pi; NULL estimates it."
log_density: "Piecewise((log(pi + (1 - pi)*prob**size), Eq(x, 0)), (log(1 - pi) + loggamma(x + size) - loggamma(size) - loggamma(x + 1) + size*log(prob) + x*log(1 - prob), True))"
limits:
  - model: zip
    firmness: proved
    when: 'The size $r$ tends to infinity and $p$ tends to one with $r(1-p)/p$ fixed and positive inflation.'
    detect: 'Size and probability are free; a signed Poisson-tail bound and complete finite-rectangle coverage select the same-inflation ZIP face, or certified positive-count and NB comparisons select the free-inflation face. Unresolved comparisons stop without selecting a limit.'
    status: ZipLimit
    numerical: true
  - model: pois
    firmness: proved
    when: 'The size $r$ tends to infinity and $p$ tends to one with $r(1-p)/p$ fixed, while inflation tends to zero.'
    detect: 'A certified ordinary-NB reduction or complete global comparison selects the zero-inflation Poisson face. An unresolved global comparison is a numerical stop, not a non-existence report.'
    status: PoissonLimit
    numerical: true
references:
- Johnson, N. L., Kemp, A. W. and Kotz, S. (2005). Univariate Discrete Distributions, third edition. Wiley. doi:10.1002/0471715816.
examples: |
  set.seed(1)
  x <- rml(200, mlzinb())
  mlzinb(x)

group: modified counts
construction:
- modification
relations:
  modification_of:
  - family: nbinom
    at:
      size: size
      prob: prob
    type: zero-inflated
    mass: pi
  special_case_of:
  - family: zmnbinom
    at:
      size: size
      prob: prob
      p0: pi+(1-pi)*prob**size
fit:
  method: profile-search
  certified: 'yes'
  note: "certificate.hpp isolates every held-size probability branch and held-probability size branch; global_certificate.hpp covers the joint profile by Taylor bounds, closes its Poisson tail and compares certified NB/ZIP faces. Arithmetic, tie and budget failures stop."
areas:
- counts and ecology
classification: specialist
classification_reason: Used to model overdispersed counts with an additional structural-zero component.
summary: Zero-inflated negative binomial provides a model for overdispersed counts with an additional structural-zero
  component.
aliases:
- "ZINB"
key_references:
- 1
existence:
  uniqueness: unknown
  statement: >-
    With all coefficients free, all-zero data attain a nonunique point-mass
    maximum. For samples containing positive counts, existence depends on
    comparison with the infinite-size zero-inflated Poisson boundary; no general
    uniqueness claim is made. A finite nonnegative size held fixed, or a
    probability held strictly below one, removes that escape and gives an attained
    maximum for compatible data. Holding only inflation below one retains the
    escape, with the same inflation held on the limiting boundary. Further holds
    retain these conclusions. Held size zero, probability one or inflation one
    permits only zeros. Holding all coefficients leaves likelihood evaluation.
  reason: >-
    For positive data, profile probability and any free inflation at each size
    $r>0$, obtaining $Q(r)$. Its infinite-size boundary is the corresponding
    maximized zero-inflated Poisson log likelihood $Q_\infty$. A finite maximum
    exists if some finite $r$ has $Q(r)\ge Q_\infty$; otherwise the supremum is
    attained only on that boundary. The certificate either resolves the required global comparison or stops
    without an estimate. Holding finite positive size makes the probability
    endpoints either attained zero laws or zero-likelihood limits. At held
    $0<p<1$, the likelihood of positive counts vanishes as $r\to\infty$ and the
    zero-size endpoint is attained. These boundary arguments establish attainment
    for those held subsets. For positive data, size held with inflation free,
    and probability held with inflation free, each give a unique maximizer.
    Holding interior inflation can instead give competing maxima; uniqueness
    is false in general even with size also held.
---

The zero-inflated negative binomial distribution adds structural zeros with probability $\pi$ to a negative binomial count. The size $r$ controls dispersion and need not be an integer. The base probability $p$, together with size, determines the count scale.

## d/p/q/r

### Half-inflation probabilities

At size $10^8$, probability one half and inflation one half, the rounded
log-half input places its lower quantile near 99881304. The positive-component
probability is about $2.319\times10^{-17}$; a mixed CDF rounded before
inversion does not retain it.

## Details

For a reduction to an ordinary negative binomial likelihood, the
[NB dossier proof](../nbinom/README.md) supplies
the variance criterion and unique size root. That theorem alone gives no
uniqueness result for an inflated or truncated profile.

Inflation zero is an attained negative binomial member. Size zero, probability one and inflation one each give the point mass at zero. Thus all-zero data attain a maximum, although the coefficients are not identified. Endpoint holds that give zero probability to a positive observation are incompatible.

Holding a finite nonnegative size removes the infinite-size escape. Holding a probability strictly below one also removes it, because increasing size at fixed probability sends mass away from the observed positive counts. Holding only inflation retains comparison against the zero-inflated Poisson boundary with that same inflation.

Free inflation profiles out analytically at each base distribution. The fitter returns only certified estimates. The log-size mesh in `zinb_candidate_mle` only proposes coefficients: it neither isolates all modes nor decides a face winner, and its coefficients are kept only when they lie in the certified enclosure.

The [count-family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-counts.md) gives the global profile characterization and held-subset arguments.

## R notes

A supplied coefficient is held; `NULL` estimates it. Fractional nonnegative size values are allowed. All-zero data with free size return `size = 0`, preserve holds and use `prob = 0.5, pi = 0` for unidentified free coordinates. With positive size held, a free probability is one. If probability is held below one, free inflation is one. These boundary fits are silent.

An infinite-size report stops with advice to use `mlzip`, or `mlpois` when the selected limiting inflation is zero. Numerical search failure also stops.

## Fixed-size branch certificate

The C++ entrypoint `zinb_fixed_size_mle` in `certificate.hpp`
implements the held-size branch of the default `zinb_mle` and the R binding.
It returns either an attained maximizer, an invalid-input status,
or a numerical stop with `ZinbCertificateFailure` and `StopDetail`.

For positive data, let $n_0$ be the zero count, $k=n-n_0$ and
$S=\sum_i x_i$. At held $r>0$ and $0<\pi<1$, put
$a=\pi/(1-\pi)$, $u=kr/(kr+S)$ and $v=nr/(nr+S)$.
The probability score has the sign of

$$
 E(p)=a[kr-(kr+S)p]+p^r[nr-(nr+S)p].
$$

It is positive below $u$ and negative above $v$. On $(u,v)$ its
zeros satisfy

$$
 R(p)=\frac{p^r(nr+S)(v-p)}{(kr+S)(p-u)}=a.
$$

The sign of $R'(p)$ is the sign of
$-rp^2+[r(u+v)-(v-u)]p-ruv$. Splitting at this quadratic's
interior roots therefore covers every monotone branch. The certificate
bisects every positive-to-negative score crossing; negative-to-positive
crossings are minima. A sign-uncertain turning point stops, so a tangency
cannot be silently discarded. Probability endpoints have zero likelihood
when the data contain a positive count. Comparing all isolated maxima
therefore determines the global fixed-size maximum. Two maxima can tie:
99 zeros and one count of ten at size ten have a crossover near
$\pi=0.7683947756875808$. General held-size uniqueness is false.

When inflation is free, its unique maximizer at a base probability is
$\max\{0,(n_0/n-p^r)/(1-p^r)\}$. The positive-count truncated-NB
probability score is strictly decreasing in its natural parameter and its
conditional mean decreases strictly from infinity to one with increasing
$p$. The unconstrained solution is used if feasible; otherwise the ordinary-NB
solution $r/(r+S/n)$ is the constrained maximum. Holding inflation zero,
or observing no zeros, also gives this ordinary-NB probability solution.
Holding probability leaves only the analytic inflation solve or likelihood
evaluation. All-zero fits retain the existing attained representatives.

Computed signs use the practical margin
$4096\epsilon(1+n+S)(1+M\epsilon)$, where $M=\max_i x_i$.
The quadratic discriminant uses $4096\epsilon$ times one plus the
absolute quadratic terms. Uncertain conditional-mean comparisons use
$4096\epsilon(1+\mu+S/k)$. Root brackets retain signed endpoints;
a small score alone is never accepted. Bisection targets the smaller of
`tol` and $10^{-10}$ times $\min(p,1-p)$; arithmetic uncertainty may
leave a wider signed bracket, accepted only below
$10^{-8}\min(p,1-p)$. Each root gets at most `maxit` bisections.
The total likelihood enclosure adds

$$
 (p_U-p_L)\left(\frac{nr}{p_L}+\frac{S}{1-p_U}\right)
 +4096\epsilon(1+n+S)(1+M\epsilon)(1+|\ell|).
$$

This follows by bounding the probability derivative uniformly over inflation;
maximizing over inflation preserves that Lipschitz bound. Overlapping peak
bounds stop as a comparison tie, retaining both witnesses. Recurrence-based
log likelihoods use compensated frequency sums. Counts above 100000,
totals above $2^{52}$, unresolved discriminants/signs and unrepresentable
coordinates stop. These are exact-arithmetic certificates conditional on
computed comparisons with the stated practical floating-point margins,
not outward-rounded intervals or universal libm error bounds.

The joint certificate uses a uniform tail bound with $t=-r\log p$ and
$A=\sum_i x_i(x_i-1)/2$:

$$
 \ell_{\mathrm{ZINB}}(r,e^{-t/r},\pi)
 \le \ell_{\mathrm{ZIP}}(t,\pi)+A/r.
$$

Indeed each positive log mass differs from its Poisson counterpart by
$\sum_{j=0}^{x-1}\log(1+j/r)+x\log[(1-e^{-t/r})/(t/r)]\le x(x-1)/(2r)$;
the zero masses agree. A finite witness strictly above a certified ZIP
upper bound can thus exclude the large-size tail. This inequality alone
cannot certify a winning ZIP face or cover all intervening size branches.
Other exterior regions admit bounds independent of inflation. For $r\le1$,
$(r)_x/x!\le r$ at every positive integer $x$, hence $\ell\le k\log r$.
Also $P(X>0)\le1-e^{-t}\le t$ gives $\ell\le k\log t$.
Once $r\ge r_L>0$ is imposed, the positive masses satisfy

$$
 \ell\le-kt+S\log t+C(r_L),\qquad
 C(r_L)=\sum_{x_i>0}\left[\sum_{j=0}^{x_i-1}\log(1+j/r_L)-\log(x_i!)\right].
$$

This upper bound decreases for $t>S/k$. Together with a strict finite-witness
advantage over the ZIP face, these inequalities reduce the global problem
to a finite rectangle in $(r,t)$. The Taylor certificate below covers that rectangle; a separate signed-tail
certificate can select the face. The retained [validation and numerical limits](../../../../../dev/research/evaluations/2026-11-06-zinb-cert-report.md)
record workload limits, unresolved samples and the reasons for retaining the historical proposal.

## Held-probability size certificate

`zinb_fixed_prob_mle` implements the default branch for free size at held
$0<p<1$. Put $t=-\log p$ and
$H(r)=\sum_{x_i>0}\sum_{j=0}^{x_i-1}(r+j)^{-1}$.
With inflation held, the total size score and its derivative are

$$
 U(r)=H(r)-t[k+n_0q(r)],\qquad
 U'(r)=-\sum_{x_i>0}\sum_{j=0}^{x_i-1}(r+j)^{-2}
       +n_0t^2q(r)(1-q(r)),
 \quad q(r)=\frac{(1-\pi)e^{-rt}}{\pi+(1-\pi)e^{-rt}}.
$$

Every root lies in $[k/(nt),S/(kt)]$: $k/r\le H(r)\le S/r$
and $k\le k+n_0q\le n$. The implementation starts with twice the
bracket width on each side and verifies padded endpoint signs. Both
$H$ and $q$ decrease. Their endpoint bounds enclose $U$ on each
segment. Bounds for $U'$ use the decreasing reciprocal-square sum
and the minimum and maximum of $q(1-q)$ over the posterior-weight
interval, including $1/4$ when that interval contains $1/2$.
Strictly decreasing segments isolate maxima by signed bisection;
strictly increasing segments contain only minima. Other segments are
bisected. This covers the finite bracket without assuming unimodality;
uncertain tangencies exhaust the budget or stop on arithmetic resolution.

With inflation free, the score is ordinary NB until $p^r=n_0/n$
and truncated NB thereafter. Both log likelihoods are strictly concave
in size at held probability, and their scores agree at the join, so the
whole profile is strictly concave and its maximizer is unique. Strict
truncated-NB concavity follows from
$t^2/[4\sinh^2(rt/2)]<1/r^2$. Positive data force zero likelihood
at both size endpoints; there is no Poisson escape with probability held.

Score and derivative comparisons use $4096\epsilon(1+M\epsilon)$
times one plus the absolute terms. The signed size enclosure must have
relative width at most $5\times10^{-8}$; ordinary bisection targets
`min(tol, 1e-10)`. Each root gets at most `maxit` bisections, within
an overall budget of `128 * min(maxit, 100000)` score evaluations.
Likelihood enclosures add the root width times $S/r_L+nt$ plus the
fixed-size arithmetic allowance above. Unresolved peak comparisons retain
all compared witnesses and stop. A free inflation coordinate must also
have an absolute bracket span at most $10^{-8}$. As with the probability certificate,
these bounds are conditional on padded computed comparisons.

## Joint finite certificate

Write $a=\log r$, $b=\log t$, $t=-r\log p$, $u=t/r$, and let $w_j$
count positive observations exceeding $j$. Apart from factorial constants,
the positive log likelihood is

$$
 -kt+S\log t+\sum_{j\ge1}w_j\log(1+j/r)
 +S\log\frac{1-e^{-u}}{u}.
$$

The zero contribution and any inflation penalty depend on $t$ alone. For
free inflation the seam is the horizontal line $t=-\log(n_0/n)$; the
profile is continuously differentiable there. Define

$$
 J(u)=\frac{u}{e^u-1},\quad h(u)=\frac{u}{1-e^{-u}}-1,\quad V(u)=J(u)h(u),
 \quad B=\sum_{j\ge1}\frac{jw_j}{r+j},\quad
 D=\sum_{j\ge1}\frac{rjw_j}{(r+j)^2}.
$$

The gradient and Hessian in $(a,b)$ are

$$
 g=\begin{pmatrix}S(1-J)-B\\ SJ-tW\end{pmatrix},\qquad
 H=\begin{pmatrix}D-SV&SV\\ SV&-tW-SV+C\end{pmatrix}.
$$

For inflation held, $W=k+n_0q(t)$ and $C=n_0t^2q(t)(1-q(t))$,
where $q(t)=(1-\pi)e^{-t}/[\pi+(1-\pi)e^{-t}]$. With inflation free,
the NB branch has $W=n,C=0$, and the inflated branch has
$W=k/(1-e^{-t})$, $C=kK(t)$, $K(t)=t^2e^t/(e^t-1)^2$.

These formulas give rectangular Hessian bounds: $J$ decreases, $h$ increases,
$K$ decreases, and each summand in $D$ has its maximum at $r=j$ and its
minimum at a rectangle endpoint. Posterior weights decrease, and $q(1-q)$
is bounded by its endpoint values and $1/4$ when the weight interval crosses
$1/2$. Union bounds cover rectangles crossing the free-inflation seam.
For half-widths $d_a,d_b$, the Taylor upper bound is the center likelihood
plus $|g_a|d_a+|g_b|d_b$ and
$(M_{aa}d_a^2+2M_{ab}d_ad_b+M_{bb}d_b^2)/2$, with arithmetic allowances.
Every discarded rectangle has an upper bound strictly below the lower
finite witness. The largest remaining upper bound is processed first.

A central rectangle starts at log-coordinate radius 0.02, halved at most
12 times until all its Hessians are negative definite. The determinant
lower bound divided by the maximum negative trace bounds the smallest
eigenvalue of its negative Hessian. The padded gradient norm divided by that bound
then encloses the stationary point. Once every exterior rectangle and
tail is excluded, existence forces the maximum into the central rectangle;
strict concavity there establishes its uniqueness for that sample. The
root errors must be below $2\times10^{-7}$ in log size and log probability,
and in absolute inflation. Output probability must also preserve the
computed log-zero coordinate after rounding. A historical proposal is kept
only within an additional $2\times10^{-7}$ of the certified coordinates
and inflation. Otherwise bounded Newton refinement supplies the proposal.
Neither Newton convergence nor a small likelihood change accepts a fit.

The joint finite and face covers each use at most
`128 * min(iterlim, 100000)` rectangle evaluations. Newton proposals have
at most 50 steps and 30 line-search trials per step. The historical proposal
uses at most 1000 iterations and a tolerance no smaller than $10^{-12}$.
Very weak curvature, unrepresentable coordinates, unresolved signs and
exhausted covers stop. General joint unimodality and completion at any fixed
budget are not assumed.

## Same-inflation ZIP face and signed tail

At held interior inflation, stationary ZIP rates lie between $S/n$ and
$S/k$. Their score is proportional to

$$
 E(t)=\frac{\pi}{1-\pi}(S-kt)+e^{-t}(S-nt).
$$

Equivalently, $e^{-t}(nt-S)/(S-kt)=\pi/(1-\pi)$. Its turning points
solve

$$
 nk t^2-S(n+k)t+S^2+S(n-k)=0.
$$

Thus the same branch isolation used for held-size probability covers every
ZIP rate maximum, including competing modes. Uncertain turning points
stop. With inflation free, a monotone truncated-Poisson conditional-mean
solve, or the attained zero-inflation branch, gives the face. Rate brackets
must have relative width below $10^{-7}$. Face upper bounds include the
root width times $n+S/t_L$; lower bounds are evaluated witnesses with the
arithmetic allowance. All face maxima contribute to its upper bound.

A negative leading tail coefficient alone does not select ZIP. Let
$A=\sum_i x_i(x_i-1)/2$ and let $\lambda_L$ be the lower endpoint of
the selected face-rate bracket. If $A-S\lambda_L/2<0$ with margin, choose
$c$ strictly between $2A/S$ and $\lambda_L$. The exact inequalities

$$
 \ell_{\rm ZINB}(r,e^{-t/r},\pi)
 \le\ell_{\rm ZIP}(t,\pi)+\frac{A-St/2}{r}+\frac{St^2}{8r^2}
$$

follow from $\log(1+x)\le x$ and
$\log[(1-e^{-u})/u]\le-u/2+u^2/8$. The latter follows by bounding the
uniform Laplace transform by the endpoint Bernoulli transform and using
$\log\cosh v\le v^2/2$.
For $t\ge c$ the tail is therefore below the true ZIP maximum once
$r>St^2/[8(St/2-A)]$. On the compact rate interval this ratio has its
maximum at an endpoint. For $t<c$, all other ZIP maxima and the interval
endpoints bound the face likelihood strictly below the selected maximum;
the uniform $A/r$ bound closes this part of the tail. Overlapping face
maxima stop as a comparison tie. All remaining finite rectangles must
then have upper bounds below the face's lower witness. Only completion of
both arguments reports `ZipLimit` or `PoissonLimit`.

The reverse bound, with $B_2=\sum_{j\ge1}j^2w_j$, is

$$
 \ell_{\rm ZINB}(r,e^{-t/r},\pi)-\ell_{\rm ZIP}(t,\pi)
 \ge\frac{A-St/2}{r}-\frac{B_2}{2r^2}.
$$

It uses $\log(1+x)\ge x-x^2/2$ and Jensen's inequality for the uniform
Laplace transform. A strictly positive coefficient at a globally maximizing
ZIP rate proves a finite improvement and hence finite attainment. It does
not locate that maximizer. Near this transition, the fitter can stop on
budget rather than resolve the coefficients; it never substitutes a ZIP
non-existence claim.

## NB reductions and numerical contract

With free inflation, an interior stationary fit matches both the empirical
zero share and empirical mean. Every finite ZINB is a mixed Poisson law,
so Jensen gives $P(X=0)>\exp[-E(X)]$ at positive mean. A padded empirical
zero deficit $n_0/n<\exp(-S/n)$ therefore excludes every positive-inflation
stationary maximum. Only the certified NB member and its Poisson limit
remain, and the NB certificate decides between them. No-zero samples also
maximize free inflation at zero; with inflation held, their likelihood is
NB plus the constant $n\log(1-\pi)$.

A second free-inflation reduction uses the certified positive-count
truncated-NB profile. If it has no feasible finite stationary maximum, only
the ordinary-NB stationary point and the certified ZIP face remain. Neither
an unresolved NB solve nor an unresolved truncated-NB profile is dropped.
NB-versus-ZIP comparisons use likelihood enclosures, including the NB
root-width allowance. Overlap stops as a tie. Inflation zero is an attained
NB member, returned silently rather than treated as non-existence.

All sums of recurrence terms use compensation. Small-argument Bernoulli
series for $J$ and the uniform log Laplace transform have remainders below
$10^{-27}$ at the switch $u=0.01$. Joint likelihood, gradient and Hessian
bounds use $4096\epsilon(1+\text{absolute term scale})(1+M\epsilon)$;
computed upper bounds get an additional allowance. Positive tail and
moment comparisons have their own term-scaled margins. These are exact
arithmetic certificates conditional on padded computed comparisons, with
practical binary64 margins; they are not outward-rounded intervals or
universal libm error bounds.

Stop candidates use `(size,prob,pi)` for finite witnesses, `(lambda,pi)`
for ZIP and `lambda` for Poisson limits. The
[validation report](../../../../../dev/research/evaluations/2026-11-06-zinb-cert-report.md)
records the unresolved near-face workloads.

Zero profiles use the empirical positive fraction $k/n$ and
$1-p^r=-\operatorname{expm1}(r\log p)$, rather than subtracting two
probabilities near one. Saturated indicator terms use `log1p(-k/n)` and
`log(k/n)`. A free-inflation NB/truncated-NB join within its term-scaled
comparison allowance stops. Analytic probability formulas receive an
$8\epsilon p$ rounding enclosure; numerical root-width targets apply to
the bisected branches. Encoded inflation must reproduce the positive mass
to relative accuracy $10^{-7}$. Zero log masses use their positive-mass
complement near a point mass and log-sum-exp in the small-zero-mass regime.
These protections can report numerical failure when an otherwise proper
maximizer is too close to a seam or cannot be encoded accurately.

A failed joint proposal can be restarted once from a feasible certified
positive-count truncated-NB fit when inflation is free. The rescue repeats
the joint root enclosure, curvature and complete exterior cover; it does
not accept a failed optimizer or the parent coefficients without the joint
certificate. The Newton proposal gate is tighter than final acceptance to
allow for the probability-coordinate transformation. Delegated and repeated
certificate work is retained in the reported iteration count.

Profiling inflation or probability does not decouple their coefficient
errors from size. The full zero-mixture information supplies that propagation;
integer rising-factorial derivatives avoid subtracting nearby digamma values
near the Poisson face.
