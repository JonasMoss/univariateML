---
id: bhatt
inference_numeric:
  - regime: "normal member with width known"
    available: true
    condition: "finite mu and sigma > 0; a = 0 held; mu and/or sigma free"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels"
  - regime: primary interior
    available: true
    condition: "finite mu, sigma > 0 and a > 0; any regular subset free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Bhattacharjee
support: {type: real, lower: -inf, upper: inf, closed: [false, false]}
params:
- name: mu
  symbol: '\mu'
  role: location
  domain: (-inf, inf)
  simulate: 0
  kind: regular
- name: sigma
  symbol: '\sigma'
  role: scale
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: a
  symbol: 'a'
  role: half-width
  domain: '[0, inf)'
  simulate: 1
  kind: regular
r:
  density: dbhatt
  sampler: rbhatt
  control:
    mu: Optional held mu; NULL estimates it.
    sigma: Optional positive held sigma; NULL estimates it.
    a: Optional nonnegative held a; NULL estimates it.
    reltol: Relative search tolerance; certified fits require coefficient enclosures with sigma as the location floor.
    iterlim: "Work limit: 1 * min(iterlim, 100000) iterations per solve and 128 * min(iterlim, 100000) certificate evaluations."
log_density: log((erf((x-mu+a)/(sqrt(2)*sigma))-erf((x-mu-a)/(sqrt(2)*sigma)))/(4*a))
limits:
  - model: unif
    firmness: proved
    when: 'The normal scale satisfies $\sigma\to0$ with positive half-width $a$ fixed or approaching a positive limit.'
    detect: 'Positive held a uses proved covering and endpoint bounds. Free a uses a global width certificate with a whole-neighbourhood small-sigma bound and compatible normal and uniform comparisons.'
    status: UniformLimit
    numerical: true
holds: []
examples: |
  set.seed(1)
  x <- rml(200, mlbhatt())
  mlbhatt(x)

group: normal convolutions
construction:
- compounding
relations:
  convolution_of:
  - family: norm
    operation: sum
    components:
    - family: norm
      at:
        mean: mu
        sd: sigma
    - family: unif
      at:
        min: -a
        max: a
    when: a > 0
fit:
  method: profile-search
  certified: 'partial'
  note: 'bhatt/README.md, Details: positive held a uses concavity; held a=0 and direct held-sigma normal domination use normal certificates. Free-a finite and uniform winners are certified only when the global width comparison closes, with coefficient enclosures for finite fits. All-free normal choices and other successful inconclusive searches remain per-fit certified=0.'
areas:
- measurement and errors
classification: uncommon
classification_reason: Introduced for dimensional chains involving rectangular and normal errors; broader adoption
  is not established by the sources checked.
summary: Bhattacharjee provides a model for dimensional tolerance chains combining rectangular and Gaussian errors.
aliases: []
references:
- Bhattacharjee, G. P., Pandit, S. N. N. and Mohan, R. (1963). Dimensional chains involving rectangular and normal
  error-distributions. Technometrics, 5, 404–406.
key_references:
- 1
existence:
  uniqueness: unknown
  statement: >-
    With all coefficients free, constant samples have unbounded likelihood.
    For nonconstant samples the only unattained nonzero limiting face is
    uniform; the normal case $a=0$ is an attained member. General joint
    uniqueness and a complete data-only existence criterion are not
    established. With positive $a$ held and location free, an MLE exists
    exactly when the sample range exceeds $2a$; otherwise the uniform
    supremum is unattained. Holding $\sigma>0$ guarantees attainment,
    whether location is held or free.
    With positive a held, an attained maximum is unique, whether location is held or free. With both scales held, free location has a unique finite maximum.
    Holding $a=0$ gives the corresponding normal fitting problem.
  reason: >-
    The density is bounded by both $1/(2a)$ for $a>0$ and
    $1/(\sqrt{2\pi}\sigma)$. Diverging scales or location lose likelihood;
    joint scale collapse loses likelihood for nonconstant data. Conditional
    location is strictly log concave. Uniform-face bounds and the endpoint
    penalties below establish the stated held-width criterion.
---

The Bhattacharjee distribution is the convolution of a normal distribution and a symmetric uniform distribution.

The coefficient $\mu$ is the centre, $\sigma$ is the normal standard
deviation and $a$ is the uniform half-width. Holding a coefficient fixes its
physical value, rather than a dimensionless shape ratio.

## d/p/q/r

### Extreme d/p/q arithmetic

Near an edge of the uniform convolution, subtract its physical half-width before dividing by the Gaussian scale. Subtracting two separately standardized coordinates loses the edge distance when the Gaussian scale is small.

## Details

At $a=0$ fitting reduces to the conditional normal problem. Positive held
$a$ has an additional concavity structure: in $(u,b)=(\mu/\sigma,1/\sigma)$,

$$
L(u,b)=-n\log(2a)+\sum_i\log\{\Phi(b(x_i+a)-u)-\Phi(b(x_i-a)-u)\}.
$$

Normal interval probability is log concave in its two endpoints, by
Prékopa's theorem. Both endpoints are affine here. Holding $\mu$ imposes
$u=b\mu$; holding $\sigma$ fixes $b$. Thus the held-width problems are
concave on convex domains, including the location-only slice. An enclosed
score root with negative definite local Hessian proves a unique global
maximum. The numerical certificate requires a coefficient enclosure within
`reltol`, with $\sigma$ as the natural location floor; a small score alone
does not suffice. With free $\mu$, positive held $a$ and range greater than $2a$, the maximum is attained and unique.
Holding $k=a/\sigma$ instead specifies a different concave slice. The
normal member at held `a=0` keeps its closed-form fit when only the practical
coefficient inclusion fails; physical residual scores still reject a displaced
normal mean or scale.

With $\sigma$ held and $a$ free, put $k=a/\sigma$. At each fixed $k$,
location is strictly log concave. The profiled objective $P(k)$ satisfies
$P(k)+n\log k$ concave. Supporting tangents of this adjusted profile give
upper bounds for every width interval, using enclosed inner optima.
Negative definite Hessian boxes and uniform nuisance inclusions isolate
stationary winners; every other interval must be excluded before accepting
a finite fit. Large widths are bounded by $f\le1/(2a)$. For small widths,
Hoeffding's inequality bounds the convolution by the corresponding normal
likelihood plus $k^2\sum_i((x_i-\mu)/\sigma)^2/2$, optimized at the
conditional normal mean. A normal comparison that cannot be excluded leaves a successful search result uncertified. With $\sigma$ held, the normal member is the unique global winner when
$\sum_i(x_i-\mu)^2\le n\sigma^2$ for held location, or
$\sum_i(x_i-\bar x)^2\le n\sigma^2$ for free location. The numerical
comparison requires a padded upper bound; an unresolved comparison keeps a
successful search uncertified.

To prove this, let $W$ be standard normal truncated to $[-k,k]$,
$v=\operatorname{Var}(W)<1$, and
$C(k)=E_{U[-k,k]}\exp(-U^2/2)$. Exponentially tilting $W$ cannot increase
its variance away from zero. Indeed, for two independent tilted copies put
$H=(W_1+W_2)/2$ and $R=(W_1-W_2)/2$. Conditional $E[R^2\mid |H|]$
decreases with $|H|$, whereas
$E[H\mid |H|]=|H|\tanh(2t|H|)$ increases for positive tilt $t$.
Thus $d\operatorname{Var}_t(W)/dt=4\operatorname{Cov}_t(R^2,H)\le0$,
and $\log E\exp(zW)\le vz^2/2$. Also $-\log C(k)\ge v/2$:
write $w=k^2$ and integrate the decreasing second moment under the density
proportional to $\exp(-wU^2/2)$ on $[-1,1]$. The density ratio therefore obeys

$$
\log\frac{f(x;\mu,\sigma,a)}{\phi_\sigma(x-\mu)}
\le \frac v2\left\{\frac{(x-\mu)^2}{\sigma^2}-1\right\}.
$$

For free location, completing the normal square gives

$$
L(\mu,\sigma,a)-L_N(\bar x,\sigma)
\le\frac v2\left\{\frac{\sum_i(x_i-\bar x)^2}{\sigma^2}-n\right\}
-\frac{(1-v)n(\mu-\bar x)^2}{2\sigma^2}.
$$

The held-location case follows by summation. The central-mass inequality is
strict for $k>0$, giving uniqueness at $a=0$ under the stated condition.
This comparison holds at fixed normal scale; it does not certify an
unrestricted normal winner when $\sigma$ can decrease as $a$ increases.

The normal-interval and score comparisons use practical binary64 margins,
conditional on computed comparisons. Their scope is the enclosed score root
and the likelihood bounds above. Coefficient enclosures are checked in original
units, and finite stopped witnesses are evaluated at their recovered physical
coefficients. An inconclusive comparison retains the existing search result with per-fit `certified=0`, unless an evaluated certificate witness proves that result loses or the search itself fails. An unproved uniform-face selection tries a finite search with face screening disabled; only the closed comparison selects a limit stop.

With both scales free, the width certificate profiles $(u,b)$ at fixed
$k=a/\sigma$, imposing $u=b\mu$ when location is held. The adjusted
profile $P(k)+n\log k$ is concave: its joint objective is a sum of
log-concave normal interval masses with affine endpoints, plus $n\log b$.
The same supporting tangents and winner boxes therefore apply with free
location. For small $k<1$, optimizing the Hoeffding bound over normal scale
gives the normal maximum minus $n\log(1-k^2)/2$.

For $k\ge2n$, let $r$ be the sample half-range with free location, or
$\max_i|x_i-\mu|$ with location held. At least one observation is at
distance at least $r$ from every compatible centre. The peak bound covers
$a\ge r$. For $a<r$, putting $t=r/a>1$ gives

$$
\prod_i f(x_i)\le(2r)^{-n}t^n\Phi(-k(t-1)).
$$

The derivative of $n\log t+\log\Phi(-k(t-1))$ is
$n/t-kR(k(t-1))<0$, since the inverse Mills ratio satisfies
$R(v)\ge\sqrt{2/\pi}$ for $v\ge0$. This bounds the **whole**
small-$\sigma$ neighbourhood by the compatible uniform face. An earlier
cutoff $K$ can be used when an interior witness dominates that face:
$\Phi(-v)\le\exp(-v^2/2)/2$ and $\log t\le t-1$ bound the
whole $k\ge K$ tail by the uniform log likelihood plus
$\max(0,n^2/(2K^2)-\log2)$. A finite
winner is accepted only after every remaining width interval is excluded
or covered by an isolated strictly concave winner region, its coefficient
enclosure meets `reltol`, and its likelihood separates from both faces
and every other surviving winner. Indistinguishable certificate winners prevent certified acceptance.
Unexcluded faces, failed enclosures and exhausted certificate budgets leave a successful existing search result uncertified. Stops remain when the search fails or an evaluated certificate witness proves its result wrong.
In particular, numerical proximity to
$a=0$ does not prove that the normal member is the global winner.

Identification follows from $E[X]=\mu$, $\kappa_4=-2a^4/15$ and
$\operatorname{Var}(X)=\sigma^2+a^2/3$. The bounds $f\le1/(2a)$ and
$f\le1/(\sqrt{2\pi}\sigma)$ exclude diverging scales on a positive
likelihood level set. With bounded scales, escaping location loses
likelihood. Joint scale collapse is unbounded only when location can
coincide with a constant sample; for nonconstant data, the only nonzero
faces are normal ($a=0$, attained) and uniform ($\sigma\to0$, unattained).
These facts do not establish joint uniqueness with both scales free.

For free width the uniform face uses the midrange and half-range, or the
maximum distance from held $\mu$. Endpoints can expand more slowly than
$\sigma$ vanishes. For held positive $a$, range below $2a$ gives face
likelihood $(2a)^{-n}$. At range $2a$, endpoint multiplicities $j,l$ give
the additional factor $(j/(j+l))^j(l/(j+l))^l$, by optimizing the two
complementary normal endpoint probabilities. With location held instead,
each observation exactly at an endpoint contributes $1/2$. Pointwise and
complementary endpoint bounds prove that these held-width faces dominate
every positive-scale fit. Range greater than $2a$, or a distance greater
than $a$ with location held, instead forces likelihood to zero as
$\sigma\to0$ and gives an attained positive-scale maximum.

Winning uniform faces need not have a unique centre. For data
$\{-1,0,1\}$ with $a=2$ held, every centre in $(-1,1)$ gives the same
unattained supremum $-3\log4$ as $\sigma\to0$. This does not contradict
uniqueness of an attained positive-scale maximum with width held.

With $\sigma>0$ held, compactness and continuity give attainment with
location held or free, since width divergence loses likelihood and $a=0$
is included. Attainment alone does not certify the width search or imply
uniqueness.

The unrestricted uniform face is always a local maximum of the likelihood
closure. Writing $r$ for the sample half-range, the peak bound rules out
improvement for $a\ge r$, and the extreme-pair probability bound does so
for $r4^{-1/n}\le a<r$. This closure maximum need not be global: twenty
zeros with one observation at each of $-10$ and $10$ have a normal-member
witness with higher likelihood. Conditional location uniqueness therefore
does not imply a single maximum of the likelihood closure. At the normal maximum for this sample, the standardized fourth Hermite
sum is 176. With total variance $v=\sigma^2+a^2/3$ and
$\eta=a/\sqrt v$, profiling the regular normal coordinates gives
$L(\eta)=L_N-44\eta^4/45+O(\eta^6)$, so the normal member is also a
strict local maximum. This local expansion does not prove global normal
domination. No example of multiple finite global scale maximizers is asserted.

## R notes

`mlunif` fits unrestricted endpoints and does not preserve held `mu` or `a`.
Normal-member comparisons retain held `mu` and `sigma`. A proved normal
winner is returned with `a = 0`; an unresolved comparison retains a successful search result with `certified=0`.
