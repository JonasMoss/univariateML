---
id: bbinom
inference_numeric:
  - regime: "size zero"
    available: false
    condition: "known integer size = 0 held; the law is a point mass and every regular score is zero"
    reason: "The free information is structurally singular; neither observed nor sandwich covariance can be inverted"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#finite-support-rank-and-known-design-submodels"
  - regime: "size one with one shape known"
    available: true
    condition: "known size = 1; exactly one positive finite shape free; the other shape known; rank one for the two-shape model; the scalar known-nuisance submodel is identifiable"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#finite-support-rank-and-known-design-submodels"
  - regime: "size one with both shapes free"
    available: false
    condition: "known size = 1; both positive finite shapes free; DQM and pointwise C2 hold, but joint information has rank one and covariance is singular"
    reason: "The free information is structurally singular; neither observed nor sandwich covariance can be inverted"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#finite-support-rank-and-known-design-submodels"
  - regime: "primary interior"
    available: true
    condition: "known integer size >= 2 held; finite alpha > 0 and beta > 0; either or both free"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#finite-support-rank-and-known-design-submodels"
name: Beta-binomial
support: {type: integer, lower: "0", upper: "size", closed: [true, true]}
params:
- name: size
  symbol: 'n'
  role: trials
  domain: '[0, inf)'
  simulate: 10
  integer: yes
  kind: design
  advice: Supply the number of trials.
- name: alpha
  symbol: '\alpha'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: beta
  symbol: '\beta'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: dbbinom
  sampler: rbbinom
  control:
    size: "Required number of trials; NULL estimates size when either shape is held."
    alpha: "Optional fixed positive alpha; NULL estimates it."
    beta: "Optional fixed value of `beta`; `NULL` (the default) estimates it."
    reltol: "Relative accuracy required in every free shape coefficient; a curvature and score enclosure must pass before acceptance."
    iterlim: "Work limit per solve: 1 * min(iterlim, 100000) solver iterations (default iterlim 500); per certificate: 128 * min(iterlim, 100000) certificate evaluations; per enumeration: 100000 * min(iterlim, 100000) enumeration cells."
log_density: "loggamma(size + 1) - loggamma(x + 1) - loggamma(size - x + 1) + loggamma(x + alpha) + loggamma(size - x + beta) - loggamma(size + alpha + beta) - loggamma(alpha) - loggamma(beta) + loggamma(alpha + beta)"
limits:
  - model: binom
    firmness: proved
    when: '$\alpha + \beta \to \infty$ with $\alpha/(\alpha + \beta)$ fixed'
    detect: signed uniform near-face exclusion and strict tangent/chord bounds exclude every finite shape pair
    status: BinomialLimit
    numerical: true
  - name: two-point law on 0 and n
    firmness: proved
    when: '$\alpha, \beta \to 0$ with $\alpha/(\alpha + \beta)$ fixed'
    detect: '$n \ge 2$, every observation is 0 or $n$, and both values occur'
    status: TwoPointLimit
  - model: binom
    firmness: proved
    when: '$\alpha/(\alpha + \beta) \to 0$ or $1$'
    detect: 'all observations are 0, or all equal $n$'
    status: PointMass
references:
- Griffiths, D. A. (1973). Maximum likelihood estimation for the beta-binomial distribution and an application to
  the household distribution of the total number of cases of a disease. Biometrics, 29(4), 637-648. doi:10.2307/2529131.
- Johnson, N. L., Kemp, A. W. and Kotz, S. (2005). Univariate Discrete Distributions, third edition. Wiley. doi:10.1002/0471715816.
holds:
- held: []
  status: not-implemented
  reason: "Estimating size together with both shapes is not implemented. It would require comparing the fits at every finite size, including their binomial and two-point limits, with the negative binomial and Poisson limits, and no usable bound on large sizes is known."
fit_args:
  size: 10
examples: |
  set.seed(1)
  x <- rml(200, mlbbinom())
  mlbbinom(x, size = 10L)

group: Poisson, binomial and negative binomial
construction:
- compounding
relations:
  mixture_of:
  - family: binom
    at:
      size: size
      prob: mix
    mixed: prob
    mixing:
      family: beta
      at:
        shape1: alpha
        shape2: beta
fit:
  method: profile-search
  certified: partial
  note: "bbinom/mle.hpp and acceptance.hpp: Certified fixed-size finite fits require a negative-curvature region, score-based enclosures transported to every free public shape, and strict tangent/chord exclusion outside that region. Binomial-limit stops use a signed uniform near-face bound; size-two saturated fits are algebraic. Certificates are conditional on computed comparisons with practical floating-point margins. Unclosed finite or face comparisons, coefficient inclusion and certificate work limits return the successful finite search result uncertified; a failed local solve without a candidate stops. Supported free-size fits retain their integer tail certificates; the unrestricted free-size fit is unsupported. See Details, Fixed-size coefficient acceptance."
areas:
- counts and ecology
classification: specialist
classification_reason: Used to model overdispersed success counts with heterogeneous binomial probabilities.
summary: Beta-binomial provides a model for overdispersed success counts with heterogeneous binomial probabilities.
aliases: []
key_references:
- 2
existence:
  uniqueness: unknown
  statement: >-
    Take $n \ge 2$ known and both shapes free. If every count is 0 or $n$, no MLE
    exists: the likelihood is maximised in the limit by a two-point law on 0
    and $n$ when both values occur, and by a point mass when only one does.
    Otherwise the only possible escape is to the binomial, and an MLE exists
    exactly when some beta-binomial fits at least as well as the best
    binomial. Overdispersion, $S^2 > n\hat p(1-\hat p)$ with
    $\hat p = \bar x/n$ and $S^2$ the sample variance with the number of
    observations as divisor, is sufficient. For $n = 2$ it is also necessary;
    for $n \ge 3$ that is a conjecture. Holding one shape leaves only the
    point-mass case. With $n = 1$ the shapes are not identified, because the
    model is a Bernoulli distribution. At $n=0$ both shapes are also
    unidentified. With beta held and size free,
    constant positive data have only a point-mass supremum; all-zero data
    attain likelihood one at size zero with alpha unidentified. For other
    samples a finite maximum exists, though the numerical search may stop
    without resolving it. With alpha held and size free, constant positive
    data have a point-mass supremum and all-zero data attain likelihood one
    at size zero with beta unidentified. Other samples may remain unresolved
    against the negative-binomial face or the size budget.
  reason: >-
    Write $p = \alpha/(\alpha+\beta)$ and $s = 1/(\alpha+\beta)$. A sample with
    a count strictly between 0 and $n$ loses all likelihood as $s \to \infty$
    or as $p$ tends to 0 or 1, so the binomial face $s = 0$ is its only
    escape. On that face the profile score in $s$ has the sign of
    $S^2 - n\hat p(1-\hat p)$, so overdispersion makes the likelihood increase
    into the interior.
---

The beta-binomial distribution is a binomial distribution whose success probability is itself beta distributed. It models overdispersed counts out of a known number of trials $n$.

## Details

The limit and sample conditions are supported by the [existence analysis](../../../../../dev/research/evaluations/2026-10-31-open-existence-report.md). The number of trials $n$ is normally known. A binomial-limit declaration requires the signed certificate below; a small finite likelihood gain is never discarded in favour of that limit.

With both shapes held it can also be estimated: each observed probability decreases in $n$ once $n + 1 \ge x(\alpha + \beta - 1)/\alpha$, so a finite search over $n$ finds the maximum. The search shares the enumeration-cell budget across candidate likelihoods. With $n$ also held, the likelihood is simply evaluated. Estimating $n$ with $\alpha$ held uses a negative-binomial face and a uniform tail bound; with both shapes free it remains unsupported. Holding $\beta$ is not equivalent to holding $\alpha$ after reflecting the counts, because the reflection $x \mapsto n - x$ depends on the unknown $n$.

## Signed binomial-face certificate

Write $h$ for the number of observations, $N$ for the known size,
$S=\sum x_i$, $Q=\sum x_i^2$, and $p_0=S/(hN)$. For a sample with
an interior count the only escape is $s=0$. With the rising-factorial
numerator denoted by $V(p,s)$ and the denominator by
$D(s)=h\sum_j\log(1+js)$, $V$ is jointly concave. Its tangent at
$(p_0,0)$ has zero $p$ slope. Since $-D''(s)\le B=h\sum_j j^2$,
this gives the bound, uniform over every $0<p<1$,

$$
L(s)-L(0)\le g_0s+\tfrac12 Bs^2,\qquad
 g_0=\frac{NhQ-(N-1)S^2-NhS}{2hNp_0(1-p_0)}.
$$

If $g_0<0$, the interval $0<s\le -g_0/B$ is strictly below the face.
The score numerator is evaluated with extended intermediates and a
$64\epsilon$ absolute-sum margin. An exactly zero numerator is used only
when the integer products and their sum fit the intermediate mantissa.
For a zero score, or to enlarge a small negative-score strip, the solver
brackets every profiled mean in a rectangle by
uniform signs of the mean score. Rational endpoint bounds on that rectangle
bound $L''=\ell_{ss}+\ell_{ps}^2/(-\ell_{pp})$. A strictly negative bound
proves that its positive-$s$ portion also lies below the face whenever
$g_0\le0$. The curvature
and score comparisons include a $1024\epsilon$ scale margin. If neither
argument closes a neighbourhood, the comparison remains unresolved.

Outside that neighbourhood, the tangent/chord upper bounds must be strictly
below the face, including their rounding margin. The tail beyond
$hH_{N-1}/\#\{0<x_i<N\}$ has negative dispersion score for every mean.
Together these bounds prove nonattainment at finite shapes, conditional on
computed comparisons. They use practical floating-point margins rather than
outward rounding. A negative boundary score by itself is not treated as a
global criterion.

For $N=2$, an overdispersed empirical table is itself a beta-binomial law,
so its shapes are obtained algebraically and its likelihood attains the
saturated multinomial upper bound. For $N>2$, finite candidates must beat
the face by a $1024\epsilon$
likelihood-scale margin. Their global tangent/chord check strictly excludes every point outside
a region whose curvature proves a unique stationary maximum, and the score
encloses its public coefficients as described below. For example, at $N=2$, frequencies $(25001,50000,25001)$
have the exact finite fit $\alpha=\beta=25000$, although its improvement is
below the former face preference. At $N=1$ every pair with the same shape
ratio gives the same fitted Bernoulli law, providing a continuum of maxima.
Other unresolved comparisons, coefficient inclusion failures and exhausted certificates return the successful finite search result with `certified = FALSE`. A failed local search without a finite candidate still stops. Proof details and reproducible checks are in the
[face report](../../../../../dev/research/evaluations/2026-11-07-bbinom-face-report.md).

## Free-size research: joint held-beta tail

Let $h$ be the number of observations, $m>0$ the number of positive
observations, $M=\max x_i$, and let $b>0$ be held. For $N\ge2M$ put
$H_N=\sum_{j=0}^{N-1}(b+1+j)^{-1}$. The held-beta inequalities proved in
`dev/research/evaluations/2026-10-30-holds-bbinom-report.md` give, for
$0<a\le1$, $P_0\le e^{-aH_N}$ and
$P_x\le 2^x a e^{-aH_N}$ for $x>0$. For $a\ge1$, stochastic monotonicity
and the same report give $P_0\le e^{-H_N}$ and
$P_x\le(2^{x+1}-1)e^{-H_N}$. Thus the **whole likelihood**, uniformly in
$a>0$, is at most

$$
\max\left\{2^{\sum_i x_i}\left(\frac{m}{e h H_N}\right)^m,
 e^{-hH_N}\prod_{x_i>0}(2^{x_i+1}-1)\right\}.
$$

The first term maximises $a^m e^{-haH_N}$ over all $a>0$; allowing
$a>1$ in that maximisation only weakens the bound. Both terms decrease in
$N$. A candidate with positive likelihood supplies a finite cutoff, but
that cutoff can be impractically large. This improves the earlier
single-observation $O(1/\log N)$ likelihood bound to
$O((\log N)^{-m})$. The power is sharp in general: along
$a=t/\log N$ for fixed $t>0$, $P_0\to e^{-t}$ and
$P_x\sim e^{-t}t/(x\log N)$ for each fixed $x>0$. Consequently the
likelihood times $(\log N)^m$ tends to
$e^{-ht}t^m/\prod_{x_i>0}x_i$.

The earlier bound alone does not certify the integer profile comparison: the
fixed-size coefficient enclosures alone do not order different integer sizes;
the free-size comparisons retain their separate likelihood margins. Held alpha has a fixed-shape
negative-binomial face, while the unrestricted subset also has arbitrary
negative-binomial and Poisson faces and fixed-size binomial and endpoint
faces. A uniform tail correction when both shapes are free and a global
comparison of the finite-size faces remain open. The held-shape searches below supply separate certificates. Details and numerical checks are recorded in
`dev/research/evaluations/2026-11-01-bbinom-size-report.md`.

## R notes

`size = NULL` estimates the number of trials when either shape is held; the search may stop without resolving the face or exhausting its size budget. A supplied `size` is held fixed and is not counted in the degrees of freedom of `logLik()`, AIC or BIC. `model_select()` skips this family because it cannot supply `size`.


## Held beta: finite profile and size certificate

Fix $b>0$, write $h$ for sample size, $m$ for the number of positive
counts and $M$ for their maximum. At fixed $N$, the profile in $a$ is
not generally concave in $\log a$: for one observation $x=N=2$ and
$b=10$, its second derivative in $\log a$ is positive for sufficiently
small $a$, with leading coefficient $1-1/10-1/11$ times $a$. Its score is
a sum of positive numerator terms and negative denominator terms. The
implementation uses the analytic score signs to bound the search domain,
proposes roots on a log grid, and accepts only after the fixed-size coefficient
certificate below. No grid spacing is assumed to isolate every mode.

At fixed $N>0$, $a\to0$ gives a point mass at zero and $a\to\infty$
a point mass at $N$. For nonconstant data both faces have zero likelihood.
For constant positive data $N=M$ has supremum one at the upper point mass,
which no finite positive shapes attain. For all-zero data, $N=0$ attains
likelihood one and leaves $a$ unidentified. For a positive observation the
joint likelihood tends to zero uniformly as $N\to\infty$, as shown above.

A practical tail certificate follows from the exact ratio
$$
\frac{P_{N+1,a,b}(x)}{P_{N,a,b}(x)}
=\frac{N+1}{N+1-x}\frac{N-x+b}{N+a+b}.
$$
It is at most one when $a\ge x(b-1)/(N+1-x)$. For $b\le1$
every likelihood decreases in $N$. Otherwise the alpha score at size $N+1$
is positive below $m/(hH)$, where $H=\sum_{j=0}^{N}(b+j)^{-1}$.
Consequently every maximizer lies above this bound. If
$m(N+1-M)\ge hM(b-1)H$, the profile at $N+1$ cannot exceed that at $N$.
The left side divided by $H$ increases with $N$, so this comparison
holds for all subsequent sizes. The search enumerates up to the first such
$N$, subject to the enumeration-cell budget, and stops on exhaustion. It compares profile values
with a $2\cdot10^{-8}h$ ambiguity margin and stops on a numerical tie.
The cutoff inequality uses an additional floating-point margin; neither
margin is an interval-arithmetic guarantee. Each visited fixed-size profile
also passes the coefficient certificate.


## Held alpha: negative-binomial face and uniform size bound

Let $a>0$ be held, $h$ be the sample size, $S=\sum x_i>0$,
$M=\max x_i$, and $N>M$. The consecutive-size ratio displayed above is
at most one exactly when $a(N+1)\ge x(a+b-1)$. An upper bound on
$b$ at all profile maxima that would force this inequality is not available
from a fixed bound: on the path $b=N a/\mu$, the law tends to the negative
binomial with shape $a$ and mean $\mu>0$, so $b$ diverges linearly. The
ordinary beta score yields the weaker finite-size bound
$b\le(hN-S)(a+N)/S$ at a stationary point.
For nonconstant data with $S>0$, bounded $b$ makes the likelihood tend
to zero, while the negative-binomial path gives a positive limit. More
generally, $b/N\to0$ forces the likelihood to zero by the small-beta bound
below, and $b/N\to\infty$ does so because
$P(X\ge1)\le E[X]=Na/(a+b)$. On compact positive ranges of $b/N$ the
likelihood converges uniformly to its negative-binomial counterpart, whose
mean profile has the unique maximizer $\bar x$. Thus any global profile
maximizers satisfy $b/N\to a/\bar x$; in particular no fixed upper bound
on them exists.

The negative-binomial face has maximized log likelihood
$$
F=\sum_i\log\frac{(a)_{x_i}}{x_i!}
 + S\log\frac{\bar x}{a+\bar x}
 + ha\log\frac{a}{a+\bar x},\quad \bar x=S/h.
$$
For any $c>0$ split the parameter space at $b=cN$. For $b\le cN$,
$P_N(x)/P_N(0)\le (a)_x/x!\,[N/(N-M)]^x$, and
$P_N(0)\le[(a+b)/(a+b+N)]^a$. Thus its log likelihood is at most
$$
B(N,c)=\sum_i\log\frac{(a)_{x_i}}{x_i!}
 +S\log\frac{N}{N-M}
 +ha\log\frac{a+cN}{a+(1+c)N}.
$$
For $b\ge cN$, compare to a negative binomial with shape $a$ and mean
$aN/b$. Their exact pmf ratio is
$$
\prod_{j=0}^{x-1}\frac{(N-j)(N+b)}{N(b+N-x+j)}\;
\exp\!\left[a\log(1+N/b)-\sum_{j=0}^{N-1}\log\left(1+\frac a{b+j}\right)\right].
$$
Bounding the product and applying $\log(1+t)\ge t-t^2/2$
and a decreasing-sum integral bound yields the uniform upper bound
$$
\ell_N(b)\le F + \frac{\sum_i x_i(x_i+1)}{2(N-M)}
 +\frac{ha^2}{2}\left(\frac1{cN}+\frac1{(cN)^2}\right).
$$
Both upper bounds decrease with $N$. A finite fit strictly above both
bounds at the next size excludes every subsequent size, using the retained
cross-size ambiguity margin. The code
selects $c$ by bisection, enumerates finite sizes subject to the enumeration-cell budget, and stops on a
numerical tie or failure. Before enumeration, it requires the first profile
to exclude the tail beyond the budget; otherwise it reports a budget stop,
even though a larger finite profile could improve the bound. If the first finite profile does not clearly beat
$F$, it returns the prefix winner uncertified; this does not prove that the face wins.
The inequalities are proved in exact arithmetic conditional on computed
comparisons; the practical floating-point margin is not an interval proof.
At each fixed size, the same tangent/chord certificate used for held beta
applies to the free beta directly. Reflection changes the data with $N$ and
therefore cannot transfer the held-beta size-tail argument.

With both shapes free, the correction above is not uniform in $a$: its
$a^2$ factor diverges. For each fixed $a>0$ and $\mu>0$, the path
$b=Na/\mu$ gives the negative-binomial face; taking $a\to\infty$ at fixed
$\mu$ gives the Poisson face. The unrestricted face supremum therefore
includes the supremum of $F(a)$ over $a>0$ and its Poisson limit. A usable
joint bound must control the large- and small-$a$ regimes, establish which
other joint escape paths can retain positive sample likelihood, compare
fixed-size binomial and endpoint faces, and propagate the finite-profile
allowances. An exhaustive closure proof and a global search remain open.

## Stop details

Binomial and endpoint faces retain their fitted probabilities. Uncertified free-size results retain evaluated finite profiles and the negative-binomial face when computed; their global upper bound remains infinite.

The size search charges rising-factorial, likelihood, derivative and
certificate-bound scans by their support cells, sharing the cell budget
across recursively fitted sizes. A size profile is not an O(1) cell. An
exhausted cell ledger stops with its evaluated candidates retained; no
size-profile unimodality is assumed.


## Fixed-size coefficient acceptance

For a finite both-shapes-free proposal, `acceptance.hpp` brackets the profiled
mean throughout a dispersion interval by decreasing rational score bounds.
On the resulting rectangle it bounds $-\ell_{pp}\ge M>0$,
$|\ell_{ps}|\le B$, and
$\ell_{ss}+\ell_{ps}^2/(-\ell_{pp})\le-K<0$.
The residual profile score, including the mean bracket's contribution,
therefore bounds the root's dispersion error by $\delta_s=|L'|/K$;
the mean error is at most its bracket radius plus $B\delta_s/M$.
The image of this rectangle under $\alpha=p/s$, $\beta=(1-p)/s$
must meet the requested relative accuracy in each free public shape.
With one shape held, a negative scalar-curvature bound gives the analogous
score-to-root enclosure directly in the other shape.

The region has at most one stationary maximum. The enclosed root lies strictly
inside it; strict tangent/chord bounds exclude its entire complement in the
analytic compact search domain. Thus acceptance proves global optimality and
uniqueness for that sample, conditional on the comparisons. The `certified` flag records whether these exclusions close. Separated proposed maxima with indistinguishable likelihoods, coefficient inclusion failures and exhausted certificates return the successful finite search result uncertified. The
size-two saturated solution retains its algebraic reduction.

Near the binomial face the dispersion score subtracts its face value
symbolically, using the integer moment numerator above and a rational
remainder, to avoid cancellation of large summands. Score margins use
$32\epsilon_L$ absolute sums, curvature uses $128\epsilon_L$, and public
coefficient conversion reserves $8\epsilon$; $\epsilon_L$ is the arithmetic
precision of the intermediates. Global comparisons include the existing
$128\epsilon$ upper padding and an additional feasible-anchor margin.
These are practical floating-point margins, not outward-rounded intervals.
A general symbolic uniqueness theorem remains unknown; at sizes zero and
one the previously described continua are explicit counterexamples.

### Extreme d/p/q arithmetic

In the mass recurrence the remaining integer count is formed before adding beta. Subtracting the counts after adding a tiny beta can discard its significant digits at the last cell, contaminating both summed tails.
