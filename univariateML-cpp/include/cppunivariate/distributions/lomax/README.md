---
id: lomax
inference_numeric:
  - regime: primary interior
    available: true
    condition: "finite lambda > 0 and kappa > 0; either or both free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Lomax
support: {type: real, lower: 0, upper: inf, closed: [false, false]}
params:
- name: lambda
  symbol: '\lambda'
  role: rate
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: kappa
  symbol: '\kappa'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: dlomax
  sampler: rlomax
  control:
    lambda: Optional held lambda; NULL estimates it.
    kappa: Optional held kappa; NULL estimates it.
    lambda0: Optional starting value for `lambda`; `NULL` uses a method-of-moments start.
    reltol: "Requested relative coefficient accuracy for the local profile correction (default 1e-10); global bounds retain their separate tolerance."
    iterlim: "Work limit per solve: 1 * min(iterlim, 100000) solver iterations; per certificate: 128 * min(iterlim, 100000) certificate evaluations."
log_density: log(lambda) + log(kappa) - (kappa + 1)*log(1 + lambda*x)
limits:
  - model: exp
    firmness: proved
    when: '$\kappa$ tends to infinity while $\lambda\kappa$ tends to a positive exponential rate'
    detect: 'both coefficients are free and the global profile upper bounds certify that no improvement over the best exponential exceeds $10^{-8}n$ log-likelihood units'
    status: ExponentialLimit
    numerical: true
references:
- del Castillo, J. and Daoudi, J. (2009). Estimation of the generalized Pareto distribution. Statistics & Probability
  Letters 79(5), 684–688. doi:10.1016/j.spl.2008.10.021.
- Grimshaw, S. D. (1993). Computing maximum likelihood estimates for the generalized Pareto distribution. Technometrics
  35(2), 185–191. doi:10.1080/00401706.1993.10485040.
- Kleiber, C. and Kotz, S. (2003). Statistical Size Distributions in Economics and Actuarial Sciences. Wiley, p.
  60.
- 'Dutang, C., Goulet, V. and Pigeon, M. (2008). actuar: An R Package for Actuarial Science. Journal of Statistical
  Software, 25(7), 1–37. doi:10.18637/jss.v025.i07.'
examples: |
  set.seed(1)
  x <- rml(200, mllomax())
  mllomax(x)

group: Pareto and Lomax
construction:
- base
relations:
  special_case_of:
  - family: burr
    at:
      shape1: kappa
      shape2: 1
      scale: 1/lambda
  - family: pareto2
    at:
      shape: kappa
      scale: 1/lambda
      min: 0
  - family: betapr
    at:
      shape1: 1
      shape2: kappa
      scale: 1/lambda
fit:
  method: profile-search
  certified: 'yes'
  note: 'Scalar profile envelopes, analytic tail bounds and signed root isolation certify the global outcome conditional on the practical floating-point allowances below. Unresolved comparisons or budget exhaustion stop.'
areas:
- actuarial
- economics and income
classification: specialist
classification_reason: Used to model positive losses and waiting times with a Pareto tail from zero.
summary: Lomax provides a model for positive losses and waiting times with a Pareto tail from zero.
aliases:
- "Pareto type II (zero threshold)"
- "Pareto of the second kind (zero threshold)"
key_references:
- 4
existence:
  uniqueness: unknown
  statement: >-
    With both coefficients free, a finite MLE exists exactly when
    $\psi(u) \le 0$ for some $0 < u < \bar u$, with $\psi$ and $\bar u$
    defined below. Otherwise the supremum is the exponential limit.
    Joint uniqueness is not established. Holding either coefficient gives
    a unique finite MLE for every nonempty positive sample, including constant data.
  reason: >-
    Put $y_i=x_i/\bar x$, $q=\lambda\bar x$ and
    $A(q)=n^{-1}\sum_i\log(1+qy_i)$. Profiling gives $\hat\kappa=1/A(q)$
    and a log-likelihood gap from the best exponential of
    $n\{\log q-\log A(q)-A(q)\}$. This tends to zero as $q\to0$ and
    to $-\infty$ as $q\to\infty$. At $q=ue^u$ its sign is that of
    $-\psi(u)$. For $u\ge\bar u$, $\psi(u)>\log(u/\bar u)\ge0$.
    With $\lambda$ held the likelihood is strictly concave in $\kappa$.
    With $\kappa$ held the score in $\log\lambda$ strictly decreases
    from $n$ to $-n\kappa$.
---

The Lomax (Pareto type II) distribution models positive incomes, losses and waiting times with a heavy right tail.

## Details

The rate $\lambda$ sets the inverse scale, and the shape $\kappa$ controls the tail. Smaller $\kappa$ gives a heavier tail.

For the existence criterion, put $y_i=x_i/\bar x$, $\psi(u)=n^{-1}\sum_i\log(e^{-u}+uy_i)$ and $\bar u=\bar x/(\prod_i x_i)^{1/n}$. The finite MLE exists if and only if $\psi(u)\le0$ somewhere in $0<u<\bar u$. This follows by comparing the profiled likelihood with the best exponential likelihood, as shown above. It does not establish uniqueness. The fitter bounds the entire profile, including both tails, and isolates a winning stationary maximum as described below. The exponential stop uses a margin of $10^{-8}n$ log-likelihood units and can reject a mathematical MLE whose positive improvement is smaller than that margin.

The moment condition $\overline{x^2}<2\bar x^2$ decides only local behaviour at the exponential limit and does not rule out a finite MLE. For example, the sample $(1,100)$ has an MLE despite satisfying that condition. For two observations, the transition occurs at a larger-to-smaller ratio of approximately 28.81, obtained numerically; the criterion above determines whether the transition value itself is included.

Holding either coefficient removes the exponential escape and gives a unique finite fit, even for constant data. Holding both evaluates the likelihood at the supplied coefficients.

## R notes

The implementation prefers the exponential limit when the certified upper bound on every profile improvement is at most $10^{-8}n$ log-likelihood units and stops with a recommendation to use `mlexp`. This tolerance can reject a mathematical MLE that ties or only slightly improves on the limit. An unresolved comparison returns a numerical failure, never an unchecked fit.

With $\kappa$ held, the total log-rate score is $n-(\kappa+1)\sum_i\operatorname{logistic}(\log\lambda+\log x_i)$. It strictly decreases from $n$ to $-n\kappa$, so its unique root is the global conditional maximum. The solver expands a log-rate bracket within the representable positive range, verifies both signs, and requires absolute total score below $10^{-3}$ at the represented rate. Log probabilities avoid overflow and underflow in the weighted logistic terms. If the root cannot be represented or certified numerically, fitting returns `IterationFailure`.


## Global profile certificate

Write $z=\log q$, $A=\overline{\log(1+e^z y)}$,
$B=\overline{p}$ and $C=\overline{p(1-p)}$, where
$p_i=\operatorname{logistic}(z+\log y_i)$. The per-observation
profile gap is $h=z-\log A-A$, with

$$h'=1-B/A-B,\qquad h''=B^2/A^2-C/A-C.$$

Since $0<C\le B\le A$ and $C\le1/4$, $h''\ge-5/4$.
For small $q$, the sharper lower curvature bound
$h''\ge-qM_2/[2(1-qM_2/2)]-q$ holds when $qM_2<2$:
$A-B\le q^2M_2/2$, $B/A\le1$ and $C\le q$ give this inequality.
The implementation uses the better of these two lower bounds uniformly
over each interval, with the largest $q$ and enlarged $M_2$.
Thus, on $[a,b]$, the straight chord of the two endpoint gaps plus
$5(b-a)^2/32$ bounds the gap above. This bound requires no unimodality
assumption. A priority queue subdivides intervals until they are strictly
dominated by a certified finite witness, or their upper bound is below
$10^{-8}$. Intervals with a strictly signed score throughout can also be
removed: a global interior maximum of this differentiable profile must
have zero score, even at an artificial subdivision boundary. Monotonicity
of $A$ and $B$ bounds $h'$ between $1-B_b/A_a-B_b$ and
$1-B_a/A_b-B_a$, with the arithmetic allowances below. If the local
curvature upper bound is negative, a negative left score or positive
right score also excludes stationary points on that whole interval.
This derivative exclusion is needed near a shallow winner, where the
likelihood difference at a neighborhood edge can be smaller than the
floating-point allowance despite a well-resolved score. Candidate neighborhoods are removed only after a termwise bound
proves $h''<0$ throughout and signed endpoint scores isolate their unique
stationary maximum. Overlapping neighborhoods allow isolation when a
root lies on a subdivision boundary with an uncertain score sign. A narrow signed bracket of width below $7\times10^{-7}$
in $z$ encloses both coefficients to that relative accuracy, since
$d\log A/dz=B/A\le1$. Newton refines the returned center using the analytic profile score and curvature. The relative rate correction and the induced log-kappa correction must both be below one quarter of the requested tolerance, with contraction across the last corrections or corrections at rounding level. A signed local bracket can instead establish both coefficient bounds when arithmetic noise prevents observable contraction. The local refinement runs after selection of the globally certified basin.
Thus positive subnormal rates are accepted only when their relative
precision is adequate. A new winner must strictly
dominate any previously isolated winner; otherwise subdivision stops with
a budget or arithmetic failure. No general joint uniqueness is claimed.


For local arithmetic put $p_i=\lambda x_i/(1+\lambda x_i)$,
$S=\overline{\log(1+\lambda x)}$, $B=\bar p$, $D=\overline{p^2}$,
$T=\overline{\log(1+\lambda x)-p}$ and
$K=\overline{p^2-2\{\log(1+\lambda x)-p\}}$. The scaled profile score and
curvature are $\lambda f=T/S-B$ and
$\lambda^2 f'=K/S+D+(T/S)^2$. For $p\le1/2$, positive power series give
$T_i=\sum_{j\ge2}p_i^j/j$ and $K_i=-2\sum_{j\ge3}p_i^j/j$.
The degree-64 remainder for $T_i$ is at most
$p_i^{65}/\{65(1-p_i)\}$; the $K_i$ bound is twice this.
These forms avoid cancellation of inverse-rate terms near a weak maximum.

For a final local bracket, $S,B,D$ increase with rate, so
$B_h^2/S_l^2+D_h/S_l+D_h-1$ bounds the scaled curvature from above.
A negative bound and separately signed endpoint scores isolate one maximum;
the actual rate and reciprocal-$S$ endpoint errors must both fit the requested
tolerance. Practical margins are $8\epsilon(|T/S|+|B|)$ for local signs and
$32\epsilon$ times the absolute curvature-bound contributions. Mean
normalization sums ratios to the sample maximum, avoiding raw-sum overflow.

Let $M_2=\overline{y^2}$ and $L=-\overline{\log y}$. For the left tail,
$A\ge q-q^2M_2/2$ gives $h\le-\log(1-qM_2/2)$ when $qM_2<2$.
The cutoff is $q=10^{-10}/\max(1,M_2)$, so its gap bound is below
$5.1\times10^{-11}$ before arithmetic allowances. For the right tail,
$A\ge z-L$ gives $h\le L-\log(z-L)$ when $z>L$; hence $z\ge L+e^L$
is dominated by the exponential. These are bounds on whole tails, not
numerical search caps. A finite witness must beat the left tail and the
exponential margin strictly. The exponential outcome is permitted only
when the left-tail allowance is also below that margin.

The implementation in `certificate.hpp` is certified in exact arithmetic
conditional on the computed comparisons. It does not use outward rounding.
Its practical binary64 allowance is
$\eta=256\epsilon(1+|\log x_{\max}|+|\log\overline{x/x_{\max}}|+\max_i|\log y_i|)
(1+n\epsilon)$, with $\epsilon=2^{-52}$. Compensated sums and exact duplicate
compression are used throughout the certificate. A gap evaluation receives
absolute allowance $\eta(1+|z|+|\log A|+A)$; positive moments receive relative
allowance $\eta$, score signs require $4\eta$, curvature signs require
$4\eta$, and the left-tail bound receives $4\eta$. These allowances assume
ordinary binary64 arithmetic and elementary functions accurate within
$8\epsilon\max(1,|f|)$, with relative accuracy for positive exp/log1p results before underflow,
and underflow error below the displayed absolute
allowances. They are practical margins, not a universal libm proof; certification is conditional
on their enclosing the exact normalized moments, scores and gaps.
Small-$q$ gap evaluation accumulates $A/q-1$ directly; its fourth-order
logarithm-ratio expansion below $qy=10^{-4}$ has remainder at most
$(qy)^5/6$. All nonfinite evaluations, subnormal intermediate ratios $x_i/x_{\max}$,
unrepresentable certificate coefficients, inadequate root isolation and exhausted
budgets stop with `IterationFailure`. A failed final coefficient-reconstruction
check reports `AccuracyFailure`. Constant samples have an exact
exponential outcome.

The subdivision budget is $128\min(\mathtt{maxit},100000)$ profile
evaluations; each root-isolation attempt has at most `maxit` bisections
and requires at least two successful refinement passes.
`iterations` records global profile evaluations for the all-free fit.
A starting hint can nominate an extra subdivision but cannot exclude any
part of the parameter space. The certificate uses all distinct observations
with their exact multiplicities; no binned approximation decides the fit.

The 2026-11-04 replacement removes the former log-grid and binned candidate
search from the decision path. A grid and a small score residual cannot
exclude an unseen maximum. Using the universal curvature bound alone also
proved too costly near the flat exponential endpoint; the small-rate bound
above is retained for that reason. Duplicate compression preserves exact
multiplicities and is distinct from the former log-bin approximation.

The `limits.numerical` flag is retained for the tolerance-aware R help
wording: the generic non-numerical template would assert mathematical
non-existence even when a finite MLE wins by less than the declared margin.
The flag does not weaken the global certificate in `fit.certified`.

With kappa held, writing the log-rate score as a count contribution plus signed logistic tails avoids cancellation when probabilities round to one. The count contribution is evaluated by fused multiply-add, so a held kappa near a ratio of integer counts is retained. The remaining terms share a logarithmic scale and use compensated summation. The derivative magnitude at a root is at least half the sum of the unsigned tail terms; this controls the error after scaling. Signs use a practical margin of eight machine epsilons times the logarithmic magnitudes and unsigned sum. An independently signed log-rate enclosure remains valid while a final raw-sign polish selects a point inside it. The physical rate is checked after exponentiation and normalization.
