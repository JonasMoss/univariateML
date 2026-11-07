---
id: gpd
inference_numeric:
  - regime: "positive shape fixed support"
    available: true
    condition: "finite mu held, sigma > 0 and xi > 0; sigma and/or xi free"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#gpd-and-gev"
  - regime: "exponential member with shape held"
    available: true
    condition: "finite mu held, sigma > 0 free and xi = 0 held"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#gpd-and-gev"
  - regime: "known threshold regular model"
    available: true
    condition: "finite mu held, sigma > 0 and xi > -1/2; sigma and/or xi free, including xi = 0 with xi free; excluding the preceding available submodels"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#gpd-and-gev"
  - regime: "critical upper endpoint"
    available: false
    condition: "finite mu held, sigma > 0 and xi = -1/2; sigma and/or xi free"
    reason: "the endpoint has a sqrt(n log n) Gaussian experiment and its complement has root-n rate; ordinary finite Fisher inference is unavailable"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#gpd-and-gev"
  - regime: "Poisson upper endpoint"
    available: false
    condition: "finite mu held, sigma > 0 and -1 <= xi < -1/2; sigma and/or xi free"
    reason: "the endpoint has a non-Gaussian Poisson experiment at rate n^(-xi); both free coordinates also permit a root-n shape tangent; free xi = -1 adds a fitted-bound constraint"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#gpd-and-gev"
  - regime: "unsupported free threshold"
    available: false
    condition: "mu free"
    reason: "the fitter does not estimate the threshold; known-threshold regularity does not justify this subset"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#gpd-and-gev"
name: Generalized Pareto
support: {type: real, lower: "mu", upper: "if (xi < 0) mu - sigma/xi else Inf", closed: [true, true], endpoint_log_density: {lower: "-log(sigma)", upper: "if (xi == -1) -log(sigma) else if (xi < -1) Inf else -Inf"}}
params:
- name: mu
  symbol: '\mu'
  role: location
  domain: (-inf, inf)
  simulate: 0
  kind: design
  advice: 'Supply a finite held mu. With sigma and xi free, threshold ties give unbounded likelihood, so choose mu strictly below the data. Estimating mu is not supported, including when other coefficients are held.'
- name: sigma
  symbol: '\sigma'
  role: scale
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: xi
  symbol: '\xi'
  role: shape
  domain: '(-inf, inf)'
  simulate: 0
  fit_domain:
    lower: -1.0
    restriction:
      mechanism: endpoint
      exempt_when_held: []
      reason: For xi below -1, moving the upper endpoint onto the sample maximum makes the likelihood unbounded when scale is free. The bound xi >= -1 removes this singularity.
  kind: regular
r:
  density: dgpd
  sampler: rgpd
  control:
    mu: Threshold, held fixed; every observation must be at or above it.
    sigma: Positive scale to hold fixed, or NULL to estimate.
    xi: Shape to hold fixed, or NULL to estimate (lower bound -1).
    reltol: Requested relative tolerance for scale bisection and profile representative refinement; global certification uses a fixed coefficient enclosure and arithmetic margin.
    iterlim: Maximum bracketing passes per root; global subdivision budget is 128 times min(iterlim, 100000).
log_density: Piecewise((-log(sigma)-(x-mu)/sigma, Eq(xi,0)), (-log(sigma)-(1+1/xi)*log(1+xi*(x-mu)/sigma), True))
limits:
  - firmness: proved
    name: threshold scale collapse
    fixed: [mu]
    status: InvalidData
    message: "scale collapse at the threshold"
    when: 'Scale $\sigma$ tends to zero with $\mu$ held. For held $\xi>0$ the likelihood has order $\sigma^{(n-k)/\xi-k}$, where $k$ counts threshold ties.'
    detect: 'Scale is free and either all observations equal $\mu$, or $k>0$ and shape is free, or held $\xi>0$ satisfies $\xi k\ge n-k$. Equality gives a finite unattained supremum for the fixed-shape scale fit.'
references:
- Grimshaw, S. D. (1993). Computing maximum likelihood estimates for the generalized Pareto distribution. Technometrics,
  35(2), 185–191. doi:10.1080/00401706.1993.10485040.
- 'Wolodzko, T. and Klinke, S. (2026). extraDistr: Additional Univariate and Multivariate Distributions. R package
  reference manual, version 1.10.0.5. https://cran.r-project.org/web/packages/extraDistr/extraDistr.pdf.'
holds:
- held: []
  status: no-mle
  reason: 'Estimating `mu` is not supported. The unrestricted likelihood is unbounded. Hold mu to fit this family; some held-coefficient submodels have endpoint maxima or uniform plateaus.'
- held:
  - xi
  status: not-implemented
  reason: 'Estimating `mu` is not supported. The unrestricted likelihood is unbounded. Hold mu to fit this family; some held-coefficient submodels have endpoint maxima or uniform plateaus.'
- held: [sigma]
  status: not-implemented
  reason: 'Estimating `mu` is not supported. The unrestricted likelihood is unbounded. Hold mu to fit this family; some held-coefficient submodels have endpoint maxima or uniform plateaus.'
- held: [sigma, xi]
  status: not-implemented
  reason: 'Estimating `mu` is not supported. The unrestricted likelihood is unbounded. Hold mu to fit this family; some held-coefficient submodels have endpoint maxima or uniform plateaus.'
fit_args:
  mu: 0.0
examples: |
  x <- datasets::precip[datasets::precip > 40] - 40
  mlgpd(x, mu = 0)

group: extreme value
construction:
- base
- generalisation
relations: {}
fit:
  method: profile-search
  certified: 'yes'
  note: 'gpd/README.md, Details and Certificate; gpd/certificate.hpp: derivative enclosures exclude all competing stationary maxima, with analytic tail exclusions. Held shape uses a monotone scale score; held scale uses the same certificate or the exact uniform density bound. Comparisons are conditional on the stated binary64 allowance; unresolved cases stop.'
areas:
- extremes and environment
classification: specialist
classification_reason: Used to model threshold exceedances in extreme-value analysis.
summary: Generalized Pareto provides a model for threshold exceedances in extreme-value analysis.
aliases:
- "GPD"
key_references:
- 2
existence:
  uniqueness: unknown
  statement: >-
    With all coefficients free the likelihood is unbounded. Fits hold $\mu$
    and estimate shape within $\xi\ge-1$. With scale and shape free,
    any observation at $\mu$ still makes the likelihood unbounded.
    For strictly positive excesses a finite maximum exists within this
    shape range. The joint maximizer need not be unique: the three-observation construction below ties an interior member with the attained uniform boundary. The fit certifies all profile competitors against the attained uniform boundary, or stops.
    With shape held at $\xi>-1$, a unique scale maximum exists unless threshold
    ties permit scale collapse. At $\xi=-1$, the scale maximum is the largest
    positive excess. Holding scale gives a finite shape maximum for every
    nonempty finite sample at or above the held threshold. It need not be
    unique, since all-zero excesses give a flat shape likelihood.
    With scale held and threshold free, a finite maximum exists within
    $\xi\ge-1$ for every nonempty finite sample, but these fits are not supported.
    Free-threshold subsets are not supported even when finite maxima exist.
    Holding all coefficients leaves likelihood evaluation.
  reason: >-
    With $\xi<-1$, moving the upper endpoint onto the sample maximum creates
    a density singularity. For $\xi\ge-1$ and positive excesses, each $y>0$
    has density at most $g(\xi)/y$, where $g(\xi)=(1+\xi)^{-1-1/\xi}$,
    $g(0)=e^{-1}$ and $g(-1)=1$. This excludes $\xi\to\infty$ from
    maximizing sequences. On bounded shape intervals the likelihood tends
    to zero at both scale ends. The closed uniform member gives upper
    semicontinuity at $\xi=-1$, so a maximum is attained on a compact set.
    For $k$ threshold ties and held $\xi>0$,
    scale collapse gives likelihood of order $\sigma^{(n-k)/\xi-k}$.
    At equality the scaled score is strictly negative for every positive scale,
    so the finite path limit is unattained. For $\xi>-1$, the scale score
    multiplied by $\sigma$ is
    $(1+\xi)\sum_i y_i/(\sigma+\xi y_i)-n$, where $y_i=x_i-\mu$.
    It strictly decreases if any excess is positive and crosses zero except
    in the stated collapse cases. For $\xi=-1$, likelihood is $\sigma^{-n}$
    on $\sigma\ge\max_i y_i$.
    With scale held and threshold free within $\xi\ge-1$, either a uniform member attains
    the density bound $\sigma^{-n}$, or threshold profiles to the sample
    minimum and the shape likelihood vanishes at both ends of its feasible interval.
---

The generalized Pareto distribution models exceedances above a chosen threshold $\mu$. Positive shape $\xi$ gives a heavy right tail, zero shape gives an exponential distribution with scale $\sigma$, and negative shape gives a finite upper endpoint $\mu-\sigma/\xi$.

## Details

Observations must be finite and at or above the held threshold. For negative shape they must also satisfy the upper support bound. At $\xi=-1$ the model is uniform on the closed interval $[\mu,\mu+\sigma]$, so the largest observation can equal the upper endpoint with positive density.

Without a shape restriction, approaching the sample maximum with an upper endpoint at $\xi<-1$ makes the likelihood unbounded when scale is free. Fitting therefore estimates shape within $\xi\ge-1$. A fully held model can still be evaluated outside this fitting range. Data generated below the bound can give constrained fits at $\xi=-1$. This existence restriction does not imply regular inference; the usual regular MLE asymptotics do not apply on $[-1,-1/2)$.

For strictly positive excesses, a finite MLE exists within $\xi\ge-1$. Maximizing the density of one observation $y>0$ over scale gives the envelope $g(\xi)/y$, with $g$ defined above. Since $g(\xi)\to0$ as $\xi\to\infty$, a maximizing sequence has bounded shape. The density is also at most $1/\sigma$, excluding arbitrarily large scales. For bounded shapes it tends uniformly to zero as scale vanishes. This follows from exponential decay for feasible nonpositive shapes, and power bounds on positive-shape intervals, including the exponential limit at zero. The closed uniform endpoint makes the likelihood upper semicontinuous at the shape floor, so the remaining compact set contains a maximum. The certificate below supplies the separate algorithmic global argument.

For $k$ observations at the held threshold, each density factor equals $1/\sigma$. For a fixed $\xi>0$, every positive excess $d$ contributes asymptotically $\xi^{-1-1/\xi}d^{-1-1/\xi}\sigma^{1/\xi}$. Thus the likelihood has order $\sigma^{(n-k)/\xi-k}$. It diverges if $\xi k>n-k$. At equality its positive finite limit is the supremum for that fixed-shape scale problem: the scaled scale score is negative everywhere. If every observation equals the threshold, likelihood diverges as $\sigma^{-n}$. With both shape and scale free, any threshold tie allows a strictly negative exponent by choosing $\xi>(n-k)/k$.

For held shape $\xi>-1$ and at least one positive excess, a strictly decreasing scaled scale score gives a unique root whenever collapse is excluded. At shape zero this is the mean excess. At $\xi=-1$ the uniform maximum is attained at the largest excess, provided it is positive. Constant positive excesses and single positive observations are allowed. Holding scale gives a scalar shape likelihood over its support-feasible interval within $\xi\ge-1$. If $\sigma\ge M=\max_i(x_i-\mu)>0$, the uniform member is the unique maximum: every density factor is at most $1/\sigma$, and any positive excess makes this inequality strict for $\xi>-1$. If $\sigma<M$, the global scalar certificate below applies. All-zero excesses with held scale give a flat shape likelihood and stop with a structured tie.

## Certificate

Write $v_i=(x_i-\mu)/M$, $b=\xi M/\sigma$, $t=\log(1+b)$ and

$$
 D(b)=\overline{\frac{\log(1+bv)}b}
     =\overline{\int_0^v\frac{ds}{1+bs}},\quad
 P=-D'=\overline{\int_0^v\frac{s\,ds}{(1+bs)^2}},\quad
 R=D''=2\overline{\int_0^v\frac{s^2\,ds}{(1+bs)^3}}.
$$

All three functions are positive and decreasing on $b>-1$ when any excess is positive. These integral formulas include $b=0$ without a singularity. Joint fitting at fixed nonzero $b$ has a unique conditional maximum at $\xi=bD$, $\sigma=MD$, whenever $bD\ge-1$. Its mean log likelihood, less $-\log M$, is

$$
 h=-\log D-bD-1,\quad
 h_b=P/D-D+bP,\quad
 h_{bb}=P^2/D^2-R/D+2P-bR.
$$

At $b=0$ these expressions give the fitted exponential member and its continuous profile derivatives. Thus shape zero is an interior point of this search, not a competing limit face. If $b<0$ and $bD<-1$, the constrained conditional optimum is $\xi=-1$, with $\sigma=-M/b\ge M$; it cannot beat the attained uniform fit $(\sigma,\xi)=(M,-1)$.

For held scale put $c=\sigma/M$. Apart from the constant $-\log\sigma$ the mean log likelihood is

$$
 h=-(1/c+b)D,\quad h_b=-D+(1/c+b)P,\quad
 h_{bb}=2P-(1/c+b)R.
$$

The numerical held-scale search is needed only for $0<c<1$; its feasible lower endpoint is $b=-1$ and is unattained. In either search,
$h_t=e^t h_b$ and $h_{tt}=e^{2t}h_{bb}+e^t h_b$.
Monotonicity of $D,P,R$ encloses them on each interval using endpoint values. Ordinary interval operations on the displayed expressions give bounds for the score and curvature. If $h_{tt}\ge-K$, the endpoint chord plus $K(t_1-t_0)^2/8$ bounds the entire interval above. This follows by subtracting the chord and comparing with the parabola $K(t-t_0)(t_1-t)/2$.

The tails are excluded analytically:

- Joint negative tail: $A=-bD$ increases as $t$ decreases. For $A\le1$, $h=\log(-b)-\log A+A-1\le-\log A+A-1$, and this bound decreases as $A$ increases. Thus the entire tail below a finite left endpoint has upper bound $-\log A_0+A_0-1$, where $A_0$ is a lower bound for $A$ there. Once $A>1$, all smaller $t$ have the constrained uniform conditional optima just described. The omitted tail must be dominated by the winning candidate; failure of that comparison stops.
- Joint positive tail: put $m=\min_i v_i>0$, $L=bD$ and $Q=\overline{(1+bv)^{-1}}$. The sign of $h_t$ equals that of $(1+L)Q-1$. Since $L\le t$ and $Q\le(1+bm)^{-1}$, the score is negative when $bm>t$. For $t\ge1$ that inequality persists as $t$ increases.
- Held-scale positive tail: using the smallest positive $v_i=m$, the score is negative if $cb>2t$ and $bm>1$. The factor equal to the fraction of positive excesses cancels from this sufficient inequality, so threshold ties are allowed. Both inequalities persist beyond such a $t\ge1$.
- Held-scale negative tail: let $w$ be the fraction of observations equal to $M$. For $t<-1$, the score is positive if $(1-c)(1-e^t)w>e^t(-t)$. To see this, use $A=-\overline{\log(1+bv)}\le-t$ and $B=\overline{e^tv/(1-v+e^tv)}\ge w$ in $h_t=(1/c+b)B/(-b)-e^t A/(cb^2)$. This sufficient inequality persists as $t$ decreases.

Positive-tail and held-scale negative-tail endpoint proposals are doubled until their comparisons pass. The joint negative tail uses $t=-32$, shortened to the $A=1$ crossing when available, and requires its analytic likelihood bound to lose. Failure to establish the required tail comparisons by $t=-32$ or $t=64$ is a numerical stop, never a returned estimate at a cap. These practical arithmetic guards bound derivative magnitudes; they are not assertions that every mathematical maximum lies within those coordinates.

The priority queue discards an interval only if its likelihood upper bound is below a verified candidate, its score excludes zero throughout, or strict negative curvature and an endpoint score exclude a local maximum. Interior global maxima must be stationary, including at artificial subdivision boundaries. A retained maximum requires strictly negative curvature throughout a neighborhood and oppositely signed endpoint scores. Overlapping neighborhoods handle roots at subdivision boundaries. Each retained neighborhood has exactly one stationary maximum. Its signed root enclosure is narrower than $2\,10^{-7}$ in $t$, or directly encloses scale to relative width $3\,10^{-7}$ and shape to width $3\,10^{-7}\max(1,|\xi|)$. The direct coefficient check avoids needlessly tight chart tolerances near a support endpoint. Newton refinement nominates a more accurate representative inside the signed enclosure. Every other interval must be excluded before returning. Distinct retained maxima and the uniform boundary are compared with a $10^{-8}$ mean-log-likelihood separation margin; unresolved ties stop. This certifies the global maximizer in exact arithmetic conditional on the computed enclosures and comparisons. It does not assume uniqueness; the construction below disproves universal joint uniqueness.

### Joint maxima need not be unique

Universal uniqueness fails even with a held threshold strictly below every observation. This can be proved without numerical root finding. For $k\in(-1,\infty)$ define

$$
 b(k)=k e^{1+k},\qquad
 a(k)=\frac{\exp\{[3k-\log(1+b(k))]/2\}-1}{b(k)},
 \qquad a(0)=\frac{3/e-1}{2}.
$$

The function $a$ is continuous, tends to $-\infty$ as $k\downarrow-1$ and to zero as $k\to\infty$. Moreover $a(k)<1$ for every $k$. For $k>0$ the latter inequality is equivalent to $k<\log(1+b(k))$, which follows from $e^k-1<k e^{1+k}$. For $k=-r\in(-1,0)$ it is equivalent to $e^r<1+er$, which follows from the convex chord bound $e^r\le1+(e-1)r$. At zero,

$$
 a'(0)=\frac{3(e^2-2e-1)}{8e}>0.
$$

Consequently $a_* = \max_{k>-1}a(k)$ is attained, satisfies $0<a_*<1$, and exceeds $a(0)$. Choose any maximizing $k_*$, which is finite, greater than $-1$, and nonzero. Consider the strictly positive excesses $(a_*,a_*,1)$.

For any $b>-1$, the admissible joint profile above has $h=-\log D-bD-1$. On its admissible range $1+bD>0$, it decreases strictly with $D$, since $h_D=-(1+bD)/D$. At fixed $b$, $D$ increases strictly with the smaller observation $a$, with derivative $2/[3(1+ba)]$. The equation $h=0$ gives $D=e^{-1-k}$ and $b=k e^{1+k}$; substituting the two-point sample into $bD=\{2\log(1+ba)+\log(1+b)\}/3$ gives exactly $a=a(k)$. The map $k\mapsto b(k)$ is strictly increasing from $-1$ to infinity. When $a(k)<0$, this comparison uses the same algebraic function on the interval from $a(k)$ to $a_*$; its logarithm stays defined because $1+ba(k)>0$. Therefore $a_*\ge a(k)$ makes every admissible profile value nonpositive. If $bD\le-1$, the constrained uniform conditional fit is already below the uniform maximum. At $b=0$ the same conclusion follows from $a_*>a(0)$.

At $k=k_*$ equality is attained by $(\sigma,\xi)=(e^{-1-k_*},k_*)$, whose total log likelihood is zero. The uniform member $(\sigma,\xi)=(1,-1)$ also has total log likelihood zero. Both are global maximizers for the same strictly positive sample and are distinct. This is an exact existence construction of nonuniqueness; it does not require that the scalar maximizer $k_*$ itself be unique. The [retained check](../../../../../dev/research/evaluations/2026-11-05-gpd-cert-data/check_uniqueness.py) numerically illustrates the transition and checks the fitter's margin-aware tie stop. Its decimal constants are numerical approximations, separate from the exact max-defined counterexample. The illustrated values are $a\approx0.07880675592383729$, $\xi\approx0.5808198884419236$ and $\sigma\approx0.2058062908136120$. Perturbing the smaller excesses by $-10^{-5}$ selects the interior maximum; perturbing by $+10^{-5}$ selects the uniform member. The binary64 approximation to the critical sample gives a structured tie stop.

### Arithmetic and failures

The practical binary64 allowance is

$$
 \eta=512\epsilon\{1+\log(n+1)\},\qquad
 \epsilon=2^{-52}.
$$

All derivative comparisons use dimensionless max-excess units, so this allowance is independent of physical rescaling. Compensated means accumulate $D,P,R$. Their positive values receive relative allowance $\eta$; point likelihoods receive allowance $\eta(1+|h|+|\log D|+|bD|)$. Score and curvature enclosures are widened by $16\eta(1+\max|h_t|)$ and $32\eta(1+\max|h_{tt}|)$, respectively. Signed root decisions require $32\eta(1+\max|t|)$ and strictly negative curvature with the stated margin. The contract assumes ordinary binary64 arithmetic and elementary-function errors covered by these allowances; it is a practical conditional certificate, not outward-rounded interval arithmetic or a universal libm error theorem.

Near $b=0$, forty terms of the convergent integral series at $|bv|<1/4$ avoid cancellation; the relative remainders for $D,P,R$ are below $10^{-20}$. Near the negative support edge the denominator is evaluated as $(M-y)/M+(y/M)e^t$ to preserve the endpoint distance. Invalid normalization, subnormal normalized positive excesses, nonfinite comparisons, inadequate physical-coefficient restoration, or exhausted tail checks stop. The stored coefficients must reproduce the certified mean likelihood within $10^{-8}$; physical scale restoration is checked as well. Root enclosures, refinement, and restoration give practical coefficient accuracy of approximately $10^{-6}$ away from ill-conditioned coordinate seams. Relative accuracy of shape arbitrarily close to zero is not claimed.

With shape held, the strictly decreasing scaled scale score is bracketed in units of $M$. The exponential and uniform subsets use exact formulas. Other held shapes require signed brackets; ambiguous signs are widened to a checked relative coefficient enclosure or stop. Invalid and unrepresentable results stop. The physical restoration check also applies to the exponential mean: a positive but coarsely rounded subnormal mean stops, while exactly representable subnormal means remain valid. The user tolerance controls refinement of the representative; it cannot weaken the certificate's coefficient check. After a signed enclosure is established, at most 48 further bisections nominate the held-shape scale representative. Joint and held-scale representatives receive at most 12 Newton steps inside their signed enclosures. These bounded proposal refinements never substitute for a certificate.

Budget, arithmetic, tie and threshold-collapse outcomes carry `core/stop_detail.hpp` records, with candidate coefficients and global likelihood bounds when available. The fixed-positive-shape threshold-collapse equality records its finite unattained limiting log likelihood separately from unbounded cases. No failed solve returns its last iterate. The global budget is $128\min(\texttt{iterlim},100000)$ subdivision evaluations; root refinements are separately bounded by `iterlim`. Closed-form held subsets need no iterative budget. A fully held model remains likelihood evaluation, including shapes below the fitting restriction.

A free threshold is excluded by interface policy for every held subset. This is stronger than a mathematical assertion of non-existence. For example, with exponential shape and scale held, the likelihood increases to a finite maximum at the sample minimum. With uniform shape and scale held, compatible thresholds form an attained plateau. The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-extreme.md) records the limiting paths; its older universal free-threshold claim does not cover these closed-support submodels.

With scale held, estimating the threshold still permits a finite maximum within $\xi\ge-1$. Let $R=\max_i x_i-\min_i x_i$. If $\sigma\ge R$, a uniform member covers the sample and attains the joint density bound $\sigma^{-n}$. If $0<\sigma<R$, support feasibility requires $\xi\ge-\sigma/R>-1$. The density then increases as the threshold rises to the sample minimum. At that threshold the shape likelihood is continuous and positive on $(-\sigma/R,\infty)$ and tends to zero at both ends, so a finite maximum exists. This argument does not prove uniqueness, and the interface still requires a held threshold.

## R notes

Supply `mu` explicitly. With shape and scale free, choose it strictly below the data to avoid threshold collapse. All three coefficients may be held; incompatible support is rejected. Estimated coefficients on an applicable `fit_domain` bound carry `restricted` and emit `univariateML_restricted` warnings. Held coefficients are not flagged. A successful fit requires the global certificate; a small optimizer score alone never overrides certificate failure. Structured stops distinguish budget, numerical, tie and threshold-collapse outcomes.

## Algorithm history

On 2026-11-05 (TASK-116.2), the 32-cell score scan with local-extremum refinement was replaced by the interval certificate. The earlier tail bounds were useful, but same-sign samples and local minimization of the score did not establish that every root pair was found. Held-scale optimization on the fixed transformed interval [-32,32] had the same coverage gap. The previous implementation remains reproducible from base revision `fe4573e9b02a950a1440039a8395c30ff2f25926`; the retained [validation reproducer](../../../../../dev/research/evaluations/2026-11-05-gpd-cert-data/check_certificate.py) builds it alongside the replacement for timing and regression comparisons.
