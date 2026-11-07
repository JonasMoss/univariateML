---
id: beta
inference_numeric:
  - regime: interior
    available: true
    condition: "shape1 > 0 and shape2 > 0 finite; either or both free"
    classification: "dev/roadmap/2026-10-30-inference-and-dqm.md#standard-models"
  - regime: boundary or limit
    available: false
    condition: "a shape at zero or a nonfinite limit"
    reason: "beta boundary and limit models are outside the interior derivative regime"
    classification: "dev/roadmap/2026-10-30-inference-and-dqm.md#standard-models"
name: Beta
support: {type: real, lower: 0, upper: 1, closed: [false, false]}
params:
- name: shape1
  symbol: '\alpha'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: shape2
  symbol: '\beta'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: stats::dbeta
  sampler: stats::rbeta
  control:
    shape1: Optional held shape1; `NULL` estimates it.
    shape2: Optional held shape2; `NULL` estimates it.
    reltol: Requested relative accuracy of the free shapes (floored at `1e-14`).
    iterlim: "Work limit per solve: 1 * min(iterlim, 100000) solver iterations."
log_density: (shape1-1)*log(x) + (shape2-1)*log(1-x) - loggamma(shape1) - loggamma(shape2) + loggamma(shape1+shape2)
references:
  - "Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995). Continuous Univariate Distributions, Volume 2, Chapter 25. Wiley, New York."
holds: []
limits:
- name: point-mass collapse
  firmness: proved
  when: Both shapes grow as $\alpha=tc$ and $\beta=t(1-c)$ with $t\to\infty$ and $0<c<1$
  detect: All observations equal the same value $c$ and both shapes are free
  status: Degenerate
  message: does not exist
examples: |
  set.seed(1)
  x <- rml(200, mlbeta())
  mlbeta(x)

summary: The beta distribution models continuous proportions on the unit interval.
classification: classical
classification_reason: A standard model for continuous proportions with two independently adjustable boundary shapes.
aliases:
- "beta of the first kind"
key_references:
- 1

group: beta and generalised beta
construction:
- base
relations:
  special_case_of:
  - family: genbeta
    at:
      shape1: shape1
      shape2: shape2
      shape3: 1
      scale: 1
  - family: nsbeta
    at:
      shape1: shape1
      shape2: shape2
      min: 0
      max: 1
fit:
  method: scalar-solve
  certified: 'yes'
  note: 'beta/README.md, Details; beta/mle.hpp (public fitter and held overloads); beta/test.cpp: Strictly concave
    beta likelihood in its two shapes; ray scores decrease strictly and the mean profile is unimodal; held-shape
    digamma scores decrease strictly. nsbeta uses supplied endpoints; unsupported endpoint fits stop.'
areas:
- proportions
existence:
  uniqueness: proved
  statement: >-
    For a nonempty sample strictly between zero and one, the MLE with both
    shapes free exists if and only if the observations are not all equal,
    and it is then unique. With either finite positive shape held, the
    remaining shape has a unique finite MLE even for constant data.
    With both shapes held, the likelihood is evaluated at those values.
  reason: >-
    The beta family is a minimal regular exponential family with natural
    parameter $(\alpha-1,\beta-1)$ and statistic $(\log x,\log(1-x))$.
    Its log likelihood is strictly concave. The interior of the convex support
    is $e^u+e^v<1$. Strict Jensen inequalities put the mean statistic there
    exactly for nonconstant samples, giving existence of the finite maximum.
    For constant data $x=c$, the path $(\alpha,\beta)=(tc,t(1-c))$ has
    density at $c$ growing as $\sqrt{t}$ by Stirling's formula.
    With one shape $h>0$ held, the free-shape score per observation is
    $a+\psi(z+h)-\psi(z)$, where $a<0$ is the corresponding mean log observation.
    Its derivative is $\psi_1(z+h)-\psi_1(z)<0$, and its limits are
    positive infinity at zero and $a<0$ at infinity.

---

The beta distribution models proportions and other continuous quantities strictly between zero and one.

## Details

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-bounded.md) derives these boundary conditions, including the relevant held parameters.

The shapes $\alpha$ and $\beta$ control behavior near zero and one,
respectively. Values below one allow density to increase towards the
corresponding endpoint. Their ratio determines the center, while their
sum controls concentration at a fixed ratio.

With both shapes free, nonconstant data have a unique finite maximum.
The fit works in mean and precision, $\alpha=\mu t$ and $\beta=(1-\mu)t$.
With $R(z,h)=\psi(z+h)-\psi(z)-\log(1+h/z)$ and the binary Kullback–Leibler
divergence $\mathrm{KL}(\mu\Vert x)$, the scores are

$$
\begin{aligned}
n^{-1}\partial_t\ell &= -\overline{\mathrm{KL}(\mu\Vert x)}+\mu R(\alpha,\beta)+(1-\mu)R(\beta,\alpha),\\
(nt)^{-1}\partial_\mu\ell &= \overline{\operatorname{logit}x}-\operatorname{logit}\mu+R(\alpha,\beta)-R(\beta,\alpha),
\end{aligned}
$$

where the logit difference is averaged per observation with `log1p`. The
leading terms cancel analytically, so the scores keep their information for
nearly constant data and for data near zero or one. Along a ray of fixed
$\mu$ the likelihood is concave in $t$, so its score has one root. The
superlevel sets of a concave function are convex, so the profile over $\mu$
is unimodal. Both levels are bracketed, safeguarded Newton root-finds in
$\log t$ and $\operatorname{logit}\mu$, with exact profile derivatives;
starts affect only speed. The outer bracket width is chosen so that both
shapes reach relative accuracy `reltol`, including the quadratic
sensitivity of $t$ to $\mu$ for concentrated data. The log-likelihood uses
the same coordinates and Stirling remainders. Exhausted solves report
numerical failure. Constant data have unbounded likelihood as both shapes
increase at a fixed ratio.

With either finite positive shape held, the remaining shape has a unique
finite maximum even for constant data. Its strictly decreasing digamma
score is solved by bracketing. With both shapes held, the likelihood is
evaluated at the supplied values. Observations at zero or one are excluded
in every case.

## Decision history

### Diagnostic status contract (2026-10-31)

Empty or invalid input carries a non-success status. Degenerate samples depend
on which coefficients are free; delegate failures survive transformations.
See the [shared diagnostic contract](../../../../../dev/validation/diagnostics.md)
and the appended TASK-132 cases in `test.cpp` for the validation scope.

For one held shape $h$, the free-shape bracket starts at $z=\max(1,2h/T)$, with $T$ the negative mean log observation in the corresponding coordinate, subject to representability. The bound $\psi'(z)\le z^{-1}+z^{-2}$ implies $\psi(z+h)-\psi(z)\le2h/z$ for $z\ge1$. Thus tiny log complements retain a signed bracket without requiring hundreds of doubling passes; computed endpoint signs still decide acceptance.

### Free fit rebuilt in mean and precision (2026-11-07, TASK-148)

The inherited free fit solved a Newton iteration on $\log\beta$ with three
fixed digamma-inversion steps for $\alpha$, using $\log(1-x)$ and direct
digamma differences. It returned status `Ok` with wrong estimates on data near
zero ($\alpha$ 43% off at $x\approx10^{-17}$, where `1 - x` rounds to one) and
on nearly constant data, where concentration is a second-order difference of
mean logs. Its goldens were about $2\times10^{-9}$ off in $\alpha$. A first
replacement profiling $\alpha$ out at fixed $\beta$ fixed the boundary cases
but not the concentrated ones. The current fit agrees with 60-digit mpmath
MLEs to $3\times10^{-14}$ relative on fifteen samples, including data at
$10^{-200}$, near one, at $0.3\pm10^{-8}$, U-shaped data and $n=2$
(`dev/validation/beta-free-fit-check.py`). The beta, nsbeta and betapr
fixed-scale golden estimates were updated in place (data unchanged) to values
that agree with mpmath; the old ones were $10^{-11}$ to $2\times10^{-9}$ off.
