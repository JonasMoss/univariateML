---
id: dgamma
name: Discretized gamma
support: {type: integer, lower: 0, upper: inf, closed: [true, false]}
params:
- name: shape
  symbol: 'k'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: rate
  symbol: '\lambda'
  role: rate
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: ddgamma
  sampler: rdgamma
  control:
    shape: "Optional held value; NULL estimates it."
    rate: "Optional positive held value; NULL estimates it."
    reltol: "Tolerance for the logarithmic search coordinates."
    iterlim: "Maximum iterations per search start."
log_density: "log((lowergamma(shape, rate*(x+1)) - lowergamma(shape, rate*x))/gamma(shape))"
limits:
  - name: point mass
    when: '$\mathrm{shape}\to\infty$ with the latent distribution concentrating inside one cell'
    detect: both parameters free and all observations equal
    status: PointMass
  - model: binom
    name: shifted Bernoulli
    when: 'the latent distribution concentrates around the shared boundary of two adjacent cells'
    detect: both parameters free and max(x) - min(x) = 1
    status: TwoPointLimit
  - name: point mass at zero
    when: '$\mathrm{rate}\to\infty$ with shape held, or $\mathrm{shape}\to0$ with rate held'
    detect: all observations zero and one parameter free
    status: PointMass
holds: []
examples: |
  set.seed(1)
  x <- rml(200, mldgamma())
  mldgamma(x)

group: gamma and Weibull
construction:
- discretisation
relations: {}
fit:
  method: multistart
  certified: partial
  note: 'dgamma/README.md, Details; dgamma/mle.hpp (public fitter and held overloads); dgamma/test.cpp: Held shape
    gives a strictly concave rate solve; joint and held-rate shape profiles use numerical grids without certified
    mode isolation.'
areas:
- counts and ecology
classification: specialist
classification_reason: Used to model nonnegative counts obtained by grouping gamma waiting times into unit intervals.
summary: Discretized gamma provides a model for nonnegative counts obtained by grouping gamma waiting times into
  unit intervals.
aliases: []
references:
- 'Wolodzko, T. and Klinke, S. (2026). extraDistr: Additional Univariate and Multivariate Distributions. R package
  reference manual, version 1.10.0.5. https://cran.r-project.org/web/packages/extraDistr/extraDistr.pdf.'
key_references:
- 1
---

The discretized gamma distribution is the integer part of a continuous gamma variable. Each observation records the latent variable in an interval from the observed integer to the next integer.

## Details

2026-10-30 (TASK-125): the shared gamma upper-tail helper switches to its asymptotic expansion before the backend tail becomes subnormal. The existing high-precision truth rows test the affected rate/shape cases; the expansion is not a uniform error guarantee.

With both parameters free, constant samples and samples on two adjacent integers have no finite maximum likelihood estimate. For adjacent values `k` and `k + 1`, use `mlbinom(x - k, size = 1)`.
Both held parameters evaluate the likelihood without fitting. With one parameter held the fitter handles its distinct boundary conditions explicitly.

Every unrestricted sample with range at least two has an attained maximum. Holding either parameter gives an attained maximum if any observation is positive; all-zero data escape to a point mass. The numerical fit uses an exact, strictly concave inner log-rate profile, a 41-point logarithmic shape grid with improving-edge expansion, and Nelder–Mead refinement of every grid-local maximum. Uniqueness has not been established; this search is not a certified global optimization algorithm. No additional proper limit model occurs as shape tends to zero: every positive fixed cell loses its probability uniformly over rate. Some mass can escape to infinity, but that does not give a positive limiting likelihood for a sample containing a positive count.

## Existence and identification

For $a>0,r>0$, $p_k=\Pr(k\le Y<k+1)$ with $Y\sim\mathrm{Gamma}(a,r)$.
The positive-cell tail satisfies
$p_k\sim C(a,r) k^{a-1}e^{-rk}$; the successive ratio identifies $r$,
and the power of $k$ then identifies $a$.
For adjacent cells the saturated multinomial likelihood is a strict upper
bound for every finite parameter value. To approach any lower-cell fraction
$p$, put $b=k+1$, $m=b-s\Phi^{-1}(p)$,
$a=m^2/s^2$, $r=m/s^2$ and send $s\downarrow0$.
The standardized gamma converges to normal, giving cell probabilities $p$
and $1-p$. Constants similarly approach their point masses by concentration
inside the cell (at zero, also by $r\to\infty$).

The potentially troublesome small-shape boundary cannot retain a positive
fixed cell. Indeed, for integer $k\ge1$, maximizing $t^a e^{-t}$ over $t=ry$
gives the uniform-in-rate bound

$$p_k\le\frac{a^a e^{-a}}{\Gamma(a)}\log(1+1/k)\longrightarrow0
\qquad(a\downarrow0).$$

Thus even joint small-shape/small-rate paths cannot give positive limiting
likelihood to a sample containing a positive count. Such paths can leave a
subprobability concentrated at zero with mass escaping to infinity; they do
not yield a log-series or zero-modified proper count law without an additional
conditioning/rescaling operation.
For shape in a compact positive interval, rate approaching zero loses every
fixed cell and rate approaching infinity loses every positive cell.
For $a\to\infty$, write $m=a/r$: the coefficient of variation is $a^{-1/2}$.
If $m\to\infty$, fixed cells lose all probability; if $m$ is bounded, variance
$m^2/a\to0$, so positive limiting probabilities can occur only in one cell
or the two cells meeting a common integer boundary. These cases exhaust
noncompact parameter sequences. A sample with range at least two therefore
has compact positive likelihood level sets and an attained maximum.

With shape held, both rate boundaries lose a positive cell unless the sample
is all zero. With rate held, shape tending to infinity loses all fixed cells,
and shape tending to zero loses any positive cell. This proves the stated
held-subset existence conditions. In an all-zero sample, rate increasing or
shape decreasing makes its cell probability tend to one, strictly exceeding
every finite fit. All log likelihoods are bounded above by zero.
These arguments establish existence, not gamma uniqueness or absence of local
maxima; the profile-grid search and independent oracles supply numerical checks
for the latter questions.

With shape held, put $T=\log Y$ and $u=\log r$. Its density is proportional
to $\exp(a(t+u)-e^{t+u})$, strictly log-concave in the location coordinate.
Integrating over each fixed interval in $t$ preserves log concavity, so the
inner log-rate score has a unique zero for samples containing a positive
count. The fitter brackets and solves this score with safeguarded Newton
iteration before the outer shape search.
