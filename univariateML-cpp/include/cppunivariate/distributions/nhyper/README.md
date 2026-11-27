---
id: nhyper
inference_numeric:
  - regime: unreviewed or unavailable
    available: false
    condition: "composition or design free"
    reason: "all coefficients are integer valued; there is no continuous free inference coordinate"
    classification: "dev/roadmap/2026-10-30-inference-and-dqm.md#discrete-coordinates"
name: Negative hypergeometric
support: {type: integer, lower: "r", upper: "if (r == 0) 0 else N-m+r", closed: [true, true]}
params:
- name: m
  symbol: 'K'
  role: white balls in the population
  domain: '[0, N]'
  simulate: 5
  integer: yes
  kind: regular
- name: "N"
  symbol: 'N'
  role: population size
  domain: '[0, inf)'
  simulate: 10
  integer: yes
  kind: design
  advice: Supply N to hold the population size, or set N = NULL to estimate it. Population and composition can be estimated jointly when the tail comparisons certify the fit.
- name: r
  symbol: 'r'
  role: white balls required to stop sampling
  domain: '[0, m]'
  simulate: 3
  integer: yes
  kind: design
r:
  density: dnhyper
  sampler: rnhyper
  control:
    iterlim: "Work limit for population bisection (default 100). The stopping-count profile allows 128 times this many lines, with the limit capped at 100000 before multiplication."
    r: "Stopping count of white balls; explicit `NULL` estimates it with either `m` or `N` supplied."
    "N": "Population size; explicit `NULL` estimates it with `m` held."
    m: "White-ball count; `NULL` estimates it with `N` known, otherwise holds it fixed."
log_density: "loggamma(x) - loggamma(r) - loggamma(x-r+1) + loggamma(N-x+1) - loggamma(m-r+1) - loggamma(N-m-x+r+1) - loggamma(N+1) + loggamma(N-m+1) + loggamma(m+1)"
references:
- 'Berry, K. J. and Mielke, P. W. (1998). The negative hypergeometric probability distribution: Sampling without
  replacement from a finite population. Perceptual and Motor Skills, 86(1), 207-210.'
- Johnson, N. L., Kemp, A. W. and Kotz, S. (2005). Univariate Discrete Distributions, third edition. Wiley. doi:10.1002/0471715816.
fit_args:
  'N': 50
  r: 3
examples: |
  set.seed(1)
  x <- rml(200, mlnhyper())
  mlnhyper(x, N = 10L, r = 3L)

group: finite sampling
construction:
- base
relations:
  transform_of:
  - family: bbinom
    at:
      size: (N-m)*min(1,r)
      alpha: max(1,r)
      beta: m-r+1
    transform: shift
    shift: r
fit:
  method: integer-search
  certified: partial
  note: 'nhyper/README.md, Details; nhyper/mle.hpp (public fitter and held overloads); nhyper/test.cpp: Supported
    certified integer searches use adjacent ratios, a single-crossing population profile with composition held, or a proved growing-urn tail bound.
    Unclosed population and composition comparisons retain the best finite candidate with its per-fit flag.'
areas:
- sampling and quality control
existence:
  uniqueness: unknown
  statement: >-
    With all counts free, constant samples attain likelihood one, while
    mixed zero and positive observations are infeasible. No complete
    existence or uniqueness criterion is asserted for positive nonconstant
    samples. With $N$ held, every feasible sample has a finite maximum.
    With $K$ held, every feasible sample also has a finite maximum, whether
    the stopping count is held or estimated. Ties can occur. With only
    positive $r$ held, a candidate strictly above the shifted
    negative-binomial supremum ensures finite attainment. With $r=0$,
    only zero observations are possible and every admissible composition
    and population consistent with the holds maximizes the likelihood.
    Holding all counts specifies the law without optimization.
  reason: >-
    A known population leaves a finite integer grid. With $K$ held and
    positive $r$, the adjacent-population ratio is below one beyond
    $K\max_i x_i/r$, leaving finitely many candidates. A free positive
    stopping count has only finitely many feasible values. With both
    population and composition free, the tail bound below proves
    attainment above the relevant negative-binomial faces, conditional on
    the comparisons. It does not classify unresolved faces or prove uniqueness.
classification: specialist
classification_reason: Used to model finite-population waiting counts up to a specified number of successes.
summary: Negative hypergeometric provides a model for finite-population waiting counts up to a specified number
  of successes.
aliases: []
key_references:
- 2
---


The negative hypergeometric distribution counts total draws without
replacement until the $r$th white ball from a population of $N$ balls,
$K$ of them white. Each observation starts with the population restored.

## d/p/q/r

### Small marked populations

With five marked positions in a population of $10^8$, the third marked
position exceeds $N-3$ with probability $60/[N(N-1)(N-2)]$. This tail is
sensitive to subtracting large gamma normalizers. A finite product over the
marked positions retains it without enumerating the position support.

## Details

With a positive stopping count held and both urn counts free, the population-tail bound reduces the comparison with the negative-binomial face to finitely many eligible populations. A finite tie with the face gives attainment. Strict exclusion on the finite list and in the tail proves escape. [Proof](../../../../../dev/research/evaluations/2026-11-25-nhyper-math-report.md#an-exact-finite-test-including-equality-and-escape).

Holding population and stopping count estimates the integer white-ball
count. For positive $r$, the possible total draws run from $r$ to
$N-K+r$. The likelihood ratio at consecutive composition counts decreases,
so bisection locates a maximum, including attained boundaries. Adjacent
counts can tie. When $r=0$, sampling stops immediately and the law is the
point mass at zero, regardless of composition.
An adjacent composition or stopping-count pair found within the comparison
margin is a proved tie only if the integer factors of its exact likelihood
ratio cancel. An unresolved comparison retains the two evaluated candidates.


A known population permits joint estimation of composition and stopping
count on a finite feasible grid. With composition held and population free,
an adjacent-population likelihood ratio with a single sign crossing gives a bisection for
each feasible stopping count. Holding population and composition instead
leaves a one-coordinate stopping-count likelihood with decreasing
consecutive ratio.

When population and composition are both free, their ratio can tend to a
fixed white proportion. Then the number of black draws, $x-r$, tends to a
negative-binomial law with size $r$ and fitted success probability
$r/\bar x$. This is a boundary of the unrestricted family, rather than an
escape from a known-population fit. A finite candidate strictly above the
relevant limiting suprema gives a finite search cutoff. An unresolved
comparison with a limiting model does not prove non-existence.

## R notes

The coefficients are `m` ($K$), `N` and `r`. Supply `N` and `r` for the
usual composition fit. Supplying `m` too evaluates the specified law.
These fits record respectively `N, r` and `m, N, r` as fixed, giving one
and zero degrees of freedom. Explicit `NULL` requests estimation of a
design count; population and composition can also be estimated jointly.
The reference's black-ball count is `n = N - m`.

One-coordinate fits return the smaller count on adjacent ties.
For all-zero observations, every maximizer has `r = 0`. The maximizing
pairs $(K,N)$ are precisely the integer pairs $0 \le K \le N \le 2^{53}-1$
consistent with the held counts. In particular, a held zero population
forces $K=0$, and a held composition of $2^{53}-1$ forces $N=2^{53}-1$.
Joint-grid conventions and budgets are given below. Counts must be below
$2^{53}$. For the growing-population limit with positive `r`, use
`mlnbinom(x - r, size = r)`. Unresolved face comparisons retain the best finite estimate with `certified = FALSE`.

### Population search with composition held

For fixed positive $r$ and $K$, the feasible population starts at
$K+\max_i x_i-r$. Write $A=N+1$. The adjacent likelihood factor for one $x$
is
$$
\frac{(A-K)(A-x)}{A(A-K-x+r)}
= 1+\frac{Kx-rA}{A(A-K-x+r)}.
$$
For $K=r$ the likelihood is maximized at the feasible lower endpoint.
For $K>r$, the log of this factor vanishes at $rA/K$ and its derivative
in $x$ is $(K-r)/[(A-x)(A-K-x+r)]$. As $A$ increases, the ratio of
the new derivative to the old one decreases in $x$. Normalizing this
ratio at the old zero shrinks the positive integrals relative to the
negative ones. The zero moves right as well. Thus a nonpositive sum of
log factors stays nonpositive. Every factor is nonpositive on the log
scale by $A=K\max_i x_i/r$.

Bisection locates the first nonpositive adjacent ratio for each feasible
$r$. A population line is certified conditional on the computed signs
of the neighboring ratios, separated by $64\epsilon(1+\sum_i|\log R_i|)$
with $\epsilon$ the double machine epsilon. The product difference in the
numerator is compensated before taking logs. For a positive constant count $x$ with $m,r$ held, $mx=r(N+1)$ proves an exact tie at the two feasible populations $N,N+1$. A line with an unresolved
sign has the log-likelihood upper bound $\sum_i\log(r/x_i)$ from
$P(T=x)=(r/x)P(H_x=r)$. The final comparison uses a margin of
$10^{-9}(1+|\ell|)$. An unresolved order returns the best evaluated
line with its certification flag withdrawn.

With only $N$ held, the finite grid retains the lexicographically first
point within 64 double-precision ulps of the best log likelihood.
A constant sample with all coefficients free ties $(c,c,c)$ with
$(N,N,c)$ for every admissible $N\ge c$, with a unique maximizer at the upper endpoint of the supported count domain.

### Growing-population bound

For positive `r`, write `c_t(j) = sum_{i=0}^{j-1} log(1-i/t)`.
The waiting-time mass divided by the negative-binomial mass of `x-r`
with size `r` and success probability `p=m/N` equals
`exp(c_m(r)+c_{N-m}(x-r)-c_N(x))`, with endpoints understood by
continuity. Both numerator corrections are nonpositive. Consequently,
for observations `x_i` and `N >= max(x_i)`, the profile for fixed `r`
is bounded above by `B_r - sum_i c_N(x_i)`, where `B_r` is the
negative-binomial face supremum at `p = r/mean(x)`. The correction is
nonnegative, decreases with `N`, and tends to zero. With `r` free and
positive observations, `1 <= r <= min(x_i)`, so replacing `B_r` by the
maximum over that finite set gives the same uniform tail bound. A finite
candidate strictly above the appropriate face supremum gives an eventual
cutoff; every feasible point below it must still be checked. For `r=0`,
the only possible observation is zero and finite maximizers exist.

The fitter profiles composition at each integer population and each feasible
stopping count, initially over 256 populations. A candidate exceeding the
largest face by more than `1e-9*(1+abs(face))` gives a tail cutoff; every
population below it is searched, subject to the 10^7 evaluation budget.
The upper bound must fall below the candidate by that margin. If exploration
finds no strict improvement, or a face matches within the margin, the best
finite profile is returned with `certified = FALSE`.

For large counts the joint exploration starts at $N=\max_i x_i$, where
feasibility forces $K=r$. The likelihood on that boundary has decreasing
adjacent ratios in $r$. The search also profiles $N$ at compositions from fixed-population slices
near the support edge and in the growing-urn regime. For a single stopping count, a boundary
winner can exclude $K>r$ using the neighboring hypergeometric masses.
Write $u=K-r$, $v=x-r$ and $w=N-K-x+r$. When all three are positive,
the product of the two adjacent mass ratios is
$r u v w/[(r+1)(u+1)(v+1)(w+1)]\ge 1/16$.
When $w=0$ and $v\ge r+1$, the upper adjacent ratio is at least one.
Thus each observation with $x-r\ge r+1$ bounds $P(H_x=r)$ by $4/5$
when $K>r$. The joint exploration returns an uncertified candidate when
it cannot exclude the remaining compositions and stopping counts.

For an interior face probability `p=r/mean(x)`, the first-order coefficient
in `t=1/N` is one half of the sum over observations of
`x*(x-1) - r*(r-1)/p - (x-r)*(x-r-1)/(1-p)`. Its sign is a local
diagnostic, not a global search certificate; for free `r` it applies to
each face separately.

## Stop details

The growing-urn face is a negative binomial translated by r, since observations count total draws. Its payload explicitly records the shift and the absence of a corresponding package family.

With composition held, a constant draw count $c\le m$ attains likelihood one at $N=m$, $r=c$. This exact member also settles the case at the largest represented population. Outside such attained cases, omitting stopping counts whose feasible populations exceed the representation range leaves the finite prefix uncertified. The large-count growing-urn search retains its best finite witness when the remaining compositions and stopping counts cannot be excluded.

The finite-grid combination sums can cancel at large counts in double-only
arithmetic. Once the grid selects a winner, its reported likelihood uses the
hypergeometric saddle-point identity for the mass, retaining the search and
tie decisions from the grid.
