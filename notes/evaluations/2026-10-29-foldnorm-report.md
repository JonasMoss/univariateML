# foldnorm — implementation and validation

No commits or Git-writing commands. No board edits, package installs or sub-agents.

## Files

- New distribution folder `cppunivariate/include/cppunivariate/distributions/foldnorm/`:
  `README.md`, `mle.hpp`, `dpqr.hpp`, `adapter.cpp`, `dpqr.R`, `test.cpp`,
  `test.R`, `golden.json`.
- Shared registration: `cppunivariate/include/cppunivariate/cppunivariate.hpp` only.
- This report. Generated R package/build outputs are gitignored.
- Existing goldens are unchanged.

## Mathematics: proved by direct derivation

Write m=mean(x), m2=mean(x^2), v=mean((x-m)^2), and m4=mean(x^4).
The continuous endpoint density is used at zero; the parent sign is
unidentifiable, so mu>=0 is the identifiable parameterization.

For nonconstant nonnegative data a maximum exists. The density is bounded
by twice the N(mu,sigma^2) density, and sum((x-mu)^2)>=n*v>0. Thus the
likelihood vanishes as sigma decreases to zero, uniformly in mu. It also
vanishes as sigma increases without bound (density<=2/(sigma*sqrt(2*pi))).
With sigma bounded above and below, mu increasing without bound also kills
the likelihood. Nonempty upper likelihood level sets are therefore compact,
and continuity, including at mu=0, gives attainment.

Constant x=c has unbounded likelihood at mu=c, sigma decreasing to zero.
With mu held, this divergence occurs exactly when every x equals that mu.
With positive sigma held the likelihood always has a finite maximum.
Zeros mixed with positive values cause no existence problem.

Set a=mu/sigma^2, b=1/(2*sigma^2). At fixed a, differentiating in b gives
b=(1+sqrt(1+4*m2*a^2))/(4*m2), the unique maximizer. Equivalently,
sigma^2=2*m2/(1+sqrt(1+4*m2*a^2)) and mu=a*sigma^2. The profile derivative
is sum(x*tanh(a*x))-n*mu. Since tanh<=1, it is negative for a>m/v;
this gives a finite analytic search bound. At stationary joint fits,
mu^2+sigma^2=m2.

At a=0 the exact member is mu=0, sigma=sqrt(m2). Both the ordinary mu
score and first profile derivative in mu^2 vanish. The first informative
inward profile term is n*(3*m2^2-m4)*a^4/12. Thus m4<3*m2^2 rules out
the boundary; m4>3*m2^2 gives a strict local boundary maximum, but requires
global comparison. Equality requires higher terms: the a^6 coefficient is
n*(m6/45-m2^3/3). No universal uniqueness claim or moment-only global
boundary classifier is assumed. A concrete counterexample to a global
moment-only rule is 99 ones and one five: m2=1.24, m4=7.24>3*m2^2, so the
boundary is locally maximal. The candidate mu=1.04, sigma^2=0.1584 has at
least the ordinary normal likelihood there, exceeding the half-normal
likelihood by n*(-log(2)+log(1.24/0.1584)/2)>33.57. Thus there must also
be an interior maximum.

For fixed sigma, tanh(c*mu)/mu strictly decreases, so a positive mu root is
unique and exists exactly when m2>sigma^2; otherwise mu=0 is the global fit.
For fixed mu>0, scale score roots satisfy
mean((x-mu)^2)<=sigma^2<=m2+mu^2. At fixed mu=0, sigma=sqrt(m2).

Joint and fixed-mu searches use a grid and bracket refinement, then chord
upper bounds. A(a)=sum(log(1+exp(-2*a*x))) is convex. The remaining joint
profile has derivative n*(m-mu(a)) and is strictly concave. Under an A chord,
the interval upper bound is maximized at mu=m+chord_slope/n, clipped to the
interval. At fixed mu, the remainder is n*log(a)/2-n*a*mean((x-mu)^2)/(2*mu)
plus a constant, also strictly concave, with an analytic chord maximizer.
Subdivide intervals whose bound exceeds the incumbent by n*reltol. Exhausting
the work budget reports failure. These are real-arithmetic bounds, not
interval-arithmetic certificates. Floating-point comparisons allow a per-
observation 1e-11 preference for the exact boundary.

## Checked numerically

- `just cpp-test`: **5/5 ctest targets passed**, including 3 folded-normal cases.
  Repeated after the exact constant-data check; again 5/5 passed.
- `UMVL_GOLDEN_IDS=foldnorm just goldens`: **1/1** generated (500 observations).
- `just test`: **5,104 expectations passed**, 0 failures/warnings/skips.
- Added the discovered two-interior-maxima regression afterward. The final
  folder-only R run passed **257/257 expectations in 5 cases**, 0 failures,
  warnings, errors or skips (the added case contributes 5 expectations).
- `just quality foldnorm`: **1/1 passed**. Full `just quality`: **72/72 passed**;
  folded-normal standardized score 7.675e-8, oracle logLik gap -1.14e-13.
- Independent profile audit: **20,000 samples**, 13,545 strict local boundary
  maxima, one sample with two interior maxima (three stationary points).
  Reproduced with the same seed and retained the 14-observation sample in
  `test.R`; its scores, curvature signs and global fit all pass.
- Independent **80-digit** check confirms the three roots
  a=1.32116653770428055, 2.01842452929909842, 3.84304939708636897,
  with curvature signs (-,+,-). The two maxima have logLik 1.74532884560876
  and 1.75039819354941; the fitter selects the latter. The intervening
  minimum has logLik 1.74502585306426.
Commands all use
`/tmp/umvl-cpu/run`; logs and research scripts: `/tmp/umvl-codex/foldnorm/`.

## Assumptions and open questions

- Floating-point chord bounds are assumed to track their real-arithmetic
  counterparts to ordinary rounding accuracy; they are not interval arithmetic.
  A general theorem counting stationary points remains open; no uniqueness
  assumption is needed by the search.
- Quantiles below the representable positive-double range round to zero;
  CDF log probabilities remain evaluable when the quantile is representable.
- No external reference package is required; identities and an independent
  normal-density oracle supply validation.
