# Tukey lambda fit speed — TASK-151 investigation

Source revision: `9fee8c34` (master). This investigation changes no
fitting code, d/p/q/r, tests, goldens, bindings or dossier text. It
records measurements, a scratch prototype and numerical structure checks.
Each claim names its evidence: timing and agreement runs are numerical
comparisons, the 40-digit reference is a high-precision comparison, and
the structural properties are numerical sweeps, not proofs. The
[evidence directory](2026-11-08-tlambda-speed-data/README.md) has every
driver, the prototype and the logs.

## Where the time goes

| fit (n=101, data `qtlambda((i+.5)/n)`) | lambda=0.14 | lambda=-0.5 | lambda=0.7 |
|---|---:|---:|---:|
| all free (legacy multistart) | 1.2 s | 0.75 s | 1.0 s |
| sigma held (legacy) | 0.53 s | 0.32 s | 0.80 s |
| shape only (certified scalar boxes) | 19 s | 25 s | 2.6 s |
| lambda, mu held (concave or scalar boxes) | 4 ms | 21 s | 4 ms |
| lambda, sigma held (concave or scalar boxes) | 1 ms | 9.2 s | 1 ms |

`profile_n101.log`; tol=1e-7, iterlim=1000, workstation under shared
load; an earlier run in the same session differed by up to a factor two, so
ratios are more reliable than absolute times.

One `dtlambda` call costs about 0.6 µs, of which about 90% is the
inverse quantile (`dcost.cpp`). The bracketed Newton in `logsmall_log`
starts at a bracket midpoint and takes about six steps, each about eight
transcendental calls (`iters.cpp`). A closed-form starting guess that is
exact at the centre and asymptotically exact in the tail did not reduce
the step count (`guess.cpp`); the step count is dominated by the final
convergence check and by moderate-z starts.

The evaluation counts matter more. The all-free legacy fit runs 33
Nelder–Mead searches (11 shapes × 3 centres), about 4000–5000 simplex
iterations at n=101. The certified scalar box searches use zeroth-order
bounds: the bound gap over a box is proportional to its width, so the
search needs on the order of `1/sqrt(tol)` boxes near the optimum
(7.9k–83k bound evaluations in the table above).

## Profile prototype

`proto.hpp` fits all three coefficients on the fitted branch
[-1,1] by profiling shape:

* Inner fit at fixed lambda: damped Newton in `A=1/sigma`, `B=-mu/sigma`
  with analytic gradient and Hessian, Levenberg damping when the Hessian
  is not negative definite, and backtracking that accepts no decrease
  beyond rounding noise. For 0<=lambda<1 the inner problem is concave
  (dossier), so this inner optimum is global.
* Outer search: the legacy shape grid
  {-1,-.75,-.5,-.25,0,.14,.3,.5,.7,.9,.99}, warm-started from the previous
  grid shape; negative shapes also start at the quartiles. The profile
  derivative is the shape score at the inner optimum (envelope theorem),
  using

  $$
  \partial_\lambda\log g=-\frac{D_\lambda}{D}+\frac{D_uQ_\lambda}{D^2},
  $$

  with a series for Q_lambda when |lambda log u| is small. Each + to −
  sign change between grid neighbours is solved by Illinois regula falsi
  to |Δλ| ≤ 1e-12(1+|λ|). The lambda=-1 face is a grid point and the exact
  uniform member is compared at the end.

Two prototype defects were found and fixed; both must be kept in an
implementation. A positive-shape warm start that leaves data outside the
support must be repaired by widening scale about the same centre;
shrinking `A` with `B` fixed moves the centre and never becomes feasible.
A derivative that overflows while the log likelihood is finite produced
a NaN Hessian, and `std::max(2*damp, NaN)` then never increased the
damping; `value()` now requires every derivative to be finite and the
damping loop is capped.

### Agreement and speed

Log-likelihood agreement is |Δℓ| < 1e-7, where Δℓ is the prototype
log likelihood minus the legacy one, both evaluated with `dtlambda`.

| n | samples | same | better | worse | legacy | prototype | ratio |
|---:|---:|---:|---:|---:|---:|---:|---:|
| 30 | 88 | 88 | 0 | 0 | 20.1 s | 0.53 s | 38× |
| 101 | 66 | 66 | 0 | 0 | 49.6 s | 1.29 s | 38× |
| 1000 | 22 | 22 | 0 | 0 | 194 s | 7.0 s | 28× |

The samples come from 11 generators: normal, t3, t1.5, Cauchy, uniform,
exponential, 10% contaminated normal, a bimodal normal mixture, Tukey
lambda at −0.8 and 0.9, and a normal rounded to quarters
(`compare2.cpp`, `c30.log`, `c101.log`, `c1000.log`). Neither fitter
failed. At n=100000 on `qtlambda((i+.5)/n,0,1,.14)` the prototype takes
21 s and 591 likelihood passes (`big.log`; 25 s in an earlier run), against 392 s for the legacy
fit in the 2026-11-07 implementation report.

### Coefficient accuracy

On the frozen n=500 golden sample, a 40-digit Newton reference
(`ref.py`, gradient 6e-26 after polishing) gives these relative errors:

| fitter | mu | sigma | lambda |
|---|---:|---:|---:|
| legacy (current golden estimates) | 3.3e-8 | 3.3e-8 | 9.9e-8 |
| prototype | 5.4e-14 | 2.3e-14 | 7.8e-14 |

The prototype meets the 2.0 contract of relative 1e-10 in every free
coefficient; the legacy search does not. The golden estimates for
tlambda therefore carry error from about the 8th digit. The sample is
frozen; its estimates should be regenerated when the fitter changes.

The prototype is uncertified, like the legacy search: the outer shape
search is a grid plus local root-finding, and negative-shape inner fits
are local. Its global coverage is at least that of the legacy search,
because each nonnegative grid shape has a global inner optimum.

## Structural checks

**Scale concavity (numerical, proof open).** Holding location, the
scale log likelihood is concave in log sigma if

$$
m(z)=-z\,\psi(z)=\frac{Q(u)\,D_u(u)}{D(u)^2},\qquad \psi=(\log g)',
$$

is nondecreasing in z>0. For 0<=lambda<1 this follows from log
concavity. A 50-digit sweep of 30 000 points over lambda in [-6,1)
(`scale_concavity.py`) found no decrease above 1e-40; the only decreases
are rounding at the asymptote m → 1+1/|lambda|. Substituting r=u/v>1
and c=1-lambda reduces the claim to one inequality in (r,c). Its leading
terms cancel, so a proof needs the next order. If proved, every held-mu
scale fit, including any negative held shape, is a concave problem with a
tangent certificate, and the inner problem of a held-mu profile fit is
global for every shape. The held-mu, held-negative-shape scale fit
currently takes 21 s at n=101 by scalar boxes.

**Shape concavity is false.** At fixed z, log g is not concave in lambda:
violations cover |z| up to about 1.5 for every band of lambda above −2
(`concav2.cpp`). The shape-only fit therefore keeps a nonconcave scalar
search; a faster certificate needs first-order bounds, not concavity.

**Side findings from the same session.** At 40 digits the density has
closed forms $g(z)=1/\{\sqrt{4+z^2}(2+\sqrt{4+z^2})\}$ at lambda=-1 and
$g(z)=(4-z^2)/(4\sqrt{8-z^2})$ on |z|<=2 at lambda=1/2 (`closed_forms.py`).
Log concavity holds for lambda in [0,1] and, numerically, for lambda in
[2,8/3]. The right endpoint matches 8/3 to 50 digits, and concavity
first fails at u=1/2-1/√5 (`logconcave_window.py`). The window affects
only held shapes above 2.

## Density evaluation

The inverse is the whole cost, so it was retried in better variables
(`fastinv.cpp`, `fastinv.log`). Solve for s = log p, where p is the smaller
tail. Each step needs exp(s), log1p(-p), exp(lambda s) and
expm1(lambda (log q - s)). Then (1-p)^lambda, D and D' follow
algebraically, so a Halley step costs the same as a Newton step. Use
log h(s) = log|z| in the tail and h(p) = |z| near the centre, and accept
after a step below 1e-5, whose cubic error is about 1e-15. This covers
the non-deficit region: lambda <= 0, and lambda > 0 with lambda|z| <= 1/2.

| method | ns per log density | Halley steps |
|---|---:|---:|
| current `dtlambda` (Newton in -log p from a bracket midpoint) | 620–790 | about 6 Newton |
| Halley from a closed-form guess exact at 0 and in the tail | 300–350 | 2.7–3.1 |
| Halley from a per-shape 64-node cubic Hermite table in log1p(abs(z)) | 150–170 | 1.00 |

Shapes -0.7 to 0.9; lambda=-1 already has a closed form, at 120–150 ns.
The table costs about 20 µs to build, so it pays only when many
densities share one shape, as in a fit or a vector call.

Against a 60-digit reference (`dump.cpp`, `truth.py`,
`density_truth.log`, 1404 points over lambda in {-0.7,-0.05,0.14,0.7}),
the maximum relative log-density errors are:

| lambda | current `dtlambda` | table + Halley |
|---:|---:|---:|
| -0.7 | 7.8e-15 | 6.0e-16 |
| -0.05 | 1.0e-14 | 2.9e-16 |
| 0.14 | 1.9e-14 | 3.6e-16 |
| 0.7 | 2.7e-15 | 3.3e-16 |

The current inversion stops at |Δ log h| < 2e-14 and returns the
unverified iterate; that termination, not the formulas, sets its error.
Still open for a production version: the deficit region near
positive-shape endpoints, tails beyond the table, held shapes outside
[-1,1], and the d/p/q/r truth-table checks. One power evaluation (about
60–70 ns) is the floor without vectorized exp/log. Warm starts across fit
passes could make the single Halley step also the final evaluation.

### One static table for the fitted branch

A single table over (eta, lambda) for lambda in [-1,1] replaces the
per-shape tables (`table2d.hpp`, `table2d.cpp`, `table2d.log`). The
coordinate

$$
\eta=\frac{\log(1-\lambda\lvert z\rvert)}{-\lambda}\qquad(\eta=\lvert z\rvert\text{ at }\lambda=0)
$$

maps every member to eta in [0,inf). For lambda>0 the deficit is exactly
exp(-lambda eta). The residual r = log p + eta equals -log 2 at the centre
and tends to 0 in every regime: approximately -exp(-(1+|lambda|)eta) for
lambda<0, -log1p(exp(-eta)) at 0, and -exp(-(1-lambda)eta) for 0<lambda<1.
The table stores r at uniform lambda nodes and t = 1-exp(-c eta), and
interpolates with a local 4×4 cubic Lagrange stencil. The guess only has
to reach about 1e-5, because one Halley step then gives about 1e-15.

| lambda × t nodes | size | build | max guess error | Halley steps per density |
|---|---:|---:|---:|---:|
| 17 × 33, c=1 | 4.4 KB | 0.4 ms | 4.3e-2 | 1.10 |
| 33 × 65, c=1/4 | 17 KB | 1.6 ms | 5.1e-3 | 1.000 |
| 65 × 129, c=1/4 | 66 KB | 6.4 ms | 8.9e-3 | 1.000 |

The validation set has 182 138 points at off-grid shapes. The largest
guess errors sit in one corner, lambda near 1 at large eta, where r decays
only like exp(-(1-lambda)eta); they cost at most a second step. Under the
same load the full density path took about 430 ns against about 1560 ns
for `dtlambda`, roughly 3.6× (absolute times were inflated by other jobs).
Against 80-digit bisection (`worst.cpp`), the twelve largest
disagreements with `dtlambda` show the table path at 1e-17 to 2e-16
relative error away from the endpoint, where `dtlambda` is at 2e-14. Within
1e-4 of a positive-shape endpoint both paths are at 1e-14 to 1e-12. There
the prototype takes its deficit from a rounded product; production code
should take it from the exact affine support residual, as `dtlambda` does.

Options for the table:

* Build it at first use with a function-local static (thread-safe since
  C++11) from the same solver: about 1.6 ms once, no generated file, no
  stored constants to keep in sync.
* Generate a header with literal constants: no startup cost, but a
  generator and 2145 literals to review, and the stored values must
  match the solver's rounding on every platform.

The table only seeds a safeguarded Halley iteration, so a poor seed costs
steps, not accuracy. Shapes outside [-1,1], which occur only when held,
fall back to the closed-form seed at about 3 steps.

## Proposed work, in order of value

1. Replace the legacy all-free, sigma-held and mu-held searches with
   profile fits. This is a numerical change: regenerate golden estimates
   and inspect the diff, update the dossier, and keep this agreement
   harness as evidence.
2. Prove the scale-concavity inequality, then certify held-mu scale fits
   at negative shapes by the existing concave tangent route.
3. Give the certified scalar boxes (shape only; held-scale location at
   negative shape) first-order bounds. ψ is monotone between its known
   extrema, so derivative enclosures are cheap: a box whose derivative
   enclosure excludes zero is bounded by its endpoint values, and boxes
   near an optimum use a mean-value bound.
4. Replace the inverse with Halley in log p, seeded by one static table
   for lambda in [-1,1] (closed-form seed outside it), with warm starts
   in fits.
