# Zero, truncation and interval observations

This document records the implemented interface and the decided target. Rationale
and retirement decisions are in the [zero/truncation roadmap](../roadmap/2026-11-11-zero-and-truncation-modifiers.md)
and [censoring roadmap](../roadmap/2026-11-05-censoring-truncation.md).
Old wrappers coexist during equivalence validation. Removal is a separate 2.0
change: no compatibility aliases are planned.

## R interface

```r
x <- c(0, 0, 1, 2, 3, 5)
a <- mlpois(x, zero = "modified")
# coefficients: lambda, p0; p0 = 2/6
# model remains Poisson; attr(a, "zero") == "modified"
b <- mlpois(x, zero = "modified", p0 = .2)
# fixed contains "p0"; degrees of freedom = 1
c <- mlpois(x[x > 0], zero = "truncated")
d <- mlpois(x[x > 0], truncate = c(1, Inf))
# identical coefficients and likelihood, different recorded modifier settings
mlbinom(x, size = 10, zero = "inflated")
mlnbinom(x, size = 2, truncate = c(0, 10))
mlnorm(c(-1, -.2, .1, .3, .4, .9), truncate = c(-2, 2))
mlpois(x, truncate = c(NA, NA))  # estimated bounds: 0 and 5
fixed <- mlpois(lambda = 2, zero = "modified", p0 = .3)
dml(0:5, fixed); pml(0:5, fixed); qml(c(.1, .5, .9), fixed); rml(10, fixed)
```

`zero` is `"none"` by default. `"truncated"` conditions on positive counts;
`"modified"` adds the holdable coefficient `p0`, the total zero probability;
`"inflated"` adds holdable `pi`, the mixture probability of an extra zero.
The inflated total zero probability is `pi + (1-pi)*f(0)`. These coefficients
follow the ordinary NULL/free and numeric/held rules. Existing controls retain
their names. Like coefficient holds, these knobs are named entries in `...`;
public wrapper formals remain `x, na.rm, ...`. Under modifiers, holds and controls must be supplied by name. General truncation
and censoring also accept `reltol` and `iterlim` when the exact-data parent is
closed form; zero routes retain their existing control availability.

The retained window is **closed**, `[a,b]` intersected with the base support.
For integers it contains `ceiling(a):floor(b)`. Infinite endpoints are unbounded;
NA estimates the corresponding bound by the sample extreme. A fixed model needs
known bounds. Data outside the retained support and empty events are invalid.
Resolved bounds are stored in `truncate`, the original window in `truncate_spec`,
and free-bound flags in `truncate_estimated`. Each estimated bound adds one to
`logLik` degrees of freedom; bootstrap refits replay NA bounds. The coefficient
vector contains the parent coefficients and any zero atom coefficient; bounds
remain window attributes. Printed fits show their zero/window/observation setting.
All-held fits evaluate the conditional likelihood. A retained singleton count
has zero likelihood information; the fitter returns a compatible representative
and preserves any supplied coefficient.

`dml`, `pml`, `qml`, `rml` use the modified law, including log probabilities,
upper tails, and nominal window endpoints. Poisson, binomial and normal windows
reuse the existing tpois/tbinom/tnorm evaluation kernels with bounds mapped to
the closed convention. The remaining CDF normalization path explicitly refuses
subtractions too close to resolve at its practical floating-point margin;
this avoids returning an unchecked conditional probability. Censoring changes observation
likelihoods; it leaves the fitted underlying law and its d/p/q/r unchanged.
Covariance for modified or censored fits is explicitly unavailable in this
implementation; the exact-data information matrix must not be reused.

## Availability and refusals

Zero eligibility is derived from integer support with literal lower bound zero,
excluding existing modification/truncation constructions. This gives 11 bases:
bbinom, binom, bnbinom, cmb, cmp, dgamma, dweibull, geom, nbinom, pig, pois.
The declared lgser exception additionally exposes the zero argument, with
unimplemented values refused. Every family exposes `truncate`.

All zero values are implemented for pois, binom, nbinom and geom, retaining the
existing conditional and mixture fitters and their status/certification scope.
General truncation is implemented for pois, geom, norm, and binom/nbinom with
size held. `[1,Inf]` takes exactly the same route as `zero="truncated"`, including
free-size fits and attained positive-truncation boundary extensions.
The generic conditional-family solver is independently checked against that
route. Other eligible values are reserved and raise these exact messages:

```text
`zero = "modified"` is not implemented for `mlcmp`; see TASK-191 or its successor
`truncate` is not implemented for `mlgamma`; see TASK-191 or its successor
`truncate` with size free is not implemented for `mlbinom`; see TASK-191 or its successor
zero + truncate is not implemented for `mlpois`; see TASK-191 or its successor
censor + zero is not implemented for `mlpois`; see TASK-191 or its successor
censor + truncate is not implemented for `mlnorm`; see TASK-191 or its successor
censoring is not implemented for `mlgamma`; see TASK-191 or its successor
censoring scheme or hold set is not implemented for `mlexp`; see TASK-191 or its successor
```

Ineligible zero arguments follow the existing unknown-argument diagnostic.
Refusals never drop the modifier and fit the base law. Zero plus truncate and
censor plus zero/truncate are reserved compositions; holds compose with every
supported modifier. A truncation domain is declared separately from the parent's
because bounded conditioning can enlarge it. This implementation retains the
parent domains; zeta's finite-window extension to s <= 1 is still required
before zipf can retire.

## Interval observations

```r
intervals <- cbind(lower = c(1,2,3,4), upper = c(1,Inf,3,Inf))
f <- mlexp(intervals)
# rate = 2 / 10 = .2; logLik = 2*log(.2)-2; n = 4
surv_like <- structure(cbind(c(1,2,3,4), c(1,0,1,0)),
                       class = "Surv", type = "right")
mlexp(surv_like)  # same fit, no survival dependency
model_select(intervals, models = c("exp", "gamma"))
# exp selected; gamma named in attr(result, "skipped") with its refusal
bootstrapml(f, type = "nonparametric")  # resample interval rows
bootstrapml(f)  # error: Parametric bootstrap of censored data requires a censoring mechanism
```

Equal endpoints are exact observations. Nonpoint cells are `(lower,upper]`;
finite lower and infinite upper means right censoring. Left and finite interval
censoring are represented but refused. Surv duck typing accepts class `Surv`,
attribute `type="right"`, and two columns `(time,status)`, with status 0/1.
Other Surv types receive the explicit message
`Surv type is not implemented; supply a two-column interval matrix`.
NA removal drops whole interval rows. Exact-only matrices reduce to vector fits.

Right-censored fits are supported for exp, weibull (every hold set), rayleigh (`sigma` free),
pareto (`b` held, `a` free), and lomax (`lambda` held, `kappa` free).
Other schemes, families and held subsets refuse. Free fits without exact events
report `AllCensored: no exact observations`. Fixed-model exceptions retain the
core likelihood evaluation rules. Model selection records unsupported families
and schemes under `skipped`; fitting failures remain errors if nothing fits.
Parametric bootstrap needs an explicit censoring mechanism and is refused.

## C++ interface and certificate

```cpp
#include "cppunivariate/distributions/pois/zero.hpp"
#include "cppunivariate/distributions/nbinom/truncation.hpp"
#include "cppunivariate/distributions/norm/truncation.hpp"
auto zero_fit = cppunivariate::PoisZeroTraits::modified(counts, 1e-10, 200);
auto window_fit = cppunivariate::truncated<cppunivariate::PoisTruncationTraits>
    ::fit(counts, 1, 10); // parameter, logLik, iterations, status
// NB size held at 2; probability free (NaN):
auto nb = cppunivariate::truncated<cppunivariate::NbinomTruncationTraits>
    ::fit(counts, 1, 10, {2}, NAN, 1e-10, 200);
auto normal = cppunivariate::truncated<cppunivariate::NormTruncationTraits>
    ::fit(values, -2, 2, NAN, NAN, 1e-10, 200);
```

The zero traits retain family-specific result enums, parameters, holds and
failure states. The default family functions remain unchanged. Discrete generic
truncation uses `TruncationStatus` (`Ok`, `InvalidInput`, `NotImplemented`,
`LowerLimit`, `UpperLimit`, `NumericalFailure`). The additive common binding
seam provides runtime family/modifier routing for R and JS. Unsupported runtime
values produce explicit failures; no exception or logging occurs in the core.

For the one-parameter exponential families, the conditional score is
`n*(sample_mean - E[X])`; its derivative is `-n*Var[X]`. Thus a sign bracket
contains the unique maximum in exact arithmetic. Bisection controls relative
coefficient error via the natural-parameter interval. Mode-centred recurrences
bound discarded tail mass and first moments by geometric tails. Normal uses the
existing joint concavity certificate in `(mean/sd^2,-1/(2*sd^2))`, including its
exponential/uniform face tests and held subsets. Numerical comparisons are
conditional on floating-point arithmetic with the recurrence's practical
`1e-18` relative first-moment margin; this is not outward-rounded arithmetic.
A failed search or unresolved coefficient bracket returns failure. Inherited
successful searches retain their existing uncertified fallback flags.

The layout move is isolated to `core/zero_modified.hpp`,
`core/zero_modified_fit.hpp` and the two forwarding `profile.hpp` files.
The second header depends on positive-conditional fitters and is separate to
avoid cyclic includes; no numerical changes are part of that extraction.

## JS interface

```js
const api = await create();
const fit = api.pois.fit(new Float64Array([0,0,1,2,3,5]),
                         {p0: .2}, {zero: 'modified'});
// estimates {lambda: ..., p0: .2}; fixed ['p0']; zero 'modified'
api.pois.d(new Float64Array([0,1,2]), [fit.estimates.lambda, fit.estimates.p0],
           {zero: 'modified'});
api.pois.fit(new Float64Array([1,2,3]), {}, {truncate: [1,10]});
api.exp.fit({lower: new Float64Array([1,2,3,4]),
             upper: new Float64Array([1,Infinity,3,Infinity])});
```

Modifier options are in the existing third `control` argument for fits and
in d/p/q flags; r takes a fourth modifier-options argument after its seed.
Coefficient arrays append `p0` or `pi`. JS interval input is `{lower,upper}`;
there is no Surv class in JS. Errors carry the same refusal text as R.

## Metadata and reduction inventory

The [front-matter schema](../../univariateML-cpp/include/cppunivariate/distributions/README_SCHEMA.md#modifiers)
defines `modifiers.zero`, `.truncate`, `.censor`, certification scopes, held/free
requirements, bounded domains and limits. Absent eligible configurations are
`not-implemented`. Derived catalog tags distinguish eligibility
(`zero-modifiable`, `truncatable`) and implementation (`zero-modified`,
`truncation`). The catalog has a Modifiers facet.

[capabilities.csv](../validation/capabilities.csv) has one row per current family,
coefficient hold subset and modifier/scheme. Columns record eligibility, status,
certification, escape count, frozen golden fixture, base family and replacement.
[family-reductions.csv](../validation/family-reductions.csv) lists every current
family once. Both are generated by
[modifier_capabilities.py](../validation/modifier_capabilities.py), also run by
`just gen`; no reduction rows are hand-maintained. Golden coverage identifies
available legacy fixtures, not a certification theorem or a completed retirement.

There are 99 fitted families. The agreed 16 replacements leave 83 base entries.
The zero knobs unlock 33 regular-base configurations (11 bases times 3 values),
12 implemented initially and 21 explicitly unimplemented, plus the lgser
exception. General truncation exposes every family with the supported scopes
above; zero-truncation special cases also retain free-size routes. All 16
families remain callable until equivalence, escape messages, domain extensions
and release migration are complete.

The traits-based hurdle entry point is also available independently of the
compatibility fitters:

```cpp
auto h = cppunivariate::zero_modified<cppunivariate::PoisZeroTraits>
    ::fit(counts, NAN);  // free p0, positive MLE in h.positive
// h.p0, h.logLik, h.status; h.positive preserves the family's status enum
```

It fits only the positive observations through the traits, then adds the
Bernoulli log likelihood. `HurdleStatus::AllZero` explicitly reports unidentified
positive coefficients; `PositiveFitFailure` retains the positive fitter's
specific status in the nested result. Legacy wrappers retain their established
all-zero representatives and warnings during coexistence.
