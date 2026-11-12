# Modifiers: interface and first implementation

TASK-191 and interface part of TASK-142.1. Baseline: `bb460787`; worktree:
`modifiers`, branch `codex/modifiers`. No commits, pushes, board edits or changes
to the protected status/roadmap/NEWS files were made.

## Delivered

The [design](../../design/modifiers.md) records the R, C++ and JS interfaces,
exact refusal messages, observation conventions, composition rules and metadata
schema. Generated wrappers preserve their existing `x, na.rm, ...` formals and
control names. The knobs are named entries in `...` in R and options in JS.
Default fitters and all frozen goldens are unchanged.

Zero truncation, modification and inflation are implemented for Poisson,
binomial, negative binomial and geometric bases. This includes holdable `p0`
(total zero probability) or `pi` (extra-zero mixture probability), fixed models,
modified d/p/q/r, print metadata, and bootstrap replay. Geometric inflation uses
the negative-binomial mixture with size held at one. The 11 regular eligible
bases are derived from support and construction metadata; lgser is a declared
exception. Their other reserved values refuse explicitly.

Closed-window truncation is implemented for Poisson, geometric, normal, and
binomial/negative binomial with size held. Positive full-window truncation also
retains the existing free-size routes. NA endpoints estimate sample extremes,
add their degrees of freedom and remain NA instructions when bootstrap refits.
`zero="truncated"` and `truncate=c(1,Inf)` share the same binding route, including
existing attained boundary extensions. An independent generic C++ route is
checked against it.

The new shared binding seam is additive: `bindings/common/fit.hpp` and
`bindings/r/scripts/common_fit.h` were not edited. The extracted zero helpers
have forwarding headers at both old paths. Their exact correspondence to the
baseline, allowing only include-path substitutions, is checked by
[verify_layout.py](2026-11-12-modifiers-data/verify_layout.py).
The extraction is separable from the new numerical code without touching the
retiring families' `mle.hpp` files. The metadata also drives the site's Modifiers
facet; site packaging includes both new JS modules.

## Concrete interface and outputs

These R outputs are reproduced by
[interface.R](2026-11-12-modifiers-data/interface.R) and retained in
[interface.json](2026-11-12-modifiers-data/interface.json).

```r
x <- c(0, 0, 1, 2, 3, 5)
f <- mlpois(x, zero="modified")
# lambda = 2.5312005265216175, p0 = 1/3
# logLik = -10.669025915484696
h <- mlpois(x, zero="modified", p0=.2)
# lambda unchanged; fixed = "p0"; logLik = -10.961390935840859
w <- mlpois(x[x>0], truncate=c(1,5))
# lambda = 2.8630270281285908; logLik = -6.606708353789406
dml(0:3, f)
# 0.3333333333 0.1458663157 0.1846084475 0.1557603332
qml(c(.1,.5,.9), f)
# 0 2 4
pml(0:3, f); rml(10, f)
```

`mlpois(lambda=2, zero="modified", p0=.3)` creates a fixed law. Bounds
are attributes, while the coefficient vector contains the base coefficients and
any atom coefficient. Lower/upper-tail and log-probability options are retained.

```cpp
#include "cppunivariate/distributions/pois/zero.hpp"
// counts is cppunivariate::ispan containing positive observations:
auto z = cppunivariate::PoisZeroTraits::truncated(counts);
auto t = cppunivariate::truncated<cppunivariate::PoisTruncationTraits>
    ::fit(counts, 1, 10); // parameter, logLik, iterations, status
// Generic hurdle; nested positive result retains the native status enum:
auto h = cppunivariate::zero_modified<cppunivariate::PoisZeroTraits>
    ::fit(counts, NAN);
```

```js
const api = await create();
const fit = api.pois.fit(new Float64Array([0,0,1,2,3,5]),
                        {p0: .2}, {zero: 'modified'});
// estimates {lambda: 2.5312005265..., p0: .2}; fixed ['p0']
api.pois.d([0,1,2], [fit.estimates.lambda, fit.estimates.p0],
           {zero: 'modified'});
api.pois.fit([1,2,3], {}, {truncate: [1,10]});
api.exp.fit({lower: [1,2,3,4], upper: [1,Infinity,3,Infinity]});
```

## Censored observations and refusals

```r
cells <- cbind(lower=c(1,2,3,4), upper=c(1,Inf,3,Inf))
mlexp(cells) # rate = .2; logLik = -5.218875824868201
s <- structure(cbind(c(1,2,3,4), c(1,0,1,0)),
               class="Surv", type="right")
mlexp(s)     # identical; no survival dependency
model_select(cells, models=c("exp","gamma"))
# exp fits; gamma recorded under attr(result, "skipped")
```

Equal endpoints denote exact values; other cells are `(lower,upper]`.
NA removal drops whole rows. Exact-only matrices reduce to ordinary vector
fits. Right censoring reaches the existing C++ implementations for exp,
weibull (all hold sets), rayleigh (sigma free), pareto (b held, a free), and
lomax (lambda held, kappa free). Other families, schemes and hold sets refuse;
recorded values are never silently fitted as exact observations.

Nonparametric bootstrap resamples whole interval rows. Parametric bootstrap of
censored data refuses until a censoring mechanism is supplied. For ordinary
modified fits, simulation and refitting preserve modifiers, holds and estimated
bound instructions. Modified/censored covariance refuses rather than reusing
exact-data information. Censoring leaves evaluation of the underlying fitted
law unchanged.

Examples of exact refusal text:

```text
`zero = "modified"` is not implemented for `mlcmp`; see TASK-191 or its successor
`truncate` is not implemented for `mlgamma`; see TASK-191 or its successor
zero + truncate is not implemented for `mlpois`; see TASK-191 or its successor
censoring is not implemented for `mlgamma`; see TASK-191 or its successor
```

Zero plus truncation, and censoring plus zero or truncation, are reserved
compositions. The design lists the remaining exact diagnostics, including
unsupported Surv types, free-size windows and censor hold sets.

## Generated reduction and capability inventory

[capabilities.csv](../../validation/capabilities.csv) contains **3,370**
hold-subset × modifier/scheme rows, including declared status, certification,
escapes and retained golden coverage. The conformance test exercises every
`not-implemented` modifier/hold row. Both inventories regenerate without changes
after package regeneration; see
[regeneration verification](2026-11-12-modifiers-data/regeneration-verification.txt).
The regular zero knobs expose 33 configurations: 12 implemented and 21 reserved,
plus the lgser exception's reserved configurations.

[family-reductions.csv](../../validation/family-reductions.csv) contains every
current family once. **99 current entries minus 16 pending retirements leaves
83 base entries.** All 99 existing wrappers remain callable. The following rows
are generated directly from that CSV, not a separate hand-maintained mapping;
its coefficient-map column supplies any renaming.

| Family | Base | Replacement arguments | Domain condition |
|---|---|---|---|
| tbinom | binom | `truncate = c(max(0, floor(a)+1), min(size, floor(b)))` | parent domain |
| tnorm | norm | `truncate = c(a, b)` | parent domain |
| tpois | pois | `truncate = c(max(0, floor(a)+1), floor(b))` | parent domain |
| zib | binom | `zero = "inflated", pi = pi` | parent domain |
| zinb | nbinom | `zero = "inflated", pi = pi` | parent domain |
| zip | pois | `zero = "inflated", pi = pi` | parent domain |
| zipf | zeta | `truncate = c(1, N)` | s > 1 |
| zmbinom | binom | `zero = "modified", p0 = p0` | parent domain |
| zmgeom | geom | `zero = "modified", p0 = p0` | parent domain |
| zmlogarithmic | lgser | `zero = "modified", p0 = p0` | parent domain |
| zmnbinom | nbinom | `zero = "modified", p0 = p0` | parent domain |
| zmpois | pois | `zero = "modified", p0 = p0` | parent domain |
| ztbinom | binom | `zero = "truncated"` | parent domain |
| ztgeom | geom | `zero = "truncated"` | parent domain |
| ztnbinom | nbinom | `zero = "truncated"` | parent domain |
| ztpois | pois | `zero = "truncated"` | parent domain |

## Mathematical and numerical evidence

For a nondegenerate retained support, the conditional one-parameter exponential
family has score `n*(sample_mean-E[X])` and derivative `-n*Var[X]` in the natural
parameter. A score sign bracket therefore contains the unique maximum in exact
arithmetic. The discrete solver bisects until the returned coefficient interval
meets relative tolerance, bounds omitted mass and first moments with geometric
tails, and recomputes likelihood at the returned rounded coefficient. Singleton
supports and attained atoms are handled separately; coefficients that round to
an incompatible atom are rejected. An optional unfinished boundary calculation
does not suppress a successfully certified root. Work follows the existing
iterlim-based budgets; an unfinished solve never returns an unchecked estimate.

The practical recurrence margin is `1e-18` relative to the centred first moment.
These are exact-arithmetic certificates conditional on computed comparisons,
not outward-rounded interval certificates. Normal reuses the existing joint
concavity and face analysis. Zero/mixture routes retain the existing search and
certification scopes, including successful uncertified fallbacks; regression
equivalence is not presented as a new proof of their searches.

New tests compare the knobs with 11 zero-family frozen samples and the three
`tpois`/`tbinom`/`tnorm` samples, including held coefficients, log likelihood and
law evaluation. The direct generic C++ solver is compared with all four existing
positive-truncation routes. Additional cases cover base-law containment,
p0=0, singleton/atomic windows, boundary nonexistence, tight work budgets,
small tails, quantile endpoints, unresolved floating-point normalizations,
interval reduction, censor refusals, and estimated-bound replay. These are
regression and numerical checks; no new independent high-precision oracle sweep
is claimed.

## Final verification

Every command below completed successfully on the workstation, through `run4`
for the suites/builds. BLAS/OpenMP were pinned to one thread per worker. Simbox
attempts returned 75 (unreachable), so their required local fallbacks were used.
GCC was 15.2.0 and R was 4.6.1.

| Command | Final result | Machine |
|---|---|---|
| `just cpp-test` | 108/108, zero failures | workstation |
| `just cpp-test-no-long-double` | 135/135, zero failures | workstation/GCC 15 |
| `just test` | 28,774 passes; 154 planned skips; zero failures/warnings | workstation |
| `just test-full` | 55,532 passes; zero failures/warnings/skips | workstation |
| `just js-build` | successful native/WASM build | workstation |
| `just js-test` | 617/617; zero failures/skips | workstation |
| `just site-check` | build, browser and static checks passed | workstation |

The final R dev and full runs include the latest quantile guards. The latest
C++, JS and site runs include their final source changes. All commands have
finished; none is left running. A focused R modifier file also passed after
regeneration. [checks.json](2026-11-12-modifiers-data/checks.json) records commands,
counts and hashes of the local logs. Full logs remain under
`build/scratch/final/` while this worktree exists.

`git diff --check` passes. The extraction and deterministic regeneration checks
pass. A supplementary repository-wide Markdown audit reports 45 broken
destinations, all inherited from `bb460787`; the
[baseline comparison](2026-11-12-modifiers-data/verify_links.py) reports zero new
broken destinations. Regeneration also reports two existing roxygen topic
warnings for `[1e-7,1e-6]` in unchanged lst/std prose. Neither finding was hidden
or repaired by editing unrelated material.

## Open work before retirement

- **GCC 13 cross-machine verification:** simbox was unreachable. Its
  no-long-double run could not execute; the GCC 15 counterpart and all local
  fallback suites passed.
- **Remaining zero bases:** implement the 21 reserved regular-base values after
  their positive-conditional limits are established. Implement lgser's declared
  exception with its theta/prob mapping before removing zmlogarithmic.
- **General free-size windows:** binomial and negative-binomial windows currently
  require size held, except the existing positive full-window route. Free-size
  searches and their escape/certificate analysis are still needed for complete
  tbinom replacement.
- **Zipf:** zeta truncation is reserved. Finite-window zeta needs the enlarged
  `s <= 1` domain; the generated existing relation only covers `s > 1`.
- **Conditional-tail evaluation:** Poisson/binomial/normal reuse settled
  truncation kernels. The generic negative-binomial/geometric CDF path refuses
  rare normalization or quantile transformations whose floating-point
  conditioning cannot meet its margin. A direct conditional-tail backend is
  needed to extend those numerical regions; it currently returns no unchecked
  probabilities or samples there.
- **Reserved compositions and inference:** zero + window, censor + zero/window,
  additional censoring schemes, modified/censored covariance and censored
  parametric-bootstrap mechanisms remain explicit refusals.
- **Deletion and 2.0 migration:** the retained samples establish initial
  equivalence, not a complete all-hold/escape/domain retirement audit. Finish
  that audit, route named limit messages to replacement calls, then remove the
  16 families/wrappers and add the authorized 2.0 NEWS migration entries. No
  aliases or removals were introduced by this job.

## Artifact display

The report was submitted to the artifact pane with the prescribed `show`
command. The viewer returned HTTP 403 because this worktree is outside its
configured readable roots. No files were copied into another checkout and no
viewer configuration was changed. The report remains at its requested path.
