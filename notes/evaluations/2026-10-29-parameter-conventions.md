# Parameter conventions across the 98 families (2026-10-29)

Every parameter with its README flags, adapter default and the reference
function's default is in `2026-10-29-parameter-table.csv` (generator:
`2026-10-29-parameter-table.R`). This note lists where the families do not
follow one rule and the choice each case needs. Probes used the build of
master plus TASK-76.

## 1. Endpoints and thresholds

Five behaviours exist for a parameter that is a support endpoint:

| behaviour | families |
|---|---|
| estimated as the sample extreme by default | pareto `b`, power `alpha`, unif, dunif, zipf `N`, triang `a`/`b`, hnorm `mu` |
| required, no default (`required_reason`) | pareto2, pareto3, pareto4, fpareto `min`; nsbeta `min`/`max`; genbeta `scale` (upper endpoint); tpois, tbinom `a`/`b` |
| held at a default, settable | gpd `mu = 0`; tnorm `a = -Inf`, `b = Inf` |
| held, not settable | fatigue `mu = 0` |
| no endpoint parameter | hcauchy, ht (at 0); every positive-support family (threshold versions deferred, `roadmap/2026-10-29-classical-failure-families.md`) |

The working rule is: estimate the endpoint when its free MLE exists, require
it when the likelihood is unbounded with it free. The exceptions:

- **gpd.** A free threshold makes the likelihood unbounded, as for Pareto II
  (Pareto II is the GPD with $\xi > 0$), yet `mu` defaults to 0 where
  `mlpareto2` requires `min`. With data above 5, `mlgpd(x)` returns
  `sigma = 7.2, xi = -0.5` (the shape bound) without a message;
  `mlgpd(x, mu = 5)` gives `xi = 0.24`. Proposal: `mu` required, with a
  reason, as for pareto2.
- **fatigue.** `mu` is held at 0 and cannot be set; gpd's threshold can.
  Proposal: settable, default 0 (the two-parameter Birnbaum-Saunders).
- **hnorm.** `mlhnorm(x)` estimates `mu` (the sample minimum), while
  hcauchy and ht sit at 0 with no location. On `abs(rnorm(100))` the
  default gives `mu = 0.033`. The standard half-normal is at 0. Proposal:
  hold `mu = 0` by default, `mu = NULL` estimates (huber's pattern).
  mlhnorm is new in 1.5.0, so nothing breaks.
- **Truncation bounds.** tnorm defaults to no truncation (`mltnorm(x)` is
  `mlnorm(x)`); tpois and tbinom require both bounds. Proposal: required
  for all three, with infinite bounds allowed explicitly.

## 2. Ranges narrower than the family

| family | restriction | reason (README) |
|---|---|---|
| ged, sged | `nu >= 1` | unbounded as $\nu \to 0$ (D6) |
| lst | `df >= 1` | unbounded through scale collapse below 1 |
| gev | `xi` in $[-1/2, 1]$ | unbounded below $-1$ and for large positive shapes |
| gpd | `xi >= -1/2` | regularity; unbounded below $-1$ |
| frechet | `lambda >= 1` with free `mu` only | the GEV route, $\xi = 1/\lambda \le 1$ |
| tlambda | `lambda` in $[-1, 1]$ | unbounded as `lambda` grows |

(std and sstd `nu > 2` is part of their standardized parametrization.)

Data beyond a restriction gives the boundary value, silently:

| data | fit |
|---|---|
| Fréchet, `lambda = 0.6`, free `mu` | `lambda = 1.0000` |
| Fréchet, `lambda = 0.6`, `mu = 0` held | `lambda = 0.665` (no restriction) |
| GEV, `xi = 1.6` | `xi = 1.000` |
| location-scale t, `df = 0.6` | `df = 1.000` |
| GED, `nu = 0.6` | `nu = 1.000` |
| Cauchy into tlambda | `lambda = -1` |

Each is the restricted MLE, and the README says so (gev: "fits at either shape bound are returned as attained members"), but neither the printed fit nor the object says so.
Options: (a) keep and document; (b) warn when an estimate sits on a
restriction boundary and record it in an attribute; (c) stop, like the
escapes. Proposal: (b).

## 3. Which parameters can be held

- **All free parameters fixable (23):** bbinom, bnbinom, cmb, cmp, dgamma,
  dlaplace, dnorm, dweibull, emg, foldnorm, fpareto, gb2, genbeta, ht,
  huber, pig, sgomp, skellam, sn, tbinom, tnorm, tpois, triang.
- **None (48):** among them every 1.x family: norm, exp, gamma, weibull,
  beta, lnorm, logis, cauchy, laplace, gumbel, invgamma, invgauss, llogis,
  lomax, kumar, pareto, power, unif, pois, geom, zip, …
- **Some (27):** these look arbitrary:
  - trgamma and invtrgamma hold their shapes but not scale;
  - pareto4 holds only scale; pareto2 and pareto3 hold nothing;
  - std holds `nu`, sstd does not;
  - gev and gpd hold `xi` only;
  - the scale-only cases (betapr, burr, paralogis, ...) are 1.x leftovers
    from a scale fixed at 1.

`mldgamma(x, shape = 2)` works; `mlgamma(x, shape = 2)` does not.
Proposal: every parameter fixable, family by family. Each held subset is
its own existence question; most reduce to a one-dimensional profile.

## 4. What `NULL` means

For fixable parameters `NULL` estimates. Exceptions:
- tnorm `a`/`b`: `NULL` means an infinite bound, held;
- huber `epsilon`: held at 1.345 by default, `NULL` estimates;
- gpd `mu`: defaults to 0.

Proposal: `NULL` always estimates, and held defaults are values in the
signature (`a = -Inf`).

## 5. Rate or scale

All positive families with an actuar reference could use either, since
actuar accepts both:
- **rate:** llogis, invweibull, invexp, and lomax (`lambda`, extraDistr);
- **scale:** burr, invburr, paralogis, invparalogis, trgamma, invtrgamma,
  gb2, genbeta, fpareto, pareto2–4.

The rate cases are 1.x return values. Proposal: scale throughout, except
exp and gamma, which keep the stats convention.

## 6. Binomial-type `size`

- **Estimated by default:** binom (1.x behaviour), zib, zmbinom, ztbinom.
- **Required:** bbinom, cmb, tbinom.

A free `size` is the classic ill-posed problem: underdispersed data escape
to the Poisson. Proposal: required everywhere.

## 7. Metadata slips (mechanical)

- zmnbinom and ztnbinom label `size` as "trials" with domain $[0, \infty)$;
  it is a real shape (nbinom, zinb: "shape", $(0, \infty)$). actuar allows
  `size = 0` for the zero-truncated case (the logarithmic limit), so the
  closed domain may be deliberate; the role is not.
- Display names from 1.x read oddly in the README table: "InverseWeibull",
  "LogitNormal", "PowerDist", "BetaPrime", "Loggamma", "Loglogistic".

## 8. Left alone

- Parameter names follow each reference (`mu`/`mean`/`location`,
  `df`/`nu`); own families follow their literature (sn: `xi`, `omega`,
  `alpha`).
- `default:` values are simulation values for goldens and
  `simulate_default` (gumbel 3/3, weibull 3/4, logitnorm 2/3). They are
  arbitrary but not user-facing; changing them changes goldens.

## Found on the way: scale equivariance (TASK-77)

Not a judgement call, a bug. `mlstd(diff(log(EuStockMarkets[, "DAX"])))`
returns the start value `nu = 6`:
- its logLik is 5976.05, against 5983.32 at the optimum;
- at 100 times the data it finds `nu = 4.19`;
- below a data sd of about 0.03 it always returns the start value;
- `mlsstd` does the same (`nu = 6`, `xi = 1`).

The scale probe (`2026-10-29-scale-equivariance.R`) also finds:
- `mlsstd(1e-3 * x)` stops with a false "`nu -> 2`" escape;
- `mlgompertz(1e3 * x)` fails;
- power (`epsilon`) and fatigue (`abstol`) use absolute tolerances.
