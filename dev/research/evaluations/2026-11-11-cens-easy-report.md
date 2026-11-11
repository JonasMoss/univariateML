# Right censoring: core type, closed forms and Weibull with free shape

2026-11-11. TASK-142.2, TASK-142.3, TASK-142.4. Baseline master `206e979f`.
Plan: [censoring support tiers](../../roadmap/2026-11-05-censoring-truncation.md#censoring-support-tiers-2026-11-11).
The first pass was written by Codex (job `cens-easy`, stopped before its report).
Claude reviewed it, then added Weibull with a free shape and this report.

C++ only. No adapter, codegen, JS, front-matter or golden changes; the R API,
metadata and refusals are TASK-142.1.

## What was added

- `core/censored.hpp`: `CensoredData {lower, upper}` (two spans), with
  `classify_censored(x, support_lower)` counting exact (`lower == upper`),
  right-censored (`upper = +inf`, finite `lower`), left-censored
  (`lower = -inf` or at or below the support's lower bound) and
  interval-censored observations. Cells are half-open, $(l, u]$.
- One right-censored fitter per family in a new `censored.hpp`, each with its
  own status enum (`Ok`, `InvalidInput`, `AllCensored`, `Unbounded`,
  `NotImplemented`, `NumericalFailure`). Existing fitters, enums and bindings
  are untouched. Left- or interval-censored input returns `NotImplemented`.

| family | hold sets | method |
|---|---|---|
| exp | rate free; rate held (evaluation) | $\hat\lambda = d/T$ |
| weibull | shape held | exp on $x^k$ |
| weibull | shape free (scale free or held); both held (evaluation) | certified bisection on a strictly decreasing score |
| rayleigh | sigma free | exp on $x^2$ |
| pareto | minimum held | exp on $\log(x/m)$ |
| lomax | lambda held | exp on $\log(1+\lambda x)$ |

Other hold sets return `NotImplemented`. $d$ is the number of exact
observations, $T$ the sum over all (transformed) times.

## Why each answer is the unique maximiser

Closed forms. On transformed times $y = g(x)$ the censored log likelihood is
$d \log\theta - \theta T + J$, with $J$ the Jacobian of the exact observations
(free of $\theta$). For $d \ge 1$ and $T > 0$ it is strictly concave in
$\theta$ with the unique root $\hat\theta = d/T$. $d = 0$: the likelihood
increases to $0$ as $\theta \to 0$ and is not attained (`AllCensored`, the
all-censored atom of the roadmap). $T = 0$ with $d \ge 1$: unbounded as
$\theta \to \infty$ (`Unbounded`). For Weibull and Rayleigh the free
coefficient is the scale, $\hat\sigma = (T/d)^{1/k}$ and
$\hat\sigma^2 = T/(2d)$; monotone reparametrisations keep uniqueness.

Weibull, both free (proved here). Profile out the scale,
$\sigma^k = \sum_{\text{all}} x_i^k / d$. With $t_i = \log(x_i / x_{\max}) \le 0$
over all positive times and weights $w_i = e^{k t_i}$, the profile score is

$$
G(k) = \frac{d}{k} + \sum_{\text{exact}} t_i - d\,\frac{\sum w_i t_i}{\sum w_i}.
$$

The weighted mean $\sum w t / \sum w$ has derivative in $k$ equal to the
weighted variance of $t$, which is nonnegative, so $G$ is strictly decreasing
($d/k$ is). $G \to +\infty$ as $k \to 0$ and
$G \to \sum_{\text{exact}} t_i$ as $k \to \infty$ (weights concentrate on
$t = 0$). So a maximiser exists, and is unique, exactly when some exact
observation lies strictly below the largest time, exact or censored.
Otherwise the profile likelihood increases without bound (collapse onto the
exact cluster): `Unbounded`.

This is finding 4 of the roadmap for Weibull: an exact cluster with all
censoring at or below it collapses, a single censoring time above it bounds
the likelihood, and censoring below the cluster does not help.

Weibull, scale held, shape free (proved here). With $z_i = \log(x_i/\sigma)$,
$\ell(k) = d \log k + \sum_{\text{exact}} (k z_i - \log x_i) - \sum_{\text{all}} e^{k z_i}$
is strictly concave ($-d/k^2 - \sum z^2 e^{kz} < 0$). Its score tends to
$+\infty$ as $k \to 0$ and, as $k \to \infty$, to $-\infty$ if some $z_i > 0$
and to $\sum_{\text{exact}} z_i$ otherwise. No maximiser exactly when every
time is at most $\sigma$ and every exact time equals $\sigma$: `Unbounded`.

Computation: both scores are evaluated in extended precision on relative log
ratios and bisected on $\log k$ over the whole binary64 range. The bracket is
certified by the score's sign at both ends and stops at width $0.01\,\mathrm{tol}$
in $\log k$, the same rule as the exact-data `weibull_mle` held-scale path. The
scale then comes from the closed-form held-shape path.

## Evidence

- Independent references, maximised directly from the censored log
  likelihood, never from the closed forms or scores:
  - `oracle.py` (Codex): 50 digits, golden section in the log coefficient,
    75 cases (5 families; scales 1, 1e-200, 1e200; 0, 30 and 90% censoring,
    ties, a single observation).
  - `oracle_weibull_free.py` (Claude): 30 digits, nested golden sections over
    log shape and log scale, 24 cases (shape free with scale free or held at
    twice the sample scale; the same scales; 0, 30 and 90% censoring; an exact
    cluster with censoring above it).
- C++ tests appended to each family's `test.cpp` compare against them within
  relative `1e-10` (coefficients) and `1e-12` (log likelihood). They also check
  that fully exact input reproduces the existing fitters, and cover the
  existence boundaries above, refusals and input validation.
- Codex's native and double-only comparisons (`native.csv`, `double.csv`,
  `check.cpp`): worst relative coefficient error 3.0e-14, worst log-likelihood
  error 9.1e-13, exact input within 1 ulp of the existing fitters.

## Final runs

- simbox (GCC 13), `just cpp-test` (dev tier): 108/108 passed.
- simbox (GCC 13), `just cpp-test-no-long-double`: 134/135; the one failure is
  the master baseline issue `distribution_bnbinom` below, not this work.
- Workstation (GCC 15), the no-long-double shards of the five changed families:
  5/5 passed (`distribution_{exp,weibull,rayleigh,pareto,lomax}`), 178 assertions in the 6 Weibull censoring cases.
- Not run: `just js-build`/`js-test`. `core/censored.hpp` is new and no binding includes it.

## Baseline issue found on the way

Codex found that `bnbinom/test.cpp:373` (`CHECK(a.logLik==b.logLik)`, exact
equality) fails under GCC 13 (simbox) on unmodified master `206e979f`, with
values equal to 6 digits. Evidence: `bnbinom-baseline*.txt`,
`header-identity.json`. It is unrelated to this work.

## For TASK-142.1

- Signatures: `<id>_censored_mle(CensoredData x, <coefficients as NaN-or-held>...)`;
  Weibull also takes `tol` and `maxit`. Status names are shared across families.
- The R adapter maps `NotImplemented` to the specific refusal and
  `AllCensored`/`Unbounded` to existence messages. Never fall back to the
  exact fitter on the recorded values.
- Dossier sentences to add with the metadata: the Weibull existence criterion
  (an exact observation below the largest time) and the held-scale
  criterion above.
