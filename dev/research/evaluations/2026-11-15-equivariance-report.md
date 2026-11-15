# Near-constant samples of equivariant families: causes and fixes (TASK-214.41)

Code analysis only: C++ was read, nothing was built or run. Quantities were
emulated in binary64 (Python floats, `math.log`) or computed in mpmath
(80 digits). Scripts, outputs and the extracted snapshot rows are in
[2026-11-15-equivariance-data/](2026-11-15-equivariance-data/). Snapshots:
`2026-11-15-fit-snapshot-data/{native,double}.jsonl.gz`. Native and double
builds end identically for all 13 families on both samples, so every table
below holds for both.

## 1. Setup

The near-constant sample is $[c, c, c^+]$ with $c = q_{50}$ and $c^+$ the next
binary64 number; `tie3` is $[q_{50}, q_{50}, q_{75}]$ of the same golden
sample. The declared map $T$ sending `tie3` onto it is

- affine, $x \mapsto A + Bx$ with $B = (c^+ - c)/(q_{75} - q_{50})$ and
  $A = c - B q_{50}$;
- power-scale, $x \mapsto A x^B$ with $B = \log(c^+/c)/\log(q_{75}/q_{50})$ and
  $A = c / q_{50}^B$.

`mapped.py` checks $T(q_{50}) = c$ and $T(q_{75}) = c^+$ to $10^{-80}$. $B$ lies
between $10^{-18}$ and $5 \times 10^{-16}$, so mapped shape and rate
coefficients are $\approx 10^{16}$ and mapped spreads $\approx 10^{-17}$.

Two readings of "representable":

- **coefficient level**: every mapped coefficient is finite, normal, and
  rounds with relative error at most $8 \times 10^{-17}$. True for all 11
  families that have coefficients (the maximum is `sn`, $7.8 \times 10^{-17}$);
  `lst` has none ($N$) and `expweibull` fails on `tie3` as well.
- **likelihood level**: the loss $\ell(\theta^\ast) - \max_{\hat\theta \in
  \text{neighbours}} \ell(\hat\theta)$ at the exact binary64 data, over the two
  binary64 neighbours of each coefficient. This is where the families differ
  (section 2).

The accuracy contract
([accuracy-contract.md](../../design/accuracy-contract.md), "steep maximizer
that is not representable", decided 2026-11-14) says what follows from a
likelihood-level loss: the fit returns the rounded point and `logLik` is the
log likelihood at the returned coefficients, not the supremum; the normal's
mean on $(1, 1, 1 + 2^{-52})$ loses $0.75$ and is the stated example. So a
coefficient-representable maximizer with a likelihood-level loss is an
estimate (or limit) at the rounded point, not `F:evaluation`.

## 2. Outcomes, mapped answers, representability

`E` estimate, `L` limit, `N` nonexistence, `F` failure. All numbers from
`mapped.py` / `mapped.json` ($\ell$ in nats, $n = 3$).

| family | `tie3` outcome (coefficients) | near-constant (snapshot) | mapped `tie3` answer on $[c,c,c^+]$ | likelihood loss by rounding | expected outcome |
|---|---|---|---|---|---|
| burr | L pareto | F:evaluation | Pareto, shape $2.6539 \times 10^{16}$, min $c$ | $< 10^{-31}$ | L pareto |
| pareto4 | L pareto (min held 0) | F:evaluation | Pareto, shape $2.0306 \times 10^{16}$, min $c$ | $< 10^{-31}$ | L pareto |
| gengamma | L pareto | F:no-convergence | Pareto, shape $2.4900 \times 10^{16}$, min $c$ | $< 10^{-33}$ | L pareto |
| invburr | L invweibull | F:evaluation | inv. Weibull, scale $1.05023714338653476$, shape $1.5101 \times 10^{16}$ | $0.196$ | L invweibull (coefficients steep) |
| trgamma | L power | F:evaluation | power, $\alpha = c^+$ (the maximum), $\beta = 1.3367 \times 10^{16}$ | $< 10^{-31}$ | L power |
| sn | L hnorm | F:no-convergence | half-normal, $\mu = c$, $\sigma = 6.4099 \times 10^{-17}$ | $< 10^{-32}$ | L hnorm |
| std | L norm | F:evaluation | normal, mean $c + \tfrac13 \mathrm{ulp}$, sd $8.18 \times 10^{-19}$ | $0.75$ (as `norm`) | L norm (mean steep) |
| emg | L translated exponential (unavailable) | F:evaluation | translated exponential, $\mu = c$, rate $2.7022 \times 10^{16}$ | $< 10^{-33}$ | L, same message as `tie3` |
| lst | N:unbounded | F:evaluation | none (collapse, $n = 3$, $k = 2$, $e^\ast = -1$) | n/a | N:unbounded |
| fpareto | E: shape2 $10.019$, scale $1.5026$ (min 0, shape1 $.3$, shape3 $.4$ held) | F:evaluation | shape2 $6.3829 \times 10^{16}$, scale $1.41701840141571635$ | $0.058$ | E at the best neighbours (below) |
| huber | E: mu $.2251$, sigma $.3464$ ($\epsilon = 1.345$ held) | F:no-convergence | $\mu = c + 0.3015\,\mathrm{ulp}$, $\sigma = 0.4483\,\mathrm{ulp}$ ($3.889 \times 10^{-19}$) | $0.452$ | E at the best neighbours |
| triang | E: $a = c = .46656$, $b = .66430$ | F:evaluation | $a = c = c_0$, $b = c_0 + \tfrac43\,\mathrm{ulp}$ | $0.523$ | E at the best neighbours |
| expweibull | F:no-convergence | F:no-convergence | none: the preimage fails too | n/a | not an equivariance failure (section 3.7) |

Best neighbours (`best_neighbour_coefficients`, `best_neighbour_ll` in
`mapped.json`), with the supremum $\ell(\theta^\ast)$ for comparison:

- fpareto: shape2 $6.382898004995409 \times 10^{16}$, scale
  $1.4170184014157163$, $\ell = 106.1735$ against $106.2314$;
- huber: $\mu = -0.007812325130757122$ ($= c$), $\sigma = 3.8886717919813423
  \times 10^{-19}$, $\ell = 122.1419$ against $122.5941$;
- triang: $a = c = 0.4665625208523124$, $b = 0.46656252085231253$
  ($c_0 + 2\,\mathrm{ulp}$; $c_0 + 1\,\mathrm{ulp}$ is the sample maximum and
  has density 0 at the maximum for mode $a$), $\ell = 111.5967$ against
  $112.1199$. The continuous maximizer has $b = c_0 + \tfrac43\,\mathrm{ulp}$:
  $\ell(b) = 3\log 2 + \log(b - 1) - 4\log b$ in ulp units, maximal at
  $b = 4/3$; at $b = 2$ it is lower by $0.523$.

The limit-outcome rows (invburr, std) report the limit model's coefficients;
for those the likelihood loss belongs to the target family's own fit. The
snapshot shows the precedent on the same kind of sample: `norm` ends in a
certified estimate (loss $0.75$), `invweibull` in an uncertified one,
`pareto`, `power`, `hnorm` in certified ones (their `near-constant` rows).
Whether the L outcome of invburr and std carries `certified = false` is a
reviewer decision; the strict rule only asks for the kind and reason of the
preimage.

Tolerance for "within $10^{-10}$ relative" would be exceeded by every
nonzero loss above, so the three E rows are steep maximizers in the sense of
the contract, not accurate ones.

## 3. Cause per family

Causes were confirmed by emulating the quoted lines. "Not run" marks the
points where the stop was inferred from the code and no C++ was executed.

### 3.1 burr, pareto4, invburr: absolute log data in the Burr certificate

`burr_cert_detail::data` ([certificate.hpp:59-70](../../../univariateML-cpp/include/cppunivariate/distributions/burr/certificate.hpp))
takes `y = log(v)` in binary64, centers and scales by their standard
deviation, and builds each datum's enclosure as
`(padded(y,y) - center) / unit`. `padded` adds
$\mathrm{pad}\,(1 + \lvert y \rvert)$ with $\mathrm{pad} = 64\,\varepsilon$
([certificate.hpp:15,24](../../../univariateML-cpp/include/cppunivariate/distributions/burr/certificate.hpp)),
an absolute allowance of $1.4 \times 10^{-14}$ around a log whose spread is
$10^{-16}$.

Emulation (`causes_logs.py`, interval class copied from the header):

| | burr | pareto4 | invburr (reciprocals) |
|---|---|---|---|
| spread of exact $\log(x_i/x_0)$ | $1.130 \times 10^{-16}$ | $1.477 \times 10^{-16}$ | $2.114 \times 10^{-16}$ |
| spread of binary64 logs | $1.145 \times 10^{-16}$ ($+1.3\%$) | $0$ (equal logs) | $2.151 \times 10^{-16}$ ($+1.7\%$) |
| `data()` | succeeds, `unit` $= 5.4 \times 10^{-17}$ | fails (variance $0$) | succeeds, `unit` $= 1.1 \times 10^{-16}$ |
| half-width of the standardized box | $531$ | n/a | $262$ |
| `distances` (certificate.hpp:300) | $[-3186, 3190]$ | n/a | $[-1566, 1575]$ |
| $P = n \log(n/\mathrm{distances}) - n$ valid | no | n/a | no |

The standardized data lie in $[-0.71, 1.41]$ while each box has half-width
$\ge 262$. `distances` contains 0, so the division in the Pareto bound (line
303) is invalid and `fit` returns with outcome `Numerical` at line 304
(`if(!P.valid()||...) return result`). For pareto4 the logs of $c, c^+$ are
the same binary64 number, `data()` returns false and `fit` returns at line 297. All three then
go through `burr_uncertified(..., burr_mle_legacy)`, which does not recover
(legacy requires positive variance of the same logs; `BurrStatus` stays
`NumericalFailure`; not run). The message `invburr certificate numerical
failure` and `Pareto IV certificate numerical failure` are the same stop.

invburr adds a second defect: `invburr_mle` forms `1.0/v`
([mle.hpp:172](../../../univariateML-cpp/include/cppunivariate/distributions/invburr/mle.hpp)),
which rounds. The spread of $\log(1/x_i)$ becomes $2.359 \times 10^{-16}$
instead of $2.114 \times 10^{-16}$ ($+11.6\%$): the reciprocal sample is a
different problem, not the exact image. Even with the intervals repaired the
certificate would solve the wrong sample.

**Proposed fix.** Replace the absolute logs in the Burr certificate by
relative logs, in the pattern of `transformed_detail::logs`
([core/transformed_data.hpp](../../../univariateML-cpp/include/cppunivariate/core/transformed_data.hpp)):

```cpp
// data(): anchor a = x_min (or x[0]); delta_i = log_ratio(x_i, a) (log1p form, exact)
// mean = Sum(delta_i)/n; var from (delta_i - mean); unit = sqrt(var)
// box_i = padded((delta_i - mean)/unit)   // O(1) value, so pad is relative
// jacobian = -(n*anchor_log + Sum delta_i) - n*log(unit)   // Pair arithmetic, as logs()
// results: c = c_std/unit; scale = x0 * expm1(mean - b/c) + x0   (as location_fit)
// invburr: delta_i = -log_ratio(x_i, x0) (flag), no reciprocal vector; jacobian -2*Sum log x
```

The other uses of absolute logs in the same function need the same change:
the root neighbourhood `original.y.push_back(y - shift)`
([certificate.hpp:351](../../../univariateML-cpp/include/cppunivariate/distributions/burr/certificate.hpp)),
`d.center` in `scale = exp(d.center - ...)` (lines 356, 403, 406) and
`burr_uncertified`'s `log(v) - log(scale)` (burr/mle.hpp), which cancels to
$\sim\varepsilon\lvert\log x\rvert$ the same way. The standardized problem
for $[c,c,c^+]$ equals that of `tie3` (both are the pattern $[-0.707,
-0.707, 1.414]$), so the certificate takes the same decisions as on `tie3`.

**Expected result.** burr L pareto, pareto4 L pareto, invburr L invweibull,
as in section 2. **Risks:** `limit_loglik` in `burr_mle_certified` sums
`dpareto` at shape $2.7 \times 10^{16}$; `fit-snapshot` shows `pareto` itself
handles this sample (E, certified), so `dpareto` is accurate there. Fixed
holds (`burr_mle_holds`, `fixed_scale.hpp`, `scale_certificate.hpp`) have
their own `std::log(v)` and are not exercised by these cases; they need the
same treatment in a later batch.

### 3.2 trgamma, fpareto: absolute logs in the shared GB2 / trgamma `LogData`

**trgamma.** `trgamma_mle` computes `y[i] = std::log(x[i])`
([mle.hpp:874](../../../univariateML-cpp/include/cppunivariate/distributions/trgamma/mle.hpp))
and then stops at line 877 if two distinct observations have the same log
(`lost_log_distinctions`, defined at line 117). On $[c, c, c^+]$ with
$c = 3.957$: $\log c \approx 1.375$ has spacing $2.2 \times 10^{-16}$, the
exact log difference is $1.12 \times 10^{-16}$, so the two binary64 logs are
equal (`causes_logs.out`, `lost_log_distinctions: true`). That is the
message "Distinct observations ... became indistinguishable after taking
logarithms" (snapshot, confirmed).

**fpareto.** The held-shape fit goes through `gb2_mle` with shape1, shape3
held: `logs[i] = std::log(x[i])`
([gb2/mle.hpp:283](../../../univariateML-cpp/include/cppunivariate/distributions/gb2/mle.hpp)),
`trgamma_detail::standardize` (line 363), then
`gb2_concave_detail::fit` (line 377). Its acceptance gate
([concave_certificate.hpp:34-60](../../../univariateML-cpp/include/cppunivariate/distributions/gb2/concave_certificate.hpp))
inflates the gradient by the input error of the logs,
`dv = 8 eps (1 + |mean| + sd |z| + |logb|) / sd`, and needs
`g < m * radius / 2`. Emulated at the `tie3` optimum
($A = 4.715$, $c = -2.746$): $\mathrm{sd} = 7.85 \times 10^{-17}$, so
$\mathrm{dv} = 39.7$, `gradient_error` $= 169$, while
$m\,\mathrm{radius}/2 \le 8.6 \times 10^{-12}$ (`causes_logs.json`,
`fpareto_gb2_concave`). The gate cannot pass; the loop ends with
`NumericalFailure` (`F:evaluation`). Binary64 logs also distort the data:
spread $1.67 \times 10^{-16}$ against exact $1.57 \times 10^{-16}$ ($+6\%$).

**Proposed fix.** Build `LogData` from relative logs: take
`d.u_i = (delta_i - mean_delta)/sd` with `delta_i = log_ratio(x_i, x_ref)`,
keep `d.mean` as the pair `anchor + mean_delta` (public scale
`= exp(mean + sd c/A)` becomes `x_ref * expm1(mean_delta + sd c/A) + x_ref`
when $\lvert\cdot\rvert < 0.5$), `d.sum` as `Sum`, and use
`dv = 8 eps (1 + |delta mean| + ...)` with the anchor term dropped. Remove
`lost_log_distinctions` for callers that pass relative logs (its premise,
logs merging distinct observations, no longer holds). `standardize` is also
called by gengamma (section 3.3), burr's Lomax fallback and trgamma's own
fixed-shape routes, so `LogData` gets a second constructor and the callers
move one family at a time.

**Expected result.** trgamma: L power, $\alpha = c^+$, $\beta = 1.337 \times
10^{16}$ (loss $< 10^{-31}$). fpareto: the gate now sees $\mathrm{dv} \approx
8\varepsilon \times O(1)$ and passes, so the fit ends in the concave Newton
solution, shape2 $\approx 6.383 \times 10^{16}$, scale $1.41701840141571635$;
the scale rounds to $1.4170184014157163$ and $\ell$ falls by $0.058$ (section
2). Under the 2026-11-14 decision that is E at the rounded point, with the
`representable` branch of line 59 (`nextafter(scale) - scale < target *
scale / 4`) true; whether `restored`-style acceptance then returns E
uncertified or the existing local-accuracy check (`transport_accuracy.hpp`,
`fpareto_transport_accuracy`) rejects the 0.058 loss is not decidable
without running. **Risk:** if the transport check rejects it, fpareto ends in
`F:tolerance`/`conditioning`, which the rule also allows only as
`conditioning`; the intended E needs the check to follow the contract.

### 3.3 gengamma: own absolute-log error model

`gengamma_mle` uses `y = log(v)` and the allowance
`log_error = max 8 eps |z|`
([mle.hpp:49-53](../../../univariateML-cpp/include/cppunivariate/distributions/gengamma/mle.hpp)),
passed to every certificate. On $c = 0.9215$: $\log_{\rm error} = 1.45 \times
10^{-16}$, sd of the logs $5.89 \times 10^{-17}$ (`causes_logs.json`). The
face certificate's arithmetic term
([face_certificate.hpp:27](../../../univariateML-cpp/include/cppunivariate/distributions/gengamma/face_certificate.hpp))
`16 * data_error / data_scale` is $39.5$ added to every seam bound, while the
bound it must beat is $O(1)$. So `side_below_face` never excludes the normal
seam, `strict_winning_face` (mle.hpp:164) returns no winner, and the Pareto
face that wins on `tie3` is not certified. The final return
`IterationFailure` (`F:no-convergence`) is on mle.hpp:166-175 or 202-204;
which of these fires is not run. The logs are not merged
(`lost_log_distinctions` false; the spread $1.249 \times 10^{-16}$ in
binary64 against $1.205 \times 10^{-16}$ exact, $+3.7\%$).

**Proposed fix.** Same class as 3.2: represent `y` as relative logs
`delta_i` about an anchor (the code already centres by `mean`), so
`log_error` becomes $8\varepsilon\lvert\delta\rvert$ plus the anchor's error
handled once through the pair type, and `data_error/data_scale` is $O(\varepsilon)$.
`mu` is the location of $\log x$ and stays absolute in the public output.
**Expected:** L pareto (shape $2.49 \times 10^{16}$, min $c$).
**Risks:** `gengamma` has the widest surface of the group (`held.hpp`,
`local.hpp`, `shape_*`, `zero_certificate.hpp` all take `logs`); the mapped
$\mu = \log A + B\mu_0$ is representable only to $\sim 8\%$ of $\sigma$
($\mathrm{ulp}(0.0817) = 1.4 \times 10^{-17}$ against $\sigma \approx
10^{-16}$), so for the finite branch the same steep-maximizer clause
applies; that branch is not reached here because the face wins.

### 3.4 lst, std: error allowance counts the centre in the interval standardization

`t_cert_detail::data`
([lst/certificate.hpp:126-142](../../../univariateML-cpp/include/cppunivariate/distributions/lst/certificate.hpp))
standardizes exactly ($v - \text{centre}$ is a Sterbenz subtraction, so the
standardized value carries relative error $\le 2\varepsilon$) but then widens
each value by

```cpp
const double error=eps*(1+std::abs(v/d.unit)+std::abs(d.center/d.unit)+std::abs(value));   // line 137
```

with `eps` $= 64\,\mathrm{DBL\_EPSILON}$ (line 18). With
$\mathrm{unit} = 8 \times 10^{-18}$ and $v \approx 0.0749$, this is $266$
for lst and $364$ for std (`causes_t.out`), against standardized values in
$[0, 1.73]$ (the exact-subtraction allowance would be $1.4 \times 10^{-14}$).
The summed enclosure of the variance becomes $[-2.8 \times 10^{-8},
2.8 \times 10^5]$ (lst) and $[-5.3 \times 10^{-8}, 5.3 \times 10^5]$ (std),
so `d.var.lo <= 0` and `data()` returns false (line 141). `t_certificate`
(line 411) then labels the sample numerical unless it is constant:
`F:evaluation`. On `tie3` the allowance is $4 \times 10^{-14}$ and the
variance interval is $[1 - 6 \times 10^{-13}, 1 + 6 \times 10^{-13}]$.

**Proposed fix.** The absolute terms `|v/unit|`, `|center/unit|` model
operand cancellation that does not occur: `fl(v - center)` has relative error
$\varepsilon$ of its own result, whatever the operands. Use
`error = eps * (1 + |value|)`. Separately, evaluate the collapse theorem
before `data()` for lst: it needs only exact equalities
(`mass > floor*(n-mass)`, line 430), and a mass-2 atom among 3 points gives
`Unbounded` at floor 1.

**Expected result.** lst: N:unbounded (mass $2 > 1 \times 1$), which also
closes `collapse-not-reported`. std (floor 2: $2 > 2$ is false): the
certificate runs on the standardized sample, which is `tie3`'s, so it should
end L norm. **Risk (std, not run):** `location_units`/`scale_units`
(lines 41-47) add $\varepsilon(\lvert\text{centre}\rvert + \ldots)$ to every
public-coordinate enclosure, i.e. $\approx 10^{-14} \times 0.0128$ against
$\mathrm{sd} = 8 \times 10^{-19}$; any later gate on the coefficient
enclosures of the normal limit would fail again. If so, the normal limit's
coefficients should come from `norm_mle`'s result as in `lst_mle`
(mle.hpp:557-585) and not from the certificate's enclosure.

### 3.5 emg: certificate coordinates built on raw data

`CertModel::initial`
([certificate_math.hpp:104-116](../../../univariateML-cpp/include/cppunivariate/distributions/emg/certificate_math.hpp))
starts at $b = \sqrt{1 + 1/k^2}/\mathrm{sd}$, $u = b\bar x - 1/k$, and
`cert_evaluate` computes $z = b v - u$ on the raw data with
`zerror = 64 eps (|b v| + |u|)` (line 203). At the profile start $k = 1$ on
$[c, c, c^+]$ ($c = 0.912$, $\mathrm{sd} = 6.41 \times 10^{-17}$):
$b = 2.2 \times 10^{16}$, $u = 2.0 \times 10^{16}$, $\mathrm{ulp}(u) = 4$,
$b(c^+ - c) = 2.45$ (`causes_loc.out`). The three computed $z$ are exactly
`[0, 0, 0]` and the allowance is $572$. So
`fixed_shape_certificate` fails, `profile(start)` returns `nullopt`
([certificate.hpp:205](../../../univariateML-cpp/include/cppunivariate/distributions/emg/certificate.hpp)),
`stopped()` returns `NumericalFailure` (`F:evaluation`). The `emg` message
"score enclosure, shape bound or arithmetic" is the same stop.

**Proposed fix.** The sn pattern
([sn/mle.hpp:242-254](../../../univariateML-cpp/include/cppunivariate/distributions/sn/mle.hpp)):
at the top of `emg_accuracy_search` set `origin = x_min + (x_max - x_min)/2`,
`scale = (x_max - x_min)/2`, `y_i = (x_i - origin)/scale` (exact for ulp
spacing), map held `mu` by `(mu - origin)/scale`, held `sigma` by
`sigma/scale`, held `lambda` by `lambda * scale`, run the certificate on
`y`, and map back `mu = origin + scale*mu'` (with `fma`),
`sigma = scale*sigma'`, `lambda = lambda'/scale`. The limit coefficients,
`translated exponential` $\mu = x_{\min}$ and rate, are produced by
`cert_exponential_face` from the raw data and need no map back except the
rate's. `emg_polish_result` already works in centred coordinates
(`center = r.mu`, `unit = r.sigma`, mle.hpp:75).

**Expected result.** The standardized sample is `tie3`'s, so the same stop as
`tie3`: L translated exponential, $\mu = c$, rate $2.70 \times 10^{16}$ (loss
$10^{-33}$), message "No translated exponential fitter is available".
**Risks:** `location_certificate` (the location-only slice, routed before
standardization on purpose, mle.hpp:25-33) must keep working on raw data;
mapping a held `mu` far from the sample may overflow `(mu - origin)/scale`.

### 3.6 sn, huber, triang: the acceptance check runs in public coordinates on a steep maximizer

These three already standardize (sn, huber: centre and range; triang:
`origin = x.front()`, `scale = range`). Their standardized solve is exactly
`tie3`'s. They fail afterwards, at a check that evaluates the result at the
rounded public coefficients. This is the "steep maximizer" case of the
contract.

**sn.** `sn_accuracy_search`
([mle.hpp:242-254](../../../univariateML-cpp/include/cppunivariate/distributions/sn/mle.hpp))
standardizes with `origin`, `scale`, `mean`, `sd`. At line 447
(`if(!isfinite(actual) || abs(actual-(best.ll - n log unit)) > 1e-9*n)`) the
finite candidate `best` is re-evaluated at the public `(center + unit h/eta,
unit/eta)` and, on failure, `bad.status = IterationFailure; return bad`.
This return comes before the half-normal faces are compared (the face loop
follows), and the face is what wins on `tie3`. Emulation
(`causes_loc.py`, standardized sample $[0,0,1]$, $\xi$ rounded to the lattice
$c + k\,\mathrm{ulp}$): the best finite candidate loses $0.36$ ($\alpha = 3$),
$0.96$ ($\alpha = 10$), $1.22$ ($\alpha = 30$), $1.33$ ($\alpha = 100$),
all $\gg 10^{-9} n$. The half-normal face has loss $< 10^{-32}$ at
$\mu = c$, $\sigma = 6.41 \times 10^{-17}$ (`mapped.json`), and the face
value $-0.5295$ exceeds every finite candidate on the grid
($-0.584$ at $\alpha = 100$). That the fit ends at line 447 is inferred (the
message "did not converge or could not certify the maximum" is
`IterationFailure`; not run).

**Fix.** An unrepresentable finite candidate must not end the search. Record
it as a witness and go on to the face comparison; compare the face's
public-coordinate likelihood with the finite candidate's public-coordinate
likelihood, as the block from line 456 already does for faces.
Expected: L hnorm. Risk: when no face wins, the same candidate must come back
as E with `logLik` at the returned coefficients (contract), not as
`IterationFailure`.

**huber.** `huber_fixed_mle` standardizes and solves (mu offset
$0.3015\,\mathrm{ulp}$, sigma $0.4483\,\mathrm{ulp}$), then
`huber_mle` evaluates `huber_nuisance_upper` at the public point
([mle.hpp:666-674](../../../univariateML-cpp/include/cppunivariate/distributions/huber/mle.hpp)).
`mu` rounds to $c$ (offset $0$), so the observations sit at
$z = (0, 0, 2.2305)$; there $g = \sum z\psi = 3.0$ and
$\mathrm{move} = h (x_{\max} - \mu)/\sigma = 3.0$, hence $k = g - \mathrm{move}
= 0$, and line 603 (`if(!(k>0)) return INFINITY`) makes the upper bound
infinite; `upper - logLik <= allowance` fails and `status = IterationFailure`
(`F:no-convergence`). Emulated exactly in `causes_huber.py`
(`nuisance_upper_terms`). **Fix:** the supporting-plane bound is correct but
uses the rounded `mu` as its tangent point; compute it at the standardized
optimum (before rounding) and add the rounding loss separately, or accept
any point whose public likelihood is within the contract's steep-maximizer
clause and report `logLik` at the returned point. **Expected:** E at
$\mu = c$, $\sigma = 3.889 \times 10^{-19}$, $\ell = 122.14$.

**triang.** `triang_mle` centres and scales, finds $a = c = x_{\min}$,
$b' = 4/3$ in range units, and restores `out.b = origin + scale*bb`, which
rounds to the sample maximum and is pushed outward to
`nextafter(maximum)` (lines 295-296). The public likelihood differs from the
predicted by $0.52$, `same_point` is false, `rounded_endpoint` is true or the
global gap test fails, and `represented` is false
([mle.hpp:301-323](../../../univariateML-cpp/include/cppunivariate/distributions/triang/mle.hpp)),
giving `TriangStatus::NumericalFailure` (outcome `F:evaluation`, line 337).
The message in the snapshot, "Triangular public coefficients lose likelihood
beyond the numerical certificate margin", is this branch; the stop is
confirmed by the code and by the $0.523$ loss (mpmath), not by running.
**Fix:** treat a loss with the best public neighbour as the contract's steep
maximizer: return Ok with `logLik` from the summed `dtriang` at the returned
coefficients (already `out.logLik = actual`), and keep `NumericalFailure`
only for non-finite results. **Expected:** E at $a = c$, $b = c + 2\,\mathrm{ulp}$,
$c$, $\ell = 111.60$.

### 3.7 expweibull: not an equivariance failure

`tie3` ends in the same `F:no-convergence` (6) as the near-constant sample
(snapshot rows, both modes). The strict-rules report's hypothesis ("expweibull's
`tie3` ends in the same failure as its near-constant sample") is confirmed,
and so its listing under `near-constant-equivariant-numerical-failure` rests
on the preimage failing, not on a defect that equivariance exposes: the fitter
already uses relative logs (`log1p((v - anchor)/anchor)`, mle.hpp:232-239) and
standardizes by `unit`, so the standardized sample of both inputs is the same.

`expweibull_probe.py` (profile the exponent, grid over $(\log a, \log b)$ on
$[2.699, 2.699, 3.487]$) gives: no interior maximum on the explored box; the
likelihood keeps rising as $a \to 0$, $c \to \infty$: $\ell = 0.42$ at
$a = 1.9 \times 10^{-4}$, $b = 0.375$, $c = 1.35 \times 10^{16}$, above the
inverse Weibull face ($-0.705$ at scale $2.787$, shape $12.43$) that the
code's final comparison uses (mle.hpp:361-372). The supremum is therefore
either at a face the code does not know, or unbounded; the numbers at
$c \approx 10^{16}$ are at the limit of binary64 so I cannot decide which.
This needs its own analysis (a three-parameter family on two distinct
points) and is separate from the 12 other families. The dossier's `min_n: 3`
declares the same open question for two-point samples.

## 4. Groups for implementation

Each group is a batch; within a group the fix is the same code change.

| batch | cause | families | change | expected afterwards |
|---|---|---|---|---|
| A | absolute logs in the Burr certificate (and `1/v` in invburr) | burr, pareto4, invburr | relative-log `data()` and root neighbourhood in `burr/certificate.hpp`; invburr without reciprocals | L pareto, L pareto, L invweibull |
| B | absolute logs in `LogData` / GB2 concave gate / `lost_log_distinctions` | trgamma, fpareto | relative-log `LogData`; drop the log-merge gate for relative inputs | L power; fpareto E (steep) |
| C | gengamma's own absolute-log error model | gengamma | relative logs through `mle.hpp`, `face_certificate.hpp`, `local.hpp`, `held*.hpp` | L pareto |
| D | interval allowance that counts the centre | lst, std | `eps*(1+\|value\|)` in `t_cert_detail::data`; collapse test before `data()` | N:unbounded; L norm |
| E | certificate on raw coordinates | emg | centre and scale at the entry of `emg_accuracy_search` (sn pattern) | L translated exponential |
| F | acceptance gate in public coordinates on a steep maximizer | sn, huber, triang | evaluate faces before rejecting; return the rounded point per the contract | L hnorm; E; E |
| (none) | preimage fails too | expweibull | separate analysis of the `tie3` problem | unchanged until then |

Batches A and B overlap in code only through the tools they share
(`core/transformed_data.hpp` already has the relative-log type and the
anchor pair). Doing A first gives the template for B and C. D and E are
independent of all others. F depends on the contract's reading below.

## 5. Open points for review

1. **E or F for steep maximizers.** The contract (2026-11-14) says E at the
   rounded point with `logLik` at the returned coefficients; `triang` (line
   337) and `huber`/`sn` (their checks) still implement a rejection. Batch F
   changes that and moves the three families (and fpareto, once batch B is
   done) from `F` to `E`/`L`. If instead the rejection is intended, the
   rule `near-constant-tie3` allows `F:conditioning`, and the code must say so
   through `AccuracyStop::Conditioning`; today it says `evaluation`.
2. **Certified flag.** `norm` returns a certified E at a $0.75$ loss;
   `invweibull` an uncertified one. The expected `certified` for the L
   outcomes of invburr and std, and for the new E rows, is not decided here.
3. **Not run.** The exact stop inside `gengamma_mle` (3.3), `sn` line 447
   (3.6), `huber` line 603 (emulated exactly, but with the standardized
   optimum taken from the `tie3` snapshot), the `std` stage after
   `data()` (3.4) and fpareto's transport check (3.2) were inferred from the
   code. Each is a one-line check once a build is allowed
   (`fit-snapshot.py --family <id> --classes near-constant`).

## 6. Reproduction

All with `/tmp/umvl-cpu/run nice -n 10`, `OPENBLAS_NUM_THREADS=1`,
from the repository root, `python3 -I <script>`:

| script (in `2026-11-15-equivariance-data/`) | output | content |
|---|---|---|
| `dump.py` | `snapshots.txt` | both outcomes, data, coefficients per mode |
| `mapped.py` | `mapped.json`, `mapped.txt` | map, mapped answers, rounding and loss (mpmath, 80 digits) |
| `causes_logs.py` | `causes_logs.json`, `.out` | logs, Burr box widths and $P$, gengamma and GB2 gates |
| `causes_t.py` | `causes_t.json`, `.out` | lst/std `data()` allowance and variance enclosure |
| `causes_loc.py` | `causes_loc.json`, `.out` | emg $z$ and allowance; sn lattice losses |
| `causes_huber.py` | `causes_huber.json`, `.out` | huber nuisance bound at public coefficients |
| `expweibull_probe.py` | `expweibull_probe.out` | likelihood scan of the `tie3` problem |

**Correction (2026-11-15).** The expweibull "likelihood keeps rising to 0.42" above is binary64
cancellation in the probe (the sum of $\log(1-e^{-t})$ is one ulp at its best point); at 50 and
200 digits the profile there is $-0.7906$, and the tie3 supremum is the inverse-Weibull face
($-0.705046$), not attained. See the
[expweibull limits report](2026-11-15-expweibull-limits-report.md).
