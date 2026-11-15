# Causes of the 54 TASK-214.5 strict failures

2026-11-15. Code reading and emulation only: nothing compiled, no C++ edited. Scripts, outputs and the extracted
entry data are in [the data folder](2026-11-15-refine-causes-data/). "Emulated" means a Python (double) or mpmath (40 to 80 digits)
re-implementation of the fitter's stopping logic that reproduces the returned point; "bit-exact" marks cases where it matches the
snapshot value digit for digit. Maximizers are the 50-digit values in `dev/validation/strict-refine.jsonl.gz` (the cache is in the
worktree under `dev/validation/`, not `build/`). Relative error is $(\hat\theta-\theta^*)/\theta^*$; the contract bound is $10^{-10}$.

## Summary

| class | entries | families |
|---|---|---|
| (a) loose default tolerance; 5c's `tol = 1e-10` fixes it | 36 | dnorm 12, gamma 4, logis 2, gompertz 2, nbinom 4, bhatt 2, cauchy case 010216 (2), burr scale-held cases 688, 689, 9424, 9884 (8) |
| (b) stopping rule that does not certify relative accuracy at tight `tol` | 0 as the primary cause (rule defects found inside (a) and (c) entries are listed per family) | |
| (c) algorithmic defect | 18 | cauchy 000764 (2), burr shape1-held 638, 639 (4), pareto3 (4), nhyper (4), tbinom route (2), emg (2, not confirmed) |
| (d) check artifact | 0 | |

5c alone fixes the 36 class (a) entries, verified by emulation at $10^{-10}$ in each family except bhatt (the certificate's
own Newton-step bound is read directly, see below) and nbinom (see the side note on the certificate bracket). It does not fix the other 18.

## Table (one row per case; each row counts the double-only and native entries unless noted)

| family | case | entries | check | returned vs maximizer (relative error per coefficient) | class | fix |
|---|---|---|---|---|---|---|
| dnorm | 1100 (mean free) | 2 | refine | mean 0, sd $6.7\times10^{-10}$ | a | 5c; also apply the last Newton step before stopping |
| dnorm | 1116, 1124 (mean held) | 4 | refine | sd $7.5\times10^{-10}$, $7.0\times10^{-10}$ | a | same |
| dnorm | 1117, 1118, 1119 (mean held, same data) | 6 | refine | sd $7.0\times10^{-10}$ | a | same |
| gamma | 1977 | 2 | refine | shape $-1.15\times10^{-9}$, scale $+1.15\times10^{-9}$ | a | 5c with a relative step |
| gamma | 1991 | 2 | refine | shape $-2.72\times10^{-9}$, scale $+2.72\times10^{-9}$ | a | same |
| logis | 4689 | 2 | refine | location $-2.2\times10^{-17}$, scale $-1.02\times10^{-9}$ | a | 5c; scale the location step by the scale |
| gompertz | 2793 | 2 | refine | a $+2.6\times10^{-8}$, b $-9.6\times10^{-9}$ | a | 5c |
| nbinom | 9668, 10128 (same data) | 4 | refine | size $-1.29\times10^{-9}$, prob $-1.1\times10^{-10}$ | a | 5c |
| bhatt | 363 (sigma held) | 2 | refine | a $-6.05\times10^{-9}$, mu $4\times10^{-17}$ | a | 5c |
| cauchy | 10216 | 2 | refine | location $-1.5\times10^{-15}$, scale $-3.0\times10^{-10}$ | a | 5c |
| cauchy | 764 | 2 | refine | location $9.2\times10^{-10}$, scale $-1.13\times10^{-9}$ | c | noise-tolerant line search; stop on the undamped step |
| burr | 688, 689, 9424, 9884 (scale held) | 8 | refine | shape2 $3.2\times10^{-9}$, $2.6\times10^{-10}$, $3.0\times10^{-9}$, $4.4\times10^{-9}$; shape1 $-2.1\times10^{-9}$, $-1.1\times10^{-10}$, $3.2\times10^{-10}$, $5.1\times10^{-10}$ | a | 5c (the legacy Newton proposal overrides the accurate root) |
| burr | 638 (shape1 held) | 2 | refine | shape2 $7.2\times10^{-10}$ | c | centred logs |
| burr | 639 (shape1 held) | 2 | refine | shape2 $4.9\times10^{-6}$ | c | centred logs |
| pareto3 | 5590, 5593 (double only) | 2 | refine | shape $-1.0\times10^{-9}$, $-9.4\times10^{-10}$ | c | centred logs |
| pareto3 | 5591 | 2 | refine | shape $-3.2\times10^{-5}$ (double), $-3.7\times10^{-9}$ (native) | c | centred logs |
| nhyper | 5161 (all free) | 2 | nesting | $(N,m,r)=(1000011,1,1)$ is not a maximum | c | exhaustive-in-r search (below) |
| nhyper | 5175 ($m=20$ held) | 2 | nesting | $(N,m,r)=(6000029,20,1)$; maximum is $(1000012,20,20)$ | c | same |
| binom +truncate=tbinom | 7325 | 2 | nesting | $(N,p)=(1000011,0.99999335)$; profile rises to the Poisson face | c | see below |
| emg | 1311 | 2 | seam-open, terminates | no result in 300 s and 600 s | c (not confirmed) | see below |

The two cases that appear in the nesting key list as the bigger-hold partners (5217, 5230, 7353) are fits that are themselves
uncertified; they are not failures here.

## dnorm (12 entries, class a)

Stop: `dnorm/mle.hpp:124` stops when the Newton decrement $-g^{\top}s \le tol^2 n$ **before** applying the step it just computed. Default `tol` is
$10^{-9}$ (the adapter's `reltol`). The decrement at the stopping iterate is $\approx 7\times10^{-19}n$ in all six cases, so the error is
$\approx tol$ times a case constant (here 0.67 to 0.75): relative error of sd $6.7$ to $7.5\times10^{-10}$, exactly as returned.
Emulation (`emu_dnorm.py`, 40 digits) reproduces every returned sd and shows that one more Newton iteration (what `tol = 1e-10`
buys) gives $5\times10^{-19}$ to $2\times10^{-18}$. Quadratic convergence is what makes this safe; the rule itself is only
accurate to `tol` times the constant. If `tol = 1e-10` is used with this rule unchanged, a worst case near the threshold would
still land at about $0.7\times10^{-10}$. Recommendation: apply the computed step, then stop (error $\sim tol^2$). Mean (free in case 1100) is exact because the symmetric data
put the mean on the cell boundary.

## gamma (4 entries, class a)

`gamma/mle.hpp:187` calls `newton_raphson_1d_abs` (`core/newton.hpp:51`): stops when $|{\rm step}|<$ `abstol` $=1.22\times10^{-4}$, absolute,
after applying the step. The Minka start was already within $6\times10^{-5}$ (1977) or $10^{-4}$ (1991) of the root, so a single
iteration ran and the error after it is $\propto{\rm step}^2$: $-1.15\times10^{-9}$ and $-2.72\times10^{-9}$. Emulation
(`emu_gamma.py`) is bit-exact. With `tol=1e-10` three iterations run and the error is $5\times10^{-16}$ (1977) and
$9\times10^{-14}$ (1991). The latter is the conditioning of $s=\log\bar x-\overline{\log x}$ for data $\sim10^{-150}$ (rounding of $s$ is $10^{-13}$,
shape sensitivity 3.3), within the contract. The rule must be relative, $|{\rm step}|<tol\cdot|k|$, since shape can be large.

## logis (2 entries, class a)

`logis/mle.hpp:181` stops on $\max(|d\mu|,|d\theta|)<$ `abstol` after the step; $\theta$ is a log scale (relative) but $\mu$ is absolute.
Data are tightly clustered near 0.266 (sd $4.2\times10^{-7}$). Two iterations ran: the $\theta$ step was $4.1\times10^{-5}$, error after $1.0\times10^{-9}$.
Emulation is bit-exact; at $10^{-10}$ four iterations give scale error $2.4\times10^{-16}$. The location step criterion is meaningless for $\mu$ here
($10^{-10}$ is $4\times10^{-4}$ of the scale); it was harmless because the location converges first. Recommend $|d\mu|/\hat s$.

## cauchy (4 entries)

`cauchy/mle.hpp:201`: stop on $\max(|d\mu|,|d\theta|)<$ `abstol`, after the (damped) step. Emulation bit-exact for both cases.

* **010216 (a)**: location is $\approx1$ with scale $6\times10^{-7}$; the absolute $\mu$ step is below `abstol` from iteration 1, the $\theta$ step gates. Three iterations, last $\theta$ step $6.6\times10^{-5}$, scale error $3.0\times10^{-10}$. At $10^{-10}$ five iterations give $2.6\times10^{-16}$.
* **000764 (c)**: `tol=1e-10` does **not** fix it (emulated error $9.9\times10^{-10}$ in scale, $1.2\times10^{-9}$ of scale in location). From iteration 5 the full Newton step has cost change $+1.8\times10^{-15}$, which is double-precision noise (cost $\sim$ O(1)), so the monotone test `cost(candidate) <= old` (`:190`) rejects the full step and halves it (fractions 0.125, then $4.9\times10^{-4}$). The *damped* step is then compared with `tol` and falsely signals convergence while the Newton step is still $\sim5\times10^{-10}$ (`emu_cauchy2.py`). The same pattern exists in logis (`logis/mle.hpp:169`); it did not trigger there.
  Fix sketch: accept `cost(cand) <= old + 64*eps*max(|old|, n)` and/or skip the line search when the undamped step is small, and test convergence on the undamped Newton step:
  ```cpp
  const double full = std::max(std::fabs(dmu)/std::exp(theta), std::fabs(dth)); // before dmu*=step
  ...
  if (accepted && step == 1.0 && full < tol) break;   // converged only on an undamped step
  ```
  With slack $64\varepsilon$ the emulated errors are $6\times10^{-17}$ (scale) and $6\times10^{-17}$ (location/scale).

## nbinom (4 entries, class a)

`nbinom/mle.hpp:204`: stop when $|{\rm step}|<|{\rm size}|\,tol+tol$ after the step; the binding default `reltol` is $1.22\times10^{-4}$.
Five iterations, last step $-4.4\times10^{-4}$, error $-1.29\times10^{-9}$. Native emulation is bit-exact (the double-only value
differs in the 11th digit through digamma rounding). At $tol=10^{-10}$: seven iterations, error $-1.4\times10^{-12}$ (the noise floor of the digamma score, data nearly equidispersed: variance minus mean $0.17$).
Side note, hypothesis from reading: `nbinom/certificate.hpp:140` returns the candidate only if it lies in the certified bracket, otherwise the bracket midpoint. The bracket closes at relative width $5\times10^{-7}$ (`:127`), so a candidate outside it would return an answer with error up to $2.5\times10^{-7}$ and status Ok. With the loose default tolerance the chance is about $10^{-3}$ per fit and falls with `tol`. Not observed here.

## gompertz (2 entries, class a)

`gompertz/mle.hpp:147`: relative step test after the step, `reltol` $1.22\times10^{-4}$, Newton on $b$ in data standardized by the mean. Three iterations, last step $-4.0\times10^{-4}$; error a $2.6\times10^{-8}$, b $-9.6\times10^{-9}$ (bit-exact). At $10^{-10}$ five iterations: $2.7\times10^{-15}$, $-9.6\times10^{-16}$.

## bhatt (2 entries, class a)

Case 363 is data $10^8+N(0,1)$, sigma held 1.0579, so mu and a are free and the path is the certificate (`bhatt/certificate.hpp:137`, `certificate_math.hpp:282-360`). The root is accepted when the interval-validated Newton–Kantorovich enclosure has half-width
$\le tol\cdot\max(\text{floor}, a)$, with $tol=$ `reltol` $=10^{-8}$. At the returned point the exact Newton step is $(\Delta\mu,\Delta a)=(-3.964\times10^{-9},\,1.265\times10^{-8})$, equal to the true error to all printed digits
(`newton_step_at_returned.py`); relative $a$ error $6.05\times10^{-9}<10^{-8}$. So the stop is the loose tolerance. Locations at $10^8$ have ulp $1.5\times10^{-8}$ but the certificate works in offset coordinates, so $10^{-10}$ is reachable; **not run** (needs the C++), so this entry is read, not emulated. Condition number of the $(\mu,a)$ Hessian is 2.1.

## burr (12 entries)

Two different code paths, dispatched in `bindings/common/distributions/burr/fit.hpp:19-23`.

**Scale held (688, 689, 9424, 9884; 8 entries; a).** Path `burr_mle_fixed_scale` (`burr/mle.hpp:301`) with `abstol` $=1.22\times10^{-4}$. A legacy Newton on $\log c$ (`fixed_scale.hpp:345-385`, stop on $|{\rm step}|<$ `tol`) provides a proposal; `scale_certificate.hpp:108` then replaces the accurately bisected root ($1\times10^{-13}$ relative) by `proposal*unit` whenever they agree within $10^{-8}$, and the certificate accepts coefficient enclosures up to $10^{-6}$ wide (`:126`). So the returned value is the legacy proposal: emulated bit-exact for all four cases (`emu_burr_fixed_scale.py`): shape2 errors $3.2\times10^{-9}$, $2.6\times10^{-10}$, $3.0\times10^{-9}$, $4.4\times10^{-9}$ (last legacy step $9.6\times10^{-5}$, $2.4\times10^{-5}$, $6.9\times10^{-5}$, $8.5\times10^{-5}$). The bisected root alone is accurate to $\le2\times10^{-14}$ (`emu_burr_scalefit.py`). With the legacy tolerance $10^{-10}$ the proposal errors are $\le3\times10^{-16}$. So 5c fixes the entries, provided the new `tol` reaches `burr_mle_fixed_scale`. Recommendation in addition: drop the override or limit it to $|{\rm proposal}\cdot{\rm unit}-t|<tol\cdot t$.

**Shape1 held (638, 639; 4 entries; c).** Path `burr_mle_holds` (`burr/mle.hpp:359`), `tol` $=$ `gradtol` irrelevant: it bisects the scores to adjacent doubles. The data are 5 points with log spread $5\times10^{-8}$ (638) and $5\times10^{-12}$ (639) around $\log x=0.714$; the code forms $l=\log x$ and the mean in double (`:386`, absolute rounding $1.6\times10^{-16}$), which makes shape2 relatively wrong by about ulp/spread: $7.2\times10^{-10}$ and $4.9\times10^{-6}$. Emulation (`emu_burr_holds.py`) is bit-exact on the returned values; replacing $l_i$ by centred $d_i=\log1p((x_i-x_0)/x_0)$ with $x_0$ the median, $w_i=d_i-\bar d$, gives $1.6\times10^{-15}$ and $-1.8\times10^{-15}$. Not tolerance related, so 5c does not help.
Fix sketch:
```cpp
const double x0 = median(x);
for (double v : x) d.push_back(std::log1p((v - x0)/x0));   // exact for close data (v - x0 is exact)
double mean_d = mean(d);  // w_i = d_i - mean_d replaces v - mean in location()/score lambdas
// scale: s = exp(log(x0) + mean_d + location(b)/b)
```

## pareto3 (4 entries, class c)

Shifted llogis with `min = 0` held; data in 5590, 5593 have log spread $5\times10^{-8}$ and in 5591 $5\times10^{-12}$. `pareto3/mle.hpp:74` forms `log((v-min)/unit)`, the ratio rounded to $1.1\times10^{-16}$ in double. Solving the 80-digit likelihood with these rounded logs as inputs reproduces the returned shape errors to 7 digits (`pareto3_rounding.py`): $-1.0053\times10^{-9}$ (returned $-1.0053\times10^{-9}$), $-3.1593\times10^{-5}$ (same), $-9.370\times10^{-10}$ (same). Exact logs give $10^{-26}$. The native build passes 5590/5593 (extended logs) and fails 5591 at $-3.7\times10^{-9}$, as the ratio of precisions predicts. Fix: as for burr, use centred `log1p((v - x_ref)/(x_ref - min))`, and put the constant offset $\log((x_{\rm ref}-min)/{\rm unit})$ into `q[1]`.

## nhyper (4 entries, class c)

Exact check (Python `Fraction`, `exact_nhyper.py`), model $P(T=x)=\frac rx\binom mr\binom{N-m}{x-r}/\binom Nx$ (matches the snapshot log likelihoods to $10^{-13}$). Data $x=(1000000,1000001,1000003,1000008,1000011)$.

* **5161 (all free).** Returned $(N,m,r)=(1000011,1,1)$, $\ell=-69.0776$. At the same $N$ the point $(m,r)=(3,3)$ has exact likelihood ratio $\prod_x 3(x-1)(x-2)/((N-1)(N-2))>1$, $\log=5.493$ (equals the 5217 gap). Why: `nhyper/mle.hpp:71-83`. For $\max x>100000$ the free fit takes the budget branch, which evaluates only $r=r_{lo}=1$ and $N\in[\max x,\max x+255]$ (`:75-76`), flags `discrete_budget`, status NumericalFailure, which `nhyper_mle` (`:307-321`) promotes to an uncertified Ok with the best candidate. Flagged, but a partial search.
* **5175 ($m=20$ held), 5230 ($m=20,r=3$ held).** Returned $N=6000029$ and $6000027$ with $r=1$ and $r=3$. These are exactly $lo+4999999$, with $lo=m+\max x-r$: the grid (`:197-217`) enumerates $r$ outer, $N$ inner, and stops after `kGridBudget/2` $=5\times10^6$ cells (`:215`, `kGridBudget`$=10^7$ in `hyper/mle.hpp:287`) inside the first $r$ line, returning the last cell seen, i.e. the boundary of the scanned window. Exact ratios: $L(N+1)/L(N)>1$ at both returned points (log ratios $2.3\times10^{-6}$ and $3.3\times10^{-7}$), and for 5175 $L(r{=}2)/L(r{=}1)>1$ at the returned $(N,m)$ (log $6.68$). The exact profile maxima by $r$ (bisection on the exact ratio, a single sign change confirmed on a log grid for each $r$) are $N^*(r)\approx m\max x/r$ and $\ell^*(r)$ increasing in $r$ from $-73.95$ ($r=1$, $N=20000092$) to $-54.10$ at $r=20$, $N=1000012$. The returned $-80.38$ is $26.3$ below the true maximum; 5230's line maximum is at $N=6666697$.
  Fix sketch: a held $m$ makes $r\le m$ small, so enumerate $r$ completely and per $r$ find the $N$ maximizer by exact integer ratio bisection (the profile in $N$ had a single sign change in all 20 lines; unimodality is a hypothesis, not proved here):
  ```cpp
  for (double rr = rlo; rr <= rhi; ++rr) {
    double lo = fixed_N ? rr : m + (c.max - rr), hi = population_bound(m, c.max, rr);   // already computed
    while (hi - lo > 1) { mid = floor((lo+hi)/2); (ratio_N(mid, m, rr) > 1 ? lo : hi) = mid; }  // product_difference-style exact ratio
    evaluate(lo or hi);   // O(log) per r instead of O(hi-lo)
  }
  ```
  and keep the uncertified grid only as a fallback that reports the scanned window and does not return a boundary cell.

## tbinom route (2 entries, class c)

Cases 7325 (free) and 7353 (`a` held; a wider window, so by monotonicity of the normalizer its maximum is $\le$ the free one). Exact rational evaluation (`exact_tbinom.py`, ratios of binomials with the double $p$ as an exact fraction): the free fit's returned $(N,p)=(1000011,0.99999335)$ has $\ell=-17.426030372$; the held fit's point $(1000035,0.99997108)$, evaluated in the free window, has $\ell=-13.240047870$, so the returned fit is not a maximum. The 40-digit profile over $N$ rises monotonically through the sampled values $-17.43,-16.18,-13.63,-12.86,-12.43,-12.38,-12.2534467,\dots$ to the truncated-Poisson face $-12.2534307$ at $N=10^{12}$. Expected outcome: escape to `tpois`.
Why it returned $N=x_{\max}$: **only partly confirmed**. `tbinom_mle.hpp:571-583` (`design_mle_impl`) turns a failed or uncertified size search that holds a finite witness into an uncertified Ok (`candidate_ready && ... best.certified=false`), as nhyper does. The profile evaluations at $N=x_{\max}+j$, $j\le128$, all succeed in an emulation of `profile()` (`emu_tbinom_profile.py`), so the early stop is not a profile failure at those sizes; the later certificate stage (`tbinom_face_certificate.hpp`, `cover(...)`, not read in full) is the probable place where the stop happens, with the incumbent never updated from $x_{\max}$. I could not confirm that without running it.

## emg (2 entries, class c, not confirmed)

Case 1311: 12 observations $\approx10^8$, sigma held, mu and lambda free, double-only. Path: `emg_mle` $\to$ `emg_accuracy_search` $\to$ `outer_certificate` (`emg/certificate.hpp:300`), `OuterCertificate::run` (`:185`).
**I found no loop that fails to terminate.** Every loop on the path has a finite bound: the work counter (`certificate_budget(500)` $=64000$ charges, `winner_certificate.hpp:55`) bounds the outer segments; the tail loops (`:210`, `:230`, `:239`) are bounded by `solver_budget(500)`; `cert_newton` (`certificate_math.hpp:348`) by 500 iterations with a 500-step backtrack and, in `cert_validate_root`, a 43-step radius loop; `location_certificate` by 500+500 steps; `polish` by 500. The queue loop (`:253`) ends in `NumericalFailure` when a segment cannot be split further (`:258`).
Hypothesis: the wait is the product of these bounds, not an infinite loop. Charges count outer evaluations only (one per `profile`/`propose`/`push`), not the up to $500\times(500+43)$ inner likelihood evaluations each can spend; at $10^8$ offsets in double precision the enclosure `accurate` flag may never become true, so every Newton runs to its limit. The nominal worst case is $64000\times500\times543\approx1.7\times10^{10}$ evaluations of a 12-point likelihood, hours. Native mode returns a stop, consistent with extended-precision enclosures closing. Not verified.
Fix sketch: charge `CertificateWork` inside `cert_newton` and `cert_validate_root` per interval-Hessian evaluation, or give `fixed_shape_certificate` a per-call evaluation cap and return `Budget`; and work in coordinates centred at the sample median (the polish already uses `center`, the certificate does not).

## Files

* Script, output pairs in [the data folder](2026-11-15-refine-causes-data/): `extract.py`/`extract.json` (returned point, held values, maximizer, relative errors for each entry); `emu_dnorm.py`, `emu_gamma.py`, `emu_cauchy.py`, `emu_cauchy2.py`, `emu_logis.py`, `emu_nbinom.py`, `emu_gompertz.py`, `newton_step_at_returned.py` (bhatt), `emu_burr_fixed_scale.py`, `emu_burr_scalefit.py`, `emu_burr_holds.py`, `pareto3_rounding.py`, `exact_nhyper.py`, `exact_tbinom.py`, `emu_tbinom_profile.py`, each with its `.json`.
* The scripts import `dev/validation/strict_common` and run from the repository root: `python3 dev/research/evaluations/2026-11-15-refine-causes-data/<script>.py`.
