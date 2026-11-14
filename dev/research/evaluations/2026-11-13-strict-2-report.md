# Strict layer, part 2: summed density, same phenomenon, refinement, one command (2026-11-13/14)

Job strict-2, consolidation step 6 ([plan](../../design/consolidation.md)).
Worktree `claude/strict-2` from master `bbe1b13a`; known lists from the
stored baselines of master `c167a975`
([snapshot report](2026-11-13-fit-snapshot-report.md)). Workstation only
(GCC 15.2.0); simbox was not used. Evidence:
[2026-11-13-strict-2-data/](2026-11-13-strict-2-data/). Guide:
[validation README](../../validation/README.md#strict-layer-checks-with-a-density-or-a-refinement).

The strict layer now has four more checks, a shared ratchet and one command,
`just strict`. On the stored baselines and on a fresh snapshot of this
worktree (identical to the baselines: 0 of 11,820 family cases and 0 of
1,934 route cases differ in each mode) it reports 343 failures, all listed,
and exits 0. The new checks found three kinds of defect the earlier layers
could not see:

- 20 `Ok` fits per mode whose `logLik` is not the likelihood at the returned
  coefficients, by up to 4% (gamma, weibull, logis, gumbel and cauchy on
  near-constant and offset data, and six families on subnormal data);
- 22 native and 24 double-only `Ok` fits outside the accuracy contract
  against a 50-digit refinement (burr, dnorm, gamma, gompertz, cauchy,
  nbinom, pareto3, logis, bhatt), by 2.6 to 316,000 times the bound;
- 41 limit stops per mode whose stated limit coefficients and supremum do not
  agree with the limit model's own density.

## 1. Log likelihood equals the summed density (`fit-density.py`)

**What is compared.** Each estimate's `logLik` (`kind` ok), each limit stop's
`limit_loglik` at its `limit_estimates` when the limit model has a C++ density
(a family or a modifier law), and each tie-stop candidate's stated
`[loglik_lower, loglik_upper]`. The comparison value is the exact sum
(`math.fsum`) of `d<id>(x_i, ..., log = TRUE)` from the family's `dpqr.hpp`,
called exactly as the family's `test.cpp` calls it in `check_dpqr_truth`, at
the returned coefficients, compiled with the snapshot's flags and precision
mode. Inputs are rebuilt with `fit-snapshot.py`'s own `cases()` (imported),
and each record's input hash is verified.

**Modifier routes are included.** All 16 route laws have a C++ density:
`dtbinom`, `dtnorm`, `dtpois`, the twelve zero-truncated, zero-modified and
zero-inflated laws, and, for the zeta window, `zeta_window_eval('d', ...)`.
`dzipf` would be wrong there, because it is the $s \ge 0$ finite Zipf law,
while the window admits every real exponent and the fitter returns negative
ones (zeta dossier, R notes). Window bounds the caller did not hold are
rebuilt as the route does: count windows from the minimum and maximum,
`tnorm` from the data range, `N` from the maximum.

**Tolerance.** With $S$ the summed density, $R$ the reported value and $A =
\sum_i |\log f(x_i)|$, a value passes when

$$|R - S| \le 10^{-10} \max(1, |R|) + n\,\epsilon\,A .$$

The first term is the accuracy contract's `logLik` tolerance
([accuracy contract](../../design/accuracy-contract.md#log-likelihood)). The
second bounds the rounding of a recursive $n$-term sum, so the check never
blames summation. On the baselines the rounding term is at most `6.4e-13`
relative (median `1.1e-15`, 99th percentile `1.3e-14`). The distribution shows
a clean gap: every passing value is at most `8.9e-11` (invburr limit, archive
10479), and every failure is at least `1.29e-10`. All attributable failures
are on the fitter side (below).

Relative differences by decade, all families and routes (native; double-only
differs only in the smallest decades; per-family counts in
[density-histogram.json](2026-11-13-strict-2-data/density-histogram.json),
per-family median, 99th percentile and maximum in
[density.txt](2026-11-13-strict-2-data/density.txt)):

| exact | 1e-18 to 1e-14 | 1e-14 to 1e-12 | 1e-12 to 1e-10 | 1e-10 to 1e-8 | 1e-8 to 1e-4 | 1e-4 and above | non-finite | tie candidates in bounds |
|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 6,469 | 4,794 | 119 | 43 | 11 | 12 | 20 | 18 | 140 |

The last four value columns are exactly the 61 failures.

**Attribution.** Each failure of a dossier family is recomputed independently
with mpmath at the exact binary64 inputs (`seam_reference.py`, at 100 to
1,600 digits, repeated 50 digits higher), as `seam-check.py` does. In every
attributable case the C++ density agrees with the independent value to
`2.2e-15` or better, and the reported value is the one that is off. The two
route failures have no dossier, so no independent value.

**Results: 61 failures per mode, the same cases in both modes.**

Estimates (`density`, 20 per mode, TASK-214.3, finding `ok-loglik-off-density`):

| family | class | reported vs summed (relative) | note |
|---|---|---:|---|
| gamma | near-constant, offset | 3.6e-2, 4.1e-2 | reported 108.21, true 104.32 at the returned coefficients |
| weibull | near-constant, offset | 3.5e-2, 2.5e-3 | |
| logis | near-constant, offset, subnormal, subnormal-signed | 3.6e-3 to 8.4e-5 | |
| gumbel | near-constant, offset | 1.9e-3, 1.6e-4 | |
| cauchy | offset, subnormal, subnormal-signed | 1.0e-4 to 3.6e-5 | |
| hnorm, laplace | subnormal | 4.5e-4 | off by exactly one unit of log likelihood |
| rayleigh | subnormal | 5.6e-4 | |
| hcauchy | subnormal | 4.2e-6 | |
| lgser | huge | 2.5e-9 | |
| pois+zero=modified, pois+zero=truncated | huge | 9.3e-9 | reported `-58.3135986328125`, a value with few significant bits |

On near-constant, offset and subnormal data the maximizer is not
representable to the contract's accuracy. The contract then asks for a
conditioning stop or a dossier allowance (a Lipschitz failure); these fits
instead return `Ok` with a `logLik` that no returned coefficient vector
attains.

Limit stops (`density:limit`, 41 per mode, NEW):

- `limit-coefficients-exclude-observation` (16): the stated limit coefficients
  put an observation outside the limit model's support by one binary64 spacing:
  bhatt `unif` (archive 334, 337, 341, 345, 346, 347: `mu + a` rounds below
  the maximum), sged `unif` (5966, 5968, 5969, 5978, 6121 and three
  resamples), st `hnorm` (resamples n3-s1, n7-s1: `mu` rounds above the
  minimum). The contract says returned coefficients never place an observation
  outside the support.
- `limit-coefficient-infinite` (2): lomax and pareto2 on subnormal data state
  the `exp` limit with `rate = Inf`, while `limit_loglik` is finite (2228.24).
- `limit-loglik-off-density` (23): `limit_loglik` exceeds the limit model's
  summed density at the stated limit coefficients: burr 616, 619 (`1.5e-10`)
  and 617, 620 (`2.0e-7`); ged 2362, 2365 (`1.3e-10` to `1.6e-10`), 2363
  (`1.1e-6`) and class offset (`7.9e-2`); trgamma 8012, 8015 (`1.3e-10`) and
  8013, 8016 (`6.4e-7`); invtrgamma 4067 (`4.1e-7`) and near-constant
  (`7.5e-4`); pareto4 5650, 5653 (`5.6e-10`); and the subnormal classes of ht,
  lst, std, snorm and st (`8e-6` to `1.5e-4`). Either the stated supremum or
  the stated limit coefficients are wrong; this check cannot tell which.

All 140 tie candidates lie within their stated bounds. Not checked (listed by
the script): 317 native and 316 double-only limit stops and candidates whose
model has no C++ density (`unavailable:*`: point masses, two-point laws,
half-GED, reflected half-normal and half-t, translated exponential,
unit-gamma), and one tnorm candidate stated in escape coordinates (`mu`,
`rate`).

## 2. Same phenomenon, same outcome (`fit-consistency.py`)

The outcome (kind and reason) of the classes empty, NaN, Inf, -Inf, negative,
zeros, one, two, constant, ties and near-constant is tabulated for the 85
families and 16 routes; the full table is in the appendix. Native and
double-only agree on every one of these cells.

**Rules implemented** (each holds for every family by the outcome definitions
in `core/outcome.hpp` or by elementary mathematics):

| rule | statement | failures per mode |
|---|---|---:|
| `empty` | an empty sample ends in `failure(invalid-input)` | 5: binom, gompertz, lgamma (no-convergence), cauchy (nonexistence), the tbinom window (invalid-argument) |
| `non-finite` | a NaN or infinite observation ends in `failure(invalid-input)` | 15: lgamma (3), gompertz (no-convergence); fpareto, nsbeta (3), pareto2, pareto3, pareto4 (invalid-argument) |
| `one-constant` | `one` and `constant` end in the same kind and reason: four equal observations have the fourth power of one observation's likelihood | 10: burr, frechet, gev, invburr, invtrgamma, pareto4, sn, sstd, tlambda, trgamma (one: invalid-input; constant: nonexistence) |
| `valid-data` | observations with a finite C++ log likelihood at the golden coefficients never end in `invalid-input` or `invalid-argument` | 22: one/two/ties/near-constant/constant refused as invalid input by burr, fpareto, frechet, gb2, gev, invburr, invtrgamma, kumar, pareto4, sn, sstd, tlambda, trgamma; pareto2 zeros (on its held lower bound) as invalid-argument |
| `outside-support` | observations beyond a support bound fixed by the held values end in `invalid-input` | 24: negative counts in 15 routes and negative data in fpareto, nsbeta, pareto2, pareto3, pareto4 (invalid-argument); gompertz, lgamma (no-convergence, also lgamma zeros); nhyper zeros below `r` (nonexistence, unattained) |
| `collapse` | constant valid data with free location and scale end in `nonexistence(unbounded)` | 0 |
| `constant-scale` | constant valid data with a free scale or rate never end in a numerical failure | 1: pareto3 (evaluation; with `min` held the likelihood is unbounded as the shape tends to zero) |

The documented sample-size requirements are implementation limits, not
mathematics: frechet's joint fitter "requires at least three observations",
burr's all-free certificate two, tlambda's attained maximum three. A valid
sample refused for that reason is not `invalid-input` by the reason's
definition, so the rule stands (question 2 below).

**Open rule questions** (not implemented):

1. *Near-constant data.* Two values one binary64 spacing apart end in an
   estimate in 52 cells, a limit in 20, `nonexistence(unbounded)` in 5, and a
   numerical failure in 22 (evaluation: betapr, burr, emg, fpareto, invburr,
   lst, pareto4, std, trgamma, triang; no-convergence: expweibull, fatigue,
   genbeta, gengamma, gompertz, huber, invparalogis, kumar, paralogis, sgomp,
   sn, sstd), plus gb2 (unsupported) and tlambda (invalid-input). The accuracy
   contract calls an unreachable tolerance `conditioning`. Should the rule be
   "never no-convergence or evaluation", given that density check 1 shows
   gamma, weibull, logis and gumbel returning wrong `Ok` on the same class?
2. *Sample-size restrictions.* Should a fitter that does not implement small
   samples report `unsupported` (today: "a hold subset the family does not
   implement"), or the mathematical outcome (nonexistence for one observation
   with a free scale)?
3. *Two observations with three or more free coefficients* end in an estimate,
   limit, invalid-input (frechet, gev, tlambda), no-convergence (expweibull,
   gengamma, st) or evaluation (sstd). Is "never a numerical failure" certain
   here? It is not obvious for every family that the outcome is decidable in
   closed form.
4. *An identically zero likelihood inside a closed support* (rayleigh at zero,
   nhyper below `r`) maps to `nonexistence(unattained)`. The snapshot report
   already notes that no reason exists for it; nhyper zeros are below a bound
   fixed by the held `r`, so the `outside-support` rule flags them as
   `invalid-input`. Is that the intended reason?
5. *Ties with half the mass on one value* end in `nonexistence(unbounded)` for
   cauchy, lst, slash and st, `nonexistence(unattained)` for std, and
   no-convergence, unresolved, unsupported or invalid-input for sstd, emg, gb2
   and tlambda. A rule would need the per-family tie theorems.

## 3. Within `tol` of a high-precision refinement (`fit-refine.py`)

**Selection.** All 101 golden samples (85 families, 16 routes) and, per family
and route, a seeded sample (`random.Random('strict-refine:<family>')`) of at
most ten archive cases with every coefficient free and ten with exactly one
held, drawn from the cases that are estimates in the stored `c167a975` native
baseline, so the selection does not depend on the snapshot judged. This gives
1,911 cases, each judged in both modes.

**Method.** `quality/reference.py` unchanged: each case is written as
`golden.json` into a shadow copy of the family folder (README and `spec.py`),
and `reference()` refines the returned estimate at 50 digits with the
dossier's symbolic density. That is damped Newton on the free regular
coefficients, closed forms or exhaustive enumeration for support-bound,
Laplace, discrete-Laplace, binomial-size and hypergeometric coordinates, and a
one-sided score check for a coefficient on a domain bound. Where
`reference()` stops on scale alone (its absolute score threshold of `1e-45`,
unreachable for data near `1e150`, or mpmath's pivot test on a badly scaled
Hessian), a fallback runs the same density, domains and coordinate classes
with Newton in coordinates scaled by each coefficient's size. It works at 50,
100 or 200 digits, accepts a relative Newton step below $10^{-(d-10)}$ with a
negative definite scaled information (the contract's Newton-step error
estimate), holds a free coefficient that sits exactly on an active support
bound (a location with support `[mu, inf)` on the sample minimum, unflagged
in the dossier), and reads a `log(0)` constant in an unused branch as `-Inf`.
Of the 1,911 references, 1,488 come from `reference()`, 219 from the
fallback (see `method` in the cache), and 204 are open.

**Tolerance.** The contract: every free coefficient the refinement determines
must satisfy

$$|\hat\theta_i - \theta^*_i| \le 10^{-10} \max(|\theta^*_i|, s_i),$$

with `tol = 1e-10` (every snapshot fit uses default controls). Here $s_i$ is
the fitted scale for a location (the contract's example), 1 for another
coefficient whose domain reaches zero or below, and 0 for a strictly positive
coefficient, whose floor the contract does not ask for.

**Timings and cache.** The 1,911 references cost 2,784 CPU seconds on one
slot (46 minutes: goldens 493 s, archive 2,291 s). About 6 more minutes went
into three development runs of earlier method versions, whose results were
discarded. That is within the two-hour budget. The heaviest families are cmb
(785 s), cmp (310 s), pig (224 s) and dgamma (173 s), where series
normalizers at 60 digits are slow. A family stops after 150 CPU seconds of
archive refinements or after two cases hit the per-case guard (120 s,
earlier 300 s), and nothing new starts past 7,000 s. Per-family numbers:
[refine-timings.txt](2026-11-13-strict-2-data/refine-timings.txt). The cache,
[strict-refine.jsonl.gz](../../validation/strict-refine.jsonl.gz) (tracked,
keyed by family, input hash, held values, density source and method version),
makes `fit-refine.py --cache-only` (as `just strict` runs it) take seconds.

**Results (native):** 1,685 cases checked, 22 failures, 204 open. Double-only
has 24 failures: pareto3 archive 5590 and 5593 fail only there.

| family | cases (archive) | worst error over the contract bound | coefficient |
|---|---|---:|---|
| burr | 638, 639, 688, 689, 9424, 9884 | 49,200 (639, relative `4.9e-6`, the TASK-214.5 row) | `shape2` |
| pareto3 | 5591 (double-only also 5590, 5593) | 37 native, 316,000 double-only | `shape` (about `1e12`) |
| gompertz | 2793 | 263 (`a` relative `2.6e-8`) | `a` |
| bhatt | 363 | 60 | `a` |
| dnorm | 1100, 1116, 1117, 1118, 1119, 1124 | 7.5 | `sd` |
| gamma | 1977, 1991 | 27 (relative `2.7e-9`) | `shape` |
| nbinom | 9668, 10128 | 13 (relative `1.3e-9`) | `size` |
| cauchy | 764, 10216 | 11 | `scale` |
| logis | 4689 | 10 | `scale` |

Independent confirmations, beyond the refinement: gamma 1977 and 1991 solve
$\log k - \psi(k) = \log\bar x - \overline{\log x}$ at 60 digits (relative
errors `1.15e-9`, `2.72e-9`), nbinom 9668 its profile score in `size`
(`1.29e-9`), and gompertz 2793 its profile likelihood (`a` `2.6e-8`, `b`
`9.6e-9`). All are TASK-214.5, finding `off-maximizer`; burr 639 is the row
the card already names (its sibling 636 was not drawn).

**Open (listed, not failed), native:** 204 cases.

- Symbolic density not evaluable at the returned point: bhatt on its normal
  face `a = 0` (10), triang with the mode held at an endpoint (18), boundary
  probabilities in zero-modified and inflated routes and skellam.
- Nothing left to refine: every free coefficient on a domain or support bound
  (gpd 17, tlambda 12, gev 6, hcauchy 6 and others).
- Reference limits: binomial size with probability held or a tail beyond
  `1e6` cells (`reference_integer.py`), and the per-case guard (cmb 5, cmp 3,
  dgamma, tpois, zipf).
- Non-smooth points: ged at `nu = 1` with the location on an observation,
  tlambda at `lambda = 1`.
- Newton failures that persist at 200 digits: near-constant gamma with shape
  `3e15` to `3e23`, beta, betapr, kumar.

Coefficients not refined (integer, support bound without a closed form, kink,
on a domain bound with its one-sided score checked) are counted in
[refine.txt](2026-11-13-strict-2-data/refine.txt). Families with no archive
case refined: expweibull and gengamma (not in the frozen archive), fpareto and
nsbeta (no estimate with every coefficient free or one held), and sged (1 of
4). Every golden was refined except dgamma's, which hit the 120-second
guard.

## 4. One command: `just strict`

```sh
/tmp/umvl-cpu/run just strict                          # build/snapshots/{native,double}, routes included
/tmp/umvl-cpu/run just strict NATIVE DOUBLE            # comma-separated snapshots, one mode each
/tmp/umvl-cpu/run just strict NATIVE none              # native only
```

It runs `fit-judge.py`, `fit-density.py`, `fit-consistency.py`,
`fit-refine.py --cache-only` and `strict-seam.py` (`seam-check.py` per mode),
and exits nonzero on any failure not in `strict-known.json`. All scripts share
one list and ratchet (`strict_common.py`). Each prunes only its own checks;
`--add REF --match REGEX --finding SLUG` adds a reviewed group. `fit-judge.py`
now uses the shared helpers, with the same checks and the same 21 entries.

`strict-seam.py` keeps `seam-check.py`'s output in `build/strict/seam/`
(resumable; a source change refits everything). A fit without a result
within seam-check's 30-second wall-clock guard is replayed once with the
snapshot's 300-second guard before it counts as `seam-open`, because the
30-second guard depends on load: genbeta 2495, 2497 and 2499 hit it in the
native run but not in the double-only run. With the replay, the seam census
has no failure in either mode. Native: 7,330 pass and 2,364 stops; the genbeta
replays end in `use mlpower` limit stops. Double-only: 7,328 pass and 2,366
stops; the emg 1311 and 1343 replays end in numerical-accuracy stops. Note
that seam-check (built with `-std=c++17`, no `-DNDEBUG`) terminates emg 1311
within 300 s, while the snapshot (gnu++17, `-DNDEBUG`) hits its 300 CPU-second
guard; fit-judge keeps that entry.

Runs (workstation, one slot):
[stored baselines](2026-11-13-strict-2-data/just-strict-baseline.txt) and
[fresh worktree snapshots](2026-11-13-strict-2-data/just-strict-fresh.txt):
both 343 listed, 0 new, 0 listed but passing, exit 0. A warm run takes about
9 CPU minutes, mostly the seam probes; the first run in a worktree refits the
seam census (about 13 and 7 CPU minutes for native and double-only).

## 5. Diagnostic sweep (TASK-214.7)

**Still needed.** The sweep is the only check of the core's own status
contract. It removes the binding guards, so invalid data reach the core, and
checks that no core result is `Ok` with non-finite or out-of-domain
coefficients or on invalid input. It covers every supported hold subset with
the degenerate and support-boundary classes (lower, below, upper, above, unit
boundary, fractional) and runs in the C++ dev tier, the merge gate. The
snapshot sees core statuses only for inputs that pass the adapters and only
under the golden holds. The strict layer is not a merge gate.

**Updated.** A full regeneration no longer works: the generator cannot parse
several current adapters (cmp computes its result in an immediately invoked
lambda; lst, lomax, logitnorm and others call the core inside a lambda; many
define result-classifying helpers before the call). I added a cmp special
case, extended the guard stripper to the `invalid_argument` and outcome
helpers, and added `--add-to FILE --families ids`. It splices freshly
generated routes and test blocks for the given families into the checked-in
sweep, dropping binding-only helper lambdas from them, and keeps every other
route verbatim, as retire-16 did when pruning. st, gengamma and expweibull are
now in [diagnostics-audit.cpp](../../validation/diagnostics-audit.cpp): 85
families, 4,864 rows, no defect, native and double-only. Their core statuses
are in [diagnostics-sweep.csv.gz](2026-11-13-strict-2-data/diagnostics-sweep.csv.gz).
`just cpp-test`: 109 of 109 passed (diagnostics_tests 3.4 s).

**What would replace it:** a core-level probe mode in `fit-snapshot.py`,
calling each family's `*_mle` with the sweep's classes under every hold
subset, plus a strict rule "a core `Ok` is finite, in the domain and on valid
input". It would also have to become part of the merge gate. Until then the
82 older routes are copies of the 2026-10-31 binding routing; a rewrite of
the generator's route extraction (follow the call into a lambda) would let a
full regeneration track today's adapters.

## Known entries added

`strict-known.json` now holds 343 entries (the 21 of part 1 unchanged):

| check | entries (both modes) | card | finding |
|---|---:|---|---|
| `density` (Ok) | 40 | TASK-214.3 | `ok-loglik-off-density` |
| `density:limit` | 46 | NEW | `limit-loglik-off-density` |
| `density:limit` | 32 | NEW | `limit-coefficients-exclude-observation` |
| `density:limit` | 4 | NEW | `limit-coefficient-infinite` |
| `phenomenon:empty` (cauchy) | 2 | TASK-214.3 | `cauchy-empty-nonexistence` |
| `phenomenon:empty`, `non-finite`, `outside-support` | 86 | NEW | `invalid-data-wrong-reason` |
| `phenomenon:one-constant`, `valid-data` | 64 | NEW | `valid-sample-refused-as-invalid` |
| `phenomenon:constant-scale` (pareto3) | 2 | NEW | `constant-data-numerical-failure` |
| `refine` | 46 | TASK-214.5 | `off-maximizer` |

New findings, for cards:

- **Wrong `logLik` on `Ok` fits** (TASK-214.3 scope, new instances): 20 cases
  per mode; the largest is gamma near-constant, reported 108.2 against 104.3.
- **Fits outside the accuracy contract** (TASK-214.5 scope, new instances):
  burr (5 new rows), pareto3, gompertz, bhatt, dnorm, gamma, nbinom, cauchy,
  logis.
- **NEW: limit-stop payloads** that disagree with the limit model's density
  (23 cases per mode), place an observation outside the limit support by one
  spacing (16), or carry an infinite coefficient (2).
- **NEW: wrong reasons for invalid data:** invalid-argument, no-convergence
  or nonexistence where `invalid-input` is defined (43 per mode).
- **NEW: valid small samples refused as invalid input** (32 per mode, 13
  families), with the matching `one`/`constant` inconsistency.
- **NEW: pareto3** constant data with a free scale end in an evaluation failure.

## Against master `051a3299` (fix-nonfinite merged)

[master-fix-nonfinite.sh](2026-11-13-strict-2-data/master-fix-nonfinite.sh)
rebuilds master's seven changed families (cauchy, gamma, gompertz, gumbel,
logis, norm, weibull) in both modes, swaps their records into the baselines
and runs the snapshot checks
([output](2026-11-13-strict-2-data/master-fix-nonfinite.txt)).

- **Will pass after that merge** (11 entries): fit-judge `invalid-input`
  cauchy empty (both modes) and `mode` gamma huge, which master already pruned;
  `phenomenon:empty` cauchy and gompertz, `phenomenon:non-finite` gompertz nan
  and `phenomenon:outside-support` gompertz negative (both modes). Run
  `--prune` after merging.
- **New failures on master** (4): `density` norm near-constant and norm offset,
  both modes. The fix returns the exact-mean `sd` (near-constant: `1.6355e-18`,
  `logLik` 118.6069), but at the returned, rounded mean the likelihood is
  117.8569, 0.75 below the reported value (mpmath agrees with the density).
  At `c167a975` both cases passed. This is the non-Lipschitz case of the
  contract: the fit needs a dossier allowance or a conditioning stop. Until
  one of those, `just strict` on master needs these four as TASK-214.3
  entries.
- Unchanged: the gamma, weibull, logis and gumbel wrong-`logLik` entries
  (their near-constant and offset fits did not change), and all refinement
  entries.

## Checks run

| run | machine | result |
|---|---|---|
| fit snapshots of the worktree, four parts, `run4` | workstation | 0 differences from the stored baselines |
| `just strict` on stored baselines | workstation | exit 0, 343 listed, 0 new |
| `just strict` on fresh snapshots | workstation | exit 0, 343 listed, 0 new |
| seam census, both modes | workstation | no failure after replay |
| refinement, 1,911 references | workstation, one slot | 2,784 CPU s |
| `just cpp-test` | workstation, `run4` | 109/109 |
| diagnostic sweep, double-only build | workstation | exit 0, no defect |

`just cpp-test-no-long-double` was not run as a full suite: no library code
changed, and the only changed C++ test (the sweep) was compiled and run in the
double-only build directly. No site, JS or R files changed.

## Files

New: `dev/validation/strict_common.py`, `fit-density.py`,
`fit-consistency.py`, `fit-refine.py`, `strict-seam.py`,
`strict-refine.jsonl.gz`. Changed: `fit-judge.py` (shared ratchet),
`strict-known.json`, `diagnostics.py` (cmp case, guard helpers, `--add-to`),
`diagnostics-audit.cpp` (three families spliced in), `diagnostics.md`,
`README.md` (new section), `Justfile` (`strict`). `quality/reference.py`,
`fit-snapshot.py`, adapters, `mle.hpp` and `dpqr.hpp` are unchanged.

## Open

- Rule questions 1 to 5 in section 2.
- The 204 open refinement cases (section 3). cmb and cmp need a cheaper
  symbolic normalizer to be refined within the guard; a dossier `bound` flag
  for the location of hcauchy, ht and gpd would let `reference()` treat it as
  a closed form, rather than the fallback's on-the-bound detection.
- Limit-stop payloads: whether `limit_loglik` or `limit_estimates` is wrong
  in the 23 `limit-loglik-off-density` cases needs the limit model refit at
  50 digits (not done; the refinement covers estimates only).
- A full regeneration of the diagnostic sweep (generator rewrite).
- On merge with master: take master's pruned `strict-known.json` entries,
  add these, then `--prune`; the four norm entries above need a decision.

## Appendix: outcome of the degenerate and invalid classes

Codes: `E` estimate (`u` uncertified, `m` not unique), `L` limit (`m`
decided by a margin), `N:` nonexistence, `F:` failure with its reason.
Native and double-only agree in every cell. Default holds are the golden
fit's (`fit_args`).

| family | empty | nan | inf | neginf | negative | zeros | one | two | constant | ties | near-constant |
|---|---|---|---|---|---|---|---|---|---|---|---|
| bbinom | F:invalid-input | - | - | - | F:invalid-input | L | L | E | L | L | L |
| beta | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | N:unbounded | E | N:unbounded | E | E |
| betapr | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | N:unbounded | Eu | N:unbounded | E | F:evaluation |
| bhatt | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | L | N:unbounded | N:unbounded | L | N:unbounded | L | L |
| binom | F:no-convergence | - | - | - | F:invalid-input | E | E | E | E | E | E |
| binom +truncate=tbinom | F:invalid-argument | - | - | - | F:invalid-argument | F:invalid-input | E | E | E | E | E |
| binom+zero=inflated | F:invalid-input | - | - | - | F:invalid-argument | E | E | E | E | E | E |
| binom+zero=modified | F:invalid-input | - | - | - | F:invalid-argument | E | E | E | E | E | E |
| binom+zero=truncated | F:invalid-input | - | - | - | F:invalid-argument | F:invalid-input | E | E | E | E | E |
| bnbinom | F:invalid-input | - | - | - | F:invalid-input | L | L | L | L | Lm | L |
| burr | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | L | N:unbounded | E | F:evaluation |
| cauchy | N:unbounded | F:invalid-input | F:invalid-input | F:invalid-input | E | N:unbounded | N:unbounded | E | N:unbounded | N:unbounded | N:unbounded |
| cmb | F:invalid-input | - | - | - | F:invalid-input | E | L | L | L | L | L |
| cmp | F:invalid-input | - | - | - | F:invalid-input | E | L | E | L | E | L |
| dgamma | F:invalid-input | - | - | - | F:invalid-input | L | L | E | L | E | L |
| dlaplace | F:invalid-input | - | - | - | Em | L | L | Em | L | E | E |
| dnorm | F:invalid-input | - | - | - | E | L | L | L | L | L | L |
| dunif | F:invalid-input | - | - | - | E | E | E | E | E | E | E |
| dweibull | F:invalid-input | - | - | - | F:invalid-input | L | L | E | L | E | L |
| emg | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:unresolved | N:unbounded | N:unbounded | L | N:unbounded | F:unresolved | F:evaluation |
| exp | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | N:unbounded | E | E | E | E | E |
| expweibull | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | N:unbounded | F:no-convergence | N:unbounded | Eu | F:no-convergence |
| fatigue | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | N:unbounded | E | N:unbounded | E | F:no-convergence |
| foldnorm | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | N:unbounded | N:unbounded | E | N:unbounded | E | E |
| fpareto | F:invalid-input | F:invalid-argument | F:invalid-input | F:invalid-argument | F:invalid-argument | F:invalid-argument | F:invalid-input | E | F:invalid-input | E | F:evaluation |
| frechet | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | L | N:unbounded | F:invalid-input | F:invalid-input | N:unbounded | E | N:unbounded |
| gamma | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | N:unbounded | E | N:unbounded | E | E |
| gb2 | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | Lm | F:invalid-input | F:unsupported | F:unsupported |
| ged | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | L | N:unbounded | N:unbounded | L | N:unbounded | E | E |
| genbeta | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | N:unbounded | E | N:unbounded | L | F:no-convergence |
| gengamma | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | N:unbounded | F:no-convergence | N:unbounded | E | F:no-convergence |
| geom | F:invalid-input | - | - | - | F:invalid-input | E | E | E | E | E | E |
| geom+zero=modified | F:invalid-input | - | - | - | F:invalid-argument | E | E | E | E | E | E |
| geom+zero=truncated | F:invalid-input | - | - | - | F:invalid-argument | F:invalid-input | E | E | E | E | E |
| gev | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | E | N:unbounded | F:invalid-input | F:invalid-input | N:unbounded | E | N:unbounded |
| gompertz | F:no-convergence | F:no-convergence | F:invalid-input | F:invalid-input | F:no-convergence | N:unbounded | N:unbounded | E | N:unbounded | E | F:no-convergence |
| gpd | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | N:unbounded | E | E | E | E | E |
| gumbel | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | E | N:unbounded | N:unbounded | E | N:unbounded | E | E |
| hcauchy | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | N:unbounded | E | E | E | E | E |
| hnorm | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | E | N:unbounded | N:unbounded | E | N:unbounded | E | E |
| ht | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | N:unbounded | Lm | Lm | Lm | Lm | Lm |
| huber | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | E | N:unbounded | N:unbounded | E | N:unbounded | E | F:no-convergence |
| hyper | F:invalid-input | - | - | - | F:invalid-input | E | E | E | E | E | E |
| invburr | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | L | N:unbounded | E | F:evaluation |
| invexp | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | E | E | E | E | E |
| invgamma | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | N:unbounded | E | N:unbounded | E | Eu |
| invgauss | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | N:unbounded | E | N:unbounded | E | E |
| invparalogis | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | N:unbounded | E | N:unbounded | E | F:no-convergence |
| invpareto | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | Lm | Lm | Lm | Lm | Lm |
| invtrgamma | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | L | N:unbounded | E | L |
| invweibull | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | N:unbounded | E | N:unbounded | E | Eu |
| kumar | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | E | F:invalid-input | E | F:no-convergence |
| laplace | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | Em | N:unbounded | N:unbounded | Em | N:unbounded | E | E |
| lgamma | F:no-convergence | F:no-convergence | F:no-convergence | F:no-convergence | F:no-convergence | F:no-convergence | N:unbounded | E | N:unbounded | E | Eu |
| lgser | F:invalid-input | - | - | - | F:invalid-input | F:invalid-input | E | E | E | E | E |
| lgser+zero=modified | F:invalid-input | - | - | - | F:invalid-argument | E | E | E | E | E | E |
| llogis | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | N:unbounded | E | N:unbounded | E | Eu |
| lnorm | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | N:unbounded | E | N:unbounded | E | Eu |
| logis | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | E | N:unbounded | N:unbounded | E | N:unbounded | E | E |
| logitnorm | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | N:unbounded | E | N:unbounded | E | Eu |
| lomax | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | Lm | Lm | Lm | Lm | Lm |
| lst | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | L | N:unbounded | N:unbounded | L | N:unbounded | N:unbounded | F:evaluation |
| naka | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | N:unbounded | E | N:unbounded | E | Eu |
| nbinom | F:invalid-input | - | - | - | F:invalid-input | E | L | E | L | L | L |
| nbinom+zero=inflated | F:invalid-input | - | - | - | F:invalid-argument | E | L | L | L | L | L |
| nbinom+zero=modified | F:invalid-input | - | - | - | F:invalid-argument | E | L | L | L | L | L |
| nbinom+zero=truncated | F:invalid-input | - | - | - | F:invalid-argument | F:invalid-input | L | L | L | L | L |
| nhyper | F:invalid-input | - | - | - | F:invalid-input | N:unattained | E | E | E | E | E |
| norm | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | E | N:unbounded | N:unbounded | E | N:unbounded | E | E |
| norm +truncate=tnorm | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | E | N:unbounded | N:unbounded | E | N:unbounded | E | E |
| nsbeta | F:invalid-input | F:invalid-argument | F:invalid-argument | F:invalid-argument | F:invalid-argument | F:invalid-argument | N:unbounded | E | N:unbounded | E | E |
| paralogis | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | N:unbounded | E | N:unbounded | E | F:no-convergence |
| pareto | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | N:unbounded | E | N:unbounded | E | E |
| pareto2 | F:invalid-input | F:invalid-argument | F:invalid-input | F:invalid-argument | F:invalid-argument | F:invalid-argument | L | E | L | E | L |
| pareto3 | F:invalid-input | F:invalid-argument | F:invalid-input | F:invalid-argument | F:invalid-argument | F:invalid-argument | F:evaluation | E | F:evaluation | E | Eu |
| pareto4 | F:invalid-input | F:invalid-argument | F:invalid-input | F:invalid-argument | F:invalid-argument | F:invalid-argument | F:invalid-input | L | N:unbounded | E | F:evaluation |
| pig | F:invalid-input | - | - | - | F:invalid-input | E | L | L | L | L | L |
| pois | F:invalid-input | - | - | - | F:invalid-input | E | E | E | E | E | E |
| pois +truncate=tpois | F:invalid-input | - | - | - | F:invalid-argument | F:invalid-input | E | E | E | E | E |
| pois+zero=inflated | F:invalid-input | - | - | - | F:invalid-argument | E | E | E | E | E | E |
| pois+zero=modified | F:invalid-input | - | - | - | F:invalid-argument | E | E | E | E | E | E |
| pois+zero=truncated | F:invalid-input | - | - | - | F:invalid-argument | F:invalid-input | E | E | E | E | E |
| power | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | N:unbounded | E | N:unbounded | E | E |
| rayleigh | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | N:unattained | E | E | E | E | E |
| sged | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | Lm | N:unbounded | N:unbounded | L | N:unbounded | Eu | Lm |
| sgomp | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | N:unbounded | N:unbounded | E | N:unbounded | E | F:no-convergence |
| skellam | F:invalid-input | - | - | - | E | E | E | E | E | E | E |
| slash | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | E | N:unbounded | N:unbounded | E | N:unbounded | N:unbounded | N:unbounded |
| sn | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | Lm | N:unbounded | F:invalid-input | Lm | N:unbounded | Lm | F:no-convergence |
| snorm | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | L | N:unbounded | N:unbounded | L | N:unbounded | L | L |
| sstd | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:evaluation | N:unbounded | F:invalid-input | F:evaluation | N:unbounded | F:no-convergence | F:no-convergence |
| st | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | Lm | N:unbounded | N:unbounded | F:no-convergence | N:unbounded | N:unbounded | N:unbounded |
| std | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | L | N:unbounded | N:unbounded | L | N:unbounded | N:unattained | F:evaluation |
| tlambda | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | Eu | N:unbounded | F:invalid-input | F:invalid-input | N:unbounded | F:invalid-input | F:invalid-input |
| trgamma | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | Lm | N:unbounded | E | F:evaluation |
| triang | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | E | N:unbounded | N:unbounded | E | N:unbounded | E | F:evaluation |
| unif | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | E | N:unbounded | N:unbounded | E | N:unbounded | E | E |
| weibull | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | F:invalid-input | N:unbounded | E | N:unbounded | E | E |
| yulesimon | F:invalid-input | - | - | - | F:invalid-input | F:invalid-input | L | E | L | E | E |
| zeta | F:invalid-input | - | - | - | F:invalid-input | F:invalid-input | L | E | L | E | E |
| zeta +truncate=zipf | F:invalid-input | - | - | - | F:invalid-argument | F:invalid-input | L | E | L | E | E |
