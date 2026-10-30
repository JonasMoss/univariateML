# TASK-78.6 — tail-accurate discrete d/p/q/r

Base: `f4d1c1224bee8e71a907b383b31c8d1b2eafe8d4`, branch `codex/fixC`.
No commits, board edits, signature changes, fitter edits or golden changes.
All compiles/suites after the reviewer redirect run through `/tmp/umvl-simbox/run`.
The local baseline was interrupted during that redirect; completed baselines are on simbox.
Scratch scripts, numerical results and full logs: `/tmp/umvl-codex/fixC/`.

## Files and implementation (one line per family)

- **zip:** log density underflowed through exp/log; upper tails cancelled and log quantiles lost range. Keep density/mixing in logs, combine direct Poisson tails and invert the smaller log tail.
- **geom:** density rounded nonintegers; upper tails cancelled and upper/log quantiles lost range. Enforce integer support, retain analytic log survival and use integer inversion, including rounded-log-half inputs.
- **lgser:** upper tails subtracted nearly-one sums; huge counts cast to integers and log endpoints failed. Use shared positive conditional-tail sums with a geometric remainder bound, and log-tail inversion.
- **pois:** density accepted adjacent nonintegers; tail logs underflowed and upper quantiles complemented rounded probabilities. Enforce integer support, use direct smaller Boost tails with bounded log-series fallback, and integer inversion.
- **nbinom:** density accepted adjacent nonintegers; huge tail logs and upper/log quantiles failed. Enforce support, stabilize the large-count gamma ratio, add bounded tail sums, and invert in log space.
- **zinb:** mixture survival underflowed and upper quantiles cancelled. Mix both accurate NB log tails and invert directly; density reuses the corrected NB log PMF.
- **zmnbinom:** shared density cast huge counts to int and survival lost range. Remove the cast, reuse the NB log PMF/tails, and form the conditional lower tail directly.
- **ztnbinom:** same shared cast/range defects, plus tiny conditional-CDF cancellation. Use corrected NB tails and directly sum small positive conditional lower tails.
- **ztpois:** shared subnormal survival logs and tiny conditional lower tails lost accuracy. Normalize accurate Poisson log tails and sum the small conditional lower tail directly.
- **ztbinom:** tiny conditional CDFs cancelled when derived from survival. Sum the positive lower masses directly, use accurate binomial tails elsewhere, and explicitly retain the unit-probability point mass (also preserving inherited zib behavior).
- **zmpois:** subnormal survival was rounded before taking logs. Shared code now normalizes an accurate Poisson log survival before exponentiating.
- **binom:** quantiles lost log-input precision at an exact half jump. Use integer log-tail inversion and preserve the strict side of rounded log-half input; correct the independently proven oracle upper median error.
- **dgamma:** at huge x, x+1 rounded to x and the interval mass became zero. Retain the gamma interval width analytically in log space in that regime; fitted-body evaluation is unchanged.
- **dnorm:** huge interval endpoints collapsed, and rounded log-half input selected the wrong integer. Retain the far-tail interval factor analytically and preserve the strict log-half side; fitted-body density evaluation is unchanged.
- **dunif:** tiny lower probabilities rounded the inverse outside support; log endpoints failed. Invert log tails within integer support, compute the smaller count directly, and guard overflowing support widths in log space.

Changed files: those ten families' `dpqr.hpp` (`zip`, `geom`, `lgser`, `pois`,
`nbinom`, `zinb`, `binom`, `dgamma`, `dnorm`, `dunif`); all fifteen assigned
`test.cpp`; `ztpois/dpqr_core.hpp`; `special/tails.hpp`; binomial `truth.py`
and its regenerated `truth.json`; this report. The five zero-family wrapper
headers need no changes because they already share `ztpois/dpqr_core.hpp`.

Shared helpers in `special/tails.hpp`: bounded scaled positive-series summation,
integer log-tail inversion with guarded bracketing, and discrete log-input
half-boundary handling. Poisson, NB and binomial all need these helpers;
mixture and conditional families reuse them. New arithmetic is double only.
No Boost includes bypass `special/special.hpp`.

## Oracle correction and regression checks

At 100 decimal digits, the exact positive polynomial sum gives
`sum(binomial(9,k), k=5..9)/512 = 0.5`. Thus Binomial(9,0.5) has upper
quantile 4 at natural input 0.5. The old exp(log-PMF) route perturbed the
exact equality and returned 5. `binom/truth.py` now sums polynomial masses
without exponentiating logarithms. Regenerated with `quality.truth.make`
(the same generator used by `just dpqr-truth`); no JSON was edited by hand.
Four parameter points verified; 64 value rows and 176 quantile rows.

The same proof gives binary64 log(0.5) minus exact log(0.5) as
`2.3190468138462996154948554638754786504e-17`: its lower quantile can
legitimately differ from the natural 0.5 quantile. Explicit tests cover this.

All fifteen truth cases have `may_fail` removed. Added 27 targeted doctest
cases (with mpmath snippets): huge log densities, direct upper tails,
log input -10000, fractional support, adjacent nonintegers at zero,
conditional-CDF cancellation, subnormal survival and half-boundary cases.
No default truth tolerance was loosened.

## Validation

- Baseline simbox `just cpp-test`: **5/6 CTest targets passed**. Distribution
  tests: **503 cases, 501 passed, 2 failed, 3 skipped**; **955567 assertions,
  946618 passed, 8949 failed**, including permitted pre-existing truth failures.
  Unpermitted failures: beta-prime slow-oracle fitter status; Student-t `nu` golden.
- Baseline simbox `just test`: **[ FAIL 3 | WARN 0 | SKIP 0 | PASS 10547 ]**.
  Failures: `sstd` frozen fGarch comparison, `bnbinom` golden, `std` golden.
- First targeted simbox doctests: **100 cases / 187203 assertions, all passed**.
  Includes all fifteen promoted truth cases and their original sampler checks.
- Standalone numerical audit: **72994 comparisons, zero mismatches** across
  all fifteen truth tables, both tails/scales, log densities and both quantile
  input scales, at the original default tolerances. See `first-audit.log`.
- `just quality` for all fifteen assigned families: **15/15 pass**.
- Golden SHA-256 verification: **98/98 unchanged**. `git diff --check` passes.
- Independent 100-digit mpmath checks: **90/90 integer quantile brackets pass**
  at log probabilities -800, -10000 and -1000000, both tails, first parameter
  point of every assigned family (`extended.py`, `extended.log`).
- Exact sampler comparison against the base headers: **1540000/1540000 draws
  identical**, using the same 77 parameter points and seeds as the truth
  harness. All fifteen sampler function bodies/signatures also match HEAD.
- Final simbox `just cpp-test`: **5/6 CTest targets passed**, with exactly the
  same two unpermitted fitter failures as baseline. Distribution counts:
  **528 cases, 526 passed, 2 failed, 3 skipped**; **955689 assertions,
  951432 passed, 4257 failed** (remaining failures include other families'
  permitted truth mismatches). See `final2-ctest-full.log`.
  The first full run exposed an inherited zib unit-probability edge case;
  the shared point-mass guard and an assigned ztbinom regression fixed it.
- Final simbox `just test`: **[ FAIL 3 | WARN 0 | SKIP 0 | PASS 10547 ]**,
  identical to baseline, with the same `sstd`, `bnbinom` and `std` failures.
  No new R failures. See `baseline-r-remote.log` and `final-r.log`.
- Final source checks: all **98 golden hashes unchanged**; all fifteen
  sampler bodies/signatures unchanged; all fifteen truth decorators removed;
  `git diff --check` passes. No out-of-scope source files were changed.

Open issue: simbox already fails the unrelated fitter/reference checks listed
above on the untouched base. Those out-of-scope files and goldens are unchanged.
