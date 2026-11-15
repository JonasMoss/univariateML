# TASK-220: SGED natural far tails down to the smallest normal (sged-tail)

Baseline: master `2c62400a`. Worktree `claude/sged-tail`, uncommitted. Data,
scripts and the reproduction recipe:
[2026-11-14-sged-tail-data/](2026-11-14-sged-tail-data/)
([reproduce.sh](2026-11-14-sged-tail-data/reproduce.sh)). All runs on the
workstation (GCC 15); simbox was off.

## Result

The three truth consumers (C++, R, JS) now check natural probabilities down to
`DBL_MIN = 2.2250738585072014e-308` instead of `1e-300`. The two SGED truth rows
that this exposed (point 7: mean 0, sd 1, nu 30, xi 3) now return their natural
value with relative errors 2.8e-13 and 3.7e-13. A dense sweep of all 62 truth
parameter points of sged, snorm and sstd plus 61 extra points finds no refusal
and no contract failure for log p in [log DBL_MIN, -0.01], on either scale or
tail, for every skewed member. Baseline had 1,290 refusals there, and 125 wrong
natural values returned without a flag, up to 769 times the tolerance.

One finding is outside the files this task may edit. At xi = 1, psged delegates
to `pged`, whose double log radius loses up to 1.05e-8 relative in natural tails
at nu = 1e4 to 1e5 (109 sweep values, no truth point). A two-line prototype for
`ged/dpqr.hpp` clears all of them; it is not applied ([Open](#open)).

## Cause

For a thin-side lower tail, `snorm_detail::probability_with_moment` gates its
result on

$$
\text{margin}=E_r\,\bar h+2\cdot10^{-12}+32\epsilon,\qquad
E_r=\frac{E_c}{t^2}+2\epsilon v ,
$$

where $v$ is the standardized radius rounded to a double, $E_c$ the centering
margin and $\bar h$ a bound on the GED hazard over $[v-E_r,v+E_r]$. On the
natural scale it refused when the margin exceeded $10^{-11}$ and
$-700<\log p<-0.01$. Printed at the two rows
([margin.py](2026-11-14-sged-tail-data/margin.py),
[margin.json](2026-11-14-sged-tail-data/margin.json)):

| row | $v$ | $E_c/t^2$ | $E_r$ | $\bar h$ | exact $h$ on the interval | margin |
|---|---:|---:|---:|---:|---:|---:|
| lower, $x=-1.8189346365433414$ | 2.1867475184583984 | 8.6e-27 | 9.71e-16 | 9325.526 | 9325.507 | 1.106e-11 |
| upper, $x=2.548015429452742$ | 2.186982766165731 | 1.3e-27 | 9.71e-16 | 9354.621 | 9354.601 | 1.109e-11 |

These rows lie at $p\approx10^{-300}$, just below the old consumer floor, so the
old tests never checked them. The NaN was already there on master.

- **The hazard bound is tight.** It exceeds the exact hazard by a factor
  $1+2\cdot10^{-6}$. It is the integration-by-parts bound
  $h(v)\le g'(v)+(\nu-1)/v$ with $g=(v/\lambda)^\nu$, valid for $\nu\ge1$, and is
  proved below.
- **The radius term is the whole excess.** $E_r$ is exactly $2\epsilon v$: the
  centering term is $10^{-27}$. The real radius rounding is 1.5e-16 and 2.2e-16,
  inside the half-ulp bound $\epsilon v/2=2.4\cdot10^{-16}$, so the term is four
  times the rounding. The log tail is sensitive to the radius by a factor
  $v\,h(v)\approx\nu(v/\lambda)^\nu\approx\nu|\log p|$, which is about $2\times10^4$
  here, and $2\epsilon v\bar h=9.06\cdot10^{-12}$ plus the $2\cdot10^{-12}$ reserve
  passes $10^{-11}$.
- **The double result really is uncertain at a few times $10^{-12}$.** Rounding
  the radius moves the exact log tail by -1.39e-12 and -2.07e-12. On master the
  base evaluator `pged(v,0,1,nu,false,true)` forms $\log v-\log\lambda$ from two
  rounded doubles, and its own error is 1.71e-12 at the lower row. Over a grid of
  800 exact double radii ([evaluator.py](2026-11-14-sged-tail-data/evaluator.py))
  that error reaches 2.4 $\epsilon\, v h(v)$: 6.8e-12 at nu = 60, 7.5e-11 at
  nu = 1e3, 1.0e-9 at nu = 1e4 and 4.2e-8 at nu ≈ 1e6. That is more than its
  $2\cdot10^{-12}$ reserve.

The uncertainty grows like $\nu|\log p|$, so a looser gate would not have been
honest. At larger shapes, master also returned wrong natural values without a
flag. Between $\log p=-708.4$ and $-700$ the natural gate was skipped entirely:
`-700` stood in for the old $10^{-300}$ floor. At nu = 999999, xi = 3,
x = 1.7320729413654756, the exact $\log p$ is -700.49999993526 and master returned
5.980196965618661e-305, a relative error of 7.7e-8 (769 times the tolerance).

## Fix

1. **Natural gate floor** (`snorm/dpqr.hpp`, `sged/dpqr.hpp`). There is a new
   constant `snorm_detail::log_smallest_normal`, $-1022\log2$. The natural gates in
   `probability_with_moment` (snorm general moments, sged and sstd),
   `snorm_detail::probability` (psnorm) and `sged_logradius_probability` apply
   whenever `selected + margin >= log_smallest_normal`, that is, whenever the exact
   value can still be normal. This replaces `selected > -700`. The `-0.01` upper
   cut and all allowances are unchanged.
2. **Two-word log radius in the sged fast path** (`sged_detail::log_scaled_radius`).
   The `upper` and `central` evaluators now take $w=\log(v/\lambda)$ from
   $q=\mathrm{fl}(v/\lambda_{hi})$ plus the exact remainder
   $(\mathrm{fma}(-q,\lambda_{hi},v)-q\lambda_{lo})/\lambda_{hi}$. Here $\lambda$
   is `three_word::exponential(ged_detail::logscale_precise(nu))`, held in two
   words and cached per nu. On $[1/2,2]$ the result is `log1p((q-1)+correction)`,
   with `q-1` exact by Sterbenz; elsewhere it is `log(q)+correction/q`. When $q$
   would leave the normal range, $|w|>700$, and the plain difference is used,
   since neither tail amplifies it there. The evaluator is then
   `-log 2 + gamma_power_log(w, nu, upper)`, which is the same formula `pged` uses
   for $v>0$, but with the accurate $w$.
3. **Three-word fallback** (`psged`, general-moment branch). When the fast path
   refuses (NaN) and nu ≥ 0.05, psged returns `sged_precise(...)`: the existing
   three-word coordinate, three-word log radius and endpoint enclosure that
   already serve nu ≥ 1e6 and extreme skew. That route refuses only on its own
   gate. Nothing changes where the fast path returns a value, apart from the more
   accurate evaluator in item 2.
4. **Consumers.** `univariateML-cpp/tests/test_support.hpp` uses
   `natural>=std::numeric_limits<double>::min()` with the prescribed comment. In
   `bindings/r/tests/testthat/test-generated-dpqr.R`, `natural > 1e-300` became
   `natural >= .Machine$double.xmin`. In `bindings/js/test.mjs`,
   `natural>1e-300` became `natural>=2.2250738585072014e-308`. Each carries a short
   comment.

The `-0.01` cut in the shared gate was left alone. The sweep's near-one band
(log p in (-0.01, 0), 15,768 tail values over the three families) has no
refusal and a worst error of 1.9e-6 of the tolerance.

The dossier ([sged README](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md#dpqr))
gains one paragraph on the log-radius sensitivity, the two-word quotient and
the fallback.

## Error bounds: what is proved and what is practical

**Proved (exact arithmetic, conditional on the computed comparisons).**

- *Mean value.* $|\log S(\hat v)-\log S(v)|\le|\hat v-v|\sup_{[v-E_r,v+E_r]}h$.
- *Hazard bound.* For $\nu\ge1$, integrating by parts gives
  $S(v)=e^{-g(v)}/g'(v)-\int_v^\infty\frac{\nu-1}{t\,g'(t)}e^{-g(t)}\,dt$. Since
  $t\,g'(t)=\nu(t/\lambda)^\nu$ increases, the integral is at most
  $\frac{\nu-1}{v g'(v)}S(v)$. Hence $h\le g'(v)+(\nu-1)/v$; this is the
  existing bound, which the code evaluates at the interval ends.
- *Radius rounding.* Rounding the two-word radius to a double moves it by at
  most $\epsilon v/2$. The $2\epsilon v$ term covers this.
- *Exact remainder.* For normal $q=\mathrm{fl}(v/\lambda_{hi})$,
  $v-q\lambda_{hi}$ is exact. $q+$correction is $v/\lambda$ to $O(\epsilon^2)$,
  and $q-1$ is exact on $[1/2,2]$.
- *Three-word route.* `sged_precise` evaluates the tail at both ends of the
  propagated log-radius interval and requires the natural interval to lie within
  relative $10^{-10}$, the contract's tolerance.

**Practical (stated floating-point margins, as in the package standard).**

- The centering margin $64\epsilon^2(\dots)$, the three-word $B_3$ margin, and
  libm `log`, `log1p` and `exp` within an ulp or two.
- The fast path's evaluator reserve of $2\cdot10^{-12}+32\epsilon$. A
  first-order analysis of the new evaluator gives $|w|$ errors of a few ulps of
  $w$. Each ulp of $w$ costs $x=(v/\lambda)^\nu$ times $\epsilon|\log x|$, so the
  error is at most about $\epsilon x(4\ln x+7)\le2.6\cdot10^{-12}$ for
  $x\le709$. Measured over the 800-radius grid it is at most **1.06e-12**
  (nu = 3 to 999999, log S down to -708.39), against up to 4.2e-8 for the master
  evaluator. Where the pessimistic first-order figure exceeds the reserve, the
  unused $3\epsilon v/2$ of the radius term covers it when $v\,h(v)\approx\nu x\gtrsim2\cdot10^3$. At
  small $\nu$ the remaining overshoot is below $10^{-12}$, against a gate of
  $10^{-11}$ and a contract of $10^{-10}$.
- The three-word route's reserve `gamma_log_margin` is
  $2\cdot10^{-12}+16\epsilon|\log Q|$; its natural acceptance is the full
  $10^{-10}$.

**Numerical (sweep evidence, not proof).** The observed errors listed below.
A finite sweep does not prove the bound at every binary64 input.

## Sweep

[sweep.py](2026-11-14-sged-tail-data/sweep.py) places inputs with the
evaluator's own quantile, falling back to the reference quantile. The
reference for each input is computed independently at the exact double x, at
60 digits, with the family's `truth.py` antiderivative.

- **Grid.** 123 parameter points: all 62 truth points, and 61 extras.
  - sged extras: nu in {5, 10, 20, 25, 30, 40, 60, 100, 300, 1e3, 1e4, 1e5,
    5e5, 999999} and {0.06, 0.5, 1, 1.5} at xi in {3, 0.4}, plus the symmetric
    member at nu in {10, 30, 100, 1e3, 1e4, 1e5}.
  - snorm extras: five skews from 1e-5 to 1e3.
  - sstd extras: nu in {2.5, 4, 30, 1e3, 1e5, 1e7} at both skews, plus the
    symmetric member at nu in {30, 1e5}.
- **Targets.** Per point and tail, 43 fixed log p targets from log DBL_MIN to
  -0.01, dense near both ends, plus 24 seeded log-uniform draws: 16,482 inputs
  in all.
- **Checks.** Both tails are compared on both scales: log within
  $10^{-10}\max(1,|v|)$, natural within relative $10^{-10}$ when the exact value
  is normal. There are 45,404 in-scope checks.

The table counts in-scope checks; "symmetric" means xi = 1, where sged
delegates to pged and sstd to pstd, while snorm evaluates its own xi = 1 member. Worst is the largest finite error in units of
the tolerance.

| family | route | scale | checks | master NaN | master fail | now NaN | now fail | now worst |
|---|---|---|---:|---:|---:|---:|---:|---:|
| sged | skew | log | 9,402 | 429 | 0 | 0 | 0 | 0.0117 |
| sged | skew | natural | 9,402 | 861 | 125 | 0 | 0 | 0.0203 |
| sged | symmetric | log | 2,836 | 0 | 0 | 0 | 0 | 0.154 |
| sged | symmetric | natural | 2,836 | 0 | 109 | 0 | 109 | 105 |
| snorm | skew | log | 2,755 | 0 | 0 | 0 | 0 | 4.8e-6 |
| snorm | skew | natural | 2,755 | 0 | 0 | 0 | 0 | 0.0029 |
| snorm | symmetric | log | 939 | 0 | 0 | 0 | 0 | 3.3e-6 |
| snorm | symmetric | natural | 939 | 0 | 0 | 0 | 0 | 0.0020 |
| sstd | skew | log | 4,568 | 0 | 0 | 0 | 0 | 6.0e-5 |
| sstd | skew | natural | 4,568 | 0 | 0 | 0 | 0 | 0.0405 |
| sstd | symmetric | log | 2,202 | 0 | 0 | 0 | 0 | 3.4e-5 |
| sstd | symmetric | natural | 2,202 | 0 | 0 | 0 | 0 | 0.0236 |

Data: [result-baseline.json.gz](2026-11-14-sged-tail-data/result-baseline.json.gz)
and [result-current.json.gz](2026-11-14-sged-tail-data/result-current.json.gz)
record every NaN and every error above 0.1 of the tolerance, with inputs.

- **Master refusals.** All 1,290 master refusals are sged skew points with
  nu ≥ 30. They include truth point 7: 12 natural values, the exposed rows among
  them. Log-scale refusals occur from nu = 1e5.
- **Master wrong values.** The 125 wrong natural values are at nu ≥ 1e3; 124 of
  them lie in the old unchecked band [-708.4, -700].
- **Remaining failures.** The 109 remaining failures are all sged at xi = 1,
  nu = 1e4 and 1e5, log p from -708.39 to -30, at most 1.05e-8 relative. They
  come from pged and are not truth points; see [Open](#open).
- **The other two families.** snorm and sstd needed no change beyond the floor.
  Their worst errors are 0.3% and 4% of the tolerance, with no refusals in the
  new band.

The 3,899 archived TASK-189 SGED/SSTD probability witnesses (240-digit
references, 2026-11-11 skew smoke) all pass on both builds at the new natural
floor: worst 1.1e-13 (sged natural) and 2.0e-13 (sstd natural).
See [replay-current.json](2026-11-14-sged-tail-data/replay-current.json).

### Outside the interval

- **Log p below log DBL_MIN.** sged truth point 22 (nu = 1e8, xi = 1e8) at
  x = -1.7320508075688814 returns NaN for the lower log tail on both builds. The
  exact value is $\log p\approx-3.4\cdot10^{116238544}$, which is not
  representable; the correctly rounded output is $-\infty$. This lies below the
  task's interval and comes from the unchanged nu ≥ 1e6 route.
- **Subnormal natural results.** `skew_probability::interval` refuses subnormal
  natural results whose rounding exceeds relative $10^{-10}$. The contract asks
  for the rounded subnormal there. This is not examined further: it is below the
  floor.
- **Spot checks of the new helper at extreme radii** (nu = 0.06, 0.5, 1.5;
  x = ±1e300, 1.7e308, 1e-310, 5e-324;
  [edge.py](2026-11-14-sged-tail-data/edge.py), [edge.txt](2026-11-14-sged-tail-data/edge.txt)). All agree with
  the reference to 4e-14 or better, or are the correctly rounded $-\infty$. At
  nu = 0.06, x = 1.7e308, master refused and the new code is accurate.

## Open

**`pged` at large shape (GED family, `ged/dpqr.hpp`, not editable here).**

- **What fails.** For 1 < nu < 1e6, `pged` forms `la = log|q-mean| - log(sd) - ged_logscale(nu)` in
  double. In natural tails its error reaches 7.5e-11 at nu = 1e3, 1.0e-9 at
  nu = 1e4 and 4.2e-8 near nu = 1e6, with no gate. The log scale is within the
  contract. The same error reaches psged at xi = 1 (and pged directly). At
  nu = 1e5, x = -1.7321738832767992, the exact $\log p$ is -703.000000009 and the
  natural result is off by 1.05e-8 relative.
- **Prototype.** [pged-prototype.patch](2026-11-14-sged-tail-data/pged-prototype.patch)
  routes pged to its existing three-word `probability_precise` when
  $8\epsilon\,\nu(e^{\nu\,la}+1)>10^{-12}$.
  With it, all 2,836 symmetric sged checks pass, worst 0.0099 of the tolerance
  ([result-pged-prototype.json.gz](2026-11-14-sged-tail-data/result-pged-prototype.json.gz)).
- **Still needed.** It also needs the GED truth table, GED tests and a dossier
  note before it can be applied. The alternative is the two-word
  `log_scaled_radius` used here, applied to `pged`'s own log radius.

## Test runs

Workstation, 2026-11-15, on the worktree (master `2c62400a` plus this
change), each through `/tmp/umvl-cpu/run4`; logs in the worktree's
`build/scratch/final/`:

| suite | result |
|---|---|
| `just cpp-test` | 110/110 |
| `just cpp-test-no-long-double` | 154/154 |
| `just test` (R dev tier) | FAIL 0, WARN 0, SKIP 157, PASS 36,209 |
| `just js-build`, `just js-test` | built; 583 pass |
| `just site-check` | passed |

Without the `dpqr.hpp` fix, the same `DBL_MIN` floor makes `distribution_sged`
fail in both C++ builds and the JS sged truth test fail at truth point 7
(natural value NaN where the truth is about $10^{-300}$): see the test runs of
the [truth-meta report](2026-11-14-truth-meta-report.md), section 6.

## Files changed

- `univariateML-cpp/tests/test_support.hpp`, `bindings/js/test.mjs`,
  `bindings/r/tests/testthat/test-generated-dpqr.R`: natural floor `DBL_MIN`.
- `univariateML-cpp/include/cppunivariate/distributions/snorm/dpqr.hpp`:
  `log_smallest_normal`; natural gate floors in `probability_with_moment` and
  `probability`.
- `univariateML-cpp/include/cppunivariate/distributions/sged/dpqr.hpp`:
  `sged_detail::log_scaled_radius`; fast-path evaluators; three-word fallback;
  natural floor in `sged_logradius_probability`.
- `univariateML-cpp/include/cppunivariate/distributions/sged/README.md`: one d/p/q
  paragraph.
- This report and `dev/research/evaluations/2026-11-14-sged-tail-data/`.

No `mle.hpp`, fitter test, `core/`, `bindings/common/`, `test.cpp`, truth table or
golden file was touched. Fitters do not call psged, so fits are unchanged.
