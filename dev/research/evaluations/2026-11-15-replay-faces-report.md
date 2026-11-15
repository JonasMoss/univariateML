# Certificate replay, mechanism D (exact face inequalities)

Job `replay-faces` (TASK-214.28). Code: `dev/validation/cert_replay_faces.py` (registered in
`dev/validation/cert-replay.py`; families that `cert_replay_roots.py` also replays, pig and hcauchy among the faces
families, get both modules' comparisons concatenated by `combined()`). Design:
[certificate-replay](../../design/certificate-replay.md). Template: [replay-roots report](2026-11-14-replay-roots-report.md).
Baselines: post-5b snapshots of master (commit a1166cbf, the merge of core5b-d, in the `validate` worktree
`build/snapshots`): `native`, `routes-native`, `double`, `routes-double`. Code read at master 9141de55; the C++ between
a1166cbf and 9141de55 does not touch these families.

## Method and evidence level

Each replay reads the claim from the snapshot record (outcome kind, reason, certified, unique, limit model and payload,
returned coefficients), states the inequality that the family README attaches to that claim, and evaluates it on the
binary64 data. Evidence level per comparison is the `rigor` field of the records:

- `exact`: rational arithmetic (`fractions.Fraction`; every double is a dyadic rational) or integer counts.
- `interval`: mpmath `iv` at 50 digits, outward rounded (sgomp face score, zip zero fraction, the two-sided tnorm face).
- No `numerical` (non-enclosed) comparison is used by this module.

Nothing is read from the core beyond the record, so a changed outcome mapping (step 5b) is judged by the same predicates.
Where the core's own floating-point evaluation is simple, the record gives a mirror of it (Python floats for binary64,
`numpy.longdouble` for native builds); mirrors are context, not evidence.

Comparisons of this module per family, native mode (double mode identical unless noted; counts are distinct
case x mask entries, each with one comparison):

| family | comparison | rigor | native | double |
|---|---|---|---|---|
| binom | moment | exact | 34 | 34 |
| binom | support, unique | exact | 2, 1 | 2, 1 |
| gompertz | moment | exact | 51 | 50 holds, 1 fails |
| gompertz | support | exact | 3 | 3 |
| pig | moment | exact | 45 | 45 |
| pig | support | exact | 13 | 13 |
| pig | unique | exact | 1 fails | 1 fails |
| foldnorm | quartic, location, support | exact | 2, 24, 27 | same |
| bbinom | support | exact | 104 | 104 |
| cauchy | multiplicity, unique | exact | 60, 2 | 60, 2 |
| slash | multiplicity | exact | 51 | 51 |
| hcauchy | multiplicity | exact | 72 | 72 |
| sgomp | face-score | interval | 21 | 21 |
| sgomp | support | exact | 53 | 53 |
| dnorm | support | exact | 63 | 63 |
| dweibull | support | exact | 59 | 59 |
| dgamma | support | exact | 59 | 57 (2 cases not certified in double mode) |
| dlaplace | support, unique | exact | 63, 42 | 63, 42 |
| cmp | support | exact | 56 | 56 |
| cmp | unique | exact | 1 fails | 1 fails |
| cmb | support | exact | 55 | 55 |
| lgser, zeta, yulesimon | support | exact | 33, 27, 27 | same |
| pois+zero=inflated | zero-fraction | interval | 25 | 25 |
| pois+zero=inflated | support, unique | exact | 3, 9 | 3, 9 |
| norm +truncate=tnorm | window | exact 36, interval 16 | 52 | 52 |
| norm +truncate=tnorm | support | exact | 3 | 3 |
| zeta +truncate=zipf | support, unique | exact | 47, 1 | 47, 1 |

Total: 1191 comparisons (1145 cases) in native mode, 1189 (1143 cases) in double mode, counting only this module's
comparison names (the root module's `root` and `root-polish` for pig, hcauchy, dnorm, dweibull, lgser, zeta, yulesimon are in
the same records and unchanged). The run took 6 CPU s for both mode pairs. There are no `open` comparisons.

## Predicates and code sources

Full list with README sources in the module docstring. The decisions mirrored (core guards re-read at master 9141de55):

| family | predicate (exact) | code source |
|---|---|---|
| binom, all free | Poisson limit iff $nS \le nQ - S^2$ (mean at most population variance) | binom/mle.hpp:82-102 (`x_bar / var_pop <= 1.0`, unpadded) |
| binom, all-zero counts | estimate, not unique | binom/mle.hpp `unique` (new in 5b) |
| gompertz, all free | exponential limit iff $nQ \ge 2S^2$ | gompertz/mle.hpp:102 (`sum_xsq/n >= 2*mean^2`, unpadded; on data divided by the mean in the standardized path) |
| gompertz, a held | exponential limit iff $S \le aQ/2$ | gompertz/mle.hpp held-a guard |
| pig | Poisson limit iff $nF \le S^2$ (free), $n\mu^2 + F \le 2\mu S$ (mean held), $F = \sum x(x-1)$ | pig/mle.hpp `free_shape`:249, `binary_shape`:136 |
| foldnorm | free: returned $\mu = 0$ needs $H(0) = 1 - \overline{x^4}/(3\overline{x^2}^2) \le 0$; sigma held: $\mu = 0$ iff $\overline{x^2} \le \sigma^2$ | foldnorm/mle.hpp:213-214 (margin 32 eps), :120 (`m2 > s*s`) |
| bbinom, size 2 | finite iff $4 n_0 n_2 > n_1^2$, closed-form shapes; point mass, two-point; size 0 or size free with all-zero counts: not unique | bbinom/mle.hpp:698 (`N == 2`), Unidentified branch (5b) |
| cauchy, slash, hcauchy | largest multiplicity $k$: $2k < n$ estimate, $2k = n$ unattained, $2k > n$ unbounded; balanced two-point: estimate, not unique | cauchy/mle.hpp `fit_cauchy` run test (5b: `Unattained`, unique flag), slash/mle.hpp, ht/mle.hpp |
| sgomp, b held | $\eta = 0$ iff $\sum(1 - 2e^{-bx}) \le 0$ | sgomp/mle.hpp:66, :217 |
| zip | $\pi > 0$ iff $\#\text{zeros}/n > e^{-\bar x}$; all-zero: not unique | pois/zip_mle.hpp (`unique`, 5b) |
| tnorm | exponential face iff population variance at least the variance of the mean-matched $t_2 = 0$ member; held mean: uniform face iff $\overline{(x-\mu)^2} \le ((a-\mu)^2 + (a-\mu)(b-\mu) + (b-\mu)^2)/3$; constant sample with an estimated bound: unbounded, also with mean or sd held | norm/tnorm_mle.hpp header, `estimated_bounds_collapse` (5b); norm/truncation.hpp `held_normal_window_mle` |
| dnorm, dweibull, dgamma, dlaplace, cmp, cmb, lgser, zeta, yulesimon, zipf | point-mass, two-point and existence statements of each README | per README |

Changes to the module from the previous agent's draft, after reading the 5b diff (2c62400a to 9141de55):

- docstring now says "code read at master 9141de55";
- bbinom: all-zero counts with free size and size 0 held are now judged (`unique`), because 5b made them certified
  non-unique estimates (previously unresolved);
- tnorm: constant sample with an estimated bound and held mean or sd is judged as unbounded, as the new core test
  `estimated_bounds_collapse` / `held_normal_window_mle` decides. The snapshots have no such case (the only constant
  samples are with both bounds held at infinity), so this rule is exercised by a negative control only.
- The guards for binom, gompertz, pig, foldnorm, slash, sgomp, zip, cauchy multiplicity are unchanged in substance
  (binom, zip, cauchy gained a `unique` flag; cauchy 2k = n reports `Unattained`, which the module already expected).

Comparison names do not collide with the root module: this module uses `moment`, `support`, `unique`, `multiplicity`,
`quartic`, `location`, `size2`, `closed-form`, `face-score`, `zero-fraction`, `window`; the root module uses `root`,
`root-polish`, `restore`, and `moment` only for pois+zero=truncated/modified and lgser+zero=modified, which this
module does not replay.

## Results

Every comparison holds except the five below (3 findings, each in native and double where applicable). There are no
`open` entries. Mirror (binary64/long double) agreement with exact arithmetic: all mirrors agree, except one case in the
double build (see failure 1). The closest cases to a face in the baselines (relative gap of the two sides) are
gompertz 0.053 (`resample/n3-s3`), binom 0.077 (`archive/009644`), foldnorm 0.052, pig 0.13; none is closer than 5%, so
the baselines do not stress the unpadded binary64 guards (a near-boundary study for them is left to TASK-214.27). All numbers are in `2026-11-15-replay-faces-data/summary.out`.

## Failures

All three are real findings (the exact predicate disagrees with what a certified outcome claims), not replay bugs.

1. **gompertz, `class/huge`, double-only build** (key `cert-replay:moment|double|gompertz|class|huge`). The record is
   a certified `limit` (exponential, rate of the order of the smallest normal double). Data are six values between 1.33e307 and
   1.7e308. Exact: $nQ - 2S^2 = -1.51\cdot 10^{617} < 0$ (relative gap 0.304), so the profile has a finite maximizer and the
   exponential limit is not the answer. Mirror: in a double-only build the sum is $+\infty$, the standardization divisor
   (the mean) is not finite, `gompertz_mle_impl` falls back to the raw data, `sum_xsq / n` and `2 mean^2` are both $\infty$,
   and `inf >= inf` selects the limit. The native (long double) mirror standardizes by the mean 8.30e307, gets
   $1.391 < 2.0$ and decides finite, as the exact predicate does. The native record is an `ok` estimate and holds. This
   is the already known mode difference `mode|both|gompertz|class|huge` (strict-known.json, TASK-214.13, "native ok,
   double-only limit"); the replay adds that the double-only answer is the wrong one: a certified limit whose defining
   inequality is false. Mechanism: overflow of `sum_x` and `sum_xsq` before the guard.
2. **pig, `class/zeros` (all-zero counts, both coefficients free), native and double** (keys
   `cert-replay:unique|native|pig|class|zeros`, `...|double|...`). Record: certified `estimate`, `unique = true`, estimates
   mean = 1, shape = 0. The pig README ("Zero member and limit faces") states that this is a counterexample to joint
   uniqueness: $(\mu, 0)$ attains the maximum for every $\mu$. The outcome should carry `unique = false`.
3. **cmp, `class/zeros` (all-zero counts, both free), native and double** (keys `cert-replay:unique|native|cmp|class|zeros`,
   `...|double|...`). Record: certified `estimate`, `unique = true`, lambda = 0, nu = 1. The cmp README states that all-zero
   data give $\lambda = 0$ with $\nu$ unidentified. The outcome should carry `unique = false`. Master 5b already sets
   `unique = false` for the analogous all-zero cases of binom, zip, cauchy (balanced two-point) and zipf
   (checked as `holds` here; bbinom has only the constructed controls), so pig and cmp are the remaining ones in these families.

No entry was added to `strict-known.json` (the job ran with an empty known list; the tracked file was neither read for
verdicts nor changed). The five failure keys above are what the ratchet reports as NEW against an empty list.

## Negative controls

`2026-11-15-replay-faces-data/negative-controls.py`, output `negative-controls.out`: 40 controls, all fail their
named comparison as intended (and a sanity record, the exact size-2 closed form, holds). Each takes a stored record (or,
where the baselines have no such case, a constructed one) and changes the claimed outcome, the uniqueness flag, a returned
coefficient or the data:

- binom, gompertz (free and a held), pig (free and mean held), foldnorm (H(0) and sigma held): finite claimed as limit or
  the reverse, mu flipped, size shifted by one;
- bbinom: constructed size-2 counts $(n_0, n_1, n_2) = (2, 1, 3)$, $D = 23$, claimed binomial face; alpha scaled by
  $1 + 10^{-8}$; size free with all-zero counts and size 0 held, each claimed unique;
- cauchy (finite claimed unbounded, 2k > n claimed unattained, balanced two-point claimed unique), slash, hcauchy;
- sgomp b held with eta flipped; zip pi flipped and all-zero claimed unique;
- dnorm, dweibull, dgamma, cmp, cmb, lgser, zeta, yulesimon: finite sample claimed point mass; dlaplace uniqueness
  flag flipped; binom all-zero claimed unique;
- tnorm: one-sided, two-sided and mean-held faces claimed finite or the reverse; constructed constant sample with held mean
  and a, claimed finite;
- zipf point mass claimed at the other end; pig and cmp all-zero (as master returns them) claimed unique, which is
  how the two findings above fail.

## Not replayed (listed, not judged)

Counts per reason, both modes, are in `run.log`. The main ones: hold subsets with no face inequality (binom size or
prob held: 94; cauchy and slash with scale held: location search, 36 and 16; gompertz b held: analytic; hold subsets
of the discrete families with no exact support predicate, e.g. cmb 118); evaluation-only masks; foldnorm free fits with
$\mu > 0$ (78: $H(0)$ does not decide a returned positive mu); tnorm with sd held or mean held on a one-sided window
(no face); pois+zero=inflated with pi held (held-rate certificate, not mechanism D); cases with no certified outcome
(budget, invalid, numerical, conditioning). bbinom with all three coefficients free is `Unsupported` in the core (5b)
and has no record. The tnorm constant-sample collapse rule with held mean or sd has no baseline case.

## Files changed

- `dev/validation/cert_replay_faces.py` (new; edited from the draft as listed above), `dev/validation/cert-replay.py`
  (registration and `combined()`, from the earlier setup; no change by me).
- `dev/research/evaluations/2026-11-15-replay-faces-data/`: `records.jsonl.gz` (all comparisons, both modes), `run.log`,
  `summary.out` (from `summarize.py`), `negative-controls.py`, `negative-controls.out`, `summarize.py`.
- This report.

Command (one slot, 6 CPU s, 50 digits, empty known list):

    cd dev/validation && OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 /tmp/umvl-cpu/run nice -n 10 python3 cert-replay.py \
      $S/native,$S/routes-native $S/double,$S/routes-double --known /tmp/faces-none.json \
      --family binom,gompertz,pig,foldnorm,bbinom,cauchy,slash,hcauchy,sgomp,dnorm,dweibull,dgamma,dlaplace,cmp,cmb,lgser,zeta,yulesimon,'pois+zero=inflated','norm +truncate=tnorm','zeta +truncate=zipf' \
      --records records.jsonl

with `S=/home/jonas/Files/programming/univariateML.wt/validate/build/snapshots` and `/tmp/faces-none.json` =
`{"known": {}}`. Exit status 1 (the 5 failures are new against an empty list).
