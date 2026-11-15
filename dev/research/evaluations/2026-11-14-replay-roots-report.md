# Certificate replay, mechanisms B and C (scalar roots and represented likelihood)

Job `replay-roots` (TASK-214.29). Code: `dev/validation/cert_replay_roots.py` (29 replay functions, registered in
`dev/validation/cert-replay.py`, which also generalises `rebuild()` to modifier-route ids). Design:
[certificate-replay](../../design/certificate-replay.md). Baselines: `2026-11-14-fit-snapshot-data` (master 7e3c9779),
native and double-mirror snapshots, each with the modifier-route snapshot.

## Method and evidence level

Each certified outcome is replayed from the returned coefficients and the data at 50 digits. Mechanism C (scalar roots):
the score changes sign across the claimed bracket `[t(1-w), t(1+w)]` (or additive), with `w` the claimed width from the
code plus a stated rounding allowance. Mechanism B: the represented-likelihood check `restored(ideal, ll, tol, scale)`
of `core/transformed_data.hpp`. Evidence level per comparison is in the `rigor` field of the records: `interval` (mp.iv, outward
rounded), `exact` (rational arithmetic) or `numerical` (mp.mpf at 50 digits, not enclosed). Counts of comparisons by rigor in native mode:

- beta numerical 83
- dnorm numerical 48
- dweibull interval 25, numerical 17
- frechet interval 43, numerical 88
- gamma numerical 95
- genbeta interval 24, numerical 55
- gumbel interval 61
- hcauchy interval 69
- invgamma numerical 106
- invweibull numerical 120
- kumar interval 72
- lgamma numerical 92
- lgser interval 32
- lgser+zero=modified interval 23
- llogis numerical 124
- lnorm interval 69
- logis interval 35, numerical 39
- logitnorm interval 62
- lomax exact 2, interval 90
- naka numerical 101
- nbinom exact 35, interval 28, numerical 2
- nsbeta numerical 75
- pareto3 numerical 112
- pig numerical 31
- pois+zero=modified interval 31
- pois+zero=truncated interval 33
- weibull interval 66
- yulesimon interval 15, numerical 4
- zeta numerical 22

Hence most root verdicts here are `numerical` evidence (high-precision evaluation of the score at the bracket ends, no enclosure), not interval proofs; the failures below are `numerical` too wherever the table says so in the records.

Claimed widths and their source lines are in the
`CLAIMS` and `TOL` tables of the module and in the `note` of every record. Double values and the core margin are not
mirrored for these families (`double None`): the "double" snapshot was replayed too (same 20 failures, see below) but gives
no core-side comparison values.

Command (single slot, `/tmp/umvl-cpu/run`, 1139 CPU s for both mode pairs, 50 digits, `--known` an empty ratchet):

    cert-replay.py D/native.jsonl.gz,D/routes-native.jsonl.gz D/double.jsonl.gz,D/routes-double.jsonl.gz --known none.json --records records.jsonl

Total: 3328 cases judged in both modes (1664 per mode). Prototype check: nbinom 102 cases and lomax 118 cases, 220 cases,
314 comparisons, all holds, no failure, as before.

## Claimed widths (family, hold subset, source)

| family | hold subset | claimed width | code source |
|---|---|---|---|
| gumbel | free | relative bracket 1e-14 in sigma | gumbel/mle.hpp:108,115 |
| gumbel | mu held | bracket 1e-12 in 1/sigma | gumbel/mle.hpp:188 |
| weibull | free | relative 1e-14 in 1/shape | weibull/mle.hpp:73 -> gumbel/mle.hpp:108,115 |
| weibull | scale held | log shape bracket 1e-12 | weibull/mle.hpp:177-189 |
| kumar | a free | relative 1e-12 in a | kumar/certificate.hpp:89,108 |
| hcauchy | sigma free | log sigma 2e-14 (1+abs(log sigma)) | ht/mle.hpp:36-37 |
| lgser | theta free | relative 4 eps in u = -log(1-theta) | lgser/mle.hpp:69,73 |
| yulesimon | rho free | log rho 1e-10 | yulesimon/mle.hpp:235,239 |
| lnorm | mean free | restored(ideal, ll, 1e-10) | core/transformed_data.hpp:99-103,117 |
| beta | free | 2 min(tol, needed) in logit(mu), tol 1.49e-8 | beta/mle.hpp:226,416-439 |
| beta | one shape held | relative 1e-13 | beta/mle.hpp:315 |
| gamma | free, Newton path | Newton step below abstol 1.22e-4, no bracket | gamma/mle.hpp:176-186 |
| gamma | free, bracket path | relative 4 eps | gamma/mle.hpp:204-211 |
| gamma | scale held | log shape 1e-12 | gamma/mle.hpp:263-270 |
| gamma_raw (naka, invgamma, lgamma, ...) | free / held parameter | restored with abstol 1.22e-4; held: log bracket 2e-13, relative 4 eps (Square) | gamma/transformed.hpp:74-110,143 |
| invweibull, llogis, frechet (location transforms) | free scale | restored(tol) on the log-data likelihood | core/transformed_location.hpp:75-77 |
| invweibull, llogis | scale held | log shape bracket 4 eps absolute | core/transformed_location.hpp:16-19,51 |
| logis | free | Newton step below 1.22e-4 in mu and log scale | logis/mle.hpp:180 |
| logis | location held / scale held | 1e-12 | logis/mle.hpp:259 / :240 |
| dnorm | free or one held | Newton decrement tol 1e-9 | dnorm/mle.hpp:118,136 |
| dweibull | beta / alpha root | 4 eps abs(u) + 1e-12 / 1e-13 (1+abs(a)) | dweibull/mle.hpp:284-285 / :258 |
| zeta | free | abs(score) <= tol mean log x or bracket tol (s-1) | zeta/mle.hpp:158 |
| pig | shape held / shape free | log mean 1e-10 / log shape 2e-7 polished to 1e-12 | pig/mle.hpp:119,127 / :313-323 |
| genbeta | a, b, scale held / power + one shape held | padded signs at t(1 -/+ 2.5e-11) / relative 1.25e-11 | genbeta/mle.hpp:283-292 / :476-482 |
| frechet | lambda held | log(-mu_std) tol (1+abs(mid)) | frechet/certificate.hpp:60 |
| gev | sigma, xi held, -1 < xi < 0 | mu bracket tol sigma | gev/mle.hpp:279-290 |
| pois+zero=truncated / =modified | route | accuracy contract 1e-10 (no bracket in code) | pois/ztpois_mle.hpp:19-22 |
| lgser+zero=modified | route | residual 1e-10 mean or bracket 1e-12 | core/zero_modified.hpp:44-52 |
| zeta+truncate=zipf | route | padded signs at s -/+ rel/(1+rel) max(1,abs(s)) | zeta/truncation.hpp:140-156 |

The nbinom and lomax rows are those of the prototype (see its docstring).

## Results per family and comparison (native mode, distinct case/mask entries)

Verdicts `holds`/`fails`/`open` are judged; `info(x)` are comparisons the module reports without judging (the
certifying mechanism is another comparison, e.g. the restore check certifies while the shape bracket is informational),
with the verdict they would have in brackets. The `info(fails)` rows are not certificate failures: the claim they test is
not what certifies the outcome.

| family | comparison | cases | verdicts |
|---|---|---|---|
| beta | root | 83 | holds 83 |
| dnorm | root | 48 | holds 48 |
| dweibull | root | 42 | holds 42 |
| frechet | restore | 44 | holds 44 |
| frechet | root | 87 | fails 4, holds 61, info(fails) 1, info(holds) 21 |
| gamma | root | 66 | fails 3, holds 63 |
| gamma | root-tol | 29 | info(fails) 7, info(holds) 22 |
| genbeta | root | 79 | info(fails) 1, info(holds) 78 |
| gumbel | root | 61 | holds 61 |
| hcauchy | root | 69 | holds 69 |
| invgamma | restore | 41 | holds 39, info(n/a) 2 |
| invgamma | root | 65 | holds 24, info(fails) 19, info(holds) 22 |
| invweibull | restore | 60 | holds 58, info(n/a) 2 |
| invweibull | root | 60 | fails 3, holds 18, info(fails) 5, info(holds) 34 |
| kumar | root | 46 | holds 46 |
| kumar | root-b | 26 | info(n/a) 26 |
| lgamma | restore | 36 | holds 34, info(n/a) 2 |
| lgamma | root | 56 | holds 20, info(holds) 36 |
| lgser | root | 32 | holds 32 |
| lgser+zero=modified | moment | 23 | holds 23 |
| llogis | restore | 62 | holds 61, info(n/a) 1 |
| llogis | root | 62 | fails 7, holds 15, info(fails) 5, info(holds) 35 |
| lnorm | restore | 69 | holds 67, info(n/a) 2 |
| logis | root | 74 | holds 68, open 6 |
| logitnorm | restore | 62 | holds 61, info(n/a) 1 |
| lomax | concave | 11 | holds 11 |
| lomax | face | 29 | holds 29 |
| lomax | gain | 11 | holds 11 |
| lomax | global | 11 | holds 11 |
| lomax | root | 30 | holds 30 |
| naka | restore | 39 | holds 38, info(n/a) 1 |
| naka | root | 62 | fails 1, holds 22, info(fails) 12, info(holds) 27 |
| nbinom | dispersion | 35 | holds 35 |
| nbinom | root | 30 | holds 30 |
| nsbeta | root | 75 | holds 75 |
| pareto3 | restore | 56 | holds 53, info(n/a) 2, open 1 |
| pareto3 | root | 56 | holds 19, info(fails) 1, info(holds) 36 |
| pig | root | 22 | holds 22 |
| pig | root-polish | 9 | info(holds) 9 |
| pois+zero=modified | moment | 31 | holds 31 |
| pois+zero=truncated | moment | 33 | holds 33 |
| weibull | root | 66 | fails 2, holds 64 |
| yulesimon | root | 19 | holds 19 |
| zeta | root | 22 | holds 22 |

## Failures (20 comparisons in native mode, the same 20 in double mode)

Fixed during the job (replay bugs, not findings): the `lgser+zero=modified` moment check tested only the residual, while
the core also accepts a root bracket of width 1e-12 on the bisection path (10 spurious failures, now judged on either
claim); the `logis` free Newton check ignored the rounding of the public location (data near 2.4e15, spacing 0.5; a half
ulp of mu and eps in log s are now allowed).

Remaining failures are all `root` comparisons, mechanism C (rigor per record, see above), claimed width as listed in the table above.
Column "offset" is (returned - root) in the quantity of the claim (relative shape or log shape), from the high-precision
root; the claim is `abs(offset) <= w`, with `w` the claimed half width plus the rounding allowance of the module
(4 eps = 8.9e-16 for the location transforms and gamma; 0.5e-14 for weibull free). Case keys are
`family|source|case|mask` as in the snapshots; hold masks: 2 = scale held, 6 = mu and sigma held, 0 = all free. The
double value and the core margin are not available (`double None`).

| case key | comparison | high-precision left / right score | offset | claimed width |
|---|---|---|---|---|
| frechet\|archive\|001946 (m6) | root, dl/dk(k e^-w) > 0 > dl/dk(k e^w) | 1.25e-15 / 3.48e-16 | -1.57e-15 | log shape 4 eps (2 eps half) |
| frechet\|archive\|001949 (m6) | same | 1.25e-15 / 3.48e-16 | -1.57e-15 | same |
| frechet\|archive\|001955 (m6) | same | see records | -3.39e-15 | same |
| frechet\|archive\|010365 (m6) | same | ... / -4.85e-16 | 1.23e-15 | same |
| gamma\|archive\|001983 (m0) | root, G(k(1-w)) > 0 > G(k(1+w)) | -1.84e-41 / -1.66e-39 | 5.68e-16 | relative 4 eps (2 eps half), shape 3.4e23 |
| gamma\|archive\|001986 (m0) | same | -1.84e-41 / -1.66e-39 | 5.68e-16 | same |
| gamma\|archive\|001994 (m0) | same | ... / 3.12e-28 | -8.67e-16 | same |
| invweibull\|archive\|004257 (m2) | root | -2.58e-15 / -6.36e-15 | 2.10e-15 | log shape 4 eps |
| invweibull\|archive\|004259 (m2) | root | ... / -1.98e-14 | 2.51e-15 | same |
| invweibull\|archive\|004260 (m2) | root | ... / -1.81e-14 | 2.22e-15 | same |
| llogis\|archive\|004562, 004565 (m2) | root | 1.51e-14 / 5.63e-15 | -1.95e-15 | same |
| llogis\|archive\|004563, 004566 (m2) | root | ... / 2.10e-15 | -1.28e-15 | same |
| llogis\|archive\|004574 (m2) | root | ... / -1.05e-14 | 1.09e-15 | same |
| llogis\|archive\|010575 (m2) | root | ... / 2.04e-15 | -1.42e-15 | same |
| llogis\|archive\|010576 (m2) | root | 1.19e-14 / 5.09e-15 | -2.21e-15 | same |
| naka\|archive\|005059 (m2) | root, G = log k - psi(k) - s | -1.01e-16 / -5.75e-16 | 7.92e-16 | relative 4 eps (Square) / log 2e-13 |
| weibull\|class\|huge (m0) | root, F(s(1-w)) < 0 < F(s(1+w)) | -1.53e-14 / -1.09e-14 | -3.03e-14 | relative 1e-14 in 1/shape (half 5e-15) |
| weibull\|class\|tiny (m0) | root | -1.14e-14 / -7.02e-15 | -2.14e-14 | same |

Complete high-precision records (all intervals) are in `records.jsonl.gz`. Reading: every offset is at the 1 to 15 ulp
level (at most 3.4e-15 in log shape for archive cases, 3e-14 for the weibull huge/tiny class data), about five orders of
magnitude below the 1e-10 accuracy contract. They are failures of the stated width, not of the accuracy contract: the
claimed brackets of width 4 eps cannot hold at the scale of the double evaluation of the score, whose rounding moves the
double root by (conditioning times eps) around the mathematical root. In the weibull huge/tiny class cases the data are
converted by `y = -log x` in binary64 before the Gumbel fit (absolute error eps times 700 for huge x), which is a
represented-data effect not covered by "relative 1e-14 in 1/shape". These are reported, not loosened: a width claim
that names its rounding (`4 eps + c eps`, or a data-representation term for the log transform) would remove them; the
decision belongs to the family owners and to the design of a rounding margin.

## Negative controls

`2026-11-14-replay-roots-data/negative-controls.py`, output `negative-controls.out`: 19 controls, all as expected.
Mechanism C: gumbel (free, mu held), weibull, kumar, hcauchy, lgser, yulesimon, beta (two), gamma, genbeta, gev and
pois+zero=truncated, coefficient scaled by 1+1e-6 (1+1e-3 where the claimed width is wider) or shifted: each fails its
`root` or `moment` comparison. Mechanism B: lnorm, logitnorm, invgamma, llogis, invweibull with a perturbed coefficient
fail `restore`. gev: the snapshot contains no certified record with held xi in (-1, 0), so the control is synthetic
(xi = -0.3, sigma = 3 range, mu located in 60 digits): the root holds at mu*, and fails at mu* + 1e-6 sigma.

## Open entries (not judged)

- gev, sigma and xi held with xi outside (-1, 0): golden-section search with no bracket claim, 18 cases (36 with the
  double mirror, xi = 0.308 in the first). Not replayed. With xi in (-1, 0) the snapshot has no certified case, so the
  `gev` replay was exercised only by the synthetic control.
- logis, free or held location, `root`: 6 cases (class `huge`, `subnormal`, `subnormal-signed`, `tiny`, archive 004685
  and 004686): high-precision Newton fails with a numerically singular matrix at the returned coefficients.
- pareto3 `restore`, archive 005612: OverflowError in the 50-digit evaluation.
- dweibull: 3 cases with counts above 20000 (hp cost) skipped.
- zeta+truncate=zipf: N above 4000 skipped (direct sum cost) where it occurs; N free, or s held: no window root.
- Not replayed because the case has no root or restore comparison (counts in `run.log`, both modes): hcauchy 58, yulesimon 19, lgser 18, pois+zero modified/truncated 47 (modified also 7 all-zero), lnorm and logitnorm with the mean held 50 and 44 (no restoration check), frechet 33 and genbeta 48 (cover, not a scalar root), zeta+truncate=zipf 72 (N free or s held), gev 139 (other hold subsets).

## Files changed

- `dev/validation/cert_replay_roots.py` (new), `dev/validation/cert-replay.py` (registration, `rebuild()` for routes).
- `dev/research/evaluations/2026-11-14-replay-roots-data/`: `records.jsonl.gz` (all comparisons, both modes),
  `run.log`, `negative-controls.py`, `negative-controls.out`.
- This report. Nothing committed.

## Rerun on the post-5b snapshots (2026-11-15)

Master 9141de55 on the snapshots of a1166cbf (after step 5b, from the combined validation): 3326 cases judged in both
modes, the same 20 root failures per mode and the same open entries (logis 6, pareto3 1 per mode). The 40 failures are
in `dev/validation/strict-known.json` under TASK-214.31 (finding `root-width-rounding`).
