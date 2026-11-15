# Certificate replay: gpd shape held, burr power held, dgamma, ht (2026-11-15)

Job `replay-covers-2` (TASK-214.34, step 8d, batch 2): mechanisms C, E and F of the
[design](../../design/certificate-replay.md). Continues the [batch 1 report](2026-11-15-replay-covers-report.md). Code:
[`dev/validation/cert_replay_covers.py`](../../validation/cert_replay_covers.py), registered in
[`cert-replay.py`](../../validation/cert-replay.py). Code read at master f04d944d; replayed on the baselines
[`2026-11-15-fit-snapshot-data`](2026-11-15-fit-snapshot-data/) (native and double, ordinary and route snapshots).
Data: [`2026-11-15-replay-covers-2-data/`](2026-11-15-replay-covers-2-data/).

## Summary

- Judged: gpd with xi held (mask 5), burr with the power (shape2) held (mask 2, a Lomax transfer), dgamma with rate held (shape free,
  a cover) and shape held (a root), ht with shape free (endpoint held or free, scale held or free: 29 or 30 root and 32 cover cases per mode).
  372 cases, 743 comparisons (199 to 200 per mode for the new subsets, plus the faces module's `support` entries that the same cases carry).
- Results: all judged comparisons hold except 3 `open` entries. **No `fails`.**
- betapr was **not judged: the stored inputs contain no unit-shape hold** (shape1 or shape2 held at 1; the held values are 2.014 and 3.26), so the Lomax and
  inverse-Pareto transfers have nothing to replay. This is a finding about the stored set, not a replay failure.
- Batch 1 families rerun: verdicts unchanged (10 fatigue fails, 4 skellam open; gpd gains 20 `root` holds per mode from the xi-held subset).
- No change to the driver beyond one binding line (`LOMAX_FACE_CLASS`).

## Method and evidence level

The method of batch 1 applies: replay from the returned coefficients and the data at 50 digits; `cover` is the generic interval branch and
bound of the module (cell closed by an upper bound, a one-signed derivative, or two tangent lines; a point above the target is `fails`).
New in this batch: two families where the evidence is **numerical**, not interval, because the needed functions have no interval version in mpmath.

| family, subset | cases per mode | comparisons per mode | interval | exact | numerical |
|---|---|---|---|---|---|
| gpd, xi held (mask 5) | 20 | root 20 | 20 | 0 | 0 |
| burr, power held (mask 2) | 24 (10 estimates, 14 limits) | estimate: root, concave, gain, global, coefficient (50); limit: face, limit (28) | 78 | 0 | 0 |
| dgamma, shape held (mask 1) | 14 | bracket 14 | 0 | 0 | 14 |
| dgamma, rate held (mask 2) | 13 to 14 | bracket 13 to 14, cover 13 to 14 | 0 | 0 | 27 to 28 |
| ht, shape free (masks 0, 1, 4, 5) | 32 | bracket 29, cover 32 | 0 | 0 | 61 |

- gpd xi held: the score is a rational function of the data, evaluated in interval arithmetic.
- burr: interval, by the Lomax replay on $y=(x/\max x)^c$; $y$ is rounded to binary64 (a relative $1.1\times10^{-16}$ input perturbation, much below the
  $8\times10^{-7}$ root enclosure; the proof is for the rounded $y$). The exact constant-sample limit is `exact`.
- dgamma: mpmath incomplete gamma at 50 digits (`numerical`). The shape score is a central difference of the log cell probabilities (error about $10^{-24}$,
  widened by $10^{-15}$); the digamma monotonicity and the conditional-mean monotonicity are the dossier's.
- ht: mpmath `loggamma`, `digamma` and a bisection for the scale root at 50 digits (`numerical`). The cell score enclosures use the dossier's inequalities, coded in
  mp arithmetic, not interval arithmetic.

`holds` at the numerical level is evidence, not proof.

## Claims and their source lines

Written to [`claims.json`](2026-11-15-replay-covers-2-data/claims.json) from the `CLAIMS` table of the module.

| family, subset | claim replayed | code and dossier |
|---|---|---|
| gpd, xi held | scaled scale score $(1+\xi)\,\overline{r/(\sigma+\xi r)}-1$, strictly decreasing, signed at $\hat\sigma(1\mp1.1\times10^{-8})$ (bracket bisected to $\min(\mathrm{tol},10^{-8})$; signed enclosure certifies $10^{-7}$) | `gpd/mle.hpp:92,98-110,124`; README "Certificate" last paragraph |
| burr, power held | Lomax certificate on $y=(x/\max x)^c$, shape $=k$, rate $=(\max x/s)^c$; Weibull limit only if the strict exponential face proves $h<0$ at every finite $z$ | `burr/mle.hpp:375-410` (limit at :391), `burr/lomax_face.hpp:14-52`, `lomax/certificate.hpp:100-101,118-119` |
| dgamma, shape held | log-rate score signed on $\hat r(1\mp10^{-10})$; strict log-concavity in $\log r$ makes the root the unique maximizer | `dgamma/mle.hpp:76-111,419`; README "Existence and identification" |
| dgamma, rate held | global shape cover to $10^{-8}$ per observation ($5\times10^{-9}$ search plus $5\times10^{-9}$ recomputation), dossier cutoffs for small and large shape | `dgamma/mle.hpp:233-281` (:275), :401, :419; README "Search certificate" |
| ht, shape free | within $10^{-8}n$ of the supremum: cover in $\log\nu$ with scale-root enclosure, score enclosure, small and large shape bounds, $\nu\ge1$ when the endpoint is free | `ht/cert.hpp:72,17-66,129-148,152-162,172-218`; README "Explicit tail and inner-solve bounds", "A computable interval score enclosure" |

Reformulations that are mine, not the dossiers':

- dgamma rate held: $f_a/n=\log r-\psi(a)+\Lambda(a)$ with $\Lambda$ the weighted conditional mean of $\log Y$, nondecreasing in $a$ at fixed $r$ (the dossier states this for
  $H_a$); the derivative enclosure on $[A,B]$ is $[f_a(A)+\psi(A)-\psi(B),\,f_a(B)+\psi(B)-\psi(A)]$ and in $\log a$ it is multiplied by $a\in[A,B]$.
- burr: $\text{jac}=n\log c+(c-1)\sum\log x-cn\log\max x$ moves the Lomax log likelihood to the Burr one (as the code does, `burr/mle.hpp:380`).

## Per-family verdicts

| family | judged per mode | holds | fails | open |
|---|---|---|---|---|
| gpd, xi held | root 20 | 20 | 0 | 0 |
| burr, power held | root 10, concave 10, gain 10, global 10, coefficient 10, face 14, limit 14 | 78 | 0 | 0 |
| dgamma | bracket 27, cover 13 to 14 | 40 to 41 | 0 | 1 (native only) |
| ht | bracket 29, cover 32 | 60 | 0 | 1 |

- **gpd xi held.** All 20 stored cases have $\xi=0.3316$ and $\mu=0$. The bracket half width $1.1\times10^{-8}$ is the bisection width of the nominated representative
  at the weakest tolerance; the sign check holds.
- **burr.** The 14 Weibull limits hold under the strict face: the face cover finds $h<0$ everywhere (supremum class `none`), which is what
  `fallback_lomax_face_proved` requires. The `face` comparison (margin $10^{-8}$) is the Lomax prototype's and holds too. The returned Weibull scale of the limit is not replayed.
- **dgamma.** Shape held: bracket holds for all 14 cases (including a rate of $2\times10^{-6}$). Rate held: bracket and cover hold for all but one; the cover
  targets $\hat f+10^{-8}$ and the largest value seen outside is within $10^{-8}$ of the target, i.e. the replay is not vacuous. Shape free *and* rate free (mask 0, 14 estimates per mode) is **not replayed** (below).
- **ht.** Judged on masks 0, 1, 4, 5. Three estimates sit at the restriction $\nu=1$ (endpoint free): bracket skipped, cover only. Four inputs (archive 003177 to 003180, $\hat\nu=14.69$) needed a cover
  budget of 20000 evaluations (about 200 CPU s each); with 2000 they were `open`. Not replayed: free endpoint with free scale and $2k=n$ (the scale-zero face; none stored among the estimates),
  and the limit outcomes (the normal face and scale-zero face comparisons).

## Failures

None. The batch 1 failures (10 fatigue covers) are unchanged and are a finding of batch 1.

## Development notes (replay bugs found and fixed before the final run)

Not findings.

- dgamma: mpmath `gammainc` raised `NoConvergence` for shape $\sim10^6$ (and failed slowly); a Kummer-series lower tail now serves shapes above $10^5$, and a difference of lower tails
  below $10^{-40}$ is reported as an error and becomes `open`.
- ht: the first cover budget (2000 evaluations) left four inputs open; the budget for this family is now five times the driver budget.

## Negative controls

[`negative-controls.py`](2026-11-15-replay-covers-2-data/negative-controls.py), output [`negative-controls.out`](2026-11-15-replay-covers-2-data/negative-controls.out): 12 controls, each ends in `fails`
for the named comparison (12 of 12, exit status 0): gpd xi held with scale moved by $10^{-6}$ and by a factor 1.5; burr power held with scale moved by $10^{-4}$, shape moved by $10^{-4}$, and a Weibull limit claimed for a heavy-tailed sample;
dgamma with rate moved by $10^{-6}$ (shape held), shape moved by $10^{-6}$ (bracket) and by a factor 1.3 (cover); ht with $\nu$ moved by $10^{-6}$ (endpoint and scale held, and endpoint held with scale free), $\nu\times1.5$ (cover), and
a half-normal sample claimed with $\nu=2$.

## Open entries

Three `open`, with reasons:

- `cover|native|dgamma|archive|001025`: shape $9.7\times10^5$ with rate 0.97. Tail differences of the Kummer series lose the cell probabilities at 50 digits; needs a relative-accuracy upper tail (continued fraction) or higher precision. The double mode of this input is not an estimate (`conditioning`).
- `cover|{native,double}|ht|class|wide`: the large-shape cutoff of the dossier bound is $\nu=e^{142}$, beyond the evaluated range ($e^{90}$). Needs the sharper $s_0$ from the $c$ largest residuals (`cert.hpp:152-162`) or an analytic tail beyond the range.

## Families left

| family | what is missing |
|---|---|
| betapr | no stored input with a unit shape hold, so the Lomax and inverse-Pareto transfers have nothing to judge. Needs unit-hold cases in `fit-snapshot.py` (shape1 = 1, shape2 = 1; masks 1, 2, 3), then the transfer as for pareto2 ($y=x$ with kappa $=q$, rate $1/b$; or $y=1/x$ with kappa $=p$, rate $b$) |
| burr, other holds | shape1 held (mask 1, $k$ held: monotone roots), both shapes held (mask 3), scale held (mask 4, `fixed_scale.hpp`), joint (two-dimensional) |
| dgamma, mask 0 (both free) | nested: profile over shape with the rate root inside; the dossier encloses the score by the rate bracket $[A/(\bar k+1),B/\bar k]$ and the secant bounds on $H_a$. 14 estimates per mode. Needs a 2-D cell enclosure (trace-like) and probably more than 2 h |
| ht, tie and limits | the $2k=n$ scale-zero face, normal-face and scale-zero-face limits; the $\nu$-held subsets (scale root, monotone) and the joint subsets with ties |
| slash, sgomp, paralogis, invparalogis, trgamma, invtrgamma, huber | as in the batch 1 table |
| batch 1 held subsets | foldnorm held location or scale; fatigue $\beta$ held; skellam one rate held; the likelihood margins between candidates |

## Command and cost

From `dev/validation`, one slot, empty known list:

    echo '{"known": {}}' > /tmp/covers2-none.json
    D=../research/evaluations/2026-11-15-fit-snapshot-data
    OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 /tmp/umvl-cpu/run nice -n 10 python3 cert-replay.py \
      $D/native.jsonl.gz,$D/routes-native.jsonl.gz $D/double.jsonl.gz,$D/routes-double.jsonl.gz \
      --known /tmp/covers2-none.json --family gpd,burr,dgamma,ht --records <data dir>/records.jsonl

Cold cost, both modes: 4100 CPU s for this run (the four ht inputs near $\nu=14.7$ about 200 CPU s each per mode; the rest of ht a few minutes; dgamma, burr and gpd under 10 s),
plus about 350 CPU s of development runs. The batch 1 rerun (cached) took 1 CPU s. `batch1-rerun.log` has its output. Cache: `/tmp/cert-replay-covers-cache.jsonl.gz`,
versions bumped for burr (new), dgamma (3) and ht (2); gpd's xi-held replay is a separate cache key.

## Files changed

- `dev/validation/cert_replay_covers.py`: gpd xi held, burr power held, dgamma, ht; `LOMAX_FACE_CLASS` binding
- `dev/validation/cert-replay.py`: one line binding `lomax_face_class` into the module
- `dev/research/evaluations/2026-11-15-replay-covers-2-report.md` (this file)
- `dev/research/evaluations/2026-11-15-replay-covers-2-data/`: `records.jsonl.gz`, `run.log`, `batch1-rerun.log`, `summary.out`, `summarize.py`, `claims.json`, `negative-controls.py`, `negative-controls.out`

No core header, binding, dossier, `strict-known.json`, Justfile or board entry changed.
