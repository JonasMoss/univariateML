# Certificate replay: slash, paralogis, invparalogis (2026-11-15)

Job `replay-covers-3` (TASK-214.34, step 8d, batch 3): mechanisms E and F of the
[design](../../design/certificate-replay.md). Continues the [batch 1 report](2026-11-15-replay-covers-report.md) and the
[batch 2 report](2026-11-15-replay-covers-2-report.md). Code:
[`dev/validation/cert_replay_covers.py`](../../validation/cert_replay_covers.py), registered in
[`cert-replay.py`](../../validation/cert-replay.py) (unchanged: the module's `REPLAYS` are merged by family name). Code read at master 868cb42b;
replayed on the baselines [`2026-11-15-fit-snapshot-data`](2026-11-15-fit-snapshot-data/) (native and double, ordinary and route snapshots).
Data: [`2026-11-15-replay-covers-3-data/`](2026-11-15-replay-covers-3-data/).

## Summary

- Judged: slash (mu held, sigma held, both free), paralogis (shape held, scale held, both free) and invparalogis (the same three, as paralogis on $1/x$).
  442 case-mode pairs with 1038 comparisons (221 cases and 519 comparisons per mode; 136, 161 and 170 of the 519 are the new slash, paralogis and invparalogis
  comparisons, the rest of the slash count is the faces module's exact `multiplicity` entries that the same cases carry).
- Results: **all judged comparisons hold. No `fails`, no `open`.**
- paralogis and invparalogis: the 11 and 8 estimates that the core returns **uncertified** (flagged) all pass the global cover at 50 digits (per mode). The replay
  certifies at the interval level what the core's compact-domain certificate left open. This is a statement about the stored inputs, not a change of any outcome.
- slash: the joint (location, scale) accuracy is checked by a full Newton iteration at 50 digits (numerical): every returned point is within $10^{-14}$ of the stationary point.
- Batch 1 and 2 families were not rerun (the module changes below leave their replays untouched; `VERSIONS` of the older families are unchanged).
- Four replay bugs were found and fixed during development (below); none was a finding about the core.

## Method and evidence level

Replay from the returned coefficients and the data at 50 digits, as in batches 1 and 2: dossier bounds coded in interval arithmetic (mpmath `iv`, outward rounded)
and the generic interval branch and bound `cover`. New in this batch: the cover variable is eliminated nested, and the inner problem is solved by a verified root.

| family, subset | cases per mode | comparisons per mode | interval | exact | numerical |
|---|---|---|---|---|---|
| slash, mu held (mask 1) | 16 | scale-bracket 16 | 16 | 0 | 0 |
| slash, sigma held (mask 2) | 15 | bracket 15, cover 15 | 30 | 0 | 0 |
| slash, both free (mask 0) | 29 | bracket 29, accuracy 29, collapse 3, cover 29 | 61 | 0 | 29 |
| paralogis, shape held | 19 | scale-root 19 | 19 | 0 | 0 |
| paralogis, scale held | 19 | bracket 19, cover 19 | 38 | 0 | 0 |
| paralogis, both free | 34 (+2 constant samples) | bracket 34, scale-root 34, cover 34, constant 2 | 102 | 2 | 0 |
| invparalogis, shape held | 22 | scale-root 22 | 22 | 0 | 0 |
| invparalogis, scale held | 22 | bracket 22, cover 22 | 44 | 0 | 0 |
| invparalogis, both free | 34 (+2) | bracket 34, scale-root 34, cover 34, constant 2 | 102 | 2 | 0 |

- **slash scale root.** The score $\sum g(t_i)$, $g(t)=1-2t/(e^t-1)$ is evaluated at $\hat\sigma(1\mp10^{-10})$ in interval arithmetic ($g$ is increasing, so intervals in $t$ are enclosed from the end values).
- **slash location cover.** A cell is closed by the interval cover. The derivative enclosure is the score at the cell midpoint plus or minus $A r$ (sigma held) or $K r$ (free scale), with $r$ the half width. $A=\sum_i H_i/s^2$, $H_i=\max(1,\min(z_+^2/4,\,2/z_-^2+1/(e^{z_-^2/2}-1)))$ is the dossier's curvature bound. For free scale the profile
  $P(m)=\max_s \ell(m,s)$ is enclosed pointwise by sign-checked root brackets (the log likelihood is $n$-Lipschitz in $\log s$), $P'=\ell_m$ at the root (envelope theorem),
  $|P''|\le A+C^2/B$ on a cell from the uniform scale bounds $s_-$ (near-distance score) and $s_+$ (far-distance score), and a cell without a positive uniform lower scale bound uses the
  residual likelihood bound (half-mass supremum $-\tfrac n2\log 2\pi-(n-k)\log2-2\sum\log d_i$ at $2k=n$).
- **slash accuracy.** `accuracy` is `numerical`: a Newton iteration from the returned coefficients (gradient from the interval scores, Hessian by central differences, both at 50 digits)
  converges to a stationary point with negative definite Hessian, and the distance travelled, $|\Delta\mu|/\max(|\mu|,\sigma)$ and $|\Delta\log\sigma|$, is compared with $10^{-10}$. It is a stationarity and accuracy check, not an enclosure; the location half of the same question is the interval `bracket`.
- **paralogis.** In the coordinates $(a,b')$ with the logs centred at the returned log scale, $L(a,b')=2n\log a+\sum z_i-(a+1)\sum\log(1+e^{z_i})$, $z_i=ay_i+b'$. Shape held: $L_{b'}$ is strictly decreasing in the intercept and signed at scale $\hat\theta(1\mp10^{-10})$. Scale held ($b'=0$): $L(a)$ and $aL_a$ are enclosed on cells in $\log a$ by interval arithmetic with an interval $a$.
  Both free: the profile $P(a)=\max_{b'}L$; $L_{b'}$ is strictly decreasing, so the root over a cell is enclosed by interval sign checks (a Newton approximation at the cell midpoint, then widened until the signs are certain), $P'=L_a$ at the root, and the value at a point is $L(a,b_{\rm mid})$ plus the concavity error $\max|L_{b'}|\,w$. The tails are the dossier's ($L\le n\log a$ small end; $L\le n\log(4^{3/5}a)-\tfrac35a\Delta$, $a\ge1$, large end). The cover runs on $\log a$ between the tail cutoffs, outside a window of relative half width $10^{-6}$ around the returned shape, against the value at the returned coefficients.
- **invparalogis.** paralogis on $y=1/x$ with the returned shape, scale $1/\hat\theta$. A certified fit is replayed on the binary64 reciprocals the core fits and certifies (`invparalogis/mle.hpp:55-64`); an uncertified fit is polished by the core on the original $x$ (`:70-82`) and is replayed on the exact reciprocals (interval $1/x$).
- **constant.** Exact predicate: a constant positive sample with both coefficients free has no finite maximum.

`holds` at the interval level is a proof of the replayed inequality for the stored data (conditional on the correctness of the mpmath interval functions). `holds` at the numerical level is evidence.

## Claims and their source lines

Written to [`claims.json`](2026-11-15-replay-covers-3-data/claims.json) from the `CLAIMS` table of the module.

| family, subset | claim replayed | code and dossier |
|---|---|---|
| slash, mu held | scale score signed at $\hat\sigma(1\mp10^{-10})$ (unique root, $2k<n$) | `slash/mle.hpp:59-63,299-300`; README "Details", "Conditional search certificate" (inner scale) |
| slash, sigma held | global location cover to $10^{-9}(1+\lvert\ell\rvert)$ over $[\min x,\max x]$; location score signed at $\hat\mu\mp10^{-10}\max(\lvert\hat\mu\rvert,\sigma)$ | `slash/mle.hpp:103-146` (margin :125,135), :73-97; README "Conditional search certificate" |
| slash, both free | profile cover to $10^{-9}(1+\lvert\ell\rvert)$ (capped $10^{-9}n$ with a half-mass value); `collapse`: $B_v<\hat\ell-10^{-8}n$; profile score signed; stationary-point accuracy $10^{-10}$ | `slash/mle.hpp:188-272` (margins :211-213, :264-266), :314-322, :406, :150-186; README "Conditional search certificate" |
| paralogis, shape held | $L_b$ signed at $\hat\theta(1\mp10^{-10})$ (concave in $b$: unique maximizer) | `paralogis/mle.hpp:380-382`, `burr/mle.hpp:343-353,414,419`; README "Details" |
| paralogis, scale held | global shape cover on $\log a$ between the tail cutoffs, window around the returned shape; shape score signed at $\hat a(1\mp10^{-10})$ | `paralogis/mle.hpp:340-356`; `certificate.hpp`; README "Global certificate" |
| paralogis, both free | global profile cover as above; profile score and scale score signed at $10^{-10}$ | `paralogis/mle.hpp:318-327`; `certificate.hpp`; README "Global certificate" |
| paralogis | constant sample with both free: unbounded | `paralogis/mle.hpp:313-318`; README existence |
| invparalogis | transfer to paralogis on $1/x$ | `invparalogis/mle.hpp:55-64,69,70-82,99-102,126-127` |

The core's `certificate.hpp` fixed-point box (unique root in a box of radius $10^{-8}a$, contraction factor below 0.9) is **not** replayed: the replay checks the signs of the profile score at $\pm10^{-10}$ instead, and the global cover.

Reformulations that are mine, not the dossiers':

- slash, free scale: the cross-derivative bound. The dossier bounds $\lvert\ell_{m,\log s}\rvert\le\sum(\sqrt2+z_+H_i)/s_-$; with an observation far from the cell ($z_+\sim5\times10^5$ for a sample containing $10^6$) that makes $K=A+C^2/B$ about $10^{11}$ and the cover exhausts its budget. I use $\sqrt2+\sup\lvert zh''(z)\rvert$ with $\sup\lvert zh''\rvert\le5$ for $z<2.6$ (from $-1\le h''\le z^2/4$) and $\le u(z_-)=2/z_-+z_-/(e^{z_-^2/2}-1)$ for $z_-\ge2.6$. The second part rests on $h''\ge0$ for $z\ge2.6$: $h''=1/t-2te^t/(e^t-1)^2+1/(e^t-1)\ge0$ once $2\sinh(t/2)\ge\sqrt2\,t$, which holds for $t\ge3.4$ ($\sinh y\ge\sqrt2y$ for $y\ge1.7$ and the derivative $\cosh y\ge\sqrt2$ beyond). A spot check ($z=2,3,4,6,10,30$) agrees with the dossier's bounds $-1\le h''\le\min(z^2/4,2/z^2+1/(e^{z^2/2}-1))$ and with $g'(t)\ge e^{-2t}$.
- slash: $P''\in[-A,A+C^2/B]$ from $\ell_{mm}\in[-A,A]$ and $P''=\ell_{mm}+\ell_{m,\log s}^2/(-\ell_{\log s\log s})$, used as a two-sided derivative enclosure $P'(c)\pm Kr$.
- paralogis: centring. The profile in $a$ is unchanged by the shift $b'=b+ac$ (a bijection of $b$ at each $a$); the envelope theorem holds in $b'$. The held-scale large-end bound $L\le2n\log a-da$ for $a\ge1$ (the dossier states it) was re-derived for the stated range; $L\le2n\log a$ at the small end.
- The cover window (relative $10^{-6}$ around the returned shape) and the target (value at the returned coefficients, no margin) follow the choices of batches 1 and 2.

## Per-family verdicts

| family | judged per mode | holds | fails | open |
|---|---|---|---|---|
| slash, mu held | 16 | 16 | 0 | 0 |
| slash, sigma held | 15 (bracket + cover) | 30 | 0 | 0 |
| slash, both free | 29 (bracket, accuracy, cover; collapse for 3) | 90 | 0 | 0 |
| paralogis | 72 estimates + 2 constant samples | 161 | 0 | 0 |
| invparalogis | 78 estimates + 2 constant samples | 170 | 0 | 0 |

- **slash.** 60 estimates per mode (16 + 15 + 29). Cover evaluations: 2 to 560 per case (median 204). The three `collapse` cases (`archive/006355`, `class/two`, `class/half-ties`) have a half-mass value: the supremum $B_v$ is below $\hat\ell-10^{-8}n$ in each (by 0.5, 0.5 and 0.02 in total log likelihood). The stored
  nonexistence outcomes (7 `unbounded` per mode) and the failure outcomes carry no certificate and are not judged here (`multiplicity` of the faces module judges the multiplicity predicate). **No stored slash input is a half-mass collapse decision (`CollapseLimit`)**, so the margin-analysis counterexample $(0,0,-1,1.0001)$ is not exercised.
- **paralogis, invparalogis.** The cover takes 4 to 940 evaluations. Eleven (paralogis) and eight (invparalogis) estimates per mode are flagged uncertified by the core; for these the replay is an independent check and all hold. They are the near-constant and extreme-scale samples (shape $5.9\times10^3$ to $5.9\times10^7$, scale $10^{-200}$ to $10^{307}$). Cases with $\hat a>5\times10^3$ (5.9e3, 7.9e5, 5.9e7, 7.1e7) have a flat profile (the largest value seen outside the window is about $5\times10^{-12}$ below the target) and the cover still closes.
- **Constant samples** (2 per mode and family): both free, nonexistence `unbounded` outcomes agree with the exact predicate.

## Failures

None.

## Development notes (replay bugs found and fixed before the final run)

Not findings.

- invparalogis `archive/010533` (uncertified, $\hat a=7.1\times10^7$): the first replay used the binary64 reciprocals $1/x$ and reported `bracket` fails (the profile score is positive on both sides; the stationary shape is $1.9\times10^{-9}$ relative above the returned one). The data spread is $10^{-8}$ and the rounding of $1/x_i$ moves the log spread by $10^{-8}$ relative. The core polishes an uncertified fit on the original $x$. With exact reciprocals for an uncertified fit the bracket and the cover hold. A certified fit stays on the binary64 reciprocals.
- paralogis cover: the first version used raw logs. Samples at scale $10^{\pm150}$ have $\lvert y\rvert\sim345$ and the interval dependency in $a\,y+b$ made 22 of 24 covers per mode exhaust the budget (`open`); centring at the returned log scale removes it (the budget is now `budget`$/2$ for the profile; 656 evaluations was the most needed).
- slash both free: a `scale-bracket` at the returned $\mu$ failed on five samples with $\sigma\ll\lvert\mu\rvert$ ($\sigma$ down to $2^{-43}$). That claim was mis-specified: the scale root at the returned $\mu$ is not $\hat\sigma$ when $\mu$ is wrong by $10^{-10}\lvert\mu\rvert\gg\sigma$ (the contract measures $\mu$ relative to $\max(\lvert\mu\rvert,\sigma)$). It was replaced by the `accuracy` Newton iteration. A one-step version of that check with a coarse difference step also reported spurious steps of $10^{-10}$ to $3\times10^{-10}$ on two samples; the full iteration with a difference step relative to the local scale gives $10^{-15}$.
- slash free scale: the dossier's cross-derivative bound made one sample (an observation at $10^6$) exhaust 4000 evaluations; the sharper termwise bound above closes it in 308.
- slash cover speed: roots by safeguarded Newton in $\log s$ (140 bisections before) reduced a 2-observation case from 18 to 3 s.

## Negative controls

[`negative-controls.py`](2026-11-15-replay-covers-3-data/negative-controls.py), output [`negative-controls.out`](2026-11-15-replay-covers-3-data/negative-controls.out): 15 controls, each ends in `fails`
for the named comparison (15 of 15, exit status 0): paralogis shape held with the scale moved by $10^{-6}$, scale held with the shape moved by $10^{-6}$, both free with the shape and with the scale moved by $10^{-6}$ and an estimate claimed for a constant sample;
the README example $x=e^{-0.001}$ with scale 1 and the local mode 2.895 claimed (`cover` fails against the mode near 9262); a cover-primitive control with the target at $1.5\hat a$ (counterexample at $a=2.0046$);
invparalogis with the scale and the shape moved by $10^{-6}$; slash with the scale moved by $10^{-6}$ (mu held), the location moved by $10^{-6}\max(\lvert\mu\rvert,\sigma)$ (sigma held and both free), sigma moved by $10^{-6}$ (both free, `accuracy`),
the local mode $-99.96$ of $(-100,100,100.5)$ with sigma 1 claimed (`cover` fails), and the lower mode of ten zeros, $-1$ and nine 10s (dossier example, found by solving the stationarity equations) claimed (`cover` fails). One control was first written with the wrong mode: the stationary point near $(4,3.1)$ is the global maximizer and the cover correctly held; the control now uses the lower mode.
There is no control for a joint paralogis cover failure through the full replay function (no stored or searched sample has two modes of the profile; five candidate samples had one); the cover machinery itself is exercised by the cover-primitive control.

## Open entries

None.

## Families left

| family | what is missing |
|---|---|
| slash | no stored input exercises the half-mass collapse decision (`CollapseLimit`, ties within $10^{-8}n$, the counterexample $(0,0,-1,1.0001)$); the failure and budget outcomes carry no claim; the two-sided cell bounds are replayed at the stored accuracy only |
| paralogis, invparalogis | the fixed-point box of `certificate.hpp` (unique root in a box of radius $10^{-8}a$, contraction below 0.9) is replaced by the profile-score signs; the exact-likelihood case of scale held with all $x=\theta$ is not replayed; the evaluation-only subsets (shape and scale held) have no claim |
| betapr, burr other holds, dgamma mask 0, ht ties and limits, sgomp, trgamma, invtrgamma, huber, batch 1 held subsets | as in the batch 2 table |

## Command and cost

From `dev/validation`, one slot, empty known list:

    echo '{"known": {}}' > /tmp/covers3-none.json
    D=../research/evaluations/2026-11-15-fit-snapshot-data
    export CERT_REPLAY_COVERS_CACHE=$PWD/../../build/cert-replay/covers-cache.jsonl.gz
    OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 /tmp/umvl-cpu/run nice -n 10 python3 cert-replay.py \
      $D/native.jsonl.gz,$D/routes-native.jsonl.gz $D/double.jsonl.gz,$D/routes-double.jsonl.gz \
      --known /tmp/covers3-none.json --family slash,paralogis,invparalogis --records <data dir>/records.jsonl

Cold cost, both modes: 1633 CPU s for the final run (slash 1265 s of replay time, of which the golden sample $n=500$ 463 s per mode; paralogis 204 s; invparalogis 164 s) plus about 100 s of negative controls. Development runs added an unmetered few thousand CPU s.
The cache is `build/cert-replay/covers-cache.jsonl.gz` of the worktree (git-ignored); the three families are new in `VERSIONS` (entry 1).

## Files changed

- `dev/validation/cert_replay_covers.py`: slash, paralogis and invparalogis replays, `VERSIONS` entries, module docstring
- `dev/research/evaluations/2026-11-15-replay-covers-3-report.md` (this file)
- `dev/research/evaluations/2026-11-15-replay-covers-3-data/`: `records.jsonl.gz`, `run.log`, `summary.out`, `summarize.py`, `claims.json`, `negative-controls.py`, `negative-controls.out`

`cert-replay.py` needed no change. No core header, binding, dossier, `strict-known.json`, Justfile or board entry changed.
