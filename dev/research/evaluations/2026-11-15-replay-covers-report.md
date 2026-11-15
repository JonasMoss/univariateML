# Certificate replay: Lomax transfers and one-dimensional covers (2026-11-15)

Job `replay-covers` (TASK-214.34), step 8d: mechanisms E and F of the
[design](../../design/certificate-replay.md), item 3 of the
[inventory report](2026-11-14-cert-replay-report.md). Code:
[`dev/validation/cert_replay_covers.py`](../../validation/cert_replay_covers.py), registered in
[`cert-replay.py`](../../validation/cert-replay.py). Code read at master b80a1831; replayed on the baselines
[`2026-11-15-fit-snapshot-data`](2026-11-15-fit-snapshot-data/) (native and double, ordinary and route snapshots).
Data: [`2026-11-15-replay-covers-data/`](2026-11-15-replay-covers-data/).

## Summary

- Judged: pareto2, invpareto (Lomax transfers), and the one-dimensional covers of cauchy (scale held), gpd (joint and
  scale held), foldnorm (joint), fatigue (shift held; scale free or alpha held) and skellam (both rates free). 648
  case-mode pairs with 1312 comparisons (324 cases and 656 comparisons per mode).
- Results: 1256 hold, 10 fail, 4 open, 42 information (the pareto2 limit mapping, 21 per mode). **All 10 failures are one finding**:
  fatigue with alpha held above 2 on five near-constant samples, a certified unique estimate whose reciprocal-scale
  twin has a likelihood higher by $10^{-24}$ to $10^{-26}$ (below).
- Not done within the hour-per-family bound: betapr and burr unit-shape subsets, ht, slash, sgomp, dgamma, paralogis,
  invparalogis, trgamma, invtrgamma and huber (reasons below), and the held subsets of the families that were done.
- No change to the driver beyond the registration. No bug found in the driver or in the Lomax prototype; one gap in the
  prototype (the shape coefficient of the all-free Lomax fit is not checked) is covered here for the transfers.

## Method and evidence level

Each certified outcome is replayed from the returned coefficients and the data, at 50 digits, without reading anything
from the core beyond the snapshot record. The dossier bound is coded in interval form (mpmath `iv`, outward rounded) or,
for the cover, a generic interval branch and bound is used that needs only an interval value and an interval derivative
enclosure:

- **bracket**: the derivative of the profile has opposite signs at the ends of the claimed enclosure of the returned
  coefficient (width and source line in the table below, plus a stated rounding allowance);
- **cover**: the profile value lies below the target everywhere on the compact domain outside a window around the
  returned point, and the analytic tails lie below it. A cell $[a,b]$ is closed when an upper bound of the value is below
  the target, when the derivative enclosure has one sign and the maximizing end is below the target, or when the two
  tangent lines from the ends (slopes $\max f'$ and $\min f'$) meet below it. A point above the target is a counterexample
  (`fails`). The target is the profile value at the returned point (or that value plus the certificate's tolerance where
  the code's claim has one: foldnorm, skellam);
- **curvature**, **gain**, **coefficient**, **tie** and **support** as named per family below.

The window is justified by the bracket plus a strict-concavity check (cauchy, gpd) or, where the claim is a likelihood
tolerance (foldnorm, skellam), not needed. `cover` proves that the global maximizer lies in the window; `bracket` and
`curvature` place the stationary point in the enclosure and make it the only one there. Uniqueness of the maximizer in
the whole domain is not separately replayed.

Evidence level per comparison is in the `rigor` field of the records. Counts per mode (identical in native and double):

| family | cases | comparisons | interval | exact | numerical |
|---|---|---|---|---|---|
| pareto2 | 66 | 175 | 172 | 3 | 0 |
| invpareto | 65 | 97 | 95 | 2 | 0 |
| cauchy (scale held) | 16 | 49 | 49 | 0 | 0 |
| gpd (joint, scale held) | 51 | 113 | 113 | 0 | 0 |
| foldnorm (joint) | 42 | 82 | 82 | 0 | 0 |
| fatigue | 56 | 112 | 112 | 0 | 0 |
| skellam (joint) | 28 | 28 | 0 | 0 | 28 |

`exact` entries are the constant-sample exponential limits and the pareto2 sample with a value at `min`. The skellam cover is
`numerical`: mpmath has no interval Bessel function, so the profile values are 50-digit evaluations and the curvature
bound is the dossier's. All other covers are enclosures, so a `holds` there is a proof for that input conditional on
mpmath's interval library and this code (design note, evidence levels). The records contain more comparisons for cauchy
and foldnorm (the `multiplicity`, `unique`, `location`, `quartic`, `support` entries of the faces module, concatenated by
`combined()`); the counts above are this module's.

## Claims and their source lines

Written to `claims.json` from the `CLAIMS` table of the module; a change to one of these certificates must change the module in
the same commit.

| family, subset | claim replayed | code and dossier |
|---|---|---|
| pareto2, shape and scale free | Lomax certificate on $y=x-\min$, shape $=\kappa$, scale $=1/\lambda$: root enclosure $\hat z\mp8\times10^{-7}$, gain above $10^{-8}$, global cover (as `lomax`) | `lomax/certificate.hpp:8,100-101,118-119`; `pareto2/mle.hpp:94-95` |
| pareto2, shape held | held-$\kappa$ root, half width $0.75\times10^{-10}$ in $\log\lambda$ | `lomax/mle.hpp:273-288,299-305`; `pareto2/mle.hpp:94-95` |
| pareto2, value at min | unbounded when both coefficients are free, or all observations equal min with one free | `pareto2/mle.hpp:23-27` |
| invpareto, free; shape held | as lomax on $y=1/x$, shape $=\kappa$, scale $=\lambda$ | `invpareto/mle.hpp:32-33,42` |
| both, shape coefficient | $\log\hat\kappa+\log A(z)$ within $10^{-8}$ over the root enclosure ($\kappa=1/A$) | `lomax/certificate.hpp:118-119` |
| cauchy, scale held | winning bracket $\le10^{-10}$ (scale units), signed ends, strict concavity on the box padded by $10^{-7}\max(1,\lvert\text{end}\rvert)$; returned point within $10^{-10}$ of the bracket; other candidates lower by a margin; compact domain $[\min y,\max y]$, monotone outside | `cauchy/mle.hpp:306-308,326,345,380-386,399`; README held-scale certificate |
| gpd, joint | root window $3\times10^{-7}$ in $t=\log(1+b)$ or in the coefficients; cells closed with bound below best minus $10^{-8}$; winner above the uniform boundary by $10^{-8}$; negative tail at $t=-32$ or the $A=1$ crossing; positive tail by score sign; $\xi=-1$ when every interior cell is below 0; scale within $10^{-8}$ of $D$ | `gpd/certificate.hpp:7,114-116,156-167,185-189,228,261-263,271-275` |
| gpd, scale held | same with $h=-(1/c+b)D$, $c=\sigma/M<1$; tails from the score sign | `gpd/certificate.hpp:176-181,185-189` |
| foldnorm, joint | grid incumbent certified to $\mathrm{tol}\,n$, $\mathrm{tol}=10^{-10}$; interior root bracket $0.25\,\mathrm{tol}$ relative in $\mu$ and $\sigma$ | `foldnorm/mle.hpp:152,193,206,236-246,294-295` |
| fatigue, scale free | every root of the profile score on $[H,A]$ isolated; winner bracketed to relative $0.01\,\mathrm{tol}=10^{-12}$ | `fatigue/certificate.hpp:40,62-72,88-90,111` |
| fatigue, alpha held | same on $[t_{\min}/r,\,t_{\max}r]$, $r=(\alpha/2+\sqrt{\alpha^2/4-1})^2$ for $\alpha>2$ | `fatigue/certificate.hpp:41` |
| skellam, joint | returned likelihood within $10^{-8}n$ of the global maximum (profile on the mean line, quadratic envelope with $M=\sum2/(\lvert k\rvert+1)$, Poisson-mode tail, series bound on $[0,\varepsilon]$) | skellam README, joint-search certificate (the margin and bounds are not in one code location) |

Two reformulations that make the claims replayable, both my derivations and checked by the interval evaluation, not
taken from the dossiers:

- foldnorm: with $a=\mu/\sigma^2$ the stationarity in $\sigma$ gives $\sigma^2+a^2\sigma^4=\overline{x^2}$ in closed form
  (the README R notes state it), so the joint profile is the one-dimensional
  $\ell(a)/n=-\tfrac12\log s-\overline{x^2}/(2s)-a^2s/2+\overline{\log\cosh(ax)}+\mathrm{const}$ with
  $\ell'(a)/n=-as+\overline{x\tanh(ax)}$ (envelope theorem). It decreases for $a\ge\bar x/(\overline{x^2}-\bar x^2)$, because
  $\overline{x\tanh(ax)}\le\bar x$ and $as$ increases to $\sqrt{\overline{x^2}}$ and equals $\bar x$ there. The cover runs on
  $[0,\bar x/(\overline{x^2}-\bar x^2)]$ with exact rescaling by $\max x$.
- skellam: any maximizer in the quadrant lies on the line $\mu_1-\mu_2=\bar x$ (interior stationary points satisfy it, and so
  does the optimum on a zero-rate face), so the global maximum is the one-dimensional profile in $t=\min(\mu_1,\mu_2)$.

## Per-family verdicts

Counts are per mode; native and double agree on every verdict.

| family | comparisons judged | holds | fails | open |
|---|---|---|---|---|
| pareto2 | root 44, concave 22, gain 22, global 22, coefficient 22, face 21, support 1 (mapping 21 information) | 154 | 0 | 0 |
| invpareto | root 30, concave 8, gain 8, global 8, coefficient 8, face 35 | 97 | 0 | 0 |
| cauchy, scale held | bracket 16, curvature 16, cover 16, tie 1 | 49 | 0 | 0 |
| gpd | bracket 20, curvature 20, gain 11, coefficient 11, cover 51 | 113 | 0 | 0 |
| foldnorm | bracket 40, cover 42 | 82 | 0 | 0 |
| fatigue | bracket 56, cover 56 | 107 | 5 | 0 |
| skellam | cover 28 | 26 | 0 | 2 |

### pareto2 and invpareto

- The Lomax replay transfers without change after $y=x-\min$ (binary64, as the core subtracts) or $y=1/x$. Cases: pareto2 with
  min held (shape and scale free, shape held), invpareto (free, shape held). Scale held is the closed form (counted, not judged).
- **pareto2 `limit(true)` mapping.** All 21 stored pareto2 limit cases are exponential stops that the interval cover proves
  have $h<0$ at every finite $z$, i.e. there is no finite maximizer and the exponential supremum is unattained (19 by the
  cover, 2 constant samples exactly). For these inputs `limit(true)` states a proved fact. The 33 invpareto stops
  per mode (35 with two constants) are of the same kind, and so are the lomax stops of the prototype, which are mapped to
  `limit(false)`. No stored input has a finite maximizer with gain below the margin, which is the case where the
  two mappings would differ in truth; the question whether `limit(true)` should also be returned for the margin-decided
  case remains a rule question for a card. Recorded as `mapping` (information) next to the `face` comparison.
- The pareto2 sample with a value at `min` is an exact predicate (`support`).
- The prototype checks the root enclosure in $\lambda$ but not the shape coefficient. The `coefficient` comparison checks
  $\log\hat\kappa=-\log A(z)$ over the root enclosure (with the code's $10^{-8}$) and holds in all 30 transfer cases; a
  negative control shows it flips for a perturbed shape.

### cauchy, scale held

Location search on $L(t)=-\sum\log(1+(y_i-t)^2)$. Bracket half width $2\times10^{-10}$ plus $8\epsilon(1+\max\lvert y\rvert+\lvert\hat t\rvert)$
(the bracket is $\le10^{-10}$ and the returned point is within $10^{-10}$ of it); concavity on $\hat t\pm(10^{-7}\max(1,\lvert\hat t\rvert)+w)$;
cover with the dossier's termwise score, curvature and value bounds in interval form; the monotone tails are the compact
domain. The balanced two-point tie (`unique` false, $n=2$): the mirror point $x_1+x_2-\hat\mu$ carries the same likelihood
(to $10^{-30}$), and both windows are excluded from the cover.

### gpd, joint and scale held

The profile $h(t)$ with $D$, $P$, $R$ in interval form (the 40-term series for $\lvert bv\rvert<1/4$ with a rigorous remainder,
the closed form otherwise). The window is the hull of $\hat t\mp3\times10^{-7}$ and the $t$ at which the shape coefficient equals
$\hat\xi\mp3\times10^{-7}\max(1,\lvert\hat\xi\rvert)$ (the code's two kinds of enclosure; the latter solved numerically, then judged by interval signs); the cover excludes five times the window
after the interval curvature bound shows $h_{tt}<0$ there. For $\hat\xi=-1$ (the uniform boundary: 31 of the 42 joint cases per mode) the target is 0, with the $A=1$ crossing as the left end. Gain over the
uniform boundary $>10^{-8}$ (joint, 11 cases) and the scale coefficient (joint, 11 cases) hold. Not replayed: the $10^{-8}$
separation between retained maxima (no stored case retains two), $\xi$ held (monotone scale root, mechanism C), and
held scale at or above the maximum excess (the uniform member, an exact predicate; 20 case-modes).

### foldnorm, joint

The one-dimensional profile above. Bracket: $\ell'(\hat a\mp w)$ with $w=10^{-10}(\max(\hat\mu,\hat\sigma)\max x/\hat\sigma^2+2\hat a)$ in the rescaled coordinate
(the code's $0.25\,\mathrm{tol}$ relative widths in $\mu$ and $\sigma$, widened). Cover target $\ell(\hat a)+\mathrm{tol}$ per observation. Estimates with $\hat\mu=0$ (the half-normal
boundary; two cases) get the cover only (their face inequality is in the faces module).
The ill-conditioned near-constant samples (profile maximum at $a\approx10^{15}$ to $10^{31}$ in original units) hold too.

### fatigue

Profile and conditional log likelihood in $b=\beta$ on $[H,A]$ (scale free) or $[t_{\min}/r,t_{\max}r]$ (alpha held),
derivative in closed form. Bracket relative half width $1.1\times10^{-12}$ (the code's $0.01\,\mathrm{tol}=10^{-12}$ plus rounding). The
cover excludes the log window $10^{-9}$ around $\hat\beta$ (the code's own separation from "the same root").

### skellam, joint

Cover over $t\in[10^{-10},T]$ with the quadratic envelope $\ell(m)+\lvert\ell'(m)\rvert h+Mh^2/2$ from the dossier, the tail bound $-\tfrac n2\log(2\pi\lfloor T\rfloor)$ for
$T\ge2$, and $\ell(\varepsilon)+2n\varepsilon$ on $[0,\varepsilon]$, target $\hat\ell+10^{-8}n$. Not replayed: ties with the zero-rate faces (within $0.5\times10^{-8}n$),
the held-rate concave solve (mechanism C), constant samples (closed form).

## Failures

**fatigue, alpha held above 2, near-constant samples: 10 `fails` (5 inputs, both modes). Real finding, not a replay bug.**
Cases `archive` 001458, 001459, 001461, 001462 and 009817 (mask 5, shift 0, alpha held at 2.858368126583106, $n=5$ or $12$,
spread of the shifted sample $5\times10^{-12}$ to $3.5\times10^{-6}$). The cover finds a point outside the window of the returned
scale at which the conditional log likelihood is higher than at $\hat\beta$, by interval arithmetic:
$f(5.43094493075)>f(\hat\beta)$ for 001458, and the same at the reciprocal end for the others. The point is the second local maximum,
the reciprocal twin $\beta_2\approx\bar t^2/\hat\beta$ (for 001458: $\hat\beta=0.150673466336$, $\beta_2=5.43094493075$). Its
likelihood is higher than the returned one by (numerical local refinement) $1.5\times10^{-24}$ (001458, 001461), $1.7\times10^{-26}$ (001459,
001462) and $4.9\times10^{-26}$ (009817) in total log-likelihood units, against the code's tie rule of $10^{-10}n$ with
$\lvert\log(\beta_2/\hat\beta)\rvert>10^{-9}$ (`fatigue/certificate.hpp:111`), which is meant to stop the fit with a comparison tie.
Both roots lie inside the domain ($2.2\times10^{-8}$ and $2.8\times10^{-8}$ relative from its ends for 001458), the replay finds the bracket at
$\hat\beta$ valid, and the outcome is `estimate`, certified, `unique` true. So the condition of the certificate (no other
separated maximum within the tie margin) is false for these inputs and the claimed unique estimate is unsupported: the maximum is
a near-exact tie between two separated maxima, the same configuration the code treats as a constant sample ("two reciprocal
scales are equal global maxima", README Details) when the sample is exactly constant. What is not determined here: why the
certificate did not list both roots (a hypothesis, not tested: the second root is not isolated by the segment tests);
the replay only shows that the claim is false. The inexact tie means the returned fit is within $10^{-24}$ of the global maximum in
likelihood, so the damage is the flag (certified unique), not the value. Needs a card for fatigue; the entries are
`cert-replay:cover|{native,double}|fatigue|archive|{001458,001459,001461,001462,009817}`. Not added to `strict-known.json`.

No other comparison fails.

## Development notes (replay bugs found and fixed before the final run)

These are not findings; they are listed so a reader can see what the controls and the cross-checks caught.

- foldnorm: my first version scaled the data by $\max x$ in binary64, which destroyed samples whose spread is $10^{-15}$ of their
  level (spurious `fails` on 9 near-constant inputs); the scaling is now exact in interval arithmetic. The derivative of the
  profile is evaluated at the two ends of a cell by monotonicity ($as$ and $\overline{x\tanh}$ both increase in $a$), which removed 13
  `open` entries caused by dependency loss.
- fatigue: a first "separation" comparison (target below the maximum by $10^{-10}n$) flagged points on the same hill; it was
  dropped. The strict `cover` is the comparison.
- skellam: orders above 5000 or rates above $10^5$ are reported `open` instead of running mpmath's series (it overflowed on the
  `large` class).

## Negative controls

[`negative-controls.py`](2026-11-15-replay-covers-data/negative-controls.py), output
[`negative-controls.out`](2026-11-15-replay-covers-data/negative-controls.out): 22 controls change the returned coefficients or the
data of a stored record so that the replayed inequality becomes false; every control ends in `fails` for the named comparison
(22 of 22, exit status 0). pareto2 scale and shape perturbed ($10^{-4}$, held scale $10^{-6}$), pareto2 value at min claimed finite, an exponential limit claimed on a
heavy-tailed sample; the same for invpareto; cauchy location moved by $10^{-8}$ and 0.5, and the weaker mode of a two-mode sample
claimed; gpd shape and scale moved by $10^{-5}$, an interior sample claimed uniform, a uniform sample claimed interior, held-scale shape
moved; foldnorm location moved by $10^{-6}$, an interior sample claimed $\mu=0$, a half-normal sample claimed $\mu=3\sigma$; fatigue scale
moved by $10^{-6}$ and the lower of two local maxima claimed; skellam both rates moved by 0.05.

## Open entries

Four `open` (skellam, 2 inputs, both modes): `archive/006309` and `class/large`, with count orders above 5000 or rates above $10^5$ (the
mpmath Bessel series is not run). Nothing else is open.

## Families not done, and what each needs

| family | what is missing |
|---|---|
| betapr, burr (unit shapes) | the Lomax and inverse-Pareto maps for the held-unit-shape subsets (data transform and coefficient mapping per held pattern), then the transfer as for pareto2; the joint covers are two-dimensional (trace). About 1 to 2 h per family, mostly reading the held-pattern branches |
| ht | interval score enclosures in $\log\nu$ need digamma intervals (mpmath has none): a numerical cover with the dossier's normal-face tail bound; about 2 h |
| slash | held scale: $h(z)=\log((1-e^{-z^2/2})/z^2)$ derivative by the Bernoulli series for small $z$, Lipschitz $n\sqrt2/s$; free scale adds the inner scale root and the half-mass collapse bound; 2 to 3 h |
| sgomp | joint cover over the rate with the conditional shape optimum solved inside (nested root, endpoint shape bounds): either a trace or a nested interval root; 3 h or more |
| dgamma | free-shape branch and bound with incomplete gamma (no interval version): numerical, with the dossier's tail bounds; 2 h |
| paralogis, invparalogis | shape profile with the Burr scale solved by a monotone root inside each evaluation (nested), analytic tail exclusion and the compact-domain exclusion; 3 h |
| trgamma, invtrgamma | profile with convex-conjugate secant bounds, the refitted-normal seam and power tails; digamma numerical; more than 3 h |
| huber | the $\epsilon$ profile needs, at each endpoint, a supporting-plane nuisance bound from an inner concave solve; the normal and Laplace faces are margin decisions; 3 h or more |
| gpd, $\xi$ held | monotone scale root (mechanism C), 20 case-modes: cheap, not done |
| foldnorm, held location or scale; fatigue, $\beta$ held; skellam, one rate held; cauchy, held scale margins | the held subsets (monotone roots, closed forms) and the likelihood margins between candidates (gpd $10^{-8}$, cauchy `ll_margin`, fatigue $10^{-10}n$ separation, skellam face ties $0.5\times10^{-8}n$) are not replayed |

## Command and cost

From `dev/validation`, one slot, empty known list (the tracked list is untouched):

    echo '{"known": {}}' > /tmp/covers-none.json
    D=../research/evaluations/2026-11-15-fit-snapshot-data
    OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 /tmp/umvl-cpu/run nice -n 10 python3 cert-replay.py \
      $D/native.jsonl.gz,$D/routes-native.jsonl.gz $D/double.jsonl.gz,$D/routes-double.jsonl.gz \
      --known /tmp/covers-none.json --family pareto2,invpareto,cauchy,gpd,foldnorm,fatigue,skellam \
      --records ../research/evaluations/2026-11-15-replay-covers-data/records.jsonl

Cold cost, both modes, 50 digits: gpd 644 CPU s (541 s of it the golden sample, $n=500$, native; the double golden is a cache hit with the same key), the
other six 40 CPU s together. With the cache (`CERT_REPLAY_COVERS_CACHE`, default `/tmp/cert-replay-covers-cache.jsonl.gz`, one entry per
family, mask, held values, returned coefficients, outcome, data and budget, plus a per-family version) a rerun takes 23 CPU s. The gpd cover would be much cheaper with
the monotone derivative enclosure used for foldnorm; not done.
`run.log` has the run's output; `records.jsonl.gz` one line per case and mode; `summary.out` the counts (from `summarize.py`).

## Files changed

- `dev/validation/cert_replay_covers.py` (new)
- `dev/validation/cert-replay.py`: imports the module, binds `replay_lomax` and `LomaxProfile` into it, and extends the existing
  registration loop to the covers' REPLAYS; nothing else
- `dev/research/evaluations/2026-11-15-replay-covers-report.md` (this file)
- `dev/research/evaluations/2026-11-15-replay-covers-data/`: `records.jsonl.gz`, `run.log`, `summary.out`, `summarize.py`, `claims.json`,
  `negative-controls.py`, `negative-controls.out`

No core header, binding, dossier, `strict-known.json`, Justfile or board entry changed.
