# Certificate replay: inventory, design and prototype (2026-11-14)

Job `cert-replay` (TASK-214.26), the first part of consolidation step 8
("replay certificate comparisons in high precision on the archive and add the
result to the strict layer; pilot outward rounding on one or two families").
Code read at master `7f12c88d`; replayed on the stored baselines of master
`7e3c9779` ([snapshot report](2026-11-14-fit-snapshot-report.md)).

Deliverables:

- design note [`dev/design/certificate-replay.md`](../../design/certificate-replay.md):
  replay routes (re-derivation or trace), the trace hook, strict-layer wiring,
  cost, the outward-rounding pilot, what is proved;
- prototype [`dev/validation/cert-replay.py`](../../validation/cert-replay.py)
  for nbinom and lomax;
- evidence in [`2026-11-14-cert-replay-data/`](2026-11-14-cert-replay-data/):
  the replay records of both modes, the run log and a negative-control script.

## Summary

- **Inventory.** The 85 fitted families decide `certified`, the outcome kind
  or `unique` from computed comparisons of eight kinds (mechanisms A to H
  below). 14 are closed forms whose only deciding comparisons are exact
  predicates on the data; 8 add a represented-likelihood check
  (`restored()`) to a theorem-backed root; 18 rest on a uniqueness theorem and
  locate the root with score signs, some with an exact face inequality; 45
  decide a face or a global claim with likelihood margins, covers or integer
  searches, most of them partial certificates with several mechanisms by hold
  subset.
- **Prototype.** nbinom (exact face decision and padded root bracket) and lomax
  (margin-decided exponential face and a one-dimensional interval cover) are
  replayed by re-derivation, in interval and exact rational arithmetic, on
  both stored baselines: 220 judged cases, 314 comparisons, **no
  disagreement and nothing open**; eight negative controls (perturbed claims)
  all fail as they should. Two findings: every non-constant lomax exponential
  stop in the baselines (27 per mode), reported as margin-decided
  (`certified = false`), has an interval proof that no finite maximizer exists
  (its left-tail step is this job's argument, marked as a sketch);
  and the nbinom free-size fits on several class and resample cases are up to
  $2\times10^{-9}$ relative from the maximizer, outside the accuracy contract
  but inside the certificate's own $5\times10^{-7}$ enclosure (the same
  phenomenon as the known TASK-214.5 entries, which fit-refine sees only on
  the archive).
- **Findings beyond the prototype.** Some outcome mappings disagree with the
  rule that a limit decided only by the likelihood margin is uncertified:
  pareto2 maps the lomax exponential stop to `limit(true)` while lomax and
  invpareto map the same comparison to `limit(false)`; huber's normal and
  Laplace faces and the zib zero-inflated-Poisson face are selected by a
  $10^{-8}n$ margin and map to `limit(true)`; slash's collapse at half
  multiplicity is a nonexistence selected by a $10^{-8}n$ margin. Two
  `limit(true)` decisions rest on unpadded binary64 comparisons of moments
  (binom underdispersion, gompertz overdispersion on raw moments); they are
  cheap to replay exactly and are the obvious first job.

## Inventory

### Mechanisms

| | mechanism | deciding comparison | replay route |
|---|---|---|---|
| A | closed form | exact predicates on the data (constant sample, zero sum, median interval, range) | none needed (exact in binary64) |
| B | represented likelihood | `restored()` in `core/transformed_data.hpp`: ideal supremum minus the likelihood at the rounded public coefficients $\le \mathrm{tol}\max(1,\lvert\ell\rvert)+128\epsilon(1+\text{magnitudes})$ | re-derive: high-precision supremum and likelihood at the returned coefficients |
| C | scalar root under a uniqueness theorem | signs of a monotone score at the ends of the final bracket, padded or unpadded; the global claim follows from the theorem | re-derive: signs at the returned coefficient times $1\mp w$ |
| D | inequality that selects a face | a moment or multiplicity inequality on the data | re-derive exactly (rational arithmetic) |
| E | face against interior with a margin | interior value minus face supremum against $10^{-8}n$, $10^{-9}n$, $2\times10^{-8}n$ or `compare_face` (8 ulps plus margin) | re-derive both values; that the interior value is the interior supremum needs F |
| F | global cover | cells closed by chord, Lipschitz or score-sign bounds; analytic tails; root isolation by signed ends and a curvature bound | 1D: independent interval cover; 2D and up: trace, or an independent cover where affordable |
| G | exhaustive integer search | profile values over an integer range, a tail bound against the incumbent, tie margins | re-derive: profile at each integer, tail bound exactly or in intervals |
| H | ties and non-uniqueness | two candidates' values equal (exact symmetry) or within a margin | re-derive both values |

Core helpers that decide comparisons for many families: `core/face_compare.hpp`
(`compare_face`: $8\epsilon$ allowances per operand plus a family margin,
`NumericalTie` inside), `core/stop_detail.hpp` (`compare_lower`: lower bounds
within 8 ulps are equal; `better_candidate` prefers an accurate family point
on equality; `comparison_outcome` and `nonunique_outcome` turn ties into
uncertified or non-unique outcomes), `core/multistart.hpp` (score gate: total
score norm below $10^{-3}$ for an accepted failed solve), `core/truncation.hpp`
and `core/zero_modified.hpp` (concave exponential-family windows and
conditional-mean bisections for the modifier routes),
`core/transformed_data.hpp` and `core/transformed_location.hpp` (`restored`),
`record_accuracy` (accuracy stops; the accuracy contract, not the certificate).

### Families

One row per fitted family that certifies anything; modifier routes are listed
with their base family. "Snapshot" counts the native baseline's outcomes (E
certified estimate, e uncertified, L certified limit, l uncertified limit, N
nonexistence, `*` non-unique). Margins are the deciding ones; $\epsilon=2^{-52}$.
Route: R re-derive, T trace (or an independent high-precision cover), X exact.
Where a row says "dossier", the mechanism is taken from the dossier's `fit`
note and the margins from the code, without tracing every branch.

#### Closed forms and represented-likelihood flags (A, B)

| family | snapshot | deciding comparisons | margins | route |
|---|---|---|---|---|
| exp, power, pareto, rayleigh, norm, hnorm, invexp, invgauss, unif, dunif | E, N | closed forms; nonexistence from exact predicates (zero sum, zero gap, constant sample, `q == 0`) | none | none |
| norm truncate route (tnorm) | E218 L28 l1 N3 | concave natural-parameter Newton on a fixed window; uniform and exponential-type faces when the profile derivative at $t_2=0$, $n(v-v_{\mathrm{face}})$, does not favour the interior; concave acceptance bound; interior and face within $10^{-9}(n+\lvert\ell\rvert)$ become a comparison tie | $10^{-9}(n+\lvert\ell\rvert)$ | D: X; R |
| laplace | E71 E\*21 N3 | median and mean absolute deviation; `unique` from the median interval (exact order statistics) | none | none |
| dlaplace | E63 E\*3 L11 | closed form; `not_unique` and point mass exact | none | none |
| geom (+ zero routes) | E | closed forms; zero-modified route: conditional-mean bisection (`zero_detail`, unpadded, NaN unless width $<10^{-12}$) | none | C: R |
| pois (+ routes) | E, L3 | closed form; zip: Lambert W root iff zero fraction exceeds $e^{-\bar x}$ (D), held-rate peaks padded $4096\epsilon(1+n+\cdot)$; tpois: concave window, Lower/Upper limits by the sign of the mean equation; ztpois, zmpois: monotone conditional means | 4096ε | D: X; C: R |
| lnorm, logitnorm | E117 e2; E105 e1 | free mean: `restored(ideal, ll, 1e-10, …)` (B) | $10^{-10}\max(1,\lvert\ell\rvert)+128\epsilon(1+\lvert\ell\rvert+\lvert\ell^\ast\rvert)$ | B: R |
| invgamma, lgamma, naka | E111 e2; E94 e2; E108 e1 | gamma digamma root (C) and `restored` on the free coefficient (B) | as above with tol | B, C: R (digamma numerical) |
| llogis, invweibull, pareto3 | E105 e1; E99 e2; E92 e2 | logistic or Gumbel profile root (C), `restored` on the free scale (B); pareto3 delegates to llogis on $x-\min$ | as above | B, C: R |

#### Uniqueness theorem with a root bracket (C), with face inequalities (D)

| family | snapshot | deciding comparisons | margins | route |
|---|---|---|---|---|
| beta, nsbeta | E105 N2; E95 | strict concavity; `decreasing_root` stops when computed signs bracket the root within tol in logit μ and log t; held shapes: bisection on decreasing digamma scores | unpadded signs | R (digamma: numerical) |
| gamma | E114 N2 | unique digamma root (Newton, bracketed fallback); held: monotone scores | unpadded | R numerical |
| gumbel, weibull | E102 N3; E113 N2 | unique scale-score root, bisection to a relative width of order tol ($0.01\,\mathrm{tol}$ on the held path); location analytic | unpadded `score > 0` | R interval |
| logis, dnorm | E92 N3; E62 L15 | strict concavity, Newton; conditional scores bisected; dnorm point mass and two-point limits exact (D) | unpadded | R (accuracy only) |
| dweibull | E59 L14 | strict concavity in transformed coordinates, bracketed profile scores; point-mass, Bernoulli and two-point limits from exact range predicates; `ShapeUnidentified` non-unique (exact) | unpadded | R, X |
| kumar | E85 | profile concavity; padded signs at both ends, enclosure $0.01\,\mathrm{tol}$ in $a$ and in $b$; held $b$ the same; held $a$ closed form | $256\epsilon(1+aT+\lvert(b-1)D\rvert)(1+n\epsilon)$ | R interval |
| lgser (+ zero route) | E46 L1 | increasing $q(u)$, bracketed bisection; point mass when every count is one (exact) | unpadded | R, X |
| zeta (+ zipf route), yulesimon | E30 L5; E27 L8 | strictly decreasing score, bisection; point mass for all-one data (exact); zipf window: mean equation, Lower/Upper limits, unidentified flag | zipf $128\epsilon(\lvert e\rvert+\lvert o\rvert)$ | R (zeta: numerical), X |
| hcauchy | E119 N3 | decreasing log-scale score; root iff the zero multiplicity is below its threshold (exact) | unpadded | R, X |
| gompertz | E85 L4 N3 | **D: `sum_xsq/n >= 2*mean^2` in binary64 raw moments selects the exponential limit, `limit(true)`, unpadded**; free $b$: decreasing shape score (Newton, hybrid stop); held $a$: concave; held $b$: analytic | none on the face decision | X (first job), R |
| nbinom (+ routes) | E63 L21; routes E, L | **prototype.** D: population variance $\le$ mean, exact integer comparison when products fit $2^{52}$, else centred sum with a pad; C: profile-score bracket of relative width $5\times10^{-7}$, padded; held prob the same; held size closed form. ztnbinom: interval bounds and tail inequalities, Poisson limit; zmnbinom; zinb: global certificate, zip and Poisson limits `limit(true)` | dispersion pad $256\epsilon(1+v+\bar x)(1+n\epsilon)$; score $128\epsilon(1+\sum\lvert\text{terms}\rvert)$ plus Euler–Maclaurin remainder; routes $256$–$4096\epsilon$ | X, R interval |
| binom (+ routes) | E80 L4; routes E, e, L | **D: `x_bar / var_pop <= 1.0` in binary64 (centred sum) selects the Poisson limit, `limit(true)`, unpadded**; free size: unimodal profile (DeRiggi) and an integer tail exclusion (G); held prob: tail exclusion. Routes: zib (zip face $1\times10^{-9}n+4096\epsilon(1+\lvert\ell\rvert)$; **`zip.logLik >= best - 1e-8 n` selects the zip limit, `limit(true)`**), ztbinom ($10^{-9}n$, Poisson limit), tbinom (allowance $10^{-9}n$, face $10^{-9}n+8192\epsilon(1+\lvert f\rvert)$, point-mass and tpois limits), zmbinom | $10^{-9}n$, $10^{-8}n$ | X (first job), R |
| pig | E41 L32 | held mean, binary counts: exact FMA comparison of $n\bar x$ with twice the number of ones (D); free: strict compact-coordinate concavity, padded signs enclose the root or select the Poisson face; zero limit | $256\epsilon(1+\lvert lo\rvert+\lvert hi\rvert+\max x)$ | X, R |
| cmp | E56 L14 | moment proposals accepted by concave tangent bounds and score half-space coefficient enclosures (`cmb/acceptance.hpp`); geometric KKT face; atomic and two-point limits exact | $10^{-9}(n+\lvert\ell\rvert)$ | R (normalizer series: numerical) |

#### Faces, covers and integer searches (E, F, G, H)

| family | snapshot | deciding comparisons | margins | route |
|---|---|---|---|---|
| lomax | E68 l29 | **prototype.** All free: 1D cover of the gap $h(z)$ over a compact $z$-interval (chord bound with $h''\ge-5/4$ or the small-$q$ bound, score-sign exclusion), analytic tails, root isolation (signed ends, curvature bound, bracket below $7\times10^{-7}$); the finite fit wins only if $h-\eta>10^{-8}$, else exponential `limit(false)`; final Newton polish from the certified root. κ held: decreasing log-rate score, bracket $\le0.5\,\mathrm{tol}$ in $\log\lambda$, restoration within $0.25\,\mathrm{tol}$. λ held: closed form | $10^{-8}$ per observation; $\eta=256\epsilon(1+\lvert\log x_{\max}\rvert+\dots)(1+n\epsilon)$, signs $4\eta$; held κ $8\epsilon(\dots)\sum\lvert\text{terms}\rvert$ | R interval |
| pareto2 | E87 L22 | lomax on $x-\min$ (min held); **`ExponentialLimit` maps to `limit(true)`, lomax to `limit(false)`** | as lomax | R (prototype transfers) |
| invpareto | E74 l34 | lomax on $1/x$; exponential limit `limit(false)` | as lomax | R (prototype transfers) |
| gpd | E100 N1 | 1D cover: derivative enclosures exclude competing stationary maxima, chord plus $K(t_1-t_0)^2/8$, analytic tails (dossier); held shape: monotone scale score; held scale: same certificate or exact uniform bound; threshold collapse limit or nonexistence; ties to `comparison_outcome` | $10^{-8}$ | R interval (1D cover) |
| ht | E132 l83 N1 | grid and golden-section proposal in $\log\nu$, then an interval cover in $\log\nu$ with digamma score enclosures, endpoint checks, normal-face tail bound; normal limit if `normal_ll >= max(face, best) - eps/4`, `limit(false)`; scale boundary (nonexistence) on a tie | $10^{-8}n$ (grid path), `eps/4` (cover) | R (1D; digamma numerical) |
| huber | E84 L58 l6 N3 | held ε: supporting-plane nuisance bounds; adaptive ε subdivision bounds the profile including both faces; `compare_face(normal, best, 1e-8 n)`, `face_numerical_tie(laplace, best, 1e-8 n)`; **normal and Laplace faces decided by the margin map to `limit(true)`** | $10^{-8}n$, $1024\epsilon(1+\lvert\ell\rvert+n)$ | R (1D ε profile), flag for the margin rule |
| slash | E76 N6 B1 | conditional scale roots unique; location cover with Lipschitz bound $n\sqrt2/s$ and an objective-gap margin; **half-multiplicity collapse is a nonexistence (unattained) when `boundary >= best - 1e-8 n`** | $10^{-9}n$, $10^{-8}n$ | R (1D cover), flag |
| skellam | E75 B2 | one held rate: concave bracketed solve; joint: 1D profile cover with a quadratic envelope and a Gaussian tail bound (certified when the tail bound is below the candidate minus $10^{-8}n$) | $0.5\times10^{-8}n$, $64\epsilon n(1+\lvert\ell\rvert)$ | R (Bessel: numerical) |
| dgamma | E56 L17 | held shape: concave rate solve; free shape: tail bounds and a branch-and-bound in shape; point-mass and two-point limits exact (range below 2) | $\max(10^{-8}(1+\lvert\ell\rvert),\dots)$ | R (1D; incomplete gamma numerical), X |
| sgomp | E93 N3 | monotone conditional roots; compact rate-profile curvature subdivision certifies the joint proposal; ties to `comparison_outcome` | $8\epsilon n$, $512\epsilon(n+\lvert\ell\rvert)$ | R (1D cover) |
| triang | E176 N3 | finite enumeration of observation modes (G) with concave endpoint solves and tangent upper bounds pruning mode intervals; no uniqueness claim | pruning $10^{-11}n(1+\lvert\ell\rvert/n)$, separation $10^{-10}n$, $64$–$256\epsilon$ | R (G) |
| foldnorm | E113 N3 | joint and held-positive-location: analytic bounds and convex-chord upper bounds; half-normal quartic $H(0)=1-\overline{x^4}/(3\overline{x^2}^2)$ sign (D); held scale: monotone location equation | $32\epsilon$ on $H(0)$ | X, R (1D) |
| fatigue | E89 e10 N2 | analytic held-β fit; complete rational-score inclusion on finite β brackets (root isolation); distinct tied maxima stop | $16\epsilon$ per interval operation; ties $10^{-10}n$ | R interval (rational score) |
| cauchy | E86 e1 N7 | free: strict geodesic convexity under the strict half-mass condition (D, exact multiplicities) and total-score norm below $10^{-3}$; balanced two-point maxima exact; held location: signed monotone bracket; held scale: exhaustive interval bounds; `AmbiguousMaximum` uncertified | interval pad $10^{-7}\max(1,\lvert a\rvert,\lvert b\rvert)$ | X, R (1D) |
| frechet | E156 e8 L18 N3 | μ held: inverse-Weibull (B, C); held λ with free μ: monotone location or profiled-scale ratio certificate; Gumbel limit `limit(true)` when $l_0>l_1+2m$ and the bracket is narrow | $m=256$–$4096\epsilon n(\dots)$ | R |
| expweibull | e2 N2 | `certified = (!free[2] || !(free[0]||free[1])) && winner_margin` (rounding noise below half the goal); power faces refute a separated local candidate; held-scale power envelopes and constant data give `limit(true)` | $256\epsilon(1+n)(1+\lvert f\rvert)$ | R |
| invtrgamma, trgamma | E115 L33; E134 l37 | exact scalar reductions; convex-conjugate secant bounds, uniform refitted-normal seam, analytic power tails, concave root neighbourhoods for the free and held-scale profiles; invtrgamma faces `limit(true)`, trgamma faces `limit(false)` | $64\epsilon n(1+\lvert\ell\rvert)$, $2\times10^{-12}(1+\lvert\ell\rvert)$ | R (1D profile; digamma numerical) |
| emg | E112 L36 N3 | location only: signed root enclosure; six subsets: global shape bounds (held-rate inverse-variance chord), validated coefficient enclosures; normal-tail certificate in exact integer polynomial arithmetic; normal and translated-exponential limits; ties | $32\epsilon$ operands, $64\epsilon$ Mills, $128\epsilon$ | X (polynomial), T |
| gev | E65 e92 N4 | held ξ = −1 closed form; held ξ = 0 and σ analytic; held σ, −1<ξ<0 concave bracket; shape only: derivative interval coverage and support-edge envelopes; joint interval cover | $4096\epsilon n(1+\lvert C\rvert)$, $64$–$256\epsilon$ | R (1D subsets), T |
| sn | E87 l76 N2 | concave inner solve with a Gaussian curvature minorant; signed held-location roots; interval and tail bounds for free location and shape (branch budget); half-normal faces `limit(false)` | width bound $5\times10^{-9}n$; $10^{-8}n$, $10^{-9}n$ | T, R |
| snorm | E66 L62 l7 N3 | all-free algebraic mode comparison; exact weighted held-skewness solves; Bernstein isolation of rational stationary equations; constrained half-normal comparisons (`compare_face`) | faces $10^{-8}n$, ties $10^{-9}(n+\lvert\ell\rvert)$, $128\epsilon$ | R (exact rational isolation possible) |
| bbinom | E117 e5 L18 B2 | fixed size: finite best against binomial face (`best > face + tie`), face winner needs every interval below the face, finite winner a profile enclosure and exclusion; runner-up ties; size 2 algebraic (D); held α, free size: size enumeration with tail exclusion against face plus margin (G) | tie $1024\epsilon(1+\lvert f\rvert+nN)$; $2\times10^{-8}n+1024\epsilon(\dots)$; size 2 $64\epsilon_{\mathrm{ext}}(l+r)$ | X, R (G), T (interval cover) |
| bnbinom | E62 e18 L36 l9 | scalar and pair covers; finite all-free superlevel and stationary cover; signed face certificate; legacy face comparison when unproved | $10^{-10}n(1+\lvert\ell/n\rvert)$, $128$–$4096\epsilon$ | T, R (faces) |
| betapr | E138 L44 l6 N2 | fixed scale: beta concavity; unit held shapes: lomax and inverse Pareto (F); conditional and joint covers against gamma and inverse-gamma faces (`limit(true)`); exponential faces `limit(false)` | $256$–$4096\epsilon(\dots)$; tail sign $10^{-10}(1+a^2\overline{x^2})$ | T, R (unit shapes via lomax) |
| bhatt | E75 e14 L76 N3 | positive held $a$: concavity; held $a=0$, held σ: normal certificates; free $a$: global width comparison; normal and uniform limits `limit(certified)` | tie $10^{-8}n$; $128$–$1024\epsilon(n+\lvert\ell\rvert)$ | T, R |
| burr, pareto4, invburr | E140 e3 L60; E125 e5 L57 l2; E147 L56 l3 | joint and held-scale: root and global enclosures (dual and chord bounds, small-$k$ exclusion, signed seam bound against the exact Weibull optimum); held power: lomax (F); pareto4 transports through the held threshold; invburr: reciprocal transport, inverse-Weibull and power faces `limit(true)`, held-first-shape via `restored` (B) | tie $10^{-8}n$; pads $64\epsilon$; 0.1 norm margin | T; R for lomax subsets |
| paralogis, invparalogis | E79 e11; E91 e8 | free shape: root enclosure, analytic tail exclusion, exhaustive compact-domain exclusion; unclosed cover keeps the search uncertified unless a feasible bound refutes it; held shape: monotone Burr scale solve | $1024\epsilon(n+\lvert\ell\rvert)$ | R (1D shape profile) |
| gb2, fpareto | E272 L75 l81; E291 L76 l68 | one free beta shape with free power and held scale: power-profile enclosure; held beta shapes: strict-concavity coefficient enclosure; held power: beta-prime transport; endpoint face for held opposite shape below one; faces via `compare_face`; five larger subsets numerical (uncertified); faces `limit(!limit_coef.empty())`; fpareto transports with min held | $8$–$256\epsilon$ | R (1D subsets), T |
| genbeta | E10 e117 L13 l9 | endpoint and power held: concavity; both beta shapes held: padded opposite signs at $1\pm\min(\cdot,10^{-10})/4$; shape1 = 1: kumar; other held shape1: uniform tail bounds and complete interval score/curvature enumeration; gamma and power faces (`compare_face`, envelopes); limits `limit(allfree_certified)` | $64$–$8192\epsilon$; $10^{-8}n$ | R (C subsets), T |
| gengamma | E2 L13 N2 | fixed-Q conditional fits; exactly symmetric $Q=0$; separated all-free parent-side winners (two side covers); strictly winning power and Pareto faces | $256$–$512\epsilon(1+n)$ plus data error | T, R |
| ged | E89 e10 e\*2 L46 N5 | free shape: profile-score enclosures, root contraction, tail comparisons; held shapes: convex or knot profiling; uniform face `limit(certified)`; Laplace median intervals and reflected knot ties non-unique (H) | tie $10^{-8}n$; `margin(·)` $\propto\epsilon$ | T, R, X (ties) |
| lst, std | E72 e14 L59 l2; E69 e20 L51 l3 | all-free, fixed-shape and shape-only global certificates; held shape at shape ≥ 1; normal faces by signed profile tails and compact covers; symmetry certificates prove non-uniqueness (H); normal and df-two limits `limit(certified)` | tie $10^{-8}n$; $256\epsilon n(1+\lvert\ell\rvert)$ | T, R, X (symmetry) |
| st | E36 e74 L18 l126 | alpha-only, location-only, symmetric scale-only, one-sign alpha/scale, constant-data faces, symmetric Student reduction; other subsets numerical with face comparisons (`compare_face`) | $10^{-8}n$ | T, R |
| sstd | E55 e45 L4 | fixed ν and ξ: exhaustive score enclosures for mean-only and sd-only; symmetric scale equality an exact unattained supremum; ξ = 1 delegates to std; other subsets uncertified | tie $10^{-8}n$; `skew_scalar_detail::margin` | R (scalar), T |
| sged | E39 e62 L41 l114 | held-shape certificates (location, scale), free skew at ν = 2 and at $0<\nu\le1$, free shape at ξ = 1 via GED, weighted shape, held moments, mode-skew; uniform and half-GED faces through comparison ties | tie $10^{-8}n$; $10^{-7}(n+\lvert\ell\rvert)$ for ν<1, $10^{-10}(n+\lvert\ell\rvert)$ | R (scalar), T |
| tlambda | E92 e76 B7 | held non-negative shapes: concavity below 1, exact uniforms at 1 and 2, support-contact reductions above 1; scalar negative-shape and shape-only fits; free shape with nuisance: uncertified profile search | $10^{-10}n$, `margin(·)` | R (scalar), X (uniforms) |
| cmb | E89 e1 L24 B5 | fixed size: score half-space coefficient enclosures, exact atomic and Bernoulli reductions; sizes excluded by entropy, integer-tail and block bounds (G); point-mass, adjacent and endpoint limits exact; face ties | faces $10^{-9}(1+\lvert f\rvert)$, sizes $10^{-8}(1+\lvert\ell\rvert)$, $1\times10^{-9}(1+n\lvert H\rvert)$ | X, R (G), T (blocks) |
| hyper | E114 E\*1 e8 N13 | one-coordinate integer fits: unimodal (G); joint designs: exhaustive with tail bounds, else best evaluated profile uncertified; Poisson face `best - poisson - margin`; ties non-unique | $10^{-9}(1+\lvert f\rvert)$ | R (G, lgamma intervals) |
| nhyper | E123 e5 N16 | certified integer grids exhaustive with a proved population tail bound (G); decreasing likelihood ratios; growing-urn face comparisons and unfinished grids uncertified | $10^{-9}(1+\lvert f\rvert)$ | R (G) |

### Mechanisms likely to change in step 5b

The four `core5b` jobs are moving each family's outcome decision into its core.
Expect changes in: the per-family `outcome()` constants (`limit(true)` or
`limit(false)` for margin-decided faces: pareto2, huber, betapr exponential
faces, gb2's `!limit_coef.empty()`, fpareto's NaN test); `comparison_tie`
handling and `comparison_outcome_of`/`nonunique_outcome_of` (sgomp, gpd, emg,
huber, frechet, fatigue, pig, cmb, hyper, nhyper, sn, sged); the equality rule
of `compare_lower` (8 ulps) and the accurate-point preference of
`better_candidate` in `core/stop_detail.hpp`; families whose `certified` was
mapped from the dossier in step 4 (constant `estimate(true)` with a margin- or
search-based path underneath: slash, skellam, dgamma, ht, huber, triang,
sgomp, emg); TASK-214.15–.18 (log likelihoods off the density, limit
payloads, invalid-data statuses). The replay should be extended per family
after its 5b merge, not before.

## Prototype

### Choice

- **nbinom**: the most common pattern among discrete families, "unique root
  or a named face": the face is chosen by an exact moment inequality and the
  root located by a padded bracket. Both are self-contained (the dossier gives
  the theorem; the certificate is one 140-line header) and the face decision
  can be replayed exactly, the bracket in interval arithmetic.
- **lomax**: the worked example of the dossier schema and the most common
  pattern among continuous profile-search families: a margin-decided face
  (`limit(false)`) and a global cover with analytic tails. The cover is
  one-dimensional, so an independent interval cover is cheap and replaces a
  trace. Its results transfer to pareto2, invpareto and the unit-shape
  subsets of betapr and burr.

### Comparisons

All re-derived from the returned coefficients, the data and the dossier
mathematics; nothing is read from the core beyond the snapshot record. Widths
and margins are read from the code at `7f12c88d`.

| family | comparison | subsets | claim checked | level |
|---|---|---|---|---|
| nbinom | `dispersion` | all free, estimate or limit | Poisson limit iff $n\sum x^2\le(\sum x)^2+n\sum x$ (population variance at most the mean; Aragon, Eberly and Eberly) | exact |
| nbinom | `root` | all free, prob held | $g(\hat r(1-w))>0>g(\hat r(1+w))$, $w=5\times10^{-7}$, for $g(r)=n^{-1}\sum_i\sum_{j<x_i}r/(r+j)-r\log(1+\bar x/r)$ (or $+r\log p$ with prob held): the certificate's bisection stops at relative width $5\times10^{-7}$ and returns a size inside the signed bracket | interval (numerical above counts of 20,000) |
| lomax | `gain` | all free, estimate | $h(\hat z)>10^{-8}$, $h(z)=z-\log A(z)-A(z)$, $\hat z=\log(\hat\lambda\bar x)$: the comparison that prefers the finite fit to the exponential limit | interval |
| lomax | `root` | all free, estimate | $h'(\hat z-W)>0>h'(\hat z+W)$, $W=8\times10^{-7}$ (signed bracket below $7\times10^{-7}$, widened by $10^{-7}$ on each side; the returned point is the final Newton polish) | interval |
| lomax | `concave` | all free, estimate | the termwise bound $B_r^2/A_l^2-C_{\min}/A_r-C_{\min}<0$ on that enclosure, so it holds exactly one stationary point | interval |
| lomax | `global` | all free, estimate | an independent interval cover of $[lo,hi]$ minus the enclosure (chord bound with the dossier's curvature floor, score-sign exclusion) and both analytic tails stay below $h(\hat z)$ | interval |
| lomax | `face` | all free, exponential limit | the same cover shows $h<10^{-8}$ everywhere: no gain above the margin, which is all a margin-decided limit claims | interval |
| lomax | `root` | κ held | $F(\log\hat\lambda-0.75\times10^{-10})>0>F(\log\hat\lambda+0.75\times10^{-10})$, $F(t)=n-(\kappa+1)\sum_i\mathrm{logistic}(t+\log x_i)$ (bracket at most $0.5\,\mathrm{tol}$, restoration within $0.25\,\mathrm{tol}$) | interval |

Not judged: nbinom with size held and lomax with λ held (closed forms), every
all-held evaluation, failures, and the all-zero nbinom sample (an estimate with
size 0). For each margin-decided lomax limit the record also classifies the
supremum (information, not a check): **none** when an interval cover shows
$h<0$ on the whole compact domain and the left tail is negative (then no
finite maximizer exists and the exponential supremum is not attained; the
argument is in the design note, marked as a sketch), **finite** when $h>0$
somewhere (a finite maximizer exists and its gain lies below the margin), or
undetermined. Where the core's own double evaluation is simple, the record
gives a Python mirror of it at the same points with the core's pad
(binary64, the same compensated sums; native builds use long double in a
few places, so the mirror matches the double-only mode best).

### Results

Run on the workstation through one slot (`/tmp/umvl-cpu/run`),
`cert-replay.py native.jsonl.gz double.jsonl.gz` on the
[2026-11-14 baselines](2026-11-14-fit-snapshot-data/); log in
[`run.log`](2026-11-14-cert-replay-data/run.log), one record per case and mode
in [`records.jsonl.gz`](2026-11-14-cert-replay-data/records.jsonl.gz). 23 CPU s
in total (lomax golden, $n=500$, 7 s per mode; every other case below 0.5 s).

| family | comparison | native | double | level |
|---|---|---|---|---|
| nbinom | `dispersion` | 35 hold | 35 hold | exact (34 on the core's exact integer path, 1 on its padded path: the `huge` class) |
| nbinom | `root` | 30 hold | 30 hold | interval (28), numerical (2: the `wide` class, counts up to $10^6$) |
| lomax | `gain` | 11 hold | 11 hold | interval |
| lomax | `root` | 30 hold | 30 hold | interval (11 all free, 19 κ held) |
| lomax | `concave` | 11 hold | 11 hold | interval |
| lomax | `global` | 11 hold | 11 hold | interval (204 to 908 profile evaluations per case) |
| lomax | `face` | 29 hold | 29 hold | interval (27), exact (2 constant samples) |

Disagreements: **none**. Open comparisons: **none**. No `strict-known.json`
entry would be needed for these two families at `7e3c9779`.

How much room the comparisons had:

- nbinom `dispersion`: the closest case to the decision boundary has
  population variance minus mean equal to $-1/25$ (archive 005106); every
  decision but one ran on the core's exact integer path, so these are exact in
  the core as well.
- nbinom `root`: at the re-derived bracket ends the high-precision score is at
  least 300 times the core's pad (all free) and far larger with prob held. The
  Python mirror of the core's padded comparison signs at every one of these
  points.
- lomax `gain`: from $0.0210$ to $14.26$ per observation, against the margin
  $10^{-8}$; `root` ends at least 149 times $4\eta$ (all free) and 40 times
  the core's pad (κ held); the returned $\hat z$ lies within
  $5\times10^{-15}$ of the high-precision root in every all-free fit.
- lomax `face`: the largest gain found anywhere on the compact domain is
  negative in every margin-decided stop (between $-5\times10^{-11}$ and
  $-2\times10^{-12}$, at the left end of the domain).

**Lomax exponential stops are proved, not only margin-decided.** For all 27
non-constant exponential stops per mode, an interval cover shows $h<0$ on the
whole compact domain, and $M_2=\overline{y^2}<2$ with $f'(q_{lo})<0$ makes the
left tail negative as well (argument in the design note, marked as a sketch).
So the supremum is the exponential limit and no finite maximizer exists, for
each of these inputs; the outcome is reported with `certified = false`
because the core's comparison is against the margin $10^{-8}$, not zero. A
certificate that compares against $0$ (with the left-tail bound
$f(q)$ instead of $-\log(1-qM_2/2)$) would certify these stops; it needs the
cover to resolve $h<0$ near the flat left end, which the small-$q$ curvature
bound already allows (the replay's covers against $0$ close within its budget of 4000
evaluations on every one of these inputs).

**nbinom sizes outside the accuracy contract (information; not a certificate
failure).** The certificate encloses the root to relative width
$5\times10^{-7}$ and returns the Newton proposal when it lies inside; the
contract's $10^{-10}$ rests on the Newton step criterion. The replay's
high-precision root (bisection on interval signs to relative width
$10^{-30}$) shows these all-free fits outside $10^{-10}$:

| case | native $(\hat r-r^\ast)/r^\ast$ | double |
|---|---|---|
| archive 009668, 010128 (known: TASK-214.5, fit-refine) | $-1.29\times10^{-9}$ | $-1.29\times10^{-9}$ |
| class `two` | $-2.04\times10^{-9}$ | $-2.04\times10^{-9}$ |
| class `base6` | $-9.89\times10^{-10}$ | $-9.89\times10^{-10}$ |
| resample `n3-s0` | $-4.06\times10^{-10}$ | $-4.06\times10^{-10}$ |
| resample `n7-s1` | $-1.55\times10^{-10}$ | $-6.41\times10^{-10}$ |
| resample `n7-s0` | $2.25\times10^{-11}$ | $2.70\times10^{-10}$ |

The archive values agree with `fit-refine.py`'s independent 50-digit
refinement (size 20.517784194189595 against 20.517784167666967). The class
and resample cases are outside fit-refine's selection (goldens and archive
only). With prob held, every replayed size is within $4\times10^{-16}$.

### Negative controls

[`negative-controls.py`](2026-11-14-cert-replay-data/negative-controls.py)
perturbs stored records so that the claim becomes false and replays them
(same run, native baseline). All eight fail on the intended comparison:

| control | case | expected failure | verdicts |
|---|---|---|---|
| nbinom size times $1+2\times10^{-6}$, all free | archive 005094 | `root` | dispersion holds, root fails |
| nbinom size times $1-2\times10^{-6}$, prob held | archive 005122 | `root` | root fails |
| Poisson-limit sample claimed finite | archive 005095 | `dispersion` | dispersion fails, root fails |
| finite sample claimed Poisson limit | archive 005094 | `dispersion` | dispersion fails |
| lomax λ times $1+10^{-5}$, all free | archive 004814 | `root` | gain and concave hold, root fails, global not run |
| lomax λ times $1+10^{-9}$, κ held | archive 004852 | `root` | root fails |
| finite-MLE sample claimed exponential limit | archive 004814 | `face` | face fails (a point with $h>10^{-8}$) |
| exponential sample claimed finite near the exponential | archive 004817 | `gain` | gain, root and concave fail |

## Open entries

No open comparison in the prototype run. Open in the sense of the brief
(what the replay does not settle yet, and what is missing):

1. **Pad adequacy.** Re-derivation checks the claims, not the comparisons the
   core actually made; whether every binary64 value lay within its pad of
   the exact value is not checked. Missing: the trace hook (design note) or
   per-family mirrors of the whole certificate search.
2. **Claimed widths read from the code.** nbinom $5\times10^{-7}$, lomax
   $8\times10^{-7}$ in $z$ and $0.75\times10^{-10}$ in $\log\lambda$ come from
   reading `nbinom/certificate.hpp` and `lomax/certificate.hpp`, `mle.hpp` at
   `7f12c88d`. If step 5b or later work changes them, the replay must follow.
   Missing: a review rule, or the trace.
3. **nbinom counts above 20,000** use ordinary mpmath digamma (numerical, not
   an enclosure): the `wide` class, both modes. Missing: an interval digamma
   (or the finite sum, at $10^6$ terms per evaluation).
4. **lomax "no finite maximizer" classification**: the tail argument is a
   sketch by this job, not reviewed. It is information only; nothing is judged
   on it.
5. **Mirrors** follow the double-only arithmetic; native builds use long double
   in the lomax Newton polish and the held-κ sum. The mirrors are context
   only.
6. **Families not replayed**: all but nbinom and lomax (inventory above;
   proposed jobs below).

## Proposed wiring

Not done (constraint of this job). After review:

1. Add to the `strict` recipe of the Justfile, after `fit-refine.py`:
   `python3 $v/cert-replay.py "{{native}}" "${double[@]}" || status=1`.
2. Add `cert-replay.py` to the `NOTE` string in `strict_common.py` and to the
   strict-layer rows of `dev/validation/README.md`.
3. No `strict-known.json` entries are needed for nbinom and lomax at
   `7e3c9779` (no failures). New failures enter only with
   `--add TASK-<card> --match <regex>`.
4. Run cost: about 10 CPU s for both modes; no cache needed yet.

## Proposed next jobs for step 8

Each is a separate brief; all run after the 5b merge of the families they
touch.

1. **Exact face inequalities (mechanism D), all families.** Add `moment`
   comparisons to `cert-replay.py` for binom (underdispersion, unpadded
   binary64), gompertz (overdispersion on raw moments, unpadded), pig binary
   counts, foldnorm $H(0)$, bbinom size 2, cauchy half mass, slash, hcauchy and
   sgomp multiplicities, zip zero fraction, tnorm window face sign, and the
   exact point-mass and two-point predicates of the discrete families. Exact
   rational arithmetic; seconds of CPU. Scope: replay code and records only;
   any failure goes to a card for the family owner. Highest value per hour:
   binom and gompertz decide `limit(true)` from unpadded binary64 moment
   comparisons.
2. **Scalar roots and represented likelihood (mechanisms B, C).** kumar,
   beta, nsbeta, gamma (with invgamma, lgamma, naka), gumbel, weibull,
   invweibull, llogis, pareto3, lnorm, logitnorm, logis, dnorm, dweibull,
   lgser, zeta, yulesimon, hcauchy, pig, genbeta (both beta shapes held),
   frechet held paths, gev held σ, the modifier-route conditional means.
   Re-derive each claimed width from the code and record it in the script;
   interval arithmetic where the score is elementary, numerical 50 digits
   with a second precision for digamma. About a day; CPU under a minute.
3. **Lomax transfers and one-dimensional covers (E, F).** pareto2 and
   invpareto (data transforms of the lomax replay; this also exhibits the
   pareto2 `limit(true)` mapping), betapr and burr unit-shape subsets, gpd,
   ht, skellam, slash, sgomp, dgamma, paralogis, invparalogis, foldnorm,
   fatigue, cauchy held scale, trgamma and invtrgamma profiles, huber's ε
   profile. Each needs its dossier bound coded in interval form. Several days;
   CPU minutes per family, a cache from the start.
4. **Integer searches (G).** binom free size, hyper, nhyper, bbinom free size,
   cmb sizes, triang mode enumeration: high-precision profile at each
   enumerated value and the tail bound. Cost proportional to the ranges; the
   archive ranges are small.
5. **Trace hook design review and multi-dimensional covers (F, 2D and up).**
   With Jonas: whether the hook of the design note goes into the core, and for
   which families (gev joint, emg, burr joint, betapr joint, bnbinom, genbeta,
   gb2, lst, std, st, sstd, sged, snorm, bhatt, cmb blocks, zinb). Until then
   these families are covered by `fit-refine.py` (local) and the multistart
   oracle only.
6. **Outward-rounding pilot** (design note): nbinom and lomax, interval type
   in `core/`, two-word log, log1p and exp, compile-time switch, snapshot
   comparison against the padded certificates and against the replay. After
   step 5b; touches `core/` and two families.

Also for a card (not replay work): the margin-rule questions above (pareto2
`limit(true)` against lomax `limit(false)` for the same comparison; huber's
margin-decided faces as `limit(true)`; slash's margin-decided nonexistence),
and the lomax exponential stops that the replay proves exact (see Results):
these could be reported as proved nonexistence of a finite maximizer instead
of a margin-decided limit.

## Files changed

- `dev/design/certificate-replay.md` (new)
- `dev/validation/cert-replay.py` (new)
- `dev/research/evaluations/2026-11-14-cert-replay-report.md` (new, this file)
- `dev/research/evaluations/2026-11-14-cert-replay-data/` (new): `records.jsonl.gz`
  (one line per replayed case and mode), `run.log`, `negative-controls.py`

No core header, binding, `strict-known.json`, Justfile, `dev/STATUS.md` or
roadmap file was changed. Nothing was committed.
