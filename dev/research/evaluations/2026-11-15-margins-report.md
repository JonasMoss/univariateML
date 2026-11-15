# Margin and unpadded-comparison decisions behind certified outcomes (TASK-214.27)

Read-only analysis at master `e6f7d131`: Python, mpmath, exact rationals and reading C++; nothing was compiled or
run natively. Scripts and outputs are in [2026-11-15-margins-data](2026-11-15-margins-data/). Source of the question:
"Findings beyond the prototype" in the [certificate-replay report](2026-11-14-cert-replay-report.md). Notation:
$u=2^{-53}$, $\gamma_k=ku/(1-ku)$ (Higham, *Accuracy and Stability of Numerical Algorithms*, 2nd ed., Lemma 3.1 and
section 4.2); $S=\sum x_i$, $Q=\sum x_i^2$.

## Summary

| case | code now (certified flag) | wrong decision possible? | proposal |
|---|---|---|---|
| pareto2 exponential stop | `pareto2/mle.hpp:128` `limit(true)`; lomax `lomax/mle.hpp:328` and invpareto `invpareto/mle.hpp:56` `limit(false)`; the decision is lomax's margin $10^{-8}$ per observation (`lomax/certificate.hpp:8,190-248`) | Yes: $x=(1,28.8092065727775)$, `min = 0`: the MLE exists (exact gain $5\times10^{-9}$ per observation), the core stops at the exponential limit and pareto2 certifies it | (c): one word, `limit(false)` |
| huber normal and Laplace faces | `huber/mle.hpp:41-42` `limit(true)` | Not through the margin. The public fitter (`huber_mle`, line 658) decides the faces by a segment cover with padded strict comparisons (lines 846-880); the $10^{-8}n$ slack decides only the internal proposal (`huber_candidate_mle`, lines 514, 540-545) | none for the faces; see the notes on the finite winner |
| zib zero-inflated-Poisson face | `binom/zib_mle.hpp:625` `limit(true)` | Not through the margin: the $10^{-8}n$ tests (lines 209, 214, 275) only nominate the face; the limit is returned after `cover_face` shows every finite size lies below the face by $10^{-9}n$ (lines 469-504, `ztbinom_face_certificate.hpp:119-141`), or after the zero-truncated equivalence (lines 515-548). One edge to test, below | none; one edge case to check |
| slash half-mass collapse | `slash/mle.hpp:406` ($\ge$ best $-10^{-8}n$) then `slash/mle.hpp:440` `nonexistence(Unattained)`; `nonexistence()` is certified by construction (`core/outcome.hpp:42-44`) | Yes, a wrong nonexistence: $(0,0,-1,1.0001)$ has an MLE, interior exceeds the collapse by $4.1\times10^{-10}$, margin is $4\times10^{-8}$ | (c): do not return nonexistence when the margin decides; see section 4 |
| binom Poisson face | `binom/mle.hpp:225` `x_bar / var_pop <= 1.0`, outcome `binom/mle.hpp:340` `limit(true)` | Yes, at exact ties and at $\delta=1$ for large data: $x=(68,56,51,51,44,43,49,59,53)$ is an exact tie (Poisson limit), binary64 says finite | (b): exact integer comparison |
| gompertz exponential face | `gompertz/mle.hpp:115` `sum_xsq / dn >= 2.0 * mean_x * mean_x`, outcome `gompertz/mle.hpp:50` `limit(true)` | Yes, relative gap $3\times10^{-18}$ already fools it (both builds) | (a): double-double evaluation with a rigorous pad; band uncertified |

Stored snapshots do not touch any boundary: the nearest binom case has relative gap $7.7\times10^{-2}$ and the
nearest gompertz case $5.3\times10^{-2}$ ([snapshot_gaps.txt](2026-11-15-margins-data/snapshot_gaps.txt)), so
the replay on snapshots cannot see these defects; constructed samples do.

## 1. pareto2 versus lomax: the exponential stop

**Criterion.** With $y_i=x_i/\bar x$, $A(q)=n^{-1}\sum\log(1+qy_i)$ and $h(q)=\log q-\log A(q)-A(q)$
([lomax README](../../../univariateML-cpp/include/cppunivariate/distributions/lomax/README.md), "reason"), the finite MLE
exists iff $\sup_q h(q)>0$ ($h\to0$ as $q\to0$). The core stops at the exponential limit when every upper bound of $h$
is at most the margin $10^{-8}$ per observation (`lomax/certificate.hpp:8`, `190`, `248`); so $0<\sup h\le10^{-8}$
is decided by the margin and the proved statement is "no improvement above $10^{-8}n$", not "no MLE". lomax and
invpareto therefore return `limit(false)`. pareto2 with `min` held delegates to `lomax_mle` on $x-\min$
(`pareto2/mle.hpp:94`) and so makes the same decision, but returns `limit(true)` (line 128). A sample with
zero excess ($x=\min$) is a separate branch (`Unbounded`, point mass, `Unattained`, lines 23-72) and never reaches the
stop.

**Evidence.** Two observations $(1,r)$: $\sup h$ crosses zero at $r_c=28.80920557873\ldots$ (README: "approximately
28.81"). Exact gain at the doubles themselves ([lomax_gain.txt](2026-11-15-margins-data/lomax_gain.txt)):

| $r$ | exact $\sup_q h$ per observation | core |
|---|---|---|
| 28.809205777543127 | $1.0\times10^{-9}$ | exponential stop |
| 28.809206572777494 | $5.0\times10^{-9}$ | exponential stop |
| 28.80920756682043 | $1.0\times10^{-8}$ | at the margin |

So `mlpareto2(c(1, 28.809206572777494), min = 0)` is certified as the exponential limit while the MLE exists
(the sup is attained at $q=e^{1.2456}$). This is a hypothesis about the core's output, derived from the
margin in the source, not a native run.

**Proposal (c).** `return FitOutcome::limit(false);` at `pareto2/mle.hpp:128`. Nothing else changes; trade-off: none
(this is a consistency fix; lomax's README already says the tolerance "can reject a mathematical MLE").
Replay predicate: in `cert-replay.py` the lomax `limit` branch already tests $\sup h<10^{-8}$ ("face"). Add a stricter
comparison and make the certified flag depend on it; reuse for pareto2 on $x-\min$ (held `min` from `r['held']`):

```python
strict, info = lomax_bnb(p, lo, hi, mp.mpf(0), budget=budget)      # sup h < 0 everywhere: the exponential limit is proved
if certified and strict != 'holds':       # certified limit but only the margin decides
    verdict = 'fails'
```

## 2. huber and zib: what the $10^{-8}n$ margin does

### huber

The public fitter `huber_mle` (`huber/mle.hpp:658`) calls `huber_candidate_mle` (line 369) for a proposal, then, with
free shape (`epsilon` NaN), recomputes both faces and covers the shape range $[\text{lower},\text{upper}]$ by
segments. A face is returned only when `current.bound < face - arithmetic` for every pending segment and the two
tails are closed (lines 846-880), with $\text{arithmetic}=1024\epsilon(1+|\text{incumbent}|+n)$ and
`normal_wins = normal > laplace_upper + arithmetic` (padded strict comparisons); otherwise `stop()` (line 740)
returns `IterationFailure` with `comparison_tie` or `budget_exhausted`, which `outcome()` maps to a failure. The
$10^{-8}n$ slack decides the proposal's face (lines 514, 540-545, `compare_face`), which the certificate then
ignores. So where the margin "decides", the face is not selected; the supporting-plane cover must show every
compact segment lies below the face by more than the arithmetic pad. `limit(true)` is justified as "proved
conditional on computed comparisons" ([README](../../../univariateML-cpp/include/cppunivariate/distributions/huber/README.md)
says the same: `numerical: true`).

What the margin does protect against: (i) the proposal flipping between faces and a finite maximum when the
profile is flat; (ii) `child.bound > incumbent - margin` prunes a segment only when it lies a full $10^{-8}n$ below
the incumbent, a safe direction when the incumbent is a face; (iii) the guard `arithmetic < margin/8` rejects
$n$ so large that padding exceeds the margin. Not examined: the finite-winner branch accepts `global <= incumbent + margin`
(line 833), i.e. a certified interior estimate whose likelihood is only within $10^{-8}n$ of the proved global
bound; that is an accuracy statement for the estimate, not for the faces.

Proposal: none for the faces. The inventory entry should be restated: the faces are decided by padded comparisons.

### zib

The nominations (`zib_mle.hpp:209`, `214`, `275`: `zip.logLik >= best.logLik - 1e-8 n`) set `ZipLimit` on the
proposal. `zib_mle_search` then (lines 469-504) runs `cover_face` (`ztbinom_face_certificate.hpp:119-141`): for each
integer size block, `ll + block_error + margin < face` with $\text{margin}=10^{-9}n+4096\epsilon(1+|\text{face}|)$ prunes
it; `ll > face + margin` returns `Finite` (status back to `Ok`); a size inside the margin makes the cover
`Unresolved`, which becomes `comparison_tie` and an `Unresolved` failure. Only `Face` reaches
`certified_face` (clears `comparison_tie`) and `limit(true)`. The second path (lines 515-548) accepts when the
zero-truncated binomial fit is a Poisson limit, $e^{-\lambda}+8192\epsilon(1+z)<z$ and the two rates agree to `tol`: the
equality $|\ell_{ZT}+\text{shift}-\ell_{ZIP}|<\text{margin}$ checks a mathematical identity (the unconstrained
zero-modified bound equals the ZIP face when the constraint is slack) rather than deciding a comparison. In both
paths the margin acts in the safe direction: the face is certified only when finite sizes are below it by the
margin, and an unresolved case is a failure. `limit(true)` is justified.

Edge to test (hypothesis, not run): the final fall-through `witness(best.stop_detail, {best.size, best.prob, best.pi},
best.logLik, false, true)` (line 551) returns early without setting `comparison_tie` when
`!std::isfinite(ll) || !valid_stop_coefficients(...)` (`ztbinom_certificate.hpp:182-184`). If that happens while
`status == ZipLimit`, `outcome()` (`zib_mle.hpp:625`) maps it to `limit(true)` without a certificate. A native
test with a proposal whose size is not finite would settle it; a one-line hardening is to set
`d.comparison_tie = comparison` on that early return too.

## 3. binom: `x_bar / var_pop <= 1.0`

**Criterion.** Poisson limit iff $\bar x\le\sigma^2_{\text{pop}}$ (README existence; equality selects the limit):

$$\delta=nS+S^2-nQ=n^2(\bar x-\sigma^2_{\text{pop}})\le0 .$$

$\delta$ is an integer for integer data, $\delta\equiv S^2\pmod n$, and $\delta(x+t)=\delta(x)+n^2t$ for a shift $t$.

**Binary64 evaluation** (`binom/mle.hpp:71-90`, plain `double` in every build; `long double` is used only in the
log-likelihood): `sum_x += v` (exact while $S<2^{53}$, so for $n<2^{22}$ with int data), `x_bar = sum_x/dn`,
`d = v - x_bar; var_pop += d*d`, `var_pop /= dn`, test `x_bar/var_pop <= 1.0`. `sum_x_sq` is computed but not used.

**Forward error.** Let $\hat m=\mathrm{fl}(S/n)=\bar x(1+\varepsilon_0)$, $\hat d_i=(x_i-\hat m)(1+\varepsilon_i)$,
the squares and the $n-1$ recursive additions add $1+(n-1)$ rounding factors, and then two divisions and the shift
term. With $V=\sum(x_i-\bar x)^2=n\sigma^2_{\text{pop}}$:

$$\hat r=\frac{\bar x}{\sigma^2_{\text{pop}}}(1+\theta),\qquad|\theta|\le\gamma_{n+5}+\frac{n\bar x^2u^2}{V},\qquad
\frac{r-1}{\ }=\frac{\delta}{nQ-S^2}.$$

This holds for any IEEE correctly rounded $+,-,\times,\div$ (no libm), with or without FMA contraction (an FMA
only removes a rounding). It is a rigorous bound under that assumption; checked on 1718 constructed
(sample, variant) pairs, worst observed error is 0.27 of the bound ([bound_check.txt](2026-11-15-margins-data/bound_check.txt)).
So the decision is correct whenever

$$\frac{\lvert\delta\rvert}{nQ-S^2}>\gamma_{n+5}\ (1+O(u)),$$

and can be wrong only in that band, which always contains $\delta=0$. In integer terms the band is
$\lvert\delta\rvert\lesssim(n+5)u\,n^2\sigma^2_{\text{pop}}$; for $n=1000$, $\bar x\approx\sigma^2\approx10^9$ it is
$\lvert\delta\rvert\lesssim110$.

**Constructed samples** ([binom_boundary.py](2026-11-15-margins-data/binom_boundary.py)): integer samples with exact
$\delta\in\{-2,\dots,2\}$ (choose $S$ with $S^2\equiv\delta\pmod n$, fix $Q\bmod n$ with sum-preserving pair moves, shift by
$t=(\delta_{\text{target}}-\delta(y))/n^2$; final $\delta$ recomputed in integers). 859 samples, $n=9..4000$, mean
between $10^1$ and $2\times10^9$ (int range). Results ([binom_boundary.txt](2026-11-15-margins-data/binom_boundary.txt)):

| exact $\delta$ | samples | wrong (plain) | wrong (fma) |
|---|---|---|---|
| $-2$, $-1$ | 60, 120 | 0 | 0 |
| $0$ (tie, Poisson) | 281 | 82 | 92 |
| $+1$ (finite) | 398 | 4 | 4 |

Ties fail at every magnitude with non-dyadic mean: $n=9$ already (60 ties, 9 wrong), $n=25$ 18/60, $n=100$ 24/60. The
$\delta=+1$ failures are at $n\ge1000$, mean $\ge7.5\times10^7$, relative gap $3\times10^{-17}$ to $8\times10^{-16}$ (up
to $7.5u$; the bound at $n=4000$ is $4.4\times10^{-13}$). Smallest example, an exact tie
([examples.txt](2026-11-15-margins-data/examples.txt)):

$x=(68,56,51,51,44,43,49,59,53)$: $n=9$, $S=474$, $Q=25438$, $\delta=0$, $\bar x=\sigma^2_{\text{pop}}=158/3$.
Binary64 gives $\hat r=1.0000000000000002$ (plain and fma), so the core takes the "finite" side and enters the
size search for an MLE that does not exist. Hypothesis (not run): at an exact tie the profile increment stays
positive and the doubling search ends at a size where `long double` noise flips the sign, returning `Ok`
with a huge size and `certified = true`, i.e. the worst bug class; a native run on this sample settles it. For
$\delta=+1$ the wrong side is `limit(true)` while a finite MLE exists at an astronomically large size
(practically harmless for the likelihood value, but the certification is false). The same comparison drives
zib, tbinom and ztbinom's Poisson faces through `binom_mle`.

**Proposals.**

- (b), recommended: exact decision. $S<2^{63}$, $nS<2^{95}$, $S^2<2^{126}$ and $nQ<2^{126}$ for $n<2^{32}$, $x_i<2^{31}$:

```cpp
// binom/mle.hpp, replacing `x_bar / var_pop <= 1.0` (counts are int >= 0, already validated)
#if defined(__SIZEOF_INT128__)
inline bool poisson_face_exact(span_int x) {
  unsigned __int128 S = 0, Q = 0;
  for (int v : x) { S += static_cast<unsigned>(v); Q += static_cast<unsigned __int128>(v) * v; }
  const unsigned __int128 n = x.size();
  return n * S + S * S <= n * Q;          // delta <= 0, exact
}
#else  // MSVC: limbs, or fall back to the padded test and mark the band uncertified
#endif
```

  Trade-off: exact and certified for every integer sample; costs one pass of 128-bit adds ($\sim n$ operations,
  cheaper than the two passes now); `__int128` is a compiler extension (GCC, Clang; the "standard library only"
  rule in AGENTS.md concerns dependencies, not language extensions, but a two-limb helper avoids the question).
  The two-pass variance is then not needed for the decision.
- (a), fallback only: pad, certify outside the band $\lvert r-1\rvert\le\gamma_{n+5}$, `limit(false)`/uncertified
  estimate inside. Ties always fall in the band, so (a) alone can never certify the README's tie convention; it is
  strictly weaker than (b).

**Replay predicate.** `replay_binom` (`cert_replay_faces.py:189-220`) already compares the claimed side with the exact
$\delta$ in rationals; no change is needed for (b) other than replacing the "binary64 mirror" in the note by the
exact integer test. For (a) add the band:

```python
B = Fraction((n+5)*2**-53) / (1 - Fraction((n+5)*2**-53))
if abs(difference*n*n) <= B*(n*Q - S*S): verdict = 'open'      # certified outcome inside the unpadded band
```

## 4. slash: collapse at half multiplicity

**Criterion** (slash README, "exact variational criterion"): for a value $v$ of multiplicity $n/2$, with
$c=1/\sqrt{2\pi}$, $B_v=c^n2^{-n/2}\prod_{x_i\ne v}\lvert x_i-v\rvert^{-2}$ is the collapse likelihood; an MLE exists iff
some interior value $L(m,s)\ge\max_vB_v$. Unattained collapse is the answer only if $L<B_v$ everywhere. The core
(`slash/mle.hpp:406`) returns `CollapseLimit` when $\log B_v\ge\text{best}-10^{-8}n$, so the margin decides every
case where the interior best is below $B_v+10^{-8}n$, and `outcome` maps it to `nonexistence(Unattained)`, certified.
The subdivision cover also prunes against $B_v+10^{-8}n$, not $B_v$ (lines 213, 266), so even for interior best
below $B_v-10^{-8}n$ it proves only $\sup L\le B_v+10^{-8}n$, never $\sup L<B_v$.

**Wrong nonexistence, constructed.** Sample $(0,0,-1,t)$, $v=0$, $n=4$ (margin $4\times10^{-8}$). Interior maximum
found by multistart and refined to a stationary point in mpmath (40 digits); $D=\max L-\log B_0$
([slash_margin.txt](2026-11-15-margins-data/slash_margin.txt)):

| $t$ | $D$ | $D/(n\,10^{-8})$ | exact fact | core (by line 406) |
|---|---|---|---|---|
| 1 | $-1.3\times10^{-25}$ | 0 | zero sum, $L<B$ with exponentially small gap: collapse | collapse (right) |
| 1.0001 | $+4.1\times10^{-10}$ | 0.010 | MLE exists at $(m,s)=(-4.27\times10^{-6},0.140)$ | collapse: nonexistence (wrong) |
| 1.001 | $+5.2\times10^{-8}$ | 1.29 | MLE exists | interior wins |
| 1.01 | $+7.0\times10^{-6}$ | 175 | MLE exists | interior wins |

So $t\in(1,\approx1.0009)$ gives a certified nonexistence of an MLE that exists; a value with $D>0$ at an explicit point
is an exact witness (mpmath evaluation of $L$ at doubles). Where the margin decides in the other direction
($D<0$ with $\lvert D\rvert<10^{-8}n$) the nonexistence may be true but is not proved. The dossier's
"stated tie preference" is a rule, not a proof, and `Nonexistence` is certified by construction.

**Proposals.**

- (c1) return `failure(OutcomeReason::Unresolved)` with the interior best as a candidate when
  $\lvert\text{best}-\log B\rvert\le10^{-8}n$ (reuse `comparison_outcome_of` machinery); keep `Unattained`
  only when the cover is run with cutoff $\log B_v+\text{pad}$ (not $+10^{-8}n$) and completes. Fits the fixed
  outcome set of AGENTS.md principle 1 (no uncertified nonexistence there). Trade-off: loses the documented tie
  preference for near-ties (they become failures), and the strict cover may exhaust its budget near the corner
  $m\to v$, $s\to0$ where the profile approaches $B_v$ asymptotically, as at $t=1$ (a proved nonexistence there
  needs the analytic argument, not a cover).
- (c2) add an `certified` argument to `FitOutcome::nonexistence(reason, certified = true)` and pass `false` inside
  the margin. Smaller change, but introduces an outcome (uncertified nonexistence) not in the fixed set; consumers
  that treat `nonexistence` as terminal would still stop.

**Replay predicate.** `replay_slash` accepts both `estimate` and `nonexistence:unattained` for $2k=n$
(`cert_replay_faces.py:489`). A sound failure predicate is a witness:

```python
# nonexistence claimed at half multiplicity: fails if some (m, s) has L(m, s) > log B_v (exact mpmath evaluation)
def slash_collapse_witness(x, v):        # interior_best(x) is slash_margin.py's multistart; any witness suffices
    val, m, s = interior_best(x); return ll_mp(x, m, s) - logB(x, v) > 0
```

## 5. gompertz: `sum_xsq / n >= 2 * mean^2`

**Criterion.** Exponential limit iff $nQ-2S^2\ge0$ (profile right derivative at $b=0$ is
$n(\bar x-\overline{x^2}/(2\bar x))$; equality selects the limit; README Details). Homogeneous of degree two, so a common
scale does not matter.

**Binary64 evaluation** (`gompertz/mle.hpp:71-115`): `unit = total/n` ($\texttt{total}$ in `long double` in native
builds, `double` in double-only builds), `z = x/unit` per element, `sx += z`, `sq += z*z`, `m = sx/dn`, compare
`sq/dn >= 2.0*m*m`. Only the per-element division rounding matters: the scale error cancels, so the long-double
sum changes the $z_i$ but not the size of the error.

**Forward error.** $z_i=(x_i/\hat u)(1+\varepsilon_i)$; $\hat S=(S/\hat u)(1+\sigma)$, $|\sigma|\le\gamma_n$;
$\hat Q=(Q/\hat u^2)(1+\kappa)$, $|\kappa|\le\gamma_{n+2}$; one rounding each for $\hat m$, $2\hat m\cdot\hat m$ and
$\hat Q/n$. Then

$$\frac{\text{lhs}}{\text{rhs}}=\frac{nQ}{2S^2}(1+\theta),\qquad|\theta|\le\gamma_{3n+8}.$$

Rigorous for correctly rounded $+,\times,\div$ (no libm), absent over/underflow of $z_i^2$ (nonzero terms
below $10^{-154}$ of the mean lose relative accuracy, a further absolute $n\cdot5\times10^{-324}$). Worst observed
error 0.19 of the bound on 3648 pairs ([bound_check.txt](2026-11-15-margins-data/bound_check.txt)). In practice the
error behaves like a random walk: the largest wrong decision is $9.7u$ at $n=1000$ against a bound of $3008u$.

**Constructed samples** ([gompertz_boundary.py](2026-11-15-margins-data/gompertz_boundary.py)): $n-1$ exponential
doubles, last value the double next to a root of $nQ-2S^2=0$ (a quadratic in the last value), then ulp neighbours
(up to $\pm1024$ for $n\ge30$); exact sign from rationals of the doubles. 912 samples, $n=3..1000$, four mirrors
(long double or double, with or without FMA; integers are not needed, data are real). Wrong decisions by relative
gap $|nQ-2S^2|/(2S^2)$ in units of $u$:

| gap $/u$ | samples | wrong (ld) | wrong (ld+fma) | wrong (double) | wrong (double+fma) |
|---|---|---|---|---|---|
| $<1$ | 379 | 171 | 163 | 160 | 154 |
| $[1,2)$ | 161 | 46 | 36 | 48 | 42 |
| $[2,4)$ | 171 | 19 | 15 | 16 | 18 |
| $[4,8)$ | 58 | 7 | 6 | 5 | 5 |
| $[8,16)$ | 29 | 1 | 1 | 1 | 1 |
| $\ge16$ | 114 | 0 | 0 | 0 | 0 |

Both directions occur (exponential limit claimed although a finite MLE exists: 142-169 cases per mirror; finite fit claimed although the limit holds: 54-72).
Example ([examples.txt](2026-11-15-margins-data/examples.txt)): $x=(415.8753190533696,\ 2688.353999767103,\
240.47409880682352)$, exact $nQ-2S^2=-7.4\times10^{-11}$ (relative $-3.3\times10^{-18}$): a finite MLE exists, but long double and
double builds (with and without FMA) all return the exponential limit.

**Proposals.**

- (a), recommended: evaluate $T=nQ-2S^2$ without rounding of the inputs and pad. Scale $x$ by an exact power of
  two ($2^{-\lceil\log_2\max x\rceil}$; exact unless a term underflows, which marks the sample uncertified),
  accumulate $S$ and $Q$ as unevaluated pairs with `TwoSum`/`TwoProd` (`std::fma`) before any final rounding
  (Ogita, Rump, Oishi 2005: error $\le\gamma_n^2\sum|x|$ resp. $\gamma_n^2\sum x^2$), form $T$ in double-double
  and decide by

  $$T>\text{pad}\Rightarrow\text{limit},\qquad T<-\text{pad}\Rightarrow\text{finite},\qquad\text{otherwise uncertified},$$

  $\text{pad}=(4n\gamma_n^2+16u^2)(n\hat Q+2\hat S^2)$ (the $16u^2$ term, for the double-double rounding of the last
  operations, is an estimate from the Joldes-Muller-Popescu bounds, not derived here). Tried on all 912 samples
  ([gompertz_dd.txt](2026-11-15-margins-data/gompertz_dd.txt)): 0 wrong, 0 undecided, relative band between $3\times10^{-30}$
  ($n=3$) and $10^{-22}$ ($n=1000$) against sample gaps down to $10^{-17}$. Inside the band return
  `Overdispersed` with a flag (README: equality selects the limit) mapped to `limit(false)`; the tie $T=0$ is never
  certified. Sketch:

```cpp
struct DD { double hi, lo; };
inline DD two_sum(double a, double b) { double s = a + b, bb = s - a; return {s, (a - (s - bb)) + (b - bb)}; }
inline DD two_prod(double a, double b) { double p = a * b; return {p, std::fma(a, b, -p)}; }
// s += z; q += z*z with (hi, lo) pairs (dd_add as in gompertz_dd.py), T = n*q - 2*s*s in DD
// const bool limit = T > pad, finite = T < -pad;  if (!limit && !finite) return {.. Overdispersed, margin_decided = true}
```

  Trade-off: roughly 20 flops per element instead of 3, no allocation; a rigorous decision outside a band of
  relative width about $4n^3u^2$ (no libm; the band bound is rigorous given the stated Sum2/Dot2 bounds); exact
  ties cannot be certified (an exact rational comparison would need big integers, about 2100 bits for doubles).
- (b) for gompertz means big-integer arithmetic over the doubles' exact integer images; not proportionate for the
  gain over (a), except for integer data below $2^{53}$ with $n<2^{10}$ where `__int128` suffices.
- The held-$a$ guard ($S\le aQ/2$, `cert_replay_faces.py:272-283`) has the same shape and the same fix; not analysed here.

**Replay predicate.** `replay_gompertz` (`cert_replay_faces.py:245-282`) already compares the claimed side with the
exact sign (and prints the long double and double mirrors). For (a) it should additionally mark a certified
outcome inside the unpadded band as `open`:

```python
g = Fraction(3*n+8)*Fraction(2**-53) / (1 - Fraction(3*n+8)*Fraction(2**-53))
if abs(value) <= g * 2*S*S: verdict = 'open'     # certified limit inside the binary64 band; under (a) the band is uncertified
```

## What was not done

No native run: the claims "the core returns X on sample Y" for pareto2, slash and binom are read off the source plus
the exact arithmetic above; each deserves one native confirmation (`mlpareto2`, `mlslash`, `mlbinom` on the stated
data). The huber finite-winner margin and the zib early-return edge are flagged, not settled. No C++, replay module
or board change was made.
