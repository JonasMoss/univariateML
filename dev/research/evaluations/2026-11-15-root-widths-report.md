# Root-finder widths: what the brackets guarantee (TASK-214.31)

Date 2026-11-15. Python, mpmath and reading C++ only; no C++ edited, no build. Scripts, outputs and the patch are in
`2026-11-15-root-widths-data/` (`rea.py` running-error arithmetic, `paths.py` the five emulations, `sweep.py`, `summarize.py`,
`tables.py`, `whatif.py`, `amplification.py`, `naka_gap.py`; outputs `sweep.jsonl`, `summary.txt`, `tables.md`, `whatif.jsonl`,
`amplification.txt`, `naka_gap.txt`, `replay-patched.log`; patch `replay-claims-stated-widths.patch`). Code read at master 3e4550ee.
Context: [replay report](2026-11-14-replay-roots-report.md) ("Failures", rerun of 2026-11-15), baselines in
`2026-11-15-fit-snapshot-data/`.

## Result in one paragraph

All 20 failing comparisons per mode are reproduced (offsets agree with the replay to three digits) and every one lies inside a width
derived from the code. The hypothesis "score rounding amplified by conditioning" is **true for gamma, naka and (as the data term)
weibull, but false for the three location paths**: there the dominant term is the resolution of the solved coordinate
$z=\log b$ ($b$ = shape times a data unit as small as $5\cdot10^{-12}$, so $|z|\approx 25$ and $\mathrm{ulp}(z)=16\varepsilon$);
score rounding contributes at most 2 to 7 $\varepsilon$. For weibull the cause is the binary64 rounding of
$y_i=-\log x_i$ (hypothesis confirmed), not the score arithmetic. All derived widths are at most $6.9\cdot10^{-3}$ of the $10^{-10}$
contract on every baseline case of these paths. A new finding: the Square-map shape bisection (naka, held $\Omega$) has an a priori
score noise of 100 to 2700 $\varepsilon$ for $5<k<100$, a range with no baseline case (section 5).

## 1. Method and notation

$\varepsilon=2^{-52}$, $u=2^{-53}$. For each path I wrote a line-by-line Python emulation of the C++ (Python floats follow the same
operation order; `numpy.longdouble` for the x87 intermediates of the native build; the compensated sums `transformed_detail::Sum` and the
Kahan sum of `gumbel/mle.hpp` are reproduced). **The returned coefficient is bit-identical to the snapshot in 285 of 288 cases (both
modes)**; the three exceptions are naka cases 005057 (double mode only, 3 ulp) and 005072 (both modes, last digit), where the emulation
uses a correctly rounded digamma instead of Boost's (the 3 ulp are themselves the noise of section 5). In those the offset is measured on
the returned shape.

`rea.py` carries, next to every computed number, a bound on its distance from the real-number evaluation of the same formula on the
true inputs. The rounding of each operation is accounted in one of two ways: `actual` (exact: the operation is redone in 500-bit
mpmath on the computed operands; no assumption on libm) or `bound` (a priori Higham-style: $u|v|$ per $+,-,\times,/$ and
$2u|v|$ ($=1$ ulp) per libm call `exp, log, log1p, expm1`; **assumption**: glibc is within 1 ulp). Propagation through $f$ is first
order ($|f'|$ times the argument bound; second order is below $10^{-12}$ relative in every use). Sums: double-double `Sum` exact to
$4\cdot2^{-106}\sum|x|$ per element (`bound`) or measured (`actual`); Kahan sum $(2u+4nu^2)\sum|x|$ (Higham, Thm. 4.8) or measured.
The "true" root comes from the exact data in 500-bit mpmath. Data errors enter at the leaves: $\mathrm{fl}(x-\mu)$ for frechet, the
logs and centering for weibull.

## 2. What a bracket guarantees: the common derivation

Let $S$ be the exact score in the solved coordinate $z$ (decreasing, $m=|S'|$ near the root) and $\hat S$ its double evaluation with
$|\hat S(z)-S(z)|\le E(z)$ (argument map $z\mapsto\mathrm{fl}(e^z)$, represented data and arithmetic all inside $E$). The bisection
invariant is $\hat S(\ell)\ge 0\ge \hat S(h)$, hence $S(\ell)\ge -E(\ell)$, $S(h)\le E(h)$ and

$$
z^\ast\in\Big[\ell-\tfrac{E(\ell)}{m},\;h+\tfrac{E(h)}{m}\Big],\qquad
|z_r-z^\ast|\le \rho+\frac{E}{m},
$$

with $z_r$ the returned coordinate and $\rho$ its distance to the farthest bracket end. The public coefficient is
$\hat\kappa=\mathrm{fl}(\mathrm{fl}(e^{z_r})/\text{unit})$ (or a geometric mean), so

$$
\Big|\log\frac{\hat\kappa}{\kappa^\ast}\Big|\;\le\;\underbrace{\rho}_{\text{bracket}}+\underbrace{E_{\rm arith}/m}_{\text{conditioning}}
+\underbrace{E_{\rm data}/m}_{\text{represented data}}+\underbrace{\delta_{\rm conv}}_{\text{conversion}} .
$$

Rigorous: the inequality (given the libm assumption for the `bound` mode; with `actual` it needs no libm assumption but is
a posteriori), $\rho$ and $\delta_{\rm conv}$. Estimates: first-order propagation, the slope $m$ taken at the root (it varies by
$10^{-15}$ relative over the bracket), and the a priori constants.

## 3. Paths

### 3.1 location, scale held (frechet $\mu,\sigma$ held; invweibull, llogis $\sigma$ held)

Code: `core/transformed_location.hpp:10-24` (`root`), `:46-66` (score in $z=\log b$, $b=\kappa\cdot$unit, `unit`=$\max|\log(x_i/x_0)|$,
$\kappa$ = public shape, `power=b/unit` at `:66`). Score evaluated is $S(z)=n+\sum_i \kappa w_i\,t(\kappa w_i)$, $w_i=\log(x_i/\sigma)$,
$t=\mathrm{expm1}(-u)$ (invweibull) or $1-2\,\mathrm{sigmoid}(u)$ (llogis), i.e. $\kappa\,\partial l/\partial\kappa$; the stop is
`hi-lo<=min(tol/16,4eps)` (`:20`, $=4\varepsilon$ for every control used) or `m==lo||m==hi`.

* **Bracket term.** $z$ is a double of magnitude up to 26. Once $\mathrm{ulp}(z)>4\varepsilon$ ($|z|\ge4$) the width test cannot be met
  and the loop ends at adjacent doubles, returning an end point: $\rho=\max(\mathrm{ulp}(z),\,2\varepsilon+\mathrm{ulp}(z)/2)$. In the
  baseline $\mathrm{ulp}(z)$ is 1, 2, 4, 8 or 16 $\varepsilon$ ($|z|$ from 0.3 to 25.8, `unit` as small as $5\cdot10^{-12}$ in the
  near-constant archive samples). The claim of the replay (4 eps) is unreachable by construction.
* **Conditioning.** $E_{\rm arith}\approx(7\text{ to }19)\,u\sum_i|\text{term}_i|$ a priori (measured over 65 cases); $E_{\rm arith}/m$:
  arithmetic part at most 1.6 $\varepsilon$ actual; with the data part at most 2.4 $\varepsilon$ actual, 6.9 $\varepsilon$ a priori (llogis).
* **Data.** $y_i=\mathrm{fl}(d_i/\text{unit})$ with $d_i$ from `log1p`/log differences, and $\mathrm{fl}(x-\mu)$ for frechet. Actual shift of the
  root from the represented data: at most 0.6 $\varepsilon$; a priori at most 1.2 $\varepsilon$.
* **Conversion.** exp and division: $\le 1.5\varepsilon$ a priori, 0.8 $\varepsilon$ actual.

Measured offset at most 15.3 $\varepsilon$; derived width (actual terms) at most 18.0 $\varepsilon$, a priori 23.4 $\varepsilon$
($5.2\cdot10^{-5}$ of $10^{-10}$). Worst ratio offset/width 0.86 (frechet), always inside. **Contradiction with the hypothesis:** the
offsets of the 14 failing location cases are dominated by $\rho$ (10 of them have $\mathrm{ulp}(z)=16\varepsilon$, the other 4 have 8 $\varepsilon$).
Example, invweibull 004257: $z=-24.78$, $\rho=3.55\cdot10^{-15}$, conditioning $0.29\cdot10^{-15}$, offset $2.10\cdot10^{-15}$.

### 3.2 gamma, bracket path (shape $3.4\cdot10^{23}$, $5\cdot10^{11}$)

Code: `gamma/mle.hpp:199` (`robust_s`), `:205-213` (bisection on `log_minus_digamma(mid)>s`, `hi/lo-1<=4eps`, `shape=sqrt(lo)*sqrt(hi)`);
$k\ge100$ here, so `log_minus_digamma` (`:77-`) is the asymptotic series. Only $s<10^{-3}$ reaches this path, hence $k\gtrsim500$ always.

* Bracket: relative $2\varepsilon$ to either end ($\sqrt{\ell}\sqrt{h}$ is the geometric mean).
* Conditioning: with $G(k)=\log k-\psi(k)\approx 1/(2k)$ and $s\approx1/(2k)$, the relative slope is $k|G'|\approx s$ and the relative error of
  the shape equals the relative error of $G$ plus that of $s$. Series: $E_G\approx1.5\varepsilon G$ (7 Horner levels); $s$ is the Jensen gap
  $\frac1n\sum\phi(t_i)-\phi(\bar t)$ with $\phi(t)=t-\log1p(t)$ and relative error $(n+c)u$-ish; truncation of the series
  $\le1/(132k^{10})$ (negligible). Result: conditioning 1.4 to 3.0 $\varepsilon$ actual, 8 to 13.5 $\varepsilon$ a priori.
* No represented-data term: $t_i=(x_i-c)/c$ uses the data as given and the identity is exact for any $c$.

Offsets 5.7e-16 ($2.6\varepsilon$), $-8.7\cdot10^{-16}$ ($-3.9\varepsilon$); derived width (actual) 5.6 $\varepsilon$, a priori 17.0 $\varepsilon$. Hypothesis
confirmed, but what is "amplified" is only 1 (the equation's relative conditioning is $\approx1$): the claim of 2 eps has no room for the
$\approx3\varepsilon$ error of $s$ itself.

### 3.3 naka, Square map, $\Omega$ held (archive 005059)

Code: `gamma/transformed.hpp:70-71` ($s=\frac1n\sum\phi(u_i)$, $u_i=\text{offset}+(1+\text{offset})\delta_i$), `:74-99` (bisection with probes,
`hi/lo-1<=min(tol/8,4eps)`, `shape=sqrt(lo)*sqrt(hi)`). Here $k=1.425<100$ so the score is `log(k)-digamma(k)`.
Conditioning (actual) 1.4e-15 = 6.2 $\varepsilon$: $E_s=5.3\cdot10^{-16}$ (n=5 terms of size 0.3, each with a $\log$, an `exp`
and a double-double reduction) over slope 0.427. Offset $7.9\cdot10^{-16}$ (3.6 $\varepsilon$) inside the derived 8.5 $\varepsilon$ (actual) / 28.6 $\varepsilon$ (a priori).
Over the 22 matched baseline cases: max offset 3.6 $\varepsilon$, max width 47.9 $\varepsilon$ actual, 117.8 $\varepsilon$ a priori
($2.6\cdot10^{-4}$ of the contract). **Digamma is assumed** (4 u relative error of Boost; not measurable here).

### 3.4 weibull free, class `huge` and `tiny`

Code: `weibull/mle.hpp:58-70` (`y_i=-(log1p(rel)` if $|rel|<.5$, else `log(x_i)-log_anchor)`, centered), `gumbel/mle.hpp:109-119`
(`accuracy=1e-14`, stop `hi-lo<=accuracy*lo` and the location condition; `sigma=lo+(hi-lo)/2`), `weibull/mle.hpp:75` (`shape=1/sigma`).
Score in the working type (long double native, double in the double build).

* Bracket: relative $\le5\cdot10^{-15}$ (actual final width $9.2\cdot10^{-15}$, half $4.6\cdot10^{-15}$).
* Arithmetic: $\le 2.6\cdot10^{-19}/0.43$ native; a priori about $0.87\,nu$ in the double build ($4.8\cdot10^{-14}$ for $n=500$).
* **Data.** For $x\approx10^{\pm300}$, $|\log x|\approx700$, and `log(x[i])-log_anchor` has absolute error up to $1.3\cdot10^{-13}$
  (`y_err` bound), against a spread $\sigma_y=0.29$. The actual shift of the root caused by the stored $y_i$, solved exactly in mpmath, is
  $1.68\cdot10^{-14}$ (tiny) and $3.35\cdot10^{-14}$ (huge); the replay offsets are $-2.14\cdot10^{-14}$ and $-3.03\cdot10^{-14}$, so
  data term plus bracket (at most $4.6\cdot10^{-15}$) explain them. First-order a priori bound from the leaf errors: $6.9\cdot10^{-13}$ and
  $5.4\cdot10^{-13}$ (pessimistic: the errors are added in absolute value, 20 to 40 times the actual).

Hypothesis confirmed (rounding of $-\log x$); the score arithmetic is not involved. The other 41 weibull cases have offsets
at most $4.7\cdot10^{-15}$ (archive 008385) and stay inside the claim, because $|\log x|$ is small there.

## 4. Results over the baselines (native; double mode identical to the digit for these paths, see `summary.txt`)


Per path, native snapshot (`tables.md`; "a priori" = `bound` mode, "actual" = `actual` mode of section 1; $\varepsilon=2^{-52}$):

| path | cases | claim fails | max offset | max half width | conditioning actual / a priori | data a priori / actual | conversion actual / a priori | max width actual | max width a priori | a priori / $10^{-10}$ |
|---|---|---|---|---|---|---|---|---|---|---|
| gamma bracket | 13 | 3 | 3.9 | 2.0 | 3.0 / 13.5 | 0 | 0.99 / 1.5 | 5.6 | 17.0 | 3.8e-5 |
| location, frechet | 22 | 4 | 15.3 | 16.0 | 1.8 / 4.7 | 0.6 / 0.8 | 0.48 / 1.5 | 17.7 | 21.4 | 4.7e-5 |
| location, invweibull | 21 | 3 | 11.3 | 16.0 | 1.8 / 4.1 | 0.6 / 1.0 | 0.80 / 1.5 | 17.8 | 21.4 | 4.8e-5 |
| location, llogis | 22 | 7 | 10.0 | 16.0 | 2.4 / 6.9 | 0.4 / 1.2 | 0.65 / 1.5 | 18.0 | 23.4 | 5.2e-5 |
| naka Square, $\Omega$ held | 22 | 1 | 3.6 | 2.0 | 45.6 / 114.3 | 0 | 0.68 / 1.5 | 47.9 | 117.8 | 2.6e-4 |
| weibull free | 43 | 2 | 136.5 | 22.5 | 3100 / 3100 (data) | 150.8 / 3100.7 | 0.41 / 1.0 | 3123 | 3124 | 6.9e-3 |

(all in units of $\varepsilon$ except the last column; weibull "conditioning" is the data term of the two class cases, $6.9\cdot10^{-13}$.)
In every one of the 285 bit-matched cases the measured offset lies inside the width from the actual terms, and inside the a priori width.
No case has a width that is not small: the largest is $6.9\cdot10^{-13}$ (weibull class `tiny`, the pessimistic data bound), and
$2.6\cdot10^{-14}$ at most for the others (naka, a priori, digamma assumed). The double build gives the same
offsets and widths except for the weibull a priori arithmetic term ($4.8\cdot10^{-14}$ at $n=500$) and the digamma-dependent naka rows.

## 5. A finding outside the failing set: naka Square noise for $5<k<100$

For $k<100$ the score is `log(k)-digamma(k)`, two numbers of size $\log k$ whose difference is $\approx1/(2k)$. The a priori relative error
of the shape from the score alone is $E_G/(k|G'|)$ (`amplification.txt`, digamma error assumed $4u$): 1.5 $\varepsilon$ for $k\ge100$
(series), but 16 $\varepsilon$ at $k=3$, 132 $\varepsilon$ at $k=10$, 604 at 30 and 2700 at 99. The baseline has naka Square cases only at
$k\le5.3$ and $k\approx468$. A prediction test (`naka_gap.txt`, 20 seeded samples with population $k_0=3$ to 95, digamma modelled as
correctly rounded, an assumption) gives offsets up to 109 $\varepsilon$ ($2.4\cdot10^{-14}$), always inside the derived width, always far
outside the claimed 2.5 $\varepsilon$. So the claim "relative 4 eps" fails there for a structural reason, not a rare one; the case
005057 ($k=5.29$, double build, 3 ulp from the native result) is the first symptom. The same noise applies to any digamma-based sign
decision at moderate $k$ (gamma with a fixed rate, the Reciprocal and Log maps use $10^{-12}$-$10^{-13}$ brackets and are not affected at that level).

## 6. Proposals

Two forms, as asked. Neither needs a change to the stopping rules to be true; both keep the 1e-10 contract (all widths above are at most
$7\cdot10^{-3}$ of it).

### Option A (recommended now): the certificate states the derived width

Cheap, no numerical change, replay mirrors it. Per path:

| path | stated width (relative in the public coefficient) | code comment to change |
|---|---|---|
| location, scale held | $\max(\mathrm{ulp}(z),\,2\varepsilon+\mathrm{ulp}(z)/2)+9\varepsilon$, $z=\log(\kappa\cdot\text{unit})$ | `transformed_location.hpp:16-20`: "width min(tol/16, 4 eps) is reached only while ulp(z) <= 4 eps; beyond it the loop ends at adjacent doubles" |
| gamma bracket ($k\ge100$) | $20\varepsilon$ ($2\varepsilon$ bracket, $\approx1.5\varepsilon$ conversion, score and $s$ over slope, a priori $\le17\varepsilon$ for $n\le12$) | `gamma/mle.hpp:204-211` |
| naka Square | $4\varepsilon+(E_G(k)+70\varepsilon s)/(k\lvert G'(k)\rvert)$; $E_G=1.5\varepsilon G$ ($k\ge100$), $\varepsilon\lvert\log k\rvert+2\varepsilon\lvert\psi\rvert+\varepsilon\lvert G\rvert/2$ ($k<100$) | `gamma/transformed.hpp:74-99` |
| weibull free | $5\cdot10^{-15}+4\varepsilon+(n+16)u+\sum_j\lvert\partial\log\sigma/\partial y_j\rvert\,\delta_j$, $\delta_j=2^{-52}(\lvert\log x_j\rvert+\lvert\log x_0\rvert)$ on the log-difference branch | `gumbel/mle.hpp:109`, `weibull/mle.hpp:65` |

The constants 9, 20 and 70 are the maxima of the a priori bounds over the baselines (`sweep.jsonl`), not theorems; the structure
(bracket + conversion + $E/m$ + data) is the derivation of section 2. Trade-off: the claims are honest and the replay stays a regression
oracle, but the width is a number the replay recomputes from the data, not one the core reports.
**Patch** `2026-11-15-root-widths-data/replay-claims-stated-widths.patch` (`git apply --check` clean, not applied; changes `CLAIMS` texts and the widths
in `replay_location` scale-held branch, `replay_gamma` bracket path, `replay_gamma_raw` naka, `replay_weibull`, adds `location_half`,
`log_minus_digamma_width`, `weibull_data_term`). I applied it temporarily and ran `cert-replay.py` on native and double for the six families:
946 cases judged, failures: none, the 40 TASK-214.31 entries would all be FIXED (log `replay-patched.log`); then `git checkout`
restored the file.

### Option B: remove the avoidable terms in the code and keep (smaller) claims

1. **Location: bisect $\log\kappa$, not $\log b$.** The unit ($5\cdot10^{-12}$) is only a scaling that moves $z$ to $-25$. Sketch
   (`transformed_location.hpp:62-66`):
   ```cpp
   const double t=root([&](double z){const double k=std::exp(z)*unit,c=location(k);  /* unchanged body */ },tol,maxit);
   power=std::exp(t);b=power*unit;                                                   // was b=std::exp(t);power=b/unit
   ```
   Tested as `whatif.py` (variant `kappa`): max offset falls from 15.3 to 4.7 $\varepsilon$ (frechet), 2.8 and 2.5 for the others; $\rho\le4\varepsilon$;
   a priori width $\le11.8\varepsilon$. The bracket claim becomes $\max(\mathrm{ulp}(t),2\varepsilon+\mathrm{ulp}(t)/2)+8\varepsilon$ (replay: $z\to\log\kappa$,
   $9\to8$). It still cannot be literally 4 eps (conditioning 1.8 to 2.4 $\varepsilon$ actual).
2. **Weibull: log of the rounded ratio** instead of a difference of logs on the $|rel|\ge.5$ branch:
   ```cpp
   const double q=x[i]/anchor;
   log_x[i]=-(std::abs(relative)<.5?std::log1p(relative):(q>1e-300&&q<1e300?std::log(q):std::log(x[i])-log_anchor));
   ```
   Data error drops from $|\log x|\varepsilon$ to $\approx\varepsilon$; tested (`whatif.py`, variant `ratio`): class `huge` and `tiny` offsets become
   $4.2\cdot10^{-15}$ and $4.3\cdot10^{-15}$ (the data shift falls to $4\cdot10^{-17}$); all 43 offsets are at most $4.3\cdot10^{-15}$, inside the existing $5\cdot10^{-15}$ claim
   plus conditioning (1.3 to 1.5 $\times10^{-15}$): the claim would need $5\cdot10^{-15}+8\varepsilon$. The same difference-of-logs appears in `transformed_data.hpp:43-46`
   (`log_ratio`, used by every location and gamma map): the same $|\log x|\varepsilon$ error for data near $10^{\pm300}$ (not exercised by a root claim in this task).
3. Gamma/naka: nothing to remove for $k\ge100$. For $k<100$ the noise is the evaluation of $\log k-\psi(k)$; computing it in
   `CPPUNIV_EXTENDED_REAL` (native: $2^{11}$ times smaller noise) does not help the WASM/double build; state the width (Option A).

### Option C: pad the sign checks by the score error (rigorous enclosure)

Make the score return $(g,E)$ with $E=c\,u\sum_i|\text{term}_i|$ ($c\approx7$ to 19 for the location score a priori; accumulate $\sum|\text{term}|$ in the same loop):
```cpp
template<class Score> inline double root(Score score,double tol,int maxit,double* radius){
  /* expansion as before, but require g>=err at lo and g<=-err at hi */
  for(int j=0;j<maxit;++j){const double m=lo+(hi-lo)/2;auto r=score(m);   // r.g, r.err
    if(r.g> r.err)lo=m;else if(r.g<-r.err)hi=m;else break;                 // indeterminate zone reached
    if(hi-lo<=std::min(tol/16,4*eps))break;}
  *radius=(hi-lo)/2;return lo+(hi-lo)/2;}                                   // [lo,hi] encloses the exact root
```
The returned bracket then contains the exact root whatever the conditioning; the claim is its radius plus conversion (the core reports it, e.g. in
`attainable_tol`). Cost: one extra accumulation per score evaluation; the width is data dependent (about $\ge 2E/m$, 5 to 14 $\varepsilon$), the
returned point moves by at most that, and the replay needs the radius in the snapshot. I recommend A now, B1 and B2 as small clean-ups, and C only if the
numerical-certificates track (paper plan) wants rigorous enclosures from the core.

## 7. Limits and assumptions

* libm within 1 ulp ($2u$ relative) for `exp, log, log1p, expm1` is assumed in the a priori ("bound") numbers; the "actual" numbers need no assumption but are
  per case.
* **Boost digamma accuracy is assumed** ($4u$, i.e. 2 ulp) for $k<100$; it was not compiled or measured. The naka rows with $k<100$ and
  `naka_gap.txt` depend on it.
* The emulation reproduces Python/glibc libm results; bit identity with the snapshot (285 of 288) is evidence that the C++ used the same results.
* Error propagation is first order; the "rigorous" statements are rigorous only up to that and up to the unit-roundoff assumption of x87 extended arithmetic
  being honoured (no excess precision beyond `long double` in the weibull path).
* The data term for weibull is an a priori sum of absolute leaf errors (20 to 40 times the actual shift); the exact shift is reported next to it.
* Not covered: the same coordinate effect in `invburr` (scale held, `location_fit(...,sign=-1)`, no failing case) and the `pareto3` polish; lgamma/invgamma held-parameter
  roots (brackets $2\cdot10^{-13}$, no failure).

## Files

`2026-11-15-root-widths-data/`: `rea.py`, `paths.py`, `sweep.py`, `summarize.py`, `tables.py`, `whatif.py`, `amplification.py`, `naka_gap.py`; `sweep.jsonl`,
`summary.txt`, `tables.md`, `whatif.jsonl`, `amplification.txt`, `naka_gap.txt`, `replay-patched.log`, `replay-claims-stated-widths.patch`.
Rerun: `/tmp/umvl-cpu/run nice -n 10 python3 .../sweep.py <dir>` (about 1 min), then `summarize.py sweep.jsonl`, `whatif.py whatif.jsonl`, `tables.py`.
