# Gompertz, ZIB, and Slash margin follow-ups

TASK-214.43 · `codex/margin-2` · Base `460a0ff2`

The Gompertz counterexample now returns a certified finite estimate, and the held-hazard comparison resolves a second root that binary64 previously rounded onto the exponential face. ZIB requires a finite comparison bound for a certified ZIP limit. The Slash proof below resolves the symmetric example mathematically. The general free-location fitter retains the margin outcome.

## 1. Gompertz with a small positive rate

The original score formed differences of nearly equal exponentials after dividing each observation by the sample mean. At the counterexample’s scale, both the score calculation and the rounded normalized inputs can erase the positive right score. With power-of-two scale $2^e$, let $z_i=x_i/2^e$, $\beta=b2^e$, $S=\sum_i z_i$, $Q=\sum_i z_i^2$, and
$$
H(\beta)=\sum_i z_i\int_0^1 \exp(\beta z_i t)\,dt.
$$
The free profile score has the sign of $SH/n-H'$. Its constant term is $-T/(2n)$, where $T=nQ-2S^2$ is the existing padded face comparison on the original data. The solver carries this term into the Taylor evaluation, brackets the rate, and checks padded signs on both sides of its represented value within half the requested relative tolerance. The exact profile is strictly concave. The bound $H'/H\le1$ controls the propagation of the rate error into $a=n/(2^eH)$. The near-constant hazard-underflow guard still precedes the solve. See `gompertz/mle.hpp:115`, `:157`, `:284`, and `:403`.

The independent 100-digit reference differentiates the direct profile using $\operatorname{expm1}(bx)/b$. For $(415.8753190533696,2688.353999767103,240.47409880682352)$, it gives $a=0.00089694051322738038848$, $b=5.2888123512948032101\times10^{-21}$ in the original units, and log likelihood $-24.0495630467305243$. The rate in the scaled units is about $2.17\times10^{-17}$. Both compiled probes returned a certified estimate in one iteration at the defaults. The appended test also checks two power-of-two rescalings, both coefficients, and log likelihood at `kFitReferenceTol`. See `gompertz/test.cpp:261`. [Reference and arithmetic checker](2026-11-21-margin-2-data/check.py) · [Output](2026-11-21-margin-2-data/high-precision.txt) · [Native probe](2026-11-21-margin-2-data/gompertz-probe-native.jsonl) · [Double-only probe](2026-11-21-margin-2-data/gompertz-probe-double.jsonl) · [Probe inputs](2026-11-21-margin-2-data/gompertz-probe.in)

## 2. Gompertz with the initial hazard held

In scaled coordinates, the held score at zero is $D=S-\alpha Q/2$, with $\alpha=a2^e$. The new comparison reuses the moment pairs $s_h+s_l$ and $q_h+q_l$. Put $p=\mathrm{fl}(\alpha q_h/2)$. The product residual $p_e$ comes from FMA, and TwoSum gives $d+d_e=s_h-p$. The unevaluated comparison is therefore $d+(d_e-p_e+s_l-\alpha q_l/2)$.

Let $u=2^{-53}$, $\gamma_j=ju/(1-ju)$, $\eta=2^{-1074}$, and $W=|d_e|+|p_e|+|s_l|+|\alpha q_l/2|$. The pad is
$$
(1+10^{-6})\left[2(\gamma_n^2s_h+\gamma_{2n}^2p)+16uW+16n(1+\alpha)\eta\right].
$$
For $n\le2^{30}$, summing the high words and then their residuals gives moment errors bounded by $\gamma_n^2 S$ and $\gamma_{2n}^2 Q$ in the normal range. For the squares, the low-word sum includes one product residual and one summation residual per observation. After weighting the second moment by $\alpha/2$, the factor two covers the substitution of the rounded high terms for the exact scales. The low expression has one product and three additions or subtractions, so its absolute arithmetic error is bounded by $\gamma_4 W$ before accounting for the calculation of $W$. The $16uW$ term allows for both calculations and the final addition when deciding the sign. The underflow allowance covers rounding of the scaled inputs, the square and product residuals, and the scaling of $\alpha/2$. Scaling an input can move its value by at most $\eta/2$ and its square by at most $\eta$. Each subnormal product or residual loses at most $\eta/2$. The final factor supplies practical slack for evaluating the pad itself. If $\alpha\ge8n$, the largest $z_i$ is at least $1/2$, so $Q\ge1/4$ and $S<n$ already give a strict negative sign. See `gompertz/mle.hpp:137` and `:427`.

The earlier margin-fix report explicitly says that its free-fit constants had no independent check. Here the separate Fraction checker computes the exact binary64 input score and emulates the rounded calculation, including an exact product residual rounded once for FMA. All 3,360 comparisons fell inside the proposed error allowance and gave the exact sign when decisive. The maximum observed error divided by the pad was 0.142657. The check supports the derived allowance on those inputs. The certificate uses the package’s practical floating-point margin convention. [Earlier margin-fix report](2026-11-15-margin-fixes-report.md) · [Exact-rational check](2026-11-21-margin-2-data/high-precision.txt)

For $x=(1,2)$ and binary64 $a=1.2$, the former normalized comparison rounded both sides to $1.5$. The exact unscaled score is $2^{-53}>0$. The 100-digit profile gives $b=3.0839528461809904368\times10^{-17}$. Both compiled modes agree with the reference at `kFitReferenceTol`. The appended test also checks the next representable $a$ above $1.2$, which gives a certified exponential limit, and $x=(0,2),a=1$, which gives a margin-decided exact tie. See `gompertz/test.cpp:277`.

## 3. ZIB with a rejected finite-size witness

The final face fall-through requested an unresolved comparison, but `witness()` used to return before setting the flag if the candidate parameters or likelihood were nonfinite. The shared helper now records the comparison, budget, and unbounded upper endpoint before filtering the candidate. The ZIB outcome also requires valid ZIP coefficients, a finite face likelihood, and a finite upper bound at least as large as the face likelihood. The existing finite-estimate fallback still applies when a checked candidate is available. See `binom/ztbinom_certificate.hpp:181` and `binom/zib_mle.hpp:549`, `:576`, and `:625`.

For samples with at most $2^{22}$ observations and nonnegative signed 32-bit counts, the sums used by the proposal are exact before division. After the no-zero and zero-inflation delegates and the binary-support return, the first integer grid size is $M+1$ for maximum count $M\ge2$. On the held-inflation branch the initialized probability is in $(0,1)$, because the positive mean is at most $M$. Its likelihood is finite even if the probability solver rejects the point. With inflation free and at least 40 iterations, the positive-mean probability bracket at $M+1$ is narrower than $2^{-39}$ and its midpoint is strictly interior. If the zero mass underflows, the inflation branch gives the zeros positive mass. If the constrained branch instead changes the probability to the ordinary mean divided by size, that branch had binomial zero mass above the observed zero fraction, at least $2^{-22}$. This bounds the conditional mean by about 17, using $rp/(1-(1-p)^r)\le1+rp$ and the probability bracket. The recomputed zero mass is representable. Below 40 iterations the bracket helper returns NaN for the fractional grid sizes, and the required grid refinement exits before ZIP nomination. A subsequent update cannot replace a finite incumbent with a nonfinite likelihood. This rules out the proposed invalid-witness fall-through in that count range, conditional on the ordinary binary64 likelihood evaluations.

The appended regression directly injects an infinite size and, independently, an infinite negative likelihood into the witness seam. Each retains an unresolved comparison. A missing bound without the flag also fails. The public sample $(1,8)$ still returns a certified ZIP limit with rate $4.5$ and zero inflation. See `binom/test.cpp:688`. The recorded route samples exposed no invalid-witness fall-through.

## 4. Slash with free location and half multiplicity

The dossier gives a strict local bound when the off-point reciprocal sum vanishes. For location offset $\delta=m-v$ and small scale $s$, the center points contribute $k\log\int_0^1 e^{-\delta^2 u/(2s^2)}\,du$. The off-point linear terms cancel. For $0<|\delta|\le s$ the upper bound is $\delta^2(C-k/(8s^2))$, where $C=4\sum_{x_i\ne v}(x_i-v)^{-2}$. For $|\delta|\ge s$, the center contribution is bounded above by a negative constant. The off-point density terms supply strictness at $\delta=0$. This handles the corner even though the likelihood tends to $B_v$ there. A proof for a general sample still needs a comparison away from that neighborhood. See `slash/README.md:129`.

For the symmetric sample $(0,0,-1,1)$, the local result extends globally. Set $D=m^2$, $V=4s^2$, and $h(t)=(1-e^{-t})/t$, with $h(0)=1$. The inequality $1-e^{-t}<t/(1+t/2)$ for $t>0$ follows by integrating $1/(1-y^2)$ from zero to $t/2$ when $t<2$, and directly when $t\ge2$. For $V\le1-D$ and $|m|<1$, this gives $h(m^2/(2s^2))\le1-D$. The off-point factors then give $L/B_0<1$. For $V>1-D$, applying the same bound to both off-point factors gives
$$
\frac{L}{B_0}
\le
\frac{4V^2}{(V+D)^2[V^2+2(1+D)V+(1-D)^2]}.
$$
Writing $w=V-(1-D)>0$, the denominator minus $4V^2$ is $4D(1-D)+4w+(9-4D)w^2+6w^3+w^4>0$. The bound is strict at $m=\pm1$ by continuity of that expression, and projection onto the sample range handles locations outside it. Thus $B_0$ is an unattained supremum for this sample. Translation, scaling, and equal replication of the four observations preserve the conclusion.

The 100-digit check evaluated the symmetric bound at 36 interior points. For the asymmetric $(0,0,-1,1.0001)$, the explicit point $m=-0.00000427, s=0.14$ gives $\log(L/B_0)=4.08618293225622\times10^{-10}$, below the $4\times10^{-8}$ decision margin. The free-location implementation retains its existing margin classification and returns `failure(Unresolved)`. The new regression checks that flag on the symmetric sample and checks certified `nonexistence(Unattained)` with the location held at zero. The asymmetric regression is retained. See `slash/mle.hpp:406`, `:446`, and `slash/test.cpp:307`. [High-precision output](2026-11-21-margin-2-data/high-precision.txt)

## 5. R reference tolerance

The flat held-`a` zero-data test compares the returned free $b=1$ and held $a=2$ at $10^{-10}+(1+10^{-10})\,2^{-53}$, the R equivalent of `mltest::kFitReferenceTol`. See `bindings/r/distributions/gompertz/test.R:89`.

## Snapshot and suite results

The stored November 15 baselines matched all 815 selected canonical records and all 1,950 modifier-route records in each precision mode. Every family had zero changed records and zero maximum relative coefficient movement among estimate pairs. No outcome kind, reason, or certification flag moved. The nine canonical families were bbinom, binom, cmb, geom, gompertz, lgser, nbinom, pois, and slash. Shifted Gompertz has no dependency on the Gompertz fitter. The binomial routes were tbinom, zib, zmbinom, and ztbinom. All 12 other archived routes sharing the modifier adapter were checked as well. [Per-family and route comparison](2026-11-21-margin-2-data/snapshot-diff.txt) · [Stored baselines](2026-11-15-fit-snapshot-data/)

| Run | Machine | Result |
|---|---|---|
| Canonical and route snapshots, both modes | Workstation, GCC 15.2 | No differences in selected cases |
| Certificate replay, both modes | Workstation, one slot, 50 digits | 1,434 cases judged, zero failures, two open CMB search comparisons for archive case 000838 in the unchanged inputs |
| `just test-changed --no-long-double` | Workstation, four slots, GCC 15.2 | C++ 9/9 native and 9/9 double-only. R 2,798 pass, 19 skip, zero failures or warnings. JS build passed and 264/264 tests passed |
| `just quality gompertz slash binom` | Workstation, four-slot fallback | Three passed. Simbox was unreachable and returned 75 |
| `just site-check` | Workstation | Passed, including 85 family pages and 85 generated C++ snippets |

[Certificate replay output](2026-11-21-margin-2-data/cert-replay.txt) · [Native provenance](2026-11-21-margin-2-data/native.provenance.json) · [Double-only provenance](2026-11-21-margin-2-data/double.provenance.json) · [Native route provenance](2026-11-21-margin-2-data/routes-native.provenance.json) · [Double-only route provenance](2026-11-21-margin-2-data/routes-double.provenance.json)

## Open

- ZIB historical reachability beyond the exact count-sum range needs either a public reproducer or a bound that remains valid when accumulated integer sums round. The witness and outcome guards apply independently of that reachability.
- General free-location Slash certification with a vanishing reciprocal sum needs a strict comparison over the complement of the proved corner neighborhoods. The symmetric proof is recorded here and is not used as a fitter shortcut.
