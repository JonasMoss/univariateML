# Exponentiated Weibull: limit faces, the `tie3` supremum and an existence criterion

TASK-217, 2026-11-15. Analysis only; no fitter, dossier or test was changed. Scripts and outputs are in
[2026-11-15-expweibull-limits-data](2026-11-15-expweibull-limits-data/). Notation follows the dossier and the
[first](2026-11-10-ew-theory-report.md) and [second](2026-11-11-ew-theory2-report.md) theory reports: scale $a$
(with $\mu=\log a$), shape $b$, exponent $c$ (with $T=\log c$), $y_i=\log x_i$, $t_i=e^{b(y_i-\mu)}$,
$h(t)=\log(1-e^{-t})$. (The task text calls the shape "$a$" and the scale "$\sigma$"; "$a\to0$" there is $b\to0$ here.)
Evidence levels: **P** proved (exact arithmetic), **L** derived at leading order (expansion, remainder not bounded),
**N** numerical (mpmath, dps 40 unless stated).

## 1. Summary

1. **There is no unknown face.** The likelihood of $0.42$ on `tie3` is binary64 cancellation (**N**, section 3).
   In exact arithmetic the profile likelihood on `tie3` rises to the inverse-Weibull value $I=-0.705046$ from
   below as $b\to0$, $c\to\infty$, and stays below it; it is not unbounded. The second report's exhaustion
   (**P**: every likelihood-retaining path converges to a finite member, the inverse-Weibull face or the power face,
   and nonconstant data are bounded) stands. The joint limit $b\to0$, $c\to\infty$, $bT\to k$ is the inverse Weibull
   law: $\log X$ is maximum-Gumbel with scale $1/k$. No other named law appears.
2. **New, leading order (L):** along the inverse-Weibull path
   $\ell=I(k,s)+A(k,s)/T+O(T^{-2})$ with $A$ in closed form (section 4). Its sign at the face optimum decides
   whether finite parameters beat the face ($A>0$) or the face is approached from below ($A<0$). The power face
   is always approached from below, with loss $\frac{n\kappa}{b}\{1+\log\frac{mb}{2n\kappa}\}$ (verified to
   $10^{-6}$ relative).
3. **Verdicts (N).** Of 19 snapshot samples (18 failing), 13 have the power face as supremum
   (limit, not attained), 4 have the inverse-Weibull face, 2 samples have a finite maximum (`ties`, an Ok fit, and
   `offset`, a conditioning failure that is not nonexistence). `tie3` and `near-constant` are the same
   two-point problem and both go to the inverse-Weibull face.
4. **Two-point samples reduce to one parameter** (**P** for the reduction, **N** for the verdict): with
   $p$ the share at the lower value, the free fit never has a finite maximum on the 9 values of $p$ tested; the
   supremum is the power face for $p<p_c=0.642878$ and the inverse-Weibull face for $p>p_c$.
5. **Existence criterion.** The exact criterion of the second report (a finite $\theta$ with
   $\ell(\theta)\ge\max(P,I)$) is kept; new sufficient condition (L): $I\ge P$ and $A>0$. No necessary and
   sufficient checkable criterion; nonexistence is not proved for any sample (section 6).
6. **Fitter defect:** the code compares faces only after a polished finite candidate exists. When the
   likelihood has no finite stationary point (the typical case here), it ends in `no-convergence` even though both
   face values are known. It should evaluate both faces first and return the winning face as a limit
   (section 7).

## 2. Joint limits (restated, with what the corners are)

With $q(t)=1-e^{-t}$, $Y=\log X=\mu+\frac1b\log T_c$, $T_c=-\log(1-U^{1/c})$. Proved in the second report (**P**):

* $c\to\infty$, $T=\log c$, $bT\to k\in(0,\infty)$, $\mu+\frac{\log T}{b}\to\log s$: $\log X\to\log s+G/k$, $G$
  standard maximum Gumbel, i.e. $X\to$ inverse Weibull$(k,s)$ (Fréchet). Explicit path
  $b=k/T$, $\mu=\log s-\frac{\log T}{b}$, so $t_i=T(x_i/s)^{k/T}=T+k\log(x_i/s)+O(1/T)$.
* $c\to0$, $b\to\infty$, $bc\to\kappa$, $\mu\to\log e_{pt}$: power law on $(0,e_{pt})$,
  $\log X=\log e_{pt}-E/\kappa$, $E$ standard exponential.
* $k=bT\to0$ or $\infty$, $b\to0$ with $c$ fixed, and so on: the likelihood of nonconstant data tends to $-\infty$
  (bound (2) of the second report). For constant data $bT\to\infty$ is the unbounded concentration path.

The expression of the family in $\log x$: the exponentiated Weibull is a three-parameter family of log-concave
laws $Y=\mu+b^{-1}\log T_c$ with location $\mu$, scale $1/b$, shape $c$; both faces are limits in the shape $c$
(Gumbel-type at $c=\infty$, negative-exponential-type at $c=0$).

## 3. The $0.42$ is cancellation (N)

[artifact.py](2026-11-15-expweibull-limits-data/artifact.py), [artifact.out](2026-11-15-expweibull-limits-data/artifact.out).
At the probe's best point $(a,b,c)=(1.877\times10^{-4},0.3753,1.351\times10^{16})$, $t=(36.35,36.35,40.01)$, so
$\sum h_i=-3e^{-36}\approx-3\times10^{-16}$, but binary64 `log(-expm1(-t))` returns multiples of $2^{-53}$;
the probe's $\sum h=-2.2204\times10^{-16}$ is exactly one ulp, so $\hat c=-n/\sum h$ is a quantised number and the
probe likelihood $0.4216$ is wrong. At 50 and 200 digits the same $(a,b)$ gives profile $\ell=-0.79056$
(true $\hat c=9.02\times10^{15}$, not $1.35\times10^{16}$); at the stored $c$, $\ell=-1.0721$.
Large exponents need `log1p(-exp(-t))`, which is relatively accurate (`core.h` in `core.py` does this).

Full mpmath profile on `tie3` ([face.py](2026-11-15-expweibull-limits-data/face.py), 60+ digits, $\mu$ solved from
the score, $c$ profiled): $I=-0.7050458837$ at $k=12.4581$, $s=2.78371$, and

| $b$ | $T=\log\hat c$ | $I-\ell_p$ | $(I-\ell_p)/b$ |
|---|---|---|---|
| 0.5 | 23.7 | 0.0661 | 0.1322 |
| 0.1 | 123.4 | 0.01290 | 0.12896 |
| 0.01 | 1244.6 | 0.001282 | 0.12822 |
| 0.002 | 6227.8 | 2.563e-4 | 0.128154 |

The gap tends to $0$ linearly in $b\sim k/T$; the slope $0.12815\,k=1.5965=-A$ matches section 4
($A=-1.5963$). Coarser profile over $b\in[e^{-8},e^{12}]$ (z units) in `profile.out`: below $I$ everywhere.

## 4. First-order structure of the two faces

### Inverse Weibull (L)

Put $u_i=y_i-\log s$ and use the path above with $k=bT$. Then $t_i=T+ku_i+\varepsilon_i$,
$\varepsilon_i=k^2u_i^2/(2T)+O(T^{-2})$, $(c-1)h(t_i)=-e^{-ku_i-\varepsilon_i}+O(e^{-T}\!)$, and
$\log t_i=\log T+ku_i/T+O(T^{-2})$. Inserting in
$\ell=n\log b+n T-\sum y_i+\sum(\log t_i-t_i)+(c-1)\sum h(t_i)$:

$$\ell=I(k,s)+\frac{A(k,s)}{T}+O(T^{-2}),$$

$$I(k,s)=n\log k-\sum_i y_i-k\sum_i u_i-\sum_i e^{-ku_i},\qquad
A(k,s)=k\sum_i u_i+\frac{k^2}{2}\sum_i u_i^2\bigl(e^{-ku_i}-1\bigr).$$

$I(k,s)$ is the inverse-Weibull log likelihood. Maximising over $s$ gives $\sum_i e^{-ku_i}=n$, and over $k$ the
second report's strictly concave profile (4). Since $\nabla I=0$ at $(\hat k,\hat s)$, the supremum over $(k,s)$ at
large $T$ is $I+A(\hat k,\hat s)/T+O(T^{-2})$. Hence (L):

* $A(\hat k,\hat s)<0$: near the face every finite parameter is below $I$; the face is approached from below
  (rate $|A|/T$).
* $A(\hat k,\hat s)>0$: finite parameters with $\ell>I$ exist (a single high-precision evaluation at one large $T$
  certifies this).

Checks: `tie3` $A=-1.5963$ against slope $1.5965$ above; `offset` $A=1.178>0$ with $I>P$ and a finite maximum $0.124$
above the faces (section 5).

### Power (L, verified)

Path: $c=\kappa/b$, $b\to\infty$, $\mu=y_+-\sigma/b$, with $m$ observations at $y_+=\max y_i$, $\kappa=n/D_+$.
Writing $g=bc\,R(t)q(t)^c$, $\log R(t)=-t/2+O(t^2)$, the $n$ observations each gain $\kappa\sigma/b$ from the shorter
distance to the endpoint (power term $c\log t$) and the $m$ top observations pay $\log R(e^{\sigma})\approx-e^{\sigma}/2$.
Maximising over $\sigma$ ($e^{\sigma}=2n\kappa/(mb)$):

$$P-\ell\;\approx\;\frac{n\kappa}{b}\Bigl\{1+\log\frac{m\,b}{2n\kappa}\Bigr\}>0 .$$

Always positive for large $b$: the power face is **never** attained locally and no finite parameter near it exceeds
it. [asym.py](2026-11-15-expweibull-limits-data/asym.py) / [asym.out](2026-11-15-expweibull-limits-data/asym.out):
ratio of true to predicted loss, `two` ($b_y=e^{6},\dots,e^{16}$): $0.9919,0.9978,0.99956,0.99992,0.999987,0.999998$;
`base6`: $0.9973,\dots,0.999999$. So $\sup=P$ (or $I$) is only beaten by a finite parameter far from both faces.

## 5. Verdict per sample (N)

Method ([sup.py](2026-11-15-expweibull-limits-data/sup.py), [profile.py](2026-11-15-expweibull-limits-data/profile.py)):
for every $b$ on a log grid ($b\,\mathrm{sd}(y)\in[e^{-8},e^{12}]$, 15 values) maximise over $c$ (grid of 33 values of
$T\in[-14,6000]$ then golden section), $\mu$ exact (the score in $\mu$ has a unique root for given $b,c$, first report);
plus a $(\log b,T)$ grid with Nelder-Mead refinement. Faces $P$ (eq. (3)) and $I$ (eq. (4), 1-D concave) in closed form at
50 digits. "Verdict" = best profile value relative to $\max(P,I)$: positive means a finite maximum, negative with the
best value at a grid edge means the face. Reproducible outputs: `profile.out`, `sup.jsonl`, `verdicts.out/json`.

| sample (snapshot) | snapshot outcome | $P$ | $I$ | $A$ | sup | face parameters |
|---|---|---|---|---|---|---|
| `tie3` $[q_{50},q_{50},q_{75}]$ | no-convergence | $-0.9340$ | $-0.70505$ | $-1.596$ | **inverse Weibull**, unattained | shape $12.4581$, scale $2.783715$ |
| `near-constant` $[c,c,c^+]$ | no-convergence | $104.268$ | $104.497$ | $-1.596$ | **inverse Weibull**, unattained | shape $1.9404\times10^{16}$, scale $2.698973$ |
| `n3-s3` | no-convergence | $-5.6675$ | $-5.5199$ | $-1.266$ | inverse Weibull | shape $3.0342$, scale $2.91814$ |
| `n7-s3` | no-convergence | $-10.438$ | $-9.8365$ | $-0.977$ | inverse Weibull | shape $3.6928$, scale $2.53642$ |
| `base6`, `tiny`, `huge` (scalings of each other) | refuted by face / no-convergence | $-8.1584$ | $-9.677$ | $2.51$ | **power**, unattained | endpoint $\max x$, shape $1.78074$ |
| `two` $[q_{25},q_{75}]$ | no-convergence | $-1.5926$ | $-2.4153$ | $0.00$ | power | endpoint $3.48736$, shape $3.0948$ |
| `half-ties` | refuted by face | $-5.6936$ | $-8.0247$ | $3.07$ | power | endpoint $3.48736$, shape $2.43327$ |
| `subnormal` $=[1,2,3]\cdot5\mathrm{e}{-324}$ | no-convergence | $2230.60$ | $2229.46$ | $0.756$ | power | endpoint $1.482\times10^{-323}$, shape $1.9946$ |
| `wide` | refuted by face | $-14.719$ | $-15.418$ | $0.420$ | power | endpoint $2.699\times10^{8}$, shape $0.054287$ |
| `n3-s0`, `n3-s1`, `n3-s2` | no-convergence | see `verdicts.out` | | | power | in `verdicts.out` |
| `n7-s0`, `n7-s1`, `n7-s2` | refuted / no-convergence | see `verdicts.out` | | | power | in `verdicts.out` |
| `ties` | Ok (uncertified) | $-4.1547$ | $-5.2925$ | $3.38$ | **finite maximum**, $0.065$ above $P$ | fit exists |
| `offset` | evaluation failure | $-9.2131$ | $-9.0773$ | $1.178$ | **finite maximum**, $0.124$ above $I$ | fit exists |

Comments.

* `tie3`: the candidate that "keeps rising" is the inverse-Weibull face, approached from below; sup $=I$, not attained,
  not unbounded. `near-constant` is the same sample after the affine map $y\mapsto(y-y_L)/D$ ($D=\log(x_H/x_L)\approx1.65\times10^{-16}$):
  the profile agrees with `tie3` to the printed digits ($-3.56\times10^{-4}$ at $b\,\mathrm{sd}=e^{-8}$), and the face
  shape is $12.4581/(\text{ratio})\to1.94\times10^{16}$. This is the exact-arithmetic answer; its binary64 rounding
  (steepness) is the contract's question and is not decided here.
* `n7-s0`: a finite local maximum exists (profile bump at $b\,\mathrm{sd}=1$, $0.227$ below $P$); the power face still
  wins, matching the code's "face refutes the candidate". `base6` also has a local bump, $0.58$ below $P$.
* `offset` ($x\approx1.66\times10^{15}+\{0,\dots,3.2\}$): a finite maximum exists, at $b\,\mathrm{sd}\approx e^{-1}$,
  $T\approx3.7$; the snapshot failure is the evaluation/conditioning stop, not an existence problem.
* The 18 "failing" classes of the fit report plus `ties`: no sample is unbounded; every sample without finite
  maximum has $\max(P,I)$ as supremum.

### Two distinct values (reduction **P**, regimes **N**)

For values $x_L<x_H$ with $m$ and $n-m$ copies, $p=m/n$: the map $y\mapsto(y-y_L)/D$ with $b\mapsto bD$,
$\mu\mapsto(\mu-y_L)/D$ preserves the family, and $\ell/n=p\log g(y_L)+(1-p)\log g(y_H)-\overline y$ with a constant
shift $-\log D$. So the outcome depends on $p$ only. In the unit coordinates ($y\in\{0,1\}$, $q=1-p$, per observation):

$$\frac{P}{n}=-\log p-1,\qquad \frac{I}{n}=\max_k\Bigl[\log k-kq-\log\bigl(p+qe^{-k}\bigr)\Bigr]-1 .$$

($P$ is attained by a tie at $y_H$ via the endpoint-from-above path.) Numerically ([twopoint.py](2026-11-15-expweibull-limits-data/twopoint.py),
[asym.out](2026-11-15-expweibull-limits-data/asym.out)): $P=I$ at $p_c=0.642878$ (**N**, bisection); $A(p)>0$ for $p<1/2$,
$A(1/2)=10^{-41}$, $A<0$ for $p>1/2$. With $n=20$ copies and the profile method, $p\in\{.2,.3,.4,.5,.6,.65,.7,.8,.9\}$:
the best finite value is below $\max(P,I)$ at the grid edge of the winning face every time ($\Delta\ell/n\in[-3.3\times10^{-3},-5.8\times10^{-5}]$).
Hence (N): **two distinct values never give a finite MLE; sup $=P$ for $p<p_c$ (power, endpoint $x_H$, shape $n/(m\log(x_H/x_L))$),
sup $=I$ for $p>p_c$ (inverse Weibull).** Counts: $n=2$, $p=\tfrac12$: power. $n=3$: $p=\tfrac13$ power, $p=\tfrac23$ inverse Weibull
(`tie3`). $n=4$: $\tfrac14,\tfrac12$ power, $\tfrac34$ inverse Weibull. This settles the dossier's `min_n: 2` question for $n=2$ at the level N.

## 6. Existence criterion

**Proved** (second report): a finite free MLE exists iff some finite $\theta$ has $\ell(\theta)\ge\max(P,I)$,
with $P=n\log(n/D_+)-\sum y_i-n$, $D_+=\sum(y_+-y_i)$, $I$ the concave profile (4).

**Derived at leading order (L):** (i) the power face is never approached from above, so a finite maximum exists only if
a finite parameter, not near the power face, exceeds it. (ii) If $I\ge P$ and $A(\hat k,\hat s)>0$ then a finite maximum exists.
(iii) $A(\hat k,\hat s)<0$ makes the inverse face a local approach from below only; it does not exclude finite maxima elsewhere.

**Numerical:** two-point samples: none; `tie3`, all failing snapshot samples except `offset` and `ties`: none; no
counterexample to "existence needs enough spread" was found. **Open:** a necessary and sufficient checkable condition; any
proof that $\ell<\max(P,I)$ everywhere on a given sample (needs the global cover of the second report). A finite local
maximum below a face (`n7-s0`, `base6`) shows local roots do not decide existence.

## 7. What the fitter should do

1. Compute both face values first, before the finite search: $P$ (already in `mle.hpp`) and $I$ (the existing
   `invweibull_mle`; with $k$ and the log scale it returns, concave 1-D profile, certifiable). Compute $A$ cheaply at
   $(\hat k,\hat s)$ (optional diagnostic and seed rule).
2. Run the finite search as now. Decision: if a polished finite $\hat\theta$ has $\ell(\hat\theta)\ge\max(P,I)-\text{pad}$,
   return the estimate (uncertified; `ties`, `offset`). Otherwise the supremum is $\max(P,I)$ at the level N:
   return the limit, `use mlinvweibull` when $I\ge P$, `use mlpower` when $P>I$, **flagged** as decided by search and
   margin (not certified), never `no-convergence`. Do not depend on the search having found a local root: that
   dependence is the cause of the failures. When $|P-I|<$ pad, name both faces.
3. Seed the finite search with the inverse-Weibull path when $A>0$ and $I\ge P$.
4. Expected outcomes: `tie3`, `near-constant`, `n3-s3`, `n7-s3`: L invweibull; `base6`, `tiny`, `huge`, `two`, `half-ties`,
   `subnormal`, `wide`, `n3-s0..2`, `n7-s0..2`: L power; `ties`: estimate (as now); `offset`: estimate with the
   steep-maximizer rule (a separate conditioning issue). The power delegate: endpoint $\max x$, shape $n/D_+$; with
   ties at the maximum the free-endpoint power law equals the `mlpower` closed-endpoint density (no $\log(e-1)$ term).

## 8. Proposed dossier text (not applied)

`existence.statement`:

> A free fit that has no finite maximum has its supremum on the power face or on the inverse-Weibull face, never
> unbounded unless the data are constant. A finite maximum exists exactly when some finite parameter attains the larger
> of the two face values; every sample of two distinct values lacks one (the power face wins when the share at the
> lower value is below 0.6429, the inverse Weibull face above). Near the power face the likelihood is always lower
> than the face value by $\frac{n\kappa}{b}\{1+\log\frac{mb}{2n\kappa}\}$ at leading order; near the inverse
> Weibull face the sign of $A=k\sum u_i+\frac{k^2}{2}\sum u_i^2(e^{-ku_i}-1)$ at the face optimum decides whether finite
> parameters exceed it.

`limits` (replace the free-fit open note; existing held-scale entries unchanged):

```yaml
  - model: invweibull
    firmness: flagged
    when: 'exponent tends to infinity, shape times log exponent tends to k > 0, log scale plus log(log exponent)/shape tends to log s'
    detect: 'All coefficients free; the inverse-Weibull face value exceeds the power-face value and every polished finite candidate by the margin'
    status: InvweibullBoundary
    message: invweibull
  - model: power
    firmness: flagged
    when: 'shape tends to infinity, shape times exponent tends to n/D_+, scale tends to the maximum observation from above with shape*log(scale/max) unbounded'
    detect: 'All coefficients free; the power-face value exceeds the inverse-Weibull value and every polished finite candidate by the margin'
    status: PowerBoundary
    message: power
```

with `limits` prose: "the two faces are approached from below: the power loss is $\tfrac{n\kappa}{b}\{1+\log\frac{mb}{2n\kappa}\}$; floating point
needs relative logs and `log1p(-exp(-t))`: sums of $\log(1-e^{-t})$ at exponent $10^{16}$ are one ulp in binary64."

## 9. Evidence summary and files

| claim | level |
|---|---|
| bounded for nonconstant data; faces are the only likelihood-retaining limits | proved (theory2) |
| two-point reduction to $p$ | proved (affine equivariance) |
| $\ell=I+A/T+O(T^{-2})$; $A$ formula | leading order, checked to 4 digits on `tie3` |
| power loss formula | leading order, checked to $10^{-6}$ (`asym.out`) |
| `0.42` is cancellation | mpmath 50/200 digits |
| per-sample verdicts, $p_c=0.642878$, no finite max for two values | numerical (grid + refinement, dps 40, not a certificate) |

Files in `2026-11-15-expweibull-limits-data/`: `core.py` (shared), `face.py`, `artifact.py`, `twopoint.py`,
`profile.py`, `sup.py`, `asym.py`, `verdicts.py` with `.out`/`.json`/`.jsonl` outputs. Reproduce with
`OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 /tmp/umvl-cpu/run nice -n 10 python3 -I <script>` from the repository root
(`profile.py` before `verdicts.py`).
