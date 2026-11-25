# Beta prime: global uniqueness and faces

TASK-356 · 2026-11-25 · Base `f22e71d9`

**Proved.** All-free global uniqueness fails on the exact binary64 sample
\((2^{-512},(2^{-256})^{[10]},(2^{256})^{[10]},2^{512})\). Both faces lose and
reciprocal reflection pairs distinct finite global maxima. On another explicit
binary64 sample \((2^{-512},2^{512})\), both faces tie strictly above every
finite fit. An exact real deformation gives exactly two finite global maxima
tied with both faces. A six-observation wide-log slice has a unique all-free
MLE.

**Proved.** With one shape held, a scalar threshold in that shape gives
finite attainment, face-only nonattainment, and the precise equality rule.
A second-order tail coefficient decides some cases where the previous leading
coefficient vanishes. With a held unit shape, a finite fit can tie the
exponential face on real data, and the prior algebraic-data exclusion gives
a terminating existence decision for binary64 data.

**Numerical.** All 24 archived public coefficient vectors met the default
relative tolerance on the original binary64 observations. The largest
observed error was \(9.03\times10^{-14}\). [Coefficient replay](2026-11-25-betapr-math-data/accuracy.jsonl)

## Hold masks and likelihood closure

Write \(p,q,b\) for shape1, shape2, and scale, and assume a nonempty sample of
positive finite observations and positive finite holds. A superscript
\([k]\) denotes multiplicity \(k\). For comparisons on \(Y=\log X\), subtract
\(\overline{\log x}\) from the mean log-density of \(Y\) to recover the mean
log-density of \(X\). This shift is parameter independent.

| Held | Exact existence and uniqueness | Outcome |
|---|---|---|
| \(p,q,b\) | The one admissible vector for every sample, including constants. | Likelihood evaluation. |
| \(p,q\) | One scale root for every sample. At \(x_i=c\), it is \(b=qc/p\). | Certified singleton. |
| \(p,b\) or \(q,b\) | One free-shape root for every sample, including constants. | Certified singleton. |
| \(b\) | One finite fit iff the sample is nonconstant. Constant data have unbounded likelihood. | Certified singleton or `Unbounded`. |
| \(p\) | Constants approach gamma of shape \(p\) and scale \(c/p\). For nonconstant data use the held-shape threshold below. | Finite fit or `GammaBoundary` (`ExpBoundary` for \(p=1\)). |
| \(q\) | Apply the \(p\)-held rule to the exact reciprocals. For constants the inverse-gamma scale is \(qc\). | Finite fit or `InvGammaBoundary` (`InvExpBoundary` for \(q=1\)). |
| None | Constants give unbounded likelihood. On nonconstant data use the compact ratio decision below. | Finite fit, one or both excluded faces, or `Unbounded`. |

The fixed-scale beta transformation, strict scale score, single-shape scores,
constant-data arguments, and likelihood-relevant joint exterior bounds are
proved in the dossier and the two earlier reports. In particular, the only
competitive all-free faces on nonconstant data are gamma and inverse gamma.
The large-shape corner does not supply a lognormal face. The previous
conditional real crossing has held
\(p=0.22690393135372058\), the first eight exact decimal observations in the
linked fixture, and last observation \(400970.63293690106e^{t_*}\), where
\(t_*\in(5.6463,5.6466)\) equates the maxima on either side of \(q=0.12\).
Its proof is conditional on the previous endpoint certificate comparisons.
The reciprocal holds inherit this competition. The earlier historical
comparison-stop behavior predates the fallback and maximizer-set rules.
[Family dossier](../../../univariateML-cpp/include/cppunivariate/distributions/betapr/README.md) · [Joint proof](2026-11-08-betapr-joint-report.md) · [Conditional proof](2026-11-07-betapr-prof-report.md#nonuniqueness) · [Conditional fixture](2026-11-07-betapr-prof-data/counterexample-refined.json) · [Outcome rule](2026-11-25-ties-math-report.md#what-this-changes-for-the-fitter)

## Compact scalar decision for all-free data

**Proved.** Specialize the outer-power report's harmonic concentration chart
to power one. Put \(r=p/(p+q)\), \(k=pq/(p+q)\), and
\(m=\log b+\log(p/q)\), the mode on log data. Write
\[
 D_r(z)=\frac{\log(1+r(e^z-1))-rz}{r(1-r)},\quad
 D_0(z)=e^z-1-z,\quad D_1(z)=D_0(-z).
\]
The mean objective is \(F=\log C(k,r)-k\,\overline{D_r(y-m)}\), where, with
\(R\) the positive Stirling remainder,
\[
 \log C(k,r)=\tfrac12\log(k/(2\pi))-R(p)-R(q)+R(p+q),\qquad
 p=k/(1-r),\quad q=k/r .
\]
The endpoint normalizer uses only \(-R(k)\) after the leading term.
At each \(r\in[0,1]\), solve the strictly decreasing mode score
\(\overline{D'_r(y-m)}=0\) in the sample log range. Its root \(m(r)\) is
strictly decreasing in \(r\) on nonconstant data, since
\(D'_r(z)=(e^z-1)/(1+r(e^z-1))\) is decreasing in \(r\) and strictly
increasing in \(z\). Then solve the strictly concave concentration problem
\(\max_{k>0}\{\log C(k,r)-k\,d(r)\}\), where
\(d(r)=\overline{D_r(y-m(r))}>0\). The second derivative of \(\log C\) in \(k\)
is minus the variance of \(D_r\) under its normalized density.
The optimum has \(k\le 1/d(r)\) by the concentration derivative bound in the
earlier chart. Continuity and compactness give \(\min_{[0,1]} d>0\).
The upper bounds \(C\le k\) and \(C\le\sqrt{k/(2\pi)}\) exclude the small and
large \(k\) exteriors uniformly after profiling. The former follows by
integrating the exponential lower envelopes with the two limiting slopes,
and the latter follows from the Stirling expression. This gives a continuous
profile \(P(r)\) on the closed interval and two unique nuisance optimizers
at each ratio. [Harmonic chart and concentration bounds](2026-11-25-gbshape-math-report.md)

The endpoint shapes are unique. With \(g=\exp(\bar y)\), set
\[
 J_+=\log(\bar x/g),\qquad J_-=\log(g\,\overline{1/x}),\qquad
 H(k)=\log k-\psi(k).
\]
Solve \(H(k_\pm)=J_\pm\). The faces are gamma with shape \(k_+\) and scale
\(\bar x/k_+\), and inverse gamma with shape \(k_-\) and inverse-gamma
scale \(k_-/\overline{1/x}\). Their common shape objective differs only
through \(-k J_\pm\) on the log-data scale, hence the smaller \(J\) strictly
wins the pairwise comparison. Equality is exactly
\(\bar x/\overline{1/x}=g^2\). On rational observations this can be tested by
comparing \((\bar x/\overline{1/x})^n\) and \((\prod_i x_i)^2\) exactly.
Reciprocal reflection about \(g\) suffices, but the arithmetic test also
covers data without reflection.

Let \(V=\max(P(0),P(1))\). A finite MLE exists iff some interior stationary
ratio has \(P(r)\ge V\). Equivalently, any finite witness strictly above \(V\)
proves attainment. If no such strict witness exists, finite attainment is
exactly equality to \(V\) at an interior ratio. Every finite maximizer is
recovered from the maximizing ratios and their unique nuisance solutions.
The proof is continuity of the completed profile and interior stationarity.
The supremum is finite on every nonconstant sample. Face-only
nonattainment requires strict pointwise exclusion of interior equality.

This criterion can be checked by a scalar cover when its signs or bounds
separate. On \([r_l,r_u]\), lower-bound the data loss at any \(m\) with
\(D_{r_u}\) for \(y_i\ge m\) and \(D_{r_l}\) otherwise. The ordering follows
by integrating \(D''_r(z)=e^z/[1+r(e^z-1)]^2\) twice from zero. The resulting
piecewise loss is convex in \(m\), and a tangent gives a lower bound \(d_L\)
on its minimum. At midpoint \(r_0\) a cell upper is
\[
 2\max(r_0-r_l,r_u-r_0)+
 \sup_{k>0}\{\log C(k,r_0)-k\,d_L\},
\]
using the earlier \(|\partial_r\log C|\le2\). The supremum for \(d_L>0\)
has a concave concentration tangent bound. For stationary exclusion use
\(P'=F_r\) at the nuisance roots. For curvature use
\(P''=F_{rr}-F_{r\eta}F_{\eta\eta}^{-1}F_{\eta r}\) with \(\eta=(m,k)\).
The nuisance Hessian is diagonal there, since the mode score vanishes, and
both diagonal entries are strictly negative. A signed root bracket with
negative profile curvature proves one root, and a cover followed by the
strict height rule proves globality. An overlapping height interval alone
has no implication for equality or attainment. [Cell bounds](2026-11-25-gbshape-math-report.md) · [Comparison rule](2026-11-25-ties-math-report.md#what-this-changes-for-the-fitter)

**Proved fast face case.** The earlier fixed-power stationary exclusion
applies directly. With centered logs \(v_i\) and log span \(D\), if
\(0<D\le .01\) and \(|\overline{v_i^3}|>69D^4\), no finite stationary point
exists. Positive skew selects inverse gamma and negative skew gamma. For
example \((1,1,1+2^{-20})\) has \(\overline{v_i^3}=2D^3/27\) and
\(D<2^{-20}\), so \(2/27>69D\). Its exact reciprocals select gamma. Equality
in the skew test leaves this test undecided. [Proof of the uniform score exclusion](2026-11-25-fpareto-math-report.md#uniform-exclusion-of-finite-stationary-points)

## One shape held

### Exact threshold and a sufficient face test

**Proved.** Use the same mode and loss, and hold \(p>0\). The gap from the
matched gamma face at a fixed \(r\in(0,1)\) is
\[
 \Delta(p,r)=p[d(0)-(1-r)d(r)]-E_r(p),\qquad
 E_r(p)=-\tfrac12\log(1-r)+R((1-r)p/r)-R(p/r)>0 .
\]
The penalty \(E_r\) is decreasing in \(p\). To see this, use the Laplace
formula \(R(s)=\int_0^\infty e^{-st}g_0(t)\,dt/t\) with
\(g_0(t)=(e^t-1)^{-1}-1/t+1/2\). In the difference \(R(ap)-R(bp)\) for
\(0<a<b\), substitute \(u=pt\). The factor \(g_0(u/p)\) decreases with \(p\)
because \(g_0'(t)=t^{-2}-[2\sinh(t/2)]^{-2}>0\). It follows that
\(E_r(p)/p\) strictly decreases from infinity to zero.

For each \(r\) with \(d(0)-(1-r)d(r)>0\), let \(p_r\) be the unique shape
satisfying \(E_r(p_r)/p_r=d(0)-(1-r)d(r)\). Otherwise set \(p_r=\infty\).
Define \(p_c=\inf_{0<r<1}p_r\) from the data.

- \(p<p_c\) gives strict gamma dominance. At \(p>p_c\), some finite point
  beats gamma and a finite MLE exists.
- At \(p=p_c\), the finite maximizers are exactly the ratios with \(p_r=p_c\)
  and their corresponding scales. If the infimum is unattained at an
  interior ratio, the gamma face wins strictly.
- For nonconstant data \(0<p_c\le 1/\operatorname{Var}(x/\bar x)\). As
  \(r\to1\), the peak bound \(p(1-r)\) excludes this end. The sign formula below
  proves the upper bound on \(p_c\) and gives an interior attainment test
  for its equality case. Apply the same criterion to the reciprocal sample
  when \(q\) is held.

The lower bound follows from a direct sufficient condition. Let
\(R_x=(x_{\max}-x_{\min})/x_{\min}\). If \(p R_x^2\le 4\), the profile is
strictly increasing in \(q\) to the gamma face. Indeed at the optimal scale,
\(z_i=b/(b+x_i)\) has mean \(q/(p+q)\). The Jensen gap
\(\log\bar z-\overline{\log z}\) is at most
\(\operatorname{Var}(z)/(2z_{\min}^2)\le R_x^2p^2/[8(p+q)^2]\). Here the
range of the \(z_i\) divided by \(z_{\min}\) is
\((x_{\max}-x_{\min})/(b+x_{\min})\), and
\(b\ge q x_{\min}/p\). Applying the trapezoid inequality to
\((t+s)^{-2}\) on each unit interval gives
\(\psi_1(t)>1/t+1/(2t^2)\), hence
\(\psi(p+q)-\psi(q)-\log((p+q)/q)>p/[2q(p+q)]\). The profile derivative is
therefore positive even on the boundary \(pR_x^2=4\). For example
\(p=4\), \(x=(1,2)\) gives a strict, nonconstant gamma face.

### Vanishing leading coefficient

**Proved.** Normalize \(z=x/\bar x\), let \(m_j=\overline{z^j}\) and
\(v=m_2-1\), and write \(G(p)\) for the optimized gamma value per observation
on this normalized sample. At fixed \(p\) the scale-profile expansion is
\[
 F(p,q)=G(p)+A(p)/q+B(p)/q^2+O(q^{-3}),
 \qquad A(p)=p(pv-1)/2 ,
\]
\[
 B(p)=-\frac{p(p-1)(2p-1)}{12}
       +\frac{p^3m_2}{2}-\frac{p^3m_3}{3}+\frac{p^3v^2}{2}.
\]
For the proof, use rate \(a=q/(b/\bar x)\). The gamma objective as a
function of \(a\) has optimum \(a=p\) and second derivative \(-1/p\)
there. The raw coefficients of \(1/q,1/q^2\) before rate optimization are
\[
 C_1=\tfrac12p(p-1)-pa+a^2m_2/2,\qquad
 C_2=-p(p-1)(2p-1)/12+pa^2m_2/2-a^3m_3/3 .
\]
They follow by integrating the digamma expansion over \([q,q+p]\) and
expanding \((p+q)\log(1+az/q)\). These expansions and the needed nuisance
derivatives have uniform remainders on compact positive \(p,a\) ranges, by
the shifted digamma and elementary Taylor bounds. The scale equation
forces \(a=p+O(1/q)\) uniformly on such a \(p\) range.
The first displacement is \(p\,\partial_a C_1(p,p)/q\), giving the
optimized correction \(p(\partial_a C_1(p,p))^2/2=p^3v^2/2\).

When both shapes are free, write \(p_*=k_+\). After also profiling \(p\) in
the large-\(q\) strip, replace \(A\) by \(A(p_*)\) and \(B\) by
\[
 B_* = B(p_*)+\frac{A'(p_*)^2}{2(\psi_1(p_*)-1/p_*)}.
\]
The earlier joint exterior bounds retain a compact \(p\) range along any
sequence competitive with the face. The limiting strict gamma optimum
forces the maximizing \(p\) into its neighborhood, where the same
nonsingular nuisance expansion proves the displayed correction.
Reciprocation gives the other strip. A positive first nonzero coefficient
gives a finite witness above that face. If it is the better face, this
proves finite attainment. A negative first nonzero coefficient gives a
strictly losing tail neighborhood, and the finite region still needs
comparison. In the compact ratio chart the leading derivative is
\(A(p_*)/p_*\). At \(A(p_*)=0\) the quadratic coefficient is \(B_*/p_*^2\),
by \(1/q=r/[p(1-r)]\) and the same nuisance displacement. Use \(B(p)/p^2\)
with held \(p\).

For \(pv=1\), the held coefficient simplifies to
\(B=p(5-3p)/12-p^3\overline{(z-1)^3}/3\). Exact examples are

| Held \(p\) | Exact sample | \(A\) | \(B\) and consequence |
|---:|---|---:|---|
| \(64/49\) | \((1/8,15/8)\) | 0 | \(848/7203>0\). Finite attainment. |
| 1 | \(((1/10)^{[100]},(19/9)^{[81]})\) | 0 | \(13/135>0\). Finite attainment. |
| 1 | \(((1/2)^{[4]},3)\) | 0 | \(-1/3<0\). Strict losing tail. |

The first example also gives an exact non-unit finite/gamma tie at the
held shape \(p=p_c((1/8,15/8))\). Indeed, \(p_c\le64/49\), and the strict
finite witness at \(64/49\) persists for shapes slightly below it, so
\(p_c<64/49\). More generally when \(p_c<1/v\), \(A\) is uniformly
negative near \(p_c\) and excludes a neighborhood of \(r=0\). The peak
bound excludes \(r=1\) uniformly. Taking a limit of strict finite witnesses
from shapes above \(p_c\) within the remaining compact interval proves an
attained interior equality at \(p_c\). The definition specifies the exact
hold, independently of root rounding. [Moment and sign checks](2026-11-25-betapr-math-data/fixtures.txt)

### Held unit shape and a real equality

**Proved.** For \(p=1\), use the exact Lomax profile with
\(t=1/(b/\bar x)\), \(L(t)=\overline{\log(1+t z)}\), \(q=1/L(t)\).
The normalized gap to the exponential is \(\log(t/L(t))-L(t)\).
At \(m_2=2,\ m_3=9/2\) the linear and quadratic terms both vanish.
Expanding \(\log(t/L(t))\) gives cubic coefficient \(m_4/4-8/3\).
Both signs occur on rational data. With levels and counts
\((1/5,3375),(1/4,1886),(9/4,2898),(5/2,328)\) it is \(-97/960\).
With \((1/10,1500),(2,1274),(5,19)\) it is \(53/240\), proving finite
attainment in the latter case. The negative sign by itself excludes only the
tail. [Exact moment checks](2026-11-25-betapr-math-data/fixtures.txt)

On algebraic data the earlier Lomax face theorem excludes finite
stationary equality to the optimized exponential. It also excludes
algebraic positive stationary rates. Together with the earlier polynomial
for the derivative of the stationary equation, this gives a terminating
per-input existence test. Isolate the positive roots of that rational
derivative's numerator exactly. At each of these algebraic rates the
stationary equation has a nonzero sign, determined by increasing
precision. On the resulting monotone intervals isolate every stationary
rate, using the known zero at \(t=0\) and the negative sign at infinity.
Compare the enclosed profile heights to the exponential until signs
separate. No stationary height equals the face, so the existence decision
terminates. Comparing two finite heights still needs its own proof.
Two distinct levels give at most one finite global maximizer by the
prior degree and sign argument. General unit-shape uniqueness fails by
the prior exact Lomax tie. [Algebraic-data exclusion](2026-11-25-ties-math-report.md#unconditional-exclusion-of-a-finite-lomaxexponential-tie) · [Polynomial and finite tie](2026-11-25-lomax-uniq-math-report.md)

For an exact real finite/face equality, retain counts \(100,81\) and set
the two levels to \(1+s(1/10-1)\) and \(1+s(19/9-1)\). Let \(s_*\)
be the infimum of \(s\in[0,1]\) with a positive profile gap at some
positive rate. At small \(s\) the sufficient face test applies. At \(s=1\)
the positive quadratic term gives a strict finite witness, and continuity
moves it to \(s<1\). Thus \(0<s_*<1\). Since \(v=s^2\), the leading
coefficient at \(s_*\) is strictly negative, uniformly on a neighborhood,
and the large-rate decay is uniform because the two levels stay bounded
away from zero. The finite witnesses approaching \(s_*\) therefore give
an interior equality and no strict winner at \(s_*\). For a fixed rate,
\(L(t)\) strictly decreases in \(s>0\) by strict concavity and its zero
derivative at zero. The profile gap strictly increases. The fitted
exponential wins strictly for \(s<s_*\), and a finite fit strictly beats
it for \(s>s_*\). At \(s_*\) the two-level uniqueness theorem gives exactly
one finite maximizer. The algebraic-data exclusion proves \(s_*\) is
transcendental. **Numerical**, at 65 and 95 digits,
\(s_*\simeq0.97714966356758388264\) and \(t\simeq0.99070277533186400108\).
Exact reciprocation gives the inverse-exponential equality.
[Crossing replay](2026-11-25-betapr-math-data/fixtures.txt)

For held integer \(p\ge2\), **conditional on Schanuel's conjecture**, a
finite stationary point cannot tie the gamma on algebraic data. Here
\(U=\overline{tz/(1+tz)}\), \(q=p(1-U)/U\) is a nonconstant rational
function of \(t\), and stationarity gives
\(L=\sum_{j=0}^{p-1}1/(q+j)\). Algebraic \(t\) would make \(L>0\)
algebraic and \(e^{nL}=\prod_i(1+tz_i)\) algebraic, contradicting
Hermite–Lindemann. Thus both \(t\) and \(q\) are transcendental.
Equality to gamma would give
\(e^{(p+q)L-p}=(t/p)^p\prod_{j=0}^{p-1}(q+j)\).
The exponents \(L\) and \((p+q)L-p\) are rationally independent. A
dependence would force \((q+c)\sum_j1/(q+j)=p\) for a rational \(c\),
a nonidentity because the linear factor cancels at most one of the distinct
poles. Both exponents and their exponentials are algebraic over
\(\overline{\mathbb Q}(t)\), of transcendence degree one, contradicting
Schanuel's required degree two. This conditional result does not enter the
outcome rules.

## All-free exact samples

### Bounds for wide log data

**Proved.** Use symmetric log data \(Y=Tz\) with mean zero. Add \(\log T\)
to the mean log likelihood of \(Y\), put \(P=Tp,\ Q=Tq,\ \beta=\log b/T\),
and define \(L=\overline{(\beta-z)_+},\ R_+=\overline{(z-\beta)_+}\).
The scaled log objective lies between
\[
 H_\beta(P,Q)-(P+Q)\log 2/T
 \quad\text{and}\quad H_\beta(P,Q)+2pq,\qquad
 H_\beta(P,Q)=\log(PQ/(P+Q))-PL-QR_+ .
\]
Indeed, the softplus differs from the positive part by a number in
\([0,\log2]\). The log-beta correction above the harmonic normalizer is
\(\log\Gamma(1+p+q)-\log\Gamma(1+p)-\log\Gamma(1+q)\in(0,2pq)\), by
integrating positive decreasing trigamma. At any knot with \(L,R_+>0\),
the maximum of \(H_\beta\) is
\(-1-2\log(\sqrt L+\sqrt{R_+})\), achieved at
\(P=1/[\sqrt L(\sqrt L+\sqrt{R_+})]\) and the analogous \(Q\).
This follows by optimizing the rate sum and then the rate ratio.
For fixed rates the hinge criterion is affine between the knots and its
maximum occurs at a knot. At an extreme knot in the symmetric samples
with extrema \(\pm1\), \(L,R_+\) are 0 and 1 in some order. Under rate
ratio restrictions \(P/Q,Q/P\le K\), the hinge value there is at most
\(-1+\log(K/(K+1))\).

Each of the two optimized face values lies between
\[
 -1-(1+\log T)/T \quad\text{and}\quad -1-\log(1-\log(n)/T)
 \qquad (T>\max(1,\log n)).
\]
For the upper bound the log-gamma peak is at most its shape, by its
left exponential lower envelope, and \(J_\pm\ge T-\log n\).
For the lower bound take shape \(1/T\), use \(J_\pm\le T\), and use
\(\log\Gamma(1+1/T)\le0\) by convexity. At a reflection-fixed beta-prime
fit \(p=q,\ b=1\), the peak bound is \(p/2\) and the loss is
\(2p\,\overline{\log\cosh(Tz/2)}\ge p(T\overline{|z|}-2\log2)\). Optimizing
this upper bound gives
\(-1-\log[2(\overline{|z|}-2\log2/T)]\) when the bracket is positive.

A ratio restriction follows from the existing signed held-shape tail
inequality. On mean-normalized data \(x/\bar x\) both its maximum and
second moment are at most \(n\). For \(p\le\delta\), the inequality
\(F(p,q)\le G(p)+A(p)/q+C/q^2\) is strictly below \(G(p)\) whenever
\(q\ge Kp\), provided
\[
 K>2n,\qquad \delta(n-1)<1,\qquad
 K>\frac{1/4+2\delta n(1+2n)+2\delta n}{(1-\delta(n-1))/2}.
\]
This is substitution in the dossier's explicit \(C\) and \(A\).
It applies also to the reciprocal sample with shapes switched.
To bound the smaller shape, let \(H\) be the peak of the density of
\(Y/T\) and \(a_0=\min_\mu\overline{|z-\mu|}\). Log concavity gives
\(\ell+\log T\le\log H+1-Ha_0\). In fact, if the drop in log density
at \(z_i\) from the mode \(\mu\) is \(w_i\), integration of the log-linear
minorant gives \(1\ge H|z_i-\mu|(1-e^{-w_i})/w_i
\ge H|z_i-\mu|/(1+w_i)\), and averaging gives the stated bound.
The peak in \(Y\) increases in each shape since
\(\partial_p\log C=H_0(p)-H_0(p+q)>0\), with \(H_0(s)=\log s-\psi(s)\).
At equal shapes \(u\le1\) its value is at least \(u/4\), by splitting
the beta integral at \(1/2\). Thus a small scaled peak forces a small
minimum shape. [Signed tail and peak bounds](../../../univariateML-cpp/include/cppunivariate/distributions/betapr/README.md) · [Elementary bound checks](2026-11-25-betapr-math-data/fixtures.txt)

### Binary64 finite tie and binary64 face tie

**Proved.** Set \(T=512\log 2\) and \(z=(-1,(-1/2)^{[10]},(1/2)^{[10]},1)\).
At \(\beta=1/2\) the hinge losses are \(23/44,1/44\).
The finite witness \(P=8/5,\ Q=38/5\) gives scaled value above \(-.749\).
Both faces have values below \(-.991\). The best reflection-fixed value
is below \(-1.079\). Compactness gives at least one finite global maximum.
The exact transformation \((p,q,b)\mapsto(q,p,1/b)\) preserves the
likelihood because the observation logs are symmetric about zero.
An optimizer at \(p=q\) would use \(b=1\) by uniqueness of the scale root,
so a finite global maximum and its reflection have ratios on opposite
sides of \(1/2\). This is a structural global tie on the displayed
binary64 input.

For \(z=(-1,1)\) at the same \(T\), the two faces are exactly equal.
Any point reaching their level must have \(H<4\) by \(a_0=1\) and the face
lower bound. Hence \(\min(p,q)<16/T<.1\). With \(n=2,\delta=.1,K=6\),
the signed tail restricts both ratios to below 6. The hinge loss for any
\(\beta\) is at least \(\min(P,Q)\), hence the scaled finite upper bound is
\[
 -1+\log(6/7)+12(16/T)^2 < -1-(1+\log T)/T .
\]
This contradicts reaching the faces. It proves nonattainment with two
equal winning faces at this exact binary64 input. [Bound checks](2026-11-25-betapr-math-data/fixtures.txt)

### Exactly two finite maxima tied with both faces

**Proved.** Set \(T=10^6\) and use the exact real log sample
\[
 T(-1,(-s)^{[10]},s^{[10]},1),\qquad .81\le s\le .83 .
\]
The positive observations are its exponentials. Both faces have the same
height for every \(s\). A finite point competitive with the face has \(H<16\)
since \(a_0=(2+20s)/22\ge 1/2\). Thus \(\min(p,q)<64/T<.001\). With
\(n=22,\delta=.001,K=45\), both shape ratios are below 45, and the hinge
upper correction is at most \(\epsilon=90(64/T)^2\).
For a scale-profiled competitor, \(p,q\) lie in
\([e^{-2}/T,2880/T]\) by the peak bounds, and the log scale is in
\([-T-\log45,T+\log45]\) by the scale-root bracket. This fixed compact
rectangle contains every competitive finite maximum throughout the path.

At the positive interior knot \(\beta=s\) the losses are
\(L=s+(1-s)/22,\ R_+=(1-s)/22\). At \(s=.81\) the hinge optimum minus
its softplus allowance is greater than \(-.99544\), above the face upper
bound. At \(s=.83\), both interior knots are below \(-1.00634\) after
the correction. The two extreme knots are also below that value under the
ratio restriction. The lower face bound is above \(-1.000015\).
The maximum over the fixed compact rectangle is continuous in \(s\), as
is the common face height. Their difference crosses zero in this interval.

To count the maximizing branches, denote by \(A_s\) the hinge optimum
at \(\beta=s\). A finite global maximum with value at least the common
face must also beat \(A_s-15\log2/T\), the finite witness at the optimal
hinge rates. Up to reflection its \(\beta\) lies within \(10^{-3}\) of
\(s\). Here are explicit bounds for that localization. On the two
intervals adjacent to \(s\), the slopes of \(L,R_+\) are \((1/2,-1/2)\)
on the left and \((21/22,-1/22)\) on the right. On the collars of width
\(10^{-3}\), the derivative of \(\sqrt L+\sqrt{R_+}\) is less than \(-2.3\)
on the left and greater than \(1/4\) on the right. Its value is less
than 1.02 on those collars. Thus the two shifted endpoints lose at least
\(2(1/4)10^{-3}/1.02\) relative to \(A_s\), more than both allowances.
The value at \(\beta=0\) is at most
\(-1-\log(4(1+10s)/22)\) before the correction.
The restricted extreme knot bound also loses to the witness, using
\(\sqrt L+\sqrt{R_+}<1.009\) at \(s\). For fixed rates the hinge is affine
between these knots and shifted endpoints. All \(\beta\) outside the two
collars are therefore excluded.

Inside the positive collar \(L>.818,\ R_+>.0076\). For
\(u=P/(P+Q)\), optimizing the hinge in \(P+Q\) bounds it above by
\[
 -1+\log\{u(1-u)/[.818u+.0076(1-u)]\}.
\]
The ratio inside the logarithm first increases then decreases, with its
maximum at \(\sqrt{.0076}/(\sqrt{.818}+\sqrt{.0076})\).
At \(u=.055,.15\) it is already too low to reach the face after
adding \(\epsilon\). Hence \(.055<u<.15\). The peak bound gives
\(P+Q>1.4\). For \(P+Q\ge60\) the upper bound
\(\log(.1275(P+Q))-[.818(.055)+.0076(.945)](P+Q)+\epsilon\) decreases
and is below the face, so \(P+Q<60\). At a scale root, the mean logistic
coordinate is \(u\). All coordinates other than the repeated positive
knot contribute a total between .999 and 1.001, since each is separated
from \(\log b\) by at least \(.169T\). The logistic coordinate at the
repeated knot is therefore in \((.0209,.2301)\), giving
\(|h|=|\log b-sT|<4\).

The resulting region in \((P,Q,h)\) is convex because the ratio and sum
constraints are linear inequalities. The Hessian is strictly negative
definite throughout. In the shape block scaled on both sides by
\(\operatorname{diag}(P,Q)\), the harmonic term is
\(-I+(u,1-u)^{\mathsf T}(u,1-u)\), with largest eigenvalue
\(-2u(1-u)\). The log-beta correction adds at most \(2(60/T)^2\)
to this eigenvalue. For the Schur term from profiling \(h\), the
shape-location mixed derivatives in unscaled shape coordinates are
bounded by \(1/T\), and the magnitude of the location curvature is
\((P+Q)\overline{U_i(1-U_i)}/T\) for the logistic coordinates \(U_i\).
The repeated knot alone gives
\(\overline{U_i(1-U_i)}>10e^4/[22(1+e^4)^2]>.008\) throughout the region.
After the diagonal scaling the Schur correction is at most
\(60/(.008 T)\). Their sum is less than \(2(.055)(.945)\).
Thus there is at most one global maximizing point in the positive
collar. Reflection proves the same for the negative collar and exact
equality between their maximizing heights. This counts exactly two
global finite maxima at \(s=.81\) and at each finite/face contact.

The contact is unique in \([.81,.83]\). Near any zero of the compact
value gap, strict local curvature and the compact exclusion outside
the two maximizing neighborhoods give two smooth local branches,
with an identical value by reflection. Along either branch the value
derivative in \(s\) is \(-(10/22)(P+Q)(U_+-U_-)\) in the positive collar,
where \(U_\pm\) are the coordinates at \(\pm sT\).
Here \(U_+>.0209\), \(U_-<.001\), and \(P+Q>1.4\).
The derivative of the common face has magnitude at most
\(10T e^{-(1-.83)T}\), by differentiating \(J_+\) and using
\(k_+<1\) from its score. The gap has strictly negative derivative at
each zero, so it has exactly one zero. Define \(s_*\) by that zero.
The sample at \(s_*\) is an exact four-way equality in the completed
chart, with precisely two attained finite members. **Numerical**,
\(s_*\simeq .81819883570125213829\). The mathematical sample uses real
levels outside binary64 range.
[Uniform inequalities](2026-11-25-betapr-math-data/fixtures.txt) · [Crossing and stationarity replay](2026-11-25-betapr-math-data/ratio-scan.txt)

### Explicit all-free singleton slice

**Proved.** For \(z=(-1,0^{[4]},1)\) and any \(T\ge10^6\), the finite
witness \(\beta=0,\ P=Q=3\) gives a scaled lower bound
\(\log(3/2)-1-6\log2/T\), above both faces. For any competing point
\(H<16\) by \(a_0=1/3\). The same conservative ratio and error bounds
\(K=45,\ \epsilon=90(64/T)^2\) apply. On the positive side, outside
\(\beta=0.001\), the hinge can only improve up to its own optimum
there or at \(\beta=1\). Its value at \(0.001\) is bounded by
\(-1-2\log(\sqrt{1.005/6}+\sqrt{.999/6})\), and the unrestricted extreme
knot bound is \(-1\). Neither reaches the witness with the allowance.
Reflection then gives \(|\beta|<.001\).
Here \(L,R_+\ge.999/6\).
With \(u=P/(P+Q)\), the bound after optimizing the rate sum is
\(-1+\log[u(1-u)/(.999/6)]+\epsilon\). It excludes \(u\) outside
\((.4,.6)\). The peak bound forces \(P+Q>2\), and
\(\log((P+Q)/4)-(.999/6)(P+Q)+\epsilon\) excludes \(P+Q\ge12\).
The scale equation then gives the logistic coordinate at the middle
knot between .1 and .9, so \(|h|=|\log b|<3\).
On the convex region defined by these bounds the harmonic shape block,
scaled as above, has largest eigenvalue at most \(-.48\).
The two positive corrections to the Schur bound are at most
\(2(12/T)^2\) and \(12/(.03 T)\), since the four central observations
give \(\overline{U_i(1-U_i)}>.03\). Hence the Hessian is strictly
negative definite throughout. There is exactly one global maximum,
and reflection forces \(p=q,\ b=1\). The common shape is the unique
concentration solution at \(r=1/2\). [Bound checks](2026-11-25-betapr-math-data/fixtures.txt)

## Python verification and public coefficients

**Numerical.** The profile replay uses ratios
\(0,.025,.05,.1,.15,.2,.3,.4,.5\) at 55 and 85 decimal digits, with the
other half reflected for the symmetric inputs. It also solves the original
stationarity equations on the wide dyadic sample. One branch has
\(p\simeq .00443356197495835,\ q\simeq .0211898196144012,\
\log b\simeq178.386860661253\). The grid also probes a narrow symmetric
pair \(((1+2^{-20})^{-1},1+2^{-20})\), with both faces numerically above
the grid interior, and the unique wide central slice. These sampled
profiles do not certify a continuum or count the finite branches of
the wide dyadic example.

The coefficient replay reads the November 15 native and double
snapshots from revision `a1166cbf`, checks the original input hashes,
and independently solves all free scores simultaneously at each
archived estimate. It uses the exact dyadic observations, holds and
public values, with 95–334 decimal digits across two precisions and
negative local Hessians. Public scale is compared directly with the
joint root, without solving a different problem at rounded fitted
shapes. These are archived public outputs. The November 20 comparison
recorded zero differences for 212 beta-prime cases in each mode,
and the present math-only job did not invoke the fitter at HEAD.
The replay checks local coefficient accuracy. On the single-root hold
masks the root is the global maximum by the proof above. On the other
masks globality remains a separate claim.

| Archived case | Stress point | Largest relative coefficient error across modes |
|---|---|---:|
| golden | All free, 500 observations | \(1.07\times10^{-16}\) |
| 000178 | All free, scale about \(1.09\times10^{-11}\) | \(6.57\times10^{-17}\) |
| 000197 | Held shape1, free shape2 about 259 | \(8.76\times10^{-17}\) |
| 009874 | Held shape2, original-data reciprocal route | \(2.89\times10^{-17}\) |
| 000249 | Held scale, two concentrating shapes above \(10^{23}\) | \(3.46\times10^{-15}\) |
| 000256 | Held scale, shape2 about \(5.37\times10^{199}\) | \(9.03\times10^{-14}\) |
| 000272 | Held scale and shape1, shape2 about \(5.33\times10^5\) | \(4.02\times10^{-16}\) |

All three final Python runs passed on the workstation. Reproduce
sequentially from the worktree root.

```sh
d=dev/research/evaluations/2026-11-25-betapr-math-data
export PYTHONDONTWRITEBYTECODE=1
/tmp/umvl-cpu/run nice -n 10 python3 -u "$d/fixtures.py"
/tmp/umvl-cpu/run nice -n 10 python3 -u "$d/accuracy.py"
/tmp/umvl-cpu/run nice -n 10 python3 -u "$d/ratio_scan.py"
```

[Exact and interval checks](2026-11-25-betapr-math-data/fixtures.py) · [Coefficient replay script](2026-11-25-betapr-math-data/accuracy.py) · [Coefficient results](2026-11-25-betapr-math-data/accuracy.jsonl) · [Profile replay script](2026-11-25-betapr-math-data/ratio_scan.py) · [Profile results](2026-11-25-betapr-math-data/ratio-scan.txt) · [November 20 native comparison](2026-11-20-refine-fix-data/snapshot-native-diff.jsonl) · [November 20 double comparison](2026-11-20-refine-fix-data/snapshot-double-diff.jsonl)

## What this changes for the fitter

- Preserve constant-data dispatch and the single-root hold routes.
  For a held shape, the exact \(pR_x^2\le4\) inequality proves strict
  nonattainment, including equality in that inequality. For all-free
  narrow data use the signed log-skew condition to prove the face rather
  than choose it by a leading-order margin. Compare the two face values
  through the positive-data means or the exact algebraic predicate.
- In the unit-shape route, the stationary polynomial and algebraic-data
  theorem provide an existence certificate when the ordinary comparison
  is close. For non-unit/general tails use the second coefficient on
  the zero seam after enclosing the first. A positive tail coefficient
  on the best face proves finite attainment. A negative one excludes the
  collar, with global decision still based on the remaining cover.
- The one-dimensional ratio cover can serve as an all-free alternative
  to the shape rectangle. Retain independent identities for distinct
  stationary regions. Use `min(iterlim,100000)` for each solver and
  `128 * min(iterlim,100000)` for certificate evaluations. Validate
  coefficient boxes in physical \((p,q,b)\), including public rounding
  and reciprocal transport. A small likelihood difference or score norm
  alone does not establish coefficient accuracy.
- A strictly winning certified single face gives `GammaBoundary`,
  `InvGammaBoundary`, `ExpBoundary`, or `InvExpBoundary` as appropriate.
  Proved equal winning faces with no finite fit give certified
  nonattainment and both limit witnesses. At a finite/face equality
  return the finite maximizing member or set, with any tied faces in
  diagnostics. A proved singleton has `certified=TRUE, unique=TRUE`.
  The full proved reflected pair has `certified=TRUE, unique=FALSE`
  and both parameter vectors. If an exact orbit proves multiple members
  but the complete set is unresolved, retain that fact in diagnostics
  pending complete-set certification.
- If finite globality and coefficient accuracy are proved while count
  remains open, use `certified=TRUE, unique=NA`. If a cover cannot decide
  globality within budget, keep a successful accuracy-checked unrefuted
  search as `certified=FALSE, unique=NA`. A better reachable face or
  separated finite witness can refute that candidate. An actual search
  failure retains its failure status. [Accuracy contract](../../design/accuracy-contract.md) · [Fallback rule](../../validation/diagnostics.md#wrapper-failure-handling-rule-r6) · [Typed outcome rule](2026-11-25-ties-math-report.md#what-this-changes-for-the-fitter)

## Open

- The dyadic finite tie proves at least two separated global maximizers.
  Its complete finite cardinality still needs a cover that excludes every
  other ratio patch and certifies the two finite root boxes. The proof of
  exactly two on the wide real deformation uses a larger logarithmic
  span and does not give that exclusion on the dyadic sample.
- The exact held-shape threshold and all-free scalar criterion reduce
  arbitrary existence to stationary and endpoint comparisons. A finite
  decision procedure that resolves accidental finite/finite or
  finite/face equality outside the proved structural and algebraic
  cases is open. In particular the rational-data non-unit exclusion is
  conditional for positive integer holds above one, and no unconditional
  general free-shape equality test is established.
- When both tail coefficients vanish in the non-unit or all-free problem,
  a higher-order signed profile expansion or an independent collar bound
  is needed. The second-order expansion proves eventual signs on its
  scope. Per-input quantitative remainder constants and production
  margins through the shape and scale reconstructions still need
  validation in both arithmetic modes.
- The archived replay does not provide current production certificates on
  the new wide binary64 samples. Their candidate root count, arithmetic
  margins, public coefficient tolerances, and full typed result payloads
  should be checked when the new decisions are implemented.
