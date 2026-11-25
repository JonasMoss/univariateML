# Slash likelihood at an exactly-half tie

**Proved.** With location and scale free, either sign of a nonzero reciprocal sum gives a finite MLE that beats collapse. When the sum is zero, all three outcomes occur. A finite MLE can beat collapse, collapse can be the unattained supremum, and an attained finite maximum can equal the collapse value. The zero-sum case reduces to comparison with a finite set of stationary profile values on a sample-dependent compact set.

## Setup and collapse expansions

Let \(n=2k\), with largest multiplicity \(k\). At a value \(v\) of that multiplicity write \(\delta=m-v\), and let \(d_1,\ldots,d_k\) be the off-point residuals \(x_i-v\), with repetitions. Put
\[
S=\sum_i d_i^{-1},\qquad Q=\sum_i d_i^{-2},\qquad
a=\min_i|d_i|,\qquad r=\#\{i:|d_i|=a\}.
\]
Use \(A(t)=(1-e^{-t})/t\), with \(A(0)=1\). The slash density is \(cA((x-m)^2/(2s^2))/(2s)\), where \(c=(2\pi)^{-1/2}\). The vertical collapse value is \(B_v=c^n2^{-k}\prod_i d_i^{-2}\). In a neighborhood of \(v\), the exact log ratio is
\[
F(\delta,s)=\log(L/B_v)
=k\log A\!\left(\frac{\delta^2}{2s^2}\right)
-2\sum_i\log(1-\delta/d_i)
+\sum_i\log\!\left(1-e^{-(d_i-\delta)^2/(2s^2)}\right).
\tag{1}
\]
The previous analyses establish the collapse limits, confinement of location to the sample range, the unique scale root away from half-mass locations, and the profile bounds used below. [Slash dossier](../../../univariateML-cpp/include/cppunivariate/distributions/slash/README.md), [likelihood geometry](2026-10-29-slashbhatt-report.md), [held-scale certificate](2026-11-01-slash-cert-report.md), [free-scale certificate](2026-11-02-slash-free-report.md), [TASK-117](2026-10-31-open-existence-report.md#3-slash-exactly-half-tied).

**Proved expansion.** For fixed \(b\), use \(\log A(t)=-t/2+t^2/24+O(t^4)\) and the Taylor series of the off-point logarithms in (1). With \(\delta=bs^2\) this gives
\[
F(bs^2,s)=\left(2bS-\frac{k b^2}{4}\right)s^2+
\left(b^2Q+\frac{k b^4}{96}\right)s^4+O(s^6).
\tag{2}
\]
The exponential terms are smaller than every power of \(s\), since \(|d_i-bs^2|\ge a/2\) for small \(s\). The order-\(s^2\) term is TASK-117's expansion. When \(S=0\) it is negative for \(b\ne0\). At \(b=0\) every algebraic term vanishes and
\[
F(0,s)=\sum_i\log(1-e^{-d_i^2/(2s^2)})
=-r e^{-a^2/(2s^2)}+o(e^{-a^2/(2s^2)}).
\]
The same leading relative loss holds along the best local location at fixed small \(s\). To see the effect of allowing that location to shift, let \(E_i=e^{-d_i^2/(2s^2)}\) and \(H=\sum_i d_i E_i/(1-E_i)\). At \(S=0\), (1) gives \(F_\delta(0,s)=-H/s^2\). For sufficiently small fixed \(\epsilon>0\), \(F\) is strictly concave in \(\delta\) for \(|\delta|\le\epsilon s\). Its curvature there is \((-k/2+O(\epsilon^2))/s^2+O(1)\) by differentiating (1). On \(\epsilon s\le|\delta|\le\eta_1\) for sufficiently small fixed \(\eta_1\), the bound \(k\log A(\epsilon^2/2)+4Q\eta_1^2<0\) keeps the objective bounded away from zero. Here the \(4Q\) bound follows from the Taylor estimate below. Since \(F(0,s)\to0\), the maximizing shift in this strip is interior and is \(O(|H|)\) by the mean value theorem. On the segment to that shift, the curvature is \(-k/(2s^2)+O(1)\). Applying the mean value theorem to the score and Taylor's theorem to the value gives
\[
\delta_s=-\frac{2H}{k}(1+O(s^2)),\qquad
F(\delta_s,s)=F(0,s)+\frac{H^2}{k s^2}(1+O(s^2)).
\]
When \(H=0\) the shift and improvement are exactly zero. The improvement in this formula is exponentially smaller than the vertical loss.

**Proved expansion of the scale profile.** At nonzero \(\delta\) close to zero, let \(s=s(v+\delta)\) be the conditional scale root. The scale score at \(\delta=0\) is negative at every fixed \(s>0\). By continuity and strict decrease of the score in scale, \(s(v+\delta)\to0\). At the root, with \(t_0=\delta^2/(2s^2)\) and \(t_i=(d_i-\delta)^2/(2s^2)\), the equation is
\[
k\bigl(2-2t_0/(e^{t_0}-1)\bigr)=2\sum_i t_i/(e^{t_i}-1).
\]
Since the right side tends to zero, \(t_0\to0\). Expanding the left side as \(kt_0(1+O(t_0))\) gives
\[
\delta^2=\frac{2}{k}\sum_i (d_i-\delta)^2 e^{-t_i}(1+o(1)).
\]
This first implies \(\delta/s^2\to0\) by the exponential bound from \(|d_i-\delta|\ge a/2\). It then implies \(\delta^2=(2ra^2/k)e^{-a^2/(2s^2)}(1+o(1))\), with contributions from \(|d_i|>a\) of smaller order. Taking logarithms and substituting in (1) with \(S=0\) gives
\[
\begin{aligned}
\frac{a^2}{2s^2}&=2\log(a/|\delta|)+\log(2r/k)+o(1),\\
F(\delta,s(v+\delta))&=-\frac{k}{a^2}\delta^2\log(a/|\delta|)
+\left[Q-\frac{k}{2a^2}\bigl(1+\log(2r/k)\bigr)\right]\delta^2
+o(\delta^2).
\end{aligned}
\tag{3}
\]
In the second line the off-point exponential loss is \(-k\delta^2/(2a^2)+o(\delta^2)\), and the remainder of the tied log term is \(O(\delta^4/s^4)=o(\delta^2)\). The same two terms give the relative likelihood change \(L/B_v-1\), because the square of the log change is \(o(\delta^2)\).

## Complete sample criterion

**Proved nonzero-sum case.** If there are two half-mass values, their collapse values agree and both sums are nonzero. Otherwise the half-mass value is unique. In either case choose a largest-collapse value with nonzero \(S\). The choice \(b=4S/k\) in the TASK-117 term of (2) gives a strictly positive coefficient \(4S^2/k\). A finite likelihood then exceeds the largest escape value, so the maximum is attained and strictly wins. For a single tie, \((0,0,-1,2)\) has negative \(S\) and its reflection has positive \(S\).

**Proved zero-sum case.** There is one half-mass value. The following strip can be removed from the global comparison for all positive scales. To construct it, use \(A(t)\le (1+t/2)^{-1}\), strict for \(t>0\), since \(\tanh(t/2)<t/2\) for \(t>0\). For \(|\delta|\le a/2\), Taylor's theorem bounds \(-2\log(1-\delta/d_i)\) above by \(2\delta/d_i+4\delta^2/d_i^2\). The linear terms cancel. Dropping the strictly negative off-point exponential terms in (1) yields
\[
F(\delta,s)<-k\log(1+\delta^2/(4s^2))+4Q\delta^2.
\tag{4}
\]
Choose \(s_0=\sqrt{k/(64Q)}\) and \(\eta_0=\min(a/2,\sqrt{k/(16Q)})\). For \(s\le s_0\), \(|\delta|\le\eta_0\), and \(0<|\delta|\le2s\), the log in (4) is at least \(\delta^2/(8s^2)\). For \(|\delta|>2s\) it exceeds \(\log 2\), while \(4Q\delta^2\le k/4\). At \(\delta=0\) strictness follows from (4) itself. For \(s\ge s_0\), \(F(0,s)\le F(0,s_0)=-\gamma<0\) by monotonicity on the vertical path. The held-scale bound from the dossier makes \(F(\cdot,s)\) \(n\sqrt 2/s_0\)-Lipschitz for these scales. Therefore
\[
\eta=\min\bigl(\eta_0,\gamma s_0/(2n\sqrt2)\bigr)>0
\quad\Longrightarrow\quad
F(\delta,s)<0\;\text{for all }|\delta|\le\eta,\ s>0 .
\tag{5}
\]

Here is the finite sample condition. Let \(J=[\min x_i,\max x_i]\setminus(v-\eta,v+\eta)\). At each \(m\in J\) define \(s(m)>0\) by the unique scale root
\[
\sum_{i=1}^n\left[1-\frac{2t_i}{e^{t_i}-1}\right]=0,\qquad
t_i=\frac{(x_i-m)^2}{2s(m)^2}.
\]
Let \(C\) be the set of roots in \(J\) of the scalar equation
\[
\sum_{i=1}^n \frac{2}{x_i-m}
\left[1-\frac{t_i}{e^{t_i}-1}\right]=0,
\tag{6}
\]
where the contribution at \(x_i=m\) is zero by continuity. Put \(M=\max_{m\in C}\log(L(m,s(m))/B_v)\), using \(M=-\infty\) for an empty set.

| Sample condition | Outcome |
|---|---|
| \(M>0\) | Finite MLE, strictly above collapse |
| \(M=0\) | Finite MLE at a maximizing candidate, with the same value reachable by collapse |
| \(M<0\) | Unattained supremum \(B_v\) |

For finiteness of \(C\), the log-scale score is real analytic at every positive scale, including at zero residuals by removable extension. Its derivative in log scale is strictly negative for a nonconstant sample, since the score summand is strictly increasing in \(t_i>0\). The implicit function theorem makes \(s(m)\) and the profile analytic on each side of \(v\), including at observations of lower multiplicity. The profile derivative is (6) by the chain rule and the scale equation. The profile is strictly below \(B_v\) in (5) and tends to \(B_v\) at \(v\) from each side by (3). Hence it is nonconstant on each analytic component. The derivative can have only finitely many zeros on the compact set \(J\), by the identity theorem. Any profile reaching or exceeding \(B_v\) has a maximum in \(J\). The inner endpoints have value strictly below \(B_v\), and at either extreme observation the location score points strictly into the sample range because the density decreases strictly with absolute residual. The maximum therefore solves (6). This proves the three-way comparison. Enumerating the roots and comparing their values in this condition are exact mathematical operations.

There is also a compact rectangle bound for use in the comparison. For \(m\in J\), put \(h=\min(\eta,\Delta/2)\), where \(\Delta\) is the minimum separation of distinct observations. At most \(k-1\) observations have residual magnitude less than \(h\). Each density divided by \(c\) is bounded by \(1/(2s)\) everywhere and by \(s/h^2\) for the other residuals. Thus at \(s\le1\),
\[
L/B_v\le 2^k\!\left(\prod_i d_i^2\right)\max(1,h^{-2})^n s^2 .
\]
For large scales the peak bound gives \(L/B_v\le 2^{-k}(\prod_i d_i^2)s^{-n}\). Scales for which either applicable bound is strictly below one can be discarded. On the remaining positive-scale compact set, subdivision bounds converge uniformly as the rectangles shrink, for example by uniform continuity and bounding each density at its nearest residual and over the scale interval. A strictly collapsing sample has a strictly separated upper bound there.

## Exact examples of all three outcomes

**Proved strict collapse.** The sample \((0,0,-1,1)\) collapses. More generally, a half-mass sample whose off-point residuals pair as \((d,-d)\) has strict collapse. The inequality for \(A\) used in (4) bounds the density by \(2cs/((x-m)^2+4s^2)\). Group two tied factors with each pair \((d,-d)\) in the ratio \(L/B_v\). For \(u=4s^2/d^2>0\) and \(w=\delta^2/(4s^2)\ge0\), the upper bound for this group is the reciprocal of
\[
D=(1+w)^2[1+2u(1-w)+u^2(1+w)^2].
\]
For \(w\le1\), \(D\ge(1+w)^2\ge1\). For \(w>1\), completing the square gives \(D=4w+[u(1+w)^2-(w-1)]^2>1\). The density bound is strict at each nonzero residual, so the finite likelihood is strictly below \(B_v\).

**Proved strict finite win.** The TASK-117 sample with ten zeros, one \(-1\) and nine \(9\)'s has \(S=0\). At \(m=4,\ s=3\), exact rational lower bounds give \(L/B_v>1.456\). The witness is enough to prove existence and a strict win over collapse.

**Proved equality with an attained finite maximum.** Write \(z^{[j]}\) for \(j\) copies of \(z\), and take
\[
X(e)=\left(0^{[60]},-1,(-1/e)^{[29]},b(e)^{[30]}\right),\qquad
b(e)=30/(1+29e),\qquad e_0=1/10000\le e\le1 .
\tag{7}
\]
For every \(e\), the reciprocal sum is \(-1-29e+30/b(e)=0\), and zero is the unique half-mass value. At \(e=1\) the sample pairs as above, so it strictly collapses. At \(e=e_0\), exact rational bounds at \(m=b(e_0)/5,\ s=3b(e_0)/10\) give \(L/B_v>1\).

The compact comparison can be made uniform in \(e\). Every off-point magnitude is at least one, so \(Q\le k\). The proof of (5) works for all these samples with \(s_0=1/8,\ \eta_0=1/4\), and with the uniform lower bound \(-\log(1-e^{-1/(2s_0^2)})\) on \(\gamma\) from the fixed \(-1\). The sample range is inside \([-10000,30]\), and the peak bound holds uniformly since every off-point magnitude is at most \(10000\). Outside the strip \(|m|<\eta\), all 60 zeros and the 30 observations with sign opposite to \(m\) have residual magnitude at least \(\eta\). This bounds the normalized likelihood uniformly by a constant times \(s^{60}\) for \(s\le1\), by using the tail bound on those 90 observations and the peak bound on the other 30. These bounds give a common compact set \(K\) containing every candidate with \(L/B_v\ge1\) inside the common location range. Locations outside the common range are dominated by projecting onto it. The function \(G(e)=\max_K L/B_v\) is continuous by uniform continuity on the product of two compact sets. It obeys \(G(e_0)>1\) and \(G(1)<1\).

The following rational-limit specification gives the exact sample \(X(e_*)\). For each \(N\ge1\), inspect the dyadic triples \((e,m,s)\) with denominator \(2^N\), \(e\in[e_0,1]\), \(|m|\le N\), and \(0<s\le N\). Use the exact normalized product
\[
A(m^2/(2s^2))^{60}\ \prod_{d\ne0}\frac{d^2}{2s^2} A((d-m)^2/(2s^2)),
\]
where the product runs over the off-point observations of (7). In every factor replace \(A(t)\) by \(\sum_{j=0}^{2N-1}(-t)^j/(j+1)!\). Retain the triple if the replacements are positive and the resulting rational product exceeds one. Let \(e_N\) be the largest \(e\) retained through step \(N\), with \(e_0=1/10000\) also included in each maximum. Set \(e_*=\lim_{N\to\infty}e_N\).

The polynomial replacements are lower bounds by integrating the Taylor lower bound for \(e^{-tu}\) over \(0\le u\le1\). They converge at each fixed argument. Any strict win has a neighborhood of strict wins in the three coordinates, so a dyadic triple in that neighborhood is eventually retained. It follows that \(e_*=\sup\{e:G(e)>1\}\). The set of strict wins contains a neighborhood of \(e_0\) in the parameter interval and is bounded away from 1. Continuity gives \(G(e_*)=1\). The maximum over \(K\) is achieved at positive scale, while \((m,s)=(0,s)\) with \(s\downarrow0\) also approaches \(B_v\). This proves the equality for the exact real sample defined above.

## Verification

The [verification script](2026-11-25-slash-math-data/verify.py) passed on the workstation through the single-slot wrapper:

    TMPDIR=$PWD/build/tmp /tmp/umvl-cpu/run python3 -u dev/research/evaluations/2026-11-25-slash-math-data/verify.py

The two positive witnesses use rational arithmetic. For \(1-e^{-t}\) the script uses the degree-40 Taylor lower bound at \(\min(t,4)\) when \(t<20\). For \(t\ge20\), it uses \(1-1/(\sum_{j=0}^{40}20^j/j!)\). These follow from Taylor's remainder sign and monotonicity. The script also verifies the completed-square identity symbolically.

**Numerical.** At 80 decimal digits the two witness ratios were \(1.45605346345727598\) and \(2.03780074066131713\). At \(\delta=10^{-6},10^{-12},10^{-24}\) in the ten-zero sample, the scale-profile gaps divided by their leading term in (3) were \(0.96989933732,\ 0.984950608027,\ 0.992475304015\). Subtracting that term and dividing by \(\delta^2\) gave \(4.15856023057,\ 4.15830067382,\ 4.15830067328\). The proof of the equality sample uses continuity and rational lower bounds.

## What this changes for the fitter

- A verified nonzero sum at a largest-collapse half-mass value proves strict finite attainment. A finite witness above the boundary also suffices. Certifying the returned coefficients still requires the interior global comparison and the usual accuracy checks.
- With \(S=0\) verified, use (5) and the scale bounds to exclude the corner and compare on a compact set. The exact paired case gives an immediate sufficient collapse condition. A strict upper bound below \(B_v\) on the compact complement gives a proved free-location **CollapseLimit**, mapped to unattained nonexistence with the collapse reason.
- If a finite global maximizer is proved to equal \(B_v\), return the finite estimate. A boundary equality alone supplies no additional finite coefficient pair. Preserve any independently proved multiplicity of finite maximizers.
- When the required sign or global comparison is unresolved, including certificate budget exhaustion, retain the existing successful search result with **certified = FALSE** if it passes the applicable accuracy checks and a separated likelihood bound has not refuted it. Stop on a failed search, a refuted candidate without a valid replacement, or proved nonexistence. A small likelihood gap by itself cannot establish collapse or equality.

## Open

A closed inequality on the residuals for the general zero-sum case remains open. The criterion above requires a finite stationary-value comparison. Strict separation supports subdivision certificates, but a general practical certificate for exact equality would require a symbolic equality test or another exact structural argument. The equality example specifies an exact real by a rational limit. Its construction gives no convergence rate for that limit.
