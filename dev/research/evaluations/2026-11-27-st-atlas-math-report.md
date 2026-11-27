# Azzalini skew-t existence atlas and finite likelihood ties

TASK-403 · 2026-11-27 · base fc24c05ba5cc8eac994292af2e71f9cde1f3592c

On the fitted domain \(\nu\ge1\) for free df, the escape list is complete for the nine atlas masks. The attainment test compares a specified envelope maximum with the likelihood at finite points, or equivalently at the finite score candidates, including the attainable restriction \(\nu=1\). Critical collapse with location and df held is always unattained. Two exact constructions refute uniqueness with all coefficients free, df held, scale held, and in both masks that hold slant and location. The all-free counterexample has strict atom masses. Its two reflected fits therefore differ from the balanced Student-Cauchy continuum.

The proofs use the direct location \(\xi\), scale \(w=\omega\), slant \(\alpha\) and df \(\nu\) of the [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/st/README.md). Held df may be any strictly positive number. The [atlas proof inventory](../existence-atlas/proofs.csv) already records unrestricted all-free divergence and the scale-only existence and face results. The earlier [certificate report](2026-11-08-st-cert-report.md#mathematical-justification) proves the symmetric scale score, held-endpoint sign rule and Cauchy continuum. The [closure study](2026-11-07-skew-theory-data/proofs.md#14-closing-the-st-collapse-equality-layers-by-sign-cases) proves the critical-layer reduction. The [skew-normal report](2026-11-25-sn-math-report.md#azzalini-skew-t) proves the strict slant wing cutoff, exact held-location collapse formula and uniform normal approximation. The [df report](2026-11-25-st-math-report.md#from-the-signed-tail-to-a-global-decision) proves signed normal and critical-df collars and an exact finite/normal equality. The [original implementation report](2026-11-01-skewt-report.md#results-and-decisions) documents the conservative equality stop. The Lean map supplies no **st** theorem for these questions. [SkewLSSN](../../lean/UnivariateML/Family/SkewLSSN.lean) and [FinishSN](../../lean/UnivariateML/Family/FinishSN.lean) treat the skew-normal component, including moving half-normal contact and a stationary-value criterion. Their Gaussian nuisance concavity and positive-gap hypotheses cannot supply a skew-t uniqueness proof.

## Joint envelope and attainment comparison

All formulas below use the likelihood product \(L\). The log-likelihood envelope is its logarithm with \(\log 0=-\infty\). Let \(c_v=t_v(0)\), \(d_i=x_i-\xi\), \(z_i=d_i/w\), and
\[
 f_i=\frac{2}{w}t_\nu(z_i)\,T_{\nu+1}\!\left(\alpha z_i\sqrt{\frac{\nu+1}{\nu+z_i^2}}\right).
\]
Compactify the free \(\xi,\alpha\) by their two infinities, \(w\) by zero and infinity, and free \(\nu\) by infinity. The holds are fixed throughout. A letter below denotes a set of boundary points and its envelope values. Its contribution to the maximum is zero if it is absent.

- **Zero likelihood.** At \(w=\infty\), the bound \(2c_\nu/w\le 2\phi(0)/w\) applies to each observation. At \(|\xi|=\infty\), each density is bounded by \(C/|x_i-\xi|\), uniformly in scale. Indeed \(\sup_z |z|t_\nu(z)\) is bounded for fixed \(\nu>0\), and \(t_\nu(z)\le(1+z^2)^{-1}\) works uniformly for \(\nu\ge1\) by the [Student envelope](2026-11-07-skew-theory-data/proofs.md#8-uniform-nuisance-compactness-and-complete-st-skew-face-values). These bounds also apply to intersections with other escapes.
- **\(N\), skew-normal.** At finite \(\alpha,\xi\) and positive finite scale, free \(\nu\to\infty\) replaces \(t_\nu\) and \(T_{\nu+1}\) by \(\phi,\Phi\), and the skew argument tends to \(\alpha z_i\). This is the same-hold skew-normal likelihood and is reachable by increasing only df. The earlier uniform normal bound also applies as slant varies. For fixed finite slant and moving location the positive-scale normal face has a unique optimum on nonconstant data. In \((s,b)=(1/w,\xi/w)\) its log likelihood is \(n\log s-\sum_i(sx_i-b)^2/2+\sum_i\log\Phi(\alpha(sx_i-b))+\mathrm{const}\), strictly concave by the Gaussian kernel and concavity of \(\log\Phi\). With location held the restriction \(b=s\xi\) has the same strict concavity and its maximum exists when some residual is nonzero, by the Gaussian bound as \(s\to\infty\) and the Jacobian as \(s\to0\).
- **\(H_+,H_-\), oriented half-Student.** At positive finite scale and finite df, free \(\alpha\to\sigma\infty\) for \(\sigma=\pm1\) gives zero if any \(\sigma d_i<0\). Otherwise the envelope is the base Student product times \(2^n\) for moving location, and times \(2^{n-k}\) for held location with \(k\) contacts. The moving-location envelope is sharp at a contact by taking \(\xi=\xi_*-\sigma|\alpha|^{-1/2}\), so all touching residuals approach from the favorable side and their skew arguments diverge. Held contacts have skew argument identically zero. Interior support points give the same formula without contacts. This is the exact contact argument of the closure study, which also gives the pointwise contact factor \(2T_{\nu+1}(r\sqrt{(\nu+1)/\nu})\) when \(\alpha d_i/w\to r\). For moving location the greatest value at any fixed scale and df is at the corresponding data extreme, by strict decrease of the base density with residual magnitude on the supported side.
- **\(G_+,G_-\), oriented half-normal.** When both df and slant escape at positive finite scale the same rule as for \(H_\sigma\) holds with the normal base and contact factor \(2\Phi(r)\) for finite \(r\). The skew argument divided by \(\alpha z_i\) tends uniformly to one on a bounded standardized residual set. The uniform CDF approximation, or tightness of the Student variables and a separate bounded-argument limit, applies when the argument diverges. The moving contact path above realizes the envelope jointly in the two shape coordinates. The positive-scale faces are also limits as probability laws, by pointwise convergence away from a contact and equality of the density integrals (Scheffé's lemma).
- **\(C\), scale zero.** The following rules give the envelope at each such point, including intersections with \(N,H,G\). They matter only when scale is free.

### The scale-zero boundary

A location limit \(y\) is feasible only if it respects the location hold. If \(y\) misses the data, \(L\to0\). Otherwise put \(k=\#\{x_i=y\}\), \(p=n-k\), \(e(v)=vp-k\), and \(t=(\xi-y)/w\), with \(t=0\) when location is held. If \(\nu\to v<\infty\), the off-atom base factors are asymptotic to \(w^\nu c_\nu \nu^{(\nu+1)/2}|x_i-y|^{-(\nu+1)}\) even when \(t\) is unbounded, since \(\xi\to y\). The atom factors are exactly \(t_\nu(t)/w\) before skewing. All skew factors are at most two. Consequently the product is bounded above by a constant times \(w^{e(\nu)}t_\nu(t)^k\).

If \(v=\infty\) and \(p>0\), choose a real \(q>n/p\). For \(\nu\ge q\), monotonicity of \(\nu\log(1+z^2/\nu)\) gives \(t_\nu(z)\le\phi(0)(1+z^2/q)^{-q/2}\). Thus \(L=O(w^{qp-n})\to0\) because the \(p\) off-atom distances are bounded away from zero. The same argument applies when no data are at the limit. If \(p=0\) the envelope is infinite even at infinite df, by taking \(\xi=y\) and \(w\to0\). These collapse paths have weak limit \(\delta_y\) since the standardized laws are uniformly tight with df bounded away from zero.

For finite \(v\), \(e(v)>0\) gives zero. If \(e(v)<0\) the envelope is infinite. One can set \(\xi=y\), take the limiting df fixed and use fixed slant, or, for an infinite-slant boundary point, send \(|\alpha|\) to infinity slower than any power of \(1/w\), for example \(\log\log(1/w)\). Off-atom CDF losses are then only polynomial in \(\log\log(1/w)\), by integrating the Student density over \([a,2a]\) for a negative argument of magnitude \(a\ge1\). The atom factors have no slant loss. If \(e(v)=0\), df is free and \(v>1\), the envelope is also infinite: choose \(\nu=v-1/j\) and \(w=\exp(-j^2)\) to obtain a factor \(\exp(pj)\), with fixed or slowly diverging slant as appropriate. These statements describe the unrestricted approach inside the fitted domain, including in samples whose global supremum is already infinite.

The remaining critical case \(e(v)=0\) has \(v\) held or \(v=1\) at free df. An unbounded \(t\) gives zero. With finite slant \(a\), the envelope at fixed \(y,v,a\) is the maximum over \(t\in\mathbb R\) (or \(t=0\) under a location hold) of
\[
 C_y(t,a)=
 \left[2t_v(t)T_{v+1}\!\left(-a t\sqrt{\frac{v+1}{v+t^2}}\right)\right]^k
 \prod_{x_i\ne y}\left[
 2c_v v^{(v+1)/2}|x_i-y|^{-(v+1)}
 T_{v+1}\!\big(a\,\operatorname{sgn}(x_i-y)\sqrt{v+1}\big)
 \right].                                                     \tag{1}
\]
This is the prior critical formula with the holds imposed. In the free-df case a convergent rate \(\lambda=(\nu-1)|\log w|\) multiplies the layer value by \(\exp(-p\lambda)\), with zero at \(\lambda=\infty\). The maximal factor one is feasible at \(\nu=1\). At infinite slant a wrong-side off-atom observation makes the envelope zero. If all off-atom observations are on the favorable side, use the base constants in (1), with \(t_v(0)^k\) at the atom, and multiply by \(2^n\) for moving location or \(2^p\) for held location. The moving-location bound is reached with \(t\to0\) from the favorable side of the residual \(-t\) and \(|\alpha t|\to\infty\); held location forces \(t=0\). At a finite nonzero \(t\), the tied skew factors tend to two or zero according to the sign of \(-\alpha t\). If \(t\to0\) and \(-\alpha t\to r\) they instead tend to \(2T_{v+1}(r\sqrt{(v+1)/v})\). These alternatives, with the df rate multiplier when present, exhaust the layer after subsequences. The off-atom standardized skew argument divided by \(\alpha\) tends to the signed \(\sqrt{v+1}\), so unbounded slant cannot hide a wrong-side off-atom factor in this critical case. For mixed off-atom signs and free slant the maximum of (1) is achieved for finite \(\alpha\), since both slant ends give zero uniformly over \(t\) bounded, and the atom kernel controls large \(t\). For same-sign off-atom data and free slant the best boundary value is the favorable infinite-slant value just calculated.

### The finite comparison, including equality

**Proved.** Let \(v_0\) be the held df or \(1\) for free df. With scale free, divergence occurs iff some reachable atom satisfies \(v_0p<k\), by the envelope above and continuity. With no such atom, the likelihood is bounded. The largest reachable boundary value \(B\) is the maximum of zero and the listed face values for the mask. For \(C\) it suffices to take the largest critical value over reachable atoms at \(v=v_0\). If \(\nu\) is free, \(H\) means finite df with its \(G\) and scale-zero ends counted separately. Likewise \(N\) means finite slant with its \(G\) ends counted separately. A supremum within a face whose parameters escape thus contributes to the correct subface. \(B\) itself is attained on the compactified boundary.

Define the finite score candidates to satisfy the scores in every free coordinate, with the one exception that free df at \(\nu=1\) requires the inward score \(\partial_\nu\log L\le0\) in place of its zero. A finite MLE exists **iff at least one finite score candidate has \(L\ge B\)**. Equivalently one may use any feasible finite point with \(L\ge B\). For sufficiency, if a point reaches \(B\) and no feasible likelihood exceeds it, the point is a maximum. If any point exceeds \(B\), all sequences maximizing above it stay away from the boundary and have a convergent subsequence in the finite domain. Necessity follows because the envelope values are limits or limsups of feasible likelihoods and a finite maximum obeys the stated first-order conditions. Thus when the best finite candidate equals \(B\) it attains the supremum; when every finite candidate lies strictly below \(B\), the supremum is \(B\) and only the boundary maximizers can be limits of maximizing sequences. This follows from upper semicontinuity of the joint envelope, given by the [likelihood closure lemma](../ml-behaviour.tex#lem:closure), since the finite likelihood extends continuously to the included df restriction. The test is a candidate-value comparison, with no assumption that a maximum over all finite candidates exists beforehand. It does not provide a finite-time exact numerical equality oracle.

### Strict critical-scale loss with location held

**Proved.** With \(u=w^2\), fixed \(v>0\), fixed finite slant \(a\), and held location, the exact product in the [held-location factorization](2026-11-25-sn-math-report.md#held-location-and-scale-collapse) gives
\[
 \partial_u\log L
 =\frac{e(v)}{2u}-\frac{v}{2}\sum_{d_i\ne0}\frac{v+1+h_i R_{v+1}(h_i)}{vu+d_i^2},
 \qquad h_i=\frac{a d_i\sqrt{v+1}}{\sqrt{vu+d_i^2}},\quad R_m=t_m/T_m.
\]
For \(b>0\) and \(s>1\) the Student density satisfies \(t_m(bs)>s^{-(m+1)}t_m(b)\). Integrating from \(s=1\) to infinity gives \(T_m(-b)>b\,t_m(b)/m\). Hence \(hR_m(h)>-m\) for negative \(h\), and the bound is immediate for nonnegative \(h\). At \(e(v)=0\) there is at least one nonzero residual, so the derivative is strictly negative at all \(u>0\), for **every** finite slant. With df held there is no finite MLE even when slant is free: decreasing scale at the same slant improves each finite point. The best collapse value is precisely (1) with \(t=0\) and the corresponding slant rule.

At critical mass with free df, this argument excludes \(\nu=1\) at every scale as a candidate above or equal to the best collapse value. It also gives a strict small-scale collar for the other df values. The earlier [critical-df collar](2026-11-25-st-math-report.md#a-critical-atom-df-reduction) shows that sufficiently small \(a_s=w\sqrt{\nu}\) strictly favors \(\nu=1\) at the same \((\xi,a_s,\alpha)\). On \(1\le\nu\le2\), small \(w\) gives small \(a_s\) uniformly. On \(\nu\ge2\) there is uniform decay to zero as \(w\to0\) by the tail bound with \(q=2\) and the additional exponent from \(\nu+1\) on bounded \(\nu\), or by splitting \(2\le\nu\le3\) and \(\nu\ge3\) to use a uniform cubic density tail. Since the best collapse value is positive, this excludes all sufficiently small scales against that value. A finite global tie with a collapse value would have to come from larger scale and \(\nu>1\).

### A moving critical-atom sufficient test

**Proved.** For a finite-slant maximizer \(t,a\) of (1) at one of the competitive critical atoms with location free, follow its path at fixed \(v=v_0,\alpha=a\) and \(\xi=y+wt\). At each off-atom observation the skew argument differs from its limiting value by \(O(w^2)\), since \(d/\sqrt{vw^2+d^2}\) has no term linear in \(w\) for \(d\to x_i-y\ne0\). The base tail factor expands by differentiating \(|x_i-y-wt|^{-(v+1)}\). The tied kernel is exactly constant after factoring out \(w^{-1}\). Thus
\[
 \log L=\log C_y(t,a)+(v+1)wt\sum_{x_i\ne y}\frac1{x_i-y}+O(w^2).
\]
If this face reaches \(B\) and the coefficient of \(w\) is positive, there is a finite MLE. A zero coefficient, or a negative coefficient on only one branch, leaves other branches to compare. This test applies with fixed slant and with free slant when the competitive layer maximizer has finite slant.

## Exact nonuniqueness constructions

### Reflected finite maxima with unrestricted slant

**Proved.** Let \(R=2^{128}\). Take nine copies of each of \(-1,1\), and one of each of \(-R,R\). Put \(n=20\), \(W=1000\), \(w_*^2=5/4\), and
\[
 U=(5/4)^{10}/(1+5/4)^{18}.
\]
The maximum mass is \(9<10\), so scale-zero likelihood is zero with free df or df held at one. Consider the finite witness \((\xi,w,\alpha,\nu)=(1/2,w_*,-1/2,1)\). At df one,
\[
 \pi f_i=\frac{w}{w^2+d_i^2}
          \left(1+\frac{\alpha d_i}{\sqrt{w^2+(1+\alpha^2)d_i^2}}\right).
\]
This follows from \(2T_2(s)=1+s/\sqrt{2+s^2}\). Substitution gives \(R^4\pi^{20}L>(13/10)U\). The [rational checker](2026-11-27-st-atlas-math-data/exact_checks.py) supplies the finite arithmetic for this inequality: for each positive radicand \(b\), take \(j=\lfloor\sqrt{\lfloor 2^{160}b\rfloor}\rfloor\), bound its square root between \(j/2^{80}\) and \((j+1)/2^{80}\), and use the endpoint giving the smaller skew factor. All ensuing product comparisons are rational.

A reflection-fixed fit has \(\xi=\alpha=0\). At \(\nu=1\), \(R^4\pi^{20}L\le w^{20}/(1+w^2)^{18}\le U\), with the last maximum found by differentiation. The following bounds cover the rest of the fixed set.

1. For \(w\ge W\), the uniform Student bound gives \(R^4 L\le w^{2-18}\le W^{-16}<U/4^{20}\). For \(w\le W,\ \nu\ge2\), use \(t_\nu(z)\le6(1+z^2)^{-3/2}\). On \([2,3]\) this follows by lowering the kernel exponent to \(3/2\) and using \(1+z^2/\nu\ge(1+z^2)/3\); on \([3,\infty)\) use monotonicity of \(\nu\log(1+z^2/\nu)\). The two far factors then give \(R^4L\le36W^4/R^2<U/4^{20}\), since the other symmetric density factors are at most one.
2. On \(w\le W,\ 1\le\nu\le2\), \(\partial_\nu\log t_\nu(z)\le 5/4-\tfrac12\log(1+z^2/2)\). The normalizer derivative is at most \(1/(4\nu^2)\) by the prior df report's integral for \(C(1/\nu)\). Differentiating the kernel gives a positive term \((\nu+1)z^2/[2\nu(\nu+z^2)]\le1\) and the negative logarithm. The total score is at most \(25-\log(1+R^2/(2W^2))<0\), from the two far factors. The bound follows, for example, from \(1+R^2/(2W^2)>2^{50}>\exp(25)\). Thus \(\nu=1\) supplies the fixed-set upper bound on this region.

Every half face at its best endpoint has 19 distances at least \(R-1\). The bound \(f_i\le 2w/(w^2+d_i^2)\) for Student or normal, including the support multipliers in the envelope, gives \(L\le 2^{20}/(R-1)^{20}\) after optimizing this bound over \(w>0\). Indeed with \(u=w/(R-1)\) the residual factor is \(u^{18}/(1+u^2)^{19}\le1\). Every skew-normal value is bounded by \((2\phi(0)/w)^{20}e^{-R^2/w^2}\) since \(\sum d_i^2\ge2R^2\). Its maximum is at most \(40^{20}/R^{20}\). Each bound multiplied by \(R^4\) is less than \(U/4^{20}\); the checker verifies the rational comparisons. The witness therefore strictly beats all faces and all reflection-fixed fits. A finite maximum exists, and reflection \((\xi,\alpha)\mapsto(-\xi,-\alpha)\) gives a different finite maximum at the same likelihood. This remains true when \(\nu=1\) or \(w=w_*\) is held, since the witness remains feasible, the fixed-set and boundary upper bounds remain valid, and reflection preserves both holds. The proof gives at least two maximizers in each mask.

An additional singleton check uses the same sample with \(\xi=0\) held. Opposite observations of equal magnitude contribute complementary skew factors, whose product is maximal only at \(\alpha=0\). The centered Cauchy point \(w=w_*\) has \(R^4\pi^{20}L=U(1+w_*^2/R^2)^{-2}>U/2\). In the high-shape and large-scale bounds above, multiplying by \(\pi^{20}\) gives less than \(U/2\), since \(\pi<7/2\) and \((7/8)^{20}<1/2\). The df derivative bound then forces \(\nu=1\) on every competitive fit. At that df the log-scale score is strictly decreasing with a unique root. The resulting singleton applies with \(\xi=0\) held, with \(\alpha=\xi=0\) held, and also with \(\alpha=0\) alone held by the prior strict Student geodesic concavity at each df for this symmetric strict-mass sample. [Student uniqueness at each fixed df](2026-11-25-tcert-math-report.md#1-stationary-points)

### Two scale maxima with slant and location held

**Proved.** Fix \(\xi=0,\ \alpha=A=2^{20}\). Set \(p=100,\ n=2p+1,\ R=2^{50000}\), and take \(p\) copies of \(1\), \(p\) of \(D\), and one of \(R\), with \(D\in[D_-,D_+]=[2^{10}A,2^{100}A]\). At \(\nu=1\), multiply the likelihood by \(\pi^nD^{2p}R^2/2^{p+1}\). The resulting objective is
\[
 F(w,D)=w^2\left(\frac{w^2}{w^2+1}\right)^p
 \left(\frac{D^2}{w^2+D^2}\right)^p S_1^p
 \left(\frac{S_D}{2}\right)^p\frac{R^2}{w^2+R^2}\frac{S_R}{2},
 \qquad S_x=1+\frac{A}{\sqrt{1+A^2+(w/x)^2}}.
\]
It is continuous in \(D\) and extends with value zero to both scale endpoints, uniformly for \(D\in[D_-,D_+]\). At zero this follows from the leading \(w^2\); at infinity the original product is bounded by \((2/w)^n\) times the bounded normalization. Let \(P_-(D),P_+(D)\) be its maxima on \(0\le w\le8A\) and \(8A\le w\le\infty\), respectively, in the compactified scale coordinate. These two functions are continuous because \(F\) is continuous on a compact product.

At \(w=A/8\), \(S_1>19/10\); the five other dimensionless factors are each \(>3/4\) by Bernoulli's inequality. In particular when \(w\le x\), \(S_x/2\ge1-1/A^2\) by the tangent lower bound to \((1+y)^{-1/2}\). Therefore \(P_->E=A^2(19/10)^p/512>1\) throughout. At \(w=8A\) we have \(F\le64A^2(9/8)^p<E\). For \(w\ge8A\), \(S_1\le9/8\) and \(w^2(D^2/(D^2+w^2))^p\le D^2\), so \(P_+(D_-)\le D_-^2(9/8)^p<E\). At \(D=D_+\) the witness \(w=D_+/16\) has all five other factors \(>1/2\), \(S_1\ge1\), and \(F>w^2/32>64A^2\,2^p\ge P_-(D_+)\). The [rational checker](2026-11-27-st-atlas-math-data/exact_checks.py) verifies the inequalities with the specified integers.

Define the exact sample by \(D_*=\min\{D\in[D_-,D_+]:P_+(D)\ge P_-(D)\}\). This specifies a real number by a continuous comparison of explicit radical functions on two fixed compact sets. The strict endpoint comparisons put \(D_*\) inside the interval and give equality there by continuity. Each maximum lies strictly inside its scale branch, because \(F(8A,D_*)<P_-(D_*)\) and both outer endpoints have value zero. This proves the finite tie at held \(\nu=1\). The crossing construction is an exact real-parameter tie in the terminology of the [tie report](2026-11-25-ties-math-report.md#classification-of-the-constructions); its decimal approximation is numerical.

The same exact sample has the same maximizing scale set with df free. To show this, put \(W=2^{1000}\). For \(w\ge W\), the far factor satisfies \(R^2 f_R\le2w\), and all other factors are bounded by \(2/w\). On \(w\le W,\nu\ge2\), use the cubic Student bound above, obtaining \(R^2 f_R\le12W^2/R\). The other factors are at most one, since \(|d_i|\ge1\) and \(2w/(w^2+d_i^2)\le1/|d_i|\). After normalization both bounds are \(<1\); it suffices to compare \(4^n D_+^{2p}\) times \(2^n/W^{2p-1}\) or \(12W^2/R\) with one.

At \(w\le W,\ 1\le\nu\le2\), the derivative of each base log-density is bounded as in the reflected construction. The derivative of each log skew multiplier is at most three. For this last bound, at a fixed argument the Student log CDF df derivative is at most two, by integrating the differentiated density as in the prior [critical-df collar](2026-11-25-st-math-report.md#a-critical-atom-df-reduction). The skew argument \(h>0\) satisfies \(\partial_\nu\log h=\tfrac12[(\nu+1)^{-1}-(\nu+z_i^2)^{-1}]\le1\) and \(h\,t_{\nu+1}(h)/T_{\nu+1}(h)\le1\). Thus the total score is at most \(5n-\tfrac12\log(1+R^2/(2W^2))<0\); the argument of the log exceeds \(2^{20n}\). Every competitive optimizer is at \(\nu=1\), and the large-df skew-normal face is below the finite witness as well by the same cubic bound and continuity. This provides a second refutation with free df. The sample uses the real-valued atlas scope; its \(R\) exceeds the binary64 range.

## Results by regime

### st held all free

**Criterion and failure, proved.** On the fitted domain the positive boundary entries are \(N,H_\pm,G_\pm,C\) with moving-location contacts. The only possible infinite supremum requires \(k_{\max}>n/2\), where \(k_{\max}\) is the greatest sample multiplicity. At equality the critical atoms must be included even when both half faces are already being compared at positive scale. Below or at critical mass apply the finite comparison against \(B\). With unrestricted positive df the dossier's collapse proof instead gives an empty maximizing set for every nonempty sample: choose an atom and any fixed \(\nu>0\) below \(k/(n-k)\) if \(k<n\). The point-mass sample collapses for every fixed \(\nu>0\).

**Uniqueness, refuted on the fitted domain.** The [reflected sample](#reflected-finite-maxima-with-unrestricted-slant) proves at least two finite global maximizers with strict multiplicities. Those fits occur away from every escape. It does not count further finite maxima or imply a tie for all symmetric samples.

### st held alpha

**Criterion and failure, proved.** The only potentially positive faces are \(N\) at the specified finite slant and moving-location \(C\). Half-model and infinite-location/scale escape cannot win. A majority atom gives divergence; otherwise compare the finite candidates with \(B=\max(N,C,0)\). The normal optimum has the fixed-slant strict concavity above. In a strict-mass sample a positive inward df slope there is sufficient for attainment by the [prior signed df test](2026-11-25-st-math-report.md#expansion-in-inverse-df). At a critical atom the moving scale test above can also give a strict witness. Equality of finite and normal values really can attain here: the exact \(\lambda_*\) construction at held \(\alpha=0\) in the [df report](2026-11-25-st-math-report.md#transition-examples-and-numerical-evidence) checks a uniform df collar and attains at positive inward \(1/\nu\) while the normal face ties.

**Uniqueness, partial.** For \(\alpha=0,\ 2k_{\max}<n\), the existing fixed-df Student proof gives a unique location and scale at every \(\nu\ge1\), so the full cardinality is that of the winning df profile. The [reflected-sample calculation with slant held at zero](#reflected-finite-maxima-with-unrestricted-slant) gives an exact finite singleton example by forcing \(\nu=1\). General uniqueness over df remains open even with zero slant. The [Student two-mode example](2026-11-25-tcert-math-report.md#an-exact-sample-with-two-interior-shape-maxima) transfers to this mask, but its two local maxima have different values. For general held nonzero slant the fixed-df nuisance cardinality is also unresolved.

### st held alpha+nu

**Criterion and failure, proved.** The only potentially positive escape is \(C\), with fixed finite slant and df and moving location. If \(k_{\max}>\nu(n-k_{\max})\), collapse diverges. If \(k_{\max}<\nu(n-k_{\max})\), every escape gives zero and a finite MLE exists. At equality, compare finite candidates with the largest value of (1) over critical atoms and their displacement \(t\). The moving-atom linear test gives a sufficient witness if its maximizing branch is finite and the coefficient is positive. Equality does attain on the exact balanced Cauchy sample below. At \(\alpha=0,\nu=1\), a critical atom with more than two support points gives a nonattainment case by the [Student critical-atom proof](2026-11-07-std-held-data/derivation.md#joint-critical-atom-at-held-shape-at-least-one). That proof uses the product distance identity for Cauchy and shows strict inequality at each positive scale when the other observations have different supports.

**Uniqueness, refuted.** For \((-1,1)\), with \(\alpha=0,\ \nu=1\), the [earlier exact Cauchy identity](2026-11-08-st-cert-report.md#nonuniqueness-counterexamples) gives \(L=w^2/\{\pi^2[4w^2+(\xi^2+w^2-1)^2]\}\). The entire maximizing set is \(\xi^2+w^2=1,\ w>0\). Both collapsed endpoints tie at \(1/(4\pi^2)\). This is a structural continuum in the [ties classification](2026-11-25-ties-math-report.md#classification-of-the-constructions). The [subunit symmetric Student example](2026-11-25-tcert-math-report.md#1-stationary-points) also transfers at held zero slant and \(\nu=1/2\), giving reflected finite maxima at strict mass.

### st held alpha+nu+xi

**Criterion and failure, proved.** The only nonzero scale-zero value occurs when \(e(\nu)\le0\), with divergence below zero and (1) at \(t=0\) at equality. At equality [the strict scale loss](#strict-critical-scale-loss-with-location-held) shows that no finite scale reaches the boundary value for any held finite slant. At positive exponent both scale ends vanish, so a finite maximum exists. This sharpens the comparison criterion in the atlas.

**Uniqueness, refuted.** The [two-scale construction](#two-scale-maxima-with-slant-and-location-held) gives two separated global maximizing scales at held \(\nu=1\) with no ties at the held location. At zero held slant and positive exponent the previously proved strictly decreasing log-scale score has exactly one root. For general fixed slant and positive exponent, the scale score is analytic on \(w>0\), positive near zero from the exact factorization and negative near infinity from the finite limiting density at standardized residual zero. Thus its roots lie in a compact positive interval and are finite in number, since the score is analytic there and not identically zero. The exact tie precludes a universal claim of one winning root.

### st held alpha+xi

**Criterion and failure, proved.** The potentially positive faces are \(N\) at the two holds and \(C\) at the held location only. Apply the divergence test to \(k=\#\{x_i=\xi\}\). Otherwise use \(B=\max(N,C,0)\) and the finite comparison. At critical mass \(C\) is (1) with \(t=0,\ \nu=1\) and the stipulated slant; the signed critical collar excludes sufficiently small scales against \(C\). Strict mass and a positive inward slope at the unique normal optimum suffice for finite attainment. For a nonattainment example, take \(\alpha=0\) and all \(|x_i-\xi|=d>0\). At each df the unique scale optimum is \(w=d\). Its value is below the normal value because \(t_\nu(1)=\phi(0)E[\sqrt V\exp(-V/2)]<\phi(1)\) for \(V\sim\chi^2_\nu/\nu\), as the integrand has its unique maximum at \(V=1\).

**Uniqueness, refuted.** The [two-scale sample](#two-scale-maxima-with-slant-and-location-held) forces all competitive fits to \(\nu=1\) even when df is free and gives two separated global maximizing scales. The holding condition \(\alpha=0\) removes this example, and the exact centered example above has a unique maximizer in that restricted case.

### st held nu

**Criterion and failure, proved.** The potentially positive ends are \(H_\pm,C\), with the held df and moving-location contact. Divergence holds iff some atom has \(k>\nu(n-k)\); otherwise use \(B=\max(H_+,H_-,C,0)\) and the finite comparison, with \(C\) only at critical atoms. The half-face location is the sample extreme in its orientation. Each half-face scale comparison there has the [symmetric decreasing log-scale score](2026-11-08-st-cert-report.md#mathematical-justification), with the appropriate constant likelihood multiplier. A finite witness beating both half-face values proves attainment in strict mass. At \(\nu=1\) the balanced pair \((-1,1)\) with slant free gives a nonattainment example. The Cauchy identity bounds the Student product by \(1/(4\pi^2)\), and two finite skew factors give strictly less than four times that bound. The critical collapse at either endpoint reaches \(1/\pi^2\) by the favorable moving-contact path.

**Uniqueness, refuted.** The [reflected construction](#reflected-finite-maxima-with-unrestricted-slant) with \(\nu=1\) held gives distinct finite reflected maxima at strict mass. The Cauchy continuum for held slant zero does not by itself establish this result, since freeing slant can beat that continuum.

### st held nu+xi

**Criterion and failure, proved.** Write \(e=\nu(n-k)-k\) at the held location and take \(v=\nu\). If \(e<0\), collapse diverges. If \(e=0\), the strict critical loss gives nonattainment and the value (1) at \(t=0\) maximized over slant, including infinite slant. If \(e>0\), a finite MLE exists **iff the nonzero residuals have both signs**. With both signs each half face is zero and both scale ends vanish. With just one sign, the likelihood is strictly monotone in slant at each scale, and the compatible \(H\) wins at the unique positive scale solving the symmetric scale score. Held contacts retain factor one. There is no infinite-df face. All residuals zero puts the sample in \(e<0\), with slant unidentifiable along the divergence path.

**Uniqueness, partial.** Suppose \(e>0\) and the nonzero residual multiset is made of matching pairs \((-d,d)\) with \(d>0\). Each pair has complementary CDF factors and is optimized at slant zero, strictly. The symmetric scale score gives one scale, so the joint maximizer is unique. For asymmetrically mixed data, a conditional result at \(\nu=1\) follows by writing \(\alpha=\sinh\tau,\ w=b\cosh\tau,\ b>0\). The one-observation density is then
\[
 \frac{1}{\pi\sqrt{b^2+d^2}\cosh(\tau-\operatorname{asinh}(d/b))}.
\]
This follows by rationalizing the skew factor in the displayed df-one density. For each \(b\) the log likelihood is strictly concave in \(\tau\); its score tends to \(\pm n\) at the respective ends \(\mp\infty\). However the profiled scale curvature has no immediate fixed sign. Put \(q_i=d_i/\sqrt{b^2+d_i^2},\ T_i=\tanh(\tau-\operatorname{asinh}(d_i/b))\), and \(s_i=1-T_i^2\). In \(\log b\) the profile curvature is
\[
 \sum_i (1-q_i^2)(q_i T_i-2q_i^2)-
 \sum_i s_i(q_i-\bar q_s)^2 ,
 \qquad \bar q_s=\frac{\sum_i s_i q_i}{\sum_i s_i}.
\]
Indeed \(q_i'=-(1-q_i^2)q_i\), and the \(\tau\) score equation \(\sum_i T_i=0\) gives \(\tau'=-\bar q_s\). A joint concavity claim in these coordinates fails even for some mixed-sign samples: take many copies of a small positive residual with \(0<q<1/2\) and a single negative residual, set \(\tau\) large and take the direction \(d\tau=-q\,d\log b\). The repeated positive terms have strictly positive curvature as the first term tends to \((1-q^2)(q-2q^2)>0\) per copy, whereas the single negative term stays bounded. Establishing a sign on the profiled critical set, or constructing an exact tie away from paired data, remains open.

### st held omega

**Criterion and failure, proved.** The positive faces are \(N,H_\pm,G_\pm\) at the specified scale and with moving-location contacts. Scale collapse cannot occur. The likelihood is bounded by \((2\phi(0)/w)^n\) and the finite comparison applies for all samples. Nonattainment for a constant sample follows directly from the same bound: finite \(\nu\) gives \(c_\nu<\phi(0)\), while both half-normal directions reach it along the contact paths. The reflected sample supplies a checkable strict finite witness for nonconstant data.

**Uniqueness, refuted.** With \(w=\sqrt{5}/2\) held, the [reflected construction](#reflected-finite-maxima-with-unrestricted-slant) supplies at least two finite reflected maximizers. Its reflected tie is between finite fits strictly above all escape values.

### st held xi

**Criterion and failure, proved.** The possible positive ends are held-location \(N,H_\pm,G_\pm,C\). Only the held location can be a collapse atom. If \(k>n/2\) the likelihood diverges; otherwise use the boundary comparison, with \(C\) present at \(k=n/2\). With both signs the half faces have zero value, so only \(N,C\) can compete. The [critical collar](#strict-critical-scale-loss-with-location-held) excludes the small-scale region when \(k=n/2\). With one strict sign there is no finite MLE by pointwise slant monotonicity, regardless of which compatible face wins over df and scale. With no strict signs all residuals vanish, so collapse diverges. For mixed signs and strict mass a finite witness beating the same-hold normal optimum suffices. The centered reflected-sample calculation is one such witness.

**Uniqueness, partial.** For paired residuals the slant is zero at every positive-scale optimum, so cardinality reduces to the shape profile of the held-location Student fit after the unique scale solve at every df with positive exponent. The centered example in the [reflected construction](#reflected-finite-maxima-with-unrestricted-slant) gives a finite singleton with \(\nu=1\). For general mixed signs neither the joint scale/slant profile at fixed df nor the global finite df profile has a general root separation or exact tie test. Paired data alone also do not yield uniqueness over df.

## Summary

“Refuted” refers to universal uniqueness. “Partial” marks a scoped cardinality result and the remaining open question.

| Family | Held | Question | Result |
| --- | --- | --- | --- |
| st | all free | criterion | Proved |
| st | all free | failure | Proved |
| st | all free | uniqueness | Refuted on the fitted domain |
| st | alpha | criterion | Proved |
| st | alpha | failure | Proved |
| st | alpha | uniqueness | Partial |
| st | alpha+nu | criterion | Proved |
| st | alpha+nu | failure | Proved |
| st | alpha+nu | uniqueness | Refuted |
| st | alpha+nu+xi | criterion | Proved |
| st | alpha+nu+xi | failure | Proved |
| st | alpha+nu+xi | uniqueness | Refuted |
| st | alpha+xi | criterion | Proved |
| st | alpha+xi | failure | Proved |
| st | alpha+xi | uniqueness | Refuted |
| st | nu | criterion | Proved |
| st | nu | failure | Proved |
| st | nu | uniqueness | Refuted |
| st | nu+xi | criterion | Proved |
| st | nu+xi | failure | Proved |
| st | nu+xi | uniqueness | Partial |
| st | omega | criterion | Proved |
| st | omega | failure | Proved |
| st | omega | uniqueness | Refuted |
| st | xi | criterion | Proved |
| st | xi | failure | Proved |
| st | xi | uniqueness | Partial |

The [atlas rows](2026-11-27-st-atlas-math-data/proofs.csv) cover the questions requested for the nine masks. The existing all-free and scale-only criterion and failure entries were left for the coordinator to reconcile with the stronger results above.

## Reproduction

Both final Python runs passed on the workstation through the one-slot wrapper. The [exact checker](2026-11-27-st-atlas-math-data/exact_checks.py) verifies the rational inequalities in the two constructions, with a reflected witness lower bound of \(1.334244187054\,U\) to the digits shown. The [mpmath replay](2026-11-27-st-atlas-math-data/replay.py) passed 75 checks at 65 decimal digits, covering finite-slant critical layers, held-location monotonicity, moving contacts and the free-df rate multiplier. It also sampled 489 log scales at spacing \((\log 2)/4\) at the computed scale crossing. The tied stationary scales in that calculation have \(\log_2 w\) approximately \(17.71531044138\) and \(67.70012237307\), with \(\log_2 D\) approximately \(71.01480068311\). The numeric root and grid are checks of the construction. The existence of the exact crossing uses the uniform strict inequalities and continuity.

Run from the worktree root.

~~~sh
export TMPDIR="$PWD/build/tmp"
mkdir -p "$TMPDIR"
/tmp/umvl-cpu/run python3 -B dev/research/evaluations/2026-11-27-st-atlas-math-data/exact_checks.py
/tmp/umvl-cpu/run python3 -B dev/research/evaluations/2026-11-27-st-atlas-math-data/replay.py
~~~

## What this changes for the fitter

- Determine reachable collapse atoms from the location hold and use the held df or the free floor \(1\) for the exponent. Negative exponent proves unboundedness. At critical mass with held location and df, use the exact scale-boundary reason after profiling the collapse value over any free slant. With held location and free df, apply the signed small-scale collar before the remaining df and scale comparisons.
- Under held location and a single strict sign, report proved slant nonattainment. With fixed df and positive exponent, compute the appropriate signed half-t scale and preserve the loss of \(k\log 2\) from held endpoint contacts. With free df, compare compatible half-t, half-normal and collapse values under the same holds before naming a winning face. Constant data with held scale and moving location retain both orientations when the boundary value ties.
- Build the finite cover in the free coordinates, including the inward df score at \(1\). Use the published slant wing bounds and signed df comparisons on their stated domains. After a strict face win, report the appropriate limit or collapse status. When a finite fit ties the envelope, retain it as a maximum and retain the boundary witness. A successful search whose global comparison or coefficient enclosure remains unresolved returns an uncertified estimate. A refuted candidate or failed search follows the documented failure handling.
- A separated value certificate does not determine cardinality. Preserve reflected fit components, scale ties and the Cauchy continuum when proved; away from a cardinality proof leave uniqueness unknown. The reflected observations are representable in binary64, but the proofs compare exact products at extreme magnitudes. Validate any production coefficient or value enclosure separately with scaled arithmetic.

## Dossier follow-ups

- Replace “zero collapse exponent leaves attainment unresolved” where location and df are both held. Correct text for finite slant, held or free: “With location and df held, critical mass with at least one nonzero residual gives a finite, unattained scale-zero supremum. At every finite slant the likelihood decreases strictly with scale.” [Proof](#strict-critical-scale-loss-with-location-held)
- Qualify the existing general equality stop. Correct text: “When location moves, scale-collapse equality may tie an attained finite set, as for the balanced Cauchy pair. Compare the joint collapse envelope against finite fits. At held location with free df the critical collar excludes sufficiently small scales but leaves the comparison with surviving finite and normal fits.” [Critical proof](#strict-critical-scale-loss-with-location-held) · [Cauchy example](#st-held-alphanu)
- Add that universal uniqueness fails on the fitted all-free domain even with strict atom counts. Also qualify the held-scale and held-df subsets and the held-location/slant scale profile with their exact counterexamples. [Reflected proof](#reflected-finite-maxima-with-unrestricted-slant) · [Scale-crossing proof](#two-scale-maxima-with-slant-and-location-held)

## Open

1. **Held slant.** With zero slant and strict atom masses, the remaining obstacle is ordering the stationary values of the df profile after unique conditional location/scale profiling. The proved Student local modes rule out a one-root proof. An equal-height finite crossing requires exclusion of the normal and collapse values at the crossing. With nonzero held slant, one also needs a joint nuisance curvature or comparison argument.
2. **Held location and df, and held location alone.** For asymmetrically mixed residuals the paired-product inequality does not fix the maximizing slant. Even at \(\nu=1\) a unique conditional \(\tau\) does not bound the profiled scale curvature at its zeros; the signed term in the displayed curvature needs a stationary inequality or an exact separated tie. With free df the winning shape level also needs to be compared. The paired-data reduction leaves that comparison open.
3. **General equality and cardinality.** The exact criterion states which finite/face equalities attain; it does not compare arbitrary degenerate score components at machine precision. For moving critical atoms where the linear layer test vanishes, a signed upper collar needs a remainder uniform over optimal displacement and slant, including infinite-slant contact rates. The exact counterexamples prove at least two finite maxima in their stated masks; apart from the Cauchy continuum they do not exhaust the maximizing set.
