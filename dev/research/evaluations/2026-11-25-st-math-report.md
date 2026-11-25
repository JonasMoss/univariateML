# The skew-normal face of the free-df Azzalini skew-t

TASK-282 · 2026-11-25 · base d33239bdca24c21efba3440051f7aa14b9854581

The inward slope of the df profile is a skew-adjusted fourth-moment contrast at the best skew-normal fits. A strictly positive slope excludes the normal face. A strictly negative slope closes the large-df region against the face, leaving the finite-df comparison. When the slope vanishes, the second-order term must include the movement of the fitted coefficients. A separate power bound handles singular slant, including a symmetric zero-slant case with a sixth-order skew-normal likelihood loss. A finite-df fit can tie the normal face at the global maximum even when the inward slope is strictly negative.

The analysis uses the earlier [skew-normal and half-t exterior bounds](2026-11-25-sn-math-report.md#azzalini-skew-t), [skew-t certificate scope](2026-11-08-st-cert-report.md#open-entries), [skew-t closure and collapse layers](2026-11-07-skew-theory-data/proofs.md#14-closing-the-st-collapse-equality-layers-by-sign-cases), and [Student normal-face certificate](2026-11-25-tcert-math-report.md#normal-face). The free-df domain is \(\nu\ge1\), with \(\nu=1\) attainable. The location and scale are the direct parameters \(\xi,\omega\). The normal face is Azzalini skew-normal, with a half-normal corner in each feasible slant orientation. [Family dossier](../../../univariateML-cpp/include/cppunivariate/distributions/st/README.md)

## Expansion in inverse df

**Proved.** Put \(h=1/\nu,\ z=(x-\xi)/\omega,\ t=\alpha z\), and \(r(t)=\phi(t)/\Phi(t)\). At fixed coefficients the one-observation log-density difference is
\[
 \log f_h(x)-\log f_0(x)=h\,a(z,\alpha)+h^2 b(z,\alpha)+O(h^3),
 \qquad
 a(z,\alpha)=\frac{z^4-2z^2-1+t\,r(t)(1-2z^2-t^2)}4.
\]
For a derivative formula suited to evaluating the second coefficient, let
\[
 G(w)=\sqrt w\,e^{-z^2 w/2}\Phi(t\sqrt w),\qquad
 b(z,\alpha)=\frac{4G'''(1)/3+G^{(4)}(1)/2}{G(1)}
                 -\frac12 a(z,\alpha)^2 .
\]
Both expansions, and Taylor bounds obtained by differentiating in the nuisance and slant coordinates, are uniform on compact positive-scale, finite-slant sets. Write \(A=\sum_i a(z_i,\alpha)\), \(B=\sum_i b(z_i,\alpha)\) and \(F_0(\theta)=\ell(0,\theta)\) for the total log likelihood at the face, with \(\theta\) restricted by the holds.

To prove the formulas, use the scale mixture \(W\sim\chi_\nu^2/\nu\). Given \(W=w\), the density has the skew-normal form with scaled standardized residual \(z\sqrt w\) and Jacobian \(\sqrt w\), so the unconditional density is \(2\phi(0)E G(W)/\omega\). Integrating its Gaussian base against \(W\) gives \(t_\nu(z)\) and updates \(W\) to a gamma variable with shape \((\nu+1)/2\) and rate \((\nu+z^2)/2\). Integrating the remaining Gaussian CDF under that gamma law gives the skew-t multiplier \(T_{\nu+1}\bigl(t\sqrt{(\nu+1)/(\nu+z^2)}\bigr)\). The centered cumulants of \(W\) of order \(j\ge2\) are \(2^{j-1}(j-1)!h^{j-1}\), by its gamma moment generating function. The centered moments of orders two through four are \(2h,8h^2,12h^2+48h^3\), the fifth is \(O(h^3)\), and the sixth absolute moment is \(O(h^3)\). Expand \(G\) to degree five about one. On \([1/2,3/2]\) the remainder is bounded by \(\sup|G^{(6)}|\,|W-1|^6/6!\). Outside that interval, both \(G(W)\le\sqrt W\) and the Taylor polynomial are bounded by constants times \(|W-1|^6\), since \(|W-1|\ge1/2\). These constants can be taken uniformly on the stated compact sets. Thus \(E G=G(1)+hG''(1)+h^2(4G'''(1)/3+G^{(4)}(1)/2)+O(h^3)\). Differentiating \(G\) twice gives the displayed \(a=G''(1)/G(1)\). Taking the log uses the strictly positive minimum of \(G(1)\) on the box.

For computing explicit mixed-derivative bounds, use the Student log normalizer relative to the Gaussian
\[
 C(h)=-\frac12\int_0^\infty e^{-v}\tanh(hv/2)\,\frac{dv}{v}.
\]
Use \(z^2\int_0^1(1+h z^2 u)^{-1}\,du\) in place of \(\log(1+h z^2)/h\) in the base kernel. For the skew CDF use
\[
 T_{\nu+1}(s)=\tfrac12+s\int_0^1 t_{\nu+1}(s u)\,du,
 \qquad s=t\sqrt{(1+h)/(1+h z^2)}.
\]
In its integrand replace \(h\) by \(h/(1+h)\) in the same Student base formulas. These identities give a smooth right extension at zero to every finite derivative order. The derivatives of \(\tanh\) are polynomials in \(\tanh\) and are bounded on the real line. For derivative order \(j\ge1\) in the normalizer integral, the remaining majorant integrates as \(2^{-j-1}\int_0^\infty e^{-v}v^{j-1}dv\) times that polynomial bound. The other integrals are on \([0,1]\), the rational denominators are bounded below, and the CDF has a positive minimum on each compact input box. Interval evaluation and Taylor's theorem therefore supply finite, computable constants, including a bound for the \(h^3\) remainder. The integral from zero can cancel on the negative tail in floating point. It is a derivative identity and can be paired with a separate positive, tail-accurate CDF enclosure when forming log derivatives. The normalizer identity and its Student bounds are from the [Student certificate](2026-11-25-tcert-math-report.md#3-box-likelihood-scores-and-curvature).

At \(\alpha=0\), \(a=(z^4-2z^2-1)/4\), \(b=z^4/4-z^6/6\), the latter also following from \(C(h)=-h/4+O(h^3)\) and the log kernel. When both scale and slant are free at a finite skew-normal optimum, the slant score gives \(\sum_i t_i r(t_i)=0\), and the scale score then gives \(\sum_i z_i^2=n\). The sign is the sign of
\[
 4A=\sum_i z_i^4-3n-(2+\alpha^2)\sum_i z_i^2t_i r(t_i).
\]
For other holds use the unabridged formula for \(a\) at the hold-compatible face optimum. Evaluation at a proposed center or a stationary point that loses the normal-face optimization gives no decision for the best face.

## Profiling and signed bounds when the slope vanishes

**Proved, first order.** On a compact set \(K\) of feasible coefficients, put \(F=\max_K F_0\) and \(S=\{\theta\in K:F_0(\theta)=F\}\). Then
\[
  \max_{\theta\in K}\ell(h,\theta)=F+h\max_{\theta\in S}A(\theta)+o(h).
\]
A face maximizer with maximal \(A\) gives the lower bound. Uniform convergence forces every sequence of small-\(h\) maximizers to approach \(S\). Dropping \(F_0-F\le0\) at those maximizers and taking the limsup gives the upper bound. The same argument applies to the maximum of finitely many compact pieces, including half-t components with \(h=0\) interpreted as half-normal. At a negative maximal slope, take neighborhoods of the active maximizers with \(A\le-\gamma<0\). There the uniform remainder gives \(\ell(h,\theta)\le F-\gamma h/2\) for sufficiently small \(h\). The complement has a strictly positive face gap, which a bound on \(hA+h^2 B+O(h^3)\) preserves. This is a signed comparison against \(F\) on the entire punctured df neighborhood.

**Proved, regular maximizers.** At an interior isolated face maximizer \(\theta_0\) in the free coordinates let \(H=F_0''(\theta_0)\prec0\) and \(g=\nabla A(\theta_0)\). For sufficiently small \(h\) the locally maximizing branch is \(\theta_0-hH^{-1}g+O(h^2)\). Its log likelihood is
\[
 F+hA(\theta_0)+h^2D+O(h^3),\qquad
 D=B(\theta_0)-\tfrac12g^\top H^{-1}g .
\]
This follows from the implicit score equation and substitution into the Taylor series. With no free coefficients other than df, the quadratic correction is zero. If several active maxima exist, compare their expansions over all active branches, using the lower slopes to exclude branches first. With \(A=0\), \(D>0\) gives a feasible improvement, and \(D<0\) gives a strict local upper bound.

A computable one-sided version uses any \(G_0\succ0\) such that \(-\ell_{\theta\theta}\succeq G_0\) throughout a convex neighborhood of \(\theta_0\), uniformly over \(0\le h\le H_0\). Maximizing the quadratic tangent upper bound gives
\[
 \sup_{\text{neighborhood}}\ell(h,\theta)
 \le \ell(h,\theta_0)
       +\tfrac12\nabla_\theta\ell(h,\theta_0)^\top G_0^{-1}
                  \nabla_\theta\ell(h,\theta_0)
 \le F+hA_0+h^2(B_0+\tfrac12g^\top G_0^{-1}g)+C_3 h^3 .
\]
For \(D<0\) one can take the neighborhood and \(H_0\) small enough that \(G_0\) is close enough to \(-H\) for the quadratic coefficient to be strictly negative. Enclosing derivatives and the face root supplies the constants. Another way to bound the profile remainder uses the third implicit derivative of the value. With \(v=\theta'(h)=-\ell_{\theta\theta}^{-1}\ell_{\theta h}\), it equals
\(\ell_{hhh}+3\ell_{hh\theta}v+3\ell_{h\theta\theta}[v,v]+\ell_{\theta\theta\theta}[v,v,v]\).
The terms involving \(\theta''(h)\) cancel by the differentiated score equation. This gives a directly boundable remainder once the root tube is enclosed.

**Proved, profiling slant without an inverse joint Hessian.** At a fixed finite \(\alpha\), use the skew-normal nuisance coordinates \((1/\omega,\xi/\omega)\). The free nuisance Hessian at \(h=0\) is strictly negative definite, including on the affine subspaces for holds. On a bounded slant interval, restrict to a common convex compact nuisance box containing all conditional maximizers for small \(h\), with strict exterior exclusion. Such a box follows under the positive-scale compactness conditions below by using the minimum value of a common feasible nuisance point over the slant interval as an incumbent. Strict nuisance curvature persists uniformly for small \(h\), and the same expansion gives a reduced profile
\[
 P(h,\alpha)=P(0,\alpha)+h\,\bar A(\alpha)+h^2\bar D(\alpha)+O(h^3),
\]
where \(\bar D\) uses only the inverse nuisance Hessian and \(\bar A\) is evaluated at the exact fixed-slant skew-normal optimum. This uses the [fixed-slant nuisance concavity](2026-11-25-sn-math-report.md#one-dimensional-cover-and-source-discrepancy). The pure slant profile and the two coefficients are analytic in finite slant. The compact finite-slant maxima of the moving-location profile are isolated by the prior analyticity and strict-tail argument. Held location with mixed signs gives a unique finite skew-normal maximum in slant. If every residual vanishes with nuisance fixed, the exact comparison is \(n C(h)<0\) for \(h>0\) along the flat slant line.

At an isolated maximum \(\alpha_0\), write \(x=\alpha-\alpha_0\) and suppose \(\bar A(\alpha_0)=0\). If \(P(0,\alpha)\le F-c |x|^{2k}\) and \(\bar A(\alpha)\le L|x|^j\) nearby, \(c>0,\ L\ge0,\ j>0\), then for \(j<2k\)
\[
 P(h,\alpha)\le F+h^2(\bar D(\alpha_0)+\epsilon)
                     +M h^{2k/(2k-j)}+C_3h^3,\qquad
 M=\max_{v\ge0}(-cv^{2k}+Lv^j).
\]
Use a small enough neighborhood to bound \(\bar D\) by the displayed value, and substitute \(|x|=h^{1/(2k-j)}v\). For \(j\ge2k\) the term \(h\bar A\) is absorbed by the face loss for small \(h\). The same absorption is immediate if \(\bar A\le0\). When \(j>k\) the possible gain from slant movement has order strictly higher than \(h^2\). When \(j=k\) it is quadratic, with coefficient \(L^2/(4c)\). With signed Taylor coefficients \(P(0,\alpha)=F-c_* x^{2k}+O(|x|^{2k+1})\) and \(\bar A=L_*x^k+O(|x|^{k+1})\), use the largest nonnegative leading coefficient in the two feasible directions. Shrinking the neighborhood gives the upper coefficient and a path \(x=O(h^{1/k})\) in its maximizing direction gives the matching lower one. If \(j<k\) and the leading \(j\)-term of \(\bar A\) is positive in a feasible direction, that direction gives a strictly positive profile gain of order \(h^{2k/(2k-j)}\), ahead of the quadratic term. Each strict comparison can be made with Taylor bounds on the univariate functions. A common vanishing coefficient requires a further comparison.

### Symmetric data at zero slant and zero slope

**Proved.** Let \(\xi,\omega,\alpha\) be free and standardize symmetric nonconstant data to empirical mean zero and variance one. Write \(m_j=\overline{x^j}\). Use the slant coordinate \(\delta=\alpha/\sqrt{1+\alpha^2}\), and use the mean and sd of the **skew-normal** at the same \((\xi,\omega,\alpha)\) as nuisance coordinates \((\mu,s)\), including for \(h>0\). Put \(b_0=\sqrt{2/\pi}\). If \(m_4=3\) then \(\bar A(0)=0\). Let \(\kappa_j\) be the cumulants of \(|N(0,1)|\), and set
\[
 c_6=n\left[\frac{\kappa_3^2}{12}-\frac{\kappa_6(m_6-15)}{720}\right],
 \qquad D_0=n\left(\frac74-\frac{m_6}6\right).
\]
If \(c_6>0\), the locally maximized value in a neighborhood of zero slant is
\[
 F_{\mathrm G}+h^2D_0+O(h^3)
\]
with an upper bound of this order uniform over the whole neighborhood. Here \(F_{\mathrm G}\) is the fitted Gaussian value. If this point gives the best skew-normal value, \(D_0<0\) closes its part of the df tail. Even if another skew-normal point wins, the bound is a local statement at zero slant.

For the proof, the standardized skew-normal variate has the law of
\[
 \frac{\delta(|U|-b_0)+\sqrt{1-\delta^2}\,V}{\sqrt{1-b_0^2\delta^2}},
 \qquad U,V\text{ independent standard Gaussians}.
\]
The representation follows by conditioning the correlated Gaussian pair \((U,\delta U+\sqrt{1-\delta^2}V)\) on \(U>0\). Writing \(c_j(\delta)=\kappa_j\delta^j/(1-b_0^2\delta^2)^{j/2}\), its density ratio to the standard Gaussian through degree six in \(\delta\) is
\[
 1+\sum_{j=3}^{6}\frac{c_j(\delta)}{j!}\,\mathrm{He}_j(x)
       +\frac{c_3(\delta)^2}{72}\,\mathrm{He}_6(x)+O(\delta^7),
\]
where \(\mathrm{He}_j\phi=(-1)^j\phi^{(j)}\). To obtain this, multiply the Gaussian characteristic function by the expansion of the higher-cumulant exponential. Differentiating the characteristic function of the representation through order seven near zero slant is dominated by a polynomial in the Fourier variable times a Gaussian, since the independent Gaussian coefficient is bounded away from zero. Fourier inversion thus yields the displayed expansion and its locally uniform nuisance derivatives. In the log, the square term becomes \(c_3^2(\mathrm{He}_6-\mathrm{He}_3^2)/72\). The odd sums vanish by symmetry, and \(m_4=3\) makes the \(\mathrm{He}_4\) sum zero. The normalized sums of \(\mathrm{He}_6\) and \(\mathrm{He}_6-\mathrm{He}_3^2\) are \(m_6-15\) and \(-6\). The nuisance gradient of the cubic term at the Gaussian optimum vanishes: the location derivative is proportional to \(\sum \mathrm{He}_2(x_i)=0\), and the log-scale derivative to \(\sum x_i\mathrm{He}_2(x_i)=0\). Thus profiling changes the nuisance point by \(O(\delta^4)\) and the value by \(O(\delta^8)\). Reflection makes the value even and gives \(P(0,\delta)=F_{\mathrm G}-c_6\delta^6+O(\delta^8)\).

At the Gaussian nuisance coordinates the direct residual is \(z=x\sqrt{1-b_0^2\delta^2}+b_0\delta\). Substitution in \(a\) gives quadratic coefficient \(b_0^2(x^2-1)/4\), whose sum vanishes. The linear and cubic sums vanish by reflection, and moving to the conditionally optimal nuisance point only adds \(O(\delta^4)\). Hence \(\bar A=O(\delta^4)\). The Gaussian nuisance Hessian in \((\mu,\log s)\) is \(\operatorname{diag}(-n,-2n)\); the gradient of \(A\) is \((0,-2n)\). Therefore \(\bar D(0)=n(m_4/4-m_6/6)+n=D_0\) and, by reflection, \(\bar D(\delta)=D_0+O(\delta^2)\). In a small neighborhood the upper bound is
\[
 F_{\mathrm G}+h^2 D_0-\tfrac12 c_6\delta^6
                    +C(h\delta^4+h^2\delta^2+h^3).
\]
Putting \(v=\delta^2/h\) bounds the last terms and the negative sextic by a constant times \(h^3\). The path \(\delta=0\) gives the lower expansion.

If \(D_0=0\), there is a further explicit signed test under \(c_6>0\). Let \(A_4\) be the coefficient of \(\delta^4\) in \(\bar A\), \(D_2\) that of \(\delta^2\) in \(\bar D\), and \(E_0\) the coefficient of \(h^3\) in \(P(h,0)\). Comparing the upper bound above with \(P(h,0)\) confines maximizing \(v\) to a bounded interval. Substitution and Taylor bounds there give the cubic coefficient of the locally optimized value as
\[
 E_0+\max_{v\ge0}\{-c_6 v^3+A_4 v^2+D_2v\}.
\]
A fixed maximizing \(v\) is feasible for small \(h\) and supplies the matching lower term. A strictly signed cubic decides this local comparison. This test leaves the case of a vanishing cubic and the case of a vanishing sextic to further analysis.

For an exact sample exercising the quadratic case take ten zeros, nine copies of each of \(-1,1\), and one copy of each of \(-\sqrt6,\sqrt6\). It has \(m_4=3,\ m_6=15\), \(c_6=n\kappa_3^2/12>0\) and \(D_0=-22.5\). The conclusion for this sample concerns the neighborhood of the Gaussian point.

## From the signed tail to a global decision

**Proved.** First bound or classify the nuisance escapes. The prior Student-envelope argument gives a compact competitive nuisance set when free scale has strict collapse exponent. With all coefficients free this holds when \(2k_{\max}<n\). Above critical atom mass a fixed admissible df and finite slant witness unbounded collapse. At critical mass, any nonzero collapse limit with free df has \(\nu\to1\), with the face and rates from the prior closure proof. The large-df part still has a strict collapse exponent as soon as \(\nu\) is bounded away from the critical df. Held scale removes collapse altogether. [Closure and sign cases](2026-11-07-skew-theory-data/proofs.md#14-closing-the-st-collapse-equality-layers-by-sign-cases)

On a compact nuisance set, the prior moving-endpoint cutoff bounds all sufficiently large finite slants strictly by their reachable same-df half-t. With a held location and both residual signs, each wrong-side orientation has a fixed nonzero residual whose standardized skew argument grows uniformly with \(|\alpha|\) over the scale and df box. The Student tail majorant makes its contribution vanish uniformly. With exactly one strict sign and slant free, monotonicity reduces the problem to the corresponding half family, retaining a factor of one at each held endpoint tie. The half-t components have the Student base likelihood with \(\alpha=0\) and the appropriate constant log multiplier, and their endpoints are the sample extremes when location moves. Their large-df expansions use the same \(a,b\) at \(\alpha=0\), with regular scale profiling if scale is free and bounded away from zero. Thus the df tail is controlled by a bounded finite-slant piece and by the feasible half-t pieces. The skew-normal supremum \(F\) includes the half-normal corners; all pieces tied at \(h=0\) require a signed comparison. [Slant cutoff and uniform normal-face value bound](2026-11-25-sn-math-report.md#free-df)

If a feasible finite witness exceeds a verified upper bound for \(F\), the uniform df-tail error bound from the prior report cuts off all sufficiently large df against that witness. This also applies after a positive inward slope or higher coefficient on any active component: a reachable half-t improvement can be approximated by finite slant and supplies a witness. A positive coefficient excludes \(F\), with separate comparisons needed to establish whether a finite slant attains the global maximum. In the opposite case, a strictly signed loss on every active piece cuts off all \(0<h<H\) against \(F\) itself, with no fixed positive gap assumed between finite df and the normal limit.

Cover the remaining finite-df region \(H\le h\le1\) including the restriction face \(h=1\), and compare the half-t components with the **same** \(\nu\ge1\) restriction. A simple finite-box likelihood upper bound uses the smallest residual magnitude and smallest scale in each box for the base envelope, \(t_{\nu_U}(0)\) for its peak, and the lower penalty \((\nu_L+1)\log(1+z_{\min}^2/\nu_U)/2\). For each CDF use the upper skew argument, then the upper df endpoint for a nonnegative argument and the lower df endpoint for a negative argument. Indeed, for \(m_2>m_1>0,\ u>0\), the derivative of \(\log(t_{m_2}(u)/t_{m_1}(u))\) in \(u\) is
\[
 \frac{(m_2-m_1)u(1-u^2)}{(m_1+u^2)(m_2+u^2)} .
\]
The ratio starts above one by the normalizer monotonicity, increases to \(u=1\), then decreases to zero. Single crossing and equal integrals over the positive half-line give \(T_{m_2}(u)\ge T_{m_1}(u)\) for \(u\ge0\). Reflection proves the negative-argument comparison. These box bounds converge as boxes shrink. Score boxes or integrated Taylor bounds improve rejection; retain the nuisance stationary components on \(h=1\) with the appropriate one-sided comparison.

After the exterior regions have been excluded or represented by compared faces, let \(M\) be the maximum on the remaining compact set of genuine finite-coefficient points and \(E\) the largest reachable excluded value, including \(F\) and any surviving half-t or collapse face. An excluded df neighborhood bounded by a finite witness can be dropped from this comparison. If \(M>E\), a finite MLE exists. If \(M<E\), only excluded faces maximize, and a proved strict normal or half-normal winner gives the named limit. If \(M=E\), a finite maximizer attains the global likelihood and the tied face is a limiting witness. This decision uses bounds or exact comparisons of the candidate maxima. Overlapping numerical enclosures alone leave equality undecided. When the signed df test closes against \(F\) but the finite-region cover has an unresolved candidate, a negative tail coefficient says nothing about which has the larger global value.

### A critical-atom df reduction

**Proved.** At a critical atom \(y\) of multiplicity \(k=n/2\), with scale free, set \(a=\omega\sqrt\nu\), \(p=n-k\), and work with \(|\xi-y|\le d\) for \(d<\min_{x_i\ne y}|x_i-y|\) and \(0<a\le1\). Let \(M_{\log}\) bound \(|\log((x_i-\xi)^2+a^2)|\) for the off-atom data in this region. In the base Student part, the prior critical-atom inequality bounds the change from \(\nu=1\) at the same \((\xi,a)\) by \((\nu-1)(n+pM_{\log}/2+p\log a)\). It follows by factoring out \(a^{\nu p-k}\), dropping the nonpositive change in the atom penalty, and using \((\log(\sqrt\nu\,t_\nu(0)))'\le1\). [Student critical collar](2026-11-25-tcert-math-report.md#2-compactness-and-face-comparisons)

For the skew factors write \(m=\nu+1,\ v=\alpha(x_i-\xi)\sqrt{m}/\sqrt{a^2+(x_i-\xi)^2}\). Holding \(v\) fixed, the upper bound \(\partial_m\log t_m(u)\le 1/(4m^2)+(m+1)/(2m)\) follows by differentiating the Student density and using the normalizer derivative bound. Integrating \(t_m\) over \((-\infty,v]\) gives the same upper bound for \(\partial_m\log T_m(v)\). The differentiated tail is integrable locally uniformly in \(m\ge2\), since it adds only a logarithmic tail factor. The moving argument adds \(v t_m(v)/(2m T_m(v))\). For \(v\le0\) it is nonpositive, and for \(v>0\), \(v t_m(v)\le\int_0^v t_m(u)\,du\le T_m(v)\). Consequently each skew log factor adds at most \(2(\nu-1)\) for any finite slant. Altogether,
\[
 \ell(\xi,a,\alpha,\nu)-\ell(\xi,a,\alpha,1)
 \ \le\ (\nu-1)\left[3n+\frac{p M_{\log}}2+p\log a\right].
\]
For \(a\le\exp[-(3n+p M_{\log}/2+1)/p]\) every \(\nu>1\) point is strictly dominated by the same \((\xi,a,\alpha)\) point at \(\nu=1\). This controls the df direction even when slant diverges. Comparing the remaining \(\nu=1\) scale and slant neighborhood to its limiting collapse layer still requires a signed scale comparison if the collapse value is competitive.

## Transition examples and numerical evidence

**Proved, a normal win at zero slope and a finite-df win across it.** Take \((-\sqrt{s},\sqrt{s})\) with \(s>0\), hold \(\xi=0,\omega=1\), and free slant and df. At every df the opposite CDF factors have product \(p(1-p)\le1/4\), with equality just at slant zero. Let \(s_c=1+\sqrt2\). The log-density difference per datum at this optimum is
\[
 C(h)+\frac h2\int_0^s\frac{u-1}{1+hu}\,du .
\]
For \(s=s_c\), write the integral as \(1/2-hI\), where \(I=\int_0^{s_c}u(u-1)/(1+hu)\,du\). Uniformly in \(0<h\le1\), the negative contribution for \(u\in[0,1]\) is at least \(-1/6\). The positive contribution is at least \(\frac{2}{7}\int_1^{12/5}u(u-1)\,du\), since \(12/5<s_c<5/2\). Thus \(I>1/12\) by rational arithmetic. The earlier bound \(C(h)\le-h/4+h^3/24\) gives a strict loss. The difference as a function of \(s\) decreases up to 1 and increases thereafter, so for \(0\le s\le s_c\) it is bounded above by the larger of its endpoint values at 0 and \(s_c\), both negative at positive \(h\). For \(s>s_c\), the coefficient \((s^2-2s-1)/4\) is positive and the continuous profile on \(0\le h\le1\) has a maximum at positive \(h\). Hence \(s=s_c\) is a true slope-zero transition with a strict normal win at equality.

**Proved, perturbation to nonzero slant.** For the held-location and held-scale pair \((-s,rs)\), \(s>0,\ r>1\), the finite-df CDF product is \(T_m(Ru)T_m(-u)\) for \(m=\nu+1,\ u\) proportional to slant and \(R>1\) the ratio of the positive to negative skew argument multipliers. At \(h=0\), use \(\Phi\) and \(R=r\). The value at \(u>0\) is greater than at \(-u\) because the difference is \(T_m(Ru)-T_m(u)>0\). At positive \(u\) the score has the sign of
\[
 R\,t_m(Ru)/t_m(u) - T_m(Ru)/T_m(-u).
\]
The first term decreases strictly and the second increases strictly. At zero the difference is positive, and eventually it is negative. This proves a unique maximizing slant for all \(h\), with the same argument for \(\Phi\). At the symmetric transition \(s=\sqrt{s_c}, r=1\), the derivative of the inward slope with respect to \(s\) is strictly positive and \(D<0\). The slant optimum is regular there. Smooth dependence and the implicit function theorem give a slope-zero \(s(r)\) near \(r=1\), with \(D\) still negative. Uniform negative curvature in a short \(h\) interval and strict separation on its compact complement persist for \(r>1\) sufficiently close to 1. For the latter use continuity of the slant profile, with the mixed-sign residual bound uniform for \(s,r\) in this neighborhood. The normal face wins at \(s(r)\) and for slightly smaller \(s\). For slightly larger \(s\), a positive inward slope gives an attained positive-\(h\) maximum, unique in the strictly concave \(h\) interval that contains it.

**Proved, a finite fit tied with the normal face.** This equality is distinct from \(A=0\). Hold \(\alpha=0\) and free the other three coefficients. Let \(y_j\) be the six positive values \(1,2,3,4,99,101\), and \(v_j=y_j^2/\overline{y^2}\). For \(0\le\lambda\le1\) define the exact 12-point sample as the six pairs
\[
  x=\left\{\ \pm\sqrt{1+\lambda(v_j-1)}:\ j=1,\ldots,6\ \right\}.
\]
It has variance one. By symmetry and the known geodesic concavity for \(\nu\ge1\), the centered scale optimum is global at each df. This includes the centered representative of the balanced Cauchy set at \(\lambda=0\). The optimal squared scale lies between the smallest and largest squared data magnitudes by the scalar scale score, so the profile is jointly continuous in \(h,\lambda\) on \([0,1]^2\). The inward slope is uniformly strictly negative since
\(m_4(\lambda)=1+\lambda^2(m_4(1)-1)\) and \(m_4(1)<3\). The prior Student signed-tail bounds, or the regular expansion uniformly in this family of centered samples, therefore exclude one common interval \(0<h<H\) against the normal. At \(\lambda=0\) normal wins strictly by the balanced Student calculation. At \(\lambda=1\) the earlier exact rational finite witness beats the fitted normal by \(>0.49\) log units, an invariant difference after standardization. [Balanced Student comparison](2026-11-25-tcert-math-report.md#2-compactness-and-face-comparisons) · [Geodesic concavity and exact negative-slope counterexample](2026-11-04-t-cert-data/derivation.md#an-exact-counterexample-to-a-global-kurtosis-decision)

For fixed \(h>0\) and scale, the sum of log kernels strictly increases with \(\lambda>0\). The sum of \(\log(1+h[1+\lambda(v_j-1)]/\omega^2)\) is strictly concave in \(\lambda\), with derivative zero at \(\lambda=0\); the likelihood takes its negative. Taking the profile preserves strict increase by using the earlier scale optimum as a witness. Hence the unique exact crossing is specified by \(\lambda_*=\min\{\lambda:\exists h\in(0,1]\text{ with profile}\ge F_{\mathrm G}\}\), with \(0<\lambda_*<1\). The uniform tail exclusion makes this a closed comparison on \([H,1]\). Continuity gives equality at a finite df at \(\lambda_*\), strict normal wins below, and a finite winner above.

**Numerical checks.** Both scripts ran to completion on the workstation JonasBI under the one-slot wrapper at 65 decimal digits. The comparisons are numerical evaluations of the stated formulas and bounds. Exact conclusions above use their respective proofs. [Expansion, transition and golden checks](2026-11-25-st-math-data/check.py) · [Singular and tie checks](2026-11-25-st-math-data/degenerate.py)

| Workload | Grid or cover and result |
|---|---|
| Pointwise expansion | \(z\) in \(\{-4,-2,-.5,0,.5,2,4\}\), \(\alpha\) in \(\{-3,-1,0,1,3\}\), \(h=10^{-4},10^{-5},10^{-6}\). All 105 checks passed. The maximum observed second-order remainder divided by \(h^3\) was \(8.1910\cdot10^7\). Thin skew tails make a common unscaled remainder constant large. |
| Exact pair transition | \(s=s_c,s_c\pm10^{-6}\), at \(h=1,.1,.001,10^{-6},10^{-7},10^{-8}\). At \(s_c\) the gain at \(h=10^{-6}\) was \(-1.7761\cdot10^{-12}\). On the upper side the gain at \(10^{-7}\) was \(+1.2366\cdot10^{-13}\). |
| Asymmetric pair, \(r=2\) | Slope zero numerically at \(s=0.9097621451901831\), with fitted slant \(0.2969059825478948\) and \(D=-2.313264113206579\). For \(s\) shifted by \(\pm10^{-6}\), slopes were \(-7.53126\cdot10^{-6}\) and \(+7.53130\cdot10^{-6}\). The finite-df slant was solved on \(h=1,.5,.1,.01,.001,10^{-4},10^{-6},10^{-7}\). At slope zero all checked values lost. On the upper side the gain was \(+5.2180\cdot10^{-12}\) at \(10^{-6}\). The sampled losses on the lower side and at zero slope are grid evidence for \(r=2\). |
| Singular symmetric sample | At \(\delta=.02,.01,.005\), nuisance profiling at \(h=0\) and \(h=\delta^2\) gave \((P(0,\delta)-F_{\mathrm G})/\delta^6\) tending to \(-0.1188248398814\) and \((P(h,\delta)-F_{\mathrm G})/h^2\) tending to \(-22.5\). The last values were \(-0.1188306975216\) and \(-22.49988517525\). |
| Finite/normal equality path | The Cauchy crossing at \(\lambda=0.9432283692853\) has a better interior grid witness at \(h=.70\), with gain \(0.1748761091\). Solving the interior stationarity and equality equations instead gave \(\lambda=0.9347464805954687,\ h=0.6018538385782895,\ \omega=0.5184676180815969\). The other checked values \(h=j/100,\ j=1,\ldots,100\), lost at that crossing. At offsets \(\lambda\pm10^{-6}\), maximizing over the same grid augmented by the stationary crossing \(h\) gave gains of about \(\pm1.7460\cdot10^{-5}\). The proof defines the exact global crossing; these computations locate a numerical candidate for it. |

**Numerical golden comparison.** The repository contains one canonical 500-observation skew-t golden sample. Evaluating its stored binary64 coefficients and data gives \(-774.30230789184550196\) at \(\nu\approx4.83151\). For the skew-normal upper comparison the tangent cover stopped after visiting 25 boxes in \([-8,8]\), with the target set two log units below the finite witness. Outside this interval, use \(\log\Phi(t)\le-t^2/2\) for \(t<0\), which follows by shifting the Gaussian tail integral. At a fixed orientation and \(|\alpha|\ge8\), minimize the centered SSE plus \(8^2\) times the SSE of the wrong-side observations over the location, a convex piecewise quadratic. Optimizing the resulting Gaussian envelope over scale gives wing upper bounds \(-926.07808,-891.58925\). With the wings included, the largest terminal upper bound was \(-781.392553760183145\). A separately refined feasible stationary point on the skew-normal face had value \(-791.67024545523068\), \(\alpha=-0.8844646818155038\), and \(A=238.59586291847253\). The certificate uses the upper bound for the face comparison.

For each half-t orientation, Bernoulli's inequality gives \(t_\nu(z)\le\phi(0)/(1+z^2/2)\) at every \(\nu\ge1\). At the extreme endpoint, if \(p>n/2\) data are at distance at least \(d>0\), the log upper bound is
\(n\log(2\phi(0)/\omega)-p\log(1+d^2/(2\omega^2))\), maximized at \(\omega^2=(2p/n-1)d^2/2\). Minimizing over eligible order-statistic distances gives upper values \(-945.8912407532339,-973.8964484108005\) including the half-normal corners. On this sample the atom mass is strict. Thus the comparisons exclude every limiting model with a wide margin, establishing finite attainment conditional on the computed upper comparisons. The comparison checks neither uniqueness nor the coefficient accuracy of the stored finite witness. [Golden fixture](../../../univariateML-cpp/include/cppunivariate/distributions/st/golden.json)

Run from the worktree root.

~~~sh
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
mkdir -p "$TMPDIR"
for script in check degenerate; do
    /tmp/umvl-cpu/run python3 -Bu "dev/research/evaluations/2026-11-25-st-math-data/$script.py" || exit
done
~~~

## What this changes for the fitter

- Evaluate \(A\) over all competitive hold-compatible normal branches and half-normal corners. A strict inward gain refutes the normal face and yields a finite candidate or a reachable witness for the finite-df cover. A loss closes the large-df region only when it is uniform over every active branch. At zero slope, use \(D\) with the fitted-coefficient correction at regular points, or the reduced slant power bound at singular points. The symmetric \(m_4=3\) test uses \(m_6,\ c_6\) and the same-hold Gaussian inclusion.
- Retain \(\nu=1\) as an attainable restriction. Compare the finite enclosure, constrained half-t faces, feasible normal corners, and relevant collapse faces. A strict proved normal win gives a certified \(\texttt{SkewNormalBoundary}\) with the hold-compatible \(\texttt{mlsn}\) payload. A winning corner or half-t uses the corresponding oriented status and contact penalty. A proved unattained collapse uses the scale-boundary reason. At a finite/face equality retain the finite estimate and the limiting witness, subject to coefficient inclusion. Track distinct finite maximizing components and only assert uniqueness after excluding them.
- For a certified \(\texttt{Ok}\) require a verified winning enclosure and the requested tolerance in all public coefficients, especially \(\nu=1/h\) near the face. Use signed practical arithmetic margins on score, likelihood and coefficient bounds. Preserve a successful search result as uncertified when the certificate cannot close or exhausts its budget. Stop if a separated feasible witness refutes that candidate, the search itself fails its checks, or the mathematics proves a limit or nonexistence. The signed reductions are exact-arithmetic results and the numerical comparisons are research checks.

## Open

1. **General vanishing signed tests.** The reduced Taylor comparison gives computable bounds when its leading optimized coefficient is strict. Cases with further cancellation need higher-order validated expansions or another inequality. In particular, the symmetric cubic test has no decision at a zero cubic, and a zero sextic needs a different slant power. Exact equality of unrelated stationary and face likelihoods still needs an equality argument.
2. **Certified finite-region search and coefficient inclusion.** Implement and benchmark the four-dimensional cover, including the \(\nu=1\) restriction and the constrained half-t searches. The golden comparison closes the escape question but still needs finite-region exclusion of competing stationary components and public-coordinate inclusion at the winning point. Robust special-function derivative and tail-error bounds are needed for the relevant arithmetic, along with stress tests of the practical margins.
3. **Critical collapse and its intersection with slant escape.** The signed df collar reduces sufficiently small critical scales to \(\nu=1\). A quantitative signed scale collar at \(\nu=1\), uniform in the competitive location displacement and slant layers, is still needed when a collapse face ties or leads. Compare those layers with positive-scale stationary sets. The known Cauchy continuum requires retaining whole maximizing components in the applicable held subset.
